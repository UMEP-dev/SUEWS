#!/usr/bin/env python3
"""
SUEWS Configuration Validator

A user-friendly CLI tool for validating SUEWS YAML configurations.
"""

import click
import yaml
import json
import sys
from pathlib import Path
from typing import Optional, List
import jsonschema
from rich.console import Console
from rich.table import Table
from rich.panel import Panel
from rich.syntax import Syntax
from rich.progress import track

# Import the new JSON output formatter
try:
    from .json_output import JSONOutput, ErrorCode, ValidationError
except ImportError:
    # Fallback if module not available
    JSONOutput = None
    ErrorCode = None
    ValidationError = None

# Orchestrated YAML processor phases (A/B/C)
try:
    from ..data_model.validation.pipeline.orchestrator import (
        validate_input_file as _processor_validate_input_file,
        setup_output_paths as _processor_setup_output_paths,
        run_phase_a as _processor_run_phase_a,
        run_phase_b as _processor_run_phase_b,
        run_phase_c as _processor_run_phase_c,
    )
except Exception:
    _processor_validate_input_file = None
    _processor_setup_output_paths = None
    _processor_run_phase_a = None
    _processor_run_phase_b = None
    _processor_run_phase_c = None

# Import from supy modules
try:
    from ..data_model.core.config import SUEWSConfig
    from ..data_model.schema.version import CURRENT_SCHEMA_VERSION
    from ..data_model.schema.publisher import generate_json_schema
except ImportError:
    # Fallback for direct script execution
    import sys

    sys.path.append(str(Path(__file__).parent.parent.parent))
    from supy.data_model.core.config import SUEWSConfig
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION
    from supy.data_model.schema.publisher import generate_json_schema
    from supy.data_model.schema.migration import SchemaMigrator, check_migration_needed

console = Console()


def validate_single_file(
    file_path: Path, schema: dict, show_details: bool = True
) -> tuple[bool, List]:
    """
    Validate a single configuration file.

    Returns:
        Tuple of (is_valid, list_of_errors)
        Errors can be strings or ValidationError objects
    """
    errors = []

    try:
        # Load configuration
        with open(file_path, "r") as f:
            config = yaml.safe_load(f)

        # Check if migration needed
        if check_migration_needed(str(file_path)):
            if ValidationError:
                errors.append(
                    ValidationError(
                        code=ErrorCode.SCHEMA_VERSION_MISMATCH,
                        message="Configuration uses old schema version and may need migration",
                        location=str(file_path),
                    )
                )
            else:
                errors.append(
                    "Configuration uses old schema version and may need migration"
                )

        # Validate against schema
        validator = jsonschema.Draft7Validator(schema)
        validation_errors = list(validator.iter_errors(config))

        if validation_errors:
            for error in validation_errors:
                path = " → ".join(str(p) for p in error.path) if error.path else "root"
                if ValidationError and ErrorCode:
                    # Categorize the error based on its content
                    if "required" in error.message.lower():
                        code = ErrorCode.MISSING_REQUIRED_FIELD
                    elif "type" in error.message.lower():
                        code = ErrorCode.TYPE_ERROR
                    else:
                        code = ErrorCode.INVALID_VALUE

                    errors.append(
                        ValidationError(
                            code=code,
                            message=error.message,
                            field=path,
                            location=str(file_path),
                        )
                    )
                else:
                    errors.append(f"{path}: {error.message}")

        # Try Pydantic validation for additional checks
        try:
            SUEWSConfig(**config)
        except Exception as e:
            if ValidationError:
                errors.append(
                    ValidationError(
                        code=ErrorCode.VALIDATION_FAILED,
                        message=str(e),
                        location=str(file_path),
                        details={"validation_type": "pydantic"},
                    )
                )
            else:
                errors.append(f"Pydantic validation: {str(e)}")

        return (len(errors) == 0, errors)

    except yaml.YAMLError as e:
        if ValidationError:
            return (
                False,
                [
                    ValidationError(
                        code=ErrorCode.INVALID_YAML,
                        message=str(e),
                        location=str(file_path),
                    )
                ],
            )
        return (False, [f"YAML parsing error: {e}"])
    except FileNotFoundError:
        if ValidationError:
            return (
                False,
                [
                    ValidationError(
                        code=ErrorCode.FILE_NOT_FOUND,
                        message=f"File not found: {file_path}",
                        location=str(file_path),
                    )
                ],
            )
        return (False, [f"File not found: {file_path}"])
    except Exception as e:
        if ValidationError:
            return (
                False,
                [
                    ValidationError(
                        code=ErrorCode.VALIDATION_FAILED,
                        message=str(e),
                        location=str(file_path),
                    )
                ],
            )
        return (False, [f"Unexpected error: {e}"])


@click.command()
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
@click.option(
    "--pipeline",
    "-p",
    type=click.Choice(["A", "B", "C", "AB", "AC", "BC", "ABC"]),
    default="ABC",
    help="Phase pipeline to run when no subcommand is provided",
)
@click.option(
    "--mode",
    "-m",
    type=click.Choice(["public", "dev"]),
    default="public",
    help="Validation mode for phase pipeline",
)
@click.option(
    "--dry-run",
    is_flag=True,
    help="Do not write files; validate only (supports pipeline C and ABC)",
)
@click.option(
    "--format",
    "out_format",
    type=click.Choice(["table", "json"]),
    default="table",
    help="Output format for results (table or JSON)",
)
@click.option(
    "--schema-version",
    help="Schema version to validate against in --dry-run",
)
def cli(files, pipeline, mode, dry_run, out_format, schema_version):
    """SUEWS Configuration Validator.

    Run validation pipeline on FILES (A/B/C phases).
    JSON reports are automatically generated alongside text reports.

    \b
    Examples:
      suews-validate config.yml                # Full ABC pipeline
      suews-validate -p A config.yml           # Only Phase A
      suews-validate -p AB config.yml          # Phase A + B
      suews-validate -p C --dry-run *.yml      # Check multiple files
      suews-validate --mode dev config.yml     # Developer mode

    For schema operations, use: suews-schema
    """
    # Run the pipeline workflow
    # Dry-run handler (read-only validation)
    if dry_run:
        # Only support C and ABC for now
        if pipeline not in ("C", "ABC"):
            console.print(
                "[red]✗ --dry-run is supported for pipeline C or ABC only[/red]"
            )
            sys.exit(2)

        target_version = schema_version
        schema = generate_json_schema(version=target_version)

        # Pipeline C: allow multiple files; ABC: single file
        if pipeline == "C":
            if not files:
                console.print(
                    "[red]✗ Provide one or more YAML files for -p C --dry-run[/red]"
                )
                sys.exit(2)
            results = []
            all_valid = True
            for file_path in files:
                path = Path(file_path)
                is_valid, errors = validate_single_file(path, schema, show_details=True)
                if not is_valid:
                    all_valid = False

                # Convert ValidationError objects to dicts for JSON serialization
                error_list = []
                for error in errors:
                    if hasattr(error, "to_dict"):
                        error_list.append(error.to_dict())
                    else:
                        error_list.append(str(error))

                results.append({
                    "file": str(path),
                    "valid": is_valid,
                    "errors": error_list if not is_valid else [],
                    "error_count": len(errors) if not is_valid else 0,
                })

            if out_format == "json":
                if JSONOutput:
                    # Use the new structured JSON output
                    json_formatter = JSONOutput(command="suews-validate")
                    output = json_formatter.validation_result(
                        files=results, schema_version=target_version, dry_run=True
                    )
                    JSONOutput.output(output)
                else:
                    # Fallback to simple JSON
                    console.print(json.dumps(results, indent=2))
            else:
                table = Table(title="Validation Results")
                table.add_column("File", style="cyan")
                table.add_column("Status", justify="center")
                table.add_column("Issues", style="yellow")
                for r in results:
                    status = (
                        "[green]✓ Valid[/green]"
                        if r["valid"]
                        else "[red]✗ Invalid[/red]"
                    )
                    issues = (
                        ""
                        if r["valid"]
                        else ("\n".join(r["errors"][:3]) if r["errors"] else "")
                    )
                    if not r["valid"] and len(r["errors"]) > 3:
                        issues += f"\n... and {len(r['errors']) - 3} more"
                    table.add_row(Path(r["file"]).name, status, issues)
                console.print(table)
                console.print(
                    f"\n[bold]Summary:[/bold] {sum(1 for r in results if r['valid'])}/{len(results)} files valid"
                )

            sys.exit(0 if all_valid else 1)

        # pipeline == ABC dry-run
        if len(files) != 1:
            console.print(
                "[red]✗ Provide exactly one YAML file for -p ABC --dry-run[/red]"
            )
            sys.exit(2)
        path = Path(files[0])
        is_valid, errors = validate_single_file(path, schema, show_details=True)

        # Convert ValidationError objects to dicts for JSON serialization
        error_list = []
        for error in errors:
            if hasattr(error, "to_dict"):
                error_list.append(error.to_dict())
            else:
                error_list.append(str(error))

        result = [
            {
                "file": str(path),
                "valid": is_valid,
                "errors": error_list if not is_valid else [],
                "error_count": len(errors) if not is_valid else 0,
            }
        ]
        if out_format == "json":
            if JSONOutput:
                # Use the new structured JSON output
                json_formatter = JSONOutput(command="suews-validate")
                output = json_formatter.validation_result(
                    files=result, schema_version=target_version, dry_run=True
                )
                JSONOutput.output(output)
            else:
                # Fallback to simple JSON
                console.print(json.dumps(result, indent=2))
        else:
            table = Table(title="Validation Results")
            table.add_column("File", style="cyan")
            table.add_column("Status", justify="center")
            table.add_column("Issues", style="yellow")
            status = "[green]✓ Valid[/green]" if is_valid else "[red]✗ Invalid[/red]"
            issues = "" if is_valid else ("\n".join(errors[:3]) if errors else "")
            if not is_valid and len(errors) > 3:
                issues += f"\n... and {len(errors) - 3} more"
            table.add_row(path.name, status, issues)
            console.print(table)
            console.print(
                f"\n[bold]Summary:[/bold] {1 if is_valid else 0}/1 files valid"
            )
        sys.exit(0 if is_valid else 1)

    # Non-dry-run: execute pipeline with file writes
    if len(files) != 1:
        console.print(
            "[red]✗ Provide exactly one YAML FILE for pipeline execution[/red]"
        )
        sys.exit(2)
    code = _execute_pipeline(file=files[0], pipeline=pipeline, mode=mode)
    sys.exit(code)


# Removed redundant 'validate' command - default behavior handles validation


## Removed `check` subcommand to avoid redundancy with `validate`.


# Removed 'update' command - merged into 'schema migrate'


def _print_schema_info():
    from ..data_model._schema_version import SCHEMA_VERSIONS

    console.print(Panel("[bold]SUEWS Configuration Schema Information[/bold]"))

    console.print(f"\n[bold]Current Schema Version:[/bold] {CURRENT_SCHEMA_VERSION}")

    if CURRENT_SCHEMA_VERSION in SCHEMA_VERSIONS:
        console.print(f"[dim]{SCHEMA_VERSIONS[CURRENT_SCHEMA_VERSION]}[/dim]")

    console.print("\n[bold]Version History:[/bold]")
    for version, description in SCHEMA_VERSIONS.items():
        marker = "→" if version == CURRENT_SCHEMA_VERSION else " "
        console.print(f"  {marker} v{version}: {description}")

    console.print("\n[bold]Schema Files:[/bold]")
    console.print("  • JSON Schema: schemas/latest/schema.json")
    console.print("  • YAML Schema: schemas/latest/schema.yaml")
    console.print("  • Documentation: docs/source/inputs/yaml/schema_versioning.rst")

    console.print("\n[bold]Validation Commands:[/bold]")
    console.print("  • Pipeline run: suews-validate -p ABC config.yml")
    console.print(
        "  • Pipeline with JSON reports: suews-validate -p ABC --use-refactored config.yml"
    )
    console.print(
        "  • Read-only check: suews-validate -p C --dry-run configs/*.yml --format json"
    )
    console.print("  • Migrate: suews-validate schema migrate old_config.yml")


# Removed redundant 'version' command - now use 'schema status' or 'schema set'
def _check_schema_versions(files, update, target_version, backup):
    """Internal helper: Check or update schema_version in YAML files."""
    # Reuse common logic
    try:
        # Inline import to keep CLI startup light
        from ..data_model.schema.version import CURRENT_SCHEMA_VERSION  # noqa: F401
    except Exception:
        pass
    # Implement inline to avoid refactor breadth
    table = Table(title="Schema Version Status")
    table.add_column("File", style="cyan")
    table.add_column("Current Version", justify="center")
    table.add_column("Status", justify="center")
    if update:
        table.add_column("Action", style="yellow")

    try:
        from ..data_model.schema.version import (
            CURRENT_SCHEMA_VERSION,
            is_schema_compatible,
        )
    except Exception as e:
        console.print(f"[red]✗ Unable to load schema version module: {e}[/red]")
        sys.exit(1)

    for file_path in files:
        path = Path(file_path)
        try:
            with open(path, "r") as f:
                cfg = yaml.safe_load(f) or {}
            current = cfg.get("schema_version") or "not specified"

            if current == "not specified":
                status = "[yellow]⚠ Missing[/yellow]"
            elif is_schema_compatible(current):
                status = "[green]✓ Compatible[/green]"
            else:
                status = "[red]✗ Incompatible[/red]"

            action = ""
            if update:
                new_version = target_version or CURRENT_SCHEMA_VERSION
                if current != new_version:
                    if backup:
                        backup_path = path.with_suffix(".backup.yml")
                        path.rename(backup_path)
                    cfg["schema_version"] = new_version
                    with open(path, "w") as f:
                        yaml.dump(cfg, f, default_flow_style=False, sort_keys=False)
                    action = f"Updated → {new_version}"
                else:
                    action = "No change needed"

            if update:
                table.add_row(path.name, str(current), status, action)
            else:
                table.add_row(path.name, str(current), status)
        except Exception as e:
            if update:
                table.add_row(path.name, "Error", f"[red]✗ {e}[/red]", "Skipped")
            else:
                table.add_row(path.name, "Error", f"[red]✗ {e}[/red]")

    console.print(table)


# Removed redundant top-level 'export' command - use 'schema export' instead


def _execute_pipeline(file, pipeline, mode):
    """Run YAML processor phases A/B/C to validate and generate reports/YAML.

    Phase A: Update YAML structure and detect parameters
    Phase B: Scientific checks and adjustments
    Phase C: Pydantic validation with physics conditionals

    Args:
        file: Input YAML file path
        pipeline: Phase combination (A, B, C, AB, AC, BC, ABC)
        mode: Validation mode (public/dev)
        out_format: Output format (table or json)

    Note: JSON reports are always generated alongside text reports.
    """
    # Ensure processor is importable
    if not all([
        _processor_validate_input_file,
        _processor_setup_output_paths,
        _processor_run_phase_a,
        _processor_run_phase_b,
        _processor_run_phase_c,
    ]):
        console.print(
            "[red]✗ YAML processor is unavailable. Ensure supy.data_model.validation.pipeline is present.[/red]"
        )
        return 1

    # Validate input and prepare paths
    try:
        user_yaml_file = _processor_validate_input_file(file)
    except Exception as e:
        console.print(f"[red]✗ {e}[/red]")
        return 1

    standard_yaml_file = "src/supy/sample_data/sample_config.yml"

    (
        uptodate_file,
        report_file,
        science_yaml_file,
        science_report_file,
        pydantic_yaml_file,
        pydantic_report_file,
        _dirname,
    ) = _processor_setup_output_paths(user_yaml_file, pipeline)

    # Execute selected phases - always use refactored system for JSON reports
    if pipeline == "A":
        result = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="A",
            silent=True,
            use_refactored=True,  # Always generate JSON reports
        )
        # Handle tuple return from refactored version
        ok = result[0] if isinstance(result, tuple) else result
        console.print(
            "[green]✓ Phase A completed[/green]"
            if ok
            else "[red]✗ Phase A failed[/red]"
        )
        if ok:
            console.print(f"Text Report: {report_file}")
            json_report = report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            console.print(f"Updated YAML: {uptodate_file}")
        return 0 if ok else 1

    if pipeline == "B":
        result = _processor_run_phase_b(
            user_yaml_file,
            user_yaml_file,
            standard_yaml_file,
            science_yaml_file,
            science_report_file,
            None,
            phase_a_performed=False,
            mode=mode,
            phase="B",
            silent=True,
            use_refactored=True,  # Always generate JSON reports
        )
        # Handle tuple return from refactored version
        ok = result[0] if isinstance(result, tuple) else result
        console.print(
            "[green]✓ Phase B completed[/green]"
            if ok
            else "[red]✗ Phase B failed[/red]"
        )
        if ok:
            console.print(f"Text Report: {science_report_file}")
            json_report = science_report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            console.print(f"Updated YAML: {science_yaml_file}")
        return 0 if ok else 1

    if pipeline == "C":
        result = _processor_run_phase_c(
            user_yaml_file,
            pydantic_yaml_file,
            pydantic_report_file,
            mode=mode,
            phases_run=["C"],
            silent=True,
            use_refactored=True,  # Always generate JSON reports
        )
        # Handle tuple return from refactored version
        ok = result[0] if isinstance(result, tuple) else result
        console.print(
            "[green]✓ Phase C completed[/green]"
            if ok
            else "[red]✗ Phase C failed[/red]"
        )
        if ok:
            console.print(f"Text Report: {pydantic_report_file}")
            json_report = pydantic_report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            console.print(f"Updated YAML: {pydantic_yaml_file}")
        return 0 if ok else 1

    if pipeline == "AB":
        a_result = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="AB",
            silent=True,
            use_refactored=True,  # Always generate JSON reports
        )
        a_ok = a_result[0] if isinstance(a_result, tuple) else a_result
        if not a_ok:
            # Preserve Phase A outputs as AB outputs
            console.print("[red]✗ Phase A failed[/red]")
            if report_file:
                console.print(f"Text Report: {science_report_file}")
            json_report = science_report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            if uptodate_file:
                console.print(f"Updated YAML: {science_yaml_file}")
            return 1

        b_result = _processor_run_phase_b(
            user_yaml_file,
            uptodate_file,
            standard_yaml_file,
            science_yaml_file,
            science_report_file,
            report_file,
            phase_a_performed=True,
            mode=mode,
            phase="AB",
            silent=True,
            use_refactored=True,  # Always generate JSON reports
        )
        b_ok = b_result[0] if isinstance(b_result, tuple) else b_result
        ok = a_ok and b_ok
        console.print(
            "[green]✓ Phase AB completed[/green]"
            if ok
            else "[red]✗ Phase AB failed[/red]"
        )
        if ok:
            console.print(f"Text Report: {science_report_file}")
            json_report = science_report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            console.print(f"Updated YAML: {science_yaml_file}")
        return 0 if ok else 1

    if pipeline == "AC":
        a_result = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="AC",
            silent=True,
            use_refactored=True,  # Always generate JSON reports
        )
        a_ok = a_result[0] if isinstance(a_result, tuple) else a_result
        if not a_ok:
            console.print("[red]✗ Phase A failed[/red]")
            console.print(f"Text Report: {pydantic_report_file}")
            json_report = pydantic_report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            console.print(f"Updated YAML: {pydantic_yaml_file}")
            return 1

        c_result = _processor_run_phase_c(
            uptodate_file,
            pydantic_yaml_file,
            pydantic_report_file,
            mode=mode,
            phase_a_report_file=report_file,
            phases_run=["A", "C"],
            silent=True,
            use_refactored=True,  # Always generate JSON reports
        )
        c_ok = c_result[0] if isinstance(c_result, tuple) else c_result
        ok = a_ok and c_ok
        console.print(
            "[green]✓ Phase AC completed[/green]"
            if ok
            else "[red]✗ Phase AC failed[/red]"
        )
        if ok:
            console.print(f"Text Report: {pydantic_report_file}")
            json_report = pydantic_report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            console.print(f"Updated YAML: {pydantic_yaml_file}")
        return 0 if ok else 1

    if pipeline == "BC":
        b_ok = _processor_run_phase_b(
            user_yaml_file,
            user_yaml_file,
            standard_yaml_file,
            science_yaml_file,
            science_report_file,
            None,
            phase_a_performed=False,
            mode=mode,
            phase="BC",
            silent=True,
        )
        if not b_ok:
            console.print("[red]✗ Phase B failed[/red]")
            console.print(f"Text Report: {pydantic_report_file}")
            json_report = pydantic_report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            console.print(f"Updated YAML: {pydantic_yaml_file}")
            sys.exit(1)

        c_ok = _processor_run_phase_c(
            science_yaml_file,
            pydantic_yaml_file,
            pydantic_report_file,
            mode=mode,
            phases_run=["B", "C"],
            silent=True,
        )
        ok = b_ok and c_ok
        console.print(
            "[green]✓ Phase BC completed[/green]"
            if ok
            else "[red]✗ Phase BC failed[/red]"
        )
        if ok:
            console.print(f"Text Report: {pydantic_report_file}")
            json_report = pydantic_report_file.replace(".txt", ".json")
            console.print(f"JSON Report: {json_report}")
            console.print(f"Updated YAML: {pydantic_yaml_file}")
        sys.exit(0 if ok else 1)

    # Default: ABC
    a_ok = _processor_run_phase_a(
        user_yaml_file,
        standard_yaml_file,
        uptodate_file,
        report_file,
        mode=mode,
        phase="ABC",
        silent=True,
    )
    if not a_ok:
        console.print("[red]✗ Phase A failed[/red]")
        console.print(f"Text Report: {pydantic_report_file}")
        json_report = pydantic_report_file.replace(".txt", ".json")
        console.print(f"JSON Report: {json_report}")
        console.print(f"Updated YAML: {pydantic_yaml_file}")
        return 1

    b_ok = _processor_run_phase_b(
        user_yaml_file,
        uptodate_file,
        standard_yaml_file,
        science_yaml_file,
        science_report_file,
        report_file,
        phase_a_performed=True,
        mode=mode,
        phase="ABC",
        silent=True,
    )
    if not b_ok:
        console.print("[red]✗ Phase B failed[/red]")
        console.print(f"Text Report: {science_report_file}")
        json_report = science_report_file.replace(".txt", ".json")
        console.print(f"JSON Report: {json_report}")
        console.print(f"Updated YAML: {science_yaml_file}")
        sys.exit(1)

    c_ok = _processor_run_phase_c(
        science_yaml_file,
        pydantic_yaml_file,
        pydantic_report_file,
        mode=mode,
        phase_a_report_file=None,
        phases_run=["A", "B", "C"],
        silent=True,
    )
    ok = a_ok and b_ok and c_ok
    console.print(
        "[green]✓ Phase ABC completed[/green]"
        if ok
        else "[red]✗ Phase ABC failed[/red]"
    )
    if ok:
        console.print(f"Text Report: {pydantic_report_file}")
        json_report = pydantic_report_file.replace(".txt", ".json")
        console.print(f"JSON Report: {json_report}")
        console.print(f"Updated YAML: {pydantic_yaml_file}")
    return 0 if ok else 1


def main():
    """Main entry point."""
    cli()


if __name__ == "__main__":
    main()
