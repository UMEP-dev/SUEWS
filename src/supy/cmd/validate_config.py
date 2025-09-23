#!/usr/bin/env python3
"""
SUEWS Configuration Validator

A user-friendly CLI tool for validating SUEWS YAML configurations.
"""

import click
import yaml
import json
import sys
import os
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
    from ..data_model.schema.migration import SchemaMigrator, check_migration_needed
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


@click.group(invoke_without_command=True)
@click.argument("files", nargs=-1, type=click.Path(exists=True))
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
    help="Output format for --dry-run results",
)
@click.option(
    "--schema-version",
    help="Schema version to validate against in --dry-run",
)
@click.pass_context
def cli(ctx, files, pipeline, mode, dry_run, out_format, schema_version):
    """SUEWS Configuration Validator.

    Default behavior: run the complete validation pipeline on FILE. Use subcommands
    for specific operations (validate, schema, migrate, export).
    """
    # If invoked without a subcommand, run the pipeline workflow
    if ctx.invoked_subcommand is None:
        # Dry-run handler (read-only validation)
        if dry_run:
            # Only support C and ABC for now
            if pipeline not in ("C", "ABC"):
                console.print(
                    "[red]✗ --dry-run is supported for pipeline C or ABC only[/red]"
                )
                ctx.exit(2)

            target_version = schema_version
            schema = generate_json_schema(version=target_version)

            # Pipeline C: allow multiple files; ABC: single file
            if pipeline == "C":
                if not files:
                    console.print(
                        "[red]✗ Provide one or more YAML files for -p C --dry-run[/red]"
                    )
                    ctx.exit(2)
                results = []
                all_valid = True
                for file_path in files:
                    path = Path(file_path)
                    is_valid, errors = validate_single_file(
                        path, schema, show_details=True
                    )
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

                ctx.exit(0 if all_valid else 1)

            # pipeline == ABC dry-run
            if len(files) != 1:
                console.print(
                    "[red]✗ Provide exactly one YAML file for -p ABC --dry-run[/red]"
                )
                ctx.exit(2)
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
                status = (
                    "[green]✓ Valid[/green]" if is_valid else "[red]✗ Invalid[/red]"
                )
                issues = "" if is_valid else ("\n".join(errors[:3]) if errors else "")
                if not is_valid and len(errors) > 3:
                    issues += f"\n... and {len(errors) - 3} more"
                table.add_row(path.name, status, issues)
                console.print(table)
                console.print(
                    f"\n[bold]Summary:[/bold] {1 if is_valid else 0}/1 files valid"
                )
            ctx.exit(0 if is_valid else 1)

        # Non-dry-run: execute pipeline with file writes
        if len(files) != 1:
            console.print(
                "[red]✗ Provide exactly one YAML FILE for pipeline execution[/red]"
            )
            ctx.exit(2)
        code = _execute_pipeline(file=files[0], pipeline=pipeline, mode=mode)
        ctx.exit(code)


@cli.command()
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
@click.option("--schema-version", help="Schema version to validate against")
@click.option("--verbose", "-v", is_flag=True, help="Show detailed error messages")
@click.option("--quiet", "-q", is_flag=True, help="Only show summary")
@click.option(
    "--format",
    type=click.Choice(["table", "json"]),
    default="table",
    help="Output format",
)
def validate(files, schema_version, verbose, quiet, format):
    """Validate SUEWS YAML configuration files (schema + Pydantic)."""

    # Generate schema
    schema = generate_json_schema(version=schema_version)
    version = schema_version or CURRENT_SCHEMA_VERSION

    if not quiet and format == "table":
        console.print(
            f"\n[bold blue]Validating against schema v{version}[/bold blue]\n"
        )

    total_files = len(files)
    valid_files = 0
    results = []

    for file_path in track(
        files, description="Validating...", disable=(quiet or format == "json")
    ):
        path = Path(file_path)
        is_valid, errors = validate_single_file(path, schema, show_details=verbose)

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
        if is_valid:
            valid_files += 1

    if format == "json":
        if JSONOutput:
            # Use the new structured JSON output
            json_formatter = JSONOutput(command="suews-validate")
            output = json_formatter.validation_result(
                files=results, schema_version=version, dry_run=False
            )
            JSONOutput.output(output)
        else:
            # Fallback to simple JSON
            console.print(json.dumps(results, indent=2))
    else:
        if not quiet:
            table = Table(title="Validation Results")
            table.add_column("File", style="cyan")
            table.add_column("Status", justify="center")
            table.add_column("Issues", style="yellow")
            for r in results:
                status = (
                    "[green]✓ Valid[/green]" if r["valid"] else "[red]✗ Invalid[/red]"
                )
                if r["valid"]:
                    issues = ""
                else:
                    if verbose and r["errors"]:
                        issues = "\n".join(r["errors"][:3])
                        if len(r["errors"]) > 3:
                            issues += f"\n... and {len(r['errors']) - 3} more"
                    else:
                        issues = f"{r['error_count']} issue(s)"
                table.add_row(Path(r["file"]).name, status, issues)
            console.print(table)
            console.print(
                f"\n[bold]Summary:[/bold] {valid_files}/{total_files} files valid"
            )

    # Exit with error if any files invalid
    if valid_files < total_files:
        sys.exit(1)


## Removed `check` subcommand to avoid redundancy with `validate`.


@cli.command()
@click.argument("file", type=click.Path(exists=True))
@click.option("--output", "-o", help="Output file for migrated configuration")
@click.option("--to-version", help="Target schema version")
def migrate(file, output, to_version):
    """Migrate a configuration to a different schema version."""

    path = Path(file)
    output_path = Path(output) if output else path.with_suffix(".migrated.yml")
    target_version = to_version or CURRENT_SCHEMA_VERSION

    console.print(f"[bold]Migrating {path.name} to schema v{target_version}[/bold]\n")

    try:
        # Load configuration
        with open(path, "r") as f:
            config = yaml.safe_load(f)

        # Detect current version
        migrator = SchemaMigrator()
        current_version = migrator.auto_detect_version(config)
        console.print(f"Current version: {current_version}")

        if current_version == target_version:
            console.print(
                "[yellow]Already at target version, no migration needed[/yellow]"
            )
            return

        # Migrate
        console.print(f"Migrating to: {target_version}")
        migrated = migrator.migrate(
            config, from_version=current_version, to_version=target_version
        )

        # Save
        with open(output_path, "w") as f:
            yaml.dump(migrated, f, default_flow_style=False, sort_keys=False)

        console.print(f"\n[green]✓ Migration complete![/green]")
        console.print(f"Output saved to: {output_path}")

        # Validate migrated config
        schema = generate_json_schema(version=target_version)
        is_valid, _ = validate_single_file(output_path, schema, show_details=False)

        if is_valid:
            console.print("[green]✓ Migrated configuration is valid[/green]")
        else:
            console.print(
                "[yellow]⚠ Migrated configuration may need manual adjustments[/yellow]"
            )

    except Exception as e:
        console.print(f"[red]✗ Migration failed: {e}[/red]")
        sys.exit(1)


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
    console.print("  • Full validation: suews-validate config.yml")
    console.print(
        "  • Read-only check: suews-validate -p C --dry-run configs/*.yml --format json"
    )
    console.print("  • Migrate: suews-validate schema migrate old_config.yml")


@cli.command()
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
@click.option(
    "--update", "-u", is_flag=True, help="Update schema_version field in files"
)
@click.option("--target-version", help="Target schema version to set when updating")
@click.option(
    "--backup", "-b", is_flag=True, default=True, help="Create backup before updating"
)
def version(files, update, target_version, backup):
    """Check or update schema_version in YAML files (alias to schema status/update)."""
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


@cli.command()
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    help="Output file for schema (if omitted, prints to console)",
)
@click.option("--version", help="Schema version to export (defaults to current)")
@click.option(
    "--format",
    "fmt",
    type=click.Choice(["json", "yaml"]),
    default="json",
    help="Output format",
)
def export(output, version, fmt):
    """Export the configuration JSON Schema as JSON or YAML."""
    try:
        from ..data_model.schema.version import CURRENT_SCHEMA_VERSION
        from ..data_model.schema.publisher import generate_json_schema
    except Exception as e:
        console.print(f"[red]✗ Unable to load schema publisher: {e}[/red]")
        sys.exit(1)

    schema_version = version or CURRENT_SCHEMA_VERSION

    try:
        schema = generate_json_schema(version=schema_version)
        if fmt == "yaml":
            content = yaml.dump(schema, default_flow_style=False, sort_keys=False)
            default_name = f"suews-schema-v{schema_version}.yaml"
        else:
            content = json.dumps(schema, indent=2)
            default_name = f"suews-schema-v{schema_version}.json"

        if output:
            Path(output).write_text(content)
            console.print(f"[green]✓ Schema exported to {output}[/green]")
        else:
            console.print(
                Panel(
                    Syntax(content, fmt, theme="monokai"),
                    title=f"Schema v{schema_version}",
                    subtitle=f"Save as: {default_name}",
                )
            )
    except Exception as e:
        console.print(f"[red]✗ Export failed: {e}[/red]")
        sys.exit(1)


def _check_experimental_features_restriction(user_yaml_file, mode):
    """Check for experimental features that are restricted in public mode.

    Returns:
        bool: True if validation passes (can proceed), False if should halt
    """
    if mode != "public":
        return True  # Dev mode allows all features

    try:
        with open(user_yaml_file, "r") as f:
            user_yaml_data = yaml.safe_load(f)
    except Exception as e:
        console.print(f"[red]✗ Error reading YAML file: {e}[/red]")
        return False

    restrictions_violated = []

    # Check STEBBS method restriction
    stebbs_method = None
    if (
        user_yaml_data
        and isinstance(user_yaml_data, dict)
        and "model" in user_yaml_data
        and isinstance(user_yaml_data["model"], dict)
        and "physics" in user_yaml_data["model"]
        and isinstance(user_yaml_data["model"]["physics"], dict)
        and "stebbsmethod" in user_yaml_data["model"]["physics"]
    ):
        stebbs_entry = user_yaml_data["model"]["physics"]["stebbsmethod"]
        # Handle both direct values and RefValue format
        if isinstance(stebbs_entry, dict) and "value" in stebbs_entry:
            stebbs_method = stebbs_entry["value"]
        else:
            stebbs_method = stebbs_entry

    if stebbs_method is not None and stebbs_method != 0:
        restrictions_violated.append("STEBBS method is enabled (stebbsmethod != 0)")

    # Add more restriction checks here as needed
    # Example for future experimental features:
    # if other_experimental_feature_enabled:
    #     restrictions_violated.append("Other experimental feature is enabled")

    # If any restrictions are violated, halt execution
    if restrictions_violated:
        console.print(
            "[red]✗ Configuration contains experimental features restricted in public mode:[/red]"
        )
        for restriction in restrictions_violated:
            console.print(f"  • {restriction}")
        console.print("\n[yellow]Options to resolve:[/yellow]")
        console.print("  1. Switch to dev mode: [cyan]--mode dev[/cyan]")
        console.print("  2. Disable experimental features in your YAML file and rerun")
        console.print("     Example: Set [cyan]stebbsmethod: {value: 0}[/cyan]")
        return False

    return True


def _format_phase_output(
    phase, success, input_file, output_file=None, report_file=None, errors=None
):
    """Format phase execution output based on format preference."""
    if JSONOutput:
        json_formatter = JSONOutput(command="suews-validate")
        output = json_formatter.phase_result(
            phase=phase,
            success=success,
            input_file=str(input_file),
            output_file=str(output_file) if output_file else None,
            report_file=str(report_file) if report_file else None,
            errors=errors if errors else None,
        )
        return output
    return None


def _execute_pipeline(file, pipeline, mode):
    """Run YAML processor phases A/B/C to validate and generate reports/YAML.

    Phase A: Update YAML structure and detect parameters
    Phase B: Scientific checks and adjustments
    Phase C: Pydantic validation with physics conditionals
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

    # Check for experimental features restrictions before proceeding
    if not _check_experimental_features_restriction(user_yaml_file, mode):
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

    # Execute selected phases (logic mirrors orchestrator.main for consistency)
    if pipeline == "A":
        ok = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="A",
            silent=True,
        )
        console.print(
            "[green]✓ Validation completed[/green]"
            if ok
            else "[red]✗ Validation failed[/red]"
        )
        if ok:
            console.print(f"Report: {report_file}")
            console.print(f"Updated YAML: {uptodate_file}")
        return 0 if ok else 1

    if pipeline == "B":
        ok = _processor_run_phase_b(
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
        )
        console.print(
            "[green]✓ Phase B completed[/green]"
            if ok
            else "[red]✗ Phase B failed[/red]"
        )
        if ok:
            console.print(f"Report: {science_report_file}")
            console.print(f"Updated YAML: {science_yaml_file}")
        else:
            # Show report file even on failure if it exists
            if os.path.exists(science_report_file):
                console.print(f"Report: {science_report_file}")
            if os.path.exists(science_yaml_file):
                console.print(f"Updated YAML: {science_yaml_file}")

            # Provide helpful guidance for Phase B failures
            console.print(
                "[yellow]Phase B requires Phase A to be completed first.[/yellow]"
            )
            console.print(
                f"[yellow]Try running: suews-validate --pipeline AB {user_yaml_file}[/yellow]"
            )
        return 0 if ok else 1

    if pipeline == "C":
        ok = _processor_run_phase_c(
            user_yaml_file,
            pydantic_yaml_file,
            pydantic_report_file,
            mode=mode,
            phases_run=["C"],
            silent=True,
        )
        console.print(
            "[green]✓ Phase C completed[/green]"
            if ok
            else "[red]✗ Phase C failed[/red]"
        )
        if ok:
            console.print(f"Report: {pydantic_report_file}")
            console.print(f"Updated YAML: {pydantic_yaml_file}")
        return 0 if ok else 1

    if pipeline == "AB":
        a_ok = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="AB",
            silent=True,
        )
        if not a_ok:
            # Preserve Phase A outputs as AB outputs
            console.print("[red]✗ Validation failed[/red]")
            if report_file:
                console.print(f"Report: {science_report_file}")
            if uptodate_file:
                console.print(f"Updated YAML: {science_yaml_file}")
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
            phase="AB",
            silent=True,
        )
        ok = a_ok and b_ok
        console.print(
            "[green]✓ Phase AB completed[/green]"
            if ok
            else "[red]✗ Phase AB failed[/red]"
        )
        if ok:
            console.print(f"Report: {science_report_file}")
            console.print(f"Updated YAML: {science_yaml_file}")
        return 0 if ok else 1

    if pipeline == "AC":
        a_ok = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="AC",
            silent=True,
        )
        if not a_ok:
            console.print("[red]✗ Validation failed[/red]")
            console.print(f"Report: {pydantic_report_file}")
            console.print(f"Updated YAML: {pydantic_yaml_file}")
            return 1

        c_ok = _processor_run_phase_c(
            uptodate_file,
            pydantic_yaml_file,
            pydantic_report_file,
            mode=mode,
            phase_a_report_file=report_file,
            phases_run=["A", "C"],
            silent=True,
        )
        ok = a_ok and c_ok
        console.print(
            "[green]✓ Phase AC completed[/green]"
            if ok
            else "[red]✗ Phase AC failed[/red]"
        )
        if ok:
            console.print(f"Report: {pydantic_report_file}")
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
            console.print(f"Report: {pydantic_report_file}")
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
            console.print(f"Report: {pydantic_report_file}")
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
        # Phase A failed in ABC - create final files from Phase A outputs
        import shutil

        try:
            if os.path.exists(report_file):
                shutil.move(report_file, pydantic_report_file)  # reportA → report
            if os.path.exists(uptodate_file):
                shutil.move(uptodate_file, pydantic_yaml_file)  # updatedA → updated
        except Exception:
            pass  # Don't fail if move doesn't work

        console.print("[red]✗ Validation failed[/red]")
        console.print(f"Report: {pydantic_report_file}")
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
        # Phase B failed in ABC - create final files with mixed content
        # Final YAML: from Phase A (last successful phase), Final Report: from Phase B (contains errors)
        import shutil

        try:
            # Create final YAML from Phase A (last successful phase)
            if os.path.exists(uptodate_file):
                shutil.copy2(
                    uptodate_file, pydantic_yaml_file
                )  # Copy updatedA → updated (keep intermediate)

            # Create final Report from Phase B (contains the actual errors we need to show user)
            if os.path.exists(science_report_file):
                shutil.move(
                    science_report_file, pydantic_report_file
                )  # Move reportB → report (don't keep intermediate)
            elif os.path.exists(report_file):
                # Fallback to Phase A report if Phase B report doesn't exist
                shutil.copy2(
                    report_file, pydantic_report_file
                )  # Copy reportA → report (keep intermediate)

            # Remove failed Phase B YAML
            if os.path.exists(science_yaml_file):
                os.remove(science_yaml_file)  # Remove failed Phase B YAML
        except Exception:
            pass  # Don't fail if cleanup doesn't work

        console.print("[red]✗ Validation failed[/red]")
        console.print(f"Report: {pydantic_report_file}")
        console.print(f"Updated YAML: {pydantic_yaml_file}")

        # Show intermediate file information
        console.print(
            "\n[bold]For more details on intermediate validation and updates, check:[/bold]"
        )
        if os.path.exists(report_file):
            console.print(
                f"[yellow]• YAML structure checks report: {report_file}[/yellow]"
            )
        if os.path.exists(uptodate_file):
            console.print(
                f"[yellow]• YAML structure checks updated file: {uptodate_file}[/yellow]"
            )

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

    if not c_ok:
        # Phase C failed in ABC - create final files with mixed content
        # Final YAML: from Phase B (last successful phase), Final Report: from Phase C (contains errors)
        import shutil

        try:
            # Create final YAML from Phase B (last successful phase)
            if os.path.exists(science_yaml_file):
                shutil.copy2(
                    science_yaml_file, pydantic_yaml_file
                )  # Copy updatedB → updated (keep intermediate)

            # Final Report should be from Phase C (contains the actual errors), but Phase C might not create a file
            # In this case, we'll rely on Phase C having already created pydantic_report_file, or use Phase B as fallback
            if not os.path.exists(pydantic_report_file) and os.path.exists(
                science_report_file
            ):
                shutil.copy2(
                    science_report_file, pydantic_report_file
                )  # Fallback: copy reportB → report
        except Exception:
            pass  # Don't fail if copy doesn't work

        console.print("[red]✗ Validation failed[/red]")
        console.print(f"Report: {pydantic_report_file}")
        console.print(f"Updated YAML: {pydantic_yaml_file}")

        # Show intermediate file information
        console.print(
            "\n[bold]For more details on intermediate validation and updates, check:[/bold]"
        )
        if os.path.exists(report_file):
            console.print(
                f"[yellow]• YAML structure checks report: {report_file}[/yellow]"
            )
        if os.path.exists(uptodate_file):
            console.print(
                f"[yellow]• YAML structure checks updated file: {uptodate_file}[/yellow]"
            )
        if os.path.exists(science_report_file):
            console.print(
                f"[yellow]• Physics checks report: {science_report_file}[/yellow]"
            )
        if os.path.exists(science_yaml_file):
            console.print(
                f"[yellow]• Physics checks updated file: {science_yaml_file}[/yellow]"
            )

        return 1

    # All phases succeeded
    ok = a_ok and b_ok and c_ok
    console.print("[green]✓ Validation completed[/green]")
    console.print(f"Report: {pydantic_report_file}")
    console.print(f"Updated YAML: {pydantic_yaml_file}")

    # Show intermediate file information
    console.print(
        "\n[bold]For more details on intermediate validation and updates, check:[/bold]"
    )
    if os.path.exists(report_file):
        console.print(f"[yellow]• YAML structure checks report: {report_file}[/yellow]")
    if os.path.exists(uptodate_file):
        console.print(
            f"[yellow]• YAML structure checks updated file: {uptodate_file}[/yellow]"
        )
    if os.path.exists(science_report_file):
        console.print(
            f"[yellow]• Physics checks report: {science_report_file}[/yellow]"
        )
    if os.path.exists(science_yaml_file):
        console.print(
            f"[yellow]• Physics checks updated file: {science_yaml_file}[/yellow]"
        )

    # Keep intermediate files: updatedA_*, reportA_*, updatedB_*, reportB_*
    # Final files: updated_*, report_* (created by Phase C)
    return 0


def main():
    """Main entry point."""
    cli()


if __name__ == "__main__":
    main()


@cli.group(name="schema", invoke_without_command=True)
@click.pass_context
def schema_group(ctx):
    """Schema operations: status, update, migrate, export, info.

    Invoked without subcommand, shows schema info.
    """
    if ctx.invoked_subcommand is None:
        _print_schema_info()


@schema_group.command("status")
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
def schema_status(files):
    """Show schema_version status and compatibility for files."""
    version(files, update=False, target_version=None, backup=True)


@schema_group.command("update")
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
@click.option("--target", help="Target schema version to set")
@click.option("--no-backup", is_flag=True, help="Do not create backup before updating")
def schema_update(files, target, no_backup):
    """Update schema_version for files to target (or current)."""
    version(files, update=True, target_version=target, backup=(not no_backup))


@schema_group.command("migrate")
@click.argument("file", type=click.Path(exists=True))
@click.option("--output", "-o", help="Output file for migrated configuration")
@click.option("--to", "to_version", help="Target schema version")
def schema_migrate(file, output, to_version):
    """Migrate a configuration to a different schema version."""
    migrate(file, output, to_version)


@schema_group.command("export")
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    help="Output file for schema (if omitted, prints to console)",
)
@click.option("--version", help="Schema version to export (defaults to current)")
@click.option(
    "--format",
    "fmt",
    type=click.Choice(["json", "yaml"]),
    default="json",
    help="Output format",
)
def schema_export(output, version, fmt):
    """Export the configuration JSON Schema as JSON or YAML."""
    export(output, version, fmt)


@schema_group.command("info")
def schema_info():
    """Show schema version info and docs links."""
    _print_schema_info()
