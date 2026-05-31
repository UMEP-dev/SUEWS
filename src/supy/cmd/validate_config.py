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
from contextlib import nullcontext as _nullcontext
from copy import deepcopy
from pathlib import Path
import importlib.resources
from typing import Optional, List
import jsonschema
from rich.console import Console
from rich.table import Table
from rich.panel import Panel
from rich.syntax import Syntax
from rich.progress import track

from ..data_model.validation.pipeline.report_writer import REPORT_WRITER

# Import the new JSON output formatter
try:
    from .json_output import JSONOutput, ErrorCode, ValidationError
except ImportError:
    # Fallback if module not available
    JSONOutput = None
    ErrorCode = None
    ValidationError = None

# Canonical SUEWS CLI JSON envelope (gh#1360 / gh#1368).
from .json_envelope import Envelope, _now_iso, silent_supy_logger

# Orchestrated YAML processor phases (A/B/C)
try:
    from ..data_model.validation.pipeline.orchestrator import (
        validate_input_file as _processor_validate_input_file,
        setup_output_paths as _processor_setup_output_paths,
        run_phase_a as _processor_run_phase_a,
        run_phase_b as _processor_run_phase_b,
        run_phase_c as _processor_run_phase_c,
        create_final_user_files as _processor_create_final_user_files,
        CRITICAL_PHYSICS_PARAMS,
        copy_report_with_json as _processor_copy_report_with_json,
        move_report_with_json as _processor_move_report_with_json,
        unlink_report_with_json as _processor_unlink_report_with_json,
    )
except Exception:
    _processor_validate_input_file = None
    _processor_setup_output_paths = None
    _processor_run_phase_a = None
    _processor_run_phase_b = None
    _processor_run_phase_c = None
    _processor_create_final_user_files = None
    CRITICAL_PHYSICS_PARAMS = ()
    _processor_copy_report_with_json = None
    _processor_move_report_with_json = None
    _processor_unlink_report_with_json = None

# Import from supy modules
try:
    from ..data_model.core.config import SUEWSConfig
    from ..data_model.core.field_renames import (
        read_physics_key,
        fold_stebbs_physics as _fold_stebbs_physics,
        STEBBS_PHYSICS_LEAF_RENAMES as _STEBBS_PHYSICS_LEAF_RENAMES,
    )
    from ..data_model.schema.version import CURRENT_SCHEMA_VERSION
    from ..data_model.schema.publisher import generate_json_schema
    from ..data_model.schema.migration import SchemaMigrator, check_migration_needed
except ImportError:
    # Fallback for direct script execution
    import sys

    sys.path.append(str(Path(__file__).parent.parent.parent))
    from supy.data_model.core.config import SUEWSConfig
    from supy.data_model.core.field_renames import (
        read_physics_key,
        fold_stebbs_physics as _fold_stebbs_physics,
        STEBBS_PHYSICS_LEAF_RENAMES as _STEBBS_PHYSICS_LEAF_RENAMES,
    )
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION
    from supy.data_model.schema.publisher import generate_json_schema
    from supy.data_model.schema.migration import SchemaMigrator, check_migration_needed

# Sentinel used by the critical-physics-presence check below; module-level
# so identity comparison stays stable across calls.
_PHYSICS_KEY_MISSING = object()

console = Console()


def validate_single_file(
    file_path: Path,
    schema: dict,
    show_details: bool = True,
    schema_version: Optional[str] = None,
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

        target_is_current = (
            schema_version is None or schema_version == CURRENT_SCHEMA_VERSION
        )

        # Only flag migration pressure when validating against the current
        # schema. An explicit older target version is a schema-only check.
        if target_is_current and check_migration_needed(str(file_path)):
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
                path = " -> ".join(str(p) for p in error.path) if error.path else "root"
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

        # Structural-presence check for critical physics parameters (gh#1409).
        # Pydantic's ModelPhysics auto-fills missing fields with enum defaults
        # (e.g. NetRadiationMethod.NARP), so a YAML with `model.physics: {}`
        # passes both jsonschema and Pydantic validation silently. The full
        # pipeline's Phase A flags these as ACTION NEEDED; we replicate that
        # rule here so the dry-run JSON path agrees with the full pipeline
        # on user-facing semantics. Use `read_physics_key` so accepted legacy
        # aliases (e.g. `netradiationmethod`) are recognised the same way the
        # full SUEWSConfig.from_yaml path recognises them — otherwise this
        # check would false-negative on configurations the pipeline accepts.
        if target_is_current and CRITICAL_PHYSICS_PARAMS:
            user_physics = (
                (config or {}).get("model", {}).get("physics") or {}
            )
            if not isinstance(user_physics, dict):
                user_physics = {}
            # gh#1456: the STEBBS switches moved under model.physics.stebbs.*.
            # The critical-physics list carries their bare leaf names, so look
            # those leaves up inside the nested `stebbs` block rather than flat
            # on physics. The nested-leaf names are the values of
            # STEBBS_PHYSICS_LEAF_RENAMES.
            #
            # A current-target YAML may still be written in the legacy FLAT
            # form (`capacitance`/`setpoint`/`same_*` directly under
            # model.physics, with a flat `stebbs` master toggle) -- the real
            # loader (SUEWSConfig.from_yaml) folds those flat keys to the
            # nested shape and accepts them. Apply the SAME fold to a deep copy
            # here before the per-leaf lookup so the dry-run path agrees with
            # the loader instead of false-reporting the leaves missing.
            folded_physics = _fold_stebbs_physics(
                deepcopy(user_physics), "model.physics"
            )
            stebbs_leaf_names = set(_STEBBS_PHYSICS_LEAF_RENAMES.values())
            user_stebbs = folded_physics.get("stebbs")
            if not isinstance(user_stebbs, dict):
                user_stebbs = {}
            for param_name in CRITICAL_PHYSICS_PARAMS:
                if param_name in stebbs_leaf_names:
                    container = user_stebbs
                    field_path = f"model.physics.stebbs.{param_name}"
                else:
                    container = user_physics
                    field_path = f"model.physics.{param_name}"
                if read_physics_key(
                    container, param_name, default=_PHYSICS_KEY_MISSING
                ) is _PHYSICS_KEY_MISSING:
                    message = (
                        f"{field_path}: required physics parameter is missing "
                        "from the input YAML; runtime requires an explicit "
                        "choice rather than the Pydantic default"
                    )
                    if ValidationError and ErrorCode:
                        errors.append(
                            ValidationError(
                                code=ErrorCode.MISSING_REQUIRED_FIELD,
                                message=message,
                                field=field_path,
                                location=str(file_path),
                            )
                        )
                    else:
                        errors.append(message)

        # Try configuration consistency validation for additional checks.
        # Mirror schema_cli: explicit older target versions stay schema-only,
        # while default/current validation follows the public from_yaml() path.
        if target_is_current:
            try:
                SUEWSConfig.from_yaml(str(file_path))
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
                    errors.append(
                        f"Configuration consistency validation: {str(e)}"
                    )

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


def _emit_validation_envelope(
    results: List[dict],
    schema_version: Optional[str],
    started_at: str,
) -> bool:
    """Emit the canonical Envelope for a list of per-file validation results.

    Each ``results`` entry is the same dict shape that the validator
    pipeline already constructs (``file``, ``valid``, ``errors``,
    ``error_count``); ``errors`` may hold ``ValidationError.to_dict()``
    payloads or plain strings.

    Returns
    -------
    bool
        ``True`` when every file validated cleanly. Callers use this to
        choose the process exit code.
    """
    all_valid = all(r["valid"] for r in results)

    list_errors: List[dict] = []
    for record in results:
        if record["valid"]:
            continue
        for err in record["errors"]:
            if isinstance(err, dict):
                schema_path = (
                    err.get("path") or err.get("field") or err.get("location")
                )
                message = err.get("message", "")
                envelope_err = {
                    "file": record["file"],
                    "message": message,
                    "schema_path": schema_path,
                    "hint": message,
                }
                for key in ("code", "code_name", "severity", "site_gridid"):
                    if err.get(key) is not None:
                        envelope_err[key] = err[key]
                list_errors.append(envelope_err)
            else:
                list_errors.append({
                    "file": record["file"],
                    "message": str(err),
                    "schema_path": None,
                    "hint": str(err),
                })

    data = {
        "schema_version": schema_version,
        "files": [
            {
                "file": record["file"],
                "valid": record["valid"],
                "error_count": record.get("error_count", 0),
            }
            for record in results
        ],
        "is_valid": all_valid,
    }

    command_str = " ".join(sys.argv) if sys.argv else "suews validate"
    if all_valid:
        Envelope.success(
            data=data, command=command_str, started_at=started_at
        ).emit()
    else:
        Envelope.error(
            errors=list_errors,
            command=command_str,
            data=data,
            started_at=started_at,
        ).emit()
    return all_valid


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
@click.option(
    "--forcing",
    "-f",
    type=click.Choice(["on", "off"]),
    default="on",
    help="Enable/disable forcing data validation (default: on)",
)
@click.option(
    "--science-fixes",
    type=click.Choice(["suggest", "apply", "off"]),
    default="suggest",
    show_default=True,
    help="Handle Phase B scientific transformations: suggest, apply, or off",
)
@click.pass_context
def cli(
    ctx,
    files,
    pipeline,
    mode,
    dry_run,
    out_format,
    schema_version,
    forcing,
    science_fixes,
):
    """SUEWS Configuration Validator.

    Default behavior: run the complete validation pipeline on FILE. Use the
    validate subcommand for batch schema checks; use `suews schema` for schema
    lifecycle operations.
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
                    with (
                        silent_supy_logger() if out_format == "json" else _nullcontext()
                    ):
                        is_valid, errors = validate_single_file(
                            path,
                            schema,
                            show_details=True,
                            schema_version=target_version,
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
                    started_at = _now_iso()
                    all_valid = _emit_validation_envelope(
                        results, target_version, started_at
                    )
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
            with (silent_supy_logger() if out_format == "json" else _nullcontext()):
                is_valid, errors = validate_single_file(
                    path,
                    schema,
                    show_details=True,
                    schema_version=target_version,
                )

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
                started_at = _now_iso()
                _emit_validation_envelope(result, target_version, started_at)
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
        code = _execute_pipeline(
            file=files[0],
            pipeline=pipeline,
            mode=mode,
            forcing=forcing,
            science_fixes=science_fixes,
            out_format=out_format,
        )
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
    """Validate SUEWS YAML configuration files (schema + consistency checks)."""

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
        with (silent_supy_logger() if format == "json" else _nullcontext()):
            is_valid, errors = validate_single_file(
                path,
                schema,
                show_details=verbose,
                schema_version=schema_version,
            )

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
        started_at = _now_iso()
        _emit_validation_envelope(results, version, started_at)
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


@cli.command(hidden=True)
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
        is_valid, _ = validate_single_file(
            output_path,
            schema,
            show_details=False,
            schema_version=target_version,
        )

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
        marker = ">" if version == CURRENT_SCHEMA_VERSION else " "
        console.print(f"  {marker} v{version}: {description}")

    console.print("\n[bold]Schema Files:[/bold]")
    console.print("  • JSON Schema: schemas/latest/schema.json")
    console.print("  • YAML Schema: schemas/latest/schema.yaml")
    console.print("  • Documentation: docs/source/inputs/yaml/schema_versioning.rst")

    console.print("\n[bold]Validation Commands:[/bold]")
    console.print("  • Full validation: suews validate config.yml")
    console.print(
        "  • Read-only check: suews validate -p C --dry-run configs/*.yml --format json"
    )
    console.print("  • Migrate: suews schema migrate old_config.yml")


@cli.command(hidden=True)
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
                    action = f"Updated -> {new_version}"
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


@cli.command(hidden=True)
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


def _experimental_features_restriction(user_yaml_file, mode):
    """Return public-mode experimental-feature violations.

    Returns:
        tuple: ``(ok, restrictions, read_error)`` where ``ok`` is false
        when validation should halt.
    """
    if mode != "public":
        return True, [], None  # Dev mode allows all features

    try:
        with open(user_yaml_file, "r") as f:
            user_yaml_data = yaml.safe_load(f)
    except Exception as e:
        return False, [], f"Error reading YAML file: {e}"

    # Read physics keys via helpers that accept both the new snake_case name
    # and its legacy alias — the Pydantic shim accepts both, so this gate must
    # as well.
    from ..data_model.core.field_renames import read_physics_key
    from ..data_model.validation.core.yaml_helpers import get_stebbsmethod_value

    physics = (
        user_yaml_data.get("model", {}).get("physics", {})
        if isinstance(user_yaml_data, dict)
        else {}
    )
    restrictions_violated = []

    # gh#1456: use the shared composer so nested bool-like strings (`off`,
    # `false`, `0`, etc.) follow the same semantics as Pydantic.
    try:
        stebbsmethod = get_stebbsmethod_value(physics)
    except ValueError:
        stebbsmethod = None  # Let the normal validation pipeline report it.
    if stebbsmethod is not None and stebbsmethod != 0:
        restrictions_violated.append("STEBBS is enabled (stebbs.enabled is true)")

    snowuse = read_physics_key(physics, "snow_use")
    if snowuse is not None and snowuse != 0:
        restrictions_violated.append("Snow calculations are enabled (snow_use != 0)")

    if restrictions_violated:
        return False, restrictions_violated, None

    return True, [], None


def _print_experimental_features_restriction(restrictions_violated):
    """Print the legacy table-mode experimental-feature restriction message."""
    console.print(
        "[red]✗ Configuration contains experimental features restricted in public mode:[/red]"
    )
    for restriction in restrictions_violated:
        console.print(f"  • {restriction}")
    console.print("\n[yellow]Options to resolve:[/yellow]")
    console.print("  1. Switch to dev mode: [cyan]--mode dev[/cyan]")
    console.print("  2. Disable experimental features in your YAML file and rerun")
    console.print("     Example: Set [cyan]stebbs.enabled: {value: false}[/cyan]")


def _check_experimental_features_restriction(user_yaml_file, mode):
    """Check for experimental features that are restricted in public mode.

    Returns:
        bool: True if validation passes (can proceed), False if should halt
    """
    ok, restrictions_violated, read_error = _experimental_features_restriction(
        user_yaml_file, mode
    )
    if ok:
        return True
    if read_error:
        console.print(f"[red]✗ {read_error}[/red]")
    else:
        _print_experimental_features_restriction(restrictions_violated)
    return False


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


def _write_consolidated_sidecar(phases: list, report_path) -> None:
    """Write the consolidated multi-phase ``ValidationReport`` JSON sidecar.

    gh#1467: the CLI sidecar (``<report>.json``) carries the full
    multi-phase ``ValidationReport`` (every phase, every severity) so
    non-error informational messages reach machine consumers, not just
    validation errors. The payload is identical to the stdout
    ``--format json`` envelope's ``data.validation_report``.

    This supersedes the Phase-C-only ``PhaseReport`` sidecar that
    ``run_phase_c`` (and the move/copy helpers) leave at this path during
    the pipeline run. A serialisation or I/O failure is swallowed (with a
    ``SUEWS_DEBUG`` note) so a sidecar problem never breaks validation,
    mirroring ``_sync_report_json_paths``.

    Parameters
    ----------
    phases : list
        The ``PhaseReport`` objects for the phases that ran, in order.
    report_path : str or pathlib.Path
        Final text report path; the sidecar is written beside it with a
        ``.json`` suffix.
    """
    if not report_path:
        return

    from ..data_model.validation.pipeline.report_schema import (
        JSON_REPORT_WRITER,
        ValidationReport,
    )

    json_path = Path(report_path).with_suffix(".json")
    try:
        JSON_REPORT_WRITER.write(json_path, ValidationReport(phases=list(phases)))
    except (OSError, TypeError, ValueError) as exc:
        if os.environ.get("SUEWS_DEBUG", "").lower() in ("1", "true", "yes"):
            print(
                f"[DEBUG] consolidated sidecar write failed for {json_path}: {exc}",
                file=sys.stderr,
            )


def _emit_pipeline_result(
    phases: list,
    report_path,
    yaml_path,
    *,
    out_format: str = "table",
    command: str = "suews validate",
    started_at: Optional[str] = None,
) -> int:
    """Emit the pipeline outcome and return the exit code (gh#1409 follow-up).

    Replaces the bespoke ``console.print(...)`` + ``return 0/1`` block at
    the end of every pipeline branch in :func:`_execute_pipeline` so all
    branches funnel through one place. Honours ``out_format``:

    - ``"table"`` (default) — preserves the legacy text output: status
      banner + ``Report:`` / ``Updated YAML:`` lines printed to console.
    - ``"json"`` — emits a single canonical envelope on stdout. ``data``
      carries the structured ``ValidationReport`` (phase by phase) plus
      pointers to ``report_file`` and ``updated_yaml`` so non-MCP
      consumers can tell when full-pipeline output is available without
      parsing the human-readable report.

    Parameters
    ----------
    phases
        List of :class:`PhaseReport` objects in the order they ran. May
        contain a partial set when an early phase failed and the caller
        bailed out before later phases.
    report_path, yaml_path
        Absolute or workspace-relative paths to the consolidated report
        file and updated YAML.
    out_format
        ``"table"`` or ``"json"``.
    command
        CLI command string for envelope provenance.
    started_at
        ISO 8601 start timestamp for envelope provenance.
    """
    from ..data_model.validation.pipeline.report_schema import (
        ValidationReport,
    )

    has_errors = any(p.has_errors for p in phases)
    ok = not has_errors

    # gh#1467: in a consolidated run the per-phase intermediate artefacts
    # (temp_report*_ reports, updated*_ YAMLs) are merged into and deleted in
    # favour of the single final report/YAML. Their in-memory PhaseReport
    # objects still carry the intermediate paths, so normalise each phase's
    # paths to the surviving artefacts before publishing them — otherwise the
    # JSON sidecar and envelope advertise paths to files that no longer exist.
    if report_path:
        final_text_report = str(report_path)
        final_json_report = str(Path(report_path).with_suffix(".json"))
        for phase_report in phases:
            # The consolidated report supersedes the per-phase reports.
            phase_report.text_report_path = final_text_report
            phase_report.json_report_path = final_json_report
            # Per-phase YAML artefacts are intermediate: a phase's input or
            # output may have been consolidated/deleted, or never produced by
            # a failed phase. Clear any path that no longer resolves rather
            # than mislabel another phase's surviving file as this one's.
            # Paths that still exist (the original user YAML, or the final
            # phase's own output) are left intact, so the surviving final
            # YAML is still attributed to the phase that actually produced it.
            if phase_report.yaml_in and not Path(phase_report.yaml_in).exists():
                phase_report.yaml_in = None
            if phase_report.yaml_out and not Path(phase_report.yaml_out).exists():
                phase_report.yaml_out = None

    # gh#1467: persist the consolidated multi-phase ValidationReport as the
    # JSON sidecar next to the final text report, in BOTH table and json
    # output modes (the sidecar is a disk artefact, independent of stdout).
    _write_consolidated_sidecar(phases, report_path)

    if out_format == "json":
        validation_report = ValidationReport(phases=list(phases))
        data = {
            "validation_report": validation_report.to_dict(),
            "report_file": str(report_path),
            "updated_yaml": str(yaml_path),
            "phases_run": [p.phase for p in phases],
        }
        warnings = [
            {
                "phase": p.phase,
                "code": issue.code,
                "message": issue.message,
                "severity": issue.severity,
                "yaml_path": issue.yaml_path,
            }
            for p in phases
            for issue in p.issues
            if issue.severity == "WARNING"
        ]
        if ok:
            Envelope.success(
                data=data,
                command=command,
                warnings=warnings,
                started_at=started_at,
            ).emit()
        else:
            errors = [
                {
                    "phase": p.phase,
                    "code": issue.code,
                    "message": issue.message,
                    "severity": issue.severity,
                    "yaml_path": issue.yaml_path,
                }
                for p in phases
                for issue in p.issues
                if issue.severity == "ERROR"
            ]
            Envelope.error(
                errors=errors or [{"message": "Pipeline reported errors"}],
                command=command,
                data=data,
                started_at=started_at,
            ).emit()
        return 0 if ok else 1

    # Default: legacy text output, preserved verbatim.
    console.print(
        "[green]✓ Validation completed[/green]"
        if ok
        else "[red]✗ Validation failed[/red]"
    )
    if Path(report_path).exists():
        console.print(f"Report: {report_path}")
    if Path(yaml_path).exists():
        console.print(f"Updated YAML: {yaml_path}")
    return 0 if ok else 1


def _emit_pipeline_startup_error(
    message: str,
    *,
    out_format: str,
    command: str,
    started_at: Optional[str],
    code: str,
    data: Optional[dict] = None,
) -> int:
    """Emit an early pipeline error before phase reports exist."""
    if out_format == "json":
        Envelope.error(
            errors=[{"message": message, "code": code}],
            command=command,
            data=data or {},
            started_at=started_at,
        ).emit()
    else:
        console.print(f"[red]✗ {message}[/red]")
    return 1


def _execute_pipeline(
    file,
    pipeline,
    mode,
    forcing="on",
    science_fixes="suggest",
    out_format: str = "table",
):
    """Run YAML validation pipeline to validate and generate reports/YAML.

    The validation system uses multiple internal phases:
    - Structure validation: Update YAML structure and detect parameters
    - Scientific validation: Apply scientific checks and adjustments
    - Model validation: Pydantic validation with physics conditionals

    All findings are consolidated into a single report and updated YAML file.

    When ``out_format == "json"`` the function emits the canonical SUEWS
    envelope on stdout (via :func:`_emit_pipeline_result`) instead of the
    legacy text banner; the report file and updated YAML are still
    produced on disk in both modes (gh#1409 follow-up).
    """
    started_at = _now_iso()
    command_str = f"suews validate -p {pipeline} {file} --format {out_format}"
    debug = os.environ.get("SUEWS_DEBUG", "").lower() in ("1", "true", "yes")
    if debug:
        print(f"[DEBUG] _execute_pipeline called", file=sys.stderr)
        print(f"[DEBUG]   file: {file}", file=sys.stderr)
        print(f"[DEBUG]   pipeline: {pipeline}", file=sys.stderr)
        print(f"[DEBUG]   mode: {mode}", file=sys.stderr)
        print(f"[DEBUG]   science_fixes: {science_fixes}", file=sys.stderr)

    # Ensure processor is importable
    if not all([
        _processor_validate_input_file,
        _processor_setup_output_paths,
        _processor_run_phase_a,
        _processor_run_phase_b,
        _processor_run_phase_c,
        _processor_create_final_user_files,
        _processor_copy_report_with_json,
        _processor_move_report_with_json,
        _processor_unlink_report_with_json,
    ]):
        return _emit_pipeline_startup_error(
            "YAML processor is unavailable. Ensure supy.data_model.validation.pipeline is present.",
            out_format=out_format,
            command=command_str,
            started_at=started_at,
            code="yaml_processor_unavailable",
        )

    # Validate input and prepare paths
    try:
        user_yaml_file = _processor_validate_input_file(file)
    except Exception as e:
        return _emit_pipeline_startup_error(
            str(e),
            out_format=out_format,
            command=command_str,
            started_at=started_at,
            code="invalid_input_file",
            data={"file": str(file)},
        )

    ok_experimental, restrictions_violated, read_error = (
        _experimental_features_restriction(user_yaml_file, mode)
    )
    if not ok_experimental:
        if read_error:
            return _emit_pipeline_startup_error(
                read_error,
                out_format=out_format,
                command=command_str,
                started_at=started_at,
                code="yaml_read_failed",
                data={"file": str(user_yaml_file)},
            )
        if out_format == "json":
            return _emit_pipeline_startup_error(
                "Configuration contains experimental features restricted in public mode.",
                out_format=out_format,
                command=command_str,
                started_at=started_at,
                code="experimental_features_restricted",
                data={
                    "file": str(user_yaml_file),
                    "mode": mode,
                    "restrictions": restrictions_violated,
                },
            )
        _print_experimental_features_restriction(restrictions_violated)
        return 1

    # Use importlib.resources for robust package resource access
    # Use string "supy" instead of module reference to avoid NameError
    # Read the standard config content directly to avoid temp file cleanup issues
    # when package is installed from a wheel (as_file() creates temp files that
    # are deleted when context manager exits)
    import tempfile

    sample_data_files = importlib.resources.files("supy") / "sample_data"
    standard_config_content = (sample_data_files / "sample_config.yml").read_text()

    # Write to a persistent temp file that won't be deleted during pipeline execution
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".yml", delete=False
    ) as tmp_standard:
        tmp_standard.write(standard_config_content)
        standard_yaml_file = tmp_standard.name

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
        phase_a_report = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="A",
            silent=True,
            forcing=forcing,
        )
        return _emit_pipeline_result(
            phases=[phase_a_report],
            report_path=report_file,
            yaml_path=uptodate_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    if pipeline == "B":
        phase_b_report = _processor_run_phase_b(
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
            science_fixes=science_fixes,
        )
        return _emit_pipeline_result(
            phases=[phase_b_report],
            report_path=science_report_file,
            yaml_path=science_yaml_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    if pipeline == "C":
        phase_c_report = _processor_run_phase_c(
            user_yaml_file,
            pydantic_yaml_file,
            pydantic_report_file,
            mode=mode,
            phases_run=["C"],
            silent=True,
        )
        return _emit_pipeline_result(
            phases=[phase_c_report],
            report_path=pydantic_report_file,
            yaml_path=pydantic_yaml_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    if pipeline == "AB":
        a_ok_report = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="AB",
            silent=True,
            forcing=forcing,
        )
        a_ok = not a_ok_report.has_errors
        if not a_ok:
            # Phase A failed in AB workflow - create final user files from Phase A outputs
            final_yaml, final_report = _processor_create_final_user_files(
                user_yaml_file, uptodate_file, report_file
            )
            return _emit_pipeline_result(
                phases=[a_ok_report],
                report_path=final_report,
                yaml_path=final_yaml,
                out_format=out_format,
                command=command_str,
                started_at=started_at,
            )

        b_ok_report = _processor_run_phase_b(
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
            science_fixes=science_fixes,
        )
        b_ok = not b_ok_report.has_errors

        if not b_ok:
            # Phase B failed in AB workflow - create final user files from Phase B error report and Phase A YAML
            import shutil

            # Determine final file paths
            dirname = Path(user_yaml_file).parent
            basename = Path(user_yaml_file).name
            name_without_ext = Path(user_yaml_file).stem
            final_yaml = dirname / f"updated_{basename}"
            final_report = dirname / f"report_{name_without_ext}.txt"

            try:
                # Use Phase A YAML as final (last successful phase)
                if Path(uptodate_file).exists():
                    shutil.move(str(uptodate_file), str(final_yaml))
                else:
                    console.print(
                        f"[yellow]Warning: Updated YAML not found: {uptodate_file}[/yellow]"
                    )

                # Use Phase B report as final (contains the errors)
                if Path(science_report_file).exists():
                    _processor_move_report_with_json(
                        science_report_file, final_report, final_yaml
                    )

                # Clean up intermediate Phase A report
                _processor_unlink_report_with_json(report_file)

                # Remove failed Phase B YAML if it exists (only if different from final_yaml)
                if Path(science_yaml_file).exists() and str(science_yaml_file) != str(
                    final_yaml
                ):
                    Path(science_yaml_file).unlink()
            except Exception as e:
                console.print(f"[yellow]Warning during cleanup: {e}[/yellow]")

            return _emit_pipeline_result(
                phases=[a_ok_report, b_ok_report],
                report_path=final_report,
                yaml_path=final_yaml,
                out_format=out_format,
                command=command_str,
                started_at=started_at,
            )

        # Both A and B succeeded - consolidate reports and clean up intermediate files
        from ..data_model.validation.pipeline.orchestrator import (
            extract_no_action_messages_from_report,
            create_consolidated_report,
        )

        try:
            # Extract NO ACTION NEEDED messages from both phases
            all_messages = []
            if Path(report_file).exists():
                all_messages.extend(extract_no_action_messages_from_report(report_file))
            if Path(science_report_file).exists():
                all_messages.extend(
                    extract_no_action_messages_from_report(science_report_file)
                )

            # Create consolidated final report
            create_consolidated_report(
                phases_run=["A", "B"],
                no_action_messages=all_messages,
                final_report_file=science_report_file,
                mode=mode,
            )

            # Clean up intermediate files
            _processor_unlink_report_with_json(report_file)  # Remove Phase A report
            if Path(uptodate_file).exists():
                Path(uptodate_file).unlink()  # Remove Phase A YAML
        except Exception:
            pass

        return _emit_pipeline_result(
            phases=[a_ok_report, b_ok_report],
            report_path=science_report_file,
            yaml_path=science_yaml_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    if pipeline == "AC":
        a_ok_report = _processor_run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_file,
            mode=mode,
            phase="AC",
            silent=True,
            forcing=forcing,
        )
        a_ok = not a_ok_report.has_errors
        if not a_ok:
            # Phase A failed in AC workflow - create final user files from Phase A outputs
            final_yaml, final_report = _processor_create_final_user_files(
                user_yaml_file, uptodate_file, report_file
            )
            return _emit_pipeline_result(
                phases=[a_ok_report],
                report_path=final_report,
                yaml_path=final_yaml,
                out_format=out_format,
                command=command_str,
                started_at=started_at,
            )

        c_ok_report = _processor_run_phase_c(
            uptodate_file,
            pydantic_yaml_file,
            pydantic_report_file,
            mode=mode,
            phase_a_report_file=report_file,
            phases_run=["A", "C"],
            silent=True,
        )
        c_ok = not c_ok_report.has_errors

        if not c_ok:
            # Phase C failed in AC workflow - create final user files from Phase C error report and Phase A YAML
            import shutil

            # Determine final file paths
            dirname = Path(user_yaml_file).parent
            basename = Path(user_yaml_file).name
            name_without_ext = Path(user_yaml_file).stem
            final_yaml = dirname / f"updated_{basename}"
            final_report = dirname / f"report_{name_without_ext}.txt"

            try:
                # Use Phase A YAML as final (last successful phase)
                if Path(uptodate_file).exists():
                    shutil.move(str(uptodate_file), str(final_yaml))

                # Phase C report should already be at pydantic_report_file (final name)
                # Clean up intermediate Phase A report
                _processor_unlink_report_with_json(report_file)

                # Remove failed Phase C YAML if it exists (only if different from final_yaml)
                if Path(pydantic_yaml_file).exists() and str(pydantic_yaml_file) != str(
                    final_yaml
                ):
                    Path(pydantic_yaml_file).unlink()
            except Exception as e:
                console.print(f"[yellow]Warning during cleanup: {e}[/yellow]")

            return _emit_pipeline_result(
                phases=[a_ok_report, c_ok_report],
                report_path=final_report,
                yaml_path=final_yaml,
                out_format=out_format,
                command=command_str,
                started_at=started_at,
            )

        # Both A and C succeeded - consolidate reports and clean up intermediate files
        from ..data_model.validation.pipeline.orchestrator import (
            extract_no_action_messages_from_report,
            create_consolidated_report,
        )

        try:
            # Extract NO ACTION NEEDED messages from both phases
            all_messages = []
            if Path(report_file).exists():
                all_messages.extend(extract_no_action_messages_from_report(report_file))
            if Path(pydantic_report_file).exists():
                all_messages.extend(
                    extract_no_action_messages_from_report(pydantic_report_file)
                )

            # Create consolidated final report
            create_consolidated_report(
                phases_run=["A", "C"],
                no_action_messages=all_messages,
                final_report_file=pydantic_report_file,
                mode=mode,
            )

            # Clean up intermediate files
            _processor_unlink_report_with_json(report_file)  # Remove Phase A report
            if Path(uptodate_file).exists():
                Path(uptodate_file).unlink()  # Remove Phase A YAML
        except Exception:
            pass

        return _emit_pipeline_result(
            phases=[a_ok_report, c_ok_report],
            report_path=pydantic_report_file,
            yaml_path=pydantic_yaml_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    if pipeline == "BC":
        b_ok_report = _processor_run_phase_b(
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
            science_fixes=science_fixes,
        )
        b_ok = not b_ok_report.has_errors
        if not b_ok:
            # Phase B failed in BC workflow - create final user files from Phase B outputs
            import shutil

            # Determine final file paths
            dirname = Path(user_yaml_file).parent
            basename = Path(user_yaml_file).name
            name_without_ext = Path(user_yaml_file).stem
            final_yaml = dirname / f"updated_{basename}"
            final_report = dirname / f"report_{name_without_ext}.txt"

            try:
                # Use Phase B YAML as final (if exists)
                if Path(science_yaml_file).exists():
                    shutil.move(str(science_yaml_file), str(final_yaml))

                # Use Phase B report as final
                if Path(science_report_file).exists():
                    _processor_move_report_with_json(
                        science_report_file, final_report, final_yaml
                    )
            except Exception as e:
                console.print(f"[yellow]Warning during cleanup: {e}[/yellow]")

            return _emit_pipeline_result(
                phases=[b_ok_report],
                report_path=final_report,
                yaml_path=final_yaml,
                out_format=out_format,
                command=command_str,
                started_at=started_at,
            )

        c_ok_report = _processor_run_phase_c(
            science_yaml_file,
            pydantic_yaml_file,
            pydantic_report_file,
            mode=mode,
            phases_run=["B", "C"],
            silent=True,
        )
        c_ok = not c_ok_report.has_errors

        if not c_ok:
            # Phase C failed in BC workflow - consolidate Phase B messages into Phase C error report
            from ..data_model.validation.pipeline.orchestrator import (
                extract_no_action_messages_from_report,
            )
            import shutil

            # Determine final file paths
            dirname = Path(user_yaml_file).parent
            basename = Path(user_yaml_file).name
            name_without_ext = Path(user_yaml_file).stem
            final_yaml = dirname / f"updated_{basename}"
            final_report = dirname / f"report_{name_without_ext}.txt"

            try:
                # Extract NO ACTION NEEDED messages from Phase B
                phase_b_messages = []
                if Path(science_report_file).exists():
                    phase_b_messages = extract_no_action_messages_from_report(
                        science_report_file
                    )

                # Read Phase C error report and append Phase B messages
                if Path(pydantic_report_file).exists():
                    phase_c_content = REPORT_WRITER.read(pydantic_report_file)

                    # Append Phase B NO ACTION NEEDED messages to Phase C report
                    if phase_b_messages:
                        # Remove the closing separator and any trailing separators from Phase C report
                        lines = phase_c_content.rstrip().split("\n")
                        while lines and lines[-1].strip() == f"# {'=' * 50}":
                            lines.pop()
                        phase_c_content = "\n".join(lines)

                        # Ensure proper spacing before INFO section
                        if not phase_c_content.endswith("\n\n"):
                            phase_c_content += "\n"

                        # Add INFO section
                        phase_c_content += "\n## INFO"

                        # Add Phase B messages
                        for msg in phase_b_messages:
                            phase_c_content += f"\n{msg}"

                        # Add closing separator
                        phase_c_content += f"\n\n# {'=' * 50}\n"

                        # Write consolidated report
                        REPORT_WRITER.write(pydantic_report_file, phase_c_content)

                # Use Phase B YAML as final (last successful phase)
                if Path(science_yaml_file).exists():
                    shutil.move(str(science_yaml_file), str(final_yaml))

                # Clean up intermediate Phase B report (now that we've extracted messages)
                _processor_unlink_report_with_json(science_report_file)

                # Remove failed Phase C YAML if it exists (only if different from final_yaml)
                if Path(pydantic_yaml_file).exists() and str(pydantic_yaml_file) != str(
                    final_yaml
                ):
                    Path(pydantic_yaml_file).unlink()
            except Exception as e:
                console.print(f"[yellow]Warning during cleanup: {e}[/yellow]")

            return _emit_pipeline_result(
                phases=[b_ok_report, c_ok_report],
                report_path=final_report,
                yaml_path=final_yaml,
                out_format=out_format,
                command=command_str,
                started_at=started_at,
            )

        # Both B and C succeeded - consolidate reports and clean up intermediate files
        from ..data_model.validation.pipeline.orchestrator import (
            extract_no_action_messages_from_report,
            create_consolidated_report,
        )

        try:
            # Extract NO ACTION NEEDED messages from both phases
            all_messages = []
            if Path(science_report_file).exists():
                all_messages.extend(
                    extract_no_action_messages_from_report(science_report_file)
                )
            if Path(pydantic_report_file).exists():
                all_messages.extend(
                    extract_no_action_messages_from_report(pydantic_report_file)
                )

            # Create consolidated final report
            create_consolidated_report(
                phases_run=["B", "C"],
                no_action_messages=all_messages,
                final_report_file=pydantic_report_file,
                mode=mode,
            )

            # Clean up intermediate files
            _processor_unlink_report_with_json(
                science_report_file
            )  # Remove Phase B report
            if Path(science_yaml_file).exists():
                Path(science_yaml_file).unlink()  # Remove Phase B YAML
        except Exception:
            pass

        return _emit_pipeline_result(
            phases=[b_ok_report, c_ok_report],
            report_path=pydantic_report_file,
            yaml_path=pydantic_yaml_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    # Default: ABC
    a_ok_report = _processor_run_phase_a(
        user_yaml_file,
        standard_yaml_file,
        uptodate_file,
        report_file,
        mode=mode,
        phase="ABC",
        silent=True,
        forcing=forcing,
    )
    a_ok = not a_ok_report.has_errors
    if not a_ok:
        # Phase A failed in ABC - create final files from Phase A outputs
        if debug:
            print(f"[DEBUG] Phase A failed in ABC pipeline", file=sys.stderr)
            print(f"[DEBUG]   report_file: {report_file}, exists: {Path(report_file).exists()}", file=sys.stderr)
            if Path(report_file).exists():
                print(f"[DEBUG]   report_file size: {os.path.getsize(report_file)} bytes", file=sys.stderr)
            print(f"[DEBUG]   pydantic_report_file (target): {pydantic_report_file}", file=sys.stderr)

        try:
            # Use orchestrator helper to handle move/copy fallbacks consistently.
            _processor_create_final_user_files(
                user_yaml_file=str(file),
                source_yaml=str(uptodate_file),
                source_report=str(report_file),
            )
        except Exception as e:
            if debug:
                print(f"[DEBUG]   Move failed with exception: {e}", file=sys.stderr)
            pass  # Don't fail if move doesn't work

        return _emit_pipeline_result(
            phases=[a_ok_report],
            report_path=pydantic_report_file,
            yaml_path=pydantic_yaml_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    b_ok_report = _processor_run_phase_b(
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
        science_fixes=science_fixes,
    )
    b_ok = not b_ok_report.has_errors

    if not b_ok:
        # Phase B failed in ABC - create final files with mixed content
        # Final YAML: from Phase A (last successful phase), Final Report: from Phase B (contains errors)
        import shutil

        try:
            # Create final YAML from Phase A (last successful phase)
            if Path(uptodate_file).exists():
                shutil.copy2(
                    uptodate_file, pydantic_yaml_file
                )  # Copy updatedA → updated (keep intermediate)

            # Create final Report from Phase B (contains the actual errors we need to show user)
            if Path(science_report_file).exists():
                _processor_move_report_with_json(
                    science_report_file, pydantic_report_file, pydantic_yaml_file
                )  # Move reportB -> report (don't keep intermediate)
            elif Path(report_file).exists():
                # Fallback to Phase A report if Phase B report doesn't exist
                _processor_copy_report_with_json(
                    report_file, pydantic_report_file, pydantic_yaml_file
                )  # Copy reportA -> report (keep intermediate)

            # Remove failed Phase B YAML
            if Path(science_yaml_file).exists():
                Path(science_yaml_file).unlink()  # Remove failed Phase B YAML
        except Exception:
            pass  # Don't fail if cleanup doesn't work

        # Clean up intermediate files
        try:
            _processor_unlink_report_with_json(report_file)
            if Path(uptodate_file).exists():
                Path(uptodate_file).unlink()
        except Exception:
            pass

        return _emit_pipeline_result(
            phases=[a_ok_report, b_ok_report],
            report_path=pydantic_report_file,
            yaml_path=pydantic_yaml_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    # Both Phase A and B succeeded - extract and consolidate messages for Phase C
    from ..data_model.validation.pipeline.orchestrator import (
        extract_no_action_messages_from_report,
    )

    # Extract Phase A messages and clean up immediately (minimizes I/O time)
    phase_a_messages = []
    report_path = Path(report_file)
    if report_path.exists():
        phase_a_messages = extract_no_action_messages_from_report(report_file)
    # Always run sidecar-aware cleanup: drops the .txt if present and any
    # orphan temp_reportA_*.json that lingered after partial earlier cleanup.
    _processor_unlink_report_with_json(report_file)

    # Extract Phase B messages and clean up immediately (minimizes I/O time)
    phase_b_messages = []
    science_report_path = Path(science_report_file)
    if science_report_path.exists():
        phase_b_messages = extract_no_action_messages_from_report(science_report_file)
    # Same idempotent cleanup for Phase B's text + temp_reportB_*.json sidecar.
    _processor_unlink_report_with_json(science_report_file)

    # Deduplicate messages efficiently and filter out incomplete headers
    all_no_action_messages = []
    seen_messages = set()

    for msg in phase_a_messages + phase_b_messages:
        if msg not in seen_messages:
            # Skip incomplete header patterns that end with "to current standards:"
            if msg.strip().startswith("- Updated (") and msg.strip().endswith(
                "to current standards:"
            ):
                # This is an incomplete header, skip it
                continue

            all_no_action_messages.append(msg)
            seen_messages.add(msg)

    c_ok_report = _processor_run_phase_c(
        science_yaml_file,
        pydantic_yaml_file,
        pydantic_report_file,
        mode=mode,
        phase_a_report_file=None,  # Files already cleaned up
        science_report_file=None,  # Files already cleaned up
        phases_run=["A", "B", "C"],
        no_action_messages=all_no_action_messages,
        silent=True,
    )
    c_ok = not c_ok_report.has_errors

    if not c_ok:
        # Phase C failed in ABC - create final files with mixed content
        # Final YAML: from Phase B (last successful phase), Final Report: from Phase C (contains errors)
        import shutil

        try:
            # Create final YAML from Phase B (last successful phase)
            if Path(science_yaml_file).exists():
                shutil.copy2(
                    science_yaml_file, pydantic_yaml_file
                )  # Copy updatedB → updated (keep intermediate)

            # Final Report should be from Phase C (contains the actual errors), but Phase C might not create a file
            # In this case, we'll rely on Phase C having already created pydantic_report_file, or use Phase B as fallback
            if (
                not Path(pydantic_report_file).exists()
                and Path(science_report_file).exists()
            ):
                _processor_copy_report_with_json(
                    science_report_file, pydantic_report_file, pydantic_yaml_file
                )  # Fallback: copy reportB -> report
        except Exception:
            pass  # Don't fail if copy doesn't work

        # Clean up intermediate files
        try:
            _processor_unlink_report_with_json(report_file)
            if Path(uptodate_file).exists():
                Path(uptodate_file).unlink()
            _processor_unlink_report_with_json(science_report_file)
            if Path(science_yaml_file).exists():
                Path(science_yaml_file).unlink()
        except Exception:
            pass

        return _emit_pipeline_result(
            phases=[a_ok_report, b_ok_report, c_ok_report],
            report_path=pydantic_report_file,
            yaml_path=pydantic_yaml_file,
            out_format=out_format,
            command=command_str,
            started_at=started_at,
        )

    # All phases succeeded - clean up intermediate files and don't show them
    if debug:
        report_exists = os.path.exists(pydantic_report_file)
        report_size = os.path.getsize(pydantic_report_file) if report_exists else -1
        print(f"[DEBUG] Final report state: exists={report_exists}, size={report_size} bytes", file=sys.stderr)

    # The intermediate files are now cleaned up by run_phase_c during consolidation
    # Clean up any remaining intermediate YAML files that weren't cleaned up
    try:
        uptodate_path = Path(uptodate_file)
        if uptodate_path.exists():
            uptodate_path.unlink()  # Remove updatedA_*
        science_yaml_path = Path(science_yaml_file)
        if science_yaml_path.exists():
            science_yaml_path.unlink()  # Remove updatedB_*
    except Exception:
        pass  # Don't fail if cleanup doesn't work

    return _emit_pipeline_result(
        phases=[a_ok_report, b_ok_report, c_ok_report],
        report_path=pydantic_report_file,
        yaml_path=pydantic_yaml_file,
        out_format=out_format,
        command=command_str,
        started_at=started_at,
    )


def main():
    """Main entry point."""
    cli()


if __name__ == "__main__":
    main()


@cli.group(name="schema", invoke_without_command=True, hidden=True)
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
