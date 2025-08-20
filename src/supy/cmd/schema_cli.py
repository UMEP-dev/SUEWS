#!/usr/bin/env python3
"""
SUEWS Schema Management CLI

Unified command-line interface for managing SUEWS YAML configuration schemas.
This consolidates schema version checking, migration, and validation operations.

This module addresses GitHub issues #612 and #613, providing a single entry point
for all schema-related operations that will integrate with the future suews-wizard (#544).
"""

import click
import yaml
import json
import sys
from pathlib import Path
from typing import Optional, List, Tuple
import logging
from datetime import datetime
from rich.console import Console
from rich.table import Table
from rich.panel import Panel
from rich.progress import track
from rich.syntax import Syntax
import jsonschema

# Import from supy modules
try:
    from ..data_model.core.config import SUEWSConfig
    from ..data_model.schema.version import (
        CURRENT_SCHEMA_VERSION,
        SCHEMA_VERSIONS,
        is_schema_compatible,
        get_schema_compatibility_message,
    )
    from ..data_model.schema.publisher import generate_json_schema, save_schema
    from ..data_model.schema.migration import SchemaMigrator, check_migration_needed
except ImportError:
    # Fallback for direct script execution
    import sys

    sys.path.append(str(Path(__file__).parent.parent.parent))
    from supy.data_model.core.config import SUEWSConfig
    from supy.data_model.schema.version import (
        CURRENT_SCHEMA_VERSION,
        SCHEMA_VERSIONS,
        is_schema_compatible,
        get_schema_compatibility_message,
    )
    from supy.data_model.schema.publisher import generate_json_schema, save_schema
    from supy.data_model.schema.migration import SchemaMigrator, check_migration_needed

console = Console()
logger = logging.getLogger(__name__)


def read_yaml_file(file_path: Path) -> Tuple[dict, Optional[str]]:
    """
    Read a YAML file and extract its schema version.

    Returns:
        Tuple of (config_dict, schema_version)
    """
    try:
        with open(file_path, "r") as f:
            config = yaml.safe_load(f)
        schema_version = config.get("schema_version")
        return config, schema_version
    except yaml.YAMLError as e:
        raise click.ClickException(f"YAML parsing error in {file_path}: {e}")
    except FileNotFoundError:
        raise click.ClickException(f"File not found: {file_path}")


def validate_file_against_schema(
    file_path: Path, schema_version: Optional[str] = None, show_details: bool = True
) -> Tuple[bool, List[str]]:
    """
    Validate a configuration file against a specific schema version.

    Args:
        file_path: Path to the configuration file
        schema_version: Schema version to validate against (None = current)
        show_details: Whether to show detailed error messages

    Returns:
        Tuple of (is_valid, list_of_errors)
    """
    errors = []

    try:
        # Read configuration
        config, file_schema_version = read_yaml_file(file_path)

        # Use specified version or file's version or current
        version = schema_version or file_schema_version or CURRENT_SCHEMA_VERSION

        # Generate schema for the target version
        schema = generate_json_schema(version=version)

        # JSON Schema validation
        validator = jsonschema.Draft7Validator(schema)
        validation_errors = list(validator.iter_errors(config))

        if validation_errors:
            for error in validation_errors:
                path = " → ".join(str(p) for p in error.path) if error.path else "root"
                errors.append(f"{path}: {error.message}")

        # Pydantic validation for additional checks (only if current version)
        if version == CURRENT_SCHEMA_VERSION:
            try:
                SUEWSConfig(**config)
            except Exception as e:
                errors.append(f"Model validation: {str(e)}")

        return (len(errors) == 0, errors)

    except Exception as e:
        return (False, [str(e)])


@click.group()
@click.option("--verbose", "-v", is_flag=True, help="Enable verbose output")
@click.option("--quiet", "-q", is_flag=True, help="Suppress non-essential output")
@click.pass_context
def cli(ctx, verbose, quiet):
    """
    SUEWS Schema Management - Manage YAML configuration schemas.

    This tool provides utilities for managing SUEWS YAML configuration schemas,
    including version checking, validation, and migration between versions.
    """
    ctx.ensure_object(dict)
    ctx.obj["verbose"] = verbose
    ctx.obj["quiet"] = quiet

    # Set up logging
    if verbose:
        logging.basicConfig(level=logging.DEBUG)
    elif quiet:
        logging.basicConfig(level=logging.ERROR)
    else:
        logging.basicConfig(level=logging.INFO)


@cli.command()
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
@click.option("--update", "-u", is_flag=True, help="Update schema version in files")
@click.option("--target-version", help="Target version for update")
@click.option(
    "--backup", "-b", is_flag=True, default=True, help="Create backup before updating"
)
@click.pass_context
def version(ctx, files, update, target_version, backup):
    """
    Check or update schema versions in configuration files.

    Examples:
        suews-schema version config.yml
        suews-schema version *.yml --update --target-version 1.0
    """
    verbose = ctx.obj.get("verbose", False)
    quiet = ctx.obj.get("quiet", False)

    if not quiet:
        if update:
            console.print(
                f"[bold blue]Updating schema versions to {target_version or CURRENT_SCHEMA_VERSION}[/bold blue]\n"
            )
        else:
            console.print("[bold blue]Checking schema versions[/bold blue]\n")

    # Create results table
    table = Table(title=None if quiet else "Schema Version Status")
    table.add_column("File", style="cyan")
    table.add_column("Current Version", justify="center")
    table.add_column("Status", justify="center")
    if update:
        table.add_column("Action", style="yellow")

    for file_path in files:
        path = Path(file_path)

        try:
            config, current_version = read_yaml_file(path)
            current_version = current_version or "not specified"

            # Check compatibility
            if current_version == "not specified":
                status = "[yellow]⚠ Missing[/yellow]"
            elif is_schema_compatible(current_version):
                status = "[green]✓ Compatible[/green]"
            else:
                status = "[red]✗ Incompatible[/red]"

            # Handle update if requested
            action = ""
            if update:
                new_version = target_version or CURRENT_SCHEMA_VERSION
                if current_version != new_version:
                    # Backup if requested
                    if backup:
                        backup_path = path.with_suffix(
                            f".backup-{datetime.now():%Y%m%d-%H%M%S}.yml"
                        )
                        path.rename(backup_path)
                        with open(path, "w") as f:
                            config["schema_version"] = new_version
                            yaml.dump(
                                config, f, default_flow_style=False, sort_keys=False
                            )
                        action = f"Updated → {new_version}"
                    else:
                        config["schema_version"] = new_version
                        with open(path, "w") as f:
                            yaml.dump(
                                config, f, default_flow_style=False, sort_keys=False
                            )
                        action = f"Updated → {new_version}"
                else:
                    action = "No change needed"

            if update:
                table.add_row(path.name, current_version, status, action)
            else:
                table.add_row(path.name, current_version, status)

        except Exception as e:
            if update:
                table.add_row(path.name, "Error", f"[red]✗ {str(e)}[/red]", "Skipped")
            else:
                table.add_row(path.name, "Error", f"[red]✗ {str(e)}[/red]")

    if not quiet:
        console.print(table)


@cli.command()
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
@click.option("--target-version", help="Target schema version for migration")
@click.option(
    "--output-dir", "-o", type=click.Path(), help="Output directory for migrated files"
)
@click.option("--backup", "-b", is_flag=True, default=True, help="Keep original files")
@click.option(
    "--dry-run", "-n", is_flag=True, help="Show what would be done without doing it"
)
@click.pass_context
def migrate(ctx, files, target_version, output_dir, backup, dry_run):
    """
    Migrate configuration files between schema versions.

    Examples:
        suews-schema migrate old_config.yml
        suews-schema migrate configs/*.yml --target-version 2.0 --output-dir migrated/
    """
    verbose = ctx.obj.get("verbose", False)
    quiet = ctx.obj.get("quiet", False)

    target = target_version or CURRENT_SCHEMA_VERSION
    output_path = Path(output_dir) if output_dir else None

    if output_path:
        output_path.mkdir(parents=True, exist_ok=True)

    if not quiet:
        mode = "DRY RUN - " if dry_run else ""
        console.print(f"[bold blue]{mode}Migrating to schema v{target}[/bold blue]\n")

    migrator = SchemaMigrator()
    results = []

    for file_path in track(files, description="Migrating...", disable=quiet):
        path = Path(file_path)

        try:
            # Read configuration
            config, current_version = read_yaml_file(path)
            current_version = migrator.auto_detect_version(config)

            if current_version == target:
                results.append((
                    path.name,
                    "Already at target version",
                    "[yellow]Skipped[/yellow]",
                ))
                continue

            # Perform migration
            if not dry_run:
                migrated = migrator.migrate(
                    config, from_version=current_version, to_version=target
                )

                # Determine output path
                if output_path:
                    out_file = output_path / path.name
                else:
                    out_file = path.with_suffix(".migrated.yml")

                # Save migrated configuration
                with open(out_file, "w") as f:
                    yaml.dump(migrated, f, default_flow_style=False, sort_keys=False)

                # Validate migrated config
                is_valid, _ = validate_file_against_schema(
                    out_file, schema_version=target, show_details=False
                )

                if is_valid:
                    results.append((
                        path.name,
                        f"{current_version} → {target}",
                        "[green]✓ Success[/green]",
                    ))
                else:
                    results.append((
                        path.name,
                        f"{current_version} → {target}",
                        "[yellow]⚠ Needs review[/yellow]",
                    ))
            else:
                results.append((
                    path.name,
                    f"{current_version} → {target}",
                    "[dim]Would migrate[/dim]",
                ))

        except Exception as e:
            results.append((path.name, "Error", f"[red]✗ {str(e)}[/red]"))

    # Display results
    if not quiet:
        table = Table(title="Migration Results")
        table.add_column("File", style="cyan")
        table.add_column("Migration", justify="center")
        table.add_column("Status", justify="center")

        for name, migration, status in results:
            table.add_row(name, migration, status)

        console.print(table)


@cli.command()
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
@click.option("--schema-version", help="Schema version to validate against")
@click.option(
    "--strict", "-s", is_flag=True, help="Exit with error on validation failure"
)
@click.option(
    "--format",
    type=click.Choice(["table", "json", "yaml"]),
    default="table",
    help="Output format",
)
@click.pass_context
def validate(ctx, files, schema_version, strict, format):
    """
    Validate configuration files against their schema.

    Examples:
        suews-schema validate config.yml
        suews-schema validate configs/*.yml --schema-version 1.0 --strict
    """
    verbose = ctx.obj.get("verbose", False)
    quiet = ctx.obj.get("quiet", False)

    version = schema_version or CURRENT_SCHEMA_VERSION

    if not quiet and format == "table":
        console.print(f"[bold blue]Validating against schema v{version}[/bold blue]\n")

    results = []
    all_valid = True

    for file_path in files:
        path = Path(file_path)
        is_valid, errors = validate_file_against_schema(
            path, schema_version=schema_version, show_details=verbose
        )

        if not is_valid:
            all_valid = False

        results.append({
            "file": str(path),
            "valid": is_valid,
            "errors": errors if not is_valid else [],
            "error_count": len(errors) if not is_valid else 0,
        })

    # Output results based on format
    if format == "json":
        import json

        console.print(json.dumps(results, indent=2))
    elif format == "yaml":
        console.print(yaml.dump(results, default_flow_style=False))
    else:  # table format
        if not quiet:
            table = Table(title="Validation Results")
            table.add_column("File", style="cyan")
            table.add_column("Status", justify="center")
            table.add_column("Issues", style="yellow")

            for result in results:
                path = Path(result["file"])
                if result["valid"]:
                    status = "[green]✓ Valid[/green]"
                    issues = ""
                else:
                    status = "[red]✗ Invalid[/red]"
                    if verbose and result["errors"]:
                        issues = "\n".join(result["errors"][:3])
                        if len(result["errors"]) > 3:
                            issues += f"\n... and {len(result['errors']) - 3} more"
                    else:
                        issues = f"{result['error_count']} issue(s)"

                table.add_row(path.name, status, issues)

            console.print(table)

            valid_count = sum(1 for r in results if r["valid"])
            total_count = len(results)
            console.print(
                f"\n[bold]Summary:[/bold] {valid_count}/{total_count} files valid"
            )

    # Exit with error if strict mode and validation failed
    if strict and not all_valid:
        sys.exit(1)


@cli.command()
@click.option("--output", "-o", type=click.Path(), help="Output file for schema")
@click.option("--version", help="Schema version to export")
@click.option(
    "--format",
    type=click.Choice(["json", "yaml"]),
    default="json",
    help="Output format",
)
@click.pass_context
def export(ctx, output, version, format):
    """
    Export the JSON Schema for SUEWS configurations.

    Examples:
        suews-schema export -o schema.json
        suews-schema export --version 1.0 --format yaml
    """
    verbose = ctx.obj.get("verbose", False)
    quiet = ctx.obj.get("quiet", False)

    schema_version = version or CURRENT_SCHEMA_VERSION

    if not quiet:
        console.print(f"[bold blue]Exporting schema v{schema_version}[/bold blue]\n")

    try:
        # Generate schema
        schema = generate_json_schema(version=schema_version)

        # Convert to desired format
        if format == "yaml":
            output_content = yaml.dump(
                schema, default_flow_style=False, sort_keys=False
            )
            default_filename = f"suews-schema-v{schema_version}.yaml"
        else:
            output_content = json.dumps(schema, indent=2)
            default_filename = f"suews-schema-v{schema_version}.json"

        # Write to file or stdout
        if output:
            output_path = Path(output)
            output_path.write_text(output_content)
            if not quiet:
                console.print(f"[green]✓[/green] Schema exported to {output_path}")
        else:
            if quiet:
                print(output_content)
            else:
                console.print(
                    Panel(
                        Syntax(output_content, format, theme="monokai"),
                        title=f"Schema v{schema_version}",
                        subtitle=f"Save as: {default_filename}",
                    )
                )

    except Exception as e:
        console.print(f"[red]✗ Export failed: {e}[/red]")
        sys.exit(1)


@cli.command()
@click.pass_context
def info(ctx):
    """
    Display information about available schema versions.
    """
    quiet = ctx.obj.get("quiet", False)

    if quiet:
        # Just output current version in quiet mode
        print(CURRENT_SCHEMA_VERSION)
        return

    console.print(Panel("[bold]SUEWS Schema Information[/bold]"))

    console.print(f"\n[bold]Current Version:[/bold] {CURRENT_SCHEMA_VERSION}")

    if CURRENT_SCHEMA_VERSION in SCHEMA_VERSIONS:
        console.print(f"[dim]{SCHEMA_VERSIONS[CURRENT_SCHEMA_VERSION]}[/dim]")

    console.print("\n[bold]Available Versions:[/bold]")
    for version, description in SCHEMA_VERSIONS.items():
        marker = "→" if version == CURRENT_SCHEMA_VERSION else " "
        console.print(f"  {marker} v{version}: {description}")

    console.print("\n[bold]CLI Commands:[/bold]")
    console.print("  • Check version: [cyan]suews-schema version config.yml[/cyan]")
    console.print("  • Validate: [cyan]suews-schema validate config.yml[/cyan]")
    console.print(
        "  • Migrate: [cyan]suews-schema migrate old.yml --target-version 2.0[/cyan]"
    )
    console.print("  • Export schema: [cyan]suews-schema export -o schema.json[/cyan]")

    console.print("\n[bold]Integration:[/bold]")
    console.print(
        "  • Use with CI/CD: [cyan]suews-schema validate *.yml --strict[/cyan]"
    )
    console.print(
        "  • Batch operations: [cyan]suews-schema version configs/*.yml --update[/cyan]"
    )
    console.print("  • Future wizard: Will use these utilities for validation")


def main():
    """Main entry point for the schema CLI."""
    cli(obj={})


if __name__ == "__main__":
    main()
