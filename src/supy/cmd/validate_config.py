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
) -> tuple[bool, List[str]]:
    """
    Validate a single configuration file.

    Returns:
        Tuple of (is_valid, list_of_errors)
    """
    errors = []

    try:
        # Load configuration
        with open(file_path, "r") as f:
            config = yaml.safe_load(f)

        # Check if migration needed
        if check_migration_needed(str(file_path)):
            errors.append(
                f"Configuration uses old schema version and may need migration"
            )

        # Validate against schema
        validator = jsonschema.Draft7Validator(schema)
        validation_errors = list(validator.iter_errors(config))

        if validation_errors:
            for error in validation_errors:
                path = " → ".join(str(p) for p in error.path) if error.path else "root"
                errors.append(f"{path}: {error.message}")

        # Try Pydantic validation for additional checks
        try:
            SUEWSConfig(**config)
        except Exception as e:
            errors.append(f"Pydantic validation: {str(e)}")

        return (len(errors) == 0, errors)

    except yaml.YAMLError as e:
        return (False, [f"YAML parsing error: {e}"])
    except FileNotFoundError:
        return (False, [f"File not found: {file_path}"])
    except Exception as e:
        return (False, [f"Unexpected error: {e}"])


@click.group()
def cli():
    """SUEWS Configuration Validator - Validate and check YAML configurations."""
    pass


@cli.command()
@click.argument("files", nargs=-1, type=click.Path(exists=True), required=True)
@click.option("--schema-version", help="Schema version to validate against")
@click.option("--verbose", "-v", is_flag=True, help="Show detailed error messages")
@click.option("--quiet", "-q", is_flag=True, help="Only show summary")
def validate(files, schema_version, verbose, quiet):
    """Validate SUEWS YAML configuration files."""

    # Generate schema
    schema = generate_json_schema(version=schema_version)
    version = schema_version or CURRENT_SCHEMA_VERSION

    if not quiet:
        console.print(
            f"\n[bold blue]Validating against schema v{version}[/bold blue]\n"
        )

    # Create results table
    table = Table(title="Validation Results" if not quiet else None)
    table.add_column("File", style="cyan")
    table.add_column("Status", justify="center")
    table.add_column("Issues", style="yellow")

    total_files = len(files)
    valid_files = 0

    for file_path in track(files, description="Validating...", disable=quiet):
        path = Path(file_path)
        is_valid, errors = validate_single_file(path, schema, show_details=verbose)

        if is_valid:
            valid_files += 1
            status = "[green]✓ Valid[/green]"
            issues = ""
        else:
            status = "[red]✗ Invalid[/red]"
            if verbose:
                issues = "\n".join(errors[:3])  # Show first 3 errors
                if len(errors) > 3:
                    issues += f"\n... and {len(errors) - 3} more"
            else:
                issues = f"{len(errors)} issue(s)"

        table.add_row(path.name, status, issues)

    if not quiet:
        console.print(table)
        console.print(
            f"\n[bold]Summary:[/bold] {valid_files}/{total_files} files valid"
        )

    # Exit with error if any files invalid
    if valid_files < total_files:
        sys.exit(1)


@cli.command()
@click.argument("file", type=click.Path(exists=True))
def check(file):
    """Quick check of a configuration file with detailed feedback."""

    path = Path(file)
    console.print(Panel(f"[bold]Checking: {path.name}[/bold]"))

    try:
        # Load and display configuration
        with open(path, "r") as f:
            config = yaml.safe_load(f)

        # Check schema version
        schema_version = config.get("schema_version", "not specified")
        console.print(f"\n📋 Schema version: {schema_version}")

        if schema_version == "not specified":
            console.print(
                "[yellow]  ⚠ No schema version specified, assuming v{CURRENT_SCHEMA_VERSION}[/yellow]"
            )
        elif schema_version != CURRENT_SCHEMA_VERSION:
            console.print(
                f"[yellow]  ⚠ Different from current ({CURRENT_SCHEMA_VERSION})[/yellow]"
            )

        # Check migration need
        if check_migration_needed(str(path)):
            console.print("\n[yellow]🔄 Migration needed to current schema[/yellow]")

        # Validate structure
        console.print("\n🔍 Validating structure...")
        schema = generate_json_schema()
        is_valid, errors = validate_single_file(path, schema)

        if is_valid:
            console.print("[green]  ✓ Configuration is valid![/green]")
        else:
            console.print("[red]  ✗ Validation errors found:[/red]")
            for i, error in enumerate(errors[:5], 1):
                console.print(f"    {i}. {error}")
            if len(errors) > 5:
                console.print(f"    ... and {len(errors) - 5} more errors")

        # Check key sections
        console.print("\n📊 Configuration summary:")
        console.print(f"  • Sites: {len(config.get('sites', []))}")
        console.print(
            f"  • Model timestep: {config.get('model', {}).get('control', {}).get('tstep', 'not set')}s"
        )

        # Pydantic validation
        console.print("\n🔧 Running detailed validation...")
        try:
            SUEWSConfig(**config)
            console.print("[green]  ✓ Passed all validation checks[/green]")
        except Exception as e:
            console.print(f"[red]  ✗ {str(e)[:200]}[/red]")

    except yaml.YAMLError as e:
        console.print(f"[red]✗ YAML parsing error: {e}[/red]")
        sys.exit(1)
    except Exception as e:
        console.print(f"[red]✗ Error: {e}[/red]")
        sys.exit(1)


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


@cli.command()
def schema():
    """Display information about the configuration schema."""

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
    console.print("  • Validate file: suews-validate validate config.yml")
    console.print("  • Quick check: suews-validate check config.yml")
    console.print("  • Migrate: suews-validate migrate old_config.yml")


def main():
    """Main entry point."""
    cli()


if __name__ == "__main__":
    main()
