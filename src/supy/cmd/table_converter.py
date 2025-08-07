# command line tools
import click
import sys
from pathlib import Path

from ..util.converter import convert_table, list_ver_from, list_ver_to

# Try to import the current version from the project
try:
    from .._version import __version__

    CURRENT_VERSION = __version__
except ImportError:
    # Fallback to None if version is not available
    CURRENT_VERSION = None


@click.command(context_settings=dict(show_default=True))
@click.option(
    "-f",
    "--from",
    "fromVer",
    help="Version to convert from (auto-detect if not specified)",
    type=click.Choice(list_ver_from),
    required=False,
    default=None,
)
@click.option(
    "-t",
    "--to",
    "toVer",
    help="Version to convert to (default: latest YAML format)",
    type=str,  # Allow any version string
    default="latest",  # Default to latest version
    show_default=True,
)
@click.option(
    "-i",
    "--input",
    "input_path",
    help="Input directory containing SUEWS files (must have RunControl.nml)",
    type=click.Path(exists=True),
    required=True,
)
@click.option(
    "-o",
    "--output",
    "output_path",
    help="Output path (directory for table conversion, file path for YAML)",
    type=click.Path(),
    required=True,
)
@click.option(
    "-d",
    "--debug-dir",
    "debug_dir",
    help="Optional directory to keep intermediate conversion files for debugging. If not provided, temporary directories are removed automatically.",
    type=click.Path(),
    required=False,
    default=None,
)
@click.option(
    "--no-profile-validation",
    "no_validate_profiles",
    is_flag=True,
    default=False,
    help="Disable automatic profile validation and creation of missing profiles",
)
@click.option(
    "--force-table",
    "force_table",
    is_flag=True,
    default=False,
    help="Force table output format even for 2025a (skip YAML conversion)",
)
def convert_table_cmd(
    fromVer: str,
    toVer: str,
    input_path: str,
    output_path: str,
    debug_dir: str = None,
    no_validate_profiles: bool = False,
    force_table: bool = False,
):
    """Convert SUEWS input files between versions.

    Automatically determines conversion type based on target version:
    - Same version (fromVer == toVer): Sanitization only
    - Table versions (up to 2025a): Table-to-table conversion
    - Version 2025a: Final table version, converts to YAML format
    - 'latest' or omitted: Converts to current YAML format (aligned with project version)

    Examples:
        suews-convert -f 2016a -t 2016a -i input_dir -o output_dir  # Sanitize only
        suews-convert -f 2020a -t 2024a -i input_dir -o output_dir  # Table to table
        suews-convert -f 2024a -t 2025a -i input_dir -o config.yml  # Table to YAML
        suews-convert -i input_dir -o config.yml                    # Auto-detect version, convert to latest
        suews-convert -f 2024a -t latest -i input_dir -o config.yml # Explicit latest YAML
    """
    # Import here to avoid circular imports
    from ..util.converter import convert_table, detect_table_version

    # Auto-detect source version if not provided
    if fromVer is None:
        click.echo("Auto-detecting source version...")
        fromVer = detect_table_version(input_path)
        if fromVer is None:
            click.secho(
                "Could not auto-detect the version of the input files. "
                "Please specify the source version using -f/--from option.",
                fg="red",
                err=True,
            )
            sys.exit(1)
        click.secho(f"Detected version: {fromVer}", fg="green")
        
        # Inform user about equivalent versions
        equivalent_versions = {
            "2020a": ["2020a", "2021a", "2023a", "2024a"],
            "2018a": ["2018a", "2018b", "2018c"],
        }
        for base_ver, equiv_list in equivalent_versions.items():
            if fromVer == base_ver and len(equiv_list) > 1:
                click.echo(
                    f"Note: Versions {', '.join(equiv_list)} have identical table structures. "
                    f"Conversion will proceed as {base_ver}.",
                    err=False
                )

    # Check for same-version conversion (sanitization only)
    if fromVer == toVer and toVer != "latest":
        click.secho(
            f"Same version specified ({fromVer}). Will perform sanitization only.",
            fg="cyan",
        )

        # Ensure output is treated as directory
        output_dir = Path(output_path)

        # Call convert_table which handles same-version conversions
        convert_table(
            Path(input_path),
            output_dir,
            fromVer,
            toVer,
            debug_dir=debug_dir,
            validate_profiles=not no_validate_profiles,
        )

        click.secho(f"Successfully sanitized {fromVer} files", fg="green")
        return

    # Determine the actual target version
    if toVer == "latest":
        # 'latest' means convert to current YAML format
        # 2025a is the last table version, after which everything is YAML
        toVer = "2025a"  # This triggers YAML conversion
        version_display = (
            f"latest ({CURRENT_VERSION})" if CURRENT_VERSION else "latest YAML format"
        )
        click.secho(f"Converting to {version_display}", fg="cyan")
    elif toVer == "2025a":
        # 2025a can be either table or YAML
        if force_table:
            click.secho(
                f"Force table output: Converting to 2025a table format", fg="cyan"
            )
        else:
            click.secho(
                f"Converting to YAML format (2025a marks transition to YAML)", fg="cyan"
            )

    # Determine conversion type based on target version
    # 2025a is the boundary: it and anything after is YAML conversion
    to_yaml = False

    if toVer in list_ver_to:
        # Check if this is a known table version
        try:
            year = int(toVer[:4])
            if year >= 2025:
                to_yaml = not force_table  # Respect force_table flag for 2025a
        except (ValueError, IndexError):
            # If we can't parse the year, check if it's a valid table version
            pass
    else:
        # Unknown version - if it looks like a year >= 2025, treat as YAML
        # This allows forward compatibility with future versions
        try:
            year = int(toVer[:4])
            if year >= 2025:
                # Future version, assume YAML format
                to_yaml = True
                click.secho(
                    f"Note: '{toVer}' is not in table conversion rules, treating as YAML format",
                    fg="yellow",
                )
            else:
                click.secho(f"Error: '{toVer}' is not a valid target version", fg="red")
                click.secho(
                    f"Valid table versions: {', '.join(sorted(list_ver_to))}",
                    fg="yellow",
                )
                sys.exit(1)
        except (ValueError, IndexError):
            click.secho(f"Error: '{toVer}' is not a valid version format", fg="red")
            sys.exit(1)

    if to_yaml:
        # Convert to YAML
        click.secho(f"Converting from {fromVer} tables to YAML format...", fg="cyan")

        # Import the convert_to_yaml function
        from ..util.converter import convert_to_yaml

        # Ensure output path has .yml or .yaml extension
        output_file = Path(output_path)
        if not output_file.suffix in [".yml", ".yaml"]:
            if output_file.is_dir() or not output_file.suffix:
                # If it's a directory or has no extension, add default filename
                output_file = (
                    output_file / "suews_config.yml"
                    if output_file.is_dir()
                    else Path(str(output_file) + ".yml")
                )
                click.secho(f"Output will be saved to: {output_file}", fg="yellow")

        # Call convert_to_yaml directly
        convert_to_yaml(
            input_dir=input_path,
            output_file=str(output_file),
            from_ver=fromVer,
            debug_dir=debug_dir,
            validate_profiles=not no_validate_profiles,
        )

        click.secho(f"Successfully converted to YAML: {output_file}", fg="green")
    else:
        # Convert between table versions
        if toVer not in list_ver_to:
            click.secho(
                f"Error: '{toVer}' is not a valid table version. Available versions: {', '.join(list_ver_to)}",
                fg="red",
            )
            sys.exit(1)

        click.secho(f"Converting tables from {fromVer} to {toVer}...", fg="cyan")

        # Ensure output is treated as directory for table conversion
        output_dir = Path(output_path)

        convert_table(
            Path(input_path),
            output_dir,
            fromVer,
            toVer,
            debug_dir=debug_dir,
            validate_profiles=not no_validate_profiles,
        )

        click.secho(
            f"Successfully converted tables from {fromVer} to {toVer}", fg="green"
        )
