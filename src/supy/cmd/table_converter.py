# command line tools
import click
import sys
from pathlib import Path

from ..util.converter import list_ver_from

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
    "-i",
    "--input",
    "input_file",
    help="Input file: RunControl.nml for tables, or df_state.csv/.pkl",
    type=click.Path(exists=True, dir_okay=False, file_okay=True),
    required=True,
)
@click.option(
    "-o",
    "--output",
    "output_file",
    help="Output YAML file path",
    type=click.Path(dir_okay=False),
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
def convert_table_cmd(
    fromVer: str,
    input_file: str,
    output_file: str,
    debug_dir: str = None,
    no_validate_profiles: bool = False,
):
    """Convert SUEWS inputs to YAML configuration.

    Input must be a specific file:
    - RunControl.nml: Converts table-based SUEWS input
    - *.csv or *.pkl: Converts df_state format

    Examples:
        # Convert tables to YAML
        suews-convert -i path/to/RunControl.nml -o config.yml

        # Convert old df_state CSV to YAML
        suews-convert -i df_state.csv -o config.yml

        # Convert df_state pickle to YAML
        suews-convert -i state.pkl -o config.yml

        # Specify table version explicitly
        suews-convert -f 2024a -i RunControl.nml -o config.yml
    """
    # Import here to avoid circular imports
    from ..util.converter import (
        convert_to_yaml,
        detect_table_version,
        detect_input_type,
    )

    input_path = Path(input_file)
    output_path = Path(output_file)

    # Detect input type from file
    try:
        input_type = detect_input_type(input_path)
    except ValueError as e:
        click.secho(str(e), fg="red", err=True)
        sys.exit(1)

    # Validate output has correct extension
    if output_path.suffix not in [".yml", ".yaml"]:
        click.echo(
            f"Warning: Output file should have .yml or .yaml extension", err=True
        )

    # Handle based on input type
    if input_type == "nml":
        # Table conversion
        click.echo(f"Converting SUEWS tables to YAML")
        click.echo(f"  Input: {input_path}")
        click.echo(f"  Tables directory: {input_path.parent}")

        # Auto-detect version if needed
        if not fromVer:
            click.echo("  Auto-detecting table version...")
            fromVer = detect_table_version(input_path.parent)
            if fromVer:
                click.echo(f"  Detected version: {fromVer}")
            else:
                click.secho(
                    "Could not detect version. Use -f to specify.", fg="red", err=True
                )
                sys.exit(1)

    elif input_type == "df_state":
        # df_state conversion
        click.echo(f"Converting df_state to YAML")
        click.echo(f"  Input: {input_path}")

        if fromVer:
            click.echo(
                "  Note: Version specification ignored for df_state", fg="yellow"
            )

    # Perform conversion
    try:
        convert_to_yaml(
            input_file=str(input_path),
            output_file=str(output_path),
            from_ver=fromVer if input_type == "nml" else None,
            debug_dir=debug_dir,
            validate_profiles=not no_validate_profiles,
        )
        click.secho(f"\n[OK] Successfully created: {output_path}", fg="green")

    except Exception as e:
        click.secho(f"\n[ERROR] Conversion failed: {e}", fg="red", err=True)
        sys.exit(1)
