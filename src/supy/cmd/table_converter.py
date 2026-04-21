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


@click.command(
    context_settings=dict(show_default=True),
    help=(
        "Convert any supported SUEWS input into a current-schema YAML.\n\n"
        "Input type is auto-detected from the file:\n"
        "  RunControl.nml / *.nml    legacy SUEWS table set\n"
        "  *.csv / *.pkl             df_state snapshot\n"
        "  *.yml / *.yaml            older-release YAML (cross-release upgrade)\n\n"
        "Pass -f/--from to disambiguate the source version when auto-detection "
        "is ambiguous (table releases for .nml inputs; release tag or schema "
        "version for .yml inputs)."
    ),
)
@click.option(
    "-f",
    "--from",
    "fromVer",
    help=(
        "Source version. For .nml inputs pick a table release (e.g. 2024a); "
        "for .yml inputs pass a supy release tag (e.g. 2026.1.28) or a "
        "schema version. Auto-detected when omitted."
    ),
    required=False,
    default=None,
)
@click.option(
    "-i",
    "--input",
    "input_file",
    help=(
        "Input file: RunControl.nml for tables, *.csv/*.pkl for df_state, "
        "or *.yml/*.yaml for an older YAML config."
    ),
    type=click.Path(exists=True, dir_okay=False, file_okay=True),
    required=False,
)
@click.option(
    "-o",
    "--output",
    "output_file",
    help="Output YAML file path",
    type=click.Path(dir_okay=False),
    required=False,
)
@click.option(
    "-d",
    "--debug-dir",
    "debug_dir",
    help=(
        "Optional directory to keep intermediate conversion files for "
        "debugging table/df_state runs. Ignored for YAML upgrades."
    ),
    type=click.Path(),
    required=False,
    default=None,
)
@click.option(
    "--no-profile-validation",
    "no_validate_profiles",
    is_flag=True,
    default=False,
    help=(
        "Disable automatic profile validation and creation of missing profiles "
        "(table/df_state paths only)."
    ),
)
@click.pass_context
def convert_table_cmd(
    ctx: click.Context,
    fromVer: str,
    input_file: str,
    output_file: str,
    debug_dir: str = None,
    no_validate_profiles: bool = False,
):
    """Convert any supported SUEWS input to a current-schema YAML.

    The command auto-detects the input format from the file extension and
    dispatches to the matching converter. All three paths produce a YAML that
    parses under the current ``SUEWSConfig`` validator.

    Examples:
        # Legacy tables -> YAML
        suews-convert -i path/to/RunControl.nml -o config.yml

        # df_state snapshot -> YAML
        suews-convert -i df_state.csv -o config.yml

        # Older-release YAML -> current-schema YAML (auto-detect source)
        suews-convert -i old.yml -o new.yml

        # Older YAML without a schema_version field -> explicit source tag
        suews-convert -i old.yml -o new.yml -f 2026.1.28
    """
    if input_file is None or output_file is None:
        click.echo(ctx.get_help())
        sys.exit(0 if (input_file is None and output_file is None) else 2)

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

    # Dispatch on input type
    if input_type == "yaml":
        from ..util.converter.yaml_upgrade import YamlUpgradeError, upgrade_yaml

        try:
            upgrade_yaml(
                input_path=input_path,
                output_path=output_path,
                from_ver=fromVer,
            )
        except YamlUpgradeError as e:
            click.secho(f"[ERROR] {e}", fg="red", err=True)
            sys.exit(1)
        except Exception as e:  # noqa: BLE001 - surface unexpected failures verbatim
            click.secho(f"[ERROR] YAML upgrade failed: {e}", fg="red", err=True)
            sys.exit(1)

        click.secho(f"\n[OK] Successfully created: {output_path}", fg="green")
        return

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
        elif fromVer not in list_ver_from:
            click.secho(
                f"Unsupported table release: {fromVer}. "
                f"Supported: {', '.join(list_ver_from)}",
                fg="red",
                err=True,
            )
            sys.exit(1)

    elif input_type == "df_state":
        # df_state conversion
        click.echo(f"Converting df_state to YAML")
        click.echo(f"  Input: {input_path}")

        if fromVer:
            click.echo("  Note: Version specification ignored for df_state")

    # Perform table / df_state conversion
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


if __name__ == "__main__":
    convert_table_cmd()
