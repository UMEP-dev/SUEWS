# command line tools
from __future__ import annotations

from contextlib import nullcontext, redirect_stdout
from io import StringIO
from pathlib import Path
import sys

import click

from ..util.converter import (
    convert_to_yaml,
    detect_input_type,
    detect_table_version,
    list_ver_from,
)
from ..util.converter.yaml_upgrade import YamlUpgradeError, upgrade_yaml
from .json_envelope import (
    EXIT_OK,
    EXIT_USER_ERROR,
    Envelope,
    _now_iso,
    silent_supy_logger,
)

# Try to import the current version from the project
try:
    from .._version import __version__

    CURRENT_VERSION = __version__
except ImportError:
    # Fallback to None if version is not available
    CURRENT_VERSION = None


@click.command(
    context_settings={"show_default": True},
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
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["text", "json"], case_sensitive=False),
    default="text",
    show_default=True,
    help="Output format. 'json' emits the standard SUEWS envelope on stdout.",
)
@click.pass_context
def convert_table_cmd(  # noqa: PLR0912, PLR0915
    ctx: click.Context,
    fromVer: str,
    input_file: str | None,
    output_file: str | None,
    debug_dir: str | None = None,
    no_validate_profiles: bool = False,
    output_format: str = "text",
) -> None:
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
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    command = _command_string()
    list_warnings: list[str] = []

    if input_file is None or output_file is None:
        if json_mode:
            missing = []
            if input_file is None:
                missing.append("--input")
            if output_file is None:
                missing.append("--output")
            _emit_error(
                f"Missing required option(s): {', '.join(missing)}.",
                command=command,
                started_at=started_at,
                data={"input": input_file, "output": output_file},
                exit_code=2,
            )
        click.echo(ctx.get_help())
        sys.exit(0 if (input_file is None and output_file is None) else 2)

    input_path = Path(input_file)
    output_path = Path(output_file)

    # Detect input type from file
    try:
        input_type = detect_input_type(input_path)
    except ValueError as e:
        if json_mode:
            _emit_error(
                str(e),
                command=command,
                started_at=started_at,
                data={"input": str(input_path), "output": str(output_path)},
            )
        click.secho(str(e), fg="red", err=True)
        sys.exit(1)

    # Validate output has correct extension
    if output_path.suffix not in {".yml", ".yaml"}:
        message = "Output file should have .yml or .yaml extension"
        if json_mode:
            list_warnings.append(message)
        else:
            click.echo(f"Warning: {message}", err=True)

    # Dispatch on input type
    if input_type == "yaml":
        # In json mode the SuPy logger (default stdout handler) and any
        # accidental print would corrupt the envelope; silence the logger
        # AND capture stray stdout writes so the only thing that reaches
        # stdout is the envelope emitted by `_emit_success` / `_emit_error`.
        try:
            with (
                silent_supy_logger() if json_mode else nullcontext(),
                redirect_stdout(StringIO()) if json_mode else nullcontext(),
            ):
                upgrade_yaml(
                    input_path=input_path,
                    output_path=output_path,
                    from_ver=fromVer,
                )
        except YamlUpgradeError as e:
            if json_mode:
                _emit_error(
                    str(e),
                    command=command,
                    started_at=started_at,
                    data=_result_data(
                        input_path,
                        output_path,
                        input_type,
                        fromVer,
                        debug_dir,
                        no_validate_profiles,
                    ),
                    warnings=list_warnings,
                )
            click.secho(f"[ERROR] {e}", fg="red", err=True)
            sys.exit(1)
        except Exception as e:
            if json_mode:
                _emit_error(
                    f"YAML upgrade failed: {e}",
                    command=command,
                    started_at=started_at,
                    data=_result_data(
                        input_path,
                        output_path,
                        input_type,
                        fromVer,
                        debug_dir,
                        no_validate_profiles,
                    ),
                    warnings=list_warnings,
                )
            click.secho(f"[ERROR] YAML upgrade failed: {e}", fg="red", err=True)
            sys.exit(1)

        if json_mode:
            _emit_success(
                command=command,
                started_at=started_at,
                data=_result_data(
                    input_path,
                    output_path,
                    input_type,
                    fromVer,
                    debug_dir,
                    no_validate_profiles,
                ),
                warnings=list_warnings,
            )
        else:
            click.secho(f"\n[OK] Successfully created: {output_path}", fg="green")
        return

    if input_type == "nml":
        # Table conversion
        if not json_mode:
            click.echo("Converting SUEWS tables to YAML")
            click.echo(f"  Input: {input_path}")
            click.echo(f"  Tables directory: {input_path.parent}")

        # Auto-detect version if needed
        if not fromVer:
            if not json_mode:
                click.echo("  Auto-detecting table version...")
            fromVer = detect_table_version(input_path.parent)
            if fromVer:
                if not json_mode:
                    click.echo(f"  Detected version: {fromVer}")
            else:
                if json_mode:
                    _emit_error(
                        "Could not detect version. Use -f to specify.",
                        command=command,
                        started_at=started_at,
                        data=_result_data(
                            input_path,
                            output_path,
                            input_type,
                            fromVer,
                            debug_dir,
                            no_validate_profiles,
                        ),
                        warnings=list_warnings,
                    )
                click.secho(
                    "Could not detect version. Use -f to specify.", fg="red", err=True
                )
                sys.exit(1)
        elif fromVer not in list_ver_from:
            message = (
                f"Unsupported table release: {fromVer}. "
                f"Supported: {', '.join(list_ver_from)}"
            )
            if json_mode:
                _emit_error(
                    message,
                    command=command,
                    started_at=started_at,
                    data=_result_data(
                        input_path,
                        output_path,
                        input_type,
                        fromVer,
                        debug_dir,
                        no_validate_profiles,
                    ),
                    warnings=list_warnings,
                )
            click.secho(
                message,
                fg="red",
                err=True,
            )
            sys.exit(1)

    elif input_type == "df_state":
        # df_state conversion
        if not json_mode:
            click.echo("Converting df_state to YAML")
            click.echo(f"  Input: {input_path}")

        if fromVer:
            message = "Version specification ignored for df_state"
            if json_mode:
                list_warnings.append(message)
            else:
                click.echo(f"  Note: {message}")

    # Perform table / df_state conversion
    try:
        with (
            silent_supy_logger() if json_mode else nullcontext(),
            redirect_stdout(StringIO()) if json_mode else nullcontext(),
        ):
            convert_to_yaml(
                input_file=str(input_path),
                output_file=str(output_path),
                from_ver=fromVer if input_type == "nml" else None,
                debug_dir=debug_dir,
                validate_profiles=not no_validate_profiles,
            )
        if json_mode:
            _emit_success(
                command=command,
                started_at=started_at,
                data=_result_data(
                    input_path,
                    output_path,
                    input_type,
                    fromVer if input_type == "nml" else None,
                    debug_dir,
                    no_validate_profiles,
                ),
                warnings=list_warnings,
            )
        else:
            click.secho(f"\n[OK] Successfully created: {output_path}", fg="green")

    except Exception as e:
        if json_mode:
            _emit_error(
                f"Conversion failed: {e}",
                command=command,
                started_at=started_at,
                data=_result_data(
                    input_path,
                    output_path,
                    input_type,
                    fromVer if input_type == "nml" else None,
                    debug_dir,
                    no_validate_profiles,
                ),
                warnings=list_warnings,
            )
        click.secho(f"\n[ERROR] Conversion failed: {e}", fg="red", err=True)
        sys.exit(1)


def _command_string() -> str:
    """Return a stable canonical command string for envelope metadata."""
    list_args = sys.argv[1:]
    if list_args and list_args[0] == "convert":
        list_args = list_args[1:]
    return " ".join(["suews", "convert", *list_args])


def _result_data(
    input_path: Path,
    output_path: Path,
    input_type: str,
    from_version: str | None,
    debug_dir: str | None,
    no_validate_profiles: bool,
) -> dict[str, object]:
    """Build the command-specific payload for JSON mode."""
    return {
        "input": str(input_path),
        "output": str(output_path),
        "input_type": input_type,
        "from_version": from_version,
        "debug_dir": debug_dir,
        "profile_validation": not no_validate_profiles,
        "output_exists": output_path.exists(),
    }


def _emit_success(
    *,
    command: str,
    started_at: str,
    data: dict[str, object],
    warnings: list[str],
) -> None:
    """Emit a canonical success/warning envelope and terminate cleanly."""
    Envelope.success(
        data=data,
        command=command,
        warnings=warnings or None,
        started_at=started_at,
    ).emit()
    sys.exit(EXIT_OK)


def _emit_error(
    message: str,
    *,
    command: str,
    started_at: str,
    data: dict[str, object],
    warnings: list[str] | None = None,
    exit_code: int = EXIT_USER_ERROR,
) -> None:
    """Emit a canonical error envelope and terminate with ``exit_code``."""
    Envelope.error(
        errors=[message],
        command=command,
        data=data,
        warnings=warnings or None,
        started_at=started_at,
    ).emit()
    sys.exit(exit_code)


if __name__ == "__main__":
    convert_table_cmd()
