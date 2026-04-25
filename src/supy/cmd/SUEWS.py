# command line tools
import click
import hashlib
import json as _json
import logging
import multiprocessing
import os
import sys
from pathlib import Path

from .json_envelope import (
    EXIT_OK,
    EXIT_RUN_FAILURE,
    EXIT_USER_ERROR,
    Envelope,
    _build_meta,
    _now_iso,
)

# Get logger for CLI warnings (configured in _env.py when heavy imports load)
logger_cli = logging.getLogger("SuPy.CLI")

# Lazy import flag - heavy modules imported only when needed
_HEAVY_IMPORTS_LOADED = False


def _load_heavy_imports():
    """Load heavy imports only when needed for actual simulation.

    This defers slow imports (which can take 15+ seconds due to package discovery)
    until they're actually needed, allowing quick responses for help, errors, etc.
    """
    global _HEAVY_IMPORTS_LOADED
    if _HEAVY_IMPORTS_LOADED:
        return

    # These imports trigger slow package discovery via importlib.resources.files()
    global _init_supy, _run_supy, _save_supy, _load_forcing_grid, pd
    global load_SUEWS_nml_simple, SUEWSSimulation, YAML_SUPPORT
    from .._supy_module import (
        _init_supy,
        _run_supy,
        _save_supy,
        _load_forcing_grid,
        pd,
    )
    from .._load import load_SUEWS_nml_simple

    # Import YAML-based simulation class
    try:
        from ..suews_sim import SUEWSSimulation

        YAML_SUPPORT = True
    except ImportError:
        SUEWSSimulation = None
        YAML_SUPPORT = False

    _HEAVY_IMPORTS_LOADED = True


def _get_version():
    """Get version string without triggering heavy imports."""
    try:
        from .._version_scm import __version__

        return __version__
    except ImportError:
        return "unknown"


def _detect_config_format(config_path):
    """Detect configuration file format based on extension.

    Parameters
    ----------
    config_path : Path
        Path to configuration file

    Returns
    -------
    str
        Format type: 'yaml' or 'namelist'
    """
    suffix = config_path.suffix.lower()
    if suffix in [".yml", ".yaml"]:
        return "yaml"
    elif suffix in [".nml"]:
        return "namelist"
    else:
        # Default to namelist for backward compatibility
        return "namelist"


def _run_yaml_core(config_path: Path, output_dir: Path | None = None) -> dict:
    """Run a YAML config and return a structured summary.

    Silent — never prints. Raises on error. Used by both the human-prose path
    (``_run_with_yaml``) and the JSON path (``_run_yaml_json``).

    Parameters
    ----------
    config_path : Path
        Path to YAML configuration file.
    output_dir : Path, optional
        Explicit output directory. When ``None``, the config's default is used.

    Returns
    -------
    dict
        ``{sim, output_files, output_dir, period_start, period_end, n_steps}``
    """
    _load_heavy_imports()

    if not YAML_SUPPORT:
        raise RuntimeError(
            "YAML support not available. Please install required dependencies."
        )

    sim = SUEWSSimulation(str(config_path))
    if sim.forcing is None:
        raise ValueError(
            "No forcing data found in configuration. "
            "Ensure 'forcing_file' is specified in the YAML config."
        )

    idx_dt = sim.forcing.index
    period_start = idx_dt.min()
    period_end = idx_dt.max()

    sim.run()

    output_files = (
        sim.save(output_path=str(output_dir)) if output_dir is not None else sim.save()
    )

    if output_files:
        actual_out_dir = Path(output_files[0]).parent
    else:
        actual_out_dir = output_dir if output_dir is not None else Path(".")

    return {
        "sim": sim,
        "output_files": [str(p) for p in output_files],
        "output_dir": str(actual_out_dir),
        "period_start": period_start.isoformat()
        if hasattr(period_start, "isoformat")
        else str(period_start),
        "period_end": period_end.isoformat()
        if hasattr(period_end, "isoformat")
        else str(period_end),
        "n_steps": int(len(idx_dt)),
    }


def _run_with_yaml(config_path):
    """Human-prose YAML run flow (legacy default).

    Parameters
    ----------
    config_path : Path
        Path to YAML configuration file.
    """
    try:
        click.echo("Loading YAML configuration ...")
        result = _run_yaml_core(config_path)

        click.echo("\nSimulation period:")
        click.echo(f"{result['period_start']} - {result['period_end']}")

        click.echo("\nThe following files have been written out:")
        for path_out in result["output_files"]:
            click.echo(path_out)

        click.echo("\nSUEWS run successfully done!")

    except FileNotFoundError as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)
    except (RuntimeError, ValueError) as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)
    except KeyboardInterrupt:
        click.echo("\nSimulation interrupted by user.", err=True)
        sys.exit(130)
    except Exception as e:
        import traceback

        click.echo(f"Error running simulation: {e}", err=True)
        click.echo("\nFull traceback:", err=True)
        click.echo(traceback.format_exc(), err=True)
        sys.exit(1)


def _hash_config_file(path_config: Path) -> str:
    """SHA256 of the config input bytes.

    Phase-1 approach: hash the input YAML the user provided. A future iteration
    will hash the canonical-form resolved YAML (Pydantic round-trip) for
    stronger reproducibility guarantees; the input-bytes hash is sufficient
    for now and avoids depending on the model dump format.
    """
    return hashlib.sha256(path_config.read_bytes()).hexdigest()


def _write_provenance(
    path_out_dir: Path,
    config_path: Path,
    result: dict,
    started_at: str,
    command: str,
) -> Path:
    """Write ``provenance.json`` next to the run output. Returns the path."""
    meta = _build_meta(command=command, started_at=started_at)
    provenance = {
        "command": command,
        "config_path": str(config_path),
        "config_hash_sha256": _hash_config_file(config_path),
        "output_dir": result["output_dir"],
        "output_files": result["output_files"],
        "period_start": result["period_start"],
        "period_end": result["period_end"],
        "n_steps": result["n_steps"],
        "started_at": meta["started_at"],
        "ended_at": meta["ended_at"],
        "schema_version": meta["schema_version"],
        "suews_version": meta["suews_version"],
        "supy_version": meta["supy_version"],
        "git_commit": meta["git_commit"],
    }
    path_provenance = path_out_dir / "provenance.json"
    path_provenance.write_text(
        _json.dumps(provenance, indent=2, ensure_ascii=False), encoding="utf-8"
    )
    return path_provenance


def _run_yaml_json(
    config_path: Path,
    output_dir: Path | None,
    started_at: str,
    command: str,
) -> None:
    """JSON-mode YAML run. Emits an Envelope and writes provenance.json."""
    try:
        result = _run_yaml_core(config_path, output_dir=output_dir)
    except FileNotFoundError as e:
        Envelope.error(
            errors=[str(e)],
            command=command,
            data={"config_path": str(config_path)},
            started_at=started_at,
        ).emit()
        sys.exit(EXIT_USER_ERROR)
    except (RuntimeError, ValueError) as e:
        Envelope.error(
            errors=[str(e)],
            command=command,
            data={"config_path": str(config_path)},
            started_at=started_at,
        ).emit()
        sys.exit(EXIT_USER_ERROR)
    except Exception as e:
        Envelope.error(
            errors=[f"{type(e).__name__}: {e}"],
            command=command,
            data={"config_path": str(config_path)},
            started_at=started_at,
        ).emit()
        sys.exit(EXIT_RUN_FAILURE)

    path_out_dir = Path(result["output_dir"])
    path_provenance = _write_provenance(
        path_out_dir,
        config_path,
        result,
        started_at=started_at,
        command=command,
    )

    Envelope.success(
        data={
            "output_dir": str(path_out_dir),
            "output_files": result["output_files"],
            "provenance_path": str(path_provenance),
            "period_start": result["period_start"],
            "period_end": result["period_end"],
            "n_steps": result["n_steps"],
            "config_hash_sha256": _hash_config_file(config_path),
        },
        command=command,
        started_at=started_at,
    ).emit()


def _run_with_namelist(path_runcontrol):
    """Run SUEWS simulation using namelist configuration (legacy).

    Parameters
    ----------
    path_runcontrol : Path
        Path to RunControl.nml file
    """
    _load_heavy_imports()

    # Issue deprecation warning to stderr
    click.echo(
        "\n"
        "=" * 60 + "\n"
        "DEPRECATION WARNING: Namelist format is deprecated.\n"
        "Please migrate to YAML configuration:\n\n"
        f"  1. Convert: suews-convert -i {path_runcontrol.name} -o config.yml\n"
        "  2. Run:     suews-run config.yml\n\n"
        "For more information, see: https://docs.suews.io/\n"
        "=" * 60 + "\n",
        err=True,
    )

    try:
        path_runcontrol = Path(path_runcontrol).resolve()
        # init supy
        click.echo("Initialising ...")
        df_state_init = _init_supy(path_runcontrol)

        # load forcing
        list_grid = df_state_init.index
        click.echo(f"\n{list_grid.size} grids detected")
        ser_runctrl = load_SUEWS_nml_simple(path_runcontrol).runcontrol
        flag_multimet = ser_runctrl.multiplemetfiles

        if flag_multimet == 1:
            click.echo("\nGrid-specific forcing conditions will be used.")
            # multiple met forcing conditions according to grids:
            list_df_forcing = [
                _load_forcing_grid(path_runcontrol, grid) for grid in list_grid
            ]
            list_input = [
                (_load_forcing_grid(path_runcontrol, grid), df_state_init.loc[[grid]])
                for grid in list_grid
            ]
            click.echo("\nSimulation periods:")
            for grid, df_forcing in zip(list_grid, list_df_forcing):
                idx_dt = df_forcing.index
                start, end = idx_dt.min(), idx_dt.max()
                click.echo(f"grid {grid}: {start} - {end}")

            # Fortran SAVE variables are shared across threads: use process-based pools.
            mp_context = os.environ.get("SUPY_MP_CONTEXT", "spawn")
            try:
                ctx = multiprocessing.get_context(mp_context)
            except ValueError as e:
                msg = f"Invalid SUPY_MP_CONTEXT={mp_context!r} ({e}); falling back to 'spawn'."
                click.echo(msg, err=True)
                logger_cli.warning(msg)
                ctx = multiprocessing.get_context("spawn")

            processes = min(len(list_grid), os.cpu_count() or 1)
            with ctx.Pool(processes=processes) as pool:
                list_res = pool.starmap(_run_supy, list_input)
            try:
                list_df_output, list_df_state_final = zip(*list_res)
                df_output = pd.concat(list_df_output, names=["grid", "datetime"])
                df_state_final = pd.concat(
                    list_df_state_final, names=["grid", "datetime"]
                )

            except Exception as e:
                raise RuntimeError("SUEWS kernel error") from e

        else:
            # uniform met forcing condition across grids:
            grid = list_grid[0]
            df_forcing = _load_forcing_grid(path_runcontrol, grid)
            click.echo("\nSame forcing conditions will be used for all grids.")
            click.echo("\nSimulation period:")
            idx_dt = df_forcing.index
            start, end = idx_dt.min(), idx_dt.max()
            click.echo(f"{start} - {end}")
            # run supy
            df_output, df_state_final = _run_supy(df_forcing, df_state_init)

        # save result
        list_out_files = _save_supy(
            df_output, df_state_final, path_runcontrol=path_runcontrol
        )

        # show output files
        click.echo("\nThe following files have been written out:")
        for file in list_out_files:
            click.echo(file)

        # return
        click.echo("\nSUEWS run successfully done!")

    except KeyboardInterrupt:
        click.echo("\nSimulation interrupted by user.", err=True)
        sys.exit(130)
    except Exception as e:
        import traceback

        click.echo(f"Error: {e}", err=True)
        click.echo("\nFull traceback:", err=True)
        click.echo(traceback.format_exc(), err=True)
        sys.exit(1)


# run the whole supy workflow mimicking SUEWS binary
@click.command(
    short_help="Run SUEWS simulation using YAML or namelist configuration",
    help="""
Run SUEWS simulation using YAML (recommended) or namelist configuration.

YAML Configuration (Recommended):

    $ suews-run config.yml

    $ suews-run /path/to/config.yml

Namelist Configuration (Deprecated):

    $ suews-run -p RunControl.nml

    $ suews-run -p /path/to/RunControl.nml

The format is auto-detected based on file extension:
- .yml, .yaml: YAML format (modern, recommended)
- .nml: Namelist format (legacy, deprecated)

To migrate from namelist to YAML:

    $ suews-convert -i RunControl.nml -o config.yml
    $ suews-run config.yml

For more information, see: https://docs.suews.io/
""",
)
@click.argument(
    "config_file",
    type=click.Path(exists=True),
    required=False,
)
@click.option(
    "--path_runcontrol",
    "-p",
    type=click.Path(exists=True),
    help="[DEPRECATED] Path to RunControl namelist file. Use positional argument instead.",
)
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["text", "json"], case_sensitive=False),
    default="text",
    show_default=True,
    help="Output format. 'json' emits the standard SUEWS envelope on stdout "
    "and writes a 'provenance.json' sidecar in the output directory.",
)
@click.option(
    "--output",
    "-o",
    "output_dir",
    type=click.Path(file_okay=False, dir_okay=True),
    default=None,
    help="Output directory. When omitted, the config's default location is used.",
)
def SUEWS(config_file, path_runcontrol, output_format, output_dir):
    """Run SUEWS simulation using YAML or namelist configuration."""

    started_at = _now_iso()
    json_mode = output_format.lower() == "json"

    # In JSON mode, suppress the human banner so stdout stays parseable.
    if not json_mode:
        click.echo(
            f"""
===========================================
SUEWS version: {_get_version()}

Website: https://suews.io/
===========================================
    """
        )

    # Handle backward compatibility with -p option
    if path_runcontrol is not None:
        click.echo(
            "\nDEPRECATION: The '-p/--path_runcontrol' option is deprecated. "
            "Use positional argument instead:\n"
            f"  suews-run {Path(path_runcontrol).name}\n",
            err=True,
        )
        config_file = path_runcontrol

    # Determine config file to use
    if config_file is None:
        # Try to find default config files
        default_yaml = Path("config.yml")
        default_nml = Path("RunControl.nml")

        if default_yaml.exists():
            config_file = default_yaml
            click.echo("Using default configuration: config.yml\n")
        elif default_nml.exists():
            config_file = default_nml
            click.echo("Using default configuration: RunControl.nml\n")
        else:
            click.echo(
                "Error: No configuration file found.\n"
                "Please provide a config file or create one of:\n"
                "  - config.yml (recommended)\n"
                "  - RunControl.nml (deprecated)\n",
                err=True,
            )
            sys.exit(1)
    else:
        config_file = Path(config_file)

    # Auto-detect format and run appropriate workflow
    config_format = _detect_config_format(config_file)

    if json_mode:
        if config_format != "yaml":
            command = " ".join(["suews", "run"] + sys.argv[1:])
            Envelope.error(
                errors=[
                    "JSON output mode requires a YAML config; "
                    "namelist runs are deprecated and unsupported under --format json."
                ],
                command=command,
                data={"config_path": str(config_file)},
                started_at=started_at,
            ).emit()
            sys.exit(EXIT_USER_ERROR)
        path_out_dir = Path(output_dir) if output_dir else None
        command = " ".join(["suews", "run"] + sys.argv[1:])
        _run_yaml_json(config_file, path_out_dir, started_at, command)
        return

    if config_format == "yaml":
        _run_with_yaml(config_file)
    else:
        _run_with_namelist(config_file)
