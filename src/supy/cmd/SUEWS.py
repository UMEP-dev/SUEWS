# command line tools
import click
import sys
from pathlib import Path

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
    global load_SUEWS_nml_simple, SUEWSSimulation, YAML_SUPPORT, ThreadPool

    from multiprocessing.pool import ThreadPool
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


def _run_with_yaml(config_path):
    """Run SUEWS simulation using YAML configuration.

    Parameters
    ----------
    config_path : Path
        Path to YAML configuration file
    """
    _load_heavy_imports()

    if not YAML_SUPPORT:
        click.echo(
            "Error: YAML support not available. Please install required dependencies.",
            err=True,
        )
        sys.exit(1)

    try:
        # Load configuration and run simulation
        click.echo("Loading YAML configuration ...")
        sim = SUEWSSimulation(str(config_path))

        # Check if forcing is loaded
        if sim.forcing is None:
            click.echo("Error: No forcing data found in configuration.", err=True)
            click.echo(
                "Please ensure 'forcing_file' is specified in the YAML config.",
                err=True,
            )
            sys.exit(1)

        # Display simulation info
        click.echo(f"\nSimulation period:")
        idx_dt = sim.forcing.index
        start, end = idx_dt.min(), idx_dt.max()
        click.echo(f"{start} - {end}")

        # Run simulation
        click.echo("\nRunning simulation ...")
        results = sim.run()

        # Save results
        click.echo("\nSaving results ...")
        output_files = sim.save()

        # Show output files
        click.echo("\nThe following files have been written out:")
        for file in output_files:
            click.echo(file)

        click.echo("\nSUEWS run successfully done!")

    except FileNotFoundError as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(f"Error running simulation: {e}", err=True)
        sys.exit(1)


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
        "For more information, see: https://suews.readthedocs.io/\n"
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
            # use thread-based parallelization for multi-grid simulations
            with ThreadPool() as pool:
                list_res = pool.starmap(_run_supy, list_input)
            try:
                list_df_output, list_df_state_final = zip(*list_res)
                df_output = pd.concat(list_df_output, names=["grid", "datetime"])
                df_state_final = pd.concat(
                    list_df_state_final, names=["grid", "datetime"]
                )

            except:
                raise RuntimeError("SUEWS kernel error")

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

    except:
        # click.echo(f'{str(path_runcontrol)} not existing!')
        sys.exit()


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

For more information, see: https://suews.readthedocs.io/
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
def SUEWS(config_file, path_runcontrol):
    """Run SUEWS simulation using YAML or namelist configuration."""

    # show version info (using lightweight version getter)
    click.echo(
        f"""
===========================================
SUEWS version: {_get_version()}

Documentation: https://suews.readthedocs.io/
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

    if config_format == "yaml":
        _run_with_yaml(config_file)
    else:
        _run_with_namelist(config_file)
