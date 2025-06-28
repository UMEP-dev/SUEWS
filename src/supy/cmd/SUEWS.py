# command line tools
import os
import dask.bag as db
import click
import sys

from .._supy_module import init_supy, run_supy, save_supy, load_forcing_grid, pd, Path

from .._version import show_version, __version__

from .._load import load_SUEWS_nml_simple


# run the whole supy workflow mimicking SUEWS binary
@click.command(
    short_help="Run SUEWS simulation using settings in PATH_RUNCONTROL",
    help="""
Run SUEWS simulation using settings in PATH_RUNCONTROL
(default: "./RunControl.nml", i.e., the RunControl namelist file in the current directory).

Examples:

1. Run SUEWS simulation using the RunControl namelist file in the current directory:

    $ suews-run -p ./RunControl.nml

2. Run SUEWS simulation using an arbitray RunControl namelist file by specifying the path to the RunControl namelist file:

    $ suews-run -p /path/to/RunControl.nml
""",
)
@click.option(
    "--path_runcontrol",
    "-p",
    default="./RunControl.nml",
    type=click.Path(exists=True),
    help="Path to the RunControl namelist file (default: ./RunControl.nml).",
)
def SUEWS(path_runcontrol):
    """Run SUEWS simulation using settings in PATH_RUNCONTROL (default: "./RunControl.nml", i.e., the RunControl namelist file in the current directory)."""

    # show version info
    click.echo(
        f"""
===========================================
Running SUEWS via SuPy ...
supy version: {__version__}

Documentation sites:
SUEWS: https://suews.readthedocs.io/
===========================================
    """
    )

    try:
        path_runcontrol = Path(path_runcontrol).resolve()
        # init supy
        click.echo("Initialising ...")
        df_state_init = init_supy(path_runcontrol)

        # load forcing
        list_grid = df_state_init.index
        click.echo(f"\n{list_grid.size} grids detected")
        ser_runctrl = load_SUEWS_nml_simple(path_runcontrol).runcontrol
        flag_multimet = ser_runctrl.multiplemetfiles

        if flag_multimet == 1:
            click.echo("\nGrid-specific forcing conditions will be used.")
            # multiple met forcing conditions according to grids:
            list_df_forcing = [
                load_forcing_grid(path_runcontrol, grid) for grid in list_grid
            ]
            list_input = [
                (load_forcing_grid(path_runcontrol, grid), df_state_init.loc[[grid]])
                for grid in list_grid
            ]
            click.echo("\nSimulation periods:")
            for grid, df_forcing in zip(list_grid, list_df_forcing):
                idx_dt = df_forcing.index
                start, end = idx_dt.min(), idx_dt.max()
                click.echo(f"grid {grid}: {start} – {end}")
            # daemonic processes only support `threads` method
            method_parallel = "threads"
            list_res = (
                db.from_sequence(list_input)
                .map(lambda input_grid: run_supy(*input_grid))
                .compute(scheduler=method_parallel)
            )
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
            df_forcing = load_forcing_grid(path_runcontrol, grid)
            click.echo("\nSame forcing conditions will be used for all grids.")
            click.echo("\nSimulation period:")
            idx_dt = df_forcing.index
            start, end = idx_dt.min(), idx_dt.max()
            click.echo(f"{start} – {end}")
            # run supy
            df_output, df_state_final = run_supy(df_forcing, df_state_init)

        # save result
        list_out_files = save_supy(
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
