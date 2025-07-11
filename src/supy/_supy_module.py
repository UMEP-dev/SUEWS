# ###########################################################################
# SuPy: SUEWS for Python
#
# Authors:
# Ting Sun, ting.sun@reading.ac.uk
#
# History:
# 20 Jan 2018: first alpha release
# 01 Feb 2018: performance improvement
# 03 Feb 2018: improvement in output processing
# 08 Mar 2018: pypi packaging
# 04 Oct 2018: overhaul of structure
# 05 Oct 2018: added sample run data
# 28 Apr 2019: added support for parallel run
###########################################################################

import logging
import os
import sys
import time
import pandas
from pathlib import Path
from typing import Tuple

import numpy as np
import pandas as pd

from ._check import check_forcing, check_state
from ._env import logger_supy, trv_supy_module
from ._load import (
    load_InitialCond_grid_df,
    load_SUEWS_Forcing_met_df_raw,
    load_SUEWS_dict_ModConfig,
    load_df_state,
    resample_forcing_met,
    load_SUEWS_Forcing_met_df_yaml,
)
from ._run import run_supy_par, run_supy_ser
from ._save import get_save_info, save_df_output, save_df_state, save_initcond_nml, save_df_output_parquet
from ._post import resample_output
from ._version import __version__

# from .util._config import init_config_from_yaml
from .data_model import init_config_from_yaml


# set up logging module
logger_supy.setLevel(logging.INFO)


##############################################################################
# 1. compact wrapper for loading SUEWS settings
# @functools.lru_cache(maxsize=16)
def init_supy(
    path_init: str,
    force_reload=True,
    check_input=False,
) -> pd.DataFrame:
    """Initialise supy by loading initial model states.

    Parameters
    ----------
    path_init : str
        Path to a file that can initialise SuPy, which can be either of the follows:
            * SUEWS :ref:`RunControl.nml<suews:RunControl.nml>`: a namelist file for SUEWS configurations
            * SuPy `df_state.csv`: a CSV file including model states produced by a SuPy run via :py:func:`supy.save_supy`

    force_reload: boolean, optional
        Flag to force reload all initialisation files by clearing all cached states, with default value `True` (i.e., force reload all files).
        Note: If the number of simulation grids is large (e.g., > 100), `force_reload=False` is strongly recommended for better performance.

    check_input: boolean, optional
        flag for checking validity of input: `df_forcing` and `df_state_init`.
        If set to `True`, any detected invalid input will stop SuPy simulation;
        a `False` flag will bypass such validation and may incur kernel error if any invalid input.
        *Note: such checking procedure may take some time if the input is large.*
        (the default is `False`, which bypasses the validation).




    Returns
    -------
    df_state_init: pandas.DataFrame
        Initial model states.
        See `df_state_var` for details.

    Examples
    --------
    1. Use :ref:`RunControl.nml<suews:RunControl.nml>` to initialise SuPy

    >>> path_init = "~/SUEWS_sims/RunControl.nml"
    >>> df_state_init = supy.init_supy(path_init)

    2. Use ``df_state.csv`` to initialise SuPy

    >>> path_init = "~/SuPy_res/df_state_test.csv"
    >>> df_state_init = supy.init_supy(path_init)

    """

    try:
        path_init_x = Path(path_init).expanduser().resolve()
    except FileNotFoundError:
        logger_supy.exception(f"{path_init_x} does not exists!")
    else:
        if path_init_x.suffix == ".yml":
            # SUEWS `config_suews.yaml`:
            logger_supy.info("Loading config from yaml")
            df_state_init = init_config_from_yaml(path=path_init_x).to_df_state()
        else:
            logger_supy.warning(
                "Input is not a yaml file, loading from other sources. These methods will be deprecated in later versions.",
                stacklevel=2,
            )
            if path_init_x.suffix == ".nml":
                # SUEWS `RunControl.nml`:
                df_state_init = load_InitialCond_grid_df(
                    path_init_x,
                    force_reload=force_reload,
                )
            elif path_init_x.suffix == ".csv":
                # SuPy `df_state.csv`:
                df_state_init = load_df_state(path_init_x)
            else:
                logger_supy.critical(
                    f"{path_init_x} is NOT a valid file to initialise SuPy!"
                )
                raise RuntimeError(
                    "{path_init_x} is NOT a valid file to initialise SuPy!"
                )
        if check_input:
            try:
                list_issues = check_state(df_state_init)
                if isinstance(list_issues, list):
                    logger_supy.critical(
                        f"`df_state_init` loaded from {path_init_x} is NOT valid to initialise SuPy!"
                    )
            except:
                raise RuntimeError(
                    "{path_init_x} is NOT a valid file to initialise SuPy!"
                )

        return df_state_init


# # TODO:
# def load_forcing(path_pattern: str, grid: int = 0) -> pd.DataFrame:
#     pass


# TODO:
# to be superseded by a more generic wrapper: load_forcing
def load_forcing_grid(
    path_init: str,
    grid: int,
    check_input=False,
    force_reload=True,
    df_state_init: pd.DataFrame = None,
    config=None,
) -> pd.DataFrame:
    """Load forcing data for a specific grid included in the index of `df_state_init </data-structure/supy-io.ipynb#df_state_init:-model-initial-states>`.

    Parameters
    ----------

    path_runcontrol : str
        Path to SUEWS :ref:`RunControl.nml <suews:RunControl.nml>`
    grid : int
        Grid number
    check_input : bool, optional
        flag for checking validity of input: `df_forcing` and `df_state_init`.
        If set to `True`, any detected invalid input will stop SuPy simulation;
        a `False` flag will bypass such validation and may incur kernel error if any invalid input.
        *Note: such checking procedure may take some time if the input is large.*
        (the default is `False`, which bypasses the validation).

    Returns
    -------
    df_forcing: pandas.DataFrame
        Forcing data. See `df_forcing_var` for details.

    Examples
    --------
    >>> path_runcontrol = (
    ...     "~/SUEWS_sims/RunControl.nml"  # a valid path to `RunControl.nml`
    ... )
    >>> df_state_init = supy.init_supy(path_runcontrol)  # get `df_state_init`
    >>> grid = df_state_init.index[
    ...     0
    ... ]  # first grid number included in `df_state_init`
    >>> df_forcing = supy.load_forcing_grid(
    ...     path_runcontrol, grid
    ... )  # get df_forcing


    """

    try:
        path_init = Path(path_init).expanduser().resolve()
    except FileNotFoundError:
        logger_supy.exception(f"{path_init} does not exists!")
    else:
        if path_init.suffix == ".nml":
            # load settings from RunControl.nml
            dict_mod_cfg = load_SUEWS_dict_ModConfig(path_init)
            # load setting variables from dict_mod_cfg
            (
                filecode,
                kdownzen,
                tstep_met_in,
                tstep_ESTM_in,
                multiplemetfiles,
                multipleestmfiles,
                dir_input_cfg,
            ) = (
                dict_mod_cfg[x]
                for x in [
                    "filecode",
                    "kdownzen",
                    "resolutionfilesin",
                    "resolutionfilesinestm",
                    "multiplemetfiles",
                    "multipleestmfiles",
                    "fileinputpath",
                ]
            )

            path_site = path_init.parent
            path_input = path_site / dict_mod_cfg["fileinputpath"]
        else:
            if config is None:
                config = init_config_from_yaml(path=path_init)
            path_site = path_init.parent
            forcing_file_val = (
                config.model.control.forcing_file.value
                if hasattr(config.model.control.forcing_file, "value")
                else config.model.control.forcing_file
            )
            if isinstance(forcing_file_val, list):
                # Handle list of files
                path_input = [path_site / f for f in forcing_file_val]
            else:
                # Handle single file
                path_input = path_site / forcing_file_val

        tstep_mod, lat, lon, alt, timezone = df_state_init.loc[
            grid, [(x, "0") for x in ["tstep", "lat", "lng", "alt", "timezone"]]
        ].values

        # load raw data
        # met forcing
        if path_init.suffix == ".nml":
            df_forcing_met = load_SUEWS_Forcing_met_df_raw(
                path_input, filecode, grid, tstep_met_in, multiplemetfiles
            )
            # resample raw data from tstep_in to tstep_mod
            df_forcing_met_tstep = resample_forcing_met(
                df_forcing_met,
                tstep_met_in,
                tstep_mod,
                lat,
                lon,
                alt,
                timezone,
                kdownzen,
            )
        elif path_init.suffix == ".yml":
            df_forcing_met = load_SUEWS_Forcing_met_df_yaml(path_input)
            tstep_met_in = df_forcing_met.index[1] - df_forcing_met.index[0]
            tstep_met_in = int(tstep_met_in.total_seconds())
            kdownzen = config.model.control.kdownzen
            if kdownzen is not None:
                kdownzen = kdownzen.value
            if kdownzen is None:
                df_forcing_met_tstep = resample_forcing_met(
                    df_forcing_met, tstep_met_in, tstep_mod, lat, lon, alt, timezone
                )
            else:
                df_forcing_met_tstep = resample_forcing_met(
                    df_forcing_met,
                    tstep_met_in,
                    tstep_mod,
                    lat,
                    lon,
                    alt,
                    timezone,
                    kdownzen,
                )

        # coerced precision here to prevent numerical errors inside Fortran
        df_forcing = df_forcing_met_tstep.round(10)

        # new columns for later use in main calculation
        df_forcing[["iy", "id", "it", "imin"]] = df_forcing[
            ["iy", "id", "it", "imin"]
        ].astype(np.int64)

    if check_input:
        try:
            list_issues = check_forcing(df_forcing)
            if isinstance(list_issues, list):
                logger_supy.critical(
                    f"`df_forcing` loaded from {path_input} is NOT valid to drive SuPy!"
                )
        except:
            sys.exit()

    return df_forcing


# load sample data for quickly starting a demo run
# TODO: to deprecate this by renaming for case consistency: load_SampleData-->load_sample_data
def load_SampleData() -> Tuple[pandas.DataFrame, pandas.DataFrame]:
    logger_supy.warning(
        "This function name will be deprecated. Please use `load_sample_data()` instead.",
        stacklevel=2,
    )
    return load_sample_data()


def load_sample_data() -> Tuple[pandas.DataFrame, pandas.DataFrame]:
    """Load sample data for quickly starting a demo run.

    Returns
    -------
    df_state_init, df_forcing: Tuple[pandas.DataFrame, pandas.DataFrame]
        - df_state_init: `initial model states <df_state_var>`
        - df_forcing: `forcing data <df_forcing_var>`

    Examples
    --------

    >>> df_state_init, df_forcing = supy.load_sample_data()

    """

    trv_sample_data = trv_supy_module / "sample_run"
    path_config_default = trv_sample_data / "sample_config.yml"
    # path_config_default = trv_sample_data / "RunControl.nml" # TODO: to be deprecated - but keep for now to pass tests
    df_state_init = init_supy(path_config_default, force_reload=False)
    df_forcing = load_forcing_grid(
        path_config_default, df_state_init.index[0], df_state_init=df_state_init
    )
    return df_state_init, df_forcing


def load_config_from_df(df_state: pd.DataFrame):
    """Load SUEWS configuration from `df_state`.

    Parameters
    ----------
    df_state : pd.DataFrame
        DataFrame of model states.

    Returns
    -------
    config : SUEWSConfig
        SUEWS configuration.

    Examples
    --------
    >>> df_state_init, df_forcing = supy.load_sample_data()
    >>> config = supy.load_config_from_df(df_state_init)

    """

    from .util._config import SUEWSConfig

    config = SUEWSConfig.from_df_state(df_state)

    return config


def init_config(df_state: pd.DataFrame = None):
    """
    Initialise SUEWS configuration object either from existing df_state dataframe or as the default configuration.
    """

    if df_state is None:
        from .util._config import SUEWSConfig

        return SUEWSConfig()

    return load_config_from_df(df_state)


# input processing code end here
##############################################################################


##############################################################################
# 2. compact wrapper for running a whole simulation
# # main calculation
# input as DataFrame
def run_supy(
    df_forcing: pandas.DataFrame,
    df_state_init: pandas.DataFrame,
    save_state=False,
    chunk_day=3660,
    logging_level=logging.INFO,
    check_input=False,
    serial_mode=False,
    debug_mode=False,
) -> Tuple[pandas.DataFrame, pandas.DataFrame]:
    """Perform supy simulation.

    Parameters
    ----------
    df_forcing : pandas.DataFrame
        forcing data for all grids in `df_state_init`.
    df_state_init : pandas.DataFrame
        initial model states;
        or a collection of model states with multiple timestamps, whose last temporal record will be used as the initial model states.
    save_state : bool, optional
        flag for saving model states at each time step, which can be useful in diagnosing model runtime performance or performing a restart run.
        (the default is False, which instructs supy not to save runtime model states).
    chunk_day : int, optional
        chunk size (`chunk_day` days) to split simulation periods so memory usage can be reduced.
        (the default is 3660, which implies ~10-year forcing chunks used in simulations).
    logging_level: logging level
        one of these values [50 (CRITICAL), 40 (ERROR), 30 (WARNING), 20 (INFO), 10 (DEBUG)].
        A lower value informs SuPy for more verbose logging info.
    check_input : bool, optional
        flag for checking validity of input: `df_forcing` and `df_state_init`.
        If set to `True`, any detected invalid input will stop SuPy simulation;
        a `False` flag will bypass such validation and may incur kernel error if any invalid input.
        *Note: such checking procedure may take some time if the input is large.*
        (the default is `False`, which bypasses the validation).
    serial_mode : bool, optional
        If set to `True`, SuPy simulation will be conducted in serial mode;
        a `False` flag will try parallel simulation if possible (Windows not supported, i.e., always serial).
        (the default is `False`).
    debug_mode : bool, optional
        If set to `True`, SuPy simulation will be conducted in debug mode, which will write out additional information for debugging purposes.


    Returns
    -------
    df_output, df_state_final : Tuple[pandas.DataFrame, pandas.DataFrame]
        - df_output: `output results <df_output_var>`
        - df_state_final: `final model states <df_state_var>`

    Examples
    --------

    >>> df_output, df_state_final = supy.run_supy(df_forcing, df_state_init)


    """
    # validate input dataframes
    if check_input:
        # forcing:
        list_issues_forcing = check_forcing(df_forcing)
        if isinstance(list_issues_forcing, list):
            logger_supy.critical(f"`df_forcing` is NOT valid to drive SuPy!")
            raise RuntimeError(
                "SuPy stopped entering simulation due to invalid forcing!"
            )
        # initial model states:
        res_check_state = check_state(df_state_init)
        if isinstance(res_check_state, list):
            logger_supy.critical(f"`df_state_init` is NOT valid to initialise SuPy!")
            raise RuntimeError(
                "SuPy stopped entering simulation due to invalid initial states!"
            )
        else:
            logger_supy.info(f"SuPy simulation is starting ...")
            if isinstance(res_check_state, pd.DataFrame):
                df_state_init = res_check_state

    # enable debug mode if set
    if debug_mode:
        logging_level = logging.DEBUG
        logger_supy.setLevel(logging_level)

    # set up a timer for simulation time
    start = time.time()

    # adjust logging level
    logger_supy.setLevel(logging_level)

    # save df_init without changing its original data
    # df.copy() in pandas works as a standard python deepcopy
    # df_init = df_state_init.copy()

    # print some diagnostic info
    logger_supy.info(f"====================")
    logger_supy.info(f"SUEWS version: {__version__}")
    logger_supy.info(f"Simulation period:")
    logger_supy.info(f"  Start: {df_forcing.index[0]}")
    logger_supy.info(f"  End: {df_forcing.index[-1]}")
    logger_supy.info("")
    list_grid = df_state_init.index.get_level_values("grid").unique()
    n_grid = list_grid.size
    logger_supy.info(f"No. of grids: {n_grid}")

    if n_grid > 1 and os.name != "nt" and (not serial_mode):
        logger_supy.info(f"SUEWS is running in parallel mode")
        res_supy = run_supy_par(
            df_forcing, df_state_init, save_state, chunk_day, debug_mode
        )
    else:
        logger_supy.info(f"SUEWS is running in serial mode")
        res_supy = run_supy_ser(
            df_forcing, df_state_init, save_state, chunk_day, debug_mode
        )
        # try:
        #     res_supy = run_supy_ser(df_forcing, df_state_init, save_state, chunk_day)
        # except:
        #     res_supy = run_supy_ser(df_forcing, df_state_init, save_state, chunk_day)

    # show simulation time
    end = time.time()
    logger_supy.info(f"Execution time: {(end - start):.1f} s")
    logger_supy.info(f"====================\n")

    # unpack results
    df_output, df_state_final, res_debug, res_state = res_supy

    # return results based on debugging needs
    if debug_mode:
        return df_output, df_state_final, res_debug, res_state
    else:
        return df_output, df_state_final


##############################################################################
# 3. save results of a supy run
def save_supy(
    df_output: pandas.DataFrame,
    df_state_final: pandas.DataFrame,
    freq_s: int = 3600,
    site: str = "",
    path_dir_save: str = Path("."),
    path_runcontrol: str = None,
    save_tstep=False,
    logging_level=50,
    output_level=1,
    debug=False,
    output_config=None,
) -> list:
    """Save SuPy run results to files

    Parameters
    ----------
    df_output : pandas.DataFrame
        DataFrame of output
    df_state_final : pandas.DataFrame
        DataFrame of final model states
    freq_s : int, optional
        Output frequency in seconds (the default is 3600, which indicates hourly output)
    site : str, optional
        Site identifier (the default is '', which indicates site identifier will be left empty)
    path_dir_save : str, optional
        Path to directory to saving the files (the default is Path('.'), which indicates the current working directory)
    path_runcontrol : str, optional
        Path to SUEWS :ref:`RunControl.nml <suews:RunControl.nml>`, which, if set, will be preferably used to derive `freq_s`, `site` and `path_dir_save`.
        (the default is None, which is unset)
    save_tstep : bool, optional
        whether to save results in temporal resolution as in simulation (which may result very large files and slow progress), by default False.
    logging_level: logging level
        one of these values [50 (CRITICAL), 40 (ERROR), 30 (WARNING), 20 (INFO), 10 (DEBUG)].
        A lower value informs SuPy for more verbose logging info.
    output_level : integer, optional
        option to determine selection of output variables, by default 1.
        Notes: 0 for all but snow-related; 1 for all; 2 for a minimal set without land cover specific information.
    debug : bool, optional
        whether to enable debug mode (e.g., writing out in serial mode, and other debug uses), by default False.
    output_config : OutputConfig, optional
        Output configuration object specifying format, frequency, and groups to save. If provided, overrides freq_s parameter.


    Returns
    -------
    list
        a list of paths of saved files

    Examples
    --------
    1. save results of a supy run to the current working directory with default settings

    >>> list_path_save = supy.save_supy(df_output, df_state_final)


    2. save results according to settings in :ref:`RunControl.nml <suews:RunControl.nml>`

    >>> list_path_save = supy.save_supy(
    ...     df_output, df_state_final, path_runcontrol="path/to/RunControl.nml"
    ... )


    3. save results of a supy run at resampling frequency of 1800 s (i.e., half-hourly results) under the site code ``Test`` to a customised location 'path/to/some/dir'

    >>> list_path_save = supy.save_supy(
    ...     df_output,
    ...     df_state_final,
    ...     freq_s=1800,
    ...     site="Test",
    ...     path_dir_save="path/to/some/dir",
    ... )
    """
    # adjust logging level
    logger_supy.setLevel(logging_level)

    # get necessary information for saving procedure
    if path_runcontrol is not None:
        freq_s, path_dir_save, site, save_tstep, output_level = get_save_info(
            path_runcontrol
        )

    # Handle output configuration if provided
    output_format = "txt"  # default
    output_groups = None  # default will be handled in save_df_output
    
    if output_config is not None:
        from .data_model.model import OutputConfig, OutputFormat
        if isinstance(output_config, OutputConfig):
            # Override frequency if specified in config
            if output_config.freq is not None:
                freq_s = output_config.freq
            # Get format
            output_format = str(output_config.format)
            # Get groups for txt format
            if output_format == "txt" and output_config.groups is not None:
                output_groups = output_config.groups
        elif isinstance(output_config, str):
            # Legacy string format - issue deprecation warning
            import warnings
            warnings.warn(
                "The 'output_file' parameter as a string is deprecated and was never used. "
                "Please use the new OutputConfig format or remove this parameter. "
                "Falling back to default text output. "
                "Example: output_file: {format: 'parquet', freq: 3600}",
                DeprecationWarning,
                stacklevel=2
            )
            # Fall back to default text format
            output_format = "txt"

    # determine `save_snow` option
    snowuse = df_state_final.iloc[-1].loc["snowuse"].values.item()
    save_snow = True if snowuse == 1 else False

    # check if directory for saving results exists; if not, create one.
    path_dir_save = Path(path_dir_save)
    if not path_dir_save.exists():
        path_dir_save.mkdir(parents=True)

    # save based on format
    if output_format == "parquet":
        # Save as Parquet
        list_path_save = save_df_output_parquet(
            df_output,
            df_state_final,
            freq_s,
            site,
            path_dir_save,
            save_tstep,
        )
    else:
        # Save as text files (existing behavior)
        list_path_save = save_df_output(
            df_output,
            freq_s,
            site,
            path_dir_save,
            save_tstep,
            output_level,
            save_snow,
            debug,
            output_groups=output_groups,
        )

    # save df_state
    if path_runcontrol is not None:
        # save as nml as SUEWS binary
        list_path_nml = save_initcond_nml(df_state_final, site, path_dir_save)
        list_path_save = list_path_save + list_path_nml
    else:
        # save as supy csv for later use
        path_state_save = save_df_state(df_state_final, site, path_dir_save)
        # update list_path_save
        list_path_save.append(path_state_save)

    return list_path_save


def run_supy_sample(
    start=None,
    end=None,
    save_state=False,
    chunk_day=3660,
    logging_level=logging.INFO,
    check_input=False,
    serial_mode=False,
    debug_mode=False,
) -> Tuple[pandas.DataFrame, pandas.DataFrame, pandas.DataFrame]:
    """Quickly run SuPy with sample data and return output dataframes.

    This function loads sample data and runs SuPy simulation in one step,
    returning the output and final state dataframes.

    Parameters
    ----------
    save_state : bool, optional
        Flag for saving model states at each time step.
        (the default is False)
    chunk_day : int, optional
        Chunk size (days) to split simulation periods.
        (the default is 3660)
    logging_level : int, optional
        Logging level for verbosity control.
        (the default is logging.INFO)
    check_input : bool, optional
        Flag for checking validity of input.
        (the default is False)
    serial_mode : bool, optional
        If True, run in serial mode; otherwise try parallel if possible.
        (the default is False)
    debug_mode : bool, optional
        If True, run in debug mode with additional information.
        (the default is False)

    Returns
    -------
    df_output, df_state_final : Tuple[pandas.DataFrame, pandas.DataFrame]
        - df_output: Output results from the simulation
        - df_state_final: Final model states

    Examples
    --------
    >>> df_output, df_state_final = supy.run_supy_sample()
    """
    # Load sample data
    df_state_init, df_forcing = load_sample_data()

    # subset forcing data
    if start is not None:
        df_forcing = df_forcing[start:]
    if end is not None:
        df_forcing = df_forcing[:end]
    if start is not None and end is not None:
        df_forcing = df_forcing[start:end]

    # Run SuPy with the sample data
    res_supy = run_supy(
        df_forcing=df_forcing,
        df_state_init=df_state_init,
        save_state=save_state,
        chunk_day=chunk_day,
        logging_level=logging_level,
        check_input=check_input,
        serial_mode=serial_mode,
        debug_mode=debug_mode,
    )
    if debug_mode:
        df_output, df_state_final, df_debug = res_supy
        return df_output, df_state_final, df_debug
    else:
        df_output, df_state_final = res_supy
        return df_output, df_state_final
