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
from pathlib import Path
from typing import Optional
import warnings

import numpy as np
import pandas
import pandas as pd

from ._check import (
    check_forcing,
    check_state,
)
from ._env import logger_supy
from ._filename import prepare_filename_component
from ._load import (
    load_InitialCond_grid_df,
    load_SUEWS_dict_ModConfig,
    load_SUEWS_Forcing_met_df_raw,
    resample_forcing_met,
)
from ._save import (
    get_save_info,
    save_df_output,
    save_df_output_parquet,
    save_df_state,
    save_initcond_nml,
)

# set up logging module
logger_supy.setLevel(logging.INFO)


_FUNCTIONAL_DEPRECATIONS = {
    "load_forcing_grid": "`SUEWSSimulation(path).forcing`",
}


def _warn_functional_deprecation(name: str) -> None:
    """Emit a standardized deprecation warning for the legacy functional API.

    Uses ``FutureWarning`` so the message is visible to end users by default
    (CPython filters ``DeprecationWarning`` outside ``__main__``). The
    procedural API in this module is end-user-facing, not a developer-only
    surface, so ``FutureWarning`` is the right Python-level signal — see
    https://docs.python.org/3/library/warnings.html#warning-categories.
    """
    replacement = _FUNCTIONAL_DEPRECATIONS.get(name, "the object-oriented API")
    warnings.warn(
        f"`supy.{name}` is deprecated and will be removed in a future release. "
        f"Please migrate to {replacement}.",
        FutureWarning,
        stacklevel=3,
    )


def _load_namelist_state(
    path_runcontrol: str,
    force_reload=True,
    check_input=False,
) -> pd.DataFrame:
    """Load initial model states for the deprecated namelist CLI."""
    path_runcontrol = Path(path_runcontrol).expanduser().resolve()
    if path_runcontrol.suffix.lower() != ".nml":
        raise ValueError(
            "The legacy CLI state loader only accepts RunControl.nml files."
        )

    df_state_init = load_InitialCond_grid_df(
        path_runcontrol,
        force_reload=force_reload,
    )
    if check_input:
        list_issues = check_state(df_state_init)
        if isinstance(list_issues, list):
            raise RuntimeError(
                f"{path_runcontrol} is not valid to initialise SUEWS: "
                + "; ".join(list_issues)
            )
    return df_state_init


def _load_namelist_forcing_grid(
    path_runcontrol: str,
    grid: int,
    check_input=False,
    force_reload=True,
    df_state_init: pd.DataFrame = None,
) -> pd.DataFrame:
    """Load one grid's forcing for the deprecated namelist CLI."""
    del force_reload
    path_runcontrol = Path(path_runcontrol).expanduser().resolve()
    if path_runcontrol.suffix.lower() != ".nml":
        raise ValueError(
            "The legacy CLI forcing loader only accepts RunControl.nml files."
        )
    if df_state_init is None:
        df_state_init = _load_namelist_state(path_runcontrol)

    dict_mod_cfg = load_SUEWS_dict_ModConfig(path_runcontrol)
    path_input = path_runcontrol.parent / dict_mod_cfg["fileinputpath"]
    tstep_mod, lat, lon, alt, timezone = df_state_init.loc[
        grid, [(x, "0") for x in ["tstep", "lat", "lng", "alt", "timezone"]]
    ].values
    df_forcing_met = load_SUEWS_Forcing_met_df_raw(
        path_input,
        dict_mod_cfg["filecode"],
        grid,
        dict_mod_cfg["resolutionfilesin"],
        dict_mod_cfg["multiplemetfiles"],
    )
    df_forcing = resample_forcing_met(
        df_forcing_met,
        dict_mod_cfg["resolutionfilesin"],
        tstep_mod,
        lat,
        lon,
        alt,
        timezone,
        dict_mod_cfg["kdownzen"],
    ).round(10)
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
        except Exception as exc:
            raise RuntimeError("Invalid namelist forcing data") from exc

    return df_forcing


def load_forcing_grid(
    path_init: str,
    grid: int,
    df_state_init: pd.DataFrame = None,
) -> pd.DataFrame:
    """Load forcing for the UMEP YAML compatibility workflow.

    .. deprecated:: 2025.11.20
        Use :class:`~supy.SUEWSSimulation` and its
        :meth:`~supy.SUEWSSimulation.update_forcing` method.

    This forwarding shim is retained for the UMEP processor's YAML,
    single-grid workflow. Namelist and multi-file loading remain available
    only through the deprecated namelist CLI.
    """
    del grid, df_state_init
    _warn_functional_deprecation("load_forcing_grid")
    path_config = Path(path_init)
    if path_config.suffix.lower() not in {".yml", ".yaml"}:
        raise ValueError(
            "supy.load_forcing_grid only supports a YAML configuration; "
            "migrate namelist workflows with 'suews-convert'."
        )

    from .suews_sim import SUEWSSimulation

    simulation = SUEWSSimulation(path_config)
    if simulation.forcing is None:
        raise RuntimeError(
            f"No forcing data found in YAML configuration: {path_config}"
        )
    return simulation.forcing.to_dataframe(include_extras=True)


# input processing code end here
##############################################################################


##############################################################################
# 3. save results of a supy run
def _save_supy(
    df_output: pandas.DataFrame,
    df_state_final: pandas.DataFrame,
    freq_s: int = 3600,
    site: str = "",
    path_dir_save: str = Path("."),
    path_runcontrol: Optional[str] = None,
    save_tstep=False,
    logging_level=50,
    output_level=1,
    debug=False,
    output_config=None,
    output_format=None,
    save_state: bool = True,
) -> list:
    """Save SuPy run results to files.

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
    output_config : OutputControl, optional
        Output configuration object specifying format, frequency, and groups to save. If provided, overrides freq_s parameter.
    save_state : bool, optional
        Whether to write the legacy DFState restart artifact. Legacy callers
        keep the historical default ``True``; the OOP API writes typed
        checkpoint JSON instead.


    Returns
    -------
    list
        a list of paths of saved files

    """
    # adjust logging level
    logger_supy.setLevel(logging_level)

    # get necessary information for saving procedure
    if path_runcontrol is not None:
        freq_s, path_dir_save, site, save_tstep, output_level = get_save_info(
            path_runcontrol
        )

    # Make the site identifier safe to embed in output filenames on every
    # platform (gh#1619). The site name is concatenated verbatim into every
    # filename below; a character such as a colon is the NTFS Alternate Data
    # Stream separator on Windows and would silently write output into a hidden
    # stream instead of a normal file. This common multi-file save boundary
    # covers txt, state csv, InitialConditions nml, and Parquet. The modern OOP
    # save methods apply the same helper before constructing checkpoint names.
    site_metadata = str(site)
    site = prepare_filename_component(site_metadata, "Site name")

    # Handle output configuration if provided
    # output_format = "txt"  # default - MP: Moved as argument
    output_groups = None  # default will be handled in save_df_output

    if output_config is not None:
        from .data_model.core.model import OutputControl

        if isinstance(output_config, OutputControl):
            # Override frequency if specified in config
            if output_config.freq is not None:
                freq_s = output_config.freq
            # Fill format from config only when the caller has not set it
            # explicitly — an explicit `output_format` kwarg always wins.
            if output_format is None:
                output_format = str(output_config.format)
            # Get groups for txt format
            if output_format == "txt" and output_config.groups is not None:
                output_groups = output_config.groups
        elif isinstance(output_config, str):
            # Legacy string format - issue deprecation warning
            warnings.warn(
                "The 'output_file' parameter as a string is deprecated and was never used. "
                "Please use the new OutputControl block or remove this parameter. "
                "Falling back to default text output. "
                "Example: output: {format: 'parquet', freq: 3600}",
                DeprecationWarning,
                stacklevel=2,
            )
            if output_format is None:
                output_format = "txt"

    if output_format is None:
        output_format = "txt"

    # determine `save_snow` option
    snowuse = df_state_final.iloc[-1].loc["snowuse"]
    # Handle both scalar and array cases safely
    if hasattr(snowuse, "iloc"):
        # If it's a Series (multi-level index), get the first value
        snowuse = snowuse.iloc[0]
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
            save_state=save_state,
            site_metadata=site_metadata,
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

        # MP: Parquet saves this already - breaks the parquet save check
        # save df_state
        if save_state and path_runcontrol is not None:
            # save as nml as SUEWS binary
            list_path_nml = save_initcond_nml(df_state_final, site, path_dir_save)
            list_path_save += list_path_nml
        elif save_state:
            # save as supy csv for later use
            path_state_save = save_df_state(df_state_final, site, path_dir_save)
            # update list_path_save
            list_path_save.append(path_state_save)

    return list_path_save
