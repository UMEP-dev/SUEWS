"""DTS-based simulation runner for SUEWS.

This module provides the main run_dts() function that executes SUEWS
using direct DTS (Derived Type Structure) objects, bypassing the
intermediate df_state conversion layer.
"""

from typing import Tuple, Optional

import numpy as np
import pandas as pd

from ..supy_driver import module_ctrl_type as dts
from ..supy_driver import suews_driver as drv

from ._core import (
    create_suews_config,
    create_suews_state,
    create_suews_site,
    create_suews_forcing,
    create_suews_timer,
    create_output_line,
)
from ._populate import (
    populate_config_from_pydantic,
    populate_site_from_pydantic,
    populate_state_from_pydantic,
    populate_storedrainprm,
    populate_forcing_from_row,
    populate_timer_from_datetime,
)
from ._extract import (
    extract_output_line_to_dict,
    build_output_dataframe,
)


def run_dts(
    df_forcing: pd.DataFrame,
    config: "SUEWSConfig",  # noqa: F821
    site_index: int = 0,
    nlayer: int = 5,
    ndepth: int = 5,
) -> Tuple[pd.DataFrame, dict]:
    """Run SUEWS simulation using DTS interface.

    This function provides a direct path from Pydantic configuration to
    Fortran kernel execution, eliminating the intermediate df_state layer.

    Parameters
    ----------
    df_forcing : pd.DataFrame
        Forcing data with datetime index and meteorological variables.
    config : SUEWSConfig
        Pydantic configuration object containing Model, Site, and InitialStates.
    site_index : int, optional
        Index of site to simulate (for multi-site configs), by default 0.
    nlayer : int, optional
        Number of vertical layers, by default 5.
    ndepth : int, optional
        Number of substrate depth levels, by default 5.

    Returns
    -------
    df_output : pd.DataFrame
        Output DataFrame with MultiIndex columns (group, var).
    final_state : dict
        Dictionary containing final state for potential restart.

    Notes
    -----
    The function follows this workflow:
    1. Create DTS objects (config, site, state, forcing, timer)
    2. Populate from Pydantic configuration
    3. Calculate derived site parameters
    4. Loop over forcing timesteps calling suews_cal_main
    5. Extract and build output DataFrame
    """
    # Get components from config
    model = config.model
    site = config.sites[site_index] if hasattr(config, "sites") else config.site
    initial_states = site.initial_states

    # Determine timestep from forcing index
    if len(df_forcing) > 1:
        tstep_s = int((df_forcing.index[1] - df_forcing.index[0]).total_seconds())
    else:
        tstep_s = 3600  # Default 1 hour

    # Create DTS objects
    config_dts = create_suews_config()
    site_dts = create_suews_site(nlayer=nlayer)
    state_dts = create_suews_state(nlayer=nlayer, ndepth=ndepth)
    forcing_dts = create_suews_forcing()
    timer_dts = create_suews_timer()

    # Populate from Pydantic config
    populate_config_from_pydantic(config_dts, model)
    populate_site_from_pydantic(site_dts, site, model)
    land_cover = site.properties.land_cover
    populate_state_from_pydantic(state_dts, initial_states, nlayer, ndepth, land_cover=land_cover)

    # Populate storedrainprm (needs land cover from site)
    land_cover = site.properties.land_cover
    populate_storedrainprm(state_dts, land_cover)

    # Calculate derived site parameters
    site_dts.cal_surf(config_dts)

    # Prepare output collection
    list_output_dicts = []
    dt_start = df_forcing.index[0]

    # Run simulation loop
    for i, (dt, row) in enumerate(df_forcing.iterrows()):
        # Calculate time since start
        dt_since_start = int((dt - dt_start).total_seconds())

        # Populate forcing and timer for this timestep
        populate_forcing_from_row(forcing_dts, row)
        populate_timer_from_datetime(timer_dts, dt, tstep_s, dt_since_start)

        # Create fresh output line
        output_line = create_output_line()

        # Call the SUEWS kernel
        output_line = drv.suews_cal_main(
            timer_dts,
            forcing_dts,
            config_dts,
            site_dts,
            state_dts,
        )

        # Extract output to dictionary
        output_dict = extract_output_line_to_dict(output_line)
        list_output_dicts.append(output_dict)

    # Build output DataFrame
    grid_id = site.properties.id if hasattr(site.properties, "id") else 1
    df_output = build_output_dataframe(
        list_output_dicts,
        datetime_index=df_forcing.index,
        grid_id=grid_id,
    )

    # Prepare final state dictionary (for potential restart)
    final_state = {
        "state_dts": state_dts,
        "site_dts": site_dts,
        "config_dts": config_dts,
    }

    return df_output, final_state


def run_dts_simple(
    df_forcing: pd.DataFrame,
    config: "SUEWSConfig",  # noqa: F821
    site_index: int = 0,
) -> pd.DataFrame:
    """Simplified run_dts that returns only the output DataFrame.

    This is a convenience wrapper around run_dts() for cases where
    only the output is needed.

    Parameters
    ----------
    df_forcing : pd.DataFrame
        Forcing data with datetime index.
    config : SUEWSConfig
        Pydantic configuration object.
    site_index : int, optional
        Site index, by default 0.

    Returns
    -------
    pd.DataFrame
        Output DataFrame.
    """
    df_output, _ = run_dts(df_forcing, config, site_index)
    return df_output
