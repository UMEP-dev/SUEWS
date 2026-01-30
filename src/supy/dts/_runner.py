"""DTS-based simulation runner for SUEWS.

This module provides the main run_dts() function that executes SUEWS
using direct DTS (Derived Type Structure) objects, bypassing the
intermediate df_state conversion layer.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import numpy as np
import pandas as pd

if TYPE_CHECKING:
    from ..data_model import SUEWSConfig

from ..supy_driver import (
    module_ctrl_const_allocate as alloc,
    suews_driver as drv,
)
from ._core import (
    create_suews_config,
    create_suews_forcing,
    create_suews_site,
    create_suews_state,
    create_suews_timer,
)
from ._extract import (
    build_output_dataframe_from_block,
    extract_state_from_dts,
)
from ._populate import (
    populate_atmstate,
    populate_config_from_pydantic,
    populate_forcing_from_row,
    populate_ohmstate_defaults,
    populate_roughnessstate,
    populate_site_from_pydantic,
    populate_state_from_pydantic,
    populate_storedrainprm,
    populate_timer_from_datetime,
)


def _prepare_forcing_block(df_forcing: pd.DataFrame) -> np.ndarray:
    """Prepare forcing block array for batch DTS execution.

    The forcing block has 21 columns matching the Fortran MetForcingBlock format:
    [iy, id, it, imin, qn1_obs, qh_obs, qe, qs_obs, qf_obs, avu1, avrh,
     temp_c, press_hpa, precip, kdown, snowfrac_obs, ldown_obs, fcld_obs,
     wu_m3, xsmd, lai_obs]

    Parameters
    ----------
    df_forcing : pd.DataFrame
        Forcing DataFrame with datetime index.

    Returns
    -------
    np.ndarray
        Fortran-ordered array of shape (len_sim, 21).
    """
    len_sim = len(df_forcing)
    block = np.zeros((len_sim, 21), dtype=np.float64, order="F")

    # Time columns from index
    block[:, 0] = df_forcing.index.year  # iy
    block[:, 1] = df_forcing.index.dayofyear  # id
    block[:, 2] = df_forcing.index.hour  # it
    block[:, 3] = df_forcing.index.minute  # imin

    # Map forcing columns to block positions
    # Column mapping: (block_index, df_column, default)
    col_map = [
        (4, "qn", 0.0),  # qn1_obs
        (5, "qh", 0.0),  # qh_obs (reserved)
        (6, "qe", 0.0),  # qe (reserved)
        (7, "qs", 0.0),  # qs_obs
        (8, "qf", 0.0),  # qf_obs
        (9, "U", 0.0),  # avu1
        (10, "RH", 0.0),  # avrh
        (11, "Tair", 0.0),  # temp_c
        (12, "pres", 0.0),  # press_hpa
        (13, "rain", 0.0),  # precip
        (14, "kdown", 0.0),  # kdown
        (15, "snow", 0.0),  # snowfrac_obs
        (16, "ldown", 0.0),  # ldown_obs
        (17, "fcld", 0.0),  # fcld_obs
        (18, "Wuh", 0.0),  # wu_m3
        (19, "xsmd", 0.0),  # xsmd
        (20, "lai", 0.0),  # lai_obs
    ]

    for idx, col, default in col_map:
        if col in df_forcing.columns:
            block[:, idx] = df_forcing[col].values
        else:
            block[:, idx] = default

    return block


def run_dts(
    df_forcing: pd.DataFrame,
    config: SUEWSConfig,
    site_index: int = 0,
    nlayer: int | None = None,
    ndepth: int | None = None,
) -> tuple[pd.DataFrame, dict]:
    """Run SUEWS simulation using DTS interface.

    This function provides a direct path from Pydantic configuration to
    Fortran kernel execution, eliminating the intermediate df_state layer.
    Uses batch execution via suews_cal_multitsteps_dts for efficiency.

    Parameters
    ----------
    df_forcing : pd.DataFrame
        Forcing data with datetime index and meteorological variables.
    config : SUEWSConfig
        Pydantic configuration object containing Model, Site, and InitialStates.
    site_index : int, optional
        Index of site to simulate (for multi-site configs), by default 0.
    nlayer : int, optional
        Number of vertical layers. If None, inferred from
        config.sites[site_index].properties.vertical_layers.nlayer.
    ndepth : int, optional
        Number of substrate depth levels. If None, uses the Fortran constant (5).

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
    4. Prepare forcing block and output array
    5. Call batch suews_cal_multitsteps_dts
    6. Build output DataFrame from result block
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

    # Infer nlayer/ndepth if not provided (default: 5 per Fortran constants)
    if nlayer is None:
        try:
            val = site.properties.vertical_layers.nlayer
            nlayer = int(val.value if hasattr(val, "value") else val)
        except (AttributeError, TypeError):
            nlayer = 5

    if ndepth is None:
        ndepth = 5

    # Create DTS objects
    config_dts = create_suews_config()
    site_dts = create_suews_site(nlayer=nlayer, ndepth=ndepth)
    state_dts = create_suews_state(nlayer=nlayer, ndepth=ndepth)
    forcing_dts = create_suews_forcing()
    timer_dts = create_suews_timer()

    # Populate from Pydantic config
    populate_config_from_pydantic(config_dts, model)
    populate_site_from_pydantic(site_dts, site, model)
    land_cover = site.properties.land_cover
    populate_state_from_pydantic(
        state_dts, initial_states, nlayer, ndepth, land_cover=land_cover
    )

    # Populate storedrainprm (needs land cover from site)
    populate_storedrainprm(state_dts, land_cover)

    # Calculate derived site parameters
    site_dts.cal_surf(config_dts)

    # Initialize state components dependent on site/forcing defaults
    populate_roughnessstate(state_dts, site_dts)
    populate_ohmstate_defaults(state_dts)

    # Initialize atmospheric state from first forcing timestep
    first_row = df_forcing.iloc[0]
    populate_forcing_from_row(forcing_dts, first_row)

    # Get atmospheric state values from initial_states for continuation runs
    def _get_atm_value(name: str) -> float | None:
        val = getattr(initial_states, name, None)
        if val is not None:
            return val.value if hasattr(val, "value") else val
        return None

    tair_av_init = _get_atm_value("tair_av")
    l_mod_init = _get_atm_value("l_mod")
    ustar_init = _get_atm_value("ustar")
    ra_h_init = _get_atm_value("ra_h")
    rb_init = _get_atm_value("rb")
    rs_init = _get_atm_value("rs")

    populate_atmstate(
        state_dts,
        forcing_dts,
        tair_av=tair_av_init,
        l_mod=l_mod_init,
        ustar=ustar_init,
        ra_h=ra_h_init,
        rb=rb_init,
        rs=rs_init,
    )

    # Prepare forcing block for batch execution
    len_sim = len(df_forcing)
    metforcingblock = _prepare_forcing_block(df_forcing)

    # Prepare output array
    ncols_out = int(alloc.ncolumnsdataoutsuews)
    dataoutblock = np.zeros((len_sim, ncols_out), dtype=np.float64, order="F")

    # Initialize timer with first timestep info
    dt_start = df_forcing.index[0]
    populate_timer_from_datetime(
        timer_dts,
        dt_start,
        tstep_s,
        dt_since_start=0,
        startdls=int(site_dts.anthroemis.startdls),
        enddls=int(site_dts.anthroemis.enddls),
        lat=site_dts.lat,
    )

    # Execute batch simulation
    drv.suews_cal_multitsteps_dts(
        timer_dts,
        metforcingblock,
        len_sim,
        config_dts,
        site_dts,
        state_dts,
        dataoutblock,
    )

    # Build output DataFrame from result block
    grid_id = site.gridiv
    df_output = build_output_dataframe_from_block(
        dataoutblock,
        datetime_index=df_forcing.index,
        grid_id=grid_id,
    )

    # Extract final state to Pydantic InitialStates for continuation runs
    initial_states_final = extract_state_from_dts(
        state_dts=state_dts,
        nlayer=nlayer,
        ndepth=ndepth,
    )

    # Prepare final state dictionary (for potential restart)
    final_state = {
        "state_dts": state_dts,
        "site_dts": site_dts,
        "config_dts": config_dts,
        "initial_states": initial_states_final,
    }

    return df_output, final_state
