"""Output and state extraction functions for DTS simulation results.

This module provides functions to extract output and state from SUEWS DTS objects
and convert to DataFrame or Pydantic formats.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import numpy as np
import pandas as pd

from .._post import gen_index
from ..supy_driver import module_ctrl_type as dts

if TYPE_CHECKING:
    from ..data_model.core.state import InitialStates

_OUTPUT_LINE_FIELDS = (
    "dataoutlinesuews",
    "datetimeline",
    "dataoutlinesnow",
    "dataoutlinedailystate",
    "dataoutlinersl",
    "dataoutlinebeers",
    "dataoutlinedebug",
    "dataoutlineestm",
    "dataoutlineehc",
    "dataoutlinespartacus",
    "dataoutlinestebbs",
    "dataoutlinenhood",
)

_GROUP_TO_KEY = {
    "SUEWS": "dataoutlinesuews",
    "snow": "dataoutlinesnow",
    "DailyState": "dataoutlinedailystate",
    "RSL": "dataoutlinersl",
    "BEERS": "dataoutlinebeers",
    "debug": "dataoutlinedebug",
    "ESTM": "dataoutlineestm",
    "EHC": "dataoutlineehc",
    "SPARTACUS": "dataoutlinespartacus",
    "STEBBS": "dataoutlinestebbs",
    "Nhood": "dataoutlinenhood",
}


def _attach_grid_index(
    df: pd.DataFrame,
    grid_id: int,
    datetime_index: pd.DatetimeIndex,
) -> pd.DataFrame:
    df.index = pd.MultiIndex.from_product(
        [[grid_id], datetime_index], names=["grid", "datetime"]
    )
    return df


def extract_output_line_to_dict(output_line: dts.output_line) -> dict[str, np.ndarray]:
    """Extract output_line arrays to dictionary.

    Parameters
    ----------
    output_line : dts.output_line
        Output line object from suews_cal_main.

    Returns
    -------
    dict
        Dictionary with output array names as keys.
    """
    return {
        key: np.array(getattr(output_line, key)).copy()
        for key in _OUTPUT_LINE_FIELDS
    }


def build_output_dataframe_from_block(
    dataoutblock: np.ndarray,
    datetime_index: pd.DatetimeIndex,
    grid_id: int = 1,
) -> pd.DataFrame:
    """Build output DataFrame from batch output block array.

    This function is used with suews_cal_multitsteps_dts batch execution.

    Parameters
    ----------
    dataoutblock : np.ndarray
        Output array of shape (len_sim, ncolumnsdataoutsuews) from batch execution.
        First 5 columns are datetime (skipped), remaining are output variables.
    datetime_index : pd.DatetimeIndex
        Datetime index for the output.
    grid_id : int
        Grid identifier.

    Returns
    -------
    pd.DataFrame
        Output DataFrame with MultiIndex columns (group, var).
    """
    # Skip first 5 datetime columns
    arr_suews = dataoutblock[:, 5:]

    # Get column index for SUEWS group
    idx_suews = gen_index("dataoutlinesuews")

    df_output = pd.DataFrame(arr_suews, columns=idx_suews, index=datetime_index)
    return _attach_grid_index(df_output, grid_id, datetime_index)


def build_full_output_dataframe(
    list_output_dicts: list[dict[str, np.ndarray]],
    datetime_index: pd.DatetimeIndex,
    grid_id: int = 1,
    include_groups: list[str] | None = None,
) -> pd.DataFrame:
    """Build complete output DataFrame including all output groups.

    Parameters
    ----------
    list_output_dicts : list of dict
        List of output dictionaries, one per timestep.
    datetime_index : pd.DatetimeIndex
        Datetime index for the output.
    grid_id : int
        Grid identifier.
    include_groups : list of str, optional
        List of groups to include. If None, includes SUEWS only.

    Returns
    -------
    pd.DataFrame
        Output DataFrame with MultiIndex columns (group, var).
    """
    if include_groups is None:
        include_groups = ["SUEWS"]

    dfs = []
    for group in include_groups:
        key = _GROUP_TO_KEY.get(group)
        if key is None:
            continue

        # Stack arrays - skip first 5 datetime columns
        try:
            arr = np.vstack([d[key][5:] for d in list_output_dicts])
        except (KeyError, IndexError):
            continue

        # Get column index
        idx = gen_index(key)

        # Create DataFrame
        df_group = pd.DataFrame(arr, columns=idx, index=datetime_index)
        dfs.append(df_group)

    if not dfs:
        return pd.DataFrame()

    # Concatenate all groups
    df_output = pd.concat(dfs, axis=1)

    return _attach_grid_index(df_output, grid_id, datetime_index)


def extract_state_from_dts(
    state_dts: dts.SUEWS_STATE,
    nlayer: int,
    ndepth: int,
) -> "InitialStates":
    """Extract final state from Fortran DTS objects to Pydantic InitialStates.

    This is the reverse of populate_state_from_pydantic() - it reads the final
    state from Fortran DTS objects after simulation and creates a Pydantic
    InitialStates object that can be used for continuation runs or persistence.

    Parameters
    ----------
    state_dts : dts.SUEWS_STATE
        Fortran state object after simulation.
    nlayer : int
        Number of vertical layers for roof/wall.
    ndepth : int
        Number of depth levels for temperature profiles.

    Returns
    -------
    InitialStates
        Pydantic model containing extracted state for continuation runs.
    """
    # Import here to avoid circular imports
    from ..data_model.core.state import (
        HDD_ID,
        InitialStateBldgs,
        InitialStateBsoil,
        InitialStateDectr,
        InitialStateEvetr,
        InitialStateGrass,
        InitialStatePaved,
        InitialStates,
        InitialStateWater,
        SurfaceInitialState,
        WaterUse,
    )
    from ..data_model.core.type import RefValue

    # Extract arrays from Fortran state
    # Heat state
    temp_surf = np.array(state_dts.heatstate.temp_surf)
    tsfc_surf = np.array(state_dts.heatstate.tsfc_surf)

    # Hydro state
    soilstore_surf = np.array(state_dts.hydrostate.soilstore_surf)
    state_surf = np.array(state_dts.hydrostate.state_surf)
    wuday_id = np.array(state_dts.hydrostate.wuday_id)

    # Snow state
    snowpack = np.array(state_dts.snowstate.snowpack)
    snowfrac = np.array(state_dts.snowstate.snowfrac)
    snowdens = np.array(state_dts.snowstate.snowdens)
    icefrac = np.array(state_dts.snowstate.icefrac)
    snowwater = np.array(state_dts.snowstate.snowwater)
    snowalb = float(state_dts.snowstate.snowalb)
    snowfallcum = float(state_dts.snowstate.snowfallcum)

    # Phenology state
    lai_id = np.array(state_dts.phenstate.lai_id)
    gdd_id = np.array(state_dts.phenstate.gdd_id)
    sdd_id = np.array(state_dts.phenstate.sdd_id)
    porosity_id = float(state_dts.phenstate.porosity_id)
    decidcap_id = float(state_dts.phenstate.decidcap_id)
    tmin_id = float(state_dts.phenstate.tmin_id)
    tmax_id = float(state_dts.phenstate.tmax_id)
    lenday_id = float(state_dts.phenstate.lenday_id)
    albevetr_id = float(state_dts.phenstate.albevetr_id)
    albdectr_id = float(state_dts.phenstate.albdectr_id)
    albgrass_id = float(state_dts.phenstate.albgrass_id)

    # Anthropogenic emissions state
    hdd_id_arr = np.array(state_dts.anthroemisstate.hdd_id)

    # OHM state
    dqndt = float(state_dts.ohmstate.dqndt)
    dqnsdt = float(state_dts.ohmstate.dqnsdt)
    qn_av = float(state_dts.ohmstate.qn_av)
    qn_s_av = float(state_dts.ohmstate.qn_s_av)

    # Atmospheric state (for continuation runs)
    tair_av = float(state_dts.atmstate.tair_av)
    l_mod = float(state_dts.atmstate.l_mod)
    ustar = float(state_dts.atmstate.ustar)
    ra_h = float(state_dts.atmstate.ra_h)
    rb = float(state_dts.atmstate.rb)
    rs = float(state_dts.atmstate.rs)

    # Helper to create surface state
    def _make_surface_state(idx: int) -> SurfaceInitialState:
        return SurfaceInitialState(
            state=RefValue(float(state_surf[idx])),
            soilstore=RefValue(float(soilstore_surf[idx])),
            snowfrac=RefValue(float(snowfrac[idx])),
            snowpack=RefValue(float(snowpack[idx])),
            icefrac=RefValue(float(icefrac[idx])),
            snowwater=RefValue(float(snowwater[idx])),
            snowdens=RefValue(float(snowdens[idx])),
            temperature=RefValue([float(temp_surf[idx, j]) for j in range(ndepth)]),
            tsfc=RefValue(float(tsfc_surf[idx])),
        )

    # Helper for water use (veg_idx: 0=evetr, 1=dectr, 2=grass)
    def _make_water_use(veg_idx: int) -> WaterUse:
        base_idx = veg_idx * 3
        return WaterUse(
            wu_total=RefValue(float(wuday_id[base_idx])),
            wu_auto=RefValue(float(wuday_id[base_idx + 1])),
            wu_manual=RefValue(float(wuday_id[base_idx + 2])),
        )

    # Create surface states
    # Surface order: paved(0), bldgs(1), evetr(2), dectr(3), grass(4), bsoil(5), water(6)
    paved = InitialStatePaved(
        **_make_surface_state(0).model_dump(),
    )

    bldgs = InitialStateBldgs(
        **_make_surface_state(1).model_dump(),
    )

    # Vegetation surfaces (indices 2, 3, 4 correspond to veg_idx 0, 1, 2)
    evetr = InitialStateEvetr(
        **_make_surface_state(2).model_dump(),
        alb_id=RefValue(albevetr_id),
        lai_id=RefValue(float(lai_id[0])),
        gdd_id=RefValue(float(gdd_id[0])),
        sdd_id=RefValue(float(sdd_id[0])),
        wu=_make_water_use(0),
    )

    dectr = InitialStateDectr(
        **_make_surface_state(3).model_dump(),
        alb_id=RefValue(albdectr_id),
        lai_id=RefValue(float(lai_id[1])),
        gdd_id=RefValue(float(gdd_id[1])),
        sdd_id=RefValue(float(sdd_id[1])),
        wu=_make_water_use(1),
        porosity_id=RefValue(porosity_id),
        decidcap_id=RefValue(decidcap_id),
    )

    grass = InitialStateGrass(
        **_make_surface_state(4).model_dump(),
        alb_id=RefValue(albgrass_id),
        lai_id=RefValue(float(lai_id[2])),
        gdd_id=RefValue(float(gdd_id[2])),
        sdd_id=RefValue(float(sdd_id[2])),
        wu=_make_water_use(2),
    )

    bsoil = InitialStateBsoil(
        **_make_surface_state(5).model_dump(),
    )

    water = InitialStateWater(
        **_make_surface_state(6).model_dump(),
    )

    # Extract roof/wall states if available
    roofs = []
    walls = []

    # Check if roof arrays are allocated and have data
    # Note: For roofs/walls, soilstore/state are not physically meaningful
    # but must satisfy validation constraints (soilstore >= 10 for default model)
    try:
        temp_roof = np.array(state_dts.heatstate.temp_roof)
        tsfc_roof = np.array(state_dts.heatstate.tsfc_roof)
        soilstore_roof = np.array(state_dts.hydrostate.soilstore_roof)
        state_roof = np.array(state_dts.hydrostate.state_roof)

        for i in range(min(nlayer, len(tsfc_roof))):
            # Ensure soilstore meets validation (min 10 for non-water surfaces)
            soilstore_val = max(float(soilstore_roof[i]), 10.0)
            # Ensure state meets validation (min 0 for water state)
            state_val = max(float(state_roof[i]), 0.0)
            roofs.append(
                SurfaceInitialState(
                    state=RefValue(state_val),
                    soilstore=RefValue(soilstore_val),
                    temperature=RefValue(
                        [float(temp_roof[i, j]) for j in range(ndepth)]
                    ),
                    tsfc=RefValue(float(tsfc_roof[i])),
                    snowfrac=None,
                    snowpack=None,
                    icefrac=None,
                    snowwater=None,
                    snowdens=None,
                )
            )
    except (AttributeError, IndexError, TypeError):
        # Roof arrays not allocated - use defaults
        roofs = [SurfaceInitialState() for _ in range(nlayer)]

    try:
        temp_wall = np.array(state_dts.heatstate.temp_wall)
        tsfc_wall = np.array(state_dts.heatstate.tsfc_wall)
        soilstore_wall = np.array(state_dts.hydrostate.soilstore_wall)
        state_wall = np.array(state_dts.hydrostate.state_wall)

        for i in range(min(nlayer, len(tsfc_wall))):
            # Ensure soilstore meets validation (min 10 for non-water surfaces)
            soilstore_val = max(float(soilstore_wall[i]), 10.0)
            # Ensure state meets validation (min 0 for water state)
            state_val = max(float(state_wall[i]), 0.0)
            walls.append(
                SurfaceInitialState(
                    state=RefValue(state_val),
                    soilstore=RefValue(soilstore_val),
                    temperature=RefValue(
                        [float(temp_wall[i, j]) for j in range(ndepth)]
                    ),
                    tsfc=RefValue(float(tsfc_wall[i])),
                    snowfrac=None,
                    snowpack=None,
                    icefrac=None,
                    snowwater=None,
                    snowdens=None,
                )
            )
    except (AttributeError, IndexError, TypeError):
        # Wall arrays not allocated - use defaults
        walls = [SurfaceInitialState() for _ in range(nlayer)]

    # Create HDD_ID from array (field names match HDD_ID model)
    hdd_id = HDD_ID(
        hdd_accum=float(hdd_id_arr[0]),
        cdd_accum=float(hdd_id_arr[1]),
        temp_accum=float(hdd_id_arr[2]),
        temp_5day_accum=float(hdd_id_arr[3]),
        precip_accum=float(hdd_id_arr[4]),
        days_since_rain_accum=float(hdd_id_arr[5]),
        hdd_daily=float(hdd_id_arr[6]),
        cdd_daily=float(hdd_id_arr[7]),
        temp_daily_mean=float(hdd_id_arr[8]),
        temp_5day_mean=float(hdd_id_arr[9]),
        precip_daily_total=float(hdd_id_arr[10]),
        days_since_rain=float(hdd_id_arr[11]),
    )

    # Create final InitialStates
    return InitialStates(
        snowalb=RefValue(snowalb),
        paved=paved,
        bldgs=bldgs,
        evetr=evetr,
        dectr=dectr,
        grass=grass,
        bsoil=bsoil,
        water=water,
        roofs=roofs,
        walls=walls,
        dqndt=dqndt,
        dqnsdt=dqnsdt,
        dt_since_start=0.0,  # Reset for continuation
        lenday_id=int(lenday_id),
        qn_av=qn_av,
        qn_s_av=qn_s_av,
        tair_av=tair_av,  # 5-day moving average temperature [K]
        tmax_id=tmax_id,
        tmin_id=tmin_id,
        tstep_prev=0.0,  # Will be set by next run
        snowfallcum=snowfallcum,
        hdd_id=hdd_id,
        # Atmospheric state for resistance calculations
        l_mod=l_mod,
        ustar=ustar,
        ra_h=ra_h,
        rb=rb,
        rs=rs,
    )
