"""DTS population functions.

Functions for populating DTS objects from Python data structures
(timestamps, dictionaries, DataFrames).
"""

from typing import Any, Dict

import numpy as np
import pandas as pd

from ..supy_driver import module_ctrl_type as dts
from .. import _state_accessors as acc


def populate_timer_from_datetime(
    timer: dts.SUEWS_TIMER,
    dt: pd.Timestamp,
    tstep_s: int = 3600,
    dt_since_start: int = 0,
    is_dls: bool = False,
) -> None:
    """Populate SUEWS_TIMER from a pandas Timestamp.

    Parameters
    ----------
    timer : dts.SUEWS_TIMER
        Timer object to populate
    dt : pd.Timestamp
        Datetime for this timestep
    tstep_s : int
        Timestep in seconds
    dt_since_start : int
        Seconds since simulation start
    is_dls : bool
        Whether daylight saving is active

    Notes
    -----
    **Temporal Conventions (matching Fortran SUEWS expectations):**

    - ``id`` (day of year): 1-indexed, where Jan 1 = 1, Dec 31 = 365/366.
      Uses pandas ``dayofyear`` which follows the same convention.

    - ``dectime``: Decimal day of year, e.g., noon on Jan 1 = 1.5.
      Formula: dayofyear + (hour + min/60 + sec/3600) / 24.

    - ``dayofweek_id``: 1=Monday, 7=Sunday. Converted from pandas
      convention (0=Monday, 6=Sunday) by adding 1. This matches the
      ISO 8601 weekday numbering and SUEWS Fortran expectations.

    - ``new_day``: True when hour=0 and minute=0, indicating the first
      timestep of a new day.
    """
    timer.iy = dt.year
    timer.id = dt.dayofyear  # 1-indexed: Jan 1 = 1
    timer.it = dt.hour
    timer.imin = dt.minute
    timer.isec = dt.second
    timer.tstep = tstep_s
    timer.tstep_real = float(tstep_s)
    timer.nsh_real = 3600.0 / tstep_s
    timer.nsh = int(3600 / tstep_s)
    timer.dt_since_start = dt_since_start
    timer.dls = 1 if is_dls else 0

    # Calculate decimal time (day of year + fraction of day)
    # e.g., noon on Jan 1 = 1.5, midnight on Jan 2 = 2.0
    frac_day = (dt.hour + dt.minute / 60.0 + dt.second / 3600.0) / 24.0
    timer.dectime = float(dt.dayofyear) + frac_day

    # Day of week: 1=Monday, 7=Sunday (ISO 8601 convention)
    # pandas uses 0=Monday, 6=Sunday, so add 1 to convert
    timer.dayofweek_id = dt.dayofweek + 1

    # New day flag (true if this is first timestep of the day)
    timer.new_day = dt.hour == 0 and dt.minute == 0


def populate_config_from_dict(config: dts.SUEWS_CONFIG, params: Dict[str, Any]) -> None:
    """Populate SUEWS_CONFIG from a parameter dictionary.

    Parameters
    ----------
    config : dts.SUEWS_CONFIG
        Config object to populate
    params : dict
        Dictionary of config parameters
    """
    # Method flags mapping
    method_attrs = [
        "rslmethod",
        "emissionsmethod",
        "storageheatmethod",
        "ohmincqf",
        "netradiationmethod",
        "stabilitymethod",
        "roughlenheatmethod",
        "roughlenmommethod",
        "smdmethod",
        "snowuse",
        "waterusemethod",
        "laimethod",
        "evapmethod",
        "rcmethod",
        "stebbsmethod",
        "faimethod",
        "diagnose",
        "diagqs",
        "flag_test",
        "use_sw_direct_albedo",
    ]

    for attr in method_attrs:
        if attr in params:
            setattr(config, attr, int(params[attr]))


def populate_forcing_from_row(forcing: dts.SUEWS_FORCING, row: pd.Series) -> None:
    """Populate SUEWS_FORCING from a forcing data row.

    Parameters
    ----------
    forcing : dts.SUEWS_FORCING
        Forcing object to populate
    row : pd.Series
        Single row of forcing data

    Notes
    -----
    Supports both raw SUEWS forcing file column names (Tair, RH, U, etc.)
    and normalised column names (temp_c, avrh, wind_speed, etc.).
    Missing value indicator (-999) is converted to 0.0.
    """
    # Map forcing data columns to DTS attributes
    # Supports both raw SUEWS column names and normalised names
    # Format: {input_column_name: dts_attribute_name}
    forcing_map = {
        # Raw SUEWS forcing file column names
        "Tair": "temp_c",
        "RH": "rh",
        "pres": "pres",
        "rain": "rain",
        "kdown": "kdown",
        "ldown": "ldown",
        "fcld": "fcld",
        "U": "u",
        "xsmd": "xsmd",
        "lai": "lai_obs",
        "qn": "qn1_obs",
        "qf": "qf_obs",
        "qs": "qs_obs",
        "snow": "snowfrac",
        # Normalised/alternative column names
        "temp_c": "temp_c",
        "avrh": "rh",
        "press_hpa": "pres",
        "precip": "rain",
        "ldown_obs": "ldown",
        "fcld_obs": "fcld",
        "qn1_obs": "qn1_obs",
        "qs_obs": "qs_obs",
        "qf_obs": "qf_obs",
        "snowfrac_obs": "snowfrac",
        "lai_obs": "lai_obs",
    }

    for col, attr in forcing_map.items():
        if col in row.index:
            val = float(row[col])
            # Convert missing value indicator to 0.0
            if val == -999.0:
                val = 0.0
            setattr(forcing, attr, val)


def populate_state_from_config(
    state: dts.SUEWS_STATE,
    config_dict: Dict[str, Any],
    nlayer: int = 5,
    ndepth: int = 5,
    nsurf: int = 7,
) -> None:
    """Populate SUEWS_STATE from configuration dictionary.

    Uses accessor functions to set nested state arrays.

    Parameters
    ----------
    state : dts.SUEWS_STATE
        Allocated state object to populate
    config_dict : dict
        Configuration dictionary with initial state values
    nlayer : int
        Number of layers
    ndepth : int
        Number of depth levels
    nsurf : int
        Number of surfaces
    """
    # Extract initial heat state values
    if "temp_roof" in config_dict:
        temp_roof = np.array(config_dict["temp_roof"], dtype=np.float64, order="F")
        temp_wall = np.array(
            config_dict.get("temp_wall", temp_roof), dtype=np.float64, order="F"
        )
        temp_surf = np.array(
            config_dict.get("temp_surf", np.zeros((nsurf, ndepth))),
            dtype=np.float64,
            order="F",
        )
        acc.set_heat_state_temp(
            state, nlayer, ndepth, nsurf, temp_roof, temp_wall, temp_surf
        )

    # Extract initial surface temperatures
    if "tsfc_roof" in config_dict:
        tsfc_roof = np.array(config_dict["tsfc_roof"], dtype=np.float64)
        tsfc_wall = np.array(
            config_dict.get("tsfc_wall", tsfc_roof), dtype=np.float64
        )
        tsfc_surf = np.array(
            config_dict.get("tsfc_surf", np.zeros(nsurf)), dtype=np.float64
        )
        acc.set_heat_state_tsfc(state, nlayer, nsurf, tsfc_roof, tsfc_wall, tsfc_surf)

    # Extract initial hydro state
    if "soilstore_surf" in config_dict:
        soilstore_roof = np.array(
            config_dict.get("soilstore_roof", np.zeros(nlayer)), dtype=np.float64
        )
        soilstore_wall = np.array(
            config_dict.get("soilstore_wall", np.zeros(nlayer)), dtype=np.float64
        )
        soilstore_surf = np.array(config_dict["soilstore_surf"], dtype=np.float64)
        acc.set_hydro_state_soilstore(
            state, nlayer, soilstore_roof, soilstore_wall, soilstore_surf
        )

    if "state_surf" in config_dict:
        state_roof = np.array(
            config_dict.get("state_roof", np.zeros(nlayer)), dtype=np.float64
        )
        state_wall = np.array(
            config_dict.get("state_wall", np.zeros(nlayer)), dtype=np.float64
        )
        state_surf = np.array(config_dict["state_surf"], dtype=np.float64)
        acc.set_hydro_state_wetness(state, nlayer, state_roof, state_wall, state_surf)

    # Extract initial snow state
    if "snowpack" in config_dict:
        snowpack = np.array(config_dict["snowpack"], dtype=np.float64)
        snowfrac = np.array(
            config_dict.get("snowfrac", np.zeros(nsurf)), dtype=np.float64
        )
        snowdens = np.array(
            config_dict.get("snowdens", np.full(nsurf, 100.0)), dtype=np.float64
        )
        icefrac = np.array(
            config_dict.get("icefrac", np.zeros(nsurf)), dtype=np.float64
        )
        acc.set_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)

    if "snowalb" in config_dict:
        acc.set_snow_state_scalars(
            state,
            config_dict.get("snowalb", 0.6),
            config_dict.get("swe", 0.0),
            config_dict.get("mwh", 0.0),
            config_dict.get("qm", 0.0),
        )
