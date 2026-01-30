"""Population functions for mapping Pydantic models to DTS objects.

This module provides functions to populate SUEWS Fortran Derived Type Structures
directly from Pydantic configuration models, eliminating the intermediate
DataFrame conversion layer.
"""

from __future__ import annotations

from enum import Enum
from typing import TYPE_CHECKING, Any

import numpy as np
import pandas as pd

from ..supy_driver import module_ctrl_type as dts

if TYPE_CHECKING:
    from ..data_model.core import LandCover, Model, Site
    from ..data_model.core.state import InitialStates

SURFACE_ORDER = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
VEG_SURFACE_ORDER = ("evetr", "dectr", "grass")


def _unwrap(value: Any) -> Any:
    """Unwrap RefValue wrapper to get raw value.

    Parameters
    ----------
    value : Any
        Value that may be wrapped in RefValue or an Enum.

    Returns
    -------
    Any
        The unwrapped raw value.
    """
    # Handle RefValue wrapper
    if hasattr(value, "value"):
        value = value.value
    # Handle Enum types
    if isinstance(value, Enum):
        return int(value)
    return value


def _populate_thermal_layer_arrays(
    thermal_layers: Any,
    row_idx: int,
    ndepth: int,
    dz_array: np.ndarray,
    k_array: np.ndarray,
    cp_array: np.ndarray,
) -> None:
    """Populate EHC thermal layer arrays from Pydantic thermal_layers.

    Parameters
    ----------
    thermal_layers : ThermalLayers
        Pydantic thermal layers with dz, k, rho_cp attributes.
    row_idx : int
        Row index in the target arrays (layer index).
    ndepth : int
        Maximum depth levels to copy.
    dz_array : np.ndarray
        Target array for layer thickness [m].
    k_array : np.ndarray
        Target array for thermal conductivity [W m-1 K-1].
    cp_array : np.ndarray
        Target array for volumetric heat capacity [J m-3 K-1].
    """
    dz = _unwrap(thermal_layers.dz) if hasattr(thermal_layers, "dz") else None
    k = _unwrap(thermal_layers.k) if hasattr(thermal_layers, "k") else None
    rho_cp = (
        _unwrap(thermal_layers.rho_cp) if hasattr(thermal_layers, "rho_cp") else None
    )

    if dz is not None:
        for j, val in enumerate(dz[:ndepth]):
            dz_array[row_idx, j] = val
    if k is not None:
        for j, val in enumerate(k[:ndepth]):
            k_array[row_idx, j] = val
    if rho_cp is not None:
        for j, val in enumerate(rho_cp[:ndepth]):
            cp_array[row_idx, j] = val


def _collect_surfaces(container, names: tuple = SURFACE_ORDER):
    """Return surface objects from a container in SUEWS surface order."""
    return [getattr(container, name) for name in names]


def _row_value(row: pd.Series, *keys: str, default: Any = None) -> Any:
    """Fetch first matching key from a Series, otherwise return default."""
    for key in keys:
        if key in row:
            return row[key]
    return default


def _set_attrs(dst, src, attrs: tuple) -> None:
    """Set multiple attributes on dst from src with _unwrap."""
    for name in attrs:
        setattr(dst, name, _unwrap(getattr(src, name)))


def _set_day_profile(dst, prefix: str, profile) -> None:
    """Set working/holiday attributes from a DayProfile."""
    setattr(dst, f"{prefix}_working", _unwrap(profile.working_day))
    setattr(dst, f"{prefix}_holiday", _unwrap(profile.holiday))


def _hourly_dict_to_array(hourly_dict: dict[str, float] | None) -> np.ndarray:
    """Convert hourly profile dict to 24-element numpy array.

    Parameters
    ----------
    hourly_dict : dict[str, float] | None
        Dictionary with hour keys ("1" to "24") and float values.

    Returns
    -------
    np.ndarray
        24-element array with hourly values.
    """
    if not hourly_dict:
        return np.ones(24, dtype=np.float64) / 24.0  # Uniform default
    arr = np.zeros(24, dtype=np.float64)
    for hour_str, val in hourly_dict.items():
        try:
            hour = int(hour_str) - 1  # Convert 1-24 to 0-23
            if 0 <= hour < 24 and val is not None:
                arr[hour] = val
        except (ValueError, TypeError):
            pass
    return arr


def populate_timer_from_datetime(
    timer: dts.SUEWS_TIMER,
    dt: pd.Timestamp,
    tstep_s: int = 3600,
    dt_since_start: int = 0,
    startdls: int = 0,
    enddls: int = 0,
    lat: float | None = None,
) -> None:
    """Populate SUEWS_TIMER from a pandas Timestamp.

    Parameters
    ----------
    timer : dts.SUEWS_TIMER
        Timer object to populate.
    dt : pd.Timestamp
        Datetime for this timestep.
    tstep_s : int
        Timestep in seconds.
    dt_since_start : int
        Seconds since simulation start.
    startdls : int
        Day of year when daylight saving starts (0 = no DLS).
    enddls : int
        Day of year when daylight saving ends (0 = no DLS).

    Notes
    -----
    Temporal conventions match Fortran SUEWS expectations:
    - ``id`` (day of year): 1-indexed, Jan 1 = 1
    - ``dectime``: Decimal day of year, e.g., noon on Jan 1 = 1.5
    - ``dayofweek_id``: 1=Sunday, 7=Saturday (SUEWS/Fortran convention)
    """
    timer.iy = dt.year
    timer.id = dt.dayofyear  # 1-indexed: Jan 1 = 1
    timer.it = dt.hour
    timer.imin = dt.minute
    timer.isec = dt.second
    timer.tstep = tstep_s
    timer.tstep_prev = tstep_s
    timer.tstep_real = float(tstep_s)
    timer.nsh_real = 3600.0 / tstep_s
    timer.nsh = int(3600 / tstep_s)
    timer.dt_since_start = dt_since_start
    timer.dt_since_start_prev = max(0, dt_since_start - tstep_s)

    # Calculate decimal time (day of year + fraction of day)
    # Note: Fortran dectime = (id - 1) + it/24 + imin/(60*24) + isec/(60*60*24)
    timer.dectime = (
        (dt.dayofyear - 1)
        + dt.hour / 24.0
        + dt.minute / (60 * 24)
        + dt.second / (60 * 60 * 24)
    )

    # Day of week array: [weekday, month, season]
    # Fortran convention: 1=Sun, 2=Mon, ..., 7=Sat (not ISO 8601!)
    # Python weekday(): 0=Mon, ..., 6=Sun -> convert: (weekday + 2) % 7 or map explicitly
    # 0=Mon->2, 1=Tue->3, 2=Wed->4, 3=Thu->5, 4=Fri->6, 5=Sat->7, 6=Sun->1
    py_weekday = dt.dayofweek  # 0=Mon, 6=Sun
    fortran_weekday = (py_weekday + 2) % 7  # Convert: Mon->2, Sun->1
    if fortran_weekday == 0:
        fortran_weekday = 7  # Fortran uses 1-7, not 0

    # Determine season based on hemisphere (match Fortran day2month)
    # Season: 1=summer, 2=winter (hemisphere-aware)
    month = dt.month
    lat_val = lat if lat is not None else 1.0
    if lat_val > 0:
        season = 1 if month in [4, 5, 6, 7, 8, 9] else 2
    else:
        season = 1 if month in [10, 11, 12, 1, 2, 3] else 2

    timer.dayofweek_id = np.array([fortran_weekday, month, season], dtype=np.int32)

    # NOTE: Do NOT set timer.new_day here!
    # The new_day flag is managed by suews_cal_main (in suews_phys_ohm.f95):
    # - Set to 1 at LAST timestep of each day (when last_tstep_Q is True)
    # - Reset to 0 at FIRST timestep of next day (after OHM coefficient calculation)
    # If we overwrite it here, we break the daily OHM coefficient recalculation.
    # Initialize to 0 only at simulation start via timer creation default.

    # DLS calculation: DLS = 1 when startdls < id < enddls
    # Matches Fortran SUEWS_cal_DLS in suews_util_time.f95
    doy = dt.dayofyear
    if startdls > 0 and enddls > 0 and doy > startdls and doy < enddls:
        timer.dls = 1
    else:
        timer.dls = 0


def populate_config_from_pydantic(
    config_dts: dts.SUEWS_CONFIG,
    model: Model,
) -> None:
    """Populate SUEWS_CONFIG from Pydantic Model configuration.

    Parameters
    ----------
    config_dts : dts.SUEWS_CONFIG
        Config object to populate.
    model : Model
        Pydantic Model object containing physics settings.
    """
    physics = model.physics

    # Map physics methods and flags
    _set_attrs(
        config_dts,
        physics,
        (
            "netradiationmethod",
            "storageheatmethod",
            "stabilitymethod",
            "emissionsmethod",
            "roughlenmommethod",
            "roughlenheatmethod",
            "smdmethod",
            "waterusemethod",
            "rslmethod",
            "faimethod",
            "rsllevel",
            "ohmincqf",
            "snowuse",
            "stebbsmethod",
            "rcmethod",
        ),
    )

    # Other flags - use defaults if not available on Pydantic model
    # These are Fortran config options not yet exposed in Pydantic
    config_dts.evapmethod = (
        _unwrap(physics.evapmethod) if hasattr(physics, "evapmethod") else 2
    )
    config_dts.laimethod = (
        _unwrap(physics.laimethod) if hasattr(physics, "laimethod") else 1
    )

    # Debug flag - default off
    config_dts.diagnose = 0
    config_dts.flag_test = False


def populate_forcing_from_row(
    forcing_dts: dts.SUEWS_FORCING,
    row: pd.Series,
) -> None:
    """Populate SUEWS_FORCING from a forcing DataFrame row.

    Parameters
    ----------
    forcing_dts : dts.SUEWS_FORCING
        Forcing object to populate.
    row : pd.Series
        Single row from forcing DataFrame.
    """
    # Required forcing variables
    forcing_dts.kdown = _row_value(row, "kdown", default=0.0)
    forcing_dts.temp_c = _row_value(row, "Tair", "temp_c", default=20.0)
    forcing_dts.rh = _row_value(row, "RH", "rh", default=50.0)
    forcing_dts.pres = _row_value(row, "pres", default=1013.25)
    forcing_dts.u = _row_value(row, "U", "u", default=1.0)
    forcing_dts.rain = _row_value(row, "rain", default=0.0)

    # Optional forcing variables
    forcing_dts.ldown = _row_value(row, "ldown", default=-999.0)
    forcing_dts.fcld = _row_value(row, "fcld", default=-999.0)
    forcing_dts.wu_m3 = _row_value(row, "xwu", "wu_m3", default=0.0)
    forcing_dts.qn1_obs = _row_value(row, "qn", "qn1_obs", default=-999.0)
    forcing_dts.qf_obs = _row_value(row, "qf", "qf_obs", default=0.0)
    forcing_dts.qs_obs = _row_value(row, "qs", "qs_obs", default=-999.0)
    forcing_dts.xsmd = _row_value(row, "xsmd", default=-999.0)
    forcing_dts.lai_obs = _row_value(row, "lai_obs", default=-999.0)
    forcing_dts.snowfrac = _row_value(row, "snowfrac", default=-999.0)

    # 5-day average temperature - use current if not available
    forcing_dts.tair_av_5d = _row_value(row, "Tair_av_5d", default=forcing_dts.temp_c)


def _populate_heat_state(
    state_dts: dts.SUEWS_STATE,
    surfaces: list,
    initial_states: "InitialStates",
    nlayer: int,
    ndepth: int,
    nsurf: int,
) -> None:
    """Populate heat state arrays (temperature profiles)."""
    temp_surf = np.zeros((nsurf, ndepth), dtype=np.float64, order="F")
    tsfc_surf = np.zeros(nsurf, dtype=np.float64)

    for i, surf in enumerate(surfaces):
        temps = _unwrap(surf.temperature)
        if isinstance(temps, (list, np.ndarray)):
            temp_surf[i, : len(temps)] = temps[:ndepth]
        else:
            temp_surf[i, :] = temps

        tsfc = _unwrap(surf.tsfc)
        if tsfc is not None:
            tsfc_surf[i] = tsfc

    state_dts.heatstate.temp_surf = temp_surf
    state_dts.heatstate.tsfc_surf = tsfc_surf

    # Roof/wall temperatures if available
    if hasattr(initial_states, "roofs") and initial_states.roofs:
        temp_roof = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        tsfc_roof = np.zeros(nlayer, dtype=np.float64)
        for i, roof in enumerate(initial_states.roofs[:nlayer]):
            temps = _unwrap(roof.temperature)
            if isinstance(temps, (list, np.ndarray)):
                temp_roof[i, : len(temps)] = temps[:ndepth]
            tsfc = _unwrap(roof.tsfc)
            if tsfc is not None:
                tsfc_roof[i] = tsfc
        state_dts.heatstate.temp_roof = temp_roof
        state_dts.heatstate.tsfc_roof = tsfc_roof

    if hasattr(initial_states, "walls") and initial_states.walls:
        temp_wall = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        tsfc_wall = np.zeros(nlayer, dtype=np.float64)
        for i, wall in enumerate(initial_states.walls[:nlayer]):
            temps = _unwrap(wall.temperature)
            if isinstance(temps, (list, np.ndarray)):
                temp_wall[i, : len(temps)] = temps[:ndepth]
            tsfc = _unwrap(wall.tsfc)
            if tsfc is not None:
                tsfc_wall[i] = tsfc
        state_dts.heatstate.temp_wall = temp_wall
        state_dts.heatstate.tsfc_wall = tsfc_wall


def _populate_hydro_state(
    state_dts: dts.SUEWS_STATE,
    surfaces: list,
    nsurf: int,
) -> None:
    """Populate hydro state arrays (soil moisture and surface wetness)."""
    soilstore_surf = np.zeros(nsurf, dtype=np.float64)
    state_surf = np.zeros(nsurf, dtype=np.float64)

    for i, surf in enumerate(surfaces):
        soilstore_surf[i] = _unwrap(surf.soilstore)
        state_surf[i] = _unwrap(surf.state)

    state_dts.hydrostate.soilstore_surf = soilstore_surf
    state_dts.hydrostate.state_surf = state_surf


def _populate_snow_state(
    state_dts: dts.SUEWS_STATE,
    surfaces: list,
    initial_states: "InitialStates",
    nsurf: int,
) -> None:
    """Populate snow state arrays."""
    snowpack = np.zeros(nsurf, dtype=np.float64)
    snowfrac = np.zeros(nsurf, dtype=np.float64)
    snowdens = np.zeros(nsurf, dtype=np.float64)
    icefrac = np.zeros(nsurf, dtype=np.float64)
    snowwater = np.zeros(nsurf, dtype=np.float64)

    for i, surf in enumerate(surfaces):
        sp = _unwrap(surf.snowpack)
        if sp is not None:
            snowpack[i] = sp
        sf = _unwrap(surf.snowfrac)
        if sf is not None:
            snowfrac[i] = sf
        sd = _unwrap(surf.snowdens)
        if sd is not None:
            snowdens[i] = sd
        ic = _unwrap(surf.icefrac)
        if ic is not None:
            icefrac[i] = ic
        sw = _unwrap(surf.snowwater)
        if sw is not None:
            snowwater[i] = sw

    state_dts.snowstate.snowpack = snowpack
    state_dts.snowstate.snowfrac = snowfrac
    state_dts.snowstate.snowdens = snowdens
    state_dts.snowstate.icefrac = icefrac
    state_dts.snowstate.snowwater = snowwater
    state_dts.snowstate.snowalb = _unwrap(initial_states.snowalb)


def _populate_phenology_state(
    state_dts: dts.SUEWS_STATE,
    initial_states: "InitialStates",
    surfaces: list,
    land_cover: "LandCover | None",
    nsurf: int,
) -> None:
    """Populate phenology state (LAI, GDD, albedo, temperature extremes)."""
    # Vegetation LAI/GDD/SDD
    veg_surfaces = _collect_surfaces(initial_states, VEG_SURFACE_ORDER)

    lai_id = np.zeros(3, dtype=np.float64)
    gdd_id = np.zeros(3, dtype=np.float64)
    sdd_id = np.zeros(3, dtype=np.float64)

    for i, veg in enumerate(veg_surfaces):
        if hasattr(veg, "lai_id"):
            lai_id[i] = _unwrap(veg.lai_id)
        if hasattr(veg, "gdd_id"):
            gdd_id[i] = _unwrap(veg.gdd_id)
        if hasattr(veg, "sdd_id"):
            sdd_id[i] = _unwrap(veg.sdd_id)

    state_dts.phenstate.lai_id = lai_id
    state_dts.phenstate.gdd_id = gdd_id
    state_dts.phenstate.sdd_id = sdd_id

    # Deciduous tree specific: porosity_id and decidcap_id
    # Reference: suews_ctrl_driver.f95 lines 5025-5026
    if hasattr(initial_states.dectr, "porosity_id"):
        state_dts.phenstate.porosity_id = _unwrap(initial_states.dectr.porosity_id)
    if hasattr(initial_states.dectr, "decidcap_id"):
        state_dts.phenstate.decidcap_id = _unwrap(initial_states.dectr.decidcap_id)

    # Individual vegetation albedo IDs
    # Reference: suews_ctrl_driver.f95 lines 5027-5029
    if hasattr(initial_states.dectr, "alb_id"):
        state_dts.phenstate.albdectr_id = _unwrap(initial_states.dectr.alb_id)
    if hasattr(initial_states.evetr, "alb_id"):
        state_dts.phenstate.albevetr_id = _unwrap(initial_states.evetr.alb_id)
    if hasattr(initial_states.grass, "alb_id"):
        state_dts.phenstate.albgrass_id = _unwrap(initial_states.grass.alb_id)

    # Temperature extremes tracking
    # Reference: suews_ctrl_driver.f95 lines 5030-5032, _load.py lines 1870-1872
    # For continuation runs, preserve actual values; for fresh runs, use sentinel values
    if hasattr(initial_states, "tmin_id"):
        tmin_val = _unwrap(initial_states.tmin_id)
        # Use extracted value if it's a real temperature (not sentinel 90.0)
        if tmin_val is not None and tmin_val != 90.0:
            state_dts.phenstate.tmin_id = tmin_val
        else:
            state_dts.phenstate.tmin_id = (
                90.0  # Sentinel: will be updated to actual min
            )
    else:
        state_dts.phenstate.tmin_id = 90.0

    if hasattr(initial_states, "tmax_id"):
        tmax_val = _unwrap(initial_states.tmax_id)
        # Use extracted value if it's a real temperature (not sentinel -90.0)
        if tmax_val is not None and tmax_val != -90.0:
            state_dts.phenstate.tmax_id = tmax_val
        else:
            state_dts.phenstate.tmax_id = (
                -90.0
            )  # Sentinel: will be updated to actual max
    else:
        state_dts.phenstate.tmax_id = -90.0

    if hasattr(initial_states, "lenday_id"):
        lenday_val = _unwrap(initial_states.lenday_id)
        if lenday_val is not None:
            state_dts.phenstate.lenday_id = float(lenday_val)
        else:
            state_dts.phenstate.lenday_id = 0.0
    else:
        state_dts.phenstate.lenday_id = 0.0

    # Albedo per surface (7 surfaces)
    # For vegetation (evetr, dectr, grass), use alb_id from initial_states
    # For non-vegetation (paved, bldgs, bsoil, water), alb comes from land_cover
    alb_surf = np.zeros(nsurf, dtype=np.float64)

    lc_surfaces = None
    if land_cover is not None:
        lc_surfaces = _collect_surfaces(land_cover)

    for i, surf in enumerate(surfaces):
        if hasattr(surf, "alb_id"):
            alb_surf[i] = _unwrap(surf.alb_id)
        elif hasattr(surf, "alb"):
            alb_surf[i] = _unwrap(surf.alb)
        elif lc_surfaces is not None and hasattr(lc_surfaces[i], "alb"):
            alb_surf[i] = _unwrap(lc_surfaces[i].alb)

    state_dts.phenstate.alb = alb_surf


def _populate_anthro_and_ohm_state(
    state_dts: dts.SUEWS_STATE,
    initial_states: "InitialStates",
) -> None:
    """Populate anthropogenic emissions and OHM state."""
    # Anthropogenic emissions state: HDD_id (Heating Degree Days)
    # Critical for QF calculations - contains temperature history
    if hasattr(initial_states, "hdd_id") and initial_states.hdd_id is not None:
        hdd = initial_states.hdd_id
        # HDD_id is a 12-element array with both current day accumulators (0-5)
        # and previous day values (6-11) used in QF calculations
        if hasattr(hdd, "to_list"):
            hdd_list = hdd.to_list()
        elif isinstance(hdd, (list, tuple, np.ndarray)):
            hdd_list = list(hdd)
        else:
            hdd_list = [0.0] * 12  # Default fallback
        state_dts.anthroemisstate.hdd_id = np.array(hdd_list, dtype=np.float64)

    # OHM state: running averages for storage heat flux calculations
    # Critical for continuation runs to maintain accurate QS partitioning
    if hasattr(initial_states, "qn_av"):
        qn_av_val = _unwrap(initial_states.qn_av)
        if qn_av_val is not None and qn_av_val != 0:
            state_dts.ohmstate.qn_av = qn_av_val
    if hasattr(initial_states, "qn_s_av"):
        qn_s_av_val = _unwrap(initial_states.qn_s_av)
        if qn_s_av_val is not None:
            state_dts.ohmstate.qn_s_av = qn_s_av_val
    if hasattr(initial_states, "dqndt"):
        dqndt_val = _unwrap(initial_states.dqndt)
        if dqndt_val is not None:
            state_dts.ohmstate.dqndt = dqndt_val
    if hasattr(initial_states, "dqnsdt"):
        dqnsdt_val = _unwrap(initial_states.dqnsdt)
        if dqnsdt_val is not None:
            state_dts.ohmstate.dqnsdt = dqnsdt_val


def populate_state_from_pydantic(
    state_dts: dts.SUEWS_STATE,
    initial_states: InitialStates,
    nlayer: int = 5,
    ndepth: int = 5,
    nsurf: int = 7,
    land_cover: LandCover | None = None,
) -> None:
    """Populate SUEWS_STATE from Pydantic InitialStates.

    Parameters
    ----------
    state_dts : dts.SUEWS_STATE
        State object to populate (must be pre-allocated).
    initial_states : InitialStates
        Pydantic InitialStates object.
    nlayer : int
        Number of vertical layers.
    ndepth : int
        Number of depth levels.
    nsurf : int
        Number of surface types (always 7).
    land_cover : LandCover, optional
        Land cover object for getting albedo values for non-vegetation surfaces.
    """
    surfaces = _collect_surfaces(initial_states)

    _populate_heat_state(state_dts, surfaces, initial_states, nlayer, ndepth, nsurf)
    _populate_hydro_state(state_dts, surfaces, nsurf)
    _populate_snow_state(state_dts, surfaces, initial_states, nsurf)
    _populate_phenology_state(state_dts, initial_states, surfaces, land_cover, nsurf)
    _populate_anthro_and_ohm_state(state_dts, initial_states)


def populate_storedrainprm(
    state_dts: dts.SUEWS_STATE,
    land_cover: LandCover,
    nsurf: int = 7,
) -> None:
    """Populate storedrainprm array in phenstate from land cover config.

    The storedrainprm array is (6, 7) where:
    - Row 0: store_min (statelimit)
    - Row 1: drain_eq
    - Row 2: drain_coef_1
    - Row 3: drain_coef_2
    - Row 4: store_cap
    - Row 5: store_max

    Parameters
    ----------
    state_dts : SUEWS_STATE
        State object (must be pre-allocated).
    land_cover : LandCover
        Pydantic LandCover object with storedrainprm per surface.
    nsurf : int
        Number of surfaces (always 7).
    """
    surfaces = _collect_surfaces(land_cover)

    storedrainprm = np.full((6, nsurf), -999.0, dtype=np.float64)

    for j, surf in enumerate(surfaces):
        # Row 0: store_min (use statelimit as fallback)
        if hasattr(surf, "storedrainprm") and surf.storedrainprm is not None:
            sdp = surf.storedrainprm
            if hasattr(sdp, "store_min"):
                storedrainprm[0, j] = _unwrap(sdp.store_min)
            if hasattr(sdp, "drain_eq"):
                storedrainprm[1, j] = _unwrap(sdp.drain_eq)
            if hasattr(sdp, "drain_coef_1"):
                val = _unwrap(sdp.drain_coef_1)
                if val is not None:
                    storedrainprm[2, j] = val
            if hasattr(sdp, "drain_coef_2"):
                val = _unwrap(sdp.drain_coef_2)
                if val is not None:
                    storedrainprm[3, j] = val
            if hasattr(sdp, "store_cap"):
                val = _unwrap(sdp.store_cap)
                if val is not None:
                    storedrainprm[4, j] = val
            if hasattr(sdp, "store_max"):
                val = _unwrap(sdp.store_max)
                if val is not None:
                    storedrainprm[5, j] = val

        # Fallback to statelimit for store_min if not set
        if storedrainprm[0, j] == -999.0 and hasattr(surf, "statelimit"):
            storedrainprm[0, j] = _unwrap(surf.statelimit)

    state_dts.phenstate.storedrainprm = storedrainprm


def populate_site_from_pydantic(
    site_dts: dts.SUEWS_SITE,
    site: Site,
    model: Model,
) -> None:
    """Populate SUEWS_SITE from Pydantic Site configuration.

    Parameters
    ----------
    site_dts : dts.SUEWS_SITE
        Site object to populate (must be pre-allocated).
    site : Site
        Pydantic Site object.
    model : Model
        Pydantic Model object for control settings.
    """
    props = site.properties

    # Basic site parameters
    site_dts.lat = _unwrap(props.lat)
    site_dts.lon = _unwrap(props.lng)
    site_dts.alt = _unwrap(props.alt)
    # Handle timezone - may be TimezoneOffset enum or float
    tz_val = _unwrap(props.timezone)
    if hasattr(tz_val, "value"):
        tz_val = tz_val.value
    site_dts.timezone = float(tz_val) if isinstance(tz_val, (int, float)) else 0.0
    site_dts.surfacearea = _unwrap(props.surfacearea)
    site_dts.z = _unwrap(props.z)
    site_dts.z0m_in = _unwrap(props.z0m_in)
    site_dts.zdm_in = _unwrap(props.zdm_in)
    site_dts.pipecapacity = _unwrap(props.pipecapacity)
    site_dts.runofftowater = _unwrap(props.runofftowater)
    site_dts.narp_trans_site = _unwrap(props.narp_trans_site)
    # co2pointsource and flowchange don't exist on Pydantic model - use defaults
    site_dts.co2pointsource = (
        _unwrap(props.co2pointsource) if hasattr(props, "co2pointsource") else 0.0
    )
    site_dts.flowchange = (
        _unwrap(props.flowchange) if hasattr(props, "flowchange") else 0.0
    )

    # Surface fractions - build array from land_cover (on properties)
    # Pydantic uses 'sfr' (surface fraction), not 'fraction'
    lc = props.land_cover
    sfr_surf = np.array(
        [_unwrap(surf.sfr) for surf in _collect_surfaces(lc)],
        dtype=np.float64,
    )
    site_dts.sfr_surf = sfr_surf

    # Derived fractions (will be recalculated by cal_surf)
    site_dts.vegfraction = sfr_surf[2] + sfr_surf[3] + sfr_surf[4]  # evetr+dectr+grass
    site_dts.impervfraction = sfr_surf[0] + sfr_surf[1]  # paved+bldgs
    site_dts.pervfraction = 1.0 - site_dts.impervfraction - sfr_surf[6]
    site_dts.nonwaterfraction = 1.0 - sfr_surf[6]

    # Building parameters - directly on props, not nested
    site_dts.n_buildings = (
        _unwrap(props.n_buildings) if hasattr(props, "n_buildings") else 0.0
    )
    site_dts.h_std = _unwrap(props.h_std) if hasattr(props, "h_std") else 0.0
    site_dts.lambda_c = _unwrap(props.lambda_c) if hasattr(props, "lambda_c") else 0.0

    # Populate nested land cover types
    _populate_landcover_paved(site_dts.lc_paved, lc.paved)
    _populate_landcover_bldg(site_dts.lc_bldg, lc.bldgs)
    _populate_landcover_evetr(site_dts.lc_evetr, lc.evetr)
    _populate_landcover_dectr(site_dts.lc_dectr, lc.dectr)
    _populate_landcover_grass(site_dts.lc_grass, lc.grass)
    _populate_landcover_bsoil(site_dts.lc_bsoil, lc.bsoil)
    _populate_landcover_water(site_dts.lc_water, lc.water)

    # Populate critical nested parameter objects
    gsmodel = (
        _unwrap(model.physics.gsmodel)
        if hasattr(model, "physics") and hasattr(model.physics, "gsmodel")
        else None
    )
    _populate_conductance(site_dts.conductance, props.conductance, gsmodel=gsmodel)
    _populate_lumps(site_dts.lumps, props.lumps)
    _populate_snow(site_dts.snow, props.snow, lc)

    # Populate EHC (Element Heat Capacity) parameters from thermal_layers
    _populate_ehc(site_dts.ehc, lc, site.initial_states)

    # Populate vertical layer parameters (roofs, walls, SPARTACUS)
    if hasattr(props, "vertical_layers"):
        _populate_vertical_layers(site_dts, props.vertical_layers, site.initial_states)

    # Populate anthropogenic emissions parameters
    if hasattr(props, "anthropogenic_emissions") and props.anthropogenic_emissions:
        _populate_anthroemis(site_dts.anthroemis, props.anthropogenic_emissions)


def _populate_landcover_common(
    lc_dts: Any, lc_pydantic: Any, is_water: bool = False
) -> None:
    """Populate common parameters for all land cover types.

    This includes surface state limits, soil parameters, OHM parameters,
    water distribution, and storage/drain parameters.

    Parameters
    ----------
    lc_dts : LC_*_PRM
        Land cover DTS object to populate.
    lc_pydantic : *Properties
        Pydantic land cover properties.
    is_water : bool
        If True, skip waterdist as water surface doesn't have one.
    """
    # Surface state parameters
    if hasattr(lc_pydantic, "statelimit"):
        lc_dts.statelimit = _unwrap(lc_pydantic.statelimit)
    if hasattr(lc_pydantic, "wetthresh"):
        lc_dts.wetthresh = _unwrap(lc_pydantic.wetthresh)

    # Soil parameters
    if hasattr(lc_pydantic, "soildepth"):
        val = _unwrap(lc_pydantic.soildepth)
        if val is not None:
            lc_dts.soil.soildepth = val
    if hasattr(lc_pydantic, "soilstorecap"):
        val = _unwrap(lc_pydantic.soilstorecap)
        if val is not None:
            lc_dts.soil.soilstorecap = val
    if hasattr(lc_pydantic, "sathydraulicconduct"):
        val = _unwrap(lc_pydantic.sathydraulicconduct)
        if val is not None:
            lc_dts.soil.sathydraulicconduct = val

    # OHM thresholds
    if hasattr(lc_pydantic, "ohm_threshsw"):
        lc_dts.ohm.ohm_threshsw = _unwrap(lc_pydantic.ohm_threshsw)
    if hasattr(lc_pydantic, "ohm_threshwd"):
        lc_dts.ohm.ohm_threshwd = _unwrap(lc_pydantic.ohm_threshwd)

    # AnOHM parameters (in OHM object)
    if hasattr(lc_pydantic, "ch_anohm"):
        lc_dts.ohm.chanohm = _unwrap(lc_pydantic.ch_anohm)
    if hasattr(lc_pydantic, "rho_cp_anohm"):
        lc_dts.ohm.cpanohm = _unwrap(lc_pydantic.rho_cp_anohm)
    if hasattr(lc_pydantic, "k_anohm"):
        lc_dts.ohm.kkanohm = _unwrap(lc_pydantic.k_anohm)

    # OHM coefficients - nested structure
    # Pydantic: ohm_coef.summer_dry.a1, a2, a3
    # Fortran: ohm_coef_lc[0].summer_dry (a1), ohm_coef_lc[1].summer_dry (a2), etc.
    if hasattr(lc_pydantic, "ohm_coef") and lc_pydantic.ohm_coef is not None:
        ohm_coef = lc_pydantic.ohm_coef
        # a1 coefficients
        lc_dts.ohm.ohm_coef_lc[0].summer_dry = _unwrap(ohm_coef.summer_dry.a1)
        lc_dts.ohm.ohm_coef_lc[0].summer_wet = _unwrap(ohm_coef.summer_wet.a1)
        lc_dts.ohm.ohm_coef_lc[0].winter_dry = _unwrap(ohm_coef.winter_dry.a1)
        lc_dts.ohm.ohm_coef_lc[0].winter_wet = _unwrap(ohm_coef.winter_wet.a1)
        # a2 coefficients
        lc_dts.ohm.ohm_coef_lc[1].summer_dry = _unwrap(ohm_coef.summer_dry.a2)
        lc_dts.ohm.ohm_coef_lc[1].summer_wet = _unwrap(ohm_coef.summer_wet.a2)
        lc_dts.ohm.ohm_coef_lc[1].winter_dry = _unwrap(ohm_coef.winter_dry.a2)
        lc_dts.ohm.ohm_coef_lc[1].winter_wet = _unwrap(ohm_coef.winter_wet.a2)
        # a3 coefficients
        lc_dts.ohm.ohm_coef_lc[2].summer_dry = _unwrap(ohm_coef.summer_dry.a3)
        lc_dts.ohm.ohm_coef_lc[2].summer_wet = _unwrap(ohm_coef.summer_wet.a3)
        lc_dts.ohm.ohm_coef_lc[2].winter_dry = _unwrap(ohm_coef.winter_dry.a3)
        lc_dts.ohm.ohm_coef_lc[2].winter_wet = _unwrap(ohm_coef.winter_wet.a3)

    # Water distribution (not for water surface)
    if (
        not is_water
        and hasattr(lc_pydantic, "waterdist")
        and lc_pydantic.waterdist is not None
    ):
        wd = lc_pydantic.waterdist
        wd_dts = lc_dts.waterdist
        wd_dts.to_paved = _unwrap(wd.to_paved) if wd.to_paved is not None else 0.0
        wd_dts.to_bldg = (
            _unwrap(wd.to_bldgs)
            if hasattr(wd, "to_bldgs") and wd.to_bldgs is not None
            else 0.0
        )
        wd_dts.to_evetr = _unwrap(wd.to_evetr) if wd.to_evetr is not None else 0.0
        wd_dts.to_dectr = _unwrap(wd.to_dectr) if wd.to_dectr is not None else 0.0
        wd_dts.to_grass = _unwrap(wd.to_grass) if wd.to_grass is not None else 0.0
        wd_dts.to_bsoil = _unwrap(wd.to_bsoil) if wd.to_bsoil is not None else 0.0
        wd_dts.to_water = _unwrap(wd.to_water) if wd.to_water is not None else 0.0
        # to_soilstore is the last column
        if hasattr(wd, "to_soilstore") and wd.to_soilstore is not None:
            wd_dts.to_soilstore = _unwrap(wd.to_soilstore)
        elif hasattr(wd, "to_runoff") and wd.to_runoff is not None:
            wd_dts.to_soilstore = _unwrap(wd.to_runoff)

    # Note: storedrainprm is per-surface in old interface but
    # surf_store is at site level in DTS. Handled separately.


def _populate_landcover_paved(lc_dts, lc_pydantic) -> None:
    """Populate LC_PAVED_PRM from Pydantic."""
    lc_dts.sfr = _unwrap(lc_pydantic.sfr)
    lc_dts.emis = _unwrap(lc_pydantic.emis)
    # irrfrac for paved
    if hasattr(lc_pydantic, "irrfrac"):
        lc_dts.irrfracpaved = _unwrap(lc_pydantic.irrfrac)
    # Common parameters
    _populate_landcover_common(lc_dts, lc_pydantic)


def _populate_landcover_bldg(lc_dts, lc_pydantic) -> None:
    """Populate LC_BLDG_PRM from Pydantic."""
    lc_dts.sfr = _unwrap(lc_pydantic.sfr)
    lc_dts.emis = _unwrap(lc_pydantic.emis)
    if hasattr(lc_pydantic, "faibldg"):
        lc_dts.faibldg = _unwrap(lc_pydantic.faibldg)
    if hasattr(lc_pydantic, "bldgh"):
        lc_dts.bldgh = _unwrap(lc_pydantic.bldgh)
    # Common parameters
    _populate_landcover_common(lc_dts, lc_pydantic)


def _populate_vegetation_params(lc_dts, lc_pydantic) -> None:
    """Populate vegetation-specific parameters (LAI, bioCO2, etc.).

    This is called for evetr, dectr, and grass land cover types.
    """
    # LAI parameters
    if hasattr(lc_pydantic, "lai") and lc_pydantic.lai is not None:
        lai_py = lc_pydantic.lai
        lai_dts = lc_dts.lai
        lai_dts.laimin = _unwrap(lai_py.laimin)
        lai_dts.laimax = _unwrap(lai_py.laimax)
        lai_dts.gddfull = _unwrap(lai_py.gddfull)
        lai_dts.sddfull = _unwrap(lai_py.sddfull)
        lai_dts.baset = _unwrap(lai_py.baset)
        lai_dts.basete = _unwrap(lai_py.basete)
        lai_dts.laitype = _unwrap(lai_py.laitype)
        # LAI power coefficients (array of 4)
        if hasattr(lai_py, "laipower") and lai_py.laipower is not None:
            lp = lai_py.laipower
            lai_dts.laipower[0] = _unwrap(lp.growth_lai)
            lai_dts.laipower[1] = _unwrap(lp.growth_gdd)
            lai_dts.laipower[2] = _unwrap(lp.senescence_lai)
            lai_dts.laipower[3] = _unwrap(lp.senescence_sdd)

    # Albedo min/max
    if hasattr(lc_pydantic, "alb_min"):
        lc_dts.alb_min = _unwrap(lc_pydantic.alb_min)
    if hasattr(lc_pydantic, "alb_max"):
        lc_dts.alb_max = _unwrap(lc_pydantic.alb_max)

    # Max conductance
    if hasattr(lc_pydantic, "maxconductance"):
        lc_dts.maxconductance = _unwrap(lc_pydantic.maxconductance)


def _populate_landcover_evetr(lc_dts, lc_pydantic) -> None:
    """Populate LC_EVETR_PRM from Pydantic."""
    lc_dts.sfr = _unwrap(lc_pydantic.sfr)
    lc_dts.emis = _unwrap(lc_pydantic.emis)
    if hasattr(lc_pydantic, "faievetree"):
        lc_dts.faievetree = _unwrap(lc_pydantic.faievetree)
    if hasattr(lc_pydantic, "evetreeh"):
        lc_dts.evetreeh = _unwrap(lc_pydantic.evetreeh)
    # Common parameters
    _populate_landcover_common(lc_dts, lc_pydantic)
    # Vegetation-specific parameters
    _populate_vegetation_params(lc_dts, lc_pydantic)


def _populate_landcover_dectr(lc_dts, lc_pydantic) -> None:
    """Populate LC_DECTR_PRM from Pydantic."""
    lc_dts.sfr = _unwrap(lc_pydantic.sfr)
    lc_dts.emis = _unwrap(lc_pydantic.emis)
    if hasattr(lc_pydantic, "faidectree"):
        lc_dts.faidectree = _unwrap(lc_pydantic.faidectree)
    if hasattr(lc_pydantic, "dectreeh"):
        lc_dts.dectreeh = _unwrap(lc_pydantic.dectreeh)
    # Deciduous-specific porosity parameters (critical for phenology)
    if hasattr(lc_pydantic, "pormin_dec"):
        lc_dts.pormin_dec = _unwrap(lc_pydantic.pormin_dec)
    if hasattr(lc_pydantic, "pormax_dec"):
        lc_dts.pormax_dec = _unwrap(lc_pydantic.pormax_dec)
    # Deciduous-specific capacity parameters
    if hasattr(lc_pydantic, "capmin_dec"):
        lc_dts.capmin_dec = _unwrap(lc_pydantic.capmin_dec)
    if hasattr(lc_pydantic, "capmax_dec"):
        lc_dts.capmax_dec = _unwrap(lc_pydantic.capmax_dec)
    # Common parameters
    _populate_landcover_common(lc_dts, lc_pydantic)
    # Vegetation-specific parameters
    _populate_vegetation_params(lc_dts, lc_pydantic)


def _populate_landcover_grass(lc_dts, lc_pydantic) -> None:
    """Populate LC_GRASS_PRM from Pydantic."""
    lc_dts.sfr = _unwrap(lc_pydantic.sfr)
    lc_dts.emis = _unwrap(lc_pydantic.emis)
    # Common parameters
    _populate_landcover_common(lc_dts, lc_pydantic)
    # Vegetation-specific parameters
    _populate_vegetation_params(lc_dts, lc_pydantic)


def _populate_landcover_bsoil(lc_dts, lc_pydantic) -> None:
    """Populate LC_BSOIL_PRM from Pydantic."""
    lc_dts.sfr = _unwrap(lc_pydantic.sfr)
    lc_dts.emis = _unwrap(lc_pydantic.emis)
    # Common parameters
    _populate_landcover_common(lc_dts, lc_pydantic)


def _populate_landcover_water(lc_dts, lc_pydantic) -> None:
    """Populate LC_WATER_PRM from Pydantic."""
    lc_dts.sfr = _unwrap(lc_pydantic.sfr)
    lc_dts.emis = _unwrap(lc_pydantic.emis)
    # Common parameters (is_water=True skips waterdist)
    _populate_landcover_common(lc_dts, lc_pydantic, is_water=True)


def _populate_conductance(cond_dts, cond_pydantic, gsmodel: int | None = None) -> None:
    """Populate CONDUCTANCE_PRM from Pydantic."""
    _set_attrs(
        cond_dts,
        cond_pydantic,
        (
            "g_max",
            "g_k",
            "g_q_base",
            "g_q_shape",
            "g_t",
            "g_sm",
            "kmax",
            "s1",
            "s2",
            "th",
            "tl",
        ),
    )
    # gsmodel comes from config; fall back to site or default
    if gsmodel is not None:
        cond_dts.gsmodel = int(gsmodel)
    elif hasattr(cond_pydantic, "gsmodel"):
        cond_dts.gsmodel = _unwrap(cond_pydantic.gsmodel)
    else:
        cond_dts.gsmodel = 2  # Default to Ward et al. 2016


def _populate_lumps(lumps_dts, lumps_pydantic) -> None:
    """Populate LUMPS_PRM from Pydantic."""
    _set_attrs(
        lumps_dts,
        lumps_pydantic,
        ("drainrt", "raincover", "rainmaxres", "veg_type"),
    )


def _populate_snow(snow_dts, snow_pydantic, land_cover=None) -> None:
    """Populate SNOW_PRM from Pydantic.

    Parameters
    ----------
    snow_dts : SNOW_PRM
        Snow parameter DTS object to populate.
    snow_pydantic : SnowParams
        Pydantic snow parameters.
    land_cover : LandCover, optional
        Land cover for snowpacklimit values.
    """
    snow_dts.crwmax = _unwrap(snow_pydantic.crwmax)
    snow_dts.crwmin = _unwrap(snow_pydantic.crwmin)
    snow_dts.narp_emis_snow = _unwrap(snow_pydantic.narp_emis_snow)
    snow_dts.preciplimit = _unwrap(snow_pydantic.preciplimit)
    snow_dts.preciplimitalb = _unwrap(snow_pydantic.preciplimitalb)
    snow_dts.radmeltfact = _unwrap(snow_pydantic.radmeltfact)
    snow_dts.snowalbmax = _unwrap(snow_pydantic.snowalbmax)
    snow_dts.snowalbmin = _unwrap(snow_pydantic.snowalbmin)
    snow_dts.snowdensmax = _unwrap(snow_pydantic.snowdensmax)
    snow_dts.snowdensmin = _unwrap(snow_pydantic.snowdensmin)
    snow_dts.tau_a = _unwrap(snow_pydantic.tau_a)
    snow_dts.tau_f = _unwrap(snow_pydantic.tau_f)
    snow_dts.tau_r = _unwrap(snow_pydantic.tau_r)
    snow_dts.tempmeltfact = _unwrap(snow_pydantic.tempmeltfact)
    # Optional snow limits
    if hasattr(snow_pydantic, "snowlimbldg"):
        snow_dts.snowlimbldg = _unwrap(snow_pydantic.snowlimbldg)
    if hasattr(snow_pydantic, "snowlimpaved"):
        snow_dts.snowlimpaved = _unwrap(snow_pydantic.snowlimpaved)

    # Snow profiles (24-hour diurnal patterns)
    # Fortran expects (24,) arrays for working and holiday
    if (
        hasattr(snow_pydantic, "snowprof_24hr")
        and snow_pydantic.snowprof_24hr is not None
    ):
        profile = snow_pydantic.snowprof_24hr
        working = np.full(24, -999.0, dtype=np.float64)
        holiday = np.full(24, -999.0, dtype=np.float64)

        if hasattr(profile, "working_day") and profile.working_day is not None:
            for hour_str, val in profile.working_day.items():
                hour = int(hour_str) - 1  # Convert 1-24 to 0-23
                if 0 <= hour < 24 and val is not None:
                    working[hour] = val

        if hasattr(profile, "holiday") and profile.holiday is not None:
            for hour_str, val in profile.holiday.items():
                hour = int(hour_str) - 1  # Convert 1-24 to 0-23
                if 0 <= hour < 24 and val is not None:
                    holiday[hour] = val

        snow_dts.snowprof_24hr_working = working
        snow_dts.snowprof_24hr_holiday = holiday

    # Snowpacklimit per surface (7 surfaces)
    if land_cover is not None:
        surfaces = _collect_surfaces(land_cover)
        snowpacklimit = np.full(7, -999.0, dtype=np.float64)
        for i, surf in enumerate(surfaces):
            if hasattr(surf, "snowpacklimit"):
                val = _unwrap(surf.snowpacklimit)
                if val is not None:
                    snowpacklimit[i] = val
        snow_dts.snowpacklimit = snowpacklimit


def _populate_ehc(
    ehc_dts, land_cover, initial_states, nsurf: int = 7, ndepth: int = 5
) -> None:
    """Populate EHC_PRM from Pydantic land cover thermal_layers.

    Parameters
    ----------
    ehc_dts : EHC_PRM
        EHC parameter object to populate (must be pre-allocated).
    land_cover : LandCover
        Pydantic LandCover object containing thermal_layers for each surface.
    initial_states : InitialStates
        Pydantic InitialStates object for temperature values.
    nsurf : int
        Number of surface types (always 7).
    ndepth : int
        Number of depth levels.
    """
    # Get surface configs in order (matching SUEWS surface indexing)
    surfaces = _collect_surfaces(land_cover)

    # Get initial states for temperature
    state_surfaces = _collect_surfaces(initial_states)

    # Populate arrays for each surface type
    for i, (lc, state) in enumerate(zip(surfaces, state_surfaces)):
        # Get thermal_layers if available
        if hasattr(lc, "thermal_layers") and lc.thermal_layers is not None:
            tl = lc.thermal_layers

            # Layer thicknesses (dz)
            dz = _unwrap(tl.dz) if hasattr(tl, "dz") else None
            if dz is not None:
                for j, val in enumerate(dz[:ndepth]):
                    ehc_dts.dz_surf[i, j] = val

            # Thermal conductivity (k)
            k = _unwrap(tl.k) if hasattr(tl, "k") else None
            if k is not None:
                for j, val in enumerate(k[:ndepth]):
                    ehc_dts.k_surf[i, j] = val

            # Heat capacity (rho_cp -> cp)
            rho_cp = _unwrap(tl.rho_cp) if hasattr(tl, "rho_cp") else None
            if rho_cp is not None:
                for j, val in enumerate(rho_cp[:ndepth]):
                    ehc_dts.cp_surf[i, j] = val

        # Internal temperature from initial states
        tin = _unwrap(state.temperature) if hasattr(state, "temperature") else 2.0
        if isinstance(tin, (list, np.ndarray)):
            ehc_dts.tin_surf[i] = tin[0] if len(tin) > 0 else 2.0
        else:
            ehc_dts.tin_surf[i] = tin


def _populate_vertical_layers(
    site_dts, vertical_layers, initial_states, ndepth: int = 5
) -> None:
    """Populate vertical layer parameters (roofs, walls, SPARTACUS).

    Parameters
    ----------
    site_dts : SUEWS_SITE
        Site DTS object to populate.
    vertical_layers : VerticalLayers
        Pydantic vertical layers configuration.
    initial_states : InitialStates
        Initial states for temperature values.
    ndepth : int
        Number of depth levels for thermal arrays.
    """
    nlayer = _unwrap(vertical_layers.nlayer)
    site_dts.nlayer = nlayer

    # Populate SPARTACUS layer parameters
    sl = site_dts.spartacus_layer

    # Height array (nlayer+1 values: ground + layer heights)
    # Note: height is in spartacus, not spartacus_layer
    if hasattr(vertical_layers, "height") and vertical_layers.height is not None:
        height = _unwrap(vertical_layers.height)
        site_dts.spartacus.height = np.array(height, dtype=np.float64)

    building_frac = _unwrap(vertical_layers.building_frac)
    veg_frac = _unwrap(vertical_layers.veg_frac)
    building_scale = _unwrap(vertical_layers.building_scale)
    veg_scale = _unwrap(vertical_layers.veg_scale)

    for i in range(min(nlayer, len(building_frac))):
        sl.building_frac[i] = building_frac[i]
        sl.veg_frac[i] = veg_frac[i]
        sl.building_scale[i] = building_scale[i]
        sl.veg_scale[i] = veg_scale[i]

    # Populate roof layers
    roofs = vertical_layers.roofs
    for i, roof in enumerate(roofs[:nlayer]):
        # Albedo and emissivity
        sl.alb_roof[i] = _unwrap(roof.alb)
        sl.emis_roof[i] = _unwrap(roof.emis)

        # Roof albedo multiplier and wall specular fraction
        if hasattr(roof, "roof_albedo_dir_mult_fact"):
            sl.roof_albedo_dir_mult_fact[0, i] = _unwrap(roof.roof_albedo_dir_mult_fact)
        if hasattr(roof, "wall_specular_frac"):
            sl.wall_specular_frac[0, i] = _unwrap(roof.wall_specular_frac)

    # Populate wall layers
    walls = vertical_layers.walls
    for i, wall in enumerate(walls[:nlayer]):
        sl.alb_wall[i] = _unwrap(wall.alb)
        sl.emis_wall[i] = _unwrap(wall.emis)

    # Populate EHC roof/wall arrays from thermal_layers
    # Note: EHC arrays are allocated as (7, ndepth) but we only populate nlayer rows
    ehc = site_dts.ehc
    for i, roof in enumerate(roofs[:nlayer]):
        if hasattr(roof, "thermal_layers") and roof.thermal_layers is not None:
            _populate_thermal_layer_arrays(
                roof.thermal_layers, i, ndepth, ehc.dz_roof, ehc.k_roof, ehc.cp_roof
            )

        # Soil store capacity for roofs
        if hasattr(roof, "soilstorecap") and roof.soilstorecap is not None:
            ehc.soil_storecap_roof[i] = _unwrap(roof.soilstorecap)
        if hasattr(roof, "statelimit") and roof.statelimit is not None:
            ehc.state_limit_roof[i] = _unwrap(roof.statelimit)
        if hasattr(roof, "wetthresh") and roof.wetthresh is not None:
            ehc.wet_thresh_roof[i] = _unwrap(roof.wetthresh)

    for i, wall in enumerate(walls[:nlayer]):
        if hasattr(wall, "thermal_layers") and wall.thermal_layers is not None:
            _populate_thermal_layer_arrays(
                wall.thermal_layers, i, ndepth, ehc.dz_wall, ehc.k_wall, ehc.cp_wall
            )

        # Soil store capacity for walls
        if hasattr(wall, "soilstorecap") and wall.soilstorecap is not None:
            ehc.soil_storecap_wall[i] = _unwrap(wall.soilstorecap)
        if hasattr(wall, "statelimit") and wall.statelimit is not None:
            ehc.state_limit_wall[i] = _unwrap(wall.statelimit)
        if hasattr(wall, "wetthresh") and wall.wetthresh is not None:
            ehc.wet_thresh_wall[i] = _unwrap(wall.wetthresh)

    # Populate tin_roof and tin_wall from initial states
    if hasattr(initial_states, "roofs") and initial_states.roofs:
        for i, roof_state in enumerate(initial_states.roofs[:nlayer]):
            tin = _unwrap(roof_state.temperature)
            if isinstance(tin, (list, np.ndarray)):
                ehc.tin_roof[i] = tin[0] if len(tin) > 0 else 5.0
            elif tin is not None:
                ehc.tin_roof[i] = tin

    if hasattr(initial_states, "walls") and initial_states.walls:
        for i, wall_state in enumerate(initial_states.walls[:nlayer]):
            tin = _unwrap(wall_state.temperature)
            if isinstance(tin, (list, np.ndarray)):
                ehc.tin_wall[i] = tin[0] if len(tin) > 0 else 5.0
            elif tin is not None:
                ehc.tin_wall[i] = tin


def populate_roughnessstate(
    state_dts: dts.SUEWS_STATE,
    site_dts: dts.SUEWS_SITE,
) -> None:
    """Populate roughnessstate from site parameters.

    This initializes roughness-related state variables from the site's
    z0m_in, zdm_in, and FAI parameters.

    Parameters
    ----------
    state_dts : dts.SUEWS_STATE
        State object to populate.
    site_dts : dts.SUEWS_SITE
        Site object with z0m_in, zdm_in, and FAI values.
    """
    rs = state_dts.roughnessstate

    # Initialize from site input values
    rs.z0m = site_dts.z0m_in
    rs.zdm = site_dts.zdm_in

    # FAI values from site
    rs.faibldg_use = site_dts.lc_bldg.faibldg
    rs.faidectree_use = site_dts.lc_dectr.faidectree
    rs.faievetree_use = site_dts.lc_evetr.faievetree

    # Total frontal area index
    rs.fai = rs.faibldg_use + rs.faidectree_use + rs.faievetree_use

    # Plan area index (from building height and LAD)
    # Simple initial estimate
    sfr_bldg = site_dts.lc_bldg.sfr
    sfr_dectr = site_dts.lc_dectr.sfr
    sfr_evetr = site_dts.lc_evetr.sfr
    rs.pai = sfr_bldg + (sfr_dectr + sfr_evetr) * 0.5

    # Mean building height
    rs.zh = site_dts.lc_bldg.bldgh

    # z - d (roughness sublayer)
    rs.zzd = site_dts.z - rs.zdm

    # Initial z0v (vegetation roughness) - small value
    rs.z0v = 0.0001

    # Iteration safety flag - should be True for safe iteration
    rs.iter_safe = 1


def populate_atmstate(
    state_dts: dts.SUEWS_STATE,
    forcing_dts: dts.SUEWS_FORCING,
    tair_av: float | None = None,
    l_mod: float | None = None,
    ustar: float | None = None,
    ra_h: float | None = None,
    rb: float | None = None,
    rs: float | None = None,
) -> None:
    """Populate atmstate with initial atmospheric values.

    This initializes atmospheric state variables from the first forcing
    timestep. The tair_av (running average temperature) is essential for
    anthropogenic heat calculations.

    Parameters
    ----------
    state_dts : dts.SUEWS_STATE
        State object to populate.
    forcing_dts : dts.SUEWS_FORCING
        Forcing object with initial meteorological values.
    tair_av : float, optional
        5-day moving average temperature [K] from previous run's final state.
        If provided (continuation run), use this instead of initialising from
        forcing temperature. If None (fresh run), initialise from forcing.
    l_mod : float, optional
        Obukhov length [m] from previous run. Essential for stability-dependent
        resistance calculations in continuation runs.
    ustar : float, optional
        Friction velocity [m s-1] from previous run.
    ra_h : float, optional
        Aerodynamic resistance for heat [s m-1] from previous run.
    rb : float, optional
        Quasi-laminar boundary layer resistance [s m-1] from previous run.
    rs : float, optional
        Surface resistance [s m-1] from previous run.

    Notes
    -----
    Most atmstate values (avcp, avdens, etc.) are recalculated each timestep
    by cal_AtmMoist. However, tair_av needs initialization as it's a running
    average that persists across timesteps. For continuation runs, l_mod,
    ustar, ra_h, rb, and rs provide better initial guesses for iterative
    stability calculations, ensuring bit-identical output with continuous runs.
    """
    atm = state_dts.atmstate
    temp_c = forcing_dts.temp_c
    pres = forcing_dts.pres
    rh = forcing_dts.rh

    # Initialize tair_av: use provided value (continuation) or forcing temperature (fresh)
    if tair_av is not None and tair_av > 0:
        atm.tair_av = tair_av
    else:
        atm.tair_av = temp_c + 273.15

    # Initialize stability/resistance values for continuation runs
    if l_mod is not None and l_mod != 0:
        atm.l_mod = l_mod
    if ustar is not None and ustar > 0:
        atm.ustar = ustar
    if ra_h is not None and ra_h > 0:
        atm.ra_h = ra_h
    if rb is not None and rb > 0:
        atm.rb = rb
    if rs is not None and rs > 0:
        atm.rs = rs

    # Calculate initial atmospheric moisture values
    # These match the cal_AtmMoist calculations in Fortran

    # Latent heat of vaporization [J kg-1]
    # Formula: lv_J_kg = (2500.25 - 2.365*temp_C)*1000
    temp_k = temp_c + 273.15
    atm.lv_j_kg = (2500.25 - 2.365 * temp_c) * 1000

    # Latent heat of sublimation [J kg-1]
    atm.lvs_j_kg = 2.834e6

    # Saturation vapour pressure [hPa]
    # es = 6.1078*10**(7.5*Temp_C/(Temp_C+237.3))
    atm.es_hpa = 6.1078 * 10 ** (7.5 * temp_c / (temp_c + 237.3))

    # Actual vapour pressure [hPa]
    atm.ea_hpa = rh / 100.0 * atm.es_hpa

    # Vapour pressure deficit [hPa and Pa]
    atm.vpd_hpa = atm.es_hpa - atm.ea_hpa
    atm.vpd_pa = atm.vpd_hpa * 100.0

    # Specific humidity deficit [kg kg-1]
    # dq = 0.622*(es_hPa - Ea_hPa)/pres
    atm.dq = 0.622 * (atm.es_hpa - atm.ea_hpa) / pres

    # Dry air density [kg m-3]
    # dens_dry = pres*100/(287.04*temp_k)
    atm.dens_dry = pres * 100.0 / (287.04 * temp_k)

    # Specific heat capacity [J kg-1 K-1]
    # avcp = 1004.67 for dry air, adjusted for humidity
    atm.avcp = 1004.67 * (1.0 + 0.84 * atm.ea_hpa / pres)

    # Average air density [kg m-3]
    # Includes moisture effect
    atm.avdens = atm.dens_dry * (1.0 + 0.61 * atm.ea_hpa / pres)

    # Cloud fraction from forcing (if available)
    fcld = forcing_dts.fcld
    if fcld >= 0 and fcld <= 1:
        atm.fcld = fcld

    # Iteration safety flag
    atm.iter_safe = True

    # Initialize dynamic OHM temperatures from air temperature
    # Reference: suews_ctrl_driver.f95 lines 4973, 4977
    # MetForcingBlock(1, 12) = first timestep air temperature
    heat = state_dts.heatstate
    heat.temp_surf_dyohm = temp_c
    heat.tsfc_surf_dyohm = temp_c


def populate_ohmstate_defaults(state_dts: dts.SUEWS_STATE) -> None:
    """Initialize OHM running averages to match Fortran defaults.

    Reference: suews_ctrl_driver.f95 lines 4985-5009

    These are running averages and dynamic coefficients that need sensible
    initial values. The simulation will update them over time.

    Parameters
    ----------
    state_dts : dts.SUEWS_STATE
        State object to populate.
    """
    ohm = state_dts.ohmstate

    # Running average values
    ohm.t2_prev = 0.0  # Previous midnight temperature [degC]
    ohm.ws_rav = 2.0  # Wind speed running average [m s-1]
    ohm.tair_prev = 0.0  # Previous air temperature [degC]
    ohm.qn_rav = 0.0  # Net radiation running average [W m-2]

    # Dynamic OHM coefficients for all surfaces - initialize to 0.0
    # These are updated during simulation based on conditions
    for surface in ["bldg", "paved", "evetr", "dectr", "grass", "bsoil", "water"]:
        setattr(ohm, f"a1_{surface}", 0.0)
        setattr(ohm, f"a2_{surface}", 0.0)
        setattr(ohm, f"a3_{surface}", 0.0)


def _populate_anthroemis(anthroemis_dts, anthro_pydantic) -> None:
    """Populate anthroEMIS_PRM from Pydantic AnthropogenicEmissions.

    Parameters
    ----------
    anthroemis_dts : dts.ANTHRO_EMIS_PRM
        Fortran DTS object to populate.
    anthro_pydantic : AnthropogenicEmissions
        Pydantic anthropogenic emissions object.
    """
    # Daylight saving times
    startdls = _unwrap(anthro_pydantic.startdls) if anthro_pydantic.startdls else 0
    enddls = _unwrap(anthro_pydantic.enddls) if anthro_pydantic.enddls else 0
    anthroemis_dts.startdls = int(startdls)
    anthroemis_dts.enddls = int(enddls)

    # Populate nested anthroheat parameters
    heat = anthro_pydantic.heat
    ah = anthroemis_dts.anthroheat

    # QF parameters - working_day and holiday variants from DayProfile
    for name in ("qf0_beu", "qf_a", "qf_b", "qf_c"):
        _set_day_profile(ah, name, getattr(heat, name))

    # Base temperatures, AH min/slopes, and population density (DayProfile fields)
    for name in (
        "baset_cooling",
        "baset_heating",
        "ah_min",
        "ah_slope_cooling",
        "ah_slope_heating",
        "popdensdaytime",
    ):
        _set_day_profile(ah, name, getattr(heat, name))
    ah.popdensnighttime = _unwrap(heat.popdensnighttime)

    # 24-hour profiles - convert dict to numpy arrays
    ah.ahprof_24hr_working = _hourly_dict_to_array(heat.ahprof_24hr.working_day)
    ah.ahprof_24hr_holiday = _hourly_dict_to_array(heat.ahprof_24hr.holiday)

    ah.popprof_24hr_working = _hourly_dict_to_array(heat.popprof_24hr.working_day)
    ah.popprof_24hr_holiday = _hourly_dict_to_array(heat.popprof_24hr.holiday)

    # CO2 parameters
    co2 = anthro_pydantic.co2
    for name in (
        "ef_umolco2perj",
        "enef_v_jkm",
        "frfossilfuel_heat",
        "frfossilfuel_nonheat",
    ):
        setattr(
            anthroemis_dts,
            name,
            _unwrap(getattr(co2, name)) if hasattr(co2, name) else 0.0,
        )

    # Metabolism limits
    for name in ("maxfcmetab", "maxqfmetab", "minfcmetab", "minqfmetab"):
        setattr(
            anthroemis_dts,
            name,
            _unwrap(getattr(co2, name)) if hasattr(co2, name) else 0.0,
        )

    # Traffic parameters - trafficrate is a DayProfile
    if hasattr(co2, "trafficrate") and co2.trafficrate:
        anthroemis_dts.trafficrate_working = _unwrap(co2.trafficrate.working_day) or 0.0
        anthroemis_dts.trafficrate_holiday = _unwrap(co2.trafficrate.holiday) or 0.0
    anthroemis_dts.trafficunits = (
        _unwrap(co2.trafficunits)
        if hasattr(co2, "trafficunits") and co2.trafficunits
        else 0.0
    )

    # Traffic profiles - HourlyProfile
    if hasattr(co2, "traffprof_24hr") and co2.traffprof_24hr:
        anthroemis_dts.traffprof_24hr_working = _hourly_dict_to_array(
            co2.traffprof_24hr.working_day
        )
        anthroemis_dts.traffprof_24hr_holiday = _hourly_dict_to_array(
            co2.traffprof_24hr.holiday
        )

    # Human activity profiles - HourlyProfile
    if hasattr(co2, "humactivity_24hr") and co2.humactivity_24hr:
        anthroemis_dts.humactivity_24hr_working = _hourly_dict_to_array(
            co2.humactivity_24hr.working_day
        )
        anthroemis_dts.humactivity_24hr_holiday = _hourly_dict_to_array(
            co2.humactivity_24hr.holiday
        )

    # FcEF_v_kgkm - DayProfile gives working/holiday values as 2-element array
    if hasattr(co2, "fcef_v_kgkm") and co2.fcef_v_kgkm:
        fcef_working = _unwrap(co2.fcef_v_kgkm.working_day) or 0.0
        fcef_holiday = _unwrap(co2.fcef_v_kgkm.holiday) or 0.0
        anthroemis_dts.fcef_v_kgkm = np.array(
            [fcef_working, fcef_holiday], dtype=np.float64
        )
    else:
        anthroemis_dts.fcef_v_kgkm = np.array([0.0, 0.0], dtype=np.float64)
