"""DTS-based SUEWS simulation runner.

This module provides a simplified interface to run SUEWS simulations using
Derived Type Structures (DTS) directly, eliminating the df_state DataFrame
intermediate layer.

The approach uses f90wrap-generated DTS objects and accessor functions to
interact with the Fortran SUEWS kernel without complex DataFrame conversions.
"""

from dataclasses import dataclass
from typing import Optional, Tuple, Dict, Any

import numpy as np
import pandas as pd

from .supy_driver import suews_driver as sd
from .supy_driver import module_ctrl_type as dts
from .supy_driver import suews_state_accessors as acc


@dataclass
class DTSConfig:
    """Configuration for DTS-based simulation.

    Maps Python/YAML configuration to Fortran DTS objects.
    """
    nlayer: int = 5
    ndepth: int = 5
    nsurf: int = 7  # Standard SUEWS surfaces


def create_suews_config() -> dts.SUEWS_CONFIG:
    """Create a SUEWS_CONFIG DTS object with default values.

    Returns
    -------
    dts.SUEWS_CONFIG
        Initialised config object with default method flags.
    """
    config = dts.SUEWS_CONFIG()
    # Defaults are already set to 0 by Fortran initialisation
    return config


def create_suews_state(nlayer: int = 5, ndepth: int = 5) -> dts.SUEWS_STATE:
    """Create and allocate a SUEWS_STATE DTS object.

    Parameters
    ----------
    nlayer : int
        Number of urban canopy layers
    ndepth : int
        Number of substrate depth levels

    Returns
    -------
    dts.SUEWS_STATE
        Allocated state object ready for simulation
    """
    state = dts.SUEWS_STATE()
    state.allocate(nlayer=nlayer, ndepth=ndepth)
    return state


def create_suews_forcing() -> dts.SUEWS_FORCING:
    """Create a SUEWS_FORCING DTS object.

    Returns
    -------
    dts.SUEWS_FORCING
        Forcing object to be populated with meteorological data
    """
    return dts.SUEWS_FORCING()


def create_suews_timer() -> dts.SUEWS_TIMER:
    """Create a SUEWS_TIMER DTS object.

    Returns
    -------
    dts.SUEWS_TIMER
        Timer object to be populated with temporal info
    """
    return dts.SUEWS_TIMER()


def populate_timer_from_datetime(
    timer: dts.SUEWS_TIMER,
    dt: pd.Timestamp,
    tstep_s: int = 3600,
    dt_since_start: int = 0,
    is_dls: bool = False
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
    """
    timer.iy = dt.year
    timer.id = dt.dayofyear
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
    frac_day = (dt.hour + dt.minute / 60.0 + dt.second / 3600.0) / 24.0
    timer.dectime = float(dt.dayofyear) + frac_day

    # Day of week (1=Monday, 7=Sunday)
    timer.dayofweek_id = dt.dayofweek + 1  # pandas uses 0=Monday

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
        'rslmethod', 'emissionsmethod', 'storageheatmethod', 'ohmincqf',
        'netradiationmethod', 'stabilitymethod', 'roughlenheatmethod',
        'roughlenmommethod', 'smdmethod', 'snowuse', 'waterusemethod',
        'laimethod', 'evapmethod', 'rcmethod', 'stebbsmethod', 'faimethod',
        'diagnose', 'diagqs', 'flag_test', 'use_sw_direct_albedo'
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
    """
    # Map forcing data columns to DTS attributes
    forcing_map = {
        'kdown': 'kdown',
        'ldown_obs': 'ldown',
        'precip': 'rain',
        'press_hpa': 'pres',
        'avrh': 'rh',
        'temp_c': 'temp_c',
        'fcld_obs': 'fcld',
        'qn1_obs': 'qn1_obs',
        'qs_obs': 'qs_obs',
        'qf_obs': 'qf_obs',
        'snowfrac_obs': 'snowfrac',
        'lai_obs': 'lai_obs',
    }

    for col, attr in forcing_map.items():
        if col in row.index:
            setattr(forcing, attr, float(row[col]))


def populate_state_from_config(
    state: dts.SUEWS_STATE,
    config_dict: Dict[str, Any],
    nlayer: int = 5,
    ndepth: int = 5,
    nsurf: int = 7
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
    if 'temp_roof' in config_dict:
        temp_roof = np.array(config_dict['temp_roof'], dtype=np.float64, order='F')
        temp_wall = np.array(config_dict.get('temp_wall', temp_roof), dtype=np.float64, order='F')
        temp_surf = np.array(config_dict.get('temp_surf', np.zeros((nsurf, ndepth))), dtype=np.float64, order='F')
        acc.set_heat_state_temp(state, nlayer, ndepth, nsurf, temp_roof, temp_wall, temp_surf)

    # Extract initial surface temperatures
    if 'tsfc_roof' in config_dict:
        tsfc_roof = np.array(config_dict['tsfc_roof'], dtype=np.float64)
        tsfc_wall = np.array(config_dict.get('tsfc_wall', tsfc_roof), dtype=np.float64)
        tsfc_surf = np.array(config_dict.get('tsfc_surf', np.zeros(nsurf)), dtype=np.float64)
        acc.set_heat_state_tsfc(state, nlayer, nsurf, tsfc_roof, tsfc_wall, tsfc_surf)

    # Extract initial hydro state
    if 'soilstore_surf' in config_dict:
        soilstore_roof = np.array(config_dict.get('soilstore_roof', np.zeros(nlayer)), dtype=np.float64)
        soilstore_wall = np.array(config_dict.get('soilstore_wall', np.zeros(nlayer)), dtype=np.float64)
        soilstore_surf = np.array(config_dict['soilstore_surf'], dtype=np.float64)
        acc.set_hydro_state_soilstore(state, nlayer, soilstore_roof, soilstore_wall, soilstore_surf)

    if 'state_surf' in config_dict:
        state_roof = np.array(config_dict.get('state_roof', np.zeros(nlayer)), dtype=np.float64)
        state_wall = np.array(config_dict.get('state_wall', np.zeros(nlayer)), dtype=np.float64)
        state_surf = np.array(config_dict['state_surf'], dtype=np.float64)
        acc.set_hydro_state_wetness(state, nlayer, state_roof, state_wall, state_surf)

    # Extract initial snow state
    if 'snowpack' in config_dict:
        snowpack = np.array(config_dict['snowpack'], dtype=np.float64)
        snowfrac = np.array(config_dict.get('snowfrac', np.zeros(nsurf)), dtype=np.float64)
        snowdens = np.array(config_dict.get('snowdens', np.full(nsurf, 100.0)), dtype=np.float64)
        icefrac = np.array(config_dict.get('icefrac', np.zeros(nsurf)), dtype=np.float64)
        acc.set_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)

    if 'snowalb' in config_dict:
        acc.set_snow_state_scalars(
            state,
            config_dict.get('snowalb', 0.6),
            config_dict.get('swe', 0.0),
            config_dict.get('mwh', 0.0),
            config_dict.get('qm', 0.0)
        )


def extract_heat_state(state: dts.SUEWS_STATE) -> Dict[str, Any]:
    """Extract heat state values from DTS object.

    Parameters
    ----------
    state : dts.SUEWS_STATE
        State object to extract from

    Returns
    -------
    dict
        Dictionary of heat state values
    """
    nlayer, ndepth, nsurf = acc.get_heat_state_dims(state)

    result = {}

    # Get scalar values
    qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(state)
    result['qh'] = qh
    result['qe'] = qe
    result['qs'] = qs
    result['qn'] = qn
    result['qf'] = qf
    result['tsurf'] = tsurf

    if nlayer > 0 and ndepth > 0:
        # Get temperature arrays
        temp_roof = np.zeros((nlayer, ndepth), dtype=np.float64, order='F')
        temp_wall = np.zeros((nlayer, ndepth), dtype=np.float64, order='F')
        temp_surf = np.zeros((nsurf, ndepth), dtype=np.float64, order='F')
        acc.get_heat_state_temp(state, nlayer, ndepth, nsurf, temp_roof, temp_wall, temp_surf)
        result['temp_roof'] = temp_roof.copy()
        result['temp_wall'] = temp_wall.copy()
        result['temp_surf'] = temp_surf.copy()

        # Get surface temperatures
        tsfc_roof = np.zeros(nlayer, dtype=np.float64)
        tsfc_wall = np.zeros(nlayer, dtype=np.float64)
        tsfc_surf = np.zeros(nsurf, dtype=np.float64)
        acc.get_heat_state_tsfc(state, nlayer, nsurf, tsfc_roof, tsfc_wall, tsfc_surf)
        result['tsfc_roof'] = tsfc_roof.copy()
        result['tsfc_wall'] = tsfc_wall.copy()
        result['tsfc_surf'] = tsfc_surf.copy()

    return result


def extract_hydro_state(state: dts.SUEWS_STATE) -> Dict[str, Any]:
    """Extract hydro state values from DTS object.

    Parameters
    ----------
    state : dts.SUEWS_STATE
        State object to extract from

    Returns
    -------
    dict
        Dictionary of hydro state values
    """
    nlayer, nsurf = acc.get_hydro_state_dims(state)

    result = {}

    # Get scalar values
    smd, runoff, evap, drain = acc.get_hydro_state_scalars(state)
    result['smd'] = smd
    result['runoff_per_tstep'] = runoff
    result['ev_per_tstep'] = evap
    result['drain_per_tstep'] = drain

    if nlayer > 0:
        # Get soil store arrays
        soilstore_roof = np.zeros(nlayer, dtype=np.float64)
        soilstore_wall = np.zeros(nlayer, dtype=np.float64)
        soilstore_surf = np.zeros(nsurf, dtype=np.float64)
        acc.get_hydro_state_soilstore(state, nlayer, soilstore_roof, soilstore_wall, soilstore_surf)
        result['soilstore_roof'] = soilstore_roof.copy()
        result['soilstore_wall'] = soilstore_wall.copy()
        result['soilstore_surf'] = soilstore_surf.copy()

        # Get wetness state arrays
        state_roof = np.zeros(nlayer, dtype=np.float64)
        state_wall = np.zeros(nlayer, dtype=np.float64)
        state_surf = np.zeros(nsurf, dtype=np.float64)
        acc.get_hydro_state_wetness(state, nlayer, state_roof, state_wall, state_surf)
        result['state_roof'] = state_roof.copy()
        result['state_wall'] = state_wall.copy()
        result['state_surf'] = state_surf.copy()

    return result


def extract_snow_state(state: dts.SUEWS_STATE) -> Dict[str, Any]:
    """Extract snow state values from DTS object.

    Parameters
    ----------
    state : dts.SUEWS_STATE
        State object to extract from

    Returns
    -------
    dict
        Dictionary of snow state values
    """
    _, _, nsurf = acc.get_heat_state_dims(state)  # Use nsurf from heat dims

    result = {}

    # Get scalar values
    snowalb, swe, mwh, qm = acc.get_snow_state_scalars(state)
    result['snowalb'] = snowalb
    result['swe'] = swe
    result['mwh'] = mwh
    result['qm'] = qm

    # Get array values
    snowpack = np.zeros(nsurf, dtype=np.float64)
    snowfrac = np.zeros(nsurf, dtype=np.float64)
    snowdens = np.zeros(nsurf, dtype=np.float64)
    icefrac = np.zeros(nsurf, dtype=np.float64)
    acc.get_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)
    result['snowpack'] = snowpack.copy()
    result['snowfrac'] = snowfrac.copy()
    result['snowdens'] = snowdens.copy()
    result['icefrac'] = icefrac.copy()

    return result


def create_suews_site(nlayer: int = 5) -> dts.SUEWS_SITE:
    """Create and allocate a SUEWS_SITE DTS object.

    Parameters
    ----------
    nlayer : int
        Number of urban canopy layers

    Returns
    -------
    dts.SUEWS_SITE
        Allocated site object ready for configuration
    """
    site = dts.SUEWS_SITE()
    site.nlayer = nlayer
    site.allocate(nlayer=nlayer)
    return site


def populate_site_from_dict(
    site: dts.SUEWS_SITE,
    params: Dict[str, Any],
    nsurf: int = 7
) -> None:
    """Populate SUEWS_SITE from a parameter dictionary.

    This populates the directly accessible site parameters. Nested parameter
    types (spartacus, lumps, ehc, etc.) are not accessible through f90wrap
    and require separate accessor functions.

    Parameters
    ----------
    site : dts.SUEWS_SITE
        Site object to populate
    params : dict
        Dictionary of site parameters. Keys should match SUEWS_SITE attributes:
        - lat, lon, alt: Location coordinates
        - timezone: Time zone offset from UTC
        - gridiv: Grid identifier
        - surfacearea: Grid area [m2]
        - z: Measurement height [m]
        - z0m_in, zdm_in: Roughness parameters
        - pipecapacity, runofftowater: Water infrastructure
        - narp_trans_site: Atmospheric transmissivity
        - co2pointsource: CO2 emission factor
        - flowchange: Water flow change
        - n_buildings, h_std, lambda_c: Building parameters
        - sfr_surf: Surface fractions (7 surfaces)
        - sfr_roof, sfr_wall: Building facet fractions
    nsurf : int
        Number of surfaces (default 7)
    """
    # Scalar parameters mapping
    scalar_attrs = [
        'lat', 'lon', 'alt', 'timezone', 'gridiv', 'surfacearea',
        'z', 'z0m_in', 'zdm_in', 'pipecapacity', 'runofftowater',
        'narp_trans_site', 'co2pointsource', 'flowchange',
        'n_buildings', 'h_std', 'lambda_c'
    ]

    for attr in scalar_attrs:
        if attr in params:
            value = params[attr]
            # Handle RefValue wrappers from Pydantic config
            if hasattr(value, 'value'):
                value = value.value
            setattr(site, attr, float(value))

    # Derived fractions (calculated from sfr_surf if not provided)
    if 'sfr_surf' in params:
        sfr = np.array(params['sfr_surf'], dtype=np.float64)
        if len(sfr) == nsurf:
            site.sfr_surf = sfr
            # Calculate derived fractions
            # SUEWS surfaces: 0=Paved, 1=Bldg, 2=EveTr, 3=DecTr, 4=Grass, 5=BSoil, 6=Water
            site.vegfraction = float(np.sum(sfr[2:5]))  # EveTr + DecTr + Grass
            site.impervfraction = float(np.sum(sfr[0:2]))  # Paved + Bldg
            site.pervfraction = float(np.sum(sfr[2:6]))  # EveTr + DecTr + Grass + BSoil
            site.nonwaterfraction = float(np.sum(sfr[0:6]))  # All except Water

    # Override derived fractions if explicitly provided
    for frac_attr in ['vegfraction', 'impervfraction', 'pervfraction', 'nonwaterfraction']:
        if frac_attr in params:
            value = params[frac_attr]
            if hasattr(value, 'value'):
                value = value.value
            setattr(site, frac_attr, float(value))

    # Building facet fractions (need nlayer-sized arrays)
    nlayer = site.nlayer
    if 'sfr_roof' in params:
        sfr_roof = np.array(params['sfr_roof'], dtype=np.float64)
        if len(sfr_roof) == nlayer:
            site.sfr_roof = sfr_roof
    if 'sfr_wall' in params:
        sfr_wall = np.array(params['sfr_wall'], dtype=np.float64)
        if len(sfr_wall) == nlayer:
            site.sfr_wall = sfr_wall


def extract_site_params(site: dts.SUEWS_SITE) -> Dict[str, Any]:
    """Extract site parameters from DTS object to dictionary.

    Parameters
    ----------
    site : dts.SUEWS_SITE
        Site object to extract from

    Returns
    -------
    dict
        Dictionary of site parameters
    """
    result = {}

    # Scalar parameters
    scalar_attrs = [
        'lat', 'lon', 'alt', 'timezone', 'gridiv', 'surfacearea',
        'z', 'z0m_in', 'zdm_in', 'pipecapacity', 'runofftowater',
        'narp_trans_site', 'co2pointsource', 'flowchange',
        'n_buildings', 'h_std', 'lambda_c', 'nlayer'
    ]
    for attr in scalar_attrs:
        result[attr] = getattr(site, attr)

    # Derived fractions
    result['vegfraction'] = site.vegfraction
    result['impervfraction'] = site.impervfraction
    result['pervfraction'] = site.pervfraction
    result['nonwaterfraction'] = site.nonwaterfraction

    # Array parameters
    result['sfr_surf'] = np.array(site.sfr_surf).copy()
    result['sfr_roof'] = np.array(site.sfr_roof).copy()
    result['sfr_wall'] = np.array(site.sfr_wall).copy()

    return result


class StateAccessor:
    """Clean Python interface to Fortran SUEWS_STATE accessor functions.

    This class provides methods to extract state to dictionaries and populate
    state from dictionaries, covering all 12 nested state types in SUEWS_STATE.

    Parameters
    ----------
    state : dts.SUEWS_STATE
        The Fortran DTS state object to wrap
    nlayer : int, optional
        Number of urban canopy layers, by default 5
    ndepth : int, optional
        Number of substrate depth levels, by default 5
    nsurf : int, optional
        Number of surfaces, by default 7
    nvegsurf : int, optional
        Number of vegetation surfaces, by default 3
    """

    def __init__(
        self,
        state: dts.SUEWS_STATE,
        nlayer: int = 5,
        ndepth: int = 5,
        nsurf: int = 7,
        nvegsurf: int = 3
    ):
        self._state = state
        self._nlayer = nlayer
        self._ndepth = ndepth
        self._nsurf = nsurf
        self._nvegsurf = nvegsurf

    @property
    def state(self) -> dts.SUEWS_STATE:
        """Return the underlying Fortran state object."""
        return self._state

    # =========================================================
    # Full state extraction/population
    # =========================================================

    def to_dict(self) -> Dict[str, Any]:
        """Extract complete state to dictionary.

        Returns
        -------
        dict
            Dictionary with all state components organised by type
        """
        return {
            'flag': self.get_flag_state(),
            'solar': self.get_solar_state(),
            'roughness': self.get_roughness_state(),
            'nhood': self.get_nhood_state(),
            'ohm': self.get_ohm_state(),
            'atm': self.get_atm_state(),
            'anthro': self.get_anthro_state(),
            'phen': self.get_phen_state(),
            'stebbs': self.get_stebbs_state(),
            'heat': extract_heat_state(self._state),
            'hydro': extract_hydro_state(self._state),
            'snow': extract_snow_state(self._state),
        }

    def from_dict(self, state_dict: Dict[str, Any]) -> None:
        """Populate state from dictionary.

        Parameters
        ----------
        state_dict : dict
            Dictionary with state components to set
        """
        if 'flag' in state_dict:
            self.set_flag_state(state_dict['flag'])
        if 'solar' in state_dict:
            self.set_solar_state(state_dict['solar'])
        if 'roughness' in state_dict:
            self.set_roughness_state(state_dict['roughness'])
        if 'nhood' in state_dict:
            self.set_nhood_state(state_dict['nhood'])
        if 'ohm' in state_dict:
            self.set_ohm_state(state_dict['ohm'])
        if 'atm' in state_dict:
            self.set_atm_state(state_dict['atm'])
        if 'anthro' in state_dict:
            self.set_anthro_state(state_dict['anthro'])
        if 'phen' in state_dict:
            self.set_phen_state(state_dict['phen'])
        if 'stebbs' in state_dict:
            self.set_stebbs_state(state_dict['stebbs'])

        # Use existing populate functions for heat/hydro/snow
        if 'heat' in state_dict or 'hydro' in state_dict or 'snow' in state_dict:
            combined = {}
            if 'heat' in state_dict:
                combined.update(state_dict['heat'])
            if 'hydro' in state_dict:
                combined.update(state_dict['hydro'])
            if 'snow' in state_dict:
                combined.update(state_dict['snow'])
            populate_state_from_config(
                self._state, combined, self._nlayer, self._ndepth, self._nsurf
            )

    # =========================================================
    # FLAG_STATE accessors
    # =========================================================

    def get_flag_state(self) -> Dict[str, Any]:
        """Get flag state values."""
        flag_converge, i_iter, stebbs_bldg_init = acc.get_flag_state(self._state)
        return {
            'flag_converge': bool(flag_converge),
            'i_iter': i_iter,
            'stebbs_bldg_init': stebbs_bldg_init,
        }

    def set_flag_state(self, d: Dict[str, Any]) -> None:
        """Set flag state values."""
        acc.set_flag_state(
            self._state,
            d.get('flag_converge', False),
            d.get('i_iter', 0),
            d.get('stebbs_bldg_init', 0),
        )

    # =========================================================
    # SOLAR_STATE accessors
    # =========================================================

    def get_solar_state(self) -> Dict[str, float]:
        """Get solar state values."""
        azimuth, zenith = acc.get_solar_state(self._state)
        return {'azimuth_deg': azimuth, 'zenith_deg': zenith}

    def set_solar_state(self, d: Dict[str, float]) -> None:
        """Set solar state values."""
        acc.set_solar_state(
            self._state,
            d.get('azimuth_deg', 0.0),
            d.get('zenith_deg', 0.0),
        )

    # =========================================================
    # ROUGHNESS_STATE accessors
    # =========================================================

    def get_roughness_state(self) -> Dict[str, float]:
        """Get roughness state values."""
        (FAIBldg_use, FAIEveTree_use, FAIDecTree_use,
         FAI, PAI, Zh, z0m, z0v, zdm, ZZD) = acc.get_roughness_state(self._state)
        return {
            'FAIBldg_use': FAIBldg_use,
            'FAIEveTree_use': FAIEveTree_use,
            'FAIDecTree_use': FAIDecTree_use,
            'FAI': FAI,
            'PAI': PAI,
            'Zh': Zh,
            'z0m': z0m,
            'z0v': z0v,
            'zdm': zdm,
            'ZZD': ZZD,
        }

    def set_roughness_state(self, d: Dict[str, float]) -> None:
        """Set roughness state values."""
        acc.set_roughness_state(
            self._state,
            d.get('FAIBldg_use', 0.0),
            d.get('FAIEveTree_use', 0.0),
            d.get('FAIDecTree_use', 0.0),
            d.get('FAI', 0.0),
            d.get('PAI', 0.0),
            d.get('Zh', 0.0),
            d.get('z0m', 0.0),
            d.get('z0v', 0.0),
            d.get('zdm', 0.0),
            d.get('ZZD', 0.0),
        )

    # =========================================================
    # NHOOD_STATE accessors
    # =========================================================

    def get_nhood_state(self) -> Dict[str, float]:
        """Get neighbourhood state values."""
        U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count = acc.get_nhood_state(self._state)
        return {
            'U_hbh_1dravg': U_hbh_1dravg,
            'QN_1dravg': QN_1dravg,
            'Tair_mn_prev': Tair_mn_prev,
            'iter_count': iter_count,
        }

    def set_nhood_state(self, d: Dict[str, float]) -> None:
        """Set neighbourhood state values."""
        acc.set_nhood_state(
            self._state,
            d.get('U_hbh_1dravg', 0.0),
            d.get('QN_1dravg', 0.0),
            d.get('Tair_mn_prev', 0.0),
            d.get('iter_count', 0.0),
        )

    # =========================================================
    # OHM_STATE accessors
    # =========================================================

    def get_ohm_state(self) -> Dict[str, Any]:
        """Get OHM state values."""
        result = {}

        # Radiation values
        qn_av, dqndt, qn_s_av, dqnsdt = acc.get_ohm_state_radiation(self._state)
        result.update({
            'qn_av': qn_av, 'dqndt': dqndt, 'qn_s_av': qn_s_av, 'dqnsdt': dqnsdt
        })

        # Grid coefficients
        a1, a2, a3 = acc.get_ohm_state_coef_grid(self._state)
        result.update({'a1': a1, 'a2': a2, 'a3': a3})

        # Running averages
        t2_prev, ws_rav, tair_prev, qn_rav = acc.get_ohm_state_averages(self._state)
        result.update({
            't2_prev': t2_prev, 'ws_rav': ws_rav,
            'tair_prev': tair_prev, 'qn_rav': qn_rav
        })

        # Surface coefficients
        (a1_bldg, a2_bldg, a3_bldg,
         a1_paved, a2_paved, a3_paved,
         a1_evetr, a2_evetr, a3_evetr,
         a1_dectr, a2_dectr, a3_dectr,
         a1_grass, a2_grass, a3_grass,
         a1_bsoil, a2_bsoil, a3_bsoil,
         a1_water, a2_water, a3_water) = acc.get_ohm_state_coef_surf(self._state)
        result.update({
            'a1_bldg': a1_bldg, 'a2_bldg': a2_bldg, 'a3_bldg': a3_bldg,
            'a1_paved': a1_paved, 'a2_paved': a2_paved, 'a3_paved': a3_paved,
            'a1_evetr': a1_evetr, 'a2_evetr': a2_evetr, 'a3_evetr': a3_evetr,
            'a1_dectr': a1_dectr, 'a2_dectr': a2_dectr, 'a3_dectr': a3_dectr,
            'a1_grass': a1_grass, 'a2_grass': a2_grass, 'a3_grass': a3_grass,
            'a1_bsoil': a1_bsoil, 'a2_bsoil': a2_bsoil, 'a3_bsoil': a3_bsoil,
            'a1_water': a1_water, 'a2_water': a2_water, 'a3_water': a3_water,
        })

        return result

    def set_ohm_state(self, d: Dict[str, Any]) -> None:
        """Set OHM state values."""
        # Radiation values
        acc.set_ohm_state_radiation(
            self._state,
            d.get('qn_av', 0.0), d.get('dqndt', 0.0),
            d.get('qn_s_av', 0.0), d.get('dqnsdt', 0.0)
        )

        # Grid coefficients
        acc.set_ohm_state_coef_grid(
            self._state, d.get('a1', 0.0), d.get('a2', 0.0), d.get('a3', 0.0)
        )

        # Running averages
        acc.set_ohm_state_averages(
            self._state,
            d.get('t2_prev', 0.0), d.get('ws_rav', 0.0),
            d.get('tair_prev', 0.0), d.get('qn_rav', 0.0)
        )

        # Surface coefficients
        acc.set_ohm_state_coef_surf(
            self._state,
            d.get('a1_bldg', 0.0), d.get('a2_bldg', 0.0), d.get('a3_bldg', 0.0),
            d.get('a1_paved', 0.0), d.get('a2_paved', 0.0), d.get('a3_paved', 0.0),
            d.get('a1_evetr', 0.0), d.get('a2_evetr', 0.0), d.get('a3_evetr', 0.0),
            d.get('a1_dectr', 0.0), d.get('a2_dectr', 0.0), d.get('a3_dectr', 0.0),
            d.get('a1_grass', 0.0), d.get('a2_grass', 0.0), d.get('a3_grass', 0.0),
            d.get('a1_bsoil', 0.0), d.get('a2_bsoil', 0.0), d.get('a3_bsoil', 0.0),
            d.get('a1_water', 0.0), d.get('a2_water', 0.0), d.get('a3_water', 0.0),
        )

    # =========================================================
    # ATM_STATE accessors
    # =========================================================

    def get_atm_state(self) -> Dict[str, Any]:
        """Get atmospheric state values."""
        result = {}

        # Thermodynamic properties
        (fcld, avcp, dens_dry, avdens, dq,
         lv_J_kg, lvS_J_kg, tlv) = acc.get_atm_state_thermo(self._state)
        result.update({
            'fcld': fcld, 'avcp': avcp, 'dens_dry': dens_dry, 'avdens': avdens,
            'dq': dq, 'lv_J_kg': lv_J_kg, 'lvS_J_kg': lvS_J_kg, 'tlv': tlv
        })

        # Vapour pressure variables
        (Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa,
         s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa) = acc.get_atm_state_vapour(self._state)
        result.update({
            'Ea_hPa': Ea_hPa, 'Es_hPa': Es_hPa, 'psyc_hPa': psyc_hPa,
            'psycIce_hPa': psycIce_hPa, 's_Pa': s_Pa, 's_hpa': s_hpa,
            'sIce_hpa': sIce_hpa, 'vpd_hPa': vpd_hPa, 'vpd_pa': vpd_pa
        })

        # Turbulence and stability
        (L_mod, zL, RA_h, RS,
         UStar, TStar, RB, Tair_av) = acc.get_atm_state_turb(self._state)
        result.update({
            'L_mod': L_mod, 'zL': zL, 'RA_h': RA_h, 'RS': RS,
            'UStar': UStar, 'TStar': TStar, 'RB': RB, 'Tair_av': Tair_av
        })

        # Near-surface diagnostics
        (U10_ms, U_hbh, T2_C,
         T_hbh_C, q2_gkg, RH2) = acc.get_atm_state_diag(self._state)
        result.update({
            'U10_ms': U10_ms, 'U_hbh': U_hbh, 'T2_C': T2_C,
            'T_hbh_C': T_hbh_C, 'q2_gkg': q2_gkg, 'RH2': RH2
        })

        # Surface resistance array
        rss_surf = np.zeros(self._nsurf, dtype=np.float64)
        acc.get_atm_state_rss_surf(self._state, rss_surf)
        result['rss_surf'] = rss_surf.copy()

        return result

    def set_atm_state(self, d: Dict[str, Any]) -> None:
        """Set atmospheric state values."""
        # Thermodynamic properties
        acc.set_atm_state_thermo(
            self._state,
            d.get('fcld', 0.0), d.get('avcp', 0.0), d.get('dens_dry', 0.0),
            d.get('avdens', 0.0), d.get('dq', 0.0), d.get('lv_J_kg', 0.0),
            d.get('lvS_J_kg', 0.0), d.get('tlv', 0.0)
        )

        # Vapour pressure variables
        acc.set_atm_state_vapour(
            self._state,
            d.get('Ea_hPa', 0.0), d.get('Es_hPa', 0.0), d.get('psyc_hPa', 0.0),
            d.get('psycIce_hPa', 0.0), d.get('s_Pa', 0.0), d.get('s_hpa', 0.0),
            d.get('sIce_hpa', 0.0), d.get('vpd_hPa', 0.0), d.get('vpd_pa', 0.0)
        )

        # Turbulence and stability
        acc.set_atm_state_turb(
            self._state,
            d.get('L_mod', 0.0), d.get('zL', 0.0), d.get('RA_h', 0.0),
            d.get('RS', 0.0), d.get('UStar', 0.0), d.get('TStar', 0.0),
            d.get('RB', 0.0), d.get('Tair_av', 0.0)
        )

        # Near-surface diagnostics
        acc.set_atm_state_diag(
            self._state,
            d.get('U10_ms', 0.0), d.get('U_hbh', 0.0), d.get('T2_C', 0.0),
            d.get('T_hbh_C', 0.0), d.get('q2_gkg', 0.0), d.get('RH2', 0.0)
        )

        # Surface resistance array
        if 'rss_surf' in d:
            rss_surf = np.array(d['rss_surf'], dtype=np.float64)
            acc.set_atm_state_rss_surf(self._state, rss_surf)

    # =========================================================
    # ANTHRO_EMIS_STATE accessors
    # =========================================================

    def get_anthro_state(self) -> Dict[str, Any]:
        """Get anthropogenic emissions state values."""
        result = {}

        # HDD array
        HDD_id = np.zeros(12, dtype=np.float64)
        acc.get_anthro_state_hdd(self._state, HDD_id)
        result['HDD_id'] = HDD_id.copy()

        # CO2 flux scalars
        (Fc, Fc_anthro, Fc_biogen, Fc_build,
         Fc_metab, Fc_photo, Fc_point,
         Fc_respi, Fc_traff) = acc.get_anthro_state_co2(self._state)
        result.update({
            'Fc': Fc, 'Fc_anthro': Fc_anthro, 'Fc_biogen': Fc_biogen,
            'Fc_build': Fc_build, 'Fc_metab': Fc_metab, 'Fc_photo': Fc_photo,
            'Fc_point': Fc_point, 'Fc_respi': Fc_respi, 'Fc_traff': Fc_traff
        })

        return result

    def set_anthro_state(self, d: Dict[str, Any]) -> None:
        """Set anthropogenic emissions state values."""
        # HDD array
        if 'HDD_id' in d:
            HDD_id = np.array(d['HDD_id'], dtype=np.float64)
            acc.set_anthro_state_hdd(self._state, HDD_id)

        # CO2 flux scalars
        acc.set_anthro_state_co2(
            self._state,
            d.get('Fc', 0.0), d.get('Fc_anthro', 0.0), d.get('Fc_biogen', 0.0),
            d.get('Fc_build', 0.0), d.get('Fc_metab', 0.0), d.get('Fc_photo', 0.0),
            d.get('Fc_point', 0.0), d.get('Fc_respi', 0.0), d.get('Fc_traff', 0.0)
        )

    # =========================================================
    # PHENOLOGY_STATE accessors
    # =========================================================

    def get_phen_state(self) -> Dict[str, Any]:
        """Get phenology state values."""
        result = {}

        # Albedo array
        alb = np.zeros(self._nsurf, dtype=np.float64)
        acc.get_phen_state_alb(self._state, alb)
        result['alb'] = alb.copy()

        # LAI and degree day arrays
        lai_id = np.zeros(self._nvegsurf, dtype=np.float64)
        GDD_id = np.zeros(self._nvegsurf, dtype=np.float64)
        SDD_id = np.zeros(self._nvegsurf, dtype=np.float64)
        acc.get_phen_state_lai(self._state, lai_id, GDD_id, SDD_id)
        result['lai_id'] = lai_id.copy()
        result['GDD_id'] = GDD_id.copy()
        result['SDD_id'] = SDD_id.copy()

        # Scalar values
        (porosity_id, decidcap_id, albDecTr_id, albEveTr_id, albGrass_id,
         Tmin_id, Tmax_id, lenDay_id, TempVeg) = acc.get_phen_state_scalars(self._state)
        result.update({
            'porosity_id': porosity_id, 'decidcap_id': decidcap_id,
            'albDecTr_id': albDecTr_id, 'albEveTr_id': albEveTr_id,
            'albGrass_id': albGrass_id, 'Tmin_id': Tmin_id, 'Tmax_id': Tmax_id,
            'lenDay_id': lenDay_id, 'TempVeg': TempVeg
        })

        # Conductance function values
        (gfunc, gsc, g_kdown, g_dq,
         g_ta, g_smd, g_lai) = acc.get_phen_state_conductance(self._state)
        result.update({
            'gfunc': gfunc, 'gsc': gsc, 'g_kdown': g_kdown, 'g_dq': g_dq,
            'g_ta': g_ta, 'g_smd': g_smd, 'g_lai': g_lai
        })

        # StoreDrainPrm array
        StoreDrainPrm = np.zeros((6, self._nsurf), dtype=np.float64, order='F')
        acc.get_phen_state_drain(self._state, StoreDrainPrm)
        result['StoreDrainPrm'] = StoreDrainPrm.copy()

        return result

    def set_phen_state(self, d: Dict[str, Any]) -> None:
        """Set phenology state values."""
        # Albedo array
        if 'alb' in d:
            alb = np.array(d['alb'], dtype=np.float64)
            acc.set_phen_state_alb(self._state, alb)

        # LAI and degree day arrays
        if 'lai_id' in d or 'GDD_id' in d or 'SDD_id' in d:
            lai_id = np.array(d.get('lai_id', np.zeros(self._nvegsurf)), dtype=np.float64)
            GDD_id = np.array(d.get('GDD_id', np.zeros(self._nvegsurf)), dtype=np.float64)
            SDD_id = np.array(d.get('SDD_id', np.zeros(self._nvegsurf)), dtype=np.float64)
            acc.set_phen_state_lai(self._state, lai_id, GDD_id, SDD_id)

        # Scalar values
        acc.set_phen_state_scalars(
            self._state,
            d.get('porosity_id', 0.0), d.get('decidcap_id', 0.0),
            d.get('albDecTr_id', 0.0), d.get('albEveTr_id', 0.0),
            d.get('albGrass_id', 0.0), d.get('Tmin_id', 0.0),
            d.get('Tmax_id', 0.0), d.get('lenDay_id', 0.0), d.get('TempVeg', 0.0)
        )

        # Conductance function values
        acc.set_phen_state_conductance(
            self._state,
            d.get('gfunc', 0.0), d.get('gsc', 0.0), d.get('g_kdown', 0.0),
            d.get('g_dq', 0.0), d.get('g_ta', 0.0), d.get('g_smd', 0.0),
            d.get('g_lai', 0.0)
        )

        # StoreDrainPrm array
        if 'StoreDrainPrm' in d:
            StoreDrainPrm = np.array(d['StoreDrainPrm'], dtype=np.float64, order='F')
            acc.set_phen_state_drain(self._state, StoreDrainPrm)

    # =========================================================
    # STEBBS_STATE accessors
    # =========================================================

    def get_stebbs_state(self) -> Dict[str, Any]:
        """Get STEBBS state values."""
        result = {}

        # Shortwave radiation
        (Kdown2d, Kup2d, Kwest,
         Ksouth, Knorth, Keast) = acc.get_stebbs_state_krad(self._state)
        result.update({
            'Kdown2d': Kdown2d, 'Kup2d': Kup2d, 'Kwest': Kwest,
            'Ksouth': Ksouth, 'Knorth': Knorth, 'Keast': Keast
        })

        # Longwave radiation
        (Ldown2d, Lup2d, Lwest,
         Lsouth, Lnorth, Least) = acc.get_stebbs_state_lrad(self._state)
        result.update({
            'Ldown2d': Ldown2d, 'Lup2d': Lup2d, 'Lwest': Lwest,
            'Lsouth': Lsouth, 'Lnorth': Lnorth, 'Least': Least
        })

        # RSL arrays
        zarray = np.zeros(30, dtype=np.float64)
        dataoutLineURSL = np.zeros(30, dtype=np.float64)
        dataoutLineTRSL = np.zeros(30, dtype=np.float64)
        dataoutLineqRSL = np.zeros(30, dtype=np.float64)
        acc.get_stebbs_state_rsl(
            self._state, zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL
        )
        result['zarray'] = zarray.copy()
        result['dataoutLineURSL'] = dataoutLineURSL.copy()
        result['dataoutLineTRSL'] = dataoutLineTRSL.copy()
        result['dataoutLineqRSL'] = dataoutLineqRSL.copy()

        # Building envelope temperatures
        (DeepSoilTemperature, OutdoorAirStartTemperature,
         IndoorAirStartTemperature, IndoorMassStartTemperature,
         WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature,
         RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature,
         WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature,
         GroundFloorIndoorSurfaceTemperature,
         GroundFloorOutdoorSurfaceTemperature) = acc.get_stebbs_state_temps_envelope(self._state)
        result.update({
            'DeepSoilTemperature': DeepSoilTemperature,
            'OutdoorAirStartTemperature': OutdoorAirStartTemperature,
            'IndoorAirStartTemperature': IndoorAirStartTemperature,
            'IndoorMassStartTemperature': IndoorMassStartTemperature,
            'WallIndoorSurfaceTemperature': WallIndoorSurfaceTemperature,
            'WallOutdoorSurfaceTemperature': WallOutdoorSurfaceTemperature,
            'RoofIndoorSurfaceTemperature': RoofIndoorSurfaceTemperature,
            'RoofOutdoorSurfaceTemperature': RoofOutdoorSurfaceTemperature,
            'WindowIndoorSurfaceTemperature': WindowIndoorSurfaceTemperature,
            'WindowOutdoorSurfaceTemperature': WindowOutdoorSurfaceTemperature,
            'GroundFloorIndoorSurfaceTemperature': GroundFloorIndoorSurfaceTemperature,
            'GroundFloorOutdoorSurfaceTemperature': GroundFloorOutdoorSurfaceTemperature,
        })

        # Water tank temperatures
        (WaterTankTemperature, InternalWallWaterTankTemperature,
         ExternalWallWaterTankTemperature, MainsWaterTemperature,
         DomesticHotWaterTemperatureInUseInBuilding,
         InternalWallDHWVesselTemperature,
         ExternalWallDHWVesselTemperature) = acc.get_stebbs_state_temps_water(self._state)
        result.update({
            'WaterTankTemperature': WaterTankTemperature,
            'InternalWallWaterTankTemperature': InternalWallWaterTankTemperature,
            'ExternalWallWaterTankTemperature': ExternalWallWaterTankTemperature,
            'MainsWaterTemperature': MainsWaterTemperature,
            'DomesticHotWaterTemperatureInUseInBuilding': DomesticHotWaterTemperatureInUseInBuilding,
            'InternalWallDHWVesselTemperature': InternalWallDHWVesselTemperature,
            'ExternalWallDHWVesselTemperature': ExternalWallDHWVesselTemperature,
        })

        # Storage heat flux
        QS_stebbs = acc.get_stebbs_state_qs(self._state)
        result['QS_stebbs'] = QS_stebbs

        return result

    def set_stebbs_state(self, d: Dict[str, Any]) -> None:
        """Set STEBBS state values."""
        # Shortwave radiation
        acc.set_stebbs_state_krad(
            self._state,
            d.get('Kdown2d', 0.0), d.get('Kup2d', 0.0), d.get('Kwest', 0.0),
            d.get('Ksouth', 0.0), d.get('Knorth', 0.0), d.get('Keast', 0.0)
        )

        # Longwave radiation
        acc.set_stebbs_state_lrad(
            self._state,
            d.get('Ldown2d', 0.0), d.get('Lup2d', 0.0), d.get('Lwest', 0.0),
            d.get('Lsouth', 0.0), d.get('Lnorth', 0.0), d.get('Least', 0.0)
        )

        # RSL arrays
        if any(k in d for k in ['zarray', 'dataoutLineURSL', 'dataoutLineTRSL', 'dataoutLineqRSL']):
            zarray = np.array(d.get('zarray', np.full(30, -999.0)), dtype=np.float64)
            dataoutLineURSL = np.array(d.get('dataoutLineURSL', np.full(30, -999.0)), dtype=np.float64)
            dataoutLineTRSL = np.array(d.get('dataoutLineTRSL', np.full(30, -999.0)), dtype=np.float64)
            dataoutLineqRSL = np.array(d.get('dataoutLineqRSL', np.full(30, -999.0)), dtype=np.float64)
            acc.set_stebbs_state_rsl(
                self._state, zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL
            )

        # Building envelope temperatures
        acc.set_stebbs_state_temps_envelope(
            self._state,
            d.get('DeepSoilTemperature', 0.0),
            d.get('OutdoorAirStartTemperature', 0.0),
            d.get('IndoorAirStartTemperature', 0.0),
            d.get('IndoorMassStartTemperature', 0.0),
            d.get('WallIndoorSurfaceTemperature', 0.0),
            d.get('WallOutdoorSurfaceTemperature', 0.0),
            d.get('RoofIndoorSurfaceTemperature', 0.0),
            d.get('RoofOutdoorSurfaceTemperature', 0.0),
            d.get('WindowIndoorSurfaceTemperature', 0.0),
            d.get('WindowOutdoorSurfaceTemperature', 0.0),
            d.get('GroundFloorIndoorSurfaceTemperature', 0.0),
            d.get('GroundFloorOutdoorSurfaceTemperature', 0.0),
        )

        # Water tank temperatures
        acc.set_stebbs_state_temps_water(
            self._state,
            d.get('WaterTankTemperature', 0.0),
            d.get('InternalWallWaterTankTemperature', 0.0),
            d.get('ExternalWallWaterTankTemperature', 0.0),
            d.get('MainsWaterTemperature', 0.0),
            d.get('DomesticHotWaterTemperatureInUseInBuilding', 0.0),
            d.get('InternalWallDHWVesselTemperature', 0.0),
            d.get('ExternalWallDHWVesselTemperature', 0.0),
        )

        # Storage heat flux
        if 'QS_stebbs' in d:
            acc.set_stebbs_state_qs(self._state, d['QS_stebbs'])

    # =========================================================
    # STEBBS building temperatures (runtime state)
    # =========================================================

    def get_building_temps(self, bldg_idx: int = 1) -> Dict[str, np.ndarray]:
        """Get STEBBS building runtime temperatures.

        Parameters
        ----------
        bldg_idx : int
            Building index (1-based)

        Returns
        -------
        dict
            Dictionary with Textroof_C and Textwall_C arrays
        """
        Textroof_C = np.zeros(self._nlayer, dtype=np.float64)
        Textwall_C = np.zeros(self._nlayer, dtype=np.float64)
        acc.get_stebbs_building_temps(
            self._state, bldg_idx, self._nlayer, Textroof_C, Textwall_C
        )
        return {
            'Textroof_C': Textroof_C.copy(),
            'Textwall_C': Textwall_C.copy()
        }

    def set_building_temps(
        self,
        bldg_idx: int,
        Textroof_C: np.ndarray,
        Textwall_C: np.ndarray
    ) -> None:
        """Set STEBBS building runtime temperatures.

        Parameters
        ----------
        bldg_idx : int
            Building index (1-based)
        Textroof_C : np.ndarray
            Roof external surface temperatures
        Textwall_C : np.ndarray
            Wall external surface temperatures
        """
        Textroof_C = np.array(Textroof_C, dtype=np.float64)
        Textwall_C = np.array(Textwall_C, dtype=np.float64)
        acc.set_stebbs_building_temps(
            self._state, bldg_idx, len(Textroof_C), Textroof_C, Textwall_C
        )


def run_supy_dts_tstep(
    timer: dts.SUEWS_TIMER,
    config: dts.SUEWS_CONFIG,
    site: dts.SUEWS_SITE,
    state: dts.SUEWS_STATE,
    forcing: dts.SUEWS_FORCING,
    debug: bool = False
) -> Tuple[dts.SUEWS_STATE, Dict[str, Any]]:
    """Run a single SUEWS timestep using DTS objects.

    Parameters
    ----------
    timer : dts.SUEWS_TIMER
        Timer with temporal information for this timestep
    config : dts.SUEWS_CONFIG
        Model configuration
    site : dts.SUEWS_SITE
        Site parameters
    state : dts.SUEWS_STATE
        Model state (modified in place)
    forcing : dts.SUEWS_FORCING
        Meteorological forcing
    debug : bool
        Whether to return debug information

    Returns
    -------
    tuple
        (updated_state, output_dict) where output_dict contains
        simulation results for this timestep
    """
    # Create debug state if needed
    debug_state = dts.SUEWS_DEBUG() if debug else None
    if debug_state is not None:
        dts.init_suews_debug(debug_state)

    # Call the main calculation
    # Note: timer is the first positional argument (appears as 'self' in signature)
    output_line = sd.suews_cal_main(
        timer,
        forcing,
        config,
        site,
        state,
        debug_state
    )

    # Extract output values
    output_dict = {}

    # Extract main SUEWS outputs from dataoutlinesuews array
    if output_line is not None:
        suews_data = output_line.dataoutlinesuews
        if suews_data is not None and len(suews_data) > 0:
            output_dict['dataoutlinesuews'] = suews_data.copy()

        # Extract datetime components
        dt_line = output_line.datetimeline
        if dt_line is not None and len(dt_line) > 0:
            output_dict['datetimeline'] = dt_line.copy()

    # Also extract key state values
    output_dict.update(extract_heat_state(state))

    return state, output_dict


# Convenience function for quick testing
def test_dts_interface():
    """Test the DTS interface by running basic operations.

    Returns True if all tests pass.
    """
    try:
        # Create DTS objects
        config = create_suews_config()
        state = create_suews_state(nlayer=5, ndepth=5)
        forcing = create_suews_forcing()

        # Test dimension getters
        nlayer, ndepth, nsurf = acc.get_heat_state_dims(state)
        assert nlayer == 5, f"Expected nlayer=5, got {nlayer}"
        assert ndepth == 5, f"Expected ndepth=5, got {ndepth}"
        assert nsurf == 7, f"Expected nsurf=7, got {nsurf}"

        # Test scalar getters
        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(state)
        assert qh == 0.0, f"Expected qh=0.0, got {qh}"

        # Test scalar setters
        acc.set_heat_state_scalars(state, 100.0, 50.0, 30.0, 200.0, 10.0, 25.0)
        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(state)
        assert qh == 100.0, f"Expected qh=100.0, got {qh}"
        assert tsurf == 25.0, f"Expected tsurf=25.0, got {tsurf}"

        print("All DTS interface tests passed!")
        return True

    except Exception as e:
        print(f"DTS interface test failed: {e}")
        return False


if __name__ == "__main__":
    test_dts_interface()
