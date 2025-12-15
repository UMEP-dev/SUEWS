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
from .supy_driver import module_ctrl_accessor as acc


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


def run_supy_dts_tstep(
    config: dts.SUEWS_CONFIG,
    site: dts.SUEWS_SITE,
    state: dts.SUEWS_STATE,
    forcing: dts.SUEWS_FORCING,
    debug: bool = False
) -> Tuple[dts.SUEWS_STATE, Dict[str, Any]]:
    """Run a single SUEWS timestep using DTS objects.

    Parameters
    ----------
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

    # Call the main calculation
    # Note: This calls suews_cal_main which operates on the DTS objects
    output_line = sd.suews_cal_main(
        forcing=forcing,
        config=config,
        siteinfo=site,
        modstate=state,
        debugstate=debug_state
    )

    # Extract output values
    output_dict = {}

    # Extract from output_line (the return value contains simulation results)
    if output_line is not None:
        for attr in dir(output_line):
            if not attr.startswith('_') and not callable(getattr(output_line, attr)):
                try:
                    output_dict[attr] = getattr(output_line, attr)
                except (AttributeError, TypeError):
                    pass

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
