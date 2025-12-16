"""DTS extraction functions.

Functions for extracting state values from DTS objects into
Python dictionaries.
"""

from typing import Any, Dict

import numpy as np

from ..supy_driver import module_ctrl_type as dts
from .. import _state_accessors as acc


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
    result["qh"] = qh
    result["qe"] = qe
    result["qs"] = qs
    result["qn"] = qn
    result["qf"] = qf
    result["tsurf"] = tsurf

    if nlayer > 0 and ndepth > 0:
        # Get temperature arrays
        temp_roof = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        temp_wall = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        temp_surf = np.zeros((nsurf, ndepth), dtype=np.float64, order="F")
        acc.get_heat_state_temp(
            state, nlayer, ndepth, nsurf, temp_roof, temp_wall, temp_surf
        )
        result["temp_roof"] = temp_roof.copy()
        result["temp_wall"] = temp_wall.copy()
        result["temp_surf"] = temp_surf.copy()

        # Get surface temperatures
        tsfc_roof = np.zeros(nlayer, dtype=np.float64)
        tsfc_wall = np.zeros(nlayer, dtype=np.float64)
        tsfc_surf = np.zeros(nsurf, dtype=np.float64)
        acc.get_heat_state_tsfc(state, nlayer, nsurf, tsfc_roof, tsfc_wall, tsfc_surf)
        result["tsfc_roof"] = tsfc_roof.copy()
        result["tsfc_wall"] = tsfc_wall.copy()
        result["tsfc_surf"] = tsfc_surf.copy()

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
    result["smd"] = smd
    result["runoff_per_tstep"] = runoff
    result["ev_per_tstep"] = evap
    result["drain_per_tstep"] = drain

    if nlayer > 0:
        # Get soil store arrays
        soilstore_roof = np.zeros(nlayer, dtype=np.float64)
        soilstore_wall = np.zeros(nlayer, dtype=np.float64)
        soilstore_surf = np.zeros(nsurf, dtype=np.float64)
        acc.get_hydro_state_soilstore(
            state, nlayer, soilstore_roof, soilstore_wall, soilstore_surf
        )
        result["soilstore_roof"] = soilstore_roof.copy()
        result["soilstore_wall"] = soilstore_wall.copy()
        result["soilstore_surf"] = soilstore_surf.copy()

        # Get wetness state arrays
        state_roof = np.zeros(nlayer, dtype=np.float64)
        state_wall = np.zeros(nlayer, dtype=np.float64)
        state_surf = np.zeros(nsurf, dtype=np.float64)
        acc.get_hydro_state_wetness(state, nlayer, state_roof, state_wall, state_surf)
        result["state_roof"] = state_roof.copy()
        result["state_wall"] = state_wall.copy()
        result["state_surf"] = state_surf.copy()

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
    result["snowalb"] = snowalb
    result["swe"] = swe
    result["mwh"] = mwh
    result["qm"] = qm

    # Get array values
    snowpack = np.zeros(nsurf, dtype=np.float64)
    snowfrac = np.zeros(nsurf, dtype=np.float64)
    snowdens = np.zeros(nsurf, dtype=np.float64)
    icefrac = np.zeros(nsurf, dtype=np.float64)
    acc.get_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)
    result["snowpack"] = snowpack.copy()
    result["snowfrac"] = snowfrac.copy()
    result["snowdens"] = snowdens.copy()
    result["icefrac"] = icefrac.copy()

    return result
