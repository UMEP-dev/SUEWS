"""DTS state bootstrap from configuration.

Converts InitialStates from YAML config to DTS state via accessor functions.
"""

from typing import TYPE_CHECKING

import numpy as np

from ..supy_driver import module_ctrl_type as dts
from .. import _state_accessors as acc

if TYPE_CHECKING:
    from ..data_model.core.state import InitialStates


def _extract_value(val):
    """Extract raw value from RefValue wrapper or return as-is."""
    if hasattr(val, "value"):
        return val.value
    return val


def bootstrap_state_from_config(
    state: dts.SUEWS_STATE,
    initial_states: "InitialStates",
    nlayer: int = 5,
    ndepth: int = 5,
    nsurf: int = 7,
    nvegsurf: int = 3,
) -> None:
    """Bootstrap DTS state from config InitialStates.

    Populates nested DTS state arrays using accessor functions.

    Parameters
    ----------
    state : dts.SUEWS_STATE
        Allocated state object to populate
    initial_states : InitialStates
        Initial states from YAML configuration
    nlayer : int
        Number of vertical layers (roof/wall)
    ndepth : int
        Number of thermal depth levels
    nsurf : int
        Number of surface types (default 7)
    nvegsurf : int
        Number of vegetation surfaces (default 3: evetr, dectr, grass)

    Notes
    -----
    Surface indexing (SUEWS convention):
        0=Paved, 1=Bldgs, 2=EveTr, 3=DecTr, 4=Grass, 5=BSoil, 6=Water

    Vegetation subset (for LAI/phenology):
        0=EveTr, 1=DecTr, 2=Grass
    """
    # Surface order matching config attributes
    surfaces = [
        initial_states.paved,
        initial_states.bldgs,
        initial_states.evetr,
        initial_states.dectr,
        initial_states.grass,
        initial_states.bsoil,
        initial_states.water,
    ]

    # =========================================================================
    # Heat State: Temperature arrays
    # =========================================================================
    # Build temp_surf array (nsurf x ndepth)
    temp_surf = np.zeros((nsurf, ndepth), dtype=np.float64, order="F")
    for i, surf in enumerate(surfaces):
        temp_list = _extract_value(surf.temperature)
        for j, temp in enumerate(temp_list[:ndepth]):
            temp_surf[i, j] = temp

    # Build temp_roof array (nlayer x ndepth)
    temp_roof = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
    if initial_states.roofs:
        for i, roof in enumerate(initial_states.roofs[:nlayer]):
            temp_list = _extract_value(roof.temperature)
            for j, temp in enumerate(temp_list[:ndepth]):
                temp_roof[i, j] = temp

    # Build temp_wall array (nlayer x ndepth)
    temp_wall = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
    if initial_states.walls:
        for i, wall in enumerate(initial_states.walls[:nlayer]):
            temp_list = _extract_value(wall.temperature)
            for j, temp in enumerate(temp_list[:ndepth]):
                temp_wall[i, j] = temp

    acc.set_heat_state_temp(state, nlayer, ndepth, nsurf, temp_roof, temp_wall, temp_surf)

    # =========================================================================
    # Heat State: Surface temperatures
    # =========================================================================
    tsfc_surf = np.zeros(nsurf, dtype=np.float64)
    for i, surf in enumerate(surfaces):
        if surf.tsfc is not None:
            tsfc_surf[i] = _extract_value(surf.tsfc)

    tsfc_roof = np.zeros(nlayer, dtype=np.float64)
    if initial_states.roofs:
        for i, roof in enumerate(initial_states.roofs[:nlayer]):
            if roof.tsfc is not None:
                tsfc_roof[i] = _extract_value(roof.tsfc)

    tsfc_wall = np.zeros(nlayer, dtype=np.float64)
    if initial_states.walls:
        for i, wall in enumerate(initial_states.walls[:nlayer]):
            if wall.tsfc is not None:
                tsfc_wall[i] = _extract_value(wall.tsfc)

    acc.set_heat_state_tsfc(state, nlayer, nsurf, tsfc_roof, tsfc_wall, tsfc_surf)

    # =========================================================================
    # Hydro State: Soil moisture and surface wetness
    # =========================================================================
    # Note: Site surface properties (soilstorecap, statelimit) must be set
    # BEFORE calling this function to avoid "total SoilState > capacity" errors.
    # This is done in run_dts() via acc.set_site_soil_params().

    soilstore_surf = np.zeros(nsurf, dtype=np.float64)
    soilstore_roof = np.zeros(nlayer, dtype=np.float64)
    soilstore_wall = np.zeros(nlayer, dtype=np.float64)
    state_surf = np.zeros(nsurf, dtype=np.float64)
    state_roof = np.zeros(nlayer, dtype=np.float64)
    state_wall = np.zeros(nlayer, dtype=np.float64)

    for i, surf in enumerate(surfaces):
        if hasattr(surf, 'soilstore') and surf.soilstore is not None:
            soilstore_surf[i] = _extract_value(surf.soilstore)
        if hasattr(surf, 'state') and surf.state is not None:
            state_surf[i] = _extract_value(surf.state)

    # Roof/wall soilstore and wetness (initialise to zero - no soil on roofs/walls)
    acc.set_hydro_state_soilstore(state, nlayer, soilstore_roof, soilstore_wall, soilstore_surf)
    acc.set_hydro_state_wetness(state, nlayer, state_roof, state_wall, state_surf)

    # =========================================================================
    # Snow State: Per-surface arrays
    # =========================================================================
    snowpack = np.zeros(nsurf, dtype=np.float64)
    snowfrac = np.zeros(nsurf, dtype=np.float64)
    snowdens = np.full(nsurf, 100.0, dtype=np.float64)  # Default density
    icefrac = np.zeros(nsurf, dtype=np.float64)

    for i, surf in enumerate(surfaces):
        if surf.snowpack is not None:
            snowpack[i] = _extract_value(surf.snowpack)
        if surf.snowfrac is not None:
            snowfrac[i] = _extract_value(surf.snowfrac)
        if surf.snowdens is not None:
            val = _extract_value(surf.snowdens)
            if val > 0:  # Only override if non-zero
                snowdens[i] = val
        if surf.icefrac is not None:
            icefrac[i] = _extract_value(surf.icefrac)

    acc.set_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)

    # Snow albedo (scalar)
    snowalb = _extract_value(initial_states.snowalb)
    acc.set_snow_state_scalars(state, snowalb, 0.0, 0.0, 0.0)

    # =========================================================================
    # Phenology State: LAI, GDD, SDD for vegetation surfaces
    # =========================================================================
    # Vegetation surfaces: evetr (idx=2), dectr (idx=3), grass (idx=4)
    # LAI array order: evetr=0, dectr=1, grass=2
    lai_id = np.zeros(nvegsurf, dtype=np.float64)
    gdd_id = np.zeros(nvegsurf, dtype=np.float64)
    sdd_id = np.zeros(nvegsurf, dtype=np.float64)
    veg_surfaces = [initial_states.evetr, initial_states.dectr, initial_states.grass]
    for i, veg in enumerate(veg_surfaces):
        if hasattr(veg, "lai_id") and veg.lai_id is not None:
            lai_id[i] = _extract_value(veg.lai_id)
        if hasattr(veg, "gdd_id") and veg.gdd_id is not None:
            gdd_id[i] = _extract_value(veg.gdd_id)
        if hasattr(veg, "sdd_id") and veg.sdd_id is not None:
            sdd_id[i] = _extract_value(veg.sdd_id)

    acc.set_phen_state_lai(state, lai_id, gdd_id, sdd_id)

    # =========================================================================
    # Phenology State: Albedo for all surfaces
    # =========================================================================
    alb_id = np.zeros(nsurf, dtype=np.float64)
    for i, veg in enumerate(veg_surfaces):
        if hasattr(veg, "alb_id") and veg.alb_id is not None:
            # Vegetation surfaces start at index 2
            alb_id[i + 2] = _extract_value(veg.alb_id)

    acc.set_phen_state_alb(state, alb_id)

    # =========================================================================
    # Anthro State: Heating/Cooling Degree Days
    # =========================================================================
    hdd_list = initial_states.hdd_id.to_list()
    hdd_array = np.array(hdd_list, dtype=np.float64)
    acc.set_anthro_state_hdd(state, hdd_array)
