"""DTS-based SUEWS simulation interface.

This sub-package provides a simplified interface to run SUEWS simulations using
Derived Type Structures (DTS) directly, eliminating the df_state DataFrame
intermediate layer.

The approach uses f90wrap-generated DTS objects and accessor functions to
interact with the Fortran SUEWS kernel without complex DataFrame conversions.

Modules
-------
_core : DTSConfig dataclass and DTS factory functions
_populate : Functions to populate DTS from Python data
_extract : Functions to extract state from DTS to dicts
_site : SUEWS_SITE creation and population
_state_accessor : StateAccessor class for full state management
_runner : Simulation execution functions
_bootstrap : Bootstrap DTS state from config InitialStates

Examples
--------
>>> from supy.dts import create_suews_state, StateAccessor
>>> state = create_suews_state(nlayer=5, ndepth=5)
>>> accessor = StateAccessor(state)
>>> state_dict = accessor.to_dict()
"""

# Core DTS creation functions
from ._core import (
    DTSConfig,
    create_suews_config,
    create_suews_state,
    create_suews_forcing,
    create_suews_timer,
)

# Population functions
from ._populate import (
    populate_timer_from_datetime,
    populate_config_from_dict,
    populate_forcing_from_row,
    populate_state_from_config,
)

# Extraction functions
from ._extract import (
    extract_heat_state,
    extract_hydro_state,
    extract_snow_state,
)

# Site functions
from ._site import (
    create_suews_site,
    populate_site_from_dict,
    extract_site_params,
)

# State accessor class
from ._state import StateAccessor

# Runner functions
from ._runner import (
    run_supy_dts_tstep,
    test_dts_interface,
)

# Bootstrap functions
from ._bootstrap import bootstrap_state_from_config

__all__ = [
    # Core
    "DTSConfig",
    "create_suews_config",
    "create_suews_state",
    "create_suews_forcing",
    "create_suews_timer",
    # Populate
    "populate_timer_from_datetime",
    "populate_config_from_dict",
    "populate_forcing_from_row",
    "populate_state_from_config",
    # Extract
    "extract_heat_state",
    "extract_hydro_state",
    "extract_snow_state",
    # Site
    "create_suews_site",
    "populate_site_from_dict",
    "extract_site_params",
    # State accessor
    "StateAccessor",
    # Runner
    "run_supy_dts_tstep",
    "test_dts_interface",
    # Bootstrap
    "bootstrap_state_from_config",
]
