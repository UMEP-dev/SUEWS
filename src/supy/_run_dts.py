"""DTS-based SUEWS simulation runner.

This module provides backwards compatibility by re-exporting all public
functions from the dts sub-package.

The implementation has been moved to the dts/ sub-package for better
modularity and maintainability. Import directly from supy.dts for
new code.

Deprecated
----------
This module is deprecated. Use supy.dts instead:

    from supy.dts import create_suews_state, StateAccessor
"""

# Re-export everything from dts package for backwards compatibility
from .dts import (
    # Core
    DTSConfig,
    create_suews_config,
    create_suews_state,
    create_suews_forcing,
    create_suews_timer,
    # Populate
    populate_timer_from_datetime,
    populate_config_from_dict,
    populate_forcing_from_row,
    populate_state_from_config,
    # Extract
    extract_heat_state,
    extract_hydro_state,
    extract_snow_state,
    # Site
    create_suews_site,
    populate_site_from_dict,
    extract_site_params,
    # State accessor
    StateAccessor,
    # Runner
    run_supy_dts_tstep,
    test_dts_interface,
)

__all__ = [
    "DTSConfig",
    "create_suews_config",
    "create_suews_state",
    "create_suews_forcing",
    "create_suews_timer",
    "populate_timer_from_datetime",
    "populate_config_from_dict",
    "populate_forcing_from_row",
    "populate_state_from_config",
    "extract_heat_state",
    "extract_hydro_state",
    "extract_snow_state",
    "create_suews_site",
    "populate_site_from_dict",
    "extract_site_params",
    "StateAccessor",
    "run_supy_dts_tstep",
    "test_dts_interface",
]

if __name__ == "__main__":
    test_dts_interface()
