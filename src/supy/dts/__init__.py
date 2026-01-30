"""DTS (Derived Type Structure) interface for SUEWS.

This package provides direct access to SUEWS Fortran derived types
through f90wrap, eliminating the intermediate DataFrame conversion layer.

Main Functions
--------------
run_dts : Run SUEWS simulation using DTS interface

Factory Functions
-----------------
create_suews_config : Create SUEWS_CONFIG object
create_suews_state : Create and allocate SUEWS_STATE object
create_suews_site : Create and allocate SUEWS_SITE object
create_suews_forcing : Create SUEWS_FORCING object
create_suews_timer : Create SUEWS_TIMER object
create_output_line : Create output_line object

Population Functions
--------------------
populate_config_from_pydantic : Populate SUEWS_CONFIG from Model
populate_site_from_pydantic : Populate SUEWS_SITE from Site
populate_state_from_pydantic : Populate SUEWS_STATE from InitialStates
populate_forcing_from_row : Populate SUEWS_FORCING from DataFrame row
populate_timer_from_datetime : Populate SUEWS_TIMER from datetime

Extraction Functions
--------------------
extract_output_line_to_dict : Extract output arrays to dictionary
build_output_dataframe_from_block : Build DataFrame from batch output array
build_full_output_dataframe : Build DataFrame with all output groups
extract_state_from_dts : Extract final state to Pydantic InitialStates

Note
----
DTS features require a full build with type wrappers. If using a fast build
(``make dev``), DTS functions will raise RuntimeError with instructions to
rebuild with ``make clean && make dev-dts``.
"""

# Use shared DTS availability check (see _env.py for implementation details)
# This ensures consistent check logic across all modules that need it
from .._env import check_dts_available as _check_dts_available
from .._env import _init_dts_check

# Check DTS availability (returns bool)
_DTS_AVAILABLE = _init_dts_check()


# Conditionally import DTS functions or provide stubs
if _DTS_AVAILABLE:
    # Full build - import actual implementations
    from ._core import (
        create_output_line,
        create_suews_config,
        create_suews_forcing,
        create_suews_site,
        create_suews_state,
        create_suews_timer,
    )
    from ._extract import (
        build_full_output_dataframe,
        build_output_dataframe_from_block,
        extract_output_line_to_dict,
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
    from ._runner import run_dts, run_dts_multi

else:
    # Fast build - provide stub functions that raise clear errors

    def create_output_line():
        """Stub: DTS not available."""
        _check_dts_available()

    def create_suews_config():
        """Stub: DTS not available."""
        _check_dts_available()

    def create_suews_forcing():
        """Stub: DTS not available."""
        _check_dts_available()

    def create_suews_site(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def create_suews_state(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def create_suews_timer():
        """Stub: DTS not available."""
        _check_dts_available()

    def build_full_output_dataframe(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def build_output_dataframe_from_block(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def extract_output_line_to_dict(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def extract_state_from_dts(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_atmstate(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_config_from_pydantic(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_forcing_from_row(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_ohmstate_defaults(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_roughnessstate(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_site_from_pydantic(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_state_from_pydantic(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_storedrainprm(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def populate_timer_from_datetime(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def run_dts(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()

    def run_dts_multi(*args, **kwargs):
        """Stub: DTS not available."""
        _check_dts_available()


__all__ = [
    "build_full_output_dataframe",
    "build_output_dataframe_from_block",
    "create_output_line",
    "create_suews_config",
    "create_suews_forcing",
    "create_suews_site",
    "create_suews_state",
    "create_suews_timer",
    "extract_output_line_to_dict",
    "extract_state_from_dts",
    "populate_atmstate",
    "populate_config_from_pydantic",
    "populate_forcing_from_row",
    "populate_ohmstate_defaults",
    "populate_roughnessstate",
    "populate_site_from_pydantic",
    "populate_state_from_pydantic",
    "populate_storedrainprm",
    "populate_timer_from_datetime",
    "run_dts",
    "run_dts_multi",
    # Utility for checking availability
    "_DTS_AVAILABLE",
    "_check_dts_available",
]
