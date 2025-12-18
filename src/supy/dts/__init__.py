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
build_output_dataframe : Build DataFrame from output dictionaries
"""

from ._core import (
    create_suews_config,
    create_suews_state,
    create_suews_site,
    create_suews_forcing,
    create_suews_timer,
    create_output_line,
)

from ._populate import (
    populate_config_from_pydantic,
    populate_site_from_pydantic,
    populate_state_from_pydantic,
    populate_forcing_from_row,
    populate_timer_from_datetime,
    populate_roughnessstate,
    populate_atmstate,
    populate_ohmstate_defaults,
    populate_storedrainprm,
)

from ._extract import (
    extract_output_line_to_dict,
    build_output_dataframe,
    build_full_output_dataframe,
)

from ._runner import run_dts

__all__ = [
    # Main runner
    "run_dts",
    # Factory functions
    "create_suews_config",
    "create_suews_state",
    "create_suews_site",
    "create_suews_forcing",
    "create_suews_timer",
    "create_output_line",
    # Population functions
    "populate_config_from_pydantic",
    "populate_site_from_pydantic",
    "populate_state_from_pydantic",
    "populate_forcing_from_row",
    "populate_timer_from_datetime",
    "populate_roughnessstate",
    "populate_atmstate",
    "populate_ohmstate_defaults",
    "populate_storedrainprm",
    # Extraction functions
    "extract_output_line_to_dict",
    "build_output_dataframe",
    "build_full_output_dataframe",
]
