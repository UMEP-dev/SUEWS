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
"""

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
from ._runner import run_dts

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
]
