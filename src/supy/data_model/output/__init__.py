"""Output variable definitions for SUEWS.

This module provides Python/Pydantic-first definitions of all SUEWS output variables,
replacing the previous Fortran-first runtime extraction approach.

The registry pattern provides:
- Type-safe variable definitions
- Self-documenting metadata
- Easy extensibility
- Better IDE support

Example usage:
    >>> from supy.data_model.output import OUTPUT_REGISTRY
    >>> # Get all SUEWS core variables
    >>> suews_vars = OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS)
    >>> # Get default output level variables
    >>> default_vars = OUTPUT_REGISTRY.by_level(OutputLevel.DEFAULT)
    >>> # Get aggregation rules for resampling
    >>> agg_rules = OUTPUT_REGISTRY.get_aggregation_rules()
"""

from .variables import (
    OutputVariable,
    OutputVariableRegistry,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)
from .datetime_vars import DATETIME_VARIABLES
from .suews_vars import SUEWS_VARIABLES
from .snow_vars import SNOW_VARIABLES
from .estm_vars import ESTM_VARIABLES
from .rsl_vars import RSL_VARIABLES
from .dailystate_vars import DAILYSTATE_VARIABLES
from .bl_vars import BL_VARIABLES
from .beers_vars import BEERS_VARIABLES
from .debug_vars import DEBUG_VARIABLES


# Assemble the global registry from all variable modules
OUTPUT_REGISTRY = OutputVariableRegistry(
    variables=(
        DATETIME_VARIABLES +
        SUEWS_VARIABLES +
        SNOW_VARIABLES +
        ESTM_VARIABLES +
        RSL_VARIABLES +
        DAILYSTATE_VARIABLES +
        BL_VARIABLES +
        BEERS_VARIABLES +
        DEBUG_VARIABLES
    )
)


__all__ = [
    "OUTPUT_REGISTRY",
    "OutputVariable",
    "OutputVariableRegistry",
    "AggregationMethod",
    "OutputGroup",
    "OutputLevel",
]
