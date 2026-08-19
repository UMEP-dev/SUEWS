"""Output variable definitions for SUEWS.

This module provides Python/Pydantic definitions of SUEWS output metadata. The
compiled model produces numerical arrays whose labels and aggregation behaviour
are projected from this registry.

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

from .beers_vars import BEERS_VARIABLES as BEERS_VARIABLES
from .bl_vars import BL_VARIABLES as BL_VARIABLES
from .contract import (
    OUTPUT_GROUP_SCOPES,
    OutputContractScope,
    output_contract_json_schema,
)
from .dailystate_vars import DAILYSTATE_VARIABLES as DAILYSTATE_VARIABLES
from .datetime_vars import DATETIME_VARIABLES as DATETIME_VARIABLES
from .debug_vars import DEBUG_VARIABLES as DEBUG_VARIABLES
from .ehc_vars import EHC_VARIABLES as EHC_VARIABLES
from .estm_vars import ESTM_VARIABLES as ESTM_VARIABLES
from .nhood_vars import NHOOD_VARIABLES as NHOOD_VARIABLES
from .registry import OUTPUT_REGISTRY, get_output_contract_catalogue
from .rsl_vars import RSL_VARIABLES as RSL_VARIABLES
from .snow_vars import SNOW_VARIABLES as SNOW_VARIABLES
from .spartacus_vars import SPARTACUS_VARIABLES as SPARTACUS_VARIABLES
from .stebbs_vars import STEBBS_VARIABLES as STEBBS_VARIABLES
from .suews_vars import SUEWS_VARIABLES as SUEWS_VARIABLES
from .variables import (
    AggregationMethod,
    OutputGroup,
    OutputLevel,
    OutputVariable,
    OutputVariableRegistry,
)

# The per-group lists remain explicit package attributes for compatibility.
# They are intentionally not part of the star-import API in ``__all__``.
__all__ = [
    "OUTPUT_GROUP_SCOPES",
    "OUTPUT_REGISTRY",
    "AggregationMethod",
    "OutputContractScope",
    "OutputGroup",
    "OutputLevel",
    "OutputVariable",
    "OutputVariableRegistry",
    "get_output_contract_catalogue",
    "output_contract_json_schema",
]
