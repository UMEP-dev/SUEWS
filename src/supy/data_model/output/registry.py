"""Assemble the SUEWS output registry and its contract projection."""

from functools import cache

from .beers_vars import BEERS_VARIABLES
from .bl_vars import BL_VARIABLES
from .contract import OutputContractCatalogue, _project_output_contract
from .dailystate_vars import DAILYSTATE_VARIABLES
from .datetime_vars import DATETIME_VARIABLES
from .debug_vars import DEBUG_VARIABLES
from .ehc_vars import EHC_VARIABLES
from .estm_vars import ESTM_VARIABLES
from .nhood_vars import NHOOD_VARIABLES
from .rsl_vars import RSL_VARIABLES
from .snow_vars import SNOW_VARIABLES
from .spartacus_vars import SPARTACUS_VARIABLES
from .stebbs_vars import STEBBS_VARIABLES
from .suews_vars import SUEWS_VARIABLES
from .variables import OutputVariableRegistry

OUTPUT_REGISTRY = OutputVariableRegistry(
    variables=(
        DATETIME_VARIABLES
        + SUEWS_VARIABLES
        + SNOW_VARIABLES
        + ESTM_VARIABLES
        + RSL_VARIABLES
        + DAILYSTATE_VARIABLES
        + BL_VARIABLES
        + BEERS_VARIABLES
        + DEBUG_VARIABLES
        + EHC_VARIABLES
        + SPARTACUS_VARIABLES
        + STEBBS_VARIABLES
        + NHOOD_VARIABLES
    )
)


@cache
def get_output_contract_catalogue() -> OutputContractCatalogue:
    """Return the cached contract projection of ``OUTPUT_REGISTRY``."""
    return _project_output_contract(OUTPUT_REGISTRY)


__all__ = ["OUTPUT_REGISTRY", "get_output_contract_catalogue"]
