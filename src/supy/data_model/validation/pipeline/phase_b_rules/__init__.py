"""Phase B rule registration.

Rules are imported explicitly for their registration side effects. The execution
order is kept in one place so scientific validation reports stay deterministic.
"""

from .rules_core import (
    RulesRegistry,
    ValidationContext,
    ValidationResult,
)

from . import physics_rules as _physics_rules
from . import other_rules as _other_rules
from . import stebbs_rules as _stebbs_rules
from .other_rules import validate_irrigation_doy
from .physics_rules import (
    validate_rslmethod_dependency,
    validate_smdmethod_dependency,
    validate_storageheatmethod_dependency,
)


DEFAULT_RULE_ORDER = (
    "physics_params",
    "option_dependencies",
    "land_cover",
    "geographic",
    "irrigation",
    "archetype_properties",
    "occupants_metabolism",
    "daylight_control",
    "stebbs_props",
    "setpoint",
    "rcmethod",
    "same_albedo",
    "same_emissivity",
    "forcing_height",
    "veg_albedo",
)

SFR_FRACTION_TOL = _other_rules.SFR_FRACTION_TOL

__all__ = (
    "DEFAULT_RULE_ORDER",
    "RulesRegistry",
    "SFR_FRACTION_TOL",
    "ValidationContext",
    "ValidationResult",
    "validate_irrigation_doy",
    "validate_rslmethod_dependency",
    "validate_smdmethod_dependency",
    "validate_storageheatmethod_dependency",
)
