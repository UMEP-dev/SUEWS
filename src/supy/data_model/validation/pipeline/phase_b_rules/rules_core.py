"""
Development script for assigning rules to the validator controller

All functions should expect a single argument of type ValidationContext and
to retrieve all arguments from the class properties.

ValidationContext is designed to be immutable and as such property assignments cannot be made
and all data types are immutable data types. For instance checks for dict, use type Mapping
from collections.abc import Mapping.

Example Usage:

@RulesRegistry.add_rule("rule name/descriptor")
def function_name(context):
    variable = context.variable

    errors = [ValidationResult()]
    
    # Validation logic

    return errors
"""

from dataclasses import dataclass, fields
from types import MappingProxyType
from typing import Any, Mapping, Tuple, Optional


@dataclass
class ValidationResult:
    """Structured result from scientific validation checks."""

    status: str  # 'PASS', 'WARNING', 'ERROR'
    category: str  # 'PHYSICS', 'GEOGRAPHY', 'SEASONAL', 'LAND_COVER', 'MODEL_OPTIONS'
    parameter: str
    site_index: Optional[int] = None  # Array index (for internal use)
    site_gridid: Optional[int] = None  # GRIDID value (for display)
    message: str = ""
    suggested_value: Any = None
    applied_fix: bool = False


@dataclass(frozen=True)
class ValidationContext:
    """Immutable context for validation rules"""

    yaml_data: dict
    start_date: str|None = None
    model_year: int|None = None


    def __post_init__(self):
        for f in fields(self):
            value = getattr(self, f.name)
            object.__setattr__(self, f.name, self._deep_freeze(value))

    def _deep_freeze(self, value):
        if isinstance(value, dict):
            return MappingProxyType({
                k: self._deep_freeze(v) for k, v in value.items()
            })
        elif isinstance(value, list):
            return tuple(self._deep_freeze(v) for v in value)
        elif isinstance(value, set):
            return frozenset(self._deep_freeze(v) for v in value)
        else:
            return value


class RulesRegistry:

    rules = {}

    def __init__(self, context:ValidationContext=None):
        self.context = context

    def __getitem__(self, item):
        return self.rules[item]

    def get(self, key):
        return self.rules.get(key)


    @classmethod
    def add_rule(cls, rule_id):
        def decorator(rule_fn):
            cls.rules[rule_id] = rule_fn
            return rule_fn
        return decorator

    def run_validation(self, context:ValidationContext=None):
        validation_results = []

        if context is None:
            if self.context is None:
                raise ValueError("Must provide context of type ValidationContext to run validation registry.")
            context = self.context

        for rule_id, rule_fn in self.rules.items():
            validation_results.extend(
                rule_fn(context)
            )
        return validation_results
