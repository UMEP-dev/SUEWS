"""
Development script for assigning rules to the validator controller

Example Usage:

@RulesRegistry.add_phase_b("rule name/descriptor")
def function_name(arguments):
    errors = [ValidationResult()]
    
    # Validation logic

    return errors
"""

from dataclasses import dataclass, fields
from types import MappingProxyType
from typing import Any, Mapping, Tuple, Optional


class RulesRegistry:

    phase_b = {}

    def __getitem__(self, item):
        return self.phase_b[item]

    def get(self, key):
        return self.phase_b.get(key)


    @classmethod
    def add_phase_b(cls, rule_id):
        def decorator(rule_fn):
            cls.phase_b[rule_id] = rule_fn
            return
        return decorator


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
