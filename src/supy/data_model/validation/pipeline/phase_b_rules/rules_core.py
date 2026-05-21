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

from dataclasses import asdict, dataclass, fields
from types import MappingProxyType
from typing import Any, Mapping, Tuple, Optional


VALID_SEVERITIES = {
    "ERROR",
    "WARNING",
    "SUGGESTION",
    "APPLIED_FIX",
    "INFO",
    "PASS",
}


@dataclass
class ValidationResult:
    """Structured result from scientific validation checks."""

    status: str  # 'ERROR', 'WARNING', 'SUGGESTION', 'APPLIED_FIX', 'INFO', 'PASS'
    category: str  # 'PHYSICS', 'GEOGRAPHY', 'SEASONAL', 'LAND_COVER', 'MODEL_OPTIONS'
    parameter: str
    site_index: Optional[int] = None  # Array index (for internal use)
    site_gridid: Optional[int] = None  # GRIDID value (for display)
    message: str = ""
    suggested_value: Any = None
    applied_fix: bool = False
    code: Optional[str] = None
    severity: Optional[str] = None
    path: Optional[str] = None
    source: str = "phase_b"

    def __setattr__(self, name, value):
        if name == "status" and isinstance(value, str):
            value = value.upper()
            object.__setattr__(self, name, value)
            if "severity" in self.__dict__:
                object.__setattr__(self, "severity", value)
            return
        object.__setattr__(self, name, value)

    def __post_init__(self):
        self.status = (self.status or "INFO").upper()
        self.severity = (self.severity or self.status).upper()
        if self.severity not in VALID_SEVERITIES:
            raise ValueError(
                f"Invalid validation severity '{self.severity}'. "
                f"Expected one of {sorted(VALID_SEVERITIES)}."
            )
        if self.code is None:
            normalised_parameter = self.parameter.replace(".", "_").replace(" ", "_")
            self.code = f"{self.category}.{normalised_parameter}".upper()
        if self.path is None:
            self.path = self.parameter

    def to_dict(self) -> dict:
        """Return a stable serialisable representation for reports and JSON."""
        return asdict(self)

    def to_issue(self):
        """Adapt this Phase B result to the canonical ``Issue`` schema.

        Imported lazily to keep ``rules_core`` independent of the
        ``report_schema`` module's import order.
        """
        from ..report_schema import Issue, SEVERITY_INFO

        severity = (self.severity or self.status or SEVERITY_INFO).upper()
        code = self.code or f"B.{self.category}.{self.parameter}".upper()
        return Issue(
            phase="B",
            severity=severity,
            code=code,
            message=self.message,
            yaml_path=self.path or self.parameter,
            site_gridid=self.site_gridid,
            site_index=self.site_index,
            category=self.category,
            suggested_value=self.suggested_value,
            applied_fix=bool(self.applied_fix),
        )


@dataclass(frozen=True)
class ValidationContext:
    """Immutable context for validation rules"""

    yaml_data: dict
    start_date: Optional[str] = None
    model_year: Optional[int] = None


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

    def __init__(self, context:ValidationContext=None, rule_order=None):
        self.context = context
        self.rule_order = tuple(rule_order) if rule_order is not None else None

    def __getitem__(self, item):
        return self.rules[item]

    def get(self, key):
        return self.rules.get(key)

    def ordered_rule_items(self):
        rule_order = self.rule_order
        if rule_order is None:
            try:
                from . import DEFAULT_RULE_ORDER

                rule_order = DEFAULT_RULE_ORDER
            except ImportError:
                rule_order = tuple(self.rules.keys())

        seen = set()
        for rule_id in rule_order:
            rule_fn = self.rules.get(rule_id)
            if rule_fn is None:
                continue
            seen.add(rule_id)
            yield rule_id, rule_fn

        for rule_id, rule_fn in self.rules.items():
            if rule_id not in seen:
                yield rule_id, rule_fn


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

        for rule_id, rule_fn in self.ordered_rule_items():
            validation_results.extend(
                rule_fn(context)
            )
        return validation_results
