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

from collections.abc import Mapping
from dataclasses import dataclass, fields
import logging
from types import MappingProxyType
from typing import Any, Optional

from ..document import thaw_copy


logger = logging.getLogger("supy.data_model")


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
        if name in {"status", "severity"} and isinstance(value, str):
            norm = value.upper()
            if self.__dict__.get("_initialized", False) and norm not in VALID_SEVERITIES:
                raise ValueError(
                    f"Invalid validation {name} '{norm}'. "
                    f"Expected one of {sorted(VALID_SEVERITIES)}."
                )
            object.__setattr__(self, name, norm)
            if name == "status" and self.__dict__.get("_initialized", False):
                object.__setattr__(self, "severity", norm)
            return
        object.__setattr__(self, name, value)

    def __post_init__(self):
        status_norm = (self.status or "INFO").upper()
        severity_norm = (self.severity or status_norm).upper()
        if status_norm not in VALID_SEVERITIES:
            raise ValueError(
                f"Invalid validation status '{status_norm}'. "
                f"Expected one of {sorted(VALID_SEVERITIES)}."
            )
        if severity_norm not in VALID_SEVERITIES:
            raise ValueError(
                f"Invalid validation severity '{severity_norm}'. "
                f"Expected one of {sorted(VALID_SEVERITIES)}."
            )
        object.__setattr__(self, "status", status_norm)
        object.__setattr__(self, "severity", severity_norm)
        if self.code is None:
            normalised_parameter = self.parameter.replace(".", "_").replace(" ", "_")
            object.__setattr__(
                self, "code", f"{self.category}.{normalised_parameter}".upper()
            )
        if self.path is None:
            object.__setattr__(self, "path", self.parameter)
        object.__setattr__(self, "_initialized", True)

    def to_dict(self) -> dict:
        """Return a stable serialisable representation for reports and JSON."""
        return {
            f.name: thaw_copy(getattr(self, f.name))
            for f in fields(self)
        }

    @classmethod
    def from_violation(cls, violation) -> "ValidationResult":
        """Build a Phase B ERROR ``ValidationResult`` from a shared ``InvariantViolation``."""
        return cls(
            status="ERROR",
            category=violation.category,
            parameter=violation.parameter,
            site_index=violation.site_index,
            site_gridid=violation.site_gridid,
            message=violation.message,
            suggested_value=violation.suggested_value,
            code=violation.code,
        )

    @classmethod
    def from_advisory(cls, finding) -> "ValidationResult":
        """Build a Phase B WARNING/INFO ``ValidationResult`` from an ``AdvisoryFinding``."""
        return cls(
            status=finding.status,
            category=finding.category,
            parameter=finding.parameter,
            site_index=finding.site_index,
            site_gridid=finding.site_gridid,
            message=finding.message,
            suggested_value=finding.suggested_value,
        )

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
        if isinstance(value, Mapping):
            return MappingProxyType({
                k: self._deep_freeze(v) for k, v in value.items()
            })
        elif isinstance(value, (list, tuple)):
            return tuple(self._deep_freeze(v) for v in value)
        elif isinstance(value, (set, frozenset)):
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
            from . import DEFAULT_RULE_ORDER

            rule_order = DEFAULT_RULE_ORDER

        seen = set()
        for rule_id in rule_order:
            if rule_id in seen:
                continue
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

        from ...rules.typed_view import TypedConfigView

        for site_idx, param, message in TypedConfigView.from_mapping(
            context.yaml_data
        ).site_structure_errors:
            validation_results.append(
                ValidationResult(
                    status="ERROR",
                    category="STRUCTURE",
                    parameter=param,
                    site_index=site_idx,
                    message=message,
                    suggested_value=(
                        "Provide a mapping of site properties for each entry in 'sites'."
                    ),
                )
            )

        for rule_id, rule_fn in self.ordered_rule_items():
            try:
                for result in rule_fn(context):
                    validation_results.append(result)
            except Exception as exc:
                # One faulty rule must not hide the findings of the others (or
                # items this rule already yielded): report the crash as a single
                # ERROR naming the rule and carry on.
                logger.debug("Phase B rule %r failed", rule_id, exc_info=True)
                validation_results.append(
                    rule_crash_result(f"rule '{rule_id}'", rule_id, exc)
                )
        return validation_results


def rule_crash_result(label: str, rule_id: str, exc: BaseException) -> ValidationResult:
    """Build the ERROR result reported when a Phase B rule or adjustment crashes."""
    return ValidationResult(
        status="ERROR",
        category="VALIDATOR",
        parameter=str(rule_id),
        code=f"VALIDATOR.RULE_FAILED.{rule_id}".upper(),
        message=(
            f"Phase B {label} failed with {type(exc).__name__}: {exc}. "
            "The remaining checks still ran. This is a validator fault rather "
            "than a configuration error; please report it with the YAML file."
        ),
        suggested_value=None,
    )
