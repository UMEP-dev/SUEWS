"""Structured metadata for input fields whose units depend on a selector.

This module defines the structural schema, builder, and validator for
``Field(..., json_schema_extra={"conditional_unit": ...})`` declarations on
SUEWS configuration models. Each declaring field remains the single source of
truth: Pydantic introspection, JSON Schema export, generated Sphinx reference
pages, MCP schema search, and unit-metadata tests all read the same structure.

Three branch kinds are supported:

- ``"cf"``: a fixed CF unit string accepted by :func:`supy.units.parse_unit`;
- ``"formula"``: a symbolic dimensional expression whose exponent depends on
  one or more dimensionless sibling fields (no fixed CF unit exists);
- ``"empirical_base"``: the base of an empirical power ``base**x`` whose
  exponent ``x`` carries ``basis_unit`` and whose logarithm carries the
  reciprocal ``log_unit`` (neither dimensionless nor linearly convertible).
"""

from collections.abc import Iterator, Mapping
from dataclasses import dataclass
from enum import Enum
import inspect
import math
import re
from types import MappingProxyType, UnionType
from typing import Any, Literal, Union, get_args, get_origin

from pydantic import BaseModel

from ...units import Unit, parse_unit

__all__ = [
    "CONDITIONAL_UNIT_KEY",
    "ConditionalUnitCase",
    "ConditionalUnitKind",
    "ConditionalUnitSpec",
    "SelectorScope",
    "conditional_unit_extra",
    "iter_cf_unit_strings",
    "resolve_conditional_unit_case",
    "validate_conditional_unit_binding",
]

CONDITIONAL_UNIT_KEY = "conditional_unit"

ConditionalUnitKind = Literal["cf", "formula", "empirical_base"]
SelectorScope = Literal["sibling", "config"]

_VALID_KINDS = frozenset({"cf", "formula", "empirical_base"})
_VALID_SCOPES = frozenset({"sibling", "config"})
_SELECTOR_CODE_RE = re.compile(r"^-?[0-9]+$")
_IDENTIFIER_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
_DIMENSIONLESS_DIM = (0, 0, 0, 0, 0, 0)

_CASE_ALLOWED_KEYS = frozenset({
    "label",
    "kind",
    "formula",
    "unit",
    "expression",
    "base_units",
    "depends_on",
    "basis_unit",
    "log_unit",
    "note",
})
_SPEC_ALLOWED_KEYS = frozenset({
    "selector",
    "selector_scope",
    "selector_model",
    "selector_field",
    "cases",
    "inactive_note",
    "activation_note",
})


def _require_ascii_text(value: Any, name: str) -> str:
    """Validate a non-empty stripped ASCII string."""
    if not isinstance(value, str) or not value.strip() or value != value.strip():
        raise ValueError(f"{name} must be a non-empty stripped string, got {value!r}")
    if not value.isascii():
        raise ValueError(f"{name} must contain ASCII characters only, got {value!r}")
    return value


def _require_optional_ascii_text(value: Any, name: str) -> str | None:
    """Validate an optional non-empty stripped ASCII string."""
    if value is None:
        return None
    return _require_ascii_text(value, name)


def _require_cf_unit(value: Any, name: str) -> Unit:
    """Validate a non-null CF unit string against the SUEWS unit vocabulary."""
    if not isinstance(value, str):
        raise ValueError(f"{name} must be a CF unit string, got {value!r}")
    parsed = parse_unit(value)
    if parsed.is_no_unit:
        raise ValueError(
            f"{name} must be a physical or dimensionless CF unit, not None"
        )
    return parsed


def _require_string_tuple(value: Any, name: str) -> tuple[str, ...]:
    """Validate a tuple or list of unique non-empty ASCII strings."""
    if not isinstance(value, (tuple, list)):
        raise ValueError(f"{name} must be a tuple or list of strings, got {value!r}")
    items = tuple(
        _require_ascii_text(item, f"{name}[{idx}]") for idx, item in enumerate(value)
    )
    if len(set(items)) != len(items):
        raise ValueError(f"{name} must not contain duplicate entries: {items!r}")
    return items


def _validate_cf_case(case: "ConditionalUnitCase") -> None:
    """Validate fields for a fixed-CF branch."""
    _require_cf_unit(case.unit, "unit")
    if case.expression is not None:
        raise ValueError("cf case must not define expression")
    if case.base_units or case.depends_on:
        raise ValueError("cf case must not define base_units or depends_on")
    if case.basis_unit is not None or case.log_unit is not None:
        raise ValueError("cf case must not define basis_unit or log_unit")


def _validate_formula_case(case: "ConditionalUnitCase") -> None:
    """Validate fields for a symbolic-exponent formula branch."""
    _require_ascii_text(case.expression, "expression")
    if not case.base_units:
        raise ValueError("formula case requires non-empty base_units")
    for idx, text in enumerate(case.base_units):
        _require_cf_unit(text, f"base_units[{idx}]")
    if not case.depends_on:
        raise ValueError("formula case requires non-empty depends_on")
    for dep in case.depends_on:
        if _IDENTIFIER_RE.fullmatch(dep) is None:
            raise ValueError(
                f"depends_on entries must be field identifiers, got {dep!r}"
            )
    if case.basis_unit is not None or case.log_unit is not None:
        raise ValueError("formula case must not define basis_unit or log_unit")


def _validate_empirical_base_case(case: "ConditionalUnitCase") -> None:
    """Validate fields for an empirical-power-base branch."""
    _require_ascii_text(case.expression, "expression")
    if case.base_units or case.depends_on:
        raise ValueError("empirical_base case must not define base_units or depends_on")
    basis = _require_cf_unit(case.basis_unit, "basis_unit")
    log = _require_cf_unit(case.log_unit, "log_unit")
    if bool(basis.offset) or bool(log.offset):
        raise ValueError(
            "empirical_base basis_unit and log_unit must not be offset scales"
        )
    assert basis.dimension is not None and log.dimension is not None
    net_dim = tuple(a + b for a, b in zip(basis.dimension, log.dimension, strict=True))
    if net_dim != _DIMENSIONLESS_DIM or not math.isclose(
        basis.scale * log.scale, 1.0, rel_tol=1e-12, abs_tol=0.0
    ):
        raise ValueError(
            "empirical_base log_unit must be the exact reciprocal of basis_unit "
            f"(got basis_unit={case.basis_unit!r}, log_unit={case.log_unit!r})"
        )


@dataclass(frozen=True, slots=True)
class ConditionalUnitCase:
    """Dimensional specification for one active selector value."""

    label: str
    kind: ConditionalUnitKind
    formula: str
    unit: str | None = None
    expression: str | None = None
    base_units: tuple[str, ...] = ()
    depends_on: tuple[str, ...] = ()
    basis_unit: str | None = None
    log_unit: str | None = None
    note: str | None = None

    def __post_init__(self) -> None:
        """Validate branch metadata invariants upon construction."""
        _require_ascii_text(self.label, "label")
        _require_ascii_text(self.formula, "formula")
        _require_optional_ascii_text(self.note, "note")
        if self.kind not in _VALID_KINDS:
            raise ValueError(
                f"kind must be one of {sorted(_VALID_KINDS)}, got {self.kind!r}"
            )
        base_units = _require_string_tuple(self.base_units, "base_units")
        depends_on = _require_string_tuple(self.depends_on, "depends_on")
        object.__setattr__(self, "base_units", base_units)
        object.__setattr__(self, "depends_on", depends_on)

        if self.kind == "cf":
            _validate_cf_case(self)
            return
        if self.unit is not None:
            raise ValueError(
                f"{self.kind} case must not invent a fixed CF unit (got {self.unit!r})"
            )
        if self.kind == "formula":
            _validate_formula_case(self)
            return
        _validate_empirical_base_case(self)

    def to_schema_dict(self) -> dict[str, Any]:
        """Return a clean JSON-Schema-serialisable representation."""
        data: dict[str, Any] = {
            "label": self.label,
            "kind": self.kind,
        }
        if self.unit is not None:
            data["unit"] = self.unit
        if self.expression is not None:
            data["expression"] = self.expression
        if self.base_units:
            data["base_units"] = list(self.base_units)
        if self.depends_on:
            data["depends_on"] = list(self.depends_on)
        if self.basis_unit is not None:
            data["basis_unit"] = self.basis_unit
        if self.log_unit is not None:
            data["log_unit"] = self.log_unit
        data["formula"] = self.formula
        if self.note is not None:
            data["note"] = self.note
        return data

    @classmethod
    def from_dict(cls, data: Mapping[str, Any]) -> "ConditionalUnitCase":
        """Parse and validate one case dictionary from JSON Schema metadata."""
        if not isinstance(data, Mapping):
            raise ValueError(f"conditional_unit case must be a mapping, got {data!r}")
        unknown = set(data.keys()) - _CASE_ALLOWED_KEYS
        if unknown:
            raise ValueError(
                f"conditional_unit case has unknown keys: {sorted(unknown)}"
            )
        missing = {"label", "kind", "formula"} - set(data.keys())
        if missing:
            raise ValueError(
                f"conditional_unit case is missing required keys: {sorted(missing)}"
            )
        return cls(
            label=data["label"],
            kind=data["kind"],
            formula=data["formula"],
            unit=data.get("unit"),
            expression=data.get("expression"),
            base_units=data.get("base_units", ()),
            depends_on=data.get("depends_on", ()),
            basis_unit=data.get("basis_unit"),
            log_unit=data.get("log_unit"),
            note=data.get("note"),
        )


@dataclass(frozen=True, slots=True)
class ConditionalUnitSpec:
    """Structural unit metadata bound to an explicit selector field."""

    selector: str
    selector_scope: SelectorScope
    selector_model: str
    selector_field: str
    cases: Mapping[str, ConditionalUnitCase]
    inactive_note: str
    activation_note: str | None = None

    def __post_init__(self) -> None:
        """Validate spec invariants and freeze the cases mapping."""
        selector = _require_ascii_text(self.selector, "selector")
        selector_model = _require_ascii_text(self.selector_model, "selector_model")
        selector_field = _require_ascii_text(self.selector_field, "selector_field")
        _require_ascii_text(self.inactive_note, "inactive_note")
        _require_optional_ascii_text(self.activation_note, "activation_note")

        if self.selector_scope not in _VALID_SCOPES:
            raise ValueError(
                f"selector_scope must be one of {sorted(_VALID_SCOPES)}, "
                f"got {self.selector_scope!r}"
            )
        if _IDENTIFIER_RE.fullmatch(selector_model) is None:
            raise ValueError(
                f"selector_model must be a class identifier, got {selector_model!r}"
            )
        if _IDENTIFIER_RE.fullmatch(selector_field) is None:
            raise ValueError(
                f"selector_field must be a field identifier, got {selector_field!r}"
            )

        if self.selector_scope == "sibling":
            if "." in selector or selector != selector_field:
                raise ValueError(
                    "sibling selector must match selector_field without dots "
                    f"(got selector={selector!r}, selector_field={selector_field!r})"
                )
        else:
            parts = selector.split(".")
            if len(parts) < 2 or any(
                _IDENTIFIER_RE.fullmatch(part) is None for part in parts
            ):
                raise ValueError(
                    "config selector must be a dotted path of identifiers "
                    f"(got {selector!r})"
                )
            if parts[-1] != selector_field:
                raise ValueError(
                    "config selector path must end with selector_field "
                    f"(got selector={selector!r}, selector_field={selector_field!r})"
                )

        if not isinstance(self.cases, Mapping) or not self.cases:
            raise ValueError("conditional_unit requires a non-empty cases mapping")
        validated: dict[str, ConditionalUnitCase] = {}
        for code, case in self.cases.items():
            if not isinstance(code, str) or _SELECTOR_CODE_RE.fullmatch(code) is None:
                raise ValueError(
                    f"conditional_unit case keys must be integer strings, got {code!r}"
                )
            if not isinstance(case, ConditionalUnitCase):
                raise ValueError(
                    f"conditional_unit case {code!r} must be ConditionalUnitCase, "
                    f"got {type(case).__name__}"
                )
            validated[code] = case
        object.__setattr__(self, "cases", MappingProxyType(validated))

    def to_schema_dict(self) -> dict[str, Any]:
        """Return a clean JSON-Schema-serialisable dictionary."""
        data: dict[str, Any] = {
            "selector": self.selector,
            "selector_scope": self.selector_scope,
            "selector_model": self.selector_model,
            "selector_field": self.selector_field,
            "cases": {code: case.to_schema_dict() for code, case in self.cases.items()},
        }
        if self.activation_note is not None:
            data["activation_note"] = self.activation_note
        data["inactive_note"] = self.inactive_note
        return data

    @classmethod
    def from_dict(cls, data: Mapping[str, Any]) -> "ConditionalUnitSpec":
        """Parse and validate a conditional_unit mapping from JSON Schema."""
        if not isinstance(data, Mapping):
            raise ValueError(f"conditional_unit must be a mapping, got {data!r}")
        unknown = set(data.keys()) - _SPEC_ALLOWED_KEYS
        if unknown:
            raise ValueError(f"conditional_unit has unknown keys: {sorted(unknown)}")
        missing = {
            "selector",
            "selector_scope",
            "selector_model",
            "selector_field",
            "cases",
            "inactive_note",
        } - set(data.keys())
        if missing:
            raise ValueError(
                f"conditional_unit is missing required keys: {sorted(missing)}"
            )
        raw_cases = data["cases"]
        if not isinstance(raw_cases, Mapping) or not raw_cases:
            raise ValueError("conditional_unit requires a non-empty cases mapping")
        parsed_cases = {
            code: (
                case
                if isinstance(case, ConditionalUnitCase)
                else ConditionalUnitCase.from_dict(case)
            )
            for code, case in raw_cases.items()
        }
        return cls(
            selector=data["selector"],
            selector_scope=data["selector_scope"],
            selector_model=data["selector_model"],
            selector_field=data["selector_field"],
            cases=parsed_cases,
            inactive_note=data["inactive_note"],
            activation_note=data.get("activation_note"),
        )


def conditional_unit_extra(spec: ConditionalUnitSpec) -> dict[str, Any]:
    """Build the ``json_schema_extra`` entry for a conditional-unit field."""
    if not isinstance(spec, ConditionalUnitSpec):
        raise ValueError(
            f"spec must be a ConditionalUnitSpec, got {type(spec).__name__}"
        )
    return {CONDITIONAL_UNIT_KEY: spec.to_schema_dict()}


def iter_cf_unit_strings(spec: ConditionalUnitSpec) -> Iterator[tuple[str, str]]:
    """Yield every ``(subpath, cf_unit_text)`` pair embedded in ``spec``."""
    for code, case in spec.cases.items():
        if case.kind == "cf" and case.unit is not None:
            yield (f"cases[{code}].unit", case.unit)
        elif case.kind == "formula":
            for idx, unit_text in enumerate(case.base_units):
                yield (f"cases[{code}].base_units[{idx}]", unit_text)
        elif case.kind == "empirical_base":
            if case.basis_unit is not None:
                yield (f"cases[{code}].basis_unit", case.basis_unit)
            if case.log_unit is not None:
                yield (f"cases[{code}].log_unit", case.log_unit)


def _parse_selector_code(selector_value: Any) -> str | None:
    """Convert a selector value (scalar, RefValue, Enum) to a canonical code."""
    value = selector_value
    if hasattr(value, "value"):
        value = value.value
    if hasattr(value, "value"):
        value = value.value
    if value is None or isinstance(value, bool):
        return None
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return str(int(value)) if value.is_integer() else None
    if isinstance(value, str):
        text = value.strip()
        return text if _SELECTOR_CODE_RE.fullmatch(text) else None
    return None


def resolve_conditional_unit_case(
    raw: ConditionalUnitSpec | Mapping[str, Any],
    selector_value: Any,
) -> ConditionalUnitCase | None:
    """Resolve the active :class:`ConditionalUnitCase` for ``selector_value``."""
    spec = (
        raw
        if isinstance(raw, ConditionalUnitSpec)
        else ConditionalUnitSpec.from_dict(raw)
    )
    code = _parse_selector_code(selector_value)
    if code is None:
        return None
    return spec.cases.get(code)


def _unwrap_ref_or_container_models(annotation: Any) -> list[type[BaseModel]]:
    """Extract nested Pydantic model classes from an annotation for path walking."""
    origin = get_origin(annotation)
    if origin in {Union, UnionType}:
        found: list[type[BaseModel]] = []
        for arg in get_args(annotation):
            if arg is type(None):
                continue
            found.extend(_unwrap_ref_or_container_models(arg))
        return found
    if origin in {list, tuple, set, frozenset}:
        found = []
        for arg in get_args(annotation):
            if arg is Ellipsis:
                continue
            found.extend(_unwrap_ref_or_container_models(arg))
        return found
    if inspect.isclass(annotation) and issubclass(annotation, BaseModel):
        if (
            annotation.__name__.startswith("RefValue")
            and "value" in annotation.model_fields
        ):
            return _unwrap_ref_or_container_models(
                annotation.model_fields["value"].annotation
            )
        return [annotation]
    return []


def _unwrap_leaf_types(annotation: Any) -> set[type]:
    """Unwrap Optional, Union, and RefValue wrappers to underlying value types."""
    origin = get_origin(annotation)
    if origin in {Union, UnionType}:
        found: set[type] = set()
        for arg in get_args(annotation):
            if arg is type(None):
                continue
            found.update(_unwrap_leaf_types(arg))
        return found
    if inspect.isclass(annotation) and issubclass(annotation, BaseModel):
        if (
            annotation.__name__.startswith("RefValue")
            and "value" in annotation.model_fields
        ):
            return _unwrap_leaf_types(annotation.model_fields["value"].annotation)
        return {annotation}
    if inspect.isclass(annotation):
        return {annotation}
    return set()


def _extract_enum_class(annotation: Any) -> type[Enum] | None:
    """Return the unique Enum class wrapped in a selector annotation, if any."""
    enums = [
        cls
        for cls in _unwrap_leaf_types(annotation)
        if inspect.isclass(cls) and issubclass(cls, Enum)
    ]
    if len(enums) == 1:
        return enums[0]
    return None


def _is_numeric_or_enum_selector(annotation: Any) -> bool:
    """Return True when the unwrapped field annotation accepts numeric/enum codes."""
    leaf_types = _unwrap_leaf_types(annotation)
    if not leaf_types:
        return False
    for cls in leaf_types:
        if cls is bool:
            return False
        if cls in {int, float} or (inspect.isclass(cls) and issubclass(cls, Enum)):
            continue
        return False
    return True


def _resolve_config_selector(
    root_model: type[BaseModel], path: str
) -> tuple[type[BaseModel], str, Any]:
    """Walk a dotted config path from ``root_model`` to ``(model, field, FieldInfo)``."""
    parts = path.split(".")
    current_model = root_model
    for idx, segment in enumerate(parts[:-1]):
        if segment not in current_model.model_fields:
            raise ValueError(
                f"config selector {path!r}: {current_model.__name__} has no field "
                f"{segment!r}"
            )
        field_info = current_model.model_fields[segment]
        nested = _unwrap_ref_or_container_models(field_info.annotation)
        if len(nested) != 1:
            raise ValueError(
                f"config selector {path!r}: segment {segment!r} on "
                f"{current_model.__name__} does not resolve to a unique model "
                f"(at step {idx})"
            )
        current_model = nested[0]

    leaf_field = parts[-1]
    if leaf_field not in current_model.model_fields:
        raise ValueError(
            f"config selector {path!r}: {current_model.__name__} has no field "
            f"{leaf_field!r}"
        )
    return current_model, leaf_field, current_model.model_fields[leaf_field]


def _resolve_selector_field(
    owner_model: type[BaseModel],
    field_name: str,
    spec: ConditionalUnitSpec,
    root_model: type[BaseModel],
) -> tuple[type[BaseModel], Any]:
    """Resolve and verify the selector model and FieldInfo named by ``spec``."""
    field_path = f"{owner_model.__name__}.{field_name}"
    if spec.selector_scope == "sibling":
        if spec.selector_model != owner_model.__name__:
            raise ValueError(
                f"{field_path}: sibling selector_model {spec.selector_model!r} "
                f"does not match declaring model {owner_model.__name__!r}"
            )
        if spec.selector_field == field_name:
            raise ValueError(f"{field_path}: field cannot select its own unit")
        if spec.selector_field not in owner_model.model_fields:
            raise ValueError(
                f"{field_path}: sibling selector {spec.selector_field!r} "
                f"not found on {owner_model.__name__}"
            )
        return owner_model, owner_model.model_fields[spec.selector_field]

    selector_model_cls, _, selector_field_info = _resolve_config_selector(
        root_model, spec.selector
    )
    mro_names = {cls.__name__ for cls in selector_model_cls.__mro__}
    if spec.selector_model not in mro_names:
        raise ValueError(
            f"{field_path}: config selector {spec.selector!r} resolved to "
            f"{selector_model_cls.__name__}, not {spec.selector_model!r}"
        )
    return selector_model_cls, selector_field_info


def _validate_formula_dependency(
    owner_model: type[BaseModel],
    field_name: str,
    spec: ConditionalUnitSpec,
    code: str,
    dep: str,
) -> None:
    """Check that one ``depends_on`` exponent field exists and is dimensionless."""
    field_path = f"{owner_model.__name__}.{field_name}"
    if dep in {field_name, spec.selector_field}:
        raise ValueError(
            f"{field_path}: formula case {code!r} cannot depend on {dep!r}"
        )
    if dep not in owner_model.model_fields:
        raise ValueError(
            f"{field_path}: formula case {code!r} depends_on unknown field "
            f"{dep!r} on {owner_model.__name__}"
        )
    dep_extra = owner_model.model_fields[dep].json_schema_extra
    if not isinstance(dep_extra, Mapping):
        raise ValueError(
            f"{field_path}: formula dependency {dep!r} lacks unit metadata"
        )
    if CONDITIONAL_UNIT_KEY in dep_extra:
        dep_spec = ConditionalUnitSpec.from_dict(dep_extra[CONDITIONAL_UNIT_KEY])
        same_selector = (
            dep_spec.selector_scope == spec.selector_scope
            and dep_spec.selector_model == spec.selector_model
            and dep_spec.selector_field == spec.selector_field
            and dep_spec.selector == spec.selector
        )
        if not same_selector:
            raise ValueError(
                f"{field_path}: formula dependency {dep!r} uses a different "
                f"selector ({dep_spec.selector!r} != {spec.selector!r})"
            )
        dep_case = dep_spec.cases.get(code)
        if (
            dep_case is None
            or dep_case.kind != "cf"
            or not parse_unit(dep_case.unit).is_dimensionless
        ):
            raise ValueError(
                f"{field_path}: formula dependency {dep!r} must be a "
                f"dimensionless CF unit for selector code {code!r}"
            )
        return
    if "unit" not in dep_extra:
        raise ValueError(
            f"{field_path}: formula dependency {dep!r} lacks unit metadata"
        )
    dep_unit = parse_unit(dep_extra["unit"])
    if dep_unit.is_no_unit or not dep_unit.is_dimensionless:
        raise ValueError(
            f"{field_path}: formula dependency {dep!r} must be "
            f"dimensionless, got {dep_extra['unit']!r}"
        )


def validate_conditional_unit_binding(
    owner_model: type[BaseModel],
    field_name: str,
    extra: Mapping[str, Any] | None,
    *,
    root_model: type[BaseModel],
) -> ConditionalUnitSpec:
    """Validate a field's ``conditional_unit`` metadata against the live models.

    Checks mutual exclusivity with ``"unit"``, schema validity, selector
    resolution (sibling or config path), unitless selector metadata, exact enum
    branch coverage where the selector is an :class:`~enum.Enum`, and
    dimensionless ``depends_on`` references for ``"formula"`` cases.
    """
    field_path = f"{owner_model.__name__}.{field_name}"
    if not isinstance(extra, Mapping) or CONDITIONAL_UNIT_KEY not in extra:
        raise ValueError(f"{field_path}: missing {CONDITIONAL_UNIT_KEY!r} metadata")
    if "unit" in extra:
        raise ValueError(
            f"{field_path}: must not declare both 'unit' and {CONDITIONAL_UNIT_KEY!r}"
        )

    spec = ConditionalUnitSpec.from_dict(extra[CONDITIONAL_UNIT_KEY])
    selector_model_cls, selector_field_info = _resolve_selector_field(
        owner_model, field_name, spec, root_model
    )

    selector_extra = selector_field_info.json_schema_extra
    if (
        not isinstance(selector_extra, Mapping)
        or "unit" not in selector_extra
        or selector_extra["unit"] is not None
        or CONDITIONAL_UNIT_KEY in selector_extra
    ):
        raise ValueError(
            f"{field_path}: selector {spec.selector!r} on "
            f"{selector_model_cls.__name__} must be a unitless selector "
            f"(json_schema_extra={{'unit': None}}), got {selector_extra!r}"
        )
    if not _is_numeric_or_enum_selector(selector_field_info.annotation):
        raise ValueError(
            f"{field_path}: selector {spec.selector!r} must have a numeric or "
            f"Enum type, got {selector_field_info.annotation!r}"
        )

    enum_cls = _extract_enum_class(selector_field_info.annotation)
    if enum_cls is not None:
        expected_codes = {str(member.value) for member in enum_cls}
        actual_codes = set(spec.cases.keys())
        if actual_codes != expected_codes:
            raise ValueError(
                f"{field_path}: cases {sorted(actual_codes)} do not match "
                f"{enum_cls.__name__} values {sorted(expected_codes)}"
            )

    for code, case in spec.cases.items():
        if case.kind != "formula":
            continue
        for dep in case.depends_on:
            _validate_formula_dependency(owner_model, field_name, spec, code, dep)

    return spec
