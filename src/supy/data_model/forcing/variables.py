"""Typed definitions for SUEWS forcing variables."""

from __future__ import annotations

from enum import StrEnum
from typing import Any

from pydantic import BaseModel, ConfigDict, Field, model_validator


class ForcingDataType(StrEnum):
    """Primitive data types used by forcing columns."""

    INTEGER = "integer"
    NUMBER = "number"


class TemporalSemantics(StrEnum):
    """How a forcing value relates to its timestamp."""

    TIME = "time"
    INSTANTANEOUS = "inst"
    AVERAGE = "avg"
    SUM = "sum"


class ForcingRequirement(StrEnum):
    """Baseline and conditional forcing requirements."""

    DATETIME = "datetime"
    BASELINE = "baseline"
    CONDITIONAL = "conditional"
    OPTIONAL = "optional"
    DERIVED = "derived"


class MissingValuePolicy(StrEnum):
    """Whether the missing-value sentinel may be used."""

    FORBIDDEN = "forbidden"
    OPTIONAL_FILL = "optional_fill"
    ALLOWED_UNLESS_REQUIRED = "allowed_unless_physics_required"
    NOT_APPLICABLE = "not_applicable"


class PhysicsRequirement(BaseModel):
    """Physics selectors that make a forcing variable mandatory."""

    field: str = Field(description="Current ModelPhysics field name")
    values: tuple[int, ...] = Field(
        description="Selector values requiring the variable"
    )
    legacy_field: str | None = Field(
        default=None,
        description="Equivalent legacy run-control field name",
    )
    legacy_values: tuple[int, ...] | None = Field(
        default=None,
        description="Equivalent legacy selector values",
    )

    model_config = ConfigDict(extra="forbid")


class NumericRange(BaseModel):
    """Documented inclusive range for a numeric forcing variable."""

    minimum: float | None = None
    maximum: float | None = None

    model_config = ConfigDict(extra="forbid")


class ForcingVariable(BaseModel):
    """Definition of one forcing or derived time column."""

    name: str = Field(description="Canonical, case-sensitive column name")
    aliases: tuple[str, ...] = Field(
        default=(),
        description="Accepted or programmatic aliases",
    )
    data_type: ForcingDataType
    unit: str
    description: str
    temporal_semantics: TemporalSemantics
    requirement: ForcingRequirement
    physics_requirements: tuple[PhysicsRequirement, ...] = ()
    missing_value_policy: MissingValuePolicy
    missing_value: float | None = Field(
        default=None,
        description="Sentinel used when an optional value is unavailable",
    )
    valid_range: NumericRange | None = None
    validation_scale: float = Field(
        default=1.0,
        description=(
            "Scale applied before legacy runtime range validation; input and "
            "catalogue ranges remain in the declared unit"
        ),
    )
    surface_suffixes: tuple[str, ...] = Field(
        default=(),
        description="Allowed suffixes for per-land-cover columns",
    )
    legacy_position: int | None = Field(
        default=None,
        description="One-based position in the legacy 24-column forcing format",
    )
    input_column: bool = Field(
        default=True,
        description="Whether users may supply this column in a named-column file",
    )

    model_config = ConfigDict(extra="forbid")

    @model_validator(mode="before")
    @classmethod
    def set_default_missing_value_policy(cls, data: Any) -> Any:
        """Derive the explicit policy from requiredness when not supplied."""
        if not isinstance(data, dict) or "missing_value_policy" in data:
            return data
        requirement = ForcingRequirement(data["requirement"])
        if requirement in {
            ForcingRequirement.DATETIME,
            ForcingRequirement.BASELINE,
        }:
            policy = MissingValuePolicy.FORBIDDEN
        elif requirement == ForcingRequirement.CONDITIONAL:
            policy = MissingValuePolicy.ALLOWED_UNLESS_REQUIRED
        elif requirement == ForcingRequirement.OPTIONAL:
            policy = MissingValuePolicy.OPTIONAL_FILL
        else:
            policy = MissingValuePolicy.NOT_APPLICABLE
        return {**data, "missing_value_policy": policy}


class ForcingVariableRegistry(BaseModel):
    """Canonical registry and compatibility projections for forcing metadata."""

    variables: tuple[ForcingVariable, ...]

    model_config = ConfigDict(extra="forbid")

    @model_validator(mode="after")
    def validate_unique_names_and_aliases(self) -> ForcingVariableRegistry:
        """Reject ambiguous names and aliases using case-insensitive matching."""
        owners: dict[str, str] = {}
        for variable in self.variables:
            for candidate in (variable.name, *variable.aliases):
                key = candidate.casefold()
                owner = owners.get(key)
                if owner is not None and owner != variable.name:
                    raise ValueError(
                        f"forcing name or alias {candidate!r} is shared by "
                        f"{owner!r} and {variable.name!r}"
                    )
                owners[key] = variable.name
        return self

    def by_name(self, name: str) -> ForcingVariable | None:
        """Return a variable by canonical name or alias."""
        key = name.casefold()
        for variable in self.variables:
            if key in {
                candidate.casefold() for candidate in (variable.name, *variable.aliases)
            }:
                return variable
        return None

    @property
    def input_variables(self) -> tuple[ForcingVariable, ...]:
        """Variables accepted in named-column forcing files."""
        return tuple(variable for variable in self.variables if variable.input_column)

    @property
    def canonical_names(self) -> tuple[str, ...]:
        """Canonical names accepted in named-column forcing files."""
        return tuple(variable.name for variable in self.input_variables)

    @property
    def datetime_names(self) -> tuple[str, ...]:
        """Required date and time columns."""
        return tuple(
            variable.name
            for variable in self.input_variables
            if variable.requirement == ForcingRequirement.DATETIME
        )

    @property
    def baseline_names(self) -> tuple[str, ...]:
        """Required non-datetime forcing columns."""
        return tuple(
            variable.name
            for variable in self.input_variables
            if variable.requirement == ForcingRequirement.BASELINE
        )

    @property
    def optional_names(self) -> tuple[str, ...]:
        """Non-baseline forcing columns, including conditionally required ones."""
        return tuple(
            variable.name
            for variable in self.input_variables
            if variable.requirement
            in {ForcingRequirement.CONDITIONAL, ForcingRequirement.OPTIONAL}
        )

    @property
    def temporal_types(self) -> dict[str, str]:
        """Compatibility projection used by forcing resampling."""
        return {
            variable.name: variable.temporal_semantics.value
            for variable in self.variables
        }

    @property
    def aliases(self) -> dict[str, list[str]]:
        """Compatibility projection of programmatic aliases."""
        return {
            variable.name: list(variable.aliases)
            for variable in self.input_variables
            if variable.aliases
        }

    @property
    def per_landcover_suffixes(self) -> dict[str, tuple[str, ...]]:
        """Allowed per-land-cover column suffixes by bulk variable."""
        return {
            variable.name.casefold(): variable.surface_suffixes
            for variable in self.input_variables
            if variable.surface_suffixes
        }

    def physics_requirements(
        self,
        *,
        legacy: bool = False,
    ) -> dict[tuple[str, int], frozenset[str]]:
        """Project variable-centred requirements to the existing lookup shape."""
        requirements: dict[tuple[str, int], set[str]] = {}
        for variable in self.input_variables:
            for condition in variable.physics_requirements:
                field = condition.legacy_field if legacy else condition.field
                values = condition.legacy_values if legacy else condition.values
                if field is None or values is None:
                    continue
                for value in values:
                    requirements.setdefault((field, value), set()).add(
                        variable.name.casefold()
                    )
        return {key: frozenset(columns) for key, columns in requirements.items()}

    @property
    def checker_rules(self) -> dict[str, dict]:
        """Compatibility projection for the legacy range checker."""
        rules: dict[str, dict] = {}
        for variable in self.input_variables:
            if variable.valid_range is None:
                continue
            minimum = variable.valid_range.minimum
            maximum = variable.valid_range.maximum
            scale = variable.validation_scale
            rules[variable.name.casefold()] = {
                "cat": "grid",
                "logic": "range",
                "optional": variable.requirement
                not in {
                    ForcingRequirement.DATETIME,
                    ForcingRequirement.BASELINE,
                },
                "param": {
                    "min": "-inf" if minimum is None else minimum * scale,
                    "max": "inf" if maximum is None else maximum * scale,
                },
                "unit": variable.unit,
            }
        return rules
