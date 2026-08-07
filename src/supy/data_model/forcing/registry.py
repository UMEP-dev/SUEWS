"""Typed inventory models for the unpublished SUEWS forcing contract."""

from typing import Literal

from pydantic import BaseModel, ConfigDict, Field, model_validator

ForcingDataType = Literal["integer", "number"]
ForcingRole = Literal["coordinate", "driver", "observation", "reserved"]
TemporalSemantics = Literal["time", "inst", "avg", "sum"]
ForcingRequiredness = Literal["baseline", "conditional", "optional"]
MissingValuePolicy = Literal["forbidden", "sentinel", "fallback"]


class ForcingVariable(BaseModel):
    """Metadata for one externally supplied forcing column."""

    name: str = Field(description="Canonical external column name")
    data_type: ForcingDataType
    role: ForcingRole
    unit: str | None = Field(
        description="Input unit, or None while its scientific contract is unresolved"
    )
    description: str
    temporal: TemporalSemantics
    requiredness: ForcingRequiredness
    missing_policy: MissingValuePolicy
    validation_range: tuple[float | None, float | None] | None = Field(
        default=None,
        description="Inclusive enforced range in the input unit",
    )
    file_aliases: tuple[str, ...] = Field(
        default=(),
        description="Additional names accepted in forcing-file headers",
    )
    accessor_aliases: tuple[str, ...] = Field(
        default=(),
        description="Additional names accepted by programmatic accessors",
    )
    legacy_position: int | None = Field(
        default=None,
        description="One-based position in the legacy 24-column layout",
    )
    fallback: str | None = Field(
        default=None,
        description="Canonical bulk column used when this column is absent",
    )
    runtime_unit: str | None = Field(
        default=None,
        description="Internal unit after input normalisation, when different",
    )
    runtime_scale: float = Field(
        default=1.0,
        description="Scale applied when converting the input to its runtime unit",
    )
    metadata_note: str | None = Field(
        default=None,
        description="Known limitation that must be resolved before publication",
    )

    model_config = ConfigDict(extra="forbid", frozen=True)

    @model_validator(mode="after")
    def validate_missing_and_unit_metadata(self) -> "ForcingVariable":
        """Reject incomplete fallback, unit, and baseline declarations."""
        if self.requiredness == "baseline" and self.missing_policy != "forbidden":
            raise ValueError("baseline forcing variables must forbid missing values")
        if self.missing_policy == "fallback" and self.fallback is None:
            raise ValueError("fallback missing policy requires a fallback column")
        if self.missing_policy != "fallback" and self.fallback is not None:
            raise ValueError("only fallback variables may name a fallback column")
        if self.unit is None and self.metadata_note is None:
            raise ValueError("an unresolved unit requires a metadata note")
        if (
            self.runtime_unit is None and self.runtime_scale != 1.0  # ruff: ignore[float-equality-comparison]
        ):
            raise ValueError("a runtime scale requires a runtime unit")
        return self


class RequirementRule(BaseModel):
    """Columns required by one physics selector, including alternatives."""

    selector: str = Field(description="Current ModelPhysics field name")
    values: tuple[int, ...]
    alternatives: tuple[tuple[str, ...], ...] = Field(
        description=(
            "Any one complete alternative satisfies the requirement; columns within "
            "an alternative are jointly required"
        )
    )
    legacy_selector: str | None = None
    legacy_values: tuple[int, ...] = ()

    model_config = ConfigDict(extra="forbid", frozen=True)

    @model_validator(mode="after")
    def validate_rule_shape(self) -> "RequirementRule":
        """Reject empty or partially specified selector rules."""
        if not self.values:
            raise ValueError("requirement rule values cannot be empty")
        if not self.alternatives or any(not item for item in self.alternatives):
            raise ValueError("requirement alternatives cannot be empty")
        if (self.legacy_selector is None) != (not self.legacy_values):
            raise ValueError("legacy selector and values must be declared together")
        return self


class ForcingRegistry(BaseModel):
    """Unpublished forcing inventory with separate alias namespaces."""

    variables: tuple[ForcingVariable, ...]
    requirement_rules: tuple[RequirementRule, ...]
    missing_value: float = -999.0
    case_sensitive_headers: bool = False
    stripped_header_prefixes: tuple[str, ...] = ("%",)

    model_config = ConfigDict(extra="forbid", frozen=True)

    @model_validator(mode="after")
    def validate_inventory(self) -> "ForcingRegistry":
        """Reject ambiguous aliases, positions, fallbacks, and requirement names."""
        dict_variables = {variable.name: variable for variable in self.variables}
        if len(dict_variables) != len(self.variables):
            raise ValueError("forcing variable names must be unique")

        self._validate_alias_namespace("file_aliases")
        self._validate_alias_namespace("accessor_aliases")

        list_positions = sorted(
            variable.legacy_position
            for variable in self.variables
            if variable.legacy_position is not None
        )
        if list_positions != list(range(1, len(list_positions) + 1)):
            raise ValueError("legacy positions must be unique and contiguous from one")

        for variable in self.variables:
            if (
                variable.fallback is not None
                and variable.fallback not in dict_variables
            ):
                raise ValueError(
                    f"fallback {variable.fallback!r} for {variable.name!r} is unknown"
                )

        for rule in self.requirement_rules:
            for alternative in rule.alternatives:
                unknown = set(alternative).difference(dict_variables)
                if unknown:
                    raise ValueError(
                        f"requirement {rule.selector!r} references unknown columns "
                        f"{sorted(unknown)}"
                    )
        return self

    def _validate_alias_namespace(self, field_name: str) -> None:
        """Ensure one case-insensitive alias namespace has no ambiguous owner."""
        dict_owners: dict[str, str] = {}
        for variable in self.variables:
            aliases = getattr(variable, field_name)
            for candidate in (variable.name, *aliases):
                key = candidate.casefold()
                owner = dict_owners.get(key)
                if owner is not None and owner != variable.name:
                    raise ValueError(
                        f"{field_name} name {candidate!r} is shared by "
                        f"{owner!r} and {variable.name!r}"
                    )
                dict_owners[key] = variable.name

    def by_file_name(self, name: str) -> ForcingVariable | None:
        """Return the variable matching a canonical file name or file alias."""
        normalised = name
        for prefix in self.stripped_header_prefixes:
            if normalised.startswith(prefix):
                normalised = normalised[len(prefix) :]
                break
        return self._lookup(normalised, "file_aliases", self.case_sensitive_headers)

    def by_accessor_name(self, name: str) -> ForcingVariable | None:
        """Return the variable matching a canonical name or accessor alias."""
        return self._lookup(name, "accessor_aliases", False)

    @property
    def legacy_variables(self) -> tuple[ForcingVariable, ...]:
        """Return external legacy columns in their compatibility order."""
        return tuple(
            sorted(
                (
                    variable
                    for variable in self.variables
                    if variable.legacy_position is not None
                ),
                key=lambda variable: variable.legacy_position or 0,
            )
        )

    @property
    def canonical_file_columns(self) -> tuple[str, ...]:
        """Return canonical external columns in legacy file order."""
        return tuple(variable.name for variable in self.legacy_variables)

    @property
    def baseline_datetime_columns(self) -> tuple[str, ...]:
        """Return required timestamp columns in legacy file order."""
        return tuple(
            variable.name
            for variable in self.legacy_variables
            if variable.requiredness == "baseline" and variable.role == "coordinate"
        )

    @property
    def baseline_driver_columns(self) -> tuple[str, ...]:
        """Return required meteorological columns in legacy file order."""
        return tuple(
            variable.name
            for variable in self.legacy_variables
            if variable.requiredness == "baseline" and variable.role != "coordinate"
        )

    @property
    def baseline_file_columns(self) -> tuple[str, ...]:
        """Return every column required directly in an external forcing file."""
        return self.baseline_datetime_columns + self.baseline_driver_columns

    @property
    def optional_canonical_columns(self) -> tuple[str, ...]:
        """Return non-baseline legacy columns in compatibility order."""
        return tuple(
            variable.name
            for variable in self.legacy_variables
            if variable.requiredness != "baseline"
        )

    @property
    def temporal_types(self) -> dict[str, TemporalSemantics]:
        """Return resampling semantics for canonical external columns."""
        return {variable.name: variable.temporal for variable in self.legacy_variables}

    @property
    def runtime_validation_ranges(
        self,
    ) -> dict[str, tuple[float | None, float | None, str | None]]:
        """Return enforced ranges converted from file units to runtime units."""
        projected: dict[str, tuple[float | None, float | None, str | None]] = {}
        for variable in self.legacy_variables:
            if variable.validation_range is None:
                continue
            lower, upper = variable.validation_range
            scale = variable.runtime_scale
            projected[variable.name] = (
                None if lower is None else lower * scale,
                None if upper is None else upper * scale,
                variable.runtime_unit or variable.unit,
            )
        return projected

    @property
    def accessor_aliases(self) -> dict[str, list[str]]:
        """Return programmatic aliases without admitting them as file headers."""
        return {
            variable.name: list(variable.accessor_aliases)
            for variable in self.variables
            if variable.accessor_aliases
        }

    @property
    def per_landcover_columns(self) -> dict[str, tuple[str, ...]]:
        """Return surface-expanded columns grouped by their bulk fallback."""
        grouped: dict[str, list[str]] = {}
        for variable in self.variables:
            if variable.fallback is not None:
                grouped.setdefault(variable.fallback.casefold(), []).append(
                    variable.name
                )
        return {name: tuple(columns) for name, columns in grouped.items()}

    @property
    def current_requirements(self) -> dict[tuple[str, int], frozenset[str]]:
        """Project current physics selectors onto their bulk compatibility path."""
        projected: dict[tuple[str, int], frozenset[str]] = {}
        for rule in self.requirement_rules:
            required = frozenset(name.casefold() for name in rule.alternatives[0])
            for value in rule.values:
                projected[rule.selector, value] = required
        return projected

    @property
    def legacy_requirements(self) -> dict[tuple[str, int], list[str]]:
        """Project legacy checker selectors without changing their vocabulary."""
        projected: dict[tuple[str, int], list[str]] = {}
        for rule in self.requirement_rules:
            if rule.legacy_selector is None:
                continue
            for value in rule.legacy_values:
                projected[rule.legacy_selector, value] = list(rule.alternatives[0])
        return projected

    def _lookup(
        self,
        name: str,
        field_name: str,
        case_sensitive: bool,
    ) -> ForcingVariable | None:
        """Look up a name in exactly one alias namespace."""
        key = name if case_sensitive else name.casefold()
        for variable in self.variables:
            aliases = getattr(variable, field_name)
            candidates = (variable.name, *aliases)
            if any(
                key == (candidate if case_sensitive else candidate.casefold())
                for candidate in candidates
            ):
                return variable
        return None
