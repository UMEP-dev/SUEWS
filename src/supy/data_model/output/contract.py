"""Machine-readable projection of the SUEWS output-variable registry."""

from enum import Enum, StrEnum
from typing import Literal

from pydantic import BaseModel, ConfigDict, Field

from .variables import (
    AggregationMethod,
    OutputGroup,
    OutputLevel,
    OutputVariableRegistry,
)


class OutputContractScope(StrEnum):
    """Stability scope assigned to an output group."""

    COORDINATE = "coordinate"
    STABLE = "stable"
    PROVISIONAL = "provisional"
    INTERNAL = "internal"


OUTPUT_GROUP_SCOPES: dict[OutputGroup, OutputContractScope] = {
    OutputGroup.DATETIME: OutputContractScope.COORDINATE,
    OutputGroup.SUEWS: OutputContractScope.STABLE,
    OutputGroup.SNOW: OutputContractScope.STABLE,
    OutputGroup.ESTM: OutputContractScope.STABLE,
    OutputGroup.RSL: OutputContractScope.STABLE,
    OutputGroup.BL: OutputContractScope.STABLE,
    OutputGroup.DAILYSTATE: OutputContractScope.STABLE,
    OutputGroup.DEBUG: OutputContractScope.INTERNAL,
    OutputGroup.EHC: OutputContractScope.PROVISIONAL,
    OutputGroup.BEERS: OutputContractScope.PROVISIONAL,
    OutputGroup.SPARTACUS: OutputContractScope.PROVISIONAL,
    OutputGroup.STEBBS: OutputContractScope.PROVISIONAL,
    OutputGroup.NHOOD: OutputContractScope.PROVISIONAL,
}


class OutputMissingValues(BaseModel):
    """Missing-value representation used by each supported output form."""

    dataframe: Literal["nan"] = "nan"
    text: Literal["sentinel:-999.0"] = "sentinel:-999.0"
    parquet: Literal["null"] = "null"

    model_config = ConfigDict(extra="forbid", frozen=True)


class OutputRepresentation(BaseModel):
    """Metadata shared by every entry in the current registry."""

    value_type: Literal["number"] = "number"
    shape: Literal["scalar"] = "scalar"
    missing_values: OutputMissingValues = Field(default_factory=OutputMissingValues)

    model_config = ConfigDict(extra="forbid", frozen=True)


class OutputContractGroup(BaseModel):
    """Ordered output group and its stability scope."""

    group: OutputGroup
    ordinal: int = Field(ge=0)
    scope: OutputContractScope

    model_config = ConfigDict(extra="forbid", frozen=True, use_enum_values=True)


class OutputContractVariable(BaseModel):
    """One registry variable projected into the output contract catalogue.

    The pair ``(group, name)`` is the variable identity. ``ordinal`` is its
    zero-based position inside that group in ``OUTPUT_REGISTRY``.
    """

    group: OutputGroup
    name: str
    ordinal: int = Field(ge=0)
    unit: str
    description: str
    aggregation: AggregationMethod
    level: OutputLevel

    model_config = ConfigDict(extra="forbid", frozen=True, use_enum_values=True)


class OutputContractCatalogue(BaseModel):
    """Deterministic, in-memory catalogue projected from ``OUTPUT_REGISTRY``."""

    kind: Literal["output"] = "output"
    representation: OutputRepresentation = Field(default_factory=OutputRepresentation)
    groups: tuple[OutputContractGroup, ...]
    variables: tuple[OutputContractVariable, ...]

    model_config = ConfigDict(
        extra="forbid",
        frozen=True,
        title="SUEWS output contract catalogue",
    )


def _enum_value(value: object) -> object:
    """Return an enum value while accepting Pydantic's value conversion."""
    return value.value if isinstance(value, Enum) else value


def _project_output_contract(
    registry: OutputVariableRegistry,
) -> OutputContractCatalogue:
    """Project the output contract directly from a variable registry."""
    list_group_names: list[str] = []
    dict_next_ordinal: dict[str, int] = {}
    list_variables: list[OutputContractVariable] = []

    for variable in registry.variables:
        group_name = str(_enum_value(variable.group))
        if group_name not in dict_next_ordinal:
            list_group_names.append(group_name)
            dict_next_ordinal[group_name] = 0

        list_variables.append(
            OutputContractVariable(
                group=OutputGroup(group_name),
                name=variable.name,
                ordinal=dict_next_ordinal[group_name],
                unit=variable.unit,
                description=variable.description,
                aggregation=AggregationMethod(_enum_value(variable.aggregation)),
                level=OutputLevel(_enum_value(variable.level)),
            )
        )
        dict_next_ordinal[group_name] += 1

    set_registry_groups = {OutputGroup(group_name) for group_name in list_group_names}
    set_scoped_groups = set(OUTPUT_GROUP_SCOPES)
    if set_registry_groups != set_scoped_groups:
        missing = sorted(
            group.value for group in set_registry_groups - set_scoped_groups
        )
        unused = sorted(
            group.value for group in set_scoped_groups - set_registry_groups
        )
        raise ValueError(
            "Output contract scopes must cover the registry exactly "
            f"(missing={missing}, unused={unused})"
        )

    groups = tuple(
        OutputContractGroup(
            group=OutputGroup(group_name),
            ordinal=ordinal,
            scope=OUTPUT_GROUP_SCOPES[OutputGroup(group_name)],
        )
        for ordinal, group_name in enumerate(list_group_names)
    )
    return OutputContractCatalogue(groups=groups, variables=tuple(list_variables))


def output_contract_json_schema() -> dict[str, object]:
    """Return the JSON Schema for the in-memory output catalogue."""
    return OutputContractCatalogue.model_json_schema()


__all__ = [
    "OUTPUT_GROUP_SCOPES",
    "OutputContractCatalogue",
    "OutputContractScope",
    "output_contract_json_schema",
]
