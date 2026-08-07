"""Tests for the output-owned contract projection."""

import importlib
import json
from pathlib import Path
import subprocess
import sys

from jsonschema import Draft202012Validator, ValidationError
import pytest

from supy.data_model.output import (
    OUTPUT_GROUP_SCOPES,
    OUTPUT_REGISTRY,
    OutputContractScope,
    get_output_contract_catalogue,
    output_contract_json_schema,
)
from supy.data_model.output.version import CURRENT_OUTPUT_VERSION

pytestmark = pytest.mark.api

_GROUP_VARIABLE_EXPORTS = (
    ("DATETIME_VARIABLES", "datetime_vars"),
    ("SUEWS_VARIABLES", "suews_vars"),
    ("SNOW_VARIABLES", "snow_vars"),
    ("ESTM_VARIABLES", "estm_vars"),
    ("RSL_VARIABLES", "rsl_vars"),
    ("DAILYSTATE_VARIABLES", "dailystate_vars"),
    ("BL_VARIABLES", "bl_vars"),
    ("BEERS_VARIABLES", "beers_vars"),
    ("DEBUG_VARIABLES", "debug_vars"),
    ("EHC_VARIABLES", "ehc_vars"),
    ("SPARTACUS_VARIABLES", "spartacus_vars"),
    ("STEBBS_VARIABLES", "stebbs_vars"),
    ("NHOOD_VARIABLES", "nhood_vars"),
)


def test_version_import_does_not_construct_contract_catalogue() -> None:
    """Keep ordinary output imports independent of the contract projection."""
    data_model_dir = Path(__file__).resolve().parents[2] / "src" / "supy" / "data_model"
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            (
                "import output.version; "
                "from output.registry import get_output_contract_catalogue; "
                "assert get_output_contract_catalogue.cache_info().currsize == 0"
            ),
        ],
        check=False,
        cwd=data_model_dir,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stderr


def _value(value):
    """Return an enum value while accepting Pydantic's value conversion."""
    return value.value if hasattr(value, "value") else value


@pytest.mark.parametrize(("attribute", "module_name"), _GROUP_VARIABLE_EXPORTS)
def test_group_variable_lists_remain_package_attributes(attribute, module_name):
    """Keep the explicit package imports available after moving assembly."""
    output_package = importlib.import_module("supy.data_model.output")
    definition_module = importlib.import_module(f"supy.data_model.output.{module_name}")

    assert getattr(output_package, attribute) is getattr(definition_module, attribute)


def test_catalogue_is_an_exact_registry_projection():
    """Project identity, metadata, and group-relative order without duplication."""
    catalogue = get_output_contract_catalogue()
    assert get_output_contract_catalogue() is catalogue
    assert len(catalogue.variables) == len(OUTPUT_REGISTRY.variables)

    dict_next_ordinal: dict[str, int] = {}
    set_identities: set[tuple[str, str]] = set()
    for projected, registered in zip(
        catalogue.variables,
        OUTPUT_REGISTRY.variables,
        strict=True,
    ):
        group = str(_value(registered.group))
        expected_ordinal = dict_next_ordinal.setdefault(group, 0)
        assert (
            projected.group,
            projected.name,
            projected.ordinal,
            projected.unit,
            projected.description,
            projected.aggregation,
            projected.level,
        ) == (
            group,
            registered.name,
            expected_ordinal,
            registered.unit,
            registered.description,
            _value(registered.aggregation),
            _value(registered.level),
        )
        set_identities.add((projected.group, projected.name))
        dict_next_ordinal[group] += 1

    assert len(set_identities) == len(catalogue.variables)


def test_group_order_and_scopes_are_explicit():
    """Classify every registry group once without changing registry order."""
    catalogue = get_output_contract_catalogue()
    list_registry_groups = list(
        dict.fromkeys(
            str(_value(variable.group)) for variable in OUTPUT_REGISTRY.variables
        )
    )
    assert [group.group for group in catalogue.groups] == list_registry_groups
    assert [group.ordinal for group in catalogue.groups] == list(
        range(len(list_registry_groups))
    )
    assert {group.group: group.scope for group in catalogue.groups} == {
        group.value: scope.value for group, scope in OUTPUT_GROUP_SCOPES.items()
    }

    assert {
        group.group
        for group in catalogue.groups
        if group.scope == OutputContractScope.STABLE
    } == {"SUEWS", "snow", "ESTM", "RSL", "BL", "DailyState"}


def test_representation_metadata_is_uniform_and_unpublished():
    """Declare shared representation metadata without publishing a version."""
    assert get_output_contract_catalogue().representation.model_dump() == {
        "value_type": "number",
        "shape": "scalar",
        "missing_values": {
            "dataframe": "nan",
            "text": "sentinel:-999.0",
            "parquet": "null",
        },
    }
    assert CURRENT_OUTPUT_VERSION is None


def test_catalogue_json_schema_is_valid_and_deterministic():
    """Validate the deterministic JSON projection with its generated schema."""
    schema_first = output_contract_json_schema()
    schema_second = output_contract_json_schema()
    schema_first_json = json.dumps(
        schema_first,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )
    schema_second_json = json.dumps(
        schema_second,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )
    assert schema_first is not schema_second
    assert schema_first_json == schema_second_json
    Draft202012Validator.check_schema(schema_first)

    document = get_output_contract_catalogue().model_dump(mode="json")
    Draft202012Validator(schema_first).validate(document)
    serialised = json.dumps(
        document,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )
    assert json.loads(serialised) == document

    invalid_document = {**document, "kind": "forcing"}
    with pytest.raises(ValidationError):
        Draft202012Validator(schema_first).validate(invalid_document)
