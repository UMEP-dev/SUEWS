"""Tests for published forcing and output schemas and catalogues."""

from __future__ import annotations

import jsonschema
import pytest

from supy.data_model.output import OUTPUT_REGISTRY
from supy.data_model.schema.interfaces import (
    export_data_interface_artifacts,
    generate_data_interface_catalogue,
    generate_data_interface_schema,
)

pytestmark = pytest.mark.api


@pytest.mark.parametrize("kind", ["forcing", "output"])
def test_catalogue_validates_against_generated_schema(kind: str) -> None:
    schema = generate_data_interface_schema(kind)
    catalogue = generate_data_interface_catalogue(kind)

    jsonschema.Draft202012Validator.check_schema(schema)
    jsonschema.Draft202012Validator(schema).validate(catalogue)
    assert catalogue["kind"] == kind
    assert catalogue["catalogue_version"] == schema["version"]
    assert catalogue["$schema"] == schema["$id"]


def test_forcing_catalogue_contains_contract_metadata() -> None:
    catalogue = generate_data_interface_catalogue("forcing")
    variables = {variable["name"]: variable for variable in catalogue["variables"]}

    assert variables["pres"]["unit"] == "kPa"
    assert variables["Wuh"]["unit"] == "mm"
    assert variables["Wuh"]["temporal_semantics"] == "sum"
    assert variables["Wuh"]["missing_value_policy"] == "allowed_unless_physics_required"
    assert variables["Wuh"]["valid_range"] == {"minimum": 0.0, "maximum": 10.0}
    assert variables["Wuh"]["surface_suffixes"] == [
        "paved",
        "bldgs",
        "evetr",
        "dectr",
        "grass",
        "bsoil",
        "water",
    ]
    assert variables["isec"]["input_column"] is False


def test_output_catalogue_uses_existing_registry() -> None:
    catalogue = generate_data_interface_catalogue("output")
    assert len(catalogue["variables"]) == len(OUTPUT_REGISTRY.variables)


def test_unknown_data_interface_version_is_rejected() -> None:
    with pytest.raises(ValueError, match="not available"):
        generate_data_interface_schema("forcing", "999.0.0")


def test_unknown_data_interface_kind_is_rejected() -> None:
    with pytest.raises(ValueError, match="choose 'forcing' or 'output'"):
        generate_data_interface_schema("configuration")


def test_export_writes_versioned_and_latest_artifacts(tmp_path) -> None:
    exported = export_data_interface_artifacts(
        tmp_path,
        base_url="https://example.test/preview",
    )

    for kind, paths in exported.items():
        schema = paths["schema"].read_text(encoding="utf-8")
        catalogue = paths["catalogue"].read_text(encoding="utf-8")
        assert paths["schema_latest"].read_text(encoding="utf-8") == schema
        assert paths["catalogue_latest"].read_text(encoding="utf-8") == catalogue
        assert f"https://example.test/preview/schemas/{kind}-variables/" in schema
        assert f'"kind": "{kind}"' in catalogue
