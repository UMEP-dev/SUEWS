"""Unit tests for the ``search_schema`` MCP tool."""

from __future__ import annotations

import json
from typing import Any

import pytest

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.hydro import WaterDistribution

pytestmark = [pytest.mark.api, pytest.mark.contract("C-API")]


def _schema_envelope(schema: dict[str, Any]) -> dict[str, Any]:
    return {
        "status": "success",
        "data": schema,
        "errors": [],
        "warnings": [],
        "meta": {"command": "suews schema --format json", "git_commit": "abc1234"},
    }


def test_search_schema_preserves_field_metadata(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Single-field matches keep scalar schema metadata instead of
    collapsing the whole field block to ``"<dict>"`` (gh#1411).
    """
    from suews_mcp.tools import schema_search

    schema = {
        "$defs": {
            "BldgsProperties": {
                "properties": {
                    "irrigation_fraction": {
                        "anyOf": [{"type": "number"}, {"type": "null"}],
                        "default": 0.0,
                        "description": "Fraction of surface area that can be irrigated",
                        "display_name": "Irrigation Fraction",
                        "title": "Irrigation Fraction",
                        "unit": "dimensionless",
                    }
                }
            }
        }
    }

    monkeypatch.setattr(
        schema_search,
        "run_suews_cli",
        lambda *_args, **_kwargs: _schema_envelope(schema),
    )

    result = schema_search.search_schema("irrigation_fraction")

    assert result["status"] == "success"
    field_match = next(
        match
        for match in result["data"]["matches"]
        if match["path"].endswith(".irrigation_fraction")
    )
    assert field_match["value"]["description"] == (
        "Fraction of surface area that can be irrigated"
    )
    assert field_match["value"]["default"] == 0.0
    assert field_match["value"]["unit"] == "dimensionless"
    assert field_match["value"]["anyOf"] == "<list>"
    assert field_match["value"] != "<dict>"
    assert len(json.dumps(field_match["value"])) < 3_000


def test_search_schema_expands_parent_properties_as_sorted_field_names(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Parent object matches expose a sorted field-name list so agents can
    enumerate YAML fields without a follow-up query per child (gh#1411).
    """
    from suews_mcp.tools import schema_search

    schema = {
        "$defs": {
            "EvetrProperties": {
                "description": "Evergreen tree properties.",
                "properties": {
                    "water_use": {"type": "number"},
                    "height": {"type": "number"},
                    "leaf_area_index": {"type": "number"},
                },
                "title": "EvetrProperties",
            }
        }
    }

    monkeypatch.setattr(
        schema_search,
        "run_suews_cli",
        lambda *_args, **_kwargs: _schema_envelope(schema),
    )

    result = schema_search.search_schema("EvetrProperties")

    parent_match = next(
        match
        for match in result["data"]["matches"]
        if match["path"] == "$defs.EvetrProperties"
    )
    assert parent_match["value"]["description"] == "Evergreen tree properties."
    assert parent_match["value"]["properties"] == [
        "height",
        "leaf_area_index",
        "water_use",
    ]
    assert parent_match["value"]["properties"] != "<dict>"
    assert len(json.dumps(parent_match["value"])) < 3_000


def test_shallow_truncates_long_string_scalars() -> None:
    from suews_mcp.tools.schema_search import _shallow

    result = _shallow({"description": "x" * 250})

    assert result["description"] == ("x" * 200) + "..."


def test_search_schema_surfaces_waterdistribution_rules_without_truncation(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """MCP ``search_schema`` displays the live WaterDistribution contract clearly."""
    mcp_schema_search = pytest.importorskip("suews_mcp.tools.schema_search")
    live_wd_schema = WaterDistribution.model_json_schema()
    live_schema = {
        "$defs": {
            "WaterDistribution": {
                key: value for key, value in live_wd_schema.items() if key != "$defs"
            }
        }
    }
    monkeypatch.setattr(mcp_schema_search, "_SCHEMA_CACHE", {})
    monkeypatch.setattr(
        mcp_schema_search,
        "run_suews_cli",
        lambda *_args, **_kwargs: _schema_envelope(live_schema),
    )

    for query, suffix in (
        ("WaterDistribution", "$defs.WaterDistribution"),
        ("to_soilstore", "$defs.WaterDistribution.properties.to_soilstore"),
        ("to_runoff", "$defs.WaterDistribution.properties.to_runoff"),
    ):
        result = mcp_schema_search.search_schema(query)
        assert result["status"] == "success"
        match = next(
            item for item in result["data"]["matches"] if item["path"] == suffix
        )
        desc = match["value"]["description"]
        assert "sfr > 0" in desc
        assert "0 normalises to absent at load" in desc
        assert "non-zero or post-load edits fail" in desc
        if suffix != "$defs.WaterDistribution":
            assert not desc.endswith("...")


def test_search_schema_surfaces_conditional_unit_metadata(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """MCP ``search_schema`` summarises ``conditional_unit`` instead of ``"<dict>"``."""
    mcp_schema_search = pytest.importorskip("suews_mcp.tools.schema_search")
    live_schema = SUEWSConfig.model_json_schema()
    monkeypatch.setattr(mcp_schema_search, "_SCHEMA_CACHE", {})
    monkeypatch.setattr(
        mcp_schema_search,
        "run_suews_cli",
        lambda *_args, **_kwargs: _schema_envelope(live_schema),
    )

    expectations = (
        (
            "drain_coef_1",
            "$defs.StorageDrainParams.properties.drain_coef_1",
            "drain_eq",
            {"1": "formula", "2": "cf", "3": "formula"},
        ),
        (
            "drain_coef_2",
            "$defs.StorageDrainParams.properties.drain_coef_2",
            "drain_eq",
            {"1": "cf", "2": "cf", "3": "cf"},
        ),
        (
            "g_q_base",
            "$defs.Conductance.properties.g_q_base",
            "model.physics.surface_conductance",
            {"1": "cf", "2": "cf"},
        ),
        (
            "g_q_shape",
            "$defs.Conductance.properties.g_q_shape",
            "model.physics.surface_conductance",
            {"1": "cf", "2": "empirical_base"},
        ),
        (
            "traffic_rate",
            "$defs.CO2Params.properties.traffic_rate",
            "type_traffic_rate",
            {"1": "cf", "2": "cf"},
        ),
    )

    for query, path, selector, expected_kinds in expectations:
        result = mcp_schema_search.search_schema(query)
        assert result["status"] == "success"
        match = next(
            item for item in result["data"]["matches"] if item["path"] == path
        )
        value = match["value"]
        assert "unit" not in value
        cond = value["conditional_unit"]
        assert isinstance(cond, dict)
        assert cond["selector"] == selector
        assert {
            code: case["kind"] for code, case in cond["cases"].items()
        } == expected_kinds
        assert len(json.dumps(value)) < 3_000
