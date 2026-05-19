"""Unit tests for the ``search_schema`` MCP tool."""

from __future__ import annotations

import json
from typing import Any

import pytest

pytestmark = pytest.mark.api


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
