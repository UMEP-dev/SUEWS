"""Tests for data catalogue search in LocalBackend."""

from __future__ import annotations

import asyncio
import json
from pathlib import Path
from typing import Any
from unittest.mock import patch

import pytest

from suews_mcp.backend.local import LocalBackend


@pytest.fixture()
def catalogue_data() -> dict[str, Any]:
    """Load the real data catalogue JSON."""
    catalogue_path = (
        Path(__file__).resolve().parents[1]
        / "src"
        / "suews_mcp"
        / "data"
        / "data-catalogue.json"
    )
    return json.loads(catalogue_path.read_text(encoding="utf-8"))


def _make_backend_with_catalogue(catalogue: dict[str, Any]) -> LocalBackend:
    """Create a LocalBackend with a custom catalogue, bypassing CLI checks."""
    with patch.object(LocalBackend, "__init__", lambda self, **kw: None):
        backend = LocalBackend.__new__(LocalBackend)
        backend._data_catalogue = catalogue
        backend._catalogue_text_cache = backend._build_catalogue_text_cache()
    return backend


class TestCatalogueSearch:
    def test_forcing_query_matches_forcing_section(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        backend = _make_backend_with_catalogue(catalogue_data)
        result = asyncio.run(backend.search_catalogue("forcing requirements"))
        sections = [s["section"] for s in result["matched_sections"]]
        assert "forcing_variables" in sections

    def test_era5_query_matches_external_sources(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        backend = _make_backend_with_catalogue(catalogue_data)
        result = asyncio.run(backend.search_catalogue("ERA5 download"))
        sections = [s["section"] for s in result["matched_sections"]]
        assert "external_data_sources" in sections

    def test_sample_query_matches_sample_datasets(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        backend = _make_backend_with_catalogue(catalogue_data)
        result = asyncio.run(backend.search_catalogue("sample data London"))
        sections = [s["section"] for s in result["matched_sections"]]
        assert "sample_datasets" in sections

    def test_land_cover_query_matches_surface_params(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        backend = _make_backend_with_catalogue(catalogue_data)
        result = asyncio.run(backend.search_catalogue("land cover fractions"))
        sections = [s["section"] for s in result["matched_sections"]]
        assert "surface_parameter_groups" in sections

    def test_empty_catalogue_returns_message(self) -> None:
        backend = _make_backend_with_catalogue({})
        result = asyncio.run(backend.search_catalogue("anything"))
        assert "not available" in result.get("message", "")

    def test_no_match_returns_all_sections(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        backend = _make_backend_with_catalogue(catalogue_data)
        result = asyncio.run(backend.search_catalogue("xyzzy_no_match_99"))
        # Should return compact summaries of all sections
        assert len(result["matched_sections"]) > 0

    def test_result_has_available_sections(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        backend = _make_backend_with_catalogue(catalogue_data)
        result = asyncio.run(backend.search_catalogue("forcing"))
        assert "available_sections" in result
        assert "forcing_variables" in result["available_sections"]
        assert "sample_datasets" in result["available_sections"]

    def test_limits_to_two_sections(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        backend = _make_backend_with_catalogue(catalogue_data)
        # A broad query that could match many sections
        result = asyncio.run(backend.search_catalogue("SUEWS data parameters"))
        assert len(result["matched_sections"]) <= 2


class TestCatalogueContent:
    """Verify the data catalogue JSON has the expected structure."""

    def test_has_required_top_level_keys(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        for key in [
            "sample_datasets",
            "forcing_variables",
            "surface_parameter_groups",
            "external_data_sources",
        ]:
            assert key in catalogue_data, f"Missing top-level key: {key}"

    def test_forcing_has_required_and_optional(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        forcing = catalogue_data["forcing_variables"]
        assert len(forcing["required_variables"]) == 6
        assert len(forcing["optional_variables"]) > 0

    def test_forcing_minimum_set(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        minimum = catalogue_data["forcing_variables"]["minimum_forcing_set"]
        expected = {"kdown", "Tair", "RH", "pres", "U", "rain"}
        assert set(minimum["variables"]) == expected

    def test_land_cover_types_count(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        types = catalogue_data["surface_parameter_groups"]["land_cover_types"]
        assert len(types) == 7

    def test_each_land_cover_has_schema_type(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        types = catalogue_data["surface_parameter_groups"]["land_cover_types"]
        for lc in types:
            assert "schema_type" in lc, f"Missing schema_type for {lc.get('name')}"
            assert lc["schema_type"].startswith("lc-"), (
                f"Unexpected schema_type: {lc['schema_type']}"
            )

    def test_external_sources_have_ids(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        sources = catalogue_data["external_data_sources"]
        ids = {s["id"] for s in sources}
        assert "era5" in ids
        assert "epw" in ids

    def test_sample_dataset_has_kcl(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        samples = catalogue_data["sample_datasets"]
        ids = {s["id"] for s in samples}
        assert "kcl-2012" in ids

    def test_search_aliases_present(
        self, catalogue_data: dict[str, Any]
    ) -> None:
        aliases = catalogue_data.get("search_aliases", {})
        assert "forcing" in aliases
        assert "era5" in aliases
        assert "sample" in aliases
