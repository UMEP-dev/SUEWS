from __future__ import annotations

import asyncio
from pathlib import Path
from typing import Any

from suews_mcp.backend.base import SUEWSBackend, SimulationResult
from suews_mcp.tools.search import handle_search


class StubBackend(SUEWSBackend):
    def __init__(self) -> None:
        self.calls: list[tuple[str, Any]] = []

    async def list_types(self, query: str = "") -> dict[str, Any]:
        self.calls.append(("list_types", query))
        return {
            "query": query,
            "total_types": 2,
            "category_counts": {"config": 1, "param": 1},
            "categories": {
                "config": [
                    {
                        "type_name": "suews-config",
                        "description": "Top-level model configuration.",
                        "manifest_name": "SUEWS_CONFIG",
                    }
                ],
                "param": [
                    {
                        "type_name": "ohm-prm",
                        "description": "Core OHM parameter set.",
                        "manifest_name": "OHM_PRM",
                    }
                ],
            },
        }

    async def get_schema(self, type_name: str, detail_level: str) -> dict[str, Any]:
        self.calls.append(("get_schema", (type_name, detail_level)))
        return {"type_name": type_name, "detail_level": detail_level}

    async def run_simulation(self, config_yaml: str, work_dir: Path) -> SimulationResult:
        raise NotImplementedError

    async def get_knowledge(self, topic: str) -> dict[str, Any]:
        raise NotImplementedError

    async def get_source_index(self, query: str) -> dict[str, Any]:
        raise NotImplementedError

    async def get_source_excerpt(
        self, file_name: str, subroutine: str | None = None
    ) -> dict[str, Any]:
        raise NotImplementedError

    async def get_diagnostic(
        self,
        variable: str,
        symptom: str | None = None,
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        raise NotImplementedError

    async def search_catalogue(self, query: str) -> dict[str, Any]:
        self.calls.append(("search_catalogue", query))
        return {
            "query": query,
            "matched_sections": [
                {
                    "section": "forcing_variables",
                    "description": "Forcing data requirements",
                    "score": 3,
                    "content": {"description": "Test forcing info"},
                }
            ],
            "available_sections": [
                "sample_datasets",
                "forcing_variables",
                "surface_parameter_groups",
                "external_data_sources",
            ],
        }


def test_search_without_type_calls_both_index_and_catalogue() -> None:
    backend = StubBackend()
    result = asyncio.run(handle_search(backend=backend, query="forcing"))

    call_names = [name for name, _ in backend.calls]
    assert "list_types" in call_names
    assert "search_catalogue" in call_names


def test_search_without_type_returns_compact_types() -> None:
    backend = StubBackend()
    result = asyncio.run(handle_search(backend=backend, query="ohm"))

    # Should have compact type entries (no manifest_name in compact view)
    types = result["types"]
    assert types["total"] == 2
    config_entries = types["categories"]["config"]
    assert len(config_entries) == 1
    assert "type_name" in config_entries[0]
    assert "description" in config_entries[0]
    # manifest_name should be stripped in compact mode
    assert "manifest_name" not in config_entries[0]


def test_search_without_type_includes_catalogue() -> None:
    backend = StubBackend()
    result = asyncio.run(handle_search(backend=backend, query="forcing"))

    catalogue = result["data_catalogue"]
    assert "matched_sections" in catalogue
    assert len(catalogue["matched_sections"]) > 0
    assert catalogue["matched_sections"][0]["section"] == "forcing_variables"


def test_search_with_type_uses_schema_lookup() -> None:
    backend = StubBackend()
    result = asyncio.run(
        handle_search(
            backend=backend,
            query="irrelevant",
            type_name="ohm-state",
            detail_level="schema",
        )
    )

    assert result["type_name"] == "ohm-state"
    assert result["detail_level"] == "schema"
    assert backend.calls == [("get_schema", ("ohm-state", "schema"))]


def test_search_with_type_does_not_call_catalogue() -> None:
    backend = StubBackend()
    asyncio.run(
        handle_search(
            backend=backend,
            query="x",
            type_name="ohm-state",
            detail_level="index",
        )
    )

    call_names = [name for name, _ in backend.calls]
    assert "search_catalogue" not in call_names


def test_search_rejects_invalid_detail_level() -> None:
    backend = StubBackend()

    try:
        asyncio.run(
            handle_search(
                backend=backend,
                query="x",
                type_name="ohm-state",
                detail_level="full",
            )
        )
    except ValueError as exc:
        assert "Invalid detail_level" in str(exc)
    else:
        raise AssertionError("Expected ValueError for invalid detail_level")
