from __future__ import annotations

import asyncio
from pathlib import Path
from typing import Any

from suews_mcp.backend.base import SUEWSBackend, SimulationResult
from suews_mcp.tools.explain import handle_explain


class StubBackend(SUEWSBackend):
    def __init__(self) -> None:
        self.diagnostic_calls: list[tuple[str, str | None, dict[str, Any] | None]] = []

    async def list_types(self, query: str = "") -> dict[str, Any]:
        raise NotImplementedError

    async def get_schema(self, type_name: str, detail_level: str) -> dict[str, Any]:
        raise NotImplementedError

    async def run_simulation(self, config_yaml: str, work_dir: Path) -> SimulationResult:
        raise NotImplementedError

    async def get_knowledge(self, topic: str) -> dict[str, Any]:
        return {
            "topic": topic,
            "matched_topic": "energy-balance",
            "score": 3,
            "content": "A" * 100,
            "available_topics": ["energy-balance"],
        }

    async def get_source_index(self, query: str) -> dict[str, Any]:
        lowered = query.lower()
        if "snow" not in lowered and "meltheat" not in lowered:
            return {
                "query": query,
                "has_matches": False,
                "best_score": 0,
                "available_files": 0,
                "matches": [],
                "best_match": None,
            }

        return {
            "query": query,
            "has_matches": True,
            "best_score": 5,
            "available_files": 1,
            "matches": [
                {
                    "file_name": "suews_phys_snow.f95",
                    "score": 5,
                    "subroutines": [
                        {
                            "name": "MeltHeat",
                            "line_start": 10,
                            "line_end": 25,
                            "signature": "SUBROUTINE MeltHeat(...)",
                        }
                    ],
                }
            ],
            "best_match": {
                "file_name": "suews_phys_snow.f95",
                "score": 5,
                "subroutines": [
                    {
                        "name": "MeltHeat",
                        "line_start": 10,
                        "line_end": 25,
                        "signature": "SUBROUTINE MeltHeat(...)",
                    }
                ],
            },
        }

    async def get_source_excerpt(
        self,
        file_name: str,
        subroutine: str | None = None,
    ) -> dict[str, Any]:
        return {
            "file_name": file_name,
            "subroutine": subroutine,
            "signature": "SUBROUTINE MeltHeat(...)" if subroutine else None,
            "line_start": 10,
            "line_end": 25,
            "total_lines": 200,
            "source": "SUBROUTINE MeltHeat(...)\nEND SUBROUTINE MeltHeat\n",
        }

    async def get_diagnostic(
        self,
        variable: str,
        symptom: str | None = None,
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        self.diagnostic_calls.append((variable, symptom, context))
        return {
            "matched_variable": variable,
            "detected_symptom": symptom,
            "diagnosis": "diagnostic result",
            "controlling_params": [],
            "suggested_adjustments": [],
            "search_calls": [],
            "next_steps": [],
        }

    async def search_catalogue(self, query: str) -> dict[str, Any]:
        raise NotImplementedError


def test_explain_returns_knowledge() -> None:
    result = asyncio.run(handle_explain(backend=StubBackend(), topic="energy"))

    assert result["mode"] == "concept"
    assert result["matched_topic"] == "energy-balance"
    assert result["truncated"] is False


def test_explain_applies_truncation() -> None:
    result = asyncio.run(
        handle_explain(backend=StubBackend(), topic="energy", max_chars=10)
    )

    assert result["truncated"] is True
    assert result["content"].endswith("...")


def test_explain_routes_to_source_index_mode() -> None:
    result = asyncio.run(handle_explain(backend=StubBackend(), topic="snow physics"))

    assert result["mode"] == "source"
    assert result["show_source"] is False
    assert result["best_match"]["file_name"] == "suews_phys_snow.f95"


def test_explain_routes_to_source_excerpt_with_show_source() -> None:
    result = asyncio.run(
        handle_explain(
            backend=StubBackend(),
            topic="MeltHeat",
            show_source=True,
        )
    )

    assert result["mode"] == "source"
    assert result["show_source"] is True
    assert "SUBROUTINE MeltHeat" in result["source"]


def test_explain_routes_to_diagnostic_mode() -> None:
    backend = StubBackend()
    result = asyncio.run(
        handle_explain(
            backend=backend,
            topic="LAI peaks too early",
            context={"LAI_DecTr_peak_month": 2},
        )
    )

    assert result["mode"] == "diagnostic"
    assert result["detected_symptom"] == "peaks_too_early"
    assert backend.diagnostic_calls[0][0] == "LAI"
    assert backend.diagnostic_calls[0][1] == "peaks_too_early"
