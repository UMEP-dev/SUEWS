from __future__ import annotations

import asyncio

from suews_mcp.backend.local import LocalBackend


def _make_backend(monkeypatch) -> LocalBackend:
    monkeypatch.setattr("suews_mcp.backend.local.shutil.which", lambda _: "/usr/local/bin/suews")
    return LocalBackend()


def test_diagnostic_detects_lai_symptom_from_context(monkeypatch) -> None:
    backend = _make_backend(monkeypatch)
    backend._diagnostic_rules = {
        "variables": {
            "LAI": {
                "description": "Leaf area index",
                "output_columns": ["LAI_DecTr"],
                "controlling_params": [{"type": "lai-prm", "field": "baset"}],
                "symptoms": {
                    "peaks_too_early": {
                        "likely_cause": "baset too low",
                        "suggestion": "increase baset",
                        "parameter_hints": [
                            {
                                "type": "lai-prm",
                                "field": "baset",
                                "suggested": "7-8",
                                "reason": "delay onset",
                            }
                        ],
                    }
                },
            }
        }
    }

    result = asyncio.run(
        backend.get_diagnostic(
            variable="LAI",
            context={"LAI_DecTr_peak_month": 2, "baset": 5.0},
        )
    )

    assert result["matched_variable"] == "LAI"
    assert result["detected_symptom"] == "peaks_too_early"
    assert result["suggested_adjustments"][0]["current"] == 5.0


def test_diagnostic_returns_fallback_for_unknown_variable(monkeypatch) -> None:
    backend = _make_backend(monkeypatch)
    backend._diagnostic_rules = {"variables": {"LAI": {}}}

    result = asyncio.run(backend.get_diagnostic(variable="unknown_metric"))

    assert result["matched_variable"] is None
    assert result["available_variables"] == ["LAI"]
