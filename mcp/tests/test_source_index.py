from __future__ import annotations

import asyncio

from suews_mcp.backend.local import LocalBackend


def _make_backend(monkeypatch) -> LocalBackend:
    monkeypatch.setattr("suews_mcp.backend.local.shutil.which", lambda _: "/usr/local/bin/suews")
    return LocalBackend()


def test_source_index_query_matching(monkeypatch) -> None:
    backend = _make_backend(monkeypatch)
    backend._source_index = {
        "files": {
            "suews_phys_snow.f95": {
                "category": "physics",
                "description": "Snow physics",
                "modules": ["module_phys_snow"],
                "uses": ["module_phys_evap"],
                "subroutines": [
                    {
                        "name": "MeltHeat",
                        "signature": "SUBROUTINE MeltHeat(...)",
                        "line_start": 2,
                        "line_end": 3,
                        "calls": ["update_snow_albedo"],
                        "called_by": ["suews_ctrl_driver.f95:SUEWS_cal_Main"],
                    }
                ],
            }
        },
        "scheme_map": {"snow": "suews_phys_snow.f95"},
    }

    result = asyncio.run(backend.get_source_index("MeltHeat"))

    assert result["has_matches"] is True
    assert result["best_match"]["file_name"] == "suews_phys_snow.f95"


def test_source_excerpt_for_subroutine(monkeypatch) -> None:
    backend = _make_backend(monkeypatch)
    backend._source_index = {
        "files": {
            "suews_phys_snow.f95": {
                "subroutines": [
                    {
                        "name": "MeltHeat",
                        "signature": "SUBROUTINE MeltHeat(...)",
                        "line_start": 2,
                        "line_end": 3,
                    }
                ]
            }
        },
        "scheme_map": {},
    }

    monkeypatch.setattr(
        backend,
        "_load_source_text",
        lambda _: "line1\nSUBROUTINE MeltHeat(...)\nEND SUBROUTINE MeltHeat\nline4\n",
    )

    result = asyncio.run(
        backend.get_source_excerpt(file_name="suews_phys_snow.f95", subroutine="MeltHeat")
    )

    assert result["subroutine"] == "MeltHeat"
    assert "SUBROUTINE MeltHeat" in result["source"]
    assert "line1" not in result["source"]
