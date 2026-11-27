"""Unit tests for the ``suews://runs/...`` resource."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

pytestmark = [pytest.mark.unit, pytest.mark.contract("C-ERROR")]


def test_provenance_resource_returns_payload(tmp_path: Path) -> None:
    from suews_mcp.resources.runs import read_run_resource

    run_dir = tmp_path / "run01"
    run_dir.mkdir()
    sample = {"command": "suews run", "schema_version": "2026.5.dev5"}
    (run_dir / "provenance.json").write_text(
        json.dumps(sample), encoding="utf-8"
    )

    result = read_run_resource("run01", "provenance", project_root=str(tmp_path))
    assert result["status"] == "success"
    assert result["data"]["payload"] == sample


def test_diagnostics_resource_returns_payload(tmp_path: Path) -> None:
    from suews_mcp.resources.runs import read_run_resource

    run_dir = tmp_path / "run01"
    run_dir.mkdir()
    sample = {"checks": [], "summary": {"n_pass": 0, "n_warn": 0, "n_fail": 0}}
    (run_dir / "diagnostics.json").write_text(
        json.dumps(sample), encoding="utf-8"
    )

    result = read_run_resource("run01", "diagnostics", project_root=str(tmp_path))
    assert result["status"] == "success"
    assert result["data"]["payload"] == sample


def test_unknown_kind_returns_error(tmp_path: Path) -> None:
    from suews_mcp.resources.runs import read_run_resource

    (tmp_path / "run01").mkdir()
    result = read_run_resource("run01", "bogus", project_root=str(tmp_path))
    assert result["status"] == "error"


def test_missing_provenance_returns_error(tmp_path: Path) -> None:
    from suews_mcp.resources.runs import read_run_resource

    (tmp_path / "run01").mkdir()
    result = read_run_resource("run01", "provenance", project_root=str(tmp_path))
    assert result["status"] == "error"


def test_path_outside_root_rejected(tmp_path: Path) -> None:
    from suews_mcp.resources.runs import read_run_resource

    (tmp_path / "case").mkdir()
    result = read_run_resource(
        "/etc/passwd", "provenance", project_root=str(tmp_path / "case")
    )
    assert result["status"] == "error"


@pytest.mark.parametrize("kind", ["provenance", "diagnostics"])
def test_corrupt_json_returns_error(tmp_path: Path, kind: str) -> None:
    from suews_mcp.resources.runs import read_run_resource

    # ARRANGE
    run_dir = tmp_path / "run01"
    run_dir.mkdir()
    (run_dir / f"{kind}.json").write_text("{truncated", encoding="utf-8")

    # ACT
    result = read_run_resource("run01", kind, project_root=str(tmp_path))

    # ASSERT
    assert result["status"] == "error"
    assert result["errors"]


def test_non_utf8_sidecar_returns_error(tmp_path: Path) -> None:
    from suews_mcp.resources.runs import read_run_resource

    # ARRANGE
    run_dir = tmp_path / "run01"
    run_dir.mkdir()
    (run_dir / "provenance.json").write_bytes(b"\xff\xfe\x00\x00")

    # ACT
    result = read_run_resource("run01", "provenance", project_root=str(tmp_path))

    # ASSERT
    assert result["status"] == "error"
    assert result["errors"]


def test_missing_diagnostics_error_interpolates_run_dir(tmp_path: Path) -> None:
    from suews_mcp.resources.runs import read_run_resource

    # ARRANGE
    run_dir = tmp_path / "run01"
    run_dir.mkdir()

    # ACT
    result = read_run_resource("run01", "diagnostics", project_root=str(tmp_path))

    # ASSERT
    assert result["status"] == "error"
    message = result["errors"][0]["message"]
    assert "{run_dir}" not in message
    assert f"{run_dir}/diagnostics.json" in message
