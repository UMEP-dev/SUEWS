"""Unit tests for the ``summarise_run`` MCP tool."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def _stub_envelope(captured: dict) -> "callable":
    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        envelope = {
            "status": "success",
            "data": {"summary": {"QH": {"mean": 100.0, "min": 0.0, "max": 200.0}}},
            "errors": [],
            "warnings": [],
            "meta": {"command": " ".join(cmd)},
        }
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    return fake_run


def test_summarise_passes_run_dir_and_format(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import summarise_run

    run_dir = tmp_path / "run01"
    run_dir.mkdir()

    captured: dict = {}
    monkeypatch.setattr(subprocess, "run", _stub_envelope(captured))

    result = summarise_run(str(run_dir), project_root=str(tmp_path))
    assert result["status"] == "success"
    assert captured["cmd"][:2] == ["suews", "summarise"]
    assert str(run_dir) in captured["cmd"]
    assert "--format" in captured["cmd"]


def test_summarise_forwards_variables(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import summarise_run

    run_dir = tmp_path / "run01"
    run_dir.mkdir()

    captured: dict = {}
    monkeypatch.setattr(subprocess, "run", _stub_envelope(captured))

    summarise_run(
        str(run_dir), project_root=str(tmp_path), variables="QH,QE,QN"
    )
    assert "--variables" in captured["cmd"]
    assert "QH,QE,QN" in captured["cmd"]


def test_summarise_sandbox_violation_returns_error(tmp_path: Path) -> None:
    from suews_mcp.tools import summarise_run

    (tmp_path / "case").mkdir()
    result = summarise_run("/etc/passwd", project_root=str(tmp_path / "case"))
    assert result["status"] == "error"
    assert any("outside the project root" in str(e) for e in result["errors"])
