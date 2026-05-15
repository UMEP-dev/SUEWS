"""Unit tests for the ``compare_runs`` MCP tool."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def _stub_envelope(captured: dict):
    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        envelope = {
            "status": "success",
            "data": {"metrics": {"QH": {"rmse": 1.0, "bias": 0.1, "r": 0.99}}},
            "errors": [],
            "warnings": [],
            "meta": {"command": " ".join(cmd)},
        }
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    return fake_run


def test_compare_uses_two_paths_and_format(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import compare_runs

    run_a = tmp_path / "runA"
    run_b = tmp_path / "runB"
    run_a.mkdir()
    run_b.mkdir()

    captured: dict = {}
    monkeypatch.setattr(subprocess, "run", _stub_envelope(captured))

    result = compare_runs(str(run_a), str(run_b), project_root=str(tmp_path))
    assert result["status"] == "success"
    assert captured["cmd"][:2] == ["suews", "compare"]
    assert str(run_a) in captured["cmd"]
    assert str(run_b) in captured["cmd"]


def test_compare_forwards_metrics_and_variables(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import compare_runs

    run_a = tmp_path / "runA"
    run_b = tmp_path / "runB"
    run_a.mkdir()
    run_b.mkdir()

    captured: dict = {}
    monkeypatch.setattr(subprocess, "run", _stub_envelope(captured))

    compare_runs(
        str(run_a),
        str(run_b),
        project_root=str(tmp_path),
        metrics="rmse,bias",
        variables="QH",
    )
    assert "--metrics" in captured["cmd"]
    assert "rmse,bias" in captured["cmd"]
    assert "--variables" in captured["cmd"]
    assert "QH" in captured["cmd"]


def test_compare_rejects_path_outside_root(tmp_path: Path) -> None:
    from suews_mcp.tools import compare_runs

    (tmp_path / "case").mkdir()
    result = compare_runs(
        "/etc/passwd", "case", project_root=str(tmp_path / "case")
    )
    assert result["status"] == "error"
