"""Unit tests for the ``diagnose_run`` MCP tool."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def test_diagnose_returns_envelope_from_subprocess(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import diagnose_run

    run_dir = tmp_path / "run01"
    run_dir.mkdir()

    envelope = {
        "status": "success",
        "data": {
            "checks": [{"name": "provenance_present", "status": "pass"}],
            "summary": {"n_pass": 1, "n_warn": 0, "n_fail": 0},
        },
        "errors": [],
        "warnings": [],
        "meta": {"command": "suews diagnose"},
    }

    def fake_run(cmd, **_kwargs):
        assert cmd[:2] == ["suews", "diagnose"]
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    monkeypatch.setattr(subprocess, "run", fake_run)
    result = diagnose_run(str(run_dir), project_root=str(tmp_path))
    assert result == envelope


def test_diagnose_sandbox_violation_returns_error(tmp_path: Path) -> None:
    from suews_mcp.tools import diagnose_run

    (tmp_path / "case").mkdir()
    result = diagnose_run("/etc/passwd", project_root=str(tmp_path / "case"))
    assert result["status"] == "error"
