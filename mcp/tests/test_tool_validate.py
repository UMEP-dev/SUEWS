"""Unit tests for the ``validate_config`` MCP tool."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def test_returns_envelope_from_subprocess(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import validate_config

    config = tmp_path / "config.yml"
    config.write_text("x: 1", encoding="utf-8")

    envelope = {
        "status": "success",
        "data": {"errors": [], "warnings": [], "schema_version": "2026.5.dev5"},
        "errors": [],
        "warnings": [],
        "meta": {"command": "suews validate config.yml --format json"},
    }

    def fake_run(cmd, **_kwargs):
        assert cmd[:2] == ["suews", "validate"]
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    monkeypatch.setattr(subprocess, "run", fake_run)
    result = validate_config(str(config), project_root=str(tmp_path))
    assert result == envelope


def test_sandbox_violation_returns_error_envelope(tmp_path: Path) -> None:
    from suews_mcp.tools import validate_config

    (tmp_path / "case").mkdir()
    result = validate_config("/etc/passwd", project_root=str(tmp_path / "case"))
    assert result["status"] == "error"
    assert any("outside the project root" in str(e) for e in result["errors"])


def test_executable_missing_returns_error_envelope(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import validate_config

    config = tmp_path / "config.yml"
    config.write_text("x: 1", encoding="utf-8")

    def fake_run(cmd, **_kwargs):
        raise FileNotFoundError(cmd[0])

    monkeypatch.setattr(subprocess, "run", fake_run)
    result = validate_config(str(config), project_root=str(tmp_path))
    assert result["status"] == "error"
