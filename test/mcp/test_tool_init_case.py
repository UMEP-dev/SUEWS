"""Unit tests for the ``init_case`` MCP tool."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def test_init_uses_simple_urban_default(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import init_case

    target = tmp_path / "case01"

    captured: dict = {}

    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        envelope = {
            "status": "success",
            "data": {"created_files": ["config.yml"]},
            "errors": [],
            "warnings": [],
            "meta": {"command": " ".join(cmd)},
        }
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    monkeypatch.setattr(subprocess, "run", fake_run)
    result = init_case(str(target), project_root=str(tmp_path))
    assert result["status"] == "success"
    assert "--template" in captured["cmd"]
    assert "simple-urban" in captured["cmd"]


def test_init_rejects_unknown_template(tmp_path: Path) -> None:
    from suews_mcp.tools import init_case

    result = init_case(
        str(tmp_path / "case01"),
        project_root=str(tmp_path),
        template="rogue",
    )
    assert result["status"] == "error"
    assert any("not in the allow-list" in str(e) for e in result["errors"])


def test_init_sandbox_violation_returns_error(tmp_path: Path) -> None:
    from suews_mcp.tools import init_case

    (tmp_path / "case").mkdir()
    result = init_case("/etc/case", project_root=str(tmp_path / "case"))
    assert result["status"] == "error"
