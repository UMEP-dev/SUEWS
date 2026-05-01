"""Unit tests for the ``convert_config`` MCP tool."""

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
            "data": {"output": "config.yml"},
            "errors": [],
            "warnings": [],
            "meta": {"command": " ".join(cmd)},
        }
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    return fake_run


def test_convert_passes_input_and_output(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import convert_config

    src = tmp_path / "RunControl.nml"
    src.write_text("&runcontrol", encoding="utf-8")
    dst = tmp_path / "out.yml"

    captured: dict = {}
    monkeypatch.setattr(subprocess, "run", _stub_envelope(captured))

    result = convert_config(str(src), str(dst), project_root=str(tmp_path))
    assert result["status"] == "success"
    assert "-i" in captured["cmd"]
    assert "-o" in captured["cmd"]
    assert str(src) in captured["cmd"]
    assert str(dst) in captured["cmd"]


def test_convert_forwards_optional_flags(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import convert_config

    src = tmp_path / "old.yml"
    src.write_text("schema_version: 2026.1", encoding="utf-8")
    dst = tmp_path / "new.yml"
    debug = tmp_path / "debug"

    captured: dict = {}
    monkeypatch.setattr(subprocess, "run", _stub_envelope(captured))

    convert_config(
        str(src),
        str(dst),
        project_root=str(tmp_path),
        from_version="2026.1.28",
        debug_dir=str(debug),
        no_profile_validation=True,
    )
    assert "-f" in captured["cmd"]
    assert "2026.1.28" in captured["cmd"]
    assert "-d" in captured["cmd"]
    assert "--no-profile-validation" in captured["cmd"]


def test_convert_sandbox_violation_returns_error(tmp_path: Path) -> None:
    from suews_mcp.tools import convert_config

    (tmp_path / "case").mkdir()
    result = convert_config(
        "/etc/passwd",
        "out.yml",
        project_root=str(tmp_path / "case"),
    )
    assert result["status"] == "error"
