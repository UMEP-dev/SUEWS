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


def test_init_emits_recommendation_and_mcp_tool_call_next_steps(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    """The MCP wrapper replaces the CLI's shell-command ``next_steps``
    with imperative MCP-tool calls and adds a ``recommendation`` field
    so agents know which MCP tool to call next without scanning the
    array (gh#1408).

    In the EGU26 poster trace, after a successful ``init_case`` the
    agent fired 11 ``query_knowledge`` calls before timing out, never
    editing the scaffolded YAML. That trace is what motivates
    promoting ``next_steps`` to imperative MCP-tool form here.
    """
    from suews_mcp.tools import init_case

    target = tmp_path / "case01"

    def fake_run(cmd, **_kwargs):
        envelope = {
            "status": "success",
            "data": {
                "target_dir": str(target),
                "template": "simple-urban",
                "files_created": [
                    str(target / "sample_config.yml"),
                    str(target / "Kc_2012_data_60.txt"),
                ],
                "schema_version": "2026.5.dev9",
                "next_steps": [
                    f"Edit {target}/sample_config.yml",
                    f"suews validate {target}/sample_config.yml",
                    f"suews run {target}/sample_config.yml",
                ],
            },
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
    data = result["data"]

    # Recommendation is a single string the agent can act on directly.
    assert isinstance(data["recommendation"], str)
    assert "sample_config.yml" in data["recommendation"]

    # next_steps is the full imperative MCP-tool-call list, not the
    # CLI's shell-command form.
    steps_text = "\n".join(data["next_steps"])
    assert "mcp__suews__validate_config" in steps_text
    assert "mcp__suews__inspect_config" in steps_text
    assert "suews validate " not in steps_text, (
        "shell-form CLI commands must be replaced with MCP-tool-call form"
    )
    assert "suews run" not in steps_text

    # Original CLI-side metadata (target_dir / files_created / template /
    # schema_version) is preserved.
    assert data["target_dir"] == str(target)
    assert data["template"] == "simple-urban"
    assert data["schema_version"] == "2026.5.dev9"
    assert data["files_created"][0].endswith("sample_config.yml")
