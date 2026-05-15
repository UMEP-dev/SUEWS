"""Unit tests for ``suews_mcp.backend.cli.run_suews_cli``."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path
from unittest.mock import MagicMock

import pytest

pytestmark = pytest.mark.api


def test_rejects_subcommand_outside_allow_list() -> None:
    from suews_mcp.backend.cli import SUEWSMCPError, run_suews_cli

    with pytest.raises(SUEWSMCPError, match="not in the MCP allow-list"):
        run_suews_cli("rm", ["-rf", "/"])


def test_appends_format_json_when_missing(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.backend import cli as cli_mod

    captured: dict[str, list[str]] = {}

    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        out = json.dumps({"status": "success", "data": {}, "errors": [], "warnings": [], "meta": {}})
        return subprocess.CompletedProcess(args=cmd, returncode=0, stdout=out, stderr="")

    monkeypatch.setattr(subprocess, "run", fake_run)
    cli_mod.run_suews_cli("validate", ["config.yml"])
    assert captured["cmd"] == ["suews", "validate", "config.yml", "--format", "json"]


def test_passes_through_when_format_already_set(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.backend import cli as cli_mod

    captured: dict[str, list[str]] = {}

    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        out = json.dumps({"status": "success", "data": {}, "errors": [], "warnings": [], "meta": {}})
        return subprocess.CompletedProcess(args=cmd, returncode=0, stdout=out, stderr="")

    monkeypatch.setattr(subprocess, "run", fake_run)
    cli_mod.run_suews_cli("validate", ["config.yml", "--format", "table"])
    assert "--format" in captured["cmd"]
    assert captured["cmd"].count("--format") == 1


def test_returns_parsed_envelope(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.backend import cli as cli_mod

    envelope = {
        "status": "success",
        "data": {"x": 1},
        "errors": [],
        "warnings": [],
        "meta": {"command": "suews validate"},
    }

    def fake_run(cmd, **_kwargs):
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    monkeypatch.setattr(subprocess, "run", fake_run)
    result = cli_mod.run_suews_cli("validate", ["config.yml"])
    assert result == envelope


def test_returns_error_envelope_on_nonzero_exit(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.backend import cli as cli_mod

    envelope = {
        "status": "error",
        "data": {},
        "errors": [{"message": "validation failed"}],
        "warnings": [],
        "meta": {"command": "suews validate"},
    }

    def fake_run(cmd, **_kwargs):
        return subprocess.CompletedProcess(
            args=cmd, returncode=1, stdout=json.dumps(envelope), stderr=""
        )

    monkeypatch.setattr(subprocess, "run", fake_run)
    result = cli_mod.run_suews_cli("validate", ["config.yml"])
    assert result["status"] == "error"


def test_raises_on_nonjson_stdout(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.backend import cli as cli_mod
    from suews_mcp.backend.cli import SUEWSMCPError

    def fake_run(cmd, **_kwargs):
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout="not json", stderr=""
        )

    monkeypatch.setattr(subprocess, "run", fake_run)
    with pytest.raises(SUEWSMCPError, match="not valid JSON"):
        cli_mod.run_suews_cli("validate", ["config.yml"])


def test_raises_on_timeout(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.backend import cli as cli_mod
    from suews_mcp.backend.cli import SUEWSMCPError

    def fake_run(cmd, **_kwargs):
        raise subprocess.TimeoutExpired(cmd=cmd, timeout=1)

    monkeypatch.setattr(subprocess, "run", fake_run)
    with pytest.raises(SUEWSMCPError, match="exceeded"):
        cli_mod.run_suews_cli("validate", ["config.yml"], timeout=1)


def test_raises_when_executable_missing(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.backend import cli as cli_mod
    from suews_mcp.backend.cli import SUEWSMCPError

    def fake_run(cmd, **_kwargs):
        raise FileNotFoundError(cmd[0])

    monkeypatch.setattr(subprocess, "run", fake_run)
    with pytest.raises(SUEWSMCPError, match="not found on PATH"):
        cli_mod.run_suews_cli("validate", ["config.yml"])
