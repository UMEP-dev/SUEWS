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
    cli_mod.run_suews_cli("validate", ["config.yml"], suews_executable="suews")
    assert captured["cmd"] == ["suews", "validate", "config.yml", "--format", "json"]


def test_passes_through_when_format_already_set(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.backend import cli as cli_mod

    captured: dict[str, list[str]] = {}

    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        out = json.dumps({"status": "success", "data": {}, "errors": [], "warnings": [], "meta": {}})
        return subprocess.CompletedProcess(args=cmd, returncode=0, stdout=out, stderr="")

    monkeypatch.setattr(subprocess, "run", fake_run)
    cli_mod.run_suews_cli(
        "validate", ["config.yml", "--format", "table"], suews_executable="suews"
    )
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
    with pytest.raises(SUEWSMCPError, match="not found via"):
        cli_mod.run_suews_cli("validate", ["config.yml"])


# ---------------------------------------------------------------------------
# Sibling-anchored executable resolution — gh#1400
# ---------------------------------------------------------------------------


def test_resolve_suews_executable_prefers_sibling_of_sys_executable(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """When MCP plugin hosts launch ``suews-mcp`` without sourcing the
    venv, PATH does not include the venv's bin/. The resolver must
    still find the sibling ``suews`` console script next to
    ``sys.executable`` (gh#1400).
    """
    import sys

    from suews_mcp.backend import cli as cli_mod

    # Stage a fake interpreter and sibling script under tmp_path/bin/.
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    fake_python = bin_dir / "python3"
    fake_suews = bin_dir / "suews"
    fake_python.write_text("#!/bin/sh\nexit 0\n")
    fake_suews.write_text("#!/bin/sh\nexit 0\n")
    fake_python.chmod(0o755)
    fake_suews.chmod(0o755)

    monkeypatch.setattr(sys, "executable", str(fake_python))
    # PATH only sees /usr/bin so a successful resolution proves the
    # sibling lookup ran rather than falling back to PATH.
    monkeypatch.setenv("PATH", "/usr/bin")

    resolved = cli_mod._resolve_suews_executable()
    assert resolved == str(fake_suews)


def test_resolve_suews_executable_falls_back_to_path(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """When no sibling exists (e.g. system-wide pipx install), the
    resolver must fall back to the user's PATH.
    """
    import sys

    from suews_mcp.backend import cli as cli_mod

    # Sibling directory contains only python, not suews.
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    fake_python = bin_dir / "python3"
    fake_python.write_text("#!/bin/sh\nexit 0\n")
    fake_python.chmod(0o755)

    # PATH-only suews lives elsewhere.
    path_dir = tmp_path / "system" / "bin"
    path_dir.mkdir(parents=True)
    path_suews = path_dir / "suews"
    path_suews.write_text("#!/bin/sh\nexit 0\n")
    path_suews.chmod(0o755)

    monkeypatch.setattr(sys, "executable", str(fake_python))
    monkeypatch.setenv("PATH", str(path_dir))

    resolved = cli_mod._resolve_suews_executable()
    assert resolved == str(path_suews)


def test_resolve_suews_executable_returns_name_when_unreachable(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """When neither the sibling nor PATH yields a hit, the resolver
    returns the raw name so ``subprocess.run`` raises a meaningful
    ``FileNotFoundError`` rather than silently swallowing the lookup.
    """
    import sys

    from suews_mcp.backend import cli as cli_mod

    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    fake_python = bin_dir / "python3"
    fake_python.write_text("")
    fake_python.chmod(0o755)

    monkeypatch.setattr(sys, "executable", str(fake_python))
    monkeypatch.setenv("PATH", str(tmp_path / "nonexistent"))

    resolved = cli_mod._resolve_suews_executable()
    assert resolved == "suews"


def test_run_suews_cli_uses_resolver_by_default(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """``run_suews_cli`` must call the resolver when ``suews_executable``
    is left at the default ``None`` so plugin-host launches work without
    a manual PATH override (gh#1400).
    """
    import sys

    from suews_mcp.backend import cli as cli_mod

    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    fake_python = bin_dir / "python3"
    fake_suews = bin_dir / "suews"
    fake_python.write_text("")
    fake_suews.write_text("")
    fake_python.chmod(0o755)
    fake_suews.chmod(0o755)

    monkeypatch.setattr(sys, "executable", str(fake_python))
    monkeypatch.setenv("PATH", "/usr/bin")

    captured: dict[str, list[str]] = {}

    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        out = json.dumps({"status": "success", "data": {}, "errors": [], "warnings": [], "meta": {}})
        return subprocess.CompletedProcess(args=cmd, returncode=0, stdout=out, stderr="")

    monkeypatch.setattr(subprocess, "run", fake_run)
    cli_mod.run_suews_cli("validate", ["config.yml"])

    assert captured["cmd"][0] == str(fake_suews), (
        f"expected resolver-anchored path, got {captured['cmd'][0]!r}"
    )
