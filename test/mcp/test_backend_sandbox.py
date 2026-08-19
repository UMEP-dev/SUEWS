"""Unit tests for ``suews_mcp.backend.sandbox.ProjectRoot``."""

from __future__ import annotations

from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def test_resolves_relative_path_under_root(tmp_path: Path) -> None:
    from suews_mcp.backend.sandbox import ProjectRoot

    (tmp_path / "case01").mkdir()
    (tmp_path / "case01" / "config.yml").write_text("x: 1", encoding="utf-8")

    root = ProjectRoot(tmp_path)
    resolved = root.resolve("case01/config.yml")
    assert resolved == (tmp_path / "case01" / "config.yml").resolve()


def test_resolves_absolute_path_under_root(tmp_path: Path) -> None:
    from suews_mcp.backend.sandbox import ProjectRoot

    config = tmp_path / "case01" / "config.yml"
    config.parent.mkdir()
    config.write_text("x: 1", encoding="utf-8")

    root = ProjectRoot(tmp_path)
    assert root.resolve(str(config)) == config.resolve()


def test_rejects_path_above_root(tmp_path: Path) -> None:
    from suews_mcp.backend.sandbox import ProjectRoot, SUEWSMCPSandboxError

    (tmp_path / "case01").mkdir()
    root = ProjectRoot(tmp_path / "case01")

    with pytest.raises(SUEWSMCPSandboxError, match="outside the project root"):
        root.resolve("../../etc/passwd")


def test_rejects_unrelated_absolute_path(tmp_path: Path) -> None:
    from suews_mcp.backend.sandbox import ProjectRoot, SUEWSMCPSandboxError

    (tmp_path / "case01").mkdir()
    root = ProjectRoot(tmp_path / "case01")

    with pytest.raises(SUEWSMCPSandboxError):
        root.resolve("/etc/passwd")


def test_root_property_is_absolute(tmp_path: Path) -> None:
    from suews_mcp.backend.sandbox import ProjectRoot

    root = ProjectRoot(tmp_path)
    assert root.root.is_absolute()


def test_default_root_from_env(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    from suews_mcp.backend.sandbox import ProjectRoot

    monkeypatch.setenv("SUEWS_MCP_PROJECT_ROOT", str(tmp_path))
    root = ProjectRoot()
    assert root.root == tmp_path.resolve()


def test_rejection_message_names_root_and_flag(tmp_path: Path) -> None:
    """Sandbox rejection must surface both the configured root and the
    flag/env-var that controls it (gh#1405). Without this, users
    debugging a "wrong root" launch (server inheriting a Conductor temp
    cwd because no ``--root`` was passed) cannot see why a
    workspace-absolute path is being rejected.
    """
    from suews_mcp.backend.sandbox import ProjectRoot, SUEWSMCPSandboxError

    (tmp_path / "case01").mkdir()
    root = ProjectRoot(tmp_path / "case01")

    with pytest.raises(SUEWSMCPSandboxError) as excinfo:
        root.resolve("/etc/passwd")

    msg = str(excinfo.value)
    assert "outside the project root" in msg
    assert "--root" in msg, "error must name the --root flag for self-correction"
    assert "SUEWS_MCP_PROJECT_ROOT" in msg, "error must name the env var fallback"


def test_requested_root_cannot_escape_session_root(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.backend.sandbox import ProjectRoot

    session_root = tmp_path / "session"
    session_root.mkdir()
    monkeypatch.setenv("SUEWS_MCP_PROJECT_ROOT", str(session_root))

    root = ProjectRoot("/")
    assert root.root == session_root.resolve()
