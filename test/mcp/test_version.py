"""Regression tests for MCP version alignment with SUEWS/SuPy."""

from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest
import suews_mcp

from supy import __version__ as supy_version

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]


def _load_version_script():
    spec = importlib.util.spec_from_file_location(
        "get_ver_git_for_test",
        REPO_ROOT / "get_ver_git.py",
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_mcp_runtime_version_matches_supy() -> None:
    assert suews_mcp.__version__ == supy_version


def test_mcp_pyproject_uses_generated_dynamic_version() -> None:
    pyproject = REPO_ROOT / "mcp" / "pyproject.toml"
    text = pyproject.read_text(encoding="utf-8")

    assert 'dynamic = ["version"]' in text
    assert 'version = "0.1.0"' not in text
    # Route via the package __init__, which has a tracked fallback chain
    # (_version_scm -> supy.__version__ -> "0+unknown"). Pointing
    # setuptools directly at the gitignored _version_scm broke fresh
    # `pip install -e mcp/` (gh#1384).
    assert 'version = { attr = "suews_mcp.__version__" }' in text


def test_version_script_writes_supy_and_mcp_version_files(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    version_script = _load_version_script()

    (tmp_path / "src" / "supy").mkdir(parents=True)
    (tmp_path / "mcp" / "src" / "suews_mcp").mkdir(parents=True)
    monkeypatch.chdir(tmp_path)

    version_script.write_version_file("2026.5.1.dev2")

    for rel_path in (
        Path("src/supy/_version_scm.py"),
        Path("mcp/src/suews_mcp/_version_scm.py"),
    ):
        generated = tmp_path / rel_path
        text = generated.read_text(encoding="utf-8")
        assert "__version__ = version = '2026.5.1.dev2'" in text
        assert "__version_tuple__ = version_tuple = (2026, 5, 1, 'dev2')" in text
        # gh#1401: the build-time commit hash is baked in so the envelope
        # ``meta.git_commit`` field still carries provenance after a wheel
        # install (no .git directory at runtime).
        assert "__commit_hash__ = commit_hash = '" in text


def test_version_script_describes_explicit_source_ref(monkeypatch: pytest.MonkeyPatch) -> None:
    """Docs builds on the rtd branch must describe the source commit, not rtd HEAD."""
    version_script = _load_version_script()

    def fake_check_output(command, stderr=None):
        assert command == [
            "git",
            "describe",
            "--tags",
            "--long",
            "--match=[0-9]*",
            "origin/master",
        ]
        return b"2026.6.2.dev-1-g50db70b49c\n"

    monkeypatch.setattr(version_script.subprocess, "check_output", fake_check_output)

    assert version_script.get_version_from_git("origin/master") == "2026.6.2.dev1"


def test_version_script_peels_annotated_tag_for_commit_info(monkeypatch: pytest.MonkeyPatch) -> None:
    version_script = _load_version_script()
    calls = []

    def fake_check_output(command, stderr=None):
        calls.append(command)
        if command == ["git", "rev-parse", "--short=7", "2026.6.2.dev^{}"]:
            return b"9438baa\n"
        if command == ["git", "rev-parse", "2026.6.2.dev^{}"]:
            return b"9438baa1043f5c40fa2e016e4f4e33a98b545657\n"
        raise AssertionError(f"unexpected command: {command!r}")

    monkeypatch.setattr(version_script.subprocess, "check_output", fake_check_output)

    assert version_script.get_commit_info("2026.6.2.dev") == (
        "9438baa",
        "9438baa1043f5c40fa2e016e4f4e33a98b545657",
    )
    assert calls == [
        ["git", "rev-parse", "--short=7", "2026.6.2.dev^{}"],
        ["git", "rev-parse", "2026.6.2.dev^{}"],
    ]


def test_mcp_server_advertises_package_version() -> None:
    """The MCP ``initialize`` handshake's ``serverInfo.version`` must
    match ``suews_mcp.__version__`` (gh#1401).

    Without this override, FastMCP advertises its own SDK version
    (e.g. "1.27.0") in the handshake, weakening the provenance story
    for clients that log ``serverInfo.version``.
    """
    pytest.importorskip(
        "mcp.server.fastmcp",
        reason=(
            "The optional 'mcp' SDK FastMCP server is not installed in the "
            "standard supy wheel matrix."
        ),
    )
    from suews_mcp.server import _build_server

    server = _build_server()
    assert server._mcp_server.version == suews_mcp.__version__
