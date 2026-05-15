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
    text = pyproject.read_text()

    assert 'dynamic = ["version"]' in text
    assert 'version = "0.1.0"' not in text
    assert 'version = { attr = "suews_mcp._version_scm.__version__" }' in text


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
        text = generated.read_text()
        assert "__version__ = version = '2026.5.1.dev2'" in text
        assert "__version_tuple__ = version_tuple = (2026, 5, 1, 'dev2')" in text
