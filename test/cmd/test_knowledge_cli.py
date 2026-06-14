"""Tests for the ``suews knowledge`` command group."""

from __future__ import annotations

import json
from pathlib import Path

from click.testing import CliRunner
import pytest

from supy.cmd.suews_cli import cli
from supy.knowledge import build_pack

pytestmark = pytest.mark.api


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def _make_pack(tmp_path: Path) -> Path:
    path_repo = tmp_path / "repo"
    _write(
        path_repo / "src/suews/src/suews_phys_waterdist.f95",
        "MODULE waterdist\nCONTAINS\nSUBROUTINE runoff_route()\nEND SUBROUTINE\nEND MODULE\n",
    )
    _write(path_repo / "src/suews_bridge/src/main.rs", "fn main() {}\n")
    _write(path_repo / "src/supy/_version_scm.py", "__version__ = version = '2026.5.1.test0'\n")
    _write(path_repo / "src/supy/data_model/schema/version.py", "CURRENT_SCHEMA_VERSION = \"2026.5.test\"\n")
    path_pack = tmp_path / "pack"
    build_pack(path_repo, path_pack, git_sha="def456")
    return path_pack


def test_knowledge_manifest_json(tmp_path: Path) -> None:
    path_pack = _make_pack(tmp_path)
    result = CliRunner().invoke(cli, ["knowledge", "manifest", "--pack", str(path_pack), "--format", "json"])

    assert result.exit_code == 0, result.output
    payload = json.loads(result.output)
    assert payload["status"] == "success"
    assert payload["data"]["manifest"]["git_sha"] == "def456"


def test_knowledge_query_json(tmp_path: Path) -> None:
    path_pack = _make_pack(tmp_path)
    result = CliRunner().invoke(
        cli,
        [
            "knowledge",
            "query",
            "runoff route",
            "--pack",
            str(path_pack),
            "--format",
            "json",
        ],
    )

    assert result.exit_code == 0, result.output
    payload = json.loads(result.output)
    assert payload["status"] == "success"
    assert payload["data"]["matches"][0]["repo_path"] == "src/suews/src/suews_phys_waterdist.f95"


def test_knowledge_build_json(tmp_path: Path) -> None:
    path_repo = tmp_path / "repo"
    _write(path_repo / "src/suews/src/suews_phys_lumps.f95", "MODULE lumps\nEND MODULE\n")
    _write(path_repo / "src/suews_bridge/src/main.rs", "fn main() {}\n")
    _write(path_repo / "src/supy/_version_scm.py", "__version__ = version = '2026.5.1.test0'\n")
    _write(path_repo / "src/supy/data_model/schema/version.py", "CURRENT_SCHEMA_VERSION = \"2026.5.test\"\n")
    path_pack = tmp_path / "pack"

    result = CliRunner().invoke(
        cli,
        [
            "knowledge",
            "build",
            "--repo-root",
            str(path_repo),
            "--output",
            str(path_pack),
            "--git-sha",
            "feed123",
            "--format",
            "json",
        ],
    )

    assert result.exit_code == 0, result.output
    payload = json.loads(result.output)
    assert payload["status"] == "success"
    assert payload["data"]["manifest"]["git_sha"] == "feed123"
    assert (path_pack / "manifest.json").exists()
