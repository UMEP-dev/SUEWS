"""Tests for the SUEWS source-evidence knowledge pack."""

from __future__ import annotations

import gzip
import json
from pathlib import Path

import pytest

from supy.knowledge import build_pack, load_manifest, query_pack

pytestmark = pytest.mark.api


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def _make_repo(path_repo: Path) -> None:
    _write(
        path_repo / "src/suews/src/suews_phys_evap.f95",
        "\n".join(
            [
                "MODULE suews_phys_evap",
                "! Evaporation source evidence should be preserved.",
                "CONTAINS",
                "SUBROUTINE evap_calculate(storage, runoff)",
                "  REAL :: storage",
                "  REAL :: runoff",
                "END SUBROUTINE evap_calculate",
                "END MODULE suews_phys_evap",
            ]
        ),
    )
    _write(
        path_repo / "src/suews_bridge/src/sim.rs",
        "\n".join(
            [
                "pub struct BridgeRun {}",
                "pub fn run_bridge() {",
                "    // Rust bridge evidence should be preserved.",
                "}",
            ]
        ),
    )
    _write(path_repo / "src/suews_bridge/Cargo.toml", "[package]\nname = \"suews-engine\"\n")
    _write(
        path_repo / "src/suews_bridge/bridge-manifest.json",
        "{\"manifest_version\": 1, \"types\": []}\n",
    )
    _write(
        path_repo / "src/supy/cmd/example.py",
        "def validate_config():\n    return 'schema evidence'\n",
    )
    _write(
        path_repo / "src/supy/data_model/schema/version.py",
        "CURRENT_SCHEMA_VERSION = \"2026.5.test\"\n",
    )
    _write(
        path_repo / "src/supy/_version_scm.py",
        "__version__ = version = '2026.5.1.test0'\n",
    )
    _write(path_repo / "docs/source/index.rst", "This online documentation is not bundled.\n")


def _read_chunks(path_pack: Path) -> list[dict]:
    with gzip.open(path_pack / "chunks.jsonl.gz", "rt", encoding="utf-8") as stream:
        return [json.loads(line) for line in stream if line.strip()]


def test_build_pack_records_git_bound_manifest(tmp_path: Path) -> None:
    path_repo = tmp_path / "repo"
    _make_repo(path_repo)
    path_pack = tmp_path / "pack"

    manifest = build_pack(
        path_repo,
        path_pack,
        git_sha="abc123",
        generated_at="2026-05-01T00:00:00Z",
    )

    loaded = load_manifest(path_pack)
    assert loaded == manifest
    assert manifest["git_sha"] == "abc123"
    assert manifest["suews_version"] == "2026.5.1.test0"
    assert manifest["schema_version"] == "2026.5.test"
    assert manifest["official_docs"]["stable"] == "https://docs.suews.io/stable/"
    assert manifest["chunk_count"] > 0


def test_pack_preserves_selected_source_and_excludes_docs(tmp_path: Path) -> None:
    path_repo = tmp_path / "repo"
    _make_repo(path_repo)
    path_pack = tmp_path / "pack"

    build_pack(path_repo, path_pack, git_sha="abc123")
    chunks = _read_chunks(path_pack)

    paths = {chunk["repo_path"] for chunk in chunks}
    assert "src/suews/src/suews_phys_evap.f95" in paths
    assert "src/suews_bridge/src/sim.rs" in paths
    assert "src/supy/cmd/example.py" in paths
    assert "docs/source/index.rst" not in paths

    for chunk in chunks:
        assert chunk["git_sha"] == "abc123"
        assert "#L" in chunk["github_url"]
        assert chunk["line_start"] <= chunk["line_end"]
        assert chunk["text"]


def test_query_pack_returns_cited_evidence(tmp_path: Path) -> None:
    path_repo = tmp_path / "repo"
    _make_repo(path_repo)
    path_pack = tmp_path / "pack"
    build_pack(path_repo, path_pack, git_sha="abc123")

    result = query_pack("How does evaporation storage runoff work?", path_pack, limit=3)

    assert result["manifest"]["git_sha"] == "abc123"
    assert result["matches"]
    first = result["matches"][0]
    assert first["repo_path"] == "src/suews/src/suews_phys_evap.f95"
    assert "Evaporation source evidence should be preserved." in first["text"]


def test_query_pack_can_scope_to_rust(tmp_path: Path) -> None:
    path_repo = tmp_path / "repo"
    _make_repo(path_repo)
    path_pack = tmp_path / "pack"
    build_pack(path_repo, path_pack, git_sha="abc123")

    result = query_pack("bridge run evidence", path_pack, limit=5, scope="rust")

    assert result["matches"]
    assert {match["content_type"] for match in result["matches"]} == {"rust"}


def test_python_chunks_prefer_top_level_definitions_without_dropping_preamble(tmp_path: Path) -> None:
    path_repo = tmp_path / "repo"
    _make_repo(path_repo)
    _write(
        path_repo / "src/supy/tools.py",
        "\n".join(
            [
                "import math",
                "VALUE = 1",
                "def alpha():",
                "    return math.sqrt(VALUE)",
                "class Beta:",
                "    def gamma(self):",
                "        return VALUE",
            ]
        ),
    )
    path_pack = tmp_path / "pack"

    build_pack(path_repo, path_pack, git_sha="abc123")
    chunks = [
        chunk
        for chunk in _read_chunks(path_pack)
        if chunk["repo_path"] == "src/supy/tools.py"
    ]

    assert [(chunk["line_start"], chunk["line_end"]) for chunk in chunks] == [(1, 4), (5, 7)]
    assert chunks[0]["symbol"] == "def alpha"
    assert chunks[1]["symbol"] == "class Beta"
    assert "import math" in chunks[0]["text"]
