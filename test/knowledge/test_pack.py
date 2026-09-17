"""Tests for the SUEWS source-evidence knowledge pack."""

from __future__ import annotations

import gzip
import json
from pathlib import Path

import pytest

from supy.knowledge import build_pack, default_pack_dir, load_manifest, query_pack
from supy.knowledge.pack import EXCLUDED_GENERATED_ROOTS, MAX_CHUNK_BYTES

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
        path_repo / "src/supy/data_model/configuration/version.py",
        "CURRENT_SCHEMA_VERSION = \"2026.5.test\"\n",
    )
    _write(
        path_repo / "src/supy/data_model/schema/version.py",
        "from ..configuration.version import CURRENT_SCHEMA_VERSION\n",
    )
    _write(
        path_repo / "src/supy/_version_scm.py",
        "__version__ = version = '2026.5.1.test0'\n",
    )
    _write(path_repo / "docs/source/index.rst", "This online documentation is not bundled.\n")


def _read_chunks(path_pack: Path) -> list[dict]:
    with gzip.open(path_pack / "chunks.jsonl.gz", "rt", encoding="utf-8") as stream:
        return [json.loads(line) for line in stream if line.strip()]


def _read_installed_chunks() -> list[dict]:
    """Read the chunks of the pack shipped with the installed package."""
    resource = default_pack_dir().joinpath("chunks.jsonl.gz")
    try:
        present = resource.is_file()
    except OSError:
        present = False
    if not present:
        pytest.skip(
            "No built knowledge pack is installed; it is produced by the meson "
            "build. Run `make dev` first."
        )
    with resource.open("rb") as raw, gzip.open(raw, "rt", encoding="utf-8") as stream:
        return [json.loads(line) for line in stream if line.strip()]


def _chunk_size(chunk: dict) -> int:
    return len(chunk["text"].encode("utf-8"))


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
    assert "src/supy/data_model/configuration/version.py" in paths
    assert "src/supy/data_model/schema/version.py" in paths
    assert "docs/source/index.rst" not in paths

    content_types = {
        chunk["repo_path"]: chunk["content_type"] for chunk in chunks
    }
    assert content_types["src/supy/data_model/configuration/version.py"] == "schema"
    assert content_types["src/supy/data_model/schema/version.py"] == "schema"

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


# -----------------------------------------------------------------------
# Chunk size bound and generated-artefact exclusion (gh#1815)
# -----------------------------------------------------------------------


def test_generated_contract_artefacts_are_excluded_and_declared(tmp_path: Path) -> None:
    """Published contract projections stay out of the pack, visibly so."""
    path_repo = tmp_path / "repo"
    _make_repo(path_repo)
    _write(
        path_repo / "src/supy/data_model/output/artefacts/1.0.0/catalogue.json",
        '{"groups":[{"group":"SUEWS","scope":"stable"}]}\n',
    )
    _write(
        path_repo / "src/supy/data_model/forcing/artefacts/1.0.0.json",
        '{"missing_value":-999.0}\n',
    )
    path_pack = tmp_path / "pack"

    manifest = build_pack(path_repo, path_pack, git_sha="abc123")
    paths = {chunk["repo_path"] for chunk in _read_chunks(path_pack)}

    assert "src/supy/data_model/output/artefacts/1.0.0/catalogue.json" not in paths
    assert "src/supy/data_model/forcing/artefacts/1.0.0.json" not in paths
    for root in EXCLUDED_GENERATED_ROOTS:
        assert root in manifest["excluded_roots"]
    assert manifest["max_chunk_bytes"] == MAX_CHUNK_BYTES


def test_single_line_file_over_the_bound_splits_into_byte_windows(tmp_path: Path) -> None:
    """A one-line file larger than the bound becomes several bounded chunks.

    The fixture is deliberately non-ASCII: the payload is a run of 3-byte
    codepoints behind a 9-byte prefix, so the bound falls inside a codepoint
    and a naive byte slice would emit undecodable text.
    """
    path_repo = tmp_path / "repo"
    _make_repo(path_repo)
    line = '{"note":"' + "€" * 20000 + '"}'
    assert len(line.encode("utf-8")) > MAX_CHUNK_BYTES
    _write(path_repo / "src/supy/data_model/big_registry.json", line + "\n")
    path_pack = tmp_path / "pack"

    build_pack(path_repo, path_pack, git_sha="abc123")
    chunks = [
        chunk
        for chunk in _read_chunks(path_pack)
        if chunk["repo_path"] == "src/supy/data_model/big_registry.json"
    ]

    assert len(chunks) > 1
    assert all(_chunk_size(chunk) <= MAX_CHUNK_BYTES for chunk in chunks)
    # Every piece cites the single line it came from, so the citation resolves.
    assert {(chunk["line_start"], chunk["line_end"]) for chunk in chunks} == {(1, 1)}
    assert all(chunk["github_url"].endswith("#L1-L1") for chunk in chunks)
    # Nothing is lost or duplicated, and no codepoint was cut in half.
    assert "".join(chunk["text"] for chunk in chunks) == line
    assert len({chunk["id"] for chunk in chunks}) == len(chunks)


def test_oversized_multi_line_window_splits_on_line_boundaries(tmp_path: Path) -> None:
    """An over-long multi-line window is re-cut on lines, keeping spans exact."""
    path_repo = tmp_path / "repo"
    _make_repo(path_repo)
    lines = [f"! {index:04d} " + "x" * 400 for index in range(200)]
    _write(path_repo / "src/suews/src/suews_phys_wide.f95", "\n".join(lines) + "\n")
    path_pack = tmp_path / "pack"

    build_pack(path_repo, path_pack, git_sha="abc123")
    chunks = [
        chunk
        for chunk in _read_chunks(path_pack)
        if chunk["repo_path"] == "src/suews/src/suews_phys_wide.f95"
    ]

    assert len(chunks) > 1
    for chunk in chunks:
        assert _chunk_size(chunk) <= MAX_CHUNK_BYTES
        start, end = chunk["line_start"], chunk["line_end"]
        assert chunk["text"] == "\n".join(lines[start - 1 : end])
    # The first line window (1-160) is covered contiguously by its pieces.
    first_window = [chunk for chunk in chunks if chunk["line_start"] <= 160][:2]
    assert first_window[0]["line_start"] == 1
    assert first_window[1]["line_start"] == first_window[0]["line_end"] + 1


def test_installed_pack_respects_the_byte_bound() -> None:
    """The pack shipped with the package carries no unbounded chunk."""
    chunks = _read_installed_chunks()

    oversized = sorted(
        (
            (_chunk_size(chunk), chunk["repo_path"], chunk["line_start"], chunk["line_end"])
            for chunk in chunks
            if _chunk_size(chunk) > MAX_CHUNK_BYTES
        ),
        reverse=True,
    )
    assert not oversized, (
        f"Chunks exceed the {MAX_CHUNK_BYTES} byte bound: {oversized[:5]}"
    )

    packed_exclusions = sorted({
        chunk["repo_path"]
        for chunk in chunks
        if chunk["repo_path"].startswith(EXCLUDED_GENERATED_ROOTS)
    })
    assert not packed_exclusions, (
        f"Generated contract artefacts reached the pack: {packed_exclusions}"
    )

    manifest = load_manifest()
    assert manifest.get("max_chunk_bytes") == MAX_CHUNK_BYTES, (
        "The installed pack predates the chunk-size bound; rebuild with `make dev`."
    )
