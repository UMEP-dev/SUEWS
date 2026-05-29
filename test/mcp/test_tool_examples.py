"""Unit tests for ``list_examples`` and ``read_example``."""

from __future__ import annotations

import pytest

pytestmark = pytest.mark.api

# These tools rely on the bundled supy sample_data; skip cleanly if supy is
# not available in this environment.
supy = pytest.importorskip("supy", reason="supy must be installed to test example tools")


def test_list_examples_returns_simple_urban() -> None:
    from suews_mcp.tools import list_examples

    result = list_examples()
    assert result["status"] == "success"
    names = {e["name"] for e in result["data"]["examples"]}
    assert "simple-urban" in names


def test_read_example_default_mode_is_summary() -> None:
    """Default ``mode`` returns a bounded summary, not the full content
    (gh#1403). Previously the unbounded payload exceeded every host's
    per-tool token cap and the agent saw only a rejection message.
    """
    from suews_mcp.tools import read_example

    result = read_example("simple-urban")
    assert result["status"] == "success"
    assert result["data"]["mode"] == "summary"
    files = result["data"]["files"]
    roles = {f["role"] for f in files}
    assert {"config", "forcing"}.issubset(roles)

    config_entry = next(f for f in files if f["role"] == "config")
    # Summary mode emits a preview, not full content.
    assert "preview" in config_entry, "summary mode must emit a preview field"
    assert "size_bytes" in config_entry
    assert config_entry["preview"] is not None
    # Whole-payload size is bounded.
    payload_bytes = sum(
        len((f.get("preview") or "").encode("utf-8")) for f in files
    )
    assert payload_bytes < 16_000, (
        f"summary payload exceeded budget: {payload_bytes} bytes"
    )


def test_read_example_manifest_mode_omits_content() -> None:
    """``mode="manifest"`` returns metadata only — no preview, no
    content — for the cheapest "what's in this example" lookup
    (gh#1403).
    """
    from suews_mcp.tools import read_example

    result = read_example("simple-urban", mode="manifest")
    assert result["status"] == "success"
    assert result["data"]["mode"] == "manifest"
    for entry in result["data"]["files"]:
        assert "preview" not in entry, "manifest mode must not emit preview"
        assert "content" not in entry, "manifest mode must not emit content"
        assert "size_bytes" in entry


def test_read_example_file_mode_returns_one_file(tmp_path) -> None:
    """``mode="file"`` returns the full content of a single named file,
    capped at ``_FILE_MODE_BYTES_CAP`` (gh#1403).
    """
    from suews_mcp.tools import read_example
    from suews_mcp.tools.examples import _FILE_MODE_BYTES_CAP

    summary = read_example("simple-urban")
    config_path = next(
        f["path"] for f in summary["data"]["files"] if f["role"] == "config"
    )

    result = read_example("simple-urban", mode="file", path=config_path)
    assert result["status"] == "success"
    assert result["data"]["mode"] == "file"
    files = result["data"]["files"]
    assert len(files) == 1
    entry = files[0]
    assert entry["role"] == "config"
    assert isinstance(entry["content"], str)
    assert len(entry["content"].encode("utf-8")) <= _FILE_MODE_BYTES_CAP


def test_read_example_file_mode_requires_path() -> None:
    """``mode="file"`` without ``path`` is a user error, not a silent
    fall-through to full bundle (gh#1403).
    """
    from suews_mcp.tools import read_example

    result = read_example("simple-urban", mode="file")
    assert result["status"] == "error"
    assert any(
        "requires the `path` argument" in str(e) for e in result["errors"]
    )


def test_read_example_unknown_mode_returns_error() -> None:
    from suews_mcp.tools import read_example

    result = read_example("simple-urban", mode="not-a-mode")
    assert result["status"] == "error"
    assert any("Unknown mode" in str(e) for e in result["errors"])


def test_read_example_unknown_returns_error() -> None:
    from suews_mcp.tools import read_example

    result = read_example("does-not-exist")
    assert result["status"] == "error"
    assert any("not in the built-in catalogue" in str(e) for e in result["errors"])
