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


def test_read_example_returns_config_and_forcing() -> None:
    from suews_mcp.tools import read_example

    result = read_example("simple-urban")
    assert result["status"] == "success"
    files = result["data"]["files"]
    roles = {f["role"] for f in files}
    assert {"config", "forcing"}.issubset(roles)
    config_entry = next(f for f in files if f["role"] == "config")
    assert config_entry["content"] is not None
    assert "schema_version" in config_entry["content"] or "site" in config_entry["content"].lower()


def test_read_example_unknown_returns_error() -> None:
    from suews_mcp.tools import read_example

    result = read_example("does-not-exist")
    assert result["status"] == "error"
    assert any("not in the built-in catalogue" in str(e) for e in result["errors"])
