"""Tests for the ``search_schema`` MCP tool.

The Phase-1 implementation originally did a single literal substring
match against the whole query string, so any natural-language query
("leaf area index", "green infrastructure vegetation") scored zero
matches even though the relevant fields exist. These tests pin the
contract that natural-language queries surface the schema fields the
canonical Sue-Grimmond questions (gh#1384) require.
"""

from __future__ import annotations

import shutil

import pytest

pytestmark = pytest.mark.api

pytestmark_skipif = pytest.mark.skipif(
    shutil.which("suews") is None,
    reason="`suews` CLI not on PATH; search_schema shells to `suews schema`. Run `make dev`.",
)


def _paths(envelope: dict) -> str:
    matches = (envelope.get("data") or {}).get("matches") or []
    return "\n".join(str(m.get("path", "")) for m in matches)


@pytestmark_skipif
@pytest.mark.parametrize(
    "query, expected_leaf",
    [
        # B5 / B2 — vegetation & green infrastructure must reach the
        # per-surface vegetation field families.
        ("leaf area index", "lai"),
        ("vegetation green infrastructure", "evetr"),
        ("irrigation water use for trees", "irrigation_fraction"),
        ("frontal area index for roughness", "frontal_area_index"),
        ("building morphology height", "building_frac"),
        ("surface albedo", "alb"),
    ],
)
def test_natural_language_query_surfaces_fields(query: str, expected_leaf: str) -> None:
    """A natural-language query returns at least one match whose path
    contains the expected schema field token."""
    from suews_mcp.tools import search_schema

    env = search_schema(query=query)
    assert env.get("status") == "success", env.get("errors")
    blob = _paths(env)
    assert expected_leaf in blob, (
        f"query {query!r} did not surface a field containing "
        f"{expected_leaf!r}. Returned paths:\n{blob[:1500]}"
    )


@pytestmark_skipif
def test_single_token_query_still_works() -> None:
    """A bare token query keeps working (regression guard for the
    literal-substring path that already worked)."""
    from suews_mcp.tools import search_schema

    env = search_schema(query="albedo")
    assert env.get("status") == "success"
    assert (env.get("data") or {}).get("n_matches", 0) > 0


@pytestmark_skipif
def test_empty_query_returns_full_schema_envelope() -> None:
    """An empty query returns the unfiltered schema envelope unchanged."""
    from suews_mcp.tools import search_schema

    env = search_schema(query="")
    assert env.get("status") == "success"
    # Full schema dump has the JSON-Schema keys, not the match wrapper.
    assert "matches" not in (env.get("data") or {})
