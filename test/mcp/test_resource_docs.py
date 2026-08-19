"""Tests for the ``suews://docs`` resource and the ``list_docs`` tool.

Pins the gh#1384 contract that the curated doc catalogue (a) advertises
slash-free, URI-safe slugs, (b) actually resolves to real content for
the docs Sue's questions need (forcing, parameterisations, the
setup-own-site tutorial, the output-variable dictionary), and (c)
returns a helpful error for an unknown slug.
"""

from __future__ import annotations

from pathlib import Path

import pytest

pytestmark = pytest.mark.api

from suews_mcp.resources.docs import _DOCS, _repo_root, list_docs, read_doc


def test_all_slugs_are_uri_safe() -> None:
    """Slugs must be slash-free so the ``suews://docs/{slug}`` template routes."""
    for slug in _DOCS:
        assert "/" not in slug, f"slug {slug!r} contains '/', breaks URI routing"
        assert " " not in slug, f"slug {slug!r} contains a space"


def test_list_docs_advertises_key_slugs() -> None:
    """The catalogue covers the docs the canonical questions require."""
    env = list_docs()
    assert env["status"] == "success"
    slugs = {d["slug"] for d in env["data"]["docs"]}
    for required in {
        "forcing-data",
        "parameterisations",
        "output-variables",
        "tutorial-setup-own-site",
        "tutorial-results-analysis",
    }:
        assert required in slugs, f"catalogue missing {required!r}"
    assert all(d["summary"] for d in env["data"]["docs"]), "every doc needs a summary"


def test_unknown_slug_errors_with_catalogue() -> None:
    env = read_doc("does-not-exist")
    assert env["status"] == "error"
    assert env["errors"]
    assert "list_docs" in env["errors"][0]["message"]


def _docs_available() -> bool:
    """True when the doc tree resolves (editable / source checkout)."""
    return (_repo_root() / "docs" / "source" / "inputs" / "forcing-data.rst").exists()


_skip_no_docs = pytest.mark.skipif(
    not _docs_available(),
    reason="docs/source not resolvable from repo root (non-editable install; docs not bundled).",
)


@_skip_no_docs
@pytest.mark.parametrize(
    "slug, needle",
    [
        ("forcing-data", "Tair"),
        ("tutorial-setup-own-site", "US-AR1"),
        ("output-variables", "T2"),
        ("parameterisations", "water"),
    ],
)
def test_doc_resolves_to_real_content(slug: str, needle: str) -> None:
    """Each key slug resolves to non-empty content containing an expected token."""
    env = read_doc(slug)
    assert env["status"] == "success", env.get("warnings")
    content = "\n".join(
        s.get("content") or "" for s in env["data"]["snippets"]
    )
    assert content.strip(), f"{slug!r} returned empty content"
    assert needle in content, f"{slug!r} content missing expected token {needle!r}"
