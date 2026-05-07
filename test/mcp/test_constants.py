"""Sanity tests on the constants module."""

from __future__ import annotations

import pytest

pytestmark = pytest.mark.api


def test_allowed_subcommands_contain_full_set() -> None:
    from suews_mcp.constants import ALLOWED_SUBCOMMANDS

    expected = (
        "validate",
        "schema",
        "convert",
        "run",
        "inspect",
        "diagnose",
        "summarise",
        "compare",
        "init",
        "knowledge",
    )
    for cmd in expected:
        assert cmd in ALLOWED_SUBCOMMANDS


def test_uri_prefixes_share_scheme() -> None:
    from suews_mcp.constants import (
        URI_DOCS_PREFIX,
        URI_EXAMPLES_PREFIX,
        URI_KNOWLEDGE_PREFIX,
        URI_RUNS_PREFIX,
        URI_SCHEMA_PREFIX,
        URI_SCHEME,
    )

    for prefix in (
        URI_SCHEMA_PREFIX,
        URI_EXAMPLES_PREFIX,
        URI_DOCS_PREFIX,
        URI_RUNS_PREFIX,
        URI_KNOWLEDGE_PREFIX,
    ):
        assert prefix.startswith(URI_SCHEME)


def test_timeout_is_positive() -> None:
    from suews_mcp.constants import DEFAULT_TIMEOUT_SECONDS

    assert DEFAULT_TIMEOUT_SECONDS > 0
