"""Shared fixtures for the SUEWS MCP test suite.

The ``search_schema`` tool caches the parsed ``suews schema`` envelope in a
process-global dict (``schema_search._SCHEMA_CACHE``) so a long-lived MCP
server pays the ~10s CLI cost only once per version. That cache is the right
production behaviour, but it leaks across tests: a test that monkeypatches
``run_suews_cli`` to return a small fake schema populates the cache with that
fake (the cache only skips *error* envelopes, not mocked-success ones), and
every later test — including ones that shell to the real CLI — then reads the
poisoned cache and finds no fields.

The autouse fixture below clears the cache around every test so each test sees
a clean slate, regardless of collection order.
"""

from __future__ import annotations

import pytest


@pytest.fixture(autouse=True)
def _clear_mcp_caches():
    """Reset the per-process MCP caches before and after each test.

    Both ``schema_search._SCHEMA_CACHE`` and ``readiness._SAMPLE_CACHE`` cache
    *successful* CLI envelopes for the process lifetime (correct in production),
    so a test that monkeypatches ``run_suews_cli`` with a fake success would
    poison them for later tests. Clearing around each test guarantees isolation
    regardless of collection order.
    """
    from suews_mcp.tools import readiness, schema_search

    schema_search._SCHEMA_CACHE.clear()
    readiness._SAMPLE_CACHE.clear()
    yield
    schema_search._SCHEMA_CACHE.clear()
    readiness._SAMPLE_CACHE.clear()
