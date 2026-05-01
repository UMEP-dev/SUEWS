"""Import-time visibility tests for the procedural-API deprecation (gh#1370).

The procedural top-level API (``run_supy``, ``init_supy``, ...) is being
phased out in favour of ``SUEWSSimulation``. Phase 1 added an in-body
``_warn_functional_deprecation`` call to each function so that *calling*
the function emits a ``FutureWarning``. This test guards Phase 2 of the
plan: the lazy ``__getattr__`` in ``supy/__init__.py`` must also emit the
warning on the first attribute access, so users who hold a reference but
defer the call still see the migration nudge.

The tests run in subprocesses on purpose. ``test/conftest.py`` monkey-patches
the public functions to private implementations during ``pytest_configure``,
which both populates ``supy.__dict__`` (bypassing ``__getattr__`` entirely)
and would consume the import-time warning before any in-process test could
observe it. A clean subprocess gives us the user-visible behaviour.
"""

from __future__ import annotations

import json
import subprocess
import sys
import textwrap

import pytest

pytestmark = pytest.mark.api


def _run_visibility_probe(name: str) -> dict:
    """Spawn a fresh interpreter and return the warning trace for `name`.

    The probe:

    1. imports ``supy`` (does not trigger ``__getattr__`` for `name`),
    2. resolves the attribute twice through ``getattr``,
    3. dumps the captured warnings to stdout as JSON.

    Subprocess isolation guarantees a clean module cache and a clean
    warning filter, neither of which can be enforced from inside the
    pytest run because of ``conftest.py`` monkey-patching.
    """
    probe = textwrap.dedent(
        f"""
        import json, sys, warnings
        warnings.simplefilter("always")
        with warnings.catch_warnings(record=True) as caught_first:
            warnings.simplefilter("always")
            import supy
            getattr(supy, {name!r})
        with warnings.catch_warnings(record=True) as caught_second:
            warnings.simplefilter("always")
            getattr(supy, {name!r})

        def _serialise(records):
            return [
                {{"category": w.category.__name__, "message": str(w.message)}}
                for w in records
            ]

        json.dump(
            {{"first": _serialise(caught_first), "second": _serialise(caught_second)}},
            sys.stdout,
        )
        """
    )
    result = subprocess.run(
        [sys.executable, "-c", probe],
        capture_output=True,
        text=True,
        check=True,
    )
    return json.loads(result.stdout)


def _registry_keys() -> list[str]:
    """Read `_FUNCTIONAL_DEPRECATIONS` from a fresh subprocess.

    Reading it in-process would import `_supy_module` and surface the
    monkey-patched module under conftest, which we want to avoid.
    """
    probe = textwrap.dedent(
        """
        import json, sys
        from supy._supy_module import _FUNCTIONAL_DEPRECATIONS
        json.dump(sorted(_FUNCTIONAL_DEPRECATIONS.keys()), sys.stdout)
        """
    )
    result = subprocess.run(
        [sys.executable, "-c", probe],
        capture_output=True,
        text=True,
        check=True,
    )
    return json.loads(result.stdout)


# Resolved at collection time so failures show one parametrised case per
# deprecated name rather than collapsing into a single opaque assertion.
_DEPRECATED_NAMES = _registry_keys()


@pytest.mark.core
def test_router_set_matches_registry():
    """`_DEPRECATED_FUNCTIONAL_NAMES` in `__init__.py` must equal the registry.

    The router hard-codes the deprecated-name set so the lazy path stays
    fast (no `_supy_module` import on every attribute miss). This test
    guards against the two drifting silently when a future PR adds a new
    deprecated symbol but forgets to wire the router.
    """
    probe = textwrap.dedent(
        """
        import json, sys
        from supy import _DEPRECATED_FUNCTIONAL_NAMES
        from supy._supy_module import _FUNCTIONAL_DEPRECATIONS
        json.dump(
            {
                "router": sorted(_DEPRECATED_FUNCTIONAL_NAMES),
                "registry": sorted(_FUNCTIONAL_DEPRECATIONS.keys()),
            },
            sys.stdout,
        )
        """
    )
    result = subprocess.run(
        [sys.executable, "-c", probe],
        capture_output=True,
        text=True,
        check=True,
    )
    payload = json.loads(result.stdout)
    assert payload["router"] == payload["registry"], (
        "supy.__init__._DEPRECATED_FUNCTIONAL_NAMES has drifted from "
        "_supy_module._FUNCTIONAL_DEPRECATIONS — "
        f"router={payload['router']} registry={payload['registry']}"
    )


@pytest.mark.core
@pytest.mark.parametrize("name", _DEPRECATED_NAMES)
def test_first_access_emits_future_warning(name):
    """First `getattr(supy, name)` must emit one FutureWarning naming the symbol."""
    trace = _run_visibility_probe(name)
    future_warnings_first = [
        record
        for record in trace["first"]
        if record["category"] == "FutureWarning"
        and "deprecated" in record["message"].lower()
        and name in record["message"]
    ]
    assert len(future_warnings_first) == 1, (
        f"Expected exactly one FutureWarning for first access of supy.{name}; "
        f"got {trace['first']}"
    )


@pytest.mark.core
@pytest.mark.parametrize("name", _DEPRECATED_NAMES)
def test_second_access_is_silent(name):
    """Second `getattr` must hit `_lazy_cache` and emit no new warning."""
    trace = _run_visibility_probe(name)
    future_warnings_second = [
        record
        for record in trace["second"]
        if record["category"] == "FutureWarning"
    ]
    assert future_warnings_second == [], (
        f"Second access of supy.{name} re-emitted FutureWarning(s); "
        f"cache should suppress: {trace['second']}"
    )
