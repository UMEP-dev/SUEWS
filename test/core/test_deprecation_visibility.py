"""Import-time visibility tests for the procedural-API deprecation (gh#1370).

The procedural top-level API (``run_supy``, ``init_supy``, ...) is being
phased out in favour of ``SUEWSSimulation``. Phase 1 added an in-body
``_warn_functional_deprecation`` call to each function so that *calling*
the function emits a ``FutureWarning``. This test guards Phase 2 of the
plan: the lazy ``__getattr__`` in ``supy/__init__.py`` must also emit the
warning on the first attribute access, so users who hold a reference but
defer the call still see the migration nudge.

The tests use one subprocess on purpose. ``test/conftest.py`` monkey-patches
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


def _probe_payload() -> dict:
    """Read the registry and probe every deprecated name in one interpreter.

    The probe:

    1. imports ``supy`` and reads the lightweight lazy-router set,
    2. resolves each routed attribute twice through ``getattr``,
    3. imports the functional registry and dumps both sets plus warning traces.

    Subprocess isolation guarantees a clean module cache and a clean
    warning filter, neither of which can be enforced from inside the
    pytest run because of ``conftest.py`` monkey-patching. One subprocess is
    sufficient: resolving one lazy attribute does not populate the others.
    """
    probe = textwrap.dedent(
        """
        import json, sys, warnings
        import supy

        names = sorted(supy._DEPRECATED_FUNCTIONAL_NAMES)

        def _serialise(records):
            return [
                {"category": w.category.__name__, "message": str(w.message)}
                for w in records
            ]

        traces = {}
        for name in names:
            try:
                with warnings.catch_warnings(record=True) as caught_first:
                    warnings.simplefilter("always")
                    getattr(supy, name)
                with warnings.catch_warnings(record=True) as caught_second:
                    warnings.simplefilter("always")
                    getattr(supy, name)
                traces[name] = {
                    "first": _serialise(caught_first),
                    "second": _serialise(caught_second),
                }
            except Exception as exc:
                traces[name] = {"error": repr(exc)}

        from supy._supy_module import _FUNCTIONAL_DEPRECATIONS
        json.dump(
            {
                "router": names,
                "registry": sorted(_FUNCTIONAL_DEPRECATIONS.keys()),
                "traces": traces,
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
    return json.loads(result.stdout)


# Resolve once at collection so failures still show one case per deprecated
# name without launching a fresh interpreter for every assertion.
_PROBE_PAYLOAD = _probe_payload()
_DEPRECATED_NAMES = sorted(
    set(_PROBE_PAYLOAD["router"]) | set(_PROBE_PAYLOAD["registry"])
)


@pytest.mark.core
def test_router_set_matches_registry():
    """`_DEPRECATED_FUNCTIONAL_NAMES` in `__init__.py` must equal the registry.

    The router hard-codes the deprecated-name set so the lazy path stays
    fast (no `_supy_module` import on every attribute miss). This test
    guards against the two drifting silently when a future PR adds a new
    deprecated symbol but forgets to wire the router.
    """
    assert _PROBE_PAYLOAD["router"] == _PROBE_PAYLOAD["registry"], (
        "supy.__init__._DEPRECATED_FUNCTIONAL_NAMES has drifted from "
        "_supy_module._FUNCTIONAL_DEPRECATIONS — "
        f"router={_PROBE_PAYLOAD['router']} "
        f"registry={_PROBE_PAYLOAD['registry']}"
    )


@pytest.mark.core
@pytest.mark.parametrize("name", _DEPRECATED_NAMES)
def test_access_warns_once(name):
    """First access warns for the symbol; the cached second access is silent."""
    assert name in _PROBE_PAYLOAD["traces"], (
        f"supy.{name} is registered but missing from the lazy router probe"
    )
    trace = _PROBE_PAYLOAD["traces"][name]
    assert "error" not in trace, f"supy.{name} could not be resolved: {trace}"
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
    future_warnings_second = [
        record for record in trace["second"] if record["category"] == "FutureWarning"
    ]
    assert future_warnings_second == [], (
        f"Second access of supy.{name} re-emitted FutureWarning(s); "
        f"cache should suppress: {trace['second']}"
    )
