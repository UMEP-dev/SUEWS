"""Pytest marker expressions of the CI test tiers, for the scripts that check them.

Schema 1 (active default on master) defines ``physics`` and ``api`` lanes.
The workflows spell those same expressions out inline (the physics tier in
``.github/actions/build-suews/action.yml``, the api tier in
``.github/workflows/test-api-cross-python-reusable.yml``), because a workflow
cannot import Python. ``test/core/test_core_guard.py`` reads both files and
fails when either drifts from ``PHYSICS_TIERS`` or ``API_TIERS``, so the
core-test guard (``scripts/suews/check_core_guard.py``) always judges a lane
against the expression that lane actually ran.

Schema 2 (activated by ``GATE_AXIS_SCHEMA=2`` in #2349 W1-6) defines the 4+1
dependency axes (``tooling``, ``unit``, ``native``, ``physics`` plus
``os_compat``) and the installed-wheel ``abi3_smoke`` cross-CPython lane,
alongside the ``physics-core`` ready tier (smoke plus the exact sample-output
reference witness).
"""

from __future__ import annotations

from collections.abc import Iterable, Mapping
import os
import sys

SCHEMA_V1 = 1
SCHEMA_V2 = 2
SUPPORTED_SCHEMAS = (SCHEMA_V1, SCHEMA_V2)
SCHEMA_ENV_VAR = "GATE_AXIS_SCHEMA"

SCHEMA_V1_LANES = ("physics", "api")
SCHEMA_V2_AXES = ("tooling", "unit", "native", "physics")
SCHEMA_V2_AUX_LANES = ("os_compat", "abi3_smoke")
SCHEMA_V2_LANES = (*SCHEMA_V2_AXES, *SCHEMA_V2_AUX_LANES)

PHYSICS_TIERS: dict[str, str] = {
    "smoke": "physics and smoke and not (medium or slow)",
    "core": "physics and (smoke or core) and not slow",
    "cfg": "physics and (smoke or cfg) and not slow",
    "standard": "physics and (core or not slow)",
    "physics-full": "physics",
    "all": "physics",
}

API_TIERS: dict[str, str] = {
    "smoke": "api and smoke and not (medium or slow) and not qgis",
    "core": "api and (smoke or core) and not slow and not qgis",
    "cfg": "api and (smoke or cfg) and not slow and not qgis",
    "standard": "api and (core or not slow) and not qgis",
    "physics-full": "api and (core or not slow) and not qgis",
    "all": "api",
    # The newer Python bookend on a ready pull request (22 Nov 2026): every
    # core api test, slow ones included, plus smoke; the non-core api tests
    # on that Python wait for the nightly. The build Python runs the full tier.
    "core-bookend": "api and (smoke or core) and not qgis",
}


def _standard_axis_tiers(axis: str) -> dict[str, str]:
    """Return the Schema 2 marker expressions for one non-physics axis."""
    return {
        "smoke": f"{axis} and smoke and not (medium or slow) and not qgis",
        "core": f"{axis} and (smoke or core) and not slow and not qgis",
        "cfg": f"{axis} and (smoke or cfg) and not slow and not qgis",
        "physics-core": f"{axis} and (core or not slow) and not qgis",
        "standard": f"{axis} and (core or not slow) and not qgis",
        "physics-full": f"{axis} and (core or not slow) and not qgis",
        "all": axis,
    }


# Schema 2 physics tiers: adds ``physics-core`` for wheel-building Python PRs
# outside the four full-physics triggers (smoke tests via -m plus
# test/core/test_sample_output_exact.py added explicitly by the runner/guard).
PHYSICS_TIERS_V2: dict[str, str] = {
    **PHYSICS_TIERS,
    "physics-core": "physics and smoke and not (medium or slow)",
}

TOOLING_TIERS: dict[str, str] = _standard_axis_tiers("tooling")
UNIT_TIERS: dict[str, str] = _standard_axis_tiers("unit")
NATIVE_TIERS: dict[str, str] = _standard_axis_tiers("native")
OS_COMPAT_TIERS: dict[str, str] = _standard_axis_tiers("os_compat")

ABI3_SMOKE_EXPRESSION = "smoke_bridge and not (medium or slow) and not qgis"
ABI3_SMOKE_TIERS: dict[str, str] = dict.fromkeys(
    ("smoke", "core", "cfg", "physics-core", "standard", "physics-full", "all"),
    ABI3_SMOKE_EXPRESSION,
)

SCHEMA_TABLES: dict[int, dict[str, dict[str, str]]] = {
    SCHEMA_V1: {
        "physics": PHYSICS_TIERS,
        "api": API_TIERS,
    },
    SCHEMA_V2: {
        "tooling": TOOLING_TIERS,
        "unit": UNIT_TIERS,
        "native": NATIVE_TIERS,
        "physics": PHYSICS_TIERS_V2,
        "os_compat": OS_COMPAT_TIERS,
        "abi3_smoke": ABI3_SMOKE_TIERS,
    },
}

# Tiers that run every core test of both axes in Schema 1. Only these are
# mergeable evidence: a draft runs smoke, core (not slow) or cfg, and the
# merge queue re-runs the standard tier on Linux whatever the pull request ran.
READY_TIERS = ("standard", "physics-full")

# Schema 2 ready tiers: adds ``physics-core`` for Python-only pull requests.
READY_TIERS_V2 = ("physics-core", "standard", "physics-full")


def validate_schema(schema: int) -> int:
    """Validate and return a supported axis schema version (1 or 2)."""
    if (
        isinstance(schema, bool)
        or not isinstance(schema, int)
        or schema not in SUPPORTED_SCHEMAS
    ):
        raise ValueError(
            f"unsupported axis schema {schema!r}; expected one of {SUPPORTED_SCHEMAS}"
        )
    return schema


def schema_from_env(env: Mapping[str, str] | None = None) -> int:
    """Resolve the axis schema from ``GATE_AXIS_SCHEMA``.

    Unset or empty defaults to ``1`` (the active Schema 1 gate on master); any
    other value besides ``'1'`` or ``'2'`` raises ``ValueError``.
    """
    source = os.environ if env is None else env
    raw = source.get(SCHEMA_ENV_VAR)
    if not raw:
        return SCHEMA_V1
    if raw == "1":
        return SCHEMA_V1
    if raw == "2":
        return SCHEMA_V2
    raise ValueError(f"invalid {SCHEMA_ENV_VAR}={raw!r}; expected '1' or '2'")


def ready_tiers(schema: int = SCHEMA_V1) -> tuple[str, ...]:
    """Return the ready-PR test tiers accepted under ``schema``."""
    resolved = validate_schema(schema)
    return READY_TIERS if resolved == SCHEMA_V1 else READY_TIERS_V2


def tier_expression(kind: str, tier: str, *, schema: int = SCHEMA_V1) -> str:
    """Return the marker expression lane ``kind`` runs at ``tier`` under ``schema``."""
    resolved = validate_schema(schema)
    table = SCHEMA_TABLES[resolved].get(kind)
    if table is None:
        raise ValueError(f"unknown lane kind {kind!r} for schema {resolved}")
    try:
        return table[tier]
    except KeyError:
        raise ValueError(
            f"unknown test tier {tier!r} for lane {kind!r} (schema {resolved})"
        ) from None


def _parse_cli_args(args: list[str]) -> tuple[str, str, int | None]:
    """Parse ``[--schema 1|2] KIND TIER`` without raising SystemExit."""
    positional: list[str] = []
    schema: int | None = None
    index = 0
    while index < len(args):
        token = args[index]
        if token == "--schema":
            if index + 1 >= len(args):
                raise ValueError("--schema requires a value (1 or 2)")
            raw = args[index + 1]
            index += 2
            if raw not in {"1", "2"}:
                raise ValueError(f"invalid --schema {raw!r}; expected 1 or 2")
            schema = int(raw)
        elif token.startswith("--schema="):
            raw = token.split("=", 1)[1]
            index += 1
            if raw not in {"1", "2"}:
                raise ValueError(f"invalid --schema {raw!r}; expected 1 or 2")
            schema = int(raw)
        elif token.startswith("-"):
            raise ValueError(f"unknown option {token!r}")
        else:
            positional.append(token)
            index += 1
    if len(positional) != 2:
        raise ValueError("usage: ci_tiers.py [--schema 1|2] KIND TIER")
    return positional[0], positional[1], schema


def main(argv: list[str] | None = None) -> int:
    """Print the marker expression for ``KIND TIER`` (used by CI shard jobs)."""
    args = sys.argv[1:] if argv is None else argv
    try:
        kind, tier, cli_schema = _parse_cli_args(args)
        schema = schema_from_env() if cli_schema is None else cli_schema
        print(tier_expression(kind, tier, schema=schema))
    except ValueError as exc:
        print(exc, file=sys.stderr)
        return 2
    return 0


def matches(expression: str, markers: Iterable[str]) -> bool:
    """Evaluate a pytest ``-m`` expression against a test's marker names."""
    # Deferred import so layout jobs without pytest installed can still import ci_tiers.
    from _pytest.mark.expression import (  # ruff: ignore[import-outside-top-level, import-private-name]
        Expression,
    )

    names = set(markers)
    return Expression.compile(expression).evaluate(
        lambda name, /, **_kwargs: name in names
    )


if __name__ == "__main__":
    raise SystemExit(main())
