"""Pytest marker expressions of the CI test tiers, for the scripts that check them.

The workflows spell the same expressions out inline (the physics tier in
``.github/actions/build-suews/action.yml``, the api tier in
``.github/workflows/test-api-cross-python-reusable.yml``), because a workflow
cannot import Python. ``test/core/test_core_guard.py`` reads both files and
fails when either drifts from the tables below, so the core-test guard
(``scripts/suews/check_core_guard.py``) always judges a lane against the
expression that lane actually ran.
"""

from __future__ import annotations

from collections.abc import Iterable

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

# Tiers that run every core test of both axes. Only these are mergeable
# evidence: a draft runs smoke, core (not slow) or cfg, and the merge queue
# re-runs the standard tier on Linux whatever the pull request ran.
READY_TIERS = ("standard", "physics-full")


def tier_expression(kind: str, tier: str) -> str:
    """Return the marker expression lane ``kind`` (physics or api) runs at ``tier``."""
    table = {"physics": PHYSICS_TIERS, "api": API_TIERS}.get(kind)
    if table is None:
        raise ValueError(f"unknown lane kind {kind!r}")
    try:
        return table[tier]
    except KeyError:
        raise ValueError(f"unknown test tier {tier!r}") from None


def main(argv: list[str] | None = None) -> int:
    """Print the marker expression for ``KIND TIER`` (used by the physics shard jobs)."""
    import sys

    args = sys.argv[1:] if argv is None else argv
    if len(args) != 2:
        print("usage: ci_tiers.py physics|api TIER", file=sys.stderr)
        return 2
    try:
        print(tier_expression(args[0], args[1]))
    except ValueError as exc:
        print(exc, file=sys.stderr)
        return 2
    return 0


def matches(expression: str, markers: Iterable[str]) -> bool:
    """Evaluate a pytest ``-m`` expression against a test's marker names."""
    from _pytest.mark.expression import Expression

    names = set(markers)
    return Expression.compile(expression).evaluate(
        lambda name, /, **_kwargs: name in names
    )


if __name__ == "__main__":
    raise SystemExit(main())
