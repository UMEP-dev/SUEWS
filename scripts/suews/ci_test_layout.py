#!/usr/bin/env python3
"""Lay out the test jobs of one platform's wheel: cells and physics shards.

Called by the ``layout`` job in ``.github/workflows/build-wheels-reusable.yml``
with that call's inputs, and writes JSON outputs to ``$GITHUB_OUTPUT`` (or stdout):

Schema 1 (active default on master):
``api_cells``
    One entry per api job: ``{"python_version", "shard", "tier"}``. The
    Python named by ``--shard-python`` (or every non-``bookend_core`` Python
    when ``--shard-python`` is ``"all"``) is split into ``--shard-count``
    shards (``K-of-N``, scripts/suews/pytest_shard.py); with
    ``--bookend-core`` every other Python runs the ``core-bookend`` api tier
    (every core api test plus smoke) in one job instead of the full tier.
    Everything else runs the call's tier whole.
``physics_shards``
    The physics shard labels (``K-of-N``) the separate physics jobs run, or a
    single empty label when the physics tier stays inside the wheel job.
``effective_split_tests``
    ``"true"`` or ``"false"`` indicating whether physics tests split out of
    the wheel job.

Schema 2 (``--schema 2`` or ``GATE_AXIS_SCHEMA=2``, #2349 W1-6):
    Adds ``native_cells`` (once per OS on ``--full-python``, optionally
    sharded), ``abi3_smoke_cells`` (unsharded cross-CPython smoke for every
    other interpreter), and ``os_compat_cells`` (unsharded ``os_compat`` slice
    when ``--include-os-compat`` is passed), while emitting ``api_cells=[]``
    and keeping ``physics-core`` unsharded inside the wheel job.

Small tiers are never sharded: an empty shard exits pytest with code 5.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import sys

if __package__:
    from .ci_tiers import (
        PHYSICS_TIERS_V2,
        SCHEMA_V1,
        SCHEMA_V2,
        schema_from_env,
        validate_schema,
    )
else:
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    from ci_tiers import (
        PHYSICS_TIERS_V2,
        SCHEMA_V1,
        SCHEMA_V2,
        schema_from_env,
        validate_schema,
    )

# Tiers that run few enough tests that a shard could come out empty (Schema 1).
# Checked against build-wheels-reusable.yml by test_ci_run_metrics.py.
UNSHARDED_TIERS = ("smoke", "core", "cfg")

# Schema 2 physics tiers that stay inside the wheel job without sharding:
# ``physics-core`` runs only smoke plus test_sample_output_exact.py (~50 s).
SCHEMA_V2_UNSHARDED_TIERS = (*UNSHARDED_TIERS, "physics-core")
SCHEMA_V2_TIERS = tuple(PHYSICS_TIERS_V2)


def _validate_schema2_tier(tier: str) -> str:
    """Reject unknown Schema 2 tiers so typos cannot silently alter sharding."""
    if tier not in PHYSICS_TIERS_V2:
        raise ValueError(
            f"unknown schema 2 tier {tier!r}; expected one of {SCHEMA_V2_TIERS}"
        )
    return tier


def _validate_python_tag(python: str, label: str = "python") -> str:
    """Require a non-empty Python tag string."""
    if not isinstance(python, str) or not python.strip():
        raise ValueError(f"{label} must be a non-empty Python tag, got {python!r}")
    return python


def _parse_python_list(raw: str, flag: str) -> list[str]:
    """Parse a JSON array of non-empty Python tag strings."""
    try:
        parsed = json.loads(raw)
    except json.JSONDecodeError as exc:
        raise ValueError(f"{flag} is not valid JSON: {exc}") from None
    if not isinstance(parsed, list) or not all(
        isinstance(item, str) and item.strip() for item in parsed
    ):
        raise ValueError(
            f"{flag} must be a JSON array of non-empty strings, got {raw!r}"
        )
    return list(parsed)


def shard_labels(count: int) -> list[str]:
    """Return ``["1-of-N", ..., "N-of-N"]``, or ``[""]`` for one shard."""
    if count <= 1:
        return [""]
    return [f"{k}-of-{count}" for k in range(1, count + 1)]


def api_cells(
    pythons: list[str],
    tier: str,
    shard_python: str,
    shard_count: int,
    bookend_core: bool,
    full_python: str,
) -> list[dict[str, str]]:
    """Return the Schema 1 api job cells for one platform."""
    cells: list[dict[str, str]] = []
    for python in pythons:
        if bookend_core and python != full_python:
            cells.append({
                "python_version": python,
                "shard": "",
                "tier": "core-bookend",
            })
            continue
        count = (
            shard_count
            if shard_python in {"all", python} and tier not in UNSHARDED_TIERS
            else 1
        )
        cells.extend(
            {"python_version": python, "shard": label, "tier": tier}
            for label in shard_labels(count)
        )
    return cells


def native_cells(
    full_python: str,
    tier: str,
    shard_count: int = 1,
) -> list[dict[str, str]]:
    """Return the Schema 2 native job cells for one platform (on ``full_python`` only).

    Draft tiers (``UNSHARDED_TIERS``) are never sharded. Ready tiers -- including
    ``physics-core`` where native still runs its full ready suite -- honour
    ``shard_count``.
    """
    _validate_python_tag(full_python, "full_python")
    _validate_schema2_tier(tier)
    count = shard_count if tier not in UNSHARDED_TIERS else 1
    return [
        {"python_version": full_python, "shard": label, "tier": tier, "lane": "native"}
        for label in shard_labels(count)
    ]


def abi3_smoke_cells(
    pythons: list[str],
    tier: str,
    full_python: str = "cp312",
) -> list[dict[str, str]]:
    """Return the Schema 2 ``abi3_smoke`` cells for interpreters other than ``full_python``."""
    _validate_python_tag(full_python, "full_python")
    _validate_schema2_tier(tier)
    seen: set[str] = set()
    cells: list[dict[str, str]] = []
    for python in pythons:
        _validate_python_tag(python, "python")
        if python == full_python or python in seen:
            continue
        seen.add(python)
        cells.append({
            "python_version": python,
            "shard": "",
            "tier": tier,
            "lane": "abi3_smoke",
        })
    return cells


def os_compat_cells(
    full_python: str,
    tier: str,
    *,
    include: bool = True,
) -> list[dict[str, str]]:
    """Return the Schema 2 ``os_compat`` cell for one platform when enabled."""
    _validate_schema2_tier(tier)
    if not include:
        return []
    _validate_python_tag(full_python, "full_python")
    return [
        {"python_version": full_python, "shard": "", "tier": tier, "lane": "os_compat"}
    ]


def effective_split_tests(
    split_tests: bool,
    tier: str,
    *,
    schema: int = SCHEMA_V1,
) -> bool:
    """Return whether physics tests actually split out of the wheel job.

    Unsharded tiers (``smoke``, ``core``, ``cfg`` in Schema 1, plus
    ``physics-core`` in Schema 2) stay inside the wheel job even when
    ``split_tests`` is requested: running one unsharded physics job would
    upload its metrics under the wheel job's artefact name (#2284).
    """
    resolved = validate_schema(schema)
    if resolved == SCHEMA_V2:
        _validate_schema2_tier(tier)
        unsharded: tuple[str, ...] = SCHEMA_V2_UNSHARDED_TIERS
    else:
        unsharded = UNSHARDED_TIERS
    return bool(split_tests and tier not in unsharded)


def physics_shards(
    split_tests: bool,
    tier: str,
    count: int,
    *,
    schema: int = SCHEMA_V1,
) -> list[str]:
    """Return the physics shard labels; ``[""]`` means the physics tier stays in the wheel job.

    A split layout always has at least two shards: an unsharded physics job
    would upload its metrics under the wheel job's artefact name.
    """
    if not effective_split_tests(split_tests, tier, schema=schema):
        return [""]
    return shard_labels(max(count, 2))


def _layout_lines_from_args(args: argparse.Namespace) -> list[str]:
    """Compute the ``GITHUB_OUTPUT`` lines for parsed CLI ``args``."""
    schema = schema_from_env() if args.schema is None else validate_schema(args.schema)
    _validate_python_tag(args.full_python, "--full-python")
    if schema == SCHEMA_V1:
        if args.api_python is None:
            raise ValueError("--api-python is required for schema 1")
        if (
            args.native_shards is not None
            or args.abi3_smoke_python is not None
            or args.include_os_compat
        ):
            raise ValueError(
                "--native-shards, --abi3-smoke-python and --include-os-compat "
                "are only valid with --schema 2"
            )
        pythons = _parse_python_list(args.api_python, "--api-python")
        cells = api_cells(
            pythons,
            args.tier,
            args.shard_python,
            args.shard_count,
            args.bookend_core,
            args.full_python,
        )
        split_active = effective_split_tests(
            args.split_tests, args.tier, schema=SCHEMA_V1
        )
        shards = physics_shards(
            args.split_tests, args.tier, args.physics_shards, schema=SCHEMA_V1
        )
        return [
            f"api_cells={json.dumps(cells, separators=(',', ':'))}",
            f"physics_shards={json.dumps(shards, separators=(',', ':'))}",
            f"effective_split_tests={'true' if split_active else 'false'}",
        ]

    if args.bookend_core:
        raise ValueError("--bookend-core is only valid with --schema 1")
    if args.abi3_smoke_python is not None:
        smoke_pythons = _parse_python_list(
            args.abi3_smoke_python, "--abi3-smoke-python"
        )
    elif args.api_python is not None:
        smoke_pythons = _parse_python_list(args.api_python, "--api-python")
    else:
        smoke_pythons = []
    native = native_cells(
        args.full_python,
        args.tier,
        1 if args.native_shards is None else args.native_shards,
    )
    abi3_smoke = abi3_smoke_cells(smoke_pythons, args.tier, args.full_python)
    os_compat = os_compat_cells(
        args.full_python, args.tier, include=args.include_os_compat
    )
    split_active = effective_split_tests(args.split_tests, args.tier, schema=SCHEMA_V2)
    shards = physics_shards(
        args.split_tests, args.tier, args.physics_shards, schema=SCHEMA_V2
    )
    return [
        "api_cells=[]",
        f"native_cells={json.dumps(native, separators=(',', ':'))}",
        f"abi3_smoke_cells={json.dumps(abi3_smoke, separators=(',', ':'))}",
        f"os_compat_cells={json.dumps(os_compat, separators=(',', ':'))}",
        f"physics_shards={json.dumps(shards, separators=(',', ':'))}",
        f"effective_split_tests={'true' if split_active else 'false'}",
    ]


def main(argv: list[str] | None = None) -> int:
    """Write layout JSON outputs to ``$GITHUB_OUTPUT`` (or stdout)."""
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument(
        "--schema",
        type=int,
        choices=(SCHEMA_V1, SCHEMA_V2),
        default=None,
        help="Axis schema version (1 or 2; defaults to GATE_AXIS_SCHEMA or 1)",
    )
    parser.add_argument(
        "--api-python",
        default=None,
        help="JSON array of Python tags (required in schema 1; fallback for abi3_smoke in schema 2)",
    )
    parser.add_argument("--tier", required=True)
    parser.add_argument("--shard-python", default="")
    parser.add_argument("--shard-count", type=int, default=2)
    parser.add_argument("--bookend-core", action="store_true")
    parser.add_argument("--full-python", default="cp312")
    parser.add_argument("--split-tests", action="store_true")
    parser.add_argument("--physics-shards", type=int, default=1)
    parser.add_argument(
        "--native-shards",
        type=int,
        default=None,
        help="Schema 2 only: shard count for the native lane on --full-python",
    )
    parser.add_argument(
        "--abi3-smoke-python",
        default=None,
        help="Schema 2 only: JSON array of Python tags for the abi3_smoke lane",
    )
    parser.add_argument(
        "--include-os-compat",
        action="store_true",
        help="Schema 2 only: emit an os_compat cell for this platform",
    )
    args = parser.parse_args(argv)

    try:
        lines = _layout_lines_from_args(args)
    except ValueError as exc:
        print(f"[X] {exc}", file=sys.stderr)
        return 2

    output = os.environ.get("GITHUB_OUTPUT")
    if output:
        with Path(output).open("a", encoding="utf-8") as stream:
            stream.write("\n".join(lines) + "\n")
    print("\n".join(lines), file=sys.stdout)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
