#!/usr/bin/env python3
"""Lay out the test jobs of one platform's wheel: api cells and physics shards.

Called by the ``layout`` job in ``.github/workflows/build-wheels-reusable.yml``
with that call's inputs, and writes two JSON outputs:

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

Small tiers are never sharded: an empty shard exits pytest with code 5.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import sys

# Tiers that run few enough tests that a shard could come out empty.
UNSHARDED_TIERS = ("smoke", "core", "cfg")


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
    """Return the api job cells for one platform."""
    cells: list[dict[str, str]] = []
    for python in pythons:
        if bookend_core and python != full_python:
            cells.append({"python_version": python, "shard": "", "tier": "core-bookend"})
            continue
        count = (
            shard_count
            if (shard_python == "all" or python == shard_python)
            and tier not in UNSHARDED_TIERS
            else 1
        )
        cells.extend(
            {"python_version": python, "shard": label, "tier": tier}
            for label in shard_labels(count)
        )
    return cells


def effective_split_tests(split_tests: bool, tier: str) -> bool:
    """Return whether physics tests actually split out of the wheel job.

    Unsharded tiers (``smoke``, ``core``, ``cfg``) stay inside the wheel job
    even when ``split_tests`` is requested: running one unsharded physics job
    would upload its metrics under the wheel job's artefact name (#2284).
    """
    return bool(split_tests and tier not in UNSHARDED_TIERS)


def physics_shards(split_tests: bool, tier: str, count: int) -> list[str]:
    """Return the physics shard labels; ``[""]`` means the physics tier stays in the wheel job.

    A split layout always has at least two shards: an unsharded physics job
    would upload its metrics under the wheel job's artefact name.
    """
    if not effective_split_tests(split_tests, tier):
        return [""]
    return shard_labels(max(count, 2))


def main(argv: list[str] | None = None) -> int:
    """Write ``api_cells``, ``physics_shards``, and ``effective_split_tests`` to ``$GITHUB_OUTPUT`` (or stdout)."""
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--api-python", required=True, help="JSON array of Python tags")
    parser.add_argument("--tier", required=True)
    parser.add_argument("--shard-python", default="")
    parser.add_argument("--shard-count", type=int, default=2)
    parser.add_argument("--bookend-core", action="store_true")
    parser.add_argument("--full-python", default="cp312")
    parser.add_argument("--split-tests", action="store_true")
    parser.add_argument("--physics-shards", type=int, default=1)
    args = parser.parse_args(argv)

    cells = api_cells(
        json.loads(args.api_python),
        args.tier,
        args.shard_python,
        args.shard_count,
        args.bookend_core,
        args.full_python,
    )
    split_active = effective_split_tests(args.split_tests, args.tier)
    shards = physics_shards(args.split_tests, args.tier, args.physics_shards)
    lines = [
        f"api_cells={json.dumps(cells, separators=(',', ':'))}",
        f"physics_shards={json.dumps(shards, separators=(',', ':'))}",
        f"effective_split_tests={'true' if split_active else 'false'}",
    ]
    output = os.environ.get("GITHUB_OUTPUT")
    if output:
        with Path(output).open("a", encoding="utf-8") as stream:
            stream.write("\n".join(lines) + "\n")
    print("\n".join(lines), file=sys.stdout)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
