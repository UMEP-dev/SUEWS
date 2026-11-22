"""Deterministic test sharding for the CI api lane, without a new dependency.

Load this module as a pytest plugin and pass ``--suews-shard K/N`` to keep
only the collected tests whose node id hashes to shard ``K`` of ``N``; the
rest are deselected. The hash is SHA-256 of the node id, so the split is the
same on every platform, interpreter and xdist worker (Python's built-in
``hash()`` is salted per process and would give each xdist worker a
different selection, which xdist rejects). The N shards are disjoint and
together cover every collected test.

The split is by node id rather than by file because one file dominates the
Windows api lane: ``test/core/test_stream_chunk.py`` was 37% of its test wall
time in run 44997671456, so a file-level split left one shard at 1.35 times
an even half, against 1.02 to 1.03 for the node-id split across three
pull-request runs. The cost is that a module-scoped fixture used by tests
in both shards is built once in each.

Without the option the plugin does nothing, so loading it is harmless.
"""

from __future__ import annotations

import hashlib
import re

import pytest

_SHARD_RE = re.compile(r"^(\d+)/(\d+)$")


def parse_shard(value: str) -> tuple[int, int]:
    """Return ``(index, total)`` for ``K/N`` with ``1 <= K <= N``."""
    match = _SHARD_RE.match(value.strip())
    if match is None:
        raise ValueError(f"--suews-shard expects K/N, got {value!r}")
    index, total = int(match.group(1)), int(match.group(2))
    if total < 1 or not 1 <= index <= total:
        raise ValueError(f"--suews-shard {value!r} needs 1 <= K <= N")
    return index, total


def shard_of(node_id: str, total: int) -> int:
    """Return the 1-based shard that ``node_id`` belongs to out of ``total``."""
    digest = hashlib.sha256(node_id.encode("utf-8")).digest()
    return int.from_bytes(digest[:8], "big") % total + 1


def pytest_addoption(parser: pytest.Parser) -> None:
    parser.addoption(
        "--suews-shard",
        default=None,
        metavar="K/N",
        help="Run only shard K of N, split by SHA-256 of the test node id.",
    )


def pytest_configure(config: pytest.Config) -> None:
    value = config.getoption("--suews-shard")
    if value:
        try:
            parse_shard(value)
        except ValueError as exc:
            raise pytest.UsageError(str(exc)) from exc


@pytest.hookimpl(trylast=True)
def pytest_collection_modifyitems(
    config: pytest.Config, items: list[pytest.Item]
) -> None:
    value = config.getoption("--suews-shard")
    if not value:
        return
    index, total = parse_shard(value)
    keep: list[pytest.Item] = []
    drop: list[pytest.Item] = []
    for item in items:
        (keep if shard_of(item.nodeid, total) == index else drop).append(item)
    if drop:
        config.hook.pytest_deselected(items=drop)
        items[:] = keep
