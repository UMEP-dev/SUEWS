"""Write every collected test's node id and marker names to a JSON file.

Load it as a pytest plugin with ``--collect-only`` and
``--suews-inventory PATH``. The Linux cp312 api lane records the whole suite
this way, independently of the tier's ``-m`` expression and of any shard
split, so the core-test guard (``scripts/suews/check_core_guard.py``) can
compare what each lane ran against what the suite contains. Without the option
the plugin does nothing.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

SCHEMA_VERSION = 1


def pytest_addoption(parser: pytest.Parser) -> None:
    """Register ``--suews-inventory``."""
    parser.addoption(
        "--suews-inventory",
        default=None,
        metavar="PATH",
        help="Write the collected node ids and their marker names to PATH (JSON).",
    )


def inventory(items: list[pytest.Item]) -> dict[str, object]:
    """Return the inventory document for ``items``."""
    tests = [
        {
            "node_id": item.nodeid,
            "markers": sorted({marker.name for marker in item.iter_markers()}),
        }
        for item in items
    ]
    tests.sort(key=lambda record: record["node_id"])
    # "nodes", not "tests": the metrics readers (scripts/lint/check_cost_markers.py
    # and others) treat any document with a "tests" list as pytest metrics.
    return {
        "kind": "test-inventory",
        "schema_version": SCHEMA_VERSION,
        "node_count": len(tests),
        "nodes": tests,
    }


@pytest.hookimpl(trylast=True)
def pytest_collection_finish(session: pytest.Session) -> None:
    """Write the inventory once collection is complete (not on xdist workers)."""
    path = session.config.getoption("--suews-inventory")
    if not path or hasattr(session.config, "workerinput"):
        return
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(
        json.dumps(inventory(session.items), indent=1, sort_keys=True) + "\n",
        encoding="utf-8",
    )
