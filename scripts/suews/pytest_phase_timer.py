"""Minimal pytest test-phase timer used as the metrics-off overhead control."""

from __future__ import annotations

from dataclasses import dataclass, field
import hashlib
import json
import os
from pathlib import Path
import time
from typing import Any

import pytest


@dataclass
class _TimerState:
    """Mutable controller state for the metrics-off control."""

    started: float = 0.0
    duration: float = 0.0
    node_ids: list[str] = field(default_factory=list)

    def reset(self) -> None:
        """Clear state for a repeat invocation in one process."""
        self.started = 0.0
        self.duration = 0.0
        self.node_ids.clear()


_STATE = _TimerState()


def _inventory(node_ids: list[str]) -> dict[str, Any]:
    """Build the same stable inventory fields as the full plugin."""
    normalised = sorted(set(node_ids))
    return {
        "node_count": len(normalised),
        "node_id_sha256": hashlib.sha256("\n".join(normalised).encode()).hexdigest(),
    }


def pytest_sessionstart(session: pytest.Session) -> None:
    """Reset module state for repeat in-process invocations."""
    del session
    _STATE.reset()


def pytest_collection_finish(session: pytest.Session) -> None:
    """Capture serial node IDs."""
    if session.items:
        _STATE.node_ids = [item.nodeid for item in session.items]


@pytest.hookimpl(optionalhook=True)
def pytest_xdist_node_collection_finished(node: Any, ids: list[str]) -> None:
    """Capture the common xdist inventory once."""
    del node
    if not _STATE.node_ids:
        _STATE.node_ids = list(ids)


@pytest.hookimpl(hookwrapper=True)
def pytest_runtestloop(session: pytest.Session):
    """Measure only pytest's existing test loop."""
    del session
    _STATE.started = time.perf_counter()
    yield
    _STATE.duration += time.perf_counter() - _STATE.started


def pytest_sessionfinish(session: pytest.Session, exitstatus: int) -> None:
    """Write one control result from the xdist controller or serial process."""
    if hasattr(session.config, "workerinput"):
        return
    output = os.environ.get("SUEWS_CI_PHASE_TIMER")
    if not output:
        return
    path = Path(output)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "schema_version": 1,
        "mode": "metrics-off-control",
        "result": {"exit_code": int(exitstatus)},
        "phases": {"tests": {"duration_seconds": round(_STATE.duration, 6)}},
        "inventory": _inventory(_STATE.node_ids),
    }
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(path)
