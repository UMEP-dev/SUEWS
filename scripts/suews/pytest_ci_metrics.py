"""Low-overhead, machine-readable metrics for SUEWS pytest jobs.

Load this module as a pytest plugin and set ``SUEWS_CI_METRICS`` to the output
path. The plugin performs no additional collection or test execution: it only
records timestamps and data already exposed by pytest hooks.
"""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass, field
from datetime import UTC, datetime
import hashlib
import json
import os
from pathlib import Path
import platform
import time
from typing import Any
import warnings

import pytest

SCHEMA_VERSION = 1
SUMMARY_WARNING_LIMIT = 10


@dataclass
class _MetricsState:
    """Mutable measurements for one pytest controller process."""

    session_started: float = 0.0
    collection_seconds: float = 0.0
    tests_seconds: float = 0.0
    node_ids: list[str] = field(default_factory=list)
    warning_counts: Counter[tuple[str, str]] = field(default_factory=Counter)
    effective_worker_count: int = 1
    uses_xdist: bool = False

    def reset(self) -> None:
        """Clear measurements when pytest is invoked again in one process."""
        self.session_started = time.perf_counter()
        self.collection_seconds = 0.0
        self.tests_seconds = 0.0
        self.node_ids.clear()
        self.warning_counts.clear()
        self.effective_worker_count = 1
        self.uses_xdist = False


_STATE = _MetricsState()


def _normalise_node_ids(node_ids: list[str]) -> list[str]:
    """Return stable, sorted node IDs without duplicates."""
    return sorted(set(node_ids))


def _inventory(node_ids: list[str]) -> dict[str, Any]:
    """Build the stable test-coverage fingerprint."""
    normalised = _normalise_node_ids(node_ids)
    digest = hashlib.sha256("\n".join(normalised).encode()).hexdigest()
    return {
        "node_count": len(normalised),
        "node_id_sha256": digest,
    }


def _warning_records(
    counts: Counter[tuple[str, str]],
) -> list[dict[str, Any]]:
    """Group warnings by category and message with a stable fingerprint."""
    records = []
    for (category, message), count in sorted(counts.items()):
        fingerprint = hashlib.sha256(f"{category}\n{message}".encode()).hexdigest()
        records.append({
            "category": category,
            "count": count,
            "fingerprint": fingerprint,
            "message": message,
        })
    return records


def _metrics(exit_code: int, session_seconds: float) -> dict[str, Any]:
    """Serialise the current controller measurements."""

    def duration(seconds: float) -> dict[str, float]:
        return {"duration_seconds": round(seconds, 6)}

    return {
        "schema_version": SCHEMA_VERSION,
        "generated_at": datetime.now(UTC).isoformat().replace("+00:00", "Z"),
        "environment": {
            "github_job": os.environ.get("GITHUB_JOB"),
            "python": platform.python_version(),
            "runner_arch": os.environ.get("RUNNER_ARCH"),
            "runner_os": os.environ.get("RUNNER_OS"),
        },
        "result": {"exit_code": exit_code},
        "phases": {
            "collection": duration(_STATE.collection_seconds),
            "session": duration(session_seconds),
            "tests": duration(_STATE.tests_seconds),
        },
        "inventory": _inventory(_STATE.node_ids),
        "execution": {
            "effective_worker_count": _STATE.effective_worker_count,
            "xdist": _STATE.uses_xdist,
        },
        "warnings": _warning_records(_STATE.warning_counts),
    }


def _write_json(path: Path, metrics: dict[str, Any]) -> None:
    """Write the artefact atomically so failed writes cannot leave partial JSON."""
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary_path = path.with_suffix(path.suffix + ".tmp")
    temporary_path.write_text(
        json.dumps(metrics, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary_path.replace(path)


def _append_step_summary(path: Path, metrics: dict[str, Any]) -> None:
    """Append a compact human view to the GitHub Actions step summary."""
    phases = metrics["phases"]
    inventory = metrics["inventory"]
    execution = metrics["execution"]
    warning_records = metrics["warnings"]
    lines = [
        "## Pytest CI metrics",
        "",
        "| Measurement | Value |",
        "|---|---:|",
        f"| Collected tests | {inventory['node_count']} |",
        f"| Effective workers | {execution['effective_worker_count']} |",
        f"| Collection | {phases['collection']['duration_seconds']:.3f} s |",
        f"| Tests | {phases['tests']['duration_seconds']:.3f} s |",
        f"| Session | {phases['session']['duration_seconds']:.3f} s |",
        "",
        f"Coverage fingerprint: `{inventory['node_id_sha256']}`",
        "",
        "### Grouped warnings",
        "",
    ]
    if warning_records:
        ordered_warnings = sorted(
            warning_records,
            key=lambda record: (-record["count"], record["fingerprint"]),
        )
        lines.extend(
            f"- {record['count']} x {record['category']}: {record['message']} "
            f"(`{record['fingerprint'][:12]}`)"
            for record in ordered_warnings[:SUMMARY_WARNING_LIMIT]
        )
        hidden_count = len(ordered_warnings) - SUMMARY_WARNING_LIMIT
        if hidden_count > 0:
            lines.append(
                f"- {hidden_count} additional warning fingerprints are in the JSON artefact."
            )
    else:
        lines.append("No warnings recorded.")
    lines.append("")

    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as summary:
        summary.write("\n".join(lines))


def pytest_sessionstart(session: pytest.Session) -> None:
    """Reset state and start the whole-session timer."""
    _STATE.reset()


@pytest.hookimpl(hookwrapper=True)
def pytest_collection(session: pytest.Session):
    """Measure collection without issuing an additional collection pass."""
    started = time.perf_counter()
    yield
    _STATE.collection_seconds += time.perf_counter() - started


def pytest_collection_finish(session: pytest.Session) -> None:
    """Capture node IDs for serial pytest runs."""
    if session.items:
        _STATE.node_ids = [item.nodeid for item in session.items]


@pytest.hookimpl(optionalhook=True)
def pytest_xdist_setupnodes(config: pytest.Config, specs: list[Any]) -> None:
    """Capture the resolved worker count, including ``-n auto`` runs."""
    _STATE.uses_xdist = True
    _STATE.effective_worker_count = len(specs)


@pytest.hookimpl(optionalhook=True)
def pytest_xdist_node_collection_finished(node: Any, ids: list[str]) -> None:
    """Capture the common xdist inventory reported by the first worker."""
    if not _STATE.node_ids:
        _STATE.node_ids = list(ids)


@pytest.hookimpl(hookwrapper=True)
def pytest_runtestloop(session: pytest.Session):
    """Measure the test phase around pytest's existing run loop."""
    started = time.perf_counter()
    yield
    _STATE.tests_seconds += time.perf_counter() - started


def pytest_warning_recorded(
    warning_message: warnings.WarningMessage,
    when: str,
    nodeid: str,
    location: tuple[str, int, str] | None,
) -> None:
    """Group warnings by their stable category and message."""
    del when, nodeid, location
    category = warning_message.category.__name__
    _STATE.warning_counts[category, str(warning_message.message)] += 1


def pytest_sessionfinish(session: pytest.Session, exitstatus: int) -> None:
    """Write the requested artefact once, from the controller process only."""
    if hasattr(session.config, "workerinput"):
        return
    output = os.environ.get("SUEWS_CI_METRICS")
    if not output:
        return

    session_seconds = max(0.0, time.perf_counter() - _STATE.session_started)
    metrics = _metrics(int(exitstatus), session_seconds)
    _write_json(Path(output), metrics)

    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        _append_step_summary(Path(summary), metrics)
