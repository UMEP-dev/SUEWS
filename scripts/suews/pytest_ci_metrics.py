"""Low-overhead, machine-readable metrics for SUEWS pytest jobs.

Load this module as a pytest plugin and set ``SUEWS_CI_METRICS`` to the output
path. The plugin performs no additional collection or test execution: it only
records timestamps and data already exposed by pytest hooks. On Linux, a small
background sampler reads procfs for process-tree CPU time and peak RSS.
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
import re
from statistics import median
import sys
import tempfile
from threading import Event, Thread
import time
from typing import Any
import warnings

import pytest

SCHEMA_VERSION = 2
SUMMARY_WARNING_LIMIT = 10
OUTCOME_NAMES = ("passed", "failed", "skipped", "xfailed", "xpassed")
DEFAULT_SAMPLE_INTERVAL_SECONDS = 0.25
_ADDRESS_RE = re.compile(r"0x[0-9a-fA-F]+")
_UUID_RE = re.compile(
    r"\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-"
    r"[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}\b"
)


@dataclass
class _WorkerMetrics:
    """Measurements attributed to one xdist worker."""

    node_ids: set[str] = field(default_factory=set)
    busy_duration_seconds: float = 0.0
    finished_at_seconds: float | None = None


@dataclass
class _MetricsState:
    """Mutable measurements for one pytest controller process."""

    session_started: float = 0.0
    test_phase_started: float = 0.0
    collection_seconds: float = 0.0
    tests_seconds: float = 0.0
    node_ids: list[str] = field(default_factory=list)
    warning_counts: Counter[tuple[str, str]] = field(default_factory=Counter)
    warning_samples: dict[tuple[str, str], str] = field(default_factory=dict)
    node_outcomes: dict[str, str] = field(default_factory=dict)
    workers: dict[str, _WorkerMetrics] = field(default_factory=dict)
    effective_worker_count: int = 1
    uses_xdist: bool = False
    resource_sampler: ProcfsSampler | None = None

    def reset(self, *, collect_resources: bool) -> None:
        """Clear measurements when pytest is invoked again in one process."""
        if self.resource_sampler is not None:
            self.resource_sampler.stop()
        self.session_started = time.perf_counter()
        self.test_phase_started = 0.0
        self.collection_seconds = 0.0
        self.tests_seconds = 0.0
        self.node_ids.clear()
        self.warning_counts.clear()
        self.warning_samples.clear()
        self.node_outcomes.clear()
        self.workers.clear()
        self.effective_worker_count = 1
        self.uses_xdist = False
        self.resource_sampler = ProcfsSampler.from_environment()
        if collect_resources:
            self.resource_sampler.start()


class ProcfsSampler:
    """Sample the current process tree through Linux procfs."""

    def __init__(
        self,
        interval_seconds: float,
        *,
        proc_root: Path = Path("/proc"),
    ) -> None:
        self.interval_seconds = interval_seconds
        self.proc_root = proc_root
        self.root_pid = os.getpid()
        self.stop_event = Event()
        self.thread: Thread | None = None
        self.sample_count = 0
        self.peak_rss_bytes = 0
        self.cpu_seconds_by_process: dict[tuple[int, int], float] = {}
        self.error_reason: str | None = None
        self.supported = sys.platform.startswith("linux") and proc_root.is_dir()
        self.clock_ticks = float(os.sysconf("SC_CLK_TCK")) if self.supported else 1.0
        self.page_size = int(os.sysconf("SC_PAGE_SIZE")) if self.supported else 1

    @classmethod
    def from_environment(cls) -> ProcfsSampler:
        """Build a sampler with a bounded user-selectable interval."""
        raw_interval = os.environ.get("SUEWS_CI_RESOURCE_SAMPLE_INTERVAL", "")
        try:
            interval = (
                float(raw_interval) if raw_interval else DEFAULT_SAMPLE_INTERVAL_SECONDS
            )
        except ValueError:
            interval = DEFAULT_SAMPLE_INTERVAL_SECONDS
        return cls(min(5.0, max(0.05, interval)))

    def start(self) -> None:
        """Start sampling when procfs is supported."""
        if not self.supported:
            return
        self.thread = Thread(target=self._run, name="suews-ci-resources", daemon=True)
        self.thread.start()

    def stop(self) -> None:
        """Stop sampling and retain the last CPU value for exited children."""
        if self.thread is None:
            return
        self.stop_event.set()
        self.thread.join(timeout=max(1.0, self.interval_seconds * 2))
        self.thread = None

    def measurements(self) -> dict[str, Any]:
        """Return resource values with explicit support metadata."""
        base = {
            "sample_count": self.sample_count,
            "sample_interval_seconds": round(self.interval_seconds, 6),
        }
        if not self.supported:
            reason = "Process-tree sampling requires Linux procfs."
            return {
                **base,
                "process_tree_cpu_seconds": _unavailable("seconds", reason),
                "process_tree_peak_rss_bytes": _unavailable("bytes", reason),
            }
        if self.sample_count == 0:
            reason = self.error_reason or "No readable procfs samples were captured."
            return {
                **base,
                "process_tree_cpu_seconds": _unavailable(
                    "seconds", reason, status="error"
                ),
                "process_tree_peak_rss_bytes": _unavailable(
                    "bytes", reason, status="error"
                ),
            }
        return {
            **base,
            "process_tree_cpu_seconds": _available(
                "seconds",
                round(sum(self.cpu_seconds_by_process.values()), 6),
            ),
            "process_tree_peak_rss_bytes": _available("bytes", self.peak_rss_bytes),
        }

    def _run(self) -> None:
        """Sample immediately, then at the configured interval."""
        while not self.stop_event.is_set():
            try:
                self._sample()
            except (OSError, ValueError) as error:
                self.error_reason = f"procfs sample failed: {type(error).__name__}"
            self.stop_event.wait(self.interval_seconds)
        try:
            self._sample()
        except (OSError, ValueError) as error:
            self.error_reason = f"final procfs sample failed: {type(error).__name__}"

    def _sample(self) -> None:
        """Read current descendants while tolerating child exit races."""
        pending = [self.root_pid]
        seen: set[int] = set()
        total_rss_bytes = 0
        root_read = False
        while pending:
            pid = pending.pop()
            if pid in seen:
                continue
            seen.add(pid)
            pending.extend(_proc_children(pid, proc_root=self.proc_root))
            process = read_proc_process(
                pid,
                self.clock_ticks,
                self.page_size,
                proc_root=self.proc_root,
            )
            if process is None:
                continue
            start_time, cpu_seconds, rss_bytes = process
            root_read = root_read or pid == self.root_pid
            key = (pid, start_time)
            self.cpu_seconds_by_process[key] = max(
                cpu_seconds,
                self.cpu_seconds_by_process.get(key, 0.0),
            )
            total_rss_bytes += rss_bytes
        if root_read:
            self.sample_count += 1
            self.peak_rss_bytes = max(self.peak_rss_bytes, total_rss_bytes)


def _proc_children(pid: int, *, proc_root: Path = Path("/proc")) -> list[int]:
    """Return direct children for a live Linux process."""
    path = proc_root / str(pid) / "task" / str(pid) / "children"
    try:
        content = path.read_text(encoding="utf-8").strip()
    except (FileNotFoundError, PermissionError, ProcessLookupError, OSError):
        return []
    return [int(child) for child in content.split()] if content else []


def read_proc_process(
    pid: int,
    clock_ticks: float,
    page_size: int,
    *,
    proc_root: Path = Path("/proc"),
) -> tuple[int, float, int] | None:
    """Read start time, cumulative CPU and resident pages from procfs stat."""
    path = proc_root / str(pid) / "stat"
    try:
        stat = path.read_text(encoding="utf-8")
        fields = stat[stat.rfind(")") + 2 :].split()
        cpu_seconds = (float(fields[11]) + float(fields[12])) / clock_ticks
        start_time = int(fields[19])
        rss_bytes = max(0, int(fields[21])) * page_size
    except (
        FileNotFoundError,
        PermissionError,
        ProcessLookupError,
        OSError,
        ValueError,
        IndexError,
    ):
        return None
    return start_time, cpu_seconds, rss_bytes


def _available(unit: str, value: Any) -> dict[str, Any]:
    """Build a populated resource measurement."""
    return {
        "available": True,
        "method": "linux-procfs-sampling",
        "reason": None,
        "status": "sampled",
        "unit": unit,
        "value": value,
    }


def _unavailable(
    unit: str,
    reason: str,
    *,
    status: str = "unavailable",
) -> dict[str, Any]:
    """Build an explicit unavailable resource measurement."""
    return {
        "available": False,
        "method": None,
        "reason": reason,
        "status": status,
        "unit": unit,
        "value": None,
    }


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


def _normalise_warning_message(message: str) -> str:
    """Replace volatile path, address and UUID components before hashing."""
    normalised = message
    workspace = os.environ.get("GITHUB_WORKSPACE")
    if workspace:
        normalised = normalised.replace(workspace, "<workspace>")
        normalised = normalised.replace(workspace.replace("/", "\\"), "<workspace>")

    temp_root = tempfile.gettempdir().rstrip("/\\")
    roots = {temp_root}
    if temp_root.startswith("/var/"):
        roots.add("/private" + temp_root)
    for root in sorted(roots, key=len, reverse=True):
        pattern = re.compile(re.escape(root) + r"[/\\](?:tmp|pytest-of-)[^/\\\s:]+")
        normalised = pattern.sub("<temp>", normalised)
    normalised = re.sub(
        r"(?:/tmp|/var/tmp)[/\\](?:tmp|pytest-of-)[^/\\\s:]+",
        "<temp>",
        normalised,
    )
    normalised = re.sub(
        r"[A-Za-z]:\\[^\s:]*\\Temp\\(?:tmp|pytest-of-)[^\\\s:]+",
        "<temp>",
        normalised,
        flags=re.IGNORECASE,
    )
    normalised = _ADDRESS_RE.sub("0x<address>", normalised)
    return _UUID_RE.sub("<uuid>", normalised)


def _warning_records() -> list[dict[str, Any]]:
    """Group warnings by category and normalised message fingerprint."""
    records = []
    for (category, normalised_message), count in sorted(_STATE.warning_counts.items()):
        fingerprint = hashlib.sha256(
            f"{category}\n{normalised_message}".encode()
        ).hexdigest()
        key = (category, normalised_message)
        records.append({
            "category": category,
            "count": count,
            "fingerprint": fingerprint,
            "message": _STATE.warning_samples[key],
            "normalised_message": normalised_message,
        })
    return records


def _outcomes() -> dict[str, int]:
    """Return stable outcome keys, including zero-count outcomes."""
    counts = Counter(_STATE.node_outcomes.values())
    return {name: counts[name] for name in OUTCOME_NAMES}


def _worker_records() -> tuple[list[dict[str, Any]], float, float]:
    """Serialise assignments and final-test completion spread."""
    records = []
    finishes = []
    for worker_id, worker in sorted(_STATE.workers.items()):
        node_ids = sorted(worker.node_ids)
        inventory = _inventory(node_ids)
        finish = worker.finished_at_seconds
        if finish is not None:
            finishes.append(finish)
        records.append({
            "busy_duration_seconds": round(worker.busy_duration_seconds, 6),
            "finished_at_seconds": None if finish is None else round(finish, 6),
            **inventory,
            "node_ids": node_ids,
            "worker_id": worker_id,
        })
    if len(finishes) < 2:
        return records, 0.0, 0.0
    latest = max(finishes)
    return (
        records,
        round(latest - min(finishes), 6),
        round(latest - median(finishes), 6),
    )


def _metrics(exit_code: int, session_seconds: float) -> dict[str, Any]:
    """Serialise the current controller measurements."""

    def duration(seconds: float) -> dict[str, float]:
        return {"duration_seconds": round(seconds, 6)}

    workers, finish_skew, tail_over_median = _worker_records()
    sampler = _STATE.resource_sampler or ProcfsSampler.from_environment()
    return {
        "schema_version": SCHEMA_VERSION,
        "generated_at": datetime.now(UTC).isoformat().replace("+00:00", "Z"),
        "environment": {
            "github_job": os.environ.get("GITHUB_JOB"),
            "python": platform.python_version(),
            "runner_arch": os.environ.get("RUNNER_ARCH"),
            "runner_os": os.environ.get("RUNNER_OS"),
        },
        "result": {"exit_code": exit_code, "outcomes": _outcomes()},
        "phases": {
            "collection": duration(_STATE.collection_seconds),
            "session": duration(session_seconds),
            "tests": duration(_STATE.tests_seconds),
        },
        "inventory": _inventory(_STATE.node_ids),
        "execution": {
            "effective_worker_count": _STATE.effective_worker_count,
            "worker_finish_skew_seconds": finish_skew,
            "worker_tail_over_median_seconds": tail_over_median,
            "workers": workers,
            "xdist": _STATE.uses_xdist,
        },
        "resources": sampler.measurements(),
        "warnings": _warning_records(),
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
    resources = metrics["resources"]
    warning_records = metrics["warnings"]
    cpu = resources["process_tree_cpu_seconds"]
    rss = resources["process_tree_peak_rss_bytes"]
    cpu_value = f"{cpu['value']:.3f} s" if cpu["available"] else cpu["status"]
    rss_value = (
        f"{rss['value'] / (1024 * 1024):.1f} MiB" if rss["available"] else rss["status"]
    )
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
        f"| Worker finish skew | {execution['worker_finish_skew_seconds']:.3f} s |",
        f"| Process-tree CPU | {cpu_value} |",
        f"| Process-tree peak RSS | {rss_value} |",
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
    """Reset state and start whole-session and resource measurements."""
    output_requested = bool(os.environ.get("SUEWS_CI_METRICS"))
    is_worker = hasattr(session.config, "workerinput")
    _STATE.reset(collect_resources=output_requested and not is_worker)


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
    for spec in specs:
        _STATE.workers.setdefault(str(spec.id), _WorkerMetrics())


@pytest.hookimpl(optionalhook=True)
def pytest_xdist_node_collection_finished(node: Any, ids: list[str]) -> None:
    """Capture the common xdist inventory reported by the first worker."""
    if not _STATE.node_ids:
        _STATE.node_ids = list(ids)


@pytest.hookimpl(hookwrapper=True)
def pytest_runtestloop(session: pytest.Session):
    """Measure the test phase around pytest's existing run loop."""
    started = time.perf_counter()
    _STATE.test_phase_started = started
    yield
    _STATE.tests_seconds += time.perf_counter() - started


def pytest_runtest_logreport(report: pytest.TestReport) -> None:
    """Record outcomes and xdist assignments from existing reports."""
    _record_outcome(report)
    worker_id = getattr(report, "worker_id", None)
    if worker_id is None:
        return
    worker = _STATE.workers.setdefault(str(worker_id), _WorkerMetrics())
    worker.node_ids.add(report.nodeid)
    worker.busy_duration_seconds += max(0.0, float(report.duration))
    if _STATE.test_phase_started:
        worker.finished_at_seconds = max(
            0.0,
            time.perf_counter() - _STATE.test_phase_started,
        )


def _record_outcome(report: pytest.TestReport) -> None:
    """Store one terminal outcome per node, allowing teardown failures to win."""
    if report.when == "setup":
        if report.failed:
            _STATE.node_outcomes[report.nodeid] = "failed"
        elif report.skipped:
            _STATE.node_outcomes[report.nodeid] = "skipped"
        return
    if report.when == "call":
        was_xfail = hasattr(report, "wasxfail")
        if report.skipped and was_xfail:
            outcome = "xfailed"
        elif report.passed and was_xfail:
            outcome = "xpassed"
        else:
            outcome = report.outcome
        _STATE.node_outcomes[report.nodeid] = outcome
    elif report.when == "teardown" and report.failed:
        _STATE.node_outcomes[report.nodeid] = "failed"


def pytest_warning_recorded(
    warning_message: warnings.WarningMessage,
    when: str,
    nodeid: str,
    location: tuple[str, int, str] | None,
) -> None:
    """Group warnings by stable category and normalised message."""
    del when, nodeid, location
    category = warning_message.category.__name__
    message = str(warning_message.message)
    normalised = _normalise_warning_message(message)
    key = (category, normalised)
    _STATE.warning_counts[key] += 1
    _STATE.warning_samples.setdefault(key, message)


def pytest_sessionfinish(session: pytest.Session, exitstatus: int) -> None:
    """Write the requested artefact once, from the controller process only."""
    sampler = _STATE.resource_sampler
    if sampler is not None:
        sampler.stop()
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
