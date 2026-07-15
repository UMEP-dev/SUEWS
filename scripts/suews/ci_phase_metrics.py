#!/usr/bin/env python3
"""Record cibuildwheel phase boundaries and combine them with pytest metrics."""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import time
from typing import Any

SCHEMA_VERSION = 1
REQUIRED_WHEEL_PHASES = ("checkout", "toolchain_setup", "build", "repair", "install")
PYTEST_PHASES = ("collection", "tests", "session")


def _read_state(path: Path) -> dict[str, Any]:
    """Read a phase state file or return an empty state."""
    if not path.exists():
        return {"schema_version": SCHEMA_VERSION, "phases": {}}
    return json.loads(path.read_text(encoding="utf-8"))


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    """Write JSON atomically across phase subprocesses."""
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(path)


def start_phase(
    path: Path,
    phase: str,
    *,
    now: float | None = None,
    source: str = "cibuildwheel-hook-wall-clock",
) -> None:
    """Record the start boundary for one phase."""
    state = _read_state(path)
    existing = state["phases"].get(phase)
    if existing and existing.get("status") == "running":
        raise ValueError(f"Phase {phase!r} is already running.")
    state["phases"][phase] = {
        "source": source,
        "started_at_epoch_seconds": time.time() if now is None else now,
        "status": "running",
    }
    _write_json(path, state)


def stop_phase(path: Path, phase: str, *, now: float | None = None) -> None:
    """Record the completion boundary for one running phase."""
    state = _read_state(path)
    record = state["phases"].get(phase)
    if not record or record.get("status") != "running":
        raise ValueError(f"Phase {phase!r} has no running start boundary.")
    completed = time.time() if now is None else now
    started = float(record["started_at_epoch_seconds"])
    if completed < started:
        raise ValueError(f"Phase {phase!r} completed before it started.")
    record.update({
        "completed_at_epoch_seconds": completed,
        "duration_seconds": round(completed - started, 6),
        "status": "measured",
    })
    _write_json(path, state)


def transition_phase(
    path: Path,
    from_phase: str,
    to_phase: str,
    *,
    now: float | None = None,
    source: str = "cibuildwheel-hook-wall-clock",
) -> None:
    """Close one phase and open the next at the same wall-clock boundary."""
    boundary = time.time() if now is None else now
    stop_phase(path, from_phase, now=boundary)
    start_phase(path, to_phase, now=boundary, source=source)


def _measured_phase(record: dict[str, Any]) -> dict[str, Any]:
    """Reduce internal timestamps to the published phase contract."""
    return {
        "available": True,
        "duration_seconds": record["duration_seconds"],
        "reason": None,
        "source": record["source"],
        "status": "measured",
    }


def _unavailable_phase(phase: str) -> dict[str, Any]:
    """Publish a missing phase explicitly rather than as zero seconds."""
    return {
        "available": False,
        "duration_seconds": None,
        "reason": f"No completed {phase} boundary was recorded.",
        "source": None,
        "status": "unavailable",
    }


def read_pytest_metrics(path: Path) -> dict[str, Any]:
    """Read pytest metrics or describe an earlier wheel-job failure."""
    if path.exists():
        return json.loads(path.read_text(encoding="utf-8"))
    return {
        "available": False,
        "reason": "The wheel job failed before pytest metrics were written.",
        "schema_version": None,
        "result": None,
        "phases": None,
        "inventory": None,
        "resources": None,
    }


def finalise_evidence(
    state_path: Path,
    pytest_metrics: dict[str, Any],
    *,
    job_name: str,
) -> dict[str, Any]:
    """Combine wheel boundaries and pytest schema v2 into one job artefact."""
    state = _read_state(state_path)
    phases = {}
    for phase in REQUIRED_WHEEL_PHASES:
        record = state["phases"].get(phase, {})
        phases[phase] = (
            _measured_phase(record)
            if record.get("status") == "measured"
            else _unavailable_phase(phase)
        )
    pytest_phases = pytest_metrics.get("phases") or {}
    for phase in PYTEST_PHASES:
        pytest_phase = pytest_phases.get(phase)
        if pytest_phase and "duration_seconds" in pytest_phase:
            phases[phase] = {
                "available": True,
                "duration_seconds": pytest_phase["duration_seconds"],
                "reason": None,
                "source": "pytest-hook",
                "status": "measured",
            }
        else:
            phases[phase] = _unavailable_phase(phase)
    return {
        "schema_version": SCHEMA_VERSION,
        "kind": "wheel-job-ci-metrics",
        "job_name": job_name,
        "phases": phases,
        "pytest_metrics": pytest_metrics,
    }


def _phase_summary_value(record: dict[str, Any]) -> str:
    """Render a measured or explicitly unavailable phase."""
    if record.get("available"):
        return f"{record['duration_seconds']:.3f} s"
    return f"unavailable ({record.get('reason', 'no reason recorded')})"


def _resource_summary_value(record: dict[str, Any], *, bytes_value: bool) -> str:
    """Render one resource measurement without inventing a zero."""
    if record.get("available"):
        value = float(record["value"])
        if bytes_value:
            return f"{value / (1024 * 1024):.1f} MiB"
        return f"{value:.3f} s"
    return str(record.get("status", "unavailable"))


def append_step_summary(path: Path, evidence: dict[str, Any]) -> None:
    """Append a compact host-side wheel, pytest and resource summary."""
    phases = evidence["phases"]
    pytest_metrics = evidence["pytest_metrics"]
    inventory = pytest_metrics.get("inventory") or {}
    resources = pytest_metrics.get("resources") or {}
    cpu = resources.get("process_tree_cpu_seconds", {})
    rss = resources.get("process_tree_peak_rss_bytes", {})
    lines = [
        f"## Wheel job CI metrics: {evidence['job_name']}",
        "",
        "| Measurement | Value |",
        "|---|---:|",
    ]
    phase_labels = {
        "checkout": "Checkout",
        "toolchain_setup": "Toolchain Setup",
        "build": "Build",
        "repair": "Repair",
        "install": "Install",
        "collection": "Collection",
        "tests": "Tests",
    }
    lines.extend(
        f"| {label} | {_phase_summary_value(phases[phase])} |"
        for phase, label in phase_labels.items()
    )
    lines.extend([
        f"| Collected tests | {inventory.get('node_count', 'unavailable')} |",
        f"| Process-tree CPU | {_resource_summary_value(cpu, bytes_value=False)} |",
        f"| Process-tree peak RSS | {_resource_summary_value(rss, bytes_value=True)} |",
        "",
        f"Coverage fingerprint: `{inventory.get('node_id_sha256') or 'unavailable'}`",
        "",
    ])
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as summary:
        summary.write("\n".join(lines))


def _state_path(argument: Path | None) -> Path:
    """Resolve the CLI state path from an argument or environment."""
    if argument is not None:
        return argument
    environment_path = os.environ.get("SUEWS_CI_PHASES")
    if not environment_path:
        raise SystemExit("Set SUEWS_CI_PHASES or pass --state.")
    return Path(environment_path)


def _parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--state", type=Path)
    subparsers = parser.add_subparsers(dest="command", required=True)
    start = subparsers.add_parser("start")
    start.add_argument("phase")
    start.add_argument("--source", default="cibuildwheel-hook-wall-clock")
    stop = subparsers.add_parser("stop")
    stop.add_argument("phase")
    transition = subparsers.add_parser("transition")
    transition.add_argument("from_phase")
    transition.add_argument("to_phase")
    transition.add_argument("--source", default="cibuildwheel-hook-wall-clock")
    finalise = subparsers.add_parser("finalise")
    finalise.add_argument("--pytest", type=Path, required=True)
    finalise.add_argument("--output", type=Path, required=True)
    finalise.add_argument("--job-name", required=True)
    return parser


def main() -> int:
    """Execute one phase-boundary or finalisation operation."""
    args = _parser().parse_args()
    state_path = _state_path(args.state)
    if args.command == "start":
        start_phase(state_path, args.phase, source=args.source)
    elif args.command == "stop":
        stop_phase(state_path, args.phase)
    elif args.command == "transition":
        transition_phase(
            state_path,
            args.from_phase,
            args.to_phase,
            source=args.source,
        )
    else:
        pytest_metrics = read_pytest_metrics(args.pytest)
        evidence = finalise_evidence(
            state_path,
            pytest_metrics,
            job_name=args.job_name,
        )
        _write_json(args.output, evidence)
        summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
        if summary_path:
            append_step_summary(Path(summary_path), evidence)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
