#!/usr/bin/env python3
"""Tabulate matched pytest trials recorded by ``pytest_ci_metrics``.

The hosted ``api-workers`` lane of the scheduler benchmark runs the same API
selection serially and on N xdist workers in A/B/B/A order. This script turns
the four schema-v2 metrics artefacts into one Markdown table (wall time, peak
RSS per worker and process tree, worker finish skew) plus a JSON record. It
reports; it does not gate. A trial whose artefact is missing is listed as
such, because a worker lane killed by the runner writes no artefact at all.
"""

from __future__ import annotations

import argparse
from collections.abc import Sequence
import ctypes
from ctypes import wintypes
import json
import os
from pathlib import Path
from statistics import median
import sys
from typing import Any

MIB = 1024 * 1024
SUMMARY_SCHEMA_VERSION = 1


class TrialError(ValueError):
    """Raised when a trial argument or artefact cannot be interpreted."""


def runner_memory_bytes() -> int | None:
    """Return total physical memory on Linux, macOS or Windows, else None."""
    if sys.platform.startswith("win"):

        class MemoryStatusEx(ctypes.Structure):
            _fields_ = [
                ("dwLength", wintypes.DWORD),
                ("dwMemoryLoad", wintypes.DWORD),
                ("ullTotalPhys", ctypes.c_uint64),
                ("ullAvailPhys", ctypes.c_uint64),
                ("ullTotalPageFile", ctypes.c_uint64),
                ("ullAvailPageFile", ctypes.c_uint64),
                ("ullTotalVirtual", ctypes.c_uint64),
                ("ullAvailVirtual", ctypes.c_uint64),
                ("ullAvailExtendedVirtual", ctypes.c_uint64),
            ]

        status = MemoryStatusEx()
        status.dwLength = ctypes.sizeof(status)
        try:
            kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
            if not kernel32.GlobalMemoryStatusEx(ctypes.byref(status)):
                return None
        except (OSError, AttributeError):
            return None
        return int(status.ullTotalPhys)
    try:
        return int(os.sysconf("SC_PHYS_PAGES")) * int(os.sysconf("SC_PAGE_SIZE"))
    except (ValueError, OSError, AttributeError):
        return None


def _value(measurement: dict[str, Any] | None) -> int | float | None:
    """Return a measurement's value when it is available."""
    if not isinstance(measurement, dict) or not measurement.get("available"):
        return None
    return measurement.get("value")


def _mib(value: int | float | None) -> str:
    """Render bytes as MiB or a dash."""
    return "-" if value is None else f"{value / MIB:.0f}"


def _seconds(value: float | None) -> str:
    """Render seconds to one decimal or a dash."""
    return "-" if value is None else f"{value:.1f}"


def summarise_trial(label: str, path: Path) -> dict[str, Any]:
    """Compact one metrics artefact; a missing file is a reportable row."""
    if not path.is_file():
        return {"label": label, "path": str(path), "missing": True}
    try:
        metrics = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise TrialError(f"{label}: cannot read {path}: {error}") from error
    if metrics.get("schema_version") != 2:
        raise TrialError(f"{label}: {path} is not a schema-v2 metrics artefact")

    execution = metrics["execution"]
    resources = metrics["resources"]
    workers = execution["workers"]
    worker_peaks = [_value(worker.get("peak_rss_bytes")) for worker in workers]
    reported = [peak for peak in worker_peaks if peak is not None]
    outcomes = metrics["result"]["outcomes"]
    return {
        "label": label,
        "path": str(path),
        "missing": False,
        "exit_code": metrics["result"]["exit_code"],
        "outcomes": outcomes,
        "node_count": metrics["inventory"]["node_count"],
        "node_id_sha256": metrics["inventory"]["node_id_sha256"],
        "xdist": execution["xdist"],
        "requested_worker_count": execution["effective_worker_count"],
        "worker_records": len(workers),
        "workers_reporting_peak": len(reported),
        "session_seconds": metrics["phases"]["session"]["duration_seconds"],
        "tests_seconds": metrics["phases"]["tests"]["duration_seconds"],
        "process_tree_peak_rss_bytes": _value(
            resources.get("process_tree_peak_rss_bytes")
        ),
        "controller_peak_rss_bytes": _value(resources.get("controller_peak_rss_bytes")),
        "max_worker_peak_rss_bytes": max(reported) if reported else None,
        "sum_worker_peak_rss_bytes": sum(reported) if reported else None,
        "worker_finish_skew_seconds": execution["worker_finish_skew_seconds"],
        "worker_tail_over_median_seconds": execution["worker_tail_over_median_seconds"],
        "runner_os": metrics["environment"].get("runner_os"),
        "python": metrics["environment"].get("python"),
    }


def _per_process_peak(trial: dict[str, Any]) -> int | float | None:
    """Return the largest single-process peak: a worker's, or the serial controller's."""
    if trial["xdist"]:
        return trial["max_worker_peak_rss_bytes"]
    return trial["controller_peak_rss_bytes"]


def _row(trial: dict[str, Any]) -> str:
    """Render one Markdown table row."""
    if trial["missing"]:
        return f"| {trial['label']} | missing | - | - | - | - | - | - | - | - | - |"
    outcomes = trial["outcomes"]
    outcome_text = f"{outcomes['passed']}/{outcomes['failed']}/{outcomes['skipped']}"
    workers = (
        f"{trial['worker_records']}/{trial['requested_worker_count']}"
        if trial["xdist"]
        else "serial"
    )
    return (
        f"| {trial['label']} | {workers} | {trial['exit_code']} | {outcome_text} "
        f"| {_seconds(trial['session_seconds'])} | {_seconds(trial['tests_seconds'])} "
        f"| {_mib(trial['process_tree_peak_rss_bytes'])} "
        f"| {_mib(_per_process_peak(trial))} "
        f"| {_mib(trial['sum_worker_peak_rss_bytes'])} "
        f"| {_seconds(trial['worker_finish_skew_seconds'])} "
        f"| {_seconds(trial['worker_tail_over_median_seconds'])} |"
    )


def build_summary(
    trials: list[dict[str, Any]],
    *,
    title: str,
    memory_bytes: int | None,
) -> dict[str, Any]:
    """Derive the serial/parallel comparison from the tabulated trials."""
    present = [trial for trial in trials if not trial["missing"]]
    serial = [trial["session_seconds"] for trial in present if not trial["xdist"]]
    parallel = [trial["session_seconds"] for trial in present if trial["xdist"]]
    worker_peaks = [
        trial["max_worker_peak_rss_bytes"]
        for trial in present
        if trial["xdist"] and trial["max_worker_peak_rss_bytes"] is not None
    ]
    tree_peaks = [
        trial["process_tree_peak_rss_bytes"]
        for trial in present
        if trial["process_tree_peak_rss_bytes"] is not None
    ]
    inventories = {trial["node_id_sha256"] for trial in present}
    speedup = (
        median(serial) / median(parallel)
        if serial and parallel and median(parallel)
        else None
    )
    max_tree = max(tree_peaks) if tree_peaks else None
    headroom = (
        None
        if memory_bytes is None or max_tree is None
        else 1.0 - max_tree / memory_bytes
    )
    return {
        "schema_version": SUMMARY_SCHEMA_VERSION,
        "title": title,
        "runner_memory_bytes": memory_bytes,
        "trials": trials,
        "missing_trials": [trial["label"] for trial in trials if trial["missing"]],
        "same_inventory": len(inventories) <= 1,
        "median_serial_session_seconds": median(serial) if serial else None,
        "median_parallel_session_seconds": median(parallel) if parallel else None,
        "speedup": speedup,
        "max_worker_peak_rss_bytes": max(worker_peaks) if worker_peaks else None,
        "max_process_tree_peak_rss_bytes": max_tree,
        "process_tree_memory_headroom_fraction": headroom,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    """Render the table and the derived comparison."""
    lines = [
        f"## {summary['title']}",
        "",
        "| Trial | Workers | Exit | Pass/fail/skip | Session s | Tests s "
        "| Tree peak MiB | Per-process peak MiB | Sum worker peaks MiB "
        "| Finish skew s | Tail over median s |",
        "|---|---|---:|---|---:|---:|---:|---:|---:|---:|---:|",
        *(_row(trial) for trial in summary["trials"]),
        "",
    ]
    memory = summary["runner_memory_bytes"]
    lines.append(
        f"- Runner memory: {_mib(memory)} MiB" if memory else "- Runner memory: unknown"
    )
    if summary["missing_trials"]:
        lines.append(
            "- Missing artefacts (trial ended before pytest wrote metrics): "
            + ", ".join(summary["missing_trials"])
        )
    if not summary["same_inventory"]:
        lines.append("- WARNING: the trials did not collect the same node inventory")
    if summary["speedup"] is not None:
        lines.append(
            f"- Median session: serial {summary['median_serial_session_seconds']:.1f} s, "
            f"parallel {summary['median_parallel_session_seconds']:.1f} s "
            f"(speedup {summary['speedup']:.2f}x)"
        )
    if summary["max_worker_peak_rss_bytes"] is not None:
        lines.append(
            f"- Max single-worker peak RSS across parallel trials: "
            f"{_mib(summary['max_worker_peak_rss_bytes'])} MiB"
        )
    if summary["max_process_tree_peak_rss_bytes"] is not None:
        headroom = summary["process_tree_memory_headroom_fraction"]
        headroom_text = (
            "" if headroom is None else f" ({headroom:.0%} of runner memory left)"
        )
        lines.append(
            f"- Max process-tree peak RSS (Linux procfs only): "
            f"{_mib(summary['max_process_tree_peak_rss_bytes'])} MiB{headroom_text}"
        )
    else:
        lines.append(
            "- Process-tree peak RSS: not sampled on this platform; "
            "per-process peaks exclude child processes"
        )
    lines.append("")
    return "\n".join(lines)


def _parse_trial(argument: str) -> tuple[str, Path]:
    """Split ``LABEL=PATH``."""
    label, separator, path = argument.partition("=")
    if not separator or not label or not path:
        raise TrialError(f"trial must be LABEL=PATH, got {argument!r}")
    return label, Path(path)


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--trial",
        action="append",
        required=True,
        metavar="LABEL=PATH",
        help="metrics artefact for one trial, in run order",
    )
    parser.add_argument("--title", default="Pytest ABBA trials")
    parser.add_argument(
        "--runner-memory-bytes",
        default="auto",
        help="total runner memory in bytes, or 'auto' to read it here",
    )
    parser.add_argument("--output", type=Path, help="JSON record to write")
    parser.add_argument("--summary", type=Path, help="Markdown file to append to")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    """Tabulate the trials, print the Markdown and write the optional outputs."""
    args = _parser().parse_args(argv)
    try:
        pairs = [_parse_trial(argument) for argument in args.trial]
        trials = [summarise_trial(label, path) for label, path in pairs]
    except TrialError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2
    if args.runner_memory_bytes == "auto":
        memory = runner_memory_bytes()
    else:
        memory = int(args.runner_memory_bytes)
    summary = build_summary(trials, title=args.title, memory_bytes=memory)
    markdown = render_markdown(summary)
    print(markdown)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(
            json.dumps(summary, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
    if args.summary is not None:
        args.summary.parent.mkdir(parents=True, exist_ok=True)
        with args.summary.open("a", encoding="utf-8") as handle:
            handle.write(markdown)
    return 0


if __name__ == "__main__":
    sys.exit(main())
