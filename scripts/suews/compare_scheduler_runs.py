#!/usr/bin/env python3
"""Compare matched loadscope/worksteal pytest runs in ABBA order."""

from __future__ import annotations

import argparse
from collections.abc import Sequence
import hashlib
import json
import math
from pathlib import Path
import re
from statistics import median
import sys
from typing import Any

ABBA_ORDER = ("loadscope", "worksteal", "worksteal", "loadscope")
MAX_WORKERS = 4
OUTCOME_NAMES = {"passed", "failed", "skipped", "xfailed", "xpassed"}
DEFAULT_MAX_MEDIAN_SESSION_REGRESSION_FRACTION = 0.05
DEFAULT_MAX_PEAK_RSS_REGRESSION_FRACTION = 0.10
DEFAULT_MIN_MEMORY_HEADROOM_FRACTION = 0.20
LINUX_WHEEL_ARTIFACT = "cp312-manylinux-x86_64"


class ComparisonError(ValueError):
    """Raised when matched scheduler evidence violates its contract."""


def _sha256_node_ids(node_ids: list[str]) -> str:
    """Return the provider's stable hash for sorted node IDs."""
    return hashlib.sha256("\n".join(sorted(node_ids)).encode()).hexdigest()


def _delta(challenger: float, incumbent: float) -> dict[str, float | None]:
    """Return absolute and relative challenger-minus-incumbent changes."""
    difference = challenger - incumbent
    relative = difference / incumbent if incumbent else None
    return {"absolute": difference, "fraction": relative}


def _finite_nonnegative(path: Path, label: str, value: Any) -> float:
    """Require one numeric measurement to be finite and non-negative."""
    if (
        isinstance(value, bool)
        or not isinstance(value, (int, float))
        or not math.isfinite(value)
        or value < 0
    ):
        raise ComparisonError(f"{path}: {label} must be finite and non-negative")
    return float(value)


def _validate_provenance(provenance: dict[str, Any] | None) -> dict[str, Any] | None:
    """Validate the source run and exact Linux wheel identity when supplied."""
    if provenance is None:
        return None
    required = {
        "source_run_id",
        "source_sha",
        "expected_source_sha",
        "wheel_artifact_name",
        "wheel_sha256",
    }
    if set(provenance) != required:
        raise ComparisonError("complete hosted provenance is required")
    if isinstance(provenance["source_run_id"], bool) or not isinstance(
        provenance["source_run_id"], int
    ):
        raise ComparisonError("source run ID must be a positive integer")
    if provenance["source_run_id"] <= 0:
        raise ComparisonError("source run ID must be a positive integer")
    if not re.fullmatch(r"[0-9a-f]{40}", provenance["source_sha"]):
        raise ComparisonError("source SHA must be a lowercase 40-character hex digest")
    if provenance["source_sha"] != provenance["expected_source_sha"]:
        raise ComparisonError(
            "source SHA mismatch between successful run and dispatch input"
        )
    if provenance["wheel_artifact_name"] != LINUX_WHEEL_ARTIFACT:
        raise ComparisonError(f"wheel artifact must be {LINUX_WHEEL_ARTIFACT}")
    if not re.fullmatch(r"[0-9a-f]{64}", provenance["wheel_sha256"]):
        raise ComparisonError(
            "wheel SHA-256 must be a lowercase 64-character hex digest"
        )
    return dict(provenance)


def _read_metrics(path: Path) -> dict[str, Any]:
    """Read one schema-v2 pytest metrics artefact."""
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise ComparisonError(f"cannot read metrics artefact {path}: {exc}") from exc
    if payload.get("schema_version") != 2:
        raise ComparisonError(f"{path}: expected metrics schema_version 2")
    return payload


def _validate_workers(path: Path, workers: list[dict[str, Any]]) -> list[str]:
    """Validate per-worker node fingerprints and timing measurements."""
    worker_ids = [worker["worker_id"] for worker in workers]
    if len(worker_ids) != len(set(worker_ids)):
        raise ComparisonError(f"{path}: duplicate worker IDs")

    node_ids = []
    for worker in workers:
        assigned = worker["node_ids"]
        worker_id = worker["worker_id"]
        if worker["node_count"] != len(assigned):
            raise ComparisonError(f"{path}: worker {worker_id} node count mismatch")
        if worker["node_id_sha256"] != _sha256_node_ids(assigned):
            raise ComparisonError(
                f"{path}: worker {worker_id} assignment fingerprint mismatch"
            )
        _finite_nonnegative(
            path, f"worker {worker_id} busy duration", worker["busy_duration_seconds"]
        )
        _finite_nonnegative(
            path, f"worker {worker_id} finish time", worker["finished_at_seconds"]
        )
        node_ids.extend(assigned)
    return node_ids


def _validate_inventory(path: Path, inventory: dict[str, Any]) -> None:
    """Validate the stable collected-node fingerprint."""
    node_count = inventory["node_count"]
    if (
        isinstance(node_count, bool)
        or not isinstance(node_count, int)
        or node_count <= 0
    ):
        raise ComparisonError(f"{path}: inventory node count must be positive")
    if re.fullmatch(r"[0-9a-f]{64}", inventory["node_id_sha256"]) is None:
        raise ComparisonError(f"{path}: inventory node hash must be SHA-256")


def _validate_result(
    path: Path,
    result: dict[str, Any],
    inventory: dict[str, Any],
) -> dict[str, int]:
    """Validate one successful and complete pytest outcome ledger."""
    exit_code = result["exit_code"]
    if isinstance(exit_code, bool) or not isinstance(exit_code, int) or exit_code != 0:
        raise ComparisonError(f"{path}: pytest exit code is {exit_code}")
    outcomes = result["outcomes"]
    if set(outcomes) != OUTCOME_NAMES or any(
        isinstance(value, bool) or not isinstance(value, int) or value < 0
        for value in outcomes.values()
    ):
        raise ComparisonError(f"{path}: complete outcome counts are required")
    if sum(outcomes.values()) != inventory["node_count"]:
        raise ComparisonError(f"{path}: outcome total does not match inventory")
    if outcomes["failed"] != 0:
        raise ComparisonError(f"{path}: successful exit cannot contain failed outcomes")
    return outcomes


def _validate_execution(
    path: Path,
    execution: dict[str, Any],
    inventory: dict[str, Any],
) -> dict[str, Any]:
    """Validate the resolved worker budget, assignments and finish tail."""
    workers = execution["workers"]
    worker_count = execution["effective_worker_count"]
    if execution["xdist"] is not True:
        raise ComparisonError(f"{path}: xdist must be active")
    if (
        isinstance(worker_count, bool)
        or not isinstance(worker_count, int)
        or not 1 <= worker_count <= MAX_WORKERS
    ):
        raise ComparisonError(
            f"{path}: effective worker count must be between 1 and {MAX_WORKERS}"
        )
    if len(workers) != worker_count:
        raise ComparisonError(f"{path}: worker records do not match effective count")
    node_ids = _validate_workers(path, workers)
    if len(node_ids) != len(set(node_ids)):
        raise ComparisonError(f"{path}: worker assignments contain duplicate node IDs")
    if len(node_ids) != inventory["node_count"]:
        raise ComparisonError(f"{path}: worker assignments do not cover the inventory")
    if _sha256_node_ids(node_ids) != inventory["node_id_sha256"]:
        raise ComparisonError(
            f"{path}: worker assignment hash does not match inventory"
        )

    finish_times = [float(worker["finished_at_seconds"]) for worker in workers]
    finish_spread = _finite_nonnegative(
        path, "finish skew", execution["worker_finish_skew_seconds"]
    )
    tail = _finite_nonnegative(
        path, "tail over median", execution["worker_tail_over_median_seconds"]
    )
    if not math.isclose(
        finish_spread, max(finish_times) - min(finish_times), abs_tol=1e-6
    ):
        raise ComparisonError(f"{path}: finish skew does not match worker finish times")
    if not math.isclose(tail, max(finish_times) - median(finish_times), abs_tol=1e-6):
        raise ComparisonError(
            f"{path}: tail over median does not match worker finish times"
        )
    return {
        "worker_count": worker_count,
        "finish_spread_seconds": finish_spread,
        "tail_over_median_seconds": tail,
        "workers": workers,
    }


def _validate_resources(path: Path, resources: dict[str, Any]) -> float:
    """Validate Linux process-tree sampling and return peak RSS bytes."""
    resource = resources["process_tree_peak_rss_bytes"]
    if not resource["available"] or resource["unit"] != "bytes":
        raise ComparisonError(
            f"{path}: process-tree peak RSS is unavailable or not bytes"
        )
    if (
        resource.get("status") != "sampled"
        or resource.get("method") != "linux-procfs-sampling"
    ):
        raise ComparisonError(
            f"{path}: process-tree peak RSS availability metadata is invalid"
        )
    peak_rss_bytes = _finite_nonnegative(path, "peak RSS", resource["value"])
    if peak_rss_bytes == 0:
        raise ComparisonError(f"{path}: peak RSS must be greater than zero")
    sample_count = resources["sample_count"]
    if (
        isinstance(sample_count, bool)
        or not isinstance(sample_count, int)
        or sample_count <= 0
    ):
        raise ComparisonError(f"{path}: resource sample count must be positive")
    sample_interval = _finite_nonnegative(
        path,
        "resource sample interval",
        resources["sample_interval_seconds"],
    )
    if sample_interval == 0:
        raise ComparisonError(f"{path}: resource sample interval must be positive")
    return peak_rss_bytes


def _run_summary_payload(path: Path, scheduler: str) -> dict[str, Any]:
    """Validate and compact one scheduler trial."""
    payload = _read_metrics(path)
    inventory = payload["inventory"]
    _validate_inventory(path, inventory)
    outcomes = _validate_result(path, payload["result"], inventory)
    execution = _validate_execution(path, payload["execution"], inventory)
    wall_seconds = _finite_nonnegative(
        path,
        "session duration",
        payload["phases"]["session"]["duration_seconds"],
    )
    test_seconds = _finite_nonnegative(
        path,
        "test duration",
        payload["phases"]["tests"]["duration_seconds"],
    )
    peak_rss_bytes = _validate_resources(path, payload["resources"])
    workers = execution["workers"]

    return {
        "path": str(path),
        "scheduler": scheduler,
        "wall_seconds": wall_seconds,
        "test_seconds": test_seconds,
        "finish_skew_seconds": execution["finish_spread_seconds"],
        "tail_over_median_seconds": execution["tail_over_median_seconds"],
        "peak_rss_bytes": peak_rss_bytes,
        "worker_count": execution["worker_count"],
        "inventory": inventory,
        "outcomes": outcomes,
        "workers": [
            {
                "worker_id": worker["worker_id"],
                "node_count": worker["node_count"],
                "node_id_sha256": worker["node_id_sha256"],
                "busy_duration_seconds": worker["busy_duration_seconds"],
                "finished_at_seconds": worker["finished_at_seconds"],
            }
            for worker in workers
        ],
    }


def _run_summary(path: Path, scheduler: str) -> dict[str, Any]:
    """Convert malformed provider data into one contract-level error type."""
    try:
        return _run_summary_payload(path, scheduler)
    except ComparisonError:
        raise
    except (KeyError, TypeError, ValueError, IndexError) as exc:
        raise ComparisonError(f"{path}: malformed schema-v2 metrics: {exc}") from exc


def compare_abba(
    paths: Sequence[Path],
    *,
    runner_memory_bytes: int,
    expected_worker_count: int | None = None,
    max_median_session_regression_fraction: float = (
        DEFAULT_MAX_MEDIAN_SESSION_REGRESSION_FRACTION
    ),
    max_peak_rss_regression_fraction: float = DEFAULT_MAX_PEAK_RSS_REGRESSION_FRACTION,
    min_memory_headroom_fraction: float = DEFAULT_MIN_MEMORY_HEADROOM_FRACTION,
    provenance: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Compare two loadscope and two worksteal trials on one runner."""
    if len(paths) != len(ABBA_ORDER):
        raise ComparisonError("ABBA comparison requires exactly four metrics files")
    if runner_memory_bytes <= 0:
        raise ComparisonError("runner memory must be positive")
    if (
        expected_worker_count is not None
        and not 1 <= expected_worker_count <= MAX_WORKERS
    ):
        raise ComparisonError(
            f"expected worker count must be between 1 and {MAX_WORKERS}"
        )
    if not 0 <= max_peak_rss_regression_fraction <= 1:
        raise ComparisonError(
            "peak RSS regression fraction must be between zero and one"
        )
    if not 0 <= max_median_session_regression_fraction <= 1:
        raise ComparisonError(
            "median session regression fraction must be between zero and one"
        )
    if not 0 <= min_memory_headroom_fraction < 1:
        raise ComparisonError(
            "memory headroom fraction must be at least zero and below one"
        )
    validated_provenance = _validate_provenance(provenance)

    runs = [
        _run_summary(Path(path), scheduler)
        for path, scheduler in zip(paths, ABBA_ORDER, strict=True)
    ]
    first = runs[0]
    if any(run["inventory"] != first["inventory"] for run in runs[1:]):
        raise ComparisonError("inventory mismatch across ABBA trials")
    if any(run["outcomes"] != first["outcomes"] for run in runs[1:]):
        raise ComparisonError("outcome mismatch across ABBA trials")
    if any(run["worker_count"] != first["worker_count"] for run in runs[1:]):
        raise ComparisonError("worker count mismatch across ABBA trials")
    if (
        expected_worker_count is not None
        and first["worker_count"] != expected_worker_count
    ):
        raise ComparisonError(
            f"expected {expected_worker_count} effective workers, got {first['worker_count']}"
        )
    grouped = {
        scheduler: [run for run in runs if run["scheduler"] == scheduler]
        for scheduler in ("loadscope", "worksteal")
    }
    schedulers = {
        scheduler: {
            "median_session_seconds": median(run["wall_seconds"] for run in trials),
            "median_test_seconds": median(run["test_seconds"] for run in trials),
            "median_finish_spread_seconds": median(
                run["finish_skew_seconds"] for run in trials
            ),
            "median_tail_over_median_seconds": median(
                run["tail_over_median_seconds"] for run in trials
            ),
            "median_peak_rss_bytes": median(run["peak_rss_bytes"] for run in trials),
            "max_peak_rss_bytes": max(run["peak_rss_bytes"] for run in trials),
        }
        for scheduler, trials in grouped.items()
    }
    loadscope = schedulers["loadscope"]
    worksteal = schedulers["worksteal"]
    if not (
        worksteal["median_finish_spread_seconds"]
        < loadscope["median_finish_spread_seconds"]
        and worksteal["median_tail_over_median_seconds"]
        < loadscope["median_tail_over_median_seconds"]
    ):
        raise ComparisonError(
            "worksteal requires strictly lower median finish spread and tail"
        )
    maximum_worksteal_session_seconds = loadscope["median_session_seconds"] * (
        1 + max_median_session_regression_fraction
    )
    if worksteal["median_session_seconds"] > maximum_worksteal_session_seconds:
        raise ComparisonError(
            "worksteal median session regression exceeds the safety threshold"
        )
    highest_peak_rss = max(run["peak_rss_bytes"] for run in runs)
    memory_headroom_bytes = runner_memory_bytes - highest_peak_rss
    memory_headroom_fraction = memory_headroom_bytes / runner_memory_bytes
    if memory_headroom_fraction < min_memory_headroom_fraction:
        raise ComparisonError(
            "memory headroom is below the configured hosted-runner safety threshold"
        )
    peak_rss_delta = _delta(
        worksteal["max_peak_rss_bytes"],
        loadscope["max_peak_rss_bytes"],
    )
    if peak_rss_delta["fraction"] is None or (
        peak_rss_delta["fraction"] > max_peak_rss_regression_fraction
    ):
        raise ComparisonError(
            "worksteal peak RSS regression exceeds the safety threshold"
        )

    deltas = {
        "median_session_seconds": _delta(
            worksteal["median_session_seconds"], loadscope["median_session_seconds"]
        ),
        "median_test_seconds": _delta(
            worksteal["median_test_seconds"], loadscope["median_test_seconds"]
        ),
        "median_finish_spread_seconds": _delta(
            worksteal["median_finish_spread_seconds"],
            loadscope["median_finish_spread_seconds"],
        ),
        "median_tail_over_median_seconds": _delta(
            worksteal["median_tail_over_median_seconds"],
            loadscope["median_tail_over_median_seconds"],
        ),
        "max_peak_rss_bytes": peak_rss_delta,
    }
    return {
        "schema_version": 1,
        "order": list(ABBA_ORDER),
        "selected_scheduler": "worksteal",
        "provenance": validated_provenance,
        "invariants": {
            "effective_worker_count": first["worker_count"],
            "inventory": first["inventory"],
            "outcomes": first["outcomes"],
            "runner_memory_bytes": runner_memory_bytes,
        },
        "schedulers": schedulers,
        "deltas": deltas,
        "session_guardrail": {
            "loadscope_median_seconds": loadscope["median_session_seconds"],
            "worksteal_median_seconds": worksteal["median_session_seconds"],
            "maximum_worksteal_seconds": maximum_worksteal_session_seconds,
            "maximum_regression_fraction": max_median_session_regression_fraction,
            "passed": True,
        },
        "memory_guardrail": {
            "runner_capacity_bytes": runner_memory_bytes,
            "highest_peak_rss_bytes": highest_peak_rss,
            "headroom_bytes": memory_headroom_bytes,
            "headroom_fraction": memory_headroom_fraction,
            "minimum_headroom_fraction": min_memory_headroom_fraction,
            "maximum_peak_rss_regression_fraction": max_peak_rss_regression_fraction,
            "passed": True,
        },
        "runs": runs,
    }


def _wheel_sha256(path: Path) -> str:
    """Fingerprint the exact wheel installed for all four trials."""
    digest = hashlib.sha256()
    with path.open("rb") as wheel:
        for chunk in iter(lambda: wheel.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _write_manifest(path: Path, manifest: dict[str, Any]) -> None:
    """Write one deterministic decision manifest."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def _append_summary(path: Path, manifest: dict[str, Any]) -> None:
    """Append the hosted scheduler decision to a GitHub step summary."""
    lines = ["## Hosted scheduler ABBA", ""]
    if manifest["status"] != "passed":
        lines.extend([f"Result: failed - {manifest['error']}", ""])
    else:
        loadscope = manifest["schedulers"]["loadscope"]
        worksteal = manifest["schedulers"]["worksteal"]
        memory = manifest["memory_guardrail"]
        session = manifest["session_guardrail"]
        lines.extend([
            "Result: worksteal accepted under the fixed four-worker budget.",
            "",
            "| Measurement | loadscope | worksteal |",
            "|---|---:|---:|",
            f"| Median session | {loadscope['median_session_seconds']:.3f} s | {worksteal['median_session_seconds']:.3f} s |",
            f"| Median test phase | {loadscope['median_test_seconds']:.3f} s | {worksteal['median_test_seconds']:.3f} s |",
            f"| Median finish spread | {loadscope['median_finish_spread_seconds']:.3f} s | {worksteal['median_finish_spread_seconds']:.3f} s |",
            f"| Median tail over worker median | {loadscope['median_tail_over_median_seconds']:.3f} s | {worksteal['median_tail_over_median_seconds']:.3f} s |",
            f"| Maximum process-tree peak RSS | {loadscope['max_peak_rss_bytes']} B | {worksteal['max_peak_rss_bytes']} B |",
            "",
            f"Runner memory headroom: {memory['headroom_fraction']:.1%}.",
            f"Median session regression limit: {session['maximum_regression_fraction']:.1%}.",
            f"Source SHA: `{manifest['provenance']['source_sha']}`.",
            f"Wheel SHA-256: `{manifest['provenance']['wheel_sha256']}`.",
            "",
        ])
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as summary:
        summary.write("\n".join(lines))


def _parser() -> argparse.ArgumentParser:
    """Build the hosted-comparison command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("metrics", nargs=4, type=Path, metavar="METRICS")
    parser.add_argument("--runner-memory-bytes", required=True, type=int)
    parser.add_argument("--source-run-id", required=True, type=int)
    parser.add_argument("--source-sha", required=True)
    parser.add_argument("--expected-source-sha", required=True)
    parser.add_argument("--wheel", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--summary", type=Path)
    parser.add_argument(
        "--max-median-session-regression-fraction",
        type=float,
        default=DEFAULT_MAX_MEDIAN_SESSION_REGRESSION_FRACTION,
    )
    parser.add_argument(
        "--max-peak-rss-regression-fraction",
        type=float,
        default=DEFAULT_MAX_PEAK_RSS_REGRESSION_FRACTION,
    )
    parser.add_argument(
        "--min-memory-headroom-fraction",
        type=float,
        default=DEFAULT_MIN_MEMORY_HEADROOM_FRACTION,
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    """Run the fail-closed hosted ABBA comparison."""
    args = _parser().parse_args(argv)
    provenance = {
        "source_run_id": args.source_run_id,
        "source_sha": args.source_sha,
        "expected_source_sha": args.expected_source_sha,
        "wheel_artifact_name": LINUX_WHEEL_ARTIFACT,
        "wheel_sha256": "",
    }
    try:
        provenance["wheel_sha256"] = _wheel_sha256(args.wheel)
        manifest = compare_abba(
            args.metrics,
            runner_memory_bytes=args.runner_memory_bytes,
            expected_worker_count=MAX_WORKERS,
            max_median_session_regression_fraction=(
                args.max_median_session_regression_fraction
            ),
            max_peak_rss_regression_fraction=args.max_peak_rss_regression_fraction,
            min_memory_headroom_fraction=args.min_memory_headroom_fraction,
            provenance=provenance,
        )
        manifest["status"] = "passed"
        exit_code = 0
    except (ComparisonError, OSError) as exc:
        manifest = {
            "schema_version": 1,
            "status": "failed",
            "error": str(exc),
            "provenance": provenance,
        }
        exit_code = 1
    _write_manifest(args.output, manifest)
    if args.summary:
        _append_summary(args.summary, manifest)
    if exit_code:
        print(manifest["error"], file=sys.stderr)
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
