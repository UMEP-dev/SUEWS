#!/usr/bin/env python3
"""Build a queue, execution and dependency-path view from Actions Jobs REST data."""

from __future__ import annotations

import argparse
from datetime import datetime
from fnmatch import fnmatchcase
import json
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
DEFAULT_EXCLUDED_JOB_PATTERNS = ("CI observability summary",)


def _timestamp(value: str) -> datetime:
    """Parse a GitHub UTC timestamp."""
    return datetime.fromisoformat(value.replace("Z", "+00:00"))


def _seconds(start: datetime, end: datetime) -> float:
    """Return a non-negative duration rounded for stable JSON."""
    return round(max(0.0, (end - start).total_seconds()), 6)


def _matching_jobs(
    jobs: list[dict[str, Any]],
    pattern: str,
) -> list[dict[str, Any]]:
    """Resolve one declared-needs display-name pattern, including matrix children."""
    return [job for job in jobs if fnmatchcase(job["name"], pattern)]


def _dependency_patterns(
    job_name: str,
    declared_needs: dict[str, list[str]],
) -> list[str]:
    """Return all dependency patterns declared for a matching job pattern."""
    patterns = []
    for target_pattern, dependencies in declared_needs.items():
        if fnmatchcase(job_name, target_pattern):
            patterns.extend(dependencies)
    return patterns


def _prepare_jobs(
    jobs_payload: dict[str, Any],
    excluded_job_patterns: tuple[str, ...],
) -> list[dict[str, Any]]:
    """Filter completed non-summary jobs and parse their timestamps."""
    jobs = []
    for raw_job in jobs_payload.get("jobs", []):
        if any(
            fnmatchcase(raw_job["name"], pattern) for pattern in excluded_job_patterns
        ):
            continue
        if (
            raw_job.get("status") != "completed"
            or raw_job.get("conclusion") == "skipped"
        ):
            continue
        if not all(
            raw_job.get(field) for field in ("created_at", "started_at", "completed_at")
        ):
            continue
        job = dict(raw_job)
        job["_created"] = _timestamp(raw_job["created_at"])
        job["_started"] = _timestamp(raw_job["started_at"])
        job["_completed"] = _timestamp(raw_job["completed_at"])
        jobs.append(job)
    return jobs


def _resolve_dependencies(
    jobs: list[dict[str, Any]],
    declared_needs: dict[str, list[str]],
) -> dict[str, list[str]]:
    """Expand declared display-name patterns, including every matrix child."""
    dependencies = {}
    for job in jobs:
        resolved = []
        for pattern in _dependency_patterns(job["name"], declared_needs):
            resolved.extend(
                candidate["name"] for candidate in _matching_jobs(jobs, pattern)
            )
        dependencies[job["name"]] = sorted(set(resolved) - {job["name"]})
    return dependencies


def _step_records(job: dict[str, Any]) -> list[dict[str, Any]]:
    """Reduce Actions step timestamps to named execution durations."""
    records = []
    for step in job.get("steps") or []:
        if not step.get("started_at") or not step.get("completed_at"):
            continue
        records.append({
            "duration_seconds": _seconds(
                _timestamp(step["started_at"]),
                _timestamp(step["completed_at"]),
            ),
            "name": step["name"],
        })
    return records


def _job_record(
    job: dict[str, Any],
    dependencies: list[str],
    jobs_by_name: dict[str, dict[str, Any]],
    run_created: datetime,
) -> dict[str, Any]:
    """Build one queue/execution/fan-in record."""
    dependency_jobs = [
        jobs_by_name[name] for name in dependencies if name in jobs_by_name
    ]
    barrier = max(
        (dependency["_completed"] for dependency in dependency_jobs),
        default=run_created,
    )
    earliest_dependency = min(
        (dependency["_completed"] for dependency in dependency_jobs),
        default=barrier,
    )
    critical_predecessor = (
        max(dependency_jobs, key=lambda dependency: dependency["_completed"])["name"]
        if dependency_jobs
        else None
    )
    return {
        "completed_at": job["completed_at"],
        "conclusion": job.get("conclusion"),
        "created_at": job["created_at"],
        "critical_predecessor": critical_predecessor,
        "declared_dependencies": dependencies,
        "execution_seconds": _seconds(job["_started"], job["_completed"]),
        "fan_in_spread_seconds": _seconds(earliest_dependency, barrier),
        "name": job["name"],
        "orchestration_delay_seconds": _seconds(barrier, job["_created"]),
        "ready_offset_seconds": _seconds(run_created, barrier),
        "runner_queue_seconds": _seconds(job["_created"], job["_started"]),
        "started_at": job["started_at"],
        "steps": _step_records(job),
    }


def _trace_critical_path(
    records_by_name: dict[str, dict[str, Any]],
    target_name: str,
) -> tuple[float, list[str]]:
    """Follow the latest-finishing predecessor through the declared DAG."""
    memo: dict[str, tuple[float, list[str]]] = {}

    def visit(job_name: str, visiting: set[str]) -> tuple[float, list[str]]:
        if job_name in memo:
            return memo[job_name]
        if job_name in visiting:
            raise ValueError(f"Cycle in declared needs at {job_name}")
        visiting.add(job_name)
        record = records_by_name[job_name]
        own_duration = sum(
            record[field]
            for field in (
                "orchestration_delay_seconds",
                "runner_queue_seconds",
                "execution_seconds",
            )
        )
        parent = record["critical_predecessor"]
        if parent in records_by_name:
            parent_duration, parent_path = visit(parent, visiting)
        else:
            parent_duration, parent_path = 0.0, []
        visiting.remove(job_name)
        result = round(parent_duration + own_duration, 6), [*parent_path, job_name]
        memo[job_name] = result
        return result

    return visit(target_name, set())


def analyse_run(
    run: dict[str, Any],
    jobs_payload: dict[str, Any],
    *,
    declared_needs: dict[str, list[str]] | None = None,
    excluded_job_patterns: tuple[str, ...] = DEFAULT_EXCLUDED_JOB_PATTERNS,
    target_job_pattern: str = "PR build validation",
) -> dict[str, Any]:
    """Separate dependency barriers, runner queues and execution on a declared DAG."""
    run_created = _timestamp(run["created_at"])
    jobs = _prepare_jobs(jobs_payload, excluded_job_patterns)
    dependencies = _resolve_dependencies(jobs, declared_needs or {})
    jobs_by_name = {job["name"]: job for job in jobs}
    records_by_name = {
        job["name"]: _job_record(
            job,
            dependencies[job["name"]],
            jobs_by_name,
            run_created,
        )
        for job in jobs
    }

    target_jobs = [job for job in jobs if fnmatchcase(job["name"], target_job_pattern)]
    if len(target_jobs) != 1:
        raise ValueError(
            f"Expected one completed target matching {target_job_pattern!r}; "
            f"found {len(target_jobs)}."
        )
    gate_job = target_jobs[0]
    path_duration, path_names = _trace_critical_path(records_by_name, gate_job["name"])
    path_records = [records_by_name[name] for name in path_names]

    return {
        "schema_version": SCHEMA_VERSION,
        "workflow": {
            "event": run.get("event"),
            "gate_completed_at": gate_job["completed_at"],
            "head_sha": run.get("head_sha"),
            "id": run.get("id"),
            "created_at": run["created_at"],
            "elapsed_seconds": _seconds(run_created, gate_job["_completed"]),
        },
        "observed_critical_path": {
            "max_fan_in_spread_seconds": round(
                max(
                    (record["fan_in_spread_seconds"] for record in path_records),
                    default=0.0,
                ),
                6,
            ),
            "duration_seconds": path_duration,
            "execution_seconds": round(
                sum(record["execution_seconds"] for record in path_records),
                6,
            ),
            "job_names": path_names,
            "orchestration_delay_seconds": round(
                sum(record["orchestration_delay_seconds"] for record in path_records),
                6,
            ),
            "runner_queue_seconds": round(
                sum(record["runner_queue_seconds"] for record in path_records),
                6,
            ),
            "target_job": path_names[-1] if path_names else None,
        },
        "jobs": sorted(
            records_by_name.values(), key=lambda record: record["created_at"]
        ),
    }


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    """Write JSON atomically."""
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(path)


def _append_summary(path: Path, metrics: dict[str, Any]) -> None:
    """Append a concise workflow critical-path view to a step summary."""
    critical = metrics["observed_critical_path"]
    lines = [
        "## Workflow CI observability",
        "",
        f"Observed declared-needs path: `{' -> '.join(critical['job_names']) or 'none'}`",
        "",
        "| Critical-path component | Duration |",
        "|---|---:|",
        f"| Orchestration hand-off | {critical['orchestration_delay_seconds']:.1f} s |",
        f"| Runner queue | {critical['runner_queue_seconds']:.1f} s |",
        f"| Job execution | {critical['execution_seconds']:.1f} s |",
        f"| Total | {critical['duration_seconds']:.1f} s |",
        "",
        f"Maximum dependency fan-in spread on this path: {critical['max_fan_in_spread_seconds']:.1f} s.",
        "",
        "Queue is `created_at -> started_at`; execution is `started_at -> completed_at`.",
        "The summary job itself is excluded from the gate path.",
        "",
    ]
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as summary:
        summary.write("\n".join(lines))


def _parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--run-json", type=Path, required=True)
    parser.add_argument("--jobs-json", type=Path, required=True)
    parser.add_argument("--needs-json", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--summary", type=Path)
    parser.add_argument("--target-job", default="PR build validation")
    return parser


def main() -> int:
    """Run the workflow analysis CLI."""
    args = _parser().parse_args()
    run = json.loads(args.run_json.read_text(encoding="utf-8"))
    jobs = json.loads(args.jobs_json.read_text(encoding="utf-8"))
    needs = json.loads(args.needs_json.read_text(encoding="utf-8"))
    metrics = analyse_run(
        run,
        jobs,
        declared_needs=needs,
        target_job_pattern=args.target_job,
    )
    _write_json(args.output, metrics)
    if args.summary:
        _append_summary(args.summary, metrics)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
