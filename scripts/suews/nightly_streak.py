#!/usr/bin/env python3
"""List the last N scheduled runs of a workflow with their failing job names.

The plan's verification criterion for making the nightly the scientific tier is
thirty consecutive green scheduled runs with the alert job silent. This reads
that streak in one command:

    python scripts/suews/nightly_streak.py

Green means the run concluded `success` and no gating job failed or was
cancelled. The recording jobs (tolerance spread, cost-marker drift) carry
`continue-on-error: true`, so they can fail without reddening the run and
without opening the nightly tracking issue; they are listed separately rather
than breaking the streak.
"""

from __future__ import annotations

import argparse
from fnmatch import fnmatchcase
import json
from pathlib import Path
import subprocess
import sys
from typing import Any

DEFAULT_REPO = "UMEP-dev/SUEWS"
DEFAULT_WORKFLOW = "build-publish_to_pypi.yml"
DEFAULT_LIMIT = 30

# Display-name patterns of the jobs that record rather than gate: they are
# `continue-on-error: true` in the workflow and feed neither report_scheduled_run
# nor pr-gate, so their failure is information, not a red nightly.
NON_GATING_JOB_PATTERNS = ("Tolerance spread*", "Cost-marker drift*")

BAD_CONCLUSIONS = ("failure", "cancelled", "timed_out")

# A whole-matrix failure lists dozens of near-identical job names; the first
# few plus a count say the same thing without burying the streak.
MAX_LISTED_FAILURES = 5


def _is_non_gating(name: str, patterns: tuple[str, ...]) -> bool:
    """Return whether a job display name is one of the recording jobs."""
    return any(fnmatchcase(name, pattern) for pattern in patterns)


def summarise_run(
    run: dict[str, Any],
    jobs: list[dict[str, Any]],
    non_gating_patterns: tuple[str, ...] = NON_GATING_JOB_PATTERNS,
) -> dict[str, Any]:
    """Reduce one run and its jobs to the fields the streak view needs."""
    gating_failures = []
    recording_failures = []
    for job in jobs:
        if job.get("conclusion") not in BAD_CONCLUSIONS:
            continue
        name = job.get("name", "")
        if _is_non_gating(name, non_gating_patterns):
            recording_failures.append(name)
        else:
            gating_failures.append(name)
    conclusion = run.get("conclusion") or run.get("status") or "unknown"
    return {
        "run_id": run.get("id"),
        "run_number": run.get("run_number"),
        "date": (run.get("created_at") or "")[:10],
        "head_sha": (run.get("head_sha") or "")[:9],
        "conclusion": conclusion,
        "url": run.get("html_url"),
        "gating_failures": sorted(set(gating_failures)),
        "recording_failures": sorted(set(recording_failures)),
        # The alert (report_scheduled_run) opens or keeps open the tracking
        # issue whenever one of the jobs it needs is not a success. A gating
        # failure anywhere in the run is the observable form of that.
        "alert_expected": conclusion != "success" or bool(gating_failures),
    }


def summarise_runs(
    runs: list[dict[str, Any]],
    jobs_by_run_id: dict[int, list[dict[str, Any]]],
    non_gating_patterns: tuple[str, ...] = NON_GATING_JOB_PATTERNS,
) -> dict[str, Any]:
    """Summarise runs newest first and count the leading green streak."""
    records = [
        summarise_run(run, jobs_by_run_id.get(run.get("id"), []), non_gating_patterns)
        for run in runs
    ]
    streak = 0
    for record in records:
        if record["alert_expected"]:
            break
        streak += 1
    return {
        "runs_examined": len(records),
        "green_streak": streak,
        "runs": records,
    }


def _gh_json(args: list[str]) -> Any:
    """Run a gh command and parse its JSON output."""
    completed = subprocess.run(
        args, capture_output=True, text=True, check=False, encoding="utf-8"
    )
    if completed.returncode != 0:
        message = completed.stderr.strip() or completed.stdout.strip()
        raise SystemExit(f"[X] {' '.join(args)} failed: {message}")
    return json.loads(completed.stdout)


def fetch(repo: str, workflow: str, limit: int) -> tuple[list, dict]:
    """Fetch the most recent scheduled runs and their jobs through gh."""
    payload = _gh_json(
        [
            "gh",
            "api",
            f"repos/{repo}/actions/workflows/{workflow}/runs"
            f"?event=schedule&per_page={limit}",
        ]
    )
    runs = payload.get("workflow_runs", [])[:limit]
    if limit > 100:
        raise SystemExit("--limit is capped at 100: the runs API returns one page of at most 100")
    jobs_by_run_id = {}
    for run in runs:
        jobs_payload = _gh_json(
            ["gh", "api", "--paginate", f"repos/{repo}/actions/runs/{run['id']}/jobs"]
        )
        jobs_by_run_id[run["id"]] = jobs_payload.get("jobs", [])
    return runs, jobs_by_run_id


def _failure_lines(names: list[str], label: str) -> list[str]:
    """Render at most MAX_LISTED_FAILURES job names, then a count of the rest."""
    lines = [f"         {label}: {name}" for name in names[:MAX_LISTED_FAILURES]]
    remainder = len(names) - MAX_LISTED_FAILURES
    if remainder > 0:
        lines.append(f"         ... and {remainder} more")
    return lines


def render(summary: dict[str, Any], workflow: str) -> str:
    """Render the streak view as plain ASCII text."""
    lines = [
        f"Last {summary['runs_examined']} scheduled runs of {workflow} (newest first)",
        "",
    ]
    for record in summary["runs"]:
        marker = "[OK]" if not record["alert_expected"] else "[X] "
        lines.append(
            f"  {marker} {record['date']}  {record['conclusion']:<12}"
            f"  #{record['run_number']}  {record['url']}"
        )
        for line in _failure_lines(record["gating_failures"], "failing"):
            lines.append(line)
        for line in _failure_lines(
            record["recording_failures"], "recording job failed (not gating)"
        ):
            lines.append(line)
    lines += [
        "",
        f"Green streak from the newest run: {summary['green_streak']}",
        "A run is green when it concluded success and no gating job failed;",
        "that is when the report_scheduled_run alert stays silent (a needed job",
        "that was skipped counts as green here, but its upstream failure reddens the",
        "run conclusion, so the two views agree in practice).",
    ]
    return "\n".join(lines)


def _parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--repo", default=DEFAULT_REPO)
    parser.add_argument("--workflow", default=DEFAULT_WORKFLOW)
    parser.add_argument(
        "--limit",
        type=int,
        default=DEFAULT_LIMIT,
        help="scheduled runs to examine, at most 100 (one page of the runs API)",
    )
    parser.add_argument(
        "--runs-json",
        type=Path,
        help="Read the runs payload from a file instead of calling gh",
    )
    parser.add_argument(
        "--jobs-json",
        type=Path,
        help="Read a {run_id: jobs payload} mapping from a file instead of calling gh",
    )
    parser.add_argument("--json", action="store_true", help="Print JSON, not text")
    return parser


def main() -> int:
    """Run the nightly streak CLI."""
    args = _parser().parse_args()
    if args.runs_json:
        runs = json.loads(args.runs_json.read_text(encoding="utf-8"))["workflow_runs"]
        runs = runs[: args.limit]
        raw = (
            json.loads(args.jobs_json.read_text(encoding="utf-8"))
            if args.jobs_json
            else {}
        )
        jobs_by_run_id = {
            int(key): value.get("jobs", value) for key, value in raw.items()
        }
    else:
        runs, jobs_by_run_id = fetch(args.repo, args.workflow, args.limit)

    summary = summarise_runs(runs, jobs_by_run_id)
    if args.json:
        print(json.dumps(summary, indent=2, sort_keys=True))
    else:
        print(render(summary, args.workflow))
    return 0


if __name__ == "__main__":
    sys.exit(main())
