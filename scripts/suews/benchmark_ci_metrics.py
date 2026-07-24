#!/usr/bin/env python3
"""Run a same-job metrics-off/on/on/off pytest overhead comparison."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
from statistics import median
import subprocess
import sys
from typing import Any

PROJECT_ROOT = Path(__file__).resolve().parents[2]


def compute_overhead(
    baseline_seconds: list[float],
    instrumented_seconds: list[float],
    threshold_percent: float,
) -> dict[str, Any]:
    """Reduce paired durations to a median percentage overhead decision."""
    if not baseline_seconds or not instrumented_seconds:
        raise ValueError("Both baseline and instrumented trials are required.")
    baseline_median = median(baseline_seconds)
    instrumented_median = median(instrumented_seconds)
    if baseline_median <= 0:
        raise ValueError("Baseline median must be positive.")
    overhead = ((instrumented_median / baseline_median) - 1.0) * 100.0
    return {
        "baseline_median_seconds": round(baseline_median, 6),
        "instrumented_median_seconds": round(instrumented_median, 6),
        "overhead_percent": round(overhead, 6),
        "passed": overhead <= threshold_percent,
        "threshold_percent": threshold_percent,
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


def file_sha256(path: Path) -> str:
    """Hash the exact wheel installed for the paired run."""
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _run_trial(
    command: list[str],
    output_dir: Path,
    mode: str,
    suffix: str,
) -> dict[str, Any]:
    """Run one isolated control or instrumented pytest process."""
    output_path = output_dir / f"metrics-{mode}-{suffix}.json"
    env = os.environ.copy()
    env.pop("GITHUB_STEP_SUMMARY", None)
    env["PYTHONPATH"] = os.pathsep.join(
        part for part in (str(PROJECT_ROOT), env.get("PYTHONPATH", "")) if part
    )
    if mode == "on":
        env["SUEWS_CI_METRICS"] = str(output_path)
        env.pop("SUEWS_CI_PHASE_TIMER", None)
        plugin = "scripts.suews.pytest_ci_metrics"
    else:
        env["SUEWS_CI_PHASE_TIMER"] = str(output_path)
        env.pop("SUEWS_CI_METRICS", None)
        plugin = "scripts.suews.pytest_phase_timer"
    basetemp = output_dir / f"basetemp-{mode}-{suffix}"
    completed = subprocess.run(
        [
            *command,
            "--basetemp",
            str(basetemp),
            "-p",
            "no:cacheprovider",
            "-p",
            plugin,
        ],
        cwd=PROJECT_ROOT,
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )
    (output_dir / f"metrics-{mode}-{suffix}.stdout.txt").write_text(
        completed.stdout,
        encoding="utf-8",
    )
    (output_dir / f"metrics-{mode}-{suffix}.stderr.txt").write_text(
        completed.stderr,
        encoding="utf-8",
    )
    if not output_path.exists():
        raise RuntimeError(
            f"{mode}-{suffix} produced no metrics (pytest exit {completed.returncode})"
        )
    payload = json.loads(output_path.read_text(encoding="utf-8"))
    if completed.returncode != 0 or payload["result"]["exit_code"] != 0:
        raise RuntimeError(f"{mode}-{suffix} pytest failed; inspect captured logs")
    return {
        "file": output_path.name,
        "inventory": payload["inventory"],
        "mode": mode,
        "tests_seconds": payload["phases"]["tests"]["duration_seconds"],
    }


def _option_value(command: list[str], option: str) -> str | None:
    """Read either ``--option value`` or ``--option=value`` from a command."""
    for index, argument in enumerate(command):
        if argument == option and index + 1 < len(command):
            return command[index + 1]
        if argument.startswith(option + "="):
            return argument.split("=", 1)[1]
        if option == "-n" and argument.startswith("-n") and argument != "-n":
            return argument[2:]
    return None


def validate_command(command: list[str], worker_count: int, scheduler: str) -> None:
    """Require the command to enact the declared fixed scheduler contract."""
    if not 1 <= worker_count <= 4:
        raise ValueError("The controlled harness requires between 1 and 4 workers.")
    actual_workers = _option_value(command, "-n")
    actual_cap = _option_value(command, "--maxprocesses")
    actual_scheduler = _option_value(command, "--dist")
    if actual_workers != str(worker_count):
        raise ValueError("The pytest -n value must equal --worker-count.")
    if actual_cap != str(worker_count):
        raise ValueError("pytest --maxprocesses must equal --worker-count.")
    if actual_scheduler != scheduler:
        raise ValueError("The pytest --dist value must equal --scheduler.")


def _parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--threshold-percent", type=float, default=2.0)
    parser.add_argument("--worker-count", type=int, required=True)
    parser.add_argument("--scheduler", required=True)
    parser.add_argument("--source-sha", required=True)
    parser.add_argument("--wheel", type=Path, required=True)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    return parser


def main() -> int:
    """Execute four alternating trials and write the comparison manifest."""
    args = _parser().parse_args()
    command = list(args.command)
    if command and command[0] == "--":
        command = command[1:]
    if not command:
        raise SystemExit("A pytest command is required after --.")
    if args.threshold_percent < 0:
        raise SystemExit("--threshold-percent must be non-negative.")
    if not args.wheel.is_file():
        raise SystemExit(f"Wheel not found: {args.wheel}")
    try:
        validate_command(command, args.worker_count, args.scheduler)
    except ValueError as error:
        raise SystemExit(str(error)) from error
    args.output_dir.mkdir(parents=True, exist_ok=True)

    order = (("off", "a"), ("on", "a"), ("on", "b"), ("off", "b"))
    runs = [
        _run_trial(command, args.output_dir, mode, suffix) for mode, suffix in order
    ]
    inventories = {
        (run["inventory"]["node_count"], run["inventory"]["node_id_sha256"])
        for run in runs
    }
    if len(inventories) != 1:
        raise RuntimeError("Overhead trials collected different test inventories.")
    comparison = compute_overhead(
        [run["tests_seconds"] for run in runs if run["mode"] == "off"],
        [run["tests_seconds"] for run in runs if run["mode"] == "on"],
        args.threshold_percent,
    )
    manifest = {
        "schema_version": 1,
        "command": command,
        "comparison": comparison,
        "inventory": runs[0]["inventory"],
        "order": [f"{mode}-{suffix}" for mode, suffix in order],
        "provenance": {
            "source_sha": args.source_sha,
            "wheel_name": args.wheel.name,
            "wheel_sha256": file_sha256(args.wheel),
        },
        "runs": runs,
        "scheduler": {
            "distribution": args.scheduler,
            "worker_count": args.worker_count,
        },
    }
    _write_json(args.output_dir / "comparison-manifest.json", manifest)
    json.dump(manifest, sys.stdout, indent=2, sort_keys=True)
    sys.stdout.write("\n")
    return 0 if comparison["passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
