"""Deliberately bypass the normal provenance gate to demonstrate the historical harm."""

from pathlib import Path

from .environment import execute
from .oracle import evaluate, snapshot


def replay_consumer(spec: dict, work: Path, env: dict, stale: bool) -> dict:
    """Assert a missing-path usage error or successful validation of a stale copy."""
    before = snapshot(work)
    process = execute(spec["command"], work, env)
    promises = (
        [
            {"kind": "json", "pointer": "/data/is_valid", "expected": True},
            {"kind": "json", "pointer": "/errors", "expected": []},
            {"kind": "unchanged", "path": "site.migrated.yml"},
        ]
        if stale
        else [
            {
                "kind": "diagnostic",
                "stream": "stderr",
                "pattern": r"(?i)(does not exist|not found)",
            },
            {
                "kind": "diagnostic",
                "stream": "stderr",
                "pattern": r"site\.migrated\.yml",
            },
            {"kind": "absent", "path": "site.migrated.yml"},
        ]
    )
    after = snapshot(work)
    checks = [evaluate(promise, before, after, work, process) for promise in promises]
    expected_exit = 0 if stale else 2
    return {
        "label": "diagnostic replay: provenance deliberately bypassed",
        "scenario": "stale-output" if stale else "clean-directory",
        "source_lines": spec["source_lines"],
        **process,
        "before": before,
        "after": after,
        "promises": checks,
        "passed": process["exit_status"] == expected_exit
        and all(p["passed"] for p in checks),
        "expected_exit_status": expected_exit,
    }


def assert_counterexample(record: dict, expected: dict) -> bool:
    """Require the specific creates failure and the named, unrun first consumer."""
    failed = [s for s in record["steps"] if s["status"] == "failed"]
    planted = record.get("planted_file")
    if planted and not all(
        step.get(when, {}).get(planted["path"], {}).get("sha256") == planted["sha256"]
        for step in failed
        for when in ("before", "after")
    ):
        return False
    return (
        len(failed) == 1
        and failed[0]["id"] == expected["step"]
        and failed[0]["exit_status"] == 0
        and any(
            not p["passed"] and p["expected"]["kind"] == expected["promise"]
            for p in failed[0]["promises"]
        )
        and all(
            p["passed"]
            for p in failed[0]["promises"]
            if p["expected"]["kind"] != expected["promise"]
        )
        and all(
            p["passed"]
            for p in record["path_outcomes"]
            if p["expected"]["kind"] != expected["promise"]
        )
        and record["first_unrun_consumer"] == expected["consumer"]
        and any(
            s["id"] == expected["consumer"] and s["status"] == "unrun"
            for s in record["steps"]
        )
    )


def assess_replay(
    record: dict, journey: dict, consumer: dict, work: Path, env: dict
) -> None:
    """Record acceptance of the diagnostic without turning the normal failure green."""
    record["expected_failure"] = journey["expected_failure"]
    record["counterexample_reproduced"] = assert_counterexample(
        record, journey["expected_failure"]
    )
    record["diagnostic_replay"] = replay_consumer(
        consumer, work, env, journey.get("plant_stale", False)
    )
    if planted := record.get("planted_file"):
        replay = record["diagnostic_replay"]
        replay["planted_file"] = planted
        replay["planted_bytes_validated"] = planted["path"] in replay["argv"] and all(
            replay[when].get(planted["path"], {}).get("sha256") == planted["sha256"]
            for when in ("before", "after")
        )
        replay["passed"] &= replay["planted_bytes_validated"]
    record["accepted"] = (
        record["counterexample_reproduced"] and record["diagnostic_replay"]["passed"]
    )
