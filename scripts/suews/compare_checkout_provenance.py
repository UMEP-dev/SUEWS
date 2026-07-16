#!/usr/bin/env python3
"""Collect and compare full/blob-filtered checkout provenance evidence."""

from __future__ import annotations

import argparse
from email.parser import BytesParser
from email.policy import default
import hashlib
import json
import math
from pathlib import Path
import re
from statistics import median
import subprocess
import sys
import time
from typing import Any
import zipfile

TRIAL_ORDER = (
    ("A1", "full"),
    ("B1", "blob:none"),
    ("B2", "blob:none"),
    ("A2", "full"),
)
GIT_EQUIVALENCE_FIELDS = (
    "commit_sha",
    "tree_sha",
    "git_describe",
    "derived_version",
    "generated_commit_hash",
    "generated_version_file_sha256",
    "tags_sha256",
)
WHEEL_EQUIVALENCE_FIELDS = (
    "metadata_sha256",
    "name",
    "version",
    "tags",
    "embedded_version_sha256",
)


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _git(repository: Path, *arguments: str) -> str:
    return subprocess.check_output(
        ["git", *arguments],
        cwd=repository,
        stderr=subprocess.STDOUT,
        text=True,
    ).strip()


def _write_json(path: Path, payload: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(path)


def collect_checkout_provenance(
    repository: Path,
    *,
    label: str,
    mode: str,
    expected_sha: str,
    started_at_epoch_ms: int,
) -> dict[str, Any]:
    """Collect provenance after checkout without including it in the timing."""
    completed_at_epoch_ms = time.time_ns() // 1_000_000
    repository = repository.resolve()
    if (label, mode) not in TRIAL_ORDER:
        raise ValueError(f"unsupported trial pair: {label}/{mode}")
    if re.fullmatch(r"[0-9a-f]{40}", expected_sha) is None:
        raise ValueError("expected_sha must be a lowercase 40-character hex digest")

    commit_sha = _git(repository, "rev-parse", "HEAD^{}")
    if commit_sha != expected_sha:
        raise ValueError(f"{label} checked out {commit_sha}, expected {expected_sha}")
    generated = subprocess.check_output(
        [sys.executable, "get_ver_git.py"],
        cwd=repository,
        stderr=subprocess.STDOUT,
        text=True,
    ).strip()
    version_file = repository / "src/supy/_version_scm.py"
    version_text = version_file.read_text(encoding="utf-8")
    commit_match = re.search(
        r"^__commit_hash__\s*=\s*commit_hash\s*=\s*['\"]([^'\"]+)",
        version_text,
        re.MULTILINE,
    )
    if commit_match is None:
        raise ValueError(f"{label} generated no readable commit hash")
    tags = _git(
        repository,
        "for-each-ref",
        "--format=%(refname)%00%(objectname)%00%(*objectname)",
        "refs/tags",
    ).encode("utf-8")
    partial_filter = subprocess.run(
        ["git", "config", "--get", "remote.origin.partialclonefilter"],
        cwd=repository,
        check=False,
        capture_output=True,
        text=True,
    ).stdout.strip()
    return {
        "schema_version": 1,
        "label": label,
        "mode": mode,
        "duration_seconds": round(
            (completed_at_epoch_ms - started_at_epoch_ms) / 1000.0, 6
        ),
        "started_at_epoch_ms": started_at_epoch_ms,
        "completed_at_epoch_ms": completed_at_epoch_ms,
        "commit_sha": commit_sha,
        "tree_sha": _git(repository, "rev-parse", "HEAD^{tree}"),
        "git_describe": _git(
            repository,
            "describe",
            "--tags",
            "--long",
            "--match=[0-9]*",
        ),
        "derived_version": generated.splitlines()[-1],
        "generated_commit_hash": commit_match.group(1),
        "generated_version_file_sha256": _sha256(version_text.encode("utf-8")),
        "tags_sha256": _sha256(tags),
        "partial_clone_filter": partial_filter or None,
    }


def read_wheel_provenance(path: Path) -> dict[str, Any]:
    """Read deterministic provenance fields from a built wheel."""
    path = path.resolve()
    if not path.is_file():
        raise ValueError(f"wheel not found: {path}")
    with zipfile.ZipFile(path) as archive:
        names = archive.namelist()
        metadata_names = [
            name for name in names if name.endswith(".dist-info/METADATA")
        ]
        wheel_names = [name for name in names if name.endswith(".dist-info/WHEEL")]
        version_names = [
            name for name in names if name.endswith("supy/_version_scm.py")
        ]
        if len(metadata_names) != 1 or len(wheel_names) != 1 or len(version_names) != 1:
            raise ValueError(
                "wheel must contain exactly one METADATA, WHEEL and "
                "supy/_version_scm.py file"
            )
        metadata_bytes = archive.read(metadata_names[0])
        wheel_bytes = archive.read(wheel_names[0])
        embedded_version = archive.read(version_names[0])

    metadata = BytesParser(policy=default).parsebytes(metadata_bytes)
    wheel_metadata = BytesParser(policy=default).parsebytes(wheel_bytes)
    return {
        "file": path.name,
        "file_sha256": _file_sha256(path),
        "metadata_sha256": _sha256(metadata_bytes),
        "name": metadata.get("Name"),
        "version": metadata.get("Version"),
        "tags": sorted(wheel_metadata.get_all("Tag", [])),
        "embedded_version_sha256": _sha256(embedded_version),
    }


def build_comparison(
    trials: list[dict[str, Any]],
    baseline_wheel: dict[str, Any],
    filtered_wheel: dict[str, Any],
    *,
    expected_sha: str,
    baseline_metrics: dict[str, Any] | None = None,
    filtered_metrics: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Validate the ABBA order and provenance equality, then reduce timings."""
    observed_order = [(trial.get("label"), trial.get("mode")) for trial in trials]
    if observed_order != list(TRIAL_ORDER):
        raise ValueError(
            f"trial order must be {list(TRIAL_ORDER)}, got {observed_order}"
        )
    if any(trial.get("commit_sha") != expected_sha for trial in trials):
        raise ValueError("one or more trials do not match expected_sha")
    if any(trial.get("generated_commit_hash") != expected_sha[:7] for trial in trials):
        raise ValueError(
            "one or more generated commit hashes do not match expected_sha"
        )
    for trial in trials:
        expected_filter = "blob:none" if trial["mode"] == "blob:none" else None
        if trial.get("partial_clone_filter") != expected_filter:
            raise ValueError(
                f"{trial['label']} did not enact {trial['mode']} checkout mode"
            )

    reference = trials[0]
    for field in GIT_EQUIVALENCE_FIELDS:
        values = {str(trial.get(field)) for trial in trials}
        if len(values) != 1:
            raise ValueError(
                f"checkout provenance mismatch for {field}: {sorted(values)}"
            )
        if reference.get(field) in {None, ""}:
            raise ValueError(f"checkout provenance field {field} is empty")

    wheel_mismatches = [
        field
        for field in WHEEL_EQUIVALENCE_FIELDS
        if baseline_wheel.get(field) != filtered_wheel.get(field)
        or baseline_wheel.get(field) in (None, "", [])
    ]
    if wheel_mismatches:
        raise ValueError("wheel provenance mismatch for " + ", ".join(wheel_mismatches))

    full = [
        float(trial["duration_seconds"]) for trial in trials if trial["mode"] == "full"
    ]
    filtered = [
        float(trial["duration_seconds"])
        for trial in trials
        if trial["mode"] == "blob:none"
    ]
    full_median = median(full)
    filtered_median = median(filtered)
    if full_median <= 0 or filtered_median <= 0:
        raise ValueError("checkout durations must be positive")
    for label, metrics in (
        ("baseline", baseline_metrics),
        ("blob_none", filtered_metrics),
    ):
        if metrics is not None:
            _validate_wheel_metrics(metrics, label=label)

    return {
        "schema_version": 1,
        "passed": True,
        "expected_sha": expected_sha,
        "checkout": {
            "order": [trial["label"] for trial in trials],
            "trials": trials,
            "full_median_seconds": round(full_median, 6),
            "blob_none_median_seconds": round(filtered_median, 6),
            "blob_none_change_percent": round(
                ((filtered_median / full_median) - 1.0) * 100.0, 6
            ),
            "performance_interpretation": (
                "same-runner observational evidence; not a general hosted-runner speed-up"
            ),
            "provenance_equivalent": True,
        },
        "wheel_provenance": {
            "equivalent": True,
            "compared_fields": list(WHEEL_EQUIVALENCE_FIELDS),
            "baseline": baseline_wheel,
            "blob_none": filtered_wheel,
        },
        "wheel_phase_metrics": {
            "baseline": baseline_metrics,
            "blob_none": filtered_metrics,
        },
    }


def _validate_wheel_metrics(metrics: dict[str, Any], *, label: str) -> None:
    """Require the existing machine-readable wheel phase evidence contract."""
    if (
        metrics.get("schema_version") != 1
        or metrics.get("kind") != "wheel-job-ci-metrics"
        or metrics.get("job_name") != "physics-cp312-win-AMD64"
    ):
        raise ValueError(f"{label} wheel metrics have the wrong identity")
    required = {"checkout", "toolchain_setup", "build", "repair", "install", "tests"}
    phases = metrics.get("phases")
    if not isinstance(phases, dict):
        raise ValueError(f"{label} wheel metrics have no phases mapping")
    missing = required.difference(phases)
    if missing:
        raise ValueError(f"{label} wheel metrics omit phases: {sorted(missing)}")
    for phase in required:
        record = phases[phase]
        duration = record.get("duration_seconds")
        if (
            record.get("available") is not True
            or record.get("status") != "measured"
            or not isinstance(duration, (int, float))
            or not math.isfinite(duration)
            or duration <= 0
        ):
            raise ValueError(f"{label} wheel phase {phase} is not measured")
    pytest_metrics = metrics.get("pytest_metrics") or {}
    result = pytest_metrics.get("result") or {}
    inventory = pytest_metrics.get("inventory") or {}
    if result.get("exit_code") != 0 or not inventory.get("node_id_sha256"):
        raise ValueError(f"{label} wheel pytest evidence did not pass")


def _append_summary(path: Path, comparison: dict[str, Any]) -> None:
    checkout = comparison["checkout"]
    wheel = comparison["wheel_provenance"]
    lines = [
        "## Windows checkout provenance ABBA",
        "",
        "| Mode | Median checkout |",
        "|---|---:|",
        f"| Full history | {checkout['full_median_seconds']:.3f} s |",
        f"| `blob:none` | {checkout['blob_none_median_seconds']:.3f} s |",
        "",
        f"Observed same-runner change: {checkout['blob_none_change_percent']:.2f}%.",
        "This is hosted-runner observational evidence, not a general speed-up claim.",
        "",
        f"Git provenance equivalent: **{str(checkout['provenance_equivalent']).lower()}**.",
        f"Wheel provenance equivalent: **{str(wheel['equivalent']).lower()}**.",
        "",
    ]
    with path.open("a", encoding="utf-8") as summary:
        summary.write("\n".join(lines))


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    collect = subparsers.add_parser("collect")
    collect.add_argument("--repository", type=Path, required=True)
    collect.add_argument("--label", required=True)
    collect.add_argument("--mode", required=True)
    collect.add_argument("--expected-sha", required=True)
    collect.add_argument("--started-at-epoch-ms", type=int, required=True)
    collect.add_argument("--output", type=Path, required=True)

    compare = subparsers.add_parser("compare")
    compare.add_argument("trials", nargs=4, type=Path)
    compare.add_argument("--expected-sha", required=True)
    compare.add_argument("--baseline-wheel", type=Path, required=True)
    compare.add_argument("--filtered-wheel", type=Path, required=True)
    compare.add_argument("--baseline-metrics", type=Path, required=True)
    compare.add_argument("--filtered-metrics", type=Path, required=True)
    compare.add_argument("--output", type=Path, required=True)
    compare.add_argument("--summary", type=Path)
    return parser


def main() -> int:
    """Run the requested evidence collection or comparison command."""
    args = _parser().parse_args()
    if args.command == "collect":
        payload = collect_checkout_provenance(
            args.repository,
            label=args.label,
            mode=args.mode,
            expected_sha=args.expected_sha,
            started_at_epoch_ms=args.started_at_epoch_ms,
        )
        _write_json(args.output, payload)
        return 0

    trials = [json.loads(path.read_text(encoding="utf-8")) for path in args.trials]
    baseline_metrics = json.loads(args.baseline_metrics.read_text(encoding="utf-8"))
    filtered_metrics = json.loads(args.filtered_metrics.read_text(encoding="utf-8"))
    try:
        comparison = build_comparison(
            trials,
            read_wheel_provenance(args.baseline_wheel),
            read_wheel_provenance(args.filtered_wheel),
            expected_sha=args.expected_sha,
            baseline_metrics=baseline_metrics,
            filtered_metrics=filtered_metrics,
        )
    except (ValueError, KeyError, TypeError) as error:
        _write_json(
            args.output,
            {"schema_version": 1, "passed": False, "error": str(error)},
        )
        raise SystemExit(str(error)) from error
    _write_json(args.output, comparison)
    if args.summary:
        _append_summary(args.summary, comparison)
    json.dump(comparison, sys.stdout, indent=2, sort_keys=True)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
