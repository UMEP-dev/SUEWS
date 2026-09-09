#!/usr/bin/env python3
"""Check the `medium` and `slow` cost markers against measured CPU seconds.

The cost markers select tests into CI tiers (`.claude/rules/tests/patterns.md`,
"Importance and cost"). They used to be assigned by wall-clock feel, and wall
time on a hosted runner sits on a 2x noise floor: the same 166-test physics tier
took 169 s and 343 s on the same runner class on the same day. The markers are
therefore defined by process CPU seconds on the Linux reference runner
(`ubuntu-latest`, cp312), which the metrics plugin
(`scripts/suews/pytest_ci_metrics.py`) records per test in the
`ci-metrics-api-cp312-manylinux-x86_64` and
`ci-metrics-physics-cp312-manylinux-x86_64` artefacts of every nightly run.

This script reads one or more of those artefacts (files, or directories as
`gh run download` leaves them) and reports every test whose marker disagrees
with its measured cost:

- no cost marker, but at or above the `medium` threshold (mark it `medium`,
  or `slow` when it also clears the `slow` threshold);
- marked `medium`, but at or above the `slow` threshold (mark it `slow`);
- marked `slow`, but under the `medium` threshold (drop the mark);
- marked `medium`, but under the `medium` threshold (drop the mark).

A node that appears in several artefacts (a file marked both `physics` and
`api` runs in both lanes) is judged on its largest measurement. The exit code
is 1 when any test is flagged, so a local run is useful on its own; the nightly
step that runs it carries `continue-on-error`, so drift is reported and never
reddens the run.

Usage::

    python scripts/lint/check_cost_markers.py ci-metrics/ [--markdown SUMMARY]
    python scripts/lint/check_cost_markers.py api.json physics-pytest.json --histogram
"""

from __future__ import annotations

import argparse
from collections.abc import Iterable, Iterator
from dataclasses import dataclass, field
import json
from pathlib import Path
import sys
from typing import Any

# Thresholds in process CPU seconds on the Linux reference runner. The
# reasoning and the distribution they were read from live in
# `.claude/rules/tests/patterns.md` ("Importance and cost").
MEDIUM_CPU_SECONDS = 10.0
SLOW_CPU_SECONDS = 60.0
# The phase whose CPU seconds define a test's cost. `call` is the test body;
# `total` adds fixture setup and teardown, which under xdist charges a shared
# session fixture to whichever test reaches it first on each worker.
DEFAULT_PHASE = "call"
PHASES = ("setup", "call", "teardown", "total")
COST_MARKERS = ("medium", "slow")
METRICS_SCHEMA_VERSION = 2
# Log-spaced bin edges for the histogram, in CPU seconds.
HISTOGRAM_EDGES = (0.1, 0.3, 1.0, 3.0, 10.0, 30.0, 60.0, 100.0, 300.0)


@dataclass(frozen=True)
class TestCost:
    """One test's measured cost in one artefact."""

    node_id: str
    markers: tuple[str, ...]
    cpu_seconds: float
    wall_seconds: float
    source: str

    @property
    def cost_marker(self) -> str | None:
        """The cost marker the test carries: `slow` wins over `medium`."""
        if "slow" in self.markers:
            return "slow"
        if "medium" in self.markers:
            return "medium"
        return None


@dataclass(frozen=True)
class Finding:
    """One marker that disagrees with the measurement."""

    kind: str
    test: TestCost
    wanted: str

    def line(self) -> str:
        current = self.test.cost_marker or "no cost marker"
        return (
            f"  - {self.test.node_id}: {self.test.cpu_seconds:.1f} CPU-s "
            f"({current} -> {self.wanted}; {self.test.source})"
        )


@dataclass
class Report:
    """Outcome of one check over a set of artefacts."""

    medium_seconds: float
    slow_seconds: float
    phase: str
    tests: list[TestCost] = field(default_factory=list)
    findings: list[Finding] = field(default_factory=list)
    sources: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return not self.findings

    def by_kind(self) -> dict[str, list[Finding]]:
        grouped: dict[str, list[Finding]] = {kind: [] for kind in FINDING_KINDS}
        for finding in self.findings:
            grouped[finding.kind].append(finding)
        return grouped

    def message(self) -> str:
        """Plain-text report: header, then each finding class with its nodes."""
        lines = [
            f"Cost markers against {self.phase}-phase CPU seconds "
            f"(medium >= {self.medium_seconds:g} s, slow >= {self.slow_seconds:g} s); "
            f"{len(self.tests)} tests from {len(self.sources)} artefact(s).",
        ]
        if self.ok:
            lines.append("[OK] every medium and slow marker agrees with its measured cost.")
            return "\n".join(lines)
        lines.append(f"[X] {len(self.findings)} marker(s) disagree with the measurement:")
        for kind, findings in self.by_kind().items():
            if not findings:
                continue
            lines.append(f"{FINDING_KINDS[kind]} ({len(findings)}):")
            lines.extend(finding.line() for finding in findings)
        lines.append(
            "Fix the marker, not the threshold: thresholds and their reasoning are in "
            ".claude/rules/tests/patterns.md."
        )
        return "\n".join(lines)

    def markdown(self) -> str:
        """Markdown for a GitHub step summary."""
        lines = [
            "## Cost-marker drift",
            "",
            f"{len(self.tests)} tests from {len(self.sources)} artefact(s); "
            f"cost = {self.phase}-phase CPU seconds; "
            f"`medium` >= {self.medium_seconds:g} s, `slow` >= {self.slow_seconds:g} s.",
            "",
        ]
        if self.ok:
            lines.append("No drift: every `medium` and `slow` marker agrees with its measured cost.")
        else:
            lines.extend([
                f"{len(self.findings)} marker(s) disagree with the measurement.",
                "",
                "| Test | CPU-s | Current | Wanted | Class | Source |",
                "|---|---:|---|---|---|---|",
            ])
            for finding in self.findings:
                test = finding.test
                lines.append(
                    f"| `{test.node_id}` | {test.cpu_seconds:.1f} | "
                    f"{test.cost_marker or '-'} | {finding.wanted} | "
                    f"{finding.kind} | {test.source} |"
                )
        lines.extend(["", "### Distribution", "", *histogram_lines(self.tests), ""])
        return "\n".join(lines)


FINDING_KINDS = {
    "unmarked-over-medium": "Unmarked tests at or above the medium threshold",
    "medium-over-slow": "Tests marked medium at or above the slow threshold",
    "slow-under-medium": "Tests marked slow under the medium threshold",
    "medium-under-medium": "Tests marked medium under the medium threshold",
}


def iter_artefact_paths(paths: Iterable[Path]) -> Iterator[Path]:
    """Yield JSON files from the given files and directories, sorted."""
    for path in paths:
        if path.is_dir():
            yield from sorted(candidate for candidate in path.rglob("*.json"))
        else:
            yield path


def _pytest_payload(payload: Any) -> dict[str, Any] | None:
    """Return the pytest metrics inside an artefact, or None if it is not one.

    The api lane uploads the plugin's JSON directly; the physics lane uploads
    it beside a `-phases.json` state file and embedded in `-wheel-job.json`
    under `pytest_metrics`. The embedded copy is skipped so a directory is not
    counted twice.
    """
    if not isinstance(payload, dict):
        return None
    if payload.get("kind") == "wheel-job-ci-metrics":
        return None
    if payload.get("schema_version") != METRICS_SCHEMA_VERSION:
        return None
    if not isinstance(payload.get("tests"), list):
        return None
    return payload


def load_tests(
    paths: Iterable[Path],
    *,
    phase: str = DEFAULT_PHASE,
    skipped: list[str] | None = None,
) -> tuple[list[TestCost], list[str]]:
    """Read per-test costs from every metrics artefact under `paths`.

    Returns the costs and the list of artefacts they came from. Files that are
    not schema-2 metrics with per-test records are listed in `skipped` when
    given, so a directory of mixed artefacts can be passed whole.
    """
    tests: list[TestCost] = []
    sources: list[str] = []
    for path in iter_artefact_paths(paths):
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            payload = None
        metrics = _pytest_payload(payload)
        if metrics is None:
            if skipped is not None:
                skipped.append(path.as_posix())
            continue
        sources.append(path.name)
        for record in metrics["tests"]:
            tests.append(
                TestCost(
                    node_id=str(record["node_id"]),
                    markers=tuple(sorted(str(m) for m in record.get("markers", []))),
                    cpu_seconds=float(record["cpu_seconds"][phase]),
                    wall_seconds=float(record["wall_seconds"][phase]),
                    source=path.name,
                )
            )
    return tests, sources


def largest_per_node(tests: Iterable[TestCost]) -> list[TestCost]:
    """Keep one measurement per node id: the largest CPU cost seen."""
    best: dict[str, TestCost] = {}
    for test in tests:
        current = best.get(test.node_id)
        if current is None or test.cpu_seconds > current.cpu_seconds:
            best[test.node_id] = test
    return [best[node_id] for node_id in sorted(best)]


def wanted_marker(cpu_seconds: float, medium_seconds: float, slow_seconds: float) -> str | None:
    """The cost marker a measurement calls for."""
    if cpu_seconds >= slow_seconds:
        return "slow"
    if cpu_seconds >= medium_seconds:
        return "medium"
    return None


def check_costs(
    tests: Iterable[TestCost],
    *,
    medium_seconds: float = MEDIUM_CPU_SECONDS,
    slow_seconds: float = SLOW_CPU_SECONDS,
    phase: str = DEFAULT_PHASE,
    sources: Iterable[str] = (),
) -> Report:
    """Compare every test's cost marker with the marker its cost calls for."""
    if not medium_seconds < slow_seconds:
        raise ValueError("the medium threshold must be below the slow threshold")
    report = Report(medium_seconds, slow_seconds, phase, sources=list(sources))
    report.tests = largest_per_node(tests)
    for test in report.tests:
        wanted = wanted_marker(test.cpu_seconds, medium_seconds, slow_seconds)
        current = test.cost_marker
        if current == wanted:
            continue
        if current is None:
            kind = "unmarked-over-medium"
        elif current == "medium" and wanted == "slow":
            kind = "medium-over-slow"
        elif current == "slow":
            # A slow mark over a medium-cost test is left alone: `slow` is also
            # the honest marker for a test unsuitable for routine PR runs for a
            # reason other than CPU (external data, a full-year run's memory).
            if wanted == "medium":
                continue
            kind = "slow-under-medium"
        else:
            kind = "medium-under-medium"
        report.findings.append(Finding(kind, test, wanted or "no cost marker"))
    return report


def histogram_lines(tests: Iterable[TestCost]) -> list[str]:
    """Render a log-binned count of tests by CPU seconds as a Markdown table."""
    costs = sorted(test.cpu_seconds for test in tests)
    edges = (0.0, *HISTOGRAM_EDGES, float("inf"))
    lines = ["| CPU-s | tests | cumulative |", "|---|---:|---:|"]
    cumulative = 0
    for lower, upper in zip(edges[:-1], edges[1:]):
        count = sum(1 for cost in costs if lower <= cost < upper)
        cumulative += count
        label = f">= {lower:g}" if upper == float("inf") else f"{lower:g} - {upper:g}"
        lines.append(f"| {label} | {count} | {cumulative} |")
    if costs:
        lines.append(
            f"| total {len(costs)}; median {costs[len(costs) // 2]:.2f}; "
            f"max {costs[-1]:.1f} | | |"
        )
    return lines


def _parse_args(argv: list[str] | None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "paths",
        nargs="+",
        type=Path,
        help="metrics JSON files or directories holding them",
    )
    parser.add_argument(
        "--medium-seconds",
        type=float,
        default=MEDIUM_CPU_SECONDS,
        help=f"CPU seconds from which a test is medium (default {MEDIUM_CPU_SECONDS:g})",
    )
    parser.add_argument(
        "--slow-seconds",
        type=float,
        default=SLOW_CPU_SECONDS,
        help=f"CPU seconds from which a test is slow (default {SLOW_CPU_SECONDS:g})",
    )
    parser.add_argument(
        "--phase",
        choices=PHASES,
        default=DEFAULT_PHASE,
        help=f"which phase's CPU seconds define the cost (default {DEFAULT_PHASE})",
    )
    parser.add_argument(
        "--markdown",
        type=Path,
        help="append a Markdown report (findings and histogram) to this file",
    )
    parser.add_argument(
        "--histogram",
        action="store_true",
        help="also print the CPU-seconds histogram",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(argv)
    skipped: list[str] = []
    tests, sources = load_tests(args.paths, phase=args.phase, skipped=skipped)
    for path in skipped:
        print(f"[skip] {path}: not a metrics artefact with per-test records")
    if not tests:
        print("[X] no per-test records found; pass ci-metrics-*-cp312-manylinux-x86_64 artefacts")
        return 2
    report = check_costs(
        tests,
        medium_seconds=args.medium_seconds,
        slow_seconds=args.slow_seconds,
        phase=args.phase,
        sources=sources,
    )
    print(report.message())
    if args.histogram:
        print()
        print("\n".join(histogram_lines(report.tests)))
    if args.markdown:
        args.markdown.parent.mkdir(parents=True, exist_ok=True)
        with args.markdown.open("a", encoding="utf-8") as handle:
            handle.write(report.markdown())
    return 0 if report.ok else 1


if __name__ == "__main__":
    sys.exit(main())
