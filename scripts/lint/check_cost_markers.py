#!/usr/bin/env python3
"""Check the `medium` and `slow` cost markers against measured CPU seconds.

The cost markers select tests into CI tiers (`.claude/rules/tests/patterns.md`,
"Importance and cost"). They used to be assigned by wall-clock feel, and wall
time on a hosted runner sits on a 2x noise floor: the same 166-test physics tier
took 169 s and 343 s on the same runner class on the same day. The markers are
therefore defined by process CPU seconds of the test body (the `call` phase)
on the Linux reference runner (`ubuntu-latest`, cp312), which the metrics
plugin (`scripts/suews/pytest_ci_metrics.py`) records per test in the
`ci-metrics-api-cp312-manylinux-x86_64` and
`ci-metrics-physics-cp312-manylinux-x86_64` artefacts of every nightly run.

This script reads one or more of those artefacts (files, or directories as
`gh run download` leaves them) and reports every test whose marker disagrees
with its measured cost:

- `unmarked-over-medium`: no cost marker, but clearly at or above the `medium`
  threshold (mark it `medium`, or `slow` when it also clearly clears `slow`);
- `medium-over-slow`: marked `medium`, but clearly at or above the `slow`
  threshold;
- `medium-under-medium`: marked `medium`, but clearly under the `medium`
  threshold;
- `slow-without-reason`: marked `slow` with a bare marker, but clearly under the
  `slow` threshold. `slow` also means "unsuitable for routine PR runs" for a
  cause other than CPU (network, credentials, a run-count policy, a platform
  that is far slower than the reference); such a test states the cause as
  `pytest.mark.slow(reason="...")`, the plugin records it, and this check
  accepts it. A bare `slow` is a CPU claim and is checked as one.

"Clearly" is the hysteresis band. Per-test CPU seconds on hosted runners vary
by up to about 1.8x between dispatches (the runners are not one hardware
generation, and on the four-worker physics lane a change elsewhere in the tree
moves what the workers contend for and with it every test's CPU time), so a
marker is questioned only when the reading is on the wrong side of a threshold
by more than the band factor: unmarked from `medium * band`, `medium` under
`medium / band` or from `slow * band`, bare `slow` under `slow / band`. Inside
a band either marking is accepted, so a test near a threshold does not flap
between two nights' readings.

A node that appears in several artefacts (a file marked both `physics` and
`api` runs in both lanes) is judged on its largest measurement. Nodes that did
not run their body (skipped, xfailed) are excluded by outcome and not judged.
The exit code is 1 when any test is flagged, so a local run is useful on its
own; the nightly step that runs it carries `continue-on-error`, so drift is
reported and never reddens the run.

`--test-file` restricts the check to the tests in the named test files (the
part of a node id before `::`), which is how a pull-request run judges only the
test files that pull request adds or changes. `--json` writes the outcome as
JSON (counts and one record per flagged test) for a workflow step to read.

Usage::

    python scripts/lint/check_cost_markers.py ci-metrics/ [--markdown SUMMARY]
    python scripts/lint/check_cost_markers.py api.json physics-pytest.json --histogram
    python scripts/lint/check_cost_markers.py ci-metrics/ --test-file test/core/test_x.py --json out.json
"""

from __future__ import annotations

import argparse
from collections.abc import Iterable, Iterator
from dataclasses import dataclass, field
import json
from pathlib import Path
import sys
from typing import Any

# Thresholds in process CPU seconds of the test body on the Linux reference
# runner. The distribution they were read from and the reasoning live in
# `.claude/rules/tests/patterns.md` ("Cost thresholds").
MEDIUM_CPU_SECONDS = 10.0
SLOW_CPU_SECONDS = 30.0
# Hysteresis: a marker is questioned only when the reading is on the wrong side
# of a threshold by more than this factor (see the module docstring).
BAND = 2.0
# The phase whose CPU seconds define a test's cost. `call` is the test body;
# `total` adds fixture setup and teardown, which under xdist charges a shared
# session fixture to whichever test reaches it first on each worker.
DEFAULT_PHASE = "call"
PHASES = ("setup", "call", "teardown", "total")
COST_MARKERS = ("medium", "slow")
# Outcomes whose call phase ran and so carry a measurement.
MEASURED_OUTCOMES = frozenset({"passed", "failed", "xpassed"})
METRICS_SCHEMA_VERSION = 2
# Log-spaced bin edges for the histogram, in CPU seconds.
HISTOGRAM_EDGES = (0.1, 0.3, 1.0, 3.0, 10.0, 30.0, 60.0, 100.0, 300.0)

FINDING_KINDS = {
    "unmarked-over-medium": "Unmarked tests clearly at or above the medium threshold",
    "medium-over-slow": "Tests marked medium clearly at or above the slow threshold",
    "medium-under-medium": "Tests marked medium clearly under the medium threshold",
    "slow-without-reason": "Tests marked slow clearly under the slow threshold without a stated reason",
}


@dataclass(frozen=True)
class TestCost:
    """One test's measured cost in one artefact."""

    node_id: str
    markers: tuple[str, ...]
    cpu_seconds: float
    wall_seconds: float
    source: str
    outcome: str = "passed"
    marker_reasons: tuple[tuple[str, str], ...] = ()

    @property
    def cost_marker(self) -> str | None:
        """The cost marker the test carries: `slow` wins over `medium`."""
        if "slow" in self.markers:
            return "slow"
        if "medium" in self.markers:
            return "medium"
        return None

    @property
    def slow_reason(self) -> str | None:
        return dict(self.marker_reasons).get("slow")

    @property
    def measured(self) -> bool:
        return self.outcome in MEASURED_OUTCOMES


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
    band: float
    phase: str
    tests: list[TestCost] = field(default_factory=list)
    findings: list[Finding] = field(default_factory=list)
    sources: list[str] = field(default_factory=list)
    unmeasured: int = 0
    # The test files the check was restricted to, or None for every test.
    restricted_to: tuple[str, ...] | None = None

    @property
    def ok(self) -> bool:
        return not self.findings

    def by_kind(self) -> dict[str, list[Finding]]:
        grouped: dict[str, list[Finding]] = {kind: [] for kind in FINDING_KINDS}
        for finding in self.findings:
            grouped[finding.kind].append(finding)
        return grouped

    def _scope(self) -> str:
        restriction = ""
        if self.restricted_to is not None:
            restriction = f" in {len(self.restricted_to)} given test file(s)"
        return (
            f"{len(self.tests)} measured tests{restriction} ({self.unmeasured} skipped or "
            f"xfailed not judged) from {len(self.sources)} artefact(s); cost = {self.phase}-phase "
            f"CPU seconds; medium >= {self.medium_seconds:g} s, slow >= "
            f"{self.slow_seconds:g} s, band {self.band:g}."
        )

    def to_dict(self) -> dict[str, Any]:
        """Return the outcome as plain data, for `--json`."""
        return {
            "ok": self.ok,
            "flagged": len(self.findings),
            "measured": len(self.tests),
            "unmeasured": self.unmeasured,
            "restricted_to": (
                None if self.restricted_to is None else list(self.restricted_to)
            ),
            "phase": self.phase,
            "medium_seconds": self.medium_seconds,
            "slow_seconds": self.slow_seconds,
            "band": self.band,
            "sources": list(self.sources),
            "findings": [
                {
                    "node_id": finding.test.node_id,
                    "kind": finding.kind,
                    "cpu_seconds": finding.test.cpu_seconds,
                    "current": finding.test.cost_marker,
                    "wanted": finding.wanted,
                    "source": finding.test.source,
                }
                for finding in self.findings
            ],
        }

    def message(self) -> str:
        """Plain-text report: header, then each finding class with its nodes."""
        lines = [f"Cost markers against measured CPU seconds: {self._scope()}"]
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
            ".claude/rules/tests/patterns.md. A slow test that is slow for a reason "
            "other than CPU states it as pytest.mark.slow(reason=...)."
        )
        return "\n".join(lines)

    def markdown(self) -> str:
        """Markdown for a GitHub step summary."""
        lines = ["## Cost-marker drift", "", self._scope(), ""]
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


def iter_artefact_paths(paths: Iterable[Path]) -> Iterator[Path]:
    """Yield JSON files from the given files and directories, sorted."""
    for path in paths:
        if path.is_dir():
            yield from sorted(candidate for candidate in path.rglob("*.json"))
        else:
            yield path


class UnsupportedMetricsSchema(ValueError):
    """An artefact carries per-test records under a schema this lint does not read."""


def _sanitise_diagnostic(text: str) -> str:
    """Keep diagnostic fragments on one printable ASCII line without leading `::`."""
    cleaned = "".join(ch if 0x20 <= ord(ch) <= 0x7E else " " for ch in str(text))
    stripped = cleaned.lstrip()
    if stripped.startswith("::"):
        prefix_len = len(cleaned) - len(stripped)
        cleaned = cleaned[:prefix_len] + ": :" + stripped[2:]
    return cleaned


def _pytest_payload(payload: Any, path: Path) -> dict[str, Any] | None:
    """Return the pytest metrics inside an artefact, or None if it is not one.

    The api lane uploads the plugin's JSON directly; the physics lane uploads
    it beside a `-phases.json` state file and embedded in `-wheel-job.json`
    under `pytest_metrics`. The embedded copy is skipped so a directory is not
    counted twice.

    An artefact that carries per-test records under another schema version is
    an error, not a skip: silently skipping it would judge the remaining lanes
    and report them clean, so a schema bump could read as no drift.
    """
    if not isinstance(payload, dict):
        return None
    if payload.get("kind") == "wheel-job-ci-metrics":
        return None
    if not isinstance(payload.get("tests"), list):
        return None
    version = payload.get("schema_version")
    if version != METRICS_SCHEMA_VERSION:
        safe_path = _sanitise_diagnostic(path.as_posix())
        safe_version = _sanitise_diagnostic(repr(version))
        raise UnsupportedMetricsSchema(
            f"{safe_path}: metrics schema {safe_version} not supported by this lint "
            f"(expects {METRICS_SCHEMA_VERSION}); update METRICS_SCHEMA_VERSION and the "
            "record reader together"
        )
    return payload


def load_tests(
    paths: Iterable[Path],
    *,
    phase: str = DEFAULT_PHASE,
    skipped: list[str] | None = None,
) -> tuple[list[TestCost], list[str]]:
    """Read per-test costs from every metrics artefact under `paths`.

    Returns the costs and the list of artefacts they came from. Files that are
    not metrics artefacts with per-test records are listed in `skipped` when
    given, so a directory of mixed artefacts can be passed whole. An artefact
    with per-test records under a schema version other than
    `METRICS_SCHEMA_VERSION` raises `UnsupportedMetricsSchema`.
    """
    tests: list[TestCost] = []
    sources: list[str] = []
    for path in iter_artefact_paths(paths):
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            payload = None
        metrics = _pytest_payload(payload, path)
        if metrics is None:
            if skipped is not None:
                skipped.append(path.as_posix())
            continue
        sources.append(path.name)
        for record in metrics["tests"]:
            reasons = record.get("marker_reasons") or {}
            tests.append(
                TestCost(
                    node_id=str(record["node_id"]),
                    markers=tuple(sorted(str(m) for m in record.get("markers", []))),
                    cpu_seconds=float(record["cpu_seconds"][phase]),
                    wall_seconds=float(record["wall_seconds"][phase]),
                    source=path.name,
                    outcome=str(record.get("outcome")),
                    marker_reasons=tuple(sorted((str(k), str(v)) for k, v in reasons.items())),
                )
            )
    return tests, sources


def _posix_path(path: str) -> str:
    """Return a repository-relative path in the form pytest writes into a node id."""
    path = path.replace("\\", "/")
    while path.startswith("./"):
        path = path[2:]
    return path


def node_file(node_id: str) -> str:
    """Return the test file a node id belongs to: everything before the first `::`."""
    return _posix_path(node_id.split("::", 1)[0])


def restrict_to_files(tests: Iterable[TestCost], test_files: Iterable[str]) -> list[TestCost]:
    """Keep the tests whose node id lies in one of `test_files`."""
    wanted = {_posix_path(path) for path in test_files}
    return [test for test in tests if node_file(test.node_id) in wanted]


def largest_per_node(tests: Iterable[TestCost]) -> list[TestCost]:
    """Keep one measurement per node id: the largest CPU cost seen."""
    best: dict[str, TestCost] = {}
    for test in tests:
        current = best.get(test.node_id)
        if current is None or test.cpu_seconds > current.cpu_seconds:
            best[test.node_id] = test
    return [best[node_id] for node_id in sorted(best)]


def wanted_marker(cpu_seconds: float, medium_seconds: float, slow_seconds: float) -> str | None:
    """The cost marker a measurement calls for, with the thresholds as given."""
    if cpu_seconds >= slow_seconds:
        return "slow"
    if cpu_seconds >= medium_seconds:
        return "medium"
    return None


def classify(
    test: TestCost,
    *,
    medium_seconds: float,
    slow_seconds: float,
    band: float,
) -> Finding | None:
    """Return the finding for one measured test, or None when its marker holds.

    A marker is questioned only when the reading is on the wrong side of a
    threshold by more than `band`; inside a band either marking is accepted.
    """
    cpu = test.cpu_seconds
    current = test.cost_marker
    if current is None:
        # Under-marked: judge against the thresholds raised by the band.
        wanted = wanted_marker(cpu, medium_seconds * band, slow_seconds * band)
        if wanted is None:
            return None
        return Finding("unmarked-over-medium", test, wanted)
    if current == "medium":
        if cpu >= slow_seconds * band:
            return Finding("medium-over-slow", test, "slow")
        if cpu < medium_seconds / band:
            return Finding("medium-under-medium", test, "no cost marker")
        return None
    # current == "slow"
    if cpu >= slow_seconds / band or test.slow_reason:
        return None
    wanted = wanted_marker(cpu, medium_seconds, slow_seconds)
    return Finding("slow-without-reason", test, wanted or "no cost marker")


def check_costs(
    tests: Iterable[TestCost],
    *,
    medium_seconds: float = MEDIUM_CPU_SECONDS,
    slow_seconds: float = SLOW_CPU_SECONDS,
    band: float = BAND,
    phase: str = DEFAULT_PHASE,
    sources: Iterable[str] = (),
    test_files: Iterable[str] | None = None,
) -> Report:
    """Compare every measured test's cost marker with the marker its cost calls for.

    With `test_files`, only the tests in those files are judged and counted.
    """
    if not medium_seconds < slow_seconds:
        raise ValueError("the medium threshold must be below the slow threshold")
    if band < 1.0:
        raise ValueError("the band must be at least 1")
    report = Report(medium_seconds, slow_seconds, band, phase, sources=list(sources))
    if test_files is not None:
        report.restricted_to = tuple(sorted({_posix_path(path) for path in test_files}))
        tests = restrict_to_files(tests, report.restricted_to)
    all_tests = largest_per_node(tests)
    report.tests = [test for test in all_tests if test.measured]
    report.unmeasured = len(all_tests) - len(report.tests)
    for test in report.tests:
        finding = classify(
            test, medium_seconds=medium_seconds, slow_seconds=slow_seconds, band=band
        )
        if finding is not None:
            report.findings.append(finding)
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
        "--band",
        type=float,
        default=BAND,
        help=f"hysteresis factor: a marker is questioned only beyond threshold * or / band (default {BAND:g})",
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
    parser.add_argument(
        "--test-file",
        dest="test_files",
        action="append",
        metavar="PATH",
        help=(
            "judge only the tests in this test file (repository-relative, as in the "
            "node id); repeat for several files"
        ),
    )
    parser.add_argument(
        "--json",
        dest="json_path",
        type=Path,
        help="write the outcome (counts and flagged tests) as JSON to this file",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(argv)
    skipped: list[str] = []
    try:
        tests, sources = load_tests(args.paths, phase=args.phase, skipped=skipped)
    except UnsupportedMetricsSchema as exc:
        print(f"[X] {exc}")
        return 2
    for path in skipped:
        print(f"[skip] {path}: not a metrics artefact with per-test records")
    if not tests:
        print("[X] no per-test records found; pass ci-metrics-*-cp312-manylinux-x86_64 artefacts")
        return 2
    report = check_costs(
        tests,
        medium_seconds=args.medium_seconds,
        slow_seconds=args.slow_seconds,
        band=args.band,
        phase=args.phase,
        sources=sources,
        test_files=args.test_files,
    )
    print(report.message())
    if args.histogram:
        print()
        print("\n".join(histogram_lines(report.tests)))
    if args.markdown:
        args.markdown.parent.mkdir(parents=True, exist_ok=True)
        with args.markdown.open("a", encoding="utf-8") as handle:
            handle.write(report.markdown())
    if args.json_path:
        args.json_path.parent.mkdir(parents=True, exist_ok=True)
        args.json_path.write_text(
            json.dumps(report.to_dict(), indent=2) + "\n", encoding="utf-8"
        )
    return 0 if report.ok else 1


if __name__ == "__main__":
    sys.exit(main())
