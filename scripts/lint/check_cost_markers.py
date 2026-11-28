#!/usr/bin/env python3
"""Check the `medium` and `slow` cost markers against measured CPU seconds.

The cost markers select tests into CI tiers (`.claude/rules/tests/patterns.md`,
"Importance and cost"). They used to be assigned by wall-clock feel, and wall
time on a hosted runner sits on a 2x noise floor: the same 166-test physics tier
took 169 s and 343 s on the same runner class on the same day. The markers are
therefore defined by process CPU seconds of the test body (the `call` phase)
on the Linux reference runner (`ubuntu-latest`, cp312), which the metrics
plugin (`scripts/suews/pytest_ci_metrics.py`) records per test in the
per-axis Linux cp312 metrics artefacts (schema 1: `api` and `physics`;
schema 2: `tooling`, `unit`, `native`, and `physics`) of every nightly run.

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

In addition to per-node cost-marker drift, this script emits strictly warn-only
dynamic CPU budget diagnostics (calibration phase, gh#2349 W1-8; never changes
the exit code):

- **Per-function parametrisation explosion**: groups measured non-`slow` nodes
  by base test function (`file::[Class::]test_func`, stripping `[...]`),
  computes `(node_count, median_node_cpu, total_call_cpu)`, and warns when
  `node_count >= 2` and `node_count * median_node_cpu > 20.0` CPU-s
  (`PARAMETRISE_EXPLOSION_CPU_SECONDS`, overridable via
  `--parametrize-budget-seconds`).
- **Per-axis call CPU summary**: totals measured call CPU seconds per axis
  (`tooling`, `unit`, `native`, `physics` on Schema-2 artefacts, falling back
  gracefully to `api`/`physics` on Schema-1 or `unattributed` when unlabelled).
- **Optional per-PR added-CPU delta**: when `--baseline-metrics` and/or
  `--budget-warn` is supplied, reports the CPU delta against the baseline (and
  whether it exceeds the advisory budget) without affecting the exit code.

Usage::

    python scripts/lint/check_cost_markers.py ci-metrics/ [--markdown SUMMARY]
    python scripts/lint/check_cost_markers.py api.json physics-pytest.json --histogram
    python scripts/lint/check_cost_markers.py ci-metrics/ --test-file test/core/test_x.py --json out.json
    python scripts/lint/check_cost_markers.py ci-metrics/ --baseline-metrics base-metrics/ --budget-warn 30
"""

from __future__ import annotations

import argparse
from collections.abc import Iterable, Iterator
from dataclasses import dataclass, field
import json
import math
from pathlib import Path
import re
import statistics
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
# Warn-only dynamic CPU budget thresholds (gh#2349 W1-8, calibration phase).
# Findings produced by these thresholds are strictly advisory and never change
# the process exit code.
PARAMETRISE_EXPLOSION_CPU_SECONDS = 20.0
DEFAULT_PR_ADDED_CPU_BUDGET_SECONDS = 30.0
# The phase whose CPU seconds define a test's cost. `call` is the test body;
# `total` adds fixture setup and teardown, which under xdist charges a shared
# session fixture to whichever test reaches it first on each worker.
DEFAULT_PHASE = "call"
PHASES = ("setup", "call", "teardown", "total")
LEGACY_AXES = ("api", "physics")
MULTI_AXES = ("tooling", "unit", "native", "physics")
AXES = ("api", "tooling", "unit", "native", "physics")
_TOKEN_SPLIT_RE = re.compile(r"[^a-z0-9]+")
_CI_METRICS_PREFIXES = ("ci-metrics-", "ci_metrics_")
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
    axis: str | None = None
    call_cpu_seconds: float | None = None
    call_wall_seconds: float | None = None

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

    @property
    def effective_call_cpu(self) -> float:
        """Call-phase CPU seconds (falling back to ``cpu_seconds`` when unset)."""
        return (
            self.call_cpu_seconds
            if self.call_cpu_seconds is not None
            else self.cpu_seconds
        )

    @property
    def effective_call_wall(self) -> float:
        """Call-phase wall seconds (falling back to ``wall_seconds`` when unset)."""
        return (
            self.call_wall_seconds
            if self.call_wall_seconds is not None
            else self.wall_seconds
        )


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


@dataclass(frozen=True)
class FunctionExplosionWarning:
    """Warn-only finding for a parametrised non-slow function exceeding CPU budget."""

    function_id: str
    node_count: int
    median_node_cpu: float
    estimated_cpu_seconds: float
    total_call_cpu: float
    threshold_seconds: float
    axis: str | None = None

    def line(self) -> str:
        """Render a single plain-text diagnostic line for this explosion warning."""
        safe_func = _sanitise_diagnostic(self.function_id)
        axis_note = (
            f"; axis={_sanitise_diagnostic(self.axis)}"
            if self.axis is not None
            else ""
        )
        return (
            f"  - {safe_func}: {self.node_count} nodes x "
            f"{self.median_node_cpu:.2f} median = {self.estimated_cpu_seconds:.1f} "
            f"CPU-s > {self.threshold_seconds:g} CPU-s "
            f"(total {self.total_call_cpu:.1f} CPU-s{axis_note}; warn-only)"
        )


@dataclass(frozen=True)
class AxisCpuSummary:
    """Total measured CPU and wall seconds grouped by dependency axis."""

    axis: str
    test_count: int
    total_cpu_seconds: float
    total_wall_seconds: float


@dataclass(frozen=True)
class AxisCpuDelta:
    """Per-axis CPU comparison between current and baseline measurements."""

    axis: str
    current_cpu_seconds: float
    baseline_cpu_seconds: float
    delta_cpu_seconds: float


@dataclass(frozen=True)
class BudgetDelta:
    """Warn-only per-PR CPU delta against baseline metrics or an absolute budget."""

    current_cpu_seconds: float
    baseline_cpu_seconds: float | None
    delta_cpu_seconds: float
    budget_seconds: float | None
    exceeded: bool
    added_test_count: int
    added_tests_cpu_seconds: float
    per_axis: tuple[AxisCpuDelta, ...] = ()

    def summary_line(self) -> str:
        """Render a one-line advisory summary of the PR CPU delta."""
        tag = "[WARN]" if self.exceeded else "[INFO]"
        if self.baseline_cpu_seconds is not None:
            delta_str = f"{self.delta_cpu_seconds:+.1f} CPU-s"
            base_part = (
                f"current {self.current_cpu_seconds:.1f} CPU-s vs baseline "
                f"{self.baseline_cpu_seconds:.1f} CPU-s (delta {delta_str}; "
                f"{self.added_test_count} added test(s), "
                f"{self.added_tests_cpu_seconds:.1f} CPU-s)"
            )
        else:
            base_part = f"current {self.current_cpu_seconds:.1f} CPU-s (no baseline metrics)"
        if self.budget_seconds is not None:
            status = "exceeds" if self.exceeded else "within"
            return (
                f"{tag} PR CPU budget (warn-only): {base_part}; "
                f"{status} advisory budget {self.budget_seconds:g} CPU-s."
            )
        return f"{tag} PR CPU delta (warn-only): {base_part}."


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
    # Strictly warn-only dynamic CPU budget fields (never affect `ok` or exit code).
    explosion_threshold_seconds: float = PARAMETRISE_EXPLOSION_CPU_SECONDS
    explosion_warnings: list[FunctionExplosionWarning] = field(default_factory=list)
    axis_summary: list[AxisCpuSummary] = field(default_factory=list)
    budget_delta: BudgetDelta | None = None
    advisory_errors: list[str] = field(default_factory=list)

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
        budget_payload: dict[str, Any] | None = None
        if self.budget_delta is not None:
            budget_payload = {
                "current_cpu_seconds": round(self.budget_delta.current_cpu_seconds, 4),
                "baseline_cpu_seconds": (
                    None
                    if self.budget_delta.baseline_cpu_seconds is None
                    else round(self.budget_delta.baseline_cpu_seconds, 4)
                ),
                "delta_cpu_seconds": round(self.budget_delta.delta_cpu_seconds, 4),
                "budget_seconds": self.budget_delta.budget_seconds,
                "exceeded": self.budget_delta.exceeded,
                "added_test_count": self.budget_delta.added_test_count,
                "added_tests_cpu_seconds": round(
                    self.budget_delta.added_tests_cpu_seconds, 4
                ),
                "per_axis": [
                    {
                        "axis": row.axis,
                        "current_cpu_seconds": round(row.current_cpu_seconds, 4),
                        "baseline_cpu_seconds": round(row.baseline_cpu_seconds, 4),
                        "delta_cpu_seconds": round(row.delta_cpu_seconds, 4),
                    }
                    for row in self.budget_delta.per_axis
                ],
            }
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
            "axis_cpu_summary": [
                {
                    "axis": row.axis,
                    "test_count": row.test_count,
                    "total_cpu_seconds": round(row.total_cpu_seconds, 4),
                    "total_wall_seconds": round(row.total_wall_seconds, 4),
                }
                for row in self.axis_summary
            ],
            "parametrize_explosion_threshold_seconds": self.explosion_threshold_seconds,
            "parametrize_explosion_warnings": [
                {
                    "function_id": warn.function_id,
                    "node_count": warn.node_count,
                    "median_node_cpu": round(warn.median_node_cpu, 4),
                    "estimated_cpu_seconds": round(warn.estimated_cpu_seconds, 4),
                    "total_call_cpu": round(warn.total_call_cpu, 4),
                    "threshold_seconds": warn.threshold_seconds,
                    "axis": warn.axis,
                }
                for warn in self.explosion_warnings
            ],
            "budget_delta": budget_payload,
            "advisory_errors": list(self.advisory_errors),
        }

    def message(self) -> str:
        """Plain-text report: header, findings, and warn-only CPU budget diagnostics."""
        lines = [f"Cost markers against measured CPU seconds: {self._scope()}"]
        if self.ok:
            lines.append("[OK] every medium and slow marker agrees with its measured cost.")
        else:
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
        if self.axis_summary:
            axis_parts = [
                f"{_sanitise_diagnostic(row.axis)}={row.total_cpu_seconds:.1f} CPU-s ({row.test_count})"
                for row in self.axis_summary
            ]
            lines.append(f"Axis call CPU summary: {', '.join(axis_parts)}.")
        if self.explosion_warnings:
            lines.append(
                f"[WARN] {len(self.explosion_warnings)} parametrised non-slow function(s) "
                f"exceed {self.explosion_threshold_seconds:g} CPU-s "
                "(node_count * median_node_cpu; warn-only):"
            )
            lines.extend(warn.line() for warn in self.explosion_warnings)
        if self.budget_delta is not None:
            lines.append(_sanitise_diagnostic(self.budget_delta.summary_line()))
            for row in self.budget_delta.per_axis:
                safe_axis = _sanitise_diagnostic(row.axis)
                lines.append(
                    f"  - axis {safe_axis}: {row.current_cpu_seconds:.1f} CPU-s "
                    f"(baseline {row.baseline_cpu_seconds:.1f}, "
                    f"delta {row.delta_cpu_seconds:+.1f} CPU-s)"
                )
        for err in self.advisory_errors:
            lines.append(
                f"[WARN] advisory budget diagnostic unavailable (warn-only): "
                f"{_sanitise_diagnostic(err)}"
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
        if self.axis_summary:
            lines.extend([
                "",
                "### Per-axis call CPU summary",
                "",
                "| Axis | Tests | Total CPU-s | Total wall-s |",
                "|---|---:|---:|---:|",
            ])
            for row in self.axis_summary:
                safe_axis = _sanitise_diagnostic(row.axis)
                lines.append(
                    f"| `{safe_axis}` | {row.test_count} | "
                    f"{row.total_cpu_seconds:.1f} | {row.total_wall_seconds:.1f} |"
                )
        if self.explosion_warnings:
            lines.extend([
                "",
                "### Parametrisation explosion warnings (warn-only)",
                "",
                "| Function | Nodes | Median CPU-s | Nodes x Median | Total CPU-s | Axis |",
                "|---|---:|---:|---:|---:|---|",
            ])
            for warn in self.explosion_warnings:
                safe_func = _sanitise_diagnostic(warn.function_id)
                safe_axis = _sanitise_diagnostic(warn.axis) if warn.axis else "-"
                lines.append(
                    f"| `{safe_func}` | {warn.node_count} | "
                    f"{warn.median_node_cpu:.2f} | {warn.estimated_cpu_seconds:.1f} | "
                    f"{warn.total_call_cpu:.1f} | {safe_axis} |"
                )
        if self.budget_delta is not None:
            lines.extend([
                "",
                "### PR CPU budget delta (warn-only)",
                "",
                _sanitise_diagnostic(self.budget_delta.summary_line()),
            ])
            if self.budget_delta.per_axis:
                lines.extend([
                    "",
                    "| Axis | Current CPU-s | Baseline CPU-s | Delta CPU-s |",
                    "|---|---:|---:|---:|",
                ])
                for row in self.budget_delta.per_axis:
                    safe_axis = _sanitise_diagnostic(row.axis)
                    lines.append(
                        f"| `{safe_axis}` | {row.current_cpu_seconds:.1f} | "
                        f"{row.baseline_cpu_seconds:.1f} | {row.delta_cpu_seconds:+.1f} |"
                    )
        if self.advisory_errors:
            lines.extend(["", "### Advisory budget diagnostics (warn-only)", ""])
            for err in self.advisory_errors:
                lines.append(f"- {_sanitise_diagnostic(err)}")
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


def _axis_tokens(name: str) -> set[str]:
    """Return recognised axis tokens present in a filename or directory name."""
    return {tok for tok in _TOKEN_SPLIT_RE.split(name.lower()) if tok in AXES}


def _axis_from_prefixed_name(name: str) -> tuple[str | None, bool]:
    """Extract an axis from a prefixed artefact name, flagging multi-axis ambiguity."""
    lowered = name.lower()
    for prefix in _CI_METRICS_PREFIXES:
        if lowered.startswith(prefix):
            lowered = lowered[len(prefix) :]
            break
    tokens = _axis_tokens(lowered)
    if len(tokens) > 1:
        return None, True
    for axis in AXES:
        if (
            lowered == axis
            or lowered.startswith(f"{axis}-")
            or lowered.startswith(f"{axis}_")
            or lowered.startswith(f"{axis}.")
        ):
            return axis, False
    return None, False


def _axis_from_parent_dir(name: str, *, immediate: bool) -> tuple[str | None, bool]:
    """Extract an axis from a parent directory name, flagging ambiguity."""
    lowered = name.lower()
    if lowered in AXES:
        return lowered, False
    if lowered in {"physics-core", "physics_core"}:
        return "physics", False
    if lowered.startswith(_CI_METRICS_PREFIXES):
        return _axis_from_prefixed_name(lowered)
    if immediate and len(_axis_tokens(lowered)) > 1:
        return None, True
    return None, False


def source_axis(path: Path) -> str | None:
    """Return the CI test axis a metrics artefact belongs to.

    Recognises both schema 1 (``api``, ``physics``) and schema 2
    (``tooling``, ``unit``, ``native``, ``physics``, including
    ``physics-core``). Returns ``None`` when the path carries no axis label,
    when a path component names multiple axes ambiguously, or when the
    filename and enclosing artefact directory contradict each other.
    """
    stem_axis, stem_ambiguous = _axis_from_prefixed_name(path.stem)
    if stem_ambiguous:
        return None
    parent_axis: str | None = None
    parent_from_artefact_dir = False
    for depth, parent in enumerate(path.parents):
        axis, ambiguous = _axis_from_parent_dir(parent.name, immediate=(depth == 0))
        if ambiguous:
            return None
        if axis is None:
            continue
        is_artefact_dir = depth == 0 or parent.name.lower().startswith(
            _CI_METRICS_PREFIXES
        )
        if stem_axis is not None and axis != stem_axis and is_artefact_dir:
            return None
        if parent_axis is None:
            parent_axis = axis
            parent_from_artefact_dir = is_artefact_dir
        elif axis != parent_axis and (is_artefact_dir or parent_from_artefact_dir):
            return None
    return stem_axis if stem_axis is not None else parent_axis


def missing_required_axes(
    tests: Iterable[TestCost], required_axes: Iterable[str]
) -> list[str]:
    """Return required axes that have no measured tests in ``tests``."""
    present = {test.axis for test in tests if test.measured and test.axis is not None}
    missing: list[str] = []
    seen: set[str] = set()
    for axis in required_axes:
        if axis not in AXES:
            raise ValueError(f"unknown axis {axis!r}; expected one of {AXES}")
        if axis not in present and axis not in seen:
            missing.append(axis)
            seen.add(axis)
    return missing


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
        axis = source_axis(path)
        for record in metrics["tests"]:
            reasons = record.get("marker_reasons") or {}
            cpu_map = record["cpu_seconds"]
            wall_map = record["wall_seconds"]
            call_cpu = (
                float(cpu_map["call"]) if "call" in cpu_map else float(cpu_map[phase])
            )
            call_wall = (
                float(wall_map["call"])
                if "call" in wall_map
                else float(wall_map[phase])
            )
            tests.append(
                TestCost(
                    node_id=str(record["node_id"]),
                    markers=tuple(sorted(str(m) for m in record.get("markers", []))),
                    cpu_seconds=float(cpu_map[phase]),
                    wall_seconds=float(wall_map[phase]),
                    source=path.name,
                    outcome=str(record.get("outcome")),
                    marker_reasons=tuple(sorted((str(k), str(v)) for k, v in reasons.items())),
                    axis=axis,
                    call_cpu_seconds=call_cpu,
                    call_wall_seconds=call_wall,
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


def base_function_id(node_id: str) -> str:
    """Return ``file::[Class::]test_func`` with any ``[...]`` parameter suffix stripped."""
    sep = node_id.find("::")
    if sep == -1:
        return _posix_path(node_id)
    file_part = _posix_path(node_id[:sep])
    rest = node_id[sep + 2 :]
    bracket = rest.find("[")
    func_part = rest[:bracket] if bracket != -1 else rest
    return f"{file_part}::{func_part}"


def detect_parametrize_explosions(
    tests: Iterable[TestCost],
    *,
    threshold_seconds: float = PARAMETRISE_EXPLOSION_CPU_SECONDS,
) -> list[FunctionExplosionWarning]:
    """Flag non-``slow`` parametrised functions where ``node_count * median_call_cpu > threshold``.

    Parameters
    ----------
    tests : Iterable[TestCost]
        Measured per-node cost records (deduplicated per node id).
    threshold_seconds : float, optional
        Warn-only CPU budget in seconds (default ``PARAMETRISE_EXPLOSION_CPU_SECONDS``).

    Returns
    -------
    list[FunctionExplosionWarning]
        Warn-only findings ordered by descending estimated CPU seconds.
    """
    if not math.isfinite(threshold_seconds) or threshold_seconds <= 0.0:
        raise ValueError("the parametrize explosion threshold must be finite and positive")

    grouped: dict[str, list[TestCost]] = {}
    for test in tests:
        if not test.measured or test.cost_marker == "slow":
            continue
        if not math.isfinite(test.effective_call_cpu):
            continue
        grouped.setdefault(base_function_id(test.node_id), []).append(test)

    warnings: list[FunctionExplosionWarning] = []
    for func_id, group in grouped.items():
        node_count = len(group)
        if node_count < 2:
            continue
        cpus = [item.effective_call_cpu for item in group]
        median_cpu = float(statistics.median(cpus))
        total_cpu = float(sum(cpus))
        estimated = node_count * median_cpu
        if not (
            math.isfinite(median_cpu)
            and math.isfinite(total_cpu)
            and math.isfinite(estimated)
        ):
            continue
        if estimated > threshold_seconds:
            axes = {item.axis for item in group if item.axis is not None}
            axis = next(iter(axes)) if len(axes) == 1 else None
            warnings.append(
                FunctionExplosionWarning(
                    function_id=func_id,
                    node_count=node_count,
                    median_node_cpu=median_cpu,
                    estimated_cpu_seconds=estimated,
                    total_call_cpu=total_cpu,
                    threshold_seconds=threshold_seconds,
                    axis=axis,
                )
            )
    warnings.sort(key=lambda item: (-item.estimated_cpu_seconds, item.function_id))
    return warnings


def _ordered_axes(axes: Iterable[str]) -> list[str]:
    """Order observed axis labels canonically (Schema-2, Schema-1, unattributed)."""
    axis_set = set(axes)
    canonical = (*MULTI_AXES, "api", "unattributed")
    ordered = [axis for axis in canonical if axis in axis_set]
    extras = sorted(axis_set - set(canonical))
    return [*ordered, *extras]


def largest_per_axis_node(tests: Iterable[TestCost]) -> list[TestCost]:
    """Keep one measurement per ``(axis, node_id)``: the largest call CPU cost seen."""
    best: dict[tuple[str | None, str], TestCost] = {}
    for test in tests:
        key = (test.axis, test.node_id)
        current = best.get(key)
        if current is None or test.effective_call_cpu > current.effective_call_cpu:
            best[key] = test
    return [best[key] for key in sorted(best, key=lambda item: (item[0] or "", item[1]))]


def _all_call_measurements_finite(tests: Iterable[TestCost]) -> bool:
    """Return True when every measured record carries finite call CPU and wall seconds."""
    return all(
        math.isfinite(t.effective_call_cpu) and math.isfinite(t.effective_call_wall)
        for t in tests
        if t.measured
    )


def summarise_axis_cpu(tests: Iterable[TestCost]) -> list[AxisCpuSummary]:
    """Summarise measured call CPU and wall seconds per axis (deduplicated within each axis)."""
    counts: dict[str, int] = {}
    cpu_totals: dict[str, float] = {}
    wall_totals: dict[str, float] = {}
    for test in largest_per_axis_node(tests):
        if not test.measured:
            continue
        if not (
            math.isfinite(test.effective_call_cpu)
            and math.isfinite(test.effective_call_wall)
        ):
            raise ValueError(f"non-finite call metrics for {test.node_id}")
        axis = test.axis if test.axis is not None else "unattributed"
        counts[axis] = counts.get(axis, 0) + 1
        cpu_totals[axis] = cpu_totals.get(axis, 0.0) + test.effective_call_cpu
        wall_totals[axis] = wall_totals.get(axis, 0.0) + test.effective_call_wall

    summaries: list[AxisCpuSummary] = []
    for axis in _ordered_axes(counts):
        if not (
            math.isfinite(cpu_totals[axis]) and math.isfinite(wall_totals[axis])
        ):
            raise ValueError(f"non-finite aggregated call metrics for axis {axis}")
        summaries.append(
            AxisCpuSummary(
                axis=axis,
                test_count=counts[axis],
                total_cpu_seconds=cpu_totals[axis],
                total_wall_seconds=wall_totals[axis],
            )
        )
    return summaries


def compute_budget_delta(
    current_tests: Iterable[TestCost],
    baseline_tests: Iterable[TestCost] | None = None,
    *,
    budget_seconds: float | None = None,
) -> BudgetDelta:
    """Compute warn-only per-PR call CPU delta (per-axis-deduplicated) against optional baseline."""
    if budget_seconds is not None and (
        not math.isfinite(budget_seconds) or budget_seconds < 0.0
    ):
        raise ValueError("the budget warning threshold must be finite and non-negative")

    curr_measured = [t for t in largest_per_axis_node(current_tests) if t.measured]
    if not _all_call_measurements_finite(curr_measured):
        raise ValueError("current metrics contain non-finite call CPU/wall values")
    curr_total = float(sum(t.effective_call_cpu for t in curr_measured))
    curr_by_axis: dict[str, float] = {}
    for test in curr_measured:
        axis = test.axis if test.axis is not None else "unattributed"
        curr_by_axis[axis] = curr_by_axis.get(axis, 0.0) + test.effective_call_cpu
    if not math.isfinite(curr_total) or not all(
        math.isfinite(v) for v in curr_by_axis.values()
    ):
        raise ValueError("current call CPU totals overflowed to non-finite values")

    if baseline_tests is None:
        delta_total = curr_total
        exceeded = budget_seconds is not None and delta_total > budget_seconds
        per_axis = tuple(
            AxisCpuDelta(
                axis=axis,
                current_cpu_seconds=curr_by_axis[axis],
                baseline_cpu_seconds=0.0,
                delta_cpu_seconds=curr_by_axis[axis],
            )
            for axis in _ordered_axes(curr_by_axis)
        )
        return BudgetDelta(
            current_cpu_seconds=curr_total,
            baseline_cpu_seconds=None,
            delta_cpu_seconds=delta_total,
            budget_seconds=budget_seconds,
            exceeded=exceeded,
            added_test_count=len(curr_measured),
            added_tests_cpu_seconds=curr_total,
            per_axis=per_axis,
        )

    base_measured = [t for t in largest_per_axis_node(baseline_tests) if t.measured]
    if not _all_call_measurements_finite(base_measured):
        raise ValueError("baseline metrics contain non-finite call CPU/wall values")
    base_total = float(sum(t.effective_call_cpu for t in base_measured))
    base_keys = {(t.axis, t.node_id) for t in base_measured}
    base_by_axis: dict[str, float] = {}
    for test in base_measured:
        axis = test.axis if test.axis is not None else "unattributed"
        base_by_axis[axis] = base_by_axis.get(axis, 0.0) + test.effective_call_cpu
    if not math.isfinite(base_total) or not all(
        math.isfinite(v) for v in base_by_axis.values()
    ):
        raise ValueError("baseline call CPU totals overflowed to non-finite values")

    added_tests = [t for t in curr_measured if (t.axis, t.node_id) not in base_keys]
    added_cpu = float(sum(t.effective_call_cpu for t in added_tests))
    delta_total = curr_total - base_total
    if not (math.isfinite(delta_total) and math.isfinite(added_cpu)):
        raise ValueError("computed CPU delta is non-finite")
    exceeded = budget_seconds is not None and delta_total > budget_seconds
    all_axes = _ordered_axes(set(curr_by_axis) | set(base_by_axis))
    per_axis = tuple(
        AxisCpuDelta(
            axis=axis,
            current_cpu_seconds=curr_by_axis.get(axis, 0.0),
            baseline_cpu_seconds=base_by_axis.get(axis, 0.0),
            delta_cpu_seconds=curr_by_axis.get(axis, 0.0) - base_by_axis.get(axis, 0.0),
        )
        for axis in all_axes
    )
    return BudgetDelta(
        current_cpu_seconds=curr_total,
        baseline_cpu_seconds=base_total,
        delta_cpu_seconds=delta_total,
        budget_seconds=budget_seconds,
        exceeded=exceeded,
        added_test_count=len(added_tests),
        added_tests_cpu_seconds=added_cpu,
        per_axis=per_axis,
    )


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


def _coerce_advisory_float(
    raw: float | str | None,
    *,
    name: str,
    allow_zero: bool,
    fallback: float | None,
    advisory_errors: list[str],
) -> float | None:
    """Parse and validate an advisory threshold without ever raising or exiting non-zero."""
    if raw is None:
        return None
    try:
        value = float(raw)
    except (TypeError, ValueError, OverflowError):
        advisory_errors.append(
            f"invalid {name} {raw!r} (expected a finite number)"
        )
        return fallback
    bound_ok = value >= 0.0 if allow_zero else value > 0.0
    if not math.isfinite(value) or not bound_ok:
        bound_desc = "finite and non-negative" if allow_zero else "finite and positive"
        advisory_errors.append(f"invalid {name} {raw!r} (must be {bound_desc})")
        return fallback
    return value


def check_costs(
    tests: Iterable[TestCost],
    *,
    medium_seconds: float = MEDIUM_CPU_SECONDS,
    slow_seconds: float = SLOW_CPU_SECONDS,
    band: float = BAND,
    phase: str = DEFAULT_PHASE,
    sources: Iterable[str] = (),
    test_files: Iterable[str] | None = None,
    explosion_seconds: float | str = PARAMETRISE_EXPLOSION_CPU_SECONDS,
    baseline_tests: Iterable[TestCost] | None = None,
    budget_warn_seconds: float | str | None = None,
) -> Report:
    """Compare every measured test's cost marker with the marker its cost calls for.

    With `test_files`, only the tests in those files are judged and counted.
    Also populates strictly warn-only dynamic CPU budget summaries (per-function
    parametrisation explosions, per-axis call CPU totals, and optional PR delta);
    invalid advisory options or non-finite advisory metrics are isolated into
    ``report.advisory_errors`` rather than raising or altering ``report.ok``.
    """
    if not medium_seconds < slow_seconds:
        raise ValueError("the medium threshold must be below the slow threshold")
    if band < 1.0:
        raise ValueError("the band must be at least 1")

    advisory_errors: list[str] = []
    safe_explosion = _coerce_advisory_float(
        explosion_seconds,
        name="--parametrize-budget-seconds",
        allow_zero=False,
        fallback=PARAMETRISE_EXPLOSION_CPU_SECONDS,
        advisory_errors=advisory_errors,
    )
    assert safe_explosion is not None
    safe_budget = _coerce_advisory_float(
        budget_warn_seconds,
        name="--budget-warn",
        allow_zero=True,
        fallback=None,
        advisory_errors=advisory_errors,
    )

    report = Report(
        medium_seconds,
        slow_seconds,
        band,
        phase,
        sources=list(sources),
        explosion_threshold_seconds=safe_explosion,
        advisory_errors=advisory_errors,
    )
    raw_tests = list(tests)
    raw_baseline = None if baseline_tests is None else list(baseline_tests)
    if test_files is not None:
        report.restricted_to = tuple(sorted({_posix_path(path) for path in test_files}))
        raw_tests = restrict_to_files(raw_tests, report.restricted_to)
        if raw_baseline is not None:
            raw_baseline = restrict_to_files(raw_baseline, report.restricted_to)
    all_tests = largest_per_node(raw_tests)
    report.tests = [test for test in all_tests if test.measured]
    report.unmeasured = len(all_tests) - len(report.tests)
    for test in report.tests:
        finding = classify(
            test, medium_seconds=medium_seconds, slow_seconds=slow_seconds, band=band
        )
        if finding is not None:
            report.findings.append(finding)

    try:
        report.axis_summary = summarise_axis_cpu(raw_tests)
    except (ValueError, TypeError, OverflowError) as exc:
        report.advisory_errors.append(f"axis summary unavailable: {exc}")
    try:
        report.explosion_warnings = detect_parametrize_explosions(
            report.tests, threshold_seconds=safe_explosion
        )
    except (ValueError, TypeError, OverflowError) as exc:
        report.advisory_errors.append(f"parametrisation explosion check unavailable: {exc}")
    if raw_baseline is not None or safe_budget is not None:
        try:
            report.budget_delta = compute_budget_delta(
                raw_tests,
                baseline_tests=raw_baseline,
                budget_seconds=safe_budget,
            )
        except (ValueError, TypeError, OverflowError) as exc:
            report.advisory_errors.append(f"PR CPU budget delta unavailable: {exc}")
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
    parser.add_argument(
        "--require-axis",
        dest="require_axes",
        action="append",
        choices=AXES,
        help=(
            "require at least one measured test from this lane axis "
            f"({', '.join(AXES)}); repeat to require multiple axes before reporting clean"
        ),
    )
    parser.add_argument(
        "--parametrize-budget-seconds",
        type=str,
        default=str(PARAMETRISE_EXPLOSION_CPU_SECONDS),
        help=(
            "warn-only threshold for per-function parametrisation explosion "
            "(node_count * median_call_cpu, in CPU seconds; "
            f"default {PARAMETRISE_EXPLOSION_CPU_SECONDS:g})"
        ),
    )
    parser.add_argument(
        "--baseline-metrics",
        dest="baseline_paths",
        action="append",
        type=Path,
        metavar="PATH",
        help=(
            "optional baseline metrics JSON file or directory for warn-only "
            "per-PR added-CPU delta reporting; repeat for multiple paths"
        ),
    )
    parser.add_argument(
        "--budget-warn",
        nargs="?",
        const=str(DEFAULT_PR_ADDED_CPU_BUDGET_SECONDS),
        default=None,
        type=str,
        metavar="SECONDS",
        help=(
            "warn-only advisory PR added-CPU budget in CPU seconds "
            f"(default {DEFAULT_PR_ADDED_CPU_BUDGET_SECONDS:g} when flag is given without a value); "
            "never changes exit code"
        ),
    )
    return parser.parse_args(argv)


def _load_optional_baseline_tests(
    baseline_paths: list[Path] | None,
    phase: str,
) -> tuple[list[TestCost] | None, str | None]:
    """Load optional ``--baseline-metrics`` artefacts, isolating all advisory failures."""
    if not baseline_paths:
        return None, None
    baseline_skipped: list[str] = []
    try:
        baseline_tests, _ = load_tests(
            baseline_paths, phase=phase, skipped=baseline_skipped
        )
    except (
        UnsupportedMetricsSchema,
        KeyError,
        TypeError,
        ValueError,
        AttributeError,
        OverflowError,
        OSError,
    ) as exc:
        return None, f"baseline metrics: {exc}"
    for path in baseline_skipped:
        print(
            f"[skip] baseline {_sanitise_diagnostic(path)}: "
            "not a metrics artefact with per-test records"
        )
    if not baseline_tests:
        return None, "no per-test records found in --baseline-metrics"
    if not _all_call_measurements_finite(baseline_tests):
        return None, "baseline metrics contain non-finite call CPU/wall values"
    return baseline_tests, None


def main(argv: list[str] | None = None) -> int:
    """Run the cost-marker and warn-only dynamic CPU budget CLI."""
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
    if args.require_axes:
        missing = missing_required_axes(tests, args.require_axes)
        if missing:
            present = sorted({t.axis for t in tests if t.measured and t.axis is not None})
            found_str = ", ".join(present) if present else "none"
            print(
                f"[X] missing measured metrics for required axis/axes: {', '.join(missing)} "
                f"(measured axes present: {found_str})"
            )
            return 2
    baseline_tests, baseline_err = _load_optional_baseline_tests(
        args.baseline_paths, args.phase
    )
    extra_advisory_errors: list[str] = []
    budget_warn_arg = args.budget_warn
    if baseline_err is not None and budget_warn_arg is not None:
        _coerce_advisory_float(
            budget_warn_arg,
            name="--budget-warn",
            allow_zero=True,
            fallback=None,
            advisory_errors=extra_advisory_errors,
        )
        budget_warn_arg = None
    if baseline_err is not None:
        extra_advisory_errors.append(baseline_err)
    report = check_costs(
        tests,
        medium_seconds=args.medium_seconds,
        slow_seconds=args.slow_seconds,
        band=args.band,
        phase=args.phase,
        sources=sources,
        test_files=args.test_files,
        explosion_seconds=args.parametrize_budget_seconds,
        baseline_tests=baseline_tests,
        budget_warn_seconds=budget_warn_arg,
    )
    report.advisory_errors.extend(extra_advisory_errors)
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
            json.dumps(report.to_dict(), indent=2, allow_nan=False) + "\n",
            encoding="utf-8",
        )
    return 0 if report.ok else 1


if __name__ == "__main__":
    sys.exit(main())
