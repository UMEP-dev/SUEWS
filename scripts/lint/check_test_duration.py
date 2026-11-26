#!/usr/bin/env python3
"""Report-only CI guard for Python test-lane durations and cost markers.

GitHub-hosted Linux runners are bimodal (roughly 1.57x on the API lane and up
to ~1.95x on the physics lane for the same commit), so raw lane totals cannot
detect a +10% slowdown without flooding pull requests with false positives.

This script scales each metrics artefact independently relative to unchanged
reference tests in a committed baseline
(``scripts/lint/test_duration_baseline.json``).  Both baseline schemas are
supported: schema 1 (legacy ``api`` and ``physics`` lanes) and schema 2
(multi-axis ``tooling``, ``unit``, ``native``, and ``physics`` lanes/cohorts).
Scaling per artefact cancels bimodal runner speed differences across both
unsharded and sharded lanes (2-way or 4-way SHA-256 sharding via
``scripts/suews/pytest_shard.py``) and reports cost relative to that reference
cohort (note: a uniform slowdown affecting every reference test equally is
absorbed into the cohort factor).  After scaling, shards of the same lane are
aggregated so each test is judged once, whichever shard ran it.

Two conditions are reported (both advisory; CI runs this job with
``continue-on-error`` and never adds it to ``pr-gate``):

1. Any new or changed test whose cohort-normalised ``call``-phase CPU seconds
   exceed the repository's cost-marker threshold without the matching marker
   (reusing ``MEDIUM_CPU_SECONDS``, ``SLOW_CPU_SECONDS``, and ``BAND`` from
   ``scripts/lint/check_cost_markers.py``; see ``.claude/rules/tests/patterns.md``).
2. Any lane whose cohort-normalised ``call``-phase CPU total exceeds the
   committed baseline by more than ``--max-regression`` (default 10%).

Regenerating the baseline
-------------------------
Regenerate ``scripts/lint/test_duration_baseline.json`` after deliberate suite
growth (e.g. a new test module) or after hotspot speed-ups land on ``master``
so the reference stays close to current ``master`` behaviour.  Download the
Linux cp312 API and physics artefacts (using wildcards so sharded artefacts are
placed in separate subdirectories) from 6 or more recent green ``standard``
runs and pass their run directories to ``--write-baseline``::

    for id in <run_id_1> <run_id_2> ...; do
      mkdir -p ".context/runs/$id/api" ".context/runs/$id/physics"
      gh run download "$id" -p "ci-metrics-api-cp312-manylinux-x86_64*" -D ".context/runs/$id/api"
      gh run download "$id" -p "ci-metrics-physics-cp312-manylinux-x86_64*" -D ".context/runs/$id/physics"
    done
    python scripts/lint/check_test_duration.py --write-baseline .context/runs/*

Usage::

    python scripts/lint/check_test_duration.py ci-metrics/ [--markdown SUMMARY] [--json out.json]
    python scripts/lint/check_test_duration.py ci-metrics/ --test-file test/core/test_x.py --annotations
    python scripts/lint/check_test_duration.py --write-baseline run_dir_1 run_dir_2 ...
"""

from __future__ import annotations

import argparse
from collections.abc import Iterable, Sequence
from dataclasses import dataclass, field
import hashlib
import json
import math
from pathlib import Path
import re
import secrets
import statistics
import sys
from typing import Any

try:
    from scripts.lint import check_cost_markers as cost_markers
except ImportError:
    _LINT_DIR = Path(__file__).resolve().parent
    if str(_LINT_DIR) not in sys.path:
        sys.path.insert(0, str(_LINT_DIR))
    import check_cost_markers as cost_markers  # type: ignore[no-redef]

BASELINE_SCHEMA_VERSION = 1
MULTI_AXIS_BASELINE_SCHEMA_VERSION = 2
SUPPORTED_BASELINE_SCHEMA_VERSIONS = (
    BASELINE_SCHEMA_VERSION,
    MULTI_AXIS_BASELINE_SCHEMA_VERSION,
)
DEFAULT_BASELINE_PATH = Path("scripts") / "lint" / "test_duration_baseline.json"
LEGACY_LANES = ("api", "physics")
MULTI_AXIS_LANES = ("tooling", "unit", "native", "physics")
LANES = LEGACY_LANES
ALL_LANES = ("api", "tooling", "unit", "native", "physics")
SCHEMA_REQUIRED_LANES: dict[int, tuple[str, ...]] = {
    BASELINE_SCHEMA_VERSION: LEGACY_LANES,
    MULTI_AXIS_BASELINE_SCHEMA_VERSION: MULTI_AXIS_LANES,
}
_CI_METRICS_PREFIXES = ("ci-metrics-", "ci_metrics_")
DEFAULT_MAX_REGRESSION = 0.10
DEFAULT_MIN_REFERENCE_SECONDS = 1.0
DEFAULT_MAX_REFERENCES_PER_LANE = 200
DEFAULT_MIN_REFERENCE_MATCHES = 5
DEFAULT_MIN_COVERAGE_RATIO = 0.90
MAX_SHARD_TOTAL = 64
MAX_REPORTED_MISSING_SHARDS = 10
MAX_MEASURED_TEST_COUNT = 1_000_000_000
_INVALID_SHARD = (-1, -1)
_MAX_DISPLAY_FILENAME = 120
UNDERMARKED_KINDS = frozenset({"unmarked-over-medium", "medium-over-slow"})
INCOMPLETE_LANE_STATUSES = frozenset({"missing", "unavailable"})
TOP_CONTRIBUTORS_LIMIT = 10
_TOKEN_SPLIT_RE = re.compile(r"[^a-z0-9]+")
_SHARD_NAME_RE = re.compile(
    r"(?<![a-z0-9])shard[-_](\d+)[-_]of[-_](\d+)(?![a-z0-9])",
    re.IGNORECASE,
)
_SHARD_MARKER_RE = re.compile(r"(?<![a-z0-9])shard(?![a-z0-9])", re.IGNORECASE)
_COMPLETE_EXIT_CODES = frozenset({0, 1})
_VALID_OUTCOMES = frozenset({"passed", "failed", "skipped", "xfailed", "xpassed"})


@dataclass(frozen=True)
class LaneBaseline:
    """Committed duration baseline for one test lane."""

    lane: str
    total_cpu_seconds: float
    measured_test_count: int
    reference_tests: dict[str, float]


@dataclass(frozen=True)
class DurationBaseline:
    """Committed runner-normalised duration baseline across lanes."""

    schema_version: int
    phase: str
    min_reference_seconds: float
    max_references_per_lane: int
    generated_from_runs: tuple[str, ...]
    lanes: dict[str, LaneBaseline]


@dataclass(frozen=True)
class ArtefactTests:
    """Per-test measurements loaded from a single metrics artefact file."""

    source: str
    lane: str
    tests: tuple[cost_markers.TestCost, ...]
    shard: tuple[int, int] | None = None
    exit_code: int | None = None
    collected_count: int | None = None
    recorded_outcome_count: int | None = None
    invalid_measurement_count: int = 0
    session_metadata_invalid: bool = False


@dataclass(frozen=True)
class ArtefactScale:
    """Runner-speed normalisation summary for one artefact (job or shard)."""

    source: str
    lane: str
    measured_count: int
    matched_references: int
    runner_factor: float | None


@dataclass(frozen=True)
class ScaledTest:
    """One measured test after per-artefact runner normalisation."""

    test: cost_markers.TestCost
    lane: str
    runner_factor: float
    scaled_cpu_seconds: float


@dataclass(frozen=True)
class TestDelta:
    """Summary entry for a top contributing test in a lane."""

    node_id: str
    lane: str
    scaled_cpu_seconds: float
    baseline_cpu_seconds: float | None
    delta_seconds: float | None


@dataclass(frozen=True)
class LaneResult:
    """Normalised lane-total comparison against the committed baseline."""

    lane: str
    status: str
    baseline_total_seconds: float
    raw_total_seconds: float
    scaled_total_seconds: float | None
    delta_ratio: float | None
    regressed: bool
    measured_count: int
    unmeasured_count: int
    artefacts: tuple[ArtefactScale, ...]
    top_changed_tests: tuple[TestDelta, ...] = ()
    top_reference_increases: tuple[TestDelta, ...] = ()
    reason: str | None = None
    reason_code: str | None = None
    reason_details: dict[str, Any] = field(default_factory=dict)

    def factor_summary(self) -> str:
        """Render the per-artefact runner speed factor(s) as a compact string."""
        factors = [
            art.runner_factor for art in self.artefacts if art.runner_factor is not None
        ]
        if not factors:
            return "n/a"
        if len(factors) == 1:
            return f"{factors[0]:.2f}x"
        return f"{min(factors):.2f}x..{max(factors):.2f}x ({len(factors)} shards)"


@dataclass(frozen=True)
class MarkerWarning:
    """One new or changed test over a cost-marker threshold after normalisation."""

    kind: str
    node_id: str
    lane: str
    raw_cpu_seconds: float
    scaled_cpu_seconds: float
    runner_factor: float
    current: str | None
    wanted: str
    source: str

    def line(self) -> str:
        """Render a single plain-text bullet describing this finding."""
        current = _sanitise_identifier(self.current or "no cost marker")
        node_id = _sanitise_identifier(self.node_id)
        wanted = _sanitise_identifier(self.wanted)
        source = _sanitise_identifier(self.source)
        return (
            f"  - {node_id}: {self.scaled_cpu_seconds:.1f} normalised CPU-s "
            f"(raw {self.raw_cpu_seconds:.1f} s, runner {self.runner_factor:.2f}x; "
            f"{current} -> {wanted}; {source})"
        )


@dataclass
class DurationReport:
    """Complete report-only duration guard outcome across lanes and changed tests."""

    max_regression: float
    medium_seconds: float
    slow_seconds: float
    band: float
    phase: str
    lanes: dict[str, LaneResult]
    marker_warnings: list[MarkerWarning]
    checked_changed_tests: int
    restricted_to: tuple[str, ...] | None
    sources: list[str]

    @property
    def regressed_lanes(self) -> list[LaneResult]:
        """Return lanes whose normalised total exceeded the regression threshold."""
        return [result for result in self.lanes.values() if result.regressed]

    @property
    def incomplete_lanes(self) -> list[LaneResult]:
        """Return lanes that could not be compared (missing or unavailable)."""
        return [
            result
            for result in self.lanes.values()
            if result.status in INCOMPLETE_LANE_STATUSES
        ]

    @property
    def complete(self) -> bool:
        """Return True when every baseline lane was evaluated."""
        return not self.incomplete_lanes

    @property
    def status(self) -> str:
        """Return the overall report status: ``ok``, ``warning``, or ``incomplete``."""
        if self.regressed_lanes or self.marker_warnings:
            return "warning"
        if self.incomplete_lanes:
            return "incomplete"
        return "ok"

    @property
    def ok(self) -> bool:
        """Return True only when comparison completed cleanly with no warnings."""
        return self.complete and not self.regressed_lanes and not self.marker_warnings

    def to_dict(self) -> dict[str, Any]:
        """Serialise the report to plain JSON-compatible data."""
        return {
            "ok": self.ok,
            "complete": self.complete,
            "status": self.status,
            "incomplete_lanes": [res.lane for res in self.incomplete_lanes],
            "max_regression": self.max_regression,
            "phase": self.phase,
            "medium_seconds": self.medium_seconds,
            "slow_seconds": self.slow_seconds,
            "band": self.band,
            "checked_changed_tests": self.checked_changed_tests,
            "restricted_to": (
                None if self.restricted_to is None else list(self.restricted_to)
            ),
            "sources": list(self.sources),
            "lanes": {
                lane: {
                    "lane": res.lane,
                    "status": res.status,
                    "baseline_total_seconds": round(res.baseline_total_seconds, 2),
                    "raw_total_seconds": round(res.raw_total_seconds, 2),
                    "scaled_total_seconds": (
                        None
                        if res.scaled_total_seconds is None
                        else round(res.scaled_total_seconds, 2)
                    ),
                    "delta_ratio": (
                        None if res.delta_ratio is None else round(res.delta_ratio, 4)
                    ),
                    "delta_percent": (
                        None
                        if res.delta_ratio is None
                        else round(res.delta_ratio * 100.0, 2)
                    ),
                    "regressed": res.regressed,
                    "measured_count": res.measured_count,
                    "unmeasured_count": res.unmeasured_count,
                    "artefact_count": len(res.artefacts),
                    "factor_summary": res.factor_summary(),
                    "artefacts": [
                        {
                            "source": art.source,
                            "measured_count": art.measured_count,
                            "matched_references": art.matched_references,
                            "runner_factor": (
                                None
                                if art.runner_factor is None
                                else round(art.runner_factor, 4)
                            ),
                        }
                        for art in res.artefacts
                    ],
                    "top_changed_tests": [
                        {
                            "node_id": td.node_id,
                            "scaled_cpu_seconds": round(td.scaled_cpu_seconds, 2),
                            "baseline_cpu_seconds": (
                                None
                                if td.baseline_cpu_seconds is None
                                else round(td.baseline_cpu_seconds, 2)
                            ),
                            "delta_seconds": (
                                None
                                if td.delta_seconds is None
                                else round(td.delta_seconds, 2)
                            ),
                        }
                        for td in res.top_changed_tests
                    ],
                    "top_reference_increases": [
                        {
                            "node_id": td.node_id,
                            "scaled_cpu_seconds": round(td.scaled_cpu_seconds, 2),
                            "baseline_cpu_seconds": (
                                None
                                if td.baseline_cpu_seconds is None
                                else round(td.baseline_cpu_seconds, 2)
                            ),
                            "delta_seconds": (
                                None
                                if td.delta_seconds is None
                                else round(td.delta_seconds, 2)
                            ),
                        }
                        for td in res.top_reference_increases
                    ],
                    "reason": res.reason,
                    "reason_code": res.reason_code,
                    "reason_details": dict(res.reason_details),
                }
                for lane, res in self.lanes.items()
            },
            "marker_warnings": [
                {
                    "kind": warning.kind,
                    "node_id": warning.node_id,
                    "lane": warning.lane,
                    "raw_cpu_seconds": round(warning.raw_cpu_seconds, 2),
                    "scaled_cpu_seconds": round(warning.scaled_cpu_seconds, 2),
                    "runner_factor": round(warning.runner_factor, 4),
                    "current": warning.current,
                    "wanted": warning.wanted,
                    "source": warning.source,
                }
                for warning in self.marker_warnings
            ],
        }

    def message(self) -> str:
        """Return an ASCII plain-text summary of the duration guard result."""
        threshold_pct = self.max_regression * 100.0
        lines = [
            f"Test duration guard (Linux cp312, {self.phase}-phase CPU-s, "
            f"threshold +{threshold_pct:g}%, medium >= {self.medium_seconds:g} s, "
            f"slow >= {self.slow_seconds:g} s, band {self.band:g}):"
        ]
        for lane, res in self.lanes.items():
            if res.scaled_total_seconds is not None and res.delta_ratio is not None:
                tag = "[X]" if res.regressed else "[OK]"
                lines.append(
                    f"  {tag} {lane}: {res.scaled_total_seconds:.1f} s normalised vs "
                    f"{res.baseline_total_seconds:.1f} s baseline "
                    f"({_format_signed_pct(res.delta_ratio)}; raw {res.raw_total_seconds:.1f} s, "
                    f"runner {res.factor_summary()}, {res.measured_count} measured tests)"
                )
            else:
                reason = _sanitise_identifier(res.reason or "unavailable")
                lines.append(f"  [skip] {lane}: {res.status} ({reason})")

        if self.marker_warnings:
            lines.append(
                f"[X] {len(self.marker_warnings)} test(s) over cost-marker threshold "
                "after runner normalisation:"
            )
            lines.extend(warning.line() for warning in self.marker_warnings)
            lines.append(
                "Fix the marker or reduce test cost; thresholds and their reasoning "
                "are in .claude/rules/tests/patterns.md."
            )
        if self.incomplete_lanes:
            summary = "; ".join(
                f"{res.lane} ({res.status}: {_sanitise_identifier(res.reason or res.status)})"
                for res in self.incomplete_lanes
            )
            lines.append(
                f"[INCOMPLETE] comparison incomplete; could not compare: {summary}."
            )
        elif self.ok:
            lines.append(
                "[OK] normalised lane totals within threshold and no under-marked "
                "changed tests."
            )
        return "\n".join(lines)

    def _markdown_headline(self) -> str:
        """Build the one-line status headline for the Markdown summary."""
        if self.status == "ok":
            return (
                "Status: **OK** -- comparison completed; normalised lane totals "
                "within threshold and no under-marked changed tests."
            )
        missing_summary = "; ".join(
            f"`{res.lane}` ({res.status}: {_sanitise_markdown_prose(res.reason or res.status)})"
            for res in self.incomplete_lanes
        )
        if self.status == "incomplete":
            return (
                "Status: **INCOMPLETE** -- comparison incomplete; could not "
                f"compare: {missing_summary}."
            )
        if self.incomplete_lanes:
            return (
                "Status: **WARNING (comparison incomplete)** -- regression or "
                f"under-marked test detected; could not compare: {missing_summary}."
            )
        return (
            "Status: **WARNING** -- duration regression or under-marked "
            "changed test detected."
        )

    def markdown(self) -> str:
        """Render a GitHub Actions step summary in ASCII Markdown."""
        threshold_pct = self.max_regression * 100.0
        lines = [
            "## Test duration guard (report-only)",
            "",
            self._markdown_headline(),
            "",
            f"Runner-normalised `{self.phase}`-phase CPU seconds on Linux cp312 "
            f"(lane threshold `+{threshold_pct:g}%`; cost markers "
            f"`medium >= {self.medium_seconds:g} s`, `slow >= {self.slow_seconds:g} s`, "
            f"band `{self.band:g}`).",
            "",
            "| Lane | Status | Normalised CPU-s | Baseline CPU-s | Delta | Raw CPU-s | Runner factor | Measured |",
            "|---|---|---:|---:|---:|---:|---|---:|",
        ]
        for lane, res in self.lanes.items():
            if res.scaled_total_seconds is not None and res.delta_ratio is not None:
                status = "WARNING" if res.regressed else "OK"
                lines.append(
                    f"| `{lane}` | {status} | {res.scaled_total_seconds:.1f} | "
                    f"{res.baseline_total_seconds:.1f} | {_format_signed_pct(res.delta_ratio)} | "
                    f"{res.raw_total_seconds:.1f} | {res.factor_summary()} | "
                    f"{res.measured_count} |"
                )
            else:
                lines.append(
                    f"| `{lane}` | {res.status} | - | "
                    f"{res.baseline_total_seconds:.1f} | - | "
                    f"{res.raw_total_seconds:.1f} | n/a | {res.measured_count} |"
                )

        if self.incomplete_lanes:
            lines.append("")
            for res in self.incomplete_lanes:
                reason = _sanitise_markdown_prose(res.reason or res.status)
                lines.append(f"- `{res.lane}` ({res.status}): {reason}")

        if self.marker_warnings:
            lines.extend([
                "",
                "### Under-marked changed tests (runner-normalised)",
                "",
                "| Test | Lane | Normalised CPU-s | Raw CPU-s | Runner | Current | Wanted |",
                "|---|---|---:|---:|---:|---|---|",
            ])
            for warning in self.marker_warnings:
                node_md = _sanitise_markdown_code(warning.node_id)
                lines.append(
                    f"| `{node_md}` | `{warning.lane}` | "
                    f"{warning.scaled_cpu_seconds:.1f} | {warning.raw_cpu_seconds:.1f} | "
                    f"{warning.runner_factor:.2f}x | {warning.current or '-'} | "
                    f"{warning.wanted} |"
                )

        for lane, res in self.lanes.items():
            if not res.regressed:
                continue
            if res.top_changed_tests:
                lines.extend([
                    "",
                    f"### Heaviest changed tests in `{lane}`",
                    "",
                    "| Test | Normalised CPU-s | Baseline CPU-s | Delta |",
                    "|---|---:|---:|---:|",
                ])
                for item in res.top_changed_tests:
                    base_str = (
                        "-"
                        if item.baseline_cpu_seconds is None
                        else f"{item.baseline_cpu_seconds:.2f}"
                    )
                    delta_str = (
                        "new"
                        if item.delta_seconds is None
                        else f"{item.delta_seconds:+.2f}"
                    )
                    node_md = _sanitise_markdown_code(item.node_id)
                    lines.append(
                        f"| `{node_md}` | {item.scaled_cpu_seconds:.2f} | "
                        f"{base_str} | {delta_str} |"
                    )
            if res.top_reference_increases:
                lines.extend([
                    "",
                    f"### Largest reference-test increases in `{lane}`",
                    "",
                    "| Test | Normalised CPU-s | Baseline CPU-s | Delta |",
                    "|---|---:|---:|---:|",
                ])
                for item in res.top_reference_increases:
                    node_md = _sanitise_markdown_code(item.node_id)
                    lines.append(
                        f"| `{node_md}` | {item.scaled_cpu_seconds:.2f} | "
                        f"{item.baseline_cpu_seconds:.2f} | {item.delta_seconds:+.2f} |"
                    )

        lines.append("")
        return "\n".join(lines)

    def annotations(self) -> list[str]:
        """Return GitHub Actions ``::warning`` workflow commands for each finding."""
        commands: list[str] = []
        threshold_pct = self.max_regression * 100.0
        for res in self.regressed_lanes:
            assert res.scaled_total_seconds is not None and res.delta_ratio is not None
            msg = (
                f"{res.lane} lane normalised call CPU-s is "
                f"{_format_signed_pct(res.delta_ratio)} vs baseline "
                f"({res.scaled_total_seconds:.1f} s vs {res.baseline_total_seconds:.1f} s, "
                f"threshold +{threshold_pct:g}%, runner {res.factor_summary()}; "
                "report-only, see job summary)."
            )
            commands.append(_format_warning_annotation("Test duration regression", msg))
        for warning in self.marker_warnings:
            current = _sanitise_identifier(warning.current or "no cost marker")
            node_id = _sanitise_identifier(warning.node_id)
            wanted = _sanitise_identifier(warning.wanted)
            msg = (
                f"{node_id} ({warning.lane}): "
                f"{warning.scaled_cpu_seconds:.1f} normalised CPU-s "
                f"(raw {warning.raw_cpu_seconds:.1f} s, runner {warning.runner_factor:.2f}x; "
                f"{current} -> {wanted}). See .claude/rules/tests/patterns.md."
            )
            commands.append(_format_warning_annotation("Test cost marker", msg))
        for res in self.incomplete_lanes:
            reason = _sanitise_identifier(res.reason or res.status)
            msg = (
                f"{res.lane} lane comparison incomplete ({res.status}: "
                f"{reason}); report-only, see job summary."
            )
            commands.append(
                _format_warning_annotation("Test duration guard incomplete", msg)
            )
        return commands


def _neutralise_html_comments(text: str) -> str:
    """Break any HTML comment delimiters so untrusted text cannot split marker blocks."""
    out = str(text)
    while True:
        updated = out.replace("<!--", "&lt;!--").replace("-->", "--&gt;")
        if updated == out:
            return out
        out = updated


def _sanitise_identifier(text: str) -> str:
    """Replace control/non-ASCII characters and neutralise leading ``::``."""
    cleaned = "".join(ch if 0x20 <= ord(ch) <= 0x7E else " " for ch in str(text))
    stripped = cleaned.lstrip()
    if stripped.startswith("::"):
        prefix_len = len(cleaned) - len(stripped)
        cleaned = cleaned[:prefix_len] + ": :" + stripped[2:]
    return cleaned


def _sanitise_filename(name: str) -> str:
    """Sanitise an artefact filename for safe inclusion in diagnostics."""
    cleaned = " ".join(_sanitise_identifier(name).split())
    cleaned = _neutralise_html_comments(cleaned)
    cleaned = cleaned.replace("<", "&lt;").replace(">", "&gt;")
    for ch in ("`", "[", "]", "*", "_", "~", "|"):
        cleaned = cleaned.replace(ch, "")
    if len(cleaned) > _MAX_DISPLAY_FILENAME:
        cleaned = cleaned[: _MAX_DISPLAY_FILENAME - 3] + "..."
    return cleaned or "<unnamed>"


def _sanitise_markdown_code(text: str) -> str:
    """Sanitise untrusted text for safe placement inside a Markdown table code span."""
    return (
        _neutralise_html_comments(_sanitise_identifier(text))
        .replace("`", "'")
        .replace("|", "/")
    )


def _sanitise_markdown_prose(text: str) -> str:
    """Sanitise untrusted text for safe placement in Markdown prose."""
    cleaned = _neutralise_html_comments(_sanitise_identifier(text))
    return cleaned.replace("<", "&lt;").replace(">", "&gt;")


def _parse_nonneg_finite(value: Any) -> float | None:
    """Parse a finite non-negative float, returning None for malformed/NaN/Inf/negative/overflow."""
    if not isinstance(value, (int, float)) or isinstance(value, bool):
        return None
    try:
        parsed = float(value)
    except (OverflowError, ValueError, TypeError):
        return None
    if not math.isfinite(parsed) or parsed < 0.0:
        return None
    return parsed


def _parse_positive_finite(value: Any) -> float | None:
    """Parse a finite positive float, returning None for malformed/NaN/Inf/<=0/overflow."""
    parsed = _parse_nonneg_finite(value)
    if parsed is None or parsed <= 0.0:
        return None
    return parsed


def _is_finite_nonneg(value: Any) -> bool:
    """Return True when ``value`` is a finite, non-negative real number (not bool)."""
    return _parse_nonneg_finite(value) is not None


def _is_finite_positive(value: Any) -> bool:
    """Return True when ``value`` is a finite, strictly positive real number (not bool)."""
    return _parse_positive_finite(value) is not None


def _format_signed_pct(ratio: float) -> str:
    """Format a fractional delta as a signed percentage without ``-0.0%``."""
    pct = ratio * 100.0
    if abs(pct) < 0.05:
        pct = 0.0
    return f"{pct:+.1f}%"


def _escape_annotation(text: str) -> str:
    """Escape characters that delimit GitHub Actions workflow commands."""
    return text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def _format_warning_annotation(title: str, message: str) -> str:
    """Format one GitHub Actions ``::warning`` command."""
    return f"::warning title={_escape_annotation(title)}::{_escape_annotation(message)}"


def _tokens(text: str) -> set[str]:
    """Split a path component into lowercase alphanumeric tokens."""
    return {tok for tok in _TOKEN_SPLIT_RE.split(text.lower()) if tok}


def parse_shard_from_name(name: str) -> tuple[int, int] | None:
    """Extract ``(index, total)`` from a ``-shard-K-of-N`` artefact name, or None.

    Any name carrying multiple ``shard`` tokens, malformed shard labels (e.g.
    ``shard-x-of-2``, ``shard--1-of-2``, ``shard-1-of``, or a malformed token
    beside a valid one), or oversized digit strings returns ``_INVALID_SHARD``
    so callers reject it rather than treating it as unsharded or accepting an
    ambiguous substring.
    """
    markers = list(_SHARD_MARKER_RE.finditer(name))
    if not markers:
        return None
    matches = list(_SHARD_NAME_RE.finditer(name))
    if len(markers) != 1 or len(matches) != 1 or markers[0].start() != matches[0].start():
        return _INVALID_SHARD
    match = matches[0]
    idx_str, total_str = match.group(1), match.group(2)
    if len(idx_str) > 9 or len(total_str) > 9:
        return _INVALID_SHARD
    return int(idx_str), int(total_str)


def _parse_shard_from_path(path: Path) -> tuple[int, int] | None:
    """Extract ``(index, total)`` from an artefact's filename and parent directory.

    When both carry shard labels they must agree; any malformed or
    contradictory label returns ``_INVALID_SHARD``.
    """
    stem_shard = parse_shard_from_name(path.stem)
    parent_shard = parse_shard_from_name(path.parent.name)
    if stem_shard == _INVALID_SHARD or parent_shard == _INVALID_SHARD:
        return _INVALID_SHARD
    if stem_shard is not None and parent_shard is not None:
        if stem_shard != parent_shard:
            return _INVALID_SHARD
        return stem_shard
    return stem_shard if stem_shard is not None else parent_shard


def _session_metadata(
    metrics: dict[str, Any],
    tests: Sequence[cost_markers.TestCost],
) -> tuple[int | None, int | None, int | None, bool]:
    """Read optional pytest exit code, collected count, recorded outcome count, and validity flag."""
    result = metrics.get("result")
    exit_code: int | None = None
    recorded_count: int | None = None
    invalid = False
    if result is not None:
        if not isinstance(result, dict):
            invalid = True
        else:
            raw_exit = result.get("exit_code")
            if raw_exit is not None:
                if isinstance(raw_exit, int) and not isinstance(raw_exit, bool):
                    exit_code = raw_exit
                else:
                    invalid = True
            outcomes = result.get("outcomes")
            if outcomes is not None:
                if not isinstance(outcomes, dict):
                    invalid = True
                else:
                    total_outcomes = 0
                    for val in outcomes.values():
                        if (
                            not isinstance(val, int)
                            or isinstance(val, bool)
                            or val < 0
                            or val > MAX_MEASURED_TEST_COUNT
                        ):
                            invalid = True
                            break
                        total_outcomes += val
                        if total_outcomes > MAX_MEASURED_TEST_COUNT:
                            invalid = True
                            break
                    if not invalid:
                        recorded_count = total_outcomes

    inventory = metrics.get("inventory")
    collected_count: int | None = None
    if inventory is not None:
        if not isinstance(inventory, dict):
            invalid = True
        else:
            raw_collected = inventory.get("node_count")
            if raw_collected is not None:
                if (
                    isinstance(raw_collected, int)
                    and not isinstance(raw_collected, bool)
                    and 0 <= raw_collected <= MAX_MEASURED_TEST_COUNT
                ):
                    collected_count = raw_collected
                else:
                    invalid = True

    if not invalid and recorded_count is None and collected_count is not None:
        recorded_count = sum(1 for test in tests if test.outcome in _VALID_OUTCOMES)
    return exit_code, collected_count, recorded_count, invalid


def _validate_allowed_lanes(
    allowed_lanes: Iterable[str] | None,
) -> tuple[str, ...]:
    """Return validated lane names, defaulting to ``ALL_LANES``."""
    if allowed_lanes is None:
        return ALL_LANES
    lanes = tuple(allowed_lanes)
    unknown = [lane for lane in lanes if lane not in ALL_LANES]
    if unknown:
        raise ValueError(
            f"unknown lane(s) {', '.join(repr(lane) for lane in unknown)}; "
            f"expected subset of {ALL_LANES}"
        )
    return lanes


def _lane_from_stem(stem: str) -> tuple[str | None, bool]:
    """Extract a unique lane token from a filename stem, flagging ambiguity."""
    toks = _tokens(stem)
    matched = [lane for lane in ALL_LANES if lane in toks]
    if len(matched) == 1:
        return matched[0], False
    if len(matched) > 1:
        return None, True
    return None, False


def _lane_from_parent_name(name: str) -> tuple[str | None, bool]:
    """Extract a lane from an immediate parent directory name.

    Matches exact lane directories (``unit``), ``ci-metrics-<lane>*`` artefact
    directories, or ``<lane>-*`` / ``<lane>_*`` lane folders so unrelated
    temporary parent names (such as pytest's ``test_unit_...``) never collide.
    """
    lowered = name.lower()
    if lowered in ALL_LANES:
        return lowered, False
    is_lane_dir = lowered.startswith(_CI_METRICS_PREFIXES) or any(
        lowered.startswith((f"{lane}-", f"{lane}_")) for lane in ALL_LANES
    )
    if not is_lane_dir:
        return None, False
    matched = [lane for lane in ALL_LANES if lane in _tokens(lowered)]
    if len(matched) == 1:
        return matched[0], False
    return None, len(matched) > 1


def infer_lane(
    path: Path,
    tests: Sequence[cost_markers.TestCost],
    *,
    allowed_lanes: Iterable[str] | None = None,
) -> str | None:
    """Infer which CI test lane an artefact belongs to.

    Supports both schema 1 (``api``, ``physics``) and schema 2
    (``tooling``, ``unit``, ``native``, ``physics``). Checks the filename stem
    first, then the immediate parent directory name (never higher ancestors,
    so test temporary paths cannot collide), and finally falls back to test
    markers when the path carries no lane token. When ``allowed_lanes`` is
    given, marker fallback is scoped to those lanes so transitional
    dual-marked tests (for example ``unit`` plus compatibility ``api``)
    resolve against the active baseline schema.
    """
    candidate_lanes = _validate_allowed_lanes(allowed_lanes)

    stem_lane, stem_ambiguous = _lane_from_stem(path.stem)
    parent_lane, parent_ambiguous = _lane_from_parent_name(path.parent.name)
    if stem_ambiguous or parent_ambiguous:
        return None
    if stem_lane is not None and parent_lane is not None and stem_lane != parent_lane:
        return None
    if stem_lane is not None or parent_lane is not None:
        return stem_lane if stem_lane is not None else parent_lane

    measured = [test for test in tests if test.measured]
    if not measured:
        return None
    present = [
        lane
        for lane in candidate_lanes
        if any(lane in test.markers for test in measured)
    ]
    return present[0] if len(present) == 1 else None


def load_artefacts(
    paths: Iterable[Path],
    *,
    phase: str = cost_markers.DEFAULT_PHASE,
    skipped: list[str] | None = None,
    allowed_lanes: Iterable[str] | None = None,
) -> list[ArtefactTests]:
    """Load per-test measurements grouped by artefact file and inferred lane.

    Skips non-pytest JSON files (``-phases.json``, ``-wheel-job.json``, etc.)
    through ``check_cost_markers._pytest_payload`` while propagating
    ``UnsupportedMetricsSchema`` if a per-test artefact carries an unknown
    schema version. When ``allowed_lanes`` is provided, artefacts belonging to
    other lanes are skipped cleanly.
    """
    allowed_set = (
        frozenset(_validate_allowed_lanes(allowed_lanes))
        if allowed_lanes is not None
        else None
    )
    artefacts: list[ArtefactTests] = []
    existing_paths = [path for path in paths if path.exists()]
    for path in cost_markers.iter_artefact_paths(existing_paths):
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            payload = None
        metrics = cost_markers._pytest_payload(payload, path)
        if metrics is None:
            if skipped is not None:
                skipped.append(path.as_posix())
            continue
        tests: list[cost_markers.TestCost] = []
        invalid_measurements = 0
        for record in metrics["tests"]:
            if not isinstance(record, dict):
                invalid_measurements += 1
                continue
            node_id = record.get("node_id")
            cpu_map = record.get("cpu_seconds")
            wall_map = record.get("wall_seconds")
            if (
                not isinstance(node_id, str)
                or not node_id
                or not isinstance(cpu_map, dict)
                or not isinstance(wall_map, dict)
                or phase not in cpu_map
                or phase not in wall_map
            ):
                invalid_measurements += 1
                continue
            cpu_val = _parse_nonneg_finite(cpu_map.get(phase))
            wall_val = _parse_nonneg_finite(wall_map.get(phase))
            if cpu_val is None or wall_val is None:
                invalid_measurements += 1
                continue
            raw_markers = record.get("markers", [])
            if not isinstance(raw_markers, list):
                invalid_measurements += 1
                continue
            raw_reasons = record.get("marker_reasons")
            if raw_reasons is None:
                reasons: dict[str, Any] = {}
            elif isinstance(raw_reasons, dict):
                reasons = raw_reasons
            else:
                invalid_measurements += 1
                continue
            tests.append(
                cost_markers.TestCost(
                    node_id=node_id,
                    markers=tuple(sorted(str(m) for m in raw_markers)),
                    cpu_seconds=cpu_val,
                    wall_seconds=wall_val,
                    source=path.name,
                    outcome=str(record.get("outcome")),
                    marker_reasons=tuple(
                        sorted((str(k), str(v)) for k, v in reasons.items())
                    ),
                )
            )
        lane = infer_lane(path, tests, allowed_lanes=allowed_set)
        if lane is None or (allowed_set is not None and lane not in allowed_set):
            if skipped is not None:
                skipped.append(path.as_posix())
            continue
        exit_code, collected_count, recorded_count, meta_invalid = _session_metadata(
            metrics, tests
        )
        artefacts.append(
            ArtefactTests(
                source=path.name,
                lane=lane,
                tests=tuple(tests),
                shard=_parse_shard_from_path(path),
                exit_code=exit_code,
                collected_count=collected_count,
                recorded_outcome_count=recorded_count,
                invalid_measurement_count=invalid_measurements,
                session_metadata_invalid=meta_invalid,
            )
        )
    return artefacts


def _extract_raw_baseline_lanes(
    payload: dict[str, Any],
    version: int,
    safe_path: str,
) -> tuple[dict[str, Any], tuple[str, ...]]:
    """Validate and return the raw lane/cohort mapping and required lane order."""
    expected_lanes = SCHEMA_REQUIRED_LANES[version]
    if version == BASELINE_SCHEMA_VERSION:
        raw_lanes = payload.get("lanes")
    else:
        has_lanes = "lanes" in payload
        has_cohorts = "cohorts" in payload
        if has_lanes and has_cohorts:
            raise ValueError(
                f"{safe_path}: baseline schema {version} cannot define both "
                "'lanes' and 'cohorts'"
            )
        raw_lanes = payload.get("lanes") if has_lanes else payload.get("cohorts")
    if not isinstance(raw_lanes, dict) or not raw_lanes:
        raise ValueError(f"{safe_path}: baseline must define required lanes")
    missing_lanes = [lane for lane in expected_lanes if lane not in raw_lanes]
    if missing_lanes:
        raise ValueError(
            f"{safe_path}: baseline is missing required lane(s): "
            f"{', '.join(missing_lanes)}"
        )
    unknown_lanes = sorted(set(raw_lanes) - set(expected_lanes))
    if unknown_lanes:
        raise ValueError(
            f"{safe_path}: baseline schema {version} contains unexpected lane(s): "
            f"{', '.join(unknown_lanes)}"
        )
    return raw_lanes, expected_lanes


def load_baseline(path: Path) -> DurationBaseline:
    """Read and validate a committed duration baseline JSON file (schema 1 or 2)."""
    safe_path = _sanitise_identifier(path.as_posix())
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"{safe_path}: baseline root must be a JSON object")
    version = payload.get("schema_version")
    if (
        not isinstance(version, int)
        or isinstance(version, bool)
        or version not in SCHEMA_REQUIRED_LANES
    ):
        supported = ", ".join(str(v) for v in SUPPORTED_BASELINE_SCHEMA_VERSIONS)
        raise ValueError(
            f"{safe_path}: unsupported baseline schema {version!r} "
            f"(expected one of {supported})"
        )
    raw_lanes, expected_lanes = _extract_raw_baseline_lanes(payload, version, safe_path)
    lanes: dict[str, LaneBaseline] = {}
    for lane_name in expected_lanes:
        lane_data = raw_lanes[lane_name]
        if not isinstance(lane_data, dict):
            raise ValueError(f"{safe_path}: lane {lane_name!r} must be an object")
        total = _parse_positive_finite(lane_data.get("total_cpu_seconds"))
        if total is None:
            raise ValueError(
                f"{safe_path}: lane {lane_name!r} total_cpu_seconds must be a finite positive number"
            )
        raw_measured_count = lane_data.get("measured_test_count", 0)
        if (
            not isinstance(raw_measured_count, int)
            or isinstance(raw_measured_count, bool)
            or raw_measured_count < 0
            or raw_measured_count > MAX_MEASURED_TEST_COUNT
        ):
            raise ValueError(
                f"{safe_path}: lane {lane_name!r} measured_test_count must be an integer "
                f"in [0, {MAX_MEASURED_TEST_COUNT}]"
            )
        refs_raw = lane_data.get("reference_tests")
        if not isinstance(refs_raw, dict) or not refs_raw:
            raise ValueError(
                f"{safe_path}: lane {lane_name!r} must define reference_tests"
            )
        refs: dict[str, float] = {}
        for nid, val in refs_raw.items():
            parsed = _parse_positive_finite(val)
            if parsed is None:
                raise ValueError(
                    f"{safe_path}: lane {lane_name!r} reference_tests must be finite positive numbers"
                )
            refs[str(nid)] = parsed
        lanes[str(lane_name)] = LaneBaseline(
            lane=str(lane_name),
            total_cpu_seconds=total,
            measured_test_count=raw_measured_count,
            reference_tests=refs,
        )
    min_ref = _parse_positive_finite(
        payload.get("min_reference_seconds", DEFAULT_MIN_REFERENCE_SECONDS)
    )
    if min_ref is None:
        raise ValueError(
            f"{safe_path}: min_reference_seconds must be a finite positive number"
        )
    max_refs = payload.get("max_references_per_lane", DEFAULT_MAX_REFERENCES_PER_LANE)
    if not isinstance(max_refs, int) or isinstance(max_refs, bool) or max_refs < 1:
        raise ValueError(
            f"{safe_path}: max_references_per_lane must be a positive integer"
        )
    return DurationBaseline(
        schema_version=version,
        phase=str(payload.get("phase", cost_markers.DEFAULT_PHASE)),
        min_reference_seconds=min_ref,
        max_references_per_lane=max_refs,
        generated_from_runs=tuple(
            str(item) for item in payload.get("generated_from_runs", [])
        ),
        lanes=lanes,
    )


def _select_reference_nodes(
    median_costs: dict[str, float],
    *,
    min_reference_seconds: float,
    max_references_per_lane: int,
) -> list[str]:
    """Select a compact, shard-balanced set of reference tests deterministically.

    Ordering eligible tests by SHA-256 of the node id spreads references
    uniformly across test files and across SHA-256 shards so every shard
    retains enough reference tests even when a PR edits a large test file.
    """
    eligible = [
        node_id
        for node_id, cost in median_costs.items()
        if cost >= min_reference_seconds
    ]
    if not eligible:
        raise ValueError(
            f"no tests met min_reference_seconds={min_reference_seconds:g}"
        )
    hashed = sorted(
        eligible,
        key=lambda node_id: hashlib.sha256(node_id.encode("utf-8")).hexdigest(),
    )
    return sorted(hashed[:max_references_per_lane])


def _check_measurement_validity(
    lane_artefacts: Sequence[ArtefactTests],
) -> tuple[str, dict[str, Any], str] | None:
    """Return a structured reason if any artefact contains non-finite or negative timings."""
    bad_sources: list[str] = []
    for artefact in lane_artefacts:
        has_bad_test = any(
            not _is_finite_nonneg(test.cpu_seconds)
            or not _is_finite_nonneg(test.wall_seconds)
            for test in artefact.tests
        )
        if artefact.invalid_measurement_count > 0 or has_bad_test:
            bad_sources.append(_sanitise_filename(artefact.source))
    if not bad_sources:
        return None
    return (
        "invalid_measurements",
        {"artefact_count": len(bad_sources)},
        f"invalid or non-finite CPU measurements in {len(bad_sources)} artefact(s)",
    )


def _check_session_completeness(
    lane_artefacts: Sequence[ArtefactTests],
) -> tuple[str, dict[str, Any], str] | None:
    """Return a structured reason if any artefact records an incomplete pytest session."""
    for artefact in lane_artefacts:
        safe_src = _sanitise_filename(artefact.source)
        if artefact.session_metadata_invalid:
            return (
                "invalid_session_metadata",
                {},
                f"malformed session metadata in {safe_src}",
            )
        if (
            artefact.exit_code is not None
            and artefact.exit_code not in _COMPLETE_EXIT_CODES
        ):
            return (
                "incomplete_session_exit_code",
                {"exit_code": artefact.exit_code},
                f"incomplete session in {safe_src} "
                f"(pytest exit_code={artefact.exit_code})",
            )
        if (
            artefact.collected_count is not None
            and artefact.collected_count > 0
            and artefact.recorded_outcome_count is not None
            and artefact.recorded_outcome_count < artefact.collected_count
        ):
            return (
                "incomplete_session_outcomes",
                {
                    "recorded_outcome_count": artefact.recorded_outcome_count,
                    "collected_count": artefact.collected_count,
                },
                f"incomplete session in {safe_src} "
                f"({artefact.recorded_outcome_count}/{artefact.collected_count} "
                "collected tests recorded)",
            )
    return None


def _check_shard_completeness(
    lane_artefacts: Sequence[ArtefactTests],
) -> tuple[str, dict[str, Any], str] | None:
    """Return a structured reason if sharded artefacts are missing shards or inconsistent."""
    specs = [
        (art.source, art.shard or parse_shard_from_name(art.source))
        for art in lane_artefacts
    ]
    sharded = [(src, spec) for src, spec in specs if spec is not None]
    if not sharded:
        return None
    if len(sharded) != len(specs):
        return (
            "mixed_sharded_and_unsharded",
            {},
            "mixed sharded and unsharded artefacts in lane",
        )
    invalid: list[tuple[str, int, int]] = []
    for src, (index, total) in sharded:
        if total < 1 or total > MAX_SHARD_TOTAL or not 1 <= index <= total:
            invalid.append((_sanitise_filename(src), index, total))
    if invalid:
        first_src, first_idx, first_total = invalid[0]
        return (
            "invalid_shard_id",
            {"invalid_count": len(invalid)},
            f"invalid shard identifier {first_idx}-of-{first_total} in {first_src}",
        )
    totals = {total for _, (_, total) in sharded}
    if len(totals) > 1:
        sorted_totals = sorted(totals)
        return (
            "inconsistent_shard_totals",
            {"totals": sorted_totals},
            f"inconsistent shard totals ({', '.join(str(t) for t in sorted_totals)})",
        )
    expected_total = next(iter(totals))
    present = {index for _, (index, _) in sharded}
    if len(present) != len(sharded):
        return (
            "duplicate_shard_id",
            {
                "artefact_count": len(sharded),
                "distinct_count": len(present),
                "expected_total": expected_total,
            },
            f"duplicate shard identifier(s) (found {len(sharded)} artefacts for "
            f"{len(present)} distinct of {expected_total})",
        )
    if len(present) != expected_total:
        missing_count = expected_total - len(present)
        missing_sample: list[int] = []
        for idx in range(1, expected_total + 1):
            if idx not in present:
                missing_sample.append(idx)
                if len(missing_sample) >= MAX_REPORTED_MISSING_SHARDS:
                    break
        missing_str = ", ".join(str(idx) for idx in missing_sample)
        if missing_count > len(missing_sample):
            missing_str += f", ... (+{missing_count - len(missing_sample)} more)"
        return (
            "missing_shards",
            {
                "expected_total": expected_total,
                "present_count": len(present),
                "missing_count": missing_count,
                "missing_shards": missing_sample,
            },
            f"incomplete shard set: missing shard(s) {missing_str} of "
            f"{expected_total} (found {len(present)}/{expected_total})",
        )
    return None


def _check_artefact_and_shard_completeness(
    lane_artefacts: Sequence[ArtefactTests],
) -> tuple[str, dict[str, Any], str] | None:
    """Return a structured reason if any artefact session or shard set is incomplete."""
    return _check_session_completeness(lane_artefacts) or _check_shard_completeness(
        lane_artefacts
    )


def _build_lane_baseline(
    lane: str,
    run_artefacts_list: list[list[ArtefactTests]],
    *,
    min_reference_seconds: float,
    max_references_per_lane: int,
    min_reference_matches: int = DEFAULT_MIN_REFERENCE_MATCHES,
) -> LaneBaseline:
    """Compute reference tests and median normalised total for one lane.

    Normalises each artefact or shard independently with ``_scale_artefact``
    before aggregating across shards, matching the evaluation estimator so
    mixed-speed shards cannot bias the committed lane total.
    """
    per_run_raw: list[dict[str, float]] = []
    for run_artefacts in run_artefacts_list:
        invalid = _check_measurement_validity(run_artefacts)
        if invalid is not None:
            raise ValueError(f"lane {lane!r}: {invalid[2]}")
        incomplete = _check_artefact_and_shard_completeness(run_artefacts)
        if incomplete is not None:
            raise ValueError(f"lane {lane!r}: {incomplete[2]}")
        deduped_raw = cost_markers.largest_per_node(
            test for art in run_artefacts for test in art.tests if test.measured
        )
        per_run_raw.append({test.node_id: test.cpu_seconds for test in deduped_raw})

    common_nodes = sorted(set.intersection(*(set(run) for run in per_run_raw)))
    if not common_nodes:
        raise ValueError(f"lane {lane!r} has no measured tests common to all runs")
    median_costs = {
        node_id: statistics.median(run[node_id] for run in per_run_raw)
        for node_id in common_nodes
    }
    selected = _select_reference_nodes(
        median_costs,
        min_reference_seconds=min_reference_seconds,
        max_references_per_lane=max_references_per_lane,
    )
    reference_tests = {node_id: round(median_costs[node_id], 4) for node_id in selected}
    provisional = LaneBaseline(
        lane=lane,
        total_cpu_seconds=1.0,
        measured_test_count=0,
        reference_tests=reference_tests,
    )

    scaled_totals: list[float] = []
    measured_counts: list[int] = []
    for run_artefacts in run_artefacts_list:
        scaled_run: list[ScaledTest] = []
        for artefact in run_artefacts:
            scale, scaled, _unmeasured = _scale_artefact(
                artefact,
                provisional,
                set(),
                min_reference_matches=min_reference_matches,
            )
            if scale.runner_factor is None:
                raise ValueError(
                    f"lane {lane!r}: too few reference matches in "
                    f"{_sanitise_filename(artefact.source)} "
                    f"({scale.matched_references}/{min_reference_matches})"
                )
            scaled_run.extend(scaled)
        deduped_scaled = _dedupe_scaled(scaled_run)
        run_total = sum(item.scaled_cpu_seconds for item in deduped_scaled)
        if not _is_finite_positive(run_total):
            raise ValueError(
                f"lane {lane!r}: aggregated normalised CPU total is non-finite or non-positive"
            )
        scaled_totals.append(run_total)
        measured_counts.append(len(deduped_scaled))

    median_total = round(statistics.median(scaled_totals), 2)
    if not _is_finite_positive(median_total) or any(
        not _is_finite_positive(val) for val in reference_tests.values()
    ):
        raise ValueError(
            f"lane {lane!r}: computed baseline contains non-finite or non-positive values"
        )
    return LaneBaseline(
        lane=lane,
        total_cpu_seconds=median_total,
        measured_test_count=int(round(statistics.median(measured_counts))),
        reference_tests=reference_tests,
    )


def build_baseline(
    run_paths: Sequence[Path],
    *,
    phase: str = cost_markers.DEFAULT_PHASE,
    min_reference_seconds: float = DEFAULT_MIN_REFERENCE_SECONDS,
    max_references_per_lane: int = DEFAULT_MAX_REFERENCES_PER_LANE,
    min_reference_matches: int = DEFAULT_MIN_REFERENCE_MATCHES,
    schema_version: int = BASELINE_SCHEMA_VERSION,
) -> DurationBaseline:
    """Build a duration baseline from one or more run artefact directories.

    Each entry in ``run_paths`` represents one CI run and must provide
    complete, measured artefacts for every lane required by ``schema_version``
    (schema 1: ``api`` and ``physics``; schema 2: ``tooling``, ``unit``,
    ``native``, and ``physics``). Only the basename of each directory is
    recorded in ``generated_from_runs`` so local filesystem paths never enter
    the committed baseline.
    """
    if (
        not isinstance(schema_version, int)
        or isinstance(schema_version, bool)
        or schema_version not in SCHEMA_REQUIRED_LANES
    ):
        supported = ", ".join(str(v) for v in SUPPORTED_BASELINE_SCHEMA_VERSIONS)
        raise ValueError(
            f"unsupported baseline schema {schema_version!r} "
            f"(expected one of {supported})"
        )
    if not run_paths:
        raise ValueError("at least one run directory is required to build a baseline")
    if not _is_finite_positive(min_reference_seconds):
        raise ValueError("min_reference_seconds must be a finite positive number")
    if max_references_per_lane < 1:
        raise ValueError("max_references_per_lane must be at least 1")
    if min_reference_matches < 1:
        raise ValueError("min_reference_matches must be at least 1")

    required_lanes = SCHEMA_REQUIRED_LANES[schema_version]
    per_lane_runs: dict[str, list[list[ArtefactTests]]] = {
        lane: [] for lane in required_lanes
    }
    run_labels: list[str] = []

    for run_path in run_paths:
        run_label = _sanitise_filename(run_path.name)
        run_labels.append(run_path.name)
        artefacts = load_artefacts(
            [run_path], phase=phase, allowed_lanes=required_lanes
        )
        by_lane: dict[str, list[ArtefactTests]] = {lane: [] for lane in required_lanes}
        for artefact in artefacts:
            if artefact.lane in by_lane:
                by_lane[artefact.lane].append(artefact)
        missing_lanes = [
            lane
            for lane in required_lanes
            if not any(test.measured for art in by_lane[lane] for test in art.tests)
        ]
        if missing_lanes:
            raise ValueError(
                f"run {run_label!r} is missing measured artefacts for required "
                f"lane(s): {', '.join(missing_lanes)}"
            )
        for lane in required_lanes:
            per_lane_runs[lane].append(by_lane[lane])

    lane_baselines = {
        lane: _build_lane_baseline(
            lane,
            per_lane_runs[lane],
            min_reference_seconds=min_reference_seconds,
            max_references_per_lane=max_references_per_lane,
            min_reference_matches=min_reference_matches,
        )
        for lane in required_lanes
    }

    return DurationBaseline(
        schema_version=schema_version,
        phase=phase,
        min_reference_seconds=min_reference_seconds,
        max_references_per_lane=max_references_per_lane,
        generated_from_runs=tuple(sorted(run_labels)),
        lanes=lane_baselines,
    )


def baseline_to_dict(baseline: DurationBaseline) -> dict[str, Any]:
    """Serialise a ``DurationBaseline`` to a deterministic JSON dictionary."""
    schema_flag = (
        f" --baseline-schema {baseline.schema_version}"
        if baseline.schema_version != BASELINE_SCHEMA_VERSION
        else ""
    )
    return {
        "schema_version": baseline.schema_version,
        "description": (
            "Runner-normalised Linux cp312 call-phase CPU baseline for the "
            "report-only CI test-duration guard. Regenerate with: "
            f"python scripts/lint/check_test_duration.py --write-baseline{schema_flag} <run_dirs...>"
        ),
        "phase": baseline.phase,
        "metric": "cpu_seconds",
        "min_reference_seconds": baseline.min_reference_seconds,
        "max_references_per_lane": baseline.max_references_per_lane,
        "generated_from_runs": list(baseline.generated_from_runs),
        "lanes": {
            lane: {
                "total_cpu_seconds": lane_base.total_cpu_seconds,
                "measured_test_count": lane_base.measured_test_count,
                "reference_tests": dict(sorted(lane_base.reference_tests.items())),
            }
            for lane, lane_base in baseline.lanes.items()
        },
    }


def write_baseline(baseline: DurationBaseline, path: Path) -> None:
    """Write ``baseline`` as UTF-8, ASCII-only formatted JSON."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(
            baseline_to_dict(baseline),
            indent=2,
            ensure_ascii=True,
            allow_nan=False,
        )
        + "\n",
        encoding="utf-8",
    )


def _scale_artefact(
    artefact: ArtefactTests,
    lane_baseline: LaneBaseline,
    changed_files: set[str],
    *,
    min_reference_matches: int,
) -> tuple[ArtefactScale, list[ScaledTest], int]:
    """Normalise one artefact against unchanged reference tests in ``lane_baseline``."""
    measured = [test for test in artefact.tests if test.measured]
    unmeasured = len(artefact.tests) - len(measured)

    ratios: list[float] = []
    for test in measured:
        base_cpu = lane_baseline.reference_tests.get(test.node_id)
        if not _is_finite_positive(base_cpu) or not _is_finite_positive(
            test.cpu_seconds
        ):
            continue
        if cost_markers.node_file(test.node_id) in changed_files:
            continue
        ratio = test.cpu_seconds / base_cpu
        if _is_finite_positive(ratio):
            ratios.append(ratio)

    if len(ratios) < min_reference_matches:
        scale = ArtefactScale(
            source=artefact.source,
            lane=artefact.lane,
            measured_count=len(measured),
            matched_references=len(ratios),
            runner_factor=None,
        )
        return scale, [], unmeasured

    factor = statistics.median(ratios)
    if not _is_finite_positive(factor):
        scale = ArtefactScale(
            source=artefact.source,
            lane=artefact.lane,
            measured_count=len(measured),
            matched_references=len(ratios),
            runner_factor=None,
        )
        return scale, [], unmeasured

    scaled: list[ScaledTest] = []
    for test in measured:
        if not _is_finite_nonneg(test.cpu_seconds):
            scale = ArtefactScale(
                source=artefact.source,
                lane=artefact.lane,
                measured_count=len(measured),
                matched_references=len(ratios),
                runner_factor=None,
            )
            return scale, [], unmeasured
        scaled_cpu = test.cpu_seconds / factor
        if not _is_finite_nonneg(scaled_cpu):
            scale = ArtefactScale(
                source=artefact.source,
                lane=artefact.lane,
                measured_count=len(measured),
                matched_references=len(ratios),
                runner_factor=None,
            )
            return scale, [], unmeasured
        scaled.append(
            ScaledTest(
                test=test,
                lane=artefact.lane,
                runner_factor=factor,
                scaled_cpu_seconds=scaled_cpu,
            )
        )
    scale = ArtefactScale(
        source=artefact.source,
        lane=artefact.lane,
        measured_count=len(measured),
        matched_references=len(ratios),
        runner_factor=factor,
    )
    return scale, scaled, unmeasured


def _dedupe_scaled(scaled_tests: Iterable[ScaledTest]) -> list[ScaledTest]:
    """Keep one scaled measurement per node id: the largest scaled CPU cost."""
    best: dict[str, ScaledTest] = {}
    for item in scaled_tests:
        current = best.get(item.test.node_id)
        if current is None or item.scaled_cpu_seconds > current.scaled_cpu_seconds:
            best[item.test.node_id] = item
    return [best[node_id] for node_id in sorted(best)]


def _evaluate_lane(
    lane: str,
    lane_artefacts: list[ArtefactTests],
    lane_baseline: LaneBaseline,
    changed_files: set[str],
    *,
    max_regression: float,
    min_reference_matches: int,
    min_coverage_ratio: float,
) -> tuple[LaneResult, list[ScaledTest]]:
    """Normalise every artefact in ``lane``, aggregate across shards, and judge total."""
    if not _is_finite_positive(lane_baseline.total_cpu_seconds):
        return (
            LaneResult(
                lane=lane,
                status="unavailable",
                baseline_total_seconds=0.0,
                raw_total_seconds=0.0,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=0,
                unmeasured_count=0,
                artefacts=(),
                reason="invalid or non-finite baseline values",
                reason_code="invalid_baseline",
                reason_details={},
            ),
            [],
        )

    if not lane_artefacts:
        return (
            LaneResult(
                lane=lane,
                status="missing",
                baseline_total_seconds=lane_baseline.total_cpu_seconds,
                raw_total_seconds=0.0,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=0,
                unmeasured_count=0,
                artefacts=(),
                reason="no metrics artefacts found for lane",
                reason_code="no_artefacts",
                reason_details={},
            ),
            [],
        )

    invalid_measurements = _check_measurement_validity(lane_artefacts)
    if invalid_measurements is not None:
        code, details, reason = invalid_measurements
        valid_raw: dict[str, float] = {}
        unmeasured_total = 0
        for artefact in lane_artefacts:
            for test in artefact.tests:
                if test.measured and _is_finite_nonneg(test.cpu_seconds):
                    prev = valid_raw.get(test.node_id)
                    if prev is None or test.cpu_seconds > prev:
                        valid_raw[test.node_id] = test.cpu_seconds
                elif not test.measured:
                    unmeasured_total += 1
        raw_sum = sum(valid_raw.values())
        return (
            LaneResult(
                lane=lane,
                status="unavailable",
                baseline_total_seconds=lane_baseline.total_cpu_seconds,
                raw_total_seconds=raw_sum if math.isfinite(raw_sum) else 0.0,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=len(valid_raw),
                unmeasured_count=unmeasured_total,
                artefacts=(),
                reason=reason,
                reason_code=code,
                reason_details=details,
            ),
            [],
        )

    artefact_scales: list[ArtefactScale] = []
    scaled_all: list[ScaledTest] = []
    unmeasured_total = 0
    raw_by_node: dict[str, float] = {}

    for artefact in lane_artefacts:
        for test in artefact.tests:
            if test.measured:
                prev = raw_by_node.get(test.node_id)
                if prev is None or test.cpu_seconds > prev:
                    raw_by_node[test.node_id] = test.cpu_seconds
        scale, scaled, unmeasured = _scale_artefact(
            artefact,
            lane_baseline,
            changed_files,
            min_reference_matches=min_reference_matches,
        )
        artefact_scales.append(scale)
        scaled_all.extend(scaled)
        unmeasured_total += unmeasured

    raw_total = sum(raw_by_node.values())
    if not _is_finite_nonneg(raw_total):
        return (
            LaneResult(
                lane=lane,
                status="unavailable",
                baseline_total_seconds=lane_baseline.total_cpu_seconds,
                raw_total_seconds=0.0,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=len(raw_by_node),
                unmeasured_count=unmeasured_total,
                artefacts=tuple(artefact_scales),
                reason="invalid or non-finite CPU measurements in lane total",
                reason_code="invalid_measurements",
                reason_details={"artefact_count": len(lane_artefacts)},
            ),
            [],
        )
    measured_count = len(raw_by_node)

    incomplete = _check_artefact_and_shard_completeness(lane_artefacts)
    if incomplete is not None:
        code, details, reason = incomplete
        return (
            LaneResult(
                lane=lane,
                status="unavailable",
                baseline_total_seconds=lane_baseline.total_cpu_seconds,
                raw_total_seconds=raw_total,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=measured_count,
                unmeasured_count=unmeasured_total,
                artefacts=tuple(artefact_scales),
                reason=reason,
                reason_code=code,
                reason_details=details,
            ),
            _dedupe_scaled(scaled_all),
        )

    if (
        lane_baseline.measured_test_count > 0
        and min_coverage_ratio > 0.0
        and measured_count < lane_baseline.measured_test_count * min_coverage_ratio
    ):
        pct = round(min_coverage_ratio * 100.0, 2)
        return (
            LaneResult(
                lane=lane,
                status="unavailable",
                baseline_total_seconds=lane_baseline.total_cpu_seconds,
                raw_total_seconds=raw_total,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=measured_count,
                unmeasured_count=unmeasured_total,
                artefacts=tuple(artefact_scales),
                reason=(
                    f"incomplete lane coverage: {measured_count} measured tests is "
                    f"below {min_coverage_ratio * 100:g}% of baseline "
                    f"({lane_baseline.measured_test_count})"
                ),
                reason_code="low_coverage",
                reason_details={
                    "measured_count": measured_count,
                    "baseline_measured_count": lane_baseline.measured_test_count,
                    "min_coverage_percent": pct,
                },
            ),
            _dedupe_scaled(scaled_all),
        )

    missing_factors = [art for art in artefact_scales if art.runner_factor is None]
    if missing_factors:
        detail = ", ".join(
            f"{_sanitise_filename(art.source)} "
            f"({art.matched_references}/{min_reference_matches})"
            for art in missing_factors
        )
        return (
            LaneResult(
                lane=lane,
                status="unavailable",
                baseline_total_seconds=lane_baseline.total_cpu_seconds,
                raw_total_seconds=raw_total,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=measured_count,
                unmeasured_count=unmeasured_total,
                artefacts=tuple(artefact_scales),
                reason=f"too few unchanged reference matches in {detail}",
                reason_code="insufficient_references",
                reason_details={
                    "artefact_count": len(missing_factors),
                    "min_reference_matches": min_reference_matches,
                },
            ),
            _dedupe_scaled(scaled_all),
        )

    deduped = _dedupe_scaled(scaled_all)
    scaled_total = sum(item.scaled_cpu_seconds for item in deduped)
    if not _is_finite_nonneg(scaled_total):
        return (
            LaneResult(
                lane=lane,
                status="unavailable",
                baseline_total_seconds=lane_baseline.total_cpu_seconds,
                raw_total_seconds=raw_total,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=measured_count,
                unmeasured_count=unmeasured_total,
                artefacts=tuple(artefact_scales),
                reason="invalid or non-finite scaled CPU total",
                reason_code="invalid_measurements",
                reason_details={"artefact_count": len(lane_artefacts)},
            ),
            [],
        )
    delta_ratio = (
        scaled_total - lane_baseline.total_cpu_seconds
    ) / lane_baseline.total_cpu_seconds
    if not (math.isfinite(delta_ratio) and math.isfinite(delta_ratio * 100.0)):
        return (
            LaneResult(
                lane=lane,
                status="unavailable",
                baseline_total_seconds=lane_baseline.total_cpu_seconds,
                raw_total_seconds=raw_total,
                scaled_total_seconds=None,
                delta_ratio=None,
                regressed=False,
                measured_count=measured_count,
                unmeasured_count=unmeasured_total,
                artefacts=tuple(artefact_scales),
                reason="invalid or non-finite lane delta ratio",
                reason_code="invalid_measurements",
                reason_details={"artefact_count": len(lane_artefacts)},
            ),
            [],
        )
    regressed = delta_ratio > max_regression

    changed_deltas = [
        TestDelta(
            node_id=item.test.node_id,
            lane=lane,
            scaled_cpu_seconds=item.scaled_cpu_seconds,
            baseline_cpu_seconds=lane_baseline.reference_tests.get(item.test.node_id),
            delta_seconds=(
                None
                if item.test.node_id not in lane_baseline.reference_tests
                else item.scaled_cpu_seconds
                - lane_baseline.reference_tests[item.test.node_id]
            ),
        )
        for item in deduped
        if cost_markers.node_file(item.test.node_id) in changed_files
    ]
    changed_deltas.sort(key=lambda td: (-td.scaled_cpu_seconds, td.node_id))

    ref_increases = [
        TestDelta(
            node_id=item.test.node_id,
            lane=lane,
            scaled_cpu_seconds=item.scaled_cpu_seconds,
            baseline_cpu_seconds=lane_baseline.reference_tests[item.test.node_id],
            delta_seconds=item.scaled_cpu_seconds
            - lane_baseline.reference_tests[item.test.node_id],
        )
        for item in deduped
        if item.test.node_id in lane_baseline.reference_tests
        and item.scaled_cpu_seconds > lane_baseline.reference_tests[item.test.node_id]
    ]
    ref_increases.sort(
        key=lambda td: (-(td.delta_seconds or 0.0), td.node_id)
    )

    result = LaneResult(
        lane=lane,
        status="regressed" if regressed else "ok",
        baseline_total_seconds=lane_baseline.total_cpu_seconds,
        raw_total_seconds=raw_total,
        scaled_total_seconds=scaled_total,
        delta_ratio=delta_ratio,
        regressed=regressed,
        measured_count=len(deduped),
        unmeasured_count=unmeasured_total,
        artefacts=tuple(artefact_scales),
        top_changed_tests=tuple(changed_deltas[:TOP_CONTRIBUTORS_LIMIT]),
        top_reference_increases=tuple(ref_increases[:TOP_CONTRIBUTORS_LIMIT]),
    )
    return result, deduped


def check_durations(
    artefacts: Iterable[ArtefactTests],
    baseline: DurationBaseline,
    *,
    changed_test_files: Iterable[str] | None = None,
    max_regression: float = DEFAULT_MAX_REGRESSION,
    medium_seconds: float = cost_markers.MEDIUM_CPU_SECONDS,
    slow_seconds: float = cost_markers.SLOW_CPU_SECONDS,
    band: float = cost_markers.BAND,
    min_reference_matches: int = DEFAULT_MIN_REFERENCE_MATCHES,
    min_coverage_ratio: float = DEFAULT_MIN_COVERAGE_RATIO,
) -> DurationReport:
    """Evaluate normalised lane totals and cost markers of changed tests.

    Parameters
    ----------
    artefacts : Iterable[ArtefactTests]
        Loaded metrics artefacts across lanes and shards.
    baseline : DurationBaseline
        Committed duration baseline.
    changed_test_files : Iterable[str] | None, optional
        Repository-relative test files changed by the PR.  Pass an empty
        iterable when the PR changed no test files (lane totals are still
        checked; per-test marker checks examine zero tests).  Pass ``None``
        to judge every normalised test against the cost-marker thresholds.
    max_regression : float, optional
        Fractional lane-total increase above baseline that triggers a warning,
        by default ``0.10`` (+10%).
    medium_seconds : float, optional
        Medium cost-marker threshold in CPU seconds, by default ``10.0``.
    slow_seconds : float, optional
        Slow cost-marker threshold in CPU seconds, by default ``30.0``.
    band : float, optional
        Hysteresis factor reused from ``check_cost_markers.py``, by default ``2.0``.
    min_reference_matches : int, optional
        Minimum unchanged reference tests required per artefact to estimate a
        runner speed factor, by default ``5``.
    min_coverage_ratio : float, optional
        Minimum fraction of ``baseline.lanes[lane].measured_test_count``
        required before a lane total is judged, by default ``0.90``.

    Returns
    -------
    DurationReport
        Aggregated duration-guard findings across lanes and changed tests.
    """
    if not (
        _is_finite_nonneg(max_regression)
        and math.isfinite(float(max_regression) * 100.0)
    ):
        raise ValueError(
            "max_regression must be a finite non-negative number with finite percentage"
        )
    if not (
        _is_finite_positive(medium_seconds)
        and _is_finite_positive(slow_seconds)
        and medium_seconds < slow_seconds
    ):
        raise ValueError(
            "medium_seconds and slow_seconds must be finite positive numbers with medium < slow"
        )
    if not (_is_finite_positive(band) and band >= 1.0):
        raise ValueError("the band must be a finite number at least 1")
    if min_reference_matches < 1:
        raise ValueError("min_reference_matches must be at least 1")
    if not (_is_finite_nonneg(min_coverage_ratio) and min_coverage_ratio <= 1.0):
        raise ValueError("min_coverage_ratio must be a finite number in [0, 1]")

    artefact_list = list(artefacts)
    restricted_to: tuple[str, ...] | None = None
    changed_set: set[str] = set()
    if changed_test_files is not None:
        restricted_to = tuple(
            sorted({cost_markers._posix_path(p) for p in changed_test_files})
        )
        changed_set = set(restricted_to)

    by_lane: dict[str, list[ArtefactTests]] = {lane: [] for lane in baseline.lanes}
    for artefact in artefact_list:
        if artefact.lane in by_lane:
            by_lane[artefact.lane].append(artefact)

    lane_results: dict[str, LaneResult] = {}
    all_scaled: list[ScaledTest] = []
    for lane, lane_baseline in baseline.lanes.items():
        result, scaled_lane = _evaluate_lane(
            lane,
            by_lane[lane],
            lane_baseline,
            changed_set,
            max_regression=max_regression,
            min_reference_matches=min_reference_matches,
            min_coverage_ratio=min_coverage_ratio,
        )
        lane_results[lane] = result
        all_scaled.extend(scaled_lane)

    # Judge each test once across lanes/shards (keeping its largest scaled cost).
    pooled = _dedupe_scaled(all_scaled)
    if restricted_to is not None:
        judged = [
            item
            for item in pooled
            if cost_markers.node_file(item.test.node_id) in changed_set
        ]
    else:
        judged = pooled

    marker_warnings: list[MarkerWarning] = []
    for item in judged:
        normalised_cost = cost_markers.TestCost(
            node_id=item.test.node_id,
            markers=item.test.markers,
            cpu_seconds=item.scaled_cpu_seconds,
            wall_seconds=item.test.wall_seconds,
            source=item.test.source,
            outcome=item.test.outcome,
            marker_reasons=item.test.marker_reasons,
        )
        finding = cost_markers.classify(
            normalised_cost,
            medium_seconds=medium_seconds,
            slow_seconds=slow_seconds,
            band=band,
        )
        if finding is not None and finding.kind in UNDERMARKED_KINDS:
            marker_warnings.append(
                MarkerWarning(
                    kind=finding.kind,
                    node_id=item.test.node_id,
                    lane=item.lane,
                    raw_cpu_seconds=item.test.cpu_seconds,
                    scaled_cpu_seconds=item.scaled_cpu_seconds,
                    runner_factor=item.runner_factor,
                    current=item.test.cost_marker,
                    wanted=finding.wanted,
                    source=item.test.source,
                )
            )
    marker_warnings.sort(key=lambda w: (-w.scaled_cpu_seconds, w.node_id))

    return DurationReport(
        max_regression=max_regression,
        medium_seconds=medium_seconds,
        slow_seconds=slow_seconds,
        band=band,
        phase=baseline.phase,
        lanes=lane_results,
        marker_warnings=marker_warnings,
        checked_changed_tests=len(judged),
        restricted_to=restricted_to,
        sources=[
            artefact.source for artefact in artefact_list if artefact.lane in by_lane
        ],
    )


def _parse_args(argv: list[str] | None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "paths",
        nargs="+",
        type=Path,
        help=(
            "metrics JSON files or directories holding them (or one directory per "
            "run when used with --write-baseline)"
        ),
    )
    parser.add_argument(
        "--baseline",
        type=Path,
        default=DEFAULT_BASELINE_PATH,
        help=f"path to committed baseline JSON (default {DEFAULT_BASELINE_PATH.as_posix()})",
    )
    parser.add_argument(
        "--write-baseline",
        action="store_true",
        help="regenerate the baseline JSON from the given run directories",
    )
    parser.add_argument(
        "--baseline-schema",
        type=int,
        choices=SUPPORTED_BASELINE_SCHEMA_VERSIONS,
        default=BASELINE_SCHEMA_VERSION,
        help=(
            "baseline schema version when generating a baseline with "
            "--write-baseline (1 = api/physics, 2 = tooling/unit/native/physics; "
            f"default {BASELINE_SCHEMA_VERSION})"
        ),
    )
    parser.add_argument(
        "--min-reference-seconds",
        type=float,
        default=DEFAULT_MIN_REFERENCE_SECONDS,
        help=(
            "minimum median call CPU-s for a reference test when generating a "
            f"baseline (default {DEFAULT_MIN_REFERENCE_SECONDS:g})"
        ),
    )
    parser.add_argument(
        "--max-references",
        type=int,
        default=DEFAULT_MAX_REFERENCES_PER_LANE,
        help=(
            "maximum reference tests stored per lane in the baseline "
            f"(default {DEFAULT_MAX_REFERENCES_PER_LANE})"
        ),
    )
    parser.add_argument(
        "--max-regression",
        type=float,
        default=DEFAULT_MAX_REGRESSION,
        help=(
            "fractional lane-total increase above baseline that emits a warning "
            f"(default {DEFAULT_MAX_REGRESSION:g} = +{DEFAULT_MAX_REGRESSION * 100:g}%%)"
        ),
    )
    parser.add_argument(
        "--medium-seconds",
        type=float,
        default=cost_markers.MEDIUM_CPU_SECONDS,
        help=(
            "CPU seconds from which a test is medium "
            f"(default {cost_markers.MEDIUM_CPU_SECONDS:g})"
        ),
    )
    parser.add_argument(
        "--slow-seconds",
        type=float,
        default=cost_markers.SLOW_CPU_SECONDS,
        help=(
            "CPU seconds from which a test is slow "
            f"(default {cost_markers.SLOW_CPU_SECONDS:g})"
        ),
    )
    parser.add_argument(
        "--band",
        type=float,
        default=cost_markers.BAND,
        help=(
            "hysteresis factor reused from check_cost_markers.py "
            f"(default {cost_markers.BAND:g})"
        ),
    )
    parser.add_argument(
        "--min-reference-matches",
        type=int,
        default=DEFAULT_MIN_REFERENCE_MATCHES,
        help=(
            "minimum unchanged reference tests required per artefact to compute "
            f"a runner factor (default {DEFAULT_MIN_REFERENCE_MATCHES})"
        ),
    )
    parser.add_argument(
        "--min-coverage-ratio",
        type=float,
        default=DEFAULT_MIN_COVERAGE_RATIO,
        help=(
            "minimum fraction of baseline measured_test_count required to judge "
            f"a lane total (default {DEFAULT_MIN_COVERAGE_RATIO:g})"
        ),
    )
    parser.add_argument(
        "--test-file",
        dest="test_files",
        action="append",
        metavar="PATH",
        help=(
            "restrict per-test cost-marker checks and reference exclusions to "
            "these changed test files (repeat for several files)"
        ),
    )
    parser.add_argument(
        "--no-changed-test-files",
        action="store_true",
        help=(
            "indicate the PR changed no Python test files (check lane totals "
            "without judging unrelated pre-existing tests for cost markers)"
        ),
    )
    parser.add_argument(
        "--markdown",
        type=Path,
        help="append a Markdown report to this file (e.g. $GITHUB_STEP_SUMMARY)",
    )
    parser.add_argument(
        "--json",
        dest="json_path",
        type=Path,
        help="write the structured report as JSON to this file",
    )
    parser.add_argument(
        "--annotations",
        action="store_true",
        help="emit GitHub Actions ::warning annotations for each finding",
    )
    return parser.parse_args(argv)


def _emit_cli_output(
    report: DurationReport,
    skipped: Sequence[str],
    *,
    emit_annotations: bool,
) -> None:
    """Print sanitised plain-text output, guarded by ``::stop-commands::`` when emitting annotations."""
    stop_token = (
        f"end-duration-guard-{secrets.token_hex(12)}" if emit_annotations else None
    )
    if stop_token is not None:
        print(f"::stop-commands::{stop_token}")
    for path_str in skipped:
        print(
            f"[skip] {_sanitise_identifier(path_str)}: not a lane pytest metrics artefact"
        )
    print(report.message())
    if stop_token is not None:
        print(f"::{stop_token}::")
        for command in report.annotations():
            print(command)


def _emit_cli_error(prefix: str, exc: BaseException, *, emit_annotations: bool) -> None:
    """Print a sanitised error line, guarded by ``::stop-commands::`` when emitting annotations."""
    stop_token = (
        f"end-duration-guard-{secrets.token_hex(12)}" if emit_annotations else None
    )
    if stop_token is not None:
        print(f"::stop-commands::{stop_token}")
    print(f"[X] {prefix}{_sanitise_identifier(str(exc))}")
    if stop_token is not None:
        print(f"::{stop_token}::")


def main(argv: list[str] | None = None) -> int:
    """Run the CLI to check or regenerate test-duration baselines."""
    args = _parse_args(argv)
    if args.write_baseline:
        try:
            baseline = build_baseline(
                args.paths,
                min_reference_seconds=args.min_reference_seconds,
                max_references_per_lane=args.max_references,
                min_reference_matches=args.min_reference_matches,
                schema_version=args.baseline_schema,
            )
            write_baseline(baseline, args.baseline)
        except (OSError, ValueError, cost_markers.UnsupportedMetricsSchema) as exc:
            _emit_cli_error("", exc, emit_annotations=args.annotations)
            return 2
        summary = ", ".join(
            f"{lane}: {lb.total_cpu_seconds:.1f} s ({len(lb.reference_tests)} refs)"
            for lane, lb in baseline.lanes.items()
        )
        print(
            f"[OK] wrote {_sanitise_identifier(args.baseline.as_posix())} from "
            f"{len(baseline.generated_from_runs)} run(s): {summary}"
        )
        return 0

    try:
        baseline = load_baseline(args.baseline)
    except (OSError, ValueError) as exc:
        _emit_cli_error(
            "failed to load baseline: ", exc, emit_annotations=args.annotations
        )
        return 2

    skipped: list[str] = []
    try:
        artefacts = load_artefacts(
            args.paths,
            phase=baseline.phase,
            skipped=skipped,
            allowed_lanes=tuple(baseline.lanes),
        )
    except cost_markers.UnsupportedMetricsSchema as exc:
        _emit_cli_error("", exc, emit_annotations=args.annotations)
        return 2

    changed_test_files: list[str] | None
    if args.test_files:
        changed_test_files = list(args.test_files)
    elif args.no_changed_test_files:
        changed_test_files = []
    else:
        changed_test_files = None

    report = check_durations(
        artefacts,
        baseline,
        changed_test_files=changed_test_files,
        max_regression=args.max_regression,
        medium_seconds=args.medium_seconds,
        slow_seconds=args.slow_seconds,
        band=args.band,
        min_reference_matches=args.min_reference_matches,
        min_coverage_ratio=args.min_coverage_ratio,
    )
    _emit_cli_output(report, skipped, emit_annotations=args.annotations)
    if args.markdown:
        args.markdown.parent.mkdir(parents=True, exist_ok=True)
        with args.markdown.open("a", encoding="utf-8") as handle:
            handle.write(report.markdown())
    if args.json_path:
        args.json_path.parent.mkdir(parents=True, exist_ok=True)
        args.json_path.write_text(
            json.dumps(
                report.to_dict(), indent=2, ensure_ascii=True, allow_nan=False
            )
            + "\n",
            encoding="utf-8",
        )
    return 0 if report.ok else 1


if __name__ == "__main__":
    sys.exit(main())
