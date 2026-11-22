#!/usr/bin/env python3
"""Report-only CI guard for Python test-lane durations and cost markers.

GitHub-hosted Linux runners are bimodal (roughly 1.57x on the API lane and up
to ~1.95x on the physics lane for the same commit), so raw lane totals cannot
detect a +10% slowdown without flooding pull requests with false positives.

This script normalises each metrics artefact independently by comparing
unchanged reference tests against a committed baseline
(``scripts/lint/test_duration_baseline.json``).  Normalising per artefact
handles both mixed fast/slow runners between the API and physics jobs and
mixed runners across API shards (2-way or 4-way SHA-256 sharding via
``scripts/suews/pytest_shard.py``).  After normalisation, shards of the same
lane are aggregated so each test is judged once, whichever shard ran it.

Two conditions are reported (both advisory; CI runs this job with
``continue-on-error`` and never adds it to ``pr-gate``):

1. Any new or changed test whose runner-normalised ``call``-phase CPU seconds
   exceed the repository's cost-marker threshold without the matching marker
   (reusing ``MEDIUM_CPU_SECONDS``, ``SLOW_CPU_SECONDS``, and ``BAND`` from
   ``scripts/lint/check_cost_markers.py``; see ``.claude/rules/tests/patterns.md``).
2. Any lane whose runner-normalised ``call``-phase CPU total exceeds the
   committed baseline by more than ``--max-regression`` (default 10%).

Regenerating the baseline
-------------------------
Regenerate ``scripts/lint/test_duration_baseline.json`` after deliberate suite
growth (e.g. a new test module) or after hotspot speed-ups land on ``master``
so the reference stays close to current ``master`` behaviour.  Download the
Linux cp312 API and physics artefacts from 6 or more recent green ``standard``
runs and pass their run directories to ``--write-baseline``::

    for id in <run_id_1> <run_id_2> ...; do
      mkdir -p ".context/runs/$id/api" ".context/runs/$id/physics"
      gh run download "$id" -p "ci-metrics-api-cp312-manylinux-x86_64*" -D ".context/runs/$id/api"
      gh run download "$id" -n "ci-metrics-physics-cp312-manylinux-x86_64" -D ".context/runs/$id/physics"
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
from dataclasses import dataclass
import hashlib
import json
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
DEFAULT_BASELINE_PATH = Path("scripts") / "lint" / "test_duration_baseline.json"
LANES = ("api", "physics")
DEFAULT_MAX_REGRESSION = 0.10
DEFAULT_MIN_REFERENCE_SECONDS = 1.0
DEFAULT_MAX_REFERENCES_PER_LANE = 200
DEFAULT_MIN_REFERENCE_MATCHES = 5
DEFAULT_MIN_COVERAGE_RATIO = 0.90
UNDERMARKED_KINDS = frozenset({"unmarked-over-medium", "medium-over-slow"})
INCOMPLETE_LANE_STATUSES = frozenset({"missing", "unavailable"})
TOP_CONTRIBUTORS_LIMIT = 10
_TOKEN_SPLIT_RE = re.compile(r"[^a-z0-9]+")
_SHARD_NAME_RE = re.compile(
    r"(?:^|[-_])shard[-_](\d+)[-_]of[-_](\d+)(?:$|[-_.])",
    re.IGNORECASE,
)
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
            f"`{res.lane}` ({res.status}: {_sanitise_identifier(res.reason or res.status)})"
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
                reason = _sanitise_identifier(res.reason or res.status)
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


def _sanitise_identifier(text: str) -> str:
    """Replace control/non-ASCII characters and neutralise leading ``::``."""
    cleaned = "".join(ch if 0x20 <= ord(ch) <= 0x7E else " " for ch in str(text))
    stripped = cleaned.lstrip()
    if stripped.startswith("::"):
        prefix_len = len(cleaned) - len(stripped)
        cleaned = cleaned[:prefix_len] + ": :" + stripped[2:]
    return cleaned


def _sanitise_markdown_code(text: str) -> str:
    """Sanitise untrusted text for safe placement inside a Markdown table code span."""
    return _sanitise_identifier(text).replace("`", "'").replace("|", "/")


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
    """Extract ``(index, total)`` from a ``-shard-K-of-N`` artefact name, or None."""
    match = _SHARD_NAME_RE.search(name)
    if match is None:
        return None
    return int(match.group(1)), int(match.group(2))


def _parse_shard_from_path(path: Path) -> tuple[int, int] | None:
    """Extract ``(index, total)`` from an artefact's filename or parent directory."""
    for candidate in (path.stem, path.parent.name):
        parsed = parse_shard_from_name(candidate)
        if parsed is not None:
            return parsed
    return None


def _session_metadata(
    metrics: dict[str, Any],
    tests: Sequence[cost_markers.TestCost],
) -> tuple[int | None, int | None, int | None]:
    """Read optional pytest exit code, collected count, and recorded outcome count."""
    result = metrics.get("result")
    exit_code: int | None = None
    recorded_count: int | None = None
    if isinstance(result, dict):
        raw_exit = result.get("exit_code")
        if isinstance(raw_exit, int) and not isinstance(raw_exit, bool):
            exit_code = raw_exit
        outcomes = result.get("outcomes")
        if isinstance(outcomes, dict):
            recorded_count = sum(
                int(val)
                for val in outcomes.values()
                if isinstance(val, int) and not isinstance(val, bool)
            )

    inventory = metrics.get("inventory")
    collected_count: int | None = None
    if isinstance(inventory, dict):
        raw_collected = inventory.get("node_count")
        if isinstance(raw_collected, int) and not isinstance(raw_collected, bool):
            collected_count = raw_collected

    if recorded_count is None and collected_count is not None:
        recorded_count = sum(1 for test in tests if test.outcome in _VALID_OUTCOMES)
    return exit_code, collected_count, recorded_count


def infer_lane(
    path: Path,
    tests: Sequence[cost_markers.TestCost],
) -> str | None:
    """Infer whether an artefact belongs to ``api`` or ``physics``.

    Checks the filename stem first, then the immediate parent directory name
    (never higher ancestors, so test temporary paths cannot collide), and
    finally falls back to test markers when the filename carries no lane token.
    """
    for candidate in (path.stem, path.parent.name):
        toks = _tokens(candidate)
        has_api = "api" in toks
        has_physics = "physics" in toks
        if has_api and not has_physics:
            return "api"
        if has_physics and not has_api:
            return "physics"

    measured = [test for test in tests if test.measured]
    if not measured:
        return None
    has_api_marker = any("api" in test.markers for test in measured)
    has_physics_marker = any("physics" in test.markers for test in measured)
    if has_api_marker and not has_physics_marker:
        return "api"
    if has_physics_marker and not has_api_marker:
        return "physics"
    return None


def load_artefacts(
    paths: Iterable[Path],
    *,
    phase: str = cost_markers.DEFAULT_PHASE,
    skipped: list[str] | None = None,
) -> list[ArtefactTests]:
    """Load per-test measurements grouped by artefact file and inferred lane.

    Skips non-pytest JSON files (``-phases.json``, ``-wheel-job.json``, etc.)
    through ``check_cost_markers._pytest_payload`` while propagating
    ``UnsupportedMetricsSchema`` if a per-test artefact carries an unknown
    schema version.
    """
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
        for record in metrics["tests"]:
            reasons = record.get("marker_reasons") or {}
            tests.append(
                cost_markers.TestCost(
                    node_id=str(record["node_id"]),
                    markers=tuple(sorted(str(m) for m in record.get("markers", []))),
                    cpu_seconds=float(record["cpu_seconds"][phase]),
                    wall_seconds=float(record["wall_seconds"][phase]),
                    source=path.name,
                    outcome=str(record.get("outcome")),
                    marker_reasons=tuple(
                        sorted((str(k), str(v)) for k, v in reasons.items())
                    ),
                )
            )
        lane = infer_lane(path, tests)
        if lane is None:
            if skipped is not None:
                skipped.append(path.as_posix())
            continue
        exit_code, collected_count, recorded_count = _session_metadata(metrics, tests)
        artefacts.append(
            ArtefactTests(
                source=path.name,
                lane=lane,
                tests=tuple(tests),
                shard=_parse_shard_from_path(path),
                exit_code=exit_code,
                collected_count=collected_count,
                recorded_outcome_count=recorded_count,
            )
        )
    return artefacts


def load_baseline(path: Path) -> DurationBaseline:
    """Read and validate a committed duration baseline JSON file."""
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"{path.as_posix()}: baseline root must be a JSON object")
    version = payload.get("schema_version")
    if version != BASELINE_SCHEMA_VERSION:
        raise ValueError(
            f"{path.as_posix()}: unsupported baseline schema {version!r} "
            f"(expected {BASELINE_SCHEMA_VERSION})"
        )
    raw_lanes = payload.get("lanes")
    if not isinstance(raw_lanes, dict) or not raw_lanes:
        raise ValueError(f"{path.as_posix()}: baseline must define at least one lane")
    lanes: dict[str, LaneBaseline] = {}
    for lane_name, lane_data in raw_lanes.items():
        if not isinstance(lane_data, dict):
            raise ValueError(f"{path.as_posix()}: lane {lane_name!r} must be an object")
        total = float(lane_data["total_cpu_seconds"])
        if total <= 0.0:
            raise ValueError(
                f"{path.as_posix()}: lane {lane_name!r} total_cpu_seconds must be positive"
            )
        refs_raw = lane_data.get("reference_tests")
        if not isinstance(refs_raw, dict) or not refs_raw:
            raise ValueError(
                f"{path.as_posix()}: lane {lane_name!r} must define reference_tests"
            )
        refs = {str(nid): float(val) for nid, val in refs_raw.items()}
        if any(val <= 0.0 for val in refs.values()):
            raise ValueError(
                f"{path.as_posix()}: lane {lane_name!r} reference_tests must be positive"
            )
        lanes[str(lane_name)] = LaneBaseline(
            lane=str(lane_name),
            total_cpu_seconds=total,
            measured_test_count=int(lane_data.get("measured_test_count", 0)),
            reference_tests=refs,
        )
    return DurationBaseline(
        schema_version=BASELINE_SCHEMA_VERSION,
        phase=str(payload.get("phase", cost_markers.DEFAULT_PHASE)),
        min_reference_seconds=float(
            payload.get("min_reference_seconds", DEFAULT_MIN_REFERENCE_SECONDS)
        ),
        max_references_per_lane=int(
            payload.get("max_references_per_lane", DEFAULT_MAX_REFERENCES_PER_LANE)
        ),
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


def _check_session_completeness(
    lane_artefacts: Sequence[ArtefactTests],
) -> str | None:
    """Return a reason string if any artefact records an incomplete pytest session."""
    for artefact in lane_artefacts:
        if (
            artefact.exit_code is not None
            and artefact.exit_code not in _COMPLETE_EXIT_CODES
        ):
            return (
                f"incomplete session in {artefact.source} "
                f"(pytest exit_code={artefact.exit_code})"
            )
        if (
            artefact.collected_count is not None
            and artefact.collected_count > 0
            and artefact.recorded_outcome_count is not None
            and artefact.recorded_outcome_count < artefact.collected_count
        ):
            return (
                f"incomplete session in {artefact.source} "
                f"({artefact.recorded_outcome_count}/{artefact.collected_count} "
                "collected tests recorded)"
            )
    return None


def _check_shard_completeness(
    lane_artefacts: Sequence[ArtefactTests],
) -> str | None:
    """Return a reason string if sharded artefacts are missing shards or inconsistent."""
    specs = [
        (art.source, art.shard or parse_shard_from_name(art.source))
        for art in lane_artefacts
    ]
    sharded = [(src, spec) for src, spec in specs if spec is not None]
    if not sharded:
        return None
    if len(sharded) != len(specs):
        return "mixed sharded and unsharded artefacts in lane"
    for src, (index, total) in sharded:
        if total < 1 or not 1 <= index <= total:
            return f"invalid shard identifier {index}-of-{total} in {src}"
    totals = {total for _, (_, total) in sharded}
    if len(totals) > 1:
        return (
            f"inconsistent shard totals ({', '.join(str(t) for t in sorted(totals))})"
        )
    expected_total = next(iter(totals))
    present = {index for _, (index, _) in sharded}
    missing = sorted(set(range(1, expected_total + 1)) - present)
    if missing:
        missing_str = ", ".join(str(idx) for idx in missing)
        return (
            f"incomplete shard set: missing shard(s) {missing_str} of "
            f"{expected_total} (found {len(present)}/{expected_total})"
        )
    return None


def _check_artefact_and_shard_completeness(
    lane_artefacts: Sequence[ArtefactTests],
) -> str | None:
    """Return a reason string if any artefact session or shard set is incomplete."""
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
        incomplete = _check_artefact_and_shard_completeness(run_artefacts)
        if incomplete is not None:
            raise ValueError(f"lane {lane!r}: {incomplete}")
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
                    f"lane {lane!r}: too few reference matches in {artefact.source} "
                    f"({scale.matched_references}/{min_reference_matches})"
                )
            scaled_run.extend(scaled)
        deduped_scaled = _dedupe_scaled(scaled_run)
        scaled_totals.append(sum(item.scaled_cpu_seconds for item in deduped_scaled))
        measured_counts.append(len(deduped_scaled))

    return LaneBaseline(
        lane=lane,
        total_cpu_seconds=round(statistics.median(scaled_totals), 2),
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
) -> DurationBaseline:
    """Build a duration baseline from one or more run artefact directories.

    Each entry in ``run_paths`` represents one CI run (and may contain
    unsharded or sharded API artefacts alongside physics artefacts).  Only the
    basename of each directory is recorded in ``generated_from_runs`` so local
    filesystem paths never enter the committed baseline.
    """
    if not run_paths:
        raise ValueError("at least one run directory is required to build a baseline")
    if min_reference_seconds <= 0.0:
        raise ValueError("min_reference_seconds must be positive")
    if max_references_per_lane < 1:
        raise ValueError("max_references_per_lane must be at least 1")
    if min_reference_matches < 1:
        raise ValueError("min_reference_matches must be at least 1")

    per_lane_runs: dict[str, list[list[ArtefactTests]]] = {lane: [] for lane in LANES}
    run_labels: list[str] = []

    for run_path in run_paths:
        run_labels.append(run_path.name)
        artefacts = load_artefacts([run_path], phase=phase)
        by_lane: dict[str, list[ArtefactTests]] = {}
        for artefact in artefacts:
            if artefact.lane in per_lane_runs:
                by_lane.setdefault(artefact.lane, []).append(artefact)
        for lane, lane_arts in by_lane.items():
            if any(test.measured for art in lane_arts for test in art.tests):
                per_lane_runs[lane].append(lane_arts)

    lane_baselines = {
        lane: _build_lane_baseline(
            lane,
            runs,
            min_reference_seconds=min_reference_seconds,
            max_references_per_lane=max_references_per_lane,
            min_reference_matches=min_reference_matches,
        )
        for lane, runs in per_lane_runs.items()
        if runs
    }
    if not lane_baselines:
        raise ValueError("no API or physics metrics artefacts found in the given runs")

    return DurationBaseline(
        schema_version=BASELINE_SCHEMA_VERSION,
        phase=phase,
        min_reference_seconds=min_reference_seconds,
        max_references_per_lane=max_references_per_lane,
        generated_from_runs=tuple(sorted(run_labels)),
        lanes=lane_baselines,
    )


def baseline_to_dict(baseline: DurationBaseline) -> dict[str, Any]:
    """Serialise a ``DurationBaseline`` to a deterministic JSON dictionary."""
    return {
        "schema_version": baseline.schema_version,
        "description": (
            "Runner-normalised Linux cp312 call-phase CPU baseline for the "
            "report-only CI test-duration guard. Regenerate with: "
            "python scripts/lint/check_test_duration.py --write-baseline <run_dirs...>"
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
        json.dumps(baseline_to_dict(baseline), indent=2, ensure_ascii=True) + "\n",
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
        if base_cpu is None or base_cpu <= 0.0 or test.cpu_seconds <= 0.0:
            continue
        if cost_markers.node_file(test.node_id) in changed_files:
            continue
        ratios.append(test.cpu_seconds / base_cpu)

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
    scale = ArtefactScale(
        source=artefact.source,
        lane=artefact.lane,
        measured_count=len(measured),
        matched_references=len(ratios),
        runner_factor=factor,
    )
    scaled = [
        ScaledTest(
            test=test,
            lane=artefact.lane,
            runner_factor=factor,
            scaled_cpu_seconds=test.cpu_seconds / factor,
        )
        for test in measured
    ]
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
    measured_count = len(raw_by_node)

    incomplete_reason = _check_artefact_and_shard_completeness(lane_artefacts)
    if incomplete_reason is not None:
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
                reason=incomplete_reason,
            ),
            _dedupe_scaled(scaled_all),
        )

    if (
        lane_baseline.measured_test_count > 0
        and min_coverage_ratio > 0.0
        and measured_count < lane_baseline.measured_test_count * min_coverage_ratio
    ):
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
            ),
            _dedupe_scaled(scaled_all),
        )

    missing_factors = [art for art in artefact_scales if art.runner_factor is None]
    if missing_factors:
        detail = ", ".join(
            f"{art.source} ({art.matched_references}/{min_reference_matches})"
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
            ),
            _dedupe_scaled(scaled_all),
        )

    deduped = _dedupe_scaled(scaled_all)
    scaled_total = sum(item.scaled_cpu_seconds for item in deduped)
    delta_ratio = (
        scaled_total - lane_baseline.total_cpu_seconds
    ) / lane_baseline.total_cpu_seconds
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
    if max_regression < 0.0:
        raise ValueError("max_regression must be non-negative")
    if not medium_seconds < slow_seconds:
        raise ValueError("the medium threshold must be below the slow threshold")
    if band < 1.0:
        raise ValueError("the band must be at least 1")
    if min_reference_matches < 1:
        raise ValueError("min_reference_matches must be at least 1")
    if not 0.0 <= min_coverage_ratio <= 1.0:
        raise ValueError("min_coverage_ratio must lie in [0, 1]")

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
        sources=[artefact.source for artefact in artefact_list],
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
            )
        except (OSError, ValueError, cost_markers.UnsupportedMetricsSchema) as exc:
            print(f"[X] {exc}")
            return 2
        write_baseline(baseline, args.baseline)
        summary = ", ".join(
            f"{lane}: {lb.total_cpu_seconds:.1f} s ({len(lb.reference_tests)} refs)"
            for lane, lb in baseline.lanes.items()
        )
        print(
            f"[OK] wrote {args.baseline.as_posix()} from {len(baseline.generated_from_runs)} "
            f"run(s): {summary}"
        )
        return 0

    try:
        baseline = load_baseline(args.baseline)
    except (OSError, ValueError) as exc:
        print(f"[X] failed to load baseline: {exc}")
        return 2

    skipped: list[str] = []
    try:
        artefacts = load_artefacts(args.paths, phase=baseline.phase, skipped=skipped)
    except cost_markers.UnsupportedMetricsSchema as exc:
        print(f"[X] {exc}")
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
            json.dumps(report.to_dict(), indent=2, ensure_ascii=True) + "\n",
            encoding="utf-8",
        )
    return 0 if report.ok else 1


if __name__ == "__main__":
    sys.exit(main())
