"""Executable regression evaluation for the multi-version benchmark.

Pure functions. Given the parsed ``results/index.json`` mapping, a tolerance
mapping (``regression_thresholds.json``) and a (previous, candidate) release
pair, evaluate whether the candidate has regressed on either axis:

* energy balance -- full-period MAE and |MBE| per flux (W m^-2)
* RSL air temperature -- full-period MAE per measurement height (K)

A metric fails when its increase exceeds ``max(floor, relative * previous)``.
What counts as "material" therefore lives in the tolerance file, together
with its ``status``: the evaluation is only a scientific release decision when
that file records an approved policy (``policy_is_approved``); otherwise it is
advisory.

Coverage is part of the evaluation. Every quantity the previous release
carries (and every flux the index declares) must be present and finite in the
candidate; a candidate that loses a quantity, or yields no comparable checks at
all, fails rather than passing vacuously. The RSL axis is skipped only when the
PREVIOUS release lacks it (historical coverage that never existed), never when
the candidate drops it.

Exceptions are narrow: an entry waives exactly one (axis, key, metric) check on
one release pair, only up to its recorded ``max_delta``, and must carry its
basis, evidence and reason. It never waives any other failure on that pair.

No supy import and no I/O, so the evaluation is identical in every pinned
environment and trivially unit-testable (cf. ``bench_drift.py``, which is
descriptive only).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional

from packaging.version import InvalidVersion, Version

EB_METRICS = ("MAE", "MBE")
RSL_METRICS = ("MAE",)
EXCEPTION_FIELDS = (
    "from",
    "to",
    "axis",
    "key",
    "metric",
    "max_delta",
    "basis",
    "evidence",
    "reason",
)


@dataclass(frozen=True)
class MetricCheck:
    axis: str  # "energy_balance" | "rsl"
    key: str  # flux name or RSL height label
    metric: str  # "MAE" | "MBE"
    previous: float  # |MBE| for the bias metric
    candidate: float
    delta: float
    tolerance: float
    passed: bool
    waived_by: Optional[dict] = None  # the exception entry that covers this failure


@dataclass(frozen=True)
class CoverageFailure:
    axis: str
    key: str
    metric: str
    problem: str  # "missing in candidate" | "missing in previous" | "non-finite in candidate" | ...


@dataclass(frozen=True)
class Decision:
    from_tag: str
    to_tag: str
    fingerprint_changed: bool
    checks: list[MetricCheck] = field(default_factory=list)
    coverage_failures: list[CoverageFailure] = field(default_factory=list)
    skipped: list[str] = field(default_factory=list)
    stale_exceptions: list[dict] = field(default_factory=list)

    @property
    def failures(self) -> list[MetricCheck]:
        """Checks beyond tolerance, whether or not an exception waives them."""
        return [c for c in self.checks if not c.passed]

    @property
    def unwaived_failures(self) -> list[MetricCheck]:
        return [c for c in self.failures if c.waived_by is None]

    @property
    def waived_failures(self) -> list[MetricCheck]:
        return [c for c in self.failures if c.waived_by is not None]

    @property
    def passed(self) -> bool:
        """Within tolerance with full coverage. Zero checks is not a pass."""
        return (
            bool(self.checks)
            and not self.coverage_failures
            and not self.unwaived_failures
        )


def _version_key(tag: str):
    try:
        return (0, Version(tag))
    except InvalidVersion:
        return (1, tag)


def ordered_versions(index: dict) -> list[str]:
    """Release tags in the index, oldest first, PEP 440 ordered."""
    return sorted(index.get("versions", {}), key=_version_key)


def policy_is_approved(thresholds: dict) -> bool:
    """True only when the tolerance file records an explicit approval."""
    approval = thresholds.get("approval")
    if thresholds.get("approved") is not True or not isinstance(approval, dict):
        return False
    return all(
        isinstance(approval.get(k), str) and approval[k].strip()
        for k in ("by", "on", "reference")
    )


def _axis_thresholds(thresholds: dict, axis: str) -> dict:
    block = thresholds.get(axis)
    if not isinstance(block, dict):
        raise ValueError(f"thresholds missing '{axis}' block")
    for k in ("relative", "floor"):
        if k not in block:
            raise ValueError(f"thresholds['{axis}'] missing '{k}'")
    return block


def _tolerance(previous: float, block: dict) -> float:
    return max(float(block["floor"]), float(block["relative"]) * abs(previous))


def _finite(value) -> Optional[float]:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    return float(value) if math.isfinite(value) else None


def _check(
    axis: str,
    key: str,
    metric: str,
    prev: dict,
    cand: dict,
    block: dict,
    coverage: list[CoverageFailure],
):
    a, b = _finite(prev.get(metric)), _finite(cand.get(metric))
    if a is None:
        problem = (
            "missing in previous"
            if prev.get(metric) is None
            else "non-finite in previous"
        )
        coverage.append(CoverageFailure(axis, key, metric, problem))
        return None
    if b is None:
        problem = (
            "missing in candidate"
            if cand.get(metric) is None
            else "non-finite in candidate"
        )
        coverage.append(CoverageFailure(axis, key, metric, problem))
        return None
    # MBE is signed; a regression is growth in its magnitude.
    if metric == "MBE":
        a, b = abs(a), abs(b)
    delta = round(b - a, 6)
    tol = _tolerance(a, block)
    return MetricCheck(axis, key, metric, a, b, delta, tol, passed=delta <= tol)


def _validate_exception(entry: dict) -> None:
    missing = [k for k in EXCEPTION_FIELDS if not entry.get(k) and entry.get(k) != 0]
    if missing:
        raise ValueError(
            f"exception {entry.get('from')}->{entry.get('to')} missing {missing}"
        )
    if _finite(entry["max_delta"]) is None or entry["max_delta"] < 0:
        raise ValueError(
            f"exception {entry['from']}->{entry['to']} has invalid max_delta"
        )


def _exceptions_for(thresholds: dict, from_tag: str, to_tag: str) -> list[dict]:
    out = []
    for entry in thresholds.get("exceptions", []) or []:
        _validate_exception(entry)
        if entry["from"] == from_tag and entry["to"] == to_tag:
            out.append(entry)
    return out


def _apply_exceptions(
    checks: list[MetricCheck], entries: list[dict]
) -> tuple[list[MetricCheck], list[dict]]:
    """Waive failing checks that an exception names exactly and bounds."""
    used: set[int] = set()
    waived: list[MetricCheck] = []
    for c in checks:
        entry = None
        if not c.passed:
            for i, e in enumerate(entries):
                if (e["axis"], str(e["key"]), e["metric"]) == (
                    c.axis,
                    str(c.key),
                    c.metric,
                ) and c.delta <= float(e["max_delta"]):
                    entry, _ = e, used.add(i)
                    break
        waived.append(
            MetricCheck(
                c.axis,
                c.key,
                c.metric,
                c.previous,
                c.candidate,
                c.delta,
                c.tolerance,
                c.passed,
                entry,
            )
        )
    stale = [e for i, e in enumerate(entries) if i not in used]
    return waived, stale


def decide(index: dict, thresholds: dict, from_tag: str, to_tag: str) -> Decision:
    """Evaluate ``to_tag`` against ``from_tag`` under ``thresholds``."""
    blocks = index.get("versions", {})
    for tag in (from_tag, to_tag):
        if tag not in blocks:
            raise KeyError(f"release {tag!r} not in index")
    prev, cand = blocks[from_tag], blocks[to_tag]
    for tag, block in ((from_tag, prev), (to_tag, cand)):
        if block.get("status", "ok") != "ok":
            raise ValueError(
                f"release {tag!r} has status {block.get('status')!r}; cannot compare"
            )

    checks: list[MetricCheck] = []
    coverage: list[CoverageFailure] = []
    skipped: list[str] = []

    # Energy balance: required fluxes are those the index declares plus any the
    # previous release carries; the candidate must provide every one of them.
    eb = _axis_thresholds(thresholds, "energy_balance")
    prev_full, cand_full = prev.get("full", {}) or {}, cand.get("full", {}) or {}
    required = list(
        dict.fromkeys(list(index.get("fluxes", []) or []) + list(prev_full))
    )
    if not required:
        coverage.append(
            CoverageFailure(
                "energy_balance",
                "*",
                "*",
                "no fluxes declared by index or previous release",
            )
        )
    for flux in required:
        for metric in EB_METRICS:
            c = _check(
                "energy_balance",
                flux,
                metric,
                prev_full.get(flux, {}) or {},
                cand_full.get(flux, {}) or {},
                eb,
                coverage,
            )
            if c is not None:
                checks.append(c)

    # RSL: skipped only when the previous release never had the axis.
    rsl = _axis_thresholds(thresholds, "rsl")
    prev_rsl = (prev.get("rsl") or {}).get("stats") or {}
    cand_rsl = (cand.get("rsl") or {}).get("stats") or {}
    if not prev_rsl:
        skipped.append(
            f"rsl: axis not recorded for {from_tag} (no historical coverage to compare)"
        )
    elif not cand_rsl:
        coverage.append(
            CoverageFailure(
                "rsl",
                "*",
                "*",
                "axis recorded for previous release but missing in candidate",
            )
        )
    else:
        for height in prev_rsl:
            p_full = (prev_rsl.get(height) or {}).get("full", {}).get("all", {}) or {}
            c_full = (cand_rsl.get(height) or {}).get("full", {}).get("all", {}) or {}
            for metric in RSL_METRICS:
                c = _check("rsl", str(height), metric, p_full, c_full, rsl, coverage)
                if c is not None:
                    checks.append(c)

    checks, stale = _apply_exceptions(
        checks, _exceptions_for(thresholds, from_tag, to_tag)
    )
    return Decision(
        from_tag=from_tag,
        to_tag=to_tag,
        fingerprint_changed=prev.get("fingerprint") != cand.get("fingerprint"),
        checks=checks,
        coverage_failures=coverage,
        skipped=skipped,
        stale_exceptions=stale,
    )


def sweep(index: dict, thresholds: dict) -> list[Decision]:
    """Evaluate every consecutive OK release pair in the index, oldest first."""
    versions = [
        v
        for v in ordered_versions(index)
        if index["versions"][v].get("status", "ok") == "ok"
    ]
    return [
        decide(index, thresholds, versions[i - 1], versions[i])
        for i in range(1, len(versions))
    ]


def previous_release(index: dict, tag: str) -> Optional[str]:
    """The release immediately before ``tag`` in PEP 440 order, if any."""
    key = _version_key(tag)
    before = [v for v in ordered_versions(index) if v != tag and _version_key(v) < key]
    return before[-1] if before else None
