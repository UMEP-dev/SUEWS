"""Executable regression decision for the multi-version benchmark.

Pure functions. Given the parsed ``results/index.json`` mapping, a thresholds
mapping (``regression_thresholds.json``) and a (previous, candidate) release
pair, decide whether the candidate has regressed materially on either
evaluation axis:

* energy balance -- full-period MAE and |MBE| per flux (W m^-2)
* RSL air temperature -- full-period MAE per measurement height (K)

"Material" is defined by the thresholds file, not here: a metric fails when
its increase exceeds ``max(floor, relative * previous)``. Previously reviewed
shifts can be listed under ``accepted`` in the thresholds file so that the
sweep over recorded history passes while the reason stays on record.

No supy import and no I/O, so the decision is identical in every pinned
environment and trivially unit-testable (cf. ``bench_drift.py``, which is
descriptive only).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional

from packaging.version import InvalidVersion, Version

EB_METRICS = ("MAE", "MBE")
RSL_METRICS = ("MAE",)


@dataclass(frozen=True)
class MetricCheck:
    axis: str  # "energy_balance" | "rsl"
    key: str  # flux name or RSL height label
    metric: str  # "MAE" | "MBE"
    previous: float
    candidate: float
    delta: float
    tolerance: float
    passed: bool


@dataclass(frozen=True)
class Decision:
    from_tag: str
    to_tag: str
    fingerprint_changed: bool
    checks: list[MetricCheck] = field(default_factory=list)
    accepted: Optional[dict] = None
    skipped: list[str] = field(default_factory=list)

    @property
    def failures(self) -> list[MetricCheck]:
        return [c for c in self.checks if not c.passed]

    @property
    def passed(self) -> bool:
        return not self.failures or self.accepted is not None


def _version_key(tag: str):
    try:
        return (0, Version(tag))
    except InvalidVersion:
        return (1, tag)


def ordered_versions(index: dict) -> list[str]:
    """Release tags in the index, oldest first, PEP 440 ordered."""
    return sorted(index.get("versions", {}), key=_version_key)


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


def _check(
    axis: str, key: str, metric: str, prev: dict, cand: dict, block: dict
) -> Optional[MetricCheck]:
    a, b = prev.get(metric), cand.get(metric)
    if a is None or b is None:
        return None
    # MBE is signed; a regression is growth in its magnitude.
    if metric == "MBE":
        a, b = abs(a), abs(b)
    delta = round(b - a, 6)
    tol = _tolerance(a, block)
    return MetricCheck(axis, key, metric, a, b, delta, tol, passed=delta <= tol)


def _accepted_entry(thresholds: dict, from_tag: str, to_tag: str) -> Optional[dict]:
    for entry in thresholds.get("accepted", []) or []:
        if entry.get("from") == from_tag and entry.get("to") == to_tag:
            if not entry.get("reason"):
                raise ValueError(f"accepted entry {from_tag}->{to_tag} has no 'reason'")
            return entry
    return None


def decide(index: dict, thresholds: dict, from_tag: str, to_tag: str) -> Decision:
    """Compare ``to_tag`` against ``from_tag`` under ``thresholds``."""
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
    skipped: list[str] = []

    eb = _axis_thresholds(thresholds, "energy_balance")
    for flux, cand_stats in cand.get("full", {}).items():
        prev_stats = prev.get("full", {}).get(flux)
        if prev_stats is None:
            skipped.append(f"energy_balance/{flux}: absent in {from_tag}")
            continue
        for metric in EB_METRICS:
            c = _check("energy_balance", flux, metric, prev_stats, cand_stats, eb)
            if c is not None:
                checks.append(c)

    rsl = _axis_thresholds(thresholds, "rsl")
    prev_rsl = (prev.get("rsl") or {}).get("stats")
    cand_rsl = (cand.get("rsl") or {}).get("stats")
    if prev_rsl and cand_rsl:
        for height, cand_h in cand_rsl.items():
            prev_h = prev_rsl.get(height)
            if prev_h is None:
                skipped.append(f"rsl/{height}: absent in {from_tag}")
                continue
            prev_full = prev_h.get("full", {}).get("all", {})
            cand_full = cand_h.get("full", {}).get("all", {})
            for metric in RSL_METRICS:
                c = _check("rsl", height, metric, prev_full, cand_full, rsl)
                if c is not None:
                    checks.append(c)
    else:
        skipped.append("rsl: axis not present in both releases")

    fingerprint_changed = prev.get("fingerprint") != cand.get("fingerprint")
    accepted = _accepted_entry(thresholds, from_tag, to_tag)
    failing = [c for c in checks if not c.passed]
    # An accepted entry only covers a pair that actually needs it; a stale
    # entry on a clean pair is reported so it can be removed.
    return Decision(
        from_tag=from_tag,
        to_tag=to_tag,
        fingerprint_changed=fingerprint_changed,
        checks=checks,
        accepted=accepted if failing else None,
        skipped=skipped,
    )


def sweep(index: dict, thresholds: dict) -> list[Decision]:
    """Decide every consecutive release pair in the index, oldest first."""
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
    versions = ordered_versions(index)
    others = [v for v in versions if v != tag]
    key = _version_key(tag)
    before = [v for v in others if _version_key(v) < key]
    return before[-1] if before else None
