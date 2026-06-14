"""Cross-version energy-balance drift from a benchmark results index.

Pure functions. Given the parsed ``results/index.json`` mapping, compute how
the error metrics move from one SUEWS release to the next. This is framed as
*drift* (descriptive deltas between releases), NOT a reproduction assertion.

No supy import and no I/O here, so it behaves identically regardless of the
pinned supy version and is trivially unit-testable.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterator, Optional

from packaging.version import InvalidVersion, Version


@dataclass(frozen=True)
class DriftRecord:
    from_tag: str
    to_tag: str
    framing: str  # "consecutive" | "baseline"
    flux: str
    interval: str  # "full" | "seasonal"
    bucket: str  # "all" | "DJF" | "MAM" | "JJA" | "SON"
    delta_mae: Optional[float]
    delta_mbe: Optional[float]
    fingerprint_changed: Optional[bool]


def _version_key(tag: str):
    try:
        return (0, Version(tag))
    except InvalidVersion:
        # Legacy 'a'-suffixed tags never appear in the modern index, but be
        # safe: order them before any PEP 440 version, lexically.
        return (1, tag)


def _ordered_versions(index: dict) -> list[str]:
    return sorted(index.get("versions", {}), key=_version_key)


def _buckets(block: dict) -> Iterator[tuple[str, str, str, dict]]:
    """Yield (interval, bucket, flux, stats) for one version block."""
    for flux, stats in block.get("full", {}).items():
        yield ("full", "all", flux, stats)
    for flux, seasons in block.get("seasonal", {}).items():
        for season, stats in seasons.items():
            yield ("seasonal", season, flux, stats)


def _delta(base: dict, target: dict, key: str) -> Optional[float]:
    a, b = base.get(key), target.get(key)
    if a is None or b is None:
        return None
    return round(b - a, 6)


def _pair_records(
    from_tag: str, to_tag: str, framing: str, blocks: dict
) -> Iterator[DriftRecord]:
    from_block, to_block = blocks[from_tag], blocks[to_tag]
    base_map = {(iv, bk, fx): st for iv, bk, fx, st in _buckets(from_block)}
    changed = from_block.get("fingerprint") != to_block.get("fingerprint")
    for iv, bk, fx, st in _buckets(to_block):
        base = base_map.get((iv, bk, fx))
        if base is None:
            continue
        yield DriftRecord(
            from_tag=from_tag,
            to_tag=to_tag,
            framing=framing,
            flux=fx,
            interval=iv,
            bucket=bk,
            delta_mae=_delta(base, st, "MAE"),
            delta_mbe=_delta(base, st, "MBE"),
            fingerprint_changed=changed,
        )


def compute_drift(index: dict, baseline_tag: Optional[str] = None) -> list[DriftRecord]:
    """Return consecutive and baseline drift records for an index mapping."""
    versions = _ordered_versions(index)
    blocks = index.get("versions", {})
    if len(versions) < 2:
        return []
    baseline = baseline_tag or versions[0]

    records: list[DriftRecord] = []
    for i in range(1, len(versions)):
        records.extend(
            _pair_records(versions[i - 1], versions[i], "consecutive", blocks)
        )
    # Baseline framing: how far each LATER release has drifted from the pinned
    # baseline. Only versions after the baseline's position are compared, so a
    # baseline pinned to the newest release yields no baseline records.
    if baseline in blocks and baseline in versions:
        start = versions.index(baseline) + 1
        for tag in versions[start:]:
            records.extend(_pair_records(baseline, tag, "baseline", blocks))
    return records
