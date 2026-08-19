"""Unit tests for the cross-version drift computation."""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from bench_drift import compute_drift, DriftRecord  # noqa: E402

# Two synthetic versions; EB 'full' + one 'seasonal' bucket each.
INDEX = {
    "site": "TEST",
    "versions": {
        "2025.1.1": {
            "fingerprint": "aaa",
            "full": {"QH": {"MAE": 10.0, "MBE": 1.0, "n": 100}},
            "seasonal": {"QH": {"DJF": {"MAE": 8.0, "MBE": 0.5, "n": 50}}},
        },
        "2025.2.2": {
            "fingerprint": "bbb",
            "full": {"QH": {"MAE": 12.5, "MBE": 0.4, "n": 100}},
            "seasonal": {"QH": {"DJF": {"MAE": 9.0, "MBE": 0.1, "n": 50}}},
        },
    },
}


def _find(records, framing, interval, bucket):
    return [
        r for r in records
        if r.framing == framing and r.interval == interval and r.bucket == bucket
    ]


def test_consecutive_full_delta():
    records = compute_drift(INDEX)
    full = _find(records, "consecutive", "full", "all")
    assert len(full) == 1
    r = full[0]
    assert r.from_tag == "2025.1.1" and r.to_tag == "2025.2.2"
    assert r.flux == "QH"
    assert r.delta_mae == 2.5
    assert r.delta_mbe == -0.6
    assert r.fingerprint_changed is True


def test_seasonal_bucket_present():
    records = compute_drift(INDEX)
    djf = _find(records, "consecutive", "seasonal", "DJF")
    assert len(djf) == 1
    assert djf[0].delta_mae == 1.0


def test_baseline_framing_uses_first_version_by_default():
    records = compute_drift(INDEX)
    base = _find(records, "baseline", "full", "all")
    assert len(base) == 1
    assert base[0].from_tag == "2025.1.1"


def test_explicit_baseline_tag_override():
    records = compute_drift(INDEX, baseline_tag="2025.2.2")
    base = [r for r in records if r.framing == "baseline"]
    # Baseline == last version => only the baseline-vs-itself is skipped,
    # leaving no baseline pairs for a 2-version index.
    assert base == []


def test_records_are_drift_record_instances():
    records = compute_drift(INDEX)
    assert records and all(isinstance(r, DriftRecord) for r in records)
