"""Unit tests for the executable regression decision."""

import copy
import json
import sys
from pathlib import Path

import pytest

BENCH = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(BENCH))

from bench_regression import decide, previous_release, sweep  # noqa: E402
import check_regression  # noqa: E402

THRESHOLDS = {
    "energy_balance": {"relative": 0.05, "floor": 0.05},
    "rsl": {"relative": 0.05, "floor": 0.05},
    "accepted": [],
}


def _block(fp, qh_mae, qh_mbe, t65_mae=None):
    block = {
        "status": "ok",
        "fingerprint": fp,
        "full": {"QH": {"MAE": qh_mae, "MBE": qh_mbe, "n": 100}},
        "seasonal": {"QH": {"DJF": {"MAE": qh_mae, "MBE": qh_mbe, "n": 50}}},
    }
    if t65_mae is not None:
        block["rsl"] = {
            "stats": {"6.5": {"full": {"all": {"MAE": t65_mae, "MBE": 0.1, "n": 10}}}}
        }
    return block


def _index(*blocks):
    return {"versions": {tag: b for tag, b in blocks}}


def test_identical_fingerprint_passes_with_zero_deltas():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("a", 10.0, 1.0))
    )
    d = decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2")
    assert d.passed and not d.fingerprint_changed
    assert all(c.delta == 0.0 for c in d.checks)


def test_material_mae_increase_fails():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("b", 11.0, 1.0))
    )
    d = decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2")
    assert not d.passed
    (f,) = d.failures
    assert (f.axis, f.key, f.metric) == ("energy_balance", "QH", "MAE")
    assert f.tolerance == pytest.approx(0.5)  # 5% of 10 beats the 0.05 floor


def test_increase_within_tolerance_passes():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("b", 10.4, 1.0))
    )
    assert decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2").passed


def test_floor_applies_to_small_metrics():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 0.10)), ("2025.2.2", _block("b", 10.0, 0.14))
    )
    d = decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2")
    assert d.passed  # +0.04 |MBE| is under the 0.05 floor (5% of 0.10 = 0.005)


def test_bias_regression_is_growth_in_magnitude():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, -1.0)), ("2025.2.2", _block("b", 10.0, -2.0))
    )
    d = decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2")
    assert [c.metric for c in d.failures] == ["MBE"]
    # Bias shrinking towards zero is never a regression.
    ix2 = _index(
        ("2025.1.1", _block("a", 10.0, -2.0)), ("2025.2.2", _block("b", 10.0, 0.5))
    )
    assert decide(ix2, THRESHOLDS, "2025.1.1", "2025.2.2").passed


def test_improvement_never_fails():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0, 0.5)),
        ("2025.2.2", _block("b", 5.0, 0.2, 0.2)),
    )
    assert decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2").passed


def test_rsl_axis_is_gated_when_both_releases_carry_it():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0, 0.40)),
        ("2025.2.2", _block("b", 10.0, 1.0, 0.52)),
    )
    d = decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2")
    assert [(c.axis, c.key) for c in d.failures] == [("rsl", "6.5")]


def test_rsl_axis_skipped_when_absent_in_either_release():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("b", 10.0, 1.0, 0.9))
    )
    d = decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2")
    assert d.passed and any(s.startswith("rsl") for s in d.skipped)


def test_accepted_entry_passes_but_is_recorded():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("b", 11.0, 1.0))
    )
    th = copy.deepcopy(THRESHOLDS)
    th["accepted"] = [
        {"from": "2025.1.1", "to": "2025.2.2", "reason": "reviewed physics change"}
    ]
    d = decide(ix, th, "2025.1.1", "2025.2.2")
    assert d.passed and d.accepted["reason"] == "reviewed physics change"
    assert len(d.failures) == 1  # the failing check is still reported


def test_accepted_entry_requires_a_reason():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("b", 11.0, 1.0))
    )
    th = copy.deepcopy(THRESHOLDS)
    th["accepted"] = [{"from": "2025.1.1", "to": "2025.2.2"}]
    with pytest.raises(ValueError, match="reason"):
        decide(ix, th, "2025.1.1", "2025.2.2")


def test_accepted_entry_is_not_reported_on_a_clean_pair():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("a", 10.0, 1.0))
    )
    th = copy.deepcopy(THRESHOLDS)
    th["accepted"] = [{"from": "2025.1.1", "to": "2025.2.2", "reason": "stale"}]
    assert decide(ix, th, "2025.1.1", "2025.2.2").accepted is None


def test_failed_release_cannot_be_compared():
    bad = _block("b", 10.0, 1.0)
    bad["status"] = "failed"
    ix = _index(("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", bad))
    with pytest.raises(ValueError, match="status"):
        decide(ix, THRESHOLDS, "2025.1.1", "2025.2.2")


def test_missing_thresholds_block_is_an_error():
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("a", 10.0, 1.0))
    )
    with pytest.raises(ValueError, match="rsl"):
        decide(
            ix,
            {"energy_balance": {"relative": 0.05, "floor": 0.05}},
            "2025.1.1",
            "2025.2.2",
        )


def test_sweep_orders_by_version_and_skips_failed_releases():
    bad = _block("z", 99.0, 9.0)
    bad["status"] = "failed"
    ix = _index(
        ("2025.10.1", _block("c", 10.0, 1.0)),
        ("2025.2.2", _block("b", 10.0, 1.0)),
        ("2025.1.1", _block("a", 10.0, 1.0)),
        ("2025.5.5", bad),
    )
    pairs = [(d.from_tag, d.to_tag) for d in sweep(ix, THRESHOLDS)]
    assert pairs == [("2025.1.1", "2025.2.2"), ("2025.2.2", "2025.10.1")]


def test_previous_release_uses_pep440_order():
    ix = _index(("2025.10.1", {}), ("2025.2.2", {}), ("2025.1.1", {}))
    assert previous_release(ix, "2025.10.1") == "2025.2.2"
    assert previous_release(ix, "2025.1.1") is None


def test_committed_history_passes_under_committed_thresholds():
    rc = check_regression.main(["--sweep"])
    assert rc == 0


def test_cli_fails_on_a_regressed_index(tmp_path, capsys):
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("b", 12.0, 1.0))
    )
    ix_path = tmp_path / "index.json"
    ix_path.write_text(json.dumps(ix), encoding="utf-8")
    th_path = tmp_path / "th.json"
    th_path.write_text(json.dumps(THRESHOLDS), encoding="utf-8")
    report = tmp_path / "out" / "report.json"
    rc = check_regression.main([
        "--sweep",
        "--index",
        str(ix_path),
        "--thresholds",
        str(th_path),
        "--report",
        str(report),
    ])
    assert rc == 1
    out = capsys.readouterr().out
    assert "FAIL" in out and "energy_balance/QH MAE" in out
    rep = json.loads(report.read_text(encoding="utf-8"))
    assert rep["passed"] is False
    assert rep["decisions"][0]["failures"][0]["delta"] == pytest.approx(2.0)
    # Derived statistics only: no raw observations or model output leaks into the report.
    assert set(rep["decisions"][0]["failures"][0]) >= {
        "previous",
        "candidate",
        "delta",
        "tolerance",
    }


def test_cli_candidate_mode_defaults_to_predecessor(tmp_path):
    ix = _index(
        ("2025.1.1", _block("a", 10.0, 1.0)), ("2025.2.2", _block("a", 10.0, 1.0))
    )
    ix_path = tmp_path / "index.json"
    ix_path.write_text(json.dumps(ix), encoding="utf-8")
    th_path = tmp_path / "th.json"
    th_path.write_text(json.dumps(THRESHOLDS), encoding="utf-8")
    assert (
        check_regression.main([
            "--candidate",
            "2025.2.2",
            "--index",
            str(ix_path),
            "--thresholds",
            str(th_path),
        ])
        == 0
    )
    assert (
        check_regression.main([
            "--candidate",
            "2025.1.1",
            "--index",
            str(ix_path),
            "--thresholds",
            str(th_path),
        ])
        == 2
    )
