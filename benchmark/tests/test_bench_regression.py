"""Unit tests for the executable regression evaluation."""

import copy
import json
import math
import sys
from pathlib import Path

import pytest

BENCH = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(BENCH))

from bench_regression import decide, policy_is_approved, previous_release, sweep  # noqa: E402
import check_regression  # noqa: E402

THRESHOLDS = {
    "status": "proposed",
    "approved": False,
    "energy_balance": {"relative": 0.05, "floor": 0.05},
    "rsl": {"relative": 0.05, "floor": 0.05},
    "exceptions": [],
}
APPROVED = {
    **THRESHOLDS,
    "status": "approved",
    "approved": True,
    "approval": {"by": "maintainers", "on": "2026-01-01", "reference": "#0"},
}


def _exception(**over):
    base = {
        "from": "2025.1.1",
        "to": "2025.2.2",
        "axis": "energy_balance",
        "key": "QH",
        "metric": "MAE",
        "max_delta": 1.0,
        "basis": "recorded-history",
        "evidence": "PR #0",
        "reason": "test",
    }
    base.update(over)
    return base


def _block(fp, qh_mae, qh_mbe, t65_mae=None, kup=(2.0, -0.5)):
    block = {
        "status": "ok",
        "fingerprint": fp,
        "full": {
            "QH": {"MAE": qh_mae, "MBE": qh_mbe, "n": 100},
            "Kup": {"MAE": kup[0], "MBE": kup[1], "n": 100},
        },
        "seasonal": {"QH": {"DJF": {"MAE": qh_mae, "MBE": qh_mbe, "n": 50}}},
    }
    if t65_mae is not None:
        block["rsl"] = {
            "stats": {"6.5": {"full": {"all": {"MAE": t65_mae, "MBE": 0.1, "n": 10}}}}
        }
    return block


def _index(*blocks, fluxes=("QH", "Kup")):
    return {"fluxes": list(fluxes), "versions": {tag: b for tag, b in blocks}}


def _pair(prev, cand):
    return _index(("2025.1.1", prev), ("2025.2.2", cand))


def _decide(ix, th=THRESHOLDS):
    return decide(ix, th, "2025.1.1", "2025.2.2")


# --- tolerance semantics -----------------------------------------------------


def test_identical_fingerprint_passes_with_zero_deltas():
    d = _decide(_pair(_block("a", 10.0, 1.0), _block("a", 10.0, 1.0)))
    assert d.passed and not d.fingerprint_changed
    assert len(d.checks) == 4 and all(c.delta == 0.0 for c in d.checks)


def test_material_mae_increase_fails():
    d = _decide(_pair(_block("a", 10.0, 1.0), _block("b", 11.0, 1.0)))
    assert not d.passed
    (f,) = d.failures
    assert (f.axis, f.key, f.metric) == ("energy_balance", "QH", "MAE")
    assert f.tolerance == pytest.approx(0.5)  # 5% of 10 beats the 0.05 floor


def test_increase_within_tolerance_passes():
    assert _decide(_pair(_block("a", 10.0, 1.0), _block("b", 10.4, 1.0))).passed


def test_floor_applies_to_small_metrics():
    d = _decide(_pair(_block("a", 10.0, 0.10), _block("b", 10.0, 0.14)))
    assert d.passed  # +0.04 |MBE| is under the 0.05 floor (5% of 0.10 = 0.005)


def test_bias_regression_is_growth_in_magnitude():
    d = _decide(_pair(_block("a", 10.0, -1.0), _block("b", 10.0, -2.0)))
    assert [(c.key, c.metric) for c in d.failures] == [("QH", "MBE")]
    assert _decide(_pair(_block("a", 10.0, -2.0), _block("b", 10.0, 0.5))).passed


def test_improvement_never_fails():
    assert _decide(
        _pair(_block("a", 10.0, 1.0, 0.5), _block("b", 5.0, 0.2, 0.2))
    ).passed


def test_rsl_axis_is_evaluated_when_both_releases_carry_it():
    d = _decide(_pair(_block("a", 10.0, 1.0, 0.40), _block("b", 10.0, 1.0, 0.52)))
    assert [(c.axis, c.key) for c in d.failures] == [("rsl", "6.5")]


# --- coverage: a candidate cannot pass vacuously -----------------------------


def test_candidate_with_no_metrics_fails():
    cand = _block("b", 10.0, 1.0, 0.5)
    cand["full"] = {}
    cand.pop("rsl")
    d = _decide(_pair(_block("a", 10.0, 1.0, 0.5), cand))
    assert not d.passed and not d.checks
    problems = {(c.axis, c.key, c.metric, c.problem) for c in d.coverage_failures}
    assert ("energy_balance", "QH", "MAE", "missing in candidate") in problems
    assert (
        "rsl",
        "*",
        "*",
        "axis recorded for previous release but missing in candidate",
    ) in problems


def test_candidate_missing_one_required_flux_fails():
    cand = _block("b", 10.0, 1.0)
    del cand["full"]["Kup"]
    d = _decide(_pair(_block("a", 10.0, 1.0), cand))
    assert not d.passed
    assert {(c.key, c.metric) for c in d.coverage_failures} == {
        ("Kup", "MAE"),
        ("Kup", "MBE"),
    }
    assert len(d.checks) == 2  # QH is still evaluated and reported


def test_candidate_missing_one_metric_fails():
    cand = _block("b", 10.0, 1.0)
    del cand["full"]["QH"]["MBE"]
    d = _decide(_pair(_block("a", 10.0, 1.0), cand))
    assert not d.passed
    assert [(c.key, c.metric, c.problem) for c in d.coverage_failures] == [
        ("QH", "MBE", "missing in candidate")
    ]


@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf, None, "12.0"])
def test_non_finite_candidate_metric_fails(bad):
    cand = _block("b", 10.0, 1.0)
    cand["full"]["QH"]["MAE"] = bad
    d = _decide(_pair(_block("a", 10.0, 1.0), cand))
    assert not d.passed
    assert d.coverage_failures[0].key == "QH" and d.coverage_failures[0].metric == "MAE"


def test_index_declared_flux_is_required_even_if_previous_lacks_it():
    ix = _pair(_block("a", 10.0, 1.0), _block("b", 10.0, 1.0))
    ix["fluxes"] = ["QH", "Kup", "QE"]
    d = _decide(ix)
    assert not d.passed
    assert {c.key for c in d.coverage_failures} == {"QE"}
    assert all(c.problem == "missing in previous" for c in d.coverage_failures)


def test_rsl_dropped_by_candidate_fails_but_absent_history_is_skipped():
    d = _decide(_pair(_block("a", 10.0, 1.0, 0.5), _block("b", 10.0, 1.0)))
    assert not d.passed and d.coverage_failures[0].axis == "rsl"
    d2 = _decide(_pair(_block("a", 10.0, 1.0), _block("b", 10.0, 1.0, 0.9)))
    assert d2.passed and any(s.startswith("rsl") for s in d2.skipped)


def test_committed_index_with_emptied_candidate_fails():
    ix = json.loads((BENCH / "results" / "index.json").read_text(encoding="utf-8"))
    th = json.loads((BENCH / "regression_thresholds.json").read_text(encoding="utf-8"))
    ix["versions"]["2026.4.3"]["full"] = {}
    ix["versions"]["2026.4.3"].pop("rsl", None)
    d = decide(ix, th, "2026.1.28", "2026.4.3")
    assert not d.passed and not d.checks and d.coverage_failures


# --- exceptions are narrow ----------------------------------------------------


def test_exception_waives_only_the_named_bounded_check():
    ix = _pair(_block("a", 10.0, 1.0), _block("b", 11.0, 1.0))
    th = {**THRESHOLDS, "exceptions": [_exception()]}
    d = _decide(ix, th)
    assert (
        d.passed
        and len(d.waived_failures) == 1
        and d.waived_failures[0].waived_by["reason"] == "test"
    )


def test_exception_does_not_waive_an_unrelated_failure():
    ix = _pair(_block("a", 10.0, 1.0), _block("b", 11.0, 1.0, kup=(10000.0, -0.5)))
    th = {**THRESHOLDS, "exceptions": [_exception()]}
    d = _decide(ix, th)
    assert not d.passed
    assert [(c.key, c.metric) for c in d.unwaived_failures] == [("Kup", "MAE")]
    assert [(c.key, c.metric) for c in d.waived_failures] == [("QH", "MAE")]


def test_exception_does_not_waive_an_enlarged_shift():
    ix = _pair(_block("a", 10.0, 1.0), _block("b", 12.0, 1.0))
    th = {**THRESHOLDS, "exceptions": [_exception(max_delta=1.0)]}
    d = _decide(ix, th)
    assert not d.passed and d.unwaived_failures[0].key == "QH"


def test_exception_does_not_cover_a_different_metric_or_axis():
    ix = _pair(_block("a", 10.0, 1.0, 0.4), _block("b", 10.0, 3.0, 0.9))
    th = {**THRESHOLDS, "exceptions": [_exception(metric="MAE", max_delta=5.0)]}
    d = _decide(ix, th)
    assert not d.passed
    assert {(c.axis, c.key, c.metric) for c in d.unwaived_failures} == {
        ("energy_balance", "QH", "MBE"),
        ("rsl", "6.5", "MAE"),
    }


def test_exception_does_not_cover_coverage_failures():
    cand = _block("b", 11.0, 1.0)
    del cand["full"]["Kup"]
    th = {**THRESHOLDS, "exceptions": [_exception()]}
    assert not _decide(_pair(_block("a", 10.0, 1.0), cand), th).passed


def test_committed_rsl_exception_does_not_waive_a_flux_regression():
    ix = json.loads((BENCH / "results" / "index.json").read_text(encoding="utf-8"))
    th = json.loads((BENCH / "regression_thresholds.json").read_text(encoding="utf-8"))
    assert decide(ix, th, "2026.1.28", "2026.4.3").passed
    ix["versions"]["2026.4.3"]["full"]["Kup"]["MAE"] = 10000.0
    d = decide(ix, th, "2026.1.28", "2026.4.3")
    assert not d.passed
    assert [(c.key, c.metric) for c in d.unwaived_failures] == [("Kup", "MAE")]
    assert [(c.axis, c.key) for c in d.waived_failures] == [("rsl", "6.5")]


@pytest.mark.parametrize(
    "missing", ["axis", "key", "metric", "max_delta", "basis", "evidence", "reason"]
)
def test_exception_requires_every_field(missing):
    entry = _exception()
    entry.pop(missing)
    th = {**THRESHOLDS, "exceptions": [entry]}
    with pytest.raises(ValueError, match=missing):
        _decide(_pair(_block("a", 10.0, 1.0), _block("b", 11.0, 1.0)), th)


def test_stale_exception_is_reported_on_a_clean_pair():
    th = {**THRESHOLDS, "exceptions": [_exception()]}
    d = _decide(_pair(_block("a", 10.0, 1.0), _block("a", 10.0, 1.0)), th)
    assert d.passed and len(d.stale_exceptions) == 1 and not d.waived_failures


# --- policy status ----------------------------------------------------------------


def test_policy_is_approved_requires_explicit_record():
    assert not policy_is_approved(THRESHOLDS)
    assert policy_is_approved(APPROVED)
    half = {**APPROVED, "approval": {"by": "x", "on": "", "reference": "y"}}
    assert not policy_is_approved(half)
    assert not policy_is_approved({**APPROVED, "approved": "yes"})


def test_committed_policy_is_not_approved():
    th = json.loads((BENCH / "regression_thresholds.json").read_text(encoding="utf-8"))
    assert th["status"] == "proposed" and not policy_is_approved(th)


# --- errors and ordering ----------------------------------------------------------


def test_failed_release_cannot_be_compared():
    bad = _block("b", 10.0, 1.0)
    bad["status"] = "failed"
    with pytest.raises(ValueError, match="status"):
        _decide(_pair(_block("a", 10.0, 1.0), bad))


def test_missing_thresholds_block_is_an_error():
    with pytest.raises(ValueError, match="rsl"):
        _decide(
            _pair(_block("a", 10.0, 1.0), _block("a", 10.0, 1.0)),
            {"energy_balance": {"relative": 0.05, "floor": 0.05}},
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


# --- CLI ----------------------------------------------------------------------------


def _write(tmp_path, ix, th):
    ix_path, th_path = tmp_path / "index.json", tmp_path / "th.json"
    ix_path.write_text(json.dumps(ix), encoding="utf-8")
    th_path.write_text(json.dumps(th), encoding="utf-8")
    return ["--index", str(ix_path), "--thresholds", str(th_path)]


def test_committed_history_passes_the_sweep():
    assert check_regression.main(["--sweep"]) == check_regression.EXIT_OK


def test_cli_sweep_fails_on_a_regressed_index(tmp_path, capsys):
    ix = _pair(_block("a", 10.0, 1.0), _block("b", 12.0, 1.0))
    report = tmp_path / "out" / "report.json"
    rc = check_regression.main([
        "--sweep",
        *_write(tmp_path, ix, THRESHOLDS),
        "--report",
        str(report),
    ])
    assert rc == check_regression.EXIT_REGRESSION
    out = capsys.readouterr().out
    assert "FAIL" in out and "energy_balance/QH MAE" in out
    rep = json.loads(report.read_text(encoding="utf-8"))
    assert (
        rep["passed"] is False
        and rep["mode"] == "historical-sweep"
        and rep["policy_approved"] is False
    )
    assert rep["decisions"][0]["coverage"]["checks"] == 4
    assert rep["decisions"][0]["unwaived_failures"][0]["delta"] == pytest.approx(2.0)


def test_cli_sweep_fails_on_an_empty_candidate(tmp_path, capsys):
    cand = _block("b", 10.0, 1.0)
    cand["full"] = {}
    rc = check_regression.main([
        "--sweep",
        *_write(tmp_path, _pair(_block("a", 10.0, 1.0), cand), THRESHOLDS),
    ])
    assert rc == check_regression.EXIT_REGRESSION
    assert "no comparable checks" in capsys.readouterr().out


def test_cli_candidate_is_advisory_until_policy_approved(tmp_path, capsys):
    ix = _pair(_block("a", 10.0, 1.0), _block("a", 10.0, 1.0))
    rc = check_regression.main([
        "--candidate",
        "2025.2.2",
        *_write(tmp_path, ix, THRESHOLDS),
    ])
    assert rc == check_regression.EXIT_DECISION_UNAVAILABLE
    assert "ADVISORY ONLY" in capsys.readouterr().err
    rc = check_regression.main([
        "--candidate",
        "2025.2.2",
        *_write(tmp_path, ix, APPROVED),
    ])
    assert rc == check_regression.EXIT_OK


def test_cli_candidate_regression_fails_regardless_of_policy(tmp_path):
    ix = _pair(_block("a", 10.0, 1.0), _block("b", 12.0, 1.0))
    assert (
        check_regression.main([
            "--candidate",
            "2025.2.2",
            *_write(tmp_path, ix, THRESHOLDS),
        ])
        == 1
    )
    assert (
        check_regression.main([
            "--candidate",
            "2025.2.2",
            *_write(tmp_path, ix, APPROVED),
        ])
        == 1
    )


def test_cli_candidate_without_predecessor_is_a_usage_error(tmp_path):
    ix = _pair(_block("a", 10.0, 1.0), _block("a", 10.0, 1.0))
    assert (
        check_regression.main([
            "--candidate",
            "2025.1.1",
            *_write(tmp_path, ix, THRESHOLDS),
        ])
        == 2
    )
