"""Unit tests for the legacy 73/87-column output parser/summariser.

Uses a tiny synthetic fixture (fixtures/legacy_SUEWS_60.txt) that carries the
named time + flux columns and one ``******`` token, exercising NaN coercion,
datetime-index reconstruction from Year/DOY/Hour/Min, the raw per-flux summary
(full + seasonal), and the deterministic fingerprint.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import legacy_output as lo  # noqa: E402

FIXTURE = Path(__file__).resolve().parent / "fixtures" / "legacy_SUEWS_60.txt"
FIXTURE_2016A = Path(__file__).resolve().parent / "fixtures" / "legacy_2016a_5.txt"


def test_read_builds_datetime_index():
    df = lo.read_legacy_output(FIXTURE)
    assert isinstance(df.index, pd.DatetimeIndex)
    # Row 1: Year 2011, DOY 1, Hour 1, Min 0 -> 2011-01-01 01:00.
    assert df.index[0] == pd.Timestamp("2011-01-01 01:00")
    # DOY 60 -> 2011-03-01 (2011 is not a leap year).
    assert df.index[2] == pd.Timestamp("2011-03-01 12:00")
    # DOY 182 -> 2011-07-01.
    assert df.index[3] == pd.Timestamp("2011-07-01 12:00")


def test_star_token_becomes_nan():
    df = lo.read_legacy_output(FIXTURE)
    # The last row's Kdown is ``******`` -> NaN.
    assert np.isnan(df["Kdown"].iloc[-1])
    # The flux we summarise on that row (QN=50) stays finite.
    assert df["QN"].iloc[-1] == 50.0


def test_raw_summary_full_and_seasonal():
    df = lo.read_legacy_output(FIXTURE)
    summary = lo.raw_flux_summary(df)
    assert set(summary.keys()) == {"QN", "QH", "QE", "Kup", "Lup"}

    qn_full = summary["QN"]["full"]
    # QN values: -50, -49, 240, 330, 50  -> n=5.
    assert qn_full["n"] == 5
    assert qn_full["min"] == -50.0
    assert qn_full["max"] == 330.0
    assert qn_full["mean"] == pytest.approx(np.mean([-50, -49, 240, 330, 50]), abs=1e-6)

    # Seasonal split: rows fall in DJF (Jan x2), MAM (Mar), JJA (Jul), SON (Oct).
    seasonal = summary["QN"]["seasonal"]
    assert seasonal["DJF"]["n"] == 2
    assert seasonal["MAM"]["n"] == 1
    assert seasonal["JJA"]["n"] == 1
    assert seasonal["SON"]["n"] == 1
    assert seasonal["DJF"]["mean"] == pytest.approx(np.mean([-50, -49]), abs=1e-6)


def test_fingerprint_is_deterministic_and_value_sensitive():
    df = lo.read_legacy_output(FIXTURE)
    fp1 = lo.output_fingerprint(df)
    fp2 = lo.output_fingerprint(lo.read_legacy_output(FIXTURE))
    assert fp1 == fp2
    assert len(fp1) == 64

    # Perturb one value -> fingerprint must change.
    df2 = df.copy()
    df2.loc[df2.index[0], "QH"] = df2["QH"].iloc[0] + 1.0
    assert lo.output_fingerprint(df2) != fp1


def test_summarise_output_top_level_shape():
    out = lo.summarise_output(FIXTURE)
    assert out["fluxes"] == ["QN", "QH", "QE", "Kup", "Lup"]
    assert out["n_rows"] == 5
    assert out["period"]["start"] == "2011-01-01 01:00"
    assert out["period"]["end"] == "2011-10-01 18:00"
    assert len(out["output_fingerprint"]) == 64
    assert len(out["file_sha256"]) == 64
    assert out["raw_summary"]["QH"]["full"]["n"] == 5


def test_missing_time_column_raises(tmp_path):
    bad = tmp_path / "bad.txt"
    bad.write_text("Year DOY Hour QN\n2011 1 1 5.0\n", encoding="utf-8")
    with pytest.raises(ValueError):
        lo.read_legacy_output(bad)


def test_reads_2016a_lowercase_percent_header():
    # 2016a output: '%'-prefixed lowercase header (iy/id/it/imin, qn/kup/...).
    df = lo.read_legacy_output(FIXTURE_2016A)
    assert isinstance(df.index, pd.DatetimeIndex)
    # Canonical flux names are present after alias normalisation.
    for flux in ("QN", "QH", "QE", "Kup", "Lup"):
        assert flux in df.columns, f"{flux} missing after canonicalisation"
    # 2012 is a leap year: DOY 1 hour 1 -> 2012-01-01 01:00.
    assert df.index[0] == pd.Timestamp("2012-01-01 01:00")
    # DOY 60 in a leap year -> 2012-02-29.
    assert df.index[2] == pd.Timestamp("2012-02-29 12:00")
    # The '%'-marker column maps to Year, not a stray '%iy' column.
    assert "Year" in df.columns and "%iy" not in df.columns


def test_2016a_star_token_and_summary():
    df = lo.read_legacy_output(FIXTURE_2016A)
    # kdown ******  -> NaN (it is the down-welling SW, not summarised, but parsed).
    assert np.isnan(df["Kdown"].iloc[-1])
    summary = lo.raw_flux_summary(df)
    assert set(summary.keys()) == {"QN", "QH", "QE", "Kup", "Lup"}
    qn = summary["QN"]["full"]
    assert qn["n"] == 5
    assert qn["min"] == -50.0
    assert qn["max"] == 330.0


def test_obs_path_reuses_bench_stats():
    df = lo.read_legacy_output(FIXTURE)
    # Build a trivial obs frame: obs == model for QH so MBE == 0 there.
    obs = pd.DataFrame(
        {
            "qh": df["QH"].to_numpy(),
            "qe": df["QE"].to_numpy(),
            "qn": df["QN"].to_numpy(),
            "kup": df["Kup"].to_numpy(),
            "lup": df["Lup"].to_numpy(),
        },
        index=df.index,
    )
    result = lo.stats_against_obs(df, obs)
    assert "stats" in result and "fingerprint" in result
    qh_full = result["stats"]["QH"]["full"]["all"]
    assert qh_full["MBE"] == 0.0
    assert qh_full["MAE"] == 0.0
    assert qh_full["n"] == 5
