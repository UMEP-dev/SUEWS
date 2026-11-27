"""Behavioural tests for ``supy.util._gap_filler`` and ``supy.util._debug``."""

from __future__ import annotations

import io
import json
import os
import tempfile
import zipfile

import numpy as np
import pandas as pd
import pytest

from supy.util._debug import save_zip_debug
from supy.util._gap_filler import (
    cal_ratio_info,
    fill_gap_all,
    fill_gap_all_x,
    fill_gap_one,
    loc_gap,
    norm_ser_dt,
)

pytestmark = [pytest.mark.api, pytest.mark.contract("C-FORCING", "C-SAVE")]


def _make_diurnal_series(n_days: int = 4, freq: str = "1h") -> pd.Series:
    """Create a deterministic positive diurnal series spanning ``n_days`` full days."""
    periods = int(pd.Timedelta(days=n_days) / pd.Timedelta(freq))
    idx = pd.date_range("2023-07-01 00:00", periods=periods, freq=freq)
    hour_frac = idx.hour + idx.minute / 60.0
    values = 15.0 + 10.0 * np.sin(2.0 * np.pi * (hour_frac - 6.0) / 24.0)
    return pd.Series(values, index=idx, dtype=float)


class TestSaveZipDebug:
    """Tests for ``save_zip_debug`` packaging and error-report handling."""

    def test_save_zip_debug_bundles_dataframes_and_version_json(
        self, tmp_path, monkeypatch
    ):
        """Without error_info, bundle holds pickled frames and valid supy_info.json."""
        monkeypatch.chdir(tmp_path)
        df_forcing = pd.DataFrame({"Tair": [10.0, 12.0], "RH": [55.0, 60.0]})
        df_state = pd.DataFrame({"grid": [1], "sfr_paved": [0.4]})

        zip_path = save_zip_debug(df_forcing, df_state)

        assert zip_path.parent == tmp_path
        assert zip_path.name.startswith("supy_debug-")
        assert zip_path.suffix == ".zip"
        assert zip_path.is_file()

        with zipfile.ZipFile(zip_path, "r") as zf:
            names = set(zf.namelist())
            assert names == {"df_forcing.pkl", "df_state_init.pkl", "supy_info.json"}
            info = json.loads(zf.read("supy_info.json").decode("utf-8"))
            assert "supy" in info
            archived_forcing = pd.read_pickle(io.BytesIO(zf.read("df_forcing.pkl")))
            archived_state = pd.read_pickle(io.BytesIO(zf.read("df_state_init.pkl")))

        pd.testing.assert_frame_equal(archived_forcing, df_forcing)
        pd.testing.assert_frame_equal(archived_state, df_state)

    def test_save_zip_debug_with_exception_writes_traceback_report(
        self, tmp_path, monkeypatch
    ):
        """Passing a raised Exception records its type, message, and traceback."""
        monkeypatch.chdir(tmp_path)
        df_forcing = pd.DataFrame({"kdown": [0.0, 100.0]})
        df_state = pd.DataFrame({"grid": [1]})

        captured_exc: Exception | None = None
        try:
            raise ValueError("synthetic forcing error")
        except ValueError as exc:
            captured_exc = exc

        zip_path = save_zip_debug(df_forcing, df_state, error_info=captured_exc)

        with zipfile.ZipFile(zip_path, "r") as zf:
            assert "error_info.md" in zf.namelist()
            report = zf.read("error_info.md").decode("utf-8")

        assert "# SuPy Error Report" in report
        assert "## Error Type" in report
        assert "`ValueError`" in report
        assert "synthetic forcing error" in report
        assert "## Traceback" in report
        assert "test_save_zip_debug_with_exception_writes_traceback_report" in report

    def test_save_zip_debug_with_string_error_info(self, tmp_path, monkeypatch):
        """Passing a plain string records it under 'Error Details'."""
        monkeypatch.chdir(tmp_path)
        df_forcing = pd.DataFrame({"kdown": [50.0]})
        df_state = pd.DataFrame({"grid": [1]})

        zip_path = save_zip_debug(
            df_forcing, df_state, error_info="custom diagnostic note"
        )

        with zipfile.ZipFile(zip_path, "r") as zf:
            report = zf.read("error_info.md").decode("utf-8")

        assert "## Error Details" in report
        assert "custom diagnostic note" in report

    def test_save_zip_debug_falls_back_to_tempdir_when_cwd_unwritable(
        self, tmp_path, monkeypatch
    ):
        """When cwd cannot be written, artefacts are saved under tempfile.gettempdir()."""
        if os.name == "nt" or (hasattr(os, "geteuid") and os.geteuid() == 0):
            pytest.skip(
                "POSIX non-root directory permissions required for read-only cwd"
            )

        ro_dir = tmp_path / "readonly_cwd"
        ro_dir.mkdir()
        fallback_dir = tmp_path / "fallback_tmp"
        fallback_dir.mkdir()

        monkeypatch.setattr(tempfile, "tempdir", str(fallback_dir))
        ro_dir.chmod(0o555)
        try:
            monkeypatch.chdir(ro_dir)
            df_forcing = pd.DataFrame({"Tair": [18.0]})
            df_state = pd.DataFrame({"grid": [1]})

            zip_path = save_zip_debug(df_forcing, df_state)
            assert zip_path.parent == fallback_dir
            assert zip_path.is_file()
        finally:
            ro_dir.chmod(0o755)


class TestGapFillerHelpers:
    """Tests for gap-locating, normalising, and single-gap filling primitives."""

    def test_norm_ser_dt_expands_partial_days_to_complete_diurnal_cycles(self):
        """norm_ser_dt pads leading/trailing missing hours to full calendar days."""
        idx = pd.date_range("2023-07-01 06:00", "2023-07-02 18:00", freq="1h")
        ser = pd.Series(np.arange(len(idx), dtype=float), index=idx)

        ser_norm = norm_ser_dt(ser)

        assert len(ser_norm) == 48
        assert ser_norm.index[0] == pd.Timestamp("2023-07-01 00:00")
        assert ser_norm.index[-1] == pd.Timestamp("2023-07-02 23:00")
        assert ser_norm.iloc[:6].isna().all()
        assert ser_norm.iloc[-5:].isna().all()
        np.testing.assert_allclose(ser_norm.loc[ser.index].values, ser.values)

    def test_cal_ratio_info_computes_fraction_of_gap_free_days(self):
        """cal_ratio_info returns 1 - (gapped_days / total_days)."""
        ser = _make_diurnal_series(n_days=4)
        assert cal_ratio_info(ser) == pytest.approx(1.0)

        ser.iloc[24 + 5] = np.nan  # 1 of 4 days gapped
        assert cal_ratio_info(ser) == pytest.approx(0.75)

        ser.iloc[48 + 3] = np.nan  # 2 of 4 days gapped
        assert cal_ratio_info(ser) == pytest.approx(0.50)

    def test_loc_gap_identifies_patterns_and_rejects_excessive_gaps(self):
        """loc_gap locates '010', '01', and '10' patterns and raises if ratio < 0.5."""
        # Pattern 010: day 1 gapped, days 0 & 2 & 3 valid.
        ser_010 = _make_diurnal_series(n_days=4)
        ser_010.iloc[24 + 4 : 24 + 8] = np.nan
        loc_010 = loc_gap(ser_010, freq="1D", pattern="010")
        assert list(loc_010) == list(pd.date_range("2023-07-01", periods=3, freq="1D"))

        # Pattern 01: last day gapped.
        ser_01 = _make_diurnal_series(n_days=3)
        ser_01.iloc[48 + 2] = np.nan
        loc_01 = loc_gap(ser_01, freq="1D", pattern="01")
        assert list(loc_01) == list(pd.date_range("2023-07-02", periods=2, freq="1D"))

        # Pattern 10: first day gapped.
        ser_10 = _make_diurnal_series(n_days=3)
        ser_10.iloc[3] = np.nan
        loc_10 = loc_gap(ser_10, freq="1D", pattern="10")
        assert list(loc_10) == list(pd.date_range("2023-07-01", periods=2, freq="1D"))

        # Too many gaps: 3 of 4 days gapped -> ratio 0.25 < 0.5.
        ser_bad = _make_diurnal_series(n_days=4)
        ser_bad.iloc[1] = np.nan
        ser_bad.iloc[25] = np.nan
        ser_bad.iloc[49] = np.nan
        with pytest.raises(RuntimeError, match="too many gaps"):
            loc_gap(ser_bad, freq="1D", pattern="010")

    def test_fill_gap_one_010_rescales_partial_gap_analytically(self):
        """When day 1 is 2x its neighbours with <50% missing, rescaling recovers 2x."""
        ser = _make_diurnal_series(n_days=3)
        day1_truth = ser.iloc[24:48] * 2.0
        ser.iloc[24:48] = day1_truth
        gap_slice = slice(24 + 8, 24 + 14)  # 6 of 24 hours missing -> >50% present
        ser.iloc[gap_slice] = np.nan

        filled = fill_gap_one(ser, freq="1D", pattern="010")

        assert not filled.isna().any()
        np.testing.assert_allclose(
            filled.iloc[gap_slice].values,
            day1_truth.iloc[8:14].values,
            rtol=1e-12,
        )

    def test_fill_gap_one_010_clamps_extreme_or_nan_scale_and_handles_large_gap(self):
        """Extreme/NaN scale_fill and >50%-missing days fall back to unscaled median."""
        # Case A: scale_fill > 10 (day 1 tiny compared to neighbours) -> clamped to 1.
        ser_small = _make_diurnal_series(n_days=3)
        base_day = ser_small.iloc[:24].copy()
        ser_small.iloc[24:48] = base_day * 0.01  # ratio fill/gap = 100 > 10
        ser_small.iloc[24 + 6 : 24 + 10] = np.nan
        filled_small = fill_gap_one(ser_small, freq="1D", pattern="010")
        np.testing.assert_allclose(
            filled_small.iloc[24 + 6 : 24 + 10].values,
            base_day.iloc[6:10].values,
            rtol=1e-12,
        )

        # Case B: scale_fill < 0.1 (day 1 huge compared to neighbours) -> clamped to 1.
        ser_large = _make_diurnal_series(n_days=3)
        ser_large.iloc[24:48] = base_day * 50.0  # ratio fill/gap = 0.02 < 0.1
        ser_large.iloc[24 + 6 : 24 + 10] = np.nan
        filled_large = fill_gap_one(ser_large, freq="1D", pattern="010")
        np.testing.assert_allclose(
            filled_large.iloc[24 + 6 : 24 + 10].values,
            base_day.iloc[6:10].values,
            rtol=1e-12,
        )

        # Case C: NaN scale_fill (0/0 on all present timesteps) -> clamped to 1.
        idx_zero = pd.date_range("2023-07-01 00:00", periods=72, freq="1h")
        vals_zero = np.zeros(72, dtype=float)
        # Non-zero only on hours 6..9 of neighbouring days; day 1 hours 6..9 are missing.
        vals_zero[6:10] = 12.0
        vals_zero[48 + 6 : 48 + 10] = 12.0
        ser_nan_scale = pd.Series(vals_zero, index=idx_zero)
        ser_nan_scale.iloc[24 + 6 : 24 + 10] = np.nan
        filled_nan_scale = fill_gap_one(ser_nan_scale, freq="1D", pattern="010")
        np.testing.assert_allclose(
            filled_nan_scale.iloc[24 + 6 : 24 + 10].values,
            np.full(4, 12.0),
            rtol=1e-12,
        )

        # Case D: >50% missing on gapped day -> skips rescaling branch (scale_fill=1).
        ser_heavy = _make_diurnal_series(n_days=3)
        ser_heavy.iloc[24:48] = base_day * 2.0
        ser_heavy.iloc[24 + 4 : 24 + 20] = np.nan  # 16 of 24 missing
        filled_heavy = fill_gap_one(ser_heavy, freq="1D", pattern="010")
        np.testing.assert_allclose(
            filled_heavy.iloc[24 + 4 : 24 + 20].values,
            base_day.iloc[4:20].values,
            rtol=1e-12,
        )

        # Case E: no matching pattern returns input unchanged.
        ser_clean = _make_diurnal_series(n_days=3)
        unchanged = fill_gap_one(ser_clean, freq="1D", pattern="010")
        pd.testing.assert_series_equal(unchanged, ser_clean)

    def test_fill_gap_one_edge_patterns_01_and_10(self):
        """Boundary gaps ('01' at end, '10' at start) are filled from adjacent valid day."""
        base = _make_diurnal_series(n_days=3)

        # Pattern '01': gap on final day.
        ser_end = base.copy()
        ser_end.iloc[48 + 5 : 48 + 12] = np.nan
        filled_end = fill_gap_one(ser_end, freq="1D", pattern="01")
        assert not filled_end.isna().any()
        np.testing.assert_allclose(
            filled_end.iloc[48 + 5 : 48 + 12].values,
            base.iloc[48 + 5 : 48 + 12].values,
            rtol=1e-12,
        )

        # Pattern '10': gap on first day.
        ser_start = base.copy()
        ser_start.iloc[5:12] = np.nan
        filled_start = fill_gap_one(ser_start, freq="1D", pattern="10")
        assert not filled_start.isna().any()
        np.testing.assert_allclose(
            filled_start.iloc[5:12].values,
            base.iloc[5:12].values,
            rtol=1e-12,
        )


class TestFillGapAll:
    """Tests for iterative and recursive gap filling across multi-day series."""

    def test_fill_gap_all_x_short_series_with_and_without_preinterpolation(self):
        """limit_fill=1 pre-interpolates 1-step NaNs; limit_fill=0 uses day matching."""
        base = _make_diurnal_series(n_days=4)
        ser = base.copy()
        ser.iloc[24 + 3] = np.nan  # single-step gap
        ser.iloc[48 + 6 : 48 + 10] = np.nan  # multi-step gap

        filled_interp = fill_gap_all_x(ser, freq="1D", limit_fill=1)
        filled_no_interp = fill_gap_all_x(ser, freq="1D", limit_fill=0)

        assert not filled_interp.isna().any()
        assert not filled_no_interp.isna().any()
        # With limit_fill=0, the single-step gap is filled from identical neighbouring days.
        assert filled_no_interp.iloc[24 + 3] == pytest.approx(
            base.iloc[24 + 3], rel=1e-12
        )
        # With limit_fill=1, the single-step gap is linearly interpolated between neighbours.
        expected_lin = 0.5 * (base.iloc[24 + 2] + base.iloc[24 + 4])
        assert filled_interp.iloc[24 + 3] == pytest.approx(expected_lin, rel=1e-12)

    def test_fill_gap_all_x_returns_gapped_series_when_too_few_valid_days(self):
        """When valid-day ratio < 0.5 in a <=4-day chunk, fill_gap_all_x returns gracefully."""
        ser = _make_diurnal_series(n_days=3)
        ser.iloc[2:6] = np.nan
        ser.iloc[26:30] = np.nan  # 2 of 3 days gapped -> ratio 0.33 < 0.5

        res = fill_gap_all_x(ser, freq="1D", limit_fill=0)
        assert res.isna().sum() == ser.isna().sum()

    def test_fill_gap_all_x_recursive_split_with_and_without_residual(self):
        """Series > 4 days split recursively, preserving both exact and residual lengths.

        Note: recursive calls at ``_gap_filler.py:160-161`` omit ``limit_fill`` and
        default to ``limit_fill=1``, so each recursion level applies one linear
        interpolation step before neighbouring-day matching. This test verifies the
        complete filled series (including every missing timestep) under ``limit_fill=1``.
        """
        # Exact 6 days (1 split into 3 + 3 days, no residual): two interpolation
        # passes (top level + child chunk) fill the first 2 missing steps per gap,
        # and neighbouring-day matching recovers the remaining missing steps exactly.
        base_exact = _make_diurnal_series(n_days=6)
        ser_exact = base_exact.copy()
        ser_exact.iloc[24 + 4 : 24 + 10] = np.nan
        ser_exact.iloc[96 + 8 : 96 + 14] = np.nan

        expected_exact = base_exact.copy()
        interp_two_levels = ser_exact.interpolate(limit=1).interpolate(limit=1)
        expected_exact.iloc[24 + 4 : 24 + 6] = interp_two_levels.iloc[24 + 4 : 24 + 6]
        expected_exact.iloc[96 + 8 : 96 + 10] = interp_two_levels.iloc[96 + 8 : 96 + 10]

        filled_exact = fill_gap_all_x(ser_exact, freq="1D", limit_fill=1)
        assert len(filled_exact) == len(base_exact)
        assert not filled_exact.isna().any()
        np.testing.assert_allclose(
            filled_exact.values, expected_exact.values, rtol=1e-12
        )

        # 5 days + 6 trailing hours (exercises residual tail branch).
        idx_res = pd.date_range("2023-07-01 00:00", periods=5 * 24 + 6, freq="1h")
        hour_frac = idx_res.hour.astype(float)
        base_res = pd.Series(
            15.0 + 10.0 * np.sin(2.0 * np.pi * (hour_frac - 6.0) / 24.0),
            index=idx_res,
        )
        ser_res = base_res.copy()
        ser_res.iloc[24 + 4 : 24 + 8] = np.nan

        expected_res = base_res.copy()
        res_interp_two_levels = ser_res.interpolate(limit=1).interpolate(limit=1)
        expected_res.iloc[24 + 4 : 24 + 6] = res_interp_two_levels.iloc[24 + 4 : 24 + 6]

        filled_res = fill_gap_all_x(ser_res, freq="1D", limit_fill=1)
        assert len(filled_res) == len(base_res)
        assert not filled_res.isna().any()
        np.testing.assert_allclose(filled_res.values, expected_res.values, rtol=1e-12)

    def test_fill_gap_all_end_to_end_and_threshold_error(self):
        """fill_gap_all fills multi-day partial-boundary series and enforces thresh_ratio."""
        # 7 days starting at 04:00 (partial day 0) with a multi-hour gap on day 4:
        # split produces days 0-2 (1/3 gapped) and days 3-6 (1/4 gapped), so both
        # halves satisfy the >=0.5 valid-day ratio on the first pass.
        full = _make_diurnal_series(n_days=7)
        ser = full.iloc[4:].copy()
        truth = ser.copy()
        gap_start = 4 * 24 - 4 + 5  # day 4, hour 05:00 in sliced series
        gap_end = gap_start + 7
        ser.iloc[gap_start:gap_end] = np.nan

        expected = truth.copy()
        interp_two_levels = ser.interpolate(limit=1).interpolate(limit=1)
        expected.iloc[gap_start : gap_start + 2] = interp_two_levels.iloc[
            gap_start : gap_start + 2
        ]

        filled = fill_gap_all(ser, freq="1D", limit_fill=1, thresh_ratio=0.8)

        assert list(filled.index) == list(ser.index)
        assert not filled.isna().any()
        np.testing.assert_allclose(filled.values, expected.values, rtol=1e-12)

        # Multi-pass case: 2 adjacent 3-hour gaps in the first 3-day half trigger
        # the outer retry loop in fill_gap_all (while ser_test_filled.isna().any()).
        ser_multi = _make_diurnal_series(n_days=6)
        ser_multi.iloc[2:5] = np.nan
        ser_multi.iloc[26:29] = np.nan
        non_gap_multi = ~ser_multi.isna()
        filled_multi = fill_gap_all(
            ser_multi, freq="1D", limit_fill=1, thresh_ratio=0.6
        )
        assert not filled_multi.isna().any()
        np.testing.assert_allclose(
            filled_multi[non_gap_multi].values,
            ser_multi[non_gap_multi].values,
            rtol=1e-12,
        )

        # Below thresh_ratio raises RuntimeError.
        ser_too_gapped = _make_diurnal_series(n_days=4)
        ser_too_gapped.iloc[24 + 2] = np.nan
        ser_too_gapped.iloc[48 + 2] = np.nan  # ratio = 0.5 < 0.8
        with pytest.raises(RuntimeError, match="input series is too gapped"):
            fill_gap_all(ser_too_gapped, thresh_ratio=0.8)
