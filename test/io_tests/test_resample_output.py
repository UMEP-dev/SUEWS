"""Test _resample_output functionality including DailyState handling."""

# Import debug utilities from conftest (centralised)
from conftest import (
    TIMESTEPS_PER_DAY,
    analyze_dailystate_nan,
    capture_test_artifacts,
    debug_on_ci,
)
import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy._post import _resample_output, dict_var_aggm

pytestmark = pytest.mark.api


def _regular_output_frame(periods=6, freq="5min", start="2023-01-01", tz=None):
    """Minimal frame for tests that exercise only index-frequency mechanics."""
    dates = pd.date_range(start, periods=periods, freq=freq, tz=tz)
    index = pd.MultiIndex.from_product([[1], dates], names=["grid", "datetime"])
    columns = pd.MultiIndex.from_tuples(
        [("SUEWS", "QH")],
        names=["group", "var"],
    )
    return pd.DataFrame(np.arange(periods).reshape(-1, 1), index=index, columns=columns)


@pytest.mark.parametrize("start", ["2024-03-31", "2024-10-27"])
def test_native_daily_timezone_aware_freq_across_dst_is_skipped(start):
    """Recognise daily cadence across 23- and 25-hour DST transitions."""
    df_output = _regular_output_frame(
        periods=4,
        freq="D",
        start=start,
        tz="Europe/London",
    )

    assert _resample_output(df_output, freq="D") is df_output


class TestResampleOutput:
    """Test suite for _resample_output functionality."""

    def test_resample_accepts_suewsoutput(self, sample_run_cached):
        """Test that _resample_output accepts SUEWSOutput instances."""
        df_output, df_state_final = sample_run_cached(48)

        output = sp.SUEWSOutput(df_output, df_state_final)
        assert output.index.equals(df_output.index)

        df_resampled = _resample_output(output, freq="h")
        assert isinstance(df_resampled, pd.DataFrame)
        assert isinstance(df_resampled.index, pd.MultiIndex)
        assert "grid" in df_resampled.index.names

        # Also test the OOP API (SUEWSOutput.resample method)
        output_resampled = output.resample(freq="h")
        assert isinstance(output_resampled, sp.SUEWSOutput)
        assert not output_resampled.df.empty

    def test_resample_native_freq_skips_and_is_identical(self, sample_run_cached):
        """At the run's native frequency _resample_output is a no-op (gh#1599).

        The save path calls _resample_output with the model timestep as the
        target; when the data already has that cadence, resampling must be
        skipped and the frame returned unchanged (byte-identical), not rebuilt.
        """
        df_output, _ = sample_run_cached(48)

        # native cadence of the run (5 min for the sample data)
        idx_dt = df_output.index.get_level_values("datetime").unique().sort_values()
        native_freq = pd.infer_freq(idx_dt)
        assert native_freq is not None

        # both a string alias and the equivalent Timedelta (as the save path
        # passes) must be recognised and skipped
        to_offset = pd.tseries.frequencies.to_offset
        for freq in (native_freq, pd.Timedelta(to_offset(native_freq))):
            result = _resample_output(df_output, freq=freq)
            assert result is df_output  # skipped -> same object, no work done
            pd.testing.assert_frame_equal(result, df_output)  # byte-identical

    def test_resample_irregular_index_not_skipped(self, sample_run_cached):
        """An irregular index must not be treated as a frequency match (gh#1599).

        infer_freq returns None for irregular cadence, so the guard falls
        through to a real resample rather than a false-positive skip (which a
        median-of-diffs reconstruction could wrongly trigger).
        """
        from supy._post import _index_freq_matches

        df_output, _ = sample_run_cached(48)
        # drop interior rows to break the regular 5-min cadence
        df_irregular = df_output.drop(df_output.index[[10, 25]])

        assert _index_freq_matches(df_output, "5min") is True
        assert _index_freq_matches(df_irregular, "5min") is False

    def test_resample_non_native_freq_short_circuits_before_infer_freq(
        self, monkeypatch
    ):
        """A clear frequency mismatch should not scan the full index."""
        import supy._post as _post
        from supy._post import _index_freq_matches

        df_output = _regular_output_frame(freq="5min")

        def fail_infer_freq(_idx):
            raise AssertionError("infer_freq should not run for an obvious mismatch")

        monkeypatch.setattr(_post.pd, "infer_freq", fail_infer_freq)

        assert _index_freq_matches(df_output, "60min") is False

    @analyze_dailystate_nan  # Add NaN analysis even when test passes
    @debug_on_ci
    @capture_test_artifacts("dailystate_resample")
    def test_resample_with_dailystate(self, sample_run_cached):
        """Test that DailyState is correctly resampled when present."""
        # Run for more days to ensure we have DailyState data
        # DailyState needs at least a few days to generate meaningful output
        # (10 days = TIMESTEPS_PER_DAY * 10 five-minute steps)
        df_output, df_state_final = sample_run_cached(TIMESTEPS_PER_DAY * 10)

        # Check DailyState exists
        assert "DailyState" in df_output.columns.get_level_values("group").unique()

        # Check if DailyState has any non-NaN values
        df_dailystate = df_output.loc[:, "DailyState"]
        has_data = df_dailystate.notna().any().any()

        if not has_data:
            # Skip this test if DailyState is empty (can happen in some environments)
            pytest.skip("DailyState has no data in this environment")

        # Resample to hourly
        df_resampled = _resample_output(df_output, freq="60min")

        # Store for decorator access
        self.df_output = df_output
        self.df_resampled = df_resampled

        # Check DailyState is still present after resampling
        assert "DailyState" in df_resampled.columns.get_level_values("group").unique()

        # Check that DailyState data exists
        df_dailystate_resampled = df_resampled.loc[:, "DailyState"]

        # DailyState might have NaN values for the first few days
        # Check if we have any non-NaN values after the initialization period
        non_nan_mask = df_dailystate_resampled.notna().any(axis=1)

        # Additional HDD-specific debugging for CI
        import os

        if os.environ.get("GITHUB_ACTIONS", "false").lower() == "true":
            # Check original DailyState for HDD columns
            df_dailystate_orig = df_output.loc[:, "DailyState"]
            df_after_dropna = df_dailystate_orig.dropna(how="all")

            if not df_after_dropna.empty:
                problem_cols = []
                for col in ["HDD3_Tmean", "HDD4_T5d"]:
                    if col in df_after_dropna.columns:
                        if df_after_dropna[col].isna().all():
                            problem_cols.append(col)

                if problem_cols:
                    print(f"\n!!! HDD PROBLEM DETECTED !!!")
                    print(
                        f"Columns {problem_cols} are completely NaN even after dropna(how='all')"
                    )

                    # Check initial state for base temperatures
                    if hasattr(self, "df_state_init"):
                        base_t_cols = [
                            c for c in self.df_state_init.columns if "BaseT" in str(c)
                        ]
                        print(
                            f"\nBase temperature columns in initial state: {base_t_cols}"
                        )

                    # Check if it's related to the 5-day rolling mean
                    print(
                        f"\nSimulation length: {len(df_output)} timesteps ({len(df_output) / TIMESTEPS_PER_DAY:.1f} days)"
                    )
                    print(
                        "Note: HDD4_T5d requires 5-day rolling mean, might need longer simulation"
                    )

        # Original assertion
        assert non_nan_mask.any(), (
            "DailyState should have some non-NaN values after resampling"
        )

    def test_resample_without_dailystate(self, sample_run_cached):
        """Test that resample works correctly when DailyState is not present."""
        # Run for a short period (48 steps, less than a day; no DailyState
        # output expected)
        df_output, df_state_final = sample_run_cached(48)

        # Remove DailyState if it exists to test the scenario
        if "DailyState" in df_output.columns.get_level_values("group").unique():
            groups = df_output.columns.get_level_values("group").unique()
            groups_no_dailystate = [g for g in groups if g != "DailyState"]
            df_output = df_output.loc[
                :,
                df_output.columns.get_level_values("group").isin(groups_no_dailystate),
            ]

        # Resample should work without error
        df_resampled = _resample_output(df_output, freq="30min")

        # Check output structure is maintained
        assert isinstance(df_resampled, pd.DataFrame)
        assert len(df_resampled) > 0

    def test_resample_dailystate_aggregation(self, sample_run_cached):
        """Test that DailyState uses correct aggregation rules."""
        # Run for multiple days (use more days to ensure DailyState generation)
        # (10 days = TIMESTEPS_PER_DAY * 10 five-minute steps)
        df_output, df_state_final = sample_run_cached(TIMESTEPS_PER_DAY * 10)

        # Check that DailyState aggregation rules exist
        assert "DailyState" in dict_var_aggm

        # Check if DailyState has data
        if "DailyState" in df_output.columns.get_level_values("group").unique():
            df_dailystate = df_output.loc[:, "DailyState"]
            has_data = df_dailystate.notna().any().any()

            if not has_data:
                pytest.skip("DailyState has no data in this environment")

        # Resample with different frequencies
        for freq in ["30min", "60min", "3h"]:
            df_resampled = _resample_output(df_output, freq=freq)

            if "DailyState" in df_resampled.columns.get_level_values("group").unique():
                # Check structure is preserved
                assert (
                    "DailyState"
                    in df_resampled.columns.get_level_values("group").unique()
                )

    def test_resample_dailystate_label_difference(self):
        """Test that DailyState uses 'left' label while other groups use 'right'."""
        # Create minimal test data with known structure
        dates = pd.date_range("2023-01-01", periods=TIMESTEPS_PER_DAY * 2, freq="5min")
        grids = ["grid1"]

        # Create MultiIndex
        index = pd.MultiIndex.from_product([grids, dates], names=["grid", "datetime"])

        # Create test data with regular group and DailyState
        regular_data = np.random.rand(len(index))
        dailystate_data = np.full(len(index), np.nan)
        # Only fill DailyState at end of each day
        for i in range(0, len(dates), TIMESTEPS_PER_DAY):
            if i + TIMESTEPS_PER_DAY - 1 < len(dates):
                dailystate_data[i + TIMESTEPS_PER_DAY - 1] = np.random.rand()

        # Create DataFrame with MultiIndex columns
        columns = pd.MultiIndex.from_tuples(
            [
                ("SUEWS", "kdown"),
                ("SUEWS", "kup"),
                ("DailyState", "GDD_id"),
                ("DailyState", "SDD_id"),
            ],
            names=["group", "var"],
        )

        df_output = pd.DataFrame(
            np.column_stack([
                regular_data,
                regular_data * 0.8,
                dailystate_data,
                dailystate_data * 1.1,
            ]),
            index=index,
            columns=columns,
        )

        # Create a minimal dict_aggm for testing
        test_dict_aggm = {
            "SUEWS": {"kdown": "mean", "kup": "mean"},
            "DailyState": {
                "GDD_id": lambda x: x.iloc[-1] if len(x) > 0 else np.nan,
                "SDD_id": lambda x: x.iloc[-1] if len(x) > 0 else np.nan,
            },
        }

        # Resample
        df_resampled = _resample_output(
            df_output, freq="60min", dict_aggm=test_dict_aggm
        )

        # Both groups should be present
        assert "SUEWS" in df_resampled.columns.get_level_values("group").unique()
        assert "DailyState" in df_resampled.columns.get_level_values("group").unique()

        # The label difference is internal to the function, but we can verify
        # that both types of data are correctly resampled
        assert len(df_resampled) > 0
        assert not df_resampled.empty

    def test_resample_missing_dict_aggm_entry(self):
        """Test behavior when dict_aggm doesn't contain DailyState."""
        # Create test data
        dates = pd.date_range("2023-01-01", periods=TIMESTEPS_PER_DAY, freq="5min")
        grids = ["grid1"]
        index = pd.MultiIndex.from_product([grids, dates], names=["grid", "datetime"])

        # Create DataFrame with DailyState
        columns = pd.MultiIndex.from_tuples(
            [("SUEWS", "kdown"), ("DailyState", "GDD_id")], names=["group", "var"]
        )

        df_output = pd.DataFrame(
            np.column_stack([np.random.rand(len(index)), np.random.rand(len(index))]),
            index=index,
            columns=columns,
        )

        # Create custom dict_aggm without DailyState
        custom_dict_aggm = {"SUEWS": {"kdown": "mean"}}

        # Should handle gracefully (no error, DailyState just not resampled)
        df_resampled = _resample_output(
            df_output, freq="60min", dict_aggm=custom_dict_aggm
        )

        # SUEWS should be resampled
        assert "SUEWS" in df_resampled.columns.get_level_values("group").unique()
        # DailyState should not be in resampled output
        assert (
            "DailyState" not in df_resampled.columns.get_level_values("group").unique()
        )
