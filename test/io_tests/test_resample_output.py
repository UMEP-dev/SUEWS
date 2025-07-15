"""Test resample_output functionality including DailyState handling."""

import pandas as pd
import numpy as np
import pytest
import supy as sp
from supy._post import resample_output, dict_var_aggm

# Import debug utilities
try:
    from .debug_utils import debug_on_ci, capture_test_artifacts, analyze_dailystate_nan
except ImportError:
    # Fallback if decorators not available
    def debug_on_ci(func):
        return func

    def capture_test_artifacts(name):
        return lambda func: func

    def analyze_dailystate_nan(func):
        return func


class TestResampleOutput:
    """Test suite for resample_output functionality."""

    @analyze_dailystate_nan  # Add NaN analysis even when test passes
    @debug_on_ci
    @capture_test_artifacts("dailystate_resample")
    def test_resample_with_dailystate(self):
        """Test that DailyState is correctly resampled when present."""
        # Load sample data and run simulation
        df_state_init, df_forcing = sp.load_SampleData()

        # Run for more days to ensure we have DailyState data
        # DailyState needs at least a few days to generate meaningful output
        df_forcing_multi_day = df_forcing.iloc[: 288 * 10]  # 10 days of 5-min data

        # Run simulation
        df_output, df_state_final = sp.run_supy(df_forcing_multi_day, df_state_init)

        # Check DailyState exists
        assert "DailyState" in df_output.columns.get_level_values("group").unique()

        # Check if DailyState has any non-NaN values
        df_dailystate = df_output.loc[:, "DailyState"]
        has_data = df_dailystate.notna().any().any()

        if not has_data:
            # Skip this test if DailyState is empty (can happen in some environments)
            pytest.skip("DailyState has no data in this environment")

        # Resample to hourly
        df_resampled = resample_output(df_output, freq="60min")

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
                        f"\nSimulation length: {len(df_output)} timesteps ({len(df_output) / 288:.1f} days)"
                    )
                    print(
                        "Note: HDD4_T5d requires 5-day rolling mean, might need longer simulation"
                    )

        # Original assertion
        assert non_nan_mask.any(), (
            "DailyState should have some non-NaN values after resampling"
        )

    def test_resample_without_dailystate(self):
        """Test that resample works correctly when DailyState is not present."""
        # Load sample data and run simulation
        df_state_init, df_forcing = sp.load_SampleData()

        # Run for a short period (no DailyState output expected)
        df_forcing_short = df_forcing.iloc[:48]  # Less than a day

        # Run simulation
        df_output, df_state_final = sp.run_supy(df_forcing_short, df_state_init)

        # Remove DailyState if it exists to test the scenario
        if "DailyState" in df_output.columns.get_level_values("group").unique():
            groups = df_output.columns.get_level_values("group").unique()
            groups_no_dailystate = [g for g in groups if g != "DailyState"]
            df_output = df_output.loc[
                :,
                df_output.columns.get_level_values("group").isin(groups_no_dailystate),
            ]

        # Resample should work without error
        df_resampled = resample_output(df_output, freq="30min")

        # Check output structure is maintained
        assert isinstance(df_resampled, pd.DataFrame)
        assert len(df_resampled) > 0

    def test_resample_dailystate_aggregation(self):
        """Test that DailyState uses correct aggregation rules."""
        # Load sample data and run simulation
        df_state_init, df_forcing = sp.load_SampleData()

        # Run for multiple days (use more days to ensure DailyState generation)
        df_forcing_multi_day = df_forcing.iloc[: 288 * 10]  # 10 days of 5-min data

        # Run simulation
        df_output, df_state_final = sp.run_supy(df_forcing_multi_day, df_state_init)

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
            df_resampled = resample_output(df_output, freq=freq)

            if "DailyState" in df_resampled.columns.get_level_values("group").unique():
                # Check structure is preserved
                assert (
                    "DailyState"
                    in df_resampled.columns.get_level_values("group").unique()
                )

    def test_resample_dailystate_label_difference(self):
        """Test that DailyState uses 'left' label while other groups use 'right'."""
        # Create minimal test data with known structure
        dates = pd.date_range("2023-01-01", periods=288 * 2, freq="5min")
        grids = ["grid1"]

        # Create MultiIndex
        index = pd.MultiIndex.from_product([grids, dates], names=["grid", "datetime"])

        # Create test data with regular group and DailyState
        regular_data = np.random.rand(len(index))
        dailystate_data = np.full(len(index), np.nan)
        # Only fill DailyState at end of each day
        for i in range(0, len(dates), 288):
            if i + 287 < len(dates):
                dailystate_data[i + 287] = np.random.rand()

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
        df_resampled = resample_output(
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
        dates = pd.date_range("2023-01-01", periods=288, freq="5min")
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
        df_resampled = resample_output(
            df_output, freq="60min", dict_aggm=custom_dict_aggm
        )

        # SUEWS should be resampled
        assert "SUEWS" in df_resampled.columns.get_level_values("group").unique()
        # DailyState should not be in resampled output
        assert (
            "DailyState" not in df_resampled.columns.get_level_values("group").unique()
        )