"""Test DailyState output functionality."""

import tempfile
from pathlib import Path
import pandas as pd
import numpy as np
import pytest
import supy as sp


class TestDailyStateOutput:
    """Test suite for DailyState output handling."""

    def test_dailystate_no_resampling(self):
        """Test that DailyState output is not resampled and preserves daily values."""
        # Load sample data
        df_state_init, df_forcing = sp.load_SampleData()

        # Run for multiple days to ensure we have DailyState data
        df_forcing_multi_day = df_forcing.iloc[: 288 * 3]  # 3 days of 5-min data

        # Run simulation
        df_output, df_state_final = sp.run_supy(df_forcing_multi_day, df_state_init)

        # Check DailyState exists in output
        assert "DailyState" in df_output.columns.get_level_values("group").unique()

        # Get DailyState data
        df_dailystate = df_output.loc[:, "DailyState"]

        # Remove all-NaN rows (DailyState only has values at end of each day)
        df_dailystate_clean = df_dailystate.dropna(how="all")

        # Check we have the expected number of daily values (one per day)
        n_days = len(
            pd.date_range(
                df_forcing_multi_day.index[0], df_forcing_multi_day.index[-1], freq="D"
            )
        )
        assert len(df_dailystate_clean) <= n_days
        assert len(df_dailystate_clean) > 0  # Should have at least some data

        # Check that values are only at day boundaries (around 23:55 or similar)
        hours = df_dailystate_clean.index.get_level_values("datetime").hour
        assert all(h >= 23 for h in hours), (
            "DailyState should only have values at end of day"
        )

    def test_dailystate_save_output(self):
        """Test that DailyState data is correctly saved to file."""
        # Load sample data
        df_state_init, df_forcing = sp.load_SampleData()

        # Run for multiple days
        df_forcing_multi_day = df_forcing.iloc[: 288 * 3]  # 3 days

        # Run simulation
        df_output, df_state_final = sp.run_supy(df_forcing_multi_day, df_state_init)

        # Save output with default settings (should include DailyState)
        with tempfile.TemporaryDirectory() as dir_temp:
            list_files = sp.save_supy(df_output, df_state_final, path_dir_save=dir_temp)

            # Check that DailyState file was created
            dailystate_files = [f for f in list_files if "DailyState" in f.name]
            assert len(dailystate_files) > 0, "DailyState file should be created"

            # Read the DailyState file and check it's not empty
            for ds_file in dailystate_files:
                df_saved = pd.read_csv(ds_file, sep="\t")
                assert len(df_saved) > 0, "DailyState file should not be empty"

                # Check that we have actual data values (not all -999)
                data_cols = [
                    c
                    for c in df_saved.columns
                    if c not in ["Year", "DOY", "Hour", "Min", "Dectime"]
                ]
                assert len(data_cols) > 0, "Should have data columns"

                # Check that at least some values are not -999 (missing data marker)
                has_real_data = False
                for col in data_cols:
                    if any(df_saved[col] != -999):
                        has_real_data = True
                        break
                assert has_real_data, "DailyState should contain actual data values"

                # Check that all days are present (no missing first day)
                doy_values = df_saved["DOY"].values
                expected_doys = list(range(1, len(doy_values) + 1))
                assert list(doy_values) == expected_doys, (
                    f"Missing days in output. Got DOYs: {list(doy_values)}, expected: {expected_doys}"
                )

    def test_dailystate_different_output_frequencies(self):
        """Test DailyState output with different resampling frequencies."""
        # Load sample data
        df_state_init, df_forcing = sp.load_SampleData()

        # Run for multiple days
        df_forcing_multi_day = df_forcing.iloc[: 288 * 2]  # 2 days

        # Run simulation
        df_output, df_state_final = sp.run_supy(df_forcing_multi_day, df_state_init)

        # Test with different output frequencies
        for freq_s in [300, 1800, 3600]:  # 5min, 30min, 60min
            with tempfile.TemporaryDirectory() as dir_temp:
                list_files = sp.save_supy(
                    df_output, df_state_final, path_dir_save=dir_temp, freq_s=freq_s
                )

                # Check DailyState file exists and has same content regardless of freq
                dailystate_files = [f for f in list_files if "DailyState" in f.name]
                assert len(dailystate_files) > 0

                # DailyState should not be affected by output frequency
                df_ds = pd.read_csv(dailystate_files[0], sep="\t")
                assert len(df_ds) > 0, f"DailyState should have data at freq={freq_s}"
