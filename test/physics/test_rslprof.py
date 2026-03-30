"""
Test suews_phys_rslprof - RSL/MOST wind profile calculations.

Regression tests for Issue #572 where the MIN constraint on height arrays
caused negative LOG arguments and negative wind speeds for tall buildings.
"""

import logging
import warnings

import numpy as np
import pytest

import supy as sp


class TestWindProfiles:
    """Test suite for wind profile calculations."""

    @pytest.fixture
    def sample_data(self):
        """Load sample data for testing."""
        df_state_init, df_forcing = sp.load_SampleData()
        return df_state_init, df_forcing

    def test_most_height_array_monotonic(self, sample_data):
        """Test that MOST generates monotonic height arrays for various building heights."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:1]

        for bldgh in [2.0, 10.0, 50.0]:
            df_state_init.loc[:, "bldgh"] = bldgh
            df_state_init.loc[:, "rslmethod"] = 0  # MOST diagnostic

            df_output, _ = sp.run_supy(df_forcing_short, df_state_init)

            z_cols = [col for col in df_output.columns if col[1].startswith("z_")]
            heights = df_output[z_cols].iloc[0].values
            heights = heights[~np.isnan(heights)]

            assert np.all(np.diff(heights) > 0), (
                f"Height array not monotonic for {bldgh}m building"
            )

    def test_most_and_rsl_methods_run(self, sample_data):
        """Test that both MOST and RSL methods run without errors."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:1]
        df_state_init.loc[:, "bldgh"] = 10.0

        # Test MOST
        df_state_init.loc[:, "rslmethod"] = 0
        df_output_most, _ = sp.run_supy(df_forcing_short, df_state_init)
        assert not df_output_most.empty, "MOST failed to run"

        # Test RSL
        df_state_init.loc[:, "rslmethod"] = 1
        df_output_rsl, _ = sp.run_supy(df_forcing_short, df_state_init)
        assert not df_output_rsl.empty, "RSL failed to run"

    @pytest.mark.parametrize(
        "zh,z0m_in,zdm_in",
        [
            (2.0, 0.1, 0.1),  # Very low buildings
            (5.0, 0.5, 3.5),  # zdm > building height scenario
            (10.0, 1.0, 7.0),  # Standard case
            (1.0, 0.01, 0.01),  # Extremely low values
            (2.0, 0.01, 0.01),  # Original test case from issue
        ],
    )
    def test_wind_speeds_positive(self, sample_data, zh, z0m_in, zdm_in):
        """Test that wind speeds are always positive for various building configs."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:48]

        df_state_init.loc[:, "zh"] = zh
        df_state_init.loc[:, "z0m_in"] = z0m_in
        df_state_init.loc[:, "zdm_in"] = zdm_in
        df_state_init.loc[:, "rslmethod"] = 0
        df_state_init.loc[:, "stabilitymethod"] = 2

        df_output, _ = sp.run_supy(df_forcing_short, df_state_init, save_state=False)

        # Check U10 wind speeds
        if "SUEWS" in df_output.columns.get_level_values(0):
            suews_data = df_output.xs("SUEWS", level=0, axis=1)
            if "U10" in suews_data.columns:
                u10_valid = suews_data["U10"].dropna()
                if len(u10_valid) > 0:
                    assert (u10_valid >= 0).all(), (
                        f"U10 negative for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"
                    )

    def test_tall_building_no_negative_winds(self, sample_data):
        """Test that tall buildings don't cause negative wind speeds."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:1]

        # Tall building configuration
        df_state_init.loc[:, "bldgh"] = 50.0
        df_state_init.loc[:, "z0m_in"] = 5.0
        df_state_init.loc[:, "zdm_in"] = 35.0
        df_state_init.loc[:, "z"] = 60.0  # Above building height
        df_state_init.loc[:, "rslmethod"] = 0

        df_output, _ = sp.run_supy(df_forcing_short, df_state_init)

        u_cols = [col for col in df_output.columns if col[1].startswith("U_")]
        if u_cols:
            wind_speeds = df_output[u_cols].iloc[0].values
            valid_winds = wind_speeds[~np.isnan(wind_speeds)]
            assert np.all(valid_winds >= 0), "Negative wind speeds detected"

    def test_full_building_auto_rsl_grid_runs_without_nan(self, sample_data):
        """Regression test for GH#1223 full-building grids in the auto MOST path."""
        df_state_init, df_forcing = sample_data
        df_state_issue = df_state_init.iloc[[0]].copy()
        df_forcing_short = df_forcing.iloc[:2]

        for idx in range(7):
            df_state_issue.loc[:, ("sfr_surf", f"({idx},)")] = 0.0
        df_state_issue.loc[:, ("sfr_surf", "(1,)")] = 1.0

        df_state_issue.loc[:, ("bldgh", "0")] = 17.464
        df_state_issue.loc[:, ("evetreeh", "0")] = 0.0
        df_state_issue.loc[:, ("dectreeh", "0")] = 0.0
        df_state_issue.loc[:, ("faibldg", "0")] = 0.011
        df_state_issue.loc[:, ("faievetree", "0")] = 0.0
        df_state_issue.loc[:, ("faidectree", "0")] = 0.0
        df_state_issue.loc[:, ("faimethod", "0")] = 0
        df_state_issue.loc[:, ("roughlenmommethod", "0")] = 2
        df_state_issue.loc[:, ("rslmethod", "0")] = 2
        df_state_issue.loc[:, ("stabilitymethod", "0")] = 3
        df_state_issue.loc[:, ("z", "0")] = 52.392
        df_state_issue.loc[:, ("z0m_in", "0")] = 0.03
        df_state_issue.loc[:, ("zdm_in", "0")] = 17.464

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            df_output, _ = sp.run_supy(
                df_forcing_short,
                df_state_issue,
                logging_level=logging.CRITICAL,
                check_input=False,
            )

        u10 = df_output[("SUEWS", "U10")]
        assert not u10.isna().all(), "Full-building auto MOST grid returned only NaN wind output"
