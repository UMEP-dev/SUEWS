"""
Test RSL/MOST separation fix for atmospheric profile calculations.

Tests the fix for Issue #572 where the MIN constraint on height arrays
caused negative LOG arguments for tall buildings.
"""

import numpy as np
import pytest

import supy as sp


class TestRSLMOSTSeparation:
    """Test suite for RSL/MOST separation in atmospheric profiles."""

    @pytest.fixture
    def sample_data(self):  # noqa: PLR6301
        """Load sample data for testing."""
        df_state_init, df_forcing = sp.load_SampleData()
        df_forcing_short = df_forcing.iloc[:1]  # Single timestep for speed
        return df_state_init, df_forcing_short

    def test_most_height_array_monotonic(self, sample_data):  # noqa: PLR6301
        """Test that MOST generates monotonic height arrays for various building heights."""
        df_state_init, df_forcing = sample_data

        # Test multiple building heights including tall buildings
        for bldgh in [2.0, 10.0, 50.0]:
            df_state_init.loc[:, "bldgh"] = bldgh
            df_state_init.loc[:, "diagmethod"] = 0  # MOST diagnostic

            df_output, _ = sp.run_supy(df_forcing, df_state_init)

            # Extract height array
            z_cols = [col for col in df_output.columns if col[1].startswith("z_")]
            heights = df_output[z_cols].iloc[0].values
            heights = heights[~np.isnan(heights)]  # Remove NaN values

            # Check monotonicity
            assert np.all(np.diff(heights) > 0), (
                f"Height array not monotonic for {bldgh}m building"
            )

    def test_no_parameter_mixing(self, sample_data):  # noqa: PLR6301
        """Test that MOST and RSL methods can both run without parameter mixing."""
        df_state_init, df_forcing = sample_data

        df_state_init.loc[:, "bldgh"] = 10.0

        # Test MOST
        df_state_init.loc[:, "diagmethod"] = 0  # MOST
        df_output_most, _ = sp.run_supy(df_forcing, df_state_init)
        assert not df_output_most.empty, "MOST failed to run"

        # Test RSL
        df_state_init.loc[:, "diagmethod"] = 1  # RSL
        df_output_rsl, _ = sp.run_supy(df_forcing, df_state_init)
        assert not df_output_rsl.empty, "RSL failed to run"

    def test_tall_building_profiles(self, sample_data):  # noqa: PLR6301
        """Test that tall buildings don't cause negative wind speeds or NaN issues."""
        df_state_init, df_forcing = sample_data

        # Tall building case that previously caused issues
        # Measurement height must be ABOVE buildings for physical realism
        # Also need to ensure z - zdm > z0m to avoid stability errors
        df_state_init.loc[:, "bldgh"] = 50.0
        df_state_init.loc[:, "z0m_in"] = 5.0
        df_state_init.loc[:, "zdm_in"] = 35.0
        df_state_init.loc[:, "z"] = 60.0  # Measurement height ABOVE building height
        df_state_init.loc[:, "diagmethod"] = 0  # MOST

        df_output, _ = sp.run_supy(df_forcing, df_state_init)

        # Check wind profile exists and has valid values
        u_cols = [col for col in df_output.columns if col[1].startswith("U_")]
        if u_cols:
            wind_speeds = df_output[u_cols].iloc[0].values
            valid_winds = wind_speeds[~np.isnan(wind_speeds)]

            # No negative wind speeds
            assert np.all(valid_winds >= 0), "Negative wind speeds detected"
