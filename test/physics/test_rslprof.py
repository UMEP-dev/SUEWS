"""
Test suews_phys_rslprof - RSL/MOST wind profile calculations.

Regression tests for Issue #572 where the MIN constraint on height arrays
caused negative LOG arguments and negative wind speeds for tall buildings.
"""

import warnings

from conftest import load_sample_frames, run_simulation
import numpy as np
import pytest

pytestmark = pytest.mark.physics


class TestWindProfiles:
    """Test suite for wind profile calculations."""

    @pytest.fixture
    def sample_data(self):
        """Load sample data for testing."""
        df_state_init, df_forcing = load_sample_frames()
        return df_state_init, df_forcing

    def test_most_height_array_monotonic(self, sample_data):
        """Test that MOST generates monotonic height arrays for various building heights."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:2]

        for bldgh in [2.0, 10.0, 50.0]:
            df_state_init.loc[:, "bldgh"] = bldgh
            df_state_init.loc[:, "rslmethod"] = 0  # MOST diagnostic

            df_output, _ = run_simulation(df_forcing_short, df_state_init)

            z_cols = [col for col in df_output.columns if col[1].startswith("z_")]
            heights = df_output[z_cols].iloc[0].values
            heights = heights[~np.isnan(heights)]

            assert np.all(np.diff(heights) > 0), (
                f"Height array not monotonic for {bldgh}m building"
            )

    def test_most_and_rsl_methods_run(self, sample_data):
        """Test that both MOST and RSL methods run without errors."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:2]
        df_state_init.loc[:, "bldgh"] = 10.0

        # Test MOST
        df_state_init.loc[:, "rslmethod"] = 0
        df_output_most, _ = run_simulation(df_forcing_short, df_state_init)
        assert not df_output_most.empty, "MOST failed to run"

        # Test RSL
        df_state_init.loc[:, "rslmethod"] = 1
        df_output_rsl, _ = run_simulation(df_forcing_short, df_state_init)
        assert not df_output_rsl.empty, "RSL failed to run"

    def test_rsl_grid_includes_exact_canopy_top(self, sample_data):
        """Regression test for GH#296: RSL level 20 is the canopy top."""
        df_state_init, df_forcing = sample_data
        df_state = df_state_init.iloc[[0]].copy()

        df_state.loc[:, ("rslmethod", "0")] = 1
        df_state.loc[:, ("bldgh", "0")] = 40.0

        df_output, _ = run_simulation(df_forcing.iloc[:2], df_state)
        rsl = df_output.xs("RSL", axis=1, level=0).iloc[0]

        assert rsl["flag_RSL"] == 1
        assert rsl["z_20"] == pytest.approx(rsl["zH_RSL"], abs=1e-9)

    @pytest.mark.parametrize(
        "zh,z0m_in,zdm_in",
        [
            (2.0, 0.1, 1.4),  # Very low buildings (z0m < zdm)
            (5.0, 0.5, 3.5),  # Low-rise scenario (catches #572 profile cap)
            (10.0, 1.0, 7.0),  # Standard case
            (1.0, 0.01, 0.7),  # Extremely low roughness
            (2.0, 0.01, 1.4),  # Low roughness with 2 m canopy
        ],
    )
    def test_wind_speeds_positive(self, sample_data, zh, z0m_in, zdm_in):
        """Test that U10 and full RSL/MOST profiles stay finite, non-negative, and monotone."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:48]

        df_state_init.loc[:, "zh"] = zh
        df_state_init.loc[:, "z0m_in"] = z0m_in
        df_state_init.loc[:, "zdm_in"] = zdm_in
        df_state_init.loc[:, "rslmethod"] = 0
        df_state_init.loc[:, "stabilitymethod"] = 2

        df_output, _ = run_simulation(df_forcing_short, df_state_init)

        suews_data = df_output.xs("SUEWS", level=0, axis=1)
        u10 = suews_data["U10"].to_numpy(dtype=float)
        assert len(u10) == len(df_forcing_short)
        assert np.isfinite(u10).all(), (
            f"U10 non-finite for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"
        )
        assert (u10 >= 0).all(), f"U10 negative for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"

        # Verify the full 30-level diagnostic profile across all 48 timesteps
        # (catches #572 where U10 stayed positive while profile winds went negative/NaN).
        rsl_data = df_output.xs("RSL", level=0, axis=1)
        z_profile = rsl_data[[f"z_{level}" for level in range(1, 31)]].to_numpy(
            dtype=float
        )
        u_profile = rsl_data[[f"U_{level}" for level in range(1, 31)]].to_numpy(
            dtype=float
        )
        assert z_profile.shape == (len(df_forcing_short), 30)
        assert u_profile.shape == (len(df_forcing_short), 30)
        assert np.isfinite(z_profile).all(), (
            f"Profile heights non-finite for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"
        )
        assert (np.diff(z_profile, axis=1) > 0).all(), (
            f"Profile heights not strictly monotone for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"
        )
        assert np.isfinite(u_profile).all(), (
            f"Profile winds non-finite for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"
        )
        assert (u_profile >= 0).all(), (
            f"Profile winds negative for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"
        )

    @pytest.mark.parametrize(
        "zh,z0m_in,zdm_in",
        [
            (2.0, 0.1, 0.1),
            (1.0, 0.01, 0.01),
            (2.0, 0.01, 0.01),
        ],
    )
    def test_equal_roughness_and_displacement_refused(
        self, sample_data, zh, z0m_in, zdm_in
    ):
        """Equal roughness and displacement heights are rejected before execution."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:2]

        df_state_init.loc[:, "zh"] = zh
        df_state_init.loc[:, "z0m_in"] = z0m_in
        df_state_init.loc[:, "zdm_in"] = zdm_in
        df_state_init.loc[:, "rslmethod"] = 0
        df_state_init.loc[:, "stabilitymethod"] = 2

        with pytest.raises(ValueError, match=r"z0m_in .* must be less than zdm_in"):
            run_simulation(df_forcing_short, df_state_init)

    def test_tall_building_no_negative_winds(self, sample_data):
        """Test that tall buildings don't cause negative wind speeds."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:2]

        # Tall building configuration
        df_state_init.loc[:, "bldgh"] = 50.0
        df_state_init.loc[:, "z0m_in"] = 5.0
        df_state_init.loc[:, "zdm_in"] = 35.0
        df_state_init.loc[:, "z"] = 60.0  # Above building height
        df_state_init.loc[:, "rslmethod"] = 0

        df_output, _ = run_simulation(df_forcing_short, df_state_init)

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
            df_output, _ = run_simulation(
                df_forcing_short,
                df_state_issue,
            )

        u10 = df_output[("SUEWS", "U10")]
        assert not u10.isna().all(), (
            "Full-building auto MOST grid returned only NaN wind output"
        )
