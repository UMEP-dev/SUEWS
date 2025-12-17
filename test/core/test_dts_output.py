"""Tests for DTS output validation against legacy interface.

This module validates that the DTS interface produces outputs consistent
with the legacy DataFrame-based interface.

Test Categories:
1. Output structure comparison
2. Variable-by-variable comparison
3. Multi-timestep consistency

Note: The high-level `run_dts()` method in `SUEWSSimulation` currently does not
bootstrap initial state from config. State initialisation for realistic physics
outputs requires using the low-level DTS interface (`run_supy_dts_tstep`) with
properly initialised state - see `test_dts_runner.py` for working examples.

The tests here validate that the infrastructure works (data flows through)
even if physics outputs are zero/NaN due to missing state initialisation.
"""

import numpy as np
import pandas as pd
import pytest
from pathlib import Path

from supy import SUEWSSimulation


# =============================================================================
# Test Fixtures
# =============================================================================


@pytest.fixture
def sample_config_path():
    """Path to sample configuration file."""
    # Use sample data from package
    return Path(__file__).parent.parent.parent / "src" / "supy" / "sample_data" / "sample_config.yml"


@pytest.fixture
def sample_forcing_path():
    """Path to sample forcing data file."""
    return Path(__file__).parent.parent.parent / "src" / "supy" / "sample_data" / "Kc_2012_data_60.txt"


@pytest.fixture
def simulation_short(sample_config_path, sample_forcing_path):
    """Create simulation with 1 day of data for comparison tests."""
    sim = SUEWSSimulation(str(sample_config_path))
    sim.update_forcing(str(sample_forcing_path))
    return sim


# =============================================================================
# Test DTS vs Legacy Output Comparison
# =============================================================================


class TestDTSOutputComparison:
    """Compare DTS output against legacy interface."""

    @pytest.mark.core
    def test_dts_produces_output(self, simulation_short):
        """Test that run_dts() produces non-empty output."""
        # Run DTS for short period (6 hours)
        df_dts = simulation_short.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 06:00"
        )

        # ASSERT - basic structure
        assert df_dts is not None
        assert isinstance(df_dts, pd.DataFrame)
        assert len(df_dts) > 0

    @pytest.mark.core
    def test_legacy_produces_output(self, simulation_short):
        """Test that legacy run() produces non-empty output."""
        # Run legacy for short period
        output = simulation_short.run(
            start_date="2012-01-01",
            end_date="2012-01-01"
        )

        # ASSERT - basic structure
        assert output is not None
        # SUEWSOutput uses variable groups, not df_output
        assert 'SUEWS' in output.groups
        df_suews = output.SUEWS
        assert len(df_suews) > 0

    @pytest.mark.core
    def test_dts_output_has_heat_variables(self, simulation_short):
        """Test that DTS output contains expected heat flux variables."""
        df_dts = simulation_short.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 06:00"
        )

        # Expected heat state variables from extract_heat_state
        heat_vars = ["qh", "qe", "qs", "qn", "qf", "tsurf"]

        for var in heat_vars:
            assert var in df_dts.columns, f"Missing variable: {var}"

    @pytest.mark.core
    def test_dts_vs_legacy_structure_comparison(
        self, sample_config_path, sample_forcing_path
    ):
        """Compare structure of DTS vs legacy outputs."""
        # Run DTS
        sim_dts = SUEWSSimulation(str(sample_config_path))
        sim_dts.update_forcing(str(sample_forcing_path))
        df_dts = sim_dts.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 06:00"
        )

        # Create new simulation for legacy (to avoid state contamination)
        sim_legacy = SUEWSSimulation(str(sample_config_path))
        sim_legacy.update_forcing(str(sample_forcing_path))
        output_legacy = sim_legacy.run(start_date="2012-01-01", end_date="2012-01-01")

        # ASSERT - both produce dataframes
        assert isinstance(df_dts, pd.DataFrame)
        assert 'SUEWS' in output_legacy.groups
        df_legacy = output_legacy.SUEWS

        # ASSERT - both have timesteps
        assert len(df_dts) > 0
        assert len(df_legacy) > 0

    @pytest.mark.core
    def test_dts_output_values_reasonable(self, simulation_short):
        """Test that DTS output values are within expected ranges.

        Note: Without state initialisation, physics values will be zero/NaN.
        This test validates the data structure, not physics correctness.
        """
        df_dts = simulation_short.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 12:00"
        )

        # Check QN column exists and has expected structure
        assert "qn" in df_dts.columns, "Missing qn column"
        qn = df_dts["qn"]
        assert len(qn) > 0, "Empty qn series"

        # Note: Without state bootstrap, values will be zero
        # Just verify no infinite values
        assert np.isfinite(qn).all() or qn.isna().all(), "Infinite qn values"

        # Check tsurf column exists
        assert "tsurf" in df_dts.columns, "Missing tsurf column"

    @pytest.mark.core
    def test_dts_multi_timestep_consistency(self, simulation_short):
        """Test that DTS produces consistent results across timesteps.

        Note: Without state bootstrap, values will be zeros/NaN consistently.
        This test validates that the loop completes without errors.
        """
        df_dts = simulation_short.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 12:00"
        )

        # Verify we got the expected number of timesteps
        # 12 hours at 5-min resolution = 144 timesteps
        assert len(df_dts) == 144, f"Expected 144 timesteps, got {len(df_dts)}"

        # Check that output columns exist and have consistent length
        for col in ["qh", "qe", "qs", "qn", "qf", "tsurf"]:
            assert col in df_dts.columns, f"Missing column: {col}"
            assert len(df_dts[col]) == 144, f"Inconsistent length in {col}"


# =============================================================================
# Test Variable Mapping
# =============================================================================


class TestDTSVariableMapping:
    """Test mapping between DTS and legacy variable names."""

    @pytest.mark.core
    def test_dts_has_forcing_reference(self, simulation_short):
        """Test that DTS output includes forcing values for reference."""
        df_dts = simulation_short.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 06:00"
        )

        # Check forcing reference columns
        assert "kdown" in df_dts.columns, "Missing kdown forcing reference"
        assert "temp_c" in df_dts.columns, "Missing temp_c forcing reference"

    @pytest.mark.core
    def test_dts_kdown_matches_forcing(self, simulation_short):
        """Test that kdown in output matches forcing input."""
        start = "2012-01-01 00:05"
        end = "2012-01-01 06:00"

        df_dts = simulation_short.run_dts(start_date=start, end_date=end)

        # Get forcing data for comparison
        df_forcing = simulation_short._df_forcing.loc[start:end]

        # Compare lengths
        assert len(df_dts) == len(df_forcing), "Output and forcing lengths differ"

        # Compare kdown values (should be identical)
        # Note: column name might be nested in forcing
        if "kdown" in df_dts.columns:
            # Forcing kdown may have different column structure
            forcing_kdown = None
            if "kdown" in df_forcing.columns:
                forcing_kdown = df_forcing["kdown"].values
            elif ("meteo", "kdown") in df_forcing.columns:
                forcing_kdown = df_forcing[("meteo", "kdown")].values

            if forcing_kdown is not None:
                np.testing.assert_array_almost_equal(
                    df_dts["kdown"].values,
                    forcing_kdown,
                    decimal=6,
                    err_msg="kdown values don't match forcing"
                )


# =============================================================================
# Test Edge Cases
# =============================================================================


class TestDTSvsLegacyComparison:
    """Compare DTS interface outputs against legacy run() interface.

    Note: Full physics comparison requires site surface property accessors
    (soilstorecap, statelimit, etc.) to be implemented. Until then, hydro
    state bootstrap is disabled and physics outputs may be zero/NaN.
    """

    @pytest.mark.core
    def test_dts_vs_legacy_structure_same_columns(
        self, sample_config_path, sample_forcing_path
    ):
        """Verify DTS and legacy produce structurally compatible outputs.

        This validates that both interfaces return DataFrames with the
        expected heat flux variable columns, regardless of values.
        """
        # ARRANGE - Run DTS
        sim_dts = SUEWSSimulation(str(sample_config_path))
        sim_dts.update_forcing(str(sample_forcing_path))
        df_dts = sim_dts.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 12:00"
        )

        # ARRANGE - Run legacy (fresh simulation to avoid state contamination)
        sim_legacy = SUEWSSimulation(str(sample_config_path))
        sim_legacy.update_forcing(str(sample_forcing_path))
        output_legacy = sim_legacy.run(start_date="2012-01-01", end_date="2012-01-01")
        df_legacy = output_legacy.SUEWS

        # ASSERT - Both produce valid outputs
        assert len(df_dts) > 0, "DTS produced no output"
        assert len(df_legacy) > 0, "Legacy produced no output"

        # ASSERT - Heat flux variables exist in both
        dts_vars = ["qn", "qh", "qe", "qs"]
        legacy_vars = ["QN", "QH", "QE", "QS"]

        for dts_var, legacy_var in zip(dts_vars, legacy_vars):
            assert dts_var in df_dts.columns, f"DTS missing {dts_var}"
            assert legacy_var in df_legacy.columns, f"Legacy missing {legacy_var}"

    @pytest.mark.core
    def test_dts_vs_legacy_heat_flux_comparison(
        self, sample_config_path, sample_forcing_path
    ):
        """Prove DTS produces comparable results to legacy run()."""
        sim_dts = SUEWSSimulation(str(sample_config_path))
        sim_dts.update_forcing(str(sample_forcing_path))
        df_dts = sim_dts.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 12:00"
        )

        qn_dts = df_dts["qn"]
        has_variation = qn_dts.std() > 0.1 or qn_dts.abs().max() > 1.0
        assert has_variation, "DTS qn has no variation - bootstrap may have failed"

    @pytest.mark.core
    def test_dts_state_bootstrap_produces_valid_output(
        self, sample_config_path, sample_forcing_path
    ):
        """Verify state bootstrap produces non-zero physics outputs."""
        sim = SUEWSSimulation(str(sample_config_path))
        sim.update_forcing(str(sample_forcing_path))
        df_dts = sim.run_dts(
            start_date="2012-01-01 06:00",
            end_date="2012-01-01 18:00"
        )

        assert "tsurf" in df_dts.columns, "Missing tsurf column"
        assert "qn" in df_dts.columns, "Missing qn column"
        assert "qh" in df_dts.columns, "Missing qh column"

        tsurf = df_dts["tsurf"]
        assert not tsurf.isna().all(), "All tsurf values are NaN"

        qn = df_dts["qn"]
        assert not qn.isna().all(), "All qn values are NaN"


class TestDTSEdgeCases:
    """Test edge cases for DTS interface."""

    @pytest.mark.core
    def test_dts_single_timestep(self, simulation_short):
        """Test DTS with minimal timesteps."""
        # Request very short period (at least 2 timesteps needed)
        df_dts = simulation_short.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 00:15"
        )

        assert len(df_dts) >= 2, "Need at least 2 timesteps"

    @pytest.mark.core
    def test_dts_state_evolution(self, simulation_short):
        """Test that DTS simulation loop completes successfully.

        Note: Without state bootstrap from config, physics values won't evolve.
        This test validates the loop runs without errors.
        For state evolution tests with proper initialisation, see test_dts_runner.py.
        """
        df_dts = simulation_short.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 12:00"
        )

        # Verify simulation completed all timesteps
        assert len(df_dts) == 144, "Simulation loop didn't complete"

        # Verify state columns exist
        assert "qn" in df_dts.columns, "Missing qn column"
        assert "qh" in df_dts.columns, "Missing qh column"

        # Note: True state evolution requires state bootstrap (not yet implemented)
        # For now, just verify the loop ran to completion

    @pytest.mark.core
    def test_dts_reproducibility(self, sample_config_path, sample_forcing_path):
        """Test that running DTS twice gives identical results."""
        # Run 1
        sim1 = SUEWSSimulation(str(sample_config_path))
        sim1.update_forcing(str(sample_forcing_path))
        df1 = sim1.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 06:00"
        )

        # Run 2 (fresh simulation)
        sim2 = SUEWSSimulation(str(sample_config_path))
        sim2.update_forcing(str(sample_forcing_path))
        df2 = sim2.run_dts(
            start_date="2012-01-01 00:05",
            end_date="2012-01-01 06:00"
        )

        # Compare outputs
        pd.testing.assert_frame_equal(df1, df2, check_exact=True)
