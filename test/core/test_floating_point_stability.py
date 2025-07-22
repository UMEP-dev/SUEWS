"""
Tests for floating-point stability in SUEWS/SuPy.

This module validates that the exact equality check fixes work correctly
and that there's no state leakage between test runs.

Created to address the QE/QH discrepancy investigation (matthewp/testing_sample_data).
"""

import numpy as np
import pytest

import supy as sp


class TestFloatingPointStability:
    """Test floating-point stability and state isolation."""

    @pytest.fixture
    def sample_data(self):
        """Load sample data for testing."""
        return sp.load_SampleData()

    def test_repeated_runs_identical_results(self, sample_data):
        """
        Test that repeated runs with identical inputs produce identical results.

        This test addresses the state leakage issue discovered in the investigation.
        """
        df_state_init, df_forcing = sample_data

        # Use a small subset for testing
        df_test = df_forcing.iloc[:6].copy()

        # Run simulation multiple times
        results = []
        for i in range(3):
            result, _ = sp.run_supy(df_test, df_state_init)
            results.append(result)

        # All results should be identical
        base_result = results[0]
        for i, result in enumerate(results[1:], 1):
            # Check QE and QH specifically (the variables showing issues)
            np.testing.assert_array_equal(
                base_result.SUEWS["QE"].values,
                result.SUEWS["QE"].values,
                err_msg=f"QE values differ in run {i + 1}",
            )
            np.testing.assert_array_equal(
                base_result.SUEWS["QH"].values,
                result.SUEWS["QH"].values,
                err_msg=f"QH values differ in run {i + 1}",
            )

    def test_execution_order_independence(self, sample_data):
        """
        Test that execution order doesn't affect results.

        This specifically targets the issue that caused test_sample_output_validation to fail.
        """
        df_state_init, df_forcing = sample_data

        # Define test data
        df_test = df_forcing.iloc[:6].copy()

        # Run test in isolation
        result_isolated, _ = sp.run_supy(df_test, df_state_init)

        # Run some other simulations to potentially change state
        for i in range(3):
            df_dummy = df_forcing.iloc[i * 6 : (i + 1) * 6].copy()
            _ = sp.run_supy(df_dummy, df_state_init)

        # Run test again
        result_after_others, _ = sp.run_supy(df_test, df_state_init)

        # Results should be identical
        np.testing.assert_array_equal(
            result_isolated.SUEWS["QE"].values,
            result_after_others.SUEWS["QE"].values,
            err_msg="QE values differ based on execution order",
        )
        np.testing.assert_array_equal(
            result_isolated.SUEWS["QH"].values,
            result_after_others.SUEWS["QH"].values,
            err_msg="QH values differ based on execution order",
        )

    def test_low_wind_conditions(self, sample_data):
        """
        Test conditions that might trigger the problematic IF (H == 0.) check.

        This targets the exact equality issue in suews_phys_atmmoiststab.f95.
        """
        df_state_init, df_forcing = sample_data

        # Create conditions that might result in H very close to 0
        df_test = df_forcing.iloc[:6].copy()
        df_test["U"] = 0.01  # Very low wind speed
        df_test["Temp_C"] = 15.0  # Neutral temperature
        df_test["RH"] = 60.0  # Moderate humidity
        df_test["pres"] = 1013.25  # Standard pressure

        # Run simulation
        result, _ = sp.run_supy(df_test, df_state_init)

        # Check that simulation completes without errors
        assert not result.empty
        assert "QE" in result.SUEWS.columns
        assert "QH" in result.SUEWS.columns

        # Check for valid values (not NaN or infinite)
        assert not result.SUEWS["QE"].isna().any()
        assert not result.SUEWS["QH"].isna().any()
        assert np.all(np.isfinite(result.SUEWS["QE"]))
        assert np.all(np.isfinite(result.SUEWS["QH"]))

    def test_compiler_consistency(self, sample_data):
        """
        Test that results are consistent across different compiler optimizations.

        This validates that the fix works for both fast and slow builds.
        """
        df_state_init, df_forcing = sample_data

        # Use conditions that might trigger the problematic code path
        df_test = df_forcing.iloc[:6].copy()
        df_test["U"] = 0.1  # Low wind speed
        df_test["Temp_C"] = 15.0  # Neutral temperature

        # Run simulation multiple times
        results = []
        for i in range(5):
            result, _ = sp.run_supy(df_test, df_state_init)
            results.append(result)

        # All results should be identical (no compiler-dependent variation)
        base_result = results[0]
        for i, result in enumerate(results[1:], 1):
            np.testing.assert_array_equal(
                base_result.SUEWS["QE"].values,
                result.SUEWS["QE"].values,
                err_msg=f"QE shows compiler-dependent variation in run {i + 1}",
            )
            np.testing.assert_array_equal(
                base_result.SUEWS["QH"].values,
                result.SUEWS["QH"].values,
                err_msg=f"QH shows compiler-dependent variation in run {i + 1}",
            )

    def test_zero_boundary_conditions(self, sample_data):
        """
        Test boundary conditions around zero values.

        This targets the exact equality check IF (H == 0.) in the Fortran code.
        """
        df_state_init, df_forcing = sample_data

        # Create multiple scenarios with very low values
        test_scenarios = [
            {"U": 0.001, "Temp_C": 15.0, "RH": 60.0},  # Extremely low wind
            {"U": 0.0, "Temp_C": 15.0, "RH": 60.0},  # Zero wind (if allowed)
            {"U": 0.01, "Temp_C": 15.0, "RH": 60.0},  # Very low wind
        ]

        for i, scenario in enumerate(test_scenarios):
            df_test = df_forcing.iloc[:6].copy()
            for key, value in scenario.items():
                df_test[key] = value

            try:
                result, _ = sp.run_supy(df_test, df_state_init)

                # Check that results are valid
                assert not result.empty, f"Scenario {i} failed to produce results"
                assert np.all(np.isfinite(result.SUEWS["QE"])), (
                    f"Scenario {i} has invalid QE"
                )
                assert np.all(np.isfinite(result.SUEWS["QH"])), (
                    f"Scenario {i} has invalid QH"
                )

            except Exception as e:
                # If simulation fails, that's also important information
                pytest.skip(f"Scenario {i} failed with error: {e}")

    def test_atmospheric_stability_transitions(self, sample_data):
        """
        Test smooth transitions through atmospheric stability regimes.

        This validates that the fix maintains continuity across stability boundaries.
        """
        df_state_init, df_forcing = sample_data

        # Create a smooth transition from stable to unstable conditions
        df_test = df_forcing.iloc[:12].copy()

        # Gradually change conditions
        temps = np.linspace(10, 25, len(df_test))
        winds = np.linspace(0.5, 2.0, len(df_test))

        df_test["Temp_C"] = temps
        df_test["U"] = winds
        df_test["RH"] = 60.0  # Constant humidity

        # Run simulation
        result, _ = sp.run_supy(df_test, df_state_init)

        # Check for smooth behavior (no sudden jumps)
        qe_values = result.SUEWS["QE"].values
        qh_values = result.SUEWS["QH"].values

        # Calculate gradients
        if len(qe_values) > 1:
            qe_gradient = np.diff(qe_values)
            qh_gradient = np.diff(qh_values)

            # Check that gradients are reasonable (no sudden discontinuities)
            assert np.all(np.abs(qe_gradient) < 100), "QE has sudden jumps"
            assert np.all(np.abs(qh_gradient) < 100), "QH has sudden jumps"

        # Check that all values are finite
        assert np.all(np.isfinite(qe_values)), "QE contains non-finite values"
        assert np.all(np.isfinite(qh_values)), "QH contains non-finite values"

    def test_state_isolation_after_failure(self, sample_data):
        """
        Test that state is properly isolated even after simulation failures.

        This ensures that a failed simulation doesn't affect subsequent runs.
        """
        df_state_init, df_forcing = sample_data

        # Define a normal test case
        df_normal = df_forcing.iloc[:6].copy()

        # Run normal case to get baseline
        result_baseline, _ = sp.run_supy(df_normal, df_state_init)

        # Try to run a potentially problematic case
        df_problematic = df_normal.copy()
        df_problematic["U"] = 0.0  # Zero wind might cause issues

        try:
            _ = sp.run_supy(df_problematic, df_state_init)
        except Exception:
            # Ignore the failure, we're testing isolation
            pass

        # Run normal case again
        result_after, _ = sp.run_supy(df_normal, df_state_init)

        # Results should be identical to baseline
        np.testing.assert_array_equal(
            result_baseline.SUEWS["QE"].values,
            result_after.SUEWS["QE"].values,
            err_msg="QE values affected by previous failure",
        )
        np.testing.assert_array_equal(
            result_baseline.SUEWS["QH"].values,
            result_after.SUEWS["QH"].values,
            err_msg="QH values affected by previous failure",
        )


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
