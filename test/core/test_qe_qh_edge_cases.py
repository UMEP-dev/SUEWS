"""
Quick edge case tests for QE/QH stability.

This is a consolidated, fast-running test suite that ensures the QE/QH 
exact equality bug doesn't regress. Extracted from the original 
test_floating_point_stability.py for essential coverage only.

Runtime: <1 second
Purpose: Catch regression of the exact equality bug fixed in Issue #504
"""

import numpy as np
import pytest

import supy as sp


class TestQEQHEdgeCases:
    """Fast edge case tests for QE/QH stability."""

    @pytest.fixture
    def sample_data(self):
        """Load sample data for testing."""
        return sp.load_SampleData()

    def test_low_wind_and_zero_boundaries(self, sample_data):
        """
        Test conditions that trigger the problematic IF (H == 0.) check.
        
        This is the most critical test - directly targets the exact equality 
        issue in suews_phys_atmmoiststab.f95 lines 243 and 288.
        """
        df_state_init, df_forcing = sample_data
        
        # Test multiple edge cases in one run for efficiency
        test_scenarios = [
            {"U": 0.01, "name": "very_low_wind"},   # Very low wind
            {"U": 0.001, "name": "extreme_low_wind"}, # Extremely low wind
            {"U": 0.1, "name": "low_wind"},         # Low wind
        ]
        
        for scenario in test_scenarios:
            # Use only 6 timesteps for speed
            df_test = df_forcing.iloc[:6].copy()
            df_test["U"] = scenario["U"]
            df_test["Temp_C"] = 15.0  # Neutral temperature
            df_test["RH"] = 60.0      # Moderate humidity
            
            # Run simulation
            result, _ = sp.run_supy(df_test, df_state_init)
            
            # Verify valid results (no NaN or infinite values)
            assert not result.empty, f"Failed for {scenario['name']}"
            assert not result.SUEWS["QE"].isna().any(), f"QE has NaN for {scenario['name']}"
            assert not result.SUEWS["QH"].isna().any(), f"QH has NaN for {scenario['name']}"
            assert np.all(np.isfinite(result.SUEWS["QE"])), f"QE non-finite for {scenario['name']}"
            assert np.all(np.isfinite(result.SUEWS["QH"])), f"QH non-finite for {scenario['name']}"

    def test_repeated_runs_deterministic(self, sample_data):
        """
        Verify that repeated runs produce identical results.
        
        This ensures no random state pollution or non-deterministic behavior.
        Quick version - only 2 runs with 6 timesteps each.
        """
        df_state_init, df_forcing = sample_data
        
        # Small subset for speed
        df_test = df_forcing.iloc[:6].copy()
        df_test["U"] = 0.01  # Low wind to stress the system
        
        # Run twice
        result1, _ = sp.run_supy(df_test, df_state_init)
        result2, _ = sp.run_supy(df_test, df_state_init)
        
        # Results must be identical
        np.testing.assert_array_equal(
            result1.SUEWS["QE"].values,
            result2.SUEWS["QE"].values,
            err_msg="QE values differ between identical runs"
        )
        np.testing.assert_array_equal(
            result1.SUEWS["QH"].values,
            result2.SUEWS["QH"].values,
            err_msg="QH values differ between identical runs"
        )

    def test_atmospheric_stability_continuity(self, sample_data):
        """
        Test smooth transitions through stability regimes.
        
        Ensures the epsilon-based fix maintains physical continuity.
        Quick version with just 12 timesteps.
        """
        df_state_init, df_forcing = sample_data
        
        # Create smooth transition
        df_test = df_forcing.iloc[:12].copy()
        df_test["Temp_C"] = np.linspace(10, 25, 12)  # Gradual warming
        df_test["U"] = np.linspace(0.5, 2.0, 12)     # Increasing wind
        
        # Run simulation
        result, _ = sp.run_supy(df_test, df_state_init)
        
        # Check for reasonable gradients (no discontinuities)
        qe_values = result.SUEWS["QE"].values
        qh_values = result.SUEWS["QH"].values
        
        if len(qe_values) > 1:
            qe_gradient = np.diff(qe_values)
            qh_gradient = np.diff(qh_values)
            
            # Gradients should be reasonable (no sudden jumps > 100 W/m²)
            max_qe_jump = np.max(np.abs(qe_gradient))
            max_qh_jump = np.max(np.abs(qh_gradient))
            
            assert max_qe_jump < 100, f"QE has sudden jump of {max_qe_jump:.1f} W/m²"
            assert max_qh_jump < 100, f"QH has sudden jump of {max_qh_jump:.1f} W/m²"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])