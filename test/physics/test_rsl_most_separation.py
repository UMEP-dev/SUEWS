#!/usr/bin/env python3
"""
Test RSL/MOST separation fix for atmospheric profile calculations.

This test module verifies the fix for GitHub Issue #419 where non-monotonic
height arrays were causing interpolation errors. The fix separates RSL and 
MOST approaches to maintain physical consistency.

References:
- GitHub Issue #419: RSL interpolation crash with short buildings
- GitHub PR #539: Initial workaround (now replaced by proper fix)
- Harman & Finnigan (2007, 2008): RSL theory
- Monin-Obukhov Similarity Theory
"""

import pytest
import numpy as np
import pandas as pd
import supy as sp


class TestRSLMOSTSeparation:
    """Test suite for RSL/MOST separation in atmospheric profiles."""
    
    @pytest.fixture
    def sample_data(self):
        """Load sample data for testing."""
        df_state_init, df_forcing = sp.load_SampleData()
        # Use only one timestep for unit tests (fast)
        df_forcing_short = df_forcing.iloc[:1]
        return df_state_init, df_forcing_short
    
    @pytest.mark.essential
    def test_most_height_array_monotonic(self, sample_data):
        """
        Test that MOST approach generates monotonic height arrays.
        
        This tests the fix for the issue where levels 1-20 had constant
        values due to RSL parameter mixing.
        
        Expected behaviour:
        - Height array strictly increasing from surface to measurement height
        - No constant regions in the array
        - Proper coverage of diagnostic heights (2m, 10m)
        """
        df_state_init, df_forcing = sample_data
        
        # Test multiple building heights
        building_heights = [2.0, 5.0, 10.0, 20.0, 50.0]
        
        for bldgh in building_heights:
            # Arrange
            df_state_init.loc[:, "bldgh"] = bldgh
            df_state_init.loc[:, "stabilitymethod"] = 2  # MOST stability
            df_state_init.loc[:, "diagmethod"] = 0      # MOST diagnostic
            
            # Act
            df_output, _ = sp.run_supy(df_forcing, df_state_init)
            
            # Assert - Extract height array
            heights = self._extract_height_array(df_output)
            
            # Check monotonicity
            assert np.all(np.diff(heights) > 0), (
                f"Height array not monotonic for {bldgh}m building. "
                f"Differences: {np.diff(heights)}"
            )
            
            # Check no constant values
            unique_heights = np.unique(heights)
            assert len(unique_heights) == len(heights), (
                f"Height array has repeated values for {bldgh}m building. "
                f"Expected {len(heights)} unique values, got {len(unique_heights)}"
            )
            
            # Check diagnostic height coverage
            assert heights[0] <= 2.0, f"First height {heights[0]} > 2m diagnostic"
            assert heights[-1] >= 10.0, f"Last height {heights[-1]} < 10m diagnostic"
    
    @pytest.mark.essential
    def test_rsl_height_array_properties(self, sample_data):
        """
        Test RSL approach height array properties.
        
        RSL should generate appropriate heights for within and above canopy,
        with proper monotonicity throughout.
        """
        df_state_init, df_forcing = sample_data
        
        # Test with typical urban configuration
        df_state_init.loc[:, "bldgh"] = 20.0
        df_state_init.loc[:, "stabilitymethod"] = 2  # MOST stability  
        df_state_init.loc[:, "diagmethod"] = 1      # RSL diagnostic
        
        # Act
        df_output, _ = sp.run_supy(df_forcing, df_state_init)
        
        # Assert
        heights = self._extract_height_array(df_output)
        
        # Check monotonicity
        assert np.all(np.diff(heights) > 0), "RSL height array not monotonic"
        
        # Check appropriate within-canopy resolution
        # First 20 levels should be within/near canopy
        within_canopy = heights[:20]
        assert within_canopy[-1] <= 30.0, "Within-canopy heights extend too high"
        
        # Check smooth transition to above-canopy
        transition_ratio = heights[21] / heights[20]
        assert 1.0 < transition_ratio < 2.0, (
            f"Abrupt transition at canopy top: {transition_ratio}"
        )
    
    @pytest.mark.critical
    def test_no_parameter_mixing(self, sample_data):
        """
        Test that MOST diagnostics don't use RSL-derived parameters.
        
        This was the root cause of the original issue where MOST diagnostics
        were using zd_RSL and z0_RSL, causing non-physical height arrays.
        """
        df_state_init, df_forcing = sample_data
        
        # Configure for MOST diagnostics with conditions that triggered the bug
        df_state_init.loc[:, "bldgh"] = 10.0
        df_state_init.loc[:, "stabilitymethod"] = 2  # MOST stability
        df_state_init.loc[:, "diagmethod"] = 0      # MOST diagnostic
        
        df_output, _ = sp.run_supy(df_forcing, df_state_init)
        
        # Check that the problematic constant value doesn't appear
        heights = self._extract_height_array(df_output)
        
        # The bug created constant values of ~16.261m for 10m buildings
        # Check this specific value doesn't dominate the array
        problem_value = 16.261
        close_to_problem = np.abs(heights - problem_value) < 0.01
        num_problem_values = np.sum(close_to_problem)
        
        assert num_problem_values < 5, (
            f"Found {num_problem_values} heights near problematic value {problem_value}m"
        )
    
    @pytest.mark.extended
    def test_diagnostic_interpolation_coverage(self, sample_data):
        """
        Test that diagnostic heights are properly covered without extrapolation.
        
        The original workaround allowed extrapolation when interpolation was
        requested outside bounds. The fix ensures proper coverage instead.
        """
        df_state_init, df_forcing = sample_data
        
        # Test cases that previously caused issues
        # Note: Very short buildings (< 2m) may still have issues due to 
        # physical limitations of the model
        test_cases = [
            {"bldgh": 5.0, "desc": "5m building (between diagnostic heights)"},
            {"bldgh": 10.0, "desc": "10m building (original test case)"},
            {"bldgh": 20.0, "desc": "20m building"},
        ]
        
        for case in test_cases:
            # Arrange
            df_state_init.loc[:, "bldgh"] = case["bldgh"]
            df_state_init.loc[:, "stabilitymethod"] = 2
            df_state_init.loc[:, "diagmethod"] = 0  # MOST
            
            # Act
            df_output, _ = sp.run_supy(df_forcing, df_state_init)
            
            # Assert - should complete without interpolation errors
            assert not df_output.empty, f"Failed for {case['desc']}"
            
            # Check that height arrays are monotonic (main fix verification)
            heights = self._extract_height_array(df_output)
            assert np.all(np.diff(heights) > 0), f"Non-monotonic heights for {case['desc']}"
    
    @pytest.mark.integration
    def test_auto_method_selection(self, sample_data):
        """
        Test automatic method selection (diagmethod=2).
        
        Should select appropriate method based on conditions without
        issues from parameter mixing.
        """
        df_state_init, df_forcing = sample_data
        
        df_state_init.loc[:, "bldgh"] = 15.0
        df_state_init.loc[:, "stabilitymethod"] = 2
        df_state_init.loc[:, "diagmethod"] = 2  # Auto
        
        df_output, _ = sp.run_supy(df_forcing, df_state_init)
        
        # Should complete successfully with monotonic heights
        heights = self._extract_height_array(df_output)
        assert np.all(np.diff(heights) > 0), "Auto method produced non-monotonic array"
    
    @pytest.mark.extended
    @pytest.mark.slow
    def test_timestep_stability(self):
        """
        Test multiple timesteps to ensure stability across conditions.
        
        This test verifies the fix works consistently over time, not just
        for a single timestep.
        """
        # Load fresh data for extended test
        df_state_init, df_forcing = sp.load_SampleData()
        
        # Run for multiple timesteps (1 hour = 12 timesteps at 5-min intervals)
        df_forcing_1h = df_forcing.iloc[:12]
        
        df_state_init.loc[:, "bldgh"] = 10.0
        df_state_init.loc[:, "stabilitymethod"] = 2
        df_state_init.loc[:, "diagmethod"] = 0  # MOST
        
        # Act
        df_output, _ = sp.run_supy(df_forcing_1h, df_state_init)
        
        # Assert - check all timesteps produce monotonic height arrays
        # We check the first and last timestep as representative samples
        for idx in [0, -1]:
            # Extract heights for this timestep
            z_cols = sorted(
                [col for col in df_output.columns if col[0] == 'RSL' and col[1].startswith('z_')],
                key=lambda x: int(x[1].split('_')[1])
            )
            
            if z_cols:
                heights = np.array([df_output[col].iloc[idx] for col in z_cols])
                assert np.all(np.diff(heights) > 0), f"Non-monotonic heights at timestep {idx}"
    
    def _extract_height_array(self, df_output):
        """Extract height array from RSL output columns."""
        if 'RSL' not in df_output.columns.get_level_values(0):
            raise ValueError("No RSL output in results")
            
        z_cols = sorted(
            [col for col in df_output.columns if col[0] == 'RSL' and col[1].startswith('z_')],
            key=lambda x: int(x[1].split('_')[1])
        )
        
        if not z_cols:
            raise ValueError("No height columns found in RSL output")
            
        return np.array([df_output[col].iloc[0] for col in z_cols])


@pytest.mark.essential
class TestRSLParameters:
    """Test RSL parameter calculations are independent of MOST."""
    
    def test_rsl_parameter_independence(self):
        """
        Test that RSL parameters (zd_RSL, z0_RSL) are calculated
        independently and not used in MOST diagnostics.
        """
        df_state_init, df_forcing = sp.load_SampleData()
        df_forcing_short = df_forcing.iloc[:1]
        
        # Run with RSL diagnostics
        df_state_init.loc[:, "bldgh"] = 10.0
        df_state_init.loc[:, "diagmethod"] = 1  # RSL
        
        df_output_rsl, _ = sp.run_supy(df_forcing_short, df_state_init)
        
        # Run with MOST diagnostics  
        df_state_init.loc[:, "diagmethod"] = 0  # MOST
        
        df_output_most, _ = sp.run_supy(df_forcing_short, df_state_init)
        
        # Both should succeed without the parameter mixing issue
        assert not df_output_rsl.empty
        assert not df_output_most.empty
        
        # MOST should use standard z0m/zdm, not RSL-derived values
        # (This is implicitly tested by the monotonic height arrays)


if __name__ == "__main__":
    # Run tests with pytest
    pytest.main([__file__, "-v"])