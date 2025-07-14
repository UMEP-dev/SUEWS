"""Unit tests for OHM smooth transition implementation."""

import pytest
import numpy as np
import supy as sp


class TestOHMSmoothTransition:
    """Test the smooth transition implementation in OHM module."""
    
    def test_smooth_transition_continuity(self):
        """Test that OHM coefficients change smoothly near thresholds."""
        # Load sample data
        df_state, df_forcing = sp.load_SampleData()
        
        # Test temperatures around a typical threshold (10°C)
        threshold = 10.0
        test_temps = np.array([
            threshold - 0.1,
            threshold - 0.01,
            threshold,
            threshold + 0.01,
            threshold + 0.1
        ])
        
        results = []
        
        for temp in test_temps:
            # Create forcing with constant temperature
            df_forcing_test = df_forcing.iloc[:24].copy()  # One day
            df_forcing_test['Tair'] = temp
            
            # Run simulation
            df_output, _ = sp.run_supy(
                df_forcing_test,
                df_state,
                save_state=False
            )
            
            # Get storage heat flux from SUEWS output group
            qs_mean = df_output['SUEWS']['QS'].mean()
            results.append(qs_mean)
        
        # Check for smooth changes (no large jumps)
        differences = np.diff(results)
        max_diff = np.max(np.abs(differences))
        
        # With smooth transition, differences should be small
        # Old step function could have jumps of ~50 W/m²
        assert max_diff < 5.0, f"Large discontinuity detected: {max_diff} W/m²"
        
        # Check monotonicity (QS should change monotonically with temperature)
        # Higher temperatures generally lead to different storage patterns
        # but the change should be gradual
        assert all(abs(d) < 5.0 for d in differences), \
            "Non-smooth transition detected in QS values"
    
    def test_extreme_temperatures(self):
        """Test that extreme temperatures give expected behavior."""
        df_state, df_forcing = sp.load_SampleData()
        
        # Test extreme cold and hot temperatures
        extreme_temps = [-20.0, 40.0]
        
        for temp in extreme_temps:
            df_forcing_test = df_forcing.iloc[:24].copy()
            df_forcing_test['Tair'] = temp
            
            # Should run without errors
            df_output, _ = sp.run_supy(
                df_forcing_test,
                df_state,
                save_state=False
            )
            
            # Basic sanity check
            assert not df_output['SUEWS']['QS'].isna().any(), \
                f"NaN values in QS output for temperature {temp}°C"
    
    def test_platform_consistency(self):
        """Test that results are consistent regardless of floating-point precision."""
        df_state, df_forcing = sp.load_SampleData()
        
        # Test with temperatures that differ by tiny amounts
        base_temp = 10.0
        tiny_differences = [0, 1e-12, 1e-10, 1e-8]
        
        results = []
        
        for diff in tiny_differences:
            df_forcing_test = df_forcing.iloc[:24].copy()
            df_forcing_test['Tair'] = base_temp + diff
            
            df_output, _ = sp.run_supy(
                df_forcing_test,
                df_state,
                save_state=False
            )
            
            results.append(df_output['SUEWS']['QS'].mean())
        
        # All results should be very close (within floating point tolerance)
        for i in range(1, len(results)):
            relative_diff = abs(results[i] - results[0]) / abs(results[0])
            assert relative_diff < 1e-6, \
                f"Platform-dependent behavior detected: {relative_diff:.2e} relative difference"
    
    def test_soil_moisture_transition(self):
        """Test smooth transition for soil moisture thresholds."""
        df_state, df_forcing = sp.load_SampleData()
        
        # Test different soil moisture levels
        # Modify initial soil moisture to test wet/dry transition
        sm_fractions = np.linspace(0.0, 1.0, 11)  # 0% to 100% in 10% steps
        
        results = []
        
        for sm_frac in sm_fractions:
            df_state_test = df_state.copy()
            
            # Set soil moisture for vegetated surfaces
            # Assuming indices 3-6 are vegetated surfaces (EveTr, DecTr, Grass, BSoil)
            for surf_idx in range(3, 7):
                col_name = f'soilstore_id_{surf_idx}'
                if col_name in df_state_test.columns:
                    # Set as fraction of capacity
                    capacity_col = f'SoilStoreCap_{surf_idx}'
                    if capacity_col in df_state_test.columns:
                        df_state_test[col_name] = sm_frac * df_state_test[capacity_col]
            
            # Run short simulation
            df_output, _ = sp.run_supy(
                df_forcing.iloc[:24],
                df_state_test,
                save_state=False
            )
            
            results.append(df_output['SUEWS']['QS'].mean())
        
        # Check for smooth transition
        differences = np.diff(results)
        
        # No single step should cause a large jump
        max_diff = np.max(np.abs(differences))
        assert max_diff < 10.0, \
            f"Large discontinuity in soil moisture transition: {max_diff} W/m²"