"""Test forcing data interpolation functionality."""

import numpy as np
import pandas as pd
import pytest
from supy._load import resample_forcing_met


class TestForcingInterpolation:
    """Test suite for forcing data interpolation issues."""

    def test_no_interpolation_when_tstep_equals_resolutionfilesin(self):
        """Test that forcing data is not interpolated when tstep = resolutionfilesin.
        
        This addresses issue #161: When the model timestep equals the input data
        resolution, no interpolation should occur. Previously, the data was
        incorrectly interpolated, affecting averaged variables like Kdown.
        """
        # Create test forcing data at 300s (5 min) intervals
        time_index = pd.date_range('2023-01-01 00:00:00', periods=24, freq='300s')
        
        # Create test data with distinct values to detect interpolation
        test_data = {
            # Instantaneous variables
            'Tair': np.arange(20.0, 22.4, 0.1),  # Temperature increasing by 0.1
            'RH': np.full(24, 70.0),  # Constant relative humidity
            'U': np.full(24, 5.0),  # Constant wind speed
            'pres': np.full(24, 1013.0),  # Constant pressure
            
            # Average variables (these should NOT be interpolated)
            'kdown': np.array([0, 0, 0, 0, 100, 200, 300, 400, 500, 600, 700, 800,
                             800, 700, 600, 500, 400, 300, 200, 100, 0, 0, 0, 0]),
            'ldown': np.full(24, 350.0),
            
            # Sum variables
            'rain': np.zeros(24),
            
            # Time columns
            'iy': time_index.year,
            'id': time_index.dayofyear,
            'it': time_index.hour,
            'imin': time_index.minute,
            'isec': time_index.second
        }
        
        # Add a single rain event
        test_data['rain'][12] = 5.0
        
        df_forcing = pd.DataFrame(test_data, index=time_index)
        
        # Resample with tstep_in = tstep_mod = 300 (no interpolation should occur)
        tstep_in = 300  # Input data resolution in seconds
        tstep_mod = 300  # Model timestep in seconds
        
        # Call the resample function
        df_resampled = resample_forcing_met(
            df_forcing, tstep_in, tstep_mod,
            lat=51.5, lon=0.0, alt=100, timezone=0, kdownzen=0
        )
        
        # Verify that average variables (kdown) are NOT interpolated
        # The values should remain exactly the same
        np.testing.assert_array_equal(
            df_forcing['kdown'].values,
            df_resampled['kdown'].values,
            err_msg="kdown values were interpolated when they should not have been"
        )
        
        # Verify instantaneous variables are also unchanged
        np.testing.assert_array_equal(
            df_forcing['Tair'].values,
            df_resampled['Tair'].values,
            err_msg="Tair values changed when no interpolation should occur"
        )
        
        # Verify sum variables (rain) are unchanged
        np.testing.assert_array_equal(
            df_forcing['rain'].values,
            df_resampled['rain'].values,
            err_msg="rain values changed when no interpolation should occur"
        )

    def test_interpolation_when_tstep_less_than_resolutionfilesin(self):
        """Test that forcing data IS interpolated when tstep < resolutionfilesin.
        
        This ensures the fix doesn't break the normal interpolation case.
        """
        # Create test forcing data at 600s (10 min) intervals
        time_index = pd.date_range('2023-01-01 00:00:00', periods=12, freq='600s')
        
        # Create test data
        test_data = {
            # Instantaneous variables
            'Tair': np.linspace(20.0, 22.0, 12),
            'RH': np.full(12, 70.0),
            'U': np.full(12, 5.0),
            'pres': np.full(12, 1013.0),
            
            # Average variables
            'kdown': np.array([0, 0, 100, 200, 400, 600, 600, 400, 200, 100, 0, 0]),
            'ldown': np.full(12, 350.0),
            
            # Sum variables
            'rain': np.zeros(12),
            
            # Time columns
            'iy': time_index.year,
            'id': time_index.dayofyear,
            'it': time_index.hour,
            'imin': time_index.minute,
            'isec': time_index.second
        }
        
        df_forcing = pd.DataFrame(test_data, index=time_index)
        
        # Resample with tstep_in=600, tstep_mod=300 (should interpolate)
        tstep_in = 600   # Input data resolution in seconds
        tstep_mod = 300  # Model timestep in seconds (finer than input)
        
        # Call the resample function
        df_resampled = resample_forcing_met(
            df_forcing, tstep_in, tstep_mod,
            lat=51.5, lon=0.0, alt=100, timezone=0, kdownzen=0
        )
        
        # Verify that resampled data has more points (interpolated)
        assert len(df_resampled) > len(df_forcing), \
            "Resampled data should have more points when tstep < resolutionfilesin"
        
        # Verify time resolution is correct
        time_diff = df_resampled.index[1] - df_resampled.index[0]
        assert time_diff.total_seconds() == tstep_mod, \
            f"Resampled time step should be {tstep_mod}s, got {time_diff.total_seconds()}s"

    def test_average_variable_interpolation_shift(self):
        """Test that average variables are correctly shifted during interpolation.
        
        This verifies the shift behavior mentioned in the issue for average variables.
        """
        # Create test forcing data at 600s intervals
        time_index = pd.date_range('2023-01-01 00:00:00', periods=6, freq='600s')
        
        # Create simple pattern for average variable
        test_data = {
            'kdown': np.array([0, 100, 200, 300, 200, 100]),
            'Tair': np.full(6, 20.0),  # Instantaneous variable
            'rain': np.zeros(6),  # Sum variable
            'iy': time_index.year,
            'id': time_index.dayofyear,
            'it': time_index.hour,
            'imin': time_index.minute,
            'isec': time_index.second
        }
        
        df_forcing = pd.DataFrame(test_data, index=time_index)
        
        # Test with interpolation
        tstep_in = 600
        tstep_mod = 300
        
        df_resampled = resample_forcing_met(
            df_forcing, tstep_in, tstep_mod,
            lat=51.5, lon=0.0, alt=100, timezone=0, kdownzen=0
        )
        
        # Check that interpolation creates intermediate values
        # Between 0 and 100, we should get ~50
        assert 40 < df_resampled['kdown'].iloc[1] < 60, \
            "Interpolation should create intermediate values for average variables"