"""
Test atmospheric calculation utilities.

This module tests the atmospheric calculation functions in supy.util._atm,
including humidity calculations, latent heat, and thermodynamic properties.
"""

from unittest import TestCase
import warnings

import numpy as np
import pandas as pd
import pytest

from supy.util._atm import (
    cal_cp,
    cal_des_dta,
    cal_dq,
    cal_lat_vap,
    cal_qa,
    cal_rh,
    correct_wind_height,
)


class TestAtmosphericCalculations(TestCase):
    """Test atmospheric calculation functions."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)
        # Standard atmospheric conditions for testing
        self.ta_c = 20.0  # Temperature in Celsius
        self.ta_k = self.ta_c + 273.15  # Temperature in Kelvin
        self.rh_pct = 60.0  # Relative humidity in %
        self.pres_hpa = 1013.25  # Pressure in hPa
        self.pres_pa = self.pres_hpa * 100  # Pressure in Pa

    def test_cal_des_dta_scalar(self):
        """Test saturation vapour pressure slope calculation with scalar inputs."""
        print("\n========================================")
        print("Testing cal_des_dta with scalar inputs...")

        des_dta = cal_des_dta(self.ta_k, self.pres_pa)

        # Validate output
        self.assertIsInstance(des_dta, (float, np.floating))
        self.assertGreater(des_dta, 0)  # Should be positive at normal temperatures
        self.assertLess(des_dta, 500)  # Reasonable upper bound

        print(f"✓ des/dTa at {self.ta_c}°C: {des_dta:.2f} Pa/K")

    def test_cal_des_dta_series(self):
        """Test saturation vapour pressure slope calculation with Series inputs."""
        print("\n========================================")
        print("Testing cal_des_dta with Series inputs...")

        # Create temperature series
        temps_c = np.array([0, 10, 20, 30, 40])
        temps_k = temps_c + 273.15
        idx = pd.date_range("2023-01-01", periods=len(temps_c), freq="h")
        ta_series = pd.Series(temps_k, index=idx)

        des_dta = cal_des_dta(ta_series, self.pres_pa)

        # Validate output
        self.assertIsInstance(des_dta, pd.Series)
        self.assertEqual(len(des_dta), len(ta_series))
        self.assertTrue((des_dta > 0).all())  # All values should be positive

        # Check that slope increases with temperature (Clausius-Clapeyron)
        self.assertTrue((des_dta.diff()[1:] > 0).all())

        print(f"✓ Calculated des/dTa for {len(temps_c)} temperatures")

    def test_cal_qa(self):
        """Test specific humidity calculation."""
        print("\n========================================")
        print("Testing cal_qa (specific humidity)...")

        qa = cal_qa(self.rh_pct, self.ta_k, self.pres_hpa)

        # Validate output
        self.assertIsInstance(qa, (float, np.floating))
        self.assertGreater(qa, 0)  # Should be positive
        self.assertLess(qa, 0.05)  # Reasonable upper bound for specific humidity

        # Convert to g/kg for display
        qa_g_kg = qa * 1000
        print(
            f"✓ Specific humidity at {self.rh_pct}% RH, {self.ta_c}°C: {qa_g_kg:.2f} g/kg"
        )

    def test_cal_dq(self):
        """Test specific humidity deficit calculation."""
        print("\n========================================")
        print("Testing cal_dq (specific humidity deficit)...")

        dq = cal_dq(self.rh_pct, self.ta_c, self.pres_hpa)

        # Validate output
        self.assertIsInstance(dq, (float, np.floating))
        self.assertGreater(dq, 0)  # Should be positive for RH < 100%

        # At 100% RH, deficit should be near zero
        dq_saturated = cal_dq(100.0, self.ta_c, self.pres_hpa)
        self.assertLess(abs(dq_saturated), 1e-6)

        print(f"✓ Humidity deficit at {self.rh_pct}% RH: {dq * 1000:.2f} g/kg")

    def test_cal_rh(self):
        """Test relative humidity calculation from specific humidity."""
        print("\n========================================")
        print("Testing cal_rh (relative humidity)...")

        # Calculate qa first from known RH
        qa = cal_qa(self.rh_pct, self.ta_k, self.pres_hpa)

        # Now calculate RH back from qa
        rh_calc = cal_rh(qa, self.ta_k, self.pres_hpa)

        # Should get back approximately the same RH
        self.assertIsInstance(rh_calc, (float, np.floating))
        self.assertAlmostEqual(rh_calc, self.rh_pct, delta=1.0)  # Within 1% tolerance

        print(f"✓ Calculated RH: {rh_calc:.1f}% (expected: {self.rh_pct}%)")

    def test_cal_lat_vap(self):
        """Test latent heat of vaporisation calculation."""
        print("\n========================================")
        print("Testing cal_lat_vap (latent heat)...")

        # Calculate qa first
        qa = cal_qa(self.rh_pct, self.ta_k, self.pres_hpa)

        # Calculate latent heat
        lv = cal_lat_vap(qa, self.ta_k, self.pres_hpa)

        # Validate output
        self.assertIsInstance(lv, (float, np.floating))
        # Typical range: 2.4-2.5 MJ/kg
        self.assertGreater(lv, 2.4e6)
        self.assertLess(lv, 2.6e6)

        print(f"✓ Latent heat at {self.ta_c}°C: {lv / 1e6:.3f} MJ/kg")

    def test_cal_cp(self):
        """Test specific heat capacity calculation."""
        print("\n========================================")
        print("Testing cal_cp (specific heat capacity)...")

        # Calculate qa first
        qa = cal_qa(self.rh_pct, self.ta_k, self.pres_hpa)

        # Calculate specific heat
        cp = cal_cp(qa, self.ta_k, self.pres_hpa)

        # Validate output - cal_cp returns a Series
        self.assertIsInstance(cp, (float, np.floating, pd.Series, np.ndarray))
        cp_val = cp.iloc[0] if isinstance(cp, pd.Series) else cp
        # Typical range for moist air: 1000-1100 J/kg/K
        self.assertGreater(cp_val, 1000)
        self.assertLess(cp_val, 1200)

        print(f"✓ Specific heat capacity: {cp_val:.0f} J/kg/K")

    def test_temperature_extremes(self):
        """Test calculations at temperature extremes."""
        print("\n========================================")
        print("Testing atmospheric calculations at temperature extremes...")

        # Test at cold temperature
        ta_cold_c = -20.0
        ta_cold_k = ta_cold_c + 273.15
        qa_cold = cal_qa(self.rh_pct, ta_cold_k, self.pres_hpa)
        self.assertGreater(qa_cold, 0)
        self.assertLess(qa_cold, 0.001)  # Very low at cold temps

        # Test at hot temperature
        ta_hot_c = 40.0
        ta_hot_k = ta_hot_c + 273.15
        qa_hot = cal_qa(self.rh_pct, ta_hot_k, self.pres_hpa)
        self.assertGreater(qa_hot, qa_cold)  # Should be higher at hot temps
        self.assertLess(qa_hot, 0.05)

        print(f"✓ qa at -20°C: {qa_cold * 1000:.3f} g/kg")
        print(f"✓ qa at +40°C: {qa_hot * 1000:.3f} g/kg")

    def test_humidity_consistency(self):
        """Test consistency between humidity calculations."""
        print("\n========================================")
        print("Testing humidity calculation consistency...")

        # Calculate qa from RH
        qa = cal_qa(self.rh_pct, self.ta_k, self.pres_hpa)

        # Calculate dq
        dq = cal_dq(self.rh_pct, self.ta_c, self.pres_hpa)

        # Calculate saturated qa (at 100% RH)
        qa_sat = cal_qa(100.0, self.ta_k, self.pres_hpa)

        # Check consistency: qa + dq ≈ qa_sat
        self.assertAlmostEqual(qa + dq, qa_sat, delta=1e-3)  # Relaxed tolerance

        print(f"✓ Humidity consistency check passed")
        print(f"  qa = {qa * 1000:.3f} g/kg")
        print(f"  dq = {dq * 1000:.3f} g/kg")
        print(f"  qa_sat = {qa_sat * 1000:.3f} g/kg")


class TestAtmosphericEdgeCases(TestCase):
    """Test edge cases and error handling in atmospheric calculations."""

    def test_zero_humidity(self):
        """Test calculations at zero humidity."""
        print("\n========================================")
        print("Testing calculations at 0% RH...")

        qa_dry = cal_qa(0.0, 293.15, 1013.25)
        self.assertAlmostEqual(qa_dry, 0.0, delta=1e-10)

        dq_dry = cal_dq(0.0, 20.0, 1013.25)
        self.assertGreater(dq_dry, 0)  # Maximum deficit at 0% RH

        print("✓ Zero humidity calculations handled correctly")

    def test_saturated_conditions(self):
        """Test calculations at 100% humidity."""
        print("\n========================================")
        print("Testing calculations at 100% RH...")

        dq_sat = cal_dq(100.0, 20.0, 1013.25)
        self.assertLess(abs(dq_sat), 1e-6)  # Should be near zero

        print("✓ Saturated conditions handled correctly")

    def test_pressure_variations(self):
        """Test calculations at different pressures."""
        print("\n========================================")
        print("Testing calculations at different pressures...")

        # Sea level
        qa_sea = cal_qa(60.0, 293.15, 1013.25)

        # High altitude (e.g., 3000m, ~700 hPa)
        qa_alt = cal_qa(60.0, 293.15, 700.0)

        # qa should be different at different pressures
        self.assertNotAlmostEqual(qa_sea, qa_alt, delta=1e-5)

        print(f"✓ qa at sea level: {qa_sea * 1000:.3f} g/kg")
        print(f"✓ qa at 3000m: {qa_alt * 1000:.3f} g/kg")


class TestWindHeightCorrection(TestCase):
    """Test wind speed height correction function."""

    def test_wind_height_correction_basic(self):
        """Test basic wind speed height correction."""
        print("\n========================================")
        print("Testing wind speed height correction...")

        # Test correction from 10 m to 50 m
        ws_10m = 5.0  # m/s
        z0m = 0.1  # m

        ws_50m = correct_wind_height(ws_10m, z_meas=10, z_target=50, z0m=z0m)

        # Wind speed should increase with height
        self.assertGreater(ws_50m, ws_10m)
        # Should be physically reasonable (not more than 2x at these heights)
        self.assertLess(ws_50m, ws_10m * 2)

        print(f"✓ Wind speed at 10 m: {ws_10m:.2f} m/s")
        print(f"✓ Wind speed at 50 m: {ws_50m:.2f} m/s")
        print(f"✓ Ratio: {ws_50m / ws_10m:.2f}")

    def test_wind_height_correction_epw_standard(self):
        """Test correction from EPW standard height (10 m) to 2 m."""
        print("\n========================================")
        print("Testing EPW to screen height correction...")

        ws_10m = 5.0
        z0m = 0.1

        # Correct down to 2 m (screen height)
        ws_2m = correct_wind_height(ws_10m, z_meas=10, z_target=2, z0m=z0m)

        # Wind speed should decrease when going down
        self.assertLess(ws_2m, ws_10m)
        self.assertGreater(ws_2m, 0)

        print(f"✓ Wind speed at 10 m (EPW): {ws_10m:.2f} m/s")
        print(f"✓ Wind speed at 2 m (screen): {ws_2m:.2f} m/s")

    def test_wind_height_correction_reversibility(self):
        """Test that forward and reverse corrections are consistent."""
        print("\n========================================")
        print("Testing wind height correction reversibility...")

        ws_10m_original = 5.0
        z0m = 0.1

        # Correct from 10 m to 50 m
        ws_50m = correct_wind_height(ws_10m_original, z_meas=10, z_target=50, z0m=z0m)

        # Correct back from 50 m to 10 m
        ws_10m_back = correct_wind_height(ws_50m, z_meas=50, z_target=10, z0m=z0m)

        # Should get back approximately the original value
        self.assertAlmostEqual(ws_10m_back, ws_10m_original, delta=1e-6)

        print(f"✓ Original wind speed at 10 m: {ws_10m_original:.6f} m/s")
        print(f"✓ After correction to 50 m and back: {ws_10m_back:.6f} m/s")
        print(f"✓ Difference: {abs(ws_10m_back - ws_10m_original):.10f} m/s")

    def test_wind_height_correction_same_height(self):
        """Test that correction to same height returns original value."""
        print("\n========================================")
        print("Testing wind height correction with same height...")

        ws_original = 5.0
        z0m = 0.1

        ws_corrected = correct_wind_height(ws_original, z_meas=10, z_target=10, z0m=z0m)

        self.assertAlmostEqual(ws_corrected, ws_original, delta=1e-10)

        print(f"✓ Original wind speed: {ws_original:.2f} m/s")
        print(f"✓ Corrected (same height): {ws_corrected:.2f} m/s")

    def test_wind_height_correction_array(self):
        """Test wind speed height correction with array inputs."""
        print("\n========================================")
        print("Testing wind speed height correction with arrays...")

        # Create array of wind speeds
        ws_10m_array = np.array([3.0, 5.0, 7.0, 10.0])
        z0m = 0.1

        ws_50m_array = correct_wind_height(
            ws_10m_array, z_meas=10, z_target=50, z0m=z0m
        )

        # All values should increase
        self.assertTrue(np.all(ws_50m_array > ws_10m_array))
        # Shape should be preserved
        self.assertEqual(ws_50m_array.shape, ws_10m_array.shape)

        print(f"✓ Wind speeds at 10 m: {ws_10m_array}")
        print(f"✓ Wind speeds at 50 m: {ws_50m_array}")

    def test_wind_height_correction_roughness(self):
        """Test effect of different roughness lengths."""
        print("\n========================================")
        print("Testing wind height correction with different roughness...")

        ws_10m = 5.0

        # Urban surface (higher roughness)
        ws_50m_urban = correct_wind_height(ws_10m, z_meas=10, z_target=50, z0m=1.0)

        # Grass surface (lower roughness)
        ws_50m_grass = correct_wind_height(ws_10m, z_meas=10, z_target=50, z0m=0.01)

        # Higher roughness should lead to larger wind speed increase with height
        self.assertGreater(ws_50m_urban, ws_50m_grass)

        print(f"✓ Wind speed at 10 m: {ws_10m:.2f} m/s")
        print(f"✓ Wind speed at 50 m (urban, z0m=1.0): {ws_50m_urban:.2f} m/s")
        print(f"✓ Wind speed at 50 m (grass, z0m=0.01): {ws_50m_grass:.2f} m/s")


if __name__ == "__main__":
    unittest.main()
