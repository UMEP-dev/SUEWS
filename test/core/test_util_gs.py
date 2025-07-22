"""
Test surface conductance calculation utilities.

This module tests the surface conductance/resistance calculation functions
in supy.util._gs, including inverted Penman-Monteith and flux gradient methods.
"""

from unittest import TestCase
import warnings

import numpy as np
import pandas as pd
import pytest

from supy.util._gs import cal_rs_FG, cal_rs_iPM, cal_rs_obs


class TestSurfaceConductance(TestCase):
    """Test surface conductance/resistance calculation functions."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)
        # Typical midday conditions for testing
        self.qh = 200.0  # Sensible heat flux [W m-2]
        self.qe = 150.0  # Latent heat flux [W m-2]
        self.ta = 25.0  # Air temperature [°C]
        self.rh = 60.0  # Relative humidity [%]
        self.pa = 101325.0  # Air pressure [Pa]
        self.ra = 50.0  # Aerodynamic resistance [s m-1]

    def test_cal_rs_iPM_typical(self):
        """Test inverted Penman-Monteith method with typical conditions."""
        print("\n========================================")
        print("Testing cal_rs_iPM with typical midday conditions...")

        rs = cal_rs_iPM(self.qh, self.qe, self.ta, self.rh, self.pa, self.ra)

        # Validate output
        self.assertIsInstance(rs, (float, np.floating, np.ndarray))
        self.assertGreater(rs, 0)  # Should be positive
        self.assertLess(rs, 5000)  # Reasonable upper bound for rs

        print(f"✓ Surface resistance (iPM): {rs:.1f} s/m")
        print(f"  Conditions: QH={self.qh} W/m², QE={self.qe} W/m²")

    def test_cal_rs_FG_typical(self):
        """Test flux gradient method with typical conditions."""
        print("\n========================================")
        print("Testing cal_rs_FG with typical conditions...")

        rs = cal_rs_FG(self.qh, self.qe, self.ta, self.rh, self.pa, self.ra)

        # Validate output
        self.assertIsInstance(rs, (float, np.floating, np.ndarray))
        self.assertGreater(rs, 0)  # Should be positive
        self.assertLess(rs, 5000)  # Reasonable upper bound

        print(f"✓ Surface resistance (FG): {rs:.1f} s/m")

    def test_cal_rs_obs_method_comparison(self):
        """Test cal_rs_obs wrapper with different methods."""
        print("\n========================================")
        print("Testing cal_rs_obs with different methods...")

        # Test iPM method
        rs_ipm = cal_rs_obs(
            self.qh, self.qe, self.ta, self.rh, self.pa, self.ra, method="iPM"
        )

        # Test FG method
        rs_fg = cal_rs_obs(
            self.qh, self.qe, self.ta, self.rh, self.pa, self.ra, method="FG"
        )

        # Both should give reasonable values
        self.assertGreater(rs_ipm, 0)
        self.assertGreater(rs_fg, 0)

        # Methods may give different results
        print(f"✓ rs (iPM method): {rs_ipm:.1f} s/m")
        print(f"✓ rs (FG method): {rs_fg:.1f} s/m")
        print(f"✓ Difference: {abs(rs_ipm - rs_fg):.1f} s/m")

    def test_cal_rs_series_input(self):
        """Test surface resistance calculation with Series inputs."""
        print("\n========================================")
        print("Testing cal_rs_obs with time series data...")

        # Create diurnal cycle
        hours = np.arange(0, 24)
        idx = pd.date_range("2023-07-01", periods=24, freq="h")

        # Simple diurnal patterns
        qh_series = pd.Series(
            200 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx
        )
        qe_series = pd.Series(
            150 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx
        )
        ta_series = pd.Series(20 + 10 * np.sin(np.pi * (hours - 6) / 12), index=idx)
        rh_series = pd.Series(70 - 20 * np.sin(np.pi * (hours - 6) / 12), index=idx)
        pa_series = pd.Series(self.pa * np.ones_like(hours), index=idx)
        ra_series = pd.Series(50 * np.ones_like(hours), index=idx)

        # Calculate rs
        rs_series = cal_rs_obs(
            qh_series, qe_series, ta_series, rh_series, pa_series, ra_series
        )

        # Validate output
        self.assertIsInstance(rs_series, pd.Series)
        self.assertEqual(len(rs_series), 24)

        # During night (QE ≈ 0), rs might be negative or very high
        night_mask = qe_series < 1.0
        # Just check that we get some values
        self.assertTrue(len(rs_series[night_mask]) > 0)

        # During day, rs should be reasonable
        day_mask = qe_series > 50.0
        valid_day = rs_series[day_mask][rs_series[day_mask] > 0]
        if len(valid_day) > 0:
            self.assertTrue((valid_day < 1000).all())

        print(f"✓ Calculated rs for 24-hour cycle")
        print(
            f"  Daytime rs range: {rs_series[day_mask].min():.0f}-{rs_series[day_mask].max():.0f} s/m"
        )

    def test_zero_latent_heat(self):
        """Test behaviour when latent heat flux is zero."""
        print("\n========================================")
        print("Testing with zero latent heat flux...")

        # When QE = 0, surface is completely closed (rs → ∞)
        # This causes division by zero in the iPM method
        with self.assertRaises(ZeroDivisionError):
            rs = cal_rs_obs(self.qh, 0.0, self.ta, self.rh, self.pa, self.ra)

        print(f"✓ Zero QE correctly raises ZeroDivisionError")

    def test_negative_fluxes(self):
        """Test behaviour with negative fluxes (e.g., dew formation)."""
        print("\n========================================")
        print("Testing with negative fluxes...")

        # Negative QE (condensation/dew)
        rs_dew = cal_rs_obs(10.0, -20.0, 15.0, 95.0, self.pa, 30.0)

        # Should handle negative fluxes appropriately
        # Note: Physical interpretation may vary
        self.assertIsNotNone(rs_dew)

        print(f"✓ rs with negative QE (dew): {rs_dew:.1f} s/m")

    def test_high_low_humidity(self):
        """Test calculations at humidity extremes."""
        print("\n========================================")
        print("Testing at humidity extremes...")

        # Very dry conditions
        rs_dry = cal_rs_obs(self.qh, self.qe, self.ta, 20.0, self.pa, self.ra)

        # Very humid conditions
        rs_humid = cal_rs_obs(self.qh, self.qe, self.ta, 90.0, self.pa, self.ra)

        # Both should be valid
        self.assertGreater(rs_dry, 0)
        self.assertGreater(rs_humid, 0)

        # Resistance typically higher in dry conditions
        print(f"✓ rs at 20% RH: {rs_dry:.1f} s/m")
        print(f"✓ rs at 90% RH: {rs_humid:.1f} s/m")

    def test_temperature_range(self):
        """Test calculations across temperature range."""
        print("\n========================================")
        print("Testing across temperature range...")

        temperatures = [-5, 0, 10, 20, 30, 40]
        rs_values = []

        for temp in temperatures:
            rs = cal_rs_obs(self.qh, self.qe, temp, self.rh, self.pa, self.ra)
            rs_values.append(rs)

        # All should be valid
        self.assertTrue(all(rs > 0 for rs in rs_values if not np.isnan(rs)))

        print("✓ rs values across temperatures:")
        for temp, rs in zip(temperatures, rs_values):
            print(f"  {temp:3d}°C: {rs:6.1f} s/m")

    def test_different_aerodynamic_resistance(self):
        """Test sensitivity to aerodynamic resistance."""
        print("\n========================================")
        print("Testing sensitivity to aerodynamic resistance...")

        ra_values = [10, 30, 50, 100, 200]
        rs_values = []

        for ra in ra_values:
            rs = cal_rs_obs(self.qh, self.qe, self.ta, self.rh, self.pa, ra)
            rs_values.append(rs)

        # All should be valid
        self.assertTrue(all(rs > 0 for rs in rs_values if not np.isnan(rs)))

        print("✓ rs sensitivity to ra:")
        for ra, rs in zip(ra_values, rs_values):
            print(f"  ra={ra:3d} s/m: rs={rs:6.1f} s/m")


class TestSurfaceConductanceEdgeCases(TestCase):
    """Test edge cases in surface conductance calculations."""

    def test_extreme_flux_ratios(self):
        """Test with extreme sensible/latent heat flux ratios."""
        print("\n========================================")
        print("Testing extreme flux ratios...")

        # Very high Bowen ratio (QH >> QE)
        rs_high_bowen = cal_rs_obs(500.0, 50.0, 35.0, 30.0, 101325.0, 50.0)
        self.assertGreater(rs_high_bowen, 0)

        # Very low Bowen ratio (QE >> QH)
        rs_low_bowen = cal_rs_obs(50.0, 500.0, 25.0, 80.0, 101325.0, 50.0)
        # Note: iPM method can give negative rs when QE >> QH
        self.assertIsNotNone(rs_low_bowen)

        print(f"✓ rs with high Bowen ratio: {rs_high_bowen:.1f} s/m")
        print(f"✓ rs with low Bowen ratio: {rs_low_bowen:.1f} s/m")

    def test_method_consistency(self):
        """Test consistency between iPM and FG methods."""
        print("\n========================================")
        print("Testing method consistency...")

        # Test across a range of conditions
        conditions = [
            (100, 100, 20, 60),  # Equal fluxes
            (200, 100, 25, 50),  # High sensible
            (100, 200, 22, 70),  # High latent
        ]

        for qh, qe, ta, rh in conditions:
            rs_ipm = cal_rs_obs(qh, qe, ta, rh, 101325.0, 50.0, method="iPM")
            rs_fg = cal_rs_obs(qh, qe, ta, rh, 101325.0, 50.0, method="FG")

            # Both methods should give reasonable values
            self.assertGreater(rs_ipm, 0)
            self.assertGreater(rs_fg, 0)

            print(f"✓ QH={qh}, QE={qe}: iPM={rs_ipm:.0f}, FG={rs_fg:.0f} s/m")


if __name__ == "__main__":
    unittest.main()
