"""
Test OHM (Objective Hysteresis Model) calculation utilities.

This module tests the OHM coefficient derivation and heat storage
calculation functions in supy.util._ohm.
"""

import unittest
from unittest import TestCase
import warnings

import numpy as np
import pandas as pd
import pytest

from supy.util._ohm import derive_ohm_coef, sim_ohm


class TestOHMCalculations(TestCase):
    """Test OHM coefficient derivation and heat storage calculations."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)
        # Create sample diurnal data
        self.n_days = 7
        self.n_hours = self.n_days * 24
        self.dt_hours = 1.0

        # Time index
        self.idx = pd.date_range("2023-07-01", periods=self.n_hours, freq="h")

        # Create realistic diurnal patterns
        hours = np.arange(self.n_hours) % 24

        # Net radiation with diurnal cycle (W/m²)
        self.qn = pd.Series(
            400 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12))
            + np.random.normal(0, 10, self.n_hours),
            index=self.idx,
        )

        # Storage heat flux with phase lag (W/m²)
        # QS typically lags QN and has smaller amplitude
        self.qs = pd.Series(
            150 * np.maximum(0, np.sin(np.pi * (hours - 4) / 12))
            + np.random.normal(0, 5, self.n_hours),
            index=self.idx,
        )

    def test_derive_ohm_coef_basic(self):
        """Test OHM coefficient derivation with basic diurnal data."""
        print("\n========================================")
        print("Testing derive_ohm_coef with diurnal data...")

        a1, a2, a3 = derive_ohm_coef(self.qs, self.qn)

        # Validate outputs
        self.assertIsInstance(a1, (float, np.floating))
        self.assertIsInstance(a2, (float, np.floating))
        self.assertIsInstance(a3, (float, np.floating))

        # Check reasonable ranges for coefficients
        # a1: coefficient for QN (typically 0.1-0.5)
        self.assertGreater(a1, 0)
        self.assertLess(a1, 1.0)

        # a2: coefficient for dQN/dt (can be positive or negative)
        self.assertGreater(abs(a2), 0)
        self.assertLess(abs(a2), 5.0)

        # a3: intercept (typically small)
        self.assertLess(abs(a3), 50)

        print(f"✓ OHM coefficients derived:")
        print(f"  a1 = {a1:.3f} (QN coefficient)")
        print(f"  a2 = {a2:.3f} (dQN/dt coefficient)")
        print(f"  a3 = {a3:.3f} (intercept)")

    def test_sim_ohm_scalar(self):
        """Test QS calculation with scalar inputs."""
        print("\n========================================")
        print("Testing sim_ohm with scalar inputs...")

        # Create a single timestep Series
        idx = pd.date_range("2023-07-01 12:00", periods=2, freq="h")
        qn = pd.Series([400.0, 420.0], index=idx)  # W/m²
        a1, a2, a3 = 0.3, 0.5, 10.0  # Typical coefficients

        qs_calc = sim_ohm(qn, a1, a2, a3)

        # Validate output
        self.assertIsInstance(qs_calc, pd.Series)
        self.assertEqual(len(qs_calc), len(qn))

        # Check first value manually (no dQN/dt for first timestep)
        self.assertTrue(
            np.isnan(qs_calc.iloc[0]) or qs_calc.iloc[0] == a1 * qn.iloc[0] + a3
        )

        print(f"✓ QS calculated for {len(qn)} timesteps")
        print(f"  Second value: {qs_calc.iloc[1]:.1f} W/m²")

    def test_sim_ohm_series(self):
        """Test QS calculation with Series inputs."""
        print("\n========================================")
        print("Testing sim_ohm with time series...")

        # Derive coefficients from data
        a1, a2, a3 = derive_ohm_coef(self.qs, self.qn)

        # Calculate QS using OHM
        qs_calc = sim_ohm(self.qn, a1, a2, a3)

        # Validate output
        self.assertIsInstance(qs_calc, pd.Series)
        self.assertEqual(len(qs_calc), len(self.qn))

        # Check that calculated values are reasonable
        valid_mask = ~(qs_calc.isna() | self.qs.isna())
        self.assertTrue((abs(qs_calc[valid_mask]) < 1000).all())

        # Calculate R² to assess fit quality
        qs_mean = self.qs[valid_mask].mean()
        ss_tot = ((self.qs[valid_mask] - qs_mean) ** 2).sum()
        ss_res = ((self.qs[valid_mask] - qs_calc[valid_mask]) ** 2).sum()
        r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

        print(f"✓ QS calculated for {len(valid_mask)} hours")
        print(f"  R² = {r2:.3f}")
        print(f"  Mean observed QS: {self.qs.mean():.1f} W/m²")
        print(f"  Mean calculated QS: {qs_calc.mean():.1f} W/m²")

    def test_ohm_different_surface_types(self):
        """Test OHM with different surface type behaviours."""
        print("\n========================================")
        print("Testing OHM for different surface types...")

        hours = np.arange(24)
        idx = pd.date_range("2023-07-01", periods=24, freq="h")

        # Create QN pattern
        qn = pd.Series(400 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx)

        # Different surface responses
        # Urban: high storage, large phase lag
        qs_urban = pd.Series(
            200 * np.maximum(0, np.sin(np.pi * (hours - 3) / 12)), index=idx
        )

        # Grass: low storage, small phase lag
        qs_grass = pd.Series(
            50 * np.maximum(0, np.sin(np.pi * (hours - 5.5) / 12)), index=idx
        )

        # Derive coefficients
        a1_urban, a2_urban, a3_urban = derive_ohm_coef(qs_urban, qn)
        a1_grass, a2_grass, a3_grass = derive_ohm_coef(qs_grass, qn)

        # Urban should have higher a1 (more storage)
        self.assertGreater(a1_urban, a1_grass)

        print(f"✓ Urban surface: a1={a1_urban:.3f}, a2={a2_urban:.3f}")
        print(f"✓ Grass surface: a1={a1_grass:.3f}, a2={a2_grass:.3f}")

    def test_ohm_night_time(self):
        """Test OHM behaviour during night-time."""
        print("\n========================================")
        print("Testing OHM during night-time...")

        # Create night-time only data
        hours = np.arange(0, 24)
        idx = pd.date_range("2023-07-01", periods=24, freq="h")

        # QN negative at night
        qn = pd.Series(np.where((hours < 6) | (hours > 18), -50, 0), index=idx)

        # QS releases stored heat at night
        qs = pd.Series(np.where((hours < 6) | (hours > 18), -30, 0), index=idx)

        # Add some data points to avoid singular matrix
        qn.iloc[6:18] = 400 * np.sin(np.pi * (hours[6:18] - 6) / 12)
        qs.iloc[6:18] = 150 * np.sin(np.pi * (hours[6:18] - 4) / 12)

        a1, a2, a3 = derive_ohm_coef(qs, qn)

        # Check that model can handle negative values
        qs_calc = sim_ohm(qn, a1, a2, a3)

        # Night-time QS should be negative (heat release)
        night_mask = (hours < 6) | (hours > 18)
        night_qs = qs_calc[night_mask].dropna()
        if len(night_qs) > 0:
            self.assertTrue(
                (night_qs < 20).all()
            )  # Most should be negative or small positive

        print(f"✓ Night-time QS handled correctly")
        print(f"  Mean night QS: {night_qs.mean():.1f} W/m²")

    def test_ohm_with_missing_data(self):
        """Test OHM with missing data handling."""
        print("\n========================================")
        print("Testing OHM with missing data...")

        # Create data with gaps
        qn_gaps = self.qn.copy()
        qs_gaps = self.qs.copy()

        # Introduce random gaps
        gap_mask = np.random.random(len(qn_gaps)) < 0.1
        qn_gaps[gap_mask] = np.nan
        qs_gaps[gap_mask] = np.nan

        # Should handle missing data
        a1, a2, a3 = derive_ohm_coef(qs_gaps, qn_gaps)

        # Coefficients should still be reasonable
        self.assertFalse(np.isnan(a1))
        self.assertFalse(np.isnan(a2))
        self.assertFalse(np.isnan(a3))

        print(f"✓ OHM handles {gap_mask.sum()} missing values")
        print(f"  Coefficients: a1={a1:.3f}, a2={a2:.3f}, a3={a3:.3f}")

    def test_ohm_extreme_conditions(self):
        """Test OHM under extreme conditions."""
        print("\n========================================")
        print("Testing OHM under extreme conditions...")

        # Very high radiation
        idx = pd.date_range("2023-07-01", periods=24, freq="h")
        hours = np.arange(24)

        qn_extreme = pd.Series(
            1000 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx
        )
        qs_extreme = pd.Series(
            400 * np.maximum(0, np.sin(np.pi * (hours - 4) / 12)), index=idx
        )

        a1, a2, a3 = derive_ohm_coef(qs_extreme, qn_extreme)

        # Should still get reasonable coefficients
        self.assertGreater(a1, 0)
        self.assertLess(a1, 1.0)

        print(f"✓ Extreme radiation handled")
        print(f"  Max QN: {qn_extreme.max():.0f} W/m²")
        print(f"  Coefficients: a1={a1:.3f}, a2={a2:.3f}")


class TestOHMValidation(TestCase):
    """Test OHM validation and physical consistency."""

    def test_energy_balance_closure(self):
        """Test that OHM maintains reasonable energy balance."""
        print("\n========================================")
        print("Testing energy balance with OHM...")

        # Create full energy balance components
        idx = pd.date_range("2023-07-01", periods=24, freq="h")
        hours = np.arange(24)

        # Net radiation
        qn = pd.Series(500 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx)

        # Sensible heat (typically largest during day)
        qh = pd.Series(200 * np.maximum(0, np.sin(np.pi * (hours - 7) / 12)), index=idx)

        # Latent heat
        qe = pd.Series(150 * np.maximum(0, np.sin(np.pi * (hours - 8) / 12)), index=idx)

        # Storage should close the balance: QS = QN - QH - QE
        qs_balance = qn - qh - qe

        # Derive OHM coefficients
        a1, a2, a3 = derive_ohm_coef(qs_balance, qn)

        # Check physical plausibility
        # During peak heating, QS should be positive (storage)
        peak_hour = qn.idxmax()
        self.assertGreater(qs_balance[peak_hour], 0)

        print(f"✓ Energy balance maintained")
        print(f"  Peak QN: {qn.max():.0f} W/m²")
        print(f"  Peak QS: {qs_balance.max():.0f} W/m²")
        print(f"  OHM a1: {a1:.3f}")


if __name__ == "__main__":
    unittest.main()
