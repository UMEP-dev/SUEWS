"""Test OHM (Objective Hysteresis Model) calculation utilities."""

import numpy as np
import pandas as pd
import pytest

from supy.util._ohm import derive_ohm_coef, sim_ohm


class TestOHMCalculations:
    """Test OHM coefficient derivation and heat storage calculations."""

    @pytest.fixture
    def diurnal_data(self):
        """Create sample diurnal radiation/storage data."""
        n_days = 7
        n_hours = n_days * 24
        idx = pd.date_range("2023-07-01", periods=n_hours, freq="h")
        hours = np.arange(n_hours) % 24

        # Net radiation with diurnal cycle
        qn = pd.Series(
            400 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12))
            + np.random.normal(0, 10, n_hours),
            index=idx,
        )
        # Storage heat flux with phase lag
        qs = pd.Series(
            150 * np.maximum(0, np.sin(np.pi * (hours - 4) / 12))
            + np.random.normal(0, 5, n_hours),
            index=idx,
        )
        return qn, qs

    def test_coefficient_derivation(self, diurnal_data):
        """Test OHM coefficient derivation gives physically reasonable values."""
        qn, qs = diurnal_data
        a1, a2, a3 = derive_ohm_coef(qs, qn)

        # a1: QN coefficient (typically 0.1-0.5 for urban surfaces)
        assert 0 < a1 < 1.0, f"a1={a1:.3f} outside expected range"
        # a2: dQN/dt coefficient
        assert abs(a2) < 5.0, f"a2={a2:.3f} unreasonably large"
        # a3: intercept
        assert abs(a3) < 50, f"a3={a3:.3f} unreasonably large"

    def test_model_fit_quality(self, diurnal_data):
        """Test that OHM model fits observations reasonably well."""
        qn, qs = diurnal_data
        a1, a2, a3 = derive_ohm_coef(qs, qn)
        qs_calc = sim_ohm(qn, a1, a2, a3)

        # Calculate R²
        valid = ~(qs_calc.isna() | qs.isna())
        ss_tot = ((qs[valid] - qs[valid].mean()) ** 2).sum()
        ss_res = ((qs[valid] - qs_calc[valid]) ** 2).sum()
        r2 = 1 - (ss_res / ss_tot)

        assert r2 > 0.5, f"R²={r2:.3f} indicates poor model fit"

    def test_surface_type_differences(self):
        """Test that urban surfaces have higher storage than grass."""
        hours = np.arange(24)
        idx = pd.date_range("2023-07-01", periods=24, freq="h")
        qn = pd.Series(400 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx)

        # Urban: high storage, large phase lag
        qs_urban = pd.Series(200 * np.maximum(0, np.sin(np.pi * (hours - 3) / 12)), index=idx)
        # Grass: low storage, small phase lag
        qs_grass = pd.Series(50 * np.maximum(0, np.sin(np.pi * (hours - 5.5) / 12)), index=idx)

        a1_urban, _, _ = derive_ohm_coef(qs_urban, qn)
        a1_grass, _, _ = derive_ohm_coef(qs_grass, qn)

        assert a1_urban > a1_grass, "Urban should have higher a1 than grass"

    def test_energy_balance_closure(self):
        """Test that OHM-derived storage closes energy balance."""
        hours = np.arange(24)
        idx = pd.date_range("2023-07-01", periods=24, freq="h")

        # Create balanced energy budget
        qn = pd.Series(500 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx)
        qh = pd.Series(200 * np.maximum(0, np.sin(np.pi * (hours - 7) / 12)), index=idx)
        qe = pd.Series(150 * np.maximum(0, np.sin(np.pi * (hours - 8) / 12)), index=idx)
        qs = qn - qh - qe  # Storage by residual

        a1, a2, a3 = derive_ohm_coef(qs, qn)

        # Peak storage should be positive (storing heat)
        assert qs[qn.idxmax()] > 0, "Peak storage should be positive"
        # a1 should be positive
        assert a1 > 0, "a1 should be positive for energy balance"

    def test_missing_data_handling(self, diurnal_data):
        """Test OHM handles missing data gracefully."""
        qn, qs = diurnal_data

        # Introduce gaps
        qn_gaps = qn.copy()
        qs_gaps = qs.copy()
        gap_mask = np.random.random(len(qn)) < 0.1
        qn_gaps[gap_mask] = np.nan
        qs_gaps[gap_mask] = np.nan

        a1, a2, a3 = derive_ohm_coef(qs_gaps, qn_gaps)

        assert not np.isnan(a1), "a1 should not be NaN with missing data"
        assert not np.isnan(a2), "a2 should not be NaN with missing data"
        assert not np.isnan(a3), "a3 should not be NaN with missing data"
