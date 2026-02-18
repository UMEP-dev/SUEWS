"""
Tests for the T2/q2 attribution module.

Tests cover:
1. Mathematical correctness of Shapley decomposition
2. Physical calculation functions
3. Main attribution functions with realistic data
4. Edge cases and error handling
5. AttributionResult class methods

These tests ensure the attribution module produces mathematically correct
and physically meaningful decompositions of near-surface variable changes.
"""

from unittest import TestCase
import warnings

import numpy as np
import pandas as pd
import pytest

from supy.util._attribution import (
    AttributionResult,
    attribute_t2,
    diagnose_t2,
)
from supy.util._attribution._core import (
    shapley_binary_product,
    shapley_triple_product,
)
from supy.util._attribution._helpers import extract_suews_group
from supy.util._attribution._physics import (
    cal_gamma_heat,
    cal_gamma_humidity,
    cal_r_eff_heat,
    decompose_flux_budget,
)


class TestShapleyDecomposition(TestCase):
    """Test Shapley value calculations for exact closure."""

    def test_triple_product_closure_scalars(self):
        """Sum of Shapley values must equal change in product for scalars."""
        # Known values: f = x * y * z
        x_A, y_A, z_A = 2.0, 3.0, 4.0  # f_A = 24
        x_B, y_B, z_B = 3.0, 4.0, 5.0  # f_B = 60
        expected_delta = 60 - 24  # = 36

        Phi_x, Phi_y, Phi_z = shapley_triple_product(
            np.array([x_A]),
            np.array([x_B]),
            np.array([y_A]),
            np.array([y_B]),
            np.array([z_A]),
            np.array([z_B]),
        )

        actual_sum = Phi_x[0] + Phi_y[0] + Phi_z[0]
        self.assertAlmostEqual(
            actual_sum,
            expected_delta,
            places=10,
            msg=f"Shapley closure failed: {actual_sum} != {expected_delta}",
        )

    def test_triple_product_closure_arrays(self):
        """Sum of Shapley values must equal change in product for arrays."""
        n = 100
        np.random.seed(42)
        x_A = np.random.uniform(1, 10, n)
        y_A = np.random.uniform(1, 10, n)
        z_A = np.random.uniform(1, 10, n)
        x_B = np.random.uniform(1, 10, n)
        y_B = np.random.uniform(1, 10, n)
        z_B = np.random.uniform(1, 10, n)

        f_A = x_A * y_A * z_A
        f_B = x_B * y_B * z_B
        expected_delta = f_B - f_A

        Phi_x, Phi_y, Phi_z = shapley_triple_product(x_A, x_B, y_A, y_B, z_A, z_B)

        actual_sum = Phi_x + Phi_y + Phi_z

        np.testing.assert_allclose(
            actual_sum,
            expected_delta,
            rtol=1e-10,
            err_msg="Shapley closure failed for array inputs",
        )

    def test_triple_product_zero_change(self):
        """When one factor doesn't change, its Shapley value should be zero."""
        x_A = np.array([2.0, 3.0, 4.0])
        x_B = np.array([2.0, 3.0, 4.0])  # No change in x
        y_A = np.array([1.0, 2.0, 3.0])
        y_B = np.array([2.0, 4.0, 6.0])
        z_A = np.array([5.0, 5.0, 5.0])
        z_B = np.array([10.0, 10.0, 10.0])

        Phi_x, Phi_y, Phi_z = shapley_triple_product(x_A, x_B, y_A, y_B, z_A, z_B)

        np.testing.assert_allclose(
            Phi_x,
            np.zeros_like(Phi_x),
            atol=1e-15,
            err_msg="Shapley value should be zero when factor doesn't change",
        )

    def test_triple_product_symmetry(self):
        """Equal changes should produce equal Shapley values."""
        # When all factors change by same multiplier from same base
        base = 2.0
        x_A = np.array([base])
        y_A = np.array([base])
        z_A = np.array([base])
        # All increase by factor of 2
        x_B = np.array([base * 2])
        y_B = np.array([base * 2])
        z_B = np.array([base * 2])

        Phi_x, Phi_y, Phi_z = shapley_triple_product(x_A, x_B, y_A, y_B, z_A, z_B)

        # All contributions should be equal due to symmetry
        self.assertAlmostEqual(
            Phi_x[0],
            Phi_y[0],
            places=10,
            msg="Symmetric changes should produce equal Shapley values",
        )
        self.assertAlmostEqual(
            Phi_y[0],
            Phi_z[0],
            places=10,
            msg="Symmetric changes should produce equal Shapley values",
        )

    def test_binary_product_closure(self):
        """Sum of Shapley values must equal change in binary product."""
        n = 100
        np.random.seed(42)
        x_A = np.random.uniform(1, 10, n)
        y_A = np.random.uniform(1, 10, n)
        x_B = np.random.uniform(1, 10, n)
        y_B = np.random.uniform(1, 10, n)

        f_A = x_A * y_A
        f_B = x_B * y_B
        expected_delta = f_B - f_A

        Phi_x, Phi_y = shapley_binary_product(x_A, x_B, y_A, y_B)

        actual_sum = Phi_x + Phi_y

        np.testing.assert_allclose(
            actual_sum,
            expected_delta,
            rtol=1e-10,
            err_msg="Binary Shapley closure failed",
        )


class TestPhysicalCalculations(TestCase):
    """Test physical quantity calculations."""

    def test_gamma_heat_realistic_values(self):
        """Test gamma calculation produces realistic values."""
        # At 20 degC, 50% RH, 1013.25 hPa:
        # rho ~ 1.2 kg/m3, cp ~ 1005 J/(kg*K)
        # gamma ~ 1/(1.2*1005) ~ 8.3e-4 K*m3/J
        temp_C = np.array([20.0])
        rh_pct = np.array([50.0])
        press_hPa = np.array([1013.25])

        gamma = cal_gamma_heat(temp_C, rh_pct, press_hPa)

        # Check order of magnitude
        self.assertGreater(gamma[0], 1e-4, "gamma_heat too small")
        self.assertLess(gamma[0], 1e-3, "gamma_heat too large")

    def test_gamma_humidity_realistic_values(self):
        """Test humidity gamma produces realistic values."""
        temp_C = np.array([20.0])
        rh_pct = np.array([50.0])
        press_hPa = np.array([1013.25])

        gamma = cal_gamma_humidity(temp_C, rh_pct, press_hPa)

        # gamma_humidity ~ 1/(rho*Lv) ~ 1/(1.2*2.5e6) ~ 3e-7
        self.assertGreater(gamma[0], 1e-8, "gamma_humidity too small")
        self.assertLess(gamma[0], 1e-5, "gamma_humidity too large")

    def test_r_eff_heat_low_flux_produces_nan(self):
        """Low flux values should produce NaN in resistance calculation."""
        T2 = np.array([25.0, 26.0, 27.0])
        T_ref = np.array([20.0, 20.0, 20.0])
        QH = np.array([100.0, 0.05, 200.0])  # Middle value below threshold
        gamma = np.array([8e-4, 8e-4, 8e-4])
        min_flux = 0.1

        r_eff = cal_r_eff_heat(T2, T_ref, QH, gamma, min_flux)

        self.assertFalse(np.isnan(r_eff[0]), "Valid flux should not produce NaN")
        self.assertTrue(np.isnan(r_eff[1]), "Low flux should produce NaN")
        self.assertFalse(np.isnan(r_eff[2]), "Valid flux should not produce NaN")


class TestFluxBudgetDecomposition(TestCase):
    """Test hierarchical flux budget decomposition."""

    def test_flux_budget_sums_to_total(self):
        """Flux component contributions should sum to flux_total."""
        n = 50
        np.random.seed(42)

        # Create component values first
        Qstar_A = np.random.uniform(100, 400, n)
        QE_A = np.random.uniform(50, 150, n)
        QS_A = np.random.uniform(20, 80, n)
        QF_A = np.random.uniform(0, 50, n)

        # Different in scenario B
        Qstar_B = Qstar_A + np.random.uniform(-50, 50, n)
        QE_B = QE_A + np.random.uniform(-30, 30, n)
        QS_B = QS_A + np.random.uniform(-20, 20, n)
        QF_B = QF_A + np.random.uniform(-10, 10, n)

        # Compute QH from energy budget: QH = Q* - QE - QS + QF
        # This ensures physical consistency
        QH_A = Qstar_A - QE_A - QS_A + QF_A
        QH_B = Qstar_B - QE_B - QS_B + QF_B

        # Component dictionary with signs matching energy budget
        components_A = {"Qstar": Qstar_A, "QE": -QE_A, "dQS": -QS_A, "QF": QF_A}
        components_B = {"Qstar": Qstar_B, "QE": -QE_B, "dQS": -QS_B, "QF": QF_B}

        # Total flux contribution (arbitrary for this test)
        total_contribution = np.random.uniform(-10, 10, n)

        contributions = decompose_flux_budget(
            QH_A, QH_B, components_A, components_B, total_contribution
        )

        # Sum of contributions should equal total
        sum_contributions = sum(contributions.values())

        # Only check where d_flux is significant (not NaN)
        d_flux = QH_B - QH_A
        valid_mask = np.abs(d_flux) >= 1e-10

        np.testing.assert_allclose(
            sum_contributions[valid_mask],
            total_contribution[valid_mask],
            rtol=1e-10,
            err_msg="Flux budget contributions don't sum to total",
        )


class TestExtractSuewsGroup(TestCase):
    """Test DataFrame extraction and validation."""

    def test_flat_dataframe_passes_through(self):
        """Flat DataFrame should pass through unchanged."""
        df = pd.DataFrame({"T2": [20, 21, 22], "QH": [100, 150, 200]})

        result = extract_suews_group(df)

        pd.testing.assert_frame_equal(result, df)

    def test_missing_columns_raises_error(self):
        """Missing required columns should raise ValueError."""
        df = pd.DataFrame({"T2": [20, 21, 22]})  # Missing QH

        with self.assertRaises(ValueError) as context:
            extract_suews_group(df, required_cols={"T2", "QH"})

        self.assertIn("QH", str(context.exception))

    def test_multiindex_extraction(self):
        """MultiIndex DataFrame should extract SUEWS group."""
        # Create MultiIndex DataFrame
        data = {
            ("SUEWS", "T2"): [20, 21, 22],
            ("SUEWS", "QH"): [100, 150, 200],
            ("Other", "var"): [1, 2, 3],
        }
        df = pd.DataFrame(data)

        result = extract_suews_group(df, required_cols={"T2", "QH"})

        self.assertIn("T2", result.columns)
        self.assertIn("QH", result.columns)
        self.assertEqual(len(result), 3)


class TestAttributeT2Integration(TestCase):
    """Integration tests for T2 attribution with mock data."""

    def setUp(self):
        """Create mock SUEWS output and forcing data."""
        np.random.seed(42)
        n = 100

        # Create time index
        dates = pd.date_range("2020-01-01", periods=n, freq="h")

        # Create forcing data (required for attribution)
        self.df_forcing = pd.DataFrame(
            {
                "Tair": 15 + 5 * np.sin(np.linspace(0, 4 * np.pi, n)),
                "RH": 60 + 10 * np.sin(np.linspace(0, 4 * np.pi, n)),
                "pres": 1013.25 + np.random.normal(0, 2, n),
            },
            index=dates,
        )

        # Scenario A: baseline
        self.df_A = pd.DataFrame(
            {
                "T2": 20
                + 5 * np.sin(np.linspace(0, 4 * np.pi, n))
                + np.random.normal(0, 0.5, n),
                "QH": 100
                + 50 * np.sin(np.linspace(0, 4 * np.pi, n))
                + np.random.normal(0, 5, n),
                "QE": 50
                + 20 * np.sin(np.linspace(0, 4 * np.pi, n))
                + np.random.normal(0, 2, n),
                "QN": 200 + 100 * np.sin(np.linspace(0, 4 * np.pi, n)),
                "QS": 30 + 15 * np.sin(np.linspace(0, 4 * np.pi, n)),
                "QF": 20 + np.random.normal(0, 2, n),
            },
            index=dates,
        )

        # Scenario B: modified (e.g., more vegetation)
        self.df_B = pd.DataFrame(
            {
                "T2": self.df_A["T2"] - 1.5 + np.random.normal(0, 0.3, n),  # Cooler
                "QH": self.df_A["QH"]
                - 20
                + np.random.normal(0, 3, n),  # Less sensible heat
                "QE": self.df_A["QE"]
                + 15
                + np.random.normal(0, 2, n),  # More evaporation
                "QN": self.df_A["QN"] - 10,  # Slightly less net radiation
                "QS": self.df_A["QS"] - 5,  # Less storage
                "QF": self.df_A["QF"],  # Same anthropogenic heat
            },
            index=dates,
        )

    def test_attribute_t2_produces_result(self):
        """Attribution should produce a valid result object."""
        result = attribute_t2(
            self.df_A, self.df_B, self.df_forcing, self.df_forcing
        )

        self.assertIsInstance(result, AttributionResult)
        self.assertEqual(result.variable, "T2")
        self.assertIn("delta_total", result.contributions.columns)
        self.assertIn("flux_total", result.contributions.columns)
        self.assertIn("resistance", result.contributions.columns)
        self.assertIn("air_props", result.contributions.columns)

    def test_attribute_t2_closure(self):
        """Attribution components should sum to total change."""
        result = attribute_t2(
            self.df_A, self.df_B, self.df_forcing, self.df_forcing
        )

        delta_total = result.contributions["delta_total"]
        sum_components = (
            result.contributions["T_ref"]
            + result.contributions["flux_total"]
            + result.contributions["resistance"]
            + result.contributions["air_props"]
        )

        # Closure should be very tight (Shapley guarantees exactness)
        np.testing.assert_allclose(
            sum_components.values,
            delta_total.values,
            rtol=1e-10,
            err_msg="Attribution closure failed",
        )

    def test_attribute_t2_closure_different_forcing(self):
        """Closure must hold when scenarios have different forcing."""
        np.random.seed(99)
        n = len(self.df_forcing)

        # Create a different forcing for scenario B (warmer by 2 degC)
        df_forcing_B = self.df_forcing.copy()
        df_forcing_B["Tair"] = df_forcing_B["Tair"] + 2.0

        result = attribute_t2(
            self.df_A, self.df_B, self.df_forcing, df_forcing_B
        )

        delta_total = result.contributions["delta_total"]
        sum_components = (
            result.contributions["T_ref"]
            + result.contributions["flux_total"]
            + result.contributions["resistance"]
            + result.contributions["air_props"]
        )

        # T_ref should be non-zero since forcing differs
        self.assertFalse(
            np.allclose(result.contributions["T_ref"].values, 0),
            "T_ref should be non-zero with different forcing",
        )

        # Closure should still hold exactly
        np.testing.assert_allclose(
            sum_components.values,
            delta_total.values,
            rtol=1e-10,
            err_msg="Attribution closure failed with different forcing",
        )

    def test_attribute_t2_correct_sign(self):
        """Negative T2 change should produce negative mean contribution."""
        result = attribute_t2(
            self.df_A, self.df_B, self.df_forcing, self.df_forcing
        )

        mean_delta = result.contributions["delta_total"].mean()

        # We constructed B to be cooler than A
        self.assertLess(
            mean_delta, 0, "Mean delta_T2 should be negative (B cooler than A)"
        )

    def test_attribute_t2_hierarchical_decomposition(self):
        """Hierarchical mode should include flux budget components."""
        result = attribute_t2(
            self.df_A, self.df_B, self.df_forcing, self.df_forcing, hierarchical=True
        )

        # Should have flux sub-components
        flux_cols = [c for c in result.contributions.columns if c.startswith("flux_")]
        self.assertGreater(
            len(flux_cols), 1, "Hierarchical mode should add flux sub-components"
        )

    def test_attribute_t2_no_overlap_raises(self):
        """Non-overlapping indices should raise ValueError."""
        dates_A = pd.date_range("2020-01-01", periods=100, freq="h")
        dates_B = pd.date_range("2021-01-01", periods=100, freq="h")  # Different year

        df_A = pd.DataFrame({"T2": [20] * 100, "QH": [100] * 100}, index=dates_A)
        df_B = pd.DataFrame({"T2": [21] * 100, "QH": [110] * 100}, index=dates_B)
        df_forcing_A = pd.DataFrame(
            {"Tair": [15] * 100, "RH": [60] * 100, "pres": [1013.25] * 100},
            index=dates_A,
        )
        df_forcing_B = pd.DataFrame(
            {"Tair": [15] * 100, "RH": [60] * 100, "pres": [1013.25] * 100},
            index=dates_B,
        )

        with self.assertRaises(ValueError) as context:
            attribute_t2(df_A, df_B, df_forcing_A, df_forcing_B)

        self.assertIn("overlapping", str(context.exception).lower())


class TestAttributionResult(TestCase):
    """Test AttributionResult class methods."""

    def setUp(self):
        """Create a mock AttributionResult."""
        dates = pd.date_range("2020-01-01", periods=24, freq="h")
        self.result = AttributionResult(
            variable="T2",
            contributions=pd.DataFrame(
                {
                    "delta_total": np.random.normal(-1.5, 0.5, 24),
                    "flux_total": np.random.normal(-0.9, 0.3, 24),
                    "resistance": np.random.normal(-0.4, 0.2, 24),
                    "air_props": np.random.normal(-0.2, 0.1, 24),
                },
                index=dates,
            ),
            summary=pd.DataFrame(
                {
                    "mean": [-1.5, -0.9, -0.4, -0.2],
                    "std": [0.5, 0.3, 0.2, 0.1],
                    "min": [-2.5, -1.5, -0.8, -0.4],
                    "max": [-0.5, -0.3, 0.0, 0.0],
                },
                index=["delta_total", "flux_total", "resistance", "air_props"],
            ),
            metadata={"n_timesteps": 24},
        )

    def test_repr_produces_string(self):
        """__repr__ should produce a readable string."""
        repr_str = repr(self.result)

        self.assertIsInstance(repr_str, str)
        self.assertIn("T2", repr_str)
        self.assertIn("delta_T2", repr_str)
        self.assertIn("flux_total", repr_str)

    def test_to_dataframe(self):
        """to_dataframe should return a copy of contributions."""
        df = self.result.to_dataframe()

        pd.testing.assert_frame_equal(df, self.result.contributions)
        # Should be a copy, not the same object
        self.assertIsNot(df, self.result.contributions)


class TestDiagnoseT2(TestCase):
    """Test diagnose_t2 function."""

    def setUp(self):
        """Create mock SUEWS output and forcing with clear diurnal pattern."""
        np.random.seed(42)
        # 7 days of hourly data
        dates = pd.date_range("2020-07-01", periods=168, freq="h")
        hours = dates.hour

        # Create diurnal pattern: warmer in afternoon
        diurnal = 5 * np.sin((hours - 6) * np.pi / 12)
        diurnal = np.where(hours < 6, 0, diurnal)
        diurnal = np.where(hours > 18, 0, diurnal)

        self.df_output = pd.DataFrame(
            {
                "T2": 20 + diurnal + np.random.normal(0, 0.5, 168),
                "QH": 100 + 50 * diurnal / 5 + np.random.normal(0, 5, 168),
                "QE": 50 + np.random.normal(0, 3, 168),
                "QN": 200 + 100 * diurnal / 5,
                "QS": 30 + 10 * diurnal / 5,
                "QF": 20 + np.random.normal(0, 2, 168),
            },
            index=dates,
        )

        # Create forcing data with similar diurnal pattern
        self.df_forcing = pd.DataFrame(
            {
                "Tair": 15 + 0.8 * diurnal + np.random.normal(0, 0.2, 168),
                "RH": 60 - 10 * diurnal / 5 + np.random.normal(0, 2, 168),
                "pres": 1013.25 + np.random.normal(0, 2, 168),
            },
            index=dates,
        )

    def test_diagnose_t2_diurnal_method(self):
        """Diurnal method should compare afternoon vs morning."""
        result = diagnose_t2(self.df_output, self.df_forcing, method="diurnal")

        self.assertIsInstance(result, AttributionResult)
        self.assertEqual(result.metadata["method"], "diurnal")

    def test_diagnose_t2_anomaly_method(self):
        """Anomaly method should find extreme values."""
        result = diagnose_t2(
            self.df_output, self.df_forcing, method="anomaly", threshold=1.5
        )

        self.assertIsInstance(result, AttributionResult)
        self.assertEqual(result.metadata["method"], "anomaly")

    def test_diagnose_t2_insufficient_anomalies(self):
        """Should raise error when too few anomalies found."""
        # Create very uniform data
        dates = pd.date_range("2020-01-01", periods=100, freq="h")
        df_uniform = pd.DataFrame(
            {
                "T2": 20 + np.random.normal(0, 0.01, 100),  # Very low variance
                "QH": [100] * 100,
                "QE": [50] * 100,
                "QN": [200] * 100,
                "QS": [30] * 100,
                "QF": [20] * 100,
            },
            index=dates,
        )
        df_forcing_uniform = pd.DataFrame(
            {
                "Tair": [15] * 100,
                "RH": [60] * 100,
                "pres": [1013.25] * 100,
            },
            index=dates,
        )

        with self.assertRaises(ValueError) as context:
            diagnose_t2(df_uniform, df_forcing_uniform, method="anomaly", threshold=5.0)

        self.assertIn("anomalous timesteps", str(context.exception).lower())


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
