"""
Test supy post-processing functionality.

This module tests all post-processing functions in supy, including
output resampling, aggregation, and data manipulation utilities.
"""

from unittest import TestCase
import warnings

import pandas as pd

import supy as sp
from supy._post import dict_var_aggm, resample_output  # noqa: PLC2701


class TestResampleOutput(TestCase):
    """Test output resampling functionality."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)

        # Create sample output data for testing
        self.df_state_init, self.df_forcing = sp.load_SampleData()

        # Run a short simulation to get output
        df_forcing_short = self.df_forcing.iloc[: 288 * 7]  # One week
        self.df_output, self.df_state = sp.run_supy(
            df_forcing_short, self.df_state_init, check_input=False
        )

    def test_resample_output_hourly(self):
        """Test resampling output to hourly frequency."""
        print("\n========================================")
        print("Testing resample_output to hourly...")

        # Resample to hourly
        df_hourly = resample_output(self.df_output, freq="60T", dict_aggm=dict_var_aggm)

        # Validation
        self.assertIsInstance(df_hourly, pd.DataFrame)
        # Check frequency on datetime level of MultiIndex
        if isinstance(df_hourly.index, pd.MultiIndex):
            # Get the datetime level and check its frequency
            datetime_level = df_hourly.index.get_level_values("datetime")
            # Check the time difference between consecutive unique timestamps
            unique_times = datetime_level.unique()
            if len(unique_times) > 1:
                time_diff = (unique_times[1] - unique_times[0]).total_seconds() / 60
                self.assertEqual(time_diff, 60.0, "Hourly frequency not correct")
        else:
            self.assertEqual(df_hourly.index.freq.n, 60)  # 60 minutes

        # Check that we have fewer rows (aggregated)
        self.assertLess(len(df_hourly), len(self.df_output))

        # Check that columns are preserved
        original_groups = self.df_output.columns.get_level_values("group").unique()
        resampled_groups = df_hourly.columns.get_level_values("group").unique()
        for group in original_groups:
            if group != "DailyState":  # DailyState handled differently
                self.assertIn(group, resampled_groups)

        print(
            f"✓ Resampled from {len(self.df_output)} to {len(df_hourly)} hourly records"
        )

    def test_resample_output_daily(self):
        """Test resampling output to daily frequency."""
        print("\n========================================")
        print("Testing resample_output to daily...")

        # Resample to daily
        df_daily = resample_output(self.df_output, freq="D", dict_aggm=dict_var_aggm)

        # Validation
        self.assertIsInstance(df_daily, pd.DataFrame)
        self.assertEqual(len(df_daily), 7)  # 7 days

        # Check aggregation methods for grid 1
        # Note: df_output has MultiIndex (datetime, grid), so we need to handle it properly
        grid_id = self.df_output.index.get_level_values("grid")[0]
        df_grid = self.df_output.xs(grid_id, level="grid")
        df_daily_grid = df_daily.xs(grid_id, level="grid")

        # Energy fluxes should be averaged
        # Use same resampling parameters as resample_output (closed='right', label='right')
        qn_manual = (
            df_grid.SUEWS["QN"].resample("D", closed="right", label="right").mean()
        )
        qn_daily = df_daily_grid.SUEWS["QN"]
        pd.testing.assert_series_equal(qn_manual, qn_daily, check_names=False)

        # Rain should be summed
        rain_manual = (
            df_grid.SUEWS["Rain"].resample("D", closed="right", label="right").sum()
        )
        rain_daily = df_daily_grid.SUEWS["Rain"]
        pd.testing.assert_series_equal(rain_manual, rain_daily, check_names=False)

        print(f"✓ Resampled to {len(df_daily)} daily records with correct aggregation")

    def test_resample_output_custom_freq(self):
        """Test resampling with custom frequency."""
        print("\n========================================")
        print("Testing resample_output with custom frequency...")

        # Resample to 30 minutes
        df_30min = resample_output(self.df_output, freq="30T")

        # Should have twice as many records as hourly
        df_hourly = resample_output(self.df_output, freq="60T")
        self.assertAlmostEqual(len(df_30min), len(df_hourly) * 2, delta=2)

        print("✓ Custom frequency resampling works correctly")

    def test_resample_output_custom_aggm(self):
        """Test resampling with custom aggregation methods."""
        print("\n========================================")
        print("Testing resample_output with custom aggregation...")

        # Create custom aggregation dictionary - must be nested {group: {var: method}}
        custom_aggm = {
            "SUEWS": {
                "QN": "max",  # Maximum instead of mean
                "Rain": "mean",  # Mean instead of sum
            },
            "DailyState": {"HDD1_h": "last"},
        }

        # Resample with custom aggregation
        df_custom = resample_output(self.df_output, freq="D", dict_aggm=custom_aggm)

        # Verify custom aggregation for grid 1
        # Note: df_output has MultiIndex (datetime, grid), so we need to handle it properly
        grid_id = self.df_output.index.get_level_values("grid")[0]
        qn_series = self.df_output.xs(grid_id, level="grid").SUEWS["QN"]
        # Use same resampling parameters as resample_output (closed='right', label='right')
        qn_max = qn_series.resample("D", closed="right", label="right").max()
        qn_custom = df_custom.xs(grid_id, level="grid").SUEWS["QN"]
        pd.testing.assert_series_equal(qn_max, qn_custom, check_names=False)

        print("✓ Custom aggregation methods work correctly")

    def test_resample_preserves_multiindex(self):
        """Test that resampling preserves MultiIndex column structure."""
        print("\n========================================")
        print("Testing MultiIndex preservation in resampling...")

        # Resample
        df_resampled = resample_output(self.df_output, freq="60T")

        # Check MultiIndex structure
        self.assertIsInstance(df_resampled.columns, pd.MultiIndex)
        self.assertEqual(df_resampled.columns.nlevels, 2)
        self.assertIn("group", df_resampled.columns.names)
        self.assertIn("var", df_resampled.columns.names)

        print("✓ MultiIndex structure preserved correctly")


class TestAggregationMethods(TestCase):
    """Test aggregation method definitions."""

    def test_dict_var_aggm_completeness(self):
        """Test that aggregation dictionary covers common variables."""
        print("\n========================================")
        print("Testing aggregation method dictionary...")

        # dict_var_aggm is nested: {group: {var: method}}
        # Check essential variables have aggregation methods in SUEWS group
        # Note: Meteorological forcing variables (Tair, RH, Pres, U) are not in output
        essential_vars = [
            "QN",
            "QF",
            "QS",
            "QE",
            "QH",  # Energy fluxes
            "Rain",
            "Evap",
            "RO",  # Water fluxes
            "Kdown",
            "Kup",
            "Ldown",
            "Lup",  # Radiation
        ]

        # Check that SUEWS group exists
        self.assertIn(
            "SUEWS", dict_var_aggm, "Missing SUEWS group in aggregation dictionary"
        )

        suews_vars = dict_var_aggm["SUEWS"]

        for var in essential_vars:
            self.assertIn(var, suews_vars, f"Missing aggregation method for {var}")

        # Check aggregation methods are valid
        valid_methods = ["mean", "sum", "max", "min", "first", "last"]
        # Also accept lambda functions
        for _, var_dict in dict_var_aggm.items():
            for var, method in var_dict.items():
                if not callable(method):
                    self.assertIn(
                        method, valid_methods, f"Invalid method '{method}' for {var}"
                    )

        # Count total variables across all groups
        total_vars = sum(len(var_dict) for var_dict in dict_var_aggm.values())
        print(
            f"✓ Aggregation dictionary contains {total_vars} variables across {len(dict_var_aggm)} groups"
        )

    def test_aggregation_method_logic(self):
        """Test that aggregation methods make physical sense."""
        print("\n========================================")
        print("Testing aggregation method logic...")

        # dict_var_aggm is nested: {group: {var: method}}
        # Get SUEWS variables
        if "SUEWS" not in dict_var_aggm:
            self.skipTest("SUEWS group not in aggregation dictionary")

        suews_vars = dict_var_aggm["SUEWS"]

        # Cumulative variables should be summed
        cumulative_vars = [
            "Rain",
            "Irr",
            "Evap",
            "RO",
            "ROSoil",
            "ROPipe",
            "ROWater",
            "ROPav",
            "ROVeg",
        ]
        for var in cumulative_vars:
            if var in suews_vars:
                self.assertEqual(suews_vars[var], "sum", f"{var} should be summed")

        # Instantaneous variables should be averaged
        instant_vars = ["Tair", "RH", "Pres", "U", "QN", "QF", "QS", "QE", "QH"]
        for var in instant_vars:
            if var in suews_vars:
                self.assertEqual(suews_vars[var], "mean", f"{var} should be averaged")

        print("✓ Aggregation methods follow physical logic")


class TestPostProcessingUtilities(TestCase):
    """Test other post-processing utilities."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)

        # Run a minimal simulation
        df_state_init, df_forcing = sp.load_SampleData()
        df_forcing_short = df_forcing.iloc[: 288 * 2]  # Two days
        self.df_output, self.df_state = sp.run_supy(
            df_forcing_short, df_state_init, check_input=False
        )

    def test_output_groups(self):
        """Test output group structure."""
        print("\n========================================")
        print("Testing output group structure...")

        # Get unique groups
        groups = self.df_output.columns.get_level_values("group").unique()

        # Check essential groups exist
        essential_groups = ["SUEWS", "DailyState"]
        for group in essential_groups:
            self.assertIn(group, groups, f"Missing output group: {group}")

        # Access groups using attribute notation
        self.assertTrue(hasattr(self.df_output, "SUEWS"))
        self.assertIsInstance(self.df_output.SUEWS, pd.DataFrame)

        print(f"✓ Output contains {len(groups)} groups")

    def test_dailystate_extraction(self):
        """Test DailyState data extraction."""
        print("\n========================================")
        print("Testing DailyState extraction...")

        # Extract DailyState using xs()
        df_daily = self.df_output.xs("DailyState", level="group", axis=1)

        # Check that we have daily data
        mask_has_data = df_daily.notna().any(axis=1)
        n_days = mask_has_data.sum()

        # Should have approximately 2 days of data
        self.assertGreaterEqual(n_days, 1)
        self.assertLessEqual(n_days, 3)

        # Check some DailyState variables exist
        daily_vars = ["HDD1_h", "HDD2_c"]
        for var in daily_vars:
            if var in df_daily.columns:
                self.assertTrue(df_daily[var].notna().any())

        print(f"✓ Extracted {n_days} days of DailyState data")

    def test_energy_balance(self):
        """Test energy balance calculations from output."""
        print("\n========================================")
        print("Testing energy balance calculations...")

        # Calculate energy balance residual
        df_suews = self.df_output.SUEWS

        # Energy balance in SUEWS: QN + QF = QH + QE + QS
        # So the residual should be close to zero
        residual = (df_suews["QN"] + df_suews["QF"]) - (
            df_suews["QH"] + df_suews["QE"] + df_suews["QS"]
        )

        # Check that residual is small
        # Use absolute tolerance for near-zero values
        mean_abs_residual = residual.abs().mean()
        self.assertLess(
            mean_abs_residual,
            1.0,
            f"Energy balance residual too large: {mean_abs_residual:.3f} W/m²",
        )

        # Also check relative error when fluxes are significant
        total_flux = df_suews["QN"].abs() + df_suews["QF"].abs()
        mask_significant = total_flux > 10.0  # Only check when fluxes are significant
        if mask_significant.any():
            relative_error = (
                residual[mask_significant].abs() / total_flux[mask_significant]
            ).mean()
            self.assertLess(
                relative_error,
                0.01,
                f"Relative energy balance error too large: {relative_error * 100:.2f}%",
            )

        print(f"✓ Energy balance residual: {mean_abs_residual:.3f} W/m²")

    def test_water_balance(self):
        """Test water balance calculations from output."""
        print("\n========================================")
        print("Testing water balance calculations...")

        df_suews = self.df_output.SUEWS

        # Water balance: Rain + Irr = Evap + RO + ΔStorage
        water_in = df_suews["Rain"] + df_suews["Irr"]
        water_out = df_suews["Evap"] + df_suews["RO"]
        storage_change = df_suews["State"].diff()

        # Calculate balance (ignoring first timestep due to diff)
        balance = water_in[1:] - water_out[1:] - storage_change[1:]

        # Check balance is close to zero
        self.assertLess(balance.abs().mean(), 0.01, "Water balance error too large")

        print(f"✓ Water balance error: {balance.abs().mean():.6f} mm")


class TestMultiGridPostProcessing(TestCase):
    """Test post-processing for multi-grid simulations."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)

        # Create multi-grid simulation
        df_state_single, df_forcing = sp.load_SampleData()

        # Duplicate for 3 grids
        n_grids = 3
        df_state_multi = pd.concat([df_state_single for _ in range(n_grids)])
        df_state_multi.index = pd.RangeIndex(n_grids, name="grid")

        # Run short simulation
        df_forcing_short = df_forcing.iloc[:288]  # One day
        self.df_output, self.df_state = sp.run_supy(
            df_forcing_short, df_state_multi, check_input=False
        )
        self.n_grids = n_grids

    def test_multigrid_resample(self):
        """Test resampling multi-grid output."""
        print("\n========================================")
        print("Testing multi-grid resampling...")

        # Resample to hourly
        df_hourly = resample_output(self.df_output, freq="60T")

        # Check that grid structure is preserved
        self.assertEqual(df_hourly.index.nlevels, 2)  # datetime and grid
        self.assertIn("grid", df_hourly.index.names)

        # Check we have data for all grids
        grids = df_hourly.index.get_level_values("grid").unique()
        self.assertEqual(len(grids), self.n_grids)

        print(f"✓ Resampled {self.n_grids} grids correctly")

    def test_grid_aggregation(self):
        """Test aggregating across grids."""
        print("\n========================================")
        print("Testing grid aggregation...")

        # Get mean values across all grids
        df_suews = self.df_output.SUEWS

        # Group by time and calculate mean across grids
        df_mean = df_suews.groupby(level="datetime").mean()

        # Validate
        self.assertEqual(len(df_mean), 288)  # One day of 5-min data
        # Check that we have a simple index (not MultiIndex)
        self.assertEqual(df_mean.index.nlevels, 1)  # Single level index
        self.assertEqual(df_mean.index.name, "datetime")  # Index name is datetime

        # Check that mean is reasonable
        # When all grids are identical (as in our test), mean should equal the values
        # Since we created 3 identical grids, the mean should equal any single grid's values

        # Compare mean with first grid's values
        test_var = "QN"
        grid0_qn = df_suews.xs(0, level="grid")[test_var]

        # The mean across identical grids should equal the individual grid values
        pd.testing.assert_series_equal(
            df_mean[test_var],
            grid0_qn,
            check_names=False,
            check_freq=False,  # Don't check frequency metadata
            rtol=1e-10,
            atol=1e-10,
        )

        print("✓ Grid aggregation works correctly")


class TestErrorHandling(TestCase):
    """Test error handling in post-processing."""

    def test_invalid_frequency(self):
        """Test handling of invalid resampling frequency."""
        print("\n========================================")
        print("Testing error handling for invalid frequency...")

        # Create minimal output
        df_state, df_forcing = sp.load_SampleData()
        df_output, _ = sp.run_supy(df_forcing.iloc[:288], df_state)

        # Test invalid frequency
        with self.assertRaises((ValueError, KeyError)):
            resample_output(df_output, freq="invalid")

        print("✓ Error handling works correctly")

    def test_empty_dataframe(self):
        """Test handling of empty DataFrame."""
        print("\n========================================")
        print("Testing error handling for empty DataFrame...")

        # Create empty DataFrame with correct structure and at least one grid
        # The resample_output function expects to find at least one grid
        df_empty = pd.DataFrame(
            columns=pd.MultiIndex.from_tuples(
                [("SUEWS", "QN"), ("SUEWS", "QH")], names=["group", "var"]
            ),
            index=pd.MultiIndex.from_tuples([], names=["grid", "datetime"]),
        )

        # resample_output expects grids to exist, so it will raise an error
        # This is expected behavior - empty dataframes should not be resampled
        with self.assertRaises((ValueError, KeyError, IndexError)):
            resample_output(df_empty, freq="60T", dict_aggm=dict_var_aggm)

        print("✓ Empty DataFrame error handling works correctly")


if __name__ == "__main__":
    import unittest

    unittest.main()
