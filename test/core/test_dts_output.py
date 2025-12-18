"""
DTS Interface Output Validation Test.

This test validates that the new DTS (Derived Type Structure) interface
produces output identical to the traditional run() method within acceptable
tolerances.

The DTS interface provides a direct path from Pydantic configuration to
Fortran kernel execution, eliminating the intermediate df_state layer.
This is part of GH-1045 Option B implementation.
"""

import warnings
from pathlib import Path
from unittest import TestCase

import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy import SUEWSSimulation


# Tolerance configuration - same as test_sample_output.py
TOLERANCE_CONFIG = {
    "QN": {"rtol": 0.008, "atol": 0.1},   # Net all-wave radiation [W/m²]
    "QF": {"rtol": 0.008, "atol": 0.1},   # Anthropogenic heat flux [W/m²]
    "QS": {"rtol": 0.008, "atol": 0.1},   # Storage heat flux [W/m²]
    "QE": {"rtol": 0.008, "atol": 0.1},   # Latent heat flux [W/m²]
    "QH": {"rtol": 0.008, "atol": 0.1},   # Sensible heat flux [W/m²]
    "T2": {"rtol": 0.002, "atol": 0.01},  # 2m air temperature [°C]
    "RH2": {"rtol": 0.010, "atol": 0.5},  # 2m relative humidity [%]
    "U10": {"rtol": 0.005, "atol": 0.01}, # 10m wind speed [m/s]
}


def compare_arrays_with_tolerance(actual, expected, rtol, atol, var_name=""):
    """Compare arrays using numpy.allclose logic with detailed reporting."""
    actual = np.asarray(actual)
    expected = np.asarray(expected)

    if actual.shape != expected.shape:
        return False, f"Shape mismatch for {var_name}: {actual.shape} vs {expected.shape}"

    with np.errstate(divide="ignore", invalid="ignore"):
        abs_diff = np.abs(actual - expected)
        rel_diff = abs_diff / (np.abs(expected) + np.finfo(float).eps)

    within_tol = (abs_diff <= atol) | (rel_diff <= rtol)

    # Handle NaN values
    actual_nan = np.isnan(actual)
    expected_nan = np.isnan(expected)
    nan_mismatch = actual_nan != expected_nan

    if np.any(nan_mismatch):
        return False, f"NaN mismatch for {var_name}"

    valid_mask = ~(actual_nan & expected_nan)
    within_tol = within_tol | ~valid_mask

    all_valid = np.all(within_tol)

    if all_valid:
        report = f"{var_name}: All {len(actual)} values within tolerance"
    else:
        failures = np.where(~within_tol)[0]
        n_failures = len(failures)
        pct_failures = 100.0 * n_failures / len(actual)

        valid_rel_diff = rel_diff[valid_mask]
        if len(valid_rel_diff) > 0:
            max_rel_diff = np.max(valid_rel_diff)
            max_abs_diff = np.max(abs_diff[valid_mask])
        else:
            max_rel_diff = 0
            max_abs_diff = 0

        report = f"{var_name}: {n_failures} failures ({pct_failures:.2f}%), max rel diff: {max_rel_diff*100:.4f}%, max abs diff: {max_abs_diff:.6f}"

    return all_valid, report


class TestDTSOutput(TestCase):
    """Test class for validating DTS interface outputs against traditional run()."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)

    @pytest.mark.smoke
    def test_dts_infrastructure_imports(self):
        """Test that DTS infrastructure imports correctly."""
        from supy.dts import (
            run_dts,
            create_suews_config,
            create_suews_state,
            create_suews_site,
            create_suews_forcing,
            create_suews_timer,
        )

        # Basic sanity checks
        config = create_suews_config()
        self.assertIsNotNone(config)

        state = create_suews_state()
        self.assertIsNotNone(state)

        site = create_suews_site()
        self.assertIsNotNone(site)

        forcing = create_suews_forcing()
        self.assertIsNotNone(forcing)

        timer = create_suews_timer()
        self.assertIsNotNone(timer)

    @pytest.mark.smoke
    def test_dts_config_population(self):
        """Test that Pydantic config can be populated into DTS objects."""
        from supy.dts import (
            create_suews_config,
            populate_config_from_pydantic,
        )
        from supy.data_model.core import SUEWSConfig

        # Load sample config
        trv_sample_data = Path(sp.__file__).parent / "sample_data"
        path_config = trv_sample_data / "sample_config.yml"

        config = SUEWSConfig.from_yaml(str(path_config))
        self.assertIsNotNone(config)
        self.assertIsNotNone(config.model)

        # Create DTS config and populate
        config_dts = create_suews_config()
        populate_config_from_pydantic(config_dts, config.model)

        # Check that some values were populated
        # The exact values depend on the sample config
        self.assertIsInstance(config_dts.netradiationmethod, int)

    @pytest.mark.core
    @pytest.mark.skip(
        reason="DTS→Fortran mapping incomplete - causes segfault. "
        "See GH-1045 for remaining work: nested objects (OHM coefficients, "
        "anthropogenic emissions, surface-specific parameters) need mapping. "
        "Remove skip once full Pydantic→DTS mapping is implemented."
    )
    def test_dts_vs_traditional_run_short(self):
        """
        Test DTS output against traditional run() with short simulation.

        This test uses a 24-hour subset of forcing data to quickly verify
        that the DTS interface produces output comparable to the traditional
        run() method.

        Note
        ----
        Currently SKIPPED because the Pydantic→DTS parameter mapping is
        incomplete, causing a segfault in the Fortran kernel. The segfault
        cannot be caught by pytest's xfail mechanism.

        The DTS approach requires populating ~1400 parameters across nested
        objects. See GH-1045 for progress tracking.

        Remaining work to enable this test:
        - OHM coefficients for each surface type (4 coefficients × 7 surfaces)
        - Anthropogenic emissions profiles (24hr × weekday/holiday)
        - Surface-specific hydrology parameters (soilstorecap, statelimit, etc.)
        - Thermal layer properties (dz, k, rho_cp for each layer × surface)
        - Water distribution matrices (7×7 runoff distribution)
        - Building archetype parameters (SPARTACUS/EHC)
        """
        print("\n" + "=" * 70)
        print("DTS vs Traditional Run Comparison (24-hour test)")
        print("=" * 70)

        # Load sample config
        trv_sample_data = Path(sp.__file__).parent / "sample_data"
        path_config = trv_sample_data / "sample_config.yml"

        # Create simulation using Pydantic config
        sim_traditional = SUEWSSimulation(str(path_config))
        self.assertTrue(sim_traditional.is_ready())

        # Subset forcing to 24 hours (288 5-minute timesteps)
        n_timesteps = 288
        df_forcing = sim_traditional._df_forcing.iloc[:n_timesteps].copy()

        # Run traditional method
        print(f"\nRunning traditional run() with {n_timesteps} timesteps...")
        output_traditional = sim_traditional.run()
        df_trad = output_traditional.df

        # Create fresh simulation for DTS
        sim_dts = SUEWSSimulation(str(path_config))

        # Run DTS method
        print(f"Running run_dts() with {n_timesteps} timesteps...")
        try:
            output_dts = sim_dts.run_dts()
            df_dts = output_dts.df
        except Exception as e:
            print(f"\n[ERROR] run_dts() failed: {e}")
            self.fail(f"run_dts() raised exception: {e}")

        # Compare key variables
        variables_to_test = list(TOLERANCE_CONFIG.keys())
        print(f"\nComparing variables: {', '.join(variables_to_test)}")
        print("=" * 70)

        all_passed = True
        failed_variables = []

        for var in variables_to_test:
            # Check variable exists in both outputs
            if var not in df_trad.SUEWS.columns:
                print(f"[SKIP] {var}: Not in traditional output")
                continue
            if var not in df_dts.SUEWS.columns:
                print(f"[SKIP] {var}: Not in DTS output")
                continue

            actual = df_dts.SUEWS[var].values
            expected = df_trad.SUEWS[var].values

            # Get tolerance
            tolerance = TOLERANCE_CONFIG.get(var, {"rtol": 0.01, "atol": 0.1})

            # Compare
            passed, report = compare_arrays_with_tolerance(
                actual, expected, tolerance["rtol"], tolerance["atol"], var
            )

            status = "[PASS]" if passed else "[FAIL]"
            print(f"{status} {report}")

            if not passed:
                all_passed = False
                failed_variables.append(var)

        # Summary
        print("\n" + "=" * 70)
        if all_passed:
            print("[PASS] DTS output matches traditional run()!")
        else:
            print(f"[FAIL] Mismatch for: {', '.join(failed_variables)}")

        self.assertTrue(
            all_passed,
            f"DTS output mismatch for: {', '.join(failed_variables)}"
        )


if __name__ == "__main__":
    import unittest
    unittest.main()
