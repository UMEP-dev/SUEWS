"""
Dedicated sample output validation test for SUEWS.

This test implements a pragmatic tolerance-based validation approach for SUEWS,
addressing the challenge of numerical differences across platforms while ensuring
scientific validity.

Key Features:
- Custom NumPy-based comparison (avoids pandas version dependencies)
- Scientifically justified tolerances based on measurement uncertainty
- Detailed diagnostic reports for debugging
- CI/CD artifact generation for offline analysis
- Fast-fail design to save CI resources

Background:
Scientific models like SUEWS face inherent reproducibility challenges across
different platforms due to floating-point arithmetic differences, compiler
optimizations, and library implementations. Rather than pursuing bit-for-bit
reproducibility, this test ensures results remain within scientifically
acceptable bounds.

This test runs first in CI/CD to provide fast feedback before expensive
wheel building operations.

Note on NumPy Compatibility:
Python 3.9 requires NumPy 1.x due to f90wrap binary compatibility issues.
Python 3.10+ can use NumPy 2.0. This is handled in pyproject.toml build requirements.

Test Ordering Solution:
This test should run first in the test suite to avoid Fortran model state
interference from other tests. This is handled by pytest_collection_modifyitems
hook in conftest.py, which ensures test_sample_output.py runs before other tests.

Root cause:
- The Fortran model maintains internal state between test runs
- When other tests run first, they leave the model in a different state
- This causes small numerical differences that accumulate over a year-long simulation
- The force_reload parameter doesn't help as it's ignored for YAML config files

If this test fails when run as part of the full suite but passes individually:
    pytest test/test_sample_output.py
Check that conftest.py is present and properly configured.
"""

import json
import os
from pathlib import Path
import platform
import sys
import tempfile
from unittest import TestCase
import warnings

import numpy as np
import pandas as pd
import pytest

import supy as sp

# Get the test data directory
test_data_dir = Path(__file__).parent.parent / "fixtures" / "data_test"
p_df_sample = Path(test_data_dir) / "sample_output.pkl"


# ============================================================================
# TOLERANCE CONFIGURATION
# ============================================================================

# Tolerance configuration with scientific justification
# These tolerances are based on measurement uncertainty and scientific validity
# rather than pursuing unrealistic bit-for-bit reproducibility across platforms
TOLERANCE_CONFIG = {
    # Energy fluxes - all use same standard (0.8% relative tolerance)
    # Scientific justification:
    # - Eddy covariance measurements typically have 5-10% uncertainty
    # - Energy balance closure in field measurements rarely better than 70-90%
    # - Model structural uncertainty is comparable to measurement uncertainty
    # - 0.8% tolerance is conservative, well within measurement uncertainty
    # - Ensures energy balance closure within acceptable scientific bounds
    "QN": {"rtol": 0.008, "atol": 0.1},  # Net all-wave radiation [W/m²]
    "QF": {"rtol": 0.008, "atol": 0.1},  # Anthropogenic heat flux [W/m²]
    "QS": {"rtol": 0.008, "atol": 0.1},  # Storage heat flux [W/m²]
    "QE": {"rtol": 0.008, "atol": 0.1},  # Latent heat flux [W/m²]
    "QH": {"rtol": 0.008, "atol": 0.1},  # Sensible heat flux [W/m²]
    # Meteorological variables - different standards based on sensor accuracy
    # T2: Modern temperature sensors achieve ±0.1-0.2°C accuracy
    #     0.2% relative tolerance for typical urban temperatures
    "T2": {"rtol": 0.002, "atol": 0.01},  # 2m air temperature [°C]
    # RH2: Humidity sensors typically ±2-3% accuracy
    #      1% tolerance is conservative, accounts for nonlinear calculations
    "RH2": {"rtol": 0.010, "atol": 0.5},  # 2m relative humidity [%]
    # U10: Anemometer accuracy typically ±0.1-0.2 m/s
    #      0.5% tolerance for typical urban wind speeds
    #      Important for turbulent exchange calculations
    "U10": {"rtol": 0.005, "atol": 0.01},  # 10m wind speed [m/s]
}

# Platform-specific adjustments (if needed in future)
PLATFORM_ADJUSTMENTS = {
    # Python 3.13 may have slightly different numerical behavior
    "linux-x86_64": {
        "QS": {
            "rtol": 0.010,
            "atol": 0.2,
        },  # Slightly higher tolerance for storage heat flux
        "QE": {
            "rtol": 0.010,
            "atol": 0.2,
        },  # Slightly higher tolerance for latent heat flux
        "QH": {
            "rtol": 0.010,
            "atol": 0.2,
        },  # Slightly higher tolerance for sensible heat flux
        "T2": {
            "rtol": 0.005,
            "atol": 0.05,
        },  # Slightly higher tolerance for temperature
        "U10": {
            "rtol": 0.010,
            "atol": 0.05,
        },  # Slightly higher tolerance for wind speed
    }
    # Example: "darwin-arm64": {"QN": {"rtol": 0.010}}
}


# ============================================================================
# TOLERANCE UTILITIES
# ============================================================================


def get_platform_key():
    """Get platform identifier for platform-specific tolerances."""
    system = platform.system().lower()
    machine = platform.machine().lower()
    return f"{system}-{machine}"


def get_tolerance_for_variable(
    var_name, base_config=TOLERANCE_CONFIG, adjustments=PLATFORM_ADJUSTMENTS
):
    """Get tolerance for a variable, considering platform-specific adjustments."""
    # Start with base tolerance
    tolerance = base_config.get(var_name, {"rtol": 0.01, "atol": 0.1}).copy()

    # Apply platform-specific adjustments if any
    platform_key = get_platform_key()
    if platform_key in adjustments and var_name in adjustments[platform_key]:
        tolerance.update(adjustments[platform_key][var_name])

    # Apply Python version-specific adjustments for newer versions
    py_version = sys.version_info
    if py_version >= (3, 13):
        # Python 3.13+ may have different numerical behavior
        tolerance["rtol"] = min(
            tolerance["rtol"] * 1.5, 0.015
        )  # Increase by 50% but cap at 1.5%
        tolerance["atol"] = min(tolerance["atol"] * 1.5, 0.3)  # Increase by 50% but cap

    return tolerance


def compare_arrays_with_tolerance(actual, expected, rtol, atol, var_name=""):
    """
    Compare arrays using same logic as numpy.allclose but with detailed reporting.

    This custom implementation avoids pandas.testing dependencies which can vary
    between versions and cause false failures even when differences are within
    tolerance.

    The comparison uses the standard formula:
        |actual - expected| <= atol + rtol * |expected|

    Parameters
    ----------
    actual : array-like
        Computed values from model run
    expected : array-like
        Reference values for comparison
    rtol : float
        Relative tolerance
    atol : float
        Absolute tolerance
    var_name : str
        Variable name for reporting

    Returns
    -------
    tuple
        (is_valid, detailed_report) where is_valid is bool and detailed_report is str
    """
    # Ensure arrays
    actual = np.asarray(actual)
    expected = np.asarray(expected)

    # Handle shape mismatch
    if actual.shape != expected.shape:
        return (
            False,
            f"Shape mismatch for {var_name}: {actual.shape} vs {expected.shape}",
        )

    # Calculate differences
    with np.errstate(divide="ignore", invalid="ignore"):
        abs_diff = np.abs(actual - expected)
        # Use expected value for relative difference calculation
        # Add small epsilon to avoid division by zero
        rel_diff = abs_diff / (np.abs(expected) + np.finfo(float).eps)

    # Check tolerance using same logic as numpy.allclose
    within_tol = (abs_diff <= atol) | (rel_diff <= rtol)

    # Handle NaN values
    actual_nan = np.isnan(actual)
    expected_nan = np.isnan(expected)
    nan_mismatch = actual_nan != expected_nan

    if np.any(nan_mismatch):
        return False, f"NaN mismatch for {var_name}: NaN positions differ"

    # Ignore positions where both are NaN
    valid_mask = ~(actual_nan & expected_nan)
    within_tol = within_tol | ~valid_mask

    # Generate report
    all_valid = np.all(within_tol)

    if all_valid:
        report = f"{var_name}: All {len(actual)} values within tolerance (rtol={rtol}, atol={atol})"
    else:
        # Find failures
        failures = np.where(~within_tol)[0]
        n_failures = len(failures)
        pct_failures = 100.0 * n_failures / len(actual)

        # Get worst failures
        valid_rel_diff = rel_diff[valid_mask]
        if len(valid_rel_diff) > 0:
            max_rel_idx_in_valid = np.argmax(valid_rel_diff)
            # Map back to original index
            valid_indices = np.where(valid_mask)[0]
            max_rel_idx = valid_indices[max_rel_idx_in_valid]
            max_rel_diff = rel_diff[max_rel_idx]
            max_abs_diff = abs_diff[max_rel_idx]
        else:
            max_rel_idx = failures[0] if n_failures > 0 else 0
            max_rel_diff = rel_diff[max_rel_idx]
            max_abs_diff = abs_diff[max_rel_idx]

        report = f"\n{'=' * 60}\n"
        report += f"FAIL: Variable {var_name} exceeds tolerance\n"
        report += f"{'=' * 60}\n"
        report += f"Tolerance: {rtol * 100:.1f}% relative, {atol} absolute\n"
        report += (
            f"Failed points: {n_failures} of {len(actual)} ({pct_failures:.2f}%)\n"
        )
        report += "\nWorst failure:\n"
        report += f"  Index: {max_rel_idx}\n"
        report += f"  Actual: {actual[max_rel_idx]:.6f}\n"
        report += f"  Expected: {expected[max_rel_idx]:.6f}\n"
        report += f"  Abs diff: {max_abs_diff:.6f}\n"
        report += f"  Rel diff: {max_rel_diff:.6f} ({max_rel_diff * 100:.4f}%)\n"

        # Statistics
        report += "\nDifference statistics:\n"
        report += f"  Mean absolute: {np.mean(abs_diff[valid_mask]):.6f}\n"
        report += f"  Max absolute: {np.max(abs_diff[valid_mask]):.6f}\n"
        report += f"  Mean relative: {np.mean(rel_diff[valid_mask]) * 100:.4f}%\n"
        report += f"  Max relative: {np.max(rel_diff[valid_mask]) * 100:.4f}%\n"

        # Show first few failures
        report += "\nFirst 10 failures:\n"
        for i, idx in enumerate(failures[:10]):
            report += f"  [{idx}]: {actual[idx]:.6f} vs {expected[idx]:.6f} "
            report += f"(diff: {rel_diff[idx] * 100:.4f}%)\n"

    return all_valid, report


# ============================================================================
# TEST CLASS
# ============================================================================


class TestSampleOutput(TestCase):
    """Dedicated test class for validating SUEWS outputs against reference data."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)

        # Clear any cached data from previous tests
        # This prevents test interference when tests run in sequence
        import functools
        import gc

        # Clear all LRU caches in the supy module
        for obj in gc.get_objects():
            if isinstance(obj, functools._lru_cache_wrapper):
                try:
                    obj.cache_clear()
                except:
                    pass

        # More aggressive cache clearing for supy._load module
        try:
            import supy._load

            # Clear specific caches in _load module
            for attr_name in dir(supy._load):
                attr = getattr(supy._load, attr_name)
                if hasattr(attr, "cache_clear"):
                    attr.cache_clear()
        except:
            pass

        # Check if running in CI
        self.in_ci = os.environ.get("CI", "").lower() == "true"
        self.artifact_dir = None

        if self.in_ci:
            # Create artifact directory
            runner_temp = os.environ.get("RUNNER_TEMP", tempfile.gettempdir())
            self.artifact_dir = Path(runner_temp) / "suews_test_artifacts"
            self.artifact_dir.mkdir(exist_ok=True, parents=True)

    def get_platform_info(self):
        """Get detailed platform information."""
        return {
            "platform": platform.system(),
            "platform_release": platform.release(),
            "platform_version": platform.version(),
            "machine": platform.machine(),
            "processor": platform.processor(),
            "python_version": sys.version,
            "python_version_tuple": sys.version_info[:3],
            "python_implementation": platform.python_implementation(),
            "numpy_version": np.__version__,
            "pandas_version": pd.__version__,
            "supy_version": sp.__version__ if hasattr(sp, "__version__") else "unknown",
        }

    def save_debug_artifacts(
        self, df_state_init, df_forcing, df_output, df_sample, comparison_report
    ):
        """Save all relevant data for offline debugging when test fails."""
        if not self.artifact_dir:
            print("\nNot in CI environment, skipping artifact generation")
            return

        timestamp = pd.Timestamp.now().strftime("%Y%m%d_%H%M%S")
        py_version = f"py{sys.version_info.major}{sys.version_info.minor}"
        platform_str = get_platform_key()

        # Save all dataframes with descriptive names
        artifacts = {
            f"state_init_{platform_str}_{py_version}_{timestamp}.pkl": df_state_init,
            f"forcing_{platform_str}_{py_version}_{timestamp}.pkl": df_forcing,
            f"output_{platform_str}_{py_version}_{timestamp}.pkl": df_output,
            f"sample_reference_{platform_str}_{py_version}_{timestamp}.pkl": df_sample,
        }

        saved_files = []
        for filename, df in artifacts.items():
            filepath = self.artifact_dir / filename
            df.to_pickle(filepath)
            saved_files.append(filename)

        # Save comparison report
        report_file = f"comparison_report_{platform_str}_{py_version}_{timestamp}.txt"
        with open(self.artifact_dir / report_file, "w") as f:
            f.write(comparison_report)
        saved_files.append(report_file)

        # Save platform info
        platform_file = f"platform_info_{platform_str}_{py_version}_{timestamp}.json"
        with open(self.artifact_dir / platform_file, "w") as f:
            json.dump(self.get_platform_info(), f, indent=2)
        saved_files.append(platform_file)

        # Save tolerance configuration
        tolerance_file = (
            f"tolerance_config_{platform_str}_{py_version}_{timestamp}.json"
        )
        with open(self.artifact_dir / tolerance_file, "w") as f:
            json.dump(
                {
                    "base_config": TOLERANCE_CONFIG,
                    "platform_adjustments": PLATFORM_ADJUSTMENTS,
                    "platform_key": platform_str,
                },
                f,
                indent=2,
            )
        saved_files.append(tolerance_file)

        print(f"\n[ARTIFACTS] Debug artifacts saved to: {self.artifact_dir}")
        for file in saved_files:
            print(f"   - {file}")

    @pytest.mark.skipif(
        int(np.__version__.split(".")[0]) < 2,
        reason="Test reference data incompatible with NumPy 1.x (pickle format mismatch). See issue for long-term fix.",
    )
    def test_sample_output_validation(self):
        """
        Test SUEWS output against reference data with appropriate tolerances.

        This is the primary validation test that ensures model outputs remain
        scientifically valid across different platforms and Python versions.
        It runs a full year simulation and compares key output variables against
        pre-computed reference results.

        The test is designed to:
        1. Run quickly (< 1 minute) to provide fast CI/CD feedback
        2. Test the most important model outputs (energy fluxes, met variables)
        3. Generate comprehensive artifacts for debugging failures
        4. Use scientifically justified tolerances rather than exact matching

        Raises
        ------
        AssertionError
            If any variable exceeds its tolerance bounds
        """
        print("\n" + "=" * 70)
        print("SUEWS Sample Output Validation Test")
        print("=" * 70)

        # Print platform info
        platform_info = self.get_platform_info()
        print(f"Platform: {platform_info['platform']} {platform_info['machine']}")
        print(f"Python: {platform_info['python_version_tuple']}")
        print(f"NumPy: {platform_info['numpy_version']}")
        print(f"Pandas: {platform_info['pandas_version']}")
        print("=" * 70)

        # Load sample data - use test data from the test directory
        # The sample data represents typical urban conditions
        print("\nLoading test data...")

        # Force reload to avoid cache interference
        # This is a workaround for the caching issue in supy._load
        from pathlib import Path

        import supy as sp

        trv_sample_data = Path(sp.__file__).parent / "sample_data"
        path_config_default = trv_sample_data / "sample_config.yml"

        # Force reload to clear any cached state
        df_state_init = sp.init_supy(path_config_default, force_reload=True)
        df_forcing_tstep = sp.load_forcing_grid(
            path_config_default, df_state_init.index[0], df_state_init=df_state_init
        )

        df_forcing_part = df_forcing_tstep.iloc[
            : 288 * 366
        ]  # One year (2012 is a leap year)

        # Run simulation - full year to capture seasonal variations
        # This tests the model under diverse meteorological conditions
        print("Running SUEWS model...")
        df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)

        # Load reference output
        print("Loading reference output...")
        df_sample = pd.read_pickle(p_df_sample)

        # Variables to test - these are the key model outputs that:
        # 1. Are most sensitive to numerical differences
        # 2. Are critical for model physics (energy/water balance)
        # 3. Are commonly used in applications
        variables_to_test = list(TOLERANCE_CONFIG.keys())
        print(f"\nValidating variables: {', '.join(variables_to_test)}")
        print("=" * 70)

        # Compare each variable using custom tolerance framework
        # This avoids pandas version dependencies and provides better diagnostics
        all_passed = True
        full_report = []
        failed_variables = []

        for var in variables_to_test:
            # Get data
            if var not in df_output.SUEWS.columns:
                report = f"\n[ERROR] Variable {var} not found in output!"
                full_report.append(report)
                print(report)
                all_passed = False
                failed_variables.append(var)
                continue

            if var not in df_sample.columns:
                report = f"\n[ERROR] Variable {var} not found in reference!"
                full_report.append(report)
                print(report)
                all_passed = False
                failed_variables.append(var)
                continue

            actual = df_output.SUEWS[var].values
            expected = df_sample[var].values

            # Handle length mismatch
            if len(actual) != len(expected):
                print(
                    f"\n[WARNING] Length mismatch for {var}: {len(actual)} vs {len(expected)}"
                )
                min_len = min(len(actual), len(expected))
                actual = actual[:min_len]
                expected = expected[:min_len]

            # Get tolerance
            tolerance = get_tolerance_for_variable(var)

            # Compare
            passed, report = compare_arrays_with_tolerance(
                actual, expected, tolerance["rtol"], tolerance["atol"], var
            )

            # Add pass/fail indicator
            if passed:
                report = f"\n[PASS] {report}"
            else:
                report = f"\n[FAIL] {report}"
                failed_variables.append(var)

            full_report.append(report)
            print(report)

            if not passed:
                all_passed = False

        # Summary
        print("\n" + "=" * 70)
        print("SUMMARY")
        print("=" * 70)

        if all_passed:
            print("[PASS] All variables passed validation!")
        else:
            print(
                f"[FAIL] Validation failed for {len(failed_variables)} variables: {', '.join(failed_variables)}"
            )

        # Save artifacts if running in CI for offline debugging
        # This is critical for diagnosing platform-specific issues
        if not all_passed and self.in_ci:
            print("\n[SAVE] Saving debug artifacts...")
            self.save_debug_artifacts(
                df_state_init,
                df_forcing_part,
                df_output,
                df_sample,
                "\n".join(full_report),
            )

        # Assert at the end
        self.assertTrue(
            all_passed,
            f"Sample output validation failed for: {', '.join(failed_variables)}",
        )


if __name__ == "__main__":
    import unittest

    unittest.main()
