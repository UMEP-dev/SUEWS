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
import subprocess
import sys
import tempfile
from unittest import TestCase

import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy.dts import _DTS_AVAILABLE
from conftest import TIMESTEPS_PER_DAY

# Get the test data directory
test_data_dir = Path(__file__).parent.parent / "fixtures" / "data_test"
p_df_sample = Path(test_data_dir) / "sample_output.csv.gz"


def _rust_library_available() -> bool:
    """Return True when the Rust Python bridge exposes run_suews()."""
    try:
        from importlib import import_module

        module = import_module("supy.suews_bridge")
    except Exception:
        try:
            from importlib import import_module

            module = import_module("suews_bridge")
        except Exception:
            return False
    return hasattr(module, "run_suews")


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


@pytest.mark.smoke
class TestSampleOutput(TestCase):
    """Dedicated test class for validating SUEWS outputs against reference data."""

    def setUp(self):
        """Set up test environment."""
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
            "version": sp.__version__ if hasattr(sp, "__version__") else "unknown",
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

    @pytest.mark.core
    @pytest.mark.smoke
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
            : TIMESTEPS_PER_DAY * 366
        ]  # One year (2012 is a leap year)

        # Run simulation - full year to capture seasonal variations
        # This tests the model under diverse meteorological conditions
        print("Running SUEWS model...")
        df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)

        # Load reference output (CSV format for transparency and compatibility)
        print("Loading reference output...")
        df_sample = pd.read_csv(
            p_df_sample, compression="gzip", index_col=[0, 1], parse_dates=[1]
        )

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

    @pytest.mark.core
    @pytest.mark.smoke
    @pytest.mark.skipif(
        not _DTS_AVAILABLE,
        reason="DTS not available (fast build without type wrappers)",
    )
    def test_dts_vs_traditional_parity(self):
        """
        Test DTS interface produces identical output to traditional run_supy.

        This smoke test validates that the new DTS (Derived Type Structure)
        interface produces bit-identical output to the traditional run_supy
        method. This is critical for ensuring the DTS interface can be used
        as a drop-in replacement.

        Two modes are tested:
        1. Traditional: Uses run_supy() which goes through df_state packing
        2. DTS: Uses direct Pydantic->DTS->Fortran kernel path

        The test uses a short 48-timestep (4-hour) simulation for fast feedback.
        """
        print("\n" + "=" * 70)
        print("DTS vs Traditional Run Parity Test")
        print("=" * 70)

        # Print platform info
        platform_info = self.get_platform_info()
        print(f"Platform: {platform_info['platform']} {platform_info['machine']}")
        print(f"Python: {platform_info['python_version_tuple']}")
        print("=" * 70)

        # Import DTS infrastructure
        from supy import load_SampleData
        from supy.data_model import SUEWSConfig
        from supy.dts import run_dts

        # Load sample data
        print("\nLoading sample data...")
        df_state_init, df_forcing = load_SampleData()
        grid_id = df_state_init.index[0]

        # Use 48 timesteps (4 hours) for smoke test - enough to verify correctness
        n_timesteps = 48
        df_forcing_subset = df_forcing.head(n_timesteps)

        # =====================================================================
        # MODE 1: Traditional run_supy
        # =====================================================================
        print(f"\n[MODE 1] Running traditional run_supy ({n_timesteps} timesteps)...")
        df_output_trad, _ = sp.run_supy(df_forcing_subset, df_state_init)

        # =====================================================================
        # MODE 2: DTS interface (batch runner)
        # =====================================================================
        print(f"[MODE 2] Running DTS interface ({n_timesteps} timesteps)...")

        # Create Pydantic config
        config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])

        # Run DTS batch simulation
        df_output_dts, _ = run_dts(df_forcing_subset, config, site_index=0)

        # =====================================================================
        # COMPARISON
        # =====================================================================
        print("\n" + "=" * 70)
        print("COMPARING OUTPUTS")
        print("=" * 70)

        all_passed = True
        failed_variables = []
        variables_to_compare = ("QN", "QF", "QS", "QH", "QE")

        for var in variables_to_compare:
            dts_arr = df_output_dts[("SUEWS", var)].values
            trad_arr = df_output_trad[("SUEWS", var)].values

            # Check for exact match (DTS should be bit-identical)
            max_diff = np.max(np.abs(dts_arr - trad_arr))

            if max_diff == 0:
                print(f"[PASS] {var}: Identical (max diff = 0)")
            else:
                print(f"[FAIL] {var}: max diff = {max_diff:.6e}")
                failed_variables.append(var)
                all_passed = False

        # Summary
        print("\n" + "=" * 70)
        print("SUMMARY")
        print("=" * 70)

        if all_passed:
            print("[PASS] DTS interface produces identical output to traditional run!")
        else:
            print(f"[FAIL] Parity failed for: {', '.join(failed_variables)}")

        self.assertTrue(
            all_passed,
            f"DTS vs traditional parity failed for: {', '.join(failed_variables)}",
        )

    @pytest.mark.core
    @pytest.mark.rust
    @pytest.mark.skipif(
        not _rust_library_available(),
        reason="Rust library backend not available (install src/suews_bridge with physics feature)",
    )
    def test_rust_backend_via_simulation(self):
        """Validate SUEWSSimulation backend='rust' against reference output."""
        print("\n" + "=" * 70)
        print("Rust Backend (SUEWSSimulation) Validation Test")
        print("=" * 70)

        sim = sp.SUEWSSimulation.from_sample_data()
        output = sim.run(backend="rust")
        df_output = output.df

        self.assertEqual(
            len(df_output),
            105408,
            f"Unexpected rust backend row count: {len(df_output)}",
        )

        df_ref = pd.read_csv(
            p_df_sample, compression="gzip", index_col=[0, 1], parse_dates=[1]
        )

        variables_to_test = list(TOLERANCE_CONFIG.keys())
        all_passed = True
        failed_variables = []
        reports = []
        warmup_steps = TIMESTEPS_PER_DAY * 2

        for var in variables_to_test:
            col_key = ("SUEWS", var)
            if col_key not in df_output.columns:
                all_passed = False
                failed_variables.append(var)
                reports.append(f"[ERROR] Missing variable in rust output: {var}")
                continue
            if var not in df_ref.columns:
                all_passed = False
                failed_variables.append(var)
                reports.append(f"[ERROR] Missing variable in reference output: {var}")
                continue

            actual = df_output[col_key].values[warmup_steps:]
            expected = df_ref[var].values[warmup_steps:]
            tolerance = get_tolerance_for_variable(var)
            passed, report = compare_arrays_with_tolerance(
                actual,
                expected,
                rtol=tolerance["rtol"],
                atol=tolerance["atol"],
                var_name=var,
            )
            reports.append(report)
            if not passed:
                all_passed = False
                failed_variables.append(var)

        self.assertTrue(
            all_passed,
            "Rust backend validation failed for: "
            f"{', '.join(failed_variables)}\n"
            + "\n".join(reports),
        )

    @pytest.mark.core
    @pytest.mark.rust
    def test_rust_bridge_sample_output(self):
        """
        Test Rust CLI bridge produces output matching the sample reference.

        Runs the Rust binary on the sample YAML config and compares the 8 key
        output variables against sample_output.csv.gz using the same tolerance
        framework as test_sample_output_validation.

        Skipped if the Rust binary has not been built.
        """
        import pyarrow.ipc as ipc
        import yaml

        print("\n" + "=" * 70)
        print("Rust Bridge Sample Output Validation Test")
        print("=" * 70)

        # Locate the Rust binary
        repo_root = Path(__file__).parent.parent.parent
        rust_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / "suews"
        if not rust_binary.exists():
            pytest.skip(
                f"Rust binary not found at {rust_binary}; "
                "build with: cd src/suews_bridge && cargo build --release"
            )

        # Locate the sample YAML config and its directory
        sample_dir = Path(sp.__file__).parent / "sample_data"
        sample_config = sample_dir / "sample_config.yml"
        assert sample_config.exists(), f"Sample config not found: {sample_config}"

        # Load reference first to get SUEWS variable names
        print("Loading reference output...")
        df_ref = pd.read_csv(
            p_df_sample, compression="gzip", index_col=[0, 1], parse_dates=[1]
        )
        print(f"Reference: {df_ref.shape[0]} rows x {df_ref.shape[1]} columns")

        # Run the Rust CLI with a temporary config pointing output to tmpdir
        print(f"\nRunning Rust CLI: {rust_binary.name} run (Arrow output)")
        with tempfile.TemporaryDirectory() as tmpdir:
            # Copy config to tmpdir and modify output_file.path
            with open(sample_config) as f:
                cfg = yaml.safe_load(f)
            cfg["model"]["control"]["output_file"]["path"] = tmpdir
            tmp_config = Path(tmpdir) / "sample_config.yml"
            with open(tmp_config, "w") as f:
                yaml.dump(cfg, f, default_flow_style=False, sort_keys=False)

            # Symlink the forcing file so the CLI can find it
            forcing_name = cfg["model"]["control"]["forcing_file"]
            if isinstance(forcing_name, dict):
                forcing_name = forcing_name["value"]
            forcing_src = sample_dir / forcing_name
            forcing_dst = Path(tmpdir) / forcing_name
            if forcing_src.exists() and not forcing_dst.exists():
                os.symlink(forcing_src, forcing_dst)

            result = subprocess.run(
                [str(rust_binary), "run", str(tmp_config)],
                capture_output=True,
                text=True,
                timeout=120,
            )

            if result.returncode != 0:
                self.fail(
                    f"Rust CLI exited with code {result.returncode}\n"
                    f"stderr: {result.stderr[:500]}"
                )

            # Load Rust Arrow output (all 1134 columns across 11 groups)
            rust_output_path = Path(tmpdir) / "suews_output.arrow"
            assert rust_output_path.exists(), "Rust CLI did not produce suews_output.arrow"
            reader = ipc.open_file(rust_output_path)
            table = reader.read_all()
            df_rust_all = table.to_pandas()

        # SUEWS group uses proper variable names (Kdown, Kup, QN, etc.)
        # directly in the Arrow file — just use df_rust_all as-is
        df_rust = df_rust_all

        print(f"Rust output: {df_rust.shape[0]} rows x {df_rust.shape[1]} columns")

        # Verify row count alignment
        n_rust = len(df_rust)
        n_ref = len(df_ref)
        self.assertEqual(
            n_rust, n_ref,
            f"Row count mismatch: Rust={n_rust}, reference={n_ref}",
        )

        # Compare the 8 key variables (all timesteps)
        variables_to_test = list(TOLERANCE_CONFIG.keys())
        print(f"\nValidating variables: {', '.join(variables_to_test)}")
        print(f"Comparing all {n_rust} timesteps")
        print("=" * 70)

        all_passed = True
        failed_variables = []
        full_report = []

        for var in variables_to_test:
            if var not in df_rust.columns:
                report = f"\n[ERROR] Variable {var} not found in Rust output!"
                full_report.append(report)
                print(report)
                all_passed = False
                failed_variables.append(var)
                continue

            if var not in df_ref.columns:
                report = f"\n[ERROR] Variable {var} not found in reference!"
                full_report.append(report)
                print(report)
                all_passed = False
                failed_variables.append(var)
                continue

            rust_arr = df_rust[var].values
            ref_arr = df_ref[var].values

            tolerance = get_tolerance_for_variable(var)
            is_valid, report = compare_arrays_with_tolerance(
                rust_arr,
                ref_arr,
                rtol=tolerance["rtol"],
                atol=tolerance["atol"],
                var_name=var,
            )

            status = "[PASS]" if is_valid else "[FAIL]"
            print(f"{status} {report}")
            full_report.append(f"{status} {report}")

            if not is_valid:
                all_passed = False
                failed_variables.append(var)

        # Summary
        print("\n" + "=" * 70)
        print("SUMMARY")
        print("=" * 70)
        if all_passed:
            print("[PASS] Rust bridge output matches sample reference!")
        else:
            print(f"[FAIL] Validation failed for: {', '.join(failed_variables)}")

        self.assertTrue(
            all_passed,
            f"Rust bridge vs reference failed for: {', '.join(failed_variables)}\n"
            + "\n".join(full_report),
        )


if __name__ == "__main__":
    import unittest

    unittest.main()


# ============================================================================
# STEBBS VALIDATION TEST
# ============================================================================


class TestSTEBBSOutput(TestCase):
    """Test class for validating STEBBS building energy outputs."""

    def setUp(self):
        """Set up test environment."""
        # Check if running in CI
        self.in_ci = os.environ.get("CI", "").lower() == "true"
        self.artifact_dir = None

        if self.in_ci:
            # Create artifact directory
            runner_temp = os.environ.get("RUNNER_TEMP", tempfile.gettempdir())
            self.artifact_dir = Path(runner_temp) / "suews_test_artifacts"
            self.artifact_dir.mkdir(exist_ok=True, parents=True)

    def test_stebbs_building_energy_outputs(self):
        """
        Test STEBBS building energy model outputs.

        This test validates that the STEBBS (Simple Thermal Energy Balance
        Building Simulator) module produces correct building energy outputs
        including:
        - Indoor air temperature
        - Heating and cooling loads
        - Building surface temperatures
        - Radiation fluxes on building surfaces

        The test uses a short 2-day simulation with STEBBS enabled
        (storageheatmethod=7, stebbsmethod=1, output groups=['SUEWS', 'STEBBS']).
        """
        print("\n" + "=" * 70)
        print("STEBBS Building Energy Output Validation Test")
        print("=" * 70)

        # Print platform info
        platform_info = {
            "platform": platform.system(),
            "machine": platform.machine(),
            "python_version": sys.version_info[:3],
            "numpy_version": np.__version__,
            "pandas_version": pd.__version__,
        }
        print(f"Platform: {platform_info['platform']} {platform_info['machine']}")
        print(f"Python: {platform_info['python_version']}")
        print(f"NumPy: {platform_info['numpy_version']}")
        print("=" * 70)

        # Load STEBBS test configuration
        stebbs_test_dir = (
            Path(__file__).parent.parent / "fixtures" / "data_test" / "stebbs_test"
        )
        config_path = stebbs_test_dir / "sample_config.yml"
        reference_output_path = stebbs_test_dir / "sample_output_stebbs.csv"

        print(f"\nLoading STEBBS test configuration from: {config_path}")

        # Initialize and run simulation
        print("Initializing SUEWS with STEBBS...")
        df_state_init = sp.init_supy(str(config_path))

        print("Loading forcing data...")
        df_forcing_full = sp.load_forcing_grid(
            str(config_path), df_state_init.index[0], df_state_init=df_state_init
        )

        # Subset forcing data to match config period (2017-08-26 to 2017-08-27)
        df_forcing = df_forcing_full.loc["2017-08-26":"2017-08-27"]

        print(f"Running STEBBS simulation ({len(df_forcing)} timesteps, 2 days)...")
        df_output, df_state = sp.run_supy(df_forcing, df_state_init)

        # Load reference output
        print("Loading reference output...")
        df_reference = pd.read_csv(reference_output_path)

        # Define STEBBS-specific variables to test with tolerances
        # Higher tolerances for building energy due to complex thermal dynamics
        stebbs_variables = {
            # Indoor conditions - affected by complex heat transfer
            "Tair_ind": {"rtol": 0.02, "atol": 0.5},  # 2% / 0.5K tolerance
            # Building loads - higher tolerance due to control logic
            "QHload_heating_FA": {"rtol": 0.05, "atol": 5.0},  # 5% / 5W tolerance
            "QHload_cooling_FA": {"rtol": 0.05, "atol": 5.0},  # 5% / 5W tolerance
        }

        print(f"\nValidating STEBBS variables: {', '.join(stebbs_variables.keys())}")
        print("=" * 70)

        # Extract only 2017-08-27 data from simulation output to match reference
        # Reference contains data for 2017-08-27 00:00 to 23:55 (TIMESTEPS_PER_DAY timesteps)
        # Simulation output contains 2 days, so we take the second day
        # df_output has MultiIndex columns, so we slice the second day of data
        df_output_day2 = df_output.iloc[TIMESTEPS_PER_DAY : TIMESTEPS_PER_DAY * 2]

        print(f"\nFiltered output to match reference period (2017-08-27):")
        print(f"  Simulation output (2nd day) length: {len(df_output_day2)}")
        print(f"  Reference data length: {len(df_reference)}")

        # Compare each variable
        all_passed = True
        full_report = []
        failed_variables = []

        for var, tolerance in stebbs_variables.items():
            # Get data from output
            if var not in df_output_day2.STEBBS.columns:
                report = f"\n[ERROR] Variable {var} not found in STEBBS output!"
                full_report.append(report)
                print(report)
                all_passed = False
                failed_variables.append(var)
                continue

            if var not in df_reference.columns:
                report = f"\n[ERROR] Variable {var} not found in reference output!"
                full_report.append(report)
                print(report)
                all_passed = False
                failed_variables.append(var)
                continue

            actual = df_output_day2.STEBBS[var].values
            expected = df_reference[var].values

            # Handle length mismatch (should not occur after filtering)
            if len(actual) != len(expected):
                print(
                    f"\n[WARNING] Length mismatch for {var}: {len(actual)} vs {len(expected)}"
                )
                min_len = min(len(actual), len(expected))
                actual = actual[:min_len]
                expected = expected[:min_len]

            # Skip variables where reference is all NaN (e.g., cooling loads in winter)
            if np.all(np.isnan(expected)):
                report = f"\n[SKIP] {var}: Reference data is all NaN (not tested)"
                full_report.append(report)
                print(report)
                continue

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
            print("[PASS] All STEBBS variables passed validation!")
        else:
            print(
                f"[FAIL] Validation failed for {len(failed_variables)} variables: {', '.join(failed_variables)}"
            )

        # Assert at the end
        self.assertTrue(
            all_passed,
            f"STEBBS output validation failed for: {', '.join(failed_variables)}",
        )
