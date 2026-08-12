"""
Dedicated sample output validation test for SUEWS.

This test implements a pragmatic tolerance-based validation approach for SUEWS,
addressing the challenge of numerical differences across platforms while ensuring
scientific validity.

Key Features:
- Custom NumPy-based comparison (avoids pandas version dependencies)
- Scientifically justified tolerances based on measurement uncertainty
- Detailed diagnostic reports for debugging
- Fast-fail design to save CI resources

Background:
Scientific models like SUEWS face inherent reproducibility challenges across
different platforms due to floating-point arithmetic differences, compiler
optimizations, and library implementations. Rather than pursuing bit-for-bit
reproducibility, this test ensures results remain within scientifically
acceptable bounds.

This test is independent of collection order and can run alongside other
physics validation.
"""

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
import yaml

import supy as sp

from conftest import TIMESTEPS_PER_DAY

pytestmark = pytest.mark.physics

# Get the test data directory
test_data_dir = Path(__file__).parent.parent / "fixtures" / "data_test"
# The reference output is stored as twelve monthly plain-CSV shards;
# load_sample_output reconstructs the full-year frame. See the split/combine
# convention in fixtures/data_test/sample_output_io.py.
sys.path.insert(0, str(test_data_dir))
from sample_output_io import load_sample_output  # noqa: E402
FAIL_FAST_STEPS_ENV = "SUEWS_FAIL_FAST_STEPS"
# Default the smoke path to one model day. Set SUEWS_FAIL_FAST_STEPS to a larger
# value, or to 0 for the whole window, when an exhaustive local comparison is
# needed. Coverage of accumulated-state and day-of-year-gated behaviour does not
# depend on this switch: it lives in test_sample_output_validation_full_year,
# which ignores it entirely.
DEFAULT_FAIL_FAST_STEPS = TIMESTEPS_PER_DAY


def _resolve_fail_fast_steps(
    total_steps: int, default_steps: int = DEFAULT_FAIL_FAST_STEPS
) -> int:
    """Return how many timesteps to validate, resolved against what is available.

    Non-positive means the whole window, matching test_soil_obs_conversion.py, the
    other reader of SUEWS_FAIL_FAST_STEPS.

    Resolution lives here rather than at the call sites deliberately. This module
    has two callers, and an unresolved sentinel silently collapses a validation
    horizon to zero timesteps, which passes vacuously rather than failing. That is
    the same failure mode this module's full-year test exists to prevent.
    """
    raw = os.environ.get(FAIL_FAST_STEPS_ENV)
    if not raw:
        requested = default_steps
    else:
        try:
            requested = int(raw)
        except ValueError as exc:
            raise ValueError(
                f"{FAIL_FAST_STEPS_ENV} must be an integer, got: {raw!r}"
            ) from exc

    if requested <= 0:
        return total_steps
    return min(requested, total_steps)


def _write_forcing_prefix(source: Path, destination: Path, data_rows: int) -> None:
    """Write the forcing header plus the requested number of data rows."""
    lines = [
        line for line in source.read_text(encoding="utf-8").splitlines() if line.strip()
    ]
    if not lines:
        raise ValueError(f"Forcing file is empty: {source}")

    rows_to_write = [lines[0], *lines[1 : data_rows + 1]]
    destination.write_text("\n".join(rows_to_write) + "\n", encoding="utf-8")


def _locate_engine() -> Path:
    """Return the Rust CLI binary, skipping the test if it has not been built.

    Prefers the development build; falls back to the copy bundled inside the
    installed package, which is what CI and cibuildwheel see.
    """
    repo_root = Path(__file__).parent.parent.parent
    dev_binary = (
        repo_root / "src" / "suews_bridge" / "target" / "release" / "suews-engine"
    )
    if dev_binary.exists():
        return dev_binary

    try:
        from supy.cmd.rust_bridge import _bridge_binary

        return _bridge_binary()
    except (ImportError, FileNotFoundError):
        pytest.skip(
            "Rust CLI binary not found; "
            "build with: cd src/suews_bridge && cargo build --release"
        )


def _forcing_filename(control: dict) -> str:
    """Return the forcing file name from a config's model.control block.

    Accepts the current `forcing.file` shape (gh#1372) and the legacy
    `forcing_file` shape, each of which may wrap the value in a RefValue dict,
    so this test works against pre- and post-migration sample configs.
    """
    name = None
    if isinstance(control.get("forcing"), dict):
        name = control["forcing"].get("file")
    if name is None:
        name = control.get("forcing_file")
    if isinstance(name, dict):
        name = name["value"]
    if not name:
        raise AssertionError("No forcing file declared in model.control")
    return name


def _forcing_rows_for(validation_steps: int, tstep: int, truncated: bool) -> int:
    """Return how many forcing rows cover the requested number of timesteps.

    One extra row is included when the window is truncated, so interpolation at
    the final checked timestep is not a boundary effect.
    """
    steps_per_row = max(1, 3600 // tstep)
    rows = max(2, (validation_steps + steps_per_row - 1) // steps_per_row)
    return rows + 1 if truncated else rows


def _write_run_inputs(
    sample_dir, sample_config, run_dir: Path, validation_steps: int, truncated: bool
) -> tuple[Path, int]:
    """Write a config and forcing prefix into run_dir; return the config and row count.

    The config is redirected to write its output into run_dir. The forcing file is
    truncated to the rows the requested window needs, because the engine runs the
    whole forcing file it is given.
    """
    with open(sample_config, encoding="utf-8") as handle:
        cfg = yaml.safe_load(handle)

    control = cfg["model"]["control"]
    tstep = int(control.get("tstep", 300))
    control["output"]["dir"] = str(run_dir)

    config_path = run_dir / "sample_config.yml"
    with open(config_path, "w", encoding="utf-8") as handle:
        yaml.dump(cfg, handle, default_flow_style=False, sort_keys=False)

    forcing_name = _forcing_filename(control)
    forcing_rows = _forcing_rows_for(validation_steps, tstep, truncated)
    _write_forcing_prefix(
        sample_dir / forcing_name, run_dir / forcing_name, forcing_rows
    )
    return config_path, forcing_rows


def _run_engine(binary: Path, config_path: Path, run_dir: Path, timeout: int) -> bytes:
    """Run the engine and return the Arrow output as bytes.

    Read to bytes rather than handing back a path: pyarrow keeps the file open,
    which blocks TemporaryDirectory cleanup on Windows.
    """
    result = subprocess.run(
        [str(binary), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=timeout,
    )
    if result.returncode != 0:
        raise AssertionError(
            f"Engine exited with code {result.returncode}\n"
            f"stderr: {result.stderr[:500]}"
        )

    output_path = run_dir / "suews_output.arrow"
    if not output_path.exists():
        raise AssertionError("Engine did not produce suews_output.arrow")
    return output_path.read_bytes()


def _read_engine_output(arrow_bytes: bytes, columns) -> pd.DataFrame:
    """Return the requested columns of the Arrow output as a DataFrame.

    Projects before converting to pandas. A full year is ~1.1 GB across 1350
    columns; converting all of them and copying the slice peaked at 3.3 GB to
    compare nine, against 1.2 GB when projected first.
    """
    import pyarrow.ipc as ipc

    table = ipc.open_file(arrow_bytes).read_all()
    present = [name for name in columns if name in table.schema.names]
    return table.select(present).to_pandas()


def _compare_frames(df_actual, df_expected, variables) -> tuple[bool, list, list]:
    """Compare each variable within its tolerance.

    Returns (all_passed, failed_variables, report_lines). A variable missing from
    either frame counts as a failure rather than being skipped, so a column that
    silently disappears from the engine output cannot weaken the test.
    """
    failed: list = []
    report: list = []

    for var in variables:
        for frame, label in ((df_actual, "engine output"), (df_expected, "reference")):
            if var not in frame.columns:
                line = f"\n[ERROR] Variable {var} not found in {label}!"
                report.append(line)
                print(line)
                failed.append(var)
                break
        else:
            tolerance = get_tolerance_for_variable(var)
            is_valid, detail = compare_arrays_with_tolerance(
                df_actual[var].values,
                df_expected[var].values,
                rtol=tolerance["rtol"],
                atol=tolerance["atol"],
                var_name=var,
            )
            status = "[PASS]" if is_valid else "[FAIL]"
            print(f"{status} {detail}")
            report.append(f"{status} {detail}")
            if not is_valid:
                failed.append(var)

    return not failed, failed, report


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
    # LAI: the phenology state itself, not a flux. Compared directly because it
    # is the direct output of the GDD/SDD scheme; relying on it only through its
    # effect on QE and QN means a phenology regression has to be large enough to
    # move an energy flux by 0.8% before any test notices.
    "LAI": {"rtol": 0.008, "atol": 0.001},  # bulk leaf area index [m2 m-2]
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

    @pytest.mark.core
    @pytest.mark.rust
    @pytest.mark.skipif(
        not _rust_library_available(),
        reason="Rust library backend not available (install src/suews_bridge with physics feature)",
    )
    def test_library_cli_parity(self):
        """Quick parity check: Python library bridge vs CLI reference.

        Runs only 3 days of simulation to keep execution fast.
        Compares the variables in TOLERANCE_CONFIG against the corresponding slice
        of the monthly sample-output shards (the CLI-generated reference).
        """
        sim = sp.SUEWSSimulation.from_sample_data()
        # Run 3 days only: 1 day spin-up + 2 days checked
        n_days = 3
        output = sim.run(
            backend="rust",
            end_date=pd.Timestamp("2012-01-01") + pd.Timedelta(days=n_days),
        )
        df_output = output.df

        df_ref = load_sample_output(test_data_dir)

        variables_to_test = list(TOLERANCE_CONFIG.keys())
        failed_variables = []
        # Skip first day (spin-up), compare remaining timesteps
        warmup_steps = TIMESTEPS_PER_DAY
        n_check = len(df_output) - warmup_steps

        for var in variables_to_test:
            col_key = ("SUEWS", var)
            if col_key not in df_output.columns or var not in df_ref.columns:
                failed_variables.append(var)
                continue

            actual = df_output[col_key].values[warmup_steps:]
            expected = df_ref[var].values[warmup_steps : warmup_steps + n_check]
            tolerance = get_tolerance_for_variable(var)
            passed, _ = compare_arrays_with_tolerance(
                actual,
                expected,
                rtol=tolerance["rtol"],
                atol=tolerance["atol"],
                var_name=var,
            )
            if not passed:
                failed_variables.append(var)

        self.assertFalse(
            failed_variables,
            f"Library-CLI parity failed for: {', '.join(failed_variables)}",
        )

    @pytest.mark.core
    @pytest.mark.rust
    @pytest.mark.smoke
    def test_sample_output_validation(self):
        """Validate the smoke window (one model day by default) against the reference."""
        self._validate_sample_output()

    @pytest.mark.rust
    def test_sample_output_validation_full_year(self):
        """Validate the whole simulated year against the reference.

        The horizon is deliberately independent of SUEWS_FAIL_FAST_STEPS, so no
        environment setting can silently shorten it. Restores the coverage of
        accumulated-state and day-of-year-gated behaviour dropped in gh#1236 and
        gh#1382, without which a regression that first diverges mid-year passes CI:
        GDD and SDD take months to reach the values their branches test, and the
        day-140/170/250/300 gates are never evaluated inside a one-day run.

        Carries no tier marker, which is deliberate. Per the tier definitions,
        `standard` is "all non-slow tests for the relevant nature axis", so an
        unmarked test lands in `standard` and the full physics tiers: every ready
        PR touching code, every merge-queue entry, and the nightly. Since the
        queue always runs `standard`, nothing merges without this having passed.

        Not `slow`: `standard` resolves to "physics and not slow", so that marker
        would drop this from ready PRs and the queue too, leaving the coverage
        dependent on someone applying the 0-physics:change label by hand. That
        dependency is the mechanism that failed and produced this gap.

        Not `core` or `smoke` either: those tiers are for fast feedback on drafts
        and narrow changes, and a full year of engine time does not belong in a
        tier defined as "fast enough for draft PRs". gh#1348 put a full-year run
        in `smoke` and gh#1382 reverted it six days later over a Windows
        per-test timeout.

        Measured runtimes, and the case for promoting this to `smoke` should
        Windows prove to have headroom, are recorded in gh#1679 where they carry
        a date. They are deliberately not repeated here: nothing asserts them, so
        a docstring cannot tell a reader when they stopped being true.
        """
        self._validate_sample_output(full_year=True)

    def _validate_sample_output(self, full_year: bool = False):
        """Run the engine on the sample config and compare against the reference.

        Compares every variable in TOLERANCE_CONFIG. The horizon is the whole
        reference when full_year, otherwise the fail-fast window. Skipped if the
        engine binary has not been built.
        """
        print("\n" + "=" * 70)
        print("Sample Output Validation")
        print("=" * 70)

        engine = _locate_engine()
        sample_dir = Path(sp.__file__).parent / "sample_data"
        sample_config = sample_dir / "sample_config.yml"
        assert sample_config.exists(), f"Sample config not found: {sample_config}"

        df_ref = load_sample_output(test_data_dir)
        print(f"Reference: {df_ref.shape[0]} rows x {df_ref.shape[1]} columns")

        validation_steps = (
            len(df_ref) if full_year else _resolve_fail_fast_steps(len(df_ref))
        )
        truncated = validation_steps < len(df_ref)

        # The smoke path stays short for wheel CI, especially on Windows where a
        # full-year run can exceed the per-test timeout.
        timeout = 1800 if full_year else 120

        with tempfile.TemporaryDirectory() as tmpdir:
            run_dir = Path(tmpdir)
            config_path, forcing_rows = _write_run_inputs(
                sample_dir, sample_config, run_dir, validation_steps, truncated
            )
            print(
                f"Validating first {validation_steps} timesteps "
                f"from {forcing_rows} forcing rows"
            )
            arrow_bytes = _run_engine(engine, config_path, run_dir, timeout)

        df_actual = _read_engine_output(arrow_bytes, TOLERANCE_CONFIG)
        if len(df_actual) < validation_steps:
            self.fail(
                "Engine produced fewer rows than requested: "
                f"got {len(df_actual)}, requested {validation_steps}"
            )
        df_actual = df_actual.iloc[:validation_steps]

        self.assertLessEqual(
            len(df_actual),
            len(df_ref),
            f"Engine output is longer than reference: "
            f"{len(df_actual)} vs {len(df_ref)}",
        )
        df_expected = df_ref.iloc[: len(df_actual)]

        print(
            f"\nComparing {len(df_actual)} timesteps across "
            f"{', '.join(TOLERANCE_CONFIG)}"
        )
        print("=" * 70)

        all_passed, failed, report = _compare_frames(
            df_actual, df_expected, TOLERANCE_CONFIG
        )

        print("\n" + "=" * 70)
        print(
            "[PASS] Output matches reference"
            if all_passed
            else f"[FAIL] Validation failed for: {', '.join(failed)}"
        )

        self.assertTrue(
            all_passed,
            f"Engine vs reference failed for: {', '.join(failed)}\n"
            + "\n".join(report),
        )




if __name__ == "__main__":
    import unittest

    unittest.main()


# ============================================================================
# STEBBS VALIDATION TEST
# ============================================================================


@pytest.mark.core
@pytest.mark.slow  # Runs in test-all, scheduled builds, release builds, or manual all-tier validation.
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

        # Subset forcing data to match config period (2017-08-26 to 2017-08-27).
        # Run day 1 as spin-up and validate only the first N timesteps of day 2
        # for fail-fast debugging.
        df_forcing_window = df_forcing_full.loc["2017-08-26":"2017-08-27"]
        max_validation_steps = len(df_forcing_window) - TIMESTEPS_PER_DAY
        if max_validation_steps < 1:
            self.fail(
                "Insufficient forcing data for STEBBS validation: "
                f"{len(df_forcing_window)} rows."
            )
        validation_steps = _resolve_fail_fast_steps(max_validation_steps)
        if validation_steps == max_validation_steps:
            print(f"[INFO] Validating all {validation_steps} available steps.")
        df_forcing = df_forcing_window.iloc[: TIMESTEPS_PER_DAY + validation_steps]

        print(
            "Running STEBBS simulation "
            f"({len(df_forcing)} timesteps: day-1 spin-up + "
            f"{validation_steps} validation steps)..."
        )
        df_output, df_state = sp.run_supy(df_forcing, df_state_init)

        # Load reference output
        print("Loading reference output...")
        df_reference = pd.read_csv(reference_output_path).iloc[:validation_steps].copy()

        # Define STEBBS-specific variables to test with tolerances
        # Higher tolerances for building energy due to complex thermal dynamics
        stebbs_variables = {
            # water mains temperature
            "Twater_mains": {"rtol": 0.02, "atol": 0.5},  # 2% / 0.5K tolerance
            # Indoor conditions - affected by complex heat transfer
            "Tair_ind": {"rtol": 0.02, "atol": 0.5},  # 2% / 0.5K tolerance
            # Building loads - higher tolerance due to control logic
            "QHload_heating_FA": {"rtol": 0.05, "atol": 5.0},  # 5% / 5W tolerance
            "QHload_cooling_FA": {"rtol": 0.05, "atol": 5.0},  # 5% / 5W tolerance
            "QH_lighting_FA": {"rtol": 0.05, "atol": 5.0},  # 5% / 5W tolerance
        }

        print(f"\nValidating STEBBS variables: {', '.join(stebbs_variables.keys())}")
        print("=" * 70)

        # Extract day-2 validation window from simulation output to match reference.
        df_output_day2 = df_output.iloc[
            TIMESTEPS_PER_DAY : TIMESTEPS_PER_DAY + validation_steps
        ]

        print("\nFiltered output to match reference period (2017-08-27):")
        print(f"  Validation timesteps: {validation_steps}")
        print(f"  Simulation output (2nd day window) length: {len(df_output_day2)}")
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
