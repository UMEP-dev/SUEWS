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

import functools
import json
import os
from pathlib import Path
import platform
import subprocess
import sys
import tempfile
from unittest import TestCase

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest
import yaml

import supy as sp
from supy.cmd import rust_bridge

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-PARITY", "C-RESPONSE")]

# Get the test data directory
test_data_dir = Path(__file__).parent.parent / "fixtures" / "data_test"
# The reference output is stored as twelve monthly plain-CSV shards written at
# seven significant figures, plus a provenance.json sidecar. The shards are
# parsed once per session by the `sample_reference` fixture in test/conftest.py,
# not per test: the full-year frame costs a few seconds of CSV parsing and
# every reader wants the same immutable copy. See the split/combine convention
# and the precision justification in fixtures/data_test/sample_output_io.py.
sys.path.insert(0, str(test_data_dir))
from sample_output_io import (  # noqa: E402
    REFERENCE_FLOAT_FORMAT,
    column_names_sha256,
    read_reference_provenance,
    reference_file_digest,
    shard_identities,
)

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


def _sample_data_dir():
    """Return supy's bundled sample_data directory.

    Uses the package's own resource handle rather than `Path(supy.__file__).parent`.
    The latter assumes the package is an unpacked directory on disk, which is not
    guaranteed by the import system, and it couples tests to supy's internal
    layout. supy resolves its own data this way in `_env.py`; the tests simply
    never adopted it, because there is no public accessor for this path.
    """
    from supy._env import trv_supy_module

    return trv_supy_module / "sample_data"


def _engine_binary_name(os_name: str | None = None) -> str:
    """Return the platform-specific Rust CLI binary filename."""
    return "suews-engine.exe" if (os_name or os.name) == "nt" else "suews-engine"


def _locate_engine(*, repo_root: Path | None = None) -> Path:
    """Return the Rust CLI binary, skipping the test if none is available.

    Prefers the copy bundled inside the installed ``supy`` package (the
    artefact tested in wheel CI and ``cibuildwheel``) so a stale local build
    under ``src/suews_bridge/target/release`` cannot shadow the installed
    wheel. Falls back to the unpackaged release build only when no bundled
    binary is present in the installed package.
    """
    try:
        bundled = Path(rust_bridge._bridge_binary())
    except FileNotFoundError:
        bundled = None
    else:
        if bundled.is_file():
            return bundled

    root = Path(__file__).resolve().parents[2] if repo_root is None else repo_root
    dev_binary = (
        root / "src" / "suews_bridge" / "target" / "release" / _engine_binary_name()
    )
    if dev_binary.is_file():
        return dev_binary

    pytest.skip(
        "Rust CLI binary not found in installed package or target/release; "
        "build with: make bridge"
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

    The config is redirected to write its output into run_dir. When ``truncated``
    is true, the forcing file keeps one extra row past ``validation_steps`` for
    boundary-free interpolation and ``model.control.end_time`` is aligned to the
    exact ``validation_steps`` horizon so the engine enforces a covered period.

    The engine expects resolved YAML, so the config written here is the one
    ``suews run`` hands the bridge: validated through the data model, with every
    default filled in (gh#2004). The forcing copy sits beside it, so the relative
    ``forcing.file`` still resolves against the config's own directory.
    """
    from supy._run_rust import config_to_engine_yaml
    from supy.data_model import SUEWSConfig

    # sample_config is a Traversable, so use its own open() rather than the
    # builtin, which requires os.PathLike and fails for zip-backed resources.
    with sample_config.open(encoding="utf-8") as handle:
        cfg = yaml.safe_load(handle)

    control = cfg["model"]["control"]
    tstep = int(control.get("tstep", 300))
    control["output"]["dir"] = str(run_dir)
    if truncated:
        end_ts = pd.Timestamp(control["start_time"]) + pd.Timedelta(
            seconds=validation_steps * tstep
        )
        control["end_time"] = end_ts.strftime("%Y-%m-%d %H:%M:%S")

    config_path = run_dir / "sample_config.yml"
    config_path.write_text(
        config_to_engine_yaml(SUEWSConfig.from_dict(cfg)), encoding="utf-8"
    )

    forcing_name = _forcing_filename(control)
    forcing_rows = _forcing_rows_for(validation_steps, tstep, truncated)
    _write_forcing_prefix(
        sample_dir / forcing_name, run_dir / forcing_name, forcing_rows
    )
    return config_path, forcing_rows


def _run_engine(binary: Path, config_path: Path, run_dir: Path, timeout: int) -> Path:
    """Run the engine and return the path of its Arrow output."""
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
    return output_path


def _read_engine_output(output_path: Path, columns) -> pd.DataFrame:
    """Return the requested columns of the Arrow output as a DataFrame.

    Reads through a memory map and projects before materialising anything, so
    only the pages holding the requested columns are ever faulted in. The
    engine writes all eleven output groups: a full year is 1,350 columns over
    105,408 rows, about 1.1 GB on disk, and this comparison reads nine of those
    columns. Slurping the file into ``bytes`` and calling ``read_all()`` cost
    that whole 1.1 GB in this process; projecting off the map costs the nine
    columns.

    Every column is copied out of the map before returning, and the map is
    closed here rather than being left for the caller: pyarrow holding the file
    open blocks TemporaryDirectory cleanup on Windows.
    """
    import pyarrow as pa
    import pyarrow.ipc as ipc

    with pa.memory_map(str(output_path), "rb") as source:
        reader = ipc.open_file(source)
        present = [name for name in columns if name in reader.schema.names]
        table = reader.read_all().select(present)
        # to_pandas() may hand back arrays that alias the map, which would
        # dangle once it is closed. Copy each column explicitly instead.
        data = {
            name: table.column(name).to_numpy(zero_copy_only=False).copy()
            for name in present
        }
        del table, reader
    return pd.DataFrame(data)


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


def deviation_arrays(actual, expected):
    """Return (abs_diff, rel_diff, valid_mask, nan_mismatch) for two equal-shape arrays.

    This is the arithmetic the comparator applies before any tolerance is
    consulted, factored out so scripts/suews/tolerance_spread.py can record the
    raw cross-platform deviation with exactly the comparator's definitions: the
    relative deviation divides by ``|expected| + eps``, and ``valid_mask`` is
    False only where both arrays are NaN.
    """
    actual = np.asarray(actual)
    expected = np.asarray(expected)
    if actual.shape != expected.shape:
        raise ValueError(f"Shape mismatch: {actual.shape} vs {expected.shape}")

    with np.errstate(divide="ignore", invalid="ignore"):
        abs_diff = np.abs(actual - expected)
        # Use expected value for relative difference calculation
        # Add small epsilon to avoid division by zero
        rel_diff = abs_diff / (np.abs(expected) + np.finfo(float).eps)

    actual_nan = np.isnan(actual)
    expected_nan = np.isnan(expected)
    nan_mismatch = actual_nan != expected_nan
    # Ignore positions where both are NaN
    valid_mask = ~(actual_nan & expected_nan)
    return abs_diff, rel_diff, valid_mask, nan_mismatch


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

    # Handle shape mismatch or empty arrays (np.all on empty is vacuously True)
    if actual.shape != expected.shape:
        return (
            False,
            f"Shape mismatch for {var_name}: {actual.shape} vs {expected.shape}",
        )
    if actual.size == 0:
        return (
            False,
            f"Empty comparison array for {var_name}: expected at least 1 value",
        )

    # Calculate differences
    abs_diff, rel_diff, valid_mask, nan_mismatch = deviation_arrays(actual, expected)

    # Check tolerance using same logic as numpy.allclose
    within_tol = (abs_diff <= atol) | (rel_diff <= rtol)

    # Handle NaN values
    if np.any(nan_mismatch):
        return False, f"NaN mismatch for {var_name}: NaN positions differ"

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


_SUPY_LOAD_CACHE_NAMES = (
    "load_SUEWS_nml",
    "load_SUEWS_table",
    "lookup_code_lib",
    "lookup_KeySeq_lib",
    "gen_all_code_df",
    "build_code_exp_df",
    "gen_df_siteselect_exp",
    "gen_df_gridSurfaceChar_exp",
    "load_SUEWS_SurfaceChar_df",
)


def _clear_supy_load_caches() -> None:
    """Clear the explicit file/table LRU caches in ``supy._load``."""
    import supy._load as load_mod

    for name in _SUPY_LOAD_CACHE_NAMES:
        getattr(load_mod, name).cache_clear()


def _slice_aligned_library_parity(
    df_output: pd.DataFrame,
    df_ref: pd.DataFrame,
    expected_timestamps: pd.DatetimeIndex,
    *,
    warmup_steps: int,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Validate full grid/timestamp coverage and return aligned post-warmup slices.

    Derives the expected grid and timestamp index strictly from the forcing
    window and reference output (never from ``len(df_output)``), so truncating
    model output to the spin-up window fails loudly instead of comparing empty
    slices.
    """
    n_expected = len(expected_timestamps)
    assert n_expected > warmup_steps, (
        f"Expected forcing horizon ({n_expected}) must exceed warmup ({warmup_steps})"
    )
    assert len(df_ref) >= n_expected, (
        f"Reference shorter than requested horizon: {len(df_ref)} < {n_expected}"
    )

    df_ref_window = df_ref.iloc[:n_expected]
    expected_full_index = df_ref_window.index
    assert expected_full_index.get_level_values("datetime").equals(
        expected_timestamps
    ), "Reference timestamps do not match requested forcing timestamps"
    assert df_output.index.equals(expected_full_index), (
        "Library output grid/timestamp index does not match forcing/reference "
        f"window: got {len(df_output)} rows, expected {len(expected_full_index)}"
    )

    df_actual_check = df_output.iloc[warmup_steps:]
    df_expected_check = df_ref_window.iloc[warmup_steps:]
    n_post_warmup = n_expected - warmup_steps
    assert len(df_actual_check) == len(df_expected_check) == n_post_warmup > 0, (
        f"Expected {n_post_warmup} post-warmup rows, got {len(df_actual_check)}"
    )
    assert df_actual_check.index.equals(df_expected_check.index)
    return df_actual_check, df_expected_check


def _slice_aligned_stebbs_day2(
    df_output: pd.DataFrame,
    df_reference: pd.DataFrame,
    expected_forcing_index: pd.DatetimeIndex,
    *,
    warmup_steps: int,
    expected_grid: int,
) -> pd.DataFrame:
    """Validate full 2-day STEBBS output alignment and return the day-2 slice.

    Enforces expected grid, forcing timestamp index, and non-empty reference
    timestamp alignment before any per-variable comparison or all-NaN skip.
    """
    n_ref = len(df_reference)
    assert n_ref > 0, "STEBBS reference slice is empty"
    expected_total = warmup_steps + n_ref
    assert len(expected_forcing_index) == expected_total, (
        f"Forcing window ({len(expected_forcing_index)}) does not match "
        f"warmup ({warmup_steps}) + reference ({n_ref})"
    )

    expected_full_index = pd.MultiIndex.from_arrays(
        [[expected_grid] * expected_total, expected_forcing_index],
        names=["grid", "datetime"],
    )
    assert df_output.index.equals(expected_full_index), (
        "STEBBS output grid/timestamp index does not match expected 2-day "
        f"window: got {len(df_output)} rows, expected {expected_total}"
    )

    df_output_day2 = df_output.iloc[warmup_steps : warmup_steps + n_ref]
    assert len(df_output_day2) == n_ref > 0, (
        f"STEBBS day-2 output length mismatch: got {len(df_output_day2)}, expected {n_ref}"
    )
    assert "datetime" in df_reference.columns, (
        "STEBBS reference data is missing required 'datetime' column"
    )
    expected_day2_dt = pd.DatetimeIndex(pd.to_datetime(df_reference["datetime"]))
    assert df_output_day2.index.get_level_values("datetime").equals(
        expected_day2_dt
    ), "STEBBS day-2 timestamps do not match reference datetime column"
    return df_output_day2


class TestSampleOutput(TestCase):
    """Dedicated test class for validating SUEWS outputs against reference data."""

    # A TestCase method cannot take a fixture as an argument, so the
    # session-scoped reference is bound onto the instance by an autouse
    # fixture. This is what keeps the twelve shards parsed once per pytest
    # invocation rather than once per test method.
    @pytest.fixture(autouse=True)
    def _bind_sample_reference(self, sample_reference):
        self.df_ref = sample_reference

    def setUp(self):
        """Clear explicit ``supy._load`` table caches without a process-wide sweep."""
        _clear_supy_load_caches()

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
        end_date = pd.Timestamp("2012-01-01") + pd.Timedelta(days=n_days)
        expected_timestamps = sim.forcing.df.loc[:end_date].index
        output = sim.run(
            backend="rust",
            end_date=end_date,
        )
        df_output = output.df

        df_ref = self.df_ref

        variables_to_test = list(TOLERANCE_CONFIG.keys())
        failed_variables = []
        # Skip first day (spin-up), enforcing full forcing/reference alignment first.
        warmup_steps = TIMESTEPS_PER_DAY
        df_actual_check, df_expected_check = _slice_aligned_library_parity(
            df_output,
            df_ref,
            expected_timestamps,
            warmup_steps=warmup_steps,
        )

        for var in variables_to_test:
            col_key = ("SUEWS", var)
            if col_key not in df_actual_check.columns or var not in df_expected_check.columns:
                failed_variables.append(var)
                continue

            actual = df_actual_check[col_key].values
            expected = df_expected_check[var].values
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
    @pytest.mark.e2e
    def test_sample_output_validation(self):
        """Validate the smoke window (one model day by default) against the reference."""
        self._validate_sample_output()

    @pytest.mark.core
    @pytest.mark.slow(
        reason="full-year comparison: 17 to 36 CPU-s on Linux across dispatches and over two minutes of wall time on Windows"
    )
    @pytest.mark.rust
    @pytest.mark.e2e
    def test_sample_output_validation_full_year(self):
        """Validate the whole simulated year against the reference.

        The horizon is deliberately independent of SUEWS_FAIL_FAST_STEPS, so no
        environment setting can silently shorten it. Restores the coverage of
        accumulated-state and day-of-year-gated behaviour dropped in gh#1236 and
        gh#1382, without which a regression that first diverges mid-year passes CI:
        GDD and SDD take months to reach the values their branches test, and the
        day-140/170/250/300 gates are never evaluated inside a one-day run.

        Carries both `core` and `slow`: the regression is essential before merge
        and expensive. Ready PRs and merge queue resolve `standard` to "physics
        and (core or not slow)", so importance overrides cost there. Draft
        `core` selection still ends in `and not slow`, keeping the full-year run
        off fast feedback paths. It is deliberately not `smoke`: gh#1348 put a
        full-year run there and gh#1382 reverted it six days later over a Windows
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

        Why the CLI binary rather than SUEWSSimulation, which is what users call:
        memory. A full year through the library holds the whole 105,408 x 1295
        output as a DataFrame, measured at 8.2 GB peak resident memory. The CLI
        writes Arrow, so the columns under test can be read straight off a
        memory map without the other ten output groups ever being faulted in,
        measured at 128 MiB in this process. Under xdist the difference decides
        whether this test can run at all on hosted runners. What the engine
        subprocess itself costs while producing that file is a separate matter
        and is not reduced here.

        What that trades away is real and worth knowing: this path exercises the
        engine and its Arrow output, not the PyO3 bridge, DataFrame construction
        or the rest of the supy wrapper. That surface is covered by
        test_library_cli_parity, which currently runs 3 days. So the library, the
        thing users actually call, has a much shorter reference comparison than
        the CLI does. gh#1209 originally ran both over a full year; gh#1236 cut
        the library side to 3 days and the CLI side is what survived.
        """
        print("\n" + "=" * 70)
        print("Sample Output Validation")
        print("=" * 70)

        engine = _locate_engine()
        sample_dir = _sample_data_dir()
        sample_config = sample_dir / "sample_config.yml"
        assert sample_config.is_file(), f"Sample config not found: {sample_config}"

        df_ref = self.df_ref
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
            output_path = _run_engine(engine, config_path, run_dir, timeout)
            df_actual = _read_engine_output(output_path, TOLERANCE_CONFIG)

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


# ============================================================================
# ENGINE BINARY PRECEDENCE AND TARGETED CACHE CLEARING
# ============================================================================


def test_engine_binary_name_matches_platform():
    """Platform helper adds .exe only on Windows."""
    assert _engine_binary_name("nt") == "suews-engine.exe"
    assert _engine_binary_name("posix") == "suews-engine"


def test_locate_engine_prefers_installed_package_over_target_release(
    tmp_path, monkeypatch
):
    """Installed package binary wins even when a stale target/release artefact exists."""
    binary_name = _engine_binary_name()
    pkg_binary = tmp_path / "installed_pkg" / "bin" / binary_name
    pkg_binary.parent.mkdir(parents=True)
    pkg_binary.write_bytes(b"installed-wheel-binary")

    repo_root = tmp_path / "repo"
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    dev_binary.parent.mkdir(parents=True)
    dev_binary.write_bytes(b"stale-dev-binary")

    monkeypatch.setattr(rust_bridge, "_bridge_binary", lambda: pkg_binary)

    assert _locate_engine(repo_root=repo_root) == pkg_binary


def test_locate_engine_falls_back_to_target_release_when_package_missing(
    tmp_path, monkeypatch
):
    """Fallback uses target/release only when no installed package binary exists."""
    binary_name = _engine_binary_name()
    repo_root = tmp_path / "repo"
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    dev_binary.parent.mkdir(parents=True)
    dev_binary.write_bytes(b"dev-binary")

    def _missing_bundled() -> Path:
        raise FileNotFoundError("no bundled suews-engine")

    monkeypatch.setattr(rust_bridge, "_bridge_binary", _missing_bundled)

    assert _locate_engine(repo_root=repo_root) == dev_binary


def test_locate_engine_skips_when_no_binary_exists(tmp_path, monkeypatch):
    """Skip with a concrete build hint when neither installed nor dev binary exists."""

    def _missing_bundled() -> Path:
        raise FileNotFoundError("no bundled suews-engine")

    monkeypatch.setattr(rust_bridge, "_bridge_binary", _missing_bundled)

    with pytest.raises(pytest.skip.Exception, match="make bridge"):
        _locate_engine(repo_root=tmp_path / "empty_repo")


def test_clear_supy_load_caches_is_targeted_not_process_wide():
    """TestSampleOutput.setUp clears only supy._load caches, never unrelated LRU caches."""
    import supy._load as load_mod

    @functools.lru_cache(maxsize=4)
    def unrelated_sentinel(key: str) -> str:
        return f"cached:{key}"

    assert unrelated_sentinel("keep-me") == "cached:keep-me"
    assert unrelated_sentinel.cache_info().currsize == 1

    # Verify the explicit list covers every lru_cache wrapper defined in supy._load.
    discovered = {
        name
        for name, obj in vars(load_mod).items()
        if isinstance(obj, functools._lru_cache_wrapper)
        and getattr(obj, "__module__", None) == load_mod.__name__
    }
    assert set(_SUPY_LOAD_CACHE_NAMES) == discovered

    # Run setUp() on a TestSampleOutput instance (uses _clear_supy_load_caches).
    TestSampleOutput().setUp()

    # Unrelated process LRU cache must survive untouched (fails under gc.get_objects sweep).
    assert unrelated_sentinel.cache_info().currsize == 1
    for name in _SUPY_LOAD_CACHE_NAMES:
        assert getattr(load_mod, name).cache_info().currsize == 0


def test_compare_arrays_with_tolerance_rejects_empty_arrays():
    """Empty arrays must fail rather than passing vacuously via np.all([])."""
    passed, report = compare_arrays_with_tolerance(
        np.array([], dtype=float),
        np.array([], dtype=float),
        rtol=0.01,
        atol=0.1,
        var_name="QH",
    )
    assert passed is False
    assert "Empty comparison array" in report


def test_library_parity_alignment_rejects_spinup_only_truncation():
    """Truncating 3-day output to 288 spin-up rows, wrong grid, or shifted times fails."""
    n_days = 3
    timestamps = pd.date_range(
        "2012-01-01 00:05", periods=TIMESTEPS_PER_DAY * n_days, freq="5min"
    )
    idx = pd.MultiIndex.from_arrays(
        [[1] * len(timestamps), timestamps], names=["grid", "datetime"]
    )
    df_ref = pd.DataFrame({"QH": np.arange(len(timestamps), dtype=float)}, index=idx)
    df_output = pd.DataFrame(
        np.arange(len(timestamps), dtype=float),
        index=idx,
        columns=pd.MultiIndex.from_tuples([("SUEWS", "QH")], names=["group", "var"]),
    )

    # Positive control: full 3-day aligned output yields 576 post-warmup rows.
    actual_ok, expected_ok = _slice_aligned_library_parity(
        df_output, df_ref, timestamps, warmup_steps=TIMESTEPS_PER_DAY
    )
    assert len(actual_ok) == len(expected_ok) == TIMESTEPS_PER_DAY * 2

    # Truncated to only the 288 spin-up rows (0 evaluation rows) must fail.
    with pytest.raises(AssertionError, match="grid/timestamp index"):
        _slice_aligned_library_parity(
            df_output.iloc[:TIMESTEPS_PER_DAY],
            df_ref,
            timestamps,
            warmup_steps=TIMESTEPS_PER_DAY,
        )

    # Wrong grid ID must fail.
    wrong_grid_idx = pd.MultiIndex.from_arrays(
        [[99] * len(timestamps), timestamps], names=["grid", "datetime"]
    )
    with pytest.raises(AssertionError, match="grid/timestamp index"):
        _slice_aligned_library_parity(
            df_output.set_axis(wrong_grid_idx),
            df_ref,
            timestamps,
            warmup_steps=TIMESTEPS_PER_DAY,
        )

    # Shifted timestamps must fail.
    shifted_idx = pd.MultiIndex.from_arrays(
        [[1] * len(timestamps), timestamps + pd.Timedelta(minutes=5)],
        names=["grid", "datetime"],
    )
    with pytest.raises(AssertionError, match="grid/timestamp index"):
        _slice_aligned_library_parity(
            df_output.set_axis(shifted_idx),
            df_ref,
            timestamps,
            warmup_steps=TIMESTEPS_PER_DAY,
        )


def test_stebbs_day2_alignment_rejects_spinup_only_truncation():
    """Truncating 2-day STEBBS output to spin-up rows or short slices must fail."""
    forcing_dt = pd.date_range(
        "2017-08-26 00:00", periods=TIMESTEPS_PER_DAY * 2, freq="5min"
    )
    day2_dt = forcing_dt[TIMESTEPS_PER_DAY:]
    df_ref = pd.DataFrame({
        "datetime": day2_dt,
        "QN_bldg_FA": np.linspace(1.0, 2.0, len(day2_dt)),
    })
    idx = pd.MultiIndex.from_arrays(
        [[1] * len(forcing_dt), forcing_dt], names=["grid", "datetime"]
    )
    df_output = pd.DataFrame(
        np.linspace(0.0, 2.0, len(forcing_dt)),
        index=idx,
        columns=pd.MultiIndex.from_tuples(
            [("STEBBS", "QN_bldg_FA")], names=["group", "var"]
        ),
    )

    # Positive control: exact 2-day aligned output returns 288 day-2 rows.
    df_day2 = _slice_aligned_stebbs_day2(
        df_output,
        df_ref,
        forcing_dt,
        warmup_steps=TIMESTEPS_PER_DAY,
        expected_grid=1,
    )
    assert len(df_day2) == TIMESTEPS_PER_DAY

    # Truncated to only the 288 spin-up rows (0 day-2 rows) must fail.
    with pytest.raises(AssertionError, match="grid/timestamp index"):
        _slice_aligned_stebbs_day2(
            df_output.iloc[:TIMESTEPS_PER_DAY],
            df_ref,
            forcing_dt,
            warmup_steps=TIMESTEPS_PER_DAY,
            expected_grid=1,
        )

    # Partial day-2 truncation (10 rows instead of 288) must fail.
    with pytest.raises(AssertionError, match="grid/timestamp index"):
        _slice_aligned_stebbs_day2(
            df_output.iloc[: TIMESTEPS_PER_DAY + 10],
            df_ref,
            forcing_dt,
            warmup_steps=TIMESTEPS_PER_DAY,
            expected_grid=1,
        )

    # Wrong grid ID must fail.
    wrong_grid_idx = pd.MultiIndex.from_arrays(
        [[2] * len(forcing_dt), forcing_dt], names=["grid", "datetime"]
    )
    with pytest.raises(AssertionError, match="grid/timestamp index"):
        _slice_aligned_stebbs_day2(
            df_output.set_axis(wrong_grid_idx),
            df_ref,
            forcing_dt,
            warmup_steps=TIMESTEPS_PER_DAY,
            expected_grid=1,
        )

    # Mismatched reference day-2 timestamps must fail.
    df_ref_shifted = df_ref.copy()
    df_ref_shifted["datetime"] = day2_dt + pd.Timedelta(minutes=5)
    with pytest.raises(AssertionError, match="reference datetime"):
        _slice_aligned_stebbs_day2(
            df_output,
            df_ref_shifted,
            forcing_dt,
            warmup_steps=TIMESTEPS_PER_DAY,
            expected_grid=1,
        )


def test_stebbs_day2_alignment_rejects_missing_reference_datetime(tmp_path):
    """Omitting the required 'datetime' column from STEBBS reference data must fail."""
    forcing_dt = pd.date_range(
        "2017-08-26 00:00", periods=TIMESTEPS_PER_DAY * 2, freq="5min"
    )
    day2_dt = forcing_dt[TIMESTEPS_PER_DAY:]
    df_ref = pd.DataFrame({
        "datetime": day2_dt,
        "QN_bldg_FA": np.linspace(1.0, 2.0, len(day2_dt)),
    })
    idx = pd.MultiIndex.from_arrays(
        [[1] * len(forcing_dt), forcing_dt], names=["grid", "datetime"]
    )
    df_output = pd.DataFrame(
        np.linspace(0.0, 2.0, len(forcing_dt)),
        index=idx,
        columns=pd.MultiIndex.from_tuples(
            [("STEBBS", "QN_bldg_FA")], names=["group", "var"]
        ),
    )

    # In-memory DataFrame without 'datetime' must fail.
    df_ref_no_dt = df_ref.drop(columns=["datetime"])
    with pytest.raises(AssertionError, match="missing required 'datetime' column"):
        _slice_aligned_stebbs_day2(
            df_output,
            df_ref_no_dt,
            forcing_dt,
            warmup_steps=TIMESTEPS_PER_DAY,
            expected_grid=1,
        )

    # Round-tripped reference CSV without 'datetime' header/column must also fail.
    csv_path = tmp_path / "sample_output_stebbs_no_datetime.csv"
    df_ref_no_dt.to_csv(csv_path, index=False)
    df_csv_no_dt = pd.read_csv(csv_path)
    with pytest.raises(AssertionError, match="missing required 'datetime' column"):
        _slice_aligned_stebbs_day2(
            df_output,
            df_csv_no_dt,
            forcing_dt,
            warmup_steps=TIMESTEPS_PER_DAY,
            expected_grid=1,
        )


# ============================================================================
# REFERENCE PROVENANCE
# ============================================================================


def test_shard_identity_ignores_line_endings(tmp_path):
    """A CRLF checkout of a shard (git autocrlf on Windows) has the same
    identity as the LF original, so the sidecar assertion holds everywhere."""
    lf = tmp_path / "lf"
    crlf = tmp_path / "crlf"
    lf.mkdir()
    crlf.mkdir()
    body = "a,b\n1,2\n3,4\n"
    (lf / "sample_output_2012-01.csv").write_bytes(body.encode())
    (crlf / "sample_output_2012-01.csv").write_bytes(body.replace("\n", "\r\n").encode())
    assert shard_identities(lf) == shard_identities(crlf)
    assert shard_identities(lf)[0]["size_bytes"] == len(body)


@pytest.mark.core
@pytest.mark.smoke
def test_reference_provenance_matches_shards(sample_reference, sample_reference_dir):
    """The provenance sidecar describes the reference actually on disk.

    The sidecar records which SuPy build, compiler and platform produced the
    reference, so a later refresh can be told apart from a drifted checkout.
    That is only worth anything if the sidecar and the shards cannot fall out
    of step, which is what this asserts: the shard set, each shard's bytes, the
    row and column counts and the column names must all match what the
    generator recorded.

    The check is on file bytes rather than on a hash of the loaded frame, so it
    cannot fail because a pandas hashing implementation changed underneath it.
    A sidecar that is stale, missing a shard, or absent altogether fails here
    rather than being noticed the next time somebody wonders where the numbers
    came from.
    """
    provenance = read_reference_provenance(sample_reference_dir)

    assert provenance["float_format"] == REFERENCE_FLOAT_FORMAT, (
        "sidecar records a different write precision than the fixture module "
        f"defines: {provenance['float_format']!r} vs {REFERENCE_FLOAT_FORMAT!r}"
    )
    assert provenance["rows"] == len(sample_reference)
    assert provenance["columns"] == sample_reference.shape[1]
    assert provenance["column_names_sha256"] == column_names_sha256(sample_reference), (
        "reference columns differ from the set the sidecar was written for"
    )

    recorded = {shard["name"]: shard for shard in provenance["shards"]}
    on_disk = {shard["name"]: shard for shard in shard_identities(sample_reference_dir)}
    assert recorded.keys() == on_disk.keys(), (
        "shard set differs from the sidecar: "
        f"only on disk {sorted(on_disk.keys() - recorded.keys())}, "
        f"only in sidecar {sorted(recorded.keys() - on_disk.keys())}"
    )
    mismatched = [
        name
        for name in sorted(on_disk)
        if on_disk[name]["sha256"] != recorded[name]["sha256"]
    ]
    assert not mismatched, (
        f"shard contents differ from the sidecar: {', '.join(mismatched)} -- "
        "regenerate the reference with scripts/suews/gen_sample_output.py so "
        "the sidecar travels with the numbers"
    )

    build = provenance["build"]
    assert build["supy_version"], "sidecar records no SuPy version"


if __name__ == "__main__":
    import unittest

    unittest.main()


# ============================================================================
# STEBBS VALIDATION TEST
# ============================================================================


@pytest.mark.core
# Expensive, but core keeps it in ready-PR and queue standard tiers.
@pytest.mark.slow(
    reason="full-year STEBBS comparison: 21 to 34 CPU-s on Linux across dispatches and 148 s of wall time on Windows"
)
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

        provenance = json.loads(
            (stebbs_test_dir / "provenance.json").read_text(encoding="utf-8")
        )
        self.assertEqual(
            provenance["files"],
            {
                name: reference_file_digest(stebbs_test_dir / name)[0]
                for name in (
                    config_path.name,
                    "test_2017_data_15_mp.txt",
                    reference_output_path.name,
                )
            },
        )

        print(f"\nLoading STEBBS test configuration from: {config_path}")

        # Initialise and run simulation
        print("Initializing SUEWS with STEBBS...")
        simulation = sp.SUEWSSimulation(config_path)
        df_forcing_full = simulation.forcing.df

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
        df_forcing = df_forcing_window.iloc[
            : TIMESTEPS_PER_DAY + validation_steps
        ].copy()
        # The supported OOP path validates forcing before execution. The STEBBS
        # fixture uses -999 as a dry-period sentinel, so normalise it to zero.
        df_forcing["rain"] = df_forcing["rain"].clip(lower=0)

        print(
            "Running STEBBS simulation "
            f"({len(df_forcing)} timesteps: day-1 spin-up + "
            f"{validation_steps} validation steps)..."
        )
        simulation.update_forcing(df_forcing)
        # The fixture forcing spans 2017-08-26 00:00 to 2017-08-27 23:55; run
        # exactly those rows rather than the calendar days the YAML names.
        output = simulation.run(
            start_date=df_forcing.index[0], end_date=df_forcing.index[-1]
        )
        df_output = output.df

        # Load reference output
        print("Loading reference output...")
        df_reference = pd.read_csv(reference_output_path).iloc[:validation_steps].copy()

        # Define STEBBS-specific variables to test with tolerances
        # Higher tolerances for building energy due to complex thermal dynamics
        stebbs_variables = {
            # Energy-budget regressions: use the repository's energy-flux
            # floor (0.8%, 1 W m-2), tighter than the load-control checks.
            # No cross-platform spread evidence justifies relaxing this floor.
            "QN_bldg_FA": {"rtol": 0.008, "atol": 1.0},
            "QS_total_FA": {"rtol": 0.008, "atol": 1.0},
            "QH_bldg_FA": {"rtol": 0.008, "atol": 1.0},
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

        # Extract day-2 validation window after enforcing full forcing/reference
        # grid and timestamp alignment (never truncating silently to min_len).
        expected_grid = simulation.config.sites[0].gridiv
        df_output_day2 = _slice_aligned_stebbs_day2(
            df_output,
            df_reference,
            df_forcing.index,
            warmup_steps=TIMESTEPS_PER_DAY,
            expected_grid=expected_grid,
        )

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

            # Enforce exact non-empty length match before any all-NaN skip.
            self.assertEqual(
                len(actual),
                len(expected),
                f"Length mismatch for {var}: {len(actual)} vs {len(expected)}",
            )
            self.assertGreater(len(expected), 0, f"Empty reference series for {var}")

            # Skip variables where non-empty reference is all NaN (e.g., cooling loads in winter)
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
