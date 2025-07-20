"""Test decorators for enhanced debugging output."""

import functools
import sys
import traceback
import numpy as np
import pandas as pd
from datetime import datetime


def debug_on_ci(func):
    """
    Decorator that adds detailed debug output when running in CI environment.
    Only activates when GITHUB_ACTIONS environment variable is set.
    """

    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        import os

        is_ci = os.environ.get("GITHUB_ACTIONS", "false").lower() == "true"

        if is_ci:
            print(f"\n{'=' * 60}")
            print(f"DEBUG: Starting {func.__name__} at {datetime.now()}")
            print(f"Platform: {sys.platform}")
            print(f"Python: {sys.version}")
            print(f"NumPy: {np.__version__}")
            print(f"Pandas: {pd.__version__}")
            print(f"{'=' * 60}\n")

        try:
            result = func(*args, **kwargs)
            if is_ci:
                print(f"\n✓ {func.__name__} completed successfully")
            return result
        except Exception as e:
            if is_ci:
                print(f"\n{'=' * 60}")
                print(f"✗ {func.__name__} FAILED")
                print(f"Error type: {type(e).__name__}")
                print(f"Error message: {str(e)}")
                print(f"Platform: {sys.platform}")
                print(f"Python: {sys.version}")
                print(f"NumPy: {np.__version__}")
                print(f"Pandas: {pd.__version__}")
                print("\nTraceback:")
                traceback.print_exc()
                print(f"{'=' * 60}\n")

                # Try to extract test instance and print relevant data
                if args and hasattr(args[0], "__dict__"):
                    print("\nTest instance attributes:")
                    for attr, value in args[0].__dict__.items():
                        if not attr.startswith("_"):
                            print(f"  {attr}: {type(value)}")
            raise

    return wrapper


def debug_dataframe_output(func):
    """
    Decorator specifically for tests that produce DataFrame outputs.
    Captures and logs DataFrame characteristics on failure.
    """

    @functools.wraps(func)
    def wrapper(self, *args, **kwargs):
        import os

        is_ci = os.environ.get("GITHUB_ACTIONS", "false").lower() == "true"

        try:
            result = func(self, *args, **kwargs)
            return result
        except AssertionError as e:
            if is_ci:
                print(f"\n{'=' * 60}")
                print(f"ASSERTION FAILED in {func.__name__}")
                print(f"Error: {str(e)}")

                # Try to find DataFrames in instance
                for attr_name in dir(self):
                    if not attr_name.startswith("_"):
                        attr = getattr(self, attr_name, None)
                        if isinstance(attr, pd.DataFrame):
                            print(f"\nDataFrame '{attr_name}':")
                            print(f"  Shape: {attr.shape}")
                            print(f"  Columns: {list(attr.columns)[:10]}...")
                            print(f"  Non-null count: {attr.count().sum()}")
                            print(
                                f"  Memory usage: {attr.memory_usage().sum() / 1024:.1f} KB"
                            )

                            if attr.empty:
                                print("  WARNING: DataFrame is empty!")
                            else:
                                print(f"  First few rows:\n{attr.head(3)}")

                print(f"{'=' * 60}\n")
            raise

    return wrapper


def debug_water_balance(func):
    """
    Decorator specifically for water balance tests.
    Logs detailed water balance components on failure.
    """

    @functools.wraps(func)
    def wrapper(self, *args, **kwargs):
        import os

        is_ci = os.environ.get("GITHUB_ACTIONS", "false").lower() == "true"

        try:
            # Store original assertion method
            original_assertTrue = self.assertTrue

            # Replace with debugging version if in CI
            if is_ci:

                def debug_assertTrue(condition, msg=None):
                    if not condition:
                        print(f"\n{'=' * 60}")
                        print("WATER BALANCE DEBUG INFO")
                        print(f"{'=' * 60}")

                        # Try to access water balance variables from locals
                        frame = sys._getframe(1)
                        local_vars = frame.f_locals

                        # Print relevant variables if they exist
                        for var_name in [
                            "ser_totalstore_change",
                            "ser_water_balance",
                            "ser_water_in",
                            "ser_water_out",
                            "test_dif",
                        ]:
                            if var_name in local_vars:
                                var_value = local_vars[var_name]
                                print(f"\n{var_name}:")
                                if isinstance(var_value, pd.Series):
                                    print(f"  Length: {len(var_value)}")
                                    print(f"  Min: {var_value.min():.6f}")
                                    print(f"  Max: {var_value.max():.6f}")
                                    print(f"  Mean: {var_value.mean():.6f}")
                                    print(f"  Std: {var_value.std():.6f}")

                                    # Find worst cases
                                    if "test_dif" not in var_name:
                                        abs_max_idx = var_value.abs().idxmax()
                                        print(
                                            f"  Largest absolute value: {var_value[abs_max_idx]:.6f} at {abs_max_idx}"
                                        )
                                else:
                                    print(f"  Value: {var_value}")

                        # Calculate and show the difference
                        if (
                            "ser_totalstore_change" in local_vars
                            and "ser_water_balance" in local_vars
                        ):
                            diff = (
                                local_vars["ser_totalstore_change"]
                                - local_vars["ser_water_balance"]
                            ).abs()
                            print(f"\nWater balance difference:")
                            print(f"  Max absolute difference: {diff.max():.9f}")
                            print(f"  Exceeds 1e-6 threshold: {diff.max() > 1e-6}")

                            # Show timestamps with largest errors
                            if len(diff) > 0:
                                worst_times = diff.nlargest(5)
                                print(f"\n  Worst 5 timestamps:")
                                for time, error in worst_times.items():
                                    print(f"    {time}: {error:.9f}")

                        print(f"{'=' * 60}\n")

                    # Call original assertion
                    original_assertTrue(condition, msg)

                self.assertTrue = debug_assertTrue

            # Run the test
            result = func(self, *args, **kwargs)

            # Restore original method
            if is_ci:
                self.assertTrue = original_assertTrue

            return result

        except Exception:
            # Make sure to restore on exception too
            if is_ci and hasattr(self, "assertTrue"):
                self.assertTrue = original_assertTrue
            raise

    return wrapper


def capture_test_artifacts(artifact_name):
    """
    Decorator that saves test data to files when tests fail in CI.
    Useful for debugging with actual data from CI environment.
    """

    def decorator(func):
        @functools.wraps(func)
        def wrapper(self, *args, **kwargs):
            import os
            import tempfile
            import pickle

            is_ci = os.environ.get("GITHUB_ACTIONS", "false").lower() == "true"

            try:
                result = func(self, *args, **kwargs)
                return result
            except Exception as e:
                if is_ci:
                    # Create artifacts directory
                    artifact_dir = os.environ.get("RUNNER_TEMP", tempfile.gettempdir())
                    artifact_dir = os.path.join(artifact_dir, "suews_test_artifacts")
                    os.makedirs(artifact_dir, exist_ok=True)

                    # Save test data
                    artifact_file = os.path.join(
                        artifact_dir, f"{artifact_name}_{func.__name__}.pkl"
                    )

                    # Collect relevant data
                    test_data = {
                        "test_name": func.__name__,
                        "error_type": type(e).__name__,
                        "error_message": str(e),
                        "platform": sys.platform,
                        "python_version": sys.version,
                        "numpy_version": np.__version__,
                        "pandas_version": pd.__version__,
                    }

                    # Try to collect DataFrames from test instance
                    for attr_name in dir(self):
                        if not attr_name.startswith("_"):
                            attr = getattr(self, attr_name, None)
                            if isinstance(attr, (pd.DataFrame, pd.Series, np.ndarray)):
                                test_data[attr_name] = attr

                    # Save to file
                    with open(artifact_file, "wb") as f:
                        pickle.dump(test_data, f)

                    print(f"\n✓ Test artifacts saved to: {artifact_file}")
                    print(
                        f"  File size: {os.path.getsize(artifact_file) / 1024:.1f} KB"
                    )

                raise

        return wrapper

    return decorator


def analyze_dailystate_nan(func):
    """
    Decorator specifically for DailyState tests that logs NaN patterns
    even when tests pass. Only activates for cp312 on ARM Mac in CI.
    """

    @functools.wraps(func)
    def wrapper(self, *args, **kwargs):
        import os
        import platform

        # Check if we're in CI on ARM Mac with Python 3.12
        is_ci = os.environ.get("GITHUB_ACTIONS", "false").lower() == "true"
        is_arm_mac = platform.machine() == "arm64" and platform.system() == "Darwin"
        is_py312 = sys.version_info[:2] == (3, 12)

        should_analyze = is_ci and is_arm_mac and is_py312

        # Debug print to verify conditions
        if is_ci:
            print(
                f"\n[NaN Analysis] CI={is_ci}, ARM={is_arm_mac}, PY312={is_py312}, platform={platform.machine()}"
            )

        if should_analyze:
            print(f"\n{'=' * 60}")
            print(f"DAILYSTATE NaN ANALYSIS (cp312 ARM Mac)")
            print(f"Test: {func.__name__}")
            print(f"Time: {datetime.now()}")
            print(f"{'=' * 60}")

        # Run the test
        result = func(self, *args, **kwargs)

        # After test passes, analyze DailyState if available
        if should_analyze:
            try:
                # Try to find df_output in the test instance or locals
                df_output = None

                # Check instance attributes
                if hasattr(self, "df_output"):
                    df_output = self.df_output
                else:
                    # Try to access from function's local variables
                    # This is a bit hacky but useful for debugging
                    import inspect

                    frame = inspect.currentframe()
                    if frame and frame.f_back and frame.f_back.f_locals:
                        df_output = frame.f_back.f_locals.get("df_output")

                if (
                    df_output is not None
                    and "DailyState"
                    in df_output.columns.get_level_values("group").unique()
                ):
                    df_dailystate = df_output.loc[:, "DailyState"]

                    print(f"\nDailyState shape: {df_dailystate.shape}")
                    print(f"Total values: {df_dailystate.size}")
                    print(f"Non-NaN values: {df_dailystate.notna().sum().sum()}")
                    print(
                        f"NaN percentage: {(df_dailystate.isna().sum().sum() / df_dailystate.size * 100):.2f}%"
                    )

                    # Analyze NaN patterns by column
                    col_nan_counts = df_dailystate.isna().sum()
                    col_nan_pct = (col_nan_counts / len(df_dailystate) * 100).round(2)

                    # Group columns by NaN pattern
                    always_nan = col_nan_counts[col_nan_counts == len(df_dailystate)]
                    never_nan = col_nan_counts[col_nan_counts == 0]
                    sometimes_nan = col_nan_counts[
                        (col_nan_counts > 0) & (col_nan_counts < len(df_dailystate))
                    ]

                    if len(always_nan) > 0:
                        print(f"\nColumns ALWAYS NaN ({len(always_nan)}):")
                        for col in always_nan.index[:10]:
                            print(f"  - {col}")
                        if len(always_nan) > 10:
                            print(f"  ... and {len(always_nan) - 10} more")

                    if len(never_nan) > 0:
                        print(f"\nColumns NEVER NaN ({len(never_nan)}):")
                        for col in never_nan.index[:10]:
                            print(f"  - {col}")
                        if len(never_nan) > 10:
                            print(f"  ... and {len(never_nan) - 10} more")

                    if len(sometimes_nan) > 0:
                        print(f"\nColumns with MIXED NaN ({len(sometimes_nan)}):")
                        for col in sometimes_nan.index:
                            nan_pct = col_nan_pct[col]
                            print(f"  - {col}: {nan_pct}% NaN")

                    # Analyze temporal pattern
                    rows_with_any_data = ~df_dailystate.isna().all(axis=1)
                    if rows_with_any_data.any():
                        indices_with_data = df_dailystate.index[rows_with_any_data]
                        print(f"\nTemporal pattern:")
                        print(
                            f"  Rows with data: {len(indices_with_data)} out of {len(df_dailystate)}"
                        )
                        print(f"  First data at: {indices_with_data[0]}")
                        print(f"  Last data at: {indices_with_data[-1]}")

                        # Check if it's truly daily (end of day pattern)
                        if len(indices_with_data) > 1:
                            time_diffs = pd.Series(indices_with_data).diff().dropna()
                            unique_diffs = time_diffs.value_counts()
                            print(f"  Time intervals between data points:")
                            for diff, count in unique_diffs.head(3).items():
                                print(f"    {diff}: {count} occurrences")

                    # After resampling analysis
                    if (
                        hasattr(self, "df_resampled")
                        or "df_resampled" in frame.f_back.f_locals
                    ):
                        df_resampled = getattr(
                            self, "df_resampled", None
                        ) or frame.f_back.f_locals.get("df_resampled")
                        if (
                            df_resampled is not None
                            and "DailyState"
                            in df_resampled.columns.get_level_values("group").unique()
                        ):
                            df_dailystate_resampled = df_resampled.loc[:, "DailyState"]
                            print(f"\nAfter resampling:")
                            print(f"  Shape: {df_dailystate_resampled.shape}")
                            print(
                                f"  Non-NaN values: {df_dailystate_resampled.notna().sum().sum()}"
                            )
                            print(
                                f"  NaN percentage: {(df_dailystate_resampled.isna().sum().sum() / df_dailystate_resampled.size * 100):.2f}%"
                            )

                else:
                    print("\nNo DailyState data found in output")

            except Exception as e:
                print(
                    f"\nError during DailyState analysis: {type(e).__name__}: {str(e)}"
                )
                import traceback

                traceback.print_exc()

            print(f"\n{'=' * 60}")
            print(f"Test {func.__name__} completed successfully")
            print(f"{'=' * 60}\n")

        return result

    return wrapper
