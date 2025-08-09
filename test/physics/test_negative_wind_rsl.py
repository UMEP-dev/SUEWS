"""Test to reproduce negative wind speeds in RSL output (Issue #572)"""

import numpy as np
import pandas as pd
import supy as sp
import pytest


def test_no_negative_wind_speeds_with_rsl_disabled():
    """Test that wind speeds are never negative when RSL is disabled (rslmethod=0)"""

    # Load sample data
    df_state_init, df_forcing = sp.load_SampleData()

    # Create a grid with RSL disabled but potentially problematic parameters
    # Set RSL method to 0 (MOST only, no RSL)
    df_state_init.loc[:, "rslmethod"] = 0

    # Test with various displacement heights and roughness lengths that might cause issues
    test_cases = [
        {"zh": 2.0, "z0m_in": 0.1, "zdm_in": 0.1},  # Very low buildings
        {"zh": 5.0, "z0m_in": 0.5, "zdm_in": 3.5},  # zdm > building height scenario
        {"zh": 10.0, "z0m_in": 1.0, "zdm_in": 7.0},  # Standard case
        {"zh": 1.0, "z0m_in": 0.01, "zdm_in": 0.01},  # Extremely low values
    ]

    for case in test_cases:
        # Update grid with test parameters
        grid_test = df_state_init.copy()
        grid_test.loc[:, "zh"] = case["zh"]
        grid_test.loc[:, "z0m_in"] = case["z0m_in"]
        grid_test.loc[:, "zdm_in"] = case["zdm_in"]

        # Run simulation for a short period
        df_forcing_short = df_forcing.iloc[:48]  # 48 timesteps (1 day with 30min data)

        # Run the model
        df_output, df_state_final = sp.run_supy(
            df_forcing_short, grid_test, save_state=False
        )

        # Check RSL output if it exists
        if "RSL" in df_output.columns.get_level_values(1):
            # Extract RSL wind speed profiles
            rsl_data = df_output.xs("RSL", level=1, axis=1)

            # RSL columns are structured as: z1, z2, ..., z30, U1, U2, ..., U30, T1, T2, ..., T30, q1, q2, ..., q30, ...
            # Wind speeds are in columns 31-60 (assuming 30 vertical levels)
            n_levels = 30
            wind_columns = [f"U{i}" for i in range(1, n_levels + 1)]

            # Check if wind speed columns exist
            wind_cols_present = [col for col in wind_columns if col in rsl_data.columns]

            if wind_cols_present:
                wind_speeds = rsl_data[wind_cols_present]

                # Check for negative values
                negative_mask = wind_speeds < 0
                n_negative = negative_mask.sum().sum()

                if n_negative > 0:
                    # Find where negatives occur
                    negative_info = []
                    for col in wind_cols_present:
                        neg_in_col = negative_mask[col]
                        if neg_in_col.any():
                            min_val = wind_speeds.loc[neg_in_col, col].min()
                            negative_info.append(f"{col}: min={min_val:.3f}")

                    pytest.fail(
                        f"Negative wind speeds found with zh={case['zh']}, z0m={case['z0m_in']}, zdm={case['zdm_in']}:\n"
                        f"  Total negative values: {n_negative}\n"
                        f"  Affected levels: {', '.join(negative_info)}"
                    )

                # Also check for NaN values
                nan_mask = wind_speeds.isna()
                n_nan = nan_mask.sum().sum()

                if n_nan > 0:
                    pytest.fail(
                        f"NaN wind speeds found with zh={case['zh']}, z0m={case['z0m_in']}, zdm={case['zdm_in']}:\n"
                        f"  Total NaN values: {n_nan}"
                    )


def test_wind_speeds_always_positive_most_profile():
    """Test that MOST profile calculations always produce positive wind speeds"""

    # Load sample data
    df_state_init, df_forcing = sp.load_SampleData()

    # Force MOST method (no RSL)
    df_state_init.loc[:, "rslmethod"] = 0
    df_state_init.loc[:, "stabilitymethod"] = 2  # Use method 2 as in issue report

    # Set very small displacement and roughness heights (potential for negative winds)
    df_state_init.loc[:, "zh"] = 2.0
    df_state_init.loc[:, "z0m_in"] = 0.01
    df_state_init.loc[:, "zdm_in"] = 0.01

    # Run for a full day
    df_forcing_day = df_forcing.iloc[:48]

    df_output, df_state_final = sp.run_supy(
        df_forcing_day, df_state_init, save_state=False
    )

    # Check standard output wind speeds
    if "SUEWS" in df_output.columns.get_level_values(0):
        suews_data = df_output.xs("SUEWS", level=0, axis=1)
        if "U10" in suews_data.columns:
            u10 = suews_data["U10"]
            # Filter out NaN values for the check
            u10_valid = u10.dropna()
            if len(u10_valid) > 0:
                assert (u10_valid >= 0).all(), (
                    f"U10 wind speeds should always be positive, but found min: {u10_valid.min()}"
                )
            # Note: If all values are NaN, that might indicate a different issue
            # but it's not a negative wind speed issue

    # Check RSL output if available
    if "RSL" in df_output.columns.get_level_values(1):
        rsl_data = df_output.xs("RSL", level=1, axis=1)

        # Find wind speed columns (they start with 'U')
        wind_cols = [col for col in rsl_data.columns if col.startswith("U")]

        if wind_cols:
            wind_speeds = rsl_data[wind_cols]

            # Check all wind speeds are non-negative
            min_wind = wind_speeds.min().min()
            assert min_wind >= 0, (
                f"All RSL wind speeds should be non-negative, but found minimum: {min_wind:.6f}"
            )

            # Check no NaN values
            assert not wind_speeds.isna().any().any(), (
                "RSL wind speeds should not contain NaN values"
            )


if __name__ == "__main__":
    # Run tests directly
    test_no_negative_wind_speeds_with_rsl_disabled()
    test_wind_speeds_always_positive_most_profile()
    print("All tests passed!")
