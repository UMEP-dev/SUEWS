"""Test to reproduce and verify fix for negative wind speeds in MOST profiles (Issue #572)"""

import numpy as np
import pandas as pd
import supy as sp
import pytest


def test_wind_speeds_always_positive_most_profile():
    """Test that MOST profile calculations always produce positive wind speeds.
    
    This test verifies that wind speeds remain positive across various building
    configurations when using MOST diagnostics (diagmethod=0), particularly for
    cases that previously caused negative values due to the MIN constraint issue.
    """

    # Load sample data
    df_state_init, df_forcing = sp.load_SampleData()

    # Force MOST method (no RSL)
    df_state_init.loc[:, "diagmethod"] = 0
    df_state_init.loc[:, "stabilitymethod"] = 2  # Use method 2 as in issue report

    # Test with various displacement heights and roughness lengths that might cause issues
    test_cases = [
        {"zh": 2.0, "z0m_in": 0.1, "zdm_in": 0.1},  # Very low buildings
        {"zh": 5.0, "z0m_in": 0.5, "zdm_in": 3.5},  # zdm > building height scenario
        {"zh": 10.0, "z0m_in": 1.0, "zdm_in": 7.0},  # Standard case
        {"zh": 1.0, "z0m_in": 0.01, "zdm_in": 0.01},  # Extremely low values
        {"zh": 2.0, "z0m_in": 0.01, "zdm_in": 0.01},  # Original test case
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

        # Check standard SUEWS output wind speeds
        if "SUEWS" in df_output.columns.get_level_values(0):
            suews_data = df_output.xs("SUEWS", level=0, axis=1)
            if "U10" in suews_data.columns:
                u10 = suews_data["U10"]
                # Filter out NaN values for the check
                u10_valid = u10.dropna()
                if len(u10_valid) > 0:
                    assert (u10_valid >= 0).all(), (
                        f"U10 wind speeds should always be positive for case {case}, "
                        f"but found min: {u10_valid.min()}"
                    )

        # Check RSL output if it exists
        if "RSL" in df_output.columns.get_level_values(1):
            # Extract RSL wind speed profiles
            rsl_data = df_output.xs("RSL", level=1, axis=1)

            # Find wind speed columns (they start with 'U')
            wind_cols = [col for col in rsl_data.columns if col.startswith("U")]

            if wind_cols:
                wind_speeds = rsl_data[wind_cols]

                # Check for negative values
                negative_mask = wind_speeds < 0
                n_negative = negative_mask.sum().sum()

                if n_negative > 0:
                    # Find where negatives occur
                    negative_info = []
                    for col in wind_cols:
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


if __name__ == "__main__":
    # Run tests directly
    test_wind_speeds_always_positive_most_profile()
    print("All tests passed!")