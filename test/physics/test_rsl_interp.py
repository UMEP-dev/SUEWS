#!/usr/bin/env python3
"""Test for RSL interpolation fix for short buildings."""

import supy as sp
import pandas as pd
import numpy as np


def test_rsl_short_building():
    """Test RSL with a short building (10m) to ensure no interpolation errors."""

    # Load test data
    df_state_init, df_forcing = sp.load_SampleData()

    # Set up a configuration with short buildings
    df_state_init.loc[:, "bldgh"] = 10.0  # 10m building height
    df_state_init.loc[:, "evetreeh"] = 8.0  # Trees shorter than buildings
    df_state_init.loc[:, "dectreeh"] = 8.0

    # Configure to use RSL
    df_state_init.loc[:, "stabilitymethod"] = 2  # Use RSL
    df_state_init.loc[:, "diagmethod"] = 0  # Use MOST (triggers RSL_flag=False)

    # Run the model - this should not crash with the fix
    try:
        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[
                :288
            ],  # Run for 24 hours (288 timesteps at 5-min intervals)
            df_state_init,
        )

        # Check that we got reasonable output
        assert not df_output.empty
        assert "SUEWS" in df_output.columns.get_level_values(0)

        # Check that T2, RH2, U10 diagnostics are reasonable
        t2_values = df_output["SUEWS"]["T2"].values
        rh2_values = df_output["SUEWS"]["RH2"].values
        u10_values = df_output["SUEWS"]["U10"].values

        assert np.all(np.isfinite(t2_values)), "T2 contains non-finite values"
        assert np.all(np.isfinite(rh2_values)), "RH2 contains non-finite values"
        assert np.all(np.isfinite(u10_values)), "U10 contains non-finite values"

        # Basic sanity checks
        assert np.all(t2_values > -50) and np.all(t2_values < 60), (
            "T2 outside reasonable range"
        )
        assert np.all(rh2_values >= 0) and np.all(rh2_values <= 105), (
            "RH2 outside reasonable range"
        )
        assert np.all(u10_values >= 0) and np.all(u10_values < 100), (
            "U10 outside reasonable range"
        )

        print("✓ RSL interpolation test passed for 10m building")

    except Exception as e:
        raise AssertionError(f"RSL interpolation failed for short building: {str(e)}")


def test_rsl_very_short_building():
    """Test RSL with a very short building (2m) - extreme case."""

    # Load test data
    df_state_init, df_forcing = sp.load_SampleData()

    # Set up a configuration with very short buildings
    df_state_init.loc[:, "bldgh"] = (
        2.0  # 2m building height (same as diagnostic height)
    )
    df_state_init.loc[:, "evetreeh"] = 1.5
    df_state_init.loc[:, "dectreeh"] = 1.5

    # Configure to use RSL
    df_state_init.loc[:, "stabilitymethod"] = 2  # Use RSL
    df_state_init.loc[:, "diagmethod"] = 0  # Use MOST

    # Run the model - this should not crash with the fix
    try:
        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[
                :288
            ],  # Run for 24 hours (288 timesteps at 5-min intervals)
            df_state_init,
        )

        # Check that we got reasonable output
        assert not df_output.empty
        assert "SUEWS" in df_output.columns.get_level_values(0)

        print("✓ RSL interpolation test passed for 2m building")

    except Exception as e:
        raise AssertionError(
            f"RSL interpolation failed for very short building: {str(e)}"
        )


def test_rsl_tall_building():
    """Test RSL with tall buildings as a control case."""

    # Load test data
    df_state_init, df_forcing = sp.load_SampleData()

    # Set up a configuration with tall buildings
    df_state_init.loc[:, "bldgh"] = 50.0  # 50m building height

    # Configure to use RSL
    df_state_init.loc[:, "stabilitymethod"] = 2  # Use RSL
    df_state_init.loc[:, "diagmethod"] = 0  # Use MOST

    # Run the model - this should work fine
    try:
        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[
                :288
            ],  # Run for 24 hours (288 timesteps at 5-min intervals)
            df_state_init,
        )

        # Check that we got reasonable output
        assert not df_output.empty
        assert "SUEWS" in df_output.columns.get_level_values(0)

        print("✓ RSL interpolation test passed for 50m building (control)")

    except Exception as e:
        raise AssertionError(f"RSL interpolation failed for tall building: {str(e)}")


if __name__ == "__main__":
    # Run the tests
    print("Testing RSL interpolation fix...")
    test_rsl_short_building()
    test_rsl_very_short_building()
    test_rsl_tall_building()
    print("\nAll RSL interpolation tests passed! ✓")
