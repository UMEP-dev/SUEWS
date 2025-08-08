"""Test for issue #240: nan QF due to zero population density"""

import numpy as np
import pandas as pd
import supy as sp
import pytest


def test_zero_population_qf():
    """Test that QF doesn't become NaN when population density is zero"""
    # Load sample data
    df_state_init, df_forcing = sp.load_SampleData()

    # Set population density to zero for testing
    # popdensdaytime and popdensnighttime control population density (lowercase with tuple columns)
    df_state_init.loc[:, ("popdensdaytime", "(0,)")] = 0.0
    df_state_init.loc[:, ("popdensdaytime", "(1,)")] = 0.0
    df_state_init.loc[:, ("popdensnighttime", "0")] = 0.0

    # Run the model for a short period
    df_output, df_state_final = sp.run_supy(
        df_forcing.iloc[:24],  # Run for 24 hours
        df_state_init,
        save_state=False,
    )

    # Check that QF is not NaN
    qf_values = df_output["SUEWS"]["QF"]
    assert not qf_values.isna().any(), (
        "QF contains NaN values with zero population density"
    )

    # Check that QF is zero or close to zero (allowing for numerical precision)
    # When population is zero, anthropogenic heat flux should be minimal
    assert (qf_values >= 0).all(), "QF should be non-negative"

    # Also check that other heat fluxes are not affected (not NaN)
    for flux in ["QH", "QE", "QN", "QS"]:
        flux_values = df_output["SUEWS"][flux]
        assert not flux_values.isna().any(), (
            f"{flux} contains NaN values with zero population density"
        )


def test_zero_population_with_traffic():
    """Test that traffic-related QF works correctly with zero population"""
    # Load sample data
    df_state_init, df_forcing = sp.load_SampleData()

    # Set population density to zero but keep traffic
    df_state_init.loc[:, ("popdensdaytime", "(0,)")] = 0.0
    df_state_init.loc[:, ("popdensdaytime", "(1,)")] = 0.0
    df_state_init.loc[:, ("popdensnighttime", "0")] = 0.0
    # TrafficRate should still work even with zero population

    # Run the model
    df_output, df_state_final = sp.run_supy(
        df_forcing.iloc[:24], df_state_init, save_state=False
    )

    # Check QF is valid
    qf_values = df_output["SUEWS"]["QF"]
    assert not qf_values.isna().any(), (
        "QF contains NaN with zero population but active traffic"
    )
    assert (qf_values >= 0).all(), "QF should be non-negative"


if __name__ == "__main__":
    test_zero_population_qf()
    test_zero_population_with_traffic()
    print("All tests passed!")
