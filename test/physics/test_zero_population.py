"""Test for issue #240: nan QF due to zero population density"""

import pytest
import supy as sp


@pytest.mark.parametrize(
    "scenario",
    [
        "zero_population",
        "zero_population_with_traffic",
    ],
)
def test_zero_population_qf(scenario):
    """Test that QF doesn't become NaN when population density is zero.

    Regression test for issue #240.
    """
    df_state_init, df_forcing = sp.load_SampleData()

    # Set population density to zero
    df_state_init.loc[:, ("popdensdaytime", "(0,)")] = 0.0
    df_state_init.loc[:, ("popdensdaytime", "(1,)")] = 0.0
    df_state_init.loc[:, ("popdensnighttime", "0")] = 0.0

    # Run the model for 24 hours
    df_output, _ = sp.run_supy(
        df_forcing.iloc[:24],
        df_state_init,
        save_state=False,
    )

    # Check QF is valid (not NaN, non-negative)
    qf_values = df_output["SUEWS"]["QF"]
    assert not qf_values.isna().any(), "QF contains NaN with zero population"
    assert (qf_values >= 0).all(), "QF should be non-negative"

    # Check other heat fluxes are not affected
    for flux in ["QH", "QE", "QN", "QS"]:
        flux_values = df_output["SUEWS"][flux]
        assert not flux_values.isna().any(), f"{flux} contains NaN with zero population"
