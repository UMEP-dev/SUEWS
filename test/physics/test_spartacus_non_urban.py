"""Regression tests for non-urban SPARTACUS paths."""

from conftest import load_sample_frames, run_simulation
import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]


def _forest_only_direct_albedo_state(df_state_init):
    df_state = df_state_init.iloc[[0]].copy()

    for idx in range(7):
        df_state.loc[:, ("sfr_surf", f"({idx},)")] = 0.0
    df_state.loc[:, ("sfr_surf", "(2,)")] = 1.0

    for idx in range(3):
        df_state.loc[:, ("building_frac", f"({idx},)")] = 0.0
        df_state.loc[:, ("building_scale", f"({idx},)")] = 0.0
        df_state.loc[:, ("veg_frac", f"({idx},)")] = 0.0
    df_state.loc[:, ("veg_frac", "(0,)")] = 1.0

    df_state.loc[:, ("bldgh", "0")] = 0.0
    df_state.loc[:, ("faibldg", "0")] = 0.0
    df_state.loc[:, ("evetreeh", "0")] = 13.1
    df_state.loc[:, ("faievetree", "0")] = 0.3
    df_state.loc[:, ("dectreeh", "0")] = 0.0
    df_state.loc[:, ("faidectree", "0")] = 0.0

    df_state.loc[:, ("netradiationmethod", "0")] = 1001
    df_state.loc[:, ("use_sw_direct_albedo", "0")] = 1.0

    return df_state


@pytest.mark.parametrize("air_ext_lw", [0.0, 0.01])
@pytest.mark.parametrize("with_trees", [True, False], ids=["forest", "flat"])
def test_non_urban_spartacus_direct_albedo_runs_without_urban_arrays(
    air_ext_lw, with_trees
):
    """Zero building geometry must work with and without trees (GH#1498)."""
    df_state_init, df_forcing = load_sample_frames()
    df_state = _forest_only_direct_albedo_state(df_state_init)
    df_state.loc[:, ("air_ext_lw", "0")] = air_ext_lw
    if not with_trees:
        df_state.loc[:, ("sfr_surf", "(2,)")] = 0.0
        df_state.loc[:, ("sfr_surf", "(4,)")] = 1.0
        df_state.loc[:, "veg_frac"] = 0.0
        df_state.loc[:, "veg_scale"] = 0.0
        df_state.loc[:, ("evetreeh", "0")] = 0.0
        df_state.loc[:, ("faievetree", "0")] = 0.0
    df_forcing_daytime = df_forcing.loc[df_forcing["kdown"] > 100].iloc[:2]
    # Method 1001 uses observed longwave; the sample has missing ldown.
    df_forcing_daytime = df_forcing_daytime.copy()
    df_forcing_daytime["ldown"] = 300.0

    df_output, _ = run_simulation(
        df_forcing_daytime,
        df_state,
        validate_forcing=False,
        serial_mode=True,
    )

    assert "SPARTACUS" in df_output.columns.get_level_values(0)

    spartacus = df_output.xs("SPARTACUS", level=0, axis=1)
    fluxes = spartacus[
        [
            "KTopDnDir",
            "KTopDnDif",
            "Kup",
            "Lup",
            "KNet_Grnd",
            "LUp_Grnd",
            "LTopDn",
            "LTopNet",
            "LNet_Grnd",
        ]
    ]

    assert len(spartacus) == len(df_forcing_daytime)
    assert (spartacus["KTopDnDir"] > 0).any()
    assert np.isfinite(fluxes.to_numpy()).all()
    assert np.isfinite(df_output["SUEWS", "QN"]).all()
    assert not np.isclose(fluxes.to_numpy(), -999.0).any()
    np.testing.assert_allclose(spartacus["LTopDn"], 300.0)
    # Absent facets contribute zero at every configured layer.
    for facet in ("Roof", "Wall"):
        for band in ("K", "L"):
            for quantity in ("In", "Net"):
                columns = [f"{band}{quantity}_{facet}_{layer}" for layer in range(1, 4)]
                np.testing.assert_allclose(spartacus[columns], 0.0, atol=1e-12)
    if not with_trees:
        # A flat tile has no canopy absorption; budgets agree to round-off.
        np.testing.assert_allclose(
            spartacus["LTopNet"], spartacus["LNet_Grnd"], rtol=0, atol=1e-10
        )
        np.testing.assert_allclose(
            spartacus["KTopNet"], spartacus["KNet_Grnd"], rtol=0, atol=1e-10
        )
