"""Regression tests for non-urban SPARTACUS paths."""

from conftest import load_sample_frames
import numpy as np
import pytest
from supy import SUEWSSimulation

pytestmark = [pytest.mark.physics, pytest.mark.core]

# SPARTACUS radiation_constants.F90, in W m-2 K-4.
STEFAN_BOLTZMANN = 5.67037321e-8


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
    albedo_diffuse = 0.25
    albedo_direct_factor = df_state[("ground_albedo_dir_mult_fact", "0")].iloc[0]
    emissivity = 0.95
    fraction_direct = 0.6
    if not with_trees:
        df_state.loc[:, ("sfr_surf", "(2,)")] = 0.0
        df_state.loc[:, ("sfr_surf", "(4,)")] = 1.0
        df_state.loc[:, "veg_frac"] = 0.0
        df_state.loc[:, "veg_scale"] = 0.0
        df_state.loc[:, ("evetreeh", "0")] = 0.0
        df_state.loc[:, ("faievetree", "0")] = 0.0
        # Fix grass optical properties independently of seasonal phenology.
        for name in ("albmin_grass", "albmax_grass", "albgrass_id"):
            df_state.loc[:, (name, "0")] = albedo_diffuse
        df_state.loc[:, ("emis", "(4,)")] = emissivity
        df_state.loc[:, ("kdown_split_method", "0")] = 2
        df_state.loc[:, ("sw_dn_direct_frac", "0")] = fraction_direct
    df_forcing_daytime = df_forcing.loc[df_forcing["kdown"] > 100].iloc[:2]
    # Method 1001 uses observed longwave; the sample has missing ldown.
    df_forcing_daytime = df_forcing_daytime.copy()
    df_forcing_daytime["ldown"] = 300.0

    simulation = SUEWSSimulation.from_state(df_state)
    simulation.update_forcing(df_forcing_daytime)
    output = simulation.run(n_jobs=1)
    kernel_warnings = output.kernel_warnings
    spartacus_warnings = kernel_warnings[
        kernel_warnings["location"].str.contains("SPARTACUS", case=False, na=False)
    ]
    assert spartacus_warnings.empty, spartacus_warnings.to_string(index=False)
    df_output = output.df

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
        # No canopy: both boundaries obey the flat-tile radiation laws.
        kdown = df_forcing_daytime["kdown"].to_numpy()
        expected_shortwave = (
            kdown * fraction_direct * (1 - albedo_diffuse * albedo_direct_factor)
            + kdown * (1 - fraction_direct) * (1 - albedo_diffuse)
        )
        # Tsurf is the area-weighted temperature used in radiation, before
        # the subsequent turbulent-flux temperature update. Here it is grass.
        temperature_ground = df_output["SUEWS", "Tsurf"].to_numpy() + 273.15
        expected_longwave = emissivity * (
            df_forcing_daytime["ldown"].to_numpy()
            - STEFAN_BOLTZMANN * temperature_ground**4
        )
        # Shortwave should agree to double-precision round-off.
        for column in ("KTopNet", "KNet_Grnd"):
            np.testing.assert_allclose(
                spartacus[column], expected_shortwave, rtol=0, atol=1e-10
            )
        for column in ("LTopNet", "LNet_Grnd"):
            # The kernel's single-precision 273.15 literal shifts emission
            # by about 3e-5 W m-2 at these temperatures.
            np.testing.assert_allclose(
                spartacus[column], expected_longwave, rtol=0, atol=1e-4
            )
