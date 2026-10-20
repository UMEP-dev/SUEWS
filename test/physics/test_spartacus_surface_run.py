"""Check the ground budget through the supported simulation interface."""

from conftest import load_sample_frames, run_simulation
import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]

SURFACES = ["Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water"]
GROUND = [0, 2, 3, 4, 5, 6]


def run_surface_case(direct_albedo=False):
    """Six daytime timesteps, SPARTACUS radiation and OHM storage."""
    state, forcing = load_sample_frames()
    state = state.iloc[[0]].copy()
    state.loc[:, ("netradiationmethod", "0")] = 1003
    state.loc[:, ("storageheatmethod", "0")] = 1
    state.loc[:, ("use_sw_direct_albedo", "0")] = float(direct_albedo)
    state.loc[:, ("ground_albedo_dir_mult_fact", "0")] = 1.4 if direct_albedo else 1.0
    # Keep the canopy and land-cover ground areas identical.
    state.loc[:, ("building_frac", "(0,)")] = state[("sfr_surf", "(1,)")].iloc[0]
    # The sample's highest layer is above both crowns.
    state.loc[:, ("veg_frac", "(2,)")] = 0.0
    state.loc[:, ("veg_scale", "(2,)")] = 0.0
    first = np.flatnonzero(forcing["kdown"].to_numpy() > 100)[0]
    forcing = forcing.iloc[first:first + 6].copy()
    output, final = run_simulation(forcing, state, serial_mode=True)
    return output, state, final


@pytest.mark.parametrize("direct_albedo", [False, True])
def test_simulated_surface_ground_budget(direct_albedo):
    output, state, _ = run_surface_case(direct_albedo)
    fractions = state["sfr_surf"].iloc[0].to_numpy()
    qn_ground = output["SUEWS"][[f"QN_{SURFACES[i]}" for i in GROUND]].to_numpy()
    sw_ground = qn_ground @ fractions[GROUND] - output["SPARTACUS", "LNet_Grnd"]
    np.testing.assert_allclose(
        sw_ground, output["SPARTACUS", "KNet_Grnd"], rtol=0, atol=1e-8,
        err_msg="Surface absorption must conserve the solver's ground shortwave budget",
    )
    building_budget = (
        output["SPARTACUS", "Qn"]
        - output["SPARTACUS", "KNet_Grnd"]
        - output["SPARTACUS", "LNet_Grnd"]
    ) / fractions[1]
    np.testing.assert_allclose(
        output["SUEWS", "QN_Bldgs"], building_budget, rtol=0, atol=1e-8,
    )
