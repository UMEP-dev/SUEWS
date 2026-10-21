"""Check the ground budget through the supported simulation interface."""

from conftest import load_sample_frames, run_simulation
import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]

SURFACES = ["Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water"]
GROUND = [0, 2, 3, 4, 5, 6]


def run_surface_case(direct_albedo=False, storage_heat=1):
    """Six daytime timesteps with SPARTACUS and the selected storage scheme."""
    state, forcing = load_sample_frames()
    state = state.iloc[[0]].copy()
    state.loc[:, ("netradiationmethod", "0")] = 1003
    state.loc[:, ("storageheatmethod", "0")] = storage_heat
    # Since #1858 this flag reaches the kernel; the test below checks it.
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
    vegetation_sw = output["SPARTACUS"][[f"KVegAbs_{i}" for i in range(1, 4)]].sum(axis=1)
    sw_ground = qn_ground @ fractions[GROUND] - output["SPARTACUS", "LNet_Grnd"]
    np.testing.assert_allclose(
        sw_ground, output["SPARTACUS", "KNet_Grnd"] + vegetation_sw, rtol=0, atol=1e-8,
        err_msg="Surface absorption must conserve the solver's ground plus canopy shortwave budget",
    )
    building_budget = (
        output["SPARTACUS", "Qn"]
        - output["SPARTACUS", "KNet_Grnd"]
        - output["SPARTACUS", "LNet_Grnd"]
        - vegetation_sw
    ) / fractions[1]
    np.testing.assert_allclose(
        output["SUEWS", "QN_Bldgs"], building_budget, rtol=0, atol=1e-8,
    )


@pytest.mark.parametrize("storage_heat", [6, 8])
def test_spartacus_dyohm_surface_energy_budget(storage_heat):
    output, state, _ = run_surface_case(storage_heat=storage_heat)
    fractions = state["sfr_surf"].iloc[0].to_numpy()
    fluxes = output["SUEWS"]
    columns = [f"{flux}_{surface}" for flux in ["QN", "QS"] for surface in SURFACES]
    assert np.isfinite(fluxes[columns].to_numpy()).all()
    turbulent = output["debug"]
    assert np.isfinite(turbulent[[f"{flux}_{s}" for flux in ["QH", "QE"] for s in SURFACES]].to_numpy()).all()
    for flux in ["QN", "QS"]:
        weighted = fluxes[[f"{flux}_{surface}" for surface in SURFACES]].to_numpy() @ fractions
        np.testing.assert_allclose(weighted, fluxes[flux], rtol=0, atol=1e-6)
    # All active surface energy balances include the common anthropogenic
    # heat source. Test the coupled turbulent/storage handoff, not just QN.
    for i, surface in enumerate(SURFACES):
        if fractions[i] == 0:
            continue
        residual = (fluxes[f"QN_{surface}"] + fluxes["QF"] - fluxes[f"QS_{surface}"]
                    - turbulent[f"QH_{surface}"] - turbulent[f"QE_{surface}"])
        np.testing.assert_allclose(residual, 0, rtol=0, atol=1e-6)
    # DyOHM must actively change storage relative to ordinary OHM.
    ohm, _, _ = run_surface_case()
    assert np.max(np.abs(fluxes["QS"] - ohm["SUEWS", "QS"])) > 1e-3


def test_direct_albedo_flag_reaches_the_kernel():
    """A direct-albedo multiplier must change the ground shortwave the run reports."""
    diffuse_only, _, _ = run_surface_case(direct_albedo=False)
    with_direct, _, _ = run_surface_case(direct_albedo=True)
    difference = (
        with_direct["SPARTACUS", "KNet_Grnd"] - diffuse_only["SPARTACUS", "KNet_Grnd"]
    ).abs()
    assert difference.max() > 1e-3, "use_sw_direct_albedo had no effect on ground shortwave"
