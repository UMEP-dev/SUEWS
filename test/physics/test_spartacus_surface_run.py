"""Check the ground budget through the supported simulation interface."""

from conftest import load_sample_frames, run_simulation
import numpy as np
import pytest

from supy.data_model import SUEWSConfig

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-BALANCE", "C-BIND"),
]

SURFACES = ["Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water"]
GROUND = [0, 2, 3, 4, 5, 6]


def run_surface_case(direct_albedo=False, storage_heat=5, ehc_surface_coupling=None):
    """Six daytime timesteps with SPARTACUS and the selected storage scheme."""
    state, forcing = load_sample_frames()
    state = state.iloc[[0]].copy()
    state.loc[:, ("netradiationmethod", "0")] = 1003
    state.loc[:, ("storageheatmethod", "0")] = storage_heat
    if ehc_surface_coupling is not None:
        state.loc[:, ("ehc_surface_coupling", "0")] = ehc_surface_coupling
    # Since #1858 this flag reaches the kernel; the test below checks it.
    state.loc[:, ("use_sw_direct_albedo", "0")] = float(direct_albedo)
    state.loc[:, ("ground_albedo_dir_mult_fact", "0")] = 1.4 if direct_albedo else 1.0
    # Keep the canopy and land-cover ground areas identical.
    state.loc[:, ("building_frac", "(0,)")] = state[("sfr_surf", "(1,)")].iloc[0]
    # The sample's highest layer is above both crowns.
    state.loc[:, ("veg_frac", "(2,)")] = 0.0
    state.loc[:, ("veg_scale", "(2,)")] = 0.0
    first = np.flatnonzero(forcing["kdown"].to_numpy() > 100)[0]
    forcing = forcing.iloc[first : first + 6].copy()
    output, final = run_simulation(forcing, state, serial_mode=True)
    return output, state, final


@pytest.mark.parametrize("direct_albedo", [False, True])
def test_simulated_surface_ground_budget(direct_albedo):
    output, state, _ = run_surface_case(direct_albedo)
    fractions = state["sfr_surf"].iloc[0].to_numpy()
    qn_ground = output["SUEWS"][[f"QN_{SURFACES[i]}" for i in GROUND]].to_numpy()
    vegetation_sw = output["SPARTACUS"][[f"KVegAbs_{i}" for i in range(1, 4)]].sum(
        axis=1
    )
    sw_ground = qn_ground @ fractions[GROUND] - output["SPARTACUS", "LNet_Grnd"]
    np.testing.assert_allclose(
        sw_ground,
        output["SPARTACUS", "KNet_Grnd"] + vegetation_sw,
        rtol=0,
        atol=1e-8,
        err_msg="Surface absorption must conserve the solver's ground plus canopy shortwave budget",
    )
    building_budget = (
        output["SPARTACUS", "Qn"]
        - output["SPARTACUS", "KNet_Grnd"]
        - output["SPARTACUS", "LNet_Grnd"]
        - vegetation_sw
    ) / fractions[1]
    np.testing.assert_allclose(
        output["SUEWS", "QN_Bldgs"],
        building_budget,
        rtol=0,
        atol=1e-8,
    )


@pytest.mark.parametrize(
    ("ehc_surface_coupling", "ground_includes_qf"),
    [
        pytest.param(None, False, id="default_implicit"),
        pytest.param(0, True, id="explicit"),
    ],
)
def test_spartacus_ehc_surface_energy_budget(ehc_surface_coupling, ground_includes_qf):
    """SPARTACUS with buildings and supported EHC closes the per-surface energy budget."""
    output, state, _ = run_surface_case(
        storage_heat=5,
        ehc_surface_coupling=ehc_surface_coupling,
    )
    fractions = state["sfr_surf"].iloc[0].to_numpy()
    fluxes = output["SUEWS"]
    columns = [f"{flux}_{surface}" for flux in ["QN", "QS"] for surface in SURFACES]
    assert np.isfinite(fluxes[columns].to_numpy()).all()
    turbulent = output["debug"]
    assert np.isfinite(
        turbulent[[f"{flux}_{s}" for flux in ["QH", "QE"] for s in SURFACES]].to_numpy()
    ).all()
    for flux in ["QN", "QS"]:
        weighted = (
            fluxes[[f"{flux}_{surface}" for surface in SURFACES]].to_numpy() @ fractions
        )
        np.testing.assert_allclose(weighted, fluxes[flux], rtol=0, atol=1e-6)
    # Grid-scale closure holds with QF included in both implicit and explicit coupling.
    grid_residual = (
        fluxes["QN"] + fluxes["QF"] - fluxes["QS"] - fluxes["QH"] - fluxes["QE"]
    )
    np.testing.assert_allclose(grid_residual, 0, rtol=0, atol=1e-6)
    # Under default implicit EHC, QF is not distributed onto ground facets
    # (QF - QIndoor enters canyon air and grid-scale QH; suews_ctrl_driver.f95).
    # Under explicit EHC (ehc_surface_coupling=0), QF is still added per ground facet.
    # Under SPARTACUS + EHC, QN_Bldgs holds the SPARTACUS residual (including
    # canopy LW and air absorption; suews_phys_spartacus.f95), whereas QH_Bldgs
    # aggregates pure roof/wall facet balances without QF (suews_ctrl_driver.f95).
    qf_ground = fluxes["QF"] if ground_includes_qf else 0.0
    for i in GROUND:
        if fractions[i] == 0:
            continue
        surface = SURFACES[i]
        residual = (
            fluxes[f"QN_{surface}"]
            + qf_ground
            - fluxes[f"QS_{surface}"]
            - turbulent[f"QH_{surface}"]
            - turbulent[f"QE_{surface}"]
        )
        np.testing.assert_allclose(residual, 0, rtol=0, atol=1e-6)


@pytest.mark.parametrize("storage_heat", [1, 6, 8])
def test_spartacus_with_buildings_refuses_non_facet_storage_heat(storage_heat):
    """Both from_state() and from_dict() reject non-facet storage heat with SPARTACUS+buildings (gh#1834)."""
    with pytest.raises(ValueError, match="SPARTACUS with buildings"):
        run_surface_case(storage_heat=storage_heat)

    state, _ = load_sample_frames()
    cfg_dict = SUEWSConfig.from_df_state(state.iloc[[0]]).model_dump(
        exclude_none=True, mode="json"
    )
    cfg_dict["model"]["physics"]["net_radiation"] = {"value": 1003}
    cfg_dict["model"]["physics"]["storage_heat"] = {"value": storage_heat}
    layers = cfg_dict["sites"][0]["properties"]["vertical_layers"]
    for key in ("veg_frac", "veg_scale"):
        target = layers[key]["value"] if isinstance(layers[key], dict) else layers[key]
        target[2] = 0.0
    with pytest.raises(ValueError, match="SPARTACUS with buildings"):
        SUEWSConfig.from_dict(cfg_dict)


def test_direct_albedo_flag_reaches_the_kernel():
    """A direct-albedo multiplier must change the ground shortwave the run reports."""
    diffuse_only, _, _ = run_surface_case(direct_albedo=False)
    with_direct, _, _ = run_surface_case(direct_albedo=True)
    difference = (
        with_direct["SPARTACUS", "KNet_Grnd"] - diffuse_only["SPARTACUS", "KNet_Grnd"]
    ).abs()
    assert difference.max() > 1e-3, "use_sw_direct_albedo had no effect on ground shortwave"
