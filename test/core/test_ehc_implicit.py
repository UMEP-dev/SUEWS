"""Model-level checks of the implicit EHC surface coupling.

`ehc_surface_coupling: implicit` solves each facet's surface temperature together
with its conduction layers (backward Euler, linearised surface energy balance
as the top boundary) instead of iterating a relaxed fixed point on the residual
sensible heat flux. These tests run the short sample with storage_heat 5 and
check what that buys: the iteration ends at the minimum almost always, each
facet's residual sensible heat agrees with the aerodynamic flux implied by its
surface temperature, and a run split by a checkpoint or into chunks
reproduces the continuous run exactly.
"""

import numpy as np
import pandas as pd
import pytest

from supy.data_model import SUEWSConfig
from supy.suews_sim import SUEWSSimulation

pytestmark = [
    pytest.mark.physics,
    pytest.mark.rust,
    pytest.mark.contract(
        "C-BALANCE",
        "C-RESPONSE",
        "C-RESTART",
        "C-EHC-RESTART",
        "C-VALID",
        "C-NATIVE-ERROR",
    ),
]

TIMESTEPS_PER_DAY = 288


def _ehc_sim(short_sample_sim, net_radiation, n_steps, coupling="implicit"):
    sim = short_sample_sim()
    cfg = sim.config.model_dump(exclude_none=True, mode="json")
    cfg["model"]["physics"]["net_radiation"] = {"value": net_radiation}
    cfg["model"]["physics"]["storage_heat"] = {"value": 5}
    cfg["model"]["physics"]["ehc_surface_coupling"] = {"value": coupling}
    if net_radiation > 1000:
        layers = cfg["sites"][0]["properties"]["vertical_layers"]
        layers["veg_frac"]["value"][2] = 0.0
        layers["veg_scale"]["value"][2] = 0.0
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        cfg.pop(key, None)
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(sim.forcing.df.iloc[:n_steps])
    return sim


def _run(sim, **kwargs):
    index = sim.forcing.index
    return sim.run(start_date=index[0], end_date=index[-1], n_jobs=1, **kwargs)


@pytest.mark.parametrize("net_radiation", [3, 1003], ids=["plan-area-slab", "spartacus-facets"])
def test_implicit_coupling_converges_without_relaxation(
    short_sample_sim, net_radiation
):
    """Fresh implicit starts converge well inside the cap without relaxation."""
    out = _run(_ehc_sim(short_sample_sim, net_radiation, 2 * TIMESTEPS_PER_DAY)).df
    suews = out["SUEWS"]
    for name in ("QN", "QF", "QS", "QE", "QH"):
        assert np.isfinite(suews[name]).all(), name
    # grid ledger Q* + QF = QH + QE + QS (no snow in the sample) to round-off
    ledger = suews["QN"] + suews["QF"] - suews["QH"] - suews["QE"] - suews["QS"]
    assert np.abs(ledger.to_numpy()).max() < 1e-8
    # the debug counter is one past the last pass; the loop's floor is two
    # passes and its cap 59
    n_iter = out[("debug", "iter")].to_numpy()
    capped = n_iter >= 60
    # The steady initial envelope and indoor film remove the six startup
    # caps seen with a cold profile and bare indoor boundary.
    # Keep this regression strict; the annual comparison covers the full trajectory.
    assert not capped.any()
    assert n_iter.max() < 15
    assert n_iter.mean() < 4.5


def test_implicit_facets_close_their_surface_energy_balance(short_sample_sim):
    """Residual QH of each roof and wall facet equals rho*cp*(Ts - Ta)/r_a."""
    sim = _ehc_sim(short_sample_sim, 1003, 2 * TIMESTEPS_PER_DAY)
    out = _run(sim).df
    ehc, dbg = out["EHC"], out["debug"]
    h = dbg["avdens"] * dbg["avcp"] / dbg["RA"]
    times = out.index.get_level_values("datetime")
    ta = sim.forcing.df["Tair"].reindex(times).to_numpy()
    assert np.isfinite(ta).all()
    for facet in ("Roof", "Wall"):
        for layer in (1, 2, 3):
            qh_residual = ehc[f"QH_{facet}_{layer}"].to_numpy()
            qh_aero = h.to_numpy() * (ehc[f"Ts_{facet}_{layer}"].to_numpy() - ta)
            mismatch = np.abs(qh_residual - qh_aero)
            # closes to the outer-loop tolerance (QE and r_a lag one pass)
            assert np.quantile(mismatch, 0.99) < 1.0, (facet, layer, np.quantile(mismatch, [0.5, 0.99, 1]))


@pytest.mark.parametrize("net_radiation", [3, 1003])
@pytest.mark.parametrize("split", [1, TIMESTEPS_PER_DAY])
def test_implicit_checkpoint_continuation_matches_continuous_run(short_sample_sim, net_radiation, split):
    sim_full = _ehc_sim(short_sample_sim, net_radiation, 300)
    out_full = _run(sim_full)

    sim_first = _ehc_sim(short_sample_sim, net_radiation, max(2, split))
    sim_first.run(end_date=sim_first.forcing.index[split - 1], n_jobs=1)
    sim_second = SUEWSSimulation.from_checkpoint(sim_first.config, sim_first.checkpoint)
    sim_second.update_forcing(sim_full.forcing.df.iloc[split:])
    out_second = _run(sim_second)

    pd.testing.assert_frame_equal(
        out_second.df[["SUEWS", "EHC"]],
        out_full.df[["SUEWS", "EHC"]].loc[out_second.df.index],
        check_exact=True,
    )


@pytest.mark.parametrize("net_radiation", [3, 1003])
def test_implicit_chunked_run_matches_uninterrupted_run(short_sample_sim, net_radiation):
    out_full = _run(_ehc_sim(short_sample_sim, net_radiation, 300), chunk_day=3660)
    out_chunked = _run(_ehc_sim(short_sample_sim, net_radiation, 300), chunk_day=1)
    pd.testing.assert_frame_equal(out_chunked.df[["SUEWS", "EHC"]], out_full.df[["SUEWS", "EHC"]], check_exact=True)


@pytest.mark.parametrize("net_radiation,surface,label", [
    (3, "bldgs", "Bldgs"), (3, "water", "Water"),
    (1003, "paved", "Paved"), (1003, "water", "Water"),
])
@pytest.mark.parametrize("property_name,absent_value", [("rho_cp", 0.0), ("dz", -999.0)])
def test_implicit_surface_without_thermal_layers_still_converges(
    short_sample_sim, net_radiation, surface, label, property_name, absent_value
):
    """A wholly absent column stores nothing and must not stall either solver."""
    sim = _ehc_sim(short_sample_sim, net_radiation, TIMESTEPS_PER_DAY)
    cfg = sim.config.model_dump(exclude_none=True, mode="json")
    cfg["sites"][0]["properties"]["land_cover"][surface]["thermal_layers"][property_name] = {"value": [absent_value] * 5}
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        cfg.pop(key, None)
    sim.update_config(cfg, auto_load_forcing=False)
    out = _run(sim).df
    assert np.abs(out[("EHC", f"QS_{label}")].to_numpy()).max() == 0.0
    assert np.abs(out[("SUEWS", "QS")].to_numpy()).max() > 1.0
    assert out[("debug", "iter")].max() < 15


SURFACES = (("paved", "Paved"), ("bldgs", "Bldgs"), ("evetr", "EveTr"), ("dectr", "DecTr"),
            ("grass", "Grass"), ("bsoil", "BSoil"), ("water", "Water"))


def _v(node):
    return node["value"] if isinstance(node, dict) and "value" in node else node


def _site(sim):
    return sim.config.model_dump(exclude_none=True, mode="json")["sites"][0]


def _update_site(sim, edit):
    cfg = sim.config.model_dump(exclude_none=True, mode="json")
    edit(cfg)
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        cfg.pop(key, None)
    sim.update_config(cfg, auto_load_forcing=False)


@pytest.mark.parametrize("net_radiation", [3, 1003], ids=["plan-area-slab", "spartacus-facets"])
def test_implicit_building_qf_enters_through_the_indoor_boundary(short_sample_sim, net_radiation):
    """No facet carries QF: the envelope takes in what the indoor boundary
    conducts into it, and the rest of QF goes to the air (grid QH)."""
    sim = _ehc_sim(short_sample_sim, net_radiation, TIMESTEPS_PER_DAY)
    out = _run(sim).df
    suews, ehc = out["SUEWS"], out["EHC"]
    land_cover = _site(sim)["properties"]["land_cover"]

    np.testing.assert_allclose(ehc["QF_Air"], suews["QF"] - ehc["QIndoor_EHC"], rtol=0, atol=1e-9)
    # grid QS is the area-weighted surface QS; the building tile's includes
    # its indoor input, so the ledger Q* + QF = QH + QE + QS still closes
    qs_sum = sum(float(_v(land_cover[key]["sfr"])) * ehc[f"QS_{name}"] for key, name in SURFACES)
    np.testing.assert_allclose(suews["QS"], qs_sum, rtol=0, atol=1e-8)
    # buildings are heated (the indoor boundary is warmer than the envelope)
    assert ehc["QIndoor_EHC"].mean() > 0.0
    assert (ehc["QF_Bldg"] >= 0.0).all()
    assert (ehc["QF_Bldg"] <= suews["QF"] + 1e-9).all()
    if net_radiation < 1000:
        # backward Euler on one slab: dU/dt = q_top - q_bottom exactly
        dudt = suews["QS"] - ehc["QIndoor_EHC"] - ehc["QBot_EHC"]
        np.testing.assert_allclose(ehc["dUdt_EHC"], dudt, rtol=0, atol=1e-8)


@pytest.mark.parametrize("net_radiation", [3, 1003], ids=["slab", "facets"])
def test_implicit_zero_qf_retains_indoor_input_and_closes_ledger(short_sample_sim, net_radiation):
    """A zero emissions inventory does not cap the prescribed indoor input."""
    sim = _ehc_sim(short_sample_sim, net_radiation, TIMESTEPS_PER_DAY)
    _update_site(sim, lambda cfg: cfg["model"]["physics"].update(emissions={"value": 0}))
    forcing = sim.forcing.df.copy()
    forcing["qf"] = 0.0
    sim.update_forcing(forcing)
    out = _run(sim).df
    suews, ehc = out["SUEWS"], out["EHC"]
    assert (suews["QF"] == 0.0).all()
    np.testing.assert_allclose(ehc["QF_Air"], -ehc["QIndoor_EHC"], rtol=0, atol=1e-9)
    assert (ehc.loc[ehc["QIndoor_EHC"] > 0.0, "QF_Air"] < 0.0).all()
    assert (ehc["QIndoor_EHC"] > 0.0).any()
    ledger = suews["QN"] + suews["QF"] - suews["QH"] - suews["QE"] - suews["QS"]
    np.testing.assert_allclose(ledger, 0.0, rtol=0, atol=1e-8)


def test_observed_qf_uses_building_energy_fraction_as_fallback(short_sample_sim):
    """Observed QF has no split by source: QF_Bldg = QF0_BEU x QF."""
    sim = _ehc_sim(short_sample_sim, 3, TIMESTEPS_PER_DAY)
    _update_site(sim, lambda cfg: cfg["model"]["physics"].update(emissions={"value": 0}))
    forcing = sim.forcing.df.copy()
    forcing["qf"] = 30.0
    sim.update_forcing(forcing)
    out = _run(sim).df
    beu = _site(sim)["properties"]["anthropogenic_emissions"]["heat"]["qf0_beu"]
    fractions = {float(_v(beu["working_day"])), float(_v(beu["holiday"]))}
    ratio = (out[("EHC", "QF_Bldg")] / out[("SUEWS", "QF")]).round(12)
    assert set(ratio.unique()) <= fractions


def _expected_deep_temperature(site, times):
    props = site["properties"]
    stebbs = props["stebbs"]
    t_mean = float(_v(stebbs["temperature_air_annual_mean"]))
    t_amp = 0.5 * float(_v(stebbs["temperature_air_month_mean_diffmax"]))
    t0 = 15.0 + (0.5 * 365.25 if float(_v(props["lat"])) < 0 else 0.0)
    w_day = 2 * np.pi / 365.25
    w_sec = w_day / 86400.0
    dectime = (times.dayofyear - 1) + times.hour / 24 + times.minute / 1440
    total, weight = 0.0, 0.0
    for key, _ in SURFACES:
        lc = props["land_cover"][key]
        sfr = float(_v(lc["sfr"]))
        if key == "bldgs" or sfr <= 0:
            continue
        layers = lc["thermal_layers"]
        dz, k, cp = (np.asarray(_v(layers[name]), dtype=float) for name in ("dz", "k", "rho_cp"))
        depth = dz.sum()
        kappa = depth**2 / ((dz / k).sum() * (cp * dz).sum())
        d_damp = np.sqrt(2 * kappa / w_sec)
        phase = w_day * (np.asarray(dectime) + 1.0 - t0)
        total = total + sfr * (t_mean - t_amp * np.exp(-depth / d_damp) * np.cos(phase - depth / d_damp))
        weight += sfr
    return total / weight


def test_implicit_lower_boundaries_follow_annual_wave_and_indoor_setpoint(short_sample_sim):
    sim = _ehc_sim(short_sample_sim, 3, TIMESTEPS_PER_DAY)
    out = _run(sim).df
    times = out.index.get_level_values("datetime")
    site = _site(sim)
    # deep soil: annual mean plus the damped, lagged annual wave at each
    # column's depth (one 5-minute step of timing slack is ~1e-4 K)
    np.testing.assert_allclose(out[("EHC", "T_Deep_EHC")], _expected_deep_temperature(site, times), atol=2e-3)
    # indoor: the configured setpoint (sample: 20 degC)
    setpoint = float(_v(site["properties"]["stebbs"]["temperature_air_indoor_initial"]))
    assert (out[("EHC", "T_Indoor_EHC")] == setpoint).all()

    # a fresh simulation: a second run() of the first would continue from its end
    sim = _ehc_sim(short_sample_sim, 3, TIMESTEPS_PER_DAY)
    _update_site(sim, lambda cfg: cfg["sites"][0]["properties"]["stebbs"].update(
        temperature_air_indoor_initial={"value": 17.0}))
    out = _run(sim).df
    assert (out[("EHC", "T_Indoor_EHC")] == 17.0).all()


def test_explicit_coupling_keeps_its_configured_boundaries(short_sample_sim):
    """The new boundaries belong to the implicit coupling only."""
    sim = _ehc_sim(short_sample_sim, 3, TIMESTEPS_PER_DAY, coupling="explicit")
    out = _run(sim).df
    assert (out[("EHC", "T_Indoor_EHC")] == -999.0).all() or out[("EHC", "T_Indoor_EHC")].isna().all()
    site = _site(sim)
    tin = {key: float(_v(site["initial_states"][key]["tin"])) for key, _ in SURFACES}
    sfr = {key: float(_v(site["properties"]["land_cover"][key]["sfr"])) for key, _ in SURFACES}
    ground = [key for key, _ in SURFACES if key != "bldgs" and sfr[key] > 0]
    expected = sum(sfr[k] * tin[k] for k in ground) / sum(sfr[k] for k in ground)
    np.testing.assert_allclose(out[("EHC", "T_Deep_EHC")], expected, rtol=0, atol=1e-12)


def test_implicit_indoor_boundary_follows_stebbs(short_sample_sim):
    """With STEBBS running, the indoor boundary is its indoor air temperature."""
    sim = _ehc_sim(short_sample_sim, 3, TIMESTEPS_PER_DAY)
    _update_site(sim, lambda cfg: cfg["model"]["physics"]["stebbs"].update(enabled=True))
    out = _run(sim).df
    t_indoor = out[("EHC", "T_Indoor_EHC")]
    tair_ind = out[("STEBBS", "Tair_ind")] - 273.15
    np.testing.assert_allclose(t_indoor, tair_ind, rtol=0, atol=1e-9)
    assert t_indoor.std() > 0.0


@pytest.mark.parametrize("r_roof,r_wall", [(0.10, 0.13), (0.0, 0.0), (0.25, 0.30)])
def test_fresh_envelope_matches_manually_prepared_resistance_profile(short_sample_sim, r_roof, r_wall):
    """Automatic preparation and an explicitly supplied steady profile agree."""
    automatic = _ehc_sim(short_sample_sim, 1003, 6)
    manual = _ehc_sim(short_sample_sim, 1003, 6)
    tair = float(automatic.forcing.df["Tair"].iloc[0])

    def prepare(cfg, automatic_profile):
        physics = cfg["model"]["physics"]
        physics["control_ehc_initial_profile"] = automatic_profile
        physics["resistance_ehc_indoor_roof"] = r_roof
        physics["resistance_ehc_indoor_wall"] = r_wall
        if automatic_profile:
            return
        site = cfg["sites"][0]
        indoor = _v(site["properties"]["stebbs"]["temperature_air_indoor_initial"])
        for facets, rsi in (("roofs", r_roof), ("walls", r_wall)):
            for material, state in zip(site["properties"]["vertical_layers"][facets], site["initial_states"][facets]):
                layers = material["thermal_layers"]
                resistance = np.array(_v(layers["dz"])) / np.array(_v(layers["k"]))
                centres = np.cumsum(resistance) - 0.5 * resistance
                state["temperature"] = {"value": (tair + (indoor - tair) * centres / (sum(resistance) + rsi)).tolist()}

    _update_site(automatic, lambda cfg: prepare(cfg, True))
    _update_site(manual, lambda cfg: prepare(cfg, False))
    pd.testing.assert_frame_equal(_run(automatic).df[["SUEWS", "EHC"]],
                                  _run(manual).df[["SUEWS", "EHC"]], atol=1e-9, rtol=1e-11)


@pytest.mark.parametrize("net_radiation", [3, 1003])
def test_explicit_coupling_ignores_indoor_options(short_sample_sim, net_radiation):
    baseline = _ehc_sim(short_sample_sim, net_radiation, 6, coupling="explicit")
    changed = _ehc_sim(short_sample_sim, net_radiation, 6, coupling="explicit")
    def edit(cfg):
        cfg["model"]["physics"].update(control_ehc_initial_profile=False,
            resistance_ehc_indoor_roof=0.5, resistance_ehc_indoor_wall=0.8)
    _update_site(changed, edit)
    pd.testing.assert_frame_equal(_run(baseline).df, _run(changed).df, check_exact=True)


def test_legacy_saved_state_disables_configuration_initialisation(short_sample_sim):
    original = _ehc_sim(short_sample_sim, 1003, 2)
    with pytest.warns(DeprecationWarning, match="legacy DFState"):
        resumed = SUEWSSimulation.from_state(original.state_init)
    assert not resumed.state_init[("control_ehc_initial_profile", "0")].any()
    # from_state owns a copy: the source configuration remains unchanged.
    assert original.state_init[("control_ehc_initial_profile", "0")].all()


@pytest.mark.parametrize("control", ["off", "relax", "secant"])
@pytest.mark.parametrize("coupling", ["explicit", "implicit"])
def test_stability_controls_preserve_early_convergence(short_sample_sim, control, coupling):
    baseline = _ehc_sim(short_sample_sim, 3, TIMESTEPS_PER_DAY, coupling=coupling)
    changed = _ehc_sim(short_sample_sim, 3, TIMESTEPS_PER_DAY, coupling=coupling)
    _update_site(changed, lambda cfg: cfg["model"]["physics"].update(control_ehc_stability=control))
    reference = _run(baseline).df
    if coupling == "implicit":
        assert reference[("debug", "iter")].max() <= 16
    actual = _run(changed).df
    pd.testing.assert_frame_equal(actual, reference, check_exact=True)
    assert np.array_equal(actual.to_numpy().view(np.uint64), reference.to_numpy().view(np.uint64))


@pytest.mark.parametrize("control", ["relax", "secant"])
def test_legacy_state_retains_stability_control(short_sample_sim, control):
    original = _ehc_sim(short_sample_sim, 3, 2)
    _update_site(original, lambda cfg: cfg["model"]["physics"].update(control_ehc_stability=control))
    with pytest.warns(DeprecationWarning, match="legacy DFState"):
        resumed = SUEWSSimulation.from_state(original.state_init)
    np.testing.assert_array_equal(resumed.state_init[("control_ehc_stability", "0")],
                                  original.state_init[("control_ehc_stability", "0")])


@pytest.mark.core
@pytest.mark.slow
@pytest.mark.parametrize("control", ["relax", "secant"])
def test_stability_control_checkpoint_after_august_cycle(control):
    """Late-pass history is timestep-local and cannot leak across a checkpoint."""
    def simulation():
        sim = SUEWSSimulation.from_sample_data()
        _update_site(sim, lambda cfg: cfg["model"]["physics"].update(
            storage_heat=5, net_radiation=3, ehc_surface_coupling="implicit",
            control_ehc_stability=control))
        return sim

    end = pd.Timestamp("2012-08-18 05:00")
    split = pd.Timestamp("2012-08-18 04:45")
    continuous = simulation()
    expected = continuous.run(end_date=end, n_jobs=1).df
    first = simulation()
    first.run(end_date=split, n_jobs=1)
    resumed = SUEWSSimulation.from_checkpoint(first.config, first.checkpoint)
    forcing = continuous.forcing.df
    resumed.update_forcing(forcing.loc[(forcing.index > split) & (forcing.index <= end)])
    actual = _run(resumed).df
    pd.testing.assert_frame_equal(actual, expected.loc[actual.index], check_exact=True)


@pytest.mark.parametrize(
    "storage_heat,include_qf",
    [(0, False), (1, False), (1, True), (3, False), (6, False), (7, False), (8, False)],
)
def test_coupling_default_is_inert_for_other_storage_methods(short_sample_sim, storage_heat, include_qf):
    """Every runnable non-EHC storage scheme ignores the EHC selector exactly."""
    outputs = []
    for coupling in (None, "explicit"):
        sim = short_sample_sim()
        cfg = sim.config.model_dump(exclude_none=True, mode="json")
        physics = cfg["model"]["physics"]
        physics["storage_heat"] = {"value": storage_heat}
        physics["ohm_inc_qf"] = {"value": int(include_qf)}
        physics.pop("ehc_surface_coupling", None)
        if coupling is not None:
            physics["ehc_surface_coupling"] = coupling
        if storage_heat == 7:
            physics["net_radiation"] = {"value": 1003}
            physics["stebbs"]["enabled"] = True
            layers = cfg["sites"][0]["properties"]["vertical_layers"]
            layers["veg_frac"]["value"][2] = 0.0
            layers["veg_scale"]["value"][2] = 0.0
        for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
            cfg.pop(key, None)
        sim.update_config(SUEWSConfig.model_validate(cfg), auto_load_forcing=False)
        if storage_heat == 0:
            forcing = sim.forcing.df.copy()
            forcing["qs"] = 30.0
            sim.update_forcing(forcing)
        outputs.append(sim.run(end_date=sim.forcing.index[TIMESTEPS_PER_DAY - 1], n_jobs=1).df)
    pd.testing.assert_frame_equal(*outputs, check_exact=True)
    np.testing.assert_array_equal(
        outputs[0].to_numpy().view(np.uint64), outputs[1].to_numpy().view(np.uint64)
    )


@pytest.mark.parametrize("net_radiation", [3, 1003], ids=["slab", "facets"])
@pytest.mark.parametrize("surface", ["paved", "water"])
@pytest.mark.parametrize("layer", [0, 1, 4], ids=["first", "interior", "bottom"])
@pytest.mark.parametrize("property_name", ["dz", "k", "rho_cp"])
@pytest.mark.parametrize("invalid", [0.0, -1.0, -999.0, float("nan")])
def test_implicit_rejects_malformed_layers(
    short_sample_sim, net_radiation, surface, layer, property_name, invalid
):
    """Malformed active columns fail visibly instead of dropping heat storage."""
    sim = _ehc_sim(short_sample_sim, net_radiation, 2)
    cfg = sim.config.model_dump(exclude_none=True, mode="json")
    thermal = cfg["sites"][0]["properties"]["land_cover"][surface]["thermal_layers"]
    thermal[property_name]["value"][layer] = invalid
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        cfg.pop(key, None)
    with pytest.raises((ValueError, RuntimeError), match="thermal layer"):
        sim.update_config(cfg, auto_load_forcing=False)
        _run(sim)


@pytest.mark.parametrize("facet", ["roofs", "walls"])
@pytest.mark.parametrize("layer", [0, 1, 4], ids=["first", "interior", "bottom"])
def test_implicit_rejects_zero_thickness_in_building_facets(short_sample_sim, facet, layer):
    sim = _ehc_sim(short_sample_sim, 1003, 2)

    def edit(cfg):
        thermal = cfg["sites"][0]["properties"]["vertical_layers"][facet][0]["thermal_layers"]
        thermal["dz"]["value"][layer] = 0.0

    _update_site(sim, edit)
    with pytest.raises((ValueError, RuntimeError), match="thermal layer"):
        _run(sim)
