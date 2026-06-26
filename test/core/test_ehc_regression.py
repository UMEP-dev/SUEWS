import copy

import numpy as np
import pytest

import supy as sp


pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.rust,
    pytest.mark.smoke,
]


SURFACE_NAMES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
EHC_ITERATION_ENV_KEYS = (
    "SUEWS_EHC_ADAPTIVE_RELAX",
    "SUEWS_EHC_DUAL_TIMESCALE",
    "SUEWS_EHC_MAX_ITER",
    "SUEWS_EHC_RESTORE_BEST_ITER",
    "SUEWS_EHC_STATE_ADMITTANCE",
    "SUEWS_EHC_TSFC_RELAX",
    "SUEWS_EHC_TSFC_STEP_QH_LIMIT",
)
EHC_RA_HEAT_ENV_KEYS = (
    "SUEWS_EHC_RA_HEAT_FACTOR",
    "SUEWS_EHC_RA_HEAT_MAX_FACTOR",
    "SUEWS_EHC_RA_HEAT_MODE",
    "SUEWS_EHC_RA_STATE_BOOST",
    "SUEWS_EHC_RA_STATE_KDOWN_MIN",
    "SUEWS_EHC_RA_STATE_KDOWN_REF",
    "SUEWS_EHC_RA_STATE_LOW_RA_REF",
    "SUEWS_EHC_RA_STATE_USTAR_REF",
)


def _run_short_ehc_with_surface_cp(rho_cp, surface_rho_cp=None, zero_kdown=False):
    sim = sp.SUEWSSimulation.from_sample_data()
    if zero_kdown:
        sim._df_forcing = sim._df_forcing.copy()
        sim._df_forcing["kdown"] = 0.0
    surface_rho_cp = surface_rho_cp or {}
    surface_updates = {
        name: {
            "thermal_layers": {
                "rho_cp": {"value": [surface_rho_cp.get(name, rho_cp)] * 5}
            }
        }
        for name in SURFACE_NAMES
    }
    sim.update_config(
        {
            "model": {
                "physics": {
                    "net_radiation": {"value": 3},
                    "storage_heat": {"value": 5},
                }
            },
            "sites": {
                "properties": {
                    "land_cover": surface_updates,
                }
            },
        }
    )

    output = sim.run(end_date=sim._df_forcing.index[11])
    return output.df[("SUEWS", "QS")].to_numpy()


def _run_short_ehc_fluxes_with_surface_cp(rho_cp):
    sim = sp.SUEWSSimulation.from_sample_data()
    surface_updates = {
        name: {
            "thermal_layers": {
                "rho_cp": {"value": [rho_cp] * 5}
            }
        }
        for name in SURFACE_NAMES
    }
    sim.update_config(
        {
            "model": {
                "physics": {
                    "net_radiation": {"value": 3},
                    "storage_heat": {"value": 5},
                }
            },
            "sites": {
                "properties": {
                    "land_cover": surface_updates,
                }
            },
        }
    )

    output = sim.run(end_date=sim._df_forcing.index[11])
    return output.df["SUEWS"][["QS", "QE", "QH"]].to_numpy()


def _with_vertical_layer_cp(layers, rho_cp):
    updates = []
    for layer in layers:
        layer_update = copy.deepcopy(layer)
        thermal_layers = layer_update.setdefault("thermal_layers", {})
        existing_cp = thermal_layers.get("rho_cp", {})
        if isinstance(existing_cp, dict):
            ndepth = len(existing_cp.get("value", [])) or 5
        else:
            ndepth = 5
        thermal_layers["rho_cp"] = {"value": [rho_cp] * ndepth}
        updates.append(layer_update)
    return updates


def _run_short_spartacus_ehc_with_building_cp(rho_cp):
    sim = sp.SUEWSSimulation.from_sample_data()
    cfg = sim._config.model_dump(exclude_none=True, mode="json")
    land_cover = cfg["sites"][0]["properties"]["land_cover"]
    vertical_layers = cfg["sites"][0]["properties"]["vertical_layers"]
    vertical_layers["roofs"] = _with_vertical_layer_cp(vertical_layers["roofs"], rho_cp)
    vertical_layers["walls"] = _with_vertical_layer_cp(vertical_layers["walls"], rho_cp)
    vertical_layers["building_frac"]["value"][0] = land_cover["bldgs"]["sfr"]["value"]
    vertical_layers["veg_frac"]["value"][0] = (
        land_cover["evetr"]["sfr"]["value"] + land_cover["dectr"]["sfr"]["value"]
    )
    vertical_layers["veg_frac"]["value"][2] = 0.0
    vertical_layers["veg_scale"]["value"][2] = 0.0
    cfg["model"]["physics"]["net_radiation"] = {"value": 1003}
    cfg["model"]["physics"]["storage_heat"] = {"value": 5}
    for internal_key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        cfg.pop(internal_key, None)

    sim.update_config(cfg)

    output = sim.run(end_date=sim._df_forcing.index[11])
    return output.df[("SUEWS", "QS")].to_numpy()


def test_ehc_lumped_storage_is_sensitive_to_surface_rho_cp():
    low_cp_qs = _run_short_ehc_with_surface_cp(1.0e6)
    high_cp_qs = _run_short_ehc_with_surface_cp(4.0e6)

    assert low_cp_qs.shape == high_cp_qs.shape
    assert np.max(np.abs(high_cp_qs - low_cp_qs)) > 1.0


def test_ehc_lumped_storage_skips_invalid_surface_without_zeroing_grid():
    qs = _run_short_ehc_with_surface_cp(1.0e6, surface_rho_cp={"bldgs": 0.0})

    assert np.max(np.abs(qs)) > 1.0


def test_ehc_dual_timescale_lumped_branch_changes_storage_response(monkeypatch):
    env_keys = [
        "SUEWS_EHC_DUAL_TIMESCALE",
        "SUEWS_EHC_FAST_WEIGHT",
        "SUEWS_EHC_FAST_CAP_SCALE",
        "SUEWS_EHC_SLOW_CAP_SCALE",
        "SUEWS_EHC_FAST_G_SCALE",
        "SUEWS_EHC_SLOW_G_SCALE",
    ]
    for key in env_keys:
        monkeypatch.delenv(key, raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_DUAL_TIMESCALE", "1")
    monkeypatch.setenv("SUEWS_EHC_FAST_WEIGHT", "0.45")
    monkeypatch.setenv("SUEWS_EHC_FAST_CAP_SCALE", "0.30")
    monkeypatch.setenv("SUEWS_EHC_SLOW_CAP_SCALE", "1.50")
    monkeypatch.setenv("SUEWS_EHC_FAST_G_SCALE", "2.00")
    monkeypatch.setenv("SUEWS_EHC_SLOW_G_SCALE", "0.70")
    dual_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(dual_qs))
    assert np.max(np.abs(dual_qs - default_qs)) > 0.1


def test_ehc_state_admittance_changes_lumped_storage_response(monkeypatch):
    for key in (
        "SUEWS_EHC_STATE_ADMITTANCE",
        "SUEWS_EHC_WARMING_G_BOOST",
        "SUEWS_EHC_COOLING_G_DAMP",
        "SUEWS_EHC_GRADIENT_SCALE",
        "SUEWS_EHC_MIN_G_SCALE",
    ):
        monkeypatch.delenv(key, raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_STATE_ADMITTANCE", "1")
    monkeypatch.setenv("SUEWS_EHC_WARMING_G_BOOST", "0.75")
    monkeypatch.setenv("SUEWS_EHC_COOLING_G_DAMP", "0.40")
    monkeypatch.setenv("SUEWS_EHC_GRADIENT_SCALE", "2.0")
    monkeypatch.setenv("SUEWS_EHC_MIN_G_SCALE", "0.45")
    state_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(state_qs))
    assert np.max(np.abs(state_qs - default_qs)) > 0.1


def test_ehc_impervious_qf_allocation_changes_lumped_storage_response(monkeypatch):
    monkeypatch.delenv("SUEWS_EHC_QF_SURF_MODE", raising=False)
    monkeypatch.delenv("SUEWS_EHC_QF_IMPERVIOUS_ONLY", raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_QF_IMPERVIOUS_ONLY", "1")
    impervious_qf_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(impervious_qf_qs))
    assert np.max(np.abs(impervious_qf_qs - default_qs)) > 0.1


def test_ehc_direct_air_qf_mode_changes_lumped_storage_response(monkeypatch):
    monkeypatch.delenv("SUEWS_EHC_QF_IMPERVIOUS_ONLY", raising=False)
    monkeypatch.delenv("SUEWS_EHC_QF_SURF_MODE", raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_QF_SURF_MODE", "none")
    direct_air_qf_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(direct_air_qf_qs))
    assert np.max(np.abs(direct_air_qf_qs - default_qs)) > 0.1


def test_ehc_direct_air_qf_mode_changes_coupled_qe_response(monkeypatch):
    monkeypatch.delenv("SUEWS_EHC_QF_IMPERVIOUS_ONLY", raising=False)
    monkeypatch.delenv("SUEWS_EHC_QF_SURF_MODE", raising=False)
    default_fluxes = _run_short_ehc_fluxes_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_QF_SURF_MODE", "none")
    direct_air_fluxes = _run_short_ehc_fluxes_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(direct_air_fluxes))
    assert np.max(np.abs(direct_air_fluxes[:, 1] - default_fluxes[:, 1])) > 0.1


def test_ehc_daylight_qf_mode_changes_lumped_storage_response(monkeypatch):
    for key in (
        "SUEWS_EHC_QF_IMPERVIOUS_ONLY",
        "SUEWS_EHC_QF_SURF_MODE",
        "SUEWS_EHC_QF_SURF_DAY_FRAC",
        "SUEWS_EHC_QF_SURF_NIGHT_FRAC",
        "SUEWS_EHC_QF_SURF_KDOWN_REF",
    ):
        monkeypatch.delenv(key, raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_QF_SURF_MODE", "daylight")
    monkeypatch.setenv("SUEWS_EHC_QF_SURF_DAY_FRAC", "1.0")
    monkeypatch.setenv("SUEWS_EHC_QF_SURF_NIGHT_FRAC", "0.25")
    monkeypatch.setenv("SUEWS_EHC_QF_SURF_KDOWN_REF", "150.0")
    daylight_qf_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(daylight_qf_qs))
    assert np.max(np.abs(daylight_qf_qs - default_qs)) > 0.1


def test_ehc_surface_gradient_allocation_changes_lumped_storage_response(monkeypatch):
    monkeypatch.delenv("SUEWS_EHC_QS_SURF_ALLOC", raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_QS_SURF_ALLOC", "conductance_gradient")
    gradient_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(gradient_qs))
    assert np.max(np.abs(gradient_qs - default_qs)) > 0.1


def test_ehc_parallel_standard_surfaces_preserves_heterogeneous_response(monkeypatch):
    monkeypatch.delenv("SUEWS_EHC_PARALLEL_SURFACES", raising=False)
    surface_rho_cp = {
        "paved": 3.2e6,
        "bldgs": 2.4e6,
        "evetr": 1.2e6,
        "dectr": 1.0e6,
        "grass": 1.1e6,
        "bsoil": 1.6e6,
        "water": 4.0e6,
    }
    pre_lumped_qs = _run_short_ehc_with_surface_cp(2.0e6, surface_rho_cp=surface_rho_cp)

    monkeypatch.setenv("SUEWS_EHC_PARALLEL_SURFACES", "1")
    parallel_qs = _run_short_ehc_with_surface_cp(2.0e6, surface_rho_cp=surface_rho_cp)

    assert np.all(np.isfinite(parallel_qs))
    assert np.max(np.abs(parallel_qs - pre_lumped_qs)) > 0.05


def test_ehc_converged_lumped_storage_is_nearly_relaxation_invariant(monkeypatch):
    for key in EHC_ITERATION_ENV_KEYS:
        monkeypatch.delenv(key, raising=False)

    monkeypatch.setenv("SUEWS_EHC_MAX_ITER", "120")
    monkeypatch.setenv("SUEWS_EHC_TSFC_RELAX", "0.3")
    slow_relax_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_TSFC_RELAX", "0.8")
    fast_relax_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(fast_relax_qs))
    assert np.max(np.abs(fast_relax_qs - slow_relax_qs)) < 0.75


def test_ehc_ra_heat_factor_changes_lumped_storage_response(monkeypatch):
    for key in EHC_RA_HEAT_ENV_KEYS:
        monkeypatch.delenv(key, raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_RA_HEAT_FACTOR", "1.5")
    higher_ra_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(higher_ra_qs))
    assert np.max(np.abs(higher_ra_qs - default_qs)) > 0.1


def test_ehc_state_dependent_ra_heat_guard_changes_lumped_storage_response(monkeypatch):
    for key in EHC_RA_HEAT_ENV_KEYS:
        monkeypatch.delenv(key, raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_RA_HEAT_MODE", "overcoupled_guard")
    monkeypatch.setenv("SUEWS_EHC_RA_STATE_BOOST", "1.5")
    monkeypatch.setenv("SUEWS_EHC_RA_STATE_KDOWN_MIN", "0.0")
    monkeypatch.setenv("SUEWS_EHC_RA_STATE_KDOWN_REF", "1000.0")
    guarded_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(guarded_qs))
    assert np.max(np.abs(guarded_qs - default_qs)) > 0.1


def test_ehc_state_dependent_ra_heat_guard_includes_zero_kdown(monkeypatch):
    for key in EHC_RA_HEAT_ENV_KEYS:
        monkeypatch.delenv(key, raising=False)
    default_qs = _run_short_ehc_with_surface_cp(2.0e6, zero_kdown=True)

    monkeypatch.setenv("SUEWS_EHC_RA_HEAT_MODE", "overcoupled_guard")
    monkeypatch.setenv("SUEWS_EHC_RA_STATE_BOOST", "1.5")
    monkeypatch.setenv("SUEWS_EHC_RA_STATE_KDOWN_MIN", "0.0")
    monkeypatch.setenv("SUEWS_EHC_RA_STATE_KDOWN_REF", "1000.0")
    guarded_qs = _run_short_ehc_with_surface_cp(2.0e6, zero_kdown=True)

    assert np.all(np.isfinite(guarded_qs))
    assert np.max(np.abs(guarded_qs - default_qs)) > 0.1


def test_ehc_adaptive_relax_limits_high_relax_response(monkeypatch):
    for key in (
        "SUEWS_EHC_ADAPTIVE_RELAX",
        "SUEWS_EHC_TSFC_RELAX",
        "SUEWS_EHC_TSFC_STEP_QH_LIMIT",
    ):
        monkeypatch.delenv(key, raising=False)

    monkeypatch.setenv("SUEWS_EHC_TSFC_RELAX", "0.8")
    high_relax_qs = _run_short_ehc_with_surface_cp(2.0e6)

    monkeypatch.setenv("SUEWS_EHC_ADAPTIVE_RELAX", "1")
    monkeypatch.setenv("SUEWS_EHC_TSFC_STEP_QH_LIMIT", "5.0")
    guarded_qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(guarded_qs))
    assert np.max(np.abs(guarded_qs - high_relax_qs)) > 0.1


def test_ehc_restore_best_iteration_path_runs(monkeypatch):
    for key in (
        "SUEWS_EHC_ADAPTIVE_RELAX",
        "SUEWS_EHC_RESTORE_BEST_ITER",
        "SUEWS_EHC_TSFC_RELAX",
        "SUEWS_EHC_TSFC_STEP_QH_LIMIT",
        "SUEWS_EHC_MAX_ITER",
    ):
        monkeypatch.delenv(key, raising=False)

    monkeypatch.setenv("SUEWS_EHC_ADAPTIVE_RELAX", "1")
    monkeypatch.setenv("SUEWS_EHC_RESTORE_BEST_ITER", "always")
    monkeypatch.setenv("SUEWS_EHC_TSFC_RELAX", "1.0")
    monkeypatch.setenv("SUEWS_EHC_TSFC_STEP_QH_LIMIT", "25.0")
    monkeypatch.setenv("SUEWS_EHC_MAX_ITER", "6")
    qs = _run_short_ehc_with_surface_cp(2.0e6)

    assert np.all(np.isfinite(qs))
    assert np.max(np.abs(qs)) > 1.0


def test_ehc_spartacus_facet_storage_is_sensitive_to_building_rho_cp():
    low_cp_qs = _run_short_spartacus_ehc_with_building_cp(1.0e6)
    high_cp_qs = _run_short_spartacus_ehc_with_building_cp(4.0e6)

    assert low_cp_qs.shape == high_cp_qs.shape
    assert np.max(np.abs(high_cp_qs - low_cp_qs)) > 1.0
