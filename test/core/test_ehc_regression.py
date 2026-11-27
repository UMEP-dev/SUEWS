"""Legacy explicit EHC regression tests; implicit coupling has separate coverage."""

import copy

import numpy as np
import pytest

import supy as sp

pytestmark = [
    pytest.mark.physics,
    pytest.mark.rust,
    pytest.mark.contract("C-RESPONSE"),
]


SURFACE_NAMES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")


def _run_short_ehc_with_surface_cp(rho_cp, surface_rho_cp=None):
    sim = sp.SUEWSSimulation.from_sample_data()
    surface_rho_cp = surface_rho_cp or {}
    surface_updates = {
        name: {
            "thermal_layers": {
                "rho_cp": {"value": [surface_rho_cp.get(name, rho_cp)] * 5}
            }
        }
        for name in SURFACE_NAMES
    }
    sim.update_config({
        "model": {
            "physics": {
                "net_radiation": {"value": 3},
                "storage_heat": {"value": 5},
                "ehc_surface_coupling": "explicit",
            }
        },
        "sites": {
            "properties": {
                "land_cover": surface_updates,
            }
        },
    })

    output = sim.run(end_date=sim._df_forcing.index[11])
    return output.df["SUEWS", "QS"].to_numpy()


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
    cfg["model"]["physics"]["ehc_surface_coupling"] = "explicit"
    for internal_key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        cfg.pop(internal_key, None)

    sim.update_config(cfg)

    output = sim.run(end_date=sim._df_forcing.index[11])
    return output.df["SUEWS", "QS"].to_numpy()


@pytest.mark.core
@pytest.mark.medium
def test_ehc_lumped_storage_is_sensitive_to_surface_rho_cp():
    low_cp_qs = _run_short_ehc_with_surface_cp(1.0e6)
    high_cp_qs = _run_short_ehc_with_surface_cp(4.0e6)

    assert low_cp_qs.shape == high_cp_qs.shape
    assert np.max(np.abs(high_cp_qs - low_cp_qs)) > 1.0


@pytest.mark.smoke
def test_ehc_lumped_storage_skips_invalid_surface_without_zeroing_grid():
    qs = _run_short_ehc_with_surface_cp(1.0e6, surface_rho_cp={"bldgs": 0.0})

    assert np.max(np.abs(qs)) > 1.0


@pytest.mark.core
def test_ehc_spartacus_facet_storage_is_sensitive_to_building_rho_cp():
    low_cp_qs = _run_short_spartacus_ehc_with_building_cp(1.0e6)
    high_cp_qs = _run_short_spartacus_ehc_with_building_cp(4.0e6)

    assert low_cp_qs.shape == high_cp_qs.shape
    assert np.max(np.abs(high_cp_qs - low_cp_qs)) > 1.0


@pytest.mark.core
@pytest.mark.parametrize("coupling", ["explicit", "implicit"])
def test_ehc_building_facets_evap_matches_qe(coupling):
    """Regression test for GH#1883: EHC building-facet Evap must match QE."""
    sim = sp.SUEWSSimulation.from_sample_data()
    cfg = sim._config.model_dump(exclude_none=True, mode="json")
    land_cover = cfg["sites"][0]["properties"]["land_cover"]
    vertical_layers = cfg["sites"][0]["properties"]["vertical_layers"]
    vertical_layers["building_frac"]["value"][0] = land_cover["bldgs"]["sfr"]["value"]
    vertical_layers["veg_frac"]["value"][0] = (
        land_cover["evetr"]["sfr"]["value"] + land_cover["dectr"]["sfr"]["value"]
    )
    vertical_layers["veg_frac"]["value"][2] = 0.0
    vertical_layers["veg_scale"]["value"][2] = 0.0
    cfg["model"]["physics"]["net_radiation"] = {"value": 1003}
    cfg["model"]["physics"]["storage_heat"] = {"value": 5}
    cfg["model"]["physics"]["ehc_surface_coupling"] = coupling
    for internal_key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        cfg.pop(internal_key, None)

    sim.update_config(cfg, auto_load_forcing=False)
    # Use a 24-hour summer window with precipitation so roof/wall facets wet
    # and evaporate, exercising the building-facet water balance.
    forcing = sim._df_forcing.copy()
    forcing_win = forcing.loc["2012-07-01 00:05:00":"2012-07-02 00:00:00"].copy()
    forcing_win.iloc[12:18, forcing_win.columns.get_loc("rain")] = 1.0
    sim.update_forcing(forcing_win)

    output = sim.run(
        start_date=forcing_win.index[0],
        end_date=forcing_win.index[-1],
        n_jobs=1,
    )
    df = output.df
    tstep = float(sim._config.model.control.tstep)
    tlv = df["debug", "lv_J_kg"].to_numpy(dtype=float) / tstep

    qe_bldgs = df["debug", "QE_Bldgs"].to_numpy(dtype=float)
    ev_bldgs = df["debug", "ev_Bldgs"].to_numpy(dtype=float)
    qe_grid = df["SUEWS", "QE"].to_numpy(dtype=float)
    ev_grid = df["SUEWS", "Evap"].to_numpy(dtype=float)

    # Confirm the building-facet latent heat path was actively exercised.
    assert np.max(np.abs(qe_bldgs)) > 1.0
    np.testing.assert_allclose(qe_bldgs, tlv * ev_bldgs, rtol=1e-12, atol=1e-10)
    np.testing.assert_allclose(qe_grid, tlv * ev_grid, rtol=1e-12, atol=1e-10)
