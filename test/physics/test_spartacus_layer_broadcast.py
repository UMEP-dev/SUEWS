"""Regression tests for SPARTACUS multilayer spectral-property transfer (gh#2130).

Before gh#2130, ``suews_phys_spartacus.f95`` assigned six rank-2
``(nspec, ntotlay)`` spectral-property arrays from scalar ``(nspec, ncol)``
slices (layer 1 only), broadcasting layer 1 across every vertical canopy layer:
``roof_albedo``, ``wall_albedo``, ``wall_specular_frac``, ``roof_albedo_dir``,
``roof_emissivity``, and ``wall_emissivity``.

All six assignments are fixed in ``suews_phys_spartacus.f95``. To keep this PR
independent of gh#2122 (which fixes the Rust YAML bridge ``nspec`` FFI stride
and gh#2085 roof/wall clobber for the two rank-2 fields
``roof_albedo_dir_mult_fact`` and ``wall_specular_frac``), these end-to-end
tests verify on a 3-layer SPARTACUS configuration that:
1. Perturbing any of the four rank-1 per-layer properties (``roof_albedo``,
   ``wall_albedo``, ``roof_emissivity``, ``wall_emissivity``) in layer 1, 2, or
   3 individually shifts radiative outputs with non-zero magnitude (and the
   expected physical sign for shortwave albedo perturbations).
2. Perturbing layer 1 alone does not produce the same response as perturbing
   all three layers uniformly (proving layer 1 is no longer broadcast).
3. A vertically heterogeneous 3-layer profile with distinct optical and
   emissivity properties in every layer closes shortwave, longwave, and coupled
   surface energy budgets cleanly.
"""

from __future__ import annotations

import copy
from importlib.resources import as_file, files

import numpy as np
import pytest
import yaml

import supy
from supy.data_model import SUEWSConfig

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-RESPONSE", "C-BALANCE"),
]

BASE_ROOF_ALB = (0.30, 0.30, 0.30)
BASE_WALL_ALB = (0.30, 0.30, 0.30)
BASE_ROOF_EMIS = (0.90, 0.90, 0.90)
BASE_WALL_EMIS = (0.90, 0.90, 0.90)


@pytest.fixture(scope="module")
def spartacus_midday_window():
    """Load bundled sample config and a 2-hour midday summer forcing window."""
    with as_file(files("supy") / "sample_data") as sample_dir:
        with (sample_dir / "sample_config.yml").open(encoding="utf-8") as handle:
            base_dict = yaml.safe_load(handle)
        base_sim = supy.SUEWSSimulation(sample_dir / "sample_config.yml")
        window_forcing = base_sim.forcing.df.loc[
            "2012-06-13 11:05:00":"2012-06-13 13:00:00"
        ].copy()

    base_dict["model"]["control"]["start_time"] = "2012-06-13 11:05:00"
    base_dict["model"]["control"]["end_time"] = "2012-06-13 13:00:00"
    base_dict["model"]["physics"]["net_radiation"] = {"value": 1003}
    base_dict["model"]["physics"]["storage_heat"] = {"value": 5}
    site_props = base_dict["sites"][0]["properties"]
    site_props["spartacus"]["use_sw_direct_albedo"] = {"value": 1.0}
    # Sample top layer (15-22 m) sits above both tree crowns (13.1 m).
    site_props["vertical_layers"]["veg_frac"]["value"][2] = 0.0
    site_props["vertical_layers"]["veg_scale"]["value"][2] = 0.0
    return base_dict, window_forcing


def _run_3layer_case(
    base_dict: dict,
    window_forcing,
    *,
    roof_alb: tuple[float, float, float] = BASE_ROOF_ALB,
    wall_alb: tuple[float, float, float] = BASE_WALL_ALB,
    roof_emis: tuple[float, float, float] = BASE_ROOF_EMIS,
    wall_emis: tuple[float, float, float] = BASE_WALL_EMIS,
):
    cfg_dict = copy.deepcopy(base_dict)
    vlayers = cfg_dict["sites"][0]["properties"]["vertical_layers"]
    for idx, (roof, wall) in enumerate(
        zip(vlayers["roofs"], vlayers["walls"], strict=True)
    ):
        roof["alb"] = {"value": roof_alb[idx]}
        roof["emis"] = {"value": roof_emis[idx]}
        roof["roof_albedo_dir_mult_fact"] = {"value": 1.0}
        wall["alb"] = {"value": wall_alb[idx]}
        wall["emis"] = {"value": wall_emis[idx]}
        wall["wall_specular_frac"] = {"value": 0.10}

    config = SUEWSConfig.from_dict(cfg_dict)
    sim = supy.SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(window_forcing)
    return sim.run(n_jobs=1).df


def _with_layer_value(
    base_vals: tuple[float, float, float],
    layer_idx: int,
    perturbed: float,
) -> tuple[float, float, float]:
    vals = list(base_vals)
    vals[layer_idx] = perturbed
    return (vals[0], vals[1], vals[2])


@pytest.fixture(scope="module")
def homogeneous_baseline_df(spartacus_midday_window):
    """Homogeneous 3-layer SPARTACUS reference output."""
    base_dict, window_forcing = spartacus_midday_window
    return _run_3layer_case(base_dict, window_forcing)


@pytest.mark.parametrize("layer_idx", [0, 1, 2])
def test_roof_albedo_perturbation_per_layer(
    spartacus_midday_window,
    homogeneous_baseline_df,
    layer_idx,
):
    """Raising roofs[i].alb increases Kup and reduces KNet_Roof_(i+1) for every layer."""
    base_dict, window_forcing = spartacus_midday_window
    df_pert = _run_3layer_case(
        base_dict,
        window_forcing,
        roof_alb=_with_layer_value(BASE_ROOF_ALB, layer_idx, 0.70),
    )

    delta_kup = df_pert["SUEWS", "Kup"] - homogeneous_baseline_df["SUEWS", "Kup"]
    delta_qn = df_pert["SUEWS", "QN"] - homogeneous_baseline_df["SUEWS", "QN"]
    delta_facet = (
        df_pert["SPARTACUS", f"KNet_Roof_{layer_idx + 1}"]
        - homogeneous_baseline_df["SPARTACUS", f"KNet_Roof_{layer_idx + 1}"]
    )

    assert np.isfinite(delta_kup.to_numpy()).all()
    assert np.isfinite(delta_qn.to_numpy()).all()
    assert np.isfinite(delta_facet.to_numpy()).all()
    assert delta_kup.max() > 1.0, (
        f"Layer {layer_idx + 1} roof albedo perturbation produced Kup shift {delta_kup.max()}"
    )
    assert delta_qn.min() < -1.0, (
        f"Layer {layer_idx + 1} roof albedo perturbation produced QN shift {delta_qn.min()}"
    )
    assert delta_facet.min() < -50.0, (
        f"Layer {layer_idx + 1} roof albedo perturbation produced KNet_Roof shift {delta_facet.min()}"
    )


@pytest.mark.parametrize("layer_idx", [0, 1, 2])
def test_wall_albedo_perturbation_per_layer(
    spartacus_midday_window,
    homogeneous_baseline_df,
    layer_idx,
):
    """Raising walls[i].alb increases Kup and reduces KNet_Wall_(i+1) for every layer."""
    base_dict, window_forcing = spartacus_midday_window
    df_pert = _run_3layer_case(
        base_dict,
        window_forcing,
        wall_alb=_with_layer_value(BASE_WALL_ALB, layer_idx, 0.70),
    )

    delta_kup = df_pert["SUEWS", "Kup"] - homogeneous_baseline_df["SUEWS", "Kup"]
    delta_qn = df_pert["SUEWS", "QN"] - homogeneous_baseline_df["SUEWS", "QN"]
    delta_facet = (
        df_pert["SPARTACUS", f"KNet_Wall_{layer_idx + 1}"]
        - homogeneous_baseline_df["SPARTACUS", f"KNet_Wall_{layer_idx + 1}"]
    )

    assert np.isfinite(delta_kup.to_numpy()).all()
    assert np.isfinite(delta_qn.to_numpy()).all()
    assert np.isfinite(delta_facet.to_numpy()).all()
    assert delta_kup.max() > 0.5, (
        f"Layer {layer_idx + 1} wall albedo perturbation produced Kup shift {delta_kup.max()}"
    )
    assert delta_qn.min() < -0.5, (
        f"Layer {layer_idx + 1} wall albedo perturbation produced QN shift {delta_qn.min()}"
    )
    assert delta_facet.min() < -20.0, (
        f"Layer {layer_idx + 1} wall albedo perturbation produced KNet_Wall shift {delta_facet.min()}"
    )


@pytest.mark.parametrize("layer_idx", [0, 1, 2])
def test_roof_emissivity_perturbation_per_layer(
    spartacus_midday_window,
    homogeneous_baseline_df,
    layer_idx,
):
    """Lowering roofs[i].emis shifts Lup, QN, and LNet_Roof_(i+1) for every layer."""
    base_dict, window_forcing = spartacus_midday_window
    df_pert = _run_3layer_case(
        base_dict,
        window_forcing,
        roof_emis=_with_layer_value(BASE_ROOF_EMIS, layer_idx, 0.60),
    )

    delta_lup = (
        df_pert["SUEWS", "Lup"] - homogeneous_baseline_df["SUEWS", "Lup"]
    ).abs()
    delta_qn = (df_pert["SUEWS", "QN"] - homogeneous_baseline_df["SUEWS", "QN"]).abs()
    delta_facet = (
        df_pert["SPARTACUS", f"LNet_Roof_{layer_idx + 1}"]
        - homogeneous_baseline_df["SPARTACUS", f"LNet_Roof_{layer_idx + 1}"]
    ).abs()

    assert np.isfinite(delta_lup.to_numpy()).all()
    assert np.isfinite(delta_qn.to_numpy()).all()
    assert np.isfinite(delta_facet.to_numpy()).all()
    assert delta_lup.max() > 0.5, (
        f"Layer {layer_idx + 1} roof emissivity produced Lup shift {delta_lup.max()}"
    )
    assert delta_qn.max() > 0.5, (
        f"Layer {layer_idx + 1} roof emissivity produced QN shift {delta_qn.max()}"
    )
    assert delta_facet.max() > 10.0, (
        f"Layer {layer_idx + 1} roof emissivity produced LNet_Roof shift {delta_facet.max()}"
    )


@pytest.mark.parametrize("layer_idx", [0, 1, 2])
def test_wall_emissivity_perturbation_per_layer(
    spartacus_midday_window,
    homogeneous_baseline_df,
    layer_idx,
):
    """Lowering walls[i].emis shifts Lup, QN, and LNet_Wall_(i+1) for every layer."""
    base_dict, window_forcing = spartacus_midday_window
    df_pert = _run_3layer_case(
        base_dict,
        window_forcing,
        wall_emis=_with_layer_value(BASE_WALL_EMIS, layer_idx, 0.60),
    )

    delta_lup = (
        df_pert["SUEWS", "Lup"] - homogeneous_baseline_df["SUEWS", "Lup"]
    ).abs()
    delta_qn = (df_pert["SUEWS", "QN"] - homogeneous_baseline_df["SUEWS", "QN"]).abs()
    delta_facet = (
        df_pert["SPARTACUS", f"LNet_Wall_{layer_idx + 1}"]
        - homogeneous_baseline_df["SPARTACUS", f"LNet_Wall_{layer_idx + 1}"]
    ).abs()

    assert np.isfinite(delta_lup.to_numpy()).all()
    assert np.isfinite(delta_qn.to_numpy()).all()
    assert np.isfinite(delta_facet.to_numpy()).all()
    assert delta_lup.max() > 0.1, (
        f"Layer {layer_idx + 1} wall emissivity produced Lup shift {delta_lup.max()}"
    )
    assert delta_qn.max() > 0.1, (
        f"Layer {layer_idx + 1} wall emissivity produced QN shift {delta_qn.max()}"
    )
    assert delta_facet.max() > 1.0, (
        f"Layer {layer_idx + 1} wall emissivity produced LNet_Wall shift {delta_facet.max()}"
    )


def test_layer1_perturbation_is_not_broadcast_to_upper_layers(
    spartacus_midday_window,
    homogeneous_baseline_df,
):
    """Perturbing layer 1 alone must produce a strictly smaller shift than perturbing all 3 layers."""
    base_dict, window_forcing = spartacus_midday_window
    df_layer1_alb = _run_3layer_case(
        base_dict,
        window_forcing,
        roof_alb=(0.70, 0.30, 0.30),
    )
    df_all_alb = _run_3layer_case(
        base_dict,
        window_forcing,
        roof_alb=(0.70, 0.70, 0.70),
    )
    kup_l1_series = (
        df_layer1_alb["SUEWS", "Kup"] - homogeneous_baseline_df["SUEWS", "Kup"]
    )
    kup_all_series = (
        df_all_alb["SUEWS", "Kup"] - homogeneous_baseline_df["SUEWS", "Kup"]
    )
    assert np.isfinite(kup_l1_series.to_numpy()).all()
    assert np.isfinite(kup_all_series.to_numpy()).all()
    kup_l1 = kup_l1_series.max()
    kup_all = kup_all_series.max()
    assert 0.0 < kup_l1 < 0.5 * kup_all, (
        f"Expected layer-1-only Kup shift ({kup_l1:.3f}) to be strictly smaller than "
        f"uniform 3-layer shift ({kup_all:.3f})"
    )

    df_layer1_emis = _run_3layer_case(
        base_dict,
        window_forcing,
        roof_emis=(0.60, 0.90, 0.90),
    )
    df_all_emis = _run_3layer_case(
        base_dict,
        window_forcing,
        roof_emis=(0.60, 0.60, 0.60),
    )
    lup_l1_series = (
        df_layer1_emis["SUEWS", "Lup"] - homogeneous_baseline_df["SUEWS", "Lup"]
    ).abs()
    lup_all_series = (
        df_all_emis["SUEWS", "Lup"] - homogeneous_baseline_df["SUEWS", "Lup"]
    ).abs()
    assert np.isfinite(lup_l1_series.to_numpy()).all()
    assert np.isfinite(lup_all_series.to_numpy()).all()
    lup_l1 = lup_l1_series.max()
    lup_all = lup_all_series.max()
    assert 0.0 < lup_l1 < 0.5 * lup_all, (
        f"Expected layer-1-only Lup shift ({lup_l1:.3f}) to be strictly smaller than "
        f"uniform 3-layer shift ({lup_all:.3f})"
    )


def test_heterogeneous_3layer_radiative_and_energy_balance_closure(
    spartacus_midday_window,
):
    """Vertically heterogeneous 3-layer optical/emissivity profile closes SW, LW, and SEB."""
    base_dict, window_forcing = spartacus_midday_window
    df = _run_3layer_case(
        base_dict,
        window_forcing,
        roof_alb=(0.20, 0.45, 0.70),
        wall_alb=(0.25, 0.45, 0.65),
        roof_emis=(0.95, 0.85, 0.70),
        wall_emis=(0.95, 0.82, 0.68),
    )

    suews = df["SUEWS"]
    spc = df["SPARTACUS"]

    # Bulk all-wave radiation identity: QN == Kdown - Kup + Ldown - Lup == SPARTACUS Qn.
    qn_components = suews["Kdown"] - suews["Kup"] + suews["Ldown"] - suews["Lup"]
    for name, arr in (
        ("SUEWS.QN", suews["QN"].to_numpy()),
        ("SPARTACUS.Qn", spc["Qn"].to_numpy()),
        ("qn_components", qn_components.to_numpy()),
    ):
        assert np.isfinite(arr).all(), f"Non-finite values in {name}"
    np.testing.assert_allclose(
        suews["QN"].to_numpy(),
        spc["Qn"].to_numpy(),
        rtol=0.0,
        atol=1e-6,
        err_msg="SUEWS QN must match SPARTACUS Qn",
    )
    np.testing.assert_allclose(
        suews["QN"].to_numpy(),
        qn_components.to_numpy(),
        rtol=0.0,
        atol=1e-4,
        err_msg="All-wave radiation QN must equal Kdown - Kup + Ldown - Lup",
    )

    # Canopy shortwave budget closure across ground, 3 roof/wall layers, veg, and clear air.
    sw_absorbed = spc["KNet_Grnd"].to_numpy().copy()
    lw_absorbed = spc["LNet_Grnd"].to_numpy().copy()
    for lay in (1, 2, 3):
        sw_absorbed += (
            spc[f"KNet_Roof_{lay}"].to_numpy() * spc[f"sfr_Roof_{lay}"].to_numpy()
            + spc[f"KNet_Wall_{lay}"].to_numpy() * spc[f"sfr_Wall_{lay}"].to_numpy()
            + spc[f"KVegAbs_{lay}"].to_numpy()
            + spc[f"KCAAbs_{lay}"].to_numpy()
        )
        lw_absorbed += (
            spc[f"LNet_Roof_{lay}"].to_numpy() * spc[f"sfr_Roof_{lay}"].to_numpy()
            + spc[f"LNet_Wall_{lay}"].to_numpy() * spc[f"sfr_Wall_{lay}"].to_numpy()
            + spc[f"LVegAbs_{lay}"].to_numpy()
            + spc[f"LCAAbs_{lay}"].to_numpy()
        )

    for name, arr in (
        ("sw_absorbed", sw_absorbed),
        ("SPARTACUS.KTopNet", spc["KTopNet"].to_numpy()),
        ("lw_absorbed", lw_absorbed),
        ("SPARTACUS.LTopNet", spc["LTopNet"].to_numpy()),
    ):
        assert np.isfinite(arr).all(), f"Non-finite values in {name}"
    np.testing.assert_allclose(
        sw_absorbed,
        spc["KTopNet"].to_numpy(),
        rtol=0.0,
        atol=1e-5,
        err_msg="SPARTACUS 3-layer SW facet/canopy absorption must equal KTopNet",
    )
    np.testing.assert_allclose(
        lw_absorbed,
        spc["LTopNet"].to_numpy(),
        rtol=0.0,
        atol=1e-2,
        err_msg="SPARTACUS 3-layer LW facet/canopy absorption must equal LTopNet",
    )

    # Coupled SUEWS surface energy balance closure: QN + QF = QS + QH + QE.
    seb_residual = suews["QN"] + suews["QF"] - suews["QS"] - suews["QH"] - suews["QE"]
    assert np.isfinite(seb_residual.to_numpy()).all()
    np.testing.assert_allclose(
        seb_residual.to_numpy(),
        0.0,
        rtol=0.0,
        atol=1e-2,
        err_msg="Coupled SUEWS surface energy balance must close for heterogeneous 3-layer SPARTACUS",
    )
