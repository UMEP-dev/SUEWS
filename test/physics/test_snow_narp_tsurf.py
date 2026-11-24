"""Regression tests for snow longwave emission temperature and sentinel 3000 in NARP (gh#2106).

In ``src/suews/src/suews_phys_narp.f95``, two snow-radiation defects previously
caused ``TSURF_SNOW_K`` to evaluate to uninitialised ``tsurf_0_K`` (``0.0 K``
under ``-finit-real=zero``):

1. ``NetRadiationMethod = 0`` with ``SnowUse = 1`` sets the internal sentinel
   ``NetRadiationMethod_use = 3000`` in ``RadMethod``, which missed the
   ``NetRadiationMethod_use < 10`` guards in both the snow-free and snow
   emission blocks of ``NARP``.
2. Iterative surface-temperature schemes (``11, 12, 13, 1001, 1002, 1003``)
   assigned ``TSURF_SNOW_K = tsurf_0_K`` instead of tracking the iterated
   surface temperature capped at the melting point ``MIN(tsfc_surf_K(is), C2K)``.
"""

from __future__ import annotations

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-RESPONSE", "C-BALANCE")]

C2K = 273.15
SIGMA_SB = 5.670374419e-8
SURFACES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
ITERATIVE_METHODS = (11, 12, 13, 1001, 1002, 1003)


def _run_snow_narp_day(
    make_sim,
    *,
    net_radiation: int,
    tair_c: float,
    snow_use: int = 1,
    snowpack_mm: float = 50.0,
    snowfrac: float = 1.0,
    storage_heat: int = 5,
    snow_emissivity: float = 0.99,
    ldown_wm2: float = 240.0,
    fcld_obs: float = 0.4,
    qn_obs_offset: float = -25.0,
    ehc_surface_coupling: int | None = None,
):
    """Run one synthetic dry day with explicit snow state and radiation scheme."""
    sim = make_sim()
    forcing = sim.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()
    forcing["rain"] = 0.0
    forcing["Tair"] = tair_c
    forcing["RH"] = 80.0
    forcing["U"] = 3.0
    forcing["pres"] = 1013.0
    forcing["ldown"] = ldown_wm2
    forcing["fcld"] = fcld_obs
    forcing["qn"] = qn_obs_offset + 0.1 * forcing["kdown"]
    forcing["snow"] = snowfrac if snowpack_mm > 0.0 else 0.0

    sites = [site.model_dump() for site in sim.config.sites]
    site = sites[0]
    for surf in SURFACES:
        frac = snowfrac if (snowpack_mm > 0.0 and surf != "water") else 0.0
        state = site["initial_states"][surf]
        state.update(
            snowfrac=frac,
            snowpack=snowpack_mm if frac > 0.0 else 0.0,
            snowdens=150.0 if frac > 0.0 else 0.0,
            snowwater=0.0,
            icefrac=0.0,
        )
    site["initial_states"]["snowalb"] = 0.8 if snowpack_mm > 0.0 else 0.0
    site["properties"]["snow"]["narp_emissivity_snow"] = snow_emissivity

    # Clear upper-layer vegetation above tree height so SPARTACUS checks accept
    # the matched EHC site configuration alongside NARP schemes.
    layers = site["properties"]["vertical_layers"]
    for key in ("veg_frac", "veg_scale"):
        layers[key]["value"][2] = 0.0

    physics = {
        "net_radiation": net_radiation,
        "snow_use": snow_use,
        "storage_heat": storage_heat,
    }
    if ehc_surface_coupling is not None:
        physics["ehc_surface_coupling"] = ehc_surface_coupling

    sim.update_config(
        {
            "sites": sites,
            "model": {
                "physics": physics,
                "control": {"output": {"freq": 300}},
            },
        },
        auto_load_forcing=False,
    )
    sim.update_forcing(forcing)
    df = sim.run(
        start_date=forcing.index[0],
        end_date=forcing.index[-1],
        n_jobs=1,
    ).df
    suews = df.xs("SUEWS", level="group", axis=1).xs(1, level="grid")
    snow = df.xs("snow", level="group", axis=1).xs(1, level="grid")
    ehc = df.xs("EHC", level="group", axis=1).xs(1, level="grid")
    return forcing, suews, snow, ehc


def _implied_lup_snow(suews, snow, surface: str = "Paved"):
    """Recover per-surface snow outgoing longwave from NARP radiation balance."""
    return (
        suews["Kdown"]
        - snow[f"kup_{surface}Snow"]
        + suews["Ldown"]
        - snow[f"Qn_{surface}Snow"]
    )


@pytest.mark.core
@pytest.mark.parametrize("snowfrac", [1.0, 0.5], ids=["full-snow", "partial-snow"])
def test_observed_qn_with_snow_routes_sentinel_3000_to_standard_narp(
    short_sample_sim, snowfrac
):
    """NetRadiationMethod=0 + SnowUse=1 must keep QN=qn1_obs and standard NARP emission."""
    tair_c = -5.0
    emis = 0.99

    forcing_obs, suews_obs, snow_obs, _ = _run_snow_narp_day(
        short_sample_sim,
        net_radiation=0,
        tair_c=tair_c,
        snowpack_mm=50.0,
        snowfrac=snowfrac,
        storage_heat=1,
        snow_emissivity=emis,
    )
    _, suews_ref, snow_ref, _ = _run_snow_narp_day(
        short_sample_sim,
        net_radiation=3,
        tair_c=tair_c,
        snowpack_mm=50.0,
        snowfrac=snowfrac,
        storage_heat=1,
        snow_emissivity=emis,
    )

    # Preserve QSTARall = qn1_obs (line 506 sentinel 3000 contract).
    np.testing.assert_allclose(
        suews_obs["QN"].to_numpy(),
        forcing_obs["qn"].to_numpy(),
        rtol=0,
        atol=1e-6,
    )
    # Confirm synthetic qn1_obs is distinct from modelled QN so the assertion
    # cannot pass accidentally.
    assert np.max(np.abs(suews_obs["QN"] - suews_ref["QN"])) > 5.0

    # Both snow-free and snow blocks must route sentinel 3000 into standard NARP.
    expected_temp_k = tair_c + C2K
    np.testing.assert_allclose(
        snow_obs["Tsnow_Paved"].to_numpy(),
        expected_temp_k,
        rtol=0,
        atol=1e-4,
    )
    lup_snow = _implied_lup_snow(suews_obs, snow_obs, "Paved")
    expected_lup_snow = (
        emis * SIGMA_SB * expected_temp_k**4 + (1.0 - emis) * suews_obs["Ldown"]
    )
    assert np.all(np.isfinite(lup_snow))
    assert float(lup_snow.min()) > 250.0
    np.testing.assert_allclose(
        lup_snow.to_numpy(),
        expected_lup_snow.to_numpy(),
        rtol=0,
        atol=1e-2,
    )
    np.testing.assert_allclose(
        snow_obs["Qn_PavedSnow"].to_numpy(),
        snow_ref["Qn_PavedSnow"].to_numpy(),
        rtol=0,
        atol=1e-2,
    )
    # First timestep has identical snow fraction before any feedback from QN;
    # snow-free + snow LUP and Tsurf must match standard NARP method 3.
    assert suews_obs["Lup"].iloc[0] == pytest.approx(suews_ref["Lup"].iloc[0], abs=1e-2)
    assert suews_obs["Tsurf"].iloc[0] == pytest.approx(
        suews_ref["Tsurf"].iloc[0], abs=1e-3
    )


@pytest.mark.core
@pytest.mark.parametrize("method", ITERATIVE_METHODS, ids=lambda m: f"netrad_{m}")
def test_iterative_snow_longwave_tracks_cold_tsfc_surf(short_sample_sim, method):
    """Iterative radiation schemes must emit sub-freezing snow LW using tsfc_surf_K."""
    tair_c = -5.0
    emis = 0.99

    _, suews, snow, ehc = _run_snow_narp_day(
        short_sample_sim,
        net_radiation=method,
        tair_c=tair_c,
        snowpack_mm=50.0,
        snowfrac=1.0,
        storage_heat=5,
        snow_emissivity=emis,
    )

    tsnow_k = snow["Tsnow_Paved"]
    tsfc_k = ehc["Ts_Paved"] + C2K

    # Sub-freezing snowpack tracks iterated tsfc_surf_K rather than 0 K.
    assert np.all(np.isfinite(tsnow_k))
    assert float(tsnow_k.min()) > 240.0
    assert float(tsnow_k.max()) < C2K
    np.testing.assert_allclose(
        tsnow_k.to_numpy(),
        np.minimum(tsfc_k.to_numpy(), C2K),
        rtol=0,
        atol=0.5,
    )

    # Snow outgoing longwave obeys Stefan-Boltzmann with Tsnow_Paved.
    lup_snow = _implied_lup_snow(suews, snow, "Paved")
    expected_lup_snow = emis * SIGMA_SB * tsnow_k**4 + (1.0 - emis) * suews["Ldown"]
    assert float(lup_snow.min()) > 240.0
    np.testing.assert_allclose(
        lup_snow.to_numpy(),
        expected_lup_snow.to_numpy(),
        rtol=0,
        atol=1e-2,
    )

    # Energy balance closure remains exact with snow terms.
    residual = (suews["QN"] + suews["QF"] + suews["QMRain"]) - (
        suews["QH"] + suews["QE"] + suews["QS"] + suews["QM"] + suews["QMFreeze"]
    )
    np.testing.assert_allclose(residual.to_numpy(), 0.0, rtol=0, atol=1e-6)


@pytest.mark.core
@pytest.mark.parametrize("method", (13, 1003), ids=lambda m: f"netrad_{m}")
def test_iterative_snow_qn_no_longer_inflated_by_missing_emission(
    short_sample_sim, method
):
    """Snow Qn under methods 13 and 1003 must align with method 3 instead of +290 W m-2."""
    tair_c = -5.0
    emis = 0.99

    _, _, snow_ref, _ = _run_snow_narp_day(
        short_sample_sim,
        net_radiation=3,
        tair_c=tair_c,
        snowpack_mm=50.0,
        snowfrac=1.0,
        storage_heat=5,
        snow_emissivity=emis,
    )
    _, _, snow_iter, _ = _run_snow_narp_day(
        short_sample_sim,
        net_radiation=method,
        tair_c=tair_c,
        snowpack_mm=50.0,
        snowfrac=1.0,
        storage_heat=5,
        snow_emissivity=emis,
    )

    delta_qn = (snow_iter["Qn_PavedSnow"] - snow_ref["Qn_PavedSnow"]).to_numpy()
    # Missing emission at -5 degC inflated Qn_PavedSnow by +290.2 W m-2.
    # With Tsnow tracking sub-freezing tsfc_surf_K, the difference reflects
    # only the physical tsfc_surf vs Tair offset (well under 40 W m-2).
    assert float(np.max(np.abs(delta_qn))) < 40.0


@pytest.mark.core
@pytest.mark.parametrize("method", ITERATIVE_METHODS, ids=lambda m: f"netrad_{m}")
def test_iterative_snow_longwave_caps_warm_emission_at_melting_point(
    short_sample_sim, method
):
    """When iterated tsfc_surf exceeds 0 degC over snow, emission caps at C2K (273.15 K)."""
    emis = 0.99

    _, suews, snow, ehc = _run_snow_narp_day(
        short_sample_sim,
        net_radiation=method,
        tair_c=10.0,
        snowpack_mm=500.0,
        snowfrac=1.0,
        storage_heat=5,
        snow_emissivity=emis,
        ldown_wm2=320.0,
    )

    warm_snow_mask = (ehc["Ts_Paved"] > 0.5) & (snow["fr_Paved"] > 0.0)
    assert int(warm_snow_mask.sum()) >= 100

    tsnow_warm = snow["Tsnow_Paved"][warm_snow_mask]
    np.testing.assert_allclose(
        tsnow_warm.to_numpy(),
        C2K,
        rtol=0,
        atol=1e-4,
    )

    lup_snow = _implied_lup_snow(suews, snow, "Paved")[warm_snow_mask]
    expected_lup_snow = (
        emis * SIGMA_SB * C2K**4 + (1.0 - emis) * suews["Ldown"][warm_snow_mask]
    )
    np.testing.assert_allclose(
        lup_snow.to_numpy(),
        expected_lup_snow.to_numpy(),
        rtol=0,
        atol=1e-2,
    )


@pytest.mark.core
@pytest.mark.parametrize("method", (13, 1003), ids=lambda m: f"netrad_{m}")
@pytest.mark.parametrize(
    ("ehc_surface_coupling", "min_excess_wm2"),
    [
        pytest.param(0, 4.0, id="explicit"),
        pytest.param(None, 3.0, id="default_implicit"),
    ],
)
def test_iterative_snow_longwave_responds_with_physical_sign_to_emissivity(
    short_sample_sim, method, ehc_surface_coupling, min_excess_wm2
):
    """Lowering snow emissivity below 1 must reduce net outgoing LW when sigma*T^4 > Ldown."""
    tair_c = -5.0

    _, suews_hi, snow_hi, _ = _run_snow_narp_day(
        short_sample_sim,
        net_radiation=method,
        tair_c=tair_c,
        snowpack_mm=50.0,
        snowfrac=1.0,
        storage_heat=5,
        snow_emissivity=0.99,
        ehc_surface_coupling=ehc_surface_coupling,
    )
    _, suews_lo, snow_lo, _ = _run_snow_narp_day(
        short_sample_sim,
        net_radiation=method,
        tair_c=tair_c,
        snowpack_mm=50.0,
        snowfrac=1.0,
        storage_heat=5,
        snow_emissivity=0.90,
        ehc_surface_coupling=ehc_surface_coupling,
    )

    lup_hi = _implied_lup_snow(suews_hi, snow_hi, "Paved")
    lup_lo = _implied_lup_snow(suews_lo, snow_lo, "Paved")

    # On broken master (Tsnow = 0 K), lowering emissivity increased LUP_SNOW
    # because only reflected (1 - emis) * Ldown remained (~ -21.6 W m-2).
    # With physical emission (sigma*Tsnow^4 > Ldown at -5 degC), higher
    # emissivity emits more LUP_SNOW: explicit coupling keeps the original
    # > 4.0 W m-2 margin, while coupled implicit EHC radiative cooling lowers
    # Tsfc slightly more at emis=0.99 than 0.90 (minimum excess ~3.75 W m-2).
    assert float((lup_hi - lup_lo).min()) > min_excess_wm2
