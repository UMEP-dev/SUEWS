"""Water- and energy-balance regressions for impervious evaporation shortfall (gh#1951).

When dry paved or building surfaces cannot supply their initial Penman-Monteith
evaporation demand (``ev0``), ``cal_water_storage`` transfers the unmet
shortfall (``ev_shortfall_imperv``) to the land-pervious surfaces
(``ConifSurf:BSoilSurf``) as ``ev_from_imperv``. Open water (``WaterSurf``)
does not receive ``ev_from_imperv``, so dividing by ``PervFraction =
1 - (f_paved + f_bldgs)`` left a share ``f_water / PervFraction`` unallocated,
dropping it out of ``QE`` and into residual ``QH``.

These tests verify on controlled dry summer scenarios (2012-07-04, 5-minute
output) that:

- With impervious cover, land-pervious cover, and open water all present, the
  entire impervious evaporation shortfall is transferred to the land-pervious
  surfaces proportionally to their area fractions, open water is unaffected,
  and both water and energy balances close.
- With zero land-pervious cover and open water present, the explicit zero-cover
  guard keeps all outputs finite and preserves water and energy closure.
- With zero open water, shortfall transfer and budget closure hold identically.
"""

from __future__ import annotations

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-BALANCE", "C-RESPONSE")]

GRID = 1
SHORTFALL_TOLERANCE_MM = 1e-10
WATER_CLOSURE_TOLERANCE_MM = 1e-6
ENERGY_CLOSURE_TOLERANCE_WM2 = 1e-6

ALL_SURFACES = ("Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water")
LAND_PERVIOUS_SURFACES = ("EveTr", "DecTr", "Grass", "BSoil")
SURFACE_TO_CONFIG_KEY = {
    "Paved": "paved",
    "Bldgs": "bldgs",
    "EveTr": "evetr",
    "DecTr": "dectr",
    "Grass": "grass",
    "BSoil": "bsoil",
    "Water": "water",
}


def _run_dry_summer_day(
    sfrs: dict[str, float],
    *,
    initial_soilstore_mm: float = 120.0,
) -> tuple[SUEWSSimulation, pd.DataFrame]:
    """Run one rain-free summer day (2012-07-04) at 5-minute output resolution."""
    sample = SUEWSSimulation.from_sample_data()
    cfg = sample.config.model_copy(deep=True)
    cfg.model.control.output.freq = 300

    land_cover = cfg.sites[0].properties.land_cover
    for surf, frac in sfrs.items():
        getattr(land_cover, SURFACE_TO_CONFIG_KEY[surf]).sfr.value = frac

    # Ensure zero external water inflow to open water so the water balance
    # closes cleanly on Rain + Irr - Evap - RO.
    land_cover.water.flowchange.value = 0.0

    # Start impervious surfaces completely dry so daytime demand produces a
    # genuine evaporation shortfall, and give land-pervious surfaces ample
    # soil water so they can absorb the redistributed demand.
    init = cfg.sites[0].initial_states
    init.paved.state.value = 0.0
    init.bldgs.state.value = 0.0
    for surf in LAND_PERVIOUS_SURFACES:
        state_lc = getattr(init, SURFACE_TO_CONFIG_KEY[surf])
        state_lc.state.value = 0.0
        state_lc.soilstore.value = initial_soilstore_mm

    forcing = sample.forcing.df.loc["2012-07-04":"2012-07-04"].copy()
    assert len(forcing) == TIMESTEPS_PER_DAY
    forcing["rain"] = 0.0

    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(forcing)
    output = sim.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)
    return sim, output.df


def _water_balance_residual_mm(
    sim: SUEWSSimulation, df_output: pd.DataFrame
) -> pd.Series:
    """Per-timestep water-balance residual (dTotalStore - (Rain + Irr - Evap - RO)) [mm]."""
    suews = df_output.loc[GRID, "SUEWS"]
    debug = df_output.loc[GRID, "debug"]
    sfr = sim.state_init.sfr_surf.iloc[0].to_numpy(dtype=float)
    soil_next = debug[[f"ss_{s}_next" for s in ALL_SURFACES]].to_numpy(dtype=float)
    soil_store = pd.Series(soil_next @ sfr, index=suews.index)
    total_store = soil_store + suews["State"]
    net_input = suews["Rain"] + suews["Irr"] - suews["Evap"] - suews["RO"]
    return (total_store.diff() - net_input).dropna()


def _energy_balance_residual_wm2(df_output: pd.DataFrame) -> pd.Series:
    """Per-timestep energy-balance residual (QN + QF - QH - QE - QS) [W m-2]."""
    suews = df_output.loc[GRID, "SUEWS"]
    return suews["QN"] + suews["QF"] - suews["QH"] - suews["QE"] - suews["QS"]


@pytest.mark.core
def test_impervious_evap_shortfall_fully_allocated_with_open_water() -> None:
    """Impervious shortfall is fully redistributed to land-pervious surfaces when open water is present."""
    # ARRANGE
    sfrs = {
        "Paved": 0.40,
        "Bldgs": 0.30,
        "EveTr": 0.03,
        "DecTr": 0.02,
        "Grass": 0.05,
        "BSoil": 0.05,
        "Water": 0.15,
    }
    land_perv_fraction = sum(sfrs[s] for s in LAND_PERVIOUS_SURFACES)

    # ACT
    sim, df_output = _run_dry_summer_day(sfrs)
    debug = df_output.loc[GRID, "debug"]

    # ASSERT: daytime steps produce substantial impervious evaporation shortfall
    shortfall_paved = debug["ev0_Paved"] - debug["ev_Paved"]
    shortfall_bldgs = debug["ev0_Bldgs"] - debug["ev_Bldgs"]
    shortfall_grid = shortfall_paved * sfrs["Paved"] + shortfall_bldgs * sfrs["Bldgs"]

    active_shortfall = shortfall_grid > 1e-6
    assert int(active_shortfall.sum()) > 50, (
        "dry summer scenario must generate positive impervious evaporation shortfall"
    )
    assert float(shortfall_grid.max()) > 1e-3

    # Every land-pervious surface still has soil moisture and receives the exact
    # per-unit-area transfer shortfall_grid / land_perv_fraction.
    expected_transfer = shortfall_grid / land_perv_fraction
    for surf in LAND_PERVIOUS_SURFACES:
        assert (debug[f"ss_{surf}_next"] > 0.0).all(), (
            f"soil store depleted unexpectedly on {surf}"
        )
        actual_transfer = debug[f"ev_{surf}"] - debug[f"ev0_{surf}"]
        np.testing.assert_allclose(
            actual_transfer.to_numpy(dtype=float),
            expected_transfer.to_numpy(dtype=float),
            rtol=0.0,
            atol=SHORTFALL_TOLERANCE_MM,
            err_msg=f"shortfall transfer mismatch on {surf}",
        )

    # Open water does not receive impervious shortfall.
    assert (debug["st_Water_next"] > 0.0).all()
    np.testing.assert_allclose(
        debug["ev_Water"].to_numpy(dtype=float),
        debug["ev0_Water"].to_numpy(dtype=float),
        rtol=0.0,
        atol=SHORTFALL_TOLERANCE_MM,
    )

    # Grid-level actual evaporation matches full unclipped demand (zero unallocated share).
    sfr_vec = np.array([sfrs[s] for s in ALL_SURFACES], dtype=float)
    ev0_grid = debug[[f"ev0_{s}" for s in ALL_SURFACES]].to_numpy(dtype=float) @ sfr_vec
    ev_grid = debug[[f"ev_{s}" for s in ALL_SURFACES]].to_numpy(dtype=float) @ sfr_vec
    np.testing.assert_allclose(
        ev_grid,
        ev0_grid,
        rtol=0.0,
        atol=SHORTFALL_TOLERANCE_MM,
        err_msg="grid evaporation lost unallocated impervious shortfall",
    )

    # Exact water-balance and energy-balance closure.
    wb_residual = _water_balance_residual_mm(sim, df_output)
    eb_residual = _energy_balance_residual_wm2(df_output)
    assert float(wb_residual.abs().max()) < WATER_CLOSURE_TOLERANCE_MM
    assert float(eb_residual.abs().max()) < ENERGY_CLOSURE_TOLERANCE_WM2


@pytest.mark.core
def test_zero_land_pervious_with_open_water_closes_budgets_without_nan() -> None:
    """Zero land-pervious cover with open water guards against division by zero and closes budgets."""
    # ARRANGE
    sfrs = {
        "Paved": 0.50,
        "Bldgs": 0.30,
        "EveTr": 0.0,
        "DecTr": 0.0,
        "Grass": 0.0,
        "BSoil": 0.0,
        "Water": 0.20,
    }

    # ACT
    sim, df_output = _run_dry_summer_day(sfrs)
    suews = df_output.loc[GRID, "SUEWS"]
    debug = df_output.loc[GRID, "debug"]

    # ASSERT: all core fluxes and states remain finite
    for col in ("QN", "QF", "QS", "QH", "QE", "Evap", "RO", "State"):
        assert np.isfinite(suews[col].to_numpy(dtype=float)).all(), (
            f"non-finite values in SUEWS.{col} with zero land-pervious cover"
        )

    # Open water evaporates at its own rate without absorbing impervious shortfall.
    assert (debug["st_Water_next"] > 0.0).all()
    np.testing.assert_allclose(
        debug["ev_Water"].to_numpy(dtype=float),
        debug["ev0_Water"].to_numpy(dtype=float),
        rtol=0.0,
        atol=SHORTFALL_TOLERANCE_MM,
    )

    wb_residual = _water_balance_residual_mm(sim, df_output)
    eb_residual = _energy_balance_residual_wm2(df_output)
    assert float(wb_residual.abs().max()) < WATER_CLOSURE_TOLERANCE_MM
    assert float(eb_residual.abs().max()) < ENERGY_CLOSURE_TOLERANCE_WM2


@pytest.mark.core
def test_zero_open_water_conserves_shortfall_transfer_and_budgets() -> None:
    """Without open water, shortfall redistribution equals shortfall / land_perv_fraction and closes budgets."""
    # ARRANGE
    sfrs = {
        "Paved": 0.40,
        "Bldgs": 0.30,
        "EveTr": 0.0,
        "DecTr": 0.0,
        "Grass": 0.30,
        "BSoil": 0.0,
        "Water": 0.0,
    }

    # ACT
    sim, df_output = _run_dry_summer_day(sfrs)
    debug = df_output.loc[GRID, "debug"]

    # ASSERT
    shortfall_grid = (
        (debug["ev0_Paved"] - debug["ev_Paved"]) * sfrs["Paved"]
        + (debug["ev0_Bldgs"] - debug["ev_Bldgs"]) * sfrs["Bldgs"]
    )
    assert float(shortfall_grid.max()) > 1e-3

    actual_transfer = debug["ev_Grass"] - debug["ev0_Grass"]
    expected_transfer = shortfall_grid / sfrs["Grass"]
    np.testing.assert_allclose(
        actual_transfer.to_numpy(dtype=float),
        expected_transfer.to_numpy(dtype=float),
        rtol=0.0,
        atol=SHORTFALL_TOLERANCE_MM,
    )

    wb_residual = _water_balance_residual_mm(sim, df_output)
    eb_residual = _energy_balance_residual_wm2(df_output)
    assert float(wb_residual.abs().max()) < WATER_CLOSURE_TOLERANCE_MM
    assert float(eb_residual.abs().max()) < ENERGY_CLOSURE_TOLERANCE_WM2
