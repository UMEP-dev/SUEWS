"""Deterministic snow accumulation, melt and clearance budgets (gh#747).

These tests run the sample site with ``model.physics.snow_use`` enabled under a
synthetic, fully controlled forcing so that every phase of a snow event is
present and its expected direction is known a priori:

* day 0: cold (-5 degC), dry -- spin-up so surface temperatures sit below zero;
* day 1: cold, steady snowfall of 1 mm h-1 (24 mm in total) -- accumulation;
* day 2: cold, dry -- the pack is held (only sublimation/deposition acts);
* days 3-4: warm (+5 degC), dry -- degree-day and radiation melt;
* days 5-7: warm, dry -- snow-free control after the pack has gone.

The assertions are *internal consistency* checks: mass and energy conservation
of the model's own outputs, the documented degree-day melt law, and the
qualitative behaviour of albedo, density and fractions. They are not an
observational validation and do not change the experimental status of the snow
module. Historical snow reference outputs shipped with the legacy fixtures are
not used as truth.

Two of the tests are regressions for kernel defects found while adding this
coverage (see the linked issue): the updated snow/hydrology states were written
to local copies inside ``SUEWS_cal_snow`` and never returned to the model state,
so no snowpack could ever accumulate; and the grid aggregates ``SWE``,
``MeltWStore`` and ``SnowRemoval`` were re-zeroed for every surface, so they only
ever carried the last (water) surface.
"""

from __future__ import annotations

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.data_model.core.model import SnowUse

pytestmark = pytest.mark.physics

# Scenario definition -------------------------------------------------------

N_DAYS = 8
DAY_SPIN = 0
DAY_SNOWFALL = 1
DAY_HOLD = 2
DAYS_MELT = (3, 4)
FIRST_SNOW_FREE_DAY = 5

T_COLD_C = -5.0
T_WARM_C = 5.0
SNOWFALL_RATE_MM_H = 1.0
SNOWFALL_TOTAL_MM = SNOWFALL_RATE_MM_H * 24.0
TSTEP_H = 24.0 / TIMESTEPS_PER_DAY  # 5 min in hours

# Physical constants as used by ``suews_phys_snow.f95``
WATER_DENSITY_KG_M3 = 999.8395
# Latent heat of fusion range accepted for the melt-heat / meltwater ratio.
# The Fortran uses ``lvS_J_kg - lv_J_kg`` (sublimation minus vaporisation);
# the vaporisation term falls with air temperature (about 2.4 kJ kg-1 K-1) so
# the difference is 3.34e5 J kg-1 at 0 degC and about 3.45e5 J kg-1 at +5 degC.
LATENT_HEAT_FUSION_RANGE_J_KG = (3.30e5, 3.50e5)

SURFACES = ("Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water")
LAND_COVER_KEYS = {
    "Paved": "paved",
    "Bldgs": "bldgs",
    "EveTr": "evetr",
    "DecTr": "dectr",
    "Grass": "grass",
    "BSoil": "bsoil",
    "Water": "water",
}


class SnowRun:
    """Container for one synthetic snow-event run and its derived series."""

    def __init__(self) -> None:
        sim = SUEWSSimulation.from_sample_data()
        cfg = sim._config
        cfg.model.physics.snow_use = SnowUse.ENABLED
        # Per-timestep output so budgets can be checked step by step.
        cfg.model.control.output.freq = int(TSTEP_H * 3600)
        # The scenario runs for N_DAYS, not the sample config's full year. Declare
        # that period explicitly: since gh#1268 a requested period the forcing does
        # not cover is rejected rather than silently run on the overlap.
        scenario_index = sim.forcing.df.index[: TIMESTEPS_PER_DAY * N_DAYS]
        cfg.model.control.start_time = scenario_index[0].strftime("%Y-%m-%d")
        # Interval-end stamping: the final row is stamped at the midnight closing
        # the last day, so step back one timestep to name that day.
        cfg.model.control.end_time = (
            scenario_index[-1] - pd.Timedelta(hours=TSTEP_H)
        ).strftime("%Y-%m-%d")
        sim._df_state_init = cfg.to_df_state()

        snow_prm = cfg.sites[0].properties.snow
        self.temp_melt_factor = snow_prm.temperature_melt_factor.value
        self.snow_albedo_max = snow_prm.snow_albedo_max.value
        self.snow_albedo_min = snow_prm.snow_albedo_min.value
        self.snow_density_min = snow_prm.snow_density_min.value
        self.snow_density_max = snow_prm.snow_density_max.value
        land_cover = cfg.sites[0].properties.land_cover
        self.sfr = {
            surf: getattr(land_cover, key).sfr.value
            for surf, key in LAND_COVER_KEYS.items()
        }

        forcing = sim.forcing.df.copy().iloc[: TIMESTEPS_PER_DAY * N_DAYS].copy()
        day = (
            ((forcing.index - forcing.index[0]).total_seconds() // 86400)
            .astype(int)
            .to_numpy()
        )
        forcing["Tair"] = np.where(day <= DAY_HOLD, T_COLD_C, T_WARM_C)
        forcing["rain"] = np.where(
            day == DAY_SNOWFALL, SNOWFALL_RATE_MM_H * TSTEP_H, 0.0
        )
        forcing["RH"] = 80.0
        forcing["U"] = 3.0
        forcing["pres"] = 1013.0
        sim.update_forcing(forcing)

        # n_jobs=1: keep the scientific evidence independent of gh#1741.
        out = sim.run(n_jobs=1).df
        self.suews = out.xs("SUEWS", level="group", axis=1).xs(1, level="grid")
        self.snow = out.xs("snow", level="group", axis=1).xs(1, level="grid")
        self.day = pd.Series(
            ((self.suews.index - self.suews.index[0]).total_seconds() // 86400).astype(
                int
            ),
            index=self.suews.index,
        )

    # -- derived series ----------------------------------------------------

    def snow_fraction(self, surf: str) -> pd.Series:
        """Snow fraction per surface; water is all-or-nothing and not output."""
        if surf == "Water":
            return (self.snow["SWE_Water"] > 0).astype(float)
        return self.snow[f"fr_{surf}"]

    def area_weighted_pack(self, exclude_water: bool) -> pd.Series:
        """Grid snow water equivalent rebuilt from the per-surface outputs."""
        total = pd.Series(0.0, index=self.snow.index)
        for surf in SURFACES:
            if exclude_water and surf == "Water":
                continue
            total += (
                self.snow[f"SWE_{surf}"] * self.sfr[surf] * self.snow_fraction(surf)
            )
        return total

    def area_weighted(self, prefix: str, exclude_water: bool = False) -> pd.Series:
        """Area- and snow-fraction-weighted sum of a snow-group per-surface variable."""
        total = pd.Series(0.0, index=self.snow.index)
        for surf in SURFACES:
            if exclude_water and surf == "Water":
                continue
            total += (
                self.snow[f"{prefix}_{surf}"]
                * self.sfr[surf]
                * self.snow_fraction(surf)
            )
        return total

    def surface_state(self) -> pd.Series:
        """Area-weighted surface water state from the SUEWS ``St*`` columns."""
        return sum(self.suews[f"St{surf}"] * self.sfr[surf] for surf in SURFACES)

    def on_days(self, *days: int) -> pd.Series:
        return self.day.isin(days)


@pytest.fixture(scope="module")
def run() -> SnowRun:
    return SnowRun()


# Regression tests for the kernel defects ------------------------------------


@pytest.mark.core
def test_snowpack_accumulates_across_timesteps(run: SnowRun):
    """A day of snowfall at -5 degC must build a pack close to the snowfall total.

    Regression: before the snow state was written back to the model state, the
    per-surface SWE stayed pinned at one timestep of precipitation
    (1/12 mm) because every step restarted from the initial (empty) pack.
    Sublimation at -5 degC and 80 % RH is well below 1 mm day-1, so the pack
    at the end of the snowfall day must lie within 1 mm of the 24 mm fallen.
    """
    swe_paved = run.snow["SWE_Paved"][run.on_days(DAY_SNOWFALL)]

    assert swe_paved.is_monotonic_increasing
    assert SNOWFALL_TOTAL_MM - 1.0 <= swe_paved.iloc[-1] <= SNOWFALL_TOTAL_MM
    # The pack survives the cold hold day (only sublimation acts).
    swe_hold = run.snow["SWE_Paved"][run.on_days(DAY_HOLD)]
    assert swe_hold.iloc[-1] > SNOWFALL_TOTAL_MM - 2.0


@pytest.mark.core
def test_grid_swe_is_area_weighted_sum_of_surface_packs(run: SnowRun):
    """``SUEWS.SWE`` must aggregate every land-cover pack, not only the last one.

    Regression: ``swe`` was re-zeroed inside the per-surface loop of
    ``SUEWS_cal_snow`` so the grid column only ever carried the water surface.
    ``SnowCalc`` accumulates ``SWE`` over the non-water surfaces (the water
    branch keeps its own book-keeping), so the identity is checked against the
    non-water sum. During the hold day the snow fractions are constant, so the
    identity is exact; during melt the depletion curve updates the fraction
    after the sum is taken, so only bounds are asserted there.
    """
    grid = run.suews["SWE"]
    rebuilt = run.area_weighted_pack(exclude_water=True)
    unweighted = sum(
        run.snow[f"SWE_{s}"] * run.sfr[s] for s in SURFACES if s != "Water"
    )

    hold = run.on_days(DAY_HOLD)
    assert grid[hold].max() > SNOWFALL_TOTAL_MM * 0.5
    np.testing.assert_allclose(grid[hold], rebuilt[hold], rtol=0, atol=1e-9)

    melt = run.on_days(*DAYS_MELT)
    assert (grid[melt] >= rebuilt[melt] - 1e-9).all()
    assert (grid[melt] <= unweighted[melt] + 1e-9).all()

    # The melt-water store aggregates the same way (non-water surfaces, snow
    # fraction taken before the depletion-curve update).
    store_grid = run.suews["MeltWStore"]
    store_rebuilt = run.area_weighted("MwStore", exclude_water=True)
    store_unweighted = sum(
        run.snow[f"MwStore_{s}"] * run.sfr[s] for s in SURFACES if s != "Water"
    )
    assert store_grid[melt].max() > 0.0
    assert (store_grid[melt] >= store_rebuilt[melt] - 1e-9).all()
    assert (store_grid[melt] <= store_unweighted[melt] + 1e-9).all()


# Conservation --------------------------------------------------------------


def test_energy_balance_closes_with_snow_terms(run: SnowRun):
    """QN + QF + QMRain = QH + QE + QS + QM + QMFreeze at every timestep.

    ``QH`` is the residual of this identity in the driver, so closure is a
    guard on the output wiring of the snow heat terms rather than on physics.
    """
    s = run.suews
    residual = (s["QN"] + s["QF"] + s["QMRain"]) - (
        s["QH"] + s["QE"] + s["QS"] + s["QM"] + s["QMFreeze"]
    )
    np.testing.assert_allclose(residual.to_numpy(), 0.0, rtol=0, atol=1e-8)
    # Melt heat is present only while the pack melts.
    assert s["QM"][run.on_days(*DAYS_MELT)].mean() > 0.0
    assert not s["QM"][run.on_days(DAY_SPIN, FIRST_SNOW_FREE_DAY)].any()


def test_water_budget_closes_over_the_snow_event(run: SnowRun):
    """Rain - Evap - RO = d(pack + melt-water store + surface state).

    Per timestep during accumulation and hold the budget closes to better
    than 1e-3 mm. Over the whole event (snowfall through to the last melt) the
    cumulative residual is bounded by 1 % of the snowfall: the only open terms
    are the snow-fraction weighting at the step where a pack disappears and
    evaporation drawn from the soil store, which the snow path does not report
    (``SMD`` is not updated while snow is on; that limitation is documented on
    the linked issue and is not covered here).
    """
    s = run.suews
    store = (
        run.area_weighted_pack(exclude_water=False)
        + run.area_weighted("MwStore")
        + run.surface_state()
    )
    residual = s["Rain"] - s["Evap"] - s["RO"] - store.diff()

    steady = run.on_days(DAY_SNOWFALL, DAY_HOLD)
    steady.iloc[0] = False  # the first step has no previous state
    assert residual[steady].abs().max() < 1e-3

    event = run.on_days(DAY_SNOWFALL, DAY_HOLD, *DAYS_MELT)
    assert abs(residual[event].sum()) < 0.01 * SNOWFALL_TOTAL_MM
    assert s["Rain"][event].sum() == pytest.approx(SNOWFALL_TOTAL_MM, rel=1e-9)


# Melt law ------------------------------------------------------------------


def test_night_melt_follows_degree_day_law(run: SnowRun):
    """At night with a melting pack, Mw = TempMeltFact * Tair * dt.

    ``MeltHeat`` uses the hourly degree-day factor whenever the snow surface net
    radiation is negative (here: every night-time step, K_down = 0) and the pack
    exceeds one step of melt.
    """
    night = run.on_days(DAYS_MELT[0]) & (run.suews["Kdown"] <= 0.0)
    assert night.sum() > 12
    expected = run.temp_melt_factor * T_WARM_C * TSTEP_H
    np.testing.assert_allclose(run.snow["Mw_Paved"][night], expected, rtol=1e-6, atol=0)
    # No melt at all while it is cold.
    assert not run.snow["Mw_Paved"][run.on_days(DAY_SNOWFALL, DAY_HOLD)].any()


def test_melt_heat_is_latent_heat_of_fusion_times_meltwater(run: SnowRun):
    """Qm = rho_w * L_f * Mw / dt for every melting step."""
    melting = run.on_days(*DAYS_MELT) & (run.snow["Mw_Paved"] > 1e-6)
    ratio = run.snow["Qm_Paved"][melting] / run.snow["Mw_Paved"][melting]
    latent_heat = ratio * (TSTEP_H * 3600) * 1000.0 / WATER_DENSITY_KG_M3
    lo, hi = LATENT_HEAT_FUSION_RANGE_J_KG
    assert (latent_heat > lo).all() and (latent_heat < hi).all()
    assert latent_heat.std() < 1.0  # constant within a run


# Snow-free transition and state evolution ------------------------------------


def test_pack_clears_and_stays_clear(run: SnowRun):
    """Melt removes the pack; afterwards every snow output is identically zero."""
    clear = run.on_days(*range(FIRST_SNOW_FREE_DAY, N_DAYS))
    for prefix in ("SWE", "fr", "Sd", "Mw", "MwStore", "Qm"):
        for surf in SURFACES:
            col = f"{prefix}_{surf}"
            if col not in run.snow:
                continue
            if col == "MwStore_Water":
                # The water-body branch of ``SnowCalc`` does not empty its
                # melt-water store when the ice cover disappears; the small
                # constant remainder is a documented follow-up, not asserted.
                continue
            assert not run.snow[col][clear].any(), col
    for col in ("SWE", "MeltWater", "MeltWStore", "QM", "SnowCh"):
        assert not run.suews[col][clear].any(), col

    # Total meltwater balances the snowfall corrected for sublimation and
    # deposition on the pack over the event.
    event = run.on_days(DAY_SNOWFALL, DAY_HOLD, *DAYS_MELT)
    melted = run.suews["MeltWater"].sum()
    expected = SNOWFALL_TOTAL_MM - run.suews["Evap"][event].sum()
    assert melted == pytest.approx(expected, abs=0.02 * SNOWFALL_TOTAL_MM)


def test_snow_albedo_resets_on_snowfall_and_ages(run: SnowRun):
    """Fresh snow resets the albedo to its maximum; it then decays, bounded below."""
    alb = run.snow["SnowAlb"]
    assert alb[run.on_days(DAY_SNOWFALL)].max() == pytest.approx(run.snow_albedo_max)
    hold = alb[run.on_days(DAY_HOLD)]
    assert (hold.diff().dropna() <= 1e-12).all()
    with_snow = run.on_days(DAY_HOLD, *DAYS_MELT) & (run.snow["SWE_Paved"] > 0)
    assert alb[with_snow].min() >= run.snow_albedo_min


def test_snow_density_and_depth_are_consistent(run: SnowRun):
    """Fresh snow takes the minimum density, ages upward and stays bounded.

    Depth is reported as SWE * rho_water / rho_snow in metres (the unit the
    kernel produces) and is diagnosed in ``MeltHeat`` from the pack carried
    into the timestep, so it lags the reported SWE by one step; the density
    ageing within a step is below 1 %, hence the tolerance.
    """
    dens = run.snow["DensSnow_Paved"]
    swe = run.snow["SWE_Paved"]
    first_snow = swe[swe > 0].index[0]
    assert dens[first_snow] == pytest.approx(run.snow_density_min)
    hold = dens[run.on_days(DAY_HOLD)]
    assert (hold.diff().dropna() >= -1e-12).all()
    assert dens.max() <= run.snow_density_max

    swe_in = swe.shift(1)
    with_snow = (swe_in > 0) & (swe > 0)
    depth_expected = (
        swe_in[with_snow] * WATER_DENSITY_KG_M3 / (1000.0 * dens[with_snow])
    )
    np.testing.assert_allclose(
        run.snow["Sd_Paved"][with_snow], depth_expected, rtol=1e-2, atol=0
    )
