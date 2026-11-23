"""Representative parameter-sensitivity and inactive-invariance physics suite.

Complements schema and bridge Jacobian tests by asserting end-to-end physical
responsiveness through ``SUEWSSimulation`` (YAML -> Rust engine -> Fortran
kernel) across major SUEWS physics subsystems:

- Surface radiation (albedo and emissivity)
- Aerodynamics / roughness (MacDonald building height)
- Storage heat (OHM ``a1`` and EHC thermal properties)
- Surface conductance / evaporation (``conductance.g_max``)
- Drainage / interception capacity and soil storage capacity
- Anthropogenic heat (``qf_a``)
- SPARTACUS multi-layer radiation (roof albedo)
- Snow accumulation, reflection, and melt (``snow_albedo_max``,
  ``temperature_melt_factor``)

Every active perturbation checks three properties:
1. Non-vacuous sensitivity (``max|dFlux| > eps``),
2. Physically expected sign where deterministic, and
3. Surface energy-balance conservation on both baseline and perturbed runs, plus
   water-balance conservation except where a documented pre-existing hydrology
   exception applies (snow-path runs under gh#1905/#1927/#1958, and combined
   EHC + SPARTACUS building-facet runs where ``SUEWS_cal_QE`` routes building
   storage through ``cal_water_storage_building`` and divides
   ``soilstore_building / capStore_builing`` (``0/0`` leaving ``SMDBldgs``,
   ``SMD``, and ``TotCh`` as ``NaN`` when ``bldgs.sfr == 0``)).

Every inactive perturbation (disabled physics module or ``sfr == 0`` surface)
verifies strict invariance (``max|dFlux| == 0.0``) across grid-level fluxes.
"""

from __future__ import annotations

import copy

from conftest import TIMESTEPS_PER_DAY
from layer5_checks import (
    SUEWS_ABSENT_OBSERVED_LDOWN,
    SUEWS_MISSING_SENTINEL,
    assert_active_output_finiteness,
    assert_simulation_output_finiteness,
    expected_dailystate_mask,
    strict_finite_array,
    strict_groupby_reduce,
    strict_max_abs,
    strict_mean,
    strict_sum,
    water_balance_residual_mm,
)
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig
from supy.data_model.core.model import (
    MomentumRoughnessMethod,
    NetRadiationMethod,
    SnowUse,
    StorageHeatMethod,
)

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-RESPONSE", "C-BALANCE")]

WATER_CLOSURE_TOLERANCE_MM = 1e-4
ENERGY_CLOSURE_TOLERANCE_W_M2 = 1e-6
GRID_FLUX_COLUMNS = (
    "QN",
    "Kup",
    "Lup",
    "QF",
    "QH",
    "QE",
    "QS",
    "Evap",
    "RO",
    "State",
    "SMD",
    "T2",
    "RH2",
    "U10",
    "UStar",
)


def _make_repeated_forcing(
    base_day: pd.DataFrame, start_date: str, n_days: int
) -> pd.DataFrame:
    """Repeat one 24-hour 5-minute forcing day across ``n_days`` calendar days."""
    start_ts = pd.Timestamp(start_date) + pd.Timedelta(minutes=5)
    index = pd.date_range(start_ts, periods=TIMESTEPS_PER_DAY * n_days, freq="300s")
    df_forcing = pd.concat(
        [base_day.reset_index(drop=True) for _ in range(n_days)],
        ignore_index=True,
    )
    df_forcing.index = index
    df_forcing["iy"] = index.year
    df_forcing["id"] = index.day_of_year
    df_forcing["it"] = index.hour
    df_forcing["imin"] = index.minute
    df_forcing["isec"] = index.second
    return df_forcing


def _run_and_verify_conservation(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    *,
    check_water_balance: bool = True,
    include_snow_energy_terms: bool = False,
) -> pd.DataFrame:
    """Run ``config`` via ``SUEWSSimulation`` and assert finiteness and conservation."""
    sim = SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    output = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    )
    df_out = output.df

    phys = config.model.physics
    storage_heat = int(getattr(phys.storage_heat, "value", phys.storage_heat))
    net_radiation = int(getattr(phys.net_radiation, "value", phys.net_radiation))
    snow_use = int(getattr(phys.snow_use, "value", phys.snow_use))
    nlayer_cfg = config.sites[0].properties.vertical_layers.nlayer
    nlayer = int(getattr(nlayer_cfg, "value", nlayer_cfg))
    assert_simulation_output_finiteness(
        df_out,
        expected_grids=(1,),
        expected_timestamps=df_forcing.index,
        tstep=300,
        storage_heat=storage_heat,
        net_radiation=net_radiation,
        snow_use=snow_use,
        nlayer=nlayer,
    )

    suews = df_out.loc[1, "SUEWS"]
    if include_snow_energy_terms:
        eb_residual = (suews["QN"] + suews["QF"] + suews["QMRain"]) - (
            suews["QH"] + suews["QE"] + suews["QS"] + suews["QM"] + suews["QMFreeze"]
        )
    else:
        eb_residual = (
            suews["QN"] + suews["QF"] - suews["QH"] - suews["QE"] - suews["QS"]
        )
    max_eb = strict_max_abs(eb_residual, label="energy_balance_residual")
    assert max_eb < ENERGY_CLOSURE_TOLERANCE_W_M2, (
        f"Surface energy balance not closed: max residual {max_eb:.3e} W m-2"
    )

    if check_water_balance:
        max_wb = water_balance_residual_mm(
            df_out, sim.state_init, grid=1, expected_timestamps=df_forcing.index
        )
        assert max_wb < WATER_CLOSURE_TOLERANCE_MM, (
            f"Surface water balance not closed: max residual {max_wb:.3e} mm"
        )

    return df_out


@pytest.fixture(scope="module")
def sensitivity_env(short_sample_yaml_path):
    """Shared base config, synthetic forcing regimes, and cached baseline output."""
    base_cfg = SUEWSConfig.from_yaml(str(short_sample_yaml_path))
    sim = SUEWSSimulation(str(short_sample_yaml_path))
    day_template = sim.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()

    # Controlled warm sunny dry day: strong radiation and turbulent fluxes.
    df_dry_day = day_template.copy()
    df_dry_day["Tair"] = 20.0
    df_dry_day["RH"] = 55.0
    df_dry_day["U"] = 3.5
    df_dry_day["pres"] = 1013.0
    df_dry_day["kdown"] = np.maximum(
        0.0, 650.0 * np.sin(np.pi * (np.arange(TIMESTEPS_PER_DAY) - 72) / 144.0)
    )
    df_dry_day["rain"] = 0.0

    # Same warm sunny day with a 3-hour morning rainfall pulse for drainage/soil tests.
    df_rain_day = df_dry_day.copy()
    df_rain_day.iloc[60:96, df_rain_day.columns.get_loc("rain")] = 1.5

    # 3-day cold snowfall (day 0) -> cold sunlit hold (day 1) -> warm melt (day 2).
    df_snow_3d = _make_repeated_forcing(df_dry_day, "2012-01-01", n_days=3)
    day_idx = (
        ((df_snow_3d.index - df_snow_3d.index[0]).total_seconds() // 86400)
        .astype(int)
        .to_numpy()
    )
    df_snow_3d["Tair"] = np.where(day_idx <= 1, -5.0, 5.0)
    df_snow_3d["rain"] = np.where(day_idx == 0, 1.0 / 12.0, 0.0)
    df_snow_3d["RH"] = 80.0

    # Cache baseline outputs for the dry and rainy single-day runs.
    df_out_dry_base = _run_and_verify_conservation(base_cfg, df_dry_day)
    df_out_rain_base = _run_and_verify_conservation(base_cfg, df_rain_day)

    return {
        "base_cfg": base_cfg,
        "base_state_init": sim.state_init.copy(),
        "df_dry_day": df_dry_day,
        "df_rain_day": df_rain_day,
        "df_snow_3d": df_snow_3d,
        "snow_day_idx": day_idx,
        "out_dry_base": df_out_dry_base,
        "out_rain_base": df_out_rain_base,
    }


# =============================================================================
# Active parameter sensitivity across physics subsystems
# =============================================================================


@pytest.mark.core
def test_surface_albedo_sensitivity_increases_kup_and_reduces_qn(sensitivity_env):
    """Higher paved albedo must reflect more shortwave (Kup) and reduce QN."""
    # ARRANGE
    cfg_pert = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_pert.sites[0].properties.land_cover.paved.alb.value = 0.35
    df_dry = sensitivity_env["df_dry_day"]
    daytime = (df_dry["kdown"] > 50.0).to_numpy()

    # ACT
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_dry)

    # ASSERT
    s_base = sensitivity_env["out_dry_base"].loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_kup = (s_pert["Kup"] - s_base["Kup"]).to_numpy()[daytime]
    d_qn = (s_pert["QN"] - s_base["QN"]).to_numpy()[daytime]
    d_qh = (s_pert["QH"] - s_base["QH"]).to_numpy()[daytime]

    assert strict_max_abs(d_kup, label="albedo.dKup") > 10.0
    assert (strict_finite_array(d_kup, label="albedo.dKup") > 0.0).all(), (
        "Higher albedo must strictly increase daytime Kup"
    )
    assert strict_mean(d_kup, label="albedo.dKup") > 10.0
    assert strict_mean(d_qn, label="albedo.dQN") < -10.0
    assert strict_max_abs(d_qh, label="albedo.dQH") > 5.0


@pytest.mark.core
def test_surface_emissivity_sensitivity_changes_lup_and_qn(sensitivity_env):
    """Lower paved emissivity must reduce emitted longwave (Lup) and raise QN."""
    # ARRANGE
    cfg_pert = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_pert.sites[0].properties.land_cover.paved.emis.value = 0.75
    df_dry = sensitivity_env["df_dry_day"]

    # ACT
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_dry)

    # ASSERT
    s_base = sensitivity_env["out_dry_base"].loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_lup = (s_pert["Lup"] - s_base["Lup"]).to_numpy()
    d_qn = (s_pert["QN"] - s_base["QN"]).to_numpy()

    assert strict_max_abs(d_lup, label="emis.dLup") > 1.0
    assert strict_mean(d_lup, label="emis.dLup") < -1.0
    assert strict_mean(d_qn, label="emis.dQN") > 1.0


@pytest.mark.core
def test_aerodynamics_building_height_sensitivity_increases_ustar(sensitivity_env):
    """Taller buildings under MacDonald roughness must increase friction velocity."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.model.physics.roughness_length_momentum = MomentumRoughnessMethod.MACDONALD
    cfg_pert = copy.deepcopy(cfg_base)
    cfg_pert.sites[0].properties.land_cover.bldgs.bldgh.value = 35.0
    df_dry = sensitivity_env["df_dry_day"]

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_dry)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_dry)

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_ustar = (s_pert["UStar"] - s_base["UStar"]).to_numpy()
    d_u10 = (s_pert["U10"] - s_base["U10"]).to_numpy()
    d_qh = (s_pert["QH"] - s_base["QH"]).to_numpy()

    assert strict_max_abs(d_ustar, label="bldgh.dUStar") > 0.02
    assert strict_mean(d_ustar, label="bldgh.dUStar") > 0.02
    assert strict_max_abs(d_u10, label="bldgh.dU10") > 0.1
    assert strict_max_abs(d_qh, label="bldgh.dQH") > 0.5


@pytest.mark.core
def test_ohm_storage_heat_a1_sensitivity_shifts_qs_and_qh(sensitivity_env):
    """Reducing OHM ``a1`` must reduce daytime storage heat QS and raise QH."""
    # ARRANGE
    cfg_pert = copy.deepcopy(sensitivity_env["base_cfg"])
    ohm_paved = cfg_pert.sites[0].properties.land_cover.paved.ohm_coef
    for regime in ("summer_dry", "summer_wet", "winter_dry", "winter_wet"):
        getattr(ohm_paved, regime).a1.value = 0.40
    df_dry = sensitivity_env["df_dry_day"]
    daytime = (df_dry["kdown"] > 50.0).to_numpy()

    # ACT
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_dry)

    # ASSERT
    s_base = sensitivity_env["out_dry_base"].loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_qs = (s_pert["QS"] - s_base["QS"]).to_numpy()[daytime]
    d_qh = (s_pert["QH"] - s_base["QH"]).to_numpy()[daytime]

    assert strict_max_abs(d_qs, label="ohm.dQS") > 10.0
    assert strict_mean(d_qs, label="ohm.dQS") < -10.0
    assert strict_mean(d_qh, label="ohm.dQH") > 10.0


@pytest.mark.core
def test_ehc_thermal_layers_sensitivity_shifts_qs_and_qh(sensitivity_env):
    """Raising EHC thermal conductivity and heat capacity must shift QS and QH."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.model.physics.storage_heat = StorageHeatMethod.EHC
    cfg_pert = copy.deepcopy(cfg_base)
    layers = cfg_pert.sites[0].properties.land_cover.paved.thermal_layers
    layers.k.value = [2.2] * 5
    layers.rho_cp.value = [4.4e6, 4.4e6, 4.4e6, 4.4e6, 5.2e6]
    df_dry = sensitivity_env["df_dry_day"]

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_dry)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_dry)

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_qs = (s_pert["QS"] - s_base["QS"]).to_numpy()
    d_qh = (s_pert["QH"] - s_base["QH"]).to_numpy()

    assert strict_max_abs(d_qs, label="ehc.dQS") > 5.0
    assert strict_max_abs(d_qh, label="ehc.dQH") > 5.0


@pytest.mark.core
def test_conductance_gmax_sensitivity_increases_qe_and_evaporation(sensitivity_env):
    """Higher maximum surface conductance must raise daytime QE/Evap and lower QH."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.sites[0].properties.land_cover.paved.sfr.value = 0.16
    cfg_base.sites[0].properties.land_cover.grass.sfr.value = 0.30
    cfg_base.sites[0].initial_states.grass.lai_id.value = 3.0
    cfg_pert = copy.deepcopy(cfg_base)
    cfg_pert.sites[0].properties.conductance.g_max.value = 12.0
    df_dry = sensitivity_env["df_dry_day"]
    daytime = (df_dry["kdown"] > 50.0).to_numpy()

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_dry)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_dry)

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_qe = (s_pert["QE"] - s_base["QE"]).to_numpy()[daytime]
    d_qh = (s_pert["QH"] - s_base["QH"]).to_numpy()[daytime]
    d_evap_total = strict_sum(s_pert["Evap"] - s_base["Evap"], label="gmax.dEvap")

    assert strict_max_abs(d_qe, label="gmax.dQE") > 10.0
    assert strict_mean(d_qe, label="gmax.dQE") > 10.0
    assert d_evap_total > 0.05
    assert strict_mean(d_qh, label="gmax.dQH") < -10.0


@pytest.mark.core
def test_drainage_storage_capacity_sensitivity_retains_state_and_reduces_runoff(
    sensitivity_env,
):
    """Raising threshold-drainage ``store_cap`` must retain more water and cut RO."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    paved_base = cfg_base.sites[0].properties.land_cover.paved
    paved_base.state_limit.value = 4.0
    paved_base.storage_drain_params.drain_eq.value = 1
    paved_base.storage_drain_params.store_cap.value = 0.48
    paved_base.storage_drain_params.store_max.value = 0.48
    paved_base.storage_drain_params.store_min.value = 0.48

    cfg_pert = copy.deepcopy(cfg_base)
    cfg_pert.sites[
        0
    ].properties.land_cover.paved.storage_drain_params.store_cap.value = 2.5
    df_rain = sensitivity_env["df_rain_day"]

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_rain)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_rain)

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_state = (s_pert["State"] - s_base["State"]).to_numpy()
    d_ro_total = strict_sum(s_pert["RO"] - s_base["RO"], label="store_cap.dRO")

    assert strict_max_abs(d_state, label="store_cap.dState") > 0.05
    assert float(
        np.max(strict_finite_array(s_pert["State"], label="store_cap.State_pert"))
    ) > float(
        np.max(strict_finite_array(s_base["State"], label="store_cap.State_base"))
    )
    assert d_ro_total < -0.05


@pytest.mark.core
def test_soil_store_capacity_sensitivity_increases_runoff_when_reduced(sensitivity_env):
    """Reducing ``grass.soil_store_capacity`` at fixed initial soil store increases RO."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.sites[0].properties.land_cover.paved.sfr.value = 0.16
    cfg_base.sites[0].properties.land_cover.grass.sfr.value = 0.30
    cfg_base.sites[0].properties.land_cover.grass.soil_store_capacity.value = 150.0
    cfg_base.sites[0].initial_states.grass.soilstore.value = 120.0

    cfg_pert = copy.deepcopy(cfg_base)
    cfg_pert.sites[0].properties.land_cover.grass.soil_store_capacity.value = 125.0
    df_rain = sensitivity_env["df_rain_day"]

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_rain)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_rain)

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_ro = (s_pert["RO"] - s_base["RO"]).to_numpy()

    assert strict_max_abs(d_ro, label="soil_cap.dRO") > 0.01
    assert strict_sum(d_ro, label="soil_cap.dRO") > 0.05


@pytest.mark.core
def test_anthropogenic_heat_qfa_sensitivity_increases_qf_and_qh(sensitivity_env):
    """Raising ``qf_a`` must strictly increase QF at every step and raise QH."""
    # ARRANGE
    cfg_pert = copy.deepcopy(sensitivity_env["base_cfg"])
    heat = cfg_pert.sites[0].properties.anthropogenic_emissions.heat
    heat.qf_a.working_day = 0.80
    heat.qf_a.holiday = 0.80
    df_dry = sensitivity_env["df_dry_day"]

    # ACT
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_dry)

    # ASSERT
    s_base = sensitivity_env["out_dry_base"].loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_qf = (s_pert["QF"] - s_base["QF"]).to_numpy()
    d_qh = (s_pert["QH"] - s_base["QH"]).to_numpy()

    assert (strict_finite_array(d_qf, label="qfa.dQF") > 0.0).all(), (
        "Higher qf_a must strictly increase QF at every timestep"
    )
    assert strict_max_abs(d_qf, label="qfa.dQF") > 10.0
    assert strict_mean(d_qh, label="qfa.dQH") > 10.0


@pytest.mark.core
def test_spartacus_roof_albedo_sensitivity_increases_kup_and_reduces_qn(
    sensitivity_env,
):
    """Higher SPARTACUS roof albedo must raise daytime Kup and reduce QN."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.model.physics.net_radiation = NetRadiationMethod.LDOWN_SS_AIR
    cfg_base.model.physics.storage_heat = StorageHeatMethod.STEBBS
    cfg_base.model.physics.stebbs.enabled = True
    vl_base = cfg_base.sites[0].properties.vertical_layers
    vl_base.building_frac.value[0] = cfg_base.sites[
        0
    ].properties.land_cover.bldgs.sfr.value
    vl_base.veg_frac.value[2] = 0.0
    vl_base.veg_scale.value[2] = 0.0

    cfg_pert = copy.deepcopy(cfg_base)
    for roof in cfg_pert.sites[0].properties.vertical_layers.roofs:
        roof.alb.value = 0.80
    df_dry = sensitivity_env["df_dry_day"]
    daytime = (df_dry["kdown"] > 50.0).to_numpy()

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_dry)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_dry)

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_kup = (s_pert["Kup"] - s_base["Kup"]).to_numpy()[daytime]
    d_qn = (s_pert["QN"] - s_base["QN"]).to_numpy()[daytime]
    d_qh = (s_pert["QH"] - s_base["QH"]).to_numpy()[daytime]

    assert strict_max_abs(d_kup, label="spartacus.dKup") > 10.0
    assert strict_mean(d_kup, label="spartacus.dKup") > 10.0
    assert strict_mean(d_qn, label="spartacus.dQN") < -10.0
    assert strict_max_abs(d_qh, label="spartacus.dQH") > 5.0


@pytest.mark.core
def test_snow_melt_factor_and_albedo_sensitivity_with_energy_closure(sensitivity_env):
    """Snow melt factor must accelerate melt and snow albedo must raise cold-day Kup."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.model.physics.snow_use = SnowUse.ENABLED

    cfg_melt = copy.deepcopy(cfg_base)
    cfg_melt.sites[0].properties.snow.temperature_melt_factor.value = 0.40

    cfg_alb = copy.deepcopy(cfg_base)
    cfg_alb.sites[0].properties.snow.snow_albedo_max.value = 0.95

    df_snow = sensitivity_env["df_snow_3d"]
    day_idx = sensitivity_env["snow_day_idx"]

    # ACT
    # Snow-path water closure is tracked separately (gh#1905/#1927/#1958); assert
    # full snow energy balance closure (QN + QF + QMRain = QH + QE + QS + QM + QMFreeze).
    df_out_base = _run_and_verify_conservation(
        cfg_base,
        df_snow,
        check_water_balance=False,
        include_snow_energy_terms=True,
    )
    df_out_melt = _run_and_verify_conservation(
        cfg_melt,
        df_snow,
        check_water_balance=False,
        include_snow_energy_terms=True,
    )
    df_out_alb = _run_and_verify_conservation(
        cfg_alb,
        df_snow,
        check_water_balance=False,
        include_snow_energy_terms=True,
    )

    # ASSERT: higher temperature_melt_factor depletes SWE faster during warm melt days
    melt_mask = day_idx >= 2
    d_swe = (
        df_out_melt.loc[1, ("snow", "SWE_Paved")]
        - df_out_base.loc[1, ("snow", "SWE_Paved")]
    ).to_numpy()[melt_mask]
    assert strict_max_abs(d_swe, label="snow.dSWE") > 1.0
    assert strict_mean(d_swe, label="snow.dSWE") < -1.0

    # ASSERT: higher snow_albedo_max raises reflected shortwave on sunlit cold snowpack
    cold_daytime = (day_idx == 1) & (df_snow["kdown"].to_numpy() > 50.0)
    d_kup = (
        df_out_alb.loc[1, ("SUEWS", "Kup")] - df_out_base.loc[1, ("SUEWS", "Kup")]
    ).to_numpy()[cold_daytime]
    d_qn = (
        df_out_alb.loc[1, ("SUEWS", "QN")] - df_out_base.loc[1, ("SUEWS", "QN")]
    ).to_numpy()[cold_daytime]
    assert strict_max_abs(d_kup, label="snow.dKup") > 10.0
    assert strict_mean(d_kup, label="snow.dKup") > 10.0
    assert strict_mean(d_qn, label="snow.dQN") < -10.0


# =============================================================================
# Inactive invariance (zero-area surfaces and disabled physics modules)
# =============================================================================


@pytest.mark.core
@pytest.mark.parametrize("surface", ["bsoil", "evetr"])
def test_zero_area_surface_hydrology_perturbation_is_invariant(
    sensitivity_env, surface
):
    """Perturbing hydrology/soil parameters of a ``sfr == 0`` surface must leave grid fluxes bit-identical."""
    # ARRANGE
    cfg_pert = copy.deepcopy(sensitivity_env["base_cfg"])
    lc = getattr(cfg_pert.sites[0].properties.land_cover, surface)
    np.testing.assert_equal(
        lc.sfr.value,
        0.0,
        err_msg=f"Sample fixture precondition: {surface}.sfr must be 0",
    )

    if surface == "bsoil":
        lc.storage_drain_params.store_cap.value = 2.5
        lc.storage_drain_params.store_min.value = 2.0
        lc.storage_drain_params.store_max.value = 3.0
    lc.soil_store_capacity.value = 250.0
    lc.saturated_hydraulic_conductivity.value = 0.01

    # ACT / ASSERT under both rainy and dry regimes
    for regime, df_forcing, base_key in (
        ("rain", sensitivity_env["df_rain_day"], "out_rain_base"),
        ("dry", sensitivity_env["df_dry_day"], "out_dry_base"),
    ):
        df_out_pert = _run_and_verify_conservation(cfg_pert, df_forcing)
        s_base = sensitivity_env[base_key].loc[1, "SUEWS"][list(GRID_FLUX_COLUMNS)]
        s_pert = df_out_pert.loc[1, "SUEWS"][list(GRID_FLUX_COLUMNS)]
        np.testing.assert_array_equal(
            s_pert.to_numpy(),
            s_base.to_numpy(),
            err_msg=(
                f"Perturbing zero-area '{surface}' hydrology changed grid fluxes ({regime})"
            ),
        )


@pytest.mark.core
def test_zero_area_surface_radiation_perturbation_leaves_grid_fluxes_invariant(
    sensitivity_env,
):
    """Perturbing radiation parameters of ``evetr`` (``sfr == 0``) must leave grid fluxes invariant."""
    # ARRANGE
    cfg_pert = copy.deepcopy(sensitivity_env["base_cfg"])
    evetr = cfg_pert.sites[0].properties.land_cover.evetr
    np.testing.assert_equal(evetr.sfr.value, 0.0)
    evetr.emis.value = 0.85

    # ACT
    df_out_pert = _run_and_verify_conservation(cfg_pert, sensitivity_env["df_rain_day"])

    # ASSERT
    base_suews = sensitivity_env["out_rain_base"].loc[1, "SUEWS"]
    pert_suews = df_out_pert.loc[1, "SUEWS"]
    np.testing.assert_array_equal(
        pert_suews[list(GRID_FLUX_COLUMNS)].to_numpy(),
        base_suews[list(GRID_FLUX_COLUMNS)].to_numpy(),
        err_msg="Perturbing zero-area evetr.emis changed grid fluxes",
    )
    np.testing.assert_array_equal(
        df_out_pert.loc[1, ("debug", "iter")].to_numpy(),
        sensitivity_env["out_rain_base"].loc[1, ("debug", "iter")].to_numpy(),
        err_msg="Perturbing zero-area evetr.emis changed iteration counts",
    )


@pytest.mark.core
@pytest.mark.parametrize("scheme", ["narp_13", "spartacus_1003", "ehc_5"])
def test_zero_area_surface_radiation_perturbation_invariant_across_coupled_schemes(
    sensitivity_env, scheme
):
    """Absent ``evetr`` emissivity perturbation must be invariant under NARP-13, SPARTACUS-1003, and EHC."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    if scheme == "narp_13":
        cfg_base.model.physics.net_radiation = NetRadiationMethod.LDOWN_AIR_SURFACE
    elif scheme == "spartacus_1003":
        cfg_base.model.physics.net_radiation = NetRadiationMethod.LDOWN_SS_AIR
        cfg_base.model.physics.storage_heat = StorageHeatMethod.STEBBS
        cfg_base.model.physics.stebbs.enabled = True
        vl = cfg_base.sites[0].properties.vertical_layers
        vl.building_frac.value[0] = cfg_base.sites[
            0
        ].properties.land_cover.bldgs.sfr.value
        vl.veg_frac.value[2] = 0.0
        vl.veg_scale.value[2] = 0.0
    else:
        cfg_base.model.physics.storage_heat = StorageHeatMethod.EHC

    cfg_pert = copy.deepcopy(cfg_base)
    evetr = cfg_pert.sites[0].properties.land_cover.evetr
    np.testing.assert_equal(evetr.sfr.value, 0.0)
    evetr.emis.value = 0.85
    df_rain = sensitivity_env["df_rain_day"]

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_rain)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_rain)

    # ASSERT
    np.testing.assert_array_equal(
        df_out_pert.loc[1, "SUEWS"][list(GRID_FLUX_COLUMNS)].to_numpy(),
        df_out_base.loc[1, "SUEWS"][list(GRID_FLUX_COLUMNS)].to_numpy(),
        err_msg=f"Zero-area evetr.emis changed grid fluxes under {scheme}",
    )
    np.testing.assert_array_equal(
        df_out_pert.loc[1, ("debug", "iter")].to_numpy(),
        df_out_base.loc[1, ("debug", "iter")].to_numpy(),
        err_msg=f"Zero-area evetr.emis changed iteration counts under {scheme}",
    )


@pytest.mark.core
def test_all_active_surfaces_respond_to_evetr_emissivity_perturbation(
    sensitivity_env,
):
    """When all 7 surfaces have ``sfr > 0``, perturbing ``evetr.emis`` must alter grid fluxes."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    lc = cfg_base.sites[0].properties.land_cover
    lc.evetr.sfr.value = 0.05
    lc.bsoil.sfr.value = 0.05
    lc.paved.sfr.value = round(lc.paved.sfr.value - 0.10, 10)
    sfr_values = [
        getattr(lc, name).sfr.value
        for name in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
    ]
    assert all(val > 0.0 for val in sfr_values)
    np.testing.assert_allclose(sum(sfr_values), 1.0, rtol=0.0, atol=1e-12)

    cfg_pert = copy.deepcopy(cfg_base)
    cfg_pert.sites[0].properties.land_cover.evetr.emis.value = 0.85
    df_rain = sensitivity_env["df_rain_day"]

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_rain)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_rain)

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_qn = (s_pert["QN"] - s_base["QN"]).to_numpy()
    d_lup = (s_pert["Lup"] - s_base["Lup"]).to_numpy()
    d_qs = (s_pert["QS"] - s_base["QS"]).to_numpy()

    assert strict_max_abs(d_qn, label="all_active_evetr.dQN") > 0.2
    assert strict_max_abs(d_lup, label="all_active_evetr.dLup") > 0.2
    assert strict_max_abs(d_qs, label="all_active_evetr.dQS") > 0.1


@pytest.mark.core
def test_ehc_low_wind_absent_surface_does_not_block_convergence(sensitivity_env):
    """Absent surfaces must not force EHC to stall at ``max_iter = 60`` under low wind."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.model.physics.storage_heat = StorageHeatMethod.EHC
    evetr = cfg_base.sites[0].properties.land_cover.evetr
    np.testing.assert_equal(evetr.sfr.value, 0.0)

    cfg_pert = copy.deepcopy(cfg_base)
    cfg_pert.sites[0].properties.land_cover.evetr.emis.value = 0.85

    df_low_wind = sensitivity_env["df_rain_day"].copy()
    df_low_wind["U"] = 0.35
    df_low_wind["kdown"] = np.maximum(
        0.0,
        900.0 * np.sin(np.pi * (np.arange(len(df_low_wind)) - 72) / 144.0),
    )

    # ACT
    df_out_base = _run_and_verify_conservation(cfg_base, df_low_wind)
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_low_wind)

    # ASSERT: without the sfr_surf > 0 mask, absent evetr hits the +50 K clamp
    # with QS_surf(absent) == 0 and locks step 83 (and 109/288 steps overall)
    # at i_iter == 60; with the mask, step 83 converges well below 60 and
    # absent-surface emissivity perturbations remain bit-identical.
    iters = strict_finite_array(
        df_out_base.loc[1, ("debug", "iter")], label="ehc_low_wind.iter"
    )
    assert iters[83] < 60.0, (
        f"Step 83 failed to converge under low-wind EHC: i_iter={iters[83]:.0f}"
    )
    n_hit_cap = int(np.sum(iters >= 60.0))
    assert n_hit_cap < 60, (
        f"Too many low-wind EHC steps hit max_iter=60: {n_hit_cap}/288"
    )
    np.testing.assert_array_equal(
        df_out_pert.loc[1, "SUEWS"][list(GRID_FLUX_COLUMNS)].to_numpy(),
        df_out_base.loc[1, "SUEWS"][list(GRID_FLUX_COLUMNS)].to_numpy(),
        err_msg="Zero-area evetr.emis changed grid fluxes under low-wind EHC",
    )
    np.testing.assert_array_equal(
        df_out_pert.loc[1, ("debug", "iter")].to_numpy(),
        df_out_base.loc[1, ("debug", "iter")].to_numpy(),
        err_msg="Zero-area evetr.emis changed iteration counts under low-wind EHC",
    )


@pytest.mark.core
def test_ehc_spartacus_absent_building_facets_leave_grid_fluxes_invariant(
    sensitivity_env,
):
    """When ``bldgs.sfr == 0`` under EHC + SPARTACUS, facet perturbations must leave grid fluxes invariant."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.model.physics.net_radiation = NetRadiationMethod.LDOWN_SS_AIR
    cfg_base.model.physics.storage_heat = StorageHeatMethod.EHC
    lc = cfg_base.sites[0].properties.land_cover
    lc.paved.sfr.value = round(lc.paved.sfr.value + lc.bldgs.sfr.value, 10)
    lc.bldgs.sfr.value = 0.0
    np.testing.assert_equal(lc.bldgs.sfr.value, 0.0)
    vl = cfg_base.sites[0].properties.vertical_layers
    vl.building_frac.value = [0.0] * len(vl.building_frac.value)
    vl.veg_frac.value[2] = 0.0
    vl.veg_scale.value[2] = 0.0

    cfg_pert = copy.deepcopy(cfg_base)
    for roof in cfg_pert.sites[0].properties.vertical_layers.roofs:
        roof.alb.value = 0.80
        roof.emis.value = 0.70
        roof.thermal_layers.k.value = [20.0] * 5
        roof.thermal_layers.rho_cp.value = [5.0e6] * 5
        roof.thermal_layers.dz.value = [0.02] * 5
    for wall in cfg_pert.sites[0].properties.vertical_layers.walls:
        wall.alb.value = 0.80
        wall.emis.value = 0.70
        wall.thermal_layers.k.value = [20.0] * 5
        wall.thermal_layers.rho_cp.value = [5.0e6] * 5
        wall.thermal_layers.dz.value = [0.02] * 5
    # Exercise both moderate wind/warm morning (where dif_tsfc_iter > 0.05 K
    # triggers on absent facets) and high wind/cold afternoon (where small RA_h
    # and high-conductance roof/wall QS make dif_qh_iter > 5 W m-2 trigger even
    # when dif_tsfc_iter is guarded) so both sfr_surf(BldgSurf) > 0 guards are
    # individually discriminated.
    df_facet = sensitivity_env["df_rain_day"].copy()
    df_facet["U"] = np.where(np.arange(len(df_facet)) < 144, 3.5, 15.0)
    df_facet["Tair"] = np.where(np.arange(len(df_facet)) < 144, 20.0, 0.0)

    # ACT
    # Run directly rather than through full-frame finiteness: SUEWS_cal_QE:3631
    # has a pre-existing 0/0 in soilstore_building/capStore_builing when
    # bldgs.sfr == 0 (leaving SMDBldgs, SMD, and TotCh as NaN), while every
    # other active SUEWS output column and energy-balance closure remain finite.
    excluded_suews_cols = set(SUEWS_ABSENT_OBSERVED_LDOWN) | {
        "SMDBldgs",
        "SMD",
        "TotCh",
    }
    outputs = []
    checked_suews_cols: list[str] = []
    for label, cfg in (("base", cfg_base), ("pert", cfg_pert)):
        sim = SUEWSSimulation()
        sim.update_config(cfg, auto_load_forcing=False)
        sim.update_forcing(df_facet)
        df_out = sim.run(
            start_date=df_facet.index[0],
            end_date=df_facet.index[-1],
            n_jobs=1,
        ).df
        suews = df_out.loc[1, "SUEWS"]
        assert suews.index.equals(df_facet.index)
        assert excluded_suews_cols.issubset(suews.columns)
        checked_suews_cols = [
            col for col in suews.columns if col not in excluded_suews_cols
        ]
        for col in checked_suews_cols:
            strict_finite_array(
                suews[col], label=f"ehc_spartacus_bldg0.{label}.{col}"
            )
        strict_finite_array(
            df_out.loc[1, ("debug", "iter")],
            label=f"ehc_spartacus_bldg0.{label}.iter",
        )
        eb_residual = (
            suews["QN"] + suews["QF"] - suews["QH"] - suews["QE"] - suews["QS"]
        )
        assert (
            strict_max_abs(eb_residual, label=f"ehc_spartacus_bldg0.{label}.eb")
            < ENERGY_CLOSURE_TOLERANCE_W_M2
        )
        outputs.append(df_out)
    df_out_base, df_out_pert = outputs

    # ASSERT: every active SUEWS column (all columns except Fcld and the three
    # documented capStore_builing 0/0 columns) and debug.iter stay bit-identical.
    np.testing.assert_array_equal(
        df_out_pert.loc[1, "SUEWS"][checked_suews_cols].to_numpy(),
        df_out_base.loc[1, "SUEWS"][checked_suews_cols].to_numpy(),
        err_msg="Absent building-facet perturbation changed SUEWS outputs under EHC + SPARTACUS",
    )
    np.testing.assert_array_equal(
        df_out_pert.loc[1, ("debug", "iter")].to_numpy(),
        df_out_base.loc[1, ("debug", "iter")].to_numpy(),
        err_msg="Absent building-facet perturbation changed iteration counts under EHC + SPARTACUS",
    )


@pytest.mark.core
def test_ehc_spartacus_active_building_facets_respond_to_perturbation(
    sensitivity_env,
):
    """When ``bldgs.sfr > 0`` under EHC + SPARTACUS, facet perturbations must alter grid fluxes."""
    # ARRANGE
    cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
    cfg_base.model.physics.net_radiation = NetRadiationMethod.LDOWN_SS_AIR
    cfg_base.model.physics.storage_heat = StorageHeatMethod.EHC
    bldg_sfr = cfg_base.sites[0].properties.land_cover.bldgs.sfr.value
    assert bldg_sfr > 0.0
    vl = cfg_base.sites[0].properties.vertical_layers
    vl.building_frac.value[0] = bldg_sfr
    vl.veg_frac.value[2] = 0.0
    vl.veg_scale.value[2] = 0.0

    cfg_pert = copy.deepcopy(cfg_base)
    for roof in cfg_pert.sites[0].properties.vertical_layers.roofs:
        roof.alb.value = 0.80
        roof.thermal_layers.rho_cp.value = [4.0e6] * 5
    for wall in cfg_pert.sites[0].properties.vertical_layers.walls:
        wall.thermal_layers.rho_cp.value = [4.0e6] * 5
    df_dry = sensitivity_env["df_dry_day"]

    # ACT
    # Multi-layer building facet water storage under EHC + SPARTACUS routes
    # through cal_water_storage_building rather than the 7-surface soil-store
    # ledger checked by water_balance_residual_mm; assert full output
    # finiteness and strict surface energy-balance closure.
    df_out_base = _run_and_verify_conservation(
        cfg_base, df_dry, check_water_balance=False
    )
    df_out_pert = _run_and_verify_conservation(
        cfg_pert, df_dry, check_water_balance=False
    )

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"]
    s_pert = df_out_pert.loc[1, "SUEWS"]
    d_qn = (s_pert["QN"] - s_base["QN"]).to_numpy()
    d_kup = (s_pert["Kup"] - s_base["Kup"]).to_numpy()
    d_qs = (s_pert["QS"] - s_base["QS"]).to_numpy()
    d_qh = (s_pert["QH"] - s_base["QH"]).to_numpy()

    assert strict_max_abs(d_qn, label="ehc_spartacus_bldg.dQN") > 10.0
    assert strict_max_abs(d_kup, label="ehc_spartacus_bldg.dKup") > 10.0
    assert strict_max_abs(d_qs, label="ehc_spartacus_bldg.dQS") > 5.0
    assert strict_max_abs(d_qh, label="ehc_spartacus_bldg.dQH") > 5.0


@pytest.mark.core
@pytest.mark.parametrize(
    "disabled_module",
    ["snow", "spartacus", "ohm_under_ehc", "ehc_under_ohm"],
)
def test_disabled_module_parameter_perturbation_is_invariant(
    sensitivity_env, disabled_module
):
    """Perturbing a disabled physics module's parameters must yield ``max|dFlux| == 0.0``."""
    # ARRANGE
    df_rain = sensitivity_env["df_rain_day"]
    if disabled_module == "ohm_under_ehc":
        cfg_base = copy.deepcopy(sensitivity_env["base_cfg"])
        cfg_base.model.physics.storage_heat = StorageHeatMethod.EHC
        df_out_base = _run_and_verify_conservation(cfg_base, df_rain)
        cfg_pert = copy.deepcopy(cfg_base)
        for regime in ("summer_dry", "summer_wet", "winter_dry", "winter_wet"):
            getattr(
                cfg_pert.sites[0].properties.land_cover.paved.ohm_coef, regime
            ).a1.value = 0.20
        check_all_suews = True
    else:
        cfg_base = sensitivity_env["base_cfg"]
        df_out_base = sensitivity_env["out_rain_base"]
        cfg_pert = copy.deepcopy(cfg_base)
        if disabled_module == "snow":
            snow = cfg_pert.sites[0].properties.snow
            snow.snow_albedo_max.value = 0.95
            snow.snow_albedo_min.value = 0.45
            snow.temperature_melt_factor.value = 0.50
            snow.radiation_melt_factor.value = 0.01
            check_all_suews = True
        elif disabled_module == "spartacus":
            cfg_pert.sites[0].properties.vertical_layers.roofs[0].alb.value = 0.90
            cfg_pert.sites[0].properties.vertical_layers.walls[0].alb.value = 0.90
            cfg_pert.sites[0].properties.spartacus.veg_ssa_sw.value = 0.90
            check_all_suews = True
        else:
            # storage_heat=1 (OHM): thermal_layers must leave grid-level fluxes invariant.
            layers = cfg_pert.sites[0].properties.land_cover.paved.thermal_layers
            layers.k.value = [5.0] * 5
            layers.rho_cp.value = [5.0e6] * 5
            check_all_suews = False

    # ACT
    df_out_pert = _run_and_verify_conservation(cfg_pert, df_rain)

    # ASSERT
    s_base = df_out_base.loc[1, "SUEWS"].drop(columns=list(SUEWS_ABSENT_OBSERVED_LDOWN))
    s_pert = df_out_pert.loc[1, "SUEWS"].drop(columns=list(SUEWS_ABSENT_OBSERVED_LDOWN))
    if check_all_suews:
        np.testing.assert_array_equal(
            s_pert.to_numpy(),
            s_base.to_numpy(),
            err_msg=(
                f"Perturbing disabled module '{disabled_module}' moved SUEWS output"
            ),
        )
    else:
        grid_base = s_base[list(GRID_FLUX_COLUMNS)]
        grid_pert = s_pert[list(GRID_FLUX_COLUMNS)]
        np.testing.assert_array_equal(
            grid_pert.to_numpy(),
            grid_base.to_numpy(),
            err_msg=(
                f"Perturbing disabled module '{disabled_module}' moved grid fluxes"
            ),
        )


# =============================================================================
# Negative & positive controls for finiteness and alignment
# =============================================================================


def _make_two_day_grid_frame(grid_day1: pd.DataFrame) -> pd.DataFrame:
    """Build an intact 2-day (576-row) single-grid frame by appending day 1 shifted +1 day."""
    day2 = grid_day1.copy()
    day2.index += pd.Timedelta(days=1)
    return pd.concat([grid_day1, day2])


@pytest.mark.core
def test_active_finiteness_rejects_truncation_duplicates_missing_columns_and_sentinels(
    sensitivity_env,
):
    """Damaged time coverage, missing columns, NaNs, and -999 sentinels must fail; -950 passes."""
    # ARRANGE: intact 2-day (576-row) single-grid and MultiIndex positive controls
    day1_grid = sensitivity_env["out_rain_base"].loc[1]
    two_day_grid = _make_two_day_grid_frame(day1_grid)
    expected_576 = two_day_grid.index
    assert len(expected_576) == 2 * TIMESTEPS_PER_DAY

    # Positive controls: intact 576-row output and valid large negative flux (-950.0) pass.
    assert_active_output_finiteness(
        two_day_grid, expected_timestamps=expected_576, tstep=300
    )
    neg_flux_ok = two_day_grid.copy()
    neg_flux_ok.iloc[12, neg_flux_ok.columns.get_loc(("SUEWS", "QH"))] = -950.0
    assert_active_output_finiteness(
        neg_flux_ok, expected_timestamps=expected_576, tstep=300
    )

    # Negative controls 1-3: truncated 12/576, missing day 1, and duplicate day fail.
    with pytest.raises(AssertionError, match="row count mismatch"):
        assert_active_output_finiteness(
            two_day_grid.iloc[:12], expected_timestamps=expected_576, tstep=300
        )
    with pytest.raises(AssertionError, match="row count mismatch"):
        assert_active_output_finiteness(
            two_day_grid.iloc[TIMESTEPS_PER_DAY:],
            expected_timestamps=expected_576,
            tstep=300,
        )
    dup_day = pd.concat([day1_grid, day1_grid])
    assert len(dup_day) == len(expected_576)
    with pytest.raises(AssertionError, match="duplicate timestamps"):
        assert_active_output_finiteness(
            dup_day, expected_timestamps=expected_576, tstep=300
        )

    # Negative controls 4-5: wrong grid ID and duplicate (grid, datetime) fail.
    multi_ok = pd.concat({1: two_day_grid}, names=["grid", "datetime"])
    assert_simulation_output_finiteness(
        multi_ok, expected_grids=(1,), expected_timestamps=expected_576, tstep=300
    )
    multi_wrong_grid = pd.concat({99: two_day_grid}, names=["grid", "datetime"])
    with pytest.raises(AssertionError, match="grid IDs mismatch"):
        assert_simulation_output_finiteness(
            multi_wrong_grid,
            expected_grids=(1,),
            expected_timestamps=expected_576,
            tstep=300,
        )
    multi_dup = pd.concat([multi_ok, multi_ok.iloc[:1]])
    with pytest.raises(AssertionError, match="duplicate"):
        assert_simulation_output_finiteness(
            multi_dup, expected_grids=(1,), expected_timestamps=expected_576, tstep=300
        )

    # Negative controls 6-7: missing required columns QH or QE/QN/Kup fail.
    with pytest.raises(AssertionError, match="QH"):
        assert_active_output_finiteness(
            two_day_grid.drop(columns=[("SUEWS", "QH")]),
            expected_timestamps=expected_576,
            tstep=300,
        )
    with pytest.raises(AssertionError, match="Kup"):
        assert_active_output_finiteness(
            two_day_grid.drop(
                columns=[("SUEWS", "QE"), ("SUEWS", "QN"), ("SUEWS", "Kup")]
            ),
            expected_timestamps=expected_576,
            tstep=300,
        )

    # Negative controls 8-12: exact -999 sentinel, active NaN, EOD NaN, non-EOD write, Fcld.
    bad_sentinel = two_day_grid.copy()
    bad_sentinel.iloc[12, bad_sentinel.columns.get_loc(("SUEWS", "QH"))] = (
        SUEWS_MISSING_SENTINEL
    )
    with pytest.raises(AssertionError, match="QH"):
        assert_active_output_finiteness(
            bad_sentinel, expected_timestamps=expected_576, tstep=300
        )

    bad_nan = two_day_grid.copy()
    bad_nan.iloc[12, bad_nan.columns.get_loc(("SUEWS", "QH"))] = np.nan
    with pytest.raises(AssertionError, match="QH"):
        assert_active_output_finiteness(
            bad_nan, expected_timestamps=expected_576, tstep=300
        )

    bad_ds_eod = two_day_grid.copy()
    eod_idx = int(np.flatnonzero(expected_dailystate_mask(expected_576, tstep=300))[0])
    bad_ds_eod.iloc[
        eod_idx, bad_ds_eod.columns.get_loc(("DailyState", "LAI_Grass"))
    ] = np.nan
    with pytest.raises(AssertionError, match="LAI_Grass"):
        assert_active_output_finiteness(
            bad_ds_eod, expected_timestamps=expected_576, tstep=300
        )

    bad_ds_mid = two_day_grid.copy()
    bad_ds_mid.iloc[0, bad_ds_mid.columns.get_loc(("DailyState", "LAI_Grass"))] = 1.0
    with pytest.raises(AssertionError, match="non-EOD"):
        assert_active_output_finiteness(
            bad_ds_mid, expected_timestamps=expected_576, tstep=300
        )

    bad_absent = two_day_grid.copy()
    bad_absent.iloc[0, bad_absent.columns.get_loc(("SUEWS", "Fcld"))] = 0.5
    with pytest.raises(AssertionError, match="Fcld"):
        assert_active_output_finiteness(
            bad_absent, expected_timestamps=expected_576, tstep=300
        )


@pytest.mark.core
def test_strict_reductions_and_water_balance_reject_sentinels_and_misalignment(
    sensitivity_env,
):
    """Strict reductions and named-surface water balance reject NaN/-999/misalignment."""
    # ARRANGE
    df_rain_out = sensitivity_env["out_rain_base"]
    df_rain_forcing = sensitivity_env["df_rain_day"]
    state_init = sensitivity_env["base_state_init"]
    qh_series = df_rain_out.loc[1, ("SUEWS", "QH")]

    # Positive controls: intact water balance closes and -950.0 valid negative flux passes.
    assert (
        water_balance_residual_mm(
            df_rain_out, state_init, grid=1, expected_timestamps=df_rain_forcing.index
        )
        < WATER_CLOSURE_TOLERANCE_MM
    )
    valid_neg = qh_series.copy()
    valid_neg.iloc[10] = -950.0
    assert np.isfinite(strict_mean(valid_neg, label="QH_neg950"))
    assert np.isfinite(strict_sum(valid_neg, label="QH_neg950"))
    assert strict_max_abs(valid_neg, label="QH_neg950") >= 950.0

    # Negative controls: NaN, +Inf, and exact -999.0 rejected by all strict reductions.
    for bad_val, pattern in (
        (np.nan, "non-finite"),
        (np.inf, "non-finite"),
        (SUEWS_MISSING_SENTINEL, "-999.0"),
    ):
        poisoned = qh_series.copy()
        poisoned.iloc[10] = bad_val
        with pytest.raises(AssertionError, match=pattern):
            strict_mean(poisoned, label="QH_poisoned")
        with pytest.raises(AssertionError, match=pattern):
            strict_sum(poisoned, label="QH_poisoned")
        with pytest.raises(AssertionError, match=pattern):
            strict_max_abs(poisoned, label="QH_poisoned")
        with pytest.raises(AssertionError, match=pattern):
            strict_groupby_reduce(
                poisoned,
                pd.Series("day0", index=poisoned.index),
                agg="mean",
                label="QH_poisoned",
            )

    # Negative controls for named-surface and time-aligned water_balance_residual_mm.
    with pytest.raises(AssertionError, match="ss_Grass_next"):
        water_balance_residual_mm(
            df_rain_out.drop(columns=[("debug", "ss_Grass_next")]),
            state_init,
            grid=1,
            expected_timestamps=df_rain_forcing.index,
        )
    shifted_ts = df_rain_forcing.index + pd.Timedelta(minutes=5)
    with pytest.raises(AssertionError, match="expected forcing timestamps"):
        water_balance_residual_mm(
            df_rain_out, state_init, grid=1, expected_timestamps=shifted_ts
        )
    bad_sfr_state = state_init.drop(columns=[("sfr_surf", "(4,)")])
    with pytest.raises(AssertionError, match="sfr_surf"):
        water_balance_residual_mm(
            df_rain_out,
            bad_sfr_state,
            grid=1,
            expected_timestamps=df_rain_forcing.index,
        )
    for bad_val, pattern in (
        (np.nan, "non-finite"),
        (SUEWS_MISSING_SENTINEL, "-999.0"),
    ):
        bad_wb = df_rain_out.copy()
        bad_wb.iloc[20, bad_wb.columns.get_loc(("SUEWS", "RO"))] = bad_val
        with pytest.raises(AssertionError, match=pattern):
            water_balance_residual_mm(
                bad_wb, state_init, grid=1, expected_timestamps=df_rain_forcing.index
            )
