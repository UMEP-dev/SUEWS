"""Regression tests for Fortran inactive-scheme parameter guards (#2363, #2348).

Verifies at both the direct Fortran-kernel level (with literal IEEE quiet NaN,
zero denominators, -999 sentinels, and strict IEEE exception-flag checks for
``ieee_invalid`` and ``ieee_divide_by_zero``) and end-to-end through
``SUEWSSimulation`` that inactive-scheme parameters are not read and cannot
corrupt active outputs or trigger errors:

- ``STEBBSmethod == 0`` (``STEBBS_NONE``): ``stebbsonlinecouple`` and
  ``gen_building`` ignore inactive STEBBS and ``building_archtype`` fields while
  preserving safe ``Textroof_C`` / ``Textwall_C`` allocation for downstream
  ``ASSOCIATE`` blocks; incompatible ``StorageHeatMethod == SHM_STEBBS`` with
  ``STEBBSmethod == 0`` is rejected cleanly before consuming ``QS_stebbs``.
- ``LAImethod == 0`` (observed LAI) and absent vegetation (``sfr == 0``):
  ``SUEWS_cal_DailyState`` and ``update_gddlai`` ignore inactive GDD/SDD
  phenology parameters and ``lai_power`` while preserving active observed-LAI
  vegetation updates (albedo, deciduous storage capacity, porosity) for present
  vegetation; absent vegetation surfaces (``sfr == 0``) are skipped in
  ``SUEWS_cal_DailyState``, ``update_gddlai``, ``update_Veg``, and
  ``LUMPS_cal_QHQE``.
- ``EmissionsMethod`` inactive sub-schemes: ``SUEWS_cal_DailyState`` and
  ``update_DailyState_Day`` ignore inactive heating/cooling base temperatures;
  ``SUEWS_cal_AnthropogenicEmission`` and ``AnthropogenicEmissions`` ignore
  inactive CO2, cooling, L11/J11, and detailed-traffic parameters; and
  ``SUEWS_cal_BiogenCO2`` does not read biogenic CO2 parameters when
  ``EmissionsMethod < 11``.
"""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path
import subprocess

from conftest import TIMESTEPS_PER_DAY, require_fortran_probe_toolchain
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.data_model.core.model import EmissionsMethod, LAIMethod, RCMethod

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-RESPONSE", "C-NATIVE-ERROR"),
]

# Two full days so DailyState midnight rollover, update_gddlai, and update_Veg execute.
RUN_STEPS = 2 * TIMESTEPS_PER_DAY
STEBBS_FIXTURE_CONFIG = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "data_test"
    / "stebbs_test"
    / "sample_config.yml"
)


def _run_short_sim(
    base_cfg,
    df_forcing: pd.DataFrame,
    *,
    mutate_config: Callable | None = None,
    mutate_forcing: Callable | None = None,
) -> pd.DataFrame:
    """Run a two-day SUEWSSimulation with optional config/forcing mutators."""
    cfg = base_cfg.model_copy(deep=True)
    if mutate_config is not None:
        mutate_config(cfg)
    forcing = df_forcing.copy()
    if mutate_forcing is not None:
        mutate_forcing(forcing)
    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(forcing)
    result = sim.run(
        start_date=forcing.index[0],
        end_date=forcing.index[-1],
        n_jobs=1,
    )
    return result.df


def _assert_frames_bit_identical(
    df_base: pd.DataFrame,
    df_mut: pd.DataFrame,
    *,
    groups: tuple[str, ...] = ("SUEWS", "DailyState"),
) -> None:
    """Assert two output DataFrames match bit-for-bit across selected groups."""
    for grp in groups:
        if grp not in df_base.columns.get_level_values("group"):
            continue
        a = df_base.xs(grp, level="group", axis=1).to_numpy(dtype=np.float64)
        b = df_mut.xs(grp, level="group", axis=1).to_numpy(dtype=np.float64)
        assert a.shape == b.shape, (
            f"Shape mismatch in group {grp}: {a.shape} vs {b.shape}"
        )
        nan_a = np.isnan(a)
        nan_b = np.isnan(b)
        assert np.array_equal(nan_a, nan_b), f"NaN mask differs in group {grp}"
        valid = ~nan_a
        assert np.array_equal(a[valid].view(np.uint64), b[valid].view(np.uint64)), (
            f"Bitwise output mismatch in group {grp}"
        )


def _set_val(container, attr: str, value) -> None:
    """Assign either a plain attribute or a RefValue's ``.value`` in place."""
    current = getattr(container, attr)
    if hasattr(current, "value"):
        current.value = value
    else:
        setattr(container, attr, value)


def _poison_stebbs_and_archetype(cfg) -> None:
    """Set extreme STEBBS and building_archetype values while STEBBS is disabled."""
    cfg.model.physics.stebbs.enabled = False
    _set_val(cfg.model.physics.stebbs, "capacitance", RCMethod.PARAMETERISE)
    props = cfg.sites[0].properties
    arch = props.building_archetype
    _set_val(arch, "area_footprint", 99999.0)
    _set_val(arch, "area_wall_external", 88888.0)
    _set_val(arch, "ratio_window_to_wall", 0.99)
    _set_val(arch, "thickness_wall", 5.0)
    _set_val(arch, "thickness_wall_outer", 4.5)
    _set_val(arch, "conductivity_wall", 99.0)
    _set_val(arch, "density_wall", 9999.0)
    _set_val(arch, "specific_heat_capacity_wall", 9999.0)
    stebbs = props.stebbs
    _set_val(stebbs, "thermal_conductivity_ground", 50.0)
    _set_val(stebbs, "depth_ground", 25.0)
    _set_val(stebbs, "convection_coefficient_wall_external", 99.0)
    _set_val(stebbs, "convection_coefficient_roof_external", 99.0)


@pytest.fixture(scope="module")
def _two_day_sample(sample_config_loaded, sample_forcing_parsed):
    """Cached base config and two-day forcing slice."""
    _, df_forcing = sample_forcing_parsed(RUN_STEPS)
    return sample_config_loaded, df_forcing


def test_stebbs_disabled_ignores_inactive_stebbs_and_archetype_parameters(
    _two_day_sample,
):
    """STEBBSmethod=0 outputs are bit-identical under extreme STEBBS/archetype values."""
    base_cfg, df_forcing = _two_day_sample

    def configure_disabled_baseline(cfg) -> None:
        cfg.model.physics.stebbs.enabled = False

    df_base = _run_short_sim(
        base_cfg, df_forcing, mutate_config=configure_disabled_baseline
    )
    df_mut = _run_short_sim(
        base_cfg, df_forcing, mutate_config=_poison_stebbs_and_archetype
    )

    _assert_frames_bit_identical(
        df_base, df_mut, groups=("SUEWS", "DailyState", "RSL", "BEERS")
    )
    assert np.isfinite(df_mut.xs("SUEWS", level="group", axis=1)["QN"]).all()
    assert np.isfinite(df_mut.xs("SUEWS", level="group", axis=1)["QS"]).all()


def test_ehc_with_stebbs_disabled_ignores_inactive_stebbs_archetype_parameters(
    _two_day_sample,
):
    """Active EHC (storage_heat=5) stays unaffected by extreme inactive STEBBS fields."""
    base_cfg, df_forcing = _two_day_sample

    def configure_ehc_base(cfg) -> None:
        cfg.model.physics.net_radiation = 3
        cfg.model.physics.storage_heat = 5
        cfg.model.physics.ehc_surface_coupling = "implicit"
        cfg.model.physics.stebbs.enabled = False

    def configure_ehc_extreme(cfg) -> None:
        configure_ehc_base(cfg)
        _poison_stebbs_and_archetype(cfg)

    df_base = _run_short_sim(base_cfg, df_forcing, mutate_config=configure_ehc_base)
    df_mut = _run_short_sim(base_cfg, df_forcing, mutate_config=configure_ehc_extreme)

    _assert_frames_bit_identical(df_base, df_mut, groups=("SUEWS", "DailyState", "EHC"))
    assert np.isfinite(df_mut.xs("SUEWS", level="group", axis=1)["QS"]).all()


def test_active_stebbs_matches_reference_building_outputs():
    """Active STEBBS (storage_heat=7, stebbs enabled) matches shipped reference outputs."""
    sim = SUEWSSimulation(STEBBS_FIXTURE_CONFIG)
    df_window = sim.forcing.df.loc["2017-08-26":"2017-08-27"]
    # Run day-1 spin-up + 12 validation steps on day 2 so outputs match sample_output_stebbs.csv.
    val_steps = 12
    df_forcing = df_window.iloc[: TIMESTEPS_PER_DAY + val_steps].copy()
    df_forcing["rain"] = df_forcing["rain"].clip(lower=0)
    sim.update_forcing(df_forcing)
    result = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    )
    stebbs = result.df.xs("STEBBS", level="group", axis=1).iloc[TIMESTEPS_PER_DAY:]
    ref_path = STEBBS_FIXTURE_CONFIG.parent / "sample_output_stebbs.csv"
    ref = pd.read_csv(ref_path).iloc[:val_steps]
    for col, atol in (
        ("Tair_ind", 0.5),
        ("QN_bldg_FA", 1.0),
        ("QS_total_FA", 1.0),
        ("QH_bldg_FA", 1.0),
    ):
        actual = stebbs[col].to_numpy(dtype=np.float64)
        expected = ref[col].to_numpy(dtype=np.float64)
        assert np.isfinite(actual).all(), (
            f"Active STEBBS column {col} became non-finite"
        )
        np.testing.assert_allclose(actual, expected, rtol=0.02, atol=atol)


def test_observed_lai_ignores_inactive_phenology_parameters(_two_day_sample):
    """laimethod=0 ignores GDD/SDD/lai_power while preserving active vegetation updates."""
    base_cfg, df_forcing = _two_day_sample

    def add_varying_observed_lai(df: pd.DataFrame) -> None:
        # Vary observed LAI across the two days so LAI_id /= LAI_id_prev in
        # update_Veg, proving active observed-LAI vegetation updates still run.
        n_half = TIMESTEPS_PER_DAY
        lai_vals = np.concatenate([
            np.full(n_half, 1.5),
            np.full(len(df) - n_half, 3.5),
        ])
        df["lai"] = lai_vals

    def configure_obs_base(cfg) -> None:
        cfg.model.physics.laimethod = LAIMethod.OBSERVED

    def configure_obs_perturbed(cfg) -> None:
        cfg.model.physics.laimethod = LAIMethod.OBSERVED
        lc = cfg.sites[0].properties.land_cover
        for veg_name in ("evetr", "dectr", "grass"):
            lai = getattr(lc, veg_name).lai
            _set_val(lai, "base_temperature", 42.0)
            _set_val(lai, "base_temperature_senescence", -25.0)
            _set_val(lai, "gdd_base", 9999.0)
            _set_val(lai, "sdd_base", -9999.0)
            _set_val(lai.lai_power, "growth_lai", 9.0)
            _set_val(lai.lai_power, "growth_gdd", 9.0)
            _set_val(lai.lai_power, "senescence_lai", 9.0)
            _set_val(lai.lai_power, "senescence_sdd", 9.0)

    df_base = _run_short_sim(
        base_cfg,
        df_forcing,
        mutate_config=configure_obs_base,
        mutate_forcing=add_varying_observed_lai,
    )
    df_mut = _run_short_sim(
        base_cfg,
        df_forcing,
        mutate_config=configure_obs_perturbed,
        mutate_forcing=add_varying_observed_lai,
    )

    _assert_frames_bit_identical(df_base, df_mut, groups=("SUEWS", "DailyState"))
    ds = df_mut.xs("DailyState", level="group", axis=1).dropna(how="all")
    for col in ("LAI_EveTr", "LAI_DecTr", "LAI_Grass", "AlbDecTr", "Porosity"):
        assert np.isfinite(ds[col]).all(), f"{col} became non-finite under laimethod=0"
    # Active observed-LAI response must still update vegetation properties across days.
    assert ds["LAI_DecTr"].iloc[-1] != ds["LAI_DecTr"].iloc[0]
    assert ds["AlbDecTr"].iloc[-1] != ds["AlbDecTr"].iloc[0]
    assert ds["Porosity"].iloc[-1] != ds["Porosity"].iloc[0]


def test_observed_emissions_ignores_inactive_base_temps_and_co2_parameters(
    _two_day_sample,
):
    """emissions=0 ignores heating/cooling base temps and all modelled heat/CO2 params."""
    base_cfg, df_forcing = _two_day_sample

    def add_observed_qf(df: pd.DataFrame) -> None:
        df["qf"] = 18.5

    def configure_obs_qf_base(cfg) -> None:
        cfg.model.physics.emissions = EmissionsMethod.OBSERVED

    def configure_obs_qf_perturbed(cfg) -> None:
        cfg.model.physics.emissions = EmissionsMethod.OBSERVED
        ae = cfg.sites[0].properties.anthropogenic_emissions
        heat = ae.heat
        co2 = ae.co2
        _set_val(heat.baset_heating, "working_day", 50.0)
        _set_val(heat.baset_heating, "holiday", 50.0)
        _set_val(heat.baset_cooling, "working_day", -30.0)
        _set_val(heat.baset_cooling, "holiday", -30.0)
        _set_val(heat.qf_a, "working_day", 99.0)
        _set_val(heat.qf_b, "working_day", 99.0)
        _set_val(heat.qf_c, "working_day", 99.0)
        _set_val(heat.ah_min, "working_day", 99.0)
        _set_val(heat.ah_slope_heating, "working_day", 99.0)
        _set_val(heat.ah_slope_cooling, "working_day", 99.0)
        _set_val(heat.qf0_beu, "working_day", 0.99)
        _set_val(heat.qf0_beu, "holiday", 0.99)
        _set_val(co2, "emission_factor_co2_fuel", 99.0)
        _set_val(co2, "emission_factor_energy_vehicle", 9.9e7)
        _set_val(co2, "emission_co2_metabolism_max", 999.0)
        _set_val(co2, "emission_co2_metabolism_min", 999.0)
        _set_val(co2, "fraction_fossil_fuel_heating", 0.99)
        _set_val(co2, "fraction_fossil_fuel_non_heating", 0.99)

    df_base = _run_short_sim(
        base_cfg,
        df_forcing,
        mutate_config=configure_obs_qf_base,
        mutate_forcing=add_observed_qf,
    )
    df_mut = _run_short_sim(
        base_cfg,
        df_forcing,
        mutate_config=configure_obs_qf_perturbed,
        mutate_forcing=add_observed_qf,
    )

    _assert_frames_bit_identical(df_base, df_mut, groups=("SUEWS", "DailyState"))
    ds = df_mut.xs("DailyState", level="group", axis=1).dropna(how="all")
    np.testing.assert_array_equal(ds["HDD1_h"].to_numpy(), 0.0)
    np.testing.assert_array_equal(ds["HDD2_c"].to_numpy(), 0.0)
    suews = df_mut.xs("SUEWS", level="group", axis=1)
    np.testing.assert_allclose(suews["QF"].to_numpy(), 18.5, rtol=0.0, atol=1e-12)


@pytest.mark.parametrize(
    "emissions_code",
    [1, 2, 3, 5],
    ids=["L11-inst-no-co2", "J11-no-co2", "L11-daily-no-co2", "J11-detailed-no-co2"],
)
def test_heat_only_emissions_ignores_inactive_co2_and_submethod_parameters(
    _two_day_sample,
    emissions_code: int,
):
    """emissions in 1..6 ignores CO2-only params (and cooling base temps for 1/4)."""
    base_cfg, df_forcing = _two_day_sample

    def configure_base(cfg) -> None:
        cfg.model.physics.emissions = EmissionsMethod(emissions_code)

    def configure_perturbed(cfg) -> None:
        cfg.model.physics.emissions = EmissionsMethod(emissions_code)
        ae = cfg.sites[0].properties.anthropogenic_emissions
        heat = ae.heat
        co2 = ae.co2
        _set_val(co2, "emission_factor_co2_fuel", 99.0)
        _set_val(co2, "emission_co2_metabolism_max", 999.0)
        _set_val(co2, "emission_co2_metabolism_min", 999.0)
        _set_val(co2, "fraction_fossil_fuel_heating", 0.99)
        _set_val(co2, "fraction_fossil_fuel_non_heating", 0.99)
        _set_val(co2.emission_factor_co2_vehicle, "working_day", 9.9)
        _set_val(co2.emission_factor_co2_vehicle, "holiday", 9.9)
        _set_val(co2, "emission_co2_point_source", 1.0e6)
        if emissions_code in (1, 2, 3):
            # Simple SAHP (1..3) does not use traffic energy factor or traffic rate
            _set_val(co2, "emission_factor_energy_vehicle", 9.9e7)
            _set_val(co2.traffic_rate, "working_day", 99.0)
            _set_val(co2.traffic_rate, "holiday", 99.0)
        if emissions_code == 1:
            # L11 instantaneous has no cooling contribution
            _set_val(heat.baset_cooling, "working_day", -30.0)
            _set_val(heat.baset_cooling, "holiday", -30.0)
            _set_val(heat.ah_slope_cooling, "working_day", 99.0)
            _set_val(heat.ah_slope_cooling, "holiday", 99.0)

    df_base = _run_short_sim(base_cfg, df_forcing, mutate_config=configure_base)
    df_mut = _run_short_sim(base_cfg, df_forcing, mutate_config=configure_perturbed)

    _assert_frames_bit_identical(df_base, df_mut, groups=("SUEWS", "DailyState"))
    suews = df_mut.xs("SUEWS", level="group", axis=1)
    for col in ("Fc", "FcMetab", "FcTraff", "FcBuild", "FcPoint"):
        np.testing.assert_array_equal(suews[col].to_numpy(), 0.0)
    if emissions_code == 1:
        ds = df_mut.xs("DailyState", level="group", axis=1).dropna(how="all")
        np.testing.assert_array_equal(ds["HDD2_c"].to_numpy(), 0.0)


@pytest.mark.parametrize(
    "emissions_code",
    [11, 12],
    ids=["L11-inst-with-co2", "J11-with-co2"],
)
def test_co2_enabled_emissions_ignores_inactive_submethod_parameters(
    _two_day_sample,
    emissions_code: int,
):
    """CO2-enabled emissions (11, 12) remain active while ignoring inactive fields."""
    base_cfg, df_forcing = _two_day_sample

    def configure_base(cfg) -> None:
        cfg.model.physics.emissions = EmissionsMethod(emissions_code)

    def configure_perturbed(cfg) -> None:
        cfg.model.physics.emissions = EmissionsMethod(emissions_code)
        ae = cfg.sites[0].properties.anthropogenic_emissions
        heat = ae.heat
        co2 = ae.co2
        # Simple CO2 sub-methods (11..13) do not consume traffic_rate.
        _set_val(co2.traffic_rate, "working_day", 99.0)
        _set_val(co2.traffic_rate, "holiday", 99.0)
        if emissions_code == 11:
            # L11 instantaneous does not consume cooling base temp or J11 Qf_A/B/C.
            _set_val(heat.baset_cooling, "working_day", -30.0)
            _set_val(heat.baset_cooling, "holiday", -30.0)
            _set_val(heat.ah_slope_cooling, "working_day", 99.0)
            _set_val(heat.ah_slope_cooling, "holiday", 99.0)
            _set_val(heat.qf_a, "working_day", 99.0)
            _set_val(heat.qf_b, "working_day", 99.0)
            _set_val(heat.qf_c, "working_day", 99.0)
        elif emissions_code == 12:
            # J11 does not consume L11 ah_min or L11 heating/cooling slopes.
            _set_val(heat.ah_min, "working_day", 99.0)
            _set_val(heat.ah_slope_heating, "working_day", 99.0)
            _set_val(heat.ah_slope_cooling, "working_day", 99.0)

    df_base = _run_short_sim(base_cfg, df_forcing, mutate_config=configure_base)
    df_mut = _run_short_sim(base_cfg, df_forcing, mutate_config=configure_perturbed)

    _assert_frames_bit_identical(df_base, df_mut, groups=("SUEWS", "DailyState"))
    suews = df_mut.xs("SUEWS", level="group", axis=1)
    assert np.isfinite(suews["Fc"]).all()
    assert np.any(suews["Fc"].to_numpy() != 0.0)


@pytest.mark.e2e
def test_native_fortran_guards_survive_nan_and_zero_inactive_parameters(tmp_path):
    """Direct Fortran probe verifies NaN / -999 / zero denominators stay inert.

    Checks IEEE exception flags (``ieee_invalid`` and ``ieee_divide_by_zero``)
    around every guarded call so silent NaN comparisons or divisions cannot pass.
    """
    compiler, suews = require_fortran_probe_toolchain(
        "Native inactive-scheme guard probe",
        libraries=(
            "libsuewsdriver.a",
            "libsuewsphys.a",
            "libsuewsutil.a",
            "libspartacus.a",
        ),
        modules=(
            "module_ctrl_const_options.mod",
            "module_ctrl_driver.mod",
            "module_ctrl_error_state.mod",
            "module_ctrl_type.mod",
            "module_phys_anthro.mod",
            "module_phys_biogenco2.mod",
            "module_phys_dailystate.mod",
            "module_phys_lai.mod",
            "module_phys_lumps.mod",
            "module_phys_stebbs.mod",
        ),
    )
    source = tmp_path / "inactive_guards_probe.f90"
    source.write_text(
        """program inactive_guards_probe
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan
use, intrinsic :: ieee_exceptions, only: ieee_invalid, ieee_divide_by_zero, &
   ieee_set_flag, ieee_get_flag
use module_ctrl_const_allocate, only: &
   ncolumnsDataOutSTEBBS, ncolumnsDataOutSPARTACUS, nsurf, nvegsurf, &
   PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf, &
   ivConif, ivDecid, ivGrass
use module_ctrl_const_options, only: &
   STEBBS_NONE, RC_PARAMETERISE, SHM_STEBBS
use module_ctrl_driver, only: SUEWS_cal_Main, SUEWS_cal_Qn, SUEWS_cal_BiogenCO2
use module_ctrl_error_state, only: reset_supy_error, supy_error_flag, get_supy_error
use module_ctrl_type, only: &
   SUEWS_CLOCK, SUEWS_TIMER, SUEWS_CONFIG, SUEWS_FORCING, SUEWS_SITE, SUEWS_STATE, &
   STEBBS_STATE, STEBBS_PRM, BUILDING_ARCHETYPE_PRM, STEBBS_BLDG, error_state, &
   output_line
use module_phys_stebbs, only: stebbsonlinecouple
use module_phys_lai, only: update_gddlai
use module_phys_dailystate, only: SUEWS_cal_DailyState, update_DailyState_Day, update_Veg
use module_phys_anthro, only: AnthropogenicEmissions
use module_phys_biogenco2, only: CO2_biogen
use module_phys_lumps, only: LUMPS_cal_QHQE
implicit none

real(kind(1d0)) :: qnan
integer :: err_code
character(len=256) :: err_msg
real(kind(1d0)) :: fc_bio_leaf, fc_photo_leaf, fc_respi_leaf, fc_photo_ref
type(SUEWS_CLOCK) :: clock
type(SUEWS_TIMER) :: timer
type(SUEWS_CONFIG) :: config
type(SUEWS_FORCING) :: forcing
type(SUEWS_SITE) :: siteInfo
type(SUEWS_STATE) :: modState
type(STEBBS_STATE) :: stebbsState
type(STEBBS_PRM) :: stebbsPrm
type(BUILDING_ARCHETYPE_PRM) :: arch
type(STEBBS_BLDG) :: bldg
type(error_state) :: errState
type(output_line) :: outLine
real(kind(1d0)) :: dtLine(5), outSTEBBS(ncolumnsDataOutSTEBBS - 5)
real(kind(1d0)) :: outSPARTACUS(ncolumnsDataOutSPARTACUS - 5)

! DailyState / LAI / Veg locals
real(kind(1d0)) :: lai_obs(nvegsurf), lai_prev(nvegsurf), lai_next(nvegsurf)
real(kind(1d0)) :: gdd(nvegsurf), sdd(nvegsurf), lai_min(nvegsurf), lai_max(nvegsurf)
real(kind(1d0)) :: base_t_gdd(nvegsurf), base_t_sdd(nvegsurf)
real(kind(1d0)) :: gdd_base(nvegsurf), sdd_base(nvegsurf), lai_power(4, nvegsurf)
real(kind(1d0)) :: store_drain(6, nsurf), sfr_zero(nvegsurf)
real(kind(1d0)) :: decid_cap, alb_dec, alb_eve, alb_gra, poros
real(kind(1d0)) :: tmin_id, tmax_id, lenday_id, hdd_id(12)

! Anthropogenic locals
real(kind(1d0)) :: ah_prof(0:23, 2), hum_prof(0:23, 2), traff_prof(0:23, 2), pop_prof(0:23, 2)
real(kind(1d0)) :: qf_sahp, fc_anthro, fc_metab, fc_traff, fc_build, fc_point, qf_build_out
integer :: em

qnan = ieee_value(0.0d0, ieee_quiet_nan)

! -------------------------------------------------------------------------
! 1. STEBBSmethod == STEBBS_NONE: gen_building and stebbsonlinecouple must
!    not read or validate NaN/zero/sentinel archetype or STEBBS parameters,
!    and gen_building must still leave Textroof_C / Textwall_C allocated.
! -------------------------------------------------------------------------
call reset_supy_error()
config%stebbsmethod = STEBBS_NONE
config%rcmethod = RC_PARAMETERISE
arch%footprint_area = 0.0d0
arch%wall_thickness = 0.0d0
arch%wall_density = -999.0d0
arch%wall_specific_heat_capacity = qnan
arch%roof_thickness = 0.0d0
arch%roof_density = 0.0d0
arch%roof_specific_heat_capacity = qnan
stebbsPrm%ground_depth = qnan
stebbsPrm%external_ground_conductivity = qnan
stebbsState%wall_outdoor_surface_temperature = qnan
stebbsState%roof_outdoor_surface_temperature = qnan

call clear_fpe()
call gen_building(stebbsState, stebbsPrm, arch, config, bldg, 3, errState)
call assert_no_fpe(101)
if (supy_error_flag()) stop 10
if (.not. allocated(bldg%Textroof_C) .or. size(bldg%Textroof_C) /= 3) stop 11
if (.not. allocated(bldg%Textwall_C) .or. size(bldg%Textwall_C) /= 3) stop 12
if (.not. all(ieee_is_finite(bldg%Textroof_C))) stop 13
if (.not. all(ieee_is_finite(bldg%Textwall_C))) stop 14

siteInfo%building_archtype = arch
siteInfo%stebbs = stebbsPrm
dtLine = 0.0d0
outSTEBBS = 0.0d0
call clear_fpe()
call stebbsonlinecouple(clock, config, forcing, siteInfo, modState, dtLine, 3, outSTEBBS)
call assert_no_fpe(102)
if (supy_error_flag()) stop 15
if (allocated(modState%stebbsState%buildings)) stop 16
if (any(outSTEBBS /= -999.0d0)) stop 17

! -------------------------------------------------------------------------
! 2. StorageHeatMethod == SHM_STEBBS with STEBBSmethod == STEBBS_NONE:
!    SUEWS_cal_Qn and SUEWS_cal_Main must reject the incompatible pair
!    before consuming poisoned QS_stebbs or uninitialised STEBBS state.
! -------------------------------------------------------------------------
call reset_supy_error()
config%StorageHeatMethod = SHM_STEBBS
config%stebbsmethod = STEBBS_NONE
modState%stebbsState%QS_stebbs = qnan
modState%heatState%qs = 0.0d0
outSPARTACUS = 0.0d0
call clear_fpe()
call SUEWS_cal_Qn(clock, config, forcing, siteInfo, modState, outSPARTACUS)
call assert_no_fpe(103)
if (.not. supy_error_flag()) stop 18
call get_supy_error(err_code, err_msg)
if (err_code /= 106 .or. index(err_msg, 'SHM_STEBBS') == 0) stop 18
if (any(outSPARTACUS /= -999.0d0)) stop 19

call reset_supy_error()
timer%iy = 2012
timer%id = 120
timer%it = 12
timer%imin = 0
timer%isec = 0
timer%tstep = 300
timer%tstep_prev = 300
timer%dt_since_start = 300
modState%stebbsState%QS_stebbs = qnan
modState%heatState%qs = 0.0d0
call clear_fpe()
call SUEWS_cal_Main(timer, forcing, config, siteInfo, modState, outLine)
call assert_no_fpe(104)
if (.not. supy_error_flag()) stop 20
call get_supy_error(err_code, err_msg)
if (err_code /= 106 .or. index(err_msg, 'SHM_STEBBS') == 0) stop 20
if (.not. ieee_is_finite(modState%heatState%qs)) stop 21
config%StorageHeatMethod = 1

! -------------------------------------------------------------------------
! 3. Full SUEWS_cal_DailyState closing-step path:
!    (a) Modelled LAI (LAImethod=1) with absent DecidSurf (sfr=0) poisoned
!        with NaN GDD/SDD/LAI/albedo/capacity/porosity -> must not trigger
!        error 117 or IEEE_INVALID, while active EveTr/Grass update cleanly.
!    (b) Observed LAI (LAImethod=0) with NaN GDD/SDD/lai_power and varying
!        observed LAI -> must update active albedo/capacity/porosity without
!        raising errors or IEEE_INVALID.
! -------------------------------------------------------------------------
call reset_supy_error()
clock%timer%tstep = 300
clock%timer%tstep_prev = 300
clock%timer%dt_since_start = 86400
clock%nsh_real = 12.0d0
clock%id_day = 150
clock%dayofWeek_day = [2, 5, 2]
clock%elapsed_sec_of_day = 86400
clock%first_step_of_run = .false.
clock%first_step_of_day = .false.
clock%last_step_of_day = .true.
clock%prev_step_closed_day = .false.
clock%first_step_of_deliv_day_st = .false.

config%LAImethod = 1
config%EmissionsMethod = 0
config%RSLLevel = 0
config%WaterUseMethod = 0

siteInfo%lat = 51.5d0
siteInfo%sfr_surf = 0.0d0
siteInfo%sfr_surf(PavSurf) = 0.5d0
siteInfo%sfr_surf(ConifSurf) = 0.25d0
siteInfo%sfr_surf(DecidSurf) = 0.0d0
siteInfo%sfr_surf(GrassSurf) = 0.25d0

! Active EveTr and Grass phenology/albedo bounds
siteInfo%lc_evetr%lai%base_temperature = 5.0d0
siteInfo%lc_evetr%lai%base_temperature_senescence = 10.0d0
siteInfo%lc_evetr%lai%gdd_base = 300.0d0
siteInfo%lc_evetr%lai%sdd_base = -450.0d0
siteInfo%lc_evetr%lai%lai_min = 4.0d0
siteInfo%lc_evetr%lai%lai_max = 5.1d0
siteInfo%lc_evetr%lai%lai_power = [0.03d0, 0.0005d0, 0.03d0, 0.0005d0]
siteInfo%lc_evetr%Alb_Min = 0.10d0
siteInfo%lc_evetr%Alb_Max = 0.14d0

siteInfo%lc_grass%lai%base_temperature = 5.0d0
siteInfo%lc_grass%lai%base_temperature_senescence = 10.0d0
siteInfo%lc_grass%lai%gdd_base = 300.0d0
siteInfo%lc_grass%lai%sdd_base = -450.0d0
siteInfo%lc_grass%lai%lai_min = 1.6d0
siteInfo%lc_grass%lai%lai_max = 5.9d0
siteInfo%lc_grass%lai%lai_power = [0.03d0, 0.0005d0, 0.03d0, 0.0005d0]
siteInfo%lc_grass%Alb_Min = 0.18d0
siteInfo%lc_grass%Alb_Max = 0.22d0

! Poison ALL parameters of absent DecidSurf with literal NaN
siteInfo%lc_dectr%lai%base_temperature = qnan
siteInfo%lc_dectr%lai%base_temperature_senescence = qnan
siteInfo%lc_dectr%lai%gdd_base = qnan
siteInfo%lc_dectr%lai%sdd_base = qnan
siteInfo%lc_dectr%lai%lai_min = qnan
siteInfo%lc_dectr%lai%lai_max = qnan
siteInfo%lc_dectr%lai%lai_power = qnan
siteInfo%lc_dectr%Alb_Min = qnan
siteInfo%lc_dectr%Alb_Max = qnan
siteInfo%lc_dectr%capacity_min_deciduous = qnan
siteInfo%lc_dectr%capacity_max_deciduous = qnan
siteInfo%lc_dectr%porosity_min_deciduous = qnan
siteInfo%lc_dectr%porosity_max_deciduous = qnan

! Poison inactive heating/cooling base temps for EmissionsMethod=0
siteInfo%anthroEmis%anthro_heat%BaseT_Heating_working = qnan
siteInfo%anthroEmis%anthro_heat%BaseT_Heating_holiday = qnan
siteInfo%anthroEmis%anthro_heat%BaseT_Cooling_working = qnan
siteInfo%anthroEmis%anthro_heat%BaseT_Cooling_holiday = qnan

forcing%kdown = 250.0d0
forcing%Temp_C = 18.0d0
forcing%rain = 0.0d0
forcing%LAI_evetr = 4.2d0
forcing%LAI_dectr = 2.0d0
forcing%LAI_grass = 2.5d0

modState%flagState%i_iter = 1
modState%phenState%Tmin_id = 12.0d0
modState%phenState%Tmax_id = 22.0d0
modState%phenState%lenDay_id = 12.0d0
modState%phenState%LAI_id = [4.2d0, 1.0d0, 2.5d0]
modState%phenState%GDD_id = [100.0d0, 0.0d0, 100.0d0]
modState%phenState%SDD_id = [0.0d0, 0.0d0, 0.0d0]
modState%phenState%albEveTr_id = 0.11d0
modState%phenState%albDecTr_id = 0.14d0
modState%phenState%albGrass_id = 0.19d0
modState%phenState%DecidCap_id = 0.5d0
modState%phenState%porosity_id = 0.45d0
modState%phenState%storage_drain_params = 0.0d0
modState%anthroEmisState%HDD_id = 0.0d0

call clear_fpe()
call SUEWS_cal_DailyState(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(105)
if (supy_error_flag()) stop 22
if (.not. ieee_is_finite(modState%phenState%LAI_id(ivConif))) stop 23
if (.not. ieee_is_finite(modState%phenState%LAI_id(ivGrass))) stop 24
if (abs(modState%phenState%LAI_id(ivDecid) - 1.0d0) > 1.0d-12) stop 25
if (abs(modState%phenState%albDecTr_id - 0.14d0) > 1.0d-12) stop 26

! Now test LAImethod=0 with present DecidSurf, varying observed LAI, and
! NaN GDD/SDD/lai_power: active vegetation updates must run cleanly.
call reset_supy_error()
config%LAImethod = 0
siteInfo%sfr_surf(DecidSurf) = 0.25d0
siteInfo%sfr_surf(PavSurf) = 0.25d0
siteInfo%lc_dectr%lai%lai_min = 1.0d0
siteInfo%lc_dectr%lai%lai_max = 3.0d0
siteInfo%lc_dectr%Alb_Min = 0.12d0
siteInfo%lc_dectr%Alb_Max = 0.18d0
siteInfo%lc_dectr%capacity_min_deciduous = 0.3d0
siteInfo%lc_dectr%capacity_max_deciduous = 0.8d0
siteInfo%lc_dectr%porosity_min_deciduous = 0.2d0
siteInfo%lc_dectr%porosity_max_deciduous = 0.6d0
siteInfo%lc_evetr%lai%base_temperature = qnan
siteInfo%lc_evetr%lai%base_temperature_senescence = qnan
siteInfo%lc_evetr%lai%gdd_base = qnan
siteInfo%lc_evetr%lai%sdd_base = qnan
siteInfo%lc_evetr%lai%lai_power = qnan
siteInfo%lc_grass%lai%base_temperature = qnan
siteInfo%lc_grass%lai%base_temperature_senescence = qnan
siteInfo%lc_grass%lai%gdd_base = qnan
siteInfo%lc_grass%lai%sdd_base = qnan
siteInfo%lc_grass%lai%lai_power = qnan

modState%phenState%LAI_id = [4.2d0, 1.0d0, 2.5d0]
modState%phenState%albDecTr_id = 0.14d0
modState%phenState%DecidCap_id = 0.5d0
modState%phenState%porosity_id = 0.45d0
forcing%LAI_evetr = 4.2d0
forcing%LAI_dectr = 2.0d0
forcing%LAI_grass = 2.5d0

call clear_fpe()
call SUEWS_cal_DailyState(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(106)
if (supy_error_flag()) stop 27
! deltaLAI = (2 - 1)/(3 - 1) = 0.5 -> albDecTr: 0.14 + (0.18 - 0.12)*0.5 = 0.17
if (abs(modState%phenState%albDecTr_id - 0.17d0) > 1.0d-12) stop 28
! porChange = (0.2 - 0.6)*0.5 = -0.2 -> porosity: 0.45 - 0.20 = 0.25
if (abs(modState%phenState%porosity_id - 0.25d0) > 1.0d-12) stop 29
! CapChange = (0.3 - 0.8)*0.5 = -0.25 -> DecidCap: 0.5 - (-0.25) = 0.75
if (abs(modState%phenState%DecidCap_id - 0.75d0) > 1.0d-12) stop 30

! Direct update_Veg probe with all surfaces absent and NaN bounds
decid_cap = 0.5d0
alb_dec = 0.14d0
alb_eve = 0.10d0
alb_gra = 0.20d0
poros = 0.45d0
store_drain = 0.0d0
sfr_zero = 0.0d0
lai_obs = [1.2d0, 2.4d0, 3.6d0]
lai_prev = [1.0d0, 1.0d0, 1.0d0]
call clear_fpe()
call update_Veg( &
   LAImax=[qnan, qnan, qnan], LAIMin=[qnan, qnan, qnan], &
   AlbMax_DecTr=qnan, AlbMax_EveTr=qnan, AlbMax_Grass=qnan, &
   AlbMin_DecTr=qnan, AlbMin_EveTr=qnan, AlbMin_Grass=qnan, &
   CapMax_dec=qnan, CapMin_dec=qnan, &
   PorMax_dec=qnan, PorMin_dec=qnan, &
   LAI_id=lai_obs, LAI_id_prev=lai_prev, &
   DecidCap_id=decid_cap, albDecTr_id=alb_dec, albEveTr_id=alb_eve, &
   albGrass_id=alb_gra, porosity_id=poros, StoreDrainPrm=store_drain, &
   sfr_veg=sfr_zero)
call assert_no_fpe(107)
if (.not. (ieee_is_finite(decid_cap) .and. ieee_is_finite(alb_dec) .and. &
           ieee_is_finite(alb_eve) .and. ieee_is_finite(alb_gra) .and. &
           ieee_is_finite(poros))) stop 31

! 3c. Absent vegetation (sfr == 0) with finite -999 sentinel in lai_power
!     and reached growth threshold must NOT overflow 0.1**(-999) or raise
!     error 117.
call reset_supy_error()
config%LAImethod = 1
siteInfo%sfr_surf = 0.0d0
siteInfo%sfr_surf(PavSurf) = 0.5d0
siteInfo%sfr_surf(ConifSurf) = 0.25d0
siteInfo%sfr_surf(DecidSurf) = 0.0d0
siteInfo%sfr_surf(GrassSurf) = 0.25d0
! Restore active EveTr and Grass phenology after the LAImethod=0 NaN test above
siteInfo%lc_evetr%lai%base_temperature = 5.0d0
siteInfo%lc_evetr%lai%base_temperature_senescence = 10.0d0
siteInfo%lc_evetr%lai%gdd_base = 300.0d0
siteInfo%lc_evetr%lai%sdd_base = -450.0d0
siteInfo%lc_evetr%lai%lai_power = [0.03d0, 0.0005d0, 0.03d0, 0.0005d0]
siteInfo%lc_grass%lai%base_temperature = 5.0d0
siteInfo%lc_grass%lai%base_temperature_senescence = 10.0d0
siteInfo%lc_grass%lai%gdd_base = 300.0d0
siteInfo%lc_grass%lai%sdd_base = -450.0d0
siteInfo%lc_grass%lai%lai_power = [0.03d0, 0.0005d0, 0.03d0, 0.0005d0]
siteInfo%lc_dectr%lai%base_temperature = 5.0d0
siteInfo%lc_dectr%lai%base_temperature_senescence = 10.0d0
siteInfo%lc_dectr%lai%gdd_base = 100.0d0
siteInfo%lc_dectr%lai%sdd_base = -200.0d0
siteInfo%lc_dectr%lai%lai_min = 0.1d0
siteInfo%lc_dectr%lai%lai_max = 3.0d0
siteInfo%lc_dectr%lai%lai_power = [-999.0d0, -999.0d0, -999.0d0, -999.0d0]
modState%phenState%LAI_id = [4.2d0, 0.1d0, 2.5d0]
modState%phenState%GDD_id = [200.0d0, 200.0d0, 200.0d0]
modState%phenState%SDD_id = [0.0d0, 0.0d0, 0.0d0]
call clear_fpe()
call SUEWS_cal_DailyState(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(122)
if (supy_error_flag()) stop 32
if (abs(modState%phenState%LAI_id(ivDecid) - 0.1d0) > 1.0d-12) stop 33

! 3d. Absent vegetation (sfr == 0) with valid parameters including legitimate
!     negative sdd_base = -1000.0 preserves exact diagnostic GDD/LAI and
!     update_Veg albedo evolution (0.12 -> 0.17 for LAI 1 -> 2 on [1, 3]).
call reset_supy_error()
config%LAImethod = 2
siteInfo%sfr_surf(ConifSurf) = 0.0d0
siteInfo%sfr_surf(DecidSurf) = 0.25d0
siteInfo%lc_evetr%lai%base_temperature = 5.0d0
siteInfo%lc_evetr%lai%base_temperature_senescence = 10.0d0
siteInfo%lc_evetr%lai%gdd_base = 100.0d0
siteInfo%lc_evetr%lai%sdd_base = -1000.0d0
siteInfo%lc_evetr%lai%lai_min = 1.0d0
siteInfo%lc_evetr%lai%lai_max = 3.0d0
siteInfo%lc_evetr%lai%lai_power = [0.03d0, 0.0005d0, 0.03d0, 0.0005d0]
siteInfo%lc_evetr%Alb_Min = 0.10d0
siteInfo%lc_evetr%Alb_Max = 0.20d0
siteInfo%lc_dectr%lai%lai_power = [0.03d0, 0.0005d0, 0.03d0, 0.0005d0]
modState%phenState%LAI_id = [1.5d0, 2.0d0, 2.5d0]
modState%phenState%GDD_id = [150.0d0, 150.0d0, 150.0d0]
modState%phenState%SDD_id = [0.0d0, 0.0d0, 0.0d0]
modState%phenState%albEveTr_id = 0.12d0
call clear_fpe()
call SUEWS_cal_DailyState(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(123)
if (supy_error_flag()) stop 34
! Tmin=12, Tmax=22 -> mean=17, base_t=5 -> delta_gdd=12 -> GDD=150+12=162
if (abs(modState%phenState%GDD_id(ivConif) - 162.0d0) > 1.0d-12) stop 35
if (modState%phenState%LAI_id(ivConif) <= 1.5d0) stop 36
! Direct update_Veg witness: absent EveTr with valid bounds evolves 0.12 -> 0.17
alb_eve = 0.12d0
call clear_fpe()
call update_Veg( &
   LAImax=[3.0d0, 3.0d0, 3.0d0], LAIMin=[1.0d0, 1.0d0, 1.0d0], &
   AlbMax_DecTr=0.18d0, AlbMax_EveTr=0.20d0, AlbMax_Grass=0.25d0, &
   AlbMin_DecTr=0.12d0, AlbMin_EveTr=0.10d0, AlbMin_Grass=0.15d0, &
   CapMax_dec=0.8d0, CapMin_dec=0.3d0, &
   PorMax_dec=0.6d0, PorMin_dec=0.2d0, &
   LAI_id=[2.0d0, 1.0d0, 1.0d0], LAI_id_prev=[1.0d0, 1.0d0, 1.0d0], &
   DecidCap_id=decid_cap, albDecTr_id=alb_dec, albEveTr_id=alb_eve, &
   albGrass_id=alb_gra, porosity_id=poros, StoreDrainPrm=store_drain, &
   sfr_veg=[0.0d0, 0.25d0, 0.25d0])
call assert_no_fpe(124)
if (abs(alb_eve - 0.17d0) > 1.0d-12) stop 37

! 3e. Absent surface with zero initial LAI, lai_min=0, lai_max=3,
!     lai_power=[0, 0.01, -1, 0.01] during growth season (delta_gdd=10) and
!     nearby non-sentinel sdd_base=-999.0000005 must preserve valid growth
!     to LAI=0.1 and GDD accumulation (negative senescence exponent unused).
call reset_supy_error()
config%LAImethod = 1
siteInfo%sfr_surf(ConifSurf) = 0.0d0
siteInfo%lc_evetr%lai%base_temperature = 7.0d0
siteInfo%lc_evetr%lai%base_temperature_senescence = 10.0d0
siteInfo%lc_evetr%lai%gdd_base = 100.0d0
siteInfo%lc_evetr%lai%sdd_base = -999.0000005d0
siteInfo%lc_evetr%lai%lai_min = 0.0d0
siteInfo%lc_evetr%lai%lai_max = 3.0d0
siteInfo%lc_evetr%lai%lai_power = [0.0d0, 0.01d0, -1.0d0, 0.01d0]
modState%phenState%Tmin_id = 12.0d0
modState%phenState%Tmax_id = 22.0d0
modState%phenState%LAI_id = [0.0d0, 2.0d0, 2.5d0]
modState%phenState%GDD_id = [150.0d0, 150.0d0, 150.0d0]
modState%phenState%SDD_id = [0.0d0, 0.0d0, 0.0d0]
call clear_fpe()
call SUEWS_cal_DailyState(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(127)
if (supy_error_flag()) stop 38
! mean T=17, base_t=7 -> delta_gdd=10 -> GDD=160; LAI = 0**0 * 10 * 0.01 + 0 = 0.1
if (abs(modState%phenState%GDD_id(ivConif) - 160.0d0) > 1.0d-12) stop 39
if (abs(modState%phenState%LAI_id(ivConif) - 0.1d0) > 1.0d-12) stop 43

! -------------------------------------------------------------------------
! 4. update_DailyState_Day with calc_hdd=.false., calc_cdd=.false. and
!    NaN / -999 BaseT_Heating / BaseT_Cooling.
! -------------------------------------------------------------------------
tmin_id = 15.0d0
tmax_id = 15.0d0
lenday_id = 0.0d0
hdd_id = 0.0d0
call clear_fpe()
call update_DailyState_Day( &
   DayofWeek_id=[2, 1, 1], avkdn=200.0d0, Tair=12.0d0, Precip=1.5d0, &
   BaseT_Heating=[qnan, -999.0d0], BaseT_Cooling=[-999.0d0, qnan], &
   nsh_real=12.0d0, Tmin_id=tmin_id, Tmax_id=tmax_id, lenDay_id=lenday_id, &
   HDD_id=hdd_id, calc_hdd=.false., calc_cdd=.false.)
call assert_no_fpe(108)
if (hdd_id(1) /= 0.0d0 .or. hdd_id(2) /= 0.0d0) stop 40
if (abs(hdd_id(3) - 12.0d0) > 1.0d-12 .or. abs(hdd_id(5) - 1.5d0) > 1.0d-12) stop 41
if (abs(tmin_id - 12.0d0) > 1.0d-12 .or. abs(tmax_id - 15.0d0) > 1.0d-12) stop 42

! -------------------------------------------------------------------------
! 5. AnthropogenicEmissions for emissions=1..6 and SUEWS_cal_BiogenCO2 for
!    emissions < 11 with literal NaN inactive CO2/biogenic/traffic/cooling
!    parameters and zero EnEF_v_Jkm denominator (for 1..3).
! -------------------------------------------------------------------------
ah_prof = 1.0d0
hum_prof = 1.5d0
pop_prof = 1.5d0
hdd_id = 0.0d0
hdd_id(7) = 5.0d0
hdd_id(8) = 2.0d0
hdd_id(10) = 12.0d0

do em = 1, 6
   call reset_supy_error()
   if (em <= 3) then
      traff_prof = qnan
   else
      traff_prof = 1.0d0
   end if
   call clear_fpe()
   call AnthropogenicEmissions( &
      CO2PointSource=qnan, EmissionsMethod=em, &
      it=12, imin=0, DLS=0, DayofWeek_id=[2, 1, 1], &
      EF_umolCO2perJ=qnan, FcEF_v_kgkm=[qnan, qnan], &
      EnEF_v_Jkm=merge(0.0d0, 3.0d6, em <= 3), &
      TrafficUnits=merge(qnan, 1.0d0, em <= 3), &
      FrFossilFuel_Heat=qnan, FrFossilFuel_NonHeat=qnan, &
      MinFCMetab=qnan, MaxFCMetab=qnan, &
      MinQFMetab=100.0d0, MaxQFMetab=175.0d0, &
      PopDensDaytime=[100.0d0, 80.0d0], PopDensNighttime=90.0d0, &
      Tair=10.0d0, HDD_id=hdd_id, &
      Qf_A=[0.05d0, 0.04d0], Qf_B=[0.01d0, 0.01d0], Qf_C=[0.02d0, 0.02d0], &
      AH_MIN=[15.0d0, 12.0d0], &
      AH_SLOPE_Heating=[2.0d0, 1.8d0], &
      AH_SLOPE_Cooling=merge([qnan, qnan], [1.5d0, 1.2d0], em == 1 .or. em == 4), &
      BaseT_Heating=[18.0d0, 18.0d0], &
      BaseT_Cooling=merge([qnan, qnan], [25.0d0, 25.0d0], em == 1 .or. em == 4), &
      TrafficRate=merge([qnan, qnan], [0.04d0, 0.02d0], em <= 3), &
      QF0_BEU=[0.5d0, 0.5d0], QF_SAHP=qf_sahp, &
      Fc_anthro=fc_anthro, Fc_metab=fc_metab, Fc_traff=fc_traff, &
      Fc_build=fc_build, Fc_point=fc_point, &
      AHProf_24hr=ah_prof, HumActivity_24hr=hum_prof, &
      TraffProf_24hr=traff_prof, PopProf_24hr=pop_prof, &
      SurfaceArea=1.0d6, QF_build_out=qf_build_out)
   call assert_no_fpe(110 + em)
   if (supy_error_flag()) stop 50
   if (.not. ieee_is_finite(qf_sahp) .or. .not. ieee_is_finite(qf_build_out)) stop 51
   if (fc_anthro /= 0.0d0 .or. fc_metab /= 0.0d0 .or. fc_traff /= 0.0d0 .or. &
       fc_build /= 0.0d0 .or. fc_point /= 0.0d0) stop 52
end do

! SUEWS_cal_BiogenCO2 with EmissionsMethod=2 and poisoned biogenic CO2 params
call reset_supy_error()
config%EmissionsMethod = 2
siteInfo%lc_evetr%bioco2%alpha_bioco2 = qnan
siteInfo%lc_dectr%bioco2%beta_bioco2 = qnan
siteInfo%lc_grass%bioco2%theta_bioco2 = qnan
siteInfo%lc_evetr%bioco2%resp_a = qnan
siteInfo%lc_dectr%bioco2%resp_b = qnan
siteInfo%lc_grass%bioco2%min_res_bioco2 = qnan
modState%anthroEmisState%Fc_anthro = 0.0d0
call clear_fpe()
call SUEWS_cal_BiogenCO2(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(120)
if (supy_error_flag()) stop 53
if (modState%anthroEmisState%Fc_biogen /= 0.0d0 .or. &
    modState%anthroEmisState%Fc /= 0.0d0) stop 54

! SUEWS_cal_BiogenCO2 for active methods 32 (31..36) and 42 (41..46) with
! absent DecidSurf (sfr=0) poisoned with NaN biogenic CO2 parameters.
siteInfo%sfr_surf = 0.0d0
siteInfo%sfr_surf(PavSurf) = 0.5d0
siteInfo%sfr_surf(ConifSurf) = 0.25d0
siteInfo%sfr_surf(DecidSurf) = 0.0d0
siteInfo%sfr_surf(GrassSurf) = 0.25d0
siteInfo%conductance%gs_model = 1
config%RSLLevel = 0
config%SMDMethod = 0
config%Diagnose = 0
forcing%kdown = 400.0d0
forcing%Temp_C = 20.0d0
forcing%RH = 50.0d0
forcing%pres = 1013.0d0
modState%snowState%snow_fraction = 0.0d0
modState%phenState%LAI_id = [4.0d0, 1.0d0, 2.5d0]
modState%phenState%g_func = 0.5d0
siteInfo%lc_evetr%lai%lai_min = 4.0d0
siteInfo%lc_evetr%lai%lai_max = 5.0d0
siteInfo%lc_evetr%bioco2%alpha_bioco2 = 0.05d0
siteInfo%lc_evetr%bioco2%alpha_enh_bioco2 = 0.01d0
siteInfo%lc_evetr%bioco2%beta_bioco2 = -2.0d0
siteInfo%lc_evetr%bioco2%beta_enh_bioco2 = 20.0d0
siteInfo%lc_evetr%bioco2%theta_bioco2 = 0.8d0
siteInfo%lc_evetr%bioco2%resp_a = 1.5d0
siteInfo%lc_evetr%bioco2%resp_b = 0.05d0
siteInfo%lc_evetr%bioco2%min_res_bioco2 = 0.6d0
siteInfo%lc_grass%lai%lai_min = 1.6d0
siteInfo%lc_grass%lai%lai_max = 5.0d0
siteInfo%lc_grass%bioco2%alpha_bioco2 = 0.05d0
siteInfo%lc_grass%bioco2%alpha_enh_bioco2 = 0.01d0
siteInfo%lc_grass%bioco2%beta_bioco2 = -2.0d0
siteInfo%lc_grass%bioco2%beta_enh_bioco2 = 20.0d0
siteInfo%lc_grass%bioco2%theta_bioco2 = 0.8d0
siteInfo%lc_grass%bioco2%resp_a = 1.5d0
siteInfo%lc_grass%bioco2%resp_b = 0.05d0
siteInfo%lc_grass%bioco2%min_res_bioco2 = 0.6d0
! Poison absent DecidSurf biogenic CO2 and LAI parameters with NaN
siteInfo%lc_dectr%lai%lai_min = qnan
siteInfo%lc_dectr%lai%lai_max = qnan
siteInfo%lc_dectr%bioco2%alpha_bioco2 = qnan
siteInfo%lc_dectr%bioco2%alpha_enh_bioco2 = qnan
siteInfo%lc_dectr%bioco2%beta_bioco2 = qnan
siteInfo%lc_dectr%bioco2%beta_enh_bioco2 = qnan
siteInfo%lc_dectr%bioco2%theta_bioco2 = qnan
siteInfo%lc_dectr%bioco2%resp_a = qnan
siteInfo%lc_dectr%bioco2%resp_b = qnan
siteInfo%lc_dectr%bioco2%min_res_bioco2 = qnan

call reset_supy_error()
config%EmissionsMethod = 32
modState%anthroEmisState%Fc_anthro = 1.0d0
call clear_fpe()
call SUEWS_cal_BiogenCO2(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(125)
if (supy_error_flag()) stop 55
if (.not. (ieee_is_finite(modState%anthroEmisState%Fc_photo) .and. &
           ieee_is_finite(modState%anthroEmisState%Fc_biogen) .and. &
           ieee_is_finite(modState%anthroEmisState%Fc))) stop 56
if (modState%anthroEmisState%Fc_photo >= 0.0d0) stop 57

call reset_supy_error()
config%EmissionsMethod = 42
siteInfo%lc_evetr%bioco2%beta_bioco2 = 20.0d0
siteInfo%lc_grass%bioco2%beta_bioco2 = 20.0d0
modState%anthroEmisState%Fc_anthro = 1.0d0
call clear_fpe()
call SUEWS_cal_BiogenCO2(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(126)
if (supy_error_flag()) stop 58
! Analytical Fc_photo for em=42: -(0.25*4.0*20.0 + 0.25*2.5*20.0)*0.5 = -16.25
if (abs(modState%anthroEmisState%Fc_photo - (-16.25d0)) > 1.0d-12) stop 59
if (.not. (ieee_is_finite(modState%anthroEmisState%Fc_biogen) .and. &
           ieee_is_finite(modState%anthroEmisState%Fc))) stop 64

! Direct leaf CO2_biogen probes with literal NaNs in absent entries:
! (a) Method 42 with literal NaN in absent DecidSurf beta_bioCO2
call clear_fpe()
call CO2_biogen( &
   [0.05d0, qnan, 0.05d0], [0.01d0, qnan, 0.01d0], 400.0d0, &
   [20.0d0, qnan, 20.0d0], [2.0d0, qnan, 2.0d0], BSoilSurf, &
   ConifSurf, DecidSurf, 120.5d0, 42, 0.5d0, 0.5d0, GrassSurf, 1, &
   120, 12, ivConif, ivDecid, ivGrass, [4.0d0, 1.0d0, 2.5d0], &
   [4.0d0, qnan, 1.6d0], [5.0d0, qnan, 5.0d0], [0.6d0, qnan, 0.6d0], &
   nsurf, nvegsurf, [1.5d0, qnan, 1.5d0], [0.05d0, qnan, 0.05d0], &
   [0.5d0, 0.0d0, 0.25d0, 0.0d0, 0.25d0, 0.0d0, 0.0d0], &
   [0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0], &
   20.0d0, [0.8d0, qnan, 0.8d0], fc_bio_leaf, fc_photo_leaf, fc_respi_leaf)
call assert_no_fpe(128)
if (abs(fc_photo_leaf - (-16.25d0)) > 1.0d-12) stop 68

! (b) Method 32 with absent ConifSurf (sfr=[0, 0.25, 0.25]) and literal NaNs
!     for unequal present coefficients (exercises weighted present-only branch)
call clear_fpe()
call CO2_biogen( &
   [qnan, 0.02d0, 0.04d0], [qnan, 0.01d0, 0.01d0], 400.0d0, &
   [qnan, -2.0d0, -3.0d0], [qnan, 20.0d0, 20.0d0], BSoilSurf, &
   ConifSurf, DecidSurf, 120.5d0, 32, 0.5d0, 0.5d0, GrassSurf, 1, &
   120, 12, ivConif, ivDecid, ivGrass, [1.0d0, 2.0d0, 2.5d0], &
   [qnan, 1.0d0, 1.6d0], [qnan, 5.0d0, 5.0d0], [qnan, 0.6d0, 0.6d0], &
   nsurf, nvegsurf, [qnan, 1.5d0, 1.5d0], [qnan, 0.05d0, 0.05d0], &
   [0.5d0, 0.0d0, 0.0d0, 0.25d0, 0.25d0, 0.0d0, 0.0d0], &
   [0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0], &
   20.0d0, [qnan, 0.8d0, 0.7d0], fc_bio_leaf, fc_photo_leaf, fc_respi_leaf)
call assert_no_fpe(129)
if (.not. (ieee_is_finite(fc_photo_leaf) .and. ieee_is_finite(fc_bio_leaf))) stop 69

! (c) Method 32 with absent ConifSurf and VALID unequal coefficients
!     [0.01, 0.02, 0.04] must preserve the exact legacy 3-surface denominator
!     (0.03 / 0.07) through SUEWS_cal_BiogenCO2 as well as CO2_biogen directly.
siteInfo%sfr_surf = 0.0d0
siteInfo%sfr_surf(PavSurf) = 0.5d0
siteInfo%sfr_surf(ConifSurf) = 0.0d0
siteInfo%sfr_surf(DecidSurf) = 0.25d0
siteInfo%sfr_surf(GrassSurf) = 0.25d0
siteInfo%lc_evetr%bioco2%alpha_bioco2 = 0.01d0
siteInfo%lc_dectr%bioco2%alpha_bioco2 = 0.02d0
siteInfo%lc_grass%bioco2%alpha_bioco2 = 0.04d0
siteInfo%lc_evetr%bioco2%alpha_enh_bioco2 = 0.0d0
siteInfo%lc_evetr%bioco2%beta_bioco2 = -2.0d0
siteInfo%lc_dectr%bioco2%beta_bioco2 = -2.0d0
siteInfo%lc_grass%bioco2%beta_bioco2 = -3.0d0
siteInfo%lc_evetr%bioco2%beta_enh_bioco2 = 20.0d0
siteInfo%lc_evetr%bioco2%theta_bioco2 = 0.8d0
siteInfo%lc_dectr%bioco2%theta_bioco2 = 0.8d0
siteInfo%lc_grass%bioco2%theta_bioco2 = 0.7d0
siteInfo%lc_dectr%lai%lai_min = 1.0d0
siteInfo%lc_dectr%lai%lai_max = 5.0d0
siteInfo%lc_dectr%bioco2%resp_a = 1.5d0
siteInfo%lc_dectr%bioco2%resp_b = 0.05d0
siteInfo%lc_dectr%bioco2%min_res_bioco2 = 0.6d0
modState%phenState%LAI_id = [1.0d0, 2.0d0, 2.5d0]
config%EmissionsMethod = 32
call clear_fpe()
call SUEWS_cal_BiogenCO2(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(130)
call CO2_biogen( &
   [0.01d0, 0.02d0, 0.04d0], [0.0d0, 0.0d0, 0.0d0], 400.0d0, &
   [-2.0d0, -2.0d0, -3.0d0], [20.0d0, 20.0d0, 20.0d0], BSoilSurf, &
   ConifSurf, DecidSurf, 120.5d0, 32, 0.5d0, 0.5d0, GrassSurf, 1, &
   120, 12, ivConif, ivDecid, ivGrass, [1.0d0, 2.0d0, 2.5d0], &
   [1.0d0, 1.0d0, 1.6d0], [5.0d0, 5.0d0, 5.0d0], [0.6d0, 0.6d0, 0.6d0], &
   nsurf, nvegsurf, [1.5d0, 1.5d0, 1.5d0], [0.05d0, 0.05d0, 0.05d0], &
   [0.5d0, 0.0d0, 0.0d0, 0.25d0, 0.25d0, 0.0d0, 0.0d0], &
   [0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0], &
   20.0d0, [0.8d0, 0.8d0, 0.7d0], fc_bio_leaf, fc_photo_ref, fc_respi_leaf)
if (abs(modState%anthroEmisState%Fc_photo - fc_photo_ref) > 1.0d-12) stop 70
! Verify legacy 3-surface denominator (0.03/0.07) differs from 2-surface (0.03/0.06)
if (abs(fc_photo_ref - fc_photo_leaf) <= 1.0d-6) stop 71

! Active CO2-enabled AnthropogenicEmissions (em=11, 12, 15, 42) with poisoned
! inactive sub-method parameters must produce finite non-zero Fc_anthro.
do em = 11, 15
   if (em == 13 .or. em == 14) cycle
   call reset_supy_error()
   if (em == 15) then
      traff_prof = 1.0d0
   else
      traff_prof = qnan
   end if
   call clear_fpe()
   call AnthropogenicEmissions( &
      CO2PointSource=0.0d0, EmissionsMethod=em, &
      it=12, imin=0, DLS=0, DayofWeek_id=[2, 1, 1], &
      EF_umolCO2perJ=1.0d0, FcEF_v_kgkm=[0.2d0, 0.2d0], &
      EnEF_v_Jkm=3.0d6, &
      TrafficUnits=merge(1.0d0, qnan, em == 15), &
      FrFossilFuel_Heat=0.8d0, FrFossilFuel_NonHeat=0.5d0, &
      MinFCMetab=100.0d0, MaxFCMetab=175.0d0, &
      MinQFMetab=100.0d0, MaxQFMetab=175.0d0, &
      PopDensDaytime=[100.0d0, 80.0d0], PopDensNighttime=90.0d0, &
      Tair=10.0d0, HDD_id=hdd_id, &
      Qf_A=[0.05d0, 0.04d0], Qf_B=[0.01d0, 0.01d0], Qf_C=[0.02d0, 0.02d0], &
      AH_MIN=[15.0d0, 12.0d0], &
      AH_SLOPE_Heating=[2.0d0, 1.8d0], &
      AH_SLOPE_Cooling=merge([qnan, qnan], [1.5d0, 1.2d0], em == 11), &
      BaseT_Heating=[18.0d0, 18.0d0], &
      BaseT_Cooling=merge([qnan, qnan], [25.0d0, 25.0d0], em == 11), &
      TrafficRate=merge([0.04d0, 0.02d0], [qnan, qnan], em == 15), &
      QF0_BEU=[0.5d0, 0.5d0], QF_SAHP=qf_sahp, &
      Fc_anthro=fc_anthro, Fc_metab=fc_metab, Fc_traff=fc_traff, &
      Fc_build=fc_build, Fc_point=fc_point, &
      AHProf_24hr=ah_prof, HumActivity_24hr=hum_prof, &
      TraffProf_24hr=traff_prof, PopProf_24hr=pop_prof, &
      SurfaceArea=1.0d6, QF_build_out=qf_build_out)
   call assert_no_fpe(130 + em)
   if (supy_error_flag()) stop 65
   if (.not. (ieee_is_finite(qf_sahp) .and. ieee_is_finite(fc_anthro))) stop 66
   if (fc_anthro <= 0.0d0) stop 67
end do

! -------------------------------------------------------------------------
! 6. LUMPS_cal_QHQE with mixed present/absent vegetation (DecidSurf sfr=0)
!    and NaN lai_max/lai_min on the absent surface.
! -------------------------------------------------------------------------
call reset_supy_error()
siteInfo%sfr_surf = 0.0d0
siteInfo%sfr_surf(PavSurf) = 0.5d0
siteInfo%sfr_surf(ConifSurf) = 0.25d0
siteInfo%sfr_surf(DecidSurf) = 0.0d0
siteInfo%sfr_surf(GrassSurf) = 0.25d0
siteInfo%lc_evetr%lai%lai_max = 5.1d0
siteInfo%lc_grass%lai%lai_max = 5.9d0
siteInfo%lc_dectr%lai%lai_max = qnan
siteInfo%lc_dectr%lai%lai_min = qnan
siteInfo%lumps%veg_type = 1
siteInfo%lumps%drain_rate = 0.25d0
siteInfo%lumps%rain_max_res = 10.0d0
siteInfo%lumps%rain_cover = 1.0d0
config%SnowUse = 0
clock%tstep_real = 300.0d0
clock%nsh_real = 12.0d0
forcing%Temp_C = 18.0d0
forcing%pres = 1013.0d0
forcing%rain = 0.0d0
modState%flagState%i_iter = 1
modState%atmState%av_cp = 1005.0d0
modState%atmState%lv_J_kg = 2.45d6
modState%heatState%qn = 200.0d0
modState%heatState%qf = 20.0d0
modState%heatState%qs = 30.0d0
modState%snowState%qm = 0.0d0
modState%phenState%LAI_id = [4.0d0, qnan, 2.5d0]

call clear_fpe()
call LUMPS_cal_QHQE(clock, config, forcing, siteInfo, modState)
call assert_no_fpe(121)
if (supy_error_flag()) stop 60
if (.not. ieee_is_finite(modState%heatState%QH_LUMPS)) stop 61
if (.not. ieee_is_finite(modState%heatState%QE_LUMPS)) stop 62
if (.not. ieee_is_finite(modState%phenState%veg_phen_lumps)) stop 63

write(*, '(a)') 'all inactive-scheme Fortran guards passed'

contains

subroutine clear_fpe()
   call ieee_set_flag(ieee_invalid, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
end subroutine clear_fpe

subroutine assert_no_fpe(code)
   integer, intent(in) :: code
   logical :: inv_flag, divz_flag
   call ieee_get_flag(ieee_invalid, inv_flag)
   call ieee_get_flag(ieee_divide_by_zero, divz_flag)
   if (inv_flag .or. divz_flag) then
      write(*, '(a,i0,a,l1,a,l1)') 'FPE raised at checkpoint ', code, &
         ' invalid=', inv_flag, ' divzero=', divz_flag
      stop 99
   end if
end subroutine assert_no_fpe

end program inactive_guards_probe
""",
        encoding="ascii",
    )
    executable = tmp_path / "inactive_guards_probe"
    compile_res = subprocess.run(
        [
            compiler,
            str(source),
            "-I",
            str(suews / "mod"),
            "-L",
            str(suews / "lib"),
            "-lsuewsdriver",
            "-lsuewsphys",
            "-lsuewsutil",
            "-lspartacus",
            "-o",
            str(executable),
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert compile_res.returncode == 0, compile_res.stderr
    run_res = subprocess.run(
        [str(executable)],
        capture_output=True,
        text=True,
        check=False,
    )
    assert run_res.returncode == 0, run_res.stdout + run_res.stderr
    assert "all inactive-scheme Fortran guards passed" in run_res.stdout
