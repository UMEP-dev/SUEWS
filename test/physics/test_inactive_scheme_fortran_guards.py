"""Regression tests for Fortran inactive-scheme parameter guards (#2363, #2348).

Verifies at both the direct Fortran-kernel level (with literal IEEE NaN,
zero denominators, and -999 sentinels) and end-to-end through ``SUEWSSimulation``
that inactive-scheme parameters are not read and cannot corrupt outputs or
trigger errors:

- ``STEBBSmethod == 0`` (``STEBBS_NONE``): ``stebbsonlinecouple`` and
  ``gen_building`` ignore inactive STEBBS and ``building_archtype`` fields while
  preserving safe ``Textroof_C`` / ``Textwall_C`` allocation for downstream
  ``ASSOCIATE`` blocks.
- ``LAImethod == 0`` (observed LAI): ``update_gddlai``, ``update_Veg``, and
  ``LUMPS_cal_QHQE`` ignore inactive GDD/SDD phenology parameters and
  ``lai_min``; absent vegetation surfaces (``sfr == 0``) are also guarded in
  ``update_Veg``.
- ``EmissionsMethod`` inactive sub-schemes: ``update_DailyState_Day`` ignores
  inactive heating/cooling base temperatures (``emissions=0`` ignores both;
  L11 sub-methods ``1, 4, ...`` ignore cooling base temperatures), and
  ``AnthropogenicEmissions`` ignores inactive CO2 parameters (and inactive
  traffic profiles for sub-methods ``1..3``) when ``emissions`` is in ``1..6``.
"""

from __future__ import annotations

from collections.abc import Callable
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

N_DAYS = 2
RUN_STEPS = TIMESTEPS_PER_DAY * N_DAYS


def _assert_frames_bit_identical(
    df_a: pd.DataFrame,
    df_b: pd.DataFrame,
    *,
    groups: tuple[str, ...] = ("SUEWS", "DailyState"),
) -> None:
    """Assert selected output groups match bit-for-bit (including NaN masks)."""
    for group in groups:
        sub_a = df_a.xs(group, level="group", axis=1)
        sub_b = df_b.xs(group, level="group", axis=1)
        assert list(sub_a.columns) == list(sub_b.columns)
        arr_a = sub_a.to_numpy(dtype=np.float64)
        arr_b = sub_b.to_numpy(dtype=np.float64)
        nan_a = np.isnan(arr_a)
        nan_b = np.isnan(arr_b)
        assert np.array_equal(nan_a, nan_b), f"{group}: NaN mask differed"
        bits_a = arr_a[~nan_a].view(np.uint64)
        bits_b = arr_b[~nan_b].view(np.uint64)
        n_diff = int(np.count_nonzero(bits_a != bits_b))
        assert n_diff == 0, f"{group}: {n_diff} finite cell(s) differed at bit level"


def _run_short_sim(
    base_config,
    df_forcing_base: pd.DataFrame,
    *,
    mutate_config: Callable | None = None,
    mutate_forcing: Callable | None = None,
) -> pd.DataFrame:
    """Run a deterministic two-day simulation from a defensive copy."""
    cfg = base_config.model_copy(deep=True)
    cfg.model.control.start_time = None
    cfg.model.control.end_time = None
    if mutate_config is not None:
        mutate_config(cfg)

    df_forcing = df_forcing_base.iloc[:RUN_STEPS].copy()
    if mutate_forcing is not None:
        mutate_forcing(df_forcing)

    sim = SUEWSSimulation(cfg)
    sim.update_forcing(df_forcing)
    res = sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])
    return res.df


def _set_val(container, attr: str, value) -> None:
    """Assign either a plain attribute or a RefValue's ``.value`` in place."""
    current = getattr(container, attr)
    if hasattr(current, "value"):
        current.value = value
    else:
        setattr(container, attr, value)


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

    def configure_disabled_extreme(cfg) -> None:
        cfg.model.physics.stebbs.enabled = False
        # Even with RC_PARAMETERISE selected on the inactive STEBBS sub-switch,
        # extreme archetype / STEBBS properties must never be evaluated.
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

    df_base = _run_short_sim(
        base_cfg, df_forcing, mutate_config=configure_disabled_baseline
    )
    df_mut = _run_short_sim(
        base_cfg, df_forcing, mutate_config=configure_disabled_extreme
    )

    _assert_frames_bit_identical(
        df_base, df_mut, groups=("SUEWS", "DailyState", "RSL", "BEERS")
    )
    assert np.isfinite(df_mut.xs("SUEWS", level="group", axis=1)["QN"]).all()
    assert np.isfinite(df_mut.xs("SUEWS", level="group", axis=1)["QS"]).all()


def test_observed_lai_ignores_inactive_phenology_and_lai_min_parameters(
    _two_day_sample,
):
    """laimethod=0 outputs are bit-identical under perturbed GDD/SDD/lai_min params."""
    base_cfg, df_forcing = _two_day_sample

    def add_varying_observed_lai(df: pd.DataFrame) -> None:
        # Vary observed LAI across the two days so LAI_id /= LAI_id_prev in
        # update_Veg, proving update_Veg does not divide by (lai_max - lai_min).
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
            _set_val(lai, "lai_min", 0.05)
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


def test_native_fortran_guards_survive_nan_and_zero_inactive_parameters(tmp_path):
    """Direct Fortran probe verifies NaN / -999 / zero denominators stay inert."""
    compiler, suews = require_fortran_probe_toolchain(
        "Native inactive-scheme guard probe",
        libraries=("libsuewsphys.a", "libsuewsutil.a", "libspartacus.a"),
        modules=(
            "module_ctrl_const_options.mod",
            "module_ctrl_error_state.mod",
            "module_ctrl_type.mod",
            "module_phys_anthro.mod",
            "module_phys_dailystate.mod",
            "module_phys_lai.mod",
            "module_phys_stebbs.mod",
        ),
    )
    source = tmp_path / "inactive_guards_probe.f90"
    source.write_text(
        """program inactive_guards_probe
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan
use module_ctrl_const_allocate, only: ncolumnsDataOutSTEBBS, nsurf, nvegsurf
use module_ctrl_const_options, only: STEBBS_NONE, RC_PARAMETERISE
use module_ctrl_error_state, only: reset_supy_error, supy_error_flag
use module_ctrl_type, only: &
   SUEWS_CLOCK, SUEWS_CONFIG, SUEWS_FORCING, SUEWS_SITE, SUEWS_STATE, &
   STEBBS_STATE, STEBBS_PRM, BUILDING_ARCHETYPE_PRM, STEBBS_BLDG, error_state
use module_phys_stebbs, only: stebbsonlinecouple
use module_phys_lai, only: update_gddlai
use module_phys_dailystate, only: update_DailyState_Day, update_Veg
use module_phys_anthro, only: AnthropogenicEmissions
implicit none

real(kind(1d0)) :: qnan
type(SUEWS_CLOCK) :: clock
type(SUEWS_CONFIG) :: config
type(SUEWS_FORCING) :: forcing
type(SUEWS_SITE) :: siteInfo
type(SUEWS_STATE) :: modState
type(STEBBS_STATE) :: stebbsState
type(STEBBS_PRM) :: stebbsPrm
type(BUILDING_ARCHETYPE_PRM) :: arch
type(STEBBS_BLDG) :: bldg
type(error_state) :: errState
real(kind(1d0)) :: dtLine(5), outSTEBBS(ncolumnsDataOutSTEBBS - 5)

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

call gen_building(stebbsState, stebbsPrm, arch, config, bldg, 3, errState)
if (supy_error_flag()) stop 10
if (.not. allocated(bldg%Textroof_C) .or. size(bldg%Textroof_C) /= 3) stop 11
if (.not. allocated(bldg%Textwall_C) .or. size(bldg%Textwall_C) /= 3) stop 12
if (.not. all(ieee_is_finite(bldg%Textroof_C))) stop 13
if (.not. all(ieee_is_finite(bldg%Textwall_C))) stop 14

siteInfo%building_archtype = arch
siteInfo%stebbs = stebbsPrm
dtLine = 0.0d0
outSTEBBS = 0.0d0
call stebbsonlinecouple(clock, config, forcing, siteInfo, modState, dtLine, 3, outSTEBBS)
if (supy_error_flag()) stop 15
if (allocated(modState%stebbsState%buildings)) stop 16
if (any(outSTEBBS /= -999.0d0)) stop 17

! -------------------------------------------------------------------------
! 2. update_gddlai with lai_calc_yes == 0: must not read NaN/sentinel
!    base_t_gdd, base_t_sdd, gdd_base, sdd_base, lai_min, lai_max, lai_power.
! -------------------------------------------------------------------------
call reset_supy_error()
lai_obs = [1.2d0, 2.4d0, 3.6d0]
lai_prev = [1.0d0, 1.0d0, 1.0d0]
gdd = [10.0d0, 20.0d0, 30.0d0]
sdd = [-1.0d0, -2.0d0, -3.0d0]
base_t_gdd = qnan
base_t_sdd = -999.0d0
gdd_base = qnan
sdd_base = qnan
lai_min = -999.0d0
lai_max = qnan
lai_power = qnan

call update_gddlai( &
   id=120, lai_calc_yes=0, lat=51.5d0, lai_obs=lai_obs, &
   t_min_id_prev=5.0d0, t_max_id_prev=20.0d0, &
   base_t_gdd=base_t_gdd, base_t_sdd=base_t_sdd, &
   gdd_base=gdd_base, sdd_base=sdd_base, &
   lai_min=lai_min, lai_max=lai_max, lai_power=lai_power, &
   lai_id_prev=lai_prev, gdd_id=gdd, sdd_id=sdd, lai_id_next=lai_next)
if (supy_error_flag()) stop 20
if (any(lai_next /= lai_obs)) stop 21
if (any(gdd /= [10.0d0, 20.0d0, 30.0d0])) stop 22
if (any(sdd /= [-1.0d0, -2.0d0, -3.0d0])) stop 23

! -------------------------------------------------------------------------
! 3. update_Veg with lai_calc_yes == 0 (sentinel lai_min / NaN lai_max) and
!    with absent vegetation (sfr_veg == 0, NaN albedo/porosity bounds).
! -------------------------------------------------------------------------
decid_cap = 0.5d0
alb_dec = 0.14d0
alb_eve = 0.10d0
alb_gra = 0.20d0
poros = 0.45d0
store_drain = 0.0d0
call update_Veg( &
   LAImax=[qnan, qnan, qnan], LAIMin=[-999.0d0, -999.0d0, -999.0d0], &
   AlbMax_DecTr=0.18d0, AlbMax_EveTr=0.12d0, AlbMax_Grass=0.25d0, &
   AlbMin_DecTr=0.12d0, AlbMin_EveTr=0.08d0, AlbMin_Grass=0.15d0, &
   CapMax_dec=0.8d0, CapMin_dec=0.3d0, &
   PorMax_dec=0.6d0, PorMin_dec=0.2d0, &
   LAI_id=lai_obs, LAI_id_prev=lai_prev, &
   DecidCap_id=decid_cap, albDecTr_id=alb_dec, albEveTr_id=alb_eve, &
   albGrass_id=alb_gra, porosity_id=poros, StoreDrainPrm=store_drain, &
   lai_calc_yes=0)
if (.not. (ieee_is_finite(decid_cap) .and. ieee_is_finite(alb_dec) .and. &
           ieee_is_finite(alb_eve) .and. ieee_is_finite(alb_gra) .and. &
           ieee_is_finite(poros))) stop 30
if (abs(decid_cap - 0.5d0) > 1.0d-12 .or. abs(poros - 0.45d0) > 1.0d-12) stop 31

sfr_zero = 0.0d0
call update_Veg( &
   LAImax=[qnan, qnan, qnan], LAIMin=[qnan, qnan, qnan], &
   AlbMax_DecTr=qnan, AlbMax_EveTr=qnan, AlbMax_Grass=qnan, &
   AlbMin_DecTr=qnan, AlbMin_EveTr=qnan, AlbMin_Grass=qnan, &
   CapMax_dec=qnan, CapMin_dec=qnan, &
   PorMax_dec=qnan, PorMin_dec=qnan, &
   LAI_id=lai_obs, LAI_id_prev=lai_prev, &
   DecidCap_id=decid_cap, albDecTr_id=alb_dec, albEveTr_id=alb_eve, &
   albGrass_id=alb_gra, porosity_id=poros, StoreDrainPrm=store_drain, &
   lai_calc_yes=1, sfr_veg=sfr_zero)
if (.not. (ieee_is_finite(decid_cap) .and. ieee_is_finite(alb_dec) .and. &
           ieee_is_finite(alb_eve) .and. ieee_is_finite(alb_gra) .and. &
           ieee_is_finite(poros))) stop 32

! -------------------------------------------------------------------------
! 4. update_DailyState_Day with calc_hdd=.false., calc_cdd=.false. and
!    NaN / -999 BaseT_Heating / BaseT_Cooling.
! -------------------------------------------------------------------------
tmin_id = 15.0d0
tmax_id = 15.0d0
lenday_id = 0.0d0
hdd_id = 0.0d0
call update_DailyState_Day( &
   DayofWeek_id=[2, 1, 1], avkdn=200.0d0, Tair=12.0d0, Precip=1.5d0, &
   BaseT_Heating=[qnan, -999.0d0], BaseT_Cooling=[-999.0d0, qnan], &
   nsh_real=12.0d0, Tmin_id=tmin_id, Tmax_id=tmax_id, lenDay_id=lenday_id, &
   HDD_id=hdd_id, calc_hdd=.false., calc_cdd=.false.)
if (hdd_id(1) /= 0.0d0 .or. hdd_id(2) /= 0.0d0) stop 40
if (abs(hdd_id(3) - 12.0d0) > 1.0d-12 .or. abs(hdd_id(5) - 1.5d0) > 1.0d-12) stop 41
if (abs(tmin_id - 12.0d0) > 1.0d-12 .or. abs(tmax_id - 15.0d0) > 1.0d-12) stop 42

! -------------------------------------------------------------------------
! 5. AnthropogenicEmissions for emissions=1..6 with NaN CO2 parameters,
!    EnEF_v_Jkm=0.0 (for 1..3), NaN TraffProf_24hr (for 1..3), and
!    NaN BaseT_Cooling (for 1, 4).
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
   if (supy_error_flag()) stop 50
   if (.not. ieee_is_finite(qf_sahp) .or. .not. ieee_is_finite(qf_build_out)) stop 51
   if (fc_anthro /= 0.0d0 .or. fc_metab /= 0.0d0 .or. fc_traff /= 0.0d0 .or. &
       fc_build /= 0.0d0 .or. fc_point /= 0.0d0) stop 52
end do

write(*, '(a)') 'all inactive-scheme Fortran guards passed'
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
