"""Regression tests for deterministic INTENT(OUT) initialisation in ``drainage`` (#2363).

Verifies that:
1. End-to-end ``SUEWSSimulation`` runs with a wet zero-area non-water surface
   (``evetr.sfr == 0.0``, ``initial_states.evetr.state == 1.0``) and omitted or
   zero ``drain_eq`` produce deterministic finite ``0.0`` drainage on the
   absent surface, bit-identical active ``SUEWS`` outputs, and no spurious
   ``ErrorHint(61)`` warnings.
2. Direct Fortran-kernel calls to ``drainage`` and ``SUEWS_cal_Water`` always
   overwrite ``INTENT(OUT) drain_is`` with deterministic ``0.0D0`` for unset or
   unsupported ``DrainEq`` selectors (``0``, ``-999``, ``4``, ``1.0D30``,
   ``NaN``, ``+Inf``) even when the caller passes pre-poisoned signaling NaN or
   out-of-range values, without triggering IEEE invalid/divide-by-zero flags or
   spurious ``ErrorHint(61)`` warnings.
"""

from __future__ import annotations

import subprocess

from conftest import SHORT_RUN_STEPS, require_fortran_probe_toolchain
import numpy as np
import pytest

from supy import SUEWSSimulation
from supy.data_model.core.hydro import StorageDrainParams

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-RESPONSE", "C-NATIVE-ERROR"),
]


def _run_short_sim(cfg, df_forcing):
    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(df_forcing.copy())
    output = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    )
    return sim, output.df


@pytest.mark.parametrize("mode", ["omitted_block", "explicit_zero", "inactive_eq2"])
def test_wet_zero_area_surface_has_deterministic_zero_drainage(
    sample_config_loaded, sample_forcing_parsed, mode
):
    """A wet zero-area surface with drain_eq=0/omitted produces finite 0.0 drainage."""
    _, df_forcing = sample_forcing_parsed(SHORT_RUN_STEPS)

    base_cfg = sample_config_loaded.model_copy(deep=True)
    assert base_cfg.sites[0].properties.land_cover.evetr.sfr.value == 0.0
    base_cfg.sites[0].initial_states.evetr.state.value = 1.0
    _, df_base = _run_short_sim(base_cfg, df_forcing)

    cfg = sample_config_loaded.model_copy(deep=True)
    site = cfg.sites[0]
    assert site.properties.land_cover.evetr.sfr.value == 0.0
    site.initial_states.evetr.state.value = 1.0
    if mode == "omitted_block":
        site.properties.land_cover.evetr.storage_drain_params = StorageDrainParams()
    elif mode == "explicit_zero":
        params = site.properties.land_cover.evetr.storage_drain_params
        params.drain_eq.value = 0
        params.drain_coef_1 = None
        params.drain_coef_2 = None
    else:
        params = site.properties.land_cover.evetr.storage_drain_params
        params.drain_eq.value = 2
        params.drain_coef_1 = None
        params.drain_coef_2 = None

    sim, df_mut = _run_short_sim(cfg, df_forcing)

    debug_evetr_drain = df_mut.xs("debug", level="group", axis=1).xs(1, level="grid")[
        "drain_EveTr"
    ]
    assert np.isfinite(debug_evetr_drain.to_numpy()).all()
    assert (debug_evetr_drain.to_numpy() == 0.0).all()

    suews_base_df = df_base.xs("SUEWS", level="group", axis=1).xs(1, level="grid")
    suews_mut_df = df_mut.xs("SUEWS", level="group", axis=1).xs(1, level="grid")
    suews_base = suews_base_df.to_numpy(dtype=np.float64)
    suews_mut = suews_mut_df.to_numpy(dtype=np.float64)
    assert np.array_equal(np.isnan(suews_base), np.isnan(suews_mut))
    valid = ~np.isnan(suews_base)
    assert np.array_equal(
        suews_base[valid].view(np.uint64), suews_mut[valid].view(np.uint64)
    )

    drain_warnings = [
        w for w in sim.kernel_warnings if "SUEWS_drain" in str(w) or "61" in str(w)
    ]
    assert not drain_warnings, f"Unexpected drainage warning: {drain_warnings}"


@pytest.mark.e2e
def test_native_fortran_drainage_initialises_intent_out_without_fpe(tmp_path):
    """Direct Fortran probe verifies drain_is=0.0D0 with pre-poisoned sNaN/garbage."""
    compiler, suews = require_fortran_probe_toolchain(
        "Native drainage INTENT(OUT) initialisation probe",
        libraries=(
            "libsuewsdriver.a",
            "libsuewsphys.a",
            "libsuewsutil.a",
            "libspartacus.a",
        ),
        modules=(
            "module_ctrl_const_allocate.mod",
            "module_ctrl_driver.mod",
            "module_ctrl_error_state.mod",
            "module_ctrl_type.mod",
            "module_phys_waterdist.mod",
        ),
    )
    source = tmp_path / "drainage_init_probe.f90"
    source.write_text(
        """program drainage_init_probe
use, intrinsic :: ieee_arithmetic, only: &
   ieee_is_finite, ieee_value, ieee_quiet_nan, ieee_signaling_nan, ieee_positive_inf
use, intrinsic :: ieee_exceptions, only: &
   ieee_invalid, ieee_divide_by_zero, ieee_set_flag, ieee_get_flag
use module_ctrl_const_allocate, only: nsurf, PavSurf, ConifSurf, WaterSurf
use module_ctrl_driver, only: SUEWS_cal_Water
use module_ctrl_error_state, only: reset_supy_error, supy_error_flag
use module_ctrl_type, only: &
   SUEWS_CLOCK, SUEWS_CONFIG, SUEWS_FORCING, SUEWS_SITE, SUEWS_STATE
use module_phys_waterdist, only: drainage
implicit none

real(kind(1d0)) :: qnan, snan, pinf, drain_out, selectors(6), poisons(3)
integer :: i, j
type(SUEWS_CLOCK) :: clock
type(SUEWS_CONFIG) :: config
type(SUEWS_FORCING) :: forcing
type(SUEWS_SITE) :: siteInfo
type(SUEWS_STATE) :: modState

qnan = ieee_value(0.0d0, ieee_quiet_nan)
snan = ieee_value(0.0d0, ieee_signaling_nan)
pinf = ieee_value(0.0d0, ieee_positive_inf)

selectors = [0.0d0, -999.0d0, 4.0d0, 1.0d30, qnan, pinf]
poisons = [snan, 1.0d6, 0.5d0]

! 1. Direct calls to drainage with wet state_is=1.0 and unsupported DrainEq.
do i = 1, size(selectors)
   do j = 1, size(poisons)
      call reset_supy_error()
      call modState%errorState%reset()
      call modState%errorState%clear_log()
      drain_out = poisons(j)
      call clear_fpe()
      call drainage( &
         ConifSurf, 1.0d0, 0.5d0, selectors(i), qnan, qnan, 12.0d0, &
         drain_out, modState)
      call assert_no_fpe(10 * i + j)
      if (supy_error_flag()) stop 11
      if (.not. ieee_is_finite(drain_out)) stop 12
      if (drain_out /= 0.0d0) stop 13
      if (modState%errorState%flag .or. modState%errorState%n_total /= 0) stop 14
   end do
end do

! 2. Valid DrainEq 1, 2, 3 still produce expected finite positive drainage.
do i = 1, 3
   call reset_supy_error()
   call modState%errorState%reset()
   call modState%errorState%clear_log()
   drain_out = snan
   call clear_fpe()
   call drainage( &
      PavSurf, 2.0d0, 0.5d0, real(i, kind(1d0)), 0.5d0, 1.0d0, 12.0d0, &
      drain_out, modState)
   call assert_no_fpe(100 + i)
   if (supy_error_flag()) stop 21
   if (.not. ieee_is_finite(drain_out)) stop 22
   if (drain_out <= 0.0d0) stop 23
end do

! 3. SUEWS_cal_Water with wet zero-area ConifSurf and NaN/zero/active-eq2 StoreDrainPrm.
do i = 1, 2
   call reset_supy_error()
   call modState%errorState%reset()
   call modState%errorState%clear_log()
   clock%nsh_real = 12.0d0
   config%Diagnose = 0
   config%SnowUse = 0
   config%StorageHeatMethod = 1
   siteInfo%sfr_surf = 0.0d0
   siteInfo%sfr_surf(PavSurf) = 1.0d0
   siteInfo%sfr_surf(ConifSurf) = 0.0d0
   siteInfo%NonWaterFraction = 1.0d0
   siteInfo%lc_paved%waterdist%to_soilstore = 1.0d0
   modState%hydroState%state_surf = 0.0d0
   modState%hydroState%state_surf(PavSurf) = 2.0d0
   modState%hydroState%state_surf(ConifSurf) = 1.0d0
   modState%hydroState%drain_surf = 0.0d0
   modState%hydroState%drain_surf(ConifSurf) = snan
   modState%phenState%storage_drain_params = 0.0d0
   modState%phenState%storage_drain_params(:, PavSurf) = [0.5d0, 3.0d0, 10.0d0, 3.0d0, 0.5d0, 0.5d0]
   modState%phenState%storage_drain_params(:, ConifSurf) = qnan
   if (i == 1) then
      modState%phenState%storage_drain_params(2, ConifSurf) = 0.0d0
   else
      modState%phenState%storage_drain_params(2, ConifSurf) = 2.0d0
   end if

   call clear_fpe()
   call SUEWS_cal_Water(clock, config, forcing, siteInfo, modState)
   call assert_no_fpe(200 + i)
   if (supy_error_flag()) stop 31
   if (.not. ieee_is_finite(modState%hydroState%drain_surf(ConifSurf))) stop 32
   if (modState%hydroState%drain_surf(ConifSurf) /= 0.0d0) stop 33
   if (.not. ieee_is_finite(modState%hydroState%drain_per_tstep)) stop 34
   if (modState%hydroState%drain_per_tstep <= 0.0d0) stop 35
   if (modState%hydroState%drain_surf(WaterSurf) /= 0.0d0) stop 36
   if (modState%errorState%flag .or. modState%errorState%n_total /= 0) stop 37
end do

write(*, '(a)') 'drainage INTENT(OUT) probe passed'

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

end program drainage_init_probe
""",
        encoding="ascii",
    )
    executable = tmp_path / "drainage_init_probe"
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
    assert "drainage INTENT(OUT) probe passed" in run_res.stdout
