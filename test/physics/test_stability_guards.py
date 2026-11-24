"""Kernel stability dispatch and non-finite z/L guards (GH#1783/1784/2171)."""

import copy
import gzip
from importlib import import_module
import json
import os
from pathlib import Path
import shutil
import subprocess

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-NATIVE-ERROR", "C-RESPONSE")]


@pytest.fixture(scope="module")
def stability_probe(tmp_path_factory):
    """Call the actual Fortran selectors, including their thread-local errors."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        pytest.skip("Native checks need gfortran and source-build libraries; run tests cover wheels")
    try:
        with gzip.open(suews / "mod" / "module_phys_atmmoiststab.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except gzip.BadGzipFile:
        is_gnu = False
    if not is_gnu:
        pytest.skip("Native harness needs GNU Fortran modules")
    directory = tmp_path_factory.mktemp("stability")
    source = directory / "probe.f95"
    source.write_text(
        """program probe
use module_phys_atmmoiststab
use module_ctrl_error_state, only: reset_supy_error, get_supy_error
implicit none
integer :: method, selector, code, status
real(kind(1d0)) :: zeta, value
character(len=512) :: message
do
  read(*, *, iostat=status) method, selector, zeta
  if (status /= 0) exit
  call reset_supy_error()
  select case(selector)
  case(1)
    value = stab_psi_mom(method, zeta)
  case(2)
    value = stab_psi_heat(method, zeta)
  case(3)
    value = stab_phi_mom(method, zeta)
  case(4)
    value = stab_phi_heat(method, zeta)
  end select
  call get_supy_error(code, message)
  write(*, '(ES24.16,1X,I0)') value, code
  write(*, '(A)') trim(message)
end do
end program
""",
        encoding="ascii",
    )
    executable = directory / "probe"
    compiled = subprocess.run(
        [compiler, str(source), "-I", str(suews / "mod"),
         "-L", str(suews / "lib"), "-lsuewsphys", "-lsuewsutil",
         "-lspartacus", "-fopenmp", "-o", str(executable)],
        check=False, capture_output=True, text=True,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run(method, selector, zeta):
        result = subprocess.run(
            [str(executable)], input=f"{method} {selector} {zeta}\n",
            check=True, capture_output=True, text=True,
        )
        lines = result.stdout.splitlines()
        value, code = lines[0].split()
        return float(value), int(code), lines[1].strip()

    return run


@pytest.mark.parametrize("method", [0, 1, -1, -999, 99])
@pytest.mark.parametrize("selector", [1, 2, 3, 4])
def test_reserved_and_unknown_kernel_codes_fail(stability_probe, method, selector):
    value, code, message = stability_probe(method, selector, -0.1)
    assert code == 107
    assert f"stability={method} is reserved or unsupported" in message
    recommendation = "3 (recommended), 2 or 4" if selector <= 2 else "3 (recommended) or 4"
    assert message.endswith(f"use {recommendation}.")
    assert value == (0.0 if selector <= 2 else 1.0)


@pytest.mark.parametrize("method", [3, 4, 5])
@pytest.mark.parametrize("selector", [1, 2, 3, 4])
@pytest.mark.parametrize("zeta", [-0.1, 0.0, 0.1])
def test_implemented_kernel_methods_remain_finite(stability_probe, method, selector, zeta):
    value, code, message = stability_probe(method, selector, zeta)
    assert code == 0, message
    assert np.isfinite(value)
    if zeta == 0:
        assert value == (0.0 if selector <= 2 else 1.0)


@pytest.fixture(scope="module")
def bridge_inputs(sample_config_loaded, sample_data_loaded):
    """Use the direct bridge's mapping input, which bypasses Pydantic."""
    run_rust = import_module("supy._run_rust")
    _, forcing = sample_data_loaded
    forcing = forcing.iloc[:2]
    return (
        run_rust._check_rust_available(),
        sample_config_loaded.model_dump(exclude_none=True, mode="json"),
        run_rust._prepare_forcing_block(forcing).ravel(order="C").tolist(),
        len(forcing),
    )


@pytest.mark.core
@pytest.mark.parametrize("method", [0, 1, -1, -999, 99])
def test_unchecked_mapping_reports_kernel_error(method, bridge_inputs):
    """Direct bridge callers also receive GH#1783's fatal kernel error."""
    rust, config, forcing, n_steps = bridge_inputs
    config = copy.deepcopy(config)
    config["model"]["physics"]["stability"] = {"value": method}
    with pytest.raises(RuntimeError, match=f"stability={method} is reserved or unsupported") as error:
        rust.run_suews_multi([json.dumps(config)], forcing, n_steps, 1)
    assert "(code 107)" in str(error.value)


@pytest.mark.core
@pytest.mark.parametrize("wind", [float("nan"), float("inf"), -float("inf")])
def test_non_finite_wind_reaches_driver_error_boundary(wind, sample_config_loaded, sample_data_loaded):
    """Unchecked bridge forcing must expose cal_Stab error 110 to Python."""
    run_rust = import_module("supy._run_rust")
    _, forcing = sample_data_loaded
    forcing = forcing.iloc[:2].copy()
    forcing["U"] = wind
    block = run_rust._prepare_forcing_block(forcing).ravel(order="C").tolist()
    config = sample_config_loaded.model_dump(exclude_none=True, mode="json")
    with pytest.raises(RuntimeError, match=r"code 110.*cal_Stab: non-finite"):
        run_rust._check_rust_available().run_suews_multi([json.dumps(config)], block, len(forcing), 1)


@pytest.mark.parametrize("selector", [1, 2, 3, 4])
@pytest.mark.parametrize("zeta", [-0.1, 0.0, 0.1])
def test_w16_psi_runs_but_phi_fails_explicitly(stability_probe, selector, zeta):
    """W16's implemented integral forms remain usable outside the RSL path."""
    value, code, message = stability_probe(2, selector, zeta)
    assert np.isfinite(value)
    if selector <= 2:
        assert code == 0, message
    else:
        assert code == 108
        assert "stability=2 (W16) has no phi implementation for RSL" in message
        assert "roughness_sublayer=0 (MOST)" in message
        assert value == 1.0


@pytest.mark.parametrize("method", [2, 3, 4])
@pytest.mark.parametrize("rsl", [0, 1, 2])
def test_stability_profile_combinations(method, rsl, sample_config_loaded, sample_data_loaded):
    """The public API refuses W16/RSL and retains finite working profiles."""
    config = sample_config_loaded.model_copy(deep=True)
    config.model.physics.stability = method
    config.model.physics.roughness_sublayer = rsl
    _, forcing = sample_data_loaded
    forcing = forcing.iloc[:TIMESTEPS_PER_DAY]
    simulation = sp.SUEWSSimulation()
    simulation.update_config(config, auto_load_forcing=False)
    simulation.update_forcing(forcing)
    run_options = dict(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)

    if method == 2 and rsl != 0:
        with pytest.raises(RuntimeError, match="has no phi implementation for RSL") as error:
            simulation.run(**run_options)
        assert "(code 108)" in str(error.value)
    else:
        output = simulation.run(**run_options).df
        diagnostics = output.loc[:, ("SUEWS", ["T2", "RH2", "Q2", "U10"])]
        assert len(diagnostics) == len(forcing)
        assert np.isfinite(diagnostics.to_numpy(dtype=float)).all()
        if method == 2 and rsl == 0:
            # Pre-guard origin/master: 2ffde7377024d58f5c3e46e61beea7c8164fdc7a.
            # Same sample config, first 288 forcing rows, stability=2,
            # roughness_sublayer=0 and n_jobs=1; see fixture provenance.
            # Last two rows refreshed for gh#1878 (DailyState daily means) and
            # gh#1919 (first day started at the run's first step).
            # Full reference refreshed for gh#1953 (shared physical constants)
            # with scripts/suews/gen_w16_most_output.py.
            path_reference = Path(__file__).resolve().parents[1] / "fixtures" / "w16_most.csv"
            df_expected = pd.read_csv(
                path_reference, index_col=[0, 1], parse_dates=[1], float_precision="round_trip",
            )
            df_actual = output["SUEWS"][["QH", "QE", "T2", "U10"]]
            pd.testing.assert_index_equal(df_actual.index, df_expected.index)
            pd.testing.assert_index_equal(df_actual.columns, df_expected.columns, check_names=False)
            # Numerical-noise allowance, much smaller than observational
            # uncertainty; this guards unchanged equations, not physical skill.
            np.testing.assert_allclose(
                df_actual.to_numpy(dtype=float), df_expected.to_numpy(dtype=float),
                rtol=1e-7, atol=1e-8, equal_nan=False,
                err_msg="W16/MOST changed from the recorded reference",
            )


DIRECT_TWO_SIDED_PROCS = (
    ("psi_mom_J12", 0.0),
    ("phi_mom_J12", 1.0),
    ("psi_heat_J12", 0.0),
    ("phi_heat_J12", 1.0),
    ("phi_mom_K75", 1.0),
    ("phi_heat_K75", 1.0),
    ("psi_mom_K75", 0.0),
    ("psi_heat_K75", 0.0),
    ("phi_mom_B71", 1.0),
    ("phi_heat_B71", 1.0),
    ("psi_mom_B71", 0.0),
    ("psi_heat_B71", 0.0),
    ("psi_mom_W16", 0.0),
    ("psi_heat_W16", 0.0),
)

DIRECT_G00_PROCS = (
    ("psi_mom_G00", 0.0),
    ("psi_heat_G00", 0.0),
    ("phi_mom_G00", 1.0),
    ("phi_heat_G00", 1.0),
)

DIRECT_CB05_PROCS = (
    ("psi_mom_CB05", 0.0),
    ("psi_heat_CB05", 0.0),
    ("phi_mom_CB05", 1.0),
    ("phi_heat_CB05", 1.0),
)

ALL_22_DIRECT_PROCS = tuple(
    name for name, _ in (*DIRECT_TWO_SIDED_PROCS, *DIRECT_G00_PROCS, *DIRECT_CB05_PROCS)
)

NONFINITE_KINDS = ("qnan", "+inf", "-inf")


def _native_probe_unavailable(reason):
    if os.environ.get("SUEWS_REQUIRE_FORTRAN_PROBES") == "1":
        pytest.fail(reason)
    pytest.skip(reason)


@pytest.fixture(scope="module")
def stability_nonfinite_probe(tmp_path_factory):
    """Compile a native Fortran probe for direct schemes, selectors, and callers (GH#2171)."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        _native_probe_unavailable(
            "Native checks need gfortran and source-build libraries; run tests cover wheels"
        )
    try:
        with gzip.open(suews / "mod" / "module_phys_atmmoiststab.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except (gzip.BadGzipFile, FileNotFoundError):
        is_gnu = False
    if not is_gnu:
        _native_probe_unavailable("Native harness needs GNU Fortran modules")

    directory = tmp_path_factory.mktemp("stability_nonfinite")
    source = directory / "probe_nonfinite.f95"
    source.write_text(
        """program probe_nonfinite
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
   ieee_positive_inf, ieee_negative_inf, ieee_is_nan, ieee_is_finite
use module_phys_atmmoiststab
use module_phys_resist, only: AerodynamicResistance
use module_phys_rslprof, only: cal_profile_MOST, cal_profile_RSL, &
   cal_beta_lc, cal_cm, cal_ch, cal_psim_hat, cal_psih_hat, cal_z0_RSL
use module_ctrl_type, only: SUEWS_STATE
use module_ctrl_error_state, only: reset_supy_error, get_supy_error, supy_error_flag
implicit none

character(len=32) :: mode, target_name, input_kind
integer :: method, selector, do_reset, code, status, flag_i
real(kind(1d0)) :: finite_val, zl, val
real(kind(1d0)) :: l_mod, tstar, ustar, zl_out, ra, z0v, c2, cm, ch
real(kind(1d0)) :: zarr(30), uarr(30), tarr(30), qarr(30), psihatm(30), psihath(30)
type(SUEWS_STATE) :: state
character(len=512) :: message
character(len=16) :: cls

do
  read(*, *, iostat=status) mode, target_name, method, selector, input_kind, finite_val, do_reset
  if (status /= 0) exit

  select case (trim(input_kind))
  case ('qnan')
    zl = ieee_value(0.0d0, ieee_quiet_nan)
  case ('+inf')
    zl = ieee_value(0.0d0, ieee_positive_inf)
  case ('-inf')
    zl = ieee_value(0.0d0, ieee_negative_inf)
  case default
    zl = finite_val
  end select

  if (do_reset /= 0) call reset_supy_error()
  val = 0.0d0

  if (trim(mode) == 'direct') then
    select case (trim(target_name))
    case ('psi_mom_J12');   val = psi_mom_J12(zl)
    case ('phi_mom_J12');   val = phi_mom_J12(zl)
    case ('psi_heat_J12');  val = psi_heat_J12(zl)
    case ('phi_heat_J12');  val = phi_heat_J12(zl)
    case ('psi_mom_G00');   val = psi_mom_G00(zl)
    case ('psi_heat_G00');  val = psi_heat_G00(zl)
    case ('phi_mom_G00');   val = phi_mom_G00(zl)
    case ('phi_heat_G00');  val = phi_heat_G00(zl)
    case ('psi_mom_CB05');  val = psi_mom_CB05(zl)
    case ('psi_heat_CB05'); val = psi_heat_CB05(zl)
    case ('phi_mom_CB05');  val = phi_mom_CB05(zl)
    case ('phi_heat_CB05'); val = phi_heat_CB05(zl)
    case ('phi_mom_K75');   val = phi_mom_K75(zl)
    case ('phi_heat_K75');  val = phi_heat_K75(zl)
    case ('psi_mom_K75');   val = psi_mom_K75(zl)
    case ('psi_heat_K75');  val = psi_heat_K75(zl)
    case ('phi_mom_B71');   val = phi_mom_B71(zl)
    case ('phi_heat_B71');  val = phi_heat_B71(zl)
    case ('psi_mom_B71');   val = psi_mom_B71(zl)
    case ('psi_heat_B71');  val = psi_heat_B71(zl)
    case ('psi_mom_W16');   val = psi_mom_W16(zl)
    case ('psi_heat_W16');  val = psi_heat_W16(zl)
    case default
      stop 2
    end select
  else if (trim(mode) == 'selector') then
    select case (selector)
    case (1); val = stab_psi_mom(method, zl)
    case (2); val = stab_psi_heat(method, zl)
    case (3); val = stab_phi_mom(method, zl)
    case (4); val = stab_phi_heat(method, zl)
    case default
      stop 3
    end select
  else if (trim(mode) == 'caller') then
    psihatm = 0.0d0
    psihath = 0.0d0
    select case (trim(target_name))
    case ('stability_scales')
      call stability_scales(method, 25.8d0, 1.9d0, 2.43d0, -0.01d0, zl, l_mod, ustar, tstar)
      val = l_mod
    case ('cal_Stab_avdens')
      call cal_Stab(method, 25.8d0, 1.9d0, 14.2d0, 2.43d0, 20.0d0, -14.7d0, zl, 1005.0d0, &
                    l_mod, tstar, ustar, zl_out)
      val = zl_out
    case ('AerodynamicResistance')
      call AerodynamicResistance(25.8d0, 1.9d0, 2.43d0, zl, 0.4d0, 0.0d0, 0.3d0, &
                                 2, method, 2, ra, z0v, state)
      val = ra
    case ('cal_profile_MOST')
      call cal_profile_MOST(method, 30, 40.0d0, 14.2d0, 1.9d0, 0.01d0, zl, 2.43d0, &
                            20.0d0, -0.1d0, -0.0001d0, 8.0d0, zarr, uarr, tarr, qarr)
      val = uarr(30)
    case ('cal_profile_RSL')
      call cal_profile_RSL(method, 30, 20, 40.0d0, 15.0d0, zl, 10.0d0, 1.5d0, 0.35d0, &
                           1.7d0, 0.5d0, 0.1d0, 20.0d0, 0.4d0, -0.1d0, -0.0001d0, &
                           8.0d0, psihatm, psihath, zarr, uarr, tarr, qarr)
      val = uarr(30)
    case ('cal_beta_lc')
      val = cal_beta_lc(method, 0.35d0, zl)
    case ('cal_cm')
      call cal_cm(method, 15.0d0, 10.0d0, 20.0d0, 0.35d0, zl, c2, cm)
      val = cm
    case ('cal_ch')
      call cal_ch(method, 15.0d0, 10.0d0, 20.0d0, 0.35d0, zl, 0.5d0, 0.1d0, c2, ch)
      val = ch
    case ('cal_psim_hat')
      val = cal_psim_hat(method, 0.0d0, 0.0d0, 40.0d0, 30.0d0, 20.0d0, 0.5d0, 0.5d0, &
                         15.0d0, 10.0d0, zl, 0.35d0, 1.7d0, 20.0d0)
    case ('cal_psih_hat')
      val = cal_psih_hat(method, 0.0d0, 0.0d0, 40.0d0, 30.0d0, 20.0d0, 0.5d0, 0.5d0, &
                         15.0d0, 10.0d0, zl, 0.35d0, 1.7d0, 20.0d0)
    case ('cal_z0_RSL')
      val = cal_z0_RSL(method, 15.0d0, 10.0d0, 0.35d0, zl, 0.1d0)
    case default
      stop 4
    end select
  else
    stop 5
  end if

  call get_supy_error(code, message)
  flag_i = merge(1, 0, supy_error_flag())
  if (ieee_is_nan(val)) then
    cls = 'nan'
  else if (.not. ieee_is_finite(val)) then
    cls = 'inf'
  else
    cls = 'finite'
  end if
  write(*, '(A,1X,ES24.16E3,1X,I0,1X,I0)') trim(cls), val, flag_i, code
  write(*, '(A)') trim(message)
end do
end program probe_nonfinite
""",
        encoding="ascii",
    )
    executable = directory / "probe_nonfinite"
    compiled = subprocess.run(
        [
            compiler,
            "-ffree-line-length-none",
            str(source),
            "-I",
            str(suews / "mod"),
            "-L",
            str(suews / "lib"),
            "-lsuewsphys",
            "-lsuewsutil",
            "-lspartacus",
            "-fopenmp",
            "-o",
            str(executable),
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run_sequence(calls):
        payload = "".join(
            f"{mode} {target_name} {method} {selector} {input_kind} "
            f"{float(finite_val)!r} {1 if do_reset else 0}\n"
            for mode, target_name, method, selector, input_kind, finite_val, do_reset in calls
        )
        result = subprocess.run(
            [str(executable)],
            input=payload,
            check=True,
            capture_output=True,
            text=True,
        )
        lines = result.stdout.splitlines()
        assert len(lines) == 2 * len(calls)
        parsed = []
        for idx in range(len(calls)):
            cls, raw_val, flag_s, code_s = lines[2 * idx].split()
            val = float("nan") if cls == "nan" else float(raw_val)
            parsed.append((cls, val, int(flag_s), int(code_s), lines[2 * idx + 1].strip()))
        return parsed

    def run(mode, target_name, method, selector, input_kind, finite_val=0.0):
        return run_sequence(
            [(mode, target_name, method, selector, input_kind, finite_val, True)]
        )[0]

    run.sequence = run_sequence
    return run


@pytest.mark.core
@pytest.mark.parametrize("proc", ALL_22_DIRECT_PROCS)
@pytest.mark.parametrize("input_kind", NONFINITE_KINDS)
def test_all_22_stability_functions_reject_non_finite_zl(
    stability_nonfinite_probe, proc, input_kind
):
    """Every stability scheme reports error 112 and returns quiet NaN on non-finite z/L (GH#2171)."""
    cls, val, flag, code, message = stability_nonfinite_probe(
        "direct", proc, 0, 0, input_kind, 0.0
    )
    assert flag == 1
    assert code == 112
    assert message == f"{proc}: non-finite z/L"
    assert cls == "nan" and np.isnan(val)


@pytest.mark.core
@pytest.mark.parametrize("proc,neutral", DIRECT_G00_PROCS)
def test_g00_schemes_reject_stable_out_of_domain_zl(
    stability_nonfinite_probe, proc, neutral
):
    """G00 is defined only for unstable/neutral z/L and must reject stable z/L > neut_limit."""
    cls, val, flag, code, message = stability_nonfinite_probe(
        "direct", proc, 0, 0, "finite", 0.1
    )
    assert flag == 1
    assert code == 112
    assert f"{proc}: stable z/L > neut_limit" in message
    assert cls == "nan" and np.isnan(val)

    # Valid unstable and neutral controls remain error-free and finite.
    cls_u, val_u, flag_u, code_u, msg_u = stability_nonfinite_probe(
        "direct", proc, 0, 0, "finite", -0.5
    )
    assert flag_u == 0 and code_u == 0, msg_u
    assert cls_u == "finite" and np.isfinite(val_u)
    cls_n, val_n, flag_n, code_n, msg_n = stability_nonfinite_probe(
        "direct", proc, 0, 0, "finite", 0.0
    )
    assert flag_n == 0 and code_n == 0, msg_n
    assert cls_n == "finite" and val_n == neutral


@pytest.mark.core
@pytest.mark.parametrize("proc,neutral", DIRECT_CB05_PROCS)
def test_cb05_schemes_reject_unstable_out_of_domain_zl(
    stability_nonfinite_probe, proc, neutral
):
    """CB05 is defined only for stable/neutral z/L and must reject unstable z/L < -neut_limit."""
    cls, val, flag, code, message = stability_nonfinite_probe(
        "direct", proc, 0, 0, "finite", -0.1
    )
    assert flag == 1
    assert code == 112
    assert f"{proc}: unstable z/L < -neut_limit" in message
    assert cls == "nan" and np.isnan(val)

    # Valid stable and neutral controls remain error-free and finite.
    cls_s, val_s, flag_s, code_s, msg_s = stability_nonfinite_probe(
        "direct", proc, 0, 0, "finite", 0.5
    )
    assert flag_s == 0 and code_s == 0, msg_s
    assert cls_s == "finite" and np.isfinite(val_s)
    cls_n, val_n, flag_n, code_n, msg_n = stability_nonfinite_probe(
        "direct", proc, 0, 0, "finite", 0.0
    )
    assert flag_n == 0 and code_n == 0, msg_n
    assert cls_n == "finite" and val_n == neutral


@pytest.mark.core
@pytest.mark.parametrize("proc,neutral", DIRECT_TWO_SIDED_PROCS)
@pytest.mark.parametrize("zeta", [-0.5, 0.0, 0.5, 1.5])
def test_two_sided_stability_schemes_valid_finite_controls(
    stability_nonfinite_probe, proc, neutral, zeta
):
    """Valid unstable, neutral, and stable z/L controls stay finite with no error."""
    cls, val, flag, code, message = stability_nonfinite_probe(
        "direct", proc, 0, 0, "finite", zeta
    )
    assert flag == 0 and code == 0, message
    assert cls == "finite" and np.isfinite(val)
    if zeta == 0:
        assert val == pytest.approx(neutral, abs=0.0)


@pytest.mark.core
@pytest.mark.parametrize(
    "method,selector,expected_proc",
    [
        (2, 1, "psi_mom_W16"),
        (2, 2, "psi_heat_W16"),
        (3, 1, "psi_mom_K75"),
        (3, 2, "psi_heat_K75"),
        (3, 3, "phi_mom_K75"),
        (3, 4, "phi_heat_K75"),
        (4, 1, "psi_mom_B71"),
        (4, 2, "psi_heat_B71"),
        (4, 3, "phi_mom_B71"),
        (4, 4, "phi_heat_B71"),
        (5, 1, "psi_mom_J12"),
        (5, 2, "psi_heat_J12"),
        (5, 3, "phi_mom_J12"),
        (5, 4, "phi_heat_J12"),
    ],
)
@pytest.mark.parametrize("input_kind", NONFINITE_KINDS)
def test_public_selectors_reject_non_finite_zl(
    stability_nonfinite_probe, method, selector, expected_proc, input_kind
):
    """Supported public stability selectors report code 112 and NaN for qNaN, +Inf, and -Inf."""
    cls, val, flag, code, message = stability_nonfinite_probe(
        "selector", "sel", method, selector, input_kind, 0.0
    )
    assert flag == 1
    assert code == 112
    assert message == f"{expected_proc}: non-finite z/L"
    assert cls == "nan" and np.isnan(val)


@pytest.mark.core
@pytest.mark.parametrize("input_kind", NONFINITE_KINDS)
def test_public_selectors_keep_method_error_precedence_on_non_finite_zl(
    stability_nonfinite_probe, input_kind
):
    """Method errors 107 (reserved/unknown) and 108 (W16 phi) take precedence over z/L checks."""
    for selector in (3, 4):
        cls, val, flag, code, message = stability_nonfinite_probe(
            "selector", "sel", 2, selector, input_kind, 0.0
        )
        assert flag == 1 and code == 108
        assert "has no phi implementation for RSL" in message
        assert cls == "finite" and val == pytest.approx(1.0, abs=0.0)

    for selector in (1, 2, 3, 4):
        cls, val, flag, code, message = stability_nonfinite_probe(
            "selector", "sel", 0, selector, input_kind, 0.0
        )
        assert flag == 1 and code == 107
        assert "stability=0 is reserved or unsupported" in message
        expected_neutral = 0.0 if selector <= 2 else 1.0
        assert cls == "finite" and val == pytest.approx(expected_neutral, abs=0.0)


@pytest.mark.core
@pytest.mark.parametrize(
    "caller,method,input_kind,finite_val",
    [
        ("stability_scales", 3, "qnan", 0.0),
        ("stability_scales", 4, "+inf", 0.0),
        ("stability_scales", 5, "-inf", 0.0),
        ("cal_Stab_avdens", 3, "qnan", 0.0),
        ("AerodynamicResistance", 3, "qnan", 0.0),
        ("AerodynamicResistance", 4, "finite", 0.0),
        ("cal_profile_MOST", 3, "qnan", 0.0),
        ("cal_profile_MOST", 3, "finite", 0.0),
        ("cal_profile_RSL", 3, "qnan", 0.0),
        ("cal_beta_lc", 3, "qnan", 0.0),
        ("cal_beta_lc", 3, "-inf", 0.0),
        ("cal_cm", 3, "qnan", 0.0),
        ("cal_ch", 3, "qnan", 0.0),
        ("cal_psim_hat", 3, "qnan", 0.0),
        ("cal_psih_hat", 3, "qnan", 0.0),
        ("cal_z0_RSL", 3, "qnan", 0.0),
    ],
)
def test_non_finite_zl_propagates_code_112_through_callers(
    stability_nonfinite_probe, caller, method, input_kind, finite_val
):
    """Cross-caller non-finite stability inputs set fatal code 112 without kernel abort."""
    _, _, flag, code, message = stability_nonfinite_probe(
        "caller", caller, method, 0, input_kind, finite_val
    )
    assert flag == 1
    assert code == 112
    assert "non-finite z/L" in message


@pytest.mark.core
@pytest.mark.parametrize(
    "invalid_call,valid_call,expect_nan,expected_substr",
    [
        (
            ("direct", "psi_mom_G00", 0, 0, "finite", 0.1),
            ("direct", "psi_mom_G00", 0, 0, "finite", -0.5),
            True,
            "psi_mom_G00: stable z/L > neut_limit",
        ),
        (
            ("selector", "sel", 3, 1, "qnan", 0.0),
            ("selector", "sel", 3, 1, "finite", -0.1),
            True,
            "psi_mom_K75: non-finite z/L",
        ),
        (
            ("caller", "cal_Stab_avdens", 3, 0, "qnan", 0.0),
            ("caller", "cal_Stab_avdens", 3, 0, "finite", 1.2),
            False,
            "non-finite z/L",
        ),
        (
            ("caller", "stability_scales", 3, 0, "qnan", 0.0),
            ("caller", "stability_scales", 3, 0, "finite", -0.1),
            True,
            "non-finite z/L",
        ),
    ],
)
def test_same_thread_tls_recovery_after_code_112(
    stability_nonfinite_probe, invalid_call, valid_call, expect_nan, expected_substr
):
    """Fatal code 112 is sticky until TLS reset, then the same thread recovers cleanly."""
    clean, failed, sticky_valid, recovered = stability_nonfinite_probe.sequence(
        [
            (*valid_call, True),
            (*invalid_call, True),
            (*valid_call, False),
            (*valid_call, True),
        ]
    )

    assert clean[0] == "finite" and np.isfinite(clean[1])
    assert clean[2] == 0 and clean[3] == 0 and not clean[4]

    assert failed[2] == 1 and failed[3] == 112
    assert expected_substr in failed[4]
    if expect_nan:
        assert failed[0] == "nan" and np.isnan(failed[1])

    assert sticky_valid[0] == "finite" and np.isfinite(sticky_valid[1])
    assert sticky_valid[1] == pytest.approx(clean[1], rel=0.0, abs=0.0)
    assert sticky_valid[2] == 1 and sticky_valid[3] == 112
    assert expected_substr in sticky_valid[4]

    assert recovered[0] == "finite" and np.isfinite(recovered[1])
    assert recovered[1] == pytest.approx(clean[1], rel=0.0, abs=0.0)
    assert recovered[2] == 0 and recovered[3] == 0 and not recovered[4]
