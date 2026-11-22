"""Fatal non-finite guards for RSL_cal_prms and RSLProfile (GH#2171)."""

import copy
import gzip
from importlib import import_module
from importlib.resources import as_file, files
import json
import os
from pathlib import Path
import shutil
import subprocess

from conftest import run_cli_command
import pandas as pd
import pytest
import yaml

import supy as sp
from supy.cmd.SUEWS import SUEWS
from supy.data_model import SUEWSConfig

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-RESPONSE", "C-ERROR"),
]

NONFINITE_KINDS = ("qnan", "+inf", "-inf")


def _native_probe_unavailable(reason: str) -> None:
    if os.environ.get("SUEWS_REQUIRE_FORTRAN_PROBES") == "1":
        pytest.fail(reason)
    pytest.skip(reason)


@pytest.fixture(scope="module")
def rsl_nonfinite_probe(tmp_path_factory):
    """Compile a native Fortran probe for RSL_cal_prms and RSLProfile."""
    compiler = shutil.which("gfortran")
    suews = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews / "lib" / "libsuewsphys.a").exists():
        _native_probe_unavailable(
            "Native checks need gfortran and source-build libraries; run tests cover wheels"
        )
    try:
        with gzip.open(suews / "mod" / "module_phys_rslprof.mod", "rb") as stream:
            is_gnu = stream.readline().startswith(b"GFORTRAN")
    except (gzip.BadGzipFile, FileNotFoundError):
        is_gnu = False
    if not is_gnu:
        _native_probe_unavailable("Native harness needs GNU Fortran modules")

    directory = tmp_path_factory.mktemp("rsl_nonfinite")
    source = directory / "probe_rsl.f95"
    source.write_text(
        """program probe_rsl
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
   ieee_positive_inf, ieee_negative_inf, ieee_is_finite
use module_phys_rslprof, only: RSL_cal_prms, RSLProfile, setup_RSL_heights
use module_ctrl_const_allocate, only: nsurf, ncolumnsDataOutRSL
use module_ctrl_type, only: SUEWS_CONFIG, SUEWS_TIMER, SUEWS_FORCING, &
   SUEWS_SITE, SUEWS_STATE
use module_ctrl_error_state, only: reset_supy_error, get_supy_error, supy_error_flag
implicit none

integer, parameter :: nz = 30, nz_can = 20, nz_above = nz - nz_can + 1
character(len=24) :: mode, target_field, input_kind
integer :: method, do_reset, code, status, flag_i, all_sentinel_i, all_finite_i
real(kind(1d0)) :: zh_in, fai_in, pai_in, l_mod_in, injected
real(kind(1d0)) :: z_full(nz), z_above(nz_above)
real(kind(1d0)) :: psihatm(nz_above), psihath(nz_above)
real(kind(1d0)) :: sfr_surf(nsurf)
real(kind(1d0)) :: zH_RSL, L_MOD_RSL, Lc, beta, zd_RSL, z0_RSL, elm, Scc, fx
real(kind(1d0)) :: dataoutLineRSL(ncolumnsDataOutRSL - 5)
type(SUEWS_CONFIG) :: config
type(SUEWS_TIMER) :: timer
type(SUEWS_FORCING) :: forcing
type(SUEWS_SITE) :: siteInfo
type(SUEWS_STATE) :: modState
character(len=512) :: message

sfr_surf = [0.4d0, 0.3d0, 0.05d0, 0.05d0, 0.1d0, 0.05d0, 0.05d0]
call setup_RSL_heights(nz, nz_can, 10.0d0, 40.0d0, z_full)
z_above = z_full(nz_can:nz)

do
  read(*, *, iostat=status) mode, target_field, input_kind, method, zh_in, fai_in, pai_in, l_mod_in, do_reset
  if (status /= 0) exit

  select case (trim(input_kind))
  case ('qnan')
    injected = ieee_value(0.0d0, ieee_quiet_nan)
  case ('+inf')
    injected = ieee_value(0.0d0, ieee_positive_inf)
  case ('-inf')
    injected = ieee_value(0.0d0, ieee_negative_inf)
  case default
    injected = l_mod_in
  end select

  select case (trim(target_field))
  case ('l_mod')
    l_mod_in = injected
  case ('zh')
    zh_in = injected
  case ('fai')
    fai_in = injected
  case ('pai')
    pai_in = injected
  end select

  if (do_reset /= 0) call reset_supy_error()

  select case (trim(mode))
  case ('prms')
    psihatm = 0.0d0
    psihath = 0.0d0
    call RSL_cal_prms( &
       method, nz_above, z_above, zh_in, 2.0d0, l_mod_in, sfr_surf, &
       fai_in, pai_in, 10000.0d0, 10.0d0, &
       psihatm, psihath, zH_RSL, L_MOD_RSL, Lc, beta, zd_RSL, z0_RSL, elm, Scc, fx)
    if (all(psihatm == -999.0d0) .and. all(psihath == -999.0d0) .and. &
        zH_RSL == -999.0d0 .and. L_MOD_RSL == -999.0d0 .and. Lc == -999.0d0 .and. &
        beta == -999.0d0 .and. zd_RSL == -999.0d0 .and. z0_RSL == -999.0d0 .and. &
        elm == -999.0d0 .and. Scc == -999.0d0 .and. fx == -999.0d0) then
      all_sentinel_i = 1
    else
      all_sentinel_i = 0
    end if
    if (all(ieee_is_finite(psihatm)) .and. all(ieee_is_finite(psihath)) .and. &
        ieee_is_finite(zH_RSL) .and. ieee_is_finite(L_MOD_RSL) .and. ieee_is_finite(Lc) .and. &
        ieee_is_finite(beta) .and. ieee_is_finite(zd_RSL) .and. ieee_is_finite(z0_RSL) .and. &
        ieee_is_finite(elm) .and. ieee_is_finite(Scc) .and. ieee_is_finite(fx)) then
      all_finite_i = 1
    else
      all_finite_i = 0
    end if
  case ('profile')
    config%RSLMethod = 1
    config%StabilityMethod = method
    config%Diagnose = 0
    config%SMDMethod = 0
    config%StorageHeatMethod = 1
    timer%tstep_real = 300.0d0
    forcing%Temp_C = 15.0d0
    forcing%U = 3.0d0
    forcing%RH = 60.0d0
    forcing%pres = 1013.25d0
    forcing%xsmd = 0.0d0
    siteInfo%z = 40.0d0
    siteInfo%z0m_in = 1.0d0
    siteInfo%zdm_in = 6.0d0
    siteInfo%h_std = 2.0d0
    siteInfo%n_buildings = 10.0d0
    siteInfo%SurfaceArea = 10000.0d0
    siteInfo%sfr_surf = sfr_surf
    modState%atmState%RA_h = 30.0d0
    modState%atmState%av_density = 1.2d0
    modState%atmState%av_cp = 1005.0d0
    modState%atmState%lv_J_kg = 2.45d6
    modState%atmState%L_MOD = l_mod_in
    modState%roughnessState%Zh = zh_in
    modState%roughnessState%z0m = 1.0d0
    modState%roughnessState%zdm = 6.0d0
    modState%roughnessState%z0v = 0.1d0
    modState%roughnessState%FAI = fai_in
    modState%roughnessState%PAI = pai_in
    modState%heatState%qh = 50.0d0
    modState%heatState%qe = 30.0d0
    dataoutLineRSL = 0.0d0
    call RSLProfile(timer, config, forcing, siteInfo, modState, dataoutLineRSL)
    L_MOD_RSL = dataoutLineRSL(121)
    Lc = dataoutLineRSL(123)
    beta = dataoutLineRSL(124)
    z0_RSL = dataoutLineRSL(126)
    if (all(dataoutLineRSL == -999.0d0)) then
      all_sentinel_i = 1
    else
      all_sentinel_i = 0
    end if
    if (all(ieee_is_finite(dataoutLineRSL))) then
      all_finite_i = 1
    else
      all_finite_i = 0
    end if
  case default
    stop 2
  end select

  flag_i = merge(1, 0, supy_error_flag())
  call get_supy_error(code, message)
  write(*, '(I0,1X,I0,1X,I0,1X,I0,1X,ES25.16E3,1X,ES25.16E3,1X,ES25.16E3,1X,ES25.16E3)') &
     flag_i, code, all_sentinel_i, all_finite_i, L_MOD_RSL, Lc, beta, z0_RSL
  write(*, '(A)') trim(message)
end do
end program
""",
        encoding="ascii",
    )
    executable = directory / "probe_rsl"
    compiled = subprocess.run(
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
            "-fopenmp",
            "-o",
            str(executable),
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    assert compiled.returncode == 0, compiled.stderr

    def run_sequence(rows):
        payload = (
            "\n".join(
                f"{mode} {field} {kind} {method} {zh:.17e} {fai:.17e} {pai:.17e} {l_mod:.17e} {int(reset)}"
                for mode, field, kind, method, zh, fai, pai, l_mod, reset in rows
            )
            + "\n"
        )
        result = subprocess.run(
            [str(executable)],
            input=payload,
            check=True,
            capture_output=True,
            text=True,
        )
        lines = result.stdout.splitlines()
        assert len(lines) == 2 * len(rows), result.stdout
        parsed = []
        for idx in range(len(rows)):
            parts = lines[2 * idx].split()
            parsed.append({
                "flag": int(parts[0]),
                "code": int(parts[1]),
                "all_sentinel": bool(int(parts[2])),
                "all_finite": bool(int(parts[3])),
                "L_MOD_RSL": float(parts[4]),
                "Lc": float(parts[5]),
                "beta": float(parts[6]),
                "z0_RSL": float(parts[7]),
                "message": lines[2 * idx + 1].strip(),
            })
        return parsed

    def run_one(
        mode="prms",
        field="none",
        kind="finite",
        method=3,
        zh=10.0,
        fai=0.2,
        pai=0.3,
        l_mod=-50.0,
        reset=True,
    ):
        return run_sequence([(mode, field, kind, method, zh, fai, pai, l_mod, reset)])[
            0
        ]

    run_one.sequence = run_sequence
    return run_one


@pytest.mark.parametrize("method", [3, 4, 5])
@pytest.mark.parametrize("input_kind", NONFINITE_KINDS)
def test_rsl_cal_prms_rejects_non_finite_l_mod(rsl_nonfinite_probe, method, input_kind):
    """Non-finite L_MOD must raise fatal code 114 instead of clamping lc_over_L to -2."""
    res = rsl_nonfinite_probe(
        mode="prms", field="l_mod", kind=input_kind, method=method
    )
    assert res["flag"] == 1
    assert res["code"] == 114
    assert res["message"] == "RSL_cal_prms: non-finite Obukhov length L_MOD"
    assert res["all_sentinel"] is True


@pytest.mark.parametrize("field", ["zh", "fai", "pai"])
@pytest.mark.parametrize("input_kind", NONFINITE_KINDS)
def test_rsl_cal_prms_rejects_non_finite_canopy_geometry(
    rsl_nonfinite_probe, field, input_kind
):
    """Non-finite zh, FAI, or PAI must raise code 114 before MAX/MERGE can hide NaN."""
    res = rsl_nonfinite_probe(mode="prms", field=field, kind=input_kind, method=3)
    assert res["flag"] == 1
    assert res["code"] == 114
    assert (
        res["message"]
        == "RSL_cal_prms: non-finite canopy geometry input (zh, FAI, or PAI)"
    )
    assert res["all_sentinel"] is True


@pytest.mark.parametrize(
    "zh,fai,pai,l_mod,expected_msg",
    [
        (
            10.0,
            0.2,
            0.0,
            -50.0,
            "RSL_cal_prms: non-finite canopy drag length scale Lc_min",
        ),
        (
            10.0,
            0.2,
            -0.1,
            -50.0,
            "RSL_cal_prms: non-finite canopy drag length scale Lc_min",
        ),
        (
            1.0e305,
            1.0e-6,
            0.3,
            -50.0,
            "RSL_cal_prms: non-finite canopy drag length scale Lc",
        ),
        (
            0.4,
            0.3,
            1.0e308,
            -50.0,
            "RSL_cal_prms: non-finite canopy drag length scale Lc",
        ),
        (
            1.0e305,
            1.0e-5,
            1.0,
            -50.0,
            "RSL_cal_prms: non-finite canopy drag length scale Lc_min",
        ),
        (
            1.0e300,
            1.0e-5,
            0.3,
            2.0e-6,
            "RSL_cal_prms: non-finite stability ratio lc_over_L = Lc/L_MOD",
        ),
        (
            1.0e300,
            1.0e-5,
            0.3,
            -2.0e-6,
            "RSL_cal_prms: non-finite stability ratio lc_over_L = Lc/L_MOD",
        ),
        (
            0.4,
            1.0e20,
            0.5,
            1.0e308,
            "RSL_cal_prms: non-finite clamped Obukhov length L_MOD_RSL",
        ),
    ],
)
def test_rsl_cal_prms_rejects_derived_non_finite_scales(
    rsl_nonfinite_probe, zh, fai, pai, l_mod, expected_msg
):
    """Finite inputs yielding non-finite Lc, lc_over_L, or L_MOD_RSL raise fatal code 114."""
    res = rsl_nonfinite_probe(
        mode="prms",
        field="none",
        kind="finite",
        method=3,
        zh=zh,
        fai=fai,
        pai=pai,
        l_mod=l_mod,
    )
    assert res["flag"] == 1
    assert res["code"] == 114
    assert res["message"] == expected_msg
    assert res["all_sentinel"] is True


@pytest.mark.parametrize(
    "l_mod,expected_l_mod_rsl",
    [
        (1.0e-9, 17.5),  # |L_MOD| < 1e-6 -> lc_over_L = +2 -> Lc / 2 = 35 / 2
        (-1.0e-9, -17.5),  # |L_MOD| < 1e-6 -> lc_over_L = -2 -> Lc / (-2) = -17.5
        (0.0, 17.5),  # exact +0.0 -> SIGN(2.0, +0.0) = +2 -> +17.5
        (2.0, 17.5),  # Lc / L_MOD = 17.5 clamped to +2 -> +17.5
        (-2.0, -17.5),  # Lc / L_MOD = -17.5 clamped to -2 -> -17.5
        (50.0, 50.0),  # |Lc / L_MOD| = 0.7 <= 2 -> unchanged
        (-50.0, -50.0),  # |Lc / L_MOD| = 0.7 <= 2 -> unchanged
        (1.0e12, 1.0e12),  # near-neutral positive finite
        (-1.0e12, -1.0e12),  # near-neutral negative finite
    ],
)
def test_rsl_cal_prms_preserves_finite_controls(
    rsl_nonfinite_probe, l_mod, expected_l_mod_rsl
):
    """Valid finite L_MOD inputs (including |L_MOD| < 1e-6) keep exact expected clamping."""
    res = rsl_nonfinite_probe(
        mode="prms",
        field="none",
        kind="finite",
        method=3,
        zh=10.0,
        fai=0.2,
        pai=0.3,
        l_mod=l_mod,
    )
    assert res["flag"] == 0
    assert res["code"] == 0
    assert not res["message"]
    assert res["all_finite"] is True
    assert res["Lc"] == pytest.approx(35.0, rel=1e-12, abs=0.0)
    assert res["L_MOD_RSL"] == pytest.approx(expected_l_mod_rsl, rel=1e-12, abs=0.0)
    assert 0.0 < res["beta"] <= 0.5
    assert res["z0_RSL"] >= 1.0e-2


@pytest.mark.parametrize("input_kind", NONFINITE_KINDS)
def test_rsl_profile_propagates_code_114_without_overwrite(
    rsl_nonfinite_probe, input_kind
):
    """RSLProfile returns immediately on RSL_cal_prms fatal error without calling interp_z."""
    res = rsl_nonfinite_probe(mode="profile", field="l_mod", kind=input_kind, method=3)
    assert res["flag"] == 1
    assert res["code"] == 114
    assert res["message"] == "RSL_cal_prms: non-finite Obukhov length L_MOD"
    assert res["all_sentinel"] is True


def test_rsl_profile_preserves_w16_method_error_108(rsl_nonfinite_probe):
    """W16 (StabilityMethod=2) with finite L_MOD still surfaces error code 108."""
    res = rsl_nonfinite_probe(mode="profile", field="none", kind="finite", method=2)
    assert res["flag"] == 1
    assert res["code"] == 108
    assert "has no phi implementation for RSL" in res["message"]
    assert res["all_sentinel"] is True


def test_rsl_cal_prms_same_thread_tls_recovery(rsl_nonfinite_probe):
    """Fatal code 114 is sticky until TLS reset, then the same thread recovers cleanly."""
    clean, failed, sticky_valid, recovered = rsl_nonfinite_probe.sequence([
        ("prms", "none", "finite", 3, 10.0, 0.2, 0.3, -50.0, True),
        ("prms", "l_mod", "qnan", 3, 10.0, 0.2, 0.3, -50.0, True),
        ("prms", "none", "finite", 3, 10.0, 0.2, 0.3, -50.0, False),
        ("prms", "none", "finite", 3, 10.0, 0.2, 0.3, -50.0, True),
    ])

    assert clean["flag"] == 0 and clean["code"] == 0 and clean["all_finite"] is True

    assert failed["flag"] == 1 and failed["code"] == 114
    assert failed["message"] == "RSL_cal_prms: non-finite Obukhov length L_MOD"
    assert failed["all_sentinel"] is True

    assert sticky_valid["flag"] == 1 and sticky_valid["code"] == 114
    assert sticky_valid["message"] == "RSL_cal_prms: non-finite Obukhov length L_MOD"
    assert sticky_valid["all_sentinel"] is True

    assert recovered["flag"] == 0 and recovered["code"] == 0
    assert recovered["all_finite"] is True
    assert recovered["L_MOD_RSL"] == pytest.approx(clean["L_MOD_RSL"], rel=0.0, abs=0.0)
    assert recovered["beta"] == pytest.approx(clean["beta"], rel=0.0, abs=0.0)
    assert recovered["z0_RSL"] == pytest.approx(clean["z0_RSL"], rel=0.0, abs=0.0)


def test_bridge_forced_rsl_zero_pai_raises_code_114_and_recovers(
    sample_config_loaded, sample_data_loaded
):
    """Forced RSL with PAI=0 (non-finite Lc) raises RuntimeError code 114 and recovers."""
    run_rust = import_module("supy._run_rust")
    rust = run_rust._check_rust_available()
    _, forcing = sample_data_loaded
    forcing_short = forcing.iloc[:2]
    forcing_flat = (
        run_rust._prepare_forcing_block(forcing_short).ravel(order="C").tolist()
    )
    n_steps = len(forcing_short)

    valid_cfg = sample_config_loaded.model_dump(exclude_none=True, mode="json")
    baseline = rust.run_suews_multi([json.dumps(valid_cfg)], forcing_flat, n_steps, 1)

    zero_pai_cfg = copy.deepcopy(valid_cfg)
    zero_pai_cfg["model"]["physics"]["roughness_sublayer"] = {"value": 1}
    lc = zero_pai_cfg["sites"][0]["properties"]["land_cover"]
    lc["paved"]["sfr"] = {"value": 1.0}
    for key in ("bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
        lc[key]["sfr"] = {"value": 0.0}

    with pytest.raises(
        RuntimeError,
        match=r"code 114.*RSL_cal_prms: non-finite canopy drag length scale Lc_min",
    ):
        rust.run_suews_multi([json.dumps(zero_pai_cfg)], forcing_flat, n_steps, 1)

    recovered = rust.run_suews_multi([json.dumps(valid_cfg)], forcing_flat, n_steps, 1)
    assert recovered == baseline


def _forced_rsl_zero_pai(config):
    """Force RSL on a 100 % paved grid, so PAI = 0 and Lc is non-finite."""
    config.model.physics.roughness_sublayer = 1
    land_cover = config.sites[0].properties.land_cover
    for key in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
        getattr(land_cover, key).sfr = 1.0 if key == "paved" else 0.0
    return config


@pytest.mark.parametrize("n_jobs", [1, -1], ids=["serial", "default-jobs"])
def test_simulation_run_forced_rsl_zero_pai_raises_code_114_and_recovers(
    sample_config_loaded, sample_data_loaded, n_jobs
):
    """``SUEWSSimulation.run()`` surfaces code 114 and a following valid run succeeds."""
    _, forcing = sample_data_loaded
    forcing_short = forcing.iloc[:2].copy()
    run_options = {
        "start_date": forcing_short.index[0],
        "end_date": forcing_short.index[-1],
        "n_jobs": n_jobs,
    }

    def make_sim(config):
        sim = sp.SUEWSSimulation()
        sim.update_config(config, auto_load_forcing=False)
        sim.update_forcing(forcing_short)
        return sim

    baseline = (
        make_sim(sample_config_loaded.model_copy(deep=True)).run(**run_options).df
    )

    zero_pai = make_sim(
        _forced_rsl_zero_pai(sample_config_loaded.model_copy(deep=True))
    )
    with pytest.raises(
        RuntimeError,
        match=r"code 114.*RSL_cal_prms: non-finite canopy drag length scale Lc_min",
    ):
        zero_pai.run(**run_options)

    recovered = (
        make_sim(sample_config_loaded.model_copy(deep=True)).run(**run_options).df
    )
    pd.testing.assert_frame_equal(recovered, baseline)


def test_suews_run_cli_forced_rsl_zero_pai_fails_with_code_114(
    cli_runner, tmp_path, monkeypatch
):
    """``suews-run`` exits non-zero with code 114 and writes no output files."""
    benchmark_dir = Path(__file__).resolve().parents[1] / "fixtures" / "benchmark1"
    forcing_name = "Kc1_2011_data_5_short.txt"
    (tmp_path / "forcing").mkdir()
    shutil.copyfile(
        benchmark_dir / "forcing" / forcing_name, tmp_path / "forcing" / forcing_name
    )
    config = yaml.safe_load(
        (benchmark_dir / "benchmark1_short.yml").read_text(encoding="utf-8")
    )
    config["model"]["physics"]["roughness_sublayer"] = {"value": 1}
    land_cover = config["sites"][0]["properties"]["land_cover"]
    for key in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
        land_cover[key]["sfr"] = {"value": 1.0 if key == "paved" else 0.0}
    path_config = tmp_path / "forced_rsl_zero_pai.yml"
    path_config.write_text(yaml.safe_dump(config, sort_keys=False), encoding="utf-8")
    monkeypatch.chdir(tmp_path)

    result = run_cli_command(cli_runner, SUEWS, [str(path_config)], check=False)

    combined = result.stdout + result.stderr
    assert result.returncode != 0, combined
    assert "code 114" in combined, combined
    assert "RSL_cal_prms: non-finite canopy drag length scale Lc_min" in combined, (
        combined
    )
    assert "successfully done" not in combined
    assert not list(tmp_path.glob("*_SUEWS_output.*"))


def _locate_suews_engine() -> Path:
    """Return the bundled or locally built ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(bundled) as bundled_path:
        if bundled_path.exists():
            return Path(bundled_path)
    repo_root = Path(__file__).resolve().parents[2]
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.exists():
        return dev_binary
    pytest.skip("Rust CLI binary not found; build with: make bridge")


def _run_suews_engine(run_dir: Path, mutate_config=None, mutate_resolved=None):
    """Run ``suews-engine run`` on the sample case cut to four forcing rows.

    ``mutate_config`` edits the YAML mapping before ``SUEWSConfig`` validates
    it; ``mutate_resolved`` edits the resolved engine YAML afterwards, for
    values the data model would refuse.
    """
    sample_dir = files("supy").joinpath("sample_data")
    cfg = yaml.safe_load(
        sample_dir.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )
    control = cfg["model"]["control"]
    control["output"]["dir"] = str(run_dir)
    forcing_entry = control["forcing"]["file"]
    forcing_name = (
        forcing_entry["value"] if isinstance(forcing_entry, dict) else forcing_entry
    )
    if mutate_config is not None:
        mutate_config(cfg)
    run_rust = import_module("supy._run_rust")
    resolved = yaml.safe_load(
        run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(cfg))
    )
    if mutate_resolved is not None:
        mutate_resolved(resolved)
    forcing_lines = [
        line
        for line in sample_dir
        .joinpath(forcing_name)
        .read_text(encoding="utf-8")
        .splitlines()
        if line.strip()
    ]
    (run_dir / forcing_name).write_text(
        "\n".join(forcing_lines[:5]) + "\n", encoding="utf-8"
    )
    config_path = run_dir / "engine_config.yml"
    config_path.write_text(yaml.safe_dump(resolved, sort_keys=False), encoding="utf-8")
    return subprocess.run(
        [str(_locate_suews_engine()), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )


def test_suews_engine_cli_forced_rsl_zero_pai_fails_with_code_114(tmp_path):
    """Standalone ``suews-engine run`` exits non-zero with code 114 and no output."""

    def forced_rsl_zero_pai(cfg):
        cfg["model"]["physics"]["roughness_sublayer"] = {"value": 1}
        land_cover = cfg["sites"][0]["properties"]["land_cover"]
        for key in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            land_cover[key]["sfr"] = {"value": 1.0 if key == "paved" else 0.0}

    result = _run_suews_engine(tmp_path, mutate_config=forced_rsl_zero_pai)

    assert result.returncode != 0, result.stderr
    assert "114" in result.stderr, result.stderr
    assert "RSL_cal_prms: non-finite canopy drag length scale Lc_min" in result.stderr
    assert not list(tmp_path.glob("*.arrow"))
