"""Regression tests for exact exponential weights and step-1 guard in ``OHM_dqndt_cal_X``.

Verifies the GH#2194 ``c2_exp_cascaded_1h_2h`` formulation:

- Cold-start step-1 ``dqndt`` equals zero across ``300 s`` and ``3600 s``
  timesteps and across ``OHM``, ``AnOHM``, ``DyOHM`` (methods 6, 7, 8), and
  ``Snow`` schemes.
- Hourly-aggregated ``5 min`` (``300 s``) and native ``60 min`` (``3600 s``)
  ``dqndt`` and ``QS`` remain consistent under smooth diurnal forcing (unlike
  linear weights, which collapse the 1-hour low-pass filter at ``dt = 3600 s``).
- Coupled surface energy balance closes to double-precision round-off across
  all exercised schemes and timesteps.
"""

from __future__ import annotations

import copy
import ctypes
from functools import lru_cache
from importlib import import_module
from importlib.resources import as_file, files
import json
import math
import os
from pathlib import Path
import shutil
import subprocess

import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy.data_model import SUEWSConfig
from supy.data_model.core.model import SnowUse, StorageHeatMethod
from supy.util import read_forcing

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-RESPONSE", "C-BALANCE")]

HOURS_PER_DAY = 24
SECONDS_PER_HOUR = 3600
OHM_TAU1_MAX_S = 0.5 * float(SECONDS_PER_HOUR)
OHM_TAU2_S = 1.0 * float(SECONDS_PER_HOUR)
TSTEP_5MIN = 300
TSTEP_60MIN = 3600
ENERGY_CLOSURE_TOLERANCE_W_M2 = 1.0e-12
COLD_START_ZERO_TOLERANCE = 1.0e-15

STEBBS_CONFIG_PATH = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "data_test"
    / "stebbs_test"
    / "sample_config.yml"
)


def _c2_exp_cascaded_recurrence(
    qn_series: np.ndarray,
    tstep: int,
) -> tuple[np.ndarray, np.ndarray]:
    """Evaluate ``c2_exp_cascaded_1h_2h`` once per timestep from cold start."""
    dt_s = float(tstep)
    dt_hr = dt_s / float(SECONDS_PER_HOUR)
    w2 = math.exp(-dt_s / OHM_TAU2_S)

    n_steps = len(qn_series)
    qn_av_out = np.empty(n_steps, dtype=float)
    dqndt_out = np.empty(n_steps, dtype=float)
    qn_av_prev, dqndt_prev = 0.0, 0.0

    for step_idx, qn_val in enumerate(qn_series):
        dt_since_start = (step_idx + 1) * tstep
        qn1 = float(qn_val)
        if dt_since_start <= tstep:
            qn_av_next, dqndt_next = qn1, 0.0
        else:
            w1 = math.exp(-dt_s / min(OHM_TAU1_MAX_S, float(dt_since_start)))
            qn_av_next = w1 * qn_av_prev + (1.0 - w1) * qn1
            dqndt_next = w2 * dqndt_prev + (1.0 - w2) * (
                (qn_av_next - qn_av_prev) / dt_hr
            )
        qn_av_out[step_idx] = qn_av_next
        dqndt_out[step_idx] = dqndt_next
        qn_av_prev, dqndt_prev = qn_av_next, dqndt_next

    return qn_av_out, dqndt_out


def _decode_ohm_checkpoint(sim: sp.SUEWSSimulation) -> dict[str, float]:
    """Decode the Rust checkpoint ``OHM_STATE`` payload into a dictionary."""
    checkpoint = sim.checkpoint
    assert checkpoint is not None
    state_json = next(iter(checkpoint.grid_states.values()))
    checkpoint_payload = json.loads(state_json)
    ohm_payload = checkpoint_payload["state"]["members"]["ohm_state"]
    bridge = import_module("supy.suews_bridge")
    state = bridge.OhmState.from_values_payload(
        ohm_payload["schema_version"], ohm_payload["values"]
    )
    return state.to_dict()


def _energy_balance_max_residual(suews_df: pd.DataFrame, snow: bool = False) -> float:
    """Return maximum absolute surface energy balance residual in W m-2."""
    if snow:
        residual = (
            suews_df["QN"]
            + suews_df["QF"]
            + suews_df["QMRain"]
            - (
                suews_df["QH"]
                + suews_df["QE"]
                + suews_df["QS"]
                + suews_df["QM"]
                + suews_df["QMFreeze"]
            )
        )
    else:
        residual = (
            suews_df["QN"]
            + suews_df["QF"]
            - (suews_df["QH"] + suews_df["QE"] + suews_df["QS"])
        )
    return float(np.max(np.abs(residual.to_numpy(dtype=float))))


@lru_cache(maxsize=4)
def _load_sample_forcing_full(tstep: int) -> pd.DataFrame:
    """Cache full bundled 2012 sample forcing resampled to ``tstep`` seconds."""
    with as_file(files("supy") / "sample_data") as sample_dir:
        return read_forcing(str(sample_dir / "Kc_2012_data_60.txt"), tstep_mod=tstep)


def _load_sample_forcing(tstep: int, n_hours: int) -> pd.DataFrame:
    """Load bundled 2012 sample forcing resampled to ``tstep`` seconds."""
    n_steps = n_hours * (SECONDS_PER_HOUR // tstep)
    return _load_sample_forcing_full(tstep).iloc[:n_steps].copy()


def _make_smooth_diurnal_forcing(
    template_df: pd.DataFrame,
    tstep: int,
    n_days: int,
) -> pd.DataFrame:
    """Construct smooth, identical diurnal forcing at ``tstep`` seconds."""
    steps_per_day = (HOURS_PER_DAY * SECONDS_PER_HOUR) // tstep
    n_steps = n_days * steps_per_day
    df = template_df.iloc[:n_steps].copy()

    # Evaluate the analytical forcing at each interval midpoint so
    # 5-minute and 60-minute forcing share the same diurnal cycle.
    hours_from_start = (np.arange(n_steps, dtype=float) + 0.5) * (
        float(tstep) / float(SECONDS_PER_HOUR)
    )
    hour_of_day = np.mod(hours_from_start, float(HOURS_PER_DAY))
    solar_angle = np.pi * (hour_of_day - 6.0) / 12.0
    kdown = np.where(
        (hour_of_day > 6.0) & (hour_of_day < 18.0),
        700.0 * np.sin(solar_angle),
        0.0,
    )
    tair = 18.0 + 6.0 * np.sin(2.0 * np.pi * (hour_of_day - 9.0) / 24.0)

    df["kdown"] = kdown
    df["Tair"] = tair
    df["RH"] = 55.0
    df["U"] = 3.5
    df["pres"] = 1013.0
    df["rain"] = 0.0
    df["ldown"] = 320.0
    return df


def _run_config(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    end_date: pd.Timestamp | None = None,
) -> tuple[sp.SUEWSSimulation, pd.DataFrame]:
    """Run ``config`` on ``df_forcing`` with a single worker."""
    sim = sp.SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    output = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1] if end_date is None else end_date,
        n_jobs=1,
    )
    return sim, output.df


def _hourly_5m_vs_60m_metrics(
    out_300: pd.DataFrame,
    out_3600: pd.DataFrame,
) -> dict[str, float]:
    """Compute post-spin-up hourly ``5 min`` vs ``60 min`` consistency metrics."""
    frame_300 = pd.DataFrame({
        "dqndt": out_300.loc[1, ("debug", "dqndt")],
        "QS": out_300.loc[1, ("SUEWS", "QS")],
    })
    grp = frame_300.resample("1h", closed="right", label="right")
    hourly_300 = grp.mean().where(grp.count() == 12)
    frame_3600 = pd.DataFrame({
        "dqndt": out_3600.loc[1, ("debug", "dqndt")],
        "QS": out_3600.loc[1, ("SUEWS", "QS")],
    })

    spinup_end = frame_3600.index[0] + pd.Timedelta(hours=HOURS_PER_DAY)
    common = hourly_300.loc[spinup_end:].index.intersection(
        frame_3600.loc[spinup_end:].index
    )
    assert len(common) >= HOURS_PER_DAY, "Insufficient aligned post-spinup hours"

    dq_5m = hourly_300.loc[common, "dqndt"].to_numpy(dtype=float)
    dq_60m = frame_3600.loc[common, "dqndt"].to_numpy(dtype=float)
    qs_5m = hourly_300.loc[common, "QS"].to_numpy(dtype=float)
    qs_60m = frame_3600.loc[common, "QS"].to_numpy(dtype=float)

    return {
        "rmse_dqndt": float(np.sqrt(np.mean((dq_60m - dq_5m) ** 2))),
        "corr_dqndt": float(np.corrcoef(dq_5m, dq_60m)[0, 1]),
        "std_ratio": float(np.std(dq_60m) / np.std(dq_5m)),
        "rmse_qs": float(np.sqrt(np.mean((qs_60m - qs_5m) ** 2))),
    }


@pytest.mark.core
@pytest.mark.parametrize(
    ("case_id", "storage_method", "tstep", "snow_use", "use_stebbs"),
    [
        ("ohm_300s", 1, TSTEP_5MIN, 0, False),
        ("ohm_3600s", 1, TSTEP_60MIN, 0, False),
        ("anohm_300s", 3, TSTEP_5MIN, 0, False),
        ("anohm_3600s", 3, TSTEP_60MIN, 0, False),
        ("dyohm_all_300s", 6, TSTEP_5MIN, 0, False),
        ("dyohm_stebbs_300s", 7, TSTEP_5MIN, 0, True),
        ("dyohm_bldg_300s", 8, TSTEP_5MIN, 0, False),
        ("ohm_snow_300s", 1, TSTEP_5MIN, 1, False),
        ("ohm_snow_3600s", 1, TSTEP_60MIN, 1, False),
    ],
)
def test_cold_start_step1_dqndt_zero_and_energy_closure(
    case_id: str,
    storage_method: int,
    tstep: int,
    snow_use: int,
    use_stebbs: bool,
):
    """Step-1 cold start must yield zero ``dqndt`` and machine-precision energy closure."""
    n_hours = 6
    if use_stebbs:
        sim_template = sp.SUEWSSimulation(str(STEBBS_CONFIG_PATH))
        cfg = copy.deepcopy(sim_template.config)
        cfg.model.control.output.groups = ["SUEWS", "STEBBS", "debug"]
        df_forcing = sim_template.forcing.df.iloc[
            : n_hours * (SECONDS_PER_HOUR // tstep)
        ].copy()
        df_forcing["rain"] = df_forcing["rain"].clip(lower=0.0)
    else:
        sim_template = sp.SUEWSSimulation.from_sample_data()
        cfg = copy.deepcopy(sim_template.config)
        df_forcing = _load_sample_forcing(tstep=tstep, n_hours=n_hours)

    cfg.model.physics.storage_heat = StorageHeatMethod(storage_method)
    cfg.model.physics.snow_use = SnowUse(snow_use)
    cfg.model.control.tstep = tstep
    cfg.model.control.output.freq = tstep

    if snow_use == 1:
        df_forcing["Tair"] = -5.0
        df_forcing["rain"] = (
            0.5 if tstep == TSTEP_60MIN else 0.5 / (SECONDS_PER_HOUR // TSTEP_5MIN)
        )
        df_forcing["RH"] = 85.0
        df_forcing["U"] = 3.0

    # Verify exact step-1 checkpoint state as well as multi-step evolution.
    sim_step1, df_step1 = _run_config(cfg, df_forcing, end_date=df_forcing.index[0])
    dqndt_step1 = float(df_step1.loc[1, ("debug", "dqndt")].iloc[0])
    qn_step1 = float(df_step1.loc[1, ("SUEWS", "QN")].iloc[0])
    assert abs(dqndt_step1) <= COLD_START_ZERO_TOLERANCE, (
        f"{case_id}: step-1 cold-start dqndt={dqndt_step1:.6e} W m-2 h-1 (expected 0.0)"
    )

    ohm_step1 = _decode_ohm_checkpoint(sim_step1)
    assert abs(ohm_step1["dqndt"]) <= COLD_START_ZERO_TOLERANCE, (
        f"{case_id}: checkpoint step-1 dqndt={ohm_step1['dqndt']:.6e}"
    )
    assert abs(ohm_step1["dqnsdt"]) <= COLD_START_ZERO_TOLERANCE, (
        f"{case_id}: checkpoint step-1 dqnsdt={ohm_step1['dqnsdt']:.6e}"
    )
    for surface in ("paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water"):
        val = ohm_step1[f"dqndt_surf.{surface}"]
        assert abs(val) <= COLD_START_ZERO_TOLERANCE, (
            f"{case_id}: checkpoint step-1 dqndt_surf.{surface}={val:.6e}"
        )
    if storage_method in {1, 3, 8}:
        assert abs(qn_step1) > 1.0, (
            f"{case_id}: non-vacuous step-1 QN check failed (QN={qn_step1:.6e})"
        )
        assert ohm_step1["qn_av"] == pytest.approx(qn_step1, rel=1e-12, abs=1e-12), (
            f"{case_id}: step-1 qn_av={ohm_step1['qn_av']:.6e} != QN={qn_step1:.6e}"
        )
    else:
        assert any(
            abs(ohm_step1[f"qn_surfs.{surface}"]) > 1.0
            for surface in (
                "paved",
                "bldg",
                "evetr",
                "dectr",
                "grass",
                "bsoil",
                "water",
            )
        ), f"{case_id}: step-1 DyOHM qn_surfs were not seeded from step-1 QN"

    # Multi-step check: finite fluxes, zero step-1 spike, and tight energy closure.
    _, df_multi = _run_config(cfg, df_forcing)
    suews = df_multi.loc[1, "SUEWS"]
    dqndt = df_multi.loc[1, ("debug", "dqndt")].to_numpy(dtype=float)
    assert abs(float(dqndt[0])) <= COLD_START_ZERO_TOLERANCE
    assert np.isfinite(
        suews[["QN", "QH", "QE", "QS", "Ts"]].to_numpy(dtype=float)
    ).all()
    assert np.isfinite(dqndt).all()

    eb_res = _energy_balance_max_residual(suews, snow=bool(snow_use))
    assert eb_res <= ENERGY_CLOSURE_TOLERANCE_W_M2, (
        f"{case_id}: energy balance residual {eb_res:.3e} W m-2 exceeds tolerance"
    )


@pytest.mark.core
def test_5min_vs_60min_dqndt_consistency_under_smooth_diurnal_forcing():
    """Exact exponential weights must keep 5-min and 60-min ``dqndt`` and ``QS`` consistent."""
    n_days = 3
    base_cfg = copy.deepcopy(sp.SUEWSSimulation.from_sample_data().config)
    base_cfg.model.physics.storage_heat = StorageHeatMethod.OHM_WITHOUT_QF
    base_cfg.model.physics.ohm_inc_qf = 0
    base_cfg.model.physics.net_radiation = 1

    runs: dict[int, pd.DataFrame] = {}
    for tstep in (TSTEP_5MIN, TSTEP_60MIN):
        df_template = _load_sample_forcing(tstep=tstep, n_hours=n_days * HOURS_PER_DAY)
        df_forcing = _make_smooth_diurnal_forcing(df_template, tstep, n_days)
        cfg = copy.deepcopy(base_cfg)
        cfg.model.control.tstep = tstep
        cfg.model.control.output.freq = tstep
        _, runs[tstep] = _run_config(cfg, df_forcing)

    metrics = _hourly_5m_vs_60m_metrics(runs[TSTEP_5MIN], runs[TSTEP_60MIN])

    # Linear weights fail sharply here (RMSE(dqndt) ~ 25.6 W m-2 h-1, std ratio > 1.2).
    assert metrics["rmse_dqndt"] < 10.0, (
        f"5-min vs 60-min dqndt RMSE={metrics['rmse_dqndt']:.2f} W m-2 h-1 exceeds 10.0"
    )
    assert metrics["corr_dqndt"] > 0.985, (
        f"5-min vs 60-min dqndt correlation r={metrics['corr_dqndt']:.4f} below 0.985"
    )
    assert 0.90 <= metrics["std_ratio"] <= 1.10, (
        f"60-min/5-min dqndt std ratio={metrics['std_ratio']:.3f} outside [0.90, 1.10]"
    )
    assert metrics["rmse_qs"] < 6.0, (
        f"5-min vs 60-min QS RMSE={metrics['rmse_qs']:.2f} W m-2 exceeds 6.0"
    )

    for tstep in (TSTEP_5MIN, TSTEP_60MIN):
        suews_df = runs[tstep].loc[1, "SUEWS"]
        dqndt_obs = runs[tstep].loc[1, ("debug", "dqndt")].to_numpy(dtype=float)
        _, dqndt_exp = _c2_exp_cascaded_recurrence(
            suews_df["QN"].to_numpy(dtype=float), tstep
        )
        np.testing.assert_allclose(
            dqndt_obs,
            dqndt_exp,
            rtol=1.0e-10,
            atol=1.0e-9,
            err_msg=f"{tstep} s dqndt diverges from exact moment-matched 0.5 h + 1.0 h recurrence",
        )
        eb_res = _energy_balance_max_residual(suews_df, snow=False)
        assert eb_res <= ENERGY_CLOSURE_TOLERANCE_W_M2, (
            f"{tstep} s: energy balance residual {eb_res:.3e} W m-2 exceeds tolerance"
        )


@pytest.mark.core
def test_moment_matched_diurnal_phase_lag_matches_2016_centroid():
    """Cascaded 0.5 h + 1.0 h weights must match the ~1.5 h lag of 2016 OHM, not ~2.8 h."""
    n_days = 4
    df_template = _load_sample_forcing(tstep=TSTEP_5MIN, n_hours=n_days * HOURS_PER_DAY)
    df_forcing = _make_smooth_diurnal_forcing(df_template, TSTEP_5MIN, n_days)
    cfg = copy.deepcopy(sp.SUEWSSimulation.from_sample_data().config)
    cfg.model.control.tstep = TSTEP_5MIN
    cfg.model.control.output.freq = TSTEP_5MIN
    cfg.model.physics.storage_heat = StorageHeatMethod.OHM_WITHOUT_QF
    cfg.model.physics.ohm_inc_qf = 0
    cfg.model.physics.net_radiation = 1

    _, df_out = _run_config(cfg, df_forcing)
    # Inspect the last full day (post-spinup) to locate the Q* peak and dQ*/dt zero-crossing.
    last_day = df_out.loc[1].iloc[-288:]
    qn = last_day["SUEWS", "QN"].to_numpy(dtype=float)
    dq = last_day["debug", "dqndt"].to_numpy(dtype=float)
    hrs = np.arange(len(qn), dtype=float) * (TSTEP_5MIN / float(SECONDS_PER_HOUR))
    peak_hr = float(hrs[int(np.argmax(qn))])
    zc_hr = float("nan")
    for i in range(len(hrs) - 1):
        if 10.0 <= hrs[i] <= 17.0 and dq[i] >= 0.0 and dq[i + 1] <= 0.0 and dq[i] != dq[i + 1]:
            zc_hr = float(hrs[i] + (0.0 - dq[i]) * (hrs[i + 1] - hrs[i]) / (dq[i + 1] - dq[i]))
            break
    lag_hr = zc_hr - peak_hr
    assert 1.0 <= lag_hr <= 1.85, (
        f"Expected diurnal phase lag near 1.5 h (moment-matched to 2016 OHM), got {lag_hr:.2f} h "
        f"(Q* peak {peak_hr:.2f} h, dQ*/dt zero-crossing {zc_hr:.2f} h)"
    )


def _compile_ohm_dqndt_cal_probe(tmp_path: Path):
    """Compile a minimal ``bind(C)`` wrapper around ``module_phys_ohm::OHM_dqndt_cal``."""
    compiler = shutil.which("gfortran")
    suews_dir = Path(__file__).resolve().parents[2] / "src" / "suews"
    if compiler is None or not (suews_dir / "lib" / "libsuewsphys.a").exists():
        pytest.skip(
            "Native OHM_dqndt_cal check requires gfortran and source-build libraries"
        )

    wrapper = tmp_path / "ohm_dqndt_cal_probe.f90"
    wrapper.write_text(
        """subroutine call_ohm_dqndt_cal(nsh, qn, qn_store, qn_av_store, dqndt) bind(C)
  use iso_c_binding, only: c_int, c_double
  use module_phys_ohm, only: OHM_dqndt_cal
  implicit none
  integer(c_int), value :: nsh
  real(c_double), value :: qn
  real(c_double), intent(inout) :: qn_store(nsh)
  real(c_double), intent(inout) :: qn_av_store(2 * nsh + 1)
  real(c_double), intent(out) :: dqndt
  call OHM_dqndt_cal(nsh, qn, qn_store, qn_av_store, dqndt)
end subroutine call_ohm_dqndt_cal
""",
        encoding="ascii",
    )
    lib_path = tmp_path / (
        "ohm_dqndt_cal_probe.dll" if os.name == "nt" else "ohm_dqndt_cal_probe.so"
    )
    result = subprocess.run(
        [
            compiler,
            "-shared",
            "-fPIC",
            str(wrapper),
            "-I",
            str(suews_dir / "mod"),
            "-L",
            str(suews_dir / "lib"),
            "-lsuewsphys",
            "-lsuewsutil",
            "-lspartacus",
            "-o",
            str(lib_path),
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, (
        f"Failed to compile OHM_dqndt_cal probe:\n{result.stderr}"
    )
    if os.name == "nt":
        with os.add_dll_directory(str(Path(compiler).resolve().parent)):
            lib = ctypes.CDLL(str(lib_path))
    else:
        lib = ctypes.CDLL(str(lib_path))
    arr_arg = np.ctypeslib.ndpointer(dtype=np.float64, flags="C_CONTIGUOUS")
    fn = lib.call_ohm_dqndt_cal
    fn.argtypes = [
        ctypes.c_int,
        ctypes.c_double,
        arr_arg,
        arr_arg,
        ctypes.POINTER(ctypes.c_double),
    ]
    fn.restype = None
    return fn


@pytest.mark.core
def test_legacy_ohm_dqndt_cal_nsh1_hourly_shift(tmp_path: Path):
    """Legacy ``OHM_dqndt_cal`` with ``nsh = 1`` must shift the 3-slot ring buffer."""
    call_probe = _compile_ohm_dqndt_cal_probe(tmp_path)
    qn_store = np.full(1, -999.0, dtype=np.float64)
    qn_av_store = np.full(3, -999.0, dtype=np.float64)
    dqndt_out = ctypes.c_double(-999.0)

    hourly_qn = [100.0, 220.0, 340.0, 400.0]
    expected_dqndt = [
        0.0,
        0.0,
        0.5 * (hourly_qn[2] - hourly_qn[0]),
        0.5 * (hourly_qn[3] - hourly_qn[1]),
    ]

    for step, (qn_val, expected) in enumerate(
        zip(hourly_qn, expected_dqndt, strict=True), start=1
    ):
        call_probe(1, qn_val, qn_store, qn_av_store, ctypes.byref(dqndt_out))
        assert dqndt_out.value == pytest.approx(expected, rel=1.0e-12, abs=1.0e-12), (
            f"OHM_dqndt_cal(nsh=1) step {step}: got {dqndt_out.value}, expected {expected}"
        )

    np.testing.assert_allclose(
        qn_av_store, [220.0, 340.0, 400.0], rtol=0.0, atol=1.0e-12
    )
