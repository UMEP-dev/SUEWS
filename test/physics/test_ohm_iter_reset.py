"""Regression tests for OHM_STATE iteration reset, unaliasing, AnOHM, and DyOHM.

Setting ``OHM_STATE%iter_safe = .FALSE.`` ensures running-mean net radiation
histories (``OHM_dqndt_cal_X``) and daily running averages/coefficients in
AnOHM and DyOHM advance once per converged timestep rather than once per inner
surface-temperature pass. Companion fixes remove illegal Fortran 2018
``INTENT(IN)``/``INTENT(OUT)`` aliasing at ``CALL OHM`` and ``CALL AnOHM``,
pass ``dqndt_next`` into ``OHM_QS_cal`` inside ``AnOHM``, and restore
``timer%dt_since_start_prev`` and ``timer%new_day`` alongside ``ohmState`` on
inner retry passes.
"""

from __future__ import annotations

import copy
from importlib import import_module
import json
from pathlib import Path

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy.data_model import SUEWSConfig
from supy.data_model.core.model import StorageHeatMethod

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-RESPONSE", "C-BALANCE")]

ENERGY_CLOSURE_TOLERANCE_W_M2 = 1.0e-6
STEBBS_CONFIG_PATH = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "data_test"
    / "stebbs_test"
    / "sample_config.yml"
)


def _ohm_dqndt_single_pass_series(
    qn_series: np.ndarray,
    tstep: int = 300,
    qn_av_init: float = 0.0,
    dqndt_init: float = 0.0,
) -> tuple[np.ndarray, np.ndarray, float, float]:
    """Evaluate ``OHM_dqndt_cal_X`` once per timestep on a converged ``QN`` series."""
    dt0_thresh = 3600
    window_hr = 2.0
    # Match Fortran `dt/sec_per_hour_sp` single-precision quotient in OHM_dqndt_cal_X.
    dt_hr = float(np.float32(tstep) / np.float32(3600.0))

    n_steps = len(qn_series)
    qn_av_out = np.empty(n_steps, dtype=float)
    dqndt_out = np.empty(n_steps, dtype=float)
    qn_av_prev = float(qn_av_init)
    dqndt_prev = float(dqndt_init)

    for step_idx in range(n_steps):
        # Fresh runs start at dt_since_start = tstep (see src/suews_bridge/src/sim.rs).
        dt_since_start = (step_idx + 1) * tstep
        dt0 = dt_since_start + tstep if dt_since_start < dt0_thresh else dt0_thresh
        qn1 = float(qn_series[step_idx])
        qn1_av_0 = qn_av_prev - dqndt_prev * (window_hr - dt_hr)
        qn_av_next = (qn_av_prev * (dt0 - tstep) + qn1 * tstep) / float(dt0)
        dqndt_next = (qn_av_next - qn1_av_0) / window_hr
        qn_av_out[step_idx] = qn_av_next
        dqndt_out[step_idx] = dqndt_next
        qn_av_prev = qn_av_next
        dqndt_prev = dqndt_next

    return qn_av_out, dqndt_out, qn_av_prev, dqndt_prev


def _decode_ohm_checkpoint(sim: sp.SUEWSSimulation) -> dict:
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


def _make_rain_day(sim_short: sp.SUEWSSimulation) -> pd.DataFrame:
    """Construct the controlled warm sunlit day with a 3-hour morning rainfall pulse."""
    df_rain_day = sim_short.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()
    df_rain_day["Tair"] = 20.0
    df_rain_day["RH"] = 55.0
    df_rain_day["U"] = 3.5
    df_rain_day["pres"] = 1013.0
    df_rain_day["kdown"] = np.maximum(
        0.0, 650.0 * np.sin(np.pi * (np.arange(TIMESTEPS_PER_DAY) - 72) / 144.0)
    )
    df_rain_day["rain"] = 0.0
    df_rain_day.iloc[60:96, df_rain_day.columns.get_loc("rain")] = 1.5
    return df_rain_day


def _run_config(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
) -> tuple[sp.SUEWSSimulation, pd.DataFrame]:
    """Run ``config`` on ``df_forcing`` with a single worker and return ``(sim, df)``."""
    sim = sp.SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    output = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    )
    return sim, output.df


@pytest.mark.core
def test_ohm_small_sfr_continuity(short_sample_yaml_path):
    """Tiny ``evetr.sfr = 1e-6`` must remain continuous in ``QS`` without iteration amplification."""
    base_cfg = SUEWSConfig.from_yaml(str(short_sample_yaml_path))
    sim_short = sp.SUEWSSimulation(str(short_sample_yaml_path))
    df_rain_day = _make_rain_day(sim_short)

    def _build_case(evetr_sfr: float, evetr_emis: float) -> SUEWSConfig:
        cfg = copy.deepcopy(base_cfg)
        lc = cfg.sites[0].properties.land_cover
        paved0 = float(lc.paved.sfr.value)
        lc.bsoil.sfr.value = 0.05
        lc.evetr.sfr.value = evetr_sfr
        lc.paved.sfr.value = round(paved0 - 0.05 - evetr_sfr, 10)
        lc.evetr.emis.value = evetr_emis
        return cfg

    _, df_zero = _run_config(_build_case(0.0, 0.98), df_rain_day)
    _, df_tiny_base = _run_config(_build_case(1.0e-6, 0.98), df_rain_day)
    _, df_tiny_pert = _run_config(_build_case(1.0e-6, 0.85), df_rain_day)

    s_zero = df_zero.loc[1, "SUEWS"]
    s_tiny_base = df_tiny_base.loc[1, "SUEWS"]
    s_tiny_pert = df_tiny_pert.loc[1, "SUEWS"]

    dqs_pert = float(np.max(np.abs((s_tiny_pert["QS"] - s_tiny_base["QS"]).to_numpy())))
    dqn_pert = float(np.max(np.abs((s_tiny_pert["QN"] - s_tiny_base["QN"]).to_numpy())))
    dqs_vs_zero = float(np.max(np.abs((s_tiny_base["QS"] - s_zero["QS"]).to_numpy())))

    assert dqn_pert > 0.0
    assert dqs_pert < 1.0e-4, (
        f"Small-sfr emissivity perturbation jumped QS by {dqs_pert:.3e} W m-2 "
        f"(dQN={dqn_pert:.3e}, ratio={dqs_pert / dqn_pert:.1f}x)"
    )
    assert dqs_pert / dqn_pert < 2.0, (
        f"QS response ratio {dqs_pert / dqn_pert:.2f}x is not linear in sfr"
    )
    # Replacing 1e-6 of paved (a1 ~ 0.82) by evetr (a1 ~ 0.11) shifts QS linearly by
    # O(sfr * d(a1) * QN) ~ 2e-4 W m-2 rather than jumping by ~0.08 W m-2.
    assert dqs_vs_zero < 1.0e-3, (
        f"evetr.sfr=1e-6 vs 0.0 jumped QS by {dqs_vs_zero:.3e} W m-2"
    )

    for label, suews in (
        ("zero", s_zero),
        ("tiny_base", s_tiny_base),
        ("tiny_pert", s_tiny_pert),
    ):
        eb_res = float(
            np.max(
                np.abs(
                    (
                        suews["QN"]
                        + suews["QF"]
                        - suews["QH"]
                        - suews["QE"]
                        - suews["QS"]
                    ).to_numpy()
                )
            )
        )
        assert eb_res < ENERGY_CLOSURE_TOLERANCE_W_M2, (
            f"{label}: energy closure residual {eb_res:.3e} W m-2"
        )


def _set_uniform_ohm_coefficients(
    config: SUEWSConfig, a1: float, a2: float, a3: float
) -> None:
    """Set uniform OHM coefficients across all seven surfaces and four regimes."""
    lc = config.sites[0].properties.land_cover
    for surface_name in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
        ohm_coef = getattr(lc, surface_name).ohm_coef
        for regime in ("summer_dry", "summer_wet", "winter_dry", "winter_wet"):
            regime_coef = getattr(ohm_coef, regime)
            regime_coef.a1.value = a1
            regime_coef.a2.value = a2
            regime_coef.a3.value = a3


def _areal_anohm_coefficients(
    config: SUEWSConfig, ohm_state: dict
) -> tuple[float, float, float]:
    """Return areal-weighted AnOHM ``(a1, a2, a3)`` from checkpoint surface coefficients."""
    lc = config.sites[0].properties.land_cover
    sfr = np.array(
        [
            float(getattr(lc, name).sfr.value)
            for name in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
        ],
        dtype=float,
    )
    surf_names = ("paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water")
    return tuple(
        float(
            np.dot(
                sfr,
                [ohm_state[f"anohm_{coeff}_surf.{name}"] for name in surf_names],
            )
        )
        for coeff in ("a1", "a2", "a3")
    )


@pytest.mark.core
def test_ohm_dqndt_single_pass_consistency(short_sample_yaml_path):
    """Multi-iteration OHM ``dqndt`` and ``QS`` must match single-pass recurrence on converged ``QN``."""
    cfg = SUEWSConfig.from_yaml(str(short_sample_yaml_path))
    cfg.model.physics.storage_heat = StorageHeatMethod.OHM_WITHOUT_QF
    cfg.model.physics.ohm_inc_qf = 0
    a1_const, a2_const, a3_const = 0.42, 0.35, -24.0
    _set_uniform_ohm_coefficients(cfg, a1_const, a2_const, a3_const)

    df_day = _make_rain_day(sp.SUEWSSimulation(str(short_sample_yaml_path)))
    df_day["rain"] = 0.0

    sim, df_out = _run_config(cfg, df_day)
    # Confirm the simulation genuinely executes multi-pass surface-temperature iterations.
    assert float(np.mean(df_out.loc[1, ("debug", "iter")].to_numpy(dtype=float))) > 3.0

    qn = df_out.loc[1, ("SUEWS", "QN")].to_numpy(dtype=float)
    _, dqndt_single, qn_av_final, dqndt_final = _ohm_dqndt_single_pass_series(
        qn, tstep=300
    )

    np.testing.assert_allclose(
        df_out.loc[1, ("debug", "dqndt")].to_numpy(dtype=float),
        dqndt_single,
        rtol=1.0e-10,
        atol=1.0e-9,
        err_msg="OHM dqndt diverges from single-pass OHM_dqndt_cal_X recurrence",
    )
    np.testing.assert_allclose(
        df_out.loc[1, ("SUEWS", "QS")].to_numpy(dtype=float),
        a1_const * qn + a2_const * dqndt_single + a3_const,
        rtol=1.0e-10,
        atol=1.0e-9,
        err_msg="OHM QS diverges from single-pass OHM_QS_cal evaluated at converged QN",
    )

    ohm_state = _decode_ohm_checkpoint(sim)
    assert ohm_state["iter_safe"] == pytest.approx(0.0)
    assert ohm_state["qn_av"] == pytest.approx(qn_av_final, rel=1.0e-10, abs=1.0e-9)
    assert ohm_state["dqndt"] == pytest.approx(dqndt_final, rel=1.0e-10, abs=1.0e-9)


@pytest.mark.core
def test_anohm_uses_updated_dqndt_next_and_commits_cleanly():
    """AnOHM must feed freshly computed ``dqndt_next`` into ``OHM_QS_cal`` and commit daily samples once."""
    sim_template = sp.SUEWSSimulation.from_sample_data()
    cfg = copy.deepcopy(sim_template.config)
    cfg.model.physics.net_radiation = 3
    cfg.model.physics.storage_heat = StorageHeatMethod.ANOHM
    cfg.model.physics.ohm_inc_qf = 0

    # End at 2012-01-03 23:55 (id=3) so the final checkpoint holds day 3's live
    # AnOHM coefficients and all 24 on-the-hour samples accumulated on day 3.
    sim, df_out = _run_config(
        cfg, sim_template.forcing.df.iloc[: TIMESTEPS_PER_DAY * 3 - 1].copy()
    )
    qn = df_out.loc[1, ("SUEWS", "QN")].to_numpy(dtype=float)
    qs = df_out.loc[1, ("SUEWS", "QS")].to_numpy(dtype=float)
    _, dqndt_single, qn_av_final, dqndt_final = _ohm_dqndt_single_pass_series(
        qn, tstep=300
    )

    np.testing.assert_allclose(
        df_out.loc[1, ("debug", "dqndt")].to_numpy(dtype=float),
        dqndt_single,
        rtol=1.0e-10,
        atol=1.0e-9,
        err_msg="AnOHM dqndt diverges from single-pass OHM_dqndt_cal_X recurrence",
    )

    ohm_state = _decode_ohm_checkpoint(sim)
    assert ohm_state["iter_safe"] == pytest.approx(0.0)
    assert ohm_state["anohm_coeff_ready"] == pytest.approx(1.0)
    assert ohm_state["anohm_working_count"] == pytest.approx(24.0)
    assert ohm_state["anohm_coeff_count"] == pytest.approx(24.0)
    assert ohm_state["qn_av"] == pytest.approx(qn_av_final, rel=1.0e-10, abs=1.0e-9)
    assert ohm_state["dqndt"] == pytest.approx(dqndt_final, rel=1.0e-10, abs=1.0e-9)

    coeffs = _areal_anohm_coefficients(cfg, ohm_state)
    day3_start = 2 * TIMESTEPS_PER_DAY - 1
    qs_day3 = qs[day3_start:]
    np.testing.assert_allclose(
        qs_day3,
        coeffs[0] * qn[day3_start:] + coeffs[1] * dqndt_single[day3_start:] + coeffs[2],
        rtol=1.0e-9,
        atol=1.0e-8,
        err_msg="AnOHM QS must use updated dqndt_next rather than stale dqndt_prev",
    )
    # Guard against a vacuous check: dqndt_prev and dqndt_next must meaningfully differ.
    assert (
        float(
            np.max(
                np.abs(
                    qs_day3
                    - (
                        coeffs[0] * qn[day3_start:]
                        + coeffs[1] * dqndt_single[day3_start - 1 : -1]
                        + coeffs[2]
                    )
                )
            )
        )
        > 0.2
    )


@pytest.mark.core
@pytest.mark.parametrize("storage_method", [6, 7, 8])
def test_dyohm_updates_daily_coefficients_under_iter_reset(storage_method):
    """DyOHM (methods 6, 7, 8) must keep updating daily running averages and coefficients."""
    if storage_method == 7:
        sim_template = sp.SUEWSSimulation(str(STEBBS_CONFIG_PATH))
        cfg = copy.deepcopy(sim_template.config)
        cfg.model.control.output.groups = ["SUEWS", "STEBBS", "DailyState", "debug"]
        df_5d = sim_template.forcing.df.iloc[: TIMESTEPS_PER_DAY * 5].copy()
        # STEBBS fixture forcing uses -999 as dry-period sentinel; normalise to 0.
        df_5d["rain"] = df_5d["rain"].clip(lower=0)
        coeff_cols = ("a1", "a2", "a3")
    else:
        sim_template = sp.SUEWSSimulation.from_sample_data()
        cfg = copy.deepcopy(sim_template.config)
        cfg.model.physics.storage_heat = StorageHeatMethod(storage_method)
        df_5d = sim_template.forcing.df.iloc[: TIMESTEPS_PER_DAY * 5].copy()
        coeff_cols = (
            ("a1", "a2", "a3", "a1_bldg", "a2_bldg", "a3_bldg")
            if storage_method == 6
            else ("a1_bldg", "a2_bldg", "a3_bldg")
        )

    sim, df_out = _run_config(cfg, df_5d)
    suews = df_out.loc[1, "SUEWS"]
    daily = df_out.loc[1, "DailyState"].dropna(how="all")
    iters = df_out.loc[1, ("debug", "iter")].to_numpy(dtype=float)

    # Confirm multi-pass inner iterations ran (where missing timer reset would freeze DyOHM).
    assert float(np.mean(iters)) > 2.5

    core_values = suews[["QN", "QH", "QE", "QS", "Ts"]].to_numpy(dtype=float)
    assert np.isfinite(core_values).all(), (
        f"Non-finite core output for StorageHeatMethod={storage_method}"
    )

    eb_res = float(
        np.max(
            np.abs(
                (
                    suews["QN"] + suews["QF"] - suews["QH"] - suews["QE"] - suews["QS"]
                ).to_numpy()
            )
        )
    )
    assert eb_res < ENERGY_CLOSURE_TOLERANCE_W_M2, (
        f"StorageHeatMethod={storage_method}: energy closure residual {eb_res:.3e} W m-2"
    )

    for col in coeff_cols:
        vals = daily[col].replace(-999.0, np.nan).dropna()
        n_unique = int(vals.round(6).nunique())
        assert n_unique == 5, (
            f"StorageHeatMethod={storage_method}: expected 5 distinct daily {col} values, "
            f"got {n_unique} ({vals.tolist()})"
        )

    ohm_state = _decode_ohm_checkpoint(sim)
    assert ohm_state["iter_safe"] == pytest.approx(0.0)
    assert ohm_state["ws_rav"] > 0.0, (
        "DyOHM running-average wind speed ws_rav froze at 0"
    )
    qn_rav_vals = [
        ohm_state[f"qn_rav.{name}"]
        for name in ("paved", "bldg", "evetr", "dectr", "grass", "bsoil", "water")
    ]
    assert any(abs(val) > 1.0e-3 for val in qn_rav_vals), (
        "DyOHM running-average net radiation qn_rav froze at 0"
    )
