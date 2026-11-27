"""Behavioural tests for ``supy.util._roughness`` and ``supy.util._ohm``."""

from __future__ import annotations

import logging

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest

from supy.util._ohm import (
    compare_heat_storage,
    derive_ohm_coef,
    replace_ohm_coeffs,
    sim_ohm,
)
from supy.util._roughness import cal_neutral, cal_z0zd

pytestmark = [pytest.mark.api, pytest.mark.contract("C-RESPONSE")]


@pytest.fixture(autouse=True)
def _close_figures():
    """Ensure every matplotlib figure created during a test is closed cleanly."""
    yield
    plt.close("all")


@pytest.fixture
def supy_caplog(caplog):
    """Capture SuPy logger records even though SuPy defaults to propagate=False."""
    supy_logger = logging.getLogger("SuPy")
    original = supy_logger.propagate
    supy_logger.propagate = True
    try:
        yield caplog
    finally:
        supy_logger.propagate = original


def _make_neutral_series(
    n_neutral: int = 20,
    z_meas: float = 30.0,
    z0_true: float = 1.2,
    zd_true: float = 6.5,
) -> dict[str, pd.Series]:
    """Build synthetic micrometeorological series with known neutral subset."""
    n_total = n_neutral + 4
    idx = pd.date_range("2023-07-01", periods=n_total, freq="h")

    # Near-zero sensible heat flux gives |L| >> z_meas -> |(z_meas - z_d)/L| < 0.01.
    # Final 4 timesteps carry strong positive/negative heat flux (non-neutral).
    qh_vals = np.concatenate([
        np.full(n_neutral, 0.01),
        np.array([250.0, -180.0, 320.0, -220.0]),
    ])
    ustar_vals = np.concatenate([
        np.linspace(0.3, 1.5, n_neutral),
        np.array([0.2, 0.2, 0.25, 0.2]),
    ])
    ta_vals = np.full(n_total, 20.0)
    rh_vals = np.full(n_total, 55.0)
    pres_vals = np.full(n_total, 1013.25)

    # Exact logarithmic wind profile for the neutral rows.
    ws_vals = (ustar_vals / 0.4) * np.log((z_meas - zd_true) / z0_true)
    # Perturb non-neutral rows so a failure to filter them breaks the fit.
    ws_vals[n_neutral:] = ws_vals[n_neutral:] * 3.0 + 10.0

    return {
        "qh": pd.Series(qh_vals, index=idx),
        "ustar": pd.Series(ustar_vals, index=idx),
        "ta": pd.Series(ta_vals, index=idx),
        "rh": pd.Series(rh_vals, index=idx),
        "pres": pd.Series(pres_vals, index=idx),
        "ws": pd.Series(ws_vals, index=idx),
    }


def _make_state_with_ohm(n_grids: int = 2) -> pd.DataFrame:
    """Construct a minimal state DataFrame with canonical (8, 4, 3) ohm_coef columns."""
    ohm_tuples = [
        ("ohm_coef", f"({lc}, {cond}, {coef})")
        for lc in range(8)
        for cond in range(4)
        for coef in range(3)
    ]
    other_tuples = [("alb", "(0,)"), ("alb", "(1,)")]
    cols = pd.MultiIndex.from_tuples(
        ohm_tuples + other_tuples, names=["var", "ind_dim"]
    )
    base_ohm = np.arange(n_grids * 8 * 4 * 3, dtype=float).reshape(n_grids, 8 * 4 * 3)
    other_vals = np.full((n_grids, 2), 0.15)
    data = np.hstack([base_ohm, other_vals])
    return pd.DataFrame(data, index=range(1, n_grids + 1), columns=cols)


class TestRoughnessUtilities:
    """Tests for neutral filtering and roughness parameter fitting."""

    def test_cal_neutral_selects_only_quasi_neutral_rows(self):
        """Rows with |(z_meas - 0.7*h_sfc)/L| <= 0.01 are kept and non-neutral excluded."""
        z_meas = 30.0
        h_sfc = 10.0
        n_neutral = 12
        data = _make_neutral_series(n_neutral=n_neutral, z_meas=z_meas)

        ser_ws_n, ser_ustar_n = cal_neutral(
            data["qh"],
            data["ustar"],
            data["ta"],
            data["rh"],
            data["pres"],
            data["ws"],
            z_meas=z_meas,
            h_sfc=h_sfc,
        )

        expected_index = data["ws"].index[:n_neutral]
        assert list(ser_ws_n.index) == list(expected_index)
        assert list(ser_ustar_n.index) == list(expected_index)
        np.testing.assert_allclose(ser_ws_n.values, data["ws"].iloc[:n_neutral].values)
        np.testing.assert_allclose(
            ser_ustar_n.values, data["ustar"].iloc[:n_neutral].values
        )

    def test_cal_neutral_logs_when_displacement_exceeds_measurement_height(
        self, supy_caplog
    ):
        """When 0.7*h_sfc >= z_meas, cal_neutral logs an exception warning."""
        data = _make_neutral_series(n_neutral=4, z_meas=10.0)
        with supy_caplog.at_level(logging.ERROR, logger="SuPy"):
            ser_ws_n, ser_ustar_n = cal_neutral(
                data["qh"],
                data["ustar"],
                data["ta"],
                data["rh"],
                data["pres"],
                data["ws"],
                z_meas=10.0,
                h_sfc=20.0,  # 0.7 * 20 = 14 >= 10
            )

        assert any(
            "vegetation height is greater than measuring height" in rec.message
            for rec in supy_caplog.records
        )
        assert len(ser_ws_n) == len(ser_ustar_n)

    def test_cal_z0zd_recovers_log_law_ratio_and_respects_bounds(self):
        """Fitted (z0, zd) reproduces the exact neutral log-law slope within bounds."""
        z_meas = 30.0
        h_sfc = 10.0
        z0_true = 1.2
        zd_true = 6.5
        data = _make_neutral_series(
            n_neutral=20, z_meas=z_meas, z0_true=z0_true, zd_true=zd_true
        )

        z0_fit, zd_fit = cal_z0zd(
            data["qh"],
            data["ustar"],
            data["ta"],
            data["rh"],
            data["pres"],
            data["ws"],
            z_meas=z_meas,
            h_sfc=h_sfc,
            debug=False,
        )

        # Bounds enforced by lmfit Parameter setup: [0.01 * h_sfc, 0.95 * h_sfc].
        assert 0.01 * h_sfc <= z0_fit <= 0.95 * h_sfc
        assert 0.01 * h_sfc <= zd_fit <= 0.95 * h_sfc

        # At single measurement height z_meas, log((z_meas - zd)/z0) is uniquely identified.
        expected_log_ratio = np.log((z_meas - zd_true) / z0_true)
        fitted_log_ratio = np.log((z_meas - zd_fit) / z0_fit)
        assert fitted_log_ratio == pytest.approx(expected_log_ratio, rel=1e-4)

    def test_cal_z0zd_debug_returns_model_result_consistent_with_default(self):
        """debug=True returns an lmfit ModelResult matching debug=False parameters."""
        z_meas = 30.0
        h_sfc = 10.0
        data = _make_neutral_series(n_neutral=15, z_meas=z_meas)

        z0_plain, zd_plain = cal_z0zd(
            data["qh"],
            data["ustar"],
            data["ta"],
            data["rh"],
            data["pres"],
            data["ws"],
            z_meas=z_meas,
            h_sfc=h_sfc,
            debug=False,
        )
        res_debug = cal_z0zd(
            data["qh"],
            data["ustar"],
            data["ta"],
            data["rh"],
            data["pres"],
            data["ws"],
            z_meas=z_meas,
            h_sfc=h_sfc,
            debug=True,
        )

        assert res_debug.success
        assert res_debug.best_values["z0"] == pytest.approx(z0_plain, rel=1e-9)
        assert res_debug.best_values["zd"] == pytest.approx(zd_plain, rel=1e-9)
        assert np.max(np.abs(res_debug.residual)) < 1e-4

    def test_cal_z0zd_falls_back_to_rule_of_thumb_when_no_neutral_rows(self, capsys):
        """When neutral filtering leaves zero rows, fallback returns (0.1*h, 0.7*h)."""
        h_sfc = 12.0
        z_meas = 30.0
        idx = pd.date_range("2023-07-01", periods=6, freq="h")
        # Strongly convective conditions at all timestamps -> zero neutral rows.
        ser_qh = pd.Series(np.full(6, 400.0), index=idx)
        ser_ustar = pd.Series(np.full(6, 0.2), index=idx)
        ser_ta = pd.Series(np.full(6, 25.0), index=idx)
        ser_rh = pd.Series(np.full(6, 50.0), index=idx)
        ser_pres = pd.Series(np.full(6, 1013.25), index=idx)
        ser_ws = pd.Series(np.full(6, 2.5), index=idx)

        z0_fb, zd_fb = cal_z0zd(
            ser_qh,
            ser_ustar,
            ser_ta,
            ser_rh,
            ser_pres,
            ser_ws,
            z_meas=z_meas,
            h_sfc=h_sfc,
        )

        assert z0_fb == pytest.approx(0.1 * h_sfc, rel=1e-12)
        assert zd_fb == pytest.approx(0.7 * h_sfc, rel=1e-12)
        captured = capsys.readouterr()
        assert "Fitting failed!" in captured.out


class TestOhmUtilities:
    """Analytical, state-mutation, error-path, and plotting tests for _ohm."""

    def test_derive_ohm_coef_exact_recovery_subhourly_and_nonfinite(self):
        """derive_ohm_coef recovers exact (a1, a2, a3) at 30-min freq despite inf/NaN."""
        idx = pd.date_range("2023-07-01", periods=96, freq="30min")
        t_hr = np.arange(len(idx)) * 0.5
        ser_qn = pd.Series(
            300.0 * np.sin(2.0 * np.pi * t_hr / 24.0)
            + 80.0 * np.cos(4.0 * np.pi * t_hr / 24.0),
            index=idx,
        )
        a1_true, a2_true, a3_true = 0.35, 0.45, -12.5
        ser_qs = sim_ohm(ser_qn, a1_true, a2_true, a3_true)
        # Inject +/-inf rows to confirm they are filtered out cleanly.
        ser_qs.iloc[10] = np.inf
        ser_qs.iloc[20] = -np.inf

        a1_fit, a2_fit, a3_fit = derive_ohm_coef(ser_qs, ser_qn)

        assert a1_fit == pytest.approx(a1_true, abs=1e-10)
        assert a2_fit == pytest.approx(a2_true, abs=1e-10)
        assert a3_fit == pytest.approx(a3_true, abs=1e-10)

    def test_sim_ohm_analytical_formula_and_missing_freq_error(self):
        """sim_ohm applies a1*QN + a2*dQN/dt + a3 and rejects indices without freq."""
        idx = pd.date_range("2023-07-01", periods=10, freq="30min")
        ser_qn = pd.Series(
            np.linspace(0.0, 180.0, 10), index=idx
        )  # +20 per 30min = +40/h
        ser_qs = sim_ohm(ser_qn, a1=0.3, a2=0.5, a3=-5.0)

        assert np.isnan(ser_qs.iloc[0])
        expected = 0.3 * ser_qn.iloc[1:] + 0.5 * 40.0 - 5.0
        np.testing.assert_allclose(ser_qs.iloc[1:].values, expected.values, rtol=1e-12)

        ser_no_freq = pd.Series([10.0, 20.0, 30.0])  # RangeIndex has no .freq attribute
        with pytest.raises(RuntimeError, match="frequency info is missing"):
            sim_ohm(ser_no_freq, 0.3, 0.5, -5.0)

    @pytest.mark.parametrize(
        ("lc_input", "expected_idx"),
        [
            ("Paved", 0),
            ("BLDGS", 1),
            ("evetr", 2),
            ("DecTr", 3),
            ("grass", 4),
            ("BSOIL", 5),
            ("Water", 6),
            (3, 3),
        ],
    )
    def test_replace_ohm_coeffs_updates_target_land_cover_without_mutating_input(
        self, lc_input, expected_idx
    ):
        """replace_ohm_coeffs updates only the chosen land cover and leaves input intact."""
        df_orig = _make_state_with_ohm(n_grids=2)
        orig_snapshot = df_orig.copy(deep=True)
        new_coefs = (0.42, 0.25, -18.0)

        df_updated = replace_ohm_coeffs(df_orig, new_coefs, lc_input)

        # Input DataFrame is never mutated.
        pd.testing.assert_frame_equal(df_orig, orig_snapshot)

        # Non-OHM columns remain untouched.
        pd.testing.assert_frame_equal(df_updated.loc[:, "alb"], df_orig.loc[:, "alb"])

        # Target land cover block matches new_coefs across all 4 conditions and all grids.
        ohm_before = df_orig.loc[:, "ohm_coef"].values.reshape((-1, 8, 4, 3))
        ohm_after = df_updated.loc[:, "ohm_coef"].values.reshape((-1, 8, 4, 3))
        expected_block = np.broadcast_to(np.array(new_coefs), (2, 4, 3))
        np.testing.assert_allclose(ohm_after[:, expected_idx], expected_block)

        # Every other land cover (including snow slot 7) is unchanged.
        for lc in range(8):
            if lc != expected_idx:
                np.testing.assert_allclose(ohm_after[:, lc], ohm_before[:, lc])

    def test_replace_ohm_coeffs_accepts_per_condition_sequences(self):
        """4-element sequences populate SW/SD/WW/WD conditions distinctly."""
        df_orig = _make_state_with_ohm(n_grids=1)
        a1_seq = [0.11, 0.12, 0.13, 0.14]
        a2_seq = [0.21, 0.22, 0.23, 0.24]
        a3_seq = [-1.0, -2.0, -3.0, -4.0]

        df_updated = replace_ohm_coeffs(df_orig, (a1_seq, a2_seq, a3_seq), "Bldgs")

        ohm_after = df_updated.loc[:, "ohm_coef"].values.reshape((1, 8, 4, 3))
        np.testing.assert_allclose(ohm_after[0, 1, :, 0], a1_seq)
        np.testing.assert_allclose(ohm_after[0, 1, :, 1], a2_seq)
        np.testing.assert_allclose(ohm_after[0, 1, :, 2], a3_seq)

    def test_compare_heat_storage_returns_diurnal_and_scatter_figures(self):
        """compare_heat_storage returns diurnal climatology and 1:1 comparison plots."""
        idx = pd.date_range("2023-07-01", periods=72, freq="h")
        hours = np.arange(len(idx)) % 24
        ser_qn = pd.Series(
            400.0 * np.maximum(0.0, np.sin(np.pi * (hours - 6) / 12)),
            index=idx,
        )
        ser_qs_obs = sim_ohm(ser_qn, 0.35, 0.25, -10.0) + 2.0

        plot1, plot2 = compare_heat_storage(ser_qn, ser_qs_obs, 0.35, 0.25, -10.0)
        fig1, ax1 = plot1
        fig2, ax2 = plot2

        assert isinstance(fig1, plt.Figure)
        assert isinstance(fig2, plt.Figure)
        labels_diurnal = {line.get_label() for line in ax1.get_lines()}
        assert {"Sim", "Obs"}.issubset(labels_diurnal)

        legend_texts = [t.get_text() for t in ax2.get_legend().get_texts()]
        assert any("R^2" in txt and "n=71" in txt for txt in legend_texts)
        assert ax2.get_xlim() == ax2.get_ylim()
