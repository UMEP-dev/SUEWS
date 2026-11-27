"""Behavioural and numerical tests for supy.util._attribution.

Covers U10 and q2 attribution and diagnostics, generic dispatchers,
helper alignment/extraction contracts, AttributionResult formatting and
plotting, and flux-performance error handling.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest

from supy.constants import C2K
from supy.suews_forcing import SUEWSForcing
from supy.suews_output import SUEWSOutput
from supy.util._atm import cal_psi_mom, cal_qa
from supy.util._attribution import (
    AttributionResult,
    attribute,
    attribute_q2,
    attribute_t2,
    attribute_u10,
    diagnose,
    diagnose_flux_performance,
    diagnose_q2,
    diagnose_t2,
    diagnose_u10,
)
from supy.util._attribution._helpers import (
    align_scenarios,
    detect_anomalies,
    extract_suews_group,
    unwrap_forcing,
)
from supy.util._attribution._physics import (
    cal_gamma_humidity,
    cal_r_eff_humidity,
    cal_u10_profile_components,
)

pytestmark = [pytest.mark.api, pytest.mark.contract("C-BALANCE")]

VON_KARMAN = 0.4


def _make_log_u10(
    ustar: np.ndarray,
    z0m: np.ndarray,
    zd: np.ndarray,
    lob: np.ndarray,
    z_ref: float = 10.0,
) -> np.ndarray:
    """Compute U10 exactly from the logarithmic wind profile used by attribute_u10."""
    z_eff = z_ref - zd
    zeta = np.clip(z_eff / lob, -5.0, 5.0)
    return (ustar / VON_KARMAN) * (np.log(z_eff / z0m) - cal_psi_mom(zeta))


def _make_u10_frame(
    index: pd.DatetimeIndex,
    *,
    ustar: np.ndarray,
    z0m: np.ndarray,
    zd: np.ndarray,
    lob: np.ndarray,
    z_ref: float = 10.0,
) -> pd.DataFrame:
    """Construct a SUEWS-like output DataFrame with profile-consistent U10."""
    u10 = _make_log_u10(ustar, z0m, zd, lob, z_ref=z_ref)
    return pd.DataFrame(
        {
            "U10": u10,
            "UStar": ustar,
            "z0m": z0m,
            "zd": zd,
            "Lob": lob,
        },
        index=index,
    )


def _make_q2_scenario(
    index: pd.DatetimeIndex,
    *,
    tair: np.ndarray,
    rh: np.ndarray,
    pres: np.ndarray,
    qe: np.ndarray,
    r_eff: np.ndarray,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Build profile-consistent (df_output, df_forcing) for q2 attribution."""
    q_ref = np.asarray(cal_qa(rh, tair + C2K, pres), dtype=float)
    gamma = np.asarray(cal_gamma_humidity(tair, rh, pres), dtype=float)
    q2 = q_ref + r_eff * qe * gamma
    df_output = pd.DataFrame({"q2": q2, "QE": qe}, index=index)
    df_forcing = pd.DataFrame({"Tair": tair, "RH": rh, "pres": pres}, index=index)
    return df_output, df_forcing


def _make_t2_scenario(
    index: pd.DatetimeIndex,
    *,
    t_offset: float = 0.0,
    qh_scale: float = 1.0,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Build deterministic T2 output and forcing frames for dispatcher tests."""
    n = len(index)
    phase = np.linspace(0.0, 4.0 * np.pi, n)
    df_forcing = pd.DataFrame(
        {
            "Tair": 16.0 + 4.0 * np.sin(phase),
            "RH": 60.0 - 8.0 * np.sin(phase),
            "pres": np.full(n, 1013.25),
        },
        index=index,
    )
    qh = qh_scale * (110.0 + 40.0 * np.sin(phase))
    qe = 60.0 + 15.0 * np.sin(phase)
    qs = 30.0 + 10.0 * np.sin(phase)
    qf = np.full(n, 20.0)
    qn = qh + qe + qs - qf
    df_output = pd.DataFrame(
        {
            "T2": 20.0 + t_offset + 5.0 * np.sin(phase),
            "QH": qh,
            "QE": qe,
            "QN": qn,
            "QS": qs,
            "QF": qf,
        },
        index=index,
    )
    return df_output, df_forcing


class TestU10Attribution:
    """Tests for attribute_u10, diagnose_u10, and cal_u10_profile_components."""

    def test_profile_components_values_and_invalid_geometry(self):
        """Verify F, R, S formulas, zeta clamping, and NaN on invalid geometry."""
        ustar = np.array([0.4, 0.5, 0.5, 0.5])
        z0m = np.array([0.5, 0.0, 0.5, 0.5])  # index 1 invalid (z0m <= 0)
        zd = np.array([2.0, 2.0, 10.0, 2.0])  # index 2 invalid (z_ref <= zd)
        lob = np.array([-40.0, 50.0, 50.0, 0.5])  # index 3 extreme zeta > 5

        f_comp, r_comp, s_comp = cal_u10_profile_components(
            ustar, z0m, zd, lob, z_ref=10.0
        )

        np.testing.assert_allclose(f_comp, ustar / VON_KARMAN, rtol=1e-12)
        assert r_comp[0] == pytest.approx(np.log((10.0 - 2.0) / 0.5), rel=1e-12)
        assert np.isnan(r_comp[1])
        assert np.isnan(r_comp[2])
        assert s_comp[0] == pytest.approx(-cal_psi_mom((10.0 - 2.0) / -40.0), rel=1e-12)
        # Extreme zeta = (10 - 2) / 0.5 = 16 must clamp to 5.
        assert s_comp[3] == pytest.approx(-cal_psi_mom(5.0), rel=1e-12)

    def test_attribute_u10_exact_closure_and_metadata(self):
        """Forcing + roughness + stability must close delta_total exactly."""
        idx = pd.date_range("2023-07-01", periods=24, freq="h")
        phase = np.linspace(0.0, 2.0 * np.pi, len(idx))

        df_a = _make_u10_frame(
            idx,
            ustar=0.35 + 0.1 * np.sin(phase),
            z0m=np.full(len(idx), 0.3),
            zd=np.full(len(idx), 1.5),
            lob=np.where(np.sin(phase) >= 0, -80.0, 120.0),
        )
        df_b = _make_u10_frame(
            idx,
            ustar=0.42 + 0.1 * np.sin(phase),
            z0m=np.full(len(idx), 0.8),
            zd=np.full(len(idx), 2.5),
            lob=np.where(np.sin(phase) >= 0, -40.0, 60.0),
        )

        result = attribute_u10(df_a, df_b, z_ref=10.0, min_ustar=0.01)

        assert isinstance(result, AttributionResult)
        assert result.variable == "U10"
        assert result.metadata["n_timesteps"] == len(idx)
        assert result.metadata["z_ref"] == 10.0
        assert result.metadata["unit"] == "m/s"
        assert result.metadata["n_low_ustar_flagged"] == 0

        contrib = result.contributions
        total_from_parts = (
            contrib["forcing"] + contrib["roughness"] + contrib["stability"]
        )
        np.testing.assert_allclose(
            total_from_parts.to_numpy(),
            contrib["delta_total"].to_numpy(),
            rtol=1e-12,
            atol=1e-12,
        )
        for col in ("F_A", "F_B", "R_A", "R_B", "S_A", "S_B"):
            assert col in contrib.columns

    def test_attribute_u10_isolated_factor_sensitivities(self):
        """Changing one physical factor isolates its contribution with the right sign."""
        idx = pd.date_range("2023-07-01", periods=12, freq="h")
        base_ustar = np.full(12, 0.4)
        base_z0m = np.full(12, 0.3)
        base_zd = np.full(12, 1.5)
        base_lob = np.full(12, -100.0)
        df_base = _make_u10_frame(
            idx, ustar=base_ustar, z0m=base_z0m, zd=base_zd, lob=base_lob
        )

        # Higher friction velocity -> positive forcing contribution only.
        df_high_u = _make_u10_frame(
            idx, ustar=base_ustar * 1.25, z0m=base_z0m, zd=base_zd, lob=base_lob
        )
        res_u = attribute_u10(df_base, df_high_u)
        assert (res_u.contributions["forcing"] > 0).all()
        np.testing.assert_allclose(res_u.contributions["roughness"], 0.0, atol=1e-12)
        np.testing.assert_allclose(res_u.contributions["stability"], 0.0, atol=1e-12)
        np.testing.assert_allclose(
            res_u.contributions["forcing"],
            res_u.contributions["delta_total"],
            rtol=1e-12,
        )

        # Higher roughness length -> negative roughness contribution only.
        df_rough = _make_u10_frame(
            idx, ustar=base_ustar, z0m=base_z0m * 2.0, zd=base_zd, lob=base_lob
        )
        res_r = attribute_u10(df_base, df_rough)
        assert (res_r.contributions["roughness"] < 0).all()
        np.testing.assert_allclose(res_r.contributions["forcing"], 0.0, atol=1e-12)
        np.testing.assert_allclose(res_r.contributions["stability"], 0.0, atol=1e-12)

        # Shift from unstable (Lob < 0) to stable (Lob > 0) -> positive stability term
        # because psi_m(stable) < 0 < psi_m(unstable), so -psi_m increases.
        df_stable = _make_u10_frame(
            idx, ustar=base_ustar, z0m=base_z0m, zd=base_zd, lob=np.full(12, 50.0)
        )
        res_s = attribute_u10(df_base, df_stable)
        assert (res_s.contributions["stability"] > 0).all()
        np.testing.assert_allclose(res_s.contributions["forcing"], 0.0, atol=1e-12)
        np.testing.assert_allclose(res_s.contributions["roughness"], 0.0, atol=1e-12)

    def test_attribute_u10_low_ustar_flag_and_custom_z_ref(self):
        """Low |u*| timesteps are flagged and custom z_ref is honoured."""
        idx = pd.date_range("2023-07-01", periods=6, freq="h")
        z_ref = 20.0
        ustar_a = np.array([0.002, 0.005, 0.2, 0.3, 0.4, 0.5])
        ustar_b = np.array([0.05, 0.1, 0.25, 0.35, 0.45, 0.55])
        z0m = np.full(6, 0.4)
        zd = np.full(6, 2.0)
        lob = np.full(6, 100.0)

        df_a = _make_u10_frame(idx, ustar=ustar_a, z0m=z0m, zd=zd, lob=lob, z_ref=z_ref)
        df_b = _make_u10_frame(idx, ustar=ustar_b, z0m=z0m, zd=zd, lob=lob, z_ref=z_ref)

        result = attribute_u10(df_a, df_b, z_ref=z_ref, min_ustar=0.01)

        assert result.metadata["z_ref"] == z_ref
        assert result.metadata["n_low_ustar_flagged"] == 2
        assert list(result.contributions["flag_low_ustar"]) == [
            True,
            True,
            False,
            False,
            False,
            False,
        ]
        np.testing.assert_allclose(
            result.contributions[["forcing", "roughness", "stability"]].sum(axis=1),
            result.contributions["delta_total"],
            rtol=1e-12,
        )

    def test_attribute_u10_missing_columns_raises(self):
        """Missing required U10 columns raise an informative ValueError."""
        idx = pd.date_range("2023-07-01", periods=4, freq="h")
        df_ok = _make_u10_frame(
            idx,
            ustar=np.full(4, 0.3),
            z0m=np.full(4, 0.2),
            zd=np.full(4, 1.0),
            lob=np.full(4, 50.0),
        )
        with pytest.raises(ValueError, match="Lob"):
            attribute_u10(df_ok.drop(columns=["Lob"]), df_ok)

    @pytest.mark.parametrize("method", ["anomaly", "extreme", "diurnal"])
    def test_diagnose_u10_methods(self, method: str):
        """diagnose_u10 produces single-row aggregate attribution for all methods."""
        idx = pd.date_range("2023-07-01", periods=168, freq="h")
        hours = idx.hour.to_numpy()
        # Strong afternoon wind speed signal with repeated daily extremes.
        diurnal = np.where((hours >= 12) & (hours <= 15), 1.0, 0.0)
        ustar = 0.25 + 0.35 * diurnal + 0.05 * np.sin(2.0 * np.pi * hours / 24.0)
        z0m = np.full(len(idx), 0.3)
        zd = np.full(len(idx), 1.5)
        lob = np.where(diurnal > 0, -40.0, 80.0)
        df = _make_u10_frame(idx, ustar=ustar, z0m=z0m, zd=zd, lob=lob)

        result = diagnose_u10(df, method=method, threshold=1.2, z_ref=10.0)

        assert result.variable == "U10"
        assert list(result.contributions.index) == ["aggregate"]
        assert result.metadata["method"] == method
        assert result.metadata["aggregate_comparison"] is True
        assert result.metadata["n_timesteps"] == 1
        assert result.metadata["n_anomaly"] >= 10
        assert result.metadata["n_reference"] >= 10
        assert result.metadata["threshold"] == (1.2 if method == "anomaly" else None)
        # Afternoon is windier than morning/reference for diurnal method.
        if method == "diurnal":
            assert result.contributions.loc["aggregate", "delta_total"] > 0

    def test_diagnose_u10_missing_u10_raises(self):
        """diagnose_u10 rejects DataFrames without a U10 column."""
        idx = pd.date_range("2023-07-01", periods=24, freq="h")
        df = pd.DataFrame({"UStar": np.full(24, 0.3)}, index=idx)
        with pytest.raises(ValueError, match="U10 not found"):
            diagnose_u10(df)


class TestQ2Attribution:
    """Tests for attribute_q2, diagnose_q2, and cal_r_eff_humidity."""

    def test_cal_r_eff_humidity_values_and_low_flux_nan(self):
        """cal_r_eff_humidity inverts q2 = q_ref + r * QE * gamma and masks low QE."""
        q_ref = np.array([0.008, 0.008, 0.008])
        qe = np.array([100.0, 0.05, 200.0])
        gamma = np.array([3.3e-7, 3.3e-7, 3.3e-7])
        r_true = np.array([45.0, 50.0, 60.0])
        q2 = q_ref + r_true * qe * gamma

        r_calc = cal_r_eff_humidity(q2, q_ref, qe, gamma, min_flux=0.1)

        assert r_calc[0] == pytest.approx(45.0, rel=1e-12)
        assert np.isnan(r_calc[1])
        assert r_calc[2] == pytest.approx(60.0, rel=1e-12)

    def test_attribute_q2_exact_closure_same_and_different_forcing(self):
        """q2 attribution closes exactly in g/kg for shared and differing forcing."""
        idx = pd.date_range("2023-07-01", periods=24, freq="h")
        phase = np.linspace(0.0, 2.0 * np.pi, len(idx))
        tair_a = 20.0 + 4.0 * np.sin(phase)
        rh_a = 55.0 - 10.0 * np.sin(phase)
        pres = np.full(len(idx), 1013.25)

        df_a, df_fc_a = _make_q2_scenario(
            idx,
            tair=tair_a,
            rh=rh_a,
            pres=pres,
            qe=80.0 + 30.0 * np.sin(phase),
            r_eff=np.full(len(idx), 50.0),
        )
        df_b, _ = _make_q2_scenario(
            idx,
            tair=tair_a,
            rh=rh_a,
            pres=pres,
            qe=130.0 + 30.0 * np.sin(phase),
            r_eff=np.full(len(idx), 60.0),
        )

        # Shared forcing: q_ref contribution is identically zero.
        res_same = attribute_q2(df_a, df_b, df_fc_a, df_fc_a)
        assert res_same.variable == "q2"
        assert res_same.metadata["unit"] == "g/kg"
        assert res_same.metadata["hierarchical"] is False
        assert res_same.metadata["n_low_flux_flagged"] == 0
        np.testing.assert_allclose(res_same.contributions["q_ref"], 0.0, atol=1e-14)
        # Higher QE and higher resistance increase q2.
        assert (res_same.contributions["flux_total"] > 0).all()
        assert (res_same.contributions["resistance"] > 0).all()
        # Verify kg/kg -> g/kg scaling.
        np.testing.assert_allclose(
            res_same.contributions["delta_total"],
            (df_b["q2"] - df_a["q2"]) * 1000.0,
            rtol=1e-12,
        )
        sum_same = res_same.contributions[
            ["q_ref", "flux_total", "resistance", "air_props"]
        ].sum(axis=1)
        np.testing.assert_allclose(
            sum_same, res_same.contributions["delta_total"], rtol=1e-10, atol=1e-12
        )

        # Different forcing (more humid background): q_ref is positive and closure holds.
        df_b2, df_fc_b2 = _make_q2_scenario(
            idx,
            tair=tair_a + 1.5,
            rh=rh_a + 8.0,
            pres=pres,
            qe=130.0 + 30.0 * np.sin(phase),
            r_eff=np.full(len(idx), 55.0),
        )
        res_diff = attribute_q2(df_a, df_b2, df_fc_a, df_fc_b2)
        assert (res_diff.contributions["q_ref"] > 0).all()
        sum_diff = res_diff.contributions[
            ["q_ref", "flux_total", "resistance", "air_props"]
        ].sum(axis=1)
        np.testing.assert_allclose(
            sum_diff, res_diff.contributions["delta_total"], rtol=1e-10, atol=1e-12
        )

    def test_attribute_q2_isolated_and_mixed_factor_attribution(self):
        """Isolated QE, isolated resistance, and mixed changes match analytical Shapley values."""
        idx = pd.date_range("2023-07-01", periods=6, freq="h")
        tair = np.full(6, 22.0)
        rh = np.full(6, 55.0)
        pres = np.full(6, 1013.25)
        gamma = np.asarray(cal_gamma_humidity(tair, rh, pres), dtype=float)
        scale = 1000.0  # kg/kg -> g/kg

        qe_a = np.full(6, 100.0)
        r_a = np.full(6, 50.0)
        df_base, df_fc = _make_q2_scenario(
            idx, tair=tair, rh=rh, pres=pres, qe=qe_a, r_eff=r_a
        )

        # 1. QE-only change: all attribution belongs to flux_total.
        qe_b = np.full(6, 160.0)
        df_qe_only, _ = _make_q2_scenario(
            idx, tair=tair, rh=rh, pres=pres, qe=qe_b, r_eff=r_a
        )
        res_qe = attribute_q2(df_base, df_qe_only, df_fc, df_fc)
        expected_flux_only = r_a * (qe_b - qe_a) * gamma * scale
        np.testing.assert_allclose(
            res_qe.contributions["flux_total"], expected_flux_only, rtol=1e-12
        )
        np.testing.assert_allclose(res_qe.contributions["resistance"], 0.0, atol=1e-14)
        np.testing.assert_allclose(res_qe.contributions["q_ref"], 0.0, atol=1e-14)
        np.testing.assert_allclose(res_qe.contributions["air_props"], 0.0, atol=1e-14)

        # 2. Resistance-only change: all attribution belongs to resistance.
        r_b = np.full(6, 70.0)
        df_r_only, _ = _make_q2_scenario(
            idx, tair=tair, rh=rh, pres=pres, qe=qe_a, r_eff=r_b
        )
        res_r = attribute_q2(df_base, df_r_only, df_fc, df_fc)
        expected_r_only = (r_b - r_a) * qe_a * gamma * scale
        np.testing.assert_allclose(
            res_r.contributions["resistance"], expected_r_only, rtol=1e-12
        )
        np.testing.assert_allclose(res_r.contributions["flux_total"], 0.0, atol=1e-14)
        np.testing.assert_allclose(res_r.contributions["q_ref"], 0.0, atol=1e-14)
        np.testing.assert_allclose(res_r.contributions["air_props"], 0.0, atol=1e-14)

        # 3. Mixed QE + resistance change with distinct magnitudes to prevent column swaps.
        df_mixed, _ = _make_q2_scenario(
            idx, tair=tair, rh=rh, pres=pres, qe=qe_b, r_eff=r_b
        )
        res_mix = attribute_q2(df_base, df_mixed, df_fc, df_fc)
        # With gamma unchanged, triple Shapley reduces to binary Shapley scaled by gamma:
        # Phi_QE = dQE * (r_A + r_B) / 2 * gamma; Phi_r = dr * (QE_A + QE_B) / 2 * gamma.
        expected_phi_qe = (qe_b - qe_a) * 0.5 * (r_a + r_b) * gamma * scale
        expected_phi_r = (r_b - r_a) * 0.5 * (qe_a + qe_b) * gamma * scale
        assert not np.allclose(expected_phi_qe, expected_phi_r)
        np.testing.assert_allclose(
            res_mix.contributions["flux_total"], expected_phi_qe, rtol=1e-12
        )
        np.testing.assert_allclose(
            res_mix.contributions["resistance"], expected_phi_r, rtol=1e-12
        )
        np.testing.assert_allclose(res_mix.contributions["q_ref"], 0.0, atol=1e-14)
        np.testing.assert_allclose(res_mix.contributions["air_props"], 0.0, atol=1e-14)

    def test_attribute_q2_low_flux_flag_and_oop_wrappers(self):
        """Low QE timesteps are flagged and SUEWSForcing/SUEWSOutput wrappers unwrap."""
        idx = pd.date_range("2023-07-01", periods=6, freq="h")
        tair = np.full(6, 22.0)
        rh = np.full(6, 60.0)
        pres = np.full(6, 1013.25)

        df_a, df_fc = _make_q2_scenario(
            idx,
            tair=tair,
            rh=rh,
            pres=pres,
            qe=np.array([0.02, 80.0, 90.0, 100.0, 110.0, 120.0]),
            r_eff=np.full(6, 50.0),
        )
        df_b, _ = _make_q2_scenario(
            idx,
            tair=tair,
            rh=rh,
            pres=pres,
            qe=np.array([50.0, 100.0, 110.0, 120.0, 130.0, 140.0]),
            r_eff=np.full(6, 50.0),
        )

        # Wrap inputs in real SUEWSForcing and SUEWSOutput objects.
        mi_cols = pd.MultiIndex.from_tuples(
            [("SUEWS", c) for c in df_a.columns], names=["group", "var"]
        )
        mi_idx = pd.MultiIndex.from_arrays(
            [np.ones(len(idx), dtype=int), idx], names=["grid", "datetime"]
        )
        out_a = SUEWSOutput(
            pd.DataFrame(df_a.to_numpy(), index=mi_idx, columns=mi_cols),
            df_state_final=pd.DataFrame(),
        )
        out_b = SUEWSOutput(
            pd.DataFrame(df_b.to_numpy(), index=mi_idx, columns=mi_cols),
            df_state_final=pd.DataFrame(),
        )
        fc_wrapped = SUEWSForcing(df_fc)

        result = attribute_q2(out_a, out_b, fc_wrapped, fc_wrapped, min_flux=0.1)

        assert result.metadata["n_low_flux_flagged"] == 1
        assert bool(result.contributions["flag_low_flux"].iloc[0]) is True
        assert np.isnan(result.contributions["resistance"].iloc[0])
        valid = ~result.contributions["flag_low_flux"]
        sum_valid = result.contributions.loc[
            valid, ["q_ref", "flux_total", "resistance", "air_props"]
        ].sum(axis=1)
        np.testing.assert_allclose(
            sum_valid,
            result.contributions.loc[valid, "delta_total"],
            rtol=1e-10,
            atol=1e-12,
        )

    @pytest.mark.parametrize("method", ["anomaly", "extreme", "diurnal"])
    def test_diagnose_q2_methods(self, method: str):
        """diagnose_q2 closes exactly across anomaly, extreme, and diurnal modes."""
        idx = pd.date_range("2023-07-01", periods=168, freq="h")
        hours = idx.hour.to_numpy()
        afternoon = np.where((hours >= 12) & (hours <= 15), 1.0, 0.0)
        tair = 18.0 + 4.0 * afternoon + 1.0 * np.sin(2.0 * np.pi * hours / 24.0)
        rh = 55.0 + 12.0 * afternoon
        pres = np.full(len(idx), 1013.25)
        qe = 60.0 + 90.0 * afternoon
        r_eff = np.full(len(idx), 55.0)
        df_out, df_fc = _make_q2_scenario(
            idx, tair=tair, rh=rh, pres=pres, qe=qe, r_eff=r_eff
        )

        result = diagnose_q2(df_out, df_fc, method=method, threshold=1.2)

        assert result.variable == "q2"
        assert list(result.contributions.index) == ["aggregate"]
        assert result.metadata["method"] == method
        assert result.metadata["aggregate_comparison"] is True
        assert result.metadata["unit"] == "g/kg"
        assert result.metadata["threshold"] == (1.2 if method == "anomaly" else None)

        row = result.contributions.loc["aggregate"]
        total_parts = (
            row["q_ref"] + row["flux_total"] + row["resistance"] + row["air_props"]
        )
        assert total_parts == pytest.approx(row["delta_total"], rel=1e-10, abs=1e-12)

    def test_diagnose_q2_missing_column_raises(self):
        """diagnose_q2 raises ValueError when q2 is absent."""
        idx = pd.date_range("2023-07-01", periods=24, freq="h")
        df_out = pd.DataFrame({"QE": np.full(24, 100.0)}, index=idx)
        df_fc = pd.DataFrame(
            {
                "Tair": np.full(24, 20.0),
                "RH": np.full(24, 50.0),
                "pres": np.full(24, 1013.25),
            },
            index=idx,
        )
        with pytest.raises(ValueError, match="q2 not found"):
            diagnose_q2(df_out, df_fc)


class TestAttributionDispatchersAndHelpers:
    """Tests for generic attribute()/diagnose() dispatchers and helper edge cases."""

    def test_generic_attribute_dispatches_all_variables_and_rejects_unknown(self):
        """attribute() matches variable-specific functions for T2, q2, U10."""
        idx = pd.date_range("2023-07-01", periods=12, freq="h")

        df_t2_a, df_fc_t2 = _make_t2_scenario(idx, t_offset=0.0, qh_scale=1.0)
        df_t2_b, _ = _make_t2_scenario(idx, t_offset=-1.0, qh_scale=0.85)
        res_t2 = attribute(
            df_t2_a,
            df_t2_b,
            variable="T2",
            df_forcing_A=df_fc_t2,
            df_forcing_B=df_fc_t2,
        )
        ref_t2 = attribute_t2(df_t2_a, df_t2_b, df_fc_t2, df_fc_t2)
        pd.testing.assert_frame_equal(res_t2.contributions, ref_t2.contributions)

        df_q2_a, df_fc_q2 = _make_q2_scenario(
            idx,
            tair=np.full(12, 20.0),
            rh=np.full(12, 55.0),
            pres=np.full(12, 1013.25),
            qe=np.full(12, 80.0),
            r_eff=np.full(12, 50.0),
        )
        df_q2_b, _ = _make_q2_scenario(
            idx,
            tair=np.full(12, 20.0),
            rh=np.full(12, 55.0),
            pres=np.full(12, 1013.25),
            qe=np.full(12, 120.0),
            r_eff=np.full(12, 50.0),
        )
        res_q2 = attribute(
            df_q2_a,
            df_q2_b,
            variable="q2",
            df_forcing_A=df_fc_q2,
            df_forcing_B=df_fc_q2,
        )
        ref_q2 = attribute_q2(df_q2_a, df_q2_b, df_fc_q2, df_fc_q2)
        pd.testing.assert_frame_equal(res_q2.contributions, ref_q2.contributions)

        df_u10_a = _make_u10_frame(
            idx,
            ustar=np.full(12, 0.3),
            z0m=np.full(12, 0.3),
            zd=np.full(12, 1.5),
            lob=np.full(12, -80.0),
        )
        df_u10_b = _make_u10_frame(
            idx,
            ustar=np.full(12, 0.4),
            z0m=np.full(12, 0.5),
            zd=np.full(12, 2.0),
            lob=np.full(12, -60.0),
        )
        res_u10 = attribute(df_u10_a, df_u10_b, variable="U10")
        ref_u10 = attribute_u10(df_u10_a, df_u10_b)
        pd.testing.assert_frame_equal(res_u10.contributions, ref_u10.contributions)

        with pytest.raises(ValueError, match="Unknown variable"):
            attribute(df_u10_a, df_u10_b, variable="RH2")  # type: ignore[arg-type]

    def test_generic_diagnose_dispatches_all_variables_and_rejects_unknown(self):
        """diagnose() matches variable-specific functions for T2, q2, U10."""
        idx = pd.date_range("2023-07-01", periods=168, freq="h")
        hours = idx.hour.to_numpy()
        afternoon = np.where((hours >= 12) & (hours <= 15), 1.0, 0.0)

        df_t2, df_fc_t2 = _make_t2_scenario(idx)
        res_t2 = diagnose(df_t2, variable="T2", df_forcing=df_fc_t2, method="diurnal")
        ref_t2 = diagnose_t2(df_t2, df_fc_t2, method="diurnal")
        pd.testing.assert_frame_equal(res_t2.contributions, ref_t2.contributions)

        df_q2, df_fc_q2 = _make_q2_scenario(
            idx,
            tair=18.0 + 3.0 * afternoon,
            rh=55.0 + 10.0 * afternoon,
            pres=np.full(len(idx), 1013.25),
            qe=70.0 + 60.0 * afternoon,
            r_eff=np.full(len(idx), 50.0),
        )
        res_q2 = diagnose(df_q2, variable="q2", df_forcing=df_fc_q2, method="diurnal")
        ref_q2 = diagnose_q2(df_q2, df_fc_q2, method="diurnal")
        pd.testing.assert_frame_equal(res_q2.contributions, ref_q2.contributions)

        df_u10 = _make_u10_frame(
            idx,
            ustar=0.3 + 0.2 * afternoon,
            z0m=np.full(len(idx), 0.3),
            zd=np.full(len(idx), 1.5),
            lob=np.where(afternoon > 0, -50.0, 60.0),
        )
        res_u10 = diagnose(df_u10, variable="U10", method="diurnal")
        ref_u10 = diagnose_u10(df_u10, method="diurnal")
        pd.testing.assert_frame_equal(res_u10.contributions, ref_u10.contributions)

        with pytest.raises(ValueError, match="Unknown variable"):
            diagnose(df_u10, variable="RH2")  # type: ignore[arg-type]

    def test_helpers_multiindex_fallback_alignment_warning_and_anomaly_errors(self):
        """Verify non-SUEWS MultiIndex fallback, unwrap_forcing, <90% warning, and detect_anomalies errors."""
        # MultiIndex columns without a 'SUEWS' top-level group fall back to droplevel(0).
        df_other = pd.DataFrame({
            ("Custom", "U10"): [2.0, 3.0],
            ("Custom", "UStar"): [0.2, 0.3],
        })
        extracted = extract_suews_group(df_other, required_cols={"U10", "UStar"})
        assert list(extracted.columns) == ["U10", "UStar"]

        # unwrap_forcing returns raw DataFrames and non-wrapper inputs intact.
        sentinel = {"not_a_dataframe": True}
        assert unwrap_forcing(sentinel) is sentinel

        # align_scenarios warns when overlap retains < 90% of either scenario.
        idx_a = pd.date_range("2023-07-01", periods=20, freq="h")
        idx_b = pd.date_range("2023-07-01 10:00", periods=20, freq="h")
        df_a = pd.DataFrame({"x": np.arange(20)}, index=idx_a)
        df_b = pd.DataFrame({"x": np.arange(20)}, index=idx_b)
        with pytest.warns(UserWarning, match="Significant data loss during alignment"):
            a_al, b_al, common = align_scenarios(df_a, df_b)
        assert len(common) == 10
        assert len(a_al) == len(b_al) == 10

        # detect_anomalies rejects unknown methods and too few reference points.
        ser = pd.Series(
            np.arange(24, dtype=float),
            index=pd.date_range("2023-07-01", periods=24, freq="h"),
        )
        with pytest.raises(ValueError, match="Unknown method"):
            detect_anomalies(ser, method="bogus")  # type: ignore[arg-type]

        # Only afternoon hours (12-15) present -> 0 morning reference timesteps.
        idx_afternoon = (
            pd
            .date_range("2023-07-01 12:00", periods=4, freq="h")
            .append(pd.date_range("2023-07-02 12:00", periods=4, freq="h"))
            .append(pd.date_range("2023-07-03 12:00", periods=4, freq="h"))
        )
        ser_afternoon = pd.Series(
            np.linspace(1.0, 5.0, len(idx_afternoon)), index=idx_afternoon
        )
        with pytest.raises(ValueError, match="reference timesteps"):
            detect_anomalies(ser_afternoon, method="diurnal")

    def test_flux_performance_missing_column_errors(self):
        """diagnose_flux_performance reports missing residual or required columns clearly."""
        idx = pd.date_range("2023-07-01", periods=4, freq="h")
        df_complete = pd.DataFrame(
            {
                "QN": [200.0] * 4,
                "QF": [20.0] * 4,
                "QS": [80.0] * 4,
                "QE": [50.0] * 4,
                "QH": [90.0] * 4,
            },
            index=idx,
        )
        # Missing QS and QF -> cannot derive QS from residual.
        with pytest.raises(ValueError, match="QS is required"):
            diagnose_flux_performance(
                df_complete.drop(columns=["QS", "QF"]), df_complete, variable="QE"
            )
        # Has QS, missing QF -> fails required-column check.
        with pytest.raises(ValueError, match="missing required columns"):
            diagnose_flux_performance(
                df_complete.drop(columns=["QF"]), df_complete, variable="QE"
            )


class TestAttributionResultFormattingAndPlotting:
    """Tests for AttributionResult.__repr__ and AttributionResult.plot."""

    def _make_multimonth_t2_result(self) -> AttributionResult:
        idx = pd.date_range("2023-06-30", periods=48, freq="h")
        df_a, df_fc = _make_t2_scenario(idx, t_offset=0.0, qh_scale=1.0)
        df_b, _ = _make_t2_scenario(idx, t_offset=-1.2, qh_scale=0.8)
        return attribute_t2(df_a, df_b, df_fc, df_fc, hierarchical=True)

    def test_repr_covers_hierarchical_t2_and_u10_formats(self):
        """__repr__ shows flux breakdown for hierarchical T2 and omits it for U10."""
        res_t2 = self._make_multimonth_t2_result()
        text_t2 = repr(res_t2)
        assert "T2 Attribution Results" in text_t2
        assert "Flux breakdown:" in text_t2
        assert "dQstar" in text_t2
        assert "Closure residual:" in text_t2

        idx = pd.date_range("2023-07-01", periods=6, freq="h")
        df_u = _make_u10_frame(
            idx,
            ustar=np.full(6, 0.35),
            z0m=np.full(6, 0.3),
            zd=np.full(6, 1.5),
            lob=np.full(6, -80.0),
        )
        # Identical scenarios -> zero delta_total exercises the zero-total % branch.
        res_u_zero = attribute_u10(df_u, df_u)
        text_u = repr(res_u_zero)
        assert "U10 Attribution Results" in text_u
        assert "Flux breakdown:" not in text_u
        assert "(  0.0%)" in text_u

    @pytest.mark.parametrize("kind", ["bar", "diurnal", "line", "heatmap"])
    def test_plot_all_kinds_render_expected_artists(self, kind: str):
        """All four plot kinds render expected titles, labels, and artists cleanly."""
        res = self._make_multimonth_t2_result()
        fig = None
        try:
            fig, ax = res.plot(kind=kind)
            assert "T2 Attribution" in ax.get_title()
            if kind == "bar":
                # Default T2 plot components: flux_total, resistance, air_props.
                assert len(ax.patches) == 3
                assert len(ax.texts) == 3
                assert "degC" in ax.get_ylabel()
            elif kind == "diurnal":
                # 3 component median lines + 1 zero reference line.
                assert len(ax.lines) == 4
                assert ax.get_xlabel() == "Hour of day"
            elif kind == "line":
                assert len(ax.lines) == 4
                assert ax.get_xlabel() == "Time"
            elif kind == "heatmap":
                assert len(ax.images) == 1
                assert ax.get_xlabel() == "Month"
                assert ax.get_ylabel() == "Hour"
                # Spans June and July -> 2 month columns.
                assert len(ax.get_xticks()) == 2
        finally:
            if fig is not None:
                plt.close(fig)

    def test_plot_accepts_existing_ax_and_custom_components(self):
        """plot() draws onto a caller-supplied Axes and respects custom components."""
        res = self._make_multimonth_t2_result()
        fig_in, ax_in = plt.subplots()
        try:
            fig_out, ax_out = res.plot(
                kind="bar", ax=ax_in, components=["flux_total", "resistance"]
            )
            assert fig_out is fig_in
            assert ax_out is ax_in
            assert len(ax_out.patches) == 2
        finally:
            plt.close(fig_in)

    @pytest.mark.parametrize("kind", ["diurnal", "heatmap"])
    def test_plot_rejects_non_datetime_index_for_diurnal_and_heatmap(self, kind: str):
        """Aggregate results raise ValueError for diurnal and heatmap plots."""
        idx = pd.date_range("2023-07-01", periods=168, freq="h")
        df_t2, df_fc = _make_t2_scenario(idx)
        res_agg = diagnose_t2(df_t2, df_fc, method="diurnal")

        fig_in, ax_in = plt.subplots()
        try:
            with pytest.raises(ValueError, match="DatetimeIndex"):
                res_agg.plot(kind=kind, ax=ax_in)
        finally:
            plt.close(fig_in)
