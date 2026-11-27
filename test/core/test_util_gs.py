"""Test surface conductance calculation utilities."""

from lmfit import Parameters
from lmfit.model import ModelResult
import numpy as np
import pandas as pd
import pytest

from supy.util._atm import cal_dq
from supy.util._gs import (
    cal_g_dq,
    cal_g_dq_noah,
    cal_g_kd,
    cal_g_kd_noah,
    cal_g_lai,
    cal_g_smd,
    cal_g_swc_noah,
    cal_g_ta,
    cal_g_ta_noah,
    cal_gs_obs,
    cal_gs_suews,
    cal_rs_FG,
    cal_rs_iPM,
    cal_rs_obs,
    calib_g,
    deriv_g_dq_noah,
    deriv_g_kd_noah,
    deriv_g_smd_noah,
    deriv_g_ta_noah,
    fit_g_dq,
    fit_g_kd,
    fit_g_smd,
    fit_g_ta,
)

pytestmark = [pytest.mark.api, pytest.mark.contract("C-RESPONSE")]


class TestSurfaceConductance:
    """Test surface conductance/resistance calculation functions."""

    # Standard test conditions
    QH = 200.0  # Sensible heat flux [W m-2]
    QE = 150.0  # Latent heat flux [W m-2]
    TA = 25.0  # Air temperature [°C]
    RH = 60.0  # Relative humidity [%]
    PA = 101325.0  # Air pressure [Pa]
    RA = 50.0  # Aerodynamic resistance [s m-1]

    def test_method_comparison(self):
        """Test iPM and FG methods give consistent, reasonable results."""
        rs_ipm = cal_rs_obs(
            self.QH, self.QE, self.TA, self.RH, self.PA, self.RA, method="iPM"
        )
        rs_fg = cal_rs_obs(
            self.QH, self.QE, self.TA, self.RH, self.PA, self.RA, method="FG"
        )

        # Both should give positive, reasonable values
        assert rs_ipm > 0, "iPM method gave non-positive rs"
        assert rs_fg > 0, "FG method gave non-positive rs"
        assert rs_ipm < 5000, "iPM result unreasonably high"
        assert rs_fg < 5000, "FG result unreasonably high"

    def test_series_input(self):
        """Test with pandas Series (diurnal cycle)."""
        hours = np.arange(0, 24)
        idx = pd.date_range("2023-07-01", periods=24, freq="h")

        # Diurnal patterns
        qh = pd.Series(200 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx)
        qe = pd.Series(150 * np.maximum(0, np.sin(np.pi * (hours - 6) / 12)), index=idx)
        ta = pd.Series(20 + 10 * np.sin(np.pi * (hours - 6) / 12), index=idx)
        rh = pd.Series(70 - 20 * np.sin(np.pi * (hours - 6) / 12), index=idx)
        pa = pd.Series(self.PA * np.ones(24), index=idx)
        ra = pd.Series(50 * np.ones(24), index=idx)

        rs = cal_rs_obs(qh, qe, ta, rh, pa, ra)

        assert isinstance(rs, pd.Series)
        assert len(rs) == 24

    def test_zero_latent_heat_raises(self):
        """Test ZeroDivisionError when QE=0 (surface completely closed)."""
        with pytest.raises(ZeroDivisionError):
            cal_rs_obs(self.QH, 0.0, self.TA, self.RH, self.PA, self.RA)

    @pytest.mark.parametrize(
        "qh,qe,ta,rh,description",
        [
            (500, 50, 35, 30, "high_bowen_ratio"),
            (50, 500, 25, 80, "low_bowen_ratio"),
            (200, 150, 20, 20, "dry_conditions"),
            (200, 150, 20, 90, "humid_conditions"),
            (10, -20, 15, 95, "dew_formation"),
        ],
    )
    def test_edge_conditions(self, qh, qe, ta, rh, description):
        """Test various edge conditions give valid results."""
        rs = cal_rs_obs(qh, qe, ta, rh, self.PA, self.RA)
        assert rs is not None, f"Failed for {description}"

    def test_cal_gs_obs_is_reciprocal_resistance_in_mm_s(self):
        """cal_gs_obs returns 1000 / cal_rs_obs for both scalar and Series inputs."""
        rs_scalar = cal_rs_obs(self.QH, self.QE, self.TA, self.RH, self.PA, self.RA)
        gs_scalar = cal_gs_obs(self.QH, self.QE, self.TA, self.RH, self.PA, self.RA)
        assert gs_scalar == pytest.approx(1000.0 / rs_scalar, rel=1e-12)

        idx = pd.date_range("2023-07-01 09:00", periods=6, freq="h")
        qh = pd.Series(np.linspace(120.0, 240.0, 6), index=idx)
        qe = pd.Series(np.linspace(90.0, 180.0, 6), index=idx)
        ta = pd.Series(np.linspace(20.0, 28.0, 6), index=idx)
        rh = pd.Series(np.linspace(65.0, 45.0, 6), index=idx)
        pa = pd.Series(np.full(6, self.PA), index=idx)
        ra = pd.Series(np.full(6, self.RA), index=idx)

        rs_fg_series = cal_rs_FG(qh, qe, ta, rh, pa, ra)
        assert isinstance(rs_fg_series, pd.Series)
        assert rs_fg_series.index.equals(idx)
        assert (rs_fg_series > 0).all()

        gs_series = cal_gs_obs(qh, qe, ta, rh, pa, ra)
        rs_ipm_series = cal_rs_iPM(qh, qe, ta, rh, pa, ra)
        pd.testing.assert_series_equal(gs_series, 1000.0 / rs_ipm_series)

    def test_cal_rs_obs_invalid_method_raises(self):
        """Unknown surface-resistance method raises KeyError."""
        with pytest.raises(KeyError):
            cal_rs_obs(
                self.QH, self.QE, self.TA, self.RH, self.PA, self.RA, method="unknown"
            )


class TestSurfaceConductanceFactors:
    """Analytical boundary and monotonicity checks for SUEWS g_* modifiers."""

    def test_cal_g_lai_linear_scaling(self):
        """cal_g_lai scales linearly with LAI from 0 to g_lai at lai_max."""
        assert cal_g_lai(0.0, g_lai=3.5, lai_max=5.0) == pytest.approx(0.0)
        assert cal_g_lai(5.0, g_lai=3.5, lai_max=5.0) == pytest.approx(3.5)
        assert cal_g_lai(2.5, g_lai=3.5, lai_max=5.0) == pytest.approx(1.75)

    def test_cal_g_kd_bounds_and_monotonicity(self):
        """cal_g_kd is 0 at kd=0, 1 at kd=kd_max, and strictly increasing."""
        assert cal_g_kd(0.0, g_kd=100.0, kd_max=1200.0) == pytest.approx(0.0)
        assert cal_g_kd(1200.0, g_kd=100.0, kd_max=1200.0) == pytest.approx(1.0)
        kd_vals = np.array([0.0, 100.0, 300.0, 600.0, 1200.0])
        g_vals = cal_g_kd(kd_vals, g_kd=100.0, kd_max=1200.0)
        assert np.all(np.diff(g_vals) > 0)

    def test_cal_g_dq_bounds_and_decay(self):
        """cal_g_dq equals 1 at dq=0 and decays monotonically toward g_dq_base."""
        assert cal_g_dq(0.0, g_dq_base=0.1, g_dq_shape=0.7) == pytest.approx(1.0)
        dq_vals = np.array([0.0, 2.0, 5.0, 10.0, 20.0])
        g_vals = cal_g_dq(dq_vals, g_dq_base=0.1, g_dq_shape=0.7)
        assert np.all(np.diff(g_vals) < 0)
        assert g_vals[-1] > 0.1
        assert g_vals[-1] < 0.11

    def test_cal_g_ta_peak_boundaries_and_exponent_cap(self):
        """cal_g_ta peaks at 1.0 when ta_c == g_ta, vanishes at tl/th, and caps tc."""
        assert cal_g_ta(25.0, g_ta=25.0, tl=-10.0, th=55.0) == pytest.approx(1.0)
        assert cal_g_ta(-10.0, g_ta=25.0, tl=-10.0, th=55.0) == pytest.approx(0.0)
        assert cal_g_ta(55.0, g_ta=25.0, tl=-10.0, th=55.0) == pytest.approx(0.0)
        assert cal_g_ta(10.0, g_ta=25.0) < 1.0
        assert cal_g_ta(40.0, g_ta=25.0) < 1.0
        # When g_ta is very close to tl, tc = (th - g_ta) / (g_ta - tl) = 64 is capped at 20.
        ta_off = 10.0
        g_ta_near_tl = -9.0
        tl, th = -10.0, 55.0
        expected_capped = ((ta_off - tl) * (th - ta_off) ** 20) / (
            (g_ta_near_tl - tl) * (th - g_ta_near_tl) ** 20
        )
        expected_uncapped = ((ta_off - tl) * (th - ta_off) ** 64) / (
            (g_ta_near_tl - tl) * (th - g_ta_near_tl) ** 64
        )
        g_capped = cal_g_ta(ta_off, g_ta=g_ta_near_tl, tl=tl, th=th)
        assert g_capped == pytest.approx(expected_capped, rel=1e-12)
        assert g_capped > 1e4 * expected_uncapped

    def test_cal_g_smd_bounds_and_monotonicity(self):
        """cal_g_smd equals 1 at smd=0, 0 at wp_smd, and decreases with smd."""
        assert cal_g_smd(0.0, g_smd=0.05, wp_smd=150.0) == pytest.approx(1.0)
        assert cal_g_smd(150.0, g_smd=0.05, wp_smd=150.0) == pytest.approx(0.0)
        smd_vals = np.array([0.0, 30.0, 60.0, 100.0, 150.0])
        g_vals = cal_g_smd(smd_vals, g_smd=0.05, wp_smd=150.0)
        assert np.all(np.diff(g_vals) < 0)


class TestSUEWSConductanceModel:
    """Behavioural and multiplicative-identity tests for cal_gs_suews."""

    G_CST = (3.5, 100.0, 0.1, 0.7, 25.0, 0.05)

    def test_cal_gs_suews_debug_dataframe_and_multiplicative_identity(self):
        """debug=True returns component DataFrame satisfying gs_mod == g_max * prod(g_i)."""
        idx = pd.date_range("2023-07-01 08:00", periods=6, freq="h")
        kd = pd.Series(np.linspace(150.0, 800.0, 6), index=idx)
        ta_c = pd.Series(np.linspace(18.0, 28.0, 6), index=idx)
        rh = pd.Series(np.linspace(70.0, 45.0, 6), index=idx)
        pa = pd.Series(np.full(6, 101325.0), index=idx)
        smd = pd.Series(np.linspace(10.0, 40.0, 6), index=idx)
        lai = pd.Series(np.full(6, 3.0), index=idx)
        g_max = 10.0

        gs_plain = cal_gs_suews(
            kd,
            ta_c,
            rh,
            pa,
            smd,
            lai,
            self.G_CST,
            g_max=g_max,
            lai_max=5.0,
            wp_smd=150.0,
        )
        df_dbg = cal_gs_suews(
            kd,
            ta_c,
            rh,
            pa,
            smd,
            lai,
            self.G_CST,
            g_max=g_max,
            lai_max=5.0,
            wp_smd=150.0,
            debug=True,
        )

        pd.testing.assert_series_equal(df_dbg["gs_mod"], gs_plain, check_names=False)
        prod_factors = (
            df_dbg["g_lai"]
            * df_dbg["g_kd"]
            * df_dbg["g_dq"]
            * df_dbg["g_ta"]
            * df_dbg["g_smd"]
        )
        np.testing.assert_allclose(df_dbg["gs_c"], prod_factors, rtol=1e-12)
        np.testing.assert_allclose(df_dbg["gs_mod"], g_max * df_dbg["gs_c"], rtol=1e-12)

    def test_cal_gs_suews_physical_sensitivities(self):
        """Conductance rises with Kdown and LAI and falls with drier soil or air."""
        base = cal_gs_suews(
            kd=400.0,
            ta_c=24.0,
            rh=60.0,
            pa=101325.0,
            smd=30.0,
            lai=3.0,
            g_cst=self.G_CST,
            g_max=10.0,
            lai_max=5.0,
            wp_smd=150.0,
        )
        higher_kd = cal_gs_suews(
            kd=800.0,
            ta_c=24.0,
            rh=60.0,
            pa=101325.0,
            smd=30.0,
            lai=3.0,
            g_cst=self.G_CST,
            g_max=10.0,
            lai_max=5.0,
            wp_smd=150.0,
        )
        higher_lai = cal_gs_suews(
            kd=400.0,
            ta_c=24.0,
            rh=60.0,
            pa=101325.0,
            smd=30.0,
            lai=4.5,
            g_cst=self.G_CST,
            g_max=10.0,
            lai_max=5.0,
            wp_smd=150.0,
        )
        drier_soil = cal_gs_suews(
            kd=400.0,
            ta_c=24.0,
            rh=60.0,
            pa=101325.0,
            smd=110.0,
            lai=3.0,
            g_cst=self.G_CST,
            g_max=10.0,
            lai_max=5.0,
            wp_smd=150.0,
        )
        drier_air = cal_gs_suews(
            kd=400.0,
            ta_c=24.0,
            rh=25.0,
            pa=101325.0,
            smd=30.0,
            lai=3.0,
            g_cst=self.G_CST,
            g_max=10.0,
            lai_max=5.0,
            wp_smd=150.0,
        )
        assert higher_kd > base
        assert higher_lai > base
        assert drier_soil < base
        assert drier_air < base


class TestConductanceFittingAndCalibration:
    """Parameter-recovery tests for fit_g_* helpers and calib_g."""

    def test_fit_g_ta_recovers_optimum_temperature(self):
        """fit_g_ta recovers the true optimum temperature from synthetic data."""
        ta = pd.Series(np.linspace(-10.0, 55.0, 66))
        gs = pd.Series(cal_g_ta(ta, g_ta=22.0, tl=-10.0, th=55.0))
        res = fit_g_ta(ta, gs)
        assert res.success
        assert res.best_values["g_ta"] == pytest.approx(22.0, abs=0.05)

    def test_fit_g_smd_recovers_smd_parameter(self):
        """fit_g_smd recovers the true g_smd decay parameter."""
        wp_smd = 150.0
        smd = pd.Series(np.linspace(0.0, wp_smd, 50))
        gs = pd.Series(cal_g_smd(smd, g_smd=0.04, wp_smd=wp_smd))
        res = fit_g_smd(smd, gs, wp_smd=wp_smd)
        assert res.success
        assert res.best_values["g_smd"] == pytest.approx(0.04, rel=1e-3)

    def test_fit_g_kd_recovers_radiation_parameter(self):
        """fit_g_kd recovers the true g_kd half-saturation parameter."""
        kd = pd.Series(np.linspace(10.0, 1200.0, 60))
        gs = pd.Series(cal_g_kd(kd, g_kd=120.0))
        res = fit_g_kd(kd, gs)
        assert res.success
        assert res.best_values["g_kd"] == pytest.approx(120.0, rel=1e-3)

    def test_fit_g_dq_recovers_humidity_shape_parameter(self):
        """fit_g_dq recovers the true humidity-deficit decay shape parameter."""
        dq = pd.Series(np.linspace(0.0, 20.0, 60))
        gs = pd.Series(cal_g_dq(dq, g_dq_base=0.1, g_dq_shape=0.75))
        res = fit_g_dq(dq, gs)
        assert res.success
        assert res.best_values["g_dq_shape"] == pytest.approx(0.75, rel=1e-2)

    def test_calib_g_with_explicit_parameters_debug_and_best_values(self):
        """calib_g drops NaN rows, fits parameters within bounds, and supports debug=True."""
        idx = pd.date_range("2023-07-01 07:00", periods=13, freq="h")
        df_fc = pd.DataFrame(
            {
                "qh": np.linspace(80.0, 220.0, 13),
                "qe": np.linspace(60.0, 180.0, 13),
                "Tair": np.linspace(18.0, 28.0, 13),
                "RH": np.linspace(70.0, 45.0, 13),
                "pres": np.full(13, 1013.25),
                "kdown": np.linspace(100.0, 800.0, 13),
                "xsmd": np.linspace(10.0, 40.0, 13),
                "lai": np.full(13, 3.0),
            },
            index=idx,
        )
        # Introduce one missing observation row to verify dropna filtering.
        df_fc.loc[idx[-1], "qe"] = np.nan
        ser_ra = pd.Series(np.full(13, 50.0), index=idx)

        prms = Parameters()
        prms.add("g_lai", value=3.5, min=0.1, max=10.0, vary=True)
        prms.add("g_kd", value=80.0, min=5.0, max=300.0, vary=True)
        prms.add("g_dq_base", value=0.1, min=0.0, max=1.0, vary=False)
        prms.add("g_dq_shape", value=0.7, min=0.01, max=0.99, vary=False)
        prms.add("g_ta", value=25.0, min=-10.0, max=55.0, vary=False)
        prms.add("g_smd", value=0.05, min=0.02, max=0.1, vary=False)

        best_vals = calib_g(
            df_fc,
            ser_ra,
            g_max=10.0,
            lai_max=5.0,
            wp_smd=150.0,
            method="leastsq",
            prms_init=prms,
            debug=False,
        )
        assert isinstance(best_vals, dict)
        assert set(best_vals) == {
            "g_lai",
            "g_kd",
            "g_dq_base",
            "g_dq_shape",
            "g_ta",
            "g_smd",
        }
        assert 0.1 <= best_vals["g_lai"] <= 10.0
        assert 5.0 <= best_vals["g_kd"] <= 300.0

        res_dbg = calib_g(
            df_fc,
            ser_ra,
            g_max=10.0,
            lai_max=5.0,
            wp_smd=150.0,
            method="leastsq",
            prms_init=prms,
            debug=True,
        )
        assert isinstance(res_dbg, ModelResult)
        assert res_dbg.success
        assert res_dbg.ndata == 12
        for key, val in best_vals.items():
            assert res_dbg.best_values[key] == pytest.approx(val, rel=1e-12)

    @staticmethod
    def _build_synthetic_calib_dataset():
        """Build synthetic daytime forcing and exact Jarvis conductance targets."""
        n_obs = 48
        idx = pd.date_range("2023-07-01 06:00", periods=n_obs, freq="h")
        phase = np.linspace(0.0, 2.0 * np.pi, n_obs, endpoint=False)

        kdown = pd.Series(500.0 + 350.0 * np.sin(phase), index=idx)
        ta_c = pd.Series(22.0 + 9.0 * np.sin(2.0 * phase + 0.4), index=idx)
        rh = pd.Series(60.0 + 22.0 * np.cos(3.0 * phase + 0.2), index=idx)
        pres_hpa = pd.Series(np.full(n_obs, 1013.25), index=idx)
        pres_pa = pres_hpa * 100.0
        xsmd = pd.Series(50.0 + 40.0 * np.sin(phase / 2.0 + 0.7), index=idx)
        lai = pd.Series(3.0 + 1.2 * np.cos(phase + 0.3), index=idx)
        ser_ra = pd.Series(np.full(n_obs, 50.0), index=idx)

        g_max, lai_max, wp_smd = 10.0, 5.0, 150.0
        g_true = {
            "g_lai": 4.0,
            "g_kd": 90.0,
            "g_dq_base": 0.15,
            "g_dq_shape": 0.72,
            "g_ta": 23.0,
            "g_smd": 0.045,
        }

        # Independent analytical Jarvis conductance oracle (avoids calling cal_gs_suews)
        # so parameter-slot wiring is verified end-to-end against distinct asymmetric values.
        dq = cal_dq(rh, ta_c, pres_hpa) * 1000.0
        f_lai = (lai / lai_max) * g_true["g_lai"]
        f_kd = (kdown / (g_true["g_kd"] + kdown)) / (1200.0 / (g_true["g_kd"] + 1200.0))
        f_dq = g_true["g_dq_base"] + (1.0 - g_true["g_dq_base"]) * (
            g_true["g_dq_shape"] ** dq
        )
        tl, th = -10.0, 55.0
        tc = min((th - g_true["g_ta"]) / (g_true["g_ta"] - tl), 20.0)
        f_ta = ((ta_c - tl) * (th - ta_c) ** tc) / (
            (g_true["g_ta"] - tl) * (th - g_true["g_ta"]) ** tc
        )
        f_smd = (1.0 - np.exp(g_true["g_smd"] * (xsmd - wp_smd))) / (
            1.0 - np.exp(-g_true["g_smd"] * wp_smd)
        )
        gs_true = g_max * f_lai * f_kd * f_dq * f_ta * f_smd

        # Solve for qh using the affine linearity of cal_rs_obs in qh without
        # duplicating its internal Penman-Monteith implementation.
        rs_true = 1000.0 / gs_true
        qe = pd.Series(np.full(n_obs, 150.0), index=idx)
        rs_at_qh0 = cal_rs_obs(pd.Series(0.0, index=idx), qe, ta_c, rh, pres_pa, ser_ra)
        rs_at_qh1 = cal_rs_obs(pd.Series(1.0, index=idx), qe, ta_c, rh, pres_pa, ser_ra)
        qh = (rs_true - rs_at_qh0) / (rs_at_qh1 - rs_at_qh0)

        np.testing.assert_allclose(
            cal_gs_obs(qh, qe, ta_c, rh, pres_pa, ser_ra),
            gs_true,
            rtol=1e-10,
        )

        df_fc = pd.DataFrame(
            {
                "qh": qh,
                "qe": qe,
                "Tair": ta_c,
                "RH": rh,
                "pres": pres_hpa,
                "kdown": kdown,
                "xsmd": xsmd,
                "lai": lai,
            },
            index=idx,
        )
        # Drop one observation to exercise NaN filtering on the default-prms path.
        df_fc.loc[idx[-1], "qe"] = np.nan
        return df_fc, ser_ra, gs_true.iloc[:-1], g_true, (g_max, lai_max, wp_smd)

    def test_calib_g_default_preset_parameters_converges_on_synthetic_data(self):
        """calib_g(..., prms_init=None) initialises g_dq_base/g_dq_shape and uses safe bounds."""
        df_fc, ser_ra, gs_expected, g_true, (g_max, lai_max, wp_smd) = (
            self._build_synthetic_calib_dataset()
        )
        expected_params = set(g_true)

        best_vals = calib_g(
            df_fc,
            ser_ra,
            g_max=g_max,
            lai_max=lai_max,
            wp_smd=wp_smd,
            prms_init=None,
            debug=False,
        )
        assert isinstance(best_vals, dict)
        assert set(best_vals) == expected_params

        res_dbg = calib_g(
            df_fc,
            ser_ra,
            g_max=g_max,
            lai_max=lai_max,
            wp_smd=wp_smd,
            prms_init=None,
            debug=True,
        )
        assert isinstance(res_dbg, ModelResult)
        assert res_dbg.success
        assert res_dbg.ndata == len(gs_expected)
        assert set(res_dbg.params) == expected_params
        assert "g_q1" not in res_dbg.params
        assert "g_q2" not in res_dbg.params

        for name, val in best_vals.items():
            prm = res_dbg.params[name]
            assert prm.min <= val <= prm.max
            assert res_dbg.best_values[name] == pytest.approx(val, rel=1e-12)
        assert res_dbg.params["g_dq_base"].min == pytest.approx(0.0)
        assert res_dbg.params["g_dq_base"].max == pytest.approx(1.0)
        assert res_dbg.params["g_dq_shape"].min == pytest.approx(0.0)
        assert res_dbg.params["g_dq_shape"].max == pytest.approx(1.0)
        assert res_dbg.init_values["g_dq_base"] == pytest.approx(0.1)
        assert res_dbg.init_values["g_dq_shape"] == pytest.approx(0.7)

        # Default bounds must stay away from singular points (g_kd=0 at kd=0; g_ta=tl/th).
        assert res_dbg.params["g_kd"].min > 0.0
        assert -10.0 < res_dbg.params["g_ta"].min < res_dbg.params["g_ta"].max < 55.0
        param_order = ("g_lai", "g_kd", "g_dq_base", "g_dq_shape", "g_ta", "g_smd")
        for bound_attr in ("min", "max"):
            g_bound = [
                getattr(res_dbg.params[name], bound_attr) for name in param_order
            ]
            gs_edge = cal_gs_suews(
                kd=0.0,
                ta_c=20.0,
                rh=100.0,
                pa=101325.0,
                smd=0.0,
                lai=2.0,
                g_cst=g_bound,
                g_max=g_max,
                lai_max=lai_max,
                wp_smd=wp_smd,
            )
            assert np.isfinite(gs_edge)

        # Derivative-free COBYLA terminates at its default trust-region tolerance
        # (~3% max relative residual on this 6-parameter problem); 5% allows minor
        # platform/SciPy variation while catching misconfigured models.
        np.testing.assert_allclose(res_dbg.best_fit, gs_expected, rtol=5e-2)

    def test_calib_g_default_preset_recovers_all_parameters_via_leastsq(self):
        """calib_g(..., prms_init=None, method='leastsq') recovers all six asymmetric oracle parameters."""
        df_fc, ser_ra, gs_expected, g_true, (g_max, lai_max, wp_smd) = (
            self._build_synthetic_calib_dataset()
        )
        res_lsq = calib_g(
            df_fc,
            ser_ra,
            g_max=g_max,
            lai_max=lai_max,
            wp_smd=wp_smd,
            method="leastsq",
            prms_init=None,
            debug=True,
        )
        assert res_lsq.success
        assert res_lsq.ndata == len(gs_expected)
        for name, true_val in g_true.items():
            assert res_lsq.best_values[name] == pytest.approx(true_val, rel=1e-6)
        np.testing.assert_allclose(res_lsq.best_fit, gs_expected, rtol=1e-6)


class TestNoahParameterisations:
    """Tests for NOAH conductance functions and SUEWS parameter derivations."""

    def test_cal_g_kd_noah_limits_and_monotonicity(self):
        """cal_g_kd_noah starts at rc_min/rc_max at kd=0 and increases toward 1."""
        assert cal_g_kd_noah(0.0, rc_min=200.0, rc_max=5000.0) == pytest.approx(
            200.0 / 5000.0
        )
        vals = cal_g_kd_noah(np.array([0.0, 50.0, 200.0, 800.0, 1200.0]))
        assert np.all(np.diff(vals) > 0)
        assert vals[-1] < 1.0

    def test_cal_g_dq_noah_bounds_and_decay(self):
        """cal_g_dq_noah equals 1 at dq=0 and decreases with dq."""
        assert cal_g_dq_noah(0.0, hs=31.0) == pytest.approx(1.0)
        assert cal_g_dq_noah(0.01, hs=31.0) == pytest.approx(1.0 / (1.0 + 0.31))
        vals = cal_g_dq_noah(np.array([0.0, 0.005, 0.01, 0.02]), hs=31.0)
        assert np.all(np.diff(vals) < 0)

    def test_cal_g_swc_noah_linear_bounds(self):
        """cal_g_swc_noah is 0 at q_wilt, 1 at q_ref, and linear in between."""
        assert cal_g_swc_noah(0.1, q_wilt=0.1, q_ref=0.35) == pytest.approx(0.0)
        assert cal_g_swc_noah(0.35, q_wilt=0.1, q_ref=0.35) == pytest.approx(1.0)
        assert cal_g_swc_noah(0.225, q_wilt=0.1, q_ref=0.35) == pytest.approx(0.5)

    def test_cal_g_ta_noah_symmetric_parabola(self):
        """cal_g_ta_noah peaks at 1.0 at tref and is symmetric around tref."""
        tref = 25.0
        assert cal_g_ta_noah(tref, b1=0.0016, tref=tref) == pytest.approx(1.0)
        assert cal_g_ta_noah(tref - 10.0, b1=0.0016, tref=tref) == pytest.approx(
            cal_g_ta_noah(tref + 10.0, b1=0.0016, tref=tref)
        )
        assert cal_g_ta_noah(tref - 10.0, b1=0.0016, tref=tref) == pytest.approx(
            1.0 - 0.16
        )

    def test_noah_to_suews_derivation_wrappers(self):
        """deriv_g_*_noah wrappers converge to physically valid SUEWS parameters."""
        res_ta = deriv_g_ta_noah(0.0016)
        assert res_ta.success
        assert 15.0 < res_ta.best_values["g_ta"] < 35.0

        res_dq = deriv_g_dq_noah(31.0)
        assert res_dq.success
        assert 0.01 <= res_dq.best_values["g_dq_shape"] <= 0.95
        assert res_dq.redchi < 1e-2

        res_kd = deriv_g_kd_noah(rgl=30.0, lai=2.0, rc_min=200.0)
        assert res_kd.success
        assert 10.0 <= res_kd.best_values["g_kd"] <= 300.0
        assert res_kd.redchi < 1e-3

        res_smd = deriv_g_smd_noah(dsoil=150.0, q_wilt=0.1, q_ref=0.35)
        assert res_smd.success
        assert res_smd.best_values["wp_smd"] == pytest.approx(150.0 * 0.25)
        assert res_smd.redchi < 1e-3
