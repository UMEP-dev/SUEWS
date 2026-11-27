"""Tests for atmospheric stability, Obukhov length, and aerodynamic resistance in supy.util._atm."""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from supy.util._atm import cal_Lob, cal_psi_heat, cal_psi_mom, cal_ra_obs

pytestmark = [pytest.mark.unit, pytest.mark.contract("C-RESPONSE")]


class TestStabilityAndAerodynamicResistance:
    """Tests for stability corrections, Obukhov length, and aerodynamic resistance."""

    def test_cal_psi_mom_regimes_and_clamping(self):
        """cal_psi_mom is zero near neutral, negative when stable, positive when unstable, and clamped at |z/L|=5."""
        assert float(cal_psi_mom(0.0)) == pytest.approx(0.0)
        assert float(cal_psi_mom(5e-6)) == pytest.approx(0.0)
        assert float(cal_psi_mom(-5e-6)) == pytest.approx(0.0)

        zol_stab = 1.5
        assert float(cal_psi_mom(zol_stab)) == pytest.approx(
            -6.0 * np.log(1.0 + zol_stab), rel=1e-12
        )
        assert float(cal_psi_mom(zol_stab)) < 0.0

        zol_unstab = -1.5
        expected_unstab = 1.2 * np.log((1.0 + np.sqrt(1.0 - 16.0 * zol_unstab)) / 2.0)
        assert float(cal_psi_mom(zol_unstab)) == pytest.approx(
            expected_unstab, rel=1e-12
        )
        assert float(cal_psi_mom(zol_unstab)) > 0.0

        # Values outside [-5, 5] clamp to the +/-5 boundary.
        assert float(cal_psi_mom(12.0)) == pytest.approx(float(cal_psi_mom(5.0)))
        assert float(cal_psi_mom(-12.0)) == pytest.approx(float(cal_psi_mom(-5.0)))

        # Strictly decreasing across stability from unstable to stable.
        arr = cal_psi_mom(np.array([-4.0, -1.0, 0.0, 1.0, 4.0]))
        assert np.all(np.diff(arr) < 0)

    def test_cal_psi_heat_regimes_and_clamping(self):
        """cal_psi_heat matches analytical stable/unstable branches and clamps at |z/L|=5."""
        assert float(cal_psi_heat(0.0)) == pytest.approx(0.0)
        assert float(cal_psi_heat(1e-6)) == pytest.approx(0.0)

        zol_stab = 2.0
        assert float(cal_psi_heat(zol_stab)) == pytest.approx(
            -4.5 * zol_stab, rel=1e-12
        )

        zol_unstab = -2.0
        expected_unstab = 2.0 * np.log((1.0 + np.sqrt(1.0 - 16.0 * zol_unstab)) / 2.0)
        assert float(cal_psi_heat(zol_unstab)) == pytest.approx(
            expected_unstab, rel=1e-12
        )

        assert float(cal_psi_heat(9.0)) == pytest.approx(float(cal_psi_heat(5.0)))
        assert float(cal_psi_heat(-9.0)) == pytest.approx(float(cal_psi_heat(-5.0)))
        arr = cal_psi_heat(np.array([-4.0, -1.0, 0.0, 1.0, 4.0]))
        assert np.all(np.diff(arr) < 0)

    def test_cal_lob_sign_and_scaling(self):
        """cal_Lob is negative for positive QH, positive for negative QH, and scales as u*^3 / QH."""
        lob_unstab = float(cal_Lob(200.0, 0.4, 20.0, 50.0, 1013.25).iloc[0])
        lob_stab = float(cal_Lob(-100.0, 0.4, 20.0, 50.0, 1013.25).iloc[0])
        assert lob_unstab < 0.0
        assert lob_stab > 0.0

        # Doubling u* multiplies Lob by 2^3 = 8; doubling QH halves Lob.
        lob_2u = float(cal_Lob(200.0, 0.8, 20.0, 50.0, 1013.25).iloc[0])
        lob_2qh = float(cal_Lob(400.0, 0.4, 20.0, 50.0, 1013.25).iloc[0])
        assert lob_2u == pytest.approx(8.0 * lob_unstab, rel=1e-12)
        assert lob_2qh == pytest.approx(0.5 * lob_unstab, rel=1e-12)

        # Series inputs preserve index alignment.
        idx = pd.date_range("2023-07-01", periods=3, freq="h")
        qh = pd.Series([150.0, 200.0, -80.0], index=idx)
        ustar = pd.Series([0.3, 0.4, 0.3], index=idx)
        ta = pd.Series([20.0, 22.0, 15.0], index=idx)
        rh = pd.Series([50.0, 45.0, 70.0], index=idx)
        pres = pd.Series([1013.25, 1013.25, 1013.25], index=idx)
        ser_lob = cal_Lob(qh, ustar, ta, rh, pres)
        assert ser_lob.index.equals(idx)
        assert ser_lob.iloc[0] < 0.0
        assert ser_lob.iloc[1] < 0.0
        assert ser_lob.iloc[2] > 0.0

    def test_cal_ra_obs_neutral_limit_and_physical_ordering(self):
        """cal_ra_obs matches the neutral log-law limit and orders unstable < neutral < stable."""
        zm, zd, z0m, z0v, ws = 20.0, 5.0, 0.5, 0.05, 4.0
        k = 0.4
        zmd = zm - zd

        ra_neutral = float(cal_ra_obs(zm, zd, z0m, z0v, ws, Lob=1e9))
        expected_neutral = np.log(zmd / z0m) * np.log(zmd / z0v) / (k**2 * ws)
        assert ra_neutral == pytest.approx(expected_neutral, rel=1e-6)

        # Inversely proportional to wind speed.
        ra_double_ws = float(cal_ra_obs(zm, zd, z0m, z0v, 2.0 * ws, Lob=1e9))
        assert ra_double_ws == pytest.approx(0.5 * ra_neutral, rel=1e-12)

        # Unstable mixing reduces resistance; stable stratification increases it.
        ra_unstable = float(cal_ra_obs(zm, zd, z0m, z0v, ws, Lob=-50.0))
        ra_stable = float(cal_ra_obs(zm, zd, z0m, z0v, ws, Lob=50.0))
        assert 0.0 < ra_unstable < ra_neutral < ra_stable

        # Rougher surface has lower aerodynamic resistance.
        ra_rough = float(cal_ra_obs(zm, zd, z0m=1.0, z0v=0.1, ws=ws, Lob=1e9))
        assert ra_rough < ra_neutral
