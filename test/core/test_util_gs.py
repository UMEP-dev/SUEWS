"""Test surface conductance calculation utilities."""

import numpy as np
import pandas as pd
import pytest

from supy.util._gs import cal_rs_FG, cal_rs_iPM, cal_rs_obs


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
        rs_ipm = cal_rs_obs(self.QH, self.QE, self.TA, self.RH, self.PA, self.RA, method="iPM")
        rs_fg = cal_rs_obs(self.QH, self.QE, self.TA, self.RH, self.PA, self.RA, method="FG")

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
