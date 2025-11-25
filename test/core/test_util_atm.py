"""Test atmospheric calculation utilities."""

import numpy as np
import pandas as pd
import pytest

from supy.util._atm import cal_des_dta, cal_dq, cal_lat_vap, cal_qa, cal_rh


class TestAtmosphericCalculations:
    """Test atmospheric calculation functions."""

    # Standard conditions
    TA_C = 20.0  # Temperature [°C]
    TA_K = TA_C + 273.15  # Temperature [K]
    RH = 60.0  # Relative humidity [%]
    PRES_HPA = 1013.25  # Pressure [hPa]
    PRES_PA = PRES_HPA * 100  # Pressure [Pa]

    def test_humidity_round_trip(self):
        """Test qa → RH → qa round-trip consistency."""
        qa = cal_qa(self.RH, self.TA_K, self.PRES_HPA)
        rh_calc = cal_rh(qa, self.TA_K, self.PRES_HPA)

        assert abs(rh_calc - self.RH) < 1.0, "Round-trip RH differs by >1%"

    def test_humidity_consistency(self):
        """Test that qa + dq ≈ qa_sat (humidity budget closes)."""
        qa = cal_qa(self.RH, self.TA_K, self.PRES_HPA)
        dq = cal_dq(self.RH, self.TA_C, self.PRES_HPA)
        qa_sat = cal_qa(100.0, self.TA_K, self.PRES_HPA)

        assert abs(qa + dq - qa_sat) < 1e-3, "Humidity budget doesn't close"

    def test_clausius_clapeyron(self):
        """Test that saturation vapour pressure slope increases with temperature."""
        temps_k = np.array([273.15, 283.15, 293.15, 303.15, 313.15])
        idx = pd.date_range("2023-01-01", periods=len(temps_k), freq="h")
        ta_series = pd.Series(temps_k, index=idx)

        des_dta = cal_des_dta(ta_series, self.PRES_PA)

        # Clausius-Clapeyron: slope should increase with temperature
        assert (des_dta.diff()[1:] > 0).all(), "des/dTa not increasing with T"

    def test_latent_heat_range(self):
        """Test latent heat of vaporisation is in expected range."""
        qa = cal_qa(self.RH, self.TA_K, self.PRES_HPA)
        lv = cal_lat_vap(qa, self.TA_K, self.PRES_HPA)

        # Typical range: 2.4-2.6 MJ/kg
        assert 2.4e6 < lv < 2.6e6, f"Latent heat {lv/1e6:.2f} MJ/kg outside expected range"

    @pytest.mark.parametrize(
        "rh,expected_behaviour",
        [
            (0.0, "zero_humidity"),
            (100.0, "saturated"),
        ],
    )
    def test_humidity_extremes(self, rh, expected_behaviour):
        """Test calculations at humidity extremes."""
        qa = cal_qa(rh, self.TA_K, self.PRES_HPA)
        dq = cal_dq(rh, self.TA_C, self.PRES_HPA)

        if rh == 0.0:
            assert abs(qa) < 1e-10, "qa should be ~0 at 0% RH"
            assert dq > 0, "dq should be maximum at 0% RH"
        else:  # 100%
            assert abs(dq) < 1e-6, "dq should be ~0 at 100% RH"

    @pytest.mark.parametrize(
        "pres_hpa,description",
        [
            (1013.25, "sea_level"),
            (700.0, "high_altitude_3000m"),
        ],
    )
    def test_pressure_effect(self, pres_hpa, description):
        """Test calculations at different pressures."""
        qa = cal_qa(self.RH, self.TA_K, pres_hpa)
        assert qa > 0, f"Negative qa at {description}"
        assert qa < 0.05, f"qa too high at {description}"
