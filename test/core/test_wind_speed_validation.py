"""Test wind speed validation in forcing data."""

import pytest
import pandas as pd
import numpy as np
from supy._check import check_forcing, check_range
from supy._check import dict_rules_indiv


class TestWindSpeedValidation:
    """Test class for wind speed validation."""

    def test_wind_speed_minimum_rule(self):
        """Test that wind speed minimum is set to 0.01 m/s in rules."""
        assert "u" in dict_rules_indiv
        assert dict_rules_indiv["u"]["param"]["min"] == 0.01
        assert dict_rules_indiv["u"]["param"]["max"] == 60
        assert dict_rules_indiv["u"]["optional"] is False

    def test_check_forcing_with_zero_wind_speed(self):
        """Test that check_forcing detects zero wind speed."""
        # Create sample forcing data with zero wind speed
        dates = pd.date_range("2021-01-01", periods=24, freq="h")
        df_forcing = pd.DataFrame(
            {
                "iy": dates.year,
                "id": dates.dayofyear,
                "it": dates.hour,
                "imin": dates.minute,
                "isec": 0,
                "kdown": 100,
                "Tair": 20,
                "RH": 60,
                "pres": 101.3,
                "rain": 0,
                "U": 0,  # Zero wind speed
            },
            index=dates,
        )

        # Check forcing data without fix
        issues = check_forcing(df_forcing, fix=False)

        # Should have at least one issue about wind speed
        assert len(issues) > 0
        wind_speed_issue = [
            issue
            for issue in issues
            if "Wind speed" in issue and "must be >= 0.01" in issue
        ]
        assert len(wind_speed_issue) > 0

    def test_check_forcing_with_low_wind_speed(self):
        """Test that check_forcing detects wind speed below 0.01 m/s."""
        # Create sample forcing data with low wind speed
        dates = pd.date_range("2021-01-01", periods=24, freq="h")
        df_forcing = pd.DataFrame(
            {
                "iy": dates.year,
                "id": dates.dayofyear,
                "it": dates.hour,
                "imin": dates.minute,
                "isec": 0,
                "kdown": 100,
                "Tair": 20,
                "RH": 60,
                "pres": 101.3,
                "rain": 0,
                "U": 0.005,  # Wind speed below minimum
            },
            index=dates,
        )

        # Check forcing data without fix
        issues = check_forcing(df_forcing, fix=False)

        # Should have at least one issue about wind speed
        assert len(issues) > 0
        wind_speed_issue = [
            issue
            for issue in issues
            if "Wind speed" in issue and "must be >= 0.01" in issue
        ]
        assert len(wind_speed_issue) > 0

    def test_check_forcing_fixes_low_wind_speed(self):
        """Test that check_forcing can fix low wind speed values."""
        # Create sample forcing data with various wind speeds
        dates = pd.date_range("2021-01-01", periods=24, freq="h")
        wind_speeds = np.array(
            [0, 0.001, 0.005, 0.009, 0.01, 0.02, 0.5, 1.0] + [2.0] * 16
        )
        df_forcing = pd.DataFrame(
            {
                "iy": dates.year,
                "id": dates.dayofyear,
                "it": dates.hour,
                "imin": dates.minute,
                "isec": 0,
                "kdown": 100,
                "Tair": 20,
                "RH": 60,
                "pres": 101.3,
                "rain": 0,
                "U": wind_speeds,
            },
            index=dates,
        )

        # Check forcing data with fix
        df_fixed = check_forcing(df_forcing, fix=True)

        # All wind speeds should now be >= 0.01
        assert (df_fixed["U"] >= 0.01).all()
        # Values that were already >= 0.01 should be unchanged
        assert (
            df_fixed["U"][wind_speeds >= 0.01] == wind_speeds[wind_speeds >= 0.01]
        ).all()
        # Values that were < 0.01 should be clipped to 0.01
        assert (df_fixed["U"][wind_speeds < 0.01] == 0.01).all()

    def test_check_range_wind_speed_specific_message(self):
        """Test that check_range provides specific message for wind speed."""
        # Create a series with low wind speed
        ser_wind = pd.Series([0.005, 0.008, 0.012], name="U")

        # Check range
        var, is_accepted, description, suggestion = check_range(
            ser_wind, dict_rules_indiv
        )

        assert var == "u"  # Note: lowercased
        assert not is_accepted
        assert "Wind speed" in description
        assert "must be >= 0.01 m/s" in description
        assert "division by zero errors" in description
        assert "numerical stability in SUEWS" in suggestion

    def test_valid_wind_speed_passes_validation(self):
        """Test that valid wind speed passes validation."""
        # Create sample forcing data with valid wind speed
        dates = pd.date_range("2021-01-01", periods=24, freq="h")

        # Create forcing data with all required columns in correct order
        df_forcing = pd.DataFrame(
            {
                "iy": dates.year,
                "id": dates.dayofyear,
                "it": dates.hour,
                "imin": dates.minute,
                "qn": -999,  # Optional
                "qh": -999,  # Optional
                "qe": -999,  # Optional
                "qs": -999,  # Optional
                "qf": -999,  # Optional
                "U": np.random.uniform(0.01, 10, 24),  # Valid wind speeds
                "RH": 60,
                "Tair": 20,
                "pres": 1013,  # hPa
                "rain": 0,
                "kdown": 100,
                "snow": -999,  # Optional
                "ldown": -999,  # Optional
                "fcld": -999,  # Optional
                "Wuh": -999,  # Optional
                "xsmd": -999,  # Optional
                "lai": -999,  # Optional
                "kdiff": -999,  # Optional
                "kdir": -999,  # Optional
                "wdir": -999,  # Optional
            },
            index=dates,
        )
        # Check forcing data
        issues = check_forcing(df_forcing, fix=False)

        # Should have no issues (or at least no wind speed issues)
        if issues:
            wind_speed_issues = [
                issue
                for issue in issues
                if "Wind speed" in issue and "must be >= 0.01" in issue
            ]
            assert len(wind_speed_issues) == 0

    def test_mixed_wind_speeds_partial_fix(self):
        """Test handling of mixed valid and invalid wind speeds."""
        # Create sample forcing data with mixed wind speeds
        dates = pd.date_range("2021-01-01", periods=100, freq="h")
        wind_speeds = np.random.uniform(0, 5, 100)
        wind_speeds[::10] = 0  # Set every 10th value to 0

        df_forcing = pd.DataFrame(
            {
                "iy": dates.year,
                "id": dates.dayofyear,
                "it": dates.hour,
                "imin": dates.minute,
                "isec": 0,
                "kdown": 100,
                "Tair": 20,
                "RH": 60,
                "pres": 101.3,
                "rain": 0,
                "U": wind_speeds,
            },
            index=dates,
        )

        # Count invalid values
        n_invalid = (wind_speeds < 0.01).sum()

        # Check without fix
        issues = check_forcing(df_forcing, fix=False)
        wind_speed_issue = [
            issue
            for issue in issues
            if "Wind speed" in issue and "must be >= 0.01" in issue
        ]
        assert len(wind_speed_issue) > 0

        # Check with fix
        df_fixed = check_forcing(df_forcing, fix=True)

        # Verify all values are now valid
        assert (df_fixed["U"] >= 0.01).all()
        # Verify the number of modified values
        assert (df_fixed["U"] == 0.01).sum() == n_invalid
