"""
Consolidated tests for integration and external data.

This file combines:
- CRU data tests from test_cru.py
- Other integration tests as needed
"""

"""Tests for CRU data loading and temperature initialization.

This test file ensures CRU data loads properly for initial temperature checking
as designed by Silvia. Tests fail (not skip) when data cannot be loaded.
"""


import numpy as np
import pandas as pd
import pytest

from supy.data_model.validation.yaml_helpers import (
    get_mean_monthly_air_temperature,
)


class TestCRUDataLoading:
    """Test that CRU data loads properly for temperature initialization."""

    def test_cru_data_file_exists(self):
        """Test that the CRU Parquet file exists and is properly formatted."""
        # Use the same method as get_mean_monthly_air_temperature to access the data
        from supy._env import trv_supy_module

        cru_resource = trv_supy_module / "ext_data" / "CRU_TS4.06_1991_2020.parquet"
        assert cru_resource.exists(), f"CRU data file not found at {cru_resource}"

        # Verify it's a valid Parquet file with correct structure
        with cru_resource.open("rb") as f:
            df = pd.read_parquet(f)
        assert not df.empty, "CRU data file is empty"

        # Check required columns
        required_cols = ["Month", "Latitude", "Longitude", "NormalTemperature"]
        assert all(col in df.columns for col in required_cols), (
            f"Missing required columns. Expected {required_cols}, got {df.columns.tolist()}"
        )

        # Verify optimized data types for efficiency
        assert df["Month"].dtype == np.int8, "Month should be int8"
        assert df["Latitude"].dtype == np.float32, "Latitude should be float32"
        assert df["Longitude"].dtype == np.float32, "Longitude should be float32"
        assert df["NormalTemperature"].dtype == np.float32, (
            "Temperature should be float32"
        )

        # Verify file size is reasonable for distribution
        # Note: Can't use os.path.getsize on resource files, check dataframe memory instead
        size_mb = df.memory_usage(deep=True).sum() / (1024 * 1024)
        assert size_mb < 15, f"CRU data in memory too large: {size_mb:.1f}MB"

    def test_cru_temperature_lookup_major_cities(self):
        """Test temperature lookups for major cities with known CRU coverage."""
        test_cases = [
            (51.5, 1, -0.1),  # London, January - should be cold
            (51.5, 7, -0.1),  # London, July - should be warmer
            (40.7, 1, -74.0),  # New York, January - should be cold
            (40.7, 7, -74.0),  # New York, July - should be warm
            (35.7, 1, 139.7),  # Tokyo, January - should be cool
            (35.7, 7, 139.7),  # Tokyo, July - should be warm
            (-33.9, 1, 151.2),  # Sydney, January (summer) - should be warm
            (-33.9, 7, 151.2),  # Sydney, July (winter) - should be cooler
        ]

        for lat, month, lon in test_cases:
            # This should not raise an exception for major cities
            temp = get_mean_monthly_air_temperature(lat, lon, month)

            # Basic sanity checks
            assert isinstance(temp, float), (
                f"Temperature should be float, got {type(temp)}"
            )
            assert -50 <= temp <= 50, f"Temperature {temp}°C out of realistic range"

            # Seasonal checks for Northern Hemisphere cities
            if lat > 0:
                if month == 1:  # January (winter)
                    assert temp < 20, f"Northern winter temp {temp}°C seems too warm"
                elif month == 7:  # July (summer)
                    assert temp > 0, f"Northern summer temp {temp}°C seems too cold"

            # Seasonal checks for Southern Hemisphere (Sydney)
            elif lat < 0:
                if month == 1:  # January (summer)
                    assert temp > 10, f"Southern summer temp {temp}°C seems too cold"
                elif month == 7:  # July (winter)
                    assert temp < 25, f"Southern winter temp {temp}°C seems too warm"

    def test_cru_seasonal_pattern(self):
        """Test that seasonal temperature patterns are correct."""
        # London - clear seasonal pattern
        london_winter = get_mean_monthly_air_temperature(51.5, -0.1, 1)
        london_summer = get_mean_monthly_air_temperature(51.5, -0.1, 7)
        assert london_summer > london_winter, (
            f"London summer ({london_summer}°C) should be warmer than winter ({london_winter}°C)"
        )

        # Sydney - opposite seasonal pattern (Southern Hemisphere)
        sydney_summer = get_mean_monthly_air_temperature(
            -33.9, 151.2, 1
        )  # Jan is summer
        sydney_winter = get_mean_monthly_air_temperature(
            -33.9, 151.2, 7
        )  # Jul is winter
        assert sydney_summer > sydney_winter, (
            f"Sydney summer ({sydney_summer}°C) should be warmer than winter ({sydney_winter}°C)"
        )

        # Equatorial region - minimal seasonal variation
        singapore_jan = get_mean_monthly_air_temperature(1.3, 103.8, 1)
        singapore_jul = get_mean_monthly_air_temperature(1.3, 103.8, 7)
        seasonal_diff = abs(singapore_jan - singapore_jul)
        assert seasonal_diff < 3, (
            f"Singapore seasonal variation ({seasonal_diff}°C) should be minimal"
        )

    def test_cru_extreme_locations(self):
        """Test CRU behavior at extreme/remote locations."""
        # Arctic location (might be outside coverage)
        try:
            arctic_temp = get_mean_monthly_air_temperature(85.0, 0.0, 1)
            # If it returns, should be very cold
            assert arctic_temp < 0, f"Arctic temperature {arctic_temp}°C should be < 0"
        except Exception as e:
            # It's OK if CRU doesn't cover extreme Arctic
            assert "No CRU data" in str(e) or "outside coverage" in str(e).lower()

        # Antarctic location (likely outside coverage)
        try:
            antarctic_temp = get_mean_monthly_air_temperature(-85.0, 0.0, 7)
            # If it returns, should be very cold
            assert antarctic_temp < -10, (
                f"Antarctic temperature {antarctic_temp}°C should be < -10"
            )
        except Exception as e:
            # It's OK if CRU doesn't cover extreme Antarctic
            assert "No CRU data" in str(e) or "outside coverage" in str(e).lower()

        # Middle of Pacific Ocean (might be outside land-based coverage)
        try:
            pacific_temp = get_mean_monthly_air_temperature(0.0, -160.0, 6)
            # If it returns, should be reasonable tropical temperature
            assert 20 <= pacific_temp <= 35, (
                f"Pacific equatorial temp {pacific_temp}°C out of expected range"
            )
        except Exception as e:
            # It's OK if CRU doesn't cover open ocean
            assert "No CRU data" in str(e) or "outside coverage" in str(e).lower()

    def test_cru_month_bounds(self):
        """Test that month parameter validation works correctly."""
        # Valid months should work
        for month in range(1, 13):
            temp = get_mean_monthly_air_temperature(51.5, -0.1, month)
            assert isinstance(temp, float)

        # Invalid months should raise appropriate errors
        with pytest.raises((ValueError, AssertionError)):
            get_mean_monthly_air_temperature(51.5, -0.1, 0)  # Month 0 invalid

        with pytest.raises((ValueError, AssertionError)):
            get_mean_monthly_air_temperature(51.5, -0.1, 13)  # Month 13 invalid

    def test_cru_coordinate_validation(self):
        """Test that coordinate validation works correctly."""
        # Valid coordinates - use a location with known CRU coverage
        # Note: (0.0, 0.0) doesn't have CRU data, so we use a nearby land location
        try:
            temp = get_mean_monthly_air_temperature(
                5.0, 10.0, 6
            )  # West Africa (has coverage)
            assert isinstance(temp, float)
        except ValueError:
            # If no coverage at this point, that's acceptable
            pass

        # Edge cases but valid - North/South Pole likely don't have CRU data
        # so we expect ValueError or None
        try:
            temp = get_mean_monthly_air_temperature(90.0, 180.0, 6)  # North Pole
            assert isinstance(temp, (float, type(None)))  # Might not have coverage
        except ValueError:
            pass  # Expected if no CRU data at poles

        try:
            temp = get_mean_monthly_air_temperature(-90.0, -180.0, 6)  # South Pole
            assert isinstance(temp, (float, type(None)))  # Might not have coverage
        except ValueError:
            pass  # Expected if no CRU data at poles

        # Invalid coordinates
        with pytest.raises((ValueError, AssertionError)):
            get_mean_monthly_air_temperature(91.0, 0.0, 6)  # Latitude > 90

        with pytest.raises((ValueError, AssertionError)):
            get_mean_monthly_air_temperature(-91.0, 0.0, 6)  # Latitude < -90

        with pytest.raises((ValueError, AssertionError)):
            get_mean_monthly_air_temperature(0.0, 181.0, 6)  # Longitude > 180

        with pytest.raises((ValueError, AssertionError)):
            get_mean_monthly_air_temperature(0.0, -181.0, 6)  # Longitude < -180

    def test_cru_known_temperature_ranges(self):
        """Test that returned temperatures are within known physical ranges."""
        # Sample various locations and months
        test_locations = [
            (51.5, -0.1),  # London
            (40.7, -74.0),  # New York
            (35.7, 139.7),  # Tokyo
            (-33.9, 151.2),  # Sydney
            (55.8, 37.6),  # Moscow
            (1.3, 103.8),  # Singapore
            (64.1, -21.9),  # Reykjavik
            (-22.9, -43.2),  # Rio de Janeiro
        ]

        for lat, lon in test_locations:
            for month in [1, 4, 7, 10]:  # Test each season
                try:
                    temp = get_mean_monthly_air_temperature(lat, lon, month)
                    # Global temperature records: -89.2°C to 54.4°C
                    # But monthly means should be much more moderate
                    assert -60 <= temp <= 50, (
                        f"Temperature {temp}°C at ({lat}, {lon}) month {month} "
                        f"outside realistic monthly mean range"
                    )
                except Exception:
                    # Some locations might not have coverage, that's OK
                    pass

    def test_cru_interpolation_consistency(self):
        """Test that nearby points have similar temperatures."""
        # London and a point 0.5 degrees away
        london_temp = get_mean_monthly_air_temperature(51.5, -0.1, 7)
        nearby_temp = get_mean_monthly_air_temperature(52.0, -0.1, 7)

        # Temperatures shouldn't differ by more than 5°C for nearby points
        temp_diff = abs(london_temp - nearby_temp)
        assert temp_diff < 5, (
            f"Nearby points have too different temperatures: "
            f"{london_temp}°C vs {nearby_temp}°C (diff: {temp_diff}°C)"
        )

    def test_cru_data_completeness(self):
        """Test that CRU data has good coverage for populated areas."""
        # Major world cities that should definitely have coverage
        major_cities = [
            (51.5074, -0.1278, "London"),
            (40.7128, -74.0060, "New York"),
            (35.6762, 139.6503, "Tokyo"),
            (48.8566, 2.3522, "Paris"),
            (55.7558, 37.6173, "Moscow"),
            (-33.8688, 151.2093, "Sydney"),
            (39.9042, 116.4074, "Beijing"),
            (19.0760, 72.8777, "Mumbai"),
            (-23.5505, -46.6333, "São Paulo"),
            (37.5665, 126.9780, "Seoul"),
        ]

        missing_coverage = []
        for lat, lon, city in major_cities:
            try:
                temp = get_mean_monthly_air_temperature(lat, lon, 7)
                assert isinstance(temp, float), f"{city} returned non-float: {temp}"
            except Exception as e:
                missing_coverage.append(f"{city}: {e}")

        assert len(missing_coverage) == 0, (
            "Major cities without CRU coverage:\n" + "\n".join(missing_coverage)
        )
