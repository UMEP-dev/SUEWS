"""Tests for CRU data loading and temperature initialization.

This test file ensures CRU data loads properly for initial temperature checking
as designed by Silvia. Tests fail (not skip) when data cannot be loaded.
"""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path
from supy.data_model.precheck import get_mean_monthly_air_temperature


class TestCRUDataLoading:
    """Test that CRU data loads properly for temperature initialization."""

    def test_cru_data_file_exists(self):
        """Test that the CRU Parquet file exists and is properly formatted."""
        parquet_path = Path("src/supy/ext_data/CRU_TS4.06_1991_2020.parquet")
        assert parquet_path.exists(), f"CRU data file not found at {parquet_path}"

        # Verify it's a valid Parquet file with correct structure
        df = pd.read_parquet(parquet_path)
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
        import os

        size_mb = os.path.getsize(parquet_path) / (1024 * 1024)
        assert size_mb < 5, f"Parquet file too large: {size_mb:.1f}MB"

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
            temp = get_mean_monthly_air_temperature(lat, month, lon)

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
        london_winter = get_mean_monthly_air_temperature(51.5, 1, -0.1)
        london_summer = get_mean_monthly_air_temperature(51.5, 7, -0.1)
        assert london_summer > london_winter, (
            f"London summer ({london_summer}°C) should be warmer than winter ({london_winter}°C)"
        )

        # Sydney - opposite seasonal pattern (Southern Hemisphere)
        sydney_summer = get_mean_monthly_air_temperature(
            -33.9, 1, 151.2
        )  # Jan is summer
        sydney_winter = get_mean_monthly_air_temperature(
            -33.9, 7, 151.2
        )  # Jul is winter
        assert sydney_summer > sydney_winter, (
            f"Sydney summer/Jan ({sydney_summer}°C) should be warmer than winter/Jul ({sydney_winter}°C)"
        )

    def test_cru_input_validation(self):
        """Test that invalid inputs are properly rejected."""
        # Invalid latitude
        with pytest.raises(ValueError, match="Latitude must be between"):
            get_mean_monthly_air_temperature(91.0, 7, 0.0)

        with pytest.raises(ValueError, match="Latitude must be between"):
            get_mean_monthly_air_temperature(-91.0, 7, 0.0)

        # Invalid month
        with pytest.raises(ValueError, match="Month must be between"):
            get_mean_monthly_air_temperature(45.0, 0, 0.0)

        with pytest.raises(ValueError, match="Month must be between"):
            get_mean_monthly_air_temperature(45.0, 13, 0.0)

    def test_cru_data_coverage_gaps_should_fail(self):
        """Test that locations without CRU data raise appropriate errors.

        These should FAIL (raise ValueError), not skip, when no data is available.
        """
        # Locations that likely have no CRU data (extreme ocean/poles)
        no_data_locations = [
            (0.0, 7, -140.0),  # Central Pacific Ocean
            (30.0, 1, -40.0),  # Middle of Atlantic Ocean
            (-85.0, 7, 0.0),  # Near South Pole (beyond CRU coverage)
            (88.0, 1, 0.0),  # Near North Pole (beyond CRU coverage)
        ]

        for lat, month, lon in no_data_locations:
            with pytest.raises(ValueError, match="No CRU data found"):
                get_mean_monthly_air_temperature(lat, month, lon)


class TestCRUIntegrationWithPrecheck:
    """Test CRU data integration with the precheck temperature initialization."""

    def test_precheck_uses_cru_temperature(self):
        """Test that precheck properly uses CRU data for temperature initialization."""
        from copy import deepcopy
        from supy.data_model.precheck import precheck_update_temperature

        # Build minimal configuration
        data = {
            "model": {
                "control": {
                    "start_time": "2011-07-01",  # July
                    "end_time": "2011-12-31",
                }
            },
            "sites": [
                {
                    "properties": {
                        "lat": {"value": 51.5},  # London
                        "lng": {"value": -0.1},
                    },
                    "initial_states": {
                        surf: {
                            "temperature": {"value": [0, 0, 0, 0, 0]},
                            "tsfc": {"value": 0},
                            "tin": {"value": 0},
                        }
                        for surf in [
                            "paved",
                            "bldgs",
                            "evetr",
                            "dectr",
                            "grass",
                            "bsoil",
                            "water",
                        ]
                    },
                }
            ],
        }

        # Get expected temperature from CRU
        expected_temp = get_mean_monthly_air_temperature(51.5, 7, -0.1)  # London, July

        # Run precheck temperature update
        updated = precheck_update_temperature(deepcopy(data), start_date="2011-07-01")

        # Verify all surfaces were updated with CRU temperature
        for surface in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]:
            temp_array = updated["sites"][0]["initial_states"][surface]["temperature"][
                "value"
            ]
            tsfc = updated["sites"][0]["initial_states"][surface]["tsfc"]["value"]
            tin = updated["sites"][0]["initial_states"][surface]["tin"]["value"]

            # All temperature values should be set to CRU temperature
            assert all(t == expected_temp for t in temp_array), (
                f"{surface} temperature array not properly initialized"
            )
            assert tsfc == expected_temp, f"{surface} tsfc not properly initialized"
            assert tin == expected_temp, f"{surface} tin not properly initialized"
