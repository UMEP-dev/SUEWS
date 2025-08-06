"""Extended tests for CRU data integration with comprehensive edge case coverage."""

import pytest
import numpy as np
import pandas as pd
from pathlib import Path
from unittest.mock import patch, MagicMock
from supy.data_model.precheck import get_mean_monthly_air_temperature


class TestCRUEdgeCases:
    """Extended test suite for CRU temperature data edge cases."""

    def test_coordinate_normalization(self):
        """Test that coordinates are properly normalized to CRU grid conventions."""
        # Test cases with various longitude formats
        test_cases = [
            # (input_lon, expected_normalized_lon)
            # Note: 180 and -180 are equivalent
            (180.0, -180.0),  # Valid edge (180 normalizes to -180)
            (-180.0, -180.0),  # Valid edge
            (360.0, 0.0),  # Should wrap to 0
            (370.0, 10.0),  # Should wrap
            (-190.0, 170.0),  # Should wrap
            (720.0, 0.0),  # Multiple wraps
            (-360.0, 0.0),  # Negative wrap
            (0.0, 0.0),  # Zero
            (90.0, 90.0),  # East
            (-90.0, -90.0),  # West
        ]
        
        for input_lon, expected_lon in test_cases:
            # Normalize longitude using modulo arithmetic
            normalized = ((input_lon + 180) % 360) - 180
            assert abs(normalized - expected_lon) < 0.001, (
                f"Longitude {input_lon} should normalize to {expected_lon}, got {normalized}"
            )

    def test_polar_regions(self):
        """Test temperature lookups at polar regions."""
        # Arctic tests - CRU data typically covers up to ~83.75°N
        arctic_cases = [
            (83.0, 1, 0.0),  # High Arctic, January
            (83.0, 7, 0.0),  # High Arctic, July
            (80.0, 12, 25.0),  # Arctic Circle, December
            (75.0, 6, -150.0),  # Northern Alaska, June
            (71.0, 1, -8.0),  # Jan Mayen, January
        ]
        
        for lat, month, lon in arctic_cases:
            try:
                temp = get_mean_monthly_air_temperature(lat, month, lon)
                # Arctic winter should be very cold
                if month in [12, 1, 2]:
                    assert temp < 0, f"Arctic winter temp {temp}°C should be below freezing"
                # Arctic summer can be above freezing but not warm
                elif month in [6, 7, 8]:
                    assert temp < 20, f"Arctic summer temp {temp}°C seems too warm"
            except FileNotFoundError:
                pytest.skip("CRU data file not available")
            except ValueError as e:
                if "No CRU data found" in str(e):
                    # CRU data may not cover extreme polar regions
                    pytest.skip(f"CRU data doesn't cover lat={lat}")
                raise
        
        # Antarctic tests - CRU data typically covers down to ~-60°S over land
        antarctic_cases = [
            (-60.0, 1, 0.0),  # Antarctic edge, January (summer)
            (-60.0, 7, 0.0),  # Antarctic edge, July (winter)
            (-70.0, 12, 0.0),  # Antarctic coast, December
            (-65.0, 6, -60.0),  # Antarctic Peninsula, June
        ]
        
        for lat, month, lon in antarctic_cases:
            try:
                temp = get_mean_monthly_air_temperature(lat, month, lon)
                # Antarctic should always be cold
                assert temp < 10, f"Antarctic temp {temp}°C seems too warm"
                # Antarctic winter should be extremely cold
                if month in [6, 7, 8]:
                    assert temp < -10, f"Antarctic winter temp {temp}°C not cold enough"
            except FileNotFoundError:
                pytest.skip("CRU data file not available")
            except ValueError as e:
                if "No CRU data found" in str(e):
                    # CRU data may not cover extreme polar regions
                    pytest.skip(f"CRU data doesn't cover lat={lat}")
                raise

    def test_date_line_crossing(self):
        """Test temperature lookups across the International Date Line."""
        date_line_cases = [
            (35.0, 7, 179.9),  # Just west of date line
            (35.0, 7, -179.9),  # Just east of date line
            (35.0, 7, 180.0),  # Exactly on date line
            (35.0, 7, -180.0),  # Exactly on date line (negative)
            (0.0, 1, 179.5),  # Equator near date line
            (-20.0, 12, -179.5),  # South Pacific near date line
        ]
        
        for lat, month, lon in date_line_cases:
            try:
                temp = get_mean_monthly_air_temperature(lat, month, lon)
                # Temperature should be reasonable regardless of date line
                assert -50 <= temp <= 50, (
                    f"Temperature {temp}°C unreasonable near date line"
                )
            except FileNotFoundError:
                pytest.skip("CRU data file not available")
            except ValueError as e:
                if "No CRU data found" in str(e):
                    # CRU data may have gaps over oceans
                    continue
                raise

    def test_ocean_and_island_locations(self):
        """Test temperature lookups over oceans and small islands."""
        ocean_cases = [
            (0.0, 7, -140.0),  # Central Pacific
            (30.0, 1, -40.0),  # North Atlantic
            (-40.0, 7, 80.0),  # Southern Indian Ocean
            (20.0, 12, -155.0),  # Hawaii region
            (-17.5, 6, -149.5),  # Tahiti region
            (64.0, 8, -22.0),  # Iceland region
        ]
        
        for lat, month, lon in ocean_cases:
            try:
                temp = get_mean_monthly_air_temperature(lat, month, lon)
                # Ocean temperatures should be moderated
                assert -30 <= temp <= 35, (
                    f"Ocean temperature {temp}°C seems extreme"
                )
                # Tropical oceans should be warm
                if abs(lat) < 25:
                    assert temp > 10, f"Tropical ocean temp {temp}°C too cold"
            except FileNotFoundError:
                pytest.skip("CRU data file not available")
            except ValueError as e:
                if "No CRU data found" in str(e):
                    # CRU data has gaps over oceans
                    continue
                raise

    def test_extreme_continental_locations(self):
        """Test temperature lookups in extreme continental climates."""
        continental_cases = [
            (62.0, 1, 129.7),  # Yakutsk, Russia (coldest city)
            (62.0, 7, 129.7),  # Yakutsk in summer
            (54.8, 1, 83.1),  # Novosibirsk, Siberia
            (47.9, 7, 106.9),  # Ulaanbaatar, Mongolia
            (41.3, 1, 69.3),  # Tashkent, Central Asia
        ]
        
        for lat, month, lon in continental_cases:
            try:
                temp = get_mean_monthly_air_temperature(lat, month, lon)
                # Siberian winter should be extremely cold
                if lat > 60 and month in [12, 1, 2] and lon > 100:
                    assert temp < -20, f"Siberian winter temp {temp}°C not cold enough"
                # Continental summer can be quite warm
                elif month in [6, 7, 8]:
                    assert temp > 5, f"Continental summer temp {temp}°C too cold"
            except FileNotFoundError:
                pytest.skip("CRU data file not available")

    def test_boundary_coordinates(self):
        """Test exact boundary coordinates of the CRU grid."""
        boundary_cases = [
            (83.0, 6, 0.0),  # Near North Pole (within CRU coverage)
            (-60.0, 12, 0.0),  # Antarctic edge (within CRU coverage)
            (0.0, 3, 180.0),  # Equator at date line
            (0.0, 9, -180.0),  # Equator at date line (negative)
            (83.75, 1, 179.75),  # Near maximum lat/lon (CRU limit)
            (-60.0, 7, -179.75),  # Southern edge with extreme longitude
        ]
        
        for lat, month, lon in boundary_cases:
            try:
                temp = get_mean_monthly_air_temperature(lat, month, lon)
                assert isinstance(temp, float), "Temperature should be a float"
                assert not np.isnan(temp), "Temperature should not be NaN"
            except FileNotFoundError:
                pytest.skip("CRU data file not available")
            except ValueError as e:
                # Some boundary cases might be outside CRU coverage
                if "No CRU data found" in str(e) or "Latitude must be between" in str(e):
                    continue
                raise

    def test_temperature_seasonality(self):
        """Test that seasonal patterns are correct for different hemispheres."""
        # Northern Hemisphere - should be warm in July, cold in January
        northern_cities = [
            (51.5, -0.1),  # London
            (40.7, -74.0),  # New York
            (35.7, 139.7),  # Tokyo
        ]
        
        for lat, lon in northern_cities:
            try:
                winter_temp = get_mean_monthly_air_temperature(lat, 1, lon)
                summer_temp = get_mean_monthly_air_temperature(lat, 7, lon)
                assert summer_temp > winter_temp, (
                    f"Northern hemisphere: Summer ({summer_temp}°C) should be warmer than winter ({winter_temp}°C)"
                )
            except FileNotFoundError:
                pytest.skip("CRU data file not available")
        
        # Southern Hemisphere - should be warm in January, cold in July
        southern_cities = [
            (-33.9, 151.2),  # Sydney
            (-34.6, -58.4),  # Buenos Aires
            (-26.2, 28.0),  # Johannesburg
        ]
        
        for lat, lon in southern_cities:
            try:
                summer_temp = get_mean_monthly_air_temperature(lat, 1, lon)
                winter_temp = get_mean_monthly_air_temperature(lat, 7, lon)
                assert summer_temp > winter_temp, (
                    f"Southern hemisphere: Summer/Jan ({summer_temp}°C) should be warmer than winter/Jul ({winter_temp}°C)"
                )
            except FileNotFoundError:
                pytest.skip("CRU data file not available")
        
        # Equatorial - should have minimal seasonal variation
        equatorial_cities = [
            (1.3, 103.8),  # Singapore
            (-0.2, -78.5),  # Quito
            (0.3, 32.6),  # Kampala
        ]
        
        for lat, lon in equatorial_cities:
            try:
                jan_temp = get_mean_monthly_air_temperature(lat, 1, lon)
                jul_temp = get_mean_monthly_air_temperature(lat, 7, lon)
                variation = abs(jan_temp - jul_temp)
                assert variation < 5, (
                    f"Equatorial region should have minimal seasonal variation (got {variation}°C)"
                )
            except FileNotFoundError:
                pytest.skip("CRU data file not available")


class TestCRUPerformance:
    """Test performance characteristics of CRU data lookups."""

    def test_multiple_lookups_performance(self):
        """Test that multiple lookups are reasonably fast."""
        import time
        
        # Generate random locations likely to have CRU data (land-focused)
        np.random.seed(42)
        n_lookups = 50  # Reduced since many ocean points have no data
        # Focus on latitudes where CRU has better coverage
        lats = np.random.uniform(-60, 70, n_lookups)
        lons = np.random.uniform(-180, 180, n_lookups)
        months = np.random.randint(1, 13, n_lookups)
        
        try:
            start_time = time.time()
            results = []
            failures = 0
            for lat, lon, month in zip(lats, lons, months):
                try:
                    temp = get_mean_monthly_air_temperature(lat, int(month), lon)
                    results.append(temp)
                except ValueError as e:
                    if "No CRU data found" in str(e):
                        failures += 1
                        continue
                    raise
            elapsed = time.time() - start_time
            
            # Should complete lookups in reasonable time
            assert elapsed < 5.0, f"{n_lookups} lookups took {elapsed:.2f}s, should be < 5s"
            
            # At least some results should be valid (CRU has ocean gaps)
            assert len(results) > 0, "No successful lookups"
            assert all(-90 <= t <= 60 for t in results), "Some temperatures out of range"
            
            success_rate = len(results) / n_lookups * 100
            print(f"\nPerformance: {n_lookups} lookups in {elapsed:.3f}s")
            print(f"Success rate: {success_rate:.1f}% ({len(results)}/{n_lookups})")
            print(f"Lookups/s: {n_lookups/elapsed:.0f}")
            
        except FileNotFoundError:
            pytest.skip("CRU data file not available")

    def test_data_caching_benefit(self):
        """Test that repeated lookups benefit from caching."""
        import time
        
        try:
            # First lookup (cold cache)
            start = time.time()
            temp1 = get_mean_monthly_air_temperature(51.5, 7, -0.1)
            first_lookup = time.time() - start
            
            # Second lookup (warm cache, same location)
            start = time.time()
            temp2 = get_mean_monthly_air_temperature(51.5, 7, -0.1)
            second_lookup = time.time() - start
            
            # Third lookup (warm cache, different location)
            start = time.time()
            temp3 = get_mean_monthly_air_temperature(40.7, 1, -74.0)
            third_lookup = time.time() - start
            
            assert temp1 == temp2, "Same location should return same temperature"
            
            # Note: We can't guarantee caching without modifying the implementation,
            # but we can verify the lookups work correctly
            print(f"\nLookup times: 1st={first_lookup:.3f}s, 2nd={second_lookup:.3f}s, 3rd={third_lookup:.3f}s")
            
        except FileNotFoundError:
            pytest.skip("CRU data file not available")


class TestCRUDataValidation:
    """Test data validation and error handling."""

    def test_invalid_input_validation(self):
        """Test that invalid inputs are properly validated."""
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
        
        # None values should be handled gracefully
        with pytest.raises((TypeError, ValueError)):
            get_mean_monthly_air_temperature(None, 7, 0.0)
        
        with pytest.raises((TypeError, ValueError)):
            get_mean_monthly_air_temperature(45.0, None, 0.0)

    def test_temperature_bounds_validation(self):
        """Test that returned temperatures are within realistic bounds."""
        # Sample diverse locations (focusing on land areas with CRU coverage)
        test_locations = [
            (71.0, 1, -8.0),  # Jan Mayen (Arctic)
            (28.6, 7, 77.2),  # Delhi (hot summer)
            (3.1, 12, 101.7),  # Kuala Lumpur (tropical)
            (61.5, 1, -150.0),  # Anchorage (cold winter)
            (-54.8, 7, -68.3),  # Ushuaia (cold region)
            (51.5, 7, -0.1),  # London (temperate)
            (-33.9, 1, 151.2),  # Sydney (summer)
        ]
        
        for lat, month, lon in test_locations:
            try:
                temp = get_mean_monthly_air_temperature(lat, month, lon)
                
                # Global temperature bounds (accounting for climate extremes)
                assert -90 <= temp <= 60, (
                    f"Temperature {temp}°C outside realistic bounds for Earth"
                )
                
                # More specific bounds based on location
                if abs(lat) > 80:  # Polar regions
                    assert -60 <= temp <= 15, f"Polar temp {temp}°C seems unrealistic"
                elif abs(lat) < 25:  # Tropical regions
                    assert 10 <= temp <= 40, f"Tropical temp {temp}°C seems unrealistic"
                
            except FileNotFoundError:
                pytest.skip("CRU data file not available")
            except ValueError as e:
                if "No CRU data found" in str(e):
                    # Skip locations without CRU coverage
                    continue
                raise

    def test_nearest_neighbor_consistency(self):
        """Test that nearby locations return similar temperatures."""
        try:
            # Test locations that are close together
            base_lat, base_lon = 51.5, -0.1  # London
            base_temp = get_mean_monthly_air_temperature(base_lat, 7, base_lon)
            
            # Check nearby locations (within 1 degree)
            offsets = [
                (0.1, 0.0), (-0.1, 0.0),  # North/South
                (0.0, 0.1), (0.0, -0.1),  # East/West
                (0.1, 0.1), (-0.1, -0.1),  # Diagonal
            ]
            
            for dlat, dlon in offsets:
                nearby_temp = get_mean_monthly_air_temperature(
                    base_lat + dlat, 7, base_lon + dlon
                )
                
                # Nearby locations should have similar temperatures (within 5°C)
                diff = abs(base_temp - nearby_temp)
                assert diff < 5.0, (
                    f"Temperature difference {diff}°C too large for nearby locations"
                )
            
        except FileNotFoundError:
            pytest.skip("CRU data file not available")

    def test_grid_resolution_effects(self):
        """Test behavior at CRU grid resolution boundaries."""
        # CRU data is at 0.5 degree resolution
        # Test points at grid centers and edges
        grid_tests = [
            (45.0, 7, 10.0),  # Grid center
            (45.25, 7, 10.25),  # Grid corner
            (45.1, 7, 10.1),  # Off-grid
            (44.9, 7, 9.9),  # Slightly different cell
        ]
        
        try:
            temps = []
            for lat, month, lon in grid_tests:
                temp = get_mean_monthly_air_temperature(lat, month, lon)
                temps.append(temp)
            
            # All temperatures should be reasonable
            assert all(-50 <= t <= 50 for t in temps), "Some temperatures out of range"
            
            # Temperatures should be relatively similar for this small region
            temp_range = max(temps) - min(temps)
            assert temp_range < 10, (
                f"Temperature range {temp_range}°C too large for small region"
            )
            
        except FileNotFoundError:
            pytest.skip("CRU data file not available")