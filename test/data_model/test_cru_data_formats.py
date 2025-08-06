"""Test that CRU data in different formats (CSV and Parquet) produce identical results."""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path
from supy.data_model.precheck import get_mean_monthly_air_temperature


class TestCRUDataFormats:
    """Test suite to verify CRU data format consistency."""

    def test_parquet_csv_data_equivalence(self):
        """Verify that Parquet and CSV files contain identical data."""
        # Load both files
        parquet_path = Path("src/supy/ext_data/CRU_TS4.06_1991_2020.parquet")
        csv_path = Path(
            "test/fixtures/cru_data/CRU_TS4.06_cell_monthly_normals_1991_2020.csv"
        )

        # Skip if files don't exist
        if not parquet_path.exists() or not csv_path.exists():
            pytest.skip("CRU data files not found")

        df_parquet = pd.read_parquet(parquet_path)
        df_csv = pd.read_csv(csv_path)

        # Check shape
        assert df_parquet.shape == df_csv.shape, "DataFrames have different shapes"

        # Check columns
        assert set(df_parquet.columns) == set(df_csv.columns), (
            "DataFrames have different columns"
        )

        # Sort both dataframes to ensure same order
        sort_cols = ["Month", "Latitude", "Longitude"]
        df_parquet = df_parquet.sort_values(sort_cols).reset_index(drop=True)
        df_csv = df_csv.sort_values(sort_cols).reset_index(drop=True)

        # Check data values (allowing for float32 vs float64 precision differences)
        for col in df_csv.columns:
            if col == "Month":
                # Month should be identical
                assert (df_parquet[col] == df_csv[col]).all(), f"Month values differ"
            else:
                # For float columns, check within tolerance
                np.testing.assert_allclose(
                    df_parquet[col].values,
                    df_csv[col].values,
                    rtol=1e-5,  # Relative tolerance
                    atol=1e-6,  # Absolute tolerance
                    err_msg=f"Column {col} values differ beyond acceptable tolerance",
                )

    def test_temperature_lookup_consistency(self):
        """Test that temperature lookups produce consistent results across formats."""
        # Test cases covering different regions and months
        test_cases = [
            (51.5, 1, -0.1),  # London, January
            (51.5, 7, -0.1),  # London, July
            (40.7, 1, -74.0),  # New York, January
            (40.7, 7, -74.0),  # New York, July
            (-33.9, 1, 151.2),  # Sydney, January (summer)
            (-33.9, 7, 151.2),  # Sydney, July (winter)
            (35.7, 1, 139.7),  # Tokyo, January
            (35.7, 7, 139.7),  # Tokyo, July
            (60.2, 1, 24.9),  # Helsinki, January
            (60.2, 7, 24.9),  # Helsinki, July
            (0.0, 1, 10.0),  # Equator (Africa)
            (70.0, 12, 25.0),  # Arctic
            (-55.0, 1, -70.0),  # Patagonia (near Antarctic)
        ]

        # We can't directly control which file is loaded, but we can verify
        # that the function produces reasonable results for all test cases
        for lat, month, lon in test_cases:
            try:
                temp = get_mean_monthly_air_temperature(lat, month, lon)

                # Verify temperature is reasonable
                assert -50 <= temp <= 50, (
                    f"Temperature {temp}°C unreasonable for lat={lat}, month={month}"
                )

                # Verify specific expectations
                if lat > 60 and month in [12, 1, 2]:  # Arctic winter
                    assert temp < 5, f"Arctic winter temp {temp}°C seems too warm"
                elif lat < -60 and month in [6, 7, 8]:  # Antarctic winter
                    assert temp < 0, f"Antarctic winter temp {temp}°C seems too warm"
                elif abs(lat) < 25 and month in [6, 7, 8]:  # Tropical summer
                    assert temp > 15, f"Tropical temp {temp}°C seems too cold"

            except FileNotFoundError:
                pytest.skip("CRU data file not available")

    def test_data_file_sizes(self):
        """Verify that Parquet format provides expected size reduction."""
        import os

        parquet_path = Path("src/supy/ext_data/CRU_TS4.06_1991_2020.parquet")
        csv_path = Path(
            "test/fixtures/cru_data/CRU_TS4.06_cell_monthly_normals_1991_2020.csv"
        )

        if not parquet_path.exists() or not csv_path.exists():
            pytest.skip("CRU data files not found")

        parquet_size = os.path.getsize(parquet_path) / (1024 * 1024)  # MB
        csv_size = os.path.getsize(csv_path) / (1024 * 1024)  # MB

        # Parquet should be significantly smaller
        assert parquet_size < csv_size * 0.3, (
            f"Parquet ({parquet_size:.1f}MB) not significantly smaller than CSV ({csv_size:.1f}MB)"
        )

        # Parquet should be reasonably sized for distribution
        assert parquet_size < 5, (
            f"Parquet file too large for distribution: {parquet_size:.1f}MB"
        )

        print(f"\nFile sizes:")
        print(f"  CSV:     {csv_size:.1f} MB")
        print(f"  Parquet: {parquet_size:.1f} MB")
        print(f"  Reduction: {(1 - parquet_size / csv_size):.1%}")

    def test_parquet_data_types_optimized(self):
        """Verify that Parquet file uses optimized data types."""
        parquet_path = Path("src/supy/ext_data/CRU_TS4.06_1991_2020.parquet")

        if not parquet_path.exists():
            pytest.skip("Parquet file not found")

        df = pd.read_parquet(parquet_path)

        # Check optimized data types
        assert df["Month"].dtype == np.int8, "Month should be int8"
        assert df["Latitude"].dtype == np.float32, "Latitude should be float32"
        assert df["Longitude"].dtype == np.float32, "Longitude should be float32"
        assert df["NormalTemperature"].dtype == np.float32, (
            "Temperature should be float32"
        )

        # Verify memory usage is optimized
        memory_mb = df.memory_usage(deep=True).sum() / (1024 * 1024)
        assert memory_mb < 15, f"Memory usage too high: {memory_mb:.1f} MB"
