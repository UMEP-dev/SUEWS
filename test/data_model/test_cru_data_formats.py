"""Test CRU data format and optimization."""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path


class TestCRUDataFormat:
    """Essential tests for CRU data format and optimization."""

    def test_parquet_csv_equivalence(self):
        """Verify Parquet and CSV files contain equivalent data."""
        parquet_path = Path("src/supy/ext_data/CRU_TS4.06_1991_2020.parquet")
        csv_path = Path("test/fixtures/cru_data/CRU_TS4.06_cell_monthly_normals_1991_2020.csv")

        # Both files must exist for this test
        assert parquet_path.exists(), f"Parquet file not found: {parquet_path}"
        assert csv_path.exists(), f"CSV file not found: {csv_path}"

        df_parquet = pd.read_parquet(parquet_path)
        df_csv = pd.read_csv(csv_path)

        # Basic structure checks
        assert df_parquet.shape == df_csv.shape, "DataFrames have different shapes"
        assert set(df_parquet.columns) == set(df_csv.columns), "Different columns"

        # Sort for comparison
        sort_cols = ["Month", "Latitude", "Longitude"]
        df_parquet = df_parquet.sort_values(sort_cols).reset_index(drop=True)
        df_csv = df_csv.sort_values(sort_cols).reset_index(drop=True)

        # Verify data equivalence (allowing for type conversion precision)
        np.testing.assert_allclose(
            df_parquet["NormalTemperature"].values,
            df_csv["NormalTemperature"].values,
            rtol=1e-5,
            atol=1e-6,
            err_msg="Temperature values differ beyond acceptable tolerance"
        )

    def test_parquet_optimization(self):
        """Verify Parquet file is properly optimized."""
        import os
        
        parquet_path = Path("src/supy/ext_data/CRU_TS4.06_1991_2020.parquet")
        csv_path = Path("test/fixtures/cru_data/CRU_TS4.06_cell_monthly_normals_1991_2020.csv")
        
        assert parquet_path.exists(), f"Parquet file not found: {parquet_path}"
        
        # Size comparison
        parquet_size_mb = os.path.getsize(parquet_path) / (1024 * 1024)
        assert parquet_size_mb < 5, f"Parquet too large: {parquet_size_mb:.1f}MB"
        
        if csv_path.exists():
            csv_size_mb = os.path.getsize(csv_path) / (1024 * 1024)
            compression_ratio = parquet_size_mb / csv_size_mb
            assert compression_ratio < 0.3, f"Poor compression: {compression_ratio:.1%}"
        
        # Data type optimization
        df = pd.read_parquet(parquet_path)
        assert df["Month"].dtype == np.int8, "Month should be int8"
        assert df["Latitude"].dtype == np.float32, "Latitude should be float32"
        assert df["Longitude"].dtype == np.float32, "Longitude should be float32"
        assert df["NormalTemperature"].dtype == np.float32, "Temperature should be float32"
