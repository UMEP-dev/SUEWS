"""Test df_state conversion functionality."""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path
import tempfile
import shutil

import supy as sp
from supy.util.converter import (
    detect_input_type,
    load_df_state_file,
    detect_df_state_version,
    convert_df_state_format,
    convert_to_yaml,
)
from supy.util.converter.df_state import validate_converted_df_state


class TestDfStateDetection:
    """Test input type and version detection."""

    def test_detect_nml_input(self, tmp_path):
        """Test detection of RunControl.nml input."""
        nml_file = tmp_path / "RunControl.nml"
        nml_file.write_text("&runcontrol\n/\n")

        assert detect_input_type(nml_file) == "nml"

    def test_detect_csv_input(self, tmp_path):
        """Test detection of CSV df_state input."""
        csv_file = tmp_path / "df_state.csv"
        csv_file.touch()

        assert detect_input_type(csv_file) == "df_state"

    def test_detect_pkl_input(self, tmp_path):
        """Test detection of pickle df_state input."""
        pkl_file = tmp_path / "state.pkl"
        pkl_file.touch()

        assert detect_input_type(pkl_file) == "df_state"

    def test_detect_invalid_input(self, tmp_path):
        """Test error on invalid input type."""
        txt_file = tmp_path / "invalid.txt"
        txt_file.touch()

        with pytest.raises(ValueError, match="Unknown input file type"):
            detect_input_type(txt_file)

    def test_detect_directory_input(self, tmp_path):
        """Test error on directory input."""
        with pytest.raises(ValueError, match="Input must be a file"):
            detect_input_type(tmp_path)


class TestCsvFileConversion:
    """Test conversion of actual CSV file."""

    @pytest.fixture
    def old_csv_path(self):
        """Path to the provided old format CSV."""
        # Use the fixture file in the test directory
        csv_path = Path("test/fixtures/legacy_format/old_df_state.csv")
        if csv_path.exists():
            return csv_path
        else:
            pytest.skip("Test CSV file not available")

    def test_load_old_csv(self, old_csv_path):
        """Test loading the provided old CSV file."""
        df = load_df_state_file(old_csv_path)

        assert isinstance(df, pd.DataFrame)
        assert isinstance(df.columns, pd.MultiIndex)
        assert len(df) == 1  # Single grid
        assert len(df.columns) == 1397  # Expected number of columns

    def test_detect_old_csv_version(self, old_csv_path):
        """Test version detection on provided CSV."""
        df = load_df_state_file(old_csv_path)
        version = detect_df_state_version(df)

        assert version == "old"

    def test_convert_old_csv(self, old_csv_path):
        """Test converting the provided old CSV."""
        df_old = load_df_state_file(old_csv_path)
        df_new = convert_df_state_format(df_old)

        # Check structure
        assert len(df_new) == len(df_old)
        assert len(df_new.columns) == 1423  # New format has 1423 columns (1420 + 3 view factors from master)

        # Check old columns removed
        col_names = {
            col[0] if isinstance(col, tuple) else col for col in df_new.columns
        }
        assert "age_0_4" not in col_names
        assert "age_5_11" not in col_names

        # Check new columns added
        assert "buildingname" in col_names
        assert "buildingtype" in col_names

    @pytest.mark.slow
    def test_full_yaml_conversion(self, old_csv_path, tmp_path):
        """Test full conversion from old CSV to YAML."""
        output_yaml = tmp_path / "config.yml"

        # Convert to YAML
        convert_to_yaml(
            input_file=str(old_csv_path),
            output_file=str(output_yaml),
        )

        # Check output exists
        assert output_yaml.exists()

        # Check it's valid YAML
        import yaml

        with open(output_yaml) as f:
            config = yaml.safe_load(f)

        assert "sites" in config
        assert len(config["sites"]) == 1
