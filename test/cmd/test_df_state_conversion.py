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


class TestDfStateVersionDetection:
    """Test df_state version detection."""

    def test_detect_old_format(self):
        """Test detection of old df_state format with age columns."""
        # Create mock old format df_state
        columns = pd.MultiIndex.from_tuples([
            ("age_0_4", "0"),
            ("age_5_11", "0"),
            ("age_12_18", "0"),
            ("other_col", "0"),
        ])
        df = pd.DataFrame([[1, 2, 3, 4]], columns=columns, index=[1])

        assert detect_df_state_version(df) == "old"

    def test_detect_new_format(self):
        """Test detection of df_state format that differs from current."""
        # Create mock df_state with different columns than current
        columns = pd.MultiIndex.from_tuples([
            ("buildingname", "0"),
            ("buildingtype", "0"),
            ("config", ""),
            ("description", ""),
            ("h_std", "0"),
            ("lambda_c", "0"),
            ("n_buildings", "0"),
            ("other_col", "0"),
        ])
        df = pd.DataFrame(
            [["b1", "res", "cfg", "desc", 1, 2, 3, 4]], columns=columns, index=[1]
        )

        # Since it doesn't match current template exactly, it should be "old"
        assert detect_df_state_version(df) == "old"

    def test_detect_current_format(self):
        """Test detection of current df_state format."""
        # Use actual SuPy template
        df_template, _ = sp.load_sample_data()

        version = detect_df_state_version(df_template)
        assert version in ["new", "current"]


class TestDfStateConversion:
    """Test df_state format conversion."""

    def test_convert_removes_old_columns(self):
        """Test that conversion removes deprecated columns."""
        # Create old format with deprecated columns
        old_cols = [
            ("age_0_4", "0"),
            ("age_5_11", "0"),
            ("age_12_18", "0"),
            ("age_19_64", "0"),
            ("age_65plus", "0"),
            ("hhs0", "0"),
            ("alt", "0"),  # Common column to preserve
        ]
        df_old = pd.DataFrame(
            [[1, 2, 3, 4, 5, 6, 100]],
            columns=pd.MultiIndex.from_tuples(old_cols),
            index=[1],
        )

        df_new = convert_df_state_format(df_old)

        # Check old columns are removed
        col_names = {
            col[0] if isinstance(col, tuple) else col for col in df_new.columns
        }
        assert "age_0_4" not in col_names
        assert "age_5_11" not in col_names
        assert "hhs0" not in col_names

        # Check common column is preserved
        assert any("alt" in str(col) for col in df_new.columns)

    def test_convert_adds_new_columns(self):
        """Test that conversion adds new required columns."""
        # Create minimal old format
        df_old = pd.DataFrame(
            [[100]], columns=pd.MultiIndex.from_tuples([("alt", "0")]), index=[1]
        )

        df_new = convert_df_state_format(df_old)

        # Check new columns are added
        col_names = {
            col[0] if isinstance(col, tuple) else col for col in df_new.columns
        }
        assert "buildingname" in col_names
        assert "buildingtype" in col_names
        assert "config" in col_names
        assert "description" in col_names
        assert "h_std" in col_names
        assert "lambda_c" in col_names
        assert "n_buildings" in col_names

    def test_converted_passes_validation(self):
        """Test that converted df_state passes SuPy validation."""
        # Get template and modify to create old format
        df_template, _ = sp.load_sample_data()

        # Simulate old format by removing new columns
        cols_to_keep = [
            col
            for col in df_template.columns
            if not any(
                x in str(col[0]) if isinstance(col, tuple) else str(col)
                for x in [
                    "buildingname",
                    "buildingtype",
                    "config",
                    "description",
                    "h_std",
                    "lambda_c",
                    "n_buildings",
                ]
            )
        ]

        # Add old columns
        df_old = df_template[cols_to_keep].copy()
        for col_name in [
            "age_0_4",
            "age_5_11",
            "age_12_18",
            "age_19_64",
            "age_65plus",
            "hhs0",
        ]:
            df_old[(col_name, "0")] = 10

        # Convert
        df_new = convert_df_state_format(df_old)

        # Validate
        is_valid, message = validate_converted_df_state(df_new)
        assert is_valid or "warning" in message.lower()


class TestCsvFileConversion:
    """Test conversion of actual CSV file."""

    @pytest.fixture
    def old_csv_path(self):
        """Path to the provided old format CSV."""
        # This is the CSV file provided by the user
        csv_path = Path(
            "/Users/tingsun/Library/Application Support/com.conductor.app/uploads/originals/932628f2-d84e-48d2-84b2-fd2c2d81c49b.csv"
        )
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
        assert len(df_new.columns) == 1398  # New format has 1398 columns

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
