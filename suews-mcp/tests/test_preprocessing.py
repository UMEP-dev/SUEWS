"""Tests for SUEWS MCP preprocessing tools."""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path
import tempfile
import yaml

from suews_mcp.preprocessing import (
    ForcingDataPreprocessor,
    ConfigValidator,
    DataFormatConverter,
    DataQualityIssue,
    PreprocessingResult,
)


class TestDataQualityIssue:
    """Test DataQualityIssue class."""

    def test_issue_creation(self):
        issue = DataQualityIssue(
            issue_type="missing_data",
            message="Missing temperature values",
            severity="warning",
            location="column: Tair",
            value=10,
        )

        assert issue.issue_type == "missing_data"
        assert issue.message == "Missing temperature values"
        assert issue.severity == "warning"
        assert issue.location == "column: Tair"
        assert issue.value == 10

    def test_issue_to_dict(self):
        issue = DataQualityIssue("test_issue", "Test message", "error")
        issue_dict = issue.to_dict()

        assert issue_dict["type"] == "test_issue"
        assert issue_dict["message"] == "Test message"
        assert issue_dict["severity"] == "error"
        assert "timestamp" in issue_dict


class TestPreprocessingResult:
    """Test PreprocessingResult class."""

    def test_result_initialization(self):
        result = PreprocessingResult()

        assert result.success is True
        assert result.issues == []
        assert result.data is None
        assert result.metadata == {}
        assert result.processing_log == []

    def test_add_issue(self):
        result = PreprocessingResult()
        issue = DataQualityIssue("test_issue", "Test message", "warning")

        result.add_issue(issue)

        assert len(result.issues) == 1
        assert result.issues[0] == issue
        assert result.success is True  # Warnings don't fail

    def test_add_error_issue(self):
        result = PreprocessingResult()
        issue = DataQualityIssue("test_error", "Error message", "error")

        result.add_issue(issue)

        assert len(result.issues) == 1
        assert result.success is False  # Errors cause failure

    def test_add_log(self):
        result = PreprocessingResult()

        result.add_log("Test log message")

        assert len(result.processing_log) == 1
        assert "Test log message" in result.processing_log[0]

    def test_get_summary(self):
        result = PreprocessingResult()
        result.add_issue(DataQualityIssue("test", "Test error", "error"))
        result.add_issue(DataQualityIssue("test", "Test warning", "warning"))
        result.add_issue(DataQualityIssue("test", "Test info", "info"))
        result.data = pd.DataFrame({"a": [1, 2, 3]})
        result.metadata = {"test": "value"}
        result.add_log("Test log")

        summary = result.get_summary()

        assert summary["success"] is False
        assert summary["total_issues"] == 3
        assert summary["errors"] == 1
        assert summary["warnings"] == 1
        assert summary["info"] == 1
        assert summary["data_shape"] == (3, 1)
        assert summary["metadata"] == {"test": "value"}
        assert summary["processing_steps"] == 1


class TestForcingDataPreprocessor:
    """Test ForcingDataPreprocessor class."""

    def create_sample_forcing_data(self, with_issues=False):
        """Create sample forcing data for testing."""
        n_rows = 24  # One day of hourly data
        data = {
            "iy": [2023] * n_rows,
            "id": [1] * n_rows,
            "it": list(range(n_rows)),
            "imin": [0] * n_rows,
            "qn": np.random.normal(100, 50, n_rows),
            "qh": np.random.normal(50, 30, n_rows),
            "qe": np.random.normal(30, 20, n_rows),
            "qs": np.random.normal(20, 10, n_rows),
            "qf": np.random.normal(10, 5, n_rows),
            "U": np.random.uniform(1, 10, n_rows),
            "RH": np.random.uniform(30, 90, n_rows),
            "Tair": np.random.normal(15, 5, n_rows),
            "pres": np.random.uniform(98, 102, n_rows),
            "rain": np.random.exponential(0.1, n_rows),
            "kdown": np.maximum(0, np.random.normal(200, 100, n_rows)),
        }

        if with_issues:
            # Add some data issues for testing
            data["Tair"][5:8] = -999  # Missing data
            data["RH"][10] = 150  # Out of range
            data["U"][15] = -5  # Invalid negative wind speed
            data["kdown"][20] = -50  # Invalid negative solar radiation

        return pd.DataFrame(data)

    def test_preprocessor_initialization(self):
        preprocessor = ForcingDataPreprocessor()
        assert preprocessor.result is not None
        assert isinstance(preprocessor.result, PreprocessingResult)

    @pytest.fixture
    def sample_data_file(self, tmp_path):
        """Create a temporary forcing data file."""
        df = self.create_sample_forcing_data()
        file_path = tmp_path / "sample_forcing.txt"
        df.to_csv(file_path, sep=" ", index=False, float_format="%.2f")
        return file_path

    @pytest.fixture
    def sample_data_file_with_issues(self, tmp_path):
        """Create a temporary forcing data file with data quality issues."""
        df = self.create_sample_forcing_data(with_issues=True)
        file_path = tmp_path / "sample_forcing_issues.txt"
        df.to_csv(file_path, sep=" ", index=False, float_format="%.2f")
        return file_path

    def test_load_forcing_data_success(self, sample_data_file):
        preprocessor = ForcingDataPreprocessor()
        df = preprocessor._load_forcing_data(sample_data_file)

        assert df is not None
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 24
        assert "iy" in df.columns
        assert "Tair" in df.columns

    def test_load_forcing_data_missing_file(self):
        preprocessor = ForcingDataPreprocessor()
        df = preprocessor._load_forcing_data(Path("nonexistent_file.txt"))

        assert df is None
        assert len(preprocessor.result.issues) > 0
        assert any(
            "Cannot read file" in issue.message for issue in preprocessor.result.issues
        )

    def test_validate_data_structure(self, sample_data_file):
        preprocessor = ForcingDataPreprocessor()
        df = preprocessor._load_forcing_data(sample_data_file)

        preprocessor._validate_data_structure(df)

        # Should not have any issues with valid data
        structure_issues = [
            i for i in preprocessor.result.issues if "missing" in i.issue_type
        ]
        assert len(structure_issues) == 0

    def test_validate_data_structure_missing_columns(self):
        preprocessor = ForcingDataPreprocessor()
        df = pd.DataFrame({"iy": [2023], "incomplete": [1]})  # Missing required columns

        preprocessor._validate_data_structure(df)

        missing_col_issues = [
            i for i in preprocessor.result.issues if "missing_columns" in i.issue_type
        ]
        assert len(missing_col_issues) > 0

    def test_validate_data_ranges(self, sample_data_file_with_issues):
        preprocessor = ForcingDataPreprocessor()
        df = preprocessor._load_forcing_data(sample_data_file_with_issues)

        preprocessor._validate_data_ranges(df)

        # Should find out of range values
        range_issues = [
            i for i in preprocessor.result.issues if "out_of_range" in i.issue_type
        ]
        assert len(range_issues) > 0

    def test_check_missing_data(self, sample_data_file_with_issues):
        preprocessor = ForcingDataPreprocessor()
        df = preprocessor._load_forcing_data(sample_data_file_with_issues)

        preprocessor._check_missing_data(df)

        # Should find missing data
        missing_issues = [
            i for i in preprocessor.result.issues if "missing_data" in i.issue_type
        ]
        assert len(missing_issues) > 0

    def test_validate_energy_balance(self, sample_data_file):
        preprocessor = ForcingDataPreprocessor()
        df = preprocessor._load_forcing_data(sample_data_file)

        preprocessor._validate_energy_balance(df)

        # Should calculate energy balance statistics
        assert (
            "energy_balance_stats" in preprocessor.result.metadata
            or len(preprocessor.result.issues) > 0
        )

    def test_apply_unit_conversions(self, sample_data_file):
        preprocessor = ForcingDataPreprocessor()
        df = preprocessor._load_forcing_data(sample_data_file)

        # Modify pressure to hPa
        df["pres"] = df["pres"] * 10  # Convert kPa to hPa

        df_converted = preprocessor._apply_unit_conversions(df)

        # Should convert back to kPa
        assert df_converted["pres"].mean() < 200  # Should be in kPa range

    def test_auto_fix_data_issues(self, sample_data_file_with_issues):
        preprocessor = ForcingDataPreprocessor()
        df = preprocessor._load_forcing_data(sample_data_file_with_issues)

        df_fixed = preprocessor._auto_fix_data_issues(df)

        # Should fix negative solar radiation
        assert all(df_fixed["kdown"] >= 0)

        # Should have logged fixes
        assert len(preprocessor.result.processing_log) > 0

    def test_preprocess_forcing_file_success(self, sample_data_file, tmp_path):
        preprocessor = ForcingDataPreprocessor()
        output_file = tmp_path / "processed_forcing.txt"

        result = preprocessor.preprocess_forcing_file(
            file_path=sample_data_file,
            output_path=output_file,
            validate_energy_balance=True,
            auto_fix_issues=False,
        )

        assert result.success is True
        assert result.data is not None
        assert len(result.processing_log) > 0
        assert output_file.exists()

    def test_preprocess_forcing_file_with_auto_fix(
        self, sample_data_file_with_issues, tmp_path
    ):
        preprocessor = ForcingDataPreprocessor()
        output_file = tmp_path / "processed_forcing.txt"

        result = preprocessor.preprocess_forcing_file(
            file_path=sample_data_file_with_issues,
            output_path=output_file,
            validate_energy_balance=True,
            auto_fix_issues=True,
        )

        assert result.data is not None
        # May still have some issues but should be processed
        assert len(result.processing_log) > 0
        assert output_file.exists()


class TestConfigValidator:
    """Test ConfigValidator class."""

    def create_sample_config(self, with_issues=False):
        """Create sample SUEWS configuration."""
        config = {
            "name": "test_config",
            "description": "Test configuration",
            "model": {
                "control": {"tstep": 300, "forcing_file": {"value": "forcing/"}},
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "roughlenmommethod": {"value": 1},
                },
            },
            "sites": [
                {
                    "name": "test_site",
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10.0},
                    },
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.4}},
                        "bldgs": {"sfr": {"value": 0.3}},
                        "grass": {"sfr": {"value": 0.3}},
                    },
                }
            ],
        }

        if with_issues:
            # Add some validation issues
            config["model"]["control"]["tstep"] = -100  # Invalid timestep
            config["sites"][0]["properties"]["lat"]["value"] = 100  # Invalid latitude
            config["sites"][0]["land_cover"]["paved"]["sfr"]["value"] = (
                0.8  # Fractions don't sum to 1
            )

        return config

    @pytest.fixture
    def sample_config_file(self, tmp_path):
        """Create a temporary config file."""
        config = self.create_sample_config()
        file_path = tmp_path / "test_config.yml"
        with open(file_path, "w") as f:
            yaml.dump(config, f)
        return file_path

    @pytest.fixture
    def sample_config_file_with_issues(self, tmp_path):
        """Create a temporary config file with issues."""
        config = self.create_sample_config(with_issues=True)
        file_path = tmp_path / "test_config_issues.yml"
        with open(file_path, "w") as f:
            yaml.dump(config, f)
        return file_path

    def test_validator_initialization(self):
        validator = ConfigValidator()
        assert validator.result is not None
        assert isinstance(validator.result, PreprocessingResult)

    def test_load_config_success(self, sample_config_file):
        validator = ConfigValidator()
        config_data = validator._load_config(sample_config_file)

        assert config_data is not None
        assert "name" in config_data
        assert "model" in config_data
        assert "sites" in config_data

    def test_load_config_missing_file(self):
        validator = ConfigValidator()
        config_data = validator._load_config(Path("nonexistent_config.yml"))

        assert config_data is None
        assert len(validator.result.issues) > 0

    def test_validate_config_structure_success(self, sample_config_file):
        validator = ConfigValidator()
        config_data = validator._load_config(sample_config_file)

        validator._validate_config_structure(config_data)

        # Should not have structural issues
        structure_issues = [
            i
            for i in validator.result.issues
            if "missing" in i.issue_type and "section" in i.issue_type
        ]
        assert len(structure_issues) == 0

    def test_validate_config_structure_missing_sections(self):
        validator = ConfigValidator()
        config_data = {"incomplete": "config"}  # Missing required sections

        validator._validate_config_structure(config_data)

        structure_issues = [
            i for i in validator.result.issues if "missing" in i.issue_type
        ]
        assert len(structure_issues) > 0

    def test_validate_required_fields(self, sample_config_file):
        validator = ConfigValidator()
        config_data = validator._load_config(sample_config_file)

        validator._validate_required_fields(config_data, strict_mode=False)

        # With complete config, should have no missing field issues
        missing_field_issues = [
            i
            for i in validator.result.issues
            if "missing_required_field" in i.issue_type
        ]
        assert len(missing_field_issues) == 0

    def test_validate_value_ranges_with_issues(self, sample_config_file_with_issues):
        validator = ConfigValidator()
        config_data = validator._load_config(sample_config_file_with_issues)

        validator._validate_value_ranges(config_data, strict_mode=True)

        # Should find invalid values
        invalid_issues = [
            i for i in validator.result.issues if "invalid" in i.issue_type
        ]
        assert len(invalid_issues) > 0

    def test_validate_surface_fractions_invalid_sum(
        self, sample_config_file_with_issues
    ):
        validator = ConfigValidator()
        config_data = validator._load_config(sample_config_file_with_issues)

        validator._validate_surface_fractions(config_data)

        # Should find surface fraction sum issue
        fraction_issues = [
            i for i in validator.result.issues if "fraction" in i.issue_type
        ]
        assert len(fraction_issues) > 0

    def test_validate_config_success(self, sample_config_file):
        validator = ConfigValidator()

        result = validator.validate_config(
            config_path=sample_config_file,
            strict_mode=False,
            check_file_paths=False,  # Skip file checks for test
        )

        assert result.success is True
        assert len(result.processing_log) > 0
        assert "config_file" in result.metadata

    def test_validate_config_with_issues(self, sample_config_file_with_issues):
        validator = ConfigValidator()

        result = validator.validate_config(
            config_path=sample_config_file_with_issues,
            strict_mode=True,
            check_file_paths=False,
        )

        assert result.success is False  # Should fail due to issues
        assert len(result.issues) > 0
        errors = [i for i in result.issues if i.severity == "error"]
        assert len(errors) > 0


class TestDataFormatConverter:
    """Test DataFormatConverter class."""

    def create_sample_data(self):
        """Create sample meteorological data."""
        return pd.DataFrame(
            {
                "year": [2023] * 24,
                "day": [1] * 24,
                "hour": list(range(24)),
                "minute": [0] * 24,
                "temperature": np.random.normal(15, 5, 24),
                "humidity": np.random.uniform(30, 90, 24),
                "wind_speed": np.random.uniform(1, 10, 24),
                "solar_radiation": np.maximum(0, np.random.normal(200, 100, 24)),
            }
        )

    @pytest.fixture
    def sample_csv_file(self, tmp_path):
        """Create a temporary CSV file."""
        df = self.create_sample_data()
        file_path = tmp_path / "sample_data.csv"
        df.to_csv(file_path, index=False)
        return file_path

    @pytest.fixture
    def sample_txt_file(self, tmp_path):
        """Create a temporary TXT file."""
        df = self.create_sample_data()
        file_path = tmp_path / "sample_data.txt"
        df.to_csv(file_path, sep=" ", index=False, float_format="%.2f")
        return file_path

    def test_converter_initialization(self):
        converter = DataFormatConverter()
        assert converter.result is not None
        assert isinstance(converter.result, PreprocessingResult)

    def test_load_data_by_format_csv(self, sample_csv_file):
        converter = DataFormatConverter()
        df = converter._load_data_by_format(sample_csv_file, "csv")

        assert df is not None
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 24
        assert "temperature" in df.columns

    def test_load_data_by_format_txt(self, sample_txt_file):
        converter = DataFormatConverter()
        df = converter._load_data_by_format(sample_txt_file, "txt")

        assert df is not None
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 24

    def test_load_data_unsupported_format(self, sample_csv_file):
        converter = DataFormatConverter()
        df = converter._load_data_by_format(sample_csv_file, "unsupported")

        assert df is None
        assert len(converter.result.issues) > 0

    def test_apply_format_transformations(self, sample_csv_file):
        converter = DataFormatConverter()
        df = converter._load_data_by_format(sample_csv_file, "csv")

        # Test column mapping
        column_mapping = {"temperature": "Tair", "humidity": "RH", "wind_speed": "U"}

        df_transformed = converter._apply_format_transformations(
            df, "csv", "suews_txt", column_mapping=column_mapping
        )

        assert "Tair" in df_transformed.columns
        assert "RH" in df_transformed.columns
        assert "U" in df_transformed.columns
        assert "temperature" not in df_transformed.columns

    def test_convert_format_csv_to_txt(self, sample_csv_file, tmp_path):
        converter = DataFormatConverter()
        output_file = tmp_path / "converted_data.txt"

        result = converter.convert_format(
            input_path=sample_csv_file,
            output_path=output_file,
            input_format="csv",
            output_format="txt",
        )

        assert result.success is True
        assert output_file.exists()
        assert result.data is not None
        assert "conversion_timestamp" in result.metadata

    def test_convert_format_with_column_mapping(self, sample_csv_file, tmp_path):
        converter = DataFormatConverter()
        output_file = tmp_path / "converted_mapped.txt"

        column_mapping = {"temperature": "Tair", "humidity": "RH"}

        result = converter.convert_format(
            input_path=sample_csv_file,
            output_path=output_file,
            input_format="csv",
            output_format="suews_txt",
            column_mapping=column_mapping,
        )

        assert result.success is True
        assert output_file.exists()

        # Check that mapping was applied
        converted_df = pd.read_csv(output_file, sep=" ")
        assert "Tair" in converted_df.columns
        assert "RH" in converted_df.columns

    def test_convert_format_missing_parameters(self):
        converter = DataFormatConverter()

        result = converter.convert_format(
            input_path="test.csv",
            output_path="",  # Missing output path
            input_format="csv",
            output_format="",  # Missing output format
        )

        assert result.success is False
        assert len(result.issues) > 0


class TestIntegration:
    """Integration tests for preprocessing tools."""

    @pytest.fixture
    def benchmark_forcing_file(self):
        """Use the actual benchmark forcing file for integration testing."""
        benchmark_path = (
            Path(__file__).parent.parent.parent
            / "test"
            / "fixtures"
            / "benchmark1"
            / "forcing"
            / "Kc1_2011_data_5.txt"
        )
        if benchmark_path.exists():
            return benchmark_path
        else:
            pytest.skip(f"Benchmark file not found: {benchmark_path}")

    def test_preprocess_real_forcing_data(self, benchmark_forcing_file, tmp_path):
        """Test preprocessing with real SUEWS forcing data."""
        if not benchmark_forcing_file.exists():
            pytest.skip("Benchmark forcing file not available")

        preprocessor = ForcingDataPreprocessor()
        output_file = tmp_path / "processed_benchmark.txt"

        result = preprocessor.preprocess_forcing_file(
            file_path=benchmark_forcing_file,
            output_path=output_file,
            validate_energy_balance=True,
            auto_fix_issues=True,
        )

        # Should process without critical errors
        assert result.data is not None
        assert len(result.processing_log) > 0

        # Check that required columns exist
        assert all(col in result.data.columns for col in ["iy", "id", "it", "imin"])

        # Should detect reasonable timestep
        if "detected_timestep_seconds" in result.metadata:
            timestep = result.metadata["detected_timestep_seconds"]
            assert 300 <= timestep <= 3600  # Reasonable timestep range

        # Output file should exist and be readable
        assert output_file.exists()

        # Test that output can be read back
        df_output = pd.read_csv(output_file, sep=" ")
        assert len(df_output) > 0

    def test_end_to_end_data_workflow(self, tmp_path):
        """Test complete workflow: generate data -> preprocess -> convert -> validate."""

        # Step 1: Create sample data
        n_rows = 48  # 2 days of hourly data
        sample_data = pd.DataFrame(
            {
                "year": [2023] * n_rows,
                "doy": [1] * 24 + [2] * 24,
                "hour": list(range(24)) * 2,
                "min": [0] * n_rows,
                "temp_air": np.random.normal(15, 5, n_rows),
                "rel_hum": np.random.uniform(30, 90, n_rows),
                "wind_spd": np.random.uniform(1, 10, n_rows),
                "net_rad": np.random.normal(100, 50, n_rows),
                "sens_heat": np.random.normal(50, 30, n_rows),
                "lat_heat": np.random.normal(30, 20, n_rows),
                "storage_heat": np.random.normal(20, 10, n_rows),
                "anthro_heat": np.random.uniform(5, 15, n_rows),
                "pressure": np.random.uniform(98, 102, n_rows),
                "precip": np.random.exponential(0.05, n_rows),
                "solar_down": np.maximum(0, np.random.normal(200, 100, n_rows)),
            }
        )

        # Save initial data
        input_file = tmp_path / "raw_data.csv"
        sample_data.to_csv(input_file, index=False)

        # Step 2: Convert to SUEWS format
        converter = DataFormatConverter()
        converted_file = tmp_path / "converted_data.txt"

        column_mapping = {
            "year": "iy",
            "doy": "id",
            "hour": "it",
            "min": "imin",
            "temp_air": "Tair",
            "rel_hum": "RH",
            "wind_spd": "U",
            "net_rad": "qn",
            "sens_heat": "qh",
            "lat_heat": "qe",
            "storage_heat": "qs",
            "anthro_heat": "qf",
            "pressure": "pres",
            "precip": "rain",
            "solar_down": "kdown",
        }

        convert_result = converter.convert_format(
            input_path=input_file,
            output_path=converted_file,
            input_format="csv",
            output_format="suews_txt",
            column_mapping=column_mapping,
        )

        assert convert_result.success is True
        assert converted_file.exists()

        # Step 3: Preprocess the converted data
        preprocessor = ForcingDataPreprocessor()
        processed_file = tmp_path / "processed_data.txt"

        preprocess_result = preprocessor.preprocess_forcing_file(
            file_path=converted_file,
            output_path=processed_file,
            validate_energy_balance=True,
            auto_fix_issues=True,
        )

        assert preprocess_result.data is not None
        assert processed_file.exists()

        # Step 4: Create and validate a sample config
        config = {
            "name": "integration_test",
            "description": "Integration test configuration",
            "model": {
                "control": {
                    "tstep": 3600,
                    "forcing_file": {"value": str(processed_file)},
                },
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "roughlenmommethod": {"value": 1},
                },
            },
            "sites": [
                {
                    "name": "test_site",
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10.0},
                    },
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.4}},
                        "bldgs": {"sfr": {"value": 0.3}},
                        "grass": {"sfr": {"value": 0.3}},
                    },
                }
            ],
        }

        config_file = tmp_path / "test_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(config, f)

        # Validate the configuration
        validator = ConfigValidator()
        validate_result = validator.validate_config(
            config_path=config_file,
            strict_mode=False,
            check_file_paths=True,  # Check that forcing file exists
        )

        # Should be valid (or have only minor warnings)
        assert validate_result.data is None  # Config validator doesn't return data
        # Allow some warnings but no critical errors
        errors = [i for i in validate_result.issues if i.severity == "error"]
        assert len(errors) == 0 or all(
            "missing_forcing_file" not in e.issue_type for e in errors
        )

        # Verify all files exist and are non-empty
        assert input_file.exists() and input_file.stat().st_size > 0
        assert converted_file.exists() and converted_file.stat().st_size > 0
        assert processed_file.exists() and processed_file.stat().st_size > 0
        assert config_file.exists() and config_file.stat().st_size > 0

        print(f"✅ End-to-end workflow completed successfully!")
        print(f"   • Raw data: {input_file}")
        print(f"   • Converted: {converted_file}")
        print(f"   • Processed: {processed_file}")
        print(f"   • Config: {config_file}")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
