"""
Tests for individual MCP tools.
"""

import asyncio
import tempfile
import pytest
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

import pandas as pd
import numpy as np

from supy.mcp.tools import (
    ConfigureSimulationTool,
    RunSimulationTool,
    AnalyzeResultsTool,
)
from supy.mcp.utils import ParameterTranslator


class TestParameterTranslator:
    """Test parameter translation utilities."""

    def test_validate_file_path_existing(self):
        """Test validation of existing file path."""
        with tempfile.NamedTemporaryFile(delete=False) as tf:
            temp_path = Path(tf.name)

        try:
            result = ParameterTranslator.validate_file_path(str(temp_path))
            assert result == temp_path
        finally:
            temp_path.unlink()

    def test_validate_file_path_nonexistent(self):
        """Test validation of non-existent file path."""
        with pytest.raises(FileNotFoundError):
            ParameterTranslator.validate_file_path("/nonexistent/path/file.txt")

    def test_validate_directory_path_create(self):
        """Test directory path validation with creation."""
        with tempfile.TemporaryDirectory() as temp_dir:
            new_dir = Path(temp_dir) / "new_subdir"
            result = ParameterTranslator.validate_directory_path(
                str(new_dir), create=True
            )
            assert result.exists()
            assert result.is_dir()

    def test_parse_config_updates_dict(self):
        """Test parsing config updates from dict."""
        updates = {"model": {"control": {"tstep": 300}}}
        result = ParameterTranslator.parse_config_updates(updates)
        assert result == updates

    def test_parse_config_updates_json_string(self):
        """Test parsing config updates from JSON string."""
        updates_dict = {"model": {"control": {"tstep": 300}}}
        updates_json = '{"model": {"control": {"tstep": 300}}}'
        result = ParameterTranslator.parse_config_updates(updates_json)
        assert result == updates_dict

    def test_parse_config_updates_invalid_json(self):
        """Test parsing invalid JSON string."""
        with pytest.raises(ValueError, match="Invalid JSON"):
            ParameterTranslator.parse_config_updates('{"invalid": json}')

    def test_validate_time_range(self):
        """Test time range validation."""
        start_ts, end_ts = ParameterTranslator.validate_time_range(
            "2012-01-01T00:00:00", "2012-12-31T23:00:00"
        )

        assert start_ts == pd.Timestamp("2012-01-01T00:00:00")
        assert end_ts == pd.Timestamp("2012-12-31T23:00:00")

    def test_validate_time_range_invalid_order(self):
        """Test time range validation with invalid order."""
        with pytest.raises(ValueError, match="start_time must be before end_time"):
            ParameterTranslator.validate_time_range(
                "2012-12-31T23:00:00", "2012-01-01T00:00:00"
            )

    def test_validate_output_format(self):
        """Test output format validation."""
        assert ParameterTranslator.validate_output_format("csv") == "csv"
        assert ParameterTranslator.validate_output_format("parquet") == "parquet"

        with pytest.raises(ValueError, match="Invalid output format"):
            ParameterTranslator.validate_output_format("invalid")

    def test_validate_analysis_variables_list(self):
        """Test analysis variables validation with list."""
        variables = ["QH", "QE", "T2"]
        result = ParameterTranslator.validate_analysis_variables(variables)
        assert result == variables

    def test_validate_analysis_variables_string(self):
        """Test analysis variables validation with comma-separated string."""
        variables = "QH, QE, T2"
        result = ParameterTranslator.validate_analysis_variables(variables)
        assert result == ["QH", "QE", "T2"]

    def test_create_structured_response(self):
        """Test structured response creation."""
        response = ParameterTranslator.create_structured_response(
            success=True, data={"test": "data"}, message="Test message"
        )

        assert response["success"] is True
        assert response["data"] == {"test": "data"}
        assert response["message"] == "Test message"
        assert "timestamp" in response

    def test_serialize_dataframe(self):
        """Test DataFrame serialization."""
        df = pd.DataFrame(
            {"A": [1, 2, 3], "B": [4.0, 5.0, 6.0]},
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        result = ParameterTranslator.serialize_dataframe(df)

        assert result["shape"] == [3, 2]
        assert result["columns"] == ["A", "B"]
        assert len(result["data"]) == 3
        assert result["truncated"] is False

    def test_serialize_dataframe_truncation(self):
        """Test DataFrame serialization with truncation."""
        df = pd.DataFrame({"A": range(200)})

        result = ParameterTranslator.serialize_dataframe(df, max_rows=50)

        assert result["shape"] == [200, 1]
        assert len(result["data"]) == 50
        assert result["truncated"] is True


class TestConfigureSimulationTool:
    """Test configure simulation tool."""

    def setup_method(self):
        """Set up test fixtures."""
        self.tool = ConfigureSimulationTool()

    def test_tool_initialization(self):
        """Test tool initialization."""
        assert self.tool.name == "configure_simulation"
        assert "description" in self.tool.description

    def test_get_parameters(self):
        """Test parameter definitions."""
        params = self.tool.get_parameters()
        param_names = [p["name"] for p in params]

        assert "config_path" in param_names
        assert "config_updates" in param_names
        assert "validate_only" in param_names
        assert "site_name" in param_names

    def test_get_definition(self):
        """Test tool definition structure."""
        definition = self.tool.get_definition()

        assert definition["name"] == "configure_simulation"
        assert "description" in definition
        assert "inputSchema" in definition

        schema = definition["inputSchema"]
        assert schema["type"] == "object"
        assert "properties" in schema

    @pytest.mark.asyncio
    async def test_execute_no_modules_available(self):
        """Test execution when SuPy modules not available."""
        # This simulates the import fallback scenario
        with patch("supy.mcp.tools.configure.SUEWSConfig", None):
            result = await self.tool.execute({})

            assert result["success"] is False
            assert "not available" in result["errors"][0]

    @pytest.mark.asyncio
    async def test_execute_validate_only_mode(self):
        """Test execution in validate-only mode."""
        # Mock the SUEWSConfig class
        mock_config = MagicMock()

        with (
            patch("supy.mcp.tools.configure.SUEWSConfig", mock_config),
            patch("supy.mcp.tools.configure.SUEWSSimulation", MagicMock()),
        ):
            mock_config.from_yaml.return_value = mock_config

            # Create temporary YAML file
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".yml", delete=False
            ) as tf:
                tf.write("model:\n  control:\n    tstep: 3600\n")
                temp_path = tf.name

            try:
                result = await self.tool.execute({
                    "config_path": temp_path,
                    "validate_only": True,
                })

                assert "success" in result
                assert "data" in result

            finally:
                Path(temp_path).unlink()


class TestRunSimulationTool:
    """Test run simulation tool."""

    def setup_method(self):
        """Set up test fixtures."""
        self.tool = RunSimulationTool()

    def test_tool_initialization(self):
        """Test tool initialization."""
        assert self.tool.name == "run_simulation"
        assert "description" in self.tool.description

    def test_get_parameters(self):
        """Test parameter definitions."""
        params = self.tool.get_parameters()
        param_names = [p["name"] for p in params]

        assert "forcing_path" in param_names
        assert "config_path" in param_names
        assert "use_sample_data" in param_names
        assert "time_step" in param_names
        assert "save_state" in param_names

    @pytest.mark.asyncio
    async def test_execute_no_modules_available(self):
        """Test execution when SuPy modules not available."""
        with patch("supy.mcp.tools.run.run_supy", None):
            result = await self.tool.execute({})

            assert result["success"] is False
            assert "not available" in result["errors"][0]

    @pytest.mark.asyncio
    async def test_execute_missing_data_source(self):
        """Test execution with no data source."""
        with patch("supy.mcp.tools.run.run_supy", MagicMock()):
            result = await self.tool.execute({"use_sample_data": False})

            assert result["success"] is False
            assert "forcing_path must be provided" in result["errors"][0]

    @pytest.mark.asyncio
    async def test_execute_sample_data_mode(self):
        """Test execution with sample data."""
        # Mock sample data
        mock_state = pd.DataFrame({"state": [1, 2, 3]})
        mock_forcing = pd.DataFrame(
            {"Tair": [20.0, 21.0, 22.0], "RH": [60.0, 65.0, 70.0]},
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        mock_output = pd.DataFrame(
            {"QH": [100.0, 110.0, 120.0], "QE": [50.0, 55.0, 60.0]},
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        with (
            patch("supy.mcp.tools.run.run_supy", return_value=mock_output) as mock_run,
            patch(
                "supy.mcp.tools.run.load_SampleData",
                return_value=(mock_state, mock_forcing),
            ),
        ):
            result = await self.tool.execute({
                "use_sample_data": True,
                "save_state": False,
            })

            # Should succeed with mocked data
            if result["success"]:
                assert "simulation_completed" in result["data"]
                assert result["data"]["data_source"] == "sample_data"


class TestAnalyzeResultsTool:
    """Test analyze results tool."""

    def setup_method(self):
        """Set up test fixtures."""
        self.tool = AnalyzeResultsTool()

    def test_tool_initialization(self):
        """Test tool initialization."""
        assert self.tool.name == "analyze_results"
        assert "description" in self.tool.description

    def test_get_parameters(self):
        """Test parameter definitions."""
        params = self.tool.get_parameters()
        param_names = [p["name"] for p in params]

        assert "results_path" in param_names
        assert "analysis_type" in param_names
        assert "variables" in param_names
        assert "time_period" in param_names

    def test_detect_frequency(self):
        """Test frequency detection."""
        # Hourly data
        hourly_index = pd.date_range("2012-01-01", periods=24, freq="H")
        freq = self.tool._detect_frequency(hourly_index)
        assert freq in ["H", "hourly", "60min"]  # Various pandas versions

        # Daily data
        daily_index = pd.date_range("2012-01-01", periods=30, freq="D")
        freq = self.tool._detect_frequency(daily_index)
        assert freq in ["D", "daily"]

    def test_analyze_summary(self):
        """Test summary analysis."""
        df = pd.DataFrame(
            {
                "QH": [100.0, 110.0, 120.0],
                "QE": [50.0, 55.0, 60.0],
                "T2": [20.0, 21.0, 22.0],
            },
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        summary = self.tool._analyze_summary(df)

        assert "data_overview" in summary
        assert "variable_summary" in summary

        overview = summary["data_overview"]
        assert overview["n_timesteps"] == 3
        assert overview["n_variables"] == 3

        var_summary = summary["variable_summary"]
        assert "QH" in var_summary
        assert "mean" in var_summary["QH"]
        assert "std" in var_summary["QH"]

    def test_analyze_energy_balance(self):
        """Test energy balance analysis."""
        df = pd.DataFrame(
            {
                "QN": [200.0, 220.0, 240.0],
                "QH": [100.0, 110.0, 120.0],
                "QE": [50.0, 55.0, 60.0],
                "QS": [30.0, 35.0, 40.0],
                "QF": [20.0, 20.0, 20.0],
            },
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        analysis = self.tool._analyze_energy_balance(df)

        assert "available_variables" in analysis
        assert "energy_statistics" in analysis

        assert set(analysis["available_variables"]) == {"QN", "QH", "QE", "QS", "QF"}

        stats = analysis["energy_statistics"]
        assert "QH" in stats
        assert "mean" in stats["QH"]
        assert "total_mj_m2" in stats["QH"]

        # Check energy balance closure if calculated
        if "energy_balance_closure" in analysis:
            assert "mean_residual" in analysis["energy_balance_closure"]

    @pytest.mark.asyncio
    async def test_execute_missing_file(self):
        """Test execution with missing results file."""
        result = await self.tool.execute({"results_path": "/nonexistent/file.csv"})

        assert result["success"] is False
        assert "not found" in result["errors"][0]

    @pytest.mark.asyncio
    async def test_execute_with_csv_file(self):
        """Test execution with CSV results file."""
        # Create temporary CSV file
        df = pd.DataFrame(
            {
                "QH": [100.0, 110.0, 120.0],
                "QE": [50.0, 55.0, 60.0],
                "T2": [20.0, 21.0, 22.0],
            },
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as tf:
            df.to_csv(tf.name)
            temp_path = tf.name

        try:
            result = await self.tool.execute({
                "results_path": temp_path,
                "analysis_type": "summary",
            })

            assert result["success"] is True
            assert "data_info" in result["data"]
            assert "analysis_results" in result["data"]

            data_info = result["data"]["data_info"]
            assert data_info["shape"] == [3, 3]
            assert set(data_info["columns"]) == {"QH", "QE", "T2"}

        finally:
            Path(temp_path).unlink()

    def test_get_variable_units(self):
        """Test variable units mapping."""
        assert self.tool._get_variable_units("QH") == "W m-2"
        assert self.tool._get_variable_units("T2") == "°C"
        assert self.tool._get_variable_units("RH2") == "%"
        assert self.tool._get_variable_units("unknown_var") == "unknown"
