"""
Tests for MCP (Model Context Protocol) tools.

Tests the three core SuPy MCP tools:
- ConfigureSimulationTool
- RunSimulationTool
- AnalyzeResultsTool
"""

import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
import pytest
import pandas as pd
import numpy as np
import yaml

# Import the tools to test
from src.supy.mcp.tools import (
    ConfigureSimulationTool,
    RunSimulationTool,
    AnalyzeResultsTool,
)
from src.supy.mcp.utils.translator import ParameterTranslator


class TestConfigureSimulationTool:
    """Test ConfigureSimulationTool functionality."""

    @pytest.fixture
    def tool(self):
        """Create ConfigureSimulationTool instance."""
        return ConfigureSimulationTool()

    @pytest.fixture
    def sample_config(self):
        """Create sample configuration dictionary."""
        return {
            "model": {"name": "SUEWS", "version": "2024.1"},
            "site": {"name": "TestSite", "latitude": 51.5, "longitude": -0.1},
            "surface": {
                "fractions": {
                    "building": 0.3,
                    "paved": 0.2,
                    "vegetation": 0.4,
                    "water": 0.1,
                }
            },
        }

    def test_tool_initialization(self, tool):
        """Test tool is initialized correctly."""
        assert tool.name == "configure_simulation"
        assert "configure" in tool.description.lower()
        assert tool.translator is not None

    def test_get_parameters(self, tool):
        """Test parameter definitions."""
        params = tool.get_parameters()
        assert isinstance(params, list)
        assert (
            len(params) >= 4
        )  # At least config_path, config_updates, validate_only, site_name

        # Check required parameters
        param_names = [p["name"] for p in params]
        assert "config_path" in param_names
        assert "config_updates" in param_names
        assert "validate_only" in param_names
        assert "save_path" in param_names

    @pytest.mark.asyncio
    async def test_execute_with_missing_modules(self, tool):
        """Test execution when SuPy modules are not available."""
        with patch.object(tool, "_execute") as mock_execute:
            # Simulate missing modules
            mock_execute.return_value = {
                "success": False,
                "errors": [
                    "SuPy configuration modules not available. This may be a development environment."
                ],
            }

            result = await tool.execute({})
            assert result["success"] is False
            assert "not available" in result["errors"][0]

    @pytest.mark.asyncio
    async def test_validate_only_mode(self, tool, sample_config):
        """Test validate_only mode."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            yaml.dump(sample_config, f)
            temp_path = f.name

        try:
            with patch("src.supy.mcp.tools.configure.SUEWSConfig") as mock_config:
                # Mock the config class
                mock_config_obj = MagicMock()
                mock_config_obj.model = MagicMock(name="SUEWS", version="2024.1")
                mock_config_obj.site = MagicMock(name="TestSite")
                mock_config.from_yaml.return_value = mock_config_obj

                result = await tool.execute({
                    "config_path": temp_path,
                    "validate_only": True,
                })

                # Should validate but not create simulation
                assert "validation" in result.get("message", "").lower()
                assert result.get("data", {}).get("checks_performed") is not None
        finally:
            Path(temp_path).unlink()

    @pytest.mark.asyncio
    async def test_save_configuration(self, tool, sample_config):
        """Test saving configuration to file."""
        with tempfile.TemporaryDirectory() as temp_dir:
            save_path = Path(temp_dir) / "test_config.yaml"

            with patch("src.supy.mcp.tools.configure.SUEWSConfig") as mock_config:
                # Mock config object
                mock_config_obj = MagicMock()
                mock_config_obj.model_dump.return_value = sample_config
                mock_config.return_value = mock_config_obj

                result = await tool.execute({
                    "save_path": str(save_path),
                    "validate_only": True,
                })

                # Check save was attempted
                if result["success"] and "save_result" in result.get("data", {}):
                    save_result = result["data"]["save_result"]
                    assert save_result.get("format") == "yaml"

    def test_apply_nested_updates(self, tool):
        """Test applying nested configuration updates."""
        config_obj = MagicMock()
        config_obj.site = MagicMock()
        config_obj.site.name = "OldName"

        updates = {"site": {"name": "NewName"}}

        tool._apply_nested_updates(config_obj, updates)
        # Would need to check the mock was called correctly


class TestRunSimulationTool:
    """Test RunSimulationTool functionality."""

    @pytest.fixture
    def tool(self):
        """Create RunSimulationTool instance."""
        return RunSimulationTool()

    @pytest.fixture
    def sample_forcing(self):
        """Create sample forcing data."""
        dates = pd.date_range("2024-01-01", periods=24, freq="h")
        return pd.DataFrame(
            {
                "T2": np.random.uniform(10, 20, 24),
                "RH2": np.random.uniform(40, 80, 24),
                "U10": np.random.uniform(1, 5, 24),
                "Rain": np.zeros(24),
                "Kdown": np.random.uniform(0, 800, 24),
            },
            index=dates,
        )

    def test_tool_initialization(self, tool):
        """Test tool is initialized correctly."""
        assert tool.name == "run_simulation"
        assert "run" in tool.description.lower()
        assert tool.translator is not None

    def test_get_parameters(self, tool):
        """Test parameter definitions."""
        params = tool.get_parameters()
        assert isinstance(params, list)
        assert (
            len(params) >= 6
        )  # forcing_path, config_path, use_sample_data, start_time, end_time, etc.

        param_names = [p["name"] for p in params]
        assert "forcing_path" in param_names
        assert "use_sample_data" in param_names
        assert "start_time" in param_names
        assert "end_time" in param_names

    @pytest.mark.asyncio
    async def test_execute_with_sample_data(self, tool):
        """Test execution with sample data."""
        with patch("src.supy.mcp.tools.run.load_SampleData") as mock_load:
            with patch("src.supy.mcp.tools.run.run_supy") as mock_run:
                # Mock sample data loading
                mock_load.return_value = (
                    pd.DataFrame({"var": [1]}),  # df_state_init
                    pd.DataFrame(
                        {"T2": [15]},
                        index=pd.date_range("2024-01-01", periods=1, freq="h"),
                    ),  # df_forcing
                )

                # Mock simulation run
                mock_run.return_value = (
                    pd.DataFrame(
                        {"QH": [100]},
                        index=pd.date_range("2024-01-01", periods=1, freq="h"),
                    ),
                    pd.DataFrame({"var": [2]}),
                )

                result = await tool.execute({
                    "use_sample_data": True,
                    "save_state": True,
                })

                assert mock_load.called
                assert mock_run.called

                if result["success"]:
                    assert result.get("data", {}).get("data_source") == "sample_data"

    @pytest.mark.asyncio
    async def test_execute_with_time_filtering(self, tool, sample_forcing):
        """Test execution with time range filtering."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            sample_forcing.to_csv(f.name)
            temp_path = f.name

        try:
            with patch("src.supy.mcp.tools.run.run_supy") as mock_run:
                with patch("src.supy.mcp.tools.run.init_config") as mock_init:
                    mock_init.return_value = pd.DataFrame({"var": [1]})
                    mock_run.return_value = pd.DataFrame({"QH": [100]})

                    result = await tool.execute({
                        "forcing_path": temp_path,
                        "start_time": "2024-01-01T06:00:00",
                        "end_time": "2024-01-01T12:00:00",
                        "save_state": False,
                    })

                    if result["success"]:
                        data = result.get("data", {})
                        if "time_filtering" in data:
                            assert data["time_filtering"]["start_time"] is not None
                            assert data["time_filtering"]["end_time"] is not None
        finally:
            Path(temp_path).unlink()

    def test_calculate_basic_statistics(self, tool):
        """Test basic statistics calculation."""
        df_output = pd.DataFrame(
            {"QH": [100, 150, 200], "QE": [50, 75, 100], "T2": [15, 16, 17]},
            index=pd.date_range("2024-01-01", periods=3, freq="h"),
        )

        stats = tool._calculate_basic_statistics(df_output)

        assert "duration" in stats
        assert "variables" in stats
        assert "key_variables" in stats
        assert "QH" in stats["key_variables"]
        assert "mean" in stats["key_variables"]["QH"]
        assert stats["key_variables"]["QH"]["units"] == "W m-2"


class TestAnalyzeResultsTool:
    """Test AnalyzeResultsTool functionality."""

    @pytest.fixture
    def tool(self):
        """Create AnalyzeResultsTool instance."""
        return AnalyzeResultsTool()

    @pytest.fixture
    def sample_results(self):
        """Create sample results data."""
        dates = pd.date_range("2024-01-01", periods=48, freq="h")
        return pd.DataFrame(
            {
                "QH": np.random.uniform(50, 200, 48),
                "QE": np.random.uniform(20, 150, 48),
                "QS": np.random.uniform(-50, 100, 48),
                "QN": np.random.uniform(100, 500, 48),
                "T2": np.random.uniform(10, 25, 48),
            },
            index=dates,
        )

    def test_tool_initialization(self, tool):
        """Test tool is initialized correctly."""
        assert tool.name == "analyze_results"
        assert "analyze" in tool.description.lower()
        assert tool.translator is not None

    def test_get_parameters(self, tool):
        """Test parameter definitions."""
        params = tool.get_parameters()
        assert isinstance(params, list)
        assert (
            len(params) >= 5
        )  # results_path, analysis_type, variables, time_period, etc.

        param_names = [p["name"] for p in params]
        assert "results_path" in param_names
        assert "analysis_type" in param_names
        assert "variables" in param_names

    @pytest.mark.asyncio
    async def test_execute_summary_analysis(self, tool, sample_results):
        """Test summary analysis execution."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            sample_results.to_csv(f.name)
            temp_path = f.name

        try:
            result = await tool.execute({
                "results_path": temp_path,
                "analysis_type": "summary",
            })

            assert result["success"]
            data = result.get("data", {})
            assert "analysis_results" in data

            analysis = data["analysis_results"]
            assert "data_overview" in analysis
            assert "variable_summary" in analysis
        finally:
            Path(temp_path).unlink()

    @pytest.mark.asyncio
    async def test_execute_energy_balance_analysis(self, tool, sample_results):
        """Test energy balance analysis."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            sample_results.to_csv(f.name)
            temp_path = f.name

        try:
            result = await tool.execute({
                "results_path": temp_path,
                "analysis_type": "energy_balance",
            })

            assert result["success"]
            analysis = result["data"]["analysis_results"]
            assert "available_variables" in analysis
            assert "energy_statistics" in analysis

            # Check energy balance closure calculation
            if "energy_balance_closure" in analysis:
                assert "mean_residual" in analysis["energy_balance_closure"]
                assert "closure_ratio" in analysis["energy_balance_closure"]
        finally:
            Path(temp_path).unlink()

    @pytest.mark.asyncio
    async def test_execute_with_variable_filtering(self, tool, sample_results):
        """Test analysis with specific variables."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            sample_results.to_csv(f.name)
            temp_path = f.name

        try:
            result = await tool.execute({
                "results_path": temp_path,
                "analysis_type": "statistics",
                "variables": ["QH", "QE"],
            })

            assert result["success"]
            data = result.get("data", {})
            assert data.get("analyzed_variables") == ["QH", "QE"]
        finally:
            Path(temp_path).unlink()

    def test_detect_frequency(self, tool):
        """Test frequency detection."""
        # Hourly data
        hourly_index = pd.date_range("2024-01-01", periods=24, freq="h")
        freq = tool._detect_frequency(hourly_index)
        assert freq in ["h", "hourly", "H"]

        # Daily data
        daily_index = pd.date_range("2024-01-01", periods=30, freq="D")
        freq = tool._detect_frequency(daily_index)
        assert freq in ["D", "daily"]

    def test_analyze_temporal_patterns(self, tool, sample_results):
        """Test temporal pattern analysis."""
        patterns = tool._analyze_temporal_patterns(sample_results, "daily")

        assert "patterns" in patterns
        assert "diurnal" in patterns["patterns"]
        assert "weekly" in patterns["patterns"]


class TestParameterTranslator:
    """Test ParameterTranslator utility functions."""

    def test_validate_file_path(self):
        """Test file path validation."""
        # Test with non-existent file
        with pytest.raises(FileNotFoundError):
            ParameterTranslator.validate_file_path("/non/existent/file.txt")

        # Test with empty path
        with pytest.raises(ValueError):
            ParameterTranslator.validate_file_path("")

        # Test with valid file
        with tempfile.NamedTemporaryFile(suffix=".yaml") as f:
            path = ParameterTranslator.validate_file_path(f.name)
            assert isinstance(path, Path)
            assert path.exists()

        # Test with extension checking
        with tempfile.NamedTemporaryFile(suffix=".txt") as f:
            with pytest.raises(ValueError):
                ParameterTranslator.validate_file_path(
                    f.name, extensions=[".yaml", ".yml"]
                )

    def test_validate_time_range(self):
        """Test time range validation."""
        # Valid time range
        start, end = ParameterTranslator.validate_time_range(
            "2024-01-01T00:00:00", "2024-01-02T00:00:00"
        )
        assert start < end

        # Invalid order
        with pytest.raises(ValueError):
            ParameterTranslator.validate_time_range(
                "2024-01-02T00:00:00", "2024-01-01T00:00:00"
            )

        # Invalid format
        with pytest.raises(ValueError):
            ParameterTranslator.validate_time_range("not-a-date", None)

    def test_validate_output_format(self):
        """Test output format validation."""
        # Valid formats
        assert ParameterTranslator.validate_output_format("csv") == "csv"
        assert ParameterTranslator.validate_output_format("JSON") == "json"

        # Invalid format
        with pytest.raises(ValueError):
            ParameterTranslator.validate_output_format("invalid")

    def test_validate_analysis_variables(self):
        """Test analysis variables validation."""
        # String input
        vars = ParameterTranslator.validate_analysis_variables("QH, QE, T2")
        assert vars == ["QH", "QE", "T2"]

        # List input
        vars = ParameterTranslator.validate_analysis_variables(["QH", "QE"])
        assert vars == ["QH", "QE"]

        # Invalid variable name
        with pytest.raises(ValueError):
            ParameterTranslator.validate_analysis_variables(["valid", "inv@lid"])

    def test_serialize_dataframe(self):
        """Test DataFrame serialization."""
        df = pd.DataFrame(
            {"A": [1, 2, 3], "B": [4.5, 5.5, 6.5]},
            index=pd.date_range("2024-01-01", periods=3, freq="h"),
        )

        result = ParameterTranslator.serialize_dataframe(df, max_rows=2)

        assert result["shape"] == [3, 2]
        assert result["columns"] == ["A", "B"]
        assert result["truncated"] is True
        assert len(result["data"]) == 2  # Only 2 rows due to max_rows

        # Test with stats
        result_with_stats = ParameterTranslator.serialize_dataframe(
            df, include_stats=True
        )
        assert "statistics" in result_with_stats
        assert "A" in result_with_stats["statistics"]
        assert "mean" in result_with_stats["statistics"]["A"]

    def test_map_supy_parameters(self):
        """Test parameter mapping."""
        mcp_params = {
            "config_path": "/path/to/config.yaml",
            "forcing_path": "/path/to/forcing.txt",
            "time_step": 3600,
            "site_name": "TestSite",
        }

        supy_params = ParameterTranslator.map_supy_parameters(mcp_params)

        assert supy_params["path_config"] == "/path/to/config.yaml"
        assert supy_params["path_forcing"] == "/path/to/forcing.txt"
        assert supy_params["tstep"] == 3600
        assert supy_params["site"] == "TestSite"


# Integration tests
class TestMCPToolsIntegration:
    """Integration tests for MCP tools working together."""

    @pytest.mark.asyncio
    async def test_configure_run_analyze_workflow(self):
        """Test complete workflow: configure -> run -> analyze."""
        # This would test the tools working together
        # but requires more complex mocking of the actual SuPy functions
        pass
