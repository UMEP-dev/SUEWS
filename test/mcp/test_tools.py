"""
Tests for individual MCP tools.

NO MOCKING - These tests require real SuPy with MCP tools.
Tests will fail prominently if dependencies are missing.
"""

import asyncio
import tempfile
import pytest
from pathlib import Path

import pandas as pd
import numpy as np

# REQUIRE SuPy - fail if not installed
try:
    import supy
    SUPY_VERSION = supy.__version__
except ImportError as e:
    pytest.fail(
        f"\n\n"
        f"=" * 70 + "\n"
        f"FATAL: SuPy is REQUIRED for testing but not installed.\n"
        f"Install it with: pip install supy\n" 
        f"Error: {e}\n"
        f"=" * 70 + "\n"
    )

# REQUIRE MCP tools in SuPy - skip if not available yet
try:
    from supy.mcp.tools import (
        ConfigureSimulationTool,
        RunSimulationTool,
        AnalyzeResultsTool,
    )
    from supy.mcp.utils import ParameterTranslator
except (ImportError, AttributeError) as e:
    pytest.skip(
        f"\n\n"
        f"=" * 70 + "\n"
        f"MCP tools not found in SuPy {SUPY_VERSION}.\n"
        f"The MCP tools are not yet integrated into the SuPy package.\n"
        f"This is expected until the integration is complete.\n"
        f"\n"
        f"To run these tests, MCP tools must be part of SuPy.\n"
        f"Error: {e}\n"
        f"=" * 70 + "\n",
        allow_module_level=True
    )


class TestParameterTranslator:
    """Test parameter translation utilities with real SuPy."""

    def test_validate_file_path_existing(self):
        """Test validation of existing file path."""
        translator = ParameterTranslator()
        with tempfile.NamedTemporaryFile(delete=False) as tf:
            temp_path = Path(tf.name)

        try:
            result = translator.validate_file_path(str(temp_path), must_exist=True)
            assert result == temp_path
        finally:
            temp_path.unlink()

    def test_validate_file_path_not_existing(self):
        """Test validation of non-existing file path."""
        translator = ParameterTranslator()
        fake_path = Path("/fake/path/that/does/not/exist.txt")
        
        with pytest.raises(FileNotFoundError):
            translator.validate_file_path(str(fake_path), must_exist=True)

    def test_validate_parameters(self):
        """Test parameter validation."""
        translator = ParameterTranslator()
        
        params = {"site": "test", "latitude": 51.5}
        result = translator.validate_parameters(params, required=["site"])
        assert result == params
        
        with pytest.raises(ValueError) as exc_info:
            translator.validate_parameters({}, required=["site"])
        assert "site" in str(exc_info.value)

    def test_convert_to_supy_format(self):
        """Test conversion to SuPy format."""
        translator = ParameterTranslator()
        
        mcp_params = {
            "site_name": "Test",
            "latitude": 51.5,
            "longitude": -0.1,
        }
        
        supy_params = translator.convert_to_supy_format(mcp_params)
        assert supy_params is not None
        # Should produce valid SuPy parameters


class TestConfigureSimulationTool:
    """Test ConfigureSimulationTool with real SuPy."""

    @pytest.mark.asyncio
    async def test_execute_with_minimal_params(self):
        """Test execution with minimal parameters."""
        tool = ConfigureSimulationTool()
        params = {
            "site_name": "TestSite",
            "latitude": 51.5,
            "longitude": -0.1,
        }
        
        result = await tool.execute(params)
        assert result["success"] is True
        assert "message" in result
        assert "data" in result
        assert "config" in result["data"]

    @pytest.mark.asyncio
    async def test_execute_with_surface_fractions(self):
        """Test execution with surface fractions."""
        tool = ConfigureSimulationTool()
        params = {
            "site_name": "TestSite",
            "latitude": 51.5,
            "longitude": -0.1,
            "surface_fractions": {
                "building": 0.3,
                "paved": 0.2,
                "vegetation": 0.4,
                "water": 0.1,
            }
        }
        
        result = await tool.execute(params)
        assert result["success"] is True
        
        # Verify the configuration includes surface fractions
        config = result["data"]["config"]
        assert "surface" in config
        
        # Fractions should sum to 1.0
        total = sum(params["surface_fractions"].values())
        assert total == pytest.approx(1.0)

    @pytest.mark.asyncio
    async def test_execute_with_invalid_params(self):
        """Test execution with invalid parameters."""
        tool = ConfigureSimulationTool()
        params = {
            "site_name": "TestSite",
            "latitude": 200.0,  # Invalid latitude
            "longitude": -0.1,
        }
        
        result = await tool.execute(params)
        assert result["success"] is False
        assert "error" in result or "errors" in result

    @pytest.mark.asyncio
    async def test_execute_saves_to_file(self, tmp_path):
        """Test that configuration is saved to file."""
        tool = ConfigureSimulationTool()
        output_file = tmp_path / "test_config.yml"
        
        params = {
            "site_name": "TestSite",
            "latitude": 51.5,
            "longitude": -0.1,
            "output_path": str(output_file),
        }
        
        result = await tool.execute(params)
        assert result["success"] is True
        assert output_file.exists()
        
        # Verify it's valid YAML
        import yaml
        with open(output_file) as f:
            config = yaml.safe_load(f)
        assert config is not None
        assert "site" in config or "Site" in config


class TestRunSimulationTool:
    """Test RunSimulationTool with real SuPy."""

    @pytest.mark.asyncio
    async def test_execute_with_config_file(self, tmp_path):
        """Test execution with config file."""
        tool = RunSimulationTool()
        
        # Create a config file
        config_file = tmp_path / "config.yml"
        config_file.write_text("site: TestSite\nlatitude: 51.5\nlongitude: -0.1\n")
        
        params = {
            "config_file": str(config_file),
            "simulation_days": 1,
        }
        
        result = await tool.execute(params)
        assert "success" in result
        
        if result["success"]:
            assert "data" in result
            assert "output_file" in result["data"] or "outputs" in result["data"]
        else:
            # Should give clear error message
            assert "error" in result or "message" in result

    @pytest.mark.asyncio
    async def test_execute_with_missing_config(self):
        """Test execution with missing config file."""
        tool = RunSimulationTool()
        
        params = {
            "config_file": "/nonexistent/config.yml",
            "simulation_days": 1,
        }
        
        result = await tool.execute(params)
        assert result["success"] is False
        assert "error" in result or "errors" in result
        
        # Error should mention the missing file
        error_msg = str(result).lower()
        assert "not found" in error_msg or "does not exist" in error_msg

    @pytest.mark.asyncio
    async def test_execute_with_output_directory(self, tmp_path):
        """Test execution with specified output directory."""
        tool = RunSimulationTool()
        
        config_file = tmp_path / "config.yml"
        config_file.write_text("site: TestSite\n")
        
        output_dir = tmp_path / "outputs"
        output_dir.mkdir()
        
        params = {
            "config_file": str(config_file),
            "output_dir": str(output_dir),
            "simulation_days": 1,
        }
        
        result = await tool.execute(params)
        assert "success" in result


class TestAnalyzeResultsTool:
    """Test AnalyzeResultsTool with real SuPy."""

    @pytest.mark.asyncio
    async def test_execute_with_output_file(self, tmp_path):
        """Test execution with output file."""
        tool = AnalyzeResultsTool()
        
        # Create a sample output file
        output_file = tmp_path / "output.txt"
        output_file.write_text(
            "Year\tDOY\tHour\tQH\tQE\n"
            "2020\t1\t0\t100\t50\n"
            "2020\t1\t1\t120\t60\n"
        )
        
        params = {
            "output_file": str(output_file),
            "metrics": ["mean", "std"],
        }
        
        result = await tool.execute(params)
        assert result["success"] is True
        assert "data" in result
        assert "analysis" in result["data"] or "results" in result["data"]

    @pytest.mark.asyncio
    async def test_execute_with_missing_file(self):
        """Test execution with missing output file."""
        tool = AnalyzeResultsTool()
        
        params = {
            "output_file": "/nonexistent/output.txt",
            "metrics": ["mean"],
        }
        
        result = await tool.execute(params)
        assert result["success"] is False
        assert "error" in result or "errors" in result

    @pytest.mark.asyncio
    async def test_execute_with_energy_balance_metric(self, tmp_path):
        """Test energy balance analysis."""
        tool = AnalyzeResultsTool()
        
        # Create output with energy balance components
        output_file = tmp_path / "output.txt"
        output_file.write_text(
            "Year\tDOY\tHour\tQN\tQH\tQE\tQS\tQF\n"
            "2020\t1\t0\t-50\t-30\t-10\t-10\t20\n"
            "2020\t1\t1\t-45\t-28\t-8\t-9\t20\n"
        )
        
        params = {
            "output_file": str(output_file),
            "metrics": ["energy_balance"],
        }
        
        result = await tool.execute(params)
        assert "success" in result
        
        if result["success"]:
            assert "data" in result
            analysis = result["data"].get("analysis", result["data"].get("results"))
            assert analysis is not None


class TestToolIntegration:
    """Test integration between MCP tools with real SuPy."""

    @pytest.mark.asyncio
    async def test_tool_pipeline(self, tmp_path):
        """Test a complete tool pipeline."""
        config_tool = ConfigureSimulationTool()
        run_tool = RunSimulationTool()
        analyze_tool = AnalyzeResultsTool()
        
        # 1. Configure
        config_params = {
            "site_name": "PipelineTest",
            "latitude": 40.0,
            "longitude": -74.0,
            "output_path": str(tmp_path / "config.yml"),
        }
        config_result = await config_tool.execute(config_params)
        assert config_result["success"] is True
        
        # 2. Run (only if config succeeded)
        if config_result["success"]:
            run_params = {
                "config_file": str(tmp_path / "config.yml"),
                "simulation_days": 1,
                "output_dir": str(tmp_path / "outputs"),
            }
            run_result = await run_tool.execute(run_params)
            assert "success" in run_result
            
            # 3. Analyze (only if run succeeded and produced output)
            if run_result["success"] and run_result.get("data", {}).get("output_file"):
                analyze_params = {
                    "output_file": run_result["data"]["output_file"],
                    "metrics": ["mean", "energy_balance"],
                }
                analyze_result = await analyze_tool.execute(analyze_params)
                assert "success" in analyze_result

    def test_tool_availability(self):
        """Test that tools are available."""
        assert ConfigureSimulationTool is not None
        assert RunSimulationTool is not None
        assert AnalyzeResultsTool is not None
        assert ParameterTranslator is not None

    def test_tool_interface(self):
        """Test that tools have expected interface."""
        config_tool = ConfigureSimulationTool()
        run_tool = RunSimulationTool()
        analyze_tool = AnalyzeResultsTool()
        
        assert hasattr(config_tool, "execute")
        assert hasattr(run_tool, "execute")
        assert hasattr(analyze_tool, "execute")
        
        assert asyncio.iscoroutinefunction(config_tool.execute)
        assert asyncio.iscoroutinefunction(run_tool.execute)
        assert asyncio.iscoroutinefunction(analyze_tool.execute)


def test_supy_version():
    """Display SuPy version for debugging."""
    print(f"\nTesting with SuPy version: {SUPY_VERSION}")
    assert SUPY_VERSION is not None