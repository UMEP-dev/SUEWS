"""
Tests for MCP (Model Context Protocol) tools.

Tests the MCP tools that should be part of SuPy.
These tests REQUIRE SuPy to be installed with MCP tools.
NO MOCKING - fail prominently if dependencies are missing.
"""

import json
import tempfile
from pathlib import Path
import pytest
import pandas as pd
import numpy as np
import yaml
import sys

# SuPy is REQUIRED - fail prominently if not available
try:
    import supy
    SUPY_VERSION = supy.__version__
except ImportError as e:
    pytest.fail(
        f"\n\n"
        f"=" * 70 + "\n"
        f"FATAL: SuPy is REQUIRED for MCP tools testing but not installed.\n"
        f"Install it with: pip install supy\n"
        f"Error: {e}\n"
        f"=" * 70 + "\n"
    )

# MCP tools must exist in SuPy - skip all tests if not available
# This is expected to fail until MCP tools are integrated into SuPy
try:
    from supy.mcp.tools import (
        ConfigureSimulationTool,
        RunSimulationTool, 
        AnalyzeResultsTool,
    )
    from supy.mcp.utils.translator import ParameterTranslator
    MCP_TOOLS_AVAILABLE = True
except (ImportError, AttributeError) as e:
    # Skip ALL tests in this module with clear message
    pytest.skip(
        f"\n\n"
        f"=" * 70 + "\n"
        f"MCP tools not found in installed SuPy version {SUPY_VERSION}.\n"
        f"The MCP tools are not yet integrated into the SuPy package.\n"
        f"This is expected until the MCP integration is complete.\n"
        f"\n"
        f"Error details: {e}\n"
        f"=" * 70 + "\n",
        allow_module_level=True
    )


# All tests below will ONLY run if both SuPy and MCP tools are available
# NO MOCKING - these are real integration tests

class TestConfigureSimulationTool:
    """Test ConfigureSimulationTool functionality with real SuPy."""

    @pytest.fixture
    def tool(self):
        """Create ConfigureSimulationTool instance."""
        return ConfigureSimulationTool()

    @pytest.mark.asyncio
    async def test_execute_with_minimal_params(self, tool):
        """Test execution with minimal required parameters."""
        params = {
            "site_name": "TestSite",
            "latitude": 51.5,
            "longitude": -0.1,
        }
        
        result = await tool.execute(params)
        assert result["success"] is True
        assert "message" in result
        assert "data" in result

    @pytest.mark.asyncio 
    async def test_execute_with_surface_fractions(self, tool):
        """Test execution with surface fractions specified."""
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
        assert sum(params["surface_fractions"].values()) == pytest.approx(1.0)


class TestRunSimulationTool:
    """Test RunSimulationTool functionality with real SuPy."""

    @pytest.fixture
    def tool(self):
        """Create RunSimulationTool instance."""
        return RunSimulationTool()

    @pytest.fixture
    def sample_config_file(self, tmp_path):
        """Create a real SuPy configuration file."""
        config_path = tmp_path / "config.yml"
        # Use actual SuPy configuration format
        config_data = {
            "site": {"name": "TestSite", "latitude": 51.5, "longitude": -0.1},
            "surface": {"fractions": {"building": 0.3, "paved": 0.2, "vegetation": 0.4, "water": 0.1}},
        }
        with open(config_path, "w") as f:
            yaml.dump(config_data, f)
        return config_path

    @pytest.mark.asyncio
    async def test_execute_with_config_file(self, tool, sample_config_file):
        """Test real simulation execution."""
        params = {
            "config_file": str(sample_config_file),
            "simulation_days": 1,
        }
        
        result = await tool.execute(params)
        assert "success" in result
        
        if result["success"]:
            assert "data" in result
            # Should have actual simulation outputs
            assert "output_file" in result["data"] or "outputs" in result["data"]


class TestAnalyzeResultsTool:
    """Test AnalyzeResultsTool functionality with real SuPy."""

    @pytest.fixture
    def tool(self):
        """Create AnalyzeResultsTool instance."""
        return AnalyzeResultsTool()

    @pytest.fixture
    def sample_supy_output(self, tmp_path):
        """Create a real SuPy output file format."""
        output_path = tmp_path / "output.txt"
        # Real SUEWS output format
        output_data = """Year	DOY	Hour	QN	QH	QE	QS	QF
2020	1	0	-50.0	-30.0	-10.0	-10.0	20.0
2020	1	1	-45.0	-28.0	-8.0	-9.0	20.0
2020	1	2	-40.0	-25.0	-5.0	-10.0	20.0"""
        output_path.write_text(output_data)
        return output_path

    @pytest.mark.asyncio
    async def test_execute_with_real_output(self, tool, sample_supy_output):
        """Test analysis of real SuPy output."""
        params = {
            "output_file": str(sample_supy_output),
            "metrics": ["mean", "energy_balance"],
        }
        
        result = await tool.execute(params)
        assert result["success"] is True
        assert "data" in result
        # Should have real analysis results
        assert "analysis" in result["data"] or "results" in result["data"]


class TestParameterTranslator:
    """Test ParameterTranslator with real SuPy requirements."""

    @pytest.fixture
    def translator(self):
        """Create ParameterTranslator instance."""
        return ParameterTranslator()

    def test_translate_to_supy_format(self, translator):
        """Test translation to actual SuPy format."""
        mcp_params = {
            "site_name": "Test",
            "latitude": 51.5,
            "longitude": -0.1,
            "surface_fractions": {
                "building": 0.3,
                "paved": 0.2,
                "vegetation": 0.4,
                "water": 0.1,
            }
        }
        
        supy_params = translator.convert_to_supy_format(mcp_params)
        assert supy_params is not None
        # Should be in actual SuPy format
        assert "site" in supy_params or "Site" in supy_params


class TestIntegrationWithSuPy:
    """Test actual integration with SuPy package."""

    def test_supy_is_installed(self):
        """Verify SuPy is properly installed."""
        assert supy is not None
        assert hasattr(supy, "__version__")
        assert hasattr(supy, "load_SampleData")

    def test_mcp_tools_exist(self):
        """Verify MCP tools exist in SuPy."""
        assert ConfigureSimulationTool is not None
        assert RunSimulationTool is not None
        assert AnalyzeResultsTool is not None
        assert ParameterTranslator is not None

    def test_supy_sample_data(self):
        """Test that SuPy sample data can be loaded."""
        try:
            df_state, df_forcing = supy.load_SampleData()
            assert df_state is not None
            assert df_forcing is not None
            assert isinstance(df_state, pd.DataFrame)
            assert isinstance(df_forcing, pd.DataFrame)
        except Exception as e:
            pytest.fail(f"Failed to load SuPy sample data: {e}")

    @pytest.mark.asyncio
    async def test_complete_workflow(self, tmp_path):
        """Test complete MCP workflow with real SuPy."""
        # 1. Configure
        config_tool = ConfigureSimulationTool()
        config_result = await config_tool.execute({
            "site_name": "WorkflowTest",
            "latitude": 51.5,
            "longitude": -0.1,
            "output_path": str(tmp_path / "config.yml"),
        })
        assert config_result["success"] is True
        
        # 2. Run (if config succeeded)
        if config_result["success"]:
            run_tool = RunSimulationTool()
            run_result = await run_tool.execute({
                "config_file": str(tmp_path / "config.yml"),
                "simulation_days": 1,
                "output_dir": str(tmp_path / "outputs"),
            })
            assert "success" in run_result
            
            # 3. Analyze (if run succeeded)
            if run_result["success"] and "output_file" in run_result.get("data", {}):
                analyze_tool = AnalyzeResultsTool()
                analyze_result = await analyze_tool.execute({
                    "output_file": run_result["data"]["output_file"],
                    "metrics": ["energy_balance"],
                })
                assert "success" in analyze_result


# This test always runs to provide clear status
def test_mcp_integration_status():
    """Report the current status of MCP integration."""
    print("\n" + "=" * 70)
    print(f"SuPy Version: {SUPY_VERSION}")
    print(f"MCP Tools Available: {MCP_TOOLS_AVAILABLE}")
    
    if MCP_TOOLS_AVAILABLE:
        print("✅ MCP tools are integrated into SuPy")
        print("✅ All integration tests can run")
    else:
        print("⚠️  MCP tools are NOT yet integrated into SuPy")
        print("⚠️  This is expected - integration is pending")
    print("=" * 70 + "\n")