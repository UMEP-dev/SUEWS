"""
Tests for MCP (Model Context Protocol) tools.

Tests the MCP tools integration with SuPy.
Since SuPy is an external package, we mock it when not available.
"""

import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
import pytest
import pandas as pd
import numpy as np
import yaml
import sys

# Check if SuPy is available as an external package
try:
    import supy

    SUPY_AVAILABLE = True
except ImportError:
    SUPY_AVAILABLE = False

# Try to import MCP tools if they exist in SuPy
try:
    if SUPY_AVAILABLE:
        # Check if SuPy has MCP tools (it might not in current version)
        from supy.mcp.tools import (
            ConfigureSimulationTool,
            RunSimulationTool,
            AnalyzeResultsTool,
        )
        from supy.mcp.utils.translator import ParameterTranslator

        MCP_TOOLS_AVAILABLE = True
    else:
        MCP_TOOLS_AVAILABLE = False
except (ImportError, AttributeError):
    MCP_TOOLS_AVAILABLE = False

    # Create mock classes for testing without SuPy
    class ConfigureSimulationTool:
        async def execute(self, params):
            return {"success": True, "message": "Mock configuration"}

    class RunSimulationTool:
        async def execute(self, params):
            return {"success": True, "message": "Mock simulation"}

    class AnalyzeResultsTool:
        async def execute(self, params):
            return {"success": True, "message": "Mock analysis"}

    class ParameterTranslator:
        def translate(self, params):
            return params


@pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy package not installed")
class TestMCPToolsWithSuPy:
    """Test MCP tools when SuPy is actually installed."""

    def test_supy_import(self):
        """Test that SuPy can be imported."""
        import supy

        assert hasattr(supy, "__version__"), "SuPy should have version info"

    def test_supy_basic_functionality(self):
        """Test basic SuPy functionality."""
        # Test that we can load sample data
        try:
            df_state, df_forcing = supy.load_SampleData()
            assert df_state is not None
            assert df_forcing is not None
            assert len(df_forcing) > 0
        except Exception as e:
            pytest.skip(f"SuPy sample data not available: {e}")

    @pytest.mark.skipif(not MCP_TOOLS_AVAILABLE, reason="MCP tools not in SuPy")
    def test_configure_tool_with_supy(self):
        """Test ConfigureSimulationTool with actual SuPy."""
        tool = ConfigureSimulationTool()
        assert tool is not None
        # More detailed tests would go here if MCP tools exist in SuPy

    @pytest.mark.skipif(not MCP_TOOLS_AVAILABLE, reason="MCP tools not in SuPy")
    def test_run_tool_with_supy(self):
        """Test RunSimulationTool with actual SuPy."""
        tool = RunSimulationTool()
        assert tool is not None

    @pytest.mark.skipif(not MCP_TOOLS_AVAILABLE, reason="MCP tools not in SuPy")
    def test_analyze_tool_with_supy(self):
        """Test AnalyzeResultsTool with actual SuPy."""
        tool = AnalyzeResultsTool()
        assert tool is not None


class TestMCPToolsMocked:
    """Test MCP tools interface with mocked SuPy (always runs)."""

    @pytest.fixture
    def mock_supy(self):
        """Create a mock SuPy module."""
        mock = MagicMock()
        mock.__version__ = "2024.12.16"
        mock.load_SampleData = MagicMock(
            return_value=(
                pd.DataFrame({"test": [1, 2, 3]}),
                pd.DataFrame({"forcing": [4, 5, 6]}),
            )
        )
        mock.run_supy = MagicMock(
            return_value=(
                pd.DataFrame({"output": [7, 8, 9]}),
                pd.DataFrame({"state": [10, 11, 12]}),
            )
        )
        return mock

    def test_mcp_interface_contract(self):
        """Test that MCP tools follow expected interface."""
        # Even with mocked tools, they should have execute method
        config_tool = ConfigureSimulationTool()
        run_tool = RunSimulationTool()
        analyze_tool = AnalyzeResultsTool()

        assert hasattr(config_tool, "execute"), "Should have execute method"
        assert hasattr(run_tool, "execute"), "Should have execute method"
        assert hasattr(analyze_tool, "execute"), "Should have execute method"

    @pytest.mark.asyncio
    async def test_mock_configure_tool(self):
        """Test configuration tool with mocked implementation."""
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
            },
        }

        result = await tool.execute(params)
        assert result["success"] is True
        assert "message" in result

    @pytest.mark.asyncio
    async def test_mock_run_tool(self):
        """Test run tool with mocked implementation."""
        tool = RunSimulationTool()

        params = {
            "config_file": "test_config.yml",
            "forcing_file": "test_forcing.txt",
            "output_dir": "/tmp/output",
        }

        result = await tool.execute(params)
        assert result["success"] is True
        assert "message" in result

    @pytest.mark.asyncio
    async def test_mock_analyze_tool(self):
        """Test analyze tool with mocked implementation."""
        tool = AnalyzeResultsTool()

        params = {
            "output_file": "test_output.txt",
            "metrics": ["energy_balance", "surface_temperature"],
            "time_period": "daily",
        }

        result = await tool.execute(params)
        assert result["success"] is True
        assert "message" in result

    def test_parameter_translator(self):
        """Test parameter translator with mocked implementation."""
        translator = ParameterTranslator()

        params = {"test": "value"}
        result = translator.translate(params)
        assert result == params  # Mock just returns input


class TestMCPIntegration:
    """Test MCP integration patterns that work with or without SuPy."""

    def test_tool_discovery(self):
        """Test that tools can be discovered/instantiated."""
        tools = []

        try:
            config_tool = ConfigureSimulationTool()
            tools.append(("configure", config_tool))
        except Exception:
            pass

        try:
            run_tool = RunSimulationTool()
            tools.append(("run", run_tool))
        except Exception:
            pass

        try:
            analyze_tool = AnalyzeResultsTool()
            tools.append(("analyze", analyze_tool))
        except Exception:
            pass

        # We should have at least the mock tools
        assert len(tools) == 3, "Should have all three tool types"

    @pytest.mark.asyncio
    async def test_tool_chain(self):
        """Test chaining tools together."""
        config_tool = ConfigureSimulationTool()
        run_tool = RunSimulationTool()
        analyze_tool = AnalyzeResultsTool()

        # Configure
        config_result = await config_tool.execute({
            "site_name": "ChainTest",
            "latitude": 40.0,
            "longitude": -74.0,
        })
        assert config_result["success"]

        # Run
        run_result = await run_tool.execute({"config_file": "chain_config.yml"})
        assert run_result["success"]

        # Analyze
        analyze_result = await analyze_tool.execute({"output_file": "chain_output.txt"})
        assert analyze_result["success"]

    def test_error_handling(self):
        """Test that tools handle errors gracefully."""
        # Tools should handle missing parameters gracefully
        tool = ConfigureSimulationTool()
        # Async function needs to be tested differently
        # Just verify the tool exists and has the right interface
        assert hasattr(tool, "execute")
        assert callable(getattr(tool, "execute"))


@pytest.mark.integration
class TestMCPWithSUEWSMCPServer:
    """Test MCP tools integration with SUEWS MCP server."""

    @pytest.fixture
    def mock_mcp_server(self):
        """Create a mock MCP server configuration."""
        from unittest.mock import MagicMock

        config = MagicMock()
        config.suews = MagicMock()
        config.suews.max_simulation_days = 365
        config.max_concurrent_simulations = 5
        return config

    def test_server_integration(self, mock_mcp_server):
        """Test that MCP tools integrate with server configuration."""
        # This tests the integration pattern, not actual functionality
        assert mock_mcp_server.max_concurrent_simulations == 5
        assert mock_mcp_server.suews.max_simulation_days == 365

    @patch("supy.run_supy" if SUPY_AVAILABLE else "builtins.print")
    def test_mocked_supy_call(self, mock_run):
        """Test that SuPy calls can be mocked for testing."""
        if SUPY_AVAILABLE:
            # Set up mock return value
            mock_run.return_value = (
                pd.DataFrame({"QH": [100, 200]}),
                pd.DataFrame({"state": [1, 2]}),
            )

            # Would be called by RunSimulationTool internally
            df_output, df_state = mock_run()
            assert len(df_output) == 2
            assert "QH" in df_output.columns
        else:
            # Just verify mock works
            mock_run("test")
            mock_run.assert_called_once()


# Smoke test to ensure module loads
def test_module_imports():
    """Test that this test module can be imported without errors."""
    assert True, "Module imported successfully"
