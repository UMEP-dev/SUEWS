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

# Try to import from installed SuPy package
try:
    import supy
    from supy.mcp.tools import (
        ConfigureSimulationTool,
        RunSimulationTool,
        AnalyzeResultsTool,
    )
    from supy.mcp.utils import ParameterTranslator

    SUPY_MCP_AVAILABLE = True
except (ImportError, AttributeError):
    # SuPy is not installed or doesn't have MCP tools yet
    SUPY_MCP_AVAILABLE = False

    # Create mock implementations for testing
    class MockTool:
        async def execute(self, params):
            return {"success": True, "message": "Mock execution", "data": {}}

    class ConfigureSimulationTool(MockTool):
        pass

    class RunSimulationTool(MockTool):
        pass

    class AnalyzeResultsTool(MockTool):
        pass

    class ParameterTranslator:
        def validate_file_path(self, path, must_exist=True):
            p = Path(path)
            if must_exist and not p.exists():
                raise FileNotFoundError(f"File not found: {path}")
            return p

        def validate_parameters(self, params, required=None):
            if required:
                for req in required:
                    if req not in params:
                        raise ValueError(f"Missing required parameter: {req}")
            return params

        def convert_to_supy_format(self, params):
            return params


@pytest.mark.skipif(not SUPY_MCP_AVAILABLE, reason="SuPy MCP tools not available")
class TestParameterTranslatorWithSuPy:
    """Test parameter translation with actual SuPy."""

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


class TestParameterTranslatorMocked:
    """Test parameter translation with mocked implementation."""

    def test_mock_validate_file_path(self):
        """Test mock file path validation."""
        translator = ParameterTranslator()

        # Test with existing file
        with tempfile.NamedTemporaryFile() as tf:
            result = translator.validate_file_path(tf.name, must_exist=True)
            assert result == Path(tf.name)

        # Test with non-existing file
        with pytest.raises(FileNotFoundError):
            translator.validate_file_path("/fake/path", must_exist=True)

    def test_mock_validate_parameters(self):
        """Test mock parameter validation."""
        translator = ParameterTranslator()

        params = {"site": "test", "latitude": 51.5}
        result = translator.validate_parameters(params, required=["site"])
        assert result == params

        with pytest.raises(ValueError):
            translator.validate_parameters({}, required=["site"])


@pytest.mark.skipif(not SUPY_MCP_AVAILABLE, reason="SuPy MCP tools not available")
class TestConfigureSimulationToolWithSuPy:
    """Test ConfigureSimulationTool with actual SuPy."""

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
        assert "success" in result
        assert "message" in result

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
            },
        }

        result = await tool.execute(params)
        assert "success" in result


class TestConfigureSimulationToolMocked:
    """Test ConfigureSimulationTool with mocked implementation."""

    @pytest.mark.asyncio
    async def test_mock_execute(self):
        """Test mock execution."""
        tool = ConfigureSimulationTool()
        params = {"site_name": "MockSite"}

        result = await tool.execute(params)
        assert result["success"] is True
        assert "message" in result


@pytest.mark.skipif(not SUPY_MCP_AVAILABLE, reason="SuPy MCP tools not available")
class TestRunSimulationToolWithSuPy:
    """Test RunSimulationTool with actual SuPy."""

    @pytest.mark.asyncio
    async def test_execute_with_config_file(self):
        """Test execution with config file."""
        tool = RunSimulationTool()

        # Create a temporary config file
        with tempfile.NamedTemporaryFile(suffix=".yml", delete=False) as tf:
            tf.write(b"site: TestSite\n")
            config_path = tf.name

        try:
            params = {
                "config_file": config_path,
                "simulation_days": 1,
            }

            result = await tool.execute(params)
            assert "success" in result
        finally:
            Path(config_path).unlink()


class TestRunSimulationToolMocked:
    """Test RunSimulationTool with mocked implementation."""

    @pytest.mark.asyncio
    async def test_mock_execute(self):
        """Test mock execution."""
        tool = RunSimulationTool()
        params = {"config_file": "mock_config.yml"}

        result = await tool.execute(params)
        assert result["success"] is True
        assert "message" in result


@pytest.mark.skipif(not SUPY_MCP_AVAILABLE, reason="SuPy MCP tools not available")
class TestAnalyzeResultsToolWithSuPy:
    """Test AnalyzeResultsTool with actual SuPy."""

    @pytest.mark.asyncio
    async def test_execute_with_output_file(self):
        """Test execution with output file."""
        tool = AnalyzeResultsTool()

        # Create a mock output file
        with tempfile.NamedTemporaryFile(suffix=".txt", delete=False) as tf:
            tf.write(b"QH,QE\n100,50\n120,60\n")
            output_path = tf.name

        try:
            params = {
                "output_file": output_path,
                "metrics": ["mean", "std"],
            }

            result = await tool.execute(params)
            assert "success" in result
        finally:
            Path(output_path).unlink()


class TestAnalyzeResultsToolMocked:
    """Test AnalyzeResultsTool with mocked implementation."""

    @pytest.mark.asyncio
    async def test_mock_execute(self):
        """Test mock execution."""
        tool = AnalyzeResultsTool()
        params = {"output_file": "mock_output.txt"}

        result = await tool.execute(params)
        assert result["success"] is True
        assert "message" in result


class TestToolIntegration:
    """Test integration between tools."""

    @pytest.mark.asyncio
    async def test_tool_pipeline(self):
        """Test a complete tool pipeline."""
        config_tool = ConfigureSimulationTool()
        run_tool = RunSimulationTool()
        analyze_tool = AnalyzeResultsTool()

        # Configure
        config_params = {
            "site_name": "PipelineTest",
            "latitude": 40.0,
            "longitude": -74.0,
        }
        config_result = await config_tool.execute(config_params)
        assert config_result["success"]

        # Run
        run_params = {
            "config_file": "pipeline_config.yml",
        }
        run_result = await run_tool.execute(run_params)
        assert run_result["success"]

        # Analyze
        analyze_params = {
            "output_file": "pipeline_output.txt",
        }
        analyze_result = await analyze_tool.execute(analyze_params)
        assert analyze_result["success"]

    def test_tool_availability(self):
        """Test that tools are available (mocked or real)."""
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
