"""Unit tests for individual MCP tools with mock data."""

import pytest
import json
import tempfile
import yaml
from pathlib import Path
from unittest.mock import Mock, AsyncMock, patch, mock_open, MagicMock
from typing import Dict, Any

# Test imports
from suews_mcp.handlers import SUEWSMCPHandlers, SUPY_MCP_TOOLS_AVAILABLE
from suews_mcp.config import MCPServerConfig


class TestHealthCheckTool:
    """Test the health_check tool in isolation."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_health_check_basic_functionality(self, handlers):
        """Test basic health check functionality."""
        result = await handlers._health_check_tool({})

        # Check return structure
        assert "content" in result
        assert isinstance(result["content"], list)
        assert len(result["content"]) == 1

        content = result["content"][0]
        assert content["type"] == "text"
        assert isinstance(content["text"], str)

        # Check content includes expected information
        text = content["text"]
        assert "Health Check" in text
        assert "Status:" in text
        assert "Version:" in text
        assert "Active simulations:" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_health_check_simulation_tracking(self, handlers):
        """Test health check reports simulation states correctly."""
        # Add mock simulations
        handlers._active_simulations = {
            "sim1": {"status": "running", "config_file": "test1.yml"},
            "sim2": {"status": "completed", "config_file": "test2.yml"},
            "sim3": {
                "status": "failed",
                "config_file": "test3.yml",
                "error": "test error",
            },
            "sim4": {"status": "running", "config_file": "test4.yml"},
        }

        result = await handlers._health_check_tool({})
        text = result["content"][0]["text"]

        # Check counts are correct
        assert "Active simulations: 2/" in text  # 2 running
        assert "Completed simulations: 1" in text  # 1 completed
        assert "Failed simulations: 1" in text  # 1 failed
        assert "Available slots:" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_health_check_tool_status_reporting(self, test_config):
        """Test health check reports enabled tools correctly."""
        # Test with all tools enabled
        handlers = SUEWSMCPHandlers(test_config)
        result = await handlers._health_check_tool({})
        text = result["content"][0]["text"]

        assert "simulation" in text
        assert "validation" in text
        assert "analysis" in text

        # Test with minimal configuration
        minimal_config = MCPServerConfig(
            enable_simulation_tool=False,
            enable_validation_tool=True,
            enable_analysis_tool=False,
        )
        minimal_handlers = SUEWSMCPHandlers(minimal_config)
        result = await minimal_handlers._health_check_tool({})
        text = result["content"][0]["text"]

        assert "validation" in text
        # Should not mention disabled tools
        lines = text.split("\n")
        enabled_line = [line for line in lines if "Enabled tools:" in line]
        assert len(enabled_line) == 1
        assert "simulation" not in enabled_line[0]
        assert "analysis" not in enabled_line[0]


class TestListResourcesTool:
    """Test the list_resources tool in isolation."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_list_resources_basic_functionality(self, handlers, tmp_path):
        """Test basic resource listing functionality."""
        # Mock the templates directory structure
        with patch.object(Path, "parent", new_callable=lambda: tmp_path):
            templates_dir = tmp_path / "templates"
            templates_dir.mkdir(exist_ok=True)

            # Create mock config templates
            configs_dir = templates_dir / "configs"
            configs_dir.mkdir(exist_ok=True)

            (configs_dir / "residential.yml").write_text(
                "# Residential configuration\ndescription: Residential area template\n"
            )
            (configs_dir / "commercial.yml").write_text(
                "# Commercial configuration\ndescription: Commercial area template\n"
            )

            # Create mock workflow documentation
            workflows_dir = templates_dir / "workflows"
            workflows_dir.mkdir(exist_ok=True)
            (workflows_dir / "setup_guide.md").write_text(
                "# Setup Guide\nStep-by-step setup instructions\n"
            )

            result = await handlers._list_resources_tool({"resource_type": "all"})

        assert "content" in result
        assert len(result["content"]) == 1

        text = result["content"][0]["text"]
        assert "Available SUEWS MCP Resources:" in text
        assert "Configuration Templates:" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_list_resources_filtered_types(self, handlers, tmp_path):
        """Test resource listing with specific types."""
        with patch.object(Path, "parent", new_callable=lambda: tmp_path):
            templates_dir = tmp_path / "templates"
            templates_dir.mkdir(exist_ok=True)

            configs_dir = templates_dir / "configs"
            configs_dir.mkdir(exist_ok=True)
            (configs_dir / "test.yml").write_text("test: config")

            # Test config_template filter
            result = await handlers._list_resources_tool(
                {"resource_type": "config_template"}
            )
            text = result["content"][0]["text"]

            assert "Configuration Templates:" in text
            # Should not include other sections when filtered

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_list_resources_missing_directory(self, handlers, tmp_path):
        """Test resource listing when templates directory doesn't exist."""
        with patch.object(Path, "parent", new_callable=lambda: tmp_path):
            # Don't create templates directory
            result = await handlers._list_resources_tool({"resource_type": "all"})

        # Should not crash, should return empty resource list
        assert "content" in result
        assert isinstance(result["content"], list)
        text = result["content"][0]["text"]
        assert "Available SUEWS MCP Resources:" in text


class TestGetResourceTool:
    """Test the get_resource tool in isolation."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_get_resource_basic_functionality(self, handlers, tmp_path):
        """Test basic resource retrieval functionality."""
        # Create a test resource file
        test_content = "# Test Configuration\nsite:\n  lat: 51.5\n  lon: -0.1"
        resource_file = tmp_path / "templates" / "configs" / "test.yml"
        resource_file.parent.mkdir(parents=True, exist_ok=True)
        resource_file.write_text(test_content)

        with patch.object(Path, "parent", new_callable=lambda: tmp_path):
            result = await handlers._get_resource_tool(
                {"resource_path": "templates/configs/test.yml"}
            )

        assert "content" in result
        assert not result.get("is_error", False)

        text = result["content"][0]["text"]
        assert "Resource: templates/configs/test.yml" in text
        assert test_content in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_get_resource_missing_file(self, handlers, tmp_path):
        """Test resource retrieval for non-existent file."""
        with patch.object(Path, "parent", new_callable=lambda: tmp_path):
            result = await handlers._get_resource_tool(
                {"resource_path": "templates/configs/nonexistent.yml"}
            )

        assert "content" in result
        assert result.get("is_error", True)

        text = result["content"][0]["text"]
        assert "not found" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_get_resource_security_path_traversal(self, handlers, tmp_path):
        """Test that path traversal attacks are prevented."""
        # Try to access file outside allowed directory
        with patch.object(Path, "parent", new_callable=lambda: tmp_path):
            result = await handlers._get_resource_tool(
                {"resource_path": "../../../etc/passwd"}
            )

        assert "content" in result
        assert result.get("is_error", True)

        text = result["content"][0]["text"]
        assert "Access denied" in text or "outside allowed directories" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_get_resource_missing_parameter(self, handlers):
        """Test resource retrieval with missing parameters."""
        result = await handlers._get_resource_tool({})

        assert "content" in result
        assert result.get("is_error", True)

        text = result["content"][0]["text"]
        assert "required" in text


class TestValidationTools:
    """Test validation tools in isolation."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_validate_suews_config_basic(self, handlers):
        """Test basic SUEWS config validation (legacy tool)."""
        result = await handlers._validate_config_tool(
            {"config_file": "/path/to/test_config.yml", "strict": False}
        )

        assert "content" in result
        assert not result.get("is_error", False)

        text = result["content"][0]["text"]
        assert "valid" in text.lower()
        assert "/path/to/test_config.yml" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_validate_suews_config_strict_mode(self, handlers):
        """Test SUEWS config validation in strict mode."""
        result = await handlers._validate_config_tool(
            {"config_file": "/path/to/test_config.yml", "strict": True}
        )

        assert "content" in result
        text = result["content"][0]["text"]
        assert "strict mode" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_validate_suews_config_missing_file(self, handlers):
        """Test validation with missing config_file parameter."""
        result = await handlers._validate_config_tool({})

        assert "content" in result
        assert result.get("is_error", True)

        text = result["content"][0]["text"]
        assert "required" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    @patch("suews_mcp.handlers.ConfigValidator")
    async def test_validate_config_enhanced_tool(self, mock_validator_class, handlers):
        """Test enhanced configuration validation tool."""
        # Mock the validator
        mock_validator = Mock()
        mock_validator_class.return_value = mock_validator

        mock_result = Mock()
        mock_result.success = True
        mock_result.issues = []
        mock_result.processing_log = ["Validation started", "Validation completed"]
        mock_result.get_summary.return_value = {
            "success": True,
            "total_issues": 0,
            "errors": 0,
            "warnings": 0,
        }
        mock_validator.validate_config.return_value = mock_result

        result = await handlers._validate_config_enhanced_tool(
            {
                "config_file": "/path/to/test.yml",
                "strict_mode": False,
                "check_file_paths": True,
            }
        )

        assert "content" in result
        assert not result.get("is_error", False)

        text = result["content"][0]["text"]
        assert "Validation Results" in text
        assert "PASSED" in text or "✓" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    @patch("suews_mcp.handlers.ConfigValidator")
    async def test_validate_config_enhanced_with_errors(
        self, mock_validator_class, handlers
    ):
        """Test enhanced validation with errors."""
        mock_validator = Mock()
        mock_validator_class.return_value = mock_validator

        mock_issue = Mock()
        mock_issue.severity = "error"
        mock_issue.message = "Invalid surface fraction"
        mock_issue.location = "surfaces.fr_paved"
        mock_issue.issue_type = "range_error"

        mock_result = Mock()
        mock_result.success = False
        mock_result.issues = [mock_issue]
        mock_result.processing_log = ["Validation started", "Error found"]
        mock_result.get_summary.return_value = {
            "success": False,
            "total_issues": 1,
            "errors": 1,
            "warnings": 0,
        }
        mock_validator.validate_config.return_value = mock_result

        result = await handlers._validate_config_enhanced_tool(
            {"config_file": "/path/to/test.yml"}
        )

        assert "content" in result
        assert result.get("is_error", True)

        text = result["content"][0]["text"]
        assert "FAILED" in text or "✗" in text
        assert "Invalid surface fraction" in text


class TestSimulationTool:
    """Test simulation tools in isolation."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_run_simulation_basic_functionality(self, handlers):
        """Test basic simulation functionality."""
        result = await handlers._run_simulation_tool(
            {
                "config_file": "/path/to/config.yml",
                "simulation_id": "test_sim_001",
                "output_dir": "/path/to/output",
            }
        )

        assert "content" in result
        assert not result.get("is_error", False)

        text = result["content"][0]["text"]
        assert "completed successfully" in text
        assert "/path/to/config.yml" in text
        assert "/path/to/output" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_run_simulation_auto_id_generation(self, handlers):
        """Test simulation with automatic ID generation."""
        result = await handlers._run_simulation_tool(
            {"config_file": "/path/to/config.yml"}
        )

        assert "content" in result
        assert not result.get("is_error", False)

        # Check that simulation was tracked
        assert len(handlers._active_simulations) > 0

        # Find the generated simulation ID
        sim_ids = list(handlers._active_simulations.keys())
        assert len(sim_ids) == 1
        assert sim_ids[0].startswith("sim_")

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_run_simulation_missing_config(self, handlers):
        """Test simulation with missing config_file parameter."""
        result = await handlers._run_simulation_tool(
            {"simulation_id": "test_sim", "output_dir": "/path/to/output"}
        )

        assert "content" in result
        assert result.get("is_error", True)

        text = result["content"][0]["text"]
        assert "required" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_run_simulation_tracking(self, handlers):
        """Test that simulations are properly tracked."""
        initial_count = len(handlers._active_simulations)

        # Start simulation
        result = await handlers._run_simulation_tool(
            {"config_file": "/path/to/config.yml", "simulation_id": "tracked_sim"}
        )

        # Check simulation was added and marked as completed
        assert len(handlers._active_simulations) == initial_count + 1
        assert "tracked_sim" in handlers._active_simulations

        sim_info = handlers._active_simulations["tracked_sim"]
        assert sim_info["status"] == "completed"
        assert sim_info["config_file"] == "/path/to/config.yml"
        assert "start_time" in sim_info

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_run_simulation_concurrency_limiting(self, test_config):
        """Test simulation concurrency limiting."""
        # Set up handler with low concurrency limit
        test_config.max_concurrent_simulations = 1
        handlers = SUEWSMCPHandlers(test_config)

        # Mock long-running simulation
        original_sleep = __builtins__.__dict__.get("asyncio", Mock()).sleep
        sleep_calls = []

        async def mock_sleep(duration):
            sleep_calls.append(duration)
            # Simulate some delay but don't actually wait
            return

        with patch("asyncio.sleep", mock_sleep):
            # Start two simulations concurrently
            import asyncio

            task1 = asyncio.create_task(
                handlers._run_simulation_tool(
                    {"config_file": "/path/to/config1.yml", "simulation_id": "sim1"}
                )
            )
            task2 = asyncio.create_task(
                handlers._run_simulation_tool(
                    {"config_file": "/path/to/config2.yml", "simulation_id": "sim2"}
                )
            )

            # Wait for both to complete
            results = await asyncio.gather(task1, task2)

            # Both should succeed
            assert not results[0].get("is_error", False)
            assert not results[1].get("is_error", False)

            # Both should be tracked
            assert "sim1" in handlers._active_simulations
            assert "sim2" in handlers._active_simulations


class TestAnalysisTool:
    """Test analysis tools in isolation."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_analyze_output_basic_functionality(self, handlers):
        """Test basic output analysis functionality."""
        result = await handlers._analyze_output_tool(
            {
                "output_file": "/path/to/output.csv",
                "metrics": ["QH", "QE", "QN"],
                "time_period": "daily",
            }
        )

        assert "content" in result
        assert not result.get("is_error", False)

        text = result["content"][0]["text"]
        assert "Analysis of" in text
        assert "/path/to/output.csv" in text
        assert "QH, QE, QN" in text
        assert "daily" in text
        assert "completed successfully" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_analyze_output_default_parameters(self, handlers):
        """Test analysis with default parameters."""
        result = await handlers._analyze_output_tool(
            {"output_file": "/path/to/output.csv"}
        )

        assert "content" in result
        text = result["content"][0]["text"]

        # Should use default metrics and time period
        assert "QH, QE, QN" in text  # Default metrics
        assert "all" in text  # Default time period

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_analyze_output_missing_file(self, handlers):
        """Test analysis with missing output_file parameter."""
        result = await handlers._analyze_output_tool(
            {"metrics": ["QH", "QE"], "time_period": "hourly"}
        )

        assert "content" in result
        assert result.get("is_error", True)

        text = result["content"][0]["text"]
        assert "required" in text


class TestDataProcessingTools:
    """Test data processing tools in isolation."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    @patch("suews_mcp.handlers.ForcingDataPreprocessor")
    async def test_preprocess_forcing_basic(self, mock_preprocessor_class, handlers):
        """Test basic forcing data preprocessing."""
        # Mock the preprocessor
        mock_preprocessor = Mock()
        mock_preprocessor_class.return_value = mock_preprocessor

        mock_result = Mock()
        mock_result.success = True
        mock_result.issues = []
        mock_result.metadata = {"detected_timestep_seconds": 3600}
        mock_result.processing_log = ["Started preprocessing", "Completed successfully"]
        mock_result.get_summary.return_value = {
            "success": True,
            "data_shape": "(24, 20)",
            "total_issues": 0,
            "errors": 0,
            "warnings": 0,
            "info": 0,
        }
        mock_preprocessor.preprocess_forcing_file.return_value = mock_result

        result = await handlers._preprocess_forcing_tool(
            {
                "input_file": "/path/to/forcing.csv",
                "output_file": "/path/to/processed.csv",
                "validate_energy_balance": True,
            }
        )

        assert "content" in result
        assert not result.get("is_error", False)

        text = result["content"][0]["text"]
        assert "Preprocessing Results" in text
        assert "SUCCESS" in text or "✓" in text
        assert "/path/to/forcing.csv" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    @patch("suews_mcp.handlers.DataFormatConverter")
    async def test_convert_data_format_basic(self, mock_converter_class, handlers):
        """Test basic data format conversion."""
        # Mock the converter
        mock_converter = Mock()
        mock_converter_class.return_value = mock_converter

        mock_result = Mock()
        mock_result.success = True
        mock_result.issues = []
        mock_result.metadata = {"input_shape": "(100, 10)", "output_shape": "(100, 10)"}
        mock_result.processing_log = ["Started conversion", "Conversion completed"]
        mock_result.get_summary.return_value = {
            "success": True,
            "data_shape": "(100, 10)",
            "total_issues": 0,
            "errors": 0,
            "warnings": 0,
        }
        mock_converter.convert_format.return_value = mock_result

        result = await handlers._convert_data_format_tool(
            {
                "input_file": "/path/to/data.csv",
                "output_file": "/path/to/data.txt",
                "input_format": "csv",
                "output_format": "suews_txt",
                "column_mapping": {"temp": "Tair", "humidity": "RH"},
            }
        )

        assert "content" in result
        assert not result.get("is_error", False)

        text = result["content"][0]["text"]
        assert "Conversion Results" in text
        assert "SUCCESS" in text or "✓" in text
        assert "CSV" in text and "SUEWS_TXT" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_convert_data_format_missing_parameters(self, handlers):
        """Test data conversion with missing required parameters."""
        result = await handlers._convert_data_format_tool(
            {
                "input_file": "/path/to/data.csv",
                "output_file": "/path/to/data.txt",
                # Missing input_format and output_format
            }
        )

        assert "content" in result
        assert result.get("is_error", True)

        text = result["content"][0]["text"]
        assert "Missing required parameters" in text
        assert "input_format" in text
        assert "output_format" in text


class TestSuPyMCPToolsIntegration:
    """Test integration with SuPy MCP tools when available."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    @pytest.mark.skipif(
        not SUPY_MCP_TOOLS_AVAILABLE, reason="SuPy MCP tools not available"
    )
    async def test_supy_tools_initialization(self, test_config):
        """Test that SuPy MCP tools are properly initialized when available."""
        handlers = SUEWSMCPHandlers(test_config)

        # Check that tools were initialized
        assert handlers._configure_tool is not None
        assert handlers._run_tool is not None
        assert handlers._analyze_tool is not None

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_fallback_tools_when_supy_unavailable(self, test_config):
        """Test that fallback tools are used when SuPy MCP tools are not available."""
        with patch("suews_mcp.handlers.SUPY_MCP_TOOLS_AVAILABLE", False):
            handlers = SUEWSMCPHandlers(test_config)

            # Should use None for SuPy tools
            assert handlers._configure_tool is None
            assert handlers._run_tool is None
            assert handlers._analyze_tool is None

            # But should still be able to list tools (fallback implementations)
            result = await handlers.handle_list_tools()
            tools = {tool["name"] for tool in result["tools"]}

            # Should have fallback tools
            assert "run_suews_simulation" in tools
            assert "validate_suews_config" in tools
            assert "analyze_suews_output" in tools

    @pytest.mark.unit
    @pytest.mark.asyncio
    @patch("suews_mcp.handlers.SUPY_MCP_TOOLS_AVAILABLE", True)
    async def test_supy_tool_execution_mock(self, handlers):
        """Test SuPy tool execution with mocked tools."""
        # Mock the SuPy tools
        mock_configure_tool = Mock()
        mock_configure_tool.get_definition.return_value = {
            "name": "configure_simulation",
            "description": "Configure SUEWS simulation",
            "inputSchema": {"type": "object", "properties": {}},
        }
        mock_configure_tool.execute = AsyncMock(
            return_value={
                "success": True,
                "message": "Configuration completed",
                "data": {},
            }
        )

        handlers._configure_tool = mock_configure_tool

        # Test tool execution
        result = await handlers.handle_call_tool("configure_simulation", {})

        assert "content" in result
        assert not result.get("is_error", False)

        text = result["content"][0]["text"]
        assert "Configuration completed" in text

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_tool_format_result_helper(self, handlers):
        """Test the _format_tool_result helper method."""
        # Test successful result
        success_result = {
            "success": True,
            "message": "Operation successful",
            "data": {"key": "value", "number": 42},
        }

        formatted = handlers._format_tool_result(success_result)

        assert "content" in formatted
        assert not formatted.get("is_error", False)

        text = formatted["content"][0]["text"]
        assert "Operation successful" in text
        assert "Key: value" in text
        assert "Number: 42" in text

        # Test error result
        error_result = {"success": False, "errors": ["Error 1", "Error 2"]}

        formatted = handlers._format_tool_result(error_result)

        assert "content" in formatted
        assert formatted.get("is_error", True)

        text = formatted["content"][0]["text"]
        assert "Error 1" in text
        assert "Error 2" in text
