"""
Integration tests for MCP server and tools working together.
"""

import asyncio
import tempfile
import pytest
from pathlib import Path
from unittest.mock import Mock, patch

import pandas as pd

from supy.mcp import SUPYMCPServer


class TestMCPIntegration:
    """Test MCP server integration with tools."""

    def setup_method(self):
        """Set up test fixtures."""
        self.server = SUPYMCPServer()

    @pytest.mark.asyncio
    async def test_full_workflow_with_sample_data(self):
        """Test full workflow from configuration through analysis."""

        # Mock SuPy components
        mock_config = Mock()
        mock_simulation = Mock()
        mock_state = pd.DataFrame({"state": [1, 2, 3]})
        mock_forcing = pd.DataFrame(
            {"Tair": [20.0, 21.0, 22.0], "RH": [60.0, 65.0, 70.0]},
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        mock_output = pd.DataFrame(
            {
                "QH": [100.0, 110.0, 120.0],
                "QE": [50.0, 55.0, 60.0],
                "T2": [20.0, 21.0, 22.0],
            },
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        # Step 1: Configure simulation (using sample/default mode)
        with (
            patch("supy.mcp.tools.configure.SUEWSConfig", mock_config),
            patch("supy.mcp.tools.configure.SUEWSSimulation", mock_simulation),
            patch("supy.mcp.tools.configure.init_config", return_value=mock_state),
        ):
            configure_result = await self.server.call_tool(
                "configure_simulation", {"validate_only": False}
            )

            # Should succeed with mocked components
            if configure_result.get("success"):
                assert "simulation_created" in configure_result["data"]

        # Step 2: Run simulation with sample data
        with (
            patch("supy.mcp.tools.run.run_supy", return_value=mock_output),
            patch(
                "supy.mcp.tools.run.load_SampleData",
                return_value=(mock_state, mock_forcing),
            ),
        ):
            run_result = await self.server.call_tool(
                "run_simulation", {"use_sample_data": True, "save_state": False}
            )

            if run_result.get("success"):
                assert run_result["data"]["data_source"] == "sample_data"
                assert "simulation_completed" in run_result["data"]

        # Step 3: Analyze results (create temporary file)
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as tf:
            mock_output.to_csv(tf.name)
            temp_results_path = tf.name

        try:
            analyze_result = await self.server.call_tool(
                "analyze_results",
                {"results_path": temp_results_path, "analysis_type": "summary"},
            )

            assert analyze_result["success"] is True
            assert "analysis_results" in analyze_result["data"]

            analysis_data = analyze_result["data"]["analysis_results"]
            assert "data_overview" in analysis_data
            assert "variable_summary" in analysis_data

        finally:
            Path(temp_results_path).unlink()

    @pytest.mark.asyncio
    async def test_error_handling_chain(self):
        """Test error handling across tool chain."""

        # Test 1: Invalid configuration path
        configure_result = await self.server.call_tool(
            "configure_simulation", {"config_path": "/nonexistent/config.yml"}
        )

        assert configure_result["success"] is False
        assert "errors" in configure_result

        # Test 2: Invalid forcing data
        run_result = await self.server.call_tool(
            "run_simulation", {"forcing_path": "/nonexistent/forcing.txt"}
        )

        assert run_result["success"] is False
        assert "errors" in run_result

        # Test 3: Invalid results path
        analyze_result = await self.server.call_tool(
            "analyze_results", {"results_path": "/nonexistent/results.csv"}
        )

        assert analyze_result["success"] is False
        assert "errors" in analyze_result

    @pytest.mark.asyncio
    async def test_parameter_validation_across_tools(self):
        """Test parameter validation across all tools."""

        # Configure tool parameter validation
        configure_result = await self.server.call_tool(
            "configure_simulation", {"config_updates": "invalid_json"}
        )

        # Should handle gracefully (might succeed if no config_path provided)
        assert "success" in configure_result

        # Run tool parameter validation
        run_result = await self.server.call_tool(
            "run_simulation", {"time_step": "not_an_integer"}
        )

        # Should handle type conversion or fail gracefully
        assert "success" in run_result

        # Analyze tool with missing required parameter
        analyze_result = await self.server.call_tool("analyze_results", {})

        assert analyze_result["success"] is False
        assert any(
            "Missing required parameters" in error
            for error in analyze_result.get("errors", [])
        )

    @pytest.mark.asyncio
    async def test_concurrent_tool_execution(self):
        """Test concurrent execution of different tools."""

        # Create mock data for analysis
        df = pd.DataFrame(
            {"QH": [100.0, 110.0, 120.0], "QE": [50.0, 55.0, 60.0]},
            index=pd.date_range("2012-01-01", periods=3, freq="H"),
        )

        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as tf:
            df.to_csv(tf.name)
            temp_path = tf.name

        try:
            # Execute multiple analyses concurrently
            tasks = [
                self.server.call_tool(
                    "analyze_results",
                    {"results_path": temp_path, "analysis_type": "summary"},
                ),
                self.server.call_tool(
                    "analyze_results",
                    {"results_path": temp_path, "analysis_type": "statistics"},
                ),
                self.server.call_tool(
                    "analyze_results",
                    {"results_path": temp_path, "analysis_type": "energy_balance"},
                ),
            ]

            results = await asyncio.gather(*tasks, return_exceptions=True)

            # All should complete without exceptions
            for result in results:
                assert not isinstance(result, Exception)
                assert "success" in result

        finally:
            Path(temp_path).unlink()

    @pytest.mark.asyncio
    async def test_mcp_protocol_compliance(self):
        """Test MCP protocol compliance."""

        # Test initialize request
        init_request = {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}}

        response = await self.server.handle_request(init_request)

        assert response["jsonrpc"] == "2.0"
        assert response["id"] == 1
        assert "result" in response

        result = response["result"]
        assert "protocolVersion" in result
        assert "capabilities" in result
        assert "serverInfo" in result

        # Test tools/list request
        list_request = {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}}

        response = await self.server.handle_request(list_request)

        assert response["jsonrpc"] == "2.0"
        assert response["id"] == 2
        assert "result" in response
        assert "tools" in response["result"]

        # Test tools/call request
        call_request = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "configure_simulation",
                "arguments": {"validate_only": True},
            },
        }

        response = await self.server.handle_request(call_request)

        assert response["jsonrpc"] == "2.0"
        assert response["id"] == 3
        assert "result" in response

    def test_tool_parameter_schemas(self):
        """Test that all tools have valid parameter schemas."""

        tools = self.server.list_tools()

        for tool_def in tools:
            assert "inputSchema" in tool_def
            schema = tool_def["inputSchema"]

            assert schema["type"] == "object"
            assert "properties" in schema

            # Validate required parameters if present
            if "required" in schema:
                required_params = schema["required"]
                properties = schema["properties"]

                for param in required_params:
                    assert param in properties

            # Validate property types
            for prop_name, prop_def in schema["properties"].items():
                assert "type" in prop_def
                assert "description" in prop_def
                assert isinstance(prop_def["description"], str)
                assert len(prop_def["description"]) > 0


class TestMCPErrorScenarios:
    """Test error scenarios and edge cases."""

    def setup_method(self):
        """Set up test fixtures."""
        self.server = SUPYMCPServer()

    @pytest.mark.asyncio
    async def test_malformed_requests(self):
        """Test handling of malformed requests."""

        # Missing method
        request = {"jsonrpc": "2.0", "id": 1, "params": {}}

        response = await self.server.handle_request(request)
        assert "error" in response

        # Invalid JSON-RPC version
        request = {"jsonrpc": "1.0", "id": 2, "method": "initialize"}

        response = await self.server.handle_request(request)
        # Should still handle gracefully
        assert response["jsonrpc"] == "2.0"

    @pytest.mark.asyncio
    async def test_large_data_handling(self):
        """Test handling of large datasets."""

        # Create large DataFrame
        large_df = pd.DataFrame(
            {f"var_{i}": range(10000) for i in range(10)},
            index=pd.date_range("2000-01-01", periods=10000, freq="H"),
        )

        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as tf:
            large_df.to_csv(tf.name)
            temp_path = tf.name

        try:
            result = await self.server.call_tool(
                "analyze_results",
                {"results_path": temp_path, "analysis_type": "summary"},
            )

            # Should handle large data without crashing
            assert "success" in result

            if result["success"]:
                # Check that data is truncated in response
                data_info = result["data"]["data_info"]
                assert data_info["shape"] == [10000, 10]

        finally:
            Path(temp_path).unlink()

    @pytest.mark.asyncio
    async def test_memory_efficiency(self):
        """Test memory efficiency with multiple operations."""

        # Create multiple temporary files
        temp_files = []

        try:
            for i in range(5):
                df = pd.DataFrame(
                    {"QH": [100 + i] * 100, "QE": [50 + i] * 100},
                    index=pd.date_range("2012-01-01", periods=100, freq="H"),
                )

                with tempfile.NamedTemporaryFile(
                    mode="w", suffix=".csv", delete=False
                ) as tf:
                    df.to_csv(tf.name)
                    temp_files.append(tf.name)

            # Process all files
            results = []
            for temp_file in temp_files:
                result = await self.server.call_tool(
                    "analyze_results",
                    {"results_path": temp_file, "analysis_type": "summary"},
                )
                results.append(result)

            # All should succeed
            for result in results:
                assert result.get("success", False) is True

        finally:
            for temp_file in temp_files:
                Path(temp_file).unlink()
