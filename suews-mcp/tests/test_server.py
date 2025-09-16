"""Comprehensive tests for SUEWS MCP Server."""

import pytest
import asyncio
import os
import sys
from unittest.mock import Mock, AsyncMock, patch, MagicMock
from typing import Dict, Any

# Test imports
from suews_mcp.server import SUEWSMCPServer, run_server, main
from suews_mcp.handlers import SUEWSMCPHandlers, MCP_AVAILABLE
from suews_mcp.config import MCPServerConfig, load_config, setup_logging


class TestSUEWSMCPServer:
    """Test suite for SUEWSMCPServer class."""

    @pytest.mark.unit
    @pytest.mark.server
    def test_server_initialization_with_config(self, test_config):
        """Test server initializes correctly with provided configuration."""
        server = SUEWSMCPServer(test_config)

        assert server.config == test_config
        assert server.handlers is not None
        assert isinstance(server.handlers, SUEWSMCPHandlers)
        assert server.config.server_name == "test-suews-mcp"
        assert server.config.server_version == "0.1.0-test"

    @pytest.mark.unit
    @pytest.mark.server
    def test_server_initialization_without_config(self):
        """Test server initializes correctly with default configuration."""
        server = SUEWSMCPServer()

        assert server.config is not None
        assert server.handlers is not None
        assert server.config.server_name == "suews-mcp"
        assert server.config.server_version == "0.1.0"

    @pytest.mark.unit
    @pytest.mark.server
    def test_server_initialization_mcp_available(self, test_config):
        """Test server initialization when MCP library is available."""
        server = SUEWSMCPServer(test_config)

        if MCP_AVAILABLE:
            assert server.server is not None
        else:
            assert server.server is None

    @pytest.mark.unit
    @pytest.mark.server
    def test_server_initialization_mcp_unavailable(self, test_config):
        """Test server initialization when MCP library is unavailable."""
        with patch("suews_mcp.server.MCP_AVAILABLE", False):
            with patch("suews_mcp.server.HANDLERS_MCP_AVAILABLE", False):
                server = SUEWSMCPServer(test_config)
                assert server.server is None

    @pytest.mark.unit
    @pytest.mark.server
    @pytest.mark.skipif(not MCP_AVAILABLE, reason="MCP library not available")
    def test_register_handlers_with_mcp(self, test_config):
        """Test handler registration when MCP is available."""
        server = SUEWSMCPServer(test_config)

        # Check that server has the MCP server instance
        assert server.server is not None

        # The handlers should be registered (we can't directly test this
        # without inspecting the MCP server internals, but we can verify
        # that the server was created properly)
        assert hasattr(server, "server")
        assert hasattr(server, "handlers")

    @pytest.mark.asyncioio
    @pytest.mark.server
    @pytest.mark.integration
    async def test_server_run_mcp_unavailable(self, test_config):
        """Test server run method when MCP library is unavailable."""
        with patch("suews_mcp.server.MCP_AVAILABLE", False):
            server = SUEWSMCPServer(test_config)

            with pytest.raises(RuntimeError, match="MCP library not available"):
                await server.run()

    @pytest.mark.asyncio
    @pytest.mark.server
    @pytest.mark.integration
    @pytest.mark.skipif(not MCP_AVAILABLE, reason="MCP library not available")
    async def test_server_run_with_mcp(self, test_config, mock_stdio_server):
        """Test server run method with MCP library available."""
        with patch("suews_mcp.server.stdio_server", return_value=mock_stdio_server):
            with patch.object(SUEWSMCPServer, "server") as mock_server:
                mock_server.run = AsyncMock()
                server = SUEWSMCPServer(test_config)
                server.server = mock_server

                # This should complete without error
                await server.run()

                # Verify server.run was called
                mock_server.run.assert_called_once()

    @pytest.mark.asyncio
    @pytest.mark.server
    @pytest.mark.integration
    @pytest.mark.skipif(not MCP_AVAILABLE, reason="MCP library not available")
    async def test_server_run_keyboard_interrupt(self, test_config, mock_stdio_server):
        """Test server handles KeyboardInterrupt gracefully."""
        with patch("suews_mcp.server.stdio_server", return_value=mock_stdio_server):
            with patch.object(SUEWSMCPServer, "server") as mock_server:
                mock_server.run = AsyncMock(side_effect=KeyboardInterrupt())
                server = SUEWSMCPServer(test_config)
                server.server = mock_server

                # Should not raise an exception
                await server.run()

                mock_server.run.assert_called_once()

    @pytest.mark.asyncio
    @pytest.mark.server
    @pytest.mark.integration
    @pytest.mark.skipif(not MCP_AVAILABLE, reason="MCP library not available")
    async def test_server_run_with_exception(self, test_config, mock_stdio_server):
        """Test server handles exceptions during run."""
        with patch("suews_mcp.server.stdio_server", return_value=mock_stdio_server):
            with patch.object(SUEWSMCPServer, "server") as mock_server:
                mock_server.run = AsyncMock(side_effect=Exception("Test error"))
                server = SUEWSMCPServer(test_config)
                server.server = mock_server

                with pytest.raises(Exception, match="Test error"):
                    await server.run()

    @pytest.mark.asyncio
    @pytest.mark.server
    async def test_server_cleanup(self, test_config):
        """Test server cleanup method."""
        server = SUEWSMCPServer(test_config)

        # Mock the handlers cleanup method
        server.handlers.cleanup = Mock()

        # Should complete without error
        await server.cleanup()

        # Verify cleanup was called
        server.handlers.cleanup.assert_called_once()

    @pytest.mark.asyncio
    @pytest.mark.server
    async def test_server_cleanup_with_exception(self, test_config):
        """Test server cleanup handles exceptions."""
        server = SUEWSMCPServer(test_config)

        # Mock the handlers cleanup method to raise an exception
        server.handlers.cleanup = Mock(side_effect=Exception("Cleanup error"))

        # Should complete without raising the exception
        await server.cleanup()

        # Verify cleanup was called
        server.handlers.cleanup.assert_called_once()


class TestSUEWSMCPHandlers:
    """Test suite for SUEWSMCPHandlers class."""

    @pytest.mark.unit
    @pytest.mark.handlers
    def test_handlers_initialization(self, test_config):
        """Test handlers initialize correctly."""
        handlers = SUEWSMCPHandlers(test_config)

        assert handlers.config == test_config
        assert (
            handlers._simulation_semaphore._value
            == test_config.max_concurrent_simulations
        )
        assert len(handlers._active_simulations) == 0

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_initialize(self, handlers, sample_initialize_params):
        """Test handle_initialize method."""
        result = await handlers.handle_initialize(sample_initialize_params)

        assert result["protocol_version"] == "2024-11-05"
        assert "capabilities" in result
        assert "server_info" in result
        assert result["server_info"]["name"] == handlers.config.server_name
        assert result["server_info"]["version"] == handlers.config.server_version

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_list_tools_full_config(self, handlers):
        """Test handle_list_tools with all tools enabled."""
        result = await handlers.handle_list_tools()

        assert "tools" in result
        tools = result["tools"]

        # Should have all tools plus health_check
        expected_tools = {
            "run_suews_simulation",
            "validate_suews_config",
            "analyze_suews_output",
            "health_check",
        }
        actual_tools = {tool["name"] for tool in tools}

        assert expected_tools == actual_tools

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_list_tools_minimal_config(self, minimal_handlers):
        """Test handle_list_tools with minimal configuration."""
        result = await minimal_handlers.handle_list_tools()

        assert "tools" in result
        tools = result["tools"]

        # Should only have enabled tools plus health_check
        expected_tools = {"validate_suews_config", "health_check"}
        actual_tools = {tool["name"] for tool in tools}

        assert expected_tools == actual_tools

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_call_tool_health_check(self, handlers):
        """Test health check tool."""
        result = await handlers.handle_call_tool("health_check", {})

        assert not result.get("is_error", False)
        assert "content" in result
        assert len(result["content"]) > 0

        content_text = result["content"][0]["text"]
        assert "SUEWS MCP Server Health Check" in content_text
        assert "Status: healthy" in content_text
        assert "Version:" in content_text

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_call_tool_simulation(self, handlers, sample_tool_arguments):
        """Test simulation tool."""
        args = sample_tool_arguments["run_suews_simulation"]
        result = await handlers.handle_call_tool("run_suews_simulation", args)

        assert not result.get("is_error", False)
        assert "content" in result

        content_text = result["content"][0]["text"]
        assert "SUEWS simulation completed successfully" in content_text
        assert args["config_file"] in content_text

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_call_tool_validation(self, handlers, sample_tool_arguments):
        """Test validation tool."""
        args = sample_tool_arguments["validate_suews_config"]
        result = await handlers.handle_call_tool("validate_suews_config", args)

        assert not result.get("is_error", False)
        assert "content" in result

        content_text = result["content"][0]["text"]
        assert "Configuration file" in content_text
        assert "is valid" in content_text

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_call_tool_analysis(self, handlers, sample_tool_arguments):
        """Test analysis tool."""
        args = sample_tool_arguments["analyze_suews_output"]
        result = await handlers.handle_call_tool("analyze_suews_output", args)

        assert not result.get("is_error", False)
        assert "content" in result

        content_text = result["content"][0]["text"]
        assert "Analysis of" in content_text
        assert args["output_file"] in content_text
        assert "Analysis completed successfully" in content_text

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_call_tool_unknown(self, handlers):
        """Test calling unknown tool."""
        result = await handlers.handle_call_tool("unknown_tool", {})

        assert result.get("is_error", True)
        assert "content" in result

        content_text = result["content"][0]["text"]
        assert "Unknown tool: unknown_tool" in content_text

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_call_tool_missing_arguments(
        self, handlers, invalid_tool_arguments
    ):
        """Test tools with missing required arguments."""
        for tool_name, invalid_args in invalid_tool_arguments.items():
            result = await handlers.handle_call_tool(tool_name, invalid_args)

            assert result.get("is_error", True)
            assert "content" in result

            content_text = result["content"][0]["text"]
            assert "required" in content_text or "parameter is required" in content_text

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_call_tool_exception_handling(self, handlers):
        """Test tool exception handling."""
        # Mock a method to raise an exception
        original_method = handlers._health_check_tool
        handlers._health_check_tool = AsyncMock(side_effect=Exception("Test exception"))

        result = await handlers.handle_call_tool("health_check", {})

        assert result.get("is_error", True)
        assert "content" in result

        content_text = result["content"][0]["text"]
        assert "Error executing tool health_check: Test exception" in content_text

        # Restore original method
        handlers._health_check_tool = original_method

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_simulation_concurrency_limits(self, test_config):
        """Test simulation concurrency limits."""
        test_config.max_concurrent_simulations = 1
        handlers = SUEWSMCPHandlers(test_config)

        # Start first simulation
        task1 = asyncio.create_task(
            handlers.handle_call_tool(
                "run_suews_simulation", {"config_file": "test1.yml"}
            )
        )

        # Give first task a chance to acquire the semaphore
        await asyncio.sleep(0.01)

        # Start second simulation (should be blocked)
        task2 = asyncio.create_task(
            handlers.handle_call_tool(
                "run_suews_simulation", {"config_file": "test2.yml"}
            )
        )

        # Complete both tasks
        result1 = await task1
        result2 = await task2

        # Both should succeed
        assert not result1.get("is_error", False)
        assert not result2.get("is_error", False)

        # Check that simulations were tracked
        assert len(handlers._active_simulations) == 2

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_list_prompts(self, handlers):
        """Test handle_list_prompts method."""
        result = await handlers.handle_list_prompts()

        assert "prompts" in result
        assert isinstance(result["prompts"], list)
        # Currently returns empty list
        assert len(result["prompts"]) == 0

    @pytest.mark.asyncio
    @pytest.mark.unit
    @pytest.mark.handlers
    async def test_handle_get_prompt(self, handlers):
        """Test handle_get_prompt method."""
        result = await handlers.handle_get_prompt("test_prompt", {})

        assert "messages" in result
        assert isinstance(result["messages"], list)
        # Currently returns "not found" message
        assert len(result["messages"]) > 0
        assert "Prompt test_prompt not found" in str(result)

    @pytest.mark.unit
    @pytest.mark.handlers
    def test_cleanup(self, handlers):
        """Test handlers cleanup method."""
        # Add some test simulations
        handlers._active_simulations["test1"] = {"status": "completed"}
        handlers._active_simulations["test2"] = {"status": "running"}

        assert len(handlers._active_simulations) == 2

        # Cleanup should clear all simulations
        handlers.cleanup()

        assert len(handlers._active_simulations) == 0


class TestConfiguration:
    """Test suite for configuration management."""

    @pytest.mark.unit
    @pytest.mark.config
    def test_default_configuration(self):
        """Test default configuration values."""
        config = load_config()

        assert config.server_name == "suews-mcp"
        assert config.server_version == "0.1.0"
        assert config.log_level == "INFO"
        assert config.suews_timeout == 300
        assert config.max_concurrent_simulations == 5
        assert config.enable_simulation_tool is True
        assert config.enable_validation_tool is True
        assert config.enable_analysis_tool is True

    @pytest.mark.unit
    @pytest.mark.config
    def test_environment_variable_config(self, cleanup_environment):
        """Test configuration from environment variables."""
        os.environ["SUEWS_MCP_SERVER_NAME"] = "env-test-server"
        os.environ["SUEWS_MCP_LOG_LEVEL"] = "DEBUG"
        os.environ["SUEWS_MCP_MAX_CONCURRENT_SIMULATIONS"] = "3"

        config = load_config()

        assert config.server_name == "env-test-server"
        assert config.log_level == "DEBUG"
        assert config.max_concurrent_simulations == 3

    @pytest.mark.unit
    @pytest.mark.config
    def test_config_validation(self):
        """Test configuration validation."""
        # Valid configuration should work
        config = MCPServerConfig(
            log_level="INFO", suews_timeout=60, max_concurrent_simulations=2
        )
        assert config.log_level == "INFO"

        # Test invalid log level
        with pytest.raises(ValueError, match="Log level must be one of"):
            MCPServerConfig(log_level="INVALID")

        # Test invalid timeout
        with pytest.raises(ValueError, match="Timeout must be positive"):
            MCPServerConfig(suews_timeout=-1)

        # Test invalid max concurrent
        with pytest.raises(
            ValueError, match="Max concurrent simulations must be positive"
        ):
            MCPServerConfig(max_concurrent_simulations=0)

    @pytest.mark.unit
    @pytest.mark.config
    def test_config_methods(self, test_config):
        """Test configuration helper methods."""
        # Test get_log_level
        log_level = test_config.get_log_level()
        assert isinstance(log_level, int)
        assert log_level == 10  # DEBUG level

        # Test get_temp_dir
        temp_dir = test_config.get_temp_dir()
        assert isinstance(temp_dir, str)
        assert "suews-mcp" in temp_dir

        # Test to_dict
        config_dict = test_config.to_dict()
        assert isinstance(config_dict, dict)
        assert "server_name" in config_dict
        assert config_dict["server_name"] == "test-suews-mcp"

    @pytest.mark.unit
    @pytest.mark.config
    def test_setup_logging(self, test_config):
        """Test logging setup."""
        # This should not raise an error
        setup_logging(test_config)

        # Test with debug enabled
        test_config.enable_debug = True
        setup_logging(test_config)


class TestHealthCheck:
    """Test suite specifically for health check functionality."""

    @pytest.mark.asyncio
    @pytest.mark.health
    async def test_health_check_empty_state(self, handlers):
        """Test health check with no active simulations."""
        result = await handlers._health_check_tool({})

        content_text = result["content"][0]["text"]
        assert "Active simulations: 0/" in content_text
        assert "Completed simulations: 0" in content_text
        assert "Failed simulations: 0" in content_text
        assert "Status: healthy" in content_text

    @pytest.mark.asyncio
    @pytest.mark.health
    async def test_health_check_with_simulations(self, handlers):
        """Test health check with various simulation states."""
        # Add test simulations
        handlers._active_simulations["sim1"] = {"status": "running"}
        handlers._active_simulations["sim2"] = {"status": "completed"}
        handlers._active_simulations["sim3"] = {"status": "failed"}
        handlers._active_simulations["sim4"] = {"status": "running"}

        result = await handlers._health_check_tool({})

        content_text = result["content"][0]["text"]
        assert "Active simulations: 2/" in content_text
        assert "Completed simulations: 1" in content_text
        assert "Failed simulations: 1" in content_text
        assert "Status: healthy" in content_text

    @pytest.mark.asyncio
    @pytest.mark.health
    async def test_health_check_available_slots(self, test_config):
        """Test health check shows correct available slots."""
        test_config.max_concurrent_simulations = 3
        handlers = SUEWSMCPHandlers(test_config)

        # Add one running simulation
        handlers._active_simulations["sim1"] = {"status": "running"}

        result = await handlers._health_check_tool({})

        content_text = result["content"][0]["text"]
        assert "Available slots: 2" in content_text

    @pytest.mark.asyncio
    @pytest.mark.health
    async def test_health_check_tools_enabled(self, handlers, minimal_handlers):
        """Test health check shows correct tool status."""
        # Full config - all tools enabled
        result = await handlers._health_check_tool({})
        content_text = result["content"][0]["text"]
        assert "Enabled tools: simulation, validation, analysis" in content_text

        # Minimal config - only validation enabled
        result = await minimal_handlers._health_check_tool({})
        content_text = result["content"][0]["text"]
        assert "Enabled tools: validation" in content_text


class TestErrorHandling:
    """Test suite for error handling scenarios."""

    @pytest.mark.asyncio
    @pytest.mark.unit
    async def test_server_error_handling_in_run(self, test_config, mock_stdio_server):
        """Test server error handling during run method."""
        if not MCP_AVAILABLE:
            pytest.skip("MCP library not available")

        server = SUEWSMCPServer(test_config)

        with patch("suews_mcp.server.stdio_server", return_value=mock_stdio_server):
            with patch.object(
                server.server, "run", side_effect=RuntimeError("Server error")
            ):
                with pytest.raises(RuntimeError, match="Server error"):
                    await server.run()

    @pytest.mark.asyncio
    @pytest.mark.unit
    async def test_tool_error_propagation(self, handlers):
        """Test that tool errors are properly caught and returned."""
        # Test with a tool that will cause an error by patching its method
        original_method = handlers._run_simulation_tool
        handlers._run_simulation_tool = AsyncMock(
            side_effect=ValueError("Invalid config")
        )

        result = await handlers.handle_call_tool(
            "run_suews_simulation", {"config_file": "test.yml"}
        )

        assert result.get("is_error", True)
        assert "Invalid config" in result["content"][0]["text"]

        # Restore original method
        handlers._run_simulation_tool = original_method


class TestIntegration:
    """Integration tests for the complete server setup."""

    @pytest.mark.integration
    @pytest.mark.slow
    def test_full_server_lifecycle(self, test_config):
        """Test complete server initialization and basic operations."""
        server = SUEWSMCPServer(test_config)

        # Test that server is properly initialized
        assert server.config == test_config
        assert server.handlers is not None

        # Test that handlers can perform basic operations
        assert server.handlers.config == test_config
        assert len(server.handlers._active_simulations) == 0

    @pytest.mark.integration
    @pytest.mark.asyncio
    async def test_concurrent_operations(self, handlers):
        """Test concurrent tool operations."""
        # Create multiple concurrent health checks
        tasks = [handlers.handle_call_tool("health_check", {}) for _ in range(5)]

        results = await asyncio.gather(*tasks)

        # All should succeed
        for result in results:
            assert not result.get("is_error", False)
            assert "Status: healthy" in result["content"][0]["text"]


class TestMainFunctions:
    """Test suite for main entry point functions."""

    @pytest.mark.unit
    def test_main_function_keyboard_interrupt(self):
        """Test main function handles KeyboardInterrupt."""
        with patch("suews_mcp.server.asyncio.run", side_effect=KeyboardInterrupt()):
            with pytest.raises(SystemExit) as exc_info:
                main()
            assert exc_info.value.code == 0

    @pytest.mark.unit
    def test_main_function_exception(self):
        """Test main function handles general exceptions."""
        with patch("suews_mcp.server.asyncio.run", side_effect=Exception("Test error")):
            with pytest.raises(SystemExit) as exc_info:
                main()
            assert exc_info.value.code == 1

    @pytest.mark.asyncio
    @pytest.mark.unit
    async def test_run_server_function(self, test_config):
        """Test run_server function."""
        with patch.object(SUEWSMCPServer, "run", new_callable=AsyncMock) as mock_run:
            await run_server(test_config)
            mock_run.assert_called_once()

    @pytest.mark.asyncio
    @pytest.mark.unit
    async def test_run_server_function_default_config(self):
        """Test run_server function with default config."""
        with patch.object(SUEWSMCPServer, "run", new_callable=AsyncMock) as mock_run:
            await run_server()
            mock_run.assert_called_once()
