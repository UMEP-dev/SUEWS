"""
Tests for MCP server functionality.
"""

import asyncio
import json
import pytest
from pathlib import Path
from unittest.mock import Mock, patch

from supy.mcp import SUPYMCPServer
from supy.mcp.tools import ConfigureSimulationTool, RunSimulationTool, AnalyzeResultsTool


class TestSUPYMCPServer:
    """Test MCP server basic functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.server = SUPYMCPServer()
    
    def test_server_initialization(self):
        """Test server initializes correctly."""
        assert self.server.name == "supy-mcp-server"
        assert self.server.version == "1.0.0"
        assert len(self.server.tools) == 3
        assert "configure_simulation" in self.server.tools
        assert "run_simulation" in self.server.tools
        assert "analyze_results" in self.server.tools
    
    def test_list_tools(self):
        """Test tool listing functionality."""
        tools = self.server.list_tools()
        assert len(tools) == 3
        
        tool_names = [tool["name"] for tool in tools]
        assert "configure_simulation" in tool_names
        assert "run_simulation" in tool_names
        assert "analyze_results" in tool_names
        
        # Check tool structure
        for tool in tools:
            assert "name" in tool
            assert "description" in tool
            assert "inputSchema" in tool
            assert tool["inputSchema"]["type"] == "object"
    
    def test_get_capabilities(self):
        """Test capabilities reporting."""
        capabilities = self.server.get_capabilities()
        assert "tools" in capabilities
    
    def test_get_next_request_id(self):
        """Test request ID generation."""
        first_id = self.server.get_next_request_id()
        second_id = self.server.get_next_request_id()
        assert second_id == first_id + 1
    
    @pytest.mark.asyncio
    async def test_call_tool_invalid_name(self):
        """Test calling non-existent tool."""
        with pytest.raises(ValueError, match="Tool 'nonexistent' not found"):
            await self.server.call_tool("nonexistent", {})
    
    @pytest.mark.asyncio
    async def test_handle_initialize_request(self):
        """Test initialize request handling."""
        request = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {}
        }
        
        response = await self.server.handle_request(request)
        
        assert response["jsonrpc"] == "2.0"
        assert response["id"] == 1
        assert "result" in response
        
        result = response["result"]
        assert "protocolVersion" in result
        assert "capabilities" in result
        assert "serverInfo" in result
        
        server_info = result["serverInfo"]
        assert server_info["name"] == "supy-mcp-server"
        assert server_info["version"] == "1.0.0"
    
    @pytest.mark.asyncio
    async def test_handle_tools_list_request(self):
        """Test tools list request handling."""
        request = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list",
            "params": {}
        }
        
        response = await self.server.handle_request(request)
        
        assert response["jsonrpc"] == "2.0"
        assert response["id"] == 2
        assert "result" in response
        
        result = response["result"]
        assert "tools" in result
        assert len(result["tools"]) == 3
    
    @pytest.mark.asyncio
    async def test_handle_unknown_method(self):
        """Test handling of unknown methods."""
        request = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "unknown/method",
            "params": {}
        }
        
        response = await self.server.handle_request(request)
        
        assert response["jsonrpc"] == "2.0"
        assert response["id"] == 3
        assert "error" in response
        
        error = response["error"]
        assert error["code"] == -32603  # Internal error
        assert "Unknown method" in error["message"]
    
    @pytest.mark.asyncio
    async def test_handle_tools_call_missing_params(self):
        """Test tools call with missing parameters."""
        request = {
            "jsonrpc": "2.0",
            "id": 4,
            "method": "tools/call",
            "params": {
                "name": "configure_simulation",
                "arguments": {}
            }
        }
        
        response = await self.server.handle_request(request)
        
        assert response["jsonrpc"] == "2.0"
        assert response["id"] == 4
        assert "result" in response
        
        result = response["result"]
        # Should succeed but may have validation errors in the result
        assert "success" in result


class TestMCPToolIntegration:
    """Test MCP tools integration with server."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.server = SUPYMCPServer()
    
    @pytest.mark.asyncio
    async def test_configure_simulation_tool_definition(self):
        """Test configure simulation tool definition."""
        tool = self.server.tools["configure_simulation"]
        definition = tool.get_definition()
        
        assert definition["name"] == "configure_simulation"
        assert "description" in definition
        assert "inputSchema" in definition
        
        schema = definition["inputSchema"]
        assert "properties" in schema
        # Check for expected parameters
        properties = schema["properties"]
        assert "config_path" in properties
        assert "config_updates" in properties
    
    @pytest.mark.asyncio
    async def test_run_simulation_tool_definition(self):
        """Test run simulation tool definition."""
        tool = self.server.tools["run_simulation"]
        definition = tool.get_definition()
        
        assert definition["name"] == "run_simulation"
        assert "description" in definition
        
        schema = definition["inputSchema"]
        properties = schema["properties"]
        assert "forcing_path" in properties
        assert "use_sample_data" in properties
        assert "time_step" in properties
    
    @pytest.mark.asyncio
    async def test_analyze_results_tool_definition(self):
        """Test analyze results tool definition."""
        tool = self.server.tools["analyze_results"]
        definition = tool.get_definition()
        
        assert definition["name"] == "analyze_results"
        assert "description" in definition
        
        schema = definition["inputSchema"]
        properties = schema["properties"]
        assert "results_path" in properties
        assert "analysis_type" in properties
        
        # Check required parameters
        required = schema.get("required", [])
        assert "results_path" in required