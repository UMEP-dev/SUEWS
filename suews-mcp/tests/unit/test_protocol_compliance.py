"""Unit tests for MCP protocol compliance."""

import pytest
import json
from unittest.mock import Mock, AsyncMock, patch
from typing import Dict, Any, List

# Test imports
from suews_mcp.handlers import SUEWSMCPHandlers, MCP_AVAILABLE
from suews_mcp.config import MCPServerConfig


class TestMCPProtocolCompliance:
    """Test MCP protocol compliance for all required methods."""

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_initialize_protocol_compliance(self, handlers):
        """Test that initialize follows MCP 2024-11-05 protocol."""
        params = {
            "protocol_version": "2024-11-05",
            "capabilities": {"roots": {"list_changed": False}},
            "client_info": {"name": "test-client", "version": "1.0.0"},
        }

        result = await handlers.handle_initialize(params)

        # Check required fields exist
        assert "protocol_version" in result
        assert "capabilities" in result
        assert "server_info" in result

        # Check protocol version compliance
        assert result["protocol_version"] == "2024-11-05"

        # Check server_info structure
        server_info = result["server_info"]
        assert "name" in server_info
        assert "version" in server_info
        assert isinstance(server_info["name"], str)
        assert isinstance(server_info["version"], str)

        # Check capabilities structure
        capabilities = result["capabilities"]
        assert "tools" in capabilities
        assert "prompts" in capabilities
        
        # Tools capability should have list_changed
        if isinstance(capabilities["tools"], dict):
            assert "list_changed" in capabilities["tools"]
            assert isinstance(capabilities["tools"]["list_changed"], bool)

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_list_tools_protocol_compliance(self, handlers):
        """Test that list_tools follows MCP protocol structure."""
        result = await handlers.handle_list_tools()

        # Check required field exists
        assert "tools" in result
        assert isinstance(result["tools"], list)

        # Check each tool has required fields
        for tool in result["tools"]:
            assert "name" in tool
            assert "description" in tool
            assert "input_schema" in tool
            
            # Check field types
            assert isinstance(tool["name"], str)
            assert isinstance(tool["description"], str)
            assert isinstance(tool["input_schema"], dict)
            
            # Check input_schema structure
            schema = tool["input_schema"]
            assert "type" in schema
            assert schema["type"] == "object"
            if "properties" in schema:
                assert isinstance(schema["properties"], dict)
            if "required" in schema:
                assert isinstance(schema["required"], list)

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_call_tool_protocol_compliance_success(self, handlers):
        """Test that call_tool success response follows MCP protocol."""
        result = await handlers.handle_call_tool("health_check", {})

        # Check required fields
        assert "content" in result
        assert isinstance(result["content"], list)
        assert len(result["content"]) > 0

        # Check content structure
        content_item = result["content"][0]
        assert "type" in content_item
        assert "text" in content_item
        assert content_item["type"] == "text"
        assert isinstance(content_item["text"], str)

        # Check error field (should be False or missing for success)
        if "is_error" in result:
            assert result["is_error"] is False

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_call_tool_protocol_compliance_error(self, handlers):
        """Test that call_tool error response follows MCP protocol."""
        result = await handlers.handle_call_tool("unknown_tool", {})

        # Check required fields for error response
        assert "content" in result
        assert "is_error" in result
        assert result["is_error"] is True
        
        # Check content structure for error
        assert isinstance(result["content"], list)
        assert len(result["content"]) > 0
        
        content_item = result["content"][0]
        assert "type" in content_item
        assert "text" in content_item
        assert content_item["type"] == "text"
        assert isinstance(content_item["text"], str)

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_list_prompts_protocol_compliance(self, handlers):
        """Test that list_prompts follows MCP protocol structure."""
        result = await handlers.handle_list_prompts()

        # Check required field exists
        assert "prompts" in result
        assert isinstance(result["prompts"], list)

        # Check each prompt has required fields (if any prompts exist)
        for prompt in result["prompts"]:
            assert "name" in prompt
            assert "description" in prompt
            
            # Check field types
            assert isinstance(prompt["name"], str)
            assert isinstance(prompt["description"], str)
            
            # Check optional arguments field
            if "arguments" in prompt:
                assert isinstance(prompt["arguments"], list)
                for arg in prompt["arguments"]:
                    assert "name" in arg
                    assert "description" in arg
                    assert isinstance(arg["name"], str)
                    assert isinstance(arg["description"], str)
                    if "required" in arg:
                        assert isinstance(arg["required"], bool)

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_get_prompt_protocol_compliance(self, handlers):
        """Test that get_prompt follows MCP protocol structure."""
        result = await handlers.handle_get_prompt("setup_simulation", {})

        # Check required field exists
        assert "messages" in result
        assert isinstance(result["messages"], list)

        # Check each message has required fields (if any messages exist)
        for message in result["messages"]:
            assert "role" in message
            assert "content" in message
            
            # Check role is valid
            assert message["role"] in ["user", "assistant", "system"]
            
            # Check content structure
            content = message["content"]
            assert "type" in content
            assert "text" in content
            assert content["type"] == "text"
            assert isinstance(content["text"], str)

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_tool_name_validation(self, handlers):
        """Test that tool names are valid identifiers."""
        result = await handlers.handle_list_tools()
        
        for tool in result["tools"]:
            tool_name = tool["name"]
            
            # Tool names should be valid identifiers
            assert tool_name.isidentifier() or "_" in tool_name or "-" in tool_name
            assert not tool_name.startswith("_")
            assert tool_name.islower() or "_" in tool_name
            
            # No spaces or special characters (except underscore and hyphen)
            import re
            assert re.match(r'^[a-z][a-z0-9_-]*$', tool_name), f"Invalid tool name: {tool_name}"

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_json_serialization_compliance(self, handlers):
        """Test that all MCP responses can be JSON serialized."""
        # Test initialize
        init_result = await handlers.handle_initialize({"protocol_version": "2024-11-05"})
        json.dumps(init_result)  # Should not raise exception
        
        # Test list_tools
        tools_result = await handlers.handle_list_tools()
        json.dumps(tools_result)  # Should not raise exception
        
        # Test call_tool success
        call_result = await handlers.handle_call_tool("health_check", {})
        json.dumps(call_result)  # Should not raise exception
        
        # Test call_tool error
        error_result = await handlers.handle_call_tool("unknown_tool", {})
        json.dumps(error_result)  # Should not raise exception
        
        # Test list_prompts
        prompts_result = await handlers.handle_list_prompts()
        json.dumps(prompts_result)  # Should not raise exception
        
        # Test get_prompt
        prompt_result = await handlers.handle_get_prompt("setup_simulation", {})
        json.dumps(prompt_result)  # Should not raise exception

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_schema_validation_compliance(self, handlers):
        """Test that input schemas are valid JSON Schema."""
        result = await handlers.handle_list_tools()
        
        for tool in result["tools"]:
            schema = tool["input_schema"]
            
            # Basic JSON Schema requirements
            assert "type" in schema
            assert schema["type"] == "object"
            
            if "properties" in schema:
                properties = schema["properties"]
                assert isinstance(properties, dict)
                
                for prop_name, prop_schema in properties.items():
                    assert isinstance(prop_schema, dict)
                    assert "type" in prop_schema
                    assert prop_schema["type"] in [
                        "string", "number", "integer", "boolean", "array", "object"
                    ]
                    
                    if "description" in prop_schema:
                        assert isinstance(prop_schema["description"], str)
            
            if "required" in schema:
                required = schema["required"]
                assert isinstance(required, list)
                
                # All required fields should be in properties
                if "properties" in schema:
                    for req_field in required:
                        assert req_field in schema["properties"]

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_error_handling_protocol_compliance(self, handlers):
        """Test that errors are handled according to MCP protocol."""
        # Test with various invalid inputs
        invalid_calls = [
            ("", {}),  # Empty tool name
            ("invalid-tool-name", {}),  # Non-existent tool
            ("run_suews_simulation", {}),  # Missing required parameters
            ("validate_suews_config", {"invalid": "param"}),  # Invalid parameters
        ]
        
        for tool_name, args in invalid_calls:
            result = await handlers.handle_call_tool(tool_name, args)
            
            # All errors should follow protocol
            assert "content" in result
            assert "is_error" in result
            assert result["is_error"] is True
            assert isinstance(result["content"], list)
            assert len(result["content"]) > 0
            
            content = result["content"][0]
            assert content["type"] == "text"
            assert isinstance(content["text"], str)
            assert len(content["text"]) > 0

    @pytest.mark.unit
    def test_server_capabilities_declaration(self, handlers):
        """Test that server correctly declares its capabilities."""
        config = handlers.config
        
        # Server should declare what it supports based on configuration
        expected_tools = set()
        
        if config.enable_simulation_tool:
            expected_tools.add("run_suews_simulation")
        if config.enable_validation_tool:
            expected_tools.add("validate_suews_config")
        if config.enable_analysis_tool:
            expected_tools.add("analyze_suews_output")
        
        # Always available tools
        expected_tools.update([
            "health_check",
            "list_resources", 
            "get_resource",
            "preprocess_forcing",
            "validate_config",
            "convert_data_format"
        ])
        
        # Test that declared tools match configuration
        import asyncio
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            tools_result = loop.run_until_complete(handlers.handle_list_tools())
            actual_tools = {tool["name"] for tool in tools_result["tools"]}
            
            # Check that expected tools are present
            for tool_name in expected_tools:
                assert tool_name in actual_tools, f"Expected tool {tool_name} not found"
        finally:
            loop.close()

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_prompt_argument_validation(self, handlers):
        """Test that prompt arguments follow MCP specification."""
        result = await handlers.handle_list_prompts()
        
        for prompt in result["prompts"]:
            if "arguments" in prompt:
                for arg in prompt["arguments"]:
                    # Required fields
                    assert "name" in arg
                    assert "description" in arg
                    
                    # Field types
                    assert isinstance(arg["name"], str)
                    assert isinstance(arg["description"], str)
                    assert len(arg["name"]) > 0
                    assert len(arg["description"]) > 0
                    
                    # Optional required field
                    if "required" in arg:
                        assert isinstance(arg["required"], bool)

    @pytest.mark.unit
    @pytest.mark.asyncio  
    async def test_content_type_consistency(self, handlers):
        """Test that content types are consistent across responses."""
        # Get various tool responses
        health_result = await handlers.handle_call_tool("health_check", {})
        error_result = await handlers.handle_call_tool("unknown_tool", {})
        
        # All content should use "text" type consistently
        for result in [health_result, error_result]:
            for content_item in result["content"]:
                assert content_item["type"] == "text"
                assert isinstance(content_item["text"], str)
        
        # Prompt responses should also be consistent
        prompt_result = await handlers.handle_get_prompt("setup_simulation", {})
        for message in prompt_result["messages"]:
            content = message["content"]
            assert content["type"] == "text"
            assert isinstance(content["text"], str)

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_protocol_version_handling(self, handlers):
        """Test handling of different protocol versions."""
        # Test with exact version
        result = await handlers.handle_initialize({"protocol_version": "2024-11-05"})
        assert result["protocol_version"] == "2024-11-05"
        
        # Test with different version (should still return supported version)
        result = await handlers.handle_initialize({"protocol_version": "2024-06-01"})
        assert result["protocol_version"] == "2024-11-05"  # Should return what server supports
        
        # Test with missing version
        result = await handlers.handle_initialize({})
        assert result["protocol_version"] == "2024-11-05"  # Should return default


class TestMCPDataStructures:
    """Test MCP data structure compliance."""

    @pytest.mark.unit
    def test_mcp_imports_available(self):
        """Test that MCP types are properly imported when available."""
        if MCP_AVAILABLE:
            from mcp.server.models import (
                InitializeResult,
                Tool, 
                ListToolsResult,
                CallToolResult,
                TextContent,
                ServerCapabilities,
                PromptMessage,
                GetPromptResult,
                ListPromptsResult,
                Prompt,
            )
            # All imports should succeed
            assert InitializeResult is not None
            assert Tool is not None
            assert ListToolsResult is not None
            assert CallToolResult is not None
            assert TextContent is not None
        else:
            # When MCP not available, should use fallback types
            from suews_mcp.handlers import (
                InitializeResult,
                Tool,
                ListToolsResult, 
                CallToolResult,
                TextContent,
            )
            # Fallback types should be dict
            assert InitializeResult == dict
            assert Tool == dict

    @pytest.mark.unit
    @pytest.mark.skipif(not MCP_AVAILABLE, reason="MCP library not available")
    def test_mcp_model_instantiation(self):
        """Test that MCP models can be properly instantiated."""
        from mcp.server.models import (
            Tool,
            TextContent,
            ServerCapabilities,
            Prompt,
        )

        # Test Tool creation
        tool = Tool(
            name="test_tool",
            description="A test tool",
            input_schema={
                "type": "object",
                "properties": {
                    "param": {"type": "string", "description": "A parameter"}
                },
                "required": ["param"]
            }
        )
        assert tool.name == "test_tool"
        assert tool.description == "A test tool"
        assert isinstance(tool.input_schema, dict)

        # Test TextContent creation  
        content = TextContent(type="text", text="Hello, world!")
        assert content.type == "text"
        assert content.text == "Hello, world!"

        # Test ServerCapabilities creation
        capabilities = ServerCapabilities(
            tools={"list_changed": False},
            prompts={"list_changed": False}
        )
        assert capabilities.tools["list_changed"] is False

        # Test Prompt creation
        prompt = Prompt(
            name="test_prompt",
            description="A test prompt"
        )
        assert prompt.name == "test_prompt"
        assert prompt.description == "A test prompt"

    @pytest.mark.unit
    @pytest.mark.asyncio
    async def test_response_structure_consistency(self, handlers):
        """Test that all responses have consistent structure."""
        # Test initialize response structure
        init_response = await handlers.handle_initialize({"protocol_version": "2024-11-05"})
        assert isinstance(init_response, dict)
        required_fields = ["protocol_version", "capabilities", "server_info"]
        for field in required_fields:
            assert field in init_response

        # Test tools response structure
        tools_response = await handlers.handle_list_tools()
        assert isinstance(tools_response, dict)
        assert "tools" in tools_response
        assert isinstance(tools_response["tools"], list)

        # Test tool call response structure
        call_response = await handlers.handle_call_tool("health_check", {})
        assert isinstance(call_response, dict)
        assert "content" in call_response
        assert isinstance(call_response["content"], list)

        # Test prompts response structure
        prompts_response = await handlers.handle_list_prompts()
        assert isinstance(prompts_response, dict)
        assert "prompts" in prompts_response
        assert isinstance(prompts_response["prompts"], list)

        # Test prompt response structure
        prompt_response = await handlers.handle_get_prompt("setup_simulation", {})
        assert isinstance(prompt_response, dict)
        assert "messages" in prompt_response
        assert isinstance(prompt_response["messages"], list)