#!/usr/bin/env python3
"""Test MCP server locally using the MCP test harness."""

import asyncio
import json
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def test_mcp():
    """Test the MCP server with basic requests."""
    
    # Set up server parameters
    server_params = StdioServerParameters(
        command="python",
        args=["run_server.py"],
        env={"PYTHONPATH": "src"}
    )
    
    # Create client session
    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            # Initialize the session
            await session.initialize()
            print("✓ MCP server initialized")
            
            # List available tools
            result = await session.list_tools()
            print(f"\n✓ Found {len(result.tools)} tools:")
            for tool in result.tools:
                print(f"  - {tool.name}: {tool.description}")
            
            # List available prompts
            result = await session.list_prompts()
            print(f"\n✓ Found {len(result.prompts)} prompts:")
            for prompt in result.prompts:
                print(f"  - {prompt.name}: {prompt.description}")
            
            # Test a simple tool (explain_parameter)
            print("\n✓ Testing explain_parameter tool:")
            result = await session.call_tool(
                "explain_suews_parameter",
                arguments={"parameter_name": "tstep", "include_examples": False}
            )
            print(f"  Response: {result.content[0].text[:100]}...")

if __name__ == "__main__":
    print("Testing SUEWS MCP Server locally...\n")
    asyncio.run(test_mcp())