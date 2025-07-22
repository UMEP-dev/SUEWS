#!/usr/bin/env python3
"""Test SUEWS MCP server with a simple client."""

import asyncio
import json
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


async def test_suews_mcp():
    """Test the SUEWS MCP server."""
    
    # Set up server parameters
    server_params = StdioServerParameters(
        command="python",
        args=["run_server.py"],
        env={}
    )
    
    try:
        # Create client session
        async with stdio_client(server_params) as (read, write):
            async with ClientSession(read, write) as session:
                # Initialize the session
                init_result = await session.initialize()
                print("✓ Connected to SUEWS MCP server")
                if hasattr(init_result, 'server_info'):
                    print(f"  Server: {init_result.server_info.name}")
                    print(f"  Version: {init_result.server_info.version}")
                else:
                    print("  Server info not available")
                
                # List available tools
                result = await session.list_tools()
                print(f"\n✓ Found {len(result.tools)} tools:")
                for tool in result.tools:
                    print(f"  - {tool.name}: {tool.description[:60]}...")
                
                # Test explain_parameter tool
                print("\n✓ Testing explain_parameter tool:")
                result = await session.call_tool(
                    "explain_suews_parameter",
                    arguments={"parameter_name": "tstep", "include_examples": False}
                )
                if result.content:
                    print(f"  Response: {result.content[0].text[:200]}...")
                else:
                    print("  No response received")
                
                # Test template generation
                print("\n✓ Testing template generation:")
                result = await session.call_tool(
                    "generate_suews_template",
                    arguments={
                        "site_type": "suburban",
                        "research_focus": "energy_balance",
                        "include_comments": False
                    }
                )
                if result.content:
                    print(f"  Generated template with {len(result.content[0].text)} characters")
                else:
                    print("  No template generated")
                    
    except Exception as e:
        print(f"\n❌ Error: {type(e).__name__}: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    print("Testing SUEWS MCP Server...\n")
    asyncio.run(test_suews_mcp())