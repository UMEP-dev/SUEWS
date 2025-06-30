#!/usr/bin/env python3
"""Command-line interface for testing SUEWS MCP tools."""

import asyncio
import json
import sys
from pathlib import Path
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


async def call_tool(tool_name, args):
    """Call a specific MCP tool."""
    server_params = StdioServerParameters(
        command="python",
        args=["run_server.py"],
        env={"PYTHONPATH": "src"}
    )
    
    try:
        async with stdio_client(server_params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                
                # Call the tool
                result = await session.call_tool(tool_name, arguments=args)
                
                # Return the result
                if result.content:
                    return result.content[0].text
                else:
                    return "No response received"
                    
    except Exception as e:
        return f"Error: {type(e).__name__}: {e}"


def main():
    """Main CLI interface."""
    if len(sys.argv) < 2:
        print("SUEWS MCP CLI - Test MCP tools from command line")
        print("\nUsage:")
        print("  python mcp_cli.py list                              # List all tools")
        print("  python mcp_cli.py explain <parameter>               # Explain a parameter")
        print("  python mcp_cli.py validate <config.yml>             # Validate config")
        print("  python mcp_cli.py template <site_type> <focus>      # Generate template")
        print("  python mcp_cli.py check-physics <json>              # Check physics compatibility")
        print("\nExamples:")
        print("  python mcp_cli.py explain albedo_deciduous_summer")
        print("  python mcp_cli.py validate my_config.yml")
        print("  python mcp_cli.py template suburban energy_balance")
        print('  python mcp_cli.py check-physics \'{"NetRadiationMethod":"LDOWN_AIR","EmissionsMethod":"J19"}\'')
        sys.exit(1)
    
    command = sys.argv[1]
    
    if command == "list":
        # List tools
        async def list_tools():
            server_params = StdioServerParameters(
                command="python",
                args=["run_server.py"],
                env={"PYTHONPATH": "src"}
            )
            
            async with stdio_client(server_params) as (read, write):
                async with ClientSession(read, write) as session:
                    await session.initialize()
                    result = await session.list_tools()
                    
                    print("Available SUEWS MCP Tools:")
                    print("-" * 80)
                    for tool in result.tools:
                        print(f"{tool.name:40} {tool.description[:40]}...")
        
        asyncio.run(list_tools())
        
    elif command == "explain" and len(sys.argv) >= 3:
        param = sys.argv[2]
        result = asyncio.run(call_tool("explain_suews_parameter", {
            "parameter_name": param,
            "include_examples": True
        }))
        print(result)
        
    elif command == "validate" and len(sys.argv) >= 3:
        config_path = sys.argv[2]
        result = asyncio.run(call_tool("validate_suews_config", {
            "config_path": config_path,
            "strict": False
        }))
        print(result)
        
    elif command == "template" and len(sys.argv) >= 4:
        site_type = sys.argv[2]
        focus = sys.argv[3]
        result = asyncio.run(call_tool("generate_suews_template", {
            "site_type": site_type,
            "research_focus": focus,
            "include_comments": True
        }))
        print(result)
        
    elif command == "check-physics" and len(sys.argv) >= 3:
        methods = json.loads(sys.argv[2])
        result = asyncio.run(call_tool("check_suews_physics_compatibility", {
            "methods": methods
        }))
        print(result)
        
    else:
        print(f"Unknown command or missing arguments: {command}")
        print("Run without arguments to see usage.")
        sys.exit(1)


if __name__ == "__main__":
    main()