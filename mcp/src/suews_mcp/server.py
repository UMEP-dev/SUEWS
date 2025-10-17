"""Main MCP server for SUEWS.

This module provides a Model Context Protocol server that enables AI assistants
to interact with SUEWS through natural language.
"""

import sys
from typing import Any, Dict

from mcp.server import Server
from mcp.types import Tool, TextContent

from .tools import configure, simulate, analyze

# Create MCP server instance
app = Server("supy-mcp")


@app.list_tools()
async def list_tools() -> list[Tool]:
    """List available MCP tools for SUEWS."""
    return [
        # Configuration tools
        Tool(
            name="validate_config",
            description="Validate a SUEWS YAML configuration file using the data model",
            inputSchema={
                "type": "object",
                "properties": {
                    "config_path": {
                        "type": "string",
                        "description": "Path to YAML configuration file",
                    }
                },
                "required": ["config_path"],
            },
        ),
        Tool(
            name="create_config",
            description="Create a new SUEWS configuration file",
            inputSchema={
                "type": "object",
                "properties": {
                    "name": {
                        "type": "string",
                        "description": "Configuration name",
                    },
                    "description": {
                        "type": "string",
                        "description": "Configuration description",
                    },
                    "output_path": {
                        "type": "string",
                        "description": "Where to save the configuration",
                    },
                    "template": {
                        "type": "string",
                        "description": "Optional template file to base config on",
                    },
                },
                "required": ["name", "description", "output_path"],
            },
        ),
        Tool(
            name="get_config_info",
            description="Get information about a configuration file",
            inputSchema={
                "type": "object",
                "properties": {
                    "config_path": {
                        "type": "string",
                        "description": "Path to configuration file",
                    }
                },
                "required": ["config_path"],
            },
        ),
        Tool(
            name="update_config",
            description="Update an existing configuration file",
            inputSchema={
                "type": "object",
                "properties": {
                    "config_path": {
                        "type": "string",
                        "description": "Path to configuration file",
                    },
                    "updates": {
                        "type": "object",
                        "description": "Dictionary of updates to apply",
                    },
                },
                "required": ["config_path", "updates"],
            },
        ),
        # Simulation tools
        Tool(
            name="run_simulation",
            description="Run a SUEWS simulation from configuration file",
            inputSchema={
                "type": "object",
                "properties": {
                    "config_path": {
                        "type": "string",
                        "description": "Path to YAML configuration file",
                    },
                    "output_dir": {
                        "type": "string",
                        "description": "Optional output directory for results",
                    },
                },
                "required": ["config_path"],
            },
        ),
        Tool(
            name="estimate_runtime",
            description="Estimate simulation runtime based on configuration",
            inputSchema={
                "type": "object",
                "properties": {
                    "config_path": {
                        "type": "string",
                        "description": "Path to configuration file",
                    }
                },
                "required": ["config_path"],
            },
        ),
        # Analysis tools
        Tool(
            name="load_results",
            description="Load simulation results from file",
            inputSchema={
                "type": "object",
                "properties": {
                    "results_path": {
                        "type": "string",
                        "description": "Path to results file",
                    },
                    "variables": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "Optional list of variables to load",
                    },
                },
                "required": ["results_path"],
            },
        ),
        Tool(
            name="compute_statistics",
            description="Compute statistics on simulation results",
            inputSchema={
                "type": "object",
                "properties": {
                    "results_path": {
                        "type": "string",
                        "description": "Path to results file",
                    },
                    "variables": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "Variables to analyze",
                    },
                    "aggregation": {
                        "type": "string",
                        "enum": ["mean", "sum", "min", "max", "std"],
                        "description": "Aggregation method",
                        "default": "mean",
                    },
                },
                "required": ["results_path", "variables"],
            },
        ),
        Tool(
            name="create_plot",
            description="Create a plot from simulation results",
            inputSchema={
                "type": "object",
                "properties": {
                    "results_path": {
                        "type": "string",
                        "description": "Path to results file",
                    },
                    "variables": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "Variables to plot",
                    },
                    "output_path": {
                        "type": "string",
                        "description": "Where to save plot",
                    },
                    "plot_type": {
                        "type": "string",
                        "enum": ["timeseries", "scatter", "histogram"],
                        "description": "Type of plot",
                        "default": "timeseries",
                    },
                },
                "required": ["results_path", "variables", "output_path"],
            },
        ),
        Tool(
            name="export_results",
            description="Export results to different format",
            inputSchema={
                "type": "object",
                "properties": {
                    "results_path": {
                        "type": "string",
                        "description": "Path to source results",
                    },
                    "output_path": {
                        "type": "string",
                        "description": "Output path",
                    },
                    "format": {
                        "type": "string",
                        "enum": ["csv", "json", "netcdf"],
                        "description": "Output format",
                        "default": "csv",
                    },
                    "variables": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "Optional variable subset",
                    },
                },
                "required": ["results_path", "output_path"],
            },
        ),
    ]


@app.call_tool()
async def call_tool(name: str, arguments: Dict[str, Any]) -> list[TextContent]:
    """Execute an MCP tool."""
    import json

    try:
        # Route to appropriate tool function
        if name == "validate_config":
            result = await configure.validate_config(**arguments)
        elif name == "create_config":
            result = await configure.create_config(**arguments)
        elif name == "get_config_info":
            result = await configure.get_config_info(**arguments)
        elif name == "update_config":
            result = await configure.update_config(**arguments)
        elif name == "run_simulation":
            result = await simulate.run_simulation(**arguments)
        elif name == "estimate_runtime":
            result = await simulate.estimate_runtime(**arguments)
        elif name == "load_results":
            result = await analyze.load_results(**arguments)
        elif name == "compute_statistics":
            result = await analyze.compute_statistics(**arguments)
        elif name == "create_plot":
            result = await analyze.create_plot(**arguments)
        elif name == "export_results":
            result = await analyze.export_results(**arguments)
        else:
            result = {"error": f"Unknown tool: {name}"}

        # Format result as TextContent
        return [TextContent(type="text", text=json.dumps(result, indent=2))]

    except Exception as e:
        error_result = {"error": str(e)}
        return [TextContent(type="text", text=json.dumps(error_result, indent=2))]


async def main_async():
    """Run the MCP server."""
    from mcp.server.stdio import stdio_server

    async with stdio_server() as (read_stream, write_stream):
        await app.run(read_stream, write_stream, app.create_initialization_options())


def main():
    """Main entry point for the MCP server CLI."""
    import asyncio

    try:
        asyncio.run(main_async())
    except KeyboardInterrupt:
        print("\nShutting down SuPy MCP server...", file=sys.stderr)
        sys.exit(0)


if __name__ == "__main__":
    main()
