"""Main MCP server for SUEWS.

This module provides a Model Context Protocol server that enables AI assistants
to interact with SUEWS through natural language.
"""

import sys
from typing import Any, Dict

from mcp.server import Server
from mcp.types import Tool, TextContent

from .tools import configure, simulate, analyze, knowledge, utilities, forcing

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
            description="Create a new SUEWS configuration file. Uses comprehensive sample_config.yml as base template (2356 lines with all physics parameters). Optional parameters override template defaults for site location.",
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
                        "description": "Optional custom template file (default: uses built-in sample_config.yml)",
                    },
                    "lat": {
                        "type": "number",
                        "description": "Site latitude in decimal degrees (default: from template, 51.5 for London)",
                    },
                    "lon": {
                        "type": "number",
                        "description": "Site longitude in decimal degrees (default: from template, -0.1 for London)",
                    },
                    "alt": {
                        "type": "number",
                        "description": "Site altitude in meters (default: from template, 10.0)",
                    },
                    "timezone": {
                        "type": "integer",
                        "description": "Timezone offset from UTC in hours (default: from template, 0)",
                    },
                    "site_name": {
                        "type": "string",
                        "description": "Name for the default site (default: from template, 'KCL')",
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
        # Knowledge tools - SUEWS domain knowledge
        Tool(
            name="get_config_schema",
            description="Get JSON Schema for SUEWS configuration from Pydantic data model",
            inputSchema={
                "type": "object",
                "properties": {},
            },
        ),
        Tool(
            name="get_config_docs",
            description="Get configuration parameter documentation for SUEWS models (e.g., 'Site', 'OHM', 'SurfaceProperties'). Returns what you can configure, not physics implementation.",
            inputSchema={
                "type": "object",
                "properties": {
                    "config_name": {
                        "type": "string",
                        "description": "Name of configuration model to document",
                    }
                },
                "required": ["config_name"],
            },
        ),
        Tool(
            name="list_available_models",
            description="List all available Pydantic models in SUEWS data model",
            inputSchema={
                "type": "object",
                "properties": {},
            },
        ),
        Tool(
            name="get_variable_info",
            description="Get information about SUEWS output variables (QH, QE, QS, etc.)",
            inputSchema={
                "type": "object",
                "properties": {
                    "variable_name": {
                        "type": "string",
                        "description": "Optional variable name (e.g., 'QH', 'QE'). If not provided, lists all variables.",
                    }
                },
            },
        ),
        Tool(
            name="list_physics_schemes",
            description="List available SUEWS physics schemes with descriptions and source files",
            inputSchema={
                "type": "object",
                "properties": {},
            },
        ),
        Tool(
            name="get_physics_implementation",
            description="Get actual Fortran source code for a physics scheme (OHM, water_balance, etc.)",
            inputSchema={
                "type": "object",
                "properties": {
                    "scheme_name": {
                        "type": "string",
                        "description": "Name of physics scheme",
                    }
                },
                "required": ["scheme_name"],
            },
        ),
        Tool(
            name="get_forcing_format_guide",
            description="Get comprehensive guidance for converting meteorological data to SUEWS forcing format. Returns format specifications, Python conversion templates, unit conversions, and validation scripts.",
            inputSchema={
                "type": "object",
                "properties": {
                    "source_format": {
                        "type": "string",
                        "description": "Optional hint about source data format (e.g., 'csv', 'netcdf', 'weather_station')",
                    },
                    "variables_available": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "Optional list of available variable names in source data",
                    },
                },
            },
        ),
        Tool(
            name="get_era5_forcing",
            description="Retrieve ERA5 reanalysis data and convert to SUEWS forcing format. Downloads meteorological data for any location on Earth (1940-present) using the optimized ERA5 time series API. Requires CDS API credentials.",
            inputSchema={
                "type": "object",
                "properties": {
                    "lat": {
                        "type": "number",
                        "description": "Latitude in decimal degrees (-90 to 90)",
                    },
                    "lon": {
                        "type": "number",
                        "description": "Longitude in decimal degrees (-180 to 180)",
                    },
                    "start_date": {
                        "type": "string",
                        "description": "Start date in YYYY-MM-DD format",
                    },
                    "end_date": {
                        "type": "string",
                        "description": "End date in YYYY-MM-DD format",
                    },
                    "output_path": {
                        "type": "string",
                        "description": "Path where to save the SUEWS forcing file",
                    },
                    "api_key": {
                        "type": "string",
                        "description": "Optional CDS API key (if not configured in ~/.cdsapirc)",
                    },
                },
                "required": ["lat", "lon", "start_date", "end_date", "output_path"],
            },
        ),
        # Utility tools - SUEWS-specific calculations
        Tool(
            name="calculate_ohm_coefficients",
            description="Calculate OHM coefficients from observed storage heat flux and net radiation",
            inputSchema={
                "type": "object",
                "properties": {
                    "results_path": {
                        "type": "string",
                        "description": "Path to results file with QS and QN observations",
                    },
                    "surface_type": {
                        "type": "string",
                        "description": "Optional surface type identifier",
                    },
                },
                "required": ["results_path"],
            },
        ),
        Tool(
            name="calculate_surface_conductance",
            description="Calculate surface conductance for calibrating SUEWS vegetation parameters",
            inputSchema={
                "type": "object",
                "properties": {
                    "results_path": {
                        "type": "string",
                        "description": "Path to results file with meteorological and flux data",
                    },
                    "method": {
                        "type": "string",
                        "enum": ["suews", "observed"],
                        "description": "Calculation method",
                        "default": "suews",
                    },
                },
                "required": ["results_path"],
            },
        ),
        # Data access tools
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
        # Configuration tools
        if name == "validate_config":
            result = await configure.validate_config(**arguments)
        elif name == "create_config":
            result = await configure.create_config(**arguments)
        elif name == "get_config_info":
            result = await configure.get_config_info(**arguments)
        elif name == "update_config":
            result = await configure.update_config(**arguments)
        # Simulation tools
        elif name == "run_simulation":
            result = await simulate.run_simulation(**arguments)
        # Knowledge tools
        elif name == "get_config_schema":
            result = knowledge.get_config_schema(**arguments)
        elif name == "get_config_docs":
            result = knowledge.get_config_docs(**arguments)
        elif name == "list_available_models":
            result = knowledge.list_available_models(**arguments)
        elif name == "get_variable_info":
            result = knowledge.get_variable_info(**arguments)
        elif name == "list_physics_schemes":
            result = knowledge.list_physics_schemes(**arguments)
        elif name == "get_physics_implementation":
            result = knowledge.get_physics_implementation(**arguments)
        elif name == "get_forcing_format_guide":
            result = knowledge.get_forcing_format_guide(**arguments)
        # Forcing data tools
        elif name == "get_era5_forcing":
            result = await forcing.get_era5_forcing(**arguments)
        # Utility tools
        elif name == "calculate_ohm_coefficients":
            result = utilities.calculate_ohm_coefficients(**arguments)
        elif name == "calculate_surface_conductance":
            result = utilities.calculate_surface_conductance(**arguments)
        # Data access tools
        elif name == "load_results":
            result = await analyze.load_results(**arguments)
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
