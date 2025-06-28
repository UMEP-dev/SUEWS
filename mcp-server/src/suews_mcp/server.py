"""Main MCP server implementation for SUEWS."""

import logging
from typing import Any, Dict, List, Optional

from mcp.server import FastMCP

from .tools import (
    validate_config,
    suggest_parameters,
    check_physics_compatibility,
    generate_config_template,
    explain_parameter,
    diagnose_energy_balance,
    interpret_thermal_comfort,
    analyze_urban_effects,
    validate_against_observations,
    generate_insights_report,
)

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Create MCP server instance
mcp = FastMCP(
    name="suews-mcp",
    instructions="""
    This MCP server provides intelligent configuration guidance and result interpretation 
    for the Surface Urban Energy and Water balance Scheme (SUEWS).
    
    Available tools help with:
    - Creating scientifically sound configurations
    - Validating parameters
    - Understanding model outputs
    - Detecting common issues
    - Comparing results against typical patterns
    """,
)


# Configuration guidance tools
@mcp.tool(description="Validate a SUEWS configuration file for scientific soundness")
async def validate_suews_config(config_path: str, strict: bool = True) -> str:
    """
    Validate a SUEWS configuration file.

    Args:
        config_path: Path to YAML configuration file
        strict: Whether to enforce all validation rules

    Returns:
        Validation results with errors and suggestions
    """
    return await validate_config(config_path, strict)


@mcp.tool(description="Get parameter suggestions based on urban context and climate")
async def suggest_suews_parameters(
    partial_config: str, context: str = "general", climate_zone: str = "temperate"
) -> str:
    """
    Suggest parameters based on partial configuration and context.

    Args:
        partial_config: Path to partial YAML configuration
        context: Urban context (city_centre, suburban, industrial, etc.)
        climate_zone: Climate classification

    Returns:
        Parameter suggestions with scientific rationale
    """
    return await suggest_parameters(partial_config, context, climate_zone)


@mcp.tool(description="Check compatibility between physics methods")
async def check_suews_physics_compatibility(methods: Dict[str, str]) -> str:
    """
    Check compatibility between selected physics methods.

    Args:
        methods: Dictionary of method selections

    Returns:
        Compatibility analysis and recommendations
    """
    return await check_physics_compatibility(methods)


@mcp.tool(description="Generate a SUEWS configuration template")
async def generate_suews_template(
    site_type: str, research_focus: str, include_comments: bool = True
) -> str:
    """
    Generate a configuration template based on site characteristics.

    Args:
        site_type: Type of urban site
        research_focus: Primary research interest
        include_comments: Whether to include explanatory comments

    Returns:
        YAML configuration template
    """
    return await generate_config_template(site_type, research_focus, include_comments)


@mcp.tool(description="Get detailed explanation of a SUEWS parameter")
async def explain_suews_parameter(parameter_name: str, include_examples: bool = True) -> str:
    """
    Explain a SUEWS parameter with scientific context.

    Args:
        parameter_name: Name of the parameter
        include_examples: Whether to include usage examples

    Returns:
        Parameter explanation with units and typical values
    """
    return await explain_parameter(parameter_name, include_examples)


# Result interpretation tools
@mcp.tool(description="Diagnose energy balance from SUEWS outputs")
async def diagnose_suews_energy_balance(
    output_path: str, site_metadata: Optional[Dict[str, Any]] = None
) -> str:
    """
    Diagnose energy balance closure and identify issues.

    Args:
        output_path: Path to SUEWS output file
        site_metadata: Optional site information

    Returns:
        Energy balance analysis with insights
    """
    return await diagnose_energy_balance(output_path, site_metadata)


@mcp.tool(description="Analyze thermal comfort from SUEWS results")
async def interpret_suews_thermal_comfort(output_path: str, period: str = "full") -> str:
    """
    Analyze thermal comfort from SUEWS outputs.

    Args:
        output_path: Path to SUEWS output file
        period: Analysis period (full, summer, heatwave, etc.)

    Returns:
        Thermal comfort assessment with health implications
    """
    return await interpret_thermal_comfort(output_path, period)


@mcp.tool(description="Analyze urban climate effects like heat island")
async def analyze_suews_urban_effects(
    output_path: str, rural_reference: Optional[str] = None
) -> str:
    """
    Analyze urban climate effects from SUEWS results.

    Args:
        output_path: Path to SUEWS output file
        rural_reference: Optional rural reference data

    Returns:
        Urban heat island analysis and insights
    """
    return await analyze_urban_effects(output_path, rural_reference)


@mcp.tool(description="Validate model results against observations")
async def validate_suews_against_observations(
    model_output: str, observations: str, variables: Optional[List[str]] = None
) -> str:
    """
    Validate model results against observations.

    Args:
        model_output: Path to SUEWS output
        observations: Path to observation data
        variables: Specific variables to validate

    Returns:
        Validation metrics with interpretation
    """
    return await validate_against_observations(model_output, observations, variables)


@mcp.tool(description="Generate comprehensive insights report from SUEWS simulation")
async def generate_suews_insights_report(
    output_path: str, config_path: str, research_context: str
) -> str:
    """
    Generate comprehensive insights report from SUEWS results.

    Args:
        output_path: Path to SUEWS output
        config_path: Path to configuration used
        research_context: Research questions and goals

    Returns:
        Narrative report with key findings
    """
    return await generate_insights_report(output_path, config_path, research_context)


# Main entry point
def main():
    """Run the SUEWS MCP server."""
    import uvicorn

    # Run with stdio transport by default
    mcp.run(transport="stdio")
