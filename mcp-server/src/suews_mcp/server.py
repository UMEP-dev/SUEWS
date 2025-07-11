"""Simplified MCP server for SUEWS parameter explanations."""

import logging
from mcp.server import FastMCP
from .tools.parameter_explainer import explain_parameter

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Create MCP server instance
mcp = FastMCP(
    name="suews-assistant",
    instructions="""
    This MCP server provides detailed explanations of SUEWS parameters.
    
    Use the explain_suews_parameter tool to get:
    - Parameter descriptions and units
    - Typical values and ranges
    - Scientific context
    - Usage examples
    - Related parameters
    """,
)


@mcp.tool(description="Get detailed explanation of a SUEWS parameter with typical values and scientific context")
async def explain_suews_parameter(parameter_name: str, include_examples: bool = True) -> str:
    """
    Explain a SUEWS parameter with scientific context.
    
    Args:
        parameter_name: Name of the parameter (e.g., 'tstep', 'albedo', 'lai_max')
        include_examples: Whether to include usage examples
        
    Returns:
        Detailed parameter explanation with units, typical values, and scientific context
    """
    return await explain_parameter(parameter_name, include_examples)


# Main entry point
def main():
    """Run the SUEWS parameter explainer MCP server."""
    import sys
    
    # Debug output
    print("SUEWS Parameter Explainer MCP server starting...", file=sys.stderr)
    
    # Run with stdio transport by default
    mcp.run(transport="stdio")
    

if __name__ == "__main__":
    main()