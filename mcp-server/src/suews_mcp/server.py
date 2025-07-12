"""MCP server for SUEWS parameter explanations with proper error handling."""

import logging
import asyncio
import sys
from typing import Any
from mcp.server import FastMCP
from mcp.server.exceptions import McpError

# Configure logging for debugging (to stderr to avoid interfering with stdio)
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[logging.StreamHandler(sys.stderr)]
)
logger = logging.getLogger(__name__)

# Import tool implementation
try:
    from .tools.parameter_explainer import explain_parameter
except ImportError as e:
    logger.error(f"Failed to import parameter explainer: {e}")
    sys.exit(1)

# Configuration
TOOL_TIMEOUT = 30.0  # seconds
MAX_PARAMETER_LENGTH = 100

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
        
    Raises:
        McpError: If parameter name is invalid
    """
    # Input validation
    if not parameter_name:
        raise McpError("Parameter name cannot be empty")
    
    if not isinstance(parameter_name, str):
        raise McpError("Parameter name must be a string")
    
    if len(parameter_name) > MAX_PARAMETER_LENGTH:
        raise McpError(f"Parameter name too long (max {MAX_PARAMETER_LENGTH} characters)")
    
    # Sanitize input - remove any potentially dangerous characters
    parameter_name = parameter_name.strip()
    
    try:
        # Apply timeout to prevent hanging
        logger.debug(f"Explaining parameter: {parameter_name}")
        result = await asyncio.wait_for(
            explain_parameter(parameter_name, include_examples),
            timeout=TOOL_TIMEOUT
        )
        
        if not result:
            logger.warning(f"Empty result for parameter: {parameter_name}")
            return f"No information available for parameter '{parameter_name}'"
            
        return result
        
    except asyncio.TimeoutError:
        logger.error(f"Timeout explaining parameter: {parameter_name}")
        return f"Operation timed out while explaining '{parameter_name}'. Please try again."
        
    except Exception as e:
        logger.error(f"Error explaining parameter '{parameter_name}': {e}", exc_info=True)
        # Don't expose internal error details to user
        return f"Unable to explain parameter '{parameter_name}'. Please check the parameter name and try again."


# Main entry point
def main():
    """Run the SUEWS parameter explainer MCP server with error handling."""
    try:
        logger.info("SUEWS Assistant MCP server starting...")
        logger.info("Server ready for connections")
        
        # Run with stdio transport
        mcp.run(transport="stdio")
        
    except KeyboardInterrupt:
        logger.info("Server shutdown requested")
        sys.exit(0)
        
    except Exception as e:
        logger.error(f"Server failed to start: {e}", exc_info=True)
        sys.exit(1)
    

if __name__ == "__main__":
    main()