#!/usr/bin/env python3
"""Entry point for SUEWS Assistant DXT extension."""

import sys
import os
import logging

# Configure logging immediately (before any imports that might use logging)
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    handlers=[logging.StreamHandler(sys.stderr)],
)
logger = logging.getLogger(__name__)

# Add the src directory to Python path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "src"))

# Log Python version and environment for debugging
logger.debug(f"Python version: {sys.version}")
logger.debug(f"Python path: {sys.path}")


# Check for required dependencies before starting
def check_dependencies():
    """Check if required dependencies are available."""
    missing = []

    try:
        import mcp
    except ImportError:
        missing.append("mcp")

    try:
        import pydantic
    except ImportError:
        missing.append("pydantic")

    if missing:
        logger.error(f"Missing required dependencies: {', '.join(missing)}")
        logger.error("Please install with: pip install " + " ".join(missing))
        return False

    return True


# Main execution
if __name__ == "__main__":
    try:
        # Check dependencies
        if not check_dependencies():
            sys.exit(1)

        # Import and run the server
        from suews_mcp.server import main

        logger.info("Starting SUEWS Assistant MCP server...")
        main()

    except ImportError as e:
        logger.error(f"Import error: {e}")
        logger.error("Make sure all dependencies are installed")
        sys.exit(1)

    except Exception as e:
        logger.error(f"Unexpected error: {e}", exc_info=True)
        sys.exit(1)
