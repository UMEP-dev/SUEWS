#!/usr/bin/env python3
"""Main entry point for SUEWS MCP Server.

This script serves as the primary entry point for the SUEWS Model Context Protocol server.
It can be run directly or used as a module.
"""

import sys
import os

# Add the src directory to Python path to ensure imports work
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "src"))

from suews_mcp.server import main

if __name__ == "__main__":
    main()
