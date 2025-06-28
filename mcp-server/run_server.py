#!/usr/bin/env python3
"""Direct runner script for SUEWS MCP server."""

import sys
import os

# Add the src directory to Python path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

# For local testing, also add the parent SuPy
parent_supy = os.path.join(os.path.dirname(__file__), '../../../src')
if os.path.exists(parent_supy):
    sys.path.insert(0, parent_supy)
    print(f"Added parent SuPy to path: {parent_supy}", file=sys.stderr)

# Now import and run the server
try:
    from suews_mcp.server import main
    print("Starting SUEWS MCP server from run_server.py...", file=sys.stderr)
    main()
except Exception as e:
    print(f"Error starting server: {e}", file=sys.stderr)
    import traceback
    traceback.print_exc(file=sys.stderr)
    sys.exit(1)