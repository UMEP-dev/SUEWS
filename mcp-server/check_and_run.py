#!/usr/bin/env python3
"""Check dependencies and run SUEWS MCP server."""

import sys
import os
import subprocess


def check_dependencies():
    """Check if all dependencies are available."""
    print("Checking dependencies...", file=sys.stderr)
    
    # Check SuPy
    try:
        import supy
        print(f"✓ SuPy {supy.__version__} found", file=sys.stderr)
    except ImportError:
        print("✗ SuPy not installed!", file=sys.stderr)
        print("\nTo install SuPy:", file=sys.stderr)
        print(f"  {sys.executable} -m pip install --user supy==2025.6.2.dev", file=sys.stderr)
        print("\nOr install from GitHub:", file=sys.stderr)
        print(f"  {sys.executable} -m pip install --user git+https://github.com/UMEP-dev/SUEWS.git@master", file=sys.stderr)
        return False
    
    # Check MCP
    try:
        import mcp
        print("✓ MCP found", file=sys.stderr)
    except ImportError:
        print("✗ MCP not installed!", file=sys.stderr)
        print(f"  {sys.executable} -m pip install --user mcp", file=sys.stderr)
        return False
    
    # Check other dependencies
    missing = []
    for dep in ['pydantic', 'pyyaml', 'numpy', 'pandas']:
        try:
            __import__(dep)
        except ImportError:
            missing.append(dep)
    
    if missing:
        print(f"✗ Missing: {', '.join(missing)}", file=sys.stderr)
        print(f"  {sys.executable} -m pip install --user {' '.join(missing)}", file=sys.stderr)
        return False
    
    return True


if __name__ == "__main__":
    # Add src to path
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))
    
    # Check dependencies
    if not check_dependencies():
        print("\nPlease install missing dependencies and restart Claude Desktop.", file=sys.stderr)
        sys.exit(1)
    
    # Run the server
    try:
        from suews_mcp.server import main
        print("Starting SUEWS MCP server...", file=sys.stderr)
        main()
    except Exception as e:
        print(f"Error starting server: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc(file=sys.stderr)
        sys.exit(1)