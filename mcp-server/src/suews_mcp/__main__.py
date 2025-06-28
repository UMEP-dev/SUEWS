"""Main entry point for the SUEWS MCP server."""

import sys

# Add debug output
print("Starting SUEWS MCP server...", file=sys.stderr)
print(f"Python version: {sys.version}", file=sys.stderr)
print(f"Python executable: {sys.executable}", file=sys.stderr)

try:
    from .server import main
    main()
except Exception as e:
    print(f"Error starting server: {e}", file=sys.stderr)
    import traceback
    traceback.print_exc(file=sys.stderr)
    sys.exit(1)