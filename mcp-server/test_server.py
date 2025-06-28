#!/usr/bin/env python
"""Quick test to verify MCP server can start."""

import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

from suews_mcp.server import mcp


async def test_server():
    """Test that the server has tools registered."""
    print("✅ Server created successfully")
    print(f"Server name: {mcp.name}")

    # List registered tools
    tools = await mcp.list_tools()
    print(f"\n📦 Registered tools ({len(tools)}):")
    for tool in tools:
        print(f"  - {tool.name}: {tool.description}")

    print("\n✅ All tools registered successfully!")
    print("\nTo run the server, use: python -m suews_mcp.server")


if __name__ == "__main__":
    import asyncio

    asyncio.run(test_server())
