"""Test configuration for the SUEWS MCP package."""

from __future__ import annotations

from pathlib import Path
import sys

MCP_SRC = Path(__file__).resolve().parents[2] / "mcp" / "src"
if str(MCP_SRC) not in sys.path:
    sys.path.insert(0, str(MCP_SRC))
