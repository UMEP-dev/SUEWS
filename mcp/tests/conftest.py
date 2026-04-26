"""Test configuration for the standalone SUEWS MCP package."""

from __future__ import annotations

from pathlib import Path
import sys

PACKAGE_SRC = Path(__file__).resolve().parents[1] / "src"
sys.path.insert(0, str(PACKAGE_SRC))
