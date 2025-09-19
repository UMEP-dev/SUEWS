#!/usr/bin/env python
"""Demo script showing SUEWS MCP server capabilities."""

import asyncio
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parents[1] / "src"))

from suews_mcp.server import (
    validate_suews_config,
    suggest_suews_parameters,
    explain_suews_parameter,
)


async def demo():
    """Demonstrate MCP server functionality."""
    print("🌆 SUEWS MCP Server Demo\n")

    # Demo 1: Validate configuration
    print("1️⃣ Validating example configuration...")
    config_path = Path(__file__).parent / "example_config.yaml"
    result = await validate_suews_config(str(config_path), strict=False)
    print(result[:500] + "...\n" if len(result) > 500 else result + "\n")

    # Demo 2: Get parameter suggestions
    print("2️⃣ Getting parameter suggestions for suburban site...")
    suggestions = await suggest_suews_parameters(
        str(config_path), context="suburban", climate_zone="temperate"
    )
    print(suggestions[:500] + "...\n" if len(suggestions) > 500 else suggestions + "\n")

    # Demo 3: Explain a parameter
    print("3️⃣ Explaining a parameter...")
    explanation = await explain_suews_parameter("height_mean")
    print(explanation + "\n")

    print("✅ Demo complete! The MCP server provides many more tools for:")
    print("   - Physics method compatibility checking")
    print("   - Configuration template generation")
    print("   - Energy balance diagnosis")
    print("   - Thermal comfort analysis")
    print("   - Urban heat island analysis")
    print("   - Model validation against observations")
    print("   - Comprehensive insights reports")


if __name__ == "__main__":
    asyncio.run(demo())
