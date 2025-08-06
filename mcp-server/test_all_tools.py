#!/usr/bin/env python3
"""Comprehensive test of all SUEWS MCP tools."""

import asyncio
import json
from pathlib import Path
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


async def test_all_tools():
    """Test all available SUEWS MCP tools."""

    # Create test files
    test_config = Path("test_config.yml")
    test_config.write_text("""
grid:
  frac_paved: 0.3
  frac_buildings: 0.2
  frac_evergreen: 0.1
  frac_deciduous: 0.15
  frac_grass: 0.2
  frac_bare_soil: 0.05
  frac_water: 0.0
  height_mean: 15.0
  population_density_day: 100.0

forcing:
  file_path: "test_forcing.txt"
  
runcontrol:
  start_date: "2024-01-01"
  end_date: "2024-01-31"
  tstep: 300
  
initial_conditions:
  soil_moisture: 0.5
""")

    # Set up server parameters
    server_params = StdioServerParameters(command="python", args=["run_server.py"], env={})

    try:
        async with stdio_client(server_params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                print("✓ Connected to SUEWS MCP server\n")

                # Test 1: validate_suews_config
                print("1. Testing validate_suews_config:")
                try:
                    result = await session.call_tool(
                        "validate_suews_config",
                        arguments={"config_path": str(test_config), "strict": False},
                    )
                    print(f"   ✓ Validation result: {result.content[0].text[:100]}...")
                except Exception as e:
                    print(f"   ✗ Error: {e}")

                # Test 2: explain_suews_parameter
                print("\n2. Testing explain_suews_parameter:")
                try:
                    result = await session.call_tool(
                        "explain_suews_parameter",
                        arguments={
                            "parameter_name": "albedo_deciduous_summer",
                            "include_examples": True,
                        },
                    )
                    print(f"   ✓ Explanation: {result.content[0].text[:100]}...")
                except Exception as e:
                    print(f"   ✗ Error: {e}")

                # Test 3: generate_suews_template
                print("\n3. Testing generate_suews_template:")
                try:
                    result = await session.call_tool(
                        "generate_suews_template",
                        arguments={
                            "site_type": "city_centre",
                            "research_focus": "heat_island",
                            "include_comments": True,
                        },
                    )
                    print(f"   ✓ Generated template: {len(result.content[0].text)} characters")
                except Exception as e:
                    print(f"   ✗ Error: {e}")

                # Test 4: check_suews_physics_compatibility
                print("\n4. Testing check_suews_physics_compatibility:")
                try:
                    result = await session.call_tool(
                        "check_suews_physics_compatibility",
                        arguments={
                            "methods": {
                                "NetRadiationMethod": "LDOWN_AIR",
                                "EmissionsMethod": "J19",
                                "StorageHeatMethod": "OHM_WITH_QF",
                            }
                        },
                    )
                    print(f"   ✓ Compatibility check: {result.content[0].text[:100]}...")
                except Exception as e:
                    print(f"   ✗ Error: {e}")

                # Test 5: suggest_suews_parameters
                print("\n5. Testing suggest_suews_parameters:")
                try:
                    result = await session.call_tool(
                        "suggest_suews_parameters",
                        arguments={
                            "partial_config": str(test_config),
                            "context": "suburban",
                            "climate_zone": "temperate",
                        },
                    )
                    print(f"   ✓ Suggestions: {result.content[0].text[:100]}...")
                except Exception as e:
                    print(f"   ✗ Error: {e}")

                # Summary
                print("\n✓ All basic tool tests completed!")

    except Exception as e:
        print(f"\n❌ Server Error: {type(e).__name__}: {e}")
        import traceback

        traceback.print_exc()
    finally:
        # Clean up test files
        if test_config.exists():
            test_config.unlink()


if __name__ == "__main__":
    print("Testing all SUEWS MCP tools...\n")
    asyncio.run(test_all_tools())
