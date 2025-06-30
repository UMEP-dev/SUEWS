#!/usr/bin/env python3
"""Test client for SuPy settings MCP server."""

import asyncio
import json
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


async def test_settings_workflow():
    """Test the complete settings workflow."""
    
    server_params = StdioServerParameters(
        command="python",
        args=["server.py"]
    )
    
    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            print("✓ Connected to SuPy Settings MCP server\n")
            
            # Test 1: Create valid settings
            print("1. Creating valid settings...")
            result = await session.call_tool(
                "create_settings",
                arguments={
                    "site_id": "123",
                    "start_date": "2024-01-01",
                    "end_date": "2024-12-31",
                    "timestep": 300,
                    "veg_fraction": 0.25,
                    "water_fraction": 0.05,
                    "paved_fraction": 0.70,
                    "anthrop_heat": 15.0,
                    "met_forcing_file": "era5_london_2024.nc",
                    "lat": 51.5074,
                    "lon": -0.1278
                }
            )
            print(result.content[0].text)
            
            # Extract resource ID from result
            resource_id = None
            for line in result.content[0].text.split('\n'):
                if 'Resource ID' in line and '`' in line:
                    resource_id = line.split('`')[1]
                    break
            
            # Test 2: Invalid settings (fractions don't sum to 1)
            print("\n2. Testing invalid settings (bad fractions)...")
            result = await session.call_tool(
                "create_settings",
                arguments={
                    "site_id": "456",
                    "start_date": "2024-01-01",
                    "end_date": "2024-12-31",
                    "veg_fraction": 0.5,
                    "water_fraction": 0.3,
                    "paved_fraction": 0.5  # Sum = 1.3!
                }
            )
            print(result.content[0].text)
            
            # Test 3: Retrieve settings
            if resource_id:
                print(f"\n3. Retrieving settings {resource_id}...")
                result = await session.call_tool(
                    "get_settings",
                    arguments={"resource_id": resource_id}
                )
                settings = json.loads(result.content[0].text)
                print("Retrieved settings:")
                print(json.dumps(settings, indent=2)[:200] + "...")
            
            # Test 4: List all settings
            print("\n4. Listing all saved settings...")
            result = await session.call_tool("list_settings", arguments={})
            print(result.content[0].text)


async def test_edge_cases():
    """Test edge cases and validation."""
    
    server_params = StdioServerParameters(
        command="python",
        args=["server.py"]
    )
    
    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            
            # Test invalid site ID
            print("\n5. Testing invalid site ID...")
            result = await session.call_tool(
                "create_settings",
                arguments={
                    "site_id": "ABC",  # Should be 3 digits
                    "start_date": "2024-01-01",
                    "end_date": "2024-12-31"
                }
            )
            print(result.content[0].text)
            
            # Test invalid date range
            print("\n6. Testing invalid date range...")
            result = await session.call_tool(
                "create_settings",
                arguments={
                    "site_id": "001",
                    "start_date": "2024-12-31",
                    "end_date": "2024-01-01"  # End before start!
                }
            )
            print(result.content[0].text)


if __name__ == "__main__":
    print("Testing SuPy Settings MCP Server\n")
    print("=" * 50)
    
    # Run main workflow test
    asyncio.run(test_settings_workflow())
    
    # Run edge case tests
    asyncio.run(test_edge_cases())
    
    print("\n✓ All tests completed!")