#!/usr/bin/env python3
"""Simple CLI for testing SuPy settings server."""

import asyncio
import json
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


async def create_settings_cli():
    """Interactive CLI for creating settings."""
    
    print("\n🏙️  SuPy Settings Creator")
    print("=" * 40)
    
    # Get input from user
    site_id = input("Site ID (3 digits, e.g. 001): ").strip() or "001"
    start_date = input("Start date (YYYY-MM-DD): ").strip() or "2024-01-01"
    end_date = input("End date (YYYY-MM-DD): ").strip() or "2024-12-31"
    
    print("\nLand cover fractions (must sum to 1.0):")
    veg = float(input("  Vegetation fraction [0.25]: ").strip() or "0.25")
    water = float(input("  Water fraction [0.05]: ").strip() or "0.05") 
    paved = float(input("  Paved fraction [0.70]: ").strip() or "0.70")
    
    anthrop_heat = float(input("\nAnthropogenic heat (W/m²) [20]: ").strip() or "20")
    
    # Connect and create
    server_params = StdioServerParameters(
        command="python",
        args=["server.py"]
    )
    
    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            
            result = await session.call_tool(
                "create_settings",
                arguments={
                    "site_id": site_id,
                    "start_date": start_date,
                    "end_date": end_date,
                    "veg_fraction": veg,
                    "water_fraction": water,
                    "paved_fraction": paved,
                    "anthrop_heat": anthrop_heat,
                    "met_forcing_file": "default_forcing.nc"
                }
            )
            
            print("\nResult:")
            print(result.content[0].text)


if __name__ == "__main__":
    asyncio.run(create_settings_cli())