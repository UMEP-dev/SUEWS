#!/usr/bin/env python3
"""Simple MCP server for SuPy settings validation and storage."""

import json
import tempfile
from pathlib import Path
from datetime import datetime
from typing import Dict, Any

from mcp.server import FastMCP
from settings import SuPySettings


# Initialize MCP server
mcp = FastMCP(
    name="supy-settings",
    instructions="""
    Simple MCP server for validating and storing SuPy simulation settings.
    No simulation execution - just validate configuration and return resource ID.
    """
)

# In-memory resource store (for demo purposes)
resource_store: Dict[str, Dict[str, Any]] = {}
resource_counter = 0


@mcp.tool(
    description="Validate and save SuPy run-control settings"
)
async def create_settings(
    site_id: str,
    start_date: str,
    end_date: str,
    timestep: int = 300,
    veg_fraction: float = 0.25,
    water_fraction: float = 0.05,
    paved_fraction: float = 0.70,
    anthrop_heat: float = 20.0,
    met_forcing_file: str = "default_forcing.nc",
    lat: float = None,
    lon: float = None
) -> str:
    """
    Create and validate SuPy settings.
    
    Args:
        site_id: Three-digit site code (e.g., "001")
        start_date: Simulation start date (YYYY-MM-DD)
        end_date: Simulation end date (YYYY-MM-DD)
        timestep: Time step in seconds (60-3600)
        veg_fraction: Vegetation fraction (0-1)
        water_fraction: Water fraction (0-1)
        paved_fraction: Paved fraction (0-1)
        anthrop_heat: Anthropogenic heat flux (W/m²)
        met_forcing_file: Path or resource ID for meteorological forcing
        lat: Latitude (optional)
        lon: Longitude (optional)
    
    Returns:
        Resource ID for the saved settings
    """
    try:
        # Build settings object
        settings = SuPySettings(
            site_id=site_id,
            start_date=start_date,
            end_date=end_date,
            timestep=timestep,
            urban_frac={
                "veg": veg_fraction,
                "water": water_fraction,
                "paved": paved_fraction
            },
            anthrop_heat=anthrop_heat,
            met_forcing_file=met_forcing_file,
            lat=lat,
            lon=lon
        )
        
        # Validate fractions sum to 1
        settings.validate_fractions()
        
        # Add SuPy version if available
        try:
            import supy
            settings.supy_version = supy.__version__
        except ImportError:
            settings.supy_version = "not_installed"
        
        # Generate resource ID
        global resource_counter
        resource_counter += 1
        resource_id = f"supy_settings_{resource_counter:04d}"
        
        # Store in memory (in production, save to file/database)
        resource_store[resource_id] = {
            "settings": settings.model_dump(),
            "created_at": datetime.now().isoformat(),
            "json_str": settings.model_dump_json(indent=2)
        }
        
        # Also save to temp file for persistence
        tmp_dir = Path(tempfile.gettempdir()) / "supy_mcp_settings"
        tmp_dir.mkdir(exist_ok=True)
        
        settings_file = tmp_dir / f"{resource_id}.json"
        settings_file.write_text(settings.model_dump_json(indent=2))
        
        return f"""✅ Settings validated and saved!

**Resource ID**: `{resource_id}`
**Location**: `{settings_file}`

**Summary**:
- Site: {site_id}
- Period: {start_date} to {end_date}
- Time step: {timestep}s
- Land cover: {veg_fraction:.0%} veg, {water_fraction:.0%} water, {paved_fraction:.0%} paved
- Anthropogenic heat: {anthrop_heat} W/m²

Use this resource ID to load settings for simulation."""
        
    except Exception as e:
        return f"❌ Validation failed: {str(e)}"


@mcp.tool(
    description="Retrieve previously saved SuPy settings"
)
async def get_settings(resource_id: str) -> str:
    """
    Retrieve saved settings by resource ID.
    
    Args:
        resource_id: The resource ID returned by create_settings
    
    Returns:
        JSON string of the settings or error message
    """
    if resource_id in resource_store:
        return resource_store[resource_id]["json_str"]
    
    # Try to load from file
    tmp_dir = Path(tempfile.gettempdir()) / "supy_mcp_settings"
    settings_file = tmp_dir / f"{resource_id}.json"
    
    if settings_file.exists():
        return settings_file.read_text()
    
    return f"❌ Settings not found for resource ID: {resource_id}"


@mcp.tool(
    description="List all saved SuPy settings"
)
async def list_settings() -> str:
    """List all available settings resources."""
    # Check temp directory
    tmp_dir = Path(tempfile.gettempdir()) / "supy_mcp_settings"
    
    if not tmp_dir.exists():
        return "No settings saved yet."
    
    settings_files = list(tmp_dir.glob("supy_settings_*.json"))
    
    if not settings_files:
        return "No settings saved yet."
    
    result = "📋 **Saved Settings**\n\n"
    
    for f in sorted(settings_files):
        try:
            data = json.loads(f.read_text())
            result += f"- `{f.stem}`: Site {data['site_id']}, "
            result += f"{data['start_date']} to {data['end_date']}\n"
        except:
            result += f"- `{f.stem}`: (error reading)\n"
    
    return result


def main():
    """Run the MCP server."""
    import sys
    print("Starting SuPy Settings MCP server...", file=sys.stderr)
    mcp.run(transport="stdio")


if __name__ == "__main__":
    main()