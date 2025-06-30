# SuPy Settings MCP Server (Simple Demo)

A minimal MCP server for validating and storing SuPy simulation settings. No simulation execution - just validate configuration and return a resource ID.

## What it does

1. **Validates** SuPy settings (dates, fractions, ranges)
2. **Stores** validated settings as JSON
3. **Returns** a resource ID for later use

## Quick Start

```bash
# Install
pip install -e .

# Run server
python server.py

# Test it
python test_client.py
```

## Core Components

### `settings.py` - Pydantic Models
- `SuPySettings`: Main configuration model
- `UrbanFraction`: Land cover fractions (must sum to 1.0)
- Built-in validation for ranges and cross-field logic

### `server.py` - MCP Tools
- `create_settings`: Validate and save settings
- `get_settings`: Retrieve by resource ID  
- `list_settings`: Show all saved configurations

## Example Usage

```python
# Valid settings
{
    "site_id": "123",
    "start_date": "2024-01-01", 
    "end_date": "2024-12-31",
    "timestep": 300,
    "veg_fraction": 0.25,
    "water_fraction": 0.05,
    "paved_fraction": 0.70,
    "anthrop_heat": 15.0,
    "met_forcing_file": "era5_london_2024.nc"
}
# Returns: "supy_settings_0001"
```

## Validation Rules

- **site_id**: 3-digit string (e.g., "001")
- **dates**: ISO format, end > start
- **timestep**: 60-3600 seconds
- **fractions**: 0-1, must sum to 1.0 (±0.01)
- **anthrop_heat**: 0-800 W/m²

## Storage

Settings are saved to:
- Memory (during session)
- `/tmp/supy_mcp_settings/` (JSON files)

## Next Steps

1. Add more SuPy parameters as needed
2. Connect to actual simulation runner
3. Add forcing file validation
4. Implement proper resource management