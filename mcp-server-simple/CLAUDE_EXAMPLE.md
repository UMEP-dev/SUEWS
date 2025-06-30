# Using with Claude Desktop

## Example Prompts

### Basic Settings Creation

```
Create SuPy settings for:
- London site (code 001)
- Full year 2024 simulation
- Urban park: 40% grass, 10% water, 50% paved
- Low anthropogenic heat (10 W/m²)
```

Claude will call:
```json
{
  "tool": "create_settings",
  "arguments": {
    "site_id": "001",
    "start_date": "2024-01-01",
    "end_date": "2024-12-31",
    "veg_fraction": 0.40,
    "water_fraction": 0.10,
    "paved_fraction": 0.50,
    "anthrop_heat": 10.0
  }
}
```

### Validation Examples

```
I want to simulate a site that's 60% buildings and 60% roads. 
Is this valid?
```

Claude will catch the error (fractions > 1.0) before calling the tool.

### Retrieve Settings

```
Get my settings from supy_settings_0001
```

Claude will call:
```json
{
  "tool": "get_settings",
  "arguments": {
    "resource_id": "supy_settings_0001"
  }
}
```

## Integration Benefits

1. **Type Safety**: Claude sees the exact schema from Pydantic
2. **Validation**: Errors are caught and explained clearly
3. **Persistence**: Settings are saved with unique IDs
4. **Simplicity**: Just 3 tools, focused purpose

## Next Steps

1. Add `run_simulation` tool that takes a resource ID
2. Add `validate_forcing` tool for met data checks
3. Create visualization tools for results