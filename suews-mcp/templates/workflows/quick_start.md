# Quick Start: Running Your First SUEWS Simulation

This workflow guides you through setting up and running a basic SUEWS urban climate simulation using the MCP server tools.

## Prerequisites

- SUEWS MCP server running
- Meteorological forcing data
- Basic understanding of your study area characteristics

## Step 1: Choose a Configuration Template

Select an appropriate configuration template based on your urban area type:

```python
# Using MCP tools to get available templates
templates = await client.call_tool("list_resources", {
    "resource_type": "config_template"
})
```

Available templates:
- **residential.yml** - Mixed residential areas (25% paved, 35% buildings, 30% grass)
- **commercial.yml** - Dense downtown areas (40% paved, 55% buildings, minimal vegetation)  
- **industrial.yml** - Industrial zones (50% paved, 35% buildings, minimal vegetation)
- **park.yml** - Urban parks/green spaces (45% grass, 35% trees, minimal built area)

## Step 2: Customize Your Configuration

```python
# Get a template configuration
config_template = await client.call_tool("get_resource", {
    "resource_path": "templates/configs/residential.yml"
})

# Key parameters to customise for your site:
# - lat/lng: Your study area coordinates
# - start_time/end_time: Simulation period
# - forcing_file: Path to your meteorological data
# - land_cover.*.sfr: Surface fraction values that sum to 1.0
```

### Critical Parameters to Check:

1. **Site Properties**:
   ```yaml
   lat: 51.5      # Your latitude
   lng: -0.1      # Your longitude
   alt: 50.0      # Elevation above sea level (metres)
   timezone: 0    # UTC offset
   ```

2. **Surface Fractions** (must sum to 1.0):
   ```yaml
   land_cover:
     paved:
       sfr: 0.25    # Fraction of paved surfaces
     bldgs:
       sfr: 0.35    # Fraction of buildings
     grass:
       sfr: 0.30    # Fraction of grass
     # ... other surface types
   ```

3. **Forcing Data**:
   ```yaml
   forcing_file:
     value: "your_forcing_data.txt"  # Path to your meteorological data
   ```

## Step 3: Validate Your Configuration

```python
# Validate configuration before running simulation
validation_result = await client.call_tool("validate_suews_config", {
    "config_file": "your_modified_config.yml",
    "strict": True
})
```

Common validation errors and fixes:
- **Surface fractions don't sum to 1.0**: Adjust values to total exactly 1.0
- **Missing forcing file**: Ensure meteorological data file exists and is accessible
- **Invalid dates**: Check start_time and end_time format (YYYY-MM-DD)

## Step 4: Run the Simulation

```python
# Run SUEWS simulation
simulation_result = await client.call_tool("run_suews_simulation", {
    "config_file": "your_modified_config.yml",
    "simulation_id": "my_first_simulation",
    "output_dir": "./outputs/"
})
```

## Step 5: Analyse Results

```python
# Analyse simulation outputs
analysis = await client.call_tool("analyze_suews_output", {
    "output_file": "./outputs/simulation_output.txt",
    "metrics": ["QH", "QE", "QN", "T2"],
    "time_period": "monthly"
})
```

### Key Output Variables:
- **QH**: Sensible heat flux (W/m²)
- **QE**: Latent heat flux (W/m²) 
- **QN**: Net all-wave radiation (W/m²)
- **QS**: Storage heat flux (W/m²)
- **T2**: 2m air temperature (°C)
- **RH2**: 2m relative humidity (%)

## Troubleshooting

### Simulation Fails to Start
1. Check configuration validation results
2. Verify forcing data file exists and has correct format
3. Ensure surface fractions sum to 1.0

### Unexpected Results
1. Compare with expected seasonal patterns
2. Check anthropogenic heat emissions are reasonable for your area type
3. Verify initial conditions are appropriate for your climate

### Performance Issues
1. Consider reducing simulation period for testing
2. Increase output frequency for shorter debugging runs
3. Check available system resources

## Example Complete Workflow

```python
# Complete workflow example
async def run_quick_simulation():
    # 1. Get template
    template = await client.call_tool("get_resource", {
        "resource_path": "templates/configs/residential.yml"
    })
    
    # 2. Modify template (save as new file)
    # Edit lat, lng, dates, forcing_file in template
    
    # 3. Validate
    validation = await client.call_tool("validate_suews_config", {
        "config_file": "my_config.yml"
    })
    
    # 4. Run simulation
    if "valid" in validation.content[0].text.lower():
        result = await client.call_tool("run_suews_simulation", {
            "config_file": "my_config.yml",
            "simulation_id": "test_run"
        })
        
        # 5. Analyse
        analysis = await client.call_tool("analyze_suews_output", {
            "output_file": "test_run_output.txt",
            "metrics": ["QH", "QE", "T2"]
        })
        
        return analysis
```

## Next Steps

- Try the [Sensitivity Analysis Workflow](sensitivity_analysis.md) to understand parameter impacts
- Follow the [Validation Workflow](validation_workflow.md) to compare with observations
- Explore advanced configuration options in the SuPy documentation

## Support Resources

- [SuPy Documentation](https://supy.readthedocs.io/)
- [SUEWS Manual](https://suews.readthedocs.io/)
- [Sample Data](data_sources.md)