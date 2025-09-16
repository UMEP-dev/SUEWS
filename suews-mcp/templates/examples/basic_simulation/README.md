# Basic SUEWS Simulation Example

This is a minimal working example demonstrating how to run a SUEWS urban climate simulation using the MCP server tools.

## Overview

This example shows:
- Loading sample data with built-in meteorological forcing
- Running a one-year urban climate simulation
- Analysing basic energy and water balance outputs
- Saving results for further analysis

## Files in This Example

- `run_simulation.py` - Complete Python script using MCP tools
- `config_basic.yml` - Basic configuration for residential area
- `analysis_output.py` - Script to analyse simulation results
- `README.md` - This documentation

## Quick Start

### 1. Using MCP Tools Directly

```python
import asyncio
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def run_basic_simulation():
    # Connect to SUEWS MCP server
    server_params = StdioServerParameters(
        command="python", 
        args=["-m", "suews_mcp.server"]
    )
    
    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            # Initialize MCP session
            await session.initialize()
            
            # Get residential template
            template_result = await session.call_tool(
                "get_resource", 
                {"resource_path": "templates/configs/residential.yml"}
            )
            print("✅ Template loaded")
            
            # Run simulation with sample data
            sim_result = await session.call_tool(
                "run_suews_simulation",
                {
                    "config_data": template_result.content[0].text,
                    "use_sample_data": True,
                    "simulation_id": "basic_example",
                    "duration_months": 12
                }
            )
            print("✅ Simulation completed")
            
            # Analyse results
            analysis_result = await session.call_tool(
                "analyze_suews_output",
                {
                    "simulation_id": "basic_example",
                    "metrics": ["QH", "QE", "QN", "T2", "RH2"],
                    "time_aggregation": "monthly"
                }
            )
            print("✅ Analysis completed")
            
            return analysis_result

# Run the example
if __name__ == "__main__":
    result = asyncio.run(run_basic_simulation())
    print(result.content[0].text)
```

### 2. Using SuPy Directly (Without MCP)

```python
import supy as sp
import matplotlib.pyplot as plt

# Load sample data - no configuration needed!
df_state_init, df_forcing = sp.load_sample_data()

# Use one year of data for demonstration
df_forcing_year = df_forcing.loc["2012"].iloc[1:]
grid = df_state_init.index[0]

# Run SUEWS simulation
print("🔄 Running SUEWS simulation...")
df_output, df_state_final = sp.run_supy(df_forcing_year, df_state_init)
print("✅ Simulation completed successfully!")

# Quick analysis - monthly energy balance
monthly_data = df_output["SUEWS"].loc[grid].resample("1M").mean()
energy_vars = ["QN", "QH", "QE", "QS", "QF"]

# Create summary plot
fig, ax = plt.subplots(figsize=(10, 6))
monthly_data[energy_vars].plot(ax=ax, kind="bar")
ax.set_title("Monthly Average Surface Energy Balance")
ax.set_ylabel("Flux (W/m²)")
ax.set_xlabel("Month")
ax.legend()
plt.xticks(rotation=45)
plt.tight_layout()
plt.savefig("energy_balance.png")
print("📊 Results plotted to energy_balance.png")

# Save results
output_files = sp.save_supy(df_output, df_state_final)
print(f"💾 Results saved: {len(output_files)} files created")
```

## Expected Results

After running this example, you should see:
- **QN (Net Radiation)**: Seasonal pattern with summer peak ~200 W/m²
- **QH (Sensible Heat)**: Positive values, highest in summer
- **QE (Latent Heat)**: Lower in winter, higher in growing season
- **QF (Anthropogenic Heat)**: Relatively constant year-round
- **T2 (Air Temperature)**: Clear seasonal cycle

## Key Learning Points

1. **Sample Data**: SuPy provides built-in sample data for immediate testing
2. **Simple Workflow**: Load data → Run simulation → Analyse results
3. **Energy Balance**: SUEWS models all components of urban energy budget
4. **Seasonal Patterns**: Urban areas show distinct seasonal energy patterns
5. **Data Integration**: Results integrate seamlessly with pandas/matplotlib

## Customisation Options

To adapt this example for your research:

### Change Location
```python
# Modify coordinates in template
config_data["lat"] = 40.7128  # New York latitude
config_data["lng"] = -74.0060  # New York longitude
config_data["alt"] = 10.0     # Elevation (metres)
config_data["timezone"] = -5   # UTC offset
```

### Adjust Urban Characteristics
```python
# Modify surface fractions (must sum to 1.0)
config_data["land_cover"]["paved"]["sfr"] = 0.4
config_data["land_cover"]["bldgs"]["sfr"] = 0.4  
config_data["land_cover"]["grass"]["sfr"] = 0.2
```

### Use Your Own Meteorological Data
```python
# Load your forcing data
df_forcing = sp.load_forcing_data("your_met_data.txt")
```

## Troubleshooting

**Common Issues:**

1. **"Surface fractions don't sum to 1.0"**
   - Check that all `sfr` values in `land_cover` sum exactly to 1.0

2. **"Forcing data format error"**
   - Ensure your meteorological data follows SuPy format
   - Use sample data first to test setup

3. **"Simulation fails to start"**
   - Validate configuration with MCP validation tools
   - Check that all required parameters are present

**Getting Help:**
- See [SuPy Documentation](https://supy.readthedocs.io/)
- Check [SUEWS Troubleshooting Guide](https://suews.readthedocs.io/en/latest/troubleshooting.html)
- Try the [Quick Start Tutorial](https://suews.readthedocs.io/en/latest/tutorials/python/quick-start.html)

## Next Steps

After completing this basic example:
1. Try the [Urban Park Study](../urban_park_study/) for green infrastructure analysis
2. Explore [Building Energy](../building_energy/) example for energy modelling
3. Learn about [Multi-site Analysis](../multi_site_analysis/) for comparative studies
4. Follow the [Sensitivity Analysis Workflow](../../workflows/sensitivity_analysis.md)