# Urban Park Study Example

This example demonstrates how to use SUEWS to analyse urban green infrastructure impacts, focusing on how parks and green spaces affect the urban energy and water balance.

## Overview

This example shows:
- Configuring SUEWS for park/green space areas
- Comparing park vs built-area energy balances
- Quantifying cooling benefits of urban vegetation
- Analysing water balance in green infrastructure
- Assessing ecosystem services provided by urban parks

## Research Questions Addressed

1. **How much cooling do urban parks provide?**
2. **What's the evapotranspiration rate from urban vegetation?**
3. **How do parks affect surrounding areas?**
4. **What's the water demand for maintaining urban green spaces?**

## Files in This Example

- `park_simulation.py` - Main simulation script
- `park_config.yml` - Park-specific SUEWS configuration  
- `compare_scenarios.py` - Compare park vs built scenarios
- `cooling_analysis.py` - Quantify cooling benefits
- `README.md` - This documentation

## Quick Start

### Using MCP Tools

```python
import asyncio
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def run_park_study():
    server_params = StdioServerParameters(
        command="python", 
        args=["-m", "suews_mcp.server"]
    )
    
    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            
            # Get park template
            park_template = await session.call_tool(
                "get_resource",
                {"resource_path": "templates/configs/park.yml"}
            )
            
            # Run park simulation
            park_result = await session.call_tool(
                "run_suews_simulation",
                {
                    "config_data": park_template.content[0].text,
                    "use_sample_data": True,
                    "simulation_id": "urban_park",
                    "duration_months": 12
                }
            )
            
            # For comparison, run built area simulation
            built_template = await session.call_tool(
                "get_resource", 
                {"resource_path": "templates/configs/commercial.yml"}
            )
            
            built_result = await session.call_tool(
                "run_suews_simulation",
                {
                    "config_data": built_template.content[0].text,
                    "use_sample_data": True, 
                    "simulation_id": "built_area",
                    "duration_months": 12
                }
            )
            
            # Compare results
            comparison = await session.call_tool(
                "compare_simulations",
                {
                    "simulation_ids": ["urban_park", "built_area"],
                    "metrics": ["QH", "QE", "T2", "RH2"],
                    "analysis_type": "cooling_benefits"
                }
            )
            
            return comparison

# Run the study
result = asyncio.run(run_park_study())
print(result.content[0].text)
```

### Using SuPy Directly

```python
import supy as sp
import matplotlib.pyplot as plt
import pandas as pd

# Load sample data
df_state_init, df_forcing = sp.load_sample_data()
df_forcing_year = df_forcing.loc["2012"].iloc[1:]
grid = df_state_init.index[0]

# Configure for park scenario (high vegetation)
df_state_park = df_state_init.copy()
df_state_park.loc[grid, "sfr_surf"] = [0.05, 0.05, 0.45, 0.35, 0.10, 0.00, 0.00]  # Low built, high veg

# Configure for built scenario (low vegetation)  
df_state_built = df_state_init.copy()
df_state_built.loc[grid, "sfr_surf"] = [0.45, 0.50, 0.03, 0.01, 0.01, 0.00, 0.00]  # High built, low veg

print("🌳 Running park simulation...")
df_output_park, _ = sp.run_supy(df_forcing_year, df_state_park)

print("🏢 Running built area simulation...")
df_output_built, _ = sp.run_supy(df_forcing_year, df_state_built)

# Extract SUEWS outputs for comparison
park_data = df_output_park["SUEWS"].loc[grid]
built_data = df_output_built["SUEWS"].loc[grid]

# Calculate cooling effect (temperature difference)
temperature_diff = built_data["T2"] - park_data["T2"]
print(f"📊 Average cooling effect: {temperature_diff.mean():.2f}°C")
print(f"📊 Maximum cooling effect: {temperature_diff.max():.2f}°C")

# Compare energy fluxes
energy_comparison = pd.DataFrame({
    "Park_QH": park_data["QH"],
    "Built_QH": built_data["QH"], 
    "Park_QE": park_data["QE"],
    "Built_QE": built_data["QE"],
    "Park_T2": park_data["T2"],
    "Built_T2": built_data["T2"]
})

# Monthly analysis
monthly_comparison = energy_comparison.resample("1M").mean()

# Visualise results
fig, axes = plt.subplots(2, 2, figsize=(12, 10))

# Temperature comparison
axes[0,0].plot(monthly_comparison.index, monthly_comparison["Park_T2"], label="Park", color="green")
axes[0,0].plot(monthly_comparison.index, monthly_comparison["Built_T2"], label="Built", color="red")
axes[0,0].set_title("Air Temperature Comparison")
axes[0,0].set_ylabel("Temperature (°C)")
axes[0,0].legend()

# Sensible heat flux
axes[0,1].plot(monthly_comparison.index, monthly_comparison["Park_QH"], label="Park", color="green") 
axes[0,1].plot(monthly_comparison.index, monthly_comparison["Built_QH"], label="Built", color="red")
axes[0,1].set_title("Sensible Heat Flux")
axes[0,1].set_ylabel("QH (W/m²)")
axes[0,1].legend()

# Latent heat flux
axes[1,0].plot(monthly_comparison.index, monthly_comparison["Park_QE"], label="Park", color="green")
axes[1,0].plot(monthly_comparison.index, monthly_comparison["Built_QE"], label="Built", color="red")
axes[1,0].set_title("Latent Heat Flux (Evapotranspiration)")
axes[1,0].set_ylabel("QE (W/m²)")
axes[1,0].legend()

# Cooling effect over time
axes[1,1].plot(temperature_diff.resample("1M").mean(), color="blue", linewidth=2)
axes[1,1].set_title("Park Cooling Effect")
axes[1,1].set_ylabel("Temperature Reduction (°C)")
axes[1,1].axhline(y=0, color="black", linestyle="--", alpha=0.3)

plt.tight_layout()
plt.savefig("park_cooling_analysis.png", dpi=150, bbox_inches="tight")
print("📈 Analysis plot saved to park_cooling_analysis.png")
```

## Key Results to Expect

### Temperature Effects
- **Average cooling**: 1-3°C during daytime in summer
- **Maximum cooling**: Up to 5°C during heat waves  
- **Nighttime effects**: Less pronounced, 0.5-1°C
- **Seasonal variation**: Strongest cooling in growing season

### Energy Balance Changes
- **Sensible heat (QH)**: 30-50% lower in parks vs built areas
- **Latent heat (QE)**: 200-400% higher in parks (evapotranspiration)
- **Storage heat (QS)**: Much lower due to less thermal mass
- **Net radiation**: Slightly lower due to vegetation shading

### Water Balance
- **Evapotranspiration**: 300-600 mm/year from healthy urban vegetation
- **Irrigation needs**: Peak demand in summer (May-September)
- **Runoff reduction**: Parks reduce surface runoff by 40-80%

## Research Applications

### Urban Heat Island Studies
```python
# Calculate UHI intensity reduction
uhi_reduction = temperature_diff.groupby(temperature_diff.index.hour).mean()

# Peak afternoon cooling (typical UHI max time)
afternoon_cooling = uhi_reduction.loc[14:16].mean()
print(f"Peak afternoon UHI reduction: {afternoon_cooling:.2f}°C")
```

### Ecosystem Services Valuation
```python
# Calculate annual evapotranspiration
annual_et = park_data["QE"].sum() * 3600 * 5 / (2.45e6)  # Convert to mm/year
print(f"Annual evapotranspiration: {annual_et:.1f} mm/year")

# Cooling energy savings (simplified)
cooling_degree_hours = (temperature_diff * (temperature_diff > 0)).sum()
energy_savings = cooling_degree_hours * 0.1  # kWh/m²/year (rough estimate)
print(f"Estimated cooling energy savings: {energy_savings:.1f} kWh/m²/year")
```

### Green Infrastructure Design
```python
# Test different vegetation scenarios
vegetation_fractions = [0.2, 0.4, 0.6, 0.8]
cooling_effects = []

for veg_fraction in vegetation_fractions:
    # Modify vegetation fraction and run simulation
    # (simulation code here)
    cooling_effects.append(temperature_diff.mean())

# Plot vegetation fraction vs cooling effect
plt.plot(vegetation_fractions, cooling_effects, 'o-')
plt.xlabel("Vegetation Fraction")
plt.ylabel("Average Cooling Effect (°C)")
plt.title("Cooling Benefits vs Vegetation Coverage")
plt.savefig("vegetation_cooling_curve.png")
```

## Advanced Analysis Options

### Diurnal Cooling Patterns
```python
# Analyse cooling effect by time of day
hourly_cooling = temperature_diff.groupby(temperature_diff.index.hour).mean()

# Peak cooling time
peak_hour = hourly_cooling.idxmax()
peak_cooling = hourly_cooling.max()

print(f"Peak cooling occurs at {peak_hour}:00 with {peak_cooling:.2f}°C reduction")
```

### Seasonal Ecosystem Services
```python
# Monthly evapotranspiration from parks
monthly_et = park_data["QE"].resample("1M").sum() * 3600 * 24 * 30.4 / (2.45e6)

# Growing season analysis
growing_season = monthly_et.loc[monthly_et.index.month.isin([4,5,6,7,8,9])]
dormant_season = monthly_et.loc[~monthly_et.index.month.isin([4,5,6,7,8,9])]

print(f"Growing season ET: {growing_season.sum():.1f} mm")
print(f"Dormant season ET: {dormant_season.sum():.1f} mm")
```

### Water Management Implications
```python
# Calculate irrigation requirements
# Assume 50% of ET needs to be supplemented by irrigation
irrigation_need = park_data["QE"] * 0.5 * 3600 * 5 / (2.45e6)  # mm per timestep

# Peak irrigation periods
peak_irrigation = irrigation_need.resample("1M").sum()
max_irrigation_month = peak_irrigation.idxmax()

print(f"Peak irrigation month: {max_irrigation_month.strftime('%B')}")
print(f"Peak irrigation need: {peak_irrigation.max():.1f} mm/month")
```

## Policy and Planning Insights

### Quantifying Benefits
The results help answer policy questions:
- **How much cooling per hectare of parkland?**
- **What's the optimal vegetation fraction for different climates?**
- **How do parks compare to other cooling strategies?**
- **What are the water management implications?**

### Design Guidelines  
- **Minimum effective size**: Parks >0.5 hectares show measurable cooling
- **Vegetation type matters**: Trees + grass more effective than grass alone
- **Location optimization**: Parks provide cooling 100-300m downwind
- **Maintenance requirements**: Higher ET requires more irrigation support

## Troubleshooting

**Common Issues:**
1. **Unrealistic ET rates**: Check vegetation parameters and soil moisture
2. **No cooling effect**: Verify vegetation fractions are set correctly
3. **Water balance errors**: Ensure irrigation/rainfall inputs are reasonable

**Validation checks:**
1. Compare ET rates with local measurements (2-6 mm/day typical)
2. Verify cooling effects match urban climatology studies
3. Check that QH+QE energy partitioning is reasonable

## Next Steps

1. **Validate results** against park temperature measurements
2. **Test design scenarios** with different park configurations
3. **Scale up analysis** to neighbourhood or city level
4. **Integrate with building energy models** for comprehensive assessment
5. **Explore climate change impacts** on park effectiveness

## Links and References

- [Urban Green Infrastructure](https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#vegetation-phenology-and-conductance)
- [SuPy Park Configuration](https://supy.readthedocs.io/en/latest/tutorials/python/setup-own-site.html)
- [Urban Cooling Studies](https://suews.readthedocs.io/en/latest/related_publications.html)