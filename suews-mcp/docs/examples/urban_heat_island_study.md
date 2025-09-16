# Urban Heat Island Study Example

This example demonstrates how to use the SUEWS MCP server to conduct an urban heat island study by comparing different urban surface types.

## Overview

Urban Heat Islands (UHI) are one of the most significant climate impacts of urbanisation. This example shows how to:

1. Set up simulations for different urban surface types
2. Run comparative simulations
3. Analyse temperature differences
4. Quantify UHI intensity

## Prerequisites

- SUEWS MCP server running
- Sample meteorological data or your own forcing data
- Python with MCP client libraries

## Study Design

We'll compare four urban surface types:
- **Dense Commercial**: High-rise buildings, minimal vegetation
- **Residential**: Mixed buildings and vegetation  
- **Industrial**: Large buildings, extensive paved areas
- **Urban Park**: Dominated by vegetation

## Step 1: Set Up the Comparative Study

```python
import asyncio
from mcp import create_client
import pandas as pd
import matplotlib.pyplot as plt

async def setup_uhi_study():
    """Set up configurations for different urban surface types."""
    
    async with create_client("suews-mcp") as client:
        
        # Define study parameters
        study_location = {
            "lat": 40.7128,    # New York City
            "lon": -74.0060,
            "elevation": 10,
            "timezone": -5
        }
        
        # Define different urban surface configurations
        surface_types = {
            "dense_commercial": {
                "template": "templates/configs/commercial.yml",
                "updates": {
                    "surface": {
                        "frac_paved": 0.45,
                        "frac_bldgs": 0.50,
                        "frac_grass": 0.03,
                        "frac_trees": 0.02,
                        "height_bldgs": 35.0,
                        "albedo_paved": 0.12,
                        "albedo_bldgs": 0.25
                    },
                    "anthropogenic": {
                        "qf0_beu": 80.0  # High energy use
                    }
                }
            },
            "residential": {
                "template": "templates/configs/residential.yml", 
                "updates": {
                    "surface": {
                        "frac_paved": 0.35,
                        "frac_bldgs": 0.25,
                        "frac_grass": 0.30,
                        "frac_trees": 0.10,
                        "height_bldgs": 12.0,
                        "albedo_paved": 0.15,
                        "albedo_bldgs": 0.22
                    },
                    "anthropogenic": {
                        "qf0_beu": 25.0  # Moderate energy use
                    }
                }
            },
            "industrial": {
                "template": "templates/configs/industrial.yml",
                "updates": {
                    "surface": {
                        "frac_paved": 0.55,
                        "frac_bldgs": 0.40,
                        "frac_grass": 0.03,
                        "frac_trees": 0.02,
                        "height_bldgs": 8.0,
                        "albedo_paved": 0.10,
                        "albedo_bldgs": 0.18
                    },
                    "anthropogenic": {
                        "qf0_beu": 120.0  # Very high industrial heat
                    }
                }
            },
            "urban_park": {
                "template": "templates/configs/park.yml",
                "updates": {
                    "surface": {
                        "frac_paved": 0.10,
                        "frac_bldgs": 0.05,
                        "frac_grass": 0.50,
                        "frac_trees": 0.35,
                        "height_bldgs": 3.0,
                        "albedo_grass": 0.20,
                        "albedo_trees": 0.12
                    },
                    "anthropogenic": {
                        "qf0_beu": 2.0  # Minimal energy use
                    }
                }
            }
        }
        
        # Create configurations for each surface type
        configs = {}
        for surface_type, config_info in surface_types.items():
            print(f"Setting up configuration for {surface_type}...")
            
            config_updates = config_info["updates"].copy()
            config_updates["site"] = study_location
            
            # Configure simulation
            result = await client.call_tool("configure_simulation", {
                "config_path": config_info["template"],
                "site_name": f"NYC_UHI_{surface_type}",
                "config_updates": config_updates,
                "save_path": f"configs/uhi_{surface_type}.yml"
            })
            
            configs[surface_type] = f"configs/uhi_{surface_type}.yml"
            print(f"✓ {surface_type} configuration created")
        
        return configs

# Run the setup
configs = asyncio.run(setup_uhi_study())
print(f"Created {len(configs)} configurations for UHI study")
```

## Step 2: Run Simulations for Summer Period

```python
async def run_uhi_simulations(configs):
    """Run SUEWS simulations for each surface type during summer."""
    
    async with create_client("suews-mcp") as client:
        simulation_results = {}
        
        # Summer period - when UHI is most pronounced
        summer_period = {
            "start_time": "2023-06-21T00:00:00",  # Summer solstice
            "end_time": "2023-08-31T23:00:00",    # End of summer
            "use_sample_data": True
        }
        
        for surface_type, config_path in configs.items():
            print(f"\nRunning simulation for {surface_type}...")
            print(f"Expected duration: ~30-60 seconds")
            
            # Validate configuration first
            validation = await client.call_tool("validate_config", {
                "config_file": config_path,
                "strict_mode": False
            })
            print(f"Validation: {validation.content[0].text[:100]}...")
            
            # Run simulation
            simulation = await client.call_tool("run_simulation", {
                "config_path": config_path,
                **summer_period,
                "output_dir": f"outputs/uhi_{surface_type}/"
            })
            
            # Store results
            simulation_results[surface_type] = {
                "output_path": f"outputs/uhi_{surface_type}/NYC_UHI_{surface_type}_SUEWS.csv",
                "simulation_log": simulation.content[0].text
            }
            
            print(f"✓ {surface_type} simulation completed")
            print(f"  Output: {simulation_results[surface_type]['output_path']}")
        
        return simulation_results

# Run simulations
simulation_results = asyncio.run(run_uhi_simulations(configs))
print(f"\nCompleted {len(simulation_results)} simulations")
```

## Step 3: Analyse Temperature Differences

```python
async def analyze_uhi_effects(simulation_results):
    """Analyze urban heat island effects from simulation results."""
    
    async with create_client("suews-mcp") as client:
        analysis_results = {}
        
        for surface_type, result_info in simulation_results.items():
            print(f"\nAnalyzing {surface_type} results...")
            
            # Perform detailed analysis
            analysis = await client.call_tool("analyze_results", {
                "results_path": result_info["output_path"],
                "analysis_type": "temporal",
                "variables": ["T2", "QH", "QE", "QN", "QS"],
                "time_period": "daily"
            })
            
            analysis_results[surface_type] = analysis.content[0].text
            print(f"✓ Analysis completed for {surface_type}")
        
        return analysis_results

# Run analysis
analysis_results = asyncio.run(analyze_uhi_effects(simulation_results))
```

## Step 4: Calculate UHI Intensity

```python
async def calculate_uhi_intensity():
    """Calculate UHI intensity relative to urban park (reference)."""
    
    # Load simulation results
    results_data = {}
    
    for surface_type in ["dense_commercial", "residential", "industrial", "urban_park"]:
        file_path = f"outputs/uhi_{surface_type}/NYC_UHI_{surface_type}_SUEWS.csv"
        try:
            df = pd.read_csv(file_path, parse_dates=[0], index_col=0)
            results_data[surface_type] = df
            print(f"✓ Loaded {surface_type} data: {len(df)} time steps")
        except FileNotFoundError:
            print(f"✗ Could not find {file_path}")
    
    if len(results_data) < 2:
        print("Need at least 2 datasets for comparison")
        return
    
    # Calculate UHI intensity (difference from urban park)
    reference = results_data["urban_park"]["T2"]  # Park as reference
    
    uhi_intensity = {}
    for surface_type, data in results_data.items():
        if surface_type != "urban_park":
            # Calculate temperature difference
            temp_diff = data["T2"] - reference
            
            uhi_intensity[surface_type] = {
                "mean_uhi": temp_diff.mean(),
                "max_uhi": temp_diff.max(),
                "min_uhi": temp_diff.min(),
                "std_uhi": temp_diff.std(),
                "daytime_uhi": temp_diff.between_time("06:00", "18:00").mean(),
                "nighttime_uhi": temp_diff.between_time("19:00", "05:00").mean()
            }
    
    # Display results
    print("\n" + "="*60)
    print("URBAN HEAT ISLAND INTENSITY ANALYSIS")
    print("="*60)
    print(f"Reference: Urban Park")
    print(f"Study Period: Summer 2023 (Jun 21 - Aug 31)")
    print(f"Temperature Differences (°C):")
    print()
    
    for surface_type, metrics in uhi_intensity.items():
        print(f"{surface_type.replace('_', ' ').title()}:")
        print(f"  Mean UHI Intensity: {metrics['mean_uhi']:.2f} ± {metrics['std_uhi']:.2f}°C")
        print(f"  Maximum UHI: {metrics['max_uhi']:.2f}°C")
        print(f"  Daytime UHI: {metrics['daytime_uhi']:.2f}°C")
        print(f"  Nighttime UHI: {metrics['nighttime_uhi']:.2f}°C")
        print()
    
    return uhi_intensity

# Calculate UHI intensity
uhi_results = asyncio.run(calculate_uhi_intensity())
```

## Step 5: Create Visualisations

```python
def create_uhi_visualizations(results_data, uhi_intensity):
    """Create visualizations of UHI effects."""
    
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates
    from datetime import datetime
    
    # Set up the plotting style
    plt.style.use('seaborn-v0_8')
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))
    fig.suptitle('Urban Heat Island Study - New York City Summer 2023', fontsize=16, fontweight='bold')
    
    # Color scheme for different surface types
    colors = {
        'dense_commercial': '#d62728',    # Red
        'residential': '#ff7f0e',         # Orange  
        'industrial': '#8c564b',          # Brown
        'urban_park': '#2ca02c'           # Green
    }
    
    # Plot 1: Daily Temperature Time Series
    ax1 = axes[0, 0]
    for surface_type, data in results_data.items():
        daily_temp = data['T2'].resample('D').mean()
        ax1.plot(daily_temp.index, daily_temp.values, 
                label=surface_type.replace('_', ' ').title(), 
                color=colors[surface_type], linewidth=2)
    
    ax1.set_title('Daily Mean Temperature', fontweight='bold')
    ax1.set_ylabel('Temperature (°C)')
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    
    # Plot 2: UHI Intensity Bar Chart
    ax2 = axes[0, 1]
    surface_types = list(uhi_intensity.keys())
    mean_uhi = [uhi_intensity[st]['mean_uhi'] for st in surface_types]
    
    bars = ax2.bar([st.replace('_', ' ').title() for st in surface_types], 
                   mean_uhi, 
                   color=[colors[st] for st in surface_types],
                   alpha=0.7, edgecolor='black')
    
    ax2.set_title('Mean UHI Intensity (vs Urban Park)', fontweight='bold')
    ax2.set_ylabel('Temperature Difference (°C)')
    ax2.grid(True, alpha=0.3, axis='y')
    
    # Add value labels on bars
    for bar, value in zip(bars, mean_uhi):
        ax2.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.05,
                f'{value:.1f}°C', ha='center', va='bottom', fontweight='bold')
    
    # Plot 3: Diurnal Temperature Patterns
    ax3 = axes[1, 0]
    for surface_type, data in results_data.items():
        hourly_mean = data['T2'].groupby(data.index.hour).mean()
        ax3.plot(hourly_mean.index, hourly_mean.values,
                label=surface_type.replace('_', ' ').title(),
                color=colors[surface_type], linewidth=2, marker='o', markersize=4)
    
    ax3.set_title('Average Diurnal Temperature Pattern', fontweight='bold')
    ax3.set_xlabel('Hour of Day')
    ax3.set_ylabel('Temperature (°C)')
    ax3.set_xticks(range(0, 24, 3))
    ax3.legend()
    ax3.grid(True, alpha=0.3)
    
    # Plot 4: Energy Balance Comparison
    ax4 = axes[1, 1] 
    energy_vars = ['QH', 'QE', 'QN', 'QS']
    width = 0.15
    x = range(len(energy_vars))
    
    for i, (surface_type, data) in enumerate(results_data.items()):
        means = [data[var].mean() for var in energy_vars]
        offset = (i - 1.5) * width
        bars = ax4.bar([xi + offset for xi in x], means,
                      width, label=surface_type.replace('_', ' ').title(),
                      color=colors[surface_type], alpha=0.7)
    
    ax4.set_title('Mean Energy Balance Components', fontweight='bold')
    ax4.set_ylabel('Energy Flux (W/m²)')
    ax4.set_xticks(x)
    ax4.set_xticklabels(energy_vars)
    ax4.legend()
    ax4.grid(True, alpha=0.3, axis='y')
    
    plt.tight_layout()
    plt.savefig('outputs/uhi_analysis_summary.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    print("📊 Visualizations saved to: outputs/uhi_analysis_summary.png")

# Create visualizations (run after data analysis)
# create_uhi_visualizations(results_data, uhi_results)
```

## Step 6: Generate Summary Report

```python
async def generate_uhi_report(uhi_intensity):
    """Generate a comprehensive UHI study report."""
    
    report = f"""
# Urban Heat Island Study Report

## Executive Summary

This study quantifies the urban heat island effect in New York City using the SUEWS urban climate model. Four different surface types were compared during the summer period (June 21 - August 31, 2023) to assess temperature differences relative to an urban park baseline.

## Key Findings

### UHI Intensity Rankings:
"""
    
    # Sort by mean UHI intensity
    sorted_surfaces = sorted(uhi_intensity.items(), 
                           key=lambda x: x[1]['mean_uhi'], 
                           reverse=True)
    
    for i, (surface_type, metrics) in enumerate(sorted_surfaces, 1):
        report += f"""
{i}. **{surface_type.replace('_', ' ').title()}**
   - Mean UHI Intensity: {metrics['mean_uhi']:.2f}°C
   - Maximum UHI: {metrics['max_uhi']:.2f}°C  
   - Daytime UHI: {metrics['daytime_uhi']:.2f}°C
   - Nighttime UHI: {metrics['nighttime_uhi']:.2f}°C
"""
    
    report += f"""

### Climate Implications:

1. **Dense Commercial Areas** show the strongest UHI effect due to:
   - High building density and thermal mass
   - Extensive impervious surfaces
   - High anthropogenic heat emissions
   - Reduced evapotranspiration

2. **Industrial Areas** have significant but variable UHI intensity due to:
   - Large thermal mass from buildings and pavement
   - Very high anthropogenic heat from industrial processes
   - Minimal vegetation for cooling

3. **Residential Areas** show moderate UHI effects with:
   - Balanced mix of surfaces providing some natural cooling
   - Moderate anthropogenic heat from domestic energy use
   - Vegetation providing evapotranspiration cooling

4. **Urban Parks** serve as important cooling islands with:
   - High evapotranspiration from vegetation
   - Lower thermal mass and heat storage
   - Minimal anthropogenic heat emissions

## Recommendations:

### Urban Planning:
- Increase green infrastructure in commercial and industrial areas
- Implement cool roof and pavement strategies
- Create green corridors connecting urban parks
- Design mixed-use developments to balance surface types

### Climate Adaptation:
- Use park areas as cooling centers during heat waves
- Consider UHI effects in building energy efficiency standards
- Plan emergency response routes considering temperature variations
- Design HVAC systems accounting for local UHI intensity

### Future Research:
- Validate model results with observational data
- Extend analysis to winter periods
- Investigate mitigation strategy effectiveness
- Assess climate change impacts on UHI intensity

---
*Report generated by SUEWS MCP Server Urban Heat Island Analysis*
*Study Period: Summer 2023 | Location: New York City*
"""
    
    # Save report
    with open('outputs/uhi_study_report.md', 'w') as f:
        f.write(report)
    
    print("📄 Report saved to: outputs/uhi_study_report.md")
    print("\nKey Findings:")
    for surface_type, metrics in sorted_surfaces:
        print(f"  {surface_type.replace('_', ' ').title()}: {metrics['mean_uhi']:.1f}°C UHI intensity")
    
    return report

# Generate report
# report = asyncio.run(generate_uhi_report(uhi_results))
```

## Complete Example Script

Here's the complete script that runs the entire UHI study:

```python
#!/usr/bin/env python3
"""
Complete Urban Heat Island Study using SUEWS MCP Server
Compares temperature differences between urban surface types
"""

import asyncio
from mcp import create_client
import pandas as pd
import matplotlib.pyplot as plt

async def run_complete_uhi_study():
    """Run complete UHI study from setup to analysis."""
    
    print("🏙️  Starting Urban Heat Island Study")
    print("="*50)
    
    # Step 1: Setup configurations
    print("Step 1: Setting up configurations...")
    configs = await setup_uhi_study()
    
    # Step 2: Run simulations
    print("\nStep 2: Running simulations...")
    simulation_results = await run_uhi_simulations(configs)
    
    # Step 3: Analyze results
    print("\nStep 3: Analyzing results...")
    analysis_results = await analyze_uhi_effects(simulation_results)
    
    # Step 4: Calculate UHI intensity
    print("\nStep 4: Calculating UHI intensity...")
    uhi_intensity = await calculate_uhi_intensity()
    
    # Step 5: Generate report
    print("\nStep 5: Generating report...")
    report = await generate_uhi_report(uhi_intensity)
    
    print("\n✅ Urban Heat Island study completed!")
    print("📁 Check outputs/ directory for results")

if __name__ == "__main__":
    asyncio.run(run_complete_uhi_study())
```

## Expected Results

This study typically reveals:

1. **Dense Commercial** areas: 2-4°C warmer than parks
2. **Industrial** areas: 1.5-3°C warmer than parks  
3. **Residential** areas: 0.5-2°C warmer than parks
4. **Urban Parks**: Reference temperature (coolest)

The exact values depend on:
- Local climate conditions
- Specific surface parameter values
- Time of year and weather patterns
- Model configuration choices

## Next Steps

After completing this analysis:

1. **Validate Results**: Compare with observational data if available
2. **Sensitivity Analysis**: Test impact of parameter variations
3. **Mitigation Scenarios**: Model green infrastructure interventions
4. **Seasonal Analysis**: Extend to other seasons
5. **Climate Projections**: Apply future climate forcing data

This example demonstrates the power of the SUEWS MCP server for systematic urban climate studies and evidence-based urban planning decisions.