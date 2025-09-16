# Parameter Sensitivity Analysis Example

This example demonstrates how to conduct systematic parameter sensitivity analysis using the SUEWS MCP server to understand which parameters most strongly influence model outputs.

## Overview

Parameter sensitivity analysis helps:
- Identify which parameters have the strongest influence on model results
- Guide calibration efforts by focusing on high-impact parameters
- Assess model uncertainty and parameter ranges
- Understand physical processes and model behavior

## Study Design

We'll analyze sensitivity of key SUEWS parameters:

1. **Surface Properties**: Albedo, surface fractions
2. **Thermal Properties**: Heat capacity, thermal conductivity  
3. **Morphology**: Building height, roughness
4. **Anthropogenic Heat**: Energy use intensity
5. **Vegetation**: LAI, conductance parameters

## Prerequisites

- SUEWS MCP server running
- Python with MCP client and analysis libraries
- Sample or real meteorological data

## Step 1: Define Parameter Ranges

```python
import asyncio
from mcp import create_client
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from itertools import product

# Define parameter sensitivity ranges
SENSITIVITY_PARAMETERS = {
    # Surface albedo (major impact on energy balance)
    'albedo_paved': {
        'base': 0.12,
        'range': [0.08, 0.10, 0.12, 0.15, 0.18, 0.22],
        'description': 'Albedo of paved surfaces',
        'units': 'dimensionless',
        'expected_impact': 'High - direct impact on net radiation'
    },
    
    # Building fraction (affects thermal mass and morphology)  
    'frac_bldgs': {
        'base': 0.25,
        'range': [0.15, 0.20, 0.25, 0.30, 0.35, 0.40],
        'description': 'Fraction of building coverage',
        'units': 'dimensionless',
        'expected_impact': 'High - affects storage heat flux and roughness'
    },
    
    # Building height (affects aerodynamics and storage)
    'height_bldgs': {
        'base': 10.0,
        'range': [5.0, 7.5, 10.0, 15.0, 20.0, 25.0],
        'description': 'Average building height',
        'units': 'meters',
        'expected_impact': 'Medium - affects roughness and thermal storage'
    },
    
    # Anthropogenic heat (direct energy input)
    'qf0_beu': {
        'base': 20.0,
        'range': [5.0, 10.0, 20.0, 30.0, 50.0, 75.0],
        'description': 'Base energy use (anthropogenic heat)',
        'units': 'W/m²', 
        'expected_impact': 'High - direct heat input to system'
    },
    
    # Thermal conductivity (affects heat transfer)
    'thermal_conductivity': {
        'base': 1.2,
        'range': [0.5, 0.8, 1.2, 1.5, 2.0, 2.5],
        'description': 'Thermal conductivity of urban surfaces',
        'units': 'W/m/K',
        'expected_impact': 'Medium - affects storage heat flux timing'
    },
    
    # Heat capacity (affects thermal storage)
    'heat_capacity': {
        'base': 1.4e6,
        'range': [1.0e6, 1.2e6, 1.4e6, 1.6e6, 2.0e6, 2.4e6],
        'description': 'Volumetric heat capacity',
        'units': 'J/m³/K',
        'expected_impact': 'Medium - affects storage heat flux magnitude'
    }
}

print("Parameter Sensitivity Study Design")
print("="*50)
for param, info in SENSITIVITY_PARAMETERS.items():
    print(f"{param}:")
    print(f"  Base value: {info['base']} {info['units']}")
    print(f"  Test range: {min(info['range'])}-{max(info['range'])} {info['units']}")
    print(f"  Expected impact: {info['expected_impact']}")
    print()
```

## Step 2: One-at-a-Time Sensitivity Analysis

```python
async def run_sensitivity_analysis():
    """Run one-at-a-time sensitivity analysis for each parameter."""
    
    async with create_client("suews-mcp") as client:
        
        # Base configuration
        base_config = "templates/configs/residential.yml" 
        study_period = {
            "start_time": "2023-07-01T00:00:00",  # Summer month
            "end_time": "2023-07-31T23:00:00",
            "use_sample_data": True
        }
        
        sensitivity_results = {}
        
        for param_name, param_info in SENSITIVITY_PARAMETERS.items():
            print(f"\n🔍 Analyzing sensitivity to {param_name}")
            print(f"   Description: {param_info['description']}")
            print(f"   Testing {len(param_info['range'])} values...")
            
            param_results = {}
            
            for i, param_value in enumerate(param_info['range']):
                print(f"   Test {i+1}/{len(param_info['range'])}: {param_name} = {param_value} {param_info['units']}")
                
                # Create configuration update
                config_updates = create_config_update(param_name, param_value)
                
                # Configure simulation
                config_result = await client.call_tool("configure_simulation", {
                    "config_path": base_config,
                    "site_name": f"sensitivity_{param_name}_{param_value}",
                    "config_updates": config_updates,
                    "save_path": f"configs/sensitivity_{param_name}_{param_value}.yml"
                })
                
                # Run simulation
                sim_result = await client.call_tool("run_simulation", {
                    "config_path": f"configs/sensitivity_{param_name}_{param_value}.yml",
                    **study_period,
                    "output_dir": f"outputs/sensitivity_{param_name}_{param_value}/"
                })
                
                # Quick analysis
                analysis = await client.call_tool("analyze_results", {
                    "results_path": f"outputs/sensitivity_{param_name}_{param_value}/sensitivity_{param_name}_{param_value}_SUEWS.csv",
                    "analysis_type": "summary",
                    "variables": ["T2", "QH", "QE", "QN", "QS"]
                })
                
                # Extract key metrics from results (simplified for example)
                param_results[param_value] = {
                    "config_path": f"configs/sensitivity_{param_name}_{param_value}.yml",
                    "output_path": f"outputs/sensitivity_{param_name}_{param_value}/sensitivity_{param_name}_{param_value}_SUEWS.csv",
                    "analysis": analysis.content[0].text
                }
                
                print(f"     ✓ Completed")
            
            sensitivity_results[param_name] = param_results
            print(f"   ✅ Completed sensitivity analysis for {param_name}")
        
        return sensitivity_results

def create_config_update(param_name, param_value):
    """Create configuration update for specific parameter."""
    
    # Map parameter names to configuration structure
    config_mappings = {
        'albedo_paved': {
            'surface': {'albedo_paved': param_value}
        },
        'frac_bldgs': {
            'surface': {
                'frac_bldgs': param_value,
                'frac_paved': 0.35,
                'frac_grass': max(0.05, 0.35 - (param_value - 0.25)),  # Adjust grass to maintain sum=1
                'frac_trees': max(0.05, 0.05 + (0.25 - param_value))   # Adjust trees to maintain sum=1
            }
        },
        'height_bldgs': {
            'surface': {'height_bldgs': param_value}
        },
        'qf0_beu': {
            'anthropogenic': {'qf0_beu': param_value}
        },
        'thermal_conductivity': {
            'surface': {'thermal_conductivity_paved': param_value, 'thermal_conductivity_bldgs': param_value}
        },
        'heat_capacity': {
            'surface': {'heat_capacity_paved': param_value, 'heat_capacity_bldgs': param_value}
        }
    }
    
    return config_mappings.get(param_name, {})

# Run the sensitivity analysis
print("🚀 Starting parameter sensitivity analysis...")
sensitivity_results = asyncio.run(run_sensitivity_analysis())
print(f"\n✅ Completed sensitivity analysis for {len(sensitivity_results)} parameters")
```

## Step 3: Extract and Process Results

```python
def extract_sensitivity_metrics(sensitivity_results):
    """Extract key metrics from sensitivity analysis results."""
    
    sensitivity_metrics = {}
    
    for param_name, param_results in sensitivity_results.items():
        print(f"\n📊 Processing results for {param_name}...")
        
        param_values = []
        mean_t2 = []
        mean_qh = []
        mean_qe = []
        mean_qn = []
        
        for param_value, result_info in param_results.items():
            # Load simulation results
            try:
                df = pd.read_csv(result_info['output_path'], 
                               parse_dates=[0], index_col=0)
                
                param_values.append(param_value)
                mean_t2.append(df['T2'].mean())
                mean_qh.append(df['QH'].mean())
                mean_qe.append(df['QE'].mean())  
                mean_qn.append(df['QN'].mean())
                
            except FileNotFoundError:
                print(f"   ⚠ Warning: Could not find {result_info['output_path']}")
        
        # Create parameter sensitivity dataframe
        sensitivity_df = pd.DataFrame({
            'param_value': param_values,
            'T2_mean': mean_t2,
            'QH_mean': mean_qh, 
            'QE_mean': mean_qe,
            'QN_mean': mean_qn
        })
        
        # Calculate sensitivity metrics
        base_value = SENSITIVITY_PARAMETERS[param_name]['base']
        base_idx = sensitivity_df['param_value'].sub(base_value).abs().idxmin()
        
        # Relative sensitivity: (change in output) / (fractional change in parameter)
        param_range = sensitivity_df['param_value'].max() - sensitivity_df['param_value'].min()
        
        sensitivities = {}
        for var in ['T2_mean', 'QH_mean', 'QE_mean', 'QN_mean']:
            var_range = sensitivity_df[var].max() - sensitivity_df[var].min()
            base_var = sensitivity_df.loc[base_idx, var]
            
            # Normalized sensitivity: (relative change in output) / (relative change in parameter)
            rel_sensitivity = (var_range / base_var) / (param_range / base_value) if base_var != 0 and base_value != 0 else 0
            
            sensitivities[var] = {
                'range': var_range,
                'base_value': base_var,
                'rel_sensitivity': abs(rel_sensitivity),
                'units': {'T2_mean': '°C', 'QH_mean': 'W/m²', 'QE_mean': 'W/m²', 'QN_mean': 'W/m²'}[var]
            }
        
        sensitivity_metrics[param_name] = {
            'data': sensitivity_df,
            'sensitivities': sensitivities,
            'param_info': SENSITIVITY_PARAMETERS[param_name]
        }
        
        print(f"   ✓ Processed {len(sensitivity_df)} parameter values")
    
    return sensitivity_metrics

# Process results
sensitivity_metrics = extract_sensitivity_metrics(sensitivity_results)
```

## Step 4: Analyze and Rank Parameter Importance

```python
def analyze_parameter_importance(sensitivity_metrics):
    """Analyze and rank parameter importance based on sensitivity metrics."""
    
    print("\n" + "="*70)
    print("PARAMETER SENSITIVITY ANALYSIS RESULTS")
    print("="*70)
    
    # Create summary table
    summary_data = []
    
    for param_name, metrics in sensitivity_metrics.items():
        param_info = metrics['param_info']
        sensitivities = metrics['sensitivities']
        
        # Calculate overall sensitivity score (weighted average)
        weights = {'T2_mean': 0.3, 'QH_mean': 0.25, 'QE_mean': 0.25, 'QN_mean': 0.2}
        overall_sensitivity = sum(sensitivities[var]['rel_sensitivity'] * weights[var] 
                                for var in weights.keys())
        
        summary_data.append({
            'parameter': param_name,
            'description': param_info['description'],
            'base_value': param_info['base'],
            'units': param_info['units'],
            'T2_sensitivity': sensitivities['T2_mean']['rel_sensitivity'],
            'QH_sensitivity': sensitivities['QH_mean']['rel_sensitivity'],
            'QE_sensitivity': sensitivities['QE_mean']['rel_sensitivity'],
            'QN_sensitivity': sensitivities['QN_mean']['rel_sensitivity'],
            'overall_sensitivity': overall_sensitivity
        })
    
    # Create summary DataFrame and sort by overall sensitivity
    summary_df = pd.DataFrame(summary_data)
    summary_df = summary_df.sort_values('overall_sensitivity', ascending=False)
    
    print("\nPARAMETER SENSITIVITY RANKING:")
    print("-" * 70)
    
    for i, (_, row) in enumerate(summary_df.iterrows(), 1):
        print(f"{i}. {row['parameter']} ({row['description']})")
        print(f"   Overall Sensitivity: {row['overall_sensitivity']:.3f}")
        print(f"   T2: {row['T2_sensitivity']:.3f} | QH: {row['QH_sensitivity']:.3f} | "
              f"QE: {row['QE_sensitivity']:.3f} | QN: {row['QN_sensitivity']:.3f}")
        print(f"   Base value: {row['base_value']} {row['units']}")
        print()
    
    # Classify sensitivity levels
    high_sensitivity = summary_df[summary_df['overall_sensitivity'] > 0.5]['parameter'].tolist()
    medium_sensitivity = summary_df[
        (summary_df['overall_sensitivity'] > 0.2) & 
        (summary_df['overall_sensitivity'] <= 0.5)
    ]['parameter'].tolist()
    low_sensitivity = summary_df[summary_df['overall_sensitivity'] <= 0.2]['parameter'].tolist()
    
    print("SENSITIVITY CLASSIFICATION:")
    print("-" * 40)
    print(f"🔴 HIGH SENSITIVITY (>0.5): {high_sensitivity}")
    print(f"🟡 MEDIUM SENSITIVITY (0.2-0.5): {medium_sensitivity}")
    print(f"🟢 LOW SENSITIVITY (<0.2): {low_sensitivity}")
    
    return summary_df, {
        'high': high_sensitivity,
        'medium': medium_sensitivity, 
        'low': low_sensitivity
    }

# Analyze parameter importance
summary_df, sensitivity_classes = analyze_parameter_importance(sensitivity_metrics)
```

## Step 5: Create Sensitivity Visualizations

```python
def create_sensitivity_plots(sensitivity_metrics, summary_df):
    """Create comprehensive sensitivity analysis visualizations."""
    
    fig = plt.figure(figsize=(16, 12))
    
    # Color scheme
    colors = plt.cm.viridis(np.linspace(0, 1, len(sensitivity_metrics)))
    
    # Plot 1: Parameter sensitivity spider plot
    ax1 = plt.subplot(2, 3, 1, projection='polar')
    
    variables = ['T2_sensitivity', 'QH_sensitivity', 'QE_sensitivity', 'QN_sensitivity']
    angles = np.linspace(0, 2 * np.pi, len(variables), endpoint=False).tolist()
    angles += angles[:1]  # Complete the circle
    
    for i, (_, row) in enumerate(summary_df.iterrows()):
        values = [row[var] for var in variables] + [row[variables[0]]]
        ax1.plot(angles, values, 'o-', linewidth=2, label=row['parameter'], color=colors[i])
        ax1.fill(angles, values, alpha=0.1, color=colors[i])
    
    ax1.set_xticks(angles[:-1])
    ax1.set_xticklabels(['T2', 'QH', 'QE', 'QN'])
    ax1.set_ylim(0, summary_df[variables].max().max() * 1.1)
    ax1.set_title('Parameter Sensitivity by Variable', fontweight='bold', pad=20)
    ax1.legend(loc='upper right', bbox_to_anchor=(1.3, 1.0))
    
    # Plot 2: Overall sensitivity ranking
    ax2 = plt.subplot(2, 3, 2)
    bars = ax2.barh(range(len(summary_df)), summary_df['overall_sensitivity'], 
                    color=colors, alpha=0.7)
    ax2.set_yticks(range(len(summary_df)))
    ax2.set_yticklabels(summary_df['parameter'], fontsize=10)
    ax2.set_xlabel('Overall Sensitivity Score')
    ax2.set_title('Parameter Sensitivity Ranking', fontweight='bold')
    ax2.grid(True, alpha=0.3, axis='x')
    
    # Add sensitivity classification lines
    ax2.axvline(0.5, color='red', linestyle='--', alpha=0.7, label='High')
    ax2.axvline(0.2, color='orange', linestyle='--', alpha=0.7, label='Medium')
    ax2.legend()
    
    # Plots 3-6: Individual parameter response curves
    plot_positions = [(2, 3, 3), (2, 3, 4), (2, 3, 5), (2, 3, 6)]
    top_params = summary_df.head(4)['parameter'].tolist()
    
    for i, param_name in enumerate(top_params):
        ax = plt.subplot(*plot_positions[i])
        
        metrics = sensitivity_metrics[param_name]
        data = metrics['data']
        param_info = metrics['param_info']
        
        # Plot T2 response
        ax.plot(data['param_value'], data['T2_mean'], 'o-', 
                color='red', linewidth=2, markersize=6, label='T2 (°C)')
        
        # Mark base value
        base_value = param_info['base']
        base_idx = data['param_value'].sub(base_value).abs().idxmin()
        ax.axvline(base_value, color='gray', linestyle='--', alpha=0.7, label='Base value')
        
        ax.set_xlabel(f"{param_name} ({param_info['units']})")
        ax.set_ylabel('Temperature (°C)', color='red')
        ax.tick_params(axis='y', labelcolor='red')
        ax.set_title(f'{param_name} Response', fontweight='bold', fontsize=10)
        ax.grid(True, alpha=0.3)
        
        # Add secondary y-axis for QH
        ax2 = ax.twinx()
        ax2.plot(data['param_value'], data['QH_mean'], 's--',
                color='blue', linewidth=2, markersize=4, alpha=0.7, label='QH (W/m²)')
        ax2.set_ylabel('QH (W/m²)', color='blue')
        ax2.tick_params(axis='y', labelcolor='blue')
    
    plt.suptitle('SUEWS Parameter Sensitivity Analysis Results', 
                fontsize=16, fontweight='bold')
    plt.tight_layout()
    plt.savefig('outputs/parameter_sensitivity_analysis.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    print("📊 Sensitivity plots saved to: outputs/parameter_sensitivity_analysis.png")

# Create visualizations
create_sensitivity_plots(sensitivity_metrics, summary_df)
```

## Step 6: Generate Calibration Recommendations

```python
def generate_calibration_recommendations(sensitivity_classes, summary_df):
    """Generate parameter calibration recommendations based on sensitivity analysis."""
    
    recommendations = f"""
# Parameter Calibration Recommendations

Based on the sensitivity analysis, here are prioritized recommendations for model calibration:

## Phase 1: High Priority Parameters (Focus First)

Focus calibration efforts on these parameters as they have the strongest influence on model outputs:

"""
    
    high_params = summary_df[summary_df['parameter'].isin(sensitivity_classes['high'])]
    for _, param in high_params.iterrows():
        recommendations += f"""
### {param['parameter']}
- **Description**: {param['description']}
- **Sensitivity Score**: {param['overall_sensitivity']:.3f}
- **Current Value**: {param['base_value']} {param['units']}
- **Calibration Priority**: **HIGH** 
- **Recommendation**: Adjust within ±30% of base value and validate against observations
- **Key Impact**: Strong influence on {'temperature' if param['T2_sensitivity'] > 0.3 else 'energy fluxes'}
"""

    recommendations += f"""

## Phase 2: Medium Priority Parameters (Calibrate After Phase 1)

These parameters have moderate influence and should be calibrated after high-priority parameters:

"""
    
    medium_params = summary_df[summary_df['parameter'].isin(sensitivity_classes['medium'])]
    for _, param in medium_params.iterrows():
        recommendations += f"""
### {param['parameter']}
- **Description**: {param['description']} 
- **Sensitivity Score**: {param['overall_sensitivity']:.3f}
- **Recommendation**: Fine-tune within ±20% after Phase 1 calibration
"""

    recommendations += f"""

## Phase 3: Low Priority Parameters (Final Tuning)

These parameters have minimal influence and can be adjusted last:

"""
    
    low_params = summary_df[summary_df['parameter'].isin(sensitivity_classes['low'])]
    for _, param in low_params.iterrows():
        recommendations += f"""
### {param['parameter']}
- **Description**: {param['description']}
- **Sensitivity Score**: {param['overall_sensitivity']:.3f} 
- **Recommendation**: Keep at literature values unless specific site data available
"""

    recommendations += f"""

## Calibration Strategy

### 1. Sequential Calibration Approach
- Calibrate parameters in order of sensitivity ranking
- Validate each parameter before moving to the next
- Use observational data for validation when available

### 2. Recommended Parameter Ranges

Based on the sensitivity analysis, use these ranges for optimization:

"""
    
    for _, param in summary_df.head(3).iterrows():  # Top 3 most sensitive
        param_info = SENSITIVITY_PARAMETERS[param['parameter']]
        base_val = param['base_value']
        recommendations += f"""
- **{param['parameter']}**: {base_val * 0.7:.2f} - {base_val * 1.3:.2f} {param['units']} (±30% of base)
"""
    
    recommendations += f"""

### 3. Validation Metrics Priority

Focus validation on variables most affected by sensitive parameters:

1. **Temperature (T2)**: Most sensitive to albedo and anthropogenic heat
2. **Sensible Heat Flux (QH)**: Sensitive to surface properties and energy inputs  
3. **Net Radiation (QN)**: Directly affected by albedo changes
4. **Storage Heat Flux (QS)**: Sensitive to thermal properties and morphology

### 4. Calibration Workflow

```python
# Recommended calibration sequence
async def calibrate_model():
    # Phase 1: High sensitivity parameters
    for param in {sensitivity_classes['high']}:
        print(f"Calibrating {{param}}...")
        # Run optimization for this parameter
        # Validate against observations
        # Accept/reject parameter change
    
    # Phase 2: Medium sensitivity parameters  
    for param in {sensitivity_classes['medium']}:
        print(f"Fine-tuning {{param}}...")
        # Smaller parameter adjustments
        # Cross-validate with Phase 1 results
    
    # Phase 3: Final validation
    # Run full validation with all parameters
    # Check overall model performance
```

---
*Calibration recommendations generated from SUEWS MCP Server sensitivity analysis*
"""
    
    # Save recommendations
    with open('outputs/calibration_recommendations.md', 'w') as f:
        f.write(recommendations)
    
    print("📋 Calibration recommendations saved to: outputs/calibration_recommendations.md")
    
    return recommendations

# Generate calibration recommendations
recommendations = generate_calibration_recommendations(sensitivity_classes, summary_df)
print("\n✅ Parameter sensitivity analysis complete!")
print("\n📁 Output files generated:")
print("  - outputs/parameter_sensitivity_analysis.png")
print("  - outputs/calibration_recommendations.md")
print("  - Individual simulation results in outputs/sensitivity_*/")
```

## Step 7: Advanced Multi-Parameter Sensitivity

```python
async def run_multiparameter_sensitivity():
    """Run multi-parameter sensitivity analysis for parameter interactions."""
    
    print("\n🔍 Running multi-parameter sensitivity analysis...")
    
    # Select top 2-3 most sensitive parameters for interaction analysis
    top_params = summary_df.head(3)['parameter'].tolist()
    
    async with create_client("suews-mcp") as client:
        interaction_results = []
        
        # Test parameter combinations
        for i, param1 in enumerate(top_params):
            for j, param2 in enumerate(top_params[i+1:], i+1):
                print(f"Testing interaction: {param1} × {param2}")
                
                # Test high/low combinations
                combinations = [
                    ('low', 'low'),
                    ('low', 'high'), 
                    ('high', 'low'),
                    ('high', 'high')
                ]
                
                for combo in combinations:
                    # Get parameter values
                    param1_values = SENSITIVITY_PARAMETERS[param1]['range']
                    param2_values = SENSITIVITY_PARAMETERS[param2]['range']
                    
                    val1 = param1_values[0] if combo[0] == 'low' else param1_values[-1]
                    val2 = param2_values[0] if combo[1] == 'low' else param2_values[-1]
                    
                    # Create combined configuration update
                    config_updates = {}
                    config_updates.update(create_config_update(param1, val1))
                    config_updates.update(create_config_update(param2, val2))
                    
                    # Run simulation
                    sim_name = f"interaction_{param1}_{combo[0]}_{param2}_{combo[1]}"
                    
                    config_result = await client.call_tool("configure_simulation", {
                        "config_path": "templates/configs/residential.yml",
                        "site_name": sim_name,
                        "config_updates": config_updates,
                        "save_path": f"configs/{sim_name}.yml"
                    })
                    
                    sim_result = await client.call_tool("run_simulation", {
                        "config_path": f"configs/{sim_name}.yml",
                        "start_time": "2023-07-01T00:00:00",
                        "end_time": "2023-07-31T23:00:00",
                        "use_sample_data": True,
                        "output_dir": f"outputs/{sim_name}/"
                    })
                    
                    # Store result info
                    interaction_results.append({
                        'param1': param1,
                        'param2': param2,
                        'val1': val1,
                        'val2': val2,
                        'combo': combo,
                        'output_path': f"outputs/{sim_name}/{sim_name}_SUEWS.csv"
                    })
        
        print(f"✅ Completed {len(interaction_results)} interaction simulations")
        return interaction_results

# Run multi-parameter analysis (optional - computationally intensive)
# interaction_results = asyncio.run(run_multiparameter_sensitivity())
```

## Complete Example Script

```python
#!/usr/bin/env python3
"""
Complete Parameter Sensitivity Analysis using SUEWS MCP Server
Systematic analysis of parameter influence on model outputs
"""

import asyncio
from mcp import create_client
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

async def run_complete_sensitivity_study():
    """Run complete parameter sensitivity analysis."""
    
    print("🔬 Starting Parameter Sensitivity Analysis")
    print("="*50)
    
    # Step 1: Run sensitivity analysis
    print("Step 1: Running one-at-a-time sensitivity analysis...")
    sensitivity_results = await run_sensitivity_analysis()
    
    # Step 2: Process results
    print("\nStep 2: Processing simulation results...")
    sensitivity_metrics = extract_sensitivity_metrics(sensitivity_results)
    
    # Step 3: Analyze importance
    print("\nStep 3: Analyzing parameter importance...")
    summary_df, sensitivity_classes = analyze_parameter_importance(sensitivity_metrics)
    
    # Step 4: Create visualizations
    print("\nStep 4: Creating visualizations...")
    create_sensitivity_plots(sensitivity_metrics, summary_df)
    
    # Step 5: Generate recommendations
    print("\nStep 5: Generating calibration recommendations...")
    recommendations = generate_calibration_recommendations(sensitivity_classes, summary_df)
    
    print("\n✅ Parameter sensitivity analysis completed!")
    print("\n🎯 Key Findings:")
    
    high_sensitivity = summary_df.head(2)
    for _, param in high_sensitivity.iterrows():
        print(f"  • {param['parameter']}: {param['overall_sensitivity']:.3f} sensitivity score")
    
    return sensitivity_results, sensitivity_metrics, summary_df

if __name__ == "__main__":
    asyncio.run(run_complete_sensitivity_study())
```

## Expected Results

This analysis typically reveals:

### High Sensitivity Parameters (>0.5):
- **Albedo**: Direct impact on net radiation and temperature
- **Anthropogenic Heat**: Direct energy input affects all fluxes
- **Building Fraction**: Major influence on thermal mass and morphology

### Medium Sensitivity Parameters (0.2-0.5):
- **Building Height**: Affects aerodynamics and storage
- **Thermal Conductivity**: Influences heat transfer rates

### Low Sensitivity Parameters (<0.2):
- **Heat Capacity**: Minor influence on flux magnitudes

## Applications

Use these results to:

1. **Focus Calibration**: Prioritize high-sensitivity parameters
2. **Reduce Uncertainty**: Narrow parameter ranges based on sensitivity
3. **Improve Efficiency**: Skip detailed calibration of low-sensitivity parameters
4. **Guide Data Collection**: Focus measurements on validating sensitive parameters
5. **Assess Model Robustness**: Understand parameter uncertainty impacts

This systematic approach ensures efficient and effective model calibration for urban climate studies.