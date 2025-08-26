# Sensitivity Analysis Workflow

This workflow demonstrates how to systematically test the sensitivity of SUEWS model outputs to different parameter values, helping you understand which parameters most influence your results.

## Overview

Parameter sensitivity analysis helps you:
- Identify which parameters most affect model outputs
- Understand model uncertainty
- Guide field measurement priorities
- Validate model behaviour

## Step 1: Define Base Configuration

Start with a validated baseline configuration:

```python
# Use a template as baseline
base_config = await client.call_tool("get_resource", {
    "resource_path": "templates/configs/residential.yml"
})

# Run baseline simulation
baseline_result = await client.call_tool("run_suews_simulation", {
    "config_file": "baseline_config.yml",
    "simulation_id": "baseline",
    "output_dir": "./sensitivity_analysis/"
})
```

## Step 2: Define Parameters to Test

Common parameters for sensitivity testing:

### Surface Properties
```python
surface_params = {
    "building_fraction": [0.25, 0.35, 0.45],  # ±10% from baseline
    "paved_fraction": [0.20, 0.25, 0.30],
    "grass_fraction": [0.25, 0.30, 0.35],
    "building_height": [6.0, 8.0, 10.0]
}
```

### Thermal Properties
```python
thermal_params = {
    "paved_albedo": [0.12, 0.15, 0.18],
    "building_albedo": [0.17, 0.20, 0.23],
    "grass_albedo": [0.22, 0.25, 0.28]
}
```

### Anthropogenic Heat
```python
heat_params = {
    "qf0_beu_weekday": [12.0, 15.0, 18.0],  # Base energy use (W/m²)
    "qf0_beu_weekend": [9.0, 12.0, 15.0],
    "heating_base_temp": [16.0, 18.0, 20.0]
}
```

## Step 3: Run Parameter Sensitivity Tests

### Single Parameter Sensitivity

Test one parameter at a time while keeping others constant:

```python
async def single_parameter_sensitivity(param_name, param_values, base_config):
    results = {}
    
    for value in param_values:
        # Modify configuration
        modified_config = modify_config(base_config, param_name, value)
        
        # Validate configuration
        validation = await client.call_tool("validate_suews_config", {
            "config_file": f"config_{param_name}_{value}.yml"
        })
        
        if "valid" in validation.content[0].text.lower():
            # Run simulation
            result = await client.call_tool("run_suews_simulation", {
                "config_file": f"config_{param_name}_{value}.yml",
                "simulation_id": f"{param_name}_{value}",
                "output_dir": "./sensitivity_analysis/"
            })
            
            # Analyse outputs
            analysis = await client.call_tool("analyze_suews_output", {
                "output_file": f"./sensitivity_analysis/{param_name}_{value}_output.txt",
                "metrics": ["QH", "QE", "QN", "T2"],
                "time_period": "monthly"
            })
            
            results[value] = analysis
    
    return results

# Example: Test building fraction sensitivity
building_fraction_results = await single_parameter_sensitivity(
    "building_fraction", [0.25, 0.35, 0.45], base_config
)
```

### Multi-Parameter Sensitivity

Test combinations of parameters:

```python
async def multi_parameter_sensitivity():
    # Define parameter combinations
    combinations = [
        {"building_fraction": 0.3, "building_height": 6.0},
        {"building_fraction": 0.3, "building_height": 10.0},
        {"building_fraction": 0.4, "building_height": 6.0},
        {"building_fraction": 0.4, "building_height": 10.0}
    ]
    
    results = {}
    for i, combo in enumerate(combinations):
        config_name = f"combo_{i}"
        
        # Create modified configuration
        modified_config = create_config_with_params(base_config, combo)
        
        # Run simulation
        result = await client.call_tool("run_suews_simulation", {
            "config_file": f"{config_name}.yml",
            "simulation_id": config_name
        })
        
        # Store results
        results[config_name] = result
    
    return results
```

## Step 4: Analyse Sensitivity Results

### Calculate Sensitivity Metrics

```python
def calculate_sensitivity_metrics(baseline, sensitivity_results):
    """
    Calculate sensitivity metrics for each parameter
    """
    sensitivities = {}
    
    for param_value, result in sensitivity_results.items():
        # Extract key metrics (this would parse the actual output)
        metrics = parse_simulation_output(result)
        
        # Calculate relative change from baseline
        relative_change = {}
        for metric in ["QH", "QE", "QN", "T2"]:
            baseline_mean = baseline[metric].mean()
            test_mean = metrics[metric].mean()
            
            relative_change[metric] = (test_mean - baseline_mean) / baseline_mean * 100
        
        sensitivities[param_value] = relative_change
    
    return sensitivities
```

### Interpret Results

Key sensitivity indicators:
- **High sensitivity (>10% change)**: Parameter strongly affects model output
- **Medium sensitivity (2-10% change)**: Parameter has moderate influence  
- **Low sensitivity (<2% change)**: Parameter has minimal impact

### Expected Sensitivities by Urban Type

#### Residential Areas
- **High sensitivity**: Building fraction, anthropogenic heat
- **Medium sensitivity**: Albedo values, building height
- **Low sensitivity**: Vegetation parameters (if vegetation fraction is small)

#### Commercial Areas  
- **High sensitivity**: Anthropogenic heat profiles, building properties
- **Medium sensitivity**: Surface fractions
- **Low sensitivity**: Irrigation parameters

#### Industrial Areas
- **High sensitivity**: Building characteristics, energy emissions
- **Medium sensitivity**: Surface thermal properties
- **Low sensitivity**: Vegetation parameters

## Step 5: Advanced Sensitivity Analysis

### Morris Method (One-at-a-Time)

```python
async def morris_sensitivity_analysis():
    """
    Systematic one-at-a-time parameter variation
    """
    parameters = {
        "building_fraction": [0.2, 0.3, 0.4, 0.5],
        "paved_albedo": [0.10, 0.15, 0.20, 0.25],
        "qf0_beu": [10, 15, 20, 25]
    }
    
    # Generate Morris sampling plan
    morris_results = {}
    
    for param, values in parameters.items():
        param_results = []
        
        for value in values:
            # Run simulation with modified parameter
            result = await run_simulation_with_param(param, value)
            param_results.append(result)
        
        morris_results[param] = param_results
    
    return morris_results
```

### Global Sensitivity Analysis

For more comprehensive analysis, consider Latin Hypercube Sampling:

```python
async def latin_hypercube_sensitivity(n_samples=50):
    """
    Global sensitivity analysis using Latin Hypercube Sampling
    """
    import numpy as np
    from scipy.stats import qmc
    
    # Define parameter ranges
    param_bounds = {
        "building_fraction": [0.2, 0.5],
        "paved_fraction": [0.15, 0.4],
        "grass_fraction": [0.1, 0.4],
        "building_albedo": [0.15, 0.3],
        "paved_albedo": [0.08, 0.2],
        "qf0_beu": [8, 25]
    }
    
    # Generate Latin Hypercube samples
    sampler = qmc.LatinHypercube(d=len(param_bounds))
    samples = sampler.random(n=n_samples)
    
    # Scale samples to parameter bounds
    scaled_samples = []
    for i, (param, bounds) in enumerate(param_bounds.items()):
        scaled_values = qmc.scale(samples[:, i], bounds[0], bounds[1])
        scaled_samples.append(scaled_values)
    
    # Run simulations for all samples
    lhc_results = []
    for sample_idx in range(n_samples):
        param_set = {
            param: scaled_samples[i][sample_idx] 
            for i, param in enumerate(param_bounds.keys())
        }
        
        result = await run_simulation_with_params(param_set)
        lhc_results.append(result)
    
    return lhc_results
```

## Step 6: Report Sensitivity Results

### Generate Summary Report

```python
def generate_sensitivity_report(sensitivity_results):
    """
    Create a comprehensive sensitivity analysis report
    """
    report = {
        "summary": {
            "most_sensitive_params": [],
            "least_sensitive_params": [],
            "critical_thresholds": []
        },
        "detailed_results": sensitivity_results,
        "recommendations": []
    }
    
    # Identify most/least sensitive parameters
    # Add recommendations for field measurements
    # Suggest model calibration priorities
    
    return report
```

## Example Use Cases

### Case 1: New Development Impact
Test how changing building density affects local climate:

```python
development_scenarios = {
    "current": {"building_fraction": 0.25},
    "moderate_development": {"building_fraction": 0.35},
    "high_development": {"building_fraction": 0.45}
}
```

### Case 2: Green Infrastructure
Assess impact of increasing vegetation:

```python
green_scenarios = {
    "baseline": {"grass_fraction": 0.20, "tree_fraction": 0.05},
    "green_roofs": {"grass_fraction": 0.25, "tree_fraction": 0.05},
    "urban_forest": {"grass_fraction": 0.20, "tree_fraction": 0.15}
}
```

## Next Steps

- Use sensitivity results to prioritise field measurements
- Focus calibration efforts on most sensitive parameters
- Consider uncertainty bounds in model predictions
- Apply findings to [Validation Workflow](validation_workflow.md)

## Tools and Resources

- Statistical analysis packages (scipy, numpy)
- Sensitivity analysis libraries (SALib)
- [SUEWS parameter documentation](https://suews.readthedocs.io/)