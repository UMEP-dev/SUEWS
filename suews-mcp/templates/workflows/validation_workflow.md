# Model Validation Workflow

This workflow guides you through validating SUEWS model predictions against observational data to assess model performance and identify areas for improvement.

## Overview

Model validation involves:
- Comparing simulated outputs with observations
- Calculating statistical performance metrics  
- Identifying systematic biases
- Assessing seasonal and diurnal patterns
- Determining model suitability for your application

## Step 1: Prepare Observational Data

### Required Observations

**Essential measurements:**
- Net radiation (QN) [W/m²]
- Sensible heat flux (QH) [W/m²] 
- Latent heat flux (QE) [W/m²]
- Air temperature (T2) [°C]
- Relative humidity (RH2) [%]

**Additional helpful measurements:**
- Storage heat flux (QS) [W/m²]
- Surface temperatures
- Soil moisture
- Wind speed and direction

### Data Quality Checks

```python
async def prepare_validation_data(obs_file_path):
    """
    Load and quality check observational data
    """
    import pandas as pd
    import numpy as np
    
    # Load observations
    obs_data = pd.read_csv(obs_file_path, parse_dates=['datetime'])
    
    # Quality checks
    quality_report = {
        "total_records": len(obs_data),
        "missing_data": obs_data.isnull().sum(),
        "data_range": {
            col: {"min": obs_data[col].min(), "max": obs_data[col].max()}
            for col in obs_data.select_dtypes(include=[np.number]).columns
        },
        "outliers": identify_outliers(obs_data),
        "temporal_gaps": find_temporal_gaps(obs_data)
    }
    
    return obs_data, quality_report

def identify_outliers(data):
    """Identify potential outliers using statistical methods"""
    outliers = {}
    for col in ['QN', 'QH', 'QE', 'T2', 'RH2']:
        if col in data.columns:
            Q1 = data[col].quantile(0.25)
            Q3 = data[col].quantile(0.75)
            IQR = Q3 - Q1
            
            outliers[col] = {
                "count": len(data[(data[col] < Q1 - 1.5*IQR) | 
                                 (data[col] > Q3 + 1.5*IQR)]),
                "percentage": len(data[(data[col] < Q1 - 1.5*IQR) | 
                                      (data[col] > Q3 + 1.5*IQR)]) / len(data) * 100
            }
    
    return outliers
```

## Step 2: Run Model Simulation

Configure SUEWS to match your observation site as closely as possible:

```python
# Create site-specific configuration
async def create_validation_config(site_info, obs_period):
    """
    Create SUEWS configuration matching observation site
    """
    
    # Get appropriate template
    template = await client.call_tool("get_resource", {
        "resource_path": f"templates/configs/{site_info['type']}.yml"
    })
    
    # Customise for validation site
    validation_config = customize_config_for_site(template, {
        "lat": site_info["latitude"],
        "lng": site_info["longitude"], 
        "alt": site_info["elevation"],
        "start_time": obs_period["start"],
        "end_time": obs_period["end"],
        "forcing_file": site_info["forcing_data"],
        "surface_fractions": site_info["land_cover"],
        "building_height": site_info["building_height"],
        "measurement_height": site_info["measurement_height"]
    })
    
    return validation_config

# Run validation simulation
validation_result = await client.call_tool("run_suews_simulation", {
    "config_file": "validation_config.yml",
    "simulation_id": "validation_run",
    "output_dir": "./validation/"
})
```

## Step 3: Statistical Validation Metrics

### Basic Performance Statistics

```python
def calculate_validation_metrics(observed, simulated):
    """
    Calculate comprehensive validation statistics
    """
    import numpy as np
    from scipy import stats
    
    # Remove missing data pairs
    valid_mask = ~(np.isnan(observed) | np.isnan(simulated))
    obs_clean = observed[valid_mask]
    sim_clean = simulated[valid_mask]
    
    n = len(obs_clean)
    
    metrics = {
        # Basic statistics
        "n_points": n,
        "obs_mean": np.mean(obs_clean),
        "sim_mean": np.mean(sim_clean),
        "obs_std": np.std(obs_clean),
        "sim_std": np.std(sim_clean),
        
        # Bias metrics  
        "bias": np.mean(sim_clean - obs_clean),  # Mean bias
        "pbias": np.mean(sim_clean - obs_clean) / np.mean(obs_clean) * 100,  # Percent bias
        "abs_bias": np.mean(np.abs(sim_clean - obs_clean)),  # Mean absolute error
        
        # Correlation and agreement
        "correlation": stats.pearsonr(obs_clean, sim_clean)[0],
        "r_squared": stats.pearsonr(obs_clean, sim_clean)[0]**2,
        "spearman": stats.spearmanr(obs_clean, sim_clean)[0],
        
        # Error metrics
        "rmse": np.sqrt(np.mean((sim_clean - obs_clean)**2)),
        "nrmse": np.sqrt(np.mean((sim_clean - obs_clean)**2)) / np.mean(obs_clean) * 100,
        "mae": np.mean(np.abs(sim_clean - obs_clean)),
        
        # Efficiency metrics
        "nash_sutcliffe": 1 - np.sum((sim_clean - obs_clean)**2) / np.sum((obs_clean - np.mean(obs_clean))**2),
        "willmott_index": calculate_willmott_index(obs_clean, sim_clean),
        
        # Distribution comparison
        "ks_statistic": stats.ks_2samp(obs_clean, sim_clean)[0],
        "ks_p_value": stats.ks_2samp(obs_clean, sim_clean)[1]
    }
    
    return metrics

def calculate_willmott_index(observed, simulated):
    """Calculate Willmott's index of agreement"""
    obs_mean = np.mean(observed)
    numerator = np.sum((observed - simulated)**2)
    denominator = np.sum((np.abs(simulated - obs_mean) + np.abs(observed - obs_mean))**2)
    
    return 1 - (numerator / denominator)
```

### Performance Benchmarks

```python
def interpret_validation_metrics(metrics, variable):
    """
    Interpret validation metrics with standard benchmarks
    """
    interpretation = {
        "overall_performance": "unknown",
        "strengths": [],
        "weaknesses": [],
        "recommendations": []
    }
    
    # Correlation benchmarks
    if metrics["correlation"] > 0.9:
        interpretation["strengths"].append("Excellent correlation")
    elif metrics["correlation"] > 0.7:
        interpretation["strengths"].append("Good correlation")
    elif metrics["correlation"] < 0.5:
        interpretation["weaknesses"].append("Poor correlation")
    
    # Nash-Sutcliffe efficiency benchmarks
    if metrics["nash_sutcliffe"] > 0.75:
        interpretation["strengths"].append("Excellent model efficiency")
    elif metrics["nash_sutcliffe"] > 0.36:
        interpretation["strengths"].append("Acceptable model efficiency")
    else:
        interpretation["weaknesses"].append("Poor model efficiency")
    
    # Variable-specific benchmarks
    variable_benchmarks = {
        "QH": {"good_rmse": 30, "acceptable_rmse": 50},  # W/m²
        "QE": {"good_rmse": 40, "acceptable_rmse": 60},  # W/m²  
        "QN": {"good_rmse": 25, "acceptable_rmse": 40},  # W/m²
        "T2": {"good_rmse": 1.5, "acceptable_rmse": 2.5},  # °C
        "RH2": {"good_rmse": 8, "acceptable_rmse": 12}  # %
    }
    
    if variable in variable_benchmarks:
        benchmarks = variable_benchmarks[variable]
        if metrics["rmse"] < benchmarks["good_rmse"]:
            interpretation["strengths"].append(f"Low RMSE for {variable}")
        elif metrics["rmse"] > benchmarks["acceptable_rmse"]:
            interpretation["weaknesses"].append(f"High RMSE for {variable}")
    
    return interpretation
```

## Step 4: Temporal Pattern Analysis

### Diurnal Cycle Validation

```python
async def validate_diurnal_patterns(obs_data, sim_data):
    """
    Compare observed and simulated diurnal cycles
    """
    # Group by hour of day
    obs_diurnal = obs_data.groupby(obs_data.datetime.dt.hour).mean()
    sim_diurnal = sim_data.groupby(sim_data.datetime.dt.hour).mean()
    
    diurnal_metrics = {}
    for var in ["QH", "QE", "QN", "T2"]:
        if var in obs_data.columns and var in sim_data.columns:
            diurnal_metrics[var] = {
                "amplitude_obs": obs_diurnal[var].max() - obs_diurnal[var].min(),
                "amplitude_sim": sim_diurnal[var].max() - sim_diurnal[var].min(),
                "peak_hour_obs": obs_diurnal[var].idxmax(),
                "peak_hour_sim": sim_diurnal[var].idxmax(),
                "correlation": obs_diurnal[var].corr(sim_diurnal[var])
            }
    
    return diurnal_metrics

### Seasonal Cycle Validation

async def validate_seasonal_patterns(obs_data, sim_data):
    """
    Compare observed and simulated seasonal cycles
    """
    # Group by month
    obs_monthly = obs_data.groupby(obs_data.datetime.dt.month).mean()
    sim_monthly = sim_data.groupby(sim_data.datetime.dt.month).mean()
    
    seasonal_metrics = {}
    for var in ["QH", "QE", "QN", "T2"]:
        if var in obs_data.columns and var in sim_data.columns:
            seasonal_metrics[var] = {
                "winter_bias": (sim_monthly.loc[[12, 1, 2], var].mean() - 
                               obs_monthly.loc[[12, 1, 2], var].mean()),
                "summer_bias": (sim_monthly.loc[[6, 7, 8], var].mean() - 
                               obs_monthly.loc[[6, 7, 8], var].mean()),
                "seasonal_correlation": obs_monthly[var].corr(sim_monthly[var])
            }
    
    return seasonal_metrics
```

## Step 5: Energy Balance Validation

```python
def validate_energy_balance(obs_data, sim_data):
    """
    Validate energy balance closure and partitioning
    """
    # Calculate energy balance residual
    obs_data['residual_obs'] = obs_data['QN'] - obs_data['QH'] - obs_data['QE'] - obs_data.get('QS', 0)
    sim_data['residual_sim'] = sim_data['QN'] - sim_data['QH'] - sim_data['QE'] - sim_data.get('QS', 0)
    
    # Bowen ratio analysis  
    obs_data['bowen_obs'] = obs_data['QH'] / obs_data['QE']
    sim_data['bowen_sim'] = sim_data['QH'] / sim_data['QE']
    
    energy_balance_metrics = {
        "closure": {
            "obs_closure": 1 - (obs_data['residual_obs'].mean() / obs_data['QN'].mean()),
            "sim_closure": 1 - (sim_data['residual_sim'].mean() / sim_data['QN'].mean())
        },
        "bowen_ratio": {
            "obs_mean": obs_data['bowen_obs'].median(),  # Use median for robustness
            "sim_mean": sim_data['bowen_sim'].median(),
            "correlation": obs_data['bowen_obs'].corr(sim_data['bowen_sim'])
        },
        "partitioning": {
            "qh_fraction_obs": obs_data['QH'].mean() / obs_data['QN'].mean(),
            "qh_fraction_sim": sim_data['QH'].mean() / sim_data['QN'].mean(),
            "qe_fraction_obs": obs_data['QE'].mean() / obs_data['QN'].mean(), 
            "qe_fraction_sim": sim_data['QE'].mean() / sim_data['QN'].mean()
        }
    }
    
    return energy_balance_metrics
```

## Step 6: Generate Validation Report

```python
async def generate_validation_report(obs_data, sim_data, site_info):
    """
    Create comprehensive validation report
    """
    report = {
        "site_information": site_info,
        "data_summary": {
            "observation_period": f"{obs_data.datetime.min()} to {obs_data.datetime.max()}",
            "total_observations": len(obs_data),
            "data_coverage": calculate_data_coverage(obs_data)
        },
        "statistical_metrics": {},
        "temporal_analysis": {},
        "energy_balance": {},
        "recommendations": []
    }
    
    # Calculate metrics for each variable
    for var in ["QH", "QE", "QN", "T2", "RH2"]:
        if var in obs_data.columns and var in sim_data.columns:
            metrics = calculate_validation_metrics(obs_data[var], sim_data[var])
            interpretation = interpret_validation_metrics(metrics, var)
            
            report["statistical_metrics"][var] = {
                "metrics": metrics,
                "interpretation": interpretation
            }
    
    # Temporal patterns
    report["temporal_analysis"]["diurnal"] = await validate_diurnal_patterns(obs_data, sim_data)
    report["temporal_analysis"]["seasonal"] = await validate_seasonal_patterns(obs_data, sim_data)
    
    # Energy balance
    report["energy_balance"] = validate_energy_balance(obs_data, sim_data)
    
    # Overall recommendations
    report["recommendations"] = generate_recommendations(report)
    
    return report

def generate_recommendations(validation_report):
    """
    Generate specific recommendations based on validation results
    """
    recommendations = []
    
    # Check for systematic biases
    for var, results in validation_report["statistical_metrics"].items():
        bias = results["metrics"]["bias"]
        if abs(bias) > 10:  # Variable-specific thresholds would be better
            recommendations.append(f"Address systematic bias in {var} ({bias:.1f} units)")
    
    # Check energy balance
    energy_balance = validation_report["energy_balance"]
    if energy_balance["closure"]["sim_closure"] < 0.8:
        recommendations.append("Improve energy balance closure - check QS parameterisation")
    
    # Check correlations
    low_corr_vars = [var for var, results in validation_report["statistical_metrics"].items()
                     if results["metrics"]["correlation"] < 0.6]
    if low_corr_vars:
        recommendations.append(f"Improve model performance for: {', '.join(low_corr_vars)}")
    
    return recommendations
```

## Step 7: Model Improvement

Based on validation results, consider:

### Parameter Calibration
- Adjust parameters with highest sensitivity (from [sensitivity analysis](sensitivity_analysis.md))
- Focus on parameters affecting poorly-performing variables
- Use automated calibration tools if available

### Configuration Refinement  
- Refine surface fraction estimates
- Update anthropogenic heat profiles
- Adjust initial conditions
- Verify forcing data quality

### Advanced Diagnostics
```python
async def advanced_diagnostics(validation_report):
    """
    Perform advanced diagnostic analysis
    """
    diagnostics = {
        "conditional_performance": analyze_conditional_performance(),
        "error_decomposition": decompose_errors(),
        "physical_consistency": check_physical_consistency(),
        "uncertainty_analysis": quantify_uncertainties()
    }
    
    return diagnostics
```

## Validation Benchmarks by Variable

### Heat Fluxes (W/m²)
- **Excellent**: RMSE < 30, R² > 0.8, |bias| < 10
- **Good**: RMSE < 50, R² > 0.6, |bias| < 20  
- **Acceptable**: RMSE < 70, R² > 0.4, |bias| < 30

### Temperature (°C)
- **Excellent**: RMSE < 1.5, R² > 0.9, |bias| < 0.5
- **Good**: RMSE < 2.5, R² > 0.8, |bias| < 1.0
- **Acceptable**: RMSE < 3.5, R² > 0.6, |bias| < 1.5

### Relative Humidity (%)  
- **Excellent**: RMSE < 8, R² > 0.7, |bias| < 3
- **Good**: RMSE < 12, R² > 0.5, |bias| < 5
- **Acceptable**: RMSE < 16, R² > 0.3, |bias| < 8

## Next Steps

- Use validation results to improve model configuration
- Document model limitations for your specific application
- Consider ensemble approaches for uncertainty quantification
- Apply validated model to scenario analysis

## Resources

- [SUEWS validation examples](https://suews.readthedocs.io/en/latest/validation.html)
- Statistical validation packages (scikit-learn, scipy)
- Energy balance measurement best practices