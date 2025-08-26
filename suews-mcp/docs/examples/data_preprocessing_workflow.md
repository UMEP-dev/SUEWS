# Data Preprocessing Workflow Example

This example demonstrates how to use the SUEWS MCP server to preprocess meteorological data from various sources and prepare it for SUEWS simulations.

## Overview

Data preprocessing is crucial for reliable SUEWS simulations. This workflow covers:

1. **Data Quality Assessment**: Identify missing data, outliers, and inconsistencies
2. **Format Conversion**: Convert between CSV, Excel, NetCDF, and SUEWS formats
3. **Gap Filling**: Handle missing data with appropriate interpolation methods
4. **Energy Balance Validation**: Check physical consistency of flux measurements
5. **Time Series Validation**: Ensure proper temporal structure and resolution

## Common Data Sources

- **Weather Stations**: Standard meteorological variables (temperature, humidity, wind, radiation)
- **Flux Towers**: Eddy covariance measurements of energy and water fluxes
- **Satellite Data**: Gridded meteorological datasets (reanalysis, remote sensing)
- **Climate Models**: Output from regional/global climate models

## Prerequisites

- SUEWS MCP server running
- Raw meteorological data in various formats
- Python with MCP client libraries

## Step 1: Assess Raw Data Quality

```python
import asyncio
from mcp import create_client
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

async def assess_raw_data():
    """Assess quality of raw meteorological data files."""
    
    async with create_client("suews-mcp") as client:
        
        # Example data files from different sources
        data_files = {
            'weather_station': {
                'file': 'raw_data/london_weather_station_2023.csv',
                'format': 'csv',
                'description': 'Standard meteorological station data'
            },
            'flux_tower': {
                'file': 'raw_data/london_flux_tower_2023.xlsx', 
                'format': 'excel',
                'description': 'Eddy covariance flux measurements'
            },
            'reanalysis': {
                'file': 'raw_data/era5_london_2023.nc',
                'format': 'netcdf',
                'description': 'ERA5 reanalysis gridded data'
            },
            'manual_observations': {
                'file': 'raw_data/manual_observations.txt',
                'format': 'txt', 
                'description': 'Manually recorded observations'
            }
        }
        
        assessment_results = {}
        
        for source_name, file_info in data_files.items():
            print(f"\n📊 Assessing {source_name} data...")
            print(f"   Description: {file_info['description']}")
            print(f"   File: {file_info['file']}")
            
            # Preprocess and assess data quality
            try:
                result = await client.call_tool("preprocess_forcing", {
                    "input_file": file_info['file'],
                    "output_file": f"processed/{source_name}_processed.txt",
                    "validate_energy_balance": True,
                    "auto_fix_issues": False  # Don't fix yet, just assess
                })
                
                assessment_results[source_name] = {
                    'file_info': file_info,
                    'assessment': result.content[0].text,
                    'status': 'success'
                }
                
                print(f"   ✅ Assessment completed")
                print(f"   Preview: {result.content[0].text[:200]}...")
                
            except Exception as e:
                assessment_results[source_name] = {
                    'file_info': file_info,
                    'error': str(e),
                    'status': 'failed'
                }
                print(f"   ❌ Assessment failed: {str(e)}")
        
        return assessment_results

# Run data quality assessment
print("🔍 Starting data quality assessment...")
assessment_results = asyncio.run(assess_raw_data())

# Display summary
print("\n" + "="*60)
print("DATA QUALITY ASSESSMENT SUMMARY")
print("="*60)

for source, result in assessment_results.items():
    status_icon = "✅" if result['status'] == 'success' else "❌"
    print(f"{status_icon} {source}: {result['status']}")
    if result['status'] == 'failed':
        print(f"   Error: {result['error']}")
```

## Step 2: Handle Different Data Formats

```python
async def convert_data_formats():
    """Convert various data formats to SUEWS-ready format."""
    
    async with create_client("suews-mcp") as client:
        
        conversion_tasks = [
            {
                'name': 'weather_station_csv',
                'input_file': 'raw_data/london_weather_station_2023.csv',
                'output_file': 'processed/weather_station_suews.txt',
                'input_format': 'csv',
                'output_format': 'suews_txt',
                'column_mapping': {
                    'air_temperature': 'Tair',
                    'relative_humidity': 'RH',
                    'wind_speed': 'U',
                    'wind_direction': 'WDir', 
                    'pressure': 'Pres',
                    'global_radiation': 'Kdown',
                    'precipitation': 'Rain'
                }
            },
            {
                'name': 'flux_tower_excel',
                'input_file': 'raw_data/london_flux_tower_2023.xlsx',
                'output_file': 'processed/flux_tower_suews.txt', 
                'input_format': 'excel',
                'output_format': 'suews_txt',
                'column_mapping': {
                    'TIMESTAMP': 'datetime',
                    'TA_1_1_1': 'Tair',         # Air temperature
                    'RH_1_1_1': 'RH',           # Relative humidity  
                    'WS_1_1_1': 'U',            # Wind speed
                    'WD_1_1_1': 'WDir',         # Wind direction
                    'PA_1_1_1': 'Pres',         # Pressure
                    'SW_IN_1_1_1': 'Kdown',     # Shortwave incoming
                    'LW_IN_1_1_1': 'Ldown',     # Longwave incoming
                    'NETRAD_1_1_1': 'QN',       # Net radiation
                    'H_1_1_1': 'QH',            # Sensible heat flux
                    'LE_1_1_1': 'QE',           # Latent heat flux
                    'G_1_1_1': 'QS',            # Ground heat flux  
                    'P_1_1_1': 'Rain'           # Precipitation
                }
            },
            {
                'name': 'reanalysis_netcdf',
                'input_file': 'raw_data/era5_london_2023.nc',
                'output_file': 'processed/reanalysis_suews.txt',
                'input_format': 'netcdf',
                'output_format': 'suews_txt',
                'column_mapping': {
                    't2m': 'Tair',              # 2m temperature (K -> °C)
                    'r2m': 'RH',                # 2m relative humidity
                    'u10': 'U10',               # 10m u wind component
                    'v10': 'V10',               # 10m v wind component  
                    'sp': 'Pres',               # Surface pressure
                    'ssrd': 'Kdown',            # Surface solar radiation downwards
                    'strd': 'Ldown',            # Surface thermal radiation downwards
                    'tp': 'Rain'                # Total precipitation
                }
            }
        ]
        
        conversion_results = {}
        
        for task in conversion_tasks:
            print(f"\n🔄 Converting {task['name']}...")
            print(f"   {task['input_format'].upper()} → {task['output_format'].upper()}")
            print(f"   Mapping {len(task['column_mapping'])} columns")
            
            try:
                result = await client.call_tool("convert_data_format", {
                    "input_file": task['input_file'],
                    "output_file": task['output_file'],
                    "input_format": task['input_format'],
                    "output_format": task['output_format'],
                    "column_mapping": task['column_mapping']
                })
                
                conversion_results[task['name']] = {
                    'status': 'success',
                    'output_file': task['output_file'],
                    'result': result.content[0].text
                }
                
                print(f"   ✅ Conversion completed")
                print(f"   Output: {task['output_file']}")
                
            except Exception as e:
                conversion_results[task['name']] = {
                    'status': 'failed',
                    'error': str(e)
                }
                print(f"   ❌ Conversion failed: {str(e)}")
        
        return conversion_results

# Run format conversions
conversion_results = asyncio.run(convert_data_formats())
```

## Step 3: Data Quality Control and Gap Filling

```python
async def quality_control_and_gap_filling():
    """Apply quality control and gap filling to processed data."""
    
    async with create_client("suews-mcp") as client:
        
        # Process each converted file with quality control
        qc_tasks = [
            {
                'name': 'weather_station',
                'file': 'processed/weather_station_suews.txt',
                'expected_issues': ['missing_data', 'outliers'],
                'auto_fix': True
            },
            {
                'name': 'flux_tower', 
                'file': 'processed/flux_tower_suews.txt',
                'expected_issues': ['energy_balance_residual', 'nighttime_fluxes'],
                'auto_fix': True
            },
            {
                'name': 'reanalysis',
                'file': 'processed/reanalysis_suews.txt', 
                'expected_issues': ['temporal_resolution', 'unit_conversion'],
                'auto_fix': True
            }
        ]
        
        qc_results = {}
        
        for task in qc_tasks:
            print(f"\n🔧 Quality control for {task['name']}...")
            
            try:
                # First pass: detailed assessment
                assessment = await client.call_tool("preprocess_forcing", {
                    "input_file": task['file'],
                    "output_file": f"processed/{task['name']}_qc_assessed.txt",
                    "validate_energy_balance": True,
                    "auto_fix_issues": False,  # Just assess first
                    "target_timestep": 3600    # Hourly target
                })
                
                print(f"   📊 Assessment completed")
                
                # Second pass: apply fixes if auto_fix enabled
                if task['auto_fix']:
                    fixing = await client.call_tool("preprocess_forcing", {
                        "input_file": task['file'],
                        "output_file": f"processed/{task['name']}_qc_fixed.txt",
                        "validate_energy_balance": True,
                        "auto_fix_issues": True,
                        "target_timestep": 3600
                    })
                    
                    qc_results[task['name']] = {
                        'assessment': assessment.content[0].text,
                        'fixing': fixing.content[0].text,
                        'final_file': f"processed/{task['name']}_qc_fixed.txt",
                        'status': 'success'
                    }
                    
                    print(f"   ✅ Quality control and fixing completed")
                else:
                    qc_results[task['name']] = {
                        'assessment': assessment.content[0].text,
                        'final_file': f"processed/{task['name']}_qc_assessed.txt",
                        'status': 'assessed_only'
                    }
                    
                    print(f"   ✅ Quality control assessment completed")
                
            except Exception as e:
                qc_results[task['name']] = {
                    'error': str(e),
                    'status': 'failed'
                }
                print(f"   ❌ Quality control failed: {str(e)}")
        
        return qc_results

# Run quality control
qc_results = asyncio.run(quality_control_and_gap_filling())

# Display QC summary
print("\n" + "="*60)
print("QUALITY CONTROL SUMMARY")
print("="*60)

for dataset, result in qc_results.items():
    if result['status'] == 'success':
        print(f"✅ {dataset}:")
        print(f"   Final file: {result['final_file']}")
        print(f"   Issues found and fixed")
    elif result['status'] == 'assessed_only':
        print(f"📊 {dataset}:")
        print(f"   Assessment file: {result['final_file']}")
        print(f"   Manual review recommended")
    else:
        print(f"❌ {dataset}: {result['error']}")
```

## Step 4: Energy Balance Validation

```python
async def validate_energy_balance():
    """Validate energy balance closure for flux tower data."""
    
    print("\n⚖️  Energy Balance Validation")
    print("="*40)
    
    # Load flux tower data for detailed energy balance analysis
    flux_file = 'processed/flux_tower_qc_fixed.txt'
    
    try:
        # Load processed flux data
        df = pd.read_csv(flux_file, delim_whitespace=True, 
                        parse_dates=[[0,1,2,3]], index_col=0)
        
        # Calculate energy balance components
        if all(col in df.columns for col in ['QN', 'QH', 'QE', 'QS']):
            # Energy balance: QN = QH + QE + QS + residual
            df['residual'] = df['QN'] - (df['QH'] + df['QE'] + df['QS'])
            df['closure'] = (df['QH'] + df['QE'] + df['QS']) / df['QN']
            df['bowen_ratio'] = df['QH'] / df['QE']
            
            # Calculate statistics
            stats = {
                'mean_residual': df['residual'].mean(),
                'std_residual': df['residual'].std(),
                'mean_closure': df['closure'].mean(),
                'closure_r2': df[['QN', 'QH', 'QE', 'QS']].corr().iloc[0,1:].mean(),
                'median_bowen': df['bowen_ratio'].median(),
                'data_completeness': (1 - df.isna().mean().mean()) * 100
            }
            
            print(f"Energy Balance Statistics:")
            print(f"  Mean Residual: {stats['mean_residual']:.1f} ± {stats['std_residual']:.1f} W/m²")
            print(f"  Energy Balance Closure: {stats['mean_closure']:.1%}")
            print(f"  Median Bowen Ratio: {stats['median_bowen']:.2f}")
            print(f"  Data Completeness: {stats['data_completeness']:.1f}%")
            
            # Quality assessment
            if abs(stats['mean_residual']) < 20:
                print("  ✅ Energy balance closure: EXCELLENT")
            elif abs(stats['mean_residual']) < 50:
                print("  ✅ Energy balance closure: GOOD")
            else:
                print("  ⚠️  Energy balance closure: NEEDS ATTENTION")
            
            # Create energy balance plot
            create_energy_balance_plot(df)
            
            return stats
            
        else:
            print("❌ Required energy balance variables not found")
            print(f"Available columns: {list(df.columns)}")
            return None
            
    except FileNotFoundError:
        print(f"❌ File not found: {flux_file}")
        return None

def create_energy_balance_plot(df):
    """Create energy balance validation plots."""
    
    fig, axes = plt.subplots(2, 2, figsize=(12, 10))
    
    # Plot 1: Energy balance scatter
    ax1 = axes[0, 0]
    energy_sum = df['QH'] + df['QE'] + df['QS']
    ax1.scatter(df['QN'], energy_sum, alpha=0.6, s=1)
    
    # 1:1 line
    min_val = min(df['QN'].min(), energy_sum.min())
    max_val = max(df['QN'].max(), energy_sum.max())
    ax1.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2)
    
    ax1.set_xlabel('Available Energy QN (W/m²)')
    ax1.set_ylabel('Turbulent Fluxes QH+QE+QS (W/m²)')
    ax1.set_title('Energy Balance Closure')
    ax1.grid(True, alpha=0.3)
    
    # Add R² and slope
    from scipy import stats as scipy_stats
    slope, intercept, r_value, p_value, std_err = scipy_stats.linregress(df['QN'], energy_sum)
    ax1.text(0.05, 0.95, f'R² = {r_value**2:.3f}\nSlope = {slope:.3f}', 
             transform=ax1.transAxes, verticalalignment='top',
             bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    # Plot 2: Residual time series
    ax2 = axes[0, 1]
    daily_residual = df['residual'].resample('D').mean()
    ax2.plot(daily_residual.index, daily_residual.values, linewidth=1)
    ax2.axhline(0, color='red', linestyle='--', alpha=0.7)
    ax2.set_ylabel('Residual (W/m²)')
    ax2.set_title('Energy Balance Residual')
    ax2.grid(True, alpha=0.3)
    
    # Plot 3: Diurnal energy balance pattern
    ax3 = axes[1, 0]
    hourly_mean = df[['QN', 'QH', 'QE', 'QS']].groupby(df.index.hour).mean()
    
    ax3.plot(hourly_mean.index, hourly_mean['QN'], 'k-', linewidth=2, label='QN')
    ax3.plot(hourly_mean.index, hourly_mean['QH'], 'r-', linewidth=2, label='QH')
    ax3.plot(hourly_mean.index, hourly_mean['QE'], 'b-', linewidth=2, label='QE') 
    ax3.plot(hourly_mean.index, hourly_mean['QS'], 'g-', linewidth=2, label='QS')
    
    ax3.set_xlabel('Hour of Day')
    ax3.set_ylabel('Energy Flux (W/m²)')
    ax3.set_title('Mean Diurnal Energy Balance')
    ax3.legend()
    ax3.grid(True, alpha=0.3)
    
    # Plot 4: Closure vs available energy
    ax4 = axes[1, 1]
    closure_binned = df.groupby(pd.cut(df['QN'], bins=20))['closure'].mean()
    bin_centers = [(interval.left + interval.right) / 2 for interval in closure_binned.index]
    
    ax4.plot(bin_centers, closure_binned.values, 'o-', linewidth=2)
    ax4.axhline(1, color='red', linestyle='--', alpha=0.7, label='Perfect closure')
    ax4.set_xlabel('Available Energy QN (W/m²)')
    ax4.set_ylabel('Energy Balance Closure Ratio')
    ax4.set_title('Closure vs Available Energy')
    ax4.legend()
    ax4.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('processed/energy_balance_validation.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    print("📊 Energy balance plots saved: processed/energy_balance_validation.png")

# Run energy balance validation
eb_stats = asyncio.run(validate_energy_balance())
```

## Step 5: Create Final SUEWS-Ready Dataset

```python
async def create_final_dataset():
    """Create final SUEWS-ready dataset from best available data."""
    
    async with create_client("suews-mcp") as client:
        
        print("\n📋 Creating final SUEWS-ready dataset...")
        
        # Define data hierarchy (preference order)
        data_hierarchy = [
            {
                'name': 'flux_tower',
                'file': 'processed/flux_tower_qc_fixed.txt',
                'priority': 1,
                'variables': ['QN', 'QH', 'QE', 'QS', 'Tair', 'RH', 'U', 'WDir'],
                'description': 'High-quality flux tower measurements'
            },
            {
                'name': 'weather_station', 
                'file': 'processed/weather_station_qc_fixed.txt',
                'priority': 2,
                'variables': ['Tair', 'RH', 'U', 'WDir', 'Pres', 'Kdown', 'Rain'],
                'description': 'Standard meteorological measurements'
            },
            {
                'name': 'reanalysis',
                'file': 'processed/reanalysis_qc_fixed.txt',
                'priority': 3,
                'variables': ['Tair', 'RH', 'U', 'Pres', 'Kdown', 'Ldown'],
                'description': 'Gap-filling from reanalysis data'
            }
        ]
        
        # Merge datasets using hierarchical approach
        print("   Merging datasets by priority...")
        merged_data = merge_datasets_by_hierarchy(data_hierarchy)
        
        if merged_data is not None:
            # Save merged dataset
            output_file = 'processed/final_suews_forcing.txt'
            merged_data.to_csv(output_file, sep=' ', float_format='%.3f')
            
            # Final validation
            final_result = await client.call_tool("preprocess_forcing", {
                "input_file": output_file,
                "output_file": 'processed/final_suews_forcing_validated.txt',
                "validate_energy_balance": True,
                "auto_fix_issues": False,
                "target_timestep": 3600
            })
            
            print(f"   ✅ Final dataset created: {output_file}")
            print(f"   📊 Validation results:")
            print(f"      {final_result.content[0].text[:300]}...")
            
            # Create data summary
            create_dataset_summary(merged_data, data_hierarchy)
            
            return output_file
        else:
            print("   ❌ Failed to create merged dataset")
            return None

def merge_datasets_by_hierarchy(data_hierarchy):
    """Merge datasets using hierarchical data quality approach."""
    
    merged_data = None
    data_sources = {}
    
    # Load all available datasets
    for source in data_hierarchy:
        try:
            df = pd.read_csv(source['file'], delim_whitespace=True,
                           parse_dates=[[0,1,2,3]], index_col=0)
            data_sources[source['name']] = {
                'data': df,
                'info': source
            }
            print(f"   ✅ Loaded {source['name']}: {len(df)} records")
        except FileNotFoundError:
            print(f"   ⚠️  Skipped {source['name']}: file not found")
    
    if not data_sources:
        print("   ❌ No datasets available for merging")
        return None
    
    # Create time index from all datasets
    all_times = set()
    for source_data in data_sources.values():
        all_times.update(source_data['data'].index)
    
    time_index = pd.DatetimeIndex(sorted(all_times))
    
    # Required SUEWS variables
    required_vars = ['Tair', 'RH', 'U', 'Pres', 'Kdown', 'Rain']
    optional_vars = ['WDir', 'Ldown', 'QN', 'QH', 'QE', 'QS']
    
    # Initialize merged dataset
    merged_data = pd.DataFrame(index=time_index)
    
    # Merge variables by priority
    for var in required_vars + optional_vars:
        print(f"     Processing {var}...")
        
        for source in data_hierarchy:
            source_name = source['name']
            if source_name in data_sources and var in source['variables']:
                source_df = data_sources[source_name]['data']
                
                if var in source_df.columns:
                    # Fill missing values with higher priority data
                    if var not in merged_data.columns:
                        merged_data[var] = np.nan
                    
                    # Use higher priority data where available
                    mask = merged_data[var].isna()
                    if mask.any():
                        available_data = source_df[var].reindex(merged_data.index)
                        merged_data.loc[mask, var] = available_data.loc[mask]
                        
                        filled_count = (~merged_data[var].isna()).sum() - (~mask).sum()
                        if filled_count > 0:
                            print(f"       Filled {filled_count} values from {source_name}")
    
    # Data completeness check
    completeness = {}
    for var in required_vars:
        if var in merged_data.columns:
            completeness[var] = (1 - merged_data[var].isna().mean()) * 100
        else:
            completeness[var] = 0
    
    print(f"   Data Completeness:")
    for var, pct in completeness.items():
        status = "✅" if pct > 95 else "⚠️" if pct > 80 else "❌"
        print(f"     {status} {var}: {pct:.1f}%")
    
    # Check if minimum requirements met
    min_completeness = min(completeness[var] for var in required_vars if var in completeness)
    if min_completeness < 80:
        print(f"   ⚠️  Warning: Minimum completeness {min_completeness:.1f}% < 80%")
    
    return merged_data

def create_dataset_summary(merged_data, data_hierarchy):
    """Create comprehensive dataset summary report."""
    
    summary_report = f"""
# SUEWS Forcing Data Summary Report

## Dataset Overview

**Final Dataset**: `processed/final_suews_forcing_validated.txt`
**Time Period**: {merged_data.index.min()} to {merged_data.index.max()}
**Duration**: {(merged_data.index.max() - merged_data.index.min()).days} days
**Time Resolution**: {pd.infer_freq(merged_data.index)} (inferred)
**Total Records**: {len(merged_data):,}

## Data Sources

"""
    
    for i, source in enumerate(data_hierarchy, 1):
        summary_report += f"""
### {i}. {source['name'].replace('_', ' ').title()}
- **Priority**: {source['priority']}
- **Description**: {source['description']}
- **File**: `{source['file']}`
- **Variables**: {', '.join(source['variables'])}
"""
    
    summary_report += f"""

## Variable Completeness

| Variable | Description | Completeness | Status |
|----------|-------------|--------------|--------|
"""
    
    var_descriptions = {
        'Tair': 'Air temperature (°C)',
        'RH': 'Relative humidity (%)',
        'U': 'Wind speed (m/s)',
        'WDir': 'Wind direction (degrees)',
        'Pres': 'Atmospheric pressure (kPa)',
        'Kdown': 'Incoming shortwave radiation (W/m²)',
        'Ldown': 'Incoming longwave radiation (W/m²)',
        'Rain': 'Precipitation (mm/h)',
        'QN': 'Net radiation (W/m²)',
        'QH': 'Sensible heat flux (W/m²)',
        'QE': 'Latent heat flux (W/m²)',
        'QS': 'Storage heat flux (W/m²)'
    }
    
    for var in merged_data.columns:
        if var in var_descriptions:
            completeness = (1 - merged_data[var].isna().mean()) * 100
            status = "✅ Complete" if completeness > 95 else "⚠️ Minor gaps" if completeness > 80 else "❌ Major gaps"
            summary_report += f"| {var} | {var_descriptions[var]} | {completeness:.1f}% | {status} |\n"
    
    summary_report += f"""

## Data Quality Metrics

### Temperature Statistics
- **Mean**: {merged_data['Tair'].mean():.1f}°C
- **Range**: {merged_data['Tair'].min():.1f}°C to {merged_data['Tair'].max():.1f}°C
- **Seasonal Variation**: {merged_data['Tair'].max() - merged_data['Tair'].min():.1f}°C

### Wind Statistics  
- **Mean Wind Speed**: {merged_data['U'].mean():.1f} m/s
- **Max Wind Speed**: {merged_data['U'].max():.1f} m/s
- **Calm Conditions**: {(merged_data['U'] < 1.0).mean() * 100:.1f}% (< 1 m/s)

### Radiation Statistics (if available)
"""
    
    if 'Kdown' in merged_data.columns:
        summary_report += f"""
- **Mean Solar Radiation**: {merged_data['Kdown'].mean():.1f} W/m²
- **Max Solar Radiation**: {merged_data['Kdown'].max():.1f} W/m²
- **Clear Sky Days**: {(merged_data['Kdown'] > 800).sum()} hours
"""
    
    if 'QN' in merged_data.columns and 'QH' in merged_data.columns and 'QE' in merged_data.columns:
        bowen_ratio = (merged_data['QH'] / merged_data['QE']).median()
        summary_report += f"""

### Energy Balance (if available)
- **Median Bowen Ratio**: {bowen_ratio:.2f}
- **Mean Net Radiation**: {merged_data['QN'].mean():.1f} W/m²
- **Mean Sensible Heat**: {merged_data['QH'].mean():.1f} W/m²
- **Mean Latent Heat**: {merged_data['QE'].mean():.1f} W/m²
"""
    
    summary_report += f"""

## Recommendations

### For SUEWS Simulation:
1. **Dataset Quality**: {"✅ Excellent" if min([(1 - merged_data[var].isna().mean()) * 100 for var in ['Tair', 'RH', 'U'] if var in merged_data.columns]) > 95 else "✅ Good" if min([(1 - merged_data[var].isna().mean()) * 100 for var in ['Tair', 'RH', 'U'] if var in merged_data.columns]) > 80 else "⚠️ Needs attention"}
2. **Time Period**: Suitable for {"annual" if (merged_data.index.max() - merged_data.index.min()).days > 300 else "seasonal"} analysis
3. **Gap Filling**: {"✅ Minimal gaps" if merged_data.isna().mean().mean() < 0.05 else "⚠️ Consider additional gap filling"}

### Validation Recommendations:
- Compare simulation results with observed fluxes (if available)
- Validate temperature predictions against observations
- Check energy balance closure in model outputs
- Consider parameter calibration if large discrepancies exist

---
*Report generated by SUEWS MCP Server Data Preprocessing Workflow*
"""
    
    # Save summary report
    with open('processed/dataset_summary_report.md', 'w') as f:
        f.write(summary_report)
    
    print(f"📄 Dataset summary saved: processed/dataset_summary_report.md")

# Create final dataset
final_dataset = asyncio.run(create_final_dataset())
```

## Step 6: Validation Against SUEWS Requirements

```python
async def validate_suews_requirements():
    """Validate final dataset against SUEWS requirements."""
    
    async with create_client("suews-mcp") as client:
        
        print("\n🔍 Validating against SUEWS requirements...")
        
        if final_dataset:
            # Comprehensive validation
            validation = await client.call_tool("preprocess_forcing", {
                "input_file": final_dataset,
                "validate_energy_balance": True,
                "auto_fix_issues": False,
                "target_timestep": 3600
            })
            
            print("Final Validation Results:")
            print("=" * 50)
            print(validation.content[0].text)
            
            # Check SUEWS-specific requirements
            df = pd.read_csv(final_dataset, delim_whitespace=True, 
                           parse_dates=[[0,1,2,3]], index_col=0)
            
            suews_requirements = {
                'minimum_variables': ['Tair', 'RH', 'U', 'Pres', 'Kdown'],
                'temperature_range': (-50, 60),  # °C
                'humidity_range': (0, 100),     # %
                'wind_range': (0, 50),          # m/s
                'pressure_range': (80, 110),    # kPa
                'radiation_range': (0, 1500)    # W/m²
            }
            
            validation_results = {}
            
            # Check required variables
            missing_vars = [var for var in suews_requirements['minimum_variables'] 
                          if var not in df.columns]
            validation_results['missing_variables'] = missing_vars
            
            # Check value ranges
            range_issues = []
            if 'Tair' in df.columns:
                temp_range = suews_requirements['temperature_range']
                if df['Tair'].min() < temp_range[0] or df['Tair'].max() > temp_range[1]:
                    range_issues.append(f"Temperature outside range {temp_range}")
            
            if 'RH' in df.columns:
                rh_range = suews_requirements['humidity_range']
                if df['RH'].min() < rh_range[0] or df['RH'].max() > rh_range[1]:
                    range_issues.append(f"Humidity outside range {rh_range}")
            
            validation_results['range_issues'] = range_issues
            
            # Overall assessment
            if not missing_vars and not range_issues:
                print("\n✅ SUEWS REQUIREMENTS: PASSED")
                print("   Dataset ready for SUEWS simulation")
            else:
                print("\n⚠️  SUEWS REQUIREMENTS: ISSUES FOUND")
                if missing_vars:
                    print(f"   Missing variables: {missing_vars}")
                if range_issues:
                    print(f"   Range issues: {range_issues}")
            
            return validation_results
        else:
            print("❌ No final dataset to validate")
            return None

# Run final validation
validation_results = asyncio.run(validate_suews_requirements())
```

## Complete Preprocessing Script

```python
#!/usr/bin/env python3
"""
Complete Data Preprocessing Workflow for SUEWS MCP Server
Process meteorological data from various sources into SUEWS-ready format
"""

import asyncio
from mcp import create_client
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

async def run_complete_preprocessing_workflow():
    """Run complete data preprocessing workflow."""
    
    print("📊 Starting SUEWS Data Preprocessing Workflow")
    print("="*60)
    
    # Step 1: Assess raw data quality
    print("Step 1: Assessing raw data quality...")
    assessment_results = await assess_raw_data()
    
    # Step 2: Convert formats
    print("\nStep 2: Converting data formats...")
    conversion_results = await convert_data_formats()
    
    # Step 3: Quality control and gap filling
    print("\nStep 3: Applying quality control...")
    qc_results = await quality_control_and_gap_filling()
    
    # Step 4: Energy balance validation
    print("\nStep 4: Validating energy balance...")
    eb_stats = await validate_energy_balance()
    
    # Step 5: Create final dataset
    print("\nStep 5: Creating final SUEWS dataset...")
    final_dataset = await create_final_dataset()
    
    # Step 6: Final validation
    print("\nStep 6: Final SUEWS validation...")
    validation_results = await validate_suews_requirements()
    
    print("\n✅ Data preprocessing workflow completed!")
    print("\n📁 Output files:")
    print("  - processed/final_suews_forcing_validated.txt (Ready for SUEWS)")
    print("  - processed/dataset_summary_report.md (Summary report)")
    print("  - processed/energy_balance_validation.png (QC plots)")
    
    return {
        'final_dataset': final_dataset,
        'validation_results': validation_results,
        'energy_balance_stats': eb_stats
    }

if __name__ == "__main__":
    results = asyncio.run(run_complete_preprocessing_workflow())
```

## Expected Outputs

This workflow produces:

1. **Quality-controlled forcing data** in SUEWS format
2. **Data quality assessment reports** with issue identification
3. **Energy balance validation plots** showing closure statistics  
4. **Comprehensive dataset summary** with completeness metrics
5. **SUEWS-ready forcing file** validated against model requirements

## Best Practices

- **Always validate energy balance** for flux tower data
- **Use hierarchical data merging** to maximize data quality
- **Document data sources** and processing steps
- **Check temporal consistency** and resolution requirements
- **Validate against SUEWS requirements** before simulation
- **Keep processing logs** for reproducibility

This systematic approach ensures reliable, high-quality forcing data for SUEWS urban climate simulations.