# SUEWS MCP Server API Reference

Complete documentation for all Model Context Protocol tools provided by the SUEWS MCP Server.

## Table of Contents

1. [Tool Categories](#tool-categories)
2. [Data Processing Tools](#data-processing-tools)
3. [Simulation Tools](#simulation-tools)  
4. [Resource Tools](#resource-tools)
5. [Utility Tools](#utility-tools)
6. [Common Parameters](#common-parameters)
7. [Error Handling](#error-handling)
8. [Response Formats](#response-formats)

## Tool Categories

The SUEWS MCP Server provides **10+ tools** organised into functional categories:

- **Data Processing**: Quality check, validate, and convert meteorological data
- **Simulation**: Configure, run, and manage SUEWS simulations
- **Resource**: Access templates, examples, and documentation
- **Utility**: Server health monitoring and diagnostics

---

# Data Processing Tools

## `preprocess_forcing`

**Preprocess and validate meteorological forcing data for SUEWS simulations**

### Description
Comprehensive data quality assessment tool that validates meteorological forcing data, detects issues, and optionally applies fixes. Essential for ensuring reliable simulation results.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `input_file` | string | ✅ | - | Path to input forcing data file (CSV, TXT, or other formats) |
| `output_file` | string | ❌ | auto-generated | Optional path to save preprocessed data |
| `target_timestep` | integer | ❌ | auto-detect | Target time step in seconds (e.g., 300 for 5-min, 3600 for hourly) |
| `validate_energy_balance` | boolean | ❌ | `true` | Whether to validate energy balance components (QN = QH + QE + QS) |
| `auto_fix_issues` | boolean | ❌ | `false` | Whether to automatically fix common data issues |

### Example Request

```json
{
  "input_file": "data/weather_station_2023.csv",
  "output_file": "processed/forcing_2023.txt",
  "target_timestep": 3600,
  "validate_energy_balance": true,
  "auto_fix_issues": true
}
```

### Example Response

```
Forcing Data Preprocessing Results for: data/weather_station_2023.csv
============================================================

Status: ✓ SUCCESS
Data Shape: (8760, 15)
Issues Found: 3 (Errors: 0, Warnings: 2, Info: 1)

Data Summary:
  • Detected time step: 3600 seconds
  • Median Bowen ratio (QH/QE): 1.25
  • Energy balance residual: 12.5 ± 18.3 W/m²

Data Quality Issues:
  ⚠ WARNING: 12 missing values in RH column (rows 1250-1261)
  ⚠ WARNING: Wind speed values below 0.5 m/s for 8% of records
  ℹ INFO: Temperature range: -5.2°C to 34.8°C (normal for temperate climate)

Processing Steps:
  • Validated temporal consistency
  • Applied energy balance closure checks
  • Fixed 12 missing RH values using interpolation
  • Standardised column names for SUEWS compatibility

✓ Data has been automatically processed and should be ready for simulations.

Processed data saved to: processed/forcing_2023.txt
```

### Validation Checks Performed

1. **Temporal Validation**:
   - Consistent time steps
   - No missing time periods
   - Proper date/time formatting

2. **Value Range Validation**:
   - Temperature: -50°C to +60°C
   - Relative humidity: 0% to 100%
   - Wind speed: ≥0 m/s
   - Radiation: ≥0 W/m²

3. **Energy Balance Validation**:
   - QN = QH + QE + QS closure check
   - Bowen ratio analysis (QH/QE)
   - Energy balance residual statistics

4. **Data Quality Checks**:
   - Missing data detection
   - Outlier identification
   - Temporal consistency checks

---

## `validate_config`

**Comprehensive validation of SUEWS configuration files**

### Description
Validates SUEWS YAML configuration files with detailed error reporting, parameter range checks, and physics option compatibility validation.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `config_file` | string | ✅ | - | Path to SUEWS configuration YAML file to validate |
| `strict_mode` | boolean | ❌ | `false` | Enable strict validation mode with enhanced checks |
| `check_file_paths` | boolean | ❌ | `true` | Whether to validate that referenced files exist |

### Example Request

```json
{
  "config_file": "configs/london_residential.yml",
  "strict_mode": false,
  "check_file_paths": true
}
```

### Example Response

```
SUEWS Configuration Validation Results for: configs/london_residential.yml
======================================================================

Validation Status: ✓ PASSED
Mode: Standard validation
Issues Found: 2 (Errors: 0, Warnings: 2)

⚠️  WARNINGS (Should Review):
  • Building height (8.5m) is below typical residential range (10-15m)
    Location: surface.height_bldgs
  • Anthropogenic heat (15 W/m²) is low for UK residential areas (typical: 20-30 W/m²)
    Location: anthropogenic.qf0_beu

Validation Checklist:
  ✓ Configuration structure
  ✓ Required fields
  ✓ Value ranges
  ✓ Surface fractions
  ✓ File references
  ✓ Physics compatibility

Validation Steps Completed:
  ✓ YAML structure and syntax validation
  ✓ Required field presence check
  ✓ Parameter value range validation
  ✓ Surface fraction sum validation (total: 1.000)
  ✓ File path existence check
  ✓ Physics option compatibility check

✅ CONFIGURATION IS VALID
  • Consider reviewing WARNING items for optimal performance
  • Configuration is ready for simulation
```

### Validation Categories

1. **Structure Validation**:
   - YAML syntax correctness
   - Required sections present
   - Proper nesting structure

2. **Parameter Validation**:
   - Value ranges (e.g., albedo 0.05-0.95)
   - Physical constraints (e.g., fractions 0-1)
   - Unit consistency

3. **Surface Fraction Validation**:
   - All fractions sum to 1.0
   - Individual fractions 0-1
   - Realistic combinations

4. **Physics Compatibility**:
   - Compatible physics options
   - Required parameters for selected methods
   - Initialization value consistency

5. **File Reference Validation**:
   - Forcing file existence
   - Path accessibility
   - File format compatibility

---

## `convert_data_format`

**Convert meteorological data between different formats**

### Description
Convert meteorological data between various formats (CSV, TXT, Excel, NetCDF) with column mapping support and SUEWS-specific format generation.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `input_file` | string | ✅ | - | Path to input data file |
| `output_file` | string | ✅ | - | Path to output data file |
| `input_format` | string | ✅ | - | Input file format: `csv`, `txt`, `excel`, `netcdf` |
| `output_format` | string | ✅ | - | Output file format: `csv`, `txt`, `suews_txt`, `excel`, `netcdf` |
| `column_mapping` | object | ❌ | `{}` | Optional mapping of column names from input to output |

### Example Request

```json
{
  "input_file": "raw_data/weather_station.xlsx", 
  "output_file": "processed/suews_forcing.txt",
  "input_format": "excel",
  "output_format": "suews_txt",
  "column_mapping": {
    "Air_Temperature": "Tair",
    "Relative_Humidity": "RH",
    "Wind_Speed_10m": "U", 
    "Incoming_Solar": "Kdown",
    "Net_Radiation": "QN",
    "Sensible_Heat": "QH",
    "Latent_Heat": "QE"
  }
}
```

### Example Response

```
Data Format Conversion Results
========================================

Status: ✓ SUCCESS
Input: raw_data/weather_station.xlsx (EXCEL)
Output: processed/suews_forcing.txt (SUEWS_TXT)
Shape: (17520, 12) → (17520, 12)
Issues: 0

Column Mapping Applied:
  • 'Air_Temperature' → 'Tair'
  • 'Relative_Humidity' → 'RH'
  • 'Wind_Speed_10m' → 'U'
  • 'Incoming_Solar' → 'Kdown'
  • 'Net_Radiation' → 'QN'
  • 'Sensible_Heat' → 'QH'
  • 'Latent_Heat' → 'QE'

Format Notes:
  • Data formatted for direct use in SUEWS simulations (space-separated)

Conversion Steps:
  ✓ Loaded Excel file with 12 columns
  ✓ Applied column name mapping
  ✓ Validated data types and ranges
  ✓ Formatted output for SUEWS compatibility
  ✓ Added proper headers and metadata

🎉 Conversion completed successfully!
Converted data is ready for use: processed/suews_forcing.txt
```

### Supported Format Combinations

| Input Format | Output Formats | Notes |
|--------------|----------------|-------|
| `csv` | All formats | Most flexible, preserves metadata |
| `txt` | All formats | Space or tab delimited text |
| `excel` | All formats | Reads first sheet by default |
| `netcdf` | All formats | Requires specific variable names |

### Column Mapping Examples

**Common meteorological variable mappings**:

```json
{
  "temperature": "Tair",
  "temp_air": "Tair",
  "air_temperature": "Tair",
  "relative_humidity": "RH", 
  "humidity": "RH",
  "rh": "RH",
  "wind_speed": "U",
  "windspeed": "U",
  "u10": "U",
  "solar_radiation": "Kdown",
  "shortwave": "Kdown",
  "sw_in": "Kdown",
  "net_radiation": "QN",
  "Rn": "QN",
  "sensible_heat": "QH",
  "latent_heat": "QE"
}
```

---

# Simulation Tools

## `configure_simulation`

**Load, configure, validate and save SUEWS simulation parameters**

### Description
Comprehensive tool for loading SUEWS configurations from templates, applying custom updates, validating parameters, and saving configurations. Supports both template-based and custom configuration workflows.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `config_path` | string | ❌ | default template | Path to existing YAML configuration file or template |
| `config_updates` | object | ❌ | `{}` | JSON object with configuration updates to apply |
| `site_name` | string | ❌ | auto-generated | Name identifier for the simulation site |
| `save_path` | string | ❌ | auto-generated | Path to save the configured YAML file |
| `validate_only` | boolean | ❌ | `false` | If true, only validate without saving |

### Example Request

```json
{
  "config_path": "templates/configs/residential.yml",
  "config_updates": {
    "site": {
      "lat": 40.7128,
      "lon": -74.0060, 
      "elevation": 10,
      "timezone": -5
    },
    "surface": {
      "frac_paved": 0.40,
      "frac_bldgs": 0.30,
      "frac_grass": 0.20,
      "frac_trees": 0.10,
      "height_bldgs": 12.0,
      "albedo_paved": 0.15,
      "albedo_bldgs": 0.25
    },
    "anthropogenic": {
      "qf0_beu": 25.0
    },
    "model": {
      "control": {
        "tstep": 300,
        "start_time": "2023-01-01T00:00:00",
        "end_time": "2023-12-31T23:00:00"
      }
    }
  },
  "site_name": "NYC_Manhattan_Residential",
  "save_path": "configs/nyc_manhattan_residential.yml"
}
```

### Example Response

```
SUEWS Configuration Setup Complete
==================================

Site Configuration: NYC_Manhattan_Residential
Status: ✓ SUCCESS
Template Used: templates/configs/residential.yml
Output File: configs/nyc_manhattan_residential.yml

Configuration Updates Applied:
  ✓ Site coordinates: (40.7128°N, 74.0060°W, 10m elevation)
  ✓ Surface fractions: Paved 40%, Buildings 30%, Grass 20%, Trees 10%
  ✓ Building height: 12.0m (updated from template 8.0m)
  ✓ Albedo values: Paved 0.15, Buildings 0.25
  ✓ Anthropogenic heat: 25.0 W/m² (updated from template 20.0 W/m²)
  ✓ Time step: 300 seconds (5-minute intervals)
  ✓ Simulation period: 2023-01-01 to 2023-12-31 (365 days)

Validation Results:
  ✓ All required parameters present
  ✓ Surface fractions sum to 1.00
  ✓ Parameter values within valid ranges
  ✓ Physics options compatible
  ✓ Time period valid

Configuration ready for simulation!
Use 'run_simulation' tool with config_path: "configs/nyc_manhattan_residential.yml"
```

### Configuration Structure

SUEWS configurations are organised in hierarchical sections:

```yaml
site:
  lat: 40.7128           # Latitude (decimal degrees)
  lon: -74.0060          # Longitude (decimal degrees)  
  elevation: 10          # Elevation above sea level (m)
  timezone: -5           # UTC offset (hours)

surface:
  frac_paved: 0.40       # Fraction of paved surfaces (0-1)
  frac_bldgs: 0.30       # Fraction of buildings (0-1)
  frac_grass: 0.20       # Fraction of grass (0-1)
  frac_trees: 0.10       # Fraction of trees (0-1)
  height_bldgs: 12.0     # Average building height (m)
  albedo_paved: 0.15     # Albedo of paved surfaces (0-1)
  albedo_bldgs: 0.25     # Albedo of buildings (0-1)

anthropogenic:
  qf0_beu: 25.0         # Base energy use (W/m²)
  
model:
  control:
    tstep: 300           # Time step (seconds)
    start_time: "2023-01-01T00:00:00"
    end_time: "2023-12-31T23:00:00"
```

---

## `run_simulation`

**Execute SUEWS urban climate model simulations with forcing data**

### Description
Execute complete SUEWS simulations with custom parameters, forcing data, and time filtering. Provides progress tracking, comprehensive output statistics, and integration with both run_supy and SUEWSSimulation APIs.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `config_path` | string | ❌ | template | Path to SUEWS configuration YAML file |
| `forcing_path` | string | ❌ | sample data | Path to meteorological forcing data file |
| `use_sample_data` | boolean | ❌ | `false` | Use built-in sample data instead of forcing_path |
| `start_time` | string | ❌ | from config | Start time in ISO format (YYYY-MM-DDTHH:MM:SS) |
| `end_time` | string | ❌ | from config | End time in ISO format (YYYY-MM-DDTHH:MM:SS) |
| `save_state` | boolean | ❌ | `true` | Whether to save model state for continuation |
| `output_dir` | string | ❌ | "outputs/" | Directory to save simulation outputs |

### Example Request

```json
{
  "config_path": "configs/london_residential.yml",
  "forcing_path": "data/london_weather_2023.txt", 
  "start_time": "2023-06-21T00:00:00",
  "end_time": "2023-09-21T23:00:00",
  "save_state": true,
  "output_dir": "outputs/summer_2023/"
}
```

### Example Response

```
SUEWS Urban Climate Simulation Results
======================================

Simulation: london_residential (Summer 2023)
Status: ✓ COMPLETED SUCCESSFULLY
Runtime: 45.2 seconds

Simulation Details:
  • Configuration: configs/london_residential.yml  
  • Forcing Data: data/london_weather_2023.txt
  • Time Period: 2023-06-21 00:00 to 2023-09-21 23:00 (2208 hours)
  • Time Step: 3600 seconds (hourly)
  • Location: London, UK (51.51°N, 0.13°W, 11m elevation)

Model Configuration:
  • Urban Type: Residential
  • Surface Fractions: 35% paved, 25% buildings, 30% grass, 10% trees
  • Building Height: 12.0m average
  • Anthropogenic Heat: 20.0 W/m² base

Simulation Statistics:
  • Total Time Steps: 2208
  • Energy Balance Variables: QH, QE, QN, QS, QF
  • Meteorological Variables: T2, RH2, U10
  • Success Rate: 100% (all time steps completed)

Key Output Statistics (Summer Period):
  • Mean Air Temperature (T2): 19.2°C (range: 8.1°C to 32.4°C)
  • Mean Sensible Heat Flux (QH): 28.5 W/m² (peak: 145.2 W/m²)
  • Mean Latent Heat Flux (QE): 45.3 W/m² (peak: 220.8 W/m²)
  • Mean Net Radiation (QN): 85.2 W/m² (peak: 420.1 W/m²)
  • Mean Storage Heat Flux (QS): 11.4 W/m² (range: -45.2 to 89.3 W/m²)

Energy Balance Assessment:
  • Mean Residual: 8.2 W/m² (9.6% of QN)
  • Energy Balance Closure: GOOD (residual <10% of QN)
  • Bowen Ratio (QH/QE): 0.63 (typical for well-watered residential)

Output Files:
  ✓ Main results: outputs/summer_2023/london_residential_SUEWS.csv
  ✓ Model state: outputs/summer_2023/london_residential_state.pkl
  ✓ Metadata: outputs/summer_2023/london_residential_metadata.json
  ✓ Configuration: outputs/summer_2023/london_residential_config.yml

Next Steps:
  • Use 'analyze_results' tool for detailed analysis
  • Compare with observational data if available
  • Examine diurnal and seasonal patterns
```

### Output Variables

**Energy Balance Variables**:
- `QH`: Sensible heat flux (W/m²)
- `QE`: Latent heat flux (W/m²)  
- `QN`: Net all-wave radiation (W/m²)
- `QS`: Storage heat flux (W/m²)
- `QF`: Anthropogenic heat flux (W/m²)

**Meteorological Variables**:
- `T2`: Air temperature at 2m (°C)
- `RH2`: Relative humidity at 2m (%)
- `U10`: Wind speed at 10m (m/s)

**Additional Variables** (depending on configuration):
- Soil temperature and moisture
- Surface temperatures
- Evapotranspiration components
- Runoff and drainage

---

## `analyze_results`

**Analyze SUEWS simulation results with statistics, resampling, and visualization**

### Description
Comprehensive analysis tool for SUEWS simulation outputs. Supports multiple analysis types, temporal resampling, energy balance validation, and comparison with reference data.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `results_path` | string | ✅ | - | Path to SUEWS results file (CSV, TXT, Parquet, Pickle) |
| `analysis_type` | string | ❌ | `summary` | Analysis type: `summary`, `statistics`, `energy_balance`, `water_balance`, `temporal` |
| `variables` | array | ❌ | all variables | List of variables to analyze (e.g., ["QH", "QE", "T2"]) |
| `time_period` | string | ❌ | `all` | Temporal aggregation: `hourly`, `daily`, `monthly`, `seasonal`, `annual` |
| `comparison_path` | string | ❌ | none | Optional path to reference data for comparison |

### Example Request

```json
{
  "results_path": "outputs/london_summer_2023/london_residential_SUEWS.csv",
  "analysis_type": "energy_balance", 
  "variables": ["QH", "QE", "QN", "QS", "T2"],
  "time_period": "daily",
  "comparison_path": "observations/london_flux_tower_2023.csv"
}
```

### Example Response

```
SUEWS Simulation Analysis Results
=================================

Results File: outputs/london_summer_2023/london_residential_SUEWS.csv
Analysis Type: Energy Balance Analysis
Time Period: Daily aggregation (92 days)
Variables: QH, QE, QN, QS, T2

Dataset Summary:
  • Simulation Period: 2023-06-21 to 2023-09-21
  • Temporal Resolution: Hourly → Daily aggregation
  • Missing Data: 0.0% (all timestamps complete)
  • Urban Site Type: Residential (London, UK)

Energy Balance Analysis:
  
Daily Mean Values (W/m²):
  • Net Radiation (QN): 127.5 ± 45.2 (range: 42.1 to 235.8)
  • Sensible Heat (QH): 42.3 ± 18.7 (range: 8.5 to 89.4)
  • Latent Heat (QE): 58.9 ± 22.1 (range: 18.2 to 128.7)
  • Storage Heat (QS): 18.1 ± 12.4 (range: -8.2 to 45.1)
  • Energy Residual: 8.2 ± 6.1 (range: -12.4 to 23.8)

Energy Balance Validation:
  ✓ Energy Balance Closure: 93.6% (residual 6.4% of QN)
  ✓ Closure Quality: EXCELLENT (residual <10% of QN)
  ✓ Bowen Ratio: 0.72 ± 0.31 (typical for temperate residential)
  ✓ Evapotranspiration Fraction: 0.46 (QE/QN ratio)

Temporal Patterns:
  • Peak QN: 14:00 UTC (235.8 W/m² maximum)
  • Peak QH: 15:00 UTC (89.4 W/m² maximum)  
  • Peak QE: 13:00 UTC (128.7 W/m² maximum)
  • QS Phase Lag: +2.3 hours relative to QN (typical)

Monthly Trends:
  • June: QN=115.2, QH=38.1, QE=52.4, T2=17.8°C
  • July: QN=142.1, QH=45.8, QE=63.2, T2=20.1°C  
  • August: QN=135.8, QH=43.9, QE=61.1, T2=19.6°C
  • September: QN=116.9, QH=41.2, QE=58.9, T2=18.9°C

Comparison with Observations:
  Reference Data: observations/london_flux_tower_2023.csv
  
  Model vs Observations (RMSE, Bias, R²):
  • QH: RMSE=15.2 W/m², Bias=+3.1 W/m², R²=0.82 ✓ GOOD
  • QE: RMSE=22.4 W/m², Bias=-5.7 W/m², R²=0.74 ✓ ACCEPTABLE
  • QN: RMSE=18.9 W/m², Bias=+1.2 W/m², R²=0.95 ✓ EXCELLENT
  • T2: RMSE=1.8°C, Bias=+0.3°C, R²=0.91 ✓ EXCELLENT

Model Performance Summary:
  ✓ Energy balance closure within acceptable range
  ✓ Temporal patterns realistic and physically consistent
  ✓ Good agreement with observational data
  ✓ Bowen ratio appropriate for residential area
  ✓ Temperature predictions accurate

Recommendations:
  • Model performance is good for this residential site
  • Consider slight adjustment of surface properties to improve QE prediction
  • Validate irrigation settings for summer period if QE is consistently low
  • Model ready for scenario analysis and climate impact studies
```

### Analysis Types

1. **`summary`**: Basic statistics and overview
2. **`statistics`**: Detailed statistical analysis with distributions
3. **`energy_balance`**: Energy balance validation and closure analysis  
4. **`water_balance`**: Water cycle analysis and evapotranspiration
5. **`temporal`**: Time series analysis with patterns and trends

### Time Period Options

- **`hourly`**: No aggregation, full temporal resolution
- **`daily`**: Daily means, min, max, and standard deviation
- **`monthly`**: Monthly statistics and seasonal patterns
- **`seasonal`**: Seasonal analysis (DJF, MAM, JJA, SON)
- **`annual`**: Annual totals and yearly statistics

---

# Resource Tools

## `list_resources`

**List available SUEWS resources (templates, examples, documentation)**

### Description
Browse available SUEWS MCP server resources including configuration templates, workflow documentation, sample data, and examples.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `resource_type` | string | ❌ | `all` | Resource type: `config_template`, `workflow`, `data_sample`, `all` |

### Example Request

```json
{
  "resource_type": "config_template"
}
```

### Example Response

```
Available SUEWS MCP Resources:

Configuration Templates:
  - residential: SUEWS configuration for suburban residential areas with mixed buildings and vegetation
    Path: templates/configs/residential.yml
  - commercial: SUEWS configuration for dense urban commercial/business districts  
    Path: templates/configs/commercial.yml
  - industrial: SUEWS configuration for industrial areas with large buildings and extensive paved surfaces
    Path: templates/configs/industrial.yml
  - park: SUEWS configuration for urban parks and green spaces with high vegetation coverage
    Path: templates/configs/park.yml

Workflow Documentation:
  - quick_start: Complete beginner's guide to SUEWS MCP simulation workflow
    Path: templates/workflows/quick_start.md
  - sensitivity_analysis: Guide for parameter sensitivity analysis and calibration
    Path: templates/workflows/sensitivity_analysis.md
  - validation_workflow: Best practices for model validation against observations
    Path: templates/workflows/validation_workflow.md

Sample Data:
  - sample_forcing: Example meteorological forcing data (London, 2012)
    Path: templates/data/sample_forcing.txt
  - sample_initial: Example initial conditions file
    Path: templates/data/sample_initial.txt

Documentation:
  - documentation_links: Links to official SUEWS and SuPy documentation
    Path: documentation.md
```

---

## `get_resource`

**Get a specific SUEWS resource (template, example, documentation)**

### Description
Retrieve the contents of a specific SUEWS resource file including configuration templates, documentation, sample data, or workflow guides.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `resource_path` | string | ✅ | - | Path to the resource (e.g., "templates/configs/residential.yml") |

### Example Request

```json
{
  "resource_path": "templates/configs/residential.yml"
}
```

### Example Response

```
Resource: templates/configs/residential.yml
===========================================

# SUEWS Configuration Template - Residential Area
# Suitable for suburban residential areas with mixed buildings and vegetation
# Typical characteristics: 25-35% buildings, 20-40% vegetation, moderate energy use

description: "Residential suburban area with mixed buildings and vegetation"

site:
  lat: 51.4769           # London, UK latitude (modify for your location)
  lon: -0.1935           # London, UK longitude  
  elevation: 25          # Elevation above sea level (m)
  timezone: 0            # UTC offset (hours)
  
surface:
  # Surface fractions (must sum to 1.0)
  frac_paved: 0.35       # Paved surfaces (roads, driveways, paths)
  frac_bldgs: 0.25       # Buildings (residential structures)
  frac_grass: 0.30       # Grass/lawn areas
  frac_trees: 0.10       # Trees and large vegetation
  
  # Surface properties
  height_bldgs: 8.0      # Average building height (m)
  albedo_paved: 0.12     # Albedo of paved surfaces (asphalt/concrete)
  albedo_bldgs: 0.20     # Albedo of building surfaces
  albedo_grass: 0.18     # Albedo of grass surfaces  
  albedo_trees: 0.10     # Albedo of tree canopies
  
  # Thermal properties
  heat_capacity_paved: 1.4e6    # Heat capacity of paved surfaces (J/m³/K)
  heat_capacity_bldgs: 1.5e6    # Heat capacity of buildings
  thermal_conductivity_paved: 1.2  # Thermal conductivity (W/m/K)
  thermal_conductivity_bldgs: 1.1

anthropogenic:
  qf0_beu: 20.0         # Base energy use (W/m²) - typical for residential
  
  # Energy use profiles (hourly factors for base energy use)
  weekday_profile: [0.6, 0.5, 0.5, 0.5, 0.6, 0.8, 1.2, 1.5, 1.3, 1.0, 0.9, 0.9, 0.9, 0.9, 1.0, 1.2, 1.5, 1.8, 1.9, 1.7, 1.4, 1.2, 0.9, 0.7]
  weekend_profile: [0.7, 0.6, 0.5, 0.5, 0.6, 0.7, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.4, 1.3, 1.2, 1.2, 1.3, 1.5, 1.6, 1.5, 1.3, 1.1, 1.0, 0.8]

model:
  control:
    tstep: 3600          # Time step (seconds) - hourly
    start_time: "2012-01-01T00:00:00"
    end_time: "2012-12-31T23:00:00"
  
  physics:
    stability_method: 2  # Atmospheric stability calculation method
    roughness_method: 1  # Surface roughness calculation method
    
output:
  variables: ["QH", "QE", "QN", "QS", "QF", "T2", "RH2", "U10"]
  frequency: 3600        # Output frequency (seconds)
```

---

# Utility Tools

## `health_check`

**Check server health and status of active simulations**

### Description
Monitor SUEWS MCP server health, resource usage, and status of active simulations. Useful for monitoring server performance and troubleshooting issues.

### Parameters

None required. This tool accepts an empty parameter object.

### Example Request

```json
{}
```

### Example Response

```
SUEWS MCP Server Health Check:
Status: healthy
Version: 1.0.0
Active simulations: 2/4
Completed simulations: 15
Failed simulations: 1
Available slots: 2
Enabled tools: simulation, validation, analysis
```

### Health Metrics

- **Server Status**: Overall server health
- **Active Simulations**: Current running simulations vs maximum concurrent
- **Completed/Failed**: Historical simulation statistics
- **Available Slots**: Remaining simulation capacity
- **Tool Status**: Which tool categories are enabled

---

# Common Parameters

## Time Format

All time parameters use ISO 8601 format:
- **Format**: `YYYY-MM-DDTHH:MM:SS`
- **Examples**: 
  - `2023-01-01T00:00:00` (New Year midnight)
  - `2023-07-15T14:30:00` (July 15, 2:30 PM)
  - `2023-12-31T23:59:59` (New Year's Eve, end of day)

## File Paths

- Use absolute or relative paths
- Forward slashes work on all platforms
- Examples: `data/forcing.txt`, `/home/user/suews/config.yml`

## Surface Fractions

Must sum to exactly 1.0:
```json
{
  "frac_paved": 0.35,   # 35%
  "frac_bldgs": 0.25,   # 25% 
  "frac_grass": 0.30,   # 30%
  "frac_trees": 0.10    # 10%
  // Total = 1.00
}
```

## Coordinate System

- **Latitude**: Decimal degrees, positive North (-90 to +90)
- **Longitude**: Decimal degrees, positive East (-180 to +180)  
- **Elevation**: Meters above sea level

---

# Error Handling

## Error Response Format

```json
{
  "error": true,
  "error_type": "ValidationError",
  "message": "Surface fractions do not sum to 1.0",
  "details": {
    "current_sum": 0.95,
    "expected_sum": 1.0,
    "difference": -0.05
  },
  "suggestions": [
    "Check that all surface fractions (frac_paved, frac_bldgs, frac_grass, frac_trees) sum to exactly 1.0",
    "Use fractions (0.0-1.0) not percentages (0-100)"
  ]
}
```

## Common Error Types

### `ValidationError`
Configuration or parameter validation failed
```json
{
  "error_type": "ValidationError", 
  "message": "Parameter 'albedo_paved' must be between 0.05 and 0.95",
  "parameter": "surface.albedo_paved",
  "value": 1.2,
  "valid_range": [0.05, 0.95]
}
```

### `FileNotFoundError`
Required files are missing
```json
{
  "error_type": "FileNotFoundError",
  "message": "Forcing data file not found",
  "file_path": "data/missing_file.txt",
  "suggestions": ["Check file path", "Verify file exists", "Use absolute path"]
}
```

### `SimulationError`
Simulation execution failed
```json
{
  "error_type": "SimulationError",
  "message": "SUEWS simulation failed during execution", 
  "stage": "energy_balance_calculation",
  "suggestions": ["Check parameter values", "Validate forcing data", "Review configuration"]
}
```

### `DataFormatError`
Data format or content issues
```json
{
  "error_type": "DataFormatError",
  "message": "Invalid data format in forcing file",
  "line_number": 150,
  "expected_columns": 12,
  "actual_columns": 10
}
```

---

# Response Formats

## Success Response

```json
{
  "success": true,
  "message": "Operation completed successfully", 
  "data": {
    "result_summary": "...",
    "output_files": ["path1", "path2"],
    "statistics": {...}
  },
  "execution_time": 45.2
}
```

## Text Response 

Most tools return formatted text for easy reading:

```
SUEWS Simulation Results
========================

Status: ✓ SUCCESS
Duration: 45.2 seconds
Output: results.csv

Key Results:
  • Energy Balance Closure: 95.2%
  • Mean Temperature: 18.5°C
  • Peak Heat Flux: 125.3 W/m²

Next Steps:
  • Use analyze_results for detailed analysis
  • Compare with observational data
```

## Structured Data

Some tools provide structured JSON data:

```json
{
  "simulation_summary": {
    "duration_seconds": 45.2,
    "time_steps": 8760,
    "success_rate": 1.0
  },
  "energy_balance": {
    "mean_QH": 28.5,
    "mean_QE": 45.3, 
    "mean_QN": 85.2,
    "closure_percent": 95.2
  },
  "output_files": {
    "results": "outputs/results.csv",
    "state": "outputs/state.pkl",
    "metadata": "outputs/metadata.json"
  }
}
```

This completes the SUEWS MCP Server API Reference. For additional help:

- Check the [Quick Start Guide](quickstart.md) for examples
- Review [FAQ](faq.md) for common issues  
- Visit the [SUEWS documentation](https://suews.readthedocs.io/) for model details
- Open an issue on [GitHub](https://github.com/UMEP-dev/SUEWS/issues) for support