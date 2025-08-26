# SUEWS Data Sources and Sample Data

This document provides information about data requirements for SUEWS simulations and links to sample datasets that can be used with the MCP server.

## Required Input Data

### Meteorological Forcing Data

**Essential Variables:**
- **Air temperature** (Ta) [°C]
- **Relative humidity** (RH) [%] 
- **Atmospheric pressure** (Pres) [hPa]
- **Precipitation** (Rain) [mm]
- **Incoming shortwave radiation** (Kdn) [W/m²]
- **Incoming longwave radiation** (Ldown) [W/m²]
- **Wind speed** (Ws) [m/s]

**Optional but Recommended:**
- Wind direction (Wd) [degrees]
- Diffuse shortwave radiation (Kdiff) [W/m²]
- Direct shortwave radiation (Kdir) [W/m²]
- Cloud fraction (fcld) [-]
- External water use (Wuh) [-]
- Soil moisture (xsmd) [-]
- Leaf Area Index (lai) [-]

**Data Format Requirements:**
- Text file with space-separated values
- Temporal resolution: 5-60 minutes typical
- Missing data: Use -999
- Date/time columns: Year, DOY (day of year), Hour, Minute

### Site Parameter Data

**Surface Characteristics:**
- Land cover fractions (must sum to 1.0)
- Building heights and morphology
- Surface material properties (albedo, emissivity, thermal properties)
- Vegetation characteristics (LAI, conductance parameters)

**Location Information:**
- Latitude and longitude [decimal degrees]
- Elevation above sea level [metres]
- Timezone [hours from UTC]
- Surface area [m²]

## Sample Data Available via MCP

### Configuration Templates
Access via MCP `get_resource` tool:
- `templates/configs/residential.yml` - Residential area template
- `templates/configs/commercial.yml` - Commercial/downtown template
- `templates/configs/industrial.yml` - Industrial area template  
- `templates/configs/park.yml` - Urban park template

### Sample Forcing Data
- `templates/data/sample_forcing.txt` - Minimal example meteorological data
- `templates/data/sample_initial.txt` - Example initial conditions

### Workflow Documentation
- `templates/workflows/quick_start.md` - Getting started guide
- `templates/workflows/sensitivity_analysis.md` - Parameter testing guide
- `templates/workflows/validation_workflow.md` - Model validation guide

## External Data Sources

### Meteorological Data

**Global Reanalysis Data:**
- **ERA5**: High resolution reanalysis from ECMWF
  - Variables: All required meteorological variables
  - Resolution: ~31km globally, hourly
  - Access: Copernicus Climate Data Store (CDS)
  - URL: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels

- **MERRA-2**: NASA's reanalysis dataset
  - Variables: Complete meteorological suite
  - Resolution: 0.5° × 0.625°, hourly
  - URL: https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/

**National Weather Services:**
- **NOAA/NCEI**: US weather station data
  - URL: https://www.ncei.noaa.gov/data/
- **Met Office**: UK weather data
  - URL: https://www.metoffice.gov.uk/research/climate/maps-and-data
- **Environment Canada**: Canadian weather data
  - URL: https://climate.weather.gc.ca/

**Research Networks:**
- **FLUXNET**: Eddy covariance flux measurements
  - Excellent for model validation
  - URL: https://fluxnet.fluxdata.org/
- **AmeriFlux**: North American flux network
  - URL: https://ameriflux.lbl.gov/
- **ICOS**: European greenhouse gas observation network
  - URL: https://www.icos-cp.eu/

### Urban Morphology Data

**Building and Surface Information:**
- **OpenStreetMap**: Global building footprints and land use
  - URL: https://www.openstreetmap.org/
- **Microsoft Building Footprints**: AI-derived building polygons
  - URL: https://github.com/Microsoft/USBuildingFootprints
- **World Urban Database (WUDAPT)**: Local Climate Zones
  - URL: http://www.wudapt.org/

**Topographic and Land Cover:**
- **Copernicus Land Monitoring Service**: European land cover
  - URL: https://land.copernicus.eu/
- **USGS Land Cover**: US land cover products
  - URL: https://www.usgs.gov/centers/eros/science/national-land-cover-database
- **MODIS Land Cover**: Global land cover classification
  - URL: https://modis.gsfc.nasa.gov/data/dataprod/mod12.php

### Population and Activity Data

**Population Density:**
- **WorldPop**: High-resolution population data
  - URL: https://www.worldpop.org/
- **LandScan**: Global population distribution
  - URL: https://landscan.ornl.gov/

**Energy Use Data:**
- **International Energy Agency**: National energy statistics
  - URL: https://www.iea.org/data-and-statistics
- **Local utility companies**: Often provide aggregate consumption data
- **Building energy benchmarking programs**: City-specific data

## SuPy Built-in Sample Data

### Benchmark Dataset
SuPy includes built-in sample data accessible through:

```python
import supy as sp

# Load sample configuration and data
df_forcing, df_state_init = sp.load_SampleData()

# Sample configuration
sample_config = sp.load_SampleConfig()
```

**Sample Data Characteristics:**
- Location: King's College London, UK
- Period: 2011-2013
- Temporal resolution: 5 minutes  
- Surface type: Mixed urban (residential/commercial)
- Includes: Complete meteorological forcing and validation outputs

### Benchmark Configuration
- Pre-configured for London urban site
- Validated parameter set
- Suitable for testing and learning
- Available via: `sp.load_SampleConfig()`

## Data Quality Requirements

### Meteorological Data Quality
- **Completeness**: <10% missing data preferred
- **Temporal consistency**: Regular time intervals
- **Physical realism**: Check for outliers and impossible values
- **Energy balance**: Radiation components should be consistent
- **Diurnal patterns**: Temperature, humidity, radiation should show realistic cycles

### Site Parameter Quality  
- **Surface fractions**: Must sum exactly to 1.0
- **Physical constraints**: Albedo 0.05-0.95, building heights >0
- **Local representativeness**: Parameters should match actual site conditions
- **Consistency**: Related parameters should be physically consistent

## Data Preparation Tools

### Processing Meteorological Data
```python
# Example data preparation workflow
import pandas as pd
import numpy as np

# Load raw meteorological data
df_met = pd.read_csv('raw_met_data.csv')

# Check for missing data
missing_summary = df_met.isnull().sum()

# Fill missing values (example - use appropriate methods)
df_met = df_met.fillna(method='linear')

# Convert to SUEWS format
# Add Year, DOY, Hour, Minute columns
# Ensure column order matches SUEWS requirements
```

### Using MCP Tools for Data Preparation
```python
# Validate data quality using MCP tools
validation_result = await client.call_tool("validate_suews_config", {
    "config_file": "my_config.yml",
    "strict": True
})

# Get sample data for comparison
sample_data = await client.call_tool("get_resource", {
    "resource_path": "templates/data/sample_forcing.txt"
})
```

## Data Citation and Usage

When using external datasets in your research:

1. **Always cite data sources** appropriately in publications
2. **Check usage licenses** - some data requires registration or has restrictions
3. **Follow attribution requirements** for derived products
4. **Document data processing** steps for reproducibility
5. **Archive processed datasets** for future use and sharing

## Recommended Data Workflow

1. **Identify requirements** based on your study objectives
2. **Locate appropriate data sources** from the options above
3. **Download and assess data quality** before processing
4. **Process and format** according to SUEWS requirements
5. **Validate** using MCP validation tools
6. **Test** with short simulation periods
7. **Archive** processed data with documentation

For specific guidance on data preparation for your study area and objectives, use the MCP prompt tools:
- `setup_simulation` - Get guidance for your specific urban type and location  
- `troubleshoot_errors` - Help with data format or quality issues
- `parameter_tuning` - Optimize parameters based on available data

## Contact and Support

For questions about data sources or preparation:
- **SUEWS Community**: GitHub Discussions
- **SuPy Documentation**: https://supy.readthedocs.io/
- **UMEP Support**: https://umep-docs.readthedocs.io/
- **MCP Tools**: Use built-in help via `health_check` and prompt tools