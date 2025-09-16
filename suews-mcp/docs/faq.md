# SUEWS MCP Server FAQ & Troubleshooting

Frequently asked questions and troubleshooting guide for the SUEWS MCP Server.

## Table of Contents

1. [Installation & Setup](#installation--setup)
2. [Server Issues](#server-issues)
3. [Configuration Problems](#configuration-problems)
4. [Simulation Failures](#simulation-failures)
5. [Data Processing Issues](#data-processing-issues)
6. [Performance & Optimization](#performance--optimization)
7. [Integration with Other Tools](#integration-with-other-tools)
8. [Advanced Troubleshooting](#advanced-troubleshooting)

---

## Installation & Setup

### Q: Installation fails with "Could not find SUEWS/SuPy"

**A:** The SUEWS MCP server requires SuPy to be installed first.

```bash
# Install SuPy first
pip install supy

# Verify installation
python -c "import supy; print(f'SuPy version: {supy.__version__}')"

# Then install SUEWS MCP
pip install -e .
```

**Common issues:**
- **Python version**: Requires Python 3.9+
- **Build tools**: May need `pip install build setuptools wheel`
- **System dependencies**: Some systems need `gcc` and `gfortran` compilers

### Q: "ModuleNotFoundError: No module named 'mcp'" when starting server

**A:** Install the MCP SDK:

```bash
pip install mcp

# Or if using development dependencies
pip install -e ".[dev]"
```

### Q: Server starts but no tools are available

**A:** Check the server configuration and SuPy installation:

```python
# Verify SuPy MCP tools are available
python -c "
from suews_mcp.handlers import SUEWSMCPHandlers
from suews_mcp.config import MCPServerConfig
handlers = SUEWSMCPHandlers(MCPServerConfig())
print('Handlers initialized successfully')
"
```

If this fails, check:
1. SuPy installation: `python -c "import supy"`
2. SUEWS installation: `python -c "import supy; print(supy.sample_run())"`
3. Path issues: Ensure SUEWS source is in Python path

### Q: How do I update to the latest version?

**A:** Update from the repository:

```bash
cd SUEWS/suews-mcp
git pull origin main
pip install -e . --upgrade
```

---

## Server Issues

### Q: Server won't start - "Port already in use"

**A:** Another process is using the port:

```bash
# Check what's using port 8000
lsof -i :8000

# Kill the process if needed
kill -9 <PID>

# Or start server on different port
suews-mcp --port 8001
```

### Q: Server crashes with "Out of memory" errors

**A:** Reduce memory usage:

1. **Limit concurrent simulations**:
   ```yaml
   # config.yaml
   simulation:
     max_concurrent: 2  # Reduce from default 4
     timeout: 1800      # 30 minutes max per simulation
   ```

2. **Use shorter simulation periods for testing**:
   ```json
   {
     "start_time": "2023-07-01T00:00:00",
     "end_time": "2023-07-07T23:00:00"  // One week instead of full year
   }
   ```

3. **Clear old simulation outputs**:
   ```bash
   rm -rf outputs/*/  # Clear old simulation outputs
   ```

### Q: Server becomes unresponsive

**A:** Check server health and restart if needed:

```python
# Check server health
async with create_client("suews-mcp") as client:
    health = await client.call_tool("health_check", {})
    print(health.content[0].text)
```

If unresponsive:
```bash
# Force restart
pkill -f suews-mcp
suews-mcp --config your_config.yaml
```

### Q: How do I enable debug logging?

**A:** Set logging level in configuration:

```yaml
# config.yaml
logging:
  level: "DEBUG"
  file: "suews_mcp_debug.log"
```

Or set environment variable:
```bash
export SUEWS_MCP_LOG_LEVEL="DEBUG"
suews-mcp
```

---

## Configuration Problems

### Q: "Surface fractions do not sum to 1.0" error

**A:** The most common configuration error. Fix by ensuring fractions sum exactly to 1.0:

```json
{
  "surface": {
    "frac_paved": 0.35,
    "frac_bldgs": 0.25,
    "frac_grass": 0.30,
    "frac_trees": 0.10
    // Total: 0.35 + 0.25 + 0.30 + 0.10 = 1.00 ✓
  }
}
```

**Common mistakes:**
- Using percentages instead of fractions: ❌ `50` → ✅ `0.50`
- Rounding errors: Use exact decimals that sum to 1.0
- Missing surface types: All four fractions must be specified

### Q: "Invalid parameter range" warnings

**A:** Check parameter values against physical limits:

```json
{
  "surface": {
    "albedo_paved": 0.12,    // Valid: 0.05-0.95
    "height_bldgs": 15.0,    // Valid: >0, typically 3-50m
    "thermal_conductivity": 1.2  // Valid: 0.1-3.0 W/m/K
  },
  "anthropogenic": {
    "qf0_beu": 25.0         // Valid: 0-200 W/m²
  }
}
```

**Parameter ranges:**
- **Albedo**: 0.05-0.95 (dimensionless)
- **Building height**: 1-100m (typical: 5-30m)
- **Heat capacity**: 0.5-3.0 × 10⁶ J/m³/K
- **Anthropogenic heat**: 0-200 W/m² (typical: 10-80 W/m²)

### Q: "Incompatible physics options" error

**A:** Some physics methods require specific parameter combinations:

```yaml
model:
  physics:
    stability_method: 2     # Must be 1-3
    roughness_method: 1     # Must be 1-2
    # These must be compatible combinations
```

**Compatible combinations:**
- `stability_method: 1` works with any `roughness_method`
- `stability_method: 2` requires `roughness_method: 1`
- `stability_method: 3` works with any `roughness_method`

### Q: Configuration validation passes but simulation fails

**A:** The issue may be in the forcing data or initial conditions:

1. **Check forcing data**:
   ```python
   await client.call_tool("preprocess_forcing", {
       "input_file": "your_forcing.txt",
       "validate_energy_balance": True,
       "auto_fix_issues": False  # Just check first
   })
   ```

2. **Use sample data for testing**:
   ```json
   {
     "config_path": "your_config.yml",
     "use_sample_data": true  // Test with known-good data
   }
   ```

3. **Check initial conditions**:
   ```yaml
   initial:
     soil_temp: 10.0    # Should match climate
     soil_moisture: 0.3  # 0.1-0.5 typical range
   ```

---

## Simulation Failures

### Q: Simulation fails with "NaN values in output"

**A:** Usually caused by extreme parameter values or bad forcing data:

1. **Check for extreme parameters**:
   ```json
   // These could cause NaN:
   "albedo_paved": 0.01,  // Too low, use ≥0.05
   "qf0_beu": 500.0,      // Too high, use <200
   "height_bldgs": 0.1    // Too low, use ≥1.0
   ```

2. **Validate forcing data**:
   ```bash
   # Check for NaN, infinity, or extreme values
   python -c "
   import pandas as pd
   df = pd.read_csv('forcing.txt', delim_whitespace=True)
   print('NaN values:', df.isna().sum())
   print('Infinite values:', df.isin([float('inf'), float('-inf')]).sum())
   print('Value ranges:', df.describe())
   "
   ```

3. **Use conservative parameter values**:
   ```python
   # Start with template values and make small changes
   await client.call_tool("get_resource", {
       "resource_path": "templates/configs/residential.yml"
   })
   ```

### Q: Simulation stops early or crashes

**A:** Check simulation logs and memory usage:

1. **Check server logs**:
   ```bash
   tail -f suews_mcp.log
   ```

2. **Reduce simulation complexity**:
   ```json
   {
     "start_time": "2023-01-01T00:00:00",
     "end_time": "2023-01-07T23:00:00",  // One week first
     "save_state": true
   }
   ```

3. **Check system resources**:
   ```bash
   # Memory usage
   free -h
   
   # CPU usage
   top -p $(pgrep -f suews-mcp)
   ```

### Q: Results look unrealistic (extreme temperatures/fluxes)

**A:** Usually indicates parameter or forcing data issues:

1. **Energy balance check**:
   ```python
   await client.call_tool("analyze_results", {
       "results_path": "outputs/results.csv",
       "analysis_type": "energy_balance"
   })
   ```
   - Energy balance closure should be 80-120%
   - Bowen ratio (QH/QE) should be 0.1-5.0 for most urban areas

2. **Compare with expected ranges**:
   ```python
   # Typical ranges for temperate urban areas:
   # QH: 0-200 W/m² (peak ~100-150 W/m²)
   # QE: 0-300 W/m² (peak ~200-250 W/m²)  
   # T2: Local climate ±5°C
   # QS: -50 to +100 W/m²
   ```

3. **Check anthropogenic heat**:
   ```json
   {
     "anthropogenic": {
       "qf0_beu": 25.0  // Start conservative, typical 15-40 W/m²
     }
   }
   ```

### Q: Simulation runs but produces constant values

**A:** Check forcing data and time settings:

1. **Verify forcing data has variation**:
   ```bash
   python -c "
   import pandas as pd
   df = pd.read_csv('forcing.txt', delim_whitespace=True)
   print('Temperature variation:', df['Tair'].max() - df['Tair'].min())
   print('Radiation variation:', df['Kdown'].max() - df['Kdown'].min())
   "
   ```

2. **Check time step settings**:
   ```yaml
   model:
     control:
       tstep: 3600  # 1 hour (not 1 second!)
   ```

3. **Verify date range**:
   ```json
   {
     "start_time": "2023-01-01T00:00:00",  // ISO format
     "end_time": "2023-12-31T23:00:00"     // Full year
   }
   ```

---

## Data Processing Issues

### Q: "File format not supported" error

**A:** Check supported formats and file structure:

**Supported formats:**
- **CSV**: Comma-separated values
- **TXT**: Space/tab delimited text
- **Excel**: `.xlsx` files (first sheet used)
- **NetCDF**: `.nc` files with standard variable names

**File structure requirements:**
```
# SUEWS format example (space-separated):
Year DOY Hour Min Tair RH U WDir Pres Kdown Rain
2023   1    0   0 12.5 65 2.3  180  101.3  0   0.0
2023   1    1   0 12.2 68 2.1  185  101.2  0   0.0
```

### Q: Column mapping not working

**A:** Use exact column names and check the mapping:

```python
# Check column names first
import pandas as pd
df = pd.read_csv('data.csv')
print("Available columns:", df.columns.tolist())

# Then create accurate mapping
column_mapping = {
    "Air_Temperature_C": "Tair",      # Exact name from file
    "Relative_Humidity_Percent": "RH", 
    "Wind_Speed_ms": "U"
}
```

**Common mapping issues:**
- Column names with spaces or special characters
- Case sensitivity: `Temperature` ≠ `temperature`
- Hidden characters or encoding issues

### Q: Energy balance validation fails

**A:** Common energy balance issues and solutions:

1. **Missing energy balance components**:
   ```python
   # Required for energy balance validation:
   required = ['QN', 'QH', 'QE']  # QS optional
   
   # If missing, energy balance validation will fail
   ```

2. **Units mismatch**:
   ```python
   # All energy fluxes must be in W/m²
   # Common conversions:
   # kW/m² → W/m²: multiply by 1000
   # cal/cm²/min → W/m²: multiply by 697.8
   ```

3. **Unrealistic values**:
   ```python
   # Typical ranges:
   # QN: -100 to +800 W/m² (daily range)
   # QH: -50 to +300 W/m² (can be negative at night)
   # QE: 0 to +400 W/m² (rarely negative)
   ```

### Q: Gap filling fails with many missing values

**A:** Handle large data gaps appropriately:

1. **Check data completeness**:
   ```python
   df = pd.read_csv('data.csv')
   completeness = (1 - df.isna().mean()) * 100
   print("Completeness by variable:", completeness)
   ```

2. **Requirements for gap filling**:
   - **<5% missing**: Auto-fixing usually works well
   - **5-20% missing**: May need manual review
   - **>20% missing**: Consider alternative data sources

3. **Alternative approaches**:
   ```python
   # Use reanalysis data for large gaps
   await client.call_tool("convert_data_format", {
       "input_file": "era5_data.nc",
       "output_file": "gap_filler.txt",
       "input_format": "netcdf",
       "output_format": "suews_txt"
   })
   ```

### Q: Time series has irregular time steps

**A:** SUEWS requires regular time intervals:

```python
# Check time step consistency
df = pd.read_csv('data.txt', parse_dates=[0], index_col=0)
time_diffs = df.index.to_series().diff().dt.total_seconds()
print("Time step variation:", time_diffs.describe())

# Should be constant (e.g., 3600 for hourly)
```

**Solutions:**
1. **Resample to regular intervals**:
   ```python
   df_resampled = df.resample('1H').mean()  # Hourly
   ```

2. **Use preprocessing tool**:
   ```python
   await client.call_tool("preprocess_forcing", {
       "input_file": "irregular_data.csv",
       "target_timestep": 3600,  # Force hourly
       "auto_fix_issues": True
   })
   ```

---

## Performance & Optimization

### Q: Simulations are very slow

**A:** Several optimization strategies:

1. **Reduce time step for testing**:
   ```yaml
   model:
     control:
       tstep: 3600  # Hourly instead of 300 (5-min)
   ```

2. **Use shorter simulation periods**:
   ```json
   {
     "start_time": "2023-07-01T00:00:00",
     "end_time": "2023-07-31T23:00:00"  // One month
   }
   ```

3. **Limit concurrent simulations**:
   ```yaml
   simulation:
     max_concurrent: 2  # Instead of 4
   ```

4. **Use sample data for development**:
   ```json
   {
     "use_sample_data": true  // Faster than large forcing files
   }
   ```

### Q: High memory usage

**A:** Memory optimization techniques:

1. **Clear old outputs**:
   ```bash
   find outputs/ -name "*.csv" -mtime +7 -delete  # Delete files >7 days old
   ```

2. **Reduce output frequency**:
   ```yaml
   output:
     frequency: 3600  # Hourly instead of every time step
   ```

3. **Process results in chunks**:
   ```python
   # Instead of loading full year at once
   for month in range(1, 13):
       df_month = pd.read_csv(f'outputs/results_month_{month}.csv')
       # Process month by month
   ```

### Q: Server response is slow

**A:** Performance tuning:

1. **Check server health**:
   ```python
   await client.call_tool("health_check", {})
   ```

2. **Restart server periodically**:
   ```bash
   # In crontab: restart daily at 2 AM
   0 2 * * * pkill -f suews-mcp && suews-mcp --config /path/to/config.yaml
   ```

3. **Monitor system resources**:
   ```bash
   htop  # Check CPU and memory usage
   ```

---

## Integration with Other Tools

### Q: How do I use with Jupyter notebooks?

**A:** Install Jupyter and create async cells:

```python
# In Jupyter notebook
import asyncio
from mcp import create_client

# For notebooks, need to handle event loop
import nest_asyncio
nest_asyncio.apply()

async def run_suews_analysis():
    async with create_client("suews-mcp") as client:
        result = await client.call_tool("run_simulation", {
            "use_sample_data": True
        })
        return result

# Run in notebook
result = await run_suews_analysis()
print(result.content[0].text)
```

### Q: Integration with pandas/matplotlib

**A:** Load SUEWS results into pandas for analysis:

```python
import pandas as pd
import matplotlib.pyplot as plt

# Load simulation results
df = pd.read_csv('outputs/simulation_results.csv', 
                 parse_dates=[0], index_col=0)

# Plot energy balance
fig, ax = plt.subplots(figsize=(12, 6))
df[['QH', 'QE', 'QN', 'QS']].plot(ax=ax)
ax.set_ylabel('Energy Flux (W/m²)')
ax.set_title('SUEWS Energy Balance Time Series')
plt.tight_layout()
plt.show()
```

### Q: Exporting results to other formats

**A:** Use pandas for format conversion:

```python
# Load SUEWS results
df = pd.read_csv('outputs/results.csv')

# Export to different formats
df.to_excel('results.xlsx', index=False)
df.to_parquet('results.parquet')  # Efficient for large datasets
df.to_hdf('results.h5', key='suews_data')  # For time series

# Export summary statistics
summary = df.describe()
summary.to_csv('summary_statistics.csv')
```

### Q: Using with GIS software

**A:** Export spatial data and results:

```python
# Create spatial dataset with coordinates
df_spatial = df.copy()
df_spatial['lat'] = 51.5074  # From config
df_spatial['lon'] = -0.1278

# Export for QGIS/ArcGIS
df_spatial.to_csv('spatial_results.csv', index=False)

# Or use geopandas for proper spatial format
import geopandas as gpd
from shapely.geometry import Point

geometry = [Point(xy) for xy in zip(df_spatial['lon'], df_spatial['lat'])]
gdf = gpd.GeoDataFrame(df_spatial, geometry=geometry)
gdf.to_file('results.geojson', driver='GeoJSON')
```

---

## Advanced Troubleshooting

### Q: How do I debug internal SUEWS model errors?

**A:** Enable detailed SUEWS logging:

1. **Check SuPy logs**:
   ```python
   import supy
   import logging
   
   # Enable SuPy debug logging
   logging.basicConfig(level=logging.DEBUG)
   
   # Run simple test
   result = supy.sample_run()
   print("SuPy test successful:", result is not None)
   ```

2. **Check SUEWS source compilation**:
   ```bash
   # Find SUEWS installation
   python -c "import supy; print(supy.__file__)"
   
   # Check if Fortran modules compiled correctly
   python -c "import supy.suews_driver as sd; print('SUEWS driver loaded')"
   ```

3. **Run minimal SUEWS test**:
   ```python
   import supy as sp
   
   # Load sample data
   df_forcing, df_state = sp.load_SampleData()
   print("Sample data shape:", df_forcing.shape)
   
   # Run one time step
   df_output, df_state_final = sp.run_supy(df_forcing.iloc[[0]], df_state)
   print("Single step output:", df_output.shape)
   ```

### Q: Configuration works in SuPy but not MCP server

**A:** Check parameter translation between MCP and SuPy:

```python
# Test configuration loading directly
import yaml

# Load your MCP config
with open('your_config.yml', 'r') as f:
    mcp_config = yaml.safe_load(f)

# Try loading in SuPy
import supy as sp
try:
    df_forcing, df_state = sp.load_SampleData()
    # Manually apply your config parameters
    df_output, df_state_final = sp.run_supy(df_forcing.iloc[:24], df_state)
    print("Direct SuPy run successful")
except Exception as e:
    print(f"Direct SuPy failed: {e}")
```

### Q: MCP client connection issues

**A:** Debug the MCP connection:

```python
import asyncio
from mcp import create_client

async def debug_connection():
    try:
        async with create_client("suews-mcp") as client:
            print("✅ MCP client connected successfully")
            
            # Test basic functionality
            tools = await client.list_tools()
            print(f"✅ Available tools: {len(tools.tools)}")
            
            # Test simple tool call
            health = await client.call_tool("health_check", {})
            print("✅ Health check successful")
            
    except Exception as e:
        print(f"❌ Connection failed: {e}")
        print("Troubleshooting steps:")
        print("1. Check if server is running: ps aux | grep suews-mcp")
        print("2. Check port: lsof -i :8000")  
        print("3. Check logs: tail -f suews_mcp.log")

asyncio.run(debug_connection())
```

### Q: Inconsistent results between runs

**A:** Check for non-deterministic behavior:

1. **Random seed issues**:
   ```yaml
   model:
     control:
       random_seed: 42  # Fixed seed for reproducibility
   ```

2. **Floating point precision**:
   ```python
   # Check if results are identical
   import pandas as pd
   
   df1 = pd.read_csv('run1_results.csv')
   df2 = pd.read_csv('run2_results.csv')
   
   diff = (df1 - df2).abs().max()
   print("Maximum difference:", diff)
   
   # Should be <1e-10 for identical runs
   ```

3. **State initialization**:
   ```json
   {
     "save_state": true,  // Ensure consistent initial conditions
     "start_time": "2023-01-01T00:00:00"  // Same start time
   }
   ```

---

## Getting Help

If these solutions don't resolve your issue:

### 1. Check Server Logs
```bash
tail -f suews_mcp.log
# Look for ERROR or WARNING messages
```

### 2. Enable Debug Mode
```bash
export SUEWS_MCP_LOG_LEVEL="DEBUG"
suews-mcp
```

### 3. Create Minimal Reproduction
```python
# Create simplest possible failing case
async def minimal_test():
    async with create_client("suews-mcp") as client:
        result = await client.call_tool("run_simulation", {
            "use_sample_data": True,
            "start_time": "2012-01-01T00:00:00", 
            "end_time": "2012-01-01T23:00:00"  # Just one day
        })
        print(result.content[0].text)
```

### 4. Collect System Information
```bash
# System info
python --version
pip list | grep -i suews
pip list | grep -i mcp

# Server status  
ps aux | grep suews-mcp
lsof -i :8000
df -h  # Disk space
free -h  # Memory usage
```

### 5. Open GitHub Issue

Include in your issue:
- **Error message** (full text)
- **Configuration file** (anonymize sensitive data)
- **System information** from above
- **Steps to reproduce** the problem
- **Expected vs actual behavior**

**GitHub Issues**: https://github.com/UMEP-dev/SUEWS/issues

### 6. Community Support

- **SUEWS Documentation**: https://suews.readthedocs.io/
- **SuPy Documentation**: https://supy.readthedocs.io/
- **GitHub Discussions**: https://github.com/UMEP-dev/SUEWS/discussions
- **UMEP Community**: https://umep-docs.readthedocs.io/

Remember: Most issues have simple solutions. Check the basics first (installation, file paths, parameter values) before assuming complex problems!