# SUEWS MCP Data Preprocessing Tools

The SUEWS MCP server includes comprehensive data preprocessing and validation tools to help prepare meteorological forcing data and configuration files for SUEWS simulations.

## Available Tools

### 1. `preprocess_forcing` - Meteorological Data Preprocessing

Preprocesses meteorological forcing data with comprehensive quality checks and validation.

**Parameters:**
- `input_file` (required): Path to input forcing data file (CSV, TXT, or other formats)
- `output_file` (optional): Path to save preprocessed data
- `target_timestep` (optional): Target time step in seconds (auto-detected if not provided)
- `validate_energy_balance` (default: true): Whether to validate energy balance components
- `auto_fix_issues` (default: false): Whether to automatically fix common data issues

**Features:**
- ✅ **Data Structure Validation**: Checks for required columns and data types
- ✅ **Time Series Validation**: Validates temporal consistency and detects gaps
- ✅ **Range Validation**: Ensures values are within physically reasonable ranges
- ✅ **Missing Data Detection**: Identifies missing data patterns and consecutive gaps
- ✅ **Energy Balance Validation**: Checks QN = QH + QE + QS closure
- ✅ **Unit Conversions**: Automatic unit conversion (e.g., hPa to kPa for pressure)
- ✅ **Data Quality Issues**: Comprehensive reporting with severity levels
- ✅ **Auto-fix Capabilities**: Can automatically fix common issues like negative solar radiation

**Example Usage:**
```json
{
  "input_file": "/path/to/forcing_data.txt",
  "output_file": "/path/to/processed_data.txt",
  "validate_energy_balance": true,
  "auto_fix_issues": true
}
```

### 2. `validate_config` - Configuration File Validation

Comprehensive validation of SUEWS configuration files with detailed error reporting.

**Parameters:**
- `config_file` (required): Path to SUEWS configuration YAML file to validate
- `strict_mode` (default: false): Enable strict validation mode with enhanced checks
- `check_file_paths` (default: true): Whether to validate that referenced files exist

**Features:**
- ✅ **Structure Validation**: Checks required sections and overall structure
- ✅ **Required Fields**: Validates presence of mandatory configuration fields
- ✅ **Value Range Validation**: Ensures parameters are within valid ranges
- ✅ **Surface Fraction Validation**: Verifies surface fractions sum to 1.0
- ✅ **Physics Option Compatibility**: Checks for incompatible physics combinations
- ✅ **File Path Validation**: Verifies referenced files exist
- ✅ **Geographic Validation**: Validates latitude/longitude coordinates
- ✅ **Time Settings**: Validates temporal configuration parameters

**Example Usage:**
```json
{
  "config_file": "/path/to/suews_config.yml",
  "strict_mode": false,
  "check_file_paths": true
}
```

### 3. `convert_data_format` - Data Format Conversion

Convert meteorological data between different formats with column mapping support.

**Parameters:**
- `input_file` (required): Path to input data file
- `output_file` (required): Path to output data file
- `input_format` (required): Input file format (`csv`, `txt`, `excel`, `netcdf`)
- `output_format` (required): Output file format (`csv`, `txt`, `suews_txt`, `excel`, `netcdf`)
- `column_mapping` (optional): Mapping of column names from input to output format

**Supported Formats:**
- **CSV**: Comma-separated values
- **TXT**: Space-separated text files
- **SUEWS_TXT**: SUEWS-specific format (space-separated with proper column names)
- **Excel**: Excel spreadsheets (.xlsx)
- **NetCDF**: Network Common Data Form (requires xarray)

**Features:**
- ✅ **Multiple Format Support**: Handle various input and output formats
- ✅ **Column Mapping**: Rename columns during conversion
- ✅ **Format-specific Transformations**: Apply appropriate formatting for target format
- ✅ **Data Preservation**: Maintain data integrity during conversion
- ✅ **SUEWS Format Support**: Direct conversion to SUEWS-ready format

**Example Usage:**
```json
{
  "input_file": "/path/to/data.csv",
  "output_file": "/path/to/suews_data.txt",
  "input_format": "csv",
  "output_format": "suews_txt",
  "column_mapping": {
    "temperature": "Tair",
    "humidity": "RH",
    "wind_speed": "U",
    "solar_radiation": "kdown"
  }
}
```

## Data Quality Assessment

The preprocessing tools provide detailed data quality assessment with three severity levels:

### ❌ Errors (Must Fix)
- Missing required columns
- Invalid data types
- Values completely outside physical ranges
- Configuration structure problems
- Missing required configuration fields

### ⚠️ Warnings (Should Review)
- Values at the edge of expected ranges
- Missing data in non-critical variables
- Surface fractions that don't sum exactly to 1.0
- Unusual physics option combinations

### ℹ️ Information (Good to Know)
- Data statistics and summaries
- Processing steps completed
- Detected time steps and patterns
- Minor data quality observations

## Typical Workflow

1. **Format Conversion** (if needed):
   ```json
   {
     "tool": "convert_data_format",
     "input_format": "csv",
     "output_format": "suews_txt",
     "column_mapping": {"temp": "Tair", "humidity": "RH"}
   }
   ```

2. **Data Preprocessing**:
   ```json
   {
     "tool": "preprocess_forcing",
     "validate_energy_balance": true,
     "auto_fix_issues": true
   }
   ```

3. **Configuration Validation**:
   ```json
   {
     "tool": "validate_config",
     "strict_mode": false,
     "check_file_paths": true
   }
   ```

## SUEWS Variables Reference

### Required Time Variables
- `iy`: Year
- `id`: Day of year (1-366)
- `it`: Hour (0-23)
- `imin`: Minute (0-59)

### Meteorological Variables
- `Tair`: Air temperature [°C]
- `RH`: Relative humidity [%]
- `U`: Wind speed [m/s]
- `pres`: Atmospheric pressure [kPa]
- `rain`: Rainfall [mm]
- `kdown`: Shortwave radiation downward [W/m²]

### Energy Balance Variables (Optional)
- `qn`: Net all-wave radiation [W/m²]
- `qh`: Sensible heat flux [W/m²]
- `qe`: Latent heat flux [W/m²]
- `qs`: Storage heat flux [W/m²]
- `qf`: Anthropogenic heat flux [W/m²]

### Additional Variables (Optional)
- `snow`: Snowfall [mm]
- `ldown`: Longwave radiation downward [W/m²]
- `fcld`: Cloud fraction [0-1]
- `wuh`: External water use [mm]
- `xsmd`: Soil moisture deficit [fraction]
- `lai`: Leaf area index [m²/m²]
- `kdiff`: Diffuse shortwave radiation [W/m²]
- `kdir`: Direct shortwave radiation [W/m²]
- `wdir`: Wind direction [degrees]

## Error Handling

The preprocessing tools provide robust error handling:

- **File Access Errors**: Clear messages for missing or inaccessible files
- **Format Errors**: Detailed information about data format issues
- **Validation Errors**: Specific guidance on how to fix configuration problems
- **Processing Errors**: Graceful handling of unexpected data conditions

## Best Practices

1. **Always validate configurations** before running simulations
2. **Use strict mode** for production configurations
3. **Enable auto-fix** for common data issues
4. **Review warnings** even if processing succeeds
5. **Check energy balance** if you have flux data
6. **Verify time series continuity** for long simulations
7. **Use consistent units** throughout your data

## Integration with SUEWS

The preprocessed data and validated configurations are ready for use with:
- SuPy Python interface
- SUEWS Fortran model
- Other SUEWS MCP tools (simulation, analysis)

## Dependencies

- **pandas**: Data manipulation and analysis
- **numpy**: Numerical computations
- **pyyaml**: YAML configuration file handling
- **xarray** (optional): NetCDF file support

## Testing

Basic functionality can be tested using:
```bash
python3 test_preprocessing_basic.py
```

This will run comprehensive tests of all preprocessing tools with sample data.