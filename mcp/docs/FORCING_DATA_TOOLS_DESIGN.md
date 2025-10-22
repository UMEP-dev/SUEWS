# Forcing Data Tools Design

**Date**: 2025-10-22
**Context**: Phoenix test revealed need for forcing data management
**Priority**: HIGH - Blocking realistic scenario tests

---

## Overview

Two complementary tools needed:

1. **Knowledge tool**: Guide users to convert their own data
2. **Automated tool**: Retrieve and convert ERA5 data

---

## Tool 1: Forcing Data Conversion Guide

### Tool Specification

**Name**: `get_forcing_format_guide`

**Purpose**: Provide instructions and code templates for converting user data to SUEWS forcing format

**Type**: Knowledge tool (returns documentation + code)

**Input**:
```python
def get_forcing_format_guide(
    source_format: Optional[str] = None,  # "csv", "netcdf", "custom", etc.
    variables_available: Optional[list[str]] = None,  # User's variable names
) -> dict:
    """Get guidance for converting data to SUEWS forcing format.

    Returns:
        - Required SUEWS forcing variables
        - Format specifications
        - Example conversion code
        - Variable mapping guidance
    """
```

**Output**:
```python
{
    "success": True,
    "format_specification": {
        "required_variables": [
            {
                "name": "kdown",
                "description": "Incoming shortwave radiation",
                "units": "W/m²",
                "typical_range": [0, 1200],
                "required": True
            },
            {
                "name": "Tair",
                "description": "Air temperature",
                "units": "°C",
                "typical_range": [-30, 50],
                "required": True
            },
            # ... more variables
        ],
        "optional_variables": [
            {
                "name": "ldown",
                "description": "Incoming longwave radiation",
                "units": "W/m²",
                "can_be_calculated": True,
                "calculation_note": "SUEWS can estimate from Tair and RH if missing"
            },
            # ... more
        ],
        "file_format": {
            "type": "text",
            "delimiter": "space or tab",
            "header": "optional",
            "time_format": "YYYY DOY HH MM"  # Year, day of year, hour, minute
        }
    },
    "conversion_template": {
        "python": """
# Template for converting user data to SUEWS forcing format
import pandas as pd

# Load your data
df = pd.read_csv('your_data.csv')

# Map to SUEWS variables
forcing = pd.DataFrame({
    'iy': df['year'],
    'id': df['day_of_year'],
    'ih': df['hour'],
    'imin': df['minute'],
    'kdown': df['solar_radiation'],  # Your column name → SUEWS name
    'Tair': df['temperature'],
    'RH': df['relative_humidity'],
    'pres': df['pressure'],
    'rain': df['precipitation'],
    'wdir': df['wind_direction'],
    'wspeed': df['wind_speed']
})

# Save in SUEWS format
forcing.to_csv('suews_forcing.txt', sep=' ', index=False, header=False)
"""
    },
    "variable_mapping_guide": {
        "common_names": {
            "solar_radiation": ["kdown", "SWdown", "SW_IN", "SWIN", "solar", "radiation"],
            "temperature": ["Tair", "T2m", "temp", "air_temp", "AT"],
            "humidity": ["RH", "relative_humidity", "rh", "humidity"],
            # ... mappings for all variables
        },
        "unit_conversions": {
            "temperature": {
                "K_to_C": "T_celsius = T_kelvin - 273.15",
                "F_to_C": "T_celsius = (T_fahrenheit - 32) * 5/9"
            },
            "radiation": {
                "MJ_m2_day_to_W_m2": "W_m2 = (MJ_m2_day * 1e6) / 86400"
            },
            # ... more conversions
        }
    },
    "validation": {
        "check_script": """
# Validation script
def validate_forcing(df):
    issues = []

    # Check required columns
    required = ['iy', 'id', 'ih', 'imin', 'kdown', 'Tair', 'RH', 'pres', 'rain', 'wdir', 'wspeed']
    missing = [col for col in required if col not in df.columns]
    if missing:
        issues.append(f"Missing columns: {missing}")

    # Check ranges
    if (df['Tair'] < -50).any() or (df['Tair'] > 60).any():
        issues.append("Temperature out of reasonable range")

    if (df['RH'] < 0).any() or (df['RH'] > 100).any():
        issues.append("Relative humidity out of range [0, 100]")

    # Check for missing values
    if df.isnull().any().any():
        issues.append("Missing values detected")

    return issues
"""
    },
    "example_datasets": [
        {
            "name": "Weather station CSV",
            "format": "CSV with timestamp",
            "example": "examples/weather_station_example.csv"
        },
        {
            "name": "NetCDF climate data",
            "format": "NetCDF with time dimension",
            "example": "examples/netcdf_example.nc"
        }
    ],
    "guidance": "Use this guide to convert your meteorological data to SUEWS forcing format. The template script can be adapted to your specific data structure."
}
```

### Implementation Location

**File**: `mcp/src/suews_mcp/tools/knowledge.py`

**Function**: Add `get_forcing_format_guide()` function

**Data source**: Embedded documentation (no external files needed)

---

## Tool 2: ERA5 Data Retrieval and Conversion

### Tool Specification

**Name**: `get_era5_forcing`

**Purpose**: Automatically retrieve ERA5 reanalysis data and convert to SUEWS forcing format

**Type**: Utility tool (active retrieval)

**Reference**: https://confluence.ecmwf.int/pages/viewpage.action?pageId=505390919

**Input**:
```python
def get_era5_forcing(
    lat: float,
    lon: float,
    start_date: str,  # "YYYY-MM-DD"
    end_date: str,    # "YYYY-MM-DD"
    output_path: str,
    api_key: Optional[str] = None,  # CDS API key (or from env)
) -> dict:
    """Retrieve ERA5 data and convert to SUEWS forcing format.

    Args:
        lat: Latitude (decimal degrees)
        lon: Longitude (decimal degrees)
        start_date: Start date
        end_date: End date
        output_path: Where to save SUEWS forcing file
        api_key: ECMWF CDS API key (optional if set in environment)

    Returns:
        Status and file path
    """
```

**Output**:
```python
{
    "success": True,
    "forcing_file": "/path/to/suews_forcing.txt",
    "metadata": {
        "location": {"lat": 33.45, "lon": -112.07},
        "period": {"start": "2018-07-01", "end": "2018-07-31"},
        "source": "ERA5 reanalysis",
        "resolution": "0.25° (~25km)",
        "timestep": 3600,  # seconds
        "variables": ["kdown", "ldown", "Tair", "RH", "pres", "rain", "wdir", "wspeed"],
        "notes": "ERA5 hourly data, nearest grid point to location"
    },
    "quality_check": {
        "missing_values": 0,
        "out_of_range": 0,
        "coverage": "100%",
        "warnings": []
    },
    "guidance": "ERA5 forcing data ready for SUEWS. Use this file with your configuration."
}
```

### ERA5 Variable Mapping

**ERA5 → SUEWS mapping**:

| SUEWS Variable | ERA5 Variable | Units Conversion |
|----------------|---------------|------------------|
| kdown | ssrd (surface solar radiation downwards) | J/m² → W/m² (divide by timestep) |
| ldown | strd (surface thermal radiation downwards) | J/m² → W/m² (divide by timestep) |
| Tair | t2m (2m temperature) | K → °C (subtract 273.15) |
| RH | (derived from t2m, d2m) | Calculate from temperature and dewpoint |
| pres | sp (surface pressure) | Pa → kPa (divide by 1000) |
| rain | tp (total precipitation) | m → mm (multiply by 1000) |
| wdir | (derived from u10, v10) | Calculate from u/v components |
| wspeed | (derived from u10, v10) | Calculate: √(u² + v²) |

### Implementation Requirements

**Dependencies**:
```python
# Required packages
- cdsapi  # ECMWF Climate Data Store API
- xarray  # For NetCDF handling
- numpy   # For calculations
- pandas  # For data processing
```

**CDS API Setup**:
```python
# User must have:
# 1. ECMWF account (free registration)
# 2. CDS API key in ~/.cdsapirc or environment
# 3. Accepted CDS terms and conditions

# Example ~/.cdsapirc:
# url: https://cds.climate.copernicus.eu/api/v2
# key: {UID}:{API-KEY}
```

### Implementation Strategy

**Phase 1: Basic retrieval** (simpler)
```python
def get_era5_forcing(lat, lon, start_date, end_date, output_path, api_key=None):
    """Retrieve ERA5 and convert to SUEWS format."""

    # 1. Initialize CDS API
    import cdsapi
    c = cdsapi.Client()

    # 2. Request ERA5 data
    request = {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': [
            '2m_temperature', '2m_dewpoint_temperature',
            'surface_pressure', 'total_precipitation',
            '10m_u_component_of_wind', '10m_v_component_of_wind',
            'surface_solar_radiation_downwards',
            'surface_thermal_radiation_downwards'
        ],
        'year': [2018],
        'month': [7],
        'day': list(range(1, 32)),
        'time': [f'{h:02d}:00' for h in range(24)],
        'area': [lat+0.25, lon-0.25, lat-0.25, lon+0.25],  # [N, W, S, E]
    }

    temp_nc = 'temp_era5.nc'
    c.retrieve('reanalysis-era5-single-levels', request, temp_nc)

    # 3. Load and process
    import xarray as xr
    ds = xr.open_dataset(temp_nc)

    # 4. Convert variables
    forcing = convert_era5_to_suews(ds, lat, lon)

    # 5. Save in SUEWS format
    forcing.to_csv(output_path, sep=' ', index=False, header=False)

    return {
        "success": True,
        "forcing_file": output_path,
        "metadata": {...}
    }
```

**Phase 2: Enhanced features** (later)
- Cache ERA5 data (avoid re-downloading)
- Support multiple locations (batch)
- Gap filling for missing data
- Quality flags in output
- Spatial interpolation (nearest vs bilinear)

---

## Tool 3: Forcing Data Inspection (Integrates SuPy)

**Name**: `inspect_forcing_data`

**Purpose**: Validate and summarise forcing data before simulation

**Integration**: Wraps existing `supy._check.check_forcing()` function

**Implementation**:
```python
def inspect_forcing_data(forcing_path: str) -> dict:
    """Inspect forcing data file using SuPy validation.

    Wraps supy._check.check_forcing() and adds summary statistics.

    Args:
        forcing_path: Path to forcing data file

    Returns:
        Validation results + statistics in MCP-friendly format
    """
    import pandas as pd
    from supy._check import check_forcing

    # Load forcing data
    # Note: Need to handle SUEWS text format
    df_forcing = load_forcing_file(forcing_path)

    # Run SuPy validation
    issues = check_forcing(df_forcing, fix=False)

    # Calculate statistics
    stats = {}
    for col in df_forcing.columns:
        if col not in ["iy", "id", "it", "imin", "isec"]:
            stats[col] = {
                "mean": float(df_forcing[col].mean()),
                "min": float(df_forcing[col].min()),
                "max": float(df_forcing[col].max()),
                "std": float(df_forcing[col].std()),
                "missing": int(df_forcing[col].isna().sum())
            }

    return {
        "success": issues is None or len(issues) == 0,
        "file": forcing_path,
        "period": {
            "start": str(df_forcing.index[0]),
            "end": str(df_forcing.index[-1]),
            "duration": str(df_forcing.index[-1] - df_forcing.index[0]),
            "timestep": int(df_forcing.index.freq.delta.total_seconds()),
            "records": len(df_forcing)
        },
        "variables": stats,
        "validation": {
            "passed": issues is None or len(issues) == 0,
            "issues": issues if issues else [],
            "checks_performed": [
                "Column presence and order",
                "Temporal index validity",
                "Physical range validation (using SuPy rules)",
                "Missing value detection"
            ]
        },
        "quality_summary": {
            "missing_data": sum(s["missing"] for s in stats.values()),
            "out_of_range": len([i for i in (issues or []) if "between" in i or "outliers" in i]),
            "overall": "good" if not issues else "issues_found"
        },
        "recommendations": generate_recommendations(issues, stats),
        "guidance": "Use fix=True in check_forcing() to automatically clip out-of-range values" if issues else "Forcing data is ready for simulation"
    }
```

**Helper function for loading SUEWS forcing format**:
```python
def load_forcing_file(forcing_path: str) -> pd.DataFrame:
    """Load SUEWS forcing data file into DataFrame.

    SUEWS format: space/tab separated, no header
    Columns: iy id it imin kdown ldown Tair RH pres rain wdir wspeed [more...]
    """
    from supy._load import dict_var_type_forcing

    # Column names from SuPy
    col_names = list(dict_var_type_forcing.keys())

    # Load file
    df = pd.read_csv(
        forcing_path,
        delim_whitespace=True,
        names=col_names,
        parse_dates=False
    )

    # Create datetime index
    # iy=year, id=day_of_year, it=hour, imin=minute
    df.index = pd.to_datetime(
        df['iy'].astype(str) + ' ' +
        df['id'].astype(str) + ' ' +
        df['it'].astype(str) + ':' +
        df['imin'].astype(str),
        format='%Y %j %H:%M'
    )

    # Infer frequency
    df = df.asfreq(pd.infer_freq(df.index))

    return df


def generate_recommendations(issues, stats):
    """Generate actionable recommendations based on validation."""
    recommendations = []

    if not issues:
        recommendations.append("✓ All validation checks passed")
        recommendations.append("✓ Forcing data is ready for SUEWS simulation")
        return recommendations

    # Analyze issues
    for issue in issues:
        if "Missing columns" in issue:
            recommendations.append("⚠ Add missing required variables")
        elif "out of range" in issue or "outliers" in issue:
            recommendations.append("⚠ Check data quality for out-of-range values")
            recommendations.append("  Option 1: Correct source data")
            recommendations.append("  Option 2: Use fix=True to clip values automatically")
        elif "duplicate" in issue.lower():
            recommendations.append("⚠ Remove duplicate timestamps")
        elif "monotonic" in issue.lower():
            recommendations.append("⚠ Sort data by timestamp")
        elif "freq" in issue.lower():
            recommendations.append("⚠ Ensure regular timestep (no gaps)")

    # Check for suspiciously high missing data
    total_missing = sum(s["missing"] for s in stats.values())
    if total_missing > len(stats) * 10:  # More than 10 missing per variable
        recommendations.append("⚠ High number of missing values detected - may need gap filling")

    return recommendations
```

**Benefits of using SuPy validation**:
1. ✅ Reuses existing, well-tested validation logic
2. ✅ Uses SuPy's comprehensive rules (checker_rules_indiv.json)
3. ✅ Consistent with how SuPy internally validates data
4. ✅ Can leverage fix=True for automatic correction
5. ✅ No duplication of validation logic

---

## Implementation Priority

### Phase 1: Knowledge Tool (Week 1)
**Priority**: HIGH
**Effort**: 1-2 days
**Blocking**: Phoenix test

**Tasks**:
1. Design format specification structure
2. Create conversion templates (Python, R)
3. Document common data sources
4. Add validation examples
5. Write unit tests
6. Update MCP server registration

**Deliverable**: `get_forcing_format_guide()` tool ready

### Phase 2: ERA5 Tool (Week 2)
**Priority**: HIGH
**Effort**: 3-5 days
**Dependencies**: CDS API access

**Tasks**:
1. Set up CDS API integration
2. Implement ERA5 → SUEWS variable mapping
3. Add unit conversion functions
4. Implement location/time request
5. Add error handling (API failures, rate limits)
6. Write integration tests
7. Document CDS setup for users

**Deliverable**: `get_era5_forcing()` tool ready

### Phase 3: Inspection Tool (Week 3)
**Priority**: MEDIUM
**Effort**: 1-2 days
**Nice-to-have**: Enhances UX

**Tasks**:
1. Implement forcing file parser
2. Add statistical analysis
3. Add quality checks
4. Write tests

**Deliverable**: `inspect_forcing_data()` tool ready

---

## User Workflows Enabled

### Workflow 1: User Has Own Data
```
User: "I have weather station data in CSV format. Help me prepare it for SUEWS."

Claude:
1. Calls get_forcing_format_guide(source_format="csv")
2. Shows format requirements
3. Provides conversion template
4. User adapts template to their data
5. Validates with provided script
6. Ready for simulation
```

### Workflow 2: User Needs ERA5 Data
```
User: "Get ERA5 data for Phoenix in July 2018."

Claude:
1. Calls get_era5_forcing(lat=33.45, lon=-112.07,
                          start="2018-07-01", end="2018-07-31")
2. Retrieves from CDS API
3. Converts to SUEWS format
4. Returns forcing file path
5. Ready for simulation
```

### Workflow 3: User Wants to Check Data
```
User: "Check if this forcing data is suitable for SUEWS."

Claude:
1. Calls inspect_forcing_data("my_forcing.txt")
2. Reports time coverage, variables, quality
3. Flags any issues
4. Recommends fixes if needed
5. User corrects and retries
```

---

## Testing Plan

### Test 1: Format Guide
```python
# Test knowledge tool
result = get_forcing_format_guide(source_format="csv")
assert "required_variables" in result["format_specification"]
assert "conversion_template" in result
assert "python" in result["conversion_template"]
```

### Test 2: ERA5 Retrieval (requires API key)
```python
# Test ERA5 tool (integration test)
result = get_era5_forcing(
    lat=51.5,  # London
    lon=-0.1,
    start_date="2011-07-01",
    end_date="2011-07-07",
    output_path="test_era5_forcing.txt"
)
assert result["success"]
assert Path(result["forcing_file"]).exists()
assert result["metadata"]["variables"] == expected_vars
```

### Test 3: Inspection
```python
# Test inspection tool
result = inspect_forcing_data("test_forcing.txt")
assert result["success"]
assert "period" in result
assert "variables" in result
assert result["quality_checks"]["missing_data"] == "None"
```

---

## Documentation Updates

### MCP Server README
Add to available tools list:
```markdown
### Forcing Data Tools
- `get_forcing_format_guide` - Guide for converting user data
- `get_era5_forcing` - Retrieve ERA5 data for location/period
- `inspect_forcing_data` - Validate forcing data quality
```

### User Guide
New section: "Preparing Forcing Data"
- How to use your own data
- How to get ERA5 data
- How to validate forcing data
- Common issues and solutions

---

## Open Questions

1. **CDS API limits**: What are rate limits? Need caching strategy?
2. **ERA5 spatial resolution**: 0.25° (~25km) sufficient for urban? Document limitations?
3. **Time zones**: How to handle UTC vs local time in forcing data?
4. **Data gaps**: How to handle missing hours in ERA5 or user data?
5. **Alternative sources**: Support other reanalysis (MERRA-2, JRA-55)?

---

## Success Metrics

**Phase 1 Complete**:
- ✅ Users can convert their own data
- ✅ Phoenix test unblocked (can prepare forcing)
- ✅ Format guide tested with 3+ data sources

**Phase 2 Complete**:
- ✅ ERA5 data retrievable for any location/time
- ✅ Phoenix test runs with real ERA5 July 2018 data
- ✅ Variable mapping validated against literature

**Phase 3 Complete**:
- ✅ Forcing data quality checkable before simulation
- ✅ Users catch errors early
- ✅ Reduced simulation failures due to bad forcing

---

**Status**: Design complete, ready for implementation
**Next**: Implement Phase 1 (knowledge tool) first
**Blocking**: Phoenix realistic test scenario

cc-dev
2025-10-22
