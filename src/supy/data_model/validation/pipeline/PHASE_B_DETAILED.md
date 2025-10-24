# Phase B: Physics Validation Checks Guide

## Overview

Phase B performs physics validation checks to ensure model physics consistency and provides reasonable automatic corrections. This comprehensive guide covers all aspects of Phase B operation.

## Table of Contents

- [Architecture and Design](#architecture-and-design)
- [Technical Implementation](#technical-implementation)
- [Scientific Validation Categories](#scientific-validation-categories)
- [CRU TS4.06 Climatological Integration](#cru-ts406-climatological-integration)
- [Scientific Corrections and Adjustments](#scientific-corrections-and-adjustments)
- [Processing Modes and Behaviour](#processing-modes-and-behaviour)
- [Output Files Structure](#output-files-structure)
- [Error Handling and Edge Cases](#error-handling-and-edge-cases)
- [Integration with Other Phases](#integration-with-other-phases)
- [Testing and Validation](#testing-and-validation)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)

## Architecture and Design

Phase B implements a multi-layered scientific validation system that:

1. **Validates Physics Parameters**: Ensures required model physics options parameters are present and non-empty
2. **Checks Model Dependencies**: Validates internal consistency between physics options
3. **Validates Land Cover**: Checks surface fraction totals and parameter consistency
4. **Validates Geographic Parameters**: Ensures coordinates and location-dependent parameters are realistic
5. **Applies CRU Integration**: Uses CRU TS4.06 climatological data for temperature initialisation
6. **Makes Scientific Corrections**: Automatic adjustments that improve model realism

## Technical Implementation

### Core Functions

- `validate_phase_b_inputs()`: Input file validation and loading
- `extract_simulation_parameters()`: Extract and validate simulation parameters with comprehensive error collection
- `validate_physics_parameters()`: Required physics parameter validation
- `validate_model_option_dependencies()`: Physics option consistency checking
- `validate_land_cover_consistency()`: Surface fraction and parameter validation
- `validate_geographic_parameters()`: Coordinate and location validation
- `get_mean_monthly_air_temperature()`: CRU TS4.06 monthly climatological temperature lookup
- `get_mean_annual_air_temperature()`: CRU TS4.06 annual climatological temperature lookup (average of 12 months)
- `run_scientific_adjustment_pipeline()`: Intelligent automatic parameter adjustments
- `run_science_check()`: Main orchestration function for all validations

### Key Data Structures

```python
@dataclass
class ValidationResult:
    """Structured result from scientific validation checks."""
    status: str  # 'PASS', 'WARNING', 'ERROR'
    category: str  # 'PHYSICS', 'GEOGRAPHY', 'SEASONAL', 'LAND_COVER', 'MODEL_OPTIONS'
    parameter: str
    site_index: Optional[int] = None
    message: str = ""
    suggested_value: Any = None
    applied_fix: bool = False

@dataclass
class ScientificAdjustment:
    """Record of automatic scientific adjustment applied."""
    parameter: str
    site_index: Optional[int] = None
    old_value: Any = None
    new_value: Any = None
    reason: str = ""

class DLSCheck(BaseModel):
    """Calculate daylight saving time transitions and timezone offset from coordinates."""
    lat: float
    lng: float
    year: int
    startdls: Optional[int] = None
    enddls: Optional[int] = None
```

## Scientific Validation Categories

### Physics Parameters Validation

Validates specific critical model physics parameters that are essential for model operation:

- **Selected Required Parameters**: Key physics options validated for presence and valid values
- **Non-Empty Values**: Critical parameters cannot be null, empty, or zero when required
- **Dependency Validation**: Validates interdependencies between physics options (e.g., rslmethod-stabilitymethod)
- **Type Validation**: Parameters must have correct data types
- **Note**: Currently focuses on essential parameters rather than comprehensive validation of all physics options

### Model Option Dependencies

Validates internal consistency between different physics options using actual implemented dependency rules:

```python
def validate_model_option_dependencies(yaml_data: dict) -> List[ValidationResult]:
    """Validate internal consistency between model physics options."""
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    # Check rslmethod-stabilitymethod constraints
    rslmethod = get_value_safe(physics, "rslmethod")
    stabilitymethod = get_value_safe(physics, "stabilitymethod")
    storageheatmethod = get_value_safe(physics, "storageheatmethod")
    ohmincqf = get_value_safe(physics, "ohmincqf")

    # Constraint: If rslmethod == 2, stabilitymethod must be 3
    if rslmethod == 2 and stabilitymethod != 3:
        results.append(ValidationResult(
            status="ERROR",
            category="MODEL_OPTIONS",
            parameter="rslmethod-stabilitymethod",
            message="If rslmethod == 2, stabilitymethod must be 3",
            suggested_value="Set stabilitymethod to 3"
        ))

    # Constraint: StorageHeatMethod=1 (OHM_WITHOUT_QF) requires OhmIncQf=0
    if storageheatmethod == 1 and ohmincqf != 0:
        results.append(ValidationResult(
            status="ERROR",
            category="MODEL_OPTIONS",
            parameter="storageheatmethod-ohmincqf",
            message=f"StorageHeatMethod is set to {storageheatmethod} and OhmIncQf is set to {ohmincqf}. You should switch to OhmIncQf=0.",
            suggested_value="Set OhmIncQf to 0"
        ))

    return results
```

### Land Cover Consistency

Comprehensive validation and adjustment of surface types and parameters:

- **Surface Fraction Totals**: Must sum to 1.0 for each site - automatically adjusted if needed
- **Seasonal LAI Adjustments**: Automatic LAI calculation for deciduous trees based on season

### Geographic Parameter Validation

Location-dependent parameter validation (actual implemented checks):

- **Coordinate Validity**: Latitude (-90 to 90°), longitude (-180 to 180°) with numeric type validation
- **Timezone Parameter**: Warns if missing, can be calculated automatically from coordinates
- **Daylight Saving Parameters**: Warns if DLS parameters missing, calculated from geographic location

## CRU TS4.06 Climatological Integration

### CRU Temperature Initialisation System

Phase B integrates CRU TS4.06 monthly climatological data (1991-2020) for accurate temperature initialisation of surface types and STEBBS parameters:

### Temperature Functions

Phase B provides two CRU-based temperature functions for different use cases:

#### Monthly Temperature (Season-Dependent Parameters)

```python
def get_mean_monthly_air_temperature(
    lat: float,
    lon: float,
    month: int,
    spatial_res: float = 0.5
) -> float:
    """Calculate mean monthly air temperature using CRU TS4.06 data."""
    # Loads CRU Parquet data from package resources
    # Finds nearest grid cell within spatial resolution
    # Returns climatological mean temperature for specified month
```

Used for initialising parameters that vary with seasons:
- Surface temperatures (tsfc, tin, temperature arrays)
- STEBBS outdoor surface temperatures
- Initial state temperatures for all surface types

#### Annual Temperature (Stable Parameters)

```python
def get_mean_annual_air_temperature(
    lat: float,
    lon: float,
    spatial_res: float = 0.5
) -> float:
    """Calculate annual mean air temperature using CRU TS4.06 climate normals."""
    # Computes average of all 12 monthly climate normals (1991-2020)
    # Returns stable long-term average annual temperature
    # Suitable for parameters that do not vary rapidly with seasons
```

Used for initialising stable, non-seasonal parameters that require representative annual values rather than month-specific temperatures.

### CRU Data Features

- **Coverage**: Global land areas at 0.5° resolution
- **Period**: 1991-2020 climatological normals
- **Variables**: Monthly mean air temperature
- **Accuracy**: Location-specific estimates within 0.5° spatial resolution
- **Validation**: Ensures coordinates are within CRU coverage area

### Automatic Temperature Initialisation

```yaml
# Before Phase B processing
sites:
- properties:
    initial_states:
      paved:
        tsfc:
          value: null    # Uninitialised surface temperature
        temperature:
          value: null    # Uninitialised 5-layer temperatures

# After Phase B processing with CRU integration
sites:
- properties:
    initial_states:
      paved:
        tsfc:
          value: 15.8    # CRU-derived temperature for January at coordinates
        temperature:
          value: [15.8, 15.8, 15.8, 15.8, 15.8]    # 5-layer temperatures
```

## Scientific Corrections and Adjustments

### Intelligent Automatic Corrections

Phase B makes scientific adjustments that improve model realism without changing user intent:

### Temperature Initialisation

- **CRU Integration**: Initialises temperatures using climatological data
- **Month-Aware**: Uses correct month from simulation start date
- **Coordinate-Based**: Location-specific temperature from CRU grid

### Land Cover Adjustments

- **Fraction Normalisation**: Adjusts surface fractions to sum to 1.0 by rounding the surface with maximum fraction value
- **Seasonal LAI Adjustments**: Calculates LAI for deciduous trees based on seasonal parameters (laimin, laimax)

### STEBBS Method Integration

- **Conditional Logic**: When `stebbsmethod == 0`, nullifies STEBBS parameters
- **Parameter Cleanup**: Removes unused STEBBS parameters for clarity
- **Consistency**: Ensures STEBBS configuration matches selected method
- **Temperature Initialisation**: When `stebbsmethod == 1`, automatically updates `WallOutdoorSurfaceTemperature` and `WindowOutdoorSurfaceTemperature` using CRU climatological data
- **CRU-Based Updates**: Uses location-specific mean monthly air temperature from CRU TS4.06 dataset

### Parameter Validation Improvements

Phase B includes enhanced validation logic with improved parameter handling:

- **Improved get_value_safe Function**: Better handling of nested parameter extraction
- **Reduced False Positives**: More accurate validation with safer parameter access
- **Enhanced Error Handling**: Better detection of actual configuration issues

### DLS Parameter Calculation

- **Automatic DLS Calculation**: Computes daylight saving start/end days from coordinates
- **Timezone Integration**: Uses timezonefinder and pytz libraries for accurate calculations

## Processing Modes and Behaviour

### Mode-Dependent Behaviour

Phase B uses the mode parameter for report formatting but applies the same validation to all modes:

### Actual Implementation

- **Same Validation**: Both public and developer modes run identical validation checks
- **Same Corrections**: Both modes apply the same automatic adjustments
- **Mode Difference**: Only affects report header formatting ("Public" vs "Developer" in report title)

### Validation Status Values

```python
# Actual validation status values used in implementation
@dataclass
class ValidationResult:
    status: str  # "ERROR", "WARNING", "PASS"
    category: str  # "PHYSICS", "GEOGRAPHY", "LAND_COVER", "MODEL_OPTIONS"
    parameter: str
    message: str = ""
```

## Output Files Structure

### Updated YAML File (`updatedB_<filename>.yml`)

```yaml
# ==============================================================================
# Updated YAML
# ==============================================================================
#
# This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
# Details of changes are in the generated report.
#
# ==============================================================================

name: Scientifically Validated Configuration
model:
  physics:
    netradiationmethod: 2
    emissionsmethod: 2
    stebbsmethod: 0
sites:
- properties:
    lat: 51.5074
    lng: -0.1278
    initial_states:
      paved:
        tsfc:
          value: 12.4    # CRU-derived for January at London coordinates
```

### Scientific Validation Report Structure

Phase B generates comprehensive reports with two main sections:

- **ACTION NEEDED**: Critical physics issues requiring user attention (ERROR status validation results)
- **NO ACTION NEEDED**: Automatic adjustments made by Phase B, warnings, and Phase A information

### Scientific Validation Report (Standalone Phase B)

```text
# SUEWS Validation Report
# ==================================================
# Mode: Public
# ==================================================

## ACTION NEEDED
- Found (2) critical scientific parameter error(s):
-- rslmethod-stabilitymethod: If rslmethod == 2, stabilitymethod must be 3
   Location: model.physics.stabilitymethod
-- storageheatmethod-ohmincqf: StorageHeatMethod is set to 1 and OhmIncQf is set to 1. You should switch to OhmIncQf=0.
   Location: model.physics.ohmincqf

## NO ACTION NEEDED
- Updated (11) parameter(s):
-- initial_states.paved at site [0]: temperature, tsfc, tin → 12.4 C (Set from CRU data for coordinates (51.51, -0.13) for month 1)
-- initial_states.bldgs at site [0]: temperature, tsfc, tin → 12.4 C (Set from CRU data for coordinates (51.51, -0.13) for month 1)
-- stebbs.WallOutdoorSurfaceTemperature at site [0]: 20.0 → 12.4 C (Set from CRU data for coordinates (51.51, -0.13) for month 1)
-- stebbs.WindowOutdoorSurfaceTemperature at site [0]: 20.0 → 12.4 C (Set from CRU data for coordinates (51.51, -0.13) for month 1)
-- anthropogenic_emissions.startdls at site [0]: 15.0 → 86 (Calculated DLS start for coordinates (51.51, -0.13))
-- anthropogenic_emissions.enddls at site [0]: 12.0 → 303 (Calculated DLS end for coordinates (51.51, -0.13))
-- paved.sfr at site [0]: rounded to achieve sum of land cover fractions equal to 1.0

# ==================================================
```

## Error Handling and Edge Cases

### Initialization Error Handling (Enhanced)

Phase B now provides comprehensive error collection and reporting for initialization failures:

```python
def extract_simulation_parameters(yaml_data: dict) -> Tuple[int, str, str]:
    """Extract simulation parameters for validation."""
    # Collect all validation errors instead of failing on first error
    errors = []

    if not isinstance(start_date, str) or "-" not in str(start_date):
        errors.append("Missing or invalid 'start_time' in model.control - must be in 'YYYY-MM-DD' format")

    if not isinstance(end_date, str) or "-" not in str(end_date):
        errors.append("Missing or invalid 'end_time' in model.control - must be in 'YYYY-MM-DD' format")

    # If we have errors, combine them into a single error message for proper handling
    if errors:
        error_msg = "; ".join(errors)
        raise ValueError(error_msg)
```

When initialization fails, Phase B creates individual error reports for each issue and generates comprehensive reports even during failures, ensuring users always receive actionable guidance.

### CRU Data Availability (Actual Implementation)

```python
# Phase B handles CRU data access with proper error handling
def get_mean_monthly_air_temperature(lat: float, lon: float, month: int, spatial_res: float = 0.5) -> float:
    # Validate inputs
    if not (1 <= month <= 12):
        raise ValueError(f"Month must be between 1 and 12, got {month}")
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")
```

### Geographic Validation (Actual Implementation)

- **Coordinate Range Validation**: Latitude (-90 to 90°), longitude (-180 to 180°)
- **Missing Coordinate Handling**: ERROR status for missing lat/lng parameters
- **Invalid Coordinate Types**: ERROR status for non-numeric coordinate values
- **Timezone Warnings**: WARNING status if timezone parameter is missing

### Physics Option Validation (Actual Implementation)

- **rslmethod-stabilitymethod Dependency**: If rslmethod == 2, stabilitymethod must be 3
- **storageheatmethod-ohmincqf Compatibility**: If StorageHeatMethod == 1, OhmIncQf must be 0
- **Missing Required Parameters**: ERROR status for null physics parameters
- **Physics Section Missing**: WARNING status if entire physics section is empty

## Integration with Other Phases

Phase B output serves as input to subsequent phases in the validation pipeline:

### File Handoff

When Phase B runs as part of multi-phase pipelines, its output is processed internally and consolidated:

```bash
# Phase B in multi-phase workflows
User Input: config.yml
    ↓
Phase A (internal) → Phase B (internal) → ...
    ↓
Final Output: updated_config.yml, report_config.txt

# The final report consolidates Phase B findings with other phases
# File naming is standardised regardless of pipeline (AB, BC, ABC, etc.)
```

### Mode Integration

- **Both Modes Public and Dev**: Provide identical scientific validation - mode only affects report header
- **Phase Consolidation**: Integrates Phase A reports when available

### Workflow Integration

1. **Multi-phase workflows** (AB, BC, ABC): Phase B intermediate files preserved based on workflow success
2. **B-only workflow**: Phase B files retained as final outputs
3. **Error Handling**: Phase B outputs preserved if subsequent phases fail
4. **Report Consolidation**: Phase B reports include Phase A information when available

## Testing and Validation

Phase B includes comprehensive test coverage.

### Example Tests

#### Monthly Temperature Test

```python
def test_cru_monthly_temperature_integration():
    """Test CRU monthly climatological temperature integration."""
    # Test known coordinates (London)
    lat, lng, month = 51.5074, -0.1278, 1
    temp = get_mean_monthly_air_temperature(lat, lng, month)

    # London January temperature should be reasonable
    assert 0 <= temp <= 20, f"Unrealistic temperature: {temp}°C"
    assert temp is not None, "CRU lookup should return valid temperature"
```

#### Annual Temperature Test

```python
def test_cru_annual_temperature_integration():
    """Test CRU annual climatological temperature integration."""
    # Test known coordinates (London)
    lat, lng = 51.5074, -0.1278
    annual_temp = get_mean_annual_air_temperature(lat, lng)

    # London annual temperature should be reasonable
    assert 0 <= annual_temp <= 20, f"Unrealistic annual temperature: {annual_temp}°C"

    # Annual mean should be cooler than summer month
    summer_temp = get_mean_monthly_air_temperature(lat, lng, 7)
    assert annual_temp < summer_temp, "Annual mean should be cooler than summer"
```

## Best Practices

### For Users

1. **Run Phase B after Phase A** to ensure scientific consistency of up-to-date parameters
2. **Review ACTION NEEDED items** carefully - these require user decisions
3. **Trust scientific corrections** - automatic adjustments improve model realism
4. **Validate coordinates** ensure latitude/longitude are correct for CRU integration
5. **Use AB or ABC workflows** for comprehensive validation

### For Developers

1. **Mode selection is cosmetic** - both modes run identical validation
2. **Add validation rules** following the ValidationResult pattern (status: "ERROR"/"WARNING"/"PASS")
3. **Test CRU integration** when adding location-dependent features
4. **Update adjustment logic** using ScientificAdjustment records
5. **Maintain backward compatibility** when modifying validation rules

## Troubleshooting

### Common Issues

**Issue**: "CRU data file not found"

```text
Solution: Ensure CRU Parquet file is available in package
Check: Import should include ext_data/CRU_TS4.06_1991_2020.parquet
Fix: Reinstall SUEWS package or check data file integrity
```

**Issue**: "No CRU data found within spatial resolution"

```text
Solution: Coordinates may be over ocean or outside CRU coverage
Check: Verify latitude/longitude are for land locations
Fix: Use land-based coordinates or increase spatial resolution
```

**Issue**: "Physics option dependency violation"

```text
Solution: Incompatible physics options selected
Check: Review physics option combinations in SUEWS documentation
Fix: Adjust physics options to compatible combination
```

**Issue**: "Surface fractions sum to 1.020000, should equal 1.0"

```text
Solution: Land cover fractions are incomplete or incorrect
Check: Verify surface fractions in your configuration
Fix: Adjust surface fractions so total equals 1.0
Note: Only tiny floating-point errors are automatically corrected
```

### Advanced Usage

```python
# Direct Python usage for Phase B
from supy.data_model.science_check import run_science_check

# Function returns updated YAML data as dict
updated_data = run_science_check(
    uptodate_yaml_file="updatedA_my_config.yml",
    user_yaml_file="my_config.yml",
    standard_yaml_file="src/supy/sample_data/sample_config.yml",
    science_yaml_file="updatedB_my_config.yml",
    science_report_file="reportB_my_config.txt",
    mode="public",  # Mode only affects report header
    phase="B"
)

if updated_data:
    print("✅ Phase B scientific validation completed successfully")
else:
    print("❌ Phase B encountered errors")
```

### Command Line Usage

```bash
# Public mode (default) - standard scientific validation
python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase B --mode public

# Developer mode - identical validation with different report header
python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase B --mode dev
```

### Integration Examples

```bash
# Phase B after Phase A (AB workflow)
python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AB

# Phase B before Phase C (BC workflow)
python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase BC

# Complete pipeline including Phase B (ABC workflow)
python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC
```

## Related Documentation

### Three-Phase Validation System
- [README](README.md) - Overview of the complete three-phase validation system
- [Orchestrator](ORCHESTRATOR.md) - Implementation and workflow coordination

### Other Validation Phases
- [Phase A Detailed](PHASE_A_DETAILED.md) - Phase A parameter detection and structure validation
- [Phase C Detailed](PHASE_C_DETAILED.md) - Phase C Pydantic validation and conditional rules

### SUEWS Configuration
- Complete parameter specifications and validation details in the main SUEWS documentation

### CRU Dataset
- All CRU data are from https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.06/