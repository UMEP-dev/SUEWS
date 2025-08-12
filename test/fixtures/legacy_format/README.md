# Legacy Format Test Fixtures

This directory contains SUEWS legacy format configuration files (`.txt` and `.nml`) that serve as test fixtures for the conversion tools.

## Purpose

These files are used to test:
1. **Table Converter** (`suews-convert`): Converts between different versions of SUEWS table formats or to YAML format based on target version

## Directory Structure

```
legacy_format/
├── RunControl.nml                      # Main run control configuration
└── Input/
    ├── *.txt                           # SUEWS configuration tables
    ├── *.nml                           # Additional namelist configurations
    └── Kc_2012_data_60.txt            # Sample forcing data
```

## File Types

### Configuration Tables (`.txt`)
- SUEWS_AnthropogenicEmission.txt
- SUEWS_BiogenCO2.txt
- SUEWS_Conductance.txt
- SUEWS_ESTMCoefficients.txt
- SUEWS_Irrigation.txt
- SUEWS_NonVeg.txt
- SUEWS_OHMCoefficients.txt
- SUEWS_Profiles.txt
- SUEWS_SiteSelect.txt
- SUEWS_Snow.txt
- SUEWS_Soil.txt
- SUEWS_Veg.txt
- SUEWS_Water.txt
- SUEWS_WithinGridWaterDist.txt

### Namelist Files (`.nml`)
- RunControl.nml - Main configuration
- ESTMinput.nml - ESTM model configuration
- GridLayoutKc.nml - Grid layout settings
- InitialConditionsKc_2011.nml - Initial conditions
- RunControl_STEBBS.nml - STEBBS configuration
- SUEWS_SPARTACUS.nml - SPARTACUS radiation model
- test_stebbs_building_typologies.nml - STEBBS building types
- test_stebbs_general_params.nml - STEBBS general parameters

### Forcing Data
- Kc_2012_data_60.txt - Meteorological forcing data (not configuration)

## Usage Example

### Testing Table-to-Table Conversion (pre-2025)
```bash
suews-convert -f 2020a -t 2024a -i test/fixtures/legacy_format -o /tmp/converted_output
```

### Testing Table-to-YAML Conversion (2025+)
```bash
suews-convert -f 2024a -t 2025a -i test/fixtures/legacy_format -o /tmp/config.yml
```

## Note

These files represent the deprecated SUEWS input format. The modern workflow uses YAML configuration files exclusively. These fixtures are maintained only for testing backward compatibility and conversion tools.