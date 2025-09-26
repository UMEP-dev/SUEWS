# SUEWS YAML Processor

## Overview

The SUEWS YAML Processor is a three-phase pipeline for validating and updating SUEWS configuration files. It transforms user-provided YAML configurations into validated, up-to-date formats ready for SUEWS model execution.

## Architecture

### Three-Phase Processing Pipeline

```
User YAML → Phase A → Phase B → Phase C → Valid YAML
```

1. **Phase A: YAML structure checks and validation** (`phase_a_parameter_update.py`)
   - Detects missing parameters
   - Renames outdated parameters
   - Identifies non-standard parameters
   - Generates updated YAML with null placeholders

2. **Phase B: Physics checks and validation** (`phase_b_science_check.py`)
   - Validates physics parameters
   - Checks ALL model physics compatibility (rslmethod-stabilitymethod, StorageHeatMethod-OhmIncQf)
   - Validates land cover fractions
   - Updates initial temperatures from CRU data
   - Updates STEBBS outdoor surface temperatures when `stebbsmethod == 1`

3. **Phase C: Pydantic checks and validation** (`phase_c_pydantic_report.py`)
   - Runs Pydantic data model validation
   - Generates detailed error reports
   - Provides actionable feedback

### Components

- **`orchestrator.py`**: Main entry point that coordinates the pipeline
- **`validation_helpers.py`**: Shared validation utilities (legacy precheck functions)
- **`__init__.py`**: Module exports

## Usage

### Command Line Interface

The SUEWS validation system supports flexible pipeline and mode combinations:

#### Basic Validation

```bash
# Complete validation (default: all checks)
suews-validate config.yml

# Same as above, explicit syntax
suews-validate --pipeline ABC config.yml
```

#### Pipeline Options

```bash
# YAML structure checks only
suews-validate --pipeline A config.yml

# Physics checks only
suews-validate --pipeline B config.yml

# Pydantic checks only
suews-validate --pipeline C config.yml

# Combined workflows
suews-validate --pipeline AB config.yml   # Structure + Physics
suews-validate --pipeline AC config.yml   # Structure + Pydantic
suews-validate --pipeline BC config.yml   # Physics + Pydantic
```

#### Mode Options

```bash
# Public mode (default) - restricted features disabled
suews-validate --mode public config.yml

# Developer mode - all features available including experimental options
suews-validate --mode dev config.yml
suews-validate --mode dev --pipeline ABC config.yml
```

#### Complete Examples

```bash
# Developer doing full validation with experimental features
suews-validate --pipeline ABC --mode dev my_research_config.yml

# Quick YAML structure check during development
suews-validate --pipeline A --mode dev draft_config.yml

# Physics validation for a specific site configuration
suews-validate --pipeline B --mode public site_london.yml

### Python API

```python
from supy.data_model.yaml_processor import orchestrator

# Run complete pipeline
orchestrator.main([
    'user_config.yml',
    '--phase', 'ABC',
    '--mode', 'public'
])

# Direct orchestrator usage (legacy)
python -m supy.data_model.yaml_processor.orchestrator user_config.yml --phase ABC --mode public
```

### Phase and Mode Reference

#### Pipeline Options
- `A`: YAML structure checks only
- `B`: Physics checks only
- `C`: Pydantic checks only
- `AB`: YAML structure + Physics checks
- `AC`: YAML structure + Pydantic checks
- `BC`: Physics + Pydantic checks
- `ABC`: Complete validation pipeline (default)

#### Modes
- `public`: Standard mode with restricted features disabled (default)
- `dev`: Developer mode with all features including experimental options (STEBBS, snow models)

### Output Files

All validation runs create:
- **Final files**: `updated_config.yml`, `report_config.txt`
- **Intermediate files**: `updatedA_*.yml`, `reportA_*.txt`, `updatedB_*.yml`, `reportB_*.txt`

The validator shows all created files and their purposes in terminal output.

## Development Notes

### Phase A Details

Phase A performs parameter detection and updating:

1. **Missing Parameter Detection**
   - Compares user YAML against standard configuration
   - Classifies as URGENT (physics options) or OPTIONAL
   - Creates null placeholders for missing parameters

2. **Renamed Parameter Handling**
   - Maps outdated names to current names
   - Examples: `cp` → `rho_cp`, `diagmethod` → `rslmethod`

3. **Extra Parameter Detection**
   - Identifies parameters not in standard
   - Categorises based on Pydantic model constraints

### Phase B Details

Phase B performs scientific validation:

1. **Physics Parameter Validation**
   - Ensures all required physics options are present
   - Validates parameter value ranges

2. **Model Dependencies**
   - Checks interdependencies (e.g., rslmethod ↔ stabilitymethod)
   - Validates STEBBS requirements
   - Checks vegetation parameters

3. **CRU Temperature Integration**
   - Updates initial temperatures from climate data
   - Based on location and start date

### Phase C Details

Phase C integrates with Pydantic data models:

1. **Validation Execution**
   - Runs SUEWSConfig.model_validate()
   - Captures ValidationError details

2. **Report Generation**
   - Formats errors by category
   - Provides fix suggestions
   - Generates updated YAML

## Testing

Tests are located in `test/data_model/yaml_processor/`:

- `test_uptodate_yaml.py`: Phase A tests
- `test_suews_yaml_processor.py`: Integration tests
- `test_precheck.py`: Validation helper tests

Run tests:
```bash
pytest test/data_model/yaml_processor/ -v
```

## Key Data Structures

### Physics Options
```python
PHYSICS_OPTIONS = {
    'netradiationmethod', 'emissionsmethod', 'storageheatmethod',
    'ohmincqf', 'roughlenmommethod', 'roughlenheatmethod',
    'stabilitymethod', 'smdmethod', 'waterusemethod',
    'rslmethod', 'faimethod', 'rsllevel', 'gsmodel',
    'snowuse', 'stebbsmethod'
}
```

### Renamed Parameters
```python
RENAMED_PARAMS = {
    'cp': 'rho_cp',
    'diagmethod': 'rslmethod',
    'localclimatemethod': 'rsllevel',
    'chanohm': 'ch_anohm',
    'cpanohm': 'rho_cp_anohm',
    'kkanohm': 'k_anohm'
}
```

## Output Files

The processor generates:

1. **Updated YAML**: `updated_<input_name>.yml`
   - Contains all parameters with defaults
   - Includes update annotations

2. **Report**: `report_<input_name>.txt`
   - Detailed validation results
   - Action items categorised by priority
   - Fix suggestions

## Detailed Documentation

For comprehensive technical details about each component, see:

- **[ORCHESTRATOR.md](ORCHESTRATOR.md)** - Complete orchestrator architecture, workflow coordination, file management, and integration patterns
- **[PHASE_A_DETAILED.md](PHASE_A_DETAILED.md)** - In-depth YAML structure checks and validation: missing parameters, renamed parameters, extra parameters, mode differences
- **[PHASE_B_DETAILED.md](PHASE_B_DETAILED.md)** - Comprehensive physics checks and validation: scientific parameter validation, CRU temperature integration, physics compatibility rules
- **[PHASE_C_DETAILED.md](PHASE_C_DETAILED.md)** - Complete Pydantic checks and validation: data model validation, error reporting, conditional validation rules

These files provide exhaustive implementation details, troubleshooting guides, testing patterns, and advanced usage scenarios for developers working on or extending the validation system.

## Future Enhancements

- [ ] Add Phase D for post-processing optimisation
- [ ] Implement parallel site processing
- [ ] Add configuration caching
- [ ] Create GUI interface
- [ ] Add batch processing support