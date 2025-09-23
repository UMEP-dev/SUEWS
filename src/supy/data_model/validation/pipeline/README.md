# SUEWS YAML Processor

## Overview

The SUEWS YAML Processor is a three-phase pipeline for validating and updating SUEWS configuration files. It transforms user-provided YAML configurations into validated, up-to-date formats ready for SUEWS model execution.

## Architecture

### Three-Phase Processing Pipeline

```
User YAML → Phase A → Phase B → Phase C → Valid YAML
```

1. **Phase A: Parameter Update** (`phase_a_parameter_update.py`)
   - Detects missing parameters
   - Renames outdated parameters
   - Identifies non-standard parameters
   - Generates updated YAML with null placeholders

2. **Phase B: Scientific Validation** (`phase_b_science_check.py`)
   - Validates physics parameters
   - Checks ALL model physics compatibility (rslmethod-stabilitymethod, StorageHeatMethod-OhmIncQf)
   - Validates land cover fractions
   - Updates initial temperatures from CRU data
   - Updates STEBBS outdoor surface temperatures when `stebbsmethod == 1`

3. **Phase C: Pydantic Validation** (`phase_c_pydantic_report.py`)
   - Runs Pydantic data model validation
   - Generates detailed error reports
   - Provides actionable feedback

### Components

- **`orchestrator.py`**: Main entry point that coordinates the pipeline
- **`validation_helpers.py`**: Shared validation utilities (legacy precheck functions)
- **`__init__.py`**: Module exports

## Usage

### Command Line

```bash
python -m supy.data_model.yaml_processor.orchestrator user_config.yml --phase ABC --mode public
```

### Python API

```python
from supy.data_model.yaml_processor import orchestrator

# Run complete pipeline
orchestrator.main([
    'user_config.yml',
    '--phase', 'ABC',
    '--mode', 'public'
])
```

### Phase Options

- `A`: Phase A only (parameter updates)
- `B`: Phase B only (scientific validation)
- `C`: Phase C only (Pydantic validation)
- `AB`: Phases A and B
- `AC`: Phases A and C
- `BC`: Phases B and C
- `ABC`: Complete pipeline (default)

### Modes

- `public`: Standard mode with all validation checks
- `dev`: Developer mode with relaxed constraints

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
    'localclimatemethod': 'rsllevel'
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

## Future Enhancements

- [ ] Add Phase D for post-processing optimisation
- [ ] Implement parallel site processing
- [ ] Add configuration caching
- [ ] Create GUI interface
- [ ] Add batch processing support