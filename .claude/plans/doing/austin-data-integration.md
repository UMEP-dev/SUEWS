# Austin Data Integration Plan

## Overview
**Feature**: Austin Data Integration with YAML Processor
**Issue**: #547 - CLI wizard for YAML configuration creation
**Base Work**: #580 - SUEWS YAML Configuration Processor

## Objective
Integrate Austin meteorological data into SUEWS using the YAML processor workflow from PR #580 and potentially enhance the wizard from PR #547 to support Austin-specific configurations.

## Background

### PR #580 - YAML Processor (Completed)
The YAML processor provides a three-phase validation system:
- **Phase A**: Parameter detection, missing/outdated parameter handling
- **Phase B**: Scientific validation (physics checks)
- **Phase C**: Pydantic conditional validation

Key components:
- `orchestrator.py`: Master workflow controller
- `phase_a_parameter_update.py`: Parameter completeness checking
- `phase_b_science_check.py`: Scientific validation
- `phase_c_pydantic_report.py`: Pydantic validation and reporting

### PR #547 - CLI Wizard (In Progress)
The wizard provides an interactive CLI for creating YAML configs:
- Interactive prompts with validation
- Template support (urban, suburban, rural)
- Integration with Pydantic data model
- State management with undo/redo

## Implementation Plan

### Phase 1: Austin Data Preparation
1. **Create test directory structure**
   ```
   test/austin/
   ├── forcing/           # Austin meteorological data
   ├── config/           # YAML configurations
   ├── templates/        # Austin-specific templates
   └── validation/       # Validation results
   ```

2. **Prepare Austin forcing data**
   - Source Austin meteorological data
   - Format according to SUEWS requirements
   - Create sample forcing file (e.g., `Austin_2023_data_60.txt`)

### Phase 2: Austin YAML Template Creation
1. **Create base Austin configuration**
   - Site-specific parameters (latitude, longitude, timezone)
   - Austin-specific surface characteristics
   - Local climate parameters

2. **Integrate with existing templates**
   - Add Austin template alongside urban/suburban/rural
   - Include Austin-specific defaults

### Phase 3: Validation Pipeline Integration
1. **Run Phase A validation**
   - Check for missing parameters
   - Update outdated parameter names
   - Generate clean YAML with placeholders

2. **Run Phase B validation**
   - Apply scientific checks
   - Validate physics options
   - Check parameter ranges

3. **Run Phase C validation**
   - Pydantic conditional validation
   - Generate comprehensive report

### Phase 4: Wizard Enhancement (Optional)
1. **Add Austin-specific steps**
   - Austin data source selection
   - Local vegetation parameters
   - Urban heat island considerations

2. **Integrate with processor**
   - Call processor phases from wizard
   - Display validation results interactively

## Technical Approach

### 1. Austin Configuration Template
```yaml
# austin_template.yaml
site_info:
  location: Austin, Texas
  latitude: 30.2672
  longitude: -97.7431
  timezone: -6  # CST
  
forcing:
  path: test/austin/forcing/Austin_2023_data_60.txt
  resolution: 3600
  
surface:
  # Austin-specific land cover fractions
  fr_paved: 0.35
  fr_bldg: 0.25
  fr_evergreen: 0.10
  fr_deciduous: 0.15
  fr_grass: 0.10
  fr_baresoil: 0.03
  fr_water: 0.02
  
  # Austin climate characteristics
  albedo_mean: 0.18
  emissivity_mean: 0.95
```

### 2. Validation Workflow
```python
# austin_validator.py
from supy.data_model.yaml_processor.orchestrator import main

def validate_austin_config(config_path):
    """Validate Austin configuration through all phases"""
    # Run complete ABC validation
    result = main([config_path, '--phase', 'ABC', '--mode', 'public'])
    return result
```

### 3. Integration with Wizard
```python
# Add to wizard templates
AUSTIN_TEMPLATE = {
    'name': 'austin',
    'description': 'Austin, Texas urban configuration',
    'defaults': {
        'latitude': 30.2672,
        'longitude': -97.7431,
        'timezone': -6,
        # ... more Austin defaults
    }
}
```

## Testing Strategy

### Unit Tests
1. Test Austin template loading
2. Test Austin-specific validation rules
3. Test forcing data parsing

### Integration Tests
1. Full workflow test (Phase A→B→C)
2. Wizard with Austin template
3. Simulation run with validated config

### Validation Tests
1. Compare with known Austin climate data
2. Verify physics parameter ranges
3. Check output consistency

## Success Criteria
1. ✅ Austin forcing data successfully loaded
2. ✅ Austin YAML configuration passes all validation phases
3. ✅ SUEWS simulation runs with Austin data
4. ✅ Output validates against expected ranges
5. ✅ Documentation complete

## Next Steps
1. Gather Austin meteorological data
2. Create initial YAML configuration
3. Run validation pipeline
4. Iterate based on validation results
5. Add to test suite

## References
- PR #580: YAML Configuration Processor
- PR #547: CLI Wizard for YAML configuration
- Austin Climate Data: [source needed]
- SUEWS Documentation: [link]