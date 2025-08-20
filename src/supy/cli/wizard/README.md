# SUEWS Configuration Wizard

An interactive CLI wizard for creating SUEWS YAML configuration files with comprehensive validation and intelligent error correction.

## Features

### ✅ Complete Implementation
- **Basic Configuration**: Site information, coordinates, timezone, simulation period
- **Forcing Data**: File path, resolution, variable mapping
- **Surface Parameters**: Land cover fractions, albedo, emissivity
- **Initial Conditions**: Air temperature, humidity, soil moisture, surface temperatures, snow conditions
- **Advanced Options**: Full physics method selection with compatibility checking

### 🔬 Enhanced Validation System
The wizard now features a comprehensive validation system that combines multiple approaches:

#### Phase A - Parameter Detection
- Identifies missing critical and optional parameters
- Renames outdated parameters automatically
- Detects non-standard parameters
- Generates clean YAML with null placeholders

#### Phase B - Scientific Validation
- Checks physics option compatibility
- Validates parameter ranges
- Ensures scientifically sensible configurations
- Provides automatic corrections where possible

#### Phase C - Pydantic Validation
- Full data model validation
- Type checking and constraints
- Conditional validation based on physics options
- Comprehensive error reporting

### 🎯 Key Improvements
1. **Real-time Validation**: Each step validates input immediately
2. **Smart Defaults**: Context-aware defaults based on surface types
3. **Progressive Disclosure**: Only shows relevant options
4. **Error Recovery**: Automatic fixes suggested and applied
5. **Draft Saving**: Save progress and resume later

## Usage

### Basic Usage
```bash
# Create new configuration with wizard
suews-wizard new output.yaml

# Use a template
suews-wizard new --template urban output.yaml

# Edit existing configuration
suews-wizard edit existing.yaml

# Validate existing configuration
suews-wizard validate config.yaml
```

### Templates
Available templates:
- `urban`: High-density urban environment
- `suburban`: Mixed residential area
- `rural`: Low-density rural setting

## Architecture

### Module Structure
```
wizard/
├── cli.py                    # Click CLI commands
├── engine.py                 # Main wizard engine
├── steps/                    # Wizard steps
│   ├── base.py              # Base step class
│   ├── basic.py             # Basic configuration
│   ├── forcing.py           # Forcing data
│   ├── surface.py           # Surface parameters
│   ├── initial.py           # Initial conditions (IMPLEMENTED)
│   └── advanced.py          # Advanced options (IMPLEMENTED)
├── validators/               # Validation modules
│   ├── pydantic_integration.py        # Original Pydantic validation
│   ├── yaml_processor_integration.py  # Three-phase processor
│   └── enhanced_validator.py          # NEW: Comprehensive validation system
├── templates/                # Configuration templates
└── utils/                    # Utility modules
```

### Validation Flow

1. **During Input**: Quick field validation using `YAMLProcessorValidator.get_quick_validation()`
2. **After Each Step**: Step-specific validation
3. **Before Saving**: Complete three-phase validation:
   ```python
   Phase A → Parameter Detection → Auto-fix missing/renamed
   Phase B → Scientific Validation → Check compatibility
   Phase C → Pydantic Validation → Full model validation
   ```

## Enhanced Validation Features

### 🚀 New EnhancedWizardValidator
The wizard now includes an enhanced validator that combines all validation capabilities:

#### Features
- **Conditional Validation**: Only validates relevant parameters based on enabled physics methods
- **Structured Error Reporting**: Three levels - ERROR, WARNING, INFO
- **Machine-readable Error Codes**: For CI/CD integration
- **Automatic Corrections**: Fixes common issues automatically
- **JSON Export**: Export detailed validation reports for analysis
- **Re-validation**: Automatic re-validation after fixes are applied

#### Validation Components
1. **Schema Validation**: Checks structure against JSON schema
2. **Conditional Validation**: Physics method compatibility checks
3. **Three-phase Pipeline**: Complete A/B/C validation
4. **Fix Suggestions**: Intelligent suggestions based on errors

### Original YAMLProcessorValidator Class
```python
validator = YAMLProcessorValidator(mode="public")

# Individual phase validation
is_valid, messages, updated = validator.validate_phase_a(config)
is_valid, messages, updated = validator.validate_phase_b(config)
is_valid, messages = validator.validate_phase_c(config)

# All phases
is_valid, messages, final = validator.validate_all_phases(config)

# Quick field validation
is_valid, error = validator.get_quick_validation(field, value, context)

# Get fix suggestions
suggestions = validator.suggest_fixes(error_messages)
```

### Benefits of Integration
1. **Consistency**: Same validation as standalone processor
2. **Auto-fixing**: Automatic parameter updates and corrections
3. **Comprehensive**: All three validation phases available
4. **User-friendly**: Clear error messages and suggestions
5. **Mode Support**: Both "public" and "dev" modes

## Implementation Status

### Completed ✅
- [x] Core wizard infrastructure
- [x] Basic configuration step
- [x] Forcing data step
- [x] Surface parameters step
- [x] Initial conditions step (NEW)
- [x] Advanced options step (NEW)
- [x] Pydantic integration
- [x] YAML processor integration (NEW)
- [x] Template system
- [x] Draft saving/loading
- [x] Comprehensive validation
- [x] Unit tests
- [x] Integration tests (NEW)

### Future Enhancements
- [ ] Interactive parameter help system
- [ ] Graphical preview of configuration
- [ ] Batch configuration generation
- [ ] Configuration comparison tool
- [ ] Export to other formats

## Testing

Run tests with:
```bash
# All wizard tests
pytest test/cli/

# Integration tests specifically
pytest test/cli/test_wizard_integration.py -v

# With coverage
pytest test/cli/ --cov=src/supy/cli/wizard
```

## Development

### Adding New Steps
1. Create step class inheriting from `WizardStep`
2. Implement `collect_input()` and `validate()`
3. Add to `WizardEngine._initialize_steps()`

### Adding Validation Rules
1. Add to `YAMLProcessorValidator.get_quick_validation()` for field-level
2. Add to step's `validate()` for step-level
3. Scientific rules go in Phase B
4. Pydantic rules go in data model

## Related PRs
- #547: CLI wizard for YAML configuration creation (this PR)
- #580: SUEWS YAML Configuration Processor (integrated)
- #544: Original issue for wizard feature request