# SUEWS Validation Module

This module implements the three-phase validation system for SUEWS configuration files.

## Module Structure

```
validation/
├── core/                       # Core validation infrastructure
│   ├── config_validation.py   # Configuration validation logic
│   ├── conditional_validation.py  # Physics-based conditional validation
│   ├── controller.py          # ValidationController class
│   ├── result.py              # ValidationResult class
│   ├── utils.py               # Utility functions
│   └── yaml_helpers.py        # YAML processing utilities
└── pipeline/                   # Three-phase validation pipeline
    ├── orchestrator.py        # Main pipeline orchestrator
    ├── phase_a_uptodate.py    # Phase A: Parameter updates
    ├── phase_b_science.py     # Phase B: Scientific validation
    ├── phase_c_pydantic.py    # Phase C: Pydantic validation
    └── phase_c_reports.py     # Phase C reporting
```

## Three-Phase Validation System

### Phase A: Completeness Check
- Adds missing parameters with sensible defaults
- Updates deprecated parameter names
- Ensures YAML structure compatibility

### Phase B: Scientific Validation
- Applies scientific constraints
- Performs automatic corrections (e.g., normalize fractions)
- Integrates CRU climatological data for temperature initialization

### Phase C: Model Compatibility
- Validates against Pydantic models
- Applies conditional validation rules
- Ensures physics options compatibility

## Public API

The module exports its public API through `__init__.py`:

```python
from .core.conditional_validation import validate_suews_config_conditional
from .core.controller import ValidationController
from .core.result import ValidationResult
```

## CLI Integration

The command-line interface in `src/supy/cmd/validate_config.py` uses the pipeline orchestrator functions:

```python
from ..data_model.validation.pipeline.orchestrator import (
    validate_input_file,
    setup_output_paths,
    run_phase_a,
    run_phase_b,
    run_phase_c,
)
```

## Development Guidelines

### Adding New Validation Rules

1. **Parameter validation**: Add to `core/config_validation.py`
2. **Scientific constraints**: Add to `pipeline/phase_b_science.py`
3. **Conditional rules**: Add to `core/conditional_validation.py`

### Testing

Run validation tests:
```bash
pytest test/data_model/test_validation.py -v
```

### CRU Data Integration

The module includes CRU TS4.06 climatological data (1991-2020) for automatic temperature initialization based on location and season. Data files are in `ext_data/`.

## Architecture Decisions

### Why Separate Core and Pipeline?

- **Core** contains reusable validation infrastructure
- **Pipeline** implements the specific three-phase workflow
- This separation allows for future alternative validation workflows

### Why Three Phases?

1. **Phase A**: Structural fixes can be done automatically
2. **Phase B**: Scientific corrections require domain knowledge
3. **Phase C**: Model compatibility must be checked last

Each phase builds on the previous one, creating a robust validation pipeline.

## Error Handling

- Phase A never fails (always produces output)
- Phase B fails on scientific violations
- Phase C fails on model incompatibilities
- Errors are accumulated and reported comprehensively

## For More Details

- See `pipeline/PHASE_A_DETAILED.md` for Phase A implementation
- See `pipeline/PHASE_B_DETAILED.md` for Phase B implementation  
- See `pipeline/PHASE_C_DETAILED.md` for Phase C implementation
- See `pipeline/ORCHESTRATOR.md` for workflow coordination