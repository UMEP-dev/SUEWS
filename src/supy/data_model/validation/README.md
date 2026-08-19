# SUEWS Validation Module

This package implements the three-phase validation system used by
`suews-validate`.

## Module Structure

```text
validation/
├── core/
│   ├── controller.py          # Deprecated compatibility controller
│   ├── result.py              # Shared result helpers
│   └── utils.py               # Utility functions
└── pipeline/
    ├── orchestrator.py        # Phase orchestration and report consolidation
    ├── phase_a.py             # Structural completeness and rename checks
    ├── phase_b.py             # Scientific validation and optional fixes
    ├── phase_b_rules/         # Ordered Phase B validation rules
    └── report_writer.py       # Report file I/O
```

## Three-Phase Validation System

### Phase A: Completeness Check

- Adds missing optional parameters with null placeholders.
- Updates deprecated parameter names to current snake_case names.
- Validates forcing data unless `--forcing off` is selected.
- Leaves blocking missing physics options for the report.

### Phase B: Scientific Validation

- Runs deterministic validation rules for physics parameters, option
  dependencies, land cover, geography, irrigation, STEBBS, radiation/albedo,
  emissivity, and forcing height.
- Collects scientific initialisation suggestions by default.
- Applies scientific transformations only when `--science-fixes apply` is used.

Scientific transformations include CRU-derived initial temperatures, annual and
monthly temperature metrics, DLS/timezone suggestions, deciduous LAI seasonality,
vegetation albedo, snow albedo nullification, CO2/STEBBS nullification,
setpoint/profile cleanup, WWR-dependent nullification, and small land-cover
fraction normalisation within `1e-4`.

### Phase C: Model Compatibility

- Validates the YAML against the SUEWS data model.
- Checks runtime blockers and critical null physics parameters.
- Produces the final YAML/report when used in a combined pipeline.

## Public Interfaces

The preferred public interface is the package pipeline and CLI:

```python
from supy.data_model.validation.pipeline.orchestrator import (
    run_phase_a,
    run_phase_b,
    run_phase_c,
)
```

`src/supy/validation.py` is retained as a backward-compatible shim for one
release and emits `DeprecationWarning`. New code should use
`supy.data_model.validation`.

## CLI Integration

`suews-validate config.yml` runs the ABC pipeline and writes:

- `updated_config.yml`
- `report_config.txt`

Phase B scientific transformations are controlled by:

```bash
suews-validate --science-fixes suggest config.yml  # default
suews-validate --science-fixes apply config.yml
suews-validate --science-fixes off config.yml
```

Reports use `ACTION NEEDED`, `REVIEW ADVISED`, `SUGGESTED UPDATES`,
`APPLIED UPDATES`, and `INFO`.

## Development Guidelines

### Adding Phase B Rules

1. Add the rule implementation to the appropriate file in `phase_b_rules/`.
2. Register it with `RulesRegistry.add_rule("rule_id")`.
3. Add the rule id to `DEFAULT_RULE_ORDER` in `phase_b_rules/__init__.py`.
4. Return `ValidationResult` objects with stable severity values.

### Adding Scientific Transformations

Keep validation checks and transformations separate:

- validation checks return `ValidationResult`;
- transformations should be exposed through `collect_science_suggestions`;
- YAML mutation belongs behind `apply_science_suggestions`.

### Testing

Run targeted validation tests first:

```bash
pytest test/data_model/test_validation.py test/data_model/test_yaml_processing.py test/umep/test_preprocessor.py -v
```

Finish with:

```bash
make test-smoke
```
