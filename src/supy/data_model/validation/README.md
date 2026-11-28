# SUEWS Validation Module

This package implements the three-phase validation system used by
`suews-validate`.

## Module Structure

```text
validation/
|-- engine.py                  # Unified ValidationEngine and supy.validate_config()
|-- required_fields.py         # Single source of truth for required fields
|-- core/
|   |-- utils.py               # Shared utility functions
|   `-- yaml_helpers.py        # YAML scalar/RefValue helpers and CRU/DLS lookup
|-- rules/
|   |-- typed_view.py          # Deeply immutable typed view over YAML mappings
|   |-- readonly_model.py      # Deeply immutable proxy over Pydantic models
|   |-- hard_invariants.py     # Tier-1 hard invariants shared with Pydantic models
|   `-- advisory.py            # Tier-2 scientific advisories (Phase B warnings)
`-- pipeline/
    |-- document.py            # ConfigDocument, RefValue helpers, ConfigPatch
    |-- style_ledger.py        # Per-path YAML authoring style ledger and dumper
    |-- patch_normaliser.py    # Patch normalisation with original values and inversion
    |-- orchestrator.py        # Transactional staged pipeline orchestration
    |-- patches.py             # Pure Phase B scientific patch planners
    |-- phase_a.py             # Schema-driven structural completeness and renames
    |-- phase_b.py             # Scientific validation and patch application
    |-- phase_b_rules/         # Ordered Phase B validation rules
    |-- phase_c.py             # Single-pass Pydantic model validation
    |-- renderers.py           # Pure structured text report renderer
    |-- report_schema.py       # Canonical Issue / PhaseReport / ValidationReport
    `-- report_writer.py       # Report file I/O
```

## Three-Phase Validation System

### Phase A: Completeness Check

- Introspects `SUEWSConfig` directly to detect missing critical physics options
  and undeclared extra parameters (without padding omitted optional fields with
  nulls).
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

The preferred Python entry point is `supy.validate_config()` (or
`ValidationEngine` directly), which runs in memory with zero disk writes by
default and supports `write_outputs=True` for staged file publication:

```python
import supy

report = supy.validate_config("config.yml", pipeline="ABC")
if not report.is_valid:
    print(report.to_text())
```

The lower-level phase runners (`run_pipeline`, `run_phase_a`, `run_phase_b`,
`run_phase_c`) live in `supy.data_model.validation.pipeline.orchestrator`.

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
