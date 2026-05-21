# SUEWS Validation Pipeline

## Overview

The validation pipeline updates and checks SUEWS YAML files in three phases:

```text
User YAML -> Phase A -> Phase B -> Phase C -> Valid YAML
```

`suews-validate config.yml` runs the full ABC pipeline by default.

## Phases

### Phase A: Configuration Structure Check (`phase_a.py`)

- Detects missing parameters against the sample configuration.
- Renames outdated keys to current snake_case names.
- Identifies non-standard parameters.
- Validates forcing data by default.
- Writes structural updates and null placeholders.

### Phase B: Physics and Scientific Validation (`phase_b.py`)

- Runs ordered validation rules from `phase_b_rules/`.
- Checks physics parameters, option dependencies, land-cover fractions,
  geography, irrigation, STEBBS, radiation/albedo/emissivity, and forcing height.
- Collects scientific initialisation suggestions by default.
- Applies those transformations only with `--science-fixes apply`.

Scientific suggestions are not treated as scientific truth. They can be useful
initialisation aids, but they may be inappropriate for observed initial states,
spin-up workflows, historical timezone settings, or specialist case studies.

### Phase C: Configuration Consistency Check

- Validates with the SUEWS data model.
- Checks critical null physics parameters that would block model execution.
- Produces a final report/YAML in combined pipelines.

## CLI Usage

```bash
# Complete validation, default scientific policy is suggest
suews-validate config.yml

# Apply Phase B scientific transformations to output YAML
suews-validate --science-fixes apply config.yml

# Disable Phase B scientific transformation suggestions
suews-validate --science-fixes off config.yml

# Run individual or combined phases
suews-validate --pipeline A config.yml
suews-validate --pipeline B config.yml
suews-validate --pipeline C config.yml
suews-validate --pipeline AB config.yml
suews-validate --pipeline AC config.yml
suews-validate --pipeline BC config.yml
```

## Python Entry Points

```python
from supy.data_model.validation.pipeline.orchestrator import (
    validate_input_file,
    setup_output_paths,
    run_phase_a,
    run_phase_b,
    run_phase_c,
)
```

Direct module use:

```bash
python -m supy.data_model.validation.pipeline.orchestrator user_config.yml --phase ABC --mode public
```

## Report Sections

Reports use:

- `ACTION NEEDED`: blocking errors
- `REVIEW ADVISED`: warnings
- `SUGGESTED UPDATES`: scientific suggestions not applied to YAML
- `APPLIED UPDATES`: applied structural or scientific updates
- `INFO`: non-blocking notes

Warnings should never be placed under `NO ACTION NEEDED`.

## Development Notes

### Phase B Rule Order

Rule registration is global, but execution order is explicit in
`phase_b_rules.DEFAULT_RULE_ORDER`:

1. physics parameters
2. option dependencies
3. land cover
4. geography
5. irrigation
6. STEBBS and building rules
7. radiation, albedo, emissivity, and forcing-height checks

### Scientific Transformations

Keep checks and transformations separate:

```python
suggestions = collect_science_suggestions(data, start_date, model_year)
updated_data, applied = apply_science_suggestions(
    data, suggestions, start_date, model_year
)
```

`collect_science_suggestions` must not mutate the input data.

## Tests

Targeted validation tests:

```bash
pytest test/data_model/test_validation.py test/data_model/test_yaml_processing.py test/umep/test_preprocessor.py -v
```

Smoke check:

```bash
make test-smoke
```
