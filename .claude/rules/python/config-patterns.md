---
paths:
  - src/supy/**/*.py
---

# Configuration Patterns

Separation of concerns between configuration parsing and implementation.

---

## Core Principle

**High-level classes** parse configuration; **low-level methods** accept explicit parameters.

---

## Validated Configuration Boundary

Raw mappings are input data, not configuration objects. This includes
YAML-loaded dictionaries, JSON-like payloads, and partial update dictionaries.

- **DO**: Route full mapping inputs through the canonical Pydantic model path
  (`SUEWSConfig` or another schema-owned constructor) so field renames,
  readable physics names, `RefValue` wrappers, defaults, and conditional
  validators all run.
- **DO**: Keep equivalent public input forms semantically equivalent. A YAML
  file, an in-memory mapping loaded from that YAML, and a `SUEWSConfig` object
  should reach the same normalisation and validation path unless the API
  documents a narrower contract.
- **DO**: For partial updates, merge at the plain-data layer and revalidate the
  resulting model. The merge function may prepare data; Pydantic remains the
  validation boundary.
- **DON'T**: Recursively `setattr` user mappings into existing Pydantic model
  instances as the mechanism for parsing or validating configuration. That
  bypasses field validators and breaks wrapped fields such as physics selectors.
- **DON'T**: Infer special semantics from a Python container type alone. Lists
  replace by default unless an API explicitly documents collection-specific
  patch semantics.

If collection-specific updates are needed, expose them as named API behaviour
(for example, "update site by name/index") rather than guessing that every
list-valued config field is that collection.

Validation bypasses such as `model_construct`, `use_conditional_validation=False`,
or backend-specific unchecked paths must be named, isolated, and covered by
tests that state the bypass contract explicitly.

---

## High-Level Classes (e.g., SUEWSSimulation)

- **DO**: Parse and interpret configuration objects
- **DO**: Extract specific values from nested config structures
- **DO**: Handle RefValue wrappers and type conversions
- **DO**: Transform config data into concrete parameters
- **DON'T**: Pass configuration objects to lower-level methods

## Low-Level Methods (e.g., save_supy, run_supy)

- **DO**: Accept explicit, typed parameters (int, str, float, etc.)
- **DO**: Focus on core functionality without config knowledge
- **DON'T**: Accept configuration objects as parameters
- **DON'T**: Import or depend on configuration classes

---

## Example

```python
# WRONG - passing config directly
def save(self, output_path, format=None):
    if format == "txt":
        save_supy(df_output, df_state, output_config=self._config.output)

# CORRECT - extracting and transforming config
def save(self, output_path, format=None):
    if format == "txt":
        # Extract specific parameters from config
        freq_s = 3600  # default
        if self._config and hasattr(self._config.output, 'freq'):
            freq_s = self._config.output.freq.value  # Handle RefValue

        # Pass concrete parameters to low-level method
        save_supy(df_output, df_state, freq_s=int(freq_s), site=site_name)
```

---

## Rationale

1. **Reusability**: Low-level methods remain usable without config objects
2. **Testing**: Easier to test methods with explicit parameters
3. **Clarity**: Clear contracts - methods declare exactly what they need
4. **Flexibility**: Config structure can change without affecting core methods
5. **Backwards Compatibility**: Existing code using explicit parameters works

---

## Variable Definition Patterns

### Output Variables (Python-Only)

All 1,100+ output variables defined in Python/Pydantic as single source of truth.

**Location**: `src/supy/data_model/output/`

```python
# Adding a new output variable
OutputVariable(
    name="NewVar",
    unit="W m-2",
    description="Description of new variable",
    aggregation=AggregationMethod.AVERAGE,
    group=OutputGroup.SUEWS,
    level=OutputLevel.DEFAULT,
)
```

### Input Configuration (Dual-Source)

- **Fortran**: TYPE definitions and runtime calculations
- **Python**: Pydantic models for YAML parsing, validation, documentation

---

## Implementation Checklist

When implementing a feature that uses configuration:

- [ ] Identify concrete parameters the low-level method needs
- [ ] Extract values in the high-level class
- [ ] Handle RefValue wrappers (check for `.value` attribute)
- [ ] Convert types as needed (e.g., ensure integers)
- [ ] Pass only primitive types or simple objects to low-level methods
- [ ] Keep configuration parsing logic in one place
- [ ] Route raw mapping inputs through the validated config model path
- [ ] Revalidate after partial config merges; do not mutate Pydantic model
      internals with recursive `setattr`
- [ ] Define list update semantics explicitly; do not assume list-valued fields
      are site collections
