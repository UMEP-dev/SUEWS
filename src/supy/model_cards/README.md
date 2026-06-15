# SUEWS Model Cards

Structured documentation for SUEWS physics schemes, inspired by ML model cards.

## Architecture Decision: Hybrid YAML + Enum

**Decision date:** 2026-02-19

**Context:** SUEWS physics schemes are configured via Python Enum classes in
`src/supy/data_model/core/model.py`. Each enum already contains a docstring
documenting its options (integer values, names, one-line descriptions). Model
cards add scheme-level documentation: scientific basis, evaluation evidence,
user guidance, governance, etc.

**Options considered:**

- **Fully separate (YAML only):** YAML files contain all information including
  option descriptions. Risk of duplication and drift with the enum docstrings.
- **Fully embedded (Python only):** All model card metadata attached to enum
  classes via decorators or class attributes. Bloats `model.py` with
  prose-heavy content; mixes runtime code with documentation metadata;
  granularity mismatch (enums are method *options*, model cards are *schemes*,
  and the mapping is not 1:1).
- **Hybrid (chosen):** YAML files are the source for scheme-level prose. Each
  YAML card references its parent enum class (`enum_class`) and the specific
  option values it covers (`enum_values`). The RST generator auto-pulls option
  descriptions from the enum docstrings at build time.

**Rationale:**

- **Single source of truth per concern:** Enums own option-level detail (value,
  name, one-line description). YAML owns scheme-level analysis (scientific
  basis, evaluation, guidance).
- **No duplication:** The RST generator reads the enum docstring directly to
  produce the "Configuration Options" section. No need to restate option
  descriptions in the YAML.
- **Granularity handled naturally:** One enum class (e.g., `StorageHeatMethod`)
  can house multiple schemes (OHM, STEBBS, EHC, ...), each with its own YAML
  card pointing to different `enum_values`.
- **Maintainability:** Updating an option description means editing one place
  (the enum docstring). Adding a new scheme means adding one YAML file.

## File layout

```
src/supy/model_cards/
  _schema.py      # Pydantic schema for YAML validation
  __init__.py     # Public API: ModelCard, load_card, load_all_cards
  README.md       # This file
  narp.yaml       # Pilot card: Net All-wave Radiation Parameterisation
  ohm.yaml        # Pilot card: Objective Hysteresis Model
  stebbs.yaml     # Pilot card: STEBBS

docs/
  generate_model_cards_rst.py   # RST generator (reads YAML + enum docstrings)
  source/scheme-reference/      # Generated RST output (do not edit by hand)
```

## Adding a new model card

1. Create `<scheme_name>.yaml` in this directory.
2. Follow the schema defined in `_schema.py` (validated via Pydantic).
3. Set `enum_class` to the Python enum name (e.g., `StorageHeatMethod`).
4. Set `enum_values` to the list of integer values this scheme covers.
5. Run `python docs/generate_model_cards_rst.py` to regenerate RST.
6. Verify with `make docs`.

## Enum docstring format

For option descriptions to be auto-extracted, enum docstrings must use this
format (one option per line after the summary paragraph):

```
NUMBER: NAME - Description text
```

Example from `StorageHeatMethod`:

```
1: OHM_WITHOUT_QF - Objective Hysteresis Model using Q* only (use with OhmIncQf=0)
7: STEBBS - use STEBBS storage heat flux for building, others use OHM
```
