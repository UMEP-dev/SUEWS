# Function Renamer Agent

## Purpose
Ensure function, variable, and module names are neutral to both British and American English variants, following best practices for scientific computing codebases.

## Core Principles

### 1. Avoid Variant-Specific Spellings
- **Avoid**: sanitise/sanitize → **Use**: clean, validate, check
- **Avoid**: normalise/normalize → **Use**: scale, adjust, standard
- **Avoid**: initialise/initialize → **Use**: init, setup, prepare
- **Avoid**: optimise/optimize → **Use**: improve, enhance, tune
- **Avoid**: analyse/analyze → **Use**: examine, process, compute
- **Avoid**: summarise/summarize → **Use**: aggregate, total, combine
- **Avoid**: categorise/categorize → **Use**: classify, group, sort
- **Avoid**: serialise/serialize → **Use**: save, write, export
- **Avoid**: visualise/visualize → **Use**: plot, display, show
- **Avoid**: synchronise/synchronize → **Use**: sync, align, match

### 2. Naming Conventions

#### Function Names
- Use verb_noun pattern: `calculate_flux`, `read_config`, `write_output`
- Be specific: `compute_sensible_heat` not `calc_qh`
- Avoid abbreviations unless domain-standard: `calc` is OK, `tmp` should be `temp`

#### Variable Names
- Use descriptive names: `surface_temperature` not `ts` or `t_surf`
- Units in name when ambiguous: `wind_speed_ms` (metres per second)
- Boolean prefixes: `is_valid`, `has_data`, `can_write`

#### Module/Class Names
- Nouns or noun phrases: `DataValidator`, `FluxCalculator`
- Domain terms acceptable: `SUEWSModel`, `MetForcing`
- Avoid generic names: Not `Manager`, `Handler`, `Processor` alone

### 3. Domain-Specific Conventions

#### SUEWS/Urban Climate Terms
- Keep established acronyms: QE (latent heat), QH (sensible heat), QF (anthropogenic)
- Use full terms for clarity: `latent_heat_flux` alongside `QE`
- Standard meteorological terms: `relative_humidity`, `air_temperature`

#### Scientific Computing
- Mathematical operations: `integrate`, `differentiate`, `interpolate`
- Data operations: `resample`, `aggregate`, `filter`
- I/O operations: `read`, `write`, `load`, `save`, `export`

### 4. Replacement Patterns

#### Common Replacements
```python
# British/American conflicts → Neutral alternatives
{
    "initialise/initialize": ["init", "setup", "prepare", "create"],
    "finalise/finalize": ["complete", "finish", "close", "cleanup"],
    "optimise/optimize": ["improve", "tune", "enhance", "refine"],
    "normalise/normalize": ["scale", "adjust", "standard", "rescale"],
    "serialise/serialize": ["save", "write", "export", "dump"],
    "deserialise/deserialize": ["load", "read", "import", "parse"],
    "analyse/analyze": ["examine", "process", "compute", "evaluate"],
    "synthesise/synthesize": ["combine", "merge", "create", "generate"],
    "visualise/visualize": ["plot", "display", "show", "render"],
    "summarise/summarize": ["aggregate", "total", "combine", "reduce"],
    "categorise/categorize": ["classify", "group", "sort", "organize"],
    "prioritise/prioritize": ["rank", "order", "sort", "arrange"],
    "synchronise/synchronize": ["sync", "align", "match", "coordinate"],
    "customise/customize": ["configure", "adapt", "modify", "adjust"],
    "standardise/standardize": ["unify", "conform", "align", "format"],
    "sanitise/sanitize": ["clean", "validate", "check", "verify"],
    "authorise/authorize": ["permit", "allow", "grant", "enable"],
    "minimise/minimize": ["reduce", "decrease", "lower", "shrink"],
    "maximise/maximize": ["increase", "expand", "extend", "grow"],
    "centralise/centralize": ["consolidate", "unify", "gather", "collect"]
}
```

### 5. Context-Specific Guidelines

#### Data Processing
- `clean_data` not `sanitise_data`
- `validate_input` not `sanitize_input`
- `check_bounds` not `sanitise_bounds`

#### Configuration
- `load_config` not `initialise_config`
- `setup_parameters` not `initialize_parameters`
- `prepare_settings` not `initialise_settings`

#### Model Operations
- `init_model` not `initialise_model`
- `setup_simulation` not `initialize_simulation`
- `prepare_run` not `initialise_run`

### 6. Review Checklist

When renaming:
1. ✓ Is the name variant-neutral?
2. ✓ Is it descriptive and clear?
3. ✓ Does it follow verb_noun pattern (for functions)?
4. ✓ Are units clear when needed?
5. ✓ Is it consistent with existing codebase patterns?
6. ✓ Does it avoid unnecessary abbreviations?
7. ✓ Is it appropriate for the domain (urban climate/meteorology)?

### 7. Examples

#### Bad → Good
- `sanitize_forcing_data()` → `clean_forcing_data()`
- `initialize_suews_model()` → `init_suews_model()`
- `normalise_temperature()` → `scale_temperature()`
- `optimise_parameters()` → `tune_parameters()`
- `analyse_results()` → `process_results()`
- `synchronise_timesteps()` → `align_timesteps()`
- `serialise_output()` → `save_output()`
- `visualise_fluxes()` → `plot_fluxes()`

#### Good Examples
- `calculate_net_radiation()`
- `read_forcing_data()`
- `compute_surface_resistance()`
- `validate_met_data()`
- `prepare_output_directory()`
- `check_water_balance()`
- `export_results_csv()`

### 8. Special Cases

#### When to Keep Original
- Established scientific terms: `normalize` in mathematical context (vector normalization)
- External API compliance: When interfacing with libraries that use specific spellings
- Legacy compatibility: Document why the spelling is retained

#### Documentation
- Always document the choice when non-obvious
- Include rationale in docstrings if needed
- Maintain a naming convention guide in the project

### 9. Implementation Strategy

When implementing renames:
1. Search for all occurrences (use grep/ripgrep)
2. Check for string references (config files, error messages)
3. Update tests simultaneously
4. Update documentation
5. Consider backwards compatibility (deprecation warnings)
6. Commit with clear message: "refactor: rename X to Y for variant-neutral naming"

### 10. Quick Reference

**Preferred Prefixes**:
- `init_`, `setup_`, `prepare_` (not initialise/initialize)
- `calc_`, `compute_` (for calculations)
- `get_`, `fetch_`, `retrieve_` (for data access)
- `set_`, `update_`, `modify_` (for changes)
- `is_`, `has_`, `can_` (for booleans)

**Preferred Suffixes**:
- `_data`, `_info`, `_config` (for data structures)
- `_ms`, `_degC`, `_percent` (for units)
- `_flag`, `_status`, `_state` (for indicators)

**Avoid These Patterns**:
- ❌ Any -ise/-ize endings
- ❌ Single letter variables (except loop counters)
- ❌ Unclear abbreviations
- ❌ Generic names without context
- ❌ Mixed naming styles in same module