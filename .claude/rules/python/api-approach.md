---
paths:
  - src/supy/**/*.py
  - docs/source/tutorials/**/*.py
---

# API Approach: SUEWSSimulation vs DataFrames

Guidance on the modern OOP interface (`src/supy/suews_sim.py::SUEWSSimulation`) versus legacy DataFrame patterns.

---

## Core Principle

**Use `SUEWSSimulation` for all new scripts and tutorials.**

The class-based interface validates inputs through `SUEWSConfig`, wraps forcing and outputs in typed containers (`SUEWSForcing`, `SUEWSOutput`), and supports lossless typed checkpoint continuation (`SUEWSCheckpoint`).

---

## Modern Approach (Recommended)

### Initialisation and Factory Methods

```python
from supy import SUEWSSimulation

# From a YAML configuration file (or Path / full config dict / SUEWSConfig)
sim = SUEWSSimulation("site_config.yaml")

# From built-in sample data
sim = SUEWSSimulation.from_sample_data()

# Continuation from an in-memory SUEWSOutput instance
sim_next = SUEWSSimulation.from_output(output)

# Continuation from YAML config + saved checkpoint JSON / SUEWSCheckpoint
sim_cont = SUEWSSimulation.from_checkpoint("site_config.yaml", "checkpoint.json")

# Legacy DFState compatibility (saved CSV / Parquet path or state DataFrame)
sim_legacy = SUEWSSimulation.from_state("output/df_state.csv")
```

### Configuration and Forcing Updates

`update_config()` accepts `str | Path | dict | SUEWSConfig` (never arbitrary `**kwargs`). Partial update dicts mirror the nested YAML hierarchy and are re-validated through `SUEWSConfig`:

```python
# Update nested config fields and forcing (supports method chaining)
sim.update_config({"model": {"control": {"tstep": 300}}})
sim.update_forcing("forcing_2024.txt")

# Inspect forcing via the SUEWSForcing wrapper
sim.forcing.summary()
df_forcing = sim.forcing.df  # Underlying pandas DataFrame
```

### Execution and Results

`sim.run()` returns a `SUEWSOutput` instance:

```python
# Run simulation and capture SUEWSOutput (preferred pattern)
output = sim.run()

# Access results via SUEWSOutput
output.QH  # Sensible heat flux variable
output.SUEWS  # Main SUEWS output group DataFrame
output.DailyState  # Daily state variables
output.df  # Full MultiIndex output DataFrame
output.state_final  # Final state DataFrame
output.checkpoint  # Typed SUEWSCheckpoint for restart/continuation
```

### Deprecated and Invalid Patterns

```python
# DO NOT call non-existent SUEWSSimulation.from_config(...) -> pass path to SUEWSSimulation("site_config.yaml")
# DO NOT pass **kwargs to sim.update_config(key="value") -> pass a nested dict/Path/SUEWSConfig
# DO NOT pass a file path to SUEWSSimulation.from_output(...) -> pass a SUEWSOutput object (or use from_checkpoint / from_state for paths)

# DEPRECATED - do not use sim.results in new code or tutorials
output = sim.run()
# df_output = sim.results  # Emits DeprecationWarning; use output or output.df instead
```

---

## Hybrid Pattern (Scenario Building)

For impact studies, sensitivity sweeps, and external model coupling, extracting DataFrames from a configured simulation is appropriate:

```python
# Start with OOP interface
sim = SUEWSSimulation.from_sample_data()

# Extract DataFrames from the wrappers
df_state_init = sim.state_init
df_forcing = sim.forcing.df

# Modify for scenarios
df_state_modified = df_state_init.copy()
df_state_modified.loc[:, ("alb", "(0,)")] = 0.5

# Run modified scenario (note: from_state is the legacy DFState adapter;
# prefer sim.update_config(...) when the parameter is exposed in YAML)
sim_scenario = SUEWSSimulation.from_state(df_state_modified)
sim_scenario.update_forcing(df_forcing)
output_scenario = sim_scenario.run()
```

Prefer `sim.update_config({...})` whenever the scenario modifies YAML-backed parameters; reserve `state_init` / `forcing.df` DataFrame extraction for:
- Multi-scenario array sweeps over low-level state columns
- External model coupling (programmatic timestep forcing synthesis)

---

## Tutorial Guidelines

| Tutorial Type | Approach | Notes |
|---------------|----------|-------|
| Quick start | Pure OOP | `SUEWSSimulation.from_sample_data()`, `output = sim.run()`, `output.SUEWS` |
| Site setup | Pure OOP / YAML | `SUEWSSimulation("site_config.yaml")`, `sim.update_config({...})` |
| Impact studies | Config-level OOP (or Hybrid) | `sim.update_config({...})` for scenario diffs; state DataFrame extraction only when needed |
| External coupling | Hybrid | `sim.forcing.df` extraction and `sim.update_forcing(df_modified)` |

Always include a module-docstring note explaining why DataFrame extraction is used whenever a tutorial adopts the hybrid pattern.

---

## Migration Checklist

When updating existing scripts or tutorials:

- [ ] Load YAML configs via `SUEWSSimulation("site_config.yaml")` or `SUEWSSimulation.from_sample_data()`
- [ ] Pass nested dicts (`{"model": ...}`), paths, or `SUEWSConfig` to `sim.update_config(...)`
- [ ] Capture `output = sim.run()` and read `output.SUEWS` / `output.df` / `output.state_final` instead of deprecated `sim.results`
- [ ] Unwrap forcing DataFrames with `sim.forcing.df` when raw pandas operations are required
- [ ] Use `SUEWSSimulation.from_output(output)` or `SUEWSSimulation.from_checkpoint(config, checkpoint)` for continuations (reserving `from_state(path)` for legacy saved `DFState` CSV/Parquet files)
