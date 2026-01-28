---
paths:
  - src/supy/**/*.py
  - docs/source/tutorials/**/*.py
---

# API Approach: SUEWSSimulation vs DataFrames

Guidance on the modern OOP interface versus the legacy DataFrame approach.

---

## Core Principle

**Use `SUEWSSimulation` class for all new scripts and tutorials.**

The class-based approach provides better validation, clearer workflows, and easier maintenance.

---

## Modern Approach (Recommended)

### Factory Methods

```python
from supy import SUEWSSimulation

# From built-in sample data
sim = SUEWSSimulation.from_sample_data()

# From existing state DataFrame
sim = SUEWSSimulation.from_state(df_state)

# From previous output (restart)
sim = SUEWSSimulation.from_output(path_output)
```

### Configuration

```python
# Method chaining
sim.update_forcing(df_forcing)
sim.update_config(key="value")

# Or YAML files (preferred for complex configurations)
sim = SUEWSSimulation.from_config("site_config.yaml")
```

### Execution and Results

```python
# Run simulation and capture output (preferred pattern)
output = sim.run()

# Access results via SUEWSOutput object
output.QH              # Sensible heat flux
output.SUEWS           # Main SUEWS output group
output.DailyState      # Daily state variables
output.df              # Raw DataFrame if needed

# Final state for restart
df_state_final = sim.state_final
```

### Deprecated Patterns

```python
# DEPRECATED - avoid in new code
sim.run()
df_output = sim.results  # Triggers deprecation warning
```

---

## Legacy Approach (Deprecated)

The DataFrame-centric approach using `init_supy()`, `run_supy()`, `save_supy()` directly:

```python
# DEPRECATED - avoid in new code
df_state_init, df_forcing = sp.load_forcing_grid(path)
df_output, df_state_final = sp.run_supy(df_forcing, df_state_init)
sp.save_supy(df_output, df_state_final, path_output)
```

**When legacy is acceptable:**
- Maintaining existing scripts
- Backwards compatibility requirements
- Direct Fortran driver integration

---

## Hybrid Pattern (Scenario Building)

For impact studies and scenario analysis, extracting DataFrames from the simulation object is appropriate:

```python
# Start with OOP interface
sim = SUEWSSimulation.from_sample_data()

# Extract DataFrames for modification
df_state_init = sim.state_init
df_forcing = sim.forcing

# Modify for scenarios
df_state_modified = df_state_init.copy()
df_state_modified.loc[:, ("alb", "(1,)")] = 0.5  # Change albedo

# Create new simulation from modified state
sim_scenario = SUEWSSimulation.from_state(df_state_modified)
sim_scenario.update_forcing(df_forcing)
sim_scenario.run()
```

This hybrid approach is acceptable for:
- Multi-scenario sensitivity analysis
- Parameter sweeps
- External model coupling (forcing modification)

---

## Tutorial Guidelines

| Tutorial Type | Approach | Notes |
|---------------|----------|-------|
| Quick start | Pure OOP | `from_sample_data()`, `run()`, `results` |
| Site setup | Hybrid | DataFrame extraction for parameter modification |
| Impact studies | Hybrid | DataFrame for scenario construction |
| External coupling | Hybrid | DataFrame for forcing modification |

Always include a docstring note explaining why DataFrame extraction is used when applicable.

---

## Migration Checklist

When updating existing code:

- [ ] Replace `sp.load_forcing_grid()` with `SUEWSSimulation.from_sample_data()`
- [ ] Replace `sp.run_supy()` with `sim.run()`
- [ ] Replace direct DataFrame access with `sim.state_init`, `sim.forcing`
- [ ] Use `sim.results` instead of returned tuple unpacking
- [ ] Add context notes if hybrid approach is required
