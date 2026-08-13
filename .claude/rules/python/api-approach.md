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
| Impact studies | Config-level | Config modification for scenario construction |
| External coupling | Hybrid | DataFrame for forcing modification |

Always include a docstring note explaining the API approach used (why DataFrame extraction or config-level modification is chosen).

---

## Migration Checklist

When updating existing code:

- [ ] Replace direct DataFrame access with `sim.state_init`, `sim.forcing`
- [ ] Use `sim.results` instead of returned tuple unpacking
- [ ] Add context notes if hybrid approach is required
