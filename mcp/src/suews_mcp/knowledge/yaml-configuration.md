# YAML Configuration

A SUEWS YAML file normally contains:
- metadata (`name`, `description`);
- `model.control` (time period, forcing path, outputs, diagnostics);
- `model.physics` (method choices);
- `sites` (location, geometry, parameters, and fractions).

Best practice:
- start from a validated example;
- change one group of settings at a time;
- re-run and compare key fluxes (`Kdown`, `QH`, `QE`, `QS`).

Use `search(type_name="suews-config", detail_level="schema")` for field-level guidance.
