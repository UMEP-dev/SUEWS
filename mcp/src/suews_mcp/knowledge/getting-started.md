# Getting Started with SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) simulates exchanges of energy, water, and carbon for urban neighbourhoods.

It is designed for applications such as:
- urban climate impact studies;
- heat mitigation and adaptation planning;
- interpretation of eddy-covariance and surface flux observations.

A typical run needs:
- a YAML configuration (sites, physics choices, and parameters);
- meteorological forcing data;
- a simulation period and output settings.

In this MCP workflow, the normal sequence is:
1. use `search()` to inspect type names and schema;
2. draft or refine YAML from schema details;
3. use `execute()` to run the simulation and return a compact summary;
4. use `explain()` for concepts and interpretation support.
