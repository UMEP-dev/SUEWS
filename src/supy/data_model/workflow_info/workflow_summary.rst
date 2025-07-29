.. _workflow_validation:

Validation Workflow
===================
This page describes the validation workflow.
Workflow Summary
----------------
The process consists of four key steps:
1. Convert SUEWS namelist to YAML
2. Detect and fix common input errors
3. Run precheck
4. Perform conditional validation via Pydantic

Step 0 — Namelist to YAML
-------------------------
Converts the Fortran-style SUEWS namelist into a structured YAML format.
.. note::
  This code should be available via MP.

Step 1 — Common Mistakes
------------------------
Detects and optionally corrects:
- Missing parameters
- Deprecated parameter names
.. note::
  This step is currently partially done by precheck and needs to be implemented as a Step 1 of the workflow.

Step 2 — Precheck
-----------------
The precheck performs physical checks on initial states, site properties and land cover and, consequently, updates the YAML to be ready for Pydantic conditional validation.
Output: an updated YAML  saved as py0_<filename>.yml and a CSV report listing all changes.
.. note::
  The output will be changed to have a single file (the py0 updated yaml) with commented the parameters that have been updated by the precheck.

Step 3 — Pydantic Conditional Validation
----------------------------------------
Pydantic performs validation of a YAML file according to selected model options.
Output: An annotated YAML with inline error messages
.. note::
  The output will be changed to produce also an updated YAML file (py1_<filename>.yml) with comments at the level of the parameters that have been updated according to conditional validation. On top of that, the annotated YAML will be revised to work correctly.