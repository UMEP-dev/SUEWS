Validation Module Structure
============================

Overview
--------

The SUEWS validation system is organized under ``src/supy/data_model/validation/`` with a clear separation between core infrastructure and the three-phase validation pipeline.

Module Organisation
-------------------

The validation module is structured as follows:

.. code-block:: text

    src/supy/data_model/validation/
    ├── __init__.py                 # Public API exports
    ├── core/                       # Core validation infrastructure
    │   ├── __init__.py
    │   ├── config_validation.py   # Configuration validation logic
    │   ├── conditional_validation.py  # Physics-based conditional validation
    │   ├── controller.py          # ValidationController class
    │   ├── result.py              # ValidationResult class
    │   ├── utils.py               # Utility functions
    │   └── yaml_helpers.py        # YAML processing utilities
    └── pipeline/                   # Three-phase validation pipeline
        ├── __init__.py
        ├── orchestrator.py        # Main pipeline orchestrator
        ├── phase_a_uptodate.py    # Phase A: Parameter updates
        ├── phase_b_science.py     # Phase B: Scientific validation
        ├── phase_c_pydantic.py    # Phase C: Pydantic validation
        └── phase_c_reports.py     # Phase C reporting

Core Infrastructure (``core/``)
--------------------------------

The ``core`` subdirectory contains the fundamental validation infrastructure:

- **config_validation.py**: Main configuration validation logic and rules
- **conditional_validation.py**: Physics-based conditional validation that ensures parameter compatibility
- **controller.py**: The ``ValidationController`` class that manages the validation process
- **result.py**: The ``ValidationResult`` class for storing validation outcomes
- **utils.py**: Shared utility functions for validation operations
- **yaml_helpers.py**: YAML processing utilities including CRU data integration

Pipeline Components (``pipeline/``)
------------------------------------

The ``pipeline`` subdirectory implements the three-phase validation system:

**Phase A - Parameter Updates**
  - Updates YAML structure to latest format
  - Detects missing parameters
  - Renames deprecated parameters
  - Adds default values where appropriate

**Phase B - Scientific Validation**
  - Applies scientific constraints
  - Performs automatic corrections
  - Integrates CRU climatological data
  - Ensures physical consistency

**Phase C - Pydantic Validation**
  - Validates against Pydantic models
  - Applies conditional validation rules
  - Ensures model compatibility
  - Generates comprehensive reports

Public API
----------

The validation module exports its public API through ``src/supy/data_model/validation/__init__.py``:

.. code-block:: python

    from .core.conditional_validation import validate_suews_config_conditional
    from .core.controller import ValidationController
    from .core.result import ValidationResult

This provides a clean interface for other modules to use validation functionality.

Integration with CLI
--------------------

The command-line interface in ``src/supy/cmd/validate_config.py`` imports from the validation module:

.. code-block:: python

    from ..data_model.validation.pipeline.orchestrator import (
        validate_input_file,
        setup_output_paths,
        run_phase_a,
        run_phase_b,
        run_phase_c,
    )

This separation ensures clean architecture and maintainability.

Migration from Previous Structure
----------------------------------

The validation module was previously split between:
- ``src/supy/data_model/yaml_processor/``
- ``src/supy/data_model/validation/``

These have been consolidated under a single ``validation/`` directory with clear separation between core infrastructure and pipeline components. This reorganization:

1. Eliminates confusion from duplicate modules
2. Provides clearer code organization
3. Makes the validation pipeline more discoverable
4. Maintains backward compatibility through proper imports

Developer Notes
---------------

When working with the validation module:

1. **Core changes**: Modify files in ``core/`` for fundamental validation logic
2. **Pipeline changes**: Modify files in ``pipeline/`` for phase-specific logic
3. **Public API**: Update ``__init__.py`` exports when adding new public functionality
4. **Documentation**: Update this file when structural changes are made

Related Documentation
---------------------

- :doc:`index` - Overview of the three-phase validation system
- :doc:`processor_detailed` - Detailed orchestrator documentation
- `Phase A Details <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/data_model/validation/pipeline/PHASE_A_DETAILED.md>`_
- `Phase B Details <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/data_model/validation/pipeline/PHASE_B_DETAILED.md>`_
- `Phase C Details <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/data_model/validation/pipeline/PHASE_C_DETAILED.md>`_