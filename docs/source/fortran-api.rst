.. _fortran_api_reference:

Fortran API Reference (Developers)
===================================

.. warning::

   **For Developers Only**: This documentation is intended for SUEWS core developers working on the Fortran physics engine. Most users should use the :doc:`Python API <api>` instead.

Overview
--------

The Fortran API provides low-level access to SUEWS physics calculations and is primarily used for:

- **Model Development**: Adding new physics modules and parameterisations
- **Performance Optimisation**: Direct access to computation kernels
- **External Coupling**: Low-level integration with other atmospheric models
- **Core Physics Modification**: Extending or modifying the physics engine

Core Architecture
------------------

The SUEWS Fortran codebase follows a modular design with clear separation of concerns:

**Physics Modules** (``suews_phys_*``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``suews_phys_evap``: Evaporation and transpiration calculations
- ``suews_phys_snow``: Snow physics and snow pack dynamics
- ``suews_phys_ohm``: Objective Hysteresis Model for storage heat flux
- ``suews_phys_resistance``: Surface resistance calculations
- ``suews_phys_waterdist``: Within-grid water distribution

**Control Modules** (``suews_ctrl_*``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``suews_ctrl_input``: Input data processing and validation
- ``suews_ctrl_output``: Output formatting and file I/O
- ``suews_ctrl_time``: Time stepping and temporal calculations

**Utility Modules** (``suews_util_*``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``suews_util_datetime``: Date and time utilities
- ``suews_util_meteo``: Meteorological calculations
- ``suews_util_roughness``: Roughness length calculations

Detailed API Documentation
---------------------------

`Fortran API Documentation <_static/html/index.html>`_ provides comprehensive, automatically generated documentation of all SUEWS source code using `Doxygen <http://www.doxygen.nl>`_.

**Key Features:**
- **Function signatures**: Complete parameter lists and return types
- **Module dependencies**: Cross-references between modules
- **Source code**: Direct links to implementation
- **Call graphs**: Visual representation of function relationships

Development Guidelines
-----------------------

**Code Standards:**
- Follow Fortran 2008+ standards
- Use explicit interfaces for all procedures
- Implement proper error handling
- Document all public interfaces

**Testing:**
- All new physics modules must include unit tests
- Integration tests for coupled model components
- Performance benchmarking for computational kernels

**Build System:**
- Uses Meson build system with f90wrap for Python bindings
- Cross-platform support (Linux, macOS, Windows)
- Automatic dependency resolution

Integration with Python Interface
----------------------------------

The Fortran core is accessed through f90wrap-generated Python bindings:

.. code-block:: python

   # Low-level access (for developers)
   from supy._run import suews_cal_tstep
   
   # High-level interface (for users)
   import supy as sp
   df_output, df_state = sp.run_supy(df_forcing, df_state)

**Note**: Most development should use the high-level Python interface. Direct Fortran API access is only needed for:

- Adding new physics parameterisations
- Performance-critical modifications
- External model coupling at the timestep level

Contributing to Core Physics
-----------------------------

See the :doc:`contributing/dev_guide` for detailed guidelines on:

- Setting up the development environment
- Code review process
- Physics validation requirements
- Documentation standards

.. note::

   **Significant Overhaul Needed**: The current Fortran API documentation requires substantial improvement. Planned enhancements include:
   
   - Better module organisation and documentation
   - Improved cross-references and examples
   - Performance profiling integration
   - Modern Fortran best practices implementation