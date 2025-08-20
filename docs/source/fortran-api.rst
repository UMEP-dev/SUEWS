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

Main Driver Subroutine
~~~~~~~~~~~~~~~~~~~~~~~

**SUEWS_cal_Main** - The central computation engine that orchestrates all physics calculations:

1. **Initialization Phase**:
   - State variable allocation and setup
   - Roughness parameter calculation
   - Solar position and radiation geometry

2. **Physics Calculation Loop**:
   - Surface energy balance iteration
   - Evapotranspiration calculations
   - Snow physics (if enabled)
   - Storage heat flux (OHM)
   - Anthropogenic emissions

3. **State Update Phase**:
   - Water distribution between surfaces
   - Soil moisture updates
   - Daily state variables
   - Output line assembly

**Key Driver Components**:

- **Iteration Control**: Convergence checking for surface temperature and energy balance
- **Multi-Surface Calculations**: Parallel processing of different urban surface types
- **Model Integration**: Coordination between physics modules (LUMPS, ESTM, SPARTACUS)
- **Debug Information**: Comprehensive state tracking for model validation

**Physics Modules** (``suews_phys_*``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``suews_phys_evap``: Evaporation and transpiration calculations using Penman-Monteith
- ``suews_phys_snow``: Snow physics including accumulation, melt, and albedo dynamics
- ``suews_phys_ohm``: Objective Hysteresis Model for storage heat flux estimation
- ``suews_phys_resist``: Aerodynamic and surface resistance calculations
- ``suews_phys_waterdist``: Within-grid water distribution and soil moisture
- ``suews_phys_narp``: Net All-wave Radiation Parameterisation
- ``suews_phys_spartacus``: 3D radiation transfer in urban canopies
- ``suews_phys_estm``: Element Surface Temperature Method for detailed energy balance
- ``suews_phys_beers``: Building Envelope Energy Radiation Scheme
- ``suews_phys_lumps``: Local Urban Meteorological Parameterisation Scheme

**Control Modules** (``suews_ctrl_*``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``suews_ctrl_driver``: Main computation wrapper and physics orchestration
- ``suews_ctrl_input``: Input data processing, validation, and forcing interpolation
- ``suews_ctrl_output``: Output formatting, variable selection, and file I/O
- ``suews_ctrl_error``: Error handling and diagnostic message management
- ``suews_ctrl_type``: Data type definitions and derived type structures

**Utility Modules** (``suews_util_*``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``suews_util_datetime``: Date and time utilities and calendar calculations
- ``suews_util_meteo``: Meteorological calculations and atmospheric properties
- ``suews_util_roughness``: Surface roughness and displacement height calculations

Computational Workflow
----------------------

**Time Step Calculation Sequence**:

1. **Preprocessing** (``SUEWS_cal_Main`` initialization):
   - Validate input forcing data
   - Update solar position and zenith angle
   - Calculate surface roughness parameters
   - Initialize iteration variables

2. **Energy Balance Iteration Loop**:
   - Estimate surface temperatures for all surface types
   - Calculate radiation balance (shortwave and longwave)
   - Compute turbulent heat fluxes (sensible and latent)
   - Apply storage heat flux parameterisation (OHM/ESTM)
   - Check convergence criteria (typically ±0.1°C for surface temperature)

3. **Water Balance Calculations**:
   - Process precipitation and irrigation inputs
   - Calculate evapotranspiration for each surface type
   - Update surface water stores and soil moisture
   - Redistribute water between grid surfaces
   - Calculate runoff and drainage

4. **Specialized Physics** (conditional):
   - Snow accumulation, aging, and melt processes
   - Anthropogenic heat and CO₂ emissions
   - Vegetation phenology and LAI dynamics
   - Building energy calculations (ESTM)

5. **Output Assembly**:
   - Aggregate fluxes and states across surface types
   - Apply output variable selection (WriteOutOption)
   - Update daily state variables
   - Prepare debug information if requested

**Key Data Structures**:

- **SUEWS_FORCING**: Meteorological input data (temperature, radiation, wind, etc.)
- **SUEWS_STATE**: Model state variables (surface temperatures, soil moisture, snow, etc.)
- **SUEWS_CONFIG**: Model configuration and physics options
- **SUEWS_SITE**: Site-specific parameters (surface properties, morphology, etc.)
- **output_line**: Time series results for current time step

Detailed API Documentation
---------------------------

.. note::

   **Doxygen Documentation Disabled**: Automatic Fortran API documentation generation has been disabled to reduce build output verbosity. For detailed function signatures and module dependencies, please refer to the source code directly in ``src/suews/src/``.

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
