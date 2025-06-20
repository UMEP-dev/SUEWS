.. _wrf_suews_integration:

WRF-SUEWS Coupling
==================

The `WRF-SUEWS coupling project <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS>`_ integrates the Weather Research and Forecasting (WRF) atmospheric model with SUEWS to provide detailed urban climate simulations at the mesoscale.

.. warning::

   **Version Compatibility**: WRF-SUEWS is currently based on an older version of SUEWS. Integration with modern SuPy workflows requires additional development work.

Overview
--------

**Purpose:**
WRF-SUEWS combines the strengths of both models:

- **WRF**: Mesoscale atmospheric dynamics, 3D meteorological fields
- **SUEWS**: Detailed urban surface energy and water balance processes

**Key Benefits:**
- **Two-way coupling**: SUEWS provides surface fluxes to WRF, WRF provides meteorological forcing to SUEWS
- **Urban-specific physics**: Detailed representation of urban surface processes within mesoscale simulations
- **High-resolution urban climate**: Better representation of urban heat islands and urban meteorology
- **Research applications**: Urban climate change studies, heat wave analysis, urban planning support

Technical Architecture
----------------------

**Coupling Strategy:**
The integration replaces WRF's default urban canopy model with SUEWS:

1. **Initialisation**: SUEWS surface parameters integrated into WRF land use data
2. **Runtime coupling**: At each timestep, WRF calls SUEWS for surface flux calculations
3. **Data exchange**: Meteorological variables and surface fluxes exchanged between models
4. **Output**: Combined WRF atmospheric fields with SUEWS surface diagnostics

**Supported Features:**
- **Seven surface types**: Buildings, paved surfaces, vegetation, water bodies
- **Energy balance**: Complete surface energy balance including storage heat flux
- **Water balance**: Urban hydrology including runoff and evapotranspiration
- **Anthropogenic effects**: Human activities and building energy consumption

Installation and Setup
-----------------------

.. note::

   **High-Performance Computing**: WRF-SUEWS is typically deployed on HPC systems due to computational requirements. The installation process requires significant technical expertise.

**System Requirements:**

- **Compilers**: Intel Fortran compiler (recommended), GCC support available
- **Libraries**: NetCDF, HDF5, MPI libraries
- **Platform**: Linux/Unix systems, tested on JASMIN HPC and Apple M1

**Installation Steps:**

1. **Clone Repository:**

   .. code-block:: bash

      git clone --recurse-submodules https://github.com/Urban-Meteorology-Reading/WRF-SUEWS.git
      cd WRF-SUEWS

2. **Environment Setup:**

   .. code-block:: bash

      # Create conda environment
      conda env create --file=wrf_suews.yml
      conda activate wrf_suews

3. **Compilation:**

   Follow platform-specific compilation instructions in the repository documentation.

Usage Workflow
--------------

**1. Preprocessing (WPS):**

.. code-block:: bash

   # Process meteorological data
   ./geogrid.exe    # Define domain and terrain
   ./ungrib.exe     # Extract meteorological data
   ./metgrid.exe    # Interpolate met data to domain

**2. SUEWS Configuration:**

Prepare urban surface parameters:

- **Land use classification**: Map urban areas to SUEWS surface types
- **Surface parameters**: Building heights, vegetation fractions, surface properties
- **Anthropogenic forcing**: Population density, energy consumption patterns

**3. WRF-SUEWS Execution:**

.. code-block:: bash

   # Real data preprocessing
   ./real.exe
   
   # WRF-SUEWS simulation
   mpirun -np <cores> ./wrf.exe

**4. Output Analysis:**

WRF-SUEWS produces standard WRF output files with additional SUEWS diagnostics:

- **Atmospheric variables**: Temperature, humidity, wind fields
- **Surface fluxes**: Sensible heat, latent heat, momentum flux
- **Urban diagnostics**: Storage heat flux, runoff, building energy use

Integration with Modern SUEWS
------------------------------

**Current Limitations:**

- **Legacy SUEWS version**: Based on older SUEWS physics and interface
- **No SuPy integration**: Cannot leverage modern Python workflows
- **Manual configuration**: Requires extensive manual parameter setup

**Future Development Opportunities:**

1. **SuPy Integration:**

   .. code-block:: python

      # Conceptual future workflow
      import supy as sp
      import wrfsuews
      
      # Configure SUEWS sites from WRF grid
      sites = wrfsuews.generate_suews_sites(wrf_domain, landuse_data)
      
      # Run coupled simulation
      wrf_output = wrfsuews.run_coupled(
          wrf_config="namelist.input",
          suews_sites=sites,
          start_date="2020-06-01",
          end_date="2020-08-31"
      )

2. **Automated Parameter Generation:**
   - Use modern GIS tools to derive SUEWS parameters from spatial data
   - Integration with UMEP spatial analysis capabilities
   - Automated urban morphology characterisation

3. **Enhanced Output Processing:**
   - Native pandas/xarray integration for analysis
   - Automated visualisation tools
   - Direct integration with climate impact assessment workflows

Research Applications
---------------------

**Urban Heat Island Studies:**

.. code-block:: python

   # Example analysis (conceptual)
   # Extract urban temperature from WRF-SUEWS output
   urban_temp = wrf_output.sel(landuse='urban')['T2']
   rural_temp = wrf_output.sel(landuse='rural')['T2']
   
   # Calculate UHI intensity
   uhi_intensity = urban_temp - rural_temp

**Climate Change Assessment:**

- **Scenario analysis**: Compare current vs future climate scenarios
- **Heat wave analysis**: Detailed urban temperature during extreme events
- **Adaptation strategies**: Evaluate green infrastructure impacts

**Urban Planning Support:**

- **Development scenarios**: Test different urban development patterns
- **Green infrastructure**: Quantify cooling effects of urban vegetation
- **Building energy**: Assess urban-scale energy consumption patterns

Getting Started
---------------

**For Researchers New to WRF-SUEWS:**

1. **Background Knowledge**: Familiarity with WRF and urban climate modeling essential
2. **Start Simple**: Begin with existing test cases before custom domains
3. **Computational Resources**: Ensure adequate HPC access for meaningful simulations
4. **Community Support**: Engage with WRF and SUEWS user communities

**Resources:**

- `WRF-SUEWS GitHub Repository <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS>`_
- `WRF User Guide <https://www2.mmm.ucar.edu/wrf/users/>`_
- :doc:`SUEWS documentation <../index>`
- `JASMIN Computing Platform <https://jasmin.ac.uk/>`_ (for UK researchers)

**Contributing:**

The WRF-SUEWS project welcomes contributions:

- **Bug reports**: Issue tracking on GitHub
- **Platform support**: Help with compilation on new systems  
- **Documentation**: Improve installation and usage guides
- **Integration**: Work on modern SuPy integration

.. note::

   **Development Status**: WRF-SUEWS represents a sophisticated but complex integration. Future development should focus on simplifying the workflow and integrating with modern SUEWS/SuPy capabilities for broader adoption in the urban climate modeling community.