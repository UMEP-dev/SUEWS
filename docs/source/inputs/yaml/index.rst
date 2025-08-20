.. _yaml_input:

YAML Configuration Format
=========================

SUEWS uses YAML configuration files to define all model parameters in a single, human-readable format. This guide shows you how to create and validate your configuration.

Quick Start
-----------

**1. Start with the sample configuration:**

.. code-block:: bash

   # Copy the sample configuration
   cp sample_config.yml my_config.yml
   
   # Edit for your site
   nano my_config.yml

**2. Validate and fix your configuration:**

.. code-block:: bash

   # Automatically fix common issues
   suews-validate my_config.yml
   
   # Creates: updatedABC_my_config.yml (ready to use)

**3. Run your simulation:**

.. code-block:: python

   import supy as sp
   
   # Load your validated configuration
   config = sp.SUEWSConfig.from_yaml('updatedABC_my_config.yml')
   
   # Run simulation
   output = config.run()

Configuration Structure
-----------------------

A SUEWS YAML file has two main sections:

.. code-block:: yaml

   name: "My Simulation"
   description: "Urban climate simulation for my city"
   
   model:          # Global simulation settings
     control:      # Time and file settings
     physics:      # Physics options
     output:       # Output control
   
   sites:          # List of sites to simulate
     - name: "Site1"
       properties: # Site characteristics
       land_cover: # Surface fractions
       initial:    # Initial conditions

Essential Parameters
--------------------

Minimum Configuration Structure
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A valid SUEWS configuration requires many parameters beyond this minimal example. The validator will add missing required parameters with appropriate defaults:

.. code-block:: yaml

   model:
     control:
       tstep: 3600                    # Time step [s]
       forcing_file: "forcing.txt"    # Meteorological data
       start_time: "2020-01-01"       # Start date
       end_time: "2020-12-31"         # End date
   
   sites:
     - name: "MySite"
       properties:
         lat: 51.5                    # Latitude
         lng: -0.1                    # Longitude
         alt: 10.0                    # Altitude [m]
         timezone: 0                  # UTC offset
         surfacearea: 1000000.0       # Area [m²]
       land_cover:
         fractions:
           paved: 0.4                 # Must sum to 1.0
           bldgs: 0.3
           grass: 0.2
           dectr: 0.1

.. important::
   
   This is a **minimal example** showing the basic structure. A complete configuration requires many additional parameters for:
   
   - Physics options and methods
   - Initial conditions
   - Surface properties (albedo, emissivity, roughness)
   - Vegetation parameters (LAI, conductance)
   - Soil properties
   - Anthropogenic heat flux
   - Water use and irrigation
   - Building morphology
   
   **To explore all parameters:**
   
   1. Run ``suews-validate`` on your configuration to generate a complete file with all defaults
   2. Review the generated ``updatedABC_*.yml`` file to see all parameters
   3. Consult the :doc:`schema/index` for comprehensive parameter documentation

Parameter Documentation
-----------------------

**Complete Parameter Reference:**

The full documentation for all YAML parameters is available in the :doc:`schema/index`. This reference includes:

- Detailed descriptions for every parameter
- Units and valid ranges
- Default values
- Cross-references between related parameters

**Key Parameter Groups:**

- :doc:`schema/model` - Top-level model configuration structure
- :doc:`schema/site` - Site-specific configuration structure
- :doc:`schema/modelcontrol` - Simulation control (time steps, files, etc.)
- :doc:`schema/modelphysics` - Physics methods and options
- :doc:`schema/siteproperties` - Geographic and geometric properties
- :doc:`schema/landcover` - Surface fractions and parameters

**Surface-Specific Parameters:**

- :doc:`schema/pavedproperties` - Roads and paved surfaces
- :doc:`schema/bldgsproperties` - Buildings
- :doc:`schema/evetrproperties` - Evergreen vegetation
- :doc:`schema/dectrproperties` - Deciduous vegetation
- :doc:`schema/grassproperties` - Grass surfaces
- :doc:`schema/bsoilproperties` - Bare soil
- :doc:`schema/waterproperties` - Water bodies

Forcing Data
------------

Meteorological forcing data drives the SUEWS simulation. You specify the forcing file(s) in your configuration:

.. code-block:: yaml

   model:
     control:
       forcing_file: "forcing/met_data_2020.txt"
       # Or use multiple files:
       forcing_file:
         - "forcing/met_data_2020_Q1.txt"
         - "forcing/met_data_2020_Q2.txt"
         - "forcing/met_data_2020_Q3.txt"
         - "forcing/met_data_2020_Q4.txt"

**Forcing File Format**

The forcing file must be a text file with specific columns in the correct order. See :doc:`/inputs/forcing-data` for the complete format specification.

**Essential columns** (tab or space separated):

1. **Time columns**: ``iy`` (year), ``id`` (day of year), ``it`` (hour), ``imin`` (minute)
2. **Wind speed** [m/s] - minimum 0.01 m/s
3. **Relative humidity** [%]
4. **Air temperature** [°C]
5. **Pressure** [kPa]
6. **Rainfall** [mm]
7. **Incoming shortwave radiation** [W/m²] - must be > 0
8. **Incoming longwave radiation** [W/m²] - optional, will be modeled if missing (use -999)

**Important requirements**:

- Data must be continuous (no gaps)
- Time stamps indicate the **end** of each period
- Use local time (not UTC)
- Use -999 for missing optional variables

For detailed format specifications, column order, and optional variables, see :doc:`/inputs/forcing-data`.

Validation and Troubleshooting
-------------------------------

Using the Validation Tool
~~~~~~~~~~~~~~~~~~~~~~~~~

The ``suews-validate`` command checks your configuration and fixes common issues:

.. code-block:: bash

   # Basic validation with automatic fixes
   suews-validate config.yml
   
   # Check without making changes
   suews-validate validate config.yml
   
   # Get JSON output for scripts
   suews-validate validate config.yml --format json

What Gets Fixed Automatically
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The validator automatically corrects these issues:

- **Missing parameters** - Adds required fields with sensible defaults
- **Surface fractions** - Normalizes to sum to exactly 1.0
- **Initial temperatures** - Sets based on location and season using climate data
- **Physics options** - Ensures compatible model settings
- **Parameter names** - Corrects common typos and outdated names

For complete validation documentation, see :doc:`validation`.

Common Issues and Solutions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

**"Missing required parameter"**
   The validator will add it with a default value. Check the report to see what was added.

**"Surface fractions don't sum to 1.0"**
   Automatically normalized. Original values are proportionally adjusted.

**"Invalid physics option combination"**
   The validator suggests compatible options. Manual selection may be needed.

**"Unknown parameter name"**
   Check for typos. The validator will suggest the correct name.

Examples
--------

Urban Site Configuration
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: yaml

   name: "London Urban"
   description: "Central London urban climate"
   
   model:
     control:
       tstep: 3600
       forcing_file: "london_met_2020.txt"
       start_time: "2020-01-01"
       end_time: "2020-12-31"
     physics:
       netradiationmethod: 3
       emissionsmethod: 2
       storageheatmethod: 1
   
   sites:
     - name: "CentralLondon"
       properties:
         lat: 51.5074
         lng: -0.1278
         alt: 10.0
         timezone: 0
         surfacearea: 1000000.0
         popdens: 5500.0
       land_cover:
         fractions:
           paved: 0.35
           bldgs: 0.40
           grass: 0.15
           dectr: 0.10
       land_cover_params:
         bldgs:
           bldgh: 20.0
           faibldg: 3.5
         dectr:
           lai_id: 4.5

Tips for Success
----------------

1. **Start with the sample**: Always begin with ``sample_config.yml`` and modify it
2. **Validate early**: Run validation before long simulations
3. **Check the report**: Understand what the validator changed
4. **Use meaningful names**: Help yourself remember what each simulation is for
5. **Keep originals**: The validator creates new files, preserving your originals

Getting Help
------------

- **Validation issues**: Check the report file (``reportABC_*.txt``)
- **Parameter documentation**: See the error messages from validation
- **Examples**: Look in ``sample_data/`` directory
- **Community support**: `UMEP Community Forum <https://github.com/UMEP-dev/UMEP/discussions>`_

.. toctree::
   :maxdepth: 1
   :hidden:

   validation
   schema/index