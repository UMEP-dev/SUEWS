.. _yaml_input:

.. meta::
   :description: SUEWS YAML configuration format documentation for site parameters, model control, and surface properties
   :keywords: SUEWS, YAML, configuration, parameters, latitude, longitude, lat, lng, site properties, model control, forcing file

YAML Configuration Format
=========================

SUEWS uses `YAML (Yet Another Markup Language) <https://yaml.org/spec/1.2.2/>`_ configuration files to define all model parameters in a single, human-readable format. YAML is a data serialisation standard that's easy to read and write, making it ideal for complex scientific model configurations. Unlike traditional tabular input formats, YAML allows you to organise parameters hierarchically and include documentation directly in your configuration files.



Overview
--------

A SUEWS YAML configuration file is organized into two main sections in addition to the `name` and `description` fields:

- **model**: Global settings that control the simulation (physics options, time stepping, file paths)
- **sites**: List of sites to simulate, each with properties, initial conditions, and land cover

Here's a minimal configuration example showing all required sections:

.. code-block:: yaml

   # Minimal SUEWS configuration with all required sections
   name: "My Simulation" # custom name for the simulation
   description: "Urban climate simulation for central London" # custom description for the simulation

   model: # global settings that control the simulation
     control:                         # Time and file settings
       tstep: 3600                    # Hourly timestep [s]
       forcing_file: "forcing.txt"    # Meteorological data
       start_time: "2020-01-01"       # Start date (YYYY-MM-DD)
       end_time: "2020-12-31"         # End date
     physics:                         # Physics options
       netradiationmethod: 3          # Model LW radiation
       emissionsmethod: 2             # Temperature-dependent QF
       storageheatmethod: 1           # OHM without QF
       stabilitymethod: 3             # Campbell & Norman

   sites: # list of sites to simulate, each with static properties and initial conditions
     - name: "My_Site"
       gridiv: 1                      # Grid ID
       properties:                    # Site characteristics
         lat: 51.5                    # Latitude
         lng: -0.1                    # Longitude
         alt: 10.0                    # Altitude [m]
         timezone: 0                  # UTC offset
         surfacearea: 1000000.0       # Area [m²]
         z: 10.0                      # Measurement height [m]
         z0m_in: 1.0                  # Roughness length [m]
         zdm_in: 10.0                 # Displacement height [m]
         ...                          # Additional site properties
         land_cover:                  # Surface fractions and properties
           paved:                     # Roads, pavements, parking lots
             sfr: 0.30                # Surface fraction (must sum to 1.0)
             alb: 0.10                # Albedo (0-1)
             emis: 0.95               # Emissivity (0-1)
             ...                      # OHM coefficients, drainage, thermal properties
           bldgs:                     # Buildings and structures
             sfr: 0.35                # Surface fraction
             alb: 0.12                # Albedo
             emis: 0.91               # Emissivity
             bldgh: 15.0              # Mean building height [m]
             faibldg: 0.15            # Frontal area index [-]
             ...                      # Wall/roof properties, thermal mass
           evetr:                     # Evergreen trees/shrubs
             sfr: 0.10                # Surface fraction
             alb: 0.10                # Albedo (darker than deciduous)
             evetreeh: 10.0           # Mean tree height [m]
             ...                      # LAI, conductance, phenology
           dectr:                     # Deciduous trees/shrubs
             sfr: 0.05                # Surface fraction
             alb: 0.18                # Albedo (seasonal variation)
             dectreeh: 12.0           # Mean tree height [m]
             ...                      # LAI, conductance, phenology
           grass:                     # Grass, lawns, low vegetation
             sfr: 0.20                # Surface fraction
             alb: 0.21                # Albedo
             lai:                     # Leaf Area Index parameters
               laimax: 5.9            # Maximum LAI [m²/m²]
               laimin: 1.6            # Minimum LAI [m²/m²]
               ...                    # Growth degree days, senescence
             ...                      # Irrigation, conductance
           bsoil:                     # Bare soil
             sfr: 0.00                # Surface fraction
             alb: 0.18                # Albedo
             ...                      # Soil hydraulic properties
           water:                     # Open water, fountains
             sfr: 0.00                # Surface fraction
             alb: 0.10                # Albedo (low)
             ...                      # Flow rate, water temperature
       initial_states:                # Initial conditions for each surface
         paved:
           soilstore: 120.0           # Soil moisture store [mm]
           state: 0.0                 # Surface wetness [mm]
           ...                        # Temperature profile, snow
         # ... initial states for each surface type

For a complete working example, see the `sample configuration <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/sample_data/sample_config.yml>`_.


Configuration Structure
-----------------------

The YAML configuration file has a hierarchical structure with four main top-level components:

Top-Level Components
~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 20 80

   * - Component
     - Description
   * - ``name``
     - **Optional** - A descriptive name for your simulation
   * - ``description``
     - **Optional** - Detailed description of the simulation purpose
   * - ``model``
     - **Required** - Global settings that apply to all sites
   * - ``sites``
     - **Required** - List of sites to simulate with their specific parameters

Model Section
~~~~~~~~~~~~~

The ``model`` section contains global simulation settings:

.. list-table::
   :header-rows: 1
   :widths: 25 75

   * - Subsection
     - Purpose
   * - ``model.control``
     - Time stepping, file paths, simulation period
   * - ``model.physics``
     - Physics scheme selections and methods

**Key control parameters:**

- ``tstep`` - Model timestep in seconds
- ``forcing_file`` - Path to meteorological input data
- ``output_file`` - Output configuration (format, frequency)
- ``start_time`` / ``end_time`` - Simulation period

**Key physics methods:**

- ``netradiationmethod`` - How to calculate net radiation
- ``emissionsmethod`` - Anthropogenic heat flux method
- ``storageheatmethod`` - Storage heat flux calculation
- ``stabilitymethod`` - Atmospheric stability scheme

Sites Section
~~~~~~~~~~~~~

The ``sites`` section is a list where each site contains:

.. list-table::
   :header-rows: 1
   :widths: 25 75

   * - Subsection
     - Contents
   * - ``name``
     - Site identifier
   * - ``gridiv``
     - Grid ID number
   * - ``properties``
     - Static site characteristics and surface parameters
   * - ``initial_states``
     - Initial conditions for state variables

Properties Subsection
~~~~~~~~~~~~~~~~~~~~~

Within each site's ``properties``:

.. list-table::
   :header-rows: 1
   :widths: 30 70

   * - Category
     - Key Parameters
   * - **Location**
     - ``lat``, ``lng``, ``alt``, ``timezone``
   * - **Morphology**
     - ``surfacearea``, ``z`` (measurement height), ``z0m_in``, ``zdm_in``
   * - **Population**
     - ``pop_dens_daytime``, ``pop_dens_nighttime``
   * - **Land Cover**
     - Nested section with surface-specific parameters
   * - **Special Models**
     - ``spartacus``, ``stebbs``, ``conductance``, ``irrigation``, etc.

Land Cover Structure
~~~~~~~~~~~~~~~~~~~~

The ``land_cover`` section under ``properties`` contains parameters for each surface type:

.. code-block:: yaml

   land_cover:
     paved:       # Roads, pavements
       sfr: 0.30  # Surface fraction
       alb: 0.10  # Albedo
       emis: 0.95 # Emissivity
       # ... many more surface-specific parameters
     bldgs:       # Buildings
       sfr: 0.35
       bldgh: 15.0  # Building height
       faibldg: 0.15  # Frontal area index
       # ...
     # ... other surfaces (grass, evetr, dectr, bsoil, water)

Each surface type has:

- **Radiative properties**: albedo, emissivity
- **Hydrological properties**: drainage, soil storage, infiltration
- **Thermal properties**: heat capacity, thermal conductivity layers
- **Vegetation properties** (if applicable): LAI parameters, conductance
- **Urban properties** (if applicable): building height, frontal area

Initial States Structure
~~~~~~~~~~~~~~~~~~~~~~~~

The ``initial_states`` section provides starting values for prognostic variables:

.. code-block:: yaml

   initial_states:
     paved:
       soilstore: 120.0    # Soil moisture [mm]
       state: 0.0          # Surface wetness [mm]
       temperature: [...]  # Temperature profile
       # ...
     # ... initial states for each surface type


Schema Reference
----------------

For complete parameter documentation:

- :doc:`schema/model` - All model-level configuration parameters
- :doc:`schema/site` - All site-specific parameters


Validation and Error Handling
------------------------------

.. note:: **Configuration Wizard Coming Soon**

   A dedicated configuration wizard tool will be shipped with future SuPy releases to help you:

   - Interactively create YAML configuration files
   - Select appropriate parameter values for your site
   - Validate configurations before running
   - Convert legacy input formats to YAML

   This tool will simplify the configuration process, especially for the complex parameter sections shown above.

SUEWS validates your configuration when loading. If errors occur:

- **Clear error messages** list all missing or invalid parameters
- **Annotated YAML** can be generated to help fix issues

To generate an annotated file with error markers:

.. code-block:: python

   # Automatic generation when errors found
   config = SUEWSConfig.from_yaml('config.yml', auto_generate_annotated=True)

The annotated file (``config.yml_annotated.yml``) includes:

- Missing parameters marked with ``[ERROR] MISSING:``
- Suggested fixes marked with ``[TIP] ADD HERE:``
- Parameter descriptions and expected types

Additional Resources
--------------------

- `Sample configuration <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/sample_run/sample_config.yml>`_ - Complete working example
- :ref:`met_input` - Forcing data format
- :ref:`output_files` - Output file descriptions