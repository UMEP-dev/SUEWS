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

- **name**: A descriptive name for your simulation
- **description**: Detailed description of the simulation purpose
- **model**: Global settings that apply to all sites
- **sites**: List of sites to simulate with their specific parameters

Model Section
~~~~~~~~~~~~~

The ``model`` section contains global simulation settings that apply to all sites in your simulation. It has two main subsections:

- **Control**: Time step, simulation period, input/output configuration
- **Physics**: Physics scheme selections, calculation methods, model options

**Model Control** (``model.control``)
+++++++++++++++++++++++++++++++++++++

Controls the simulation execution and input/output handling:

- **Time control**: ``tstep`` (timestep in seconds), ``start_time``, ``end_time``
- **Forcing configuration**: ``forcing_file`` (meteorological data path, can be single file or list) - detailed description in :ref:`met_input`
- **Output configuration**: ``output_file`` (format, frequency, variable groups) - detailed description in :ref:`output_files`

For complete parameter descriptions, see :doc:`schema/modelcontrol`.

**Model Physics** (``model.physics``)
+++++++++++++++++++++++++++++++++++++

Selects which physics schemes and calculation methods to use:

- **Radiation**: ``netradiationmethod`` (0=observed, 3=modelled LW, 4=SPARTACUS)
- **Energy balance**: ``storageheatmethod`` (OHM variants), ``emissionsmethod`` (QF calculation)
- **Surface-atmosphere exchange**: ``stabilitymethod``, ``roughlenmommethod``, ``roughlenheatmethod``
- **Water balance**: ``waterusemethod``, ``snowuse``, ``smdmethod`` (soil moisture)
- **Vegetation**: ``gsmodel`` (stomatal conductance), ``laimethod`` (LAI calculation)
- **Urban schemes**: ``stebbsmethod`` (building energy), ``rslmethod`` (roughness sublayer)

For complete parameter descriptions and method options, see :doc:`schema/modelphysics`.

Sites Section
~~~~~~~~~~~~~

The ``sites`` section is a list of one or more `site <schema/site>` to simulate. Each site represents a specific location with its own characteristics:

.. list-table::
   :header-rows: 1
   :widths: 25 75

   * - Field
     - Purpose
   * - ``name``
     - Unique site identifier (used in output file names)
   * - ``gridiv``
     - Grid ID number for multi-site simulations
   * - ``properties``
     - Static site characteristics and surface parameters
   * - ``initial_states``
     - Initial conditions for prognostic variables

For complete site structure, see :doc:`schema/site`.

Properties Subsection
~~~~~~~~~~~~~~~~~~~~~
The ``properties`` section contains all static--not varying during the simulation--site characteristics, which include :doc:`general site information <schema/siteproperties>` and :doc:`land cover parameters <schema/landcover>`.

General Site Information
+++++++++++++++++++++++++

.. list-table::
   :header-rows: 1
   :widths: 30 70

   * - Category
     - Key Parameters & Purpose
   * - **Location**
     - | ``lat``, ``lng`` (coordinates in decimal degrees)
       | ``alt`` (altitude in metres), ``timezone`` (UTC offset)
   * - **Morphometry**
     - | ``surfacearea`` (grid cell area in m²)
       | ``z`` (measurement height), ``z0m_in``, ``zdm_in`` (roughness parameters)
   * - **Population**
     - | ``pop_dens_daytime``, ``pop_dens_nighttime`` (people per hectare)
       | Used for anthropogenic heat and CO₂ calculations
   * - **Land Cover**
     - | Nested section with parameters for each surface type
       | See detailed structure below
   * - **Special Models**
     - | ``spartacus`` (3D radiation parameters)
       | ``stebbs`` (building energy model parameters)
       | ``conductance`` (vegetation conductance parameters)
       | ``irrigation`` (water use and irrigation settings)
       | ``anthropogenic_emissions`` (heat and CO₂ emission parameters)

For complete properties documentation, see :doc:`schema/siteproperties`.

Land Cover Parameters
~~~~~~~~~~~~~~~~~~~~~

The ``land_cover`` section under ``properties`` defines parameters for seven surface types. The surface fractions (``sfr``) must sum to 1.0:

.. list-table::
   :header-rows: 1
   :widths: 20 30 50

   * - Surface Type
     - Common Examples
     - Key Parameters
   * - ``paved``
     - Roads, car parks, pavements
     - | ``sfr`` (fraction), ``alb`` (albedo)
       | Storage/drainage parameters
   * - ``bldgs``
     - Buildings, structures
     - | ``bldgh`` (height), ``faibldg`` (frontal area)
       | Wall/roof properties, thermal mass
   * - ``evetr``
     - Coniferous trees, evergreen shrubs
     - | ``evetreeh`` (height), LAI parameters
       | Stays green year-round
   * - ``dectr``
     - Broadleaf trees, deciduous shrubs
     - | ``dectreeh`` (height), phenology parameters
       | Seasonal LAI variation
   * - ``grass``
     - Lawns, parks, playing fields
     - | LAI range, irrigation settings
       | Conductance parameters
   * - ``bsoil``
     - Exposed soil, construction sites
     - | Soil hydraulic properties
       | Usually minimal fraction
   * - ``water``
     - Rivers, ponds, fountains
     - | Flow parameters
       | Special storage treatment

Each surface type contains these parameter groups:

- **Radiative**: ``alb`` (albedo), ``emis`` (emissivity)
- **Hydrological**: ``soilstorecap``, ``statelimit``, ``storedrainprm`` (drainage parameters)
- **Thermal**: ``thermal_layers`` (depth, conductivity, heat capacity for each layer)
- **Vegetation** (grass/trees): ``lai`` (LAI parameters), ``maxconductance``, phenology settings
- **Urban** (buildings/paved): morphology parameters, anthropogenic properties

For complete land cover parameters, see :doc:`schema/landcover`.

Initial States Structure
~~~~~~~~~~~~~~~~~~~~~~~~

The ``initial_states`` section sets starting values for prognostic (time-evolving) variables. Each surface type needs initial conditions:

.. list-table::
   :header-rows: 1
   :widths: 30 70

   * - State Variable
     - Description & Units
   * - ``soilstore``
     - Soil moisture storage [mm]
   * - ``state``
     - Surface wetness [mm]
   * - ``temperature``
     - Temperature profile through layers [°C]
   * - ``snowpack``
     - Snow water equivalent [mm]
   * - ``lai_id``
     - Initial LAI for vegetation [m²/m²]
   * - ``gdd_id``
     - Growing degree days for phenology

Special initial states:

- **Vegetation surfaces** (grass, dectr, evetr): Need ``lai_id``, ``gdd_id``, ``sdd_id``
- **Water surfaces**: Use ``state`` for water level
- **All surfaces**: Can specify ``snowpack``, ``snowfrac`` for winter starts

For complete initial states documentation, see :doc:`schema/initialstates`.


Schema Reference
----------------

The following reference pages provide complete documentation for all parameters:

**Top-Level Configuration:**

- :doc:`schema/model` - Model section structure and global settings
- :doc:`schema/site` - Site section structure and site-specific settings

**Model Configuration:**

- :doc:`schema/modelcontrol` - Time control, input/output, diagnostics
- :doc:`schema/modelphysics` - Physics methods and scheme selections

**Site Configuration:**

- :doc:`schema/siteproperties` - Location, morphometry, and site characteristics
- :doc:`schema/landcover` - Surface type parameters (paved, buildings, vegetation, etc.)
- :doc:`schema/initialstates` - Initial conditions for prognostic variables


Schema Versioning
-----------------

SUEWS YAML configurations use schema versioning to track configuration structure changes. 
See :doc:`schema_versioning` for details on:

- Understanding schema versions vs model versions  
- Automatic compatibility checking
- Migration tools for updating configurations

For validation tools and IDE integration, see :doc:`schema_publishing`.


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
