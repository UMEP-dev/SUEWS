.. _configuration_guide:

Configuration Guide
===================

This comprehensive guide helps you configure SUEWS using the YAML format, from basic setup to advanced customization.

.. contents:: Contents
   :local:
   :depth: 2

Quick Start
-----------

**Minimal Configuration**

Every SUEWS simulation requires these essential elements:

.. code-block:: yaml

   # Minimal SUEWS configuration
   model:
     control:
       tstep: 3600                    # Hourly timestep
       forcing_file: "forcing.txt"    # Meteorological data
       start_date: "2020-01-01"       # Start date
       end_date: "2020-12-31"         # End date
   
   sites:
     - name: "My_Site"
       latitude: 51.5                 # Site location
       longitude: -0.1
       land_cover:                    # Must sum to 1.0
         paved: 0.30
         bldgs: 0.35
         grass: 0.20
         evetr: 0.10
         dectr: 0.05
         bsoil: 0.00
         water: 0.00

Configuration by Use Case
-------------------------

Dense Urban Areas
~~~~~~~~~~~~~~~~~

For city centers with high building density:

.. code-block:: yaml

   sites:
     - name: "City_Center"
       land_cover:
         paved: 0.45
         bldgs: 0.40
         grass: 0.10
         evetr: 0.03
         dectr: 0.02
       properties:
         bldgs:
           height: 25.0           # Tall buildings
           fai: 0.5              # High frontal area
         pop_dens_daytime: 500    # High population
         z0m_summer: 2.0         # High roughness

**Key physics options:**

- ``emissions_method: 4`` - Include all anthropogenic heat sources
- ``storage_heat_method: 2`` - OHM with anthropogenic heat
- ``stability_method: 3`` - Full stability corrections

Suburban Residential
~~~~~~~~~~~~~~~~~~~~

For lower-density residential areas:

.. code-block:: yaml

   sites:
     - name: "Suburbs"
       land_cover:
         paved: 0.25
         bldgs: 0.25
         grass: 0.30
         evetr: 0.10
         dectr: 0.10
       properties:
         bldgs:
           height: 8.0            # Lower buildings
           fai: 0.25             # Less dense
         pop_dens_daytime: 50     # Lower population

**Key physics options:**

- ``emissions_method: 2`` - Temperature-dependent QF
- ``water_use_method: 1`` - Include irrigation

Parks and Green Spaces
~~~~~~~~~~~~~~~~~~~~~~~

For vegetated areas:

.. code-block:: yaml

   sites:
     - name: "Urban_Park"
       land_cover:
         paved: 0.10
         bldgs: 0.05
         grass: 0.50
         evetr: 0.15
         dectr: 0.15
         bsoil: 0.05
       properties:
         grass:
           lai_max: 3.0          # Healthy grass
         irrigation:
           auto_irr_grass: 1     # Automatic irrigation

Parameter Categories
--------------------

Model Control Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

**Time Configuration** (``model.control``)

==============================  ===========  ====================================
Parameter                       Type         Description
==============================  ===========  ====================================
``tstep``                       integer      Model timestep [s]
``start_date``                  string       Simulation start (YYYY-MM-DD)
``end_date``                    string       Simulation end (YYYY-MM-DD)
``forcing_file``                string/list  Meteorological data file(s)
``output_file``                 string/dict  Output configuration
==============================  ===========  ====================================

**Output Options** (``model.control.output_file``)

.. code-block:: yaml

   # Simple text output (backward compatible)
   output_file: "output.txt"
   
   # Advanced configuration
   output_file:
     format: parquet              # or 'txt'
     freq: 1800                   # Output every 30 min
     groups: ["SUEWS", "RSL"]     # Output groups

Physics Methods
~~~~~~~~~~~~~~~

**Key Method Selections** (``model.physics``)

================================  =========  ====================================
Parameter                         Options    Description
================================  =========  ====================================
``net_radiation_method``          0,1,3      | 0: Observed Q*
                                             | 1: Observed LW↓
                                             | 3: Model LW↓ (recommended)
``emissions_method``              0,1,2,4    | 0: No QF
                                             | 1: Simple daily profile
                                             | 2: Temperature-dependent
                                             | 4: Full model (buildings+traffic)
``storage_heat_method``           1,2,3      | 1: OHM without QF
                                             | 2: OHM with QF
                                             | 3: AnOHM
``stability_method``              0,2,3      | 0: Neutral
                                             | 2: Least stable
                                             | 3: Campbell & Norman (recommended)
================================  =========  ====================================

Site Properties
~~~~~~~~~~~~~~~

**Essential Site Parameters** (``site.properties``)

- **Location**: ``lat``, ``lng``, ``alt``, ``timezone``
- **Morphology**: ``surfacearea``, ``z`` (measurement height)
- **Population**: ``pop_dens_daytime``, ``pop_dens_nighttime``
- **Roughness**: ``z0m_summer``, ``z0m_winter``

Surface Properties
~~~~~~~~~~~~~~~~~~

Each surface type has specific properties. Common ones include:

**Physical Properties**

- ``albedo_min``, ``albedo_max`` - Surface reflectance
- ``emissivity`` - Longwave emissivity
- ``storage_capacity`` - Heat storage capacity [J m⁻² K⁻¹]

**Vegetation Properties** (grass, evetr, dectr)

- ``lai_max``, ``lai_min`` - Leaf area index range
- ``gdd_base`` - Growing degree day base [°C]
- ``conductance.g_max`` - Maximum stomatal conductance [mm s⁻¹]

**Building Properties** (bldgs)

- ``height`` - Mean building height [m]
- ``fai`` - Frontal area index [-]
- ``wall_area_fraction`` - Wall to plan area ratio [-]

Advanced Features
-----------------

Multiple Forcing Files
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: yaml

   model:
     control:
       forcing_file:
         - "forcing_2020.txt"
         - "forcing_2021.txt"
         - "forcing_2022.txt"

Files are automatically concatenated in chronological order.

SPARTACUS Radiation
~~~~~~~~~~~~~~~~~~~

For detailed 3D radiation calculations:

.. code-block:: yaml

   model:
     physics:
       net_radiation_method: 4    # Enable SPARTACUS
   
   site:
     properties:
       spartacus:
         building_frac: 0.4
         building_scale: 20.0      # Building width [m]
         veg_frac: 0.3
         veg_scale: 5.0           # Tree crown width [m]

Building Energy (STEBBS)
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: yaml

   model:
     physics:
       emissions_method: 5         # Enable STEBBS
   
   site:
     properties:
       stebbs:
         cooling_setpoint: 24.0    # °C
         heating_setpoint: 20.0    # °C
         cop_cooling: 3.0          # Cooling efficiency

Troubleshooting
---------------

Common Issues and Solutions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Land cover doesn't sum to 1.0**
   Check your fractions add up exactly to 1.0

**Missing required parameters**
   Enable annotated YAML generation to identify missing fields:
   
   .. code-block:: python
   
      config = SUEWSConfig.from_yaml('config.yml', auto_generate_annotated=True)

**Energy balance not closing**
   - Verify albedo values are realistic
   - Check OHM coefficients
   - Ensure radiation method matches data availability

**No evapotranspiration**
   - Check LAI values > 0 for vegetation
   - Verify soil moisture is adequate
   - Ensure surface conductance parameters are set

File Organization
-----------------

Recommended project structure:

.. code-block:: text

   project/
   ├── config_suews.yml       # Main configuration
   ├── forcing/
   │   ├── met_2020.txt      # Meteorological data
   │   └── met_2021.txt
   ├── output/               # Results directory
   └── initial_conditions/   # Optional restart files

See Also
--------

* :doc:`schema/model` - Complete model parameter reference
* :doc:`schema/site` - Complete site parameter reference
* `Sample configuration <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/sample_run/sample_config.yml>`_
* :ref:`met_input` - Forcing data format
* :ref:`output_files` - Output file descriptions