.. _yaml_input:

.. meta::
   :description: SUEWS YAML configuration format documentation for site parameters, model control, and surface properties
   :keywords: SUEWS, YAML, configuration, parameters, latitude, longitude, lat, lng, site properties, model control, forcing file

YAML Configuration Format
=========================

The YAML configuration format is the recommended method for providing inputs to SUEWS. It uses a single, structured ``config_suews.yml`` file to define all model parameters.

.. toctree::
   :maxdepth: 2
   :hidden:

   schema/model
   schema/site


Overview
--------

A SUEWS YAML configuration file is organized into two main sections:

- **model**: Global settings that control the simulation (physics options, time stepping, file paths)
- **sites**: List of sites to simulate, each with properties, initial conditions, and land cover

Here's a minimal configuration example showing all required sections:

.. code-block:: yaml

   # Minimal SUEWS configuration with all required sections
   name: "My Simulation"
   description: "Urban climate simulation for central London"

   model:
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

   sites:
     - name: "My_Site"
       gridiv: 1                      # Grid ID
       properties:                    # Site characteristics
         lat: 51.5                    # Latitude
         lng: -0.1                    # Longitude
         alt: 10.0                    # Altitude [m]
         timezone: 0                  # UTC offset
         surfacearea: 1000000.0       # Area [m²]
         z: 10.0                      # Measurement height [m]
         land_cover:                  # Fractions (must sum to 1.0)
           paved:
             sfr: 0.30
           bldgs:
             sfr: 0.35
           grass:
             sfr: 0.20
           evetr:
             sfr: 0.10
           dectr:
             sfr: 0.05
           bsoil:
             sfr: 0.00
           water:
             sfr: 0.00
       initial_states:                # Initial conditions
         # Default values will be used if not specified

For a complete working example, see the `sample configuration <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/sample_run/sample_config.yml>`_.

Validation and Error Handling
------------------------------

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

Essential Parameters
--------------------

Time Configuration (``model.control``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 30 20 50

   * - Parameter
     - Type
     - Description
   * - ``tstep``
     - integer
     - Model timestep [s]
   * - ``start_date``
     - string
     - Simulation start (YYYY-MM-DD)
   * - ``end_date``
     - string
     - Simulation end (YYYY-MM-DD)

Input/Output Files (``model.control``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 30 20 50

   * - Parameter
     - Type
     - Description
   * - ``forcing_file``
     - string/list
     - Meteorological data file(s)
   * - ``output_file``
     - string/dict
     - Output configuration

**Multiple Forcing Files:**

.. code-block:: yaml

   forcing_file:
     - "forcing_2020.txt"
     - "forcing_2021.txt"
     - "forcing_2022.txt"

Files are automatically concatenated in chronological order.

**Output Configuration:**

.. code-block:: yaml

   # Simple text output (backward compatible)
   output_file: "output.txt"

   # Advanced configuration
   output_file:
     format: parquet              # or 'txt'
     freq: 1800                   # Output every 30 min
     groups: ["SUEWS", "RSL"]     # Output groups (txt only)

.. note:: **Parquet Output Format**

   Parquet is an efficient columnar format that produces files 70-80% smaller than text:

   - **Single file** per site (vs multiple text files)
   - **Fast loading** especially for specific columns
   - **Requires PyArrow**: ``pip install pyarrow``

   Reading Parquet files:

   .. code-block:: python

      import pandas as pd
      df = pd.read_parquet('TestSite_SUEWS_output.parquet')
      qh = df[('SUEWS', 'QH')]  # Access specific variable

Physics Methods (``model.physics``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 30 15 55

   * - Parameter
     - Options
     - Description
   * - ``net_radiation_method``
     - 0, 1, 3
     - | 0: Observed Q*
       | 1: Observed LW↓
       | 3: Model LW↓ (recommended)
   * - ``emissions_method``
     - 0, 1, 2, 4
     - | 0: No QF
       | 1: Simple daily profile
       | 2: Temperature-dependent
       | 4: Full model (buildings+traffic)
   * - ``storage_heat_method``
     - 1, 2, 3
     - | 1: OHM without QF
       | 2: OHM with QF
       | 3: AnOHM
   * - ``stability_method``
     - 0, 2, 3
     - | 0: Neutral
       | 2: Least stable
       | 3: Campbell & Norman (recommended)

Site Properties (``site.properties``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Essential Parameters:**

- **Location**: ``lat``, ``lng``, ``alt``, ``timezone``
- **Morphology**: ``surfacearea``, ``z`` (measurement height)
- **Population**: ``pop_dens_daytime``, ``pop_dens_nighttime``
- **Roughness**: ``z0m_summer``, ``z0m_winter``

**Land Cover Fractions** (must sum to 1.0):

- ``paved`` - Paved surfaces
- ``bldgs`` - Buildings
- ``evetr`` - Evergreen trees
- ``dectr`` - Deciduous trees
- ``grass`` - Grass/lawn
- ``bsoil`` - Bare soil
- ``water`` - Water bodies

Advanced Features
-----------------

SPARTACUS 3D Radiation
~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~

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

**Land cover doesn't sum to 1.0**
   Check your fractions add up exactly to 1.0

**Missing required parameters**
   Enable annotated YAML generation (see Validation section above)

**Energy balance not closing**
   - Verify albedo values are realistic
   - Check OHM coefficients
   - Ensure radiation method matches data availability

**No evapotranspiration**
   - Check LAI values > 0 for vegetation
   - Verify soil moisture is adequate
   - Ensure surface conductance parameters are set

Schema Reference
----------------

For complete parameter documentation:

- :doc:`schema/model` - All model-level configuration parameters
- :doc:`schema/site` - All site-specific parameters

Additional Resources
--------------------

- `Sample configuration <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/sample_run/sample_config.yml>`_ - Complete working example
- :ref:`met_input` - Forcing data format
- :ref:`output_files` - Output file descriptions