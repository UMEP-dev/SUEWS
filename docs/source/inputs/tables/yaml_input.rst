.. _yaml_input:

YAML-based Input File
=====================

The SUEWS YAML-based input file provides a transparent, hierarchical, and human-readable way to configure your model runs.

Overview
--------

The YAML input is structured around two principal sections:

- ``model``: Controls all aspects of model behaviour, simulation period, time step, physical schemes, and output configuration.
- ``sites``: Defines the properties of each site, including geographical information, land cover characteristics, surface parameters, anthropogenic emissions, and initial conditions.

This separation allows you to flexibly manage multiple sites with a single set of model settings, or to tailor site-specific configurations as needed.

Interactive Configuration Builder
----------------------------------

.. tip::

   **NEW**: Use our interactive web-based configuration builder to create YAML files with ease!

   🚀 **Launch the builder**: `SUEWS Configuration Builder <../../_static/index.html>`__

   The configuration builder provides:
   
   - **User-friendly interface**: No need to manually edit YAML syntax
   - **Parameter guidance**: Built-in help and documentation for each parameter
   - **Real-time validation**: Catch configuration errors before running SUEWS
   - **Export functionality**: Download your complete `config_suews.yml` file
   - **Schema compliance**: Automatically ensures your configuration follows the correct format

.. note::
   The examples below showcase a subset of the available parameters for brevity. Field names directly correspond to their definitions in the SUEWS data models. Refer to the detailed schema documentation for a comprehensive list of all configurable options and their specific data types. Many parameters are defined using a ``ValueWithDOI`` type, which means they expect a nested ``value:`` key in the YAML structure, as shown in the examples.

Model Configuration
-------------------

The ``model`` section (see :doc:`Model <../yaml_input/model>`) specifies global simulation settings and controls the behaviour of the SUEWS model engine.

Example:

.. code-block:: yaml

   model:
     control:
       tstep: 300  # Timestep in seconds
       forcing_file:
         value: Input/Kc_2012_data_60.txt  # Path to meteorological forcing data
       output_file: output.txt  # Path for main output file
       diagnose: 0
       # ... other control parameters
     physics:
       netradiationmethod:
         value: 3
       emissionsmethod:
         value: 2
       storageheatmethod:
         value: 1
       ohmincqf:
         value: 0
       roughlenmommethod:
         value: 2
       roughlenheatmethod:
         value: 2
       stabilitymethod:
         value: 3
       smdmethod:
         value: 0
       waterusemethod:
         value: 0
       diagmethod:
         value: 2
       faimethod:
         value: 0
       localclimatemethod:
         value: 0
       snowuse:
         value: 0
       stebbsmethod:
         value: 0
       # ... other physics scheme selections
     # Note: output configuration is handled via output_file in control (legacy) or other sections as per schema

Sites Configuration
-------------------

The ``sites`` section is a dictionary where each key is a unique site name, and the value contains the configuration for that site (see :doc:`Site <../yaml_input/site>`).

Example:

.. code-block:: yaml

   sites:
     - name: TestSite1 # name of the site
       grid_id:
         value: 1 # grid id of the site
       properties: # Physical and descriptive properties of the site
         lat:
           value: 51.51
         lng:
           value: -0.12
         alt:
           value: 35.0
         z0m_in:
           value: 1.9
         zdm_in:
           value: 14.2
         frc_land_cover: # Surface cover fractions
           Paved:
             value: 0.43
           Buildings:
             value: 0.38
           EvergreenTrees:
             value: 0.00
           DeciduousTrees:
             value: 0.02
           Grass:
             value: 0.03
           BareSoil:
             value: 0.00
           Water:
             value: 0.14
         land_cover_params: # Detailed parameters for each land cover type
           Paved: # Parameters for Paved surfaces
             alb:
               value: 0.10
             emis:
               value: 0.95
             # ... other parameters for Paved surfaces
           Buildings: # Parameters for Buildings
             alb:
               value: 0.12
             emis:
               value: 0.91
             bldgh: # Average building height
               value: 22.0
             # ... other parameters for Buildings
           # ... parameters for other surface types (EvergreenTrees, Grass, etc.)
         anthropogenic_emissions: # Anthropogenic emissions data
           heat: # Anthropogenic heat flux parameters
             qf0_beu:
               working_day:
                 value: 0.88
               holiday:
                 value: 0.88
             # ... other anthropogenic heat parameters
           # ... other emission types
         # ... other site properties
       initial_states: # Initial conditions for state variables
         snowalb:
           value: 0.3
         paved:
           state:
             value: 0.0
           soilstore:
               value: 120.0
           # ... other initial states
         # ... initial states for other land cover types
      - name: TestSite2 # name of another site
        grid_id:
          value: 2
        properties:
          # ... site properties
        # ... other site configurations

Best Practices
--------------

- Use descriptive site names and document any site-specific assumptions.
- Group common model settings under ``model`` to avoid duplication.
- For multi-site studies, define each site under ``sites`` and reference shared model settings.
- Validate your YAML file with a linter or the SUEWS input checker before running simulations.
