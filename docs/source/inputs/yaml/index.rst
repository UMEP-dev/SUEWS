.. _yaml_input:

.. meta::
   :description: SUEWS YAML configuration format documentation for site parameters, model control, and surface properties
   :keywords: SUEWS, YAML, configuration, parameters, latitude, longitude, lat, lng, site properties, model control, forcing file

YAML Configuration Format
=========================

The YAML configuration format is the recommended method for providing inputs to SUEWS. It uses a single, structured `config_suews.yml` file to define all model parameters, making simulations easier to manage and reproduce.

Overview
--------

A SUEWS YAML configuration file is organized into two main sections:

1. :ref:`model <model>`: Contains global settings that control the simulation, such as physics options, time stepping, and file paths.
2. :ref:`sites <site>`: A list of one or more sites to be simulated. Each site has its own set of properties, initial conditions, and land cover characteristics.

Here's a minimal example of the YAML structure:

.. code-block:: yaml

   # SUEWS configuration file
   model:
     control:
       tstep: 3600
       forcing_file: "forcing.txt"
     physics:
       net_radiation_method: 3
   
   sites:
     - name: "London_KCL"
       latitude: 51.5115
       longitude: -0.1160
       land_cover:
         paved: 0.38
         bldgs: 0.37
         grass: 0.14

For a complete working example, please refer to the `sample configuration file <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/sample_run/sample_config.yml>`_ provided with SuPy.

Validation and Error Handling
-----------------------------

When loading a YAML configuration file, SUEWS performs comprehensive validation to ensure all required parameters are present and valid. If validation errors occur:

1. **Clear error messages** are displayed in the log, listing all missing or invalid parameters
2. **Instructions are provided** on how to generate an annotated YAML file to help you fix the issues

To generate an annotated YAML file, you have two options:

1. **Manual generation**: Call ``config.generate_annotated_yaml('path/to/config.yml')`` after loading
2. **Automatic generation**: Pass ``auto_generate_annotated=True`` when loading:
   
   .. code-block:: python
   
       config = SUEWSConfig.from_yaml('config.yml', auto_generate_annotated=True)

The annotated file includes:

- **Location**: ``{config_file}_annotated.yml`` in the same directory as your config
- **Error markers**: Missing parameters marked with ``[ERROR] MISSING:``
- **Help tips**: Suggested fixes marked with ``[TIP] ADD HERE:``
- **Parameter descriptions**: Each error includes the parameter description and expected type

This feature significantly simplifies the process of creating valid configuration files, especially for new users or when using advanced physics options that require additional parameters.


Data Files
----------

In addition to the YAML configuration file, SUEWS works with input and output data files:

**Input Data:**
- **Forcing data**: Meteorological time-series data specified by :ref:`model.control.forcing_file <modelcontrol>`

  The ``forcing_file`` parameter supports two modes:
  
  1. **Single file**: Specify a path to a single forcing file
     
     .. code-block:: yaml
     
        model:
          control:
            forcing_file: "forcing_2020.txt"
     
  2. **Multiple files**: Specify a list of file paths
     
     .. code-block:: yaml
     
        model:
          control:
            forcing_file: 
              - "forcing_2020.txt"
              - "forcing_2021.txt"
              - "forcing_2022.txt"
     
     When multiple files are provided, they will be automatically loaded and concatenated in chronological order.

**Output Data:**
- **Model results**: Time-series output files configured by :ref:`model.control.output_file <modelcontrol>`

  The ``output_file`` parameter now supports advanced configuration:
  
  1. **Output format**: Choose between 'txt' (traditional text files) or 'parquet' (efficient columnar format)
  2. **Output frequency**: Specify custom output frequency in seconds
     - Single value: e.g., ``freq: 3600`` for hourly output
     - Must be a multiple of the model timestep
  3. **Output groups**: Select which groups to save (txt format only)
  
  .. note::
     **Backward Compatibility**: When using a simple string value for ``output_file`` (e.g., ``output_file: "output.txt"``), 
     SUEWS will use default settings: txt format, hourly output (3600s), and save only the SUEWS and DailyState groups. 
     This ensures compatibility with existing configuration files.
  
  Example configurations:
  
  .. code-block:: yaml
  
     # Simple backward-compatible configuration (saves only SUEWS and DailyState)
     output_file: "output.txt"
  
     # Parquet output with hourly data
     output_file:
       format: parquet
       freq: 3600
       
     # Text output with selected groups at 30-minute intervals
     output_file:
       format: txt
       freq: 1800
       groups: ["SUEWS", "DailyState", "debug"]

  **Output File Naming Convention**:
  
  - **Text format**: 
    
    - Regular groups: ``{site_name}_{year}_{group}_{freq_min}.txt``
      
      - ``site_name``: Name from site configuration  
      - ``year``: Year of simulation
      - ``group``: Output group name (SUEWS, RSL, BL, debug, etc.)
      - ``freq_min``: Output frequency in minutes
      - Example: ``London_KCL_2020_SUEWS_60.txt``
    
    - DailyState: ``{site_name}_{year}_DailyState.txt``
      
      - No frequency suffix as it always contains daily data
      - Example: ``London_KCL_2020_DailyState.txt``
  
  - **Parquet format**: 
    
    - Output data: ``{site_name}_SUEWS_output.parquet``
      
      - All groups and frequencies saved in a single file
      - Contains all years of simulation data
      - Example: ``London_KCL_SUEWS_output.parquet``
    
    - Final state: ``{site_name}_SUEWS_state_final.parquet``
      
      - Final model state for restart runs
      - Example: ``London_KCL_SUEWS_state_final.parquet``
    
    Note: Parquet format does not split by year - all simulation data is in one file

For detailed information about:

- **Input data format and variables**: see :ref:`met_input`
- **Output file formats and variables**: see :ref:`output_files`
- **Output configuration options**: see the `Output Data`_ section above
- **Parquet output format**: see :ref:`parquet_note`

Creating Configuration Files
-----------------------------

To get started with SUEWS configuration:

1. **Start with the Parameter Guide**: See :doc:`parameter_guide` for practical guidance on configuring parameters for your specific use case
2. **Use sample configurations**: Adapt the `sample configuration files <https://github.com/UMEP-dev/SUEWS/blob/master/src/supy/sample_run/sample_config.yml>`_ provided with SuPy
3. **Reference the schema**: Use the detailed schema documentation below for complete parameter specifications

.. note::

   A command-line configuration wizard tool is currently in development and will provide an interactive way to create valid YAML configuration files.

Data Model Schema
-----------------

The following pages provide a detailed reference for every component of the YAML data model. Each page corresponds to a specific parameter group and details its available keys, expected data types, units, and default values.

**Key Features of the Schema Documentation:**

- **Clear Method Descriptions**: All model physics methods now include explicit explanations of what each numeric option represents (e.g., 0=MOST, 1=RST, 2=VARIABLE for diagnostic methods)
- **Comprehensive Parameter Documentation**: Every parameter includes its purpose, units, default values, and constraints
- **Hierarchical Organization**: The schema is organized hierarchically, with top-level components (model, site) linking to detailed sub-components
- **Cross-References**: Related parameters and methods are cross-referenced for easier navigation

.. toctree::
   :maxdepth: 2
   :caption: Configuration Guides
   
   parameter_guide

.. toctree::
   :maxdepth: 2
   :caption: Schema Reference

   schema/model
   schema/site

.. toctree::
   :maxdepth: 1
   :caption: Parameter References
   
   parameter_quick_reference
   parameter_index