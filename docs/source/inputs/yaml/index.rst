.. _yaml_input:

YAML Configuration Format
=========================

The YAML configuration format is the recommended method for providing inputs to SUEWS. It uses a single, structured `config_suews.yml` file to define all model parameters, making simulations easier to manage and reproduce.

Overview
--------

A SUEWS YAML configuration file is organized into two main sections:

1. **model**: Contains global settings that control the simulation, such as physics options, time stepping, and file paths.
2. **sites**: A list of one or more sites to be simulated. Each site has its own set of properties, initial conditions, and land cover characteristics.

For a complete working example, please refer to the sample `config_suews.yml` file provided with SuPy.

Configuration Builder (Recommended)
------------------------------------

.. tip::

   **Interactive Configuration Builder**: Use our web-based configuration builder to create YAML files easily with guided forms and real-time validation.

   📝 **Access the builder**: `SUEWS Configuration Builder <../../_static/index.html>`__

   Features:

   - **Interactive forms**: Step-by-step guided configuration
   - **Real-time validation**: Immediate feedback on parameter values
   - **Export options**: Download complete YAML files ready for SUEWS
   - **Parameter help**: Built-in documentation for all parameters
   - **Schema validation**: Ensures your configuration is correct before running

Data Model Schema
-----------------

The following pages provide a detailed reference for every component of the YAML data model. Each page corresponds to a specific parameter group and details its available keys, expected data types, units, and default values.

.. toctree::
   :maxdepth: 2
   :caption: Schema Reference

   schema/model
   schema/site