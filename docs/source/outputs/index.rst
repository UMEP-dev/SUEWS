.. _output_files:

Output Files
============

SUEWS produces comprehensive output data documenting the simulation results, including
energy balance, water balance, meteorological variables, and model diagnostics.

Overview
--------

SUEWS output is organised into logical groups, each containing related variables:

**Core Output Groups:**

- **datetime** - Temporal indices for all output records
- **SUEWS** - Main energy and water balance outputs (sensible heat, latent heat, etc.)
- **snow** - Snow-specific outputs for each surface type
- **DailyState** - Daily accumulated state variables

**Advanced Physics:**

- **ESTM** - Element Surface Temperature Model outputs
- **EHC** - Element Heat Capacity model outputs for building thermal layers
- **RSL** - Roughness Sublayer vertical profile outputs
- **BL** - Boundary Layer model outputs

**Specialised Models:**

- **BEERS** - BEERS radiation model outputs
- **SPARTACUS** - SPARTACUS radiation model outputs (experimental)
- **STEBBS** - STEBBS building energy model outputs (experimental)
- **NHood** - Neighbourhood-scale outputs (experimental)

**Diagnostics:**

- **debug** - Debug and diagnostic outputs for model development


Output Format Options
---------------------

SUEWS supports two output formats:

- :doc:`text_format` - Traditional tab-delimited text files (one file per year and output group)
- :doc:`parquet_format` - Modern columnar storage format (all data in consolidated files)

The output format is configured in the YAML configuration file. See :doc:`../inputs/yaml/index` for configuration details.


Output Variable Reference
-------------------------

For a complete reference of all output variables organised by group, see:

- :doc:`variables/index` - Auto-generated documentation for all 1,100+ output variables


Legacy Diagnostics
------------------

For information about runtime diagnostic files (error messages, warnings, etc.), see:

- :doc:`legacy_diagnostics`


State Output
------------

df_state_SSss.csv
^^^^^^^^^^^^^^^^^

SUEWS automatically outputs a df_state_SSss.csv file containing:

- Model configuration parameters
- Initial and final states
- Simulation metadata (SUEWS version, timestamps, etc.)

This file preserves model state for analysis and restart purposes.


Temporal Information
--------------------

.. note::

   Temporal information in output files (``iy``, ``id``, ``it``, ``imin``) are in **local time**
   (consistent with :ref:`met_forcing`) and indicate the **ending timestamp** of each period.

   For example, for hourly data, ``2021-09-12 13:00`` indicates a record for the period
   between ``2021-09-12 12:00`` (inclusive) and ``2021-09-12 13:00`` (exclusive).


Legacy Documentation
--------------------

For users working with older output files or migrating existing workflows,
the legacy output documentation with CSV column mappings is available:

- :doc:`../output_files/output_files` - Legacy output file documentation (deprecated)


.. toctree::
   :hidden:
   :maxdepth: 2

   text_format
   parquet_format
   variables/index
   legacy_diagnostics
   ../output_files/output_files
