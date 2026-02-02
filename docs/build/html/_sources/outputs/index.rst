.. _output_files:

Output Files
============

SUEWS produces comprehensive output data documenting the simulation results, including
energy balance, water balance, meteorological variables, and model diagnostics.

The output format is configured in the YAML configuration file.
See :doc:`../inputs/yaml/index` for configuration details.

.. toctree::
   :maxdepth: 1
   :caption: Output Formats

   text_format
   parquet_format

.. toctree::
   :maxdepth: 1
   :caption: Variable Reference

   variables/index

.. toctree::
   :maxdepth: 1
   :caption: Legacy Documentation

   legacy_diagnostics
   legacy_text_columns


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

   **Exception for DailyState**: When resampled to daily frequency, DailyState uses
   day-start labelling for readability. See :ref:`output-dailystate` for details.
