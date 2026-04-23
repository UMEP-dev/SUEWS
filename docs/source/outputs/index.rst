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


Restart Output
--------------

SSss_SUEWS_checkpoint.json
^^^^^^^^^^^^^^^^^^^^^^^^^^

The object-oriented API writes a typed checkpoint JSON file containing:

- SUEWS/SuPy version metadata
- State schema version
- Last simulated timestamp
- Typed Rust backend state for each grid

Use this file together with the YAML configuration to restart or continue a
simulation. Legacy ``save_supy()`` workflows may still produce
``df_state_SSss.csv`` for backwards compatibility.


Temporal Information
--------------------

.. note::

   Temporal information in output files (``iy``, ``id``, ``it``, ``imin``) are in **local time**
   (consistent with :ref:`met_forcing`) and indicate the **ending timestamp** of each period.

   For example, for hourly data, ``2021-09-12 13:00`` indicates a record for the period
   between ``2021-09-12 12:00`` (inclusive) and ``2021-09-12 13:00`` (exclusive).

   **Exception for DailyState**: When resampled to daily frequency, DailyState uses
   day-start labelling for readability. See :ref:`output-dailystate` for details.
