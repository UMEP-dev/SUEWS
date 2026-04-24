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


Restart Checkpoint
------------------

{site}_SUEWS_checkpoint.json
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Object-oriented SUEWS runs save ``{site}_SUEWS_checkpoint.json`` as the
preferred restart artefact. It contains typed runtime state from the backend,
keyed by grid ID.

The checkpoint is intentionally only the typed runtime state. To continue a run,
load the same YAML configuration, attach the next forcing period, and run from
``SUEWSSimulation.from_checkpoint(...)``.

Legacy ``df_state_SSss.csv`` and state parquet files remain documented for
backwards compatibility and developer inspection, but they are not the preferred
restart artefact for new object-oriented workflows.


Temporal Information
--------------------

.. note::

   Temporal information in output files (``iy``, ``id``, ``it``, ``imin``) are in **local time**
   (consistent with :ref:`met_forcing`) and indicate the **ending timestamp** of each period.

   For example, for hourly data, ``2021-09-12 13:00`` indicates a record for the period
   between ``2021-09-12 12:00`` (inclusive) and ``2021-09-12 13:00`` (exclusive).

   **Exception for DailyState**: When resampled to daily frequency, DailyState uses
   day-start labelling for readability. See :ref:`output-dailystate` for details.
