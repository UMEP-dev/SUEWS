.. meta::
   :description: SUEWS YAML configuration for output configuration parameters
   :keywords: SUEWS, YAML, outputconfig, parameters, configuration

.. _outputconfig:

.. index::
   single: OutputConfig (YAML parameter)
   single: YAML; OutputConfig

Output Configuration
====================

Configuration for model output files.

**Parameters:**

.. index::
   single: format (YAML parameter)
   single: OutputConfig; format

.. option:: format

   Output file format. Options: 'txt' for traditional text files (one per year/grid/group), 'parquet' for single Parquet file containing all data

   :Default: ``txt`` (TXT)

.. index::
   single: freq (YAML parameter)
   single: OutputConfig; freq

.. option:: freq

   Output frequency in seconds. Must be a multiple of the model timestep (tstep). If not specified, defaults to 3600 (hourly)

   :Default: Required - must be specified

.. index::
   single: groups (YAML parameter)
   single: OutputConfig; groups

.. option:: groups

   List of output groups to save (only applies to txt format). Available groups: 'SUEWS', 'DailyState', 'snow', 'ESTM', 'RSL', 'BL', 'debug'. If not specified, defaults to ['SUEWS', 'DailyState']

   :Default: Required - must be specified
