.. meta::
   :description: SUEWS YAML configuration for model control parameters
   :keywords: SUEWS, YAML, modelcontrol, parameters, configuration

.. _modelcontrol:

.. index::
   single: ModelControl (YAML parameter)
   single: YAML; ModelControl

Model Control
=============

**Parameters:**

.. index::
   single: tstep (YAML parameter)
   single: ModelControl; tstep

.. option:: tstep

   Time step in seconds for model calculations

   :Sample value: ``300``

.. index::
   single: forcing_file (YAML parameter)
   single: ModelControl; forcing_file

.. option:: forcing_file

   Path(s) to meteorological forcing data file(s). This can be either: (1) A single file path as a string (e.g., 'forcing.txt'), or (2) A list of file paths (e.g., ['forcing_2020.txt', 'forcing_2021.txt', 'forcing_2022.txt']). When multiple files are provided, they will be automatically concatenated in chronological order. The forcing data contains time-series meteorological measurements that drive SUEWS simulations. For detailed information about required variables, file format, and data preparation guidelines, see :ref:`met_input`.

   :Sample value: ``'forcing.txt'``

.. index::
   single: output_file (YAML parameter)
   single: ModelControl; output_file

.. option:: output_file

   Output file configuration. DEPRECATED: String values are ignored and will issue a warning. Please use an OutputConfig object specifying format ('txt' or 'parquet'), frequency (seconds, must be multiple of tstep), and groups to save (for txt format only). Example: {'format': 'parquet', 'freq': 3600} or {'format': 'txt', 'freq': 1800, 'groups': ['SUEWS', 'DailyState', 'ESTM']}. For detailed information about output variables and file structure, see :ref:`output_files`.

   :Sample value: ``'output.txt'``

   The ``output_file`` parameter group is defined by the :doc:`outputconfig` structure.

.. index::
   single: start_time (YAML parameter)
   single: ModelControl; start_time

.. option:: start_time

   Start time of model run. If None use forcing data bounds.

   :Default: Required - must be specified

.. index::
   single: end_time (YAML parameter)
   single: ModelControl; end_time

.. option:: end_time

   End time of model run. If None use forcing data bounds.

   :Default: Required - must be specified

.. index::
   single: ref (YAML parameter)
   single: ModelControl; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
