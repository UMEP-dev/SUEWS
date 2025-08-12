.. _File_related_options:

:nosearch:

File related options
~~~~~~~~~~~~~~~~~~~~

.. option:: FileCode

	:Requirement:
		Required

	:Description:
		Alphabetical site identification code (e.g. He, Sc, Kc).
	:Configuration:
		This must be consistent with names of `meterological input file <met_input>` and  `initial condition files <Initial_Conditions>`


.. option:: FileInputPath

	:Requirement:
		Required

	:Description:
		Input directory.
	:Configuration:
		This can be set either as `an absolute path <path_setting>`_ or `a relative path <path_setting>`_ where the program is initiated.


.. option:: FileOutputPath

	:Requirement:
		Required

	:Description:
		Output directory.
	:Configuration:
		This can be set either as `an absolute path <path_setting>`_ or `a relative path <path_setting>`_ where the program is initiated.


.. option:: MultipleMetFiles

	:Requirement:
		Required
	:Description:
		Specifies whether one single meteorological forcing file is used for all grids or a separate met file is provided for each grid.
	:Configuration:
		.. csv-table::
			:file: csv-table/MultipleMetFiles.csv
			:header-rows: 1
			:widths: auto


.. option:: MultipleInitFiles

	:Requirement:
		Required
	:Description:
		Specifies whether one single initial conditions file is used for all grids at the start of the run or a separate initial conditions file is provided for each grid.
	:Configuration:
		.. csv-table::
			:file: csv-table/MultipleInitFiles.csv
			:header-rows: 1
			:widths: auto


.. option:: MultipleESTMFiles

	:Requirement:
		Optional
	:Description:
		Specifies whether one single ESTM forcing file is used for all grids or a separate file is provided for each grid.
	:Configuration:
		.. csv-table::
			:file: csv-table/MultipleESTMFiles.csv
			:header-rows: 1
			:widths: auto


.. option:: KeepTstepFilesIn

	:Requirement:
		Optional
	:Description:
		Specifies whether input meteorological forcing files at the resolution of the model time step should be saved.
	:Configuration:
		.. csv-table::
			:file: csv-table/KeepTstepFilesIn.csv
			:header-rows: 1
			:widths: auto


.. option:: KeepTstepFilesOut

	:Requirement:
		Optional
	:Description:
		Specifies whether output meteorological forcing files at the resolution of the model time step should be saved.
	:Configuration:
		.. csv-table::
			:file: csv-table/KeepTstepFilesOut.csv
			:header-rows: 1
			:widths: auto


.. option:: WriteOutOption

	:Requirement:
		Optional
	:Description:
		Specifies which variables are written in the output files. For detailed information about output file formats and variables, see :ref:`output_files`.
	:Configuration:
		.. csv-table::
			:file: csv-table/WriteOutOption.csv
			:header-rows: 1
			:widths: auto


.. option:: SuppressWarnings

	:Requirement:
		Optional
	:Description:
		Controls whether the warnings.txt file is written or not.
	:Configuration:
		.. csv-table::
			:file: csv-table/SuppressWarnings.csv
			:header-rows: 1
			:widths: auto

.. _path_setting: https://en.wikipedia.org/wiki/Path_(computing)#Absolute_and_relative_paths
