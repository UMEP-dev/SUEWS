
:nosearch:

CBLinput
~~~~~~~~

.. option:: EntrainmentType

	:Requirement:
		Required
	:Description:
		Determines entrainment scheme. See Cleugh and Grimmond 2000 [16] for details.
	:Configuration:
		.. csv-table::
			:file: EntrainmentType.csv
			:header-rows: 1
			:widths: auto


.. option:: QH_Choice

	:Requirement:
		Required
	:Description:
		Determines QH used for CBL model.
	:Configuration:
		.. csv-table::
			:file: QH_Choice.csv
			:header-rows: 1
			:widths: auto


.. option:: InitialData_use

	:Requirement:
		Required
	:Description:
		Determines initial values (see `CBL_Initial_data.txt`)
	:Configuration:
		.. csv-table::
			:file: InitialData_use.csv
			:header-rows: 1
			:widths: auto


.. option:: Sondeflag

	:Requirement:
		Required
	:Description:
		to fill
	:Configuration:
		.. csv-table::
			:file: Sondeflag.csv
			:header-rows: 1
			:widths: auto


.. option:: CBLday(id)

	:Requirement:
		Required

	:Description:
		Set CBLday(id) = 1 If CBL model is set to run for DOY 175–177, CBLday(175) = 1, CBLday(176) = 1, CBLday(177) = 1
	:Configuration:
		to fill


.. option:: CO2_included

	:Requirement:
		Required

	:Description:
		Set to zero in current version
	:Configuration:
		to fill


.. option:: FileSonde(id)

	:Requirement:
		Required

	:Description:
		If Sondeflag=1, write the file name including the path from site directory e.g. FileSonde(id)= 'CBLinputfiles\XXX.txt', XXX is an arbitrary name.
	:Configuration:
		to fill


.. option:: InitialDataFileName

	:Requirement:
		Required

	:Description:
		If InitialData_use ≥ 1, write the file name including the path from site directory e.g. InitialDataFileName='CBLinputfiles\CBL_initial_data.txt'
	:Configuration:
		to fill


.. option:: Wsb

	:Requirement:
		Required

	:Description:
		Subsidence velocity (m |s^-1| ) in eq. 1 and 2 of Onomura et al. (2015) [17] . (-0.01 m |s^-1| |Recmd|)
	:Configuration:
		to fill
