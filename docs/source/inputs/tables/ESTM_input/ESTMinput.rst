

:nosearch:

ESTMinput
~~~~~~~~~

.. option:: TsurfChoice

	:Requirement:
		Required
	:Description:
		Source of surface temperature data used.
	:Configuration:
		.. csv-table::
			:file: TsurfChoice.csv
			:header-rows: 1
			:widths: auto


.. option:: evolveTibld

	:Requirement:
		Required
	:Description:
		Source of internal building temperature (Tibld)
	:Configuration:
		.. csv-table::
			:file: evolveTibld.csv
			:header-rows: 1
			:widths: auto


.. option:: IbldCHmod

	:Requirement:
		Required
	:Description:
		Method to calculate internal convective heat exchange coefficients (CH) for internal building, wall and roof if evolveTibld is 1 or 2.
	:Configuration:
		.. csv-table::
			:file: IbldCHmod.csv
			:header-rows: 1
			:widths: auto


.. option:: LBC_soil

	:Requirement:
		Required

	:Description:
		Soil temperature at lowest boundary condition [˚C]
	:Configuration:
		to fill


.. option:: Theat_fix

	:Requirement:
		Required

	:Description:
		Ideal internal building temperature [˚C]
	:Configuration:
		to fill


.. option:: Theat_off

	:Requirement:
		Required

	:Description:
		Temperature at which heat control is turned off (used when evolveTibld=1) [˚C]
	:Configuration:
		to fill


.. option:: Theat_on

	:Requirement:
		Required

	:Description:
		Temperature at which heat control is turned on (used when evolveTibld =1) [˚C]
	:Configuration:
		to fill
