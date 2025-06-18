

SOLWEIGinput
~~~~~~~~~~~~

.. option:: Posture

	:Requirement:
		Required
	:Description:
		Determines the posture of a human for which the radiant fluxes should be considered
	:Configuration:
		.. csv-table::
			:file: Posture.csv
			:header-rows: 1
			:widths: auto


.. option:: usevegdem

	:Requirement:
		Required
	:Description:
		Vegetation scheme
	:Configuration:
		.. csv-table::
			:file: usevegdem.csv
			:header-rows: 1
			:widths: auto


.. option:: onlyglobal

	:Requirement:
		Required
	:Description:
		Global radiation
	:Configuration:
		.. csv-table::
			:file: onlyglobal.csv
			:header-rows: 1
			:widths: auto


.. option:: SOLWEIGpoi_out

	:Requirement:
		Required
	:Description:
		Write output variables at point of interest (see below)
	:Configuration:
		.. csv-table::
			:file: SOLWEIGpoi_out.csv
			:header-rows: 1
			:widths: auto


.. option:: Tmrt_out

	:Requirement:
		Required
	:Description:
		-
	:Configuration:
		.. csv-table::
			:file: Tmrt_out.csv
			:header-rows: 1
			:widths: auto


.. option:: Lup2d_out

	:Requirement:
		Required
	:Description:
		-
	:Configuration:
		.. csv-table::
			:file: Lup2d_out.csv
			:header-rows: 1
			:widths: auto


.. option:: Ldown2d_out

	:Requirement:
		Required
	:Description:
		-
	:Configuration:
		.. csv-table::
			:file: Ldown2d_out.csv
			:header-rows: 1
			:widths: auto


.. option:: Kup2d_out

	:Requirement:
		Required
	:Description:
		-
	:Configuration:
		.. csv-table::
			:file: Kup2d_out.csv
			:header-rows: 1
			:widths: auto


.. option:: Kdown2d_out

	:Requirement:
		Required
	:Description:
		-
	:Configuration:
		.. csv-table::
			:file: Kdown2d_out.csv
			:header-rows: 1
			:widths: auto


.. option:: GVF_out

	:Requirement:
		Required
	:Description:
		-
	:Configuration:
		.. csv-table::
			:file: GVF_out.csv
			:header-rows: 1
			:widths: auto


.. option:: SOLWEIG_ldown

	:Requirement:
		Required
	:Description:
		-
	:Configuration:
		.. csv-table::
			:file: SOLWEIG_ldown.csv
			:header-rows: 1
			:widths: auto


.. option:: RunForGrid

	:Requirement:
		Required
	:Description:
		Grid for which SOLWEIG should be run.
	:Configuration:
		.. csv-table::
			:file: RunForGrid.csv
			:header-rows: 1
			:widths: auto


.. option:: absK

	:Requirement:
		Required

	:Description:
		Recommended value: 0.70
	:Configuration:
		to fill


.. option:: absL

	:Requirement:
		Required

	:Description:
		Recommended value: 0.97
	:Configuration:
		to fill


.. option:: BuildingName

	:Requirement:
		Required

	:Description:
		Boolean matrix for locations of building pixels
	:Configuration:
		to fill


.. option:: CDSMname

	:Requirement:
		Required

	:Description:
		Vegetation canopy DSM
	:Configuration:
		to fill


.. option:: col

	:Requirement:
		Required

	:Description:
		Y coordinate for point of interest. Here all variables from the model will written to SOLWEIGpoiOUT.txt
	:Configuration:
		to fill


.. option:: DSMname

	:Requirement:
		Required

	:Description:
		Ground and Building DSM
	:Configuration:
		to fill


.. option:: DSMPath

	:Requirement:
		Required

	:Description:
		Path to Digital Surface Models (DSM).
	:Configuration:
		to fill


.. option:: heightgravity

	:Requirement:
		Required

	:Description:
		Recommended value for a standing man: 1.1 m
	:Configuration:
		to fill


.. option:: OutInterval

	:Requirement:
		Required

	:Description:
		Output interval. Set to 60 in current version.
	:Configuration:
		to fill


.. option:: row

	:Requirement:
		Required

	:Description:
		X coordinate for point of interest. Here all variables from the model will written to SOLWEIGpoiOUT.txt
	:Configuration:
		to fill


.. option:: SVFPath

	:Requirement:
		Required

	:Description:
		Path to SVFs matrices (See Lindberg and Grimmond (2011) [19] for details)
	:Configuration:
		to fill


.. option:: SVFSuffix

	:Requirement:
		Required

	:Description:
		Suffix used (if any)
	:Configuration:
		to fill


.. option:: TDSMname

	:Requirement:
		Required

	:Description:
		Vegetation trunk zone DSM
	:Configuration:
		to fill


.. option:: TransMax

	:Requirement:
		Required

	:Description:
		Recommended value: 0.50 (Konarska et al. 2014 :cite:`K14`)
	:Configuration:
		to fill


.. option:: TransMin

	:Requirement:
		Required

	:Description:
		Recommended value: 0.02 (Konarska et al. 2014 :cite:`K14` )
	:Configuration:
		to fill
