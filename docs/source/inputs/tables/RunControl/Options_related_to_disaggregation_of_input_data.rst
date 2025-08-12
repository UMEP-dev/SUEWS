.. _Options_related_to_disaggregation_of_input_data:

:nosearch:

Options related to disaggregation of input data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. option:: DisaggMethod

	:Requirement:
		Optional
	:Description:
		Specifies how meteorological variables in the input file (except rain and snow) are disaggregated to the model time step.
		Wind direction is not currently downscaled so non -999 values will cause an error.
	:Configuration:
		.. csv-table::
			:file: csv-table/DisaggMethod.csv
			:header-rows: 1
			:widths: auto


.. option:: KdownZen

	:Requirement:
		Optional
	:Description:
		Can be used to switch off zenith checking in Kdown disaggregation. Note that the zenith calculation requires location information obtained from `SUEWS_SiteSelect.txt`. If a single met file is used for all grids, the zenith is calculated for the first grid and the disaggregated data is then applied for all grids.
	:Configuration:
		.. csv-table::
			:file: csv-table/KdownZen.csv
			:header-rows: 1
			:widths: auto


.. option:: RainDisaggMethod

	:Requirement:
		Optional
	:Description:
		Specifies how rain in the meteorological forcing file are disaggregated to the model time step. If present in the original met forcing file, snow is currently disaggregated in the same way as rainfall.
	:Configuration:
		.. csv-table::
			:file: csv-table/RainDisaggMethod.csv
			:header-rows: 1
			:widths: auto


.. option:: RainAmongN

	:Requirement:
		Optional

	:Description:
		Specifies the number of subintervals (of length tt) over which to distribute rainfall in each interval (of length TT).
	:Configuration:
		Must be an integer value. Use with RainDisaggMethod = 101.


.. option:: MultRainAmongN

	:Requirement:
		Optional

	:Description:
		Specifies the number of subintervals (of length tt) over which to distribute rainfall in each interval (of length TT) for up to 5 intensity bins. Must take integer values.
	:Configuration:
		Use with RainDisaggMethod = 102.
		e.g. MultRainAmongN(1) = 5, MultRainAmongN(2) = 8, MultRainAmongN(3) = 12


.. option:: MultRainAmongNUpperI

	:Requirement:
		Optional

	:Description:
		Specifies upper limit for each intensity bin to apply MultRainAmongN.
	:Configuration:
		Any intensities above the highest specified intensity will use the last MultRainAmongN value and write a warning to `warnings.txt`.
		Use with RainDisaggMethod = 102.
		e.g. MultRainAmongNUpperI(1) = 0.5, MultRainAmongNUpperI(2) = 2.0, MultRainAmongNUpperI(3) = 50.0


.. option:: DisaggMethodESTM

	:Requirement:
		Optional
	:Description:
		Specifies how ESTM-related temperatures in the input file are disaggregated to the model time step.
	:Configuration:
		.. csv-table::
			:file: csv-table/DisaggMethodESTM.csv
			:header-rows: 1
			:widths: auto
