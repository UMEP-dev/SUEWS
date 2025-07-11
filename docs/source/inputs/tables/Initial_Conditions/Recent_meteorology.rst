.. _Recent_meteorology:

Recent meteorology
~~~~~~~~~~~~~~~~~~

.. option:: DaysSinceRain

	:Requirement:
		Optional
	:Description:
		Days since rain [d]
	:Configuration:
		Important to use correct value if starting in summer season If starting when external water use is not occurring it will be reset with the first rain so can just be set to 0. If unknown, SUEWS sets to zero by default. Used to model irrigation.


.. option:: Temp_C0

	:Requirement:
		Optional
	:Description:
		Initial air temperature [degC]
	:Configuration:
		If unknown, SUEWS uses the mean temperature for the first day of the run.
