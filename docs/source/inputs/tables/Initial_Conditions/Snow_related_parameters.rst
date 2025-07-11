.. _Snow_related_parameters:

Snow related parameters
~~~~~~~~~~~~~~~~~~~~~~~

.. option:: SnowInitially

	:Requirement:
		Optional
	:Description:
		Flag for initial snow status [0 or 1]
	:Configuration:
		If the model run starts when there is no snow on the ground, set `SnowInitially` = 0 and the snow-related parameters will be set accordingly. If the model run starts when there is snow on the ground, the following snow-related parameters must be set appropriately. The value of `SnowInitially` overrides any values provided for the individual snow-related parameters. To prevent `SnowInitially` from setting the initial conditions, either omit it from the namelist or set to -999. If values are provided individually, they should be consistent the information provided in `SUEWS_Snow.txt` .


.. option:: SnowWaterPavedState

	:Requirement:
		Optional
	:Description:
		Initial amount of liquid water in the snow on paved surfaces `Paved`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowWaterBldgsState

	:Requirement:
		Optional
	:Description:
		Initial amount of liquid water in the snow on buildings `Bldgs`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowWaterEveTrState

	:Requirement:
		Optional
	:Description:
		Initial amount of liquid water in the snow on evergreen trees `EveTr`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowWaterDecTrState

	:Requirement:
		Optional
	:Description:
		Initial amount of liquid water in the snow on deciduous trees `DecTr`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowWaterGrassState

	:Requirement:
		Optional
	:Description:
		Initial amount of liquid water in the snow on grass surfaces `Grass`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowWaterBSoilState

	:Requirement:
		Optional
	:Description:
		Initial amount of liquid water in the snow on bare soil surfaces `BSoil`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowWaterWaterState

	:Requirement:
		Optional
	:Description:
		Initial amount of liquid water in the snow in water `Water`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowPackPaved

	:Requirement:
		Optional
	:Description:
		Initial snow water equivalent if the snow on paved surfaces `Paved`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowPackBldgs

	:Requirement:
		Optional
	:Description:
		Initial snow water equivalent if the snow on buildings `Bldgs`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowPackEveTr

	:Requirement:
		Optional
	:Description:
		Initial snow water equivalent if the snow on evergreen trees `EveTr`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowPackDecTr

	:Requirement:
		Optional
	:Description:
		Initial snow water equivalent if the snow on deciduous trees `DecTr`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowPackGrass

	:Requirement:
		Optional
	:Description:
		Initial snow water equivalent if the snow on grass surfaces `Grass`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowPackBSoil

	:Requirement:
		Optional
	:Description:
		Initial snow water equivalent if the snow on bare soil surfaces `BSoil`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowPackWater

	:Requirement:
		Optional
	:Description:
		Initial snow water equivalent if the snow on water `Water`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowFracPaved

	:Requirement:
		Optional
	:Description:
		Initial plan area fraction of snow on paved surfaces `Paved`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowFracBldgs

	:Requirement:
		Optional
	:Description:
		Initial plan area fraction of snow on buildings `Bldgs`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowFracEveTr

	:Requirement:
		Optional
	:Description:
		Initial plan area fraction of snow on evergreen trees `EveTr`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowFracDecTr

	:Requirement:
		Optional
	:Description:
		Initial plan area fraction of snow on deciduous trees `DecTr`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowFracGrass

	:Requirement:
		Optional
	:Description:
		Initial plan area fraction of snow on grass surfaces `Grass`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowFracBSoil

	:Requirement:
		Optional
	:Description:
		Initial plan area fraction of snow on bare soil surfaces `BSoil`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowFracWater

	:Requirement:
		Optional
	:Description:
		Initial plan area fraction of snow on water `Water`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowDensPaved

	:Requirement:
		Optional
	:Description:
		Initial snow density on paved surfaces `Paved`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowDensBldgs

	:Requirement:
		Optional
	:Description:
		Initial snow density on buildings `Bldgs`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowDensEveTr

	:Requirement:
		Optional
	:Description:
		Initial snow density on evergreen trees `EveTr`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowDensDecTr

	:Requirement:
		Optional
	:Description:
		Initial snow density on deciduous trees `DecTr`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowDensGrass

	:Requirement:
		Optional
	:Description:
		Initial snow density on grass surfaces `Grass`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowDensBSoil

	:Requirement:
		Optional
	:Description:
		Initial snow density on bare soil surfaces `BSoil`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowDensWater

	:Requirement:
		Optional
	:Description:
		Initial snow density on `Water`
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`


.. option:: SnowAlb0

	:Requirement:
		Optional
	:Description:
		Initial snow albedo
	:Configuration:
		The recommended values can be found from `SUEWS_Snow.txt`