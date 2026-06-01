.. _Above_ground_state:



Above ground state
~~~~~~~~~~~~~~~~~~

.. option:: PavedState

	:Requirement:
		Optional
	:Description:
		Initial wetness condition on `Paved`
	:Configuration:
		If unknown, model assumes dry surfaces (acceptable as rainfall or irrigation will update these states quickly).


.. option:: BldgsState

	:Requirement:
		Optional
	:Description:
		Initial wetness condition on `Bldgs`
	:Configuration:
		If unknown, model assumes dry surfaces (acceptable as rainfall or irrigation will update these states quickly).


.. option:: EveTrState

	:Requirement:
		Optional
	:Description:
		Initial wetness condition on `EveTr`
	:Configuration:
		If unknown, model assumes dry surfaces (acceptable as rainfall or irrigation will update these states quickly).


.. option:: DecTrState

	:Requirement:
		Optional
	:Description:
		Initial wetness condition on `DecTr`
	:Configuration:
		If unknown, model assumes dry surfaces (acceptable as rainfall or irrigation will update these states quickly).


.. option:: GrassState

	:Requirement:
		Optional
	:Description:
		Initial wetness condition on `Grass`
	:Configuration:
		If unknown, model assumes dry surfaces (acceptable as rainfall or irrigation will update these states quickly).


.. option:: BSoilState

	:Requirement:
		Optional
	:Description:
		Initial wetness condition on `BSoil`
	:Configuration:
		If unknown, model assumes dry surfaces (acceptable as rainfall or irrigation will update these states quickly).


.. option:: WaterState

	:Requirement:
		Optional
	:Description:
		Initial wetness condition on `Water`
	:Configuration:
		For a large water body (e.g. river, sea, lake) set WaterState to a large value, e.g. 20000 mm; for small water bodies (e.g. ponds, fountains) set WaterState to smaller value, e.g. 1000 mm. This value must not exceed StateLimit specified in SUEWS_Water.txt . If unknown, model uses value of WaterDepth specified in SUEWS_Water.txt .
