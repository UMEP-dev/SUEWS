Anthropogenicemissions
======================

**Parameters:**

.. option:: startdls <RefValue[float]>

   Start of daylight savings time in decimal day of year

   :Unit: day
   :Default: ``0.0``

.. option:: enddls <RefValue[float]>

   End of daylight savings time in decimal day of year

   :Unit: day
   :Default: ``0.0``

.. option:: heat <AnthropogenicHeat>

   Anthropogenic heat emission parameters

   :Default: ``PydanticUndefined``

   The ``heat`` parameter group is defined by the :doc:`anthropogenicheat` structure.

.. option:: co2 <CO2Params>

   CO2 emission parameters

   :Default: ``PydanticUndefined``

   The ``co2`` parameter group is defined by the :doc:`co2params` structure.

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
