Co2Params
=========

**Parameters:**

.. option:: co2pointsource <RefValue[float]>

   CO2 point source emission factor

   :Unit: kg m^-2 s^-1
   :Default: ``0.0``

.. option:: ef_umolco2perj <RefValue[float]>

   CO2 emission factor per unit of fuel energy

   :Unit: umol J^-1
   :Default: ``0.0``

.. option:: enef_v_jkm <RefValue[float]>

   Vehicle energy consumption factor

   :Unit: J km^-1
   :Default: ``0.0``

.. option:: fcef_v_kgkm <DayProfile>

   Fuel consumption efficiency for vehicles

   :Default: ``PydanticUndefined``

   The ``fcef_v_kgkm`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: frfossilfuel_heat <RefValue[float]>

   Fraction of heating energy from fossil fuels

   :Unit: dimensionless
   :Default: ``0.0``

.. option:: frfossilfuel_nonheat <RefValue[float]>

   Fraction of non-heating energy from fossil fuels

   :Unit: dimensionless
   :Default: ``0.0``

.. option:: maxfcmetab <RefValue[float]>

   Maximum metabolic CO2 flux rate

   :Unit: umol m^-2 s^-1
   :Default: ``0.0``

.. option:: maxqfmetab <RefValue[float]>

   Maximum metabolic heat flux rate

   :Unit: W m^-2
   :Default: ``0.0``

.. option:: minfcmetab <RefValue[float]>

   Minimum metabolic CO2 flux rate

   :Unit: umol m^-2 s^-1
   :Default: ``0.0``

.. option:: minqfmetab <RefValue[float]>

   Minimum metabolic heat flux rate

   :Unit: W m^-2
   :Default: ``0.0``

.. option:: trafficrate <DayProfile>

   Traffic rate

   :Default: ``PydanticUndefined``

   The ``trafficrate`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: trafficunits <RefValue[float]>

   Units for traffic density normalization

   :Unit: vehicle km ha^-1
   :Default: ``0.0``

.. option:: traffprof_24hr <HourlyProfile>

   24-hour profile of traffic rate

   :Default: ``PydanticUndefined``

   The ``traffprof_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. option:: humactivity_24hr <HourlyProfile>

   24-hour profile of human activity

   :Default: ``PydanticUndefined``

   The ``humactivity_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
