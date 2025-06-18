Anthropogenicheat
=================

**Parameters:**

.. option:: qf0_beu <DayProfile>

   Base anthropogenic heat flux for buildings, equipment and urban metabolism

   :Default: ``PydanticUndefined``

   The ``qf0_beu`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: qf_a <DayProfile>

   Coefficient a for anthropogenic heat flux calculation

   :Default: ``PydanticUndefined``

   The ``qf_a`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: qf_b <DayProfile>

   Coefficient b for anthropogenic heat flux calculation

   :Default: ``PydanticUndefined``

   The ``qf_b`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: qf_c <DayProfile>

   Coefficient c for anthropogenic heat flux calculation

   :Default: ``PydanticUndefined``

   The ``qf_c`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: baset_cooling <DayProfile>

   Base temperature for cooling degree days

   :Default: ``PydanticUndefined``

   The ``baset_cooling`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: baset_heating <DayProfile>

   Base temperature for heating degree days

   :Default: ``PydanticUndefined``

   The ``baset_heating`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: ah_min <DayProfile>

   Minimum anthropogenic heat flux

   :Default: ``PydanticUndefined``

   The ``ah_min`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: ah_slope_cooling <DayProfile>

   Slope of anthropogenic heat vs cooling degree days

   :Default: ``PydanticUndefined``

   The ``ah_slope_cooling`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: ah_slope_heating <DayProfile>

   Slope of anthropogenic heat vs heating degree days

   :Default: ``PydanticUndefined``

   The ``ah_slope_heating`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: ahprof_24hr <HourlyProfile>

   24-hour profile of anthropogenic heat flux

   :Default: ``PydanticUndefined``

   The ``ahprof_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. option:: popdensdaytime <DayProfile>

   Daytime population density

   :Default: ``PydanticUndefined``

   The ``popdensdaytime`` parameter group is defined by the :doc:`dayprofile` structure.

.. option:: popdensnighttime <float>

   Nighttime population density

   :Unit: people ha^-1
   :Default: ``10.0``

.. option:: popprof_24hr <HourlyProfile>

   24-hour profile of population density

   :Default: ``PydanticUndefined``

   The ``popprof_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
