Irrigationparams
================

**Parameters:**

.. option:: h_maintain <RefValue[float]>

   Water depth to maintain through irrigation

   :Unit: mm
   :Default: ``0.5``

.. option:: faut <RefValue[float]>

   Fraction of automatic irrigation

   :Unit: dimensionless
   :Default: ``0.0``

.. option:: ie_start <RefValue[float]>

   Start time of irrigation

   :Unit: hour
   :Default: ``0.0``

.. option:: ie_end <RefValue[float]>

   End time of irrigation

   :Unit: hour
   :Default: ``0.0``

.. option:: internalwateruse_h <RefValue[float]>

   Internal water use rate

   :Unit: mm h^-1
   :Default: ``0.0``

.. option:: daywatper <WeeklyProfile>

   :Default: ``PydanticUndefined``

   The ``daywatper`` parameter group is defined by the :doc:`weeklyprofile` structure.

.. option:: daywat <WeeklyProfile>

   :Default: ``PydanticUndefined``

   The ``daywat`` parameter group is defined by the :doc:`weeklyprofile` structure.

.. option:: wuprofa_24hr <HourlyProfile>

   :Default: ``PydanticUndefined``

   The ``wuprofa_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. option:: wuprofm_24hr <HourlyProfile>

   :Default: ``PydanticUndefined``

   The ``wuprofm_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
