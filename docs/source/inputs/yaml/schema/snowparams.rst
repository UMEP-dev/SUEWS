Snowparams
==========

**Parameters:**

.. option:: crwmax <RefValue[float]>

   Maximum water holding capacity of snow

   :Unit: mm
   :Default: ``0.1``

.. option:: crwmin <RefValue[float]>

   Minimum water holding capacity of snow

   :Unit: mm
   :Default: ``0.05``

.. option:: narp_emis_snow <RefValue[float]>

   Snow surface emissivity

   :Unit: dimensionless
   :Default: ``0.99``

.. option:: preciplimit <RefValue[float]>

   Temperature threshold for snow vs rain precipitation

   :Unit: degC
   :Default: ``2.2``

.. option:: preciplimitalb <RefValue[float]>

   Precipitation threshold for snow albedo aging

   :Unit: mm
   :Default: ``0.1``

.. option:: snowalbmax <RefValue[float]>

   Maximum snow albedo

   :Unit: dimensionless
   :Default: ``0.85``

.. option:: snowalbmin <RefValue[float]>

   Minimum snow albedo

   :Unit: dimensionless
   :Default: ``0.4``

.. option:: snowdensmin <RefValue[float]>

   Minimum snow density

   :Unit: kg m^-3
   :Default: ``100.0``

.. option:: snowdensmax <RefValue[float]>

   Maximum snow density

   :Unit: kg m^-3
   :Default: ``400.0``

.. option:: snowlimbldg <RefValue>

   Maximum snow depth limit on buildings
   In YAML, this is typically specified using a ``value`` key, e.g.: ``snowlimbldg: {value: ...}``.

   :Unit: m
   :Default: ``0.1``

   The ``snowlimbldg`` parameter group is defined by the :doc:`refvalue` structure.

.. option:: snowlimpaved <RefValue>

   Maximum snow depth limit on paved surfaces
   In YAML, this is typically specified using a ``value`` key, e.g.: ``snowlimpaved: {value: ...}``.

   :Unit: m
   :Default: ``0.1``

   The ``snowlimpaved`` parameter group is defined by the :doc:`refvalue` structure.

.. option:: snowprof_24hr <HourlyProfile>

   24-hour snow profile

   :Default: ``PydanticUndefined``

   The ``snowprof_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. option:: tau_a <RefValue[float]>

   Time constant for snow albedo aging in cold snow

   :Unit: dimensionless
   :Default: ``0.018``

.. option:: tau_f <RefValue[float]>

   Time constant for snow albedo aging in melting snow

   :Unit: dimensionless
   :Default: ``0.11``

.. option:: tau_r <RefValue[float]>

   Time constant for snow albedo aging in refreezing snow

   :Unit: dimensionless
   :Default: ``0.05``

.. option:: tempmeltfact <RefValue[float]>

   Hourly temperature melt factor of snow

   :Unit: mm K^-1 h^-1
   :Default: ``0.12``

.. option:: radmeltfact <RefValue[float]>

   Hourly radiation melt factor of snow

   :Unit: mm W^-1 m^2 h^-1
   :Default: ``0.0016``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
