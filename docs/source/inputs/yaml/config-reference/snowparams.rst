.. meta::
   :description: SUEWS YAML configuration for snow parameters
   :keywords: SUEWS, YAML, snowparams, parameters, configuration

.. _snowparams:

.. index::
   single: SnowParams (YAML parameter)
   single: YAML; SnowParams

Snow
====

**Parameters:**

.. index::
   single: crwmax (YAML parameter)
   single: SnowParams; crwmax

.. option:: crwmax

   Maximum water holding capacity of snow

   :Unit: mm
   :Default: ``0.1``

.. index::
   single: crwmin (YAML parameter)
   single: SnowParams; crwmin

.. option:: crwmin

   Minimum water holding capacity of snow

   :Unit: mm
   :Default: ``0.05``

.. index::
   single: narp_emis_snow (YAML parameter)
   single: SnowParams; narp_emis_snow

.. option:: narp_emis_snow

   Snow surface emissivity

   :Unit: dimensionless
   :Default: ``0.99``

.. index::
   single: preciplimit (YAML parameter)
   single: SnowParams; preciplimit

.. option:: preciplimit

   Temperature threshold for snow vs rain precipitation

   :Unit: degC
   :Default: Required - must be specified

.. index::
   single: preciplimitalb (YAML parameter)
   single: SnowParams; preciplimitalb

.. option:: preciplimitalb

   Precipitation threshold for snow albedo aging

   :Unit: mm
   :Default: ``0.1``

.. index::
   single: snowalbmax (YAML parameter)
   single: SnowParams; snowalbmax

.. option:: snowalbmax

   Maximum snow albedo

   :Unit: dimensionless
   :Default: ``0.85``

.. index::
   single: snowalbmin (YAML parameter)
   single: SnowParams; snowalbmin

.. option:: snowalbmin

   Minimum snow albedo

   :Unit: dimensionless
   :Default: ``0.4``

.. index::
   single: snowdensmin (YAML parameter)
   single: SnowParams; snowdensmin

.. option:: snowdensmin

   Minimum snow density

   :Unit: kg |m^-3|
   :Default: Required - must be specified

.. index::
   single: snowdensmax (YAML parameter)
   single: SnowParams; snowdensmax

.. option:: snowdensmax

   Maximum snow density

   :Unit: kg |m^-3|
   :Default: Required - must be specified

.. index::
   single: snowlimbldg (YAML parameter)
   single: SnowParams; snowlimbldg

.. option:: snowlimbldg

   Maximum snow depth limit on buildings

   :Unit: m
   :Default: ``0.1``

.. index::
   single: snowlimpaved (YAML parameter)
   single: SnowParams; snowlimpaved

.. option:: snowlimpaved

   Maximum snow depth limit on paved surfaces

   :Unit: m
   :Default: ``0.1``

.. index::
   single: snowprof_24hr (YAML parameter)
   single: SnowParams; snowprof_24hr

.. option:: snowprof_24hr

   24-hour snow profile

   :Default: ``PydanticUndefined``

   The ``snowprof_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. index::
   single: tau_a (YAML parameter)
   single: SnowParams; tau_a

.. option:: tau_a

   Time constant for snow albedo aging in cold snow

   :Unit: dimensionless
   :Default: ``0.018``

.. index::
   single: tau_f (YAML parameter)
   single: SnowParams; tau_f

.. option:: tau_f

   Time constant for snow albedo aging in melting snow

   :Unit: dimensionless
   :Default: ``0.11``

.. index::
   single: tau_r (YAML parameter)
   single: SnowParams; tau_r

.. option:: tau_r

   Time constant for snow albedo aging in refreezing snow

   :Unit: dimensionless
   :Default: ``0.05``

.. index::
   single: tempmeltfact (YAML parameter)
   single: SnowParams; tempmeltfact

.. option:: tempmeltfact

   Hourly temperature melt factor of snow

   :Unit: mm |K^-1| |h^-1|
   :Default: ``0.12``

.. index::
   single: radmeltfact (YAML parameter)
   single: SnowParams; radmeltfact

.. option:: radmeltfact

   Hourly radiation melt factor of snow

   :Unit: mm |W^-1| |m^2| |h^-1|
   :Default: ``0.0016``

.. index::
   single: ref (YAML parameter)
   single: SnowParams; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
