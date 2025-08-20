.. meta::
   :description: SUEWS YAML configuration for co2 emissions parameters
   :keywords: SUEWS, YAML, co2params, parameters, configuration

.. _co2params:

.. index::
   single: CO2Params (YAML parameter)
   single: YAML; CO2Params

CO2 Emissions
=============

CO2 emission parameters and profiles.

**Parameters:**

.. index::
   single: co2pointsource (YAML parameter)
   single: CO2Params; co2pointsource

.. option:: co2pointsource

   CO2 point source emission factor

   :Unit: kg |m^-2| |s^-1|
   :Default: Required - must be specified

.. index::
   single: ef_umolco2perj (YAML parameter)
   single: CO2Params; ef_umolco2perj

.. option:: ef_umolco2perj

   CO2 emission factor per unit of fuel energy

   :Unit: umol |J^-1|
   :Default: Required - must be specified

.. index::
   single: enef_v_jkm (YAML parameter)
   single: CO2Params; enef_v_jkm

.. option:: enef_v_jkm

   Vehicle energy consumption factor

   :Unit: J |km^-1|
   :Default: Required - must be specified

.. index::
   single: fcef_v_kgkm (YAML parameter)
   single: CO2Params; fcef_v_kgkm

.. option:: fcef_v_kgkm

   Fuel consumption efficiency for vehicles

   :Default: ``PydanticUndefined``

   The ``fcef_v_kgkm`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: frfossilfuel_heat (YAML parameter)
   single: CO2Params; frfossilfuel_heat

.. option:: frfossilfuel_heat

   Fraction of heating energy from fossil fuels

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: frfossilfuel_nonheat (YAML parameter)
   single: CO2Params; frfossilfuel_nonheat

.. option:: frfossilfuel_nonheat

   Fraction of non-heating energy from fossil fuels

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: maxfcmetab (YAML parameter)
   single: CO2Params; maxfcmetab

.. option:: maxfcmetab

   Maximum metabolic CO2 flux rate

   :Unit: umol |m^-2| |s^-1|
   :Default: Required - must be specified

.. index::
   single: maxqfmetab (YAML parameter)
   single: CO2Params; maxqfmetab

.. option:: maxqfmetab

   Maximum metabolic heat flux rate

   :Unit: W |m^-2|
   :Default: Required - must be specified

.. index::
   single: minfcmetab (YAML parameter)
   single: CO2Params; minfcmetab

.. option:: minfcmetab

   Minimum metabolic CO2 flux rate

   :Unit: umol |m^-2| |s^-1|
   :Default: Required - must be specified

.. index::
   single: minqfmetab (YAML parameter)
   single: CO2Params; minqfmetab

.. option:: minqfmetab

   Minimum metabolic heat flux rate

   :Unit: W |m^-2|
   :Default: Required - must be specified

.. index::
   single: trafficrate (YAML parameter)
   single: CO2Params; trafficrate

.. option:: trafficrate

   Traffic rate

   :Sample value: ``PydanticUndefined``

   The ``trafficrate`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: trafficunits (YAML parameter)
   single: CO2Params; trafficunits

.. option:: trafficunits

   Units for traffic density normalisation

   :Unit: vehicle km |ha^-1|
   :Default: Required - must be specified

.. index::
   single: traffprof_24hr (YAML parameter)
   single: CO2Params; traffprof_24hr

.. option:: traffprof_24hr

   24-hour profile of traffic rate

   :Default: ``PydanticUndefined``

   The ``traffprof_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. index::
   single: humactivity_24hr (YAML parameter)
   single: CO2Params; humactivity_24hr

.. option:: humactivity_24hr

   24-hour profile of human activity

   :Default: ``PydanticUndefined``

   The ``humactivity_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. index::
   single: ref (YAML parameter)
   single: CO2Params; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
