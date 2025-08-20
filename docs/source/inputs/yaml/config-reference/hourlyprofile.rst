.. meta::
   :description: SUEWS YAML configuration for hourly profile parameters
   :keywords: SUEWS, YAML, hourlyprofile, parameters, configuration

.. _hourlyprofile:

.. index::
   single: HourlyProfile (YAML parameter)
   single: YAML; HourlyProfile

Hourly Profile
==============

24-hour profile for diurnal variations.

**Parameters:**

.. index::
   single: working_day (YAML parameter)
   single: HourlyProfile; working_day

.. option:: working_day

   :Default: ``PydanticUndefined``

.. index::
   single: holiday (YAML parameter)
   single: HourlyProfile; holiday

.. option:: holiday

   :Default: ``PydanticUndefined``

.. index::
   single: ref (YAML parameter)
   single: HourlyProfile; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
