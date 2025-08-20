.. meta::
   :description: SUEWS YAML configuration for lai parameters
   :keywords: SUEWS, YAML, laiparams, parameters, configuration

.. _laiparams:

.. index::
   single: LAIParams (YAML parameter)
   single: YAML; LAIParams

LAI
===

**Parameters:**

.. index::
   single: baset (YAML parameter)
   single: LAIParams; baset

.. option:: baset

   Base temperature for initiating growing degree days (GDD) for leaf growth

   :Unit: degC
   :Default: Required - must be specified

.. index::
   single: gddfull (YAML parameter)
   single: LAIParams; gddfull

.. option:: gddfull

   Growing degree days (GDD) needed for full capacity of LAI

   :Unit: degC*day
   :Default: Required - must be specified

.. index::
   single: basete (YAML parameter)
   single: LAIParams; basete

.. option:: basete

   Base temperature for initiating senescence degree days (SDD) for leaf off

   :Unit: degC
   :Default: Required - must be specified

.. index::
   single: sddfull (YAML parameter)
   single: LAIParams; sddfull

.. option:: sddfull

   Senescence degree days (SDD) needed to initiate leaf off

   :Unit: degC*day
   :Default: Required - must be specified

.. index::
   single: laimin (YAML parameter)
   single: LAIParams; laimin

.. option:: laimin

   Leaf-off wintertime LAI value

   :Unit: |m^2| |m^-2|
   :Sample value: ``0.1``

.. index::
   single: laimax (YAML parameter)
   single: LAIParams; laimax

.. option:: laimax

   Full leaf-on summertime LAI value

   :Unit: |m^2| |m^-2|
   :Default: Required - must be specified

.. index::
   single: laipower (YAML parameter)
   single: LAIParams; laipower

.. option:: laipower

   LAI calculation power parameters for growth and senescence

   :Sample value: ``PydanticUndefined``

   The ``laipower`` parameter group is defined by the :doc:`laipowercoefficients` structure.

.. index::
   single: laitype (YAML parameter)
   single: LAIParams; laitype

.. option:: laitype

   LAI calculation choice (0: original, 1: new high latitude)

   :Unit: dimensionless
   :Sample value: ``0``

.. index::
   single: ref (YAML parameter)
   single: LAIParams; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
