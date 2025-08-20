.. meta::
   :description: SUEWS YAML configuration for lumps parameters parameters
   :keywords: SUEWS, YAML, lumpsparams, parameters, configuration

.. _lumpsparams:

.. index::
   single: LUMPSParams (YAML parameter)
   single: YAML; LUMPSParams

LUMPS Parameters
================

LUMPS model parameters for surface moisture.

**Parameters:**

.. index::
   single: raincover (YAML parameter)
   single: LUMPSParams; raincover

.. option:: raincover

   Rain water coverage fraction

   :Unit: dimensionless
   :Default: ``0.25``

.. index::
   single: rainmaxres (YAML parameter)
   single: LUMPSParams; rainmaxres

.. option:: rainmaxres

   Maximum rain water storage

   :Unit: mm
   :Default: ``0.25``

.. index::
   single: drainrt (YAML parameter)
   single: LUMPSParams; drainrt

.. option:: drainrt

   Drainage rate coefficient

   :Unit: dimensionless
   :Default: ``0.25``

.. index::
   single: veg_type (YAML parameter)
   single: LUMPSParams; veg_type

.. option:: veg_type

   Vegetation type selection

   :Unit: dimensionless
   :Default: ``1``

.. index::
   single: ref (YAML parameter)
   single: LUMPSParams; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
