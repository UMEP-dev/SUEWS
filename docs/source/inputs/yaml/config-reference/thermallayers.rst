.. meta::
   :description: SUEWS YAML configuration for thermal layers parameters
   :keywords: SUEWS, YAML, thermallayers, parameters, configuration

.. _thermallayers:

.. index::
   single: ThermalLayers (YAML parameter)
   single: YAML; ThermalLayers

Thermal Layers
==============

Thermal properties of surface layers.

**Parameters:**

.. index::
   single: dz (YAML parameter)
   single: ThermalLayers; dz

.. option:: dz

   Thickness of thermal layers from surface to depth

   :Unit: m
   :Default: Required - must be specified

.. index::
   single: k (YAML parameter)
   single: ThermalLayers; k

.. option:: k

   Thermal conductivity of each thermal layer

   :Unit: W |m^-1| |K^-1|
   :Default: Required - must be specified

.. index::
   single: rho_cp (YAML parameter)
   single: ThermalLayers; rho_cp

.. option:: rho_cp

   Volumetric heat capacity of each thermal layer

   :Unit: J |m^-3| |K^-1|
   :Default: Required - must be specified

.. index::
   single: ref (YAML parameter)
   single: ThermalLayers; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
