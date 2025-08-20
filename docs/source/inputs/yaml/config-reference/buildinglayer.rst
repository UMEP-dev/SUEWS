.. meta::
   :description: SUEWS YAML configuration for building layer parameters
   :keywords: SUEWS, YAML, buildinglayer, parameters, configuration

.. _buildinglayer:

.. index::
   single: BuildingLayer (YAML parameter)
   single: YAML; BuildingLayer

Building Layer
==============

**Parameters:**

.. index::
   single: alb (YAML parameter)
   single: BuildingLayer; alb

.. option:: alb

   Surface albedo

   :Unit: dimensionless
   :Default: ``0.1``

.. index::
   single: emis (YAML parameter)
   single: BuildingLayer; emis

.. option:: emis

   Surface emissivity

   :Unit: dimensionless
   :Default: ``0.95``

.. index::
   single: thermal_layers (YAML parameter)
   single: BuildingLayer; thermal_layers

.. option:: thermal_layers

   Thermal layers for the surface

   :Default: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. index::
   single: statelimit (YAML parameter)
   single: BuildingLayer; statelimit

.. option:: statelimit

   Minimum water storage capacity for state change

   :Unit: mm
   :Default: ``10.0``

.. index::
   single: soilstorecap (YAML parameter)
   single: BuildingLayer; soilstorecap

.. option:: soilstorecap

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: wetthresh (YAML parameter)
   single: BuildingLayer; wetthresh

.. option:: wetthresh

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Default: ``0.5``

.. index::
   single: roof_albedo_dir_mult_fact (YAML parameter)
   single: BuildingLayer; roof_albedo_dir_mult_fact

.. option:: roof_albedo_dir_mult_fact

   Directional albedo multiplication factor for roofs

   :Unit: dimensionless
   :Sample value: ``0.1``

.. index::
   single: wall_specular_frac (YAML parameter)
   single: BuildingLayer; wall_specular_frac

.. option:: wall_specular_frac

   Specular reflection fraction for walls

   :Unit: dimensionless
   :Sample value: ``0.1``

.. index::
   single: ref (YAML parameter)
   single: BuildingLayer; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
