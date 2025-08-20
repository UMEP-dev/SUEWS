.. meta::
   :description: SUEWS YAML configuration for wall layer parameters
   :keywords: SUEWS, YAML, walllayer, parameters, configuration

.. _walllayer:

.. index::
   single: WallLayer (YAML parameter)
   single: YAML; WallLayer

Wall Layer
==========

**Parameters:**

.. index::
   single: alb (YAML parameter)
   single: WallLayer; alb

.. option:: alb

   Surface albedo

   :Unit: dimensionless
   :Default: ``0.1``

.. index::
   single: emis (YAML parameter)
   single: WallLayer; emis

.. option:: emis

   Surface emissivity

   :Unit: dimensionless
   :Default: ``0.95``

.. index::
   single: thermal_layers (YAML parameter)
   single: WallLayer; thermal_layers

.. option:: thermal_layers

   Thermal layers for the surface

   :Default: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. index::
   single: statelimit (YAML parameter)
   single: WallLayer; statelimit

.. option:: statelimit

   Minimum water storage capacity for state change

   :Unit: mm
   :Default: ``10.0``

.. index::
   single: soilstorecap (YAML parameter)
   single: WallLayer; soilstorecap

.. option:: soilstorecap

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: wetthresh (YAML parameter)
   single: WallLayer; wetthresh

.. option:: wetthresh

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Default: ``0.5``

.. index::
   single: roof_albedo_dir_mult_fact (YAML parameter)
   single: WallLayer; roof_albedo_dir_mult_fact

.. option:: roof_albedo_dir_mult_fact

   Directional albedo multiplication factor for roofs

   :Unit: dimensionless
   :Sample value: ``0.1``

.. index::
   single: wall_specular_frac (YAML parameter)
   single: WallLayer; wall_specular_frac

.. option:: wall_specular_frac

   Specular reflection fraction for walls

   :Unit: dimensionless
   :Sample value: ``0.1``

.. index::
   single: ref (YAML parameter)
   single: WallLayer; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
