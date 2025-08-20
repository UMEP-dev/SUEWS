.. meta::
   :description: SUEWS YAML configuration for water surfaces parameters
   :keywords: SUEWS, YAML, waterproperties, parameters, configuration

.. _waterproperties:

.. index::
   single: WaterProperties (YAML parameter)
   single: YAML; WaterProperties

Water Surfaces
==============

Properties for water surfaces including rivers, lakes, and fountains.

Water surfaces have unique thermal properties with high heat capacity
and evaporative cooling effects. They moderate local temperatures but
have very low albedo values.

**Parameters:**

.. index::
   single: sfr (YAML parameter)
   single: WaterProperties; sfr

.. option:: sfr

   Surface fraction of grid area covered by this surface type

   :Unit: dimensionless
   :Sample value: ``0.14285714285714285``

.. index::
   single: emis (YAML parameter)
   single: WaterProperties; emis

.. option:: emis

   Surface emissivity for longwave radiation

   :Unit: dimensionless
   :Sample value: ``0.95``

.. index::
   single: ohm_threshsw (YAML parameter)
   single: WaterProperties; ohm_threshsw

.. option:: ohm_threshsw

   Summer/winter threshold based on temperature for OHM calculation

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: ohm_threshwd (YAML parameter)
   single: WaterProperties; ohm_threshwd

.. option:: ohm_threshwd

   Soil moisture threshold determining whether wet/dry OHM coefficients are applied

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ohm_coef (YAML parameter)
   single: WaterProperties; ohm_coef

.. option:: ohm_coef

   :Sample value: ``PydanticUndefined``

   The ``ohm_coef`` parameter group is defined by the :doc:`ohm_coefficient_season_wetness` structure.

.. index::
   single: soildepth (YAML parameter)
   single: WaterProperties; soildepth

.. option:: soildepth

   Depth of soil layer for hydrological calculations

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: soilstorecap (YAML parameter)
   single: WaterProperties; soilstorecap

.. option:: soilstorecap

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: statelimit (YAML parameter)
   single: WaterProperties; statelimit

.. option:: statelimit

   Minimum water storage capacity for state change

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: wetthresh (YAML parameter)
   single: WaterProperties; wetthresh

.. option:: wetthresh

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: sathydraulicconduct (YAML parameter)
   single: WaterProperties; sathydraulicconduct

.. option:: sathydraulicconduct

   Saturated hydraulic conductivity of soil

   :Unit: mm |s^-1|
   :Default: Required - must be specified

.. index::
   single: waterdist (YAML parameter)
   single: WaterProperties; waterdist

.. option:: waterdist

   Water distribution parameters

   :Default: Required - must be specified

   The ``waterdist`` parameter group is defined by the :doc:`waterdistribution` structure.

.. index::
   single: storedrainprm (YAML parameter)
   single: WaterProperties; storedrainprm

.. option:: storedrainprm

   Storage and drain parameters

   :Sample value: ``PydanticUndefined``

   The ``storedrainprm`` parameter group is defined by the :doc:`storagedrainparams` structure.

.. index::
   single: snowpacklimit (YAML parameter)
   single: WaterProperties; snowpacklimit

.. option:: snowpacklimit

   Limit of snow that can be held on surface

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: thermal_layers (YAML parameter)
   single: WaterProperties; thermal_layers

.. option:: thermal_layers

   Thermal layers for the surface

   :Sample value: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. index::
   single: irrfrac (YAML parameter)
   single: WaterProperties; irrfrac

.. option:: irrfrac

   Fraction of surface area that can be irrigated

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ref (YAML parameter)
   single: WaterProperties; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.

.. index::
   single: alb (YAML parameter)
   single: WaterProperties; alb

.. option:: alb

   Surface albedo

   :Unit: dimensionless
   :Sample value: ``0.1``

.. index::
   single: flowchange (YAML parameter)
   single: WaterProperties; flowchange

.. option:: flowchange

   Change in water flow for water bodies

   :Unit: mm |h^-1|
   :Sample value: ``0.0``
