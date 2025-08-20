.. meta::
   :description: SUEWS YAML configuration for surface properties parameters
   :keywords: SUEWS, YAML, nonvegetatedsurfaceproperties, parameters, configuration

.. _nonvegetatedsurfaceproperties:

.. index::
   single: NonVegetatedSurfaceProperties (YAML parameter)
   single: YAML; NonVegetatedSurfaceProperties

Surface Properties
==================

**Parameters:**

.. index::
   single: sfr (YAML parameter)
   single: NonVegetatedSurfaceProperties; sfr

.. option:: sfr

   Surface fraction of grid area covered by this surface type

   :Unit: dimensionless
   :Sample value: ``0.14285714285714285``

.. index::
   single: emis (YAML parameter)
   single: NonVegetatedSurfaceProperties; emis

.. option:: emis

   Surface emissivity for longwave radiation

   :Unit: dimensionless
   :Sample value: ``0.95``

.. index::
   single: ohm_threshsw (YAML parameter)
   single: NonVegetatedSurfaceProperties; ohm_threshsw

.. option:: ohm_threshsw

   Summer/winter threshold based on temperature for OHM calculation

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: ohm_threshwd (YAML parameter)
   single: NonVegetatedSurfaceProperties; ohm_threshwd

.. option:: ohm_threshwd

   Soil moisture threshold determining whether wet/dry OHM coefficients are applied

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ohm_coef (YAML parameter)
   single: NonVegetatedSurfaceProperties; ohm_coef

.. option:: ohm_coef

   :Sample value: ``PydanticUndefined``

   The ``ohm_coef`` parameter group is defined by the :doc:`ohm_coefficient_season_wetness` structure.

.. index::
   single: soildepth (YAML parameter)
   single: NonVegetatedSurfaceProperties; soildepth

.. option:: soildepth

   Depth of soil layer for hydrological calculations

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: soilstorecap (YAML parameter)
   single: NonVegetatedSurfaceProperties; soilstorecap

.. option:: soilstorecap

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: statelimit (YAML parameter)
   single: NonVegetatedSurfaceProperties; statelimit

.. option:: statelimit

   Minimum water storage capacity for state change

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: wetthresh (YAML parameter)
   single: NonVegetatedSurfaceProperties; wetthresh

.. option:: wetthresh

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: sathydraulicconduct (YAML parameter)
   single: NonVegetatedSurfaceProperties; sathydraulicconduct

.. option:: sathydraulicconduct

   Saturated hydraulic conductivity of soil

   :Unit: mm |s^-1|
   :Default: Required - must be specified

.. index::
   single: waterdist (YAML parameter)
   single: NonVegetatedSurfaceProperties; waterdist

.. option:: waterdist

   Water distribution parameters

   :Default: Required - must be specified

   The ``waterdist`` parameter group is defined by the :doc:`waterdistribution` structure.

.. index::
   single: storedrainprm (YAML parameter)
   single: NonVegetatedSurfaceProperties; storedrainprm

.. option:: storedrainprm

   Storage and drain parameters

   :Sample value: ``PydanticUndefined``

   The ``storedrainprm`` parameter group is defined by the :doc:`storagedrainparams` structure.

.. index::
   single: snowpacklimit (YAML parameter)
   single: NonVegetatedSurfaceProperties; snowpacklimit

.. option:: snowpacklimit

   Limit of snow that can be held on surface

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: thermal_layers (YAML parameter)
   single: NonVegetatedSurfaceProperties; thermal_layers

.. option:: thermal_layers

   Thermal layers for the surface

   :Sample value: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. index::
   single: irrfrac (YAML parameter)
   single: NonVegetatedSurfaceProperties; irrfrac

.. option:: irrfrac

   Fraction of surface area that can be irrigated

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ref (YAML parameter)
   single: NonVegetatedSurfaceProperties; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.

.. index::
   single: alb (YAML parameter)
   single: NonVegetatedSurfaceProperties; alb

.. option:: alb

   Surface albedo

   :Unit: dimensionless
   :Sample value: ``0.1``
