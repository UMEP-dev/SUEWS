.. meta::
   :description: SUEWS YAML configuration for surface properties parameters
   :keywords: SUEWS, YAML, vegetatedsurfaceproperties, parameters, configuration

.. _vegetatedsurfaceproperties:

.. index::
   single: VegetatedSurfaceProperties (YAML parameter)
   single: YAML; VegetatedSurfaceProperties

Surface Properties
==================

**Parameters:**

.. index::
   single: sfr (YAML parameter)
   single: VegetatedSurfaceProperties; sfr

.. option:: sfr

   Surface fraction of grid area covered by this surface type

   :Unit: dimensionless
   :Sample value: ``0.14285714285714285``

.. index::
   single: emis (YAML parameter)
   single: VegetatedSurfaceProperties; emis

.. option:: emis

   Surface emissivity for longwave radiation

   :Unit: dimensionless
   :Sample value: ``0.95``

.. index::
   single: ohm_threshsw (YAML parameter)
   single: VegetatedSurfaceProperties; ohm_threshsw

.. option:: ohm_threshsw

   Summer/winter threshold based on temperature for OHM calculation

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: ohm_threshwd (YAML parameter)
   single: VegetatedSurfaceProperties; ohm_threshwd

.. option:: ohm_threshwd

   Soil moisture threshold determining whether wet/dry OHM coefficients are applied

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ohm_coef (YAML parameter)
   single: VegetatedSurfaceProperties; ohm_coef

.. option:: ohm_coef

   :Sample value: ``PydanticUndefined``

   The ``ohm_coef`` parameter group is defined by the :doc:`ohm_coefficient_season_wetness` structure.

.. index::
   single: soildepth (YAML parameter)
   single: VegetatedSurfaceProperties; soildepth

.. option:: soildepth

   Depth of soil layer for hydrological calculations

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: soilstorecap (YAML parameter)
   single: VegetatedSurfaceProperties; soilstorecap

.. option:: soilstorecap

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: statelimit (YAML parameter)
   single: VegetatedSurfaceProperties; statelimit

.. option:: statelimit

   Minimum water storage capacity for state change

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: wetthresh (YAML parameter)
   single: VegetatedSurfaceProperties; wetthresh

.. option:: wetthresh

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: sathydraulicconduct (YAML parameter)
   single: VegetatedSurfaceProperties; sathydraulicconduct

.. option:: sathydraulicconduct

   Saturated hydraulic conductivity of soil

   :Unit: mm |s^-1|
   :Default: Required - must be specified

.. index::
   single: waterdist (YAML parameter)
   single: VegetatedSurfaceProperties; waterdist

.. option:: waterdist

   Water distribution parameters

   :Default: Required - must be specified

   The ``waterdist`` parameter group is defined by the :doc:`waterdistribution` structure.

.. index::
   single: storedrainprm (YAML parameter)
   single: VegetatedSurfaceProperties; storedrainprm

.. option:: storedrainprm

   Storage and drain parameters

   :Sample value: ``PydanticUndefined``

   The ``storedrainprm`` parameter group is defined by the :doc:`storagedrainparams` structure.

.. index::
   single: snowpacklimit (YAML parameter)
   single: VegetatedSurfaceProperties; snowpacklimit

.. option:: snowpacklimit

   Limit of snow that can be held on surface

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: thermal_layers (YAML parameter)
   single: VegetatedSurfaceProperties; thermal_layers

.. option:: thermal_layers

   Thermal layers for the surface

   :Sample value: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. index::
   single: irrfrac (YAML parameter)
   single: VegetatedSurfaceProperties; irrfrac

.. option:: irrfrac

   Fraction of surface area that can be irrigated

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ref (YAML parameter)
   single: VegetatedSurfaceProperties; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.

.. index::
   single: alb (YAML parameter)
   single: VegetatedSurfaceProperties; alb

.. option:: alb

   Albedo

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: alb_min (YAML parameter)
   single: VegetatedSurfaceProperties; alb_min

.. option:: alb_min

   Minimum albedo

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: alb_max (YAML parameter)
   single: VegetatedSurfaceProperties; alb_max

.. option:: alb_max

   Maximum albedo

   :Unit: dimensionless
   :Sample value: ``0.3``

.. index::
   single: beta_bioco2 (YAML parameter)
   single: VegetatedSurfaceProperties; beta_bioco2

.. option:: beta_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: beta_enh_bioco2 (YAML parameter)
   single: VegetatedSurfaceProperties; beta_enh_bioco2

.. option:: beta_enh_bioco2

   Enhanced biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Sample value: ``0.7``

.. index::
   single: alpha_bioco2 (YAML parameter)
   single: VegetatedSurfaceProperties; alpha_bioco2

.. option:: alpha_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: alpha_enh_bioco2 (YAML parameter)
   single: VegetatedSurfaceProperties; alpha_enh_bioco2

.. option:: alpha_enh_bioco2

   Enhanced biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Sample value: ``0.9``

.. index::
   single: resp_a (YAML parameter)
   single: VegetatedSurfaceProperties; resp_a

.. option:: resp_a

   Respiration coefficient

   :Unit: umol |m^-2| |s^-1|
   :Default: Required - must be specified

.. index::
   single: resp_b (YAML parameter)
   single: VegetatedSurfaceProperties; resp_b

.. option:: resp_b

   Respiration coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: theta_bioco2 (YAML parameter)
   single: VegetatedSurfaceProperties; theta_bioco2

.. option:: theta_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: maxconductance (YAML parameter)
   single: VegetatedSurfaceProperties; maxconductance

.. option:: maxconductance

   Maximum surface conductance

   :Unit: mm |s^-1|
   :Sample value: ``0.5``

.. index::
   single: min_res_bioco2 (YAML parameter)
   single: VegetatedSurfaceProperties; min_res_bioco2

.. option:: min_res_bioco2

   Minimum respiratory biogenic CO2

   :Unit: umol |m^-2| |s^-1|
   :Sample value: ``0.1``

.. index::
   single: lai (YAML parameter)
   single: VegetatedSurfaceProperties; lai

.. option:: lai

   Leaf area index parameters

   :Sample value: ``PydanticUndefined``

   The ``lai`` parameter group is defined by the :doc:`laiparams` structure.

.. index::
   single: ie_a (YAML parameter)
   single: VegetatedSurfaceProperties; ie_a

.. option:: ie_a

   Irrigation efficiency coefficient-automatic

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: ie_m (YAML parameter)
   single: VegetatedSurfaceProperties; ie_m

.. option:: ie_m

   Irrigation efficiency coefficient-manual

   :Unit: dimensionless
   :Sample value: ``0.6``
