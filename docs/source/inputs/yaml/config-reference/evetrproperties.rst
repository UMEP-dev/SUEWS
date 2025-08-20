.. meta::
   :description: SUEWS YAML configuration for evergreen trees parameters
   :keywords: SUEWS, YAML, evetrproperties, parameters, configuration

.. _evetrproperties:

.. index::
   single: EvetrProperties (YAML parameter)
   single: YAML; EvetrProperties

Evergreen Trees
===============

Properties for evergreen trees and shrubs.

Evergreen vegetation maintains foliage year-round, providing consistent
evapotranspiration and shading. Common in urban parks and residential areas,
these surfaces have relatively low albedo and high roughness lengths.

**Parameters:**

.. index::
   single: sfr (YAML parameter)
   single: EvetrProperties; sfr

.. option:: sfr

   Surface fraction of grid area covered by this surface type

   :Unit: dimensionless
   :Sample value: ``0.14285714285714285``

.. index::
   single: emis (YAML parameter)
   single: EvetrProperties; emis

.. option:: emis

   Surface emissivity for longwave radiation

   :Unit: dimensionless
   :Sample value: ``0.95``

.. index::
   single: ohm_threshsw (YAML parameter)
   single: EvetrProperties; ohm_threshsw

.. option:: ohm_threshsw

   Summer/winter threshold based on temperature for OHM calculation

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: ohm_threshwd (YAML parameter)
   single: EvetrProperties; ohm_threshwd

.. option:: ohm_threshwd

   Soil moisture threshold determining whether wet/dry OHM coefficients are applied

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ohm_coef (YAML parameter)
   single: EvetrProperties; ohm_coef

.. option:: ohm_coef

   :Sample value: ``PydanticUndefined``

   The ``ohm_coef`` parameter group is defined by the :doc:`ohm_coefficient_season_wetness` structure.

.. index::
   single: soildepth (YAML parameter)
   single: EvetrProperties; soildepth

.. option:: soildepth

   Depth of soil layer for hydrological calculations

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: soilstorecap (YAML parameter)
   single: EvetrProperties; soilstorecap

.. option:: soilstorecap

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: statelimit (YAML parameter)
   single: EvetrProperties; statelimit

.. option:: statelimit

   Minimum water storage capacity for state change

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: wetthresh (YAML parameter)
   single: EvetrProperties; wetthresh

.. option:: wetthresh

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: sathydraulicconduct (YAML parameter)
   single: EvetrProperties; sathydraulicconduct

.. option:: sathydraulicconduct

   Saturated hydraulic conductivity of soil

   :Unit: mm |s^-1|
   :Default: Required - must be specified

.. index::
   single: waterdist (YAML parameter)
   single: EvetrProperties; waterdist

.. option:: waterdist

   Water distribution for evergreen trees

   :Sample value: ``PydanticUndefined``

   The ``waterdist`` parameter group is defined by the :doc:`waterdistribution` structure.

.. index::
   single: storedrainprm (YAML parameter)
   single: EvetrProperties; storedrainprm

.. option:: storedrainprm

   Storage and drain parameters

   :Sample value: ``PydanticUndefined``

   The ``storedrainprm`` parameter group is defined by the :doc:`storagedrainparams` structure.

.. index::
   single: snowpacklimit (YAML parameter)
   single: EvetrProperties; snowpacklimit

.. option:: snowpacklimit

   Limit of snow that can be held on surface

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: thermal_layers (YAML parameter)
   single: EvetrProperties; thermal_layers

.. option:: thermal_layers

   Thermal layers for the surface

   :Sample value: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. index::
   single: irrfrac (YAML parameter)
   single: EvetrProperties; irrfrac

.. option:: irrfrac

   Fraction of surface area that can be irrigated

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ref (YAML parameter)
   single: EvetrProperties; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.

.. index::
   single: alb (YAML parameter)
   single: EvetrProperties; alb

.. option:: alb

   Albedo

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: alb_min (YAML parameter)
   single: EvetrProperties; alb_min

.. option:: alb_min

   Minimum albedo

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: alb_max (YAML parameter)
   single: EvetrProperties; alb_max

.. option:: alb_max

   Maximum albedo

   :Unit: dimensionless
   :Sample value: ``0.3``

.. index::
   single: beta_bioco2 (YAML parameter)
   single: EvetrProperties; beta_bioco2

.. option:: beta_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: beta_enh_bioco2 (YAML parameter)
   single: EvetrProperties; beta_enh_bioco2

.. option:: beta_enh_bioco2

   Enhanced biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Sample value: ``0.7``

.. index::
   single: alpha_bioco2 (YAML parameter)
   single: EvetrProperties; alpha_bioco2

.. option:: alpha_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: alpha_enh_bioco2 (YAML parameter)
   single: EvetrProperties; alpha_enh_bioco2

.. option:: alpha_enh_bioco2

   Enhanced biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Sample value: ``0.9``

.. index::
   single: resp_a (YAML parameter)
   single: EvetrProperties; resp_a

.. option:: resp_a

   Respiration coefficient

   :Unit: umol |m^-2| |s^-1|
   :Default: Required - must be specified

.. index::
   single: resp_b (YAML parameter)
   single: EvetrProperties; resp_b

.. option:: resp_b

   Respiration coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: theta_bioco2 (YAML parameter)
   single: EvetrProperties; theta_bioco2

.. option:: theta_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: maxconductance (YAML parameter)
   single: EvetrProperties; maxconductance

.. option:: maxconductance

   Maximum surface conductance

   :Unit: mm |s^-1|
   :Sample value: ``0.5``

.. index::
   single: min_res_bioco2 (YAML parameter)
   single: EvetrProperties; min_res_bioco2

.. option:: min_res_bioco2

   Minimum respiratory biogenic CO2

   :Unit: umol |m^-2| |s^-1|
   :Sample value: ``0.1``

.. index::
   single: lai (YAML parameter)
   single: EvetrProperties; lai

.. option:: lai

   Leaf area index parameters

   :Sample value: ``PydanticUndefined``

   The ``lai`` parameter group is defined by the :doc:`laiparams` structure.

.. index::
   single: ie_a (YAML parameter)
   single: EvetrProperties; ie_a

.. option:: ie_a

   Irrigation efficiency coefficient-automatic

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: ie_m (YAML parameter)
   single: EvetrProperties; ie_m

.. option:: ie_m

   Irrigation efficiency coefficient-manual

   :Unit: dimensionless
   :Sample value: ``0.6``

.. index::
   single: faievetree (YAML parameter)
   single: EvetrProperties; faievetree

.. option:: faievetree

   Frontal area index of evergreen trees

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: evetreeh (YAML parameter)
   single: EvetrProperties; evetreeh

.. option:: evetreeh

   Evergreen tree height

   :Unit: m
   :Default: Required - must be specified
