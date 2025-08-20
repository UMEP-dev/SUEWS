.. meta::
   :description: SUEWS YAML configuration for deciduous trees parameters
   :keywords: SUEWS, YAML, dectrproperties, parameters, configuration

.. _dectrproperties:

.. index::
   single: DectrProperties (YAML parameter)
   single: YAML; DectrProperties

Deciduous Trees
===============

Properties for deciduous trees and shrubs.

Deciduous vegetation undergoes seasonal changes with leaf growth and fall,
significantly affecting surface energy balance throughout the year. These
surfaces provide seasonal shading and have variable evapotranspiration rates.

**Parameters:**

.. index::
   single: sfr (YAML parameter)
   single: DectrProperties; sfr

.. option:: sfr

   Surface fraction of grid area covered by this surface type

   :Unit: dimensionless
   :Sample value: ``0.14285714285714285``

.. index::
   single: emis (YAML parameter)
   single: DectrProperties; emis

.. option:: emis

   Surface emissivity for longwave radiation

   :Unit: dimensionless
   :Sample value: ``0.95``

.. index::
   single: ohm_threshsw (YAML parameter)
   single: DectrProperties; ohm_threshsw

.. option:: ohm_threshsw

   Summer/winter threshold based on temperature for OHM calculation

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: ohm_threshwd (YAML parameter)
   single: DectrProperties; ohm_threshwd

.. option:: ohm_threshwd

   Soil moisture threshold determining whether wet/dry OHM coefficients are applied

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ohm_coef (YAML parameter)
   single: DectrProperties; ohm_coef

.. option:: ohm_coef

   :Sample value: ``PydanticUndefined``

   The ``ohm_coef`` parameter group is defined by the :doc:`ohm_coefficient_season_wetness` structure.

.. index::
   single: soildepth (YAML parameter)
   single: DectrProperties; soildepth

.. option:: soildepth

   Depth of soil layer for hydrological calculations

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: soilstorecap (YAML parameter)
   single: DectrProperties; soilstorecap

.. option:: soilstorecap

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: statelimit (YAML parameter)
   single: DectrProperties; statelimit

.. option:: statelimit

   Minimum water storage capacity for state change

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: wetthresh (YAML parameter)
   single: DectrProperties; wetthresh

.. option:: wetthresh

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: sathydraulicconduct (YAML parameter)
   single: DectrProperties; sathydraulicconduct

.. option:: sathydraulicconduct

   Saturated hydraulic conductivity of soil

   :Unit: mm |s^-1|
   :Default: Required - must be specified

.. index::
   single: waterdist (YAML parameter)
   single: DectrProperties; waterdist

.. option:: waterdist

   Water distribution for deciduous trees

   :Sample value: ``PydanticUndefined``

   The ``waterdist`` parameter group is defined by the :doc:`waterdistribution` structure.

.. index::
   single: storedrainprm (YAML parameter)
   single: DectrProperties; storedrainprm

.. option:: storedrainprm

   Storage and drain parameters

   :Sample value: ``PydanticUndefined``

   The ``storedrainprm`` parameter group is defined by the :doc:`storagedrainparams` structure.

.. index::
   single: snowpacklimit (YAML parameter)
   single: DectrProperties; snowpacklimit

.. option:: snowpacklimit

   Limit of snow that can be held on surface

   :Unit: mm
   :Sample value: ``10.0``

.. index::
   single: thermal_layers (YAML parameter)
   single: DectrProperties; thermal_layers

.. option:: thermal_layers

   Thermal layers for the surface

   :Sample value: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. index::
   single: irrfrac (YAML parameter)
   single: DectrProperties; irrfrac

.. option:: irrfrac

   Fraction of surface area that can be irrigated

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ref (YAML parameter)
   single: DectrProperties; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.

.. index::
   single: alb (YAML parameter)
   single: DectrProperties; alb

.. option:: alb

   Albedo

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: alb_min (YAML parameter)
   single: DectrProperties; alb_min

.. option:: alb_min

   Minimum albedo

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: alb_max (YAML parameter)
   single: DectrProperties; alb_max

.. option:: alb_max

   Maximum albedo

   :Unit: dimensionless
   :Sample value: ``0.3``

.. index::
   single: beta_bioco2 (YAML parameter)
   single: DectrProperties; beta_bioco2

.. option:: beta_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: beta_enh_bioco2 (YAML parameter)
   single: DectrProperties; beta_enh_bioco2

.. option:: beta_enh_bioco2

   Enhanced biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Sample value: ``0.7``

.. index::
   single: alpha_bioco2 (YAML parameter)
   single: DectrProperties; alpha_bioco2

.. option:: alpha_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: alpha_enh_bioco2 (YAML parameter)
   single: DectrProperties; alpha_enh_bioco2

.. option:: alpha_enh_bioco2

   Enhanced biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Sample value: ``0.9``

.. index::
   single: resp_a (YAML parameter)
   single: DectrProperties; resp_a

.. option:: resp_a

   Respiration coefficient

   :Unit: umol |m^-2| |s^-1|
   :Default: Required - must be specified

.. index::
   single: resp_b (YAML parameter)
   single: DectrProperties; resp_b

.. option:: resp_b

   Respiration coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: theta_bioco2 (YAML parameter)
   single: DectrProperties; theta_bioco2

.. option:: theta_bioco2

   Biogenic CO2 exchange coefficient

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: maxconductance (YAML parameter)
   single: DectrProperties; maxconductance

.. option:: maxconductance

   Maximum surface conductance

   :Unit: mm |s^-1|
   :Sample value: ``0.5``

.. index::
   single: min_res_bioco2 (YAML parameter)
   single: DectrProperties; min_res_bioco2

.. option:: min_res_bioco2

   Minimum respiratory biogenic CO2

   :Unit: umol |m^-2| |s^-1|
   :Sample value: ``0.1``

.. index::
   single: lai (YAML parameter)
   single: DectrProperties; lai

.. option:: lai

   Leaf area index parameters

   :Sample value: ``PydanticUndefined``

   The ``lai`` parameter group is defined by the :doc:`laiparams` structure.

.. index::
   single: ie_a (YAML parameter)
   single: DectrProperties; ie_a

.. option:: ie_a

   Irrigation efficiency coefficient-automatic

   :Unit: dimensionless
   :Sample value: ``0.5``

.. index::
   single: ie_m (YAML parameter)
   single: DectrProperties; ie_m

.. option:: ie_m

   Irrigation efficiency coefficient-manual

   :Unit: dimensionless
   :Sample value: ``0.6``

.. index::
   single: faidectree (YAML parameter)
   single: DectrProperties; faidectree

.. option:: faidectree

   Frontal area index of deciduous trees

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: dectreeh (YAML parameter)
   single: DectrProperties; dectreeh

.. option:: dectreeh

   Deciduous tree height

   :Unit: m
   :Default: Required - must be specified

.. index::
   single: pormin_dec (YAML parameter)
   single: DectrProperties; pormin_dec

.. option:: pormin_dec

   Minimum porosity

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: pormax_dec (YAML parameter)
   single: DectrProperties; pormax_dec

.. option:: pormax_dec

   Maximum porosity

   :Unit: dimensionless
   :Sample value: ``0.6``

.. index::
   single: capmax_dec (YAML parameter)
   single: DectrProperties; capmax_dec

.. option:: capmax_dec

   Maximum water capacity

   :Unit: mm
   :Sample value: ``100.0``

.. index::
   single: capmin_dec (YAML parameter)
   single: DectrProperties; capmin_dec

.. option:: capmin_dec

   Minimum water capacity

   :Unit: mm
   :Sample value: ``10.0``
