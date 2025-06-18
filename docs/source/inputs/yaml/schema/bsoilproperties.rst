Bsoilproperties
===============

**Parameters:**

.. option:: sfr <RefValue[float]>

   Surface fraction of grid area covered by this surface type

   :Unit: dimensionless
   :Default: ``0.14285714285714285``

.. option:: emis <RefValue[float]>

   Surface emissivity for longwave radiation

   :Unit: dimensionless
   :Default: ``0.95``

.. option:: ch_anohm <RefValue[float] (Optional)>

   Bulk transfer coefficient for this surface. Option: AnOHM

   :Unit: J m^-3 K^-1
   :Default: ``0.0``

.. option:: rho_cp_anohm <RefValue[float] (Optional)>

   Volumetric heat capacity for this surface to use in AnOHM

   :Unit: J m^-3 K^-1
   :Default: ``1200.0``

.. option:: k_anohm <RefValue[float] (Optional)>

   Thermal conductivity for this surface to use in AnOHM

   :Unit: W m^-1 K^-1
   :Default: ``0.4``

.. option:: ohm_threshsw <RefValue[float] (Optional)>

   Summer/winter threshold based on temperature for OHM calculation

   :Unit: degC
   :Default: ``0.0``

.. option:: ohm_threshwd <RefValue[float] (Optional)>

   Soil moisture threshold determining whether wet/dry OHM coefficients are applied

   :Unit: dimensionless
   :Default: ``0.0``

.. option:: ohm_coef <OHM_Coefficient_season_wetness (Optional)>

   :Default: ``PydanticUndefined``

   For ``ohm_coef``, if using the OHM_Coefficient_season_wetness structure, see :doc:`ohm_coefficient_season_wetness` for details.

.. option:: soildepth <RefValue[float]>

   Depth of soil layer for hydrological calculations

   :Unit: mm
   :Default: ``150``

.. option:: soilstorecap <RefValue[float]>

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: ``150.0``

.. option:: statelimit <RefValue[float]>

   Minimum water storage capacity for state change

   :Unit: mm
   :Default: ``10.0``

.. option:: wetthresh <RefValue[float]>

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Default: ``0.5``

.. option:: sathydraulicconduct <RefValue[float]>

   Saturated hydraulic conductivity of soil

   :Unit: mm s^-1
   :Default: ``0.0001``

.. option:: waterdist <WaterDistribution>

   Water distribution for bare soil

   :Default: ``PydanticUndefined``

   The ``waterdist`` parameter group is defined by the :doc:`waterdistribution` structure.

.. option:: storedrainprm <StorageDrainParams>

   Storage and drain parameters

   :Default: ``PydanticUndefined``

   The ``storedrainprm`` parameter group is defined by the :doc:`storagedrainparams` structure.

.. option:: snowpacklimit <RefValue[float] (Optional)>

   Limit of snow that can be held on surface

   :Unit: mm
   :Default: ``10.0``

.. option:: thermal_layers <ThermalLayers>

   Thermal layers for the surface

   :Default: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. option:: irrfrac <RefValue[float] (Optional)>

   Fraction of surface area that can be irrigated

   :Unit: dimensionless
   :Default: ``0.0``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.

.. option:: alb <RefValue[float]>

   Surface albedo

   :Unit: dimensionless
   :Default: ``0.1``
