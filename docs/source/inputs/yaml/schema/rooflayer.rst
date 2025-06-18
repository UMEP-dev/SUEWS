Rooflayer
=========

**Parameters:**

.. option:: alb <RefValue[float]>

   Surface albedo

   :Unit: dimensionless
   :Default: ``0.1``

.. option:: emis <RefValue[float]>

   Surface emissivity

   :Unit: dimensionless
   :Default: ``0.95``

.. option:: thermal_layers <ThermalLayers>

   Thermal layers for the surface

   :Default: ``PydanticUndefined``

   The ``thermal_layers`` parameter group is defined by the :doc:`thermallayers` structure.

.. option:: statelimit <RefValue[float]>

   Minimum water storage capacity for state change

   :Unit: mm
   :Default: ``10.0``

.. option:: soilstorecap <RefValue[float]>

   Maximum water storage capacity of soil

   :Unit: mm
   :Default: ``150.0``

.. option:: wetthresh <RefValue[float]>

   Surface wetness threshold for OHM calculations

   :Unit: dimensionless
   :Default: ``0.5``

.. option:: roof_albedo_dir_mult_fact <RefValue[float] (Optional)>

   Directional albedo multiplication factor for roofs

   :Unit: dimensionless
   :Default: ``0.1``

.. option:: wall_specular_frac <RefValue[float] (Optional)>

   Specular reflection fraction for walls

   :Unit: dimensionless
   :Default: ``0.1``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
