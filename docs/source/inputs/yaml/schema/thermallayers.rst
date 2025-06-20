Thermallayers
=============

**Parameters:**

.. option:: dz <RefValue[List[float]]>

   Thickness of thermal layers from surface to depth

   :Unit: m
   :Default: ``[0.1, 0.2, 0.3, 0.4, 0.5]``

.. option:: k <RefValue[List[float]]>

   Thermal conductivity of each thermal layer

   :Unit: W m^-1 K^-1
   :Default: ``[1.0, 1.0, 1.0, 1.0, 1.0]``

.. option:: rho_cp <RefValue[List[float]]>

   Volumetric heat capacity of each thermal layer

   :Unit: J m^-3 K^-1
   :Default: ``[1000, 1000, 1000, 1000, 1000]``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
