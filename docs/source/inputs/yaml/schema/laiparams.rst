Laiparams
=========

**Parameters:**

.. option:: baset <RefValue[float]>

   Base temperature for initiating growing degree days (GDD) for leaf growth

   :Unit: degC
   :Default: ``10.0``

.. option:: gddfull <RefValue[float]>

   Growing degree days (GDD) needed for full capacity of LAI

   :Unit: degC*day
   :Default: ``100.0``

.. option:: basete <RefValue[float]>

   Base temperature for initiating senescence degree days (SDD) for leaf off

   :Unit: degC
   :Default: ``10.0``

.. option:: sddfull <RefValue[float]>

   Senescence degree days (SDD) needed to initiate leaf off

   :Unit: degC*day
   :Default: ``100.0``

.. option:: laimin <RefValue[float]>

   Leaf-off wintertime LAI value

   :Unit: m^2 m^-2
   :Default: ``0.1``

.. option:: laimax <RefValue[float]>

   Full leaf-on summertime LAI value

   :Unit: m^2 m^-2
   :Default: ``10.0``

.. option:: laipower <LAIPowerCoefficients>

   LAI calculation power parameters for growth and senescence

   :Default: ``PydanticUndefined``

   The ``laipower`` parameter group is defined by the :doc:`laipowercoefficients` structure.

.. option:: laitype <RefValue[int]>

   LAI calculation choice (0: original, 1: new high latitude)

   :Unit: dimensionless
   :Default: ``0``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
