Storagedrainparams
==================

**Parameters:**

.. option:: store_min <RefValue[float]>

   Minimum water storage capacity

   :Unit: mm
   :Default: ``0.0``

.. option:: store_max <RefValue[float]>

   Maximum water storage capacity

   :Unit: mm
   :Default: ``10.0``

.. option:: store_cap <RefValue[float]>

   Water storage capacity

   :Unit: mm
   :Default: ``10.0``

.. option:: drain_eq <RefValue[int]>

   Drainage equation selection (0: linear, 1: exponential)

   :Unit: dimensionless
   :Default: ``0``

.. option:: drain_coef_1 <RefValue[float]>

   Drainage coefficient 1 (rate parameter)

   :Unit: mm h^-1
   :Default: ``0.013``

.. option:: drain_coef_2 <RefValue[float]>

   Drainage coefficient 2 (shape parameter)

   :Unit: dimensionless
   :Default: ``1.71``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
