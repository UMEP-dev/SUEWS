.. meta::
   :description: SUEWS YAML configuration for drainage parameters parameters
   :keywords: SUEWS, YAML, storagedrainparams, parameters, configuration

.. _storagedrainparams:

.. index::
   single: StorageDrainParams (YAML parameter)
   single: YAML; StorageDrainParams

Drainage Parameters
===================

Parameters for surface water storage drainage.

**Parameters:**

.. index::
   single: store_min (YAML parameter)
   single: StorageDrainParams; store_min

.. option:: store_min

   Minimum water storage capacity

   :Unit: mm
   :Default: ``0.0``

.. index::
   single: store_max (YAML parameter)
   single: StorageDrainParams; store_max

.. option:: store_max

   Maximum water storage capacity

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: store_cap (YAML parameter)
   single: StorageDrainParams; store_cap

.. option:: store_cap

   Current water storage capacity - the actual storage capacity available for surface water retention. This represents the depth of water that can be stored on or in the surface before drainage begins. For paved surfaces, this might represent depression storage; for vegetated surfaces, it includes canopy interception storage.

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: drain_eq (YAML parameter)
   single: StorageDrainParams; drain_eq

.. option:: drain_eq

   Drainage equation selection (0: linear, 1: exponential)

   :Unit: dimensionless
   :Default: ``0``

.. index::
   single: drain_coef_1 (YAML parameter)
   single: StorageDrainParams; drain_coef_1

.. option:: drain_coef_1

   Drainage coefficient 1 (rate parameter)

   :Unit: mm |h^-1|
   :Default: Required - must be specified

.. index::
   single: drain_coef_2 (YAML parameter)
   single: StorageDrainParams; drain_coef_2

.. option:: drain_coef_2

   Drainage coefficient 2 (shape parameter)

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: ref (YAML parameter)
   single: StorageDrainParams; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
