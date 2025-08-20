.. meta::
   :description: SUEWS YAML configuration for vegetation parameters parameters
   :keywords: SUEWS, YAML, vegetationparams, parameters, configuration

.. _vegetationparams:

.. index::
   single: VegetationParams (YAML parameter)
   single: YAML; VegetationParams

Vegetation Parameters
=====================

Vegetation phenology parameters.

**Parameters:**

.. index::
   single: porosity_id (YAML parameter)
   single: VegetationParams; porosity_id

.. option:: porosity_id

   Initial porosity for deciduous trees

   :Unit: dimensionless
   :Default: ``PydanticUndefined``

.. index::
   single: gdd_id (YAML parameter)
   single: VegetationParams; gdd_id

.. option:: gdd_id

   Growing degree days ID

   :Unit: degC d
   :Default: ``PydanticUndefined``

.. index::
   single: sdd_id (YAML parameter)
   single: VegetationParams; sdd_id

.. option:: sdd_id

   Senescence degree days ID

   :Unit: degC d
   :Default: ``PydanticUndefined``

.. index::
   single: lai (YAML parameter)
   single: VegetationParams; lai

.. option:: lai

   Leaf area index parameters

   :Unit: |m^2| |m^-2|
   :Sample value: ``PydanticUndefined``

.. index::
   single: ie_a (YAML parameter)
   single: VegetationParams; ie_a

.. option:: ie_a

   Irrigation efficiency coefficient a

   :Unit: dimensionless
   :Default: ``PydanticUndefined``

.. index::
   single: ie_m (YAML parameter)
   single: VegetationParams; ie_m

.. option:: ie_m

   Irrigation efficiency coefficient m

   :Unit: dimensionless
   :Default: ``PydanticUndefined``

.. index::
   single: ref (YAML parameter)
   single: VegetationParams; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
