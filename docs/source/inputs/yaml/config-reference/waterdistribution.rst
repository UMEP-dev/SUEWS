.. meta::
   :description: SUEWS YAML configuration for water distribution parameters
   :keywords: SUEWS, YAML, waterdistribution, parameters, configuration

.. _waterdistribution:

.. index::
   single: WaterDistribution (YAML parameter)
   single: YAML; WaterDistribution

Water Distribution
==================

Water routing between surface types.

**Parameters:**

.. index::
   single: to_paved (YAML parameter)
   single: WaterDistribution; to_paved

.. option:: to_paved

   Fraction of water redistributed to paved surfaces within the grid

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: to_bldgs (YAML parameter)
   single: WaterDistribution; to_bldgs

.. option:: to_bldgs

   Fraction of water redistributed to building surfaces within the grid

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: to_dectr (YAML parameter)
   single: WaterDistribution; to_dectr

.. option:: to_dectr

   Fraction of water redistributed to deciduous tree surfaces within the grid

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: to_evetr (YAML parameter)
   single: WaterDistribution; to_evetr

.. option:: to_evetr

   Fraction of water redistributed to evergreen tree surfaces within the grid

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: to_grass (YAML parameter)
   single: WaterDistribution; to_grass

.. option:: to_grass

   Fraction of water redistributed to grass surfaces within the grid

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: to_bsoil (YAML parameter)
   single: WaterDistribution; to_bsoil

.. option:: to_bsoil

   Fraction of water redistributed to bare soil surfaces within the grid

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: to_water (YAML parameter)
   single: WaterDistribution; to_water

.. option:: to_water

   Fraction of water redistributed to water surfaces within the grid

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: to_runoff (YAML parameter)
   single: WaterDistribution; to_runoff

.. option:: to_runoff

   Fraction of water going to surface runoff (for impervious surfaces: paved and buildings)

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: to_soilstore (YAML parameter)
   single: WaterDistribution; to_soilstore

.. option:: to_soilstore

   Fraction of water going to subsurface soil storage (for pervious surfaces: vegetation and bare soil)

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: ref (YAML parameter)
   single: WaterDistribution; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
