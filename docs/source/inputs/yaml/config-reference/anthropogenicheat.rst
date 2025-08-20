.. meta::
   :description: SUEWS YAML configuration for anthropogenic heat parameters
   :keywords: SUEWS, YAML, anthropogenicheat, parameters, configuration

.. _anthropogenicheat:

.. index::
   single: AnthropogenicHeat (YAML parameter)
   single: YAML; AnthropogenicHeat

Anthropogenic Heat
==================

Anthropogenic heat flux parameters and profiles.

**Parameters:**

.. index::
   single: qf0_beu (YAML parameter)
   single: AnthropogenicHeat; qf0_beu

.. option:: qf0_beu

   Base anthropogenic heat flux for buildings, equipment and urban metabolism

   :Sample value: ``PydanticUndefined``

   The ``qf0_beu`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: qf_a (YAML parameter)
   single: AnthropogenicHeat; qf_a

.. option:: qf_a

   Coefficient a for anthropogenic heat flux calculation

   :Sample value: ``PydanticUndefined``

   The ``qf_a`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: qf_b (YAML parameter)
   single: AnthropogenicHeat; qf_b

.. option:: qf_b

   Coefficient b for anthropogenic heat flux calculation

   :Sample value: ``PydanticUndefined``

   The ``qf_b`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: qf_c (YAML parameter)
   single: AnthropogenicHeat; qf_c

.. option:: qf_c

   Coefficient c for anthropogenic heat flux calculation

   :Sample value: ``PydanticUndefined``

   The ``qf_c`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: baset_cooling (YAML parameter)
   single: AnthropogenicHeat; baset_cooling

.. option:: baset_cooling

   Base temperature for cooling degree days

   :Default: ``PydanticUndefined``

   The ``baset_cooling`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: baset_heating (YAML parameter)
   single: AnthropogenicHeat; baset_heating

.. option:: baset_heating

   Base temperature for heating degree days

   :Default: ``PydanticUndefined``

   The ``baset_heating`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: ah_min (YAML parameter)
   single: AnthropogenicHeat; ah_min

.. option:: ah_min

   Minimum anthropogenic heat flux

   :Default: ``PydanticUndefined``

   The ``ah_min`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: ah_slope_cooling (YAML parameter)
   single: AnthropogenicHeat; ah_slope_cooling

.. option:: ah_slope_cooling

   Slope of anthropogenic heat vs cooling degree days

   :Default: ``PydanticUndefined``

   The ``ah_slope_cooling`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: ah_slope_heating (YAML parameter)
   single: AnthropogenicHeat; ah_slope_heating

.. option:: ah_slope_heating

   Slope of anthropogenic heat vs heating degree days

   :Default: ``PydanticUndefined``

   The ``ah_slope_heating`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: ahprof_24hr (YAML parameter)
   single: AnthropogenicHeat; ahprof_24hr

.. option:: ahprof_24hr

   24-hour profile of anthropogenic heat flux

   :Default: ``PydanticUndefined``

   The ``ahprof_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. index::
   single: popdensdaytime (YAML parameter)
   single: AnthropogenicHeat; popdensdaytime

.. option:: popdensdaytime

   Daytime population density

   :Sample value: ``PydanticUndefined``

   The ``popdensdaytime`` parameter group is defined by the :doc:`dayprofile` structure.

.. index::
   single: popdensnighttime (YAML parameter)
   single: AnthropogenicHeat; popdensnighttime

.. option:: popdensnighttime

   Nighttime population density

   :Unit: people |ha^-1|
   :Sample value: ``10.0``

.. index::
   single: popprof_24hr (YAML parameter)
   single: AnthropogenicHeat; popprof_24hr

.. option:: popprof_24hr

   24-hour profile of population density

   :Default: ``PydanticUndefined``

   The ``popprof_24hr`` parameter group is defined by the :doc:`hourlyprofile` structure.

.. index::
   single: ref (YAML parameter)
   single: AnthropogenicHeat; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
