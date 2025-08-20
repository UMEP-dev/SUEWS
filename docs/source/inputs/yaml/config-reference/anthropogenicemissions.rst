.. meta::
   :description: SUEWS YAML configuration for anthropogenic emissions parameters
   :keywords: SUEWS, YAML, anthropogenicemissions, parameters, configuration

.. _anthropogenicemissions:

.. index::
   single: AnthropogenicEmissions (YAML parameter)
   single: YAML; AnthropogenicEmissions

Anthropogenic Emissions
=======================

**Parameters:**

.. index::
   single: startdls (YAML parameter)
   single: AnthropogenicEmissions; startdls

.. option:: startdls

   Start of daylight savings time in decimal day of year

   :Unit: day
   :Default: Required - must be specified

.. index::
   single: enddls (YAML parameter)
   single: AnthropogenicEmissions; enddls

.. option:: enddls

   End of daylight savings time in decimal day of year

   :Unit: day
   :Default: Required - must be specified

.. index::
   single: heat (YAML parameter)
   single: AnthropogenicEmissions; heat

.. option:: heat

   Anthropogenic heat emission parameters

   :Default: ``PydanticUndefined``

   The ``heat`` parameter group is defined by the :doc:`anthropogenicheat` structure.

.. index::
   single: co2 (YAML parameter)
   single: AnthropogenicEmissions; co2

.. option:: co2

   CO2 emission parameters

   :Default: ``PydanticUndefined``

   The ``co2`` parameter group is defined by the :doc:`co2params` structure.

.. index::
   single: ref (YAML parameter)
   single: AnthropogenicEmissions; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
