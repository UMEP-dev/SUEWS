.. meta::
   :description: SUEWS YAML configuration for seasonal/wetness ohm coefficients parameters
   :keywords: SUEWS, YAML, ohm_coefficient_season_wetness, parameters, configuration

.. _ohm_coefficient_season_wetness:

.. index::
   single: OHM_Coefficient_season_wetness (YAML parameter)
   single: YAML; OHM_Coefficient_season_wetness

Seasonal/Wetness OHM Coefficients
=================================

OHM coefficients for different seasonal and wetness conditions.

**Parameters:**

.. index::
   single: summer_dry (YAML parameter)
   single: OHM_Coefficient_season_wetness; summer_dry

.. option:: summer_dry

   OHM coefficient for summer dry conditions

   :Default: ``PydanticUndefined``

   The ``summer_dry`` parameter group is defined by the :doc:`ohmcoefficients` structure.

.. index::
   single: summer_wet (YAML parameter)
   single: OHM_Coefficient_season_wetness; summer_wet

.. option:: summer_wet

   OHM coefficient for summer wet conditions

   :Default: ``PydanticUndefined``

   The ``summer_wet`` parameter group is defined by the :doc:`ohmcoefficients` structure.

.. index::
   single: winter_dry (YAML parameter)
   single: OHM_Coefficient_season_wetness; winter_dry

.. option:: winter_dry

   OHM coefficient for winter dry conditions

   :Default: ``PydanticUndefined``

   The ``winter_dry`` parameter group is defined by the :doc:`ohmcoefficients` structure.

.. index::
   single: winter_wet (YAML parameter)
   single: OHM_Coefficient_season_wetness; winter_wet

.. option:: winter_wet

   OHM coefficient for winter wet conditions

   :Default: ``PydanticUndefined``

   The ``winter_wet`` parameter group is defined by the :doc:`ohmcoefficients` structure.

.. index::
   single: ref (YAML parameter)
   single: OHM_Coefficient_season_wetness; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
