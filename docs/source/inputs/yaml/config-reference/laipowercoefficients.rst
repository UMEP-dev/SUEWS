.. meta::
   :description: SUEWS YAML configuration for lai power coefficients parameters
   :keywords: SUEWS, YAML, laipowercoefficients, parameters, configuration

.. _laipowercoefficients:

.. index::
   single: LAIPowerCoefficients (YAML parameter)
   single: YAML; LAIPowerCoefficients

LAI Power Coefficients
======================

Power law coefficients for LAI calculation.

**Parameters:**

.. index::
   single: growth_lai (YAML parameter)
   single: LAIPowerCoefficients; growth_lai

.. option:: growth_lai

   Power coefficient for LAI in growth equation (LAIPower[1])

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: growth_gdd (YAML parameter)
   single: LAIPowerCoefficients; growth_gdd

.. option:: growth_gdd

   Power coefficient for GDD in growth equation (LAIPower[2])

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: senescence_lai (YAML parameter)
   single: LAIPowerCoefficients; senescence_lai

.. option:: senescence_lai

   Power coefficient for LAI in senescence equation (LAIPower[3])

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: senescence_sdd (YAML parameter)
   single: LAIPowerCoefficients; senescence_sdd

.. option:: senescence_sdd

   Power coefficient for SDD in senescence equation (LAIPower[4])

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: ref (YAML parameter)
   single: LAIPowerCoefficients; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
