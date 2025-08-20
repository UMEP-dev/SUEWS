.. meta::
   :description: SUEWS YAML configuration for ohm coefficients parameters
   :keywords: SUEWS, YAML, ohmcoefficients, parameters, configuration

.. _ohmcoefficients:

.. index::
   single: OHMCoefficients (YAML parameter)
   single: YAML; OHMCoefficients

OHM Coefficients
================

Objective Hysteresis Model coefficients.

**Parameters:**

.. index::
   single: a1 (YAML parameter)
   single: OHMCoefficients; a1

.. option:: a1

   OHM coefficient a1: dimensionless coefficient relating storage heat flux to net radiation

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: a2 (YAML parameter)
   single: OHMCoefficients; a2

.. option:: a2

   OHM coefficient a2: time coefficient relating storage heat flux to rate of change of net radiation

   :Unit: h
   :Default: Required - must be specified

.. index::
   single: a3 (YAML parameter)
   single: OHMCoefficients; a3

.. option:: a3

   OHM coefficient a3: constant offset term for storage heat flux

   :Unit: W |m^-2|
   :Default: Required - must be specified

.. index::
   single: ref (YAML parameter)
   single: OHMCoefficients; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
