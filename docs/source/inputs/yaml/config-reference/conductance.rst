.. meta::
   :description: SUEWS YAML configuration for conductance parameters
   :keywords: SUEWS, YAML, conductance, parameters, configuration

.. _conductance:

.. index::
   single: Conductance (YAML parameter)
   single: YAML; Conductance

Conductance
===========

Surface conductance parameters for water vapour and heat exchange.

These parameters control the resistance to water vapour transfer from surfaces,
which is critical for calculating evapotranspiration rates.

**Parameters:**

.. index::
   single: g_max (YAML parameter)
   single: Conductance; g_max

.. option:: g_max

   Maximum surface conductance for photosynthesis

   :Unit: mm |s^-1|
   :Default: Required - must be specified

.. index::
   single: g_k (YAML parameter)
   single: Conductance; g_k

.. option:: g_k

   Conductance parameter related to incoming solar radiation

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: g_q_base (YAML parameter)
   single: Conductance; g_q_base

.. option:: g_q_base

   Base value for conductance parameter related to vapour pressure deficit

   :Unit: kPa^-1
   :Default: Required - must be specified

.. index::
   single: g_q_shape (YAML parameter)
   single: Conductance; g_q_shape

.. option:: g_q_shape

   Shape parameter for conductance related to vapour pressure deficit

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: g_t (YAML parameter)
   single: Conductance; g_t

.. option:: g_t

   Conductance parameter related to air temperature

   :Unit: degC
   :Default: Required - must be specified

.. index::
   single: g_sm (YAML parameter)
   single: Conductance; g_sm

.. option:: g_sm

   Conductance parameter related to soil moisture

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: kmax (YAML parameter)
   single: Conductance; kmax

.. option:: kmax

   Maximum incoming shortwave radiation

   :Unit: W |m^-2|
   :Default: Required - must be specified

.. index::
   single: s1 (YAML parameter)
   single: Conductance; s1

.. option:: s1

   Lower soil moisture threshold for conductance response

   :Unit: dimensionless
   :Default: Required - must be specified

.. index::
   single: s2 (YAML parameter)
   single: Conductance; s2

.. option:: s2

   Parameter related to soil moisture dependence

   :Unit: mm
   :Default: Required - must be specified

.. index::
   single: tl (YAML parameter)
   single: Conductance; tl

.. option:: tl

   Lower air temperature threshold for conductance response

   :Unit: degC
   :Default: Required - must be specified

.. index::
   single: th (YAML parameter)
   single: Conductance; th

.. option:: th

   Upper air temperature threshold for conductance response

   :Unit: degC
   :Default: Required - must be specified

.. index::
   single: ref (YAML parameter)
   single: Conductance; ref

.. option:: ref

   :Default: Reference object

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
