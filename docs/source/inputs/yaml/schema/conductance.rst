Conductance
===========

**Parameters:**

.. option:: g_max <RefValue[float]>

   Maximum surface conductance for photosynthesis

   :Unit: mm s^-1
   :Default: ``40.0``

.. option:: g_k <RefValue[float]>

   Conductance parameter related to incoming solar radiation

   :Unit: dimensionless
   :Default: ``0.6``

.. option:: g_q_base <RefValue[float]>

   Base value for conductance parameter related to vapor pressure deficit

   :Unit: kPa^-1
   :Default: ``0.03``

.. option:: g_q_shape <RefValue[float]>

   Shape parameter for conductance related to vapor pressure deficit

   :Unit: dimensionless
   :Default: ``0.9``

.. option:: g_t <RefValue[float]>

   Conductance parameter related to air temperature

   :Unit: degC
   :Default: ``30.0``

.. option:: g_sm <RefValue[float]>

   Conductance parameter related to soil moisture

   :Unit: dimensionless
   :Default: ``0.5``

.. option:: kmax <RefValue[float]>

   Maximum incoming shortwave radiation

   :Unit: W m^-2
   :Default: ``1200.0``

.. option:: gsmodel <RefValue[int]>

   Stomatal conductance model selection

   :Unit: dimensionless
   :Default: ``1``

.. option:: s1 <RefValue[float]>

   Lower soil moisture threshold for conductance response

   :Unit: dimensionless
   :Default: ``0.2``

.. option:: s2 <RefValue[float]>

   Parameter related to soil moisture dependence

   :Unit: mm
   :Default: ``0.5``

.. option:: tl <RefValue[float]>

   Lower air temperature threshold for conductance response

   :Unit: degC
   :Default: ``0.0``

.. option:: th <RefValue[float]>

   Upper air temperature threshold for conductance response

   :Unit: degC
   :Default: ``50.0``

.. option:: ref <Reference (Optional)>

   :Default: ``Reference(desc=None, ID='test id', DOI='test doi')``

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
