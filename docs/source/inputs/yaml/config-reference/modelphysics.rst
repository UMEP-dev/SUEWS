.. meta::
   :description: SUEWS YAML configuration for physics methods parameters
   :keywords: SUEWS, YAML, modelphysics, parameters, configuration

.. _modelphysics:

.. index::
   single: ModelPhysics (YAML parameter)
   single: YAML; ModelPhysics

Physics Methods
===============

Model physics configuration options.

**Key method interactions:**


- :ref:`diagmethod <diagmethod>`: Determines **HOW** near-surface values (2m temp, 10m wind) are calculated from forcing data
- :ref:`stabilitymethod <stabilitymethod>`: Provides stability correction functions used **BY** ``diagmethod`` calculations
- :ref:`localclimatemethod <localclimatemethod>`: Uses the near-surface values **FROM** ``diagmethod`` to modify vegetation processes
- :ref:`gsmodel <gsmodel>`: Stomatal conductance model that may be influenced by ``localclimatemethod`` adjustments

**Parameters:**

.. index::
   single: netradiationmethod (YAML parameter)
   single: ModelPhysics; netradiationmethod

.. option:: netradiationmethod

   Method for calculating net all-wave radiation (Q*).

   :Options:
      | ``0`` (OBSERVED) = Uses observed Q* from forcing file
      | ``1`` (LDOWN_OBSERVED) = Models Q* using observed L↓
      | ``2`` (LDOWN_CLOUD) = Models Q* with L↓ from cloud cover
      | ``3`` (LDOWN_AIR) = Models Q* with L↓ from air temp and RH **(recommended)**

   :Default: ``3`` (LDOWN_AIR)

.. index::
   single: emissionsmethod (YAML parameter)
   single: ModelPhysics; emissionsmethod

.. option:: emissionsmethod

   Method for calculating anthropogenic heat flux (QF) and CO2 emissions.

   :Options:
      | ``0`` (NO_EMISSIONS) = Observed QF from forcing file
      | ``1`` (L11) = Loridan et al. 2011 linear temp relation
      | ``2`` (J11) = Järvi et al. 2011 with HDD/CDD
      | ``4`` (J19) = Järvi et al. 2019 including metabolism and traffic

   :Default: ``2`` (J11)

.. index::
   single: storageheatmethod (YAML parameter)
   single: ModelPhysics; storageheatmethod

.. option:: storageheatmethod

   Method for calculating storage heat flux (ΔQS).

   :Options:
      | ``0`` (OBSERVED) = Uses observed ΔQS from forcing file
      | ``1`` (OHM_WITHOUT_QF) = Objective Hysteresis Model using Q* only
      | ``5`` (ESTM_EXTENDED) = Extended ESTM with separate facet temps
      | ``6`` (OHM_ENHANCED) = Enhanced OHM parameterisation

   :Default: ``1`` (OHM_WITHOUT_QF)

.. index::
   single: ohmincqf (YAML parameter)
   single: ModelPhysics; ohmincqf

.. option:: ohmincqf

   Controls inclusion of anthropogenic heat flux in OHM storage heat calculations.

   :Options:
      | ``0`` (EXCLUDE) = Use Q* only (required when StorageHeatMethod=1)
      | ``1`` (INCLUDE) = Use Q*+QF (required when StorageHeatMethod=2)

   :Sample value: ``0`` (EXCLUDE)

.. index::
   single: roughlenmommethod (YAML parameter)
   single: ModelPhysics; roughlenmommethod

.. option:: roughlenmommethod

   Method for calculating momentum roughness length (z0m).

   :Options:
      | ``1`` (FIXED) = Fixed from site parameters
      | ``2`` (VARIABLE) = Varies with vegetation LAI
      | ``3`` (MACDONALD) = MacDonald et al. 1998 morphometric method
      | ``4`` (LAMBDAP_DEPENDENT) = Varies with plan area fraction

   :Default: ``2`` (VARIABLE)

.. index::
   single: roughlenheatmethod (YAML parameter)
   single: ModelPhysics; roughlenheatmethod

.. option:: roughlenheatmethod

   Method for calculating thermal roughness length (z0h).

   :Options:
      | ``1`` (FIXED) = Fixed from site parameters
      | ``2`` (VARIABLE) = Varies with vegetation LAI
      | ``3`` (MACDONALD) = MacDonald et al. 1998 morphometric method
      | ``4`` (LAMBDAP_DEPENDENT) = Varies with plan area fraction

   :Default: ``2`` (VARIABLE)

.. index::
   single: stabilitymethod (YAML parameter)
   single: ModelPhysics; stabilitymethod

.. _stabilitymethod:

.. option:: stabilitymethod

   Atmospheric stability correction functions for momentum and heat fluxes.

   :Options:
      | ``3`` (CAMPBELL_NORMAN) = Campbell & Norman 1998 formulations **(recommended)**

   :Default: ``3`` (CAMPBELL_NORMAN)

.. index::
   single: smdmethod (YAML parameter)
   single: ModelPhysics; smdmethod

.. option:: smdmethod

   Method for determining soil moisture deficit (SMD).

   :Options:
      | ``0`` (MODELLED) = Calculated from water balance using soil parameters
      | ``1`` (OBSERVED_VOLUMETRIC) = Uses observed volumetric soil moisture (m³/m³) from forcing file
      | ``2`` (OBSERVED_GRAVIMETRIC) = Uses observed gravimetric soil moisture (kg/kg) from forcing file

   :Default: ``0`` (MODELLED)

.. index::
   single: waterusemethod (YAML parameter)
   single: ModelPhysics; waterusemethod

.. option:: waterusemethod

   Method for determining external water use (irrigation).

   :Options:
      | ``0`` (MODELLED) = Calculated based on soil moisture deficit and irrigation parameters
      | ``1`` (OBSERVED) = Uses observed water use values from forcing file

   :Default: ``0`` (MODELLED)

.. index::
   single: rslmethod (YAML parameter)
   single: ModelPhysics; rslmethod

.. option:: rslmethod

   Method for calculating near-surface meteorological diagnostics (2m temperature, 2m humidity, 10m wind speed).

   :Options:
      | ``0`` (MOST) = Monin-Obukhov Similarity Theory for homogeneous surfaces
      | ``1`` (RST) = Roughness Sublayer Theory for heterogeneous urban surfaces
      | ``2`` (VARIABLE) = Automatic selection based on surface morphology (plan area index, frontal area index, and roughness element heights)

   :Default: ``2`` (VARIABLE)

.. index::
   single: faimethod (YAML parameter)
   single: ModelPhysics; faimethod

.. option:: faimethod

   Method for calculating frontal area index (FAI) - the ratio of frontal area to plan area.

   :Options:
      | ``1`` (SIMPLE_SCHEME) = Calculate FAI using simple scheme based on surface fractions and heights

   :Default: ``0`` (USE_PROVIDED)

.. index::
   single: rsllevel (YAML parameter)
   single: ModelPhysics; rsllevel

.. option:: rsllevel

   Method for incorporating urban microclimate feedbacks on vegetation and evapotranspiration.

   :Options:
      | ``0`` (NONE) = No local climate adjustments, use forcing file meteorology directly
      | ``1`` (BASIC) = Simple adjustments for urban temperature effects on leaf area index and growing degree days
      | ``2`` (DETAILED) = Comprehensive feedbacks including moisture stress, urban CO2 dome effects, and modified phenology cycles

   :Default: ``0`` (NONE)

.. index::
   single: gsmodel (YAML parameter)
   single: ModelPhysics; gsmodel

.. _gsmodel:

.. option:: gsmodel

   Stomatal conductance parameterisation method for vegetation surfaces.

   :Options:
      | ``1`` (JARVI) = Original parameterisation (Järvi et al. 2011) based on environmental controls
      | ``2`` (WARD) = Updated parameterisation (Ward et al. 2016) with improved temperature and VPD responses

   :Default: ``2`` (WARD)

.. index::
   single: snowuse (YAML parameter)
   single: ModelPhysics; snowuse

.. option:: snowuse

   Controls snow process calculations.

   :Options:
      | ``0`` (DISABLED) = Snow processes not included
      | ``1`` (ENABLED) = Snow accumulation, melt, and albedo effects included

   :Default: ``0`` (DISABLED)

.. index::
   single: stebbsmethod (YAML parameter)
   single: ModelPhysics; stebbsmethod

.. option:: stebbsmethod

   Surface Temperature Energy Balance Based Scheme (STEBBS) for facet temperatures.

   :Options:
      | ``0`` (NONE) = STEBBS disabled
      | ``1`` (DEFAULT) = STEBBS with default parameters
      | ``2`` (PROVIDED) = STEBBS with user-specified parameters

   :Default: ``0`` (NONE)

.. index::
   single: ref (YAML parameter)
   single: ModelPhysics; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
