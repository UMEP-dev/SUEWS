Modelphysics
============

**Parameters:**

.. option:: netradiationmethod <RefValue[NetRadiationMethod]>

   Method used to calculate net all-wave radiation (Q*). Options include observed values, modelled with various longwave parameterisations, and SPARTACUS-Surface integration

   :Unit: dimensionless
   :Default: ``NetRadiationMethod.LDOWN_AIR``

.. option:: emissionsmethod <RefValue[EmissionsMethod]>

   Method used to calculate anthropogenic heat flux (QF) and CO2 emissions. Options include observed values, Loridan et al. (2011) SAHP, Järvi et al. (2011) SAHP_2, and Järvi et al. (2019) methods

   :Unit: dimensionless
   :Default: ``EmissionsMethod.J11``

.. option:: storageheatmethod <RefValue[StorageHeatMethod]>

   Method used to calculate storage heat flux (ΔQS). Options include observed values, Objective Hysteresis Model (OHM), AnOHM, Element Surface Temperature Method (ESTM), and extended ESTM

   :Unit: dimensionless
   :Default: ``StorageHeatMethod.OHM_WITHOUT_QF``

.. option:: ohmincqf <RefValue[OhmIncQf]>

   Whether to include anthropogenic heat flux (QF) in OHM storage heat calculations. 0: use Q* only, 1: use Q*+QF

   :Unit: dimensionless
   :Default: ``OhmIncQf.EXCLUDE``

.. option:: roughlenmommethod <RefValue[RoughnessMethod]>

   Method used to calculate momentum roughness length (z0). Options include fixed values, variable based on vegetation, MacDonald (1998), and Grimmond & Oke (1999) methods

   :Unit: dimensionless
   :Default: ``RoughnessMethod.VARIABLE``

.. option:: roughlenheatmethod <RefValue[RoughnessMethod]>

   Method used to calculate heat roughness length (z0h). Options include fixed values, variable based on vegetation, MacDonald (1998), and Grimmond & Oke (1999) methods

   :Unit: dimensionless
   :Default: ``RoughnessMethod.VARIABLE``

.. option:: stabilitymethod <RefValue[StabilityMethod]>

   Method used for atmospheric stability correction functions. Options include Dyer (1974)/Högström (1988), Campbell & Norman (1998), and Businger et al. (1971) formulations

   :Unit: dimensionless
   :Default: ``StabilityMethod.CAMPBELL_NORMAN``

.. option:: smdmethod <RefValue[SMDMethod]>

   Method used to calculate soil moisture deficit (SMD). Options include modelled using parameters, or observed volumetric/gravimetric soil moisture from forcing file

   :Unit: dimensionless
   :Default: ``SMDMethod.MODELLED``

.. option:: waterusemethod <RefValue[WaterUseMethod]>

   Method used to calculate external water use for irrigation. Options include modelled using parameters or observed values from forcing file

   :Unit: dimensionless
   :Default: ``WaterUseMethod.MODELLED``

.. option:: diagmethod <RefValue[DiagMethod]>

   Method used for calculating near-surface diagnostics and profiles of temperature, humidity, and wind speed. Options include MOST, RST, or variable selection based on surface characteristics

   :Unit: dimensionless
   :Default: ``DiagMethod.VARIABLE``

.. option:: faimethod <RefValue[FAIMethod]>

   Method used to calculate frontal area index (FAI). Options include fixed values or variable based on vegetation state

   :Unit: dimensionless
   :Default: ``FAIMethod.FIXED``

.. option:: localclimatemethod <RefValue[LocalClimateMethod]>

   Method used for accounting for local climate effects on surface processes (e.g. near-surface temperature impacts on phenology). Options include none, basic, or detailed approaches

   :Unit: dimensionless
   :Default: ``LocalClimateMethod.NONE``

.. option:: snowuse <RefValue[SnowUse]>

   Whether to include snow calculations in the model. 0: snow calculations disabled, 1: snow calculations enabled

   :Unit: dimensionless
   :Default: ``SnowUse.DISABLED``

.. option:: stebbsmethod <RefValue[StebbsMethod]>

   Method used for STEBBS (Surface Temperature Energy Balance Based Scheme) calculations. Options include none, default parameters, or user-provided parameters

   :Unit: dimensionless
   :Default: ``StebbsMethod.NONE``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
