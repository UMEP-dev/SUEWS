.. meta::
   :description: SUEWS YAML configuration for stebbs parameters
   :keywords: SUEWS, YAML, stebbsproperties, parameters, configuration

.. _stebbsproperties:

.. index::
   single: StebbsProperties (YAML parameter)
   single: YAML; StebbsProperties

STEBBS
======

STEBBS (Surface Temperature Energy Balance for Building Surfaces) model parameters.

Controls the building energy balance calculations including internal heating/cooling,
building materials properties, and thermal behaviour.

**Parameters:**

.. index::
   single: WallInternalConvectionCoefficient (YAML parameter)
   single: StebbsProperties; WallInternalConvectionCoefficient

.. option:: WallInternalConvectionCoefficient

   Internal convection coefficient of walls and roof [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: InternalMassConvectionCoefficient (YAML parameter)
   single: StebbsProperties; InternalMassConvectionCoefficient

.. option:: InternalMassConvectionCoefficient

   Convection coefficient of internal mass [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: FloorInternalConvectionCoefficient (YAML parameter)
   single: StebbsProperties; FloorInternalConvectionCoefficient

.. option:: FloorInternalConvectionCoefficient

   Internal convection coefficient of ground floor [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: WindowInternalConvectionCoefficient (YAML parameter)
   single: StebbsProperties; WindowInternalConvectionCoefficient

.. option:: WindowInternalConvectionCoefficient

   Internal convection coefficient of windows [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: WallExternalConvectionCoefficient (YAML parameter)
   single: StebbsProperties; WallExternalConvectionCoefficient

.. option:: WallExternalConvectionCoefficient

   Initial external convection coefficient of walls and roof [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: WindowExternalConvectionCoefficient (YAML parameter)
   single: StebbsProperties; WindowExternalConvectionCoefficient

.. option:: WindowExternalConvectionCoefficient

   Initial external convection coefficient of windows [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: GroundDepth (YAML parameter)
   single: StebbsProperties; GroundDepth

.. option:: GroundDepth

   Depth of external ground (deep soil) [m]

   :Unit: m
   :Sample value: ``0.0``

.. index::
   single: ExternalGroundConductivity (YAML parameter)
   single: StebbsProperties; ExternalGroundConductivity

.. option:: ExternalGroundConductivity

   External ground thermal conductivity

   :Unit: W |m^-1| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: IndoorAirDensity (YAML parameter)
   single: StebbsProperties; IndoorAirDensity

.. option:: IndoorAirDensity

   Density of indoor air [kg m-3]

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: IndoorAirCp (YAML parameter)
   single: StebbsProperties; IndoorAirCp

.. option:: IndoorAirCp

   Specific heat capacity of indoor air [J kg-1 K-1]

   :Unit: J |kg^-1| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: WallBuildingViewFactor (YAML parameter)
   single: StebbsProperties; WallBuildingViewFactor

.. option:: WallBuildingViewFactor

   Building view factor of external walls [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: WallGroundViewFactor (YAML parameter)
   single: StebbsProperties; WallGroundViewFactor

.. option:: WallGroundViewFactor

   Ground view factor of external walls [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: WallSkyViewFactor (YAML parameter)
   single: StebbsProperties; WallSkyViewFactor

.. option:: WallSkyViewFactor

   Sky view factor of external walls [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: MetabolicRate (YAML parameter)
   single: StebbsProperties; MetabolicRate

.. option:: MetabolicRate

   Metabolic rate of building occupants [W]

   :Unit: W
   :Sample value: ``0.0``

.. index::
   single: LatentSensibleRatio (YAML parameter)
   single: StebbsProperties; LatentSensibleRatio

.. option:: LatentSensibleRatio

   Latent-to-sensible ratio of metabolic energy release of occupants [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ApplianceRating (YAML parameter)
   single: StebbsProperties; ApplianceRating

.. option:: ApplianceRating

   Power demand of single appliance [W]

   :Unit: W
   :Sample value: ``0.0``

.. index::
   single: TotalNumberofAppliances (YAML parameter)
   single: StebbsProperties; TotalNumberofAppliances

.. option:: TotalNumberofAppliances

   Number of appliances present in building [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: ApplianceUsageFactor (YAML parameter)
   single: StebbsProperties; ApplianceUsageFactor

.. option:: ApplianceUsageFactor

   Number of appliances in use [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: HeatingSystemEfficiency (YAML parameter)
   single: StebbsProperties; HeatingSystemEfficiency

.. option:: HeatingSystemEfficiency

   Efficiency of space heating system [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: MaxCoolingPower (YAML parameter)
   single: StebbsProperties; MaxCoolingPower

.. option:: MaxCoolingPower

   Maximum power demand of cooling system [W]

   :Unit: W
   :Sample value: ``0.0``

.. index::
   single: CoolingSystemCOP (YAML parameter)
   single: StebbsProperties; CoolingSystemCOP

.. option:: CoolingSystemCOP

   Coefficient of performance of cooling system [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: VentilationRate (YAML parameter)
   single: StebbsProperties; VentilationRate

.. option:: VentilationRate

   Ventilation rate (air changes per hour, ACH) [h-1]

   :Unit: |h^-1|
   :Sample value: ``0.0``

.. index::
   single: IndoorAirStartTemperature (YAML parameter)
   single: StebbsProperties; IndoorAirStartTemperature

.. option:: IndoorAirStartTemperature

   Initial indoor air temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: IndoorMassStartTemperature (YAML parameter)
   single: StebbsProperties; IndoorMassStartTemperature

.. option:: IndoorMassStartTemperature

   Initial indoor mass temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: WallIndoorSurfaceTemperature (YAML parameter)
   single: StebbsProperties; WallIndoorSurfaceTemperature

.. option:: WallIndoorSurfaceTemperature

   Initial wall/roof indoor surface temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: WallOutdoorSurfaceTemperature (YAML parameter)
   single: StebbsProperties; WallOutdoorSurfaceTemperature

.. option:: WallOutdoorSurfaceTemperature

   Initial wall/roof outdoor surface temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: WindowIndoorSurfaceTemperature (YAML parameter)
   single: StebbsProperties; WindowIndoorSurfaceTemperature

.. option:: WindowIndoorSurfaceTemperature

   Initial window indoor surface temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: WindowOutdoorSurfaceTemperature (YAML parameter)
   single: StebbsProperties; WindowOutdoorSurfaceTemperature

.. option:: WindowOutdoorSurfaceTemperature

   Initial window outdoor surface temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: GroundFloorIndoorSurfaceTemperature (YAML parameter)
   single: StebbsProperties; GroundFloorIndoorSurfaceTemperature

.. option:: GroundFloorIndoorSurfaceTemperature

   Initial ground floor indoor surface temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: GroundFloorOutdoorSurfaceTemperature (YAML parameter)
   single: StebbsProperties; GroundFloorOutdoorSurfaceTemperature

.. option:: GroundFloorOutdoorSurfaceTemperature

   Initial ground floor outdoor surface temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: WaterTankTemperature (YAML parameter)
   single: StebbsProperties; WaterTankTemperature

.. option:: WaterTankTemperature

   Initial water temperature in hot water tank [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: InternalWallWaterTankTemperature (YAML parameter)
   single: StebbsProperties; InternalWallWaterTankTemperature

.. option:: InternalWallWaterTankTemperature

   Initial hot water tank internal wall temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: ExternalWallWaterTankTemperature (YAML parameter)
   single: StebbsProperties; ExternalWallWaterTankTemperature

.. option:: ExternalWallWaterTankTemperature

   Initial hot water tank external wall temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: WaterTankWallThickness (YAML parameter)
   single: StebbsProperties; WaterTankWallThickness

.. option:: WaterTankWallThickness

   Hot water tank wall thickness [m]

   :Unit: m
   :Sample value: ``0.0``

.. index::
   single: MainsWaterTemperature (YAML parameter)
   single: StebbsProperties; MainsWaterTemperature

.. option:: MainsWaterTemperature

   Temperature of water coming into the water tank [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: WaterTankSurfaceArea (YAML parameter)
   single: StebbsProperties; WaterTankSurfaceArea

.. option:: WaterTankSurfaceArea

   Surface area of hot water tank cylinder [m2]

   :Unit: |m^2|
   :Sample value: ``0.0``

.. index::
   single: HotWaterHeatingSetpointTemperature (YAML parameter)
   single: StebbsProperties; HotWaterHeatingSetpointTemperature

.. option:: HotWaterHeatingSetpointTemperature

   Water tank setpoint temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: HotWaterTankWallEmissivity (YAML parameter)
   single: StebbsProperties; HotWaterTankWallEmissivity

.. option:: HotWaterTankWallEmissivity

   Effective external wall emissivity of the hot water tank [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: DomesticHotWaterTemperatureInUseInBuilding (YAML parameter)
   single: StebbsProperties; DomesticHotWaterTemperatureInUseInBuilding

.. option:: DomesticHotWaterTemperatureInUseInBuilding

   Initial water temperature of water held in use in building [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: InternalWallDHWVesselTemperature (YAML parameter)
   single: StebbsProperties; InternalWallDHWVesselTemperature

.. option:: InternalWallDHWVesselTemperature

   Initial hot water vessel internal wall temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: ExternalWallDHWVesselTemperature (YAML parameter)
   single: StebbsProperties; ExternalWallDHWVesselTemperature

.. option:: ExternalWallDHWVesselTemperature

   Initial hot water vessel external wall temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: DHWVesselWallThickness (YAML parameter)
   single: StebbsProperties; DHWVesselWallThickness

.. option:: DHWVesselWallThickness

   Hot water vessel wall thickness [m]

   :Unit: m
   :Sample value: ``0.0``

.. index::
   single: DHWWaterVolume (YAML parameter)
   single: StebbsProperties; DHWWaterVolume

.. option:: DHWWaterVolume

   Volume of water held in use in building [m3]

   :Unit: |m^3|
   :Sample value: ``0.0``

.. index::
   single: DHWSurfaceArea (YAML parameter)
   single: StebbsProperties; DHWSurfaceArea

.. option:: DHWSurfaceArea

   Surface area of hot water in vessels in building [m2]

   :Unit: |m^2|
   :Sample value: ``0.0``

.. index::
   single: DHWVesselEmissivity (YAML parameter)
   single: StebbsProperties; DHWVesselEmissivity

.. option:: DHWVesselEmissivity

   NEEDS CHECKED! NOT USED (assumed same as DHWVesselWallEmissivity) [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: HotWaterFlowRate (YAML parameter)
   single: StebbsProperties; HotWaterFlowRate

.. option:: HotWaterFlowRate

   Hot water flow rate from tank to vessel [m3 s-1]

   :Unit: |m^3| |s^-1|
   :Sample value: ``0.0``

.. index::
   single: DHWDrainFlowRate (YAML parameter)
   single: StebbsProperties; DHWDrainFlowRate

.. option:: DHWDrainFlowRate

   Flow rate of hot water held in building to drain [m3 s-1]

   :Unit: |m^3| |s^-1|
   :Sample value: ``0.0``

.. index::
   single: DHWSpecificHeatCapacity (YAML parameter)
   single: StebbsProperties; DHWSpecificHeatCapacity

.. option:: DHWSpecificHeatCapacity

   Specific heat capacity of hot water [J kg-1 K-1]

   :Unit: J |kg^-1| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: HotWaterTankSpecificHeatCapacity (YAML parameter)
   single: StebbsProperties; HotWaterTankSpecificHeatCapacity

.. option:: HotWaterTankSpecificHeatCapacity

   Specific heat capacity of hot water tank wal [J kg-1 K-1]

   :Unit: J |kg^-1| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: DHWVesselSpecificHeatCapacity (YAML parameter)
   single: StebbsProperties; DHWVesselSpecificHeatCapacity

.. option:: DHWVesselSpecificHeatCapacity

   Specific heat capacity of vessels containing hot water in use in buildings [J kg-1 K-1]

   :Unit: J |kg^-1| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: DHWDensity (YAML parameter)
   single: StebbsProperties; DHWDensity

.. option:: DHWDensity

   Density of hot water in use [kg m-3]

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: HotWaterTankWallDensity (YAML parameter)
   single: StebbsProperties; HotWaterTankWallDensity

.. option:: HotWaterTankWallDensity

   Density of hot water tank wall [kg m-3]

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: DHWVesselDensity (YAML parameter)
   single: StebbsProperties; DHWVesselDensity

.. option:: DHWVesselDensity

   Density of vessels containing hot water in use [kg m-3]

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: HotWaterTankBuildingWallViewFactor (YAML parameter)
   single: StebbsProperties; HotWaterTankBuildingWallViewFactor

.. option:: HotWaterTankBuildingWallViewFactor

   Water tank/vessel internal building wall/roof view factor [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: HotWaterTankInternalMassViewFactor (YAML parameter)
   single: StebbsProperties; HotWaterTankInternalMassViewFactor

.. option:: HotWaterTankInternalMassViewFactor

   Water tank/vessel building internal mass view factor [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: HotWaterTankWallConductivity (YAML parameter)
   single: StebbsProperties; HotWaterTankWallConductivity

.. option:: HotWaterTankWallConductivity

   Effective wall conductivity of the hot water tank [W m-1 K-1]

   :Unit: W |m^-1| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: HotWaterTankInternalWallConvectionCoefficient (YAML parameter)
   single: StebbsProperties; HotWaterTankInternalWallConvectionCoefficient

.. option:: HotWaterTankInternalWallConvectionCoefficient

   Effective internal wall convection coefficient of the hot water tank [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: HotWaterTankExternalWallConvectionCoefficient (YAML parameter)
   single: StebbsProperties; HotWaterTankExternalWallConvectionCoefficient

.. option:: HotWaterTankExternalWallConvectionCoefficient

   Effective external wall convection coefficient of the hot water tank [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: DHWVesselWallConductivity (YAML parameter)
   single: StebbsProperties; DHWVesselWallConductivity

.. option:: DHWVesselWallConductivity

   Effective wall conductivity of the hot water tank [W m-1 K-1]

   :Unit: W |m^-1| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: DHWVesselInternalWallConvectionCoefficient (YAML parameter)
   single: StebbsProperties; DHWVesselInternalWallConvectionCoefficient

.. option:: DHWVesselInternalWallConvectionCoefficient

   Effective internal wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: DHWVesselExternalWallConvectionCoefficient (YAML parameter)
   single: StebbsProperties; DHWVesselExternalWallConvectionCoefficient

.. option:: DHWVesselExternalWallConvectionCoefficient

   Effective external wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]

   :Unit: W |m^-2| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: DHWVesselWallEmissivity (YAML parameter)
   single: StebbsProperties; DHWVesselWallEmissivity

.. option:: DHWVesselWallEmissivity

   Effective external wall emissivity of hot water being used within building [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: HotWaterHeatingEfficiency (YAML parameter)
   single: StebbsProperties; HotWaterHeatingEfficiency

.. option:: HotWaterHeatingEfficiency

   Efficiency of hot water system [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: MinimumVolumeOfDHWinUse (YAML parameter)
   single: StebbsProperties; MinimumVolumeOfDHWinUse

.. option:: MinimumVolumeOfDHWinUse

   Minimum volume of hot water in use [m3]

   :Unit: |m^3|
   :Sample value: ``0.0``

.. index::
   single: ref (YAML parameter)
   single: StebbsProperties; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
