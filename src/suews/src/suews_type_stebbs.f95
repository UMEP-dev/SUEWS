module module_type_stebbs

    ! use module_ctrl_type

    implicit none

    TYPE, PUBLIC :: BUILDING_ARCHETYPE_PRM
      ! This type is used to collect building archetypes for STEBBS
      ! CHARACTER(LEN=50) :: BuildingCode !
      ! CHARACTER(LEN=50) :: BuildingClass !
      ! CHARACTER(LEN=50) :: BuildingType !
      ! CHARACTER(LEN=50) :: BuildingName !
      REAL(KIND(1D0)) :: BuildingCount = 0.0D0 ! Number of buildings of this archetype [-]
      REAL(KIND(1D0)) :: Occupants = 0.0D0 ! Number of occupants present in building [-]
      REAL(KIND(1D0)) :: hhs0 = 0.0D0 !
      REAL(KIND(1D0)) :: age_0_4 = 0.0D0 !
      REAL(KIND(1D0)) :: age_5_11 = 0.0D0 !
      REAL(KIND(1D0)) :: age_12_18 = 0.0D0 !
      REAL(KIND(1D0)) :: age_19_64 = 0.0D0 !
      REAL(KIND(1D0)) :: age_65plus = 0.0D0 !
      REAL(KIND(1D0)) :: stebbs_Height = 0.0D0 ! Building height [m]
      REAL(KIND(1D0)) :: FootprintArea = 0.0D0 ! Building footprint area [m2]
      REAL(KIND(1D0)) :: WallExternalArea = 0.0D0 ! External wall area (including window area) [m2]
      REAL(KIND(1D0)) :: RatioInternalVolume = 0.0D0 ! Ratio of internal mass volume to total building volume [-]
      REAL(KIND(1D0)) :: WWR = 0.0D0 ! window to wall ratio [-]
      REAL(KIND(1D0)) :: WallThickness = 0.0D0 ! Thickness of external wall [m]
      REAL(KIND(1D0)) :: WallEffectiveConductivity = 0.0D0 ! Effective thermal conductivity of walls [W m-1 K-1]
      REAL(KIND(1D0)) :: WallDensity = 0.0D0 ! Effective density of the walls [kg m-3]
      REAL(KIND(1D0)) :: WallCp = 0.0D0 ! Effective specific heat capacity of walls [J kg-1 K-1]
      REAL(KIND(1D0)) :: WallextThickness = 0.0D0 ! Thickness of layers external to insulation in external wall [m]
      REAL(KIND(1D0)) :: WallextEffectiveConductivity = 0.0D0 ! Effective thermal conductivity of layers external to insulation inof walls [W m-1 K-1]
      REAL(KIND(1D0)) :: WallextDensity = 0.0D0 ! Effective density of layers external to insulation in the walls [kg m-3]
      REAL(KIND(1D0)) :: WallextCp = 0.0D0 ! Effective specific heat capacity of layers external to insulation in walls [J kg-1 K-1]
      REAL(KIND(1D0)) :: Wallx1 = 0.0D0 ! Weighting factor for heat capacity of walls [-]
      REAL(KIND(1D0)) :: WallExternalEmissivity = 0.0D0 ! Emissivity of the external surface of walls [-]
      REAL(KIND(1D0)) :: WallInternalEmissivity = 0.0D0 ! Emissivity of the internal surface of walls [-]
      REAL(KIND(1D0)) :: WallTransmissivity = 0.0D0 ! Transmissivity of walls [-]
      REAL(KIND(1D0)) :: WallAbsorbtivity = 0.0D0 ! Absorbtivity of walls [-]
      REAL(KIND(1D0)) :: WallReflectivity = 0.0D0 ! Reflectivity of the external surface of walls [-]
      REAL(KIND(1D0)) :: RoofThickness = 0.0D0 ! Thickness of external roof [m]
      REAL(KIND(1D0)) :: RoofEffectiveConductivity = 0.0D0 ! Effective thermal conductivity of roofs (weighted) [W m-1 K-1]
      REAL(KIND(1D0)) :: RoofDensity = 0.0D0 ! Effective density of the roof  [kg m-3]
      REAL(KIND(1D0)) :: RoofCp = 0.0D0 ! Effective specific heat capacity of layers external to insulation in roof  [J kg-1 K-1]
      REAL(KIND(1D0)) :: RoofextThickness = 0.0D0 ! Thickness of external  layers external to insulation roof [m]
      REAL(KIND(1D0)) :: RoofextEffectiveConductivity = 0.0D0 ! Effective thermal conductivity of  layers external to insulation roofs (weighted) [W m-1 K-1]
      REAL(KIND(1D0)) :: RoofextDensity = 0.0D0 ! Effective density of the  layers external to insulation roof [kg m-3]
      REAL(KIND(1D0)) :: RoofextCp = 0.0D0 ! Effective specific heat capacity of layers external to insulation in roof  [J kg-1 K-1]
      REAL(KIND(1D0)) :: Roofx1 = 0.0D0 ! Weighting factor for heat capacity of roof [-]
      REAL(KIND(1D0)) :: RoofExternalEmissivity = 0.0D0 ! Emissivity of the external surface of roof [-]
      REAL(KIND(1D0)) :: RoofInternalEmissivity = 0.0D0 ! Emissivity of the internal surface of roof [-]
      REAL(KIND(1D0)) :: RoofTransmissivity = 0.0D0 ! Transmissivity of walls roof [-]
      REAL(KIND(1D0)) :: RoofAbsorbtivity = 0.0D0 ! Absorbtivity of walls roof [-]
      REAL(KIND(1D0)) :: RoofReflectivity = 0.0D0 ! Reflectivity of the external surface of roof [-]
      REAL(KIND(1D0)) :: FloorThickness = 0.0D0 ! Thickness of ground floor [m]
      REAL(KIND(1D0)) :: GroundFloorEffectiveConductivity = 0.0D0 ! Effective thermal conductivity of ground floor [W m-1 K-1]
      REAL(KIND(1D0)) :: GroundFloorDensity = 0.0D0 ! Density of the ground floor [kg m-3]
      REAL(KIND(1D0)) :: GroundFloorCp = 0.0D0 ! Effective specific heat capacity of the ground floor [J kg-1 K-1]
      REAL(KIND(1D0)) :: WindowThickness = 0.0D0 ! Window thickness [m]
      REAL(KIND(1D0)) :: WindowEffectiveConductivity = 0.0D0 ! Effective thermal conductivity of windows [W m-1 K-1]
      REAL(KIND(1D0)) :: WindowDensity = 0.0D0 ! Effective density of the windows [kg m-3]
      REAL(KIND(1D0)) :: WindowCp = 0.0D0 ! Effective specific heat capacity of windows [J kg-1 K-1]
      REAL(KIND(1D0)) :: WindowExternalEmissivity = 0.0D0 ! Emissivity of the external surface of windows [-]
      REAL(KIND(1D0)) :: WindowInternalEmissivity = 0.0D0 ! Emissivity of the internal surface of windows [-]
      REAL(KIND(1D0)) :: WindowTransmissivity = 0.0D0 ! Transmissivity of windows [-]
      REAL(KIND(1D0)) :: WindowAbsorbtivity = 0.0D0 ! Absorbtivity of windows [-]
      REAL(KIND(1D0)) :: WindowReflectivity = 0.0D0 ! Reflectivity of the external surface of windows [-]
      REAL(KIND(1D0)) :: InternalMassDensity = 0.0D0 ! Effective density of the internal mass [kg m-3]
      REAL(KIND(1D0)) :: InternalMassCp = 0.0D0 ! Specific heat capacity of internal mass [J kg-1 K-1]
      REAL(KIND(1D0)) :: InternalMassEmissivity = 0.0D0 ! Emissivity of internal mass [-]
      REAL(KIND(1D0)) :: MaxHeatingPower = 0.0D0 ! Maximum power demand of heating system [W]
      REAL(KIND(1D0)) :: WaterTankWaterVolume = 0.0D0 ! Volume of water in hot water tank [m3]
      REAL(KIND(1D0)) :: MaximumHotWaterHeatingPower = 0.0D0 ! Maximum power demand of water heating system [W]
      REAL(KIND(1D0)) :: HeatingSetpointTemperature = 0.0D0 ! Heating setpoint temperature [degC]
      REAL(KIND(1D0)) :: CoolingSetpointTemperature = 0.0D0 ! Cooling setpoint temperature [degC]
      ! flag for iteration safety - YES - as we this should be updated every iteration
      LOGICAL :: iter_safe = .TRUE.
   END TYPE BUILDING_ARCHETYPE_PRM

   TYPE, PUBLIC :: STEBBS_PRM
      ! Collect general parameters for STEBBS
      REAL(KIND(1D0)) :: WallInternalConvectionCoefficient = 0.0D0 ! Internal convection coefficient of walls  [W m-2 K-1]
      REAL(KIND(1D0)) :: RoofInternalConvectionCoefficient = 0.0D0 ! Internal convection coefficient of roof [W m-2 K-1]
      REAL(KIND(1D0)) :: InternalMassConvectionCoefficient = 0.0D0 ! Convection coefficient of internal mass [W m-2 K-1]
      REAL(KIND(1D0)) :: FloorInternalConvectionCoefficient = 0.0D0 ! Internal convection coefficient of ground floor [W m-2 K-1]
      REAL(KIND(1D0)) :: WindowInternalConvectionCoefficient = 0.0D0 ! Internal convection coefficient of windows [W m-2 K-1]
      REAL(KIND(1D0)) :: WallExternalConvectionCoefficient = 0.0D0 ! Initial external convection coefficient of walls [W m-2 K-1]
      REAL(KIND(1D0)) :: RoofExternalConvectionCoefficient = 0.0D0 ! Initial external convection coefficient of roof [W m-2 K-1]
      REAL(KIND(1D0)) :: WindowExternalConvectionCoefficient = 0.0D0 ! Initial external convection coefficient of windows [W m-2 K-1]
      REAL(KIND(1D0)) :: GroundDepth = 0.0D0 ! Depth of external ground (deep soil) [m]
      REAL(KIND(1D0)) :: ExternalGroundConductivity = 0.0D0
      REAL(KIND(1D0)) :: IndoorAirDensity = 0.0D0 ! Density of indoor air [kg m-3]
      REAL(KIND(1D0)) :: IndoorAirCp = 0.0D0 ! Specific heat capacity of indoor air [J kg-1 K-1]
      !REAL(KIND(1D0)) :: WallBuildingViewFactor = 0.0D0 ! Building view factor of external walls [-]
      !REAL(KIND(1D0)) :: WallGroundViewFactor = 0.0D0 ! Ground view factor of external walls [-]
      !REAL(KIND(1D0)) :: WallSkyViewFactor = 0.0D0 ! Sky view factor of external roofs [-]
      !REAL(KIND(1D0)) :: RoofBuildingViewFactor = 0.0D0 ! Building view factor of external roofs [-]
      !REAL(KIND(1D0)) :: RoofGroundViewFactor = 0.0D0 ! Ground view factor of external roofs [-]
      !REAL(KIND(1D0)) :: RoofSkyViewFactor = 0.0D0 ! Sky view factor of external roofs [-]
      REAL(KIND(1D0)) :: MetabolicRate = 0.0D0 ! Metabolic rate of building occupants [W]
      REAL(KIND(1D0)) :: LatentSensibleRatio = 0.0D0 ! Latent-to-sensible ratio of metabolic energy release of occupants [-]
      REAL(KIND(1D0)) :: ApplianceRating = 0.0D0 ! Power demand of single appliance [W]
      REAL(KIND(1D0)) :: TotalNumberofAppliances = 0.0D0 ! Number of appliances present in building [-]
      REAL(KIND(1D0)) :: ApplianceUsageFactor = 0.0D0 ! Number of appliances in use [-]
      REAL(KIND(1D0)) :: HeatingSystemEfficiency = 0.0D0 ! Efficiency of space heating system [-]
      REAL(KIND(1D0)) :: MaxCoolingPower = 0.0D0 ! Maximum power demand of cooling system [W]
      REAL(KIND(1D0)) :: CoolingSystemCOP = 0.0D0 ! Coefficient of performance of cooling system [-]
      REAL(KIND(1D0)) :: VentilationRate = 0.0D0 ! Ventilation rate (air changes per hour, ACH) [h-1]
      REAL(KIND(1D0)) :: WaterTankWallThickness = 0.0D0 ! Hot water tank wall thickness [m]
      REAL(KIND(1D0)) :: WaterTankSurfaceArea = 0.0D0 ! Surface area of hot water tank cylinder [m2]
      REAL(KIND(1D0)) :: HotWaterHeatingSetpointTemperature = 0.0D0 ! Water tank setpoint temperature [degC]
      REAL(KIND(1D0)) :: HotWaterTankWallEmissivity = 0.0D0 ! Effective external wall emissivity of the hot water tank [-]
      REAL(KIND(1D0)) :: DHWVesselWallThickness = 0.0D0 ! Hot water vessel wall thickness [m]
      REAL(KIND(1D0)) :: DHWWaterVolume = 0.0D0 ! Volume of water held in use in building [m3]
      REAL(KIND(1D0)) :: DHWSurfaceArea = 0.0D0 ! Surface area of hot water in vessels in building [m2]
      REAL(KIND(1D0)) :: HotWaterFlowRate = 0.0D0 ! Hot water flow rate from tank to vessel [m3 s-1]
      REAL(KIND(1D0)) :: DHWDrainFlowRate = 0.0D0 ! Flow rate of hot water held in building to drain [m3 s-1]
      REAL(KIND(1D0)) :: DHWSpecificHeatCapacity = 0.0D0 ! Specific heat capacity of hot water [J kg-1 K-1]
      REAL(KIND(1D0)) :: HotWaterTankSpecificHeatCapacity = 0.0D0 ! Specific heat capacity of hot water tank wal [J kg-1 K-1]
      REAL(KIND(1D0)) :: DHWVesselSpecificHeatCapacity = 0.0D0 ! Specific heat capacity of vessels containing hot water in use in buildings [J kg-1 K-1]
      REAL(KIND(1D0)) :: DHWDensity = 0.0D0 ! Density of hot water in use [kg m-3]
      REAL(KIND(1D0)) :: HotWaterTankWallDensity = 0.0D0 ! Density of hot water tank wall [kg m-3]
      REAL(KIND(1D0)) :: DHWVesselDensity = 0.0D0 ! Density of vessels containing hot water in use [kg m-3]
      REAL(KIND(1D0)) :: HotWaterTankBuildingWallViewFactor = 0.0D0 ! Water tank/vessel internal building wall/roof view factor [-]
      REAL(KIND(1D0)) :: HotWaterTankInternalMassViewFactor = 0.0D0 ! Water tank/vessel building internal mass view factor [-]
      REAL(KIND(1D0)) :: HotWaterTankWallConductivity = 0.0D0 ! Effective wall conductivity of the hot water tank [W m-1 K-1]
      REAL(KIND(1D0)) :: HotWaterTankInternalWallConvectionCoefficient = 0.0D0 ! Effective internal wall convection coefficient of the hot water tank [W m-2 K-1]
      REAL(KIND(1D0)) :: HotWaterTankExternalWallConvectionCoefficient = 0.0D0 ! Effective external wall convection coefficient of the hot water tank [W m-2 K-1]
      REAL(KIND(1D0)) :: DHWVesselWallConductivity = 0.0D0 ! Effective wall conductivity of the hot water tank [W m-1 K-1]
      REAL(KIND(1D0)) :: DHWVesselInternalWallConvectionCoefficient = 0.0D0 ! Effective internal wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]
      REAL(KIND(1D0)) :: DHWVesselExternalWallConvectionCoefficient = 0.0D0 ! Effective external wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]
      REAL(KIND(1D0)) :: DHWVesselWallEmissivity = 0.0D0 ! Effective external wall emissivity of hot water being used within building [-]
      REAL(KIND(1D0)) :: HotWaterHeatingEfficiency = 0.0D0 ! Efficiency of hot water system [-]
      REAL(KIND(1D0)) :: MinimumVolumeOfDHWinUse = 0.0D0 ! Minimum volume of hot water in use [m3]
      ! flag for iteration safety - YES - as we this should be updated every iteration
      LOGICAL :: iter_safe = .TRUE.
   END TYPE STEBBS_PRM

    TYPE :: STEBBS_BLDG
      ! MP TODO: Add initialisation values e.g. =0
      CHARACTER(len=256) :: BuildingType = 'Default'
      CHARACTER(len=256) :: BuildingName= 'Default'
      CHARACTER(len=256) :: fnmlLBM = 'Default'
      CHARACTER(len=256) :: CASE = 'Default'
      INTEGER :: idLBM = 0
      INTEGER :: appliance_totalnumber = 0

      REAL(KIND(1D0)) :: QHload_heating_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QHload_cooling_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QH_metabolism = 0.0D0
      REAL(KIND(1D0)) :: QE_metabolism = 0.0D0
      REAL(KIND(1D0)) :: Qtotal_water_tank = 0.0D0
      REAL(KIND(1D0)) :: qhwtDrain = 0.0D0
      REAL(KIND(1D0)) :: ratio_window_wall = 0.0D0
      REAL(KIND(1D0)) :: Afootprint = 0.0D0
      REAL(KIND(1D0)) :: height_building = 0.0D0
      REAL(KIND(1D0)) :: wallExternalArea = 0.0D0
      REAL(KIND(1D0)) :: ratioInternalVolume = 0.0D0
      REAL(KIND(1D0)) :: thickness_wall = 0.0D0
      REAL(KIND(1D0)) :: thickness_wallext = 0.0D0
      REAL(KIND(1D0)) :: thickness_roof = 0.0D0
      REAL(KIND(1D0)) :: thickness_roofext = 0.0D0
      REAL(KIND(1D0)) :: thickness_groundfloor = 0.0D0
      REAL(KIND(1D0)) :: depth_ground = 0.0D0
      REAL(KIND(1D0)) :: thickness_window = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_intwall = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_introof = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_indoormass = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_intgroundfloor = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_intwindow = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_extwall = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_extroof = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_extwindow = 0.0D0
      REAL(KIND(1D0)) :: conductivity_wall = 0.0D0
      REAL(KIND(1D0)) :: conductivity_wallext = 0.0D0
      REAL(KIND(1D0)) :: conductivity_roof = 0.0D0
      REAL(KIND(1D0)) :: conductivity_roofext = 0.0D0
      REAL(KIND(1D0)) :: conductivity_groundfloor = 0.0D0
      REAL(KIND(1D0)) :: conductivity_window = 0.0D0
      REAL(KIND(1D0)) :: conductivity_ground = 0.0D0
      REAL(KIND(1D0)) :: density_wall = 0.0D0
      REAL(KIND(1D0)) :: density_wallext = 0.0D0
      REAL(KIND(1D0)) :: density_roof = 0.0D0
      REAL(KIND(1D0)) :: density_roofext = 0.0D0
      REAL(KIND(1D0)) :: weighting_factor_heatcapacity_wall = 0.0D0
      REAL(KIND(1D0)) :: weighting_factor_heatcapacity_roof = 0.0D0
      REAL(KIND(1D0)) :: density_groundfloor = 0.0D0
      REAL(KIND(1D0)) :: density_window = 0.0D0
      REAL(KIND(1D0)) :: density_indoormass = 0.0D0
      REAL(KIND(1D0)) :: density_air_ind = 0.0D0
      REAL(KIND(1D0)) :: cp_wall = 0.0D0
      REAL(KIND(1D0)) :: cp_wallext = 0.0D0
      REAL(KIND(1D0)) :: cp_roof = 0.0D0
      REAL(KIND(1D0)) :: cp_roofext = 0.0D0
      REAL(KIND(1D0)) :: cp_groundfloor = 0.0D0
      REAL(KIND(1D0)) :: cp_window = 0.0D0
      REAL(KIND(1D0)) :: cp_indoormass = 0.0D0
      REAL(KIND(1D0)) :: cp_air_ind = 0.0D0
      REAL(KIND(1D0)) :: emissivity_extwall = 0.0D0
      REAL(KIND(1D0)) :: emissivity_extroof = 0.0D0
      REAL(KIND(1D0)) :: emissivity_intwall = 0.0D0
      REAL(KIND(1D0)) :: emissivity_introof = 0.0D0
      REAL(KIND(1D0)) :: emissivity_indoormass = 0.0D0
      REAL(KIND(1D0)) :: emissivity_extwindow = 0.0D0
      REAL(KIND(1D0)) :: emissivity_intwindow = 0.0D0
      REAL(KIND(1D0)) :: windowTransmissivity = 0.0D0
      REAL(KIND(1D0)) :: windowAbsorbtivity = 0.0D0
      REAL(KIND(1D0)) :: windowReflectivity = 0.0D0
      REAL(KIND(1D0)) :: wallTransmisivity = 0.0D0
      REAL(KIND(1D0)) :: wallAbsorbtivity = 0.0D0
      REAL(KIND(1D0)) :: wallReflectivity = 0.0D0
      REAL(KIND(1D0)) :: roofTransmisivity = 0.0D0
      REAL(KIND(1D0)) :: roofAbsorbtivity = 0.0D0
      REAL(KIND(1D0)) :: roofReflectivity = 0.0D0
      !REAL(KIND(1D0)) :: BVF_extwall = 0.0D0
      !REAL(KIND(1D0)) :: GVF_extwall = 0.0D0
      !REAL(KIND(1D0)) :: SVF_extwall = 0.0D0
      !REAL(KIND(1D0)) :: BVF_extroof = 0.0D0
      !REAL(KIND(1D0)) :: GVF_extroof = 0.0D0
      !REAL(KIND(1D0)) :: SVF_extroof = 0.0D0
      REAL(KIND(1D0)) :: occupants = 0.0D0
      REAL(KIND(1D0)) :: metabolic_rate = 0.0D0
      REAL(KIND(1D0)) :: ratio_metabolic_latent_sensible = 0.0D0
      REAL(KIND(1D0)) :: appliance_power_rating = 0.0D0
      REAL(KIND(1D0)) :: appliance_usage_factor = 0.0D0
      REAL(KIND(1D0)) :: maxheatingpower_air = 0.0D0
      REAL(KIND(1D0)) :: heating_efficiency_air = 0.0D0
      REAL(KIND(1D0)) :: maxcoolingpower_air = 0.0D0
      REAL(KIND(1D0)) :: coeff_performance_cooling = 0.0D0
      REAL(KIND(1D0)) :: Vair_ind = 0.0D0
      REAL(KIND(1D0)) :: ventilation_rate = 0.0D0
      REAL(KIND(1D0)) :: Awall = 0.0D0
      REAL(KIND(1D0)) :: Aroof = 0.0D0
      REAL(KIND(1D0)) :: Vwall = 0.0D0
      REAL(KIND(1D0)) :: Vroof = 0.0D0
      REAL(KIND(1D0)) :: Vgroundfloor = 0.0D0
      REAL(KIND(1D0)) :: Awindow = 0.0D0
      REAL(KIND(1D0)) :: Vwindow = 0.0D0
      REAL(KIND(1D0)) :: Vindoormass = 0.0D0
      REAL(KIND(1D0)) :: Aindoormass = 0.0D0
      REAL(KIND(1D0)) :: Tair_ind = 0.0D0
      REAL(KIND(1D0)) :: Tindoormass = 0.0D0
      REAL(KIND(1D0)) :: Tintwall = 0.0D0
      REAL(KIND(1D0)) :: Tintroof = 0.0D0
      REAL(KIND(1D0)) :: Textwall = 0.0D0
      REAL(KIND(1D0)) :: Textroof = 0.0D0
      REAL(KIND(1D0)) :: Tintwindow = 0.0D0
      REAL(KIND(1D0)) :: Textwindow = 0.0D0
      REAL(KIND(1D0)) :: Tintgroundfloor = 0.0D0
      REAL(KIND(1D0)) :: Textgroundfloor = 0.0D0
      REAL(KIND(1D0)) :: Twater_tank = 0.0D0
      REAL(KIND(1D0)) :: Tintwall_tank = 0.0D0
      REAL(KIND(1D0)) :: Textwall_tank = 0.0D0
      REAL(KIND(1D0)) :: thickness_tankwall = 0.0D0
      REAL(KIND(1D0)) :: Tincomingwater_tank = 0.0D0
      REAL(KIND(1D0)) :: Vwater_tank = 0.0D0
      REAL(KIND(1D0)) :: Asurf_tank = 0.0D0
      REAL(KIND(1D0)) :: Vwall_tank = 0.0D0
      REAL(KIND(1D0)) :: setTwater_tank = 0.0D0
      REAL(KIND(1D0)) :: init_wtTs = 0.0D0
      REAL(KIND(1D0)) :: Twater_vessel = 0.0D0
      REAL(KIND(1D0)) :: Tintwall_vessel = 0.0D0
      REAL(KIND(1D0)) :: Textwall_vessel = 0.0D0
      REAL(KIND(1D0)) :: thickness_wall_vessel = 0.0D0
      REAL(KIND(1D0)) :: Vwater_vessel = 0.0D0
      REAL(KIND(1D0)) :: Awater_vessel = 0.0D0
      REAL(KIND(1D0)) :: Vwall_vessel = 0.0D0
      REAL(KIND(1D0)) :: flowrate_water_supply = 0.0D0
      REAL(KIND(1D0)) :: flowrate_water_drain = 0.0D0
      REAL(KIND(1D0)) :: single_flowrate_water_supply = 0.0D0
      REAL(KIND(1D0)) :: single_flowrate_water_drain = 0.0D0
      REAL(KIND(1D0)) :: cp_water = 0.0D0
      REAL(KIND(1D0)) :: cp_wall_tank = 0.0D0
      REAL(KIND(1D0)) :: cp_wall_vessel = 0.0D0
      REAL(KIND(1D0)) :: density_water = 0.0D0
      REAL(KIND(1D0)) :: density_wall_tank = 0.0D0
      REAL(KIND(1D0)) :: density_wall_vessel = 0.0D0
      REAL(KIND(1D0)) :: BVF_tank = 0.0D0
      REAL(KIND(1D0)) :: MVF_tank = 0.0D0
      REAL(KIND(1D0)) :: conductivity_wall_tank = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_intwall_tank = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_extwall_tank = 0.0D0
      REAL(KIND(1D0)) :: emissivity_extwall_tank = 0.0D0
      REAL(KIND(1D0)) :: conductivity_wall_vessel = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_intwall_vessel = 0.0D0
      REAL(KIND(1D0)) :: conv_coeff_extwall_vessel = 0.0D0
      REAL(KIND(1D0)) :: emissivity_extwall_vessel = 0.0D0
      REAL(KIND(1D0)) :: maxheatingpower_water = 0.0D0
      REAL(KIND(1D0)) :: heating_efficiency_water = 0.0D0
      REAL(KIND(1D0)) :: minVwater_vessel = 0.0D0
      REAL(KIND(1D0)) :: minHeatingPower_DHW = 0.0D0
      REAL(KIND(1D0)) :: HeatingPower_DHW = 0.0D0

      REAL(KIND(1D0)) :: qfm_dom = 0.0D0 ! Metabolic sensible and latent heat
      REAL(KIND(1D0)) :: qheat_dom = 0.0D0  ! Hourly heating load  [W]
      REAL(KIND(1D0)) :: qcool_dom = 0.0D0  ! Hourly cooling load  [W]
      REAL(KIND(1D0)) :: qfb_hw_dom = 0.0D0  ! Hot water
      REAL(KIND(1D0)) :: qfb_dom_air = 0.0D0  ! Sensible heat to air [W]
      REAL(KIND(1D0)) :: dom_temp = 0.0D0  ! Domain temperature   [W]
      REAL(KIND(1D0)) :: QStar = 0.0D0  ! Net radiation        [W m-2]
      REAL(KIND(1D0)) :: QEC = 0.0D0  ! Energy use           [W m-2]
      REAL(KIND(1D0)) :: QH = 0.0D0  ! Sensible heat flux   [W m-2]
      REAL(KIND(1D0)) :: QS = 0.0D0  ! Storage heat flux    [W m-2]
      REAL(KIND(1D0)) :: QBAE = 0.0D0  ! Building exchange    [W m-2]
      REAL(KIND(1D0)) :: QWaste = 0.0D0  ! Waste heating        [W m-2]

      REAL(KIND(1D0)), DIMENSION(2) :: Ts, initTs = 0.0D0
      REAL(KIND(1D0)), DIMENSION(5) :: h_i, k_eff = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: h_o = 0.0D0
      REAL(KIND(1D0)), DIMENSION(6) :: rho = 0.0D0
      REAL(KIND(1D0)), DIMENSION(6) :: Cp = 0.0D0
      REAL(KIND(1D0)), DIMENSION(7) :: emis = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: wiTAR, waTAR, roofTAR = 0.0D0
      !REAL(KIND(1D0)), DIMENSION(6) :: viewFactors = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: occupantData = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: HTsAverage, HWTsAverage = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: HWPowerAverage = 0.0D0
      REAL(KIND(1D0)), DIMENSION(40) :: EnergyExchanges = 0.0D0

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Textwall_C  ! Wall external surface temperature from STEBBS[K]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Textroof_C ! Roof external surface temperature from STEBBS[K]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.

   CONTAINS
   PROCEDURE :: ALLOCATE => allocSTEBBS_bldgState
   PROCEDURE :: DEALLOCATE => deallocSTEBBS_bldgState

   END TYPE STEBBS_BLDG

   TYPE, PUBLIC :: STEBBS_STATE

      ! Beers output for STEBBS - TODO: these should be kept in the HEAT_STATE type -
      REAL(KIND(1D0)) :: Kdown2d = 0.0D0 ! incoming shortwave radiation onto roof [W m-2]
      REAL(KIND(1D0)) :: Kup2d = 0.0D0 ! outgoing shortwave radiation from roof [W m-2]
      REAL(KIND(1D0)) :: Kwest = 0.0D0 ! incoming shortwave radiation from west [W m-2]
      REAL(KIND(1D0)) :: Ksouth = 0.0D0 ! incoming shortwave radiation from south [W m-2]
      REAL(KIND(1D0)) :: Knorth = 0.0D0 ! incoming shortwave radiation from north [W m-2]
      REAL(KIND(1D0)) :: Keast = 0.0D0 ! incoming shortwave radiation from east [W m-2]
      REAL(KIND(1D0)) :: Ldown2d = 0.0D0 ! incoming longwave radiation onto roof [W m-2]
      REAL(KIND(1D0)) :: Lup2d = 0.0D0 ! outgoing longwave radiation from roof [W m-2]
      REAL(KIND(1D0)) :: Lwest = 0.0D0 ! incoming longwave radiation from west [W m-2]
      REAL(KIND(1D0)) :: Lsouth = 0.0D0 ! incoming longwave radiation from south [W m-2]
      REAL(KIND(1D0)) :: Lnorth = 0.0D0 ! incoming longwave radiation from north [W m-2]
      REAL(KIND(1D0)) :: Least = 0.0D0 ! incoming longwave radiation from east [W m-2]
      REAL(KIND(1D0)), DIMENSION(30) :: zarray = -999 !RSL layer heights
      REAL(KIND(1D0)), DIMENSION(30) :: dataoutLineURSL = -999 ! wind speed array from RSL [m s-1]
      REAL(KIND(1D0)), DIMENSION(30) :: dataoutLineTRSL = -999 ! Temperature array from RSL[C]
      REAL(KIND(1D0)), DIMENSION(30) :: dataoutLineqRSL = -999 ! Specific humidity array from RSL[g kg-1]
      ! Initial conditions that are updated during runtime
      REAL(KIND(1D0)) :: DeepSoilTemperature = 0.0D0 ! Deep soil temperature [C]
      REAL(KIND(1D0)) :: OutdoorAirStartTemperature = 0.0D0 ! Initial outdoor air temperature [degC]
      REAL(KIND(1D0)) :: IndoorAirStartTemperature = 0.0D0 ! Initial indoor air temperature [degC]
      REAL(KIND(1D0)) :: IndoorMassStartTemperature = 0.0D0 ! Initial indoor mass temperature [degC]
      REAL(KIND(1D0)) :: WallIndoorSurfaceTemperature = 0.0D0 ! Initial wall indoor surface temperature [degC]
      REAL(KIND(1D0)) :: WallOutdoorSurfaceTemperature = 0.0D0 ! Initial walloutdoor surface temperature [degC]
      REAL(KIND(1D0)) :: RoofIndoorSurfaceTemperature = 0.0D0 ! Initial roof indoor surface temperature [degC]
      REAL(KIND(1D0)) :: RoofOutdoorSurfaceTemperature = 0.0D0 ! Initial roof outdoor surface temperature [degC]
      REAL(KIND(1D0)) :: WindowIndoorSurfaceTemperature = 0.0D0 ! Initial window indoor surface temperature [degC]
      REAL(KIND(1D0)) :: WindowOutdoorSurfaceTemperature = 0.0D0 ! Initial window outdoor surface temperature [degC]
      REAL(KIND(1D0)) :: GroundFloorIndoorSurfaceTemperature = 0.0D0 ! Initial ground floor indoor surface temperature [degC]
      REAL(KIND(1D0)) :: GroundFloorOutdoorSurfaceTemperature = 0.0D0 ! Initial ground floor outdoor surface temperature [degC]
      REAL(KIND(1D0)) :: WaterTankTemperature = 0.0D0 ! Initial water temperature in hot water tank [degC]
      REAL(KIND(1D0)) :: InternalWallWaterTankTemperature = 0.0D0 ! Initial hot water tank internal wall temperature [degC]
      REAL(KIND(1D0)) :: ExternalWallWaterTankTemperature = 0.0D0 ! Initial hot water tank external wall temperature [degC]
      REAL(KIND(1D0)) :: MainsWaterTemperature = 0.0D0 ! Temperature of water coming into the water tank [degC]
      REAL(KIND(1D0)) :: DomesticHotWaterTemperatureInUseInBuilding = 0.0D0 ! Initial water temperature of water held in use in building [degC]
      REAL(KIND(1D0)) :: InternalWallDHWVesselTemperature = 0.0D0 ! Initial hot water vessel internal wall temperature [degC]
      REAL(KIND(1D0)) :: ExternalWallDHWVesselTemperature = 0.0D0 ! Initial hot water vessel external wall temperature [degC]
      REAL(KIND(1D0)) :: QS_stebbs = 0.0D0 ! storage heat flux per footprint area[W m-2]
      TYPE(STEBBS_BLDG), ALLOCATABLE, DIMENSION(:) :: buildings ! Array holding all buildings states for STEBBS [-]

      !REAL(KIND(1D0)), DIMENSION(6) :: Textwall_C = 0.0D0 ! Wall external surface temperature from STEBBS[K]
      !REAL(KIND(1D0)), DIMENSION(6) :: Textroof_C = 0.0D0! Roof external surface temperature from STEBBS[K]
      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.

   CONTAINS
      PROCEDURE :: ALLOCATE => allocSTEBBS_bldg
      PROCEDURE :: DEALLOCATE => deallocSTEBBS_bldg

   END TYPE STEBBS_STATE

   TYPE, PUBLIC :: NHOOD_STATE

      REAL(KIND(1D0)) :: U_hbh_1dravg = 0.0D0 ! 24hr running average wind speed at half building height [m s-1]
      REAL(KIND(1D0)) :: QN_1dravg = 0.0D0 ! 24hr running average net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: Tair_mn_prev = 0.0D0 ! Previous midnight air temperature [degC]
      REAL(KIND(1D0)) :: iter_count = 0.0D0 ! iteration count of convergence loop [-]

      ! flag for iteration safety - NO
      ! iter_count is used to count the number of iterations and thus cannot be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.

   END TYPE NHOOD_STATE

   contains


   SUBROUTINE allocSTEBBS_bldgState(self, num_layer)
      IMPLICIT NONE

      CLASS(STEBBS_BLDG), INTENT(INOUT) :: self
      INTEGER, INTENT(IN) :: num_layer

      CALL self%DEALLOCATE()
      ! ALLOCATE (self%buildings(ntypes))
      ALLOCATE (self%Textroof_C(num_layer))
      ALLOCATE (self%Textwall_C(num_layer))

   END SUBROUTINE allocSTEBBS_bldgState

   SUBROUTINE deallocSTEBBS_bldgState(self)
      IMPLICIT NONE

      CLASS(STEBBS_BLDG), INTENT(INOUT) :: self
      ! IF (ALLOCATED(self%buildings)) DEALLOCATE (self%buildings)
      IF (ALLOCATED(self%Textroof_C)) DEALLOCATE (self%Textroof_C)
      IF (ALLOCATED(self%Textwall_C)) DEALLOCATE (self%Textwall_C)

   END SUBROUTINE deallocSTEBBS_bldgState



   SUBROUTINE allocSTEBBS_bldg(self, ntypes, num_layer)
      IMPLICIT NONE

      CLASS(STEBBS_STATE), INTENT(INOUT) :: self
      INTEGER, INTENT(IN) :: ntypes, num_layer

      CALL self%DEALLOCATE()
      ALLOCATE (self%buildings(ntypes))

   END SUBROUTINE allocSTEBBS_bldg

   SUBROUTINE deallocSTEBBS_bldg(self)
      IMPLICIT NONE

      CLASS(STEBBS_STATE), INTENT(INOUT) :: self
      IF (ALLOCATED(self%buildings)) DEALLOCATE (self%buildings)

   END SUBROUTINE deallocSTEBBS_bldg

END MODULE module_type_stebbs