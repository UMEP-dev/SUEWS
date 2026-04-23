module module_type_stebbs

    ! use module_ctrl_type

    implicit none

    TYPE, PUBLIC :: BUILDING_ARCHETYPE_PRM
      ! This type is used to collect building archetypes for STEBBS
      ! CHARACTER(LEN=50) :: BuildingCode !
      ! CHARACTER(LEN=50) :: BuildingClass !
      ! CHARACTER(LEN=50) :: building_type !
      ! CHARACTER(LEN=50) :: building_name !
      REAL(KIND(1D0)) :: building_count = 0.0D0 ! Number of buildings of this archetype [-]
      REAL(KIND(1D0)) :: occupants_state = 0.0D0 ! Maixmum number of occupants in building [-]
      REAL(KIND(1D0)) :: hhs0 = 0.0D0 !
      REAL(KIND(1D0)) :: age_0_4 = 0.0D0 !
      REAL(KIND(1D0)) :: age_5_11 = 0.0D0 !
      REAL(KIND(1D0)) :: age_12_18 = 0.0D0 !
      REAL(KIND(1D0)) :: age_19_64 = 0.0D0 !
      REAL(KIND(1D0)) :: age_65plus = 0.0D0 !
      REAL(KIND(1D0)) :: stebbs_Height = 0.0D0 ! Building height. This should be consistent with WallExternalArea and FootprintArea. [m]
      REAL(KIND(1D0)) :: footprint_area = 0.0D0 ! Building footprint area. This should be consistent with stebbs_Height and WallExternalArea. [m2]
      REAL(KIND(1D0)) :: wall_external_area = 0.0D0 ! External wall area (including window area). This should be consistent with stebbs_Height and FootprintArea. [m2]
      REAL(KIND(1D0)) :: internal_volume_ratio = 0.0D0 ! Ratio of internal mass volume to total building volume [-]
      REAL(KIND(1D0)) :: internal_mass_area = 0.0D0 ! Surface area of internal mass used for indoor heat exchange [m2]
      REAL(KIND(1D0)) :: WWR = 0.0D0 ! window to wall ratio [-]
      REAL(KIND(1D0)) :: wall_thickness = 0.0D0 ! Thickness of external wall [m]
      REAL(KIND(1D0)) :: wall_effective_conductivity = 0.0D0 ! Effective thermal conductivity of walls [W m-1 K-1]
      REAL(KIND(1D0)) :: wall_density = 0.0D0 ! Effective density of the walls [kg m-3]
      REAL(KIND(1D0)) :: wall_specific_heat_capacity = 0.0D0 ! Effective specific heat capacity of walls [J kg-1 K-1]
      REAL(KIND(1D0)) :: wall_external_thickness = 0.0D0 ! Thickness of layers external to insulation in external wall [m]
      REAL(KIND(1D0)) :: wall_external_effective_conductivity = 0.0D0 ! Effective thermal conductivity of layers external to insulation inof walls [W m-1 K-1]
      REAL(KIND(1D0)) :: wall_external_density = 0.0D0 ! Effective density of layers external to insulation in the walls [kg m-3]
      REAL(KIND(1D0)) :: wall_external_specific_heat_capacity = 0.0D0 ! Effective specific heat capacity of layers external to insulation in walls [J kg-1 K-1]
      REAL(KIND(1D0)) :: wall_outer_heat_capacity_fraction = 0.0D0 ! Weighting factor for heat capacity of walls [-]
      REAL(KIND(1D0)) :: wall_external_emissivity = 0.0D0 ! Emissivity of the external surface of walls [-]
      REAL(KIND(1D0)) :: wall_internal_emissivity = 0.0D0 ! Emissivity of the internal surface of walls [-]
      REAL(KIND(1D0)) :: wall_transmissivity = 0.0D0 ! Transmissivity of walls [-]
      REAL(KIND(1D0)) :: wall_absorptivity = 0.0D0 ! Absorbtivity of walls [-]
      REAL(KIND(1D0)) :: wall_reflectivity = 0.0D0 ! Reflectivity of the external surface of walls [-]
      REAL(KIND(1D0)) :: roof_thickness = 0.0D0 ! Thickness of external roof [m]
      REAL(KIND(1D0)) :: roof_effective_conductivity = 0.0D0 ! Effective thermal conductivity of roofs (weighted) [W m-1 K-1]
      REAL(KIND(1D0)) :: roof_density = 0.0D0 ! Effective density of the roof  [kg m-3]
      REAL(KIND(1D0)) :: roof_specific_heat_capacity = 0.0D0 ! Effective specific heat capacity of layers external to insulation in roof  [J kg-1 K-1]
      REAL(KIND(1D0)) :: roof_external_thickness = 0.0D0 ! Thickness of external  layers external to insulation roof [m]
      REAL(KIND(1D0)) :: roof_external_effective_conductivity = 0.0D0 ! Effective thermal conductivity of  layers external to insulation roofs (weighted) [W m-1 K-1]
      REAL(KIND(1D0)) :: roof_external_density = 0.0D0 ! Effective density of the  layers external to insulation roof [kg m-3]
      REAL(KIND(1D0)) :: roof_external_specific_heat_capacity = 0.0D0 ! Effective specific heat capacity of layers external to insulation in roof  [J kg-1 K-1]
      REAL(KIND(1D0)) :: roof_outer_heat_capacity_fraction = 0.0D0 ! Weighting factor for heat capacity of roof [-]
      REAL(KIND(1D0)) :: roof_external_emissivity = 0.0D0 ! Emissivity of the external surface of roof [-]
      REAL(KIND(1D0)) :: roof_internal_emissivity = 0.0D0 ! Emissivity of the internal surface of roof [-]
      REAL(KIND(1D0)) :: roof_transmissivity = 0.0D0 ! Transmissivity of walls roof [-]
      REAL(KIND(1D0)) :: roof_absorptivity = 0.0D0 ! Absorbtivity of walls roof [-]
      REAL(KIND(1D0)) :: roof_reflectivity = 0.0D0 ! Reflectivity of the external surface of roof [-]
      REAL(KIND(1D0)) :: ground_floor_thickness = 0.0D0 ! Thickness of ground floor [m]
      REAL(KIND(1D0)) :: ground_floor_effective_conductivity = 0.0D0 ! Effective thermal conductivity of ground floor [W m-1 K-1]
      REAL(KIND(1D0)) :: ground_floor_density = 0.0D0 ! Density of the ground floor [kg m-3]
      REAL(KIND(1D0)) :: ground_floor_specific_heat_capacity = 0.0D0 ! Effective specific heat capacity of the ground floor [J kg-1 K-1]
      REAL(KIND(1D0)) :: window_thickness = 0.0D0 ! Window thickness [m]
      REAL(KIND(1D0)) :: window_effective_conductivity = 0.0D0 ! Effective thermal conductivity of windows [W m-1 K-1]
      REAL(KIND(1D0)) :: window_density = 0.0D0 ! Effective density of the windows [kg m-3]
      REAL(KIND(1D0)) :: window_specific_heat_capacity = 0.0D0 ! Effective specific heat capacity of windows [J kg-1 K-1]
      REAL(KIND(1D0)) :: window_external_emissivity = 0.0D0 ! Emissivity of the external surface of windows [-]
      REAL(KIND(1D0)) :: window_internal_emissivity = 0.0D0 ! Emissivity of the internal surface of windows [-]
      REAL(KIND(1D0)) :: window_transmissivity = 0.0D0 ! Transmissivity of windows [-]
      REAL(KIND(1D0)) :: window_absorptivity = 0.0D0 ! Absorbtivity of windows [-]
      REAL(KIND(1D0)) :: window_reflectivity = 0.0D0 ! Reflectivity of the external surface of windows [-]
      REAL(KIND(1D0)) :: internal_mass_density = 0.0D0 ! Effective density of the internal mass [kg m-3]
      REAL(KIND(1D0)) :: internal_mass_specific_heat_capacity = 0.0D0 ! Specific heat capacity of internal mass [J kg-1 K-1]
      REAL(KIND(1D0)) :: internal_mass_emissivity = 0.0D0 ! Emissivity of internal mass [-]
      REAL(KIND(1D0)) :: max_heating_power = 0.0D0 ! Maximum power demand of heating system [W]
      REAL(KIND(1D0)) :: hot_water_tank_volume = 0.0D0 ! Volume of water in hot water tank [m3]
      REAL(KIND(1D0)) :: maximum_hot_water_heating_power = 0.0D0 ! Maximum power demand of water heating system [W]
      REAL(KIND(1D0)) :: heating_setpoint_temperature = 0.0D0 ! Heating setpoint temperature [degC]
      REAL(KIND(1D0)) :: cooling_setpoint_temperature = 0.0D0 ! Cooling setpoint temperature [degC]
      ! diurnal profiles of heating/cooling setpoint temperature [degC]
      REAL(KIND(1D0)), DIMENSION(0:143, 2) :: heating_setpoint_temperature_profile = 0.0D0
      REAL(KIND(1D0)), DIMENSION(0:143, 2) :: cooling_setpoint_temperature_profile = 0.0D0
      REAL(KIND(1D0)) :: lighting_power_density = 0.0D0 ! Lighting power per building floor area [W m-2]
      REAL(KIND(1D0)), DIMENSION(0:143, 2) :: metabolism_profile = 0.0D0 ! diurnal profiles of occupant metabolic rate [W]
      REAL(KIND(1D0)), DIMENSION(0:143, 2) :: appliance_profile = 0.0D0 ! diurnal profiles of appliance energy power [W]
      ! flag for iteration safety - YES - as we this should be updated every iteration
      LOGICAL :: iter_safe = .TRUE.
   END TYPE BUILDING_ARCHETYPE_PRM

   TYPE, PUBLIC :: STEBBS_PRM
      ! Collect general parameters for STEBBS
      REAL(KIND(1D0)) :: wall_internal_convection_coefficient = 0.0D0 ! Internal convection coefficient of walls  [W m-2 K-1]
      REAL(KIND(1D0)) :: roof_internal_convection_coefficient = 0.0D0 ! Internal convection coefficient of roof [W m-2 K-1]
      REAL(KIND(1D0)) :: internal_mass_convection_coefficient = 0.0D0 ! Convection coefficient of internal mass [W m-2 K-1]
      REAL(KIND(1D0)) :: floor_internal_convection_coefficient = 0.0D0 ! Internal convection coefficient of ground floor [W m-2 K-1]
      REAL(KIND(1D0)) :: window_internal_convection_coefficient = 0.0D0 ! Internal convection coefficient of windows [W m-2 K-1]
      REAL(KIND(1D0)) :: wall_external_convection_coefficient = 0.0D0 ! Initial external convection coefficient of walls [W m-2 K-1]
      REAL(KIND(1D0)) :: roof_external_convection_coefficient = 0.0D0 ! Initial external convection coefficient of roof [W m-2 K-1]
      REAL(KIND(1D0)) :: window_external_convection_coefficient = 0.0D0 ! Initial external convection coefficient of windows [W m-2 K-1]
      REAL(KIND(1D0)) :: ground_depth = 0.0D0 ! Depth of external ground (deep soil) [m]
      REAL(KIND(1D0)) :: external_ground_conductivity = 0.0D0
      REAL(KIND(1D0)) :: indoor_air_density = 0.0D0 ! Density of indoor air [kg m-3]
      REAL(KIND(1D0)) :: indoor_air_cp = 0.0D0 ! Specific heat capacity of indoor air [J kg-1 K-1]
      REAL(KIND(1D0)) :: metabolism_threshold = 0.0D0 ! Threshold of Metabolic rate of each occupancy for active or inactive [W]
      REAL(KIND(1D0)) :: latent_sensible_ratio = 0.0D0 ! Latent-to-sensible ratio of metabolic energy release of occupants [-]
      INTEGER :: daylight_control = 0 ! Daylight-based lighting control flag encoded as 0/1 [-]
      REAL(KIND(1D0)) :: lighting_illuminance_threshold = 300.0D0 ! Indoor illuminance threshold for switching off electric lighting [lx]
      REAL(KIND(1D0)) :: heating_system_efficiency = 0.0D0 ! Efficiency of space heating system [-]
      REAL(KIND(1D0)) :: max_cooling_power = 0.0D0 ! Maximum power demand of cooling system [W]
      REAL(KIND(1D0)) :: cooling_system_cop = 0.0D0 ! Coefficient of performance of cooling system [-]
      REAL(KIND(1D0)) :: ventilation_rate = 0.0D0 ! Ventilation rate (air changes per hour, ACH) [h-1]
      REAL(KIND(1D0)) :: hot_water_tank_wall_thickness = 0.0D0 ! Hot water tank wall thickness [m]
      REAL(KIND(1D0)) :: hot_water_tank_surface_area = 0.0D0 ! Surface area of hot water tank cylinder [m2]
      REAL(KIND(1D0)) :: hot_water_heating_setpoint_temperature = 0.0D0 ! Water tank setpoint temperature [degC]
      REAL(KIND(1D0)) :: hot_water_tank_wall_emissivity = 0.0D0 ! Effective external wall emissivity of the hot water tank [-]
      REAL(KIND(1D0)) :: hot_water_vessel_wall_thickness = 0.0D0 ! Hot water vessel wall thickness [m]
      REAL(KIND(1D0)) :: hot_water_volume = 0.0D0 ! Volume of water held in use in building [m3]
      REAL(KIND(1D0)) :: hot_water_surface_area = 0.0D0 ! Surface area of hot water in vessels in building [m2]
      REAL(KIND(1D0)) :: hot_water_flow_rate = 0.0D0 ! Hot water flow rate from tank to vessel [m3 s-1]
      REAL(KIND(1D0)), DIMENSION(0:143, 2) :: hot_water_flow_profile = 0.0D0 ! Diurnal profile of domestic hot water usage [m3 s-1]
      REAL(KIND(1D0)) :: hot_water_specific_heat_capacity = 0.0D0 ! Specific heat capacity of hot water [J kg-1 K-1]
      REAL(KIND(1D0)) :: hot_water_tank_specific_heat_capacity = 0.0D0 ! Specific heat capacity of hot water tank wal [J kg-1 K-1]
      REAL(KIND(1D0)) :: hot_water_vessel_specific_heat_capacity = 0.0D0 ! Specific heat capacity of vessels containing hot water in use in buildings [J kg-1 K-1]
      REAL(KIND(1D0)) :: hot_water_density = 0.0D0 ! Density of hot water in use [kg m-3]
      REAL(KIND(1D0)) :: hot_water_tank_wall_density = 0.0D0 ! Density of hot water tank wall [kg m-3]
      REAL(KIND(1D0)) :: hot_water_vessel_density = 0.0D0 ! Density of vessels containing hot water in use [kg m-3]
      REAL(KIND(1D0)) :: hot_water_tank_building_wall_view_factor = 0.0D0 ! Water tank/vessel internal building wall/roof view factor [-]
      REAL(KIND(1D0)) :: hot_water_tank_internal_mass_view_factor = 0.0D0 ! Water tank/vessel building internal mass view factor [-]
      REAL(KIND(1D0)) :: hot_water_tank_wall_conductivity = 0.0D0 ! Effective wall conductivity of the hot water tank [W m-1 K-1]
      REAL(KIND(1D0)) :: hot_water_tank_internal_wall_convection_coefficient = 0.0D0 ! Effective internal wall convection coefficient of the hot water tank [W m-2 K-1]
      REAL(KIND(1D0)) :: hot_water_tank_external_wall_convection_coefficient = 0.0D0 ! Effective external wall convection coefficient of the hot water tank [W m-2 K-1]
      REAL(KIND(1D0)) :: hot_water_vessel_wall_conductivity = 0.0D0 ! Effective wall conductivity of the hot water tank [W m-1 K-1]
      REAL(KIND(1D0)) :: hot_water_vessel_internal_wall_convection_coefficient = 0.0D0 ! Effective internal wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]
      REAL(KIND(1D0)) :: hot_water_vessel_external_wall_convection_coefficient = 0.0D0 ! Effective external wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]
      REAL(KIND(1D0)) :: hot_water_vessel_wall_emissivity = 0.0D0 ! Effective external wall emissivity of hot water being used within building [-]
      REAL(KIND(1D0)) :: hot_water_heating_efficiency = 0.0D0 ! Efficiency of hot water system [-]
      ! flag for iteration safety - YES - as we this should be updated every iteration
      LOGICAL :: iter_safe = .TRUE.
   END TYPE STEBBS_PRM

    TYPE :: STEBBS_BLDG
      ! MP TODO: Add initialisation values e.g. =0
      CHARACTER(len=256) :: building_type = 'Default'
      CHARACTER(len=256) :: building_name= 'Default'
      CHARACTER(len=256) :: fnml_lbm = 'Default'
      CHARACTER(len=256) :: case_id = 'Default'
      INTEGER :: id_lbm = 0
      INTEGER :: appliance_totalnumber = 0

      REAL(KIND(1D0)) :: QHload_heating_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QHload_cooling_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QH_metabolism = 0.0D0
      REAL(KIND(1D0)) :: QE_metabolism = 0.0D0
      REAL(KIND(1D0)) :: QHload_dhw_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: Qloss_drain_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: ratio_window_wall = 0.0D0
      REAL(KIND(1D0)) :: a_footprint = 0.0D0
      REAL(KIND(1D0)) :: height_building = 0.0D0
      REAL(KIND(1D0)) :: wall_external_area = 0.0D0
      REAL(KIND(1D0)) :: internal_volume_ratio = 0.0D0
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
      REAL(KIND(1D0)) :: window_transmissivity = 0.0D0
      REAL(KIND(1D0)) :: window_absorptivity = 0.0D0
      REAL(KIND(1D0)) :: window_reflectivity = 0.0D0
      REAL(KIND(1D0)) :: wall_transmissivity_state = 0.0D0
      REAL(KIND(1D0)) :: wall_absorptivity = 0.0D0
      REAL(KIND(1D0)) :: wall_reflectivity = 0.0D0
      REAL(KIND(1D0)) :: roof_transmissivity_state = 0.0D0
      REAL(KIND(1D0)) :: roof_absorptivity = 0.0D0
      REAL(KIND(1D0)) :: roof_reflectivity = 0.0D0
      REAL(KIND(1D0)) :: occupants_state = 0.0D0
      REAL(KIND(1D0)) :: metabolic_rate = 0.0D0
      REAL(KIND(1D0)) :: metabolism_threshold = 0.0D0
      REAL(KIND(1D0)) :: ratio_metabolic_latent_sensible = 0.0D0
      REAL(KIND(1D0)) :: appliance_power_rating = 0.0D0
      REAL(KIND(1D0)) :: lighting_power_rating = 0.0D0
      REAL(KIND(1D0)) :: frac_hotwater = 0.0D0
      REAL(KIND(1D0)) :: maxheatingpower_air = 0.0D0
      REAL(KIND(1D0)) :: heating_efficiency_air = 0.0D0
      REAL(KIND(1D0)) :: maxcoolingpower_air = 0.0D0
      REAL(KIND(1D0)) :: coeff_performance_cooling = 0.0D0
      REAL(KIND(1D0)) :: Vair_ind = 0.0D0
      REAL(KIND(1D0)) :: ventilation_rate = 0.0D0
      REAL(KIND(1D0)) :: a_wall = 0.0D0
      REAL(KIND(1D0)) :: a_roof = 0.0D0
      REAL(KIND(1D0)) :: v_wall = 0.0D0
      REAL(KIND(1D0)) :: v_roof = 0.0D0
      REAL(KIND(1D0)) :: v_ground_floor = 0.0D0
      REAL(KIND(1D0)) :: a_window = 0.0D0
      REAL(KIND(1D0)) :: v_window = 0.0D0
      REAL(KIND(1D0)) :: v_indoor_mass = 0.0D0
      REAL(KIND(1D0)) :: a_indoor_mass = 0.0D0
      REAL(KIND(1D0)) :: Tair_ind = 0.0D0
      REAL(KIND(1D0)) :: t_indoor_mass = 0.0D0
      REAL(KIND(1D0)) :: t_int_wall = 0.0D0
      REAL(KIND(1D0)) :: t_int_roof = 0.0D0
      REAL(KIND(1D0)) :: t_ext_wall = 0.0D0
      REAL(KIND(1D0)) :: t_ext_roof = 0.0D0
      REAL(KIND(1D0)) :: t_int_window = 0.0D0
      REAL(KIND(1D0)) :: t_ext_window = 0.0D0
      REAL(KIND(1D0)) :: t_int_ground_floor = 0.0D0
      REAL(KIND(1D0)) :: t_ext_ground_floor = 0.0D0
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
      REAL(KIND(1D0)), DIMENSION(0:143, 2) :: flowrate_water_supply_profile = 0.0D0
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
      REAL(KIND(1D0)) :: maxVwater_vessel = 0.0D0  ! Maximum volume of DHW in use [m3]
      REAL(KIND(1D0)) :: minHeatingPower_DHW = 0.0D0
      REAL(KIND(1D0)) :: HeatingPower_DHW = 0.0D0

      REAL(KIND(1D0)) :: qfm_dom = 0.0D0 ! Metabolic sensible and latent heat
      REAL(KIND(1D0)) :: qheat_dom = 0.0D0  ! Hourly heating load  [W]
      REAL(KIND(1D0)) :: qcool_dom = 0.0D0  ! Hourly cooling load  [W]
      REAL(KIND(1D0)) :: qfb_hw_dom = 0.0D0  ! Hot water
      REAL(KIND(1D0)) :: qfb_dom_air = 0.0D0  ! Sensible heat to air [W]
      REAL(KIND(1D0)) :: dom_temp = 0.0D0  ! Domain temperature   [W]
      REAL(KIND(1D0)) :: QN_bldg_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QEC_bldg_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QS_total_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QH_bldg_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QBAE_bldg_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QWaste_bldg_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QS_bldg_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QS_dhw_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QS_ground_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QEC_heating_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QEC_cooling_tstepFA = 0.0D0
      REAL(KIND(1D0)) :: QEC_dhw_tstepFA = 0.0D0

      REAL(KIND(1D0)), DIMENSION(2) :: ts, initTs = 0.0D0
      REAL(KIND(1D0)), DIMENSION(5) :: h_i, k_eff = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: h_o = 0.0D0
      REAL(KIND(1D0)), DIMENSION(6) :: rho = 0.0D0
      REAL(KIND(1D0)), DIMENSION(6) :: cp = 0.0D0
      REAL(KIND(1D0)), DIMENSION(7) :: emis = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: witar, waTAR, roofTAR = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: h_ts_average, HWTsAverage = 0.0D0
      REAL(KIND(1D0)), DIMENSION(3) :: hw_power_average = 0.0D0
      REAL(KIND(1D0)), DIMENSION(45) :: energy_exchanges = 0.0D0

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Textwall_C  ! Wall external surface temperature from STEBBS[K]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Textroof_C ! Roof external surface temperature from STEBBS[K]
      LOGICAL :: wall_surface_active = .TRUE. ! depends on WWR
      LOGICAL :: window_surface_active = .TRUE.! depends on WWR
      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.

   CONTAINS
   PROCEDURE :: ALLOCATE => allocSTEBBS_bldgState
   PROCEDURE :: DEALLOCATE => deallocSTEBBS_bldgState

   END TYPE STEBBS_BLDG

   TYPE, PUBLIC :: STEBBS_STATE

      ! Beers output for STEBBS - TODO: these should be kept in the HEAT_STATE type -
      REAL(KIND(1D0)) :: kdown_2d = 0.0D0 ! incoming shortwave radiation onto roof [W m-2]
      REAL(KIND(1D0)) :: kup_2d = 0.0D0 ! outgoing shortwave radiation from roof [W m-2]
      REAL(KIND(1D0)) :: k_west = 0.0D0 ! incoming shortwave radiation from west [W m-2]
      REAL(KIND(1D0)) :: k_south = 0.0D0 ! incoming shortwave radiation from south [W m-2]
      REAL(KIND(1D0)) :: k_north = 0.0D0 ! incoming shortwave radiation from north [W m-2]
      REAL(KIND(1D0)) :: k_east = 0.0D0 ! incoming shortwave radiation from east [W m-2]
      REAL(KIND(1D0)) :: ldown_2d = 0.0D0 ! incoming longwave radiation onto roof [W m-2]
      REAL(KIND(1D0)) :: lup_2d = 0.0D0 ! outgoing longwave radiation from roof [W m-2]
      REAL(KIND(1D0)) :: l_west = 0.0D0 ! incoming longwave radiation from west [W m-2]
      REAL(KIND(1D0)) :: l_south = 0.0D0 ! incoming longwave radiation from south [W m-2]
      REAL(KIND(1D0)) :: l_north = 0.0D0 ! incoming longwave radiation from north [W m-2]
      REAL(KIND(1D0)) :: l_east = 0.0D0 ! incoming longwave radiation from east [W m-2]
      REAL(KIND(1D0)), DIMENSION(30) :: z_array = -999 !RSL layer heights
      REAL(KIND(1D0)), DIMENSION(30) :: dataout_line_u_rsl = -999 ! wind speed array from RSL [m s-1]
      REAL(KIND(1D0)), DIMENSION(30) :: dataout_line_t_rsl = -999 ! Temperature array from RSL[C]
      REAL(KIND(1D0)), DIMENSION(30) :: dataout_line_q_rsl = -999 ! Specific humidity array from RSL[g kg-1]
      ! Initial conditions that are updated during runtime
      REAL(KIND(1D0)) :: deep_soil_temperature = 0.0D0 ! Deep soil temperature [C]
      REAL(KIND(1D0)) :: MonthMeanAirTemperature_diffmax = 0.0D0 ! largest difference of monthly mean outdoor air temperature [C]
      REAL(KIND(1D0)) :: outdoor_air_start_temperature = 0.0D0 ! Initial outdoor air temperature [degC]
      REAL(KIND(1D0)) :: indoor_air_start_temperature = 0.0D0 ! Initial indoor air temperature [degC]
      REAL(KIND(1D0)) :: indoor_mass_start_temperature = 0.0D0 ! Initial indoor mass temperature [degC]
      REAL(KIND(1D0)) :: wall_indoor_surface_temperature = 0.0D0 ! Initial wall indoor surface temperature [degC]
      REAL(KIND(1D0)) :: wall_outdoor_surface_temperature = 0.0D0 ! Initial walloutdoor surface temperature [degC]
      REAL(KIND(1D0)) :: roof_indoor_surface_temperature = 0.0D0 ! Initial roof indoor surface temperature [degC]
      REAL(KIND(1D0)) :: roof_outdoor_surface_temperature = 0.0D0 ! Initial roof outdoor surface temperature [degC]
      REAL(KIND(1D0)) :: window_indoor_surface_temperature = 0.0D0 ! Initial window indoor surface temperature [degC]
      REAL(KIND(1D0)) :: window_outdoor_surface_temperature = 0.0D0 ! Initial window outdoor surface temperature [degC]
      REAL(KIND(1D0)) :: ground_floor_indoor_surface_temperature = 0.0D0 ! Initial ground floor indoor surface temperature [degC]
      REAL(KIND(1D0)) :: ground_floor_outdoor_surface_temperature = 0.0D0 ! Initial ground floor outdoor surface temperature [degC]
      REAL(KIND(1D0)) :: water_tank_temperature_state = 0.0D0 ! Initial water temperature in hot water tank [degC]
      REAL(KIND(1D0)) :: internal_wall_water_tank_temperature = 0.0D0 ! Initial hot water tank internal wall temperature [degC]
      REAL(KIND(1D0)) :: external_wall_water_tank_temperature = 0.0D0 ! Initial hot water tank external wall temperature [degC]
      REAL(KIND(1D0)) :: mains_water_temperature = 0.0D0 ! Temperature of water coming into the water tank [degC]
      REAL(KIND(1D0)) :: domestic_hot_water_temperature_in_use_in_building = 0.0D0 ! Initial water temperature of water held in use in building [degC]
      REAL(KIND(1D0)) :: internal_wall_dhw_vessel_temperature = 0.0D0 ! Initial hot water vessel internal wall temperature [degC]
      REAL(KIND(1D0)) :: external_wall_dhw_vessel_temperature = 0.0D0 ! Initial hot water vessel external wall temperature [degC]
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
      INTEGER :: i

      CALL self%DEALLOCATE()
      ALLOCATE (self%buildings(ntypes))
      ! Pre-allocate inner arrays so they are never null when accessed
      ! via ASSOCIATE in SUEWS_cal_Qn. gen_building re-allocates later
      ! with correct values from STEBBS state.
      DO i = 1, ntypes
         CALL self%buildings(i)%ALLOCATE(num_layer)
      END DO

   END SUBROUTINE allocSTEBBS_bldg

   SUBROUTINE deallocSTEBBS_bldg(self)
      IMPLICIT NONE

      CLASS(STEBBS_STATE), INTENT(INOUT) :: self
      IF (ALLOCATED(self%buildings)) DEALLOCATE (self%buildings)

   END SUBROUTINE deallocSTEBBS_bldg

END MODULE module_type_stebbs
