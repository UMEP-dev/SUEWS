MODULE SUEWS_DEF_DTS
   USE allocateArray, ONLY: &
      nsurf, nvegsurf, nspec, &
      PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf, &
      ivConif, ivDecid, ivGrass, &
      ncolumnsDataOutSUEWS, ncolumnsDataOutSnow, &
      ncolumnsDataOutESTM, ncolumnsDataOutDailyState, &
      ncolumnsDataOutRSL, ncolumnsdataOutSOLWEIG, ncolumnsDataOutBEERS, &
      ncolumnsDataOutDebug, ncolumnsDataOutSPARTACUS, ncolumnsDataOutEHC, &
      ncolumnsDataOutSTEBBS, ncolumnsDataOutNHood

   IMPLICIT NONE
   ! in the following, the type definitions starting with `SUEWS_` are used in the main program

   ! ********** SUEWS_parameters schema (basic) **********
   TYPE, PUBLIC :: SUEWS_CONFIG
      INTEGER :: DiagMethod = 0 ! Defines how near surface diagnostics are calculated
      INTEGER :: EmissionsMethod = 0 ! method to calculate anthropogenic heat [-]
      INTEGER :: RoughLenHeatMethod = 0 ! method to calculate heat roughness length [-]
      INTEGER :: RoughLenMomMethod = 0 ! Determines how aerodynamic roughness length (z0m) and zero displacement height (zdm) are calculated [-]
      INTEGER :: FAIMethod = 0 !Determines how FAI is calculated [-]
      INTEGER :: SMDMethod = 0 ! Determines method for calculating soil moisture deficit [-]
      INTEGER :: WaterUseMethod = 0 ! Defines how external water use is calculated[-]
      INTEGER :: NetRadiationMethod = 0 ! method for calculation of radiation fluxes [-]
      INTEGER :: StabilityMethod = 0 ! method to calculate atmospheric stability [-]
      INTEGER :: StorageHeatMethod = 0 ! !Determines method for calculating storage heat flux ΔQS [-]
      INTEGER :: Diagnose = 0 ! flag for printing diagnostic info during runtime [N/A]C
      INTEGER :: SnowUse = 0 !
      LOGICAL :: use_sw_direct_albedo = .FALSE. !boolean, Specify ground and roof albedos separately for direct solar radiation [-]
      INTEGER :: ohmIncQF = 0 ! Determines whether the storage heat flux calculation uses Q* or ( Q* +QF) [-]
      INTEGER :: DiagQS = 0 ! flag for printing diagnostic info for QS module during runtime [N/A] ! not used and will be removed
      INTEGER :: EvapMethod = 0 ! Evaporation calculated according to Rutter (1) or Shuttleworth (2) [-]
      INTEGER :: LAImethod = 0 ! boolean to determine if calculate LAI [-]
      INTEGER :: localClimateMethod = 0 ! method to choose local climate variables [-] 0: not use; 1: use local climate variables
      INTEGER :: stebbsmethod = 0 ! method to calculate building energy [-]
      LOGICAL :: flag_test = .FALSE. ! FOR DEBUGGING ONLY: boolean to test specific functions [-]
   END TYPE SUEWS_CONFIG

   TYPE, PUBLIC :: SURF_STORE_PRM
      REAL(KIND(1D0)) :: store_min = 0.0D0
      REAL(KIND(1D0)) :: store_max = 0.0D0
      REAL(KIND(1D0)) :: store_cap = 0.0D0 ! this should be abundant ? (current storage capacity) [mm]
      INTEGER :: drain_eq = 0
      REAL(KIND(1D0)) :: drain_coef_1 = 0.0D0
      REAL(KIND(1D0)) :: drain_coef_2 = 0.0D0
   END TYPE SURF_STORE_PRM

   TYPE, PUBLIC :: WATER_DIST_PRM
      REAL(KIND(1D0)) :: to_paved = 0.0D0
      REAL(KIND(1D0)) :: to_bldg = 0.0D0
      REAL(KIND(1D0)) :: to_evetr = 0.0D0
      REAL(KIND(1D0)) :: to_dectr = 0.0D0
      REAL(KIND(1D0)) :: to_grass = 0.0D0
      REAL(KIND(1D0)) :: to_bsoil = 0.0D0
      REAL(KIND(1D0)) :: to_water = 0.0D0
      REAL(KIND(1D0)) :: to_soilstore = 0.0D0
   END TYPE WATER_DIST_PRM

   TYPE, PUBLIC :: bioCO2_PRM
      REAL(KIND(1D0)) :: beta_bioco2 = 0.0D0 ! The light-saturated gross photosynthesis of the canopy [umol m-2 s-1 ]
      REAL(KIND(1D0)) :: beta_enh_bioco2 = 0.0D0 ! Part of the beta coefficient related to the fraction of vegetation [umol m-2 s-1 ]
      REAL(KIND(1D0)) :: alpha_bioco2 = 0.0D0 ! The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve [-]
      REAL(KIND(1D0)) :: alpha_enh_bioco2 = 0.0D0 ! Part of the alpha coefficient related to the fraction of vegetation [-]
      REAL(KIND(1D0)) :: resp_a = 0.0D0 !Respiration coefficient a
      REAL(KIND(1D0)) :: resp_b = 0.0D0 !Respiration coefficient b - related to air temperature dependency
      REAL(KIND(1D0)) :: theta_bioco2 = 0.0D0 ! The convexity of the curve at light saturation [-]
      REAL(KIND(1D0)) :: min_res_bioCO2 = 0.0D0 ! Minimum soil respiration rate (for cold-temperature limit) [umol m-2 s-1]
   END TYPE bioCO2_PRM

   TYPE, PUBLIC :: CONDUCTANCE_PRM
      REAL(KIND(1D0)) :: g_max = 0.0D0 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)) :: g_k = 0.0D0
      REAL(KIND(1D0)) :: g_q_base = 0.0D0
      REAL(KIND(1D0)) :: g_q_shape = 0.0D0
      REAL(KIND(1D0)) :: g_t = 0.0D0
      REAL(KIND(1D0)) :: g_sm = 0.0D0 ! Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)) :: kmax = 0.0D0 ! annual maximum hourly solar radiation [W m-2]
      ! TODO Should this not be moved to the physics options!
      INTEGER :: gsmodel = 0 ! choice of gs parameterisation (1 = Ja11, 2 = Wa16) [-]
      REAL(KIND(1D0)) :: s1 = 0.0D0 ! a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)) :: s2 = 0.0D0 ! a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)) :: TH = 0.0D0 ! upper air temperature limit [degC]
      REAL(KIND(1D0)) :: TL = 0.0D0 ! lower air temperature limit [degC]
   END TYPE CONDUCTANCE_PRM

   TYPE, PUBLIC :: LAI_PRM ! do we need `lai_id` here?
      REAL(KIND(1D0)) :: baset = 0.0D0 !Base Temperature for initiating growing degree days (GDD) for leaf growth [degC]
      REAL(KIND(1D0)) :: gddfull = 0.0D0 !the growing degree days (GDD) needed for full capacity of the leaf area index [degC]
      REAL(KIND(1D0)) :: basete = 0.0D0 !Base temperature for initiating sensesance degree days (SDD) for leaf off [degC]
      REAL(KIND(1D0)) :: sddfull = 0.0D0 !the sensesence degree days (SDD) needed to initiate leaf off [degC]
      REAL(KIND(1D0)) :: laimin = 0.0D0 !leaf-off wintertime value [m2 m-2]
      REAL(KIND(1D0)) :: laimax = 0.0D0 !full leaf-on summertime value [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(4) :: laipower = 0.0D0 ! parameters required by LAI calculation.
      INTEGER :: laitype = 0 ! LAI calculation choice.
   END TYPE LAI_PRM

   TYPE, PUBLIC :: OHM_COEF_LC
      REAL(KIND(1D0)) :: summer_dry = 0.0D0
      REAL(KIND(1D0)) :: summer_wet = 0.0D0
      REAL(KIND(1D0)) :: winter_dry = 0.0D0
      REAL(KIND(1D0)) :: winter_wet = 0.0D0
   END TYPE OHM_COEF_LC

   TYPE, PUBLIC :: OHM_PRM
      REAL(KIND(1D0)) :: chanohm = 0.0D0 ! Bulk transfer coefficient for this surface to use in AnOHM [J m-3 K-1]
      REAL(KIND(1D0)) :: cpanohm = 0.0D0 ! Volumetric heat capacity for this surface to use in AnOHM  [J m-3 K-1]
      REAL(KIND(1D0)) :: kkanohm = 0.0D0 ! Thermal conductivity for this surface to use in AnOHM [W m-1 K-1]
      REAL(KIND(1D0)) :: ohm_threshsw = 0.0D0 ! Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)) :: ohm_threshwd = 0.0D0 ! Soil moisture threshold determining whether wet/dry OHM coefficients are applied [degC]
      TYPE(OHM_COEF_LC), DIMENSION(3) :: ohm_coef_lc
   END TYPE OHM_PRM

   TYPE, PUBLIC :: SOIL_PRM
      REAL(KIND(1D0)) :: soildepth = 0.0D0 ! Depth of soil beneath the surface [mm]
      REAL(KIND(1D0)) :: soilstorecap = 0.0D0 ! Capacity of soil store [mm]
      REAL(KIND(1D0)) :: sathydraulicconduct = 0.0D0 ! !Hydraulic conductivity for saturated soil [mm s-1]
   END TYPE SOIL_PRM

   TYPE, PUBLIC :: anthroHEAT_PRM
      REAL(KIND(1D0)) :: qf0_beu_working = 0.0D0 ! Fraction of base value coming from buildings (working day) [-]
      REAL(KIND(1D0)) :: qf0_beu_holiday = 0.0D0 ! Fraction of base value coming from buildings (holiday) [-]
      REAL(KIND(1D0)) :: qf_a_working = 0.0D0 ! Base value for QF (working day) [W m-2]
      REAL(KIND(1D0)) :: qf_a_holiday = 0.0D0 ! Base value for QF (holiday) [W m-2]
      REAL(KIND(1D0)) :: qf_b_working = 0.0D0 ! Parameter related to heating degree days (working day) [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)) :: qf_b_holiday = 0.0D0 ! Parameter related to heating degree days (holiday) [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)) :: qf_c_working = 0.0D0 ! Parameter related to cooling degree days (working day) [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)) :: qf_c_holiday = 0.0D0 ! Parameter related to cooling degree days (holiday) [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)) :: baset_cooling_working = 0.0D0 ! Base temperature for cooling degree days (working day) [degC]
      REAL(KIND(1D0)) :: baset_cooling_holiday = 0.0D0 ! Base temperature for cooling degree days (holiday) [degC]
      REAL(KIND(1D0)) :: baset_heating_working = 0.0D0 ! Base temperature for heating degree days (working day) [degC]
      REAL(KIND(1D0)) :: baset_heating_holiday = 0.0D0 ! Base temperature for heating degree days (holiday) [degC]
      REAL(KIND(1D0)) :: ah_min_working = 0.0D0 ! minimum QF values (working day) [W m-2]
      REAL(KIND(1D0)) :: ah_min_holiday = 0.0D0 ! minimum QF values (holiday) [W m-2]
      REAL(KIND(1D0)) :: ah_slope_cooling_working = 0.0D0 ! cooling slope for the anthropogenic heat flux calculation (working day) [W m-2 K-1]
      REAL(KIND(1D0)) :: ah_slope_cooling_holiday = 0.0D0 ! cooling slope for the anthropogenic heat flux calculation (holiday) [W m-2 K-1]
      REAL(KIND(1D0)) :: ah_slope_heating_working = 0.0D0 ! heating slope for the anthropogenic heat flux calculation (working day) [W m-2 K-1]
      REAL(KIND(1D0)) :: ah_slope_heating_holiday = 0.0D0 ! heating slope for the anthropogenic heat flux calculation (holiday) [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(0:23) :: ahprof_24hr_working = 0.0D0 ! Hourly profile values used in energy use calculation (working day) [-]
      REAL(KIND(1D0)), DIMENSION(0:23) :: ahprof_24hr_holiday = 0.0D0 ! Hourly profile values used in energy use calculation (holiday) [-]
      REAL(KIND(1D0)) :: popdensdaytime_working = 0.0D0 ! Daytime population density [people ha-1] (working day)
      REAL(KIND(1D0)) :: popdensdaytime_holiday = 0.0D0 ! Daytime population density [people ha-1] (holiday)
      REAL(KIND(1D0)) :: popdensnighttime = 0.0D0
      REAL(KIND(1D0)), DIMENSION(0:23) :: popprof_24hr_working = 0.0D0 !Hourly profile values used in dynamic population estimation[-] (working day)
      REAL(KIND(1D0)), DIMENSION(0:23) :: popprof_24hr_holiday = 0.0D0 !Hourly profile values used in dynamic population estimation[-] (holiday)
   END TYPE anthroHEAT_PRM

   TYPE, PUBLIC :: IRRIG_daywater
      ! TODO: Change flags to int or bool, not REAL!
      REAL(KIND(1D0)) :: monday_flag = 0.0D0 ! Irrigation flag: 1 for on and 0 for off.
      REAL(KIND(1D0)) :: monday_percent = 0.0D0 ! Fraction of properties using irrigation for each day of a week.
      REAL(KIND(1D0)) :: tuesday_flag = 0.0D0
      REAL(KIND(1D0)) :: tuesday_percent = 0.0D0
      REAL(KIND(1D0)) :: wednesday_flag = 0.0D0
      REAL(KIND(1D0)) :: wednesday_percent = 0.0D0
      REAL(KIND(1D0)) :: thursday_flag = 0.0D0
      REAL(KIND(1D0)) :: thursday_percent = 0.0D0
      REAL(KIND(1D0)) :: friday_flag = 0.0D0
      REAL(KIND(1D0)) :: friday_percent = 0.0D0
      REAL(KIND(1D0)) :: saturday_flag = 0.0D0
      REAL(KIND(1D0)) :: saturday_percent = 0.0D0
      REAL(KIND(1D0)) :: sunday_flag = 0.0D0
      REAL(KIND(1D0)) :: sunday_percent = 0.0D0
   END TYPE IRRIG_daywater

   TYPE, PUBLIC :: IRRIGATION_PRM ! used in irrigation
      REAL(KIND(1D0)) :: h_maintain = 0.0D0 ! ponding water depth to maintain [mm]
      REAL(KIND(1D0)) :: faut = 0.0D0 ! Fraction of irrigated area using automatic irrigation [-]
      REAL(KIND(1D0)), DIMENSION(3) :: ie_a = 0.0D0 ! Coefficient for automatic irrigation model
      REAL(KIND(1D0)), DIMENSION(3) :: ie_m = 0.0D0 ! Coefficient for automatic irrigation model
      INTEGER :: ie_start = 0 ! ending time of water use [DOY]
      INTEGER :: ie_end = 0 ! starting time of water use [DOY]
      REAL(KIND(1D0)) :: internalwateruse_h = 0.0D0 ! Internal water use [mm h-1]
      TYPE(IRRIG_daywater) :: irr_daywater
      REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofa_24hr_working = 0.0D0 ! Hourly profile values used in automatic irrigation[-]
      REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofa_24hr_holiday = 0.0D0
      REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofm_24hr_working = 0.0D0 ! Hourly profile values used in manual irrigation[-]
      REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofm_24hr_holiday = 0.0D0
   END TYPE IRRIGATION_PRM

   TYPE, PUBLIC :: anthroEMIS_PRM
      INTEGER :: startdls = 0 ! start of daylight saving  [DOY]
      INTEGER :: enddls = 0 ! end of daylight saving [DOY]
      TYPE(anthroHEAT_PRM) :: anthroheat
      REAL(KIND(1D0)) :: EF_umolCO2perJ = 0.0D0
      REAL(KIND(1D0)) :: EnEF_v_Jkm = 0.0D0
      REAL(KIND(1D0)) :: FrFossilFuel_Heat = 0.0D0
      REAL(KIND(1D0)) :: FrFossilFuel_NonHeat = 0.0D0
      REAL(KIND(1D0)), DIMENSION(2) :: FcEF_v_kgkm = 0.0D0
      REAL(KIND(1D0)), DIMENSION(0:23) :: HumActivity_24hr_working = 0.0D0
      REAL(KIND(1D0)), DIMENSION(0:23) :: HumActivity_24hr_holiday = 0.0D0
      REAL(KIND(1D0)) :: MaxFCMetab = 0.0D0
      REAL(KIND(1D0)) :: MaxQFMetab = 0.0D0
      REAL(KIND(1D0)) :: MinFCMetab = 0.0D0
      REAL(KIND(1D0)) :: MinQFMetab = 0.0D0
      REAL(KIND(1D0)) :: TrafficRate_working = 0.0D0
      REAL(KIND(1D0)) :: TrafficRate_holiday = 0.0D0
      REAL(KIND(1D0)) :: TrafficUnits = 0.0D0
      REAL(KIND(1D0)), DIMENSION(0:23) :: TraffProf_24hr_working = 0.0D0
      REAL(KIND(1D0)), DIMENSION(0:23) :: TraffProf_24hr_holiday = 0.0D0
   END TYPE anthroEMIS_PRM

   TYPE, PUBLIC :: SNOW_PRM
      REAL(KIND(1D0)) :: crwmax = 0.0D0 ! maximum water holding capacity of snow [mm]
      REAL(KIND(1D0)) :: crwmin = 0.0D0 ! minimum water holding capacity of snow [mm]
      REAL(KIND(1D0)) :: narp_emis_snow = 0.0D0 ! snow emissivity in NARP model [-]
      REAL(KIND(1D0)) :: preciplimit = 0.0D0 ! temperature limit when precipitation falls as snow [degC]
      REAL(KIND(1D0)) :: preciplimitalb = 0.0D0 ! Limit for hourly precipitation when the ground is fully covered with snow [mm]
      REAL(KIND(1D0)) :: snowalbmax = 0.0D0 ! effective surface albedo (middle of the day value) for summertime [-]
      REAL(KIND(1D0)) :: snowalbmin = 0.0D0 ! effective surface albedo (middle of the day value) for wintertime (not including snow) [-]
      REAL(KIND(1D0)) :: snowdensmax = 0.0D0 ! maximum snow density [kg m-3]
      REAL(KIND(1D0)) :: snowdensmin = 0.0D0 ! fresh snow density [kg m-3]
      REAL(KIND(1D0)) :: snowlimbldg = 0.0D0 ! Limit of the snow water equivalent for snow removal from building roofs [mm]
      REAL(KIND(1D0)) :: snowlimpaved = 0.0D0 ! limit of the snow water equivalent for snow removal from roads[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowpacklimit = 0.0D0 ! Limit for the snow water equivalent when snow cover starts to be patchy [mm]
      REAL(KIND(1D0)), DIMENSION(0:23) :: snowprof_24hr_working = 0.0D0 ! Hourly profile values used in snow clearing of working day [-]
      REAL(KIND(1D0)), DIMENSION(0:23) :: snowprof_24hr_holiday = 0.0D0 ! Hourly profile values used in snow clearing of holiday [-]
      REAL(KIND(1D0)) :: tau_a = 0.0D0 ! time constant for snow albedo aging in cold snow [-]
      REAL(KIND(1D0)) :: tau_f = 0.0D0 ! time constant for snow albedo aging in melting snow [-]
      REAL(KIND(1D0)) :: tau_r = 0.0D0 ! time constant for snow density ageing [-]
      REAL(KIND(1D0)) :: tempmeltfact = 0.0D0 ! hourly temperature melt factor of snow [mm K-1 h-1]
      REAL(KIND(1D0)) :: radmeltfact = 0.0D0 ! hourly radiation melt factor of snow [mm W-1 h-1]
   END TYPE SNOW_PRM

   TYPE, PUBLIC :: SPARTACUS_PRM
      REAL(KIND(1D0)) :: air_ext_lw = 0.0D0
      REAL(KIND(1D0)) :: air_ext_sw = 0.0D0
      REAL(KIND(1D0)) :: air_ssa_lw = 0.0D0
      REAL(KIND(1D0)) :: air_ssa_sw = 0.0D0
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: height
      REAL(KIND(1D0)) :: ground_albedo_dir_mult_fact = 0.0D0
      INTEGER :: n_stream_lw_urban ! LW streams per hemisphere [-]
      INTEGER :: n_stream_sw_urban ! shortwave diffuse streams per hemisphere [-]
      INTEGER :: n_vegetation_region_urban ! Number of regions used to describe vegetation [-]
      REAL(KIND(1D0)) :: sw_dn_direct_frac = 0.0D0
      REAL(KIND(1D0)) :: use_sw_direct_albedo = 0.0D0
      REAL(KIND(1D0)) :: veg_contact_fraction_const = 0.0D0
      REAL(KIND(1D0)) :: veg_fsd_const = 0.0D0
      REAL(KIND(1D0)) :: veg_ssa_lw = 0.0D0
      REAL(KIND(1D0)) :: veg_ssa_sw = 0.0D0
   END TYPE SPARTACUS_PRM

   TYPE, PUBLIC :: SPARTACUS_LAYER_PRM
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: building_frac ! building fraction [-]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: building_scale ! diameter of buildings [[m]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_frac ! vegetation fraction [-]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_scale ! scale of tree crowns [m]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: alb_roof ! albedo of roof [-]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: emis_roof ! emissivity of roof [-]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: alb_wall ! albedo of wall [-]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: emis_wall ! emissivity of wall [-]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: roof_albedo_dir_mult_fact !Ratio of the direct and diffuse albedo of the roof[-]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: wall_specular_frac ! Fraction of wall reflection that is specular [-]
   CONTAINS
      PROCEDURE :: ALLOCATE => allocate_spartacus_layer_prm_c
      PROCEDURE :: DEALLOCATE => deallocate_spartacus_layer_prm_c
   END TYPE SPARTACUS_LAYER_PRM

   ! TYPE, PUBLIC :: STEBBS_BUILDING_PRM

   !    ! CONTAINS
   !    !    PROCEDURE :: ALLOCATE => allocate_stebbs_c
   !    !    PROCEDURE :: DEALLOCATE => deallocate_stebbs_c
   ! END TYPE STEBBS_BUILDING_PRM

   ! ********** SUEWS_parameters schema (derived) **********

   TYPE, PUBLIC :: LUMPS_PRM
      REAL(KIND(1D0)) :: raincover = 0.0D0 ! limit when surface totally covered with water for LUMPS [mm]
      REAL(KIND(1D0)) :: rainmaxres = 0.0D0 ! maximum water bucket reservoir. Used for LUMPS surface wetness control. [mm]
      REAL(KIND(1D0)) :: drainrt = 0.0D0 ! Drainage rate of the water bucket [mm hr-1]
      INTEGER :: veg_type ! Defines how vegetation is calculated for LUMPS [-]
   END TYPE LUMPS_PRM

   TYPE, PUBLIC :: EHC_PRM
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soil_storecap_roof ! Capacity of soil store for roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soil_storecap_wall ! Capacity of soil store for wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_limit_roof ! Limit for state_id of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_limit_wall ! Limit for state_id of wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: wet_thresh_roof ! wetness threshold  of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: wet_thresh_wall ! wetness threshold  of wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_roof ! indoor temperature for roof [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_wall ! indoor temperature for wall [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_surf ! deep bottom temperature for each surface [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_roof ! thermal conductivity of roof [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_wall ! thermal conductivity of wall [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_surf ! thermal conductivity of v [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_roof ! Volumetric Heat capacity of roof [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_wall ! Volumetric Heat capacity of wall [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_surf ! Volumetric Heat capacity of each surface [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_roof ! thickness of each layer in roof [m]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_wall ! thickness of each layer in wall [m]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_surf ! thickness of each layer in surface [m]
   CONTAINS
      PROCEDURE :: ALLOCATE => allocate_ehc_prm_c
      PROCEDURE :: DEALLOCATE => deallocate_ehc_prm_c
   END TYPE EHC_PRM

   TYPE, PUBLIC :: LC_PAVED_PRM
      ! land cover specific parameters for paved surfaces
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: state = 0.0D0
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracpaved = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_PAVED_PRM

   TYPE, PUBLIC :: LC_BLDG_PRM
      ! land cover specific parameters for buildings
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: faibldg = 0.0D0
      REAL(KIND(1D0)) :: bldgh = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: state = 0.0D0
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracbldgs = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_BLDG_PRM

   TYPE, PUBLIC :: LC_DECTR_PRM
      ! land cover specific parameters for deciduous trees
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      REAL(KIND(1D0)) :: faidectree = 0.0D0
      REAL(KIND(1D0)) :: dectreeh = 0.0D0
      REAL(KIND(1D0)) :: pormin_dec = 0.0D0 ! absent for evergreen trees ??
      REAL(KIND(1D0)) :: pormax_dec = 0.0D0
      REAL(KIND(1D0)) :: alb_min = 0.0D0
      REAL(KIND(1D0)) :: alb_max = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm] ! ******* dummy variable *******
      REAL(KIND(1D0)) :: capmax_dec = 0.0D0 ! Maximum water storage capacity for upper surfaces (i.e. canopy) (absent for evergreen trees ??)
      REAL(KIND(1D0)) :: capmin_dec = 0.0D0 ! Minimum water storage capacity for upper surfaces (i.e. canopy).
      REAL(KIND(1D0)) :: irrfracdectr = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(bioCO2_PRM) :: bioco2
      REAL(KIND(1D0)) :: maxconductance = 0.0D0 ! the maximum conductance of each vegetation or surface type. [mm s-1]
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_DECTR_PRM

   TYPE, PUBLIC :: LC_EVETR_PRM
      ! land cover specific parameters for evergreen trees
      REAL(KIND(1D0)) :: sfr = 0.0D0 !surface cover fraction[-]
      REAL(KIND(1D0)) :: emis = 0.0D0 !Effective surface emissivity[-]
      REAL(KIND(1D0)) :: faievetree = 0.0D0 !frontal area index for evergreen tree [-]
      REAL(KIND(1D0)) :: evetreeh = 0.0D0 !height of evergreen tree [m]
      REAL(KIND(1D0)) :: alb_min = 0.0D0 !minimum albedo for evergreen tree [-]
      REAL(KIND(1D0)) :: alb_max = 0.0D0 !maximum albedo for evergreen tree [-]
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracevetr = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0 !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]
      TYPE(bioCO2_PRM) :: bioco2
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      REAL(KIND(1D0)) :: maxconductance = 0.0D0 ! the maximum conductance of each vegetation or surface type. [mm s-1]
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist !Fraction of water redistribution [-]
   END TYPE LC_EVETR_PRM

   TYPE, PUBLIC :: LC_GRASS_PRM
      ! land cover specific parameters for grass
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      REAL(KIND(1D0)) :: alb_min = 0.0D0
      REAL(KIND(1D0)) :: alb_max = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracgrass = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(bioCO2_PRM) :: bioco2
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      REAL(KIND(1D0)) :: maxconductance = 0.0D0 ! the maximum conductance of each vegetation or surface type. [mm s-1]
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_GRASS_PRM

   TYPE, PUBLIC :: LC_BSOIL_PRM
      ! land cover specific parameters for bare soil
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracbsoil = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_BSOIL_PRM

   TYPE, PUBLIC :: LC_WATER_PRM
      ! land cover specific parameters for water surface
      REAL(KIND(1D0)) :: sfr = 0.0D0
      REAL(KIND(1D0)) :: emis = 0.0D0
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit = 0.0D0 ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracwater = 0.0D0
      REAL(KIND(1D0)) :: wetthresh = 0.0D0 ! ******* dummy variable *******
      REAL(KIND(1D0)) :: flowchange = 0.0D0 ! special term in water
   END TYPE LC_WATER_PRM

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
      REAL(KIND(1D0)) :: DHWVesselEmissivity = 0.0D0 ! NEEDS CHECKED! NOT USED (assumed same as DHWVesselWallEmissivity) [-]
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

   TYPE, PUBLIC :: SUEWS_SITE
      REAL(KIND(1D0)) :: lat = 0.0D0 !latitude [deg]
      REAL(KIND(1D0)) :: lon = 0.0D0 !longitude [deg]
      REAL(KIND(1D0)) :: alt = 0.0D0 ! solar altitude [deg]
      INTEGER :: gridiv = 1 ! grid id [-]
      REAL(KIND(1D0)) :: timezone = 0.0D0 ! time zone, for site relative to UTC (east is positive) [h]
      REAL(KIND(1D0)) :: surfacearea = 0.0D0 ! area of the grid [m2]
      REAL(KIND(1D0)) :: z = 0.0D0 ! measurement height [m]
      REAL(KIND(1D0)) :: z0m_in = 0.0D0 ! roughness length for momentum [m]
      REAL(KIND(1D0)) :: zdm_in = 0.0D0 ! zero-plane displacement [m]
      REAL(KIND(1D0)) :: pipecapacity = 0.0D0 ! capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)) :: runofftowater = 0.0D0 ! fraction of above-ground runoff flowing to water surface during flooding [-]
      REAL(KIND(1D0)) :: narp_trans_site = 0.0D0 ! atmospheric transmissivity for NARP [-]
      REAL(KIND(1D0)) :: CO2PointSource = 0.0D0 ! CO2 emission factor [kg km-1]
      REAL(KIND(1D0)) :: flowchange = 0.0D0 ! Difference in input and output flows for water surface
      REAL(KIND(1D0)) :: n_buildings = 0.0D0 ! n_buildings
      REAL(KIND(1D0)) :: h_std = 0.0D0 ! zStd_RSL
      REAL(KIND(1D0)) :: lambda_c = 0.0D0 ! Building surface to plan area ratio [-]

      ! surface cover fractions related
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf = 0.0D0 !surface cover fraction[-]
      REAL(KIND(1D0)) :: VegFraction = 0.0D0 ! fraction of vegetation [-]
      REAL(KIND(1D0)) :: ImpervFraction = 0.0D0 !fractioin of impervious surface [-]
      REAL(KIND(1D0)) :: PervFraction = 0.0D0 !fraction of pervious surfaces [-]
      REAL(KIND(1D0)) :: NonWaterFraction = 0.0D0 !fraction of non-water [-]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: sfr_roof !fraction of roof facets [-]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: sfr_wall !fraction of wall facets [-]

      INTEGER :: nlayer ! number of vertical layers in urban canyon [-]
      TYPE(SPARTACUS_PRM) :: spartacus
      TYPE(LUMPS_PRM) :: lumps
      TYPE(EHC_PRM) :: ehc
      TYPE(SPARTACUS_LAYER_PRM) :: spartacus_layer
      TYPE(SURF_STORE_PRM) :: surf_store
      TYPE(IRRIGATION_PRM) :: irrigation
      TYPE(anthroEMIS_PRM) :: anthroemis
      ! type(anthroHEAT_PRM) :: anthroheat
      TYPE(SNOW_PRM) :: snow
      TYPE(CONDUCTANCE_PRM) :: conductance
      TYPE(LC_PAVED_PRM) :: lc_paved
      TYPE(LC_BLDG_PRM) :: lc_bldg
      TYPE(LC_DECTR_PRM) :: lc_dectr
      TYPE(LC_EVETR_PRM) :: lc_evetr
      TYPE(LC_GRASS_PRM) :: lc_grass
      TYPE(LC_BSOIL_PRM) :: lc_bsoil
      TYPE(LC_WATER_PRM) :: lc_water

      TYPE(BUILDING_ARCHETYPE_PRM) :: building_archtype
      TYPE(STEBBS_PRM) :: stebbs

   CONTAINS
      PROCEDURE :: ALLOCATE => allocate_site_prm_c
      PROCEDURE :: DEALLOCATE => deallocate_site_prm_c
      PROCEDURE :: cal_surf => SUEWS_cal_surf_DTS

   END TYPE SUEWS_SITE

   ! ********** SUEWS_stateVars schema **********
   TYPE, PUBLIC :: flag_STATE
      LOGICAL :: flag_converge = .FALSE. ! flag for convergence of surface temperature
      INTEGER :: i_iter = 0 ! number of iterations for convergence
      INTEGER :: stebbs_bldg_init = 0 ! stebbs flag for building initialization

      ! flag for iteration safety - YES - as we this should be updated every iteration
      LOGICAL :: iter_safe = .TRUE.

   END TYPE flag_STATE

   TYPE, PUBLIC :: anthroEmis_STATE
      ! TODO: #242 split HDD_id into individual explicit variables
      REAL(KIND(1D0)), DIMENSION(12) :: HDD_id = 0.0D0 !Heating Degree Days [degC d]
      ! HDD_id:
      ! first half used for update through the day
      ! HDD_id(1) ---- Heating [degC]: used for accumulation during calculation
      ! HDD_id(2) ---- Cooling [degC]: used for accumulation during calculation
      ! HDD_id(3) ---- Daily mean temp [degC]: used for accumulation during calculation
      ! HDD_id(4) ----
      ! HDD_id(5) ---- Daily precip total [mm]
      ! HDD_id(6) ---- Days since rain [d]
      ! second half used for storage of the first half for the prevous day
      ! HDD_id(6+1) ---- Heating [degC]: used for accumulation during calculation
      ! HDD_id(6+2) ---- Cooling [degC]: used for accumulation during calculation
      ! HDD_id(6+3) ---- Daily mean temp [degC]: used for accumulation during calculation
      ! HDD_id(6+4) ---- 5-day running mean temp [degC]: used for actual calculation
      ! HDD_id(6+5) ---- Daily precip total [mm]
      ! HDD_id(6+6) ---- Days since rain [d]

      REAL(KIND(1D0)) :: Fc = 0.0D0 !total co2 flux [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_anthro = 0.0D0 !anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_biogen = 0.0D0 !biogenic CO2 flux [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_build = 0.0D0 ! anthropogenic co2 flux  [umol m-2 s-1]

      REAL(KIND(1D0)) :: Fc_metab = 0.0D0 ! co2 emission from metabolism component [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_photo = 0.0D0 !co2 flux from photosynthesis [umol m
      REAL(KIND(1D0)) :: Fc_point = 0.0D0 ! co2 emission from point source [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_respi = 0.0D0 !co2 flux from respiration [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_traff = 0.0D0 ! co2 emission from traffic component [umol m-2 s-1]

      ! flag for iteration safety - NO
      ! HDD_id includes extensive quantities and thus cannot be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.
   END TYPE anthroEmis_STATE

   TYPE, PUBLIC :: OHM_STATE
      REAL(KIND(1D0)) :: qn_av = 0.0D0 ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: dqndt = 0.0D0 ! rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)) :: qn_s_av = 0.0D0 ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)) :: dqnsdt = 0.0D0 ! Rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)) :: a1 = 0.0D0 !AnOHM coefficients of grid [-]
      REAL(KIND(1D0)) :: a2 = 0.0D0 ! AnOHM coefficients of grid [h]
      REAL(KIND(1D0)) :: a3 = 0.0D0 !AnOHM coefficients of grid [W m-2]
      ! all variables are intensive and thus can be used for iteration safety
      REAL(KIND(1D0)) :: t2_prev = 0.0D0 ! previous day midnight air temperature [degC]
      REAL(KIND(1D0)) :: ws_rav = 0.0D0 ! running average of wind speed [m s-1]
      REAL(KIND(1D0)) :: tair_prev = 0.0D0
      REAL(KIND(1D0)) :: qn_rav = 0.0D0 ! running average of net radiation [W m-2]
      REAL(KIND(1D0)) :: a1_bldg = 0.0D0 ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a2_bldg = 0.0D0 ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a3_bldg = 0.0D0 ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a1_paved = 0.0D0! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a2_paved = 0.0D0! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a3_paved = 0.0D0! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a1_evetr = 0.0D0! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a2_evetr = 0.0D0! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a3_evetr = 0.0D0! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a1_dectr = 0.0D0! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a2_dectr = 0.0D0! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a3_dectr = 0.0D0! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a1_grass = 0.0D0! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a2_grass = 0.0D0! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a3_grass = 0.0D0! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a1_bsoil = 0.0D0! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a2_bsoil = 0.0D0! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a3_bsoil = 0.0D0! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a1_water = 0.0D0! Dynamic OHM coefficients of water
      REAL(KIND(1D0)) :: a2_water = 0.0D0! Dynamic OHM coefficients of water
      REAL(KIND(1D0)) :: a3_water = 0.0D0! Dynamic OHM coefficients of water
      ! flag for iteration safety - YES
      LOGICAL :: iter_safe = .TRUE.
   END TYPE OHM_STATE

   TYPE, PUBLIC :: solar_State
      REAL(KIND(1D0)) :: azimuth_deg = 0.0D0 !solar azimuth [angle]
      REAL(KIND(1D0)) :: ZENITH_deg = 0.0D0 !solar zenith angle [deg]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .TRUE.
   END TYPE solar_State

   TYPE, PUBLIC :: atm_state
      REAL(KIND(1D0)) :: fcld = 0.0D0 !estomated cloud fraction [-]
      REAL(KIND(1D0)) :: avcp = 0.0D0 !Specific heat capacity
      REAL(KIND(1D0)) :: dens_dry = 0.0D0 !Dry air density kg m-3
      REAL(KIND(1D0)) :: avdens = 0.0D0 !Average air density
      REAL(KIND(1D0)) :: dq = 0.0D0 !Specific humidity deficit
      REAL(KIND(1D0)) :: Ea_hPa = 0.0D0 !Water vapour pressure in hPa
      REAL(KIND(1D0)) :: Es_hPa = 0.0D0 !Saturation vapour pressure in hPa
      REAL(KIND(1D0)) :: lv_J_kg = 0.0D0 !Latent heat of vaporization in [J kg-1]
      REAL(KIND(1D0)) :: lvS_J_kg = 0.0D0 !latent heat of sublimation [J kg-1]
      REAL(KIND(1D0)) :: tlv = 0.0D0 !Latent heat of vaporization per timestep [J kg-1 s-1] (tlv=lv_J_kg/tstep_real)
      REAL(KIND(1D0)) :: psyc_hPa = 0.0D0 !Psychometric constant in hPa
      REAL(KIND(1D0)) :: psycIce_hPa = 0.0D0 !Psychometric constant in hPa for snow
      REAL(KIND(1D0)) :: s_Pa = 0.0D0 !Vapour pressure versus temperature slope in Pa
      REAL(KIND(1D0)) :: s_hpa = 0.0D0 !Vapour pressure versus temperature slope in hPa
      REAL(KIND(1D0)) :: sIce_hpa = 0.0D0 !Vapour pressure versus temperature slope in hPa above ice/snow
      REAL(KIND(1D0)) :: vpd_hPa = 0.0D0 !Vapour pressure deficit in hPa
      REAL(KIND(1D0)) :: vpd_pa = 0.0D0 !Vapour pressure deficit in Pa
      REAL(KIND(1D0)) :: U10_ms = 0.0D0 !average wind speed at 10m [W m-1]
      REAL(KIND(1D0)) :: U_hbh = 0.0D0 ! wind speed at half building height [m s-1]
      REAL(KIND(1D0)) :: T2_C = 0.0D0 !modelled 2 meter air temperature [degC]
      REAL(KIND(1D0)) :: T_hbh_C = 0.0D0 ! air temperature at half building height [Deg C]
      REAL(KIND(1D0)) :: q2_gkg = 0.0D0 ! Air specific humidity at 2 m [g kg-1]
      REAL(KIND(1D0)) :: RH2 = 0.0D0 ! air relative humidity at 2m [-]
      REAL(KIND(1D0)) :: L_mod = 0.0D0 !Obukhov length [m]
      REAL(KIND(1D0)) :: zL = 0.0D0 ! Stability scale [-]
      REAL(KIND(1D0)) :: RA_h = 0.0D0 ! aerodynamic resistance [s m-1]
      REAL(KIND(1D0)) :: RS = 0.0D0 ! surface resistance [s m-1]
      REAL(KIND(1D0)) :: UStar = 0.0D0 !friction velocity [m s-1]
      REAL(KIND(1D0)) :: TStar = 0.0D0 !T*, temperature scale [-]
      REAL(KIND(1D0)) :: RB = 0.0D0 !boundary layer resistance shuttleworth
      REAL(KIND(1D0)) :: Tair_av = 0.0D0 ! 5-day moving average of air temperature [degC]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: rss_surf = 0.0D0 ! surface resistance adjusted by surface wetness state[s m-1]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .TRUE.
   END TYPE atm_state

   TYPE, PUBLIC :: PHENOLOGY_STATE
      REAL(KIND(1D0)), DIMENSION(NSURF) :: alb = 0.0D0
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: lai_id = 0.0D0 ! Initial LAI values.
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDD_id = 0.0D0 ! Growing Degree Days [degC](see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDD_id = 0.0D0 ! Senescence Degree Days [degC](see SUEWS_DailyState.f95)
      REAL(KIND(1D0)) :: VegPhenLumps = 0.0D0 ! phenology indicator used lumps [-] - NOT USED - TO BE REMOVED
      REAL(KIND(1D0)) :: porosity_id = 0.0D0 ! porosity of each surface type [-]
      REAL(KIND(1D0)) :: decidcap_id = 0.0D0 ! Storage capacity of deciduous surface `DecTr`; updated each day in simulaiton due to changes in LAI.
      REAL(KIND(1D0)) :: albDecTr_id = 0.0D0 ! albedo of deciduous tree surface [-]
      REAL(KIND(1D0)) :: albEveTr_id = 0.0D0 ! albedo of evergreen tree surface [-]
      REAL(KIND(1D0)) :: albGrass_id = 0.0D0 ! albedo of grass surface [-]
      REAL(KIND(1D0)) :: Tmin_id = 0.0D0 ! Daily minimum temperature [degC]
      REAL(KIND(1D0)) :: Tmax_id = 0.0D0 ! Daily maximum temperature [degC]
      REAL(KIND(1D0)) :: lenDay_id = 0.0D0 ! daytime length [h]
      REAL(KIND(1D0)) :: TempVeg = 0.0D0 ! temporary vegetative surface fraction adjusted by rainfall [-]
      REAL(KIND(1D0)), DIMENSION(6, NSURF) :: StoreDrainPrm = 0.0D0 ! coefficients used in drainage calculation [-]

      REAL(KIND(1D0)) :: gfunc = 0.0D0 ! stomatal conductance function [-]
      REAL(KIND(1D0)) :: gsc = 0.0D0 !Surface Layer Conductance [s m-1]
      REAL(KIND(1D0)) :: g_kdown = 0.0D0 ! surface conductance function for shortwave radiation [-]
      REAL(KIND(1D0)) :: g_dq = 0.0D0 ! surface conductance function for specific humidity [-]
      REAL(KIND(1D0)) :: g_ta = 0.0D0 ! surface conductance function for air temperature [-]
      REAL(KIND(1D0)) :: g_smd = 0.0D0 ! surface conductance function for soil moisture deficit [-]
      REAL(KIND(1D0)) :: g_lai = 0.0D0 ! surface conductance function for LAI [-]

      ! flag for iteration safety - NO
      ! GDD_id and SDD_id include extensive quantities and thus cannot be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.

   END TYPE PHENOLOGY_STATE

   TYPE, PUBLIC :: SNOW_STATE
      REAL(KIND(1D0)) :: snowfallCum = 0.0D0 ! cumulative snowfall [mm]
      REAL(KIND(1D0)) :: snowalb = 0.0D0 ! albedo of snow [-]
      REAL(KIND(1D0)) :: chSnow_per_interval = 0.0D0 ! change state_id of snow and surface per time interval [mm]
      REAL(KIND(1D0)) :: mwh = 0.0D0 !snowmelt [mm]
      REAL(KIND(1D0)) :: mwstore = 0.0D0 !overall met water [mm]

      REAL(KIND(1D0)) :: qn_snow = 0.0D0 !net all-wave radiation on snow surface [W m-2]
      REAL(KIND(1D0)) :: Qm = 0.0D0 !Snowmelt-related heat [W m-2]
      REAL(KIND(1D0)) :: QmFreez = 0.0D0 !heat related to freezing of surface store [W m-2]
      REAL(KIND(1D0)) :: QmRain = 0.0D0 !melt heat for rain on snow [W m-2]

      REAL(KIND(1D0)) :: swe = 0.0D0 !overall snow water equavalent[mm]

      REAL(KIND(1D0)) :: z0vSnow = 0.0D0 !roughness for heat [m]
      REAL(KIND(1D0)) :: RAsnow = 0.0D0 !Aerodynamic resistance for snow [s m-1]
      REAL(KIND(1D0)) :: sIce_hpa = 0.0D0 !satured curve on snow [hPa]

      REAL(KIND(1D0)), DIMENSION(2) :: SnowRemoval = 0.0D0 !snow removal [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: icefrac = 0.0D0 ! fraction of ice in snowpack [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowdens = 0.0D0 ! snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowfrac = 0.0D0 ! snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowpack = 0.0D0 ! snow water equivalent on each land cover [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowwater = 0.0D0 ! snow water[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind_snow = 0.0D0 !outgoing shortwave on snowpack [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: qn_ind_snow = 0.0D0 !net all-wave radiation on snowpack [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: deltaQi = 0.0D0 ! storage heat flux of snow surfaces [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Tsurf_ind_snow = 0.0D0 !snowpack surface temperature [C]

      ! flag for iteration safety - NO
      ! multiple variables (e.g. snowpack, snowwater, snowfallCum, etc) include extensive quantities and thus cannot be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.
   END TYPE SNOW_STATE

   TYPE, PUBLIC :: HYDRO_STATE
      ! REAL(KIND(1D0)) :: runofftowater   ! Fraction of above-ground runoff flowing to water surface during flooding
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_surf = 0.0D0 ! Initial water stored in soil beneath `Bldgs` surface
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_surf = 0.0D0 ! Initial wetness condition on SUEWS land covers.

      ! ==================================================
      ! TODO: #243 split WUDay_id into individual explicit variables
      REAL(KIND(1D0)), DIMENSION(9) :: WUDay_id = 0.0D0 ! Daily water use for EveTr, DecTr, Grass [mm]
      ! WUDay_id:
      ! WUDay_id(1) - Daily water use total for Irr EveTr (automatic+manual) [mm]
      ! WUDay_id(2) - Automatic irrigation for Irr EveTr [mm]
      ! WUDay_id(3) - Manual irrigation for Irr EveTr [mm]
      ! WUDay_id(4) - Daily water use total for Irr DecTr (automatic+manual) [mm]
      ! WUDay_id(5) - Automatic irrigation for Irr DecTr [mm]
      ! WUDay_id(6) - Manual irrigation for Irr DecTr [mm]
      ! WUDay_id(7) - Daily water use total for Irr Grass (automatic+manual) [mm]
      ! WUDay_id(8) - Automatic irrigation for Irr Grass [mm]
      ! WUDay_id(9) - Manual irrigation for Irr Grass [mm]
      ! ==================================================

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soilstore_roof ! Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_roof ! wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soilstore_wall ! Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_wall ! wetness status of wall [mm]

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: ev_roof ! evapotranspiration of each roof layer [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: ev_wall ! evapotranspiration of each wall type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev0_surf = 0.0D0 ! evapotranspiration from PM of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev_surf = 0.0D0 ! evapotranspiration of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: wu_surf = 0.0D0 !external water use of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: runoffSoil = 0.0D0 !Soil runoff from each soil sub-surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: smd_surf = 0.0D0 !soil moisture deficit for each surface
      REAL(KIND(1D0)), DIMENSION(NSURF) :: drain_surf = 0.0D0 !drainage of each surface type [mm]

      REAL(KIND(1D0)) :: drain_per_tstep = 0.0D0 ! total drainage for all surface type at each timestep [mm]
      REAL(KIND(1D0)) :: ev_per_tstep = 0.0D0 ! evaporation at each time step [mm]
      REAL(KIND(1D0)) :: wu_ext = 0.0D0 !external water use [mm]
      REAL(KIND(1D0)) :: wu_int = 0.0D0 !internal water use [mm]

      REAL(KIND(1D0)) :: runoffAGveg = 0.0D0 !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)) :: runoffAGimpervious = 0.0D0 !Above ground runoff from impervious surface for all surface area [mm]
      REAL(KIND(1D0)) :: runoff_per_tstep = 0.0D0 !runoff water at each time step [mm]
      REAL(KIND(1D0)) :: runoffPipes = 0.0 !runoff to pipes [mm]
      REAL(KIND(1D0)) :: runoffSoil_per_tstep = 0.0D0 !Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
      REAL(KIND(1D0)) :: runoffwaterbody = 0.0D0 !Above ground runoff from water body for all surface area [mm]
      REAL(KIND(1D0)) :: smd = 0.0D0 !soil moisture deficit [mm]
      REAL(KIND(1D0)) :: SoilState = 0.0D0 !Area-averaged soil moisture  for whole surface [mm]
      REAL(KIND(1D0)) :: state_per_tstep = 0.0D0 !state_id at each timestep [mm]
      REAL(KIND(1D0)) :: surf_chang_per_tstep = 0.0D0 !change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)) :: tot_chang_per_tstep = 0.0D0 !Change in surface state_id [mm]
      REAL(KIND(1D0)) :: runoff_per_interval = 0.0D0 !run-off at each time interval [mm]
      REAL(KIND(1D0)) :: NWstate_per_tstep = 0.0D0 ! state_id at each tinestep(excluding water body) [mm]

      REAL(KIND(1D0)) :: SoilMoistCap !Maximum capacity of soil store [mm]
      REAL(KIND(1D0)) :: vsmd !Soil moisture deficit for vegetated surfaces only [mm]

      ! TODO: TS 25 Oct 2017
      ! the  variables are not used currently as grid-to-grid connection is NOT set up.
      ! set these variables as zero.
      REAL(KIND(1D0)) :: AdditionalWater = 0.0D0 !!Additional water coming from other grids [mm] (these are expressed as depths over the whole surface)
      REAL(KIND(1D0)) :: addImpervious = 0.0D0
      REAL(KIND(1D0)) :: addPipes = 0.0D0
      REAL(KIND(1D0)) :: addVeg = 0.0D0
      REAL(KIND(1D0)) :: addWaterBody = 0.0D0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: AddWater = 0.0D0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: frac_water2runoff = 0.0D0

      ! flag for iteration safety - NO
      ! multiple variables (e.g. soilstore_surf, state_surf, etc) include extensive quantities and thus cannot be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.
   CONTAINS
      PROCEDURE :: ALLOCATE => allocHydroState_c
      PROCEDURE :: DEALLOCATE => deallocHydroState_c
   END TYPE HYDRO_STATE

   TYPE, PUBLIC :: HEAT_STATE
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_roof ! interface temperature between depth layers in roof [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_wall ! interface temperature between depth layers in wall [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_surf ! interface temperature between depth layers [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_surf_dyohm ! interface temperature between depth layers [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof ! roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_wall ! wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf ! surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf_dyohm! surface temperature [degC]
      ! surface temperature saved at the beginning of the time step - not updated during the time step
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof_stepstart !surface temperature of roof saved at the beginning of the time step [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_wall_stepstart !surface temperature of wall saved at the beginning of the time step [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf_stepstart !surface temperature saved at the beginning of the time step [degC]

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: QS_roof ! heat storage flux for roof component [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: QN_roof ! net all-wave radiation of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qe_roof ! latent heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qh_roof ! sensible heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qh_resist_roof ! resist-based sensible heat flux of roof surface [W m-2]

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: QS_wall ! heat storage flux for wall component [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: QN_wall ! net all-wave radiation of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qe_wall ! latent heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qh_wall ! sensible heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qh_resist_wall ! resistance based sensible heat flux of wall surface [W m-2]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: qs_surf = 0.0D0 ! aggregated heat storage of of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: QN_surf = 0.0D0 ! net all-wave radiation of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe0_surf = 0.0D0 ! latent heat flux from PM of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe_surf = 0.0D0 ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_surf = 0.0D0 ! sensinle heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_resist_surf = 0.0D0 ! resistance based sensible heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: tsurf_ind = 0.0D0 !snow-free surface temperature [degC]

      REAL(KIND(1D0)) :: QH_LUMPS = 0.0D0 !turbulent sensible heat flux from LUMPS model [W m-2]
      REAL(KIND(1D0)) :: QE_LUMPS = 0.0D0 !turbulent latent heat flux by LUMPS model [W m-2]

      REAL(KIND(1D0)) :: kclear = 0.0D0 !clear sky incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: kup = 0.0D0 !outgoing shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: ldown = 0.0D0 !incoming longtwave radiation [W m-2]
      REAL(KIND(1D0)) :: lup = 0.0D0 !outgoing longwave radiation [W m-2]

      REAL(KIND(1D0)) :: qe = 0.0D0 !turbuent latent heat flux [W m-2]
      REAL(KIND(1D0)) :: qf = 0.0D0 !anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)) :: QF_SAHP = 0.0D0 !total anthropogeic heat flux when EmissionMethod is not 0 [W m-2]
      REAL(KIND(1D0)) :: qh = 0.0D0 !turbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_residual = 0.0D0 ! residual based sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_resist = 0.0D0 !resistance bnased sensible heat flux [W m-2]

      REAL(KIND(1D0)) :: qn = 0.0D0 !net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: qn_snowfree = 0.0D0 !net all-wave radiation on snow-free surface [W m-2]
      REAL(KIND(1D0)) :: qs = 0.0D0 !heat storage flux [W m-2]

      REAL(KIND(1D0)) :: TSfc_C = 0.0D0 ! surface temperature [degC]
      REAL(KIND(1D0)) :: tsurf = 0.0D0 !surface temperatue [degC]
      REAL(KIND(1D0)) :: QH_Init = 0.0D0 !initialised sensible heat flux [W m-2]

      REAL(KIND(1D0)), DIMENSION(15) :: roof_in_sw_spc ! incoming shortwave radiation on roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(15) :: roof_in_lw_spc ! incoming longwave radiation on roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(15) :: wall_in_sw_spc ! incoming shortwave radiation on wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(15) :: wall_in_lw_spc ! incoming longwave radiation on wall surface [W m-2]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .TRUE.
   CONTAINS
      PROCEDURE :: ALLOCATE => allocHeatState_c
      PROCEDURE :: DEALLOCATE => deallocHeatState_c
   END TYPE HEAT_STATE

   TYPE, PUBLIC :: ROUGHNESS_STATE
      ! this type is used to collect the intermediate results in the SUEWS model

      ! calculated values of FAI
      REAL(KIND(1D0)) :: FAIBldg_use = 0.0D0 ! frontal area index of buildings [-]
      REAL(KIND(1D0)) :: FAIEveTree_use = 0.0D0 ! frontal area index of evergreen trees [-]
      REAL(KIND(1D0)) :: FAIDecTree_use = 0.0D0 ! frontal area index of deciduous trees [-]

      REAL(KIND(1D0)) :: FAI = 0.0D0 ! frontal area index [-]
      REAL(KIND(1D0)) :: PAI = 0.0D0 ! plan area index [-]
      REAL(KIND(1D0)) :: Zh = 0.0D0 ! effective height of bluff bodies [m]
      REAL(KIND(1D0)) :: z0m = 0.0D0 ! aerodynamic roughness length [m]
      REAL(KIND(1D0)) :: z0v = 0.0D0 ! roughness for heat [m]
      REAL(KIND(1D0)) :: zdm = 0.0D0 ! zero-plance displacement [m]
      REAL(KIND(1D0)) :: ZZD = 0.0D0 ! z-zdm [m]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .TRUE.

   END TYPE ROUGHNESS_STATE

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
      REAL(KIND(1D0)) :: BVF_extwall = 0.0D0 
      REAL(KIND(1D0)) :: GVF_extwall = 0.0D0 
      REAL(KIND(1D0)) :: SVF_extwall = 0.0D0 
      REAL(KIND(1D0)) :: BVF_extroof = 0.0D0 
      REAL(KIND(1D0)) :: GVF_extroof = 0.0D0 
      REAL(KIND(1D0)) :: SVF_extroof = 0.0D0 
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

   END TYPE

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

   TYPE, PUBLIC :: SUEWS_STATE
      TYPE(flag_STATE) :: flagState
      TYPE(anthroEmis_STATE) :: anthroemisState
      TYPE(OHM_STATE) :: ohmState
      TYPE(solar_State) :: solarState
      TYPE(atm_state) :: atmState
      TYPE(PHENOLOGY_STATE) :: phenState
      TYPE(SNOW_STATE) :: snowState
      TYPE(HYDRO_STATE) :: hydroState
      TYPE(HEAT_STATE) :: heatState
      TYPE(ROUGHNESS_STATE) :: roughnessState
      TYPE(STEBBS_STATE) :: stebbsState
      TYPE(NHOOD_STATE) :: nhoodState

   CONTAINS
      PROCEDURE :: ALLOCATE => allocSUEWSState_c
      PROCEDURE :: DEALLOCATE => deallocSUEWSState_c
      PROCEDURE :: reset_atm_state => reset_atm_state
      PROCEDURE :: check_and_reset_states => check_and_reset_unsafe_states
   END TYPE SUEWS_STATE

   ! ********** SUEWS_forcing schema **********
   TYPE, PUBLIC :: SUEWS_FORCING
      REAL(KIND(1D0)) :: kdown = 0.0D0 !
      REAL(KIND(1D0)) :: ldown = 0.0D0 !
      REAL(KIND(1D0)) :: RH = 0.0D0 !
      REAL(KIND(1D0)) :: pres = 0.0D0 !
      REAL(KIND(1D0)) :: Tair_av_5d = 0.0D0 ! 5-day moving average of air temperature [degC]
      REAL(KIND(1D0)) :: U = 0.0D0 !
      REAL(KIND(1D0)) :: rain = 0.0D0 !
      REAL(KIND(1D0)) :: Wu_m3 = 0.0D0 !  external water use amount in m3 for each timestep
      REAL(KIND(1D0)) :: fcld = 0.0D0 !
      REAL(KIND(1D0)) :: LAI_obs = 0.0D0 !
      REAL(KIND(1D0)) :: snowfrac = 0.0D0 !
      REAL(KIND(1D0)) :: xsmd = 0.0D0 !
      REAL(KIND(1D0)) :: qf_obs = 0.0D0 !
      REAL(KIND(1D0)) :: qn1_obs = 0.0D0 !
      REAL(KIND(1D0)) :: qs_obs = 0.0D0 !
      REAL(KIND(1D0)) :: temp_c = 0.0D0 !
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Ts5mindata_ir !surface temperature input data[degC] used in ESTM --> may be deprecated in the future once EHC is more mature
   END TYPE SUEWS_FORCING

   TYPE, PUBLIC :: SUEWS_TIMER
      ! Timer type containing temporal information for SUEWS calculations
      ! Note: tstep_prev is retained for WRF-SUEWS coupling where adaptive timesteps are used
      INTEGER :: id = 0 !
      INTEGER :: imin = 0 !
      INTEGER :: isec = 0 !
      INTEGER :: it = 0 ! Hour of day
      INTEGER :: iy = 0 !
      INTEGER :: tstep = 0 ! Current timestep [s]
      INTEGER :: tstep_prev = 0 ! Previous timestep [s] - for WRF adaptive stepping (equals tstep in standalone)
      INTEGER :: dt_since_start = 0 !
      INTEGER :: dt_since_start_prev = 0 !

      ! values that are derived from tstep
      INTEGER :: nsh = 0 ! number of timesteps per hour
      REAL(KIND(1D0)) :: nsh_real = 0.0D0 ! nsh in type real [-]
      REAL(KIND(1D0)) :: tstep_real = 0.0D0 ! tstep in type real
      REAL(KIND(1D0)) :: dectime = 0.0D0 !decimal time [-]

      INTEGER, DIMENSION(3) :: dayofWeek_id = 0 ! 1 - day of week; 2 - month; 3 - season

      INTEGER :: DLS = 0 !daylight saving time offset [h]

      INTEGER :: new_day = 0 ! flag to indicate a new day !TODO: Should this be bool?

   END TYPE SUEWS_TIMER

   TYPE, PUBLIC :: output_block
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockSUEWS
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockSnow
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockESTM
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockEHC
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockRSL
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockBEERS
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockDebug
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockSPARTACUS
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockDailyState
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockSTEBBS
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutBlockNHood
   CONTAINS
      ! Procedures
      PROCEDURE :: init => output_block_init
      ! PROCEDURE :: finalize => output_block_finalize
      PROCEDURE :: cleanup => output_block_finalize
   END TYPE output_block

   TYPE, PUBLIC :: output_line
      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine = -999 !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS) :: dataOutLineSUEWS = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow) :: dataOutLineSnow = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM) :: dataOutLineESTM = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutEHC) :: dataOutLineEHC = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL) :: dataoutLineRSL = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS) :: dataOutLineBEERS = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug) :: dataOutLineDebug = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS) :: dataOutLineSPARTACUS = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState) :: dataOutLineDailyState = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSTEBBS) :: dataOutLineSTEBBS = -999
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutNHood) :: dataOutLineNHood = -999

   CONTAINS
      ! Procedures
      PROCEDURE :: init => output_line_init
   END TYPE output_line

   TYPE, PUBLIC :: SUEWS_DEBUG
      ! This type stores the model states for debugging purposes.
      ! The states are captured at the completion of each physical module.
      ! Naming convention: state_XX_YYY
      ! XX: sequence number in the main SUEWS calculation
      ! YYY: name of the physical module
      TYPE(SUEWS_STATE) :: state_01_dailystate
      TYPE(SUEWS_STATE) :: state_02_soilmoist
      TYPE(SUEWS_STATE) :: state_03_wateruse
      TYPE(SUEWS_STATE) :: state_04_anthroemis
      TYPE(SUEWS_STATE) :: state_05_qn
      TYPE(SUEWS_STATE) :: state_06_qs
      TYPE(SUEWS_STATE) :: state_07_qhqe_lumps
      TYPE(SUEWS_STATE) :: state_08_water
      TYPE(SUEWS_STATE) :: state_09_resist
      TYPE(SUEWS_STATE) :: state_10_qe
      TYPE(SUEWS_STATE) :: state_11_qh
      TYPE(SUEWS_STATE) :: state_12_tsurf
      TYPE(SUEWS_STATE) :: state_13_rsl
      TYPE(SUEWS_STATE) :: state_14_biogenco2
      TYPE(SUEWS_STATE) :: state_15_beers
      ! TYPE(SUEWS_STATE) :: state_snow ! unavailable - to be added #234
   CONTAINS
      PROCEDURE :: init => init_suews_debug
   END TYPE SUEWS_DEBUG

   TYPE, PUBLIC :: SUEWS_STATE_BLOCK
      TYPE(SUEWS_STATE), DIMENSION(:), ALLOCATABLE :: BLOCK
   CONTAINS
      PROCEDURE :: init => init_suews_state_block
   END TYPE SUEWS_STATE_BLOCK

CONTAINS

   SUBROUTINE init_suews_state_block(self, nlayer, ndepth, len_sim)
      CLASS(SUEWS_STATE_BLOCK), INTENT(inout) :: self
      INTEGER, INTENT(in) :: nlayer, ndepth
      INTEGER, INTENT(in) :: len_sim
      INTEGER :: ir

      ! allocate debug_state_block
      ALLOCATE (self%BLOCK(len_sim))

      ! initialise each element
      DO ir = 1, len_sim
         CALL self%BLOCK(ir)%ALLOCATE(nlayer, ndepth)
      END DO
   END SUBROUTINE init_suews_state_block

   ! SUBROUTINE dealloc_suews_debug_block(self)
   !    CLASS(SUEWS_DEBUG_BLOCK), INTENT(inout) :: self

   !    ! deallocate each element
   !    DO ir = 1, len_sim
   !       CALL self(ir)%dealloc()
   !    END DO
   ! END SUBROUTINE dealloc_suews_debug_block

   SUBROUTINE init_suews_debug(self, nlayer, ndepth)
      CLASS(SUEWS_DEBUG), INTENT(inout) :: self
      INTEGER, INTENT(in) :: nlayer, ndepth

      ! Initialise the SUEWS_DEBUG type
      CALL self%state_01_dailystate%ALLOCATE(nlayer, ndepth)
      CALL self%state_02_soilmoist%ALLOCATE(nlayer, ndepth)
      CALL self%state_03_wateruse%ALLOCATE(nlayer, ndepth)
      CALL self%state_04_anthroemis%ALLOCATE(nlayer, ndepth)
      CALL self%state_05_qn%ALLOCATE(nlayer, ndepth)
      CALL self%state_06_qs%ALLOCATE(nlayer, ndepth)
      CALL self%state_09_resist%ALLOCATE(nlayer, ndepth)
      CALL self%state_10_qe%ALLOCATE(nlayer, ndepth)
      CALL self%state_11_qh%ALLOCATE(nlayer, ndepth)
      CALL self%state_12_tsurf%ALLOCATE(nlayer, ndepth)
      CALL self%state_13_rsl%ALLOCATE(nlayer, ndepth)
      CALL self%state_14_biogenco2%ALLOCATE(nlayer, ndepth)
      CALL self%state_15_beers%ALLOCATE(nlayer, ndepth)
      ! CALL self%state_snow%init() ! unavailable - to be added #234
   END SUBROUTINE init_suews_debug

   SUBROUTINE output_line_init(self)
      CLASS(output_line), INTENT(inout) :: self

      ! Set default values
      self%datetimeLine = -999.0
      self%dataOutLineSUEWS = -999.0
      self%dataOutLineSnow = -999.0
      self%dataOutLineESTM = -999.0
      self%dataOutLineEHC = -999.0
      self%dataOutLineRSL = -999.0
      self%dataOutLineBEERS = -999.0
      self%dataOutLineDebug = -999.0
      self%dataOutLineSPARTACUS = -999.0
      self%dataOutLineDailyState = -999.0
      self%dataOutLineSTEBBS = -999.0
      self%dataOutLineNHood = -999.0
   END SUBROUTINE output_line_init

   SUBROUTINE output_block_init(self, len)
      CLASS(output_block), INTENT(inout) :: self
      INTEGER, INTENT(in) :: len

      ! Allocate memory for the arrays
      ALLOCATE (self%dataOutBlockSUEWS(len, ncolumnsDataOutSUEWS))
      ALLOCATE (self%dataOutBlockSnow(len, ncolumnsDataOutSnow))
      ALLOCATE (self%dataOutBlockESTM(len, ncolumnsDataOutESTM))
      ALLOCATE (self%dataOutBlockEHC(len, ncolumnsDataOutEHC))
      ALLOCATE (self%dataOutBlockRSL(len, ncolumnsDataOutRSL))
      ALLOCATE (self%dataOutBlockBEERS(len, ncolumnsDataOutBEERS))
      ALLOCATE (self%dataOutBlockDebug(len, ncolumnsDataOutDebug))
      ALLOCATE (self%dataOutBlockSPARTACUS(len, ncolumnsDataOutSPARTACUS))
      ALLOCATE (self%dataOutBlockDailyState(len, ncolumnsDataOutDailyState))
      ALLOCATE (self%dataOutBlockSTEBBS(len, ncolumnsDataOutSTEBBS))
      ALLOCATE (self%dataOutBlockNHood(len, ncolumnsDataOutNHood))

      ! Set default values
      self%dataOutBlockSUEWS = -999.0
      self%dataOutBlockSnow = -999.0
      self%dataOutBlockESTM = -999.0
      self%dataOutBlockEHC = -999.0
      self%dataOutBlockRSL = -999.0
      self%dataOutBlockBEERS = -999.0
      self%dataOutBlockDebug = -999.0
      self%dataOutBlockSPARTACUS = -999.0
      self%dataOutBlockDailyState = -999.0
      self%dataOutBlockSTEBBS = -999.0
      self%dataOutBlockNHood = -999.0

   END SUBROUTINE output_block_init

   SUBROUTINE output_block_finalize(self)
      CLASS(output_block), INTENT(inout) :: self

      ! Deallocate memory for the arrays
      IF (ALLOCATED(self%dataOutBlockSUEWS)) DEALLOCATE (self%dataOutBlockSUEWS)
      IF (ALLOCATED(self%dataOutBlockSnow)) DEALLOCATE (self%dataOutBlockSnow)
      IF (ALLOCATED(self%dataOutBlockESTM)) DEALLOCATE (self%dataOutBlockESTM)
      IF (ALLOCATED(self%dataOutBlockEHC)) DEALLOCATE (self%dataOutBlockEHC)
      IF (ALLOCATED(self%dataOutBlockRSL)) DEALLOCATE (self%dataOutBlockRSL)
      IF (ALLOCATED(self%dataOutBlockBEERS)) DEALLOCATE (self%dataOutBlockBEERS)
      IF (ALLOCATED(self%dataOutBlockDebug)) DEALLOCATE (self%dataOutBlockDebug)
      IF (ALLOCATED(self%dataOutBlockSPARTACUS)) DEALLOCATE (self%dataOutBlockSPARTACUS)
      IF (ALLOCATED(self%dataOutBlockDailyState)) DEALLOCATE (self%dataOutBlockDailyState)
      IF (ALLOCATED(self%dataOutBlockSTEBBS)) DEALLOCATE (self%dataOutBlockSTEBBS)
      IF (ALLOCATED(self%dataOutBlockNHood)) DEALLOCATE (self%dataOutBlockNHood)

   END SUBROUTINE output_block_finalize

   SUBROUTINE allocSUEWSState_c(self, nlayer, ndepth)
      IMPLICIT NONE
      CLASS(SUEWS_STATE), INTENT(inout) :: self
      INTEGER, INTENT(in) :: nlayer
      INTEGER, INTENT(in) :: ndepth

      CALL self%DEALLOCATE()
      CALL self%hydroState%ALLOCATE(nlayer)
      CALL self%heatState%ALLOCATE(nsurf, nlayer, ndepth)

   END SUBROUTINE allocSUEWSState_c

   SUBROUTINE deallocSUEWSState_c(self)
      IMPLICIT NONE
      CLASS(SUEWS_STATE), INTENT(inout) :: self

      CALL self%hydroState%DEALLOCATE()
      CALL self%heatState%DEALLOCATE()

   END SUBROUTINE deallocSUEWSState_c

   SUBROUTINE reset_atm_state(self)
      ! Reset atmospheric state variables to prevent state pollution
      ! between different simulation runs in the same Python process
      IMPLICIT NONE
      CLASS(SUEWS_STATE), INTENT(inout) :: self

      ! Reset the critical atmospheric state variables that cause QE/QH discrepancies
      self%atmState%RA_h = 0.0D0
      self%atmState%RS = 0.0D0
      self%atmState%UStar = 0.0D0
      self%atmState%TStar = 0.0D0
      self%atmState%RB = 0.0D0
      self%atmState%L_mod = 0.0D0
      self%atmState%zL = 0.0D0
      self%atmState%rss_surf = 0.0D0

   END SUBROUTINE reset_atm_state

   SUBROUTINE allocate_spartacus_layer_prm_c(self, nlayer)
      IMPLICIT NONE
      CLASS(SPARTACUS_LAYER_PRM), INTENT(inout) :: self
      INTEGER, INTENT(in) :: nlayer

      CALL self%DEALLOCATE()
      ALLOCATE (self%building_frac(nlayer))
      ALLOCATE (self%building_scale(nlayer))
      ALLOCATE (self%veg_frac(nlayer))
      ALLOCATE (self%veg_scale(nlayer))
      ALLOCATE (self%alb_roof(nlayer))
      ALLOCATE (self%emis_roof(nlayer))
      ALLOCATE (self%alb_wall(nlayer))
      ALLOCATE (self%emis_wall(nlayer))
      ALLOCATE (self%roof_albedo_dir_mult_fact(nspec, nlayer))
      ALLOCATE (self%wall_specular_frac(nspec, nlayer))

   END SUBROUTINE allocate_spartacus_layer_prm_c

   SUBROUTINE deallocate_spartacus_layer_prm_c(self)
      IMPLICIT NONE
      CLASS(SPARTACUS_LAYER_PRM), INTENT(inout) :: self

      IF (ALLOCATED(self%building_frac)) DEALLOCATE (self%building_frac)
      IF (ALLOCATED(self%building_scale)) DEALLOCATE (self%building_scale)
      IF (ALLOCATED(self%veg_frac)) DEALLOCATE (self%veg_frac)
      IF (ALLOCATED(self%veg_scale)) DEALLOCATE (self%veg_scale)
      IF (ALLOCATED(self%alb_roof)) DEALLOCATE (self%alb_roof)
      IF (ALLOCATED(self%emis_roof)) DEALLOCATE (self%emis_roof)
      IF (ALLOCATED(self%alb_wall)) DEALLOCATE (self%alb_wall)
      IF (ALLOCATED(self%emis_wall)) DEALLOCATE (self%emis_wall)
      IF (ALLOCATED(self%roof_albedo_dir_mult_fact)) DEALLOCATE (self%roof_albedo_dir_mult_fact)
      IF (ALLOCATED(self%wall_specular_frac)) DEALLOCATE (self%wall_specular_frac)

   END SUBROUTINE deallocate_spartacus_layer_prm_c

   SUBROUTINE allocHydroState_c(self, nlayer)
      IMPLICIT NONE
      CLASS(HYDRO_STATE), INTENT(inout) :: self
      INTEGER, INTENT(in) :: nlayer
      !
      ! CALL allocate_hydro_state(self, nlayer)
      CALL self%DEALLOCATE()
      ALLOCATE (self%soilstore_roof(nlayer))
      ALLOCATE (self%state_roof(nlayer))
      ALLOCATE (self%soilstore_wall(nlayer))
      ALLOCATE (self%state_wall(nlayer))
      ALLOCATE (self%ev_roof(nlayer))
      ALLOCATE (self%ev_wall(nlayer))
      !
   END SUBROUTINE allocHydroState_c

   SUBROUTINE deallocHydroState_c(self)
      IMPLICIT NONE
      CLASS(HYDRO_STATE), INTENT(inout) :: self
      !
      ! CALL dealloc_hydro_state(self)
      IF (ALLOCATED(self%soilstore_roof)) DEALLOCATE (self%soilstore_roof)
      IF (ALLOCATED(self%state_roof)) DEALLOCATE (self%state_roof)
      IF (ALLOCATED(self%soilstore_wall)) DEALLOCATE (self%soilstore_wall)
      IF (ALLOCATED(self%state_wall)) DEALLOCATE (self%state_wall)
      IF (ALLOCATED(self%ev_roof)) DEALLOCATE (self%ev_roof)
      IF (ALLOCATED(self%ev_wall)) DEALLOCATE (self%ev_wall)
      !
   END SUBROUTINE deallocHydroState_c

   SUBROUTINE allocHeatState_c(self, num_surf, num_layer, num_depth)
      IMPLICIT NONE
      CLASS(HEAT_STATE), INTENT(inout) :: self
      INTEGER, INTENT(in) :: num_surf, num_layer, num_depth

      !
      ! CALL allocate_heat_state(self, num_surf, num_layer, num_depth)

      CALL self%DEALLOCATE()
      ALLOCATE (self%temp_roof(num_layer, num_depth))
      ALLOCATE (self%temp_wall(num_layer, num_depth))
      ALLOCATE (self%temp_surf(num_surf, num_depth))
      ALLOCATE (self%temp_surf_dyohm(num_surf, num_depth))

      ALLOCATE (self%tsfc_roof(num_layer))
      ALLOCATE (self%tsfc_wall(num_layer))
      ALLOCATE (self%tsfc_surf(num_surf))
      ALLOCATE (self%tsfc_surf_dyohm(num_surf))

      ALLOCATE (self%tsfc_roof_stepstart(num_layer))
      ALLOCATE (self%tsfc_wall_stepstart(num_layer))
      ALLOCATE (self%tsfc_surf_stepstart(num_surf))

      ALLOCATE (self%QS_roof(num_layer))
      ALLOCATE (self%QN_roof(num_layer))
      ALLOCATE (self%qe_roof(num_layer))
      ALLOCATE (self%qh_roof(num_layer))
      ALLOCATE (self%qh_resist_roof(num_layer))
      ALLOCATE (self%QS_wall(num_layer))
      ALLOCATE (self%QN_wall(num_layer))
      ALLOCATE (self%qe_wall(num_layer))
      ALLOCATE (self%qh_wall(num_layer))
      ALLOCATE (self%qh_resist_wall(num_layer))

   END SUBROUTINE allocHeatState_c

   SUBROUTINE deallocHeatState_c(self)
      IMPLICIT NONE
      CLASS(HEAT_STATE), INTENT(inout) :: self
      !
      ! CALL dealloc_heat_state(self)
      IF (ALLOCATED(self%temp_roof)) DEALLOCATE (self%temp_roof)
      IF (ALLOCATED(self%temp_wall)) DEALLOCATE (self%temp_wall)
      IF (ALLOCATED(self%tsfc_roof)) DEALLOCATE (self%tsfc_roof)
      IF (ALLOCATED(self%tsfc_wall)) DEALLOCATE (self%tsfc_wall)
      IF (ALLOCATED(self%tsfc_surf)) DEALLOCATE (self%tsfc_surf)
      IF (ALLOCATED(self%tsfc_surf_dyohm)) DEALLOCATE (self%tsfc_surf_dyohm)
      IF (ALLOCATED(self%tsfc_roof_stepstart)) DEALLOCATE (self%tsfc_roof_stepstart)
      IF (ALLOCATED(self%tsfc_wall_stepstart)) DEALLOCATE (self%tsfc_wall_stepstart)
      IF (ALLOCATED(self%tsfc_surf_stepstart)) DEALLOCATE (self%tsfc_surf_stepstart)
      IF (ALLOCATED(self%temp_surf)) DEALLOCATE (self%temp_surf)
      IF (ALLOCATED(self%temp_surf_dyOHM)) DEALLOCATE (self%temp_surf_dyOHM)
      IF (ALLOCATED(self%QS_roof)) DEALLOCATE (self%QS_roof)
      IF (ALLOCATED(self%QN_roof)) DEALLOCATE (self%QN_roof)
      IF (ALLOCATED(self%qe_roof)) DEALLOCATE (self%qe_roof)
      IF (ALLOCATED(self%qh_roof)) DEALLOCATE (self%qh_roof)
      IF (ALLOCATED(self%qh_resist_roof)) DEALLOCATE (self%qh_resist_roof)
      IF (ALLOCATED(self%QS_wall)) DEALLOCATE (self%QS_wall)
      IF (ALLOCATED(self%QN_wall)) DEALLOCATE (self%QN_wall)
      IF (ALLOCATED(self%qe_wall)) DEALLOCATE (self%qe_wall)
      IF (ALLOCATED(self%qh_wall)) DEALLOCATE (self%qh_wall)
      IF (ALLOCATED(self%qh_resist_wall)) DEALLOCATE (self%qh_resist_wall)

   END SUBROUTINE deallocHeatState_c

   SUBROUTINE allocate_ehc_prm_c(self, nlayer, ndepth)
      CLASS(EHC_PRM), INTENT(INOUT) :: self
      INTEGER, INTENT(IN) :: nlayer, ndepth

      ! CALL allocate_ehc_prm(self, nlayer, ndepth)
      CALL self%DEALLOCATE()
      ALLOCATE (self%soil_storecap_roof(nlayer))
      ALLOCATE (self%soil_storecap_wall(nlayer))
      ALLOCATE (self%state_limit_roof(nlayer))
      ALLOCATE (self%state_limit_wall(nlayer))
      ALLOCATE (self%wet_thresh_roof(nlayer))
      ALLOCATE (self%wet_thresh_wall(nlayer))
      ALLOCATE (self%tin_roof(nlayer))
      ALLOCATE (self%tin_wall(nlayer))
      ALLOCATE (self%tin_surf(nlayer))
      ALLOCATE (self%k_roof(nlayer, ndepth))
      ALLOCATE (self%k_wall(nlayer, ndepth))
      ALLOCATE (self%k_surf(nlayer, ndepth))
      ALLOCATE (self%cp_roof(nlayer, ndepth))
      ALLOCATE (self%cp_wall(nlayer, ndepth))
      ALLOCATE (self%cp_surf(nlayer, ndepth))
      ALLOCATE (self%dz_roof(nlayer, ndepth))
      ALLOCATE (self%dz_wall(nlayer, ndepth))
      ALLOCATE (self%dz_surf(nlayer, ndepth))

   END SUBROUTINE allocate_ehc_prm_c

   SUBROUTINE deallocate_ehc_prm_c(self)
      CLASS(EHC_PRM), INTENT(INOUT) :: self

      ! CALL deallocate_ehc_prm(self)
      IF (ALLOCATED(self%soil_storecap_roof)) DEALLOCATE (self%soil_storecap_roof)
      IF (ALLOCATED(self%soil_storecap_wall)) DEALLOCATE (self%soil_storecap_wall)
      IF (ALLOCATED(self%state_limit_roof)) DEALLOCATE (self%state_limit_roof)
      IF (ALLOCATED(self%state_limit_wall)) DEALLOCATE (self%state_limit_wall)
      IF (ALLOCATED(self%wet_thresh_roof)) DEALLOCATE (self%wet_thresh_roof)
      IF (ALLOCATED(self%wet_thresh_wall)) DEALLOCATE (self%wet_thresh_wall)
      IF (ALLOCATED(self%tin_roof)) DEALLOCATE (self%tin_roof)
      IF (ALLOCATED(self%tin_wall)) DEALLOCATE (self%tin_wall)
      IF (ALLOCATED(self%tin_surf)) DEALLOCATE (self%tin_surf)
      IF (ALLOCATED(self%k_roof)) DEALLOCATE (self%k_roof)
      IF (ALLOCATED(self%k_wall)) DEALLOCATE (self%k_wall)
      IF (ALLOCATED(self%k_surf)) DEALLOCATE (self%k_surf)
      IF (ALLOCATED(self%cp_roof)) DEALLOCATE (self%cp_roof)
      IF (ALLOCATED(self%cp_wall)) DEALLOCATE (self%cp_wall)
      IF (ALLOCATED(self%cp_surf)) DEALLOCATE (self%cp_surf)
      IF (ALLOCATED(self%dz_roof)) DEALLOCATE (self%dz_roof)
      IF (ALLOCATED(self%dz_wall)) DEALLOCATE (self%dz_wall)
      IF (ALLOCATED(self%dz_surf)) DEALLOCATE (self%dz_surf)

   END SUBROUTINE deallocate_ehc_prm_c

   SUBROUTINE allocate_site_prm_c(self, nlayer)
      CLASS(SUEWS_SITE), INTENT(inout) :: self
      INTEGER, INTENT(in) :: nlayer
      CALL self%DEALLOCATE()
      ALLOCATE (self%sfr_roof(nlayer))
      ALLOCATE (self%sfr_wall(nlayer))

   END SUBROUTINE allocate_site_prm_c

   SUBROUTINE deallocate_site_prm_c(self)
      IMPLICIT NONE

      CLASS(SUEWS_SITE), INTENT(inout) :: self

      IF (ALLOCATED(self%sfr_roof)) DEALLOCATE (self%sfr_roof)
      IF (ALLOCATED(self%sfr_wall)) DEALLOCATE (self%sfr_wall)

   END SUBROUTINE deallocate_site_prm_c

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

   SUBROUTINE SUEWS_cal_surf_DTS( &
      self, & !inout
      config & !input
      ) ! output
      IMPLICIT NONE

      CLASS(SUEWS_SITE), INTENT(INOUT) :: self
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config

      ! TYPE(LC_PAVED_PRM), INTENT(IN) :: pavedPrm
      ! TYPE(LC_BLDG_PRM), INTENT(IN) :: bldgPrm
      ! TYPE(LC_EVETR_PRM), INTENT(IN) :: evetrPrm
      ! TYPE(LC_DECTR_PRM), INTENT(IN) :: dectrPrm
      ! TYPE(LC_GRASS_PRM), INTENT(IN) :: grassPrm
      ! TYPE(LC_BSOIL_PRM), INTENT(IN) :: bsoilPrm
      ! TYPE(LC_WATER_PRM), INTENT(IN) :: waterPrm

      ! TYPE(SPARTACUS_LAYER_PRM), INTENT(IN) :: spartacusLayerPrm
      ! TYPE(SPARTACUS_PRM), INTENT(IN) :: spartacusPrm

      ! INTEGER :: StorageHeatMethod ! method for storage heat calculations [-]
      ! INTEGER :: NetRadiationMethod ! method for net radiation calculations [-]
      ! INTEGER, INTENT(IN) :: nlayer !number of vertical layers[-]

      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]

      ! REAL(KIND(1D0)), DIMENSION(nlayer) :: building_frac !cumulative surface fraction of buildings across vertical layers [-]
      ! REAL(KIND(1D0)), DIMENSION(nlayer) :: building_scale !building scales of each vertical layer  [m]
      ! REAL(KIND(1D0)), DIMENSION(nlayer + 1) :: height !building height of each layer[-]
      ! REAL(KIND(1D0)), INTENT(OUT) :: VegFraction ! fraction of vegetation [-]
      ! REAL(KIND(1D0)), INTENT(OUT) :: ImpervFraction !fractioin of impervious surface [-]
      ! REAL(KIND(1D0)), INTENT(OUT) :: PervFraction !fraction of pervious surfaces [-]
      ! REAL(KIND(1D0)), INTENT(OUT) :: NonWaterFraction !fraction of non-water [-]
      ! REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: sfr_roof !fraction of roof facets [-]
      ! REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: sfr_wall !fraction of wall facets [-]

      ! REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_roof ! individual building fraction at each layer
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: dz_ind ! individual net building height at each layer
      ! REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_wall ! individual net building height at each layer
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: perimeter_ind ! individual building perimeter at each layer
      ASSOCIATE ( &
         StorageHeatMethod => config%StorageHeatMethod, &
         NetRadiationMethod => config%NetRadiationMethod, &
         nlayer => self%nlayer, &
         ! sfr_surf => siteInfo%sfr_surf, &
         pavedPrm => self%lc_paved, &
         bldgPrm => self%lc_bldg, &
         evetrPrm => self%lc_evetr, &
         dectrPrm => self%lc_dectr, &
         grassPrm => self%lc_grass, &
         bsoilPrm => self%lc_bsoil, &
         waterPrm => self%lc_water, &
         VegFraction => self%VegFraction, &
         ImpervFraction => self%ImpervFraction, &
         PervFraction => self%PervFraction, &
         NonWaterFraction => self%NonWaterFraction, &
         sfr_roof => self%sfr_roof, &
         sfr_wall => self%sfr_wall, &
         sfr_surf => self%sfr_surf, &
         ! spartacusPrm => siteInfo%spartacus, &
         height => self%spartacus%height, &
         ! spartacusLayerPrm => siteInfo%spartacusLayerPrm, &
         building_frac => self%spartacus_layer%building_frac, &
         building_scale => self%spartacus_layer%building_scale &
         &)
         ! StorageHeatMethod = config%StorageHeatMethod
         ! NetRadiationMethod = config%NetRadiationMethod
         ! ALLOCATE (sfr_roof(nlayer))
         ! ALLOCATE (sfr_wall(nlayer))
         ALLOCATE (dz_ind(nlayer))
         ALLOCATE (perimeter_ind(nlayer))

         sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]

         ! building_frac = spartacusLayerPrm%building_frac
         ! building_scale = spartacusLayerPrm%building_scale
         ! height = spartacusPrm%height

         VegFraction = sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf)
         ImpervFraction = sfr_surf(PavSurf) + sfr_surf(BldgSurf)
         PervFraction = 1 - ImpervFraction
         NonWaterFraction = 1 - sfr_surf(WaterSurf)

         IF (StorageHeatMethod == 5 .OR. NetRadiationMethod > 1000) THEN
            ! get individual building fractions of each layer
            ! NB.: sum(sfr_roof) = building_frac(1)
            sfr_roof = 0.
            IF (nlayer > 1) sfr_roof(1:nlayer - 1) = building_frac(1:nlayer - 1) - building_frac(2:nlayer)
            sfr_roof(nlayer) = building_frac(nlayer)

            ! get individual net building height of each layer
            dz_ind = 0.
            dz_ind(1:nlayer) = height(2:nlayer + 1) - height(1:nlayer)

            ! get individual building perimeter of each layer
            ! this is from eq. 8 in SS documentation:
            ! https://github.com/ecmwf/spartacus-surface/blob/master/doc/spartacus_surface_documentation.pdf
            perimeter_ind = 0.
            perimeter_ind(1:nlayer) = 4.*building_frac(1:nlayer)/building_scale(1:nlayer)

            ! sfr_wall stands for individual wall area
            ! get individual wall area at each layer
            sfr_wall = 0.
            ! this is from eq. 1 in SS documentation:
            ! https://github.com/ecmwf/spartacus-surface/blob/master/doc/spartacus_surface_documentation.pdf
            sfr_wall(1:nlayer) = perimeter_ind(1:nlayer)*dz_ind(1:nlayer)
         END IF

      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_surf_DTS

   SUBROUTINE check_and_reset_unsafe_states(self, ref_state)
      CLASS(SUEWS_STATE), INTENT(inout) :: self
      TYPE(SUEWS_STATE), INTENT(in) :: ref_state

      ! Direct checks for each component
      IF (.NOT. self%flagState%iter_safe) THEN
         self%flagState = ref_state%flagState
      END IF

      IF (.NOT. self%anthroemisState%iter_safe) THEN
         self%anthroemisState = ref_state%anthroemisState
      END IF

      IF (.NOT. self%ohmState%iter_safe) THEN
         self%ohmState = ref_state%ohmState
      END IF

      IF (.NOT. self%solarState%iter_safe) THEN
         self%solarState = ref_state%solarState
      END IF

      IF (.NOT. self%atmState%iter_safe) THEN
         self%atmState = ref_state%atmState
      END IF

      IF (.NOT. self%phenState%iter_safe) THEN
         self%phenState = ref_state%phenState
      END IF

      IF (.NOT. self%snowState%iter_safe) THEN
         self%snowState = ref_state%snowState
      END IF

      IF (.NOT. self%hydroState%iter_safe) THEN
         self%hydroState = ref_state%hydroState
      END IF

      IF (.NOT. self%heatState%iter_safe) THEN
         self%heatState = ref_state%heatState
      END IF

      IF (.NOT. self%roughnessState%iter_safe) THEN
         self%roughnessState = ref_state%roughnessState
      END IF

      IF (.NOT. self%stebbsState%iter_safe) THEN
         self%stebbsState = ref_state%stebbsState
      END IF

      IF (.NOT. self%nhoodState%iter_safe) THEN
         self%nhoodState = ref_state%nhoodState
      END IF
   END SUBROUTINE check_and_reset_unsafe_states

END MODULE SUEWS_DEF_DTS
