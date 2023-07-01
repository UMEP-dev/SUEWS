!========================================================================================
! SUEWS driver subroutines
! TS 31 Aug 2017: initial version
! TS 02 Oct 2017: added  as the generic wrapper
! TS 03 Oct 2017: added
MODULE SUEWS_Driver
   ! only the following immutable objects are imported:
   ! 1. functions/subroutines
   ! 2. constant variables

   USE meteo, ONLY: qsatf, RH2qa, qa2RH
   USE AtmMoistStab_module, ONLY: cal_AtmMoist, cal_Stab, stab_psi_heat, stab_psi_mom
   USE NARP_MODULE, ONLY: NARP_cal_SunPosition
   USE SPARTACUS_MODULE, ONLY: SPARTACUS
   USE AnOHM_module, ONLY: AnOHM
   USE resist_module, ONLY: AerodynamicResistance, BoundaryLayerResistance, SurfaceResistance, &
                            SUEWS_cal_RoughnessParameters, SUEWS_cal_RoughnessParameters_DTS
   USE ESTM_module, ONLY: ESTM
   USE EHC_module, ONLY: EHC
   USE Snow_module, ONLY: SnowCalc, MeltHeat, SnowUpdate, update_snow_albedo, update_snow_dens
   USE DailyState_module, ONLY: SUEWS_cal_DailyState, update_DailyStateLine, SUEWS_cal_DailyState_DTS
   USE WaterDist_module, ONLY: &
      drainage, cal_water_storage_surf, &
      cal_water_storage_building, &
      SUEWS_cal_SoilState, SUEWS_cal_SoilState_DTS, &
      SUEWS_update_SoilMoist, SUEWS_update_SoilMoist_DTS, &
      ReDistributeWater, SUEWS_cal_HorizontalSoilWater, &
      SUEWS_cal_HorizontalSoilWater_DTS, &
      SUEWS_cal_WaterUse, SUEWS_cal_WaterUse_DTS
   USE ctrl_output, ONLY: varListAll
   USE DailyState_module, ONLY: SUEWS_update_DailyState
   USE lumps_module, ONLY: LUMPS_cal_QHQE, LUMPS_cal_QHQE_DTS
   USE evap_module, ONLY: cal_evap_multi
   USE rsl_module, ONLY: RSLProfile, RSLProfile_DTS
   USE anemsn_module, ONLY: AnthropogenicEmissions
   USE CO2_module, ONLY: CO2_biogen
   USE allocateArray, ONLY: &
      nsurf, nvegsurf, ndepth, nspec, &
      PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf, &
      ivConif, ivDecid, ivGrass, &
      ncolumnsDataOutSUEWS, ncolumnsDataOutSnow, &
      ncolumnsDataOutESTM, ncolumnsDataOutDailyState, &
      ncolumnsDataOutRSL, ncolumnsdataOutSOLWEIG, ncolumnsDataOutBEERS, &
      ncolumnsDataOutDebug, ncolumnsDataOutSPARTACUS, ncolumnsDataOutESTMExt
   USE moist, ONLY: avcp, avdens, lv_J_kg
   USE solweig_module, ONLY: SOLWEIG_cal_main
   USE beers_module, ONLY: BEERS_cal_main
   USE version, ONLY: git_commit, compiler_ver

   IMPLICIT NONE

   TYPE, PUBLIC :: config
      INTEGER :: var1
      INTEGER :: var2

      ! private

      ! contains

   END TYPE config

   TYPE, PUBLIC :: array_m
      INTEGER, DIMENSION(2, 3) :: var1
      INTEGER, DIMENSION(3, 2) :: var2

      ! private

      ! contains

   END TYPE array_m

   TYPE, PUBLIC :: output_block
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: SUEWS
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Snow
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: ESTM
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: ESTMExt
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: RSL
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: BEERS
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Debug
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: SPARTACUS
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: DailyState
      ! contains
      !    ! Procedures
      !    PROCEDURE :: init => output_block_init
      !    PROCEDURE :: finalize => output_block_finalize
   END TYPE output_block

   TYPE, PUBLIC :: output_line
      REAL(KIND(1D0)), DIMENSION(5) :: datetime !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS) :: SUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow) :: Snow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM) :: ESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt) :: ESTMExt
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL) :: RSL
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS) :: BEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug) :: Debug
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS) :: SPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState) :: DailyState
   END TYPE output_line

   ! ********** SUEWS_parameters schema (basic) **********
   TYPE, PUBLIC :: METHOD_PRM
      INTEGER :: DiagMethod ! Defines how near surface diagnostics are calculated
      INTEGER :: EmissionsMethod ! method to calculate anthropogenic heat [-]
      INTEGER :: RoughLenHeatMethod ! method to calculate heat roughness length [-]
      INTEGER :: RoughLenMomMethod ! Determines how aerodynamic roughness length (z0m) and zero displacement height (zdm) are calculated [-]
      INTEGER :: FAIMethod !Determines how FAI is calculated [-]
      INTEGER :: SMDMethod ! Determines method for calculating soil moisture deficit [-]
      INTEGER :: WaterUseMethod ! Defines how external water use is calculated[-]
      INTEGER :: NetRadiationMethod ! method for calculation of radiation fluxes [-]
      INTEGER :: StabilityMethod ! method to calculate atmospheric stability [-]
      INTEGER :: StorageHeatMethod ! !Determines method for calculating storage heat flux ΔQS [-]
      INTEGER :: Diagnose ! flag for printing diagnostic info during runtime [N/A]C
      INTEGER :: SnowUse !
      LOGICAL :: use_sw_direct_albedo !boolean, Specify ground and roof albedos separately for direct solar radiation [-]
      INTEGER :: ohmIncQF ! Determines whether the storage heat flux calculation uses Q* or ( Q* +QF) [-]
   END TYPE METHOD_PRM

   TYPE, PUBLIC :: SURF_STORE_PRM
      REAL(KIND(1D0)) :: store_min
      REAL(KIND(1D0)) :: store_max
      REAL(KIND(1D0)) :: store_cap ! this should be abundant ? (current storage capacity) [mm]
      INTEGER :: drain_eq
      REAL(KIND(1D0)) :: drain_coef_1
      REAL(KIND(1D0)) :: drain_coef_2
   END TYPE SURF_STORE_PRM

   TYPE, PUBLIC :: WATER_DIST_PRM
      REAL(KIND(1D0)) :: to_paved
      REAL(KIND(1D0)) :: to_bldg
      REAL(KIND(1D0)) :: to_evetr
      REAL(KIND(1D0)) :: to_dectr
      REAL(KIND(1D0)) :: to_grass
      REAL(KIND(1D0)) :: to_bsoil
      REAL(KIND(1D0)) :: to_water
      REAL(KIND(1D0)) :: to_soilstore
   END TYPE WATER_DIST_PRM

   TYPE, PUBLIC :: bioCO2_PRM
      REAL(KIND(1D0)) :: beta_bioco2 ! The light-saturated gross photosynthesis of the canopy [umol m-2 s-1 ]
      REAL(KIND(1D0)) :: beta_enh_bioco2 ! Part of the beta coefficient related to the fraction of vegetation [umol m-2 s-1 ]
      REAL(KIND(1D0)) :: alpha_bioco2 ! The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve [-]
      REAL(KIND(1D0)) :: alpha_enh_bioco2 ! Part of the alpha coefficient related to the fraction of vegetation [-]
      REAL(KIND(1D0)) :: resp_a !Respiration coefficient a
      REAL(KIND(1D0)) :: resp_b !Respiration coefficient b - related to air temperature dependency
      REAL(KIND(1D0)) :: theta_bioco2 ! The convexity of the curve at light saturation [-]
      REAL(KIND(1D0)) :: min_res_bioCO2 ! Minimum soil respiration rate (for cold-temperature limit) [umol m-2 s-1]
   END TYPE bioCO2_PRM

   TYPE, PUBLIC :: CONDUCTANCE_PRM
      REAL(KIND(1D0)) :: g_max !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)) :: g_k
      REAL(KIND(1D0)) :: g_q_base
      REAL(KIND(1D0)) :: g_q_shape
      REAL(KIND(1D0)) :: g_t
      REAL(KIND(1D0)) :: g_sm ! Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)) :: kmax ! annual maximum hourly solar radiation [W m-2]
      INTEGER :: gsmodel ! choice of gs parameterisation (1 = Ja11, 2 = Wa16) [-]
      REAL(KIND(1D0)) :: s1 ! a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)) :: s2 ! a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)) :: TH ! upper air temperature limit [degC]
      REAL(KIND(1D0)) :: TL ! lower air temperature limit [degC]
   END TYPE CONDUCTANCE_PRM

   TYPE, PUBLIC :: LAI_PRM ! do we need `lai_id` here?
      REAL(KIND(1D0)) :: baset !Base Temperature for initiating growing degree days (GDD) for leaf growth [degC]
      REAL(KIND(1D0)) :: gddfull !the growing degree days (GDD) needed for full capacity of the leaf area index [degC]
      REAL(KIND(1D0)) :: basete !Base temperature for initiating sensesance degree days (SDD) for leaf off [degC]
      REAL(KIND(1D0)) :: sddfull !the sensesence degree days (SDD) needed to initiate leaf off [degC]
      REAL(KIND(1D0)) :: laimin !leaf-off wintertime value [m2 m-2]
      REAL(KIND(1D0)) :: laimax !full leaf-on summertime value [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(4) :: laipower ! parameters required by LAI calculation.
      INTEGER :: laitype ! LAI calculation choice.
   END TYPE LAI_PRM

   TYPE, PUBLIC :: OHM_COEF_LC
      REAL(KIND(1D0)) :: summer_dry
      REAL(KIND(1D0)) :: summer_wet
      REAL(KIND(1D0)) :: winter_dry
      REAL(KIND(1D0)) :: winter_wet
   END TYPE OHM_COEF_LC

   TYPE, PUBLIC :: OHM_PRM
      REAL(KIND(1D0)) :: chanohm ! Bulk transfer coefficient for this surface to use in AnOHM [J m-3 K-1]
      REAL(KIND(1D0)) :: cpanohm ! Volumetric heat capacity for this surface to use in AnOHM  [J m-3 K-1]
      REAL(KIND(1D0)) :: kkanohm ! Thermal conductivity for this surface to use in AnOHM [W m-1 K-1]
      REAL(KIND(1D0)) :: ohm_threshsw ! Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)) :: ohm_threshwd ! Soil moisture threshold determining whether wet/dry OHM coefficients are applied [degC]
      TYPE(OHM_COEF_LC), DIMENSION(3) :: ohm_coef_lc
   END TYPE OHM_PRM

   TYPE, PUBLIC :: SOIL_PRM
      REAL(KIND(1D0)) :: soildepth
      REAL(KIND(1D0)) :: soilstorecap
      REAL(KIND(1D0)) :: sathydraulicconduct ! !Hydraulic conductivity for saturated soil [mm s-1]
   END TYPE SOIL_PRM

   TYPE, PUBLIC :: anthroHEAT_PRM
      REAL(KIND(1D0)) :: qf0_beu_working ! Fraction of base value coming from buildings (working day) [-]
      REAL(KIND(1D0)) :: qf0_beu_holiday ! Fraction of base value coming from buildings (holiday) [-]
      REAL(KIND(1D0)) :: qf_a_working ! Base value for QF (working day) [W m-2]
      REAL(KIND(1D0)) :: qf_a_holiday ! Base value for QF (holiday) [W m-2]
      REAL(KIND(1D0)) :: qf_b_working ! Parameter related to heating degree days (working day) [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)) :: qf_b_holiday ! Parameter related to heating degree days (holiday) [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)) :: qf_c_working ! Parameter related to cooling degree days (working day) [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)) :: qf_c_holiday ! Parameter related to cooling degree days (holiday) [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)) :: baset_cooling_working ! Base temperature for cooling degree days (working day) [degC]
      REAL(KIND(1D0)) :: baset_cooling_holiday ! Base temperature for cooling degree days (holiday) [degC]
      REAL(KIND(1D0)) :: baset_heating_working ! Base temperature for heating degree days (working day) [degC]
      REAL(KIND(1D0)) :: baset_heating_holiday ! Base temperature for heating degree days (holiday) [degC]
      REAL(KIND(1D0)) :: ah_min_working ! minimum QF values (working day) [W m-2]
      REAL(KIND(1D0)) :: ah_min_holiday ! minimum QF values (holiday) [W m-2]
      REAL(KIND(1D0)) :: ah_slope_cooling_working ! cooling slope for the anthropogenic heat flux calculation (working day) [W m-2 K-1]
      REAL(KIND(1D0)) :: ah_slope_cooling_holiday ! cooling slope for the anthropogenic heat flux calculation (holiday) [W m-2 K-1]
      REAL(KIND(1D0)) :: ah_slope_heating_working ! heating slope for the anthropogenic heat flux calculation (working day) [W m-2 K-1]
      REAL(KIND(1D0)) :: ah_slope_heating_holiday ! heating slope for the anthropogenic heat flux calculation (holiday) [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(0:23) :: ahprof_24hr_working ! Hourly profile values used in energy use calculation (working day) [-]
      REAL(KIND(1D0)), DIMENSION(0:23) :: ahprof_24hr_holiday ! Hourly profile values used in energy use calculation (holiday) [-]
      REAL(KIND(1D0)) :: popdensdaytime_working ! Daytime population density [people ha-1] (working day)
      REAL(KIND(1D0)) :: popdensdaytime_holiday ! Daytime population density [people ha-1] (holiday)
      REAL(KIND(1D0)) :: popdensnighttime
      REAL(KIND(1D0)), DIMENSION(0:23) :: popprof_24hr_working !Hourly profile values used in dynamic population estimation[-] (working day)
      REAL(KIND(1D0)), DIMENSION(0:23) :: popprof_24hr_holiday !Hourly profile values used in dynamic population estimation[-] (holiday)
   END TYPE anthroHEAT_PRM

   TYPE, PUBLIC :: IRRIG_daywater
      REAL(KIND(1D0)) :: monday_flag ! Irrigation flag: 1 for on and 0 for off.
      REAL(KIND(1D0)) :: monday_percent ! Fraction of properties using irrigation for each day of a week.
      REAL(KIND(1D0)) :: tuesday_flag
      REAL(KIND(1D0)) :: tuesday_percent
      REAL(KIND(1D0)) :: wednesday_flag
      REAL(KIND(1D0)) :: wednesday_percent
      REAL(KIND(1D0)) :: thursday_flag
      REAL(KIND(1D0)) :: thursday_percent
      REAL(KIND(1D0)) :: friday_flag
      REAL(KIND(1D0)) :: friday_percent
      REAL(KIND(1D0)) :: saturday_flag
      REAL(KIND(1D0)) :: saturday_percent
      REAL(KIND(1D0)) :: sunday_flag
      REAL(KIND(1D0)) :: sunday_percent
   END TYPE IRRIG_daywater

   TYPE, PUBLIC :: IRRIGATION_PRM ! used in irrigation
      REAL(KIND(1D0)) :: h_maintain ! ponding water depth to maintain [mm]
      REAL(KIND(1D0)) :: faut ! Fraction of irrigated area using automatic irrigation [-]
      REAL(KIND(1D0)), DIMENSION(3) :: ie_a ! Coefficient for automatic irrigation model
      REAL(KIND(1D0)), DIMENSION(3) :: ie_m ! Coefficient for automatic irrigation model
      INTEGER :: ie_start ! ending time of water use [DOY]
      INTEGER :: ie_end ! starting time of water use [DOY]
      REAL(KIND(1D0)) :: internalwateruse_h ! Internal water use [mm h-1]
      TYPE(IRRIG_daywater) :: irr_daywater
      REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofa_24hr_working ! Hourly profile values used in automatic irrigation[-]
      REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofa_24hr_holiday
      REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofm_24hr_working ! Hourly profile values used in manual irrigation[-]
      REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofm_24hr_holiday
   END TYPE IRRIGATION_PRM

   TYPE, PUBLIC :: anthroEMIS_PRM
      INTEGER :: startdls ! start of daylight saving  [DOY]
      INTEGER :: enddls ! end of daylight saving [DOY]
      TYPE(anthroHEAT_PRM) :: anthroheat
      REAL(KIND(1D0)) :: EF_umolCO2perJ
      REAL(KIND(1D0)) :: EnEF_v_Jkm
      REAL(KIND(1D0)) :: FrFossilFuel_Heat
      REAL(KIND(1D0)) :: FrFossilFuel_NonHeat
      REAL(KIND(1D0)), DIMENSION(2) :: FcEF_v_kgkm
      REAL(KIND(1D0)), DIMENSION(0:23) :: HumActivity_24hr_working
      REAL(KIND(1D0)), DIMENSION(0:23) :: HumActivity_24hr_holiday
      REAL(KIND(1D0)) :: MaxFCMetab
      REAL(KIND(1D0)) :: MaxQFMetab
      REAL(KIND(1D0)) :: MinFCMetab
      REAL(KIND(1D0)) :: MinQFMetab
      REAL(KIND(1D0)) :: TrafficRate_working
      REAL(KIND(1D0)) :: TrafficRate_holiday
      REAL(KIND(1D0)) :: TrafficUnits
      REAL(KIND(1D0)), DIMENSION(0:23) :: TraffProf_24hr_working
      REAL(KIND(1D0)), DIMENSION(0:23) :: TraffProf_24hr_holiday
   END TYPE anthroEMIS_PRM

   TYPE, PUBLIC :: SNOW_PRM
      REAL(KIND(1D0)) :: crwmax ! maximum water holding capacity of snow [mm]
      REAL(KIND(1D0)) :: crwmin ! minimum water holding capacity of snow [mm]
      REAL(KIND(1D0)) :: narp_emis_snow ! snow emissivity in NARP model [-]
      REAL(KIND(1D0)) :: preciplimit ! temperature limit when precipitation falls as snow [degC]
      REAL(KIND(1D0)) :: preciplimitalb ! Limit for hourly precipitation when the ground is fully covered with snow [mm]
      REAL(KIND(1D0)) :: snowalbmax ! effective surface albedo (middle of the day value) for summertime [-]
      REAL(KIND(1D0)) :: snowalbmin ! effective surface albedo (middle of the day value) for wintertime (not including snow) [-]
      REAL(KIND(1D0)) :: snowdensmax ! maximum snow density [kg m-3]
      REAL(KIND(1D0)) :: snowdensmin ! fresh snow density [kg m-3]
      REAL(KIND(1D0)) :: snowlimbldg ! Limit of the snow water equivalent for snow removal from building roofs [mm]
      REAL(KIND(1D0)) :: snowlimpaved ! limit of the snow water equivalent for snow removal from roads[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowpacklimit ! Limit for the snow water equivalent when snow cover starts to be patchy [mm]
      REAL(KIND(1D0)), DIMENSION(0:23) :: snowprof_24hr_working ! Hourly profile values used in snow clearing of working day [-]
      REAL(KIND(1D0)), DIMENSION(0:23) :: snowprof_24hr_holiday ! Hourly profile values used in snow clearing of holiday [-]
      REAL(KIND(1D0)) :: tau_a ! time constant for snow albedo aging in cold snow [-]
      REAL(KIND(1D0)) :: tau_f ! time constant for snow albedo aging in melting snow [-]
      REAL(KIND(1D0)) :: tau_r ! time constant for snow density ageing [-]
      REAL(KIND(1D0)) :: tempmeltfact ! hourly temperature melt factor of snow [mm K-1 h-1]
      REAL(KIND(1D0)) :: radmeltfact ! hourly radiation melt factor of snow [mm W-1 h-1]
   END TYPE SNOW_PRM

   TYPE, PUBLIC :: SPARTACUS_PRM
      REAL(KIND(1D0)) :: air_ext_lw
      REAL(KIND(1D0)) :: air_ext_sw
      REAL(KIND(1D0)) :: air_ssa_lw
      REAL(KIND(1D0)) :: air_ssa_sw
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: height
      REAL(KIND(1D0)) :: ground_albedo_dir_mult_fact
      INTEGER :: n_stream_lw_urban ! LW streams per hemisphere [-]
      INTEGER :: n_stream_sw_urban ! shortwave diffuse streams per hemisphere [-]
      INTEGER :: n_vegetation_region_urban ! Number of regions used to describe vegetation [-]
      REAL(KIND(1D0)) :: sw_dn_direct_frac
      REAL(KIND(1D0)) :: use_sw_direct_albedo
      REAL(KIND(1D0)) :: veg_contact_fraction_const
      REAL(KIND(1D0)) :: veg_fsd_const
      REAL(KIND(1D0)) :: veg_ssa_lw
      REAL(KIND(1D0)) :: veg_ssa_sw
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
   END TYPE SPARTACUS_LAYER_PRM

   ! ********** SUEWS_parameters schema (derived) **********
   TYPE, PUBLIC :: SITE_PRM
      REAL(KIND(1D0)) :: lat !latitude [deg]
      REAL(KIND(1D0)) :: lon !longitude [deg]
      REAL(KIND(1D0)) :: alt ! solar altitude [deg]
      INTEGER :: gridiv ! grid id [-]
      REAL(KIND(1D0)) :: timezone ! time zone, for site relative to UTC (east is positive) [h]
      REAL(KIND(1D0)) :: surfacearea ! area of the grid [ha]
      REAL(KIND(1D0)) :: z ! measurement height [m]
      REAL(KIND(1D0)) :: z0m_in ! roughness length for momentum [m]
      REAL(KIND(1D0)) :: zdm_in ! zero-plane displacement [m]
      REAL(KIND(1D0)) :: pipecapacity ! capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)) :: runofftowater ! fraction of above-ground runoff flowing to water surface during flooding [-]
      REAL(KIND(1D0)) :: narp_trans_site ! atmospheric transmissivity for NARP [-]
      REAL(KIND(1D0)) :: CO2PointSource ! CO2 emission factor [kg km-1]
      REAL(KIND(1D0)) :: flowchange ! Difference in input and output flows for water surface
   END TYPE SITE_PRM

   TYPE, PUBLIC :: LUMPS_PRM
      REAL(KIND(1D0)) :: raincover ! limit when surface totally covered with water for LUMPS [mm]
      REAL(KIND(1D0)) :: rainmaxres ! maximum water bucket reservoir. Used for LUMPS surface wetness control. [mm]
      REAL(KIND(1D0)) :: drainrt ! Drainage rate of the water bucket [mm hr-1]
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
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_roof ! Heat capacity of roof [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_wall ! Heat capacity of wall [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_surf ! Heat capacity of each surface [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_roof ! thickness of each layer in roof [m]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_wall ! thickness of each layer in wall [m]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_surf ! thickness of each layer in surface [m]
   END TYPE EHC_PRM

   TYPE, PUBLIC :: LC_PAVED_PRM
      REAL(KIND(1D0)) :: sfr
      REAL(KIND(1D0)) :: emis
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: state
      REAL(KIND(1D0)) :: statelimit
      REAL(KIND(1D0)) :: irrfracpaved
      REAL(KIND(1D0)) :: wetthresh
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_PAVED_PRM

   TYPE, PUBLIC :: LC_BLDG_PRM
      REAL(KIND(1D0)) :: sfr
      REAL(KIND(1D0)) :: faibldg
      REAL(KIND(1D0)) :: bldgh
      REAL(KIND(1D0)) :: emis
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: state
      REAL(KIND(1D0)) :: statelimit
      REAL(KIND(1D0)) :: irrfracbldgs
      REAL(KIND(1D0)) :: wetthresh
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_BLDG_PRM

   TYPE, PUBLIC :: LC_DECTR_PRM
      REAL(KIND(1D0)) :: sfr
      REAL(KIND(1D0)) :: emis
      REAL(KIND(1D0)) :: faidectree
      REAL(KIND(1D0)) :: dectreeh
      REAL(KIND(1D0)) :: pormin_dec ! absent for evergreen trees ??
      REAL(KIND(1D0)) :: pormax_dec
      REAL(KIND(1D0)) :: alb_min
      REAL(KIND(1D0)) :: alb_max
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit ! ******* dummy variable *******
      REAL(KIND(1D0)) :: capmax_dec ! Maximum water storage capacity for upper surfaces (i.e. canopy) (absent for evergreen trees ??)
      REAL(KIND(1D0)) :: capmin_dec ! Minimum water storage capacity for upper surfaces (i.e. canopy).
      REAL(KIND(1D0)) :: irrfracdectr
      REAL(KIND(1D0)) :: wetthresh
      TYPE(bioCO2_PRM) :: bioco2
      REAL(KIND(1D0)) :: maxconductance ! the maximum conductance of each vegetation or surface type. [mm s-1]
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_DECTR_PRM

   TYPE, PUBLIC :: LC_EVETR_PRM
      REAL(KIND(1D0)) :: sfr !surface cover fraction[-]
      REAL(KIND(1D0)) :: emis !Effective surface emissivity[-]
      REAL(KIND(1D0)) :: faievetree !frontal area index for evergreen tree [-]
      REAL(KIND(1D0)) :: evetreeh !height of evergreen tree [m]
      REAL(KIND(1D0)) :: alb_min !minimum albedo for evergreen tree [-]
      REAL(KIND(1D0)) :: alb_max !maximum albedo for evergreen tree [-]
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit ! upper limit to the surface state [mm]
      REAL(KIND(1D0)) :: irrfracevetr
      REAL(KIND(1D0)) :: wetthresh !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]
      TYPE(bioCO2_PRM) :: bioco2
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      REAL(KIND(1D0)) :: maxconductance ! the maximum conductance of each vegetation or surface type. [mm s-1]
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist !Fraction of water redistribution [-]
   END TYPE LC_EVETR_PRM

   TYPE, PUBLIC :: LC_GRASS_PRM
      REAL(KIND(1D0)) :: sfr
      REAL(KIND(1D0)) :: emis
      REAL(KIND(1D0)) :: alb_min
      REAL(KIND(1D0)) :: alb_max
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit
      REAL(KIND(1D0)) :: irrfracgrass
      REAL(KIND(1D0)) :: wetthresh
      TYPE(bioCO2_PRM) :: bioco2
      ! TYPE(CONDUCTANCE_PRM) :: conductance
      REAL(KIND(1D0)) :: maxconductance ! the maximum conductance of each vegetation or surface type. [mm s-1]
      TYPE(LAI_PRM) :: lai
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_GRASS_PRM

   TYPE, PUBLIC :: LC_BSOIL_PRM
      REAL(KIND(1D0)) :: sfr
      REAL(KIND(1D0)) :: emis
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit
      REAL(KIND(1D0)) :: irrfracbsoil
      REAL(KIND(1D0)) :: wetthresh
      TYPE(WATER_DIST_PRM) :: waterdist
   END TYPE LC_BSOIL_PRM

   TYPE, PUBLIC :: LC_WATER_PRM
      REAL(KIND(1D0)) :: sfr
      REAL(KIND(1D0)) :: emis
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit
      REAL(KIND(1D0)) :: irrfracwater
      REAL(KIND(1D0)) :: wetthresh ! ******* dummy variable *******
      REAL(KIND(1D0)) :: flowchange ! special term in water
   END TYPE LC_WATER_PRM

   ! ********** SUEWS_stateVars schema **********
   TYPE, PUBLIC :: anthroHEAT_STATE
      REAL(KIND(1D0)), DIMENSION(12) :: HDD_id !Heating Degree Days [degC d]
   END TYPE anthroHEAT_STATE

   TYPE, PUBLIC :: HYDRO_STATE
      ! REAL(KIND(1D0)) :: runofftowater   ! Fraction of above-ground runoff flowing to water surface during flooding
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_surf ! Initial water stored in soil beneath `Bldgs` surface
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_surf ! Initial wetness condition on SUEWS land covers.
      REAL(KIND(1D0)), DIMENSION(9) :: WUDay_id ! Daily water use for EveTr, DecTr, Grass [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soilstore_roof ! Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_roof ! wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soilstore_wall ! Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_wall ! wetness status of wall [mm]
   END TYPE HYDRO_STATE

   TYPE, PUBLIC :: HEAT_STATE
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_roof ! interface temperature between depth layers in roof [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_wall ! interface temperature between depth layers in wall [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_surf ! interface temperature between depth layers [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof ! roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_wall ! wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf ! surface temperature [degC]
   END TYPE HEAT_STATE

   TYPE, PUBLIC :: OHM_STATE
      REAL(KIND(1D0)) :: qn_av ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: dqndt ! rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)) :: qn_s_av ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)) :: dqnsdt ! Rate of change of net radiation [W m-2 h-1]
   END TYPE OHM_STATE

   TYPE, PUBLIC :: PHENOLOGY_STATE
      REAL(KIND(1D0)), DIMENSION(NSURF) :: alb
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: lai_id ! Initial LAI values.
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDD_id ! Growing Degree Days [degC](see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDD_id ! Senescence Degree Days [degC](see SUEWS_DailyState.f95)
      REAL(KIND(1D0)) :: porosity_id !
      REAL(KIND(1D0)) :: decidcap_id ! Storage capacity of deciduous surface `DecTr`; updated each day in simulaiton due to changes in LAI.
      REAL(KIND(1D0)) :: albDecTr_id !
      REAL(KIND(1D0)) :: albEveTr_id !
      REAL(KIND(1D0)) :: albGrass_id !
      REAL(KIND(1D0)) :: Tmin_id ! Daily minimum temperature [degC]
      REAL(KIND(1D0)) :: Tmax_id ! Daily maximum temperature [degC]
      REAL(KIND(1D0)) :: lenDay_id ! daytime length [h]
      REAL(KIND(1D0)), DIMENSION(6, NSURF) :: StoreDrainPrm ! coefficients used in drainage calculation [-]
   END TYPE PHENOLOGY_STATE

   TYPE, PUBLIC :: SNOW_STATE
      REAL(KIND(1D0)) :: snowfallCum !
      REAL(KIND(1D0)) :: snowalb ! albedo of snow [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: icefrac ! fraction of ice in snowpack [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowdens ! snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowfrac ! snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowpack ! snow water equivalent on each land cover [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowwater ! snow water[mm]
   END TYPE SNOW_STATE

   ! ********** SUEWS_forcing schema **********
   TYPE, PUBLIC :: SUEWS_FORCING
      REAL(KIND(1D0)) :: kdown !
      REAL(KIND(1D0)) :: ldown !
      REAL(KIND(1D0)) :: RH !
      REAL(KIND(1D0)) :: pres !
      REAL(KIND(1D0)) :: Tair !
      REAL(KIND(1D0)) :: U !
      REAL(KIND(1D0)) :: rain !
      REAL(KIND(1D0)) :: Wuh !  external water use
      REAL(KIND(1D0)) :: fcld !
      REAL(KIND(1D0)) :: LAI_obs !
      REAL(KIND(1D0)) :: snowfrac !
      REAL(KIND(1D0)) :: xsmd !
      REAL(KIND(1D0)) :: qf_obs !
      REAL(KIND(1D0)) :: qn1_obs !
      REAL(KIND(1D0)) :: qs_obs !
      REAL(KIND(1D0)) :: temp_c !
   END TYPE SUEWS_FORCING

   TYPE, PUBLIC :: SUEWS_TIMER
      INTEGER :: id !
      INTEGER :: imin !
      INTEGER :: isec !
      INTEGER :: it !
      INTEGER :: iy !
      INTEGER :: tstep !
      INTEGER :: tstep_prev !
      INTEGER :: dt_since_start !
   END TYPE SUEWS_TIMER

CONTAINS

   SUBROUTINE output_line_init(this_line)
      TYPE(output_line), INTENT(inout) :: this_line

      ! Set default values
      this_line%datetime = -999.0
      this_line%SUEWS = -999.0
      this_line%Snow = -999.0
      this_line%ESTM = -999.0
      this_line%ESTMExt = -999.0
      this_line%RSL = -999.0
      this_line%BEERS = -999.0
      this_line%Debug = -999.0
      this_line%SPARTACUS = -999.0
      this_line%DailyState = -999.0
   END SUBROUTINE output_line_init

   SUBROUTINE output_block_init(this_block, len)
      TYPE(output_block), INTENT(inout) :: this_block
      INTEGER, INTENT(in) :: len

      ! Allocate memory for the arrays
      ALLOCATE (this_block%SUEWS(len, ncolumnsDataOutSUEWS))
      ALLOCATE (this_block%Snow(len, ncolumnsDataOutSnow))
      ALLOCATE (this_block%ESTM(len, ncolumnsDataOutESTM))
      ALLOCATE (this_block%ESTMExt(len, ncolumnsDataOutESTMExt))
      ALLOCATE (this_block%RSL(len, ncolumnsDataOutRSL))
      ALLOCATE (this_block%BEERS(len, ncolumnsDataOutBEERS))
      ALLOCATE (this_block%Debug(len, ncolumnsDataOutDebug))
      ALLOCATE (this_block%SPARTACUS(len, ncolumnsDataOutSPARTACUS))
      ALLOCATE (this_block%DailyState(len, ncolumnsDataOutDailyState))

      ! Set default values
      this_block%SUEWS = -999.0
      this_block%Snow = -999.0
      this_block%ESTM = -999.0
      this_block%ESTMExt = -999.0
      this_block%RSL = -999.0
      this_block%BEERS = -999.0
      this_block%Debug = -999.0
      this_block%SPARTACUS = -999.0
      this_block%DailyState = -999.0
   END SUBROUTINE output_block_init

   SUBROUTINE output_block_finalize(this_line)
      TYPE(output_block), INTENT(inout) :: this_line

      ! Deallocate memory for the arrays
      IF (ALLOCATED(this_line%SUEWS)) DEALLOCATE (this_line%SUEWS)
      IF (ALLOCATED(this_line%Snow)) DEALLOCATE (this_line%Snow)
      IF (ALLOCATED(this_line%ESTM)) DEALLOCATE (this_line%ESTM)
      IF (ALLOCATED(this_line%ESTMExt)) DEALLOCATE (this_line%ESTMExt)
      IF (ALLOCATED(this_line%RSL)) DEALLOCATE (this_line%RSL)
      IF (ALLOCATED(this_line%BEERS)) DEALLOCATE (this_line%BEERS)
      IF (ALLOCATED(this_line%Debug)) DEALLOCATE (this_line%Debug)
      IF (ALLOCATED(this_line%SPARTACUS)) DEALLOCATE (this_line%SPARTACUS)
      IF (ALLOCATED(this_line%DailyState)) DEALLOCATE (this_line%DailyState)

   END SUBROUTINE output_block_finalize

   SUBROUTINE var2add_two(arg_type, res_type)
      TYPE(config), INTENT(in) :: arg_type
      TYPE(config), INTENT(out) :: res_type
      res_type%var1 = arg_type%var1 + arg_type%var2
      res_type%var2 = arg_type%var1 - arg_type%var2

   END SUBROUTINE var2add_two

   SUBROUTINE arr2add_two(arg_type, res_type)
      TYPE(array_m), INTENT(in) :: arg_type
      TYPE(array_m), INTENT(out) :: res_type
      res_type%var1 = arg_type%var1*2
      res_type%var2 = arg_type%var2 + 3

   END SUBROUTINE arr2add_two
! ===================MAIN CALCULATION WRAPPER FOR ENERGY AND WATER FLUX===========
   SUBROUTINE SUEWS_cal_Main( &
      AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, & ! input&inout in alphabetical order
      AH_SLOPE_Heating, &
      alb, AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
      AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
      alpha_bioCO2, alpha_enh_bioCO2, alt, kdown, avRh, avU1, BaseT, BaseTe, &
      beta_bioCO2, beta_enh_bioCO2, bldgH, CapMax_dec, CapMin_dec, &
      chAnOHM, CO2PointSource, cpAnOHM, CRWmax, CRWmin, DayWat, DayWatPer, &
      DecTreeH, DiagMethod, Diagnose, DRAINRT, &
      dt_since_start, dqndt, qn_av, dqnsdt, qn_s_av, &
      EF_umolCO2perJ, emis, EmissionsMethod, EnEF_v_Jkm, endDLS, EveTreeH, FAIBldg, &
      FAIDecTree, FAIEveTree, FAIMethod, Faut, FcEF_v_kgkm, fcld_obs, FlowChange, &
      FrFossilFuel_Heat, FrFossilFuel_NonHeat, g_max, g_k, g_q_base, g_q_shape, g_t, g_sm, GDD_id, &
      GDDFull, Gridiv, gsModel, H_maintain, HDD_id, HumActivity_24hr, &
      IceFrac, id, Ie_a, Ie_end, Ie_m, Ie_start, imin, &
      InternalWaterUse_h, &
      IrrFracPaved, IrrFracBldgs, &
      IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
      IrrFracBSoil, IrrFracWater, &
      isec, it, &
      iy, kkAnOHM, Kmax, LAI_id, LAIMax, LAIMin, LAI_obs, &
      LAIPower, LAIType, lat, lenDay_id, ldown_obs, lng, MaxConductance, MaxFCMetab, MaxQFMetab, &
      SnowWater, MinFCMetab, MinQFMetab, min_res_bioCO2, &
      NARP_EMIS_SNOW, NARP_TRANS_SITE, NetRadiationMethod, &
      nlayer, &
      n_vegetation_region_urban, &
      n_stream_sw_urban, n_stream_lw_urban, &
      sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      veg_fsd_const, veg_contact_fraction_const, &
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, & !input
      height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
      alb_roof, emis_roof, alb_wall, emis_wall, &
      roof_albedo_dir_mult_fact, wall_specular_frac, &
      OHM_coef, OHMIncQF, OHM_threshSW, &
      OHM_threshWD, PipeCapacity, PopDensDaytime, &
      PopDensNighttime, PopProf_24hr, PorMax_dec, PorMin_dec, &
      Precip, PrecipLimit, PrecipLimitAlb, Press_hPa, &
      QF0_BEU, Qf_A, Qf_B, Qf_C, &
      qn1_obs, qs_obs, qf_obs, &
      RadMeltFact, RAINCOVER, RainMaxRes, resp_a, resp_b, &
      RoughLenHeatMethod, RoughLenMomMethod, RunoffToWater, S1, S2, &
      SatHydraulicConduct, SDDFull, SDD_id, SMDMethod, SnowAlb, SnowAlbMax, &
      SnowAlbMin, SnowPackLimit, SnowDens, SnowDensMax, SnowDensMin, SnowfallCum, SnowFrac, &
      SnowLimBldg, SnowLimPaved, snowFrac_obs, SnowPack, SnowProf_24hr, SnowMethod, SoilDepth, &
      StabilityMethod, startDLS, &
      soilstore_surf, SoilStoreCap_surf, state_surf, StateLimit_surf, WetThresh_surf, &
      soilstore_roof, SoilStoreCap_roof, state_roof, StateLimit_roof, WetThresh_roof, &
      soilstore_wall, SoilStoreCap_wall, state_wall, StateLimit_wall, WetThresh_wall, &
      StorageHeatMethod, StoreDrainPrm, SurfaceArea, Tair_av, tau_a, tau_f, tau_r, &
      Tmax_id, Tmin_id, &
      BaseT_Cooling, BaseT_Heating, Temp_C, TempMeltFact, TH, &
      theta_bioCO2, timezone, TL, TrafficRate, TrafficUnits, &
      sfr_surf, &
      tsfc_roof, tsfc_wall, tsfc_surf, &
      temp_roof, temp_wall, temp_surf, &
      tin_roof, tin_wall, tin_surf, &
      k_roof, k_wall, k_surf, &
      cp_roof, cp_wall, cp_surf, &
      dz_roof, dz_wall, dz_surf, &
      TraffProf_24hr, Ts5mindata_ir, tstep, tstep_prev, veg_type, &
      WaterDist, WaterUseMethod, wu_m3, &
      WUDay_id, DecidCap_id, albDecTr_id, albEveTr_id, albGrass_id, porosity_id, &
      WUProfA_24hr, WUProfM_24hr, xsmd, Z, z0m_in, zdm_in, &
      output_line_all) ! output
      ! datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, & !output
      ! dataOutLineBEERS, & !output
      ! dataOutLineDebug, dataOutLineSPARTACUS, &
      ! dataOutLineESTMExt, &
      ! dataOutLineDailyState) !output

      IMPLICIT NONE

      ! ########################################################################################
      ! input variables
      INTEGER, PARAMETER :: AerodynamicResistanceMethod = 2 !method to calculate RA [-]
      INTEGER, PARAMETER :: BaseTMethod = 2 ! base t method [-]
      INTEGER, INTENT(IN) :: Diagnose ! flag for printing diagnostic info during runtime [N/A]C
      INTEGER, PARAMETER :: DiagQN = 0 ! flag for printing diagnostic info for QN module during runtime [N/A] ! not used and will be removed
      INTEGER, PARAMETER :: DiagQS = 0 ! flag for printing diagnostic info for QS module during runtime [N/A] ! not used and will be removed
      INTEGER, INTENT(IN) :: startDLS !start of daylight saving  [DOY]
      INTEGER, INTENT(IN) :: endDLS !end of daylight saving [DOY]
      INTEGER, INTENT(IN) :: EmissionsMethod !method to calculate anthropogenic heat [-]
      INTEGER, INTENT(IN) :: Gridiv ! grid id [-]
      INTEGER, INTENT(IN) :: nlayer ! number of vertical layers in urban canyon [-]
      INTEGER, INTENT(IN) :: gsModel !choice of gs parameterisation (1 = Ja11, 2 = Wa16) [-]
      INTEGER, INTENT(IN) :: id ! day of year, 1-366 [-]
      INTEGER, INTENT(IN) :: Ie_end !ending time of water use [DOY]
      INTEGER, INTENT(IN) :: Ie_start !starting time of water use [DOY]
      INTEGER, INTENT(IN) :: isec ! seconds, 0-59 [s]
      INTEGER, INTENT(IN) :: imin !minutes, 0-59 [min]
      INTEGER, INTENT(IN) :: it ! hour, 0-23 [h]
      INTEGER, PARAMETER :: EvapMethod = 2 ! Evaporation calculated according to Rutter (1) or Shuttleworth (2) [-]
      INTEGER, INTENT(IN) :: iy ! year [y]
      INTEGER, PARAMETER :: LAImethod = 1 ! boolean to determine if calculate LAI [-]
      INTEGER, INTENT(IN) :: NetRadiationMethod ! method for calculation of radiation fluxes [-]
      INTEGER, INTENT(IN) :: OHMIncQF ! Determines whether the storage heat flux calculation uses Q* or ( Q* +QF) [-]
      INTEGER, INTENT(IN) :: RoughLenHeatMethod ! method to calculate heat roughness length [-]
      INTEGER, INTENT(IN) :: RoughLenMomMethod ! Determines how aerodynamic roughness length (z0m) and zero displacement height (zdm) are calculated [-]
      INTEGER, INTENT(IN) :: FAIMethod ! Determines how FAI is calculated [-]
      INTEGER, INTENT(IN) :: SMDMethod ! Determines method for calculating soil moisture deficit [-]
      INTEGER, INTENT(IN) :: SnowMethod ! Determines whether the snow part of the model runs[-]
      INTEGER, INTENT(IN) :: StabilityMethod !method to calculate atmospheric stability [-]
      INTEGER, INTENT(IN) :: StorageHeatMethod !Determines method for calculating storage heat flux ΔQS [-]
      INTEGER, INTENT(in) :: DiagMethod !Defines how near surface diagnostics are calculated
      INTEGER, INTENT(IN) :: tstep !timestep [s]
      INTEGER, INTENT(IN) :: tstep_prev ! tstep size of the previous step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]
      INTEGER, INTENT(IN) :: veg_type !Defines how vegetation is calculated for LUMPS [-]
      INTEGER, INTENT(IN) :: WaterUseMethod !Defines how external water use is calculated[-]

      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_DecTr !maximum albedo for deciduous tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_EveTr !maximum albedo for evergreen tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_Grass !maximum albedo for grass [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_DecTr !minimum albedo for deciduous tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_EveTr !minimum albedo for evergreen tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_Grass !minimum albedo for grass [-]
      REAL(KIND(1D0)), INTENT(IN) :: alt !solar altitude [deg]
      REAL(KIND(1D0)), INTENT(IN) :: kdown !incominging shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: avRh !relative humidity [-]
      REAL(KIND(1D0)), INTENT(IN) :: avU1 !average wind speed at 1m [W m-1]
      REAL(KIND(1D0)), PARAMETER :: BaseT_HC = 18.2 !base temperature for heating degree dayb [degC] ! to be fully removed TODO
      REAL(KIND(1D0)), INTENT(IN) :: bldgH !average building height [m]
      REAL(KIND(1D0)), INTENT(IN) :: CapMax_dec !maximum water storage capacity for upper surfaces (i.e. canopy)
      REAL(KIND(1D0)), INTENT(IN) :: CapMin_dec !minimum water storage capacity for upper surfaces (i.e. canopy)
      REAL(KIND(1D0)), INTENT(IN) :: CO2PointSource ! point source [kgC day-1]
      REAL(KIND(1D0)), INTENT(IN) :: CRWmax !maximum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(IN) :: CRWmin !minimum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(IN) :: DecTreeH !average height of deciduous tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: DRAINRT !Drainage rate of the water bucket [mm hr-1]
      REAL(KIND(1D0)), INTENT(IN) :: EF_umolCO2perJ !co2 emission factor [umol J-1]
      REAL(KIND(1D0)), INTENT(IN) :: EnEF_v_Jkm ! energy emission factor [J K m-1]
      REAL(KIND(1D0)), INTENT(IN) :: EveTreeH !height of evergreen tree [m]
      REAL(KIND(1D0)), INTENT(IN) :: FAIBldg ! frontal area index for buildings [-]
      REAL(KIND(1D0)), INTENT(IN) :: FAIDecTree ! frontal area index for deciduous tree [-]
      REAL(KIND(1D0)), INTENT(IN) :: FAIEveTree ! frontal area index for evergreen tree [-]
      REAL(KIND(1D0)), INTENT(IN) :: Faut !Fraction of irrigated area using automatic irrigation [-]
      REAL(KIND(1D0)), INTENT(IN) :: fcld_obs !observed could fraction [-]
      REAL(KIND(1D0)), INTENT(IN) :: FlowChange !Difference between the input and output flow in the water body [mm]
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_Heat ! fraction of fossil fuel heat [-]
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_NonHeat ! fraction of fossil fuel non heat [-]
      REAL(KIND(1D0)), INTENT(IN) :: g_max !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_k !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_q_base !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_q_shape !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_t !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_sm !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: H_maintain ! ponding water depth to maintain [mm]
      REAL(KIND(1D0)), INTENT(IN) :: InternalWaterUse_h !Internal water use [mm h-1]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracPaved !fraction of paved which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBldgs !fraction of buildings (e.g., green roofs) which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracDecTr !fraction of deciduous trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracEveTr !fraction of evergreen trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracGrass !fraction of grass which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBSoil !fraction of bare soil trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracWater !fraction of water which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: Kmax !annual maximum hourly solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: LAI_obs !observed LAI [m2 m-2]
      REAL(KIND(1D0)), INTENT(IN) :: lat !latitude [deg]
      REAL(KIND(1D0)), INTENT(IN) :: ldown_obs !observed incoming longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: lng !longitude [deg]
      REAL(KIND(1D0)), INTENT(IN) :: MaxFCMetab ! maximum FC metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(IN) :: MaxQFMetab ! maximum QF Metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: MinFCMetab ! minimum QF metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(IN) :: MinQFMetab ! minimum FC metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: NARP_EMIS_SNOW ! snow emissivity in NARP model [-]
      REAL(KIND(1D0)), INTENT(IN) :: NARP_TRANS_SITE !atmospheric transmissivity for NARP [-]
      REAL(KIND(1D0)), INTENT(IN) :: PipeCapacity !capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)), INTENT(IN) :: PopDensNighttime ! nighttime population density (i.e. residents) [ha-1]
      REAL(KIND(1D0)), INTENT(IN) :: PorMax_dec !full leaf-on summertime value used only for DecTr
      REAL(KIND(1D0)), INTENT(IN) :: PorMin_dec !leaf-off wintertime value used only for DecTr
      REAL(KIND(1D0)), INTENT(IN) :: Precip !rain data [mm]
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimit !temperature limit when precipitation falls as snow [degC]
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimitAlb !Limit for hourly precipitation when the ground is fully covered with snow [mm]
      REAL(KIND(1D0)), INTENT(IN) :: Press_hPa !air pressure [hPa]
      REAL(KIND(1D0)), INTENT(IN) :: qn1_obs !observed net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: qs_obs !observed heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: qf_obs !observed anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: RadMeltFact !hourly radiation melt factor of snow [mm W-1 h-1]
      REAL(KIND(1D0)), INTENT(IN) :: RAINCOVER !limit when surface totally covered with water for LUMPS [mm]
      REAL(KIND(1D0)), INTENT(IN) :: RainMaxRes !maximum water bucket reservoir. Used for LUMPS surface wetness control. [mm]
      REAL(KIND(1D0)), INTENT(IN) :: RunoffToWater !fraction of above-ground runoff flowing to water surface during flooding [-]
      REAL(KIND(1D0)), INTENT(IN) :: S1 !a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)), INTENT(IN) :: S2 !a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMax !effective surface albedo (middle of the day value) for summertime [-]
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMin !effective surface albedo (middle of the day value) for wintertime (not including snow) [-]
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMax !maximum snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMin !fresh snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimBldg !Limit of the snow water equivalent for snow removal from building roofs [mm]
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimPaved !limit of the snow water equivalent for snow removal from roads[mm]
      REAL(KIND(1D0)), INTENT(IN) :: snowFrac_obs !observed snow fraction [-]
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea !area of the grid [ha]
      REAL(KIND(1D0)), INTENT(IN) :: tau_a !time constant for snow albedo aging in cold snow [-]
      REAL(KIND(1D0)), INTENT(IN) :: tau_f !time constant for snow albedo aging in melting snow [-]
      REAL(KIND(1D0)), INTENT(IN) :: tau_r !time constant for snow density ageing [-]
      REAL(KIND(1D0)), INTENT(IN) :: Temp_C !air temperature [degC]
      REAL(KIND(1D0)), INTENT(IN) :: TempMeltFact !hourly temperature melt factor of snow [mm K-1 h-1]
      REAL(KIND(1D0)), INTENT(IN) :: TH !upper air temperature limit [degC]
      REAL(KIND(1D0)), INTENT(IN) :: timezone !time zone, for site relative to UTC (east is positive) [h]
      REAL(KIND(1D0)), INTENT(IN) :: TL !lower air temperature limit [degC]
      REAL(KIND(1D0)), INTENT(IN) :: TrafficUnits ! traffic units choice [-]
      REAL(KIND(1D0)), INTENT(IN) :: wu_m3 ! external water input (e.g., irrigation)  [m3]
      REAL(KIND(1D0)), INTENT(IN) :: xsmd ! observed soil moisture; can be provided either as volumetric ([m3 m-3] when SMDMethod = 1) or gravimetric quantity ([kg kg-1] when SMDMethod = 2
      REAL(KIND(1D0)), INTENT(IN) :: Z ! measurement height [m]
      REAL(KIND(1D0)), INTENT(IN) :: z0m_in !roughness length for momentum [m]
      REAL(KIND(1D0)), INTENT(IN) :: zdm_in !zero-plane displacement [m]

      INTEGER, DIMENSION(NVEGSURF), INTENT(IN) :: LAIType !LAI calculation choice[-]

      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_MIN !minimum QF values [W m-2]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Cooling ! cooling slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Heating ! heating slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: FcEF_v_kgkm ! CO2 Emission factor [kg km-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: QF0_BEU ! Fraction of base value coming from buildings [-]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_A ! Base value for QF [W m-2]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_B ! Parameter related to heating degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_C ! Parameter related to cooling degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: PopDensDaytime ! Daytime population density [people ha-1] (i.e. workers)
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Cooling ! base temperature for cooling degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Heating ! base temperatrue for heating degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: TrafficRate ! Traffic rate [veh km m-2 s-1]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_a !Coefficient for automatic irrigation model
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_m !Coefficients for manual irrigation models
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: MaxConductance !the maximum conductance of each vegetation or surface type. [mm s-1]
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWat !Irrigation flag: 1 for on and 0 for off [-]
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWatPer !Fraction of properties using irrigation for each day of a week [-]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshSW !Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshWD !Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: chAnOHM !Bulk transfer coefficient for this surface to use in AnOHM [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: cpAnOHM !Volumetric heat capacity for this surface to use in AnOHM [J m-3]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: emis !Effective surface emissivity[-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: kkAnOHM !Thermal conductivity for this surface to use in AnOHM [W m K-1]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SatHydraulicConduct !Hydraulic conductivity for saturated soil [mm s-1]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf !surface cover fraction[-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SnowPackLimit !Limit for the snow water equivalent when snow cover starts to be patchy [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilDepth !Depth of soil beneath the surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: StateLimit_surf !Upper limit to the surface state [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: WetThresh_surf ! !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_bioCO2 !The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve [-]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_enh_bioCO2 !Part of the alpha coefficient related to the fraction of vegetation[-]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseT !Base Temperature for initiating growing degree days (GDD) for leaf growth [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseTe !Base temperature for initiating sensesance degree days (SDD) for leaf off [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_bioCO2 !The light-saturated gross photosynthesis of the canopy [umol m-2 s-1 ]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_enh_bioCO2 !Part of the beta coefficient related to the fraction of vegetation [umol m-2 s-1 ]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: GDDFull !the growing degree days (GDD) needed for full capacity of the leaf area index [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMax !full leaf-on summertime value [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMin !leaf-off wintertime value [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: min_res_bioCO2 !Minimum soil respiration rate (for cold-temperature limit) [umol m-2 s-1]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_a !Respiration coefficient a
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_b !Respiration coefficient b - related to air temperature dependency
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: SDDFull !the sensesence degree days (SDD) needed to initiate leaf off [degC]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: SnowProf_24hr !Hourly profile values used in snow clearing [-]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: theta_bioCO2 !The convexity of the curve at light saturation [-]
      REAL(KIND(1D0)), DIMENSION(4, NVEGSURF), INTENT(IN) :: LAIPower !parameters required by LAI calculation
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, 4, 3), INTENT(IN) :: OHM_coef !Coefficients for OHM calculation
      REAL(KIND(1D0)), DIMENSION(NSURF + 1, NSURF - 1), INTENT(IN) :: WaterDist !Fraction of water redistribution [-]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: Ts5mindata_ir !surface temperature input data[degC]
      REAL(KIND(1D0)), DIMENSION(10) :: MetForcingData_grid ! met forcing array of grid

      ! diurnal profile values for 24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: AHProf_24hr !Hourly profile values used in energy use calculation [-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: HumActivity_24hr !Hourly profile values used in human activity calculation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: PopProf_24hr !Hourly profile values used in dynamic population estimation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: TraffProf_24hr !Hourly profile values used in traffic activity calculation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfA_24hr !Hourly profile values used in automatic irrigation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfM_24hr !Hourly profile values used in manual irrigation[-]

      ! ####################################################################################
      ! ESTM_ehc
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_roof !Capacity of soil store for roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_roof !Limit for state_id of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_roof ! wetness threshold  of roof[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_roof !Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_roof !wetness status of roof [mm]

      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_wall !Capacity of soil store for wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_wall !Limit for state_id of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_wall ! wetness threshold  of wall[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_wall !Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_wall !wetness status of wall [mm]

      ! ########################################################################################

      ! ########################################################################################
      ! inout variables
      ! OHM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_av ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: dqndt ! rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_s_av ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: dqnsdt ! Rate of change of net radiation [W m-2 h-1]

      ! snow related:
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowfallCum !cumulated snow falling [mm]
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowAlb !albedo of know [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: IceFrac !fraction of ice in snowpack [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowWater ! snow water[mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowDens !snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowFrac !snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowPack !snow water equivalent on each land cover [mm]

      ! water balance related:
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: soilstore_surf !soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: state_surf !wetness status of each surface type [mm]
      ! ---- Drainage characteristics ----------------------------------------------------------------
      REAL(KIND(1D0)), DIMENSION(6, NSURF), INTENT(IN) :: StoreDrainPrm !coefficients used in drainage calculation [-]
      ! 1 - min storage capacity [mm]
      ! 2 - Drainage equation to use
      ! 3 - Drainage coeff 1 [units depend on choice of eqn]
      ! 4 - Drainage coeff 2 [units depend on choice of eqn]
      ! 5 - max storage capacity [mm]
      ! 6 - current storage capacity [mm]
      !-----------------------------------------------------------------------------------------------

      ! phenology related:
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: alb !albedo [-]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: GDD_id !Growing Degree Days [degC d]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INout) :: SDD_id !Senescence Degree Days[degC d]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: LAI_id !LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), INTENT(INout) :: Tmin_id !Daily minimum temperature [degC]
      REAL(KIND(1D0)), INTENT(INout) :: Tmax_id !Daily maximum temperature [degC]
      REAL(KIND(1D0)), INTENT(INout) :: lenDay_id !daytime length [h]
      REAL(KIND(1D0)), INTENT(INOUT) :: DecidCap_id !Moisture storage capacity of deciduous trees [mm]
      REAL(KIND(1D0)), INTENT(INOUT) :: albDecTr_id !Albedo of deciduous trees [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: albEveTr_id !Albedo of evergreen trees [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: albGrass_id !Albedo of grass  [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id !Porosity of deciduous trees [-]

      ! anthropogenic heat related:
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating Degree Days [degC d]

      ! water use related:
      REAL(KIND(1D0)), DIMENSION(9), INTENT(INOUT) :: WUDay_id !Daily water use for EveTr, DecTr, Grass [mm]

      ! ESTM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: Tair_av !average air temperature [degC]

      ! ESTM_ehc related:
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(INOUT) :: temp_roof !interface temperature between depth layers in roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(INOUT) :: temp_wall !interface temperature between depth layers in wall [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(INOUT) :: temp_surf !interface temperature between depth layers [degC]

      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_roof !roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_wall !wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(INOUT) :: tsfc_surf !surface temperature [degC]

      ! SPARTACUS input variables
      INTEGER, INTENT(IN) :: n_vegetation_region_urban !Number of regions used to describe vegetation [-]
      INTEGER, INTENT(IN) :: n_stream_sw_urban ! shortwave diffuse streams per hemisphere [-]
      INTEGER, INTENT(IN) :: n_stream_lw_urban ! LW streams per hemisphere [-]
      REAL(KIND(1D0)), INTENT(IN) :: sw_dn_direct_frac
      REAL(KIND(1D0)), INTENT(IN) :: air_ext_sw
      REAL(KIND(1D0)), INTENT(IN) :: air_ssa_sw
      REAL(KIND(1D0)), INTENT(IN) :: veg_ssa_sw
      REAL(KIND(1D0)), INTENT(IN) :: air_ext_lw
      REAL(KIND(1D0)), INTENT(IN) :: air_ssa_lw
      REAL(KIND(1D0)), INTENT(IN) :: veg_ssa_lw
      REAL(KIND(1D0)), INTENT(IN) :: veg_fsd_const
      REAL(KIND(1D0)), INTENT(IN) :: veg_contact_fraction_const
      REAL(KIND(1D0)), INTENT(IN) :: ground_albedo_dir_mult_fact

      ! ########################################################################################

      ! ########################################################################################
      ! output variables
      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5) :: dataOutLineESTMExt
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5) :: dataoutLineRSL
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug - 5) :: dataOutLineDebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5) :: dataOutLineSPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5) :: dataOutLineDailyState
      ! save all output variables in a single derived type
      TYPE(output_line), INTENT(OUT) :: output_line_all
      ! ########################################################################################

      ! ########################################################################################
      ! local variables
      REAL(KIND(1D0)) :: a1 !AnOHM coefficients of grid [-]
      REAL(KIND(1D0)) :: a2 ! AnOHM coefficients of grid [h]
      REAL(KIND(1D0)) :: a3 !AnOHM coefficients of grid [W m-2]
      REAL(KIND(1D0)) :: AdditionalWater !!Additional water coming from other grids [mm] (these are expressed as depths over the whole surface)
      REAL(KIND(1D0)) :: U10_ms !average wind speed at 10m [W m-1]
      REAL(KIND(1D0)) :: azimuth !solar azimuth [angle]
      REAL(KIND(1D0)) :: chSnow_per_interval ! change state_id of snow and surface per time interval [mm]

      REAL(KIND(1D0)) :: dens_dry !Vap density or absolute humidity (kg m-3)
      REAL(KIND(1D0)) :: deltaLAI !change in LAI [m2 m-2]
      REAL(KIND(1D0)) :: drain_per_tstep ! total drainage for all surface type at each timestep [mm]
      REAL(KIND(1D0)) :: Ea_hPa !vapor pressure [hPa]
      REAL(KIND(1D0)) :: QE_LUMPS !turbulent latent heat flux by LUMPS model [W m-2]
      REAL(KIND(1D0)) :: es_hPa !Saturation vapour pressure over water  [hPa]
      REAL(KIND(1D0)) :: ev_per_tstep ! evaporation at each time step [mm]
      REAL(KIND(1D0)) :: wu_ext !external water use [mm]
      REAL(KIND(1D0)) :: Fc !total co2 flux [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_anthro !anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_biogen !biogenic CO2 flux [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_build ! anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)) :: fcld !estomated cloud fraction [-]
      REAL(KIND(1D0)) :: Fc_metab ! co2 emission from metabolism component [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_photo !co2 flux from photosynthesis [umol m
      REAL(KIND(1D0)) :: Fc_point ! co2 emission from point source [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_respi !co2 flux from respiration [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_traff ! co2 emission from traffic component [umol m-2 s-1]
      REAL(KIND(1D0)) :: gfunc
      REAL(KIND(1D0)) :: gsc !Surface Layer Conductance
      REAL(KIND(1D0)) :: QH_LUMPS !turbulent sensible heat flux from LUMPS model [W m-2]
      REAL(KIND(1D0)) :: wu_int !internal water use [mm]
      REAL(KIND(1D0)) :: kclear !clear sky incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: kup !outgoing shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: ldown !incoming longtwave radiation [W m-2]
      REAL(KIND(1D0)) :: lup !outgoing longwave radiation [W m-2]
      REAL(KIND(1D0)) :: L_mod !Obukhov length [m]
      REAL(KIND(1D0)) :: mwh !snowmelt [mm]
      REAL(KIND(1D0)) :: mwstore !overall met water [mm]
      REAL(KIND(1D0)) :: NWstate_per_tstep ! state_id at each tinestep(excluding water body) [mm]
      REAL(KIND(1D0)) :: FAI ! frontal area index [-]
      REAL(KIND(1D0)) :: PAI ! plan area index [-]
      REAL(KIND(1D0)) :: zL ! Stability scale [-]
      REAL(KIND(1D0)) :: q2_gkg ! Air specific humidity at 2 m [g kg-1]
      REAL(KIND(1D0)) :: qe !turbuent latent heat flux [W m-2]
      REAL(KIND(1D0)) :: qf !anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)) :: QF_SAHP !total anthropogeic heat flux when EmissionMethod is not 0 [W m-2]
      REAL(KIND(1D0)) :: qh !turbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_residual ! residual based sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_resist !resistance bnased sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: Qm !Snowmelt-related heat [W m-2]
      REAL(KIND(1D0)) :: QmFreez !heat related to freezing of surface store [W m-2]
      REAL(KIND(1D0)) :: QmRain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)) :: qn !net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: qn_snow !net all-wave radiation on snow surface [W m-2]
      REAL(KIND(1D0)) :: qn_snowfree !net all-wave radiation on snow-free surface [W m-2]
      REAL(KIND(1D0)) :: qs !heat storage flux [W m-2]
      REAL(KIND(1D0)) :: RA_h ! aerodynamic resistance [s m-1]
      REAL(KIND(1D0)) :: RS ! surface resistance [s m-1]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: rss_surf ! surface resistance adjusted by surface wetness state[s m-1]
      REAL(KIND(1D0)) :: RH2 ! air relative humidity at 2m [-]
      REAL(KIND(1D0)) :: runoffAGveg !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)) :: runoffAGimpervious !Above ground runoff from impervious surface for all surface area [mm]
      REAL(KIND(1D0)) :: runoff_per_tstep !runoff water at each time step [mm]
      REAL(KIND(1D0)) :: runoffPipes !runoff to pipes [mm]
      REAL(KIND(1D0)) :: runoffSoil_per_tstep !Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
      REAL(KIND(1D0)) :: runoffwaterbody !Above ground runoff from water body for all surface area [mm]
      REAL(KIND(1D0)) :: smd !soil moisture deficit [mm]
      REAL(KIND(1D0)) :: SoilState !Area-averaged soil moisture  for whole surface [mm]
      REAL(KIND(1D0)) :: state_per_tstep !state_id at each timestep [mm]
      REAL(KIND(1D0)) :: surf_chang_per_tstep !change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)) :: swe !overall snow water equavalent[mm]
      REAL(KIND(1D0)) :: t2_C !modelled 2 meter air temperature [degC]
      REAL(KIND(1D0)) :: TSfc_C ! surface temperature [degC]
      REAL(KIND(1D0)) :: TempVeg ! temporary vegetative surface fraction adjusted by rainfall [-]
      REAL(KIND(1D0)) :: tot_chang_per_tstep !Change in surface state_id [mm]
      REAL(KIND(1D0)) :: TStar !T*, temperature scale [-]
      REAL(KIND(1D0)) :: tsurf !surface temperatue [degC]
      REAL(KIND(1D0)) :: UStar !friction velocity [m s-1]
      REAL(KIND(1D0)) :: VPD_Pa !vapour pressure deficit  [Pa]
      REAL(KIND(1D0)) :: z0m !Aerodynamic roughness length [m]
      REAL(KIND(1D0)) :: zdm !zero-plane displacement [m]
      REAL(KIND(1D0)) :: ZENITH_deg !solar zenith angle in degree [°]
      REAL(KIND(1D0)) :: zH ! Mean building height [m]

      REAL(KIND(1D0)), DIMENSION(2) :: SnowRemoval !snow removal [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: wu_surf !external water use of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezMelt !freezing of melt water[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind_snow !outgoing shortwave on snowpack [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: mw_ind !melt water from sknowpack[mm]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_freezState !heat related to freezing of surface store [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_melt !melt heat [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_rain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: qn_ind_snow !net all-wave radiation on snowpack [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: rainOnSnow !rain water on snow event [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: runoffSoil !Soil runoff from each soil sub-surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: smd_nsurf !soil moisture deficit for each surface
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: snowDepth !Snow depth [m]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: Tsurf_ind_snow !snowpack surface temperature [C]

      ! INTEGER, DIMENSION(NSURF) :: snowCalcSwitch
      INTEGER, DIMENSION(3) :: dayofWeek_id ! 1 - day of week; 2 - month; 3 - season
      INTEGER :: DLS

      REAL(KIND(1D0)) :: dq !Specific humidity deficit [g/kg]
      REAL(KIND(1D0)) :: lvS_J_kg !latent heat of sublimation [J kg-1]
      REAL(KIND(1D0)) :: psyc_hPa !psychometric constant [hPa]
      REAL(KIND(1D0)) :: z0v !roughness for heat [m]
      REAL(KIND(1D0)) :: z0vSnow !roughness for heat [m]
      REAL(KIND(1D0)) :: RAsnow !Aerodynamic resistance for snow [s m-1]
      REAL(KIND(1D0)) :: RB !boundary layer resistance shuttleworth
      REAL(KIND(1D0)) :: runoff_per_interval !run-off at each time interval [mm]
      REAL(KIND(1D0)) :: s_hPa !vapour pressure versus temperature slope [hPa K-1]
      REAL(KIND(1D0)) :: sIce_hpa !satured curve on snow [hPa]
      REAL(KIND(1D0)) :: SoilMoistCap !Maximum capacity of soil store [mm]
      ! REAL(KIND(1D0)) :: veg_fr !vegetation fraction [-]
      REAL(KIND(1D0)) :: VegPhenLumps
      REAL(KIND(1D0)) :: VPd_hpa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)) :: vsmd !Soil moisture deficit for vegetated surfaces only [mm]
      REAL(KIND(1D0)) :: ZZD !Active measurement height[m]

      REAL(KIND(1D0)), DIMENSION(NSURF) :: deltaQi ! storage heat flux of snow surfaces [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: drain_surf !drainage of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezState !freezing of state_id [mm]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezStateVol !surface state_id [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: tsurf_ind !snow-free surface temperature [degC]

      ! TODO: TS 25 Oct 2017
      ! the  variables are not used currently as grid-to-grid connection is NOT set up.
      ! set these variables as zero.
      REAL(KIND(1D0)) :: addImpervious = 0
      REAL(KIND(1D0)) :: addPipes = 0
      REAL(KIND(1D0)) :: addVeg = 0
      REAL(KIND(1D0)) :: addWaterBody = 0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: AddWater = 0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: frac_waterdist_vert = 0 !Fraction of water redistributed vertically: to either runoff (if impervious) or sub-surface soil (pervious) [-]

      ! values that are derived from tstep
      INTEGER :: nsh ! number of timesteps per hour
      REAL(KIND(1D0)) :: nsh_real ! nsh in type real [-]
      REAL(KIND(1D0)) :: tstep_real ! tstep in type real
      REAL(KIND(1D0)) :: dectime !decimal time [-]

      ! values that are derived from sfr_surf (surface fractions)
      REAL(KIND(1D0)) :: VegFraction ! fraction of vegetation [-]
      REAL(KIND(1D0)) :: ImpervFraction !fractioin of impervious surface [-]
      REAL(KIND(1D0)) :: PervFraction !fraction of pervious surfaces [-]
      REAL(KIND(1D0)) :: NonWaterFraction !fraction of non-water [-]

      ! snow related temporary values
      REAL(KIND(1D0)) :: albedo_snow !snow albedo [-]

      ! ########################################################################################
      ! TS 19 Sep 2019
      ! temporary variables to save values for inout varialbes
      ! suffixes  and  denote values from last and to next tsteps, respectively
      ! these variables are introduced to allow safe and robust iterations inccurred in this subroutine
      ! so that these values won't updated in unexpectedly many times

      ! OHM related:
      REAL(KIND(1D0)) :: qn_av_prev, qn_av_next ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: dqndt_prev, dqndt_next ! Rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)) :: qn_s_av_prev, qn_s_av_next ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)) :: dqnsdt_prev, dqnsdt_next ! Rate of change of net radiation [W m-2 h-1]

      ! snow related:
      REAL(KIND(1D0)) :: SnowfallCum_prev, SnowfallCum_next !cumulative snow depth [mm]
      REAL(KIND(1D0)) :: SnowAlb_prev, SnowAlb_next !snow albedo [-]

      REAL(KIND(1D0)), DIMENSION(NSURF) :: IceFrac_prev, IceFrac_next !fraction of ice in snowpack [-]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: SnowWater_prev, SnowWater_next ! snow water[mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: SnowDens_prev, SnowDens_next !snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: SnowFrac_prev, SnowFrac_next !snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: SnowPack_prev, SnowPack_next !snow water equivalent on each land cover [mm]

      ! water balance related:
      REAL(KIND(1D0)), DIMENSION(NSURF) :: soilstore_surf_prev, soilstore_surf_next !soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: soilstore_roof_prev, soilstore_roof_next !soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: soilstore_wall_prev, soilstore_wall_next !soil moisture of wall[mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: state_surf_prev, state_surf_next !wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: state_roof_prev, state_roof_next !wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: state_wall_prev, state_wall_next !wetness status of wall [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev0_surf ! evapotranspiration from PM of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev_surf ! evapotranspiration of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_roof ! evapotranspiration of each roof layer [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_wall ! evapotranspiration of each wall type [mm]
      REAL(KIND(1D0)), DIMENSION(6, NSURF) :: StoreDrainPrm_prev, StoreDrainPrm_next !coefficients used in drainage calculation [-]

      ! phenology related:
      REAL(KIND(1D0)), DIMENSION(NSURF) :: alb_prev, alb_next !albedo [-]
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDD_id_prev, GDD_id_next !Growing Degree Days [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAI_id_prev, LAI_id_next !Senescence Degree Days[degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDD_id_prev, SDD_id_next !LAI for each veg surface [m2 m-2]

      REAL(KIND(1D0)) :: DecidCap_id_prev, DecidCap_id_next !Moisture storage capacity of deciduous trees [mm]
      REAL(KIND(1D0)) :: albDecTr_id_prev, albDecTr_id_next !Albedo of deciduous trees [-]
      REAL(KIND(1D0)) :: albEveTr_id_prev, albEveTr_id_next !Albedo of evergreen trees [-]
      REAL(KIND(1D0)) :: albGrass_id_prev, albGrass_id_next !Albedo of grass  [-]
      REAL(KIND(1D0)) :: porosity_id_prev, porosity_id_next !Porosity of deciduous trees [-]

      REAL(KIND(1D0)) :: Tmin_id_prev, Tmin_id_next !Daily minimum temperature [degC]
      REAL(KIND(1D0)) :: Tmax_id_prev, Tmax_id_next !Daily maximum temperature [degC]
      REAL(KIND(1D0)) :: lenDay_id_prev, lenDay_id_next !daytime length [h]

      ! anthropogenic heat related:
      REAL(KIND(1D0)), DIMENSION(12) :: HDD_id_prev, HDD_id_next !Heating Degree Days [degC d]

      ! water use related:
      REAL(KIND(1D0)), DIMENSION(9) :: WUDay_id_prev, WUDay_id_next !Daily water use for EveTr, DecTr, Grass [mm]

      REAL(KIND(1D0)) :: Tair_av_prev, Tair_av_next !average air temperature [degC]
      ! ########################################################################################

      ! Related to RSL wind profiles
      INTEGER, PARAMETER :: nz = 90 ! number of levels 10 levels in canopy plus 20 (3 x Zh) above the canopy

      ! flag for Tsurf convergence
      LOGICAL :: flag_converge
      REAL(KIND(1D0)) :: Ts_iter !average surface temperature of all surfaces [degC]
      REAL(KIND(1D0)) :: dif_tsfc_iter
      REAL(KIND(1D0)) :: QH_Init !initialised sensible heat flux [W m-2]
      INTEGER :: i_iter

      ! ########################################################################################
      !  ! extended for ESTM_ehc, TS 20 Jan 2022
      !
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: tsfc_out_roof, tsfc0_out_roof !surface temperature of roof[degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof ! indoor temperature for roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_roof !roof surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_in_roof ! temperature at inner interfaces of roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof ! thermal conductivity of roof [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof ! Heat capacity of roof [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof ! thickness of each layer in roof [m]
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: tsfc_out_wall, tsfc0_out_wall !surface temperature of wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall ! indoor temperature for wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_wall !wall surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_in_wall ! temperature at inner interfaces of wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall ! thermal conductivity of wall [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall ! Heat capacity of wall [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall ! thickness of each layer in wall [m]
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_out_surf, tsfc0_out_surf !surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf !deep bottom temperature for each surface [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth) :: temp_in_surf ! temperature at inner interfaces of of each surface [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf ! thermal conductivity of v [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf ! Heat capacity of each surface [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf ! thickness of each layer in each surface [m]

      ! output arrays:

      ! roof facets
      ! aggregated heat storage of all roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QS_roof ! heat storage flux for roof component [W m-2]
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_out_roof !interface temperature between depth layers [degC]

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QG_roof ! heat flux used in ESTM_ehc as forcing of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QN_roof ! net all-wave radiation of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qe_roof ! latent heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_roof ! sensible heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_resist_roof ! resist-based sensible heat flux of roof surface [W m-2]

      ! wall facets
      ! aggregated heat storage of all wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QS_wall ! heat storage flux for wall component [W m-2]
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_out_wall !interface temperature between depth layers [degC]

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QG_wall ! heat flux used in ESTM_ehc as forcing of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QN_wall ! net all-wave radiation of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qe_wall ! latent heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_wall ! sensible heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_resist_wall ! resistance based sensible heat flux of wall surface [W m-2]

      ! standard suews surfaces
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth) :: temp_out_surf !interface temperature between depth layers[degC]

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf) :: QG_surf ! heat flux used in ESTM_ehc as forcing of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: QN_surf ! net all-wave radiation of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qs_surf ! aggregated heat storage of of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe0_surf ! latent heat flux from PM of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_surf ! sensinle heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_resist_surf ! resistance based sensible heat flux of individual surface [W m-2]
      ! surface temperature
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_qh_surf ! latent heat flux of individual surface [W m-2]

      ! iterator for surfaces
      INTEGER :: i_surf !iterator for surfaces

      ! used in iteration
      INTEGER :: max_iter !maximum iteration
      REAL(KIND(1D0)) :: ratio_iter

      LOGICAL, INTENT(IN) :: use_sw_direct_albedo !boolean, Specify ground and roof albedos separately for direct solar radiation [-]

      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height ! height in spartacus [m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac !building fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac !vegetation fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale ! diameter of buildings [[m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale ! scale of tree crowns [m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof !albedo of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof ! emissivity of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall !albedo of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall ! emissivity of wall [-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact !Ratio of the direct and diffuse albedo of the roof[-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac ! Fraction of wall reflection that is specular [-]

      REAL(KIND(1D0)) :: g_kdown !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: g_dq !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: g_ta !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: g_smd !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: g_lai !gdq*gtemp*gs*gq for photosynthesis calculations

      ! ####
      ! set initial values for output arrays
      SWE = 0.
      mwh = 0.
      MwStore = 0.
      chSnow_per_interval = 0.
      SnowRemoval = 0.
      Qm = 0
      QmFreez = 0
      QmRain = 0

      ! these output variables are used for debugging
      qe0_surf = 0 ! QE from PM: only meaningful when snowuse=0
      ev0_surf = 0 ! ev from PM: only meaningful when snowuse=0
      ev_surf = 0 ! ev from water balance: only meaningful when snowuse=0

      ! ####
      ! force several snow related state variables to zero if snow module is off
      IF (SnowMethod == 0) THEN
         SnowDens = 0.
         SnowFrac = 0.
         SnowWater = 0.
         SnowAlb = 0.
         IceFrac = 0.
         SnowPack = 0.
      END IF

      ! ########################################################################################
      ! save initial values of inout variables
      qn_av_prev = qn_av
      dqndt_prev = dqndt
      qn_s_av_prev = qn_s_av
      dqnsdt_prev = dqnsdt
      SnowfallCum_prev = SnowfallCum
      SnowAlb_prev = SnowAlb
      IceFrac_prev = IceFrac
      SnowWater_prev = SnowWater
      SnowDens_prev = SnowDens
      SnowFrac_prev = MERGE(SnowFrac_obs, SnowFrac, NetRadiationMethod == 0)
      SnowPack_prev = SnowPack
      state_surf_prev = state_surf
      soilstore_surf_prev = soilstore_surf
      IF (StorageHeatMethod == 5) THEN
         state_roof_prev = state_roof
         state_wall_prev = state_wall
         soilstore_roof_prev = soilstore_roof
         soilstore_wall_prev = soilstore_wall
      END IF
      Tair_av_prev = Tair_av
      LAI_id_prev = LAI_id
      GDD_id_prev = GDD_id
      SDD_id_prev = SDD_id
      Tmin_id_prev = Tmin_id
      Tmax_id_prev = Tmax_id
      lenDay_id_prev = lenDay_id
      StoreDrainPrm_prev = StoreDrainPrm
      DecidCap_id_prev = DecidCap_id
      porosity_id_prev = porosity_id
      alb_prev = alb
      albDecTr_id_prev = albDecTr_id
      albEveTr_id_prev = albEveTr_id
      albGrass_id_prev = albGrass_id
      HDD_id_prev = HDD_id
      WUDay_id_prev = WUDay_id

      ! ESTM_ehc related
      ! save initial values of inout variables
      IF (StorageHeatMethod == 5) THEN
         temp_in_roof = temp_roof
         temp_in_wall = temp_wall
         temp_in_surf = temp_surf
      END IF
      ! initialise indoor/bottom boundary temperature arrays
      ! tin_roof = 10.
      ! tin_wall = 10.
      ! tin_surf = 3.

      ! initialise  variables
      qn_av_next = qn_av
      dqndt_next = dqndt
      qn_s_av_next = qn_s_av
      dqnsdt_next = dqnsdt
      SnowfallCum_next = SnowfallCum
      SnowAlb_next = SnowAlb
      IceFrac_next = IceFrac
      SnowWater_next = SnowWater
      SnowDens_next = SnowDens
      SnowFrac_next = SnowFrac_prev
      SnowPack_next = SnowPack
      state_surf_next = state_surf
      soilstore_surf_next = soilstore_surf

      IF (StorageHeatMethod == 5) THEN

         soilstore_roof_next = soilstore_roof
         soilstore_wall_next = soilstore_wall
         state_roof_next = state_roof
         state_wall_next = state_wall

      END IF

      Tair_av_next = Tair_av
      LAI_id_next = LAI_id
      GDD_id_next = GDD_id
      SDD_id_next = SDD_id
      Tmin_id_next = Tmin_id
      Tmax_id_next = Tmax_id
      lenDay_id_next = lenDay_id
      StoreDrainPrm_next = StoreDrainPrm
      DecidCap_id_next = DecidCap_id
      porosity_id_next = porosity_id
      alb_next = alb
      albDecTr_id_next = albDecTr_id
      albEveTr_id_next = albEveTr_id
      albGrass_id_next = albGrass_id
      HDD_id_next = HDD_id
      WUDay_id_next = WUDay_id

      ! initialise output variables
      dataOutLineSnow = -999.
      dataOutLineESTM = -999.
      dataOutLineESTMExt = -999.
      dataoutLineRSL = -999.
      dataOutLineBEERS = -999.
      dataOutLineDebug = -999.
      dataOutLineSPARTACUS = -999.
      dataOutLineDailyState = -999.

      ! transfer input variables to local variables
      StoreCap_surf = StoreDrainPrm(6, :)
      StoreCap_surf_prev = StoreCap_surf
      StoreCap_surf_next = StoreCap_surf
      StoreCapMax_surf = StoreDrainPrm(5, :)
      StoreCapMin_surf = StoreDrainPrm(1, :)
      DrainEq_surf = StoreDrainPrm(2, :)
      DrainCoef1_surf = StoreDrainPrm(3, :)
      DrainCoef2_surf = StoreDrainPrm(4, :)

      !########################################################################################
      !           main calculation starts here
      !########################################################################################

      ! iteration is used below to get results converge
      flag_converge = .FALSE.
      Ts_iter = TEMP_C

      tsfc_out_surf = tsfc_surf
      tsfc0_out_surf = tsfc_surf
      ! TODO: ESTM work: to allow heterogeneous surface temperatures
      IF (StorageHeatMethod == 5 .OR. NetRadiationMethod > 1000) THEN
         tsfc_out_roof = tsfc_roof
         tsfc0_out_roof = tsfc_roof
         tsfc_out_wall = tsfc_wall
         tsfc0_out_wall = tsfc_wall
      END IF
      ! PRINT *, 'sfr_surf for this grid ', sfr_surf
      ! PRINT *, 'before iteration Ts_iter = ', Ts_iter
      ! L_mod_iter = 10
      i_iter = 1
      max_iter = 30
      DO WHILE ((.NOT. flag_converge) .AND. i_iter < max_iter)
         ! PRINT *, '=========================== '
         ! PRINT *, 'Ts_iter of ', i_iter, ' is:', Ts_iter

         ! calculate dectime
         CALL SUEWS_cal_dectime( &
            id, it, imin, isec, & ! input
            dectime) ! output

         ! calculate tstep related VARIABLES
         CALL SUEWS_cal_tstep( &
            tstep, & ! input
            nsh, nsh_real, tstep_real) ! output

         ! calculate surface fraction related VARIABLES
         CALL SUEWS_cal_surf( &
            StorageHeatMethod, NetRadiationMethod, & !input
            nlayer, sfr_surf, & !input
            building_frac, building_scale, height, & !input
            VegFraction, ImpervFraction, PervFraction, NonWaterFraction, & ! output
            sfr_roof, sfr_wall) ! output

         ! calculate dayofweek information
         CALL SUEWS_cal_weekday( &
            iy, id, lat, & !input
            dayofWeek_id) !output

         ! calculate dayofweek information
         CALL SUEWS_cal_DLS( &
            id, startDLS, endDLS, & !input
            DLS) !output

         ! calculate mean air temperature of past 24 hours
         Tair_av_next = cal_tair_av(Tair_av_prev, dt_since_start, tstep, temp_c)

         !==============main calculation start=======================

         !==============surface roughness calculation=======================
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_RoughnessParameters...'
         IF (Diagnose == 1) PRINT *, 'z0m_in =', z0m_in
         CALL SUEWS_cal_RoughnessParameters( &
            RoughLenMomMethod, FAImethod, &
            sfr_surf, & !input
            surfacearea, & !input
            bldgH, EveTreeH, DecTreeH, &
            porosity_id_prev, FAIBldg, FAIEveTree, FAIDecTree, &
            z0m_in, zdm_in, Z, &
            FAI, PAI, & !output
            zH, z0m, zdm, ZZD)
         ! print *, 'day =', id, 'hour =', it, 'porosity_id = ', porosity_id_prev

         !=================Calculate sun position=================
         IF (Diagnose == 1) WRITE (*, *) 'Calling NARP_cal_SunPosition...'
         CALL NARP_cal_SunPosition( &
            REAL(iy, KIND(1D0)), & !input:
            dectime - tstep/2/86400, & ! sun position at middle of timestep before
            timezone, lat, lng, alt, &
            azimuth, zenith_deg) !output:

         !=================Call the SUEWS_cal_DailyState routine to get surface characteristics ready=================
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_DailyState...'
         CALL SUEWS_cal_DailyState( &
            iy, id, it, imin, isec, tstep, tstep_prev, dt_since_start, DayofWeek_id, & !input
            Tmin_id_prev, Tmax_id_prev, lenDay_id_prev, &
            BaseTMethod, &
            WaterUseMethod, Ie_start, Ie_end, &
            LAImethod, LAIType, &
            nsh_real, kdown, Temp_C, Precip, BaseT_HC, &
            BaseT_Heating, BaseT_Cooling, &
            lat, Faut, LAI_obs, &
            AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
            AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
            CapMax_dec, CapMin_dec, PorMax_dec, PorMin_dec, &
            Ie_a, Ie_m, DayWatPer, DayWat, &
            BaseT, BaseTe, GDDFull, SDDFull, LAIMin, LAIMax, LAIPower, &
            DecidCap_id_prev, StoreCap_surf_prev, LAI_id_prev, GDD_id_prev, SDD_id_prev, &
            albDecTr_id_prev, albEveTr_id_prev, albGrass_id_prev, porosity_id_prev, & !input
            HDD_id_prev, & !input
            state_surf_prev, soilstore_surf_prev, SoilStoreCap_surf, H_maintain, & !input
            HDD_id_next, & !output
            Tmin_id_next, Tmax_id_next, lenDay_id_next, &
            albDecTr_id_next, albEveTr_id_next, albGrass_id_next, porosity_id_next, & !output
            DecidCap_id_next, StoreCap_surf_next, LAI_id_next, GDD_id_next, SDD_id_next, WUDay_id_next) !output

         !=================Calculation of density and other water related parameters=================
         IF (Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_AtmMoist...'
         CALL cal_AtmMoist( &
            Temp_C, Press_hPa, avRh, dectime, & ! input:
            lv_J_kg, lvS_J_kg, & ! output:
            es_hPa, Ea_hPa, VPd_hpa, VPD_Pa, dq, dens_dry, avcp, avdens)

         !======== Calculate soil moisture =========
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_update_SoilMoist...'
         CALL SUEWS_update_SoilMoist( &
            NonWaterFraction, & !input
            SoilStoreCap_surf, sfr_surf, soilstore_surf_prev, &
            SoilMoistCap, SoilState, & !output
            vsmd, smd)

         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_WaterUse...'
         !=================Gives the external and internal water uses per timestep=================
         CALL SUEWS_cal_WaterUse( &
            nsh_real, & ! input:
            wu_m3, SurfaceArea, sfr_surf, &
            IrrFracPaved, IrrFracBldgs, &
            IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
            IrrFracBSoil, IrrFracWater, &
            DayofWeek_id, WUProfA_24hr, WUProfM_24hr, &
            InternalWaterUse_h, HDD_id_next, WUDay_id_next, &
            WaterUseMethod, NSH, it, imin, DLS, &
            wu_surf, wu_int, wu_ext) ! output:

         ! ===================ANTHROPOGENIC HEAT AND CO2 FLUX======================
         CALL SUEWS_cal_AnthropogenicEmission( &
            AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, AH_SLOPE_Heating, CO2PointSource, & ! input:
            dayofWeek_id, DLS, EF_umolCO2perJ, EmissionsMethod, EnEF_v_Jkm, &
            FcEF_v_kgkm, FrFossilFuel_Heat, FrFossilFuel_NonHeat, HDD_id_next, HumActivity_24hr, &
            imin, it, MaxFCMetab, MaxQFMetab, MinFCMetab, MinQFMetab, &
            PopDensDaytime, PopDensNighttime, PopProf_24hr, QF, QF0_BEU, Qf_A, Qf_B, Qf_C, &
            QF_obs, QF_SAHP, SurfaceArea, BaseT_Cooling, BaseT_Heating, &
            Temp_C, TrafficRate, TrafficUnits, TraffProf_24hr, &
            Fc_anthro, Fc_build, Fc_metab, Fc_point, Fc_traff) ! output:

         ! ========================================================================
         ! N.B.: the following parts involves snow-related calculations.
         ! ===================NET ALLWAVE RADIATION================================
         ! if (kdown>0 .and. i_iter == 1) then
         !    print *, 'snowFrac_prev=', snowFrac_prev
         !    snowFrac_prev=-999
         !    print *, 'snowFrac_prev=', snowFrac_prev
         ! endif
         CALL SUEWS_cal_Qn( &
            StorageHeatMethod, NetRadiationMethod, SnowMethod, & !input
            tstep, nlayer, SnowPack_prev, tau_a, tau_f, SnowAlbMax, SnowAlbMin, &
            Diagnose, ldown_obs, fcld_obs, &
            dectime, ZENITH_deg, Ts_iter, kdown, Temp_C, avRH, ea_hPa, qn1_obs, &
            SnowAlb_prev, snowFrac_prev, DiagQN, &
            NARP_TRANS_SITE, NARP_EMIS_SNOW, IceFrac_prev, &
            sfr_surf, sfr_roof, sfr_wall, &
            tsfc_out_surf, tsfc_out_roof, tsfc_out_wall, &
            emis, alb_prev, albDecTr_id_next, albEveTr_id_next, albGrass_id_next, &
            LAI_id, & !input
            n_vegetation_region_urban, &
            n_stream_sw_urban, n_stream_lw_urban, &
            sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
            veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
            veg_fsd_const, veg_contact_fraction_const, &
            ground_albedo_dir_mult_fact, use_sw_direct_albedo, & !input
            height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
            alb_roof, emis_roof, alb_wall, emis_wall, &
            roof_albedo_dir_mult_fact, wall_specular_frac, &
            alb_next, ldown, fcld, & !output
            QN_surf, QN_roof, QN_wall, &
            qn, qn_snowfree, qn_snow, kclear, kup, lup, tsurf, &
            qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, &
            albedo_snow, SnowAlb_next, &
            dataOutLineSPARTACUS)

         ! IF (qn < -300) THEN
         !    PRINT *, 'qn=', qn
         !    PRINT *, 'snowFrac_prev=', snowFrac_prev
         ! END IF

         ! PRINT *, 'Qn_surf after SUEWS_cal_Qn ', qn_surf
         ! PRINT *, 'qn_roof after SUEWS_cal_Qn ', qn_roof
         ! PRINT *, 'qn_wall after SUEWS_cal_Qn ', qn_wall
         ! PRINT *, ''

         ! =================STORAGE HEAT FLUX=======================================
         IF (i_iter == 1) THEN
            Qg_surf = 0.1*QN_surf
            Qg_roof = 0.1*QN_roof
            Qg_wall = 0.1*QN_wall
         ELSE
            Qg_surf = QN_surf + QF - (QH_surf + QE_surf)
            Qg_roof = QN_roof + QF - (QH_roof + QE_roof)
            Qg_wall = QN_wall + QF - (QH_wall + QE_wall)
         END IF

         ! PRINT *, 'Qg_surf before cal_qs', Qg_surf
         ! PRINT *, 'Qg_roof before cal_qs', Qg_roof
         ! PRINT *, 'Qg_wall before cal_qs', Qg_wall
         ! print *,''

         ! PRINT *, 'tsfc_surf before cal_qs', tsfc_out_surf
         ! PRINT *, 'tsfc_out_roof before cal_qs', tsfc_out_roof
         ! PRINT *, 'tsfc_wall before cal_qs', tsfc_out_wall
         ! PRINT *, ''

         CALL SUEWS_cal_Qs( &
            StorageHeatMethod, qs_obs, OHMIncQF, Gridiv, & !input
            id, tstep, dt_since_start, Diagnose, &
            nlayer, &
            Qg_surf, Qg_roof, Qg_wall, &
            tsfc_out_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
            tsfc_out_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
            tsfc_out_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
            OHM_coef, OHM_threshSW, OHM_threshWD, &
            soilstore_surf_prev, SoilStoreCap_surf, state_surf_prev, SnowMethod, SnowFrac_prev, DiagQS, &
            HDD_id, MetForcingData_grid, Ts5mindata_ir, qf, qn, &
            kdown, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown, &
            bldgh, alb, emis, cpAnOHM, kkAnOHM, chAnOHM, EmissionsMethod, &
            Tair_av, qn_av_prev, dqndt_prev, qn_s_av_prev, dqnsdt_prev, &
            StoreDrainPrm, &
            qn_snow, dataOutLineESTM, qs, & !output
            qn_av_next, dqndt_next, qn_s_av_next, dqnsdt_next, &
            deltaQi, a1, a2, a3, &
            temp_out_roof, QS_roof, & !output
            temp_out_wall, QS_wall, & !output
            temp_out_surf, QS_surf) !output

         ! update iteration variables
         ! temp_in_roof = temp_out_roof
         ! temp_in_wall = temp_out_wall
         ! temp_in_surf = temp_out_surf
         ! Ts_iter = DOT_PRODUCT(tsfc_out_surf, sfr_surf)
         ! PRINT *, 'QS_surf after cal_qs', QS_surf
         ! PRINT *, 'QS_roof after cal_qs', QS_roof
         ! PRINT *, 'QS_wall after cal_qs', QS_wall

         ! PRINT *, ''

         ! PRINT *, 'tsfc_surf after cal_qs', tsfc_out_surf
         ! PRINT *, 'tsfc_roof after cal_qs', tsfc_out_roof
         ! PRINT *, 'tsfc_wall after cal_qs', tsfc_out_wall
         ! PRINT *, ''
         ! print *,'tsfc_surf abs. diff.:',maxval(abs(tsfc_out_surf-tsfc0_out_surf)),maxloc(abs(tsfc_out_surf-tsfc0_out_surf))
         ! dif_tsfc_iter=maxval(abs(tsfc_out_surf-tsfc0_out_surf))
         ! print *,'tsfc_roof abs. diff.:',maxval(abs(tsfc_out_roof-tsfc0_out_roof)),maxloc(abs(tsfc_out_roof-tsfc0_out_roof))
         ! dif_tsfc_iter=max(maxval(abs(tsfc_out_roof-tsfc0_out_roof)),dif_tsfc_iter)
         ! print *,'tsfc_wall abs. diff.:',maxval(abs(tsfc_out_wall-tsfc0_out_wall)),maxloc(abs(tsfc_out_wall-tsfc0_out_wall))
         ! dif_tsfc_iter=max(maxval(abs(tsfc0_out_wall-tsfc_out_wall)),dif_tsfc_iter)

         ! tsfc0_out_surf = tsfc_out_surf
         ! tsfc0_out_roof = tsfc_out_roof
         ! tsfc0_out_wall = tsfc_out_wall

         !==================Energy related to snow melting/freezing processes=======
         IF (Diagnose == 1) WRITE (*, *) 'Calling MeltHeat'

         !==========================Turbulent Fluxes================================
         IF (Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_QHQE...'
         IF (i_iter == 1) THEN
            !Calculate QH and QE from LUMPS in the first iteration of each time step
            CALL LUMPS_cal_QHQE( &
               veg_type, & !input
               SnowMethod, qn, qf, qs, Temp_C, VegFraction, avcp, Press_hPa, lv_J_kg, &
               tstep_real, DRAINRT, nsh_real, &
               Precip, RainMaxRes, RAINCOVER, sfr_surf, LAI_id_next, LAImax, LAImin, &
               QH_LUMPS, & !output
               QE_LUMPS, psyc_hPa, s_hPa, sIce_hpa, TempVeg, VegPhenLumps)

            ! use LUMPS QH to do stability correction
            QH_Init = QH_LUMPS
         ELSE
            ! use SUEWS QH to do stability correction
            QH_Init = QH
         END IF

         !============= calculate water balance =============
         CALL SUEWS_cal_Water( &
            Diagnose, & !input
            SnowMethod, NonWaterFraction, addPipes, addImpervious, addVeg, addWaterBody, &
            state_surf_prev, sfr_surf, &
            StoreCap_surf_next, DrainEq_surf, DrainCoef1_surf, DrainCoef2_surf, &
            WaterDist, nsh_real, &
            drain_per_tstep, & !output
            drain_surf, frac_waterdist_vert, &
            AdditionalWater, runoffPipes, runoff_per_interval, &
            AddWater)
         !============= calculate water balance end =============

         !===============Resistance Calculations=======================
         CALL SUEWS_cal_Resistance( &
            StabilityMethod, & !input:
            Diagnose, AerodynamicResistanceMethod, RoughLenHeatMethod, SnowMethod, &
            id, it, gsModel, SMDMethod, &
            avdens, avcp, QH_Init, zzd, z0m, zdm, &
            avU1, Temp_C, VegFraction, kdown, &
            Kmax, &
            g_max, g_k, g_q_base, g_q_shape, &
            g_t, g_sm, s1, s2, &
            th, tl, &
            dq, xsmd, vsmd, MaxConductance, LAIMax, LAI_id_next, SnowFrac_prev, sfr_surf, &
            g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
            UStar, TStar, L_mod, & !output
            zL, gsc, RS, RA_h, RAsnow, RB, z0v, z0vSnow)

         !===================Resistance Calculations End=======================

         !===================Calculate surface hydrology and related soil water=======================
         IF (SnowMethod == 1) THEN

            ! ===================Calculate snow related hydrology=======================
            CALL SUEWS_cal_snow( &
               Diagnose, nlayer, & !input
               tstep, imin, it, EvapMethod, dayofWeek_id, CRWmin, CRWmax, &
               dectime, avdens, avcp, lv_J_kg, lvS_J_kg, avRh, Press_hPa, Temp_C, &
               RAsnow, psyc_hPa, sIce_hPa, tau_r, &
               RadMeltFact, TempMeltFact, SnowAlbMax, PrecipLimit, PrecipLimitAlb, &
               qn_ind_snow, kup_ind_snow, deltaQi, Tsurf_ind_snow, &
               SnowAlb_next, &
               PervFraction, vegfraction, addimpervious, qn_snowfree, qf, qs, vpd_hPa, s_hPa, &
               RS, RA_h, RB, SnowDensMax, snowdensmin, precip, PipeCapacity, RunoffToWater, &
               addVeg, SnowLimPaved, SnowLimBldg, &
               FlowChange, drain_surf, WetThresh_surf, SoilStoreCap_surf, &
               Tsurf_ind, sfr_surf, &
               AddWater, frac_waterdist_vert, &
               StoreCap_surf_next, StoreCapMax_surf, &
               SnowPackLimit, SnowProf_24hr, &
               SnowPack_prev, snowFrac_prev, SnowWater_prev, IceFrac_prev, SnowDens_prev, & ! input:
               SnowfallCum_prev, state_surf_prev, soilstore_surf_prev, & ! input:
               QN_surf, qs_surf, &
               SnowRemoval, & ! snow specific output
               SnowPack_next, SnowFrac_next, SnowWater_next, iceFrac_next, SnowDens_next, & ! output
               SnowfallCum_next, state_surf_next, soilstore_surf_next, & ! general output:
               state_per_tstep, NWstate_per_tstep, &
               qe, qe_surf, qe_roof, qe_wall, &
               SnowAlb_next, &
               swe, chSnow_per_interval, ev_per_tstep, runoff_per_tstep, &
               surf_chang_per_tstep, runoffPipes, mwstore, runoffwaterbody, &
               runoffAGveg, runoffAGimpervious, rss_surf, &
               dataOutLineSnow)
            ! N.B.: snow-related calculations end here.
            !===================================================
         ELSE
            !======== Evaporation and surface state_id for snow-free conditions ========
            CALL SUEWS_cal_QE( &
               Diagnose, storageheatmethod, nlayer, & !input
               tstep, &
               EvapMethod, &
               avdens, avcp, lv_J_kg, &
               psyc_hPa, &
               PervFraction, &
               addimpervious, &
               qf, vpd_hPa, s_hPa, RS, RA_h, RB, &
               precip, PipeCapacity, RunoffToWater, &
               NonWaterFraction, wu_surf, addVeg, addWaterBody, AddWater, &
               FlowChange, drain_surf, &
               frac_waterdist_vert, StoreCap_surf_next, &
               sfr_surf, StateLimit_surf, SoilStoreCap_surf, WetThresh_surf, & ! input:
               state_surf_prev, soilstore_surf_prev, QN_surf, qs_surf, & ! input:
               sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
               state_roof_prev, soilstore_roof_prev, QN_roof, qs_roof, & ! input:
               sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
               state_wall_prev, soilstore_wall_prev, QN_wall, qs_wall, & ! input:
               state_surf_next, soilstore_surf_next, ev_surf, & ! general output:
               state_roof_next, soilstore_roof_next, ev_roof, & ! general output:
               state_wall_next, soilstore_wall_next, ev_wall, & ! general output:
               state_per_tstep, NWstate_per_tstep, &
               ev0_surf, qe0_surf, &
               qe, qe_surf, qe_roof, qe_wall, &
               ev_per_tstep, runoff_per_tstep, &
               surf_chang_per_tstep, runoffPipes, &
               runoffwaterbody, &
               runoffAGveg, runoffAGimpervious, rss_surf)
            !======== Evaporation and surface state_id end========
         END IF
         IF (Diagnose == 1) PRINT *, 'before SUEWS_cal_SoilState soilstore_id = ', soilstore_surf_next

         !=== Horizontal movement between soil stores ===
         ! Now water is allowed to move horizontally between the soil stores
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_HorizontalSoilWater...'
         CALL SUEWS_cal_HorizontalSoilWater( &
            sfr_surf, & ! input: ! surface fractions
            SoilStoreCap_surf, & !Capacity of soil store for each surface [mm]
            SoilDepth, & !Depth of sub-surface soil store for each surface [mm]
            SatHydraulicConduct, & !Saturated hydraulic conductivity for each soil subsurface [mm s-1]
            SurfaceArea, & !Surface area of the study area [m2]
            NonWaterFraction, & ! sum of surface cover fractions for all except water surfaces
            tstep_real, & !tstep cast as a real for use in calculations
            soilstore_surf_next, & ! inout:!Soil moisture of each surface type [mm]
            runoffSoil, & !Soil runoff from each soil sub-surface [mm]
            runoffSoil_per_tstep & !  output:!Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
            )

         !========== Calculate soil moisture ============
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_SoilState...'
         CALL SUEWS_cal_SoilState( &
            SMDMethod, xsmd, NonWaterFraction, SoilMoistCap, & !input
            SoilStoreCap_surf, surf_chang_per_tstep, &
            soilstore_surf_next, soilstore_surf_prev, sfr_surf, &
            smd, smd_nsurf, tot_chang_per_tstep, SoilState) !output

         !============ Sensible heat flux ===============
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_QH...'
         CALL SUEWS_cal_QH( &
            1, nlayer, storageheatmethod, & !input
            qn, qf, QmRain, qe, qs, QmFreez, qm, avdens, avcp, &
            sfr_surf, sfr_roof, sfr_wall, &
            tsfc_out_surf, tsfc_out_roof, tsfc_out_wall, &
            Temp_C, &
            RA_h, &
            qh, qh_residual, qh_resist, & !output
            qh_resist_surf, qh_resist_roof, qh_resist_wall)
         ! PRINT *, 'qn_surf after SUEWS_cal_QH', qn_surf
         ! PRINT *, 'qs_surf after SUEWS_cal_QH', qs_surf
         ! PRINT *, 'qe_surf after SUEWS_cal_QH', qe_surf
         ! PRINT *, 'qh_surf after SUEWS_cal_QH (resist)', qh_surf
         ! PRINT *, 'qh_roof after SUEWS_cal_QH (resist)', qh_roof
         ! PRINT *, 'qh_wall after SUEWS_cal_QH (resist)', qh_wall
         ! PRINT *, ''

         ! PRINT *, 'tsfc_surf after SUEWS_cal_QH (resist)', tsfc_out_surf
         ! PRINT *, 'tsfc_roof after SUEWS_cal_QH (resist)', tsfc_out_roof
         ! PRINT *, 'tsfc_wall after SUEWS_cal_QH (resist)', tsfc_out_wall
         ! PRINT *, ''
         ! PRINT *, ' qh_residual: ', qh_residual, ' qh_resist: ', qh_resist
         ! PRINT *, ' dif_qh: ', ABS(qh_residual - qh_resist)
         !============ Sensible heat flux end ===============

         ! residual heat flux
         ! PRINT *, 'residual surf: ', qn_surf + qf - qs_surf - qe_surf - qh_surf
         ! PRINT *, 'residual roof: ', qn_roof + qf - qs_roof - qe_roof - qh_roof
         ! PRINT *, 'residual wall: ', qn_wall + qf - qs_wall - qe_wall - qh_wall

         !============ Sensible heat flux end===============

         !============ calculate surface temperature ===============
         TSfc_C = cal_tsfc(qh, avdens, avcp, RA_h, temp_c)

         !============= calculate surface specific QH and Tsfc ===============
         ! note: tsfc has an upper limit of temp_c+50 to avoid numerical errors
         tsfc0_out_surf = MIN(tsfc_out_surf, Temp_C + 50)
         tsfc0_out_roof = MIN(tsfc_out_roof, Temp_C + 50)
         tsfc0_out_wall = MIN(tsfc_out_wall, Temp_C + 50)

         QH_surf = QN_surf + qf - qs_surf - qe_surf
         QH_roof = QN_roof + qf - qs_roof - qe_roof
         QH_wall = QN_wall + qf - qs_wall - qe_wall

         IF (diagnose == 1) THEN
            PRINT *, 'qn_surf before QH back env.:', QN_surf
            PRINT *, 'qf before QH back env.:', qf
            PRINT *, 'qs_surf before QH back env.:', qs_surf
            PRINT *, 'qe_surf before QH back env.:', qe_surf
            PRINT *, 'qh_surf before QH back env.:', QH_surf

            PRINT *, 'qn_roof before QH back env.:', QN_roof
            PRINT *, 'qs_roof before QH back env.:', qs_roof
            PRINT *, 'qe_roof before QH back env.:', qe_roof
            PRINT *, 'qh_roof before QH back env.:', QH_roof

         END IF
         DO i_surf = 1, nsurf
            ! TSfc_QH_surf(i_surf) = cal_tsfc(qh_surf(i_surf), avdens, avcp, RA_h, temp_c)
            tsfc_out_surf(i_surf) = cal_tsfc(QH_surf(i_surf), avdens, avcp, RA_h, temp_c)
            ! if ( i_surf==1 ) then
            !    tsfc_out_surf(i_surf) = cal_tsfc(qh_surf(i_surf), avdens, avcp, RA_h, temp_c)
            ! else
            !    tsfc_out_surf(i_surf)=tsfc0_out_surf(i_surf)
            ! end if
            ! restrict calculated heat storage to a sensible range
            ! tsfc_out_surf(i_surf) = MAX(MIN(tsfc_out_surf(i_surf), 100.0), -100.0)
         END DO

         DO i_surf = 1, nlayer
            tsfc_out_roof(i_surf) = cal_tsfc(QH_roof(i_surf), avdens, avcp, RA_h, temp_c)
            tsfc_out_wall(i_surf) = cal_tsfc(QH_wall(i_surf), avdens, avcp, RA_h, temp_c)
         END DO

         IF (diagnose == 1) PRINT *, 'tsfc_surf after QH back env.:', tsfc_out_surf
         ! print *,'tsfc_roof after QH back env.:',tsfc_out_roof
         IF (diagnose == 1) PRINT *, &
            'tsfc_surf abs. diff.:', MAXVAL(ABS(tsfc_out_surf - tsfc0_out_surf)), MAXLOC(ABS(tsfc_out_surf - tsfc0_out_surf))
         dif_tsfc_iter = MAXVAL(ABS(tsfc_out_surf - tsfc0_out_surf))
         IF (StorageHeatMethod == 5) THEN
            IF (diagnose == 1) PRINT *, &
               'tsfc_roof abs. diff.:', MAXVAL(ABS(tsfc_out_roof - tsfc0_out_roof)), MAXLOC(ABS(tsfc_out_roof - tsfc0_out_roof))
            dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc_out_roof - tsfc0_out_roof)), dif_tsfc_iter)
            IF (diagnose == 1) PRINT *, &
               'tsfc_wall abs. diff.:', MAXVAL(ABS(tsfc_out_wall - tsfc0_out_wall)), MAXLOC(ABS(tsfc_out_wall - tsfc0_out_wall))
            dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc0_out_wall - tsfc_out_wall)), dif_tsfc_iter)
         END IF

         ! ====test===
         ! see if this converges better
         ratio_iter = .4
         ! ratio_iter = .3
         tsfc_out_surf = (tsfc0_out_surf*(1 - ratio_iter) + tsfc_out_surf*ratio_iter)
         tsfc_out_roof = (tsfc0_out_roof*(1 - ratio_iter) + tsfc_out_roof*ratio_iter)
         tsfc_out_wall = (tsfc0_out_wall*(1 - ratio_iter) + tsfc_out_wall*ratio_iter)
         ! =======test end=======

         ! PRINT *, 'tsfc_surf after qh_cal', TSfc_QH_surf

         !============ surface-level diagonostics end ===============

         ! force quit do-while, i.e., skip iteration and use NARP for Tsurf calculation
         ! if (NetRadiationMethod < 10 .or. NetRadiationMethod > 100) exit

         ! Test if sensible heat fluxes converge in iterations
         ! if (abs(QH - QH_Init) > 0.1) then
         ! IF (ABS(Ts_iter - TSfc_C) > 0.1) THEN
         !    flag_converge = .FALSE.
         ! ELSE
         !    flag_converge = .TRUE.
         !    PRINT *, 'Iteration done in', i_iter, ' iterations'
         !    PRINT *, ' Ts_iter: ', Ts_iter, ' TSfc_C: ', TSfc_C
         ! END IF
         ! IF (MINVAL(ABS(TSfc_QH_surf - tsfc_surf)) > 0.1) THEN
         ! IF (ABS(qh_residual - qh_resist) > .2) THEN
         IF (dif_tsfc_iter > .1) THEN
            flag_converge = .FALSE.
         ELSE
            flag_converge = .TRUE.
            ! PRINT *, 'Iteration done in', i_iter, ' iterations'
            ! PRINT *, ' qh_residual: ', qh_residual, ' qh_resist: ', qh_resist
            ! PRINT *, ' dif_qh: ', ABS(qh_residual - qh_resist)
            ! PRINT *, ' abs. dif_tsfc: ', dif_tsfc_iter

         END IF

         i_iter = i_iter + 1
         ! force quit do-while loop if not convergent after 100 iterations
         IF (Diagnose == 1 .AND. i_iter == max_iter) THEN
            ! PRINT *, 'Iteration did not converge in', i_iter, ' iterations'
            ! PRINT *, ' qh_residual: ', qh_residual, ' qh_resist: ', qh_resist
            ! PRINT *, ' dif_qh: ', ABS(qh_residual - qh_resist)
            ! PRINT *, ' Ts_iter: ', Ts_iter, ' TSfc_C: ', TSfc_C
            ! PRINT *, ' abs. dif_tsfc: ', dif_tsfc_iter
            ! exit
         END IF

         ! Ts_iter = TSfc_C
         ! l_mod_iter = l_mod
         ! PRINT *, '========================='
         ! PRINT *, ''
         !==============main calculation end=======================
      END DO ! end iteration for tsurf calculations

      !==============================================================
      ! Calculate diagnostics: these variables are decoupled from the main SUEWS calculation

      !============ roughness sub-layer diagonostics ===============
      IF (Diagnose == 1) WRITE (*, *) 'Calling RSLProfile...'
      CALL RSLProfile( &
         DiagMethod, &
         zH, z0m, zdm, z0v, &
         L_MOD, sfr_surf, FAI, PAI, &
         StabilityMethod, RA_h, &
         avcp, lv_J_kg, avdens, &
         avU1, Temp_C, avRH, Press_hPa, z, qh, qe, & ! input
         T2_C, q2_gkg, U10_ms, RH2, & !output
         dataoutLineRSL) ! output

      ! ============ BIOGENIC CO2 FLUX =======================
      CALL SUEWS_cal_BiogenCO2( &
         alpha_bioCO2, alpha_enh_bioCO2, kdown, avRh, beta_bioCO2, beta_enh_bioCO2, & ! input:
         dectime, Diagnose, EmissionsMethod, Fc_anthro, g_max, g_k, g_q_base, g_q_shape, &
         g_t, g_sm, gfunc, gsmodel, id, it, Kmax, LAI_id_next, LAIMin, &
         LAIMax, MaxConductance, min_res_bioCO2, Press_hPa, resp_a, &
         resp_b, S1, S2, sfr_surf, SMDMethod, SnowFrac, t2_C, Temp_C, theta_bioCO2, TH, TL, vsmd, xsmd, &
         Fc, Fc_biogen, Fc_photo, Fc_respi) ! output:

      ! calculations of diagnostics end
      !==============================================================

      !==============================================================
      ! update inout variables with new values
      qn_av = qn_av_next
      dqndt = dqndt_next
      qn_s_av = qn_s_av_next
      dqnsdt = dqnsdt_next
      SnowfallCum = SnowfallCum_next
      SnowAlb = SnowAlb_next
      IceFrac = IceFrac_next
      SnowWater = SnowWater_next
      SnowDens = SnowDens_next
      SnowFrac = SnowFrac_next
      SnowPack = SnowPack_next

      soilstore_surf = soilstore_surf_next
      state_surf = state_surf_next
      alb = alb_next
      GDD_id = GDD_id_next
      SDD_id = SDD_id_next
      LAI_id = LAI_id_next
      DecidCap_id = DecidCap_id_next
      albDecTr_id = albDecTr_id_next
      albEveTr_id = albEveTr_id_next
      albGrass_id = albGrass_id_next
      porosity_id = porosity_id_next
      ! StoreDrainPrm = StoreDrainPrm_next
      Tair_av = Tair_av_next
      Tmin_id = Tmin_id_next
      Tmax_id = Tmax_id_next
      lenday_id = lenday_id_next
      HDD_id = HDD_id_next
      WUDay_id = WUDay_id_next

      IF (StorageHeatMethod == 5) THEN
         ! ESTM_ehc related
         temp_roof = temp_out_roof
         temp_wall = temp_out_wall
         temp_surf = temp_out_surf
         tsfc_roof = tsfc_out_roof
         tsfc_wall = tsfc_out_wall
         tsfc_surf = tsfc_out_surf

         soilstore_roof = soilstore_roof_next
         state_roof = state_roof_next
         soilstore_wall = soilstore_wall_next
         state_wall = state_wall_next
      END IF

      !==============use SOLWEIG to get localised radiation flux==================
      ! if (sfr_surf(BldgSurf) > 0) then
      !    CALL SOLWEIG_cal_main(id, it, dectime, 0.8d0, FAI, avkdn, ldown, Temp_C, avRh, Press_hPa, TSfc_C, &
      !    lat, ZENITH_deg, azimuth, 1.d0, alb(1), alb(2), emis(1), emis(2), bldgH, dataOutLineSOLWEIG)
      ! else
      !    dataOutLineSOLWEIG = set_nan(dataOutLineSOLWEIG)
      ! endif

      !==============use BEERS to get localised radiation flux==================
      ! TS 14 Jan 2021: BEERS is a modified version of SOLWEIG
      IF (sfr_surf(BldgSurf) > 0) THEN
         PAI = sfr_surf(2)/SUM(sfr_surf(1:2))
         CALL BEERS_cal_main(iy, id, dectime, PAI, FAI, kdown, ldown, Temp_C, avrh, &
                             Press_hPa, TSfc_C, lat, lng, alt, timezone, zenith_deg, azimuth, &
                             alb(1), alb(2), emis(1), emis(2), &
                             dataOutLineBEERS) ! output
         ! CALL SOLWEIG_cal_main(id, it, dectime, 0.8d0, FAI, avkdn, ldown, Temp_C, avRh, Press_hPa, TSfc_C, &
         ! lat, ZENITH_deg, azimuth, 1.d0, alb(1), alb(2), emis(1), emis(2), bldgH, dataOutLineSOLWEIG)
      ELSE
         dataOutLineBEERS = set_nan(dataOutLineBEERS)
      END IF

      !==============translation of  output variables into output array===========
      CALL SUEWS_update_outputLine( &
         AdditionalWater, alb, kdown, U10_ms, azimuth, & !input
         chSnow_per_interval, dectime, &
         drain_per_tstep, QE_LUMPS, ev_per_tstep, wu_ext, Fc, Fc_build, fcld, &
         Fc_metab, Fc_photo, Fc_respi, Fc_point, Fc_traff, FlowChange, &
         QH_LUMPS, id, imin, wu_int, it, iy, &
         kup, LAI_id, ldown, l_mod, lup, mwh, &
         MwStore, &
         nsh_real, NWstate_per_tstep, Precip, q2_gkg, &
         qe, qf, qh, qh_resist, Qm, QmFreez, &
         QmRain, qn, qn_snow, qn_snowfree, qs, RA_h, &
         RS, RH2, runoffAGimpervious, runoffAGveg, &
         runoff_per_tstep, runoffPipes, runoffSoil_per_tstep, &
         runoffWaterBody, sfr_surf, smd, smd_nsurf, SnowAlb, SnowRemoval, &
         state_surf_next, state_per_tstep, surf_chang_per_tstep, swe, t2_C, TSfc_C, &
         tot_chang_per_tstep, tsurf, UStar, &
         wu_surf, &
         z0m, zdm, zenith_deg, &
         datetimeLine, dataOutLineSUEWS) !output

      CALL ESTMExt_update_outputLine( &
         iy, id, it, imin, dectime, nlayer, & !input
         tsfc_out_surf, qs_surf, &
         tsfc_out_roof, &
         QN_roof, &
         QS_roof, &
         QE_roof, &
         QH_roof, &
         state_roof, &
         soilstore_roof, &
         tsfc_out_wall, &
         QN_wall, &
         QS_wall, &
         QE_wall, &
         QH_wall, &
         state_wall, &
         soilstore_wall, &
         datetimeLine, dataOutLineESTMExt) !output

      ! daily state_id:
      CALL update_DailyStateLine( &
         it, imin, nsh_real, & !input
         GDD_id, HDD_id, LAI_id, &
         SDD_id, &
         Tmin_id, Tmax_id, lenday_id, &
         DecidCap_id, &
         albDecTr_id, &
         albEveTr_id, &
         albGrass_id, &
         porosity_id, &
         WUDay_id, &
         VegPhenLumps, &
         SnowAlb, SnowDens, &
         a1, a2, a3, &
         dataOutLineDailyState) !out

      !==============translation end ================

      dataoutlineDebug = &
         [tsfc0_out_surf, &
          qn_surf, qs_surf, qe0_surf, qe_surf, qh_surf, & ! energy balance
          wu_surf, ev0_surf, ev_surf, drain_surf, state_surf_prev, state_surf_next, soilstore_surf_prev, soilstore_surf_next, & ! water balance
          RS, RA_h, RB, RAsnow, rss_surf, & ! for debugging QE
          vsmd, S1/G_sm + S2, G_sm, G_sm*(vsmd - S1/G_sm + S2), & ! debug g_smd
          g_kdown, g_dq, g_ta, g_smd, g_lai, & ! for debugging RS: surface resistance
          vpd_hPa, lv_J_kg, avdens, avcp, s_hPa, psyc_hPa, & ! for debugging QE
          i_iter*1D0, &
          FAIBldg_use, FAIEveTree_use, FAIDecTree_use, FAI, &
          dqndt]

      !==============output==========================
      CALL output_line_init(output_line_all)
      output_line_all%datetime = datetimeLine
      output_line_all%SUEWS = [datetimeLine, dataOutLineSUEWS]
      output_line_all%ESTMExt = [datetimeLine, dataOutLineESTMExt]
      output_line_all%DailyState = [datetimeLine, dataOutLineDailyState]
      output_line_all%BEERS = [datetimeLine, dataOutLineBEERS]
      output_line_all%Debug = [datetimeLine, dataOutLineDebug]
      output_line_all%SPARTACUS = [datetimeLine, dataOutLineSPARTACUS]
      output_line_all%Snow = [datetimeLine, dataOutLineSnow]
      output_line_all%RSL = [datetimeLine, dataOutLineRSL]
      output_line_all%ESTM = [datetimeLine, dataOutLineESTM]

   END SUBROUTINE SUEWS_cal_Main

   SUBROUTINE SUEWS_cal_Main_DTS( &
      AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, AH_SLOPE_Heating, & ! input&inout in alphabetical order
      alb, AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
      AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
      alpha_bioCO2, alpha_enh_bioCO2, alt, kdown, avRh, avU1, BaseT, BaseTe, &
      beta_bioCO2, beta_enh_bioCO2, bldgH, CapMax_dec, CapMin_dec, &
      chAnOHM, CO2PointSource, cpAnOHM, CRWmax, CRWmin, DayWat, DayWatPer, &
      DecTreeH, DiagMethod, Diagnose, DRAINRT, &
      dt_since_start, dqndt, qn_av, dqnsdt, qn_s_av, &
      EF_umolCO2perJ, emis, EmissionsMethod, EnEF_v_Jkm, endDLS, EveTreeH, FAIBldg, &
      FAIDecTree, FAIEveTree, FAIMethod, Faut, FcEF_v_kgkm, fcld_obs, FlowChange, &
      FrFossilFuel_Heat, FrFossilFuel_NonHeat, g_max, g_k, g_q_base, g_q_shape, g_t, g_sm, GDD_id, &
      GDDFull, Gridiv, gsModel, H_maintain, HDD_id, HumActivity_24hr, &
      IceFrac, id, Ie_a, Ie_end, Ie_m, Ie_start, imin, &
      InternalWaterUse_h, &
      IrrFracPaved, IrrFracBldgs, &
      IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
      IrrFracBSoil, IrrFracWater, &
      isec, it, &
      iy, kkAnOHM, Kmax, LAI_id, LAIMax, LAIMin, LAI_obs, &
      LAIPower, LAIType, lat, lenDay_id, ldown_obs, lng, MaxConductance, MaxFCMetab, MaxQFMetab, &
      SnowWater, MinFCMetab, MinQFMetab, min_res_bioCO2, &
      NARP_EMIS_SNOW, NARP_TRANS_SITE, NetRadiationMethod, &
      nlayer, &
      n_vegetation_region_urban, &
      n_stream_sw_urban, n_stream_lw_urban, &
      sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      veg_fsd_const, veg_contact_fraction_const, &
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, & !input
      height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
      alb_roof, emis_roof, alb_wall, emis_wall, &
      roof_albedo_dir_mult_fact, wall_specular_frac, &
      OHM_coef, OHMIncQF, OHM_threshSW, &
      OHM_threshWD, PipeCapacity, PopDensDaytime, &
      PopDensNighttime, PopProf_24hr, PorMax_dec, PorMin_dec, &
      Precip, PrecipLimit, PrecipLimitAlb, Press_hPa, &
      QF0_BEU, Qf_A, Qf_B, Qf_C, &
      qn1_obs, qs_obs, qf_obs, &
      RadMeltFact, RAINCOVER, RainMaxRes, resp_a, resp_b, &
      RoughLenHeatMethod, RoughLenMomMethod, RunoffToWater, S1, S2, &
      SatHydraulicConduct, SDDFull, SDD_id, SMDMethod, SnowAlb, SnowAlbMax, &
      SnowAlbMin, SnowPackLimit, SnowDens, SnowDensMax, SnowDensMin, SnowfallCum, SnowFrac, &
      SnowLimBldg, SnowLimPaved, snowFrac_obs, SnowPack, SnowProf_24hr, SnowUse, SoilDepth, &
      StabilityMethod, startDLS, &
      soilstore_surf, SoilStoreCap_surf, state_surf, StateLimit_surf, WetThresh_surf, &
      soilstore_roof, SoilStoreCap_roof, state_roof, StateLimit_roof, WetThresh_roof, &
      soilstore_wall, SoilStoreCap_wall, state_wall, StateLimit_wall, WetThresh_wall, &
      StorageHeatMethod, StoreDrainPrm, SurfaceArea, Tair_av, tau_a, tau_f, tau_r, &
      Tmax_id, Tmin_id, &
      BaseT_Cooling, BaseT_Heating, Temp_C, TempMeltFact, TH, &
      theta_bioCO2, timezone, TL, TrafficRate, TrafficUnits, &
      sfr_surf, &
      tsfc_roof, tsfc_wall, tsfc_surf, &
      temp_roof, temp_wall, temp_surf, &
      tin_roof, tin_wall, tin_surf, &
      k_roof, k_wall, k_surf, &
      cp_roof, cp_wall, cp_surf, &
      dz_roof, dz_wall, dz_surf, &
      TraffProf_24hr, Ts5mindata_ir, tstep, tstep_prev, veg_type, &
      WaterDist, WaterUseMethod, wu_m3, &
      WUDay_id, DecidCap_id, albDecTr_id, albEveTr_id, albGrass_id, porosity_id, &
      WUProfA_24hr, WUProfM_24hr, xsmd, Z, z0m_in, zdm_in, &
      output_line_suews) ! output

      ! nlayer, &
      ! Ts5mindata_ir,  ! for coupling with external models
      ! output_line_suews

      ! ---output related variables
      ! datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, & !output
      ! dataOutLineBEERS, & !output
      ! dataOutLineDebug, dataOutLineSPARTACUS, &
      ! dataOutLineESTMExt, &
      ! dataOutLineDailyState) !output

      IMPLICIT NONE

      ! input variables
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: Ts5mindata_ir !surface temperature input data[degC]
      REAL(KIND(1D0)), DIMENSION(10) :: MetForcingData_grid ! met forcing array of grid

      INTEGER, INTENT(IN) :: nlayer ! number of vertical layers in urban canyon [-]

      ! ---siteInfo-related variables
      TYPE(SITE_PRM) :: siteInfo
      REAL(KIND(1D0)), INTENT(IN) :: lat !latitude [deg]
      REAL(KIND(1D0)), INTENT(IN) :: lng !longitude [deg]
      REAL(KIND(1D0)), INTENT(IN) :: alt !solar altitude [deg]
      INTEGER, INTENT(IN) :: Gridiv ! grid id [-]
      REAL(KIND(1D0)), INTENT(IN) :: timezone !time zone, for site relative to UTC (east is positive) [h]
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea !area of the grid [ha]
      REAL(KIND(1D0)), INTENT(IN) :: Z ! measurement height [m]
      REAL(KIND(1D0)), INTENT(IN) :: z0m_in !roughness length for momentum [m]
      REAL(KIND(1D0)), INTENT(IN) :: zdm_in !zero-plane displacement [m]
      REAL(KIND(1D0)), INTENT(IN) :: PipeCapacity !capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)), INTENT(IN) :: RunoffToWater !fraction of above-ground runoff flowing to water surface during flooding [-]
      REAL(KIND(1D0)), INTENT(IN) :: NARP_TRANS_SITE !atmospheric transmissivity for NARP [-]
      REAL(KIND(1D0)), INTENT(IN) :: CO2PointSource ! point source [kgC day-1]
      REAL(KIND(1D0)), INTENT(IN) :: FlowChange !Difference between the input and output flow in the water body [mm]

      ! ---forcing-related variables
      TYPE(SUEWS_FORCING) :: forcing
      REAL(KIND(1D0)), INTENT(IN) :: kdown !incominging shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: ldown_obs !observed incoming longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: avRh !relative humidity [-]
      REAL(KIND(1D0)), INTENT(IN) :: Press_hPa !air pressure [hPa]
      REAL(KIND(1D0)), INTENT(IN) :: avU1 !average wind speed at 1m [W m-1]
      REAL(KIND(1D0)), INTENT(IN) :: Precip !rain data [mm]
      REAL(KIND(1D0)), INTENT(IN) :: fcld_obs !observed could fraction [-]
      REAL(KIND(1D0)), INTENT(IN) :: LAI_obs !observed LAI [m2 m-2]
      REAL(KIND(1D0)), INTENT(IN) :: snowFrac_obs !observed snow fraction [-]
      REAL(KIND(1D0)), INTENT(IN) :: xsmd ! observed soil moisture; can be provided either as volumetric ([m3 m-3] when SMDMethod = 1) or gravimetric quantity ([kg kg-1] when SMDMethod = 2
      REAL(KIND(1D0)), INTENT(IN) :: qn1_obs !observed net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: qs_obs !observed heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: qf_obs !observed anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: Temp_C !air temperature [degC]
      REAL(KIND(1D0)), INTENT(IN) :: wu_m3
      ! ESTM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: Tair_av !average air temperature [degC]

      ! ---timer-related variables
      TYPE(SUEWS_TIMER) :: timer
      INTEGER, INTENT(IN) :: id ! day of year, 1-366 [-]
      INTEGER, INTENT(IN) :: imin !minutes, 0-59 [min]
      INTEGER, INTENT(IN) :: isec ! seconds, 0-59 [s]
      INTEGER, INTENT(IN) :: it ! hour, 0-23 [h]
      INTEGER, INTENT(IN) :: iy ! year [y]
      INTEGER, INTENT(IN) :: tstep !timestep [s]
      INTEGER, INTENT(IN) :: tstep_prev ! tstep size of the previous step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]

      ! ---method-related variables
      TYPE(METHOD_PRM) :: methodPrm
      INTEGER, INTENT(IN) :: Diagnose ! flag for printing diagnostic info during runtime [N/A]C
      INTEGER, INTENT(in) :: DiagMethod !Defines how near surface diagnostics are calculated
      INTEGER, INTENT(IN) :: EmissionsMethod !method to calculate anthropogenic heat [-]
      INTEGER, INTENT(IN) :: RoughLenHeatMethod ! method to calculate heat roughness length [-]
      INTEGER, INTENT(IN) :: RoughLenMomMethod ! Determines how aerodynamic roughness length (z0m) and zero displacement height (zdm) are calculated [-]
      INTEGER, INTENT(IN) :: FAIMethod !Determines how FAI is calculated [-]
      INTEGER, INTENT(IN) :: SMDMethod ! Determines method for calculating soil moisture deficit [-]
      INTEGER, INTENT(IN) :: WaterUseMethod !Defines how external water use is calculated[-]
      INTEGER, INTENT(IN) :: NetRadiationMethod ! method for calculation of radiation fluxes [-]
      INTEGER, INTENT(IN) :: StabilityMethod !method to calculate atmospheric stability [-]
      INTEGER, INTENT(IN) :: StorageHeatMethod
      INTEGER, INTENT(IN) :: SnowUse ! Determines whether the snow part of the model runs[-]
      LOGICAL, INTENT(IN) :: use_sw_direct_albedo !boolean, Specify ground and roof albedos separately for direct solar radiation [-]
      INTEGER, INTENT(IN) :: OHMIncQF ! Determines whether the storage heat flux calculation uses Q* or ( Q* +QF) [-]

      ! ---lumps-related variables
      TYPE(LUMPS_PRM) :: lumpsPrm
      REAL(KIND(1D0)), INTENT(IN) :: RAINCOVER !limit when surface totally covered with water for LUMPS [mm]
      REAL(KIND(1D0)), INTENT(IN) :: RainMaxRes !maximum water bucket reservoir. Used for LUMPS surface wetness control. [mm]
      REAL(KIND(1D0)), INTENT(IN) :: DRAINRT !Drainage rate of the water bucket [mm hr-1]
      INTEGER, INTENT(IN) :: veg_type !Defines how vegetation is calculated for LUMPS [-]

      ! ---ehc-related variables
      TYPE(EHC_PRM) :: ehcPrm
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_roof !Capacity of soil store for roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_roof !Limit for state_id of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_roof ! wetness threshold  of roof[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_wall !Capacity of soil store for wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_wall !Limit for state_id of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_wall ! wetness threshold  of wall[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof ! indoor temperature for roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof ! thermal conductivity of roof [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof ! Heat capacity of roof [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof ! thickness of each layer in roof [m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall ! indoor temperature for wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall ! thermal conductivity of wall [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall ! Heat capacity of wall [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall ! thickness of each layer in wall [m]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf !deep bottom temperature for each surface [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf ! thermal conductivity of v [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf ! Heat capacity of each surface [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf ! thickness of each layer in each surface [m]

      ! ---spartacus-related variables
      TYPE(SPARTACUS_PRM) :: spartacusPrm
      REAL(KIND(1D0)), INTENT(IN) :: air_ext_lw
      REAL(KIND(1D0)), INTENT(IN) :: air_ext_sw
      REAL(KIND(1D0)), INTENT(IN) :: air_ssa_lw
      REAL(KIND(1D0)), INTENT(IN) :: air_ssa_sw
      REAL(KIND(1D0)), INTENT(IN) :: veg_ssa_lw
      REAL(KIND(1D0)), INTENT(IN) :: veg_ssa_sw
      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height ! height in spartacus [m]
      REAL(KIND(1D0)), INTENT(IN) :: ground_albedo_dir_mult_fact
      INTEGER, INTENT(IN) :: n_stream_lw_urban ! LW streams per hemisphere [-]
      INTEGER, INTENT(IN) :: n_stream_sw_urban ! shortwave diffuse streams per hemisphere [-]
      INTEGER, INTENT(IN) :: n_vegetation_region_urban !Number of regions used to describe vegetation [-]
      REAL(KIND(1D0)), INTENT(IN) :: sw_dn_direct_frac
      REAL(KIND(1D0)), INTENT(IN) :: veg_contact_fraction_const
      REAL(KIND(1D0)), INTENT(IN) :: veg_fsd_const

      ! ---spartacusLayer-related variables
      TYPE(SPARTACUS_LAYER_PRM) :: spartacusLayerPrm
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac !building fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale ! diameter of buildings [[m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac !vegetation fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale ! scale of tree crowns [m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof !albedo of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof ! emissivity of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall !albedo of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall ! emissivity of wall [-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact !Ratio of the direct and diffuse albedo of the roof[-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac ! Fraction of wall reflection that is specular [-]

      ! ---anthropogenic heat-related variables
      TYPE(anthroEMIS_PRM) :: ahemisPrm
      INTEGER, INTENT(IN) :: startDLS !start of daylight saving  [DOY]
      INTEGER, INTENT(IN) :: endDLS !end of daylight saving [DOY]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: QF0_BEU ! Fraction of base value coming from buildings [-]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_A ! Base value for QF [W m-2]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_B ! Parameter related to heating degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_C ! Parameter related to cooling degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Cooling ! base temperature for cooling degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Heating ! base temperatrue for heating degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: PopDensDaytime ! Daytime population density [people ha-1] (i.e. workers)
      REAL(KIND(1D0)), INTENT(IN) :: PopDensNighttime ! nighttime population density (i.e. residents) [ha-1]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: PopProf_24hr !Hourly profile values used in dynamic population estimation[-]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_MIN !minimum QF values [W m-2]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: AHProf_24hr !Hourly profile values used in energy use calculation [-]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Cooling ! cooling slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Heating ! heating slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), INTENT(IN) :: EF_umolCO2perJ !co2 emission factor [umol J-1]
      REAL(KIND(1D0)), INTENT(IN) :: EnEF_v_Jkm ! energy emission factor [J K m-1]
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_Heat ! fraction of fossil fuel heat [-]
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_NonHeat ! fraction of fossil fuel non heat [-]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: FcEF_v_kgkm ! CO2 Emission factor [kg km-1]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: HumActivity_24hr !Hourly profile values used in human activity calculation[-]
      REAL(KIND(1D0)), INTENT(IN) :: MaxFCMetab ! maximum FC metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(IN) :: MaxQFMetab ! maximum QF Metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: MinFCMetab ! minimum QF metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(IN) :: MinQFMetab ! minimum FC metabolism [W m-2]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: TrafficRate ! Traffic rate [veh km m-2 s-1]
      REAL(KIND(1D0)), INTENT(IN) :: TrafficUnits ! traffic units choice [-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: TraffProf_24hr !Hourly profile values used in traffic activity calculation[-]

      ! ---irrigation-related variables
      TYPE(IRRIGATION_PRM) :: irrPrm
      REAL(KIND(1D0)), INTENT(IN) :: H_maintain ! ponding water depth to maintain [mm]
      REAL(KIND(1D0)), INTENT(IN) :: Faut !Fraction of irrigated area using automatic irrigation [-]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_a !Coefficient for automatic irrigation model
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_m !Coefficients for manual irrigation models
      INTEGER, INTENT(IN) :: Ie_end !ending time of water use [DOY]
      INTEGER, INTENT(IN) :: Ie_start !starting time of water use [DOY]
      REAL(KIND(1D0)), INTENT(IN) :: InternalWaterUse_h !Internal water use [mm h-1]
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWat !Irrigation flag: 1 for on and 0 for off [-]
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWatPer !Fraction of properties using irrigation for each day of a week [-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfA_24hr !Hourly profile values used in automatic irrigation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfM_24hr !Hourly profile values used in manual irrigation[-]

      ! ---snow-related variables
      TYPE(SNOW_PRM) :: snowPrm
      REAL(KIND(1D0)), INTENT(IN) :: CRWmax !maximum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(IN) :: CRWmin !minimum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(IN) :: NARP_EMIS_SNOW ! snow emissivity in NARP model [-]
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimit !temperature limit when precipitation falls as snow [degC]
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimitAlb !Limit for hourly precipitation when the ground is fully covered with snow [mm]
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMax !effective surface albedo (middle of the day value) for summertime [-]
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMin !effective surface albedo (middle of the day value) for wintertime (not including snow) [-]
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMax !maximum snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMin !fresh snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimBldg !Limit of the snow water equivalent for snow removal from building roofs [mm]
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimPaved !limit of the snow water equivalent for snow removal from roads[mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SnowPackLimit !Limit for the snow water equivalent when snow cover starts to be patchy [mm]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: SnowProf_24hr !Hourly profile values used in snow clearing [-]
      REAL(KIND(1D0)), INTENT(IN) :: tau_a !time constant for snow albedo aging in cold snow [-]
      REAL(KIND(1D0)), INTENT(IN) :: tau_f !time constant for snow albedo aging in melting snow [-]
      REAL(KIND(1D0)), INTENT(IN) :: tau_r !time constant for snow density ageing [-]
      REAL(KIND(1D0)), INTENT(IN) :: TempMeltFact !hourly temperature melt factor of snow [mm K-1 h-1]
      REAL(KIND(1D0)), INTENT(IN) :: RadMeltFact !hourly radiation melt factor of snow [mm W-1 h-1]

      ! ---conductance-related variables
      TYPE(CONDUCTANCE_PRM) :: conductancePrm
      REAL(KIND(1D0)), INTENT(IN) :: g_max !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_k !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_q_base !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_q_shape !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_t !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: g_sm !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: Kmax !annual maximum hourly solar radiation [W m-2]
      INTEGER, INTENT(IN) :: gsModel !choice of gs parameterisation (1 = Ja11, 2 = Wa16) [-]
      REAL(KIND(1D0)), INTENT(IN) :: S1 !a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)), INTENT(IN) :: S2 !a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)), INTENT(IN) :: TH !upper air temperature limit [degC]
      REAL(KIND(1D0)), INTENT(IN) :: TL !lower air temperature limit [degC]

      ! ---land cover related variables
      TYPE(LC_PAVED_PRM) :: pavedPrm
      TYPE(LC_BLDG_PRM) :: bldgPrm
      TYPE(LC_DECTR_PRM) :: dectrPrm
      TYPE(LC_EVETR_PRM) :: evetrPrm
      TYPE(LC_GRASS_PRM) :: grassPrm
      TYPE(LC_BSOIL_PRM) :: bsoilPrm
      TYPE(LC_WATER_PRM) :: waterPrm

      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf !surface cover fraction[-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: emis !Effective surface emissivity[-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: chAnOHM !Bulk transfer coefficient for this surface to use in AnOHM [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: cpAnOHM !Volumetric heat capacity for this surface to use in AnOHM [J m-3]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: kkAnOHM !Thermal conductivity for this surface to use in AnOHM [W m K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshSW !Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshWD !Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, 4, 3), INTENT(IN) :: OHM_coef !Coefficients for OHM calculation
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilDepth !Depth of soil beneath the surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SatHydraulicConduct !Hydraulic conductivity for saturated soil [mm s-1]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: StateLimit_surf !Upper limit to the surface state [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: WetThresh_surf ! !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF + 1, NSURF - 1), INTENT(IN) :: WaterDist !Fraction of water redistribution [-]

      REAL(KIND(1D0)), INTENT(IN) :: IrrFracPaved !fraction of paved which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBldgs !fraction of buildings (e.g., green roofs) which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracDecTr !fraction of deciduous trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracEveTr !fraction of evergreen trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracGrass !fraction of grass which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBSoil !fraction of bare soil trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracWater !fraction of water which are irrigated [-]

      REAL(KIND(1D0)), INTENT(IN) :: bldgH !average building height [m]
      REAL(KIND(1D0)), INTENT(IN) :: FAIBldg ! frontal area index for buildings [-]

      REAL(KIND(1D0)), INTENT(IN) :: DecTreeH !average height of deciduous tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: FAIDecTree ! frontal area index for deciduous tree [-]
      REAL(KIND(1D0)), INTENT(IN) :: CapMax_dec !maximum water storage capacity for upper surfaces (i.e. canopy)
      REAL(KIND(1D0)), INTENT(IN) :: CapMin_dec !minimum water storage capacity for upper surfaces (i.e. canopy)
      REAL(KIND(1D0)), INTENT(IN) :: PorMax_dec !full leaf-on summertime value used only for DecTr
      REAL(KIND(1D0)), INTENT(IN) :: PorMin_dec !leaf-off wintertime value used only for DecTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_DecTr !maximum albedo for deciduous tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_DecTr !minimum albedo for deciduous tree and shrub [-]

      REAL(KIND(1D0)), INTENT(IN) :: EveTreeH !height of evergreen tree [m]
      REAL(KIND(1D0)), INTENT(IN) :: FAIEveTree ! frontal area index for evergreen tree [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_EveTr !maximum albedo for evergreen tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_EveTr !minimum albedo for evergreen tree and shrub [-]

      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_Grass !maximum albedo for grass [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_Grass !minimum albedo for grass [-]

      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_bioCO2 !The light-saturated gross photosynthesis of the canopy [umol m-2 s-1 ]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_enh_bioCO2 !Part of the beta coefficient related to the fraction of vegetation [umol m-2 s-1 ]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_bioCO2 !The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve [-]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_enh_bioCO2 !Part of the alpha coefficient related to the fraction of vegetation[-]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_a !Respiration coefficient a
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_b !Respiration coefficient b - related to air temperature dependency
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: min_res_bioCO2 !Minimum soil respiration rate (for cold-temperature limit) [umol m-2 s-1]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: theta_bioCO2 !The convexity of the curve at light saturation [-]

      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseT !Base Temperature for initiating growing degree days (GDD) for leaf growth [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseTe !Base temperature for initiating sensesance degree days (SDD) for leaf off [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: SDDFull !the sensesence degree days (SDD) needed to initiate leaf off [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: GDDFull !the growing degree days (GDD) needed for full capacity of the leaf area index [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMax !full leaf-on summertime value [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMin !leaf-off wintertime value [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(4, NVEGSURF), INTENT(IN) :: LAIPower !parameters required by LAI calculation
      INTEGER, DIMENSION(NVEGSURF), INTENT(IN) :: LAIType !LAI calculation choice[-]

      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: MaxConductance !the maximum conductance of each vegetation or surface type. [mm s-1]

      ! ********** SUEWS_stateVariables **********
      ! ---anthropogenic heat-related states
      TYPE(anthroHEAT_STATE) :: anthroHeatState
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating Degree Days [degC d]

      ! ---water balance related states
      TYPE(HYDRO_STATE) :: hydroState
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_roof !Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_roof !wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_wall !Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_wall !wetness status of wall [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: soilstore_surf !soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: state_surf !wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(9), INTENT(INOUT) :: WUDay_id !Daily water use for EveTr, DecTr, Grass [mm]

      ! ---heat storage related states
      TYPE(HEAT_STATE) :: heatState
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(INOUT) :: temp_roof !interface temperature between depth layers in roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_roof !roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(INOUT) :: temp_wall !interface temperature between depth layers in wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_wall !wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(INOUT) :: tsfc_surf !surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(INOUT) :: temp_surf !interface temperature between depth layers [degC]

      ! ---OHM related states
      TYPE(OHM_STATE) :: ohmState
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_av ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: dqndt ! rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_s_av ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: dqnsdt ! Rate of change of net radiation [W m-2 h-1]

      ! ---snow related states
      TYPE(SNOW_STATE) :: snowState
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowfallCum !cumulated snow falling [mm]
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowAlb !albedo of know [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: IceFrac !fraction of ice in snowpack [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowWater ! snow water[mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowDens !snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowFrac !snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowPack !snow water equivalent on each land cover [mm]

      ! ---phenology related states
      TYPE(PHENOLOGY_STATE) :: phenState
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: alb !albedo [-]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: GDD_id !Growing Degree Days [degC d]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INout) :: SDD_id !Senescence Degree Days[degC d]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: LAI_id !LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), INTENT(INout) :: Tmin_id !Daily minimum temperature [degC]
      REAL(KIND(1D0)), INTENT(INout) :: Tmax_id !Daily maximum temperature [degC]
      REAL(KIND(1D0)), INTENT(INout) :: lenDay_id !daytime length [h]
      REAL(KIND(1D0)), INTENT(INOUT) :: DecidCap_id !Moisture storage capacity of deciduous trees [mm]
      REAL(KIND(1D0)), INTENT(INOUT) :: albDecTr_id !Albedo of deciduous trees [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: albEveTr_id !Albedo of evergreen trees [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: albGrass_id !Albedo of grass  [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id !Porosity of deciduous trees [-]
      REAL(KIND(1D0)), DIMENSION(6, NSURF), INTENT(INOUT) :: StoreDrainPrm !coefficients used in drainage calculation [-]

      ! ########################################################################################
      ! output variables
      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5) :: dataOutLineESTMExt
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5) :: dataoutLineRSL
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug - 5) :: dataOutLineDebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5) :: dataOutLineSPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5) :: dataOutLineDailyState
      ! save all output variables in a single derived type
      TYPE(output_line), INTENT(OUT) :: output_line_suews
      ! ########################################################################################

      ! ########################################################################################
      ! local variables
      REAL(KIND(1D0)) :: a1 !AnOHM coefficients of grid [-]
      REAL(KIND(1D0)) :: a2 ! AnOHM coefficients of grid [h]
      REAL(KIND(1D0)) :: a3 !AnOHM coefficients of grid [W m-2]
      REAL(KIND(1D0)) :: AdditionalWater = 0 !!Additional water coming from other grids [mm] (these are expressed as depths over the whole surface)
      REAL(KIND(1D0)) :: U10_ms !average wind speed at 10m [W m-1]
      REAL(KIND(1D0)) :: azimuth !solar azimuth [angle]
      REAL(KIND(1D0)) :: chSnow_per_interval ! change state_id of snow and surface per time interval [mm]

      REAL(KIND(1D0)) :: dens_dry !Vap density or absolute humidity (kg m-3)
      REAL(KIND(1D0)) :: deltaLAI !change in LAI [m2 m-2]
      REAL(KIND(1D0)) :: drain_per_tstep ! total drainage for all surface type at each timestep [mm]
      REAL(KIND(1D0)) :: Ea_hPa !vapor pressure [hPa]
      REAL(KIND(1D0)) :: QE_LUMPS !turbulent latent heat flux by LUMPS model [W m-2]
      REAL(KIND(1D0)) :: es_hPa !Saturation vapour pressure over water  [hPa]
      REAL(KIND(1D0)) :: ev_per_tstep ! evaporation at each time step [mm]
      REAL(KIND(1D0)) :: wu_ext !external water use [mm]
      REAL(KIND(1D0)) :: Fc !total co2 flux [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_anthro !anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_biogen !biogenic CO2 flux [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_build ! anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)) :: fcld !estomated cloud fraction [-]
      REAL(KIND(1D0)) :: Fc_metab ! co2 emission from metabolism component [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_photo !co2 flux from photosynthesis [umol m
      REAL(KIND(1D0)) :: Fc_point ! co2 emission from point source [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_respi !co2 flux from respiration [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_traff ! co2 emission from traffic component [umol m-2 s-1]
      REAL(KIND(1D0)) :: gfunc
      REAL(KIND(1D0)) :: gsc !Surface Layer Conductance
      REAL(KIND(1D0)) :: QH_LUMPS !turbulent sensible heat flux from LUMPS model [W m-2]
      REAL(KIND(1D0)) :: wu_int !internal water use [mm]
      REAL(KIND(1D0)) :: kclear !clear sky incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: kup !outgoing shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: ldown !incoming longtwave radiation [W m-2]
      REAL(KIND(1D0)) :: lup !outgoing longwave radiation [W m-2]
      REAL(KIND(1D0)) :: L_mod !Obukhov length [m]
      REAL(KIND(1D0)) :: mwh !snowmelt [mm]
      REAL(KIND(1D0)) :: mwstore !overall met water [mm]
      REAL(KIND(1D0)) :: NWstate_per_tstep ! state_id at each tinestep(excluding water body) [mm]
      REAL(KIND(1D0)) :: FAI ! frontal area index [-]
      REAL(KIND(1D0)) :: PAI ! plan area index [-]
      REAL(KIND(1D0)) :: zL ! Stability scale [-]
      REAL(KIND(1D0)) :: q2_gkg ! Air specific humidity at 2 m [g kg-1]
      REAL(KIND(1D0)) :: qe !turbuent latent heat flux [W m-2]
      REAL(KIND(1D0)) :: qf !anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)) :: QF_SAHP !total anthropogeic heat flux when EmissionMethod is not 0 [W m-2]
      REAL(KIND(1D0)) :: qh !turbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_residual ! residual based sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_resist !resistance bnased sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: Qm !Snowmelt-related heat [W m-2]
      REAL(KIND(1D0)) :: QmFreez !heat related to freezing of surface store [W m-2]
      REAL(KIND(1D0)) :: QmRain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)) :: qn !net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: qn_snow !net all-wave radiation on snow surface [W m-2]
      REAL(KIND(1D0)) :: qn_snowfree !net all-wave radiation on snow-free surface [W m-2]
      REAL(KIND(1D0)) :: qs !heat storage flux [W m-2]
      REAL(KIND(1D0)) :: RA_h ! aerodynamic resistance [s m-1]
      REAL(KIND(1D0)) :: RS ! surface resistance [s m-1]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: rss_surf ! surface resistance adjusted by surface wetness state[s m-1]
      REAL(KIND(1D0)) :: RH2 ! air relative humidity at 2m [-]
      REAL(KIND(1D0)) :: runoffAGveg !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)) :: runoffAGimpervious !Above ground runoff from impervious surface for all surface area [mm]
      REAL(KIND(1D0)) :: runoff_per_tstep !runoff water at each time step [mm]
      REAL(KIND(1D0)) :: runoffPipes !runoff to pipes [mm]
      REAL(KIND(1D0)) :: runoffSoil_per_tstep !Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
      REAL(KIND(1D0)) :: runoffwaterbody !Above ground runoff from water body for all surface area [mm]
      REAL(KIND(1D0)) :: smd !soil moisture deficit [mm]
      REAL(KIND(1D0)) :: SoilState !Area-averaged soil moisture  for whole surface [mm]
      REAL(KIND(1D0)) :: state_per_tstep !state_id at each timestep [mm]
      REAL(KIND(1D0)) :: surf_chang_per_tstep !change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)) :: swe !overall snow water equavalent[mm]
      REAL(KIND(1D0)) :: t2_C !modelled 2 meter air temperature [degC]
      REAL(KIND(1D0)) :: TSfc_C ! surface temperature [degC]
      REAL(KIND(1D0)) :: TempVeg ! temporary vegetative surface fraction adjusted by rainfall [-]
      REAL(KIND(1D0)) :: tot_chang_per_tstep !Change in surface state_id [mm]
      REAL(KIND(1D0)) :: TStar !T*, temperature scale [-]
      REAL(KIND(1D0)) :: tsurf !surface temperatue [degC]
      REAL(KIND(1D0)) :: UStar !friction velocity [m s-1]
      REAL(KIND(1D0)) :: VPD_Pa !vapour pressure deficit  [Pa]
      REAL(KIND(1D0)) :: z0m !Aerodynamic roughness length [m]
      REAL(KIND(1D0)) :: zdm !zero-plane displacement [m]
      REAL(KIND(1D0)) :: ZENITH_deg !solar zenith angle in degree [°]
      REAL(KIND(1D0)) :: zH ! Mean building height [m]

      REAL(KIND(1D0)), DIMENSION(2) :: SnowRemoval !snow removal [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: wu_surf !external water use of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezMelt !freezing of melt water[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind_snow !outgoing shortwave on snowpack [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: mw_ind !melt water from sknowpack[mm]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_freezState !heat related to freezing of surface store [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_melt !melt heat [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_rain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: qn_ind_snow !net all-wave radiation on snowpack [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: rainOnSnow !rain water on snow event [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: runoffSoil !Soil runoff from each soil sub-surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: smd_nsurf !soil moisture deficit for each surface
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: snowDepth !Snow depth [m]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: Tsurf_ind_snow !snowpack surface temperature [C]

      ! INTEGER, DIMENSION(NSURF) :: snowCalcSwitch
      INTEGER, DIMENSION(3) :: dayofWeek_id ! 1 - day of week; 2 - month; 3 - season
      INTEGER :: DLS

      REAL(KIND(1D0)) :: dq !Specific humidity deficit [g/kg]
      REAL(KIND(1D0)) :: lvS_J_kg !latent heat of sublimation [J kg-1]
      REAL(KIND(1D0)) :: psyc_hPa !psychometric constant [hPa]
      REAL(KIND(1D0)) :: z0v !roughness for heat [m]
      REAL(KIND(1D0)) :: z0vSnow !roughness for heat [m]
      REAL(KIND(1D0)) :: RAsnow !Aerodynamic resistance for snow [s m-1]
      REAL(KIND(1D0)) :: RB !boundary layer resistance shuttleworth
      REAL(KIND(1D0)) :: runoff_per_interval !run-off at each time interval [mm]
      REAL(KIND(1D0)) :: s_hPa !vapour pressure versus temperature slope [hPa K-1]
      REAL(KIND(1D0)) :: sIce_hpa !satured curve on snow [hPa]
      REAL(KIND(1D0)) :: SoilMoistCap !Maximum capacity of soil store [mm]
      ! REAL(KIND(1D0)) :: veg_fr !vegetation fraction [-]
      REAL(KIND(1D0)) :: VegPhenLumps
      REAL(KIND(1D0)) :: VPd_hpa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)) :: vsmd !Soil moisture deficit for vegetated surfaces only [mm]
      REAL(KIND(1D0)) :: ZZD !Active measurement height[m]

      REAL(KIND(1D0)), DIMENSION(NSURF) :: deltaQi ! storage heat flux of snow surfaces [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: drain_surf !drainage of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezState !freezing of state_id [mm]
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezStateVol !surface state_id [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: tsurf_ind !snow-free surface temperature [degC]

      ! TODO: TS 25 Oct 2017
      ! the  variables are not used currently as grid-to-grid connection is NOT set up.
      ! set these variables as zero.
      REAL(KIND(1D0)) :: addImpervious = 0
      REAL(KIND(1D0)) :: addPipes = 0
      REAL(KIND(1D0)) :: addVeg = 0
      REAL(KIND(1D0)) :: addWaterBody = 0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: AddWater = 0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: frac_water2runoff = 0

      ! values that are derived from tstep
      INTEGER :: nsh ! number of timesteps per hour
      REAL(KIND(1D0)) :: nsh_real ! nsh in type real [-]
      REAL(KIND(1D0)) :: tstep_real ! tstep in type real
      REAL(KIND(1D0)) :: dectime !decimal time [-]

      ! values that are derived from sfr_surf (surface fractions)
      REAL(KIND(1D0)) :: VegFraction ! fraction of vegetation [-]
      REAL(KIND(1D0)) :: ImpervFraction !fractioin of impervious surface [-]
      REAL(KIND(1D0)) :: PervFraction !fraction of pervious surfaces [-]
      REAL(KIND(1D0)) :: NonWaterFraction !fraction of non-water [-]

      ! snow related temporary values
      REAL(KIND(1D0)) :: albedo_snow !snow albedo [-]

      ! ########################################################################################
      ! TS 19 Sep 2019
      ! temporary variables to save values for inout varialbes
      ! suffixes  and  denote values from last and to next tsteps, respectively
      ! these variables are introduced to allow safe and robust iterations inccurred in this subroutine
      ! so that these values won't updated in unexpectedly many times

      ! OHM related:
      TYPE(OHM_STATE) :: ohmState_prev, ohmState_next

      ! snow related:
      TYPE(SNOW_STATE) :: snowState_prev, snowState_next

      ! water balance related:
      TYPE(HYDRO_STATE) :: hydroState_prev, hydroState_next
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev0_surf ! evapotranspiration from PM of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev_surf ! evapotranspiration of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_roof ! evapotranspiration of each roof layer [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_wall ! evapotranspiration of each wall type [mm]

      ! phenology related:
      TYPE(PHENOLOGY_STATE) :: phenState_prev, phenState_next

      ! anthropogenic heat related:
      TYPE(anthroHEAT_STATE) :: anthroHeatState_prev, anthroHeatState_next

      REAL(KIND(1D0)) :: Tair_av_prev, Tair_av_next !average air temperature [degC]
      ! ########################################################################################
      ! flag for Tsurf convergence
      LOGICAL :: flag_converge
      REAL(KIND(1D0)) :: Ts_iter !average surface temperature of all surfaces [degC]
      REAL(KIND(1D0)) :: dif_tsfc_iter
      REAL(KIND(1D0)) :: QH_Init !initialised sensible heat flux [W m-2]
      INTEGER :: i_iter

      ! ########################################################################################
      !  ! extended for ESTM_ehc, TS 20 Jan 2022
      !
      ! input arrays: standard suews surfaces
      TYPE(HEAT_STATE) :: heatState_in, heatState_out
      REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_roof !roof surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_wall !wall surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc0_out_roof !surface temperature of roof[degC]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc0_out_wall !surface temperature of wall[degC]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc0_out_surf !surface temperature [degC]

      ! output arrays:

      ! roof facets
      ! aggregated heat storage of all roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QS_roof ! heat storage flux for roof component [W m-2]
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_out_roof !interface temperature between depth layers [degC]

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QG_roof ! heat flux used in ESTM_ehc as forcing of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QN_roof ! net all-wave radiation of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qe_roof ! latent heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_roof ! sensible heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_resist_roof ! resist-based sensible heat flux of roof surface [W m-2]

      ! wall facets
      ! aggregated heat storage of all wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QS_wall ! heat storage flux for wall component [W m-2]
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_out_wall !interface temperature between depth layers [degC]

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QG_wall ! heat flux used in ESTM_ehc as forcing of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QN_wall ! net all-wave radiation of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qe_wall ! latent heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_wall ! sensible heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_resist_wall ! resistance based sensible heat flux of wall surface [W m-2]

      ! standard suews surfaces
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth) :: temp_out_surf !interface temperature between depth layers[degC]

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf) :: QG_surf ! heat flux used in ESTM_ehc as forcing of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: QN_surf ! net all-wave radiation of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qs_surf ! aggregated heat storage of of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe0_surf ! latent heat flux from PM of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_surf ! sensinle heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_resist_surf ! resistance based sensible heat flux of individual surface [W m-2]
      ! surface temperature
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_qh_surf ! latent heat flux of individual surface [W m-2]

      ! iterator for surfaces
      INTEGER :: i_surf !iterator for surfaces

      ! used in iteration
      INTEGER :: max_iter !maximum iteration
      REAL(KIND(1D0)) :: ratio_iter

      REAL(KIND(1D0)) :: g_kdown !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: g_dq !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: g_ta !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: g_smd !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: g_lai !gdq*gtemp*gs*gq for photosynthesis calculations

      ! ####################################################################################
      ! Related to RSL wind profiles
      INTEGER, PARAMETER :: nz = 90 ! number of levels 10 levels in canopy plus 20 (3 x Zh) above the canopy
      INTEGER, PARAMETER :: AerodynamicResistanceMethod = 2 !method to calculate RA [-]
      INTEGER, PARAMETER :: BaseTMethod = 2 ! base t method [-]
      INTEGER, PARAMETER :: DiagQN = 0 ! flag for printing diagnostic info for QN module during runtime [N/A] ! not used and will be removed
      INTEGER, PARAMETER :: DiagQS = 0 ! flag for printing diagnostic info for QS module during runtime [N/A] ! not used and will be removed
      INTEGER, PARAMETER :: EvapMethod = 2 ! Evaporation calculated according to Rutter (1) or Shuttleworth (2) [-]
      INTEGER, PARAMETER :: LAImethod = 1 ! boolean to determine if calculate LAI [-]
      REAL(KIND(1D0)), PARAMETER :: BaseT_HC = 18.2 !base temperature for heating degree dayb [degC] ! to be fully removed TODO

      ! calculated values of FAI
      REAL(KIND(1D0)) :: FAIBldg_use
      REAL(KIND(1D0)) :: FAIEveTree_use
      REAL(KIND(1D0)) :: FAIDecTree_use

      ! ####################################################################################
      ALLOCATE (hydroState_prev%soilstore_roof(nlayer))
      ALLOCATE (hydroState_prev%state_roof(nlayer))
      ALLOCATE (hydroState_prev%soilstore_wall(nlayer))
      ALLOCATE (hydroState_prev%state_wall(nlayer))
      ALLOCATE (hydroState_next%soilstore_roof(nlayer))
      ALLOCATE (hydroState_next%state_roof(nlayer))
      ALLOCATE (hydroState_next%soilstore_wall(nlayer))
      ALLOCATE (hydroState_next%state_wall(nlayer))

      ALLOCATE (heatState_in%temp_roof(nlayer, ndepth))
      ALLOCATE (heatState_in%temp_wall(nlayer, ndepth))
      ALLOCATE (heatState_in%tsfc_roof(nlayer))
      ALLOCATE (heatState_in%tsfc_wall(nlayer))
      ALLOCATE (heatState_in%tsfc_surf(nsurf))
      ALLOCATE (heatState_in%temp_surf(nsurf, ndepth))

      ALLOCATE (heatState_out%temp_roof(nlayer, ndepth))
      ALLOCATE (heatState_out%temp_wall(nlayer, ndepth))
      ALLOCATE (heatState_out%tsfc_roof(nlayer))
      ALLOCATE (heatState_out%tsfc_wall(nlayer))
      ALLOCATE (heatState_out%tsfc_surf(nsurf))
      ALLOCATE (heatState_out%temp_surf(nsurf, ndepth))

      ! ####################################################################################
      siteInfo%lat = lat
      siteInfo%lon = lng
      siteInfo%alt = alt
      siteInfo%gridiv = Gridiv
      siteInfo%timezone = timezone
      siteInfo%surfacearea = SurfaceArea
      siteInfo%z = Z
      siteInfo%z0m_in = z0m_in
      siteInfo%zdm_in = zdm_in
      siteInfo%pipecapacity = PipeCapacity
      siteInfo%runofftowater = RunoffToWater
      siteInfo%narp_trans_site = NARP_TRANS_SITE
      siteInfo%CO2PointSource = CO2PointSource
      siteInfo%flowchange = FlowChange

      forcing%kdown = kdown
      forcing%ldown = ldown_obs
      forcing%RH = avRh
      forcing%pres = Press_hPa
      forcing%U = avU1
      forcing%rain = Precip
      forcing%Wuh = wu_m3
      forcing%fcld = fcld_obs
      forcing%LAI_obs = LAI_obs
      forcing%snowfrac = snowFrac_obs
      forcing%xsmd = xsmd
      forcing%qn1_obs = qn1_obs
      forcing%qs_obs = qs_obs
      forcing%qf_obs = qf_obs
      forcing%Tair = Tair_av
      forcing%temp_c = Temp_C

      timer%id = id
      timer%imin = imin
      timer%isec = isec
      timer%it = it
      timer%iy = iy
      timer%tstep = tstep
      timer%tstep_prev = tstep_prev
      timer%dt_since_start = dt_since_start

      methodPrm%Diagnose = Diagnose
      methodPrm%DiagMethod = DiagMethod
      methodPrm%EmissionsMethod = EmissionsMethod
      methodPrm%RoughLenHeatMethod = RoughLenHeatMethod
      methodPrm%RoughLenMomMethod = RoughLenMomMethod
      methodPrm%FAIMethod = FAIMethod
      methodPrm%SMDMethod = SMDMethod
      methodPrm%WaterUseMethod = WaterUseMethod
      methodPrm%NetRadiationMethod = NetRadiationMethod
      methodPrm%StabilityMethod = StabilityMethod
      methodPrm%StorageHeatMethod = StorageHeatMethod
      methodPrm%SnowUse = SnowUse
      methodPrm%use_sw_direct_albedo = use_sw_direct_albedo
      methodPrm%ohmIncQF = OHMIncQF

      lumpsPrm%raincover = RAINCOVER
      lumpsPrm%rainmaxres = RainMaxRes
      lumpsPrm%drainrt = DRAINRT
      lumpsPrm%veg_type = veg_type

      ! ESTM_ehc
      ALLOCATE (ehcPrm%soil_storecap_roof(nlayer))
      ALLOCATE (ehcPrm%soil_storecap_wall(nlayer))
      ALLOCATE (ehcPrm%state_limit_roof(nlayer))
      ALLOCATE (ehcPrm%state_limit_wall(nlayer))
      ALLOCATE (ehcPrm%wet_thresh_roof(nlayer))
      ALLOCATE (ehcPrm%wet_thresh_wall(nlayer))
      ALLOCATE (ehcPrm%tin_roof(nlayer))
      ALLOCATE (ehcPrm%tin_wall(nlayer))
      ALLOCATE (ehcPrm%tin_surf(nlayer))
      ALLOCATE (ehcPrm%k_roof(nlayer, ndepth))
      ALLOCATE (ehcPrm%k_wall(nlayer, ndepth))
      ALLOCATE (ehcPrm%k_surf(nlayer, ndepth))
      ALLOCATE (ehcPrm%cp_roof(nlayer, ndepth))
      ALLOCATE (ehcPrm%cp_wall(nlayer, ndepth))
      ALLOCATE (ehcPrm%cp_surf(nlayer, ndepth))
      ALLOCATE (ehcPrm%dz_roof(nlayer, ndepth))
      ALLOCATE (ehcPrm%dz_wall(nlayer, ndepth))
      ALLOCATE (ehcPrm%dz_surf(nlayer, ndepth))
      ehcPrm%soil_storecap_roof = SoilStoreCap_roof
      ehcPrm%soil_storecap_wall = SoilStoreCap_wall
      ehcPrm%state_limit_roof = StateLimit_roof
      ehcPrm%state_limit_wall = StateLimit_wall
      ehcPrm%wet_thresh_roof = WetThresh_roof
      ehcPrm%wet_thresh_wall = WetThresh_wall
      ehcPrm%tin_roof = tin_roof
      ehcPrm%tin_wall = tin_wall
      ehcPrm%tin_surf = tin_surf
      ehcPrm%k_roof = k_roof
      ehcPrm%k_wall = k_wall
      ehcPrm%k_surf = k_surf
      ehcPrm%cp_roof = cp_roof
      ehcPrm%cp_wall = cp_wall
      ehcPrm%cp_surf = cp_surf
      ehcPrm%dz_roof = dz_roof
      ehcPrm%dz_wall = dz_wall
      ehcPrm%dz_surf = dz_surf

      ALLOCATE (spartacusPrm%height(nlayer + 1))
      spartacusPrm%air_ext_lw = air_ext_lw
      spartacusPrm%air_ext_sw = air_ext_sw
      spartacusPrm%air_ssa_lw = air_ssa_lw
      spartacusPrm%air_ssa_sw = air_ssa_sw
      spartacusPrm%veg_ssa_lw = veg_ssa_lw
      spartacusPrm%veg_ssa_sw = veg_ssa_sw
      spartacusPrm%height = height
      spartacusPrm%ground_albedo_dir_mult_fact = ground_albedo_dir_mult_fact
      spartacusPrm%n_stream_lw_urban = n_stream_lw_urban
      spartacusPrm%n_stream_sw_urban = n_stream_sw_urban
      spartacusPrm%n_vegetation_region_urban = n_vegetation_region_urban
      spartacusPrm%sw_dn_direct_frac = sw_dn_direct_frac
      spartacusPrm%veg_contact_fraction_const = veg_contact_fraction_const
      spartacusPrm%veg_fsd_const = veg_fsd_const

      ALLOCATE (spartacusLayerPrm%building_frac(nlayer))
      ALLOCATE (spartacusLayerPrm%building_scale(nlayer))
      ALLOCATE (spartacusLayerPrm%veg_frac(nlayer))
      ALLOCATE (spartacusLayerPrm%veg_scale(nlayer))
      ALLOCATE (spartacusLayerPrm%alb_roof(nlayer))
      ALLOCATE (spartacusLayerPrm%emis_roof(nlayer))
      ALLOCATE (spartacusLayerPrm%alb_wall(nlayer))
      ALLOCATE (spartacusLayerPrm%emis_wall(nlayer))
      ALLOCATE (spartacusLayerPrm%roof_albedo_dir_mult_fact(nspec, nlayer))
      ALLOCATE (spartacusLayerPrm%wall_specular_frac(nspec, nlayer))
      spartacusLayerPrm%building_frac = building_frac
      spartacusLayerPrm%building_scale = building_scale
      spartacusLayerPrm%veg_frac = veg_frac
      spartacusLayerPrm%veg_scale = veg_scale
      spartacusLayerPrm%alb_roof = alb_roof
      spartacusLayerPrm%emis_roof = emis_roof
      spartacusLayerPrm%alb_wall = alb_wall
      spartacusLayerPrm%emis_wall = emis_wall
      spartacusLayerPrm%roof_albedo_dir_mult_fact = roof_albedo_dir_mult_fact
      spartacusLayerPrm%wall_specular_frac = wall_specular_frac

      ahemisPrm%startdls = startDLS
      ahemisPrm%enddls = endDLS
      ahemisPrm%anthroheat%qf0_beu_working = QF0_BEU(1)
      ahemisPrm%anthroheat%qf0_beu_holiday = QF0_BEU(2)
      ahemisPrm%anthroheat%qf_a_working = Qf_A(1)
      ahemisPrm%anthroheat%qf_a_holiday = Qf_A(2)
      ahemisPrm%anthroheat%qf_b_working = Qf_B(1)
      ahemisPrm%anthroheat%qf_b_holiday = Qf_B(2)
      ahemisPrm%anthroheat%qf_c_working = Qf_C(1)
      ahemisPrm%anthroheat%qf_c_holiday = Qf_C(2)
      ahemisPrm%anthroheat%baset_cooling_working = BaseT_Cooling(1)
      ahemisPrm%anthroheat%baset_cooling_holiday = BaseT_Cooling(2)
      ahemisPrm%anthroheat%baset_heating_working = BaseT_Heating(1)
      ahemisPrm%anthroheat%baset_heating_holiday = BaseT_Heating(2)
      ahemisPrm%anthroheat%popdensdaytime_working = PopDensDaytime(1)
      ahemisPrm%anthroheat%popdensdaytime_holiday = PopDensDaytime(2)
      ahemisPrm%anthroheat%popdensnighttime = PopDensNighttime
      ahemisPrm%anthroheat%popprof_24hr_working = PopProf_24hr(:, 1)
      ahemisPrm%anthroheat%popprof_24hr_holiday = PopProf_24hr(:, 2)
      ahemisPrm%anthroheat%ah_min_working = AH_MIN(1)
      ahemisPrm%anthroheat%ah_min_holiday = AH_MIN(2)
      ahemisPrm%anthroheat%ahprof_24hr_working = AHProf_24hr(:, 1)
      ahemisPrm%anthroheat%ahprof_24hr_holiday = AHProf_24hr(:, 2)
      ahemisPrm%anthroheat%ah_slope_cooling_working = AH_SLOPE_Cooling(1)
      ahemisPrm%anthroheat%ah_slope_cooling_holiday = AH_SLOPE_Cooling(2)
      ahemisPrm%anthroheat%ah_slope_heating_working = AH_SLOPE_Heating(1)
      ahemisPrm%anthroheat%ah_slope_heating_holiday = AH_SLOPE_Heating(2)
      ahemisPrm%EF_umolCO2perJ = EF_umolCO2perJ
      ahemisPrm%EnEF_v_Jkm = EnEF_v_Jkm
      ahemisPrm%FrFossilFuel_Heat = FrFossilFuel_Heat
      ahemisPrm%FrFossilFuel_NonHeat = FrFossilFuel_NonHeat
      ahemisPrm%FcEF_v_kgkm = FcEF_v_kgkm
      ahemisPrm%HumActivity_24hr_working = HumActivity_24hr(:, 1)
      ahemisPrm%HumActivity_24hr_holiday = HumActivity_24hr(:, 2)
      ahemisPrm%MaxFCMetab = MaxFCMetab
      ahemisPrm%MaxQFMetab = MaxQFMetab
      ahemisPrm%MinFCMetab = MinFCMetab
      ahemisPrm%MinQFMetab = MinQFMetab
      ahemisPrm%TrafficRate_working = TrafficRate(1)
      ahemisPrm%TrafficRate_holiday = TrafficRate(2)
      ahemisPrm%TrafficUnits = TrafficUnits
      ahemisPrm%TraffProf_24hr_working = TraffProf_24hr(:, 1)
      ahemisPrm%TraffProf_24hr_holiday = TraffProf_24hr(:, 2)

      irrPrm%h_maintain = H_maintain
      irrPrm%faut = Faut
      irrPrm%ie_a = Ie_a
      irrPrm%ie_m = Ie_m
      irrPrm%ie_start = Ie_start
      irrPrm%ie_end = Ie_end
      irrPrm%internalwateruse_h = InternalWaterUse_h
      irrPrm%irr_daywater%monday_flag = DayWat(1)
      irrPrm%irr_daywater%monday_percent = DayWatPer(1)
      irrPrm%irr_daywater%tuesday_flag = DayWat(2)
      irrPrm%irr_daywater%tuesday_percent = DayWatPer(2)
      irrPrm%irr_daywater%wednesday_flag = DayWat(3)
      irrPrm%irr_daywater%wednesday_percent = DayWatPer(3)
      irrPrm%irr_daywater%thursday_flag = DayWat(4)
      irrPrm%irr_daywater%thursday_percent = DayWatPer(4)
      irrPrm%irr_daywater%friday_flag = DayWat(5)
      irrPrm%irr_daywater%friday_percent = DayWatPer(5)
      irrPrm%irr_daywater%saturday_flag = DayWat(6)
      irrPrm%irr_daywater%saturday_percent = DayWatPer(6)
      irrPrm%irr_daywater%sunday_flag = DayWat(7)
      irrPrm%irr_daywater%sunday_percent = DayWatPer(7)
      irrPrm%wuprofa_24hr_working = WUProfA_24hr(:, 1)
      irrPrm%wuprofa_24hr_holiday = WUProfA_24hr(:, 2)
      irrPrm%wuprofm_24hr_working = WUProfM_24hr(:, 1)
      irrPrm%wuprofm_24hr_holiday = WUProfM_24hr(:, 2)

      snowPrm%crwmax = CRWmax
      snowPrm%crwmin = CRWmin
      snowPrm%narp_emis_snow = NARP_EMIS_SNOW
      snowPrm%preciplimit = PrecipLimit
      snowPrm%preciplimitalb = PrecipLimitAlb
      snowPrm%snowalbmax = SnowAlbMax
      snowPrm%snowalbmin = SnowAlbMin
      snowPrm%snowdensmax = SnowDensMax
      snowPrm%snowdensmin = SnowDensMin
      snowPrm%snowlimbldg = SnowLimBldg
      snowPrm%snowlimpaved = SnowLimPaved
      snowPrm%snowpacklimit = SnowPackLimit
      snowPrm%snowprof_24hr_working = SnowProf_24hr(:, 1)
      snowPrm%snowprof_24hr_holiday = SnowProf_24hr(:, 2)
      snowPrm%tau_a = tau_a
      snowPrm%tau_f = tau_f
      snowPrm%tau_r = tau_r
      snowPrm%tempmeltfact = TempMeltFact
      snowPrm%radmeltfact = RadMeltFact

      conductancePrm%g_max = g_max
      conductancePrm%g_k = g_k
      conductancePrm%g_q_base = g_q_base
      conductancePrm%g_q_shape = g_q_shape
      conductancePrm%g_t = g_t
      conductancePrm%g_sm = g_sm
      conductancePrm%kmax = Kmax
      conductancePrm%gsmodel = gsModel
      conductancePrm%s1 = S1
      conductancePrm%s2 = S2
      conductancePrm%TH = TH
      conductancePrm%TL = TL

      pavedPrm%sfr = sfr_surf(PavSurf)
      pavedPrm%emis = emis(PavSurf)
      pavedPrm%ohm%chanohm = chAnOHM(PavSurf)
      pavedPrm%ohm%cpanohm = cpAnOHM(PavSurf)
      pavedPrm%ohm%kkanohm = kkAnOHM(PavSurf)
      pavedPrm%ohm%ohm_threshsw = OHM_threshSW(PavSurf)
      pavedPrm%ohm%ohm_threshwd = OHM_threshWD(PavSurf)

      !WRITE(*, *) 'OHM_COEF_pav', OHM_coef

      pavedPrm%ohm%ohm_coef_lc(1)%summer_wet = OHM_coef(PavSurf, 1, 1)
      !WRITE(*, *) 'PavSurf_OHM_COEF_A1_SUMMER_WET', OHM_coef(PavSurf, 1, 1)
      pavedPrm%ohm%ohm_coef_lc(1)%summer_dry = OHM_coef(PavSurf, 2, 1)
      !WRITE(*, *) 'PavSurf_OHM_COEF_A1_SUMMER_DRY', OHM_coef(PavSurf, 2, 1)
      pavedPrm%ohm%ohm_coef_lc(1)%winter_wet = OHM_coef(PavSurf, 3, 1)
      !WRITE(*, *) 'PavSurf_OHM_COEF_A1_WINTER_WRT', OHM_coef(PavSurf, 3, 1)
      pavedPrm%ohm%ohm_coef_lc(1)%winter_dry = OHM_coef(PavSurf, 4, 1)
      ! WRITE(*, *) 'PavSurf_OHM_COEF_A1_WINTER_DRY', OHM_coef(PavSurf, 4, 1),

      pavedPrm%ohm%ohm_coef_lc(2)%summer_wet = OHM_coef(PavSurf, 1, 2)
      pavedPrm%ohm%ohm_coef_lc(2)%summer_dry = OHM_coef(PavSurf, 2, 2)
      pavedPrm%ohm%ohm_coef_lc(2)%winter_wet = OHM_coef(PavSurf, 3, 2)
      pavedPrm%ohm%ohm_coef_lc(2)%winter_dry = OHM_coef(PavSurf, 4, 2)

      pavedPrm%ohm%ohm_coef_lc(3)%summer_wet = OHM_coef(PavSurf, 1, 3)
      pavedPrm%ohm%ohm_coef_lc(3)%summer_dry = OHM_coef(PavSurf, 2, 3)
      pavedPrm%ohm%ohm_coef_lc(3)%winter_wet = OHM_coef(PavSurf, 3, 3)
      pavedPrm%ohm%ohm_coef_lc(3)%winter_dry = OHM_coef(PavSurf, 4, 3)
      ! WRITE(*,*) 'PavSurf_OHM_COEF_A3', pavedPrm%ohm%ohm_coef_lc(3)

      pavedPrm%soil%soildepth = SoilDepth(PavSurf)
      pavedPrm%soil%soilstorecap = SoilStoreCap_surf(PavSurf)
      pavedPrm%soil%sathydraulicconduct = SatHydraulicConduct(PavSurf)
      pavedPrm%statelimit = StateLimit_surf(PavSurf)
      pavedPrm%irrfracpaved = IrrFracPaved
      pavedPrm%wetthresh = WetThresh_surf(PavSurf)
      ! pavedPrm%storedrainprm%store_min = StoreDrainPrm(1, PavSurf)
      ! pavedPrm%storedrainprm%drain_eq = StoreDrainPrm(2, PavSurf)
      ! pavedPrm%storedrainprm%drain_coef_1 = StoreDrainPrm(3, PavSurf)
      ! pavedPrm%storedrainprm%drain_coef_2 = StoreDrainPrm(4, PavSurf)
      ! pavedPrm%storedrainprm%store_max = StoreDrainPrm(5, PavSurf)
      ! pavedPrm%storedrainprm%store_cap = StoreDrainPrm(6, PavSurf)
      pavedPrm%waterdist%to_paved = WaterDist(1, PavSurf)
      pavedPrm%waterdist%to_bldg = WaterDist(2, PavSurf)
      pavedPrm%waterdist%to_evetr = WaterDist(3, PavSurf)
      pavedPrm%waterdist%to_dectr = WaterDist(4, PavSurf)
      pavedPrm%waterdist%to_grass = WaterDist(5, PavSurf)
      pavedPrm%waterdist%to_bsoil = WaterDist(6, PavSurf)
      pavedPrm%waterdist%to_water = WaterDist(7, PavSurf)
      pavedPrm%waterdist%to_soilstore = WaterDist(8, PavSurf)

      bldgPrm%sfr = sfr_surf(BldgSurf)
      bldgPrm%faibldg = FAIBldg
      bldgPrm%bldgh = bldgH
      bldgPrm%emis = emis(BldgSurf)
      bldgPrm%ohm%chanohm = chAnOHM(BldgSurf)
      bldgPrm%ohm%cpanohm = cpAnOHM(BldgSurf)
      bldgPrm%ohm%kkanohm = kkAnOHM(BldgSurf)
      bldgPrm%ohm%ohm_threshsw = OHM_threshSW(BldgSurf)
      bldgPrm%ohm%ohm_threshwd = OHM_threshWD(BldgSurf)
      bldgPrm%ohm%ohm_coef_lc(1)%summer_wet = OHM_coef(BldgSurf, 1, 1)
      bldgPrm%ohm%ohm_coef_lc(1)%summer_dry = OHM_coef(BldgSurf, 2, 1)
      bldgPrm%ohm%ohm_coef_lc(1)%winter_wet = OHM_coef(BldgSurf, 3, 1)
      bldgPrm%ohm%ohm_coef_lc(1)%winter_dry = OHM_coef(BldgSurf, 4, 1)
      ! WRITE(*,*) 'bldgPrm_OHM_COEF_A1', bldgPrm%ohm%ohm_coef_lc(1)

      bldgPrm%ohm%ohm_coef_lc(2)%summer_wet = OHM_coef(BldgSurf, 1, 2)
      bldgPrm%ohm%ohm_coef_lc(2)%summer_dry = OHM_coef(BldgSurf, 2, 2)
      bldgPrm%ohm%ohm_coef_lc(2)%winter_wet = OHM_coef(BldgSurf, 3, 2)
      bldgPrm%ohm%ohm_coef_lc(2)%winter_dry = OHM_coef(BldgSurf, 4, 2)
      ! WRITE(*,*) 'bldgPrm_OHM_COEF_A2', bldgPrm%ohm%ohm_coef_lc(2)

      bldgPrm%ohm%ohm_coef_lc(3)%summer_wet = OHM_coef(BldgSurf, 1, 3)
      bldgPrm%ohm%ohm_coef_lc(3)%summer_dry = OHM_coef(BldgSurf, 2, 3)
      bldgPrm%ohm%ohm_coef_lc(3)%winter_wet = OHM_coef(BldgSurf, 3, 3)
      bldgPrm%ohm%ohm_coef_lc(3)%winter_dry = OHM_coef(BldgSurf, 4, 3)
      ! WRITE(*,*) 'bldgPrm_OHM_COEF_A3', bldgPrm%ohm%ohm_coef_lc(3)

      bldgPrm%soil%soildepth = SoilDepth(BldgSurf)
      bldgPrm%soil%soilstorecap = SoilStoreCap_surf(BldgSurf)
      bldgPrm%soil%sathydraulicconduct = SatHydraulicConduct(BldgSurf)
      bldgPrm%statelimit = StateLimit_surf(BldgSurf)
      bldgPrm%irrfracbldgs = IrrFracBldgs
      bldgPrm%wetthresh = WetThresh_surf(BldgSurf)
      ! bldgPrm%storedrainprm%store_min = StoreDrainPrm(1, BldgSurf)
      ! bldgPrm%storedrainprm%drain_eq = StoreDrainPrm(2, BldgSurf)
      ! bldgPrm%storedrainprm%drain_coef_1 = StoreDrainPrm(3, BldgSurf)
      ! bldgPrm%storedrainprm%drain_coef_2 = StoreDrainPrm(4, BldgSurf)
      ! bldgPrm%storedrainprm%store_max = StoreDrainPrm(5, BldgSurf)
      ! bldgPrm%storedrainprm%store_cap = StoreDrainPrm(6, BldgSurf)
      bldgPrm%waterdist%to_paved = WaterDist(1, BldgSurf)
      bldgPrm%waterdist%to_bldg = WaterDist(2, BldgSurf)
      bldgPrm%waterdist%to_evetr = WaterDist(3, BldgSurf)
      bldgPrm%waterdist%to_dectr = WaterDist(4, BldgSurf)
      bldgPrm%waterdist%to_grass = WaterDist(5, BldgSurf)
      bldgPrm%waterdist%to_bsoil = WaterDist(6, BldgSurf)
      bldgPrm%waterdist%to_water = WaterDist(7, BldgSurf)
      bldgPrm%waterdist%to_soilstore = WaterDist(8, BldgSurf)

      dectrPrm%sfr = sfr_surf(DecidSurf)
      dectrPrm%emis = emis(DecidSurf)
      dectrPrm%faidectree = FAIDecTree
      dectrPrm%dectreeh = DecTreeH
      dectrPrm%pormin_dec = PorMin_dec
      dectrPrm%pormax_dec = PorMax_dec
      dectrPrm%alb_min = AlbMin_DecTr
      dectrPrm%alb_max = AlbMax_DecTr
      dectrPrm%ohm%chanohm = chAnOHM(DecidSurf)
      dectrPrm%ohm%cpanohm = cpAnOHM(DecidSurf)
      dectrPrm%ohm%kkanohm = kkAnOHM(DecidSurf)
      dectrPrm%ohm%ohm_threshsw = OHM_threshSW(DecidSurf)
      dectrPrm%ohm%ohm_threshwd = OHM_threshWD(DecidSurf)

      dectrPrm%ohm%ohm_coef_lc(1)%summer_wet = OHM_coef(DecidSurf, 1, 1)
      dectrPrm%ohm%ohm_coef_lc(1)%summer_dry = OHM_coef(DecidSurf, 2, 1)
      dectrPrm%ohm%ohm_coef_lc(1)%winter_wet = OHM_coef(DecidSurf, 3, 1)
      dectrPrm%ohm%ohm_coef_lc(1)%winter_dry = OHM_coef(DecidSurf, 4, 1)
      ! WRITE(*,*) 'dectrPrm_OHM_COEF_A1', dectrPrm%ohm%ohm_coef_lc(1)

      dectrPrm%ohm%ohm_coef_lc(2)%summer_wet = OHM_coef(DecidSurf, 1, 2)
      dectrPrm%ohm%ohm_coef_lc(2)%summer_dry = OHM_coef(DecidSurf, 2, 2)
      dectrPrm%ohm%ohm_coef_lc(2)%winter_wet = OHM_coef(DecidSurf, 3, 2)
      dectrPrm%ohm%ohm_coef_lc(2)%winter_dry = OHM_coef(DecidSurf, 4, 2)
      ! WRITE(*,*) 'dectrPrm_OHM_COEF_A2', dectrPrm%ohm%ohm_coef_lc(2)

      dectrPrm%ohm%ohm_coef_lc(3)%summer_wet = OHM_coef(DecidSurf, 1, 3)
      dectrPrm%ohm%ohm_coef_lc(3)%summer_dry = OHM_coef(DecidSurf, 2, 3)
      dectrPrm%ohm%ohm_coef_lc(3)%winter_wet = OHM_coef(DecidSurf, 3, 3)
      dectrPrm%ohm%ohm_coef_lc(3)%winter_dry = OHM_coef(DecidSurf, 4, 3)
      ! WRITE(*,*) 'dectrPrm_OHM_COEF_A3', dectrPrm%ohm%ohm_coef_lc(3)

      dectrPrm%soil%soildepth = SoilDepth(DecidSurf)
      dectrPrm%soil%soilstorecap = SoilStoreCap_surf(DecidSurf)
      dectrPrm%soil%sathydraulicconduct = SatHydraulicConduct(DecidSurf)
      ! dectrPrm%statelimit = StateLimit_surf(DecidSurf)
      dectrPrm%capmax_dec = CapMax_dec
      dectrPrm%capmin_dec = CapMin_dec
      dectrPrm%irrfracdectr = IrrFracDecTr
      dectrPrm%wetthresh = WetThresh_surf(DecidSurf)
      dectrPrm%bioco2%beta_bioco2 = beta_bioCO2(ivDecid)
      dectrPrm%bioco2%beta_enh_bioco2 = beta_enh_bioCO2(ivDecid)
      dectrPrm%bioco2%alpha_bioco2 = alpha_bioCO2(ivDecid)
      dectrPrm%bioco2%alpha_enh_bioco2 = alpha_enh_bioCO2(ivDecid)
      dectrPrm%bioco2%resp_a = resp_a(ivDecid)
      dectrPrm%bioco2%resp_b = resp_b(ivDecid)
      dectrPrm%bioco2%min_res_bioCO2 = min_res_bioCO2(ivDecid)
      dectrPrm%bioco2%theta_bioco2 = theta_bioCO2(ivDecid)
      dectrPrm%maxconductance = MaxConductance(ivDecid)
      dectrPrm%lai%baset = BaseT(ivDecid)
      dectrPrm%lai%gddfull = GDDFull(ivDecid)
      dectrPrm%lai%basete = BaseTe(ivDecid)
      dectrPrm%lai%sddfull = SDDFull(ivDecid)
      dectrPrm%lai%laimin = LAIMin(ivDecid)
      dectrPrm%lai%laimax = LAIMax(ivDecid)
      dectrPrm%lai%laipower = LAIPower(:, ivDecid)
      dectrPrm%lai%laitype = LAIType(ivDecid)
      ! dectrPrm%storedrainprm%store_min = StoreDrainPrm(1, DecidSurf)
      ! dectrPrm%storedrainprm%drain_eq = StoreDrainPrm(2, DecidSurf)
      ! dectrPrm%storedrainprm%drain_coef_1 = StoreDrainPrm(3, DecidSurf)
      ! dectrPrm%storedrainprm%drain_coef_2 = StoreDrainPrm(4, DecidSurf)
      ! dectrPrm%storedrainprm%store_max = StoreDrainPrm(5, DecidSurf)
      ! dectrPrm%storedrainprm%store_cap = StoreDrainPrm(6, DecidSurf)
      dectrPrm%waterdist%to_paved = WaterDist(1, DecidSurf)
      dectrPrm%waterdist%to_bldg = WaterDist(2, DecidSurf)
      dectrPrm%waterdist%to_evetr = WaterDist(3, DecidSurf)
      dectrPrm%waterdist%to_dectr = WaterDist(4, DecidSurf)
      dectrPrm%waterdist%to_grass = WaterDist(5, DecidSurf)
      dectrPrm%waterdist%to_bsoil = WaterDist(6, DecidSurf)
      dectrPrm%waterdist%to_water = WaterDist(7, DecidSurf)
      dectrPrm%waterdist%to_soilstore = WaterDist(8, DecidSurf)

      evetrPrm%sfr = sfr_surf(ConifSurf)
      evetrPrm%emis = emis(ConifSurf)
      evetrPrm%faievetree = FAIEveTree
      evetrPrm%evetreeh = EveTreeH
      evetrPrm%alb_min = AlbMin_EveTr
      evetrPrm%alb_max = AlbMax_EveTr
      evetrPrm%ohm%chanohm = chAnOHM(ConifSurf)
      evetrPrm%ohm%cpanohm = cpAnOHM(ConifSurf)
      evetrPrm%ohm%kkanohm = kkAnOHM(ConifSurf)
      evetrPrm%ohm%ohm_threshsw = OHM_threshSW(ConifSurf)
      evetrPrm%ohm%ohm_threshwd = OHM_threshWD(ConifSurf)
      evetrPrm%ohm%ohm_coef_lc(1)%summer_wet = OHM_coef(ConifSurf, 1, 1)
      evetrPrm%ohm%ohm_coef_lc(1)%summer_dry = OHM_coef(ConifSurf, 2, 1)
      evetrPrm%ohm%ohm_coef_lc(1)%winter_wet = OHM_coef(ConifSurf, 3, 1)
      evetrPrm%ohm%ohm_coef_lc(1)%winter_dry = OHM_coef(ConifSurf, 4, 1)
      ! WRITE(*,*) 'evetrPrm_OHM_COEF_A1', evetrPrm%ohm%ohm_coef_lc(1)

      evetrPrm%ohm%ohm_coef_lc(2)%summer_wet = OHM_coef(ConifSurf, 1, 2)
      evetrPrm%ohm%ohm_coef_lc(2)%summer_dry = OHM_coef(ConifSurf, 2, 2)
      evetrPrm%ohm%ohm_coef_lc(2)%winter_wet = OHM_coef(ConifSurf, 3, 2)
      evetrPrm%ohm%ohm_coef_lc(2)%winter_dry = OHM_coef(ConifSurf, 4, 2)
      ! WRITE(*,*) 'evetrPrm_OHM_COEF_A2', evetrPrm%ohm%ohm_coef_lc(2)

      evetrPrm%ohm%ohm_coef_lc(3)%summer_wet = OHM_coef(ConifSurf, 1, 3)
      evetrPrm%ohm%ohm_coef_lc(3)%summer_dry = OHM_coef(ConifSurf, 2, 3)
      evetrPrm%ohm%ohm_coef_lc(3)%winter_wet = OHM_coef(ConifSurf, 3, 3)
      evetrPrm%ohm%ohm_coef_lc(3)%winter_dry = OHM_coef(ConifSurf, 4, 3)
      ! WRITE(*,*) 'evetrPrm_OHM_COEF_A3', evetrPrm%ohm%ohm_coef_lc(3)

      evetrPrm%soil%soildepth = SoilDepth(ConifSurf)
      evetrPrm%soil%soilstorecap = SoilStoreCap_surf(ConifSurf)
      evetrPrm%soil%sathydraulicconduct = SatHydraulicConduct(ConifSurf)
      evetrPrm%statelimit = StateLimit_surf(ConifSurf)
      evetrPrm%irrfracevetr = IrrFracEveTr
      evetrPrm%wetthresh = WetThresh_surf(ConifSurf)
      evetrPrm%bioco2%beta_bioco2 = beta_bioCO2(ivConif)
      evetrPrm%bioco2%beta_enh_bioco2 = beta_enh_bioCO2(ivConif)
      evetrPrm%bioco2%alpha_bioco2 = alpha_bioCO2(ivConif)
      evetrPrm%bioco2%alpha_enh_bioco2 = alpha_enh_bioCO2(ivConif)
      evetrPrm%bioco2%resp_a = resp_a(ivConif)
      evetrPrm%bioco2%resp_b = resp_b(ivConif)
      evetrPrm%bioco2%min_res_bioCO2 = min_res_bioCO2(ivConif)
      evetrPrm%bioco2%theta_bioco2 = theta_bioCO2(ivConif)
      evetrPrm%maxconductance = MaxConductance(ivConif)
      evetrPrm%lai%baset = BaseT(ivConif)
      evetrPrm%lai%gddfull = GDDFull(ivConif)
      evetrPrm%lai%basete = BaseTe(ivConif)
      evetrPrm%lai%sddfull = SDDFull(ivConif)
      evetrPrm%lai%laimin = LAIMin(ivConif)
      evetrPrm%lai%laimax = LAIMax(ivConif)
      evetrPrm%lai%laipower = LAIPower(:, ivConif)
      evetrPrm%lai%laitype = LAIType(ivConif)
      ! evetrPrm%storedrainprm%store_min = StoreDrainPrm(1, ConifSurf)
      ! evetrPrm%storedrainprm%drain_eq = StoreDrainPrm(2, ConifSurf)
      ! evetrPrm%storedrainprm%drain_coef_1 = StoreDrainPrm(3, ConifSurf)
      ! evetrPrm%storedrainprm%drain_coef_2 = StoreDrainPrm(4, ConifSurf)
      ! evetrPrm%storedrainprm%store_max = StoreDrainPrm(5, ConifSurf)
      ! evetrPrm%storedrainprm%store_cap = StoreDrainPrm(6, ConifSurf)
      evetrPrm%waterdist%to_paved = WaterDist(1, ConifSurf)
      evetrPrm%waterdist%to_bldg = WaterDist(2, ConifSurf)
      evetrPrm%waterdist%to_evetr = WaterDist(3, ConifSurf)
      evetrPrm%waterdist%to_dectr = WaterDist(4, ConifSurf)
      evetrPrm%waterdist%to_grass = WaterDist(5, ConifSurf)
      evetrPrm%waterdist%to_bsoil = WaterDist(6, ConifSurf)
      evetrPrm%waterdist%to_water = WaterDist(7, ConifSurf)
      evetrPrm%waterdist%to_soilstore = WaterDist(8, ConifSurf)

      grassPrm%sfr = sfr_surf(GrassSurf)
      grassPrm%emis = emis(GrassSurf)
      grassPrm%alb_min = AlbMin_Grass
      grassPrm%alb_max = AlbMax_Grass
      grassPrm%ohm%chanohm = chAnOHM(GrassSurf)
      grassPrm%ohm%cpanohm = cpAnOHM(GrassSurf)
      grassPrm%ohm%kkanohm = kkAnOHM(GrassSurf)
      grassPrm%ohm%ohm_threshsw = OHM_threshSW(GrassSurf)
      grassPrm%ohm%ohm_threshwd = OHM_threshWD(GrassSurf)
      grassPrm%ohm%ohm_coef_lc(1)%summer_wet = OHM_coef(GrassSurf, 1, 1)
      grassPrm%ohm%ohm_coef_lc(1)%summer_dry = OHM_coef(GrassSurf, 2, 1)
      grassPrm%ohm%ohm_coef_lc(1)%winter_wet = OHM_coef(GrassSurf, 3, 1)
      grassPrm%ohm%ohm_coef_lc(1)%winter_dry = OHM_coef(GrassSurf, 4, 1)
      !WRITE(*,*) 'grassPrm_OHM_COEF_A1', grassPrm%ohm%ohm_coef_lc(1)

      grassPrm%ohm%ohm_coef_lc(2)%summer_wet = OHM_coef(GrassSurf, 1, 2)
      grassPrm%ohm%ohm_coef_lc(2)%summer_dry = OHM_coef(GrassSurf, 2, 2)
      grassPrm%ohm%ohm_coef_lc(2)%winter_wet = OHM_coef(GrassSurf, 3, 2)
      grassPrm%ohm%ohm_coef_lc(2)%winter_dry = OHM_coef(GrassSurf, 4, 2)
      !WRITE(*,*) 'grassPrm_OHM_COEF_A2', grassPrm%ohm%ohm_coef_lc(2)

      grassPrm%ohm%ohm_coef_lc(3)%summer_wet = OHM_coef(GrassSurf, 1, 3)
      grassPrm%ohm%ohm_coef_lc(3)%summer_dry = OHM_coef(GrassSurf, 2, 3)
      grassPrm%ohm%ohm_coef_lc(3)%winter_wet = OHM_coef(GrassSurf, 3, 3)
      grassPrm%ohm%ohm_coef_lc(3)%winter_dry = OHM_coef(GrassSurf, 4, 3)
      !WRITE(*,*) 'grassPrm_OHM_COEF_A3', grassPrm%ohm%ohm_coef_lc(3)

      grassPrm%soil%soildepth = SoilDepth(GrassSurf)
      grassPrm%soil%soilstorecap = SoilStoreCap_surf(GrassSurf)
      grassPrm%soil%sathydraulicconduct = SatHydraulicConduct(GrassSurf)
      grassPrm%statelimit = StateLimit_surf(GrassSurf)
      grassPrm%irrfracgrass = IrrFracGrass
      grassPrm%wetthresh = WetThresh_surf(GrassSurf)
      grassPrm%bioco2%beta_bioco2 = beta_bioCO2(ivGrass)
      grassPrm%bioco2%beta_enh_bioco2 = beta_enh_bioCO2(ivGrass)
      grassPrm%bioco2%alpha_bioco2 = alpha_bioCO2(ivGrass)
      grassPrm%bioco2%alpha_enh_bioco2 = alpha_enh_bioCO2(ivGrass)
      grassPrm%bioco2%resp_a = resp_a(ivGrass)
      grassPrm%bioco2%resp_b = resp_b(ivGrass)
      grassPrm%bioco2%min_res_bioCO2 = min_res_bioCO2(ivGrass)
      grassPrm%bioco2%theta_bioco2 = theta_bioCO2(ivGrass)
      grassPrm%maxconductance = MaxConductance(ivGrass)
      grassPrm%lai%baset = BaseT(ivGrass)
      grassPrm%lai%gddfull = GDDFull(ivGrass)
      grassPrm%lai%basete = BaseTe(ivGrass)
      grassPrm%lai%sddfull = SDDFull(ivGrass)
      grassPrm%lai%laimin = LAIMin(ivGrass)
      grassPrm%lai%laimax = LAIMax(ivGrass)
      grassPrm%lai%laipower = LAIPower(:, ivGrass)
      grassPrm%lai%laitype = LAIType(ivGrass)
      ! grassPrm%storedrainprm%store_min = StoreDrainPrm(1, GrassSurf)
      ! grassPrm%storedrainprm%drain_eq = StoreDrainPrm(2, GrassSurf)
      ! grassPrm%storedrainprm%drain_coef_1 = StoreDrainPrm(3, GrassSurf)
      ! grassPrm%storedrainprm%drain_coef_2 = StoreDrainPrm(4, GrassSurf)
      ! grassPrm%storedrainprm%store_max = StoreDrainPrm(5, GrassSurf)
      ! grassPrm%storedrainprm%store_cap = StoreDrainPrm(6, GrassSurf)
      grassPrm%waterdist%to_paved = WaterDist(1, GrassSurf)
      grassPrm%waterdist%to_bldg = WaterDist(2, GrassSurf)
      grassPrm%waterdist%to_evetr = WaterDist(3, GrassSurf)
      grassPrm%waterdist%to_dectr = WaterDist(4, GrassSurf)
      grassPrm%waterdist%to_grass = WaterDist(5, GrassSurf)
      grassPrm%waterdist%to_bsoil = WaterDist(6, GrassSurf)
      grassPrm%waterdist%to_water = WaterDist(7, GrassSurf)
      grassPrm%waterdist%to_soilstore = WaterDist(8, GrassSurf)

      bsoilPrm%sfr = sfr_surf(BSoilSurf)
      bsoilPrm%emis = emis(BSoilSurf)
      bsoilPrm%ohm%chanohm = chAnOHM(BSoilSurf)
      bsoilPrm%ohm%cpanohm = cpAnOHM(BSoilSurf)
      bsoilPrm%ohm%kkanohm = kkAnOHM(BSoilSurf)
      bsoilPrm%ohm%ohm_threshsw = OHM_threshSW(BSoilSurf)
      bsoilPrm%ohm%ohm_threshwd = OHM_threshWD(BSoilSurf)
      bsoilPrm%ohm%ohm_coef_lc(1)%summer_wet = OHM_coef(BSoilSurf, 1, 1)
      bsoilPrm%ohm%ohm_coef_lc(1)%summer_dry = OHM_coef(BSoilSurf, 2, 1)
      bsoilPrm%ohm%ohm_coef_lc(1)%winter_wet = OHM_coef(BSoilSurf, 3, 1)
      bsoilPrm%ohm%ohm_coef_lc(1)%winter_dry = OHM_coef(BSoilSurf, 4, 1)
      !WRITE(*,*) 'bsoilPrm_OHM_COEF_A1', bsoilPrm%ohm%ohm_coef_lc(1)

      bsoilPrm%ohm%ohm_coef_lc(2)%summer_wet = OHM_coef(BSoilSurf, 1, 2)
      bsoilPrm%ohm%ohm_coef_lc(2)%summer_dry = OHM_coef(BSoilSurf, 2, 2)
      bsoilPrm%ohm%ohm_coef_lc(2)%winter_wet = OHM_coef(BSoilSurf, 3, 2)
      bsoilPrm%ohm%ohm_coef_lc(2)%winter_dry = OHM_coef(BSoilSurf, 4, 2)
      !WRITE(*,*) 'bsoilPrm_OHM_COEF_A2', bsoilPrm%ohm%ohm_coef_lc(2)

      bsoilPrm%ohm%ohm_coef_lc(3)%summer_wet = OHM_coef(BSoilSurf, 1, 3)
      bsoilPrm%ohm%ohm_coef_lc(3)%summer_dry = OHM_coef(BSoilSurf, 2, 3)
      bsoilPrm%ohm%ohm_coef_lc(3)%winter_wet = OHM_coef(BSoilSurf, 3, 3)
      bsoilPrm%ohm%ohm_coef_lc(3)%winter_dry = OHM_coef(BSoilSurf, 4, 3)
      !WRITE(*,*) 'bsoilPrm_OHM_COEF_A3', bsoilPrm%ohm%ohm_coef_lc(3)

      bsoilPrm%soil%soildepth = SoilDepth(BSoilSurf)
      bsoilPrm%soil%soilstorecap = SoilStoreCap_surf(BSoilSurf)
      bsoilPrm%soil%sathydraulicconduct = SatHydraulicConduct(BSoilSurf)
      bsoilPrm%statelimit = StateLimit_surf(BSoilSurf)
      bsoilPrm%irrfracbsoil = IrrFracBSoil
      bsoilPrm%wetthresh = WetThresh_surf(BSoilSurf)
      ! bsoilPrm%storedrainprm%store_min = StoreDrainPrm(1, BSoilSurf)
      ! bsoilPrm%storedrainprm%drain_eq = StoreDrainPrm(2, BSoilSurf)
      ! bsoilPrm%storedrainprm%drain_coef_1 = StoreDrainPrm(3, BSoilSurf)
      ! bsoilPrm%storedrainprm%drain_coef_2 = StoreDrainPrm(4, BSoilSurf)
      ! bsoilPrm%storedrainprm%store_max = StoreDrainPrm(5, BSoilSurf)
      ! bsoilPrm%storedrainprm%store_cap = StoreDrainPrm(6, BSoilSurf)
      bsoilPrm%waterdist%to_paved = WaterDist(1, BSoilSurf)
      bsoilPrm%waterdist%to_bldg = WaterDist(2, BSoilSurf)
      bsoilPrm%waterdist%to_evetr = WaterDist(3, BSoilSurf)
      bsoilPrm%waterdist%to_dectr = WaterDist(4, BSoilSurf)
      bsoilPrm%waterdist%to_grass = WaterDist(5, BSoilSurf)
      bsoilPrm%waterdist%to_bsoil = WaterDist(6, BSoilSurf)
      bsoilPrm%waterdist%to_water = WaterDist(7, BSoilSurf)
      bsoilPrm%waterdist%to_soilstore = WaterDist(8, BSoilSurf)

      waterPrm%sfr = sfr_surf(WaterSurf)
      waterPrm%emis = emis(WaterSurf)
      waterPrm%ohm%chanohm = chAnOHM(WaterSurf)
      waterPrm%ohm%cpanohm = cpAnOHM(WaterSurf)
      waterPrm%ohm%kkanohm = kkAnOHM(WaterSurf)
      waterPrm%ohm%ohm_threshsw = OHM_threshSW(WaterSurf)
      waterPrm%ohm%ohm_threshwd = OHM_threshWD(WaterSurf)
      waterPrm%ohm%ohm_coef_lc(1)%summer_wet = OHM_coef(WaterSurf, 1, 1)
      waterPrm%ohm%ohm_coef_lc(1)%summer_dry = OHM_coef(WaterSurf, 2, 1)
      waterPrm%ohm%ohm_coef_lc(1)%winter_wet = OHM_coef(WaterSurf, 3, 1)
      waterPrm%ohm%ohm_coef_lc(1)%winter_dry = OHM_coef(WaterSurf, 4, 1)
      !WRITE(*,*) 'waterPrm_OHM_COEF_A1', waterPrm%ohm%ohm_coef_lc(1)

      waterPrm%ohm%ohm_coef_lc(2)%summer_wet = OHM_coef(WaterSurf, 1, 2)
      waterPrm%ohm%ohm_coef_lc(2)%summer_dry = OHM_coef(WaterSurf, 2, 2)
      waterPrm%ohm%ohm_coef_lc(2)%winter_wet = OHM_coef(WaterSurf, 3, 2)
      waterPrm%ohm%ohm_coef_lc(2)%winter_dry = OHM_coef(WaterSurf, 4, 2)
      !WRITE(*,*) 'waterPrm_OHM_COEF_A2', waterPrm%ohm%ohm_coef_lc(2)

      waterPrm%ohm%ohm_coef_lc(3)%summer_wet = OHM_coef(WaterSurf, 1, 3)
      waterPrm%ohm%ohm_coef_lc(3)%summer_dry = OHM_coef(WaterSurf, 2, 3)
      waterPrm%ohm%ohm_coef_lc(3)%winter_wet = OHM_coef(WaterSurf, 3, 3)
      waterPrm%ohm%ohm_coef_lc(3)%winter_dry = OHM_coef(WaterSurf, 4, 3)
      !WRITE(*,*) 'waterPrm_OHM_COEF_A3', waterPrm%ohm%ohm_coef_lc(3)

      waterPrm%soil%soildepth = SoilDepth(WaterSurf)
      waterPrm%soil%soilstorecap = SoilStoreCap_surf(WaterSurf)
      waterPrm%soil%sathydraulicconduct = SatHydraulicConduct(WaterSurf)
      waterPrm%statelimit = StateLimit_surf(WaterSurf)
      waterPrm%irrfracwater = IrrFracWater
      ! waterPrm%wetthresh = WetThresh_surf(WaterSurf)
      ! waterPrm%storedrainprm%store_min = StoreDrainPrm(1, WaterSurf)
      ! waterPrm%storedrainprm%drain_eq = StoreDrainPrm(2, WaterSurf)
      ! waterPrm%storedrainprm%drain_coef_1 = StoreDrainPrm(3, WaterSurf)
      ! waterPrm%storedrainprm%drain_coef_2 = StoreDrainPrm(4, WaterSurf)
      ! waterPrm%storedrainprm%store_max = StoreDrainPrm(5, WaterSurf)
      ! waterPrm%storedrainprm%store_cap = StoreDrainPrm(6, WaterSurf)

      ! ********** SUEWS_stateVariables **********
      anthroHeatState%HDD_id = HDD_id

      ! ESTM_ehc related:
      ! water balance related:
      ALLOCATE (hydroState%soilstore_roof(nlayer))
      ALLOCATE (hydroState%state_roof(nlayer))
      ALLOCATE (hydroState%soilstore_wall(nlayer))
      ALLOCATE (hydroState%state_wall(nlayer))
      hydroState%soilstore_roof = soilstore_roof
      hydroState%state_roof = state_roof
      hydroState%soilstore_wall = soilstore_wall
      hydroState%state_wall = state_wall
      hydroState%soilstore_surf = soilstore_surf
      hydroState%state_surf = state_surf
      hydroState%WUDay_id = WUDay_id

      ALLOCATE (heatState%temp_roof(nlayer, ndepth))
      ALLOCATE (heatState%temp_wall(nlayer, ndepth))
      ALLOCATE (heatState%tsfc_roof(nlayer))
      ALLOCATE (heatState%tsfc_wall(nlayer))
      ALLOCATE (heatState%tsfc_surf(nsurf))
      ALLOCATE (heatState%temp_surf(nsurf, ndepth))
      heatState%temp_roof = temp_roof
      heatState%temp_wall = temp_wall
      heatState%temp_surf = temp_surf
      heatState%tsfc_roof = tsfc_roof
      heatState%tsfc_wall = tsfc_wall
      heatState%tsfc_surf = tsfc_surf
      heatState%temp_surf = temp_surf

      ! OHM related:
      ohmState%qn_av = qn_av
      ohmState%dqndt = dqndt
      ohmState%qn_s_av = qn_s_av
      ohmState%dqnsdt = dqnsdt

      ! snow related:
      snowState%snowfallCum = SnowfallCum
      snowState%snowalb = SnowAlb
      snowState%icefrac = IceFrac
      snowState%snowdens = SnowDens
      snowState%snowfrac = SnowFrac
      snowState%snowpack = SnowPack
      snowState%snowwater = SnowWater

      ! phenology related:
      phenState%alb = alb
      phenState%lai_id = LAI_id
      phenState%SDD_id = SDD_id
      phenState%GDD_id = GDD_id
      phenState%porosity_id = porosity_id
      phenState%decidcap_id = DecidCap_id
      phenState%albDecTr_id = albDecTr_id
      phenState%albEveTr_id = albEveTr_id
      phenState%albGrass_id = albGrass_id
      phenState%Tmin_id = Tmin_id
      phenState%Tmax_id = Tmax_id
      phenState%lenDay_id = lenDay_id
      phenState%StoreDrainPrm = StoreDrainPrm

      ! ########################################3
      ! set initial values for output arrays
      SWE = 0.
      mwh = 0.
      MwStore = 0.
      chSnow_per_interval = 0.
      SnowRemoval = 0.
      Qm = 0
      QmFreez = 0
      QmRain = 0

      ! these output variables are used for debugging
      qe0_surf = 0 ! QE from PM: only meaningful when snowuse=0
      ev0_surf = 0 ! ev from PM: only meaningful when snowuse=0
      ev_surf = 0 ! ev from water balance: only meaningful when snowuse=0

      ! ####
      ! force several snow related state variables to zero if snow module is off
      IF (methodPrm%snowuse == 0) THEN
         snowState%SnowDens = 0.
         snowState%SnowFrac = 0.
         snowState%SnowWater = 0.
         snowState%SnowAlb = 0.
         snowState%IceFrac = 0.
         snowState%SnowPack = 0.
      END IF

      ! ########################################################################################
      ! save initial values of inout variables
      ohmState_prev = ohmState

      snowState_prev = snowState
      snowState_prev%snowfrac = MERGE(forcing%snowfrac, snowState%SnowFrac, methodPrm%NetRadiationMethod == 0)

      hydroState_prev = hydroState
      ! IF (methodPrm%StorageHeatMethod == 5) THEN
      !    state_roof_prev = ehcState%state_roof
      !    state_wall_prev = ehcState%state_wall
      !    soilstore_roof_prev = ehcState%soilstore_roof
      !    soilstore_wall_prev = ehcState%soilstore_wall
      ! END IF
      ! Tair_av_prev = Tair_av
      Tair_av_prev = forcing%Tair
      phenState_prev = phenState
      anthroHeatState_prev = anthroHeatState

      ! ESTM_ehc related
      ! save initial values of inout variables
      heatState_in = heatState
      ! IF (StorageHeatMethod == 5) THEN
      !    temp_in_roof = temp_roof
      !    temp_in_wall = temp_wall
      !    temp_in_surf = temp_surf
      ! END IF
      ! initialise indoor/bottom boundary temperature arrays
      ! tin_roof = 10.
      ! tin_wall = 10.
      ! tin_surf = 3.

      ! initialise  variables
      ohmState_next = ohmState
      snowState_next = snowState
      hydroState_next = hydroState
      ! state_surf_next = state_surf
      ! soilstore_surf_next = soilstore_surf

      hydroState_next = hydroState
      ! IF (StorageHeatMethod == 5) THEN

      !    soilstore_roof_next = soilstore_roof
      !    soilstore_wall_next = soilstore_wall
      !    state_roof_next = state_roof
      !    state_wall_next = state_wall

      ! END IF

      ! Tair_av_next = Tair_av
      Tair_av_next = forcing%Tair
      phenState_next = phenState
      anthroHeatState_next = anthroHeatState

      ! initialise output variables
      dataOutLineSnow = -999.
      dataOutLineESTM = -999.
      dataOutLineESTMExt = -999.
      dataoutLineRSL = -999.
      dataOutLineBEERS = -999.
      dataOutLineDebug = -999.
      dataOutLineSPARTACUS = -999.
      dataOutLineDailyState = -999.

      !########################################################################################
      !           main calculation starts here
      !########################################################################################

      ! iteration is used below to get results converge
      flag_converge = .FALSE.
      Ts_iter = TEMP_C

      heatState_out = heatState
      tsfc0_out_surf = heatState%tsfc_surf
      ! tsfc_out_surf = tsfc_surf
      ! tsfc0_out_surf = tsfc_surf
      ! ! TODO: ESTM work: to allow heterogeneous surface temperatures
      IF (StorageHeatMethod == 5 .OR. NetRadiationMethod > 1000) THEN
         tsfc0_out_roof = heatState%tsfc_roof
         tsfc0_out_wall = heatState%tsfc_wall
      END IF
      ! IF (StorageHeatMethod == 5 .OR. NetRadiationMethod > 1000) THEN
      !    tsfc_out_roof = tsfc_roof
      !    tsfc0_out_roof = tsfc_roof
      !    tsfc_out_wall = tsfc_wall
      !    tsfc0_out_wall = tsfc_wall
      ! END IF
      ! PRINT *, 'sfr_surf for this grid ', sfr_surf
      ! PRINT *, 'before iteration Ts_iter = ', Ts_iter
      ! L_mod_iter = 10
      i_iter = 1
      max_iter = 30
      DO WHILE ((.NOT. flag_converge) .AND. i_iter < max_iter)
         ! PRINT *, '=========================== '
         ! PRINT *, 'Ts_iter of ', i_iter, ' is:', Ts_iter

         ! calculate dectime
         ! CALL SUEWS_cal_dectime( &
         !    id, it, imin, isec, & ! input
         !    dectime) ! output
         CALL SUEWS_cal_dectime( &
            timer%id, timer%it, timer%imin, timer%isec, & ! input
            dectime) ! output

         ! calculate tstep related VARIABLES
         ! CALL SUEWS_cal_tstep( &
         !    tstep, & ! input
         !    nsh, nsh_real, tstep_real) ! output
         CALL SUEWS_cal_tstep( &
            timer%tstep, & ! input
            nsh, nsh_real, tstep_real) ! output

         ! calculate surface fraction related VARIABLES
         ! CALL SUEWS_cal_surf( &
         !    StorageHeatMethod, NetRadiationMethod, & !input
         !    nlayer, sfr_surf, & !input
         !    building_frac, building_scale, height, & !input
         !    VegFraction, ImpervFraction, PervFraction, NonWaterFraction, & ! output
         !    sfr_roof, sfr_wall) ! output
         CALL SUEWS_cal_surf_DTS( &
            methodPrm%StorageHeatMethod, methodPrm%NetRadiationMethod, & !input
            nlayer, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, & !input
            spartacusLayerPrm%building_frac, spartacusLayerPrm%building_scale, spartacusPrm%height, & !input
            VegFraction, ImpervFraction, PervFraction, NonWaterFraction, & ! output
            sfr_roof, sfr_wall) ! output

         ! calculate dayofweek information
         ! CALL SUEWS_cal_weekday( &
         !    iy, id, lat, & !input
         !    dayofWeek_id) !output
         CALL SUEWS_cal_weekday( &
            timer%iy, timer%id, siteInfo%lat, & !input
            dayofWeek_id) !output

         ! calculate dayofweek information
         ! CALL SUEWS_cal_DLS( &
         !    id, startDLS, endDLS, & !input
         !    DLS) !output
         CALL SUEWS_cal_DLS( &
            timer%id, ahemisPrm%startDLS, ahemisPrm%endDLS, & !input
            DLS) !output

         ! calculate mean air temperature of past 24 hours
         ! Tair_av_next = cal_tair_av(Tair_av_prev, dt_since_start, tstep, temp_c)
         Tair_av_next = cal_tair_av(Tair_av_prev, timer%dt_since_start, timer%tstep, forcing%temp_c)

         !==============main calculation start=======================

         !==============surface roughness calculation=======================
         IF (methodPrm%Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_RoughnessParameters...'
         IF (methodPrm%Diagnose == 1) PRINT *, 'z0m_in =', z0m_in
         ! CALL SUEWS_cal_RoughnessParameters( &
         !    RoughLenMomMethod, sfr_surf, & !input
         !    bldgH, EveTreeH, DecTreeH, &
         !    porosity_id_prev, FAIBldg, FAIEveTree, FAIDecTree, &
         !    z0m_in, zdm_in, Z, &
         !    FAI, PAI, & !output
         !    zH, z0m, zdm, ZZD)
         CALL SUEWS_cal_RoughnessParameters_DTS( &
            methodPrm%RoughLenMomMethod, &
            methodPrm%FAIMethod, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, & !input
            siteInfo%SurfaceArea, &
            bldgPrm%bldgH, evetrPrm%EveTreeH, dectrPrm%DecTreeH, &
            phenState_prev%porosity_id, bldgPrm%FAIBldg, evetrPrm%FAIEveTree, dectrPrm%FAIDecTree, &
            siteInfo%z0m_in, siteInfo%zdm_in, siteInfo%Z, &
            FAIBldg_use, FAIEveTree_use, FAIDecTree_use, & ! output:
            FAI, PAI, & !output
            zH, z0m, zdm, ZZD)
         ! print *, 'day =', timer%id, 'hour =', timer%it, 'porosity_id = ', phenState_prev%porosity_id

         !=================Calculate sun position=================
         IF (methodPrm%Diagnose == 1) WRITE (*, *) 'Calling NARP_cal_SunPosition...'
         ! CALL NARP_cal_SunPosition( &
         !    REAL(iy, KIND(1D0)), & !input:
         !    dectime - tstep/2/86400, & ! sun position at middle of timestep before
         !    timezone, lat, lng, alt, &
         !    azimuth, zenith_deg) !output:
         CALL NARP_cal_SunPosition( &
            REAL(timer%iy, KIND(1D0)), & !input:
            dectime - timer%tstep/2/86400, & ! sun position at middle of timestep before
            siteInfo%timezone, siteInfo%lat, siteInfo%lon, siteInfo%alt, &
            azimuth, zenith_deg) !output:

         !=================Call the SUEWS_cal_DailyState routine to get surface characteristics ready=================
         IF (methodPrm%Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_DailyState...'
         ! CALL SUEWS_cal_DailyState( &
         !    iy, id, it, imin, isec, tstep, tstep_prev, dt_since_start, DayofWeek_id, & !input
         !    Tmin_id_prev, Tmax_id_prev, lenDay_id_prev, &
         !    BaseTMethod, &
         !    WaterUseMethod, Ie_start, Ie_end, &
         !    LAImethod, LAIType, &
         !    nsh_real, kdown, Temp_C, Precip, BaseT_HC, &
         !    BaseT_Heating, BaseT_Cooling, &
         !    lat, Faut, LAI_obs, &
         !    AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
         !    AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
         !    CapMax_dec, CapMin_dec, PorMax_dec, PorMin_dec, &
         !    Ie_a, Ie_m, DayWatPer, DayWat, &
         !    BaseT, BaseTe, GDDFull, SDDFull, LAIMin, LAIMax, LAIPower, &
         !    DecidCap_id_prev, StoreDrainPrm_prev, LAI_id_prev, GDD_id_prev, SDD_id_prev, &
         !    albDecTr_id_prev, albEveTr_id_prev, albGrass_id_prev, porosity_id_prev, & !input
         !    HDD_id_prev, & !input
         !    state_surf_prev, soilstore_surf_prev, SoilStoreCap_surf, H_maintain, & !input
         !    HDD_id_next, & !output
         !    Tmin_id_next, Tmax_id_next, lenDay_id_next, &
         !    albDecTr_id_next, albEveTr_id_next, albGrass_id_next, porosity_id_next, & !output
         !    DecidCap_id_next, StoreDrainPrm_next, LAI_id_next, GDD_id_next, SDD_id_next, WUDay_id_next) !output

         !!! Do we need to separate the phenology parameters from the land cover parameters?
         CALL SUEWS_cal_DailyState_DTS( &
            timer%iy, timer%id, timer%it, timer%imin, timer%isec, &
            timer%tstep, timer%tstep_prev, timer%dt_since_start, DayofWeek_id, & !input
            phenState_prev%Tmin_id, phenState_prev%Tmax_id, phenState_prev%lenDay_id, &
            BaseTMethod, &
            methodPrm%WaterUseMethod, irrPrm%Ie_start, irrPrm%Ie_end, &
            LAImethod, &
            evetrPrm%lai%laitype, dectrPrm%lai%laitype, grassPrm%lai%laitype, &
            nsh_real, forcing%kdown, forcing%Temp_C, forcing%pres, BaseT_HC, &
            ahemisPrm%anthroheat%BaseT_Heating_working, ahemisPrm%anthroheat%BaseT_Heating_holiday, &
            ahemisPrm%anthroheat%BaseT_Cooling_working, ahemisPrm%anthroheat%BaseT_Cooling_holiday, &
            siteInfo%lat, irrPrm%Faut, forcing%LAI_obs, &
            evetrPrm%Alb_Max, dectrPrm%Alb_Max, grassPrm%Alb_Max, &
            evetrPrm%Alb_Min, dectrPrm%Alb_Min, grassPrm%Alb_Min, &
            dectrPrm%CapMax_dec, dectrPrm%CapMin_dec, dectrPrm%PorMax_dec, dectrPrm%PorMin_dec, &
            irrPrm%Ie_a, irrPrm%Ie_m, &
            irrPrm%irr_daywater%monday_percent, irrPrm%irr_daywater%tuesday_percent, &
            irrPrm%irr_daywater%wednesday_percent, irrPrm%irr_daywater%thursday_percent, &
            irrPrm%irr_daywater%friday_percent, irrPrm%irr_daywater%saturday_percent, &
            irrPrm%irr_daywater%sunday_percent, &
            irrPrm%irr_daywater%monday_flag, irrPrm%irr_daywater%tuesday_flag, irrPrm%irr_daywater%wednesday_flag, &
            irrPrm%irr_daywater%thursday_flag, irrPrm%irr_daywater%friday_flag, irrPrm%irr_daywater%saturday_flag, &
            irrPrm%irr_daywater%sunday_flag, &
            evetrPrm%lai%BaseT, dectrPrm%lai%BaseT, grassPrm%lai%BaseT, & ! LAI-related input
            evetrPrm%lai%BaseTe, dectrPrm%lai%BaseTe, grassPrm%lai%BaseTe, &
            evetrPrm%lai%gddfull, dectrPrm%lai%gddfull, grassPrm%lai%gddfull, &
            evetrPrm%lai%sddfull, dectrPrm%lai%sddfull, grassPrm%lai%sddfull, &
            evetrPrm%lai%laimin, dectrPrm%lai%laimin, grassPrm%lai%laimin, &
            evetrPrm%lai%laimax, dectrPrm%lai%laimax, grassPrm%lai%laimax, &
            evetrPrm%lai%laipower, dectrPrm%lai%laipower, grassPrm%lai%laipower, &
            phenState_prev%DecidCap_id, phenState_prev%StoreDrainPrm, &
            phenState_prev%LAI_id, phenState_prev%GDD_id, phenState_prev%SDD_id, &
            phenState_prev%albDecTr_id, phenState_prev%albEveTr_id, phenState_prev%albGrass_id, phenState_prev%porosity_id, & !input
            anthroHeatState_prev%HDD_id, & !input
            hydroState_prev%state_surf, hydroState_prev%soilstore_surf, SoilStoreCap_surf, irrPrm%H_maintain, & !input
            anthroHeatState_next%HDD_id, & !output
            phenState_next%Tmin_id, phenState_next%Tmax_id, phenState_next%lenDay_id, &
            phenState_next%albDecTr_id, phenState_next%albEveTr_id, phenState_next%albGrass_id, phenState_next%porosity_id, & !output
            phenState_next%DecidCap_id, phenState_next%StoreDrainPrm, phenState_next%LAI_id, &
            phenState_next%GDD_id, phenState_next%SDD_id, &
            hydroState_next%WUDay_id) !output

         !=================Calculation of density and other water related parameters=================
         IF (methodPrm%Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_AtmMoist...'
         ! CALL cal_AtmMoist( &
         !    Temp_C, Press_hPa, avRh, dectime, & ! input:
         !    lv_J_kg, lvS_J_kg, & ! output:
         !    es_hPa, Ea_hPa, VPd_hpa, VPD_Pa, dq, dens_dry, avcp, avdens)
         CALL cal_AtmMoist( &
            forcing%Temp_C, forcing%pres, forcing%RH, dectime, & ! input:
            lv_J_kg, lvS_J_kg, & ! output:
            es_hPa, Ea_hPa, VPd_hpa, VPD_Pa, dq, dens_dry, avcp, avdens)

         !======== Calculate soil moisture =========
         IF (methodPrm%Diagnose == 1) WRITE (*, *) 'Calling SUEWS_update_SoilMoist...'
         ! CALL SUEWS_update_SoilMoist( &
         !    NonWaterFraction, & !input
         !    SoilStoreCap_surf, sfr_surf, soilstore_surf_prev, &
         !    SoilMoistCap, SoilState, & !output
         !    vsmd, smd)
         CALL SUEWS_update_SoilMoist_DTS( &
            NonWaterFraction, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
            pavedPrm%soil%soilstorecap, bldgPrm%soil%soilstorecap, &
            evetrPrm%soil%soilstorecap, dectrPrm%soil%soilstorecap, &
            grassPrm%soil%soilstorecap, bsoilPrm%soil%soilstorecap, waterPrm%soil%soilstorecap, & !input
            hydroState_prev%soilstore_surf, &
            SoilMoistCap, SoilState, & !output
            vsmd, smd)

         IF (methodPrm%Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_WaterUse...'
         !=================Gives the external and internal water uses per timestep=================
         ! CALL SUEWS_cal_WaterUse( &
         !    nsh_real, & ! input:
         !    wu_m3, SurfaceArea, sfr_surf, &
         !    IrrFracPaved, IrrFracBldgs, &
         !    IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
         !    IrrFracBSoil, IrrFracWater, &
         !    DayofWeek_id, WUProfA_24hr, WUProfM_24hr, &
         !    InternalWaterUse_h, HDD_id_next, WUDay_id_next, &
         !    WaterUseMethod, NSH, it, imin, DLS, &
         !    wu_surf, wu_int, wu_ext) ! output:
         CALL SUEWS_cal_WaterUse_DTS( &
            nsh_real, & ! input:
            wu_m3, siteInfo%SurfaceArea, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
            pavedPrm%IrrFracPaved, bldgPrm%IrrFracBldgs, &
            evetrPrm%IrrFracEveTr, dectrPrm%IrrFracDecTr, grassPrm%IrrFracGrass, &
            bsoilPrm%IrrFracBSoil, waterPrm%IrrFracWater, &
            DayofWeek_id, &
            irrPrm%wuprofa_24hr_working, irrPrm%wuprofa_24hr_holiday, &
            irrPrm%wuprofm_24hr_working, irrPrm%wuprofm_24hr_holiday, &
            irrPrm%InternalWaterUse_h, &
            anthroHeatState_next%HDD_id, hydroState_next%WUDay_id, & ! output:
            methodPrm%WaterUseMethod, NSH, timer%it, timer%imin, DLS, &
            wu_surf, wu_int, wu_ext)

         ! ===================ANTHROPOGENIC HEAT AND CO2 FLUX======================
         ! CALL SUEWS_cal_AnthropogenicEmission( &
         !    AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, AH_SLOPE_Heating, CO2PointSource, & ! input:
         !    dayofWeek_id, DLS, EF_umolCO2perJ, EmissionsMethod, EnEF_v_Jkm, &
         !    FcEF_v_kgkm, FrFossilFuel_Heat, FrFossilFuel_NonHeat, HDD_id_next, HumActivity_24hr, &
         !    imin, it, MaxFCMetab, MaxQFMetab, MinFCMetab, MinQFMetab, &
         !    PopDensDaytime, PopDensNighttime, PopProf_24hr, QF, QF0_BEU, Qf_A, Qf_B, Qf_C, &
         !    QF_obs, QF_SAHP, SurfaceArea, BaseT_Cooling, BaseT_Heating, &
         !    Temp_C, TrafficRate, TrafficUnits, TraffProf_24hr, &
         !    Fc_anthro, Fc_build, Fc_metab, Fc_point, Fc_traff) ! output:

         CALL SUEWS_cal_AnthropogenicEmission_DTS( &
            ahemisPrm%anthroheat%ah_min_working, ahemisPrm%anthroheat%ah_min_holiday, &
            ahemisPrm%anthroheat%ahprof_24hr_working, ahemisPrm%anthroheat%ahprof_24hr_holiday, &
            ahemisPrm%anthroheat%ah_slope_cooling_working, ahemisPrm%anthroheat%ah_slope_cooling_holiday, &
            ahemisPrm%anthroheat%ah_slope_heating_working, ahemisPrm%anthroheat%ah_slope_heating_holiday, &
            siteInfo%CO2PointSource, & ! input:
            dayofWeek_id, DLS, ahemisPrm%EF_umolCO2perJ, methodPrm%EmissionsMethod, ahemisPrm%EnEF_v_Jkm, &
            ahemisPrm%FcEF_v_kgkm, ahemisPrm%FrFossilFuel_Heat, ahemisPrm%FrFossilFuel_NonHeat, &
            anthroHeatState_next%HDD_id, &
            ahemisPrm%HumActivity_24hr_working, ahemisPrm%HumActivity_24hr_holiday, &
            timer%imin, timer%it, ahemisPrm%MaxFCMetab, ahemisPrm%MaxQFMetab, ahemisPrm%MinFCMetab, ahemisPrm%MinQFMetab, &
            ahemisPrm%anthroheat%popdensdaytime_working, ahemisPrm%anthroheat%popdensdaytime_holiday, &
            ahemisPrm%anthroheat%popdensnighttime, &
            ahemisPrm%anthroheat%popprof_24hr_working, ahemisPrm%anthroheat%popprof_24hr_holiday, &
            QF, &
            ahemisPrm%anthroheat%qf0_beu_working, ahemisPrm%anthroheat%qf0_beu_holiday, &
            ahemisPrm%anthroheat%qf_a_working, ahemisPrm%anthroheat%qf_a_holiday, &
            ahemisPrm%anthroheat%qf_b_working, ahemisPrm%anthroheat%qf_b_holiday, &
            ahemisPrm%anthroheat%qf_c_working, ahemisPrm%anthroheat%qf_c_holiday, &
            forcing%QF_obs, QF_SAHP, siteInfo%SurfaceArea, &
            ahemisPrm%anthroheat%baset_cooling_working, ahemisPrm%anthroheat%baset_cooling_holiday, &
            ahemisPrm%anthroheat%baset_heating_working, ahemisPrm%anthroheat%baset_heating_holiday, &
            forcing%Temp_C, ahemisPrm%TrafficRate_working, ahemisPrm%TrafficRate_holiday, &
            ahemisPrm%TrafficUnits, &
            ahemisPrm%TraffProf_24hr_working, ahemisPrm%TraffProf_24hr_holiday, &
            Fc_anthro, Fc_build, Fc_metab, Fc_point, Fc_traff) ! output:

         ! ========================================================================
         ! N.B.: the following parts involves snow-related calculations.
         ! ===================NET ALLWAVE RADIATION================================
         ! if (kdown>0 .and. i_iter == 1) then
         !    print *, 'snowFrac_prev=', snowFrac_prev
         !    snowFrac_prev=-999
         !    print *, 'snowFrac_prev=', snowFrac_prev
         ! endif
         ! CALL SUEWS_cal_Qn( &
         !    StorageHeatMethod, NetRadiationMethod, SnowUse, & !input
         !    tstep, nlayer, SnowPack_prev, tau_a, tau_f, SnowAlbMax, SnowAlbMin, &
         !    Diagnose, ldown_obs, fcld_obs, &
         !    dectime, ZENITH_deg, Ts_iter, kdown, Temp_C, avRH, ea_hPa, qn1_obs, &
         !    SnowAlb_prev, snowFrac_prev, DiagQN, &
         !    NARP_TRANS_SITE, NARP_EMIS_SNOW, IceFrac_prev, &
         !    sfr_surf, sfr_roof, sfr_wall, &
         !    tsfc_out_surf, tsfc_out_roof, tsfc_out_wall, &
         !    emis, alb_prev, albDecTr_id_next, albEveTr_id_next, albGrass_id_next, &
         !    LAI_id, & !input
         !    n_vegetation_region_urban, &
         !    n_stream_sw_urban, n_stream_lw_urban, &
         !    sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
         !    veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
         !    veg_fsd_const, veg_contact_fraction_const, &
         !    ground_albedo_dir_mult_fact, use_sw_direct_albedo, & !input
         !    height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
         !    alb_roof, emis_roof, alb_wall, emis_wall, &
         !    roof_albedo_dir_mult_fact, wall_specular_frac, &
         !    alb_next, ldown, fcld, & !output
         !    QN_surf, QN_roof, QN_wall, &
         !    qn, qn_snowfree, qn_snow, kclear, kup, lup, tsurf, &
         !    qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, &
         !    albedo_snow, SnowAlb_next, &
         !    dataOutLineSPARTACUS)

         CALL SUEWS_cal_Qn_DTS( &
            methodPrm%StorageHeatMethod, methodPrm%NetRadiationMethod, methodPrm%SnowUse, & !input
            timer%tstep, nlayer, snowState_prev%SnowPack, snowPrm%tau_a, snowPrm%tau_f, snowPrm%SnowAlbMax, snowPrm%SnowAlbMin, &
            methodPrm%Diagnose, forcing%ldown, forcing%fcld, &
            dectime, ZENITH_deg, Ts_iter, forcing%kdown, forcing%Temp_C, forcing%RH, ea_hPa, forcing%qn1_obs, &
            snowState_prev%snowalb, snowState_prev%snowFrac, DiagQN, &
            siteInfo%NARP_TRANS_SITE, snowPrm%NARP_EMIS_SNOW, snowState_prev%IceFrac, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
            sfr_roof, sfr_wall, &
            heatState_out%tsfc_surf, heatState_out%tsfc_roof, heatState_out%tsfc_wall, &
            pavedPrm%emis, bldgPrm%emis, evetrPrm%emis, dectrPrm%emis, grassPrm%emis, bsoilPrm%emis, waterPrm%emis, &
            phenState_prev%alb, phenState_next%albDecTr_id, phenState_next%albEveTr_id, phenState_next%albGrass_id, &
            phenState%LAI_id, & !input
            spartacusPrm%n_vegetation_region_urban, &
            spartacusPrm%n_stream_sw_urban, spartacusPrm%n_stream_lw_urban, &
            spartacusPrm%sw_dn_direct_frac, spartacusPrm%air_ext_sw, spartacusPrm%air_ssa_sw, &
            spartacusPrm%veg_ssa_sw, spartacusPrm%air_ext_lw, spartacusPrm%air_ssa_lw, spartacusPrm%veg_ssa_lw, &
            spartacusPrm%veg_fsd_const, spartacusPrm%veg_contact_fraction_const, &
            spartacusPrm%ground_albedo_dir_mult_fact, methodPrm%use_sw_direct_albedo, & !input
            spartacusPrm%height, spartacusLayerPrm%building_frac, &
            spartacusLayerPrm%veg_frac, spartacusLayerPrm%building_scale, spartacusLayerPrm%veg_scale, & !input: SPARTACUS
            spartacusLayerPrm%alb_roof, spartacusLayerPrm%emis_roof, spartacusLayerPrm%alb_wall, spartacusLayerPrm%emis_wall, &
            spartacusLayerPrm%roof_albedo_dir_mult_fact, spartacusLayerPrm%wall_specular_frac, &
            phenState_next%alb, ldown, fcld, & !output
            QN_surf, QN_roof, QN_wall, &
            qn, qn_snowfree, qn_snow, kclear, kup, lup, tsurf, &
            qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, &
            albedo_snow, snowState_next%SnowAlb, &
            dataOutLineSPARTACUS)

         ! IF (qn < -300) THEN
         !    PRINT *, 'qn=', qn
         !    PRINT *, 'snowFrac_prev=', snowFrac_prev
         ! END IF

         ! PRINT *, 'Qn_surf after SUEWS_cal_Qn ', qn_surf
         ! PRINT *, 'qn_roof after SUEWS_cal_Qn ', qn_roof
         ! PRINT *, 'qn_wall after SUEWS_cal_Qn ', qn_wall
         ! PRINT *, ''

         ! =================STORAGE HEAT FLUX=======================================
         IF (i_iter == 1) THEN
            Qg_surf = 0.1*QN_surf
            Qg_roof = 0.1*QN_roof
            Qg_wall = 0.1*QN_wall
         ELSE
            Qg_surf = QN_surf + QF - (QH_surf + QE_surf)
            Qg_roof = QN_roof + QF - (QH_roof + QE_roof)
            Qg_wall = QN_wall + QF - (QH_wall + QE_wall)
         END IF

         ! PRINT *, 'Qg_surf before cal_qs', Qg_surf
         ! PRINT *, 'Qg_roof before cal_qs', Qg_roof
         ! PRINT *, 'Qg_wall before cal_qs', Qg_wall
         ! print *,''

         ! PRINT *, 'tsfc_surf before cal_qs', tsfc_out_surf
         ! PRINT *, 'tsfc_out_roof before cal_qs', tsfc_out_roof
         ! PRINT *, 'tsfc_wall before cal_qs', tsfc_out_wall
         ! PRINT *, ''

         ! CALL SUEWS_cal_Qs( &
         !    StorageHeatMethod, qs_obs, OHMIncQF, Gridiv, & !input
         !    id, tstep, dt_since_start, Diagnose, &
         !    nlayer, &
         !    Qg_surf, Qg_roof, Qg_wall, &
         !    tsfc_out_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
         !    tsfc_out_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
         !    tsfc_out_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
         !    OHM_coef, OHM_threshSW, OHM_threshWD, &
         !    soilstore_surf_prev, SoilStoreCap_surf, state_surf_prev, SnowUse, SnowFrac_prev, DiagQS, &
         !    HDD_id, MetForcingData_grid, Ts5mindata_ir, qf, qn, &
         !    kdown, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown, &
         !    bldgh, alb, emis, cpAnOHM, kkAnOHM, chAnOHM, EmissionsMethod, &
         !    Tair_av, qn_av_prev, dqndt_prev, qn_s_av_prev, dqnsdt_prev, &
         !    StoreDrainPrm, &
         !    qn_snow, dataOutLineESTM, qs, & !output
         !    qn_av_next, dqndt_next, qn_s_av_next, dqnsdt_next, &
         !    deltaQi, a1, a2, a3, &
         !    temp_out_roof, QS_roof, & !output
         !    temp_out_wall, QS_wall, & !output
         !    temp_out_surf, QS_surf) !output
         CALL SUEWS_cal_Qs_DTS( &
            methodPrm%StorageHeatMethod, forcing%qs_obs, methodPrm%OHMIncQF, siteInfo%Gridiv, & !input
            timer%id, timer%tstep, timer%dt_since_start, methodPrm%Diagnose, &
            nlayer, &
            Qg_surf, Qg_roof, Qg_wall, &
            heatState_out%tsfc_roof, ehcPrm%tin_roof, &
            heatState_in%temp_roof, ehcPrm%k_roof, &
            ehcPrm%cp_roof, ehcPrm%dz_roof, sfr_roof, & !input
            heatState_out%tsfc_wall, ehcPrm%tin_wall, &
            heatState_in%temp_wall, ehcPrm%k_wall, &
            ehcPrm%cp_wall, ehcPrm%dz_wall, sfr_wall, & !input
            heatState_out%tsfc_surf, ehcPrm%tin_surf, &
            heatState_in%temp_surf, ehcPrm%k_surf, &
            ehcPrm%cp_surf, ehcPrm%dz_surf, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, & !input
            pavedPrm%ohm%ohm_coef_lc, bldgPrm%ohm%ohm_coef_lc, evetrPrm%ohm%ohm_coef_lc, &
            dectrPrm%ohm%ohm_coef_lc, grassPrm%ohm%ohm_coef_lc, bsoilPrm%ohm%ohm_coef_lc, &
            waterPrm%ohm%ohm_coef_lc, & !input
            pavedPrm%ohm%ohm_threshsw, bldgPrm%ohm%ohm_threshsw, evetrPrm%ohm%ohm_threshsw, &
            dectrPrm%ohm%ohm_threshsw, grassPrm%ohm%ohm_threshsw, bsoilPrm%ohm%ohm_threshsw, &
            waterPrm%ohm%ohm_threshsw, & !input
            pavedPrm%ohm%ohm_threshwd, bldgPrm%ohm%ohm_threshwd, evetrPrm%ohm%ohm_threshwd, &
            dectrPrm%ohm%ohm_threshwd, grassPrm%ohm%ohm_threshwd, bsoilPrm%ohm%ohm_threshwd, &
            waterPrm%ohm%ohm_threshwd, & !input
            hydroState_prev%soilstore_surf, &
            pavedPrm%soil%soilstorecap, bldgPrm%soil%soilstorecap, &
            evetrPrm%soil%soilstorecap, dectrPrm%soil%soilstorecap, &
            grassPrm%soil%soilstorecap, bsoilPrm%soil%soilstorecap, waterPrm%soil%soilstorecap, & !input
            hydroState_prev%state_surf, methodPrm%SnowUse, snowState_prev%SnowFrac, DiagQS, &
            anthroHeatState%HDD_id, MetForcingData_grid, Ts5mindata_ir, qf, qn, &
            forcing%kdown, forcing%U, forcing%temp_c, zenith_deg, forcing%RH, forcing%pres, ldown, &
            bldgPrm%bldgh, phenState%alb, &
            pavedPrm%emis, bldgPrm%emis, evetrPrm%emis, dectrPrm%emis, grassPrm%emis, bsoilPrm%emis, waterPrm%emis, &
            pavedPrm%ohm%cpanohm, bldgPrm%ohm%cpanohm, evetrPrm%ohm%cpanohm, &
            dectrPrm%ohm%cpanohm, grassPrm%ohm%cpanohm, bsoilPrm%ohm%cpanohm, waterPrm%ohm%cpanohm, &
            pavedPrm%ohm%kkanohm, bldgPrm%ohm%kkanohm, evetrPrm%ohm%kkanohm, &
            dectrPrm%ohm%kkanohm, grassPrm%ohm%kkanohm, bsoilPrm%ohm%kkanohm, waterPrm%ohm%kkanohm, &
            pavedPrm%ohm%chanohm, bldgPrm%ohm%chanohm, evetrPrm%ohm%chanohm, &
            dectrPrm%ohm%chanohm, grassPrm%ohm%chanohm, bsoilPrm%ohm%chanohm, waterPrm%ohm%chanohm, &
            methodPrm%EmissionsMethod, &
            forcing%Tair, ohmState_prev%qn_av, ohmState_prev%dqndt, ohmState_prev%qn_s_av, ohmState_prev%dqnsdt, &
            phenState%StoreDrainPrm, &
            qn_snow, dataOutLineESTM, qs, & !output
            ohmState_next%qn_av, ohmState_next%dqndt, ohmState_next%qn_s_av, ohmState_next%dqnsdt, &
            deltaQi, a1, a2, a3, &
            heatState_out%temp_roof, QS_roof, & !output
            heatState_out%temp_wall, QS_wall, & !output
            heatState_out%temp_surf, QS_surf) !output
         ! update iteration variables
         ! temp_in_roof = temp_out_roof
         ! temp_in_wall = temp_out_wall
         ! temp_in_surf = temp_out_surf
         ! Ts_iter = DOT_PRODUCT(tsfc_out_surf, sfr_surf)
         ! PRINT *, 'QS_surf after cal_qs', QS_surf
         ! PRINT *, 'QS_roof after cal_qs', QS_roof
         ! PRINT *, 'QS_wall after cal_qs', QS_wall

         ! PRINT *, ''

         ! PRINT *, 'tsfc_surf after cal_qs', tsfc_out_surf
         ! PRINT *, 'tsfc_roof after cal_qs', tsfc_out_roof
         ! PRINT *, 'tsfc_wall after cal_qs', tsfc_out_wall
         ! PRINT *, ''
         ! print *,'tsfc_surf abs. diff.:',maxval(abs(tsfc_out_surf-tsfc0_out_surf)),maxloc(abs(tsfc_out_surf-tsfc0_out_surf))
         ! dif_tsfc_iter=maxval(abs(tsfc_out_surf-tsfc0_out_surf))
         ! print *,'tsfc_roof abs. diff.:',maxval(abs(tsfc_out_roof-tsfc0_out_roof)),maxloc(abs(tsfc_out_roof-tsfc0_out_roof))
         ! dif_tsfc_iter=max(maxval(abs(tsfc_out_roof-tsfc0_out_roof)),dif_tsfc_iter)
         ! print *,'tsfc_wall abs. diff.:',maxval(abs(tsfc_out_wall-tsfc0_out_wall)),maxloc(abs(tsfc_out_wall-tsfc0_out_wall))
         ! dif_tsfc_iter=max(maxval(abs(tsfc0_out_wall-tsfc_out_wall)),dif_tsfc_iter)

         ! tsfc0_out_surf = tsfc_out_surf
         ! tsfc0_out_roof = tsfc_out_roof
         ! tsfc0_out_wall = tsfc_out_wall

         !==================Energy related to snow melting/freezing processes=======
         IF (Diagnose == 1) WRITE (*, *) 'Calling MeltHeat'

         !==========================Turbulent Fluxes================================
         IF (Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_QHQE...'
         IF (i_iter == 1) THEN
            !Calculate QH and QE from LUMPS in the first iteration of each time step
            ! CALL LUMPS_cal_QHQE( &
            !    veg_type, & !input
            !    SnowUse, qn, qf, qs, Temp_C, VegFraction, avcp, Press_hPa, lv_J_kg, &
            !    tstep_real, DRAINRT, nsh_real, &
            !    Precip, RainMaxRes, RAINCOVER, sfr_surf, LAI_id_next, LAImax, LAImin, &
            !    QH_LUMPS, & !output
            !    QE_LUMPS, psyc_hPa, s_hPa, sIce_hpa, TempVeg, VegPhenLumps)
            CALL LUMPS_cal_QHQE_DTS( &
               lumpsPrm%veg_type, & !input
               methodPrm%SnowUse, qn, qf, qs, forcing%Temp_C, VegFraction, avcp, forcing%pres, lv_J_kg, &
               tstep_real, lumpsPrm%drainrt, nsh_real, &
               forcing%rain, lumpsPrm%rainmaxres, lumpsPrm%raincover, &
               pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
               phenState_next%LAI_id, &
               evetrPrm%lai%laimax, dectrPrm%lai%laimax, grassPrm%lai%laimax, &
               evetrPrm%lai%laimin, dectrPrm%lai%laimin, grassPrm%lai%laimin, &
               QH_LUMPS, & !output
               QE_LUMPS, psyc_hPa, s_hPa, sIce_hpa, TempVeg, VegPhenLumps)

            ! use LUMPS QH to do stability correction
            QH_Init = QH_LUMPS
         ELSE
            ! use SUEWS QH to do stability correction
            QH_Init = QH
         END IF

         !============= calculate water balance =============
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Water...'
         ! CALL SUEWS_cal_Water( &
         !    Diagnose, & !input
         !    SnowUse, NonWaterFraction, addPipes, addImpervious, addVeg, addWaterBody, &
         !    state_surf_prev, sfr_surf, StoreDrainPrm_next, WaterDist, nsh_real, &
         !    drain_per_tstep, & !output
         !    drain_surf, frac_water2runoff, &
         !    AdditionalWater, runoffPipes, runoff_per_interval, &
         !    AddWater)

         CALL SUEWS_cal_Water_DTS( &
            methodPrm%Diagnose, & !input
            methodPrm%SnowUse, NonWaterFraction, addPipes, addImpervious, addVeg, addWaterBody, &
            hydroState_prev%state_surf, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
            phenState_next%StoreDrainPrm, &
            pavedPrm%waterdist%to_paved, pavedPrm%waterdist%to_bldg, pavedPrm%waterdist%to_evetr, &
            pavedPrm%waterdist%to_dectr, pavedPrm%waterdist%to_grass, pavedPrm%waterdist%to_bsoil, pavedPrm%waterdist%to_water, &
            pavedPrm%waterdist%to_soilstore, &
            bldgPrm%waterdist%to_paved, bldgPrm%waterdist%to_bldg, bldgPrm%waterdist%to_evetr, &
            bldgPrm%waterdist%to_dectr, bldgPrm%waterdist%to_grass, bldgPrm%waterdist%to_bsoil, bldgPrm%waterdist%to_water, &
            bldgPrm%waterdist%to_soilstore, &
            evetrPrm%waterdist%to_paved, evetrPrm%waterdist%to_bldg, evetrPrm%waterdist%to_evetr, &
            evetrPrm%waterdist%to_dectr, evetrPrm%waterdist%to_grass, evetrPrm%waterdist%to_bsoil, evetrPrm%waterdist%to_water, &
            evetrPrm%waterdist%to_soilstore, &
            dectrPrm%waterdist%to_paved, dectrPrm%waterdist%to_bldg, dectrPrm%waterdist%to_evetr, &
            dectrPrm%waterdist%to_dectr, dectrPrm%waterdist%to_grass, dectrPrm%waterdist%to_bsoil, dectrPrm%waterdist%to_water, &
            dectrPrm%waterdist%to_soilstore, &
            grassPrm%waterdist%to_paved, grassPrm%waterdist%to_bldg, grassPrm%waterdist%to_evetr, &
            grassPrm%waterdist%to_dectr, grassPrm%waterdist%to_grass, grassPrm%waterdist%to_bsoil, grassPrm%waterdist%to_water, &
            grassPrm%waterdist%to_soilstore, &
            bsoilPrm%waterdist%to_paved, bsoilPrm%waterdist%to_bldg, bsoilPrm%waterdist%to_evetr, &
            bsoilPrm%waterdist%to_dectr, bsoilPrm%waterdist%to_grass, bsoilPrm%waterdist%to_bsoil, &
            bsoilPrm%waterdist%to_water, &
            bsoilPrm%waterdist%to_soilstore, &
            nsh_real, &
            drain_per_tstep, & !output
            drain_surf, frac_water2runoff, &
            AdditionalWater, runoffPipes, runoff_per_interval, &
            AddWater)
         !============= calculate water balance end =============

         !===============Resistance Calculations=======================
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Resistance...'
         ! CALL SUEWS_cal_Resistance( &
         !    StabilityMethod, & !input:
         !    Diagnose, AerodynamicResistanceMethod, RoughLenHeatMethod, SnowUse, &
         !    id, it, gsModel, SMDMethod, &
         !    avdens, avcp, QH_Init, zzd, z0m, zdm, &
         !    avU1, Temp_C, VegFraction, kdown, &
         !    Kmax, &
         !    g_max, g_k, g_q_base, g_q_shape, &
         !    g_t, g_sm, s1, s2, &
         !    th, tl, &
         !    dq, xsmd, vsmd, MaxConductance, LAIMax, LAI_id_next, SnowFrac_prev, sfr_surf, &
         !    g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
         !    UStar, TStar, L_mod, & !output
         !    zL, gsc, RS, RA_h, RAsnow, RB, z0v, z0vSnow)
         CALL SUEWS_cal_Resistance_DTS( &
            methodPrm%StabilityMethod, & !input:
            methodPrm%Diagnose, AerodynamicResistanceMethod, methodPrm%RoughLenHeatMethod, methodPrm%SnowUse, &
            timer%id, timer%it, &
            conductancePrm%gsModel, &
            methodPrm%SMDMethod, &
            avdens, avcp, QH_Init, zzd, z0m, zdm, &
            forcing%U, forcing%Temp_C, VegFraction, forcing%kdown, &
            conductancePrm%Kmax, conductancePrm%g_max, conductancePrm%g_k, conductancePrm%g_q_base, conductancePrm%g_q_shape, &
            conductancePrm%g_t, conductancePrm%g_sm, conductancePrm%s1, conductancePrm%s2, &
            conductancePrm%th, conductanceprm%tl, &
            dq, forcing%xsmd, vsmd, &
            evetrPrm%maxconductance, dectrPrm%maxconductance, grassPrm%maxconductance, &
            evetrPrm%lai%laimax, dectrPrm%lai%laimax, grassPrm%lai%laimax, &
            phenState_next%LAI_id, snowState_prev%SnowFrac, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
            g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
            UStar, TStar, L_mod, & !output
            zL, gsc, RS, RA_h, RAsnow, RB, z0v, z0vSnow)

         !===================Resistance Calculations End=======================

         !===================Calculate surface hydrology and related soil water=======================
         IF (SnowUse == 1) THEN

            ! ===================Calculate snow related hydrology=======================
            ! CALL SUEWS_cal_snow( &
            !    Diagnose, nlayer, & !input
            !    tstep, imin, it, EvapMethod, dayofWeek_id, CRWmin, CRWmax, &
            !    dectime, avdens, avcp, lv_J_kg, lvS_J_kg, avRh, Press_hPa, Temp_C, &
            !    RAsnow, psyc_hPa, sIce_hPa, tau_r, &
            !    RadMeltFact, TempMeltFact, SnowAlbMax, PrecipLimit, PrecipLimitAlb, &
            !    qn_ind_snow, kup_ind_snow, deltaQi, Tsurf_ind_snow, &
            !    SnowAlb_next, &
            !    PervFraction, vegfraction, addimpervious, qn_snowfree, qf, qs, vpd_hPa, s_hPa, &
            !    RS, RA_h, RB, SnowDensMax, snowdensmin, precip, PipeCapacity, RunoffToWater, &
            !    addVeg, SnowLimPaved, SnowLimBldg, &
            !    FlowChange, drain_surf, WetThresh_surf, SoilStoreCap_surf, &
            !    Tsurf_ind, sfr_surf, &
            !    AddWater, frac_water2runoff, StoreDrainPrm_next, SnowPackLimit, SnowProf_24hr, &
            !    SnowPack_prev, snowFrac_prev, SnowWater_prev, IceFrac_prev, SnowDens_prev, & ! input:
            !    SnowfallCum_prev, state_surf_prev, soilstore_surf_prev, & ! input:
            !    QN_surf, qs_surf, &
            !    SnowRemoval, & ! snow specific output
            !    SnowPack_next, SnowFrac_next, SnowWater_next, iceFrac_next, SnowDens_next, & ! output
            !    SnowfallCum_next, state_surf_next, soilstore_surf_next, & ! general output:
            !    state_per_tstep, NWstate_per_tstep, &
            !    qe, qe_surf, qe_roof, qe_wall, &
            !    SnowAlb_next, &
            !    swe, chSnow_per_interval, ev_per_tstep, runoff_per_tstep, &
            !    surf_chang_per_tstep, runoffPipes, mwstore, runoffwaterbody, &
            !    runoffAGveg, runoffAGimpervious, rss_surf, &
            !    dataOutLineSnow)
            CALL SUEWS_cal_snow_DTS( &
               methodPrm%Diagnose, nlayer, & !input
               tstep, timer%imin, timer%it, EvapMethod, dayofWeek_id, snowPrm%CRWmin, snowPrm%CRWmax, &
               dectime, avdens, avcp, lv_J_kg, lvS_J_kg, forcing%RH, forcing%Pres, forcing%Temp_C, &
               RAsnow, psyc_hPa, sIce_hPa, snowPrm%tau_r, &
               RadMeltFact, TempMeltFact, snowPrm%SnowAlbMax, snowPrm%PrecipLimit, snowPrm%PrecipLimitAlb, &
               qn_ind_snow, kup_ind_snow, deltaQi, Tsurf_ind_snow, &
               snowState_next%SnowAlb, &
               PervFraction, vegfraction, addimpervious, qn_snowfree, qf, qs, vpd_hPa, s_hPa, &
              RS, RA_h, RB, snowPrm%SnowDensMax, snowPrm%snowdensmin, forcing%rain, siteInfo%PipeCapacity, siteInfo%RunoffToWater, &
               addVeg, snowPrm%SnowLimPaved, snowPrm%SnowLimBldg, &
               siteInfo%FlowChange, drain_surf, &
               pavedPrm%wetthresh, bldgPrm%wetthresh, evetrPrm%wetthresh, dectrPrm%wetthresh, &
               grassPrm%wetthresh, bsoilPrm%wetthresh, waterPrm%wetthresh, &
               pavedPrm%soil%soilstorecap, bldgPrm%soil%soilstorecap, &
               evetrPrm%soil%soilstorecap, dectrPrm%soil%soilstorecap, &
               grassPrm%soil%soilstorecap, bsoilPrm%soil%soilstorecap, waterPrm%soil%soilstorecap, &
               Tsurf_ind, &
               pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
               AddWater, frac_water2runoff, phenState_next%StoreDrainPrm, snowPrm%SnowPackLimit, &
               snowPrm%snowprof_24hr_working, snowPrm%snowprof_24hr_holiday, &
               snowState_prev%SnowPack, snowState_prev%snowFrac, snowState_prev%SnowWater, &
               snowState_prev%IceFrac, snowState_prev%SnowDens, & ! input:
               snowState_prev%SnowfallCum, hydroState_prev%state_surf, hydroState_prev%soilstore_surf, & ! input:
               QN_surf, qs_surf, &
               SnowRemoval, & ! snow specific output
               snowState_next%SnowPack, snowState_next%SnowFrac, snowState_next%SnowWater, &
               snowState_next%iceFrac, snowState_next%SnowDens, & ! output
               snowState_next%SnowfallCum, hydroState_next%state_surf, hydroState_next%soilstore_surf, & ! general output:
               state_per_tstep, NWstate_per_tstep, &
               qe, qe_surf, qe_roof, qe_wall, &
               snowState_next%SnowAlb, &
               swe, chSnow_per_interval, ev_per_tstep, runoff_per_tstep, &
               surf_chang_per_tstep, runoffPipes, mwstore, runoffwaterbody, &
               runoffAGveg, runoffAGimpervious, rss_surf, &
               dataOutLineSnow)
            ! N.B.: snow-related calculations end here.
            !===================================================
         ELSE
            IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_QE...'
            !======== Evaporation and surface state_id for snow-free conditions ========
            ! CALL SUEWS_cal_QE( &
            !    Diagnose, storageheatmethod, nlayer, & !input
            !    tstep, &
            !    EvapMethod, &
            !    avdens, avcp, lv_J_kg, &
            !    psyc_hPa, &
            !    PervFraction, &
            !    addimpervious, &
            !    qf, vpd_hPa, s_hPa, RS, RA_h, RB, &
            !    precip, PipeCapacity, RunoffToWater, &
            !    NonWaterFraction, wu_surf, addVeg, addWaterBody, AddWater, &
            !    FlowChange, drain_surf, &
            !    frac_water2runoff, StoreDrainPrm_next, &
            !    sfr_surf, StateLimit_surf, SoilStoreCap_surf, WetThresh_surf, & ! input:
            !    state_surf_prev, soilstore_surf_prev, QN_surf, qs_surf, & ! input:
            !    sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
            !    state_roof_prev, soilstore_roof_prev, QN_roof, qs_roof, & ! input:
            !    sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
            !    state_wall_prev, soilstore_wall_prev, QN_wall, qs_wall, & ! input:
            !    state_surf_next, soilstore_surf_next, ev_surf, & ! general output:
            !    state_roof_next, soilstore_roof_next, ev_roof, & ! general output:
            !    state_wall_next, soilstore_wall_next, ev_wall, & ! general output:
            !    state_per_tstep, NWstate_per_tstep, &
            !    ev0_surf, qe0_surf, &
            !    qe, qe_surf, qe_roof, qe_wall, &
            !    ev_per_tstep, runoff_per_tstep, &
            !    surf_chang_per_tstep, runoffPipes, &
            !    runoffwaterbody, &
            !    runoffAGveg, runoffAGimpervious, rss_surf)
            CALL SUEWS_cal_QE_DTS( &
               methodPrm%Diagnose, methodPrm%storageheatmethod, nlayer, & !input
               timer%tstep, &
               EvapMethod, &
               avdens, avcp, lv_J_kg, &
               psyc_hPa, &
               PervFraction, &
               addimpervious, &
               qf, vpd_hPa, s_hPa, RS, RA_h, RB, &
               forcing%rain, siteInfo%pipecapacity, siteInfo%runofftowater, &
               NonWaterFraction, wu_surf, addVeg, addWaterBody, AddWater, &
               siteInfo%flowchange, drain_surf, &
               frac_water2runoff, phenState_next%StoreDrainPrm, &
               pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
               pavedPrm%statelimit, bldgPrm%statelimit, evetrPrm%statelimit, &
               dectrPrm%statelimit, grassPrm%statelimit, bsoilPrm%statelimit, waterPrm%statelimit, &
               pavedPrm%soil%soilstorecap, bldgPrm%soil%soilstorecap, &
               evetrPrm%soil%soilstorecap, dectrPrm%soil%soilstorecap, &
               grassPrm%soil%soilstorecap, bsoilPrm%soil%soilstorecap, waterPrm%soil%soilstorecap, &
               pavedPrm%wetthresh, bldgPrm%wetthresh, evetrPrm%wetthresh, &
               dectrPrm%wetthresh, grassPrm%wetthresh, bsoilPrm%wetthresh, waterPrm%wetthresh, &
               hydroState_prev%state_surf, hydroState_prev%soilstore_surf, QN_surf, qs_surf, & ! input:
               sfr_roof, ehcPrm%state_limit_roof, ehcPrm%soil_storecap_roof, ehcPrm%wet_thresh_roof, & ! input:
               hydroState_prev%state_roof, hydroState_prev%soilstore_roof, QN_roof, qs_roof, & ! input:
               sfr_wall, ehcPrm%state_limit_wall, ehcPrm%soil_storecap_wall, ehcPrm%wet_thresh_wall, & ! input:
               hydroState_prev%state_wall, hydroState_prev%soilstore_wall, QN_wall, qs_wall, & ! input:
               hydroState_next%state_surf, hydroState_next%soilstore_surf, ev_surf, & ! general output:
               hydroState_next%state_roof, hydroState_next%soilstore_roof, ev_roof, & ! general output:
               hydroState_next%state_wall, hydroState_next%soilstore_wall, ev_wall, & ! general output:
               state_per_tstep, NWstate_per_tstep, &
               ev0_surf, qe0_surf, &
               qe, qe_surf, qe_roof, qe_wall, &
               ev_per_tstep, runoff_per_tstep, &
               surf_chang_per_tstep, runoffPipes, &
               runoffwaterbody, &
               runoffAGveg, runoffAGimpervious, rss_surf)
            !======== Evaporation and surface state_id end========
         END IF
         ! IF (Diagnose == 1) PRINT *, 'before SUEWS_cal_SoilState soilstore_id = ', soilstore_surf_next
         IF (Diagnose == 1) PRINT *, 'before SUEWS_cal_SoilState soilstore_id = ', hydroState_next%soilstore_surf

         !=== Horizontal movement between soil stores ===
         ! Now water is allowed to move horizontally between the soil stores
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_HorizontalSoilWater...'
         ! CALL SUEWS_cal_HorizontalSoilWater( &
         !    sfr_surf, & ! input: ! surface fractions
         !    SoilStoreCap_surf, & !Capacity of soil store for each surface [mm]
         !    SoilDepth, & !Depth of sub-surface soil store for each surface [mm]
         !    SatHydraulicConduct, & !Saturated hydraulic conductivity for each soil subsurface [mm s-1]
         !    SurfaceArea, & !Surface area of the study area [m2]
         !    NonWaterFraction, & ! sum of surface cover fractions for all except water surfaces
         !    tstep_real, & !tstep cast as a real for use in calculations
         !    soilstore_surf_next, & ! inout:!Soil moisture of each surface type [mm]
         !    runoffSoil, & !Soil runoff from each soil sub-surface [mm]
         !    runoffSoil_per_tstep & !  output:!Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
         !    )
         CALL SUEWS_cal_HorizontalSoilWater_DTS( &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, & ! input: ! surface fractions
            pavedPrm%soil%soilstorecap, bldgPrm%soil%soilstorecap, evetrPrm%soil%soilstorecap, &
            dectrPrm%soil%soilstorecap, grassPrm%soil%soilstorecap, bsoilPrm%soil%soilstorecap, &
            waterPrm%soil%soilstorecap, & !Capacity of soil store for each surface [mm]
            pavedPrm%soil%soildepth, bldgPrm%soil%soildepth, evetrPrm%soil%soildepth, &
            dectrPrm%soil%soildepth, grassPrm%soil%soildepth, bsoilPrm%soil%soildepth, &
            waterPrm%soil%soildepth, & !Depth of sub-surface soil store for each surface [mm]
            pavedPrm%soil%sathydraulicconduct, bldgPrm%soil%sathydraulicconduct, evetrPrm%soil%sathydraulicconduct, &
            dectrPrm%soil%sathydraulicconduct, grassPrm%soil%sathydraulicconduct, bsoilPrm%soil%sathydraulicconduct, &
            waterPrm%soil%sathydraulicconduct, & !Saturated hydraulic conductivity for each soil subsurface [mm s-1]
            siteInfo%SurfaceArea, & !Surface area of the study area [m2]
            NonWaterFraction, & ! sum of surface cover fractions for all except water surfaces
            tstep_real, & !tstep cast as a real for use in calculations
            hydroState_next%soilstore_surf, & ! inout:!Soil moisture of each surface type [mm]
            runoffSoil, & !Soil runoff from each soil sub-surface [mm]
            runoffSoil_per_tstep & !  output:!Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
            )

         !========== Calculate soil moisture ============
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_SoilState...'
         ! CALL SUEWS_cal_SoilState( &
         !    SMDMethod, xsmd, NonWaterFraction, SoilMoistCap, & !input
         !    SoilStoreCap_surf, surf_chang_per_tstep, &
         !    soilstore_surf_next, soilstore_surf_prev, sfr_surf, &
         !    smd, smd_nsurf, tot_chang_per_tstep, SoilState) !output
         CALL SUEWS_cal_SoilState_DTS( &
            methodPrm%SMDMethod, forcing%xsmd, NonWaterFraction, SoilMoistCap, & !input
            pavedPrm%soil%soilstorecap, bldgPrm%soil%soilstorecap, evetrPrm%soil%soilstorecap, &
            dectrPrm%soil%soilstorecap, grassPrm%soil%soilstorecap, bsoilPrm%soil%soilstorecap, &
            waterPrm%soil%soilstorecap, & ! capacity of soil store for each surface [mm]
            surf_chang_per_tstep, &
            hydroState_next%soilstore_surf, hydroState_prev%soilstore_surf, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
            smd, smd_nsurf, tot_chang_per_tstep, SoilState) !output

         !============ Sensible heat flux ===============
         IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_QH...'
         ! CALL SUEWS_cal_QH( &
         !    1, nlayer, storageheatmethod, & !input
         !    qn, qf, QmRain, qe, qs, QmFreez, qm, avdens, avcp, &
         !    sfr_surf, sfr_roof, sfr_wall, &
         !    tsfc_out_surf, tsfc_out_roof, tsfc_out_wall, &
         !    Temp_C, &
         !    RA_h, &
         !    qh, qh_residual, qh_resist, & !output
         !    qh_resist_surf, qh_resist_roof, qh_resist_wall)

         CALL SUEWS_cal_QH_DTS( &
            1, nlayer, methodPrm%StorageHeatMethod, & !input
            qn, qf, QmRain, qe, qs, QmFreez, qm, avdens, avcp, &
            pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
            sfr_roof, sfr_wall, &
            heatState_out%tsfc_surf, heatState_out%tsfc_roof, heatState_out%tsfc_wall, &
            forcing%Temp_C, &
            RA_h, &
            qh, qh_residual, qh_resist, & !output
            qh_resist_surf, qh_resist_roof, qh_resist_wall)
         ! PRINT *, 'qn_surf after SUEWS_cal_QH', qn_surf
         ! PRINT *, 'qs_surf after SUEWS_cal_QH', qs_surf
         ! PRINT *, 'qe_surf after SUEWS_cal_QH', qe_surf
         ! PRINT *, 'qh_surf after SUEWS_cal_QH (resist)', qh_surf
         ! PRINT *, 'qh_roof after SUEWS_cal_QH (resist)', qh_roof
         ! PRINT *, 'qh_wall after SUEWS_cal_QH (resist)', qh_wall
         ! PRINT *, ''

         ! PRINT *, 'tsfc_surf after SUEWS_cal_QH (resist)', tsfc_out_surf
         ! PRINT *, 'tsfc_roof after SUEWS_cal_QH (resist)', tsfc_out_roof
         ! PRINT *, 'tsfc_wall after SUEWS_cal_QH (resist)', tsfc_out_wall
         ! PRINT *, ''
         ! PRINT *, ' qh_residual: ', qh_residual, ' qh_resist: ', qh_resist
         ! PRINT *, ' dif_qh: ', ABS(qh_residual - qh_resist)
         !============ Sensible heat flux end ===============

         ! residual heat flux
         ! PRINT *, 'residual surf: ', qn_surf + qf - qs_surf - qe_surf - qh_surf
         ! PRINT *, 'residual roof: ', qn_roof + qf - qs_roof - qe_roof - qh_roof
         ! PRINT *, 'residual wall: ', qn_wall + qf - qs_wall - qe_wall - qh_wall

         !============ Sensible heat flux end===============

         !============ calculate surface temperature ===============
         TSfc_C = cal_tsfc(qh, avdens, avcp, RA_h, forcing%temp_c)

         !============= calculate surface specific QH and Tsfc ===============
         ! note: tsfc has an upper limit of temp_c+50 to avoid numerical errors
         tsfc0_out_surf = MIN(heatState_out%tsfc_surf, forcing%Temp_C + 50)
         tsfc0_out_roof = MIN(heatState_out%tsfc_roof, forcing%Temp_C + 50)
         tsfc0_out_wall = MIN(heatState_out%tsfc_wall, forcing%Temp_C + 50)

         QH_surf = QN_surf + qf - qs_surf - qe_surf
         QH_roof = QN_roof + qf - qs_roof - qe_roof
         QH_wall = QN_wall + qf - qs_wall - qe_wall

         IF (methodPrm%diagnose == 1) THEN
            PRINT *, 'qn_surf before QH back env.:', QN_surf
            PRINT *, 'qf before QH back env.:', qf
            PRINT *, 'qs_surf before QH back env.:', qs_surf
            PRINT *, 'qe_surf before QH back env.:', qe_surf
            PRINT *, 'qh_surf before QH back env.:', QH_surf

            PRINT *, 'qn_roof before QH back env.:', QN_roof
            PRINT *, 'qs_roof before QH back env.:', qs_roof
            PRINT *, 'qe_roof before QH back env.:', qe_roof
            PRINT *, 'qh_roof before QH back env.:', QH_roof

         END IF
         DO i_surf = 1, nsurf
            ! TSfc_QH_surf(i_surf) = cal_tsfc(qh_surf(i_surf), avdens, avcp, RA_h, temp_c)
            heatState_out%tsfc_surf(i_surf) = cal_tsfc(QH_surf(i_surf), avdens, avcp, RA_h, forcing%temp_c)
            ! if ( i_surf==1 ) then
            !    tsfc_out_surf(i_surf) = cal_tsfc(qh_surf(i_surf), avdens, avcp, RA_h, temp_c)
            ! else
            !    tsfc_out_surf(i_surf)=tsfc0_out_surf(i_surf)
            ! end if
            ! restrict calculated heat storage to a sensible range
            ! tsfc_out_surf(i_surf) = MAX(MIN(tsfc_out_surf(i_surf), 100.0), -100.0)
         END DO

         DO i_surf = 1, nlayer
            heatState_out%tsfc_roof(i_surf) = cal_tsfc(QH_roof(i_surf), avdens, avcp, RA_h, forcing%temp_c)
            heatState_out%tsfc_wall(i_surf) = cal_tsfc(QH_wall(i_surf), avdens, avcp, RA_h, forcing%temp_c)
         END DO

         IF (methodPrm%diagnose == 1) PRINT *, 'tsfc_surf after QH back env.:', heatState_out%tsfc_surf
         ! print *,'tsfc_roof after QH back env.:',tsfc_out_roof
         IF (methodPrm%diagnose == 1) PRINT *, &
            'tsfc_surf abs. diff.:', MAXVAL(ABS(heatState_out%tsfc_surf - tsfc0_out_surf)), &
            MAXLOC(ABS(heatState_out%tsfc_surf - tsfc0_out_surf))
         dif_tsfc_iter = MAXVAL(ABS(heatState_out%tsfc_surf - tsfc0_out_surf))
         IF (methodPrm%StorageHeatMethod == 5) THEN
            IF (methodPrm%diagnose == 1) PRINT *, &
               'tsfc_roof abs. diff.:', MAXVAL(ABS(heatState_out%tsfc_roof - tsfc0_out_roof)), &
               MAXLOC(ABS(heatState_out%tsfc_roof - tsfc0_out_roof))
            dif_tsfc_iter = MAX(MAXVAL(ABS(heatState_out%tsfc_roof - tsfc0_out_roof)), dif_tsfc_iter)
            IF (methodPrm%diagnose == 1) PRINT *, &
               'tsfc_wall abs. diff.:', MAXVAL(ABS(heatState_out%tsfc_wall - tsfc0_out_wall)), &
               MAXLOC(ABS(heatState_out%tsfc_wall - tsfc0_out_wall))
            dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc0_out_wall - heatState_out%tsfc_wall)), dif_tsfc_iter)
         END IF

         ! ====test===
         ! see if this converges better
         ratio_iter = .4
         ! ratio_iter = .3
         heatState_out%tsfc_surf = (tsfc0_out_surf*(1 - ratio_iter) + heatState_out%tsfc_surf*ratio_iter)
         heatState_out%tsfc_roof = (tsfc0_out_roof*(1 - ratio_iter) + heatState_out%tsfc_roof*ratio_iter)
         heatState_out%tsfc_wall = (tsfc0_out_wall*(1 - ratio_iter) + heatState_out%tsfc_wall*ratio_iter)
         ! =======test end=======

         ! PRINT *, 'tsfc_surf after qh_cal', TSfc_QH_surf

         !============ surface-level diagonostics end ===============

         ! force quit do-while, i.e., skip iteration and use NARP for Tsurf calculation
         ! if (NetRadiationMethod < 10 .or. NetRadiationMethod > 100) exit

         ! Test if sensible heat fluxes converge in iterations
         ! if (abs(QH - QH_Init) > 0.1) then
         ! IF (ABS(Ts_iter - TSfc_C) > 0.1) THEN
         !    flag_converge = .FALSE.
         ! ELSE
         !    flag_converge = .TRUE.
         !    PRINT *, 'Iteration done in', i_iter, ' iterations'
         !    PRINT *, ' Ts_iter: ', Ts_iter, ' TSfc_C: ', TSfc_C
         ! END IF
         ! IF (MINVAL(ABS(TSfc_QH_surf - tsfc_surf)) > 0.1) THEN
         ! IF (ABS(qh_residual - qh_resist) > .2) THEN
         IF (dif_tsfc_iter > .1) THEN
            flag_converge = .FALSE.
         ELSE
            flag_converge = .TRUE.
            ! PRINT *, 'Iteration done in', i_iter, ' iterations'
            ! PRINT *, ' qh_residual: ', qh_residual, ' qh_resist: ', qh_resist
            ! PRINT *, ' dif_qh: ', ABS(qh_residual - qh_resist)
            ! PRINT *, ' abs. dif_tsfc: ', dif_tsfc_iter

         END IF

         i_iter = i_iter + 1
         ! force quit do-while loop if not convergent after 100 iterations
         IF (methodPrm%Diagnose == 1 .AND. i_iter == max_iter) THEN
            ! PRINT *, 'Iteration did not converge in', i_iter, ' iterations'
            ! PRINT *, ' qh_residual: ', qh_residual, ' qh_resist: ', qh_resist
            ! PRINT *, ' dif_qh: ', ABS(qh_residual - qh_resist)
            ! PRINT *, ' Ts_iter: ', Ts_iter, ' TSfc_C: ', TSfc_C
            ! PRINT *, ' abs. dif_tsfc: ', dif_tsfc_iter
            ! exit
         END IF

         ! Ts_iter = TSfc_C
         ! l_mod_iter = l_mod
         ! PRINT *, '========================='
         ! PRINT *, ''
         !==============main calculation end=======================
      END DO ! end iteration for tsurf calculations

      !==============================================================
      ! Calculate diagnostics: these variables are decoupled from the main SUEWS calculation

      !============ roughness sub-layer diagonostics ===============
      IF (methodPrm%Diagnose == 1) WRITE (*, *) 'Calling RSLProfile...'
      ! CALL RSLProfile( &
      !    DiagMethod, &
      !    zH, z0m, zdm, z0v, &
      !    L_MOD, sfr_surf, FAI, PAI, &
      !    StabilityMethod, RA_h, &
      !    avcp, lv_J_kg, avdens, &
      !    avU1, Temp_C, avRH, Press_hPa, z, qh, qe, & ! input
      !    T2_C, q2_gkg, U10_ms, RH2, & !output
      !    dataoutLineRSL) ! output

      CALL RSLProfile_DTS( &
         methodPrm%DiagMethod, &
         zH, z0m, zdm, z0v, &
         L_MOD, &
         pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
         FAI, PAI, &
         methodPrm%StabilityMethod, RA_h, &
         avcp, lv_J_kg, avdens, &
         forcing%U, forcing%Temp_C, forcing%RH, forcing%pres, siteInfo%z, qh, qe, & ! input
         T2_C, q2_gkg, U10_ms, RH2, & !output
         dataoutLineRSL) ! output

      ! ============ BIOGENIC CO2 FLUX =======================
      ! CALL SUEWS_cal_BiogenCO2( &
      !    alpha_bioCO2, alpha_enh_bioCO2, kdown, avRh, beta_bioCO2, beta_enh_bioCO2, & ! input:
      !    dectime, Diagnose, EmissionsMethod, Fc_anthro, g_max, g_k, g_q_base, g_q_shape, &
      !    g_t, g_sm, gfunc, gsmodel, id, it, Kmax, LAI_id_next, LAIMin, &
      !    LAIMax, MaxConductance, min_res_bioCO2, Press_hPa, resp_a, &
      !    resp_b, S1, S2, sfr_surf, SMDMethod, SnowFrac, t2_C, Temp_C, theta_bioCO2, TH, TL, vsmd, xsmd, &
      !    Fc, Fc_biogen, Fc_photo, Fc_respi) ! output:

      CALL SUEWS_cal_BiogenCO2_DTS( &
         evetrPrm%bioco2%alpha_bioco2, dectrPrm%bioco2%alpha_bioco2, grassPrm%bioco2%alpha_bioco2, &
         evetrPrm%bioco2%alpha_enh_bioco2, dectrPrm%bioco2%alpha_enh_bioco2, grassPrm%bioco2%alpha_enh_bioco2, &
         forcing%kdown, forcing%RH, &
         evetrPrm%bioco2%beta_bioCO2, dectrPrm%bioco2%beta_bioCO2, grassPrm%bioco2%beta_bioCO2, &
         evetrPrm%bioco2%beta_enh_bioco2, dectrPrm%bioco2%beta_enh_bioco2, grassPrm%bioco2%beta_enh_bioco2, &
         dectime, methodPrm%Diagnose, methodPrm%EmissionsMethod, Fc_anthro, &
         conductancePrm%g_max, conductancePrm%g_k, conductancePrm%g_q_base, conductancePrm%g_q_shape, &
         conductancePrm%g_t, conductancePrm%g_sm, gfunc, conductancePrm%gsmodel, &
         timer%id, timer%it, conductancePrm%Kmax, &
         phenState_next%LAI_id, &
         evetrPrm%lai%laimin, dectrPrm%lai%laimin, grassPrm%lai%laimin, &
         evetrPrm%lai%laimax, dectrPrm%lai%laimax, grassPrm%lai%laimax, &
         evetrPrm%MaxConductance, dectrPrm%MaxConductance, grassPrm%MaxConductance, &
         evetrPrm%bioco2%min_res_bioCO2, dectrPrm%bioco2%min_res_bioCO2, grassPrm%bioco2%min_res_bioCO2, &
         forcing%Pres, &
         evetrPrm%bioco2%resp_a, dectrPrm%bioco2%resp_a, grassPrm%bioco2%resp_a, &
         evetrPrm%bioco2%resp_b, dectrPrm%bioco2%resp_b, grassPrm%bioco2%resp_b, &
         conductancePrm%S1, conductancePrm%S2, &
         pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr, &
         methodPrm%SMDMethod, snowState%SnowFrac, &
         t2_C, forcing%Temp_C, &
         evetrPrm%bioco2%theta_bioCO2, dectrPrm%bioco2%theta_bioCO2, grassPrm%bioco2%theta_bioco2, &
         conductancePrm%TH, conductancePrm%TL, vsmd, forcing%xsmd, &
         Fc, Fc_biogen, Fc_photo, Fc_respi) ! output:

      ! calculations of diagnostics end
      !==============================================================

      !==============================================================
      ! update inout variables with new values
      ohmState%qn_av = ohmState_next%qn_av
      ohmState%dqndt = ohmState_next%dqndt
      ohmState%qn_s_av = ohmState_next%qn_s_av
      ohmState%dqnsdt = ohmState_next%dqnsdt
      snowState%SnowfallCum = snowState_next%SnowfallCum
      snowState%SnowAlb = snowState_next%SnowAlb
      snowState%IceFrac = snowState_next%IceFrac
      snowState%SnowWater = snowState_next%SnowWater
      snowState%SnowDens = snowState_next%SnowDens
      snowState%SnowFrac = snowState_next%SnowFrac
      snowState%SnowPack = snowState_next%SnowPack

      hydroState%soilstore_surf = hydroState_next%soilstore_surf
      hydroState%state_surf = hydroState_next%state_surf
      phenState%alb = phenState_next%alb
      phenState%GDD_id = phenState_next%GDD_id
      phenState%SDD_id = phenState_next%SDD_id
      phenState%LAI_id = phenState_next%LAI_id
      phenState%DecidCap_id = phenState_next%DecidCap_id
      phenState%albDecTr_id = phenState_next%albDecTr_id
      phenState%albEveTr_id = phenState_next%albEveTr_id
      phenState%albGrass_id = phenState_next%albGrass_id
      phenState%porosity_id = phenState_next%porosity_id
      phenState%StoreDrainPrm = phenState_next%StoreDrainPrm
      ! Tair_av = Tair_av_next
      forcing%Tair = Tair_av_next
      phenState%Tmin_id = phenState_next%Tmin_id
      phenState%Tmax_id = phenState_next%Tmax_id
      phenState%lenday_id = phenState_next%lenday_id
      anthroHeatState%HDD_id = anthroHeatState_next%HDD_id
      hydroState%WUDay_id = hydroState_next%WUDay_id

      IF (methodPrm%StorageHeatMethod == 5) THEN
         ! ESTM_ehc related
         heatState%temp_roof = heatState_out%temp_roof
         heatState%temp_wall = heatState_out%temp_wall
         heatState%temp_surf = heatState_out%temp_surf
         heatState%tsfc_roof = heatState_out%tsfc_roof
         heatState%tsfc_wall = heatState_out%tsfc_wall
         heatState%tsfc_surf = heatState_out%tsfc_surf

         hydroState%soilstore_roof = hydroState_next%soilstore_roof
         hydroState%state_roof = hydroState_next%state_roof
         hydroState%soilstore_wall = hydroState_next%soilstore_wall
         hydroState%state_wall = hydroState_next%state_wall
      END IF

      !==============================================================
      ! update inout variables with new values (to be compatible with original interface)
      Tair_av = Tair_av_next

      qn_av = ohmState_next%qn_av
      dqndt = ohmState_next%dqndt
      qn_s_av = ohmState_next%qn_s_av
      dqnsdt = ohmState_next%dqnsdt

      SnowfallCum = snowState_next%SnowfallCum
      SnowAlb = snowState_next%SnowAlb
      IceFrac = snowState_next%IceFrac
      SnowWater = snowState_next%SnowWater
      SnowDens = snowState_next%SnowDens
      SnowFrac = snowState_next%SnowFrac
      SnowPack = snowState_next%SnowPack

      soilstore_surf = hydroState_next%soilstore_surf
      state_surf = hydroState_next%state_surf
      WUDay_id = hydroState_next%WUDay_id

      alb = phenState_next%alb
      GDD_id = phenState_next%GDD_id
      SDD_id = phenState_next%SDD_id
      LAI_id = phenState_next%LAI_id
      Tmin_id = phenState_next%Tmin_id
      Tmax_id = phenState_next%Tmax_id
      lenday_id = phenState_next%lenday_id
      DecidCap_id = phenState_next%DecidCap_id
      albDecTr_id = phenState_next%albDecTr_id
      albEveTr_id = phenState_next%albEveTr_id
      albGrass_id = phenState_next%albGrass_id
      porosity_id = phenState_next%porosity_id
      StoreDrainPrm = phenState_next%StoreDrainPrm
      ! Tair_av = Tair_av_next

      HDD_id = anthroHeatState_next%HDD_id

      IF (methodPrm%StorageHeatMethod == 5) THEN
         ! ESTM_ehc related
         temp_roof = heatState_out%temp_roof
         temp_wall = heatState_out%temp_wall
         temp_surf = heatState_out%temp_surf
         tsfc_roof = heatState_out%tsfc_roof
         tsfc_wall = heatState_out%tsfc_wall
         tsfc_surf = heatState_out%tsfc_surf

         soilstore_roof = hydroState_next%soilstore_roof
         state_roof = hydroState_next%state_roof
         soilstore_wall = hydroState_next%soilstore_wall
         state_wall = hydroState_next%state_wall
      END IF

      !==============use SOLWEIG to get localised radiation flux==================
      ! if (sfr_surf(BldgSurf) > 0) then
      !    CALL SOLWEIG_cal_main(id, it, dectime, 0.8d0, FAI, avkdn, ldown, Temp_C, avRh, Press_hPa, TSfc_C, &
      !    lat, ZENITH_deg, azimuth, 1.d0, alb(1), alb(2), emis(1), emis(2), bldgH, dataOutLineSOLWEIG)
      ! else
      !    dataOutLineSOLWEIG = set_nan(dataOutLineSOLWEIG)
      ! endif

      !==============use BEERS to get localised radiation flux==================
      ! TS 14 Jan 2021: BEERS is a modified version of SOLWEIG
      IF (sfr_surf(BldgSurf) > 0) THEN
         PAI = sfr_surf(2)/SUM(sfr_surf(1:2))
         ! CALL BEERS_cal_main(iy, id, dectime, PAI, FAI, kdown, ldown, Temp_C, avrh, &
         !                     Press_hPa, TSfc_C, lat, lng, alt, timezone, zenith_deg, azimuth, &
         !                     alb(1), alb(2), emis(1), emis(2), &
         !                     dataOutLineBEERS) ! output
         CALL BEERS_cal_main(timer%iy, timer%id, dectime, PAI, FAI, forcing%kdown, ldown, forcing%Temp_C, forcing%RH, &
                           forcing%Pres, TSfc_C, siteInfo%lat, siteInfo%lon, siteInfo%alt, siteInfo%timezone, zenith_deg, azimuth, &
                             phenState%alb(1), phenState%alb(2), pavedPrm%emis, bldgPrm%emis, &
                             dataOutLineBEERS) ! output
         ! CALL SOLWEIG_cal_main(id, it, dectime, 0.8d0, FAI, avkdn, ldown, Temp_C, avRh, Press_hPa, TSfc_C, &
         ! lat, ZENITH_deg, azimuth, 1.d0, alb(1), alb(2), emis(1), emis(2), bldgH, dataOutLineSOLWEIG)
      ELSE
         dataOutLineBEERS = set_nan(dataOutLineBEERS)
      END IF

      !==============translation of  output variables into output array===========
      ! CALL SUEWS_update_outputLine( &
      !    AdditionalWater, alb, kdown, U10_ms, azimuth, & !input
      !    chSnow_per_interval, dectime, &
      !    drain_per_tstep, QE_LUMPS, ev_per_tstep, wu_ext, Fc, Fc_build, fcld, &
      !    Fc_metab, Fc_photo, Fc_respi, Fc_point, Fc_traff, FlowChange, &
      !    QH_LUMPS, id, imin, wu_int, it, iy, &
      !    kup, LAI_id, ldown, l_mod, lup, mwh, &
      !    MwStore, &
      !    nsh_real, NWstate_per_tstep, Precip, q2_gkg, &
      !    qe, qf, qh, qh_resist, Qm, QmFreez, &
      !    QmRain, qn, qn_snow, qn_snowfree, qs, RA_h, &
      !    RS, RH2, runoffAGimpervious, runoffAGveg, &
      !    runoff_per_tstep, runoffPipes, runoffSoil_per_tstep, &
      !    runoffWaterBody, sfr_surf, smd, smd_nsurf, SnowAlb, SnowRemoval, &
      !    state_surf_next, state_per_tstep, surf_chang_per_tstep, swe, t2_C, TSfc_C, &
      !    tot_chang_per_tstep, tsurf, UStar, &
      !    wu_surf, &
      !    z0m, zdm, zenith_deg, &
      !    datetimeLine, dataOutLineSUEWS) !output
      CALL SUEWS_update_outputLine( &
         AdditionalWater, phenState%alb, forcing%kdown, U10_ms, azimuth, & !input
         chSnow_per_interval, dectime, &
         drain_per_tstep, QE_LUMPS, ev_per_tstep, wu_ext, Fc, Fc_build, fcld, &
         Fc_metab, Fc_photo, Fc_respi, Fc_point, Fc_traff, siteInfo%FlowChange, &
         QH_LUMPS, timer%id, timer%imin, wu_int, timer%it, timer%iy, &
         kup, phenState%LAI_id, ldown, l_mod, lup, mwh, &
         MwStore, &
         nsh_real, NWstate_per_tstep, Precip, q2_gkg, &
         qe, qf, qh, qh_resist, Qm, QmFreez, &
         QmRain, qn, qn_snow, qn_snowfree, qs, RA_h, &
         RS, RH2, runoffAGimpervious, runoffAGveg, &
         runoff_per_tstep, runoffPipes, runoffSoil_per_tstep, &
         runoffWaterBody, &
         sfr_surf, &
         smd, smd_nsurf, snowState%SnowAlb, SnowRemoval, &
         hydroState%state_surf, state_per_tstep, surf_chang_per_tstep, swe, t2_C, TSfc_C, &
         tot_chang_per_tstep, tsurf, UStar, &
         wu_surf, &
         z0m, zdm, zenith_deg, &
         datetimeLine, dataOutLineSUEWS) !output

      ! CALL ESTMExt_update_outputLine( &
      !    iy, id, it, imin, dectime, nlayer, & !input
      !    tsfc_out_surf, qs_surf, &
      !    tsfc_out_roof, &
      !    QN_roof, &
      !    QS_roof, &
      !    QE_roof, &
      !    QH_roof, &
      !    state_roof, &
      !    soilstore_roof, &
      !    tsfc_out_wall, &
      !    QN_wall, &
      !    QS_wall, &
      !    QE_wall, &
      !    QH_wall, &
      !    state_wall, &
      !    soilstore_wall, &
      !    datetimeLine, dataOutLineESTMExt) !output
      CALL ESTMExt_update_outputLine( &
         timer%iy, timer%id, timer%it, timer%imin, dectime, nlayer, & !input
         heatState_out%tsfc_surf, qs_surf, &
         heatState_out%tsfc_roof, &
         QN_roof, &
         QS_roof, &
         QE_roof, &
         QH_roof, &
         hydroState%state_roof, &
         hydroState%soilstore_roof, &
         heatState_out%tsfc_wall, &
         QN_wall, &
         QS_wall, &
         QE_wall, &
         QH_wall, &
         hydroState%state_wall, &
         hydroState%soilstore_wall, &
         datetimeLine, dataOutLineESTMExt) !output

      ! daily state_id:
      ! CALL update_DailyStateLine( &
      !    it, imin, nsh_real, & !input
      !    GDD_id, HDD_id, LAI_id, &
      !    SDD_id, &
      !    Tmin_id, Tmax_id, lenday_id, &
      !    DecidCap_id, &
      !    albDecTr_id, &
      !    albEveTr_id, &
      !    albGrass_id, &
      !    porosity_id, &
      !    WUDay_id, &
      !    VegPhenLumps, &
      !    SnowAlb, SnowDens, &
      !    a1, a2, a3, &
      !    dataOutLineDailyState) !out
      CALL update_DailyStateLine( &
         timer%it, timer%imin, nsh_real, & !input
         phenState%GDD_id, anthroHeatState%HDD_id, phenState%LAI_id, &
         phenState%SDD_id, &
         phenState%Tmin_id, phenState%Tmax_id, phenState%lenday_id, &
         phenState%DecidCap_id, &
         phenState%albDecTr_id, &
         phenState%albEveTr_id, &
         phenState%albGrass_id, &
         phenState%porosity_id, &
         hydroState%WUDay_id, &
         VegPhenLumps, &
         snowState%SnowAlb, snowState%SnowDens, &
         a1, a2, a3, &
         dataOutLineDailyState) !out

      !==============translation end ================

      dataoutlineDebug = &
         [tsfc0_out_surf, &
          qn_surf, qs_surf, qe0_surf, qe_surf, qh_surf, & ! energy balance
          wu_surf, ev0_surf, ev_surf, drain_surf, &
          hydroState_prev%state_surf, hydroState_next%state_surf, &
          hydroState_prev%soilstore_surf, hydroState_next%soilstore_surf, & ! water balance
          RS, RA_h, RB, RAsnow, rss_surf, & ! for debugging QE
          vsmd, S1/G_sm + S2, G_sm, G_sm*(vsmd - S1/G_sm + S2), & ! debug g_smd
          g_kdown, g_dq, g_ta, g_smd, g_lai, & ! for debugging RS: surface resistance
          vpd_hPa, lv_J_kg, avdens, avcp, s_hPa, psyc_hPa, & ! for debugging QE
          i_iter*1D0, &
          FAIBldg_use, FAIEveTree_use, FAIDecTree_use, FAI, &
          ohmState%dqndt]

      !==============output==========================
      CALL output_line_init(output_line_suews)
      output_line_suews%datetimeLine = datetimeLine
      output_line_suews%dataOutLineSUEWS = [datetimeLine, dataOutLineSUEWS]
      output_line_suews%dataOutLineESTMExt = [datetimeLine, dataOutLineESTMExt]
      output_line_suews%dataOutLineDailyState = [datetimeLine, dataOutLineDailyState]
      output_line_suews%dataOutLineBEERS = [datetimeLine, dataOutLineBEERS]
      output_line_suews%dataOutLineDebug = [datetimeLine, dataOutLineDebug]
      output_line_suews%dataOutLineSPARTACUS = [datetimeLine, dataOutLineSPARTACUS]
      output_line_suews%dataOutLineSnow = [datetimeLine, dataOutLineSnow]
      output_line_suews%dataoutLineRSL = [datetimeLine, dataOutLineRSL]
      output_line_suews%dataOutLineESTM = [datetimeLine, dataOutLineESTM]

   END SUBROUTINE SUEWS_cal_Main_DTS
! ================================================================================

! ===================ANTHROPOGENIC HEAT + CO2 FLUX================================
   SUBROUTINE SUEWS_cal_AnthropogenicEmission( &
      AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, AH_SLOPE_Heating, CO2PointSource, & ! input:
      dayofWeek_id, DLS, EF_umolCO2perJ, EmissionsMethod, EnEF_v_Jkm, &
      FcEF_v_kgkm, FrFossilFuel_Heat, FrFossilFuel_NonHeat, HDD_id, HumActivity_24hr, &
      imin, it, MaxFCMetab, MaxQFMetab, MinFCMetab, MinQFMetab, &
      PopDensDaytime, PopDensNighttime, PopProf_24hr, QF, QF0_BEU, Qf_A, Qf_B, Qf_C, &
      QF_obs, QF_SAHP, SurfaceArea, BaseT_Cooling, BaseT_Heating, &
      Temp_C, TrafficRate, TrafficUnits, TraffProf_24hr, &
      Fc_anthro, Fc_build, Fc_metab, Fc_point, Fc_traff) ! output:

      IMPLICIT NONE

      ! INTEGER, INTENT(in)::Diagnose
      INTEGER, INTENT(in) :: DLS ! daylighting savings
      INTEGER, INTENT(in) :: EmissionsMethod !0 - Use values in met forcing file, or default QF;1 - Method according to Loridan et al. (2011) : SAHP; 2 - Method according to Jarvi et al. (2011)   : SAHP_2
      ! INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: it ! hour [H]
      INTEGER, INTENT(in) :: imin ! minutes [M]
      ! INTEGER, INTENT(in) :: nsh
      INTEGER, DIMENSION(3), INTENT(in) :: dayofWeek_id ! 1 - day of week; 2 - month; 3 - season

      REAL(KIND(1D0)), DIMENSION(6, 2), INTENT(in) :: HDD_id ! Heating Degree Days (see SUEWS_DailyState.f95)

      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: AH_MIN ! miniumum anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: AH_SLOPE_Heating ! heating slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: AH_SLOPE_Cooling ! cooling slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: FcEF_v_kgkm ! CO2 Emission factor [kg km-1]
      ! REAL(KIND(1d0)), DIMENSION(2), INTENT(in)::NumCapita
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: PopDensDaytime ! Daytime population density [people ha-1] (i.e. workers)
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: QF0_BEU ! Fraction of base value coming from buildings [-]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: Qf_A ! Base value for QF [W m-2]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: Qf_B ! Parameter related to heating degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: Qf_C ! Parameter related to cooling degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: BaseT_Heating ! base temperatrue for heating degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: BaseT_Cooling ! base temperature for cooling degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: TrafficRate ! Traffic rate [veh km m-2 s-1]

      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: AHProf_24hr ! diurnal profile of anthropogenic heat flux (AVERAGE of the multipliers is equal to 1) [-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: HumActivity_24hr ! diurnal profile of human activity [-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: TraffProf_24hr ! diurnal profile of traffic activity calculation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: PopProf_24hr ! diurnal profile of population [-]

      REAL(KIND(1D0)), INTENT(in) :: CO2PointSource ! point source [kgC day-1]
      REAL(KIND(1D0)), INTENT(in) :: EF_umolCO2perJ !co2 emission factor [umol J-1]
      REAL(KIND(1D0)), INTENT(in) :: EnEF_v_Jkm ! energy emission factor [J K m-1]
      REAL(KIND(1D0)), INTENT(in) :: FrFossilFuel_Heat ! fraction of fossil fuel heat [-]
      REAL(KIND(1D0)), INTENT(in) :: FrFossilFuel_NonHeat ! fraction of fossil fuel non heat [-]
      REAL(KIND(1D0)), INTENT(in) :: MaxFCMetab ! maximum FC metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: MaxQFMetab ! maximum QF Metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: MinFCMetab ! minimum QF metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: MinQFMetab ! minimum FC metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: PopDensNighttime ! nighttime population density [ha-1] (i.e. residents)
      REAL(KIND(1D0)), INTENT(in) :: QF_obs ! observed anthropogenic heat flux from met forcing file when EmissionMethod=0 [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Temp_C ! air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: TrafficUnits ! traffic units choice [-]

      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(in)::sfr_surf
      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(in)::SnowFrac
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea !surface area [m-2]

      REAL(KIND(1D0)), INTENT(out) :: Fc_anthro ! anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_build ! co2 emission from building component [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_metab ! co2 emission from metabolism component [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_point ! co2 emission from point source [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_traff ! co2 emission from traffic component [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: QF ! anthropogeic heat flux when EmissionMethod = 0 [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: QF_SAHP !total anthropogeic heat flux when EmissionMethod is not 0 [W m-2]

      INTEGER, PARAMETER :: notUsedI = -999
      REAL(KIND(1D0)), PARAMETER :: notUsed = -999

      IF (EmissionsMethod == 0) THEN ! use observed qf
         qf = QF_obs
      ELSEIF ((EmissionsMethod > 0 .AND. EmissionsMethod <= 6) .OR. EmissionsMethod >= 11) THEN
         CALL AnthropogenicEmissions( &
            CO2PointSource, EmissionsMethod, &
            it, imin, DLS, DayofWeek_id, &
            EF_umolCO2perJ, FcEF_v_kgkm, EnEF_v_Jkm, TrafficUnits, &
            FrFossilFuel_Heat, FrFossilFuel_NonHeat, &
            MinFCMetab, MaxFCMetab, MinQFMetab, MaxQFMetab, &
            PopDensDaytime, PopDensNighttime, &
            Temp_C, HDD_id, Qf_A, Qf_B, Qf_C, &
            AH_MIN, AH_SLOPE_Heating, AH_SLOPE_Cooling, &
            BaseT_Heating, BaseT_Cooling, &
            TrafficRate, &
            QF0_BEU, QF_SAHP, &
            Fc_anthro, Fc_metab, Fc_traff, Fc_build, Fc_point, &
            AHProf_24hr, HumActivity_24hr, TraffProf_24hr, PopProf_24hr, SurfaceArea)

      ELSE
         CALL ErrorHint(73, 'RunControl.nml:EmissionsMethod unusable', notUsed, notUsed, EmissionsMethod)
      END IF

      IF (EmissionsMethod >= 1) qf = QF_SAHP

      IF (EmissionsMethod >= 0 .AND. EmissionsMethod <= 6) THEN
         Fc_anthro = 0
         Fc_metab = 0
         Fc_traff = 0
         Fc_build = 0
         Fc_point = 0
      END IF

   END SUBROUTINE SUEWS_cal_AnthropogenicEmission

   SUBROUTINE SUEWS_cal_AnthropogenicEmission_DTS( &
      AH_MIN_working, AH_MIN_holiday, &
      AHProf_24hr_working, AHProf_24hr_holiday, &
      AH_SLOPE_Cooling_working, AH_SLOPE_Cooling_holiday, &
      AH_SLOPE_Heating_working, AH_SLOPE_Heating_holiday, &
      CO2PointSource, & ! input:
      dayofWeek_id, DLS, EF_umolCO2perJ, EmissionsMethod, EnEF_v_Jkm, &
      FcEF_v_kgkm, FrFossilFuel_Heat, FrFossilFuel_NonHeat, HDD_id, &
      HumActivity_24hr_working, HumActivity_24hr_holiday, &
      imin, it, MaxFCMetab, MaxQFMetab, MinFCMetab, MinQFMetab, &
      PopDensDaytime_working, PopDensDaytime_holiday, PopDensNighttime, &
      PopProf_24hr_working, PopProf_24hr_holiday, QF, &
      QF0_BEU_working, QF0_BEU_holiday, &
      Qf_A_working, Qf_A_holiday, &
      Qf_B_working, Qf_B_holiday, &
      Qf_C_working, Qf_C_holiday, &
      QF_obs, QF_SAHP, SurfaceArea, &
      BaseT_Cooling_working, BaseT_Cooling_holiday, &
      BaseT_Heating_working, BaseT_Heating_holiday, &
      Temp_C, TrafficRate_working, TrafficRate_holiday, TrafficUnits, &
      TraffProf_24hr_working, TraffProf_24hr_holiday, &
      Fc_anthro, Fc_build, Fc_metab, Fc_point, Fc_traff) ! output:

      IMPLICIT NONE

      ! INTEGER, INTENT(in)::Diagnose
      INTEGER, INTENT(in) :: DLS ! daylighting savings
      INTEGER, INTENT(in) :: EmissionsMethod !0 - Use values in met forcing file, or default QF;1 - Method according to Loridan et al. (2011) : SAHP; 2 - Method according to Jarvi et al. (2011)   : SAHP_2
      ! INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: it ! hour [H]
      INTEGER, INTENT(in) :: imin ! minutes [M]
      ! INTEGER, INTENT(in) :: nsh
      INTEGER, DIMENSION(3), INTENT(in) :: dayofWeek_id ! 1 - day of week; 2 - month; 3 - season

      REAL(KIND(1D0)), DIMENSION(6, 2), INTENT(in) :: HDD_id ! Heating Degree Days (see SUEWS_DailyState.f95)

      REAL(KIND(1D0)), INTENT(in) :: AH_MIN_working
      REAL(KIND(1D0)), INTENT(in) :: AH_MIN_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: AH_MIN ! miniumum anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: AH_SLOPE_Heating_working
      REAL(KIND(1D0)), INTENT(in) :: AH_SLOPE_Heating_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: AH_SLOPE_Heating ! heating slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), INTENT(in) :: AH_SLOPE_Cooling_working
      REAL(KIND(1D0)), INTENT(in) :: AH_SLOPE_Cooling_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: AH_SLOPE_Cooling ! cooling slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: FcEF_v_kgkm ! CO2 Emission factor [kg km-1]
      ! REAL(KIND(1d0)), DIMENSION(2), INTENT(in)::NumCapita

      REAL(KIND(1D0)), INTENT(in) :: PopDensDaytime_working
      REAL(KIND(1D0)), INTENT(in) :: PopDensDaytime_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: PopDensDaytime ! Daytime population density [people ha-1] (i.e. workers)

      REAL(KIND(1D0)), INTENT(in) :: QF0_BEU_working
      REAL(KIND(1D0)), INTENT(in) :: QF0_BEU_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: QF0_BEU ! Fraction of base value coming from buildings [-]
      REAL(KIND(1D0)), INTENT(in) :: Qf_A_working
      REAL(KIND(1D0)), INTENT(in) :: Qf_A_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_A ! Base value for QF [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Qf_B_working
      REAL(KIND(1D0)), INTENT(in) :: Qf_B_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_B ! Parameter related to heating degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), INTENT(in) :: Qf_C_working
      REAL(KIND(1D0)), INTENT(in) :: Qf_C_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_C ! Parameter related to cooling degree days [W m-2 K-1 (Cap ha-1 )-1]

      REAL(KIND(1D0)), INTENT(in) :: BaseT_Heating_working
      REAL(KIND(1D0)), INTENT(in) :: BaseT_Heating_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Heating ! base temperatrue for heating degree day [degC]
      REAL(KIND(1D0)), INTENT(in) :: BaseT_Cooling_working
      REAL(KIND(1D0)), INTENT(in) :: BaseT_Cooling_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Cooling ! base temperature for cooling degree day [degC]

      REAL(KIND(1D0)), INTENT(in) :: TrafficRate_working
      REAL(KIND(1D0)), INTENT(in) :: TrafficRate_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: TrafficRate ! Traffic rate [veh km m-2 s-1]

      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: AHProf_24hr_working
      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: AHProf_24hr_holiday
      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: AHProf_24hr ! diurnal profile of anthropogenic heat flux (AVERAGE of the multipliers is equal to 1) [-]

      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: HumActivity_24hr_working
      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: HumActivity_24hr_holiday
      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: HumActivity_24hr ! diurnal profile of human activity [-]

      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: TraffProf_24hr_working
      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: TraffProf_24hr_holiday
      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: TraffProf_24hr ! diurnal profile of traffic activity calculation[-]

      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: PopProf_24hr_working
      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: PopProf_24hr_holiday
      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: PopProf_24hr ! diurnal profile of population [-]

      REAL(KIND(1D0)), INTENT(in) :: CO2PointSource ! point source [kgC day-1]
      REAL(KIND(1D0)), INTENT(in) :: EF_umolCO2perJ !co2 emission factor [umol J-1]
      REAL(KIND(1D0)), INTENT(in) :: EnEF_v_Jkm ! energy emission factor [J K m-1]
      REAL(KIND(1D0)), INTENT(in) :: FrFossilFuel_Heat ! fraction of fossil fuel heat [-]
      REAL(KIND(1D0)), INTENT(in) :: FrFossilFuel_NonHeat ! fraction of fossil fuel non heat [-]
      REAL(KIND(1D0)), INTENT(in) :: MaxFCMetab ! maximum FC metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: MaxQFMetab ! maximum QF Metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: MinFCMetab ! minimum QF metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: MinQFMetab ! minimum FC metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: PopDensNighttime ! nighttime population density [ha-1] (i.e. residents)
      REAL(KIND(1D0)), INTENT(in) :: QF_obs ! observed anthropogenic heat flux from met forcing file when EmissionMethod=0 [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Temp_C ! air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: TrafficUnits ! traffic units choice [-]

      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(in)::sfr_surf
      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(in)::SnowFrac
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea !surface area [m-2]

      REAL(KIND(1D0)), INTENT(out) :: Fc_anthro ! anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_build ! co2 emission from building component [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_metab ! co2 emission from metabolism component [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_point ! co2 emission from point source [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_traff ! co2 emission from traffic component [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: QF ! anthropogeic heat flux when EmissionMethod = 0 [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: QF_SAHP !total anthropogeic heat flux when EmissionMethod is not 0 [W m-2]

      INTEGER, PARAMETER :: notUsedI = -999
      REAL(KIND(1D0)), PARAMETER :: notUsed = -999

      AH_MIN(1) = AH_MIN_working
      AH_MIN(2) = AH_MIN_holiday
      AH_SLOPE_Heating(1) = AH_SLOPE_Heating_working
      AH_SLOPE_Heating(2) = AH_SLOPE_Heating_holiday
      AH_SLOPE_Cooling(1) = AH_SLOPE_Cooling_working
      AH_SLOPE_Cooling(2) = AH_SLOPE_Cooling_holiday

      TrafficRate(1) = TrafficRate_working
      TrafficRate(2) = TrafficRate_holiday

      PopDensDaytime(1) = PopDensDaytime_working
      PopDensDaytime(2) = PopDensDaytime_holiday
      QF0_BEU(1) = QF0_BEU_working
      QF0_BEU(2) = QF0_BEU_holiday
      Qf_A(1) = Qf_A_working
      Qf_A(2) = Qf_A_holiday
      Qf_B(1) = Qf_B_working
      Qf_B(2) = Qf_B_holiday
      Qf_C(1) = Qf_C_working
      Qf_C(2) = Qf_C_holiday
      BaseT_Heating(1) = BaseT_Heating_working
      BaseT_Heating(2) = BaseT_Heating_holiday
      BaseT_Cooling(1) = BaseT_Cooling_working
      BaseT_Cooling(2) = BaseT_Cooling_holiday

      AHProf_24hr(:, 1) = AHProf_24hr_working
      AHProf_24hr(:, 2) = AHProf_24hr_holiday
      HumActivity_24hr(:, 1) = HumActivity_24hr_working
      HumActivity_24hr(:, 2) = HumActivity_24hr_holiday
      PopProf_24hr(:, 1) = PopProf_24hr_working
      PopProf_24hr(:, 2) = PopProf_24hr_holiday
      TraffProf_24hr(:, 1) = TraffProf_24hr_working
      TraffProf_24hr(:, 2) = TraffProf_24hr_holiday

      IF (EmissionsMethod == 0) THEN ! use observed qf
         qf = QF_obs
      ELSEIF ((EmissionsMethod > 0 .AND. EmissionsMethod <= 6) .OR. EmissionsMethod >= 11) THEN
         CALL AnthropogenicEmissions( &
            CO2PointSource, EmissionsMethod, &
            it, imin, DLS, DayofWeek_id, &
            EF_umolCO2perJ, FcEF_v_kgkm, EnEF_v_Jkm, TrafficUnits, &
            FrFossilFuel_Heat, FrFossilFuel_NonHeat, &
            MinFCMetab, MaxFCMetab, MinQFMetab, MaxQFMetab, &
            PopDensDaytime, PopDensNighttime, &
            Temp_C, HDD_id, Qf_A, Qf_B, Qf_C, &
            AH_MIN, AH_SLOPE_Heating, AH_SLOPE_Cooling, &
            BaseT_Heating, BaseT_Cooling, &
            TrafficRate, &
            QF0_BEU, QF_SAHP, &
            Fc_anthro, Fc_metab, Fc_traff, Fc_build, Fc_point, &
            AHProf_24hr, HumActivity_24hr, TraffProf_24hr, PopProf_24hr, SurfaceArea)

      ELSE
         CALL ErrorHint(73, 'RunControl.nml:EmissionsMethod unusable', notUsed, notUsed, EmissionsMethod)
      END IF

      IF (EmissionsMethod >= 1) qf = QF_SAHP

      IF (EmissionsMethod >= 0 .AND. EmissionsMethod <= 6) THEN
         Fc_anthro = 0
         Fc_metab = 0
         Fc_traff = 0
         Fc_build = 0
         Fc_point = 0
      END IF

   END SUBROUTINE SUEWS_cal_AnthropogenicEmission_DTS
! ================================================================================

!==============BIOGENIC CO2 flux==================================================
   SUBROUTINE SUEWS_cal_BiogenCO2( &
      alpha_bioCO2, alpha_enh_bioCO2, avkdn, avRh, beta_bioCO2, beta_enh_bioCO2, & ! input:
      dectime, Diagnose, EmissionsMethod, Fc_anthro, G_max, G_k, G_q_base, G_q_shape, &
      G_t, G_sm, gfunc, gsmodel, id, it, Kmax, LAI_id, LAIMin, &
      LAIMax, MaxConductance, min_res_bioCO2, Press_hPa, resp_a, &
      resp_b, S1, S2, sfr_surf, SMDMethod, SnowFrac, t2_C, Temp_C, theta_bioCO2, TH, TL, vsmd, xsmd, &
      Fc, Fc_biogen, Fc_photo, Fc_respi) ! output:

      IMPLICIT NONE

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: alpha_bioCO2 !The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve [-]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: alpha_enh_bioCO2 !part of the alpha coefficient related to the fraction of vegetation [-]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: beta_bioCO2 !The light-saturated gross photosynthesis of the canopy [umol m-2 s-1 ]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: beta_enh_bioCO2 !Part of the beta coefficient related to the fraction of vegetation [umol m-2 s-1 ]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAI_id !=LAI(id-1,:), LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAIMin !Min LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAIMax !Max LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: min_res_bioCO2 !minimum soil respiration rate (for cold-temperature limit) [umol m-2 s-1]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: resp_a !Respiration coefficient a
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: resp_b !Respiration coefficient b - related to air temperature dependency
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: theta_bioCO2 !The convexity of the curve at light saturation [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf ! surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac !surface fraction of snow cover [-]

      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: MaxConductance !max conductance [mm s-1]

      ! INTEGER, INTENT(in) :: BSoilSurf
      ! INTEGER, INTENT(in) :: ConifSurf
      ! INTEGER, INTENT(in) :: DecidSurf
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: EmissionsMethod
      ! INTEGER, INTENT(in) :: GrassSurf
      INTEGER, INTENT(in) :: gsmodel !choice of gs parameterisation (1 = Ja11, 2 = Wa16)
      INTEGER, INTENT(in) :: id !day of year [-]
      INTEGER, INTENT(in) :: it ! hour [H]
      ! INTEGER, INTENT(in) :: ivConif
      ! INTEGER, INTENT(in) :: ivDecid
      ! INTEGER, INTENT(in) :: ivGrass
      ! INTEGER, INTENT(in) :: nsurf
      ! INTEGER, INTENT(in) :: NVegSurf
      INTEGER, INTENT(in) :: SMDMethod !Method of measured soil moisture [-]

      REAL(KIND(1D0)), INTENT(in) :: avkdn !Average downwelling shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avRh !average relative humidity (%) [-]
      REAL(KIND(1D0)), INTENT(in) :: dectime !decimal time [-]
      REAL(KIND(1D0)), INTENT(in) :: Fc_anthro !anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: G_max !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_k !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_q_base !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_q_shape !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_t !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_sm !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: gfunc
      REAL(KIND(1D0)), INTENT(in) :: Kmax !annual maximum hourly solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa !air pressure [hPa]
      REAL(KIND(1D0)), INTENT(in) :: S1 !a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)), INTENT(in) :: S2 !a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)), INTENT(in) :: t2_C !modelled 2 meter air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: Temp_C ! measured air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: TH !Maximum temperature limit [degC]
      REAL(KIND(1D0)), INTENT(in) :: TL !Minimum temperature limit [degC]
      REAL(KIND(1D0)), INTENT(in) :: vsmd !Soil moisture deficit for vegetated surfaces only [mm]
      REAL(KIND(1D0)), INTENT(in) :: xsmd !Measured soil moisture deficit [mm]

      REAL(KIND(1D0)), INTENT(out) :: Fc_biogen !biogenic CO2 flux [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_photo !co2 flux from photosynthesis [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_respi !co2 flux from respiration [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc !total co2 flux [umol m-2 s-1]

      REAL(KIND(1D0)) :: gfunc2 !gdq*gtemp*gs*gq for photosynthesis calculations (With modelled 2 meter temperature)
      REAL(KIND(1D0)) :: dq !Specific humidity deficit [g/kg]
      REAL(KIND(1D0)) :: t2 !air temperature at 2m [degC]
      REAL(KIND(1D0)) :: dummy1 !Latent heat of vaporization in [J kg-1]
      REAL(KIND(1D0)) :: dummy2 !Latent heat of sublimation in J/kg
      REAL(KIND(1D0)) :: dummy3 !Saturation vapour pressure over water[hPa]
      REAL(KIND(1D0)) :: dummy4 !Vapour pressure of water[hpa]
      REAL(KIND(1D0)) :: dummy5 !vapour pressure deficit[hpa]
      REAL(KIND(1D0)) :: dummy6 !vapour pressure deficit[pa]
      REAL(KIND(1D0)) :: dummy7 !Vap density or absolute humidity [kg m-3]
      REAL(KIND(1D0)) :: dummy8 !specific heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)) :: dummy9 !Air density [kg m-3]
      REAL(KIND(1D0)) :: dummy10 !Surface Layer Conductance [mm s-1]
      REAL(KIND(1D0)) :: dummy11 !Surface resistance [s m-1]

      IF (EmissionsMethod >= 11) THEN

         IF (gsmodel == 3 .OR. gsmodel == 4) THEN ! With modelled 2 meter temperature
            ! Call LUMPS_cal_AtmMoist for dq and SurfaceResistance for gfunc with 2 meter temperature
            ! If modelled 2 meter temperature is too different from measured air temperature then
            ! use temp_c
            IF (ABS(Temp_C - t2_C) > 5) THEN
               t2 = Temp_C
            ELSE
               t2 = t2_C
            END IF

            CALL cal_AtmMoist( &
               t2, Press_hPa, avRh, dectime, & ! input:
               dummy1, dummy2, & ! output:
               dummy3, dummy4, dummy5, dummy6, dq, dummy7, dummy8, dummy9)

            CALL SurfaceResistance( &
               id, it, & ! input:
               SMDMethod, SnowFrac, sfr_surf, avkdn, t2, dq, xsmd, vsmd, MaxConductance, &
               LAIMax, LAI_id, gsModel, Kmax, &
               G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, TH, TL, S1, S2, &
               dummy10, dummy10, dummy10, dummy10, dummy10, & ! output:
               gfunc2, dummy10, dummy11) ! output:
         END IF

         ! Calculate CO2 fluxes from biogenic components
         IF (Diagnose == 1) WRITE (*, *) 'Calling CO2_biogen...'
         CALL CO2_biogen( &
            alpha_bioCO2, alpha_enh_bioCO2, avkdn, beta_bioCO2, beta_enh_bioCO2, BSoilSurf, & ! input:
            ConifSurf, DecidSurf, dectime, EmissionsMethod, gfunc, gfunc2, GrassSurf, gsmodel, &
            id, it, ivConif, ivDecid, ivGrass, LAI_id, LAIMin, LAIMax, min_res_bioCO2, nsurf, &
            NVegSurf, resp_a, resp_b, sfr_surf, SnowFrac, t2, Temp_C, theta_bioCO2, &
            Fc_biogen, Fc_photo, Fc_respi) ! output:
      END IF

      IF (EmissionsMethod >= 0 .AND. EmissionsMethod <= 6) THEN
         Fc_biogen = 0
         Fc_photo = 0
         Fc_respi = 0
      END IF

      Fc = Fc_anthro + Fc_biogen

   END SUBROUTINE SUEWS_cal_BiogenCO2

   SUBROUTINE SUEWS_cal_BiogenCO2_DTS( &
      alpha_bioCO2_evetr, alpha_bioCO2_dectr, alpha_bioCO2_grass, &
      alpha_enh_bioCO2_evetr, alpha_enh_bioCO2_dectr, alpha_enh_bioCO2_grass, &
      avkdn, avRh, &
      beta_bioCO2_evetr, beta_bioCO2_dectr, beta_bioCO2_grass, &
      beta_enh_bioCO2_evetr, beta_enh_bioCO2_dectr, beta_enh_bioCO2_grass, & ! input:
      dectime, Diagnose, EmissionsMethod, Fc_anthro, G_max, G_k, G_q_base, G_q_shape, &
      G_t, G_sm, gfunc, gsmodel, id, it, Kmax, LAI_id, &
      LAIMin_evetr, LAIMin_dectr, LAIMin_grass, &
      LAIMax_evetr, LAIMax_dectr, LAIMax_grass, &
      MaxConductance_evetr, MaxConductance_dectr, MaxConductance_grass, &
      min_res_bioCO2_evetr, min_res_bioCO2_dectr, min_res_bioCO2_grass, &
      Press_hPa, &
      resp_a_evetr, resp_a_dectr, resp_a_grass, &
      resp_b_evetr, resp_b_dectr, resp_b_grass, &
      S1, S2, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, &
      SMDMethod, SnowFrac, t2_C, Temp_C, &
      theta_bioCO2_evetr, theta_bioCO2_dectr, theta_bioCO2_grass, &
      TH, TL, vsmd, xsmd, &
      Fc, Fc_biogen, Fc_photo, Fc_respi) ! output:

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: alpha_bioCO2_evetr
      REAL(KIND(1D0)), INTENT(in) :: alpha_bioCO2_dectr
      REAL(KIND(1D0)), INTENT(in) :: alpha_bioCO2_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: alpha_bioCO2 !The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve [-]

      REAL(KIND(1D0)), INTENT(in) :: alpha_enh_bioCO2_evetr
      REAL(KIND(1D0)), INTENT(in) :: alpha_enh_bioCO2_dectr
      REAL(KIND(1D0)), INTENT(in) :: alpha_enh_bioCO2_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: alpha_enh_bioCO2 !part of the alpha coefficient related to the fraction of vegetation [-]

      REAL(KIND(1D0)), INTENT(in) :: beta_bioCO2_evetr
      REAL(KIND(1D0)), INTENT(in) :: beta_bioCO2_dectr
      REAL(KIND(1D0)), INTENT(in) :: beta_bioCO2_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: beta_bioCO2 !The light-saturated gross photosynthesis of the canopy [umol m-2 s-1 ]

      REAL(KIND(1D0)), INTENT(in) :: beta_enh_bioCO2_evetr
      REAL(KIND(1D0)), INTENT(in) :: beta_enh_bioCO2_dectr
      REAL(KIND(1D0)), INTENT(in) :: beta_enh_bioCO2_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: beta_enh_bioCO2 !Part of the beta coefficient related to the fraction of vegetation [umol m-2 s-1 ]

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAI_id !=LAI(id-1,:), LAI for each veg surface [m2 m-2]

      REAL(KIND(1D0)), INTENT(in) :: LAIMin_evetr
      REAL(KIND(1D0)), INTENT(in) :: LAIMin_dectr
      REAL(KIND(1D0)), INTENT(in) :: LAIMin_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAIMin !Min LAI [m2 m-2]

      REAL(KIND(1D0)), INTENT(in) :: LAIMax_evetr
      REAL(KIND(1D0)), INTENT(in) :: LAIMax_dectr
      REAL(KIND(1D0)), INTENT(in) :: LAIMax_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAIMax !Max LAI [m2 m-2]

      REAL(KIND(1D0)), INTENT(in) :: min_res_bioCO2_evetr
      REAL(KIND(1D0)), INTENT(in) :: min_res_bioCO2_dectr
      REAL(KIND(1D0)), INTENT(in) :: min_res_bioCO2_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: min_res_bioCO2 !minimum soil respiration rate (for cold-temperature limit) [umol m-2 s-1]

      REAL(KIND(1D0)), INTENT(in) :: resp_a_evetr
      REAL(KIND(1D0)), INTENT(in) :: resp_a_dectr
      REAL(KIND(1D0)), INTENT(in) :: resp_a_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: resp_a !Respiration coefficient a

      REAL(KIND(1D0)), INTENT(in) :: resp_b_evetr
      REAL(KIND(1D0)), INTENT(in) :: resp_b_dectr
      REAL(KIND(1D0)), INTENT(in) :: resp_b_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: resp_b !Respiration coefficient b - related to air temperature dependency

      REAL(KIND(1D0)), INTENT(in) :: theta_bioCO2_evetr
      REAL(KIND(1D0)), INTENT(in) :: theta_bioCO2_dectr
      REAL(KIND(1D0)), INTENT(in) :: theta_bioCO2_grass
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: theta_bioCO2 !The convexity of the curve at light saturation [-]

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf !surface fraction ratio [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac !surface fraction of snow cover [-]

      REAL(KIND(1D0)), INTENT(IN) :: MaxConductance_evetr
      REAL(KIND(1D0)), INTENT(IN) :: MaxConductance_dectr
      REAL(KIND(1D0)), INTENT(IN) :: MaxConductance_grass
      REAL(KIND(1D0)), DIMENSION(3) :: MaxConductance !max conductance [mm s-1]

      ! INTEGER, INTENT(in) :: BSoilSurf
      ! INTEGER, INTENT(in) :: ConifSurf
      ! INTEGER, INTENT(in) :: DecidSurf
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: EmissionsMethod
      ! INTEGER, INTENT(in) :: GrassSurf
      INTEGER, INTENT(in) :: gsmodel !choice of gs parameterisation (1 = Ja11, 2 = Wa16)
      INTEGER, INTENT(in) :: id !day of year [-]
      INTEGER, INTENT(in) :: it ! hour [H]
      ! INTEGER, INTENT(in) :: ivConif
      ! INTEGER, INTENT(in) :: ivDecid
      ! INTEGER, INTENT(in) :: ivGrass
      ! INTEGER, INTENT(in) :: nsurf
      ! INTEGER, INTENT(in) :: NVegSurf
      INTEGER, INTENT(in) :: SMDMethod !Method of measured soil moisture [-]

      REAL(KIND(1D0)), INTENT(in) :: avkdn !Average downwelling shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avRh !average relative humidity (%) [-]
      REAL(KIND(1D0)), INTENT(in) :: dectime !decimal time [-]
      REAL(KIND(1D0)), INTENT(in) :: Fc_anthro !anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: G_max !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_k !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_q_base !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_q_shape !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_t !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_sm !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: gfunc
      REAL(KIND(1D0)), INTENT(in) :: Kmax !annual maximum hourly solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa !air pressure [hPa]
      REAL(KIND(1D0)), INTENT(in) :: S1 !a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)), INTENT(in) :: S2 !a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)), INTENT(in) :: t2_C !modelled 2 meter air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: Temp_C ! measured air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: TH !Maximum temperature limit [degC]
      REAL(KIND(1D0)), INTENT(in) :: TL !Minimum temperature limit [degC]
      REAL(KIND(1D0)), INTENT(in) :: vsmd !Soil moisture deficit for vegetated surfaces only [mm]
      REAL(KIND(1D0)), INTENT(in) :: xsmd !Measured soil moisture deficit [mm]

      REAL(KIND(1D0)), INTENT(out) :: Fc_biogen !biogenic CO2 flux [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_photo !co2 flux from photosynthesis [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc_respi !co2 flux from respiration [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(out) :: Fc !total co2 flux [umol m-2 s-1]

      REAL(KIND(1D0)) :: gfunc2 !gdq*gtemp*gs*gq for photosynthesis calculations (With modelled 2 meter temperature)
      REAL(KIND(1D0)) :: dq !Specific humidity deficit [g/kg]
      REAL(KIND(1D0)) :: t2 !air temperature at 2m [degC]
      REAL(KIND(1D0)) :: dummy1 !Latent heat of vaporization in [J kg-1]
      REAL(KIND(1D0)) :: dummy2 !Latent heat of sublimation in J/kg
      REAL(KIND(1D0)) :: dummy3 !Saturation vapour pressure over water[hPa]
      REAL(KIND(1D0)) :: dummy4 !Vapour pressure of water[hpa]
      REAL(KIND(1D0)) :: dummy5 !vapour pressure deficit[hpa]
      REAL(KIND(1D0)) :: dummy6 !vapour pressure deficit[pa]
      REAL(KIND(1D0)) :: dummy7 !Vap density or absolute humidity [kg m-3]
      REAL(KIND(1D0)) :: dummy8 !specific heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)) :: dummy9 !Air density [kg m-3]
      REAL(KIND(1D0)) :: dummy10 !Surface Layer Conductance [mm s-1]
      REAL(KIND(1D0)) :: dummy11 !Surface resistance [s m-1]

      alpha_bioCO2 = [alpha_bioCO2_evetr, alpha_bioCO2_dectr, alpha_bioCO2_grass]
      alpha_enh_bioCO2 = [alpha_enh_bioCO2_evetr, alpha_enh_bioCO2_dectr, alpha_enh_bioCO2_grass]
      beta_bioCO2 = [beta_bioCO2_evetr, beta_bioCO2_dectr, beta_bioCO2_grass]
      beta_enh_bioCO2 = [beta_enh_bioCO2_evetr, beta_enh_bioCO2_dectr, beta_enh_bioCO2_grass]
      LAIMin = [LAIMin_evetr, LAIMin_dectr, LAIMin_grass]
      LAIMax = [LAIMax_evetr, LAIMax_dectr, LAIMax_grass]
      min_res_bioCO2 = [min_res_bioCO2_evetr, min_res_bioCO2_dectr, min_res_bioCO2_grass]
      resp_a = [resp_a_evetr, resp_a_dectr, resp_a_grass]
      resp_b = [resp_b_evetr, resp_b_dectr, resp_b_grass]
      theta_bioCO2 = [theta_bioCO2_evetr, theta_bioCO2_dectr, theta_bioCO2_grass]
      MaxConductance = [MaxConductance_evetr, MaxConductance_dectr, MaxConductance_grass]
      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]

      IF (EmissionsMethod >= 11) THEN

         IF (gsmodel == 3 .OR. gsmodel == 4) THEN ! With modelled 2 meter temperature
            ! Call LUMPS_cal_AtmMoist for dq and SurfaceResistance for gfunc with 2 meter temperature
            ! If modelled 2 meter temperature is too different from measured air temperature then
            ! use temp_c
            IF (ABS(Temp_C - t2_C) > 5) THEN
               t2 = Temp_C
            ELSE
               t2 = t2_C
            END IF

            CALL cal_AtmMoist( &
               t2, Press_hPa, avRh, dectime, & ! input:
               dummy1, dummy2, & ! output:
               dummy3, dummy4, dummy5, dummy6, dq, dummy7, dummy8, dummy9)

            CALL SurfaceResistance( &
               id, it, & ! input:
               SMDMethod, SnowFrac, sfr_surf, avkdn, t2, dq, xsmd, vsmd, MaxConductance, &
               LAIMax, LAI_id, gsModel, Kmax, &
               G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, TH, TL, S1, S2, &
               dummy10, dummy10, dummy10, dummy10, dummy10, & ! output:
               gfunc2, dummy10, dummy11) ! output:
         END IF

         ! Calculate CO2 fluxes from biogenic components
         IF (Diagnose == 1) WRITE (*, *) 'Calling CO2_biogen...'
         CALL CO2_biogen( &
            alpha_bioCO2, alpha_enh_bioCO2, avkdn, beta_bioCO2, beta_enh_bioCO2, BSoilSurf, & ! input:
            ConifSurf, DecidSurf, dectime, EmissionsMethod, gfunc, gfunc2, GrassSurf, gsmodel, &
            id, it, ivConif, ivDecid, ivGrass, LAI_id, LAIMin, LAIMax, min_res_bioCO2, nsurf, &
            NVegSurf, resp_a, resp_b, sfr_surf, SnowFrac, t2, Temp_C, theta_bioCO2, &
            Fc_biogen, Fc_photo, Fc_respi) ! output:
      END IF

      IF (EmissionsMethod >= 0 .AND. EmissionsMethod <= 6) THEN
         Fc_biogen = 0
         Fc_photo = 0
         Fc_respi = 0
      END IF

      Fc = Fc_anthro + Fc_biogen

   END SUBROUTINE SUEWS_cal_BiogenCO2_DTS
!========================================================================

!=============net all-wave radiation=====================================
   SUBROUTINE SUEWS_cal_Qn( &
      storageheatmethod, NetRadiationMethod, SnowUse, & !input
      tstep, nlayer, SnowPack_prev, tau_a, tau_f, SnowAlbMax, SnowAlbMin, &
      Diagnose, ldown_obs, fcld_obs, &
      dectime, ZENITH_deg, Tsurf_0, kdown, Tair_C, avRH, ea_hPa, qn1_obs, &
      SnowAlb_prev, snowFrac_prev, DiagQN, &
      NARP_TRANS_SITE, NARP_EMIS_SNOW, IceFrac, &
      sfr_surf, sfr_roof, sfr_wall, &
      tsfc_surf, tsfc_roof, tsfc_wall, &
      emis, alb_prev, albDecTr_id, albEveTr_id, albGrass_id, &
      LAI_id, & !input
      n_vegetation_region_urban, &
      n_stream_sw_urban, n_stream_lw_urban, & !input: SPARTACUS
      sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      veg_fsd_const, veg_contact_fraction_const, &
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, & !input: SPARTACUS
      height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
      alb_roof, emis_roof, alb_wall, emis_wall, &
      roof_albedo_dir_mult_fact, wall_specular_frac, &
      alb_next, ldown, fcld, & !output
      qn_surf, qn_roof, qn_wall, &
      qn, qn_snowfree, qn_snow, kclear, kup, lup, tsurf, &
      qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, &
      albedo_snow, SnowAlb_next, &
      dataOutLineSPARTACUS)
      USE NARP_MODULE, ONLY: RadMethod, NARP
      USE SPARTACUS_MODULE, ONLY: SPARTACUS

      IMPLICIT NONE
      ! INTEGER,PARAMETER ::nsurf     = 7 ! number of surface types
      ! INTEGER,PARAMETER ::ConifSurf = 3 !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER,PARAMETER ::DecidSurf = 4 !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER,PARAMETER ::GrassSurf = 5

      INTEGER, INTENT(in) :: storageheatmethod !Determines method for calculating storage heat flux ΔQS
      INTEGER, INTENT(in) :: NetRadiationMethod !Determines method for calculation of radiation fluxes
      INTEGER, INTENT(in) :: SnowUse !Determines whether the snow part of the model runs; 0-Snow calculations are not performed.1-Snow calculations are performed.
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: DiagQN
      INTEGER, INTENT(in) :: tstep !timestep [s]
      INTEGER, INTENT(in) :: nlayer !number of vertical levels in urban canopy [-]

      ! REAL(KIND(1D0)), INTENT(in) :: snowFrac_obs
      REAL(KIND(1D0)), INTENT(in) :: ldown_obs !observed incoming longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: fcld_obs !observed cloud fraction [-]
      REAL(KIND(1D0)), INTENT(in) :: dectime !decimal time [-]
      REAL(KIND(1D0)), INTENT(in) :: ZENITH_deg !solar zenith angle in degree [°]
      REAL(KIND(1D0)), INTENT(in) :: Tsurf_0
      REAL(KIND(1D0)), INTENT(in) :: kdown !incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Tair_C !Air temperature in degree C [degC]
      REAL(KIND(1D0)), INTENT(in) :: avRH !average relative humidity (%) in each layer [-]
      REAL(KIND(1D0)), INTENT(in) :: ea_hPa !vapor pressure [hPa]
      REAL(KIND(1D0)), INTENT(in) :: qn1_obs !observed net wall-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: SnowAlb_prev ! snow albedo at previous timestep [-]
      REAL(KIND(1D0)), INTENT(in) :: NARP_EMIS_SNOW ! snow emissivity in NARP model [-]
      REAL(KIND(1D0)), INTENT(in) :: NARP_TRANS_SITE !Atmospheric transmissivity for NARP [-]
      REAL(KIND(1D0)), INTENT(in) :: tau_a, tau_f, SnowAlbMax, SnowAlbMin !tau_a=Time constant for snow albedo aging in cold snow [-], tau_f=Time constant for snow albedo aging in melting snow [-], SnowAlbMax=maxmimum snow albedo, SnowAlbMin=minimum snow albedo

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAI_id !LAI for day of year [m2 m-3]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: IceFrac !fraction of ice in snowpack [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !fraction of each surfaces [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf ! surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof !  surface fraction of roofs at each surfaces [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof ! roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall ! surface fraction of walls at each surfaces [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall ! wall surface temperature [degC]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: emis ! Effective surface emissivity. [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: alb ! surface albedo [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: alb_prev ! input surface albedo [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: alb_next ! output surface albedo [-]
      REAL(KIND(1D0)), INTENT(in) :: albDecTr_id !!albedo for deciduous trees on day of year [-]
      ! REAL(KIND(1d0)), INTENT(in)  ::DecidCap_id
      REAL(KIND(1D0)), INTENT(in) :: albEveTr_id !albedo for evergreen trees and shrubs on day of year [-]
      REAL(KIND(1D0)), INTENT(in) :: albGrass_id !albedo for grass on day of year [-]

      ! REAL(KIND(1d0)), DIMENSION(6, nsurf), INTENT(inout)::StoreDrainPrm

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_prev !initial snow water equivalent on each land cover [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: snowFrac_prev !initial snow fraction [-]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: snowFrac_next
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac ! snow fractions of each surface [-]

      REAL(KIND(1D0)), INTENT(out) :: ldown ! output incoming longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: fcld ! estimated cloud fraction [-](used only for emissivity estimate)
      REAL(KIND(1D0)), INTENT(out) :: qn !  output net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qn_snowfree !output net all-wave radiation for snow free surface [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qn_snow ! output net all-wave radiation for snowpack [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: kclear !output clear sky incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: kup !output outgoing shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: lup !output outgoing longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: tsurf !output surface temperature [degC]
      REAL(KIND(1D0)), INTENT(out) :: albedo_snow !estimated albedo of snow [-]
      REAL(KIND(1D0)), INTENT(out) :: SnowAlb_next !output snow albedo [-]
      REAL(KIND(1D0)) :: albedo_snowfree !estimated albedo for snow-free surface [-]
      REAL(KIND(1D0)) :: SnowAlb ! updated snow albedo [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qn_surf !net all-wave radiation on each surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qn_ind_snow !net all-wave radiation on snowpack [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: kup_ind_snow !outgoing shortwave on snowpack [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Tsurf_ind_snow !snowpack surface temperature [C]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: tsurf_ind !snow-free surface temperature [C]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: lup_ind !outgoing longwave radiation from observation [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind !outgoing shortwave radiation from observation [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn1_ind !net all-wave radiation from observation [W m-2]

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER :: NetRadiationMethod_use
      INTEGER :: AlbedoChoice, ldown_option

      ! SPARTACUS output variables
      ! REAL(KIND(1D0)), INTENT(OUT) :: alb_spc, emis_spc, lw_emission_spc, lw_up_spc, sw_up_spc, qn_spc
      ! REAL(KIND(1D0)), INTENT(OUT) :: top_net_lw_spc, ground_net_lw_spc, top_dn_lw_spc
      ! REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: clear_air_abs_lw_spc, wall_net_lw_spc, roof_net_lw_spc, &
      !                                                roof_in_lw_spc
      ! REAL(KIND(1D0)), INTENT(OUT) :: top_dn_dir_sw_spc, top_net_sw_spc, ground_dn_dir_sw_spc, ground_net_sw_spc
      ! REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: clear_air_abs_sw_spc, wall_net_sw_spc, roof_net_sw_spc, &
      !                                                roof_in_sw_spc

      ! SPARTACUS input variables
      INTEGER, INTENT(IN) :: n_vegetation_region_urban, &
                             n_stream_sw_urban, n_stream_lw_urban
      REAL(KIND(1D0)), INTENT(IN) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
                                     veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                                     veg_fsd_const, veg_contact_fraction_const, &
                                     ground_albedo_dir_mult_fact
      LOGICAL, INTENT(IN) :: use_sw_direct_albedo !boolean, Specify ground and roof albedos separately for direct solar radiation [-]

      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height ! height in spartacus [m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac ! building fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac !vegetation fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale ! diameter of buildings [[m]. The only L method for buildings is Eq. 19 Hogan et al. 2018.
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale ! scale of tree crowns [m]. Using the default use_symmetric_vegetation_scale_urban=.TRUE. so that Eq. 20 Hogan et al. 2018 is used for L.
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof !albedo of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof ! emissivity of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall !albedo of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall ! emissivity of wall [-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact !Ratio of the direct and diffuse albedo of the roof [-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac ! Fraction of wall reflection that is specular [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qn_wall ! net all-wave radiation on the wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qn_roof ! net all-wave radiation on the roof [W m-2]

      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5), INTENT(OUT) :: dataOutLineSPARTACUS

      ! translate values
      alb = alb_prev

      ! update snow albedo
      SnowAlb = update_snow_albedo( &
                tstep, SnowPack_prev, SnowAlb_prev, Tair_C, &
                tau_a, tau_f, SnowAlbMax, SnowAlbMin)

      CALL RadMethod( &
         NetRadiationMethod, & !input
         SnowUse, & !input
         NetRadiationMethod_use, AlbedoChoice, ldown_option) !output

      SnowFrac = snowFrac_prev
      IF (NetRadiationMethod_use > 0) THEN

         ! IF (SnowUse==0) SnowFrac=snowFrac_obs
         IF (SnowUse == 0) SnowFrac = 0

         IF (ldown_option == 2) THEN !observed cloud fraction provided as forcing
            fcld = fcld_obs
         END IF

         !write(*,*) DecidCap(id), id, it, imin, 'Calc - near start'

         ! Update variables that change daily and represent seasonal variability
         alb(DecidSurf) = albDecTr_id !Change deciduous albedo
         ! StoreDrainPrm(6, DecidSurf) = DecidCap_id !Change current storage capacity of deciduous trees
         ! Change EveTr and Grass albedo too
         alb(ConifSurf) = albEveTr_id
         alb(GrassSurf) = albGrass_id

         IF (Diagnose == 1) WRITE (*, *) 'Calling NARP...'
         IF (Diagqn == 1) WRITE (*, *) 'NetRadiationMethodX:', NetRadiationMethod_use
         IF (Diagqn == 1) WRITE (*, *) 'AlbedoChoice:', AlbedoChoice

         ! TODO: TS 14 Feb 2022, ESTM development:
         ! here we use uniform `tsurf_0` for all land covers, which should be distinguished in future developments

         CALL NARP( &
            storageheatmethod, & !input:
            nsurf, sfr_surf, tsfc_surf, SnowFrac, alb, emis, IceFrac, & !
            NARP_TRANS_SITE, NARP_EMIS_SNOW, &
            dectime, ZENITH_deg, tsurf_0, kdown, Tair_C, avRH, ea_hPa, qn1_obs, ldown_obs, &
            SnowAlb, &
            AlbedoChoice, ldown_option, NetRadiationMethod_use, DiagQN, &
            qn_surf, & ! output:
            qn, qn_snowfree, qn_snow, kclear, kup, LDown, lup, fcld, tsurf, & ! output:
            qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, albedo_snowfree, albedo_snow)

         IF (Diagqn == 1) WRITE (*, *) 'Calling SPARTACUS:'
         IF (NetRadiationMethod > 1000) THEN
            ! TODO: TS 14 Feb 2022, ESTM development: introduce facet surface temperatures
            CALL SPARTACUS( &
               Diagqn, & !input:
               sfr_surf, zenith_deg, nlayer, & !input:
               tsfc_surf, tsfc_roof, tsfc_wall, &
               kdown, ldown, Tair_C, alb, emis, LAI_id, &
               n_vegetation_region_urban, &
               n_stream_sw_urban, n_stream_lw_urban, &
               sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
               veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
               veg_fsd_const, veg_contact_fraction_const, &
               ground_albedo_dir_mult_fact, use_sw_direct_albedo, &
               height, building_frac, veg_frac, sfr_roof, sfr_wall, &
               building_scale, veg_scale, & !input:
               alb_roof, emis_roof, alb_wall, emis_wall, &
               roof_albedo_dir_mult_fact, wall_specular_frac, &
               qn, kup, lup, qn_roof, qn_wall, qn_surf, & !output:
               dataOutLineSPARTACUS)
         ELSE
            qn_roof = qn_surf(BldgSurf)
            qn_wall = qn_surf(BldgSurf)
         END IF

      ELSE ! NetRadiationMethod==0
         ! SnowFrac = snowFrac_obs
         qn = qn1_obs
         qn_snowfree = qn1_obs
         qn_snow = qn1_obs
         ldown = NAN
         lup = NAN
         kup = NAN
         tsurf = NAN
         lup_ind = NAN
         kup_ind = NAN
         tsurf_ind = NAN
         qn1_ind = NAN
         Fcld = NAN
         qn_surf = qn
         qn_roof = qn_surf(BldgSurf)
         qn_wall = qn_surf(BldgSurf)
      END IF
      ! snowFrac_next = SnowFrac

      IF (ldown_option == 1) THEN
         Fcld = NAN
      END IF

      ! translate values
      alb_next = alb
      SnowAlb_next = SnowAlb

   END SUBROUTINE SUEWS_cal_Qn

   SUBROUTINE SUEWS_cal_Qn_DTS( &
      storageheatmethod, NetRadiationMethod, SnowUse, & !input
      tstep, nlayer, SnowPack_prev, tau_a, tau_f, SnowAlbMax, SnowAlbMin, &
      Diagnose, ldown_obs, fcld_obs, &
      dectime, ZENITH_deg, Tsurf_0, kdown, Tair_C, avRH, ea_hPa, qn1_obs, &
      SnowAlb_prev, snowFrac_prev, DiagQN, &
      NARP_TRANS_SITE, NARP_EMIS_SNOW, IceFrac, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, &
      sfr_roof, sfr_wall, &
      tsfc_surf, tsfc_roof, tsfc_wall, &
      emis_paved, emis_bldg, emis_evetr, emis_dectr, emis_grass, emis_bsoil, emis_water, &
      alb_prev, albDecTr_id, albEveTr_id, albGrass_id, &
      LAI_id, & !input
      n_vegetation_region_urban, &
      n_stream_sw_urban, n_stream_lw_urban, & !input: SPARTACUS
      sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      veg_fsd_const, veg_contact_fraction_const, &
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, & !input: SPARTACUS
      height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
      alb_roof, emis_roof, alb_wall, emis_wall, &
      roof_albedo_dir_mult_fact, wall_specular_frac, &
      alb_next, ldown, fcld, & !output
      qn_surf, qn_roof, qn_wall, &
      qn, qn_snowfree, qn_snow, kclear, kup, lup, tsurf, &
      qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, &
      albedo_snow, SnowAlb_next, &
      dataOutLineSPARTACUS)
      USE NARP_MODULE, ONLY: RadMethod, NARP
      USE SPARTACUS_MODULE, ONLY: SPARTACUS

      IMPLICIT NONE
      ! INTEGER,PARAMETER ::nsurf     = 7 ! number of surface types
      ! INTEGER,PARAMETER ::ConifSurf = 3 !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER,PARAMETER ::DecidSurf = 4 !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER,PARAMETER ::GrassSurf = 5

      INTEGER, INTENT(in) :: storageheatmethod !Determines method for calculating storage heat flux ΔQS
      INTEGER, INTENT(in) :: NetRadiationMethod !Determines method for calculation of radiation fluxes
      INTEGER, INTENT(in) :: SnowUse !Determines whether the snow part of the model runs; 0-Snow calculations are not performed.1-Snow calculations are performed.
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: DiagQN
      INTEGER, INTENT(in) :: tstep !timestep [s]
      INTEGER, INTENT(in) :: nlayer !number of vertical levels in urban canopy [-]

      ! REAL(KIND(1D0)), INTENT(in) :: snowFrac_obs
      REAL(KIND(1D0)), INTENT(in) :: ldown_obs !observed incoming longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: fcld_obs !observed cloud fraction [-]
      REAL(KIND(1D0)), INTENT(in) :: dectime !decimal time [-]
      REAL(KIND(1D0)), INTENT(in) :: ZENITH_deg !solar zenith angle in degree [°]
      REAL(KIND(1D0)), INTENT(in) :: Tsurf_0
      REAL(KIND(1D0)), INTENT(in) :: kdown !incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Tair_C !Air temperature in degree C [degC]
      REAL(KIND(1D0)), INTENT(in) :: avRH !average relative humidity (%) in each layer [-]
      REAL(KIND(1D0)), INTENT(in) :: ea_hPa !vapor pressure [hPa]
      REAL(KIND(1D0)), INTENT(in) :: qn1_obs !observed net wall-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: SnowAlb_prev ! snow albedo at previous timestep [-]
      REAL(KIND(1D0)), INTENT(in) :: NARP_EMIS_SNOW ! snow emissivity in NARP model [-]
      REAL(KIND(1D0)), INTENT(in) :: NARP_TRANS_SITE !Atmospheric transmissivity for NARP [-]
      REAL(KIND(1D0)), INTENT(in) :: tau_a, tau_f, SnowAlbMax, SnowAlbMin !tau_a=Time constant for snow albedo aging in cold snow [-], tau_f=Time constant for snow albedo aging in melting snow [-], SnowAlbMax=maxmimum snow albedo, SnowAlbMin=minimum snow albedo

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAI_id !LAI for day of year [m2 m-3]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: IceFrac !fraction of ice in snowpack [-]

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf ! surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof !  surface fraction of roofs at each surfaces [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof ! roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall ! surface fraction of walls at each surfaces [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall ! wall surface temperature [degC]

      REAL(KIND(1D0)), INTENT(IN) :: emis_paved
      REAL(KIND(1D0)), INTENT(IN) :: emis_bldg
      REAL(KIND(1D0)), INTENT(IN) :: emis_evetr
      REAL(KIND(1D0)), INTENT(IN) :: emis_dectr
      REAL(KIND(1D0)), INTENT(IN) :: emis_grass
      REAL(KIND(1D0)), INTENT(IN) :: emis_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: emis_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: emis ! Effective surface emissivity. [-]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: alb ! surface albedo [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: alb_prev ! input surface albedo [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: alb_next ! output surface albedo [-]
      REAL(KIND(1D0)), INTENT(in) :: albDecTr_id !!albedo for deciduous trees on day of year [-]
      ! REAL(KIND(1d0)), INTENT(in)  ::DecidCap_id
      REAL(KIND(1D0)), INTENT(in) :: albEveTr_id !albedo for evergreen trees and shrubs on day of year [-]
      REAL(KIND(1D0)), INTENT(in) :: albGrass_id !albedo for grass on day of year [-]

      ! REAL(KIND(1d0)), DIMENSION(6, nsurf), INTENT(inout)::StoreDrainPrm

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_prev !initial snow water equivalent on each land cover [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: snowFrac_prev !initial snow fraction [-]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: snowFrac_next
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac ! snow fractions of each surface [-]

      REAL(KIND(1D0)), INTENT(out) :: ldown ! output incoming longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: fcld ! estimated cloud fraction [-](used only for emissivity estimate)
      REAL(KIND(1D0)), INTENT(out) :: qn !  output net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qn_snowfree !output net all-wave radiation for snow free surface [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qn_snow ! output net all-wave radiation for snowpack [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: kclear !output clear sky incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: kup !output outgoing shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: lup !output outgoing longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: tsurf !output surface temperature [degC]
      REAL(KIND(1D0)), INTENT(out) :: albedo_snow !estimated albedo of snow [-]
      REAL(KIND(1D0)), INTENT(out) :: SnowAlb_next !output snow albedo [-]
      REAL(KIND(1D0)) :: albedo_snowfree !estimated albedo for snow-free surface [-]
      REAL(KIND(1D0)) :: SnowAlb ! updated snow albedo [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qn_surf !net all-wave radiation on each surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qn_ind_snow !net all-wave radiation on snowpack [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: kup_ind_snow !outgoing shortwave on snowpack [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Tsurf_ind_snow !snowpack surface temperature [C]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: tsurf_ind !snow-free surface temperature [C]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: lup_ind !outgoing longwave radiation from observation [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind !outgoing shortwave radiation from observation [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn1_ind !net all-wave radiation from observation [W m-2]

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER :: NetRadiationMethod_use
      INTEGER :: AlbedoChoice, ldown_option

      ! SPARTACUS output variables
      ! REAL(KIND(1D0)), INTENT(OUT) :: alb_spc, emis_spc, lw_emission_spc, lw_up_spc, sw_up_spc, qn_spc
      ! REAL(KIND(1D0)), INTENT(OUT) :: top_net_lw_spc, ground_net_lw_spc, top_dn_lw_spc
      ! REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: clear_air_abs_lw_spc, wall_net_lw_spc, roof_net_lw_spc, &
      !                                                roof_in_lw_spc
      ! REAL(KIND(1D0)), INTENT(OUT) :: top_dn_dir_sw_spc, top_net_sw_spc, ground_dn_dir_sw_spc, ground_net_sw_spc
      ! REAL(KIND(1D0)), DIMENSION(15), INTENT(OUT) :: clear_air_abs_sw_spc, wall_net_sw_spc, roof_net_sw_spc, &
      !                                                roof_in_sw_spc

      ! SPARTACUS input variables
      INTEGER, INTENT(IN) :: n_vegetation_region_urban, &
                             n_stream_sw_urban, n_stream_lw_urban
      REAL(KIND(1D0)), INTENT(IN) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
                                     veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                                     veg_fsd_const, veg_contact_fraction_const, &
                                     ground_albedo_dir_mult_fact
      LOGICAL, INTENT(IN) :: use_sw_direct_albedo !boolean, Specify ground and roof albedos separately for direct solar radiation [-]

      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height ! height in spartacus [m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac ! building fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac !vegetation fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale ! diameter of buildings [[m]. The only L method for buildings is Eq. 19 Hogan et al. 2018.
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale ! scale of tree crowns [m]. Using the default use_symmetric_vegetation_scale_urban=.TRUE. so that Eq. 20 Hogan et al. 2018 is used for L.
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof !albedo of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof ! emissivity of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall !albedo of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall ! emissivity of wall [-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact !Ratio of the direct and diffuse albedo of the roof [-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac ! Fraction of wall reflection that is specular [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qn_wall ! net all-wave radiation on the wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qn_roof ! net all-wave radiation on the roof [W m-2]

      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5), INTENT(OUT) :: dataOutLineSPARTACUS

      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]
      emis = [emis_paved, emis_bldg, emis_evetr, emis_dectr, emis_grass, emis_bsoil, emis_water]
      ! translate values
      alb = alb_prev

      ! update snow albedo
      SnowAlb = update_snow_albedo( &
                tstep, SnowPack_prev, SnowAlb_prev, Tair_C, &
                tau_a, tau_f, SnowAlbMax, SnowAlbMin)

      CALL RadMethod( &
         NetRadiationMethod, & !input
         SnowUse, & !input
         NetRadiationMethod_use, AlbedoChoice, ldown_option) !output

      SnowFrac = snowFrac_prev
      IF (NetRadiationMethod_use > 0) THEN

         ! IF (SnowUse==0) SnowFrac=snowFrac_obs
         IF (SnowUse == 0) SnowFrac = 0

         IF (ldown_option == 2) THEN !observed cloud fraction provided as forcing
            fcld = fcld_obs
         END IF

         !write(*,*) DecidCap(id), id, it, imin, 'Calc - near start'

         ! Update variables that change daily and represent seasonal variability
         alb(DecidSurf) = albDecTr_id !Change deciduous albedo
         ! StoreDrainPrm(6, DecidSurf) = DecidCap_id !Change current storage capacity of deciduous trees
         ! Change EveTr and Grass albedo too
         alb(ConifSurf) = albEveTr_id
         alb(GrassSurf) = albGrass_id

         IF (Diagnose == 1) WRITE (*, *) 'Calling NARP...'
         IF (Diagqn == 1) WRITE (*, *) 'NetRadiationMethodX:', NetRadiationMethod_use
         IF (Diagqn == 1) WRITE (*, *) 'AlbedoChoice:', AlbedoChoice

         ! TODO: TS 14 Feb 2022, ESTM development:
         ! here we use uniform `tsurf_0` for all land covers, which should be distinguished in future developments

         CALL NARP( &
            storageheatmethod, & !input:
            nsurf, sfr_surf, tsfc_surf, SnowFrac, alb, emis, IceFrac, & !
            NARP_TRANS_SITE, NARP_EMIS_SNOW, &
            dectime, ZENITH_deg, tsurf_0, kdown, Tair_C, avRH, ea_hPa, qn1_obs, ldown_obs, &
            SnowAlb, &
            AlbedoChoice, ldown_option, NetRadiationMethod_use, DiagQN, &
            qn_surf, & ! output:
            qn, qn_snowfree, qn_snow, kclear, kup, LDown, lup, fcld, tsurf, & ! output:
            qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, albedo_snowfree, albedo_snow)

         IF (Diagqn == 1) WRITE (*, *) 'Calling SPARTACUS:'
         IF (NetRadiationMethod > 1000) THEN
            ! TODO: TS 14 Feb 2022, ESTM development: introduce facet surface temperatures
            CALL SPARTACUS( &
               Diagqn, & !input:
               sfr_surf, zenith_deg, nlayer, & !input:
               tsfc_surf, tsfc_roof, tsfc_wall, &
               kdown, ldown, Tair_C, alb, emis, LAI_id, &
               n_vegetation_region_urban, &
               n_stream_sw_urban, n_stream_lw_urban, &
               sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
               veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
               veg_fsd_const, veg_contact_fraction_const, &
               ground_albedo_dir_mult_fact, use_sw_direct_albedo, &
               height, building_frac, veg_frac, sfr_roof, sfr_wall, &
               building_scale, veg_scale, & !input:
               alb_roof, emis_roof, alb_wall, emis_wall, &
               roof_albedo_dir_mult_fact, wall_specular_frac, &
               qn, kup, lup, qn_roof, qn_wall, qn_surf, & !output:
               dataOutLineSPARTACUS)
         ELSE
            qn_roof = qn_surf(BldgSurf)
            qn_wall = qn_surf(BldgSurf)
         END IF

      ELSE ! NetRadiationMethod==0
         ! SnowFrac = snowFrac_obs
         qn = qn1_obs
         qn_snowfree = qn1_obs
         qn_snow = qn1_obs
         ldown = NAN
         lup = NAN
         kup = NAN
         tsurf = NAN
         lup_ind = NAN
         kup_ind = NAN
         tsurf_ind = NAN
         qn1_ind = NAN
         Fcld = NAN
         qn_surf = qn
         qn_roof = qn_surf(BldgSurf)
         qn_wall = qn_surf(BldgSurf)
      END IF
      ! snowFrac_next = SnowFrac

      IF (ldown_option == 1) THEN
         Fcld = NAN
      END IF

      ! translate values
      alb_next = alb
      SnowAlb_next = SnowAlb

   END SUBROUTINE SUEWS_cal_Qn_DTS
!========================================================================

!=============storage heat flux=========================================
   SUBROUTINE SUEWS_cal_Qs( &
      StorageHeatMethod, qs_obs, OHMIncQF, Gridiv, & !input
      id, tstep, dt_since_start, Diagnose, &
      nlayer, &
      QG_surf, QG_roof, QG_wall, &
      tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
      tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
      tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
      OHM_coef, OHM_threshSW, OHM_threshWD, &
      soilstore_id, SoilStoreCap, state_id, SnowUse, SnowFrac, DiagQS, &
      HDD_id, MetForcingData_grid, Ts5mindata_ir, qf, qn, &
      avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown, &
      bldgh, alb, emis, cpAnOHM, kkAnOHM, chAnOHM, EmissionsMethod, &
      Tair_av, qn_av_prev, dqndt_prev, qn_s_av_prev, dqnsdt_prev, &
      StoreDrainPrm, &
      qn_S, dataOutLineESTM, qs, & !output
      qn_av_next, dqndt_next, qn_s_av_next, dqnsdt_next, &
      deltaQi, a1, a2, a3, &
      temp_out_roof, QS_roof, & !output
      temp_out_wall, QS_wall, & !output
      temp_out_surf, QS_surf) !output

      IMPLICIT NONE

      INTEGER, INTENT(in) :: StorageHeatMethod !heat storage calculation option [-]
      INTEGER, INTENT(in) :: OHMIncQF !Determines whether the storage heat flux calculation uses Q* or ( Q* +QF)
      INTEGER, INTENT(in) :: Gridiv ! grid id [-]
      INTEGER, INTENT(in) :: id ! day of year [-]
      INTEGER, INTENT(in) :: tstep ! time step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]
      INTEGER, INTENT(in) :: Diagnose
      ! INTEGER, INTENT(in)  ::nsh              ! number of timesteps in one hour
      INTEGER, INTENT(in) :: SnowUse ! option for snow related calculations [-]
      INTEGER, INTENT(in) :: DiagQS ! diagnostic option [-]
      INTEGER, INTENT(in) :: EmissionsMethod ! AnthropHeat option [-]
      INTEGER, INTENT(in) :: nlayer ! number of vertical levels in urban canopy [-]

      REAL(KIND(1D0)), INTENT(in) :: OHM_coef(nsurf + 1, 4, 3) ! OHM coefficients [-]
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW(nsurf + 1) ! Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD(nsurf + 1) ! Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]
      REAL(KIND(1D0)), INTENT(in) :: soilstore_id(nsurf) ! soil moisture on day of year
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap(nsurf) ! capacity of soil store [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: state_id(nsurf) ! wetness status [mm]

      REAL(KIND(1D0)), DIMENSION(12), INTENT(in) :: HDD_id ! Heating degree day of the day of year
      REAL(KIND(1D0)), INTENT(in) :: qf ! anthropogenic heat lufx [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qn ! net all-wave radiative flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qs_obs ! observed heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown
      REAL(KIND(1D0)), INTENT(in) :: bldgh ! mean building height [m]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: alb ! albedo [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: emis ! emissivity [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: cpAnOHM ! heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: kkAnOHM ! thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: chAnOHM ! bulk transfer coef [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac ! snow fractions of each surface [-]

      REAL(KIND(1D0)), DIMENSION(10), INTENT(in) :: MetForcingData_grid !< met forcing array of grid

      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: Ts5mindata_ir !surface temperature input data [degC]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: QG_surf ! ground heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Tair_av ! mean air temperature of past 24hr [degC]
      REAL(KIND(1D0)), INTENT(in) :: qn_av_prev ! weighted average of qn [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qn_av_next ! weighted average of qn for previous 60 mins [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: dqndt_prev ! Rate of change of net radiation at t-1 [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(out) :: dqndt_next ! Rate of change of net radiation at t+1 [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(in) :: qn_s_av_prev ! weighted average of qn over snow for previous 60mins [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qn_s_av_next ! weighted average of qn over snow for next 60mins [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: dqnsdt_prev ! Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(out) :: dqnsdt_next ! Rate of change of net radiation [W m-2 h-1] at t+1
      ! REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout)   ::qn1_store_grid
      ! REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout)   ::qn1_S_store_grid !< stored qn1 [W m-2]

      ! REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_av_store_grid
      ! REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_S_av_store_grid !< average net radiation over previous hour [W m-2]
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm !Coefficients used in drainage calculation [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: deltaQi ! storage heat flux of snow surfaces [W m-2]

      REAL(KIND(1D0)), DIMENSION(27), INTENT(out) :: dataOutLineESTM !data output from ESTM
      REAL(KIND(1D0)), INTENT(out) :: qn_S ! net all-wave radiation over snow [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qs ! storage heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: a1 !< AnOHM coefficients of grid [-]
      REAL(KIND(1D0)), INTENT(out) :: a2 !< AnOHM coefficients of grid [h]
      REAL(KIND(1D0)), INTENT(out) :: a3 !< AnOHM coefficients of grid [W m-2]

      ! extended for ESTM_ehc
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_roof ! conductive heat flux through roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof ! indoor/deep bottom temperature for roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof ! surface fraction of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_roof ! temperature at inner interfaces of roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof ! thermal conductivity of roof [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof ! Heat capacity of roof [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof ! thickness of each layer in roof [m]
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_wall ! conductive heat flux through wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall ! indoor/deep bottom temperature for wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall ! surface fraction of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_wall ! temperature at inner interfaces of wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall ! thermal conductivity of wall [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall ! Heat capacity of wall [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall ! thickness of each layer in wall [m]
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf !deep bottom temperature for each surface [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf ! fraction of each surface [-]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: temp_in_surf ! temperature at inner interfaces of of each surface [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf ! thermal conductivity of v [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf ! Heat capacity of each surface [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf ! thickness of each layer in each surface [m]
      ! output arrays
      ! roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof ! roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: QS_roof ! heat storage flux for roof component [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(out) :: temp_out_roof !interface temperature between depth layers [degC]
      ! wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall ! wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: QS_wall ! heat storage flux for wall component [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(out) :: temp_out_wall !interface temperature between depth layers [degC]
      ! standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf ! each surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: QS_surf ! heat storage flux for each surface component [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(out) :: temp_out_surf !interface temperature between depth layers [degC]

      ! internal use arrays
      REAL(KIND(1D0)) :: Tair_mav_5d ! Tair_mav_5d=HDD(id-1,4) HDD at the begining of today (id-1)
      REAL(KIND(1D0)) :: qn_use ! qn used in OHM calculations [W m-2]

      REAL(KIND(1D0)) :: moist_surf(nsurf) !< non-dimensional surface wetness status (0-1) [-]

      WRITE (*, *) 'OHM_coef = ', OHM_coef

      ! initialise output variables
      deltaQi = 0
      !SnowFrac = 0
      !qn1_S = 0
      dataOutLineESTM = -999
      qs = -999
      a1 = -999
      a2 = -999
      a3 = -999

      ! calculate qn if qf should be included
      IF (OHMIncQF == 1) THEN
         qn_use = qf + qn
      ELSEIF (OHMIncQF == 0) THEN
         qn_use = qn
      END IF

      IF (StorageHeatMethod == 0) THEN !Use observed QS
         qs = qs_obs

      ELSEIF (StorageHeatMethod == 1) THEN !Use OHM to calculate QS
         Tair_mav_5d = HDD_id(10)
         IF (Diagnose == 1) WRITE (*, *) 'Calling OHM...'
         CALL OHM(qn_use, qn_av_prev, dqndt_prev, qn_av_next, dqndt_next, &
                  qn_S, qn_s_av_prev, dqnsdt_prev, qn_s_av_next, dqnsdt_next, &
                  tstep, dt_since_start, &
                  sfr_surf, nsurf, &
                  Tair_mav_5d, &
                  OHM_coef, &
                  OHM_threshSW, OHM_threshWD, &
                  soilstore_id, SoilStoreCap, state_id, &
                  BldgSurf, WaterSurf, &
                  SnowUse, SnowFrac, &
                  DiagQS, &
                  a1, a2, a3, qs, deltaQi)
         QS_surf = qs
         QS_roof = qs
         QS_wall = qs

         ! use AnOHM to calculate QS, TS 14 Mar 2016
      ELSEIF (StorageHeatMethod == 3) THEN
         IF (Diagnose == 1) WRITE (*, *) 'Calling AnOHM...'
         ! CALL AnOHM(qn1_use,qn1_store_grid,qn1_av_store_grid,qf,&
         !      MetForcingData_grid,state_id/StoreDrainPrm(6,:),&
         !      alb, emis, cpAnOHM, kkAnOHM, chAnOHM,&
         !      sfr_surf,nsurf,nsh,EmissionsMethod,id,Gridiv,&
         !      a1,a2,a3,qs,deltaQi)
         moist_surf = state_id/StoreDrainPrm(6, :)
         ! CALL AnOHM( &
         !    tstep, dt_since_start, &
         !    qn_use, qn_av_prev, dqndt_prev, qf, &
         !    MetForcingData_grid, moist_surf, &
         !    alb, emis, cpAnOHM, kkAnOHM, chAnOHM, & ! input
         !    sfr_surf, nsurf, EmissionsMethod, id, Gridiv, &
         !    qn_av_next, dqndt_next, &
         !    a1, a2, a3, qs, deltaQi) ! output
         QS_surf = qs
         QS_roof = qs
         QS_wall = qs

         ! !Calculate QS using ESTM
      ELSEIF (StorageHeatMethod == 4 .OR. StorageHeatMethod == 14) THEN
         !    !CALL ESTM(QSestm,iMB)
         IF (Diagnose == 1) WRITE (*, *) 'Calling ESTM...'
         CALL ESTM( &
            Gridiv, & !input
            tstep, &
            avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown, &
            bldgh, Ts5mindata_ir, &
            Tair_av, &
            dataOutLineESTM, QS) !output
         !    CALL ESTM(QSestm,Gridiv,ir)  ! iMB corrected to Gridiv, TS 09 Jun 2016
         !    QS=QSestm   ! Use ESTM qs
      ELSEIF (StorageHeatMethod == 5) THEN
         !    !CALL ESTM(QSestm,iMB)
         IF (Diagnose == 1) WRITE (*, *) 'Calling extended ESTM...'
         ! facets: seven suews standard facets + extra for buildings [roof, wall] (can be extended for heterogeneous buildings)
         !
         ! ASSOCIATE (v => dz_roof(1, 1:ndepth))
         !    PRINT *, 'dz_roof in cal_qs', v, SIZE(v)
         ! END ASSOCIATE
         ! ASSOCIATE (v => dz_wall(1, 1:ndepth))
         !    PRINT *, 'dz_wall in cal_qs', v, SIZE(v)
         ! END ASSOCIATE
         CALL EHC( &
            tstep, & !input
            nlayer, &
            QG_surf, qg_roof, qg_wall, &
            tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
            tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
            tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
            temp_out_roof, QS_roof, & !output
            temp_out_wall, QS_wall, & !output
            temp_out_surf, QS_surf, & !output
            QS) !output

         ! TODO: add deltaQi to output for snow heat storage

         ! PRINT *, 'QS after ESTM_ehc', QS
         ! PRINT *, 'QS_roof after ESTM_ehc', QS_roof
         ! PRINT *, 'QS_wall after ESTM_ehc', QS_wall
         ! PRINT *, 'QS_surf after ESTM_ehc', QS_surf
         ! PRINT *, '------------------------------------'
         ! PRINT *, ''
      END IF

   END SUBROUTINE SUEWS_cal_Qs

   SUBROUTINE SUEWS_cal_Qs_DTS( &
      StorageHeatMethod, qs_obs, OHMIncQF, Gridiv, & !input
      id, tstep, dt_since_start, Diagnose, &
      nlayer, &
      QG_surf, QG_roof, QG_wall, &
      tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
      tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
      tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, & !input
      OHM_coef_paved, OHM_coef_bldg, OHM_coef_evetr, OHM_coef_dectr, &
      OHM_coef_grass, OHM_coef_bsoil, OHM_coef_water, &
      OHM_threshSW_paved, OHM_threshSW_bldg, OHM_threshSW_evetr, &
      OHM_threshSW_dectr, OHM_threshSW_grass, OHM_threshSW_bsoil, OHM_threshSW_water, &
      OHM_threshWD_paved, OHM_threshWD_bldg, OHM_threshWD_evetr, &
      OHM_threshWD_dectr, OHM_threshWD_grass, OHM_threshWD_bsoil, OHM_threshWD_water, &
      soilstore_id, &
      SoilStoreCap_paved, SoilStoreCap_bldg, SoilStoreCap_evetr, &
      SoilStoreCap_dectr, SoilStoreCap_grass, SoilStoreCap_bsoil, SoilStoreCap_water, &
      state_id, SnowUse, SnowFrac, DiagQS, &
      HDD_id, MetForcingData_grid, Ts5mindata_ir, qf, qn, &
      avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown, &
      bldgh, alb, &
      emis_paved, emis_bldg, emis_evetr, emis_dectr, emis_grass, emis_bsoil, emis_water, &
      cpAnOHM_paved, cpAnOHM_bldg, cpAnOHM_evetr, &
      cpAnOHM_dectr, cpAnOHM_grass, cpAnOHM_bsoil, cpAnOHM_water, &
      kkAnOHM_paved, kkAnOHM_bldg, kkAnOHM_evetr, &
      kkAnOHM_dectr, kkAnOHM_grass, kkAnOHM_bsoil, kkAnOHM_water, &
      chAnOHM_paved, chAnOHM_bldg, chAnOHM_evetr, &
      chAnOHM_dectr, chAnOHM_grass, chAnOHM_bsoil, chAnOHM_water, &
      EmissionsMethod, &
      Tair_av, qn_av_prev, dqndt_prev, qn_s_av_prev, dqnsdt_prev, &
      StoreDrainPrm, &
      qn_S, dataOutLineESTM, qs, & !output
      qn_av_next, dqndt_next, qn_s_av_next, dqnsdt_next, &
      deltaQi, a1, a2, a3, &
      temp_out_roof, QS_roof, & !output
      temp_out_wall, QS_wall, & !output
      temp_out_surf, QS_surf) !output

      IMPLICIT NONE

      INTEGER, INTENT(in) :: StorageHeatMethod !heat storage calculation option [-]
      INTEGER, INTENT(in) :: OHMIncQF !Determines whether the storage heat flux calculation uses Q* or ( Q* +QF)
      INTEGER, INTENT(in) :: Gridiv ! grid id [-]
      INTEGER, INTENT(in) :: id ! day of year [-]
      INTEGER, INTENT(in) :: tstep ! time step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]
      INTEGER, INTENT(in) :: Diagnose
      ! INTEGER, INTENT(in)  ::nsh              ! number of timesteps in one hour
      INTEGER, INTENT(in) :: SnowUse ! option for snow related calculations [-]
      INTEGER, INTENT(in) :: DiagQS ! diagnostic option [-]
      INTEGER, INTENT(in) :: EmissionsMethod ! AnthropHeat option [-]
      INTEGER, INTENT(in) :: nlayer ! number of vertical levels in urban canopy [-]

      TYPE(OHM_COEF_LC), INTENT(in) :: OHM_coef_paved(3)
      TYPE(OHM_COEF_LC), INTENT(in) :: OHM_coef_bldg(3)
      TYPE(OHM_COEF_LC), INTENT(in) :: OHM_coef_evetr(3)
      TYPE(OHM_COEF_LC), INTENT(in) :: OHM_coef_dectr(3)
      TYPE(OHM_COEF_LC), INTENT(in) :: OHM_coef_grass(3)
      TYPE(OHM_COEF_LC), INTENT(in) :: OHM_coef_bsoil(3)
      TYPE(OHM_COEF_LC), INTENT(in) :: OHM_coef_water(3)
      REAL(KIND(1D0)) :: OHM_coef(nsurf + 1, 4, 3) ! OHM coefficients [-]

      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW_paved
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW_bldg
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW_evetr
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW_dectr
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW_grass
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW_bsoil
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW_water
      REAL(KIND(1D0)) :: OHM_threshSW(nsurf + 1) ! Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]

      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD_paved
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD_bldg
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD_evetr
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD_dectr
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD_grass
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD_bsoil
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD_water
      REAL(KIND(1D0)) :: OHM_threshWD(nsurf + 1) ! Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]

      REAL(KIND(1D0)), INTENT(in) :: soilstore_id(nsurf) ! soil moisture on day of year

      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_paved
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_bldg
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_evetr
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_dectr
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_grass
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_bsoil
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_water
      REAL(KIND(1D0)) :: SoilStoreCap(nsurf) ! capacity of soil store [J m-3 K-1]

      REAL(KIND(1D0)), INTENT(in) :: state_id(nsurf) ! wetness status [mm]

      REAL(KIND(1D0)), DIMENSION(12), INTENT(in) :: HDD_id ! Heating degree day of the day of year
      REAL(KIND(1D0)), INTENT(in) :: qf ! anthropogenic heat lufx [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qn ! net all-wave radiative flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qs_obs ! observed heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown
      REAL(KIND(1D0)), INTENT(in) :: bldgh ! mean building height [m]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: alb ! albedo [-]

      REAL(KIND(1D0)), INTENT(in) :: emis_paved
      REAL(KIND(1D0)), INTENT(in) :: emis_bldg
      REAL(KIND(1D0)), INTENT(in) :: emis_evetr
      REAL(KIND(1D0)), INTENT(in) :: emis_dectr
      REAL(KIND(1D0)), INTENT(in) :: emis_grass
      REAL(KIND(1D0)), INTENT(in) :: emis_bsoil
      REAL(KIND(1D0)), INTENT(in) :: emis_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: emis ! emissivity [-]

      REAL(KIND(1D0)), INTENT(in) :: cpAnOHM_paved
      REAL(KIND(1D0)), INTENT(in) :: cpAnOHM_bldg
      REAL(KIND(1D0)), INTENT(in) :: cpAnOHM_evetr
      REAL(KIND(1D0)), INTENT(in) :: cpAnOHM_dectr
      REAL(KIND(1D0)), INTENT(in) :: cpAnOHM_grass
      REAL(KIND(1D0)), INTENT(in) :: cpAnOHM_bsoil
      REAL(KIND(1D0)), INTENT(in) :: cpAnOHM_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: cpAnOHM ! heat capacity [J m-3 K-1]

      REAL(KIND(1D0)), INTENT(in) :: kkAnOHM_paved
      REAL(KIND(1D0)), INTENT(in) :: kkAnOHM_bldg
      REAL(KIND(1D0)), INTENT(in) :: kkAnOHM_evetr
      REAL(KIND(1D0)), INTENT(in) :: kkAnOHM_dectr
      REAL(KIND(1D0)), INTENT(in) :: kkAnOHM_grass
      REAL(KIND(1D0)), INTENT(in) :: kkAnOHM_bsoil
      REAL(KIND(1D0)), INTENT(in) :: kkAnOHM_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kkAnOHM ! thermal conductivity [W m-1 K-1]

      REAL(KIND(1D0)), INTENT(in) :: chAnOHM_paved
      REAL(KIND(1D0)), INTENT(in) :: chAnOHM_bldg
      REAL(KIND(1D0)), INTENT(in) :: chAnOHM_evetr
      REAL(KIND(1D0)), INTENT(in) :: chAnOHM_dectr
      REAL(KIND(1D0)), INTENT(in) :: chAnOHM_grass
      REAL(KIND(1D0)), INTENT(in) :: chAnOHM_bsoil
      REAL(KIND(1D0)), INTENT(in) :: chAnOHM_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chAnOHM ! bulk transfer coef [J m-3 K-1]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac ! snow fractions of each surface [-]

      REAL(KIND(1D0)), DIMENSION(10), INTENT(in) :: MetForcingData_grid !< met forcing array of grid

      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: Ts5mindata_ir !surface temperature input data [degC]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: QG_surf ! ground heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Tair_av ! mean air temperature of past 24hr [degC]
      REAL(KIND(1D0)), INTENT(in) :: qn_av_prev ! weighted average of qn [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qn_av_next ! weighted average of qn for previous 60 mins [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: dqndt_prev ! Rate of change of net radiation at t-1 [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(out) :: dqndt_next ! Rate of change of net radiation at t+1 [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(in) :: qn_s_av_prev ! weighted average of qn over snow for previous 60mins [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qn_s_av_next ! weighted average of qn over snow for next 60mins [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: dqnsdt_prev ! Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(out) :: dqnsdt_next ! Rate of change of net radiation [W m-2 h-1] at t+1
      ! REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout)   ::qn1_store_grid
      ! REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout)   ::qn1_S_store_grid !< stored qn1 [W m-2]

      ! REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_av_store_grid
      ! REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_S_av_store_grid !< average net radiation over previous hour [W m-2]
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm !Coefficients used in drainage calculation [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: deltaQi ! storage heat flux of snow surfaces [W m-2]

      REAL(KIND(1D0)), DIMENSION(27), INTENT(out) :: dataOutLineESTM !data output from ESTM
      REAL(KIND(1D0)), INTENT(out) :: qn_S ! net all-wave radiation over snow [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qs ! storage heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: a1 !< AnOHM coefficients of grid [-]
      REAL(KIND(1D0)), INTENT(out) :: a2 !< AnOHM coefficients of grid [h]
      REAL(KIND(1D0)), INTENT(out) :: a3 !< AnOHM coefficients of grid [W m-2]

      ! extended for ESTM_ehc
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_roof ! conductive heat flux through roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof ! indoor/deep bottom temperature for roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof ! surface fraction of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_roof ! temperature at inner interfaces of roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof ! thermal conductivity of roof [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof ! Heat capacity of roof [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof ! thickness of each layer in roof [m]
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_wall ! conductive heat flux through wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall ! indoor/deep bottom temperature for wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall ! surface fraction of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_wall ! temperature at inner interfaces of wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall ! thermal conductivity of wall [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall ! Heat capacity of wall [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall ! thickness of each layer in wall [m]
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf !deep bottom temperature for each surface [degC]

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]

      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: temp_in_surf ! temperature at inner interfaces of of each surface [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf ! thermal conductivity of v [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf ! Heat capacity of each surface [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf ! thickness of each layer in each surface [m]
      ! output arrays
      ! roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof ! roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: QS_roof ! heat storage flux for roof component [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(out) :: temp_out_roof !interface temperature between depth layers [degC]
      ! wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall ! wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: QS_wall ! heat storage flux for wall component [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(out) :: temp_out_wall !interface temperature between depth layers [degC]
      ! standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf ! each surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: QS_surf ! heat storage flux for each surface component [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(out) :: temp_out_surf !interface temperature between depth layers [degC]

      ! internal use arrays
      REAL(KIND(1D0)) :: Tair_mav_5d ! Tair_mav_5d=HDD(id-1,4) HDD at the begining of today (id-1)
      REAL(KIND(1D0)) :: qn_use ! qn used in OHM calculations [W m-2]

      REAL(KIND(1D0)) :: moist_surf(nsurf) !< non-dimensional surface wetness status (0-1) [-]

      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]

      OHM_coef(1, 1, 1) = OHM_coef_paved(1)%summer_wet
      OHM_coef(1, 2, 1) = OHM_coef_paved(1)%summer_dry
      OHM_coef(1, 3, 1) = OHM_coef_paved(1)%winter_wet
      OHM_coef(1, 4, 1) = OHM_coef_paved(1)%winter_dry
      !WRITE (*, *) 'OHM_coef_paved(1)%winter_dry', OHM_coef_paved(1)%winter_dry
      !WRITE (*, *) 'OHM_coef(1, 4, 1)', OHM_coef(1, 4, 1)

      OHM_coef(1, 1, 2) = OHM_coef_paved(2)%summer_wet
      OHM_coef(1, 2, 2) = OHM_coef_paved(2)%summer_dry
      OHM_coef(1, 3, 2) = OHM_coef_paved(2)%winter_wet
      OHM_coef(1, 4, 2) = OHM_coef_paved(2)%winter_dry

      OHM_coef(1, 1, 3) = OHM_coef_paved(3)%summer_wet
      OHM_coef(1, 2, 3) = OHM_coef_paved(3)%summer_dry
      OHM_coef(1, 3, 3) = OHM_coef_paved(3)%winter_wet
      OHM_coef(1, 4, 3) = OHM_coef_paved(3)%winter_dry

      OHM_coef(2, 1, 1) = OHM_coef_bldg(1)%summer_wet
      OHM_coef(2, 2, 1) = OHM_coef_bldg(1)%summer_dry
      OHM_coef(2, 3, 1) = OHM_coef_bldg(1)%winter_wet
      OHM_coef(2, 4, 1) = OHM_coef_bldg(1)%winter_dry

      OHM_coef(2, 1, 2) = OHM_coef_bldg(2)%summer_wet
      OHM_coef(2, 2, 2) = OHM_coef_bldg(2)%summer_dry
      OHM_coef(2, 3, 2) = OHM_coef_bldg(2)%winter_wet
      OHM_coef(2, 4, 2) = OHM_coef_bldg(2)%winter_dry

      OHM_coef(2, 1, 3) = OHM_coef_bldg(3)%summer_wet
      OHM_coef(2, 2, 3) = OHM_coef_bldg(3)%summer_dry
      OHM_coef(2, 3, 3) = OHM_coef_bldg(3)%winter_wet
      OHM_coef(2, 4, 3) = OHM_coef_bldg(3)%winter_dry

      OHM_coef(3, 1, 1) = OHM_coef_evetr(1)%summer_wet
      OHM_coef(3, 2, 1) = OHM_coef_evetr(1)%summer_dry
      OHM_coef(3, 3, 1) = OHM_coef_evetr(1)%winter_wet
      OHM_coef(3, 4, 1) = OHM_coef_evetr(1)%winter_dry

      OHM_coef(3, 1, 2) = OHM_coef_evetr(2)%summer_wet
      OHM_coef(3, 2, 2) = OHM_coef_evetr(2)%summer_dry
      OHM_coef(3, 3, 2) = OHM_coef_evetr(2)%winter_wet
      OHM_coef(3, 4, 2) = OHM_coef_evetr(2)%winter_dry

      OHM_coef(3, 1, 3) = OHM_coef_evetr(3)%summer_wet
      OHM_coef(3, 2, 3) = OHM_coef_evetr(3)%summer_dry
      OHM_coef(3, 3, 3) = OHM_coef_evetr(3)%winter_wet
      OHM_coef(3, 4, 3) = OHM_coef_evetr(3)%winter_dry

      OHM_coef(4, 1, 1) = OHM_coef_dectr(1)%summer_wet
      OHM_coef(4, 2, 1) = OHM_coef_dectr(1)%summer_dry
      OHM_coef(4, 3, 1) = OHM_coef_dectr(1)%winter_wet
      OHM_coef(4, 4, 1) = OHM_coef_dectr(1)%winter_dry

      OHM_coef(4, 1, 2) = OHM_coef_dectr(2)%summer_wet
      OHM_coef(4, 2, 2) = OHM_coef_dectr(2)%summer_dry
      OHM_coef(4, 3, 2) = OHM_coef_dectr(2)%winter_wet
      OHM_coef(4, 4, 2) = OHM_coef_dectr(2)%winter_dry

      OHM_coef(4, 1, 3) = OHM_coef_dectr(3)%summer_wet
      OHM_coef(4, 2, 3) = OHM_coef_dectr(3)%summer_dry
      OHM_coef(4, 3, 3) = OHM_coef_dectr(3)%winter_wet
      OHM_coef(4, 4, 3) = OHM_coef_dectr(3)%winter_dry

      OHM_coef(5, 1, 1) = OHM_coef_grass(1)%summer_wet
      OHM_coef(5, 2, 1) = OHM_coef_grass(1)%summer_dry
      OHM_coef(5, 3, 1) = OHM_coef_grass(1)%winter_wet
      OHM_coef(5, 4, 1) = OHM_coef_grass(1)%winter_dry

      OHM_coef(5, 1, 2) = OHM_coef_grass(2)%summer_wet
      OHM_coef(5, 2, 2) = OHM_coef_grass(2)%summer_dry
      OHM_coef(5, 3, 2) = OHM_coef_grass(2)%winter_wet
      OHM_coef(5, 4, 2) = OHM_coef_grass(2)%winter_dry

      OHM_coef(5, 1, 3) = OHM_coef_grass(3)%summer_wet
      OHM_coef(5, 2, 3) = OHM_coef_grass(3)%summer_dry
      OHM_coef(5, 3, 3) = OHM_coef_grass(3)%winter_wet
      OHM_coef(5, 4, 3) = OHM_coef_grass(3)%winter_dry

      OHM_coef(6, 1, 1) = OHM_coef_bsoil(1)%summer_wet
      OHM_coef(6, 2, 1) = OHM_coef_bsoil(1)%summer_dry
      OHM_coef(6, 3, 1) = OHM_coef_bsoil(1)%winter_wet
      OHM_coef(6, 4, 1) = OHM_coef_bsoil(1)%winter_dry

      OHM_coef(6, 1, 2) = OHM_coef_bsoil(2)%summer_wet
      OHM_coef(6, 2, 2) = OHM_coef_bsoil(2)%summer_dry
      OHM_coef(6, 3, 2) = OHM_coef_bsoil(2)%winter_wet
      OHM_coef(6, 4, 2) = OHM_coef_bsoil(2)%winter_dry

      OHM_coef(6, 1, 3) = OHM_coef_bsoil(3)%summer_wet
      OHM_coef(6, 2, 3) = OHM_coef_bsoil(3)%summer_dry
      OHM_coef(6, 3, 3) = OHM_coef_bsoil(3)%winter_wet
      OHM_coef(6, 4, 3) = OHM_coef_bsoil(3)%winter_dry

      OHM_coef(7, 1, 1) = OHM_coef_water(1)%summer_wet
      OHM_coef(7, 2, 1) = OHM_coef_water(1)%summer_dry
      OHM_coef(7, 3, 1) = OHM_coef_water(1)%winter_wet
      OHM_coef(7, 4, 1) = OHM_coef_water(1)%winter_dry

      OHM_coef(7, 1, 2) = OHM_coef_water(2)%summer_wet
      OHM_coef(7, 2, 2) = OHM_coef_water(2)%summer_dry
      OHM_coef(7, 3, 2) = OHM_coef_water(2)%winter_wet
      OHM_coef(7, 4, 2) = OHM_coef_water(2)%winter_dry

      OHM_coef(7, 1, 3) = OHM_coef_water(3)%summer_wet
      OHM_coef(7, 2, 3) = OHM_coef_water(3)%summer_dry
      OHM_coef(7, 3, 3) = OHM_coef_water(3)%winter_wet
      OHM_coef(7, 4, 3) = OHM_coef_water(3)%winter_dry

      OHM_coef(8, 1, 1) = 0.0
      OHM_coef(8, 2, 1) = 0.0
      OHM_coef(8, 3, 1) = 0.0
      OHM_coef(8, 4, 1) = 0.0

      OHM_coef(8, 1, 2) = 0.0
      OHM_coef(8, 2, 2) = 0.0
      OHM_coef(8, 3, 2) = 0.0
      OHM_coef(8, 4, 2) = 0.0

      OHM_coef(8, 1, 3) = 0.0
      OHM_coef(8, 2, 3) = 0.0
      OHM_coef(8, 3, 3) = 0.0
      OHM_coef(8, 4, 3) = 0.0

      OHM_threshSW(1) = OHM_threshSW_paved
      OHM_threshSW(2) = OHM_threshSW_bldg
      OHM_threshSW(3) = OHM_threshSW_evetr
      OHM_threshSW(4) = OHM_threshSW_dectr
      OHM_threshSW(5) = OHM_threshSW_grass
      OHM_threshSW(6) = OHM_threshSW_bsoil
      OHM_threshSW(7) = OHM_threshSW_water

      OHM_threshWD(1) = OHM_threshWD_paved
      OHM_threshWD(2) = OHM_threshWD_bldg
      OHM_threshWD(3) = OHM_threshWD_evetr
      OHM_threshWD(4) = OHM_threshWD_dectr
      OHM_threshWD(5) = OHM_threshWD_grass
      OHM_threshWD(6) = OHM_threshWD_bsoil
      OHM_threshWD(7) = OHM_threshWD_water

      SoilStoreCap(1) = SoilStoreCap_paved
      SoilStoreCap(2) = SoilStoreCap_bldg
      SoilStoreCap(3) = SoilStoreCap_evetr
      SoilStoreCap(4) = SoilStoreCap_dectr
      SoilStoreCap(5) = SoilStoreCap_grass
      SoilStoreCap(6) = SoilStoreCap_bsoil
      SoilStoreCap(7) = SoilStoreCap_water

      emis(1) = emis_paved
      emis(2) = emis_bldg
      emis(3) = emis_evetr
      emis(4) = emis_dectr
      emis(5) = emis_grass
      emis(6) = emis_bsoil
      emis(7) = emis_water

      cpAnOHM(1) = cpAnOHM_paved
      cpAnOHM(2) = cpAnOHM_bldg
      cpAnOHM(3) = cpAnOHM_evetr
      cpAnOHM(4) = cpAnOHM_dectr
      cpAnOHM(5) = cpAnOHM_grass
      cpAnOHM(6) = cpAnOHM_bsoil
      cpAnOHM(7) = cpAnOHM_water

      kkAnOHM(1) = kkAnOHM_paved
      kkAnOHM(2) = kkAnOHM_bldg
      kkAnOHM(3) = kkAnOHM_evetr
      kkAnOHM(4) = kkAnOHM_dectr
      kkAnOHM(5) = kkAnOHM_grass
      kkAnOHM(6) = kkAnOHM_bsoil
      kkAnOHM(7) = kkAnOHM_water

      chAnOHM(1) = chAnOHM_paved
      chAnOHM(2) = chAnOHM_bldg
      chAnOHM(3) = chAnOHM_evetr
      chAnOHM(4) = chAnOHM_dectr
      chAnOHM(5) = chAnOHM_grass
      chAnOHM(6) = chAnOHM_bsoil
      chAnOHM(7) = chAnOHM_water

      ! WRITE (*, *) 'OHM_coef = ', OHM_coef

      ! initialise output variables
      deltaQi = 0
      !SnowFrac = 0
      !qn1_S = 0
      dataOutLineESTM = -999
      qs = -999
      a1 = -999
      a2 = -999
      a3 = -999

      ! calculate qn if qf should be included
      IF (OHMIncQF == 1) THEN
         qn_use = qf + qn
      ELSEIF (OHMIncQF == 0) THEN
         qn_use = qn
      END IF

      IF (StorageHeatMethod == 0) THEN !Use observed QS
         qs = qs_obs

      ELSEIF (StorageHeatMethod == 1) THEN !Use OHM to calculate QS
         Tair_mav_5d = HDD_id(10)
         IF (Diagnose == 1) WRITE (*, *) 'Calling OHM...'
         CALL OHM(qn_use, qn_av_prev, dqndt_prev, qn_av_next, dqndt_next, &
                  qn_S, qn_s_av_prev, dqnsdt_prev, qn_s_av_next, dqnsdt_next, &
                  tstep, dt_since_start, &
                  sfr_surf, nsurf, &
                  Tair_mav_5d, &
                  OHM_coef, &
                  OHM_threshSW, OHM_threshWD, &
                  soilstore_id, SoilStoreCap, state_id, &
                  BldgSurf, WaterSurf, &
                  SnowUse, SnowFrac, &
                  DiagQS, &
                  a1, a2, a3, qs, deltaQi)
         QS_surf = qs
         QS_roof = qs
         QS_wall = qs

         ! use AnOHM to calculate QS, TS 14 Mar 2016
      ELSEIF (StorageHeatMethod == 3) THEN
         IF (Diagnose == 1) WRITE (*, *) 'Calling AnOHM...'
         ! CALL AnOHM(qn1_use,qn1_store_grid,qn1_av_store_grid,qf,&
         !      MetForcingData_grid,state_id/StoreDrainPrm(6,:),&
         !      alb, emis, cpAnOHM, kkAnOHM, chAnOHM,&
         !      sfr_surf,nsurf,nsh,EmissionsMethod,id,Gridiv,&
         !      a1,a2,a3,qs,deltaQi)
         moist_surf = state_id/StoreDrainPrm(6, :)
         ! CALL AnOHM( &
         !    tstep, dt_since_start, &
         !    qn_use, qn_av_prev, dqndt_prev, qf, &
         !    MetForcingData_grid, moist_surf, &
         !    alb, emis, cpAnOHM, kkAnOHM, chAnOHM, & ! input
         !    sfr_surf, nsurf, EmissionsMethod, id, Gridiv, &
         !    qn_av_next, dqndt_next, &
         !    a1, a2, a3, qs, deltaQi) ! output
         QS_surf = qs
         QS_roof = qs
         QS_wall = qs

         ! !Calculate QS using ESTM
      ELSEIF (StorageHeatMethod == 4 .OR. StorageHeatMethod == 14) THEN
         !    !CALL ESTM(QSestm,iMB)
         IF (Diagnose == 1) WRITE (*, *) 'Calling ESTM...'
         CALL ESTM( &
            Gridiv, & !input
            tstep, &
            avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown, &
            bldgh, Ts5mindata_ir, &
            Tair_av, &
            dataOutLineESTM, QS) !output
         !    CALL ESTM(QSestm,Gridiv,ir)  ! iMB corrected to Gridiv, TS 09 Jun 2016
         !    QS=QSestm   ! Use ESTM qs
      ELSEIF (StorageHeatMethod == 5) THEN
         !    !CALL ESTM(QSestm,iMB)
         IF (Diagnose == 1) WRITE (*, *) 'Calling extended ESTM...'
         ! facets: seven suews standard facets + extra for buildings [roof, wall] (can be extended for heterogeneous buildings)
         !
         ! ASSOCIATE (v => dz_roof(1, 1:ndepth))
         !    PRINT *, 'dz_roof in cal_qs', v, SIZE(v)
         ! END ASSOCIATE
         ! ASSOCIATE (v => dz_wall(1, 1:ndepth))
         !    PRINT *, 'dz_wall in cal_qs', v, SIZE(v)
         ! END ASSOCIATE
         CALL EHC( &
            tstep, & !input
            nlayer, &
            QG_surf, qg_roof, qg_wall, &
            tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
            tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
            tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
            temp_out_roof, QS_roof, & !output
            temp_out_wall, QS_wall, & !output
            temp_out_surf, QS_surf, & !output
            QS) !output

         ! TODO: add deltaQi to output for snow heat storage

         ! PRINT *, 'QS after ESTM_ehc', QS
         ! PRINT *, 'QS_roof after ESTM_ehc', QS_roof
         ! PRINT *, 'QS_wall after ESTM_ehc', QS_wall
         ! PRINT *, 'QS_surf after ESTM_ehc', QS_surf
         ! PRINT *, '------------------------------------'
         ! PRINT *, ''
      END IF

   END SUBROUTINE SUEWS_cal_Qs_DTS
!=======================================================================

!==========================drainage and runoff================================
   SUBROUTINE SUEWS_cal_Water( &
      Diagnose, & !input
      SnowUse, NonWaterFraction, addPipes, addImpervious, addVeg, addWaterBody, &
      state_id, sfr_surf, &
      StoreCap_surf, DrainEq_surf, DrainCoef1_surf, DrainCoef2_surf, &
      WaterDist, nsh_real, &
      drain_per_tstep, & !output
      drain, frac_waterdist_vert, &
      AdditionalWater, runoffPipes, runoff_per_interval, &
      AddWater)

      IMPLICIT NONE
      ! INTEGER,PARAMETER :: nsurf=7! number of surface types
      ! INTEGER,PARAMETER ::WaterSurf = 7
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: SnowUse !!Snow part used (1) or not used (0) [-]

      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction !the surface fraction of non-water [-]
      REAL(KIND(1D0)), INTENT(in) :: addPipes !additional water in pipes [mm]
      REAL(KIND(1D0)), INTENT(in) :: addImpervious !water from impervious surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addWaterBody ! water from water body of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: nsh_real !nsh cast as a real for use in calculations

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_id !wetness states of each surface [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_id
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !Surface fractions [-]
      ! REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm ! drain storage capacity [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, nsurf - 1), INTENT(in) :: WaterDist !Within-grid water distribution to other surfaces and runoff/soil store [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: drain !drainage of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: frac_waterdist_vert !Fraction of water redistributed vertically: to either runoff (if impervious) or sub-surface soil (pervious) [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: AddWater !water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: stateOld
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstoreOld

      REAL(KIND(1D0)), INTENT(out) :: drain_per_tstep ! total drainage for all surface type at each timestep [mm]
      REAL(KIND(1D0)), INTENT(out) :: AdditionalWater !Additional water coming from other grids [mm] (these are expressed as depths over the whole surface)
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes !run-off in pipes [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoff_per_interval !run-off at each time interval [mm]
      INTEGER :: is

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StoreCap_surf !current storage capacity [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: DrainEq_surf !Drainage equation to use
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: DrainCoef1_surf !Drainage coeff 1 [units depend on choice of eqn]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: DrainCoef2_surf !Drainage coeff 2 [units depend on choice of eqn]

      ! StoreCap = StoreDrainPrm(6, :)
      ! DrainEq = StoreDrainPrm(2, :)
      ! DrainCoef1 = StoreDrainPrm(3, :)
      ! DrainCoef2 = StoreDrainPrm(4, :)

      ! Retain previous surface state_id and soil moisture state_id
      ! stateOld = state_id !state_id of each surface [mm] for the previous timestep
      ! soilstoreOld = soilstore_id !Soil moisture of each surface [mm] for the previous timestep

      !============= Grid-to-grid runoff =============
      ! Calculate additional water coming from other grids
      ! i.e. the variables addImpervious, addVeg, addWaterBody, addPipes
      !call RunoffFromGrid(GridFromFrac)  !!Need to code between-grid water transfer

      ! Sum water coming from other grids (these are expressed as depths over the whole surface)
      AdditionalWater = addPipes + addImpervious + addVeg + addWaterBody ![mm]

      ! Initialise runoff in pipes
      runoffPipes = addPipes !Water flowing in pipes from other grids. QUESTION: No need for scaling?
      !! CHECK p_i
      runoff_per_interval = addPipes !pipe plor added to total runoff.

      !================== Drainage ===================
      ! Calculate drainage for each soil subsurface (excluding water body)
      IF (Diagnose == 1) WRITE (*, *) 'Calling Drainage...'

      IF (NonWaterFraction /= 0) THEN !Soil states only calculated if soil exists. LJ June 2017
         DO is = 1, nsurf - 1

            CALL drainage( &
               is, & ! input:
               state_id(is), &
               StoreCap_surf(is), &
               DrainEq_surf(is), &
               DrainCoef1_surf(is), &
               DrainCoef2_surf(is), &
               nsh_real, &
               drain(is)) ! output

            ! !HCW added and changed to StoreDrainPrm(6,is) here 20 Feb 2015
            ! drain_per_tstep=drain_per_tstep+(drain(is)*sfr_surf(is)/NonWaterFraction)   !No water body included
         END DO
         drain_per_tstep = DOT_PRODUCT(drain(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction !No water body included
      ELSE
         drain(1:nsurf - 1) = 0
         drain_per_tstep = 0
      END IF

      drain(WaterSurf) = 0 ! Set drainage from water body to zero

      ! Distribute water within grid, according to WithinGridWaterDist matrix (Cols 1-7)
      IF (Diagnose == 1) WRITE (*, *) 'Calling ReDistributeWater...'
      ! CALL ReDistributeWater
      !Calculates AddWater(is)
      CALL ReDistributeWater( &
         SnowUse, WaterDist, sfr_surf, Drain, & ! input:
         frac_waterdist_vert, AddWater) ! output

   END SUBROUTINE SUEWS_cal_Water

   SUBROUTINE SUEWS_cal_Water_DTS( &
      Diagnose, & !input
      SnowUse, NonWaterFraction, addPipes, addImpervious, addVeg, addWaterBody, &
      state_id, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, & !input
      StoreDrainPrm, &
      WaterDist_paved_toPaved, WaterDist_paved_toBldg, WaterDist_paved_toEvetr, &
      WaterDist_paved_toDectr, WaterDist_paved_toGrass, WaterDist_paved_toBSoil, WaterDist_paved_toWater, &
      WaterDist_paved_toSoilstore, &
      WaterDist_bldg_toPaved, WaterDist_bldg_toBldg, WaterDist_bldg_toEvetr, &
      WaterDist_bldg_toDectr, WaterDist_bldg_toGrass, WaterDist_bldg_toBSoil, &
      WaterDist_bldg_toWater, WaterDist_bldg_toSoilstore, &
      WaterDist_evetr_toPaved, WaterDist_evetr_toBldg, WaterDist_evetr_toEvetr, &
      WaterDist_evetr_toDectr, WaterDist_evetr_toGrass, WaterDist_evetr_toBSoil, WaterDist_evetr_toWater, &
      WaterDist_evetr_toSoilstore, &
      WaterDist_dectr_toPaved, WaterDist_dectr_toBldg, WaterDist_dectr_toEvetr, &
      WaterDist_dectr_toDectr, WaterDist_dectr_toGrass, WaterDist_dectr_toBSoil, WaterDist_dectr_toWater, &
      WaterDist_dectr_toSoilstore, &
      WaterDist_grass_toPaved, WaterDist_grass_toBldg, WaterDist_grass_toEvetr, &
      WaterDist_grass_toDectr, WaterDist_grass_toGrass, WaterDist_grass_toBSoil, WaterDist_grass_toWater, &
      WaterDist_grass_toSoilstore, &
      WaterDist_bsoil_toPaved, WaterDist_bsoil_toBldg, WaterDist_bsoil_toEvetr, &
      WaterDist_bsoil_toDectr, WaterDist_bsoil_toGrass, WaterDist_bsoil_toBSoil, WaterDist_bsoil_toWater, &
      WaterDist_bsoil_toSoilStore, &
      nsh_real, &
      drain_per_tstep, & !output
      drain, frac_water2runoff, &
      AdditionalWater, runoffPipes, runoff_per_interval, &
      AddWater)

      IMPLICIT NONE
      ! INTEGER,PARAMETER :: nsurf=7! number of surface types
      ! INTEGER,PARAMETER ::WaterSurf = 7
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: SnowUse !!Snow part used (1) or not used (0) [-]

      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction !the surface fraction of non-water [-]
      REAL(KIND(1D0)), INTENT(in) :: addPipes !additional water in pipes [mm]
      REAL(KIND(1D0)), INTENT(in) :: addImpervious !water from impervious surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addWaterBody ! water from water body of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: nsh_real !nsh cast as a real for use in calculations

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_id !wetness states of each surface [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_id

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]

      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm ! drain storage capacity [mm]

      REAL(KIND(1D0)), INTENT(in) :: WaterDist_paved_toPaved
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_paved_toBldg
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_paved_toDectr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_paved_toEvetr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_paved_toGrass
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_paved_toBSoil
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_paved_toWater
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_paved_toSoilstore
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bldg_toPaved
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bldg_toBldg
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bldg_toDectr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bldg_toEvetr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bldg_toGrass
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bldg_toBSoil
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bldg_toWater
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bldg_toSoilstore
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_dectr_toPaved
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_dectr_toBldg
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_dectr_toDectr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_dectr_toEvetr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_dectr_toGrass
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_dectr_toBSoil
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_dectr_toWater
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_dectr_toSoilstore
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_evetr_toPaved
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_evetr_toBldg
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_evetr_toDectr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_evetr_toEvetr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_evetr_toGrass
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_evetr_toBSoil
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_evetr_toWater
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_evetr_toSoilstore
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_grass_toPaved
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_grass_toBldg
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_grass_toDectr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_grass_toEvetr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_grass_toGrass
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_grass_toBSoil
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_grass_toWater
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_grass_toSoilstore
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bsoil_toPaved
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bsoil_toBldg
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bsoil_toDectr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bsoil_toEvetr
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bsoil_toGrass
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bsoil_toBSoil
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bsoil_toWater
      REAL(KIND(1D0)), INTENT(in) :: WaterDist_bsoil_toSoilStore
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, nsurf - 1) :: WaterDist !Within-grid water distribution to other surfaces and runoff/soil store [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: drain !drainage of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: frac_water2runoff !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: AddWater !water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: stateOld
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstoreOld

      REAL(KIND(1D0)), INTENT(out) :: drain_per_tstep ! total drainage for all surface type at each timestep [mm]
      REAL(KIND(1D0)), INTENT(out) :: AdditionalWater !Additional water coming from other grids [mm] (these are expressed as depths over the whole surface)
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes !run-off in pipes [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoff_per_interval !run-off at each time interval [mm]
      INTEGER :: is

      AdditionalWater = 0.0
      AdditionalWater = addPipes + addImpervious + addVeg + addWaterBody ![mm]

      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]
      WaterDist(1, 1) = WaterDist_paved_toPaved
      WaterDist(2, 1) = WaterDist_paved_toBldg
      WaterDist(3, 1) = WaterDist_paved_toEvetr
      WaterDist(4, 1) = WaterDist_paved_toDectr
      WaterDist(5, 1) = WaterDist_paved_toGrass
      WaterDist(6, 1) = WaterDist_paved_toBSoil
      WaterDist(7, 1) = WaterDist_paved_toWater
      WaterDist(8, 1) = WaterDist_paved_toSoilstore

      WaterDist(1, 2) = WaterDist_bldg_toPaved
      WaterDist(2, 2) = WaterDist_bldg_toBldg
      WaterDist(3, 2) = WaterDist_bldg_toEvetr
      WaterDist(4, 2) = WaterDist_bldg_toDectr
      WaterDist(5, 2) = WaterDist_bldg_toGrass
      WaterDist(6, 2) = WaterDist_bldg_toBSoil
      WaterDist(7, 2) = WaterDist_bldg_toWater
      WaterDist(8, 2) = WaterDist_bldg_toSoilstore

      WaterDist(1, 3) = WaterDist_evetr_toPaved
      WaterDist(2, 3) = WaterDist_evetr_toBldg
      WaterDist(3, 3) = WaterDist_evetr_toEvetr
      WaterDist(4, 3) = WaterDist_evetr_toDectr
      WaterDist(5, 3) = WaterDist_evetr_toGrass
      WaterDist(6, 3) = WaterDist_evetr_toBSoil
      WaterDist(7, 3) = WaterDist_evetr_toWater
      WaterDist(8, 3) = WaterDist_evetr_toSoilstore

      WaterDist(1, 4) = WaterDist_dectr_toPaved
      WaterDist(2, 4) = WaterDist_dectr_toBldg
      WaterDist(3, 4) = WaterDist_dectr_toEvetr
      WaterDist(4, 4) = WaterDist_dectr_toDectr
      WaterDist(5, 4) = WaterDist_dectr_toGrass
      WaterDist(6, 4) = WaterDist_dectr_toBSoil
      WaterDist(7, 4) = WaterDist_dectr_toWater
      WaterDist(8, 4) = WaterDist_dectr_toSoilstore

      WaterDist(1, 5) = WaterDist_grass_toPaved
      WaterDist(2, 5) = WaterDist_grass_toBldg
      WaterDist(3, 5) = WaterDist_grass_toEvetr
      WaterDist(4, 5) = WaterDist_grass_toDectr
      WaterDist(5, 5) = WaterDist_grass_toGrass
      WaterDist(6, 5) = WaterDist_grass_toBSoil
      WaterDist(7, 5) = WaterDist_grass_toWater
      WaterDist(8, 5) = WaterDist_grass_toSoilstore

      WaterDist(1, 6) = WaterDist_bsoil_toPaved
      WaterDist(2, 6) = WaterDist_bsoil_toBldg
      WaterDist(3, 6) = WaterDist_bsoil_toEvetr
      WaterDist(4, 6) = WaterDist_bsoil_toDectr
      WaterDist(5, 6) = WaterDist_bsoil_toGrass
      WaterDist(6, 6) = WaterDist_bsoil_toBSoil
      WaterDist(7, 6) = WaterDist_bsoil_toWater
      WaterDist(8, 6) = WaterDist_bsoil_toSoilStore

      ! Retain previous surface state_id and soil moisture state_id
      ! stateOld = state_id !state_id of each surface [mm] for the previous timestep
      ! soilstoreOld = soilstore_id !Soil moisture of each surface [mm] for the previous timestep

      !============= Grid-to-grid runoff =============
      ! Calculate additional water coming from other grids
      ! i.e. the variables addImpervious, addVeg, addWaterBody, addPipes
      !call RunoffFromGrid(GridFromFrac)  !!Need to code between-grid water transfer

      ! Sum water coming from other grids (these are expressed as depths over the whole surface)

      ! Initialise runoff in pipes
      runoffPipes = addPipes !Water flowing in pipes from other grids. QUESTION: No need for scaling?
      !! CHECK p_i
      runoff_per_interval = addPipes !pipe plor added to total runoff.

      !================== Drainage ===================
      ! Calculate drainage for each soil subsurface (excluding water body)
      IF (Diagnose == 1) WRITE (*, *) 'Calling Drainage...'

      IF (NonWaterFraction /= 0) THEN !Soil states only calculated if soil exists. LJ June 2017
         DO is = 1, nsurf - 1

            CALL drainage( &
               is, & ! input:
               state_id(is), &
               StoreDrainPrm(6, is), &
               StoreDrainPrm(2, is), &
               StoreDrainPrm(3, is), &
               StoreDrainPrm(4, is), &
               nsh_real, &
               drain(is)) ! output

            ! !HCW added and changed to StoreDrainPrm(6,is) here 20 Feb 2015
            ! drain_per_tstep=drain_per_tstep+(drain(is)*sfr_surf(is)/NonWaterFraction)   !No water body included
         END DO
         drain_per_tstep = DOT_PRODUCT(drain(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction !No water body included
      ELSE
         drain(1:nsurf - 1) = 0
         drain_per_tstep = 0
      END IF

      drain(WaterSurf) = 0 ! Set drainage from water body to zero

      ! Distribute water within grid, according to WithinGridWaterDist matrix (Cols 1-7)
      IF (Diagnose == 1) WRITE (*, *) 'Calling ReDistributeWater...'
      ! CALL ReDistributeWater
      !Calculates AddWater(is)
      CALL ReDistributeWater( &
         SnowUse, WaterDist, sfr_surf, Drain, & ! input:
         frac_water2runoff, AddWater) ! output

   END SUBROUTINE SUEWS_cal_Water_DTS
!=======================================================================

!===============initialize sensible heat flux============================
   SUBROUTINE SUEWS_init_QH( &
      avdens, avcp, h_mod, qn1, dectime, & !input
      H_init) !output

      IMPLICIT NONE
      ! REAL(KIND(1d0)), INTENT(in)::qh_obs
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avcp ! air heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: h_mod !volumetric air heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: qn1 !net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: dectime !local time (days), not daylight savings
      REAL(KIND(1D0)), INTENT(out) :: H_init !initial QH [W m-2]

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER, PARAMETER :: notUsedI = -999

      ! Calculate kinematic heat flux (w'T') from sensible heat flux [W m-2] from observed data (if available) or LUMPS
      ! IF (qh_obs /= NAN) THEN   !if(qh_obs/=NAN) qh=qh_obs   !Commented out by HCW 04 Mar 2015
      !    H_init = qh_obs/(avdens*avcp)  !Use observed value
      ! ELSE
      IF (h_mod /= NAN) THEN
         H_init = h_mod/(avdens*avcp) !Use LUMPS value
      ELSE
         H_init = (qn1*0.2)/(avdens*avcp) !If LUMPS has had a problem, we still need a value
         CALL ErrorHint(38, 'LUMPS unable to calculate realistic value for H_mod.', h_mod, dectime, notUsedI)
      END IF
      ! ENDIF

   END SUBROUTINE SUEWS_init_QH
!========================================================================
   SUBROUTINE SUEWS_cal_snow( &
      Diagnose, nlayer, & !input
      tstep, imin, it, EvapMethod, dayofWeek_id, CRWmin, CRWmax, &
      dectime, avdens, avcp, lv_J_kg, lvS_J_kg, avRh, Press_hPa, Temp_C, &
      RAsnow, psyc_hPa, sIce_hPa, tau_r, &
      RadMeltFact, TempMeltFact, SnowAlbMax, PrecipLimit, PrecipLimitAlb, &
      qn_ind_snow, kup_ind_snow, deltaQi, Tsurf_ind_snow, &
      SnowAlb_in, &
      PervFraction, vegfraction, addimpervious, qn_snowfree, qf, qs, vpd_hPa, s_hPa, &
      RS, RA, RB, SnowDensMax, SnowDensMin, precip, PipeCapacity, RunoffToWater, &
      addVeg, SnowLimPaved, SnowLimBldg, &
      FlowChange, drain, WetThresh_surf, SoilStoreCap, &
      Tsurf_ind, sfr_surf, &
      AddWater, addwaterrunoff, &
      StoreCap_surf, StoreCapMax_surf, &
      SnowPackLimit, SnowProf_24hr, &
      SnowPack_in, SnowFrac_in, SnowWater_in, iceFrac_in, SnowDens_in, & ! input:
      SnowfallCum_in, state_id_in, soilstore_id_in, & ! input:
      qn_surf, qs_surf, &
      SnowRemoval, & ! snow specific output:
      SnowPack_out, SnowFrac_out, SnowWater_out, iceFrac_out, SnowDens_out, & ! output
      SnowfallCum_out, state_id_out, soilstore_id_out, & ! general output:
      state_per_tstep, NWstate_per_tstep, &
      qe, qe_surf, qe_roof, qe_wall, &
      SnowAlb_out, &
      swe, chSnow_per_tstep, ev_per_tstep, runoff_per_tstep, &
      surf_chang_per_tstep, runoffPipes, mwstore, runoffwaterbody, &
      runoffAGveg, runoffAGimpervious, rss_surf, &
      dataOutLineSnow)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: nlayer !number of vertical levels in urban canopy [-]
      INTEGER, INTENT(in) :: tstep !timestep [s]
      INTEGER, INTENT(in) :: imin ! minutes [min]
      INTEGER, INTENT(in) :: it ! hour [H]
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)

      INTEGER, DIMENSION(nsurf) :: snowCalcSwitch
      INTEGER, DIMENSION(3), INTENT(in) :: dayofWeek_id ! 1 - day of week; 2 - month; 3 - season

      REAL(KIND(1D0)), INTENT(in) :: CRWmin !minimum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(in) :: CRWmax !maximum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(in) :: dectime !decimal time [-]
      REAL(KIND(1D0)), INTENT(in) :: lvS_J_kg !latent heat of sublimation [J kg-1]
      REAL(KIND(1D0)), INTENT(in) :: lv_j_kg !Latent heat of vapourisation per timestep [J kg-1]
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avRh !relative humidity [-]
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa !air pressure [hPa]
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: RAsnow !aerodynamic resistance of snow [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa !psychometric constant [hPa]
      REAL(KIND(1D0)), INTENT(in) :: avcp !air heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: sIce_hPa !satured curve on snow [hPa]
      REAL(KIND(1D0)), INTENT(in) :: PervFraction !sum of surface cover fractions for impervious surfaces [-]
      REAL(KIND(1D0)), INTENT(in) :: vegfraction ! fraction of vegetation [-]
      REAL(KIND(1D0)), INTENT(in) :: addimpervious !Water from impervious surfaces of other grids for whole surface area [mm]
      REAL(KIND(1D0)), INTENT(in) :: qn_snowfree ! net all-wave radiation for snow-free surface [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qf !anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qs !heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)), INTENT(in) :: s_hPa !vapour pressure versus temperature slope [hPa K-1]
      REAL(KIND(1D0)), INTENT(in) :: RS !surface resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RA !aerodynamic resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RB !boundary layer resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMax !Fresh snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMin !Fresh snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: precip !rain data [mm]
      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity !Capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater !Fraction of surface runoff going to water body [-]
      ! REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
      ! REAL(KIND(1d0)), INTENT(in)::wu_EveTr!Water use for evergreen trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_DecTr!Water use for deciduous trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_Grass!Water use for grass [mm]
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      ! REAL(KIND(1D0)), INTENT(in) :: addWaterBody !Water from water surface of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: SnowLimPaved !snow limit for paved [mm]
      REAL(KIND(1D0)), INTENT(in) :: SnowLimBldg !snow limit for building [mm]
      ! REAL(KIND(1D0)), INTENT(in) :: SurfaceArea
      REAL(KIND(1D0)), INTENT(in) :: FlowChange !Difference between the input and output flow in the water body [mm]

      REAL(KIND(1D0)), INTENT(in) :: tau_r !time constant for snow density ageing [-]

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WU_nsurf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain !water flowing intyo drainage [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WetThresh_surf !surface wetness threshold [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: mw_ind !melt water from sknowpack[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap !Capacity of soil store [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: rainonsnow !rain water on snow event [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezmelt !freezing of melt water[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezstate !freezing of state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezstatevol !surface state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Qm_Melt !melt heat [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Qm_rain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind !snow-free surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !surface fraction ratio [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPackLimit !Limit for the snow water equivalent when snow cover starts to be patchy [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StateLimit !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater !addition water from other surfaces [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: addwaterrunoff !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      ! REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm !Coefficients used in drainage calculation [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StoreCap_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StoreCapMax_surf
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: SnowProf_24hr !Hourly profile values used in snow clearing [-]

      ! Total water transported to each grid for grid-to-grid connectivity
      ! REAL(KIND(1D0)), INTENT(in) :: runoff_per_interval_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_id_in ! wetness status of each surface type from previous timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_id_in !soil moisture of each surface type from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_in ! snowpack from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac_in !  snow fraction from previous timestep[-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowWater_in ! snow water from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: iceFrac_in ! ice fraction from previous timestep [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_in ! snow density from previous timestep[kg m-3]

      ! output:
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_id_out ! wetness status of each surface type at next timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstore_id_out !soil moisture of each surface type at next timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowPack_out ! snowpack at next timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowFrac_out !  snow fraction at next timestep[-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowWater_out ! snow water at nexts timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: iceFrac_out ! ice fraction at next timestep [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDens_out ! snow density at next timestep[kg m-3]

      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: runoffSnow_surf !Initialize for runoff caused by snowmelting
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_surf ! runoff for each surface [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ChangSnow_surf !change in SnowPack (mm)
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: snowDepth
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowToSurf !the water flowing into snow free area [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_snow !Evaporation of now [mm]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(out) :: SnowRemoval !snow removal [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_surf !evaporation of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rss_surf !redefined surface resistance for wet surfaces [s m-1]

      ! REAL(KIND(1D0)) :: p_mm !Inputs to surface water balance
      ! REAL(KIND(1d0)),INTENT(out)::rss
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn_surf ! net all-wave radiation of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qs_surf ! heat storage flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qe_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_roof ! latent heat flux of roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_wall ! latent heat flux of wall [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: state_per_tstep !state_id at each timestep [mm]
      REAL(KIND(1D0)), INTENT(out) :: NWstate_per_tstep ! state_id at each tinestep(excluding water body) [mm]
      REAL(KIND(1D0)), INTENT(out) :: qe !latent heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: swe !overall snow water equavalent[mm]
      REAL(KIND(1D0)), INTENT(out) :: chSnow_per_tstep ! change state_id of snow and surface per time interval [mm]
      REAL(KIND(1D0)), INTENT(out) :: ev_per_tstep ! evaporation at each time step [mm]
      REAL(KIND(1D0)) :: qe_per_tstep !latent heat flux at each timestep[W m-2]
      REAL(KIND(1D0)), INTENT(out) :: runoff_per_tstep !runoff water at each time step [mm]
      REAL(KIND(1D0)), INTENT(out) :: surf_chang_per_tstep !change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes !runoff to pipes [mm]
      REAL(KIND(1D0)), INTENT(out) :: mwstore !overall met water [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffwaterbody !Above ground runoff from water surface for all surface area [mm]
      ! REAL(KIND(1D0)) :: runoffWaterBody_m3
      ! REAL(KIND(1D0)) :: runoffPipes_m3
      REAL(KIND(1D0)), INTENT(out) :: runoffAGveg !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffAGimpervious !Above ground runoff from impervious surface for all surface area [mm]

      ! local:
      INTEGER :: is ! surface type [-]

      ! REAL(KIND(1D0)) :: runoff_per_interval
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id_surf ! wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id !soil moisture of each surface type[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowPack ! snowpack [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac !snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowWater ! water in snow [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: iceFrac !ice fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDens !snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_e_surf !net available energy for evaporation for each surfaces [W m-2]

      REAL(KIND(1D0)), DIMENSION(2) :: SurplusEvap !surface evaporation in 5 min timestep [mm]
      REAL(KIND(1D0)) :: surplusWaterBody !Extra runoff that goes to water body [mm] as specified by RunoffToWater
      REAL(KIND(1D0)) :: pin !Rain per time interval [mm]
      ! REAL(KIND(1d0))::sae
      ! REAL(KIND(1d0))::vdrc
      ! REAL(KIND(1d0))::sp
      ! REAL(KIND(1d0))::numPM
      REAL(KIND(1D0)) :: qn_e !net available energy for evaporation [W m-2]
      REAL(KIND(1D0)) :: tlv !Latent heat of vapourisation per timestep [J kg-1 s-1]
      ! REAL(KIND(1D0)) :: runoffAGimpervious_m3
      ! REAL(KIND(1D0)) :: runoffAGveg_m3
      REAL(KIND(1D0)) :: nsh_real !timestep in a hour [-]
      ! REAL(KIND(1D0)) :: tstep_real
      REAL(KIND(1D0)) :: ev_tot !total evaporation for all surfaces [mm]
      REAL(KIND(1D0)) :: qe_tot ! total latent heat flux for all surfaces [W m-2]
      REAL(KIND(1D0)) :: surf_chang_tot !total change in state_id(excluding snowpack) for all surfaces [mm]
      REAL(KIND(1D0)) :: runoff_tot !total runoff for all surfaces [mm]
      REAL(KIND(1D0)) :: chSnow_tot !total change state_id of snow and surface [mm]

      REAL(KIND(1D0)), DIMENSION(7) :: capStore_surf ! current storage capacity [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: Qm_freezState
      REAL(KIND(1D0)) :: mwh
      REAL(KIND(1D0)) :: fwh
      REAL(KIND(1D0)) :: Qm
      REAL(KIND(1D0)) :: QmFreez
      REAL(KIND(1D0)) :: QmRain
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDepth

      REAL(KIND(1D0)), INTENT(in) :: RadMeltFact
      REAL(KIND(1D0)), INTENT(in) :: TempMeltFact
      REAL(KIND(1D0)), INTENT(in) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(in) :: PrecipLimit
      REAL(KIND(1D0)), INTENT(in) :: PrecipLimitAlb

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: kup_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: deltaQi
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind_snow

      REAL(KIND(1D0)), INTENT(in) :: SnowfallCum_in
      REAL(KIND(1D0)), INTENT(out) :: SnowfallCum_out
      REAL(KIND(1D0)) :: SnowfallCum

      REAL(KIND(1D0)), INTENT(in) :: SnowAlb_in
      REAL(KIND(1D0)), INTENT(out) :: SnowAlb_out
      REAL(KIND(1D0)) :: SnowAlb
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5), INTENT(out) :: dataOutLineSnow

      ! runoff_per_interval = runoff_per_interval_in
      state_id_surf = state_id_in
      soilstore_id = soilstore_id_in

      ! tstep_real = tstep*1.D0
      nsh_real = 3600/tstep*1.D0

      capStore_surf = 0 !initialise capStore

      tlv = lv_J_kg/tstep*1.D0 !Latent heat of vapourisation per timestep

      pin = MAX(0., Precip) !Initiate rain data [mm]

      ! Initialize the output variables
      qe_surf = 0

      ev_per_tstep = 0
      qe_per_tstep = 0
      surf_chang_per_tstep = 0
      runoff_per_tstep = 0
      state_per_tstep = 0
      NWstate_per_tstep = 0
      chSnow_per_tstep = 0
      qe = 0

      runoffAGveg = 0
      runoffAGimpervious = 0
      surplusWaterBody = 0
      runoff_surf = 0
      chang = 0
      SurplusEvap = 0

      ! force these facets to be totally dry
      ! TODO: need to consider their hydrologic dynamics
      qe_roof = 0
      qe_wall = 0

      ! net available energy for evaporation
      qn_e_surf = qn_surf + qf - qs_surf ! qn1 changed to qn1_snowfree, lj in May 2013

      IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_snow...'
      ! IF (SnowUse == 1) THEN ! snow calculation
      ! net available energy for evaporation
      qn_e = qn_snowfree + qf - qs ! qn1 changed to qn1_snowfree, lj in May 2013

      SnowPack = SnowPack_in
      SnowFrac = SnowFrac_in
      SnowWater = SnowWater_in
      iceFrac = iceFrac_in
      SnowDens = SnowDens_in
      SnowfallCum = SnowfallCum_in
      SnowAlb = SnowAlb_in

      ! update snow density
      SnowDens = update_snow_dens( &
                 tstep, SnowFrac, SnowDens, &
                 tau_r, SnowDensMax, SnowDensMin)

      ! Calculate snow-related energy budgets
      CALL MeltHeat( &
         lvS_J_kg, lv_J_kg, tstep*1D0, RadMeltFact, TempMeltFact, & !input
         SnowAlbMax, SnowDensMin, Temp_C, Precip, PrecipLimit, PrecipLimitAlb, &
         nsh_real, sfr_surf, Tsurf_ind, state_id_in, qn_ind_snow, &
         SnowWater, deltaQi, &
         SnowPack, SnowFrac, SnowAlb, SnowDens, SnowfallCum, & !inout
         mwh, fwh, Qm, QmFreez, QmRain, snowCalcSwitch, & !output
         Qm_melt, Qm_freezState, Qm_rain, FreezMelt, FreezState, FreezStateVol, &
         rainOnSnow, SnowDepth, mw_ind)

      DO is = 1, nsurf !For each surface in turn
         qe_tot = 0
         ev_tot = 0
         swe = 0
         ev_snow = 0
         runoff_tot = 0
         surf_chang_tot = 0
         chSnow_tot = 0
         SnowRemoval = 0
         runoffPipes = 0
         mwstore = 0
         runoffwaterbody = 0
         IF (sfr_surf(is) > 0) THEN
            ! IF (Diagnose == 1) WRITE (*, *) 'Calling SnowCalc...'

            CALL SnowCalc( &
               tstep, imin, it, dectime, is, & !input
               snowCalcSwitch, &
               EvapMethod, CRWmin, CRWmax, nsh_real, lvS_J_kg, avdens, &
               avRh, Press_hPa, Temp_C, RAsnow, psyc_hPa, avcp, sIce_hPa, &
               PervFraction, vegfraction, addimpervious, &
               vpd_hPa, qn_e, s_hPa, RS, RA, RB, tlv, SnowDensMin, SnowProf_24hr, precip, &
               PipeCapacity, RunoffToWater, &
               addVeg, SnowLimPaved, SnowLimBldg, FlowChange, drain, &
               WetThresh_surf, state_id_in, mw_ind, SoilStoreCap, rainonsnow, &
               freezmelt, freezstate, freezstatevol, &
               Qm_Melt, Qm_rain, Tsurf_ind, sfr_surf, dayofWeek_id, &
               StoreCap_surf, StoreCapMax_surf, &
               SnowPackLimit, &
               AddWater, addwaterrunoff, &
               soilstore_id, SnowPack, SurplusEvap, & !inout
               SnowFrac, SnowWater, iceFrac, SnowDens, &
               runoffAGimpervious, runoffAGveg, surplusWaterBody, &
               ev_tot, qe_tot, runoff_tot, surf_chang_tot, chSnow_tot, & ! output
               rss_surf, &
               runoff_surf, chang, ChangSnow_surf, SnowToSurf, state_id_surf, ev_snow, &
               SnowRemoval, swe, &
               runoffPipes, mwstore, runoffwaterbody)

         ELSE
            SnowFrac(is) = 0
            SnowDens(is) = 0
            SnowPack(is) = 0
         END IF
         !Actual updates here as xx_tstep variables not taken as input to snowcalc
         ev_per_tstep = ev_per_tstep + ev_tot
         qe_per_tstep = qe_per_tstep + qe_tot
         runoff_per_tstep = runoff_per_tstep + runoff_tot
         surf_chang_per_tstep = surf_chang_per_tstep + surf_chang_tot
         chSnow_per_tstep = chSnow_per_tstep + chSnow_tot

         !Store ev_tot for each surface
         ev_surf(is) = ev_tot

      END DO

      qe = qe_per_tstep

      ! Calculate volume of water that will move between grids
      ! Volume [m3] = Depth relative to whole area [mm] / 1000 [mm m-1] * SurfaceArea [m2]
      ! Need to use these volumes when converting back to addImpervious, AddVeg and AddWater
      ! runoffAGimpervious_m3 = runoffAGimpervious/1000*SurfaceArea
      ! runoffAGveg_m3 = runoffAGveg/1000*SurfaceArea
      ! runoffWaterBody_m3 = runoffWaterBody/1000*SurfaceArea
      ! runoffPipes_m3 = runoffPipes/1000*SurfaceArea

      state_id_out = state_id_surf
      soilstore_id_out = soilstore_id

      SnowWater_out = SnowWater
      iceFrac_out = iceFrac

      SnowAlb_out = SnowAlb
      SnowDens_out = SnowDens
      SnowPack_out = SnowPack
      SnowFrac_out = SnowFrac
      SnowfallCum_out = SnowfallCum

      ! pack output into one line
      dataOutLineSnow = [ &
                        SnowPack_out(1:nsurf), mw_ind(1:nsurf), Qm_melt(1:nsurf), & !26
                        Qm_rain(1:nsurf), Qm_freezState(1:nsurf), SnowFrac_out(1:(nsurf - 1)), & !46
                        rainOnSnow(1:nsurf), & !53
                        qn_ind_snow(1:nsurf), kup_ind_snow(1:nsurf), freezMelt(1:nsurf), & !74
                        SnowWater(1:nsurf), SnowDens_out(1:nsurf), & !88
                        snowDepth(1:nsurf), Tsurf_ind_snow(1:nsurf), &
                        SnowAlb_out]

   END SUBROUTINE SUEWS_cal_snow

   SUBROUTINE SUEWS_cal_snow_DTS( &
      Diagnose, nlayer, & !input
      tstep, imin, it, EvapMethod, dayofWeek_id, CRWmin, CRWmax, &
      dectime, avdens, avcp, lv_J_kg, lvS_J_kg, avRh, Press_hPa, Temp_C, &
      RAsnow, psyc_hPa, sIce_hPa, tau_r, &
      RadMeltFact, TempMeltFact, SnowAlbMax, PrecipLimit, PrecipLimitAlb, &
      qn_ind_snow, kup_ind_snow, deltaQi, Tsurf_ind_snow, &
      SnowAlb_in, &
      PervFraction, vegfraction, addimpervious, qn_snowfree, qf, qs, vpd_hPa, s_hPa, &
      RS, RA, RB, SnowDensMax, SnowDensMin, precip, PipeCapacity, RunoffToWater, &
      addVeg, SnowLimPaved, SnowLimBldg, &
      FlowChange, drain, &
      WetThresh_paved, WetThresh_bldg, WetThresh_evetr, WetThresh_dectr, &
      WetThresh_grass, WetThresh_bsoil, WetThresh_water, & !input
      SoilStoreCap_paved, SoilStoreCap_bldg, SoilStoreCap_evetr, SoilStoreCap_dectr, &
      SoilStoreCap_grass, SoilStoreCap_bsoil, SoilStoreCap_water, & !input
      Tsurf_ind, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, & !input
      AddWater, addwaterrunoff, StoreDrainPrm, SnowPackLimit, &
      SnowProf_24hr_working, SnowProf_24hr_holiday, &
      SnowPack_in, SnowFrac_in, SnowWater_in, iceFrac_in, SnowDens_in, & ! input:
      SnowfallCum_in, state_id_in, soilstore_id_in, & ! input:
      qn_surf, qs_surf, &
      SnowRemoval, & ! snow specific output:
      SnowPack_out, SnowFrac_out, SnowWater_out, iceFrac_out, SnowDens_out, & ! output
      SnowfallCum_out, state_id_out, soilstore_id_out, & ! general output:
      state_per_tstep, NWstate_per_tstep, &
      qe, qe_surf, qe_roof, qe_wall, &
      SnowAlb_out, &
      swe, chSnow_per_tstep, ev_per_tstep, runoff_per_tstep, &
      surf_chang_per_tstep, runoffPipes, mwstore, runoffwaterbody, &
      runoffAGveg, runoffAGimpervious, rss_surf, &
      dataOutLineSnow)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: nlayer !number of vertical levels in urban canopy [-]
      INTEGER, INTENT(in) :: tstep !timestep [s]
      INTEGER, INTENT(in) :: imin ! minutes [min]
      INTEGER, INTENT(in) :: it ! hour [H]
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)

      INTEGER, DIMENSION(nsurf) :: snowCalcSwitch
      INTEGER, DIMENSION(3), INTENT(in) :: dayofWeek_id ! 1 - day of week; 2 - month; 3 - season

      REAL(KIND(1D0)), INTENT(in) :: CRWmin !minimum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(in) :: CRWmax !maximum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(in) :: dectime !decimal time [-]
      REAL(KIND(1D0)), INTENT(in) :: lvS_J_kg !latent heat of sublimation [J kg-1]
      REAL(KIND(1D0)), INTENT(in) :: lv_j_kg !Latent heat of vapourisation per timestep [J kg-1]
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avRh !relative humidity [-]
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa !air pressure [hPa]
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: RAsnow !aerodynamic resistance of snow [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa !psychometric constant [hPa]
      REAL(KIND(1D0)), INTENT(in) :: avcp !air heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: sIce_hPa !satured curve on snow [hPa]
      REAL(KIND(1D0)), INTENT(in) :: PervFraction !sum of surface cover fractions for impervious surfaces [-]
      REAL(KIND(1D0)), INTENT(in) :: vegfraction ! fraction of vegetation [-]
      REAL(KIND(1D0)), INTENT(in) :: addimpervious !Water from impervious surfaces of other grids for whole surface area [mm]
      REAL(KIND(1D0)), INTENT(in) :: qn_snowfree ! net all-wave radiation for snow-free surface [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qf !anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qs !heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)), INTENT(in) :: s_hPa !vapour pressure versus temperature slope [hPa K-1]
      REAL(KIND(1D0)), INTENT(in) :: RS !surface resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RA !aerodynamic resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RB !boundary layer resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMax !Fresh snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMin !Fresh snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: precip !rain data [mm]
      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity !Capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater !Fraction of surface runoff going to water body [-]
      ! REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
      ! REAL(KIND(1d0)), INTENT(in)::wu_EveTr!Water use for evergreen trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_DecTr!Water use for deciduous trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_Grass!Water use for grass [mm]
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      ! REAL(KIND(1D0)), INTENT(in) :: addWaterBody !Water from water surface of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: SnowLimPaved !snow limit for paved [mm]
      REAL(KIND(1D0)), INTENT(in) :: SnowLimBldg !snow limit for building [mm]
      ! REAL(KIND(1D0)), INTENT(in) :: SurfaceArea
      REAL(KIND(1D0)), INTENT(in) :: FlowChange !Difference between the input and output flow in the water body [mm]

      REAL(KIND(1D0)), INTENT(in) :: tau_r !time constant for snow density ageing [-]

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WU_nsurf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain !water flowing intyo drainage [mm]

      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_paved
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_bldg
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_evetr
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_dectr
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_grass
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: WetThresh_surf !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: mw_ind !melt water from sknowpack[mm]

      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_paved
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_bldg
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_evetr
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_dectr
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_grass
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_bsoil
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilStoreCap !Capacity of soil store for each surface [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: rainonsnow !rain water on snow event [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezmelt !freezing of melt water[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezstate !freezing of state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezstatevol !surface state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Qm_Melt !melt heat [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Qm_rain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind !snow-free surface temperature [degC]

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPackLimit !Limit for the snow water equivalent when snow cover starts to be patchy [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StateLimit !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater !addition water from other surfaces [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: addwaterrunoff !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm !Coefficients used in drainage calculation [-]

      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: SnowProf_24hr_working
      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: SnowProf_24hr_holiday
      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: SnowProf_24hr !Hourly profile values used in snow clearing [-]

      ! Total water transported to each grid for grid-to-grid connectivity
      ! REAL(KIND(1D0)), INTENT(in) :: runoff_per_interval_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_id_in ! wetness status of each surface type from previous timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_id_in !soil moisture of each surface type from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_in ! snowpack from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac_in !  snow fraction from previous timestep[-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowWater_in ! snow water from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: iceFrac_in ! ice fraction from previous timestep [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_in ! snow density from previous timestep[kg m-3]

      ! output:
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_id_out ! wetness status of each surface type at next timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstore_id_out !soil moisture of each surface type at next timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowPack_out ! snowpack at next timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowFrac_out !  snow fraction at next timestep[-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowWater_out ! snow water at nexts timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: iceFrac_out ! ice fraction at next timestep [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDens_out ! snow density at next timestep[kg m-3]

      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: runoffSnow_surf !Initialize for runoff caused by snowmelting
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_surf ! runoff for each surface [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ChangSnow_surf !change in SnowPack (mm)
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: snowDepth
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowToSurf !the water flowing into snow free area [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_snow !Evaporation of now [mm]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(out) :: SnowRemoval !snow removal [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_surf !evaporation of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rss_surf !redefined surface resistance for wet surfaces [s m-1]

      ! REAL(KIND(1D0)) :: p_mm !Inputs to surface water balance
      ! REAL(KIND(1d0)),INTENT(out)::rss
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn_surf ! net all-wave radiation of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qs_surf ! heat storage flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qe_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_roof ! latent heat flux of roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_wall ! latent heat flux of wall [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: state_per_tstep !state_id at each timestep [mm]
      REAL(KIND(1D0)), INTENT(out) :: NWstate_per_tstep ! state_id at each tinestep(excluding water body) [mm]
      REAL(KIND(1D0)), INTENT(out) :: qe !latent heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: swe !overall snow water equavalent[mm]
      REAL(KIND(1D0)), INTENT(out) :: chSnow_per_tstep ! change state_id of snow and surface per time interval [mm]
      REAL(KIND(1D0)), INTENT(out) :: ev_per_tstep ! evaporation at each time step [mm]
      REAL(KIND(1D0)) :: qe_per_tstep !latent heat flux at each timestep[W m-2]
      REAL(KIND(1D0)), INTENT(out) :: runoff_per_tstep !runoff water at each time step [mm]
      REAL(KIND(1D0)), INTENT(out) :: surf_chang_per_tstep !change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes !runoff to pipes [mm]
      REAL(KIND(1D0)), INTENT(out) :: mwstore !overall met water [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffwaterbody !Above ground runoff from water surface for all surface area [mm]
      ! REAL(KIND(1D0)) :: runoffWaterBody_m3
      ! REAL(KIND(1D0)) :: runoffPipes_m3
      REAL(KIND(1D0)), INTENT(out) :: runoffAGveg !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffAGimpervious !Above ground runoff from impervious surface for all surface area [mm]

      ! local:
      INTEGER :: is ! surface type [-]

      ! REAL(KIND(1D0)) :: runoff_per_interval
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id_surf ! wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id !soil moisture of each surface type[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowPack ! snowpack [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac !snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowWater ! water in snow [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: iceFrac !ice fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDens !snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_e_surf !net available energy for evaporation for each surfaces [W m-2]

      REAL(KIND(1D0)), DIMENSION(2) :: SurplusEvap !surface evaporation in 5 min timestep [mm]
      REAL(KIND(1D0)) :: surplusWaterBody !Extra runoff that goes to water body [mm] as specified by RunoffToWater
      REAL(KIND(1D0)) :: pin !Rain per time interval [mm]
      ! REAL(KIND(1d0))::sae
      ! REAL(KIND(1d0))::vdrc
      ! REAL(KIND(1d0))::sp
      ! REAL(KIND(1d0))::numPM
      REAL(KIND(1D0)) :: qn_e !net available energy for evaporation [W m-2]
      REAL(KIND(1D0)) :: tlv !Latent heat of vapourisation per timestep [J kg-1 s-1]
      ! REAL(KIND(1D0)) :: runoffAGimpervious_m3
      ! REAL(KIND(1D0)) :: runoffAGveg_m3
      REAL(KIND(1D0)) :: nsh_real !timestep in a hour [-]
      ! REAL(KIND(1D0)) :: tstep_real
      REAL(KIND(1D0)) :: ev_tot !total evaporation for all surfaces [mm]
      REAL(KIND(1D0)) :: qe_tot ! total latent heat flux for all surfaces [W m-2]
      REAL(KIND(1D0)) :: surf_chang_tot !total change in state_id(excluding snowpack) for all surfaces [mm]
      REAL(KIND(1D0)) :: runoff_tot !total runoff for all surfaces [mm]
      REAL(KIND(1D0)) :: chSnow_tot !total change state_id of snow and surface [mm]

      REAL(KIND(1D0)), DIMENSION(7) :: capStore_surf ! current storage capacity [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: Qm_freezState
      REAL(KIND(1D0)) :: mwh
      REAL(KIND(1D0)) :: fwh
      REAL(KIND(1D0)) :: Qm
      REAL(KIND(1D0)) :: QmFreez
      REAL(KIND(1D0)) :: QmRain
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDepth

      REAL(KIND(1D0)), INTENT(in) :: RadMeltFact
      REAL(KIND(1D0)), INTENT(in) :: TempMeltFact
      REAL(KIND(1D0)), INTENT(in) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(in) :: PrecipLimit
      REAL(KIND(1D0)), INTENT(in) :: PrecipLimitAlb

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: kup_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: deltaQi
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind_snow

      REAL(KIND(1D0)), INTENT(in) :: SnowfallCum_in
      REAL(KIND(1D0)), INTENT(out) :: SnowfallCum_out
      REAL(KIND(1D0)) :: SnowfallCum

      REAL(KIND(1D0)), INTENT(in) :: SnowAlb_in
      REAL(KIND(1D0)), INTENT(out) :: SnowAlb_out
      REAL(KIND(1D0)) :: SnowAlb
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5), INTENT(out) :: dataOutLineSnow

      WetThresh_surf = [WetThresh_paved, WetThresh_bldg, WetThresh_evetr, WetThresh_dectr, &
                        WetThresh_grass, WetThresh_bsoil, WetThresh_water]
      SoilStoreCap = [SoilStoreCap_paved, SoilStoreCap_bldg, SoilStoreCap_evetr, SoilStoreCap_dectr, &
                      SoilStoreCap_grass, SoilStoreCap_bsoil, SoilStoreCap_water]
      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]
      SnowProf_24hr(:, 1) = SnowProf_24hr_working
      SnowProf_24hr(:, 2) = SnowProf_24hr_holiday

      ! runoff_per_interval = runoff_per_interval_in
      state_id_surf = state_id_in
      soilstore_id = soilstore_id_in

      ! tstep_real = tstep*1.D0
      nsh_real = 3600/tstep*1.D0

      capStore_surf = 0 !initialise capStore

      tlv = lv_J_kg/tstep*1.D0 !Latent heat of vapourisation per timestep

      pin = MAX(0., Precip) !Initiate rain data [mm]

      ! Initialize the output variables
      qe_surf = 0

      ev_per_tstep = 0
      qe_per_tstep = 0
      surf_chang_per_tstep = 0
      runoff_per_tstep = 0
      state_per_tstep = 0
      NWstate_per_tstep = 0
      chSnow_per_tstep = 0
      qe = 0

      runoffAGveg = 0
      runoffAGimpervious = 0
      surplusWaterBody = 0
      runoff_surf = 0
      chang = 0
      SurplusEvap = 0

      ! force these facets to be totally dry
      ! TODO: need to consider their hydrologic dynamics
      qe_roof = 0
      qe_wall = 0

      ! net available energy for evaporation
      qn_e_surf = qn_surf + qf - qs_surf ! qn1 changed to qn1_snowfree, lj in May 2013

      IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_snow...'
      ! IF (SnowUse == 1) THEN ! snow calculation
      ! net available energy for evaporation
      qn_e = qn_snowfree + qf - qs ! qn1 changed to qn1_snowfree, lj in May 2013

      SnowPack = SnowPack_in
      SnowFrac = SnowFrac_in
      SnowWater = SnowWater_in
      iceFrac = iceFrac_in
      SnowDens = SnowDens_in
      SnowfallCum = SnowfallCum_in
      SnowAlb = SnowAlb_in

      ! update snow density
      SnowDens = update_snow_dens( &
                 tstep, SnowFrac, SnowDens, &
                 tau_r, SnowDensMax, SnowDensMin)

      ! Calculate snow-related energy budgets
      CALL MeltHeat( &
         lvS_J_kg, lv_J_kg, tstep*1D0, RadMeltFact, TempMeltFact, & !input
         SnowAlbMax, SnowDensMin, Temp_C, Precip, PrecipLimit, PrecipLimitAlb, &
         nsh_real, sfr_surf, Tsurf_ind, state_id_in, qn_ind_snow, &
         SnowWater, deltaQi, &
         SnowPack, SnowFrac, SnowAlb, SnowDens, SnowfallCum, & !inout
         mwh, fwh, Qm, QmFreez, QmRain, snowCalcSwitch, & !output
         Qm_melt, Qm_freezState, Qm_rain, FreezMelt, FreezState, FreezStateVol, &
         rainOnSnow, SnowDepth, mw_ind)

      DO is = 1, nsurf !For each surface in turn
         qe_tot = 0
         ev_tot = 0
         swe = 0
         ev_snow = 0
         runoff_tot = 0
         surf_chang_tot = 0
         chSnow_tot = 0
         SnowRemoval = 0
         runoffPipes = 0
         mwstore = 0
         runoffwaterbody = 0
         IF (sfr_surf(is) > 0) THEN
            ! IF (Diagnose == 1) WRITE (*, *) 'Calling SnowCalc...'

            CALL SnowCalc( &
               tstep, imin, it, dectime, is, & !input
               snowCalcSwitch, &
               EvapMethod, CRWmin, CRWmax, nsh_real, lvS_J_kg, avdens, &
               avRh, Press_hPa, Temp_C, RAsnow, psyc_hPa, avcp, sIce_hPa, &
               PervFraction, vegfraction, addimpervious, &
               vpd_hPa, qn_e, s_hPa, RS, RA, RB, tlv, SnowDensMin, SnowProf_24hr, precip, &
               PipeCapacity, RunoffToWater, &
               addVeg, SnowLimPaved, SnowLimBldg, FlowChange, drain, &
               WetThresh_surf, state_id_in, mw_ind, SoilStoreCap, rainonsnow, &
               freezmelt, freezstate, freezstatevol, &
               Qm_Melt, Qm_rain, Tsurf_ind, sfr_surf, dayofWeek_id, StoreDrainPrm, SnowPackLimit, &
               AddWater, addwaterrunoff, &
               soilstore_id, SnowPack, SurplusEvap, & !inout
               SnowFrac, SnowWater, iceFrac, SnowDens, &
               runoffAGimpervious, runoffAGveg, surplusWaterBody, &
               ev_tot, qe_tot, runoff_tot, surf_chang_tot, chSnow_tot, & ! output
               rss_surf, &
               runoff_surf, chang, ChangSnow_surf, SnowToSurf, state_id_surf, ev_snow, &
               SnowRemoval, swe, &
               runoffPipes, mwstore, runoffwaterbody)

         ELSE
            SnowFrac(is) = 0
            SnowDens(is) = 0
            SnowPack(is) = 0
         END IF
         !Actual updates here as xx_tstep variables not taken as input to snowcalc
         ev_per_tstep = ev_per_tstep + ev_tot
         qe_per_tstep = qe_per_tstep + qe_tot
         runoff_per_tstep = runoff_per_tstep + runoff_tot
         surf_chang_per_tstep = surf_chang_per_tstep + surf_chang_tot
         chSnow_per_tstep = chSnow_per_tstep + chSnow_tot

         !Store ev_tot for each surface
         ev_surf(is) = ev_tot

      END DO

      qe = qe_per_tstep

      ! Calculate volume of water that will move between grids
      ! Volume [m3] = Depth relative to whole area [mm] / 1000 [mm m-1] * SurfaceArea [m2]
      ! Need to use these volumes when converting back to addImpervious, AddVeg and AddWater
      ! runoffAGimpervious_m3 = runoffAGimpervious/1000*SurfaceArea
      ! runoffAGveg_m3 = runoffAGveg/1000*SurfaceArea
      ! runoffWaterBody_m3 = runoffWaterBody/1000*SurfaceArea
      ! runoffPipes_m3 = runoffPipes/1000*SurfaceArea

      state_id_out = state_id_surf
      soilstore_id_out = soilstore_id

      SnowWater_out = SnowWater
      iceFrac_out = iceFrac

      SnowAlb_out = SnowAlb
      SnowDens_out = SnowDens
      SnowPack_out = SnowPack
      SnowFrac_out = SnowFrac
      SnowfallCum_out = SnowfallCum

      ! pack output into one line
      dataOutLineSnow = [ &
                        SnowPack_out(1:nsurf), mw_ind(1:nsurf), Qm_melt(1:nsurf), & !26
                        Qm_rain(1:nsurf), Qm_freezState(1:nsurf), SnowFrac_out(1:(nsurf - 1)), & !46
                        rainOnSnow(1:nsurf), & !53
                        qn_ind_snow(1:nsurf), kup_ind_snow(1:nsurf), freezMelt(1:nsurf), & !74
                        SnowWater(1:nsurf), SnowDens_out(1:nsurf), & !88
                        snowDepth(1:nsurf), Tsurf_ind_snow(1:nsurf), &
                        SnowAlb_out]

   END SUBROUTINE SUEWS_cal_snow_DTS

!================latent heat flux and surface wetness===================
! TODO: optimise the structure of this function
   SUBROUTINE SUEWS_cal_QE( &
      Diagnose, storageheatmethod, nlayer, & !input
      tstep, &
      EvapMethod, &
      avdens, avcp, lv_J_kg, &
      psyc_hPa, &
      PervFraction, &
      addimpervious, &
      qf, vpd_hPa, s_hPa, RS, RA_h, RB, &
      precip, PipeCapacity, RunoffToWater, &
      NonWaterFraction, WU_surf, addVeg, addWaterBody, AddWater_surf, &
      FlowChange, drain_surf, &
      frac_waterdist_vert_surf, StoreCap_surf, &
      sfr_surf, StateLimit_surf, SoilStoreCap_surf, WetThresh_surf, & ! input:
      state_surf_in, soilstore_surf_in, qn_surf, qs_surf, & ! input:
      sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
      state_roof_in, soilstore_roof_in, qn_roof, qs_roof, & ! input:
      sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
      state_wall_in, soilstore_wall_in, qn_wall, qs_wall, & ! input:
      state_surf_out, soilstore_surf_out, ev_surf, & ! general output:
      state_roof_out, soilstore_roof_out, ev_roof, & ! general output:
      state_wall_out, soilstore_wall_out, ev_wall, & ! general output:
      state_grid, NWstate_grid, &
      ev0_surf, qe0_surf, &
      qe, qe_surf, qe_roof, qe_wall, &
      ev_grid, runoff_grid, &
      surf_chang_grid, runoffPipes_grid, &
      runoffWaterBody_grid, &
      runoffAGveg_grid, runoffAGimpervious_grid, rss_surf)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: storageheatmethod !Determines method for calculating storage heat flux ΔQS [-]
      INTEGER, INTENT(in) :: nlayer !number of vertical levels in urban canopy [-]
      INTEGER, INTENT(in) :: tstep !timesteps [s]
      ! INTEGER, INTENT(in) :: imin
      ! INTEGER, INTENT(in) :: it
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)

      REAL(KIND(1D0)), INTENT(in) :: lv_j_kg !Latent heat of vapourisation [J kg-1]
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa !Psychometric constant [hPa]
      REAL(KIND(1D0)), INTENT(in) :: avcp ! air heat capacity [J kg-1 K-1]

      REAL(KIND(1D0)), INTENT(in) :: PervFraction ! sum of surface cover fractions for impervious surfaces [-]
      ! REAL(KIND(1D0)), INTENT(in) :: vegfraction
      REAL(KIND(1D0)), INTENT(in) :: addimpervious !Water from impervious surfaces of other grids for whole surface area [mm]

      REAL(KIND(1D0)), INTENT(in) :: qf ! athropogenic heat flux [W m-2]

      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)), INTENT(in) :: s_hPa !vapour pressure versus temperature slope [hPa K-1]
      REAL(KIND(1D0)), INTENT(in) :: RS !surface resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RA_h !aerodynamic resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RB !boundary layer resistance [s m-1]
      ! REAL(KIND(1D0)), INTENT(in) :: snowdensmin
      REAL(KIND(1D0)), INTENT(in) :: precip !rain data [mm]
      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity !Capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater !Fraction of surface runoff going to water body [-]
      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction !Fraction of non-water surface [-]
      ! REAL(KIND(1d0)), INTENT(in)::wu_EveTr!Water use for evergreen trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_DecTr!Water use for deciduous trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_Grass!Water use for grass [mm]
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addWaterBody !Water from water surface of other grids [mm] for whole surface area
      ! REAL(KIND(1D0)), INTENT(in) :: SnowLimPaved
      ! REAL(KIND(1D0)), INTENT(in) :: SnowLimBldg
      ! REAL(KIND(1D0)), INTENT(in) :: SurfaceArea
      REAL(KIND(1D0)), INTENT(in) :: FlowChange !Difference between the input and output flow in the water body [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WU_surf !external water use of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain_surf !Drainage of each surface type [mm]

      ! input for generic suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StateLimit_surf !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WetThresh_surf !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_surf_in !wetness status of each surface type from previous timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_surf_in !initial water store in soil of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qs_surf ! latent heat flux of individual surface [W m-2]

      ! input for generic roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof !surface fraction ratio of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: StateLimit_roof !Limit for state_id of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: WetThresh_roof ! wetness threshold  of roof[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: SoilStoreCap_roof !Capacity of soil store for roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_roof_in !wetness status of roof from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_roof_in !Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qn_roof !net all-wave radiation for roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qs_roof !heat storage flux for roof [W m-2]

      ! input for generic wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall !surface fraction ratio of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: StateLimit_wall ! upper limit for state_id of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: WetThresh_wall ! wetness threshold  of roof[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: SoilStoreCap_wall !Capacity of soil store for wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_wall_in !wetness status of wall from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_wall_in !Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qn_wall !net all-wave radiation for wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qs_wall !heat storage flux for wall [W m-2]

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPackLimit
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater_surf !Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: frac_waterdist_vert_surf !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      ! REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm !Coefficients used in drainage calculation [-]
      ! REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: SnowProf_24hr

      ! Total water transported to each grid for grid-to-grid connectivity
      ! REAL(KIND(1D0)), INTENT(in) :: runoff_per_interval_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowWater_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: iceFrac_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_in

      ! output:
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_surf_out !wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstore_surf_out !soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: state_roof_out !Wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: soilstore_roof_out !soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: state_wall_out !wetness status of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: soilstore_wall_out !soil moisture of wall [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowPack_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowFrac_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowWater_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: iceFrac_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDens_out

      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: runoffSnow_surf !Initialize for runoff caused by snowmelting
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_surf !runoff from each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state_id [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: ChangSnow_surf
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: snowDepth
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowToSurf
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_snow
      ! REAL(KIND(1D0)), DIMENSION(2), INTENT(out) :: SnowRemoval
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qe0_surf !evaporation of each surface type by PM [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: ev0_surf !evaporation of each surface type by PM [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: ev_surf !evaporation of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rss_surf !Redefined surface resistance for wet surfaces [s m-1]

      ! REAL(KIND(1D0)) :: p_mm !Inputs to surface water balance
      ! REAL(KIND(1d0)),INTENT(out)::rss
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qe_surf ! latent heat flux on ground surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_roof ! latent heat flux on roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_wall ! latent heat flux on wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_roof ! evaporation of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: rss_roof ! redefined surface resistance for wet roof [s m-1]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: runoff_roof !runoff from roof [mm]
      ! REAL(KIND(1D0)) :: qe_roof_total !turbulent latent heat flux on the roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_wall ! evaporation of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: rss_wall ! redefined surface resistance for wet wall [s m-1]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: runoff_wall !runoff from wall [mm]
      ! REAL(KIND(1D0)) :: qe_wall_total !turbulent latent heat flux on the wall [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: state_grid !total state_id (including water body) [mm]
      REAL(KIND(1D0)), INTENT(out) :: NWstate_grid !total state_id (excluding water body) [mm]
      REAL(KIND(1D0)), INTENT(out) :: qe ! aggregated latent heat flux of all surfaces [W m-2]
      ! REAL(KIND(1D0)), INTENT(out) :: swe
      ! REAL(KIND(1D0)) :: ev
      ! REAL(KIND(1D0)), INTENT(out) :: chSnow_per_interval
      REAL(KIND(1D0)), INTENT(out) :: ev_grid ! total evaporation for all surfaces [mm]
      ! REAL(KIND(1D0)) :: qe_grid ! total latent heat flux [W m-2] for all surfaces [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: runoff_grid ! total runoff for all surfaces [mm]
      REAL(KIND(1D0)), INTENT(out) :: surf_chang_grid ! total change in surface state_id for all surfaces [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes_grid ! !Runoff in pipes for all surface area [mm]
      ! REAL(KIND(1D0)), INTENT(out) :: mwstore
      REAL(KIND(1D0)), INTENT(out) :: runoffWaterBody_grid !Above ground runoff from water surface for all surface area [mm]
      ! REAL(KIND(1D0)) :: runoffWaterBody_m3
      ! REAL(KIND(1D0)) :: runoffPipes_m3
      REAL(KIND(1D0)), INTENT(out) :: runoffAGveg_grid !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffAGimpervious_grid !Above ground runoff from impervious surface for all surface area [mm]

      ! local:
      ! INTEGER :: is

      ! REAL(KIND(1D0)) :: runoff_per_interval
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id_out
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id !Soil moisture of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowPack
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowWater
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: iceFrac
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDens
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_e_surf !net available energy for evaporation for each surface[W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qn_e_roof !net available energy for evaporation for roof[W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qn_e_wall !net available energy for evaporation for wall[W m-2]

      REAL(KIND(1D0)) :: pin !Rain per time interval
      REAL(KIND(1D0)) :: tlv !Latent heat of vapourisation per timestep [J kg-1 s-1]
      REAL(KIND(1D0)) :: nsh_real !timesteps per hour
      REAL(KIND(1D0)) :: state_building !aggregated surface water of building facets [mm]
      REAL(KIND(1D0)) :: soilstore_building !aggregated soilstore of building facets[mm]
      REAL(KIND(1D0)) :: capStore_builing ! aggregated storage capacity of building facets[mm]
      REAL(KIND(1D0)) :: runoff_building !aggregated Runoff of building facets [mm]
      REAL(KIND(1D0)) :: qe_building !aggregated qe of building facets[W m-2]

      REAL(KIND(1D0)), DIMENSION(7) :: StoreCap_surf ! current storage capacity [mm]

      ! runoff_per_interval = runoff_per_interval_in
      state_surf_out = state_surf_in
      soilstore_id = soilstore_surf_in

      nsh_real = 3600/tstep*1.D0

      tlv = lv_J_kg/tstep*1.D0 !Latent heat of vapourisation per timestep

      pin = MAX(0., Precip) !Initiate rain data [mm]

      ! force these facets to be totally dry
      ! TODO: need to consider their hydrologic dynamics
      qe_roof = 0
      qe_wall = 0
      qe0_surf = 0

      IF (Diagnose == 1) WRITE (*, *) 'Calling evap_SUEWS and SoilStore...'
      ! == calculate QE ==
      ! --- general suews surfaces ---
      ! net available energy for evaporation
      qn_e_surf = qn_surf + qf - qs_surf ! qn1 changed to qn1_snowfree, lj in May 2013

      ! above ground water store capacity
      ! capStore_surf = StoreDrainPrm(6, :)
      CALL cal_evap_multi( &
         EvapMethod, & !input
         sfr_surf, state_surf_in, WetThresh_surf, StoreCap_surf, & !input
         vpd_hPa, avdens, avcp, qn_e_surf, s_hPa, psyc_hPa, RS, RA_h, RB, tlv, &
         rss_surf, ev0_surf, qe0_surf) !output

      IF (storageheatmethod == 5) THEN
         ! --- roofs ---
         ! net available energy for evaporation
         qn_e_roof = qn_roof + qf - qs_roof ! qn1 changed to qn1_snowfree, lj in May 2013
         CALL cal_evap_multi( &
            EvapMethod, & !input
            sfr_roof, state_roof_in, WetThresh_roof, statelimit_roof, & !input
            vpd_hPa, avdens, avcp, qn_e_roof, s_hPa, psyc_hPa, RS, RA_h, RB, tlv, &
            rss_roof, ev_roof, qe_roof) !output

         ! --- walls ---
         ! net available energy for evaporation
         qn_e_wall = qn_wall + qf - qs_wall ! qn1 changed to qn1_snowfree, lj in May 2013
         CALL cal_evap_multi( &
            EvapMethod, & !input
            sfr_wall, state_wall_in, WetThresh_wall, statelimit_wall, & !input
            vpd_hPa, avdens, avcp, qn_e_wall, s_hPa, psyc_hPa, RS, RA_h, RB, tlv, &
            rss_wall, ev_wall, qe_wall) !output

         ! == calculate water balance ==
         ! --- building facets: roofs and walls ---
         CALL cal_water_storage_building( &
            pin, nsh_real, nlayer, &
            sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
            ev_roof, state_roof_in, soilstore_roof_in, & ! input:
            sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
            ev_wall, state_wall_in, soilstore_wall_in, & ! input:
            ev_roof, state_roof_out, soilstore_roof_out, runoff_roof, & ! general output:
            ev_wall, state_wall_out, soilstore_wall_out, runoff_wall, & ! general output:
            state_building, soilstore_building, runoff_building, capStore_builing)

         ! update QE based on the water balance
         qe_roof = tlv*ev_roof
         qe_wall = tlv*ev_wall

         IF (sfr_surf(BldgSurf) < 1.0E-8) THEN
            qe_building = 0.0
         ELSE
            qe_building = (DOT_PRODUCT(qe_roof, sfr_roof) + DOT_PRODUCT(qe_wall, sfr_wall))/sfr_surf(BldgSurf)
         END IF
      END IF
      ! --- general suews surfaces ---
      CALL cal_water_storage_surf( &
         pin, nsh_real, &
         PipeCapacity, RunoffToWater, & ! input:
         addImpervious, addVeg, addWaterBody, FlowChange, &
         SoilStoreCap_surf, StateLimit_surf, &
         PervFraction, &
         sfr_surf, drain_surf, AddWater_surf, frac_waterdist_vert_surf, WU_surf, &
         ev0_surf, state_surf_in, soilstore_surf_in, &
         ev_surf, state_surf_out, soilstore_surf_out, & ! output:
         runoff_surf, &
         runoffAGimpervious_grid, runoffAGveg_grid, runoffPipes_grid, runoffWaterBody_grid & ! output:
         )

      ! update QE based on the water balance
      qe_surf = tlv*ev_surf

      ! --- update building related ---
      IF (storageheatmethod == 5) THEN
         ! update building specific values
         qe_surf(BldgSurf) = qe_building
         state_surf_out(BldgSurf) = state_building
         soilstore_surf_out(BldgSurf) = soilstore_building/capStore_builing*StoreCap_surf(BldgSurf)
         runoff_surf(BldgSurf) = runoff_building
      END IF

      ! aggregate all surface water fluxes/amounts
      qe = DOT_PRODUCT(qe_surf, sfr_surf)

      ! Sum change from different surfaces to find total change to surface state_id
      surf_chang_grid = DOT_PRODUCT(state_surf_out - state_surf_in, sfr_surf)

      ! Sum evaporation from different surfaces to find total evaporation [mm]
      ev_grid = DOT_PRODUCT(ev_surf, sfr_surf)

      ! Sum runoff from different surfaces to find total runoff
      runoff_grid = DOT_PRODUCT(runoff_surf, sfr_surf)

      ! Calculate total state_id (including water body)
      state_grid = DOT_PRODUCT(state_surf_out, sfr_surf)

      IF (NonWaterFraction /= 0) THEN
         NWstate_grid = DOT_PRODUCT(state_surf_out(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction
      END IF
      ! Calculate volume of water that will move between grids
      ! Volume [m3] = Depth relative to whole area [mm] / 1000 [mm m-1] * SurfaceArea [m2]
      ! Need to use these volumes when converting back to addImpervious, AddVeg and AddWater
      ! runoffAGimpervious_m3 = runoffAGimpervious/1000*SurfaceArea
      ! runoffAGveg_m3 = runoffAGveg/1000*SurfaceArea
      ! runoffWaterBody_m3 = runoffWaterBody/1000*SurfaceArea
      ! runoffPipes_m3 = runoffPipes/1000*SurfaceArea

      ! state_id_out = state_id_out
      ! soilstore_id_out = soilstore_id
      IF (storageheatmethod == 5) THEN
         IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE soilstore_building = ', soilstore_building
         IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE capStore_builing = ', capStore_builing
         IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE capStore_surf(BldgSurf) = ', StoreCap_surf(BldgSurf)
      END IF
      IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE soilstore_id = ', soilstore_surf_out

   END SUBROUTINE SUEWS_cal_QE

   SUBROUTINE SUEWS_cal_QE_DTS( &
      Diagnose, storageheatmethod, nlayer, & !input
      tstep, &
      EvapMethod, &
      avdens, avcp, lv_J_kg, &
      psyc_hPa, &
      PervFraction, &
      addimpervious, &
      qf, vpd_hPa, s_hPa, RS, RA_h, RB, &
      precip, PipeCapacity, RunoffToWater, &
      NonWaterFraction, WU_surf, addVeg, addWaterBody, AddWater_surf, &
      FlowChange, drain_surf, &
      frac_water2runoff_surf, StoreDrainPrm, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, & !input
      StateLimit_paved, StateLimit_bldg, StateLimit_evetr, StateLimit_dectr, StateLimit_grass, StateLimit_bsoil, StateLimit_water, & !input
      SoilStoreCap_paved, SoilStoreCap_bldg, SoilStoreCap_evetr, SoilStoreCap_dectr, &
      SoilStoreCap_grass, SoilStoreCap_bsoil, SoilStoreCap_water, & !input
      WetThresh_paved, WetThresh_bldg, WetThresh_evetr, WetThresh_dectr, &
      WetThresh_grass, WetThresh_bsoil, WetThresh_water, & !input
      state_surf_in, soilstore_surf_in, qn_surf, qs_surf, & ! input:
      sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
      state_roof_in, soilstore_roof_in, qn_roof, qs_roof, & ! input:
      sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
      state_wall_in, soilstore_wall_in, qn_wall, qs_wall, & ! input:
      state_surf_out, soilstore_surf_out, ev_surf, & ! general output:
      state_roof_out, soilstore_roof_out, ev_roof, & ! general output:
      state_wall_out, soilstore_wall_out, ev_wall, & ! general output:
      state_grid, NWstate_grid, &
      ev0_surf, qe0_surf, &
      qe, qe_surf, qe_roof, qe_wall, &
      ev_grid, runoff_grid, &
      surf_chang_grid, runoffPipes_grid, &
      runoffWaterBody_grid, &
      runoffAGveg_grid, runoffAGimpervious_grid, rss_surf)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: storageheatmethod !Determines method for calculating storage heat flux ΔQS [-]
      INTEGER, INTENT(in) :: nlayer !number of vertical levels in urban canopy [-]
      INTEGER, INTENT(in) :: tstep !timesteps [s]
      ! INTEGER, INTENT(in) :: imin
      ! INTEGER, INTENT(in) :: it
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)

      REAL(KIND(1D0)), INTENT(in) :: lv_j_kg !Latent heat of vapourisation [J kg-1]
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa !Psychometric constant [hPa]
      REAL(KIND(1D0)), INTENT(in) :: avcp ! air heat capacity [J kg-1 K-1]

      REAL(KIND(1D0)), INTENT(in) :: PervFraction ! sum of surface cover fractions for impervious surfaces [-]
      ! REAL(KIND(1D0)), INTENT(in) :: vegfraction
      REAL(KIND(1D0)), INTENT(in) :: addimpervious !Water from impervious surfaces of other grids for whole surface area [mm]

      REAL(KIND(1D0)), INTENT(in) :: qf ! athropogenic heat flux [W m-2]

      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)), INTENT(in) :: s_hPa !vapour pressure versus temperature slope [hPa K-1]
      REAL(KIND(1D0)), INTENT(in) :: RS !surface resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RA_h !aerodynamic resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RB !boundary layer resistance [s m-1]
      ! REAL(KIND(1D0)), INTENT(in) :: snowdensmin
      REAL(KIND(1D0)), INTENT(in) :: precip !rain data [mm]
      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity !Capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater !Fraction of surface runoff going to water body [-]
      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction !Fraction of non-water surface [-]
      ! REAL(KIND(1d0)), INTENT(in)::wu_EveTr!Water use for evergreen trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_DecTr!Water use for deciduous trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_Grass!Water use for grass [mm]
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addWaterBody !Water from water surface of other grids [mm] for whole surface area
      ! REAL(KIND(1D0)), INTENT(in) :: SnowLimPaved
      ! REAL(KIND(1D0)), INTENT(in) :: SnowLimBldg
      ! REAL(KIND(1D0)), INTENT(in) :: SurfaceArea
      REAL(KIND(1D0)), INTENT(in) :: FlowChange !Difference between the input and output flow in the water body [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WU_surf !external water use of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain_surf !Drainage of each surface type [mm]

      ! input for generic suews surfaces
      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]

      REAL(KIND(1D0)), INTENT(IN) :: StateLimit_paved
      REAL(KIND(1D0)), INTENT(IN) :: StateLimit_bldg
      REAL(KIND(1D0)), INTENT(IN) :: StateLimit_evetr
      REAL(KIND(1D0)), INTENT(IN) :: StateLimit_dectr
      REAL(KIND(1D0)), INTENT(IN) :: StateLimit_grass
      REAL(KIND(1D0)), INTENT(IN) :: StateLimit_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: StateLimit_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: StateLimit_surf !Limit for state_id of each surface type [mm] (specified in input files)

      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_paved
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_bldg
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_evetr
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_dectr
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_grass
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: WetThresh_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: WetThresh_surf !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]

      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_paved
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_bldg
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_evetr
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_dectr
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_grass
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_bsoil
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_surf_in !wetness status of each surface type from previous timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_surf_in !initial water store in soil of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qs_surf ! latent heat flux of individual surface [W m-2]

      ! input for generic roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof !surface fraction ratio of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: StateLimit_roof !Limit for state_id of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: WetThresh_roof ! wetness threshold  of roof[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: SoilStoreCap_roof !Capacity of soil store for roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_roof_in !wetness status of roof from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_roof_in !Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qn_roof !net all-wave radiation for roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qs_roof !heat storage flux for roof [W m-2]

      ! input for generic wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall !surface fraction ratio of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: StateLimit_wall ! upper limit for state_id of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: WetThresh_wall ! wetness threshold  of roof[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: SoilStoreCap_wall !Capacity of soil store for wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_wall_in !wetness status of wall from previous timestep[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_wall_in !Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qn_wall !net all-wave radiation for wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qs_wall !heat storage flux for wall [W m-2]

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPackLimit
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater_surf !Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: frac_water2runoff_surf !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm !Coefficients used in drainage calculation [-]
      ! REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: SnowProf_24hr

      ! Total water transported to each grid for grid-to-grid connectivity
      ! REAL(KIND(1D0)), INTENT(in) :: runoff_per_interval_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowWater_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: iceFrac_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_in

      ! output:
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_surf_out !wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstore_surf_out !soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: state_roof_out !Wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: soilstore_roof_out !soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: state_wall_out !wetness status of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: soilstore_wall_out !soil moisture of wall [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowPack_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowFrac_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowWater_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: iceFrac_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDens_out

      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: runoffSnow_surf !Initialize for runoff caused by snowmelting
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_surf !runoff from each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state_id [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: ChangSnow_surf
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: snowDepth
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowToSurf
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_snow
      ! REAL(KIND(1D0)), DIMENSION(2), INTENT(out) :: SnowRemoval
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qe0_surf !evaporation of each surface type by PM [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: ev0_surf !evaporation of each surface type by PM [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: ev_surf !evaporation of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rss_surf !Redefined surface resistance for wet surfaces [s m-1]

      ! REAL(KIND(1D0)) :: p_mm !Inputs to surface water balance
      ! REAL(KIND(1d0)),INTENT(out)::rss
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qe_surf ! latent heat flux on ground surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_roof ! latent heat flux on roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_wall ! latent heat flux on wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_roof ! evaporation of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: rss_roof ! redefined surface resistance for wet roof [s m-1]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: runoff_roof !runoff from roof [mm]
      ! REAL(KIND(1D0)) :: qe_roof_total !turbulent latent heat flux on the roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_wall ! evaporation of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: rss_wall ! redefined surface resistance for wet wall [s m-1]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: runoff_wall !runoff from wall [mm]
      ! REAL(KIND(1D0)) :: qe_wall_total !turbulent latent heat flux on the wall [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: state_grid !total state_id (including water body) [mm]
      REAL(KIND(1D0)), INTENT(out) :: NWstate_grid !total state_id (excluding water body) [mm]
      REAL(KIND(1D0)), INTENT(out) :: qe ! aggregated latent heat flux of all surfaces [W m-2]
      ! REAL(KIND(1D0)), INTENT(out) :: swe
      ! REAL(KIND(1D0)) :: ev
      ! REAL(KIND(1D0)), INTENT(out) :: chSnow_per_interval
      REAL(KIND(1D0)), INTENT(out) :: ev_grid ! total evaporation for all surfaces [mm]
      ! REAL(KIND(1D0)) :: qe_grid ! total latent heat flux [W m-2] for all surfaces [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: runoff_grid ! total runoff for all surfaces [mm]
      REAL(KIND(1D0)), INTENT(out) :: surf_chang_grid ! total change in surface state_id for all surfaces [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes_grid ! !Runoff in pipes for all surface area [mm]
      ! REAL(KIND(1D0)), INTENT(out) :: mwstore
      REAL(KIND(1D0)), INTENT(out) :: runoffWaterBody_grid !Above ground runoff from water surface for all surface area [mm]
      ! REAL(KIND(1D0)) :: runoffWaterBody_m3
      ! REAL(KIND(1D0)) :: runoffPipes_m3
      REAL(KIND(1D0)), INTENT(out) :: runoffAGveg_grid !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffAGimpervious_grid !Above ground runoff from impervious surface for all surface area [mm]

      ! local:
      ! INTEGER :: is

      ! REAL(KIND(1D0)) :: runoff_per_interval
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id_out
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id !Soil moisture of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowPack
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowWater
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: iceFrac
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDens
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_e_surf !net available energy for evaporation for each surface[W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qn_e_roof !net available energy for evaporation for roof[W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qn_e_wall !net available energy for evaporation for wall[W m-2]

      REAL(KIND(1D0)) :: pin !Rain per time interval
      REAL(KIND(1D0)) :: tlv !Latent heat of vapourisation per timestep [J kg-1 s-1]
      REAL(KIND(1D0)) :: nsh_real !timesteps per hour
      REAL(KIND(1D0)) :: state_building !aggregated surface water of building facets [mm]
      REAL(KIND(1D0)) :: soilstore_building !aggregated soilstore of building facets[mm]
      REAL(KIND(1D0)) :: capStore_builing ! aggregated storage capacity of building facets[mm]
      REAL(KIND(1D0)) :: runoff_building !aggregated Runoff of building facets [mm]
      REAL(KIND(1D0)) :: qe_building !aggregated qe of building facets[W m-2]

      REAL(KIND(1D0)), DIMENSION(7) :: capStore_surf ! current storage capacity [mm]

      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]
      StateLimit_surf = [StateLimit_paved, StateLimit_bldg, StateLimit_evetr, StateLimit_dectr, &
                         StateLimit_grass, StateLimit_bsoil, StateLimit_water]
      SoilStoreCap_surf = [SoilStoreCap_paved, SoilStoreCap_bldg, SoilStoreCap_evetr, SoilStoreCap_dectr, &
                           SoilStoreCap_grass, SoilStoreCap_bsoil, SoilStoreCap_water]
      WetThresh_surf = [WetThresh_paved, WetThresh_bldg, WetThresh_evetr, WetThresh_dectr, &
                        WetThresh_grass, WetThresh_bsoil, WetThresh_water]

      ! runoff_per_interval = runoff_per_interval_in
      state_surf_out = state_surf_in
      soilstore_id = soilstore_surf_in

      nsh_real = 3600/tstep*1.D0

      tlv = lv_J_kg/tstep*1.D0 !Latent heat of vapourisation per timestep

      pin = MAX(0., Precip) !Initiate rain data [mm]

      ! force these facets to be totally dry
      ! TODO: need to consider their hydrologic dynamics
      qe_roof = 0
      qe_wall = 0
      qe0_surf = 0

      IF (Diagnose == 1) WRITE (*, *) 'Calling evap_SUEWS and SoilStore...'
      ! == calculate QE ==
      ! --- general suews surfaces ---
      ! net available energy for evaporation
      qn_e_surf = qn_surf + qf - qs_surf ! qn1 changed to qn1_snowfree, lj in May 2013

      ! soil store capacity
      capStore_surf = StoreDrainPrm(6, :)
      CALL cal_evap_multi( &
         EvapMethod, & !input
         sfr_surf, state_surf_in, WetThresh_surf, capStore_surf, & !input
         vpd_hPa, avdens, avcp, qn_e_surf, s_hPa, psyc_hPa, RS, RA_h, RB, tlv, &
         rss_surf, ev0_surf, qe0_surf) !output

      IF (storageheatmethod == 5) THEN
         ! --- roofs ---
         ! net available energy for evaporation
         qn_e_roof = qn_roof + qf - qs_roof ! qn1 changed to qn1_snowfree, lj in May 2013
         CALL cal_evap_multi( &
            EvapMethod, & !input
            sfr_roof, state_roof_in, WetThresh_roof, statelimit_roof, & !input
            vpd_hPa, avdens, avcp, qn_e_roof, s_hPa, psyc_hPa, RS, RA_h, RB, tlv, &
            rss_roof, ev_roof, qe_roof) !output

         ! --- walls ---
         ! net available energy for evaporation
         qn_e_wall = qn_wall + qf - qs_wall ! qn1 changed to qn1_snowfree, lj in May 2013
         CALL cal_evap_multi( &
            EvapMethod, & !input
            sfr_wall, state_wall_in, WetThresh_wall, statelimit_wall, & !input
            vpd_hPa, avdens, avcp, qn_e_wall, s_hPa, psyc_hPa, RS, RA_h, RB, tlv, &
            rss_wall, ev_wall, qe_wall) !output

         ! == calculate water balance ==
         ! --- building facets: roofs and walls ---
         CALL cal_water_storage_building( &
            pin, nsh_real, nlayer, &
            sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
            ev_roof, state_roof_in, soilstore_roof_in, & ! input:
            sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
            ev_wall, state_wall_in, soilstore_wall_in, & ! input:
            ev_roof, state_roof_out, soilstore_roof_out, runoff_roof, & ! general output:
            ev_wall, state_wall_out, soilstore_wall_out, runoff_wall, & ! general output:
            state_building, soilstore_building, runoff_building, capStore_builing)

         ! update QE based on the water balance
         qe_roof = tlv*ev_roof
         qe_wall = tlv*ev_wall

         IF (sfr_surf(BldgSurf) < 1.0E-8) THEN
            qe_building = 0.0
         ELSE
            qe_building = (DOT_PRODUCT(qe_roof, sfr_roof) + DOT_PRODUCT(qe_wall, sfr_wall))/sfr_surf(BldgSurf)
         END IF
      END IF
      ! --- general suews surfaces ---
      CALL cal_water_storage_surf( &
         pin, nsh_real, &
         PipeCapacity, RunoffToWater, & ! input:
         addImpervious, addVeg, addWaterBody, FlowChange, &
         SoilStoreCap_surf, StateLimit_surf, &
         PervFraction, &
         sfr_surf, drain_surf, AddWater_surf, frac_water2runoff_surf, WU_surf, &
         ev0_surf, state_surf_in, soilstore_surf_in, &
         ev_surf, state_surf_out, soilstore_surf_out, & ! output:
         runoff_surf, &
         runoffAGimpervious_grid, runoffAGveg_grid, runoffPipes_grid, runoffWaterBody_grid & ! output:
         )

      ! update QE based on the water balance
      qe_surf = tlv*ev_surf

      ! --- update building related ---
      IF (storageheatmethod == 5) THEN
         ! update building specific values
         qe_surf(BldgSurf) = qe_building
         state_surf_out(BldgSurf) = state_building
         soilstore_surf_out(BldgSurf) = soilstore_building/capStore_builing*capStore_surf(BldgSurf)
         runoff_surf(BldgSurf) = runoff_building
      END IF

      ! aggregate all surface water fluxes/amounts
      qe = DOT_PRODUCT(qe_surf, sfr_surf)

      ! Sum change from different surfaces to find total change to surface state_id
      surf_chang_grid = DOT_PRODUCT(state_surf_out - state_surf_in, sfr_surf)

      ! Sum evaporation from different surfaces to find total evaporation [mm]
      ev_grid = DOT_PRODUCT(ev_surf, sfr_surf)

      ! Sum runoff from different surfaces to find total runoff
      runoff_grid = DOT_PRODUCT(runoff_surf, sfr_surf)

      ! Calculate total state_id (including water body)
      state_grid = DOT_PRODUCT(state_surf_out, sfr_surf)

      IF (NonWaterFraction /= 0) THEN
         NWstate_grid = DOT_PRODUCT(state_surf_out(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction
      END IF
      ! Calculate volume of water that will move between grids
      ! Volume [m3] = Depth relative to whole area [mm] / 1000 [mm m-1] * SurfaceArea [m2]
      ! Need to use these volumes when converting back to addImpervious, AddVeg and AddWater
      ! runoffAGimpervious_m3 = runoffAGimpervious/1000*SurfaceArea
      ! runoffAGveg_m3 = runoffAGveg/1000*SurfaceArea
      ! runoffWaterBody_m3 = runoffWaterBody/1000*SurfaceArea
      ! runoffPipes_m3 = runoffPipes/1000*SurfaceArea

      ! state_id_out = state_id_out
      ! soilstore_id_out = soilstore_id
      IF (storageheatmethod == 5) THEN
         IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE soilstore_building = ', soilstore_building
         IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE capStore_builing = ', capStore_builing
         IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE capStore_surf(BldgSurf) = ', capStore_surf(BldgSurf)
      END IF
      IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE soilstore_id = ', soilstore_surf_out

   END SUBROUTINE SUEWS_cal_QE_DTS
!========================================================================

!===============sensible heat flux======================================
   SUBROUTINE SUEWS_cal_QH( &
      QHMethod, nlayer, storageheatmethod, & !input
      qn, qf, QmRain, qe, qs, QmFreez, qm, avdens, avcp, &
      sfr_surf, sfr_roof, sfr_wall, &
      tsfc_surf, tsfc_roof, tsfc_wall, &
      Temp_C, &
      RA, &
      qh, qh_residual, qh_resist, & !output
      qh_resist_surf, qh_resist_roof, qh_resist_wall)
      IMPLICIT NONE

      INTEGER, INTENT(in) :: QHMethod ! option for QH calculation: 1, residual; 2, resistance-based
      INTEGER, INTENT(in) :: storageheatmethod !Determines method for calculating storage heat flux ΔQS [-]
      INTEGER, INTENT(in) :: nlayer !number of vertical levels in urban canopy [-]

      REAL(KIND(1D0)), INTENT(in) :: qn !net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qf ! anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: QmRain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qe !latent heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qs !heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: QmFreez !heat related to freezing of surface store [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qm !Snowmelt-related heat [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avcp !air heat capacity [J kg-1 K-1]
      ! REAL(KIND(1D0)), INTENT(in) :: tsurf
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: RA !aerodynamic resistance [s m-1]

      REAL(KIND(1D0)), INTENT(out) :: qh ! turtbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qh_resist !resistance bnased sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qh_residual ! residual based sensible heat flux [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf !surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !surface fraction ratio [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qh_resist_surf !resistance-based sensible heat flux [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof !surface fraction of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof !roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qh_resist_roof !resistance-based sensible heat flux of roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall !surface fraction of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall !wall surface temperature[degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qh_resist_wall !resistance-based sensible heat flux of wall [W m-2]

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER :: is

      ! Calculate sensible heat flux as a residual (Modified by LJ in Nov 2012)
      qh_residual = (qn + qf + QmRain) - (qe + qs + Qm + QmFreez) !qh=(qn1+qf+QmRain+QmFreez)-(qeOut+qs+Qm)

      ! ! Calculate QH using resistance method (for testing HCW 06 Jul 2016)
      ! Aerodynamic-Resistance-based method
      DO is = 1, nsurf
         IF (RA /= 0) THEN
            qh_resist_surf(is) = avdens*avcp*(tsfc_surf(is) - Temp_C)/RA
         ELSE
            qh_resist_surf(is) = NAN
         END IF
      END DO
      IF (storageheatmethod == 5) THEN
         DO is = 1, nlayer
            IF (RA /= 0) THEN
               qh_resist_roof(is) = avdens*avcp*(tsfc_roof(is) - Temp_C)/RA
               qh_resist_wall(is) = avdens*avcp*(tsfc_wall(is) - Temp_C)/RA
            ELSE
               qh_resist_surf(is) = NAN
            END IF
         END DO

         ! IF (RA /= 0) THEN
         !    qh_resist = avdens*avcp*(tsurf - Temp_C)/RA
         ! ELSE
         !    qh_resist = NAN
         ! END IF
         ! aggregate QH of roof and wall
         qh_resist_surf(BldgSurf) = (DOT_PRODUCT(qh_resist_roof, sfr_roof) + DOT_PRODUCT(qh_resist_wall, sfr_wall))/2.
      END IF

      qh_resist = DOT_PRODUCT(qh_resist_surf, sfr_surf)

      ! choose output QH
      SELECT CASE (QHMethod)
      CASE (1)
         qh = qh_residual
      CASE (2)
         qh = qh_resist
      END SELECT

   END SUBROUTINE SUEWS_cal_QH

   SUBROUTINE SUEWS_cal_QH_DTS( &
      QHMethod, nlayer, storageheatmethod, & !input
      qn, qf, QmRain, qe, qs, QmFreez, qm, avdens, avcp, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, &
      sfr_roof, sfr_wall, &
      tsfc_surf, tsfc_roof, tsfc_wall, &
      Temp_C, &
      RA, &
      qh, qh_residual, qh_resist, & !output
      qh_resist_surf, qh_resist_roof, qh_resist_wall)
      IMPLICIT NONE

      INTEGER, INTENT(in) :: QHMethod ! option for QH calculation: 1, residual; 2, resistance-based
      INTEGER, INTENT(in) :: storageheatmethod !Determines method for calculating storage heat flux ΔQS [-]
      INTEGER, INTENT(in) :: nlayer !number of vertical levels in urban canopy [-]

      REAL(KIND(1D0)), INTENT(in) :: qn !net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qf ! anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: QmRain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qe !latent heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qs !heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: QmFreez !heat related to freezing of surface store [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qm !Snowmelt-related heat [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avcp !air heat capacity [J kg-1 K-1]
      ! REAL(KIND(1D0)), INTENT(in) :: tsurf
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: RA !aerodynamic resistance [s m-1]

      REAL(KIND(1D0)), INTENT(out) :: qh ! turtbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qh_resist !resistance bnased sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qh_residual ! residual based sensible heat flux [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf !surface temperature [degC]

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf !surface fraction ratio [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qh_resist_surf !resistance-based sensible heat flux [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof !surface fraction of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof !roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qh_resist_roof !resistance-based sensible heat flux of roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall !surface fraction of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall !wall surface temperature[degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qh_resist_wall !resistance-based sensible heat flux of wall [W m-2]

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER :: is

      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]
      ! Calculate sensible heat flux as a residual (Modified by LJ in Nov 2012)
      qh_residual = (qn + qf + QmRain) - (qe + qs + Qm + QmFreez) !qh=(qn1+qf+QmRain+QmFreez)-(qeOut+qs+Qm)

      ! ! Calculate QH using resistance method (for testing HCW 06 Jul 2016)
      ! Aerodynamic-Resistance-based method
      DO is = 1, nsurf
         IF (RA /= 0) THEN
            qh_resist_surf(is) = avdens*avcp*(tsfc_surf(is) - Temp_C)/RA
         ELSE
            qh_resist_surf(is) = NAN
         END IF
      END DO
      IF (storageheatmethod == 5) THEN
         DO is = 1, nlayer
            IF (RA /= 0) THEN
               qh_resist_roof(is) = avdens*avcp*(tsfc_roof(is) - Temp_C)/RA
               qh_resist_wall(is) = avdens*avcp*(tsfc_wall(is) - Temp_C)/RA
            ELSE
               qh_resist_surf(is) = NAN
            END IF
         END DO

         ! IF (RA /= 0) THEN
         !    qh_resist = avdens*avcp*(tsurf - Temp_C)/RA
         ! ELSE
         !    qh_resist = NAN
         ! END IF
         ! aggregate QH of roof and wall
         qh_resist_surf(BldgSurf) = (DOT_PRODUCT(qh_resist_roof, sfr_roof) + DOT_PRODUCT(qh_resist_wall, sfr_wall))/2.
      END IF

      qh_resist = DOT_PRODUCT(qh_resist_surf, sfr_surf)

      ! choose output QH
      SELECT CASE (QHMethod)
      CASE (1)
         qh = qh_residual
      CASE (2)
         qh = qh_resist
      END SELECT

   END SUBROUTINE SUEWS_cal_QH_DTS
!========================================================================

!===============Resistance Calculations=======================
   SUBROUTINE SUEWS_cal_Resistance( &
      StabilityMethod, & !input:
      Diagnose, AerodynamicResistanceMethod, RoughLenHeatMethod, SnowUse, &
      id, it, gsModel, SMDMethod, &
      avdens, avcp, QH_init, zzd, z0m, zdm, &
      avU1, Temp_C, VegFraction, &
      avkdn, Kmax, &
      G_max, G_k, G_q_base, G_q_shape, &
      G_t, G_sm, S1, S2, &
      TH, TL, &
      dq, xsmd, vsmd, MaxConductance, LAIMax, LAI_id, SnowFrac, sfr_surf, &
      g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
      UStar, TStar, L_mod, & !output
      zL, gsc, RS, RA, RASnow, RB, z0v, z0vSnow)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: StabilityMethod !method to calculate atmospheric stability [-]
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: AerodynamicResistanceMethod !method to calculate RA [-]
      INTEGER, INTENT(in) :: RoughLenHeatMethod !method to calculate heat roughness length [-]
      INTEGER, INTENT(in) :: SnowUse !!Snow part used (1) or not used (0) [-]
      INTEGER, INTENT(in) :: id ! day of the year [-]
      INTEGER, INTENT(in) :: it !hour [h]
      INTEGER, INTENT(in) :: gsModel !Choice of gs parameterisation (1 = Ja11, 2 = Wa16)
      INTEGER, INTENT(in) :: SMDMethod !Method of measured soil moisture

      ! REAL(KIND(1d0)), INTENT(in)::qh_obs
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avcp !air heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: QH_init !initial sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: zzd !Active measurement height (meas. height-displac. height) [m]
      REAL(KIND(1D0)), INTENT(in) :: z0m !Aerodynamic roughness length [m]
      REAL(KIND(1D0)), INTENT(in) :: zdm !Displacement height [m]
      REAL(KIND(1D0)), INTENT(in) :: avU1 !Average wind speed [m s-1]
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !Air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: VegFraction !Fraction of vegetation [-]
      REAL(KIND(1D0)), INTENT(in) :: avkdn !Average downwelling shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Kmax !Annual maximum hourly solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: G_max !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_k !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_q_base !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_q_shape !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_t !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_sm !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: S1 !a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)), INTENT(in) :: S2 !a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)), INTENT(in) :: TH !Maximum temperature limit [degC]
      REAL(KIND(1D0)), INTENT(in) :: TL !Minimum temperature limit [degC]
      REAL(KIND(1D0)), INTENT(in) :: dq !Specific humidity deficit
      REAL(KIND(1D0)), INTENT(in) :: xsmd !Measured soil moisture deficit
      REAL(KIND(1D0)), INTENT(in) :: vsmd !Soil moisture deficit for vegetated surfaces only[mm]

      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: MaxConductance !the maximum conductance of each vegetation or surface type. [mm s-1]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: LAIMax !Max LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: LAI_id !=LAI_id(id-1,:), LAI for each veg surface [m2 m-2]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac !Surface fraction of snow cover [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !Surface fractions [-]

      REAL(KIND(1D0)), INTENT(out) :: TStar !T* temperature scale
      REAL(KIND(1D0)), INTENT(out) :: UStar !friction velocity [m s-1]
      REAL(KIND(1D0)), INTENT(out) :: zL !stability scale
      REAL(KIND(1D0)), INTENT(out) :: gsc !Surface Layer Conductance
      REAL(KIND(1D0)), INTENT(out) :: RS !surface resistance [s m-1]
      REAL(KIND(1D0)), INTENT(out) :: RA !Aerodynamic resistance [s m-1]
      REAL(KIND(1D0)), INTENT(out) :: z0v !roughness for heat [m]
      REAL(KIND(1D0)), INTENT(out) :: RASnow !Aerodynamic resistance for snow [s m-1]
      REAL(KIND(1D0)), INTENT(out) :: z0vSnow !roughness for heat [m]
      REAL(KIND(1D0)), INTENT(out) :: RB !boundary layer resistance shuttleworth
      REAL(KIND(1D0)), INTENT(out) :: L_mod !Obukhov length [m]

      REAL(KIND(1D0)), INTENT(out) :: g_kdown !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_dq !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_ta !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_smd !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_lai !gdq*gtemp*gs*gq for photosynthesis calculations

      REAL(KIND(1D0)) :: gfunc !gdq*gtemp*gs*gq for photosynthesis calculations
      ! REAL(KIND(1d0))              ::H_init    !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity

      ! Get first estimate of sensible heat flux. Modified by HCW 26 Feb 2015
      ! CALL SUEWS_init_QH( &
      !    avdens, avcp, QH_init, qn1, dectime, &
      !    H_init)
      RAsnow = 0.0

      IF (Diagnose == 1) WRITE (*, *) 'Calling STAB_lumps...'
      !u* and Obukhov length out
      CALL cal_Stab( &
         StabilityMethod, & ! input
         zzd, & !Active measurement height (meas. height-displac. height)
         z0m, & !Aerodynamic roughness length
         zdm, & !zero-plane displacement
         avU1, & !Average wind speed
         Temp_C, & !Air temperature
         QH_init, & !sensible heat flux
         avdens, & ! air density
         avcp, & ! heat capacity of air
         L_mod, & ! output: !Obukhov length
         TStar, & !T*, temperature scale
         UStar, & !Friction velocity
         zL) !Stability scale

      IF (Diagnose == 1) WRITE (*, *) 'Calling AerodynamicResistance...'
      CALL AerodynamicResistance( &
         ZZD, & ! input:
         z0m, &
         AVU1, &
         L_mod, &
         UStar, &
         VegFraction, &
         AerodynamicResistanceMethod, &
         StabilityMethod, &
         RoughLenHeatMethod, &
         RA, z0v) ! output:

      IF (SnowUse == 1) THEN
         IF (Diagnose == 1) WRITE (*, *) 'Calling AerodynamicResistance for snow...'
         CALL AerodynamicResistance( &
            ZZD, & ! input:
            z0m, &
            AVU1, &
            L_mod, &
            UStar, &
            VegFraction, &
            AerodynamicResistanceMethod, &
            StabilityMethod, &
            3, &
            RASnow, z0vSnow) ! output:
      END IF

      IF (Diagnose == 1) WRITE (*, *) 'Calling SurfaceResistance...'
      ! CALL SurfaceResistance(id,it)   !qsc and surface resistance out
      CALL SurfaceResistance( &
         id, it, & ! input:
         SMDMethod, SnowFrac, sfr_surf, avkdn, Temp_C, dq, xsmd, vsmd, MaxConductance, &
         LAIMax, LAI_id, gsModel, Kmax, &
         G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, TH, TL, S1, S2, &
         g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
         gfunc, gsc, RS) ! output:

      IF (Diagnose == 1) WRITE (*, *) 'Calling BoundaryLayerResistance...'
      CALL BoundaryLayerResistance( &
         zzd, & ! input:     !Active measurement height (meas. height- zero-plane displacement)
         z0m, & !Aerodynamic roughness length
         avU1, & !Average wind speed
         UStar, & ! input/output:
         RB) ! output:

   END SUBROUTINE SUEWS_cal_Resistance

   SUBROUTINE SUEWS_cal_Resistance_DTS( &
      StabilityMethod, & !input:
      Diagnose, AerodynamicResistanceMethod, RoughLenHeatMethod, SnowUse, &
      id, it, gsModel, SMDMethod, &
      avdens, avcp, QH_init, zzd, z0m, zdm, &
      avU1, Temp_C, VegFraction, &
      avkdn, Kmax, &
      G_max, G_k, G_q_base, G_q_shape, &
      G_t, G_sm, S1, S2, &
      TH, TL, &
      dq, xsmd, vsmd, &
      MaxConductance_evetr, MaxConductance_dectr, MaxConductance_grass, &
      LAIMax_evetr, LAIMax_dectr, LAIMax_grass, &
      LAI_id, SnowFrac, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, & !input
      g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
      UStar, TStar, L_mod, & !output
      zL, gsc, RS, RA, RASnow, RB, z0v, z0vSnow)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: StabilityMethod !method to calculate atmospheric stability [-]
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: AerodynamicResistanceMethod !method to calculate RA [-]
      INTEGER, INTENT(in) :: RoughLenHeatMethod !method to calculate heat roughness length [-]
      INTEGER, INTENT(in) :: SnowUse !!Snow part used (1) or not used (0) [-]
      INTEGER, INTENT(in) :: id ! day of the year [-]
      INTEGER, INTENT(in) :: it !hour [h]
      INTEGER, INTENT(in) :: gsModel !Choice of gs parameterisation (1 = Ja11, 2 = Wa16)
      INTEGER, INTENT(in) :: SMDMethod !Method of measured soil moisture

      ! REAL(KIND(1d0)), INTENT(in)::qh_obs
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avcp !air heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: QH_init !initial sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: zzd !Active measurement height (meas. height-displac. height) [m]
      REAL(KIND(1D0)), INTENT(in) :: z0m !Aerodynamic roughness length [m]
      REAL(KIND(1D0)), INTENT(in) :: zdm !Displacement height [m]
      REAL(KIND(1D0)), INTENT(in) :: avU1 !Average wind speed [m s-1]
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !Air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: VegFraction !Fraction of vegetation [-]
      REAL(KIND(1D0)), INTENT(in) :: avkdn !Average downwelling shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Kmax !Annual maximum hourly solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: G_max !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_k !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_q_base !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_q_shape !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_t !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G_sm !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: S1 !a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)), INTENT(in) :: S2 !a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)), INTENT(in) :: TH !Maximum temperature limit [degC]
      REAL(KIND(1D0)), INTENT(in) :: TL !Minimum temperature limit [degC]
      REAL(KIND(1D0)), INTENT(in) :: dq !Specific humidity deficit
      REAL(KIND(1D0)), INTENT(in) :: xsmd !Measured soil moisture deficit
      REAL(KIND(1D0)), INTENT(in) :: vsmd !Soil moisture deficit for vegetated surfaces only[mm]

      REAL(KIND(1D0)), INTENT(in) :: MaxConductance_dectr
      REAL(KIND(1D0)), INTENT(in) :: MaxConductance_evetr
      REAL(KIND(1D0)), INTENT(in) :: MaxConductance_grass
      REAL(KIND(1D0)), DIMENSION(3) :: MaxConductance !the maximum conductance of each vegetation or surface type. [mm s-1]

      REAL(KIND(1D0)), INTENT(in) :: LAIMax_dectr
      REAL(KIND(1D0)), INTENT(in) :: LAIMax_evetr
      REAL(KIND(1D0)), INTENT(in) :: LAIMax_grass
      REAL(KIND(1D0)), DIMENSION(3) :: LAIMax !Max LAI [m2 m-2]

      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: LAI_id !=LAI_id(id-1,:), LAI for each veg surface [m2 m-2]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac !Surface fraction of snow cover [-]

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]

      REAL(KIND(1D0)), INTENT(out) :: TStar !T* temperature scale
      REAL(KIND(1D0)), INTENT(out) :: UStar !friction velocity [m s-1]
      REAL(KIND(1D0)), INTENT(out) :: zL !stability scale
      REAL(KIND(1D0)), INTENT(out) :: gsc !Surface Layer Conductance
      REAL(KIND(1D0)), INTENT(out) :: RS !surface resistance [s m-1]
      REAL(KIND(1D0)), INTENT(out) :: RA !Aerodynamic resistance [s m-1]
      REAL(KIND(1D0)), INTENT(out) :: z0v !roughness for heat [m]
      REAL(KIND(1D0)), INTENT(out) :: RASnow !Aerodynamic resistance for snow [s m-1]
      REAL(KIND(1D0)), INTENT(out) :: z0vSnow !roughness for heat [m]
      REAL(KIND(1D0)), INTENT(out) :: RB !boundary layer resistance shuttleworth
      REAL(KIND(1D0)), INTENT(out) :: L_mod !Obukhov length [m]

      REAL(KIND(1D0)), INTENT(out) :: g_kdown !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_dq !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_ta !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_smd !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_lai !gdq*gtemp*gs*gq for photosynthesis calculations

      REAL(KIND(1D0)) :: gfunc !gdq*gtemp*gs*gq for photosynthesis calculations
      ! REAL(KIND(1d0))              ::H_init    !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity

      ! Get first estimate of sensible heat flux. Modified by HCW 26 Feb 2015
      ! CALL SUEWS_init_QH( &
      !    avdens, avcp, QH_init, qn1, dectime, &
      !    H_init)
      RAsnow = 0.0

      MaxConductance(1) = MaxConductance_evetr
      MaxConductance(2) = MaxConductance_dectr
      MaxConductance(3) = MaxConductance_grass

      LAIMax(1) = LAIMax_evetr
      LAIMax(2) = LAIMax_dectr
      LAIMax(3) = LAIMax_grass

      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]

      IF (Diagnose == 1) WRITE (*, *) 'Calling STAB_lumps...'
      !u* and Obukhov length out
      CALL cal_Stab( &
         StabilityMethod, & ! input
         zzd, & !Active measurement height (meas. height-displac. height)
         z0m, & !Aerodynamic roughness length
         zdm, & !zero-plane displacement
         avU1, & !Average wind speed
         Temp_C, & !Air temperature
         QH_init, & !sensible heat flux
         avdens, & ! air density
         avcp, & ! heat capacity of air
         L_mod, & ! output: !Obukhov length
         TStar, & !T*, temperature scale
         UStar, & !Friction velocity
         zL) !Stability scale

      IF (Diagnose == 1) WRITE (*, *) 'Calling AerodynamicResistance...'
      CALL AerodynamicResistance( &
         ZZD, & ! input:
         z0m, &
         AVU1, &
         L_mod, &
         UStar, &
         VegFraction, &
         AerodynamicResistanceMethod, &
         StabilityMethod, &
         RoughLenHeatMethod, &
         RA, z0v) ! output:

      IF (SnowUse == 1) THEN
         IF (Diagnose == 1) WRITE (*, *) 'Calling AerodynamicResistance for snow...'
         CALL AerodynamicResistance( &
            ZZD, & ! input:
            z0m, &
            AVU1, &
            L_mod, &
            UStar, &
            VegFraction, &
            AerodynamicResistanceMethod, &
            StabilityMethod, &
            3, &
            RASnow, z0vSnow) ! output:
      END IF

      IF (Diagnose == 1) WRITE (*, *) 'Calling SurfaceResistance...'
      ! CALL SurfaceResistance(id,it)   !qsc and surface resistance out
      CALL SurfaceResistance( &
         id, it, & ! input:
         SMDMethod, SnowFrac, sfr_surf, avkdn, Temp_C, dq, xsmd, vsmd, MaxConductance, &
         LAIMax, LAI_id, gsModel, Kmax, &
         G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, TH, TL, S1, S2, &
         g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
         gfunc, gsc, RS) ! output:

      IF (Diagnose == 1) WRITE (*, *) 'Calling BoundaryLayerResistance...'
      CALL BoundaryLayerResistance( &
         zzd, & ! input:     !Active measurement height (meas. height- zero-plane displacement)
         z0m, & !Aerodynamic roughness length
         avU1, & !Average wind speed
         UStar, & ! input/output:
         RB) ! output:

   END SUBROUTINE SUEWS_cal_Resistance_DTS
!========================================================================

!==============Update output arrays=========================
   SUBROUTINE SUEWS_update_outputLine( &
      AdditionalWater, alb, avkdn, avU10_ms, azimuth, & !input
      chSnow_per_interval, dectime, &
      drain_per_tstep, E_mod, ev_per_tstep, ext_wu, Fc, Fc_build, fcld, &
      Fc_metab, Fc_photo, Fc_respi, Fc_point, Fc_traff, FlowChange, &
      h_mod, id, imin, int_wu, it, iy, &
      kup, LAI_id, ldown, l_mod, lup, mwh, &
      MwStore, &
      nsh_real, NWstate_per_tstep, Precip, q2_gkg, &
      qeOut, qf, qh, qh_resist, Qm, QmFreez, &
      QmRain, qn, qn_snow, qn_snowfree, qs, RA, &
      resistsurf, RH2, runoffAGimpervious, runoffAGveg, &
      runoff_per_tstep, runoffPipes, runoffSoil_per_tstep, &
      runoffWaterBody, sfr_surf, smd, smd_nsurf, SnowAlb, SnowRemoval, &
      state_id, state_per_tstep, surf_chang_per_tstep, swe, t2_C, tskin_C, &
      tot_chang_per_tstep, tsurf, UStar, &
      wu_nsurf, &
      z0m, zdm, zenith_deg, &
      datetimeLine, dataOutLineSUEWS) !output
      IMPLICIT NONE

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER, INTENT(in) :: iy ! year [YYYY]
      INTEGER, INTENT(in) :: id ! day of the year [DOY]
      INTEGER, INTENT(in) :: it ! hour [H]
      INTEGER, INTENT(in) :: imin ! minutes [M]
      REAL(KIND(1D0)), INTENT(in) :: AdditionalWater !Additional water coming from other grids [mm]
      REAL(KIND(1D0)), INTENT(in) :: alb(nsurf) !albedo of each surfaces [-]
      REAL(KIND(1D0)), INTENT(in) :: avkdn !Average downwelling shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avU10_ms !average wind speed at 10m [W m-1]
      REAL(KIND(1D0)), INTENT(in) :: azimuth !solar azimuth [°]
      REAL(KIND(1D0)), INTENT(in) :: chSnow_per_interval ! change state_id of snow and surface per time interval [mm]
      REAL(KIND(1D0)), INTENT(in) :: dectime !decimal time [-]
      REAL(KIND(1D0)), INTENT(in) :: drain_per_tstep ! total dr   ainage at each timestep [mm]
      REAL(KIND(1D0)), INTENT(in) :: E_mod
      REAL(KIND(1D0)), INTENT(in) :: ev_per_tstep ! evaporation at each time step [mm]
      REAL(KIND(1D0)), INTENT(in) :: ext_wu !external water use
      REAL(KIND(1D0)), INTENT(in) :: Fc !co2 emission [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: Fc_build ! co2 emission from building component [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: Fc_metab ! co2 emission from metabolism component [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: Fc_photo !co2 flux from photosynthesis [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: Fc_respi !co2 flux from respiration [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: Fc_point ! co2 emission from point source [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: Fc_traff !co2 flux from traffic [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(in) :: fcld !cloud fraction [-]
      REAL(KIND(1D0)), INTENT(in) :: FlowChange !Difference between the input and output flow in the water body [mm]
      REAL(KIND(1D0)), INTENT(in) :: h_mod !volumetric air heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: int_wu !internal water use [mm]
      REAL(KIND(1D0)), INTENT(in) :: kup !outgoing shortwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: l_mod !Obukhov length [m]
      REAL(KIND(1D0)), INTENT(in) :: LAI_id(nvegsurf) !leaf area index [m2 m-2]
      REAL(KIND(1D0)), INTENT(in) :: ldown !incoming longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: lup !outgoing longwave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: mwh !snowmelt [mm]
      REAL(KIND(1D0)), INTENT(in) :: MwStore !overall met water [mm]
      REAL(KIND(1D0)), INTENT(in) :: nsh_real !timestep in a hour [-]
      REAL(KIND(1D0)), INTENT(in) :: NWstate_per_tstep ! state_id at each tinestep(excluding water body) [mm]
      REAL(KIND(1D0)), INTENT(in) :: Precip !rain data [mm]
      REAL(KIND(1D0)), INTENT(in) :: q2_gkg ! Air specific humidity at 2 m [g kg-1]
      REAL(KIND(1D0)), INTENT(in) :: qeOut !latent heat flux [W -2]
      REAL(KIND(1D0)), INTENT(in) :: qf !anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qh !turbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qh_resist ! resistance-based turbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: Qm !snowmelt-related heat [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: QmFreez !heat related to freezing of surface store
      REAL(KIND(1D0)), INTENT(in) :: QmRain !melt heat for rain on snow [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qn !net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qn_snow !net all-wave radiation on snow surface [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qn_snowfree !net all-wave radiation on snow-free surface [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qs !heat storage flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: RA !aerodynamic resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: resistsurf !surface resistance [s m-1]
      REAL(KIND(1D0)), INTENT(in) :: RH2 ! air relative humidity at 2m [-]
      REAL(KIND(1D0)), INTENT(in) :: runoff_per_tstep !runoff water at each time step [mm]
      REAL(KIND(1D0)), INTENT(in) :: runoffAGimpervious !Above ground runoff from impervious surface for all surface area [mm]
      REAL(KIND(1D0)), INTENT(in) :: runoffAGveg !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)), INTENT(in) :: runoffPipes !runoff to pipes [mm]
      REAL(KIND(1D0)), INTENT(in) :: runoffSoil_per_tstep !Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
      REAL(KIND(1D0)), INTENT(in) :: runoffWaterBody !Above ground runoff from water body for all surface area [mm]
      REAL(KIND(1D0)), INTENT(in) :: sfr_surf(nsurf) !surface fraction [-]
      REAL(KIND(1D0)), INTENT(in) :: smd !soil moisture deficit [mm]
      REAL(KIND(1D0)), INTENT(in) :: smd_nsurf(nsurf) !smd for each surface [mm]
      REAL(KIND(1D0)), INTENT(in) :: SnowAlb !snow alebdo [-]
      REAL(KIND(1D0)), INTENT(in) :: SnowRemoval(2) !snow removal [mm]
      REAL(KIND(1D0)), INTENT(in) :: state_id(nsurf) ! wetness status of each surface type [mm]
      REAL(KIND(1D0)), INTENT(in) :: state_per_tstep !state_id at each timestep [mm]
      REAL(KIND(1D0)), INTENT(in) :: surf_chang_per_tstep !change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)), INTENT(in) :: swe !overall snow water equavalent[mm]
      REAL(KIND(1D0)), INTENT(in) :: t2_C !modelled 2 meter air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: tskin_C ! skin temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: tot_chang_per_tstep !Change in surface state_id [mm]
      REAL(KIND(1D0)), INTENT(in) :: tsurf !surface temperatue [degC]
      REAL(KIND(1D0)), INTENT(in) :: UStar !friction velocity [m s-1]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: wu_nsurf !water use of each surfaces [mm]

      REAL(KIND(1D0)), INTENT(in) :: z0m !Aerodynamic roughness length [m]
      REAL(KIND(1D0)), INTENT(in) :: zdm !zero-plane displacement [m]
      REAL(KIND(1D0)), INTENT(in) :: zenith_deg !solar zenith angle in degree [°]

      REAL(KIND(1D0)), DIMENSION(5), INTENT(OUT) :: datetimeLine !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5), INTENT(out) :: dataOutLineSUEWS
      ! REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutSnow-5),INTENT(out) :: dataOutLineSnow
      ! REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5),INTENT(out) :: dataOutLineESTM
      ! INTEGER:: is
      REAL(KIND(1D0)) :: LAI_wt !area weighted LAI [m2 m-2]
      REAL(KIND(1D0)) :: RH2_pct ! RH2 in percentage [-]

      ! the variables below with '_x' endings stand for 'exported' values
      REAL(KIND(1D0)) :: ResistSurf_x !output surface resistance [s m-1]
      REAL(KIND(1D0)) :: surf_chang_per_tstep_x !output change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)) :: l_mod_x !output  Obukhov length [m]
      REAL(KIND(1D0)) :: bulkalbedo !output area-weighted albedo [-]
      REAL(KIND(1D0)) :: smd_nsurf_x(nsurf) !output soil moisture deficit for each surface [mm]
      REAL(KIND(1D0)) :: state_x(nsurf) !output wetness status of each surfaces[mm]
      REAL(KIND(1D0)) :: wu_DecTr !water use for deciduous tree and shrubs [mm]
      REAL(KIND(1D0)) :: wu_EveTr !water use of evergreen tree and shrubs [mm]
      REAL(KIND(1D0)) :: wu_Grass !water use for grass [mm]

      !=====================================================================
      !====================== Prepare data for output ======================
      ! values outside of reasonable range are set as NAN-like numbers. TS 10 Jun 2018

      ! Remove non-existing surface type from surface and soil outputs   ! Added back in with NANs by HCW 24 Aug 2016
      state_x = UNPACK(SPREAD(NAN, dim=1, ncopies=SIZE(sfr_surf)), mask=(sfr_surf < 0.00001), field=state_id)
      smd_nsurf_x = UNPACK(SPREAD(NAN, dim=1, ncopies=SIZE(sfr_surf)), mask=(sfr_surf < 0.00001), field=smd_nsurf)

      ResistSurf_x = MIN(9999., ResistSurf)

      surf_chang_per_tstep_x = MERGE(surf_chang_per_tstep, 0.D0, ABS(surf_chang_per_tstep) > 1E-6)

      l_mod_x = MAX(MIN(9999., l_mod), -9999.)

      ! Calculate areally-weighted LAI
      ! IF(iy == (iy_prev_t  +1) .AND. (id-1) == 0) THEN   !Check for start of next year and avoid using LAI(id-1) as this is at the start of the year
      !    LAI_wt=DOT_PRODUCT(LAI(id_prev_t,:),sfr_surf(1+2:nvegsurf+2))
      ! ELSE
      !    LAI_wt=DOT_PRODUCT(LAI(id-1,:),sfr_surf(1+2:nvegsurf+2))
      ! ENDIF

      LAI_wt = DOT_PRODUCT(LAI_id(:), sfr_surf(1 + 2:nvegsurf + 2))

      ! Calculate areally-weighted albedo
      bulkalbedo = DOT_PRODUCT(alb, sfr_surf)

      ! convert RH2 to a percentage form
      RH2_pct = RH2*100.0

      ! translate water use to vegetated surfaces
      wu_DecTr = wu_nsurf(3)
      wu_EveTr = wu_nsurf(4)
      wu_Grass = wu_nsurf(5)

      !====================== update output line ==============================
      ! date & time:
      datetimeLine = [ &
                     REAL(iy, KIND(1D0)), REAL(id, KIND(1D0)), &
                     REAL(it, KIND(1D0)), REAL(imin, KIND(1D0)), dectime]
      !Define the overall output matrix to be printed out step by step
      dataOutLineSUEWS = [ &
                         avkdn, kup, ldown, lup, tsurf, &
                         qn, qf, qs, qh, qeOut, &
                         h_mod, e_mod, qh_resist, &
                         precip, ext_wu, ev_per_tstep, runoff_per_tstep, tot_chang_per_tstep, &
                         surf_chang_per_tstep_x, state_per_tstep, NWstate_per_tstep, drain_per_tstep, smd, &
                         FlowChange/nsh_real, AdditionalWater, &
                         runoffSoil_per_tstep, runoffPipes, runoffAGimpervious, runoffAGveg, runoffWaterBody, &
                         int_wu, wu_EveTr, wu_DecTr, wu_Grass, &
                         smd_nsurf_x(1:nsurf - 1), &
                         state_x(1:nsurf), &
                         zenith_deg, azimuth, bulkalbedo, Fcld, &
                         LAI_wt, z0m, zdm, &
                         UStar, l_mod, RA, ResistSurf, &
                         Fc, &
                         Fc_photo, Fc_respi, Fc_metab, Fc_traff, Fc_build, Fc_point, &
                         qn_snowfree, qn_snow, SnowAlb, &
                         Qm, QmFreez, QmRain, swe, mwh, MwStore, chSnow_per_interval, &
                         SnowRemoval(1:2), &
                         tskin_C, t2_C, q2_gkg, avU10_ms, RH2_pct & ! surface-level diagonostics
                         ]
      ! set invalid values to NAN
      ! dataOutLineSUEWS = set_nan(dataOutLineSUEWS)

      !====================update output line end==============================

   END SUBROUTINE SUEWS_update_outputLine
!========================================================================

!==============Update output arrays=========================
   SUBROUTINE ESTMExt_update_outputLine( &
      iy, id, it, imin, dectime, nlayer, & !input
      tsfc_out_surf, qs_surf, &
      tsfc_out_roof, &
      Qn_roof, &
      QS_roof, &
      QE_roof, &
      QH_roof, &
      state_roof, &
      soilstore_roof, &
      tsfc_out_wall, &
      Qn_wall, &
      QS_wall, &
      QE_wall, &
      QH_wall, &
      state_wall, &
      soilstore_wall, &
      datetimeLine, dataOutLineESTMExt) !output
      IMPLICIT NONE

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER, PARAMETER :: n_fill = 15

      INTEGER, INTENT(in) :: iy ! year [YYYY]
      INTEGER, INTENT(in) :: id ! day of the year [DOY]
      INTEGER, INTENT(in) :: it ! hour [H]
      INTEGER, INTENT(in) :: imin ! minutes [M]

      INTEGER, INTENT(in) :: nlayer ! number of vertical levels in urban canopy [-]
      REAL(KIND(1D0)), INTENT(in) :: dectime !decimal time [-]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_out_surf !surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qs_surf !heat storage flux of each surface type [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_out_roof !roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: Qn_roof !net all-wave radiation of the roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QS_roof !heat storage flux of the roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QE_roof !latent heat flux of the roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QH_roof !sensible heat flux of the roof [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_roof !wetness state of the roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_roof !soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_out_wall !wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: Qn_wall !net all-wave radiation of the wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QS_wall !heat storage flux of the wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QE_wall !latent heat flux of the wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QH_wall !sensible heat flux of the wall [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_wall !wetness state of the wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_wall !soil moisture of wall [mm]

      REAL(KIND(1D0)), DIMENSION(5), INTENT(OUT) :: datetimeLine !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5), INTENT(out) :: dataOutLineESTMExt
      ! REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutSnow-5),INTENT(out) :: dataOutLineSnow
      ! REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5),INTENT(out) :: dataOutLineESTM
      ! INTEGER:: is
      REAL(KIND(1D0)) :: LAI_wt !area weighted LAI [m2 m-2]
      REAL(KIND(1D0)) :: RH2_pct ! RH2 in percentage [-]

      ! the variables below with '_x' endings stand for 'exported' values
      REAL(KIND(1D0)) :: ResistSurf_x !output surface resistance [s m-1]
      REAL(KIND(1D0)) :: surf_chang_per_tstep_x !output change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)) :: l_mod_x !output  Obukhov length [m]
      REAL(KIND(1D0)) :: bulkalbedo !output area-weighted albedo [-]
      REAL(KIND(1D0)) :: smd_nsurf_x(nsurf) !output soil moisture deficit for each surface [mm]
      REAL(KIND(1D0)) :: state_x(nsurf) !output wetness status of each surfaces[mm]
      REAL(KIND(1D0)) :: wu_DecTr !water use for deciduous tree and shrubs [mm]
      REAL(KIND(1D0)) :: wu_EveTr !water use of evergreen tree and shrubs [mm]
      REAL(KIND(1D0)) :: wu_Grass !water use for grass [mm]

      ! date & time:
      datetimeLine = [ &
                     REAL(iy, KIND(1D0)), REAL(id, KIND(1D0)), &
                     REAL(it, KIND(1D0)), REAL(imin, KIND(1D0)), dectime]
      !Define the overall output matrix to be printed out step by step
      dataoutlineESTMExt = [ &
                           tsfc_out_surf, qs_surf, & !output
                           fill_result_x(tsfc_out_roof, n_fill), &
                           fill_result_x(Qn_roof, n_fill), &
                           fill_result_x(QS_roof, n_fill), &
                           fill_result_x(QE_roof, n_fill), &
                           fill_result_x(QH_roof, n_fill), &
                           fill_result_x(state_roof, n_fill), &
                           fill_result_x(soilstore_roof, n_fill), &
                           fill_result_x(tsfc_out_wall, n_fill), &
                           fill_result_x(Qn_wall, n_fill), &
                           fill_result_x(QS_wall, n_fill), &
                           fill_result_x(QE_wall, n_fill), &
                           fill_result_x(QH_wall, n_fill), &
                           fill_result_x(state_wall, n_fill), &
                           fill_result_x(soilstore_wall, n_fill) &
                           ]
      ! set invalid values to NAN
      ! dataOutLineSUEWS = set_nan(dataOutLineSUEWS)

      !====================update output line end==============================

   END SUBROUTINE ESTMExt_update_outputLine
!========================================================================

   FUNCTION fill_result_x(res_valid, n_fill) RESULT(res_filled)
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: res_valid
      INTEGER, INTENT(IN) :: n_fill
      REAL(KIND(1D0)), DIMENSION(n_fill) :: res_filled

      REAL(KIND(1D0)), PARAMETER :: NAN = -999

      res_filled = RESHAPE(res_valid, [n_fill], pad=[NAN])
   END FUNCTION fill_result_x

!==============Update output arrays=========================
   SUBROUTINE SUEWS_update_output( &
      SnowUse, storageheatmethod, & !input
      ReadLinesMetdata, NumberOfGrids, &
      ir, gridiv, &
      dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, dataOutLineBEERS, &
      dataoutlineDebug, dataoutlineSPARTACUS, dataOutLineESTMExt, & !input
      dataOutSUEWS, dataOutSnow, dataOutESTM, dataOutRSL, dataOutBEERS, dataOutDebug, dataOutSPARTACUS, &
      dataOutESTMExt) !inout
      IMPLICIT NONE

      INTEGER, INTENT(in) :: ReadLinesMetdata
      INTEGER, INTENT(in) :: NumberOfGrids
      INTEGER, INTENT(in) :: Gridiv
      INTEGER, INTENT(in) :: SnowUse
      INTEGER, INTENT(in) :: storageheatmethod
      INTEGER, INTENT(in) :: ir

      ! REAL(KIND(1D0)), DIMENSION(5), INTENT(in) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS), INTENT(in) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM), INTENT(in) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt), INTENT(in) :: dataOutLineESTMExt
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow), INTENT(in) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL), INTENT(in) :: dataoutLineRSL
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutBEERS), INTENT(in) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutDebug), INTENT(in) :: dataOutLineDebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutSPARTACUS), INTENT(in) :: dataOutLineSPARTACUS

      REAL(KIND(1D0)), INTENT(inout) :: dataOutSUEWS(ReadLinesMetdata, ncolumnsDataOutSUEWS, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutSnow(ReadLinesMetdata, ncolumnsDataOutSnow, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutESTM(ReadLinesMetdata, ncolumnsDataOutESTM, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutESTMExt(ReadLinesMetdata, ncolumnsDataOutESTMExt, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutRSL(ReadLinesMetdata, ncolumnsDataOutRSL, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutBEERS(ReadLinesMetdata, ncolumnsdataOutBEERS, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutDebug(ReadLinesMetdata, ncolumnsDataOutDebug, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutSPARTACUS(ReadLinesMetdata, ncolumnsDataOutSPARTACUS, NumberOfGrids)

      !====================== update output arrays ==============================
      !Define the overall output matrix to be printed out step by step
      dataOutSUEWS(ir, 1:ncolumnsDataOutSUEWS, Gridiv) = [(dataOutLineSUEWS)]
      ! dataOutSUEWS(ir, 1:ncolumnsDataOutSUEWS, Gridiv) = [ set_nan(dataOutLineSUEWS)]
      dataOutRSL(ir, 1:ncolumnsDataOutRSL, Gridiv) = [(dataoutLineRSL)]
      dataOutDebug(ir, 1:ncolumnsDataOutDebug, Gridiv) = [(dataOutLineDebug)]
      dataOutSPARTACUS(ir, 1:ncolumnsDataOutSPARTACUS, Gridiv) = [(dataOutLineSPARTACUS)]
      ! dataOutRSL(ir, 1:ncolumnsDataOutRSL, Gridiv) = [ set_nan(dataoutLineRSL)]
      dataOutBEERS(ir, 1:ncolumnsdataOutBEERS, Gridiv) = [set_nan(dataOutLineBEERS)]
      ! ! set invalid values to NAN
      ! dataOutSUEWS(ir,6:ncolumnsDataOutSUEWS,Gridiv)=set_nan(dataOutSUEWS(ir,6:ncolumnsDataOutSUEWS,Gridiv))

      IF (SnowUse == 1) THEN
         dataOutSnow(ir, 1:ncolumnsDataOutSnow, Gridiv) = [set_nan(dataOutLineSnow)]
      END IF

      IF (storageheatmethod == 4) THEN
         dataOutESTM(ir, 1:ncolumnsDataOutESTM, Gridiv) = [set_nan(dataOutLineESTM)]
      END IF

      IF (storageheatmethod == 5) THEN
         dataOutESTMExt(ir, 1:ncolumnsDataOutESTMExt, Gridiv) = [set_nan(dataOutLineESTMExt)]
      END IF

      !====================update output arrays end==============================

   END SUBROUTINE SUEWS_update_output

! !========================================================================
! SUBROUTINE SUEWS_cal_Diagnostics( &
!    dectime, &!input
!    avU1, Temp_C, avRH, Press_hPa, &
!    qh, qe, &
!    VegFraction, zMeas, z0m, zdm, RA, avdens, avcp, lv_J_kg, tstep_real, &
!    RoughLenHeatMethod, StabilityMethod, &
!    avU10_ms, t2_C, q2_gkg, tskin_C, RH2)!output
!    ! TS 03 Aug 2018: added limit on q2 by restricting RH2_max to 100%
!    ! TS 31 Jul 2018: removed dependence on surface variables (Tsurf, qsat)
!    ! TS 26 Jul 2018: improved the calculation logic
!    ! TS 05 Sep 2017: improved interface
!    ! TS 20 May 2017: calculate surface-level diagonostics
!    IMPLICIT NONE
!    REAL(KIND(1d0)), INTENT(in) ::dectime
!    REAL(KIND(1d0)), INTENT(in) ::avU1, Temp_C, avRH
!    REAL(KIND(1d0)), INTENT(in) ::qh
!    REAL(KIND(1d0)), INTENT(in) ::Press_hPa, qe
!    REAL(KIND(1d0)), INTENT(in) :: VegFraction, z0m, RA, avdens, avcp, lv_J_kg, tstep_real
!    REAL(KIND(1d0)), INTENT(in) :: zMeas! height for measurement
!    REAL(KIND(1d0)), INTENT(in) :: zdm ! displacement height

!    ! INTEGER,INTENT(in)         :: opt ! 0 for momentum, 1 for temperature, 2 for humidity
!    INTEGER, INTENT(in)         :: RoughLenHeatMethod, StabilityMethod

!    REAL(KIND(1d0)), INTENT(out):: avU10_ms, t2_C, q2_gkg, tskin_C, RH2
!    REAL(KIND(1d0))::qa_gkg
!    REAL(KIND(1d0)), PARAMETER::k = 0.4

!    ! wind speed:
!    CALL diagSfc( &
!       0, &
!       zMeas, avU1, 0d0, 10d0, avU10_ms, &
!       VegFraction, &
!       z0m, zdm, avdens, avcp, lv_J_kg, &
!       avU1, Temp_C, qh, &
!       RoughLenHeatMethod, StabilityMethod, tstep_real, dectime)

!    ! temperature at 2 m agl:
!    CALL diagSfc( &
!       1, &
!       zMeas, Temp_C, qh, 2d0, t2_C, &
!       VegFraction, &
!       z0m, zdm, avdens, avcp, lv_J_kg, &
!       avU1, Temp_C, qh, &
!       RoughLenHeatMethod, StabilityMethod, tstep_real, dectime)

!    ! skin temperature:
!    tskin_C = qh/(avdens*avcp)*RA + temp_C

!    ! humidity:
!    qa_gkg = RH2qa(avRH/100, Press_hPa, Temp_c)
!    CALL diagSfc( &
!       2, &
!       zMeas, qa_gkg, qe, 2d0, q2_gkg, &
!       VegFraction, &
!       z0m, zdm, avdens, avcp, lv_J_kg, &
!       avU1, Temp_C, qh, &
!       RoughLenHeatMethod, StabilityMethod, tstep_real, dectime)
!    ! re-examine if the diagnostic RH2 > 100% ?
!    RH2 = qa2RH(q2_gkg, Press_hPa, Temp_c)
!    IF (RH2 > 1) THEN
!       ! if so, limit RH2 to 100%
!       RH2 = 1d0
!       ! and adjust the diagnostic q2_gkg
!       q2_gkg = RH2qa(RH2, Press_hPa, Temp_c)
!    END IF

! END SUBROUTINE SUEWS_cal_Diagnostics

! calculate several surface fraction related parameters
   SUBROUTINE SUEWS_cal_surf( &
      StorageHeatMethod, NetRadiationMethod, & !input
      nlayer, sfr_surf, & !input
      building_frac, building_scale, height, & !input
      vegfraction, ImpervFraction, PervFraction, NonWaterFraction, & ! output
      sfr_roof, sfr_wall) ! output
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: StorageHeatMethod ! method for storage heat calculations [-]
      INTEGER, INTENT(IN) :: NetRadiationMethod ! method for net radiation calculations [-]
      INTEGER, INTENT(IN) :: nlayer !number of vertical layers[-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf !surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac !cumulative surface fraction of buildings across vertical layers [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale !building scales of each vertical layer  [m]
      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height !building height of each layer[-]
      REAL(KIND(1D0)), INTENT(OUT) :: VegFraction ! fraction of vegetation [-]
      REAL(KIND(1D0)), INTENT(OUT) :: ImpervFraction !fractioin of impervious surface [-]
      REAL(KIND(1D0)), INTENT(OUT) :: PervFraction !fraction of pervious surfaces [-]
      REAL(KIND(1D0)), INTENT(OUT) :: NonWaterFraction !fraction of non-water [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: sfr_roof !fraction of roof facets [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: sfr_wall !fraction of wall facets [-]

      ! REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_roof ! individual building fraction at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer) :: dz_ind ! individual net building height at each layer
      ! REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_wall ! individual net building height at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer) :: perimeter_ind ! individual building perimeter at each layer

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

   END SUBROUTINE SUEWS_cal_surf

   SUBROUTINE SUEWS_cal_surf_DTS( &
      StorageHeatMethod, NetRadiationMethod, & !input
      nlayer, &
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, & !input
      building_frac, building_scale, height, & !input
      vegfraction, ImpervFraction, PervFraction, NonWaterFraction, & ! output
      sfr_roof, sfr_wall) ! output
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: StorageHeatMethod ! method for storage heat calculations [-]
      INTEGER, INTENT(IN) :: NetRadiationMethod ! method for net radiation calculations [-]
      INTEGER, INTENT(IN) :: nlayer !number of vertical layers[-]

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]

      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac !cumulative surface fraction of buildings across vertical layers [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale !building scales of each vertical layer  [m]
      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height !building height of each layer[-]
      REAL(KIND(1D0)), INTENT(OUT) :: VegFraction ! fraction of vegetation [-]
      REAL(KIND(1D0)), INTENT(OUT) :: ImpervFraction !fractioin of impervious surface [-]
      REAL(KIND(1D0)), INTENT(OUT) :: PervFraction !fraction of pervious surfaces [-]
      REAL(KIND(1D0)), INTENT(OUT) :: NonWaterFraction !fraction of non-water [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: sfr_roof !fraction of roof facets [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: sfr_wall !fraction of wall facets [-]

      ! REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_roof ! individual building fraction at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer) :: dz_ind ! individual net building height at each layer
      ! REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_wall ! individual net building height at each layer
      REAL(KIND(1D0)), DIMENSION(nlayer) :: perimeter_ind ! individual building perimeter at each layer

      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]

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

   END SUBROUTINE SUEWS_cal_surf_DTS

! SUBROUTINE diagSfc( &
!    opt, &
!    zMeas, xMeas, xFlux, zDiag, xDiag, &
!    VegFraction, &
!    z0m, zd, avdens, avcp, lv_J_kg, &
!    avU1, Temp_C, qh, &
!    RoughLenHeatMethod, StabilityMethod, tstep_real, dectime)
!    ! TS 31 Jul 2018: removed dependence on surface variables (Tsurf, qsat)
!    ! TS 26 Jul 2018: improved the calculation logic
!    ! TS 05 Sep 2017: improved interface
!    ! TS 20 May 2017: calculate surface-level diagonostics

!    IMPLICIT NONE
!    REAL(KIND(1d0)), INTENT(in) :: dectime
!    REAL(KIND(1d0)), INTENT(in) :: qh ! sensible heat flux
!    REAL(KIND(1d0)), INTENT(in) :: z0m, avdens, avcp, lv_J_kg, tstep_real
!    REAL(KIND(1d0)), INTENT(in) :: avU1, Temp_C ! atmospheric level variables
!    REAL(KIND(1d0)), INTENT(in) :: zDiag ! height for diagonostics
!    REAL(KIND(1d0)), INTENT(in) :: zMeas! height for measurement
!    REAL(KIND(1d0)), INTENT(in) :: zd ! displacement height
!    REAL(KIND(1d0)), INTENT(in) :: xMeas ! measurement at height
!    REAL(KIND(1d0)), INTENT(in) :: xFlux!
!    REAL(KIND(1d0)), INTENT(in) :: VegFraction ! vegetation fraction

!    INTEGER, INTENT(in)         :: opt ! 0 for momentum, 1 for temperature, 2 for humidity
!    INTEGER, INTENT(in)         :: RoughLenHeatMethod, StabilityMethod

!    REAL(KIND(1d0)), INTENT(out):: xDiag

!    REAL(KIND(1d0)) :: L_mod
!    REAL(KIND(1d0)) :: psimz0, psihzDiag, psihzMeas, psihz0, psimzDiag ! stability correction functions
!    REAL(KIND(1d0)) :: z0h ! Roughness length for heat
!    REAL(KIND(1d0)) :: zDiagzd! height for diagnositcs
!    REAL(KIND(1d0)) :: zMeaszd
!    REAL(KIND(1d0)) :: tlv, H_kms, TStar, zL, UStar
!    REAL(KIND(1d0)), PARAMETER :: muu = 1.46e-5 !molecular viscosity
!    REAL(KIND(1d0)), PARAMETER :: nan = -999
!    REAL(KIND(1d0)), PARAMETER :: zdm = 0 ! assuming Displacement height is ZERO
!    REAL(KIND(1d0)), PARAMETER::k = 0.4

!    tlv = lv_J_kg/tstep_real !Latent heat of vapourisation per timestep
!    zDiagzd = zDiag + z0m ! height at hgtX assuming Displacement height is ZERO; set lower limit as z0 to prevent arithmetic error, zd=0

!    ! get !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity
!    CALL SUEWS_init_QH( &
!       avdens, avcp, qh, 0d0, dectime, & ! use qh as qh_obs to initialise H_init
!       H_kms)!output

!    ! redo the calculation for stability correction
!    CALL cal_Stab( &
!       ! input
!       StabilityMethod, &
!       dectime, & !Decimal time
!       zDiagzd, &     !Active measurement height (meas. height-displac. height)
!       z0m, &     !Aerodynamic roughness length
!       zdm, &     !Displacement height
!       avU1, &    !Average wind speed
!       Temp_C, &  !Air temperature
!       H_kms, & !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity
!       ! output:
!       L_MOD, & !Obukhov length
!       TStar, & !T*
!       UStar, & !Friction velocity
!       zL)!Stability scale

!    !***************************************************************
!    ! log-law based stability corrections:
!    ! Roughness length for heat
!    z0h = cal_z0V(RoughLenHeatMethod, z0m, VegFraction, UStar)

!    ! stability correction functions
!    ! momentum:
!    psimzDiag = stab_psi_mom(StabilityMethod, zDiagzd/L_mod)
!    ! psimz2=stab_fn_mom(StabilityMethod,z2zd/L_mod,z2zd/L_mod)
!    psimz0 = stab_psi_mom(StabilityMethod, z0m/L_mod)

!    ! heat and vapor: assuming both are the same
!    ! psihz2=stab_fn_heat(StabilityMethod,z2zd/L_mod,z2zd/L_mod)
!    psihz0 = stab_psi_heat(StabilityMethod, z0h/L_mod)

!    !***************************************************************
!    SELECT CASE (opt)
!    CASE (0) ! wind (momentum) at hgtX=10 m
!       zDiagzd = zDiag + z0m! set lower limit as z0h to prevent arithmetic error, zd=0

!       ! stability correction functions
!       ! momentum:
!       psimzDiag = stab_psi_mom(StabilityMethod, zDiagzd/L_mod)
!       psimz0 = stab_psi_mom(StabilityMethod, z0m/L_mod)
!       xDiag = UStar/k*(LOG(zDiagzd/z0m) - psimzDiag + psimz0) ! Brutsaert (2005), p51, eq.2.54

!    CASE (1) ! temperature at hgtX=2 m
!       zMeaszd = zMeas - zd
!       zDiagzd = zDiag + z0h! set lower limit as z0h to prevent arithmetic error, zd=0

!       ! heat and vapor: assuming both are the same
!       psihzMeas = stab_psi_heat(StabilityMethod, zMeaszd/L_mod)
!       psihzDiag = stab_psi_heat(StabilityMethod, zDiagzd/L_mod)
!       ! psihz0=stab_fn_heat(StabilityMethod,z0h/L_mod,z0h/L_mod)
!       xDiag = xMeas + xFlux/(k*UStar*avdens*avcp)*(LOG(zMeaszd/zDiagzd) - (psihzMeas - psihzDiag)) ! Brutsaert (2005), p51, eq.2.55
!       !  IF ( ABS((LOG(z2zd/z0h)-psihz2+psihz0))>10 ) THEN
!       !     PRINT*, '#####################################'
!       !     PRINT*, 'xSurf',xSurf
!       !     PRINT*, 'xFlux',xFlux
!       !     PRINT*, 'k*us*avdens*avcp',k*us*avdens*avcp
!       !     PRINT*, 'k',k
!       !     PRINT*, 'us',us
!       !     PRINT*, 'avdens',avdens
!       !     PRINT*, 'avcp',avcp
!       !     PRINT*, 'xFlux/X',xFlux/(k*us*avdens*avcp)
!       !     PRINT*, 'stab',(LOG(z2zd/z0h)-psihz2+psihz0)
!       !     PRINT*, 'LOG(z2zd/z0h)',LOG(z2zd/z0h)
!       !     PRINT*, 'z2zd',z2zd,'L_mod',L_mod,'z0h',z0h
!       !     PRINT*, 'z2zd/L_mod',z2zd/L_mod
!       !     PRINT*, 'psihz2',psihz2
!       !     PRINT*, 'psihz0',psihz0
!       !     PRINT*, 'psihz2-psihz0',psihz2-psihz0
!       !     PRINT*, 'xDiag',xDiag
!       !     PRINT*, '*************************************'
!       !  END IF

!    CASE (2) ! humidity at hgtX=2 m
!       zMeaszd = zMeas - zd
!       zDiagzd = zDiag + z0h! set lower limit as z0h to prevent arithmetic error, zd=0

!       ! heat and vapor: assuming both are the same
!       psihzMeas = stab_psi_heat(StabilityMethod, zMeaszd/L_mod)
!       psihzDiag = stab_psi_heat(StabilityMethod, zDiagzd/L_mod)
!       ! psihz0=stab_fn_heat(StabilityMethod,z0h/L_mod,z0h/L_mod)

!       xDiag = xMeas + xFlux/(k*UStar*avdens*tlv)*(LOG(zMeaszd/zDiagzd) - (psihzMeas - psihzDiag)) ! Brutsaert (2005), p51, eq.2.56

!    END SELECT

! END SUBROUTINE diagSfc

!===============set variable of invalid value to NAN=====================
   ELEMENTAL FUNCTION set_nan(x) RESULT(xx)
      IMPLICIT NONE
      REAL(KIND(1D0)), PARAMETER :: pNAN = 30000 ! 30000 to prevent water_state being filtered out as it can be large
      REAL(KIND(1D0)), PARAMETER :: pZERO = 1E-8 ! to prevent inconsistency caused by positive or negative zero
      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      REAL(KIND(1D0)), INTENT(in) :: x
      REAL(KIND(1D0)) :: xx

      IF (ABS(x) > pNAN) THEN
         xx = NAN
      ELSEIF (ABS(x) < pZERO) THEN
         xx = 0
      ELSE
         xx = x
      END IF

   END FUNCTION set_nan
!========================================================================

!===============the functions below are only for test in f2py conversion===
   FUNCTION square(x) RESULT(xx)
      IMPLICIT NONE
      REAL(KIND(1D0)), PARAMETER :: pNAN = 9999
      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      REAL(KIND(1D0)), INTENT(in) :: x
      REAL(KIND(1D0)) :: xx

      xx = x**2 + nan/pNAN
      xx = x**2

   END FUNCTION square

   FUNCTION square_real(x) RESULT(xx)
      IMPLICIT NONE
      REAL, PARAMETER :: pNAN = 9999
      REAL, PARAMETER :: NAN = -999
      REAL, INTENT(in) :: x
      REAL :: xx

      xx = x**2 + nan/pNAN
      xx = x**2

   END FUNCTION square_real

   SUBROUTINE output_name_n(i, name, group, aggreg, outlevel)
      ! used by f2py module  to handle output names
      IMPLICIT NONE
      ! the dimension is potentially incorrect,
      ! which should be consistent with that in output module
      INTEGER, INTENT(in) :: i
      CHARACTER(len=15), INTENT(out) :: name, group, aggreg
      INTEGER, INTENT(out) :: outlevel

      INTEGER :: nVar
      nVar = SIZE(varListAll, dim=1)
      IF (i < nVar .AND. i > 0) THEN
         name = TRIM(varListAll(i)%header)
         group = TRIM(varListAll(i)%group)
         aggreg = TRIM(varListAll(i)%aggreg)
         outlevel = varListAll(i)%level
      ELSE
         name = ''
         group = ''
         aggreg = ''
         outlevel = 0
      END IF

   END SUBROUTINE output_name_n

   SUBROUTINE output_size(nVar)
      ! used by f2py module  to get size of the output list
      IMPLICIT NONE
      ! the dimension is potentially incorrect,
      ! which should be consistent with that in output module
      INTEGER, INTENT(out) :: nVar

      nVar = SIZE(varListAll, dim=1)

   END SUBROUTINE output_size

   SUBROUTINE SUEWS_cal_multitsteps( &
      MetForcingBlock, len_sim, &
      AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, & ! input&inout in alphabetical order
      AH_SLOPE_Heating, &
      alb, AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
      AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
      alpha_bioCO2, alpha_enh_bioCO2, alt, BaseT, BaseTe, &
      beta_bioCO2, beta_enh_bioCO2, bldgH, CapMax_dec, CapMin_dec, &
      chAnOHM, CO2PointSource, cpAnOHM, CRWmax, CRWmin, DayWat, DayWatPer, &
      DecTreeH, DiagMethod, Diagnose, DRAINRT, &
      dt_since_start, dqndt, qn_av, dqnsdt, qn_s_av, &
      EF_umolCO2perJ, emis, EmissionsMethod, EnEF_v_Jkm, endDLS, EveTreeH, FAIBldg, &
      FAIDecTree, FAIEveTree, FAIMethod, Faut, FcEF_v_kgkm, FlowChange, &
      FrFossilFuel_Heat, FrFossilFuel_NonHeat, G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, GDD_id, &
      GDDFull, Gridiv, gsModel, H_maintain, HDD_id, HumActivity_24hr, &
      IceFrac, Ie_a, Ie_end, Ie_m, Ie_start, &
      InternalWaterUse_h, &
      IrrFracPaved, IrrFracBldgs, &
      IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
      IrrFracBSoil, IrrFracWater, &
      kkAnOHM, Kmax, LAI_id, LAIMax, LAIMin, &
      LAIPower, LAIType, lat, lng, MaxConductance, MaxFCMetab, MaxQFMetab, &
      SnowWater, MinFCMetab, MinQFMetab, min_res_bioCO2, &
      NARP_EMIS_SNOW, NARP_TRANS_SITE, NetRadiationMethod, &
      OHM_coef, OHMIncQF, OHM_threshSW, &
      OHM_threshWD, PipeCapacity, PopDensDaytime, &
      PopDensNighttime, PopProf_24hr, PorMax_dec, PorMin_dec, &
      PrecipLimit, PrecipLimitAlb, &
      QF0_BEU, Qf_A, Qf_B, Qf_C, &
      nlayer, &
      n_vegetation_region_urban, &
      n_stream_sw_urban, n_stream_lw_urban, &
      sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      veg_fsd_const, veg_contact_fraction_const, &
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, & !input
      height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
      alb_roof, emis_roof, alb_wall, emis_wall, &
      roof_albedo_dir_mult_fact, wall_specular_frac, &
      RadMeltFact, RAINCOVER, RainMaxRes, resp_a, resp_b, &
      RoughLenHeatMethod, RoughLenMomMethod, RunoffToWater, S1, S2, &
      SatHydraulicConduct, SDDFull, SDD_id, SMDMethod, SnowAlb, SnowAlbMax, &
      SnowAlbMin, SnowPackLimit, SnowDens, SnowDensMax, SnowDensMin, SnowfallCum, SnowFrac, &
      SnowLimBldg, SnowLimPaved, SnowPack, SnowProf_24hr, SnowMethod, SoilDepth, &
      StabilityMethod, startDLS, &
      soilstore_surf, SoilStoreCap_surf, state_surf, StateLimit_surf, WetThresh_surf, &
      soilstore_roof, SoilStoreCap_roof, state_roof, StateLimit_roof, WetThresh_roof, &
      soilstore_wall, SoilStoreCap_wall, state_wall, StateLimit_wall, WetThresh_wall, &
      StorageHeatMethod, StoreDrainPrm, SurfaceArea, Tair_av, tau_a, tau_f, tau_r, &
      BaseT_Cooling, BaseT_Heating, TempMeltFact, TH, &
      theta_bioCO2, timezone, TL, TrafficRate, TrafficUnits, &
      sfr_surf, &
      tsfc_roof, tsfc_wall, tsfc_surf, &
      temp_roof, temp_wall, temp_surf, &
      tin_roof, tin_wall, tin_surf, &
      k_wall, k_roof, k_surf, &
      cp_wall, cp_roof, cp_surf, &
      dz_wall, dz_roof, dz_surf, &
      Tmin_id, Tmax_id, lenday_id, &
      TraffProf_24hr, Ts5mindata_ir, tstep, tstep_prev, veg_type, &
      WaterDist, WaterUseMethod, &
      WUDay_id, DecidCap_id, albDecTr_id, albEveTr_id, albGrass_id, porosity_id, &
      WUProfA_24hr, WUProfM_24hr, Z, z0m_in, zdm_in, &
      output_block_suews) !output
      ! dataOutBlockSUEWS, dataOutBlockSnow, dataOutBlockESTM, dataOutBlockRSL, dataOutBlockBEERS, & !output
      ! dataOutBlockDebug, dataOutBlockSPARTACUS, dataOutBlockESTMExt, &
      ! DailyStateBlock)

      IMPLICIT NONE
      ! input:
      ! met forcing block
      REAL(KIND(1D0)), DIMENSION(len_sim, 24), INTENT(IN) :: MetForcingBlock
      INTEGER, INTENT(IN) :: len_sim
      ! input variables
      INTEGER, INTENT(IN) :: nlayer ! number of vertical layers in urban canyon
      ! INTEGER, INTENT(IN) :: AerodynamicResistanceMethod !method to calculate RA [-]
      ! INTEGER, INTENT(IN) :: Diagnose
      INTEGER, INTENT(INOUT) :: Diagnose
      ! INTEGER, INTENT(IN) :: DiagQN
      ! INTEGER, INTENT(IN) :: DiagQS
      INTEGER, INTENT(IN) :: startDLS !start of daylight saving  [DOY]
      INTEGER, INTENT(IN) :: endDLS !end of daylight saving [DOY]
      INTEGER, INTENT(IN) :: EmissionsMethod !method to calculate anthropogenic heat [-]
      INTEGER, INTENT(IN) :: Gridiv ! grid id [-]
      INTEGER, INTENT(IN) :: gsModel !choice of gs parameterisation (1 = Ja11, 2 = Wa16)
      INTEGER, INTENT(IN) :: Ie_end !ending time of water use [DOY]
      INTEGER, INTENT(IN) :: Ie_start !starting time of water use [DOY]
      ! INTEGER, INTENT(IN) :: LAICalcYes !boolean to determine if calculate LAI [-] (1 = yes, 0 = no) ! disabled for now 30 Apr 2023 and will be reactivated in future
      INTEGER, INTENT(in) :: DiagMethod !Defines how near surface diagnostics are calculated [-]
      INTEGER, INTENT(IN) :: NetRadiationMethod ! method for calculation of radiation fluxes [-]
      INTEGER, INTENT(IN) :: OHMIncQF !Determines whether the storage heat flux calculation uses Q* or ( Q* +QF) [-]
      INTEGER, INTENT(IN) :: RoughLenHeatMethod !method to calculate heat roughness length [-]
      INTEGER, INTENT(IN) :: RoughLenMomMethod !Determines how aerodynamic roughness length (z0m) and zero displacement height (zdm) are calculated [-]
      INTEGER, INTENT(IN) :: FAIMethod !Determines how FAI is calculated [-]
      INTEGER, INTENT(IN) :: SMDMethod !Determines method for calculating soil moisture deficit [-]
      INTEGER, INTENT(IN) :: SnowMethod !Determines whether the snow part of the model runs[-]
      INTEGER, INTENT(IN) :: StabilityMethod !method to calculate atmospheric stability [-]
      INTEGER, INTENT(IN) :: StorageHeatMethod !Determines method for calculating storage heat flux ΔQS [-]
      INTEGER, INTENT(IN) :: tstep !timestep [s]
      INTEGER, INTENT(IN) :: tstep_prev ! tstep size of the previous step
      ! dt_since_start is intentionally made as inout to keep naming consistency with the embedded subroutine
      INTEGER, INTENT(inout) :: dt_since_start ! time since simulation starts [s]
      INTEGER, INTENT(IN) :: veg_type !Defines how vegetation is calculated for LUMPS [-]
      INTEGER, INTENT(IN) :: WaterUseMethod !Defines how external water use is calculated[-]

      INTEGER, DIMENSION(NVEGSURF), INTENT(IN) :: LAIType !LAI calculation choice[-]

      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_DecTr !maximum albedo for deciduous tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_EveTr !maximum albedo for evergreen tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_Grass !maximum albedo for grass [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_DecTr !minimum albedo for deciduous tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_EveTr !minimum albedo for evergreen tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_Grass !minimum albedo for grass [-]
      REAL(KIND(1D0)), INTENT(IN) :: alt !solar altitude [deg]
      ! REAL(KIND(1D0)),INTENT(IN)::avkdn
      ! REAL(KIND(1D0)),INTENT(IN)::avRh
      ! REAL(KIND(1D0)),INTENT(IN)::avU1
      REAL(KIND(1D0)), INTENT(IN) :: bldgH !average building height [m]
      REAL(KIND(1D0)), INTENT(IN) :: CapMax_dec !maximum water storage capacity for upper surfaces (i.e. canopy)
      REAL(KIND(1D0)), INTENT(IN) :: CapMin_dec !minimum water storage capacity for upper surfaces (i.e. canopy)
      REAL(KIND(1D0)), INTENT(IN) :: CO2PointSource ! point source [kgC day-1]
      REAL(KIND(1D0)), INTENT(IN) :: CRWmax !maximum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(IN) :: CRWmin !minimum water holding capacity of snow [mm]
      REAL(KIND(1D0)), INTENT(IN) :: DecTreeH !average height of deciduous tree and shrub [-]
      REAL(KIND(1D0)), INTENT(IN) :: DRAINRT !Drainage rate of the water bucket [mm hr-1]
      REAL(KIND(1D0)), INTENT(IN) :: EF_umolCO2perJ !co2 emission factor [umol J-1]
      REAL(KIND(1D0)), INTENT(IN) :: EnEF_v_Jkm ! energy emission factor [J K m-1]
      REAL(KIND(1D0)), INTENT(IN) :: EveTreeH !height of evergreen tree [m]
      REAL(KIND(1D0)), INTENT(IN) :: FAIBldg ! frontal area index for buildings [-]
      REAL(KIND(1D0)), INTENT(IN) :: FAIDecTree ! frontal area index for deciduous tree [-]
      REAL(KIND(1D0)), INTENT(IN) :: FAIEveTree ! frontal area index for evergreen tree [-]
      REAL(KIND(1D0)), INTENT(IN) :: Faut !Fraction of irrigated area using automatic irrigation [-]
      ! REAL(KIND(1D0)),INTENT(IN)::fcld_obs
      REAL(KIND(1D0)), INTENT(IN) :: FlowChange !Difference between the input and output flow in the water body [mm]
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_Heat ! fraction of fossil fuel heat [-]
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_NonHeat ! fraction of fossil fuel non heat [-]
      REAL(KIND(1D0)), INTENT(IN) :: G_max !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: G_k !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: G_q_base !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: G_q_shape !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: G_t !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: G_sm !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(IN) :: H_maintain ! ponding water depth to maintain [mm]
      REAL(KIND(1D0)), INTENT(IN) :: InternalWaterUse_h !Internal water use [mm h-1
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracPaved !fraction of paved which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBldgs !fraction of buildings (e.g., green roofs) which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracEveTr !fraction of evergreen trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracDecTr !fraction of deciduous trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracGrass !fraction of grass which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBSoil !fraction of bare soil trees which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracWater !fraction of water which are irrigated [-]
      REAL(KIND(1D0)), INTENT(IN) :: Kmax !annual maximum hourly solar radiation [W m-2]
      ! REAL(KIND(1D0)),INTENT(IN)::LAI_obs
      REAL(KIND(1D0)), INTENT(IN) :: lat !latitude [deg]
      ! REAL(KIND(1D0)),INTENT(IN)::ldown_obs
      REAL(KIND(1D0)), INTENT(IN) :: lng !longitude [deg]
      REAL(KIND(1D0)), INTENT(IN) :: MaxFCMetab ! maximum FC metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(IN) :: MaxQFMetab ! maximum QF Metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: MinFCMetab ! minimum QF metabolism [umol m-2 s-1]
      REAL(KIND(1D0)), INTENT(IN) :: MinQFMetab ! minimum FC metabolism [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: NARP_EMIS_SNOW ! snow emissivity in NARP model [-]
      REAL(KIND(1D0)), INTENT(IN) :: NARP_TRANS_SITE !atmospheric transmissivity for NARP [-]
      REAL(KIND(1D0)), INTENT(IN) :: PipeCapacity !capacity of pipes to transfer water [mm]
      REAL(KIND(1D0)), INTENT(IN) :: PopDensNighttime ! nighttime population density [ha-1] (i.e. residents)
      REAL(KIND(1D0)), INTENT(IN) :: PorMax_dec !full leaf-on summertime value used only for DecTr
      REAL(KIND(1D0)), INTENT(IN) :: PorMin_dec !leaf-off wintertime value used only for DecTr
      ! REAL(KIND(1D0)),INTENT(IN)::Precip
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimit !rain data [mm]
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimitAlb !temperature limit when precipitation falls as snow [degC]
      ! REAL(KIND(1D0)),INTENT(IN)::Press_hPa
      ! REAL(KIND(1D0)),INTENT(IN)::qh_obs
      ! REAL(KIND(1D0)),INTENT(IN)::qn1_obs
      ! REAL(KIND(1D0)),INTENT(IN)::qs_obs
      ! REAL(KIND(1D0)),INTENT(IN)::qf_obs
      REAL(KIND(1D0)), INTENT(IN) :: RadMeltFact !hourly radiation melt factor of snow [mm W-1 h-1]
      REAL(KIND(1D0)), INTENT(IN) :: RAINCOVER !limit when surface totally covered with water for LUMPS [mm]
      REAL(KIND(1D0)), INTENT(IN) :: RainMaxRes !maximum water bucket reservoir [mm] Used for LUMPS surface wetness control.
      REAL(KIND(1D0)), INTENT(IN) :: RunoffToWater !fraction of above-ground runoff flowing to water surface during flooding [-]
      REAL(KIND(1D0)), INTENT(IN) :: S1 !a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)), INTENT(IN) :: S2 !a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMax !effective surface albedo (middle of the day value) for summertime [-]
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMin !effective surface albedo (middle of the day value) for wintertime (not including snow) [-]
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMax !maximum snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMin !fresh snow density [kg m-3]
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimBldg !Limit of the snow water equivalent for snow removal from building roofs [mm]
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimPaved !llimit of the snow water equivalent for snow removal from roads[mm]
      ! REAL(KIND(1D0)),INTENT(IN)::snowFrac_obs
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea !area of the grid [ha]
      REAL(KIND(1D0)), INTENT(IN) :: tau_a !time constant for snow albedo aging in cold snow [-]
      REAL(KIND(1D0)), INTENT(IN) :: tau_f !time constant for snow albedo aging in melting snow [-]
      REAL(KIND(1D0)), INTENT(IN) :: tau_r !time constant for snow density ageing [-]
      ! REAL(KIND(1D0)),INTENT(IN)::Temp_C
      REAL(KIND(1D0)), INTENT(IN) :: TempMeltFact !hourly temperature melt factor of snow [mm K-1 h-1]
      REAL(KIND(1D0)), INTENT(IN) :: TH !upper air temperature limit [degC]
      REAL(KIND(1D0)), INTENT(IN) :: timezone !time zone [h] for site relative to UTC (east is positive)
      REAL(KIND(1D0)), INTENT(IN) :: TL !lower air temperature limit [degC]
      REAL(KIND(1D0)), INTENT(IN) :: TrafficUnits ! traffic units choice [-]
      ! REAL(KIND(1D0)),INTENT(IN)::xsmd
      REAL(KIND(1D0)), INTENT(IN) :: Z ! measurement height [m]
      REAL(KIND(1D0)), INTENT(IN) :: z0m_in !roughness length for momentum [m]
      REAL(KIND(1D0)), INTENT(IN) :: zdm_in !zero-plane displacement [m]

      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_MIN !minimum QF values [W m-2]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Cooling ! cooling slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Heating ! heating slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: FcEF_v_kgkm ! CO2 Emission factor [kg km-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: QF0_BEU ! Fraction of base value coming from buildings [-]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_A ! Base value for QF [W m-2]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_B ! Parameter related to heating degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_C ! Parameter related to cooling degree days [W m-2 K-1 (Cap ha-1 )-1]
      ! REAL(KIND(1D0)), DIMENSION(2), INTENT(IN)        ::Numcapita
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: PopDensDaytime ! Daytime population density [people ha-1] (i.e. workers)
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Cooling ! base temperature for cooling degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Heating ! base temperatrue for heating degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: TrafficRate ! Traffic rate [veh km m-2 s-1]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_a !Coefficient for automatic irrigation model
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_m !Coefficients for manual irrigation models
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: MaxConductance !the maximum conductance of each vegetation or surface type. [mm s-1]
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWat !Irrigation flag: 1 for on and 0 for off [-]
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWatPer !Fraction of properties using irrigation for each day of a week [-]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshSW !Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshWD !Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: chAnOHM !Bulk transfer coefficient for this surface to use in AnOHM [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: cpAnOHM !Volumetric heat capacity for this surface to use in AnOHM [J m-3]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: emis !Effective surface emissivity[-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: kkAnOHM !Thermal conductivity for this surface to use in AnOHM [W m K-1]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SatHydraulicConduct !Hydraulic conductivity for saturated soil [mm s-1]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf !surface cover fraction[-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SnowPackLimit !Limit for the snow water equivalent when snow cover starts to be patchy [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilDepth !Depth of soil beneath the surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: StateLimit_surf !Upper limit to the surface state [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: WetThresh_surf !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_bioCO2 !The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve [-]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_enh_bioCO2 !Part of the alpha coefficient related to the fraction of vegetation[-]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseT !Base Temperature for initiating growing degree days (GDD) for leaf growth [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseTe !Base temperature for initiating sensesance degree days (SDD) for leaf off [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_bioCO2 !The light-saturated gross photosynthesis of the canopy [umol m-2 s-1 ]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_enh_bioCO2 !Part of the beta coefficient related to the fraction of vegetation [umol m-2 s-1 ]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: GDDFull !the growing degree days (GDD) needed for full capacity of the leaf area index [degC]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMax !full leaf-on summertime value [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMin !leaf-off wintertime value [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: min_res_bioCO2 !Minimum soil respiration rate (for cold-temperature limit) [umol m-2 s-1]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_a !Respiration coefficient a
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_b !Respiration coefficient b - related to air temperature dependency
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: SDDFull !the sensesence degree days (SDD) needed to initiate leaf off [degC]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: SnowProf_24hr !Hourly profile values used in snow clearing [-]
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: theta_bioCO2 !The convexity of the curve at light saturation [-]
      REAL(KIND(1D0)), DIMENSION(4, NVEGSURF), INTENT(IN) :: LAIPower !parameters required by LAI calculation
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, 4, 3), INTENT(IN) :: OHM_coef !Coefficients for OHM calculation
      REAL(KIND(1D0)), DIMENSION(NSURF + 1, NSURF - 1), INTENT(IN) :: WaterDist !Fraction of water redistribution [-]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: Ts5mindata_ir !surface temperature input data[degC]

      ! diurnal profile values for 24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: AHProf_24hr !Hourly profile values used in energy use calculation [-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: HumActivity_24hr !Hourly profile values used in human activity calculation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: PopProf_24hr !Hourly profile values used in dynamic population estimation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: TraffProf_24hr !Hourly profile values used in traffic activity calculation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfA_24hr !Hourly profile values used in automatic irrigation[-]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfM_24hr !Hourly profile values used in manual irrigation[-]
      ! ########################################################################################

      ! ########################################################################################
      ! inout variables
      ! OHM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_av ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: dqndt ! rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_s_av ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: dqnsdt ! Rate of change of net radiation [W m-2 h-1]

      ! snow related:
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowfallCum !cumulated snow falling [mm]
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowAlb !albedo of know [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: IceFrac !fraction of ice in snowpack [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowWater ! snow water[mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowDens !snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowFrac !snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowPack !snow water equivalent on each land cover [mm]

      ! water balance related:
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: soilstore_surf !soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: state_surf !wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(6, NSURF), INTENT(INOUT) :: StoreDrainPrm !coefficients used in drainage calculation [-]

      ! phenology related:
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: alb !albedo [-]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: GDD_id !Growing Degree Days [degC](see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: SDD_id !Senescence Degree Days [degC](see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: LAI_id !LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: DecidCap_id !Moisture storage capacity of deciduous trees [mm]
      REAL(KIND(1D0)), INTENT(INOUT) :: albDecTr_id !Albedo of deciduous trees [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: albEveTr_id !Albedo of evergreen trees [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: albGrass_id !Albedo of grass  [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id !Porosity of deciduous trees [-]
      REAL(KIND(1D0)), INTENT(INOUT) :: Tmin_id !Daily minimum temperature [degC]
      REAL(KIND(1D0)), INTENT(INOUT) :: Tmax_id !Daily maximum temperature [degC]
      REAL(KIND(1D0)), INTENT(INOUT) :: lenday_id !daytime length [h]

      ! anthropogenic heat related:
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating Degree Days  [degC d](see SUEWS_DailyState.f95)

      ! water use related:
      REAL(KIND(1D0)), DIMENSION(9), INTENT(INOUT) :: WUDay_id !Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)

      ! ESTM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: Tair_av !average air temperature [degC]

      !  ! extended for ESTM_ehc, TS 20 Jan 2022
      ! input arrays: standard suews surfaces
      ! REAL(KIND(1D0)), DIMENSION(nroof) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_roof !roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_roof !roof surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof ! indoor temperature for roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(inout) :: temp_roof !interface temperature between depth layers in roof[degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof ! thermal conductivity of roof [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof ! Heat capacity of roof [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof ! thickness of each layer in roof [m]
      ! input arrays: standard suews surfaces
      ! REAL(KIND(1D0)), DIMENSION(nwall) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_wall !surface temperature of wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_wall !wall surface fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall ! indoor temperature for wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(inout) :: temp_wall !interface temperature between depth layers in wall[degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall ! thermal conductivity of wall [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall ! Heat capacity of wall [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall ! thickness of each layer in wall [m]
      ! input arrays: standard suews surfaces
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(INOUT) :: tsfc_surf !surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf !deep bottom temperature for each surface [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(inout) :: temp_surf !interface temperature between depth layers for each surfaces[degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf ! thermal conductivity of v [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf ! Heat capacity of each surface [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf ! thickness of each layer in each surface [m]

      ! SPARTACUS input variables
      INTEGER, INTENT(IN) :: n_vegetation_region_urban, & !Number of regions used to describe vegetation
                             n_stream_sw_urban, n_stream_lw_urban !shortwave diffuse streams per hemisphere; LW streams per hemisphere
      REAL(KIND(1D0)), INTENT(IN) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
                                     veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                                     veg_fsd_const, veg_contact_fraction_const, &
                                     ground_albedo_dir_mult_fact
      LOGICAL, INTENT(IN) :: use_sw_direct_albedo
      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height ! height in spartacus [m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac !building fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac !vegetation fraction [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale ! diameter of buildings [[m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale ! scale of tree crowns [m]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof !albedo of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof ! emissivity of roof [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall !albedo of wall [-]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall ! emissivity of wall [-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact !Ratio of the direct and diffuse albedo of the roof[-]
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac ! Fraction of wall reflection that is specular [-]
      ! ########################################################################################

      ! ####################################################################################
      ! ESTM_ehc
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_roof !Capacity of soil store for roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_roof !Limit for state_id of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_roof ! wetness threshold  of roof[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_roof !Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_roof !wetness status of roof [mm]

      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_wall !Capacity of soil store for wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_wall !Limit for state_id of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_wall ! wetness threshold  of wall[mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_wall !Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_wall !wetness status of wall [mm]

      ! ########################################################################################
      ! output variables
      TYPE(output_line) :: output_line_all
      ! REAL(KIND(1D0)),DIMENSION(:,:,:),ALLOCATABLE,INTENT(OUT) ::datetimeBlock
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSUEWS) :: dataOutBlockSUEWS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSnow) :: dataOutBlockSnow
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTM) :: dataOutBlockESTM
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTMExt) :: dataOutBlockESTMExt
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutRSL) :: dataOutBlockRSL
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsdataOutBEERS) :: dataOutBlockBEERS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDebug) :: dataOutBlockDebug
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSPARTACUS) :: dataOutBlockSPARTACUS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDailyState) :: dataOutBlockDailyState
      ! ########################################################################################

      ! internal temporal iteration related variables
      ! INTEGER::dt_since_start ! time since simulation starts [s]

      ! model output blocks of the same size as met forcing block

      ! local variables
      ! length of met forcing block
      INTEGER :: ir
      ! met forcing variables
      INTEGER :: iy
      INTEGER :: id
      INTEGER :: it
      INTEGER :: imin
      INTEGER :: isec
      INTEGER, PARAMETER :: gridiv_x = 1 ! a dummy gridiv as this routine is only one grid
      REAL(KIND(1D0)) :: qn1_obs
      REAL(KIND(1D0)) :: qh_obs
      REAL(KIND(1D0)) :: qe_obs
      REAL(KIND(1D0)) :: qs_obs
      REAL(KIND(1D0)) :: qf_obs
      REAL(KIND(1D0)) :: avu1
      REAL(KIND(1D0)) :: avrh
      REAL(KIND(1D0)) :: Temp_C
      REAL(KIND(1D0)) :: Press_hPa
      REAL(KIND(1D0)) :: Precip
      REAL(KIND(1D0)) :: avkdn
      REAL(KIND(1D0)) :: snowFrac_obs
      REAL(KIND(1D0)) :: ldown_obs
      REAL(KIND(1D0)) :: fcld_obs
      REAL(KIND(1D0)) :: wu_m3
      REAL(KIND(1D0)) :: xsmd
      REAL(KIND(1D0)) :: LAI_obs
      REAL(KIND(1D0)) :: kdiff
      REAL(KIND(1D0)) :: kdir
      REAL(KIND(1D0)) :: wdir

      ! REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5) :: dataOutLineSUEWS
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5) :: dataOutLineSnow
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5) :: dataOutLineESTMExt
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5) :: dataOutLineRSL
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutSOLWEIG - 5) :: dataOutLineSOLWEIG
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5) :: dataOutLineBEERS
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug - 5) :: dataOutLinedebug
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5) :: dataOutLineSPARTACUS
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5) :: dataOutLineDailyState

      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSUEWS, 1) :: dataOutBlockSUEWS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSnow, 1) :: dataOutBlockSnow_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTM, 1) :: dataOutBlockESTM_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTMExt, 1) :: dataOutBlockESTMExt_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutRSL, 1) :: dataOutBlockRSL_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsdataOutBEERS, 1) :: dataOutBlockBEERS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDebug, 1) :: dataOutBlockDebug_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSPARTACUS, 1) :: dataOutBlockSPARTACUS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDailyState, 1) :: dataOutBlockDailyState_X

      ! REAL(KIND(1D0)), DIMENSION(10) :: MetForcingData_grid ! fake array as a placeholder

      TYPE(output_block), INTENT(OUT) :: output_block_suews

      !   allocate output arrays

      Diagnose = 0

      DO ir = 1, len_sim, 1
         ! =============================================================================
         ! === Translate met data from MetForcingBlock to variable names used in model ==
         ! =============================================================================
         iy = INT(MetForcingBlock(ir, 1)) !Integer variables
         id = INT(MetForcingBlock(ir, 2))
         it = INT(MetForcingBlock(ir, 3))
         imin = INT(MetForcingBlock(ir, 4))
         isec = 0 ! NOT used by SUEWS but by WRF-SUEWS via the cal_main interface
         qn1_obs = MetForcingBlock(ir, 5) !Real values (kind(1d0))
         qh_obs = MetForcingBlock(ir, 6)
         qe_obs = MetForcingBlock(ir, 7)
         qs_obs = MetForcingBlock(ir, 8)
         qf_obs = MetForcingBlock(ir, 9)
         avu1 = MetForcingBlock(ir, 10)
         avrh = MetForcingBlock(ir, 11)
         Temp_C = MetForcingBlock(ir, 12)
         Press_hPa = MetForcingBlock(ir, 13)
         Precip = MetForcingBlock(ir, 14)
         avkdn = MetForcingBlock(ir, 15)
         snowFrac_obs = MetForcingBlock(ir, 16)
         ldown_obs = MetForcingBlock(ir, 17)
         fcld_obs = MetForcingBlock(ir, 18)
         wu_m3 = MetForcingBlock(ir, 19)
         xsmd = MetForcingBlock(ir, 20)
         LAI_obs = MetForcingBlock(ir, 21)
         kdiff = MetForcingBlock(ir, 22)
         kdir = MetForcingBlock(ir, 23)
         wdir = MetForcingBlock(ir, 24)

         ! !================================================
         ! ! below is for debugging
         ! WRITE (year_txt, '(I4)') INT(iy)
         ! WRITE (id_text, '(I3)') INT(id)
         ! WRITE (it_text, '(I4)') INT(it)
         ! WRITE (imin_text, '(I4)') INT(imin)

         ! FileStateInit = './'//TRIM(ADJUSTL(year_txt))//'_'&
         ! //TRIM(ADJUSTL(id_text))//'_'&
         ! //TRIM(ADJUSTL(it_text))//'_'&
         ! //TRIM(ADJUSTL(imin_text))//'_'&
         ! //'state_init.nml'

         ! OPEN (12, file=FileStateInit, position='rewind')

         ! write (12, *) '&state_init'
         ! write (12, *) 'aerodynamicresistancemethod=', aerodynamicresistancemethod
         ! write (12, *) 'ah_min=', ah_min
         ! write (12, *) 'ahprof_24hr=', ahprof_24hr
         ! write (12, *) 'ah_slope_cooling=', ah_slope_cooling
         ! write (12, *) 'ah_slope_heating=', ah_slope_heating
         ! write (12, *) 'alb=', alb
         ! write (12, *) 'albmax_dectr=', albmax_dectr
         ! write (12, *) 'albmax_evetr=', albmax_evetr
         ! write (12, *) 'albmax_grass=', albmax_grass
         ! write (12, *) 'albmin_dectr=', albmin_dectr
         ! write (12, *) 'albmin_evetr=', albmin_evetr
         ! write (12, *) 'albmin_grass=', albmin_grass
         ! write (12, *) 'alpha_bioco2=', alpha_bioco2
         ! write (12, *) 'alpha_enh_bioco2=', alpha_enh_bioco2
         ! write (12, *) 'alt=', alt
         ! write (12, *) 'avkdn=', avkdn
         ! write (12, *) 'avrh=', avrh
         ! write (12, *) 'avu1=', avu1
         ! write (12, *) 'baset=', baset
         ! write (12, *) 'basete=', basete
         ! write (12, *) 'BaseT_HC=', BaseT_HC
         ! write (12, *) 'beta_bioco2=', beta_bioco2
         ! write (12, *) 'beta_enh_bioco2=', beta_enh_bioco2
         ! write (12, *) 'bldgh=', bldgh
         ! write (12, *) 'capmax_dec=', capmax_dec
         ! write (12, *) 'capmin_dec=', capmin_dec
         ! write (12, *) 'chanohm=', chanohm
         ! write (12, *) 'co2pointsource=', co2pointsource
         ! write (12, *) 'cpanohm=', cpanohm
         ! write (12, *) 'crwmax=', crwmax
         ! write (12, *) 'crwmin=', crwmin
         ! write (12, *) 'daywat=', daywat
         ! write (12, *) 'daywatper=', daywatper
         ! write (12, *) 'dectreeh=', dectreeh
         ! write (12, *) 'diagnose=', diagnose
         ! write (12, *) 'diagqn=', diagqn
         ! write (12, *) 'diagqs=', diagqs
         ! write (12, *) 'drainrt=', drainrt
         ! write (12, *) 'dt_since_start=', dt_since_start
         ! write (12, *) 'dqndt=', dqndt
         ! write (12, *) 'qn_av=', qn_av
         ! write (12, *) 'dqnsdt=', dqnsdt
         ! write (12, *) 'qn1_s_av=', qn1_s_av
         ! write (12, *) 'ef_umolco2perj=', ef_umolco2perj
         ! write (12, *) 'emis=', emis
         ! write (12, *) 'emissionsmethod=', emissionsmethod
         ! write (12, *) 'enef_v_jkm=', enef_v_jkm
         ! write (12, *) 'enddls=', enddls
         ! write (12, *) 'evetreeh=', evetreeh
         ! write (12, *) 'faibldg=', faibldg
         ! write (12, *) 'faidectree=', faidectree
         ! write (12, *) 'faievetree=', faievetree
         ! write (12, *) 'faut=', faut
         ! write (12, *) 'fcef_v_kgkm=', fcef_v_kgkm
         ! write (12, *) 'fcld_obs=', fcld_obs
         ! write (12, *) 'flowchange=', flowchange
         ! write (12, *) 'frfossilfuel_heat=', frfossilfuel_heat
         ! write (12, *) 'frfossilfuel_nonheat=', frfossilfuel_nonheat
         ! write (12, *) 'g1=', g1
         ! write (12, *) 'g2=', g2
         ! write (12, *) 'g3=', g3
         ! write (12, *) 'g4=', g4
         ! write (12, *) 'g5=', g5
         ! write (12, *) 'g6=', g6
         ! write (12, *) 'gdd_id=', gdd_id
         ! write (12, *) 'gddfull=', gddfull
         ! write (12, *) 'gridiv=', gridiv
         ! write (12, *) 'gsmodel=', gsmodel
         ! write (12, *) 'hdd_id=', hdd_id
         ! write (12, *) 'humactivity_24hr=', humactivity_24hr
         ! write (12, *) 'icefrac=', icefrac
         ! write (12, *) 'id=', id
         ! write (12, *) 'ie_a=', ie_a
         ! write (12, *) 'ie_end=', ie_end
         ! write (12, *) 'ie_m=', ie_m
         ! write (12, *) 'ie_start=', ie_start
         ! write (12, *) 'imin=', imin
         ! write (12, *) 'internalwateruse_h=', internalwateruse_h
         ! write (12, *) 'IrrFracEveTr=', IrrFracEveTr
         ! write (12, *) 'IrrFracDecTr=', IrrFracDecTr
         ! write (12, *) 'irrfracgrass=', irrfracgrass
         ! write (12, *) 'isec=', isec
         ! write (12, *) 'it=', it
         ! write (12, *) 'evapmethod=', evapmethod
         ! write (12, *) 'iy=', iy
         ! write (12, *) 'kkanohm=', kkanohm
         ! write (12, *) 'kmax=', kmax
         ! write (12, *) 'lai_id=', lai_id
         ! write (12, *) 'laicalcyes=', laicalcyes
         ! write (12, *) 'laimax=', laimax
         ! write (12, *) 'laimin=', laimin
         ! write (12, *) 'lai_obs=', lai_obs
         ! write (12, *) 'laipower=', laipower
         ! write (12, *) 'laitype=', laitype
         ! write (12, *) 'lat=', lat
         ! write (12, *) 'lenday_id=', lenday_id
         ! write (12, *) 'ldown_obs=', ldown_obs
         ! write (12, *) 'lng=', lng
         ! write (12, *) 'maxconductance=', maxconductance
         ! write (12, *) 'maxfcmetab=', maxfcmetab
         ! write (12, *) 'maxqfmetab=', maxqfmetab
         ! write (12, *) 'snowwater=', snowwater
         ! ! write (12, *) 'metforcingdata_grid=', metforcingdata_grid
         ! write (12, *) 'minfcmetab=', minfcmetab
         ! write (12, *) 'minqfmetab=', minqfmetab
         ! write (12, *) 'min_res_bioco2=', min_res_bioco2
         ! write (12, *) 'narp_emis_snow=', narp_emis_snow
         ! write (12, *) 'narp_trans_site=', narp_trans_site
         ! write (12, *) 'netradiationmethod=', netradiationmethod
         ! write (12, *) 'ohm_coef=', ohm_coef
         ! write (12, *) 'ohmincqf=', ohmincqf
         ! write (12, *) 'ohm_threshsw=', ohm_threshsw
         ! write (12, *) 'ohm_threshwd=', ohm_threshwd
         ! write (12, *) 'pipecapacity=', pipecapacity
         ! write (12, *) 'popdensdaytime=', popdensdaytime
         ! write (12, *) 'popdensnighttime=', popdensnighttime
         ! write (12, *) 'popprof_24hr=', popprof_24hr
         ! write (12, *) 'pormax_dec=', pormax_dec
         ! write (12, *) 'pormin_dec=', pormin_dec
         ! write (12, *) 'precip=', precip
         ! write (12, *) 'preciplimit=', preciplimit
         ! write (12, *) 'preciplimitalb=', preciplimitalb
         ! write (12, *) 'press_hpa=', press_hpa
         ! write (12, *) 'qf0_beu=', qf0_beu
         ! write (12, *) 'qf_a=', qf_a
         ! write (12, *) 'qf_b=', qf_b
         ! write (12, *) 'qf_c=', qf_c
         ! write (12, *) 'qn1_obs=', qn1_obs
         ! write (12, *) 'qh_obs=', qh_obs
         ! write (12, *) 'qs_obs=', qs_obs
         ! write (12, *) 'qf_obs=', qf_obs
         ! write (12, *) 'radmeltfact=', radmeltfact
         ! write (12, *) 'raincover=', raincover
         ! write (12, *) 'rainmaxres=', rainmaxres
         ! write (12, *) 'resp_a=', resp_a
         ! write (12, *) 'resp_b=', resp_b
         ! write (12, *) 'roughlenheatmethod=', roughlenheatmethod
         ! write (12, *) 'roughlenmommethod=', roughlenmommethod
         ! write (12, *) 'runofftowater=', runofftowater
         ! write (12, *) 's1=', s1
         ! write (12, *) 's2=', s2
         ! write (12, *) 'sathydraulicconduct=', sathydraulicconduct
         ! write (12, *) 'sddfull=', sddfull
         ! write (12, *) 'sdd_id=', sdd_id
         ! write (12, *) 'sfr_surf=', sfr_surf
         ! write (12, *) 'smdmethod=', smdmethod
         ! write (12, *) 'snowalb=', snowalb
         ! write (12, *) 'snowalbmax=', snowalbmax
         ! write (12, *) 'snowalbmin=', snowalbmin
         ! write (12, *) 'snowpacklimit=', snowpacklimit
         ! write (12, *) 'snowdens=', snowdens
         ! write (12, *) 'snowdensmax=', snowdensmax
         ! write (12, *) 'snowdensmin=', snowdensmin
         ! write (12, *) 'snowfallcum=', snowfallcum
         ! write (12, *) 'snowfrac=', snowfrac
         ! write (12, *) 'snowlimbldg=', snowlimbldg
         ! write (12, *) 'snowlimpaved=', snowlimpaved
         ! write (12, *) 'snowfrac_obs=', snowfrac_obs
         ! write (12, *) 'snowpack=', snowpack
         ! write (12, *) 'snowprof_24hr=', snowprof_24hr
         ! write (12, *) 'SnowUse=', SnowUse
         ! write (12, *) 'soildepth=', soildepth
         ! write (12, *) 'soilstore_id=', soilstore_id
         ! write (12, *) 'soilstorecap=', soilstorecap
         ! write (12, *) 'stabilitymethod=', stabilitymethod
         ! write (12, *) 'startdls=', startdls
         ! write (12, *) 'state_id=', state_id
         ! write (12, *) 'statelimit=', statelimit
         ! write (12, *) 'storageheatmethod=', storageheatmethod
         ! write (12, *) 'storedrainprm=', storedrainprm
         ! write (12, *) 'surfacearea=', surfacearea
         ! write (12, *) 'tair_av=', tair_av
         ! write (12, *) 'tau_a=', tau_a
         ! write (12, *) 'tau_f=', tau_f
         ! write (12, *) 'tau_r=', tau_r
         ! write (12, *) 'tmax_id=', tmax_id
         ! write (12, *) 'tmin_id=', tmin_id
         ! write (12, *) 'BaseT_Cooling=', BaseT_Cooling
         ! write (12, *) 'BaseT_Heating=', BaseT_Heating
         ! write (12, *) 'temp_c=', temp_c
         ! write (12, *) 'tempmeltfact=', tempmeltfact
         ! write (12, *) 'th=', th
         ! write (12, *) 'theta_bioco2=', theta_bioco2
         ! write (12, *) 'timezone=', timezone
         ! write (12, *) 'tl=', tl
         ! write (12, *) 'trafficrate=', trafficrate
         ! write (12, *) 'trafficunits=', trafficunits
         ! write (12, *) 'traffprof_24hr=', traffprof_24hr
         ! ! write (12, *) 'ts5mindata_ir=', ts5mindata_ir
         ! write (12, *) 'tstep=', tstep
         ! write (12, *) 'tstep_prev=', tstep_prev
         ! write (12, *) 'veg_type=', veg_type
         ! write (12, *) 'waterdist=', waterdist
         ! write (12, *) 'waterusemethod=', waterusemethod
         ! write (12, *) 'wetthresh=', wetthresh
         ! write (12, *) 'wu_m3=', wu_m3
         ! write (12, *) 'wuday_id=', wuday_id
         ! write (12, *) 'decidcap_id=', decidcap_id
         ! write (12, *) 'albdectr_id=', albdectr_id
         ! write (12, *) 'albevetr_id=', albevetr_id
         ! write (12, *) 'albgrass_id=', albgrass_id
         ! write (12, *) 'porosity_id=', porosity_id
         ! write (12, *) 'wuprofa_24hr=', wuprofa_24hr
         ! write (12, *) 'wuprofm_24hr=', wuprofm_24hr
         ! write (12, *) 'xsmd=', xsmd
         ! write (12, *) 'z=', z
         ! write (12, *) 'z0m_in=', z0m_in
         ! write (12, *) 'zdm_in=', zdm_in
         ! write (12, *) '/'

         ! WRITE (12, *) ''

         ! CLOSE (12)
         ! !================================================

         ! CALL SUEWS_cal_Main( &
         CALL SUEWS_cal_Main_DTS( &
            AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, & ! input&inout in alphabetical order
            AH_SLOPE_Heating, &
            alb, AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
            AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
            alpha_bioCO2, alpha_enh_bioCO2, alt, avkdn, avRh, avU1, BaseT, BaseTe, &
            beta_bioCO2, beta_enh_bioCO2, bldgH, CapMax_dec, CapMin_dec, &
            chAnOHM, CO2PointSource, cpAnOHM, CRWmax, CRWmin, DayWat, DayWatPer, &
            DecTreeH, DiagMethod, Diagnose, DRAINRT, &
            dt_since_start, dqndt, qn_av, dqnsdt, qn_s_av, &
            EF_umolCO2perJ, emis, EmissionsMethod, EnEF_v_Jkm, endDLS, EveTreeH, FAIBldg, &
            FAIDecTree, FAIEveTree, FAIMethod, Faut, FcEF_v_kgkm, fcld_obs, FlowChange, &
            FrFossilFuel_Heat, FrFossilFuel_NonHeat, G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, GDD_id, &
            GDDFull, Gridiv, gsModel, H_maintain, HDD_id, HumActivity_24hr, &
            IceFrac, id, Ie_a, Ie_end, Ie_m, Ie_start, imin, &
            InternalWaterUse_h, &
            IrrFracPaved, IrrFracBldgs, &
            IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
            IrrFracBSoil, IrrFracWater, &
            isec, it, &
            iy, kkAnOHM, Kmax, LAI_id, LAIMax, LAIMin, LAI_obs, &
            LAIPower, LAIType, lat, lenDay_id, ldown_obs, lng, MaxConductance, MaxFCMetab, MaxQFMetab, &
            SnowWater, MinFCMetab, MinQFMetab, min_res_bioCO2, &
            NARP_EMIS_SNOW, NARP_TRANS_SITE, NetRadiationMethod, &
            nlayer, &
            n_vegetation_region_urban, &
            n_stream_sw_urban, n_stream_lw_urban, &
            sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
            veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
            veg_fsd_const, veg_contact_fraction_const, &
            ground_albedo_dir_mult_fact, use_sw_direct_albedo, & !input
            height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
            alb_roof, emis_roof, alb_wall, emis_wall, &
            roof_albedo_dir_mult_fact, wall_specular_frac, &
            OHM_coef, OHMIncQF, OHM_threshSW, &
            OHM_threshWD, PipeCapacity, PopDensDaytime, &
            PopDensNighttime, PopProf_24hr, PorMax_dec, PorMin_dec, &
            Precip, PrecipLimit, PrecipLimitAlb, Press_hPa, &
            QF0_BEU, Qf_A, Qf_B, Qf_C, &
            qn1_obs, qs_obs, qf_obs, &
            RadMeltFact, RAINCOVER, RainMaxRes, resp_a, resp_b, &
            RoughLenHeatMethod, RoughLenMomMethod, RunoffToWater, S1, S2, &
            SatHydraulicConduct, SDDFull, SDD_id, SMDMethod, SnowAlb, SnowAlbMax, &
            SnowAlbMin, SnowPackLimit, SnowDens, SnowDensMax, SnowDensMin, SnowfallCum, SnowFrac, &
            SnowLimBldg, SnowLimPaved, snowFrac_obs, SnowPack, SnowProf_24hr, SnowMethod, SoilDepth, &
            StabilityMethod, startDLS, &
            soilstore_surf, SoilStoreCap_surf, state_surf, StateLimit_surf, WetThresh_surf, &
            soilstore_roof, SoilStoreCap_roof, state_roof, StateLimit_roof, WetThresh_roof, &
            soilstore_wall, SoilStoreCap_wall, state_wall, StateLimit_wall, WetThresh_wall, &
            StorageHeatMethod, StoreDrainPrm, SurfaceArea, Tair_av, tau_a, tau_f, tau_r, &
            Tmax_id, Tmin_id, &
            BaseT_Cooling, BaseT_Heating, Temp_C, TempMeltFact, TH, &
            theta_bioCO2, timezone, TL, TrafficRate, TrafficUnits, &
            sfr_surf, &
            tsfc_roof, tsfc_wall, tsfc_surf, &
            temp_roof, temp_wall, temp_surf, &
            tin_roof, tin_wall, tin_surf, &
            k_roof, k_wall, k_surf, &
            cp_roof, cp_wall, cp_surf, &
            dz_roof, dz_wall, dz_surf, &
            TraffProf_24hr, Ts5mindata_ir, tstep, tstep_prev, veg_type, &
            WaterDist, WaterUseMethod, wu_m3, &
            WUDay_id, DecidCap_id, albDecTr_id, albEveTr_id, albGrass_id, porosity_id, &
            WUProfA_24hr, WUProfM_24hr, xsmd, Z, z0m_in, zdm_in, &
            output_line_all) !output
         ! datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, & !output
         ! dataOutLineBEERS, & !output
         ! dataOutLineDebug, dataOutLineSPARTACUS, &
         ! dataOutLineESTMExt, &
         ! dataOutLineDailyState) !output

         ! update dt_since_start_x for next iteration, dt_since_start_x is used for Qn averaging. TS 28 Nov 2018
         dt_since_start = dt_since_start + tstep

         !============ update DailyStateBlock ===============
         dataOutBlockDailyState(ir, :) = [output_line_all%DailyState]

         !============ write out results ===============
         ! works at each timestep
         CALL SUEWS_update_output( &
            SnowMethod, storageheatmethod, & !input
            len_sim, 1, &
            ir, gridiv_x, &
            output_line_all%SUEWS, &
            output_line_all%Snow, &
            output_line_all%ESTM, & !input
            output_line_all%RSL, &
            output_line_all%BEERS, &
            output_line_all%Debug, &
            output_line_all%SPARTACUS, &
            output_line_all%ESTMExt, & !input
            dataOutBlockSUEWS_X, dataOutBlockSnow_X, dataOutBlockESTM_X, & !
            dataOutBlockRSL_X, dataOutBlockBEERS_X, dataOutBlockDebug_X, dataOutBlockSPARTACUS_X, dataOutBlockESTMExt_X) !inout

      END DO

      dataOutBlockSUEWS = dataOutBlockSUEWS_X(:, :, 1)
      dataOutBlockSnow = dataOutBlockSnow_X(:, :, 1)
      dataOutBlockESTM = dataOutBlockESTM_X(:, :, 1)
      dataOutBlockESTMExt = dataOutBlockESTMExt_X(:, :, 1)
      dataOutBlockRSL = dataOutBlockRSL_X(:, :, 1)
      dataOutBlockBEERS = dataOutBlockBEERS_X(:, :, 1)
      dataOutBlockDebug = dataOutBlockDebug_X(:, :, 1)
      dataOutBlockSPARTACUS = dataOutBlockSPARTACUS_X(:, :, 1)
      ! dataOutBlockDailyState = dataOutBlockDailyState_X(:, :, 1)

      ! initialize output block
      CALL output_block_finalize(output_block_suews)
      CALL output_block_init(output_block_suews, len_sim)
      ! transfer data to output block
      output_block_suews%SUEWS = dataOutBlockSUEWS
      output_block_suews%Snow = dataOutBlockSnow
      output_block_suews%ESTM = dataOutBlockESTM
      output_block_suews%ESTMExt = dataOutBlockESTMExt
      output_block_suews%RSL = dataOutBlockRSL
      output_block_suews%BEERS = dataOutBlockBEERS
      output_block_suews%Debug = dataOutBlockDebug
      output_block_suews%SPARTACUS = dataOutBlockSPARTACUS
      output_block_suews%DailyState = dataOutBlockDailyState

   END SUBROUTINE SUEWS_cal_multitsteps

! a wrapper of NARP_cal_SunPosition used by supy
   SUBROUTINE SUEWS_cal_sunposition( &
      year, idectime, UTC, locationlatitude, locationlongitude, locationaltitude, & !input
      sunazimuth, sunzenith) !output
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: year, idectime, UTC, &
                                     locationlatitude, locationlongitude, locationaltitude
      REAL(KIND(1D0)), INTENT(out) :: sunazimuth, sunzenith

      CALL NARP_cal_SunPosition( &
         year, idectime, UTC, locationlatitude, locationlongitude, locationaltitude, &
         sunazimuth, sunzenith)

   END SUBROUTINE SUEWS_cal_sunposition

! function func(arg) result(retval)
!    implicit none
!    type :: arg
!    type :: retval

! end function func

   FUNCTION cal_tair_av(tair_av_prev, dt_since_start, tstep, temp_c) RESULT(tair_av_next)
      ! calculate mean air temperature of past 24 hours
      ! TS, 17 Sep 2019
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(in) :: tair_av_prev
      REAL(KIND(1D0)), INTENT(in) :: temp_c
      INTEGER, INTENT(in) :: dt_since_start
      INTEGER, INTENT(in) :: tstep

      REAL(KIND(1D0)) :: tair_av_next

      REAL(KIND(1D0)), PARAMETER :: len_day_s = 24*3600 ! day length in seconds
      REAL(KIND(1D0)) :: len_cal_s ! length of average period in seconds
      REAL(KIND(1D0)) :: temp_k ! temp in K

      ! determine the average period
      IF (dt_since_start > len_day_s) THEN
         ! if simulation has been running over one day
         len_cal_s = len_day_s
      ELSE
         ! if simulation has been running less than one day
         len_cal_s = dt_since_start + tstep
      END IF
      temp_k = temp_c + 273.15
      tair_av_next = tair_av_prev*(len_cal_s - tstep*1.)/len_cal_s + temp_k*tstep/len_cal_s

   END FUNCTION cal_tair_av

   FUNCTION cal_tsfc(qh, avdens, avcp, RA, temp_c) RESULT(tsfc_C)
      ! calculate surface/skin temperature
      ! TS, 23 Oct 2019
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(in) :: qh ! sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avdens ! air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avcp !air heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: RA !Aerodynamic resistance [s m^-1]
      REAL(KIND(1D0)), INTENT(in) :: temp_C ! air temperature [C]

      REAL(KIND(1D0)) :: tsfc_C ! surface temperature [C]

      tsfc_C = qh/(avdens*avcp)*RA + temp_C
   END FUNCTION cal_tsfc

END MODULE SUEWS_Driver
