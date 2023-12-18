MODULE SUEWS_DEF_DTS
   USE allocateArray, ONLY: &
      nsurf, nvegsurf, nspec, &
      PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf

   IMPLICIT NONE
   ! in the following, the type definitions starting with `SUEWS_` are used in the main program

   ! ********** SUEWS_parameters schema (basic) **********
   TYPE, PUBLIC :: SUEWS_CONFIG
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
      INTEGER :: DiagQS ! flag for printing diagnostic info for QS module during runtime [N/A] ! not used and will be removed
      INTEGER :: EvapMethod ! Evaporation calculated according to Rutter (1) or Shuttleworth (2) [-]
      INTEGER :: LAImethod ! boolean to determine if calculate LAI [-]
      INTEGER :: localClimateMethod ! method to choose local climate variables [-] 0: not use; 1: use local climate variables
   END TYPE SUEWS_CONFIG

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
   CONTAINS
      PROCEDURE :: ALLOCATE => allocate_spartacus_layer_prm_c
      PROCEDURE :: DEALLOCATE => deallocate_spartacus_layer_prm_c
   END TYPE SPARTACUS_LAYER_PRM

   ! ********** SUEWS_parameters schema (derived) **********

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
   CONTAINS
      PROCEDURE :: ALLOCATE => allocate_ehc_prm_c
      PROCEDURE :: DEALLOCATE => deallocate_ehc_prm_c
   END TYPE EHC_PRM

   TYPE, PUBLIC :: LC_PAVED_PRM
      ! land cover specific parameters for paved surfaces
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
      ! land cover specific parameters for buildings
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
      ! land cover specific parameters for deciduous trees
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
      ! land cover specific parameters for evergreen trees
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
      ! land cover specific parameters for grass
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
      ! land cover specific parameters for bare soil
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
      ! land cover specific parameters for water surface
      REAL(KIND(1D0)) :: sfr
      REAL(KIND(1D0)) :: emis
      TYPE(OHM_PRM) :: ohm
      TYPE(SOIL_PRM) :: soil
      REAL(KIND(1D0)) :: statelimit
      REAL(KIND(1D0)) :: irrfracwater
      REAL(KIND(1D0)) :: wetthresh ! ******* dummy variable *******
      REAL(KIND(1D0)) :: flowchange ! special term in water
   END TYPE LC_WATER_PRM

   TYPE, PUBLIC :: SUEWS_SITE
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

      ! surface cover fractions related
      REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface cover fraction[-]
      REAL(KIND(1D0)) :: VegFraction ! fraction of vegetation [-]
      REAL(KIND(1D0)) :: ImpervFraction !fractioin of impervious surface [-]
      REAL(KIND(1D0)) :: PervFraction !fraction of pervious surfaces [-]
      REAL(KIND(1D0)) :: NonWaterFraction !fraction of non-water [-]
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

   CONTAINS
      PROCEDURE :: ALLOCATE => allocate_site_prm_c
      PROCEDURE :: DEALLOCATE => deallocate_site_prm_c
      PROCEDURE :: cal_surf => SUEWS_cal_surf_DTS

   END TYPE SUEWS_SITE

   ! ********** SUEWS_stateVars schema **********
   TYPE, PUBLIC :: anthroEmis_STATE
      REAL(KIND(1D0)), DIMENSION(12) :: HDD_id !Heating Degree Days [degC d]

      REAL(KIND(1D0)) :: Fc !total co2 flux [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_anthro !anthropogenic co2 flux  [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_biogen !biogenic CO2 flux [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_build ! anthropogenic co2 flux  [umol m-2 s-1]

      REAL(KIND(1D0)) :: Fc_metab ! co2 emission from metabolism component [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_photo !co2 flux from photosynthesis [umol m
      REAL(KIND(1D0)) :: Fc_point ! co2 emission from point source [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_respi !co2 flux from respiration [umol m-2 s-1]
      REAL(KIND(1D0)) :: Fc_traff ! co2 emission from traffic component [umol m-2 s-1]
   END TYPE anthroEmis_STATE

   TYPE, PUBLIC :: OHM_STATE
      REAL(KIND(1D0)) :: qn_av ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: dqndt ! rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)) :: qn_s_av ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)) :: dqnsdt ! Rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)) :: a1 !AnOHM coefficients of grid [-]
      REAL(KIND(1D0)) :: a2 ! AnOHM coefficients of grid [h]
      REAL(KIND(1D0)) :: a3 !AnOHM coefficients of grid [W m-2]
   END TYPE OHM_STATE

   TYPE, PUBLIC :: solar_State
      REAL(KIND(1D0)) :: azimuth_deg !solar azimuth [angle]
      REAL(KIND(1D0)) :: ZENITH_deg !solar zenith angle [deg]
   END TYPE solar_State

   TYPE, PUBLIC :: atm_state
      REAL(KIND(1D0)) :: fcld !estomated cloud fraction [-]
      REAL(KIND(1D0)) :: avcp !Specific heat capacity
      REAL(KIND(1D0)) :: dens_dry !Dry air density kg m-3
      REAL(KIND(1D0)) :: avdens !Average air density
      REAL(KIND(1D0)) :: dq !Specific humidity deficit
      REAL(KIND(1D0)) :: Ea_hPa !Water vapour pressure in hPa
      REAL(KIND(1D0)) :: Es_hPa !Saturation vapour pressure in hPa
      REAL(KIND(1D0)) :: lv_J_kg !Latent heat of vaporization in [J kg-1]
      REAL(KIND(1D0)) :: lvS_J_kg !latent heat of sublimation [J kg-1]
      REAL(KIND(1D0)) :: tlv !Latent heat of vaporization per timestep [J kg-1 s-1] (tlv=lv_J_kg/tstep_real)
      REAL(KIND(1D0)) :: psyc_hPa !Psychometric constant in hPa
      REAL(KIND(1D0)) :: psycIce_hPa !Psychometric constant in hPa for snow
      REAL(KIND(1D0)) :: s_Pa !Vapour pressure versus temperature slope in Pa
      REAL(KIND(1D0)) :: s_hpa !Vapour pressure versus temperature slope in hPa
      REAL(KIND(1D0)) :: sIce_hpa !Vapour pressure versus temperature slope in hPa above ice/snow
      REAL(KIND(1D0)) :: vpd_hPa !Vapour pressure deficit in hPa
      REAL(KIND(1D0)) :: vpd_pa !Vapour pressure deficit in Pa
      REAL(KIND(1D0)) :: U10_ms !average wind speed at 10m [W m-1]
      REAL(KIND(1D0)) :: t2_C !modelled 2 meter air temperature [degC]
      REAL(KIND(1D0)) :: q2_gkg ! Air specific humidity at 2 m [g kg-1]
      REAL(KIND(1D0)) :: RH2 ! air relative humidity at 2m [-]
      REAL(KIND(1D0)) :: L_mod !Obukhov length [m]
      REAL(KIND(1D0)) :: zL ! Stability scale [-]
      REAL(KIND(1D0)) :: RA_h ! aerodynamic resistance [s m-1]
      REAL(KIND(1D0)) :: RS ! surface resistance [s m-1]
      REAL(KIND(1D0)) :: UStar !friction velocity [m s-1]
      REAL(KIND(1D0)) :: TStar !T*, temperature scale [-]
      REAL(KIND(1D0)) :: RB !boundary layer resistance shuttleworth
      REAL(KIND(1D0)) :: Tair_av ! 5-day moving average of air temperature [degC]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: rss_surf ! surface resistance adjusted by surface wetness state[s m-1]

   END TYPE atm_state

   TYPE, PUBLIC :: PHENOLOGY_STATE
      REAL(KIND(1D0)), DIMENSION(NSURF) :: alb
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: lai_id ! Initial LAI values.
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDD_id ! Growing Degree Days [degC](see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDD_id ! Senescence Degree Days [degC](see SUEWS_DailyState.f95)
      REAL(KIND(1D0)) :: VegPhenLumps
      REAL(KIND(1D0)) :: porosity_id !
      REAL(KIND(1D0)) :: decidcap_id ! Storage capacity of deciduous surface `DecTr`; updated each day in simulaiton due to changes in LAI.
      REAL(KIND(1D0)) :: albDecTr_id !
      REAL(KIND(1D0)) :: albEveTr_id !
      REAL(KIND(1D0)) :: albGrass_id !
      REAL(KIND(1D0)) :: Tmin_id ! Daily minimum temperature [degC]
      REAL(KIND(1D0)) :: Tmax_id ! Daily maximum temperature [degC]
      REAL(KIND(1D0)) :: lenDay_id ! daytime length [h]
      REAL(KIND(1D0)) :: TempVeg ! temporary vegetative surface fraction adjusted by rainfall [-]
      REAL(KIND(1D0)), DIMENSION(6, NSURF) :: StoreDrainPrm ! coefficients used in drainage calculation [-]

      REAL(KIND(1D0)) :: gfunc ! stomatal conductance function [-]
      REAL(KIND(1D0)) :: gsc !Surface Layer Conductance [s m-1]
      REAL(KIND(1D0)) :: g_kdown ! surface conductance function for shortwave radiation [-]
      REAL(KIND(1D0)) :: g_dq ! surface conductance function for specific humidity [-]
      REAL(KIND(1D0)) :: g_ta ! surface conductance function for air temperature [-]
      REAL(KIND(1D0)) :: g_smd ! surface conductance function for soil moisture deficit [-]
      REAL(KIND(1D0)) :: g_lai ! surface conductance function for LAI [-]
   END TYPE PHENOLOGY_STATE

   TYPE, PUBLIC :: SNOW_STATE
      REAL(KIND(1D0)) :: snowfallCum !
      REAL(KIND(1D0)) :: snowalb ! albedo of snow [-]
      REAL(KIND(1D0)) :: chSnow_per_interval ! change state_id of snow and surface per time interval [mm]
      REAL(KIND(1D0)) :: mwh !snowmelt [mm]
      REAL(KIND(1D0)) :: mwstore !overall met water [mm]

      REAL(KIND(1D0)) :: qn_snow !net all-wave radiation on snow surface [W m-2]
      REAL(KIND(1D0)) :: Qm !Snowmelt-related heat [W m-2]
      REAL(KIND(1D0)) :: QmFreez !heat related to freezing of surface store [W m-2]
      REAL(KIND(1D0)) :: QmRain !melt heat for rain on snow [W m-2]

      REAL(KIND(1D0)) :: swe !overall snow water equavalent[mm]

      REAL(KIND(1D0)) :: z0vSnow !roughness for heat [m]
      REAL(KIND(1D0)) :: RAsnow !Aerodynamic resistance for snow [s m-1]
      REAL(KIND(1D0)) :: sIce_hpa !satured curve on snow [hPa]

      REAL(KIND(1D0)), DIMENSION(2) :: SnowRemoval !snow removal [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: icefrac ! fraction of ice in snowpack [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowdens ! snow density [kg m-3]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowfrac ! snow fraction [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowpack ! snow water equivalent on each land cover [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: snowwater ! snow water[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind_snow !outgoing shortwave on snowpack [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: qn_ind_snow !net all-wave radiation on snowpack [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: deltaQi ! storage heat flux of snow surfaces [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Tsurf_ind_snow !snowpack surface temperature [C]
   END TYPE SNOW_STATE

   TYPE, PUBLIC :: HYDRO_STATE
      ! REAL(KIND(1D0)) :: runofftowater   ! Fraction of above-ground runoff flowing to water surface during flooding
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_surf ! Initial water stored in soil beneath `Bldgs` surface
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_surf ! Initial wetness condition on SUEWS land covers.
      REAL(KIND(1D0)), DIMENSION(9) :: WUDay_id ! Daily water use for EveTr, DecTr, Grass [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soilstore_roof ! Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_roof ! wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soilstore_wall ! Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_wall ! wetness status of wall [mm]

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: ev_roof ! evapotranspiration of each roof layer [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: ev_wall ! evapotranspiration of each wall type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev0_surf ! evapotranspiration from PM of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev_surf ! evapotranspiration of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: wu_surf !external water use of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: runoffSoil !Soil runoff from each soil sub-surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: smd_nsurf !soil moisture deficit for each surface
      REAL(KIND(1D0)), DIMENSION(NSURF) :: drain_surf !drainage of each surface type [mm]

      REAL(KIND(1D0)) :: drain_per_tstep ! total drainage for all surface type at each timestep [mm]
      REAL(KIND(1D0)) :: QE_LUMPS !turbulent latent heat flux by LUMPS model [W m-2]
      REAL(KIND(1D0)) :: ev_per_tstep ! evaporation at each time step [mm]
      REAL(KIND(1D0)) :: wu_ext !external water use [mm]
      REAL(KIND(1D0)) :: wu_int !internal water use [mm]

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
      REAL(KIND(1D0)) :: tot_chang_per_tstep !Change in surface state_id [mm]
      REAL(KIND(1D0)) :: runoff_per_interval !run-off at each time interval [mm]
      REAL(KIND(1D0)) :: NWstate_per_tstep ! state_id at each tinestep(excluding water body) [mm]

      REAL(KIND(1D0)) :: SoilMoistCap !Maximum capacity of soil store [mm]
      REAL(KIND(1D0)) :: vsmd !Soil moisture deficit for vegetated surfaces only [mm]

      ! TODO: TS 25 Oct 2017
      ! the  variables are not used currently as grid-to-grid connection is NOT set up.
      ! set these variables as zero.
      REAL(KIND(1D0)) :: AdditionalWater = 0 !!Additional water coming from other grids [mm] (these are expressed as depths over the whole surface)
      REAL(KIND(1D0)) :: addImpervious = 0
      REAL(KIND(1D0)) :: addPipes = 0
      REAL(KIND(1D0)) :: addVeg = 0
      REAL(KIND(1D0)) :: addWaterBody = 0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: AddWater = 0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: frac_water2runoff = 0
   CONTAINS
      PROCEDURE :: ALLOCATE => allocHydroState_c
      PROCEDURE :: DEALLOCATE => deallocHydroState_c
   END TYPE HYDRO_STATE

   TYPE, PUBLIC :: HEAT_STATE
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_roof ! interface temperature between depth layers in roof [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_wall ! interface temperature between depth layers in wall [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_surf ! interface temperature between depth layers [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof ! roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_wall ! wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf ! surface temperature [degC]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc0_out_roof !surface temperature of roof[degC]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc0_out_wall !surface temperature of wall[degC]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc0_out_surf !surface temperature [degC]

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

      REAL(KIND(1D0)), DIMENSION(nsurf) :: qs_surf ! aggregated heat storage of of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: QN_surf ! net all-wave radiation of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe0_surf ! latent heat flux from PM of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_surf ! sensinle heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_resist_surf ! resistance based sensible heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: tsurf_ind !snow-free surface temperature [degC]

      REAL(KIND(1D0)) :: QH_LUMPS !turbulent sensible heat flux from LUMPS model [W m-2]

      REAL(KIND(1D0)) :: kclear !clear sky incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: kup !outgoing shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: ldown !incoming longtwave radiation [W m-2]
      REAL(KIND(1D0)) :: lup !outgoing longwave radiation [W m-2]

      REAL(KIND(1D0)) :: qe !turbuent latent heat flux [W m-2]
      REAL(KIND(1D0)) :: qf !anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)) :: QF_SAHP !total anthropogeic heat flux when EmissionMethod is not 0 [W m-2]
      REAL(KIND(1D0)) :: qh !turbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_residual ! residual based sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_resist !resistance bnased sensible heat flux [W m-2]

      REAL(KIND(1D0)) :: qn !net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: qn_snowfree !net all-wave radiation on snow-free surface [W m-2]
      REAL(KIND(1D0)) :: qs !heat storage flux [W m-2]

      REAL(KIND(1D0)) :: TSfc_C ! surface temperature [degC]
      REAL(KIND(1D0)) :: tsurf !surface temperatue [degC]
      REAL(KIND(1D0)) :: QH_Init !initialised sensible heat flux [W m-2]
   CONTAINS
      PROCEDURE :: ALLOCATE => allocHeatState_c
      PROCEDURE :: DEALLOCATE => deallocHeatState_c
   END TYPE HEAT_STATE

   TYPE, PUBLIC :: ROUGHNESS_STATE
      ! this type is used to collect the intermediate results in the SUEWS model

      ! calculated values of FAI
      REAL(KIND(1D0)) :: FAIBldg_use
      REAL(KIND(1D0)) :: FAIEveTree_use
      REAL(KIND(1D0)) :: FAIDecTree_use

      REAL(KIND(1D0)) :: FAI
      REAL(KIND(1D0)) :: PAI
      REAL(KIND(1D0)) :: Zh ! effective height of bluff bodies
      REAL(KIND(1D0)) :: z0m ! aerodynamic roughness length
      REAL(KIND(1D0)) :: z0v ! roughness for heat [m]
      REAL(KIND(1D0)) :: zdm ! zero-plance displacement
      REAL(KIND(1D0)) :: ZZD ! z-zdm

   END TYPE ROUGHNESS_STATE

   ! incorporate all model states into one lumped type
   TYPE, PUBLIC :: SUEWS_STATE
      TYPE(anthroEmis_STATE) :: anthroemisState
      TYPE(OHM_STATE) :: ohmState
      TYPE(solar_State) :: solarState
      TYPE(atm_state) :: atmState
      TYPE(PHENOLOGY_STATE) :: phenState
      TYPE(SNOW_STATE) :: snowState
      TYPE(HYDRO_STATE) :: hydroState
      TYPE(HEAT_STATE) :: heatState
      TYPE(ROUGHNESS_STATE) :: roughnessState
   END TYPE SUEWS_STATE

   ! ********** SUEWS_forcing schema **********
   TYPE, PUBLIC :: SUEWS_FORCING
      REAL(KIND(1D0)) :: kdown !
      REAL(KIND(1D0)) :: ldown !
      REAL(KIND(1D0)) :: RH !
      REAL(KIND(1D0)) :: pres !
      REAL(KIND(1D0)) :: Tair_av_5d ! 5-day moving average of air temperature [degC]
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
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Ts5mindata_ir !surface temperature input data[degC] used in ESTM --> may be deprecated in the future once EHC is more mature
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

      ! values that are derived from tstep
      INTEGER :: nsh ! number of timesteps per hour
      REAL(KIND(1D0)) :: nsh_real ! nsh in type real [-]
      REAL(KIND(1D0)) :: tstep_real ! tstep in type real
      REAL(KIND(1D0)) :: dectime !decimal time [-]

      INTEGER, DIMENSION(3) :: dayofWeek_id ! 1 - day of week; 2 - month; 3 - season

      INTEGER :: DLS !daylight saving time offset [h]

   END TYPE SUEWS_TIMER

CONTAINS
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

      ALLOCATE (self%tsfc_roof(num_layer))
      ALLOCATE (self%tsfc_wall(num_layer))
      ALLOCATE (self%tsfc_surf(num_surf))

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
      !
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
      IF (ALLOCATED(self%temp_surf)) DEALLOCATE (self%temp_surf)
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

END MODULE SUEWS_DEF_DTS