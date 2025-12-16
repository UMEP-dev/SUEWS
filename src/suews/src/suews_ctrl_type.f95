MODULE module_ctrl_type
   USE module_ctrl_const_allocate, ONLY: &
      nsurf, nvegsurf, nspec, &
      PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf, &
      ivConif, ivDecid, ivGrass, &
      ncolumnsDataOutSUEWS, ncolumnsDataOutSnow, &
      ncolumnsDataOutESTM, ncolumnsDataOutDailyState, &
      ncolumnsDataOutRSL, ncolumnsdataOutSOLWEIG, ncolumnsDataOutBEERS, &
      ncolumnsDataOutDebug, ncolumnsDataOutSPARTACUS, ncolumnsDataOutEHC, &
      ncolumnsDataOutSTEBBS, ncolumnsDataOutNHood

   use module_type_stebbs, only: &
      STEBBS_BLDG, STEBBS_STATE, NHOOD_STATE, &
      BUILDING_ARCHETYPE_PRM, STEBBS_PRM

   use module_type_soil, only: SOIL_PRM

   use module_type_landcover, only: &
      LC_PAVED_PRM, LC_BLDG_PRM, LC_DECTR_PRM, LC_EVETR_PRM, &
      LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM

   use module_type_surface, only: &
      LUMPS_PRM, OHM_COEF_LC, OHM_PRM, OHM_STATE, &
      CONDUCTANCE_PRM, ROUGHNESS_STATE

   use module_type_vegetation, only: &
      bioCO2_PRM, LAI_PRM, PHENOLOGY_STATE

   use module_type_anthro, only: &
      anthroHEAT_PRM, anthroEMIS_PRM, anthroEmis_STATE

   use module_type_waterdist, only: &
      SURF_STORE_PRM, WATER_DIST_PRM, IRRIG_daywater, IRRIGATION_PRM

   use module_type_snow, only: &
      SNOW_PRM, SNOW_STATE

   use module_type_spartacus, only: &
      SPARTACUS_PRM, SPARTACUS_LAYER_PRM

   use module_type_ehc, only: &
      EHC_PRM

   use module_type_heat, only: &
      HEAT_STATE

   use module_type_hydro, only: &
      HYDRO_STATE

   use module_type_atmosphere, only: &
      solar_State, atm_state

   IMPLICIT NONE
   ! in the following, the type definitions starting with `SUEWS_` are used in the main program

   ! ********** SUEWS_parameters schema (basic) **********
   TYPE, PUBLIC :: SUEWS_CONFIG
      INTEGER :: RSLMethod = 0 ! Defines how near surface diagnostics are calculated
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
      INTEGER :: RSLLevel = 0 ! method to choose local climate variables [-] 0: not use; 1: use local climate variables
      INTEGER :: stebbsmethod = 0 ! method to calculate building energy [-]
      INTEGER :: rcmethod = 0 ! method to split building envelope heat capacity in STEBBS [-]
      LOGICAL :: flag_test = .FALSE. ! FOR DEBUGGING ONLY: boolean to test specific functions [-]
   END TYPE SUEWS_CONFIG

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
      INTEGER :: nbtypes = 1 ! number of building archetypes [-] (GH#360)
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: sfr_bldg_arch ! surface fraction of each building archetype [-]

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

      TYPE(BUILDING_ARCHETYPE_PRM) :: building_archtype ! Single archetype (backwards compatible)
      TYPE(BUILDING_ARCHETYPE_PRM), DIMENSION(:), ALLOCATABLE :: building_archetypes ! Multi-archetype array (GH#360)
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





   SUBROUTINE allocate_site_prm_c(self, nlayer)
      CLASS(SUEWS_SITE), INTENT(inout) :: self
      INTEGER, INTENT(in) :: nlayer
      CALL self%DEALLOCATE()
      ALLOCATE (self%sfr_roof(nlayer))
      ALLOCATE (self%sfr_wall(nlayer))
      ! Allocate building archetype arrays based on nbtypes (GH#360)
      ALLOCATE (self%building_archetypes(self%nbtypes))
      ALLOCATE (self%sfr_bldg_arch(self%nbtypes))
      self%sfr_bldg_arch = 1.0D0 / REAL(self%nbtypes, KIND(1D0)) ! Equal fractions by default

   END SUBROUTINE allocate_site_prm_c

   SUBROUTINE deallocate_site_prm_c(self)
      IMPLICIT NONE

      CLASS(SUEWS_SITE), INTENT(inout) :: self

      IF (ALLOCATED(self%sfr_roof)) DEALLOCATE (self%sfr_roof)
      IF (ALLOCATED(self%sfr_wall)) DEALLOCATE (self%sfr_wall)
      IF (ALLOCATED(self%building_archetypes)) DEALLOCATE (self%building_archetypes)
      IF (ALLOCATED(self%sfr_bldg_arch)) DEALLOCATE (self%sfr_bldg_arch)

   END SUBROUTINE deallocate_site_prm_c



   SUBROUTINE SUEWS_cal_surf_DTS( &
      self, & !inout
      config & !input
      ) ! output
      IMPLICIT NONE

      CLASS(SUEWS_SITE), INTENT(INOUT) :: self
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config

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

END MODULE module_ctrl_type

