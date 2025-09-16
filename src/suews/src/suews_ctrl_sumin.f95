!========================================================================================
! a mini version of SUEWS to be coupled with WRF
! TS 22 Apr 2018: initial
! TS 11 Jun 2018: modified according to recent SUEWS development

MODULE SuMin_Module
   USE SUEWS_Driver, ONLY: SUEWS_cal_Main, &
                           PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf, &
                           ivConif, ivDecid, ivGrass, &
                           ncolumnsDataOutSUEWS, ncolumnsDataOutSnow, &
                           ncolumnsDataOutESTM, ncolumnsDataOutDailyState

   IMPLICIT NONE

CONTAINS

   ! a mini version of SUEWS
   SUBROUTINE SuMin( &
      SnowUse, EmissionsMethod, NetRadiationMethod, RoughLenHeatMethod, & ! model options
      RoughLenMomMethod, StorageHeatMethod, AerodynamicResistanceMethod, LAIType, SnowPackLimit, &
      OHM_threshSW, OHM_threshWD, th, tl, Kmax, g1, g2, g3, g4, g5, g6, s1, s2, LaiPower, &
      CRWmax, &
      CRWmin, &
      PrecipLimit, &
      PrecipLimitAlb, &
      RadMeltFact, &
      SnowAlbMax, &
      SnowAlbMin, &
      SnowDensMax, &
      SnowDensMin, &
      SnowLimBldg, &
      SnowLimPaved, &
      tau_a, &
      tau_f, &
      tau_r, &
      TempMeltFact, &
      OHMIncQF, & ! model options
      iy, id, it, imin, isec, dt_since_start, tstep, tstep_prev, startDLS, endDLS, & ! time-related input
      alt, lat, lng, Z, timezone, SurfaceArea, sfr_surf, & ! site-specific geographical settings
      z0m_in, zdm_in, & ! roughness related settings
      alb, emis, SnowAlb, OHM_coef, WaterDist, & ! surface properties
      AHProf_24hr, HumActivity_24hr, PopProf_24hr, TraffProf_24hr, WUProfA_24hr, WUProfM_24hr, snowProf_24hr, &
           & ! hourly profile values
      qn_av, dqndt, qn_s_av, dqnsdt, & ! OHM related Qn quantities
      surf_var_id, DecidCap_id, albDecTr_id, albEveTr_id, albGrass_id, &
      NumCapita_id, &
      BaseT_id, &
      BaseTe_id, &
      GDDFull_id, &
      SDDFull_id, &
      LaiMin_id, &
      LaiMax_id, &
      MaxConductance_id, &
      FAIbldg_id, &
      FAIEveTree_id, &
      FAIDecTree_id, &
      bldgH_id, &
      EveTreeH_id, &
      DecTreeH_id, &
      AH_MIN_id, &
      AH_SLOPE_Cooling_id, &
      AH_SLOPE_Heating_id, &
      QF0_BEU_id, &
      Qf_A_id, &
      Qf_B_id, &
      Qf_C_id, &
      T_CRITIC_Cooling_id, &
      T_CRITIC_Heating_id, &
      TrafficRate_id, &
      surf_attr_MinStorCap_id, &
      surf_attr_DrainEquat_id, &
      surf_attr_DrainCoef1_id, &
      surf_attr_DrainCoef2_id, &
      surf_attr_MaxStorCap_id, &
      SoilStoreCap_id, &
      SoilDepth_id, &
      SatHydraulicConduct_id, &
      AlbMin_DecTr_id, &
      AlbMax_DecTr_id, &
      AlbMin_EveTr_id, &
      AlbMax_EveTr_id, &
      AlbMin_Grass_id, &
      AlbMax_Grass_id, &
      CapMin_dec_id, &
      CapMax_dec_id, &
      PorMin_dec_id, &
      PorMax_dec_id, &
      DRAINRT_id, &
      RAINCOVER_id, &
      RAINMAXRES_id, &
      FlowChange_id, &
      PipeCapacity_id, &
      RunoffToWater_id, &
      StateLimit_id, &
      WetThresh_id, &
      BaseT_HC_id, &
      Faut_id, &
      IrrFracConif_id, &
      IrrFracDecid_id, &
      IrrFracGrass_id, &
      DayWat_id, &
      DayWatPer_id, &
      PopDensDaytime_id, PopDensNighttime_id, &
      porosity_id, &
      GDD_id, HDD_id, LAI_id, WUDay_id, soilstore_id, state_id, SnowWater, &
      avkdn, avRh, avU1, Press_hPa, Temp_C, Precip, & ! forcing variables
      qn, qf, qs, qh, qe, qsfc, tsk, CHKLOWQ, &
      kdown_o, &
      kup_o, &
      ldown_o, &
      lup_o, &
      tsurf_o, smd_o, glw1d, cldfra1d) !output

      ! model configurations
      INTEGER, INTENT(in) :: SnowUse
      INTEGER, INTENT(in) :: EmissionsMethod
      INTEGER, INTENT(in) :: NetRadiationMethod
      INTEGER, INTENT(IN) :: RoughLenHeatMethod
      INTEGER, INTENT(IN) :: RoughLenMomMethod
      INTEGER, INTENT(IN) :: StorageHeatMethod
      INTEGER, INTENT(IN) :: AerodynamicResistanceMethod
      INTEGER, DIMENSION(3), INTENT(IN) :: LAIType
      REAL(KIND(1D0)), DIMENSION(7 + 1), INTENT(IN) :: SnowPackLimit
      REAL(KIND(1D0)), DIMENSION(7 + 1), INTENT(IN) :: OHM_threshSW
      REAL(KIND(1D0)), DIMENSION(7 + 1), INTENT(IN) :: OHM_threshWD
      REAL(KIND(1D0)), INTENT(IN) :: th
      REAL(KIND(1D0)), INTENT(IN) :: tl
      REAL(KIND(1D0)), INTENT(IN) :: Kmax
      REAL(KIND(1D0)), INTENT(IN) :: g1
      REAL(KIND(1D0)), INTENT(IN) :: g2
      REAL(KIND(1D0)), INTENT(IN) :: g3
      REAL(KIND(1D0)), INTENT(IN) :: g4
      REAL(KIND(1D0)), INTENT(IN) :: g5
      REAL(KIND(1D0)), INTENT(IN) :: g6
      REAL(KIND(1D0)), INTENT(IN) :: s1
      REAL(KIND(1D0)), INTENT(IN) :: s2
      REAL(KIND(1D0)), DIMENSION(4, 3), INTENT(IN) :: LaiPower
      REAL(KIND(1D0)), INTENT(IN) :: CRWmax
      REAL(KIND(1D0)), INTENT(IN) :: CRWmin
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimit
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimitAlb
      REAL(KIND(1D0)), INTENT(IN) :: RadMeltFact
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMin
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMax
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMin
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimBldg
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimPaved
      REAL(KIND(1D0)), INTENT(IN) :: tau_a
      REAL(KIND(1D0)), INTENT(IN) :: tau_f
      REAL(KIND(1D0)), INTENT(IN) :: tau_r
      REAL(KIND(1D0)), INTENT(IN) :: TempMeltFact

      INTEGER, INTENT(IN) :: OHMIncQF !OHM calculation uses Q* only (0) or Q*+QF (1)

      ! time-related input
      INTEGER, INTENT(IN) :: iy
      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: it
      INTEGER, INTENT(IN) :: imin
      INTEGER, INTENT(in) :: isec
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]
      INTEGER, INTENT(IN) :: tstep
      INTEGER, INTENT(IN) :: tstep_prev ! tstep size of the previous step
      INTEGER, INTENT(IN) :: startDLS ! start of daylight saving (inclusive)
      INTEGER, INTENT(IN) :: endDLS ! end of daylight saving (inclusive)

      ! site-specific geographical settings
      REAL(KIND(1D0)), INTENT(IN) :: alt
      REAL(KIND(1D0)), INTENT(IN) :: lat
      REAL(KIND(1D0)), INTENT(IN) :: lng
      REAL(KIND(1D0)), INTENT(IN) :: Z
      REAL(KIND(1D0)), INTENT(IN) :: timezone
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: sfr_surf

      ! roughness related settings
      REAL(KIND(1D0)), INTENT(in) :: z0m_in
      REAL(KIND(1D0)), INTENT(in) :: zdm_in

      ! radiation related settings:
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: alb
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: emis
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowAlb

      ! OHM coeffcients
      REAL(KIND(1D0)), DIMENSION(7 + 1, 4, 3), INTENT(IN) :: OHM_coef

      ! hydrology related settings
      REAL(KIND(1D0)), DIMENSION(7 + 1, 7 - 1), INTENT(IN) :: WaterDist

      ! profiles at 24 hours
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: AHProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: HumActivity_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: PopProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: TraffProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: WUProfA_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: WUProfM_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: snowProf_24hr
      ! daily states, also initial conditions

      REAL(KIND(1D0)), INTENT(INOUT) :: qn_av
      REAL(KIND(1D0)), INTENT(INOUT) :: dqndt
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_s_av !qn_av for snow
      REAL(KIND(1D0)), INTENT(INOUT) :: dqnsdt !dqndt for snow
      REAL(KIND(1D0)), INTENT(INOUT) :: DecidCap_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albDecTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albEveTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albGrass_id
      REAL(KIND(1D0)), INTENT(INOUT) :: NumCapita_id

      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: BaseT_id
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: BaseTe_id
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: GDDFull_id
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: SDDFull_id
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: LaiMin_id
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: LaiMax_id
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: MaxConductance_id
      REAL(KIND(1D0)), INTENT(INOUT) :: FAIbldg_id
      REAL(KIND(1D0)), INTENT(INOUT) :: FAIEveTree_id
      REAL(KIND(1D0)), INTENT(INOUT) :: FAIDecTree_id
      REAL(KIND(1D0)), INTENT(INOUT) :: bldgH_id
      REAL(KIND(1D0)), INTENT(INOUT) :: EveTreeH_id
      REAL(KIND(1D0)), INTENT(INOUT) :: DecTreeH_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: AH_MIN_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: AH_SLOPE_Cooling_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: AH_SLOPE_Heating_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: QF0_BEU_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: Qf_A_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: Qf_B_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: Qf_C_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: T_CRITIC_Cooling_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: T_CRITIC_Heating_id
      REAL(KIND(1D0)), DIMENSION(2), INTENT(INOUT) :: TrafficRate_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: surf_attr_MinStorCap_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: surf_attr_DrainEquat_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: surf_attr_DrainCoef1_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: surf_attr_DrainCoef2_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: surf_attr_MaxStorCap_id

      REAL(KIND(1D0)), INTENT(INOUT) :: PopDensDaytime_id
      REAL(KIND(1D0)), INTENT(INOUT) :: PopDensNighttime_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: SoilStoreCap_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: SoilDepth_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: SatHydraulicConduct_id
      REAL(KIND(1D0)), INTENT(INOUT) :: AlbMin_DecTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: AlbMax_DecTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: AlbMin_EveTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: AlbMax_EveTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: AlbMin_Grass_id
      REAL(KIND(1D0)), INTENT(INOUT) :: AlbMax_Grass_id
      REAL(KIND(1D0)), INTENT(INOUT) :: CapMin_dec_id
      REAL(KIND(1D0)), INTENT(INOUT) :: CapMax_dec_id
      REAL(KIND(1D0)), INTENT(INOUT) :: PorMin_dec_id
      REAL(KIND(1D0)), INTENT(INOUT) :: PorMax_dec_id
      REAL(KIND(1D0)), INTENT(INOUT) :: DRAINRT_id
      REAL(KIND(1D0)), INTENT(INOUT) :: RAINCOVER_id
      REAL(KIND(1D0)), INTENT(INOUT) :: RAINMAXRES_id
      REAL(KIND(1D0)), INTENT(INOUT) :: FlowChange_id
      REAL(KIND(1D0)), INTENT(INOUT) :: PipeCapacity_id
      REAL(KIND(1D0)), INTENT(INOUT) :: RunoffToWater_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: StateLimit_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: WetThresh_id
      REAL(KIND(1D0)), INTENT(INOUT) :: BaseT_HC_id
      REAL(KIND(1D0)), INTENT(INOUT) :: Faut_id
      REAL(KIND(1D0)), INTENT(INOUT) :: IrrFracConif_id
      REAL(KIND(1D0)), INTENT(INOUT) :: IrrFracDecid_id
      REAL(KIND(1D0)), INTENT(INOUT) :: IrrFracGrass_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: DayWat_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: DayWatPer_id
      REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id
      REAL(KIND(1D0)), DIMENSION(5), INTENT(INOUT) :: GDD_id !Growing Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating/Cooling Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: LAI_id !LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(9), INTENT(INOUT) :: WUDay_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: soilstore_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: state_id
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: surf_var_id !variable to store the current states
      REAL(KIND(1D0)), DIMENSION(7), INTENT(INOUT) :: SnowWater

      ! forcing variables
      REAL(KIND(1D0)), INTENT(IN) :: avkdn
      REAL(KIND(1D0)), INTENT(IN) :: glw1d
      REAL(KIND(1D0)), INTENT(IN) :: avRh
      REAL(KIND(1D0)), INTENT(IN) :: avU1
      REAL(KIND(1D0)), INTENT(IN) :: Press_hPa
      REAL(KIND(1D0)), INTENT(IN) :: Temp_C
      REAL(KIND(1D0)), INTENT(IN) :: Precip
      REAL(KIND(1D0)), INTENT(IN) :: cldfra1d
      ! output for WRF
      REAL(KIND(1D0)), INTENT(out) :: kdown_o
      REAL(KIND(1D0)), INTENT(out) :: kup_o
      REAL(KIND(1D0)), INTENT(out) :: ldown_o
      REAL(KIND(1D0)), INTENT(out) :: lup_o
      REAL(KIND(1D0)), INTENT(out) :: tsurf_o
      REAL(KIND(1D0)), INTENT(out) :: smd_o

      REAL(KIND(1D0)), INTENT(out) :: qn
      REAL(KIND(1D0)), INTENT(out) :: qf
      REAL(KIND(1D0)), INTENT(out) :: qs
      REAL(KIND(1D0)), INTENT(out) :: qh
      REAL(KIND(1D0)), INTENT(out) :: qe
      REAL(KIND(1D0)), INTENT(out) :: qsfc
      REAL(KIND(1D0)), INTENT(out) :: tsk
      REAL(KIND(1D0)), INTENT(out) :: CHKLOWQ

      ! fixed settings in SuMin
      INTEGER, PARAMETER :: veg_type = 1
      INTEGER, PARAMETER :: gsModel = 2
      INTEGER, PARAMETER :: StabilityMethod = 3
      INTEGER, PARAMETER :: SMDMethod = 0
      INTEGER, PARAMETER :: DiagQN = 0
      INTEGER, PARAMETER :: DiagQS = 0
      INTEGER, PARAMETER :: Diagnose = 0
      INTEGER, PARAMETER :: EvapMethod = 2
      INTEGER, PARAMETER :: LAICalcYes = 1
      INTEGER, PARAMETER :: WaterUseMethod = 0

      REAL(KIND(1D0)), PARAMETER :: LAI_obs = 0
      REAL(KIND(1D0)) :: ldown_obs
      REAL(KIND(1D0)) :: fcld_obs
      REAL(KIND(1D0)), PARAMETER :: snowFrac_obs = 0
      REAL(KIND(1D0)), PARAMETER :: qn1_obs = 0
      REAL(KIND(1D0)), PARAMETER :: qh_obs = 0
      REAL(KIND(1D0)), PARAMETER :: qf_obs = 0
      REAL(KIND(1D0)), PARAMETER :: qs_obs = 0

      ! local variables not used for WRF coupling
      INTEGER :: Gridiv
      INTEGER :: Ie_end
      INTEGER :: Ie_start
#ifdef wrf
      CHARACTER(len=1024) :: message ! Used to pass through function wrf_debug() by Zhenkun Li, 10/08/2018
#endif

      ! parameters used in SUEWS for now:
      !REAL(KIND(1d0)), DIMENSION(7), PARAMETER ::SoilStoreCap = [150., 150., 150., 150., 150., 150., 0.] !Capacity of soil store for each surface [mm]
      !REAL(KIND(1D0)), DIMENSION(7), PARAMETER ::SoilDepth = 350                                      !Depth of sub-surface soil store for each surface [mm]
      !REAL(KIND(1D0)), DIMENSION(7), PARAMETER ::SatHydraulicConduct = 5E-4                                     !Saturated hydraulic conductivity for each soil subsurface [mm s-1]

      !REAL(KIND(1d0)), PARAMETER:: AlbMin_DecTr = 0.12   !Min albedo for deciduous trees [-]
      !REAL(KIND(1d0)), PARAMETER:: AlbMax_DecTr = 0.18   !Max albedo for deciduous trees [-]
      !REAL(KIND(1d0)), PARAMETER:: AlbMin_EveTr = 0.11   !Min albedo for evergreen trees [-]
      !REAL(KIND(1d0)), PARAMETER:: AlbMax_EveTr = 0.12   !Max albedo for evergreen trees [-]
      !REAL(KIND(1d0)), PARAMETER:: AlbMin_Grass = 0.18   !Min albedo for grass [-]
      !REAL(KIND(1d0)), PARAMETER:: AlbMax_Grass = 0.21    !Max albedo for grass [-]

      !REAL(KIND(1d0)), PARAMETER:: CapMin_dec = 0.3   !Min storage capacity for deciduous trees [mm] (from input information)
      !REAL(KIND(1d0)), PARAMETER:: CapMax_dec = 0.8   !Max storage capacity for deciduous trees [mm] (from input information)
      !REAL(KIND(1d0)), PARAMETER:: PorMin_dec = 0.2   !Min porosity for deciduous trees
      !REAL(KIND(1d0)), PARAMETER:: PorMax_dec = 0.6   !Max porosity for deciduous trees

      !REAL(KIND(1d0)), PARAMETER:: FAIbldg = 0. !Frontal area fraction of buildings
      !REAL(KIND(1d0)), PARAMETER:: FAIEveTree = 0. !Frontal area fraction of evergreen trees
      !REAL(KIND(1d0)), PARAMETER:: FAIDecTree = 0. !Frontal area fraction of deciduous trees

      !REAL(KIND(1d0)), PARAMETER :: bldgH = 10 !Mean building height
      !REAL(KIND(1d0)), PARAMETER :: EveTreeH = 10 !Height of evergreen trees
      !REAL(KIND(1d0)), PARAMETER :: DecTreeH = 10 !Height of deciduous trees
      !grid base
      !REAL(KIND(1d0)), DIMENSION(3), PARAMETER:: BaseT = [5, 5, 5]          !Base temperature for growing degree days [degC]
      !REAL(KIND(1d0)), DIMENSION(3), PARAMETER:: BaseTe = [11, 11, 11]       !Base temperature for senescence degree days [degC]
      !REAL(KIND(1d0)), DIMENSION(3), PARAMETER:: GDDFull = [300, 300, 300]    !Growing degree days needed for full capacity [degC]
      !REAL(KIND(1d0)), DIMENSION(3), PARAMETER:: SDDFull = [-450, -450, -450] !Senescence degree days needed to initiate leaf off [degC]
      !REAL(KIND(1d0)), DIMENSION(3), PARAMETER:: LaiMin = [4., 1., 1.6]      !Min LAI [m2 m-2]
      !REAL(KIND(1d0)), DIMENSION(3), PARAMETER:: LaiMax = [5.1, 5.5, 5.9]    !Max LAI [m2 m-2]
      !REAL(KIND(1d0)), DIMENSION(3), PARAMETER:: MaxConductance = [7.4, 11.7, 30.1]  !Max conductance [mm s-1]
      !namelist
      ! REAL(KIND(1d0)), DIMENSION(4, 3), PARAMETER:: LaiPower = RESHAPE( & !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off
      !                                               [[0.03, 0.03, 0.03], &
      !                                                [0.0005, 0.0005, 0.0005], &
      !                                                [0.03, 0.03, 0.03], &
      !                                                [0.0005, 0.0005, 0.0005]], &
      !                                               [4, 3], order=[2, 1])
      !namelist
      !INTEGER, DIMENSION(3), PARAMETER:: LAIType = 0     !LAI equation to use: original (0) or new (1)

      !REAL(KIND(1D0)), PARAMETER ::DRAINRT = 0.25 !Drainage rate of the water bucket [mm hr-1]
      !REAL(KIND(1D0)), PARAMETER ::RAINCOVER = 1
      !REAL(KIND(1D0)), PARAMETER ::RAINMAXRES = 10   !Maximum water bucket reservoir [mm]
      !REAL(KIND(1d0)), PARAMETER ::FlowChange = 0    !Difference between the input and output flow in the water body
      !REAL(KIND(1d0)), PARAMETER ::PipeCapacity = 100  !Capacity of pipes to transfer water
      !REAL(KIND(1d0)), PARAMETER ::RunoffToWater = 0.1  !Fraction of surface runoff going to water body

      !REAL(KIND(1d0)), DIMENSION(7), PARAMETER:: StateLimit = [0.48, 0.25, 1.3, 0.8, 1.9, 1.0, 30000.] !Limit for state of each surface type [mm] (specified in input files)
      !REAL(KIND(1d0)), DIMENSION(7), PARAMETER:: WetThresh = [0.48, 0.25, 1.3, 0.8, 1.9, 1., 0.5]     !When State > WetThresh, rs=0 limit in SUEWS_evap [mm] (specified in input files)

      ! ---- Drainage characteristics ----------------------------------------------------------------
      ! 1 - min storage capacity [mm]
      ! 2 - Drainage equation to use
      ! 3 - Drainage coeff 1 [units depend on choice of eqn]
      ! 4 - Drainage coeff 2 [units depend on choice of eqn]
      ! 5 - max storage capacity [mm]
      !REAL(KIND(1d0)), DIMENSION(5, 7), PARAMETER:: surf_attr = RESHAPE( & ! variable to store the above five properties
      !                                              [[0.48, 0.25, 1.3, 0.3, 1.9, 0.8, 0.5], &
      !                                               [3., 3., 2., 2., 2., 3., 0.], &
      !                                               [10., 10., 0.013, 0.013, 0.013, 10., 0.], &
      !                                               [3., 3., 1.71, 1.71, 1.71, 3., 0.], &
      !                                               [0.48, 0.25, 1.3, 0.8, 1.9, 0.8, 0.5]], &
      !                                              [5, 7], order=[2, 1])

      !REAL(KIND(1d0)), DIMENSION(1, 7), PARAMETER:: surf_attr_MinStorCap = [ 0.48, 0.25, 1.3, 0.3, 1.9, 0.8, 0.5]
      !REAL(KIND(1d0)), DIMENSION(1, 7), PARAMETER:: surf_attr_DrainEquat = [3., 3., 2., 2., 2., 3., 0.]
      !REAL(KIND(1d0)), DIMENSION(1, 7), PARAMETER:: surf_attr_DrainCoef1 = [10., 10., 0.013, 0.013, 0.013, 10., 0.]
      !REAL(KIND(1d0)), DIMENSION(1, 7), PARAMETER:: surf_attr_DrainCoef2 = [3., 3., 1.71, 1.71, 1.71, 3., 0.]
      !REAL(KIND(1d0)), DIMENSION(1, 7), PARAMETER:: surf_attr_MaxStorCap = [0.48, 0.25, 1.3, 0.8, 1.9, 0.8, 0.5]

      !namelist
      ! these will be assigned locally as data
      ! use gsModel=2 as in Ward et al. (2016)
      !REAL(KIND(1d0)), PARAMETER::th = 55   !Maximum temperature limit
      !REAL(KIND(1d0)), PARAMETER::tl = -10  !Minimum temperature limit
      !REAL(KIND(1d0)), PARAMETER::Kmax = 1200 !Annual maximum hourly solar radiation
      !REAL(KIND(1d0)), PARAMETER::g1 = 3.5  !Fitted parameter
      !REAL(KIND(1d0)), PARAMETER::g2 = 200  !Fitted parameter
      !REAL(KIND(1d0)), PARAMETER::g3 = 0.13 !Fitted parameter
      !REAL(KIND(1d0)), PARAMETER::g4 = 0.7  !Fitted parameter
      !REAL(KIND(1d0)), PARAMETER::g5 = 30   !Fitted parameter
      !REAL(KIND(1d0)), PARAMETER::g6 = 0.05 !Fitted parameter
      !REAL(KIND(1d0)), PARAMETER::s1 = 5.56 !Fitted parameter
      !REAL(KIND(1d0)), PARAMETER::s2 = 0    !surface res. calculations
      !namelist
      !REAL(KIND(1d0)), DIMENSION(7 + 1), PARAMETER:: OHM_threshSW = [10, 10, 10, 10, 10, 10, 10, 10]         !Arrays for OHM thresholds
      !REAL(KIND(1d0)), DIMENSION(7 + 1), PARAMETER:: OHM_threshWD = [0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9] !Arrays for OHM thresholds

      !REAL(KIND(1d0)), PARAMETER::  BaseT_HC = 18.9  !Base temperature for QF
      !not used
      REAL(KIND(1D0)), PARAMETER :: xsmd = 0. !Measured soil moisture deficit
      !Todo
      REAL(KIND(1D0)), PARAMETER :: wu_m3 = 0 !External water use
      !REAL(KIND(1D0)), PARAMETER::Faut = 0  !Fraction of irrigated area using automatic irrigation
      REAL(KIND(1D0)), PARAMETER :: InternalWaterUse_h = 0 !Internal water use [mm h-1]
      !REAL(KIND(1D0)), PARAMETER::IrrFracEveTr = 0 !Fraction of evergreen trees which are irrigated
      !REAL(KIND(1D0)), PARAMETER::IrrFracDecTr = 0 !Fraction of deciduous trees which are irrigated
      !REAL(KIND(1D0)), PARAMETER::IrrFracGrass = 0 !Fraction of grass which is irrigated
      !Todo
      !REAL(KIND(1D0)), DIMENSION(7), PARAMETER ::DayWat = 0                     !Days of watering allowed
      !REAL(KIND(1D0)), DIMENSION(7), PARAMETER ::DayWatPer = 0                     !% of houses following daily water
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: Ie_a = [-84.535, 9.959, 3.674] !Coefficients for automatic irrigation models
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: Ie_m = [-25.36, 2.988, 1.102] !Coefficients for manual irrigation models

      ! local variables
      REAL(KIND(1D0)), PARAMETER :: NARP_EMIS_SNOW = 0.9 !NARP-specific parameters
      REAL(KIND(1D0)) :: NARP_TRANS_SITE !NARP-specific parameters QUESTION: not used by SUEWS?

      !REAL(KIND(1D0)), PARAMETER::NumCapita = 0 !Number of people in the study area per hectare [ha-1]
      !REAL(KIND(1D0)), PARAMETER::PopDensDaytime = 0 ! Daytime population density [ha-1] (i.e. workers)
      !REAL(KIND(1D0)), PARAMETER::PopDensNighttime = 0 ! Nighttime population density [ha-1] (i.e. residents)

      ! snow related local variables
      ! REAL(KIND(1D0)), PARAMETER                   ::CRWmax = 0.2   !Free water holding capacity of shallow SnowPack
      ! REAL(KIND(1D0)), PARAMETER                   ::CRWmin = 0.05  !Free water holding capacity of deep SnowPack
      ! REAL(KIND(1D0)), PARAMETER                   ::PrecipLimit = 2.2   !Temperature limit when precipitation occurs as snow
      ! REAL(KIND(1D0)), PARAMETER                   ::PrecipLimitAlb = 2     !Precipitation limit for albedo change (in mm)
      ! REAL(KIND(1D0)), PARAMETER                   ::RadMeltFact = 0.001 !Radiation melt factor
      ! REAL(KIND(1D0)), PARAMETER                   ::SnowAlbMax = 0.8   !Maximum snow albedo
      ! REAL(KIND(1D0)), PARAMETER                   ::SnowAlbMin = 0.18 !Minimum snow albedo
      ! REAL(KIND(1D0)), PARAMETER                   ::SnowDensMax = 450   !Minimum density of snow
      ! REAL(KIND(1D0)), PARAMETER                   ::SnowDensMin = 100   !Maximum density of snow
      ! REAL(KIND(1D0)), PARAMETER                   ::SnowLimBldg = 100   !Snow removal limits for roofs in mm)
      ! REAL(KIND(1D0)), PARAMETER                   ::SnowLimPaved = 100   !Snow removal limits for paved surfaces in mm)
      ! REAL(KIND(1D0)), PARAMETER                   ::tau_a = 0.01  !Time constans related to albedo change
      ! REAL(KIND(1D0)), PARAMETER                   ::tau_f = 0.1   !Time constans related to albedo change
      ! REAL(KIND(1D0)), PARAMETER                   ::tau_r = 0.02  !Time constans related to albedo change
      ! REAL(KIND(1D0)), PARAMETER                   ::TempMeltFact = 0.12  !Temperature melt factor
      ! REAL(KIND(1D0)), DIMENSION(7), PARAMETER      ::SnowPackLimit = 0
      ! REAL(KIND(1D0)), DIMENSION(0:23, 2), PARAMETER::snowProf_24hr = 0     ! Timing of snow removal (0 or 1) Hourly, WD/WE

      !grid level
      ! Anthropogenic heat related variables
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::AH_MIN = 10!Minimum anthropogenic heat flux (AnthropHeatMethod = 1)
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::AH_SLOPE_Cooling = [2.7, 2.7]!Slope of the antrhropogenic heat flux calculation (AnthropHeatMethod = 1)
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::AH_SLOPE_Heating = [2.7, 2.7]!Slope of the antrhropogenic heat flux calculation (AnthropHeatMethod = 1)
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::QF0_BEU = [0.7442, 0.7955]
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::Qf_A = [0.1, 0.1]!Qf coefficients
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::Qf_B = [0.00986, 0.00986]!Qf coefficients
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::Qf_C = [0.0102, 0.0102]!Qf coefficients
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::BaseT_Cooling = [7, 7] !Critical temperature
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::BaseT_Heating = [7, 7] !Critical temperature
      !REAL(KIND(1D0)), DIMENSION(2), PARAMETER ::TrafficRate = [0.0134, 0.0095]
      !keep them here
      REAL(KIND(1D0)), PARAMETER :: EF_umolCO2perJ = 1.159
      REAL(KIND(1D0)), PARAMETER :: EnEF_v_Jkm = 4E6
      REAL(KIND(1D0)), PARAMETER :: FcEF_v_kgkm = 0.285
      REAL(KIND(1D0)), PARAMETER :: FrFossilFuel_Heat = 0.05
      REAL(KIND(1D0)), PARAMETER :: FrFossilFuel_NonHeat = 0
      REAL(KIND(1D0)), PARAMETER :: TrafficUnits = 1
      REAL(KIND(1D0)), PARAMETER :: MaxQFMetab = 175
      REAL(KIND(1D0)), PARAMETER :: MinQFMetab = 75

      ! AnOHM related: not used
      REAL(KIND(1D0)), DIMENSION(7), PARAMETER :: chAnOHM = 3 ! bulk transfer coef., added by TS AnOHM
      REAL(KIND(1D0)), DIMENSION(7), PARAMETER :: cpAnOHM = 2E6 ! heat capacity, added by TS AnOHM
      REAL(KIND(1D0)), DIMENSION(7), PARAMETER :: kkAnOHM = 1.2 ! heat conductivity, added by TS AnOHM
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: MetForcingData_grid

      !Biogenic CO2 related parameters
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: alpha_bioCO2 = 0.005
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: alpha_enh_bioCO2 = 0.016
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: beta_bioCO2 = 8.747
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: beta_enh_bioCO2 = 33.454
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: min_res_bioCO2 = 0.6
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: resp_a = 2.43
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: resp_b = 0
      REAL(KIND(1D0)), DIMENSION(3), PARAMETER :: theta_bioCO2 = 0.96

      ! ESTM related variables, not used
      REAL(KIND(1D0)), DIMENSION(24*3600/tstep) :: Tair24HR
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Ts5mindata_ir !TODO:allocatable array can't serve as argument?

      REAL(KIND(1D0)) :: SnowfallCum = 0 !Cumulative snowfall
      REAL(KIND(1D0)), DIMENSION(7) :: IceFrac = 0.2 !Estimated fraction of ice. Should be improved in the future
      REAL(KIND(1D0)), DIMENSION(7) :: SnowDens = 300 !Density of snow
      REAL(KIND(1D0)), DIMENSION(7) :: SnowFrac = 0 !!Surface fraction of snow cover
      REAL(KIND(1D0)), DIMENSION(7) :: SnowPack = 0 !Amount of snow on each surface in mm

      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5) :: DailyStateLine

      ! drainage related parameters
      REAL(KIND(1D0)), DIMENSION(6, 7) :: StoreDrainPrm
      StoreDrainPrm(1, :) = surf_attr_MinStorCap_id
      StoreDrainPrm(2, :) = surf_attr_DrainEquat_id
      StoreDrainPrm(3, :) = surf_attr_DrainCoef1_id
      StoreDrainPrm(4, :) = surf_attr_DrainCoef2_id
      StoreDrainPrm(5, :) = surf_attr_MaxStorCap_id

      StoreDrainPrm(6, :) = surf_var_id
      ldown_obs = glw1d
      fcld_obs = cldfra1d
      ! PRINT*,''
      ! PRINT*, 'soilstore_id',soilstore_id
      ! soilstore_id=MERGE(soilstore_id,soilstore_id*0,soilstore_id>0)
      ! print*, 'soilstore_id modified',soilstore_id
      ! PRINT*, 'state_id',state_id
#ifdef wrf
      WRITE (message, *) 'in SuMin, before calculation, OHM_coef:', OHM_coef(1, :, :)
      CALL wrf_debug(100, message)
#endif

      CALL SUEWS_cal_Main( &
         AerodynamicResistanceMethod, AH_MIN_id, AHProf_24hr, AH_SLOPE_Cooling_id, & ! input&inout in alphabetical order
         AH_SLOPE_Heating_id, &
         alb, AlbMax_DecTr_id, AlbMax_EveTr_id, AlbMax_Grass_id, &
         AlbMin_DecTr_id, AlbMin_EveTr_id, AlbMin_Grass_id, &
         alpha_bioCO2, alpha_enh_bioCO2, alt, avkdn, avRh, avU1, BaseT_id, BaseTe_id, &
         BaseT_HC_id, beta_bioCO2, beta_enh_bioCO2, bldgH_id, CapMax_dec_id, CapMin_dec_id, &
         chAnOHM, cpAnOHM, CRWmax, CRWmin, DayWat_id, DayWatPer_id, &
         DecTreeH_id, Diagnose, DiagQN, DiagQS, DRAINRT_id, &
         dt_since_start, dqndt, qn_av, dqnsdt, qn_s_av, &
         EF_umolCO2perJ, emis, EmissionsMethod, EnEF_v_Jkm, endDLS, EveTreeH_id, FAIBldg_id, &
         FAIDecTree_id, FAIEveTree_id, Faut_id, FcEF_v_kgkm, fcld_obs, FlowChange_id, &
         FrFossilFuel_Heat, FrFossilFuel_NonHeat, G1, G2, G3, G4, G5, G6, GDD_id, &
         GDDFull_id, Gridiv, gsModel, HDD_id, HumActivity_24hr, &
         IceFrac, id, Ie_a, Ie_end, Ie_m, Ie_start, imin, &
         InternalWaterUse_h, IrrFracConif_id, IrrFracDecid_id, IrrFracGrass_id, isec, it, EvapMethod, &
         iy, kkAnOHM, Kmax, LAI_id, LAICalcYes, LAIMax_id, LAIMin_id, LAI_obs, &
         LAIPower, LAIType, lat, ldown_obs, lng, MaxConductance_id, MaxQFMetab, &
         SnowWater, MetForcingData_grid, MinQFMetab, min_res_bioCO2, &
         NARP_EMIS_SNOW, NARP_TRANS_SITE, NetRadiationMethod, &
         NumCapita_id, OHM_coef, OHMIncQF, OHM_threshSW, &
         OHM_threshWD, PipeCapacity_id, PopDensDaytime_id, &
         PopDensNighttime_id, PopProf_24hr, PorMax_dec_id, PorMin_dec_id, &
         Precip, PrecipLimit, PrecipLimitAlb, Press_hPa, &
         QF0_BEU_id, Qf_A_id, Qf_B_id, Qf_C_id, &
         qn1_obs, qh_obs, qs_obs, qf_obs, &
         RadMeltFact, RAINCOVER_id, RainMaxRes_id, resp_a, resp_b, &
         RoughLenHeatMethod, RoughLenMomMethod, RunoffToWater_id, S1, S2, &
         SatHydraulicConduct_id, SDDFull_id, sfr_surf, SMDMethod, SnowAlb, SnowAlbMax, &
         SnowAlbMin, SnowPackLimit, SnowDens, SnowDensMax, SnowDensMin, SnowfallCum, SnowFrac, &
         SnowLimBldg, SnowLimPaved, snowFrac_obs, SnowPack, SnowProf_24hr, SnowUse, SoilDepth_id, &
         soilstore_id, SoilStoreCap_id, StabilityMethod, startDLS, state_id, StateLimit_id, &
         StorageHeatMethod, StoreDrainPrm, SurfaceArea, Tair24HR, tau_a, tau_f, tau_r, &
         T_CRITIC_Cooling_id, T_CRITIC_Heating_id, Temp_C, TempMeltFact, TH, &
         theta_bioCO2, timezone, TL, TrafficRate_id, TrafficUnits, &
         TraffProf_24hr, Ts5mindata_ir, tstep, tstep_prev, veg_type, &
         WaterDist, WaterUseMethod, WetThresh_id, wu_m3, &
         WUDay_id, DecidCap_id, albDecTr_id, albEveTr_id, albGrass_id, porosity_id, &
         WUProfA_24hr, WUProfM_24hr, xsmd, Z, z0m_in, zdm_in, &
         datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, & !output
         DailyStateLine) !output

      surf_var_id = StoreDrainPrm(6, :) ! update surf_var_id

      kdown_o = dataOutLineSUEWS(1)
      kup_o = dataOutLineSUEWS(2)
      ldown_o = dataOutLineSUEWS(3)
      lup_o = dataOutLineSUEWS(4)
      tsurf_o = dataOutLineSUEWS(5)
      smd_o = dataOutLineSUEWS(23)

      qn = dataOutLineSUEWS(6)
      qf = dataOutLineSUEWS(7)
      qs = dataOutLineSUEWS(8)
      qh = dataOutLineSUEWS(9)
      qe = dataOutLineSUEWS(10)
      qsfc = dataOutLineSUEWS(16)
      tsk = dataOutLineSUEWS(77) + 273.15
      CHKLOWQ = 1

      ! PRINT*,''
      ! PRINT*, 'avkdn,kup,ldown,lup,tsurf'
      ! PRINT*, dataOutLineSUEWS(1:5)
      ! PRINT*, 'qn1,qf,qs,qh,qe'
      ! PRINT*, dataOutLineSUEWS(6:10)
      ! PRINT*,''
      ! IF ( ABS(qe)>1000 ) THEN
      !    zdm_in=0.
      !    PRINT*, 10./zdm_in
      ! END IF
#ifdef wrf
      WRITE (message, *) ' in SuMin, after calculation, OHM_coef:', OHM_coef(1, :, :)
      CALL wrf_debug(100, message)

      WRITE (message, *) ' in SuMin, qn,qf,qs,qh,qe:', dataOutLineSUEWS(6:10)
      CALL wrf_debug(100, message)
#endif

   END SUBROUTINE SuMin

END MODULE SuMin_Module
