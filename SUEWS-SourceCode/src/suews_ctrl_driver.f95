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
                            cal_z0V, SUEWS_cal_RoughnessParameters
   USE ESTM_module, ONLY: ESTM, ESTM_ext
   USE Snow_module, ONLY: SnowCalc, Snow_cal_MeltHeat, SnowUpdate, update_snow_albedo, update_snow_dens
   USE DailyState_module, ONLY: SUEWS_cal_DailyState, update_DailyStateLine
   USE WaterDist_module, ONLY: &
      drainage, cal_water_storage_surf, &
      cal_water_storage_building, &
      SUEWS_cal_SoilState, SUEWS_update_SoilMoist, &
      ReDistributeWater, SUEWS_cal_HorizontalSoilWater, &
      SUEWS_cal_WaterUse
   USE ctrl_output, ONLY: varListAll
   USE DailyState_module, ONLY: SUEWS_update_DailyState
   USE lumps_module, ONLY: LUMPS_cal_QHQE
   USE evap_module, ONLY: cal_evap_multi
   USE rsl_module, ONLY: RSLProfile
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

   IMPLICIT NONE

CONTAINS
   ! ===================MAIN CALCULATION WRAPPER FOR ENERGY AND WATER FLUX===========
   SUBROUTINE SUEWS_cal_Main( &
      AerodynamicResistanceMethod, AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, & ! input&inout in alphabetical order
      AH_SLOPE_Heating, &
      alb, AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
      AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
      alpha_bioCO2, alpha_enh_bioCO2, alt, kdown, avRh, avU1, BaseT, BaseTe, &
      BaseTMethod, &
      BaseT_HC, beta_bioCO2, beta_enh_bioCO2, bldgH, CapMax_dec, CapMin_dec, &
      chAnOHM, CO2PointSource, cpAnOHM, CRWmax, CRWmin, DayWat, DayWatPer, &
      DecTreeH, DiagMethod, Diagnose, DiagQN, DiagQS, DRAINRT, &
      dt_since_start, dqndt, qn_av, dqnsdt, qn_s_av, &
      EF_umolCO2perJ, emis, EmissionsMethod, EnEF_v_Jkm, endDLS, EveTreeH, FAIBldg, &
      FAIDecTree, FAIEveTree, Faut, FcEF_v_kgkm, fcld_obs, FlowChange, &
      FrFossilFuel_Heat, FrFossilFuel_NonHeat, G1, G2, G3, G4, G5, G6, GDD_id, &
      GDDFull, Gridiv, gsModel, H_maintain, HDD_id, HumActivity_24hr, &
      IceFrac, id, Ie_a, Ie_end, Ie_m, Ie_start, imin, &
      InternalWaterUse_h, &
      IrrFracPaved, IrrFracBldgs, &
      IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
      IrrFracBSoil, IrrFracWater, &
      isec, it, EvapMethod, &
      iy, kkAnOHM, Kmax, LAI_id, LAICalcYes, LAIMax, LAIMin, LAI_obs, &
      LAIPower, LAIType, lat, lenDay_id, ldown_obs, lng, MaxConductance, MaxFCMetab, MaxQFMetab, &
      SnowWater, MetForcingData_grid, MinFCMetab, MinQFMetab, min_res_bioCO2, &
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
      sfr_roof, sfr_wall, sfr_surf, &
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
      datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, & !output
      dataOutLineBEERS, & !output
      dataOutLineDebug, dataOutLineSPARTACUS, &
      dataOutLineESTMExt, &
      DailyStateLine) !output

      IMPLICIT NONE

      ! ########################################################################################
      ! input variables
      INTEGER, INTENT(IN) :: AerodynamicResistanceMethod ! number of vertical layers in urban canyon(modify)
      INTEGER, INTENT(IN) :: BaseTMethod
      INTEGER, INTENT(IN) :: Diagnose
      INTEGER, INTENT(IN) :: DiagQN
      INTEGER, INTENT(IN) :: DiagQS
      INTEGER, INTENT(IN) :: startDLS
      INTEGER, INTENT(IN) :: endDLS
      INTEGER, INTENT(IN) :: EmissionsMethod
      INTEGER, INTENT(IN) :: Gridiv
      INTEGER, INTENT(IN) :: nlayer ! number of vertical layers in urban canyon
      INTEGER, INTENT(IN) :: gsModel
      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: Ie_end
      INTEGER, INTENT(IN) :: Ie_start
      INTEGER, INTENT(IN) :: isec
      INTEGER, INTENT(IN) :: imin
      INTEGER, INTENT(IN) :: it
      INTEGER, INTENT(IN) :: EvapMethod
      INTEGER, INTENT(IN) :: iy
      INTEGER, INTENT(IN) :: LAICalcYes
      INTEGER, INTENT(IN) :: NetRadiationMethod
      INTEGER, INTENT(IN) :: OHMIncQF
      INTEGER, INTENT(IN) :: RoughLenHeatMethod
      INTEGER, INTENT(IN) :: RoughLenMomMethod
      INTEGER, INTENT(IN) :: SMDMethod
      INTEGER, INTENT(IN) :: SnowUse
      INTEGER, INTENT(IN) :: StabilityMethod
      INTEGER, INTENT(IN) :: StorageHeatMethod
      INTEGER, INTENT(in) :: DiagMethod
      INTEGER, INTENT(IN) :: tstep
      INTEGER, INTENT(IN) :: tstep_prev ! tstep size of the previous step
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]
      INTEGER, INTENT(IN) :: veg_type
      INTEGER, INTENT(IN) :: WaterUseMethod

      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_DecTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_EveTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_Grass
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_DecTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_EveTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_Grass
      REAL(KIND(1D0)), INTENT(IN) :: alt
      REAL(KIND(1D0)), INTENT(IN) :: kdown
      REAL(KIND(1D0)), INTENT(IN) :: avRh
      REAL(KIND(1D0)), INTENT(IN) :: avU1
      REAL(KIND(1D0)), INTENT(IN) :: BaseT_HC
      REAL(KIND(1D0)), INTENT(IN) :: bldgH
      REAL(KIND(1D0)), INTENT(IN) :: CapMax_dec
      REAL(KIND(1D0)), INTENT(IN) :: CapMin_dec
      REAL(KIND(1D0)), INTENT(IN) :: CO2PointSource
      REAL(KIND(1D0)), INTENT(IN) :: CRWmax
      REAL(KIND(1D0)), INTENT(IN) :: CRWmin
      REAL(KIND(1D0)), INTENT(IN) :: DecTreeH
      REAL(KIND(1D0)), INTENT(IN) :: DRAINRT
      REAL(KIND(1D0)), INTENT(IN) :: EF_umolCO2perJ
      REAL(KIND(1D0)), INTENT(IN) :: EnEF_v_Jkm
      REAL(KIND(1D0)), INTENT(IN) :: EveTreeH
      REAL(KIND(1D0)), INTENT(IN) :: FAIBldg
      REAL(KIND(1D0)), INTENT(IN) :: FAIDecTree
      REAL(KIND(1D0)), INTENT(IN) :: FAIEveTree
      REAL(KIND(1D0)), INTENT(IN) :: Faut
      REAL(KIND(1D0)), INTENT(IN) :: fcld_obs
      REAL(KIND(1D0)), INTENT(IN) :: FlowChange
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_Heat
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_NonHeat
      REAL(KIND(1D0)), INTENT(IN) :: G1
      REAL(KIND(1D0)), INTENT(IN) :: G2
      REAL(KIND(1D0)), INTENT(IN) :: G3
      REAL(KIND(1D0)), INTENT(IN) :: G4
      REAL(KIND(1D0)), INTENT(IN) :: G5
      REAL(KIND(1D0)), INTENT(IN) :: G6
      REAL(KIND(1D0)), INTENT(IN) :: H_maintain
      REAL(KIND(1D0)), INTENT(IN) :: InternalWaterUse_h
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracPaved
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBldgs
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracEveTr
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracDecTr
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracGrass
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBSoil
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracWater
      REAL(KIND(1D0)), INTENT(IN) :: Kmax
      REAL(KIND(1D0)), INTENT(IN) :: LAI_obs
      REAL(KIND(1D0)), INTENT(IN) :: lat
      REAL(KIND(1D0)), INTENT(IN) :: ldown_obs
      REAL(KIND(1D0)), INTENT(IN) :: lng
      REAL(KIND(1D0)), INTENT(IN) :: MaxFCMetab
      REAL(KIND(1D0)), INTENT(IN) :: MaxQFMetab
      REAL(KIND(1D0)), INTENT(IN) :: MinFCMetab
      REAL(KIND(1D0)), INTENT(IN) :: MinQFMetab
      REAL(KIND(1D0)), INTENT(IN) :: NARP_EMIS_SNOW
      REAL(KIND(1D0)), INTENT(IN) :: NARP_TRANS_SITE
      REAL(KIND(1D0)), INTENT(IN) :: PipeCapacity
      REAL(KIND(1D0)), INTENT(IN) :: PopDensNighttime
      REAL(KIND(1D0)), INTENT(IN) :: PorMax_dec
      REAL(KIND(1D0)), INTENT(IN) :: PorMin_dec
      REAL(KIND(1D0)), INTENT(IN) :: Precip
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimit
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimitAlb
      REAL(KIND(1D0)), INTENT(IN) :: Press_hPa
      REAL(KIND(1D0)), INTENT(IN) :: qn1_obs
      REAL(KIND(1D0)), INTENT(IN) :: qs_obs
      REAL(KIND(1D0)), INTENT(IN) :: qf_obs
      REAL(KIND(1D0)), INTENT(IN) :: RadMeltFact
      REAL(KIND(1D0)), INTENT(IN) :: RAINCOVER
      REAL(KIND(1D0)), INTENT(IN) :: RainMaxRes
      REAL(KIND(1D0)), INTENT(IN) :: RunoffToWater
      REAL(KIND(1D0)), INTENT(IN) :: S1
      REAL(KIND(1D0)), INTENT(IN) :: S2
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMin
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMax
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMin
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimBldg
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimPaved
      REAL(KIND(1D0)), INTENT(IN) :: snowFrac_obs
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea
      REAL(KIND(1D0)), INTENT(IN) :: tau_a
      REAL(KIND(1D0)), INTENT(IN) :: tau_f
      REAL(KIND(1D0)), INTENT(IN) :: tau_r
      REAL(KIND(1D0)), INTENT(IN) :: Temp_C
      REAL(KIND(1D0)), INTENT(IN) :: TempMeltFact
      REAL(KIND(1D0)), INTENT(IN) :: TH
      REAL(KIND(1D0)), INTENT(IN) :: timezone
      REAL(KIND(1D0)), INTENT(IN) :: TL
      REAL(KIND(1D0)), INTENT(IN) :: TrafficUnits
      REAL(KIND(1D0)), INTENT(IN) :: wu_m3
      REAL(KIND(1D0)), INTENT(IN) :: xsmd
      REAL(KIND(1D0)), INTENT(IN) :: Z
      REAL(KIND(1D0)), INTENT(IN) :: z0m_in
      REAL(KIND(1D0)), INTENT(IN) :: zdm_in

      INTEGER, DIMENSION(NVEGSURF), INTENT(IN) :: LAIType

      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_MIN
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Cooling
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Heating
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: FcEF_v_kgkm
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: QF0_BEU
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_A
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_B
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_C
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: PopDensDaytime
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Cooling
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Heating
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: TrafficRate
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_a
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_m
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: MaxConductance
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWat
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWatPer
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshSW
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshWD
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: chAnOHM
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: cpAnOHM
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: emis
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: kkAnOHM
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SatHydraulicConduct
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SnowPackLimit
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilDepth
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilStoreCap_surf
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: StateLimit_surf
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: WetThresh_surf
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_enh_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseT
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseTe
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_enh_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: GDDFull
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMax
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMin
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: min_res_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_a
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_b
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: SDDFull
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: SnowProf_24hr
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: theta_bioCO2
      REAL(KIND(1D0)), DIMENSION(4, NVEGSURF), INTENT(IN) :: LAIPower
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, 4, 3), INTENT(IN) :: OHM_coef
      REAL(KIND(1D0)), DIMENSION(NSURF + 1, NSURF - 1), INTENT(IN) :: WaterDist
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: Ts5mindata_ir
      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(IN) :: MetForcingData_grid

      ! diurnal profile values for 24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: AHProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: HumActivity_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: PopProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: TraffProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfA_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfM_24hr

      ! ####################################################################################
      ! ESTM_EXT
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_roof

      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_wall

      ! ########################################################################################

      ! ########################################################################################
      ! inout variables
      ! OHM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_av
      REAL(KIND(1D0)), INTENT(INOUT) :: dqndt
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_s_av
      REAL(KIND(1D0)), INTENT(INOUT) :: dqnsdt

      ! snow related:
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowfallCum
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowAlb
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: IceFrac
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowWater
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowDens
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowFrac
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowPack

      ! water balance related:
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: soilstore_surf
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: state_surf
      REAL(KIND(1D0)), DIMENSION(6, NSURF), INTENT(INOUT) :: StoreDrainPrm

      ! phenology related:
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: alb
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: GDD_id !Growing Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INout) :: SDD_id !Senescence Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: LAI_id !LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), INTENT(INout) :: Tmin_id
      REAL(KIND(1D0)), INTENT(INout) :: Tmax_id
      REAL(KIND(1D0)), INTENT(INout) :: lenDay_id
      REAL(KIND(1D0)), INTENT(INOUT) :: DecidCap_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albDecTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albEveTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albGrass_id
      REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id

      ! anthropogenic heat related:
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating Degree Days (see SUEWS_DailyState.f95)

      ! water use related:
      REAL(KIND(1D0)), DIMENSION(9), INTENT(INOUT) :: WUDay_id

      ! ESTM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: Tair_av

      ! ESTM_ext related:
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(INOUT) :: temp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(INOUT) :: temp_wall
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(INOUT) :: temp_surf

      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(INOUT) :: tsfc_surf

      ! SPARTACUS input variables
      INTEGER, INTENT(IN) :: n_vegetation_region_urban, &
                             n_stream_sw_urban, n_stream_lw_urban
      REAL(KIND(1D0)), INTENT(IN) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
                                     veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                                     veg_fsd_const, veg_contact_fraction_const, &
                                     ground_albedo_dir_mult_fact

      ! ########################################################################################

      ! ########################################################################################
      ! output variables
      REAL(KIND(1D0)), DIMENSION(5), INTENT(OUT) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5), INTENT(OUT) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5), INTENT(OUT) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5), INTENT(OUT) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5), INTENT(OUT) :: dataOutLineESTMExt
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5), INTENT(OUT) :: dataoutLineRSL
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5), INTENT(OUT) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug - 5), INTENT(OUT) :: dataOutLineDebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5), INTENT(OUT) :: dataOutLineSPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5), INTENT(OUT) :: DailyStateLine
      ! ########################################################################################

      ! ########################################################################################
      ! local variables
      REAL(KIND(1D0)) :: a1
      REAL(KIND(1D0)) :: a2
      REAL(KIND(1D0)) :: a3
      REAL(KIND(1D0)) :: AdditionalWater
      REAL(KIND(1D0)) :: U10_ms
      REAL(KIND(1D0)) :: azimuth
      REAL(KIND(1D0)) :: chSnow_per_interval

      REAL(KIND(1D0)) :: dens_dry
      REAL(KIND(1D0)) :: deltaLAI
      REAL(KIND(1D0)) :: drain_per_tstep
      REAL(KIND(1D0)) :: Ea_hPa
      REAL(KIND(1D0)) :: QE_LUMPS
      REAL(KIND(1D0)) :: es_hPa
      REAL(KIND(1D0)) :: ev_per_tstep
      REAL(KIND(1D0)) :: wu_ext
      REAL(KIND(1D0)) :: Fc
      REAL(KIND(1D0)) :: Fc_anthro
      REAL(KIND(1D0)) :: Fc_biogen
      REAL(KIND(1D0)) :: Fc_build
      REAL(KIND(1D0)) :: fcld
      REAL(KIND(1D0)) :: Fc_metab
      REAL(KIND(1D0)) :: Fc_photo
      REAL(KIND(1D0)) :: Fc_point
      REAL(KIND(1D0)) :: Fc_respi
      REAL(KIND(1D0)) :: Fc_traff
      REAL(KIND(1D0)) :: gfunc
      REAL(KIND(1D0)) :: gsc
      REAL(KIND(1D0)) :: QH_LUMPS
      REAL(KIND(1D0)) :: wu_int
      REAL(KIND(1D0)) :: kclear
      REAL(KIND(1D0)) :: kup
      REAL(KIND(1D0)) :: ldown
      REAL(KIND(1D0)) :: lup
      REAL(KIND(1D0)) :: L_mod
      REAL(KIND(1D0)) :: mwh
      REAL(KIND(1D0)) :: mwstore
      REAL(KIND(1D0)) :: NWstate_per_tstep
      REAL(KIND(1D0)) :: FAI ! frontal area index
      REAL(KIND(1D0)) :: PAI ! plan area index
      REAL(KIND(1D0)) :: zL
      REAL(KIND(1D0)) :: q2_gkg
      REAL(KIND(1D0)) :: qe
      REAL(KIND(1D0)) :: qf
      REAL(KIND(1D0)) :: QF_SAHP
      REAL(KIND(1D0)) :: qh
      REAL(KIND(1D0)) :: qh_residual
      REAL(KIND(1D0)) :: qh_resist
      REAL(KIND(1D0)) :: Qm
      REAL(KIND(1D0)) :: QmFreez
      REAL(KIND(1D0)) :: QmRain
      REAL(KIND(1D0)) :: qn
      REAL(KIND(1D0)) :: qn_snow
      REAL(KIND(1D0)) :: qn_snowfree
      REAL(KIND(1D0)) :: qs
      REAL(KIND(1D0)) :: RA_h ! aerodynamic resistance
      REAL(KIND(1D0)) :: RS ! surface resistance
      REAL(KIND(1D0)), DIMENSION(NSURF) :: RSS_nsurf ! surface resistance adjusted by surface wetness state
      REAL(KIND(1D0)) :: RH2
      REAL(KIND(1D0)) :: runoffAGveg
      REAL(KIND(1D0)) :: runoffAGimpervious
      REAL(KIND(1D0)) :: runoff_per_tstep
      REAL(KIND(1D0)) :: runoffPipes
      REAL(KIND(1D0)) :: runoffSoil_per_tstep
      REAL(KIND(1D0)) :: runoffwaterbody
      REAL(KIND(1D0)) :: smd
      REAL(KIND(1D0)) :: SoilState
      REAL(KIND(1D0)) :: state_per_tstep
      REAL(KIND(1D0)) :: surf_chang_per_tstep
      REAL(KIND(1D0)) :: swe
      REAL(KIND(1D0)) :: t2_C
      REAL(KIND(1D0)) :: TSfc_C
      REAL(KIND(1D0)) :: TempVeg
      REAL(KIND(1D0)) :: tot_chang_per_tstep
      REAL(KIND(1D0)) :: TStar
      REAL(KIND(1D0)) :: tsurf
      REAL(KIND(1D0)) :: UStar
      REAL(KIND(1D0)) :: VPD_Pa
      REAL(KIND(1D0)) :: z0m
      REAL(KIND(1D0)) :: zdm
      REAL(KIND(1D0)) :: ZENITH_deg
      REAL(KIND(1D0)) :: zH

      REAL(KIND(1D0)), DIMENSION(2) :: SnowRemoval
      REAL(KIND(1D0)), DIMENSION(NSURF) :: wu_surf
      REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezMelt
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind_snow
      REAL(KIND(1D0)), DIMENSION(NSURF) :: mw_ind
      REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_freezState
      REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_melt
      REAL(KIND(1D0)), DIMENSION(NSURF) :: Qm_rain
      REAL(KIND(1D0)), DIMENSION(NSURF) :: qn_ind_snow
      REAL(KIND(1D0)), DIMENSION(NSURF) :: rainOnSnow
      REAL(KIND(1D0)), DIMENSION(NSURF) :: runoffSoil
      REAL(KIND(1D0)), DIMENSION(NSURF) :: smd_nsurf
      REAL(KIND(1D0)), DIMENSION(NSURF) :: snowDepth

      REAL(KIND(1D0)), DIMENSION(nsurf) :: Tsurf_ind_snow

      INTEGER, DIMENSION(NSURF) :: snowCalcSwitch
      INTEGER, DIMENSION(3) :: dayofWeek_id
      INTEGER :: DLS

      ! REAL(KIND(1D0))::avcp
      ! REAL(KIND(1D0))::avdens
      ! REAL(KIND(1D0))::lv_J_kg
      REAL(KIND(1D0)) :: dq
      REAL(KIND(1D0)) :: lvS_J_kg
      REAL(KIND(1D0)) :: psyc_hPa
      REAL(KIND(1D0)) :: z0v
      REAL(KIND(1D0)) :: z0vSnow
      REAL(KIND(1D0)) :: RAsnow
      REAL(KIND(1D0)) :: RB
      REAL(KIND(1D0)) :: runoff_per_interval
      REAL(KIND(1D0)) :: s_hPa
      REAL(KIND(1D0)) :: sIce_hpa
      REAL(KIND(1D0)) :: SoilMoistCap
      REAL(KIND(1D0)) :: veg_fr
      REAL(KIND(1D0)) :: VegPhenLumps
      REAL(KIND(1D0)) :: VPd_hpa
      REAL(KIND(1D0)) :: vsmd
      REAL(KIND(1D0)) :: ZZD

      REAL(KIND(1D0)), DIMENSION(NSURF) :: deltaQi
      REAL(KIND(1D0)), DIMENSION(NSURF) :: drain
      REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezState
      REAL(KIND(1D0)), DIMENSION(NSURF) :: FreezStateVol
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: soilstore_updated
      ! REAL(KIND(1D0)), DIMENSION(NSURF) :: state_id_updated
      REAL(KIND(1D0)), DIMENSION(NSURF) :: tsurf_ind

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
      REAL(KIND(1D0)) :: nsh_real ! nsh in type real
      REAL(KIND(1D0)) :: tstep_real ! tstep in type real
      REAL(KIND(1D0)) :: dectime

      ! values that are derived from sfr_surf (surface fractions)
      REAL(KIND(1D0)) :: VegFraction
      REAL(KIND(1D0)) :: ImpervFraction
      REAL(KIND(1D0)) :: PervFraction
      REAL(KIND(1D0)) :: NonWaterFraction

      ! snow related temporary values
      ! REAL(KIND(1D0))::albedo_snowfree
      REAL(KIND(1D0)) :: albedo_snow

      ! ########################################################################################
      ! TS 19 Sep 2019
      ! temporary variables to save values for inout varialbes
      ! suffixes  and  denote values from last and to next tsteps, respectively
      ! these variables are introduced to allow safe and robust iterations inccurred in this subroutine
      ! so that these values won't updated in unexpectedly many times

      ! OHM related:
      REAL(KIND(1D0)) :: qn_av_prev, qn_av_next
      REAL(KIND(1D0)) :: dqndt_prev, dqndt_next
      REAL(KIND(1D0)) :: qn_s_av_prev, qn_s_av_next
      REAL(KIND(1D0)) :: dqnsdt_prev, dqnsdt_next

      ! snow related:
      REAL(KIND(1D0)) :: SnowfallCum_prev, SnowfallCum_next
      REAL(KIND(1D0)) :: SnowAlb_prev, SnowAlb_next

      REAL(KIND(1D0)), DIMENSION(NSURF) :: IceFrac_prev, IceFrac_next
      REAL(KIND(1D0)), DIMENSION(NSURF) :: SnowWater_prev, SnowWater_next
      REAL(KIND(1D0)), DIMENSION(NSURF) :: SnowDens_prev, SnowDens_next
      REAL(KIND(1D0)), DIMENSION(NSURF) :: SnowFrac_prev, SnowFrac_next
      REAL(KIND(1D0)), DIMENSION(NSURF) :: SnowPack_prev, SnowPack_next

      ! water balance related:
      REAL(KIND(1D0)), DIMENSION(NSURF) :: soilstore_surf_prev, soilstore_surf_next
      REAL(KIND(1D0)), DIMENSION(nlayer) :: soilstore_roof_prev, soilstore_roof_next
      REAL(KIND(1D0)), DIMENSION(nlayer) :: soilstore_wall_prev, soilstore_wall_next
      REAL(KIND(1D0)), DIMENSION(NSURF) :: state_surf_prev, state_surf_next
      REAL(KIND(1D0)), DIMENSION(nlayer) :: state_roof_prev, state_roof_next
      REAL(KIND(1D0)), DIMENSION(nlayer) :: state_wall_prev, state_wall_next
      REAL(KIND(1D0)), DIMENSION(6, NSURF) :: StoreDrainPrm_prev, StoreDrainPrm_next

      ! phenology related:
      REAL(KIND(1D0)), DIMENSION(NSURF) :: alb_prev, alb_next
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDD_id_prev, GDD_id_next
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAI_id_prev, LAI_id_next
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDD_id_prev, SDD_id_next

      REAL(KIND(1D0)) :: DecidCap_id_prev, DecidCap_id_next
      REAL(KIND(1D0)) :: albDecTr_id_prev, albDecTr_id_next
      REAL(KIND(1D0)) :: albEveTr_id_prev, albEveTr_id_next
      REAL(KIND(1D0)) :: albGrass_id_prev, albGrass_id_next
      REAL(KIND(1D0)) :: porosity_id_prev, porosity_id_next

      REAL(KIND(1D0)) :: Tmin_id_prev, Tmin_id_next
      REAL(KIND(1D0)) :: Tmax_id_prev, Tmax_id_next
      REAL(KIND(1D0)) :: lenDay_id_prev, lenDay_id_next

      ! anthropogenic heat related:
      REAL(KIND(1D0)), DIMENSION(12) :: HDD_id_prev, HDD_id_next

      ! water use related:
      REAL(KIND(1D0)), DIMENSION(9) :: WUDay_id_prev, WUDay_id_next

      REAL(KIND(1D0)) :: Tair_av_prev, Tair_av_next
      ! ########################################################################################

      ! Related to RSL wind profiles
      INTEGER, PARAMETER :: nz = 90 ! number of levels 10 levels in canopy plus 20 (3 x Zh) above the canopy

      ! flag for Tsurf convergence
      LOGICAL :: flag_converge
      REAL(KIND(1D0)) :: Ts_iter
      REAL(KIND(1D0)) :: dif_tsfc_iter
      ! REAL(KIND(1D0)):: L_mod_iter
      REAL(KIND(1D0)) :: QH_Init
      INTEGER :: i_iter

      ! SPARTACUS test out
      REAL(KIND(1D0)) :: alb_spc, emis_spc, lw_emission_spc, lw_up_spc, sw_up_spc, qn_spc
      REAL(KIND(1D0)) :: top_net_lw_spc, ground_net_lw_spc, top_dn_lw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: clear_air_abs_lw_spc, wall_net_lw_spc, roof_net_lw_spc, &
                                        roof_in_lw_spc
      REAL(KIND(1D0)) :: top_dn_dir_sw_spc, top_net_sw_spc, ground_dn_dir_sw_spc, ground_net_sw_spc
      REAL(KIND(1D0)), DIMENSION(15) :: clear_air_abs_sw_spc, wall_net_sw_spc, roof_net_sw_spc, &
                                        roof_in_sw_spc
      ! REAL(KIND(1d0)), DIMENSION(30):: psihatm_z
      ! REAL(KIND(1d0)), DIMENSION(30):: psihath_z

      ! ########################################################################################
      !  ! extended for ESTM_ext, TS 20 Jan 2022
      !
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: tsfc_out_roof, tsfc0_out_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_in_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: tsfc_out_wall, tsfc0_out_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_in_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_out_surf, tsfc0_out_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth) :: temp_in_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf

      ! output arrays:

      ! roof facets
      ! aggregated heat storage of all roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QS_roof
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_out_roof

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QG_roof ! heat flux used in ESTM_ext as forcing of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qn_roof ! net all-wave radiation of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qe_roof ! latent heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_roof ! latent heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_resist_roof ! latent heat flux of roof surface [W m-2]

      ! wall facets
      ! aggregated heat storage of all wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QS_wall
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_out_wall

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer) :: QG_wall ! heat flux used in ESTM_ext as forcing of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qn_wall ! net all-wave radiation of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qe_wall ! latent heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_wall ! latent heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qh_resist_wall ! latent heat flux of wall surface [W m-2]

      ! standard suews surfaces
      !interface temperature between depth layers
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth) :: temp_out_surf !interface temperature between depth layers

      ! energy fluxes of individual surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf) :: QG_surf ! heat flux used in ESTM_ext as forcing of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_surf ! net all-wave radiation of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qs_surf ! aggregated heat storage of of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_resist_surf ! latent heat flux of individual surface [W m-2]
      ! surface temperature
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_qh_surf ! latent heat flux of individual surface [W m-2]

      ! iterator for surfaces
      INTEGER :: i_surf

      ! used in iteration
      INTEGER :: max_iter
      REAL(KIND(1D0)) :: ratio_iter

      LOGICAL, INTENT(IN) :: use_sw_direct_albedo

      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac

      ! ####
      ! set initial values for output arrays
      SWE = 0.
      mwh = 0.
      MwStore = 0.
      chSnow_per_interval = 0.
      SnowRemoval = 0.

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
      ! IF (StorageHeatMethod == 5) THEN
      state_roof_prev = state_roof
      state_wall_prev = state_wall
      soilstore_roof_prev = soilstore_roof
      soilstore_wall_prev = soilstore_wall

      ! END IF
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

      ! ESTM_ext related
      ! save initial values of inout variables
      temp_in_roof = temp_roof
      temp_in_wall = temp_wall
      temp_in_surf = temp_surf
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
      ! IF (StorageHeatMethod == 5) THEN

      soilstore_roof_next = soilstore_roof
      soilstore_wall_next = soilstore_wall
      state_roof_next = state_roof
      state_wall_next = state_wall

      ! END IF
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
      DailyStateLine = -999.

      !########################################################################################
      !           main calculation starts here
      !########################################################################################

      ! iteration is used below to get results converge
      flag_converge = .FALSE.
      Ts_iter = TEMP_C

      ! TODO: ESTM work: to allow heterogeneous surface temperatures
      tsfc_out_surf = tsfc_surf
      tsfc0_out_surf = tsfc_surf
      tsfc_out_roof = tsfc_roof
      tsfc0_out_roof = tsfc_roof
      tsfc_out_wall = tsfc_wall
      tsfc0_out_wall = tsfc_wall
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
            sfr_surf, & !input
            VegFraction, ImpervFraction, PervFraction, NonWaterFraction) ! output

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
            RoughLenMomMethod, sfr_surf, & !input
            bldgH, EveTreeH, DecTreeH, &
            porosity_id_prev, FAIBldg, FAIEveTree, FAIDecTree, &
            z0m_in, zdm_in, Z, &
            FAI, PAI, & !output
            zH, z0m, zdm, ZZD)

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
            LAICalcYes, LAIType, &
            nsh_real, kdown, Temp_C, Precip, BaseT_HC, &
            BaseT_Heating, BaseT_Cooling, &
            lat, Faut, LAI_obs, &
            AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
            AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
            CapMax_dec, CapMin_dec, PorMax_dec, PorMin_dec, &
            Ie_a, Ie_m, DayWatPer, DayWat, &
            BaseT, BaseTe, GDDFull, SDDFull, LAIMin, LAIMax, LAIPower, &
            DecidCap_id_prev, StoreDrainPrm_prev, LAI_id_prev, GDD_id_prev, SDD_id_prev, &
            albDecTr_id_prev, albEveTr_id_prev, albGrass_id_prev, porosity_id_prev, & !input
            HDD_id_prev, & !input
            state_surf_prev, soilstore_surf_prev, SoilStoreCap_surf, H_maintain, & !input
            HDD_id_next, & !output
            Tmin_id_next, Tmax_id_next, lenDay_id_next, &
            albDecTr_id_next, albEveTr_id_next, albGrass_id_next, porosity_id_next, & !output
            DecidCap_id_next, StoreDrainPrm_next, LAI_id_next, GDD_id_next, SDD_id_next, deltaLAI, WUDay_id_next) !output

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
         CALL SUEWS_cal_Qn( &
            StorageHeatMethod, NetRadiationMethod, SnowUse, & !input
            tstep, nlayer, SnowPack_prev, tau_a, tau_f, SnowAlbMax, SnowAlbMin, &
            Diagnose, ldown_obs, fcld_obs, &
            dectime, ZENITH_deg, Ts_iter, kdown, Temp_C, avRH, ea_hPa, qn1_obs, &
            SnowAlb_prev, snowFrac_prev, DiagQN, &
            NARP_TRANS_SITE, NARP_EMIS_SNOW, IceFrac_prev, &
            sfr_surf, tsfc_out_surf, tsfc_out_roof, tsfc_out_wall, &
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
            qn_surf, qn_roof, qn_wall, &
            qn, qn_snowfree, qn_snow, kclear, kup, lup, tsurf, &
            qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, &
            albedo_snow, SnowAlb_next, &
            ! alb_spc, emis_spc, lw_emission_spc, lw_up_spc, sw_up_spc, qn_spc, &
            ! top_net_lw_spc, ground_net_lw_spc, top_dn_lw_spc, &
            ! clear_air_abs_lw_spc, wall_net_lw_spc, roof_net_lw_spc, roof_in_lw_spc, &
            ! top_dn_dir_sw_spc, top_net_sw_spc, ground_dn_dir_sw_spc, ground_net_sw_spc, &
            ! clear_air_abs_sw_spc, wall_net_sw_spc, roof_net_sw_spc, roof_in_sw_spc, &
            dataOutLineSPARTACUS)

         ! PRINT *, 'Qn_surf after SUEWS_cal_Qn ', qn_surf
         ! PRINT *, 'qn_roof after SUEWS_cal_Qn ', qn_roof
         ! PRINT *, 'qn_wall after SUEWS_cal_Qn ', qn_wall
         ! PRINT *, ''

         ! =================STORAGE HEAT FLUX=======================================
         IF (i_iter == 1) THEN
            Qg_surf = 0.1*qn_surf
            Qg_roof = 0.1*qn_roof
            Qg_wall = 0.1*qn_wall
         ELSE
            Qg_surf = qn_surf + QF - (qh_surf + QE_surf)
            Qg_roof = qn_roof + QF - (qh_roof + QE_roof)
            Qg_wall = qn_wall + QF - (qh_wall + QE_wall)
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
            soilstore_surf, SoilStoreCap_surf, state_surf, SnowUse, SnowFrac, DiagQS, &
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

         CALL Snow_cal_MeltHeat( &
            SnowUse, & !input
            tstep, tau_r, SnowDensMax, &
            lvS_J_kg, lv_J_kg, tstep_real, RadMeltFact, TempMeltFact, SnowAlbMax, &
            SnowDensMin, Temp_C, Precip, PrecipLimit, PrecipLimitAlb, &
            nsh_real, sfr_surf, Tsurf_ind, Tsurf_ind_snow, state_surf_prev, qn_ind_snow, &
            kup_ind_snow, SnowWater_prev, deltaQi, albedo_snow, &
            SnowPack_prev, snowFrac_prev, SnowAlb_next, SnowDens_prev, SnowfallCum_prev, & !input
            SnowPack_next, SnowFrac_next, SnowAlb_next, SnowDens_next, SnowfallCum_next, & !output
            mwh, Qm, QmFreez, QmRain, & ! output
            veg_fr, snowCalcSwitch, Qm_melt, Qm_freezState, Qm_rain, FreezMelt, &
            FreezState, FreezStateVol, rainOnSnow, SnowDepth, mw_ind, &
            dataOutLineSnow) !output

         !==========================Turbulent Fluxes================================
         IF (Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_QHQE...'
         IF (i_iter == 1) THEN
            !Calculate QH and QE from LUMPS in the first iteration of each time step
            CALL LUMPS_cal_QHQE( &
               veg_type, & !input
               SnowUse, qn, qf, qs, Qm, Temp_C, Veg_Fr, avcp, Press_hPa, lv_J_kg, &
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
            SnowUse, NonWaterFraction, addPipes, addImpervious, addVeg, addWaterBody, &
            state_surf_prev, sfr_surf, StoreDrainPrm_next, WaterDist, nsh_real, &
            drain_per_tstep, & !output
            drain, frac_water2runoff, &
            AdditionalWater, runoffPipes, runoff_per_interval, &
            AddWater)
         !============= calculate water balance end =============

         !===============Resistance Calculations=======================
         CALL SUEWS_cal_Resistance( &
            StabilityMethod, & !input:
            Diagnose, AerodynamicResistanceMethod, RoughLenHeatMethod, SnowUse, &
            id, it, gsModel, SMDMethod, &
            avdens, avcp, QH_Init, zzd, z0m, zdm, &
            avU1, Temp_C, VegFraction, kdown, &
            Kmax, &
            g1, g2, g3, g4, &
            g5, g6, s1, s2, &
            th, tl, &
            dq, xsmd, vsmd, MaxConductance, LAIMax, LAI_id_next, SnowFrac_next, sfr_surf, &
            UStar, TStar, L_mod, & !output
            zL, gsc, RS, RA_h, RAsnow, RB, z0v, z0vSnow)

         !===================Resistance Calculations End=======================

         !===================Calculate surface hydrology and related soil water=======================
         IF (SnowUse == 1) THEN

            ! ===================Calculate snow related hydrology=======================
            CALL SUEWS_cal_snow( &
               Diagnose, nlayer, & !input
               tstep, imin, it, EvapMethod, snowCalcSwitch, dayofWeek_id, CRWmin, CRWmax, &
               dectime, avdens, avcp, lv_J_kg, lvS_J_kg, avRh, Press_hPa, Temp_C, &
               RAsnow, psyc_hPa, sIce_hPa, &
               PervFraction, vegfraction, addimpervious, qn_snowfree, qf, qs, vpd_hPa, s_hPa, &
               RS, RA_h, RB, snowdensmin, precip, PipeCapacity, RunoffToWater, &
               addVeg, SnowLimPaved, SnowLimBldg, &
               FlowChange, drain, WetThresh_surf, state_surf_prev, mw_ind, SoilStoreCap_surf, rainonsnow, &
               freezmelt, freezstate, freezstatevol, Qm_Melt, Qm_rain, Tsurf_ind, sfr_surf, &
               AddWater, frac_water2runoff, StoreDrainPrm_next, SnowPackLimit, SnowProf_24hr, &
               SnowPack_next, SnowFrac_next, SnowWater_prev, IceFrac_prev, SnowDens_next, & ! input:
               state_surf_prev, soilstore_surf_prev, & ! input:
               qn_surf, qs_surf, &
               SnowRemoval, & ! snow specific output
               SnowPack_next, SnowFrac_next, SnowWater_next, iceFrac_next, SnowDens_next, & ! output
               state_surf_next, soilstore_surf_next, & ! general output:
               state_per_tstep, NWstate_per_tstep, &
               qe, qe_surf, qe_roof, qe_wall, &
               swe, chSnow_per_interval, ev_per_tstep, runoff_per_tstep, &
               surf_chang_per_tstep, runoffPipes, mwstore, runoffwaterbody, &
               runoffAGveg, runoffAGimpervious, rss_nsurf)
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
               FlowChange, drain, &
               frac_water2runoff, StoreDrainPrm_next, &
               sfr_surf, StateLimit_surf, SoilStoreCap_surf, WetThresh_surf, & ! input:
               state_surf_prev, soilstore_surf_prev, qn_surf, qs_surf, & ! input:
               sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
               state_roof_prev, soilstore_roof_prev, qn_roof, qs_roof, & ! input:
               sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
               state_wall_prev, soilstore_wall_prev, qn_wall, qs_wall, & ! input:
               state_surf_next, soilstore_surf_next, & ! general output:
               state_roof_next, soilstore_roof_next, & ! general output:
               state_wall_next, soilstore_wall_next, & ! general output:
               state_per_tstep, NWstate_per_tstep, &
               qe, qe_surf, qe_roof, qe_wall, &
               ev_per_tstep, runoff_per_tstep, &
               surf_chang_per_tstep, runoffPipes, &
               runoffwaterbody, &
               runoffAGveg, runoffAGimpervious, rss_nsurf)
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

         tsfc0_out_surf = tsfc_out_surf
         tsfc0_out_roof = tsfc_out_roof
         tsfc0_out_wall = tsfc_out_wall

         qh_surf = qn_surf + qf - qs_surf - qe_surf
         qh_roof = qn_roof + qf - qs_roof - qe_roof
         qh_wall = qn_wall + qf - qs_wall - qe_wall
         IF (diagnose == 1) THEN
            PRINT *, 'qn_surf before QH back env.:', qn_surf
            PRINT *, 'qf before QH back env.:', qf
            PRINT *, 'qs_surf before QH back env.:', qs_surf
            PRINT *, 'qe_surf before QH back env.:', qe_surf
            PRINT *, 'qh_surf before QH back env.:', qh_surf

            PRINT *, 'qn_roof before QH back env.:', qn_roof
            PRINT *, 'qs_roof before QH back env.:', qs_roof
            PRINT *, 'qe_roof before QH back env.:', qe_roof
            PRINT *, 'qh_roof before QH back env.:', qh_roof

         END IF
         DO i_surf = 1, nsurf
            ! TSfc_QH_surf(i_surf) = cal_tsfc(qh_surf(i_surf), avdens, avcp, RA_h, temp_c)
            tsfc_out_surf(i_surf) = cal_tsfc(qh_surf(i_surf), avdens, avcp, RA_h, temp_c)
            ! if ( i_surf==1 ) then
            !    tsfc_out_surf(i_surf) = cal_tsfc(qh_surf(i_surf), avdens, avcp, RA_h, temp_c)
            ! else
            !    tsfc_out_surf(i_surf)=tsfc0_out_surf(i_surf)
            ! end if
            ! restrict calculated heat storage to a sensible range
            ! tsfc_out_surf(i_surf) = MAX(MIN(tsfc_out_surf(i_surf), 100.0), -100.0)
         END DO

         DO i_surf = 1, nlayer
            tsfc_out_roof(i_surf) = cal_tsfc(qh_roof(i_surf), avdens, avcp, RA_h, temp_c)
            tsfc_out_wall(i_surf) = cal_tsfc(qh_wall(i_surf), avdens, avcp, RA_h, temp_c)
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
         dectime, Diagnose, EmissionsMethod, Fc_anthro, G1, G2, G3, G4, &
         G5, G6, gfunc, gsmodel, id, it, Kmax, LAI_id_next, LAIMin, &
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
      StoreDrainPrm = StoreDrainPrm_next
      Tair_av = Tair_av_next
      Tmin_id = Tmin_id_next
      Tmax_id = Tmax_id_next
      lenday_id = lenday_id_next
      HDD_id = HDD_id_next
      WUDay_id = WUDay_id_next

      ! ESTM_ext related
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
         deltaLAI, VegPhenLumps, &
         SnowAlb, SnowDens, &
         a1, a2, a3, &
         DailyStateLine) !out

      !==============translation end ================

      dataoutlineDebug = &
         [qh_resist_surf, &
          tsfc0_out_surf, &
          ! state_surf_prev, &
          RS, RA_h, RB, RAsnow, &
          vpd_hPa, lv_J_kg, avdens, avcp, qn_av, dqndt]
      ! IF (NetRadiationMethod > 1000) THEN
      !    dataOutLineSPARTACUS = &
      !       [alb_spc, emis_spc, &
      !        top_dn_dir_sw_spc, &
      !        sw_up_spc, &
      !        top_dn_lw_spc, &
      !        lw_up_spc, &
      !        qn_spc, &
      !        top_net_sw_spc, &
      !        top_net_lw_spc, &
      !        lw_emission_spc, &
      !        ground_dn_dir_sw_spc, &
      !        ground_net_sw_spc, &
      !        ground_net_lw_spc, &
      !        roof_in_sw_spc, &
      !        roof_net_sw_spc, &
      !        wall_net_sw_spc, &
      !        clear_air_abs_sw_spc, &
      !        roof_in_lw_spc, &
      !        roof_net_lw_spc, &
      !        wall_net_lw_spc, &
      !        clear_air_abs_lw_spc &
      !        ]
      ! END IF

      ! write out ESTM_ext output
      ! dataoutlineESTMExt=-999
      ! dataoutlineESTMExt(1:nroof*ndepth)= pack(temp_out_roof,.True.)
      ! dataoutlineESTMExt(1:nroof*ndepth)= pack(temp_out_roof,.True.)
      ! dataoutlineESTMExt(1:nroof*ndepth)= pack(temp_out_roof,.True.)

      ! PRINT *, 'dataoutlineESTMExt = ', dataoutlineESTMExt

   END SUBROUTINE SUEWS_cal_Main
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
      INTEGER, INTENT(in) :: DLS
      INTEGER, INTENT(in) :: EmissionsMethod
      ! INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: it
      INTEGER, INTENT(in) :: imin
      ! INTEGER, INTENT(in) :: nsh
      INTEGER, DIMENSION(3), INTENT(in) :: dayofWeek_id

      REAL(KIND(1D0)), DIMENSION(6, 2), INTENT(in) :: HDD_id

      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: AH_MIN
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: AH_SLOPE_Heating
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: AH_SLOPE_Cooling
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: FcEF_v_kgkm
      ! REAL(KIND(1d0)), DIMENSION(2), INTENT(in)::NumCapita
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: PopDensDaytime
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: QF0_BEU
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: Qf_A
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: Qf_B
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: Qf_C
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: BaseT_Heating
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: BaseT_Cooling
      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: TrafficRate

      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: AHProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: HumActivity_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: TraffProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: PopProf_24hr

      REAL(KIND(1D0)), INTENT(in) :: CO2PointSource
      REAL(KIND(1D0)), INTENT(in) :: EF_umolCO2perJ
      REAL(KIND(1D0)), INTENT(in) :: EnEF_v_Jkm
      REAL(KIND(1D0)), INTENT(in) :: FrFossilFuel_Heat
      REAL(KIND(1D0)), INTENT(in) :: FrFossilFuel_NonHeat
      REAL(KIND(1D0)), INTENT(in) :: MaxFCMetab
      REAL(KIND(1D0)), INTENT(in) :: MaxQFMetab
      REAL(KIND(1D0)), INTENT(in) :: MinFCMetab
      REAL(KIND(1D0)), INTENT(in) :: MinQFMetab
      REAL(KIND(1D0)), INTENT(in) :: PopDensNighttime
      REAL(KIND(1D0)), INTENT(in) :: QF_obs
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: TrafficUnits

      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(in)::sfr_surf
      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(in)::SnowFrac
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea

      REAL(KIND(1D0)), INTENT(out) :: Fc_anthro
      REAL(KIND(1D0)), INTENT(out) :: Fc_build
      REAL(KIND(1D0)), INTENT(out) :: Fc_metab
      REAL(KIND(1D0)), INTENT(out) :: Fc_point
      REAL(KIND(1D0)), INTENT(out) :: Fc_traff
      REAL(KIND(1D0)), INTENT(out) :: QF
      REAL(KIND(1D0)), INTENT(out) :: QF_SAHP

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
   ! ================================================================================

   !==============BIOGENIC CO2 flux==================================================
   SUBROUTINE SUEWS_cal_BiogenCO2( &
      alpha_bioCO2, alpha_enh_bioCO2, avkdn, avRh, beta_bioCO2, beta_enh_bioCO2, & ! input:
      dectime, Diagnose, EmissionsMethod, Fc_anthro, G1, G2, G3, G4, &
      G5, G6, gfunc, gsmodel, id, it, Kmax, LAI_id, LAIMin, &
      LAIMax, MaxConductance, min_res_bioCO2, Press_hPa, resp_a, &
      resp_b, S1, S2, sfr_surf, SMDMethod, SnowFrac, t2_C, Temp_C, theta_bioCO2, TH, TL, vsmd, xsmd, &
      Fc, Fc_biogen, Fc_photo, Fc_respi) ! output:

      IMPLICIT NONE

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: alpha_bioCO2
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: alpha_enh_bioCO2
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: beta_bioCO2
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: beta_enh_bioCO2
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAI_id
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAIMin
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAIMax
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: min_res_bioCO2
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: resp_a
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: resp_b
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: theta_bioCO2

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac

      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: MaxConductance

      ! INTEGER, INTENT(in) :: BSoilSurf
      ! INTEGER, INTENT(in) :: ConifSurf
      ! INTEGER, INTENT(in) :: DecidSurf
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: EmissionsMethod
      ! INTEGER, INTENT(in) :: GrassSurf
      INTEGER, INTENT(in) :: gsmodel
      INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: it
      ! INTEGER, INTENT(in) :: ivConif
      ! INTEGER, INTENT(in) :: ivDecid
      ! INTEGER, INTENT(in) :: ivGrass
      ! INTEGER, INTENT(in) :: nsurf
      ! INTEGER, INTENT(in) :: NVegSurf
      INTEGER, INTENT(in) :: SMDMethod

      REAL(KIND(1D0)), INTENT(in) :: avkdn
      REAL(KIND(1D0)), INTENT(in) :: avRh
      REAL(KIND(1D0)), INTENT(in) :: dectime
      REAL(KIND(1D0)), INTENT(in) :: Fc_anthro
      REAL(KIND(1D0)), INTENT(in) :: G1
      REAL(KIND(1D0)), INTENT(in) :: G2
      REAL(KIND(1D0)), INTENT(in) :: G3
      REAL(KIND(1D0)), INTENT(in) :: G4
      REAL(KIND(1D0)), INTENT(in) :: G5
      REAL(KIND(1D0)), INTENT(in) :: G6
      REAL(KIND(1D0)), INTENT(in) :: gfunc
      REAL(KIND(1D0)), INTENT(in) :: Kmax
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa
      REAL(KIND(1D0)), INTENT(in) :: S1
      REAL(KIND(1D0)), INTENT(in) :: S2
      REAL(KIND(1D0)), INTENT(in) :: t2_C
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: TH
      REAL(KIND(1D0)), INTENT(in) :: TL
      REAL(KIND(1D0)), INTENT(in) :: vsmd
      REAL(KIND(1D0)), INTENT(in) :: xsmd

      REAL(KIND(1D0)), INTENT(out) :: Fc_biogen
      REAL(KIND(1D0)), INTENT(out) :: Fc_photo
      REAL(KIND(1D0)), INTENT(out) :: Fc_respi
      REAL(KIND(1D0)), INTENT(out) :: Fc

      REAL(KIND(1D0)) :: gfunc2
      REAL(KIND(1D0)) :: dq
      REAL(KIND(1D0)) :: t2
      REAL(KIND(1D0)) :: dummy1
      REAL(KIND(1D0)) :: dummy2
      REAL(KIND(1D0)) :: dummy3
      REAL(KIND(1D0)) :: dummy4
      REAL(KIND(1D0)) :: dummy5
      REAL(KIND(1D0)) :: dummy6
      REAL(KIND(1D0)) :: dummy7
      REAL(KIND(1D0)) :: dummy8
      REAL(KIND(1D0)) :: dummy9
      REAL(KIND(1D0)) :: dummy10
      REAL(KIND(1D0)) :: dummy11

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
               G1, G2, G3, G4, G5, G6, TH, TL, S1, S2, &
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
   !========================================================================

   !=============net all-wave radiation=====================================
   SUBROUTINE SUEWS_cal_Qn( &
      storageheatmethod, NetRadiationMethod, SnowUse, & !input
      tstep, nlayer, SnowPack_prev, tau_a, tau_f, SnowAlbMax, SnowAlbMin, &
      Diagnose, ldown_obs, fcld_obs, &
      dectime, ZENITH_deg, Tsurf_0, kdown, Tair_C, avRH, ea_hPa, qn1_obs, &
      SnowAlb_prev, snowFrac_prev, DiagQN, &
      NARP_TRANS_SITE, NARP_EMIS_SNOW, IceFrac, &
      sfr_surf, tsfc_surf, tsfc_roof, tsfc_wall, &
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
      ! alb_spc, emis_spc, lw_emission_spc, lw_up_spc, sw_up_spc, qn_spc, &
      ! top_net_lw_spc, ground_net_lw_spc, top_dn_lw_spc, &
      ! clear_air_abs_lw_spc, wall_net_lw_spc, roof_net_lw_spc, roof_in_lw_spc, &
      ! top_dn_dir_sw_spc, top_net_sw_spc, ground_dn_dir_sw_spc, ground_net_sw_spc, &
      ! clear_air_abs_sw_spc, wall_net_sw_spc, roof_net_sw_spc, roof_in_sw_spc, &
      dataOutLineSPARTACUS)
      USE NARP_MODULE, ONLY: RadMethod, NARP
      USE SPARTACUS_MODULE, ONLY: SPARTACUS

      IMPLICIT NONE
      ! INTEGER,PARAMETER ::nsurf     = 7 ! number of surface types
      ! INTEGER,PARAMETER ::ConifSurf = 3 !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER,PARAMETER ::DecidSurf = 4 !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER,PARAMETER ::GrassSurf = 5

      INTEGER, INTENT(in) :: storageheatmethod
      INTEGER, INTENT(in) :: NetRadiationMethod
      INTEGER, INTENT(in) :: SnowUse
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: DiagQN
      INTEGER, INTENT(in) :: tstep
      INTEGER, INTENT(in) :: nlayer

      ! REAL(KIND(1D0)), INTENT(in) :: snowFrac_obs
      REAL(KIND(1D0)), INTENT(in) :: ldown_obs
      REAL(KIND(1D0)), INTENT(in) :: fcld_obs
      REAL(KIND(1D0)), INTENT(in) :: dectime
      REAL(KIND(1D0)), INTENT(in) :: ZENITH_deg
      REAL(KIND(1D0)), INTENT(in) :: Tsurf_0
      REAL(KIND(1D0)), INTENT(in) :: kdown
      REAL(KIND(1D0)), INTENT(in) :: Tair_C
      REAL(KIND(1D0)), INTENT(in) :: avRH
      REAL(KIND(1D0)), INTENT(in) :: ea_hPa
      REAL(KIND(1D0)), INTENT(in) :: qn1_obs
      REAL(KIND(1D0)), INTENT(in) :: SnowAlb_prev
      REAL(KIND(1D0)), INTENT(in) :: NARP_EMIS_SNOW
      REAL(KIND(1D0)), INTENT(in) :: NARP_TRANS_SITE
      REAL(KIND(1D0)), INTENT(in) :: tau_a, tau_f, SnowAlbMax, SnowAlbMin

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(in) :: LAI_id

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: IceFrac
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: emis
      REAL(KIND(1D0)), DIMENSION(nsurf) :: alb
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: alb_prev
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: alb_next
      REAL(KIND(1D0)), INTENT(in) :: albDecTr_id
      ! REAL(KIND(1d0)), INTENT(in)  ::DecidCap_id
      REAL(KIND(1D0)), INTENT(in) :: albEveTr_id
      REAL(KIND(1D0)), INTENT(in) :: albGrass_id

      ! REAL(KIND(1d0)), DIMENSION(6, nsurf), INTENT(inout)::StoreDrainPrm

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_prev
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: snowFrac_prev
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: snowFrac_next
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac

      REAL(KIND(1D0)), INTENT(out) :: ldown
      REAL(KIND(1D0)), INTENT(out) :: fcld
      REAL(KIND(1D0)), INTENT(out) :: qn
      REAL(KIND(1D0)), INTENT(out) :: qn_snowfree
      REAL(KIND(1D0)), INTENT(out) :: qn_snow
      REAL(KIND(1D0)), INTENT(out) :: kclear
      REAL(KIND(1D0)), INTENT(out) :: kup
      REAL(KIND(1D0)), INTENT(out) :: lup
      REAL(KIND(1D0)), INTENT(out) :: tsurf
      REAL(KIND(1D0)), INTENT(out) :: albedo_snow
      REAL(KIND(1D0)), INTENT(out) :: SnowAlb_next
      REAL(KIND(1D0)) :: albedo_snowfree
      REAL(KIND(1D0)) :: SnowAlb

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qn_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qn_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: kup_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Tsurf_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: tsurf_ind

      REAL(KIND(1D0)), DIMENSION(nsurf) :: lup_ind
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn1_ind

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
      LOGICAL, INTENT(IN) :: use_sw_direct_albedo

      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qn_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qn_roof

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
               height, building_frac, veg_frac, building_scale, veg_scale, & !input:
               alb_roof, emis_roof, alb_wall, emis_wall, &
               roof_albedo_dir_mult_fact, wall_specular_frac, &
               ! alb_spc, emis_spc, lw_emission_spc, lw_up_spc, sw_up_spc, qn_spc, & !output:
               ! clear_air_abs_lw_spc, wall_net_lw_spc, roof_net_lw_spc, &
               ! roof_in_lw_spc, top_net_lw_spc, ground_net_lw_spc, &
               ! top_dn_lw_spc, &
               ! clear_air_abs_sw_spc, wall_net_sw_spc, roof_net_sw_spc, &
               ! roof_in_sw_spc, top_dn_dir_sw_spc, top_net_sw_spc, &
               ! ground_dn_dir_sw_spc, ground_net_sw_spc, &
               qn, kup, lup, qn_roof, qn_wall, & !output:
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

      INTEGER, INTENT(in) :: StorageHeatMethod
      INTEGER, INTENT(in) :: OHMIncQF
      INTEGER, INTENT(in) :: Gridiv
      INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: tstep ! time step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]
      INTEGER, INTENT(in) :: Diagnose
      ! INTEGER, INTENT(in)  ::nsh              ! number of timesteps in one hour
      INTEGER, INTENT(in) :: SnowUse ! option for snow related calculations
      INTEGER, INTENT(in) :: DiagQS ! diagnostic option
      INTEGER, INTENT(in) :: EmissionsMethod !< AnthropHeat option [-]
      INTEGER, INTENT(in) :: nlayer

      REAL(KIND(1D0)), INTENT(in) :: OHM_coef(nsurf + 1, 4, 3) ! OHM coefficients
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW(nsurf + 1) ! OHM thresholds
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshWD(nsurf + 1) ! OHM thresholds
      REAL(KIND(1D0)), INTENT(in) :: soilstore_id(nsurf) ! soil moisture
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap(nsurf) ! capacity of soil store
      REAL(KIND(1D0)), INTENT(in) :: state_id(nsurf) ! wetness status

      REAL(KIND(1D0)), DIMENSION(12), INTENT(in) :: HDD_id
      REAL(KIND(1D0)), INTENT(in) :: qf
      REAL(KIND(1D0)), INTENT(in) :: qn
      REAL(KIND(1D0)), INTENT(in) :: qs_obs
      REAL(KIND(1D0)), INTENT(in) :: avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown
      REAL(KIND(1D0)), INTENT(in) :: bldgh

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: alb !< albedo [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: emis !< emissivity [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: cpAnOHM !< heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: kkAnOHM !< thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: chAnOHM !< bulk transfer coef [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac

      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(in) :: MetForcingData_grid !< met forcing array of grid

      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: Ts5mindata_ir

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: QG_surf
      REAL(KIND(1D0)), INTENT(in) :: Tair_av
      REAL(KIND(1D0)), INTENT(in) :: qn_av_prev
      REAL(KIND(1D0)), INTENT(out) :: qn_av_next
      REAL(KIND(1D0)), INTENT(in) :: dqndt_prev ! Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(out) :: dqndt_next ! Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(in) :: qn_s_av_prev
      REAL(KIND(1D0)), INTENT(out) :: qn_s_av_next
      REAL(KIND(1D0)), INTENT(in) :: dqnsdt_prev ! Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(out) :: dqnsdt_next ! Rate of change of net radiation [W m-2 h-1] at t-1
      ! REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout)   ::qn1_store_grid
      ! REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout)   ::qn1_S_store_grid !< stored qn1 [W m-2]

      ! REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_av_store_grid
      ! REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_S_av_store_grid !< average net radiation over previous hour [W m-2]
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: deltaQi ! storage heat flux of snow surfaces

      REAL(KIND(1D0)), DIMENSION(27), INTENT(out) :: dataOutLineESTM
      REAL(KIND(1D0)), INTENT(out) :: qn_S
      REAL(KIND(1D0)), INTENT(out) :: qs ! storage heat flux
      REAL(KIND(1D0)), INTENT(out) :: a1 !< AnOHM coefficients of grid [-]
      REAL(KIND(1D0)), INTENT(out) :: a2 !< AnOHM coefficients of grid [h]
      REAL(KIND(1D0)), INTENT(out) :: a3 !< AnOHM coefficients of grid [W m-2]

      ! extended for ESTM_ext
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: temp_in_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf
      ! output arrays
      ! roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: QS_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(out) :: temp_out_roof !interface temperature between depth layers
      ! wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: QS_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(out) :: temp_out_wall !interface temperature between depth layers
      ! standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: QS_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(out) :: temp_out_surf !interface temperature between depth layers

      ! internal use arrays
      REAL(KIND(1D0)) :: Tair_mav_5d ! Tair_mav_5d=HDD(id-1,4) HDD at the begining of today (id-1)
      REAL(KIND(1D0)) :: qn_use ! qn used in OHM calculations

      REAL(KIND(1D0)) :: moist_surf(nsurf) !< non-dimensional surface wetness status (0-1) [-]

      ! initialise output variables
      !deltaQi = 0
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
         CALL AnOHM( &
            tstep, dt_since_start, &
            qn_use, qn_av_prev, dqndt_prev, qf, &
            MetForcingData_grid, moist_surf, &
            alb, emis, cpAnOHM, kkAnOHM, chAnOHM, & ! input
            sfr_surf, nsurf, EmissionsMethod, id, Gridiv, &
            qn_av_next, dqndt_next, &
            a1, a2, a3, qs, deltaQi) ! output
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
         CALL ESTM_ext( &
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

         ! PRINT *, 'QS after ESTM_ext', QS
         ! PRINT *, 'QS_roof after ESTM_ext', QS_roof
         ! PRINT *, 'QS_wall after ESTM_ext', QS_wall
         ! PRINT *, 'QS_surf after ESTM_ext', QS_surf
         ! PRINT *, '------------------------------------'
         ! PRINT *, ''
      END IF

   END SUBROUTINE SUEWS_cal_Qs
   !=======================================================================

   !==========================drainage and runoff================================
   SUBROUTINE SUEWS_cal_Water( &
      Diagnose, & !input
      SnowUse, NonWaterFraction, addPipes, addImpervious, addVeg, addWaterBody, &
      state_id, sfr_surf, StoreDrainPrm, WaterDist, nsh_real, &
      drain_per_tstep, & !output
      drain, frac_water2runoff, &
      AdditionalWater, runoffPipes, runoff_per_interval, &
      AddWater)

      IMPLICIT NONE
      ! INTEGER,PARAMETER :: nsurf=7! number of surface types
      ! INTEGER,PARAMETER ::WaterSurf = 7
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: SnowUse

      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
      REAL(KIND(1D0)), INTENT(in) :: addPipes
      REAL(KIND(1D0)), INTENT(in) :: addImpervious
      REAL(KIND(1D0)), INTENT(in) :: addVeg
      REAL(KIND(1D0)), INTENT(in) :: addWaterBody
      REAL(KIND(1D0)), INTENT(in) :: nsh_real !nsh cast as a real for use in calculations

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_id
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_id
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, nsurf - 1), INTENT(in) :: WaterDist

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: drain !Drainage of surface type "is" [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: frac_water2runoff !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: AddWater !water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: stateOld
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstoreOld

      REAL(KIND(1D0)), INTENT(out) :: drain_per_tstep
      REAL(KIND(1D0)), INTENT(out) :: AdditionalWater
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes
      REAL(KIND(1D0)), INTENT(out) :: runoff_per_interval
      INTEGER :: is

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

   END SUBROUTINE SUEWS_cal_Water
   !=======================================================================

   !===============initialize sensible heat flux============================
   SUBROUTINE SUEWS_init_QH( &
      avdens, avcp, h_mod, qn1, dectime, & !input
      H_init) !output

      IMPLICIT NONE
      ! REAL(KIND(1d0)), INTENT(in)::qh_obs
      REAL(KIND(1D0)), INTENT(in) :: avdens
      REAL(KIND(1D0)), INTENT(in) :: avcp
      REAL(KIND(1D0)), INTENT(in) :: h_mod
      REAL(KIND(1D0)), INTENT(in) :: qn1
      REAL(KIND(1D0)), INTENT(in) :: dectime
      REAL(KIND(1D0)), INTENT(out) :: H_init

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
      tstep, imin, it, EvapMethod, snowCalcSwitch, dayofWeek_id, CRWmin, CRWmax, &
      dectime, avdens, avcp, lv_J_kg, lvS_J_kg, avRh, Press_hPa, Temp_C, &
      RAsnow, psyc_hPa, sIce_hPa, &
      PervFraction, vegfraction, addimpervious, qn_snowfree, qf, qs, vpd_hPa, s_hPa, &
      RS, RA, RB, snowdensmin, precip, PipeCapacity, RunoffToWater, &
      addVeg, SnowLimPaved, SnowLimBldg, &
      FlowChange, drain, WetThresh_surf, stateOld, mw_ind, SoilStoreCap, rainonsnow, &
      freezmelt, freezstate, freezstatevol, Qm_Melt, Qm_rain, Tsurf_ind, sfr_surf, &
      AddWater, addwaterrunoff, StoreDrainPrm, SnowPackLimit, SnowProf_24hr, &
      SnowPack_in, SnowFrac_in, SnowWater_in, iceFrac_in, SnowDens_in, & ! input:
      state_id_in, soilstore_id_in, & ! input:
      qn_surf, qs_surf, &
      SnowRemoval, & ! snow specific output:
      SnowPack_out, SnowFrac_out, SnowWater_out, iceFrac_out, SnowDens_out, & ! output
      state_id_out, soilstore_id_out, & ! general output:
      state_per_tstep, NWstate_per_tstep, &
      qe, qe_surf, qe_roof, qe_wall, &
      swe, chSnow_per_interval, ev_per_tstep, runoff_per_tstep, &
      surf_chang_per_tstep, runoffPipes, mwstore, runoffwaterbody, &
      runoffAGveg, runoffAGimpervious, rss_surf)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: nlayer
      INTEGER, INTENT(in) :: tstep
      INTEGER, INTENT(in) :: imin
      INTEGER, INTENT(in) :: it
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)

      INTEGER, DIMENSION(nsurf), INTENT(in) :: snowCalcSwitch
      INTEGER, DIMENSION(3), INTENT(in) :: dayofWeek_id

      REAL(KIND(1D0)), INTENT(in) :: CRWmin
      REAL(KIND(1D0)), INTENT(in) :: CRWmax
      REAL(KIND(1D0)), INTENT(in) :: dectime
      REAL(KIND(1D0)), INTENT(in) :: lvS_J_kg
      REAL(KIND(1D0)), INTENT(in) :: lv_j_kg
      REAL(KIND(1D0)), INTENT(in) :: avdens
      REAL(KIND(1D0)), INTENT(in) :: avRh
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: RAsnow
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa
      REAL(KIND(1D0)), INTENT(in) :: avcp
      REAL(KIND(1D0)), INTENT(in) :: sIce_hPa
      REAL(KIND(1D0)), INTENT(in) :: PervFraction
      REAL(KIND(1D0)), INTENT(in) :: vegfraction
      REAL(KIND(1D0)), INTENT(in) :: addimpervious
      REAL(KIND(1D0)), INTENT(in) :: qn_snowfree
      REAL(KIND(1D0)), INTENT(in) :: qf
      REAL(KIND(1D0)), INTENT(in) :: qs
      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa
      REAL(KIND(1D0)), INTENT(in) :: s_hPa
      REAL(KIND(1D0)), INTENT(in) :: RS
      REAL(KIND(1D0)), INTENT(in) :: RA
      REAL(KIND(1D0)), INTENT(in) :: RB
      REAL(KIND(1D0)), INTENT(in) :: snowdensmin
      REAL(KIND(1D0)), INTENT(in) :: precip
      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater
      ! REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
      ! REAL(KIND(1d0)), INTENT(in)::wu_EveTr!Water use for evergreen trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_DecTr!Water use for deciduous trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_Grass!Water use for grass [mm]
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      ! REAL(KIND(1D0)), INTENT(in) :: addWaterBody !Water from water surface of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: SnowLimPaved
      REAL(KIND(1D0)), INTENT(in) :: SnowLimBldg
      ! REAL(KIND(1D0)), INTENT(in) :: SurfaceArea
      REAL(KIND(1D0)), INTENT(in) :: FlowChange

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WU_nsurf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WetThresh_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: stateOld
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: mw_ind
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: rainonsnow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: freezmelt
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: freezstate
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: freezstatevol
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Qm_Melt
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Qm_rain
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPackLimit
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StateLimit !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: addwaterrunoff
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: SnowProf_24hr

      ! Total water transported to each grid for grid-to-grid connectivity
      ! REAL(KIND(1D0)), INTENT(in) :: runoff_per_interval_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_id_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_id_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowWater_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: iceFrac_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_in

      ! output:
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_id_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstore_id_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowPack_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowFrac_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowWater_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: iceFrac_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDens_out

      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: runoffSnow_surf !Initialize for runoff caused by snowmelting
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_surf
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chang
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ChangSnow_surf
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: snowDepth
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowToSurf
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_snow
      REAL(KIND(1D0)), DIMENSION(2), INTENT(out) :: SnowRemoval
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rss_surf

      ! REAL(KIND(1D0)) :: p_mm !Inputs to surface water balance
      ! REAL(KIND(1d0)),INTENT(out)::rss
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qs_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qe_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_roof ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_wall ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: state_per_tstep
      REAL(KIND(1D0)), INTENT(out) :: NWstate_per_tstep
      REAL(KIND(1D0)), INTENT(out) :: qe
      REAL(KIND(1D0)), INTENT(out) :: swe
      REAL(KIND(1D0)) :: ev
      REAL(KIND(1D0)), INTENT(out) :: chSnow_per_interval
      REAL(KIND(1D0)), INTENT(out) :: ev_per_tstep
      REAL(KIND(1D0)) :: qe_per_tstep
      REAL(KIND(1D0)), INTENT(out) :: runoff_per_tstep
      REAL(KIND(1D0)), INTENT(out) :: surf_chang_per_tstep
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes
      REAL(KIND(1D0)), INTENT(out) :: mwstore
      REAL(KIND(1D0)), INTENT(out) :: runoffwaterbody
      ! REAL(KIND(1D0)) :: runoffWaterBody_m3
      ! REAL(KIND(1D0)) :: runoffPipes_m3
      REAL(KIND(1D0)), INTENT(out) :: runoffAGveg
      REAL(KIND(1D0)), INTENT(out) :: runoffAGimpervious

      ! local:
      INTEGER :: is

      ! REAL(KIND(1D0)) :: runoff_per_interval
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id_surf
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowPack
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowWater
      REAL(KIND(1D0)), DIMENSION(nsurf) :: iceFrac
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDens
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_e_surf

      REAL(KIND(1D0)), DIMENSION(2) :: SurplusEvap !Surplus for evaporation in 5 min timestep
      REAL(KIND(1D0)) :: surplusWaterBody
      REAL(KIND(1D0)) :: pin !Rain per time interval
      ! REAL(KIND(1d0))::sae
      ! REAL(KIND(1d0))::vdrc
      ! REAL(KIND(1d0))::sp
      ! REAL(KIND(1d0))::numPM
      REAL(KIND(1D0)) :: qn_e
      REAL(KIND(1D0)) :: tlv
      ! REAL(KIND(1D0)) :: runoffAGimpervious_m3
      ! REAL(KIND(1D0)) :: runoffAGveg_m3
      REAL(KIND(1D0)) :: nsh_real
      ! REAL(KIND(1D0)) :: tstep_real
      REAL(KIND(1D0)) :: ev_tot
      REAL(KIND(1D0)) :: qe_tot
      REAL(KIND(1D0)) :: surf_chang_tot
      REAL(KIND(1D0)) :: runoff_tot
      REAL(KIND(1D0)) :: chSnow_tot

      REAL(KIND(1D0)), DIMENSION(7) :: capStore_surf ! current storage capacity [mm]

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
      qe = 0
      runoffwaterbody = 0

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
      ev = 0

      mwstore = 0

      chSnow_per_interval = 0
      qe_tot = 0
      ev_tot = 0
      swe = 0
      ev_snow = 0

      SnowRemoval = 0
      SnowPack = SnowPack_in
      SnowFrac = SnowFrac_in
      SnowWater = SnowWater_in
      iceFrac = iceFrac_in
      SnowDens = SnowDens_in
      DO is = 1, nsurf !For each surface in turn
         IF (sfr_surf(is) /= 0 .AND. snowCalcSwitch(is) == 1) THEN
            ! IF (Diagnose == 1) WRITE (*, *) 'Calling SnowCalc...'
            CALL SnowCalc( &
               tstep, imin, it, dectime, is, & !input
               EvapMethod, CRWmin, CRWmax, nsh_real, lvS_J_kg, avdens, &
               avRh, Press_hPa, Temp_C, RAsnow, psyc_hPa, avcp, sIce_hPa, &
               PervFraction, vegfraction, addimpervious, &
               vpd_hPa, qn_e, s_hPa, RS, RA, RB, tlv, snowdensmin, SnowProf_24hr, precip, &
               PipeCapacity, RunoffToWater, &
               addVeg, SnowLimPaved, SnowLimBldg, FlowChange, drain, &
               WetThresh_surf, stateOld, mw_ind, SoilStoreCap, rainonsnow, &
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

            !Actual updates here as xx_tstep variables not taken as input to snowcalc
            ev_per_tstep = ev_per_tstep + ev_tot
            qe_per_tstep = qe_per_tstep + qe_tot
            runoff_per_tstep = runoff_per_tstep + runoff_tot
            surf_chang_per_tstep = surf_chang_per_tstep + surf_chang_tot
            chSnow_per_interval = chSnow_per_interval + chSnow_tot
         ELSE
            SnowFrac(is) = 0
            SnowDens(is) = 0
            SnowPack(is) = 0
         END IF

         !Store ev_tot for each surface
         ev_surf(is) = ev_tot
      END DO
      ! ELSE ! snow-free calculation
      ! ChangSnow_surf = 0
      ! ! runoffSnow_surf = 0
      ! DO is = 1, nsurf !For each surface in turn
      !    capStore_surf(is) = StoreDrainPrm(6, is)
      !    !Calculates ev [mm]
      !    CALL cal_evap( &
      !       EvapMethod, state_id_surf(is), WetThresh_surf(is), capStore_surf(is), & !input
      !       vpd_hPa, avdens, avcp, qn_e_surf(is), s_hPa, psyc_hPa, RS, RA, RB, tlv, &
      !       rss_surf(is), ev_surf(is), qe_surf(is)) !output
      !    ! print *, 'qe_surf for', is , qe_surf(is)

      !    !Surface water balance and soil store updates (can modify ev, updates state_id)
      !    CALL cal_water_storage( &
      !       is, sfr_surf, PipeCapacity, RunoffToWater, pin, & ! input:
      !       WU_nsurf, &
      !       drain, AddWater, addImpervious, nsh_real, stateOld, AddWaterRunoff, &
      !       PervFraction, addVeg, SoilStoreCap, addWaterBody, FlowChange, &
      !       StateLimit, runoffAGimpervious, surplusWaterBody, &
      !       runoffAGveg, runoffPipes, ev_surf(is), soilstore_id, SurplusEvap, runoffWaterBody, &
      !       p_mm, chang, runoff_surf, state_id_surf) !output:

      ! END DO !end loop over surfaces

      ! ! Sum evaporation from different surfaces to find total evaporation [mm]
      ! ev_per_tstep = DOT_PRODUCT(ev_surf, sfr_surf)

      ! ! Sum latent heat flux from different surfaces to find total latent heat flux
      ! qe_per_tstep = DOT_PRODUCT(qe_surf, sfr_surf)

      ! ! Sum change from different surfaces to find total change to surface state_id
      ! surf_chang_per_tstep = DOT_PRODUCT(state_id_surf - stateOld, sfr_surf)

      ! ! Sum runoff from different surfaces to find total runoff
      ! runoff_per_tstep = DOT_PRODUCT(runoff_surf, sfr_surf)

      ! ! Calculate total state_id (including water body)
      ! state_per_tstep = DOT_PRODUCT(state_id_surf, sfr_surf)

      ! IF (NonWaterFraction /= 0) THEN
      !    NWstate_per_tstep = DOT_PRODUCT(state_id_surf(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction
      ! END IF
      ! END IF

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

      SnowPack_out = SnowPack
      SnowFrac_out = SnowFrac
      SnowWater_out = SnowWater
      iceFrac_out = iceFrac
      SnowDens_out = SnowDens

   END SUBROUTINE SUEWS_cal_snow

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
      frac_water2runoff_surf, StoreDrainPrm, &
      sfr_surf, StateLimit_surf, SoilStoreCap_surf, WetThresh_surf, & ! input:
      state_surf_in, soilstore_surf_in, qn_surf, qs_surf, & ! input:
      sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
      state_roof_in, soilstore_roof_in, qn_roof, qs_roof, & ! input:
      sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
      state_wall_in, soilstore_wall_in, qn_wall, qs_wall, & ! input:
      state_surf_out, soilstore_surf_out, & ! general output:
      state_roof_out, soilstore_roof_out, & ! general output:
      state_wall_out, soilstore_wall_out, & ! general output:
      state_grid, NWstate_grid, &
      qe, qe_surf, qe_roof, qe_wall, &
      ev_grid, runoff_grid, &
      surf_chang_grid, runoffPipes_grid, &
      runoffWaterBody_grid, &
      runoffAGveg_grid, runoffAGimpervious_grid, rss_surf)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: storageheatmethod
      INTEGER, INTENT(in) :: nlayer
      INTEGER, INTENT(in) :: tstep
      ! INTEGER, INTENT(in) :: imin
      ! INTEGER, INTENT(in) :: it
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)

      ! INTEGER, DIMENSION(nsurf), INTENT(in) :: snowCalcSwitch
      ! INTEGER, DIMENSION(3), INTENT(in) :: dayofWeek_id

      ! REAL(KIND(1D0)), INTENT(in) :: CRWmin
      ! REAL(KIND(1D0)), INTENT(in) :: CRWmax
      ! REAL(KIND(1D0)), INTENT(in) :: dectime
      ! REAL(KIND(1D0)), INTENT(in) :: lvS_J_kg
      REAL(KIND(1D0)), INTENT(in) :: lv_j_kg
      REAL(KIND(1D0)), INTENT(in) :: avdens
      ! REAL(KIND(1D0)), INTENT(in) :: avRh
      ! REAL(KIND(1D0)), INTENT(in) :: Press_hPa
      ! REAL(KIND(1D0)), INTENT(in) :: Temp_C
      ! REAL(KIND(1D0)), INTENT(in) :: RAsnow
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa
      REAL(KIND(1D0)), INTENT(in) :: avcp
      ! REAL(KIND(1D0)), INTENT(in) :: sIce_hPa
      REAL(KIND(1D0)), INTENT(in) :: PervFraction
      ! REAL(KIND(1D0)), INTENT(in) :: vegfraction
      REAL(KIND(1D0)), INTENT(in) :: addimpervious
      ! REAL(KIND(1D0)), INTENT(in) :: qn_snowfree
      REAL(KIND(1D0)), INTENT(in) :: qf
      ! REAL(KIND(1D0)), INTENT(in) :: qs
      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa
      REAL(KIND(1D0)), INTENT(in) :: s_hPa
      REAL(KIND(1D0)), INTENT(in) :: RS
      REAL(KIND(1D0)), INTENT(in) :: RA_h
      REAL(KIND(1D0)), INTENT(in) :: RB
      ! REAL(KIND(1D0)), INTENT(in) :: snowdensmin
      REAL(KIND(1D0)), INTENT(in) :: precip
      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater
      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
      ! REAL(KIND(1d0)), INTENT(in)::wu_EveTr!Water use for evergreen trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_DecTr!Water use for deciduous trees/shrubs [mm]
      ! REAL(KIND(1d0)), INTENT(in)::wu_Grass!Water use for grass [mm]
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addWaterBody !Water from water surface of other grids [mm] for whole surface area
      ! REAL(KIND(1D0)), INTENT(in) :: SnowLimPaved
      ! REAL(KIND(1D0)), INTENT(in) :: SnowLimBldg
      ! REAL(KIND(1D0)), INTENT(in) :: SurfaceArea
      REAL(KIND(1D0)), INTENT(in) :: FlowChange

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WU_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain_surf

      ! input for generic suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StateLimit_surf !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WetThresh_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_surf_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_surf_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qs_surf ! latent heat flux of individual surface [W m-2]

      ! input for generic roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: StateLimit_roof !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: WetThresh_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: SoilStoreCap_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_roof_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_roof_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qn_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qs_roof

      ! input for generic wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: StateLimit_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: WetThresh_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: SoilStoreCap_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_wall_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_wall_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qn_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qs_wall

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPackLimit
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: frac_water2runoff_surf
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm
      ! REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: SnowProf_24hr

      ! Total water transported to each grid for grid-to-grid connectivity
      ! REAL(KIND(1D0)), INTENT(in) :: runoff_per_interval_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowWater_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: iceFrac_in
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_in

      ! output:
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_surf_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstore_surf_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: state_roof_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: soilstore_roof_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: state_wall_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: soilstore_wall_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowPack_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowFrac_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowWater_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: iceFrac_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDens_out

      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: runoffSnow_surf !Initialize for runoff caused by snowmelting
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_surf
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chang
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: ChangSnow_surf
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: snowDepth
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowToSurf
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_snow
      ! REAL(KIND(1D0)), DIMENSION(2), INTENT(out) :: SnowRemoval
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rss_surf

      REAL(KIND(1D0)) :: p_mm !Inputs to surface water balance
      ! REAL(KIND(1d0)),INTENT(out)::rss
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qe_surf ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_roof ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qe_wall ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_roof
      REAL(KIND(1D0)), DIMENSION(nlayer) :: rss_roof
      REAL(KIND(1D0)), DIMENSION(nlayer) :: runoff_roof
      REAL(KIND(1D0)) :: qe_roof_total
      REAL(KIND(1D0)), DIMENSION(nlayer) :: ev_wall
      REAL(KIND(1D0)), DIMENSION(nlayer) :: rss_wall
      REAL(KIND(1D0)), DIMENSION(nlayer) :: runoff_wall
      REAL(KIND(1D0)) :: qe_wall_total
      REAL(KIND(1D0)), INTENT(out) :: state_grid
      REAL(KIND(1D0)), INTENT(out) :: NWstate_grid
      REAL(KIND(1D0)), INTENT(out) :: qe
      ! REAL(KIND(1D0)), INTENT(out) :: swe
      ! REAL(KIND(1D0)) :: ev
      ! REAL(KIND(1D0)), INTENT(out) :: chSnow_per_interval
      REAL(KIND(1D0)), INTENT(out) :: ev_grid
      REAL(KIND(1D0)) :: qe_grid
      REAL(KIND(1D0)), INTENT(out) :: runoff_grid
      REAL(KIND(1D0)), INTENT(out) :: surf_chang_grid
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes_grid
      ! REAL(KIND(1D0)), INTENT(out) :: mwstore
      REAL(KIND(1D0)), INTENT(out) :: runoffWaterBody_grid
      ! REAL(KIND(1D0)) :: runoffWaterBody_m3
      ! REAL(KIND(1D0)) :: runoffPipes_m3
      REAL(KIND(1D0)), INTENT(out) :: runoffAGveg_grid
      REAL(KIND(1D0)), INTENT(out) :: runoffAGimpervious_grid

      ! local:
      ! INTEGER :: is

      ! REAL(KIND(1D0)) :: runoff_per_interval
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id_out
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowPack
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowWater
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: iceFrac
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDens
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_e_surf
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qn_e_roof
      REAL(KIND(1D0)), DIMENSION(nlayer) :: qn_e_wall

      REAL(KIND(1D0)) :: pin !Rain per time interval
      REAL(KIND(1D0)) :: tlv
      REAL(KIND(1D0)) :: nsh_real
      REAL(KIND(1D0)) :: state_building
      REAL(KIND(1D0)) :: soilstore_building
      REAL(KIND(1D0)) :: capStore_builing
      REAL(KIND(1D0)) :: runoff_building
      REAL(KIND(1D0)) :: qe_building

      REAL(KIND(1D0)), DIMENSION(7) :: capStore_surf ! current storage capacity [mm]

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
         rss_surf, ev_surf, qe_surf) !output

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
         qe_building = 0.5*(DOT_PRODUCT(qe_roof, sfr_roof) + DOT_PRODUCT(qe_wall, sfr_wall))
      END IF
      ! --- general suews surfaces ---
      CALL cal_water_storage_surf( &
         pin, nsh_real, &
         PipeCapacity, RunoffToWater, & ! input:
         addImpervious, addVeg, addWaterBody, FlowChange, &
         SoilStoreCap_surf, StateLimit_surf, &
         PervFraction, &
         sfr_surf, drain_surf, AddWater_surf, frac_water2runoff_surf, WU_surf, &
         ev_surf, state_surf_in, soilstore_surf_in, &
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
      IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE soilstore_building = ', soilstore_building
      IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE capStore_builing = ', capStore_builing
      IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE capStore_surf(BldgSurf) = ', capStore_surf(BldgSurf)
      IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE soilstore_id = ', soilstore_surf_out

   END SUBROUTINE SUEWS_cal_QE
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
      INTEGER, INTENT(in) :: storageheatmethod ! decide if roof and wall are included in QH calculation
      INTEGER, INTENT(in) :: nlayer

      REAL(KIND(1D0)), INTENT(in) :: qn
      REAL(KIND(1D0)), INTENT(in) :: qf
      REAL(KIND(1D0)), INTENT(in) :: QmRain
      REAL(KIND(1D0)), INTENT(in) :: qe
      REAL(KIND(1D0)), INTENT(in) :: qs
      REAL(KIND(1D0)), INTENT(in) :: QmFreez
      REAL(KIND(1D0)), INTENT(in) :: qm
      REAL(KIND(1D0)), INTENT(in) :: avdens
      REAL(KIND(1D0)), INTENT(in) :: avcp
      ! REAL(KIND(1D0)), INTENT(in) :: tsurf
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: RA

      REAL(KIND(1D0)), INTENT(out) :: qh
      REAL(KIND(1D0)), INTENT(out) :: qh_resist
      REAL(KIND(1D0)), INTENT(out) :: qh_residual
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qh_resist_surf
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qh_resist_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qh_resist_wall

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
   !========================================================================

   !===============Resistance Calculations=======================
   SUBROUTINE SUEWS_cal_Resistance( &
      StabilityMethod, & !input:
      Diagnose, AerodynamicResistanceMethod, RoughLenHeatMethod, SnowUse, &
      id, it, gsModel, SMDMethod, &
      avdens, avcp, QH_init, zzd, z0m, zdm, &
      avU1, Temp_C, VegFraction, &
      avkdn, Kmax, G1, G2, G3, G4, G5, G6, S1, S2, TH, TL, dq, &
      xsmd, vsmd, MaxConductance, LAIMax, LAI_id, SnowFrac, sfr_surf, &
      UStar, TStar, L_mod, & !output
      zL, gsc, RS, RA, RASnow, RB, z0v, z0vSnow)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: StabilityMethod
      INTEGER, INTENT(in) :: Diagnose
      INTEGER, INTENT(in) :: AerodynamicResistanceMethod
      INTEGER, INTENT(in) :: RoughLenHeatMethod
      INTEGER, INTENT(in) :: SnowUse
      INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: it !time: day of year and hour
      INTEGER, INTENT(in) :: gsModel !Choice of gs parameterisation (1 = Ja11, 2 = Wa16)
      INTEGER, INTENT(in) :: SMDMethod !Method of measured soil moisture

      ! REAL(KIND(1d0)), INTENT(in)::qh_obs
      REAL(KIND(1D0)), INTENT(in) :: avdens
      REAL(KIND(1D0)), INTENT(in) :: avcp
      REAL(KIND(1D0)), INTENT(in) :: QH_init
      REAL(KIND(1D0)), INTENT(in) :: zzd !Active measurement height (meas. height-displac. height)
      REAL(KIND(1D0)), INTENT(in) :: z0m !Aerodynamic roughness length
      REAL(KIND(1D0)), INTENT(in) :: zdm !Displacement height
      REAL(KIND(1D0)), INTENT(in) :: avU1 !Average wind speed
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !Air temperature
      REAL(KIND(1D0)), INTENT(in) :: VegFraction !Fraction of vegetation
      REAL(KIND(1D0)), INTENT(in) :: avkdn !Average downwelling shortwave radiation
      REAL(KIND(1D0)), INTENT(in) :: Kmax !Annual maximum hourly solar radiation
      REAL(KIND(1D0)), INTENT(in) :: G1 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G2 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G3 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G4 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G5 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: G6 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: S1 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: S2 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)), INTENT(in) :: TH !Maximum temperature limit
      REAL(KIND(1D0)), INTENT(in) :: TL !Minimum temperature limit
      REAL(KIND(1D0)), INTENT(in) :: dq !Specific humidity deficit
      REAL(KIND(1D0)), INTENT(in) :: xsmd !Measured soil moisture deficit
      REAL(KIND(1D0)), INTENT(in) :: vsmd !Soil moisture deficit for vegetated surfaces only (QUESTION: what about BSoil?)

      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: MaxConductance !Max conductance [mm s-1]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: LAIMax !Max LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: LAI_id !=LAI_id(id-1,:), LAI for each veg surface [m2 m-2]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac !Surface fraction of snow cover
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !Surface fractions [-]

      REAL(KIND(1D0)), INTENT(out) :: TStar !T*
      REAL(KIND(1D0)), INTENT(out) :: UStar !Friction velocity
      REAL(KIND(1D0)), INTENT(out) :: zL !
      REAL(KIND(1D0)), INTENT(out) :: gsc !Surface Layer Conductance
      REAL(KIND(1D0)), INTENT(out) :: RS !Surface resistance
      REAL(KIND(1D0)), INTENT(out) :: RA !Aerodynamic resistance [s m^-1]
      REAL(KIND(1D0)), INTENT(out) :: z0v !roughness for heat [m]
      REAL(KIND(1D0)), INTENT(out) :: RASnow !Aerodynamic resistance for snow [s m^-1]
      REAL(KIND(1D0)), INTENT(out) :: z0vSnow !roughness for heat [m]
      REAL(KIND(1D0)), INTENT(out) :: RB !boundary layer resistance shuttleworth
      REAL(KIND(1D0)), INTENT(out) :: L_mod !Obukhov length
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
         G1, G2, G3, G4, G5, G6, TH, TL, S1, S2, &
         gfunc, gsc, RS) ! output:

      IF (Diagnose == 1) WRITE (*, *) 'Calling BoundaryLayerResistance...'
      CALL BoundaryLayerResistance( &
         zzd, & ! input:     !Active measurement height (meas. height- zero-plane displacement)
         z0m, & !Aerodynamic roughness length
         avU1, & !Average wind speed
         UStar, & ! input/output:
         RB) ! output:

   END SUBROUTINE SUEWS_cal_Resistance
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
      INTEGER, INTENT(in) :: iy
      INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: it
      INTEGER, INTENT(in) :: imin
      REAL(KIND(1D0)), INTENT(in) :: AdditionalWater
      REAL(KIND(1D0)), INTENT(in) :: alb(nsurf)
      REAL(KIND(1D0)), INTENT(in) :: avkdn
      REAL(KIND(1D0)), INTENT(in) :: avU10_ms
      REAL(KIND(1D0)), INTENT(in) :: azimuth
      REAL(KIND(1D0)), INTENT(in) :: chSnow_per_interval
      REAL(KIND(1D0)), INTENT(in) :: dectime
      REAL(KIND(1D0)), INTENT(in) :: drain_per_tstep
      REAL(KIND(1D0)), INTENT(in) :: E_mod
      REAL(KIND(1D0)), INTENT(in) :: ev_per_tstep
      REAL(KIND(1D0)), INTENT(in) :: ext_wu
      REAL(KIND(1D0)), INTENT(in) :: Fc
      REAL(KIND(1D0)), INTENT(in) :: Fc_build
      REAL(KIND(1D0)), INTENT(in) :: Fc_metab
      REAL(KIND(1D0)), INTENT(in) :: Fc_photo
      REAL(KIND(1D0)), INTENT(in) :: Fc_respi
      REAL(KIND(1D0)), INTENT(in) :: Fc_point
      REAL(KIND(1D0)), INTENT(in) :: Fc_traff
      REAL(KIND(1D0)), INTENT(in) :: fcld
      REAL(KIND(1D0)), INTENT(in) :: FlowChange
      REAL(KIND(1D0)), INTENT(in) :: h_mod
      REAL(KIND(1D0)), INTENT(in) :: int_wu
      REAL(KIND(1D0)), INTENT(in) :: kup
      REAL(KIND(1D0)), INTENT(in) :: l_mod
      REAL(KIND(1D0)), INTENT(in) :: LAI_id(nvegsurf)
      REAL(KIND(1D0)), INTENT(in) :: ldown
      REAL(KIND(1D0)), INTENT(in) :: lup
      REAL(KIND(1D0)), INTENT(in) :: mwh
      REAL(KIND(1D0)), INTENT(in) :: MwStore
      REAL(KIND(1D0)), INTENT(in) :: nsh_real
      REAL(KIND(1D0)), INTENT(in) :: NWstate_per_tstep
      REAL(KIND(1D0)), INTENT(in) :: Precip
      REAL(KIND(1D0)), INTENT(in) :: q2_gkg
      REAL(KIND(1D0)), INTENT(in) :: qeOut
      REAL(KIND(1D0)), INTENT(in) :: qf
      REAL(KIND(1D0)), INTENT(in) :: qh
      REAL(KIND(1D0)), INTENT(in) :: qh_resist
      REAL(KIND(1D0)), INTENT(in) :: Qm
      REAL(KIND(1D0)), INTENT(in) :: QmFreez
      REAL(KIND(1D0)), INTENT(in) :: QmRain
      REAL(KIND(1D0)), INTENT(in) :: qn
      REAL(KIND(1D0)), INTENT(in) :: qn_snow
      REAL(KIND(1D0)), INTENT(in) :: qn_snowfree
      REAL(KIND(1D0)), INTENT(in) :: qs
      REAL(KIND(1D0)), INTENT(in) :: RA
      REAL(KIND(1D0)), INTENT(in) :: resistsurf
      REAL(KIND(1D0)), INTENT(in) :: RH2
      REAL(KIND(1D0)), INTENT(in) :: runoff_per_tstep
      REAL(KIND(1D0)), INTENT(in) :: runoffAGimpervious
      REAL(KIND(1D0)), INTENT(in) :: runoffAGveg
      REAL(KIND(1D0)), INTENT(in) :: runoffPipes
      REAL(KIND(1D0)), INTENT(in) :: runoffSoil_per_tstep
      REAL(KIND(1D0)), INTENT(in) :: runoffWaterBody
      REAL(KIND(1D0)), INTENT(in) :: sfr_surf(nsurf)
      REAL(KIND(1D0)), INTENT(in) :: smd
      REAL(KIND(1D0)), INTENT(in) :: smd_nsurf(nsurf)
      REAL(KIND(1D0)), INTENT(in) :: SnowAlb
      REAL(KIND(1D0)), INTENT(in) :: SnowRemoval(2)
      REAL(KIND(1D0)), INTENT(in) :: state_id(nsurf)
      REAL(KIND(1D0)), INTENT(in) :: state_per_tstep
      REAL(KIND(1D0)), INTENT(in) :: surf_chang_per_tstep
      REAL(KIND(1D0)), INTENT(in) :: swe
      REAL(KIND(1D0)), INTENT(in) :: t2_C
      REAL(KIND(1D0)), INTENT(in) :: tskin_C
      REAL(KIND(1D0)), INTENT(in) :: tot_chang_per_tstep
      REAL(KIND(1D0)), INTENT(in) :: tsurf
      REAL(KIND(1D0)), INTENT(in) :: UStar
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: wu_nsurf

      REAL(KIND(1D0)), INTENT(in) :: z0m
      REAL(KIND(1D0)), INTENT(in) :: zdm
      REAL(KIND(1D0)), INTENT(in) :: zenith_deg

      REAL(KIND(1D0)), DIMENSION(5), INTENT(OUT) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5), INTENT(out) :: dataOutLineSUEWS
      ! REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutSnow-5),INTENT(out) :: dataOutLineSnow
      ! REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5),INTENT(out) :: dataOutLineESTM
      ! INTEGER:: is
      REAL(KIND(1D0)) :: LAI_wt
      REAL(KIND(1D0)) :: RH2_pct ! RH2 in percentage

      ! the variables below with '_x' endings stand for 'exported' values
      REAL(KIND(1D0)) :: ResistSurf_x
      REAL(KIND(1D0)) :: surf_chang_per_tstep_x
      REAL(KIND(1D0)) :: l_mod_x
      REAL(KIND(1D0)) :: bulkalbedo
      REAL(KIND(1D0)) :: smd_nsurf_x(nsurf)
      REAL(KIND(1D0)) :: state_x(nsurf)
      REAL(KIND(1D0)) :: wu_DecTr
      REAL(KIND(1D0)) :: wu_EveTr
      REAL(KIND(1D0)) :: wu_Grass

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

      INTEGER, INTENT(in) :: iy
      INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: it
      INTEGER, INTENT(in) :: imin

      INTEGER, INTENT(in) :: nlayer
      REAL(KIND(1D0)), INTENT(in) :: dectime

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_out_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qs_surf
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_out_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: Qn_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QS_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QE_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QH_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_out_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: Qn_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QS_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QE_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: QH_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_wall

      REAL(KIND(1D0)), DIMENSION(5), INTENT(OUT) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5), INTENT(out) :: dataOutLineESTMExt
      ! REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutSnow-5),INTENT(out) :: dataOutLineSnow
      ! REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5),INTENT(out) :: dataOutLineESTM
      ! INTEGER:: is
      REAL(KIND(1D0)) :: LAI_wt
      REAL(KIND(1D0)) :: RH2_pct ! RH2 in percentage

      ! the variables below with '_x' endings stand for 'exported' values
      REAL(KIND(1D0)) :: ResistSurf_x
      REAL(KIND(1D0)) :: surf_chang_per_tstep_x
      REAL(KIND(1D0)) :: l_mod_x
      REAL(KIND(1D0)) :: bulkalbedo
      REAL(KIND(1D0)) :: smd_nsurf_x(nsurf)
      REAL(KIND(1D0)) :: state_x(nsurf)
      REAL(KIND(1D0)) :: wu_DecTr
      REAL(KIND(1D0)) :: wu_EveTr
      REAL(KIND(1D0)) :: wu_Grass

      ! date & time:
      datetimeLine = [ &
                     REAL(iy, KIND(1D0)), REAL(id, KIND(1D0)), &
                     REAL(it, KIND(1D0)), REAL(imin, KIND(1D0)), dectime]
      !Define the overall output matrix to be printed out step by step
      dataoutlineESTMExt = [ &
                           tsfc_out_surf, qs_surf, & !output
                           fill_result(tsfc_out_roof, n_fill), &
                           fill_result(Qn_roof, n_fill), &
                           fill_result(QS_roof, n_fill), &
                           fill_result(QE_roof, n_fill), &
                           fill_result(QH_roof, n_fill), &
                           fill_result(state_roof, n_fill), &
                           fill_result(soilstore_roof, n_fill), &
                           fill_result(tsfc_out_wall, n_fill), &
                           fill_result(Qn_wall, n_fill), &
                           fill_result(QS_wall, n_fill), &
                           fill_result(QE_wall, n_fill), &
                           fill_result(QH_wall, n_fill), &
                           fill_result(state_wall, n_fill), &
                           fill_result(soilstore_wall, n_fill) &
                           ]
      ! set invalid values to NAN
      ! dataOutLineSUEWS = set_nan(dataOutLineSUEWS)

      !====================update output line end==============================

   END SUBROUTINE ESTMExt_update_outputLine
   !========================================================================

   FUNCTION fill_result(res_valid, n_fill) RESULT(res_filled)
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: res_valid
      INTEGER, INTENT(IN) :: n_fill
      REAL(KIND(1D0)), DIMENSION(n_fill) :: res_filled

      REAL(KIND(1D0)), PARAMETER :: NAN = -999

      res_filled = RESHAPE(res_valid, [n_fill], pad=[NAN])
   END FUNCTION fill_result

   !==============Update output arrays=========================
   SUBROUTINE SUEWS_update_output( &
      SnowUse, storageheatmethod, & !input
      ReadLinesMetdata, NumberOfGrids, &
      ir, gridiv, &
      datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, dataOutLineBEERS, &
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

      REAL(KIND(1D0)), DIMENSION(5), INTENT(in) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5), INTENT(in) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5), INTENT(in) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5), INTENT(in) :: dataOutLineESTMExt
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5), INTENT(in) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5), INTENT(in) :: dataoutLineRSL
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutBEERS - 5), INTENT(in) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutDebug - 5), INTENT(in) :: dataOutLineDebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutSPARTACUS - 5), INTENT(in) :: dataOutLineSPARTACUS

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
      dataOutSUEWS(ir, 1:ncolumnsDataOutSUEWS, Gridiv) = [datetimeLine, (dataOutLineSUEWS)]
      ! dataOutSUEWS(ir, 1:ncolumnsDataOutSUEWS, Gridiv) = [datetimeLine, set_nan(dataOutLineSUEWS)]
      dataOutRSL(ir, 1:ncolumnsDataOutRSL, Gridiv) = [datetimeLine, (dataoutLineRSL)]
      dataOutDebug(ir, 1:ncolumnsDataOutDebug, Gridiv) = [datetimeLine, (dataOutLineDebug)]
      dataOutSPARTACUS(ir, 1:ncolumnsDataOutSPARTACUS, Gridiv) = [datetimeLine, (dataOutLineSPARTACUS)]
      ! dataOutRSL(ir, 1:ncolumnsDataOutRSL, Gridiv) = [datetimeLine, set_nan(dataoutLineRSL)]
      dataOutBEERS(ir, 1:ncolumnsdataOutBEERS, Gridiv) = [datetimeLine, set_nan(dataOutLineBEERS)]
      ! ! set invalid values to NAN
      ! dataOutSUEWS(ir,6:ncolumnsDataOutSUEWS,Gridiv)=set_nan(dataOutSUEWS(ir,6:ncolumnsDataOutSUEWS,Gridiv))

      IF (SnowUse == 1) THEN
         dataOutSnow(ir, 1:ncolumnsDataOutSnow, Gridiv) = [datetimeLine, set_nan(dataOutLineSnow)]
      END IF

      IF (storageheatmethod == 4) THEN
         dataOutESTM(ir, 1:ncolumnsDataOutESTM, Gridiv) = [datetimeLine, set_nan(dataOutLineESTM)]
      END IF

      IF (storageheatmethod == 5) THEN
         dataOutESTMExt(ir, 1:ncolumnsDataOutESTMExt, Gridiv) = [datetimeLine, set_nan(dataOutLineESTMExt)]
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
      sfr_surf, & !input
      vegfraction, ImpervFraction, PervFraction, NonWaterFraction) ! output
      IMPLICIT NONE

      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf
      REAL(KIND(1D0)), INTENT(OUT) :: VegFraction
      REAL(KIND(1D0)), INTENT(OUT) :: ImpervFraction
      REAL(KIND(1D0)), INTENT(OUT) :: PervFraction
      REAL(KIND(1D0)), INTENT(OUT) :: NonWaterFraction

      VegFraction = sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf)
      ImpervFraction = sfr_surf(PavSurf) + sfr_surf(BldgSurf)
      PervFraction = 1 - ImpervFraction
      NonWaterFraction = 1 - sfr_surf(WaterSurf)

   END SUBROUTINE SUEWS_cal_surf

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
      AerodynamicResistanceMethod, AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, & ! input&inout in alphabetical order
      AH_SLOPE_Heating, &
      alb, AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
      AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
      alpha_bioCO2, alpha_enh_bioCO2, alt, BaseT, BaseTe, &
      BaseTMethod, &
      BaseT_HC, beta_bioCO2, beta_enh_bioCO2, bldgH, CapMax_dec, CapMin_dec, &
      chAnOHM, CO2PointSource, cpAnOHM, CRWmax, CRWmin, DayWat, DayWatPer, &
      DecTreeH, DiagMethod, Diagnose, DiagQN, DiagQS, DRAINRT, &
      dt_since_start, dqndt, qn_av, dqnsdt, qn_s_av, &
      EF_umolCO2perJ, emis, EmissionsMethod, EnEF_v_Jkm, endDLS, EveTreeH, FAIBldg, &
      FAIDecTree, FAIEveTree, Faut, FcEF_v_kgkm, FlowChange, &
      FrFossilFuel_Heat, FrFossilFuel_NonHeat, G1, G2, G3, G4, G5, G6, GDD_id, &
      GDDFull, Gridiv, gsModel, H_maintain, HDD_id, HumActivity_24hr, &
      IceFrac, Ie_a, Ie_end, Ie_m, Ie_start, &
      InternalWaterUse_h, &
      IrrFracPaved, IrrFracBldgs, &
      IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
      IrrFracBSoil, IrrFracWater, &
      EvapMethod, &
      kkAnOHM, Kmax, LAI_id, LAICalcYes, LAIMax, LAIMin, &
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
      SnowLimBldg, SnowLimPaved, SnowPack, SnowProf_24hr, SnowUse, SoilDepth, &
      StabilityMethod, startDLS, &
      soilstore_surf, SoilStoreCap_surf, state_surf, StateLimit_surf, WetThresh_surf, &
      soilstore_roof, SoilStoreCap_roof, state_roof, StateLimit_roof, WetThresh_roof, &
      soilstore_wall, SoilStoreCap_wall, state_wall, StateLimit_wall, WetThresh_wall, &
      StorageHeatMethod, StoreDrainPrm, SurfaceArea, Tair_av, tau_a, tau_f, tau_r, &
      BaseT_Cooling, BaseT_Heating, TempMeltFact, TH, &
      theta_bioCO2, timezone, TL, TrafficRate, TrafficUnits, &
      sfr_roof, sfr_wall, sfr_surf, &
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
      dataOutBlockSUEWS, dataOutBlockSnow, dataOutBlockESTM, dataOutBlockRSL, dataOutBlockBEERS, & !output
      dataOutBlockDebug, dataOutBlockSPARTACUS, dataOutBlockESTMExt, &
      DailyStateBlock)

      IMPLICIT NONE
      ! input:
      ! met forcing block
      REAL(KIND(1D0)), DIMENSION(len_sim, 24), INTENT(IN) :: MetForcingBlock
      INTEGER, INTENT(IN) :: len_sim
      ! input variables
      INTEGER, INTENT(IN) :: nlayer ! number of vertical layers in urban canyon
      INTEGER, INTENT(IN) :: AerodynamicResistanceMethod
      INTEGER, INTENT(IN) :: BaseTMethod
      INTEGER, INTENT(IN) :: Diagnose
      INTEGER, INTENT(IN) :: DiagQN
      INTEGER, INTENT(IN) :: DiagQS
      INTEGER, INTENT(IN) :: startDLS
      INTEGER, INTENT(IN) :: endDLS
      INTEGER, INTENT(IN) :: EmissionsMethod
      INTEGER, INTENT(IN) :: Gridiv
      INTEGER, INTENT(IN) :: gsModel
      INTEGER, INTENT(IN) :: Ie_end
      INTEGER, INTENT(IN) :: Ie_start
      INTEGER, INTENT(IN) :: EvapMethod
      INTEGER, INTENT(IN) :: LAICalcYes
      INTEGER, INTENT(in) :: DiagMethod
      INTEGER, INTENT(IN) :: NetRadiationMethod
      INTEGER, INTENT(IN) :: OHMIncQF
      INTEGER, INTENT(IN) :: RoughLenHeatMethod
      INTEGER, INTENT(IN) :: RoughLenMomMethod
      INTEGER, INTENT(IN) :: SMDMethod
      INTEGER, INTENT(IN) :: SnowUse
      INTEGER, INTENT(IN) :: StabilityMethod
      INTEGER, INTENT(IN) :: StorageHeatMethod
      INTEGER, INTENT(IN) :: tstep
      INTEGER, INTENT(IN) :: tstep_prev ! tstep size of the previous step
      ! dt_since_start is intentionally made as inout to keep naming consistency with the embedded subroutine
      INTEGER, INTENT(inout) :: dt_since_start ! time since simulation starts [s]
      INTEGER, INTENT(IN) :: veg_type
      INTEGER, INTENT(IN) :: WaterUseMethod

      INTEGER, DIMENSION(NVEGSURF), INTENT(IN) :: LAIType

      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_DecTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_EveTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_Grass
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_DecTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_EveTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_Grass
      REAL(KIND(1D0)), INTENT(IN) :: alt
      ! REAL(KIND(1D0)),INTENT(IN)::avkdn
      ! REAL(KIND(1D0)),INTENT(IN)::avRh
      ! REAL(KIND(1D0)),INTENT(IN)::avU1
      REAL(KIND(1D0)), INTENT(IN) :: BaseT_HC
      REAL(KIND(1D0)), INTENT(IN) :: bldgH
      REAL(KIND(1D0)), INTENT(IN) :: CapMax_dec
      REAL(KIND(1D0)), INTENT(IN) :: CapMin_dec
      REAL(KIND(1D0)), INTENT(IN) :: CO2PointSource
      REAL(KIND(1D0)), INTENT(IN) :: CRWmax
      REAL(KIND(1D0)), INTENT(IN) :: CRWmin
      REAL(KIND(1D0)), INTENT(IN) :: DecTreeH
      REAL(KIND(1D0)), INTENT(IN) :: DRAINRT
      REAL(KIND(1D0)), INTENT(IN) :: EF_umolCO2perJ
      REAL(KIND(1D0)), INTENT(IN) :: EnEF_v_Jkm
      REAL(KIND(1D0)), INTENT(IN) :: EveTreeH
      REAL(KIND(1D0)), INTENT(IN) :: FAIBldg
      REAL(KIND(1D0)), INTENT(IN) :: FAIDecTree
      REAL(KIND(1D0)), INTENT(IN) :: FAIEveTree
      REAL(KIND(1D0)), INTENT(IN) :: Faut
      ! REAL(KIND(1D0)),INTENT(IN)::fcld_obs
      REAL(KIND(1D0)), INTENT(IN) :: FlowChange
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_Heat
      REAL(KIND(1D0)), INTENT(IN) :: FrFossilFuel_NonHeat
      REAL(KIND(1D0)), INTENT(IN) :: G1
      REAL(KIND(1D0)), INTENT(IN) :: G2
      REAL(KIND(1D0)), INTENT(IN) :: G3
      REAL(KIND(1D0)), INTENT(IN) :: G4
      REAL(KIND(1D0)), INTENT(IN) :: G5
      REAL(KIND(1D0)), INTENT(IN) :: G6
      REAL(KIND(1D0)), INTENT(IN) :: H_maintain
      REAL(KIND(1D0)), INTENT(IN) :: InternalWaterUse_h
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracPaved
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBldgs
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracEveTr
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracDecTr
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracGrass
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracBSoil
      REAL(KIND(1D0)), INTENT(IN) :: IrrFracWater
      REAL(KIND(1D0)), INTENT(IN) :: Kmax
      ! REAL(KIND(1D0)),INTENT(IN)::LAI_obs
      REAL(KIND(1D0)), INTENT(IN) :: lat
      ! REAL(KIND(1D0)),INTENT(IN)::ldown_obs
      REAL(KIND(1D0)), INTENT(IN) :: lng
      REAL(KIND(1D0)), INTENT(IN) :: MaxFCMetab
      REAL(KIND(1D0)), INTENT(IN) :: MaxQFMetab
      REAL(KIND(1D0)), INTENT(IN) :: MinFCMetab
      REAL(KIND(1D0)), INTENT(IN) :: MinQFMetab
      REAL(KIND(1D0)), INTENT(IN) :: NARP_EMIS_SNOW
      REAL(KIND(1D0)), INTENT(IN) :: NARP_TRANS_SITE
      REAL(KIND(1D0)), INTENT(IN) :: PipeCapacity
      REAL(KIND(1D0)), INTENT(IN) :: PopDensNighttime
      REAL(KIND(1D0)), INTENT(IN) :: PorMax_dec
      REAL(KIND(1D0)), INTENT(IN) :: PorMin_dec
      ! REAL(KIND(1D0)),INTENT(IN)::Precip
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimit
      REAL(KIND(1D0)), INTENT(IN) :: PrecipLimitAlb
      ! REAL(KIND(1D0)),INTENT(IN)::Press_hPa
      ! REAL(KIND(1D0)),INTENT(IN)::qh_obs
      ! REAL(KIND(1D0)),INTENT(IN)::qn1_obs
      ! REAL(KIND(1D0)),INTENT(IN)::qs_obs
      ! REAL(KIND(1D0)),INTENT(IN)::qf_obs
      REAL(KIND(1D0)), INTENT(IN) :: RadMeltFact
      REAL(KIND(1D0)), INTENT(IN) :: RAINCOVER
      REAL(KIND(1D0)), INTENT(IN) :: RainMaxRes
      REAL(KIND(1D0)), INTENT(IN) :: RunoffToWater
      REAL(KIND(1D0)), INTENT(IN) :: S1
      REAL(KIND(1D0)), INTENT(IN) :: S2
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(IN) :: SnowAlbMin
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMax
      REAL(KIND(1D0)), INTENT(IN) :: SnowDensMin
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimBldg
      REAL(KIND(1D0)), INTENT(IN) :: SnowLimPaved
      ! REAL(KIND(1D0)),INTENT(IN)::snowFrac_obs
      REAL(KIND(1D0)), INTENT(IN) :: SurfaceArea
      REAL(KIND(1D0)), INTENT(IN) :: tau_a
      REAL(KIND(1D0)), INTENT(IN) :: tau_f
      REAL(KIND(1D0)), INTENT(IN) :: tau_r
      ! REAL(KIND(1D0)),INTENT(IN)::Temp_C
      REAL(KIND(1D0)), INTENT(IN) :: TempMeltFact
      REAL(KIND(1D0)), INTENT(IN) :: TH
      REAL(KIND(1D0)), INTENT(IN) :: timezone
      REAL(KIND(1D0)), INTENT(IN) :: TL
      REAL(KIND(1D0)), INTENT(IN) :: TrafficUnits
      ! REAL(KIND(1D0)),INTENT(IN)::xsmd
      REAL(KIND(1D0)), INTENT(IN) :: Z
      REAL(KIND(1D0)), INTENT(IN) :: z0m_in
      REAL(KIND(1D0)), INTENT(IN) :: zdm_in

      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_MIN
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Cooling
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Heating
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: FcEF_v_kgkm
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: QF0_BEU
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_A
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_B
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: Qf_C
      ! REAL(KIND(1D0)), DIMENSION(2), INTENT(IN)        ::Numcapita
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: PopDensDaytime
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Cooling
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Heating
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: TrafficRate
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_a
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: Ie_m
      REAL(KIND(1D0)), DIMENSION(3), INTENT(IN) :: MaxConductance
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWat
      REAL(KIND(1D0)), DIMENSION(7), INTENT(IN) :: DayWatPer
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshSW
      REAL(KIND(1D0)), DIMENSION(nsurf + 1), INTENT(IN) :: OHM_threshWD
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: chAnOHM
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: cpAnOHM
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: emis
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: kkAnOHM
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SatHydraulicConduct
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SnowPackLimit
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilDepth
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: SoilStoreCap_surf
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: StateLimit_surf
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(IN) :: WetThresh_surf
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: alpha_enh_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseT
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: BaseTe
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: beta_enh_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: GDDFull
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMax
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: LAIMin
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: min_res_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_a
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: resp_b
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: SDDFull
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: SnowProf_24hr
      REAL(KIND(1D0)), DIMENSION(NVEGSURF), INTENT(IN) :: theta_bioCO2
      REAL(KIND(1D0)), DIMENSION(4, NVEGSURF), INTENT(IN) :: LAIPower
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, 4, 3), INTENT(IN) :: OHM_coef
      REAL(KIND(1D0)), DIMENSION(NSURF + 1, NSURF - 1), INTENT(IN) :: WaterDist
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: Ts5mindata_ir

      ! diurnal profile values for 24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: AHProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: HumActivity_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: PopProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: TraffProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfA_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: WUProfM_24hr
      ! ########################################################################################

      ! ########################################################################################
      ! inout variables
      ! OHM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_av
      REAL(KIND(1D0)), INTENT(INOUT) :: dqndt
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_s_av
      REAL(KIND(1D0)), INTENT(INOUT) :: dqnsdt

      ! snow related:
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowfallCum
      REAL(KIND(1D0)), INTENT(INOUT) :: SnowAlb
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: IceFrac
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowWater
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowDens
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowFrac
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: SnowPack

      ! water balance related:
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: soilstore_surf
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: state_surf
      REAL(KIND(1D0)), DIMENSION(6, NSURF), INTENT(INOUT) :: StoreDrainPrm

      ! phenology related:
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: alb
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: GDD_id !Growing Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: SDD_id !Senescence Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: LAI_id !LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: DecidCap_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albDecTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albEveTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albGrass_id
      REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id
      REAL(KIND(1D0)), INTENT(INOUT) :: Tmin_id
      REAL(KIND(1D0)), INTENT(INOUT) :: Tmax_id
      REAL(KIND(1D0)), INTENT(INOUT) :: lenday_id

      ! anthropogenic heat related:
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating Degree Days (see SUEWS_DailyState.f95)

      ! water use related:
      REAL(KIND(1D0)), DIMENSION(9), INTENT(INOUT) :: WUDay_id

      ! ESTM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: Tair_av

      !  ! extended for ESTM_ext, TS 20 Jan 2022
      ! input arrays: standard suews surfaces
      ! REAL(KIND(1D0)), DIMENSION(nroof) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(inout) :: temp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof
      ! input arrays: standard suews surfaces
      ! REAL(KIND(1D0)), DIMENSION(nwall) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(inout) :: temp_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall
      ! input arrays: standard suews surfaces
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(INOUT) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(inout) :: temp_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf

      ! SPARTACUS input variables
      INTEGER, INTENT(IN) :: n_vegetation_region_urban, &
                             n_stream_sw_urban, n_stream_lw_urban
      REAL(KIND(1D0)), INTENT(IN) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
                                     veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                                     veg_fsd_const, veg_contact_fraction_const, &
                                     ground_albedo_dir_mult_fact
      LOGICAL, INTENT(IN) :: use_sw_direct_albedo
      REAL(KIND(1D0)), DIMENSION(nlayer + 1), INTENT(IN) :: height
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_frac
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: building_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: veg_scale
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: alb_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: emis_wall
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: roof_albedo_dir_mult_fact
      REAL(KIND(1D0)), DIMENSION(nspec, nlayer), INTENT(IN) :: wall_specular_frac
      ! ########################################################################################

      ! ####################################################################################
      ! ESTM_EXT
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_roof

      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: SoilStoreCap_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: StateLimit_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: wetthresh_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_wall

      ! ########################################################################################
      ! output variables
      ! REAL(KIND(1D0)),DIMENSION(:,:,:),ALLOCATABLE,INTENT(OUT) ::datetimeBlock
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSUEWS), INTENT(OUT) :: dataOutBlockSUEWS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSnow), INTENT(OUT) :: dataOutBlockSnow
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTM), INTENT(OUT) :: dataOutBlockESTM
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTMExt), INTENT(OUT) :: dataOutBlockESTMExt
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutRSL), INTENT(OUT) :: dataOutBlockRSL
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsdataOutBEERS), INTENT(OUT) :: dataOutBlockBEERS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDebug), INTENT(OUT) :: dataOutBlockDebug
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSPARTACUS), INTENT(OUT) :: dataOutBlockSPARTACUS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDailyState), INTENT(OUT) :: DailyStateBlock
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

      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5) :: dataOutLineESTMExt
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5) :: dataOutLineRSL
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutSOLWEIG - 5) :: dataOutLineSOLWEIG
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug - 5) :: dataOutLinedebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5) :: dataOutLineSPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5) :: DailyStateLine

      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSUEWS, 1) :: dataOutBlockSUEWS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSnow, 1) :: dataOutBlockSnow_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTM, 1) :: dataOutBlockESTM_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTMExt, 1) :: dataOutBlockESTMExt_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutRSL, 1) :: dataOutBlockRSL_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsdataOutBEERS, 1) :: dataOutBlockBEERS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDebug, 1) :: dataOutBlockDebug_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSPARTACUS, 1) :: dataOutBlockSPARTACUS_X
      ! REAL(KIND(1d0)),DIMENSION(len_sim,ncolumnsDataOutDailyState,1) ::DailyStateBlock_X

      REAL(KIND(1D0)), DIMENSION(10, 10) :: MetForcingData_grid ! fake array as a placeholder

      ! CHARACTER(len=150):: FileStateInit
      ! CHARACTER(len=4):: year_txt
      ! CHARACTER(len=3):: id_text
      ! CHARACTER(len=2):: it_text, imin_text

      ! get initial dt_since_start_x from dt_since_start, dt_since_start_x is used for Qn averaging. TS 28 Nov 2018
      ! dt_since_start = dt_since_start

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

         CALL SUEWS_cal_Main( &
            AerodynamicResistanceMethod, AH_MIN, AHProf_24hr, AH_SLOPE_Cooling, & ! input&inout in alphabetical order
            AH_SLOPE_Heating, &
            alb, AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
            AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
            alpha_bioCO2, alpha_enh_bioCO2, alt, avkdn, avRh, avU1, BaseT, BaseTe, &
            BaseTMethod, &
            BaseT_HC, beta_bioCO2, beta_enh_bioCO2, bldgH, CapMax_dec, CapMin_dec, &
            chAnOHM, CO2PointSource, cpAnOHM, CRWmax, CRWmin, DayWat, DayWatPer, &
            DecTreeH, DiagMethod, Diagnose, DiagQN, DiagQS, DRAINRT, &
            dt_since_start, dqndt, qn_av, dqnsdt, qn_s_av, &
            EF_umolCO2perJ, emis, EmissionsMethod, EnEF_v_Jkm, endDLS, EveTreeH, FAIBldg, &
            FAIDecTree, FAIEveTree, Faut, FcEF_v_kgkm, fcld_obs, FlowChange, &
            FrFossilFuel_Heat, FrFossilFuel_NonHeat, G1, G2, G3, G4, G5, G6, GDD_id, &
            GDDFull, Gridiv, gsModel, H_maintain, HDD_id, HumActivity_24hr, &
            IceFrac, id, Ie_a, Ie_end, Ie_m, Ie_start, imin, &
            InternalWaterUse_h, &
            IrrFracPaved, IrrFracBldgs, &
            IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
            IrrFracBSoil, IrrFracWater, &
            isec, it, EvapMethod, &
            iy, kkAnOHM, Kmax, LAI_id, LAICalcYes, LAIMax, LAIMin, LAI_obs, &
            LAIPower, LAIType, lat, lenDay_id, ldown_obs, lng, MaxConductance, MaxFCMetab, MaxQFMetab, &
            SnowWater, MetForcingData_grid, MinFCMetab, MinQFMetab, min_res_bioCO2, &
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
            sfr_roof, sfr_wall, sfr_surf, &
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
            datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, & !output
            dataOutLineBEERS, & !output
            dataOutLineDebug, dataOutLineSPARTACUS, &
            dataOutLineESTMExt, &
            DailyStateLine) !output

         ! update dt_since_start_x for next iteration, dt_since_start_x is used for Qn averaging. TS 28 Nov 2018
         dt_since_start = dt_since_start + tstep

         !============ update DailyStateBlock ===============
         DailyStateBlock(ir, :) = [datetimeLine, DailyStateLine]

         !============ write out results ===============
         ! works at each timestep
         CALL SUEWS_update_output( &
            SnowUse, storageheatmethod, & !input
            len_sim, 1, &
            ir, gridiv_x, datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, & !input
            dataoutLineRSL, dataOutLineBEERS, dataOutLinedebug, dataOutLineSPARTACUS, dataOutLineESTMExt, & !input
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
      ! DailyStateBlock=DailyStateBlock_X(:,:,1)

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
