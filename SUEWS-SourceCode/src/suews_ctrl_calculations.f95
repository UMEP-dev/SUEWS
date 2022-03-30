!This subroutine does the actual calculations of the SUEWS code (mostly old SUEWS_Temporal).
!Made by LJ and HW Oct 2014
!Gives in the grid ID (Gridiv) and number of line in the met forcing data to be analyzed (ir)
!Last modification
!
!Last modification:
! TS  02 May 2018 - added explict interfaces
! LJ  15 Jun 2017 - Add parts where NonWaterFraction=0 is used as a divider so that the code won't crash with 100% water
! TS  20 May 2017 - Add surface-level diagonostics: T2, Q2, U10
! HCW 09 Dec 2016 - Add zenith and azimuth to output file
! HCw 24 Aug 2016 - smd and state for each surface set to NAN in output file if surface does not exist.
!                 - Added Fc to output file
! HCW 21 Jul 2016 - Set soil variables to -999 in output when grid is 100% water surface.
! HCW 29 Jun 2016 - Commented out StateDay and SoilMoistDay as creates jumps and should not be needed.
!                   Would not work unless each met block consists of a whole day for each grid.
! TS 09 Mar 2016  - Added AnOHM subroutine to calculate heat storage
! HCW 10 Mar 2016 - Calculation of soil moisture deficit of vegetated surfaces added (vsmd)
! LJ 2 Jan 2016   - Calculation of snow fraction moved from SUEWS_Calculations to SUEWS_Snow
! HCW 12 Nov 2015 - Added z0m and zdm to output file
! HCW 05 Nov 2015 - Changed Kawai et al. (2007) z0v calculation so VegFraction(veg+soil) rather than veg_fr(veg+soil+water) is used.
! HCW 29 Jun 2015 - Added albEveTr and albGrass
! HCW 25 Jun 2015 - Fixed bug in LAI calculation at year change.
!                 - Changed AlbDec to use id value (not (id-1) value) to avoid issues at year change.
! HCW 27 Apr 2015 - Correction to tot_chang_per_tstep calculation (water balance should now close)
! HCW 16 Feb 2015 - Updated water balance calculations
!                 - Corrected area-averaged calculations (soil moisture, drain, two versions of state with/out water)
!                 - Replaced soilmoist_state variable with SoilState (as seems to be duplicate)
! HCW 15 Jan 2015 - Added switch OHMIncQF to calculate QS with (1, default) or without (0) QF added to QSTAR
! To do
!      - add iy and imin to output files (may impact LUMPS_RunoffFromGrid)
!      - phase out _per_interval water balance variables
!      - renormalise by NonWaterFraction where necessary
!      - Update Snow subroutines similarly in terms of water balance
!==================================================================

SUBROUTINE SUEWS_Calculations(Gridiv, ir, iMB, irMax)
   USE data_in, ONLY: diagnose, ah_min, ah_slope_cooling, ah_slope_heating, &
                      alt, avkdn, avrh, avu1, BaseT_HC, diagqn, diagqs, drainrt, co2pointsource, CBLuse, &
                      ef_umolco2perj, DiagMethod, BaseTMethod, emissionsmethod, enef_v_jkm, enddls, fcef_v_kgkm, fcld_obs, &
                      frfossilfuel_heat, frfossilfuel_nonheat, EvapMethod, &
                      LAIcalcyes, LAI_obs, lat, ldown_obs, lng, maxfcmetab, maxqfmetab, &
                      minfcmetab, minqfmetab, netradiationmethod, ohmincqf, &
                      popdensdaytime, popdensnighttime, &
                      precip, press_hpa, qf0_beu, qf_a, qf_b, qf_c, &
                      qe_obs, qh_obs, qn1_obs, qs_obs, qf_obs, &
                      raincover, rainmaxres, &
                      roughlenmommethod, smdmethod, snowFrac_obs, SnowUse, startdls, &
                      storageheatmethod, BaseT_Cooling, BaseT_Heating, temp_c, &
                      timezone, trafficrate, trafficunits, waterusemethod, wu_m3, xsmd
   USE time, ONLY: iy, id, it, imin, isec, dectime, dt_since_start
   USE allocateArray, ONLY: &
      alb, &
      AlbMax_DecTr, AlbMax_EveTr, AlbMax_grass, &
      AlbMin_dectr, AlbMin_evetr, AlbMin_grass, &
      alpha_bioco2, alpha_enh_bioco2, baset, basete, &
      beta_bioco2, beta_enh_bioco2, capmax_dec, capmin_dec, &
      chanohm, cpanohm, emis, GDD_id, GDDfull, &
      Tmin_id, &
      Tmax_id, &
      lenDay_id, &
      SDD_id, &
      HDD_id, &
      DecidCap_id, porosity_id, &
      albDecTr_id, albEveTr_id, albGrass_id, &
      icefrac, kkanohm, &
      LAI_id, LAImax, LAImin, LAIpower, LAItype, maxconductance, &
      SnowWater, metforcingdata_grid, min_res_bioco2, &
      narp_emis_snow, narp_trans_site, &
      ohm_coef, ohm_threshsw, ohm_threshwd, &
      pormax_dec, pormin_dec, &
      tair_av, &
      dqndt, qn1_av, &
      dqnsdt, qn1_s_av, &
      resp_a, resp_b, sathydraulicconduct, sddfull, &
      sfr_surf, SnowPackLimit, snowdens, SnowFrac, snowpack, &
      soildepth, soilstore_id, SoilStoreCap, state_id, statelimit, &
      StoreDrainPrm, theta_bioco2, ts5mindata_ir, &
      waterdist, wetthresh, &
      WUDay_id, &
      AHProf_24Hr, HumActivity_24Hr, PopProf_24Hr, TraffProf_24Hr, WUProfA_24hr, WUProfM_24hr, &
      datetimeline, dataoutlinesuews, dataoutlinesnow, &
      dataoutlineestm, dataoutlineRSL, dataOutLineBEERS, &
      dataOutLineDebug, dataOutLineDebug, dataOutLineSPARTACUS, &
      dailystateline, dataoutdailystate, &
      dataoutsuews, dataoutsnow, dataoutestm, dataOutESTMExt, dataoutRSL, dataOutBEERS, &
      dataoutBL, dataOutDebug, dataOutSPARTACUS, dataOutLineESTMExt, &
      nlayer, n_vegetation_region_urban, &
      n_stream_sw_urban, n_stream_lw_urban, &
      sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
      veg_fsd_const, veg_contact_fraction_const, &
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, &
      sfr_roof, sfr_wall, sfr_surf, &
      tsfc_roof, tsfc_wall, tsfc_surf, &
      tin_roof, tin_wall, tin_surf, &
      temp_roof, temp_wall, temp_surf, &
      k_roof, k_wall, k_surf, &
      cp_roof, cp_wall, cp_surf, &
      dz_roof, dz_wall, dz_surf, &
      height, building_frac, veg_frac, building_scale, veg_scale, & !input: SPARTACUS
      alb_roof, emis_roof, alb_wall, emis_wall, &
      roof_albedo_dir_mult_fact, wall_specular_frac
   USE sues_data, ONLY: &
      aerodynamicresistancemethod, daywat, daywatper, faut, flowchange, &
      H_maintain, &
      ie_a, ie_end, ie_m, ie_start, internalwateruse_h, &
      IrrFracPaved, IrrFracBldgs, &
      IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
      IrrFracBSoil, IrrFracWater, &
      pipecapacity, roughlenheatmethod, runofftowater, stabilitymethod, &
      surfacearea, tstep, tstep_prev, &
      qhforCBL, qeforCBL, qh_choice, nsh_real, UStar, psih, is
   USE snowMod, ONLY: &
      crwmax, crwmin, preciplimit, preciplimitalb, radmeltfact, &
      snowalb, snowAlbMax, snowAlbMin, &
      snowdensmax, snowdensmin, snowfallcum, SnowLimBldg, &
      snowlimpaved, SnowProf_24hr, &
      tau_a, tau_f, tau_r, tempmeltfact
   USE gis_data, ONLY: &
      bldgh, dectreeh, evetreeh, faibldg, faidectree, faievetree, veg_type
   USE initial, ONLY: NumberOfGrids, ReadLinesMetdata
   USE mod_z, ONLY: z, z0m_in, zdm_in
   USE SUEWS_Driver, ONLY: suews_update_output
   USE resist, ONLY: g1, g2, g3, g4, g5, g6, gsmodel, kmax, s1, s2, th, tl
   USE DailyState_module, ONLY: SUEWS_update_DailyState
   USE SUEWS_Driver, ONLY: SUEWS_cal_Main
   USE BLUEWS_module, ONLY: CBL
   USE moist, ONLY: avcp, avdens, es_hPa, lv_J_kg

   IMPLICIT NONE

   INTEGER :: Gridiv
   INTEGER :: ir
   INTEGER :: iMB
   INTEGER :: irMax

   !==================================================================

   !Translate all data to the variables used in the model calculations
   IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_Translate...'
   CALL SUEWS_Translate(Gridiv, ir, iMB)

   ! ASSOCIATE (v => dz_roof(1, 1:5))
   !    PRINT *, 'dz_roof before driver', v, SIZE(v)
   ! END ASSOCIATE
   !PRINT *,''
   !PRINT *,'Calling SUEWS_cal_Main'
   IF (Diagnose == 1) PRINT *, 'Calling SUEWS_cal_Main...'
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
      dt_since_start, dqndt, qn1_av, dqnsdt, qn1_s_av, &
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
      soilstore_id, SoilStoreCap, StabilityMethod, startDLS, state_id, StateLimit, &
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
      WaterDist, WaterUseMethod, WetThresh, wu_m3, &
      WUDay_id, DecidCap_id, albDecTr_id, albEveTr_id, albGrass_id, porosity_id, &
      WUProfA_24hr, WUProfM_24hr, xsmd, Z, z0m_in, zdm_in, &
      datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, & !output
      dataOutLineBEERS, & !output
      dataOutLineDebug, dataOutLineSPARTACUS, &
      dataOutLineESTMExt, &
      DailyStateLine) !output

   !============ update and write out SUEWS_cal_DailyState ===============
   ! only works at the last timestep of a day
   CALL SUEWS_update_DailyState( &
      id, datetimeLine, & !input
      Gridiv, NumberOfGrids, &
      DailyStateLine, &
      dataOutDailyState) !inout

   !============ write out results ===============
   ! works at each timestep
   ! print *, 'writing out results',dataOutLineESTMExt
   ! PRINT *, '=====writing out results====='
   CALL SUEWS_update_output( &
      SnowUse, storageheatmethod, & !input
      ReadLinesMetdata, NumberOfGrids, &
      ir, gridiv, datetimeLine, dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, dataOutLineBEERS, & !input
      dataOutLineDebug, dataOutLineSPARTACUS, dataOutLineESTMExt, &
      dataOutSUEWS, dataOutSnow, dataOutESTM, dataOutRSL, dataOutBEERS, & !inout
      dataOutDebug, dataOutSPARTACUS, dataOutESTMExt) !inout

   ! NB: CBL disabled for the moment for interface improvement
   ! NB: CBL be decoupled from SUEWS TS 10 Jun 2018

   IF (Qh_choice == 1) THEN !use QH and QE from SUEWS
      qhforCBL(Gridiv) = dataOutLineSUEWS(9)
      qeforCBL(Gridiv) = dataOutLineSUEWS(10)
   ELSEIF (Qh_choice == 2) THEN !use QH and QE from LUMPS
      qhforCBL(Gridiv) = dataOutLineSUEWS(11)
      qeforCBL(Gridiv) = dataOutLineSUEWS(12)
   ELSEIF (qh_choice == 3) THEN !use QH and QE from OBS
      qhforCBL(Gridiv) = qh_obs
      qeforCBL(Gridiv) = qe_obs
      IF (qh_obs < -900 .OR. qe_obs < -900) THEN ! observed data has a problem

         CALL ErrorHint(22, 'Unrealistic observed qh or qe_value for CBL.', qh_obs, qe_obs, qh_choice)

      END IF
   END IF
   IF (CBLuse >= 1) THEN ! If CBL is used, calculated Temp_C and RH are replaced with the obs.
      IF (Diagnose == 1) WRITE (*, *) 'Calling CBL...'

      UStar = dataOutLineSUEWS(55)
      !ir=1 indicates first row of each met data block
      CALL CBL(iy, id, it, imin, ir, Gridiv, qh_choice, dectime, &
               Temp_C, Press_hPa, avkdn, avu1, avrh, avcp, avdens, es_hPa, lv_J_kg, &
               nsh_real, tstep, UStar, psih, is, NumberOfGrids, &
               qhforCBL, qeforCBL, ReadLinesMetdata, dataOutBL)
   END IF

   ! NB: SOLWEIG can be treated as a separate part:
   ! NB: SOLWEIG is disabled for v2018a TS 10 Jun 2018
   ! ===================SOLWEIG OUTPUT ========================================
   ! IF (SOLWEIGuse==1) THEN
   !    IF (OutInterval==imin) THEN
   !       IF (RunForGrid==-999) THEN
   !          IF(Diagnose==1) WRITE(*,*) 'Calling SOLWEIG_2014a_core...'
   !          CALL SOLWEIG_2014a_core(iMB)
   !          SolweigCount=SolweigCount+1
   !       ELSE
   !          IF (Gridiv == RunForGrid) THEN
   !             IF(Diagnose==1) WRITE(*,*) 'Calling SOLWEIG_2014a_core...'
   !             CALL SOLWEIG_2014a_core(iMB)
   !             SolweigCount=SolweigCount+1
   !          ENDIF
   !       ENDIF
   !    ENDIF
   ! ELSE
   ! SOLWEIGpoi_out=0 ! NB: turn off SOLWEIG for the moment
   ! ENDIF
   ! ===================SOLWEIG END================================

   IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_TranslateBack...'
   CALL SUEWS_TranslateBack(Gridiv, ir, irMax)

END SUBROUTINE SUEWS_Calculations
