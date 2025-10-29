!========================================================================================
! SUEWS driver subroutines
! TS 31 Aug 2017: initial version
! TS 02 Oct 2017: added  as the generic wrapper
! TS 03 Oct 2017: added
!
! WRF-SUEWS COUPLING NOTE:
! This module supports both standalone SUEWS and WRF-SUEWS coupling.
! Key differences for WRF coupling:
! - tstep_prev: Allows adaptive timesteps (in standalone: always equals tstep)
! - Additional atmospheric variables passed from WRF
!========================================================================================
MODULE SUEWS_Driver
   ! only the following immutable objects are imported:
   ! 1. functions/subroutines
   ! 2. constant variables
   USE module_ctrl_type, ONLY: SUEWS_CONFIG, SURF_STORE_PRM, WATER_DIST_PRM, bioCO2_PRM, CONDUCTANCE_PRM, &
                            LAI_PRM, OHM_COEF_LC, OHM_PRM, SOIL_PRM, anthroHEAT_PRM, IRRIG_daywater, &
                            IRRIGATION_PRM, anthroEMIS_PRM, SNOW_PRM, SPARTACUS_PRM, SPARTACUS_LAYER_PRM, &
                            SUEWS_SITE, LUMPS_PRM, EHC_PRM, LC_PAVED_PRM, LC_BLDG_PRM, LC_DECTR_PRM, LC_EVETR_PRM, &
                            LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, anthroEmis_STATE, &
                            OHM_STATE, PHENOLOGY_STATE, SNOW_STATE, SUEWS_FORCING, SUEWS_TIMER, &
                            HYDRO_STATE, HEAT_STATE, &
                            ROUGHNESS_STATE, solar_State, atm_state, flag_STATE, &
                            SUEWS_STATE, SUEWS_DEBUG, STEBBS_STATE, BUILDING_ARCHETYPE_PRM, STEBBS_PRM, STEBBS_BLDG, &
                            SUEWS_STATE_BLOCK, &
                            output_line, output_block
   USE meteo, ONLY: qsatf, RH2qa, qa2RH
   USE module_phys_atmmoiststab, ONLY: cal_AtmMoist, cal_Stab, stab_psi_heat, stab_psi_mom, SUEWS_update_atmState
   USE module_phys_narp, ONLY: NARP_cal_SunPosition, NARP_cal_SunPosition_DTS
   USE module_phys_atmmoiststab, ONLY: cal_AtmMoist, cal_Stab, stab_psi_heat, stab_psi_mom
   USE module_phys_narp, ONLY: NARP_cal_SunPosition
   USE module_phys_spartacus, ONLY: SPARTACUS
   ! USE AnOHM_module, ONLY: AnOHM
   USE module_phys_resist, ONLY: AerodynamicResistance, BoundaryLayerResistance, SurfaceResistance, &
                            SUEWS_cal_RoughnessParameters
   USE module_phys_ohm, ONLY: OHM
   USE module_phys_estm, ONLY: ESTM
   USE module_phys_ehc, ONLY: EHC
   USE module_phys_snow, ONLY: SnowCalc, MeltHeat, SnowUpdate, update_snow_albedo, update_snow_dens
   USE module_phys_dailystate, ONLY: update_DailyStateLine_DTS, SUEWS_cal_DailyState
   USE module_phys_waterdist, ONLY: &
      drainage, cal_water_storage_surf, &
      cal_water_storage_building, &
      SUEWS_cal_SoilState, &
      SUEWS_update_SoilMoist, SUEWS_update_SoilMoist_DTS, &
      ReDistributeWater, SUEWS_cal_HorizontalSoilWater, &
      SUEWS_cal_HorizontalSoilWater_DTS, &
      SUEWS_cal_WaterUse
   USE module_ctrl_output, ONLY: varListAll
   USE module_phys_lumps, ONLY: LUMPS_cal_QHQE_DTS
   USE module_phys_evap, ONLY: cal_evap_multi
   USE module_phys_rslprof, ONLY: RSLProfile, RSLProfile_DTS
   USE module_phys_anthro, ONLY: AnthropogenicEmissions
   USE module_phys_biogenco2, ONLY: CO2_biogen
   USE module_ctrl_const_allocate, ONLY: &
      nsurf, nvegsurf, ndepth, nspec, nbtypes, &
      PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf, &
      ivConif, ivDecid, ivGrass, &
      ncolumnsDataOutSUEWS, ncolumnsDataOutSnow, &
      ncolumnsDataOutESTM, ncolumnsDataOutDailyState, &
      ncolumnsDataOutRSL, ncolumnsdataOutSOLWEIG, ncolumnsDataOutBEERS, &
      ncolumnsDataOutDebug, ncolumnsDataOutSPARTACUS, ncolumnsDataOutEHC, &
      ncolumnsDataOutSTEBBS, ncolumnsDataOutNHood
   USE module_ctrl_const_moist, ONLY: avcp, avdens, lv_J_kg
   USE module_phys_solweig, ONLY: SOLWEIG_cal_main
   USE module_phys_beers, ONLY: BEERS_cal_main, BEERS_cal_main_DTS
   USE module_phys_stebbs, ONLY: stebbsonlinecouple
   USE module_ctrl_version, ONLY: git_commit, compiler_ver ! these are automatically generated during compilation time
   USE module_util_time, ONLY: SUEWS_cal_dectime_DTS, SUEWS_cal_tstep_DTS, SUEWS_cal_weekday_DTS, &
                          SUEWS_cal_DLS_DTS

   IMPLICIT NONE

   ! Module-level variable to track if snow warning has been shown
   LOGICAL, SAVE :: snow_warning_shown = .FALSE.

CONTAINS

! ===================MAIN CALCULATION WRAPPER FOR ENERGY AND WATER FLUX===========

   SUBROUTINE SUEWS_cal_Main( &
      timer, forcing, config, siteInfo, &
      modState, &
      outputLine, &
      debugState) ! output

      IMPLICIT NONE

      ! input variables

      ! ####################################################################################
      !  declaration for DTS variables
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(INOUT) :: forcing
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config

      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(SUEWS_DEBUG), INTENT(OUT), OPTIONAL :: debugState

      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState
      ! ####################################################################################

      ! ########################################################################################
      ! output variables
      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutEHC - 5) :: dataOutLineEHC
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5) :: dataoutLineRSL
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug - 5) :: dataOutLineDebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5) :: dataOutLineSPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5) :: dataOutLineDailyState
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSTEBBS - 5) :: dataOutLineSTEBBS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutNHood - 5) :: dataOutLineNHood
      ! save all output variables in a single derived type
      TYPE(output_line), INTENT(OUT) :: outputLine
      ! ########################################################################################

      TYPE(SUEWS_STATE) :: modState_tstepstart

      ! these local variables are used in iteration
      INTEGER, PARAMETER :: max_iter = 60 ! maximum number of iteration

      ! ####################################################################################
      ASSOCIATE ( &
         Diagnose => config%Diagnose, &
         ! modState
         flagState => modState%flagState, &
         hydroState => modState%hydroState, &
         phenState => modState%phenState, &
         heatstate => modState%heatState, &
         snowState => modState%snowState, &
         nhoodState => modState%nhoodState &
         )
         ASSOCIATE ( &
            ! timer
            ! siteInfo
            nlayer => siteInfo%nlayer, &
            sfr_surf => siteInfo%sfr_surf, &
            ! flagState
            flag_converge => flagState%flag_converge, &
            i_iter => flagState%i_iter, &
            ev_surf => hydroState%ev_surf, &
            ev0_surf => hydroState%ev0_surf, &
            ! heatState
            tsfc_surf_stepstart => heatState%tsfc_surf_stepstart, &
            tsfc_roof_stepstart => heatState%tsfc_roof_stepstart, &
            tsfc_wall_stepstart => heatState%tsfc_wall_stepstart, &
            tsfc_surf => heatState%tsfc_surf, &
            tsfc_roof => heatState%tsfc_roof, &
            tsfc_wall => heatState%tsfc_wall, &
            qe0_surf => heatState%qe0_surf, &
            qh => heatState%qh, &
            QH_LUMPS => heatState%QH_LUMPS, &
            QH_Init => heatState%QH_Init, &
            ! snowState
            chSnow_per_interval => snowState%chSnow_per_interval, &
            SnowRemoval => snowState%SnowRemoval, &
            swe => snowState%swe, &
            Qm => snowState%Qm, &
            QmFreez => snowState%QmFreez, &
            QmRain => snowState%QmRain, &
            NWstate_per_tstep => hydroState%NWstate_per_tstep, &
            mwh => snowState%mwh, &
            mwstore => snowState%mwstore &
            )

            ! IF (Diagnose == 1) WRITE (*, *) 'dectime', dectime

            ! ############# memory allocation for DTS variables (start) #############
            CALL modState_tstepstart%ALLOCATE(nlayer, ndepth)
            IF (PRESENT(debugState)) THEN
               CALL debugState%init(nlayer, ndepth)
            END IF
            ! save initial values of model states
            modState_tstepstart = modState

            ! ########################################3
            ! set initial values for output arrays
            SWE = 0.
            mwh = 0.
            MwStore = 0.
            chSnow_per_interval = 0.

            ! Check if this is updated across timesteps
            SnowRemoval = 0.
            Qm = 0.
            QmFreez = 0.
            QmRain = 0.

            ! these output variables are used for debugging
            qe0_surf = 0 ! QE from PM: only meaningful when snowuse=0
            ev0_surf = 0 ! ev from PM: only meaningful when snowuse=0
            ev_surf = 0 ! ev from water balance: only meaningful when snowuse=0

            ! force several snow related state variables to zero if snow module is off
            IF (config%snowuse == 0) THEN
               snowState%SnowDens = 0.
               snowState%SnowFrac = 0.
               snowState%SnowWater = 0.
               snowState%SnowAlb = 0.
               snowState%IceFrac = 0.
               snowState%SnowPack = 0.
            END IF

            ! ########################################################################################
            ! save initial values of inout variables
            ! snowState = snowState
            snowState%snowfrac = MERGE(forcing%snowfrac, snowState%SnowFrac, config%NetRadiationMethod == 0)

            ! initialise output variables
            dataOutLineSnow = -999.
            dataOutLineESTM = -999.
            dataOutLineEHC = -999.
            dataoutLineRSL = -999.
            dataOutLineBEERS = -999.
            dataOutLineDebug = -999.
            dataOutLineSPARTACUS = -999.
            dataOutLineDailyState = -999.
            dataOutLineSTEBBS = -999.

            !########################################################################################
            !           main calculation starts here
            !########################################################################################

            ! iteration is used below to get results converge
            flag_converge = .FALSE.

            ! save surface temperature at the beginning of the time step
            tsfc_surf_stepstart = tsfc_surf
            ! ! TODO: ESTM work: to allow heterogeneous surface temperatures
            IF (config%StorageHeatMethod == 5 .OR. config%NetRadiationMethod > 1000) THEN
               tsfc_roof_stepstart = tsfc_roof
               tsfc_wall_stepstart = tsfc_wall
            END IF

            !==============surface roughness calculation=======================
            IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_RoughnessParameters...'
            CALL SUEWS_cal_RoughnessParameters( &
               timer, config, forcing, siteInfo, & !input
               modState) ! input/output:

            !=================Calculate sun position=================
            IF (Diagnose == 1) WRITE (*, *) 'Calling NARP_cal_SunPosition...'
            ! print *, 'timer: azimuth, zenith_deg', timer%azimuth, timer%zenith_deg
            CALL NARP_cal_SunPosition_DTS( &
               timer, config, forcing, siteInfo, & !input
               modState) ! input/output:

            !=================Calculation of density and other water related parameters=================
            IF (Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_AtmMoist...'
            CALL SUEWS_update_atmState(timer, forcing, modState)

            ! start iteration-based calculation
            ! through iterations, the surface temperature is examined to be converged
            i_iter = 1
            ! max_iter = 30
            DO WHILE (i_iter < 3 .OR. ((.NOT. flag_converge) .AND. (i_iter < max_iter)))
               IF (diagnose == 1) THEN
                  PRINT *, '=========================== '
                  PRINT *, 'iteration is ', i_iter
               END IF
               ! ========================================================================================
               ! #374: solve the issue of iteration safety
               ! IMPORTANT: restore some initial states as they SHOULD NOT be changed during iterations
               IF (((.NOT. flag_converge) .AND. i_iter > 1) .OR. (flag_converge .AND. i_iter == 2)) THEN
                  CALL modState%check_and_reset_states(modState_tstepstart)
               END IF
               ! ========================================================================================
               !==============main calculation start=======================

               ! WIP notes: TS 23 Aug 2023
               ! the following part is the main calculation part of SUEWS
               ! the calculation is divided into several modules relating to major physical processes
               ! the modules are called in the following order:
               ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               ! part A: modules that are called multiple times per timestep to achieve convergence
               ! A1. SUEWS_cal_DailyState_DTS: update phenology and land cover states at a daily scale - which may occur either at the beginning or end of a day
               ! A2. SUEWS_cal_WaterUse_DTS: calculate water use
               ! A3. SUEWS_cal_AnthropogenicEmission_DTS: calculate anthropogenic heat and CO2 fluxes
               ! A4. SUEWS_cal_Qn_DTS: calculate net all-wave radiation
               ! A5. SUEWS_cal_Qs_DTS: calculate storage heat flux
               ! A6. SUEWS_cal_Water_DTS: calculate surface water balance
               ! A7. SUEWS_cal_Resistance_DTS: calculate various resistances
               ! A8.1 (optional) SUEWS_cal_snow_DTS: calculate snow-related energy balance if snowuse == 1
               ! A8.2 (optional) SUEWS_cal_QE_DTS: calculate non-snow-related energy balance if snowuse == 0
               ! A9. SUEWS_cal_HorizontalSoilWater_DTS: calculate redistribution of soil water between land covers within a grid cell
               ! A10. SUEWS_cal_SoilState_DTS: calculate stored soil water
               ! A11. SUEWS_cal_QH_DTS: calculate sensible heat flux
               ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               ! part B: modules that are called once per timestep due to computational inefficiency
               ! B1. RSLProfile_DTS: calculate RSL profiles of wind speed, temperature and humidity
               ! B2. SUEWS_cal_BiogenCO2_DTS: calculate CO2 fluxes from biogenic sources
               ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

               ! WIP: the wrapper subroutines for different physical processes have the same structure to allow easy implementation of new functionalities
               ! the structure is as follows:
               ! --------------------------------------------------------------------------------
               ! SUEWS_cal_<module_name>_DTS( & ! `DTS` stands for `derived type structure`
               !    timer, & ! time related variables <input>
               !    forcing & ! meteorological forcing variables <input>
               !    siteInfo, & ! site information <input>
               !    config, & ! configuation methods of model behaviours <input>
               !    modState_<module_name> & ! model states as input to the module <inout>
               ! &)
               ! --------------------------------------------------------------------------------
               !=================Call the SUEWS_cal_DailyState routine to get surface characteristics ready=================
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_DailyState...'
               CALL SUEWS_cal_DailyState( &
                  timer, config, forcing, siteInfo, & !input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_01_dailystate = modState
               END IF
               !======== Calculate soil moisture =========
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_update_SoilMoist...'
               CALL SUEWS_update_SoilMoist_DTS( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_02_soilmoist = modState
               END IF
               ! WIP notes: TS 03 Sep 2023
               ! upgrade the interface following this order:
               ! 1. add `timer, config, forcing, siteInfo` as input
               ! 2. add `xxState_prev` and `xxState_next` as input and output, respectively

               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_WaterUse...'
               !=================Gives the external and internal water uses per timestep=================
               CALL SUEWS_cal_WaterUse( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_03_wateruse = modState
               END IF

               ! ===================ANTHROPOGENIC HEAT AND CO2 FLUX======================
               CALL SUEWS_cal_AnthropogenicEmission( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_04_anthroemis = modState
               END IF

               ! ========================================================================
               ! N.B.: the following parts involves snow-related calculations.
               ! ===================NET ALLWAVE RADIATION================================
               IF (diagnose == 1) THEN
                  PRINT *, 'Tsfc_surf before QN', heatState%tsfc_surf
               END IF
               CALL SUEWS_cal_Qn( &
                  timer, config, forcing, siteInfo, & ! input
                  modState, & ! input/output:
                  dataOutLineSPARTACUS) ! output
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_05_qn = modState
               END IF

               IF (diagnose == 1) PRINT *, 'Tsfc_surf before QS', heatState%tsfc_surf
               CALL SUEWS_cal_Qs( &
                  timer, config, forcing, siteInfo, datetimeLine, & !input
                  modState, & ! input/output:
                  dataOutLineESTM, dataOutLineSTEBBS)
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_06_qs = modState
               END IF

               !==================Energy related to snow melting/freezing processes=======
               IF (Diagnose == 1) WRITE (*, *) 'Calling MeltHeat'

               !==========================Turbulent Fluxes================================
               IF (Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_QHQE...'
               CALL LUMPS_cal_QHQE_DTS( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_07_qhqe_lumps = modState
               END IF

               !============= calculate water balance =============
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Water...'
               CALL SUEWS_cal_Water( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_08_water = modState
               END IF
               !============= calculate water balance end =============

               !===============Resistance Calculations=======================
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Resistance...'
               CALL SUEWS_cal_Resistance( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_09_resist = modState
               END IF
               !===================Resistance Calculations End=======================

               !===================Calculate surface hydrology and related soil water=======================
               ! MP: Until Snow has been fixed this should not be used (TODO)
               IF (config%SnowUse == 1) THEN
                  ! Only show warning once per simulation run
                  IF (.NOT. snow_warning_shown) THEN
                     WRITE (*, *) "WARNING SNOW ON! Not recommended at the moment"
                     snow_warning_shown = .TRUE.
                  END IF
                  ! ===================Calculate snow related hydrology=======================
                  ! #234 the snow parts needs more work to be done
                  ! TS 18 Oct 2023: snow is temporarily turned off for easier implementation of other functionalities
                  CALL SUEWS_cal_snow( &
                     timer, config, forcing, siteInfo, & ! input
                     modState, & ! input/output
                     dataOutLineSnow)
                  ! N.B.: snow-related calculations end here.
                  !===================================================
               ELSE
                  IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_QE...'
                  !======== Evaporation and surface state_id for snow-free conditions ========
                  CALL SUEWS_cal_QE( &
                     timer, config, forcing, siteInfo, & ! input
                     modState) ! input/output:
                  IF (config%flag_test .AND. PRESENT(debugState)) THEN
                     debugState%state_10_qe = modState
                  END IF
                  !======== Evaporation and surface state_id end========
               END IF

               !============ Sensible heat flux ===============
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_QH...'
               CALL SUEWS_cal_QH( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_11_qh = modState
               END IF
               !============ Sensible heat flux end ===============

               ! ============ update surface temperature of this iteration ===============
               CALL suews_update_tsurf( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_12_tsurf = modState
               END IF


               ! IF (i_iter == 1) THEN
               !    modState_tstepstart = modState
               ! END IF
               i_iter = i_iter + 1
               IF (i_iter == max_iter .AND. .NOT. flag_converge) THEN
                  IF (diagnose == 1) PRINT *, 'Iteration did not converge in', i_iter, ' iterations'

               END IF
               IF (diagnose == 1) PRINT *, '========================='
               IF (diagnose == 1) PRINT *, ''

               !==============main calculation end=======================
            END DO ! end iteration for tsurf calculations

            ! MP: Add test for QH zL signs - recalculate zL if the same
            IF (modState%heatState%QH*modState%atmState%zL > 0) THEN
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Resistance...'
               CALL SUEWS_cal_Resistance( &
                  timer, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
            END IF

            !==============================================================
            ! Calculate diagnostics: these variables are decoupled from the main SUEWS calculation

            !============ roughness sub-layer diagonostics ===============
            IF (Diagnose == 1) WRITE (*, *) 'Calling RSLProfile...'
            CALL RSLProfile_DTS( &
               timer, config, forcing, siteInfo, & ! input
               modState, & ! input/output:
               dataoutLineRSL) ! output
            IF (config%flag_test .AND. PRESENT(debugState)) THEN
               debugState%state_13_rsl = modState
            END IF

!             ! SG 05/25: Subroutine commented out until checked
!             ! ============ BIOGENIC CO2 FLUX =======================
!             IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_BiogenCO2_DTS...'
!             CALL SUEWS_cal_BiogenCO2( &
!                timer, config, forcing, siteInfo, & ! input
!                modState) ! input/output:
!             IF (config%flag_test .AND. PRESENT(debugState)) THEN
!                debugState%state_14_biogenco2 = modState
!             END IF

            ! calculations of diagnostics end
            !==============================================================
            IF (Diagnose == 1) WRITE (*, *) 'update inout variables with new values...'
            !==============================================================
            !==============================================================
            ! update inout variables with new values (to be compatible with original interface)
            ! atmState%Tair_av = Tair_av_next

            !==============use BEERS to get localised radiation flux==================
            ! TS 14 Jan 2021: BEERS is a modified version of SOLWEIG
            IF (Diagnose == 1) WRITE (*, *) 'Calling BEERS_cal_main_DTS...'
            CALL BEERS_cal_main_DTS( &
               timer, config, forcing, siteInfo, & ! input
               modState, & ! input/output:
               dataOutLineBEERS) ! output
            IF (config%flag_test .AND. PRESENT(debugState)) THEN
               debugState%state_15_beers = modState
            END IF
            !==============Use dyOHM storage heat flux to convert surface temperature
            CALL suews_update_tsurf_dyohm( &
               timer, config, forcing, siteInfo, & ! input
               modState) ! input/output:


            !==============translation of  output variables into output array===========
            IF (Diagnose == 1) WRITE (*, *) 'Calling BEERS_cal_main_DTS...'
            CALL SUEWS_update_outputLine( &
               timer, config, forcing, siteInfo, & ! input
               modState, & ! input/output:
               datetimeLine, dataOutLineSUEWS) !output

            IF (config%StorageHeatMethod == 5) THEN
               IF (Diagnose == 1) WRITE (*, *) 'Calling ECH_update_outputLine_DTS...'
               CALL EHC_update_outputLine( &
                  timer, & !input
                  modState, & ! input/output:
                  datetimeLine, dataOutLineEHC) !output
            END IF

            ! daily state_id:
            IF (Diagnose == 1) WRITE (*, *) 'Calling update_DailyStateLine_DTS...'
            CALL update_DailyStateLine_DTS( &
               timer, config, forcing, siteInfo, & ! input
               modState, & ! input/output:
               dataOutLineDailyState) !out

            !==============translation end ================
            IF (Diagnose == 1) WRITE (*, *) 'Calling dataoutlineDebug...'
            ! TODO: #233 debugging info will be collected into a derived type for model output when the debugging flag is on
            ! here we may still use the original output array but another derived type can be used for enriched debugging info
            CALL update_debug_info( &
               timer, config, forcing, siteInfo, & ! input
               modState_tstepstart, & ! input
               modState, & ! inout
               dataoutlineDebug) ! output

            !==============output==========================
            ! TODO: collect output into a derived type for model output
            IF (Diagnose == 1) WRITE (*, *) 'Calling output_line_init...'
            ! CALL output_line_init(output_line_suews)
            CALL outputLine%init()
            outputLine%datetimeLine = datetimeLine
            outputLine%dataOutLineSUEWS = [datetimeLine, dataOutLineSUEWS]
            outputLine%dataOutLineEHC = [datetimeLine, dataOutLineEHC]
            outputLine%dataOutLineDailyState = [datetimeLine, dataOutLineDailyState]
            outputLine%dataOutLineBEERS = [datetimeLine, dataOutLineBEERS]
            outputLine%dataOutLineDebug = [datetimeLine, dataOutLineDebug]
            outputLine%dataOutLineSPARTACUS = [datetimeLine, dataOutLineSPARTACUS]
            outputLine%dataOutLineSnow = [datetimeLine, dataOutLineSnow]
            outputLine%dataoutLineRSL = [datetimeLine, dataOutLineRSL]
            outputLine%dataOutLineESTM = [datetimeLine, dataOutLineESTM]
            outputLine%dataOutLineSTEBBS = [datetimeLine, dataOutLineSTEBBS]

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_Main
! ================================================================================

   SUBROUTINE update_debug_info( &
      timer, config, forcing, siteInfo, & ! input
      modState_init, & ! input
      modState, & ! inout
      dataoutlineDebug) ! output

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_TIMER, SUEWS_SITE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, HEAT_STATE, flag_STATE
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState_init

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      REAL(KIND(1D0)), INTENT(OUT), DIMENSION(ncolumnsDataOutDebug - 5) :: dataoutlineDebug

      ASSOCIATE ( &
         hydroState => modState%hydroState, &
         heatState => modState%heatState, &
         ohmState => modState%ohmState, &
         snowState => modState%snowState, &
         anthroemisState => modState%anthroemisState, &
         phenState => modState%phenState, &
         atmState => modState%atmState, &
         flagState => modState%flagState, &
         roughnessState => modState%roughnessState, &
         conductancePrm => siteInfo%conductance &
         )

         ASSOCIATE ( &
            flag_test => config%flag_test, &
            tsfc0_out_surf => heatState%tsfc_surf_stepstart, &
            qn_surf => heatState%qn_surf, &
            qs_surf => heatState%qs_surf, &
            qe0_surf => heatState%qe0_surf, &
            qe_surf => heatState%qe_surf, &
            qh_surf => heatState%qh_surf, &
            wu_surf => hydroState%wu_surf, &
            ev0_surf => hydroState%ev0_surf, &
            ev_surf => hydroState%ev_surf, &
            drain_surf => hydroState%drain_surf, &
            RS => atmState%RS, &
            RA_h => atmState%RA_h, &
            RB => atmState%RB, &
            RAsnow => snowState%RAsnow, &
            rss_surf => atmState%rss_surf, &
            vsmd => hydroState%vsmd, &
            g_kdown => phenState%g_kdown, &
            g_dq => phenState%g_dq, &
            g_ta => phenState%g_ta, &
            g_smd => phenState%g_smd, &
            g_lai => phenState%g_lai, &
            vpd_hPa => atmState%vpd_hPa, &
            lv_J_kg => atmState%lv_J_kg, &
            avdens => atmState%avdens, &
            avcp => atmState%avcp, &
            s_hPa => atmState%s_hPa, &
            psyc_hPa => atmState%psyc_hPa, &
            i_iter => flagState%i_iter, &
            FAIBldg_use => roughnessState%FAIBldg_use, &
            FAIEveTree_use => roughnessState%FAIEveTree_use, &
            FAIDecTree_use => roughnessState%FAIDecTree_use, &
            FAI => roughnessState%FAI &
            )

            dataoutlineDebug = &
               [MERGE(1D0, 0D0, flag_test), &
                tsfc0_out_surf, &
                qn_surf, qs_surf, qe0_surf, qe_surf, qh_surf, & ! energy balance
                wu_surf, ev0_surf, ev_surf, drain_surf, &
                modState_init%hydroState%state_surf, hydroState%state_surf, &
                modState_init%hydroState%soilstore_surf, hydroState%soilstore_surf, & ! water balance
                RS, RA_h, RB, RAsnow, rss_surf, & ! for debugging QE
                vsmd, conductancePrm%S1/conductancePrm%G_sm + conductancePrm%S2, &
                conductancePrm%G_sm, &
                conductancePrm%G_sm*(vsmd - conductancePrm%S1/conductancePrm%G_sm + conductancePrm%S2), & ! debug g_smd
                g_kdown, g_dq, g_ta, g_smd, g_lai, & ! for debugging RS: surface resistance
                vpd_hPa, lv_J_kg, avdens, avcp, s_hPa, psyc_hPa, & ! for debugging QE
                i_iter*1D0, &
                FAIBldg_use, FAIEveTree_use, FAIDecTree_use, FAI, &
                ohmState%dqndt]

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE update_debug_info

   ! ================================================================================

   SUBROUTINE suews_update_tsurf( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_TIMER, SUEWS_SITE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, HEAT_STATE, flag_STATE, &
                               SUEWS_STATE
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      INTEGER :: i_surf, i_layer
      REAL(KIND(1D0)) :: dif_tsfc_iter, ratio_iter

      ASSOCIATE ( &
         flagState => modState%flagState, &
         heatState => modState%heatState, &
         atmState => modState%atmState &
         )

         ASSOCIATE ( &
            flag_converge => flagState%flag_converge, &
            diagnose => config%diagnose, &
            StorageHeatMethod => config%StorageHeatMethod, &
            nlayer => siteInfo%nlayer, &
            avdens => atmState%avdens, &
            avcp => atmState%avcp, &
            RA_h => atmState%RA_h, &
            TSfc_C => heatState%TSfc_C, &
            QH_surf => heatState%QH_surf, &
            QH_roof => heatState%QH_roof, &
            QH_wall => heatState%QH_wall, &
            qh => heatState%qh, &
            tsfc0_out_surf => heatState%tsfc_surf_stepstart, &
            tsfc0_out_roof => heatState%tsfc_roof_stepstart, &
            tsfc0_out_wall => heatState%tsfc_wall_stepstart, &
            tsfc_surf => heatState%tsfc_surf, &
            tsfc_roof => heatState%tsfc_roof, &
            tsfc_wall => heatState%tsfc_wall, &
            temp_c => forcing%temp_c &
            )

            !============ calculate surface temperature ===============
            TSfc_C = cal_tsfc(qh, avdens, avcp, RA_h, temp_c)

            !============= calculate surface specific QH and Tsfc ===============

            DO i_surf = 1, nsurf
               tsfc_surf(i_surf) = cal_tsfc(QH_surf(i_surf), avdens, avcp, RA_h, temp_c)
            END DO

            DO i_layer = 1, nlayer
               tsfc_roof(i_layer) = cal_tsfc(QH_roof(i_layer), avdens, avcp, RA_h, temp_c)
               tsfc_wall(i_layer) = cal_tsfc(QH_wall(i_layer), avdens, avcp, RA_h, temp_c)
            END DO

            dif_tsfc_iter = MAXVAL(ABS(tsfc_surf - tsfc0_out_surf))
            IF (StorageHeatMethod == 5) THEN
               dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc_roof - tsfc0_out_roof)), dif_tsfc_iter)
               dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc0_out_wall - tsfc_wall)), dif_tsfc_iter)
            END IF

            ! ====test===
            ! see if this converges better
            ! ratio_iter = 1
            ratio_iter = .3
            tsfc_surf = (tsfc0_out_surf*(1 - ratio_iter) + tsfc_surf*ratio_iter)
            IF (StorageHeatMethod == 5) THEN
               tsfc_roof = (tsfc0_out_roof*(1 - ratio_iter) + tsfc_roof*ratio_iter)
               tsfc_wall = (tsfc0_out_wall*(1 - ratio_iter) + tsfc_wall*ratio_iter)
            END IF
            ! =======test end=======

            !============ surface-level diagonostics end ===============

            ! force quit do-while, i.e., skip iteration and use NARP for Tsurf calculation
            ! if (NetRadiationMethod < 10 .or. NetRadiationMethod > 100) exit

            ! Test if sensible heat fluxes converge in iterations
            ! MP Testing 0.1 -> 0.05
            IF (dif_tsfc_iter > .05) THEN
               flag_converge = .FALSE.
            ELSE
               flag_converge = .TRUE.
               ! PRINT *, ' qh_residual: ', qh_residual, ' qh_resist: ', qh_resist
               ! PRINT *, ' dif_qh: ', ABS(qh_residual - qh_resist)
               ! PRINT *, ' abs. dif_tsfc: ', dif_tsfc_iter
            END IF

            ! note: tsfc has an upper limit of temp_c+50 to avoid numerical errors
            tsfc0_out_surf = MIN(tsfc_surf, Temp_C + 50)
            tsfc0_out_roof = MIN(tsfc_roof, Temp_C + 50)
            tsfc0_out_wall = MIN(tsfc_wall, Temp_C + 50)
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE suews_update_tsurf

   SUBROUTINE suews_update_tsurf_dyohm( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_TIMER, SUEWS_SITE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, HEAT_STATE, flag_STATE, &
                               SUEWS_STATE
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      INTEGER :: i_surf, i_layer, nz
      REAL(KIND(1D0)) :: dif_tsfc_iter, ratio_iter
      REAL(KIND(1D0)), DIMENSION(5) :: z
      REAL(KIND(1D0)), ALLOCATABLE :: prev_profile(:), next_profile(:)
      ASSOCIATE ( &
         timestep => timer%tstep, &
         flagState => modState%flagState, &
         heatState => modState%heatState, &
         atmState => modState%atmState, &
         stebbsState => modState%stebbsState, &
         ehcPrm => siteInfo%ehc &
         )

         ASSOCIATE ( &
            flag_converge => flagState%flag_converge, &
            diagnose => config%diagnose, &
            StorageHeatMethod => config%StorageHeatMethod, &
            nlayer => siteInfo%nlayer, &
            temp_surf_in => heatState%temp_surf_dyohm, &
            temp_surf_dyohm => heatState%temp_surf_dyohm, &
            temp_c => forcing%temp_c, &
            QS_surf => heatState%qs_surf, &
            k_surf => ehcPrm%k_surf, &
            cp_surf => ehcPrm%cp_surf, &
            T_bottom => stebbsState%OutdoorAirAnnualTemperature &
            )

            !============= calculate surface temperature based on QS ===============
            nz = 5
            T_bottom = 11.1D0 !annual mean air temperature in London 2012
            z = (/ 0.0D0, 0.03D0, 0.1D0, 1.5D0, 3.0D0 /)
            ! Loop over surfaces
            DO i_surf = 1, nsurf
               ALLOCATE(prev_profile(nz), next_profile(nz))
               prev_profile = heatState%temp_surf_dyohm(i_surf, :)

               next_profile = cal_tsfc_dyohm( &
                  Temp_in = prev_profile, &
                  Qs      = QS_surf(i_surf), &
                  K       = k_surf(i_surf, 1), &
                  C       = cp_surf(i_surf, 1), &
                  z       = z, nz = nz, &
                  T_bottom = T_bottom, dt = timestep )

               heatState%temp_surf_dyohm(i_surf, :) = next_profile
               !save the surface temperature
               heatState%tsfc_surf_dyohm(i_surf) = next_profile(1)
               DEALLOCATE(prev_profile, next_profile)
            END DO
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE suews_update_tsurf_dyohm

! ===================ANTHROPOGENIC HEAT + CO2 FLUX================================
   SUBROUTINE SUEWS_cal_AnthropogenicEmission( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_SITE, SUEWS_TIMER, SUEWS_CONFIG, SUEWS_FORCING, &
                               anthroEmis_STATE, atm_state, SUEWS_STATE

      IMPLICIT NONE
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INout) :: modState
      ! TYPE(anthroEmis_STATE), INTENT(INout) :: anthroEmisState
      ! TYPE(heat_STATE), INTENT(INout) :: heatState
      ! TYPE(atm_state), INTENT(INout) :: atmState

      ! INTEGER, INTENT(in)::Diagnose
      INTEGER :: EmissionsMethod !0 - Use values in met forcing file, or default QF;1 - Method according to Loridan et al. (2011) : SAHP; 2 - Method according to Jarvi et al. (2011)   : SAHP_2

      REAL(KIND(1D0)), DIMENSION(2) :: AH_MIN ! miniumum anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), DIMENSION(2) :: AH_SLOPE_Heating ! heating slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2) :: AH_SLOPE_Cooling ! cooling slope for the anthropogenic heat flux calculation [W m-2 K-1]
      ! REAL(KIND(1d0)), DIMENSION(2), INTENT(in)::NumCapita

      REAL(KIND(1D0)), DIMENSION(2) :: PopDensDaytime ! Daytime population density [people ha-1] (i.e. workers)

      REAL(KIND(1D0)), DIMENSION(2) :: QF0_BEU ! Fraction of base value coming from buildings [-]
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_A ! Base value for QF [W m-2]
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_B ! Parameter related to heating degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_C ! Parameter related to cooling degree days [W m-2 K-1 (Cap ha-1 )-1]

      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Heating ! base temperatrue for heating degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Cooling ! base temperature for cooling degree day [degC]

      REAL(KIND(1D0)), DIMENSION(2) :: TrafficRate ! Traffic rate [veh km m-2 s-1]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: AHProf_24hr ! diurnal profile of anthropogenic heat flux (AVERAGE of the multipliers is equal to 1) [-]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: HumActivity_24hr ! diurnal profile of human activity [-]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: TraffProf_24hr ! diurnal profile of traffic activity calculation[-]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: PopProf_24hr ! diurnal profile of population [-]

      REAL(KIND(1D0)) :: TrafficUnits ! traffic units choice [-]

      INTEGER, PARAMETER :: notUsedI = -999
      REAL(KIND(1D0)), PARAMETER :: notUsed = -999
      REAL(KIND(1D0)) :: Tair !ambient air temperature [degC]

      ASSOCIATE ( &
         anthroEmisState => modState%anthroEmisState, &
         heatState => modState%heatState, &
         atmstate => modState%atmstate &
         )

         ASSOCIATE ( &
            dayofWeek_id => timer%dayofWeek_id, &
            DLS => timer%DLS, &
            ahemisPrm => siteInfo%anthroemis &
         &)

            ASSOCIATE ( &
               EmissionsMethod => config%EmissionsMethod, &
               localClimateMethod => config%localClimateMethod, &
               EF_umolCO2perJ => ahemisPrm%EF_umolCO2perJ, &
               EnEF_v_Jkm => ahemisPrm%EnEF_v_Jkm, &
               FcEF_v_kgkm => ahemisPrm%FcEF_v_kgkm, &
               FrFossilFuel_Heat => ahemisPrm%FrFossilFuel_Heat, &
               FrFossilFuel_NonHeat => ahemisPrm%FrFossilFuel_NonHeat, &
               MaxFCMetab => ahemisPrm%MaxFCMetab, &
               MaxQFMetab => ahemisPrm%MaxQFMetab, &
               MinFCMetab => ahemisPrm%MinFCMetab, &
               MinQFMetab => ahemisPrm%MinQFMetab, &
               PopDensNighttime => ahemisPrm%anthroheat%popdensnighttime, &
               CO2PointSource => siteInfo%CO2PointSource, &
               SurfaceArea => siteInfo%SurfaceArea, &
               HDD_id => anthroEmisState%HDD_id, &
               imin => timer%imin, &
               it => timer%it, &
               Fc_anthro => anthroEmisState%Fc_anthro, &
               Fc_build => anthroEmisState%Fc_build, &
               Fc_metab => anthroEmisState%Fc_metab, &
               Fc_point => anthroEmisState%Fc_point, &
               Fc_traff => anthroEmisState%Fc_traff, &
               QF => heatState%QF, &
               QF_SAHP => heatState%QF_SAHP, &
               T2_c => atmstate%t2_C, &
               T_hbh_C => atmState%T_hbh_C, &
               Temp_C => forcing%Temp_C, &
               QF_obs => forcing%QF_obs &
               )

               AH_MIN(1) = ahemisPrm%anthroheat%ah_min_working
               AH_MIN(2) = ahemisPrm%anthroheat%ah_min_holiday
               AH_SLOPE_Heating(1) = ahemisPrm%anthroheat%ah_slope_heating_working
               AH_SLOPE_Heating(2) = ahemisPrm%anthroheat%ah_slope_heating_holiday
               AH_SLOPE_Cooling(1) = ahemisPrm%anthroheat%ah_slope_cooling_working
               AH_SLOPE_Cooling(2) = ahemisPrm%anthroheat%ah_slope_cooling_holiday

               TrafficRate(1) = ahemisPrm%TrafficRate_working
               TrafficRate(2) = ahemisPrm%TrafficRate_holiday

               PopDensDaytime(1) = ahemisPrm%anthroheat%popdensdaytime_working
               PopDensDaytime(2) = ahemisPrm%anthroheat%popdensdaytime_holiday
               QF0_BEU(1) = ahemisPrm%anthroheat%qf0_beu_working
               QF0_BEU(2) = ahemisPrm%anthroheat%qf0_beu_holiday
               Qf_A(1) = ahemisPrm%anthroheat%qf_a_working
               Qf_A(2) = ahemisPrm%anthroheat%qf_a_holiday
               Qf_B(1) = ahemisPrm%anthroheat%qf_b_working
               Qf_B(2) = ahemisPrm%anthroheat%qf_b_holiday
               Qf_C(1) = ahemisPrm%anthroheat%qf_c_working
               Qf_C(2) = ahemisPrm%anthroheat%qf_c_holiday
               BaseT_Heating(1) = ahemisPrm%anthroheat%baset_heating_working
               BaseT_Heating(2) = ahemisPrm%anthroheat%baset_heating_holiday
               BaseT_Cooling(1) = ahemisPrm%anthroheat%baset_cooling_working
               BaseT_Cooling(2) = ahemisPrm%anthroheat%baset_cooling_holiday

               AHProf_24hr(:, 1) = ahemisPrm%anthroheat%ahprof_24hr_working
               AHProf_24hr(:, 2) = ahemisPrm%anthroheat%ahprof_24hr_holiday
               HumActivity_24hr(:, 1) = ahemisPrm%HumActivity_24hr_working
               HumActivity_24hr(:, 2) = ahemisPrm%HumActivity_24hr_holiday
               PopProf_24hr(:, 1) = ahemisPrm%anthroheat%popprof_24hr_working
               PopProf_24hr(:, 2) = ahemisPrm%anthroheat%popprof_24hr_holiday
               TraffProf_24hr(:, 1) = ahemisPrm%TraffProf_24hr_working
               TraffProf_24hr(:, 2) = ahemisPrm%TraffProf_24hr_holiday
               TrafficUnits = ahemisPrm%TrafficUnits

               IF (EmissionsMethod == 0) THEN ! use observed qf
                  qf = QF_obs
               ELSEIF ((EmissionsMethod > 0 .AND. EmissionsMethod <= 6) .OR. EmissionsMethod >= 11) THEN
                  ! choose temperature for anthropogenic heat flux calculation
                  Tair = MERGE(T_hbh_C, MERGE(T2_C, Temp_C, localClimateMethod == 1), localClimateMethod == 2)

                  CALL AnthropogenicEmissions( &
                     CO2PointSource, EmissionsMethod, &
                     it, imin, DLS, DayofWeek_id, &
                     EF_umolCO2perJ, FcEF_v_kgkm, EnEF_v_Jkm, TrafficUnits, &
                     FrFossilFuel_Heat, FrFossilFuel_NonHeat, &
                     MinFCMetab, MaxFCMetab, MinQFMetab, MaxQFMetab, &
                     PopDensDaytime, PopDensNighttime, &
                     Tair, HDD_id, Qf_A, Qf_B, Qf_C, &
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

            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_AnthropogenicEmission
! ================================================================================

! ! SG 05/25: Need finishing/fixing - dummy variables causing compiler warnings
! !==============BIOGENIC CO2 flux==================================================
!    SUBROUTINE SUEWS_cal_BiogenCO2( &
!       timer, config, forcing, siteInfo, & ! input
!       modState) ! input/output:
!
!       USE module_ctrl_type, ONLY: LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
!                                SUEWS_CONFIG, CONDUCTANCE_PRM, SUEWS_FORCING, &
!                                SUEWS_TIMER, PHENOLOGY_STATE, SNOW_STATE, atm_state, &
!                                SUEWS_STATE
!
!       IMPLICIT NONE
!
!       TYPE(SUEWS_CONFIG), INTENT(IN) :: config
!       TYPE(SUEWS_TIMER), INTENT(IN) :: timer
!       TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
!       TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
!
!       TYPE(SUEWS_STATE), INTENT(INout) :: modState
!
!       REAL(KIND(1D0)) :: gfunc2 !gdq*gtemp*gs*gq for photosynthesis calculations (With modelled 2 meter temperature)
!       REAL(KIND(1D0)) :: dq !Specific humidity deficit [g/kg]
!       REAL(KIND(1D0)) :: t2 !air temperature at 2m [degC]
!       REAL(KIND(1D0)) :: dummy1 !Latent heat of vaporization in [J kg-1]
!       REAL(KIND(1D0)) :: dummy2 !Latent heat of sublimation in J/kg
!       REAL(KIND(1D0)) :: dummy3 !Saturation vapour pressure over water[hPa]
!       REAL(KIND(1D0)) :: dummy4 !Vapour pressure of water[hpa]
!       REAL(KIND(1D0)) :: dummy5 !vapour pressure deficit[hpa]
!       REAL(KIND(1D0)) :: dummy6 !vapour pressure deficit[pa]
!       REAL(KIND(1D0)) :: dummy7 !Vap density or absolute humidity [kg m-3]
!       REAL(KIND(1D0)) :: dummy8 !specific heat capacity [J kg-1 K-1]
!       REAL(KIND(1D0)) :: dummy9 !Air density [kg m-3]
!       REAL(KIND(1D0)) :: dummy10 !Surface Layer Conductance [mm s-1]
!       REAL(KIND(1D0)) :: dummy11 !Surface resistance [s m-1]
!
!       ASSOCIATE ( &
!          atmState => modState%atmState, &
!          phenState => modState%phenState, &
!          snowState => modState%snowState, &
!          hydroState => modState%hydroState, &
!          anthroEmisState => modState%anthroEmisState &
!          )
!
!          ASSOCIATE ( &
!             pavedPrm => siteInfo%lc_paved, &
!             bldgPrm => siteInfo%lc_bldg, &
!             evetrPrm => siteInfo%lc_evetr, &
!             dectrPrm => siteInfo%lc_dectr, &
!             grassPrm => siteInfo%lc_grass, &
!             bsoilPrm => siteInfo%lc_bsoil, &
!             waterPrm => siteInfo%lc_water, &
!             ehcPrm => siteInfo%ehc, &
!             nlayer => siteInfo%nlayer, &
!             sfr_surf => siteInfo%sfr_surf, &
!             sfr_roof => siteInfo%sfr_roof, &
!             sfr_wall => siteInfo%sfr_wall, &
!             SurfaceArea => siteInfo%SurfaceArea, &
!             snowPrm => siteInfo%snow, &
!             PipeCapacity => siteInfo%PipeCapacity, &
!             RunoffToWater => siteInfo%RunoffToWater, &
!             FlowChange => siteInfo%FlowChange, &
!             PervFraction => siteInfo%PervFraction, &
!             vegfraction => siteInfo%vegfraction, &
!             NonWaterFraction => siteInfo%NonWaterFraction, &
!             zMeas => siteInfo%z, &
!             conductancePrm => siteInfo%conductance, &
!             tstep_real => timer%tstep_real, &
!             avkdn => forcing%kdown, &
!             xsmd => forcing%xsmd, &
!             Temp_C => forcing%Temp_C, &
!             avU1 => forcing%U, &
!             avRH => forcing%RH, &
!             Press_hPa => forcing%pres, &
!             RA_h => atmState%RA_h, &
!             avdens => atmState%avdens, &
!             avcp => atmState%avcp, &
!             lv_J_kg => atmState%lv_J_kg, &
!             L_MOD => atmState%L_MOD, &
!             t2_C => atmState%t2_C, &
!             LAI_id => phenState%LAI_id, &
!             gfunc => phenState%gfunc, &
!             vsmd => hydroState%vsmd, &
!             id => timer%id, &
!             it => timer%it, &
!             dectime => timer%dectime, &
!             Fc_anthro => anthroEmisState%Fc_anthro, &
!             Fc => anthroEmisState%Fc, &
!             Fc_biogen => anthroEmisState%Fc_biogen, &
!             Fc_photo => anthroEmisState%Fc_photo, &
!             Fc_respi => anthroEmisState%Fc_respi, &
!             SnowFrac => snowState%SnowFrac, &
!             SMDMethod => config%SMDMethod, &
!             storageheatmethod => config%StorageHeatMethod, &
!             DiagMethod => config%DiagMethod, &
!             StabilityMethod => config%StabilityMethod, &
!             EmissionsMethod => config%EmissionsMethod, &
!             Diagnose => config%Diagnose &
!             )
!
!             ASSOCIATE ( &
!                alpha_bioCO2 => [evetrPrm%bioco2%alpha_bioco2, &
!                                 dectrPrm%bioco2%alpha_bioco2, &
!                                 grassPrm%bioco2%alpha_bioco2], &
!                alpha_enh_bioCO2 => [evetrPrm%bioco2%alpha_enh_bioco2, &
!                                     dectrPrm%bioco2%alpha_enh_bioco2, &
!                                     grassPrm%bioco2%alpha_enh_bioco2], &
!                beta_bioCO2 => [evetrPrm%bioco2%beta_bioCO2, &
!                                dectrPrm%bioco2%beta_bioCO2, &
!                                grassPrm%bioco2%beta_bioCO2], &
!                beta_enh_bioCO2 => [evetrPrm%bioco2%beta_enh_bioco2, &
!                                    dectrPrm%bioco2%beta_enh_bioco2, &
!                                    grassPrm%bioco2%beta_enh_bioco2], &
!                LAIMin => [evetrPrm%lai%laimin, dectrPrm%lai%laimin, grassPrm%lai%laimin], &
!                LAIMax => [evetrPrm%lai%laimax, dectrPrm%lai%laimax, grassPrm%lai%laimax], &
!                min_res_bioCO2 => [evetrPrm%bioco2%min_res_bioCO2, &
!                                   dectrPrm%bioco2%min_res_bioCO2, &
!                                   grassPrm%bioco2%min_res_bioCO2], &
!                resp_a => [evetrPrm%bioco2%resp_a, dectrPrm%bioco2%resp_a, grassPrm%bioco2%resp_a], &
!                resp_b => [evetrPrm%bioco2%resp_b, dectrPrm%bioco2%resp_b, grassPrm%bioco2%resp_b], &
!                theta_bioCO2 => [evetrPrm%bioco2%theta_bioCO2, &
!                                 dectrPrm%bioco2%theta_bioCO2, &
!                                 grassPrm%bioco2%theta_bioco2], &
!                MaxConductance => [evetrPrm%MaxConductance, dectrPrm%MaxConductance, grassPrm%MaxConductance], &
!                G_max => conductancePrm%g_max, &
!                G_k => conductancePrm%g_k, &
!                G_q_base => conductancePrm%g_q_base, &
!                G_q_shape => conductancePrm%g_q_shape, &
!                G_t => conductancePrm%g_t, &
!                G_sm => conductancePrm%g_sm, &
!                gsmodel => conductancePrm%gsmodel, &
!                Kmax => conductancePrm%Kmax, &
!                S1 => conductancePrm%S1, &
!                S2 => conductancePrm%S2, &
!                TH => conductancePrm%TH, &
!                TL => conductancePrm%TL &
!                )
!
!                IF (EmissionsMethod >= 11) THEN
!
!                   IF (gsmodel == 3 .OR. gsmodel == 4) THEN ! With modelled 2 meter temperature
!                      ! Call LUMPS_cal_AtmMoist for dq and SurfaceResistance for gfunc with 2 meter temperature
!                      ! If modelled 2 meter temperature is too different from measured air temperature then
!                      ! use temp_c
!                      IF (ABS(Temp_C - t2_C) > 5) THEN
!                         t2 = Temp_C
!                      ELSE
!                         t2 = t2_C
!                      END IF
!
!                      CALL cal_AtmMoist( &
!                         t2, Press_hPa, avRh, dectime, & ! input:
!                         dummy1, dummy2, & ! output:
!                         dummy3, dummy4, dummy5, dummy6, dq, dummy7, dummy8, dummy9)
!                      !MP: TODO FIX CO2!!! Equations for dummy10 all different assigned to one variable
!                      CALL SurfaceResistance( &
!                         id, it, & ! input:
!                         SMDMethod, SnowFrac, sfr_surf, avkdn, t2, dq, xsmd, vsmd, MaxConductance, &
!                         LAIMax, LAI_id, gsModel, Kmax, &
!                         G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, TH, TL, S1, S2, &
!                         dummy10, dummy10, dummy10, dummy10, dummy10, & ! output:
!                         gfunc2, dummy10, dummy11) ! output:
!                   END IF
!
! !                   ! Calculate CO2 fluxes from biogenic components
!                   IF (Diagnose == 1) WRITE (*, *) 'Calling CO2_biogen...'
!                   CALL CO2_biogen( &
!                      alpha_bioCO2, alpha_enh_bioCO2, avkdn, beta_bioCO2, beta_enh_bioCO2, BSoilSurf, & ! input:
!                      ConifSurf, DecidSurf, dectime, EmissionsMethod, gfunc, gfunc2, GrassSurf, gsmodel, &
!                      id, it, ivConif, ivDecid, ivGrass, LAI_id, LAIMin, LAIMax, min_res_bioCO2, nsurf, &
!                      NVegSurf, resp_a, resp_b, sfr_surf, SnowFrac, t2, Temp_C, theta_bioCO2, &
!                      Fc_biogen, Fc_photo, Fc_respi) ! output:
!                END IF
!
!                IF (EmissionsMethod >= 0 .AND. EmissionsMethod <= 6) THEN
!                   Fc_biogen = 0
!                   Fc_photo = 0
!                   Fc_respi = 0
!                END IF
!
!                Fc = Fc_anthro + Fc_biogen
!
!             END ASSOCIATE
!          END ASSOCIATE
!       END ASSOCIATE
!
!    END SUBROUTINE SUEWS_cal_BiogenCO2
! !========================================================================

!=============net all-wave radiation=====================================

   SUBROUTINE SUEWS_cal_Qn( &
      timer, config, forcing, siteInfo, & ! input
      modState, & ! input/output:
      dataOutLineSPARTACUS) ! output
      USE module_phys_narp, ONLY: RadMethod, NARP
      USE module_phys_spartacus, ONLY: SPARTACUS
      USE module_ctrl_type, ONLY: SUEWS_SITE, SUEWS_TIMER, SUEWS_CONFIG, SUEWS_FORCING
      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_TIMER, SNOW_STATE, SNOW_PRM, &
                               SUEWS_FORCING, SUEWS_SITE, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               PHENOLOGY_STATE, SPARTACUS_PRM, &
                               SPARTACUS_LAYER_PRM, HEAT_STATE, SUEWS_STATE

      IMPLICIT NONE
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      REAL(KIND(1D0)), DIMENSION(nsurf) :: emis ! Effective surface emissivity. [-]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: alb ! surface albedo [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac ! snow fractions of each surface [-]
      REAL(KIND(1D0)) :: albedo_snowfree !estimated albedo for snow-free surface [-]
      REAL(KIND(1D0)) :: SnowAlb ! updated snow albedo [-]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: lup_ind !outgoing longwave radiation from observation [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind !outgoing shortwave radiation from observation [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn1_ind !net all-wave radiation from observation [W m-2]

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER :: NetRadiationMethod_use
      INTEGER :: AlbedoChoice, ldown_option

      ! REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qn_wall ! net all-wave radiation on the wall [W m-2]
      ! REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: qn_roof ! net all-wave radiation on the roof [W m-2]

      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5), INTENT(OUT) :: dataOutLineSPARTACUS


      INTEGER, PARAMETER :: DiagQN = 0 ! flag for printing diagnostic info for QN module during runtime [N/A] ! not used and will be removed

      ASSOCIATE ( &
         solarState => modState%solarState, &
         atmState => modState%atmState, &
         phenState => modState%phenState, &
         snowState => modState%snowState, &
         heatState => modState%heatState, &
         ohmState => modState%ohmState, &
         stebbsState => modState%stebbsState &
         )
         ASSOCIATE ( &
            alb_prev => phenState%alb, &
            albDecTr_id => phenState%albDecTr_id, &
            albEveTr_id => phenState%albEveTr_id, &
            albGrass_id => phenState%albGrass_id, &
            LAI_id => phenState%LAI_id, &
            storageheatmethod => config%StorageHeatMethod, &
            NetRadiationMethod => config%NetRadiationMethod, &
            SnowUse => config%SnowUse, &
            Diagnose => config%Diagnose, &
            use_sw_direct_albedo => config%use_sw_direct_albedo, &
            tstep => timer%tstep, &
            ldown_obs => forcing%ldown, &
            fcld_obs => forcing%fcld, &
            kdown => forcing%kdown, &
            Tair_C => forcing%Temp_C, &
            avRH => forcing%RH, &
            qn1_obs => forcing%qn1_obs, &
            SnowPack_prev => snowState%SnowPack, &
            SnowAlb_prev => snowState%snowalb, &
            snowFrac_prev => snowState%snowFrac, &
            IceFrac => snowState%IceFrac, &
            qn_snow => snowState%qn_snow, &
            qn_ind_snow => snowState%qn_ind_snow, &
            kup_ind_snow => snowState%kup_ind_snow, &
            Tsurf_ind_snow => snowState%Tsurf_ind_snow, &
            dectime => timer%dectime, &
            ZENITH_deg => solarState%ZENITH_deg, &
            ea_hPa => atmState%ea_hPa, &
            fcld => atmState%fcld, &
            qn => heatState%qn, &
            kclear => heatState%kclear, &
            kup => heatState%kup, &
            lup => heatState%lup, &
            tsurf => heatState%tsurf, &
            qn_snowfree => heatState%qn_snowfree, &
            ldown => heatState%ldown, &
            qn_surf => heatState%qn_surf, &
            qn_roof => heatState%qn_roof, &
            qn_wall => heatState%qn_wall, &
            Tsurf_ind => heatState%Tsurf_ind, &
            buildings => stebbsState%buildings, &          
            spartacusPrm => siteInfo%spartacus, &
            spartacusLayerPrm => siteInfo%spartacus_layer, &
            NARP_TRANS_SITE => siteInfo%NARP_TRANS_SITE, &
            nlayer => siteInfo%nlayer, &
            sfr_roof => siteInfo%sfr_roof, &
            sfr_wall => siteInfo%sfr_wall, &
            sfr_surf => siteInfo%sfr_surf, &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            evetrPrm => siteInfo%lc_evetr, &
            dectrPrm => siteInfo%lc_dectr, &
            grassPrm => siteInfo%lc_grass, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            snowPrm => siteInfo%snow &
            )
            ASSOCIATE ( &
               building_frac => spartacusLayerPrm%building_frac, &
               veg_frac => spartacusLayerPrm%veg_frac, &
               building_scale => spartacusLayerPrm%building_scale, &
               veg_scale => spartacusLayerPrm%veg_scale, &
               alb_roof => spartacusLayerPrm%alb_roof, &
               emis_roof => spartacusLayerPrm%emis_roof, &
               alb_wall => spartacusLayerPrm%alb_wall, &
               emis_wall => spartacusLayerPrm%emis_wall, &
               roof_albedo_dir_mult_fact => spartacusLayerPrm%roof_albedo_dir_mult_fact, &
               wall_specular_frac => spartacusLayerPrm%wall_specular_frac, &
               n_vegetation_region_urban => spartacusPrm%n_vegetation_region_urban, &
               n_stream_sw_urban => spartacusPrm%n_stream_sw_urban, &
               n_stream_lw_urban => spartacusPrm%n_stream_lw_urban, &
               sw_dn_direct_frac => spartacusPrm%sw_dn_direct_frac, &
               air_ext_sw => spartacusPrm%air_ext_sw, &
               air_ssa_sw => spartacusPrm%air_ssa_sw, &
               veg_ssa_sw => spartacusPrm%veg_ssa_sw, &
               air_ext_lw => spartacusPrm%air_ext_lw, &
               air_ssa_lw => spartacusPrm%air_ssa_lw, &
               veg_ssa_lw => spartacusPrm%veg_ssa_lw, &
               veg_fsd_const => spartacusPrm%veg_fsd_const, &
               veg_contact_fraction_const => spartacusPrm%veg_contact_fraction_const, &
               ground_albedo_dir_mult_fact => spartacusPrm%ground_albedo_dir_mult_fact, &
               height => spartacusPrm%height, &
               tau_a => snowPrm%tau_a, &
               tau_f => snowPrm%tau_f, &
               SnowAlbMax => snowPrm%SnowAlbMax, &
               SnowAlbMin => snowPrm%SnowAlbMin, &
               NARP_EMIS_SNOW => snowPrm%NARP_EMIS_SNOW, &
               roof_in_sw_spc => heatState%roof_in_sw_spc, &
               roof_in_lw_spc => heatState%roof_in_lw_spc, &
               wall_in_sw_spc => heatState%wall_in_sw_spc, &
               wall_in_lw_spc => heatState%wall_in_lw_spc, &
               tsfc_surf => MERGE(heatState%tsfc_surf_dyohm, heatState%tsfc_surf, (storageheatmethod == 6 .OR. storageheatmethod == 7)), &
               tsfc_roof => MERGE(buildings(1)%Textroof_C, heatState%tsfc_roof, storageheatmethod == 7), &
               tsfc_wall => MERGE(buildings(1)%Textwall_C, heatState%tsfc_wall, storageheatmethod == 7) &               
               )

               emis = [pavedPrm%emis, bldgPrm%emis, evetrPrm%emis, dectrPrm%emis, &
                       grassPrm%emis, bsoilPrm%emis, waterPrm%emis]
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
                  ! IF (Diagqn == 1) WRITE (*, *) 'NetRadiationMethodX:', NetRadiationMethod_use
                  ! IF (Diagqn == 1) WRITE (*, *) 'AlbedoChoice:', AlbedoChoice

                  ! TODO: TS 14 Feb 2022, ESTM development:
                  ! here we use uniform `tsurf_0` for all land covers, which should be distinguished in future developments

                  CALL NARP( &
                     nsurf, sfr_surf, tsfc_surf, SnowFrac, alb, emis, IceFrac, & !
                     NARP_TRANS_SITE, NARP_EMIS_SNOW, &
                     dectime, ZENITH_deg, kdown, Tair_C, avRH, ea_hPa, qn1_obs, ldown_obs, &
                     SnowAlb, &
                     AlbedoChoice, ldown_option, NetRadiationMethod_use, DiagQN, &
                     qn_surf, & ! output:
                     qn, qn_snowfree, qn_snow, kclear, kup, LDown, lup, fcld, tsurf, & ! output:
                     qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, albedo_snowfree)

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
                        roof_in_sw_spc, roof_in_lw_spc, &
                        wall_in_sw_spc, wall_in_lw_spc, &
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
               ! alb_next = alb
               phenState%alb = alb
               snowState%SnowAlb = SnowAlb
            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_cal_Qn
!========================================================================

!=============storage heat flux=========================================

   SUBROUTINE SUEWS_cal_Qs( &
      timer, config, forcing, siteInfo, datetimeLine, & ! input
      modState, & ! input/output:
      dataOutLineESTM, dataOutLineSTEBBS)

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_SITE, SUEWS_TIMER, &
                               SNOW_STATE, EHC_PRM, &
                               anthroEmis_STATE, PHENOLOGY_STATE, OHM_STATE, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               HEAT_STATE, HYDRO_STATE, SUEWS_STATE
      USE module_ctrl_const_allocate, ONLY: ncolumnsDataOutSTEBBS
      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(in) :: timer
      TYPE(SUEWS_CONFIG), INTENT(in) :: config
      TYPE(SUEWS_FORCING), INTENT(in) :: forcing
      TYPE(SUEWS_SITE), INTENT(in) :: siteInfo
      REAL(KIND(1D0)), DIMENSION(5), INTENT(in) :: datetimeLine !date & time
      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      REAL(KIND(1D0)) :: OHM_coef(nsurf + 1, 4, 3) ! OHM coefficients [-]
      REAL(KIND(1D0)) :: OHM_threshSW(nsurf + 1) ! Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)) :: OHM_threshWD(nsurf + 1) ! Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]

      REAL(KIND(1D0)) :: SoilStoreCap(nsurf) ! capacity of soil store [mm]

      REAL(KIND(1D0)) :: state_id(nsurf) ! wetness status [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: emis ! emissivity [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: cpAnOHM ! heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kkAnOHM ! thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chAnOHM ! bulk transfer coef [J m-3 K-1]

      REAL(KIND(1D0)), DIMENSION(27), INTENT(out) :: dataOutLineESTM !data output from ESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSTEBBS - 5), INTENT(out) :: dataOutLineSTEBBS !data output from STEBBS

      ! internal use arrays
      REAL(KIND(1D0)) :: Tair_mav_5d ! Tair_mav_5d=HDD(id-1,4) HDD at the begining of today (id-1)
      REAL(KIND(1D0)) :: qn_use ! qn used in OHM calculations [W m-2]

      ASSOCIATE ( &
         atmState => modState%atmState, &
         heatState => modState%heatState, &
         solarstate => modState%solarstate, &
         snowState => modState%snowState, &
         hydroState => modState%hydroState, &
         anthroEmisState => modState%anthroEmisState, &
         phenState => modState%phenState, &
         stebbsState => modState%stebbsState, &
         ohmState => modState%ohmState &
         )

         ASSOCIATE ( &
            nlayer => siteInfo%nlayer, &
            ehcPrm => siteInfo%ehc, &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            evetrPrm => siteInfo%lc_evetr, &
            dectrPrm => siteInfo%lc_dectr, &
            grassPrm => siteInfo%lc_grass, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            sfr_roof => siteInfo%sfr_roof, &
            sfr_wall => siteInfo%sfr_wall, &
            sfr_surf => siteInfo%sfr_surf, &
            lambda_c => siteInfo%lambda_c, &
            StorageHeatMethod => config%StorageHeatMethod, &
            OHMIncQF => config%OHMIncQF, &
            Diagnose => config%Diagnose, &
            SnowUse => config%SnowUse, &
            EmissionsMethod => config%EmissionsMethod, &
            DiagQS => config%DiagQS, &
            Gridiv => siteInfo%Gridiv, &
            Ts5mindata_ir => forcing%Ts5mindata_ir, &
            qs_obs => forcing%qs_obs, &
            avkdn => forcing%kdown, &
            avu1 => forcing%U, &
            temp_c => forcing%temp_c, &
            avrh => forcing%RH, &
            press_hpa => forcing%pres, &
            Tair_av => atmState%Tair_av, &
            zenith_deg => solarstate%zenith_deg, &
            qf => heatState%qf, &
            qn => heatState%qn, &
            qs => heatState%qs, &
            ldown => heatState%ldown, &
            tsfc_roof => heatState%tsfc_roof, &
            tsfc_wall => heatState%tsfc_wall, &
            tsfc_surf => heatState%tsfc_surf, &
            temp_in_roof => heatState%temp_roof, &
            temp_in_wall => heatState%temp_wall, &
            temp_in_surf => heatState%temp_surf, &
            QS_roof => heatState%QS_roof, &
            QS_wall => heatState%QS_wall, &
            QS_surf => heatState%QS_surf, &
            QS_stebbs => stebbsState%QS_stebbs, &
            qn_snow => snowState%qn_snow, &
            deltaQi => snowState%deltaQi, &
            SnowFrac => snowState%SnowFrac, &
            soilstore_id => hydroState%soilstore_surf, &
            state_id => hydroState%state_surf, &
            HDD_id => anthroEmisState%HDD_id, &
            a1 => ohmState%a1, &
            a2 => ohmState%a2, &
            a3 => ohmState%a3, &
            a1_bldg => ohmState%a1_bldg, &
            a2_bldg => ohmState%a2_bldg, &
            a3_bldg => ohmState%a3_bldg, &
            a1_paved => ohmState%a1_paved, &
            a2_paved => ohmState%a2_paved, &
            a3_paved => ohmState%a3_paved, &
            a1_evetr => ohmState%a1_evetr, &
            a2_evetr => ohmState%a2_evetr, &
            a3_evetr => ohmState%a3_evetr, &             
            a1_dectr => ohmState%a1_dectr, &
            a2_dectr => ohmState%a2_dectr, &
            a3_dectr => ohmState%a3_dectr, &        
            a1_grass => ohmState%a1_grass, &
            a2_grass => ohmState%a2_grass, &
            a3_grass => ohmState%a3_grass, &   
            a1_bsoil => ohmState%a1_bsoil, &
            a2_bsoil => ohmState%a2_bsoil, &
            a3_bsoil => ohmState%a3_bsoil, &   
            a1_water => ohmState%a1_water, &
            a2_water => ohmState%a2_water, &
            a3_water => ohmState%a3_water, &
            t2_prev => ohmState%t2_prev, &
            ws_rav => ohmState%ws_rav, &
            qn_rav => ohmState%qn_rav, &
            alb => phenState%alb, &
            StoreDrainPrm => phenState%StoreDrainPrm, &
            id => timer%id, &
            tstep => timer%tstep, &
            dt_since_start => timer%dt_since_start &
            )
            ASSOCIATE ( &
               tin_roof => ehcPrm%tin_roof, &
               k_roof => ehcPrm%k_roof, &
               cp_roof => ehcPrm%cp_roof, &
               dz_roof => ehcPrm%dz_roof, &
               tin_wall => ehcPrm%tin_wall, &
               k_wall => ehcPrm%k_wall, &
               cp_wall => ehcPrm%cp_wall, &
               dz_wall => ehcPrm%dz_wall, &
               tin_surf => ehcPrm%tin_surf, &
               k_surf => ehcPrm%k_surf, &
               cp_surf => ehcPrm%cp_surf, &
               dz_surf => ehcPrm%dz_surf, &
               bldgh => bldgPrm%bldgh &
               )

               ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
               IF (storageheatmethod /= 6 .AND. storageheatmethod /= 7) THEN
                  OHM_coef(1, 1, 1) = pavedPrm%ohm%ohm_coef_lc(1)%summer_wet
                  OHM_coef(1, 2, 1) = pavedPrm%ohm%ohm_coef_lc(1)%summer_dry
                  OHM_coef(1, 3, 1) = pavedPrm%ohm%ohm_coef_lc(1)%winter_wet
                  OHM_coef(1, 4, 1) = pavedPrm%ohm%ohm_coef_lc(1)%winter_dry
                  !WRITE (*, *) 'OHM_coef_paved(1)%winter_dry', OHM_coef_paved(1)%winter_dry
                  !WRITE (*, *) 'OHM_coef(1, 4, 1)', OHM_coef(1, 4, 1)

                  OHM_coef(1, 1, 2) = pavedPrm%ohm%ohm_coef_lc(2)%summer_wet
                  OHM_coef(1, 2, 2) = pavedPrm%ohm%ohm_coef_lc(2)%summer_dry
                  OHM_coef(1, 3, 2) = pavedPrm%ohm%ohm_coef_lc(2)%winter_wet
                  OHM_coef(1, 4, 2) = pavedPrm%ohm%ohm_coef_lc(2)%winter_dry

                  OHM_coef(1, 1, 3) = pavedPrm%ohm%ohm_coef_lc(3)%summer_wet
                  OHM_coef(1, 2, 3) = pavedPrm%ohm%ohm_coef_lc(3)%summer_dry
                  OHM_coef(1, 3, 3) = pavedPrm%ohm%ohm_coef_lc(3)%winter_wet
                  OHM_coef(1, 4, 3) = pavedPrm%ohm%ohm_coef_lc(3)%winter_dry

                  OHM_coef(2, 1, 1) = bldgPrm%ohm%ohm_coef_lc(1)%summer_wet
                  OHM_coef(2, 2, 1) = bldgPrm%ohm%ohm_coef_lc(1)%summer_dry
                  OHM_coef(2, 3, 1) = bldgPrm%ohm%ohm_coef_lc(1)%winter_wet
                  OHM_coef(2, 4, 1) = bldgPrm%ohm%ohm_coef_lc(1)%winter_dry

                  OHM_coef(2, 1, 2) = bldgPrm%ohm%ohm_coef_lc(2)%summer_wet
                  OHM_coef(2, 2, 2) = bldgPrm%ohm%ohm_coef_lc(2)%summer_dry
                  OHM_coef(2, 3, 2) = bldgPrm%ohm%ohm_coef_lc(2)%winter_wet
                  OHM_coef(2, 4, 2) = bldgPrm%ohm%ohm_coef_lc(2)%winter_dry

                  OHM_coef(2, 1, 3) = bldgPrm%ohm%ohm_coef_lc(3)%summer_wet
                  OHM_coef(2, 2, 3) = bldgPrm%ohm%ohm_coef_lc(3)%summer_dry
                  OHM_coef(2, 3, 3) = bldgPrm%ohm%ohm_coef_lc(3)%winter_wet
                  OHM_coef(2, 4, 3) = bldgPrm%ohm%ohm_coef_lc(3)%winter_dry

                  OHM_coef(3, 1, 1) = evetrPrm%ohm%ohm_coef_lc(1)%summer_wet
                  OHM_coef(3, 2, 1) = evetrPrm%ohm%ohm_coef_lc(1)%summer_dry
                  OHM_coef(3, 3, 1) = evetrPrm%ohm%ohm_coef_lc(1)%winter_wet
                  OHM_coef(3, 4, 1) = evetrPrm%ohm%ohm_coef_lc(1)%winter_dry

                  OHM_coef(3, 1, 2) = evetrPrm%ohm%ohm_coef_lc(2)%summer_wet
                  OHM_coef(3, 2, 2) = evetrPrm%ohm%ohm_coef_lc(2)%summer_dry
                  OHM_coef(3, 3, 2) = evetrPrm%ohm%ohm_coef_lc(2)%winter_wet
                  OHM_coef(3, 4, 2) = evetrPrm%ohm%ohm_coef_lc(2)%winter_dry

                  OHM_coef(3, 1, 3) = evetrPrm%ohm%ohm_coef_lc(3)%summer_wet
                  OHM_coef(3, 2, 3) = evetrPrm%ohm%ohm_coef_lc(3)%summer_dry
                  OHM_coef(3, 3, 3) = evetrPrm%ohm%ohm_coef_lc(3)%winter_wet
                  OHM_coef(3, 4, 3) = evetrPrm%ohm%ohm_coef_lc(3)%winter_dry

                  OHM_coef(4, 1, 1) = dectrPrm%ohm%ohm_coef_lc(1)%summer_wet
                  OHM_coef(4, 2, 1) = dectrPrm%ohm%ohm_coef_lc(1)%summer_dry
                  OHM_coef(4, 3, 1) = dectrPrm%ohm%ohm_coef_lc(1)%winter_wet
                  OHM_coef(4, 4, 1) = dectrPrm%ohm%ohm_coef_lc(1)%winter_dry

                  OHM_coef(4, 1, 2) = dectrPrm%ohm%ohm_coef_lc(2)%summer_wet
                  OHM_coef(4, 2, 2) = dectrPrm%ohm%ohm_coef_lc(2)%summer_dry
                  OHM_coef(4, 3, 2) = dectrPrm%ohm%ohm_coef_lc(2)%winter_wet
                  OHM_coef(4, 4, 2) = dectrPrm%ohm%ohm_coef_lc(2)%winter_dry

                  OHM_coef(4, 1, 3) = dectrPrm%ohm%ohm_coef_lc(3)%summer_wet
                  OHM_coef(4, 2, 3) = dectrPrm%ohm%ohm_coef_lc(3)%summer_dry
                  OHM_coef(4, 3, 3) = dectrPrm%ohm%ohm_coef_lc(3)%winter_wet
                  OHM_coef(4, 4, 3) = dectrPrm%ohm%ohm_coef_lc(3)%winter_dry

                  OHM_coef(5, 1, 1) = grassPrm%ohm%ohm_coef_lc(1)%summer_wet
                  OHM_coef(5, 2, 1) = grassPrm%ohm%ohm_coef_lc(1)%summer_dry
                  OHM_coef(5, 3, 1) = grassPrm%ohm%ohm_coef_lc(1)%winter_wet
                  OHM_coef(5, 4, 1) = grassPrm%ohm%ohm_coef_lc(1)%winter_dry

                  OHM_coef(5, 1, 2) = grassPrm%ohm%ohm_coef_lc(2)%summer_wet
                  OHM_coef(5, 2, 2) = grassPrm%ohm%ohm_coef_lc(2)%summer_dry
                  OHM_coef(5, 3, 2) = grassPrm%ohm%ohm_coef_lc(2)%winter_wet
                  OHM_coef(5, 4, 2) = grassPrm%ohm%ohm_coef_lc(2)%winter_dry

                  OHM_coef(5, 1, 3) = grassPrm%ohm%ohm_coef_lc(3)%summer_wet
                  OHM_coef(5, 2, 3) = grassPrm%ohm%ohm_coef_lc(3)%summer_dry
                  OHM_coef(5, 3, 3) = grassPrm%ohm%ohm_coef_lc(3)%winter_wet
                  OHM_coef(5, 4, 3) = grassPrm%ohm%ohm_coef_lc(3)%winter_dry

                  OHM_coef(6, 1, 1) = bsoilPrm%ohm%ohm_coef_lc(1)%summer_wet
                  OHM_coef(6, 2, 1) = bsoilPrm%ohm%ohm_coef_lc(1)%summer_dry
                  OHM_coef(6, 3, 1) = bsoilPrm%ohm%ohm_coef_lc(1)%winter_wet
                  OHM_coef(6, 4, 1) = bsoilPrm%ohm%ohm_coef_lc(1)%winter_dry

                  OHM_coef(6, 1, 2) = bsoilPrm%ohm%ohm_coef_lc(2)%summer_wet
                  OHM_coef(6, 2, 2) = bsoilPrm%ohm%ohm_coef_lc(2)%summer_dry
                  OHM_coef(6, 3, 2) = bsoilPrm%ohm%ohm_coef_lc(2)%winter_wet
                  OHM_coef(6, 4, 2) = bsoilPrm%ohm%ohm_coef_lc(2)%winter_dry

                  OHM_coef(6, 1, 3) = bsoilPrm%ohm%ohm_coef_lc(3)%summer_wet
                  OHM_coef(6, 2, 3) = bsoilPrm%ohm%ohm_coef_lc(3)%summer_dry
                  OHM_coef(6, 3, 3) = bsoilPrm%ohm%ohm_coef_lc(3)%winter_wet
                  OHM_coef(6, 4, 3) = bsoilPrm%ohm%ohm_coef_lc(3)%winter_dry

                  OHM_coef(7, 1, 1) = waterPrm%ohm%ohm_coef_lc(1)%summer_wet
                  OHM_coef(7, 2, 1) = waterPrm%ohm%ohm_coef_lc(1)%summer_dry
                  OHM_coef(7, 3, 1) = waterPrm%ohm%ohm_coef_lc(1)%winter_wet
                  OHM_coef(7, 4, 1) = waterPrm%ohm%ohm_coef_lc(1)%winter_dry

                  OHM_coef(7, 1, 2) = waterPrm%ohm%ohm_coef_lc(2)%summer_wet
                  OHM_coef(7, 2, 2) = waterPrm%ohm%ohm_coef_lc(2)%summer_dry
                  OHM_coef(7, 3, 2) = waterPrm%ohm%ohm_coef_lc(2)%winter_wet
                  OHM_coef(7, 4, 2) = waterPrm%ohm%ohm_coef_lc(2)%winter_dry

                  OHM_coef(7, 1, 3) = waterPrm%ohm%ohm_coef_lc(3)%summer_wet
                  OHM_coef(7, 2, 3) = waterPrm%ohm%ohm_coef_lc(3)%summer_dry
                  OHM_coef(7, 3, 3) = waterPrm%ohm%ohm_coef_lc(3)%winter_wet
                  OHM_coef(7, 4, 3) = waterPrm%ohm%ohm_coef_lc(3)%winter_dry
               END IF
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

               OHM_threshSW(1) = pavedPrm%ohm%ohm_threshsw
               OHM_threshSW(2) = bldgPrm%ohm%ohm_threshsw
               OHM_threshSW(3) = evetrPrm%ohm%ohm_threshsw
               OHM_threshSW(4) = dectrPrm%ohm%ohm_threshsw
               OHM_threshSW(5) = grassPrm%ohm%ohm_threshsw
               OHM_threshSW(6) = bsoilPrm%ohm%ohm_threshsw
               OHM_threshSW(7) = waterPrm%ohm%ohm_threshsw

               OHM_threshWD(1) = pavedPrm%ohm%ohm_threshwd
               OHM_threshWD(2) = bldgPrm%ohm%ohm_threshwd
               OHM_threshWD(3) = evetrPrm%ohm%ohm_threshwd
               OHM_threshWD(4) = dectrPrm%ohm%ohm_threshwd
               OHM_threshWD(5) = grassPrm%ohm%ohm_threshwd
               OHM_threshWD(6) = bsoilPrm%ohm%ohm_threshwd
               OHM_threshWD(7) = waterPrm%ohm%ohm_threshwd

               SoilStoreCap(1) = pavedPrm%soil%soilstorecap
               SoilStoreCap(2) = bldgPrm%soil%soilstorecap
               SoilStoreCap(3) = evetrPrm%soil%soilstorecap
               SoilStoreCap(4) = dectrPrm%soil%soilstorecap
               SoilStoreCap(5) = grassPrm%soil%soilstorecap
               SoilStoreCap(6) = bsoilPrm%soil%soilstorecap
               SoilStoreCap(7) = waterPrm%soil%soilstorecap

               emis(1) = pavedPrm%emis
               emis(2) = bldgPrm%emis
               emis(3) = evetrPrm%emis
               emis(4) = dectrPrm%emis
               emis(5) = grassPrm%emis
               emis(6) = bsoilPrm%emis
               emis(7) = waterPrm%emis

               cpAnOHM(1) = pavedPrm%ohm%cpanohm
               cpAnOHM(2) = bldgPrm%ohm%cpanohm
               cpAnOHM(3) = evetrPrm%ohm%cpanohm
               cpAnOHM(4) = dectrPrm%ohm%cpanohm
               cpAnOHM(5) = grassPrm%ohm%cpanohm
               cpAnOHM(6) = bsoilPrm%ohm%cpanohm
               cpAnOHM(7) = waterPrm%ohm%cpanohm

               kkAnOHM(1) = pavedPrm%ohm%kkanohm
               kkAnOHM(2) = bldgPrm%ohm%kkanohm
               kkAnOHM(3) = evetrPrm%ohm%kkanohm
               kkAnOHM(4) = dectrPrm%ohm%kkanohm
               kkAnOHM(5) = grassPrm%ohm%kkanohm
               kkAnOHM(6) = bsoilPrm%ohm%kkanohm
               kkAnOHM(7) = waterPrm%ohm%kkanohm

               chAnOHM(1) = pavedPrm%ohm%chanohm
               chAnOHM(2) = bldgPrm%ohm%chanohm
               chAnOHM(3) = evetrPrm%ohm%chanohm
               chAnOHM(4) = dectrPrm%ohm%chanohm
               chAnOHM(5) = grassPrm%ohm%chanohm
               chAnOHM(6) = bsoilPrm%ohm%chanohm
               chAnOHM(7) = waterPrm%ohm%chanohm

               ! WRITE (*, *) 'OHM_coef = ', OHM_coef

               ! initialise output variables
               deltaQi = 0
               !SnowFrac = 0
               !qn1_S = 0
               dataOutLineESTM = -999
               dataOutLineSTEBBS = -999

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

               ELSEIF (StorageHeatMethod == 1 .OR. StorageHeatMethod == 6 .OR. StorageHeatMethod == 7) THEN !Use OHM to calculate QS
                  Tair_mav_5d = HDD_id(10)
                  IF (Diagnose == 1) WRITE (*, *) 'Calling OHM...'
                  CALL OHM(qn_use, ohmState%qn_av, ohmState%dqndt, &
                           ohmState%qn_av, ohmState%dqndt, &
                           qn_snow, ohmState%qn_s_av, ohmState%dqnsdt, &
                           ohmState%qn_s_av, ohmState%dqnsdt, &
                           tstep, dt_since_start, &
                           sfr_surf, nsurf, &
                           Tair_mav_5d, &
                           OHM_coef, &
                           OHM_threshSW, OHM_threshWD, &
                           soilstore_id, SoilStoreCap, state_id, &
                           BldgSurf, WaterSurf, &
                           SnowUse, SnowFrac, &
                           atmState%U_hbh, atmState%T_hbh_C, t2_prev, &
                           ws_rav, qn_rav, nlayer, &
                           dz_roof, cp_roof, k_roof, &
                           dz_wall, cp_wall, k_wall, &
                           dz_surf, cp_surf, k_surf, &
                           lambda_c, &
                           StorageHeatMethod, DiagQS, timer, &
                           a1_bldg, a2_bldg, a3_bldg, &
                           a1_paved, a2_paved, a3_paved, &
                           a1_evetr, a2_evetr, a3_evetr, &
                           a1_dectr, a2_dectr, a3_dectr, &
                           a1_grass, a2_grass, a3_grass, &
                           a1_bsoil, a2_bsoil, a3_bsoil, &
                           a1_water, a2_water, a3_water, & 
                           a1, a2, a3, qs, deltaQi)
                  QS_surf = qs
                  QS_roof = qs
                  QS_wall = qs

                  ! use AnOHM to calculate QS, TS 14 Mar 2016
                  ! disable AnOHM, TS 20 Jul 2023
                  ! ELSEIF (StorageHeatMethod == 3) THEN !
                  !    IF (Diagnose == 1) WRITE (*, *) 'Calling AnOHM...'
                  !    ! CALL AnOHM(qn1_use,qn1_store_grid,qn1_av_store_grid,qf,&
                  !    !      MetForcingData_grid,state_id/StoreDrainPrm(6,:),&
                  !    !      alb, emis, cpAnOHM, kkAnOHM, chAnOHM,&
                  !    !      sfr_surf,nsurf,nsh,EmissionsMethod,id,Gridiv,&
                  !    !      a1,a2,a3,qs,deltaQi)
                  !    moist_surf = state_id/StoreDrainPrm(6, :)
                  !    ! CALL AnOHM( &
                  !    !    tstep, dt_since_start, &
                  !    !    qn_use, qn_av_prev, dqndt_prev, qf, &
                  !    !    MetForcingData_grid, moist_surf, &
                  !    !    alb, emis, cpAnOHM, kkAnOHM, chAnOHM, & ! input
                  !    !    sfr_surf, nsurf, EmissionsMethod, id, Gridiv, &
                  !    !    qn_av_next, dqndt_next, &
                  !    !    a1, a2, a3, qs, deltaQi) ! output
                  !    QS_surf = qs
                  !    QS_roof = qs
                  !    QS_wall = qs

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
                     ! QG_surf, qg_roof, qg_wall, &
                     tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
                     tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
                     tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
                     heatState%temp_roof, QS_roof, & !output
                     heatState%temp_wall, QS_wall, & !output
                     heatState%temp_surf, QS_surf, & !output
                     QS) !output

                  ! TODO: add deltaQi to output for snow heat storage

                  ! PRINT *, 'QS after ESTM_ehc', QS
                  ! PRINT *, 'QS_roof after ESTM_ehc', QS_roof
                  ! PRINT *, 'QS_wall after ESTM_ehc', QS_wall
                  ! PRINT *, 'QS_surf after ESTM_ehc', QS_surf
                  ! PRINT *, '------------------------------------'
                  ! PRINT *, ''

               END IF
               !==============use STEBBS to get localised surface temperature and storage heat flux==================
               ! MP 12 Sep 2024: STEBBS is a simplified BEM
               IF (config%stebbsmethod == 1 .OR. config%stebbsmethod == 2) THEN
                  IF (Diagnose == 1) WRITE (*, *) 'Calling STEBBS...'
                  CALL stebbsonlinecouple( &
                     timer, config, forcing, siteInfo, & ! input
                     modState, & ! input/output:
                     datetimeLine, nlayer, & ! input
                     dataOutLineSTEBBS) ! output
                  IF (StorageHeatMethod == 7) THEN
                     qs = qs + QS_stebbs * sfr_surf(2) 
                  END IF 
               END IF
            
            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_Qs
!=======================================================================

!==========================drainage and runoff================================
   SUBROUTINE SUEWS_cal_Water( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, PHENOLOGY_STATE, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, HYDRO_STATE, &
                               SUEWS_STATE

      IMPLICIT NONE
      TYPE(SUEWS_TIMER), INTENT(in) :: timer
      TYPE(SUEWS_CONFIG), INTENT(in) :: config
      TYPE(SUEWS_FORCING), INTENT(in) :: forcing
      TYPE(SUEWS_SITE), INTENT(in) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INout) :: modState

      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id !wetness states of each surface [mm]

      REAL(KIND(1D0)), DIMENSION(6, nsurf) :: StoreDrainPrm ! drain storage capacity [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf + 1, nsurf - 1) :: WaterDist !Within-grid water distribution to other surfaces and runoff/soil store [-]

      INTEGER :: is

      ASSOCIATE ( &
         hydroState => modState%hydroState, &
         phenState => modState%phenState)

         ASSOCIATE ( &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            evetrPrm => siteInfo%lc_evetr, &
            dectrPrm => siteInfo%lc_dectr, &
            grassPrm => siteInfo%lc_grass, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            sfr_surf => siteInfo%sfr_surf, &
            NonWaterFraction => siteInfo%NonWaterFraction, &
            nsh_real => timer%nsh_real, &
            Diagnose => config%Diagnose, &
            addPipes => hydroState%addPipes, &
            addImpervious => hydroState%addImpervious, &
            addVeg => hydroState%addVeg, &
            addWaterBody => hydroState%addWaterBody, &
            drain_per_tstep => hydroState%drain_per_tstep, &
            drain_surf => hydroState%drain_surf, &
            frac_water2runoff => hydroState%frac_water2runoff, &
            AdditionalWater => hydroState%AdditionalWater, &
            runoffPipes => hydroState%runoffPipes, &
            runoff_per_interval => hydroState%runoff_per_interval, &
            AddWater => hydroState%AddWater, &
            SnowUse => config%SnowUse &
            )

            AdditionalWater = 0.0
            AdditionalWater = addPipes + addImpervious + addVeg + addWaterBody ![mm]

            ! Diagnose = config%Diagnose
            ! SnowUse = config%SnowUse

            state_id = hydroState%state_surf
            StoreDrainPrm = phenState%StoreDrainPrm

            ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
            WaterDist(1, 1) = pavedPrm%waterdist%to_paved
            WaterDist(2, 1) = pavedPrm%waterdist%to_bldg
            WaterDist(3, 1) = pavedPrm%waterdist%to_evetr
            WaterDist(4, 1) = pavedPrm%waterdist%to_dectr
            WaterDist(5, 1) = pavedPrm%waterdist%to_grass
            WaterDist(6, 1) = pavedPrm%waterdist%to_bsoil
            WaterDist(7, 1) = pavedPrm%waterdist%to_water
            WaterDist(8, 1) = pavedPrm%waterdist%to_soilstore

            WaterDist(1, 2) = bldgPrm%waterdist%to_paved
            WaterDist(2, 2) = bldgPrm%waterdist%to_bldg
            WaterDist(3, 2) = bldgPrm%waterdist%to_evetr
            WaterDist(4, 2) = bldgPrm%waterdist%to_dectr
            WaterDist(5, 2) = bldgPrm%waterdist%to_grass
            WaterDist(6, 2) = bldgPrm%waterdist%to_bsoil
            WaterDist(7, 2) = bldgPrm%waterdist%to_water
            WaterDist(8, 2) = bldgPrm%waterdist%to_soilstore

            WaterDist(1, 3) = evetrPrm%waterdist%to_paved
            WaterDist(2, 3) = evetrPrm%waterdist%to_bldg
            WaterDist(3, 3) = evetrPrm%waterdist%to_evetr
            WaterDist(4, 3) = evetrPrm%waterdist%to_dectr
            WaterDist(5, 3) = evetrPrm%waterdist%to_grass
            WaterDist(6, 3) = evetrPrm%waterdist%to_bsoil
            WaterDist(7, 3) = evetrPrm%waterdist%to_water
            WaterDist(8, 3) = evetrPrm%waterdist%to_soilstore

            WaterDist(1, 4) = dectrPrm%waterdist%to_paved
            WaterDist(2, 4) = dectrPrm%waterdist%to_bldg
            WaterDist(3, 4) = dectrPrm%waterdist%to_evetr
            WaterDist(4, 4) = dectrPrm%waterdist%to_dectr
            WaterDist(5, 4) = dectrPrm%waterdist%to_grass
            WaterDist(6, 4) = dectrPrm%waterdist%to_bsoil
            WaterDist(7, 4) = dectrPrm%waterdist%to_water
            WaterDist(8, 4) = dectrPrm%waterdist%to_soilstore

            WaterDist(1, 5) = grassPrm%waterdist%to_paved
            WaterDist(2, 5) = grassPrm%waterdist%to_bldg
            WaterDist(3, 5) = grassPrm%waterdist%to_evetr
            WaterDist(4, 5) = grassPrm%waterdist%to_dectr
            WaterDist(5, 5) = grassPrm%waterdist%to_grass
            WaterDist(6, 5) = grassPrm%waterdist%to_bsoil
            WaterDist(7, 5) = grassPrm%waterdist%to_water
            WaterDist(8, 5) = grassPrm%waterdist%to_soilstore

            WaterDist(1, 6) = bsoilPrm%waterdist%to_paved
            WaterDist(2, 6) = bsoilPrm%waterdist%to_bldg
            WaterDist(3, 6) = bsoilPrm%waterdist%to_evetr
            WaterDist(4, 6) = bsoilPrm%waterdist%to_dectr
            WaterDist(5, 6) = bsoilPrm%waterdist%to_grass
            WaterDist(6, 6) = bsoilPrm%waterdist%to_bsoil
            WaterDist(7, 6) = bsoilPrm%waterdist%to_water
            WaterDist(8, 6) = bsoilPrm%waterdist%to_soilstore

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
            ! CHECK p_i
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
                     drain_surf(is)) ! output

                  ! !HCW added and changed to StoreDrainPrm(6,is) here 20 Feb 2015
                  ! drain_per_tstep=drain_per_tstep+(drain(is)*sfr_surf(is)/NonWaterFraction)   !No water body included
               END DO
               drain_per_tstep = DOT_PRODUCT(drain_surf(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction !No water body included
            ELSE
               drain_surf(1:nsurf - 1) = 0
               drain_per_tstep = 0
            END IF

            drain_surf(WaterSurf) = 0 ! Set drainage from water body to zero

            ! Distribute water within grid, according to WithinGridWaterDist matrix (Cols 1-7)
            IF (Diagnose == 1) WRITE (*, *) 'Calling ReDistributeWater...'
            ! CALL ReDistributeWater
            !Calculates AddWater(is)
            CALL ReDistributeWater( &
               SnowUse, WaterDist, sfr_surf, drain_surf, & ! input:
               frac_water2runoff, AddWater) ! output

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_Water
!=======================================================================

!========================================================================

   SUBROUTINE SUEWS_cal_snow( &
      timer, config, forcing, siteInfo, & ! input
      modState, &
      dataOutLineSnow)

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_TIMER, SNOW_PRM, &
                               SUEWS_FORCING, PHENOLOGY_STATE, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, &
                               LC_DECTR_PRM, LC_GRASS_PRM, LC_BSOIL_PRM, &
                               LC_WATER_PRM, SUEWS_SITE, SNOW_STATE, HYDRO_STATE, &
                               atm_state

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState

      ! TYPE(atm_state), INTENT(INout) :: atmState

      ! TYPE(HEAT_STATE), INTENT(INOUT) :: heatState
      ! TYPE(HYDRO_STATE), INTENT(INOUT) :: hydroState

      TYPE(HYDRO_STATE) :: hydroState_prev
      TYPE(HYDRO_STATE) :: hydroState_next

      ! TYPE(PHENOLOGY_STATE) :: phenState
      ! TYPE(SNOW_STATE) :: snowState
      TYPE(SNOW_STATE) :: snowState_prev
      TYPE(SNOW_STATE) :: snowState_next

      INTEGER, DIMENSION(nsurf) :: snowCalcSwitch

      REAL(KIND(1D0)), DIMENSION(nsurf) :: WetThresh_surf !surface wetness threshold [mm], When State > WetThresh, RS=0 limit in SUEWS_evap [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: mw_ind !melt water from sknowpack[mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: rainonsnow !rain water on snow event [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezmelt !freezing of melt water[mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezstate !freezing of state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: freezstatevol !surface state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Qm_Melt !melt heat [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Qm_rain !melt heat for rain on snow [W m-2]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: SnowProf_24hr !Hourly profile values used in snow clearing [-]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_surf ! runoff for each surface [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ChangSnow_surf !change in SnowPack (mm)
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowToSurf !the water flowing into snow free area [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_snow !Evaporation of now [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_surf !evaporation of each surface type [mm]

      REAL(KIND(1D0)) :: qe_per_tstep !latent heat flux at each timestep[W m-2]

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
      REAL(KIND(1D0)) :: qn_e !net available energy for evaporation [W m-2]
      REAL(KIND(1D0)) :: tlv !Latent heat of vapourisation per timestep [J kg-1 s-1]
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

      REAL(KIND(1D0)) :: SnowfallCum

      REAL(KIND(1D0)) :: SnowAlb
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5), INTENT(out) :: dataOutLineSnow

      ASSOCIATE ( &
         pavedPrm => siteInfo%lc_paved, &
         bldgPrm => siteInfo%lc_bldg, &
         evetrPrm => siteInfo%lc_evetr, &
         dectrPrm => siteInfo%lc_dectr, &
         grassPrm => siteInfo%lc_grass, &
         bsoilPrm => siteInfo%lc_bsoil, &
         waterPrm => siteInfo%lc_water, &
         sfr_surf => siteInfo%sfr_surf, &
         snowPrm => siteInfo%snow, &
         nlayer => siteInfo%nlayer, &
         PipeCapacity => siteInfo%PipeCapacity, &
         RunoffToWater => siteInfo%RunoffToWater, &
         FlowChange => siteInfo%FlowChange, &
         PervFraction => siteInfo%PervFraction, &
         vegfraction => siteInfo%vegfraction, &
         avRh => forcing%RH, &
         Press_hPa => forcing%Pres, &
         Temp_C => forcing%Temp_C, &
         precip => forcing%rain, &
         imin => timer%imin, &
         it => timer%it, &
         dectime => timer%dectime, &
         tstep => timer%tstep, &
         dayofWeek_id => timer%dayofWeek_id, &
         nsh_real => timer%nsh_real, &
         atmState => modState%atmState, &
         heatState => modState%heatState, &
         hydroState => modState%hydroState, &
         snowState => modState%snowState, &
         phenState => modState%phenState &
         )
         ! save the previous state
         hydroState_prev = hydroState
         snowState_prev = snowState

         ! initialize the next state
         hydroState_next = hydroState
         snowState_next = snowState

         ASSOCIATE ( &
            avdens => atmState%avdens, &
            avcp => atmState%avcp, &
            lv_J_kg => atmState%lv_J_kg, &
            lvS_J_kg => atmState%lvS_J_kg, &
            psyc_hPa => atmState%psyc_hPa, &
            sIce_hPa => atmState%sIce_hPa, &
            vpd_hPa => atmState%vpd_hPa, &
            s_hPa => atmState%s_hPa, &
            RS => atmState%RS, &
            RA_h => atmState%RA_h, &
            RB => atmState%RB, &
            rss_surf => atmState%rss_surf, &
            RAsnow => snowState%RAsnow, &
            qn_ind_snow => snowState%qn_ind_snow, &
            kup_ind_snow => snowState%kup_ind_snow, &
            deltaQi => snowState%deltaQi, &
            Tsurf_ind_snow => snowState%Tsurf_ind_snow, &
            SnowRemoval => snowState%SnowRemoval, &
            NWstate_per_tstep => hydroState%NWstate_per_tstep, &
            swe => snowState%swe, &
            chSnow_per_interval => snowState%chSnow_per_interval, &
            mwstore => snowState%mwstore, &
            Tsurf_ind => heatState%Tsurf_ind, &
            qn_snowfree => heatState%qn_snowfree, &
            qf => heatState%qf, &
            qs => heatState%qs, &
            qn_surf => heatState%qn_surf, &
            qs_surf => heatState%qs_surf, &
            qe => heatState%qe, &
            qe_surf => heatState%qe_surf, &
            qe_roof => heatState%qe_roof, &
            qe_wall => heatState%qe_wall, &
            addimpervious => hydroState%addimpervious, &
            addVeg => hydroState%addVeg, &
            drain => hydroState%drain_surf, &
            AddWater => hydroState%AddWater, &
            frac_water2runoff => hydroState%frac_water2runoff, &
            state_per_tstep => hydroState%state_per_tstep, &
            ev_per_tstep => hydroState%ev_per_tstep, &
            runoff_per_tstep => hydroState%runoff_per_tstep, &
            surf_chang_per_tstep => hydroState%surf_chang_per_tstep, &
            runoffAGveg => hydroState%runoffAGveg, &
            runoffAGimpervious => hydroState%runoffAGimpervious, &
            runoffPipes => hydroState%runoffPipes, &
            runoffwaterbody => hydroState%runoffwaterbody, &
            state_id_in => hydroState_prev%state_surf, &
            soilstore_id_in => hydroState_prev%soilstore_surf, &
            StoreDrainPrm => phenState%StoreDrainPrm, &
            SnowPack_in => snowState_prev%SnowPack, &
            SnowFrac_in => snowState_prev%snowFrac, &
            SnowWater_in => snowState_prev%SnowWater, &
            iceFrac_in => snowState_prev%IceFrac, &
            SnowDens_in => snowState_prev%SnowDens, &
            SnowfallCum_in => snowState_prev%SnowfallCum, &
            SnowAlb_in => snowState_next%SnowAlb, &
            EvapMethod => config%EvapMethod, &
            Diagnose => config%Diagnose &
            )

            ! Diagnose = config%Diagnose
            ASSOCIATE ( &
               WetThresh_surf => [pavedPrm%wetthresh, bldgPrm%wetthresh, evetrPrm%wetthresh, dectrPrm%wetthresh, &
                                  grassPrm%wetthresh, bsoilPrm%wetthresh, waterPrm%wetthresh], &
               SoilStoreCap => [pavedPrm%soil%soilstorecap, bldgPrm%soil%soilstorecap, &
                                evetrPrm%soil%soilstorecap, dectrPrm%soil%soilstorecap, &
                                grassPrm%soil%soilstorecap, bsoilPrm%soil%soilstorecap, waterPrm%soil%soilstorecap], &
               tau_r => snowPrm%tau_r, &
               CRWmin => snowPrm%CRWmin, &
               CRWmax => snowPrm%CRWmax, &
               SnowAlbMax => snowPrm%SnowAlbMax, &
               PrecipLimit => snowPrm%PrecipLimit, &
               PrecipLimitAlb => snowPrm%PrecipLimitAlb, &
               SnowDensMax => snowPrm%SnowDensMax, &
               SnowDensMin => snowPrm%snowdensmin, &
               RadMeltFact => snowPrm%RadMeltFact, &
               TempMeltFact => snowPrm%TempMeltFact, &
               SnowLimPaved => snowPrm%SnowLimPaved, &
               SnowLimBldg => snowPrm%SnowLimBldg, &
               SnowPackLimit => snowPrm%SnowPackLimit, &
               SnowProf_24hr_working => snowPrm%snowprof_24hr_working, &
               SnowProf_24hr_holiday => snowPrm%snowprof_24hr_holiday &
               )

               ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
               SnowProf_24hr(:, 1) = SnowProf_24hr_working
               SnowProf_24hr(:, 2) = SnowProf_24hr_holiday

               ! runoff_per_interval = runoff_per_interval_in
               state_id_surf = state_id_in
               soilstore_id = soilstore_id_in

               ! tstep_real = tstep*1.D0
               ! nsh_real = 3600/tstep*1.D0

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
               chSnow_per_interval = 0
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
                        vpd_hPa, qn_e, s_hPa, RS, RA_h, RB, tlv, SnowDensMin, SnowProf_24hr, precip, &
                        PipeCapacity, RunoffToWater, &
                        addVeg, SnowLimPaved, SnowLimBldg, FlowChange, drain, &
                        WetThresh_surf, state_id_in, mw_ind, SoilStoreCap, rainonsnow, &
                        freezmelt, freezstate, freezstatevol, &
                        Qm_Melt, Qm_rain, Tsurf_ind, sfr_surf, dayofWeek_id, StoreDrainPrm, SnowPackLimit, &
                        AddWater, frac_water2runoff, &
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
                  chSnow_per_interval = chSnow_per_interval + chSnow_tot

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

               hydroState_next%state_surf = state_id_surf
               hydroState_next%soilstore_surf = soilstore_id

               snowState_next%SnowWater = SnowWater
               snowState_next%iceFrac = iceFrac

               snowState_next%SnowAlb = SnowAlb
               snowState_next%SnowDens = SnowDens
               snowState_next%SnowPack = SnowPack
               snowState_next%SnowFrac = SnowFrac
               snowState_next%SnowfallCum = SnowfallCum

               ! pack output into one line
               dataOutLineSnow = [ &
                                 snowState_next%SnowPack(1:nsurf), mw_ind(1:nsurf), Qm_melt(1:nsurf), & !26
                                 Qm_rain(1:nsurf), Qm_freezState(1:nsurf), snowState_next%SnowFrac(1:(nsurf - 1)), & !46
                                 rainOnSnow(1:nsurf), & !53
                                 qn_ind_snow(1:nsurf), kup_ind_snow(1:nsurf), freezMelt(1:nsurf), & !74
                                 SnowWater(1:nsurf), snowState_next%SnowDens(1:nsurf), & !88
                                 snowDepth(1:nsurf), Tsurf_ind_snow(1:nsurf), &
                                 snowState_next%SnowAlb]

            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_cal_snow

!================latent heat flux and surface wetness===================
! TODO: optimise the structure of this function

   SUBROUTINE SUEWS_cal_QE( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_TIMER, SUEWS_FORCING, &
                               SUEWS_SITE, EHC_PRM, &
                               LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, &
                               PHENOLOGY_STATE, HYDRO_STATE, atm_state, &
                               SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState

      TYPE(HYDRO_STATE) :: hydroState_in

      INTEGER :: nlayer !number of vertical levels in urban canopy [-]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_surf !runoff from each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: rss_roof ! redefined surface resistance for wet roof [s m-1]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: runoff_roof !runoff from roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: rss_wall ! redefined surface resistance for wet wall [s m-1]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: runoff_wall !runoff from wall [mm]

      ! local:
      INTEGER :: is

      ! REAL(KIND(1D0)) :: runoff_per_interval
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id_out
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id !Soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_e_surf !net available energy for evaporation for each surface[W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qn_e_roof !net available energy for evaporation for roof[W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qn_e_wall !net available energy for evaporation for wall[W m-2]

      REAL(KIND(1D0)) :: pin !Rain per time interval
      REAL(KIND(1D0)) :: tlv !Latent heat of vapourisation per timestep [J kg-1 s-1]
      REAL(KIND(1D0)) :: state_building !aggregated surface water of building facets [mm]
      REAL(KIND(1D0)) :: soilstore_building !aggregated soilstore of building facets[mm]
      REAL(KIND(1D0)) :: capStore_builing ! aggregated storage capacity of building facets[mm]
      REAL(KIND(1D0)) :: runoff_building !aggregated Runoff of building facets [mm]
      REAL(KIND(1D0)) :: qe_building !aggregated qe of building facets[W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: drain
      REAL(KIND(1D0)), DIMENSION(7) :: capStore_surf ! current storage capacity [mm]

      ! CALL hydroState_next%allocHydro(nlayer)
      ! ALLOCATE (hydroState%soilstore_roof(nlayer))
      ! ALLOCATE (hydroState%state_roof(nlayer))
      ! ALLOCATE (hydroState%soilstore_wall(nlayer))
      ! ALLOCATE (hydroState%state_wall(nlayer))

      ! load dim constants
      nlayer = siteInfo%nlayer

      ALLOCATE (rss_roof(nlayer))
      ALLOCATE (runoff_roof(nlayer))
      ALLOCATE (rss_wall(nlayer))
      ALLOCATE (runoff_wall(nlayer))
      ALLOCATE (qn_e_roof(nlayer))
      ALLOCATE (qn_e_wall(nlayer))
      ASSOCIATE ( &
         atmState => modState%atmState, &
         heatState => modState%heatState, &
         hydroState => modState%hydroState, &
         snowstate => modState%snowState, &
         phenState => modState%phenState &
         )

         ! save initial model states
         hydroState_in = hydroState

         ASSOCIATE ( &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            evetrPrm => siteInfo%lc_evetr, &
            dectrPrm => siteInfo%lc_dectr, &
            grassPrm => siteInfo%lc_grass, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            ehcPrm => siteInfo%ehc, &
            sfr_surf => siteInfo%sfr_surf, &
            sfr_roof => siteInfo%sfr_roof, &
            sfr_wall => siteInfo%sfr_wall, &
            snowPrm => siteInfo%snow, &
            PipeCapacity => siteInfo%PipeCapacity, &
            RunoffToWater => siteInfo%RunoffToWater, &
            FlowChange => siteInfo%FlowChange, &
            PervFraction => siteInfo%PervFraction, &
            vegfraction => siteInfo%vegfraction, &
            NonWaterFraction => siteInfo%NonWaterFraction, &
            SurfaceArea => siteInfo%SurfaceArea, &
            avRh => forcing%RH, &
            Press_hPa => forcing%Pres, &
            Temp_C => forcing%Temp_C, &
            precip => forcing%rain, &
            xsmd => forcing%xsmd, &
            imin => timer%imin, &
            it => timer%it, &
            dectime => timer%dectime, &
            tstep => timer%tstep, &
            tstep_real => timer%tstep_real, &
            dayofWeek_id => timer%dayofWeek_id, &
            nsh_real => timer%nsh_real, &
            avdens => atmState%avdens, &
            avcp => atmState%avcp, &
            lv_J_kg => atmState%lv_J_kg, &
            psyc_hPa => atmState%psyc_hPa, &
            vpd_hPa => atmState%vpd_hPa, &
            s_hPa => atmState%s_hPa, &
            RS => atmState%RS, &
            RA_h => atmState%RA_h, &
            RB => atmState%RB, &
            rss_surf => atmState%rss_surf, &
            qf => heatState%qf, &
            qe => heatState%qe, &
            QN_surf => heatState%QN_surf, &
            QN_roof => heatState%QN_roof, &
            QN_wall => heatState%QN_wall, &
            QS_surf => heatState%QS_surf, &
            QS_roof => heatState%QS_roof, &
            QS_wall => heatState%QS_wall, &
            qe_surf => heatState%qe_surf, &
            qe_roof => heatState%qe_roof, &
            qe_wall => heatState%qe_wall, &
            qe0_surf => heatState%qe0_surf, &
            WU_surf => hydroState%WU_surf, &
            addVeg => hydroState%addVeg, &
            addWaterBody => hydroState%addWaterBody, &
            AddWater_surf => hydroState%AddWater, &
            drain_surf => hydroState%drain_surf, &
            frac_water2runoff_surf => hydroState%frac_water2runoff, &
            ev_surf => hydroState%ev_surf, &
            ev_roof => hydroState%ev_roof, &
            state_roof => hydroState%state_roof, &
            soilstore_roof => hydroState%soilstore_roof, &
            ev_wall => hydroState%ev_wall, &
            state_wall => hydroState%state_wall, &
            soilstore_wall => hydroState%soilstore_wall, &
            ev0_surf => hydroState%ev0_surf, &
            state_per_tstep => hydroState%state_per_tstep, &
            NWstate_per_tstep => hydroState%NWstate_per_tstep, &
            ev_per_tstep => hydroState%ev_per_tstep, &
            runoff_per_tstep => hydroState%runoff_per_tstep, &
            surf_chang_per_tstep => hydroState%surf_chang_per_tstep, &
            runoffPipes => hydroState%runoffPipes, &
            runoffwaterbody => hydroState%runoffwaterbody, &
            runoffAGveg => hydroState%runoffAGveg, &
            runoffAGimpervious => hydroState%runoffAGimpervious, &
            storageheatmethod => config%storageheatmethod, &
            addimpervious => hydroState%addimpervious, &
            state_surf_in => hydroState_in%state_surf, &
            soilstore_surf_in => hydroState_in%soilstore_surf, &
            state_roof_in => hydroState_in%state_roof, &
            soilstore_roof_in => hydroState_in%soilstore_roof, &
            state_wall_in => hydroState_in%state_wall, &
            soilstore_wall_in => hydroState_in%soilstore_wall, &
            state_surf => hydroState%state_surf, &
            soilstore_surf => hydroState%soilstore_surf, &
            runoffSoil_surf => hydroState%runoffSoil, &
            runoffSoil_per_tstep => hydroState%runoffSoil_per_tstep, &
            SoilMoistCap => hydroState%SoilMoistCap, &
            smd_surf => hydroState%smd_surf, &
            smd => hydroState%smd, &
            tot_chang_per_tstep => hydroState%tot_chang_per_tstep, &
            SoilState => hydroState%SoilState, &
            StoreDrainPrm => phenState%StoreDrainPrm, &
            snowfrac_in => snowstate%SnowFrac, &
            SMDMethod => config%SMDMethod, &
            EvapMethod => config%EvapMethod, &
            Diagnose => config%Diagnose &
            )

            ASSOCIATE ( &
               StateLimit_roof => ehcPrm%state_limit_roof, &
               SoilStoreCap_roof => ehcPrm%soil_storecap_roof, &
               WetThresh_roof => ehcPrm%wet_thresh_roof, &
               StateLimit_wall => ehcPrm%state_limit_wall, &
               SoilStoreCap_wall => ehcPrm%soil_storecap_wall, &
               WetThresh_wall => ehcPrm%wet_thresh_wall, &
               StateLimit_surf => [pavedPrm%statelimit, bldgPrm%statelimit, evetrPrm%statelimit, &
                                   dectrPrm%statelimit, grassPrm%statelimit, bsoilPrm%statelimit, waterPrm%statelimit], &
               SoilStoreCap_surf => [pavedPrm%soil%soilstorecap, bldgPrm%soil%soilstorecap, &
                                     evetrPrm%soil%soilstorecap, dectrPrm%soil%soilstorecap, &
                                     grassPrm%soil%soilstorecap, bsoilPrm%soil%soilstorecap, waterPrm%soil%soilstorecap], &
               SoilDepth_surf => [ &
               pavedPrm%soil%soildepth, bldgPrm%soil%soildepth, evetrPrm%soil%soildepth, &
               dectrPrm%soil%soildepth, &
               grassPrm%soil%soildepth, bsoilPrm%soil%soildepth, waterPrm%soil%soildepth], &
               SatHydraulicConduct_surf => [pavedPrm%soil%sathydraulicconduct, bldgPrm%soil%sathydraulicconduct, &
                                            evetrPrm%soil%sathydraulicconduct, dectrPrm%soil%sathydraulicconduct, &
                                            grassPrm%soil%sathydraulicconduct, bsoilPrm%soil%sathydraulicconduct, &
                                            waterPrm%soil%sathydraulicconduct], &
               WetThresh_surf => [pavedPrm%wetthresh, bldgPrm%wetthresh, evetrPrm%wetthresh, &
                                  dectrPrm%wetthresh, grassPrm%wetthresh, bsoilPrm%wetthresh, waterPrm%wetthresh] &
               )

               ! StoreDrainPrm = phenState_next%StoreDrainPrm

               ! state_surf_in = hydroState_prev%state_surf
               ! soilstore_surf_in = hydroState_prev%soilstore_surf
               ! state_roof_in = hydroState_prev%state_roof
               ! soilstore_roof_in = hydroState_prev%soilstore_roof
               ! state_wall_in = hydroState_prev%state_wall
               ! soilstore_wall_in = hydroState_prev%soilstore_wall

               ! runoff_per_interval = runoff_per_interval_in
               state_surf = state_surf_in
               ! soilstore_surf = soilstore_surf_in
               ! soilstore_id = soilstore_surf_in

               ! nsh_real = 3600/tstep*1.D0

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

               ! MP: Use EHC
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
                     !                      ev_roof,
                     state_roof, soilstore_roof, runoff_roof, & ! general output:
                     !                      ev_wall,
                     state_wall, soilstore_wall, runoff_wall, & ! general output:
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
                  pin, nsh_real, SnowFrac_in, &
                  PipeCapacity, RunoffToWater, & ! input:
                  addImpervious, addVeg, addWaterBody, FlowChange, &
                  SoilStoreCap_surf, StateLimit_surf, &
                  PervFraction, &
                  sfr_surf, drain_surf, AddWater_surf, frac_water2runoff_surf, WU_surf, &
                  ev0_surf, state_surf_in, soilstore_surf_in, &
                  runoffAGimpervious, runoffAGveg, runoffPipes, runoffwaterbody, & ! output:
                  ev_surf, state_surf, soilstore_surf, & ! output:
                  runoff_surf)

               ! update QE based on the water balance
               qe_surf = tlv*ev_surf

               ! --- update building related ---
               IF (storageheatmethod == 5) THEN
                  ! update building specific values
                  qe_surf(BldgSurf) = qe_building
                  state_surf(BldgSurf) = state_building
                  soilstore_surf(BldgSurf) = soilstore_building/capStore_builing*capStore_surf(BldgSurf)
                  runoff_surf(BldgSurf) = runoff_building
               END IF

               ! aggregate all surface water fluxes/amounts
               qe = DOT_PRODUCT(qe_surf, sfr_surf)

               ! Sum change from different surfaces to find total change to surface state_id
               surf_chang_per_tstep = DOT_PRODUCT(state_surf - state_surf_in, sfr_surf)

               ! Sum evaporation from different surfaces to find total evaporation [mm]
               ev_per_tstep = DOT_PRODUCT(ev_surf, sfr_surf)

               ! Sum runoff from different surfaces to find total runoff
               runoff_per_tstep = DOT_PRODUCT(runoff_surf, sfr_surf)

               ! Calculate total state_id (including water body)
               state_per_tstep = DOT_PRODUCT(state_surf, sfr_surf)

               IF (NonWaterFraction /= 0) THEN
                  NWstate_per_tstep = DOT_PRODUCT(state_surf(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction
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
               IF (Diagnose == 1) PRINT *, 'in SUEWS_cal_QE soilstore_id = ', soilstore_surf

               !=== Horizontal movement between soil stores ===
               CALL SUEWS_cal_HorizontalSoilWater( &
                  sfr_surf, & ! input: ! surface fractions
                  SoilStoreCap_surf, & !Capacity of soil store for each surface [mm]
                  SoilDepth_surf, & !Depth of sub-surface soil store for each surface [mm]
                  SatHydraulicConduct_surf, & !Saturated hydraulic conductivity for each soil subsurface [mm s-1]
                  SurfaceArea, & !Surface area of the study area [m2]
                  NonWaterFraction, & ! sum of surface cover fractions for all except water surfaces
                  tstep_real, & !tstep cast as a real for use in calculations
                  soilstore_surf, & ! inout: !Soil moisture of each surface type [mm]
                  runoffSoil_surf, & !Soil runoff from each soil sub-surface [mm]
                  runoffSoil_per_tstep & !  output:!Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
                  )

               !========== Calculate soil moisture of a whole grid ============
               CALL SUEWS_cal_SoilState( &
                  SMDMethod, xsmd, NonWaterFraction, SoilMoistCap, & !input
                  SoilStoreCap_surf, surf_chang_per_tstep, &
                  soilstore_surf, soilstore_surf_in, sfr_surf, &
                  smd, smd_surf, tot_chang_per_tstep, SoilState) !output

            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_cal_QE
!========================================================================

!===============sensible heat flux======================================

   SUBROUTINE SUEWS_cal_QH( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:
      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_TIMER, SUEWS_SITE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, HEAT_STATE, &
                               SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      INTEGER, PARAMETER :: qhMethod = 1 ! 1 = the redidual method; 2 = the resistance method

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER :: is

      ASSOCIATE ( &
         snowState => modState%snowState, &
         heatState => modState%heatState, &
         atmState => modState%atmState &
         )

         ASSOCIATE ( &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            evetrPrm => siteInfo%lc_evetr, &
            dectrPrm => siteInfo%lc_dectr, &
            grassPrm => siteInfo%lc_grass, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            ehcPrm => siteInfo%ehc, &
            nlayer => siteInfo%nlayer, &
            sfr_surf => siteInfo%sfr_surf, &
            sfr_roof => siteInfo%sfr_roof, &
            sfr_wall => siteInfo%sfr_wall, &
            SurfaceArea => siteInfo%SurfaceArea, &
            snowPrm => siteInfo%snow, &
            PipeCapacity => siteInfo%PipeCapacity, &
            RunoffToWater => siteInfo%RunoffToWater, &
            FlowChange => siteInfo%FlowChange, &
            PervFraction => siteInfo%PervFraction, &
            vegfraction => siteInfo%vegfraction, &
            NonWaterFraction => siteInfo%NonWaterFraction, &
            tstep_real => timer%tstep_real, &
            tsfc_surf => heatState%tsfc_surf, &
            tsfc_roof => heatState%tsfc_roof, &
            tsfc_wall => heatState%tsfc_wall, &
            qn => heatState%qn, &
            qf => heatState%qf, &
            qe => heatState%qe, &
            qs => heatState%qs, &
            QmRain => snowState%QmRain, &
            QmFreez => snowState%QmFreez, &
            Qm => snowState%Qm, &
            xsmd => forcing%xsmd, &
            Temp_C => forcing%Temp_C, &
            RA_h => atmState%RA_h, &
            avdens => atmState%avdens, &
            avcp => atmState%avcp, &
            qh_resist_surf => heatState%qh_resist_surf, &
            qh_resist_roof => heatState%qh_resist_roof, &
            qh_resist_wall => heatState%qh_resist_wall, &
            qh => heatState%qh, &
            qh_init => heatState%qh_init, &
            qh_resist => heatState%qh_resist, &
            qh_residual => heatState%qh_residual, &
            qh_surf => heatState%qh_surf, &
            qh_roof => heatState%qh_roof, &
            qh_wall => heatState%qh_wall, &
            qe_surf => heatState%qe_surf, &
            qe_roof => heatState%qe_roof, &
            qe_wall => heatState%qe_wall, &
            qn_surf => heatState%qn_surf, &
            qn_roof => heatState%qn_roof, &
            qn_wall => heatState%qn_wall, &
            qs_surf => heatState%qs_surf, &
            qs_roof => heatState%qs_roof, &
            qs_wall => heatState%qs_wall, &
            SMDMethod => config%SMDMethod, &
            storageheatmethod => config%StorageHeatMethod, &
            Diagnose => config%Diagnose &
            )
            ! tsfc_surf = heatState_out%tsfc_surf
            ! tsfc_roof = heatState_out%tsfc_roof
            ! tsfc_wall = heatState_out%tsfc_wall

            ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
            ! Calculate sensible heat flux as a residual (Modified by LJ in Nov 2012)
            ! choose output QH
            SELECT CASE (QHMethod)
            CASE (1)
               qh_residual = (qn + qf + QmRain) - (qe + qs + Qm + QmFreez) !qh=(qn1+qf+QmRain+QmFreez)-(qeOut+qs+Qm)
               ! Testing: qh_resist here is a dummy test - difference in QH per cycle
!                   IF ((QH / QE) < 1) THEN
!                         qh_resist = qh - qh_residual
!                         qs = qs - qh_resist
!                         ! Dumping energy into QS
!                         qh_residual = (qn + qf + QmRain) - (qe + qs + Qm + QmFreez)
!                   END IF
               qh = qh_residual
            CASE (2)
               ! ! Calculate QH using resistance method (for testing HCW 06 Jul 2016)
               ! Aerodynamic-Resistance-based method
               DO is = 1, nsurf
                  IF (RA_h /= 0) THEN
                     qh_resist_surf(is) = avdens*avcp*(tsfc_surf(is) - Temp_C)/RA_h
                  ELSE
                     qh_resist_surf(is) = NAN
                  END IF
               END DO
               IF (storageheatmethod == 5) THEN
                  DO is = 1, nlayer
                     IF (RA_h /= 0) THEN
                        qh_resist_roof(is) = avdens*avcp*(tsfc_roof(is) - Temp_C)/RA_h
                        qh_resist_wall(is) = avdens*avcp*(tsfc_wall(is) - Temp_C)/RA_h
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

               ! MP: TODO - Check if this works correctly
               qh_resist = DOT_PRODUCT(qh_resist_surf, sfr_surf)

               qh = qh_resist
            END SELECT

            ! update QH of all facets
            QH_surf = QN_surf + qf - qs_surf - qe_surf
            QH_roof = QN_roof + qf - qs_roof - qe_roof
            QH_wall = QN_wall + qf - qs_wall - qe_wall
            !END SELECT
            qh_init = qh

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_QH
!========================================================================

!===============Resistance Calculations=======================

   SUBROUTINE SUEWS_cal_Resistance( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_TIMER, CONDUCTANCE_PRM, &
                               SUEWS_FORCING, &
                               LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, &
                               PHENOLOGY_STATE, SNOW_STATE, SUEWS_FORCING, SUEWS_SITE, &
                               SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INout) :: modState

      INTEGER, PARAMETER :: AerodynamicResistanceMethod = 2 !method to calculate RA [-]

      REAL(KIND(1D0)) :: gfunc !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)) :: Tair ! air temperature [degC]

      ASSOCIATE ( &
         phenState => modState%phenState, &
         snowState => modState%snowState, &
         atmState => modState%atmState, &
         roughnessState => modState%roughnessState, &
         hydroState => modState%hydroState, &
         heatState => modState%heatState &
         )

         ASSOCIATE ( &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            evetrPrm => siteInfo%lc_evetr, &
            dectrPrm => siteInfo%lc_dectr, &
            grassPrm => siteInfo%lc_grass, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            sfr_surf => siteInfo%sfr_surf, &
            conductancePrm => siteInfo%conductance, &
            NonWaterFraction => siteInfo%NonWaterFraction, &
            VegFraction => siteInfo%VegFraction, &
            nsh_real => timer%nsh_real, &
            id => timer%id, &
            it => timer%it, &
            avU1 => forcing%U, &
            Temp_C => forcing%Temp_C, &
            avkdn => forcing%kdown, &
            xsmd => forcing%xsmd, &
            vsmd => hydroState%vsmd, &
            avdens => atmState%avdens, &
            avcp => atmState%avcp, &
            dq => atmState%dq, &
            TStar => atmState%TStar, &
            UStar => atmState%UStar, &
            zL => atmState%zL, &
            RS => atmState%RS, &
            RA => atmState%RA_h, &
            L_mod => atmState%L_mod, &
            RB => atmState%RB, &
            T2_C => atmState%T2_C, &
            T_hbh_C => atmState%T_hbh_C, &
            QH_init => heatState%QH_init, &
            z0v => roughnessState%z0v, &
            zzd => roughnessState%zzd, &
            z0m => roughnessState%z0m, &
            zdm => roughnessState%zdm, &
            g_kdown => phenState%g_kdown, &
            g_dq => phenState%g_dq, &
            g_ta => phenState%g_ta, &
            g_smd => phenState%g_smd, &
            g_lai => phenState%g_lai, &
            gsc => phenState%gsc, &
            LAI_id => phenState%LAI_id, &
            RASnow => snowState%RASnow, &
            z0vSnow => snowState%z0vSnow, &
            SnowFrac => snowState%SnowFrac, &
            Diagnose => config%Diagnose, &
            StabilityMethod => config%StabilityMethod, &
            RoughLenHeatMethod => config%RoughLenHeatMethod, &
            localClimateMethod => config%localClimateMethod, &
            SnowUse => config%SnowUse, &
            SMDMethod => config%SMDMethod &
            )
            ASSOCIATE ( &
               LAIMax => [evetrPrm%lai%laimax, dectrPrm%lai%laimax, grassPrm%lai%laimax], &
               MaxConductance => [evetrPrm%maxconductance, dectrPrm%maxconductance, grassPrm%maxconductance], &
               gsModel => conductancePrm%gsModel, &
               Kmax => conductancePrm%Kmax, &
               G_max => conductancePrm%g_max, &
               G_k => conductancePrm%g_k, &
               G_q_base => conductancePrm%g_q_base, &
               G_q_shape => conductancePrm%g_q_shape, &
               G_t => conductancePrm%g_t, &
               G_sm => conductancePrm%g_sm, &
               S1 => conductancePrm%s1, &
               S2 => conductancePrm%s2, &
               TH => conductancePrm%th, &
               TL => conductanceprm%tl &
               )

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
               Tair = MERGE(T_hbh_C, MERGE(T2_C, Temp_C, localClimateMethod == 1), localClimateMethod == 2)
               CALL SurfaceResistance( &
                  id, it, & ! input:
                  SMDMethod, SnowFrac, sfr_surf, avkdn, Tair, dq, xsmd, vsmd, MaxConductance, &
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
            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_Resistance
!========================================================================

!==============Update output arrays=========================
   SUBROUTINE SUEWS_update_outputLine( &
      timer, config, forcing, siteInfo, & ! input
      modState, & ! input/output:
      datetimeLine, dataOutLineSUEWS) !output

      USE module_ctrl_type, ONLY: SUEWS_SITE, SUEWS_TIMER, SUEWS_CONFIG, SUEWS_FORCING, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               HYDRO_STATE, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      REAL(KIND(1D0)), PARAMETER :: NAN = -999

      REAL(KIND(1D0)), DIMENSION(5), INTENT(OUT) :: datetimeLine !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5), INTENT(out) :: dataOutLineSUEWS
      REAL(KIND(1D0)) :: LAI_wt !area weighted LAI [m2 m-2]
      REAL(KIND(1D0)) :: RH2_pct ! RH2 in percentage [-]

      ! the variables below with '_x' endings stand for 'exported' values
      REAL(KIND(1D0)) :: ResistSurf_x !output surface resistance [s m-1]
      REAL(KIND(1D0)) :: surf_chang_per_tstep_x !output change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)) :: l_mod_x !output  Obukhov length [m]
      REAL(KIND(1D0)) :: bulkalbedo !output area-weighted albedo [-]
      REAL(KIND(1D0)) :: smd_surf_x(nsurf) !output soil moisture deficit for each surface [mm]
      REAL(KIND(1D0)) :: state_x(nsurf) !output wetness status of each surfaces[mm]
      REAL(KIND(1D0)) :: wu_DecTr !water use for deciduous tree and shrubs [mm]
      REAL(KIND(1D0)) :: wu_EveTr !water use of evergreen tree and shrubs [mm]
      REAL(KIND(1D0)) :: wu_Grass !water use for grass [mm]

      !=====================================================================
      !====================== Prepare data for output ======================
      ! values outside of reasonable range are set as NAN-like numbers. TS 10 Jun 2018
      ASSOCIATE ( &
         phenState => modState%phenState, &
         snowState => modState%snowState, &
         atmState => modState%atmState, &
         roughnessState => modState%roughnessState, &
         hydroState => modState%hydroState, &
         solarState => modState%solarState, &
         anthroemisState => modState%anthroemisState, &
         heatState => modState%heatState &
         )
         ASSOCIATE ( &
            alb => phenState%alb, &
            LAI_id => phenState%LAI_id, &
            FlowChange => siteInfo%FlowChange, &
            sfr_surf => siteInfo%sfr_surf, &
            id => timer%id, &
            imin => timer%imin, &
            it => timer%it, &
            iy => timer%iy, &
            AdditionalWater => hydroState%AdditionalWater, &
            avU10_ms => atmState%U10_ms, &
            azimuth => solarState%azimuth_deg, &
            SnowAlb => snowState%SnowAlb, &
            chSnow_per_interval => snowState%chSnow_per_interval, &
            dectime => timer%dectime, &
            drain_per_tstep => hydroState%drain_per_tstep, &
            QE_LUMPS => heatState%QE_LUMPS, &
            ev_per_tstep => hydroState%ev_per_tstep, &
            wu_ext => hydroState%wu_ext, &
            fcld => forcing%fcld, &
            Fc => anthroemisState%Fc, &
            Fc_build => anthroemisState%Fc_build, &
            Fc_metab => anthroemisState%Fc_metab, &
            Fc_photo => anthroemisState%Fc_photo, &
            Fc_respi => anthroemisState%Fc_respi, &
            Fc_point => anthroemisState%Fc_point, &
            Fc_traff => anthroemisState%Fc_traff, &
            QH_LUMPS => heatState%QH_LUMPS, &
            wu_int => hydroState%wu_int, &
            kup => heatState%kup, &
            ldown => heatState%ldown, &
            l_mod => atmState%l_mod, &
            lup => heatState%lup, &
            mwh => snowState%mwh, &
            MwStore => snowState%MwStore, &
            nsh_real => timer%nsh_real, &
            NWstate_per_tstep => hydroState%NWstate_per_tstep, &
            q2_gkg => atmState%q2_gkg, &
            qe => heatState%qe, &
            qf => heatState%qf, &
            qh => heatState%qh, &
            QH_init => heatState%QH_init, &
            qh_resist => heatState%qh_resist, &
            Qm => snowState%Qm, &
            QmFreez => snowState%QmFreez, &
            QmRain => snowState%QmRain, &
            qn => heatState%qn, &
            qn_snow => snowState%qn_snow, &
            qn_snowfree => heatState%qn_snowfree, &
            qs => heatState%qs, &
            RA => atmState%RA_h, &
            RS => atmState%RS, &
            RH2 => atmState%RH2, &
            runoffAGimpervious => hydroState%runoffAGimpervious, &
            runoffAGveg => hydroState%runoffAGveg, &
            runoff_per_tstep => hydroState%runoff_per_tstep, &
            runoffPipes => hydroState%runoffPipes, &
            runoffSoil_per_tstep => hydroState%runoffSoil_per_tstep, &
            runoffWaterBody => hydroState%runoffWaterBody, &
            smd => hydroState%smd, &
            smd_surf => hydroState%smd_surf, &
            SnowRemoval => snowState%SnowRemoval, &
            state_per_tstep => hydroState%state_per_tstep, &
            surf_chang_per_tstep => hydroState%surf_chang_per_tstep, &
            swe => snowState%swe, &
            t2_C => atmState%t2_C, &
            tsfc_C => heatState%tsfc_C, &
            tot_chang_per_tstep => hydroState%tot_chang_per_tstep, &
            tsurf => heatState%tsurf, &
            Tsfc_surf => heatState%Tsfc_surf, &
            Tsfc_surf_dyohm => heatState%Tsfc_surf_dyohm, &
            UStar => atmState%UStar, &
            TStar => atmState%TStar, &
            wu_surf => hydroState%wu_surf, &
            z0m => roughnessState%z0m, &
            zdm => roughnessState%zdm, &
            zL => atmState%zL, &
            zenith_deg => solarState%zenith_deg, &
            kdown => forcing%kdown, &
            rain => forcing%rain, &
            state_surf => hydroState%state_surf &
            )

            !! Remove non-existing surface type from surface and soil outputs   ! Added back in with NANs by HCW 24 Aug 2016
            !state_x = UNPACK(SPREAD(NAN, dim=1, ncopies=SIZE(sfr_surf)), mask=(sfr_surf < 0.00001), field=state_surf)
            state_x = state_surf
            !smd_surf_x = UNPACK(SPREAD(NAN, dim=1, ncopies=SIZE(sfr_surf)), mask=(sfr_surf < 0.00001), field=smd_surf)
            smd_surf_x = smd_surf

            ResistSurf_x = MIN(9999., RS)

            surf_chang_per_tstep_x = MERGE(surf_chang_per_tstep, 0.D0, ABS(surf_chang_per_tstep) > 1E-6)

            l_mod_x = MAX(MIN(9999., l_mod), -9999.)

            LAI_wt = DOT_PRODUCT(LAI_id(:), sfr_surf(1 + 2:nvegsurf + 2)/SUM(sfr_surf(1 + 2:nvegsurf + 2)))

            ! Calculate areally-weighted albedo
            bulkalbedo = DOT_PRODUCT(alb, sfr_surf)

            ! convert RH2 to a percentage form
            RH2_pct = atmState%RH2*100.0

            ! translate water use to vegetated surfaces
            wu_DecTr = hydroState%wu_surf(3)
            wu_EveTr = hydroState%wu_surf(4)
            wu_Grass = hydroState%wu_surf(5)

            !====================== update output line ==============================
            ! date & time:
            datetimeLine = [ &
                           REAL(iy, KIND(1D0)), REAL(id, KIND(1D0)), &
                           REAL(it, KIND(1D0)), REAL(imin, KIND(1D0)), timer%dectime]
            !Define the overall output matrix to be printed out step by step
            dataOutLineSUEWS = [ &
                               kdown, kup, ldown, lup, tsurf, &
                               qn, qf, qs, qh, qe, &
                               QH_LUMPS, QE_LUMPS, QH_init, qh_resist, &
                               rain, wu_ext, ev_per_tstep, runoff_per_tstep, tot_chang_per_tstep, &
                               surf_chang_per_tstep_x, state_per_tstep, NWstate_per_tstep, drain_per_tstep, smd, &
                               FlowChange/nsh_real, AdditionalWater, &
                               runoffSoil_per_tstep, runoffPipes, runoffAGimpervious, runoffAGveg, runoffWaterBody, &
                               wu_int, wu_EveTr, wu_DecTr, wu_Grass, &
                               smd_surf_x(1:nsurf - 1), &
                               state_x(1:nsurf), &
                               zenith_deg, azimuth, bulkalbedo, Fcld, &
                               LAI_wt, z0m, zdm, zL, &
                               UStar, TStar, l_mod, RA, RS, &
                               Fc, &
                               Fc_photo, Fc_respi, Fc_metab, Fc_traff, Fc_build, Fc_point, &
                               qn_snowfree, qn_snow, SnowAlb, &
                               Qm, QmFreez, QmRain, swe, mwh, MwStore, chSnow_per_interval, &
                               SnowRemoval(1:2), &
                               tsfc_C, t2_C, q2_gkg, avU10_ms, RH2_pct, Tsfc_surf, Tsfc_surf_dyohm & ! surface-level diagonostics
                               ]
            ! set invalid values to NAN
            ! dataOutLineSUEWS = set_nan(dataOutLineSUEWS)

            !====================update output line end==============================
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_update_outputLine
!========================================================================

!==============Update output arrays=========================
   SUBROUTINE EHC_update_outputLine( &
      timer, & !input
      modState, & ! input/output:
      datetimeLine, dataOutLineEHC) !output

      USE module_ctrl_type, ONLY: SUEWS_TIMER, HEAT_STATE, HYDRO_STATE, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      ! TYPE(HEAT_STATE), INTENT(IN) :: heatState
      ! TYPE(HYDRO_STATE), INTENT(IN) :: hydroState

      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER, PARAMETER :: n_fill = 15

      REAL(KIND(1D0)), DIMENSION(5), INTENT(OUT) :: datetimeLine !date & time
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutEHC - 5), INTENT(out) :: dataOutLineEHC

      ASSOCIATE ( &
         heatState => modState%heatState, &
         hydroState => modState%hydroState &
         )
         ASSOCIATE ( &
            iy => timer%iy, &
            id => timer%id, &
            it => timer%it, &
            imin => timer%imin, &
            dectime => timer%dectime, &
            tsfc_out_surf => heatState%tsfc_surf, &
            qs_surf => heatState%qs_surf, &
            tsfc_out_roof => heatState%tsfc_roof, &
            Qn_roof => heatState%Qn_roof, &
            QS_roof => heatState%QS_roof, &
            QE_roof => heatState%QE_roof, &
            QH_roof => heatState%QH_roof, &
            state_roof => hydroState%state_roof, &
            soilstore_roof => hydroState%soilstore_roof, &
            tsfc_out_wall => heatState%tsfc_wall, &
            Qn_wall => heatState%Qn_wall, &
            QS_wall => heatState%QS_wall, &
            QE_wall => heatState%QE_wall, &
            QH_wall => heatState%QH_wall, &
            state_wall => hydroState%state_wall, &
            soilstore_wall => hydroState%soilstore_wall &
            &)

            ! date & time:
            datetimeLine = [ &
                           REAL(iy, KIND(1D0)), REAL(id, KIND(1D0)), &
                           REAL(it, KIND(1D0)), REAL(imin, KIND(1D0)), dectime]

            !Define the overall output matrix to be printed out step by step
            dataOutLineEHC = [ &
                             tsfc_out_surf, qs_surf, & !output
                             fill_sim_res(tsfc_out_roof, n_fill), &
                             fill_sim_res(Qn_roof, n_fill), &
                             fill_sim_res(QS_roof, n_fill), &
                             fill_sim_res(QE_roof, n_fill), &
                             fill_sim_res(QH_roof, n_fill), &
                             fill_sim_res(state_roof, n_fill), &
                             fill_sim_res(soilstore_roof, n_fill), &
                             fill_sim_res(tsfc_out_wall, n_fill), &
                             fill_sim_res(Qn_wall, n_fill), &
                             fill_sim_res(QS_wall, n_fill), &
                             fill_sim_res(QE_wall, n_fill), &
                             fill_sim_res(QH_wall, n_fill), &
                             fill_sim_res(state_wall, n_fill), &
                             fill_sim_res(soilstore_wall, n_fill) &
                             ]

            ! set invalid values to NAN
            ! dataOutLineSUEWS = set_nan(dataOutLineSUEWS)

            !====================update output line end==============================
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE EHC_update_outputLine
!========================================================================

   FUNCTION fill_sim_res(res_valid, n_fill) RESULT(res_filled)
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: res_valid
      INTEGER, INTENT(IN) :: n_fill
      REAL(KIND(1D0)), DIMENSION(n_fill) :: res_filled

      REAL(KIND(1D0)), PARAMETER :: NAN = -999

      res_filled = RESHAPE(res_valid, [n_fill], pad=[NAN])
   END FUNCTION fill_sim_res

!==============Update output arrays=========================
   SUBROUTINE SUEWS_update_output( &
      SnowUse, storageheatmethod, & !input
      ReadLinesMetdata, NumberOfGrids, &
      ir, gridiv, &
      dataOutLineSUEWS, dataOutLineSnow, dataOutLineESTM, dataoutLineRSL, dataOutLineBEERS, &
      dataoutlineDebug, dataoutlineSPARTACUS, dataOutLineEHC, &
      dataOutLineSTEBBS, &
      dataOutLineNHood, & !input
      dataOutSUEWS, dataOutSnow, dataOutESTM, dataOutRSL, dataOutBEERS, dataOutDebug, dataOutSPARTACUS, &
      dataOutEHC, &
      dataOutSTEBBS, &
      dataOutNHood &
      ) !inout
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
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutEHC), INTENT(in) :: dataOutLineEHC
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow), INTENT(in) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL), INTENT(in) :: dataoutLineRSL
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutBEERS), INTENT(in) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutDebug), INTENT(in) :: dataOutLineDebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutSPARTACUS), INTENT(in) :: dataOutLineSPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSTEBBS), INTENT(in) :: dataOutLineSTEBBS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutNHood), INTENT(in) :: dataOutLineNHood

      REAL(KIND(1D0)), INTENT(inout) :: dataOutSUEWS(ReadLinesMetdata, ncolumnsDataOutSUEWS, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutSnow(ReadLinesMetdata, ncolumnsDataOutSnow, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutESTM(ReadLinesMetdata, ncolumnsDataOutESTM, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutEHC(ReadLinesMetdata, ncolumnsDataOutEHC, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutRSL(ReadLinesMetdata, ncolumnsDataOutRSL, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutBEERS(ReadLinesMetdata, ncolumnsdataOutBEERS, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutDebug(ReadLinesMetdata, ncolumnsDataOutDebug, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutSPARTACUS(ReadLinesMetdata, ncolumnsDataOutSPARTACUS, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutSTEBBS(ReadLinesMetdata, ncolumnsDataOutSTEBBS, NumberOfGrids)
      REAL(KIND(1D0)), INTENT(inout) :: dataOutNHood(ReadLinesMetdata, ncolumnsDataOutNHood, NumberOfGrids)

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
      dataOutSTEBBS(ir, 1:ncolumnsDataOutSTEBBS, Gridiv) = [(dataOutLineSTEBBS)]

      IF (SnowUse == 1) THEN
         dataOutSnow(ir, 1:ncolumnsDataOutSnow, Gridiv) = [set_nan(dataOutLineSnow)]
      END IF

      IF (storageheatmethod == 4) THEN
         dataOutESTM(ir, 1:ncolumnsDataOutESTM, Gridiv) = [set_nan(dataOutLineESTM)]
      END IF

      IF (storageheatmethod == 5) THEN
         dataOutEHC(ir, 1:ncolumnsDataOutEHC, Gridiv) = [set_nan(dataOutLineEHC)]
      END IF

      dataOutNHood(ir, 1:ncolumnsDataOutNHood, Gridiv) = [set_nan(dataOutLineNHood)]
      !====================update output arrays end==============================

   END SUBROUTINE SUEWS_update_output

! calculate several surface fraction related parameters
   SUBROUTINE SUEWS_cal_surf( &
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
         IF (nlayer > 1) sfr_roof(1:nlayer - 1) = &
            MAX( &
            building_frac(1:nlayer - 1) - building_frac(2:nlayer), &
            0.01) ! minimum value for sfr_roof to avoid zero fractions when adjacent layers have the same building fraction
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
      n_buildings, h_std, &
      flag_test, &
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
      LAIPower, LAIType, lat, lng, localClimateMethod, MaxConductance, MaxFCMetab, MaxQFMetab, &
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
      ground_albedo_dir_mult_fact, use_sw_direct_albedo, &
      lambda_c, & !input
      stebbsmethod, rcmethod, & ! stebbs building input
      buildingname, buildingtype, &
      BuildingCount, Occupants, &
      ! hhs0, age_0_4, age_5_11, age_12_18, age_19_64, age_65plus, ! NOT USED
      stebbs_Height, &
      FootprintArea, WallExternalArea, RatioInternalVolume, WWR, WallThickness, WallEffectiveConductivity, &
      WallDensity, WallCp, WallextThickness, WallextEffectiveConductivity, WallextDensity, WallextCp, Wallx1, WallExternalEmissivity, WallInternalEmissivity, WallTransmissivity, &
      WallAbsorbtivity, WallReflectivity, &
      RoofThickness, RoofEffectiveConductivity, RoofDensity, RoofCp, RoofextThickness, RoofextEffectiveConductivity, RoofextDensity, RoofextCp,&
      Roofx1, RoofExternalEmissivity, RoofInternalEmissivity, RoofTransmissivity, &
      RoofAbsorbtivity, RoofReflectivity, &
      FloorThickness, GroundFloorEffectiveConductivity, &
      GroundFloorDensity, GroundFloorCp, WindowThickness, WindowEffectiveConductivity, &
      WindowDensity, WindowCp, WindowExternalEmissivity, WindowInternalEmissivity, WindowTransmissivity, &
      WindowAbsorbtivity, WindowReflectivity, InternalMassDensity, InternalMassCp, InternalMassEmissivity, &
      MaxHeatingPower, WaterTankWaterVolume, MaximumHotWaterHeatingPower, HeatingSetpointTemperature, &
      CoolingSetpointTemperature, &
      WallInternalConvectionCoefficient, RoofInternalConvectionCoefficient, InternalMassConvectionCoefficient, & ! stebbs general input
      FloorInternalConvectionCoefficient, WindowInternalConvectionCoefficient, &
      WallExternalConvectionCoefficient, RoofExternalConvectionCoefficient, WindowExternalConvectionCoefficient, &
      GroundDepth, ExternalGroundConductivity, IndoorAirDensity, IndoorAirCp, &
      !WallBuildingViewFactor, WallGroundViewFactor, WallSkyViewFactor, &
      !RoofBuildingViewFactor, RoofGroundViewFactor, RoofSkyViewFactor, &
      MetabolicRate, LatentSensibleRatio, ApplianceRating, &
      TotalNumberofAppliances, ApplianceUsageFactor, HeatingSystemEfficiency, &
      MaxCoolingPower, CoolingSystemCOP, VentilationRate, OutdoorAirAnnualTemperature, OutdoorAirStartTemperature, IndoorAirStartTemperature, &
      IndoorMassStartTemperature, WallIndoorSurfaceTemperature, RoofIndoorSurfaceTemperature, &
      WallOutdoorSurfaceTemperature, RoofOutdoorSurfaceTemperature, WindowIndoorSurfaceTemperature, &
      WindowOutdoorSurfaceTemperature, GroundFloorIndoorSurfaceTemperature, &
      GroundFloorOutdoorSurfaceTemperature, WaterTankTemperature, &
      InternalWallWaterTankTemperature, ExternalWallWaterTankTemperature, &
      WaterTankWallThickness, MainsWaterTemperature, WaterTankSurfaceArea, &
      HotWaterHeatingSetpointTemperature, HotWaterTankWallEmissivity, &
      DomesticHotWaterTemperatureInUseInBuilding, InternalWallDHWVesselTemperature, &
      ExternalWallDHWVesselTemperature, DHWVesselWallThickness, DHWWaterVolume, &
      DHWSurfaceArea, DHWVesselEmissivity, HotWaterFlowRate, DHWDrainFlowRate, &
      DHWSpecificHeatCapacity, HotWaterTankSpecificHeatCapacity, DHWVesselSpecificHeatCapacity, &
      DHWDensity, HotWaterTankWallDensity, DHWVesselDensity, HotWaterTankBuildingWallViewFactor, &
      HotWaterTankInternalMassViewFactor, HotWaterTankWallConductivity, HotWaterTankInternalWallConvectionCoefficient, &
      HotWaterTankExternalWallConvectionCoefficient, DHWVesselWallConductivity, DHWVesselInternalWallConvectionCoefficient, &
      DHWVesselExternalWallConvectionCoefficient, DHWVesselWallEmissivity, HotWaterHeatingEfficiency, &
      MinimumVolumeOfDHWinUse, &
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
      output_block_suews, state_debug, block_mod_state) !output

      IMPLICIT NONE
      LOGICAL, INTENT(IN) :: flag_test

      ! ############# DTS variables (start) #############
      ! ---anthropogenic heat-related variables
      ! ---siteInfo-related variables
      TYPE(SUEWS_SITE) :: siteInfo
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
      REAL(KIND(1D0)), INTENT(IN) :: n_buildings !Number of buildings [-]
      REAL(KIND(1D0)), INTENT(IN) :: h_std ! Standard deviation of buildins [m]
      REAL(KIND(1D0)), INTENT(IN) :: lambda_c ! Building surface to plan area ratio [-]

      ! ---forcing-related variables
      TYPE(SUEWS_FORCING) :: forcing

      ! ESTM related:
      REAL(KIND(1D0)), INTENT(INOUT) :: Tair_av !average air temperature [degC]

      ! ---timer-related variables
      TYPE(SUEWS_TIMER) :: timer

      INTEGER, INTENT(IN) :: tstep !timestep [s]
      INTEGER, INTENT(IN) :: tstep_prev ! tstep size of the previous step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]

      ! ---method-related variables
      TYPE(SUEWS_CONFIG) :: config
      INTEGER, INTENT(INOUT) :: Diagnose ! flag for printing diagnostic info during runtime [N/A]C
      INTEGER, INTENT(in) :: DiagMethod !Defines how near surface diagnostics are calculated
      INTEGER, INTENT(IN) :: EmissionsMethod !method to calculate anthropogenic heat [-]
      INTEGER, INTENT(IN) :: RoughLenHeatMethod ! method to calculate heat roughness length [-]
      INTEGER, INTENT(IN) :: RoughLenMomMethod ! Determines how aerodynamic roughness length (z0m) and zero displacement height (zdm) are calculated [-]
      INTEGER, INTENT(IN) :: FAIMethod !Determines how FAI is calculated [-]
      INTEGER, INTENT(IN) :: SMDMethod ! Determines method for calculating soil moisture deficit [-]
      INTEGER, INTENT(IN) :: WaterUseMethod !Defines how external water use is calculated[-]
      INTEGER, INTENT(IN) :: localClimateMethod !
      INTEGER, INTENT(IN) :: NetRadiationMethod ! method for calculation of radiation fluxes [-]
      INTEGER, INTENT(IN) :: StabilityMethod !method to calculate atmospheric stability [-]
      INTEGER, INTENT(IN) :: StorageHeatMethod
      INTEGER, INTENT(IN) :: SnowUse ! Determines whether the snow part of the model runs[-]
      LOGICAL, INTENT(IN) :: use_sw_direct_albedo !boolean, Specify ground and roof albedos separately for direct solar radiation [-]
      INTEGER, INTENT(IN) :: OHMIncQF ! Determines whether the storage heat flux calculation uses Q* or ( Q* +QF) [-]
      ! INTEGER, INTENT(IN) :: nbtype ! number of building types [-] STEBBS
      INTEGER, INTENT(IN) :: stebbsmethod ! method to calculate building energy use [-] STEBBS
      INTEGER, INTENT(IN) :: rcmethod ! method to split building envelope heat capacity in STEBBS [-] STEBBS

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
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_MIN ! [x] minimum QF values [W m-2]
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(IN) :: AHProf_24hr ! [x] Hourly profile values used in energy use calculation [-]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Cooling ! [x] cooling slope for the anthropogenic heat flux calculation [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: AH_SLOPE_Heating ! [x] heating slope for the anthropogenic heat flux calculation [W m-2 K-1]
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
      TYPE(anthroEmis_STATE) :: anthroEmisState
      TYPE(atm_STATE) :: atmState
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating Degree Days [degC d]

      ! ---water balance related states
      TYPE(HYDRO_STATE) :: hydroState
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_roof !Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_roof !wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: soilstore_wall !Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: state_wall !wetness status of wall [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: soilstore_surf !soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF), INTENT(INOUT) :: state_surf !wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: smd_surf !soil moisture deficit for each surface
      REAL(KIND(1D0)), DIMENSION(9), INTENT(INOUT) :: WUDay_id !Daily water use for EveTr, DecTr, Grass [mm]

      ! ---heat storage related states
      TYPE(HEAT_STATE) :: heatState
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(INOUT) :: temp_roof !interface temperature between depth layers in roof [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_roof !roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(INOUT) :: temp_wall !interface temperature between depth layers in wall [degC]
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(INOUT) :: tsfc_wall !wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(INOUT) :: tsfc_surf !surface temperature [degC]
      !REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: tsfc_surf_dyohm !surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(INOUT) :: temp_surf !interface temperature between depth layers [degC]
      !REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(OUT) :: temp_surf_dyohm !interface temperature between depth layers [degC]
      ! ---OHM related states
      TYPE(OHM_STATE) :: ohmState
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_av ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: dqndt ! rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(INOUT) :: qn_s_av ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)), INTENT(INOUT) :: dqnsdt ! Rate of change of net radiation [W m-2 h-1]

      ! Used in OHM calcs - need to be reset between supy calls - TODO: Should allow passing initial values
      REAL(KIND(1D0)) :: t2_prev ! previous day midnight air temperature [degC]
      REAL(KIND(1D0)) :: ws_rav ! running average of wind speed [m s-1]
      REAL(KIND(1D0)) :: tair_prev
      REAL(KIND(1D0)) :: qn_rav ! running average of net radiation [W m-2]
      REAL(KIND(1D0)) :: a1_bldg ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a2_bldg ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a3_bldg ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a1_paved ! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a2_paved ! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a3_paved ! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a1_evetr ! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a2_evetr ! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a3_evetr ! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a1_dectr ! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a2_dectr ! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a3_dectr ! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a1_grass ! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a2_grass ! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a3_grass ! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a1_bsoil ! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a2_bsoil ! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a3_bsoil ! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a1_water ! Dynamic OHM coefficients of water
      REAL(KIND(1D0)) :: a2_water ! Dynamic OHM coefficients of water
      REAL(KIND(1D0)) :: a3_water ! Dynamic OHM coefficients of water    
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

      ! ---stebbs related states
      TYPE(STEBBS_STATE) :: stebbsState
      REAL(KIND(1D0)) :: WallInternalConvectionCoefficient
      REAL(KIND(1D0)) :: RoofInternalConvectionCoefficient
      REAL(KIND(1D0)) :: InternalMassConvectionCoefficient
      REAL(KIND(1D0)) :: FloorInternalConvectionCoefficient
      REAL(KIND(1D0)) :: WindowInternalConvectionCoefficient
      REAL(KIND(1D0)) :: WallExternalConvectionCoefficient
      REAL(KIND(1D0)) :: RoofExternalConvectionCoefficient
      REAL(KIND(1D0)) :: WindowExternalConvectionCoefficient
      REAL(KIND(1D0)) :: GroundDepth
      REAL(KIND(1D0)) :: ExternalGroundConductivity
      REAL(KIND(1D0)) :: IndoorAirDensity
      REAL(KIND(1D0)) :: IndoorAirCp
      !REAL(KIND(1D0)) :: WallBuildingViewFactor
      !REAL(KIND(1D0)) :: WallGroundViewFactor
      !REAL(KIND(1D0)) :: WallSkyViewFactor
      !REAL(KIND(1D0)) :: RoofBuildingViewFactor
      !REAL(KIND(1D0)) :: RoofGroundViewFactor
      !REAL(KIND(1D0)) :: RoofSkyViewFactor
      REAL(KIND(1D0)) :: MetabolicRate
      REAL(KIND(1D0)) :: LatentSensibleRatio
      REAL(KIND(1D0)) :: ApplianceRating
      REAL(KIND(1D0)) :: TotalNumberofAppliances
      REAL(KIND(1D0)) :: ApplianceUsageFactor
      REAL(KIND(1D0)) :: HeatingSystemEfficiency
      REAL(KIND(1D0)) :: MaxCoolingPower
      REAL(KIND(1D0)) :: CoolingSystemCOP
      REAL(KIND(1D0)) :: VentilationRate
      REAL(KIND(1D0)) :: OutdoorAirAnnualTemperature
      REAL(KIND(1D0)) :: OutdoorAirStartTemperature
      REAL(KIND(1D0)) :: IndoorAirStartTemperature
      REAL(KIND(1D0)) :: IndoorMassStartTemperature
      REAL(KIND(1D0)) :: WallIndoorSurfaceTemperature
      REAL(KIND(1D0)) :: WallOutdoorSurfaceTemperature
      REAL(KIND(1D0)) :: RoofIndoorSurfaceTemperature
      REAL(KIND(1D0)) :: RoofOutdoorSurfaceTemperature
      REAL(KIND(1D0)) :: WindowIndoorSurfaceTemperature
      REAL(KIND(1D0)) :: WindowOutdoorSurfaceTemperature
      REAL(KIND(1D0)) :: GroundFloorIndoorSurfaceTemperature
      REAL(KIND(1D0)) :: GroundFloorOutdoorSurfaceTemperature
      REAL(KIND(1D0)) :: WaterTankTemperature
      REAL(KIND(1D0)) :: InternalWallWaterTankTemperature
      REAL(KIND(1D0)) :: ExternalWallWaterTankTemperature
      REAL(KIND(1D0)) :: WaterTankWallThickness
      REAL(KIND(1D0)) :: MainsWaterTemperature
      REAL(KIND(1D0)) :: WaterTankSurfaceArea
      REAL(KIND(1D0)) :: HotWaterHeatingSetpointTemperature
      REAL(KIND(1D0)) :: HotWaterTankWallEmissivity
      REAL(KIND(1D0)) :: DomesticHotWaterTemperatureInUseInBuilding
      REAL(KIND(1D0)) :: InternalWallDHWVesselTemperature
      REAL(KIND(1D0)) :: ExternalWallDHWVesselTemperature
      REAL(KIND(1D0)) :: DHWVesselWallThickness
      REAL(KIND(1D0)) :: DHWWaterVolume
      REAL(KIND(1D0)) :: DHWSurfaceArea
      REAL(KIND(1D0)) :: DHWVesselEmissivity
      REAL(KIND(1D0)) :: HotWaterFlowRate
      REAL(KIND(1D0)) :: DHWDrainFlowRate
      REAL(KIND(1D0)) :: DHWSpecificHeatCapacity
      REAL(KIND(1D0)) :: HotWaterTankSpecificHeatCapacity
      REAL(KIND(1D0)) :: DHWVesselSpecificHeatCapacity
      REAL(KIND(1D0)) :: DHWDensity
      REAL(KIND(1D0)) :: HotWaterTankWallDensity
      REAL(KIND(1D0)) :: DHWVesselDensity
      REAL(KIND(1D0)) :: HotWaterTankBuildingWallViewFactor
      REAL(KIND(1D0)) :: HotWaterTankInternalMassViewFactor
      REAL(KIND(1D0)) :: HotWaterTankWallConductivity
      REAL(KIND(1D0)) :: HotWaterTankInternalWallConvectionCoefficient
      REAL(KIND(1D0)) :: HotWaterTankExternalWallConvectionCoefficient
      REAL(KIND(1D0)) :: DHWVesselWallConductivity
      REAL(KIND(1D0)) :: DHWVesselInternalWallConvectionCoefficient
      REAL(KIND(1D0)) :: DHWVesselExternalWallConvectionCoefficient
      REAL(KIND(1D0)) :: DHWVesselWallEmissivity
      REAL(KIND(1D0)) :: HotWaterHeatingEfficiency
      REAL(KIND(1D0)) :: MinimumVolumeOfDHWinUse
      !REAL(KIND(1D0)), DIMENSION(nlayer) :: Textroof_C !roof surface temperature from STEBBS[degC]
      !REAL(KIND(1D0)), DIMENSION(nlayer) :: Textwall_C !wall surface temperature from STEBBS[degC]

      ! ---stebbs building related states
      TYPE(BUILDING_ARCHETYPE_PRM) :: building_archtype
      CHARACTER(LEN=10) :: buildingname
      CHARACTER(LEN=10) :: buildingtype
      REAL(KIND(1D0)) :: BuildingCount
      REAL(KIND(1D0)) :: Occupants
      ! REAL(KIND(1D0)) :: hhs0
      ! REAL(KIND(1D0)) :: age_0_4
      ! REAL(KIND(1D0)) :: age_5_11
      ! REAL(KIND(1D0)) :: age_12_18
      ! REAL(KIND(1D0)) :: age_19_64
      ! REAL(KIND(1D0)) :: age_65plus
      REAL(KIND(1D0)) :: stebbs_Height
      REAL(KIND(1D0)) :: Ta_annual
      REAL(KIND(1D0)) :: FootprintArea
      REAL(KIND(1D0)) :: WallExternalArea
      REAL(KIND(1D0)) :: RatioInternalVolume
      REAL(KIND(1D0)) :: WWR
      REAL(KIND(1D0)) :: WallThickness
      REAL(KIND(1D0)) :: WallEffectiveConductivity
      REAL(KIND(1D0)) :: WallDensity
      REAL(KIND(1D0)) :: WallCp
      REAL(KIND(1D0)) :: WallextThickness
      REAL(KIND(1D0)) :: WallextEffectiveConductivity
      REAL(KIND(1D0)) :: WallextDensity
      REAL(KIND(1D0)) :: WallextCp
      REAL(KIND(1D0)) :: Wallx1
      REAL(KIND(1D0)) :: WallExternalEmissivity
      REAL(KIND(1D0)) :: WallInternalEmissivity
      REAL(KIND(1D0)) :: WallTransmissivity
      REAL(KIND(1D0)) :: WallAbsorbtivity
      REAL(KIND(1D0)) :: WallReflectivity
      REAL(KIND(1D0)) :: RoofThickness
      REAL(KIND(1D0)) :: RoofEffectiveConductivity
      REAL(KIND(1D0)) :: RoofDensity
      REAL(KIND(1D0)) :: RoofCp
      REAL(KIND(1D0)) :: RoofextThickness
      REAL(KIND(1D0)) :: RoofextEffectiveConductivity
      REAL(KIND(1D0)) :: RoofextDensity
      REAL(KIND(1D0)) :: RoofextCp
      REAL(KIND(1D0)) :: Roofx1
      REAL(KIND(1D0)) :: RoofExternalEmissivity
      REAL(KIND(1D0)) :: RoofInternalEmissivity
      REAL(KIND(1D0)) :: RoofTransmissivity
      REAL(KIND(1D0)) :: RoofAbsorbtivity
      REAL(KIND(1D0)) :: RoofReflectivity
      REAL(KIND(1D0)) :: FloorThickness
      REAL(KIND(1D0)) :: GroundFloorEffectiveConductivity
      REAL(KIND(1D0)) :: GroundFloorDensity
      REAL(KIND(1D0)) :: GroundFloorCp
      REAL(KIND(1D0)) :: WindowThickness
      REAL(KIND(1D0)) :: WindowEffectiveConductivity
      REAL(KIND(1D0)) :: WindowDensity
      REAL(KIND(1D0)) :: WindowCp
      REAL(KIND(1D0)) :: WindowExternalEmissivity
      REAL(KIND(1D0)) :: WindowInternalEmissivity
      REAL(KIND(1D0)) :: WindowTransmissivity
      REAL(KIND(1D0)) :: WindowAbsorbtivity
      REAL(KIND(1D0)) :: WindowReflectivity
      REAL(KIND(1D0)) :: InternalMassDensity
      REAL(KIND(1D0)) :: InternalMassCp
      REAL(KIND(1D0)) :: InternalMassEmissivity
      REAL(KIND(1D0)) :: MaxHeatingPower
      REAL(KIND(1D0)) :: WaterTankWaterVolume
      REAL(KIND(1D0)) :: MaximumHotWaterHeatingPower
      REAL(KIND(1D0)) :: HeatingSetpointTemperature
      REAL(KIND(1D0)) :: CoolingSetpointTemperature

      TYPE(STEBBS_PRM) :: stebbsPrm

      ! lumped states
      TYPE(SUEWS_DEBUG), INTENT(OUT), OPTIONAL :: state_debug
      TYPE(SUEWS_STATE_BLOCK), INTENT(OUT), OPTIONAL :: block_mod_state
      TYPE(SUEWS_STATE) :: mod_state
      ! ############# DTS variables (end) #############

      ! input:
      ! met forcing block
      REAL(KIND(1D0)), DIMENSION(len_sim, 24), INTENT(IN) :: MetForcingBlock
      INTEGER, INTENT(IN) :: len_sim
      ! input variables
      INTEGER, INTENT(IN) :: nlayer ! number of vertical layers in urban canyon
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: Ts5mindata_ir !surface temperature input data[degC]
      ! ########################################################################################

      ! ########################################################################################
      ! inout variables
      ! ########################################################################################
      ! output variables
      TYPE(output_line) :: output_line_suews
      ! REAL(KIND(1D0)),DIMENSION(:,:,:),ALLOCATABLE,INTENT(OUT) ::datetimeBlock
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSUEWS) :: dataOutBlockSUEWS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSnow) :: dataOutBlockSnow
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTM) :: dataOutBlockESTM
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutEHC) :: dataOutBlockEHC
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutRSL) :: dataOutBlockRSL
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsdataOutBEERS) :: dataOutBlockBEERS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDebug) :: dataOutBlockDebug
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSPARTACUS) :: dataOutBlockSPARTACUS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDailyState) :: dataOutBlockDailyState
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSTEBBS) :: dataOutBlockSTEBBS
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutNHood) :: dataOutBlockNHood
      ! ########################################################################################

      ! internal temporal iteration related variables
      ! INTEGER::dt_since_start ! time since simulation starts [s]

      ! model output blocks of the same size as met forcing block

      ! local variables
      ! length of met forcing block
      INTEGER :: ir
      ! met forcing variables
      INTEGER, PARAMETER :: gridiv_x = 1 ! a dummy gridiv as this routine is only one grid

      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5) :: dataOutLineSUEWS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5) :: dataOutLineSnow
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutEHC - 5) :: dataOutLineEHC
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5) :: dataOutLineRSL
      ! REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutSOLWEIG - 5) :: dataOutLineSOLWEIG
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5) :: dataOutLineBEERS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug - 5) :: dataOutLinedebug
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5) :: dataOutLineSPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5) :: dataOutLineDailyState
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSTEBBS - 5) :: dataOutLineSTEBBS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutNHood - 5) :: dataOutLineNHood

      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSUEWS, 1) :: dataOutBlockSUEWS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSnow, 1) :: dataOutBlockSnow_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutESTM, 1) :: dataOutBlockESTM_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutEHC, 1) :: dataOutBlockEHC_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutRSL, 1) :: dataOutBlockRSL_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsdataOutBEERS, 1) :: dataOutBlockBEERS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDebug, 1) :: dataOutBlockDebug_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSPARTACUS, 1) :: dataOutBlockSPARTACUS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutDailyState, 1) :: dataOutBlockDailyState_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutSTEBBS, 1) :: dataOutBlockSTEBBS_X
      REAL(KIND(1D0)), DIMENSION(len_sim, ncolumnsDataOutNHood, 1) :: dataOutBlockNHood_X

      ! REAL(KIND(1D0)), DIMENSION(10) :: MetForcingData_grid ! fake array as a placeholder

      TYPE(output_block), INTENT(OUT) :: output_block_suews

      IF (flag_test .AND. PRESENT(block_mod_state)) THEN

         CALL block_mod_state%init(nlayer, ndepth, len_sim)
      END IF

      ! ############# evaluation for DTS variables (start) #############
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
      siteInfo%sfr_surf = sfr_surf
      siteInfo%nlayer = nlayer
      siteInfo%n_buildings = n_buildings
      siteInfo%h_std = h_std
      siteInfo%lambda_c = lambda_c
      ! siteInfo%nlayer = nlayer

      ! forcing%kdown = kdown
      ! forcing%ldown = ldown_obs
      !forcing%RH = avRh
      ! forcing%pres = Press_hPa
      !forcing%U = avU1
      ! forcing%rain = Precip
      ! forcing%Wuh = wu_m3
      ! forcing%fcld = fcld_obs
      ! forcing%LAI_obs = LAI_obs
      ! forcing%snowfrac = snowFrac_obs
      ! forcing%xsmd = xsmd
      !forcing%qn1_obs = qn1_obs
      !forcing%qs_obs = qs_obs
      !forcing%qf_obs = qf_obs
      ! forcing%Tair_av_5d = Tair_av
      ! forcing%temp_c = Temp_C

      ! timer%id = id
      ! timer%imin = imin
      ! timer%isec = isec
      ! timer%it = it
      ! timer%iy = iy
      timer%tstep = tstep
      timer%tstep_prev = tstep_prev
      timer%dt_since_start = dt_since_start

      ! For dynamic OHM
      timer%new_day = 0
      timer%dt_since_start_prev = 0

      config%Diagnose = Diagnose
      config%DiagMethod = DiagMethod
      config%EmissionsMethod = EmissionsMethod
      config%RoughLenHeatMethod = RoughLenHeatMethod
      config%RoughLenMomMethod = RoughLenMomMethod
      config%FAIMethod = FAIMethod
      config%SMDMethod = SMDMethod
      config%WaterUseMethod = WaterUseMethod
      config%NetRadiationMethod = NetRadiationMethod
      config%StabilityMethod = StabilityMethod
      config%StorageHeatMethod = StorageHeatMethod
      config%SnowUse = SnowUse
      config%use_sw_direct_albedo = use_sw_direct_albedo
      config%ohmIncQF = OHMIncQF
      config%localClimateMethod = localClimateMethod
      ! these options are fixed
      config%DiagQS = 0
      config%EvapMethod = 2
      config%LAImethod = 1
      config%stebbsmethod = stebbsmethod
      config%rcmethod = rcmethod

      ! testing flag
      config%flag_test = flag_test

      ! lumps parameters
      lumpsPrm%raincover = RAINCOVER
      lumpsPrm%rainmaxres = RainMaxRes
      lumpsPrm%drainrt = DRAINRT
      lumpsPrm%veg_type = veg_type

      ! ESTM_ehc
      CALL ehcPrm%ALLOCATE(nlayer, ndepth)
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

      CALL spartacusLayerPrm%ALLOCATE(nlayer)

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
      pavedPrm%ohm%ohm_coef_lc(1)%summer_dry = OHM_coef(PavSurf, 2, 1)
      pavedPrm%ohm%ohm_coef_lc(1)%winter_wet = OHM_coef(PavSurf, 3, 1)
      pavedPrm%ohm%ohm_coef_lc(1)%winter_dry = OHM_coef(PavSurf, 4, 1)

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

      bldgPrm%ohm%ohm_coef_lc(2)%summer_wet = OHM_coef(BldgSurf, 1, 2)
      bldgPrm%ohm%ohm_coef_lc(2)%summer_dry = OHM_coef(BldgSurf, 2, 2)
      bldgPrm%ohm%ohm_coef_lc(2)%winter_wet = OHM_coef(BldgSurf, 3, 2)
      bldgPrm%ohm%ohm_coef_lc(2)%winter_dry = OHM_coef(BldgSurf, 4, 2)

      bldgPrm%ohm%ohm_coef_lc(3)%summer_wet = OHM_coef(BldgSurf, 1, 3)
      bldgPrm%ohm%ohm_coef_lc(3)%summer_dry = OHM_coef(BldgSurf, 2, 3)
      bldgPrm%ohm%ohm_coef_lc(3)%winter_wet = OHM_coef(BldgSurf, 3, 3)
      bldgPrm%ohm%ohm_coef_lc(3)%winter_dry = OHM_coef(BldgSurf, 4, 3)

      bldgPrm%soil%soildepth = SoilDepth(BldgSurf)
      bldgPrm%soil%soilstorecap = SoilStoreCap_surf(BldgSurf)
      bldgPrm%soil%sathydraulicconduct = SatHydraulicConduct(BldgSurf)
      bldgPrm%statelimit = StateLimit_surf(BldgSurf)
      bldgPrm%irrfracbldgs = IrrFracBldgs
      bldgPrm%wetthresh = WetThresh_surf(BldgSurf)
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
      atmState%Tair_av = Tair_av
      anthroEmisState%HDD_id = HDD_id

      ! ESTM_ehc related:
      ! water balance related:
      CALL hydroState%ALLOCATE(nlayer)
      ! Remove non-existing surface type from surface and soil outputs   ! Added back in with NANs by HCW 24 Aug 2016
      state_surf = UNPACK(SPREAD(0.0D0, dim=1, ncopies=SIZE(sfr_surf)), mask=(sfr_surf < 0.00001), field=state_surf)
      smd_surf = UNPACK(SPREAD(0.0D0, dim=1, ncopies=SIZE(sfr_surf)), mask=(sfr_surf < 0.00001), field=smd_surf)
      hydroState%soilstore_roof = soilstore_roof
      hydroState%state_roof = state_roof
      hydroState%soilstore_wall = soilstore_wall
      hydroState%state_wall = state_wall
      hydroState%soilstore_surf = soilstore_surf
      hydroState%state_surf = state_surf
      hydroState%smd_surf = smd_surf
      hydroState%WUDay_id = WUDay_id

      CALL heatState%ALLOCATE(nsurf, nlayer, ndepth)
      heatState%temp_roof = temp_roof
      heatState%temp_wall = temp_wall
      heatState%temp_surf = temp_surf
      heatState%temp_surf_dyohm = MetForcingBlock(1, 12) !initialised temperature
      heatState%tsfc_roof = tsfc_roof
      heatState%tsfc_wall = tsfc_wall
      heatState%tsfc_surf = tsfc_surf
      heatState%tsfc_surf_dyohm = MetForcingBlock(1, 12) !initialised temperature

      ! OHM related:
      ohmState%qn_av = qn_av
      ohmState%dqndt = dqndt
      ohmState%qn_s_av = qn_s_av
      ohmState%dqnsdt = dqnsdt

      ohmState%t2_prev = 0.0 ! previous day midnight air temperature [degC]
      ohmState%ws_rav = 2.0 ! running average of wind speed [m s-1]
      ohmState%tair_prev = 0.0
      ohmState%qn_rav = 0.0 ! running average of net radiation [W m-2]
      ohmState%a1_bldg = 0.0 ! Dynamic OHM coefficients of buildings
      ohmState%a2_bldg = 0.0 ! Dynamic OHM coefficients of buildings
      ohmState%a3_bldg = 0.0 ! Dynamic OHM coefficients of buildings
      ohmState%a1_paved = 0.0 ! Dynamic OHM coefficients 
      ohmState%a2_paved = 0.0 ! Dynamic OHM coefficients 
      ohmState%a3_paved = 0.0 ! Dynamic OHM coefficients 
      ohmState%a1_evetr = 0.0 ! Dynamic OHM coefficients 
      ohmState%a2_evetr = 0.0 ! Dynamic OHM coefficients 
      ohmState%a3_evetr = 0.0 ! Dynamic OHM coefficients 
      ohmState%a1_dectr = 0.0 ! Dynamic OHM coefficients 
      ohmState%a2_dectr = 0.0 ! Dynamic OHM coefficients 
      ohmState%a3_dectr = 0.0 ! Dynamic OHM coefficients 
      ohmState%a1_grass = 0.0 ! Dynamic OHM coefficients 
      ohmState%a2_grass = 0.0 ! Dynamic OHM coefficients 
      ohmState%a3_grass = 0.0 ! Dynamic OHM coefficients 
      ohmState%a1_bsoil = 0.0 ! Dynamic OHM coefficients 
      ohmState%a2_bsoil = 0.0 ! Dynamic OHM coefficients 
      ohmState%a3_bsoil = 0.0 ! Dynamic OHM coefficients 
      ohmState%a1_water = 0.0 ! Dynamic OHM coefficients 
      ohmState%a2_water = 0.0 ! Dynamic OHM coefficients 
      ohmState%a3_water = 0.0 ! Dynamic OHM coefficients       

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

      ! assign stebbs values
      ! parameters - invariant during the simulation
      stebbsPrm%WallInternalConvectionCoefficient = WallInternalConvectionCoefficient
      stebbsPrm%RoofInternalConvectionCoefficient = RoofInternalConvectionCoefficient
      stebbsPrm%InternalMassConvectionCoefficient = InternalMassConvectionCoefficient
      stebbsPrm%FloorInternalConvectionCoefficient = FloorInternalConvectionCoefficient
      stebbsPrm%WindowInternalConvectionCoefficient = WindowInternalConvectionCoefficient
      stebbsPrm%WallExternalConvectionCoefficient = WallExternalConvectionCoefficient
      stebbsPrm%RoofExternalConvectionCoefficient = RoofExternalConvectionCoefficient
      stebbsPrm%WindowExternalConvectionCoefficient = WindowExternalConvectionCoefficient
      stebbsPrm%GroundDepth = GroundDepth
      stebbsPrm%ExternalGroundConductivity = ExternalGroundConductivity
      stebbsPrm%IndoorAirDensity = IndoorAirDensity
      stebbsPrm%IndoorAirCp = IndoorAirCp
      !stebbsPrm%WallBuildingViewFactor = WallBuildingViewFactor
      !stebbsPrm%WallGroundViewFactor = WallGroundViewFactor
      !stebbsPrm%WallSkyViewFactor = WallSkyViewFactor
      !stebbsPrm%RoofBuildingViewFactor = RoofBuildingViewFactor
      !stebbsPrm%RoofGroundViewFactor = RoofGroundViewFactor
      !stebbsPrm%RoofSkyViewFactor = RoofSkyViewFactor
      stebbsPrm%MetabolicRate = MetabolicRate
      stebbsPrm%LatentSensibleRatio = LatentSensibleRatio
      stebbsPrm%ApplianceRating = ApplianceRating
      stebbsPrm%TotalNumberofAppliances = TotalNumberofAppliances
      stebbsPrm%ApplianceUsageFactor = ApplianceUsageFactor
      stebbsPrm%HeatingSystemEfficiency = HeatingSystemEfficiency
      stebbsPrm%MaxCoolingPower = MaxCoolingPower
      stebbsPrm%CoolingSystemCOP = CoolingSystemCOP
      stebbsPrm%VentilationRate = VentilationRate
      stebbsPrm%WaterTankWallThickness = WaterTankWallThickness
      stebbsPrm%WaterTankSurfaceArea = WaterTankSurfaceArea
      stebbsPrm%HotWaterHeatingSetpointTemperature = HotWaterHeatingSetpointTemperature
      stebbsPrm%HotWaterTankWallEmissivity = HotWaterTankWallEmissivity
      stebbsPrm%DHWVesselWallThickness = DHWVesselWallThickness
      stebbsPrm%DHWWaterVolume = DHWWaterVolume
      stebbsPrm%DHWSurfaceArea = DHWSurfaceArea
      stebbsPrm%DHWVesselEmissivity = DHWVesselEmissivity
      stebbsPrm%HotWaterFlowRate = HotWaterFlowRate
      stebbsPrm%DHWDrainFlowRate = DHWDrainFlowRate
      stebbsPrm%DHWSpecificHeatCapacity = DHWSpecificHeatCapacity
      stebbsPrm%HotWaterTankSpecificHeatCapacity = HotWaterTankSpecificHeatCapacity
      stebbsPrm%DHWVesselSpecificHeatCapacity = DHWVesselSpecificHeatCapacity
      stebbsPrm%DHWDensity = DHWDensity
      stebbsPrm%HotWaterTankWallDensity = HotWaterTankWallDensity
      stebbsPrm%DHWVesselDensity = DHWVesselDensity
      stebbsPrm%HotWaterTankBuildingWallViewFactor = HotWaterTankBuildingWallViewFactor
      stebbsPrm%HotWaterTankInternalMassViewFactor = HotWaterTankInternalMassViewFactor
      stebbsPrm%HotWaterTankWallConductivity = HotWaterTankWallConductivity
      stebbsPrm%HotWaterTankInternalWallConvectionCoefficient = HotWaterTankInternalWallConvectionCoefficient
      stebbsPrm%HotWaterTankExternalWallConvectionCoefficient = HotWaterTankExternalWallConvectionCoefficient
      stebbsPrm%DHWVesselWallConductivity = DHWVesselWallConductivity
      stebbsPrm%DHWVesselInternalWallConvectionCoefficient = DHWVesselInternalWallConvectionCoefficient
      stebbsPrm%DHWVesselExternalWallConvectionCoefficient = DHWVesselExternalWallConvectionCoefficient
      stebbsPrm%DHWVesselWallEmissivity = DHWVesselWallEmissivity
      stebbsPrm%HotWaterHeatingEfficiency = HotWaterHeatingEfficiency
      stebbsPrm%MinimumVolumeOfDHWinUse = MinimumVolumeOfDHWinUse

      ! states - updated during the simulation
      ! TODO: STEBBS States act as parameters for building generation (move all but allocation?)
      CALL stebbsState%ALLOCATE(nbtypes, nlayer)
      stebbsState%OutdoorAirAnnualTemperature = OutdoorAirAnnualTemperature
      stebbsState%OutdoorAirStartTemperature = OutdoorAirStartTemperature
      stebbsState%IndoorAirStartTemperature = IndoorAirStartTemperature
      stebbsState%IndoorMassStartTemperature = IndoorMassStartTemperature
      stebbsState%WallIndoorSurfaceTemperature = WallIndoorSurfaceTemperature
      stebbsState%WallOutdoorSurfaceTemperature = WallOutdoorSurfaceTemperature
      stebbsState%RoofIndoorSurfaceTemperature = RoofIndoorSurfaceTemperature
      stebbsState%RoofOutdoorSurfaceTemperature = RoofOutdoorSurfaceTemperature
      stebbsState%WindowIndoorSurfaceTemperature = WindowIndoorSurfaceTemperature
      stebbsState%WindowOutdoorSurfaceTemperature = WindowOutdoorSurfaceTemperature
      stebbsState%GroundFloorIndoorSurfaceTemperature = GroundFloorIndoorSurfaceTemperature
      stebbsState%GroundFloorOutdoorSurfaceTemperature = GroundFloorOutdoorSurfaceTemperature
      stebbsState%WaterTankTemperature = WaterTankTemperature
      stebbsState%InternalWallWaterTankTemperature = InternalWallWaterTankTemperature
      stebbsState%ExternalWallWaterTankTemperature = ExternalWallWaterTankTemperature
      stebbsState%MainsWaterTemperature = MainsWaterTemperature
      stebbsState%DomesticHotWaterTemperatureInUseInBuilding = DomesticHotWaterTemperatureInUseInBuilding
      stebbsState%InternalWallDHWVesselTemperature = InternalWallDHWVesselTemperature
      stebbsState%ExternalWallDHWVesselTemperature = ExternalWallDHWVesselTemperature
      !stebbsState%Textroof_C(:) =  RoofOutdoorSurfaceTemperature
      !stebbsState%Textwall_C(:) =  WallOutdoorSurfaceTemperature

      ! ! transfer states into modState
      mod_State%anthroemisState = anthroEmisState
      mod_State%hydroState = hydroState
      mod_State%heatState = heatState
      mod_State%ohmState = ohmState
      mod_State%snowState = snowState
      mod_State%phenState = phenState
      mod_State%stebbsState = stebbsState

      ! ############# evaluation for DTS variables (end) #############
      CALL siteInfo%ALLOCATE(nlayer)
      siteInfo%lumps = lumpsPrm
      siteInfo%ehc = ehcPrm
      siteInfo%spartacus = spartacusPrm
      siteInfo%spartacus_Layer = spartacusLayerPrm
      siteInfo%anthroemis = ahemisPrm
      siteInfo%irrigation = irrPrm
      siteInfo%snow = snowPrm
      siteInfo%conductance = conductancePrm
      siteInfo%lc_paved = pavedPrm
      siteInfo%lc_bldg = bldgPrm
      siteInfo%lc_dectr = dectrPrm
      siteInfo%lc_evetr = evetrPrm
      siteInfo%lc_grass = grassPrm
      siteInfo%lc_bsoil = bsoilPrm
      siteInfo%lc_water = waterPrm
      CALL siteInfo%cal_surf(config)

      ! assign stebbs parameters
      siteInfo%stebbs = stebbsPrm

      ! assign stebbs building parameters
      building_archtype%BuildingCount = BuildingCount
      building_archtype%Occupants = Occupants
      building_archtype%stebbs_Height = stebbs_Height
      building_archtype%FootprintArea = FootprintArea
      building_archtype%WallExternalArea = WallExternalArea
      building_archtype%RatioInternalVolume = RatioInternalVolume
      building_archtype%WWR = WWR
      building_archtype%WallThickness = WallThickness
      building_archtype%WallEffectiveConductivity = WallEffectiveConductivity
      building_archtype%WallDensity = WallDensity
      building_archtype%WallCp = WallCp
      building_archtype%WallextThickness = WallextThickness
      building_archtype%WallextEffectiveConductivity = WallextEffectiveConductivity
      building_archtype%WallextDensity = WallextDensity
      building_archtype%WallextCp = WallextCp
      building_archtype%Wallx1 = Wallx1
      building_archtype%WallExternalEmissivity = WallExternalEmissivity
      building_archtype%WallInternalEmissivity = WallInternalEmissivity
      building_archtype%WallTransmissivity = WallTransmissivity
      building_archtype%WallAbsorbtivity = WallAbsorbtivity
      building_archtype%WallReflectivity = WallReflectivity
      building_archtype%RoofThickness = RoofThickness
      building_archtype%RoofEffectiveConductivity = RoofEffectiveConductivity
      building_archtype%RoofDensity = RoofDensity
      building_archtype%RoofCp = RoofCp
      building_archtype%RoofextThickness = RoofextThickness
      building_archtype%RoofextEffectiveConductivity = RoofextEffectiveConductivity
      building_archtype%RoofextDensity = RoofextDensity
      building_archtype%RoofextCp = RoofextCp
      building_archtype%Roofx1 = Roofx1
      building_archtype%RoofExternalEmissivity = RoofExternalEmissivity
      building_archtype%RoofInternalEmissivity = RoofInternalEmissivity
      building_archtype%RoofTransmissivity = RoofTransmissivity
      building_archtype%RoofAbsorbtivity = RoofAbsorbtivity
      building_archtype%RoofReflectivity = RoofReflectivity
      building_archtype%FloorThickness = FloorThickness
      building_archtype%GroundFloorEffectiveConductivity = GroundFloorEffectiveConductivity
      building_archtype%GroundFloorDensity = GroundFloorDensity
      building_archtype%GroundFloorCp = GroundFloorCp
      building_archtype%WindowThickness = WindowThickness
      building_archtype%WindowEffectiveConductivity = WindowEffectiveConductivity
      building_archtype%WindowDensity = WindowDensity
      building_archtype%WindowCp = WindowCp
      building_archtype%WindowExternalEmissivity = WindowExternalEmissivity
      building_archtype%WindowInternalEmissivity = WindowInternalEmissivity
      building_archtype%WindowTransmissivity = WindowTransmissivity
      building_archtype%WindowAbsorbtivity = WindowAbsorbtivity
      building_archtype%WindowReflectivity = WindowReflectivity
      building_archtype%InternalMassDensity = InternalMassDensity
      building_archtype%InternalMassCp = InternalMassCp
      building_archtype%InternalMassEmissivity = InternalMassEmissivity
      building_archtype%MaxHeatingPower = MaxHeatingPower
      building_archtype%WaterTankWaterVolume = WaterTankWaterVolume
      building_archtype%MaximumHotWaterHeatingPower = MaximumHotWaterHeatingPower
      building_archtype%HeatingSetpointTemperature = HeatingSetpointTemperature
      building_archtype%CoolingSetpointTemperature = CoolingSetpointTemperature
      siteInfo%building_archtype = building_archtype

      IF (mod_state%flagState%stebbs_bldg_init == 0) THEN
         CALL gen_building(mod_state%stebbsState, siteInfo%stebbs, siteInfo%building_archtype, config, mod_state%stebbsState%buildings(1), nlayer)
         mod_state%flagState%stebbs_bldg_init = 1
      END IF 

      !   allocate output arrays

      Diagnose = 0
      DO ir = 1, len_sim, 1
         ! =============================================================================
         ! === Translate met data from MetForcingBlock to variable names used in model ==
         ! =============================================================================
         timer%iy = INT(MetForcingBlock(ir, 1)) !Integer variables
         timer%id = INT(MetForcingBlock(ir, 2))
         timer%it = INT(MetForcingBlock(ir, 3))
         timer%imin = INT(MetForcingBlock(ir, 4))
         timer%isec = 0 ! NOT used by SUEWS but by WRF-SUEWS via the cal_main interface
         ! calculate dectime
         CALL SUEWS_cal_dectime_DTS( &
            timer, & ! input
            timer%dectime) ! output
         ! calculate tstep related VARIABLES
         CALL SUEWS_cal_tstep_DTS( &
            timer, & ! input
            timer%nsh, timer%nsh_real, timer%tstep_real) ! output

         ! calculate dayofweek information
         CALL SUEWS_cal_weekday_DTS( &
            timer, siteInfo, & !input
            timer%dayofWeek_id) !output

         ! calculate dayofweek information
         CALL SUEWS_cal_DLS_DTS( &
            timer, ahemisPrm, & !input
            timer%DLS) !output

         ! CALL NARP_cal_SunPosition_DTS( &
         !    timer, & !input:
         !    siteInfo, &
         !    timer%azimuth, timer%zenith_deg) !output:
         ! print *, 'azimuth, zenith_deg', timer%azimuth, timer%zenith_deg

         forcing%qn1_obs = MetForcingBlock(ir, 5) !Real values (kind(1d0))
         forcing%qs_obs = MetForcingBlock(ir, 8)
         forcing%qf_obs = MetForcingBlock(ir, 9)
         forcing%U = MetForcingBlock(ir, 10)
         forcing%RH = MetForcingBlock(ir, 11)
         forcing%temp_c = MetForcingBlock(ir, 12)
         forcing%pres = MetForcingBlock(ir, 13)
         forcing%rain = MetForcingBlock(ir, 14)
         forcing%kdown = MetForcingBlock(ir, 15)
         forcing%snowfrac = MetForcingBlock(ir, 16)
         forcing%ldown = MetForcingBlock(ir, 17)
         forcing%fcld = MetForcingBlock(ir, 18)
         forcing%Wu_m3 = MetForcingBlock(ir, 19)
         forcing%xsmd = MetForcingBlock(ir, 20)
         forcing%LAI_obs = MetForcingBlock(ir, 21)

         IF (config%flag_test .AND. PRESENT(state_debug)) THEN
            CALL SUEWS_cal_Main( &
               timer, forcing, config, siteInfo, &
               mod_State, &
               output_line_suews, &
               state_debug) !output
         ELSE
            CALL SUEWS_cal_Main( &
               timer, forcing, config, siteInfo, &
               mod_State, &
               output_line_suews) !output
         END IF
         ! update dt_since_start_x for next iteration, dt_since_start_x is used for Qn averaging. TS 28 Nov 2018
         timer%dt_since_start = timer%dt_since_start + timer%tstep

         !============ update DailyStateBlock ===============
         dataOutBlockDailyState(ir, :) = [output_line_suews%dataOutLineDailyState]

         !============ update state_block ===============
         IF (config%flag_test .AND. PRESENT(state_debug)) THEN
            block_mod_state%BLOCK(ir) = mod_State
         END IF

         !============ write out results ===============
         ! works at each timestep
         CALL SUEWS_update_output( &
            SnowUse, storageheatmethod, & !input
            len_sim, 1, &
            ir, gridiv_x, &
            output_line_suews%dataOutLineSUEWS, &
            output_line_suews%dataOutLineSnow, &
            output_line_suews%dataOutLineESTM, & !input
            output_line_suews%dataoutLineRSL, &
            output_line_suews%dataOutLineBEERS, &
            output_line_suews%dataOutLinedebug, &
            output_line_suews%dataOutLineSPARTACUS, &
            output_line_suews%dataOutLineEHC, &
            output_line_suews%dataOutLineSTEBBS, &
            output_line_suews%dataOutLineNHood, & !input
            dataOutBlockSUEWS_X, dataOutBlockSnow_X, dataOutBlockESTM_X, & !
            dataOutBlockRSL_X, dataOutBlockBEERS_X, dataOutBlockDebug_X, dataOutBlockSPARTACUS_X, dataOutBlockEHC_X, &
            dataOutBlockSTEBBS_X, &
            dataOutBlockNHood_X &
            ) !inout

      END DO

      ! update INOUT variables
      ! Tair_av = forcing%Tair_av_5d
      Tair_av = atmState%Tair_av

      ! ! transfer modState back into individual states
      anthroEmisState = mod_State%anthroemisState
      hydroState = mod_State%hydrostate
      heatState = mod_State%heatstate
      ohmState = mod_State%ohmstate
      snowState = mod_State%snowstate
      phenState = mod_State%phenstate

      ! mod_state_out = mod_State

      HDD_id = anthroEmisState%HDD_id

      qn_av = ohmState%qn_av
      dqndt = ohmState%dqndt
      qn_s_av = ohmState%qn_s_av
      dqnsdt = ohmState%dqnsdt

      SnowfallCum = snowState%SnowfallCum
      SnowAlb = snowState%SnowAlb
      IceFrac = snowState%IceFrac
      SnowWater = snowState%SnowWater
      SnowDens = snowState%SnowDens
      SnowFrac = snowState%SnowFrac
      SnowPack = snowState%SnowPack

      soilstore_surf = hydroState%soilstore_surf
      state_surf = hydroState%state_surf
      temp_surf = heatState%temp_surf
      tsfc_surf = heatState%tsfc_surf
      WUDay_id = hydroState%WUDay_id

      alb = phenState%alb
      GDD_id = phenState%GDD_id
      SDD_id = phenState%SDD_id
      LAI_id = phenState%LAI_id
      Tmin_id = phenState%Tmin_id
      Tmax_id = phenState%Tmax_id
      lenday_id = phenState%lenday_id
      DecidCap_id = phenState%DecidCap_id
      albDecTr_id = phenState%albDecTr_id
      albEveTr_id = phenState%albEveTr_id
      albGrass_id = phenState%albGrass_id
      porosity_id = phenState%porosity_id
      StoreDrainPrm = phenState%StoreDrainPrm

      IF (config%StorageHeatMethod == 5) THEN
         ! ESTM_ehc related
         temp_roof = heatState%temp_roof
         temp_wall = heatState%temp_wall
         tsfc_roof = heatState%tsfc_roof
         tsfc_wall = heatState%tsfc_wall

         soilstore_roof = hydroState%soilstore_roof
         state_roof = hydroState%state_roof
         soilstore_wall = hydroState%soilstore_wall
         state_wall = hydroState%state_wall
      END IF

      dataOutBlockSUEWS = dataOutBlockSUEWS_X(:, :, 1)
      dataOutBlockSnow = dataOutBlockSnow_X(:, :, 1)
      dataOutBlockESTM = dataOutBlockESTM_X(:, :, 1)
      dataOutBlockEHC = dataOutBlockEHC_X(:, :, 1)
      dataOutBlockRSL = dataOutBlockRSL_X(:, :, 1)
      dataOutBlockBEERS = dataOutBlockBEERS_X(:, :, 1)
      dataOutBlockDebug = dataOutBlockDebug_X(:, :, 1)
      dataOutBlockSPARTACUS = dataOutBlockSPARTACUS_X(:, :, 1)
      ! dataOutBlockDailyState = dataOutBlockDailyState_X(:, :, 1)
      dataOutBlockSTEBBS = dataOutBlockSTEBBS_X(:, :, 1)

      ! initialize output block
      CALL output_block_suews%cleanup()
      CALL output_block_suews%init(len_sim)
      ! CALL output_block_finalize(output_block_suews)
      ! CALL output_block_init(output_block_suews, len_sim)
      ! transfer data to output block
      output_block_suews%dataOutBlockSUEWS = dataOutBlockSUEWS
      output_block_suews%dataOutBlockSnow = dataOutBlockSnow
      output_block_suews%dataOutBlockESTM = dataOutBlockESTM
      output_block_suews%dataOutBlockEHC = dataOutBlockEHC
      output_block_suews%dataOutBlockRSL = dataOutBlockRSL
      output_block_suews%dataOutBlockBEERS = dataOutBlockBEERS
      output_block_suews%dataOutBlockDebug = dataOutBlockDebug
      output_block_suews%dataOutBlockSPARTACUS = dataOutBlockSPARTACUS
      output_block_suews%dataOutBlockDailyState = dataOutBlockDailyState
      output_block_suews%dataOutBlockSTEBBS = dataOutBlockSTEBBS

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

   FUNCTION cal_tsfc(qh, dens_air, vcp_air, RA, temp_c) RESULT(tsfc_C)
      ! calculate surface/skin temperature
      ! TS, 23 Oct 2019
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(in) :: qh ! sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: dens_air ! air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: vcp_air !air volumetric heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: RA !Aerodynamic resistance [s m^-1]
      REAL(KIND(1D0)), INTENT(in) :: temp_C ! air temperature [C]

      REAL(KIND(1D0)) :: tsfc_C ! surface temperature [C]

      tsfc_C = qh/(dens_air*vcp_air)*RA + temp_C
   END FUNCTION cal_tsfc

FUNCTION cal_tsfc_dyohm(Temp_in, Qs, K, C, z, nz, T_bottom, dt) RESULT(Temp_out)
    !--------------------------------------------------------------------
    ! Updates the soil temperature profile for one timestep using an
    ! explicit finite-difference scheme under prescribed surface heat flux
    ! and fixed deep-soil temperature.
    !
    ! Inputs:
    !   K           - thermal conductivity [W/m K]
    !   C           - volumetric heat capacity [J/m3 K]
    !   z(nz)       - depth coordinates [m], start=0, increasing downward
    !   Qs          - storage heat flux/conductive heat flux at top [W/m2], positive downward
    !   T_bottom    - fixed temperature of deep ground[°C]
    !   dt          - timestep [s]
    ! 
    ! Input:
    !   Temp_in(nz)    - soil temperature profile from previous timestep [°C]
    !
    ! Result:
    !   Temp_out(nz) - soil temperature profile for next timestep [°C]
    !
    ! Notes:
    !   Stability criterion: alpha*dt/dz_min^2 ≤ 0.5
    !--------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nz, dt
    REAL(KIND(1D0)), INTENT(IN) :: Temp_in(nz)
    REAL(KIND(1D0)), INTENT(IN) :: K, C, z(nz), Qs, T_bottom
    REAL(KIND(1D0)) :: Temp_out(nz)
    REAL(KIND(1D0)) :: alpha, dz_up, dz_down, dz_tot, dz_surface, dz_min, d2Tdz2
    INTEGER :: i

    !----------------------------------------------------------
    ! Compute thermal diffusivity [m/s]
    !----------------------------------------------------------
    alpha = K / C

    !----------------------------------------------------------
    ! Stability check
    !----------------------------------------------------------
    dz_min = MINVAL(z(2:nz) - z(1:nz-1))
    IF (alpha * dt / (dz_min**2) > 0.5D0) THEN
        PRINT *, '⚠️ Warning: time step may be too large for stability.'
        PRINT '(A,ES12.4,2X,A,I8,2X,A,F8.4)', 'alpha=', alpha, 'dt=', dt, 'dz_min=', dz_min
    END IF

    ! Initialize output
    Temp_out = Temp_in

    !----------------------------------------------------------
    ! Interior nodes
    !----------------------------------------------------------
    DO i = 2, nz - 1
        dz_up   = z(i)   - z(i - 1)
        dz_down = z(i+1) - z(i)
        dz_tot  = z(i+1) - z(i-1)

        d2Tdz2 = (2.0D0 / dz_tot) * ((Temp_in(i+1) - Temp_in(i)) / dz_down - &
                                     (Temp_in(i) - Temp_in(i-1)) / dz_up)

        Temp_out(i) = Temp_in(i) + alpha * dt * d2Tdz2
    END DO

    !----------------------------------------------------------
    ! Top boundary: prescribed flux (positive downward)
    !----------------------------------------------------------
    dz_surface = z(2) - z(1)
    Temp_out(1) = Temp_out(2) + (Qs * dz_surface / K)

    !----------------------------------------------------------
    ! Bottom boundary: fixed deep-soil temperature
    !----------------------------------------------------------
    Temp_out(nz) = T_bottom

END FUNCTION cal_tsfc_dyohm

   SUBROUTINE restore_state(mod_state, mod_state_stepstart)
      ! restore model state from the beginning of the time step while keeping the surface temperature updated from last iteration
      ! TS, 03 Mar 2025
      IMPLICIT NONE
      TYPE(SUEWS_STATE), INTENT(INOUT) :: mod_state ! model state being updated during the time step
      TYPE(SUEWS_STATE), INTENT(IN) :: mod_state_stepstart ! model state saved at the beginning of the time step

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof_tmp ! temporary surface temperature of roof saved at the beginning of the time step
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_wall_tmp ! temporary surface temperature of wall saved at the beginning of the time step
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf_tmp ! temporary surface temperature saved at the beginning of the time step
      TYPE(HEAT_STATE) :: heatstate_tmp ! temporary heat state saved at the beginning of the time step
      TYPE(ATM_STATE) :: atmstate_tmp ! temporary atmosphere state saved at the beginning of the time step
      TYPE(ROUGHNESS_STATE) :: roughnessstate_tmp ! temporary roughness state saved at the beginning of the time step
      INTEGER :: nlayer ! number of vertical layers
      INTEGER :: nsurf ! number of surfaces

      ! get dimension information from surface temperature variables
      nlayer = SIZE(mod_state%heatstate%tsfc_roof)
      nsurf = SIZE(mod_state%heatstate%tsfc_surf)

      ! allocate temporary surface temperature arrays
      ALLOCATE (tsfc_roof_tmp(nlayer))
      ALLOCATE (tsfc_wall_tmp(nlayer))
      ALLOCATE (tsfc_surf_tmp(nsurf))

      ! save surface temperature updated from last iteration and use it as initial surface temperature for the next iteration
      tsfc_roof_tmp = mod_state%heatstate%tsfc_roof
      tsfc_wall_tmp = mod_state%heatstate%tsfc_wall
      tsfc_surf_tmp = mod_state%heatstate%tsfc_surf

      heatstate_tmp = mod_state%heatstate
      atmstate_tmp = mod_state%atmstate
      roughnessstate_tmp = mod_state%roughnessstate

      ! restore model state from the beginning of the time step
      mod_state = mod_state_stepstart

      ! restore surface temperature
      mod_state%heatstate%tsfc_roof = tsfc_roof_tmp
      mod_state%heatstate%tsfc_wall = tsfc_wall_tmp
      mod_state%heatstate%tsfc_surf = tsfc_surf_tmp

      mod_state%heatstate = heatstate_tmp
      mod_state%atmstate = atmstate_tmp
      mod_state%roughnessstate = roughnessstate_tmp

      ! deallocate temporary surface temperature arrays
      DEALLOCATE (tsfc_roof_tmp)
      DEALLOCATE (tsfc_wall_tmp)
      DEALLOCATE (tsfc_surf_tmp)

   END SUBROUTINE restore_state

END MODULE SUEWS_Driver
