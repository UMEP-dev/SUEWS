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
MODULE module_ctrl_driver
   USE module_ctrl_const_options, ONLY: &
      OHM_QF_EXCLUDE, OHM_QF_INCLUDE, SHM_ANOHM, SHM_DYOHM, &
      SHM_DYOHM_BUILDING, SHM_EHC, SHM_OBSERVED, SHM_OHM, &
      SHM_STEBBS, STEBBS_DEFAULT, STEBBS_PROVIDED
   ! only the following immutable objects are imported:
   ! 1. functions/subroutines
   ! 2. constant variables
   USE module_ctrl_type, ONLY: SUEWS_CONFIG, SURF_STORE_PRM, WATER_DIST_PRM, bioCO2_PRM, CONDUCTANCE_PRM, error_state, &
                            LAI_PRM, OHM_COEF_LC, OHM_PRM, SOIL_PRM, anthroHEAT_PRM, IRRIG_daywater, &
                            IRRIGATION_PRM, anthroEMIS_PRM, SNOW_PRM, SPARTACUS_PRM, SPARTACUS_LAYER_PRM, &
                            SUEWS_SITE, LUMPS_PRM, EHC_PRM, LC_PAVED_PRM, LC_BLDG_PRM, LC_DECTR_PRM, LC_EVETR_PRM, &
                            LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, anthroEmis_STATE, &
                            OHM_STATE, PHENOLOGY_STATE, SNOW_STATE, SUEWS_FORCING, SUEWS_TIMER, SUEWS_CLOCK, &
                            HYDRO_STATE, HEAT_STATE, &
                            ROUGHNESS_STATE, solar_State, atm_state, flag_STATE, &
                            SUEWS_STATE, SUEWS_DEBUG, STEBBS_STATE, BUILDING_ARCHETYPE_PRM, STEBBS_PRM, STEBBS_BLDG, &
                            output_line
   USE module_type_surface, ONLY: ANOHM_MAX_SAMPLES, ANOHM_MIN_VALID_SAMPLES
   USE module_phys_anohm, ONLY: AnOHM
   USE module_util_meteo, ONLY: qsatf, RH2qa, qa2RH
   USE module_phys_atmmoiststab, ONLY: cal_AtmMoist, cal_Stab, stab_psi_heat, stab_psi_mom, &
                                     SUEWS_update_atmState, relax_zl, stability_scales, &
                                     free_convection_velocity, relax_free_convection
   USE module_phys_narp, ONLY: NARP_update_SunPosition
   USE module_phys_spartacus, ONLY: SPARTACUS, split_shortwave_perez, split_shortwave_forcing, split_shortwave_reindl
   USE module_phys_resist, ONLY: AerodynamicResistance, BoundaryLayerResistance, SurfaceResistance, &
                            SUEWS_cal_RoughnessParameters
   USE module_phys_ohm, ONLY: OHM
   USE module_phys_ehc, ONLY: EHC, dts_ehc_implicit, ehc_init_profile
   USE module_phys_snow, ONLY: SnowCalc, MeltHeat, SnowUpdate, update_snow_albedo, update_snow_dens
   USE module_phys_dailystate, ONLY: update_DailyStateLine, SUEWS_cal_DailyState
   USE module_phys_waterdist, ONLY: &
      drainage, cal_water_storage, cal_water_storage_surf, &
      cal_water_storage_building, &
      SUEWS_cal_SoilState, &
      SUEWS_update_SoilMoist, &
      ReDistributeWater, SUEWS_cal_HorizontalSoilWater, &
      SUEWS_cal_WaterUse
   USE module_phys_lumps, ONLY: LUMPS_cal_QHQE
   USE module_phys_evap, ONLY: cal_evap, cal_evap_multi
   USE module_phys_rslprof, ONLY: RSLProfile
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
   USE module_phys_solweig, ONLY: SOLWEIG_cal_main
   USE module_phys_beers, ONLY: BEERS_cal_main
   USE module_phys_stebbs, ONLY: stebbsonlinecouple
   USE module_ctrl_version, ONLY: git_commit, compiler_ver ! these are automatically generated during compilation time
   USE module_util_time, ONLY: SUEWS_cal_dectime, SUEWS_cal_tstep, SUEWS_cal_weekday, &
                          SUEWS_cal_DLS, SUEWS_cal_clock, cal_weekday_index
   ! Re-export error state accessors from module_ctrl_error_state
   USE module_ctrl_error_state, ONLY: supy_error_flag, get_supy_error, SUPY_ERROR_MESSAGE_LEN, &
                                       reset_supy_error, set_supy_error
   USE module_ctrl_error, ONLY: ErrorHint
   USE module_ctrl_const_mathconst, ONLY: pi
   USE module_ctrl_const_physconst, ONLY: C2K, SBConst, sec_per_day
   USE module_ctrl_const_calendar, ONLY: &
      NDAYS_WEEK, DOW_SUNDAY, DOW_MONDAY, DOW_TUESDAY, DOW_WEDNESDAY, &
      DOW_THURSDAY, DOW_FRIDAY, DOW_SATURDAY
   IMPLICIT NONE

   ! Make error state accessors public for the C-ABI driver
   PUBLIC :: supy_error_flag, get_supy_error
   PUBLIC :: reset_supy_error, set_supy_error
   PRIVATE :: SUEWS_cal_Main_impl
   PRIVATE :: store_output_line, SUEWS_load_forcing_row

   INTERFACE
      MODULE SUBROUTINE store_output_line(line_out, ir, dataOutBlockData, datetimeBlock)
         TYPE(output_line), INTENT(IN) :: line_out
         INTEGER, INTENT(IN) :: ir
         REAL(KIND(1D0)), INTENT(INOUT) :: dataOutBlockData(:, :), datetimeBlock(:, :)
      END SUBROUTINE store_output_line

      PURE MODULE SUBROUTINE SUEWS_load_forcing_row(forcing, MetForcingBlock, ir)
         TYPE(SUEWS_FORCING), INTENT(INOUT) :: forcing
         REAL(KIND(1D0)), DIMENSION(:, :), INTENT(IN) :: MetForcingBlock
         INTEGER, INTENT(IN) :: ir
      END SUBROUTINE SUEWS_load_forcing_row
   END INTERFACE

   ! Per-timestep EHC output diagnostics: recomputed every timestep and written
   ! to the EHC output group, so kept out of persisted HEAT_STATE.
   TYPE, PRIVATE :: dts_ehc_diagnostics
      REAL(KIND(1D0)) :: ehc_dudt = -999.0D0 ! heat-content tendency of EHC columns [W m-2]
      REAL(KIND(1D0)) :: ehc_q_bottom = -999.0D0 ! conductive flux leaving lower boundaries [W m-2]
      REAL(KIND(1D0)) :: ehc_q_indoor = -999.0D0 ! conductive flux from indoor boundary into building columns [W m-2]
      REAL(KIND(1D0)) :: ehc_t_indoor = -999.0D0 ! indoor boundary temperature of implicit EHC columns [degC]
      REAL(KIND(1D0)) :: ehc_t_deep = -999.0D0 ! area-weighted lower boundary temperature of ground columns [degC]
      REAL(KIND(1D0)) :: ehc_qf_air = -999.0D0 ! QF entering air rather than a surface energy balance [W m-2]
      REAL(KIND(1D0)) :: qf_build = -999.0D0 ! building energy-use part of QF [W m-2]
   END TYPE dts_ehc_diagnostics

   ! Per-timestep numerical history only: no checkpoint or shared mutable state.
   TYPE, PRIVATE :: dts_ehc_stability_iteration
      REAL(KIND(1D0)) :: qh
      REAL(KIND(1D0)) :: qh_negative, residual_negative
      REAL(KIND(1D0)) :: qh_positive, residual_positive
      LOGICAL :: has_negative, has_positive
   END TYPE dts_ehc_stability_iteration
   PRIVATE :: ehc_stability_start, ehc_stability_update

CONTAINS

! ===================MAIN CALCULATION WRAPPER FOR ENERGY AND WATER FLUX===========

   SUBROUTINE SUEWS_cal_Main( &
      timer, forcing, config, siteInfo, &
      modState, &
      outputLine, &
      debugState)

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState
      TYPE(output_line), INTENT(OUT) :: outputLine
      TYPE(SUEWS_DEBUG), INTENT(OUT), OPTIONAL :: debugState

      TYPE(SUEWS_CLOCK) :: clock

      ! The caller's timer is input only; derive this step's clock from it.
      CALL SUEWS_cal_clock( &
         timer, siteInfo, siteInfo%anthroemis, &
         config%ForcingTimestampReference, clock)

      IF (PRESENT(debugState)) THEN
         CALL SUEWS_cal_Main_impl( &
            clock, forcing, config, siteInfo, modState, outputLine, debugState)
      ELSE
         CALL SUEWS_cal_Main_impl( &
            clock, forcing, config, siteInfo, modState, outputLine)
      END IF
   END SUBROUTINE SUEWS_cal_Main

   SUBROUTINE SUEWS_cal_Main_impl( &
      clock, forcing, config, siteInfo, &
      modState, &
      outputLine, &
      debugState) ! output

      IMPLICIT NONE

      ! input variables

      ! ####################################################################################
      !  declaration for DTS variables
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
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
      INTEGER, PARAMETER :: max_iter = 60 ! maximum number of iterations
      ! free-convection velocity accepted on the latest outer pass of this timestep,
      ! carried between passes for its under-relaxation [m s-1]
      REAL(KIND(1D0)) :: w_f_pass_prev
      ! (z-d)/L [-] and free-convection velocity [m s-1] accepted before the current
      ! outer pass, restored before the EHC residual-QH resistance call
      REAL(KIND(1D0)) :: zl_pass_start, w_f_pass_start
      TYPE(dts_ehc_diagnostics) :: ehcDiag
      TYPE(dts_ehc_stability_iteration) :: ehc_stability
      INTEGER, PARAMETER :: ehc_stability_first_pass = 16
      LOGICAL :: use_ehc_stability, verify_ehc_stability, relax_ehc_stability

      ! Catch stale/mixed build artefacts early with a clear error instead of
      ! allowing downstream out-of-bounds writes.
      CALL validate_outputline_layout(outputLine)
      IF (supy_error_flag()) THEN
         CALL sync_error_to_state(modState, clock%timer)
         RETURN
      END IF

      use_ehc_stability = config%StorageHeatMethod == SHM_EHC .AND. config%EHCSurfaceCoupling == 1 &
                          .AND. config%control_ehc_stability > 0
      verify_ehc_stability = .FALSE.
      relax_ehc_stability = .FALSE.
      IF (config%StorageHeatMethod == SHM_EHC .AND. &
          (config%control_ehc_stability < 0 .OR. config%control_ehc_stability > 2)) THEN
         CALL set_supy_error(116, 'SUEWS_cal_Main: control_ehc_stability must be 0, 1 or 2')
         CALL sync_error_to_state(modState, clock%timer)
         RETURN
      END IF
      IF (config%StorageHeatMethod == SHM_STEBBS .AND. &
          config%stebbsmethod /= STEBBS_DEFAULT .AND. &
          config%stebbsmethod /= STEBBS_PROVIDED) THEN
         CALL set_supy_error( &
            106, &
            'SUEWS_cal_Main: StorageHeatMethod == SHM_STEBBS (7) requires an active STEBBSmethod (1 or 2)')
         CALL sync_error_to_state(modState, clock%timer)
         RETURN
      END IF
      ehcDiag%ehc_dudt = -999.0D0
      ehcDiag%ehc_q_bottom = -999.0D0
      ehcDiag%ehc_q_indoor = -999.0D0
      ehcDiag%ehc_t_indoor = -999.0D0
      ehcDiag%ehc_t_deep = -999.0D0
      ehcDiag%ehc_qf_air = -999.0D0
      ehcDiag%qf_build = -999.0D0
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
            SnowRemoval => snowState%snow_removal, &
            swe => snowState%swe, &
            Qm => snowState%qm, &
            QmFreez => snowState%qm_freeze, &
            QmRain => snowState%qm_rain, &
            NWstate_per_tstep => hydroState%nw_state_per_tstep, &
            mwh => snowState%mwh, &
            mwstore => snowState%melt_water_store &
            )

            ! IF (Diagnose == 1) WRITE (*, *) 'dectime', dectime

            ! ############# memory allocation for DTS variables (start) #############
            CALL modState_tstepstart%ALLOCATE(nlayer, ndepth)
            IF (PRESENT(debugState)) THEN
               CALL debugState%init(nlayer, ndepth)
            END IF
            ! Clear inactive snow before either iteration snapshot is saved.
            ! Otherwise check_and_reset_states restores the supplied initial
            ! snow on later iterations, even when SnowUse is off (gh#2010).
            IF (config%snowuse == 0) THEN
               snowState%snow_density = 0.
               snowState%snow_fraction = 0.
               snowState%snow_water = 0.
               snowState%snow_albedo = 0.
               snowState%ice_frac = 0.
               snowState%snow_pack = 0.
            END IF

            ! Initialise only fresh implicit facet envelopes, before the
            ! timestep-start snapshot that every nonlinear pass restores.
            ! Checkpoint entry disables the option in prepare_checkpoint_input;
            ! elapsed time also prevents reinitialisation on later timesteps.
            IF (config%StorageHeatMethod == SHM_EHC .AND. config%EHCSurfaceCoupling == 1 &
                .AND. config%control_ehc_initial_profile .AND. config%NetRadiationMethod > 1000 &
                .AND. clock%timer%dt_since_start == clock%timer%tstep) THEN
               CALL ehc_init_envelope(config, clock, forcing, siteInfo, modState)
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
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

            ! ########################################################################################
            ! save initial values of inout variables
            ! snowState = snowState
            snowState%snow_fraction = MERGE(forcing%snow_fraction, snowState%snow_fraction, config%NetRadiationMethod == 0)

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
            IF (config%StorageHeatMethod == SHM_EHC .OR. config%NetRadiationMethod > 1000) THEN
               tsfc_roof_stepstart = tsfc_roof
               tsfc_wall_stepstart = tsfc_wall
            END IF

            !==============surface roughness calculation=======================
            IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_RoughnessParameters...'
            CALL SUEWS_cal_RoughnessParameters( &
               clock, config, forcing, siteInfo, & !input
               modState) ! input/output:
            IF (supy_error_flag()) THEN
               CALL sync_error_to_state(modState, clock%timer)
               RETURN
            END IF

            !=================Calculate sun position=================
            IF (Diagnose == 1) WRITE (*, *) 'Calling NARP_update_SunPosition...'
            ! print *, 'timer: azimuth, zenith_deg', timer%azimuth, timer%zenith_deg
            CALL NARP_update_SunPosition( &
               clock, siteInfo, & !input
               modState) ! input/output:
            IF (supy_error_flag()) THEN
               CALL sync_error_to_state(modState, clock%timer)
               RETURN
            END IF

            !=================Calculation of density and other water related parameters=================
            IF (Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_AtmMoist...'
            CALL SUEWS_update_atmState(clock, forcing, modState)
            IF (supy_error_flag()) THEN
               CALL sync_error_to_state(modState, clock%timer)
               RETURN
            END IF

            ! start iteration-based calculation
            ! through iterations, the surface temperature is examined to be converged
            i_iter = 1
            w_f_pass_prev = 0.0D0
            ! max_iter = 30
            DO WHILE (i_iter < 3 .OR. ((.NOT. flag_converge) .AND. (i_iter < max_iter)))
               relax_ehc_stability = use_ehc_stability .AND. i_iter >= ehc_stability_first_pass &
                                     .AND. .NOT. verify_ehc_stability
               IF (diagnose == 1) THEN
                  PRINT *, '=========================== '
                  PRINT *, 'iteration is ', i_iter
               END IF
               ! ========================================================================================
               ! #374: solve the issue of iteration safety
               ! IMPORTANT: restore some initial states as they SHOULD NOT be changed during iterations
               IF (((.NOT. flag_converge) .AND. i_iter > 1) .OR. (flag_converge .AND. i_iter == 2)) THEN
                  CALL modState%check_and_reset_states(modState_tstepstart)
                  IF (config%StorageHeatMethod == SHM_EHC) THEN
                     CALL reset_ehc_heat_state_for_tsurf_iteration( &
                        modState%heatState, modState_tstepstart%heatState)
                  END IF
               END IF
               ! ========================================================================================
               !==============main calculation start=======================

               ! WIP notes: TS 23 Aug 2023
               ! the following part is the main calculation part of SUEWS
               ! the calculation is divided into several modules relating to major physical processes
               ! the modules are called in the following order:
               ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               ! part A: modules that are called multiple times per timestep to achieve convergence
               ! A1. SUEWS_cal_DailyState: update phenology and land cover states at a daily scale - which may occur either at the beginning or end of a day
               ! A2. SUEWS_cal_WaterUse: calculate water use
               ! A3. SUEWS_cal_AnthropogenicEmission: calculate anthropogenic heat and CO2 fluxes
               ! A4. SUEWS_cal_Qn: calculate net all-wave radiation
               ! A5. SUEWS_cal_Qs: calculate storage heat flux
               ! A6. SUEWS_cal_Water: calculate surface water balance
               ! A7. SUEWS_cal_Resistance: calculate various resistances
               ! A8.1 (optional) SUEWS_cal_snow: calculate snow-related energy balance if snowuse == 1
               ! A8.2 (optional) SUEWS_cal_QE: calculate non-snow-related energy balance if snowuse == 0
               ! A9. SUEWS_cal_HorizontalSoilWater: calculate redistribution of soil water between land covers within a grid cell
               ! A10. SUEWS_cal_SoilState: calculate stored soil water
               ! A11. SUEWS_cal_QH: calculate sensible heat flux
               ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               ! part B: modules that are called once per timestep due to computational inefficiency
               ! B1. RSLProfile: calculate RSL profiles of wind speed, temperature and humidity
               ! B2. SUEWS_cal_BiogenCO2: calculate CO2 fluxes from biogenic sources
               ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

               ! WIP: the wrapper subroutines for different physical processes have the same structure to allow easy implementation of new functionalities
               ! the structure is as follows:
               ! --------------------------------------------------------------------------------
               ! SUEWS_cal_<module_name>( &
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
                  clock, config, forcing, siteInfo, & !input
                  modState) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_01_dailystate = modState
               END IF
               !======== Calculate soil moisture =========
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_update_SoilMoist...'
               CALL SUEWS_update_SoilMoist( &
                  clock, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
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
                  clock, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_03_wateruse = modState
               END IF

               ! ===================ANTHROPOGENIC HEAT AND CO2 FLUX======================
               CALL SUEWS_cal_AnthropogenicEmission( &
                  clock, config, forcing, siteInfo, & ! input
                  modState, ehcDiag) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
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
                  clock, config, forcing, siteInfo, & ! input
                  modState, & ! input/output:
                  dataOutLineSPARTACUS) ! output
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_05_qn = modState
               END IF

               IF (diagnose == 1) PRINT *, 'Tsfc_surf before QS', heatState%tsfc_surf
               CALL SUEWS_cal_Qs( &
                  clock, config, forcing, siteInfo, datetimeLine, & !input
                  modState, ehcDiag, & ! input/output:
                  dataOutLineESTM, dataOutLineSTEBBS)
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_06_qs = modState
               END IF

               !==================Energy related to snow melting/freezing processes=======
               IF (Diagnose == 1) WRITE (*, *) 'Calling MeltHeat'

               !==========================Turbulent Fluxes================================
               IF (Diagnose == 1) WRITE (*, *) 'Calling LUMPS_cal_QHQE...'
               CALL LUMPS_cal_QHQE( &
                  clock, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_07_qhqe_lumps = modState
               END IF

               !============= calculate water balance =============
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Water...'
               CALL SUEWS_cal_Water( &
                  clock, config, forcing, siteInfo, & ! input
                  modState) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_08_water = modState
               END IF
               !============= calculate water balance end =============

               !===============Resistance Calculations=======================
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Resistance...'
               zl_pass_start = modState%atmState%z_l
               w_f_pass_start = w_f_pass_prev
               IF (relax_ehc_stability) THEN
                  IF (i_iter == ehc_stability_first_pass) &
                     CALL ehc_stability_start(ehc_stability, heatState%qh_init)
                  CALL SUEWS_cal_Resistance( &
                     clock, config, forcing, siteInfo, & ! input
                     modState, w_f_pass_prev, qh_stability=ehc_stability%qh) ! input/output:
               ELSE
                  CALL SUEWS_cal_Resistance( &
                     clock, config, forcing, siteInfo, & ! input
                     modState, w_f_pass_prev) ! input/output:
               END IF
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_09_resist = modState
               END IF
               !===================Resistance Calculations End=======================

               !===================Calculate surface hydrology and related soil water=======================
               ! MP: Until Snow has been fixed this should not be used (TODO)
               IF (config%SnowUse == 1) THEN
                  ! Only show warning once per grid cell (state-based, thread-safe)
                  IF (.NOT. flagState%snow_warning_shown) THEN
#ifdef wrf
                     WRITE (*, *) "WARNING SNOW ON! Not recommended at the moment"
#endif
                     flagState%snow_warning_shown = .TRUE.
                  END IF
                  ! ===================Calculate snow related hydrology=======================
                  ! #234 the snow parts needs more work to be done
                  ! TS 18 Oct 2023: snow is temporarily turned off for easier implementation of other functionalities
                  CALL SUEWS_cal_snow( &
                     clock, config, forcing, siteInfo, & ! input
                     modState, & ! input/output
                     dataOutLineSnow)
                  IF (supy_error_flag()) THEN
                     CALL sync_error_to_state(modState, clock%timer)
                     RETURN
                  END IF
                  ! N.B.: snow-related calculations end here.
                  !===================================================
               ELSE
                  IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_QE...'
                  !======== Evaporation and surface state_id for snow-free conditions ========
                  CALL SUEWS_cal_QE( &
                     clock, config, forcing, siteInfo, & ! input
                     modState) ! input/output:
                  IF (supy_error_flag()) THEN
                     CALL sync_error_to_state(modState, clock%timer)
                     RETURN
                  END IF
                  IF (config%flag_test .AND. PRESENT(debugState)) THEN
                     debugState%state_10_qe = modState
                  END IF
                  !======== Evaporation and surface state_id end========
               END IF

               !============ Sensible heat flux ===============
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_QH...'
               CALL SUEWS_cal_QH( &
                  clock, config, forcing, siteInfo, & ! input
                  modState, ehcDiag) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_11_qh = modState
               END IF
               !============ Sensible heat flux end ===============

               IF (config%StorageHeatMethod == SHM_EHC) THEN
                  IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Resistance with EHC residual QH...'
                  ! Relax once per pass: the residual-QH call relaxes against the values
                  ! accepted on the previous pass, not against the first call of this
                  ! pass, so its result supersedes the first call's
                  modState%atmState%z_l = zl_pass_start
                  w_f_pass_prev = w_f_pass_start
                  IF (relax_ehc_stability) THEN
                     CALL ehc_stability_update(config%control_ehc_stability, ehc_stability, heatState%qh_init)
                     CALL SUEWS_cal_Resistance( &
                        clock, config, forcing, siteInfo, & ! input
                        modState, w_f_pass_prev, qh_stability=ehc_stability%qh) ! input/output:
                  ELSE
                     CALL SUEWS_cal_Resistance( &
                        clock, config, forcing, siteInfo, & ! input
                        modState, w_f_pass_prev) ! input/output:
                  END IF
                  IF (supy_error_flag()) THEN
                     CALL sync_error_to_state(modState, clock%timer)
                     RETURN
                  END IF
               END IF

               ! ============ update surface temperature of this iteration ===============
               CALL suews_update_tsurf( &
                  clock, config, forcing, siteInfo, & ! input
                  modState, ehcDiag) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
               ! The surface-temperature test alone can pass while the relaxed (z-d)/L
               ! and free-convection velocity are still moving, which leaves RA
               ! alternating from one timestep to the next. Also require both to have
               ! changed little over this pass.
               flag_converge = flag_converge .AND. &
                  ABS(modState%atmState%z_l - zl_pass_start) < 0.005D0 .AND. &
                  ABS(w_f_pass_prev - w_f_pass_start) < 0.005D0
               IF (config%flag_test .AND. PRESENT(debugState)) THEN
                  debugState%state_12_tsurf = modState
               END IF

               IF (verify_ehc_stability) THEN
                  ! This complete pass used the original QH/stability update and
                  ! unchanged temperature/flux tests. Accept only its verdict.
                  verify_ehc_stability = .FALSE.
                  IF (.NOT. flag_converge) CALL ehc_stability_start(ehc_stability, heatState%qh_init)
               ELSE IF (relax_ehc_stability .AND. flag_converge) THEN
                  ! A small damped change alone is not convergence. Require a
                  ! full undamped pass, still within the original 59-pass cap.
                  flag_converge = .FALSE.
                  verify_ehc_stability = .TRUE.
               END IF
               ! IF (i_iter == 1) THEN
               !    modState_tstepstart = modState
               ! END IF
               i_iter = i_iter + 1
               IF (i_iter == max_iter .AND. .NOT. flag_converge) THEN
                  IF (diagnose == 1) PRINT *, 'Iteration did not converge in', max_iter, ' iterations'

               END IF
               IF (diagnose == 1) PRINT *, '========================='
               IF (diagnose == 1) PRINT *, ''

               !==============main calculation end=======================
            END DO ! end iteration for tsurf calculations

            ! MP: Add test for QH zL signs - recalculate zL if the same
            IF (modState%heatState%QH*modState%atmState%z_l > 0) THEN
               IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_cal_Resistance...'
               CALL SUEWS_cal_Resistance( &
                  clock, config, forcing, siteInfo, & ! input
                  modState, w_f_pass_prev, skip_relaxation=.TRUE.) ! input/output:
               IF (supy_error_flag()) THEN
                  CALL sync_error_to_state(modState, clock%timer)
                  RETURN
               END IF
            END IF

            !==============================================================
            ! Calculate diagnostics and biogenic CO2 (no feedback to energy balance)

            !============ roughness sub-layer diagonostics ===============
            IF (Diagnose == 1) WRITE (*, *) 'Calling RSLProfile...'
            CALL RSLProfile( &
               clock, config, forcing, siteInfo, & ! input
               modState, & ! input/output:
               dataoutLineRSL) ! output
            IF (supy_error_flag()) THEN
               CALL sync_error_to_state(modState, clock%timer)
               RETURN
            END IF
            IF (config%flag_test .AND. PRESENT(debugState)) THEN
               debugState%state_13_rsl = modState
            END IF

            ! ========================================================================
            ! SUEWS_cal_BiogenCO2: calculate biogenic CO2 fluxes
            ! N.B.: Called after SUEWS_cal_Resistance and RSLProfile to use current local climate diagnostics
            ! ========================================================================
            CALL SUEWS_cal_BiogenCO2( &
               clock, config, forcing, siteInfo, & ! input
               modState) ! input/output:
            IF (supy_error_flag()) THEN
               CALL sync_error_to_state(modState, clock%timer)
               RETURN
            END IF
            IF (config%flag_test .AND. PRESENT(debugState)) THEN
               debugState%state_14_biogenco2 = modState
            END IF

            ! calculations of diagnostics and biogenic CO2 end
            !==============================================================
            IF (Diagnose == 1) WRITE (*, *) 'update inout variables with new values...'
            !==============================================================
            !==============================================================
            ! update inout variables with new values (to be compatible with original interface)
            ! atmState%Tair_av = Tair_av_next

            !==============use BEERS to get localised radiation flux==================
            ! TS 14 Jan 2021: BEERS is a modified version of SOLWEIG
            IF (Diagnose == 1) WRITE (*, *) 'Calling BEERS_cal_main...'
            CALL BEERS_cal_main( &
               clock, config, forcing, siteInfo, & ! input
               modState, & ! input/output:
               dataOutLineBEERS) ! output
            IF (supy_error_flag()) THEN
               CALL sync_error_to_state(modState, clock%timer)
               RETURN
            END IF
            IF (config%flag_test .AND. PRESENT(debugState)) THEN
               debugState%state_15_beers = modState
            END IF
            !==============Use dyOHM storage heat flux to convert surface temperature
            CALL suews_update_tsurf_dyohm( &
               clock, config, forcing, siteInfo, & ! input
               modState) ! input/output:
            IF (supy_error_flag()) THEN
               CALL sync_error_to_state(modState, clock%timer)
               RETURN
            END IF


            !==============translation of  output variables into output array===========
            IF (Diagnose == 1) WRITE (*, *) 'Calling SUEWS_update_outputLine...'
            CALL SUEWS_update_outputLine( &
               clock, config, forcing, siteInfo, & ! input
               modState, & ! input/output:
               datetimeLine, dataOutLineSUEWS) !output
            IF (supy_error_flag()) THEN
               CALL sync_error_to_state(modState, clock%timer)
               RETURN
            END IF

            IF (config%StorageHeatMethod == SHM_EHC) THEN
               IF (Diagnose == 1) WRITE (*, *) 'Calling EHC_update_outputLine...'
               CALL EHC_update_outputLine( &
                  clock, & !input
                  modState, ehcDiag, & ! input/output:
                  datetimeLine, dataOutLineEHC) !output
            END IF

            ! daily state_id:
            IF (Diagnose == 1) WRITE (*, *) 'Calling update_DailyStateLine...'
            CALL update_DailyStateLine( &
               clock, config, forcing, siteInfo, & ! input
               modState, & ! input/output:
               dataOutLineDailyState) !out

            !==============translation end ================
            IF (Diagnose == 1) WRITE (*, *) 'Calling dataoutlineDebug...'
            ! TODO: #233 debugging info will be collected into a derived type for model output when the debugging flag is on
            ! here we may still use the original output array but another derived type can be used for enriched debugging info
            CALL update_debug_info( &
               clock, config, forcing, siteInfo, & ! input
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

            ! Sync module-level error state to modState for thread-safe access
            ! This enables Python to read errors from modState%errorState
            CALL sync_error_to_state(modState, clock%timer)

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_Main_impl
! ================================================================================

   ! Batch multi-timestep execution subroutine - loops internally over timesteps for efficiency
   ! This avoids Python->Fortran call overhead when processing multiple timesteps
   !
   ! Output layout: the data columns of all 11 output groups concatenated per row,
   ! without each group's 5-column datetime prefix, which is written once per row
   ! to datetimeBlock instead. The caller can then hand dataOutBlockData to Python
   ! as the final output table with no gather or copy (gh#1849).
   ! Column order: SUEWS | Snow | BEERS | ESTM | EHC | DailyState | RSL | Debug | SPARTACUS | STEBBS | NHood
   ! dataOutBlockData is (len_sim, ncols_data), where ncols_data is the sum of
   ! all ncolumnsDataOut* constants minus 5 per group; datetimeBlock is (len_sim, 5).
   !
   ! Both blocks are assumed-shape, so the caller may pass a row block of a
   ! larger column-major array (several grids sharing one output table): the
   ! descriptor carries the column stride and no temporary copy is made. They
   ! are INTENT(INOUT) because the rows outside this block belong to other
   ! grids, possibly written by other threads during this call, and must be
   ! left as they are.
   SUBROUTINE SUEWS_cal_multitsteps( &
      timer, MetForcingBlock, len_sim, &
      config, siteInfo, &
      modState, &
      dataOutBlockData, datetimeBlock)

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_TIMER, SUEWS_CLOCK, SUEWS_SITE, &
                                  SUEWS_STATE, output_line

      IMPLICIT NONE

      ! Input/Output arguments
      ! The loop owns the run's timer: it fills the clock fields from the
      ! forcing rows and advances dt_since_start. The physics see only the
      ! per-step clock derived from it.
      TYPE(SUEWS_TIMER), INTENT(INOUT) :: timer
      INTEGER, INTENT(IN) :: len_sim
      REAL(KIND(1D0)), DIMENSION(len_sim, 32), INTENT(IN) :: MetForcingBlock
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState
      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(INOUT) :: dataOutBlockData
      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(INOUT) :: datetimeBlock

      ! Local variables
      TYPE(SUEWS_FORCING) :: forcing
      TYPE(output_line) :: output_line_local
      TYPE(SUEWS_CLOCK) :: clock
      INTEGER :: ir

      ! Loop over timesteps
      DO ir = 1, len_sim, 1
         ! === Update timer from forcing block ===
         timer%iy = INT(MetForcingBlock(ir, 1))
         timer%id = INT(MetForcingBlock(ir, 2))
         timer%it = INT(MetForcingBlock(ir, 3))
         timer%imin = INT(MetForcingBlock(ir, 4))
         timer%isec = 0

         CALL SUEWS_cal_clock( &
            timer, siteInfo, siteInfo%anthroemis, &
            config%ForcingTimestampReference, clock)

         CALL SUEWS_load_forcing_row(forcing, MetForcingBlock, ir)

         ! === Call main calculation ===
         CALL SUEWS_cal_Main_impl( &
            clock, forcing, config, siteInfo, &
            modState, &
            output_line_local)

         IF (supy_error_flag()) THEN
            CALL sync_error_to_state(modState, timer)
            EXIT
         END IF

         CALL store_output_line(output_line_local, ir, dataOutBlockData, datetimeBlock)

         ! === Update dt_since_start and tstep_prev for next iteration ===
         timer%dt_since_start = timer%dt_since_start + timer%tstep
         timer%tstep_prev = timer%tstep

      END DO

   END SUBROUTINE SUEWS_cal_multitsteps


! ================================================================================

   SUBROUTINE update_debug_info( &
      clock, config, forcing, siteInfo, & ! input
      modState_init, & ! input
      modState, & ! inout
      dataoutlineDebug) ! output

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_CLOCK, SUEWS_SITE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, HEAT_STATE, flag_STATE
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
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
            RAsnow => snowState%ra_snow, &
            rss_surf => atmState%rss_surf, &
            vsmd => hydroState%vsmd, &
            g_kdown => phenState%g_kdown, &
            g_dq => phenState%g_dq, &
            g_ta => phenState%g_ta, &
            g_smd => phenState%g_smd, &
            g_lai => phenState%g_lai, &
            vpd_hPa => atmState%vpd_hPa, &
            lv_J_kg => atmState%lv_J_kg, &
            avdens => atmState%av_density, &
            avcp => atmState%av_cp, &
            s_hPa => atmState%s_hPa, &
            psyc_hPa => atmState%psyc_hPa, &
            i_iter => flagState%i_iter, &
            FAIBldg_use => roughnessState%fai_bldg_use, &
            FAIEveTree_use => roughnessState%fai_evetree_use, &
            FAIDecTree_use => roughnessState%fai_dectree_use, &
            FAI => roughnessState%FAI &
            )

            dataoutlineDebug = &
               [MERGE(1D0, 0D0, flag_test), &
                tsfc0_out_surf, &
                qn_surf, qs_surf, qe0_surf, qe_surf, qh_surf, & ! energy balance
                wu_surf, ev0_surf, ev_surf, drain_surf, &
                modState_init%hydroState%state_surf, hydroState%state_surf, &
                modState_init%hydroState%soil_store_surf, hydroState%soil_store_surf, & ! water balance
                RS, RA_h, RB, RAsnow, rss_surf, & ! for debugging QE
                vsmd, conductancePrm%S1/conductancePrm%G_sm + conductancePrm%S2, &
                conductancePrm%G_sm, &
                conductancePrm%G_sm*(vsmd - (conductancePrm%S1/conductancePrm%G_sm + conductancePrm%S2)), & ! debug g_smd
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
      clock, config, forcing, siteInfo, & ! input
      modState, ehcDiag) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_CLOCK, SUEWS_SITE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, HEAT_STATE, flag_STATE, &
                               SUEWS_STATE
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState
      TYPE(dts_ehc_diagnostics), INTENT(IN) :: ehcDiag

      INTEGER :: i_surf, i_layer
      REAL(KIND(1D0)) :: dif_tsfc_iter, dif_qh_iter, qh_flux_tol, ratio_iter, qh_resist_iter
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qf_surf_ehc
      LOGICAL :: use_implicit
      ! implicit EHC: water on the facet route keeps the legacy relaxed update
      REAL(KIND(1D0)), PARAMETER :: ratio_iter_water = 0.3D0
      ! implicit EHC outer-loop tolerances: solved Tsfc change [K] and facet
      ! balance closure [W m-2] for the coupled surface/conduction solve
      REAL(KIND(1D0)), PARAMETER :: tsfc_tol_implicit = 0.01D0
      REAL(KIND(1D0)), PARAMETER :: qh_flux_tol_implicit = 1.0D0
      REAL(KIND(1D0)) :: tsfc_tol
      ! implicit EHC with building facets: change of the water Tsfc, which
      ! keeps the legacy relaxed update and is tested as in the explicit loop
      REAL(KIND(1D0)) :: dif_tsfc_water
      REAL(KIND(1D0)), PARAMETER :: tsfc_air_limit = 50.0D0
      REAL(KIND(1D0)), PARAMETER :: tsfc_iter_step_limit = 10.0D0
      LOGICAL :: use_building_facets_for_ehc
      LOGICAL, DIMENSION(nsurf) :: active_nonwater_mask

      ASSOCIATE ( &
         flagState => modState%flagState, &
         heatState => modState%heatState, &
         atmState => modState%atmState &
         )

         ASSOCIATE ( &
            flag_converge => flagState%flag_converge, &
            diagnose => config%diagnose, &
            StorageHeatMethod => config%StorageHeatMethod, &
            NetRadiationMethod => config%NetRadiationMethod, &
            nlayer => siteInfo%nlayer, &
            sfr_surf => siteInfo%sfr_surf, &
            avdens => atmState%av_density, &
            avcp => atmState%av_cp, &
            RA_h => atmState%RA_h, &
            TSfc_C => heatState%TSfc_C, &
            QH_surf => heatState%QH_surf, &
            QH_roof => heatState%QH_roof, &
            QH_wall => heatState%QH_wall, &
            QN_surf => heatState%QN_surf, &
            QN_roof => heatState%QN_roof, &
            QN_wall => heatState%QN_wall, &
            QS_surf => heatState%QS_surf, &
            QS_roof => heatState%QS_roof, &
            QS_wall => heatState%QS_wall, &
            QE_surf => heatState%QE_surf, &
            QE_roof => heatState%QE_roof, &
            QE_wall => heatState%QE_wall, &
            qf => heatState%qf, &
            qh => heatState%qh, &
            qh_resist_surf => heatState%qh_resist_surf, &
            qh_resist_roof => heatState%qh_resist_roof, &
            qh_resist_wall => heatState%qh_resist_wall, &
            qh_residual => heatState%qh_residual, &
            tsfc0_out_surf => heatState%tsfc_surf_stepstart, &
            tsfc0_out_roof => heatState%tsfc_roof_stepstart, &
            tsfc0_out_wall => heatState%tsfc_wall_stepstart, &
            tsfc_surf => heatState%tsfc_surf, &
            tsfc_roof => heatState%tsfc_roof, &
            tsfc_wall => heatState%tsfc_wall, &
            temp_c => forcing%temp_c &
            )

            use_building_facets_for_ehc = StorageHeatMethod == SHM_EHC .AND. NetRadiationMethod > 1000
            use_implicit = StorageHeatMethod == SHM_EHC .AND. config%EHCSurfaceCoupling == 1
            IF (use_implicit) THEN
               CALL ehc_implicit_qf_surface(sfr_surf, ehcDiag%ehc_q_indoor, qf_surf_ehc)
            END IF

            ! Implicit EHC coupling: Tsfc was solved with the conduction columns
            ! in SUEWS_cal_Qs, so this routine only measures the change between
            ! iterates (no residual inversion, relaxation or step limits),
            ! except for water on the facet route, which keeps heatcond1d_CN
            ! and the legacy relaxed update from its residual sensible heat.

            !============ calculate surface temperature ===============
            IF (use_implicit) THEN
               IF (use_building_facets_for_ehc) THEN
                  tsfc_surf(WaterSurf) = tsfc0_out_surf(WaterSurf)*(1.0D0 - ratio_iter_water) &
                                         + ratio_iter_water*cal_tsfc( &
                                         QN_surf(WaterSurf) + qf_surf_ehc(WaterSurf) - QS_surf(WaterSurf) &
                                         - QE_surf(WaterSurf), avdens, avcp, RA_h, temp_c)
               END IF
            ELSE IF (StorageHeatMethod == SHM_EHC) THEN
               ! EHC solves QS from the current surface-temperature boundary.
               ! The next Tsurf iterate is therefore the temperature that would
               ! make aerodynamic sensible heat match the current surface energy
               ! residual: QN + QF - QS - QE.
               TSfc_C = cal_tsfc(qh_residual, avdens, avcp, RA_h, temp_c)

               DO i_surf = 1, nsurf
                  tsfc_surf(i_surf) = cal_tsfc( &
                     QN_surf(i_surf) + qf - QS_surf(i_surf) - QE_surf(i_surf), &
                     avdens, avcp, RA_h, temp_c)
               END DO

               IF (use_building_facets_for_ehc) THEN
                  DO i_layer = 1, nlayer
                     ! QF is a grid-scale anthropogenic heat source. Do not
                     ! inject the full grid QF into every roof/wall facet as a
                     ! conductive boundary; doing so over-forces facet storage.
                     tsfc_roof(i_layer) = cal_tsfc( &
                        QN_roof(i_layer) - QS_roof(i_layer) - QE_roof(i_layer), &
                        avdens, avcp, RA_h, temp_c)
                     tsfc_wall(i_layer) = cal_tsfc( &
                        QN_wall(i_layer) - QS_wall(i_layer) - QE_wall(i_layer), &
                        avdens, avcp, RA_h, temp_c)
                  END DO
               END IF
            ELSE
               TSfc_C = cal_tsfc(qh, avdens, avcp, RA_h, temp_c)

               !============= calculate surface specific QH and Tsfc ===============

               DO i_surf = 1, nsurf
                  tsfc_surf(i_surf) = cal_tsfc(QH_surf(i_surf), avdens, avcp, RA_h, temp_c)
               END DO

               DO i_layer = 1, nlayer
                  tsfc_roof(i_layer) = cal_tsfc(QH_roof(i_layer), avdens, avcp, RA_h, temp_c)
                  tsfc_wall(i_layer) = cal_tsfc(QH_wall(i_layer), avdens, avcp, RA_h, temp_c)
               END DO
            END IF

            ! Initialise before the disjoint StorageHeatMethod / use_implicit
            ! branches so every path defines dif_tsfc_iter manifestly for -Wmaybe-uninitialized.
            dif_tsfc_iter = 0.0D0
            IF (StorageHeatMethod /= SHM_EHC) THEN
               ! Exclude absent surfaces (sfr_surf == 0) from convergence
               ! stopping so their uncoupled temperatures cannot add
               ! iterations and leak into grid-scale fluxes.
               IF (ANY(sfr_surf > 0.0D0)) THEN
                  dif_tsfc_iter = MAXVAL(ABS(tsfc_surf - tsfc0_out_surf), MASK=(sfr_surf > 0.0D0))
               ELSE
                  dif_tsfc_iter = 0.0D0
               END IF
            END IF
            dif_qh_iter = 0.0D0
            dif_tsfc_water = 0.0D0
            ! outer-loop tolerances: the implicit coupling tests the full change
            ! of the solved Tsfc and the closure of each facet balance
            IF (use_implicit) THEN
               tsfc_tol = tsfc_tol_implicit
               qh_flux_tol = qh_flux_tol_implicit
               ratio_iter = 1.0D0
            ELSE
               ! default-real literal, as in the legacy test (dif_tsfc_iter > .05),
               ! so the explicit comparison is unchanged to the last bit
               tsfc_tol = REAL(0.05, KIND(1D0))
               qh_flux_tol = 5.0D0
               ratio_iter = 0.3D0
            END IF
            tsfc_surf = (tsfc0_out_surf*(1 - ratio_iter) + tsfc_surf*ratio_iter)
            IF (use_building_facets_for_ehc) THEN
               tsfc_roof = (tsfc0_out_roof*(1 - ratio_iter) + tsfc_roof*ratio_iter)
               tsfc_wall = (tsfc0_out_wall*(1 - ratio_iter) + tsfc_wall*ratio_iter)
            END IF

            IF (use_implicit) THEN
               IF (use_building_facets_for_ehc) THEN
                  ! water keeps its relaxed fixed-point step (omega 0.3), so a
                  ! 0.01 K test on that step would need ~13 passes per kelvin
                  ! of initial error; it keeps the legacy 0.05 K test instead
                  IF (sfr_surf(WaterSurf) > 0.0D0) THEN
                     dif_tsfc_water = ABS(tsfc_surf(WaterSurf) - tsfc0_out_surf(WaterSurf))
                  END IF
                  active_nonwater_mask = (sfr_surf > 0.0D0) .AND. [(i_surf /= WaterSurf, i_surf=1, nsurf)]
                  IF (ANY(active_nonwater_mask)) THEN
                     dif_tsfc_iter = MAXVAL(ABS(tsfc_surf - tsfc0_out_surf), MASK=active_nonwater_mask)
                  ELSE
                     dif_tsfc_iter = 0.0D0
                  END IF
                  IF (sfr_surf(BldgSurf) > 0.0D0) THEN
                     dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc_roof - tsfc0_out_roof)), dif_tsfc_iter)
                     dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc_wall - tsfc0_out_wall)), dif_tsfc_iter)
                  END IF
               ELSE
                  IF (ANY(sfr_surf > 0.0D0)) THEN
                     dif_tsfc_iter = MAXVAL(ABS(tsfc_surf - tsfc0_out_surf), MASK=(sfr_surf > 0.0D0))
                  ELSE
                     dif_tsfc_iter = 0.0D0
                  END IF
               END IF
               ! closure of each solved surface balance with the updated QE and r_a
               IF (RA_h > 0.0D0 .AND. avdens > 0.0D0 .AND. avcp > 0.0D0) THEN
                  DO i_surf = 1, nsurf
                     ! the building tile is its facets; water on the facet
                     ! route converges through its relaxed Tsfc step instead
                     IF (use_building_facets_for_ehc .AND. (i_surf == BldgSurf .OR. i_surf == WaterSurf)) CYCLE
                     IF (sfr_surf(i_surf) <= 0.0D0) CYCLE
                     qh_resist_iter = avdens*avcp*(tsfc_surf(i_surf) - temp_c)/RA_h
                     dif_qh_iter = MAX(dif_qh_iter, ABS( &
                        QN_surf(i_surf) + qf_surf_ehc(i_surf) - QS_surf(i_surf) - QE_surf(i_surf) - qh_resist_iter))
                  END DO
                  IF (use_building_facets_for_ehc .AND. sfr_surf(BldgSurf) > 0.0D0) THEN
                     DO i_layer = 1, nlayer
                        qh_resist_iter = avdens*avcp*(tsfc_roof(i_layer) - temp_c)/RA_h
                        dif_qh_iter = MAX(dif_qh_iter, ABS( &
                           QN_roof(i_layer) - QS_roof(i_layer) - QE_roof(i_layer) - qh_resist_iter))
                        qh_resist_iter = avdens*avcp*(tsfc_wall(i_layer) - temp_c)/RA_h
                        dif_qh_iter = MAX(dif_qh_iter, ABS( &
                           QN_wall(i_layer) - QS_wall(i_layer) - QE_wall(i_layer) - qh_resist_iter))
                     END DO
                  END IF
               END IF
               TSfc_C = DOT_PRODUCT(tsfc_surf, sfr_surf)
            ELSE IF (StorageHeatMethod == SHM_EHC) THEN
               ! The EHC Tsurf update is a fixed-point iterate inferred from the
               ! residual sensible heat flux. Keep that numerical iterate within
               ! a physical envelope so a bad residual cannot pollute heat or
               ! water states before convergence is reached.
               TSfc_C = MAX(MIN(TSfc_C, temp_c + tsfc_air_limit), temp_c - tsfc_air_limit)
               tsfc_surf = MAX(MIN(tsfc_surf, tsfc0_out_surf + tsfc_iter_step_limit), &
                               tsfc0_out_surf - tsfc_iter_step_limit)
               tsfc_surf = MAX(MIN(tsfc_surf, temp_c + tsfc_air_limit), temp_c - tsfc_air_limit)
               IF (use_building_facets_for_ehc) THEN
                  tsfc_roof = MAX(MIN(tsfc_roof, tsfc0_out_roof + tsfc_iter_step_limit), &
                                  tsfc0_out_roof - tsfc_iter_step_limit)
                  tsfc_wall = MAX(MIN(tsfc_wall, tsfc0_out_wall + tsfc_iter_step_limit), &
                                  tsfc0_out_wall - tsfc_iter_step_limit)
                  tsfc_roof = MAX(MIN(tsfc_roof, temp_c + tsfc_air_limit), temp_c - tsfc_air_limit)
                  tsfc_wall = MAX(MIN(tsfc_wall, temp_c + tsfc_air_limit), temp_c - tsfc_air_limit)
               END IF

               ! Convergence must be tested against the state that is actually
               ! carried into the next iteration, after relaxation and physical
               ! limiting. Testing the raw fixed-point proposal can report
               ! non-convergence even when the applied update is already small.
               ! Exclude absent surfaces (sfr_surf == 0) and absent building
               ! facets (sfr_surf(BldgSurf) == 0) from both temperature and flux
               ! residuals so uncoupled absent surfaces cannot drive convergence.
               IF (ANY(sfr_surf > 0.0D0)) THEN
                  dif_tsfc_iter = MAXVAL(ABS(tsfc_surf - tsfc0_out_surf), MASK=(sfr_surf > 0.0D0))
               ELSE
                  dif_tsfc_iter = 0.0D0
               END IF
               IF (use_building_facets_for_ehc .AND. sfr_surf(BldgSurf) > 0.0D0) THEN
                  dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc_roof - tsfc0_out_roof)), dif_tsfc_iter)
                  dif_tsfc_iter = MAX(MAXVAL(ABS(tsfc0_out_wall - tsfc_wall)), dif_tsfc_iter)
               END IF

               IF (RA_h /= 0.0D0 .AND. avdens > 0.0D0 .AND. avcp > 0.0D0) THEN
                  DO i_surf = 1, nsurf
                     IF (sfr_surf(i_surf) > 0.0D0) THEN
                        qh_resist_iter = avdens*avcp*(tsfc_surf(i_surf) - temp_c)/RA_h
                        dif_qh_iter = MAX(dif_qh_iter, ABS( &
                           QN_surf(i_surf) + qf - QS_surf(i_surf) - QE_surf(i_surf) - qh_resist_iter))
                     END IF
                  END DO
                  IF (use_building_facets_for_ehc .AND. sfr_surf(BldgSurf) > 0.0D0) THEN
                     DO i_layer = 1, nlayer
                        qh_resist_iter = avdens*avcp*(tsfc_roof(i_layer) - temp_c)/RA_h
                        dif_qh_iter = MAX(dif_qh_iter, ABS( &
                           QN_roof(i_layer) - QS_roof(i_layer) - QE_roof(i_layer) - qh_resist_iter))
                        qh_resist_iter = avdens*avcp*(tsfc_wall(i_layer) - temp_c)/RA_h
                        dif_qh_iter = MAX(dif_qh_iter, ABS( &
                           QN_wall(i_layer) - QS_wall(i_layer) - QE_wall(i_layer) - qh_resist_iter))
                     END DO
                  END IF
               END IF

               TSfc_C = DOT_PRODUCT(tsfc_surf, sfr_surf)
            END IF
            ! =======test end=======

            !============ surface-level diagonostics end ===============

            ! force quit do-while, i.e., skip iteration and use NARP for Tsurf calculation
            ! if (NetRadiationMethod < 10 .or. NetRadiationMethod > 100) exit

            ! Test if sensible heat fluxes converge in iterations
            ! MP Testing 0.1 -> 0.05
            IF (dif_tsfc_iter > tsfc_tol .OR. dif_qh_iter > qh_flux_tol &
                .OR. dif_tsfc_water > REAL(0.05, KIND(1D0))) THEN
               flag_converge = .FALSE.
            ELSE
               flag_converge = .TRUE.
               ! PRINT *, ' qh_residual: ', qh_residual, ' qh_resist: ', qh_resist
               ! PRINT *, ' dif_qh: ', ABS(qh_residual - qh_resist)
               ! PRINT *, ' abs. dif_tsfc: ', dif_tsfc_iter
            END IF

            IF (StorageHeatMethod == SHM_EHC) THEN
               ! EHC keeps the fixed-point iterate inside a symmetric physical
               ! envelope around air temperature.
               tsfc0_out_surf = MAX(MIN(tsfc_surf, Temp_C + tsfc_air_limit), Temp_C - tsfc_air_limit)
               tsfc0_out_roof = MAX(MIN(tsfc_roof, Temp_C + tsfc_air_limit), Temp_C - tsfc_air_limit)
               tsfc0_out_wall = MAX(MIN(tsfc_wall, Temp_C + tsfc_air_limit), Temp_C - tsfc_air_limit)
            ELSE
               ! Preserve the legacy non-EHC upper cap exactly.
               tsfc0_out_surf = MIN(tsfc_surf, Temp_C + 50)
               tsfc0_out_roof = MIN(tsfc_roof, Temp_C + 50)
               tsfc0_out_wall = MIN(tsfc_wall, Temp_C + 50)
            END IF
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE suews_update_tsurf

   SUBROUTINE suews_update_tsurf_dyohm( &
      clock, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_CLOCK, SUEWS_SITE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, HEAT_STATE, flag_STATE, &
                               SUEWS_STATE
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      INTEGER :: i_surf, i_layer, nz
      REAL(KIND(1D0)) :: dif_tsfc_iter, ratio_iter
      REAL(KIND(1D0)), DIMENSION(5) :: z
      REAL(KIND(1D0)), ALLOCATABLE :: prev_profile(:), next_profile(:)
      ASSOCIATE ( &
         timestep => clock%timer%tstep, &
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
            T_bottom => stebbsState%deep_soil_temperature &
            )

            !============= calculate surface temperature based on QS ===============
            ! Method 8 (dyohm_building) does not calculate DyOHM conductive
            ! surface temperatures. Method 7 leaves the building temperature
            ! to STEBBS and updates only its non-building DyOHM surfaces.
            ! Method 6 updates every surface. Other methods retain the
            ! pre-existing diagnostic update.
            IF (StorageHeatMethod == SHM_DYOHM_BUILDING) RETURN

            nz = 5
            z = (/ 0.0D0, 0.03D0, 0.1D0, 1.5D0, 3.0D0 /)
            ! Loop over surfaces
            DO i_surf = 1, nsurf
               IF (StorageHeatMethod == SHM_STEBBS .AND. i_surf == BldgSurf) CYCLE
               ALLOCATE(prev_profile(nz), next_profile(nz))
               prev_profile = heatState%temp_surf_dyohm(i_surf, :)

               next_profile = cal_tsfc_dyohm( &
                  Temp_in = prev_profile, &
                  Qs      = QS_surf(i_surf), &
                  K       = k_surf(i_surf, 1), &
                  C       = cp_surf(i_surf, 1), &
                  z       = z, nz = nz, &
                  T_bottom = T_bottom, dt = timestep, &
                  errorState = modState%errorState )

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
      clock, config, forcing, siteInfo, & ! input
      modState, ehcDiag) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_SITE, SUEWS_CLOCK, SUEWS_CONFIG, SUEWS_FORCING, &
                               anthroEmis_STATE, atm_state, SUEWS_STATE

      IMPLICIT NONE
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INout) :: modState
      TYPE(dts_ehc_diagnostics), INTENT(INOUT) :: ehcDiag
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
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_A ! Base value for QF per unit population density [W m-2 (Cap ha-1)-1]
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_B ! Parameter related to cooling degree days [W m-2 K-1 (Cap ha-1 )-1]
      REAL(KIND(1D0)), DIMENSION(2) :: Qf_C ! Parameter related to heating degree days [W m-2 K-1 (Cap ha-1 )-1]

      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Heating ! base temperatrue for heating degree day [degC]
      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Cooling ! base temperature for cooling degree day [degC]

      REAL(KIND(1D0)), DIMENSION(2) :: TrafficRate ! Traffic rate [veh km m-2 s-1]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: AHProf_24hr ! diurnal profile of anthropogenic heat flux (AVERAGE of the multipliers is equal to 1) [-]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: HumActivity_24hr ! diurnal profile of human activity [-]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: TraffProf_24hr ! diurnal profile of traffic activity calculation[-]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: PopProf_24hr ! diurnal profile of population [-]

      REAL(KIND(1D0)) :: TrafficUnits ! traffic units choice [-]
      REAL(KIND(1D0)) :: EF_umolCO2perJ
      REAL(KIND(1D0)) :: EnEF_v_Jkm
      REAL(KIND(1D0)), DIMENSION(2) :: FcEF_v_kgkm
      REAL(KIND(1D0)) :: FrFossilFuel_Heat
      REAL(KIND(1D0)) :: FrFossilFuel_NonHeat
      REAL(KIND(1D0)) :: MaxFCMetab
      REAL(KIND(1D0)) :: MinFCMetab
      REAL(KIND(1D0)) :: CO2PointSource
      INTEGER :: emissions_sub
      LOGICAL :: calc_co2
      LOGICAL :: use_detailed_qf

      INTEGER, PARAMETER :: notUsedI = -999
      REAL(KIND(1D0)), PARAMETER :: notUsed = -999
      REAL(KIND(1D0)) :: Tair !ambient air temperature [degC]

      ASSOCIATE ( &
         anthroEmisState => modState%anthroEmisState, &
         heatState => modState%heatState, &
         atmstate => modState%atmstate &
         )

         ASSOCIATE ( &
            dayofWeek_id => clock%dayofWeek_id_st, &
            DLS => clock%DLS, &
            ahemisPrm => siteInfo%anthroemis &
         &)

            ASSOCIATE ( &
               EmissionsMethod => config%EmissionsMethod, &
               RSLLevel => config%RSLLevel, &
               MaxQFMetab => ahemisPrm%max_qf_metab, &
               MinQFMetab => ahemisPrm%min_qf_metab, &
               PopDensNighttime => ahemisPrm%anthro_heat%pop_density_nighttime, &
               SurfaceArea => siteInfo%SurfaceArea, &
               HDD_id => anthroEmisState%HDD_id, &
               imin => clock%imin_st, &
               it => clock%it_st, &
               Fc_anthro => anthroEmisState%Fc_anthro, &
               Fc_build => anthroEmisState%Fc_build, &
               Fc_metab => anthroEmisState%Fc_metab, &
               Fc_point => anthroEmisState%Fc_point, &
               Fc_traff => anthroEmisState%Fc_traff, &
               QF => heatState%QF, &
               QF_SAHP => heatState%QF_SAHP, &
               T2_c => atmstate%t2_C, &
               T_half_bldg_C => atmState%T_half_bldg_C, &
               Temp_C => forcing%Temp_C, &
               QF_obs => forcing%QF_obs &
               )

               IF (EmissionsMethod == 0) THEN ! use observed qf
                  qf = QF_obs
                  ! Observed QF carries no split by source. Fallback: the
                  ! site's building energy-use fraction of the base QF,
                  ! QF0_BEU (weekday/weekend), applied to the whole observed
                  ! value; it omits the heating and cooling terms that the
                  ! modelled methods attribute to buildings, so it is likely
                  ! low in winter. Diagnostic only (EHC output group).
                  IF (config%StorageHeatMethod == SHM_EHC) THEN
                     QF0_BEU(1) = ahemisPrm%anthro_heat%qf0_beu_working
                     QF0_BEU(2) = ahemisPrm%anthro_heat%qf0_beu_holiday
                     ehcDiag%qf_build = QF0_BEU(cal_weekday_index(DayofWeek_id(1)))*qf
                  END IF
               ELSEIF ((EmissionsMethod > 0 .AND. EmissionsMethod <= 6) .OR. EmissionsMethod >= 11) THEN
                  emissions_sub = MOD(EmissionsMethod, 10)
                  calc_co2 = (EmissionsMethod >= 11)
                  use_detailed_qf = (emissions_sub >= 4 .AND. emissions_sub <= 6)

                  ! Zero-initialise optional sub-method inputs so inactive
                  ! fields are never read before entering AnthropogenicEmissions (#2363).
                  AH_MIN = 0.0D0
                  AH_SLOPE_Heating = 0.0D0
                  AH_SLOPE_Cooling = 0.0D0
                  BaseT_Heating = 0.0D0
                  BaseT_Cooling = 0.0D0
                  Qf_A = 0.0D0
                  Qf_B = 0.0D0
                  Qf_C = 0.0D0
                  TrafficRate = 0.0D0
                  TraffProf_24hr = 0.0D0
                  TrafficUnits = 0.0D0
                  EF_umolCO2perJ = 0.0D0
                  EnEF_v_Jkm = 0.0D0
                  FcEF_v_kgkm = 0.0D0
                  FrFossilFuel_Heat = 0.0D0
                  FrFossilFuel_NonHeat = 0.0D0
                  MaxFCMetab = 0.0D0
                  MinFCMetab = 0.0D0
                  CO2PointSource = 0.0D0

                  ! Parameters shared across all modelled emissions sub-methods (1..6, 11..46)
                  PopDensDaytime(1) = ahemisPrm%anthro_heat%pop_density_daytime_working
                  PopDensDaytime(2) = ahemisPrm%anthro_heat%pop_density_daytime_holiday
                  QF0_BEU(1) = ahemisPrm%anthro_heat%qf0_beu_working
                  QF0_BEU(2) = ahemisPrm%anthro_heat%qf0_beu_holiday
                  AHProf_24hr(:, 1) = ahemisPrm%anthro_heat%ahprof_24hr_working
                  AHProf_24hr(:, 2) = ahemisPrm%anthro_heat%ahprof_24hr_holiday
                  HumActivity_24hr(:, 1) = ahemisPrm%hum_activity_24hr_working
                  HumActivity_24hr(:, 2) = ahemisPrm%hum_activity_24hr_holiday
                  PopProf_24hr(:, 1) = ahemisPrm%anthro_heat%popprof_24hr_working
                  PopProf_24hr(:, 2) = ahemisPrm%anthro_heat%popprof_24hr_holiday

                  ! Sub-method-specific QF temperature-response parameters
                  IF (emissions_sub == 1 .OR. emissions_sub == 4) THEN
                     AH_MIN(1) = ahemisPrm%anthro_heat%ah_min_working
                     AH_MIN(2) = ahemisPrm%anthro_heat%ah_min_holiday
                     AH_SLOPE_Heating(1) = ahemisPrm%anthro_heat%ah_slope_heating_working
                     AH_SLOPE_Heating(2) = ahemisPrm%anthro_heat%ah_slope_heating_holiday
                     BaseT_Heating(1) = ahemisPrm%anthro_heat%baset_heating_working
                     BaseT_Heating(2) = ahemisPrm%anthro_heat%baset_heating_holiday
                  ELSEIF (emissions_sub == 2 .OR. emissions_sub == 5) THEN
                     Qf_A(1) = ahemisPrm%anthro_heat%qf_a_working
                     Qf_A(2) = ahemisPrm%anthro_heat%qf_a_holiday
                     Qf_B(1) = ahemisPrm%anthro_heat%qf_b_working
                     Qf_B(2) = ahemisPrm%anthro_heat%qf_b_holiday
                     Qf_C(1) = ahemisPrm%anthro_heat%qf_c_working
                     Qf_C(2) = ahemisPrm%anthro_heat%qf_c_holiday
                  ELSEIF (emissions_sub == 3 .OR. emissions_sub == 6) THEN
                     AH_MIN(1) = ahemisPrm%anthro_heat%ah_min_working
                     AH_MIN(2) = ahemisPrm%anthro_heat%ah_min_holiday
                     AH_SLOPE_Heating(1) = ahemisPrm%anthro_heat%ah_slope_heating_working
                     AH_SLOPE_Heating(2) = ahemisPrm%anthro_heat%ah_slope_heating_holiday
                     AH_SLOPE_Cooling(1) = ahemisPrm%anthro_heat%ah_slope_cooling_working
                     AH_SLOPE_Cooling(2) = ahemisPrm%anthro_heat%ah_slope_cooling_holiday
                     BaseT_Heating(1) = ahemisPrm%anthro_heat%baset_heating_working
                     BaseT_Heating(2) = ahemisPrm%anthro_heat%baset_heating_holiday
                     BaseT_Cooling(1) = ahemisPrm%anthro_heat%baset_cooling_working
                     BaseT_Cooling(2) = ahemisPrm%anthro_heat%baset_cooling_holiday
                  END IF

                  ! Traffic statistics are only consumed by detailed QF sub-methods (4..6)
                  IF (use_detailed_qf) THEN
                     TrafficRate(1) = ahemisPrm%traffic_rate_working
                     TrafficRate(2) = ahemisPrm%traffic_rate_holiday
                     TraffProf_24hr(:, 1) = ahemisPrm%traff_prof_24hr_working
                     TraffProf_24hr(:, 2) = ahemisPrm%traff_prof_24hr_holiday
                     TrafficUnits = ahemisPrm%traffic_units
                  END IF

                  ! EnEF_v_Jkm is consumed by detailed QF (4..6) and by simple CO2 (11..43)
                  IF (use_detailed_qf .OR. calc_co2) THEN
                     EnEF_v_Jkm = ahemisPrm%en_ef_v_jkm
                  END IF

                  ! CO2-specific parameters are only consumed when EmissionsMethod >= 11
                  IF (calc_co2) THEN
                     EF_umolCO2perJ = ahemisPrm%ef_umol_co2_per_j
                     FcEF_v_kgkm = ahemisPrm%fc_ef_v_kgkm
                     FrFossilFuel_Heat = ahemisPrm%fr_fossil_fuel_heat
                     FrFossilFuel_NonHeat = ahemisPrm%fr_fossil_fuel_non_heat
                     MaxFCMetab = ahemisPrm%max_fc_metab
                     MinFCMetab = ahemisPrm%min_fc_metab
                     CO2PointSource = siteInfo%CO2PointSource
                  END IF

                  ! choose temperature for anthropogenic heat flux calculation
                  Tair = MERGE(T_half_bldg_C, MERGE(T2_C, Temp_C, RSLLevel == 1), RSLLevel == 2)

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
                     AHProf_24hr, HumActivity_24hr, TraffProf_24hr, PopProf_24hr, SurfaceArea, &
                     modState, ehcDiag%qf_build)
                  IF (supy_error_flag()) RETURN

               ELSE
                  CALL ErrorHint(73, 'RunControl.nml:EmissionsMethod unusable', notUsed, notUsed, EmissionsMethod, modState)
                  IF (supy_error_flag()) RETURN
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

!==============BIOGENIC CO2 flux==================================================
   SUBROUTINE SUEWS_cal_BiogenCO2( &
      clock, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_SITE, LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               SUEWS_CONFIG, CONDUCTANCE_PRM, SUEWS_FORCING, &
                               SUEWS_CLOCK, PHENOLOGY_STATE, SNOW_STATE, atm_state, &
                               anthroEmis_STATE, HYDRO_STATE, SUEWS_STATE
      USE module_ctrl_const_allocate, ONLY: nsurf, NVegSurf, &
                               ivConif, ivDecid, ivGrass, &
                               ConifSurf, DecidSurf, GrassSurf, BSoilSurf
      USE module_phys_biogenco2, ONLY: CO2_biogen, is_valid_bioco2_val
      USE module_phys_resist, ONLY: SurfaceResistance
      USE module_phys_atmmoiststab, ONLY: cal_AtmMoist

      IMPLICIT NONE

      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INout) :: modState

      REAL(KIND(1D0)) :: gfunc2 !gdq*gtemp*gs*gq for photosynthesis calculations (With modelled 2 meter temperature)
      REAL(KIND(1D0)) :: gfunc_use ! conductance function for biogenic CO2 calculations [-]
      REAL(KIND(1D0)) :: dq !Specific humidity deficit [g/kg]
      REAL(KIND(1D0)) :: t2 !air temperature at 2m [degC]
      REAL(KIND(1D0)) :: Tair_local ! air temperature for biogenic CO2 [degC]
      REAL(KIND(1D0)) :: dummy1 !Latent heat of vaporization in [J kg-1]
      REAL(KIND(1D0)) :: dummy2 !Latent heat of sublimation in J/kg
      REAL(KIND(1D0)) :: dummy3 !Saturation vapour pressure over water[hPa]
      REAL(KIND(1D0)) :: dummy4 !Vapour pressure of water[hpa]
      REAL(KIND(1D0)) :: dummy5 !vapour pressure deficit[hpa]
      REAL(KIND(1D0)) :: dummy6 !vapour pressure deficit[pa]
      REAL(KIND(1D0)) :: dummy7 !Vap density or absolute humidity [kg m-3]
      REAL(KIND(1D0)) :: dummy8 !specific heat capacity [J kg-1 K-1]
      REAL(KIND(1D0)) :: dummy9 !Air density [kg m-3]
      ! Unused outputs from SurfaceResistance (distinct variables to avoid compiler warnings)
      REAL(KIND(1D0)) :: unused_gs      ! Surface conductance [mm s-1]
      REAL(KIND(1D0)) :: unused_gc1     ! Conductance component 1 [mm s-1]
      REAL(KIND(1D0)) :: unused_gc2     ! Conductance component 2 [mm s-1]
      REAL(KIND(1D0)) :: unused_gc3     ! Conductance component 3 [mm s-1]
      REAL(KIND(1D0)) :: unused_gc4     ! Conductance component 4 [mm s-1]
      REAL(KIND(1D0)) :: unused_gc5     ! Conductance component 5 [mm s-1]
      REAL(KIND(1D0)) :: unused_rs      ! Surface resistance [s m-1]
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: alpha_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: alpha_enh_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: beta_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: beta_enh_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: LAIMin
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: LAIMax
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: min_res_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: resp_a
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: resp_b
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: theta_bioCO2
      REAL(KIND(1D0)), DIMENSION(NVegSurf) :: MaxConductance

      ASSOCIATE ( &
         atmState => modState%atmState, &
         phenState => modState%phenState, &
         snowState => modState%snowState, &
         hydroState => modState%hydroState, &
         anthroEmisState => modState%anthroEmisState &
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
            avkdn => forcing%kdown, &
            xsmd => forcing%xsmd, &
            Temp_C => forcing%Temp_C, &
            avRH => forcing%RH, &
            Press_hPa => forcing%pres, &
            t2_C => atmState%t2_C, &
            T_half_bldg_C => atmState%T_half_bldg_C, &
            LAI_id => phenState%LAI_id, &
            gfunc => phenState%g_func, &
            vsmd => hydroState%vsmd, &
            id => clock%timer%id, &
            it => clock%timer%it, &
            dectime => clock%dectime, &
            Fc_anthro => anthroEmisState%Fc_anthro, &
            Fc => anthroEmisState%Fc, &
            Fc_biogen => anthroEmisState%Fc_biogen, &
            Fc_photo => anthroEmisState%Fc_photo, &
            Fc_respi => anthroEmisState%Fc_respi, &
            SnowFrac => snowState%snow_fraction, &
            SMDMethod => config%SMDMethod, &
            EmissionsMethod => config%EmissionsMethod, &
            RSLLevel => config%RSLLevel, &
            Diagnose => config%Diagnose &
            )

            ASSOCIATE ( &
               G_max => conductancePrm%g_max, &
               G_k => conductancePrm%g_k, &
               G_q_base => conductancePrm%g_q_base, &
               G_q_shape => conductancePrm%g_q_shape, &
               G_t => conductancePrm%g_t, &
               G_sm => conductancePrm%g_sm, &
               gsmodel => conductancePrm%gs_model, &
               Kmax => conductancePrm%k_max, &
               S1 => conductancePrm%S1, &
               S2 => conductancePrm%S2, &
               TH => conductancePrm%TH, &
               TL => conductancePrm%TL &
               )

               IF (EmissionsMethod >= 11) THEN
                  ! Copy biogenic CO2 and vegetation parameters only when CO2
                  ! calculations are active and only for present vegetation
                  ! surfaces (sfr_surf > 0) (#2363).
                  alpha_bioCO2 = 0.0D0
                  alpha_enh_bioCO2 = 0.0D0
                  beta_bioCO2 = 0.0D0
                  beta_enh_bioCO2 = 0.0D0
                  LAIMin = 0.0D0
                  LAIMax = 0.0D0
                  min_res_bioCO2 = 0.0D0
                  resp_a = 0.0D0
                  resp_b = 0.0D0
                  theta_bioCO2 = 0.0D0
                  MaxConductance = 0.0D0

                  IF (sfr_surf(ConifSurf) > 0.0D0) THEN
                     alpha_bioCO2(ivConif) = evetrPrm%bioco2%alpha_bioco2
                     alpha_enh_bioCO2(ivConif) = evetrPrm%bioco2%alpha_enh_bioco2
                     beta_bioCO2(ivConif) = evetrPrm%bioco2%beta_bioCO2
                     beta_enh_bioCO2(ivConif) = evetrPrm%bioco2%beta_enh_bioCO2
                     LAIMin(ivConif) = evetrPrm%lai%lai_min
                     LAIMax(ivConif) = evetrPrm%lai%lai_max
                     min_res_bioCO2(ivConif) = evetrPrm%bioco2%min_res_bioCO2
                     resp_a(ivConif) = evetrPrm%bioco2%resp_a
                     resp_b(ivConif) = evetrPrm%bioco2%resp_b
                     theta_bioCO2(ivConif) = evetrPrm%bioco2%theta_bioCO2
                     MaxConductance(ivConif) = evetrPrm%max_conductance
                  ELSE IF (EmissionsMethod >= 31 .AND. EmissionsMethod <= 36) THEN
                     ! General Bellucco model (31..36): pass valid non-sentinel
                     ! coefficients on absent surfaces to preserve legacy
                     ! normalisation, or -999 sentinel when absent/poisoned.
                     IF (is_valid_bioco2_val(evetrPrm%bioco2%alpha_bioco2) .AND. &
                         is_valid_bioco2_val(evetrPrm%bioco2%beta_bioCO2) .AND. &
                         is_valid_bioco2_val(evetrPrm%bioco2%theta_bioCO2) .AND. &
                         is_valid_bioco2_val(evetrPrm%bioco2%alpha_enh_bioco2) .AND. &
                         is_valid_bioco2_val(evetrPrm%bioco2%beta_enh_bioco2)) THEN
                        alpha_bioCO2(ivConif) = evetrPrm%bioco2%alpha_bioco2
                        alpha_enh_bioCO2(ivConif) = evetrPrm%bioco2%alpha_enh_bioco2
                        beta_bioCO2(ivConif) = evetrPrm%bioco2%beta_bioCO2
                        beta_enh_bioCO2(ivConif) = evetrPrm%bioco2%beta_enh_bioCO2
                        theta_bioCO2(ivConif) = evetrPrm%bioco2%theta_bioCO2
                     ELSE
                        alpha_bioCO2(ivConif) = -999.0D0
                        alpha_enh_bioCO2(ivConif) = -999.0D0
                        beta_bioCO2(ivConif) = -999.0D0
                        beta_enh_bioCO2(ivConif) = -999.0D0
                        theta_bioCO2(ivConif) = -999.0D0
                     END IF
                  END IF
                  IF (sfr_surf(DecidSurf) > 0.0D0) THEN
                     alpha_bioCO2(ivDecid) = dectrPrm%bioco2%alpha_bioco2
                     alpha_enh_bioCO2(ivDecid) = dectrPrm%bioco2%alpha_enh_bioco2
                     beta_bioCO2(ivDecid) = dectrPrm%bioco2%beta_bioCO2
                     beta_enh_bioCO2(ivDecid) = dectrPrm%bioco2%beta_enh_bioCO2
                     LAIMin(ivDecid) = dectrPrm%lai%lai_min
                     LAIMax(ivDecid) = dectrPrm%lai%lai_max
                     min_res_bioCO2(ivDecid) = dectrPrm%bioco2%min_res_bioCO2
                     resp_a(ivDecid) = dectrPrm%bioco2%resp_a
                     resp_b(ivDecid) = dectrPrm%bioco2%resp_b
                     theta_bioCO2(ivDecid) = dectrPrm%bioco2%theta_bioCO2
                     MaxConductance(ivDecid) = dectrPrm%max_conductance
                  ELSE IF (EmissionsMethod >= 31 .AND. EmissionsMethod <= 36) THEN
                     IF (is_valid_bioco2_val(dectrPrm%bioco2%alpha_bioco2) .AND. &
                         is_valid_bioco2_val(dectrPrm%bioco2%beta_bioCO2) .AND. &
                         is_valid_bioco2_val(dectrPrm%bioco2%theta_bioCO2)) THEN
                        alpha_bioCO2(ivDecid) = dectrPrm%bioco2%alpha_bioco2
                        beta_bioCO2(ivDecid) = dectrPrm%bioco2%beta_bioCO2
                        theta_bioCO2(ivDecid) = dectrPrm%bioco2%theta_bioCO2
                     ELSE
                        alpha_bioCO2(ivDecid) = -999.0D0
                        beta_bioCO2(ivDecid) = -999.0D0
                        theta_bioCO2(ivDecid) = -999.0D0
                     END IF
                  END IF
                  IF (sfr_surf(GrassSurf) > 0.0D0) THEN
                     alpha_bioCO2(ivGrass) = grassPrm%bioco2%alpha_bioco2
                     alpha_enh_bioCO2(ivGrass) = grassPrm%bioco2%alpha_enh_bioco2
                     beta_bioCO2(ivGrass) = grassPrm%bioco2%beta_bioCO2
                     beta_enh_bioCO2(ivGrass) = grassPrm%bioco2%beta_enh_bioCO2
                     LAIMin(ivGrass) = grassPrm%lai%lai_min
                     LAIMax(ivGrass) = grassPrm%lai%lai_max
                     min_res_bioCO2(ivGrass) = grassPrm%bioco2%min_res_bioCO2
                     resp_a(ivGrass) = grassPrm%bioco2%resp_a
                     resp_b(ivGrass) = grassPrm%bioco2%resp_b
                     theta_bioCO2(ivGrass) = grassPrm%bioco2%theta_bioCO2
                     MaxConductance(ivGrass) = grassPrm%max_conductance
                  ELSE IF (EmissionsMethod >= 31 .AND. EmissionsMethod <= 36) THEN
                     IF (is_valid_bioco2_val(grassPrm%bioco2%alpha_bioco2) .AND. &
                         is_valid_bioco2_val(grassPrm%bioco2%beta_bioCO2) .AND. &
                         is_valid_bioco2_val(grassPrm%bioco2%theta_bioCO2)) THEN
                        alpha_bioCO2(ivGrass) = grassPrm%bioco2%alpha_bioco2
                        beta_bioCO2(ivGrass) = grassPrm%bioco2%beta_bioCO2
                        theta_bioCO2(ivGrass) = grassPrm%bioco2%theta_bioCO2
                     ELSE
                        alpha_bioCO2(ivGrass) = -999.0D0
                        beta_bioCO2(ivGrass) = -999.0D0
                        theta_bioCO2(ivGrass) = -999.0D0
                     END IF
                  END IF

                  ! Initialise conductance functions for biogenic CO2 calculations
                  gfunc_use = gfunc
                  gfunc2 = gfunc_use

                  ! Select local air temperature for biogenic CO2 calculations
                  Tair_local = MERGE(T_half_bldg_C, MERGE(t2_C, Temp_C, RSLLevel == 1), RSLLevel == 2)

                  IF ((gsmodel == 1 .OR. gsmodel == 2) .AND. RSLLevel > 0) THEN
                     CALL cal_AtmMoist( &
                        Tair_local, Press_hPa, avRh, dectime, & ! input:
                        dummy1, dummy2, & ! output:
                        dummy3, dummy4, dummy5, dummy6, dq, dummy7, dummy8, dummy9)
                     CALL SurfaceResistance( &
                        id, it, & ! input:
                        SMDMethod, SnowFrac, sfr_surf, avkdn, Tair_local, dq, xsmd, vsmd, MaxConductance, &
                        LAIMax, LAI_id, gsModel, Kmax, &
                        G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, TH, TL, S1, S2, &
                        unused_gc1, unused_gc2, unused_gc3, unused_gc4, unused_gc5, & ! output: (unused conductances)
                        gfunc_use, unused_gs, unused_rs, & ! output:
                        modState)
                     IF (supy_error_flag()) RETURN
                  END IF

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
                        dummy3, dummy4, dummy5, dummy6, dq, dummy7, dummy8, dummy9, &
                        modState)
                     ! Surface resistance calculation for gfunc2
                     CALL SurfaceResistance( &
                        id, it, & ! input:
                        SMDMethod, SnowFrac, sfr_surf, avkdn, t2, dq, xsmd, vsmd, MaxConductance, &
                        LAIMax, LAI_id, gsModel, Kmax, &
                        G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, TH, TL, S1, S2, &
                        unused_gc1, unused_gc2, unused_gc3, unused_gc4, unused_gc5, & ! output: (unused conductances)
                        gfunc2, unused_gs, unused_rs, & ! output:
                        modState)
                     IF (supy_error_flag()) RETURN
                  ELSEIF ((gsmodel == 1 .OR. gsmodel == 2) .AND. RSLLevel > 0) THEN
                     ! Use local temperature for gsmodel 1/2 with RSL diagnostics
                     t2 = Tair_local
                  ELSE
                     ! Use forcing temperature (RSLLevel=0 or unrecognised gsmodel)
                     t2 = Temp_C
                  END IF

                  ! Calculate CO2 fluxes from biogenic components
                  IF (Diagnose == 1) WRITE (*, *) 'Calling CO2_biogen...'
                  CALL CO2_biogen( &
                     alpha_bioCO2, alpha_enh_bioCO2, avkdn, beta_bioCO2, beta_enh_bioCO2, BSoilSurf, & ! input:
                     ConifSurf, DecidSurf, dectime, EmissionsMethod, gfunc_use, gfunc2, GrassSurf, gsmodel, &
                     id, it, ivConif, ivDecid, ivGrass, LAI_id, LAIMin, LAIMax, min_res_bioCO2, nsurf, &
                     NVegSurf, resp_a, resp_b, sfr_surf, SnowFrac, t2, theta_bioCO2, &
                     Fc_biogen, Fc_photo, Fc_respi) ! output:
               ELSE
                  ! EmissionsMethod 0-10: No biogenic CO2 calculations
                  Fc_biogen = 0
                  Fc_photo = 0
                  Fc_respi = 0
               END IF

               ! Calculate total CO2 flux
               Fc = Fc_anthro + Fc_biogen

            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_BiogenCO2
!========================================================================

!=============net all-wave radiation=====================================

   SUBROUTINE SUEWS_cal_Qn( &
      clock, config, forcing, siteInfo, & ! input
      modState, & ! input/output:
      dataOutLineSPARTACUS) ! output
      USE module_phys_narp, ONLY: RadMethod, NARP
      USE module_phys_spartacus, ONLY: SPARTACUS, split_shortwave_perez, split_shortwave_forcing, split_shortwave_reindl
      USE module_ctrl_type, ONLY: SUEWS_SITE, SUEWS_CLOCK, SUEWS_CONFIG, SUEWS_FORCING
      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_CLOCK, SNOW_STATE, SNOW_PRM, &
                               SUEWS_FORCING, SUEWS_SITE, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               PHENOLOGY_STATE, SPARTACUS_PRM, &
                               SPARTACUS_LAYER_PRM, HEAT_STATE, SUEWS_STATE

      IMPLICIT NONE
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      REAL(KIND(1D0)), DIMENSION(nsurf) :: emis ! Effective surface emissivity. [-]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: alb ! surface albedo [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac ! snow fractions of each surface [-]
      REAL(KIND(1D0)) :: albedo_snowfree !estimated albedo for snow-free surface [-]
      REAL(KIND(1D0)) :: SnowAlb ! updated snow albedo [-]
      REAL(KIND(1D0)) :: kdown_direct ! direct-horizontal irradiance [W m-2]
      REAL(KIND(1D0)) :: kdown_diffuse ! diffuse-horizontal irradiance [W m-2]
      LOGICAL :: forcing_split_valid

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

      ! Safe selection of roof/wall surface temperatures for radiation calc.
      ! Cannot use MERGE(buildings(1)%Textroof_C, ...) because MERGE
      ! evaluates both arguments and the allocatable array may be null
      ! before gen_building runs in SUEWS_cal_Qs (gfortran 14+ segfaults).
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof_local, tsfc_wall_local
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_surf_storage

      ASSOCIATE ( &
         solarState => modState%solarState, &
         atmState => modState%atmState, &
         phenState => modState%phenState, &
         snowState => modState%snowState, &
         heatState => modState%heatState, &
         ohmState => modState%ohmState, &
         stebbsState => modState%stebbsState &
         )

         ! Reject StorageHeatMethod == SHM_STEBBS when STEBBS is disabled
         ! consistently with SUEWS_cal_Qs before consuming STEBBS state (#2363).
         IF (config%StorageHeatMethod == SHM_STEBBS .AND. &
             config%stebbsmethod /= STEBBS_DEFAULT .AND. &
             config%stebbsmethod /= STEBBS_PROVIDED) THEN
            dataOutLineSPARTACUS = -999.0D0
            CALL set_supy_error( &
               106, &
               'SUEWS_cal_Qn: StorageHeatMethod == SHM_STEBBS (7) requires an active STEBBSmethod (1 or 2)')
            RETURN
         END IF

         ! Safely resolve roof/wall surface temps without MERGE on
         ! possibly-unallocated or zero-length allocatable arrays.
         IF (config%StorageHeatMethod == SHM_STEBBS &
             .AND. (config%stebbsmethod == STEBBS_DEFAULT .OR. config%stebbsmethod == STEBBS_PROVIDED) &
             .AND. ALLOCATED(stebbsState%buildings)) THEN
            IF (SIZE(stebbsState%buildings) >= 1) THEN
               IF (ALLOCATED(stebbsState%buildings(1)%Textroof_C)) THEN
                  tsfc_roof_local = stebbsState%buildings(1)%Textroof_C
               ELSE
                  tsfc_roof_local = heatState%tsfc_roof
               END IF
               IF (ALLOCATED(stebbsState%buildings(1)%Textwall_C)) THEN
                  tsfc_wall_local = stebbsState%buildings(1)%Textwall_C
               ELSE
                  tsfc_wall_local = heatState%tsfc_wall
               END IF
            ELSE
               tsfc_roof_local = heatState%tsfc_roof
               tsfc_wall_local = heatState%tsfc_wall
            END IF
         ELSE
            tsfc_roof_local = heatState%tsfc_roof
            tsfc_wall_local = heatState%tsfc_wall
         END IF
         tsfc_surf_storage = heatState%tsfc_surf
         IF (config%StorageHeatMethod == SHM_DYOHM .OR. config%StorageHeatMethod == SHM_STEBBS) THEN
            tsfc_surf_storage = heatState%tsfc_surf_dyohm
         END IF

         ASSOCIATE ( &
            alb_prev => phenState%alb, &
            albDecTr_id => phenState%albDecTr_id, &
            albEveTr_id => phenState%albEveTr_id, &
            albGrass_id => phenState%albGrass_id, &
            LAI_id => phenState%LAI_id, &
            storageheatmethod => config%StorageHeatMethod, &
            NetRadiationMethod => config%NetRadiationMethod, &
            KdownSplitMethod => config%KdownSplitMethod, &
            SnowUse => config%SnowUse, &
            Diagnose => config%Diagnose, &
            use_sw_direct_albedo => config%use_sw_direct_albedo, &
            id => clock%timer%id, &
            tstep => clock%timer%tstep, &
            ldown_obs => forcing%l_down, &
            fcld_obs => forcing%f_cloud, &
            kdown => forcing%kdown, &
            Tair_C => forcing%Temp_C, &
            avRH => forcing%RH, &
            Press_hPa => forcing%pres, &
            qn1_obs => forcing%qn1_obs, &
            SnowPack_prev => snowState%snow_pack, &
            SnowAlb_prev => snowState%snow_albedo, &
            snowFrac_prev => snowState%snow_fraction, &
            IceFrac => snowState%ice_frac, &
            qn_snow => snowState%qn_snow, &
            qn_ind_snow => snowState%qn_ind_snow, &
            kup_ind_snow => snowState%kup_ind_snow, &
            Tsurf_ind_snow => snowState%Tsurf_ind_snow, &
            dectime => clock%dectime, &
            ZENITH_deg => solarState%zenith_deg, &
            ea_hPa => atmState%ea_hPa, &
            fcld => atmState%f_cloud, &
            qn => heatState%qn, &
            kclear => heatState%k_clear, &
            kup => heatState%k_up, &
            lup => heatState%l_up, &
            tsurf => heatState%tsurf, &
            qn_snowfree => heatState%qn_snowfree, &
            ldown => heatState%l_down, &
            qn_surf => heatState%qn_surf, &
            qn_roof => heatState%qn_roof, &
            qn_wall => heatState%qn_wall, &
            Tsurf_ind => heatState%Tsurf_ind, &
            rsl_z => stebbsState%z_array, &
            rsl_t_C => stebbsState%dataout_line_t_rsl, &
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
               n_vegetation_region_forest => spartacusPrm%n_vegetation_region_forest, &
               n_stream_sw_urban => spartacusPrm%n_stream_sw_urban, &
               n_stream_sw_forest => spartacusPrm%n_stream_sw_forest, &
               n_stream_lw_urban => spartacusPrm%n_stream_lw_urban, &
               n_stream_lw_forest => spartacusPrm%n_stream_lw_forest, &
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
               bldgH => bldgPrm%height_building, &
               sfr_evetr => evetrPrm%sfr, &
               sfr_dectr => dectrPrm%sfr, &
               EveTreeH => evetrPrm%height_evergreen_tree, &
               DecTreeH => dectrPrm%height_deciduous_tree, &
               veg_ext_input => spartacusLayerPrm%veg_ext, &
               tau_a => snowPrm%tau_a, &
               tau_f => snowPrm%tau_f, &
               SnowAlbMax => snowPrm%snow_albedo_max, &
               SnowAlbMin => snowPrm%snow_albedo_min, &
               NARP_EMIS_SNOW => snowPrm%NARP_EMIS_SNOW, &
               roof_in_sw_spc => heatState%roof_in_sw_spc, &
               roof_in_lw_spc => heatState%roof_in_lw_spc, &
               wall_in_sw_spc => heatState%wall_in_sw_spc, &
               wall_in_lw_spc => heatState%wall_in_lw_spc, &
               tsfc_surf => tsfc_surf_storage, &
               tsfc_roof => tsfc_roof_local, &
               tsfc_wall => tsfc_wall_local &
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
                     dectime, ZENITH_deg, kdown, Tair_C, avRH, ea_hPa, qn1_obs, ldown_obs, fcld_obs, &
                     SnowAlb, &
                     AlbedoChoice, ldown_option, NetRadiationMethod_use, DiagQN, &
                     qn_surf, & ! output:
                     qn, qn_snowfree, qn_snow, kclear, kup, LDown, lup, fcld, tsurf, & ! output:
                     qn_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_ind, albedo_snowfree)

                  IF (Diagqn == 1) WRITE (*, *) 'Calling SPARTACUS:'
                  IF (NetRadiationMethod > 1000) THEN
                     SELECT CASE (KdownSplitMethod)
                     CASE (1) ! observed kdir (direct-normal) and kdiff (diffuse-horizontal) forcing
                        CALL split_shortwave_forcing( &
                           kdown, forcing%kdiff, forcing%kdir, zenith_deg, &
                           kdown_direct, kdown_diffuse, forcing_split_valid)
                        IF (.NOT. forcing_split_valid) THEN
                           CALL split_shortwave_perez( &
                              kdown, id, zenith_deg, Tair_C, avRH, Press_hPa, &
                              sw_dn_direct_frac, kdown_direct)
                           kdown_diffuse = MAX(0.0D0, kdown - kdown_direct)
                           CALL modState%errorState%report( &
                              message='invalid kdir/kdiff forcing; using Perez Kdown split', &
                              location='SUEWS_cal_Qn', is_fatal=.FALSE.)
                        END IF
                     CASE (2) ! constant direct-horizontal fraction
                        kdown_direct = MAX(0.0D0, kdown) &
                                       *MAX(0.0D0, MIN(1.0D0, sw_dn_direct_frac))
                        kdown_diffuse = MAX(0.0D0, kdown) - kdown_direct
                     CASE (3) ! Perez/DISC static-DIRINT split
                        CALL split_shortwave_perez( &
                           kdown, id, zenith_deg, Tair_C, avRH, Press_hPa, &
                           sw_dn_direct_frac, kdown_direct)
                        kdown_diffuse = MAX(0.0D0, kdown - kdown_direct)
                     CASE (4) ! Reindl et al. (1990), using the BEERS diffuse correlation
                        CALL split_shortwave_reindl(kdown, id, zenith_deg, Tair_C, avRH, kdown_direct)
                        kdown_diffuse = MAX(0.0D0, kdown) - kdown_direct
                     CASE DEFAULT ! retain Perez fallback for unrecognised method values
                        CALL split_shortwave_perez( &
                           kdown, id, zenith_deg, Tair_C, avRH, Press_hPa, &
                           sw_dn_direct_frac, kdown_direct)
                        kdown_diffuse = MAX(0.0D0, kdown - kdown_direct)
                     END SELECT

                     ! TODO: TS 14 Feb 2022, ESTM development: introduce facet surface temperatures
                     CALL SPARTACUS( &
                        Diagqn, & !input:
                        sfr_surf, zenith_deg, nlayer, & !input:
                        tsfc_surf, tsfc_roof, tsfc_wall, &
                        kdown, kdown_direct, kdown_diffuse, ldown, Tair_C, rsl_z, rsl_t_C, alb, emis, LAI_id, &
                        n_vegetation_region_urban, &
                        n_vegetation_region_forest, &
                        n_stream_sw_urban, n_stream_sw_forest, n_stream_lw_urban, n_stream_lw_forest, &
                        air_ext_sw, air_ssa_sw, &
                        veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                        veg_fsd_const, veg_contact_fraction_const, &
                        ground_albedo_dir_mult_fact, use_sw_direct_albedo, &
                        height, bldgH, sfr_evetr, sfr_dectr, EveTreeH, DecTreeH, &
                        building_frac, veg_frac, sfr_roof, sfr_wall, &
                        building_scale, veg_scale, veg_ext_input, & !input:
                        alb_roof, emis_roof, alb_wall, emis_wall, &
                        roof_albedo_dir_mult_fact, wall_specular_frac, &
                        qn, kup, lup, qn_roof, qn_wall, qn_surf, & !output:
                        roof_in_sw_spc, roof_in_lw_spc, &
                        wall_in_sw_spc, wall_in_lw_spc, &
                        dataOutLineSPARTACUS, &
                        modState%errorState)
                     IF (supy_error_flag()) RETURN
                     IF (qn /= qn .OR. qn <= -999D0) THEN
                        WRITE (*, *) 'QN_DEBUG invalid qn after SPARTACUS:', &
                           ' netrad=', NetRadiationMethod, &
                           ' qn=', qn, &
                           ' qn_obs=', qn1_obs, &
                           ' kdown=', kdown, &
                           ' ldown=', ldown, &
                           ' tair=', Tair_C, &
                           ' qn_surf=', qn_surf
                     END IF
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
               snowState%snow_albedo = SnowAlb
            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_cal_Qn
!========================================================================

!=============storage heat flux=========================================

   SUBROUTINE SUEWS_cal_Qs( &
      clock, config, forcing, siteInfo, datetimeLine, & ! input
      modState, ehcDiag, & ! input/output:
      dataOutLineESTM, dataOutLineSTEBBS)

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_SITE, SUEWS_CLOCK, &
                               SNOW_STATE, EHC_PRM, &
                               anthroEmis_STATE, PHENOLOGY_STATE, OHM_STATE, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               HEAT_STATE, HYDRO_STATE, SUEWS_STATE
      USE module_ctrl_const_allocate, ONLY: ncolumnsDataOutSTEBBS
      IMPLICIT NONE

      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_CONFIG), INTENT(in) :: config
      TYPE(SUEWS_FORCING), INTENT(in) :: forcing
      TYPE(SUEWS_SITE), INTENT(in) :: siteInfo
      REAL(KIND(1D0)), DIMENSION(5), INTENT(in) :: datetimeLine !date & time
      TYPE(SUEWS_STATE), INTENT(inout) :: modState
      TYPE(dts_ehc_diagnostics), INTENT(inout) :: ehcDiag

      REAL(KIND(1D0)) :: OHM_coef(nsurf + 1, 4, 3) ! OHM coefficients [-]
      REAL(KIND(1D0)) :: OHM_threshSW(nsurf + 1) ! Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)) :: OHM_threshWD(nsurf + 1) ! Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]

      REAL(KIND(1D0)) :: SoilStoreCap(nsurf) ! capacity of soil store [mm]

      REAL(KIND(1D0)) :: state_id(nsurf) ! wetness status [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: emis ! emissivity [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: cpAnOHM ! heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kkAnOHM ! thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chAnOHM ! bulk transfer coef [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: moist_surf ! normalised soil moisture state [-]
      INTEGER :: is_anohm ! surface loop index for AnOHM moisture mapping

      REAL(KIND(1D0)), DIMENSION(27), INTENT(out) :: dataOutLineESTM !data output from ESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSTEBBS - 5), INTENT(out) :: dataOutLineSTEBBS !data output from STEBBS

      ! internal use arrays
      REAL(KIND(1D0)) :: Tair_mav_5d ! Tair_mav_5d=HDD(id-1,4) HDD at the begining of today (id-1)
      REAL(KIND(1D0)) :: qn_use ! qn used in OHM calculations [W m-2]
      ! Local copies avoid Fortran 2018 15.5.2.13 INTENT(IN)/INTENT(OUT) aliasing in OHM and AnOHM
      REAL(KIND(1D0)) :: qn_av_prev_ohm, dqndt_prev_ohm
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn_surfs_prev_ohm, dqndt_surf_prev_ohm
      REAL(KIND(1D0)) :: qn_s_av_prev_ohm, dqnsdt_prev_ohm
      CHARACTER(LEN=12) :: str_storage_heat ! StorageHeatMethod as text for the error message
      ! EHC implicit surface coupling: linearised surface energy balance,
      ! lower-boundary temperatures and the solved surface temperatures
      TYPE(dts_ehc_implicit) :: ehc_seb

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
            snowPrm => siteInfo%snow, &
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
            qs_obs => forcing%qs_obs, &
            qf => heatState%qf, &
            qn => heatState%qn, &
            qn_surf => heatState%qn_surf, &
            qs => heatState%qs, &
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
            deltaQi => snowState%delta_qi, &
            SnowFrac => snowState%snow_fraction, &
            soilstore_id => hydroState%soil_store_surf, &
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
            StoreDrainPrm => phenState%storage_drain_params, &
            id => clock%timer%id, &
            tstep => clock%timer%tstep, &
            dt_since_start => clock%timer%dt_since_start &
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
               dz_surf => ehcPrm%dz_surf &
               )

               ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
               IF (storageheatmethod /= SHM_DYOHM .AND. storageheatmethod /= SHM_STEBBS) THEN
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
               ! Snow OHM coefficients (surface index nsurf + 1 = 8).
               ! Keep outside the DyOHM guard above so snow storage heat flux
               ! deltaQi uses snowPrm%ohm across all OHM-calling methods.
               ! OHM evaluates deltaQi from the winter_wet regime (8, 3, 1:3).
               OHM_coef(8, 1, 1) = snowPrm%ohm%ohm_coef_lc(1)%summer_wet
               OHM_coef(8, 2, 1) = snowPrm%ohm%ohm_coef_lc(1)%summer_dry
               OHM_coef(8, 3, 1) = snowPrm%ohm%ohm_coef_lc(1)%winter_wet
               OHM_coef(8, 4, 1) = snowPrm%ohm%ohm_coef_lc(1)%winter_dry

               OHM_coef(8, 1, 2) = snowPrm%ohm%ohm_coef_lc(2)%summer_wet
               OHM_coef(8, 2, 2) = snowPrm%ohm%ohm_coef_lc(2)%summer_dry
               OHM_coef(8, 3, 2) = snowPrm%ohm%ohm_coef_lc(2)%winter_wet
               OHM_coef(8, 4, 2) = snowPrm%ohm%ohm_coef_lc(2)%winter_dry

               OHM_coef(8, 1, 3) = snowPrm%ohm%ohm_coef_lc(3)%summer_wet
               OHM_coef(8, 2, 3) = snowPrm%ohm%ohm_coef_lc(3)%summer_dry
               OHM_coef(8, 3, 3) = snowPrm%ohm%ohm_coef_lc(3)%winter_wet
               OHM_coef(8, 4, 3) = snowPrm%ohm%ohm_coef_lc(3)%winter_dry

               OHM_threshSW(1) = pavedPrm%ohm%ohm_threshsw
               OHM_threshSW(2) = bldgPrm%ohm%ohm_threshsw
               OHM_threshSW(3) = evetrPrm%ohm%ohm_threshsw
               OHM_threshSW(4) = dectrPrm%ohm%ohm_threshsw
               OHM_threshSW(5) = grassPrm%ohm%ohm_threshsw
               OHM_threshSW(6) = bsoilPrm%ohm%ohm_threshsw
               OHM_threshSW(7) = waterPrm%ohm%ohm_threshsw
               OHM_threshSW(8) = snowPrm%ohm%ohm_threshsw

               OHM_threshWD(1) = pavedPrm%ohm%ohm_threshwd
               OHM_threshWD(2) = bldgPrm%ohm%ohm_threshwd
               OHM_threshWD(3) = evetrPrm%ohm%ohm_threshwd
               OHM_threshWD(4) = dectrPrm%ohm%ohm_threshwd
               OHM_threshWD(5) = grassPrm%ohm%ohm_threshwd
               OHM_threshWD(6) = bsoilPrm%ohm%ohm_threshwd
               OHM_threshWD(7) = waterPrm%ohm%ohm_threshwd
               OHM_threshWD(8) = snowPrm%ohm%ohm_threshwd

               SoilStoreCap(1) = pavedPrm%soil%soil_store_capacity
               SoilStoreCap(2) = bldgPrm%soil%soil_store_capacity
               SoilStoreCap(3) = evetrPrm%soil%soil_store_capacity
               SoilStoreCap(4) = dectrPrm%soil%soil_store_capacity
               SoilStoreCap(5) = grassPrm%soil%soil_store_capacity
               SoilStoreCap(6) = bsoilPrm%soil%soil_store_capacity
               SoilStoreCap(7) = waterPrm%soil%soil_store_capacity

               emis(1) = pavedPrm%emis
               emis(2) = bldgPrm%emis
               emis(3) = evetrPrm%emis
               emis(4) = dectrPrm%emis
               emis(5) = grassPrm%emis
               emis(6) = bsoilPrm%emis
               emis(7) = waterPrm%emis

               cpAnOHM(1) = pavedPrm%ohm%cp_anohm
               cpAnOHM(2) = bldgPrm%ohm%cp_anohm
               cpAnOHM(3) = evetrPrm%ohm%cp_anohm
               cpAnOHM(4) = dectrPrm%ohm%cp_anohm
               cpAnOHM(5) = grassPrm%ohm%cp_anohm
               cpAnOHM(6) = bsoilPrm%ohm%cp_anohm
               cpAnOHM(7) = waterPrm%ohm%cp_anohm

               kkAnOHM(1) = pavedPrm%ohm%kk_anohm
               kkAnOHM(2) = bldgPrm%ohm%kk_anohm
               kkAnOHM(3) = evetrPrm%ohm%kk_anohm
               kkAnOHM(4) = dectrPrm%ohm%kk_anohm
               kkAnOHM(5) = grassPrm%ohm%kk_anohm
               kkAnOHM(6) = bsoilPrm%ohm%kk_anohm
               kkAnOHM(7) = waterPrm%ohm%kk_anohm

               chAnOHM(1) = pavedPrm%ohm%ch_anohm
               chAnOHM(2) = bldgPrm%ohm%ch_anohm
               chAnOHM(3) = evetrPrm%ohm%ch_anohm
               chAnOHM(4) = dectrPrm%ohm%ch_anohm
               chAnOHM(5) = grassPrm%ohm%ch_anohm
               chAnOHM(6) = bsoilPrm%ohm%ch_anohm
               chAnOHM(7) = waterPrm%ohm%ch_anohm

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
               IF (OHMIncQF == OHM_QF_INCLUDE) THEN
                  qn_use = qf + qn
               ELSEIF (OHMIncQF == OHM_QF_EXCLUDE) THEN
                  qn_use = qn
               END IF

               ! Reject StorageHeatMethod == SHM_STEBBS when STEBBS is disabled
               ! so stale or uninitialised QS_stebbs can never contaminate QS (#2363).
               IF (StorageHeatMethod == SHM_STEBBS .AND. &
                   config%stebbsmethod /= STEBBS_DEFAULT .AND. &
                   config%stebbsmethod /= STEBBS_PROVIDED) THEN
                  QS_surf = -999.0D0
                  QS_roof = -999.0D0
                  QS_wall = -999.0D0
                  CALL set_supy_error( &
                     106, &
                     'SUEWS_cal_Qs: StorageHeatMethod == SHM_STEBBS (7) requires an active STEBBSmethod (1 or 2)')
                  RETURN
               END IF

               !==============use STEBBS to get localised surface temperature and storage heat flux==================
               ! MP 12 Sep 2024: STEBBS is a simplified BEM
               IF (config%stebbsmethod == STEBBS_DEFAULT .OR. config%stebbsmethod == STEBBS_PROVIDED) THEN
                  IF (Diagnose == 1) WRITE (*, *) 'Calling STEBBS...'
                  CALL stebbsonlinecouple( &
                     clock, config, forcing, siteInfo, & ! input
                     modState, & ! input/output:
                     datetimeLine, nlayer, & ! input
                     dataOutLineSTEBBS) ! output
                  IF (supy_error_flag()) RETURN
               END IF

               IF (StorageHeatMethod == SHM_ANOHM) THEN
                  ! Promote any completed calendar-day buffer before AnOHM dispatch so
                  ! day-rollover steps diagnose the new day's coefficients consistently
                  ! on every pass and leave live coefficients in checkpoints.
                  CALL anohm_rollover_if_needed(clock, ohmState)
               END IF

               IF (StorageHeatMethod == SHM_OBSERVED) THEN !Use observed QS
                  qs = qs_obs

               ELSEIF (StorageHeatMethod == SHM_OHM .OR. StorageHeatMethod == SHM_DYOHM .OR. StorageHeatMethod == SHM_STEBBS .OR. &
                       StorageHeatMethod == SHM_DYOHM_BUILDING .OR. &
                       (StorageHeatMethod == SHM_ANOHM .AND. .NOT. ohmState%anohm_coeff_ready)) THEN !OHM, or AnOHM spin-up fallback
                  Tair_mav_5d = HDD_id(10)
                  IF (Diagnose == 1) WRITE (*, *) 'Calling OHM...'
                  qn_av_prev_ohm = ohmState%qn_av
                  dqndt_prev_ohm = ohmState%dqndt
                  qn_surfs_prev_ohm = ohmState%qn_surfs
                  dqndt_surf_prev_ohm = ohmState%dqndt_surf
                  qn_s_av_prev_ohm = ohmState%qn_s_av
                  dqnsdt_prev_ohm = ohmState%dqnsdt
                  CALL OHM(qn_use, qn_surf, qn_av_prev_ohm, dqndt_prev_ohm, &
                           ohmState%qn_av, ohmState%dqndt, &
                           qn_surfs_prev_ohm, dqndt_surf_prev_ohm, &
                           ohmState%qn_surfs, ohmState%dqndt_surf, &
                           qn_snow, qn_s_av_prev_ohm, dqnsdt_prev_ohm, &
                           ohmState%qn_s_av, ohmState%dqnsdt, &
                           tstep, dt_since_start, &
                           sfr_surf, nsurf, &
                           Tair_mav_5d, &
                           OHM_coef, &
                           OHM_threshSW, OHM_threshWD, &
                           soilstore_id, SoilStoreCap, state_id, &
                           BldgSurf, WaterSurf, &
                           SnowUse, SnowFrac, &
                           atmState%U_hbh, atmState%T_half_bldg_C, t2_prev, &
                           ws_rav, qn_rav, &
                           dz_surf, cp_surf, k_surf, &
                           lambda_c, &
                           StorageHeatMethod, DiagQS, clock, &
                           a1_bldg, a2_bldg, a3_bldg, &
                           a1_paved, a2_paved, a3_paved, &
                           a1_evetr, a2_evetr, a3_evetr, &
                           a1_dectr, a2_dectr, a3_dectr, &
                           a1_grass, a2_grass, a3_grass, &
                           a1_bsoil, a2_bsoil, a3_bsoil, &
                           a1_water, a2_water, a3_water, &
                           a1, a2, a3, qs, qs_surf, deltaQi, &
                           modState)
                  IF (StorageHeatMethod /= SHM_DYOHM .AND. StorageHeatMethod /= SHM_STEBBS .AND. StorageHeatMethod /= SHM_DYOHM_BUILDING) THEN
                     QS_surf = qs
                     QS_roof = qs
                     QS_wall = qs
                  ELSE
                     ! Methods 6, 7, and 8 preserve surface-specific storage heat fluxes.
                     IF (StorageHeatMethod == SHM_STEBBS) THEN !for method 7 when STEBBS is used for building
                        qs = qs + QS_stebbs * sfr_surf(2)
                        QS_surf(2) = QS_stebbs
                     END IF
                     ! Method 6 uses dyOHM for all surfaces.
                     ! Method 8 uses dyOHM for building storage heat flux only.
                     QS_roof = QS_surf(2)
                     QS_wall = QS_surf(2)

                  END IF

                  ! AnOHM (StorageHeatMethod=3): coefficients diagnosed from the
                  ! most recently completed day held in the calendar-day buffer, so no
                  ! future forcing data is required. During spin-up (buffer not
                  ! ready) the OHM branch above produces QS instead.
               ELSEIF (StorageHeatMethod == SHM_ANOHM) THEN
                  DO is_anohm = 1, nsurf
                     IF (StoreDrainPrm(6, is_anohm) > 0.0D0) THEN
                        moist_surf(is_anohm) = MAX(0.0D0, MIN(1.0D0, state_id(is_anohm)/StoreDrainPrm(6, is_anohm)))
                     ELSE
                        moist_surf(is_anohm) = 0.0D0
                     END IF
                  END DO

                  IF (Diagnose == 1) WRITE (*, *) 'Calling AnOHM...'
                  qn_av_prev_ohm = ohmState%qn_av
                  dqndt_prev_ohm = ohmState%dqndt
                  CALL AnOHM( &
                     tstep, dt_since_start, &
                     qn_use, qn_av_prev_ohm, dqndt_prev_ohm, &
                     ohmState%anohm_coeff_sd(1:ohmState%anohm_coeff_count), &
                     ohmState%anohm_coeff_ta(1:ohmState%anohm_coeff_count), &
                     ohmState%anohm_coeff_rh(1:ohmState%anohm_coeff_count), &
                     ohmState%anohm_coeff_pres(1:ohmState%anohm_coeff_count), &
                     ohmState%anohm_coeff_ws(1:ohmState%anohm_coeff_count), &
                     ohmState%anohm_coeff_ah(1:ohmState%anohm_coeff_count), &
                     ohmState%anohm_coeff_tHr(1:ohmState%anohm_coeff_count), &
                     moist_surf, &
                     alb, emis, cpAnOHM, kkAnOHM, chAnOHM, &
                     sfr_surf, nsurf, &
                     ohmState%anohm_a1_surf, ohmState%anohm_a2_surf, ohmState%anohm_a3_surf, &
                     ohmState%qn_av, ohmState%dqndt, &
                     a1, a2, a3, qs, deltaQi, &
                     modState)
                  QS_surf = qs
                  QS_roof = qs
                  QS_wall = qs

               ELSEIF (StorageHeatMethod == SHM_EHC) THEN
                  IF (Diagnose == 1) WRITE (*, *) 'Calling extended ESTM...'
                  ! facets: seven suews standard facets + extra for buildings [roof, wall] (can be extended for heterogeneous buildings)
                  !
                  ! ASSOCIATE (v => dz_roof(1, 1:ndepth))
                  !    PRINT *, 'dz_roof in cal_qs', v, SIZE(v)
                  ! END ASSOCIATE
                  ! ASSOCIATE (v => dz_wall(1, 1:ndepth))
                  !    PRINT *, 'dz_wall in cal_qs', v, SIZE(v)
                  ! END ASSOCIATE
                  IF (config%EHCSurfaceCoupling == 1) THEN
                     ALLOCATE (ehc_seb%a_roof(nlayer), ehc_seb%b_roof(nlayer), &
                               ehc_seb%a_wall(nlayer), ehc_seb%b_wall(nlayer), &
                               ehc_seb%tsfc_roof(nlayer), ehc_seb%tsfc_wall(nlayer))
                     CALL ehc_seb_linearisation( &
                        config, forcing, siteInfo, modState, & ! input
                        ehc_seb) ! inout: a, b
                     CALL ehc_implicit_boundaries( &
                        config, clock, siteInfo, modState, & ! input
                        ehc_seb) ! inout: t_deep_surf, t_indoor
                     CALL EHC( &
                        tstep, & !input
                        nlayer, config%NetRadiationMethod <= 1000, &
                        tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
                        tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
                        tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
                        heatState%temp_roof, QS_roof, & !output
                        heatState%temp_wall, QS_wall, & !output
                        heatState%temp_surf, QS_surf, & !output
                        QS, & !output
                        ehcDiag%ehc_dudt, ehcDiag%ehc_q_bottom, ehcDiag%ehc_q_indoor, & !output
                        modState%errorState, &
                        ehc_seb)
                     IF (supy_error_flag()) RETURN
                     CALL ehc_apply_implicit_tsfc( &
                        config, forcing, siteInfo, ehc_seb, & ! input
                        modState) ! inout
                     ehcDiag%ehc_t_indoor = ehc_seb%t_indoor
                     ehcDiag%ehc_t_deep = cal_ehc_ground_mean(ehc_seb%t_deep_surf, siteInfo%sfr_surf)
                  ELSE
                     CALL EHC( &
                        tstep, & !input
                        nlayer, config%NetRadiationMethod <= 1000, &
                        ! QG_surf, qg_roof, qg_wall, &
                        tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
                        tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
                        tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
                        heatState%temp_roof, QS_roof, & !output
                        heatState%temp_wall, QS_wall, & !output
                        heatState%temp_surf, QS_surf, & !output
                        QS, & !output
                        ehcDiag%ehc_dudt, ehcDiag%ehc_q_bottom, ehcDiag%ehc_q_indoor, & !output
                        modState%errorState)
                     ehcDiag%ehc_t_indoor = -999.0D0
                     ehcDiag%ehc_t_deep = cal_ehc_ground_mean(tin_surf, siteInfo%sfr_surf)
                  END IF

                  ! TODO: add deltaQi to output for snow heat storage

                  ! PRINT *, 'QS after ESTM_ehc', QS
                  ! PRINT *, 'QS_roof after ESTM_ehc', QS_roof
                  ! PRINT *, 'QS_wall after ESTM_ehc', QS_wall
                  ! PRINT *, 'QS_surf after ESTM_ehc', QS_surf
                  ! PRINT *, '------------------------------------'
                  ! PRINT *, ''

               ELSE
                  ! No scheme behind this value: ESTM (4) is disabled (GH#1802, see
                  ! dev-ref/ESTM_REACTIVATION.md to bring it back) and
                  ! 14 never reached the data model. A caller that bypasses the data
                  ! model must be refused here rather than run on with QS = -999.
                  WRITE (str_storage_heat, '(I0)') StorageHeatMethod
                  CALL set_supy_error( &
                     106, &
                     'SUEWS_cal_Qs: StorageHeatMethod '//TRIM(str_storage_heat)// &
                     ' is not available; use OHM (1), AnOHM (3), EHC (5), DyOHM (6), '// &
                     'STEBBS (7) or DyOHM_BUILDING (8)')
                  RETURN
               END IF
            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_Qs
!=======================================================================

   SUBROUTINE anohm_rollover_if_needed(clock, ohmState)
      ! AnOHM (StorageHeatMethod=3): at a day change, promote the just-completed
      ! working buffer into the coeff buffer and flag readiness once enough
      ! samples are present. Replaces the legacy future-data forcing dependency.
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(OHM_STATE), INTENT(INOUT) :: ohmState

      IF (ohmState%anohm_working_day == -999) THEN
         ohmState%anohm_working_day = clock%timer%id
      END IF

      IF (clock%timer%id /= ohmState%anohm_working_day) THEN
         ! Coefficients belong to the previous coeff buffer. Invalidate them
         ! before promoting a different completed calendar day.
         ohmState%anohm_a1_surf = -999.0D0
         ohmState%anohm_a2_surf = -999.0D0
         ohmState%anohm_a3_surf = -999.0D0

         IF (ohmState%anohm_working_count > 0) THEN
            ohmState%anohm_coeff_tHr = ohmState%anohm_working_tHr
            ohmState%anohm_coeff_sd = ohmState%anohm_working_sd
            ohmState%anohm_coeff_ta = ohmState%anohm_working_ta
            ohmState%anohm_coeff_rh = ohmState%anohm_working_rh
            ohmState%anohm_coeff_pres = ohmState%anohm_working_pres
            ohmState%anohm_coeff_ws = ohmState%anohm_working_ws
            ohmState%anohm_coeff_ah = ohmState%anohm_working_ah
            ohmState%anohm_coeff_day = ohmState%anohm_working_day
            ohmState%anohm_coeff_count = MIN(ohmState%anohm_working_count, ANOHM_MAX_SAMPLES)
            ohmState%anohm_coeff_ready = &
               COUNT(ohmState%anohm_coeff_sd(1:ohmState%anohm_coeff_count) > 5.0D0) >= ANOHM_MIN_VALID_SAMPLES
         ELSE
            ohmState%anohm_coeff_ready = .FALSE.
            ohmState%anohm_coeff_count = 0
            ohmState%anohm_coeff_day = ohmState%anohm_working_day
            ohmState%anohm_coeff_tHr = -999.0D0
            ohmState%anohm_coeff_sd = -999.0D0
            ohmState%anohm_coeff_ta = -999.0D0
            ohmState%anohm_coeff_rh = -999.0D0
            ohmState%anohm_coeff_pres = -999.0D0
            ohmState%anohm_coeff_ws = -999.0D0
            ohmState%anohm_coeff_ah = -999.0D0
         END IF

         ohmState%anohm_working_day = clock%timer%id
         ohmState%anohm_working_count = 0
         ohmState%anohm_working_tHr = -999.0D0
         ohmState%anohm_working_sd = -999.0D0
         ohmState%anohm_working_ta = -999.0D0
         ohmState%anohm_working_rh = -999.0D0
         ohmState%anohm_working_pres = -999.0D0
         ohmState%anohm_working_ws = -999.0D0
         ohmState%anohm_working_ah = -999.0D0
      END IF

   END SUBROUTINE anohm_rollover_if_needed
!=======================================================================

   SUBROUTINE anohm_append_sample(clock, forcing, qf, ohmState)
      ! AnOHM (StorageHeatMethod=3): append one on-the-hour forcing sample to the
      ! working buffer for the day in progress.
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      REAL(KIND(1D0)), INTENT(IN) :: qf
      TYPE(OHM_STATE), INTENT(INOUT) :: ohmState

      INTEGER :: idx

      IF (ohmState%anohm_working_day == -999) THEN
         ohmState%anohm_working_day = clock%timer%id
      END IF

      IF (clock%timer%id /= ohmState%anohm_working_day) THEN
         CALL anohm_rollover_if_needed(clock, ohmState)
      END IF

      ! only retain one sample per hour, on the hour
      IF (clock%timer%imin /= 0 .OR. clock%timer%isec /= 0) RETURN

      ! SUEWS_cal_Water can run repeatedly during the surface-temperature
      ! iteration. Overwrite the current hour in that case; otherwise append
      ! contiguously so working_count remains the number of real samples rather
      ! than the highest hour-of-day slot that happened to be populated.
      idx = ohmState%anohm_working_count + 1
      IF (ohmState%anohm_working_count > 0) THEN
         IF (NINT(ohmState%anohm_working_tHr(ohmState%anohm_working_count)) == clock%timer%it) THEN
            idx = ohmState%anohm_working_count
         END IF
      END IF
      IF (idx > ANOHM_MAX_SAMPLES) RETURN

      ohmState%anohm_working_tHr(idx) = REAL(clock%timer%it, KIND(1D0))
      ohmState%anohm_working_sd(idx) = forcing%kdown
      ohmState%anohm_working_ta(idx) = forcing%temp_c
      ohmState%anohm_working_rh(idx) = forcing%RH
      ohmState%anohm_working_pres(idx) = forcing%pres
      ohmState%anohm_working_ws(idx) = forcing%U
      ohmState%anohm_working_ah(idx) = qf

      ohmState%anohm_working_count = MAX(ohmState%anohm_working_count, idx)

   END SUBROUTINE anohm_append_sample
!=======================================================================

!==========================drainage and runoff================================
   SUBROUTINE SUEWS_cal_Water( &
      clock, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, PHENOLOGY_STATE, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, HYDRO_STATE, &
                               SUEWS_STATE

      IMPLICIT NONE
      TYPE(SUEWS_CLOCK), INTENT(in) :: clock
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
            nsh_real => clock%nsh_real, &
            Diagnose => config%Diagnose, &
            addPipes => hydroState%add_pipes, &
            addImpervious => hydroState%add_impervious, &
            addVeg => hydroState%add_veg, &
            addWaterBody => hydroState%add_water_body, &
            drain_per_tstep => hydroState%drain_per_tstep, &
            drain_surf => hydroState%drain_surf, &
            frac_water2runoff => hydroState%frac_water2runoff, &
            AdditionalWater => hydroState%additional_water, &
            runoffPipes => hydroState%runoff_pipes, &
            runoff_per_interval => hydroState%runoff_per_interval, &
            AddWater => hydroState%add_water, &
            SnowUse => config%SnowUse &
            )

            AdditionalWater = 0.0
            AdditionalWater = addPipes + addImpervious + addVeg + addWaterBody ![mm]

            ! Diagnose = config%Diagnose
            ! SnowUse = config%SnowUse

            state_id = hydroState%state_surf
            StoreDrainPrm = phenState%storage_drain_params
            IF (config%StorageHeatMethod == SHM_ANOHM) THEN
               ! AnOHM: accumulate the current calendar-day forcing buffer
               CALL anohm_rollover_if_needed(clock, modState%ohmState)
               CALL anohm_append_sample(clock, forcing, modState%heatState%qf, modState%ohmState)
            END IF

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
                     drain_surf(is), & ! output
                     modState)

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
      clock, config, forcing, siteInfo, & ! input
      modState, &
      dataOutLineSnow)

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_CLOCK, SNOW_PRM, &
                               SUEWS_FORCING, PHENOLOGY_STATE, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, &
                               LC_DECTR_PRM, LC_GRASS_PRM, LC_BSOIL_PRM, &
                               LC_WATER_PRM, SUEWS_SITE, SNOW_STATE, HYDRO_STATE, &
                               atm_state

      IMPLICIT NONE

      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState

      ! TYPE(atm_state), INTENT(INout) :: atmState

      ! TYPE(HEAT_STATE), INTENT(INOUT) :: heatState
      ! TYPE(HYDRO_STATE), INTENT(INOUT) :: hydroState

      TYPE(HYDRO_STATE) :: hydroState_prev ! snapshot of the incoming hydrological state

      ! TYPE(PHENOLOGY_STATE) :: phenState
      ! TYPE(SNOW_STATE) :: snowState
      TYPE(SNOW_STATE) :: snowState_prev ! snapshot of the incoming snow state

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

      REAL(KIND(1D0)), DIMENSION(2) :: ev_shortfall_imperv !Evaporation shortfall that impervious surfaces could not supply [mm]
      REAL(KIND(1D0)) :: surplusWaterBody !Extra runoff that goes to water body [mm] as specified by RunoffToWater
      REAL(KIND(1D0)) :: pin !Rain per time interval [mm]
      REAL(KIND(1D0)) :: qn_e !net available energy for evaporation [W m-2]
      REAL(KIND(1D0)) :: tlv !Latent heat of vapourisation per timestep [J kg-1 s-1]
      REAL(KIND(1D0)) :: ev_tot !total evaporation for all surfaces [mm]
      REAL(KIND(1D0)) :: qe_tot ! total latent heat flux for all surfaces [W m-2]
      REAL(KIND(1D0)) :: surf_chang_tot !total change in state_id(excluding snowpack) for all surfaces [mm]
      REAL(KIND(1D0)) :: runoff_tot !total runoff for all surfaces [mm]
      REAL(KIND(1D0)) :: chSnow_tot !total change state_id of snow and surface [mm]
      REAL(KIND(1D0)) :: ev_free !evaporation from a snow-free surface [mm]
      REAL(KIND(1D0)) :: qe_free !latent heat flux from a snow-free surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_free !surface state from the snow-free water balance [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_free !runoff from the snow-free water balance [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_snowfree !surface state of the surfaces handled as snow-free [mm]
      LOGICAL, DIMENSION(nsurf) :: is_snowfree !surface handled by the snow-free water balance this timestep

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
         FlowChange => siteInfo%flow_change, &
         PervFraction => siteInfo%PervFraction, &
         vegfraction => siteInfo%vegfraction, &
         avRh => forcing%RH, &
         Press_hPa => forcing%Pres, &
         Temp_C => forcing%Temp_C, &
         precip => forcing%rain, &
         imin => clock%imin_st, &
         it => clock%it_st, &
         dectime => clock%dectime, &
         tstep => clock%timer%tstep, &
         dayofWeek_id => clock%dayofWeek_id_st, &
         nsh_real => clock%nsh_real, &
         atmState => modState%atmState, &
         heatState => modState%heatState, &
         hydroState => modState%hydroState, &
         snowState => modState%snowState, &
         phenState => modState%phenState &
         )
         ! save the previous state
         hydroState_prev = hydroState
         snowState_prev = snowState

         ASSOCIATE ( &
            avdens => atmState%av_density, &
            avcp => atmState%av_cp, &
            lv_J_kg => atmState%lv_J_kg, &
            lvS_J_kg => atmState%lv_s_j_kg, &
            psyc_hPa => atmState%psyc_hPa, &
            sIce_hPa => atmState%s_ice_hpa, &
            vpd_hPa => atmState%vpd_hPa, &
            s_hPa => atmState%s_hPa, &
            RS => atmState%RS, &
            RA_h => atmState%RA_h, &
            RB => atmState%RB, &
            rss_surf => atmState%rss_surf, &
            RAsnow => snowState%ra_snow, &
            qn_ind_snow => snowState%qn_ind_snow, &
            kup_ind_snow => snowState%kup_ind_snow, &
            deltaQi => snowState%delta_qi, &
            Tsurf_ind_snow => snowState%Tsurf_ind_snow, &
            SnowRemoval => snowState%snow_removal, &
            NWstate_per_tstep => hydroState%nw_state_per_tstep, &
            swe => snowState%swe, &
            chSnow_per_interval => snowState%chSnow_per_interval, &
            mwstore => snowState%melt_water_store, &
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
            addimpervious => hydroState%add_impervious, &
            addVeg => hydroState%add_veg, &
            drain => hydroState%drain_surf, &
            AddWater => hydroState%add_water, &
            frac_water2runoff => hydroState%frac_water2runoff, &
            WU_surf => hydroState%WU_surf, &
            addWaterBody => hydroState%add_water_body, &
            state_per_tstep => hydroState%state_per_tstep, &
            ev_per_tstep => hydroState%ev_per_tstep, &
            runoff_per_tstep => hydroState%runoff_per_tstep, &
            surf_chang_per_tstep => hydroState%surf_chang_per_tstep, &
            runoffAGveg => hydroState%runoff_ag_veg, &
            runoffAGimpervious => hydroState%runoff_ag_impervious, &
            runoffPipes => hydroState%runoff_pipes, &
            runoffwaterbody => hydroState%runoff_waterbody, &
            state_id_in => hydroState_prev%state_surf, &
            soilstore_id_in => hydroState_prev%soil_store_surf, &
            StoreDrainPrm => phenState%storage_drain_params, &
            SnowPack_in => snowState_prev%snow_pack, &
            SnowFrac_in => snowState_prev%snow_fraction, &
            SnowWater_in => snowState_prev%snow_water, &
            iceFrac_in => snowState_prev%ice_frac, &
            SnowDens_in => snowState_prev%snow_density, &
            SnowfallCum_in => snowState_prev%snowfall_cum, &
            SnowAlb_in => snowState%snow_albedo, &
            EvapMethod => config%EvapMethod, &
            Diagnose => config%Diagnose &
            )

            ! Diagnose = config%Diagnose
            ASSOCIATE ( &
               WetThresh_surf => [pavedPrm%wet_threshold, bldgPrm%wet_threshold, evetrPrm%wet_threshold, dectrPrm%wet_threshold, &
                                  grassPrm%wet_threshold, bsoilPrm%wet_threshold, waterPrm%wet_threshold], &
               SoilStoreCap => [pavedPrm%soil%soil_store_capacity, bldgPrm%soil%soil_store_capacity, &
                                evetrPrm%soil%soil_store_capacity, dectrPrm%soil%soil_store_capacity, &
                                grassPrm%soil%soil_store_capacity, bsoilPrm%soil%soil_store_capacity, waterPrm%soil%soil_store_capacity], &
               StateLimit_surf => [pavedPrm%state_limit, bldgPrm%state_limit, evetrPrm%state_limit, &
                                   dectrPrm%state_limit, grassPrm%state_limit, bsoilPrm%state_limit, waterPrm%state_limit], &
               tau_r => snowPrm%tau_r, &
               CRWmin => snowPrm%water_holding_capacity_min, &
               CRWmax => snowPrm%water_holding_capacity_max, &
               SnowAlbMax => snowPrm%snow_albedo_max, &
               PrecipLimit => snowPrm%temperature_rain_snow_threshold, &
               PrecipLimitAlb => snowPrm%precipitation_threshold_albedo_reset, &
               SnowDensMax => snowPrm%snow_density_max, &
               SnowDensMin => snowPrm%snow_density_min, &
               RadMeltFact => snowPrm%radiation_melt_factor, &
               TempMeltFact => snowPrm%temperature_melt_factor, &
               SnowLimPaved => snowPrm%snow_depth_limit_paved, &
               SnowLimBldg => snowPrm%snow_depth_limit_building, &
               SnowPackLimit => snowPrm%snowpack_limit, &
               SnowProf_24hr_working => snowPrm%snowprof_24hr_working, &
               SnowProf_24hr_holiday => snowPrm%snowprof_24hr_holiday &
               )

               ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
               SnowProf_24hr(:, 1) = SnowProf_24hr_working
               SnowProf_24hr(:, 2) = SnowProf_24hr_holiday

               ! runoff_per_interval = runoff_per_interval_in
               state_id_surf = state_id_in
               soilstore_id = soilstore_id_in

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
               ! Grid aggregates accumulated inside SnowCalc across surfaces:
               ! initialise once here, not once per surface, or the outputs
               ! only ever carry the last (water) surface.
               swe = 0
               mwstore = 0
               SnowRemoval = 0
               qe = 0

               runoffAGveg = 0
               runoffAGimpervious = 0
               surplusWaterBody = 0
               runoff_surf = 0
               chang = 0
               ev_shortfall_imperv = 0.0D0

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

               is_snowfree = .FALSE.
               state_snowfree = state_id_in
               DO is = 1, nsurf !For each surface in turn
                  qe_tot = 0
                  ev_tot = 0
                  ev_snow = 0
                  runoff_tot = 0
                  surf_chang_tot = 0
                  chSnow_tot = 0
                  runoffPipes = 0
                  runoffwaterbody = 0
                  IF (sfr_surf(is) > 0 .AND. (snowCalcSwitch(is) == 1 .OR. is == WaterSurf)) THEN
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
                        WetThresh_surf, state_id_in, mw_ind, SoilStoreCap, StateLimit_surf, rainonsnow, &
                        freezmelt, freezstate, freezstatevol, &
                        Qm_Melt, Qm_rain, Tsurf_ind, sfr_surf, dayofWeek_id, StoreDrainPrm, SnowPackLimit, &
                        AddWater, frac_water2runoff, &
                        soilstore_id, SnowPack, ev_shortfall_imperv, & !inout
                        SnowFrac, SnowWater, iceFrac, SnowDens, &
                        runoffAGimpervious, runoffAGveg, surplusWaterBody, &
                        ev_tot, qe_tot, runoff_tot, surf_chang_tot, chSnow_tot, & ! output
                        rss_surf, &
                        runoff_surf, chang, ChangSnow_surf, SnowToSurf, state_id_surf, ev_snow, &
                        SnowRemoval, swe, &
                        runoffPipes, mwstore, runoffwaterbody)

                  ELSEIF (sfr_surf(is) > 0) THEN
                     ! A land surface with no snow, no snowfall and no freezing water takes the
                     ! snow-free evaporation and water balance, as before the 2018 restructuring
                     ! (#1905). SnowCalc does no hydrology when snowCalcSwitch is 0, so rain,
                     ! drainage and evaporation on such a surface were otherwise lost.
                     CALL cal_evap( &
                        EvapMethod, state_id_in(is), WetThresh_surf(is), StoreDrainPrm(6, is), & !input
                        vpd_hPa, avdens, avcp, qn_e, s_hPa, psyc_hPa, RS, RA_h, RB, tlv, &
                        rss_surf(is), ev_free, qe_free) !output
                     CALL cal_water_storage( &
                        is, sfr_surf, PipeCapacity, RunoffToWater, pin, & ! input:
                        WU_surf, &
                        drain, AddWater, addimpervious, nsh_real, state_id_in, frac_water2runoff, &
                        addVeg, SoilStoreCap, addWaterBody, FlowChange, StateLimit_surf, &
                        runoffAGimpervious, runoffAGveg, runoffPipes, ev_free, soilstore_id, & ! inout:
                        surplusWaterBody, ev_shortfall_imperv, runoffwaterbody, & ! inout:
                        runoff_free, state_free, & !output:
                        modState)
                     ! kept aside: SnowCalc, called for later surfaces on the same array,
                     ! resets every surface's state to its value at the start of the timestep
                     is_snowfree(is) = .TRUE.
                     state_snowfree(is) = state_free(is)
                     runoff_surf(is) = runoff_free(is)
                     ev_tot = ev_free*sfr_surf(is)
                     qe_tot = ev_free*tlv*sfr_surf(is)
                     runoff_tot = runoff_free(is)*sfr_surf(is)
                     surf_chang_tot = (state_free(is) - state_id_in(is))*sfr_surf(is)
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
               WHERE (is_snowfree) state_id_surf = state_snowfree

               qe = qe_per_tstep

               ! Calculate volume of water that will move between grids
               ! Volume [m3] = Depth relative to whole area [mm] / 1000 [mm m-1] * SurfaceArea [m2]
               ! Need to use these volumes when converting back to addImpervious, AddVeg and AddWater
               ! runoffAGimpervious_m3 = runoffAGimpervious/1000*SurfaceArea
               ! runoffAGveg_m3 = runoffAGveg/1000*SurfaceArea
               ! runoffWaterBody_m3 = runoffWaterBody/1000*SurfaceArea
               ! runoffPipes_m3 = runoffPipes/1000*SurfaceArea

               ! Write the updated states back into modState so they persist to the
               ! next timestep and feed this timestep's energy balance. Previously the
               ! results were left in local copies and discarded, so no snowpack could
               ! ever accumulate with SnowUse == 1.
               hydroState%state_surf = state_id_surf
               hydroState%soil_store_surf = soilstore_id
               snowState%snow_water = SnowWater
               snowState%ice_frac = iceFrac
               snowState%snow_albedo = SnowAlb
               snowState%snow_density = SnowDens
               snowState%snow_pack = SnowPack
               snowState%snow_fraction = SnowFrac
               snowState%snowfall_cum = SnowfallCum
               snowState%qm = Qm
               snowState%qm_freeze = QmFreez
               snowState%qm_rain = QmRain
               snowState%mwh = mwh

               ! Grid totals of surface wetness and the soil water balance, as at the
               ! end of SUEWS_cal_QE. Without them State and NWtrState stayed at the
               ! zero set above and TotCh and smd_surf kept the values of the last
               ! snow-free timestep (#1879).
               state_per_tstep = DOT_PRODUCT(state_id_surf, sfr_surf)
               IF (siteInfo%NonWaterFraction /= 0) THEN
                  NWstate_per_tstep = DOT_PRODUCT(state_id_surf(1:nsurf - 1), sfr_surf(1:nsurf - 1)) &
                                      /siteInfo%NonWaterFraction
               END IF
               CALL SUEWS_cal_SoilState( &
                  config%SMDMethod, forcing%xsmd, siteInfo%NonWaterFraction, hydroState%soil_moist_cap, & !input
                  SoilStoreCap, surf_chang_per_tstep, &
                  soilstore_id, soilstore_id_in, sfr_surf, &
                  hydroState%smd, hydroState%smd_surf, hydroState%tot_chang_per_tstep, hydroState%soil_state, & !output
                  modState)

               dataOutLineSnow = [ &
                                 snowState%snow_pack(1:nsurf), mw_ind(1:nsurf), Qm_melt(1:nsurf), & !26
                                 Qm_rain(1:nsurf), Qm_freezState(1:nsurf), snowState%snow_fraction(1:(nsurf - 1)), & !46
                                 rainOnSnow(1:nsurf), & !53
                                 qn_ind_snow(1:nsurf), kup_ind_snow(1:nsurf), freezMelt(1:nsurf), & !74
                                 SnowWater(1:nsurf), snowState%snow_density(1:nsurf), & !88
                                 snowDepth(1:nsurf), Tsurf_ind_snow(1:nsurf), &
                                 snowState%snow_albedo]

            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_cal_snow

!================latent heat flux and surface wetness===================
! TODO: optimise the structure of this function

   SUBROUTINE SUEWS_cal_QE( &
      clock, config, forcing, siteInfo, & ! input
      modState) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_CLOCK, SUEWS_FORCING, &
                               SUEWS_SITE, EHC_PRM, &
                               LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, &
                               PHENOLOGY_STATE, HYDRO_STATE, atm_state, &
                               SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
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
      REAL(KIND(1D0)) :: ev_building !aggregated evaporation of building facets [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: drain
      REAL(KIND(1D0)), DIMENSION(7) :: capStore_surf ! current storage capacity [mm]
      LOGICAL :: use_building_facets_for_ehc

      ! CALL hydroState_next%allocHydro(nlayer)
      ! ALLOCATE (hydroState%soil_store_roof(nlayer))
      ! ALLOCATE (hydroState%state_roof(nlayer))
      ! ALLOCATE (hydroState%soil_store_wall(nlayer))
      ! ALLOCATE (hydroState%state_wall(nlayer))

      ! load dim constants
      nlayer = siteInfo%nlayer
      use_building_facets_for_ehc = config%StorageHeatMethod == SHM_EHC &
                                    .AND. config%NetRadiationMethod > 1000 &
                                    .AND. siteInfo%sfr_surf(BldgSurf) > 0.0D0
      state_building = 0.0D0
      soilstore_building = 0.0D0
      capStore_builing = 0.0D0
      runoff_building = 0.0D0
      qe_building = 0.0D0
      ev_building = 0.0D0

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
            FlowChange => siteInfo%flow_change, &
            vegfraction => siteInfo%vegfraction, &
            NonWaterFraction => siteInfo%NonWaterFraction, &
            SurfaceArea => siteInfo%SurfaceArea, &
            avRh => forcing%RH, &
            Press_hPa => forcing%Pres, &
            Temp_C => forcing%Temp_C, &
            precip => forcing%rain, &
            xsmd => forcing%xsmd, &
            imin => clock%timer%imin, &
            it => clock%timer%it, &
            dectime => clock%dectime, &
            tstep => clock%timer%tstep, &
            tstep_real => clock%tstep_real, &
            dayofWeek_id => clock%dayofWeek_id, &
            nsh_real => clock%nsh_real, &
            avdens => atmState%av_density, &
            avcp => atmState%av_cp, &
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
            addVeg => hydroState%add_veg, &
            addWaterBody => hydroState%add_water_body, &
            AddWater_surf => hydroState%add_water, &
            drain_surf => hydroState%drain_surf, &
            frac_water2runoff_surf => hydroState%frac_water2runoff, &
            ev_surf => hydroState%ev_surf, &
            ev_roof => hydroState%ev_roof, &
            state_roof => hydroState%state_roof, &
            soilstore_roof => hydroState%soil_store_roof, &
            ev_wall => hydroState%ev_wall, &
            state_wall => hydroState%state_wall, &
            soilstore_wall => hydroState%soil_store_wall, &
            ev0_surf => hydroState%ev0_surf, &
            state_per_tstep => hydroState%state_per_tstep, &
            NWstate_per_tstep => hydroState%nw_state_per_tstep, &
            ev_per_tstep => hydroState%ev_per_tstep, &
            runoff_per_tstep => hydroState%runoff_per_tstep, &
            surf_chang_per_tstep => hydroState%surf_chang_per_tstep, &
            runoffPipes => hydroState%runoff_pipes, &
            runoffwaterbody => hydroState%runoff_waterbody, &
            runoffAGveg => hydroState%runoff_ag_veg, &
            runoffAGimpervious => hydroState%runoff_ag_impervious, &
            storageheatmethod => config%storageheatmethod, &
            addimpervious => hydroState%add_impervious, &
            state_surf_in => hydroState_in%state_surf, &
            soilstore_surf_in => hydroState_in%soil_store_surf, &
            state_roof_in => hydroState_in%state_roof, &
            soilstore_roof_in => hydroState_in%soil_store_roof, &
            state_wall_in => hydroState_in%state_wall, &
            soilstore_wall_in => hydroState_in%soil_store_wall, &
            state_surf => hydroState%state_surf, &
            soilstore_surf => hydroState%soil_store_surf, &
            runoffSoil_surf => hydroState%runoff_soil, &
            runoffSoil_per_tstep => hydroState%runoff_soil_per_tstep, &
            SoilMoistCap => hydroState%soil_moist_cap, &
            smd_surf => hydroState%smd_surf, &
            smd => hydroState%smd, &
            tot_chang_per_tstep => hydroState%tot_chang_per_tstep, &
            SoilState => hydroState%soil_state, &
            StoreDrainPrm => phenState%storage_drain_params, &
            snowfrac_in => snowstate%snow_fraction, &
            SMDMethod => config%SMDMethod, &
            EvapMethod => config%EvapMethod, &
            Diagnose => config%Diagnose &
            )

            ASSOCIATE ( &
               StateLimit_roof => ehcPrm%state_limit_roof, &
               SoilStoreCap_roof => ehcPrm%soil_store_capacity_roof, &
               WetThresh_roof => ehcPrm%wet_thresh_roof, &
               StateLimit_wall => ehcPrm%state_limit_wall, &
               SoilStoreCap_wall => ehcPrm%soil_store_capacity_wall, &
               WetThresh_wall => ehcPrm%wet_thresh_wall, &
               StateLimit_surf => [pavedPrm%state_limit, bldgPrm%state_limit, evetrPrm%state_limit, &
                                   dectrPrm%state_limit, grassPrm%state_limit, bsoilPrm%state_limit, waterPrm%state_limit], &
               SoilStoreCap_surf => [pavedPrm%soil%soil_store_capacity, bldgPrm%soil%soil_store_capacity, &
                                     evetrPrm%soil%soil_store_capacity, dectrPrm%soil%soil_store_capacity, &
                                     grassPrm%soil%soil_store_capacity, bsoilPrm%soil%soil_store_capacity, waterPrm%soil%soil_store_capacity], &
               SoilDepth_surf => [ &
               pavedPrm%soil%soil_depth, bldgPrm%soil%soil_depth, evetrPrm%soil%soil_depth, &
               dectrPrm%soil%soil_depth, &
               grassPrm%soil%soil_depth, bsoilPrm%soil%soil_depth, waterPrm%soil%soil_depth], &
               SatHydraulicConduct_surf => [pavedPrm%soil%saturated_hydraulic_conductivity, bldgPrm%soil%saturated_hydraulic_conductivity, &
                                            evetrPrm%soil%saturated_hydraulic_conductivity, dectrPrm%soil%saturated_hydraulic_conductivity, &
                                            grassPrm%soil%saturated_hydraulic_conductivity, bsoilPrm%soil%saturated_hydraulic_conductivity, &
                                            waterPrm%soil%saturated_hydraulic_conductivity], &
               WetThresh_surf => [pavedPrm%wet_threshold, bldgPrm%wet_threshold, evetrPrm%wet_threshold, &
                                  dectrPrm%wet_threshold, grassPrm%wet_threshold, bsoilPrm%wet_threshold, waterPrm%wet_threshold] &
               )

               state_surf = state_surf_in
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
               IF (storageheatmethod == SHM_EHC .AND. use_building_facets_for_ehc) THEN
                     ! --- roofs ---
                     ! net available energy for evaporation
                     qn_e_roof = qn_roof - qs_roof ! qn1 changed to qn1_snowfree, lj in May 2013
                  CALL cal_evap_multi( &
                     EvapMethod, & !input
                     sfr_roof, state_roof_in, WetThresh_roof, statelimit_roof, & !input
                     vpd_hPa, avdens, avcp, qn_e_roof, s_hPa, psyc_hPa, RS, RA_h, RB, tlv, &
                     rss_roof, ev_roof, qe_roof) !output

                     ! --- walls ---
                     ! net available energy for evaporation
                     qn_e_wall = qn_wall - qs_wall ! qn1 changed to qn1_snowfree, lj in May 2013
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
                     qe_building = 0.0D0
                     ev_building = 0.0D0
                  ELSE
                     qe_building = (DOT_PRODUCT(qe_roof, sfr_roof) + DOT_PRODUCT(qe_wall, sfr_wall))/sfr_surf(BldgSurf)
                     ev_building = (DOT_PRODUCT(ev_roof, sfr_roof) + DOT_PRODUCT(ev_wall, sfr_wall))/sfr_surf(BldgSurf)
                  END IF
               END IF
               ! --- general suews surfaces ---
               CALL cal_water_storage_surf( &
                  pin, nsh_real, SnowFrac_in, &
                  PipeCapacity, RunoffToWater, & ! input:
                  addImpervious, addVeg, addWaterBody, FlowChange, &
                  SoilStoreCap_surf, StateLimit_surf, &
                  sfr_surf, drain_surf, AddWater_surf, frac_water2runoff_surf, WU_surf, &
                  ev0_surf, state_surf_in, soilstore_surf_in, &
                  runoffAGimpervious, runoffAGveg, runoffPipes, runoffwaterbody, & ! output:
                  ev_surf, state_surf, soilstore_surf, & ! output:
                  runoff_surf)

               ! update QE based on the water balance
               qe_surf = tlv*ev_surf

               ! --- update building related ---
               IF (storageheatmethod == SHM_EHC .AND. use_building_facets_for_ehc) THEN
                  ! update building specific values
                  qe_surf(BldgSurf) = qe_building
                  ev_surf(BldgSurf) = ev_building
                  state_surf(BldgSurf) = state_building
                  IF (capStore_builing > 0.0D0) THEN
                     soilstore_surf(BldgSurf) = soilstore_building/capStore_builing*capStore_surf(BldgSurf)
                  END IF
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
               IF (storageheatmethod == SHM_EHC) THEN
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
                  smd, smd_surf, tot_chang_per_tstep, SoilState, & !output
                  modState)

            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_cal_QE
!========================================================================

!===============sensible heat flux======================================

   SUBROUTINE SUEWS_cal_QH( &
      clock, config, forcing, siteInfo, & ! input
      modState, ehcDiag) ! input/output:
      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_CLOCK, SUEWS_SITE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, HEAT_STATE, &
                               SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState
      TYPE(dts_ehc_diagnostics), INTENT(inout) :: ehcDiag

      INTEGER :: is
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qf_surf_ehc
      LOGICAL :: use_building_facets_for_ehc

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
            FlowChange => siteInfo%flow_change, &
            PervFraction => siteInfo%PervFraction, &
            vegfraction => siteInfo%vegfraction, &
            NonWaterFraction => siteInfo%NonWaterFraction, &
            tstep_real => clock%tstep_real, &
            tsfc_surf => heatState%tsfc_surf, &
            tsfc_roof => heatState%tsfc_roof, &
            tsfc_wall => heatState%tsfc_wall, &
            qn => heatState%qn, &
            qf => heatState%qf, &
            qe => heatState%qe, &
            qs => heatState%qs, &
            QmRain => snowState%qm_rain, &
            QmFreez => snowState%qm_freeze, &
            Qm => snowState%qm, &
            xsmd => forcing%xsmd, &
            Temp_C => forcing%Temp_C, &
            RA_h => atmState%RA_h, &
            avdens => atmState%av_density, &
            avcp => atmState%av_cp, &
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
            NetRadiationMethod => config%NetRadiationMethod, &
            Diagnose => config%Diagnose &
            )
            ! tsfc_surf = heatState_out%tsfc_surf
            ! tsfc_roof = heatState_out%tsfc_roof
            ! tsfc_wall = heatState_out%tsfc_wall

            ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
            use_building_facets_for_ehc = storageheatmethod == SHM_EHC .AND. NetRadiationMethod > 1000
            IF (storageheatmethod == SHM_EHC .AND. config%EHCSurfaceCoupling == 1) THEN
               CALL ehc_implicit_qf_surface(sfr_surf, ehcDiag%ehc_q_indoor, qf_surf_ehc)
            ELSE
               qf_surf_ehc = qf
            END IF
            IF (storageheatmethod == SHM_EHC) THEN
               ! QF that enters no surface energy balance: on the explicit
               ! facet route the building tile's share, in the implicit
               ! coupling everything but the indoor input to the envelope
               IF (use_building_facets_for_ehc .AND. config%EHCSurfaceCoupling /= 1) THEN
                  ehcDiag%ehc_qf_air = qf - DOT_PRODUCT(qf_surf_ehc, sfr_surf) + qf_surf_ehc(BldgSurf)*sfr_surf(BldgSurf)
               ELSE
                  ehcDiag%ehc_qf_air = qf - DOT_PRODUCT(qf_surf_ehc, sfr_surf)
               END IF
            END IF
            qh_residual = (qn + qf + QmRain) - (qe + qs + Qm + QmFreez) !qh=(qn1+qf+QmRain+QmFreez)-(qeOut+qs+Qm)

            ! Calculate resistance-based QH as the physical counterpart to the
            ! residual-driven Tsurf fixed point. For EHC, converged residual and
            ! resistance fluxes should match; the residual remains available as
            ! a closure diagnostic.
            IF (RA_h /= 0 .AND. avdens > 0 .AND. avcp > 0) THEN
               DO is = 1, nsurf
                  qh_resist_surf(is) = avdens*avcp*(tsfc_surf(is) - Temp_C)/RA_h
               END DO
               IF (use_building_facets_for_ehc) THEN
                  DO is = 1, nlayer
                     qh_resist_roof(is) = avdens*avcp*(tsfc_roof(is) - Temp_C)/RA_h
                     qh_resist_wall(is) = avdens*avcp*(tsfc_wall(is) - Temp_C)/RA_h
                  END DO

                  ! Aggregate building-facet QH with the same area weighting
                  ! used by EHC for building QS.
                  IF (sfr_surf(BldgSurf) > 1.0D-8) THEN
                     qh_resist_surf(BldgSurf) = &
                        (DOT_PRODUCT(qh_resist_roof, sfr_roof) + DOT_PRODUCT(qh_resist_wall, sfr_wall)) &
                        /sfr_surf(BldgSurf)
                  ELSE
                     qh_resist_surf(BldgSurf) = 0.0D0
                  END IF
               END IF
               qh_resist = DOT_PRODUCT(qh_resist_surf, sfr_surf)
            ELSE
               qh_resist_surf = QN_surf + qf_surf_ehc - qs_surf - qe_surf
               IF (use_building_facets_for_ehc) THEN
                  qh_resist_roof = QN_roof - qs_roof - qe_roof
                  qh_resist_wall = QN_wall - qs_wall - qe_wall
               ELSE
                  qh_resist_roof = QN_roof + qf - qs_roof - qe_roof
                  qh_resist_wall = QN_wall + qf - qs_wall - qe_wall
               END IF
               qh_resist = qh_residual
            END IF

            ! Preserve the legacy residual QH output for energy closure. The
            ! resistance flux is retained as a convergence/diagnostic companion.
            qh = qh_residual
            QH_surf = QN_surf + qf_surf_ehc - qs_surf - qe_surf
            IF (use_building_facets_for_ehc) THEN
               QH_roof = QN_roof - qs_roof - qe_roof
               QH_wall = QN_wall - qs_wall - qe_wall
               IF (sfr_surf(BldgSurf) > 1.0D-8) THEN
                  QH_surf(BldgSurf) = &
                     (DOT_PRODUCT(QH_roof, sfr_roof) + DOT_PRODUCT(QH_wall, sfr_wall)) &
                     /sfr_surf(BldgSurf)
               ELSE
                  QH_surf(BldgSurf) = 0.0D0
               END IF
            ELSE
               QH_roof = QN_roof + qf - qs_roof - qe_roof
               QH_wall = QN_wall + qf - qs_wall - qe_wall
            END IF
            qh_init = qh

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_QH
!========================================================================

!===============Resistance Calculations=======================

   SUBROUTINE ehc_stability_start(history, qh)
      TYPE(dts_ehc_stability_iteration), INTENT(OUT) :: history
      REAL(KIND(1D0)), INTENT(IN) :: qh

      history%qh = qh
      history%qh_negative = qh
      history%qh_positive = qh
      history%residual_negative = 0.0D0
      history%residual_positive = 0.0D0
      history%has_negative = .FALSE.
      history%has_positive = .FALSE.
   END SUBROUTINE ehc_stability_start

   SUBROUTINE ehc_stability_update(method, history, qh_residual)
      INTEGER, INTENT(IN) :: method
      TYPE(dts_ehc_stability_iteration), INTENT(INOUT) :: history
      REAL(KIND(1D0)), INTENT(IN) :: qh_residual
      REAL(KIND(1D0)), PARAMETER :: QH_RELAXATION = 0.25D0
      REAL(KIND(1D0)), PARAMETER :: SECANT_MARGIN = 0.1D0
      REAL(KIND(1D0)) :: residual, proposal, lower, upper, width

      ! Residual of the outer QH map, evaluated after the current heat balance.
      ! Other coupled fields are still iterating: this is an acceleration,
      ! not a proof of a scalar bracket. The caller verifies the original map.
      residual = qh_residual - history%qh
      IF (residual == 0.0D0) RETURN
      proposal = history%qh + QH_RELAXATION*residual
      IF (method == 2) THEN
         IF (residual < 0.0D0) THEN
            history%qh_negative = history%qh
            history%residual_negative = residual
            history%has_negative = .TRUE.
         ELSE IF (residual > 0.0D0) THEN
            history%qh_positive = history%qh
            history%residual_positive = residual
            history%has_positive = .TRUE.
         END IF
         IF (history%has_negative .AND. history%has_positive) THEN
            lower = MIN(history%qh_negative, history%qh_positive)
            upper = MAX(history%qh_negative, history%qh_positive)
            width = upper - lower
            IF (width > EPSILON(width)*MAX(1.0D0, ABS(lower), ABS(upper))) THEN
               proposal = history%qh_positive - history%residual_positive &
                          *(history%qh_positive - history%qh_negative) &
                          /(history%residual_positive - history%residual_negative)
               ! Central 80% safeguard avoids endpoint stagnation. A narrow
               ! bracket falls back to the same 0.25 relaxed update.
               IF (proposal < lower + SECANT_MARGIN*width .OR. proposal > upper - SECANT_MARGIN*width) &
                  proposal = lower + 0.5D0*width
            ELSE
               history%has_negative = .FALSE.
               history%has_positive = .FALSE.
            END IF
         END IF
      END IF
      history%qh = proposal
   END SUBROUTINE ehc_stability_update

   SUBROUTINE SUEWS_cal_Resistance( &
      clock, config, forcing, siteInfo, & ! input
      modState, w_f_pass_prev, skip_relaxation, qh_stability) ! input/output:

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_CLOCK, CONDUCTANCE_PRM, &
                               SUEWS_FORCING, &
                               LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, &
                               PHENOLOGY_STATE, SNOW_STATE, SUEWS_FORCING, SUEWS_SITE, &
                               SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INout) :: modState
      ! free-convection velocity accepted on the previous outer pass of this timestep,
      ! held by the caller; updated here on relaxed calls [m s-1]
      REAL(KIND(1D0)), INTENT(INOUT) :: w_f_pass_prev

      LOGICAL, INTENT(IN), OPTIONAL :: skip_relaxation
      REAL(KIND(1D0)), INTENT(IN), OPTIONAL :: qh_stability
      LOGICAL :: apply_relaxation
      REAL(KIND(1D0)) :: qh_stability_input

      INTEGER, PARAMETER :: AerodynamicResistanceMethod = 2 !method to calculate RA [-]

      REAL(KIND(1D0)) :: Tair ! air temperature [degC]
      REAL(KIND(1D0)) :: zL_prev_pass ! (z-d)/L used in the previous outer pass [-]
      REAL(KIND(1D0)) :: w_f_new ! free-convection velocity from this pass's heat flux [m s-1]
      REAL(KIND(1D0)) :: w_f_pass ! free-convection velocity used in this pass [m s-1]
      ! implicit EHC: r_a of the previous Tsfc iterate, for a damped update
      REAL(KIND(1D0)) :: ra_prev_iter
      REAL(KIND(1D0)), PARAMETER :: ehc_implicit_ra_relax = 0.5D0

      apply_relaxation = .TRUE.
      IF (PRESENT(skip_relaxation)) apply_relaxation = .NOT. skip_relaxation

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
            nsh_real => clock%nsh_real, &
            id => clock%timer%id, &
            it => clock%timer%it, &
            avU1 => forcing%U, &
            Temp_C => forcing%Temp_C, &
            avkdn => forcing%kdown, &
            xsmd => forcing%xsmd, &
            vsmd => hydroState%vsmd, &
            avdens => atmState%av_density, &
            avcp => atmState%av_cp, &
            dq => atmState%dq, &
            TStar => atmState%t_star, &
            UStar => atmState%u_star, &
            zL => atmState%z_l, &
            RS => atmState%RS, &
            RA => atmState%RA_h, &
            L_mod => atmState%L_mod, &
            RB => atmState%RB, &
            T2_C => atmState%T2_C, &
            T_half_bldg_C => atmState%T_half_bldg_C, &
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
            gfunc => phenState%g_func, &
            LAI_id => phenState%LAI_id, &
            RASnow => snowState%ra_snow, &
            z0vSnow => snowState%z0v_snow, &
            SnowFrac => snowState%snow_fraction, &
            Diagnose => config%Diagnose, &
            StabilityMethod => config%StabilityMethod, &
            RoughLenHeatMethod => config%RoughLenHeatMethod, &
            RSLLevel => config%RSLLevel, &
            SnowUse => config%SnowUse, &
            SMDMethod => config%SMDMethod &
            )
            ASSOCIATE ( &
               LAIMax => [evetrPrm%lai%lai_max, dectrPrm%lai%lai_max, grassPrm%lai%lai_max], &
               MaxConductance => [evetrPrm%max_conductance, dectrPrm%max_conductance, grassPrm%max_conductance], &
               gsModel => conductancePrm%gs_model, &
               Kmax => conductancePrm%k_max, &
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
               qh_stability_input = QH_init
               IF (PRESENT(qh_stability)) qh_stability_input = qh_stability
               !u* and Obukhov length out
               ! (z-d)/L carried from the previous outer pass (atmState is iteration-safe)
               zL_prev_pass = zL
               CALL cal_Stab( &
                  StabilityMethod, & ! input
                  zzd, & !Active measurement height (meas. height-displac. height)
                  z0m, & !Aerodynamic roughness length
                  zdm, & !zero-plane displacement
                  avU1, & !Average wind speed
                  Temp_C, & !Air temperature
                  qh_stability_input, & !sensible heat flux; ledger QH is never changed
                  avdens, & ! air density
                  avcp, & ! heat capacity of air
                  L_mod, & ! output: !Obukhov length
                  TStar, & !T*, temperature scale
                  UStar, & !Friction velocity
                  zL, & !Stability scale
                  modState)
               IF (supy_error_flag()) RETURN
               ! Damp the pass-to-pass two-cycle in (z-d)/L; the first pass of a
               ! timestep takes the solve as it is
               IF (apply_relaxation .AND. modState%flagState%i_iter > 1) THEN
                  zL = relax_zl(zL_prev_pass, zL, modState%flagState%i_iter)
                  CALL stability_scales(StabilityMethod, zzd, z0m, avU1, qh_stability_input/(avdens*avcp), &
                                        zL, L_mod, UStar, TStar)
                  IF (supy_error_flag()) RETURN
               END IF

               ! Free-convection velocity for the resistance, from this pass's heat
               ! flux on the unstable branch, damped between passes like (z-d)/L;
               ! the caller holds the previous pass's value for this timestep
               IF (zL < 0.0D0) THEN
                  w_f_new = free_convection_velocity(qh_stability_input/(avdens*avcp), Temp_C, zzd)
               ELSE
                  w_f_new = 0.0D0
               END IF
               w_f_pass = relax_free_convection(w_f_pass_prev, w_f_new, &
                                                modState%flagState%i_iter, apply_relaxation)
               IF (apply_relaxation) w_f_pass_prev = w_f_pass

               IF (Diagnose == 1) WRITE (*, *) 'Calling AerodynamicResistance...'
               ra_prev_iter = RA
               CALL AerodynamicResistance( &
                  ZZD, & ! input:
                  z0m, &
                  AVU1, &
                  L_mod, &
                  UStar, &
                  w_f_pass, &
                  VegFraction, &
                  AerodynamicResistanceMethod, &
                  StabilityMethod, &
                  RoughLenHeatMethod, &
                  RA, z0v, & ! output:
                  modState)
               IF (supy_error_flag()) RETURN

               ! Implicit EHC removes the conduction slope from the Tsfc
               ! iteration; the remaining Tsfc -> QH -> L -> r_a feedback
               ! can oscillate in stable conditions. Retain the 0.5 r_a
               ! damping on relaxed passes from outer pass 2 onwards.
               IF (apply_relaxation .AND. config%StorageHeatMethod == SHM_EHC .AND. config%EHCSurfaceCoupling == 1 &
                   .AND. modState%flagState%i_iter > 1 .AND. ra_prev_iter > 0.0D0) THEN
                  RA = ra_prev_iter + ehc_implicit_ra_relax*(RA - ra_prev_iter)
               END IF
               IF (SnowUse == 1) THEN
                  IF (Diagnose == 1) WRITE (*, *) 'Calling AerodynamicResistance for snow...'
                  CALL AerodynamicResistance( &
                     ZZD, & ! input:
                     z0m, &
                     AVU1, &
                     L_mod, &
                     UStar, &
                     w_f_pass, &
                     VegFraction, &
                     AerodynamicResistanceMethod, &
                     StabilityMethod, &
                     3, &
                     RASnow, z0vSnow, & ! output:
                     modState)
                  IF (supy_error_flag()) RETURN
               END IF

               IF (Diagnose == 1) WRITE (*, *) 'Calling SurfaceResistance...'
               ! CALL SurfaceResistance(id,it)   !qsc and surface resistance out
               Tair = MERGE(T_half_bldg_C, MERGE(T2_C, Temp_C, RSLLevel == 1), RSLLevel == 2)
               CALL SurfaceResistance( &
                  id, it, & ! input:
                  SMDMethod, SnowFrac, sfr_surf, avkdn, Tair, dq, xsmd, vsmd, MaxConductance, &
                  LAIMax, LAI_id, gsModel, Kmax, &
                  G_max, G_k, G_q_base, G_q_shape, G_t, G_sm, TH, TL, S1, S2, &
                  g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
                  gfunc, gsc, RS, & ! output:
                  modState)
               IF (supy_error_flag()) RETURN

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
      clock, config, forcing, siteInfo, & ! input
      modState, & ! input/output:
      datetimeLine, dataOutLineSUEWS) !output

      USE module_ctrl_type, ONLY: SUEWS_SITE, SUEWS_CLOCK, SUEWS_CONFIG, SUEWS_FORCING, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               HYDRO_STATE, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
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
      INTEGER :: out_idx

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
            FlowChange => siteInfo%flow_change, &
            sfr_surf => siteInfo%sfr_surf, &
            id => clock%timer%id, &
            imin => clock%timer%imin, &
            it => clock%timer%it, &
            iy => clock%timer%iy, &
            AdditionalWater => hydroState%additional_water, &
            avU10_ms => atmState%U10_ms, &
            azimuth => solarState%azimuth_deg, &
            SnowAlb => snowState%snow_albedo, &
            chSnow_per_interval => snowState%chSnow_per_interval, &
            dectime => clock%dectime, &
            drain_per_tstep => hydroState%drain_per_tstep, &
            QE_LUMPS => heatState%QE_LUMPS, &
            ev_per_tstep => hydroState%ev_per_tstep, &
            wu_ext => hydroState%wu_ext, &
            fcld => forcing%f_cloud, &
            Fc => anthroemisState%Fc, &
            Fc_build => anthroemisState%Fc_build, &
            Fc_metab => anthroemisState%Fc_metab, &
            Fc_photo => anthroemisState%Fc_photo, &
            Fc_respi => anthroemisState%Fc_respi, &
            Fc_point => anthroemisState%Fc_point, &
            Fc_traff => anthroemisState%Fc_traff, &
            QH_LUMPS => heatState%QH_LUMPS, &
            wu_int => hydroState%wu_int, &
            kup => heatState%k_up, &
            ldown => heatState%l_down, &
            l_mod => atmState%l_mod, &
            lup => heatState%l_up, &
            mwh => snowState%mwh, &
            MwStore => snowState%melt_water_store, &
            nsh_real => clock%nsh_real, &
            NWstate_per_tstep => hydroState%nw_state_per_tstep, &
            q2_gkg => atmState%q2_gkg, &
            qe => heatState%qe, &
            qf => heatState%qf, &
            qh => heatState%qh, &
            QH_init => heatState%QH_init, &
            qh_resist => heatState%qh_resist, &
            Qm => snowState%qm, &
            QmFreez => snowState%qm_freeze, &
            QmRain => snowState%qm_rain, &
            qn => heatState%qn, &
            qn_surf => heatState%qn_surf, &
            qn_snow => snowState%qn_snow, &
            qn_snowfree => heatState%qn_snowfree, &
            qs => heatState%qs, &
            qs_surf => heatState%qs_surf, &
            RA => atmState%RA_h, &
            RS => atmState%RS, &
            RH2 => atmState%RH2, &
            runoffAGimpervious => hydroState%runoff_ag_impervious, &
            runoffAGveg => hydroState%runoff_ag_veg, &
            runoff_per_tstep => hydroState%runoff_per_tstep, &
            runoffPipes => hydroState%runoff_pipes, &
            runoffSoil_per_tstep => hydroState%runoff_soil_per_tstep, &
            runoffWaterBody => hydroState%runoff_waterbody, &
            smd => hydroState%smd, &
            smd_surf => hydroState%smd_surf, &
            SnowRemoval => snowState%snow_removal, &
            state_per_tstep => hydroState%state_per_tstep, &
            surf_chang_per_tstep => hydroState%surf_chang_per_tstep, &
            swe => snowState%swe, &
            t2_C => atmState%t2_C, &
            tsfc_C => heatState%tsfc_C, &
            tot_chang_per_tstep => hydroState%tot_chang_per_tstep, &
            tsurf => heatState%tsurf, &
            Tsfc_surf => heatState%Tsfc_surf, &
            Tsfc_surf_dyohm => heatState%Tsfc_surf_dyohm, &
            UStar => atmState%u_star, &
            TStar => atmState%t_star, &
            wu_surf => hydroState%wu_surf, &
            z0m => roughnessState%z0m, &
            zdm => roughnessState%zdm, &
            zL => atmState%z_l, &
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
            wu_EveTr = hydroState%wu_surf(ConifSurf)
            wu_DecTr = hydroState%wu_surf(DecidSurf)
            wu_Grass = hydroState%wu_surf(GrassSurf)

            !====================== update output line ==============================
            ! date & time:
            datetimeLine = [ &
                           REAL(iy, KIND(1D0)), REAL(id, KIND(1D0)), &
                           REAL(it, KIND(1D0)), REAL(imin, KIND(1D0)), clock%dectime]
            ! Define output values with explicit bounds checks so column
            ! mismatches raise a controlled error instead of memory corruption.
            dataOutLineSUEWS = NAN
            out_idx = 1
            CALL push_scalar(kdown)
            CALL push_scalar(kup)
            CALL push_scalar(ldown)
            CALL push_scalar(lup)
            CALL push_scalar(tsurf)
            CALL push_scalar(qn)
            CALL push_scalar(qf)
            CALL push_scalar(qs)
            CALL push_scalar(qh)
            CALL push_scalar(qe)
            CALL push_scalar(QH_LUMPS)
            CALL push_scalar(QE_LUMPS)
            CALL push_scalar(QH_init)
            CALL push_scalar(qh_resist)
            CALL push_scalar(rain)
            CALL push_scalar(wu_ext)
            CALL push_scalar(ev_per_tstep)
            CALL push_scalar(runoff_per_tstep)
            CALL push_scalar(tot_chang_per_tstep)
            CALL push_scalar(surf_chang_per_tstep_x)
            CALL push_scalar(state_per_tstep)
            CALL push_scalar(NWstate_per_tstep)
            CALL push_scalar(drain_per_tstep)
            CALL push_scalar(smd)
            CALL push_scalar(FlowChange/nsh_real)
            CALL push_scalar(AdditionalWater)
            CALL push_scalar(runoffSoil_per_tstep)
            CALL push_scalar(runoffPipes)
            CALL push_scalar(runoffAGimpervious)
            CALL push_scalar(runoffAGveg)
            CALL push_scalar(runoffWaterBody)
            CALL push_scalar(wu_int)
            CALL push_scalar(wu_EveTr)
            CALL push_scalar(wu_DecTr)
            CALL push_scalar(wu_Grass)
            CALL push_vec(smd_surf_x(1:nsurf - 1))
            CALL push_vec(state_x(1:nsurf))
            CALL push_scalar(zenith_deg)
            CALL push_scalar(azimuth)
            CALL push_scalar(bulkalbedo)
            CALL push_scalar(Fcld)
            CALL push_scalar(LAI_wt)
            CALL push_scalar(z0m)
            CALL push_scalar(zdm)
            CALL push_scalar(zL)
            CALL push_scalar(UStar)
            CALL push_scalar(TStar)
            CALL push_scalar(l_mod)
            CALL push_scalar(RA)
            CALL push_scalar(RS)
            CALL push_scalar(Fc)
            CALL push_scalar(Fc_photo)
            CALL push_scalar(Fc_respi)
            CALL push_scalar(Fc_metab)
            CALL push_scalar(Fc_traff)
            CALL push_scalar(Fc_build)
            CALL push_scalar(Fc_point)
            CALL push_scalar(qn_snowfree)
            CALL push_scalar(qn_snow)
            CALL push_scalar(SnowAlb)
            CALL push_scalar(Qm)
            CALL push_scalar(QmFreez)
            CALL push_scalar(QmRain)
            CALL push_scalar(swe)
            CALL push_scalar(mwh)
            CALL push_scalar(MwStore)
            CALL push_scalar(chSnow_per_interval)
            CALL push_vec(SnowRemoval(1:2))
            CALL push_scalar(tsfc_C)
            CALL push_scalar(t2_C)
            CALL push_scalar(q2_gkg)
            CALL push_scalar(avU10_ms)
            CALL push_scalar(RH2_pct)
            CALL push_vec(Tsfc_surf)
            CALL push_vec(Tsfc_surf_dyohm)
            CALL push_vec(qn_surf)
            CALL push_vec(qs_surf)
            CALL check_packed_size()
            IF (supy_error_flag()) RETURN
            ! set invalid values to NAN
            ! dataOutLineSUEWS = set_nan(dataOutLineSUEWS)

            !====================update output line end==============================
         END ASSOCIATE
      END ASSOCIATE
   CONTAINS

      SUBROUTINE push_scalar(val)
         IMPLICIT NONE
         REAL(KIND(1D0)), INTENT(IN) :: val

         IF (supy_error_flag()) RETURN
         IF (out_idx > SIZE(dataOutLineSUEWS)) THEN
            CALL raise_pack_overflow(1)
            RETURN
         END IF
         dataOutLineSUEWS(out_idx) = val
         out_idx = out_idx + 1
      END SUBROUTINE push_scalar

      SUBROUTINE push_vec(vals)
         IMPLICIT NONE
         REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: vals
         INTEGER :: nvals

         IF (supy_error_flag()) RETURN
         nvals = SIZE(vals)
         IF (out_idx + nvals - 1 > SIZE(dataOutLineSUEWS)) THEN
            CALL raise_pack_overflow(nvals)
            RETURN
         END IF
         dataOutLineSUEWS(out_idx:out_idx + nvals - 1) = vals
         out_idx = out_idx + nvals
      END SUBROUTINE push_vec

      SUBROUTINE check_packed_size()
         IMPLICIT NONE
         CHARACTER(LEN=512) :: msg

         IF (supy_error_flag()) RETURN
         IF (out_idx - 1 /= SIZE(dataOutLineSUEWS)) THEN
            WRITE (msg, '(A,I0,A,I0,A)') &
               'SUEWS output packing size mismatch in SUEWS_update_outputLine: packed=', &
               out_idx - 1, ', capacity=', SIZE(dataOutLineSUEWS), &
               '. Check ncolumnsDataOutSUEWS and output field list.'
            CALL set_supy_error(104, TRIM(msg))
         END IF
      END SUBROUTINE check_packed_size

      SUBROUTINE raise_pack_overflow(nvals)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: nvals
         CHARACTER(LEN=512) :: msg

         IF (supy_error_flag()) RETURN
         WRITE (msg, '(A,I0,A,I0,A,I0,A)') &
            'SUEWS output packing overflow in SUEWS_update_outputLine: next_index=', &
            out_idx, ', adding=', nvals, ', capacity=', SIZE(dataOutLineSUEWS), &
            '. Check ncolumnsDataOutSUEWS and output field list.'
         CALL set_supy_error(104, TRIM(msg))
      END SUBROUTINE raise_pack_overflow

   END SUBROUTINE SUEWS_update_outputLine
!========================================================================

!==============Update output arrays=========================
   SUBROUTINE EHC_update_outputLine( &
      clock, & !input
      modState, ehcDiag, & ! input/output:
      datetimeLine, dataOutLineEHC) !output

      USE module_ctrl_type, ONLY: SUEWS_CLOCK, HEAT_STATE, HYDRO_STATE, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_STATE), INTENT(inout) :: modState
      TYPE(dts_ehc_diagnostics), INTENT(IN) :: ehcDiag

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
            iy => clock%timer%iy, &
            id => clock%timer%id, &
            it => clock%timer%it, &
            imin => clock%timer%imin, &
            dectime => clock%dectime, &
            tsfc_out_surf => heatState%tsfc_surf, &
            qs_surf => heatState%qs_surf, &
            tsfc_out_roof => heatState%tsfc_roof, &
            Qn_roof => heatState%Qn_roof, &
            QS_roof => heatState%QS_roof, &
            QE_roof => heatState%QE_roof, &
            QH_roof => heatState%QH_roof, &
            state_roof => hydroState%state_roof, &
            soilstore_roof => hydroState%soil_store_roof, &
            tsfc_out_wall => heatState%tsfc_wall, &
            Qn_wall => heatState%Qn_wall, &
            QS_wall => heatState%QS_wall, &
            QE_wall => heatState%QE_wall, &
            QH_wall => heatState%QH_wall, &
            state_wall => hydroState%state_wall, &
            soilstore_wall => hydroState%soil_store_wall, &
            ehc_dudt => ehcDiag%ehc_dudt, &
            ehc_q_bottom => ehcDiag%ehc_q_bottom, &
            ehc_q_indoor => ehcDiag%ehc_q_indoor, &
            qf_build => ehcDiag%qf_build, &
            ehc_qf_air => ehcDiag%ehc_qf_air, &
            ehc_t_indoor => ehcDiag%ehc_t_indoor, &
            ehc_t_deep => ehcDiag%ehc_t_deep &
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
                             fill_sim_res(soilstore_wall, n_fill), &
                             ehc_dudt, ehc_q_bottom, ehc_q_indoor, qf_build, ehc_qf_air, &
                             ehc_t_indoor, ehc_t_deep &
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

   SUBROUTINE validate_outputline_layout(outputLine)
      ! Validate output_line storage against compile-time column constants.
      ! This detects stale/mixed compilation units (e.g., partial rebuilds)
      ! before any output assignment can corrupt memory.
      IMPLICIT NONE
      TYPE(output_line), INTENT(IN) :: outputLine
      CHARACTER(LEN=512) :: msg

      CALL check_size('outputLine%dataOutLineSUEWS', SIZE(outputLine%dataOutLineSUEWS), ncolumnsDataOutSUEWS)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineSnow', SIZE(outputLine%dataOutLineSnow), ncolumnsDataOutSnow)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineESTM', SIZE(outputLine%dataOutLineESTM), ncolumnsDataOutESTM)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineEHC', SIZE(outputLine%dataOutLineEHC), ncolumnsDataOutEHC)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineRSL', SIZE(outputLine%dataOutLineRSL), ncolumnsDataOutRSL)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineBEERS', SIZE(outputLine%dataOutLineBEERS), ncolumnsDataOutBEERS)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineDebug', SIZE(outputLine%dataOutLineDebug), ncolumnsDataOutDebug)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineSPARTACUS', SIZE(outputLine%dataOutLineSPARTACUS), ncolumnsDataOutSPARTACUS)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineDailyState', SIZE(outputLine%dataOutLineDailyState), ncolumnsDataOutDailyState)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineSTEBBS', SIZE(outputLine%dataOutLineSTEBBS), ncolumnsDataOutSTEBBS)
      IF (supy_error_flag()) RETURN
      CALL check_size('outputLine%dataOutLineNHood', SIZE(outputLine%dataOutLineNHood), ncolumnsDataOutNHood)

   CONTAINS

      SUBROUTINE check_size(name, actual, expected)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN) :: actual, expected

         IF (actual /= expected) THEN
            WRITE (msg, '(A,A,A,I0,A,I0,A)') &
               'Build mismatch detected: ', TRIM(name), ' size=', actual, &
               ' expected=', expected, '. Run a full clean rebuild.'
            CALL set_supy_error(104, TRIM(msg))
         END IF
      END SUBROUTINE check_size

   END SUBROUTINE validate_outputline_layout

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

      SUBROUTINE reset_ehc_heat_state_for_tsurf_iteration(state_iter, state_stepstart)
         ! Restore EHC material temperatures to the beginning of the physical
         ! time step while retaining the current surface-temperature iterate and
         ! the fluxes calculated in the current iteration.
         TYPE(HEAT_STATE), INTENT(INOUT) :: state_iter
         TYPE(HEAT_STATE), INTENT(IN) :: state_stepstart

         state_iter%temp_roof = state_stepstart%temp_roof
         state_iter%temp_wall = state_stepstart%temp_wall
         state_iter%temp_surf = state_stepstart%temp_surf
      END SUBROUTINE reset_ehc_heat_state_for_tsurf_iteration

   SUBROUTINE ehc_seb_linearisation( &
      config, forcing, siteInfo, modState, &
      seb)
      ! Linearise each facet's surface energy balance about the current Tsfc
      ! iterate Ts* for the implicit EHC coupling:
      !    Q*(Ts) - QE - rho*cp*(Ts - Ta)/r_a - G = 0,
      !    Q*(Ts) ~ Q*(Ts*) - R1*(Ts - Ts*),  R1 = 4*emis*sigma*Ts*^3,
      ! giving  a - b*Ts - G = 0  with  b = rho*cp/r_a + R1  and
      ! a = Q*(Ts*) - QE + R1*Ts* + rho*cp*Ta/r_a  [W m-2, per facet area].
      ! No facet carries QF: traffic and metabolic heat enter the canyon air,
      ! and building energy use reaches the envelope through its indoor
      ! boundary (see ehc_implicit_qf_surface).
      ! R1 is zero where outgoing longwave is built from air temperature
      ! (NARP methods below 10). QE is the latest iterate (lagged by one Tsfc
      ! iteration), and r_a carries the stability correction of the latest
      ! QH; both are refreshed by the driver's outer iteration.
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(SUEWS_STATE), INTENT(IN) :: modState
      TYPE(dts_ehc_implicit), INTENT(INOUT) :: seb

      ! r_a used before any resistance exists (first step of a run) [s m-1]
      REAL(KIND(1D0)), PARAMETER :: ra_fallback = 50.0D0
      REAL(KIND(1D0)), DIMENSION(nsurf) :: emis_surf, r1_surf
      REAL(KIND(1D0)), DIMENSION(siteInfo%nlayer) :: r1_roof, r1_wall
      REAL(KIND(1D0)) :: h, ra
      LOGICAL :: lup_from_tsfc

      ASSOCIATE (heatState => modState%heatState, atmState => modState%atmState)
         ra = atmState%RA_h
         IF (.NOT. ra > 0.0D0) ra = ra_fallback
         h = atmState%av_density*atmState%av_cp/ra

         lup_from_tsfc = config%NetRadiationMethod >= 10
         emis_surf = [siteInfo%lc_paved%emis, siteInfo%lc_bldg%emis, siteInfo%lc_evetr%emis, &
                      siteInfo%lc_dectr%emis, siteInfo%lc_grass%emis, siteInfo%lc_bsoil%emis, &
                      siteInfo%lc_water%emis]
         r1_surf = 0.0D0
         r1_roof = 0.0D0
         r1_wall = 0.0D0
         IF (lup_from_tsfc) THEN
            r1_surf = 4.0D0*emis_surf*SBConst*(heatState%tsfc_surf + C2K)**3
            IF (config%NetRadiationMethod > 1000) THEN
               r1_roof = 4.0D0*siteInfo%spartacus_layer%emis_roof(1:siteInfo%nlayer)*SBConst &
                         *(heatState%tsfc_roof + C2K)**3
               r1_wall = 4.0D0*siteInfo%spartacus_layer%emis_wall(1:siteInfo%nlayer)*SBConst &
                         *(heatState%tsfc_wall + C2K)**3
            END IF
         END IF

         seb%b_surf = h + r1_surf
         seb%a_surf = heatState%QN_surf - heatState%QE_surf + r1_surf*heatState%tsfc_surf + h*forcing%temp_c
         seb%b_roof = h + r1_roof
         seb%a_roof = heatState%QN_roof - heatState%QE_roof + r1_roof*heatState%tsfc_roof + h*forcing%temp_c
         seb%b_wall = h + r1_wall
         seb%a_wall = heatState%QN_wall - heatState%QE_wall + r1_wall*heatState%tsfc_wall + h*forcing%temp_c
      END ASSOCIATE
   END SUBROUTINE ehc_seb_linearisation

   SUBROUTINE ehc_init_envelope(config, clock, forcing, siteInfo, modState)
      ! EHC roof/wall outer faces contact outdoor air; no ground-floor
      ! envelope column exists on this route. Deep-soil ground columns and
      ! the shared slab retain their configured initial temperatures.
      ! This optional initial approximation balances conduction, not the full
      ! radiative/convective SEB; subsequent timesteps solve that SEB normally.
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState
      TYPE(dts_ehc_implicit) :: seb
      INTEGER :: i

      CALL ehc_implicit_boundaries(config, clock, siteInfo, modState, seb)
      DO i = 1, siteInfo%nlayer
         CALL ehc_init_profile(siteInfo%ehc%dz_roof(i, :), siteInfo%ehc%k_roof(i, :), &
                               forcing%temp_c, seb%t_indoor, seb%r_indoor_roof, modState%heatState%temp_roof(i, :))
         CALL ehc_init_profile(siteInfo%ehc%dz_wall(i, :), siteInfo%ehc%k_wall(i, :), &
                               forcing%temp_c, seb%t_indoor, seb%r_indoor_wall, modState%heatState%temp_wall(i, :))
      END DO
   END SUBROUTINE ehc_init_envelope

   SUBROUTINE ehc_implicit_boundaries( &
      config, clock, siteInfo, modState, &
      seb)
      ! Lower-boundary temperatures of the implicit EHC columns.
      !
      ! Deep soil, under each ground surface: the annual surface temperature
      ! wave propagated to the bottom of that surface's column by the
      ! analytical solution for a periodic boundary on a homogeneous
      ! half-space,
      !    T(z, t) = Tm - A exp(-z/d) cos(w (t - t0) - z/d),  d = SQRT(2 kappa/w),
      ! with w = 2 pi / (365.25 days), z the column depth (sum of its layer
      ! thicknesses), kappa = z**2/(R C) the diffusivity of a homogeneous
      ! slab with the column's thermal resistance R = sum(dz/k) and heat
      ! capacity C = sum(rho_cp dz), Tm the annual mean air temperature and A
      ! half the largest difference between monthly mean air temperatures
      ! (properties.stebbs.temperature_air_annual_mean and
      ! temperature_air_month_mean_diffmax, used whether or not STEBBS runs),
      ! and t0 the coldest day of the surface wave: 15 January in the
      ! northern hemisphere, half a year later in the southern.
      ! YAML defaults are 10 degC for the mean and 10 K for the range;
      ! method-5 users should set both for their site's climate.
      ! Only a state with both values zero keeps the configured tin;
      ! omitting these properties from YAML does not select that fallback.
      !
      ! Indoor, for the roof and wall columns and the building surface of the
      ! plan-area slab: STEBBS's indoor air temperature where STEBBS runs,
      ! otherwise properties.stebbs.temperature_air_indoor_initial, or
      ! 20 degC where that is unset (zero).
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(SUEWS_STATE), INTENT(IN) :: modState
      TYPE(dts_ehc_implicit), INTENT(INOUT) :: seb

      REAL(KIND(1D0)), PARAMETER :: days_per_year = 365.25D0
      REAL(KIND(1D0)), PARAMETER :: coldest_day_north = 15.0D0 ! day of year
      REAL(KIND(1D0)), PARAMETER :: t_indoor_default = 20.0D0 ! [degC]
      REAL(KIND(1D0)) :: w_day, w_sec, t_mean, t_amp, t0, phase
      REAL(KIND(1D0)) :: depth, resist, cap, kappa, d_damp
      INTEGER :: i_surf

      seb%r_indoor_roof = config%resistance_ehc_indoor_roof
      seb%r_indoor_wall = config%resistance_ehc_indoor_wall
      ASSOCIATE (stebbsState => modState%stebbsState, ehcPrm => siteInfo%ehc)
         t_mean = stebbsState%deep_soil_temperature
         t_amp = 0.5D0*stebbsState%MonthMeanAirTemperature_diffmax
         w_day = 2.0D0*pi/days_per_year
         w_sec = w_day/REAL(sec_per_day, KIND(1D0))
         t0 = coldest_day_north
         IF (siteInfo%lat < 0.0D0) t0 = t0 + 0.5D0*days_per_year
         ! timer%dectime counts days from the start of the year (0 at
         ! 1 January 00:00)
         phase = w_day*(clock%dectime + 1.0D0 - t0)

         seb%t_deep_surf = ehcPrm%tin_surf
         IF (t_mean /= 0.0D0 .OR. t_amp /= 0.0D0) THEN
            DO i_surf = 1, nsurf
               depth = SUM(ehcPrm%dz_surf(i_surf, :))
               IF (ANY(ehcPrm%dz_surf(i_surf, :) <= 0.0D0) .OR. ANY(ehcPrm%k_surf(i_surf, :) <= 0.0D0) &
                   .OR. ANY(ehcPrm%cp_surf(i_surf, :) <= 0.0D0)) THEN
                  seb%t_deep_surf(i_surf) = t_mean
                  CYCLE
               END IF
               resist = SUM(ehcPrm%dz_surf(i_surf, :)/ehcPrm%k_surf(i_surf, :))
               cap = SUM(ehcPrm%cp_surf(i_surf, :)*ehcPrm%dz_surf(i_surf, :))
               kappa = depth**2/(resist*cap)
               d_damp = SQRT(2.0D0*kappa/w_sec)
               seb%t_deep_surf(i_surf) = t_mean - t_amp*EXP(-depth/d_damp)*COS(phase - depth/d_damp)
            END DO
         END IF

         seb%t_indoor = t_indoor_default
         IF (stebbsState%indoor_air_start_temperature /= 0.0D0) &
            seb%t_indoor = stebbsState%indoor_air_start_temperature
         IF (config%stebbsmethod == STEBBS_DEFAULT .OR. config%stebbsmethod == STEBBS_PROVIDED) THEN
            IF (ALLOCATED(stebbsState%buildings) .AND. modState%flagState%stebbs_bldg_init == 1) THEN
               IF (SIZE(stebbsState%buildings) >= 1) seb%t_indoor = stebbsState%buildings(1)%Tair_ind - C2K
            END IF
         END IF
      END ASSOCIATE
   END SUBROUTINE ehc_implicit_boundaries

   SUBROUTINE ehc_apply_implicit_tsfc( &
      config, forcing, siteInfo, seb, modState)
      ! Carry the surface temperatures solved with the conduction columns into
      ! the model state. With building facets, the building tile has no column
      ! of its own: its Tsfc is the one whose aerodynamic flux equals the
      ! area-weighted roof and wall fluxes, matching how QS and QH aggregate.
      ! Water on the facet route is not solved here (EHC returns its input
      ! Tsfc); suews_update_tsurf updates it.
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(dts_ehc_implicit), INTENT(IN) :: seb
      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState

      ASSOCIATE (heatState => modState%heatState, sfr_bldg => siteInfo%sfr_surf(BldgSurf), &
                 nlayer => siteInfo%nlayer)
         heatState%tsfc_surf = seb%tsfc_surf
         IF (config%NetRadiationMethod > 1000) THEN
            heatState%tsfc_roof = seb%tsfc_roof(1:nlayer)
            heatState%tsfc_wall = seb%tsfc_wall(1:nlayer)
            IF (sfr_bldg > 1.0D-8) THEN
               heatState%tsfc_surf(BldgSurf) = forcing%temp_c + &
                  (DOT_PRODUCT(siteInfo%sfr_roof, seb%tsfc_roof(1:nlayer) - forcing%temp_c) &
                   + DOT_PRODUCT(siteInfo%sfr_wall, seb%tsfc_wall(1:nlayer) - forcing%temp_c))/sfr_bldg
            END IF
         END IF
         heatState%tsfc_c = DOT_PRODUCT(heatState%tsfc_surf, siteInfo%sfr_surf)
      END ASSOCIATE
   END SUBROUTINE ehc_apply_implicit_tsfc

   SUBROUTINE ehc_implicit_qf_surface(sfr_surf, q_indoor, qf_surf)
      ! QF entering each surface's energy balance in the implicit EHC
      ! coupling. Facets carry no QF; the building tile carries the heat its
      ! envelope receives from the indoor boundary (q_indoor, plan-area
      ! weighted), so its residual QH equals the aerodynamic flux of its
      ! facets. The rest of the grid QF, QF - q_indoor (traffic, metabolism
      ! and any building energy use that does not pass through the
      ! envelope), enters the canyon air and appears only in the grid QH.
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: sfr_surf
      REAL(KIND(1D0)), INTENT(IN) :: q_indoor
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: qf_surf

      qf_surf = 0.0D0
      IF (sfr_surf(BldgSurf) > 1.0D-8 .AND. q_indoor > -999.0D0) qf_surf(BldgSurf) = q_indoor/sfr_surf(BldgSurf)
   END SUBROUTINE ehc_implicit_qf_surface

   PURE FUNCTION cal_ehc_ground_mean(t_surf, sfr_surf) RESULT(t_mean)
      ! area-weighted mean over the covered non-building surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: t_surf, sfr_surf
      REAL(KIND(1D0)) :: t_mean
      REAL(KIND(1D0)) :: w
      INTEGER :: i_surf

      t_mean = 0.0D0
      w = 0.0D0
      DO i_surf = 1, nsurf
         IF (i_surf == BldgSurf .OR. sfr_surf(i_surf) <= 0.0D0) CYCLE
         t_mean = t_mean + sfr_surf(i_surf)*t_surf(i_surf)
         w = w + sfr_surf(i_surf)
      END DO
      IF (w > 0.0D0) THEN
         t_mean = t_mean/w
      ELSE
         t_mean = -999.0D0
      END IF
   END FUNCTION cal_ehc_ground_mean

FUNCTION cal_tsfc_dyohm(Temp_in, Qs, K, C, z, nz, T_bottom, dt, errorState) RESULT(Temp_out)
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
    TYPE(error_state), INTENT(INOUT), OPTIONAL :: errorState ! per-grid warning log (GH#1737)
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
       IF (PRESENT(errorState)) CALL errorState%report( &
          message='time step may be too large for stability', &
          location='cal_tsfc_dyohm', is_fatal=.FALSE.)
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

   !==============================================================================
   ! Synchronise the thread-local fatal error state to modState%errorState
   ! The fatal store is per OS thread (suews_ctrl_error_tls.c, GH#1736), and a
   ! grid run stays on one thread, so this copy is per grid: concurrent grids
   ! cannot see or reset each other's fatal errors.
   ! Future: direct use of modState%errorState will eliminate need for sync.
   !==============================================================================
   SUBROUTINE sync_error_to_state(modState, timer)
      USE module_ctrl_type, ONLY: SUEWS_STATE, SUEWS_TIMER

      IMPLICIT NONE
      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      INTEGER :: err_code
      CHARACTER(LEN=SUPY_ERROR_MESSAGE_LEN) :: err_message

      ! Copy the calling thread's fatal error state to modState%errorState
      IF (supy_error_flag()) THEN
         CALL get_supy_error(err_code, err_message)
         CALL modState%errorState%set(err_code, TRIM(err_message))
      ELSE
         ! Clear only the fatal fields: the warning log must survive the whole
         ! run so it can be surfaced to the user (GH#1737).
         CALL modState%errorState%clear_fatal()
      END IF

      ! Give every warning raised during this timestep its timestamp
      CALL modState%errorState%stamp_pending(timer)

   END SUBROUTINE sync_error_to_state

END MODULE module_ctrl_driver

SUBMODULE (module_ctrl_driver) SUEWS_driver_io
   IMPLICIT NONE
CONTAINS
   ! Transfer this timestep's measured inputs to the object passed to SUEWS_cal_Main_impl.
   ! Columns 6 and 7 hold observed QH/QE; column 19 is the legacy bulk water-use slot.
   ! The kernel consumes the seven resolved surface water-use columns (24 through 30).
   MODULE PROCEDURE SUEWS_load_forcing_row
      forcing%qn1_obs = MetForcingBlock(ir, 5)
      forcing%qs_obs = MetForcingBlock(ir, 8)
      forcing%qf_obs = MetForcingBlock(ir, 9)
      forcing%U = MetForcingBlock(ir, 10)
      forcing%RH = MetForcingBlock(ir, 11)
      forcing%temp_c = MetForcingBlock(ir, 12)
      forcing%pres = MetForcingBlock(ir, 13)
      forcing%rain = MetForcingBlock(ir, 14)
      forcing%kdown = MetForcingBlock(ir, 15)
      forcing%snow_fraction = MetForcingBlock(ir, 16)
      forcing%l_down = MetForcingBlock(ir, 17)
      forcing%f_cloud = MetForcingBlock(ir, 18)
      forcing%xsmd = MetForcingBlock(ir, 20)
      forcing%LAI_evetr = MetForcingBlock(ir, 21)
      forcing%LAI_dectr = MetForcingBlock(ir, 22)
      forcing%LAI_grass = MetForcingBlock(ir, 23)
      forcing%Wu_mm_paved = MetForcingBlock(ir, 24)
      forcing%Wu_mm_bldgs = MetForcingBlock(ir, 25)
      forcing%Wu_mm_evetr = MetForcingBlock(ir, 26)
      forcing%Wu_mm_dectr = MetForcingBlock(ir, 27)
      forcing%Wu_mm_grass = MetForcingBlock(ir, 28)
      forcing%Wu_mm_bsoil = MetForcingBlock(ir, 29)
      forcing%Wu_mm_water = MetForcingBlock(ir, 30)
      forcing%kdiff = MetForcingBlock(ir, 31)
      forcing%kdir = MetForcingBlock(ir, 32)
   END PROCEDURE SUEWS_load_forcing_row

   ! Shared by the live timestep loop and the opt-in native output witness.
   ! Preserve the caller's column stride and all rows outside ir.
   MODULE PROCEDURE store_output_line
      INTEGER :: col_offset

      ! === Store all output groups concatenated, datetime prefix once ===
      datetimeBlock(ir, :) = line_out%dataOutLineSUEWS(1:5)
      col_offset = 0
      CALL store_group(line_out%dataOutLineSUEWS)
      CALL store_group(line_out%dataOutLineSnow)
      CALL store_group(line_out%dataOutLineBEERS)
      CALL store_group(line_out%dataOutLineESTM)
      CALL store_group(line_out%dataOutLineEHC)
      CALL store_group(line_out%dataOutLineDailyState)
      CALL store_group(line_out%dataoutLineRSL)
      CALL store_group(line_out%dataOutLineDebug)
      CALL store_group(line_out%dataOutLineSPARTACUS)
      CALL store_group(line_out%dataOutLineSTEBBS)
      CALL store_group(line_out%dataOutLineNHood)

   CONTAINS

      ! Copy one group's data columns (its line minus the datetime prefix) into
      ! row ir of dataOutBlockData and advance col_offset past them.
      SUBROUTINE store_group(line)
         REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: line
         INTEGER :: n_data

         n_data = SIZE(line) - 5
         dataOutBlockData(ir, col_offset + 1:col_offset + n_data) = line(6:)
         col_offset = col_offset + n_data
      END SUBROUTINE store_group

   END PROCEDURE store_output_line
END SUBMODULE SUEWS_driver_io
