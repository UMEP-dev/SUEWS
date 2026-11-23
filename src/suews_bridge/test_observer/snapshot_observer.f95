! Opt-in native observation of SUEWS_DEBUG stage transitions (via SUEWS_cal_Main)
! and production multiline block capture (via run_multitsteps_block, shared with
! SUEWS_cal_multitsteps). Test-only under feature `capi-observer`; no production
! snapshot wire is added or advertised.
SUBMODULE (SUEWS_Driver) test_snapshot_observer
   IMPLICIT NONE
CONTAINS

   SUBROUTINE snapshot_observe_stages( &
      config_flat, timer_flat, scalars, site_flat, site_len, site_toc, &
      state_flat, state_len, state_toc, nlayer, ndepth_in, &
      forcing_rows, wu_rows, &
      stage_dims, pre_post, aux, snow_cleared_max, err) &
      BIND(C, name='snapshot_observe_stages')
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double
      USE module_c_api_driver, ONLY: unpack_config, unpack_timer, unpack_site_scalars, &
         unpack_site_members, unpack_state_members
      IMPLICIT NONE

      REAL(c_double), INTENT(IN) :: config_flat(24), timer_flat(18), scalars(24)
      INTEGER(c_int), VALUE :: site_len, state_len, nlayer, ndepth_in
      REAL(c_double), INTENT(IN) :: site_flat(site_len), state_flat(state_len)
      INTEGER(c_int), INTENT(IN) :: site_toc(36), state_toc(26)
      REAL(c_double), INTENT(IN) :: forcing_rows(24, 3), wu_rows(7, 3)
      INTEGER(c_int), INTENT(OUT) :: stage_dims(2, 15, 3)
      REAL(c_double), INTENT(OUT) :: pre_post(2, 15, 3)
      REAL(c_double), INTENT(OUT) :: aux(40, 3)
      REAL(c_double), INTENT(OUT) :: snow_cleared_max(3)
      INTEGER(c_int), INTENT(OUT) :: err

      TYPE(SUEWS_CONFIG) :: config
      TYPE(SUEWS_TIMER) :: timer
      TYPE(SUEWS_SITE) :: site
      TYPE(SUEWS_STATE) :: state, control_state, step_start, oracle_state
      TYPE(SUEWS_FORCING) :: forcing, control_forcing, step_start_forcing
      TYPE(SUEWS_DEBUG), ALLOCATABLE :: snapshots, oracle_debug
      TYPE(output_line) :: output, control_output
      INTEGER :: row

      ALLOCATE (snapshots, oracle_debug)
      stage_dims = -1_c_int
      pre_post = 0.0_c_double
      aux = 0.0_c_double
      snow_cleared_max = -1.0_c_double
      err = 0_c_int

      IF (ndepth_in /= ndepth) THEN
         err = 100_c_int
         RETURN
      END IF

      CALL reset_supy_error()
      CALL unpack_config(config_flat, 24_c_int, config, err)
      IF (err /= 0_c_int) RETURN
      CALL unpack_timer(timer_flat, 18_c_int, timer, err)
      IF (err /= 0_c_int) RETURN
      CALL init_site_and_state( &
         site, state, INT(nlayer), INT(ndepth_in), config, &
         scalars, site_flat, site_len, site_toc, state_flat, state_len, state_toc, err)
      IF (err /= 0_c_int) RETURN

      DO row = 1, 3
         timer%iy = INT(forcing_rows(1, row))
         timer%id = INT(forcing_rows(2, row))
         timer%it = INT(forcing_rows(3, row))
         timer%imin = INT(forcing_rows(4, row))
         timer%isec = 0
         CALL load_row_forcing(forcing, forcing_rows(:, row), wu_rows(:, row))

         step_start = state
         step_start_forcing = forcing
         control_state = state
         control_forcing = forcing

         ! Paired run without optional debugState must match identically.
         config%flag_test = .FALSE.
         CALL SUEWS_cal_Main(timer, control_forcing, config, site, control_state, control_output)
         IF (supy_error_flag()) THEN
            err = 103_c_int
            RETURN
         END IF

         config%flag_test = .TRUE.
         CALL SUEWS_cal_Main(timer, forcing, config, site, state, output, snapshots)
         IF (supy_error_flag()) THEN
            err = 102_c_int
            RETURN
         END IF

         IF (.NOT. output_lines_equal_flag_test_pair(output, control_output)) THEN
            err = 104_c_int
            RETURN
         END IF
         IF (.NOT. states_equal(state, control_state)) THEN
            err = 105_c_int
            RETURN
         END IF

         CALL check_all_stage_allocs(snapshots, row, nlayer, ndepth_in, config%SnowUse, stage_dims, err)
         IF (err /= 0_c_int) RETURN
         CALL check_stage_nested_invariants(snapshots, step_start, control_state, config%SnowUse, err)
         IF (err /= 0_c_int) RETURN

         ! Independently replay every physics stage boundary outside
         ! SUEWS_cal_Main_impl and verify both final-state parity against the
         ! no-debug control and full 13-sub-state, 15-stage snapshot parity.
         CALL replay_timestep_stages( &
            timer, step_start_forcing, config, site, step_start, oracle_debug, oracle_state, err)
         IF (err /= 0_c_int) RETURN
         IF (.NOT. states_equal(oracle_state, control_state)) THEN
            err = 106_c_int
            RETURN
         END IF
         IF (.NOT. debug_snapshots_equal(snapshots, oracle_debug, config%SnowUse)) THEN
            err = 107_c_int
            RETURN
         END IF

         ! Pre/post discriminating stage witnesses for all 15 named stages.
         ! 01 dailystate: sunlit day length [h]
         pre_post(1, 1, row) = step_start%phenState%lenDay_id
         pre_post(2, 1, row) = snapshots%state_01_dailystate%phenState%lenDay_id
         ! 02 soilmoist: vegetated soil moisture deficit [mm]
         pre_post(1, 2, row) = snapshots%state_01_dailystate%hydroState%vsmd
         pre_post(2, 2, row) = snapshots%state_02_soilmoist%hydroState%vsmd
         ! 03 wateruse: external water use [mm]
         pre_post(1, 3, row) = snapshots%state_02_soilmoist%hydroState%wu_ext
         pre_post(2, 3, row) = snapshots%state_03_wateruse%hydroState%wu_ext
         ! 04 anthroemis: anthropogenic heat flux QF [W m-2]
         pre_post(1, 4, row) = snapshots%state_03_wateruse%heatState%qf
         pre_post(2, 4, row) = snapshots%state_04_anthroemis%heatState%qf
         ! 05 qn: net all-wave radiation QN [W m-2]
         pre_post(1, 5, row) = snapshots%state_04_anthroemis%heatState%qn
         pre_post(2, 5, row) = snapshots%state_05_qn%heatState%qn
         ! 06 qs: storage heat flux QS [W m-2]
         pre_post(1, 6, row) = snapshots%state_05_qn%heatState%qs
         pre_post(2, 6, row) = snapshots%state_06_qs%heatState%qs
         ! 07 qhqe_lumps: LUMPS sensible heat flux QH_LUMPS [W m-2]
         pre_post(1, 7, row) = snapshots%state_06_qs%heatState%QH_LUMPS
         pre_post(2, 7, row) = snapshots%state_07_qhqe_lumps%heatState%QH_LUMPS
         ! 08 water: drainage per timestep [mm]
         pre_post(1, 8, row) = snapshots%state_07_qhqe_lumps%hydroState%drain_per_tstep
         pre_post(2, 8, row) = snapshots%state_08_water%hydroState%drain_per_tstep
         ! 09 resist: solar conductance factor g_kdown [-]
         pre_post(1, 9, row) = snapshots%state_08_water%phenState%g_kdown
         pre_post(2, 9, row) = snapshots%state_09_resist%phenState%g_kdown
         ! 10 qe: latent heat flux QE [W m-2]
         pre_post(1, 10, row) = snapshots%state_09_resist%heatState%qe
         pre_post(2, 10, row) = snapshots%state_10_qe%heatState%qe
         ! 11 qh: sensible heat flux QH [W m-2]
         pre_post(1, 11, row) = snapshots%state_10_qe%heatState%qh
         pre_post(2, 11, row) = snapshots%state_11_qh%heatState%qh
         ! 12 tsurf: bulk aerodynamic surface temperature TSfc_C [degC]
         pre_post(1, 12, row) = snapshots%state_11_qh%heatState%TSfc_C
         pre_post(2, 12, row) = snapshots%state_12_tsurf%heatState%TSfc_C
         ! 13 rsl: 2 m diagnostic air temperature t2_C [degC]
         pre_post(1, 13, row) = snapshots%state_12_tsurf%atmState%t2_C
         pre_post(2, 13, row) = snapshots%state_13_rsl%atmState%t2_C
         ! 14 biogenco2: biogenic CO2 flux Fc_biogen [umol m-2 s-1]
         pre_post(1, 14, row) = snapshots%state_13_rsl%anthroemisState%Fc_biogen
         pre_post(2, 14, row) = snapshots%state_14_biogenco2%anthroemisState%Fc_biogen
         ! 15 beers: localised 2D incoming shortwave kdown_2d [W m-2]
         pre_post(1, 15, row) = snapshots%state_14_biogenco2%stebbsState%kdown_2d
         pre_post(2, 15, row) = snapshots%state_15_beers%stebbsState%kdown_2d

         ! Auxiliary fields for physical identities, unit checks, and output echoes.
         aux(1, row) = snapshots%state_01_dailystate%anthroemisState%HDD_id(5)
         aux(2, row) = snapshots%state_01_dailystate%phenState%Tmax_id
         aux(3, row) = snapshots%state_02_soilmoist%hydroState%soil_moist_cap
         aux(4, row) = snapshots%state_02_soilmoist%hydroState%soil_state
         aux(5, row) = snapshots%state_02_soilmoist%hydroState%smd
         aux(6, row) = snapshots%state_04_anthroemis%anthroemisState%Fc_anthro
         aux(7, row) = snapshots%state_03_wateruse%anthroemisState%Fc_anthro
         aux(8, row) = snapshots%state_06_qs%heatState%temp_roof(1, 1)
         aux(9, row) = snapshots%state_05_qn%heatState%temp_roof(1, 1)
         aux(10, row) = snapshots%state_07_qhqe_lumps%heatState%QE_LUMPS
         aux(11, row) = snapshots%state_09_resist%atmState%RA_h
         aux(12, row) = snapshots%state_09_resist%atmState%RS
         aux(13, row) = snapshots%state_09_resist%hydroState%ev_per_tstep
         aux(14, row) = snapshots%state_10_qe%hydroState%ev_per_tstep
         aux(15, row) = DOT_PRODUCT( &
            (snapshots%state_10_qe%atmState%lv_J_kg / REAL(timer%tstep, c_double)) * &
            snapshots%state_10_qe%hydroState%ev_surf, site%sfr_surf)
         aux(16, row) = snapshots%state_10_qe%hydroState%state_surf(1)
         aux(17, row) = snapshots%state_12_tsurf%atmState%av_density
         aux(18, row) = snapshots%state_12_tsurf%atmState%av_cp
         aux(19, row) = snapshots%state_12_tsurf%atmState%RA_h
         aux(20, row) = MERGE(1.0_c_double, 0.0_c_double, snapshots%state_11_qh%flagState%flag_converge)
         aux(21, row) = MERGE(1.0_c_double, 0.0_c_double, snapshots%state_12_tsurf%flagState%flag_converge)
         aux(22, row) = snapshots%state_13_rsl%atmState%U10_ms
         aux(23, row) = snapshots%state_14_biogenco2%anthroemisState%Fc_photo
         aux(24, row) = snapshots%state_14_biogenco2%anthroemisState%Fc_respi
         aux(25, row) = snapshots%state_14_biogenco2%anthroemisState%Fc
         aux(26, row) = snapshots%state_15_beers%stebbsState%lup_2d
         aux(27, row) = control_state%heatState%qh
         aux(28, row) = control_state%heatState%qe
         aux(29, row) = control_state%atmState%t2_C
         aux(30, row) = control_state%stebbsState%kdown_2d
         aux(31, row) = output%dataOutLineSUEWS(16)
         aux(32, row) = output%dataOutLineBEERS(11)
         ! Independent no-debug witnesses for stage 09 (g_kdown) and stage 07 (LUMPS).
         aux(33, row) = control_state%phenState%g_kdown
         aux(34, row) = control_output%dataOutLineDebug(120)
         aux(35, row) = output%dataOutLineDebug(120)
         aux(36, row) = snapshots%state_06_qs%heatState%qh_init
         aux(37, row) = snapshots%state_07_qhqe_lumps%heatState%qh_init
         aux(38, row) = snapshots%state_06_qs%heatState%qh
         aux(39, row) = control_output%dataOutLineSUEWS(16)
         aux(40, row) = control_output%dataOutLineSUEWS(17)

         ! #2014 witness: maximum residual inactive snow state across stage 01 and final state.
         snow_cleared_max(row) = MAX( &
            max_snow_abs(snapshots%state_01_dailystate%snowState), &
            max_snow_abs(state%snowState))

         timer%dt_since_start = timer%dt_since_start + timer%tstep
      END DO

      err = 0_c_int
   END SUBROUTINE snapshot_observe_stages

   ! Observe the genuine first-iteration (i_iter == 1) LUMPS_cal_QHQE physical
   ! transition on the exact pre-stage-07 state alongside the paired full
   ! SUEWS_cal_Main final-iteration debugState and no-debug public output.
   SUBROUTINE snapshot_observe_lumps_first_iter( &
      config_flat, timer_flat, scalars, site_flat, site_len, site_toc, &
      state_flat, state_len, state_toc, nlayer, ndepth_in, &
      forcing_rows, wu_rows, &
      lumps_pre_post, lumps_aux, err) &
      BIND(C, name='snapshot_observe_lumps_first_iter')
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double
      USE module_c_api_driver, ONLY: unpack_config, unpack_timer
      IMPLICIT NONE

      REAL(c_double), INTENT(IN) :: config_flat(24), timer_flat(18), scalars(24)
      INTEGER(c_int), VALUE :: site_len, state_len, nlayer, ndepth_in
      REAL(c_double), INTENT(IN) :: site_flat(site_len), state_flat(state_len)
      INTEGER(c_int), INTENT(IN) :: site_toc(36), state_toc(26)
      REAL(c_double), INTENT(IN) :: forcing_rows(24, 3), wu_rows(7, 3)
      REAL(c_double), INTENT(OUT) :: lumps_pre_post(2, 7, 3)
      REAL(c_double), INTENT(OUT) :: lumps_aux(14, 3)
      INTEGER(c_int), INTENT(OUT) :: err

      TYPE(SUEWS_CONFIG) :: config
      TYPE(SUEWS_TIMER) :: timer, timer_internal
      TYPE(SUEWS_SITE) :: site
      TYPE(SUEWS_STATE) :: state, control_state, st_iter1
      TYPE(SUEWS_FORCING) :: forcing, control_forcing, forcing_iter1
      TYPE(SUEWS_DEBUG), ALLOCATABLE :: snapshots
      TYPE(output_line) :: output, control_output
      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS - 5) :: dataOutLineSPARTACUS
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSTEBBS - 5) :: dataOutLineSTEBBS
      INTEGER :: row

      ALLOCATE (snapshots)
      lumps_pre_post = 0.0_c_double
      lumps_aux = 0.0_c_double
      err = 0_c_int
      datetimeLine = 0.0D0
      dataOutLineSPARTACUS = -999.0D0
      dataOutLineESTM = -999.0D0
      dataOutLineSTEBBS = -999.0D0

      IF (ndepth_in /= ndepth) THEN
         err = 300_c_int
         RETURN
      END IF

      CALL reset_supy_error()
      CALL unpack_config(config_flat, 24_c_int, config, err)
      IF (err /= 0_c_int) RETURN
      CALL unpack_timer(timer_flat, 18_c_int, timer, err)
      IF (err /= 0_c_int) RETURN
      CALL init_site_and_state( &
         site, state, INT(nlayer), INT(ndepth_in), config, &
         scalars, site_flat, site_len, site_toc, state_flat, state_len, state_toc, err)
      IF (err /= 0_c_int) RETURN

      DO row = 1, 3
         timer%iy = INT(forcing_rows(1, row))
         timer%id = INT(forcing_rows(2, row))
         timer%it = INT(forcing_rows(3, row))
         timer%imin = INT(forcing_rows(4, row))
         timer%isec = 0
         CALL load_row_forcing(forcing, forcing_rows(:, row), wu_rows(:, row))

         ! Prepare exact i_iter == 1 pre-LUMPS state matching SUEWS_cal_Main_impl.
         st_iter1 = state
         forcing_iter1 = forcing
         timer_internal = timer
         CALL SUEWS_cal_timer_reference( &
            timer_internal, site, site%anthroemis, config%ForcingTimestampReference)

         IF (config%snowuse == 0) THEN
            st_iter1%snowState%snow_density = 0.0D0
            st_iter1%snowState%snow_fraction = 0.0D0
            st_iter1%snowState%snow_water = 0.0D0
            st_iter1%snowState%snow_albedo = 0.0D0
            st_iter1%snowState%ice_frac = 0.0D0
            st_iter1%snowState%snow_pack = 0.0D0
         END IF
         st_iter1%snowState%qm = 0.0D0
         st_iter1%snowState%snow_fraction = MERGE( &
            forcing_iter1%snow_fraction, st_iter1%snowState%snow_fraction, config%NetRadiationMethod == 0)
         st_iter1%heatState%tsfc_surf_stepstart = st_iter1%heatState%tsfc_surf
         IF (config%StorageHeatMethod == 5 .OR. config%NetRadiationMethod > 1000) THEN
            st_iter1%heatState%tsfc_roof_stepstart = st_iter1%heatState%tsfc_roof
            st_iter1%heatState%tsfc_wall_stepstart = st_iter1%heatState%tsfc_wall
         END IF

         CALL SUEWS_cal_RoughnessParameters(timer_internal, config, forcing_iter1, site, st_iter1)
         IF (supy_error_flag()) THEN
            err = 301_c_int
            RETURN
         END IF
         CALL NARP_update_SunPosition(timer_internal, site, st_iter1)
         IF (supy_error_flag()) THEN
            err = 302_c_int
            RETURN
         END IF
         CALL SUEWS_update_atmState(timer_internal, forcing_iter1, st_iter1)
         IF (supy_error_flag()) THEN
            err = 303_c_int
            RETURN
         END IF

         st_iter1%flagState%i_iter = 1
         CALL SUEWS_cal_DailyState(timer_internal, config, forcing_iter1, site, st_iter1)
         IF (supy_error_flag()) THEN
            err = 304_c_int
            RETURN
         END IF
         CALL SUEWS_update_SoilMoist(timer_internal, config, forcing_iter1, site, st_iter1)
         IF (supy_error_flag()) THEN
            err = 305_c_int
            RETURN
         END IF
         CALL SUEWS_cal_WaterUse(timer_internal, config, forcing_iter1, site, st_iter1)
         IF (supy_error_flag()) THEN
            err = 306_c_int
            RETURN
         END IF
         CALL SUEWS_cal_AnthropogenicEmission(timer_internal, config, forcing_iter1, site, st_iter1)
         IF (supy_error_flag()) THEN
            err = 307_c_int
            RETURN
         END IF
         CALL SUEWS_cal_Qn(timer_internal, config, forcing_iter1, site, st_iter1, dataOutLineSPARTACUS)
         IF (supy_error_flag()) THEN
            err = 308_c_int
            RETURN
         END IF
         CALL SUEWS_cal_Qs( &
            timer_internal, config, forcing_iter1, site, datetimeLine, &
            st_iter1, dataOutLineESTM, dataOutLineSTEBBS)
         IF (supy_error_flag()) THEN
            err = 309_c_int
            RETURN
         END IF

         ! Pre-LUMPS (i_iter == 1, right after stage 06 QS).
         lumps_pre_post(1, 1, row) = st_iter1%heatState%QH_LUMPS
         lumps_pre_post(1, 2, row) = st_iter1%heatState%QE_LUMPS
         lumps_pre_post(1, 3, row) = st_iter1%heatState%QH_Init
         lumps_pre_post(1, 4, row) = st_iter1%atmState%s_hPa
         lumps_pre_post(1, 5, row) = st_iter1%atmState%psyc_hPa
         lumps_pre_post(1, 6, row) = st_iter1%phenState%veg_phen_lumps
         lumps_pre_post(1, 7, row) = st_iter1%phenState%temp_veg

         lumps_aux(1, row) = st_iter1%heatState%qn
         lumps_aux(2, row) = st_iter1%heatState%qf
         lumps_aux(3, row) = st_iter1%heatState%qs
         lumps_aux(4, row) = st_iter1%snowState%qm

         ! Production stage 07 (LUMPS_cal_QHQE) on i_iter == 1.
         CALL LUMPS_cal_QHQE(timer_internal, config, forcing_iter1, site, st_iter1)
         IF (supy_error_flag()) THEN
            err = 310_c_int
            RETURN
         END IF

         ! Post-LUMPS (i_iter == 1, right after stage 07 LUMPS).
         lumps_pre_post(2, 1, row) = st_iter1%heatState%QH_LUMPS
         lumps_pre_post(2, 2, row) = st_iter1%heatState%QE_LUMPS
         lumps_pre_post(2, 3, row) = st_iter1%heatState%QH_Init
         lumps_pre_post(2, 4, row) = st_iter1%atmState%s_hPa
         lumps_pre_post(2, 5, row) = st_iter1%atmState%psyc_hPa
         lumps_pre_post(2, 6, row) = st_iter1%phenState%veg_phen_lumps
         lumps_pre_post(2, 7, row) = st_iter1%phenState%temp_veg

         ! Paired full SUEWS_cal_Main calls (no-debug control + debug snapshots).
         control_state = state
         control_forcing = forcing
         config%flag_test = .FALSE.
         CALL SUEWS_cal_Main(timer, control_forcing, config, site, control_state, control_output)
         IF (supy_error_flag()) THEN
            err = 311_c_int
            RETURN
         END IF

         config%flag_test = .TRUE.
         CALL SUEWS_cal_Main(timer, forcing, config, site, state, output, snapshots)
         IF (supy_error_flag()) THEN
            err = 312_c_int
            RETURN
         END IF

         IF (.NOT. output_lines_equal_flag_test_pair(output, control_output)) THEN
            err = 313_c_int
            RETURN
         END IF
         IF (.NOT. states_equal(state, control_state)) THEN
            err = 314_c_int
            RETURN
         END IF

         lumps_aux(5, row) = snapshots%state_06_qs%heatState%QH_LUMPS
         lumps_aux(6, row) = snapshots%state_07_qhqe_lumps%heatState%QH_LUMPS
         lumps_aux(7, row) = snapshots%state_06_qs%heatState%QE_LUMPS
         lumps_aux(8, row) = snapshots%state_07_qhqe_lumps%heatState%QE_LUMPS
         lumps_aux(9, row) = snapshots%state_06_qs%heatState%QH_Init
         lumps_aux(10, row) = snapshots%state_07_qhqe_lumps%heatState%QH_Init
         lumps_aux(11, row) = REAL(snapshots%state_07_qhqe_lumps%flagState%i_iter, c_double)
         lumps_aux(12, row) = control_output%dataOutLineSUEWS(16)
         lumps_aux(13, row) = control_output%dataOutLineSUEWS(17)
         lumps_aux(14, row) = control_output%dataOutLineSUEWS(18)

         timer%dt_since_start = timer%dt_since_start + timer%tstep
      END DO

      err = 0_c_int
   END SUBROUTINE snapshot_observe_lumps_first_iter

   SUBROUTINE multiline_block_observe( &
      config_flat, timer_flat, scalars, site_flat, site_len, site_toc, &
      state_flat, state_len, state_toc, nlayer, ndepth_in, &
      forcing_rows, &
      block_widths, state_block_out, output_block_out, final_debug_out, mode_flags, &
      wall_rect_len, wall_temp_block_out, wall_temp_ref_out, &
      wall_final_len, wall_temp_dts_final_out, err) &
      BIND(C, name='multiline_block_observe')
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double
      USE module_c_api_driver, ONLY: unpack_config, unpack_timer, unpack_site_scalars, &
         unpack_site_members, unpack_state_members
      IMPLICIT NONE

      REAL(c_double), INTENT(IN) :: config_flat(24), timer_flat(18), scalars(24)
      INTEGER(c_int), VALUE :: site_len, state_len, nlayer, ndepth_in
      REAL(c_double), INTENT(IN) :: site_flat(site_len), state_flat(state_len)
      INTEGER(c_int), INTENT(IN) :: site_toc(36), state_toc(26)
      REAL(c_double), INTENT(IN) :: forcing_rows(24, 3)
      INTEGER(c_int), INTENT(OUT) :: block_widths(11)
      REAL(c_double), INTENT(OUT) :: state_block_out(16, 3)
      REAL(c_double), INTENT(OUT) :: output_block_out(20, 3)
      REAL(c_double), INTENT(OUT) :: final_debug_out(15)
      INTEGER(c_int), INTENT(OUT) :: mode_flags(5)
      INTEGER(c_int), VALUE :: wall_rect_len, wall_final_len
      REAL(c_double), INTENT(OUT) :: wall_temp_block_out(wall_rect_len)
      REAL(c_double), INTENT(OUT) :: wall_temp_ref_out(wall_rect_len)
      REAL(c_double), INTENT(OUT) :: wall_temp_dts_final_out(wall_final_len)
      INTEGER(c_int), INTENT(OUT) :: err

      INTEGER, PARAMETER :: nsteps = 3
      TYPE(SUEWS_CONFIG) :: cfg_base, cfg_run
      TYPE(SUEWS_TIMER) :: tmr_base, tmr_run, tmr_final
      TYPE(SUEWS_SITE) :: site
      TYPE(SUEWS_STATE) :: state_init, state_ref, state_m0, state_m1, state_m2, state_m3, state_m4, state_dts
      TYPE(SUEWS_STATE) :: ref_states(nsteps), step_start_final, oracle_state
      TYPE(SUEWS_FORCING) :: forcing, forcing_final
      TYPE(SUEWS_DEBUG), ALLOCATABLE :: ref_debug, oracle_debug, dbg_m2, dbg_m3, dbg_m4
      TYPE(SUEWS_STATE_BLOCK) :: blk_m1, blk_m3, blk_m4
      TYPE(output_line) :: ref_lines(nsteps)
      TYPE(output_block) :: ob_m0, ob_m1, ob_m2, ob_m3, ob_m4
      REAL(KIND(1D0)) :: met24(nsteps, 24), met32(nsteps, 32)
      REAL(KIND(1D0)), ALLOCATABLE :: dts_data(:, :), dts_dates(:, :)
      REAL(c_double) :: dummy_wu(7)
      INTEGER(c_int) :: dims_tmp(2, 15, 3)
      INTEGER :: r, il, id_w, widx, total_data_cols, off

      ALLOCATE (ref_debug, oracle_debug, dbg_m2, dbg_m3, dbg_m4)
      block_widths = 0_c_int
      state_block_out = 0.0_c_double
      output_block_out = 0.0_c_double
      final_debug_out = 0.0_c_double
      mode_flags = 0_c_int
      wall_temp_block_out = 0.0_c_double
      wall_temp_ref_out = 0.0_c_double
      wall_temp_dts_final_out = 0.0_c_double
      err = 0_c_int
      dummy_wu = 0.0_c_double

      IF (ndepth_in /= ndepth) THEN
         err = 200_c_int
         RETURN
      END IF
      IF (wall_final_len /= nlayer * ndepth_in .OR. wall_rect_len /= nsteps * nlayer * ndepth_in) THEN
         err = 203_c_int
         RETURN
      END IF

      CALL reset_supy_error()
      CALL unpack_config(config_flat, 24_c_int, cfg_base, err)
      IF (err /= 0_c_int) RETURN
      CALL unpack_timer(timer_flat, 18_c_int, tmr_base, err)
      IF (err /= 0_c_int) RETURN
      CALL init_site_and_state( &
         site, state_init, INT(nlayer), INT(ndepth_in), cfg_base, &
         scalars, site_flat, site_len, site_toc, state_flat, state_len, state_toc, err)
      IF (err /= 0_c_int) RETURN

      met24 = TRANSPOSE(forcing_rows)
      met32 = 0.0D0
      met32(:, 1:24) = met24

      ! Independent Reference 1: step-by-step SUEWS_cal_Main with final-row debug snapshot.
      state_ref = state_init
      tmr_run = tmr_base
      cfg_run = cfg_base
      cfg_run%flag_test = .TRUE.
      DO r = 1, nsteps
         tmr_run%iy = INT(met24(r, 1))
         tmr_run%id = INT(met24(r, 2))
         tmr_run%it = INT(met24(r, 3))
         tmr_run%imin = INT(met24(r, 4))
         tmr_run%isec = 0
         CALL load_row_forcing(forcing, forcing_rows(:, r), dummy_wu)
         IF (r == nsteps) THEN
            tmr_final = tmr_run
            forcing_final = forcing
            step_start_final = state_ref
            CALL SUEWS_cal_Main(tmr_run, forcing, cfg_run, site, state_ref, ref_lines(r), ref_debug)
         ELSE
            CALL SUEWS_cal_Main(tmr_run, forcing, cfg_run, site, state_ref, ref_lines(r))
         END IF
         IF (supy_error_flag()) THEN
            err = 201_c_int
            RETURN
         END IF
         ref_states(r) = state_ref
         tmr_run%dt_since_start = tmr_run%dt_since_start + tmr_run%tstep
      END DO

      ! Independent stage-by-stage oracle on the final timestep (outside
      ! SUEWS_cal_Main_impl) so shared capture corruption cannot cancel between
      ! ref_debug and dbg_m2/dbg_m3.
      CALL replay_timestep_stages( &
         tmr_final, forcing_final, cfg_run, site, step_start_final, oracle_debug, oracle_state, err)
      IF (err /= 0_c_int) RETURN
      IF (.NOT. states_equal(oracle_state, state_ref)) THEN
         err = 204_c_int
         RETURN
      END IF
      IF (.NOT. debug_snapshots_equal(ref_debug, oracle_debug, cfg_base%SnowUse)) THEN
         err = 205_c_int
         RETURN
      END IF

      ! Independent Reference 2: active SUEWS_cal_multitsteps_dts concatenated output table.
      total_data_cols = &
         (ncolumnsDataOutSUEWS - 5) + (ncolumnsDataOutSnow - 5) + &
         (ncolumnsDataOutBEERS - 5) + (ncolumnsDataOutESTM - 5) + &
         (ncolumnsDataOutEHC - 5) + (ncolumnsDataOutDailyState - 5) + &
         (ncolumnsDataOutRSL - 5) + (ncolumnsDataOutDebug - 5) + &
         (ncolumnsDataOutSPARTACUS - 5) + (ncolumnsDataOutSTEBBS - 5) + &
         (ncolumnsDataOutNHood - 5)
      ALLOCATE (dts_data(nsteps, total_data_cols), dts_dates(nsteps, 5))
      dts_data = 0.0D0
      dts_dates = 0.0D0
      state_dts = state_init
      tmr_run = tmr_base
      cfg_run = cfg_base
      cfg_run%flag_test = .FALSE.
      CALL SUEWS_cal_multitsteps_dts( &
         tmr_run, met32, nsteps, cfg_run, site, state_dts, dts_data, dts_dates)
      IF (supy_error_flag()) THEN
         err = 202_c_int
         RETURN
      END IF

      ! Mode 0: both optional arguments absent, flag_test = .FALSE.
      state_m0 = state_init
      tmr_run = tmr_base
      cfg_run = cfg_base
      cfg_run%flag_test = .FALSE.
      CALL run_multitsteps_block(tmr_run, met24, nsteps, cfg_run, site, state_m0, ob_m0)
      IF (supy_error_flag()) THEN
         err = 210_c_int
         RETURN
      END IF
      IF (.NOT. states_equal(state_m0, state_ref)) THEN
         err = 211_c_int
         RETURN
      END IF
      mode_flags(1) = 1_c_int

      ! Mode 1: block_mod_state present, state_debug absent, flag_test = .FALSE.
      state_m1 = state_init
      tmr_run = tmr_base
      cfg_run = cfg_base
      cfg_run%flag_test = .FALSE.
      CALL run_multitsteps_block( &
         tmr_run, met24, nsteps, cfg_run, site, state_m1, ob_m1, block_mod_state=blk_m1)
      IF (supy_error_flag()) THEN
         err = 220_c_int
         RETURN
      END IF
      IF (.NOT. ALLOCATED(blk_m1%BLOCK)) THEN
         err = 221_c_int
         RETURN
      END IF
      IF (SIZE(blk_m1%BLOCK) /= nsteps) THEN
         err = 222_c_int
         RETURN
      END IF
      IF (.NOT. output_blocks_equal(ob_m0, ob_m1)) THEN
         err = 223_c_int
         RETURN
      END IF
      IF (.NOT. states_equal(state_m1, state_ref)) THEN
         err = 225_c_int
         RETURN
      END IF
      DO r = 1, nsteps
         IF (.NOT. states_equal(blk_m1%BLOCK(r), ref_states(r))) THEN
            err = 224_c_int
            RETURN
         END IF
      END DO
      mode_flags(2) = 1_c_int

      ! Mode 2: state_debug present, block_mod_state absent, flag_test = .TRUE.
      state_m2 = state_init
      tmr_run = tmr_base
      cfg_run = cfg_base
      cfg_run%flag_test = .TRUE.
      CALL run_multitsteps_block( &
         tmr_run, met24, nsteps, cfg_run, site, state_m2, ob_m2, state_debug=dbg_m2)
      IF (supy_error_flag()) THEN
         err = 230_c_int
         RETURN
      END IF
      IF (.NOT. output_blocks_equal_flag_test_pair(ob_m2, ob_m0)) THEN
         err = 231_c_int
         RETURN
      END IF
      IF (.NOT. states_equal(state_m2, state_ref)) THEN
         err = 233_c_int
         RETURN
      END IF
      CALL check_all_stage_allocs(dbg_m2, 1, nlayer, ndepth_in, cfg_base%SnowUse, dims_tmp, err)
      IF (err /= 0_c_int) RETURN
      IF (.NOT. debug_snapshots_equal(dbg_m2, oracle_debug, cfg_base%SnowUse)) THEN
         err = 232_c_int
         RETURN
      END IF
      IF (.NOT. debug_snapshots_equal(dbg_m2, ref_debug, cfg_base%SnowUse)) THEN
         err = 234_c_int
         RETURN
      END IF
      mode_flags(3) = 1_c_int

      ! Mode 3: both state_debug and block_mod_state present, flag_test = .TRUE.
      state_m3 = state_init
      tmr_run = tmr_base
      cfg_run = cfg_base
      cfg_run%flag_test = .TRUE.
      CALL run_multitsteps_block( &
         tmr_run, met24, nsteps, cfg_run, site, state_m3, ob_m3, &
         state_debug=dbg_m3, block_mod_state=blk_m3)
      IF (supy_error_flag()) THEN
         err = 240_c_int
         RETURN
      END IF
      IF (.NOT. output_blocks_equal_flag_test_pair(ob_m3, ob_m0)) THEN
         err = 241_c_int
         RETURN
      END IF
      IF (.NOT. ALLOCATED(blk_m3%BLOCK)) THEN
         err = 242_c_int
         RETURN
      END IF
      IF (SIZE(blk_m3%BLOCK) /= nsteps) THEN
         err = 243_c_int
         RETURN
      END IF
      IF (.NOT. states_equal(state_m3, state_ref)) THEN
         err = 248_c_int
         RETURN
      END IF
      CALL check_all_stage_allocs(dbg_m3, 1, nlayer, ndepth_in, cfg_base%SnowUse, dims_tmp, err)
      IF (err /= 0_c_int) RETURN
      IF (.NOT. debug_snapshots_equal(dbg_m3, oracle_debug, cfg_base%SnowUse)) THEN
         err = 244_c_int
         RETURN
      END IF
      IF (.NOT. debug_snapshots_equal(dbg_m3, ref_debug, cfg_base%SnowUse)) THEN
         err = 249_c_int
         RETURN
      END IF
      DO r = 1, nsteps
         CALL check_state_allocs(blk_m3%BLOCK(r), nlayer, ndepth_in, .TRUE., 245_c_int, 246_c_int, err)
         IF (err /= 0_c_int) RETURN
         IF (.NOT. states_equal(blk_m3%BLOCK(r), ref_states(r))) THEN
            err = 247_c_int
            RETURN
         END IF
      END DO
      mode_flags(4) = 1_c_int

      ! Mode 4: both present, flag_test = .FALSE. -> block_mod_state populated,
      ! state_debug stays unallocated (verifies exact flag_test gate).
      state_m4 = state_init
      tmr_run = tmr_base
      cfg_run = cfg_base
      cfg_run%flag_test = .FALSE.
      CALL run_multitsteps_block( &
         tmr_run, met24, nsteps, cfg_run, site, state_m4, ob_m4, &
         state_debug=dbg_m4, block_mod_state=blk_m4)
      IF (supy_error_flag()) THEN
         err = 250_c_int
         RETURN
      END IF
      IF (.NOT. output_blocks_equal(ob_m0, ob_m4)) THEN
         err = 251_c_int
         RETURN
      END IF
      IF (.NOT. ALLOCATED(blk_m4%BLOCK)) THEN
         err = 252_c_int
         RETURN
      END IF
      IF (SIZE(blk_m4%BLOCK) /= nsteps) THEN
         err = 253_c_int
         RETURN
      END IF
      IF (.NOT. states_equal(state_m4, state_ref)) THEN
         err = 256_c_int
         RETURN
      END IF
      DO r = 1, nsteps
         IF (.NOT. states_equal(blk_m4%BLOCK(r), ref_states(r))) THEN
            err = 254_c_int
            RETURN
         END IF
      END DO
      IF (ALLOCATED(dbg_m4%state_01_dailystate%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_02_soilmoist%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_03_wateruse%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_04_anthroemis%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_05_qn%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_06_qs%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_07_qhqe_lumps%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_08_water%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_09_resist%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_10_qe%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_11_qh%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_12_tsurf%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_13_rsl%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_14_biogenco2%heatState%temp_roof) .OR. &
          ALLOCATED(dbg_m4%state_15_beers%heatState%temp_roof)) THEN
         err = 255_c_int
         RETURN
      END IF
      mode_flags(5) = 1_c_int

      ! Verify full-matrix parity between production output_block_suews and both
      ! independent references (SUEWS_cal_Main lines AND SUEWS_cal_multitsteps_dts).
      IF (.NOT. mat_equal(ob_m3%dataOutBlockSUEWS(:, 1:5), dts_dates)) THEN
         err = 260_c_int
         RETURN
      END IF
      DO r = 1, nsteps
         IF (.NOT. vec_equal(ob_m3%dataOutBlockSUEWS(r, :), ref_lines(r)%dataOutLineSUEWS)) THEN
            err = 261_c_int
            RETURN
         END IF
         IF (.NOT. vec_equal(ob_m3%dataOutBlockRSL(r, :), ref_lines(r)%dataoutLineRSL)) THEN
            err = 262_c_int
            RETURN
         END IF
         IF (.NOT. vec_equal(ob_m3%dataOutBlockDebug(r, :), ref_lines(r)%dataOutLineDebug)) THEN
            err = 263_c_int
            RETURN
         END IF
         IF (.NOT. vec_equal(ob_m3%dataOutBlockSPARTACUS(r, :), ref_lines(r)%dataOutLineSPARTACUS)) THEN
            err = 264_c_int
            RETURN
         END IF
         IF (.NOT. vec_equal(ob_m3%dataOutBlockDailyState(r, :), ref_lines(r)%dataOutLineDailyState)) THEN
            err = 265_c_int
            RETURN
         END IF
         IF (.NOT. vec_equal(ob_m3%dataOutBlockSTEBBS(r, :), ref_lines(r)%dataOutLineSTEBBS)) THEN
            err = 266_c_int
            RETURN
         END IF
         IF (.NOT. vec_equal(ob_m3%dataOutBlockBEERS(r, :), set_nan(ref_lines(r)%dataOutLineBEERS))) THEN
            err = 267_c_int
            RETURN
         END IF
         IF (cfg_base%SnowUse == 1) THEN
            IF (.NOT. vec_equal(ob_m3%dataOutBlockSnow(r, :), set_nan(ref_lines(r)%dataOutLineSnow))) THEN
               err = 268_c_int
               RETURN
            END IF
         ELSE
            IF (ANY(ob_m3%dataOutBlockSnow(r, :) /= 0.0D0)) THEN
               err = 269_c_int
               RETURN
            END IF
         END IF
         IF (cfg_base%StorageHeatMethod == 5) THEN
            IF (.NOT. vec_equal(ob_m3%dataOutBlockEHC(r, :), set_nan(ref_lines(r)%dataOutLineEHC))) THEN
               err = 270_c_int
               RETURN
            END IF
         ELSE
            IF (ANY(ob_m3%dataOutBlockEHC(r, :) /= 0.0D0)) THEN
               err = 271_c_int
               RETURN
            END IF
         END IF
         ! Legacy producer boundaries: ESTM stays 0.0D0 (disabled #1802);
         ! dataOutBlockNHood is not transferred from dataOutBlockNHood_X (stays -999.0D0).
         IF (ANY(ob_m3%dataOutBlockESTM(r, :) /= 0.0D0)) THEN
            err = 272_c_int
            RETURN
         END IF
         IF (ANY(ob_m3%dataOutBlockNHood(r, :) /= -999.0D0)) THEN
            err = 273_c_int
            RETURN
         END IF
      END DO

      ! Cross-check concatenated SUEWS_cal_multitsteps_dts slices against ob_m3.
      off = 0
      IF (.NOT. mat_equal(ob_m3%dataOutBlockSUEWS(:, 6:), dts_data(:, off + 1:off + ncolumnsDataOutSUEWS - 5))) THEN
         err = 280_c_int
         RETURN
      END IF
      off = off + (ncolumnsDataOutSUEWS - 5)
      IF (cfg_base%SnowUse == 1) THEN
         IF (.NOT. mat_equal(ob_m3%dataOutBlockSnow(:, 6:), set_nan(dts_data(:, off + 1:off + ncolumnsDataOutSnow - 5)))) THEN
            err = 281_c_int
            RETURN
         END IF
      END IF
      off = off + (ncolumnsDataOutSnow - 5)
      IF (.NOT. mat_equal(ob_m3%dataOutBlockBEERS(:, 6:), set_nan(dts_data(:, off + 1:off + ncolumnsDataOutBEERS - 5)))) THEN
         err = 282_c_int
         RETURN
      END IF
      off = off + (ncolumnsDataOutBEERS - 5)
      off = off + (ncolumnsDataOutESTM - 5)
      IF (cfg_base%StorageHeatMethod == 5) THEN
         IF (.NOT. mat_equal(ob_m3%dataOutBlockEHC(:, 6:), set_nan(dts_data(:, off + 1:off + ncolumnsDataOutEHC - 5)))) THEN
            err = 283_c_int
            RETURN
         END IF
      END IF
      off = off + (ncolumnsDataOutEHC - 5)
      IF (.NOT. mat_equal(ob_m3%dataOutBlockDailyState(:, 6:), dts_data(:, off + 1:off + ncolumnsDataOutDailyState - 5))) THEN
         err = 284_c_int
         RETURN
      END IF
      off = off + (ncolumnsDataOutDailyState - 5)
      IF (.NOT. mat_equal(ob_m3%dataOutBlockRSL(:, 6:), dts_data(:, off + 1:off + ncolumnsDataOutRSL - 5))) THEN
         err = 285_c_int
         RETURN
      END IF
      off = off + (ncolumnsDataOutRSL - 5)
      IF (ANY(ob_m3%dataOutBlockDebug(:, 6) /= 1.0D0) .OR. &
          ANY(dts_data(:, off + 1) /= 0.0D0) .OR. &
          .NOT. mat_equal(ob_m3%dataOutBlockDebug(:, 7:), dts_data(:, off + 2:off + ncolumnsDataOutDebug - 5))) THEN
         err = 286_c_int
         RETURN
      END IF
      off = off + (ncolumnsDataOutDebug - 5)
      IF (.NOT. mat_equal(ob_m3%dataOutBlockSPARTACUS(:, 6:), dts_data(:, off + 1:off + ncolumnsDataOutSPARTACUS - 5))) THEN
         err = 287_c_int
         RETURN
      END IF
      off = off + (ncolumnsDataOutSPARTACUS - 5)
      IF (.NOT. mat_equal(ob_m3%dataOutBlockSTEBBS(:, 6:), dts_data(:, off + 1:off + ncolumnsDataOutSTEBBS - 5))) THEN
         err = 288_c_int
         RETURN
      END IF
      IF (.NOT. states_equal(blk_m3%BLOCK(nsteps), state_dts)) THEN
         err = 289_c_int
         RETURN
      END IF

      ! Export runtime column widths in OUTPUT_BLOCK_FIELD_ORDER.
      block_widths(1) = INT(SIZE(ob_m3%dataOutBlockSUEWS, 2), c_int)
      block_widths(2) = INT(SIZE(ob_m3%dataOutBlockSnow, 2), c_int)
      block_widths(3) = INT(SIZE(ob_m3%dataOutBlockESTM, 2), c_int)
      block_widths(4) = INT(SIZE(ob_m3%dataOutBlockEHC, 2), c_int)
      block_widths(5) = INT(SIZE(ob_m3%dataOutBlockRSL, 2), c_int)
      block_widths(6) = INT(SIZE(ob_m3%dataOutBlockBEERS, 2), c_int)
      block_widths(7) = INT(SIZE(ob_m3%dataOutBlockDebug, 2), c_int)
      block_widths(8) = INT(SIZE(ob_m3%dataOutBlockSPARTACUS, 2), c_int)
      block_widths(9) = INT(SIZE(ob_m3%dataOutBlockDailyState, 2), c_int)
      block_widths(10) = INT(SIZE(ob_m3%dataOutBlockSTEBBS, 2), c_int)
      block_widths(11) = INT(SIZE(ob_m3%dataOutBlockNHood, 2), c_int)

      ! Export per-row named fields from production block_mod_state%BLOCK(1..3)
      ! and production output_block_suews%dataOutBlock*.
      DO r = 1, nsteps
         state_block_out(1, r) = blk_m3%BLOCK(r)%heatState%qn
         state_block_out(2, r) = blk_m3%BLOCK(r)%heatState%qs
         state_block_out(3, r) = blk_m3%BLOCK(r)%heatState%qf
         state_block_out(4, r) = blk_m3%BLOCK(r)%heatState%qh
         state_block_out(5, r) = blk_m3%BLOCK(r)%heatState%qe
         state_block_out(6, r) = blk_m3%BLOCK(r)%heatState%TSfc_C
         state_block_out(7, r) = blk_m3%BLOCK(r)%heatState%temp_roof(1, 1)
         state_block_out(8, r) = blk_m3%BLOCK(r)%hydroState%state_surf(1)
         state_block_out(9, r) = blk_m3%BLOCK(r)%hydroState%drain_per_tstep
         state_block_out(10, r) = blk_m3%BLOCK(r)%hydroState%smd
         state_block_out(11, r) = blk_m3%BLOCK(r)%phenState%lenDay_id
         state_block_out(12, r) = blk_m3%BLOCK(r)%phenState%g_kdown
         state_block_out(13, r) = blk_m3%BLOCK(r)%atmState%t2_C
         state_block_out(14, r) = blk_m3%BLOCK(r)%anthroemisState%Fc_biogen
         state_block_out(15, r) = blk_m3%BLOCK(r)%stebbsState%kdown_2d
         state_block_out(16, r) = MERGE(1.0_c_double, 0.0_c_double, blk_m3%BLOCK(r)%flagState%flag_converge)

         output_block_out(1, r) = ob_m3%dataOutBlockSUEWS(r, 4)
         output_block_out(2, r) = ob_m3%dataOutBlockSUEWS(r, 11)
         output_block_out(3, r) = ob_m3%dataOutBlockSUEWS(r, 12)
         output_block_out(4, r) = ob_m3%dataOutBlockSUEWS(r, 13)
         output_block_out(5, r) = ob_m3%dataOutBlockSUEWS(r, 14)
         output_block_out(6, r) = ob_m3%dataOutBlockSUEWS(r, 15)
         output_block_out(7, r) = ob_m3%dataOutBlockSUEWS(r, 87)
         output_block_out(8, r) = ob_m3%dataOutBlockSUEWS(r, 67)
         output_block_out(9, r) = ob_m3%dataOutBlockSnow(r, 6)
         output_block_out(10, r) = ob_m3%dataOutBlockESTM(r, 6)
         output_block_out(11, r) = ob_m3%dataOutBlockEHC(r, 6)
         output_block_out(12, r) = ob_m3%dataOutBlockRSL(r, 66)
         output_block_out(13, r) = ob_m3%dataOutBlockBEERS(r, 11)
         output_block_out(14, r) = ob_m3%dataOutBlockBEERS(r, 18)
         output_block_out(15, r) = ob_m3%dataOutBlockDebug(r, 120)
         output_block_out(16, r) = ob_m3%dataOutBlockSPARTACUS(r, 6)
         output_block_out(17, r) = ob_m3%dataOutBlockDailyState(r, 20)
         output_block_out(18, r) = ob_m3%dataOutBlockDailyState(r, 10)
         output_block_out(19, r) = ob_m3%dataOutBlockSTEBBS(r, 6)
         output_block_out(20, r) = ob_m3%dataOutBlockNHood(r, 6)

         DO il = 1, INT(nlayer)
            DO id_w = 1, INT(ndepth_in)
               widx = ((r - 1)*INT(nlayer) + (il - 1))*INT(ndepth_in) + id_w
               wall_temp_block_out(widx) = blk_m3%BLOCK(r)%heatState%temp_wall(il, id_w)
               wall_temp_ref_out(widx) = ref_states(r)%heatState%temp_wall(il, id_w)
            END DO
         END DO
      END DO

      DO il = 1, INT(nlayer)
         DO id_w = 1, INT(ndepth_in)
            widx = (il - 1)*INT(ndepth_in) + id_w
            wall_temp_dts_final_out(widx) = state_dts%heatState%temp_wall(il, id_w)
         END DO
      END DO

      CALL extract_debug_witnesses(dbg_m3, final_debug_out)
      CALL ob_m0%cleanup()
      CALL ob_m1%cleanup()
      CALL ob_m2%cleanup()
      CALL ob_m3%cleanup()
      CALL ob_m4%cleanup()
      DEALLOCATE (dts_data, dts_dates)
      err = 0_c_int
   END SUBROUTINE multiline_block_observe

   SUBROUTINE init_site_and_state( &
      site, state, nlayer_i, ndepth_i, config, &
      scalars, site_flat, site_len, site_toc, state_flat, state_len, state_toc, err)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double
      USE module_c_api_driver, ONLY: unpack_site_scalars, unpack_site_members, unpack_state_members
      IMPLICIT NONE
      TYPE(SUEWS_SITE), INTENT(INOUT) :: site
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer_i, ndepth_i
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      REAL(c_double), INTENT(IN) :: scalars(24), site_flat(*), state_flat(*)
      INTEGER(c_int), INTENT(IN) :: site_len, state_len, site_toc(36), state_toc(26)
      INTEGER(c_int), INTENT(OUT) :: err
      INTEGER :: ib

      CALL site%allocate(nlayer_i)
      site%nlayer = nlayer_i
      CALL site%ehc%allocate(nlayer_i, ndepth_i)
      CALL site%spartacus_layer%allocate(nlayer_i)
      CALL site%spartacus%allocate(nlayer_i)

      CALL state%allocate(nlayer_i, ndepth_i)
      CALL state%stebbsState%allocate(1, nlayer_i)
      IF (ALLOCATED(state%stebbsState%buildings)) THEN
         DO ib = 1, SIZE(state%stebbsState%buildings)
            CALL state%stebbsState%buildings(ib)%allocate(nlayer_i)
         END DO
      END IF

      CALL unpack_site_scalars(scalars, 24_c_int, site, err)
      IF (err /= 0_c_int) RETURN
      CALL unpack_site_members(site, nlayer_i, ndepth_i, site_flat, site_len, site_toc, 36_c_int, 18_c_int, err)
      IF (err /= 0_c_int) RETURN
      CALL site%cal_surf(config)
      CALL unpack_state_members(state, nlayer_i, ndepth_i, state_flat, state_len, state_toc, 26_c_int, 13_c_int, err)
   END SUBROUTINE init_site_and_state

   SUBROUTINE load_row_forcing(forcing, row24, wu7)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_double
      IMPLICIT NONE
      TYPE(SUEWS_FORCING), INTENT(INOUT) :: forcing
      REAL(c_double), INTENT(IN) :: row24(24), wu7(7)

      forcing%qn1_obs = row24(5)
      forcing%qs_obs = row24(8)
      forcing%qf_obs = row24(9)
      forcing%U = row24(10)
      forcing%RH = row24(11)
      forcing%temp_c = row24(12)
      forcing%pres = row24(13)
      forcing%rain = row24(14)
      forcing%kdown = row24(15)
      forcing%snow_fraction = row24(16)
      forcing%l_down = row24(17)
      forcing%f_cloud = row24(18)
      forcing%xsmd = row24(20)
      forcing%LAI_evetr = row24(21)
      forcing%LAI_dectr = row24(22)
      forcing%LAI_grass = row24(23)
      forcing%Wu_mm_paved = wu7(1)
      forcing%Wu_mm_bldgs = wu7(2)
      forcing%Wu_mm_evetr = wu7(3)
      forcing%Wu_mm_dectr = wu7(4)
      forcing%Wu_mm_grass = wu7(5)
      forcing%Wu_mm_bsoil = wu7(6)
      forcing%Wu_mm_water = wu7(7)
      forcing%kdiff = 0.0_c_double
      forcing%kdir = 0.0_c_double
   END SUBROUTINE load_row_forcing

   SUBROUTINE check_state_allocs(st, nl, nd, check_buildings, err_alloc, err_shape, status)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int
      IMPLICIT NONE
      TYPE(SUEWS_STATE), INTENT(IN) :: st
      INTEGER(c_int), INTENT(IN) :: nl, nd, err_alloc, err_shape
      LOGICAL, INTENT(IN) :: check_buildings
      INTEGER(c_int), INTENT(OUT) :: status
      INTEGER :: ib

      status = 0_c_int
      IF (.NOT. ALLOCATED(st%heatState%temp_roof) .OR. &
          .NOT. ALLOCATED(st%heatState%temp_wall) .OR. &
          .NOT. ALLOCATED(st%heatState%temp_surf) .OR. &
          .NOT. ALLOCATED(st%heatState%temp_surf_dyohm) .OR. &
          .NOT. ALLOCATED(st%heatState%temp_surf_ehc_fast) .OR. &
          .NOT. ALLOCATED(st%heatState%temp_surf_ehc_slow) .OR. &
          .NOT. ALLOCATED(st%heatState%tsfc_roof) .OR. &
          .NOT. ALLOCATED(st%heatState%tsfc_wall) .OR. &
          .NOT. ALLOCATED(st%heatState%tsfc_surf) .OR. &
          .NOT. ALLOCATED(st%heatState%tsfc_surf_dyohm) .OR. &
          .NOT. ALLOCATED(st%heatState%tsfc_roof_stepstart) .OR. &
          .NOT. ALLOCATED(st%heatState%tsfc_wall_stepstart) .OR. &
          .NOT. ALLOCATED(st%heatState%tsfc_surf_stepstart) .OR. &
          .NOT. ALLOCATED(st%heatState%QS_roof) .OR. &
          .NOT. ALLOCATED(st%heatState%QN_roof) .OR. &
          .NOT. ALLOCATED(st%heatState%qe_roof) .OR. &
          .NOT. ALLOCATED(st%heatState%qh_roof) .OR. &
          .NOT. ALLOCATED(st%heatState%qh_resist_roof) .OR. &
          .NOT. ALLOCATED(st%heatState%QS_wall) .OR. &
          .NOT. ALLOCATED(st%heatState%QN_wall) .OR. &
          .NOT. ALLOCATED(st%heatState%qe_wall) .OR. &
          .NOT. ALLOCATED(st%heatState%qh_wall) .OR. &
          .NOT. ALLOCATED(st%heatState%qh_resist_wall) .OR. &
          .NOT. ALLOCATED(st%hydroState%soil_store_roof) .OR. &
          .NOT. ALLOCATED(st%hydroState%state_roof) .OR. &
          .NOT. ALLOCATED(st%hydroState%soil_store_wall) .OR. &
          .NOT. ALLOCATED(st%hydroState%state_wall) .OR. &
          .NOT. ALLOCATED(st%hydroState%ev_roof) .OR. &
          .NOT. ALLOCATED(st%hydroState%ev_wall)) THEN
         status = err_alloc
         RETURN
      END IF

      IF (INT(SIZE(st%heatState%temp_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%temp_roof, 2), c_int) /= nd .OR. &
          INT(SIZE(st%heatState%temp_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%temp_wall, 2), c_int) /= nd .OR. &
          INT(SIZE(st%heatState%temp_surf, 1), c_int) /= INT(nsurf, c_int) .OR. &
          INT(SIZE(st%heatState%temp_surf, 2), c_int) /= nd .OR. &
          INT(SIZE(st%heatState%temp_surf_dyohm, 1), c_int) /= INT(nsurf, c_int) .OR. &
          INT(SIZE(st%heatState%temp_surf_dyohm, 2), c_int) /= nd .OR. &
          INT(SIZE(st%heatState%temp_surf_ehc_fast, 1), c_int) /= INT(nsurf, c_int) .OR. &
          INT(SIZE(st%heatState%temp_surf_ehc_fast, 2), c_int) /= nd .OR. &
          INT(SIZE(st%heatState%temp_surf_ehc_slow, 1), c_int) /= INT(nsurf, c_int) .OR. &
          INT(SIZE(st%heatState%temp_surf_ehc_slow, 2), c_int) /= nd .OR. &
          INT(SIZE(st%heatState%tsfc_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%tsfc_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%tsfc_surf, 1), c_int) /= INT(nsurf, c_int) .OR. &
          INT(SIZE(st%heatState%tsfc_surf_dyohm, 1), c_int) /= INT(nsurf, c_int) .OR. &
          INT(SIZE(st%heatState%tsfc_roof_stepstart, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%tsfc_wall_stepstart, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%tsfc_surf_stepstart, 1), c_int) /= INT(nsurf, c_int) .OR. &
          INT(SIZE(st%heatState%QS_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%QN_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%qe_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%qh_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%qh_resist_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%QS_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%QN_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%qe_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%qh_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%heatState%qh_resist_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%hydroState%soil_store_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%hydroState%state_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%hydroState%soil_store_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%hydroState%state_wall, 1), c_int) /= nl .OR. &
          INT(SIZE(st%hydroState%ev_roof, 1), c_int) /= nl .OR. &
          INT(SIZE(st%hydroState%ev_wall, 1), c_int) /= nl) THEN
         status = err_shape
         RETURN
      END IF

      IF (check_buildings) THEN
         IF (.NOT. ALLOCATED(st%stebbsState%buildings)) THEN
            status = err_alloc
            RETURN
         END IF
         IF (SIZE(st%stebbsState%buildings) < 1) THEN
            status = err_shape
            RETURN
         END IF
         DO ib = 1, SIZE(st%stebbsState%buildings)
            IF (.NOT. ALLOCATED(st%stebbsState%buildings(ib)%Textroof_C) .OR. &
                .NOT. ALLOCATED(st%stebbsState%buildings(ib)%Textwall_C)) THEN
               status = err_alloc
               RETURN
            END IF
            IF (INT(SIZE(st%stebbsState%buildings(ib)%Textroof_C, 1), c_int) /= nl .OR. &
                INT(SIZE(st%stebbsState%buildings(ib)%Textwall_C, 1), c_int) /= nl) THEN
               status = err_shape
               RETURN
            END IF
         END DO
      END IF
   END SUBROUTINE check_state_allocs

   SUBROUTINE check_stage_alloc(st, stage_idx, step_idx, nl, nd, check_buildings, dims_out, status)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int
      IMPLICIT NONE
      TYPE(SUEWS_STATE), INTENT(IN) :: st
      INTEGER, INTENT(IN) :: stage_idx, step_idx
      INTEGER(c_int), INTENT(IN) :: nl, nd
      LOGICAL, INTENT(IN) :: check_buildings
      INTEGER(c_int), INTENT(INOUT) :: dims_out(2, 15, 3)
      INTEGER(c_int), INTENT(OUT) :: status
      INTEGER(c_int) :: stage_err

      stage_err = INT(110 + stage_idx, c_int)
      CALL check_state_allocs(st, nl, nd, check_buildings, stage_err, stage_err, status)
      IF (status /= 0_c_int) RETURN
      dims_out(1, stage_idx, step_idx) = INT(SIZE(st%heatState%temp_roof, 1), c_int)
      dims_out(2, stage_idx, step_idx) = INT(SIZE(st%heatState%temp_roof, 2), c_int)
   END SUBROUTINE check_stage_alloc

   SUBROUTINE check_all_stage_allocs(dbg, step_idx, nl, nd, snow_use, dims_out, status)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int
      IMPLICIT NONE
      TYPE(SUEWS_DEBUG), INTENT(IN) :: dbg
      INTEGER, INTENT(IN) :: step_idx, snow_use
      INTEGER(c_int), INTENT(IN) :: nl, nd
      INTEGER(c_int), INTENT(INOUT) :: dims_out(2, 15, 3)
      INTEGER(c_int), INTENT(OUT) :: status

      CALL check_stage_alloc(dbg%state_01_dailystate, 1, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_02_soilmoist, 2, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_03_wateruse, 3, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_04_anthroemis, 4, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_05_qn, 5, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_06_qs, 6, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_07_qhqe_lumps, 7, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_08_water, 8, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_09_resist, 9, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      ! Explicit exclusion: when SnowUse == 1, SUEWS_cal_Main_impl executes
      ! SUEWS_cal_snow instead of SUEWS_cal_QE and does not assign state_10_qe,
      ! so state_10_qe retains debugState%init's heat/hydro allocation without
      ! stebbsState%buildings.
      CALL check_stage_alloc(dbg%state_10_qe, 10, step_idx, nl, nd, snow_use /= 1, dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_11_qh, 11, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_12_tsurf, 12, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_13_rsl, 13, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_14_biogenco2, 14, step_idx, nl, nd, .TRUE., dims_out, status)
      IF (status /= 0_c_int) RETURN
      CALL check_stage_alloc(dbg%state_15_beers, 15, step_idx, nl, nd, .TRUE., dims_out, status)
   END SUBROUTINE check_all_stage_allocs

   ! Independent stage-by-stage boundary oracle executed outside
   ! SUEWS_cal_Main_impl by calling the individual physics routines directly.
   ! Produces both the full 15-stage oracle_debug snapshot and the final
   ! oracle_state so any capture corruption inside SUEWS_cal_Main_impl or
   ! run_multitsteps_block (on any scalar, fixed array, or allocatable field
   ! of any of the 13 sub-states) is caught independently.
   SUBROUTINE replay_timestep_stages( &
      timer_in, forcing_in, config_in, siteInfo, state_in, oracle_debug, oracle_state, status)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int
      IMPLICIT NONE
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer_in
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing_in
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config_in
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(SUEWS_STATE), INTENT(IN) :: state_in
      TYPE(SUEWS_DEBUG), INTENT(OUT) :: oracle_debug
      TYPE(SUEWS_STATE), INTENT(OUT) :: oracle_state
      INTEGER(c_int), INTENT(OUT) :: status

      INTEGER, PARAMETER :: max_iter = 60
      TYPE(SUEWS_TIMER) :: tmr
      TYPE(SUEWS_FORCING) :: frc
      TYPE(SUEWS_STATE) :: st_tstepstart
      REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine
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
      REAL(KIND(1D0)) :: w_f_pass_prev, zl_pass_start, w_f_pass_start
      INTEGER :: nl

      status = 0_c_int
      tmr = timer_in
      frc = forcing_in
      oracle_state = state_in
      nl = siteInfo%nlayer

      CALL SUEWS_cal_timer_reference( &
         tmr, siteInfo, siteInfo%anthroemis, config_in%ForcingTimestampReference)

      CALL st_tstepstart%ALLOCATE(nl, ndepth)
      CALL oracle_debug%init(nl, ndepth)

      IF (config_in%snowuse == 0) THEN
         oracle_state%snowState%snow_density = 0.0D0
         oracle_state%snowState%snow_fraction = 0.0D0
         oracle_state%snowState%snow_water = 0.0D0
         oracle_state%snowState%snow_albedo = 0.0D0
         oracle_state%snowState%ice_frac = 0.0D0
         oracle_state%snowState%snow_pack = 0.0D0
      END IF

      st_tstepstart = oracle_state

      oracle_state%snowState%swe = 0.0D0
      oracle_state%snowState%mwh = 0.0D0
      oracle_state%snowState%melt_water_store = 0.0D0
      oracle_state%snowState%chSnow_per_interval = 0.0D0
      oracle_state%snowState%snow_removal = 0.0D0
      oracle_state%snowState%qm = 0.0D0
      oracle_state%snowState%qm_freeze = 0.0D0
      oracle_state%snowState%qm_rain = 0.0D0
      oracle_state%heatState%qe0_surf = 0.0D0
      oracle_state%hydroState%ev0_surf = 0.0D0
      oracle_state%hydroState%ev_surf = 0.0D0

      oracle_state%snowState%snow_fraction = MERGE( &
         frc%snow_fraction, oracle_state%snowState%snow_fraction, config_in%NetRadiationMethod == 0)

      datetimeLine = 0.0D0
      dataOutLineSUEWS = 0.0D0
      dataOutLineSnow = -999.0D0
      dataOutLineESTM = -999.0D0
      dataOutLineEHC = -999.0D0
      dataoutLineRSL = -999.0D0
      dataOutLineBEERS = -999.0D0
      dataOutLineDebug = -999.0D0
      dataOutLineSPARTACUS = -999.0D0
      dataOutLineDailyState = -999.0D0
      dataOutLineSTEBBS = -999.0D0

      oracle_state%flagState%flag_converge = .FALSE.
      oracle_state%heatState%tsfc_surf_stepstart = oracle_state%heatState%tsfc_surf
      IF (config_in%StorageHeatMethod == 5 .OR. config_in%NetRadiationMethod > 1000) THEN
         oracle_state%heatState%tsfc_roof_stepstart = oracle_state%heatState%tsfc_roof
         oracle_state%heatState%tsfc_wall_stepstart = oracle_state%heatState%tsfc_wall
      END IF

      CALL SUEWS_cal_RoughnessParameters(tmr, config_in, frc, siteInfo, oracle_state)
      IF (supy_error_flag()) THEN
         status = 401_c_int
         RETURN
      END IF
      CALL NARP_update_SunPosition(tmr, siteInfo, oracle_state)
      IF (supy_error_flag()) THEN
         status = 402_c_int
         RETURN
      END IF
      CALL SUEWS_update_atmState(tmr, frc, oracle_state)
      IF (supy_error_flag()) THEN
         status = 403_c_int
         RETURN
      END IF

      oracle_state%flagState%i_iter = 1
      w_f_pass_prev = 0.0D0
      DO WHILE (oracle_state%flagState%i_iter < 3 .OR. &
                ((.NOT. oracle_state%flagState%flag_converge) .AND. &
                 (oracle_state%flagState%i_iter < max_iter)))
         IF (((.NOT. oracle_state%flagState%flag_converge) .AND. oracle_state%flagState%i_iter > 1) .OR. &
             (oracle_state%flagState%flag_converge .AND. oracle_state%flagState%i_iter == 2)) THEN
            CALL oracle_state%check_and_reset_states(st_tstepstart)
            IF (config_in%StorageHeatMethod == 5) THEN
               CALL reset_ehc_heat_state_for_tsurf_iteration( &
                  oracle_state%heatState, st_tstepstart%heatState)
            END IF
         END IF

         CALL SUEWS_cal_DailyState(tmr, config_in, frc, siteInfo, oracle_state)
         IF (supy_error_flag()) THEN
            status = 411_c_int
            RETURN
         END IF
         oracle_debug%state_01_dailystate = oracle_state

         CALL SUEWS_update_SoilMoist(tmr, config_in, frc, siteInfo, oracle_state)
         IF (supy_error_flag()) THEN
            status = 412_c_int
            RETURN
         END IF
         oracle_debug%state_02_soilmoist = oracle_state

         CALL SUEWS_cal_WaterUse(tmr, config_in, frc, siteInfo, oracle_state)
         IF (supy_error_flag()) THEN
            status = 413_c_int
            RETURN
         END IF
         oracle_debug%state_03_wateruse = oracle_state

         CALL SUEWS_cal_AnthropogenicEmission(tmr, config_in, frc, siteInfo, oracle_state)
         IF (supy_error_flag()) THEN
            status = 414_c_int
            RETURN
         END IF
         oracle_debug%state_04_anthroemis = oracle_state

         CALL SUEWS_cal_Qn(tmr, config_in, frc, siteInfo, oracle_state, dataOutLineSPARTACUS)
         IF (supy_error_flag()) THEN
            status = 415_c_int
            RETURN
         END IF
         oracle_debug%state_05_qn = oracle_state

         CALL SUEWS_cal_Qs( &
            tmr, config_in, frc, siteInfo, datetimeLine, &
            oracle_state, dataOutLineESTM, dataOutLineSTEBBS)
         IF (supy_error_flag()) THEN
            status = 416_c_int
            RETURN
         END IF
         oracle_debug%state_06_qs = oracle_state

         CALL LUMPS_cal_QHQE(tmr, config_in, frc, siteInfo, oracle_state)
         IF (supy_error_flag()) THEN
            status = 417_c_int
            RETURN
         END IF
         oracle_debug%state_07_qhqe_lumps = oracle_state

         CALL SUEWS_cal_Water(tmr, config_in, frc, siteInfo, oracle_state)
         IF (supy_error_flag()) THEN
            status = 418_c_int
            RETURN
         END IF
         oracle_debug%state_08_water = oracle_state

         zl_pass_start = oracle_state%atmState%z_l
         w_f_pass_start = w_f_pass_prev
         CALL SUEWS_cal_Resistance(tmr, config_in, frc, siteInfo, oracle_state, w_f_pass_prev)
         IF (supy_error_flag()) THEN
            status = 419_c_int
            RETURN
         END IF
         oracle_debug%state_09_resist = oracle_state

         IF (config_in%SnowUse == 1) THEN
            IF (.NOT. oracle_state%flagState%snow_warning_shown) THEN
               oracle_state%flagState%snow_warning_shown = .TRUE.
            END IF
            CALL SUEWS_cal_snow(tmr, config_in, frc, siteInfo, oracle_state, dataOutLineSnow)
            IF (supy_error_flag()) THEN
               status = 420_c_int
               RETURN
            END IF
         ELSE
            CALL SUEWS_cal_QE(tmr, config_in, frc, siteInfo, oracle_state)
            IF (supy_error_flag()) THEN
               status = 420_c_int
               RETURN
            END IF
            oracle_debug%state_10_qe = oracle_state
         END IF

         CALL SUEWS_cal_QH(tmr, config_in, frc, siteInfo, oracle_state)
         IF (supy_error_flag()) THEN
            status = 421_c_int
            RETURN
         END IF
         oracle_debug%state_11_qh = oracle_state

         IF (config_in%StorageHeatMethod == 5) THEN
            oracle_state%atmState%z_l = zl_pass_start
            w_f_pass_prev = w_f_pass_start
            CALL SUEWS_cal_Resistance(tmr, config_in, frc, siteInfo, oracle_state, w_f_pass_prev)
            IF (supy_error_flag()) THEN
               status = 422_c_int
               RETURN
            END IF
         END IF

         CALL suews_update_tsurf(tmr, config_in, frc, siteInfo, oracle_state)
         IF (supy_error_flag()) THEN
            status = 423_c_int
            RETURN
         END IF
         oracle_state%flagState%flag_converge = oracle_state%flagState%flag_converge .AND. &
            ABS(oracle_state%atmState%z_l - zl_pass_start) < 0.005D0 .AND. &
            ABS(w_f_pass_prev - w_f_pass_start) < 0.005D0
         oracle_debug%state_12_tsurf = oracle_state

         oracle_state%flagState%i_iter = oracle_state%flagState%i_iter + 1
      END DO

      IF (oracle_state%heatState%QH * oracle_state%atmState%z_l > 0.0D0) THEN
         CALL SUEWS_cal_Resistance( &
            tmr, config_in, frc, siteInfo, oracle_state, w_f_pass_prev, skip_relaxation=.TRUE.)
         IF (supy_error_flag()) THEN
            status = 424_c_int
            RETURN
         END IF
      END IF

      CALL RSLProfile(tmr, config_in, frc, siteInfo, oracle_state, dataoutLineRSL)
      IF (supy_error_flag()) THEN
         status = 425_c_int
         RETURN
      END IF
      oracle_debug%state_13_rsl = oracle_state

      CALL SUEWS_cal_BiogenCO2(tmr, config_in, frc, siteInfo, oracle_state)
      IF (supy_error_flag()) THEN
         status = 426_c_int
         RETURN
      END IF
      oracle_debug%state_14_biogenco2 = oracle_state

      CALL BEERS_cal_main(tmr, config_in, frc, siteInfo, oracle_state, dataOutLineBEERS)
      IF (supy_error_flag()) THEN
         status = 427_c_int
         RETURN
      END IF
      oracle_debug%state_15_beers = oracle_state

      CALL suews_update_tsurf_dyohm(tmr, config_in, frc, siteInfo, oracle_state)
      IF (supy_error_flag()) THEN
         status = 428_c_int
         RETURN
      END IF

      CALL SUEWS_update_outputLine( &
         tmr, config_in, frc, siteInfo, oracle_state, datetimeLine, dataOutLineSUEWS)
      IF (supy_error_flag()) THEN
         status = 429_c_int
         RETURN
      END IF

      IF (config_in%StorageHeatMethod == 5) THEN
         CALL EHC_update_outputLine(tmr, oracle_state, datetimeLine, dataOutLineEHC)
      END IF

      CALL update_DailyStateLine(tmr, config_in, frc, siteInfo, oracle_state, dataOutLineDailyState)
      CALL update_debug_info(tmr, config_in, frc, siteInfo, st_tstepstart, oracle_state, dataOutLineDebug)
      CALL sync_error_to_state(oracle_state, tmr)
   END SUBROUTINE replay_timestep_stages

   ! Verify nested allocatable thermal and building-hydro arrays across all 15
   ! captured stages against independent anchors: step_start before the owning
   ! stage and the independent no-debug control_state from the owning stage
   ! onward (heatState temp_roof/temp_wall/temp_surf owned by stage 06 QS;
   ! hydroState state_roof/soil_store_roof/state_wall/soil_store_wall owned by
   ! stage 10 QE). Explicit exclusion: stage 10 is skipped when snow_use == 1
   ! because SUEWS_cal_Main_impl runs SUEWS_cal_snow instead of SUEWS_cal_QE.
   SUBROUTINE check_one_stage_nested(st, heat_anchor, hydro_anchor, stage_idx, status)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int
      IMPLICIT NONE
      TYPE(SUEWS_STATE), INTENT(IN) :: st, heat_anchor, hydro_anchor
      INTEGER, INTENT(IN) :: stage_idx
      INTEGER(c_int), INTENT(OUT) :: status

      status = 0_c_int
      IF (.NOT. alloc_mat_equal(st%heatState%temp_roof, heat_anchor%heatState%temp_roof) .OR. &
          .NOT. alloc_mat_equal(st%heatState%temp_wall, heat_anchor%heatState%temp_wall) .OR. &
          .NOT. alloc_mat_equal(st%heatState%temp_surf, heat_anchor%heatState%temp_surf) .OR. &
          .NOT. alloc_mat_equal(st%heatState%temp_surf_ehc_fast, heat_anchor%heatState%temp_surf_ehc_fast) .OR. &
          .NOT. alloc_mat_equal(st%heatState%temp_surf_ehc_slow, heat_anchor%heatState%temp_surf_ehc_slow) .OR. &
          .NOT. alloc_vec_equal(st%hydroState%state_roof, hydro_anchor%hydroState%state_roof) .OR. &
          .NOT. alloc_vec_equal(st%hydroState%soil_store_roof, hydro_anchor%hydroState%soil_store_roof) .OR. &
          .NOT. alloc_vec_equal(st%hydroState%state_wall, hydro_anchor%hydroState%state_wall) .OR. &
          .NOT. alloc_vec_equal(st%hydroState%soil_store_wall, hydro_anchor%hydroState%soil_store_wall)) THEN
         status = INT(140 + stage_idx, c_int)
      END IF
   END SUBROUTINE check_one_stage_nested

   SUBROUTINE check_stage_nested_invariants(dbg, step_start, control_state, snow_use, status)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int
      IMPLICIT NONE
      TYPE(SUEWS_DEBUG), INTENT(IN) :: dbg
      TYPE(SUEWS_STATE), INTENT(IN) :: step_start, control_state
      INTEGER, INTENT(IN) :: snow_use
      INTEGER(c_int), INTENT(OUT) :: status

      CALL check_one_stage_nested(dbg%state_01_dailystate, step_start, step_start, 1, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_02_soilmoist, step_start, step_start, 2, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_03_wateruse, step_start, step_start, 3, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_04_anthroemis, step_start, step_start, 4, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_05_qn, step_start, step_start, 5, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_06_qs, control_state, step_start, 6, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_07_qhqe_lumps, control_state, step_start, 7, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_08_water, control_state, step_start, 8, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_09_resist, control_state, step_start, 9, status)
      IF (status /= 0_c_int) RETURN
      IF (snow_use /= 1) THEN
         CALL check_one_stage_nested(dbg%state_10_qe, control_state, control_state, 10, status)
         IF (status /= 0_c_int) RETURN
      END IF
      CALL check_one_stage_nested(dbg%state_11_qh, control_state, control_state, 11, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_12_tsurf, control_state, control_state, 12, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_13_rsl, control_state, control_state, 13, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_14_biogenco2, control_state, control_state, 14, status)
      IF (status /= 0_c_int) RETURN
      CALL check_one_stage_nested(dbg%state_15_beers, control_state, control_state, 15, status)
   END SUBROUTINE check_stage_nested_invariants

   SUBROUTINE extract_debug_witnesses(dbg, w)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_double
      IMPLICIT NONE
      TYPE(SUEWS_DEBUG), INTENT(IN) :: dbg
      REAL(c_double), INTENT(OUT) :: w(15)

      w(1) = dbg%state_01_dailystate%phenState%lenDay_id
      w(2) = dbg%state_02_soilmoist%hydroState%vsmd
      w(3) = dbg%state_03_wateruse%hydroState%wu_ext
      w(4) = dbg%state_04_anthroemis%heatState%qf
      w(5) = dbg%state_05_qn%heatState%qn
      w(6) = dbg%state_06_qs%heatState%qs
      w(7) = dbg%state_07_qhqe_lumps%heatState%QH_LUMPS
      w(8) = dbg%state_08_water%hydroState%drain_per_tstep
      w(9) = dbg%state_09_resist%phenState%g_kdown
      w(10) = dbg%state_10_qe%heatState%qe
      w(11) = dbg%state_11_qh%heatState%qh
      w(12) = dbg%state_12_tsurf%heatState%TSfc_C
      w(13) = dbg%state_13_rsl%atmState%t2_C
      w(14) = dbg%state_14_biogenco2%anthroemisState%Fc_biogen
      w(15) = dbg%state_15_beers%stebbsState%kdown_2d
   END SUBROUTINE extract_debug_witnesses

   LOGICAL FUNCTION reals_eq(x, y)
      USE, INTRINSIC :: ieee_arithmetic, ONLY: ieee_is_nan
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(IN) :: x, y

      IF (ieee_is_nan(x) .AND. ieee_is_nan(y)) THEN
         reals_eq = .TRUE.
      ELSE
         reals_eq = (x == y)
      END IF
   END FUNCTION reals_eq

   LOGICAL FUNCTION vec_equal(a, b)
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(IN) :: a(:), b(:)
      INTEGER :: i

      vec_equal = .FALSE.
      IF (SIZE(a) /= SIZE(b)) RETURN
      DO i = 1, SIZE(a)
         IF (.NOT. reals_eq(a(i), b(i))) RETURN
      END DO
      vec_equal = .TRUE.
   END FUNCTION vec_equal

   LOGICAL FUNCTION mat_equal(a, b)
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(IN) :: a(:, :), b(:, :)
      INTEGER :: i, j

      mat_equal = .FALSE.
      IF (SIZE(a, 1) /= SIZE(b, 1) .OR. SIZE(a, 2) /= SIZE(b, 2)) RETURN
      DO j = 1, SIZE(a, 2)
         DO i = 1, SIZE(a, 1)
            IF (.NOT. reals_eq(a(i, j), b(i, j))) RETURN
         END DO
      END DO
      mat_equal = .TRUE.
   END FUNCTION mat_equal

   LOGICAL FUNCTION alloc_vec_equal(a, b)
      IMPLICIT NONE
      REAL(KIND(1D0)), ALLOCATABLE, INTENT(IN) :: a(:), b(:)

      alloc_vec_equal = .FALSE.
      IF (ALLOCATED(a) .NEQV. ALLOCATED(b)) RETURN
      IF (.NOT. ALLOCATED(a)) THEN
         alloc_vec_equal = .TRUE.
         RETURN
      END IF
      alloc_vec_equal = vec_equal(a, b)
   END FUNCTION alloc_vec_equal

   LOGICAL FUNCTION alloc_mat_equal(a, b)
      IMPLICIT NONE
      REAL(KIND(1D0)), ALLOCATABLE, INTENT(IN) :: a(:, :), b(:, :)

      alloc_mat_equal = .FALSE.
      IF (ALLOCATED(a) .NEQV. ALLOCATED(b)) RETURN
      IF (.NOT. ALLOCATED(a)) THEN
         alloc_mat_equal = .TRUE.
         RETURN
      END IF
      alloc_mat_equal = mat_equal(a, b)
   END FUNCTION alloc_mat_equal

   LOGICAL FUNCTION int_vec_equal(a, b)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: a(:), b(:)

      int_vec_equal = .FALSE.
      IF (SIZE(a) /= SIZE(b)) RETURN
      int_vec_equal = ALL(a == b)
   END FUNCTION int_vec_equal

   LOGICAL FUNCTION timers_equal(a, b)
      IMPLICIT NONE
      TYPE(SUEWS_TIMER), INTENT(IN) :: a, b

      timers_equal = .FALSE.
      IF (a%id /= b%id .OR. a%imin /= b%imin .OR. a%isec /= b%isec .OR. &
          a%it /= b%it .OR. a%iy /= b%iy .OR. a%tstep /= b%tstep .OR. &
          a%tstep_prev /= b%tstep_prev .OR. a%dt_since_start /= b%dt_since_start .OR. &
          a%dt_since_start_prev /= b%dt_since_start_prev .OR. a%nsh /= b%nsh .OR. &
          a%DLS /= b%DLS .OR. a%new_day /= b%new_day .OR. &
          a%iy_st /= b%iy_st .OR. a%id_st /= b%id_st .OR. &
          a%it_st /= b%it_st .OR. a%imin_st /= b%imin_st) RETURN
      IF (.NOT. int_vec_equal(a%dayofWeek_id, b%dayofWeek_id) .OR. &
          .NOT. int_vec_equal(a%dayofWeek_id_st, b%dayofWeek_id_st)) RETURN
      IF (.NOT. reals_eq(a%nsh_real, b%nsh_real) .OR. &
          .NOT. reals_eq(a%tstep_real, b%tstep_real) .OR. &
          .NOT. reals_eq(a%dectime, b%dectime) .OR. &
          .NOT. reals_eq(a%dectime_st, b%dectime_st) .OR. &
          .NOT. reals_eq(a%tz_solar, b%tz_solar)) RETURN
      timers_equal = .TRUE.
   END FUNCTION timers_equal

   LOGICAL FUNCTION error_entries_equal(a, b)
      USE module_ctrl_type, ONLY: error_entry
      IMPLICIT NONE
      TYPE(error_entry), INTENT(IN) :: a, b

      error_entries_equal = &
         timers_equal(a%timer, b%timer) .AND. &
         (a%message == b%message) .AND. &
         (a%location == b%location) .AND. &
         (a%is_fatal .EQV. b%is_fatal)
   END FUNCTION error_entries_equal

   LOGICAL FUNCTION error_states_equal(a, b)
      IMPLICIT NONE
      TYPE(error_state), INTENT(IN) :: a, b
      INTEGER :: i

      error_states_equal = .FALSE.
      IF ((a%flag .NEQV. b%flag) .OR. (a%has_fatal .NEQV. b%has_fatal) .OR. &
          a%code /= b%code .OR. a%count /= b%count .OR. &
          a%n_total /= b%n_total .OR. a%n_stamped /= b%n_stamped .OR. &
          a%message /= b%message) RETURN
      IF (ALLOCATED(a%log) .NEQV. ALLOCATED(b%log)) RETURN
      IF (ALLOCATED(a%log)) THEN
         IF (SIZE(a%log) < a%count .OR. SIZE(b%log) < b%count) RETURN
         DO i = 1, a%count
            IF (.NOT. error_entries_equal(a%log(i), b%log(i))) RETURN
         END DO
      END IF
      error_states_equal = .TRUE.
   END FUNCTION error_states_equal

   LOGICAL FUNCTION flag_states_equal(a, b)
      IMPLICIT NONE
      TYPE(flag_STATE), INTENT(IN) :: a, b

      flag_states_equal = &
         (a%flag_converge .EQV. b%flag_converge) .AND. &
         (a%i_iter == b%i_iter) .AND. &
         (a%stebbs_bldg_init == b%stebbs_bldg_init) .AND. &
         (a%snow_warning_shown .EQV. b%snow_warning_shown) .AND. &
         (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION flag_states_equal

   LOGICAL FUNCTION anthroemis_states_equal(a, b)
      IMPLICIT NONE
      TYPE(anthroEmis_STATE), INTENT(IN) :: a, b

      anthroemis_states_equal = .FALSE.
      IF (.NOT. vec_equal(a%HDD_id, b%HDD_id)) RETURN
      IF (.NOT. reals_eq(a%Fc, b%Fc) .OR. &
          .NOT. reals_eq(a%Fc_anthro, b%Fc_anthro) .OR. &
          .NOT. reals_eq(a%Fc_biogen, b%Fc_biogen) .OR. &
          .NOT. reals_eq(a%Fc_build, b%Fc_build) .OR. &
          .NOT. reals_eq(a%Fc_metab, b%Fc_metab) .OR. &
          .NOT. reals_eq(a%Fc_photo, b%Fc_photo) .OR. &
          .NOT. reals_eq(a%Fc_point, b%Fc_point) .OR. &
          .NOT. reals_eq(a%Fc_respi, b%Fc_respi) .OR. &
          .NOT. reals_eq(a%Fc_traff, b%Fc_traff)) RETURN
      anthroemis_states_equal = (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION anthroemis_states_equal

   LOGICAL FUNCTION ohm_states_equal(a, b)
      IMPLICIT NONE
      TYPE(OHM_STATE), INTENT(IN) :: a, b

      ohm_states_equal = .FALSE.
      IF (.NOT. reals_eq(a%qn_av, b%qn_av) .OR. &
          .NOT. reals_eq(a%dqndt, b%dqndt) .OR. &
          .NOT. vec_equal(a%qn_surfs, b%qn_surfs) .OR. &
          .NOT. vec_equal(a%dqndt_surf, b%dqndt_surf) .OR. &
          .NOT. reals_eq(a%qn_s_av, b%qn_s_av) .OR. &
          .NOT. reals_eq(a%dqnsdt, b%dqnsdt) .OR. &
          .NOT. reals_eq(a%a1, b%a1) .OR. &
          .NOT. reals_eq(a%a2, b%a2) .OR. &
          .NOT. reals_eq(a%a3, b%a3) .OR. &
          .NOT. reals_eq(a%t2_prev, b%t2_prev) .OR. &
          .NOT. reals_eq(a%ws_rav, b%ws_rav) .OR. &
          .NOT. vec_equal(a%qn_rav, b%qn_rav)) RETURN
      IF (.NOT. reals_eq(a%a1_bldg, b%a1_bldg) .OR. &
          .NOT. reals_eq(a%a2_bldg, b%a2_bldg) .OR. &
          .NOT. reals_eq(a%a3_bldg, b%a3_bldg) .OR. &
          .NOT. reals_eq(a%a1_paved, b%a1_paved) .OR. &
          .NOT. reals_eq(a%a2_paved, b%a2_paved) .OR. &
          .NOT. reals_eq(a%a3_paved, b%a3_paved) .OR. &
          .NOT. reals_eq(a%a1_evetr, b%a1_evetr) .OR. &
          .NOT. reals_eq(a%a2_evetr, b%a2_evetr) .OR. &
          .NOT. reals_eq(a%a3_evetr, b%a3_evetr) .OR. &
          .NOT. reals_eq(a%a1_dectr, b%a1_dectr) .OR. &
          .NOT. reals_eq(a%a2_dectr, b%a2_dectr) .OR. &
          .NOT. reals_eq(a%a3_dectr, b%a3_dectr) .OR. &
          .NOT. reals_eq(a%a1_grass, b%a1_grass) .OR. &
          .NOT. reals_eq(a%a2_grass, b%a2_grass) .OR. &
          .NOT. reals_eq(a%a3_grass, b%a3_grass) .OR. &
          .NOT. reals_eq(a%a1_bsoil, b%a1_bsoil) .OR. &
          .NOT. reals_eq(a%a2_bsoil, b%a2_bsoil) .OR. &
          .NOT. reals_eq(a%a3_bsoil, b%a3_bsoil) .OR. &
          .NOT. reals_eq(a%a1_water, b%a1_water) .OR. &
          .NOT. reals_eq(a%a2_water, b%a2_water) .OR. &
          .NOT. reals_eq(a%a3_water, b%a3_water)) RETURN
      IF (a%anohm_working_day /= b%anohm_working_day .OR. &
          a%anohm_working_count /= b%anohm_working_count .OR. &
          a%anohm_coeff_day /= b%anohm_coeff_day .OR. &
          a%anohm_coeff_count /= b%anohm_coeff_count .OR. &
          (a%anohm_coeff_ready .NEQV. b%anohm_coeff_ready) .OR. &
          (a%iter_safe .NEQV. b%iter_safe)) RETURN
      IF (.NOT. vec_equal(a%anohm_working_tHr, b%anohm_working_tHr) .OR. &
          .NOT. vec_equal(a%anohm_working_sd, b%anohm_working_sd) .OR. &
          .NOT. vec_equal(a%anohm_working_ta, b%anohm_working_ta) .OR. &
          .NOT. vec_equal(a%anohm_working_rh, b%anohm_working_rh) .OR. &
          .NOT. vec_equal(a%anohm_working_pres, b%anohm_working_pres) .OR. &
          .NOT. vec_equal(a%anohm_working_ws, b%anohm_working_ws) .OR. &
          .NOT. vec_equal(a%anohm_working_ah, b%anohm_working_ah) .OR. &
          .NOT. vec_equal(a%anohm_coeff_tHr, b%anohm_coeff_tHr) .OR. &
          .NOT. vec_equal(a%anohm_coeff_sd, b%anohm_coeff_sd) .OR. &
          .NOT. vec_equal(a%anohm_coeff_ta, b%anohm_coeff_ta) .OR. &
          .NOT. vec_equal(a%anohm_coeff_rh, b%anohm_coeff_rh) .OR. &
          .NOT. vec_equal(a%anohm_coeff_pres, b%anohm_coeff_pres) .OR. &
          .NOT. vec_equal(a%anohm_coeff_ws, b%anohm_coeff_ws) .OR. &
          .NOT. vec_equal(a%anohm_coeff_ah, b%anohm_coeff_ah) .OR. &
          .NOT. vec_equal(a%anohm_a1_surf, b%anohm_a1_surf) .OR. &
          .NOT. vec_equal(a%anohm_a2_surf, b%anohm_a2_surf) .OR. &
          .NOT. vec_equal(a%anohm_a3_surf, b%anohm_a3_surf)) RETURN
      ohm_states_equal = .TRUE.
   END FUNCTION ohm_states_equal

   LOGICAL FUNCTION solar_states_equal(a, b)
      IMPLICIT NONE
      TYPE(solar_State), INTENT(IN) :: a, b

      solar_states_equal = &
         reals_eq(a%azimuth_deg, b%azimuth_deg) .AND. &
         reals_eq(a%zenith_deg, b%zenith_deg) .AND. &
         (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION solar_states_equal

   LOGICAL FUNCTION atm_states_equal(a, b)
      IMPLICIT NONE
      TYPE(atm_state), INTENT(IN) :: a, b

      atm_states_equal = .FALSE.
      IF (.NOT. reals_eq(a%f_cloud, b%f_cloud) .OR. &
          .NOT. reals_eq(a%av_cp, b%av_cp) .OR. &
          .NOT. reals_eq(a%dens_dry, b%dens_dry) .OR. &
          .NOT. reals_eq(a%av_density, b%av_density) .OR. &
          .NOT. reals_eq(a%dq, b%dq) .OR. &
          .NOT. reals_eq(a%Ea_hPa, b%Ea_hPa) .OR. &
          .NOT. reals_eq(a%Es_hPa, b%Es_hPa) .OR. &
          .NOT. reals_eq(a%lv_J_kg, b%lv_J_kg) .OR. &
          .NOT. reals_eq(a%lv_s_j_kg, b%lv_s_j_kg) .OR. &
          .NOT. reals_eq(a%tlv, b%tlv) .OR. &
          .NOT. reals_eq(a%psyc_hPa, b%psyc_hPa) .OR. &
          .NOT. reals_eq(a%psyc_ice_h_pa, b%psyc_ice_h_pa) .OR. &
          .NOT. reals_eq(a%s_Pa, b%s_Pa) .OR. &
          .NOT. reals_eq(a%s_hpa, b%s_hpa) .OR. &
          .NOT. reals_eq(a%s_ice_hpa, b%s_ice_hpa) .OR. &
          .NOT. reals_eq(a%vpd_hPa, b%vpd_hPa) .OR. &
          .NOT. reals_eq(a%vpd_pa, b%vpd_pa)) RETURN
      IF (.NOT. reals_eq(a%U10_ms, b%U10_ms) .OR. &
          .NOT. reals_eq(a%U_hbh, b%U_hbh) .OR. &
          .NOT. reals_eq(a%T2_C, b%T2_C) .OR. &
          .NOT. reals_eq(a%T_half_bldg_C, b%T_half_bldg_C) .OR. &
          .NOT. reals_eq(a%q2_gkg, b%q2_gkg) .OR. &
          .NOT. reals_eq(a%RH2, b%RH2) .OR. &
          .NOT. reals_eq(a%L_mod, b%L_mod) .OR. &
          .NOT. reals_eq(a%z_l, b%z_l) .OR. &
          .NOT. reals_eq(a%RA_h, b%RA_h) .OR. &
          .NOT. reals_eq(a%RS, b%RS) .OR. &
          .NOT. reals_eq(a%u_star, b%u_star) .OR. &
          .NOT. reals_eq(a%t_star, b%t_star) .OR. &
          .NOT. reals_eq(a%RB, b%RB) .OR. &
          .NOT. reals_eq(a%Tair_av, b%Tair_av) .OR. &
          .NOT. vec_equal(a%rss_surf, b%rss_surf)) RETURN
      atm_states_equal = (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION atm_states_equal

   LOGICAL FUNCTION phen_states_equal(a, b)
      IMPLICIT NONE
      TYPE(PHENOLOGY_STATE), INTENT(IN) :: a, b

      phen_states_equal = .FALSE.
      IF (.NOT. vec_equal(a%alb, b%alb) .OR. &
          .NOT. vec_equal(a%lai_id, b%lai_id) .OR. &
          .NOT. vec_equal(a%GDD_id, b%GDD_id) .OR. &
          .NOT. vec_equal(a%SDD_id, b%SDD_id) .OR. &
          .NOT. mat_equal(a%storage_drain_params, b%storage_drain_params)) RETURN
      IF (.NOT. reals_eq(a%veg_phen_lumps, b%veg_phen_lumps) .OR. &
          .NOT. reals_eq(a%porosity_id, b%porosity_id) .OR. &
          .NOT. reals_eq(a%decidcap_id, b%decidcap_id) .OR. &
          .NOT. reals_eq(a%albDecTr_id, b%albDecTr_id) .OR. &
          .NOT. reals_eq(a%albEveTr_id, b%albEveTr_id) .OR. &
          .NOT. reals_eq(a%albGrass_id, b%albGrass_id) .OR. &
          .NOT. reals_eq(a%Tmin_id, b%Tmin_id) .OR. &
          .NOT. reals_eq(a%Tmax_id, b%Tmax_id) .OR. &
          .NOT. reals_eq(a%lenDay_id, b%lenDay_id) .OR. &
          .NOT. reals_eq(a%temp_veg, b%temp_veg) .OR. &
          .NOT. reals_eq(a%g_func, b%g_func) .OR. &
          .NOT. reals_eq(a%gsc, b%gsc) .OR. &
          .NOT. reals_eq(a%g_kdown, b%g_kdown) .OR. &
          .NOT. reals_eq(a%g_dq, b%g_dq) .OR. &
          .NOT. reals_eq(a%g_ta, b%g_ta) .OR. &
          .NOT. reals_eq(a%g_smd, b%g_smd) .OR. &
          .NOT. reals_eq(a%g_lai, b%g_lai)) RETURN
      phen_states_equal = (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION phen_states_equal

   LOGICAL FUNCTION snow_states_equal(a, b)
      IMPLICIT NONE
      TYPE(SNOW_STATE), INTENT(IN) :: a, b

      snow_states_equal = .FALSE.
      IF (.NOT. reals_eq(a%snowfall_cum, b%snowfall_cum) .OR. &
          .NOT. reals_eq(a%snow_albedo, b%snow_albedo) .OR. &
          .NOT. reals_eq(a%chSnow_per_interval, b%chSnow_per_interval) .OR. &
          .NOT. reals_eq(a%mwh, b%mwh) .OR. &
          .NOT. reals_eq(a%melt_water_store, b%melt_water_store) .OR. &
          .NOT. reals_eq(a%qn_snow, b%qn_snow) .OR. &
          .NOT. reals_eq(a%qm, b%qm) .OR. &
          .NOT. reals_eq(a%qm_freeze, b%qm_freeze) .OR. &
          .NOT. reals_eq(a%qm_rain, b%qm_rain) .OR. &
          .NOT. reals_eq(a%swe, b%swe) .OR. &
          .NOT. reals_eq(a%z0v_snow, b%z0v_snow) .OR. &
          .NOT. reals_eq(a%ra_snow, b%ra_snow)) RETURN
      IF (.NOT. vec_equal(a%snow_removal, b%snow_removal) .OR. &
          .NOT. vec_equal(a%ice_frac, b%ice_frac) .OR. &
          .NOT. vec_equal(a%snow_density, b%snow_density) .OR. &
          .NOT. vec_equal(a%snow_fraction, b%snow_fraction) .OR. &
          .NOT. vec_equal(a%snow_pack, b%snow_pack) .OR. &
          .NOT. vec_equal(a%snow_water, b%snow_water) .OR. &
          .NOT. vec_equal(a%kup_ind_snow, b%kup_ind_snow) .OR. &
          .NOT. vec_equal(a%qn_ind_snow, b%qn_ind_snow) .OR. &
          .NOT. vec_equal(a%delta_qi, b%delta_qi) .OR. &
          .NOT. vec_equal(a%Tsurf_ind_snow, b%Tsurf_ind_snow)) RETURN
      snow_states_equal = (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION snow_states_equal

   LOGICAL FUNCTION hydro_states_equal(a, b)
      IMPLICIT NONE
      TYPE(HYDRO_STATE), INTENT(IN) :: a, b

      hydro_states_equal = .FALSE.
      IF (.NOT. vec_equal(a%soil_store_surf, b%soil_store_surf) .OR. &
          .NOT. vec_equal(a%state_surf, b%state_surf) .OR. &
          .NOT. vec_equal(a%WUDay_id, b%WUDay_id) .OR. &
          .NOT. alloc_vec_equal(a%soil_store_roof, b%soil_store_roof) .OR. &
          .NOT. alloc_vec_equal(a%state_roof, b%state_roof) .OR. &
          .NOT. alloc_vec_equal(a%soil_store_wall, b%soil_store_wall) .OR. &
          .NOT. alloc_vec_equal(a%state_wall, b%state_wall) .OR. &
          .NOT. alloc_vec_equal(a%ev_roof, b%ev_roof) .OR. &
          .NOT. alloc_vec_equal(a%ev_wall, b%ev_wall)) RETURN
      IF (.NOT. vec_equal(a%ev0_surf, b%ev0_surf) .OR. &
          .NOT. vec_equal(a%ev_surf, b%ev_surf) .OR. &
          .NOT. vec_equal(a%wu_surf, b%wu_surf) .OR. &
          .NOT. vec_equal(a%runoff_soil, b%runoff_soil) .OR. &
          .NOT. vec_equal(a%smd_surf, b%smd_surf) .OR. &
          .NOT. vec_equal(a%drain_surf, b%drain_surf) .OR. &
          .NOT. vec_equal(a%add_water, b%add_water) .OR. &
          .NOT. vec_equal(a%frac_water2runoff, b%frac_water2runoff)) RETURN
      IF (.NOT. reals_eq(a%drain_per_tstep, b%drain_per_tstep) .OR. &
          .NOT. reals_eq(a%ev_per_tstep, b%ev_per_tstep) .OR. &
          .NOT. reals_eq(a%wu_ext, b%wu_ext) .OR. &
          .NOT. reals_eq(a%wu_int, b%wu_int) .OR. &
          .NOT. reals_eq(a%runoff_ag_veg, b%runoff_ag_veg) .OR. &
          .NOT. reals_eq(a%runoff_ag_impervious, b%runoff_ag_impervious) .OR. &
          .NOT. reals_eq(a%runoff_per_tstep, b%runoff_per_tstep) .OR. &
          .NOT. reals_eq(a%runoff_pipes, b%runoff_pipes) .OR. &
          .NOT. reals_eq(a%runoff_soil_per_tstep, b%runoff_soil_per_tstep) .OR. &
          .NOT. reals_eq(a%runoff_waterbody, b%runoff_waterbody) .OR. &
          .NOT. reals_eq(a%smd, b%smd) .OR. &
          .NOT. reals_eq(a%soil_state, b%soil_state)) RETURN
      IF (.NOT. reals_eq(a%state_per_tstep, b%state_per_tstep) .OR. &
          .NOT. reals_eq(a%surf_chang_per_tstep, b%surf_chang_per_tstep) .OR. &
          .NOT. reals_eq(a%tot_chang_per_tstep, b%tot_chang_per_tstep) .OR. &
          .NOT. reals_eq(a%runoff_per_interval, b%runoff_per_interval) .OR. &
          .NOT. reals_eq(a%nw_state_per_tstep, b%nw_state_per_tstep) .OR. &
          .NOT. reals_eq(a%soil_moist_cap, b%soil_moist_cap) .OR. &
          .NOT. reals_eq(a%vsmd, b%vsmd) .OR. &
          .NOT. reals_eq(a%additional_water, b%additional_water) .OR. &
          .NOT. reals_eq(a%add_impervious, b%add_impervious) .OR. &
          .NOT. reals_eq(a%add_pipes, b%add_pipes) .OR. &
          .NOT. reals_eq(a%add_veg, b%add_veg) .OR. &
          .NOT. reals_eq(a%add_water_body, b%add_water_body)) RETURN
      hydro_states_equal = (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION hydro_states_equal

   LOGICAL FUNCTION heat_states_equal(a, b)
      IMPLICIT NONE
      TYPE(HEAT_STATE), INTENT(IN) :: a, b

      heat_states_equal = .FALSE.
      IF (.NOT. alloc_mat_equal(a%temp_roof, b%temp_roof) .OR. &
          .NOT. alloc_mat_equal(a%temp_wall, b%temp_wall) .OR. &
          .NOT. alloc_mat_equal(a%temp_surf, b%temp_surf) .OR. &
          .NOT. alloc_mat_equal(a%temp_surf_dyohm, b%temp_surf_dyohm) .OR. &
          .NOT. alloc_mat_equal(a%temp_surf_ehc_fast, b%temp_surf_ehc_fast) .OR. &
          .NOT. alloc_mat_equal(a%temp_surf_ehc_slow, b%temp_surf_ehc_slow)) RETURN
      IF (.NOT. alloc_vec_equal(a%tsfc_roof, b%tsfc_roof) .OR. &
          .NOT. alloc_vec_equal(a%tsfc_wall, b%tsfc_wall) .OR. &
          .NOT. alloc_vec_equal(a%tsfc_surf, b%tsfc_surf) .OR. &
          .NOT. alloc_vec_equal(a%tsfc_surf_dyohm, b%tsfc_surf_dyohm) .OR. &
          .NOT. alloc_vec_equal(a%tsfc_roof_stepstart, b%tsfc_roof_stepstart) .OR. &
          .NOT. alloc_vec_equal(a%tsfc_wall_stepstart, b%tsfc_wall_stepstart) .OR. &
          .NOT. alloc_vec_equal(a%tsfc_surf_stepstart, b%tsfc_surf_stepstart)) RETURN
      IF (.NOT. alloc_vec_equal(a%QS_roof, b%QS_roof) .OR. &
          .NOT. alloc_vec_equal(a%QN_roof, b%QN_roof) .OR. &
          .NOT. alloc_vec_equal(a%qe_roof, b%qe_roof) .OR. &
          .NOT. alloc_vec_equal(a%qh_roof, b%qh_roof) .OR. &
          .NOT. alloc_vec_equal(a%qh_resist_roof, b%qh_resist_roof) .OR. &
          .NOT. alloc_vec_equal(a%QS_wall, b%QS_wall) .OR. &
          .NOT. alloc_vec_equal(a%QN_wall, b%QN_wall) .OR. &
          .NOT. alloc_vec_equal(a%qe_wall, b%qe_wall) .OR. &
          .NOT. alloc_vec_equal(a%qh_wall, b%qh_wall) .OR. &
          .NOT. alloc_vec_equal(a%qh_resist_wall, b%qh_resist_wall)) RETURN
      IF (.NOT. vec_equal(a%qs_surf, b%qs_surf) .OR. &
          .NOT. vec_equal(a%QN_surf, b%QN_surf) .OR. &
          .NOT. vec_equal(a%qe0_surf, b%qe0_surf) .OR. &
          .NOT. vec_equal(a%qe_surf, b%qe_surf) .OR. &
          .NOT. vec_equal(a%qh_surf, b%qh_surf) .OR. &
          .NOT. vec_equal(a%qh_resist_surf, b%qh_resist_surf) .OR. &
          .NOT. vec_equal(a%tsurf_ind, b%tsurf_ind)) RETURN
      IF (.NOT. reals_eq(a%QH_LUMPS, b%QH_LUMPS) .OR. &
          .NOT. reals_eq(a%QE_LUMPS, b%QE_LUMPS) .OR. &
          .NOT. reals_eq(a%k_clear, b%k_clear) .OR. &
          .NOT. reals_eq(a%k_up, b%k_up) .OR. &
          .NOT. reals_eq(a%l_down, b%l_down) .OR. &
          .NOT. reals_eq(a%l_up, b%l_up) .OR. &
          .NOT. reals_eq(a%qe, b%qe) .OR. &
          .NOT. reals_eq(a%qf, b%qf) .OR. &
          .NOT. reals_eq(a%QF_SAHP, b%QF_SAHP) .OR. &
          .NOT. reals_eq(a%qh, b%qh) .OR. &
          .NOT. reals_eq(a%qh_residual, b%qh_residual) .OR. &
          .NOT. reals_eq(a%qh_resist, b%qh_resist) .OR. &
          .NOT. reals_eq(a%ehc_iter_dif_tsfc, b%ehc_iter_dif_tsfc) .OR. &
          .NOT. reals_eq(a%ehc_iter_dif_qh, b%ehc_iter_dif_qh) .OR. &
          .NOT. reals_eq(a%ehc_iter_score, b%ehc_iter_score) .OR. &
          .NOT. reals_eq(a%qn, b%qn) .OR. &
          .NOT. reals_eq(a%qn_snowfree, b%qn_snowfree) .OR. &
          .NOT. reals_eq(a%qs, b%qs) .OR. &
          .NOT. reals_eq(a%TSfc_C, b%TSfc_C) .OR. &
          .NOT. reals_eq(a%tsurf, b%tsurf) .OR. &
          .NOT. reals_eq(a%QH_Init, b%QH_Init)) RETURN
      IF (.NOT. vec_equal(a%roof_in_sw_spc, b%roof_in_sw_spc) .OR. &
          .NOT. vec_equal(a%roof_in_lw_spc, b%roof_in_lw_spc) .OR. &
          .NOT. vec_equal(a%wall_in_sw_spc, b%wall_in_sw_spc) .OR. &
          .NOT. vec_equal(a%wall_in_lw_spc, b%wall_in_lw_spc)) RETURN
      heat_states_equal = (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION heat_states_equal

   LOGICAL FUNCTION roughness_states_equal(a, b)
      IMPLICIT NONE
      TYPE(ROUGHNESS_STATE), INTENT(IN) :: a, b

      roughness_states_equal = &
         reals_eq(a%fai_bldg_use, b%fai_bldg_use) .AND. &
         reals_eq(a%fai_evetree_use, b%fai_evetree_use) .AND. &
         reals_eq(a%fai_dectree_use, b%fai_dectree_use) .AND. &
         reals_eq(a%FAI, b%FAI) .AND. &
         reals_eq(a%PAI, b%PAI) .AND. &
         reals_eq(a%Zh, b%Zh) .AND. &
         reals_eq(a%z0m, b%z0m) .AND. &
         reals_eq(a%z0v, b%z0v) .AND. &
         reals_eq(a%zdm, b%zdm) .AND. &
         reals_eq(a%ZZD, b%ZZD) .AND. &
         (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION roughness_states_equal

   LOGICAL FUNCTION stebbs_bldg_equal(a, b)
      IMPLICIT NONE
      TYPE(STEBBS_BLDG), INTENT(IN) :: a, b

      stebbs_bldg_equal = .FALSE.
      IF (a%building_type /= b%building_type .OR. &
          a%building_name /= b%building_name .OR. &
          a%fnml_lbm /= b%fnml_lbm .OR. &
          a%case_id /= b%case_id .OR. &
          a%id_lbm /= b%id_lbm .OR. &
          a%appliance_totalnumber /= b%appliance_totalnumber .OR. &
          a%internal_shading /= b%internal_shading .OR. &
          a%destination_waste_heat /= b%destination_waste_heat .OR. &
          (a%wall_surface_active .NEQV. b%wall_surface_active) .OR. &
          (a%window_surface_active .NEQV. b%window_surface_active) .OR. &
          (a%iter_safe .NEQV. b%iter_safe)) RETURN
      IF (.NOT. reals_eq(a%QHload_heating_tstepFA, b%QHload_heating_tstepFA) .OR. &
          .NOT. reals_eq(a%QHload_cooling_tstepFA, b%QHload_cooling_tstepFA) .OR. &
          .NOT. reals_eq(a%QH_metabolism, b%QH_metabolism) .OR. &
          .NOT. reals_eq(a%QE_metabolism, b%QE_metabolism) .OR. &
          .NOT. reals_eq(a%QHload_dhw_tstepFA, b%QHload_dhw_tstepFA) .OR. &
          .NOT. reals_eq(a%Qloss_drain_tstepFA, b%Qloss_drain_tstepFA) .OR. &
          .NOT. reals_eq(a%ratio_window_wall, b%ratio_window_wall) .OR. &
          .NOT. reals_eq(a%a_footprint, b%a_footprint) .OR. &
          .NOT. reals_eq(a%height_building, b%height_building) .OR. &
          .NOT. reals_eq(a%wall_external_area, b%wall_external_area) .OR. &
          .NOT. reals_eq(a%internal_volume_ratio, b%internal_volume_ratio) .OR. &
          .NOT. reals_eq(a%thickness_wall, b%thickness_wall) .OR. &
          .NOT. reals_eq(a%thickness_wallext, b%thickness_wallext) .OR. &
          .NOT. reals_eq(a%thickness_roof, b%thickness_roof) .OR. &
          .NOT. reals_eq(a%thickness_roofext, b%thickness_roofext) .OR. &
          .NOT. reals_eq(a%thickness_groundfloor, b%thickness_groundfloor) .OR. &
          .NOT. reals_eq(a%depth_ground, b%depth_ground) .OR. &
          .NOT. reals_eq(a%thickness_window, b%thickness_window)) RETURN
      IF (.NOT. reals_eq(a%conv_coeff_intwall, b%conv_coeff_intwall) .OR. &
          .NOT. reals_eq(a%conv_coeff_introof, b%conv_coeff_introof) .OR. &
          .NOT. reals_eq(a%conv_coeff_indoormass, b%conv_coeff_indoormass) .OR. &
          .NOT. reals_eq(a%conv_coeff_intgroundfloor, b%conv_coeff_intgroundfloor) .OR. &
          .NOT. reals_eq(a%conv_coeff_intwindow, b%conv_coeff_intwindow) .OR. &
          .NOT. reals_eq(a%conv_coeff_extwall, b%conv_coeff_extwall) .OR. &
          .NOT. reals_eq(a%conv_coeff_extroof, b%conv_coeff_extroof) .OR. &
          .NOT. reals_eq(a%conv_coeff_extwindow, b%conv_coeff_extwindow) .OR. &
          .NOT. reals_eq(a%conductivity_wall, b%conductivity_wall) .OR. &
          .NOT. reals_eq(a%conductivity_wallext, b%conductivity_wallext) .OR. &
          .NOT. reals_eq(a%conductivity_roof, b%conductivity_roof) .OR. &
          .NOT. reals_eq(a%conductivity_roofext, b%conductivity_roofext) .OR. &
          .NOT. reals_eq(a%conductivity_groundfloor, b%conductivity_groundfloor) .OR. &
          .NOT. reals_eq(a%conductivity_window, b%conductivity_window) .OR. &
          .NOT. reals_eq(a%conductivity_ground, b%conductivity_ground) .OR. &
          .NOT. reals_eq(a%density_wall, b%density_wall) .OR. &
          .NOT. reals_eq(a%density_wallext, b%density_wallext) .OR. &
          .NOT. reals_eq(a%density_roof, b%density_roof) .OR. &
          .NOT. reals_eq(a%density_roofext, b%density_roofext)) RETURN
      IF (.NOT. reals_eq(a%weighting_factor_heatcapacity_wall, b%weighting_factor_heatcapacity_wall) .OR. &
          .NOT. reals_eq(a%weighting_factor_heatcapacity_roof, b%weighting_factor_heatcapacity_roof) .OR. &
          .NOT. reals_eq(a%density_groundfloor, b%density_groundfloor) .OR. &
          .NOT. reals_eq(a%density_window, b%density_window) .OR. &
          .NOT. reals_eq(a%density_indoormass, b%density_indoormass) .OR. &
          .NOT. reals_eq(a%density_air_ind, b%density_air_ind) .OR. &
          .NOT. reals_eq(a%cp_wall, b%cp_wall) .OR. &
          .NOT. reals_eq(a%cp_wallext, b%cp_wallext) .OR. &
          .NOT. reals_eq(a%cp_roof, b%cp_roof) .OR. &
          .NOT. reals_eq(a%cp_roofext, b%cp_roofext) .OR. &
          .NOT. reals_eq(a%cp_groundfloor, b%cp_groundfloor) .OR. &
          .NOT. reals_eq(a%cp_window, b%cp_window) .OR. &
          .NOT. reals_eq(a%cp_indoormass, b%cp_indoormass) .OR. &
          .NOT. reals_eq(a%cp_air_ind, b%cp_air_ind) .OR. &
          .NOT. reals_eq(a%emissivity_extwall, b%emissivity_extwall) .OR. &
          .NOT. reals_eq(a%emissivity_extroof, b%emissivity_extroof) .OR. &
          .NOT. reals_eq(a%emissivity_intwall, b%emissivity_intwall) .OR. &
          .NOT. reals_eq(a%emissivity_introof, b%emissivity_introof) .OR. &
          .NOT. reals_eq(a%emissivity_indoormass, b%emissivity_indoormass) .OR. &
          .NOT. reals_eq(a%emissivity_extwindow, b%emissivity_extwindow) .OR. &
          .NOT. reals_eq(a%emissivity_intwindow, b%emissivity_intwindow)) RETURN
      IF (.NOT. reals_eq(a%window_transmissivity, b%window_transmissivity) .OR. &
          .NOT. reals_eq(a%window_absorptivity, b%window_absorptivity) .OR. &
          .NOT. reals_eq(a%window_reflectivity, b%window_reflectivity) .OR. &
          .NOT. reals_eq(a%wall_transmissivity_state, b%wall_transmissivity_state) .OR. &
          .NOT. reals_eq(a%wall_absorptivity, b%wall_absorptivity) .OR. &
          .NOT. reals_eq(a%wall_reflectivity, b%wall_reflectivity) .OR. &
          .NOT. reals_eq(a%roof_transmissivity_state, b%roof_transmissivity_state) .OR. &
          .NOT. reals_eq(a%roof_absorptivity, b%roof_absorptivity) .OR. &
          .NOT. reals_eq(a%roof_reflectivity, b%roof_reflectivity) .OR. &
          .NOT. reals_eq(a%occupants_state, b%occupants_state) .OR. &
          .NOT. reals_eq(a%metabolic_rate, b%metabolic_rate) .OR. &
          .NOT. reals_eq(a%metabolism_threshold, b%metabolism_threshold) .OR. &
          .NOT. reals_eq(a%ratio_metabolic_latent_sensible, b%ratio_metabolic_latent_sensible) .OR. &
          .NOT. reals_eq(a%reduction_factor_shading, b%reduction_factor_shading) .OR. &
          .NOT. reals_eq(a%temperature_threshold_shading, b%temperature_threshold_shading) .OR. &
          .NOT. reals_eq(a%radiation_threshold_shading, b%radiation_threshold_shading) .OR. &
          .NOT. reals_eq(a%appliance_power_rating, b%appliance_power_rating) .OR. &
          .NOT. reals_eq(a%lighting_power_rating, b%lighting_power_rating) .OR. &
          .NOT. reals_eq(a%frac_hotwater, b%frac_hotwater)) RETURN
      IF (.NOT. reals_eq(a%maxheatingpower_air, b%maxheatingpower_air) .OR. &
          .NOT. reals_eq(a%heating_efficiency_air, b%heating_efficiency_air) .OR. &
          .NOT. reals_eq(a%fraction_convective_heating, b%fraction_convective_heating) .OR. &
          .NOT. reals_eq(a%maxcoolingpower_air, b%maxcoolingpower_air) .OR. &
          .NOT. reals_eq(a%coeff_performance_cooling, b%coeff_performance_cooling) .OR. &
          .NOT. reals_eq(a%Vair_ind, b%Vair_ind) .OR. &
          .NOT. reals_eq(a%ventilation_rate, b%ventilation_rate) .OR. &
          .NOT. reals_eq(a%a_wall, b%a_wall) .OR. &
          .NOT. reals_eq(a%a_roof, b%a_roof) .OR. &
          .NOT. reals_eq(a%v_wall, b%v_wall) .OR. &
          .NOT. reals_eq(a%v_roof, b%v_roof) .OR. &
          .NOT. reals_eq(a%v_ground_floor, b%v_ground_floor) .OR. &
          .NOT. reals_eq(a%a_window, b%a_window) .OR. &
          .NOT. reals_eq(a%v_window, b%v_window) .OR. &
          .NOT. reals_eq(a%v_indoor_mass, b%v_indoor_mass) .OR. &
          .NOT. reals_eq(a%a_indoor_mass, b%a_indoor_mass)) RETURN
      IF (.NOT. reals_eq(a%Tair_ind, b%Tair_ind) .OR. &
          .NOT. reals_eq(a%t_indoor_mass, b%t_indoor_mass) .OR. &
          .NOT. reals_eq(a%t_int_wall, b%t_int_wall) .OR. &
          .NOT. reals_eq(a%t_int_roof, b%t_int_roof) .OR. &
          .NOT. reals_eq(a%t_ext_wall, b%t_ext_wall) .OR. &
          .NOT. reals_eq(a%t_ext_roof, b%t_ext_roof) .OR. &
          .NOT. reals_eq(a%t_int_window, b%t_int_window) .OR. &
          .NOT. reals_eq(a%t_ext_window, b%t_ext_window) .OR. &
          .NOT. reals_eq(a%t_int_ground_floor, b%t_int_ground_floor) .OR. &
          .NOT. reals_eq(a%t_ext_ground_floor, b%t_ext_ground_floor) .OR. &
          .NOT. reals_eq(a%Twater_tank, b%Twater_tank) .OR. &
          .NOT. reals_eq(a%Tintwall_tank, b%Tintwall_tank) .OR. &
          .NOT. reals_eq(a%Textwall_tank, b%Textwall_tank) .OR. &
          .NOT. reals_eq(a%thickness_tankwall, b%thickness_tankwall) .OR. &
          .NOT. reals_eq(a%Tincomingwater_tank, b%Tincomingwater_tank) .OR. &
          .NOT. reals_eq(a%Vwater_tank, b%Vwater_tank) .OR. &
          .NOT. reals_eq(a%Asurf_tank, b%Asurf_tank) .OR. &
          .NOT. reals_eq(a%Vwall_tank, b%Vwall_tank) .OR. &
          .NOT. reals_eq(a%setTwater_tank, b%setTwater_tank) .OR. &
          .NOT. reals_eq(a%init_wtTs, b%init_wtTs)) RETURN
      IF (.NOT. reals_eq(a%Twater_vessel, b%Twater_vessel) .OR. &
          .NOT. reals_eq(a%Tintwall_vessel, b%Tintwall_vessel) .OR. &
          .NOT. reals_eq(a%Textwall_vessel, b%Textwall_vessel) .OR. &
          .NOT. reals_eq(a%thickness_wall_vessel, b%thickness_wall_vessel) .OR. &
          .NOT. reals_eq(a%Vwater_vessel, b%Vwater_vessel) .OR. &
          .NOT. reals_eq(a%Awater_vessel, b%Awater_vessel) .OR. &
          .NOT. reals_eq(a%Vwall_vessel, b%Vwall_vessel) .OR. &
          .NOT. reals_eq(a%flowrate_water_supply, b%flowrate_water_supply) .OR. &
          .NOT. mat_equal(a%flowrate_water_supply_profile, b%flowrate_water_supply_profile) .OR. &
          .NOT. reals_eq(a%single_flowrate_water_supply, b%single_flowrate_water_supply) .OR. &
          .NOT. reals_eq(a%single_flowrate_water_drain, b%single_flowrate_water_drain) .OR. &
          .NOT. reals_eq(a%cp_water, b%cp_water) .OR. &
          .NOT. reals_eq(a%cp_wall_tank, b%cp_wall_tank) .OR. &
          .NOT. reals_eq(a%cp_wall_vessel, b%cp_wall_vessel) .OR. &
          .NOT. reals_eq(a%density_water, b%density_water) .OR. &
          .NOT. reals_eq(a%density_wall_tank, b%density_wall_tank) .OR. &
          .NOT. reals_eq(a%density_wall_vessel, b%density_wall_vessel) .OR. &
          .NOT. reals_eq(a%BVF_tank, b%BVF_tank) .OR. &
          .NOT. reals_eq(a%MVF_tank, b%MVF_tank)) RETURN
      IF (.NOT. reals_eq(a%conductivity_wall_tank, b%conductivity_wall_tank) .OR. &
          .NOT. reals_eq(a%conv_coeff_intwall_tank, b%conv_coeff_intwall_tank) .OR. &
          .NOT. reals_eq(a%conv_coeff_extwall_tank, b%conv_coeff_extwall_tank) .OR. &
          .NOT. reals_eq(a%emissivity_extwall_tank, b%emissivity_extwall_tank) .OR. &
          .NOT. reals_eq(a%conductivity_wall_vessel, b%conductivity_wall_vessel) .OR. &
          .NOT. reals_eq(a%conv_coeff_intwall_vessel, b%conv_coeff_intwall_vessel) .OR. &
          .NOT. reals_eq(a%conv_coeff_extwall_vessel, b%conv_coeff_extwall_vessel) .OR. &
          .NOT. reals_eq(a%emissivity_extwall_vessel, b%emissivity_extwall_vessel) .OR. &
          .NOT. reals_eq(a%maxheatingpower_water, b%maxheatingpower_water) .OR. &
          .NOT. reals_eq(a%heating_efficiency_water, b%heating_efficiency_water) .OR. &
          .NOT. reals_eq(a%minVwater_vessel, b%minVwater_vessel) .OR. &
          .NOT. reals_eq(a%maxVwater_vessel, b%maxVwater_vessel) .OR. &
          .NOT. reals_eq(a%minHeatingPower_DHW, b%minHeatingPower_DHW) .OR. &
          .NOT. reals_eq(a%HeatingPower_DHW, b%HeatingPower_DHW)) RETURN
      IF (.NOT. reals_eq(a%qfm_dom, b%qfm_dom) .OR. &
          .NOT. reals_eq(a%qheat_dom, b%qheat_dom) .OR. &
          .NOT. reals_eq(a%qcool_dom, b%qcool_dom) .OR. &
          .NOT. reals_eq(a%qfb_hw_dom, b%qfb_hw_dom) .OR. &
          .NOT. reals_eq(a%qfb_dom_air, b%qfb_dom_air) .OR. &
          .NOT. reals_eq(a%dom_temp, b%dom_temp) .OR. &
          .NOT. reals_eq(a%QN_bldg_tstepFA, b%QN_bldg_tstepFA) .OR. &
          .NOT. reals_eq(a%QEC_bldg_tstepFA, b%QEC_bldg_tstepFA) .OR. &
          .NOT. reals_eq(a%QS_total_tstepFA, b%QS_total_tstepFA) .OR. &
          .NOT. reals_eq(a%QH_bldg_tstepFA, b%QH_bldg_tstepFA) .OR. &
          .NOT. reals_eq(a%QBAE_bldg_tstepFA, b%QBAE_bldg_tstepFA) .OR. &
          .NOT. reals_eq(a%QWaste_bldg_tstepFA, b%QWaste_bldg_tstepFA) .OR. &
          .NOT. reals_eq(a%QS_bldg_tstepFA, b%QS_bldg_tstepFA) .OR. &
          .NOT. reals_eq(a%QS_dhw_tstepFA, b%QS_dhw_tstepFA) .OR. &
          .NOT. reals_eq(a%QS_ground_tstepFA, b%QS_ground_tstepFA) .OR. &
          .NOT. reals_eq(a%QEC_heating_tstepFA, b%QEC_heating_tstepFA) .OR. &
          .NOT. reals_eq(a%QEC_cooling_tstepFA, b%QEC_cooling_tstepFA) .OR. &
          .NOT. reals_eq(a%QEC_dhw_tstepFA, b%QEC_dhw_tstepFA)) RETURN
      IF (.NOT. vec_equal(a%ts, b%ts) .OR. &
          .NOT. vec_equal(a%initTs, b%initTs) .OR. &
          .NOT. vec_equal(a%h_i, b%h_i) .OR. &
          .NOT. vec_equal(a%k_eff, b%k_eff) .OR. &
          .NOT. vec_equal(a%h_o, b%h_o) .OR. &
          .NOT. vec_equal(a%rho, b%rho) .OR. &
          .NOT. vec_equal(a%cp, b%cp) .OR. &
          .NOT. vec_equal(a%emis, b%emis) .OR. &
          .NOT. vec_equal(a%witar, b%witar) .OR. &
          .NOT. vec_equal(a%waTAR, b%waTAR) .OR. &
          .NOT. vec_equal(a%roofTAR, b%roofTAR) .OR. &
          .NOT. vec_equal(a%h_ts_average, b%h_ts_average) .OR. &
          .NOT. vec_equal(a%HWTsAverage, b%HWTsAverage) .OR. &
          .NOT. vec_equal(a%hw_power_average, b%hw_power_average) .OR. &
          .NOT. vec_equal(a%energy_exchanges, b%energy_exchanges) .OR. &
          .NOT. alloc_vec_equal(a%Textwall_C, b%Textwall_C) .OR. &
          .NOT. alloc_vec_equal(a%Textroof_C, b%Textroof_C)) RETURN
      stebbs_bldg_equal = .TRUE.
   END FUNCTION stebbs_bldg_equal

   LOGICAL FUNCTION stebbs_states_equal(a, b)
      IMPLICIT NONE
      TYPE(STEBBS_STATE), INTENT(IN) :: a, b
      INTEGER :: ib

      stebbs_states_equal = .FALSE.
      IF (.NOT. reals_eq(a%kdown_2d, b%kdown_2d) .OR. &
          .NOT. reals_eq(a%kup_2d, b%kup_2d) .OR. &
          .NOT. reals_eq(a%k_west, b%k_west) .OR. &
          .NOT. reals_eq(a%k_south, b%k_south) .OR. &
          .NOT. reals_eq(a%k_north, b%k_north) .OR. &
          .NOT. reals_eq(a%k_east, b%k_east) .OR. &
          .NOT. reals_eq(a%ldown_2d, b%ldown_2d) .OR. &
          .NOT. reals_eq(a%lup_2d, b%lup_2d) .OR. &
          .NOT. reals_eq(a%l_west, b%l_west) .OR. &
          .NOT. reals_eq(a%l_south, b%l_south) .OR. &
          .NOT. reals_eq(a%l_north, b%l_north) .OR. &
          .NOT. reals_eq(a%l_east, b%l_east)) RETURN
      IF (.NOT. vec_equal(a%z_array, b%z_array) .OR. &
          .NOT. vec_equal(a%dataout_line_u_rsl, b%dataout_line_u_rsl) .OR. &
          .NOT. vec_equal(a%dataout_line_t_rsl, b%dataout_line_t_rsl) .OR. &
          .NOT. vec_equal(a%dataout_line_q_rsl, b%dataout_line_q_rsl)) RETURN
      IF (.NOT. reals_eq(a%deep_soil_temperature, b%deep_soil_temperature) .OR. &
          .NOT. reals_eq(a%MonthMeanAirTemperature_diffmax, b%MonthMeanAirTemperature_diffmax) .OR. &
          .NOT. reals_eq(a%outdoor_air_start_temperature, b%outdoor_air_start_temperature) .OR. &
          .NOT. reals_eq(a%indoor_air_start_temperature, b%indoor_air_start_temperature) .OR. &
          .NOT. reals_eq(a%indoor_mass_start_temperature, b%indoor_mass_start_temperature) .OR. &
          .NOT. reals_eq(a%wall_indoor_surface_temperature, b%wall_indoor_surface_temperature) .OR. &
          .NOT. reals_eq(a%wall_outdoor_surface_temperature, b%wall_outdoor_surface_temperature) .OR. &
          .NOT. reals_eq(a%roof_indoor_surface_temperature, b%roof_indoor_surface_temperature) .OR. &
          .NOT. reals_eq(a%roof_outdoor_surface_temperature, b%roof_outdoor_surface_temperature) .OR. &
          .NOT. reals_eq(a%window_indoor_surface_temperature, b%window_indoor_surface_temperature) .OR. &
          .NOT. reals_eq(a%window_outdoor_surface_temperature, b%window_outdoor_surface_temperature) .OR. &
          .NOT. reals_eq(a%ground_floor_indoor_surface_temperature, b%ground_floor_indoor_surface_temperature) .OR. &
          .NOT. reals_eq(a%ground_floor_outdoor_surface_temperature, b%ground_floor_outdoor_surface_temperature) .OR. &
          .NOT. reals_eq(a%water_tank_temperature_state, b%water_tank_temperature_state) .OR. &
          .NOT. reals_eq(a%internal_wall_water_tank_temperature, b%internal_wall_water_tank_temperature) .OR. &
          .NOT. reals_eq(a%external_wall_water_tank_temperature, b%external_wall_water_tank_temperature) .OR. &
          .NOT. reals_eq(a%mains_water_temperature, b%mains_water_temperature) .OR. &
          .NOT. reals_eq(a%domestic_hot_water_temperature_in_use_in_building, &
                         b%domestic_hot_water_temperature_in_use_in_building) .OR. &
          .NOT. reals_eq(a%internal_wall_dhw_vessel_temperature, b%internal_wall_dhw_vessel_temperature) .OR. &
          .NOT. reals_eq(a%external_wall_dhw_vessel_temperature, b%external_wall_dhw_vessel_temperature) .OR. &
          .NOT. reals_eq(a%QS_stebbs, b%QS_stebbs)) RETURN
      IF (ALLOCATED(a%buildings) .NEQV. ALLOCATED(b%buildings)) RETURN
      IF (ALLOCATED(a%buildings)) THEN
         IF (SIZE(a%buildings) /= SIZE(b%buildings)) RETURN
         DO ib = 1, SIZE(a%buildings)
            IF (.NOT. stebbs_bldg_equal(a%buildings(ib), b%buildings(ib))) RETURN
         END DO
      END IF
      stebbs_states_equal = (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION stebbs_states_equal

   LOGICAL FUNCTION nhood_states_equal(a, b)
      USE module_ctrl_type, ONLY: NHOOD_STATE
      IMPLICIT NONE
      TYPE(NHOOD_STATE), INTENT(IN) :: a, b

      nhood_states_equal = &
         reals_eq(a%U_hbh_1dravg, b%U_hbh_1dravg) .AND. &
         reals_eq(a%QN_1dravg, b%QN_1dravg) .AND. &
         reals_eq(a%Tair_mn_prev, b%Tair_mn_prev) .AND. &
         reals_eq(a%iter_count, b%iter_count) .AND. &
         (a%iter_safe .EQV. b%iter_safe)
   END FUNCTION nhood_states_equal

   LOGICAL FUNCTION output_lines_equal_flag_test_pair(a_true, b_false)
      IMPLICIT NONE
      TYPE(output_line), INTENT(IN) :: a_true, b_false

      output_lines_equal_flag_test_pair = &
         vec_equal(a_true%datetimeLine, b_false%datetimeLine) .AND. &
         vec_equal(a_true%dataOutLineSUEWS, b_false%dataOutLineSUEWS) .AND. &
         vec_equal(a_true%dataOutLineSnow, b_false%dataOutLineSnow) .AND. &
         vec_equal(a_true%dataOutLineESTM, b_false%dataOutLineESTM) .AND. &
         vec_equal(a_true%dataOutLineEHC, b_false%dataOutLineEHC) .AND. &
         vec_equal(a_true%dataoutLineRSL, b_false%dataoutLineRSL) .AND. &
         vec_equal(a_true%dataOutLineBEERS, b_false%dataOutLineBEERS) .AND. &
         (a_true%dataOutLineDebug(6) == 1.0D0) .AND. &
         (b_false%dataOutLineDebug(6) == 0.0D0) .AND. &
         vec_equal(a_true%dataOutLineDebug(1:5), b_false%dataOutLineDebug(1:5)) .AND. &
         vec_equal(a_true%dataOutLineDebug(7:), b_false%dataOutLineDebug(7:)) .AND. &
         vec_equal(a_true%dataOutLineSPARTACUS, b_false%dataOutLineSPARTACUS) .AND. &
         vec_equal(a_true%dataOutLineDailyState, b_false%dataOutLineDailyState) .AND. &
         vec_equal(a_true%dataOutLineSTEBBS, b_false%dataOutLineSTEBBS) .AND. &
         vec_equal(a_true%dataOutLineNHood, b_false%dataOutLineNHood)
   END FUNCTION output_lines_equal_flag_test_pair

   LOGICAL FUNCTION output_blocks_equal(a, b)
      IMPLICIT NONE
      TYPE(output_block), INTENT(IN) :: a, b

      output_blocks_equal = &
         mat_equal(a%dataOutBlockSUEWS, b%dataOutBlockSUEWS) .AND. &
         mat_equal(a%dataOutBlockSnow, b%dataOutBlockSnow) .AND. &
         mat_equal(a%dataOutBlockESTM, b%dataOutBlockESTM) .AND. &
         mat_equal(a%dataOutBlockEHC, b%dataOutBlockEHC) .AND. &
         mat_equal(a%dataOutBlockRSL, b%dataOutBlockRSL) .AND. &
         mat_equal(a%dataOutBlockBEERS, b%dataOutBlockBEERS) .AND. &
         mat_equal(a%dataOutBlockDebug, b%dataOutBlockDebug) .AND. &
         mat_equal(a%dataOutBlockSPARTACUS, b%dataOutBlockSPARTACUS) .AND. &
         mat_equal(a%dataOutBlockDailyState, b%dataOutBlockDailyState) .AND. &
         mat_equal(a%dataOutBlockSTEBBS, b%dataOutBlockSTEBBS) .AND. &
         mat_equal(a%dataOutBlockNHood, b%dataOutBlockNHood)
   END FUNCTION output_blocks_equal

   LOGICAL FUNCTION output_blocks_equal_flag_test_pair(a_true, b_false)
      IMPLICIT NONE
      TYPE(output_block), INTENT(IN) :: a_true, b_false

      output_blocks_equal_flag_test_pair = &
         mat_equal(a_true%dataOutBlockSUEWS, b_false%dataOutBlockSUEWS) .AND. &
         mat_equal(a_true%dataOutBlockSnow, b_false%dataOutBlockSnow) .AND. &
         mat_equal(a_true%dataOutBlockESTM, b_false%dataOutBlockESTM) .AND. &
         mat_equal(a_true%dataOutBlockEHC, b_false%dataOutBlockEHC) .AND. &
         mat_equal(a_true%dataOutBlockRSL, b_false%dataOutBlockRSL) .AND. &
         mat_equal(a_true%dataOutBlockBEERS, b_false%dataOutBlockBEERS) .AND. &
         ALL(a_true%dataOutBlockDebug(:, 6) == 1.0D0) .AND. &
         ALL(b_false%dataOutBlockDebug(:, 6) == 0.0D0) .AND. &
         mat_equal(a_true%dataOutBlockDebug(:, 1:5), b_false%dataOutBlockDebug(:, 1:5)) .AND. &
         mat_equal(a_true%dataOutBlockDebug(:, 7:), b_false%dataOutBlockDebug(:, 7:)) .AND. &
         mat_equal(a_true%dataOutBlockSPARTACUS, b_false%dataOutBlockSPARTACUS) .AND. &
         mat_equal(a_true%dataOutBlockDailyState, b_false%dataOutBlockDailyState) .AND. &
         mat_equal(a_true%dataOutBlockSTEBBS, b_false%dataOutBlockSTEBBS) .AND. &
         mat_equal(a_true%dataOutBlockNHood, b_false%dataOutBlockNHood)
   END FUNCTION output_blocks_equal_flag_test_pair

   ! Complete deep comparison of all 13 sub-states of SUEWS_STATE (including
   ! every scalar, fixed array, and allocatable roof/wall/surface/building
   ! array). No fields are silently omitted.
   LOGICAL FUNCTION states_equal(a, b)
      IMPLICIT NONE
      TYPE(SUEWS_STATE), INTENT(IN) :: a, b

      states_equal = &
         error_states_equal(a%errorState, b%errorState) .AND. &
         flag_states_equal(a%flagState, b%flagState) .AND. &
         anthroemis_states_equal(a%anthroemisState, b%anthroemisState) .AND. &
         ohm_states_equal(a%ohmState, b%ohmState) .AND. &
         solar_states_equal(a%solarState, b%solarState) .AND. &
         atm_states_equal(a%atmState, b%atmState) .AND. &
         phen_states_equal(a%phenState, b%phenState) .AND. &
         snow_states_equal(a%snowState, b%snowState) .AND. &
         hydro_states_equal(a%hydroState, b%hydroState) .AND. &
         heat_states_equal(a%heatState, b%heatState) .AND. &
         roughness_states_equal(a%roughnessState, b%roughnessState) .AND. &
         stebbs_states_equal(a%stebbsState, b%stebbsState) .AND. &
         nhood_states_equal(a%nhoodState, b%nhoodState)
   END FUNCTION states_equal

   ! Complete deep comparison of all 15 captured stages of SUEWS_DEBUG.
   ! Explicit exclusion: when snow_use == 1, SUEWS_cal_Main_impl executes
   ! SUEWS_cal_snow instead of SUEWS_cal_QE and never assigns state_10_qe,
   ! leaving its debugState%init allocatables unpopulated; in that mode only
   ! stage 10 value comparison is skipped (its allocation dims are still
   ! verified by check_all_stage_allocs).
   LOGICAL FUNCTION debug_snapshots_equal(a, b, snow_use)
      IMPLICIT NONE
      TYPE(SUEWS_DEBUG), INTENT(IN) :: a, b
      INTEGER, INTENT(IN) :: snow_use

      debug_snapshots_equal = .FALSE.
      IF (.NOT. states_equal(a%state_01_dailystate, b%state_01_dailystate)) RETURN
      IF (.NOT. states_equal(a%state_02_soilmoist, b%state_02_soilmoist)) RETURN
      IF (.NOT. states_equal(a%state_03_wateruse, b%state_03_wateruse)) RETURN
      IF (.NOT. states_equal(a%state_04_anthroemis, b%state_04_anthroemis)) RETURN
      IF (.NOT. states_equal(a%state_05_qn, b%state_05_qn)) RETURN
      IF (.NOT. states_equal(a%state_06_qs, b%state_06_qs)) RETURN
      IF (.NOT. states_equal(a%state_07_qhqe_lumps, b%state_07_qhqe_lumps)) RETURN
      IF (.NOT. states_equal(a%state_08_water, b%state_08_water)) RETURN
      IF (.NOT. states_equal(a%state_09_resist, b%state_09_resist)) RETURN
      IF (snow_use /= 1) THEN
         IF (.NOT. states_equal(a%state_10_qe, b%state_10_qe)) RETURN
      END IF
      IF (.NOT. states_equal(a%state_11_qh, b%state_11_qh)) RETURN
      IF (.NOT. states_equal(a%state_12_tsurf, b%state_12_tsurf)) RETURN
      IF (.NOT. states_equal(a%state_13_rsl, b%state_13_rsl)) RETURN
      IF (.NOT. states_equal(a%state_14_biogenco2, b%state_14_biogenco2)) RETURN
      IF (.NOT. states_equal(a%state_15_beers, b%state_15_beers)) RETURN
      debug_snapshots_equal = .TRUE.
   END FUNCTION debug_snapshots_equal

   REAL(c_double) FUNCTION max_snow_abs(sn)
      USE, INTRINSIC :: iso_c_binding, ONLY: c_double
      IMPLICIT NONE
      TYPE(SNOW_STATE), INTENT(IN) :: sn

      max_snow_abs = MAX( &
         MAXVAL(ABS(sn%snow_density)), &
         MAXVAL(ABS(sn%snow_fraction)), &
         MAXVAL(ABS(sn%snow_water)), &
         ABS(sn%snow_albedo), &
         MAXVAL(ABS(sn%ice_frac)), &
         MAXVAL(ABS(sn%snow_pack)))
   END FUNCTION max_snow_abs

END SUBMODULE test_snapshot_observer
