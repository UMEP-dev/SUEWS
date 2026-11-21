! Opt-in native observation of SUEWS_cal_Main's real optional snapshots.
! This is not a production snapshot transport or a legacy multi-step caller.
module module_test_snapshot_observer
   use, intrinsic :: iso_c_binding, only: c_int, c_double
   use module_ctrl_type, only: SUEWS_CONFIG, SUEWS_TIMER, SUEWS_SITE, SUEWS_STATE, &
      SUEWS_FORCING, SUEWS_DEBUG, SUEWS_STATE_BLOCK, output_line
   use module_c_api_driver, only: unpack_config, unpack_timer, unpack_site_scalars, &
      unpack_site_members, unpack_state_members
   use module_ctrl_error_state, only: reset_supy_error, supy_error_flag
   use SUEWS_Driver, only: SUEWS_cal_Main
   implicit none
contains
   subroutine snapshot_observe(config_flat, timer_flat, scalars, site_flat, site_len, site_toc, &
      state_flat, state_len, state_toc, nlayer, ndepth, observed, err) bind(c)
      real(c_double), intent(in) :: config_flat(24), timer_flat(18), scalars(24)
      integer(c_int), value :: site_len, state_len, nlayer, ndepth
      real(c_double), intent(in) :: site_flat(site_len), state_flat(state_len)
      integer(c_int), intent(in) :: site_toc(36), state_toc(26)
      ! Per timestep: Qn at stage 05, Qs before/after stage 06, QH at stage 11,
      ! convergence before/after temperature update, two dimensions, final Qn.
      real(c_double), intent(out) :: observed(9, 3)
      integer(c_int), intent(out) :: err
      type(SUEWS_CONFIG) :: config
      type(SUEWS_TIMER) :: timer
      type(SUEWS_SITE) :: site
      type(SUEWS_STATE) :: state, control_state
      type(SUEWS_FORCING) :: forcing, control_forcing
      type(SUEWS_DEBUG) :: snapshots
      type(SUEWS_STATE_BLOCK) :: block
      type(output_line) :: output, control_output
      integer :: row

      observed = 0.0d0
      call reset_supy_error()
      call unpack_config(config_flat, 24_c_int, config, err)
      if (err /= 0) return
      call unpack_timer(timer_flat, 18_c_int, timer, err)
      if (err /= 0) return
      call site%allocate(nlayer)
      call site%ehc%allocate(nlayer, ndepth)
      call site%spartacus_layer%allocate(nlayer)
      call site%spartacus%allocate(nlayer)
      site%nlayer = nlayer
      call unpack_site_scalars(scalars, 24_c_int, site, err)
      if (err /= 0) return
      call unpack_site_members(site, nlayer, ndepth, site_flat, site_len, site_toc, 36_c_int, 18_c_int, err)
      if (err /= 0) return
      call site%cal_surf(config)
      call state%allocate(nlayer, ndepth)
      call unpack_state_members(state, nlayer, ndepth, state_flat, state_len, state_toc, 26_c_int, 13_c_int, err)
      if (err /= 0) return
      config%flag_test = .true.
      call block%init(nlayer, ndepth, 3)
      if (size(block%block) /= 3) then
         err = 101
         return
      end if
      do row = 1, 3
         timer%iy = 2012
         timer%id = 180
         timer%it = 12
         timer%imin = 5 * row
         forcing%U = 2.0d0
         forcing%RH = 60.0d0
         forcing%temp_c = 20.0d0 + row
         forcing%pres = 1013.0d0
         forcing%rain = 0.4d0 * row
         forcing%kdown = 100.0d0 + 130.0d0 * row
         forcing%l_down = 350.0d0
         forcing%f_cloud = 0.2d0
         forcing%qn1_obs = 100.0d0 + 37.0d0 * row
         forcing%qs_obs = 10.0d0 + 13.0d0 * row
         forcing%qf_obs = 5.0d0
         control_state = state
         control_forcing = forcing
         ! Identical wet timestep without the optional argument must agree.
         config%flag_test = .false.
         call SUEWS_cal_Main(timer, control_forcing, config, site, control_state, control_output)
         if (supy_error_flag()) then
            err = 103
            return
         end if
         config%flag_test = .true.
         call SUEWS_cal_Main(timer, forcing, config, site, state, output, snapshots)
         if (supy_error_flag()) then
            err = 102
            return
         end if
         if (any(output%dataOutLineSUEWS /= control_output%dataOutLineSUEWS)) then
            err = 104
            return
         end if
         if (snapshots%state_06_qs%heatState%iter_safe) then
            err = 105
            return
         end if
         if (.not. snapshots%state_06_qs%flagState%iter_safe) then
            err = 106
            return
         end if
         if (.not. allocated(snapshots%state_08_water%heatState%temp_roof)) then
            err = 107
            return
         end if
         observed(1, row) = snapshots%state_05_qn%heatState%qn
         observed(2, row) = snapshots%state_05_qn%heatState%qs
         observed(3, row) = snapshots%state_06_qs%heatState%qs
         observed(4, row) = snapshots%state_11_qh%heatState%qh
         observed(5, row) = merge(1.0d0, 0.0d0, snapshots%state_11_qh%flagState%flag_converge)
         observed(6, row) = merge(1.0d0, 0.0d0, snapshots%state_12_tsurf%flagState%flag_converge)
         observed(7, row) = size(snapshots%state_08_water%heatState%temp_roof, 1)
         observed(8, row) = size(snapshots%state_08_water%heatState%temp_roof, 2)
         ! Harness-owned storage: proves allocation/deep-copy only, NOT legacy row capture.
         block%block(row) = state
         timer%dt_since_start = timer%dt_since_start + timer%tstep
      end do
      do row = 1, 3
         observed(9, row) = block%block(row)%heatState%qn
      end do
      err = 0
   end subroutine snapshot_observe
end module module_test_snapshot_observer
