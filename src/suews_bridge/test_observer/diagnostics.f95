! Hand-maintained native diagnostic witness (gh#2096), capi-observer only.
! No shadow types or flat-state offsets: seed real reports, synchronise the
! production TLS/timer receiver, then invoke the production warning packer.
module module_test_diagnostics
   use iso_c_binding, only: c_int, c_char, c_null_char
   use module_ctrl_type, only: SUEWS_STATE, SUEWS_TIMER
   use module_ctrl_error_state, only: reset_supy_error, set_supy_error
   use SUEWS_Driver, only: sync_error_to_state
   use module_c_api_driver, only: pack_kernel_warnings
   use module_c_api_common, only: copy_to_c_buffer
   implicit none
   private

   type, bind(C) :: native_entry_fields
      integer(c_int) :: iy, id, it, imin, is_fatal
      character(c_char) :: message(257), location(65)
   end type

   type, bind(C) :: native_diagnostic_fields
      integer(c_int) :: flag, code, has_fatal, count, n_total, n_stamped, fatal_entries
      character(c_char) :: message(513)
      type(native_entry_fields) :: entries(2)
   end type

contains

   subroutine suews_obs_diagnostics(variant, reports, fields, warn_total, warn_stored, &
      warn_max, warn_timer, warn_text, warn_text_len) bind(C, name='suews_obs_diagnostics')
      integer(c_int), value, intent(in) :: variant, reports, warn_max, warn_text_len
      type(native_diagnostic_fields), intent(out) :: fields
      integer(c_int), intent(out) :: warn_total, warn_stored, warn_timer(*)
      character(c_char), intent(out) :: warn_text(*)
      type(SUEWS_STATE) :: state
      type(SUEWS_TIMER) :: timer
      integer :: i, slot, log_idx

      call reset_supy_error()
      call state%errorState%reset()
      timer%iy = 0
      timer%id = 0
      timer%it = 0
      timer%imin = 0
      do i = 1, reports
         if ((variant == 3 .or. variant == 4) .and. i == reports) then
            timer%iy = 2014
            timer%id = 200
            timer%it = 15
            timer%imin = 35
            call state%errorState%report(message='fatal: energy closure diverged', &
               location='cal_fatal_probe', is_fatal=.true.)
            ! Stamp the fatal entry without invoking sync_error_to_state's TLS
            ! clear_fatal() branch; variant 4 separately exercises that branch.
            call state%errorState%stamp_pending(timer)
         else
            if (i == 1) then
               timer%iy = 2012
               timer%id = 60
               timer%it = 13
               timer%imin = 5
               call state%errorState%report(message='time step may be too large', &
                  location='cal_tsfc_dyohm', is_fatal=.false.)
            else
               timer%iy = 2013
               timer%id = 121
               timer%it = 7
               timer%imin = 40
               if (variant == 2) then
                  call state%errorState%report(message='boundary: using nearest level', &
                     location='', is_fatal=.false.)
               else
                  call state%errorState%report(message='z_x above array maximum, using boundary value', &
                     location='interp_z', is_fatal=.false.)
               end if
            end if
            call sync_error_to_state(state, timer)
         end if
      end do

      ! The real warning receiver does not export is_fatal or the other timer
      ! members. Assert the native warning kind here, without inventing a wire slot.
      fields%fatal_entries = 0_c_int
      do i = 1, state%errorState%count
         if (state%errorState%log(i)%is_fatal) fields%fatal_entries = fields%fatal_entries + 1_c_int
      end do
      ! Independent oracle: read named native members separately from the real
      ! packer below. Rust checks these against the fixture, then the production
      ! decoded records against these observations. No warning-buffer offsets.
      do i = 1, 2
         fields%entries(i)%iy = 0_c_int
         fields%entries(i)%id = 0_c_int
         fields%entries(i)%it = 0_c_int
         fields%entries(i)%imin = 0_c_int
         fields%entries(i)%is_fatal = 0_c_int
         fields%entries(i)%message = c_null_char
         fields%entries(i)%location = c_null_char
      end do
      do slot = 1, min(2, state%errorState%count)
         if ((variant == 3 .or. variant == 4) .and. slot == 2) then
            ! For fatal-cap bypass checks (e.g. 513 reports), inspect the final
            ! stored fatal entry alongside the first non-fatal warning.
            log_idx = state%errorState%count
         else
            log_idx = slot
         end if
         fields%entries(slot)%iy = int(state%errorState%log(log_idx)%timer%iy, c_int)
         fields%entries(slot)%id = int(state%errorState%log(log_idx)%timer%id, c_int)
         fields%entries(slot)%it = int(state%errorState%log(log_idx)%timer%it, c_int)
         fields%entries(slot)%imin = int(state%errorState%log(log_idx)%timer%imin, c_int)
         fields%entries(slot)%is_fatal = merge(1_c_int, 0_c_int, state%errorState%log(log_idx)%is_fatal)
         call copy_to_c_buffer(state%errorState%log(log_idx)%message, fields%entries(slot)%message, 257_c_int)
         call copy_to_c_buffer(state%errorState%log(log_idx)%location, fields%entries(slot)%location, 65_c_int)
      end do
      call pack_kernel_warnings(state, warn_total, warn_stored, warn_max, warn_timer, warn_text, warn_text_len)

      ! Fatal TLS synchronisation is distinct from warning collection. A real
      ! fatal simulator call returns before pack_kernel_warnings; this witness
      ! deliberately observes the two boundaries separately.
      if (variant == 1) then
         call set_supy_error(102, 'calculate_x1: invalid thermal parameter')
         call sync_error_to_state(state, timer)
      else if (variant == 2) then
         call set_supy_error(103, 'interp_z: non-finite height')
         call sync_error_to_state(state, timer)
      else if (variant == 4) then
         ! With TLS unset, sync_error_to_state invokes clear_fatal() while
         ! preserving has_fatal and the full warning log (GH#1737).
         call sync_error_to_state(state, timer)
      end if
      fields%flag = merge(1_c_int, 0_c_int, state%errorState%flag)
      fields%code = int(state%errorState%code, c_int)
      fields%has_fatal = merge(1_c_int, 0_c_int, state%errorState%has_fatal)
      fields%count = int(state%errorState%count, c_int)
      fields%n_total = int(state%errorState%n_total, c_int)
      fields%n_stamped = int(state%errorState%n_stamped, c_int)
      call copy_to_c_buffer(state%errorState%message, fields%message, int(size(fields%message), c_int))
      call reset_supy_error()
   end subroutine
end module
