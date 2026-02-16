! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for flag_STATE (proof type for shared codec).
!
! This adapter intentionally uses a local shadow type to avoid importing
! `module_ctrl_type` and its wider dependency graph.
! -----------------------------------------------------------------------------
module module_c_api_flag
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_ctrl_type, only: flag_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_FLAG_STATE_LEN = 5_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_FLAG_STATE_SCHEMA_VERSION = 1_c_int

type :: flag_state_shadow
   logical :: flag_converge = .false.
   integer :: i_iter = 0
   integer :: stebbs_bldg_init = 0
   logical :: snow_warning_shown = .false.
   logical :: iter_safe = .true.
end type flag_state_shadow

public :: suews_flag_state_len
public :: suews_flag_state_schema_version
public :: suews_flag_state_default
public :: suews_flag_error_message
public :: flag_state_unpack

contains

subroutine suews_flag_state_len(n_flat, err) bind(C, name='suews_flag_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_FLAG_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_flag_state_len

subroutine suews_flag_state_schema_version(schema_version, err) bind(C, name='suews_flag_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_FLAG_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_flag_state_schema_version

subroutine suews_flag_state_default(flat, n_flat, err) bind(C, name='suews_flag_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(flag_state_shadow) :: state

   call flag_state_pack(state, flat, n_flat, err)

end subroutine suews_flag_state_default

subroutine flag_state_pack(state, flat, n_flat, err)
   implicit none

   type(flag_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: n_flat_use
   integer :: idx

   n_flat_use = n_flat
   if (n_flat_use<SUEWS_CAPI_FLAG_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%flag_converge); idx = idx + 1
   flat(idx) = real(state%i_iter, c_double); idx = idx + 1
   flat(idx) = real(state%stebbs_bldg_init, c_double); idx = idx + 1
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%snow_warning_shown); idx = idx + 1
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine flag_state_pack

subroutine flag_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(flag_STATE), intent(out) :: state
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_FLAG_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   state%flag_converge = flat(1)>=0.5_c_double
   state%i_iter = int(nint(flat(2)))
   state%stebbs_bldg_init = int(nint(flat(3)))
   state%snow_warning_shown = flat(4)>=0.5_c_double
   state%iter_safe = flat(5)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine flag_state_unpack

subroutine suews_flag_error_message(code, buffer, buffer_len) bind(C, name='suews_flag_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_flag_error_message

end module module_c_api_flag

! Backward compatibility alias (deprecated - will be removed in future version)
module c_api_flag_module
use module_c_api_flag
end module c_api_flag_module
