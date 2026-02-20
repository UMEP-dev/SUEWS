! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for NHOOD_STATE.
! -----------------------------------------------------------------------------
module module_c_api_nhood
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_stebbs, only: NHOOD_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_NHOOD_STATE_LEN = 5_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_NHOOD_STATE_SCHEMA_VERSION = 1_c_int

type :: nhood_state_shadow
   real(c_double) :: u_hbh_1dravg = 0.0_c_double
   real(c_double) :: qn_1dravg = 0.0_c_double
   real(c_double) :: tair_mn_prev = 0.0_c_double
   real(c_double) :: iter_count = 0.0_c_double
   logical :: iter_safe = .false.
end type nhood_state_shadow

public :: suews_nhood_state_len
public :: suews_nhood_state_schema_version
public :: suews_nhood_state_default
public :: suews_nhood_error_message
public :: nhood_state_unpack

contains

subroutine suews_nhood_state_len(n_flat, err) bind(C, name='suews_nhood_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_NHOOD_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_nhood_state_len

subroutine suews_nhood_state_schema_version(schema_version, err) bind(C, name='suews_nhood_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_NHOOD_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_nhood_state_schema_version

subroutine suews_nhood_state_default(flat, n_flat, err) bind(C, name='suews_nhood_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(nhood_state_shadow) :: state

   call nhood_state_pack(state, flat, n_flat, err)

end subroutine suews_nhood_state_default

subroutine nhood_state_pack(state, flat, n_flat, err)
   implicit none

   type(nhood_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_NHOOD_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = state%u_hbh_1dravg
   flat(2) = state%qn_1dravg
   flat(3) = state%tair_mn_prev
   flat(4) = state%iter_count
   flat(5) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine nhood_state_pack

subroutine nhood_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(NHOOD_STATE), intent(out) :: state
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_NHOOD_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   state%u_hbh_1dravg = flat(1)
   state%qn_1dravg = flat(2)
   state%tair_mn_prev = flat(3)
   state%iter_count = flat(4)
   state%iter_safe = flat(5)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine nhood_state_unpack

subroutine suews_nhood_error_message(code, buffer, buffer_len) bind(C, name='suews_nhood_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_nhood_error_message

end module module_c_api_nhood

module c_api_nhood_module
use module_c_api_nhood
end module c_api_nhood_module
