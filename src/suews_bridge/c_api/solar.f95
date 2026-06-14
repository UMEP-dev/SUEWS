! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for solar_State.
!
! This adapter uses a local shadow type to avoid importing full physics/control
! modules into the bridge build.
! -----------------------------------------------------------------------------
module module_c_api_solar
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_atmosphere, only: solar_State

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_SOLAR_STATE_LEN = 3_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_SOLAR_STATE_SCHEMA_VERSION = 1_c_int

type :: solar_state_shadow
   real(c_double) :: azimuth_deg = 0.0_c_double
   real(c_double) :: zenith_deg = 0.0_c_double
   logical :: iter_safe = .true.
end type solar_state_shadow

public :: suews_solar_state_len
public :: suews_solar_state_schema_version
public :: suews_solar_state_default
public :: suews_solar_error_message
public :: solar_state_unpack

contains

subroutine suews_solar_state_len(n_flat, err) bind(C, name='suews_solar_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_SOLAR_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_solar_state_len

subroutine suews_solar_state_schema_version(schema_version, err) bind(C, name='suews_solar_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_SOLAR_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_solar_state_schema_version

subroutine suews_solar_state_default(flat, n_flat, err) bind(C, name='suews_solar_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(solar_state_shadow) :: state

   call solar_state_pack(state, flat, n_flat, err)

end subroutine suews_solar_state_default

subroutine solar_state_pack(state, flat, n_flat, err)
   implicit none

   type(solar_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_SOLAR_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = state%azimuth_deg
   flat(2) = state%zenith_deg
   flat(3) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine solar_state_pack

subroutine solar_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(solar_State), intent(out) :: state
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_SOLAR_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   state%azimuth_deg = flat(1)
   state%zenith_deg = flat(2)
   state%iter_safe = flat(3)>=0.5_c_double
   err = SUEWS_CAPI_OK

end subroutine solar_state_unpack

subroutine suews_solar_error_message(code, buffer, buffer_len) bind(C, name='suews_solar_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_solar_error_message

end module module_c_api_solar

module c_api_solar_module
use module_c_api_solar
end module c_api_solar_module
