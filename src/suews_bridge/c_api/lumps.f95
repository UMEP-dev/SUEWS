! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for LUMPS_PRM.
! -----------------------------------------------------------------------------
module module_c_api_lumps
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_surface, only: LUMPS_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_LUMPS_PRM_LEN = 4_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_LUMPS_PRM_SCHEMA_VERSION = 1_c_int

type :: lumps_prm_shadow
   real(c_double) :: raincover = 0.0_c_double
   real(c_double) :: rainmaxres = 0.0_c_double
   real(c_double) :: drainrt = 0.0_c_double
   integer(c_int) :: veg_type = 0_c_int
end type lumps_prm_shadow

public :: suews_lumps_prm_len
public :: suews_lumps_prm_schema_version
public :: suews_lumps_prm_default
public :: suews_lumps_error_message
public :: lumps_prm_unpack

contains

subroutine suews_lumps_prm_len(n_flat, err) bind(C, name='suews_lumps_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_LUMPS_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_lumps_prm_len

subroutine suews_lumps_prm_schema_version(schema_version, err) bind(C, name='suews_lumps_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_LUMPS_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_lumps_prm_schema_version

subroutine suews_lumps_prm_default(flat, n_flat, err) bind(C, name='suews_lumps_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(lumps_prm_shadow) :: state

   call lumps_prm_pack(state, flat, n_flat, err)

end subroutine suews_lumps_prm_default

subroutine lumps_prm_pack(state, flat, n_flat, err)
   implicit none

   type(lumps_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_LUMPS_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = state%raincover
   flat(2) = state%rainmaxres
   flat(3) = state%drainrt
   flat(4) = real(state%veg_type, c_double)

   err = SUEWS_CAPI_OK

end subroutine lumps_prm_pack

subroutine lumps_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(LUMPS_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_LUMPS_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   state%raincover = flat(1)
   state%rainmaxres = flat(2)
   state%drainrt = flat(3)
   state%veg_type = int(nint(flat(4)))

   err = SUEWS_CAPI_OK

end subroutine lumps_prm_unpack

subroutine suews_lumps_error_message(code, buffer, buffer_len) bind(C, name='suews_lumps_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_lumps_error_message

end module module_c_api_lumps

module c_api_lumps_module
use module_c_api_lumps
end module c_api_lumps_module
