! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for ROUGHNESS_STATE.
! -----------------------------------------------------------------------------
module module_c_api_roughness
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_surface, only: ROUGHNESS_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_ROUGHNESS_STATE_LEN = 11_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_ROUGHNESS_STATE_SCHEMA_VERSION = 1_c_int

type :: roughness_state_shadow
   real(c_double) :: faibldg_use = 0.0_c_double
   real(c_double) :: faievetree_use = 0.0_c_double
   real(c_double) :: faidectree_use = 0.0_c_double
   real(c_double) :: fai = 0.0_c_double
   real(c_double) :: pai = 0.0_c_double
   real(c_double) :: zh = 0.0_c_double
   real(c_double) :: z0m = 0.0_c_double
   real(c_double) :: z0v = 0.0_c_double
   real(c_double) :: zdm = 0.0_c_double
   real(c_double) :: zzd = 0.0_c_double
   logical :: iter_safe = .true.
end type roughness_state_shadow

public :: suews_roughness_state_len
public :: suews_roughness_state_schema_version
public :: suews_roughness_state_default
public :: suews_roughness_error_message
public :: roughness_state_unpack

contains

subroutine suews_roughness_state_len(n_flat, err) bind(C, name='suews_roughness_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_ROUGHNESS_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_roughness_state_len

subroutine suews_roughness_state_schema_version(schema_version, err) bind(C, name='suews_roughness_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_ROUGHNESS_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_roughness_state_schema_version

subroutine suews_roughness_state_default(flat, n_flat, err) bind(C, name='suews_roughness_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(roughness_state_shadow) :: state

   call roughness_state_pack(state, flat, n_flat, err)

end subroutine suews_roughness_state_default

subroutine roughness_state_pack(state, flat, n_flat, err)
   implicit none

   type(roughness_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_ROUGHNESS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = state%faibldg_use
   flat(2) = state%faievetree_use
   flat(3) = state%faidectree_use
   flat(4) = state%fai
   flat(5) = state%pai
   flat(6) = state%zh
   flat(7) = state%z0m
   flat(8) = state%z0v
   flat(9) = state%zdm
   flat(10) = state%zzd
   flat(11) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine roughness_state_pack

subroutine roughness_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(ROUGHNESS_STATE), intent(out) :: state
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_ROUGHNESS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   state%faibldg_use = flat(1)
   state%faievetree_use = flat(2)
   state%faidectree_use = flat(3)
   state%fai = flat(4)
   state%pai = flat(5)
   state%zh = flat(6)
   state%z0m = flat(7)
   state%z0v = flat(8)
   state%zdm = flat(9)
   state%zzd = flat(10)
   state%iter_safe = flat(11)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine roughness_state_unpack

subroutine suews_roughness_error_message(code, buffer, buffer_len) bind(C, name='suews_roughness_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_roughness_error_message

end module module_c_api_roughness

module c_api_roughness_module
use module_c_api_roughness
end module c_api_roughness_module
