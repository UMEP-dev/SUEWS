! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SURF_STORE_PRM.
! -----------------------------------------------------------------------------
module module_c_api_surf_store
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_waterdist, only: SURF_STORE_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_SURF_STORE_PRM_LEN = 6_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_SURF_STORE_PRM_SCHEMA_VERSION = 1_c_int

type :: surf_store_prm_shadow
   real(c_double) :: store_min = 0.0_c_double
   real(c_double) :: store_max = 0.0_c_double
   real(c_double) :: store_cap = 0.0_c_double
   integer(c_int) :: drain_eq = 0_c_int
   real(c_double) :: drain_coef_1 = 0.0_c_double
   real(c_double) :: drain_coef_2 = 0.0_c_double
end type surf_store_prm_shadow

public :: suews_surf_store_prm_len
public :: suews_surf_store_prm_schema_version
public :: suews_surf_store_prm_default
public :: suews_surf_store_error_message
public :: surf_store_prm_unpack

contains

subroutine suews_surf_store_prm_len(n_flat, err) bind(C, name='suews_surf_store_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_SURF_STORE_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_surf_store_prm_len

subroutine suews_surf_store_prm_schema_version(schema_version, err) bind(C, name='suews_surf_store_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_SURF_STORE_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_surf_store_prm_schema_version

subroutine suews_surf_store_prm_default(flat, n_flat, err) bind(C, name='suews_surf_store_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(surf_store_prm_shadow) :: state

   call surf_store_prm_pack(state, flat, n_flat, err)

end subroutine suews_surf_store_prm_default

subroutine surf_store_prm_pack(state, flat, n_flat, err)
   implicit none

   type(surf_store_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_SURF_STORE_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = state%store_min
   flat(2) = state%store_max
   flat(3) = state%store_cap
   flat(4) = real(state%drain_eq, c_double)
   flat(5) = state%drain_coef_1
   flat(6) = state%drain_coef_2

   err = SUEWS_CAPI_OK

end subroutine surf_store_prm_pack

subroutine surf_store_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(SURF_STORE_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_SURF_STORE_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   state%store_min = flat(1)
   state%store_max = flat(2)
   state%store_cap = flat(3)
   state%drain_eq = int(nint(flat(4)))
   state%drain_coef_1 = flat(5)
   state%drain_coef_2 = flat(6)

   err = SUEWS_CAPI_OK

end subroutine surf_store_prm_unpack

subroutine suews_surf_store_error_message(code, buffer, buffer_len) bind(C, name='suews_surf_store_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_surf_store_error_message

end module module_c_api_surf_store

module c_api_surf_store_module
use module_c_api_surf_store
end module c_api_surf_store_module
