! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for CONDUCTANCE_PRM.
! -----------------------------------------------------------------------------
module module_c_api_conductance
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_surface, only: CONDUCTANCE_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_CONDUCTANCE_PRM_LEN = 12_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_CONDUCTANCE_PRM_SCHEMA_VERSION = 1_c_int

type :: conductance_prm_shadow
   real(c_double) :: g_max = 0.0_c_double
   real(c_double) :: g_k = 0.0_c_double
   real(c_double) :: g_q_base = 0.0_c_double
   real(c_double) :: g_q_shape = 0.0_c_double
   real(c_double) :: g_t = 0.0_c_double
   real(c_double) :: g_sm = 0.0_c_double
   real(c_double) :: kmax = 0.0_c_double
   integer(c_int) :: gsmodel = 0_c_int
   real(c_double) :: s1 = 0.0_c_double
   real(c_double) :: s2 = 0.0_c_double
   real(c_double) :: th = 0.0_c_double
   real(c_double) :: tl = 0.0_c_double
end type conductance_prm_shadow

public :: suews_conductance_prm_len
public :: suews_conductance_prm_schema_version
public :: suews_conductance_prm_default
public :: suews_conductance_error_message
public :: conductance_prm_unpack

contains

subroutine suews_conductance_prm_len(n_flat, err) bind(C, name='suews_conductance_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_CONDUCTANCE_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_conductance_prm_len

subroutine suews_conductance_prm_schema_version(schema_version, err) bind(C, name='suews_conductance_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_CONDUCTANCE_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_conductance_prm_schema_version

subroutine suews_conductance_prm_default(flat, n_flat, err) bind(C, name='suews_conductance_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(conductance_prm_shadow) :: state

   call conductance_prm_pack(state, flat, n_flat, err)

end subroutine suews_conductance_prm_default

subroutine conductance_prm_pack(state, flat, n_flat, err)
   implicit none

   type(conductance_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_CONDUCTANCE_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   flat(1) = state%g_max
   flat(2) = state%g_k
   flat(3) = state%g_q_base
   flat(4) = state%g_q_shape
   flat(5) = state%g_t
   flat(6) = state%g_sm
   flat(7) = state%kmax
   flat(8) = real(state%gsmodel, c_double)
   flat(9) = state%s1
   flat(10) = state%s2
   flat(11) = state%th
   flat(12) = state%tl

   err = SUEWS_CAPI_OK

end subroutine conductance_prm_pack

subroutine conductance_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(CONDUCTANCE_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   if (n_flat<SUEWS_CAPI_CONDUCTANCE_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   state%g_max = flat(1)
   state%g_k = flat(2)
   state%g_q_base = flat(3)
   state%g_q_shape = flat(4)
   state%g_t = flat(5)
   state%g_sm = flat(6)
   state%kmax = flat(7)
   state%gsmodel = int(nint(flat(8)))
   state%s1 = flat(9)
   state%s2 = flat(10)
   state%TH = flat(11)
   state%TL = flat(12)

   err = SUEWS_CAPI_OK

end subroutine conductance_prm_unpack

subroutine suews_conductance_error_message(code, buffer, buffer_len) bind(C, name='suews_conductance_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_conductance_error_message

end module module_c_api_conductance

module c_api_conductance_module
use module_c_api_conductance
end module c_api_conductance_module
