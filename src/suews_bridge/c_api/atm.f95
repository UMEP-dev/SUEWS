! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for atm_state.
! -----------------------------------------------------------------------------
module module_c_api_atm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_ctrl_const_allocate, only: nsurf
use module_type_atmosphere, only: atm_state

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_ATM_STATE_LEN = 39_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_ATM_STATE_SCHEMA_VERSION = 1_c_int

type :: atm_state_shadow
   real(c_double) :: fcld = 0.0_c_double
   real(c_double) :: avcp = 0.0_c_double
   real(c_double) :: dens_dry = 0.0_c_double
   real(c_double) :: avdens = 0.0_c_double
   real(c_double) :: dq = 0.0_c_double
   real(c_double) :: ea_hpa = 0.0_c_double
   real(c_double) :: es_hpa = 0.0_c_double
   real(c_double) :: lv_j_kg = 0.0_c_double
   real(c_double) :: lvs_j_kg = 0.0_c_double
   real(c_double) :: tlv = 0.0_c_double
   real(c_double) :: psyc_hpa = 0.0_c_double
   real(c_double) :: psycice_hpa = 0.0_c_double
   real(c_double) :: s_pa = 0.0_c_double
   real(c_double) :: s_hpa = 0.0_c_double
   real(c_double) :: sice_hpa = 0.0_c_double
   real(c_double) :: vpd_hpa = 0.0_c_double
   real(c_double) :: vpd_pa = 0.0_c_double
   real(c_double) :: u10_ms = 0.0_c_double
   real(c_double) :: u_hbh = 0.0_c_double
   real(c_double) :: t2_c = 0.0_c_double
   real(c_double) :: t_half_bldg_c = 0.0_c_double
   real(c_double) :: q2_gkg = 0.0_c_double
   real(c_double) :: rh2 = 0.0_c_double
   real(c_double) :: l_mod = 0.0_c_double
   real(c_double) :: zl = 0.0_c_double
   real(c_double) :: ra_h = 0.0_c_double
   real(c_double) :: rs = 0.0_c_double
   real(c_double) :: ustar = 0.0_c_double
   real(c_double) :: tstar = 0.0_c_double
   real(c_double) :: rb = 0.0_c_double
   real(c_double) :: tair_av = 0.0_c_double
   real(c_double), dimension(nsurf) :: rss_surf = 0.0_c_double
   logical :: iter_safe = .true.
end type atm_state_shadow

public :: suews_atm_state_len
public :: suews_atm_state_schema_version
public :: suews_atm_state_default
public :: suews_atm_error_message
public :: atm_state_unpack

contains

subroutine suews_atm_state_len(n_flat, err) bind(C, name='suews_atm_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_ATM_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_atm_state_len

subroutine suews_atm_state_schema_version(schema_version, err) bind(C, name='suews_atm_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_ATM_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_atm_state_schema_version

subroutine suews_atm_state_default(flat, n_flat, err) bind(C, name='suews_atm_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(atm_state_shadow) :: state

   call atm_state_pack(state, flat, n_flat, err)

end subroutine suews_atm_state_default

subroutine atm_state_pack(state, flat, n_flat, err)
   implicit none

   type(atm_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer :: idx
   integer :: i

   if (n_flat<SUEWS_CAPI_ATM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   flat(idx) = state%fcld; idx = idx + 1
   flat(idx) = state%avcp; idx = idx + 1
   flat(idx) = state%dens_dry; idx = idx + 1
   flat(idx) = state%avdens; idx = idx + 1
   flat(idx) = state%dq; idx = idx + 1
   flat(idx) = state%ea_hpa; idx = idx + 1
   flat(idx) = state%es_hpa; idx = idx + 1
   flat(idx) = state%lv_j_kg; idx = idx + 1
   flat(idx) = state%lvs_j_kg; idx = idx + 1
   flat(idx) = state%tlv; idx = idx + 1
   flat(idx) = state%psyc_hpa; idx = idx + 1
   flat(idx) = state%psycice_hpa; idx = idx + 1
   flat(idx) = state%s_pa; idx = idx + 1
   flat(idx) = state%s_hpa; idx = idx + 1
   flat(idx) = state%sice_hpa; idx = idx + 1
   flat(idx) = state%vpd_hpa; idx = idx + 1
   flat(idx) = state%vpd_pa; idx = idx + 1
   flat(idx) = state%u10_ms; idx = idx + 1
   flat(idx) = state%u_hbh; idx = idx + 1
   flat(idx) = state%t2_c; idx = idx + 1
   flat(idx) = state%t_half_bldg_c; idx = idx + 1
   flat(idx) = state%q2_gkg; idx = idx + 1
   flat(idx) = state%rh2; idx = idx + 1
   flat(idx) = state%l_mod; idx = idx + 1
   flat(idx) = state%zl; idx = idx + 1
   flat(idx) = state%ra_h; idx = idx + 1
   flat(idx) = state%rs; idx = idx + 1
   flat(idx) = state%ustar; idx = idx + 1
   flat(idx) = state%tstar; idx = idx + 1
   flat(idx) = state%rb; idx = idx + 1
   flat(idx) = state%tair_av; idx = idx + 1

   do i = 1, nsurf
      flat(idx) = state%rss_surf(i)
      idx = idx + 1
   end do

   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine atm_state_pack

subroutine atm_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(atm_state), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_ATM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   state%fcld = flat(idx); idx = idx + 1_c_int
   state%avcp = flat(idx); idx = idx + 1_c_int
   state%dens_dry = flat(idx); idx = idx + 1_c_int
   state%avdens = flat(idx); idx = idx + 1_c_int
   state%dq = flat(idx); idx = idx + 1_c_int
   state%ea_hpa = flat(idx); idx = idx + 1_c_int
   state%es_hpa = flat(idx); idx = idx + 1_c_int
   state%lv_j_kg = flat(idx); idx = idx + 1_c_int
   state%lvs_j_kg = flat(idx); idx = idx + 1_c_int
   state%tlv = flat(idx); idx = idx + 1_c_int
   state%psyc_hpa = flat(idx); idx = idx + 1_c_int
   state%psycice_hpa = flat(idx); idx = idx + 1_c_int
   state%s_pa = flat(idx); idx = idx + 1_c_int
   state%s_hpa = flat(idx); idx = idx + 1_c_int
   state%sice_hpa = flat(idx); idx = idx + 1_c_int
   state%vpd_hpa = flat(idx); idx = idx + 1_c_int
   state%vpd_pa = flat(idx); idx = idx + 1_c_int
   state%u10_ms = flat(idx); idx = idx + 1_c_int
   state%u_hbh = flat(idx); idx = idx + 1_c_int
   state%t2_c = flat(idx); idx = idx + 1_c_int
   state%t_half_bldg_c = flat(idx); idx = idx + 1_c_int
   state%q2_gkg = flat(idx); idx = idx + 1_c_int
   state%rh2 = flat(idx); idx = idx + 1_c_int
   state%l_mod = flat(idx); idx = idx + 1_c_int
   state%zl = flat(idx); idx = idx + 1_c_int
   state%ra_h = flat(idx); idx = idx + 1_c_int
   state%rs = flat(idx); idx = idx + 1_c_int
   state%ustar = flat(idx); idx = idx + 1_c_int
   state%tstar = flat(idx); idx = idx + 1_c_int
   state%rb = flat(idx); idx = idx + 1_c_int
   state%tair_av = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      state%rss_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%iter_safe = flat(idx)>=0.5_c_double
   err = SUEWS_CAPI_OK

end subroutine atm_state_unpack

subroutine suews_atm_error_message(code, buffer, buffer_len) bind(C, name='suews_atm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_atm_error_message

end module module_c_api_atm

module c_api_atm_module
use module_c_api_atm
end module c_api_atm_module
