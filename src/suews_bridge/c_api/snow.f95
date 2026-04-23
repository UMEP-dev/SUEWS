! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SNOW_STATE.
! -----------------------------------------------------------------------------
module module_c_api_snow
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_ctrl_const_allocate, only: nsurf
use module_type_snow, only: SNOW_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_SNOW_STATE_LEN = 79_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_SNOW_STATE_SCHEMA_VERSION = 1_c_int

type :: snow_state_shadow
   real(c_double) :: snowfall_cum = 0.0_c_double
   real(c_double) :: snow_albedo = 0.0_c_double
   real(c_double) :: chsnow_per_interval = 0.0_c_double
   real(c_double) :: mwh = 0.0_c_double
   real(c_double) :: melt_water_store = 0.0_c_double
   real(c_double) :: qn_snow = 0.0_c_double
   real(c_double) :: qm = 0.0_c_double
   real(c_double) :: qm_freeze = 0.0_c_double
   real(c_double) :: qm_rain = 0.0_c_double
   real(c_double) :: swe = 0.0_c_double
   real(c_double) :: z0v_snow = 0.0_c_double
   real(c_double) :: ra_snow = 0.0_c_double
   real(c_double) :: s_ice_hpa = 0.0_c_double
   real(c_double), dimension(2) :: snow_removal = 0.0_c_double
   real(c_double), dimension(nsurf) :: ice_frac = 0.0_c_double
   real(c_double), dimension(nsurf) :: snow_density = 0.0_c_double
   real(c_double), dimension(nsurf) :: snow_fraction = 0.0_c_double
   real(c_double), dimension(nsurf) :: snow_pack = 0.0_c_double
   real(c_double), dimension(nsurf) :: snow_water = 0.0_c_double
   real(c_double), dimension(nsurf) :: kup_ind_snow = 0.0_c_double
   real(c_double), dimension(nsurf) :: qn_ind_snow = 0.0_c_double
   real(c_double), dimension(nsurf) :: delta_qi = 0.0_c_double
   real(c_double), dimension(nsurf) :: tsurf_ind_snow = 0.0_c_double
   logical :: iter_safe = .false.
end type snow_state_shadow

public :: suews_snow_state_len
public :: suews_snow_state_schema_version
public :: suews_snow_state_default
public :: suews_snow_error_message
public :: snow_state_unpack

contains

subroutine suews_snow_state_len(n_flat, err) bind(C, name='suews_snow_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_SNOW_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_snow_state_len

subroutine suews_snow_state_schema_version(schema_version, err) bind(C, name='suews_snow_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_SNOW_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_snow_state_schema_version

subroutine suews_snow_state_default(flat, n_flat, err) bind(C, name='suews_snow_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(snow_state_shadow) :: state

   call snow_state_pack(state, flat, n_flat, err)

end subroutine suews_snow_state_default

subroutine snow_state_pack(state, flat, n_flat, err)
   implicit none

   type(snow_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer :: idx
   integer :: i

   if (n_flat<SUEWS_CAPI_SNOW_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1

   flat(idx) = state%snowfall_cum; idx = idx + 1
   flat(idx) = state%snow_albedo; idx = idx + 1
   flat(idx) = state%chsnow_per_interval; idx = idx + 1
   flat(idx) = state%mwh; idx = idx + 1
   flat(idx) = state%melt_water_store; idx = idx + 1
   flat(idx) = state%qn_snow; idx = idx + 1
   flat(idx) = state%qm; idx = idx + 1
   flat(idx) = state%qm_freeze; idx = idx + 1
   flat(idx) = state%qm_rain; idx = idx + 1
   flat(idx) = state%swe; idx = idx + 1
   flat(idx) = state%z0v_snow; idx = idx + 1
   flat(idx) = state%ra_snow; idx = idx + 1
   flat(idx) = state%s_ice_hpa; idx = idx + 1

   do i = 1, 2
      flat(idx) = state%snow_removal(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%ice_frac(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%snow_density(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%snow_fraction(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%snow_pack(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%snow_water(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%kup_ind_snow(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%qn_ind_snow(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%delta_qi(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%tsurf_ind_snow(i)
      idx = idx + 1
   end do

   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine snow_state_pack

subroutine snow_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(SNOW_STATE), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_SNOW_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   state%snowfall_cum = flat(idx); idx = idx + 1_c_int
   state%snow_albedo = flat(idx); idx = idx + 1_c_int
   state%chsnow_per_interval = flat(idx); idx = idx + 1_c_int
   state%mwh = flat(idx); idx = idx + 1_c_int
   state%melt_water_store = flat(idx); idx = idx + 1_c_int
   state%qn_snow = flat(idx); idx = idx + 1_c_int
   state%qm = flat(idx); idx = idx + 1_c_int
   state%qm_freeze = flat(idx); idx = idx + 1_c_int
   state%qm_rain = flat(idx); idx = idx + 1_c_int
   state%swe = flat(idx); idx = idx + 1_c_int
   state%z0v_snow = flat(idx); idx = idx + 1_c_int
   state%ra_snow = flat(idx); idx = idx + 1_c_int
   state%s_ice_hpa = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, 2_c_int
      state%snow_removal(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nsurf, c_int)
      state%ice_frac(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%snow_density(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%snow_fraction(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%snow_pack(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%snow_water(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%kup_ind_snow(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%qn_ind_snow(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%delta_qi(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%tsurf_ind_snow(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%iter_safe = flat(idx)>=0.5_c_double
   err = SUEWS_CAPI_OK

end subroutine snow_state_unpack

subroutine suews_snow_error_message(code, buffer, buffer_len) bind(C, name='suews_snow_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_snow_error_message

end module module_c_api_snow

module c_api_snow_module
use module_c_api_snow
end module c_api_snow_module
