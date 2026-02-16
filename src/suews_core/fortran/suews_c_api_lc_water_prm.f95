! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for LC_WATER_PRM.
! -----------------------------------------------------------------------------
module module_c_api_lc_water_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_landcover, only: LC_WATER_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_LC_WATER_PRM_LEN = 26_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_LC_WATER_PRM_SCHEMA_VERSION = 1_c_int

type :: ohm_coef_lc_shadow
   real(c_double) :: summer_dry = 0.0_c_double
   real(c_double) :: summer_wet = 0.0_c_double
   real(c_double) :: winter_dry = 0.0_c_double
   real(c_double) :: winter_wet = 0.0_c_double
end type ohm_coef_lc_shadow

type :: ohm_prm_shadow
   real(c_double) :: chanohm = 0.0_c_double
   real(c_double) :: cpanohm = 0.0_c_double
   real(c_double) :: kkanohm = 0.0_c_double
   real(c_double) :: ohm_threshsw = 0.0_c_double
   real(c_double) :: ohm_threshwd = 0.0_c_double
   type(ohm_coef_lc_shadow), dimension(3) :: ohm_coef_lc
end type ohm_prm_shadow

type :: soil_prm_shadow
   real(c_double) :: soildepth = 0.0_c_double
   real(c_double) :: soilstorecap = 0.0_c_double
   real(c_double) :: sathydraulicconduct = 0.0_c_double
end type soil_prm_shadow

type :: lc_water_prm_shadow
   real(c_double) :: sfr = 0.0_c_double
   real(c_double) :: emis = 0.0_c_double
   type(ohm_prm_shadow) :: ohm
   type(soil_prm_shadow) :: soil
   real(c_double) :: statelimit = 0.0_c_double
   real(c_double) :: irrfracwater = 0.0_c_double
   real(c_double) :: wetthresh = 0.0_c_double
   real(c_double) :: flowchange = 0.0_c_double
end type lc_water_prm_shadow

public :: suews_lc_water_prm_len
public :: suews_lc_water_prm_schema_version
public :: suews_lc_water_prm_default
public :: suews_lc_water_prm_error_message
public :: lc_water_prm_unpack

contains

subroutine suews_lc_water_prm_len(n_flat, err) bind(C, name='suews_lc_water_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_LC_WATER_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_lc_water_prm_len

subroutine suews_lc_water_prm_schema_version(schema_version, err) bind(C, name='suews_lc_water_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_LC_WATER_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_lc_water_prm_schema_version

subroutine suews_lc_water_prm_default(flat, n_flat, err) bind(C, name='suews_lc_water_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(lc_water_prm_shadow) :: state

   call lc_water_prm_pack(state, flat, n_flat, err)

end subroutine suews_lc_water_prm_default

subroutine lc_water_prm_pack(state, flat, n_flat, err)
   implicit none

   type(lc_water_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer :: i

   if (n_flat<SUEWS_CAPI_LC_WATER_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   flat(idx) = state%sfr; idx = idx + 1
   flat(idx) = state%emis; idx = idx + 1

   flat(idx) = state%ohm%chanohm; idx = idx + 1
   flat(idx) = state%ohm%cpanohm; idx = idx + 1
   flat(idx) = state%ohm%kkanohm; idx = idx + 1
   flat(idx) = state%ohm%ohm_threshsw; idx = idx + 1
   flat(idx) = state%ohm%ohm_threshwd; idx = idx + 1
   do i = 1, 3
      flat(idx) = state%ohm%ohm_coef_lc(i)%summer_dry; idx = idx + 1
      flat(idx) = state%ohm%ohm_coef_lc(i)%summer_wet; idx = idx + 1
      flat(idx) = state%ohm%ohm_coef_lc(i)%winter_dry; idx = idx + 1
      flat(idx) = state%ohm%ohm_coef_lc(i)%winter_wet; idx = idx + 1
   end do

   flat(idx) = state%soil%soildepth; idx = idx + 1
   flat(idx) = state%soil%soilstorecap; idx = idx + 1
   flat(idx) = state%soil%sathydraulicconduct; idx = idx + 1

   flat(idx) = state%statelimit; idx = idx + 1
   flat(idx) = state%irrfracwater; idx = idx + 1
   flat(idx) = state%wetthresh; idx = idx + 1
   flat(idx) = state%flowchange; idx = idx + 1

   err = SUEWS_CAPI_OK

end subroutine lc_water_prm_pack

subroutine lc_water_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(LC_WATER_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_LC_WATER_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   state%sfr = flat(idx); idx = idx + 1_c_int
   state%emis = flat(idx); idx = idx + 1_c_int

   state%ohm%chanohm = flat(idx); idx = idx + 1_c_int
   state%ohm%cpanohm = flat(idx); idx = idx + 1_c_int
   state%ohm%kkanohm = flat(idx); idx = idx + 1_c_int
   state%ohm%ohm_threshsw = flat(idx); idx = idx + 1_c_int
   state%ohm%ohm_threshwd = flat(idx); idx = idx + 1_c_int
   do i = 1_c_int, 3_c_int
      state%ohm%ohm_coef_lc(i)%summer_dry = flat(idx); idx = idx + 1_c_int
      state%ohm%ohm_coef_lc(i)%summer_wet = flat(idx); idx = idx + 1_c_int
      state%ohm%ohm_coef_lc(i)%winter_dry = flat(idx); idx = idx + 1_c_int
      state%ohm%ohm_coef_lc(i)%winter_wet = flat(idx); idx = idx + 1_c_int
   end do

   state%soil%soildepth = flat(idx); idx = idx + 1_c_int
   state%soil%soilstorecap = flat(idx); idx = idx + 1_c_int
   state%soil%sathydraulicconduct = flat(idx); idx = idx + 1_c_int

   state%statelimit = flat(idx); idx = idx + 1_c_int
   state%irrfracwater = flat(idx); idx = idx + 1_c_int
   state%wetthresh = flat(idx); idx = idx + 1_c_int
   state%flowchange = flat(idx)

   err = SUEWS_CAPI_OK

end subroutine lc_water_prm_unpack

subroutine suews_lc_water_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_lc_water_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_lc_water_prm_error_message

end module module_c_api_lc_water_prm

module c_api_lc_water_prm_module
use module_c_api_lc_water_prm
end module c_api_lc_water_prm_module
