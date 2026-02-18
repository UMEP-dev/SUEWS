! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SNOW_PRM.
! -----------------------------------------------------------------------------
module module_c_api_snow_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_snow, only: SNOW_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_SNOW_PRM_LEN = 71_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_SNOW_PRM_SCHEMA_VERSION = 1_c_int

type :: snow_prm_shadow
   real(c_double) :: crwmax = 0.0_c_double
   real(c_double) :: crwmin = 0.0_c_double
   real(c_double) :: narp_emis_snow = 0.0_c_double
   real(c_double) :: preciplimit = 0.0_c_double
   real(c_double) :: preciplimitalb = 0.0_c_double
   real(c_double) :: snowalbmax = 0.0_c_double
   real(c_double) :: snowalbmin = 0.0_c_double
   real(c_double) :: snowdensmax = 0.0_c_double
   real(c_double) :: snowdensmin = 0.0_c_double
   real(c_double) :: snowlimbldg = 0.0_c_double
   real(c_double) :: snowlimpaved = 0.0_c_double
   real(c_double), dimension(7) :: snowpacklimit = 0.0_c_double
   real(c_double), dimension(24) :: snowprof_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: snowprof_24hr_holiday = 0.0_c_double
   real(c_double) :: tau_a = 0.0_c_double
   real(c_double) :: tau_f = 0.0_c_double
   real(c_double) :: tau_r = 0.0_c_double
   real(c_double) :: tempmeltfact = 0.0_c_double
   real(c_double) :: radmeltfact = 0.0_c_double
end type snow_prm_shadow

public :: suews_snow_prm_len
public :: suews_snow_prm_schema_version
public :: suews_snow_prm_default
public :: suews_snow_prm_error_message
public :: snow_prm_unpack

contains

subroutine suews_snow_prm_len(n_flat, err) bind(C, name='suews_snow_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_SNOW_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_snow_prm_len

subroutine suews_snow_prm_schema_version(schema_version, err) bind(C, name='suews_snow_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_SNOW_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_snow_prm_schema_version

subroutine suews_snow_prm_default(flat, n_flat, err) bind(C, name='suews_snow_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(snow_prm_shadow) :: state

   call snow_prm_pack(state, flat, n_flat, err)

end subroutine suews_snow_prm_default

subroutine snow_prm_pack(state, flat, n_flat, err)
   implicit none

   type(snow_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer :: i

   if (n_flat<SUEWS_CAPI_SNOW_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   flat(idx) = state%crwmax; idx = idx + 1
   flat(idx) = state%crwmin; idx = idx + 1
   flat(idx) = state%narp_emis_snow; idx = idx + 1
   flat(idx) = state%preciplimit; idx = idx + 1
   flat(idx) = state%preciplimitalb; idx = idx + 1
   flat(idx) = state%snowalbmax; idx = idx + 1
   flat(idx) = state%snowalbmin; idx = idx + 1
   flat(idx) = state%snowdensmax; idx = idx + 1
   flat(idx) = state%snowdensmin; idx = idx + 1
   flat(idx) = state%snowlimbldg; idx = idx + 1
   flat(idx) = state%snowlimpaved; idx = idx + 1

   do i = 1, 7
      flat(idx) = state%snowpacklimit(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%snowprof_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%snowprof_24hr_holiday(i)
      idx = idx + 1
   end do

   flat(idx) = state%tau_a; idx = idx + 1
   flat(idx) = state%tau_f; idx = idx + 1
   flat(idx) = state%tau_r; idx = idx + 1
   flat(idx) = state%tempmeltfact; idx = idx + 1
   flat(idx) = state%radmeltfact

   err = SUEWS_CAPI_OK

end subroutine snow_prm_pack

subroutine snow_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(SNOW_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_SNOW_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   state%crwmax = flat(idx); idx = idx + 1_c_int
   state%crwmin = flat(idx); idx = idx + 1_c_int
   state%narp_emis_snow = flat(idx); idx = idx + 1_c_int
   state%preciplimit = flat(idx); idx = idx + 1_c_int
   state%preciplimitalb = flat(idx); idx = idx + 1_c_int
   state%snowalbmax = flat(idx); idx = idx + 1_c_int
   state%snowalbmin = flat(idx); idx = idx + 1_c_int
   state%snowdensmax = flat(idx); idx = idx + 1_c_int
   state%snowdensmin = flat(idx); idx = idx + 1_c_int
   state%snowlimbldg = flat(idx); idx = idx + 1_c_int
   state%snowlimpaved = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, 7_c_int
      state%snowpacklimit(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%snowprof_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%snowprof_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%tau_a = flat(idx); idx = idx + 1_c_int
   state%tau_f = flat(idx); idx = idx + 1_c_int
   state%tau_r = flat(idx); idx = idx + 1_c_int
   state%tempmeltfact = flat(idx); idx = idx + 1_c_int
   state%radmeltfact = flat(idx)

   err = SUEWS_CAPI_OK

end subroutine snow_prm_unpack

subroutine suews_snow_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_snow_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_snow_prm_error_message

end module module_c_api_snow_prm

module c_api_snow_prm_module
use module_c_api_snow_prm
end module c_api_snow_prm_module
