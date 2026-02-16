! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for IRRIGATION_PRM.
! -----------------------------------------------------------------------------
module module_c_api_irrigation_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_waterdist, only: IRRIGATION_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_IRRIGATION_PRM_LEN = 121_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_IRRIGATION_PRM_SCHEMA_VERSION = 1_c_int

type :: irrig_daywater_shadow
   real(c_double) :: monday_flag = 0.0_c_double
   real(c_double) :: monday_percent = 0.0_c_double
   real(c_double) :: tuesday_flag = 0.0_c_double
   real(c_double) :: tuesday_percent = 0.0_c_double
   real(c_double) :: wednesday_flag = 0.0_c_double
   real(c_double) :: wednesday_percent = 0.0_c_double
   real(c_double) :: thursday_flag = 0.0_c_double
   real(c_double) :: thursday_percent = 0.0_c_double
   real(c_double) :: friday_flag = 0.0_c_double
   real(c_double) :: friday_percent = 0.0_c_double
   real(c_double) :: saturday_flag = 0.0_c_double
   real(c_double) :: saturday_percent = 0.0_c_double
   real(c_double) :: sunday_flag = 0.0_c_double
   real(c_double) :: sunday_percent = 0.0_c_double
end type irrig_daywater_shadow

type :: irrigation_prm_shadow
   real(c_double) :: h_maintain = 0.0_c_double
   real(c_double) :: faut = 0.0_c_double
   real(c_double), dimension(3) :: ie_a = 0.0_c_double
   real(c_double), dimension(3) :: ie_m = 0.0_c_double
   integer(c_int) :: ie_start = 0_c_int
   integer(c_int) :: ie_end = 0_c_int
   real(c_double) :: internalwateruse_h = 0.0_c_double
   type(irrig_daywater_shadow) :: irr_daywater
   real(c_double), dimension(24) :: wuprofa_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: wuprofa_24hr_holiday = 0.0_c_double
   real(c_double), dimension(24) :: wuprofm_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: wuprofm_24hr_holiday = 0.0_c_double
end type irrigation_prm_shadow

public :: suews_irrigation_prm_len
public :: suews_irrigation_prm_schema_version
public :: suews_irrigation_prm_default
public :: suews_irrigation_prm_error_message
public :: irrigation_prm_unpack

contains

subroutine suews_irrigation_prm_len(n_flat, err) bind(C, name='suews_irrigation_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_IRRIGATION_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_irrigation_prm_len

subroutine suews_irrigation_prm_schema_version(schema_version, err) bind(C, name='suews_irrigation_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_IRRIGATION_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_irrigation_prm_schema_version

subroutine suews_irrigation_prm_default(flat, n_flat, err) bind(C, name='suews_irrigation_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(irrigation_prm_shadow) :: state

   call irrigation_prm_pack(state, flat, n_flat, err)

end subroutine suews_irrigation_prm_default

subroutine irrigation_prm_pack(state, flat, n_flat, err)
   implicit none

   type(irrigation_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer :: i

   if (n_flat<SUEWS_CAPI_IRRIGATION_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   flat(idx) = state%h_maintain; idx = idx + 1
   flat(idx) = state%faut; idx = idx + 1

   do i = 1, 3
      flat(idx) = state%ie_a(i)
      idx = idx + 1
   end do
   do i = 1, 3
      flat(idx) = state%ie_m(i)
      idx = idx + 1
   end do

   flat(idx) = real(state%ie_start, c_double); idx = idx + 1
   flat(idx) = real(state%ie_end, c_double); idx = idx + 1
   flat(idx) = state%internalwateruse_h; idx = idx + 1

   flat(idx) = state%irr_daywater%monday_flag; idx = idx + 1
   flat(idx) = state%irr_daywater%monday_percent; idx = idx + 1
   flat(idx) = state%irr_daywater%tuesday_flag; idx = idx + 1
   flat(idx) = state%irr_daywater%tuesday_percent; idx = idx + 1
   flat(idx) = state%irr_daywater%wednesday_flag; idx = idx + 1
   flat(idx) = state%irr_daywater%wednesday_percent; idx = idx + 1
   flat(idx) = state%irr_daywater%thursday_flag; idx = idx + 1
   flat(idx) = state%irr_daywater%thursday_percent; idx = idx + 1
   flat(idx) = state%irr_daywater%friday_flag; idx = idx + 1
   flat(idx) = state%irr_daywater%friday_percent; idx = idx + 1
   flat(idx) = state%irr_daywater%saturday_flag; idx = idx + 1
   flat(idx) = state%irr_daywater%saturday_percent; idx = idx + 1
   flat(idx) = state%irr_daywater%sunday_flag; idx = idx + 1
   flat(idx) = state%irr_daywater%sunday_percent; idx = idx + 1

   do i = 1, 24
      flat(idx) = state%wuprofa_24hr_working(i)
      idx = idx + 1
   end do
   do i = 1, 24
      flat(idx) = state%wuprofa_24hr_holiday(i)
      idx = idx + 1
   end do
   do i = 1, 24
      flat(idx) = state%wuprofm_24hr_working(i)
      idx = idx + 1
   end do
   do i = 1, 24
      flat(idx) = state%wuprofm_24hr_holiday(i)
      idx = idx + 1
   end do

   err = SUEWS_CAPI_OK

end subroutine irrigation_prm_pack

subroutine irrigation_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(IRRIGATION_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_IRRIGATION_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   state%h_maintain = flat(idx); idx = idx + 1_c_int
   state%faut = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, 3_c_int
      state%ie_a(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, 3_c_int
      state%ie_m(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%ie_start = int(nint(flat(idx))); idx = idx + 1_c_int
   state%ie_end = int(nint(flat(idx))); idx = idx + 1_c_int
   state%internalwateruse_h = flat(idx); idx = idx + 1_c_int

   state%irr_daywater%monday_flag = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%monday_percent = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%tuesday_flag = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%tuesday_percent = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%wednesday_flag = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%wednesday_percent = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%thursday_flag = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%thursday_percent = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%friday_flag = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%friday_percent = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%saturday_flag = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%saturday_percent = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%sunday_flag = flat(idx); idx = idx + 1_c_int
   state%irr_daywater%sunday_percent = flat(idx); idx = idx + 1_c_int

   do i = 0_c_int, 23_c_int
      state%wuprofa_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 0_c_int, 23_c_int
      state%wuprofa_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 0_c_int, 23_c_int
      state%wuprofm_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 0_c_int, 23_c_int
      state%wuprofm_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   err = SUEWS_CAPI_OK

end subroutine irrigation_prm_unpack

subroutine suews_irrigation_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_irrigation_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_irrigation_prm_error_message

end module module_c_api_irrigation_prm

module c_api_irrigation_prm_module
use module_c_api_irrigation_prm
end module c_api_irrigation_prm_module
