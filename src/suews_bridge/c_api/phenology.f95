! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for PHENOLOGY_STATE.
! -----------------------------------------------------------------------------
module module_c_api_phenology
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_ctrl_const_allocate, only: nsurf, nvegsurf
use module_type_vegetation, only: PHENOLOGY_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_PHENOLOGY_STATE_LEN = 76_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_PHENOLOGY_STATE_SCHEMA_VERSION = 1_c_int

type :: phenology_state_shadow
   real(c_double), dimension(nsurf) :: alb = 0.0_c_double
   real(c_double), dimension(nvegsurf) :: lai_id = 0.0_c_double
   real(c_double), dimension(nvegsurf) :: gdd_id = 0.0_c_double
   real(c_double), dimension(nvegsurf) :: sdd_id = 0.0_c_double
   real(c_double) :: vegphenlumps = 0.0_c_double
   real(c_double) :: porosity_id = 0.0_c_double
   real(c_double) :: decidcap_id = 0.0_c_double
   real(c_double) :: albdectr_id = 0.0_c_double
   real(c_double) :: albevetr_id = 0.0_c_double
   real(c_double) :: albgrass_id = 0.0_c_double
   real(c_double) :: tmin_id = 0.0_c_double
   real(c_double) :: tmax_id = 0.0_c_double
   real(c_double) :: lenday_id = 0.0_c_double
   real(c_double) :: tempveg = 0.0_c_double
   real(c_double), dimension(6, nsurf) :: storedrainprm = 0.0_c_double
   real(c_double) :: gfunc = 0.0_c_double
   real(c_double) :: gsc = 0.0_c_double
   real(c_double) :: g_kdown = 0.0_c_double
   real(c_double) :: g_dq = 0.0_c_double
   real(c_double) :: g_ta = 0.0_c_double
   real(c_double) :: g_smd = 0.0_c_double
   real(c_double) :: g_lai = 0.0_c_double
   logical :: iter_safe = .false.
end type phenology_state_shadow

public :: suews_phenology_state_len
public :: suews_phenology_state_schema_version
public :: suews_phenology_state_default
public :: suews_phenology_error_message
public :: phenology_state_unpack

contains

subroutine suews_phenology_state_len(n_flat, err) bind(C, name='suews_phenology_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_PHENOLOGY_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_phenology_state_len

subroutine suews_phenology_state_schema_version(schema_version, err) bind(C, name='suews_phenology_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_PHENOLOGY_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_phenology_state_schema_version

subroutine suews_phenology_state_default(flat, n_flat, err) bind(C, name='suews_phenology_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(phenology_state_shadow) :: state

   call phenology_state_pack(state, flat, n_flat, err)

end subroutine suews_phenology_state_default

subroutine phenology_state_pack(state, flat, n_flat, err)
   implicit none

   type(phenology_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer :: idx
   integer :: i
   integer :: j

   if (n_flat<SUEWS_CAPI_PHENOLOGY_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1

   do i = 1, nsurf
      flat(idx) = state%alb(i)
      idx = idx + 1
   end do

   do i = 1, nvegsurf
      flat(idx) = state%lai_id(i)
      idx = idx + 1
   end do

   do i = 1, nvegsurf
      flat(idx) = state%gdd_id(i)
      idx = idx + 1
   end do

   do i = 1, nvegsurf
      flat(idx) = state%sdd_id(i)
      idx = idx + 1
   end do

   flat(idx) = state%vegphenlumps; idx = idx + 1
   flat(idx) = state%porosity_id; idx = idx + 1
   flat(idx) = state%decidcap_id; idx = idx + 1
   flat(idx) = state%albdectr_id; idx = idx + 1
   flat(idx) = state%albevetr_id; idx = idx + 1
   flat(idx) = state%albgrass_id; idx = idx + 1
   flat(idx) = state%tmin_id; idx = idx + 1
   flat(idx) = state%tmax_id; idx = idx + 1
   flat(idx) = state%lenday_id; idx = idx + 1
   flat(idx) = state%tempveg; idx = idx + 1

   do j = 1, nsurf
      do i = 1, 6
         flat(idx) = state%storedrainprm(i, j)
         idx = idx + 1
      end do
   end do

   flat(idx) = state%gfunc; idx = idx + 1
   flat(idx) = state%gsc; idx = idx + 1
   flat(idx) = state%g_kdown; idx = idx + 1
   flat(idx) = state%g_dq; idx = idx + 1
   flat(idx) = state%g_ta; idx = idx + 1
   flat(idx) = state%g_smd; idx = idx + 1
   flat(idx) = state%g_lai; idx = idx + 1
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine phenology_state_pack

subroutine phenology_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(PHENOLOGY_STATE), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: j

   if (n_flat<SUEWS_CAPI_PHENOLOGY_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   do i = 1_c_int, int(nsurf, c_int)
      state%alb(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nvegsurf, c_int)
      state%lai_id(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nvegsurf, c_int)
      state%gdd_id(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nvegsurf, c_int)
      state%sdd_id(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%vegphenlumps = flat(idx); idx = idx + 1_c_int
   state%porosity_id = flat(idx); idx = idx + 1_c_int
   state%decidcap_id = flat(idx); idx = idx + 1_c_int
   state%albdectr_id = flat(idx); idx = idx + 1_c_int
   state%albevetr_id = flat(idx); idx = idx + 1_c_int
   state%albgrass_id = flat(idx); idx = idx + 1_c_int
   state%tmin_id = flat(idx); idx = idx + 1_c_int
   state%tmax_id = flat(idx); idx = idx + 1_c_int
   state%lenday_id = flat(idx); idx = idx + 1_c_int
   state%tempveg = flat(idx); idx = idx + 1_c_int

   do j = 1_c_int, int(nsurf, c_int)
      do i = 1_c_int, 6_c_int
         state%storedrainprm(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

   state%gfunc = flat(idx); idx = idx + 1_c_int
   state%gsc = flat(idx); idx = idx + 1_c_int
   state%g_kdown = flat(idx); idx = idx + 1_c_int
   state%g_dq = flat(idx); idx = idx + 1_c_int
   state%g_ta = flat(idx); idx = idx + 1_c_int
   state%g_smd = flat(idx); idx = idx + 1_c_int
   state%g_lai = flat(idx); idx = idx + 1_c_int
   state%iter_safe = flat(idx)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine phenology_state_unpack

subroutine suews_phenology_error_message(code, buffer, buffer_len) bind(C, name='suews_phenology_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_phenology_error_message

end module module_c_api_phenology

module c_api_phenology_module
use module_c_api_phenology
end module c_api_phenology_module
