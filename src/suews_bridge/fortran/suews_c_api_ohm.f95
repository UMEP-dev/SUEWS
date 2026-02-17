! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade (OHM MVP).
!
! Why this separate file exists:
! 1. Keep original SUEWS physics modules untouched.
! 2. Expose a stable C ABI entry-point for Rust/Python without f90wrap.
! 3. Limit bridge scope to OHM-focused kernels during MVP.
!
! Design guardrails:
! - No new physical parameterisation is introduced here.
! - Scalar formulas mirror `module_phys_ohm` kernels (`OHM_dqndt_cal_X`, `OHM_QS_cal`).
! - Physics changes must continue to be made in `module_phys_ohm` first.
!
! Bridge extension in this revision:
! - Add flatten/unflatten for `OHM_STATE` so Rust/Python can handle class-like state.
! -----------------------------------------------------------------------------
module module_c_api_ohm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_DT, SUEWS_CAPI_BAD_TIME, &
   SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_ctrl_const_allocate, only: nsurf
use module_type_surface, only: OHM_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_DT
public :: SUEWS_CAPI_BAD_TIME
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_NSURF = nsurf
! Flat layout length for OHM_STATE bridge payload:
! 2 (qn_av,dqndt)
! + 7 (qn_surfs)
! + 7 (dqndt_surf)
! + 2 (qn_s_av,dqnsdt)
! + 3 (a1,a2,a3)
! + 3 (t2_prev,ws_rav,tair_prev)
! + 7 (qn_rav)
! + 7 (dyn_a1, surface order: paved,bldg,evetr,dectr,grass,bsoil,water)
! + 7 (dyn_a2, same order)
! + 7 (dyn_a3, same order)
! + 1 (iter_safe as 0/1)
integer(c_int), parameter, public :: SUEWS_CAPI_OHM_STATE_LEN = 53_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_OHM_STATE_SCHEMA_VERSION = 1_c_int

public :: suews_ohm_qs_calc
public :: suews_ohm_dqndt_step
public :: suews_ohm_step
public :: suews_ohm_state_len
public :: suews_ohm_state_schema_version
public :: suews_ohm_state_default
public :: suews_ohm_state_step
public :: suews_ohm_error_message
public :: ohm_state_unpack

contains

subroutine suews_ohm_qs_calc(qn1, dqndt, a1, a2, a3, qs, err) bind(C, name='suews_ohm_qs_calc')
   implicit none

   real(c_double), value, intent(in) :: qn1, dqndt, a1, a2, a3
   real(c_double), intent(out) :: qs
   integer(c_int), intent(out) :: err

   qs = qn1 * a1 + dqndt * a2 + a3
   err = SUEWS_CAPI_OK

end subroutine suews_ohm_qs_calc

subroutine suews_ohm_dqndt_step(dt, dt_since_start, qn1_av_prev, qn1, dqndt_prev, qn1_av_next, dqndt_next, err) &
   bind(C, name='suews_ohm_dqndt_step')
   implicit none

   integer(c_int), value, intent(in) :: dt
   integer(c_int), value, intent(in) :: dt_since_start
   real(c_double), value, intent(in) :: qn1_av_prev
   real(c_double), value, intent(in) :: qn1
   real(c_double), value, intent(in) :: dqndt_prev
   real(c_double), intent(out) :: qn1_av_next
   real(c_double), intent(out) :: dqndt_next
   integer(c_int), intent(out) :: err

   real(c_double), parameter :: dt0_thresh = 3600.0_c_double
   real(c_double), parameter :: window_hr = 2.0_c_double

   integer(c_int) :: dt0
   real(c_double) :: qn1_av_0

   qn1_av_next = -999.0_c_double
   dqndt_next = -999.0_c_double

   if (dt<=0_c_int) then
      err = SUEWS_CAPI_BAD_DT
      return
   end if

   if (dt_since_start<0_c_int) then
      err = SUEWS_CAPI_BAD_TIME
      return
   end if

   if (real(dt_since_start, c_double)<dt0_thresh) then
      dt0 = dt_since_start + dt
   else
      dt0 = int(dt0_thresh, c_int)
   end if

   qn1_av_0 = qn1_av_prev - dqndt_prev * (window_hr - real(dt, c_double) / 3600.0_c_double)
   qn1_av_next = (qn1_av_prev * real(dt0 - dt, c_double) + qn1 * real(dt, c_double)) / real(dt0, c_double)
   dqndt_next = (qn1_av_next - qn1_av_0) / window_hr

   err = SUEWS_CAPI_OK

end subroutine suews_ohm_dqndt_step

subroutine suews_ohm_step(dt, dt_since_start, qn1_av_prev, dqndt_prev, qn1, a1, a2, a3, &
                          qn1_av_next, dqndt_next, qs, err) bind(C, name='suews_ohm_step')
   implicit none

   integer(c_int), value, intent(in) :: dt
   integer(c_int), value, intent(in) :: dt_since_start
   real(c_double), value, intent(in) :: qn1_av_prev
   real(c_double), value, intent(in) :: dqndt_prev
   real(c_double), value, intent(in) :: qn1
   real(c_double), value, intent(in) :: a1, a2, a3

   real(c_double), intent(out) :: qn1_av_next
   real(c_double), intent(out) :: dqndt_next
   real(c_double), intent(out) :: qs
   integer(c_int), intent(out) :: err

   integer(c_int) :: err_local

   call suews_ohm_dqndt_step(dt, dt_since_start, qn1_av_prev, qn1, dqndt_prev, qn1_av_next, dqndt_next, err_local)
   if (err_local/=SUEWS_CAPI_OK) then
      qs = -999.0_c_double
      err = err_local
      return
   end if

   call suews_ohm_qs_calc(qn1, dqndt_next, a1, a2, a3, qs, err_local)
   err = err_local

end subroutine suews_ohm_step

subroutine suews_ohm_state_len(n_flat, nsurf_out, err) bind(C, name='suews_ohm_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: nsurf_out
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_OHM_STATE_LEN
   nsurf_out = SUEWS_CAPI_NSURF
   err = SUEWS_CAPI_OK

end subroutine suews_ohm_state_len

subroutine suews_ohm_state_schema_version(schema_version, err) bind(C, name='suews_ohm_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_OHM_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_ohm_state_schema_version

subroutine suews_ohm_state_default(flat, n_flat, err) bind(C, name='suews_ohm_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(OHM_STATE) :: state

   call ohm_state_pack(state, flat, n_flat, err)

end subroutine suews_ohm_state_default

subroutine suews_ohm_state_step(flat_in, n_flat_in, dt, dt_since_start, qn1, a1, a2, a3, &
                                flat_out, n_flat_out, qs, err) bind(C, name='suews_ohm_state_step')
   implicit none

   real(c_double), intent(in) :: flat_in(*)
   integer(c_int), value, intent(in) :: n_flat_in
   integer(c_int), value, intent(in) :: dt
   integer(c_int), value, intent(in) :: dt_since_start
   real(c_double), value, intent(in) :: qn1, a1, a2, a3

   real(c_double), intent(out) :: flat_out(*)
   integer(c_int), value, intent(in) :: n_flat_out
   real(c_double), intent(out) :: qs
   integer(c_int), intent(out) :: err

   type(OHM_STATE) :: state
   real(c_double) :: qn1_av_next
   real(c_double) :: dqndt_next
   integer(c_int) :: err_local

   call ohm_state_unpack(flat_in, n_flat_in, state, err_local)
   if (err_local/=SUEWS_CAPI_OK) then
      qs = -999.0_c_double
      err = err_local
      return
   end if

   call suews_ohm_step(dt, dt_since_start, state%qn_av, state%dqndt, qn1, a1, a2, a3, qn1_av_next, dqndt_next, qs, err_local)
   if (err_local/=SUEWS_CAPI_OK) then
      err = err_local
      return
   end if

   state%qn_av = qn1_av_next
   state%dqndt = dqndt_next

   call ohm_state_pack(state, flat_out, n_flat_out, err_local)
   err = err_local

end subroutine suews_ohm_state_step

subroutine ohm_state_pack(state, flat, n_flat, err)
   implicit none

   type(OHM_STATE), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer :: idx
   integer :: i
   integer(c_int) :: n_flat_use

   n_flat_use = n_flat
   if (n_flat_use<SUEWS_CAPI_OHM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   flat(idx) = state%qn_av; idx = idx + 1
   flat(idx) = state%dqndt; idx = idx + 1

   do i = 1, nsurf
      flat(idx) = state%qn_surfs(i); idx = idx + 1
   end do
   do i = 1, nsurf
      flat(idx) = state%dqndt_surf(i); idx = idx + 1
   end do

   flat(idx) = state%qn_s_av; idx = idx + 1
   flat(idx) = state%dqnsdt; idx = idx + 1
   flat(idx) = state%a1; idx = idx + 1
   flat(idx) = state%a2; idx = idx + 1
   flat(idx) = state%a3; idx = idx + 1
   flat(idx) = state%t2_prev; idx = idx + 1
   flat(idx) = state%ws_rav; idx = idx + 1
   flat(idx) = state%tair_prev; idx = idx + 1

   do i = 1, nsurf
      flat(idx) = state%qn_rav(i); idx = idx + 1
   end do

   flat(idx) = state%a1_paved; idx = idx + 1
   flat(idx) = state%a1_bldg; idx = idx + 1
   flat(idx) = state%a1_evetr; idx = idx + 1
   flat(idx) = state%a1_dectr; idx = idx + 1
   flat(idx) = state%a1_grass; idx = idx + 1
   flat(idx) = state%a1_bsoil; idx = idx + 1
   flat(idx) = state%a1_water; idx = idx + 1

   flat(idx) = state%a2_paved; idx = idx + 1
   flat(idx) = state%a2_bldg; idx = idx + 1
   flat(idx) = state%a2_evetr; idx = idx + 1
   flat(idx) = state%a2_dectr; idx = idx + 1
   flat(idx) = state%a2_grass; idx = idx + 1
   flat(idx) = state%a2_bsoil; idx = idx + 1
   flat(idx) = state%a2_water; idx = idx + 1

   flat(idx) = state%a3_paved; idx = idx + 1
   flat(idx) = state%a3_bldg; idx = idx + 1
   flat(idx) = state%a3_evetr; idx = idx + 1
   flat(idx) = state%a3_dectr; idx = idx + 1
   flat(idx) = state%a3_grass; idx = idx + 1
   flat(idx) = state%a3_bsoil; idx = idx + 1
   flat(idx) = state%a3_water; idx = idx + 1

   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine ohm_state_pack

subroutine ohm_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(OHM_STATE), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer :: idx
   integer :: i
   integer(c_int) :: n_flat_use

   n_flat_use = n_flat
   if (n_flat_use<SUEWS_CAPI_OHM_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   state%qn_av = flat(idx); idx = idx + 1
   state%dqndt = flat(idx); idx = idx + 1

   do i = 1, nsurf
      state%qn_surfs(i) = flat(idx); idx = idx + 1
   end do
   do i = 1, nsurf
      state%dqndt_surf(i) = flat(idx); idx = idx + 1
   end do

   state%qn_s_av = flat(idx); idx = idx + 1
   state%dqnsdt = flat(idx); idx = idx + 1
   state%a1 = flat(idx); idx = idx + 1
   state%a2 = flat(idx); idx = idx + 1
   state%a3 = flat(idx); idx = idx + 1
   state%t2_prev = flat(idx); idx = idx + 1
   state%ws_rav = flat(idx); idx = idx + 1
   state%tair_prev = flat(idx); idx = idx + 1

   do i = 1, nsurf
      state%qn_rav(i) = flat(idx); idx = idx + 1
   end do

   state%a1_paved = flat(idx); idx = idx + 1
   state%a1_bldg = flat(idx); idx = idx + 1
   state%a1_evetr = flat(idx); idx = idx + 1
   state%a1_dectr = flat(idx); idx = idx + 1
   state%a1_grass = flat(idx); idx = idx + 1
   state%a1_bsoil = flat(idx); idx = idx + 1
   state%a1_water = flat(idx); idx = idx + 1

   state%a2_paved = flat(idx); idx = idx + 1
   state%a2_bldg = flat(idx); idx = idx + 1
   state%a2_evetr = flat(idx); idx = idx + 1
   state%a2_dectr = flat(idx); idx = idx + 1
   state%a2_grass = flat(idx); idx = idx + 1
   state%a2_bsoil = flat(idx); idx = idx + 1
   state%a2_water = flat(idx); idx = idx + 1

   state%a3_paved = flat(idx); idx = idx + 1
   state%a3_bldg = flat(idx); idx = idx + 1
   state%a3_evetr = flat(idx); idx = idx + 1
   state%a3_dectr = flat(idx); idx = idx + 1
   state%a3_grass = flat(idx); idx = idx + 1
   state%a3_bsoil = flat(idx); idx = idx + 1
   state%a3_water = flat(idx); idx = idx + 1

   state%iter_safe = flat(idx)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine ohm_state_unpack

subroutine suews_ohm_error_message(code, buffer, buffer_len) bind(C, name='suews_ohm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)

   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_ohm_error_message

end module module_c_api_ohm

! Backward compatibility alias (deprecated - will be removed in future version)
module c_api_ohm_module
use module_c_api_ohm
end module c_api_ohm_module
