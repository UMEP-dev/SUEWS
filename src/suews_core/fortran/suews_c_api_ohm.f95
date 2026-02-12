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
MODULE module_c_api_ohm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char, c_null_char
   USE module_ctrl_const_allocate, ONLY: nsurf
   USE module_type_surface, ONLY: OHM_STATE

   IMPLICIT NONE

   PRIVATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OK = 0_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BAD_DT = 1_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BAD_TIME = 2_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BAD_BUFFER = 3_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BAD_STATE = 4_c_int

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_NSURF = nsurf
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
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OHM_STATE_LEN = 53_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OHM_STATE_SCHEMA_VERSION = 1_c_int

   PUBLIC :: suews_ohm_qs_calc
   PUBLIC :: suews_ohm_dqndt_step
   PUBLIC :: suews_ohm_step
   PUBLIC :: suews_ohm_state_len
   PUBLIC :: suews_ohm_state_schema_version
   PUBLIC :: suews_ohm_state_default
   PUBLIC :: suews_ohm_state_step
   PUBLIC :: suews_ohm_error_message

CONTAINS

   SUBROUTINE suews_ohm_qs_calc(qn1, dqndt, a1, a2, a3, qs, err) BIND(C, name='suews_ohm_qs_calc')
      IMPLICIT NONE

      REAL(c_double), VALUE, INTENT(in) :: qn1, dqndt, a1, a2, a3
      REAL(c_double), INTENT(out) :: qs
      INTEGER(c_int), INTENT(out) :: err

      qs = qn1*a1 + dqndt*a2 + a3
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_ohm_qs_calc


   SUBROUTINE suews_ohm_dqndt_step(dt, dt_since_start, qn1_av_prev, qn1, dqndt_prev, qn1_av_next, dqndt_next, err) &
      BIND(C, name='suews_ohm_dqndt_step')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: dt
      INTEGER(c_int), VALUE, INTENT(in) :: dt_since_start
      REAL(c_double), VALUE, INTENT(in) :: qn1_av_prev
      REAL(c_double), VALUE, INTENT(in) :: qn1
      REAL(c_double), VALUE, INTENT(in) :: dqndt_prev
      REAL(c_double), INTENT(out) :: qn1_av_next
      REAL(c_double), INTENT(out) :: dqndt_next
      INTEGER(c_int), INTENT(out) :: err

      REAL(c_double), PARAMETER :: dt0_thresh = 3600.0_c_double
      REAL(c_double), PARAMETER :: window_hr = 2.0_c_double

      INTEGER(c_int) :: dt0
      REAL(c_double) :: qn1_av_0

      qn1_av_next = -999.0_c_double
      dqndt_next = -999.0_c_double

      IF (dt <= 0_c_int) THEN
         err = SUEWS_CAPI_BAD_DT
         RETURN
      END IF

      IF (dt_since_start < 0_c_int) THEN
         err = SUEWS_CAPI_BAD_TIME
         RETURN
      END IF

      IF (REAL(dt_since_start, c_double) < dt0_thresh) THEN
         dt0 = dt_since_start + dt
      ELSE
         dt0 = INT(dt0_thresh, c_int)
      END IF

      qn1_av_0 = qn1_av_prev - dqndt_prev*(window_hr - REAL(dt, c_double)/3600.0_c_double)
      qn1_av_next = (qn1_av_prev*REAL(dt0 - dt, c_double) + qn1*REAL(dt, c_double))/REAL(dt0, c_double)
      dqndt_next = (qn1_av_next - qn1_av_0)/window_hr

      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_ohm_dqndt_step


   SUBROUTINE suews_ohm_step(dt, dt_since_start, qn1_av_prev, dqndt_prev, qn1, a1, a2, a3, &
                             qn1_av_next, dqndt_next, qs, err) BIND(C, name='suews_ohm_step')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: dt
      INTEGER(c_int), VALUE, INTENT(in) :: dt_since_start
      REAL(c_double), VALUE, INTENT(in) :: qn1_av_prev
      REAL(c_double), VALUE, INTENT(in) :: dqndt_prev
      REAL(c_double), VALUE, INTENT(in) :: qn1
      REAL(c_double), VALUE, INTENT(in) :: a1, a2, a3

      REAL(c_double), INTENT(out) :: qn1_av_next
      REAL(c_double), INTENT(out) :: dqndt_next
      REAL(c_double), INTENT(out) :: qs
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: err_local

      CALL suews_ohm_dqndt_step(dt, dt_since_start, qn1_av_prev, qn1, dqndt_prev, qn1_av_next, dqndt_next, err_local)
      IF (err_local /= SUEWS_CAPI_OK) THEN
         qs = -999.0_c_double
         err = err_local
         RETURN
      END IF

      CALL suews_ohm_qs_calc(qn1, dqndt_next, a1, a2, a3, qs, err_local)
      err = err_local

   END SUBROUTINE suews_ohm_step


   SUBROUTINE suews_ohm_state_len(n_flat, nsurf_out, err) BIND(C, name='suews_ohm_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: nsurf_out
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_OHM_STATE_LEN
      nsurf_out = SUEWS_CAPI_NSURF
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_ohm_state_len


   SUBROUTINE suews_ohm_state_schema_version(schema_version, err) BIND(C, name='suews_ohm_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_OHM_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_ohm_state_schema_version


   SUBROUTINE suews_ohm_state_default(flat, n_flat, err) BIND(C, name='suews_ohm_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(OHM_STATE) :: state

      CALL ohm_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_ohm_state_default


   SUBROUTINE suews_ohm_state_step(flat_in, n_flat_in, dt, dt_since_start, qn1, a1, a2, a3, &
                                   flat_out, n_flat_out, qs, err) BIND(C, name='suews_ohm_state_step')
      IMPLICIT NONE

      REAL(c_double), INTENT(in) :: flat_in(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat_in
      INTEGER(c_int), VALUE, INTENT(in) :: dt
      INTEGER(c_int), VALUE, INTENT(in) :: dt_since_start
      REAL(c_double), VALUE, INTENT(in) :: qn1, a1, a2, a3

      REAL(c_double), INTENT(out) :: flat_out(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat_out
      REAL(c_double), INTENT(out) :: qs
      INTEGER(c_int), INTENT(out) :: err

      TYPE(OHM_STATE) :: state
      REAL(c_double) :: qn1_av_next
      REAL(c_double) :: dqndt_next
      INTEGER(c_int) :: err_local

      CALL ohm_state_unpack(flat_in, n_flat_in, state, err_local)
      IF (err_local /= SUEWS_CAPI_OK) THEN
         qs = -999.0_c_double
         err = err_local
         RETURN
      END IF

      CALL suews_ohm_step(dt, dt_since_start, state%qn_av, state%dqndt, qn1, a1, a2, a3, qn1_av_next, dqndt_next, qs, err_local)
      IF (err_local /= SUEWS_CAPI_OK) THEN
         err = err_local
         RETURN
      END IF

      state%qn_av = qn1_av_next
      state%dqndt = dqndt_next

      CALL ohm_state_pack(state, flat_out, n_flat_out, err_local)
      err = err_local

   END SUBROUTINE suews_ohm_state_step


   SUBROUTINE ohm_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(OHM_STATE), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER :: idx
      INTEGER :: i
      INTEGER(c_int) :: n_flat_use

      n_flat_use = n_flat
      IF (n_flat_use < SUEWS_CAPI_OHM_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      flat(idx) = state%qn_av; idx = idx + 1
      flat(idx) = state%dqndt; idx = idx + 1

      DO i = 1, nsurf
         flat(idx) = state%qn_surfs(i); idx = idx + 1
      END DO
      DO i = 1, nsurf
         flat(idx) = state%dqndt_surf(i); idx = idx + 1
      END DO

      flat(idx) = state%qn_s_av; idx = idx + 1
      flat(idx) = state%dqnsdt; idx = idx + 1
      flat(idx) = state%a1; idx = idx + 1
      flat(idx) = state%a2; idx = idx + 1
      flat(idx) = state%a3; idx = idx + 1
      flat(idx) = state%t2_prev; idx = idx + 1
      flat(idx) = state%ws_rav; idx = idx + 1
      flat(idx) = state%tair_prev; idx = idx + 1

      DO i = 1, nsurf
         flat(idx) = state%qn_rav(i); idx = idx + 1
      END DO

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

      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE ohm_state_pack


   SUBROUTINE ohm_state_unpack(flat, n_flat, state, err)
      IMPLICIT NONE

      REAL(c_double), INTENT(in) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      TYPE(OHM_STATE), INTENT(out) :: state
      INTEGER(c_int), INTENT(out) :: err

      INTEGER :: idx
      INTEGER :: i
      INTEGER(c_int) :: n_flat_use

      n_flat_use = n_flat
      IF (n_flat_use < SUEWS_CAPI_OHM_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      state%qn_av = flat(idx); idx = idx + 1
      state%dqndt = flat(idx); idx = idx + 1

      DO i = 1, nsurf
         state%qn_surfs(i) = flat(idx); idx = idx + 1
      END DO
      DO i = 1, nsurf
         state%dqndt_surf(i) = flat(idx); idx = idx + 1
      END DO

      state%qn_s_av = flat(idx); idx = idx + 1
      state%dqnsdt = flat(idx); idx = idx + 1
      state%a1 = flat(idx); idx = idx + 1
      state%a2 = flat(idx); idx = idx + 1
      state%a3 = flat(idx); idx = idx + 1
      state%t2_prev = flat(idx); idx = idx + 1
      state%ws_rav = flat(idx); idx = idx + 1
      state%tair_prev = flat(idx); idx = idx + 1

      DO i = 1, nsurf
         state%qn_rav(i) = flat(idx); idx = idx + 1
      END DO

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

      state%iter_safe = flat(idx) >= 0.5_c_double

      err = SUEWS_CAPI_OK

   END SUBROUTINE ohm_state_unpack


   SUBROUTINE suews_ohm_error_message(code, buffer, buffer_len) BIND(C, name='suews_ohm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      SELECT CASE (code)
      CASE (SUEWS_CAPI_OK)
         msg = 'ok'
      CASE (SUEWS_CAPI_BAD_DT)
         msg = 'invalid timestep: dt must be positive'
      CASE (SUEWS_CAPI_BAD_TIME)
         msg = 'invalid time: dt_since_start must be non-negative'
      CASE (SUEWS_CAPI_BAD_BUFFER)
         msg = 'invalid buffer'
      CASE (SUEWS_CAPI_BAD_STATE)
         msg = 'invalid state payload'
      CASE DEFAULT
         msg = 'unknown SUEWS C API error code'
      END SELECT

      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_ohm_error_message


   SUBROUTINE copy_to_c_buffer(text, buffer, buffer_len)
      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(in) :: text
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), INTENT(in) :: buffer_len

      INTEGER :: i
      INTEGER :: ncopy
      INTEGER :: len_buf

      len_buf = INT(buffer_len)
      IF (len_buf <= 0) RETURN

      ncopy = MIN(LEN_TRIM(text), len_buf - 1)

      DO i = 1, ncopy
         buffer(i) = text(i:i)
      END DO

      buffer(ncopy + 1) = c_null_char

      DO i = ncopy + 2, len_buf
         buffer(i) = c_null_char
      END DO

   END SUBROUTINE copy_to_c_buffer

END MODULE module_c_api_ohm

! Backward compatibility alias (deprecated - will be removed in future version)
MODULE c_api_ohm_module
   USE module_c_api_ohm
END MODULE c_api_ohm_module
