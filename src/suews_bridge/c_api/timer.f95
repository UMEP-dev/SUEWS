! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SUEWS_TIMER.
! -----------------------------------------------------------------------------
MODULE module_c_api_timer
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_TIMER_LEN = 18_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_TIMER_SCHEMA_VERSION = 1_c_int

   TYPE :: suews_timer_shadow
      INTEGER :: id = 0
      INTEGER :: imin = 0
      INTEGER :: isec = 0
      INTEGER :: it = 0
      INTEGER :: iy = 0
      INTEGER :: tstep = 0
      INTEGER :: tstep_prev = 0
      INTEGER :: dt_since_start = 0
      INTEGER :: dt_since_start_prev = 0
      INTEGER :: nsh = 0
      REAL(c_double) :: nsh_real = 0.0_c_double
      REAL(c_double) :: tstep_real = 0.0_c_double
      REAL(c_double) :: dectime = 0.0_c_double
      INTEGER, DIMENSION(3) :: dayofweek_id = 0
      INTEGER :: dls = 0
      INTEGER :: new_day = 0
   END TYPE suews_timer_shadow

   PUBLIC :: suews_timer_len
   PUBLIC :: suews_timer_schema_version
   PUBLIC :: suews_timer_default
   PUBLIC :: suews_timer_error_message

CONTAINS

   SUBROUTINE suews_timer_len(n_flat, err) BIND(C, name='suews_timer_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_TIMER_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_timer_len


   SUBROUTINE suews_timer_schema_version(schema_version, err) BIND(C, name='suews_timer_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_TIMER_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_timer_schema_version


   SUBROUTINE suews_timer_default(flat, n_flat, err) BIND(C, name='suews_timer_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(suews_timer_shadow) :: state

      CALL suews_timer_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_timer_default


   SUBROUTINE suews_timer_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(suews_timer_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_TIMER_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = REAL(state%id, c_double)
      flat(2) = REAL(state%imin, c_double)
      flat(3) = REAL(state%isec, c_double)
      flat(4) = REAL(state%it, c_double)
      flat(5) = REAL(state%iy, c_double)
      flat(6) = REAL(state%tstep, c_double)
      flat(7) = REAL(state%tstep_prev, c_double)
      flat(8) = REAL(state%dt_since_start, c_double)
      flat(9) = REAL(state%dt_since_start_prev, c_double)
      flat(10) = REAL(state%nsh, c_double)
      flat(11) = state%nsh_real
      flat(12) = state%tstep_real
      flat(13) = state%dectime
      flat(14) = REAL(state%dayofweek_id(1), c_double)
      flat(15) = REAL(state%dayofweek_id(2), c_double)
      flat(16) = REAL(state%dayofweek_id(3), c_double)
      flat(17) = REAL(state%dls, c_double)
      flat(18) = REAL(state%new_day, c_double)

      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_timer_pack


   SUBROUTINE suews_timer_error_message(code, buffer, buffer_len) BIND(C, name='suews_timer_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_timer_error_message

END MODULE module_c_api_timer

MODULE c_api_timer_module
   USE module_c_api_timer
END MODULE c_api_timer_module
