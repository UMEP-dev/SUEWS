! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for NHOOD_STATE.
! -----------------------------------------------------------------------------
MODULE module_c_api_nhood
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_NHOOD_STATE_LEN = 5_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_NHOOD_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: nhood_state_shadow
      REAL(c_double) :: u_hbh_1dravg = 0.0_c_double
      REAL(c_double) :: qn_1dravg = 0.0_c_double
      REAL(c_double) :: tair_mn_prev = 0.0_c_double
      REAL(c_double) :: iter_count = 0.0_c_double
      LOGICAL :: iter_safe = .FALSE.
   END TYPE nhood_state_shadow

   PUBLIC :: suews_nhood_state_len
   PUBLIC :: suews_nhood_state_schema_version
   PUBLIC :: suews_nhood_state_default
   PUBLIC :: suews_nhood_error_message

CONTAINS

   SUBROUTINE suews_nhood_state_len(n_flat, err) BIND(C, name='suews_nhood_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_NHOOD_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_nhood_state_len


   SUBROUTINE suews_nhood_state_schema_version(schema_version, err) BIND(C, name='suews_nhood_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_NHOOD_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_nhood_state_schema_version


   SUBROUTINE suews_nhood_state_default(flat, n_flat, err) BIND(C, name='suews_nhood_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(nhood_state_shadow) :: state

      CALL nhood_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_nhood_state_default


   SUBROUTINE nhood_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(nhood_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_NHOOD_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%u_hbh_1dravg
      flat(2) = state%qn_1dravg
      flat(3) = state%tair_mn_prev
      flat(4) = state%iter_count
      flat(5) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE nhood_state_pack


   SUBROUTINE suews_nhood_error_message(code, buffer, buffer_len) BIND(C, name='suews_nhood_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_nhood_error_message

END MODULE module_c_api_nhood

MODULE c_api_nhood_module
   USE module_c_api_nhood
END MODULE c_api_nhood_module
