! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for anthroEmis_STATE.
! -----------------------------------------------------------------------------
MODULE module_c_api_anthro_emis_state
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ANTHROEMIS_STATE_LEN = 22_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ANTHROEMIS_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: anthroemis_state_shadow
      REAL(c_double), DIMENSION(12) :: hdd_id = 0.0_c_double
      REAL(c_double) :: fc = 0.0_c_double
      REAL(c_double) :: fc_anthro = 0.0_c_double
      REAL(c_double) :: fc_biogen = 0.0_c_double
      REAL(c_double) :: fc_build = 0.0_c_double
      REAL(c_double) :: fc_metab = 0.0_c_double
      REAL(c_double) :: fc_photo = 0.0_c_double
      REAL(c_double) :: fc_point = 0.0_c_double
      REAL(c_double) :: fc_respi = 0.0_c_double
      REAL(c_double) :: fc_traff = 0.0_c_double
      LOGICAL :: iter_safe = .FALSE.
   END TYPE anthroemis_state_shadow

   PUBLIC :: suews_anthroemis_state_len
   PUBLIC :: suews_anthroemis_state_schema_version
   PUBLIC :: suews_anthroemis_state_default
   PUBLIC :: suews_anthroemis_state_error_message

CONTAINS

   SUBROUTINE suews_anthroemis_state_len(n_flat, err) BIND(C, name='suews_anthroemis_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_ANTHROEMIS_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_anthroemis_state_len


   SUBROUTINE suews_anthroemis_state_schema_version(schema_version, err) BIND(C, name='suews_anthroemis_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_ANTHROEMIS_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_anthroemis_state_schema_version


   SUBROUTINE suews_anthroemis_state_default(flat, n_flat, err) BIND(C, name='suews_anthroemis_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(anthroemis_state_shadow) :: state

      CALL anthroemis_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_anthroemis_state_default


   SUBROUTINE anthroemis_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(anthroemis_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER :: i
      INTEGER :: idx

      IF (n_flat < SUEWS_CAPI_ANTHROEMIS_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      DO i = 1, 12
         flat(idx) = state%hdd_id(i)
         idx = idx + 1
      END DO
      flat(idx) = state%fc; idx = idx + 1
      flat(idx) = state%fc_anthro; idx = idx + 1
      flat(idx) = state%fc_biogen; idx = idx + 1
      flat(idx) = state%fc_build; idx = idx + 1
      flat(idx) = state%fc_metab; idx = idx + 1
      flat(idx) = state%fc_photo; idx = idx + 1
      flat(idx) = state%fc_point; idx = idx + 1
      flat(idx) = state%fc_respi; idx = idx + 1
      flat(idx) = state%fc_traff; idx = idx + 1
      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE anthroemis_state_pack


   SUBROUTINE suews_anthroemis_state_error_message(code, buffer, buffer_len) BIND(C, name='suews_anthroemis_state_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_anthroemis_state_error_message

END MODULE module_c_api_anthro_emis_state

MODULE c_api_anthro_emis_state_module
   USE module_c_api_anthro_emis_state
END MODULE c_api_anthro_emis_state_module
