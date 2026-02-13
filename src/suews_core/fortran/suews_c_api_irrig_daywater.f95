! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for IRRIG_daywater.
! -----------------------------------------------------------------------------
MODULE module_c_api_irrig_daywater
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_IRRIG_DAYWATER_LEN = 14_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_IRRIG_DAYWATER_SCHEMA_VERSION = 1_c_int

   TYPE :: irrig_daywater_shadow
      REAL(c_double) :: monday_flag = 0.0_c_double
      REAL(c_double) :: monday_percent = 0.0_c_double
      REAL(c_double) :: tuesday_flag = 0.0_c_double
      REAL(c_double) :: tuesday_percent = 0.0_c_double
      REAL(c_double) :: wednesday_flag = 0.0_c_double
      REAL(c_double) :: wednesday_percent = 0.0_c_double
      REAL(c_double) :: thursday_flag = 0.0_c_double
      REAL(c_double) :: thursday_percent = 0.0_c_double
      REAL(c_double) :: friday_flag = 0.0_c_double
      REAL(c_double) :: friday_percent = 0.0_c_double
      REAL(c_double) :: saturday_flag = 0.0_c_double
      REAL(c_double) :: saturday_percent = 0.0_c_double
      REAL(c_double) :: sunday_flag = 0.0_c_double
      REAL(c_double) :: sunday_percent = 0.0_c_double
   END TYPE irrig_daywater_shadow

   PUBLIC :: suews_irrig_daywater_len
   PUBLIC :: suews_irrig_daywater_schema_version
   PUBLIC :: suews_irrig_daywater_default
   PUBLIC :: suews_irrig_daywater_error_message

CONTAINS

   SUBROUTINE suews_irrig_daywater_len(n_flat, err) BIND(C, name='suews_irrig_daywater_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_IRRIG_DAYWATER_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_irrig_daywater_len


   SUBROUTINE suews_irrig_daywater_schema_version(schema_version, err) BIND(C, name='suews_irrig_daywater_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_IRRIG_DAYWATER_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_irrig_daywater_schema_version


   SUBROUTINE suews_irrig_daywater_default(flat, n_flat, err) BIND(C, name='suews_irrig_daywater_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(irrig_daywater_shadow) :: state

      CALL irrig_daywater_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_irrig_daywater_default


   SUBROUTINE irrig_daywater_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(irrig_daywater_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_IRRIG_DAYWATER_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%monday_flag
      flat(2) = state%monday_percent
      flat(3) = state%tuesday_flag
      flat(4) = state%tuesday_percent
      flat(5) = state%wednesday_flag
      flat(6) = state%wednesday_percent
      flat(7) = state%thursday_flag
      flat(8) = state%thursday_percent
      flat(9) = state%friday_flag
      flat(10) = state%friday_percent
      flat(11) = state%saturday_flag
      flat(12) = state%saturday_percent
      flat(13) = state%sunday_flag
      flat(14) = state%sunday_percent

      err = SUEWS_CAPI_OK

   END SUBROUTINE irrig_daywater_pack


   SUBROUTINE suews_irrig_daywater_error_message(code, buffer, buffer_len) BIND(C, name='suews_irrig_daywater_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_irrig_daywater_error_message

END MODULE module_c_api_irrig_daywater

MODULE c_api_irrig_daywater_module
   USE module_c_api_irrig_daywater
END MODULE c_api_irrig_daywater_module
