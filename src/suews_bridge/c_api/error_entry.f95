! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for error_entry.
! -----------------------------------------------------------------------------
MODULE module_c_api_error_entry
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ERROR_ENTRY_TIMER_LEN = 18_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ERROR_ENTRY_MESSAGE_LEN = 256_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ERROR_ENTRY_LOCATION_LEN = 64_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ERROR_ENTRY_SCHEMA_VERSION = 1_c_int

   TYPE :: error_entry_shadow
      REAL(c_double), DIMENSION(SUEWS_CAPI_ERROR_ENTRY_TIMER_LEN) :: timer_flat = 0.0_c_double
      CHARACTER(LEN=SUEWS_CAPI_ERROR_ENTRY_MESSAGE_LEN) :: message = ''
      CHARACTER(LEN=SUEWS_CAPI_ERROR_ENTRY_LOCATION_LEN) :: location = ''
      LOGICAL :: is_fatal = .FALSE.
   END TYPE error_entry_shadow

   PUBLIC :: suews_error_entry_len
   PUBLIC :: suews_error_entry_schema_version
   PUBLIC :: suews_error_entry_default
   PUBLIC :: suews_error_entry_error_message

CONTAINS

   INTEGER(c_int) FUNCTION c_bool(value)
      IMPLICIT NONE
      LOGICAL, INTENT(in) :: value

      IF (value) THEN
         c_bool = 1_c_int
      ELSE
         c_bool = 0_c_int
      END IF

   END FUNCTION c_bool


   SUBROUTINE suews_error_entry_len(timer_flat_len, message_len, location_len, err) BIND(C, name='suews_error_entry_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: timer_flat_len
      INTEGER(c_int), INTENT(out) :: message_len
      INTEGER(c_int), INTENT(out) :: location_len
      INTEGER(c_int), INTENT(out) :: err

      timer_flat_len = SUEWS_CAPI_ERROR_ENTRY_TIMER_LEN
      message_len = SUEWS_CAPI_ERROR_ENTRY_MESSAGE_LEN
      location_len = SUEWS_CAPI_ERROR_ENTRY_LOCATION_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_error_entry_len


   SUBROUTINE suews_error_entry_schema_version(schema_version, err) BIND(C, name='suews_error_entry_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_ERROR_ENTRY_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_error_entry_schema_version


   SUBROUTINE suews_error_entry_default(timer_flat, n_timer_flat, message, message_len, location, location_len, is_fatal, err) BIND(C, name='suews_error_entry_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: timer_flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_timer_flat
      CHARACTER(c_char), INTENT(out) :: message(*)
      INTEGER(c_int), VALUE, INTENT(in) :: message_len
      CHARACTER(c_char), INTENT(out) :: location(*)
      INTEGER(c_int), VALUE, INTENT(in) :: location_len
      INTEGER(c_int), INTENT(out) :: is_fatal
      INTEGER(c_int), INTENT(out) :: err

      TYPE(error_entry_shadow) :: state

      CALL error_entry_pack(state, timer_flat, n_timer_flat, message, message_len, location, location_len, is_fatal, err)

   END SUBROUTINE suews_error_entry_default


   SUBROUTINE error_entry_pack(state, timer_flat, n_timer_flat, message, message_len, location, location_len, is_fatal, err)
      IMPLICIT NONE

      TYPE(error_entry_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: timer_flat(*)
      INTEGER(c_int), INTENT(in) :: n_timer_flat
      CHARACTER(c_char), INTENT(out) :: message(*)
      INTEGER(c_int), INTENT(in) :: message_len
      CHARACTER(c_char), INTENT(out) :: location(*)
      INTEGER(c_int), INTENT(in) :: location_len
      INTEGER(c_int), INTENT(out) :: is_fatal
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: i

      IF (n_timer_flat < SUEWS_CAPI_ERROR_ENTRY_TIMER_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      DO i = 1_c_int, SUEWS_CAPI_ERROR_ENTRY_TIMER_LEN
         timer_flat(i) = state%timer_flat(i)
      END DO

      CALL copy_to_c_buffer(state%message, message, message_len)
      CALL copy_to_c_buffer(state%location, location, location_len)
      is_fatal = c_bool(state%is_fatal)

      err = SUEWS_CAPI_OK

   END SUBROUTINE error_entry_pack


   SUBROUTINE suews_error_entry_error_message(code, buffer, buffer_len) BIND(C, name='suews_error_entry_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_error_entry_error_message

END MODULE module_c_api_error_entry

MODULE c_api_error_entry_module
   USE module_c_api_error_entry
END MODULE c_api_error_entry_module
