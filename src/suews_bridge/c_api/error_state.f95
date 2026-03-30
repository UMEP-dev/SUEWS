! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for error_state.
! -----------------------------------------------------------------------------
MODULE module_c_api_error_state
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ERROR_STATE_MESSAGE_LEN = 512_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ERROR_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: error_state_shadow
      LOGICAL :: flag = .FALSE.
      INTEGER :: code = 0
      CHARACTER(LEN=SUEWS_CAPI_ERROR_STATE_MESSAGE_LEN) :: message = ''
      LOGICAL :: has_fatal = .FALSE.
      INTEGER :: count = 0
   END TYPE error_state_shadow

   PUBLIC :: suews_error_state_len
   PUBLIC :: suews_error_state_schema_version
   PUBLIC :: suews_error_state_default
   PUBLIC :: suews_error_state_error_message

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


   SUBROUTINE suews_error_state_len(message_len, err) BIND(C, name='suews_error_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: message_len
      INTEGER(c_int), INTENT(out) :: err

      message_len = SUEWS_CAPI_ERROR_STATE_MESSAGE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_error_state_len


   SUBROUTINE suews_error_state_schema_version(schema_version, err) BIND(C, name='suews_error_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_ERROR_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_error_state_schema_version


   SUBROUTINE suews_error_state_default(flag, code, message, message_len, has_fatal, count, err) BIND(C, name='suews_error_state_default')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: flag
      INTEGER(c_int), INTENT(out) :: code
      CHARACTER(c_char), INTENT(out) :: message(*)
      INTEGER(c_int), VALUE, INTENT(in) :: message_len
      INTEGER(c_int), INTENT(out) :: has_fatal
      INTEGER(c_int), INTENT(out) :: count
      INTEGER(c_int), INTENT(out) :: err

      TYPE(error_state_shadow) :: state

      CALL error_state_pack(state, flag, code, message, message_len, has_fatal, count, err)

   END SUBROUTINE suews_error_state_default


   SUBROUTINE error_state_pack(state, flag, code, message, message_len, has_fatal, count, err)
      IMPLICIT NONE

      TYPE(error_state_shadow), INTENT(in) :: state
      INTEGER(c_int), INTENT(out) :: flag
      INTEGER(c_int), INTENT(out) :: code
      CHARACTER(c_char), INTENT(out) :: message(*)
      INTEGER(c_int), INTENT(in) :: message_len
      INTEGER(c_int), INTENT(out) :: has_fatal
      INTEGER(c_int), INTENT(out) :: count
      INTEGER(c_int), INTENT(out) :: err

      IF (message_len <= 0_c_int) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flag = c_bool(state%flag)
      code = INT(state%code, c_int)
      has_fatal = c_bool(state%has_fatal)
      count = INT(state%count, c_int)
      CALL copy_to_c_buffer(state%message, message, message_len)

      err = SUEWS_CAPI_OK

   END SUBROUTINE error_state_pack


   SUBROUTINE suews_error_state_error_message(code, buffer, buffer_len) BIND(C, name='suews_error_state_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_error_state_error_message

END MODULE module_c_api_error_state

MODULE c_api_error_state_module
   USE module_c_api_error_state
END MODULE c_api_error_state_module
