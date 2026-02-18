! -----------------------------------------------------------------------------
! Shared helpers for SUEWS Rust bridge C API adapter modules.
!
! This module centralises:
! - Common error codes
! - Generic error-code-to-text lookup
! - Safe copy from Fortran CHARACTER to C char buffer
! -----------------------------------------------------------------------------
MODULE module_c_api_common
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_char, c_null_char

   IMPLICIT NONE

   PRIVATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OK = 0_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BAD_DT = 1_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BAD_TIME = 2_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BAD_BUFFER = 3_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BAD_STATE = 4_c_int

   PUBLIC :: copy_to_c_buffer
   PUBLIC :: suews_capi_error_text

CONTAINS

   SUBROUTINE suews_capi_error_text(code, msg)
      IMPLICIT NONE

      INTEGER(c_int), INTENT(in) :: code
      CHARACTER(LEN=*), INTENT(out) :: msg

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

   END SUBROUTINE suews_capi_error_text


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

END MODULE module_c_api_common
