! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for solar_State.
!
! This adapter uses a local shadow type to avoid importing full physics/control
! modules into the bridge build.
! -----------------------------------------------------------------------------
MODULE module_c_api_solar
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SOLAR_STATE_LEN = 3_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SOLAR_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: solar_state_shadow
      REAL(c_double) :: azimuth_deg = 0.0_c_double
      REAL(c_double) :: zenith_deg = 0.0_c_double
      LOGICAL :: iter_safe = .TRUE.
   END TYPE solar_state_shadow

   PUBLIC :: suews_solar_state_len
   PUBLIC :: suews_solar_state_schema_version
   PUBLIC :: suews_solar_state_default
   PUBLIC :: suews_solar_error_message

CONTAINS

   SUBROUTINE suews_solar_state_len(n_flat, err) BIND(C, name='suews_solar_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_SOLAR_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_solar_state_len


   SUBROUTINE suews_solar_state_schema_version(schema_version, err) BIND(C, name='suews_solar_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_SOLAR_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_solar_state_schema_version


   SUBROUTINE suews_solar_state_default(flat, n_flat, err) BIND(C, name='suews_solar_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(solar_state_shadow) :: state

      CALL solar_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_solar_state_default


   SUBROUTINE solar_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(solar_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_SOLAR_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%azimuth_deg
      flat(2) = state%zenith_deg
      flat(3) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE solar_state_pack


   SUBROUTINE suews_solar_error_message(code, buffer, buffer_len) BIND(C, name='suews_solar_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_solar_error_message

END MODULE module_c_api_solar

MODULE c_api_solar_module
   USE module_c_api_solar
END MODULE c_api_solar_module
