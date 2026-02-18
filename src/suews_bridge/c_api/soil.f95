! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SOIL_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_soil
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SOIL_PRM_LEN = 3_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SOIL_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: soil_prm_shadow
      REAL(c_double) :: soildepth = 0.0_c_double
      REAL(c_double) :: soilstorecap = 0.0_c_double
      REAL(c_double) :: sathydraulicconduct = 0.0_c_double
   END TYPE soil_prm_shadow

   PUBLIC :: suews_soil_prm_len
   PUBLIC :: suews_soil_prm_schema_version
   PUBLIC :: suews_soil_prm_default
   PUBLIC :: suews_soil_error_message

CONTAINS

   SUBROUTINE suews_soil_prm_len(n_flat, err) BIND(C, name='suews_soil_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_SOIL_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_soil_prm_len


   SUBROUTINE suews_soil_prm_schema_version(schema_version, err) BIND(C, name='suews_soil_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_SOIL_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_soil_prm_schema_version


   SUBROUTINE suews_soil_prm_default(flat, n_flat, err) BIND(C, name='suews_soil_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(soil_prm_shadow) :: state

      CALL soil_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_soil_prm_default


   SUBROUTINE soil_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(soil_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_SOIL_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%soildepth
      flat(2) = state%soilstorecap
      flat(3) = state%sathydraulicconduct

      err = SUEWS_CAPI_OK

   END SUBROUTINE soil_prm_pack


   SUBROUTINE suews_soil_error_message(code, buffer, buffer_len) BIND(C, name='suews_soil_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_soil_error_message

END MODULE module_c_api_soil

MODULE c_api_soil_module
   USE module_c_api_soil
END MODULE c_api_soil_module
