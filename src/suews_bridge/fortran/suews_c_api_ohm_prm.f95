! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for OHM_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_ohm_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OHM_PRM_LEN = 17_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OHM_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: ohm_coef_lc_shadow
      REAL(c_double) :: summer_dry = 0.0_c_double
      REAL(c_double) :: summer_wet = 0.0_c_double
      REAL(c_double) :: winter_dry = 0.0_c_double
      REAL(c_double) :: winter_wet = 0.0_c_double
   END TYPE ohm_coef_lc_shadow

   TYPE :: ohm_prm_shadow
      REAL(c_double) :: chanohm = 0.0_c_double
      REAL(c_double) :: cpanohm = 0.0_c_double
      REAL(c_double) :: kkanohm = 0.0_c_double
      REAL(c_double) :: ohm_threshsw = 0.0_c_double
      REAL(c_double) :: ohm_threshwd = 0.0_c_double
      TYPE(ohm_coef_lc_shadow), DIMENSION(3) :: ohm_coef_lc
   END TYPE ohm_prm_shadow

   PUBLIC :: suews_ohm_prm_len
   PUBLIC :: suews_ohm_prm_schema_version
   PUBLIC :: suews_ohm_prm_default
   PUBLIC :: suews_ohm_prm_error_message

CONTAINS

   SUBROUTINE suews_ohm_prm_len(n_flat, err) BIND(C, name='suews_ohm_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_OHM_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_ohm_prm_len


   SUBROUTINE suews_ohm_prm_schema_version(schema_version, err) BIND(C, name='suews_ohm_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_OHM_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_ohm_prm_schema_version


   SUBROUTINE suews_ohm_prm_default(flat, n_flat, err) BIND(C, name='suews_ohm_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(ohm_prm_shadow) :: state

      CALL ohm_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_ohm_prm_default


   SUBROUTINE ohm_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(ohm_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_OHM_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%chanohm
      flat(2) = state%cpanohm
      flat(3) = state%kkanohm
      flat(4) = state%ohm_threshsw
      flat(5) = state%ohm_threshwd
      flat(6) = state%ohm_coef_lc(1)%summer_dry
      flat(7) = state%ohm_coef_lc(1)%summer_wet
      flat(8) = state%ohm_coef_lc(1)%winter_dry
      flat(9) = state%ohm_coef_lc(1)%winter_wet
      flat(10) = state%ohm_coef_lc(2)%summer_dry
      flat(11) = state%ohm_coef_lc(2)%summer_wet
      flat(12) = state%ohm_coef_lc(2)%winter_dry
      flat(13) = state%ohm_coef_lc(2)%winter_wet
      flat(14) = state%ohm_coef_lc(3)%summer_dry
      flat(15) = state%ohm_coef_lc(3)%summer_wet
      flat(16) = state%ohm_coef_lc(3)%winter_dry
      flat(17) = state%ohm_coef_lc(3)%winter_wet

      err = SUEWS_CAPI_OK

   END SUBROUTINE ohm_prm_pack


   SUBROUTINE suews_ohm_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_ohm_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_ohm_prm_error_message

END MODULE module_c_api_ohm_prm

MODULE c_api_ohm_prm_module
   USE module_c_api_ohm_prm
END MODULE c_api_ohm_prm_module
