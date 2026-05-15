! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for bioCO2_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_bioco2
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BIOCO2_PRM_LEN = 8_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BIOCO2_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: bioco2_prm_shadow
      REAL(c_double) :: beta_bioco2 = 0.0_c_double
      REAL(c_double) :: beta_enh_bioco2 = 0.0_c_double
      REAL(c_double) :: alpha_bioco2 = 0.0_c_double
      REAL(c_double) :: alpha_enh_bioco2 = 0.0_c_double
      REAL(c_double) :: resp_a = 0.0_c_double
      REAL(c_double) :: resp_b = 0.0_c_double
      REAL(c_double) :: theta_bioco2 = 0.0_c_double
      REAL(c_double) :: min_res_bioco2 = 0.0_c_double
   END TYPE bioco2_prm_shadow

   PUBLIC :: suews_bioco2_prm_len
   PUBLIC :: suews_bioco2_prm_schema_version
   PUBLIC :: suews_bioco2_prm_default
   PUBLIC :: suews_bioco2_error_message

CONTAINS

   SUBROUTINE suews_bioco2_prm_len(n_flat, err) BIND(C, name='suews_bioco2_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_BIOCO2_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_bioco2_prm_len


   SUBROUTINE suews_bioco2_prm_schema_version(schema_version, err) BIND(C, name='suews_bioco2_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_BIOCO2_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_bioco2_prm_schema_version


   SUBROUTINE suews_bioco2_prm_default(flat, n_flat, err) BIND(C, name='suews_bioco2_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(bioco2_prm_shadow) :: state

      CALL bioco2_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_bioco2_prm_default


   SUBROUTINE bioco2_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(bioco2_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_BIOCO2_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%beta_bioco2
      flat(2) = state%beta_enh_bioco2
      flat(3) = state%alpha_bioco2
      flat(4) = state%alpha_enh_bioco2
      flat(5) = state%resp_a
      flat(6) = state%resp_b
      flat(7) = state%theta_bioco2
      flat(8) = state%min_res_bioco2

      err = SUEWS_CAPI_OK

   END SUBROUTINE bioco2_prm_pack


   SUBROUTINE suews_bioco2_error_message(code, buffer, buffer_len) BIND(C, name='suews_bioco2_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_bioco2_error_message

END MODULE module_c_api_bioco2

MODULE c_api_bioco2_module
   USE module_c_api_bioco2
END MODULE c_api_bioco2_module
