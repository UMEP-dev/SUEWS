! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for LAI_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_lai
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_LAI_PRM_LEN = 17_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_LAI_PRM_SCHEMA_VERSION = 2_c_int

   TYPE :: lai_prm_shadow
      REAL(c_double) :: baset = 0.0_c_double
      REAL(c_double) :: gddfull = 0.0_c_double
      REAL(c_double) :: basete = 0.0_c_double
      REAL(c_double) :: sddfull = 0.0_c_double
      REAL(c_double) :: laimin = 0.0_c_double
      REAL(c_double) :: laimax = 0.0_c_double
      REAL(c_double), DIMENSION(4) :: laipower = 0.0_c_double
      INTEGER(c_int) :: laitype = 0_c_int
      ! GH-1292 moisture-aware phenology parameters (laitype=2); relative soil water thresholds + days.
      REAL(c_double) :: w_wilt = 0.15_c_double
      REAL(c_double) :: w_opt = 0.40_c_double
      REAL(c_double) :: f_shape = 1.0_c_double
      REAL(c_double) :: w_on = 0.35_c_double
      REAL(c_double) :: w_off = 0.20_c_double
      REAL(c_double) :: tau_w = 15.0_c_double
   END TYPE lai_prm_shadow

   PUBLIC :: suews_lai_prm_len
   PUBLIC :: suews_lai_prm_schema_version
   PUBLIC :: suews_lai_prm_default
   PUBLIC :: suews_lai_error_message

CONTAINS

   SUBROUTINE suews_lai_prm_len(n_flat, err) BIND(C, name='suews_lai_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_LAI_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_lai_prm_len


   SUBROUTINE suews_lai_prm_schema_version(schema_version, err) BIND(C, name='suews_lai_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_LAI_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_lai_prm_schema_version


   SUBROUTINE suews_lai_prm_default(flat, n_flat, err) BIND(C, name='suews_lai_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(lai_prm_shadow) :: state

      CALL lai_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_lai_prm_default


   SUBROUTINE lai_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(lai_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_LAI_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%baset
      flat(2) = state%gddfull
      flat(3) = state%basete
      flat(4) = state%sddfull
      flat(5) = state%laimin
      flat(6) = state%laimax
      flat(7) = state%laipower(1)
      flat(8) = state%laipower(2)
      flat(9) = state%laipower(3)
      flat(10) = state%laipower(4)
      flat(11) = REAL(state%laitype, c_double)
      flat(12) = state%w_wilt
      flat(13) = state%w_opt
      flat(14) = state%f_shape
      flat(15) = state%w_on
      flat(16) = state%w_off
      flat(17) = state%tau_w

      err = SUEWS_CAPI_OK

   END SUBROUTINE lai_prm_pack


   SUBROUTINE suews_lai_error_message(code, buffer, buffer_len) BIND(C, name='suews_lai_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_lai_error_message

END MODULE module_c_api_lai

MODULE c_api_lai_module
   USE module_c_api_lai
END MODULE c_api_lai_module
