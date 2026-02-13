! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SNOW_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_snow_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SNOW_PRM_LEN = 71_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SNOW_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: snow_prm_shadow
      REAL(c_double) :: crwmax = 0.0_c_double
      REAL(c_double) :: crwmin = 0.0_c_double
      REAL(c_double) :: narp_emis_snow = 0.0_c_double
      REAL(c_double) :: preciplimit = 0.0_c_double
      REAL(c_double) :: preciplimitalb = 0.0_c_double
      REAL(c_double) :: snowalbmax = 0.0_c_double
      REAL(c_double) :: snowalbmin = 0.0_c_double
      REAL(c_double) :: snowdensmax = 0.0_c_double
      REAL(c_double) :: snowdensmin = 0.0_c_double
      REAL(c_double) :: snowlimbldg = 0.0_c_double
      REAL(c_double) :: snowlimpaved = 0.0_c_double
      REAL(c_double), DIMENSION(7) :: snowpacklimit = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: snowprof_24hr_working = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: snowprof_24hr_holiday = 0.0_c_double
      REAL(c_double) :: tau_a = 0.0_c_double
      REAL(c_double) :: tau_f = 0.0_c_double
      REAL(c_double) :: tau_r = 0.0_c_double
      REAL(c_double) :: tempmeltfact = 0.0_c_double
      REAL(c_double) :: radmeltfact = 0.0_c_double
   END TYPE snow_prm_shadow

   PUBLIC :: suews_snow_prm_len
   PUBLIC :: suews_snow_prm_schema_version
   PUBLIC :: suews_snow_prm_default
   PUBLIC :: suews_snow_prm_error_message

CONTAINS

   SUBROUTINE suews_snow_prm_len(n_flat, err) BIND(C, name='suews_snow_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_SNOW_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_snow_prm_len


   SUBROUTINE suews_snow_prm_schema_version(schema_version, err) BIND(C, name='suews_snow_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_SNOW_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_snow_prm_schema_version


   SUBROUTINE suews_snow_prm_default(flat, n_flat, err) BIND(C, name='suews_snow_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(snow_prm_shadow) :: state

      CALL snow_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_snow_prm_default


   SUBROUTINE snow_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(snow_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: idx
      INTEGER :: i

      IF (n_flat < SUEWS_CAPI_SNOW_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      flat(idx) = state%crwmax; idx = idx + 1
      flat(idx) = state%crwmin; idx = idx + 1
      flat(idx) = state%narp_emis_snow; idx = idx + 1
      flat(idx) = state%preciplimit; idx = idx + 1
      flat(idx) = state%preciplimitalb; idx = idx + 1
      flat(idx) = state%snowalbmax; idx = idx + 1
      flat(idx) = state%snowalbmin; idx = idx + 1
      flat(idx) = state%snowdensmax; idx = idx + 1
      flat(idx) = state%snowdensmin; idx = idx + 1
      flat(idx) = state%snowlimbldg; idx = idx + 1
      flat(idx) = state%snowlimpaved; idx = idx + 1

      DO i = 1, 7
         flat(idx) = state%snowpacklimit(i)
         idx = idx + 1
      END DO

      DO i = 1, 24
         flat(idx) = state%snowprof_24hr_working(i)
         idx = idx + 1
      END DO

      DO i = 1, 24
         flat(idx) = state%snowprof_24hr_holiday(i)
         idx = idx + 1
      END DO

      flat(idx) = state%tau_a; idx = idx + 1
      flat(idx) = state%tau_f; idx = idx + 1
      flat(idx) = state%tau_r; idx = idx + 1
      flat(idx) = state%tempmeltfact; idx = idx + 1
      flat(idx) = state%radmeltfact

      err = SUEWS_CAPI_OK

   END SUBROUTINE snow_prm_pack


   SUBROUTINE suews_snow_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_snow_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_snow_prm_error_message

END MODULE module_c_api_snow_prm

MODULE c_api_snow_prm_module
   USE module_c_api_snow_prm
END MODULE c_api_snow_prm_module
