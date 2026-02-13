! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SUEWS_FORCING.
! -----------------------------------------------------------------------------
MODULE module_c_api_forcing
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_FORCING_BASE_LEN = 16_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_FORCING_SCHEMA_VERSION = 1_c_int

   TYPE :: suews_forcing_shadow
      REAL(c_double) :: kdown = 0.0_c_double
      REAL(c_double) :: ldown = 0.0_c_double
      REAL(c_double) :: rh = 0.0_c_double
      REAL(c_double) :: pres = 0.0_c_double
      REAL(c_double) :: tair_av_5d = 0.0_c_double
      REAL(c_double) :: u = 0.0_c_double
      REAL(c_double) :: rain = 0.0_c_double
      REAL(c_double) :: wu_m3 = 0.0_c_double
      REAL(c_double) :: fcld = 0.0_c_double
      REAL(c_double) :: lai_obs = 0.0_c_double
      REAL(c_double) :: snowfrac = 0.0_c_double
      REAL(c_double) :: xsmd = 0.0_c_double
      REAL(c_double) :: qf_obs = 0.0_c_double
      REAL(c_double) :: qn1_obs = 0.0_c_double
      REAL(c_double) :: qs_obs = 0.0_c_double
      REAL(c_double) :: temp_c = 0.0_c_double
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: ts5mindata_ir
   END TYPE suews_forcing_shadow

   PUBLIC :: suews_forcing_len
   PUBLIC :: suews_forcing_schema_version
   PUBLIC :: suews_forcing_default
   PUBLIC :: suews_forcing_error_message

CONTAINS

   SUBROUTINE suews_forcing_len(n_flat, ts5mindata_ir_len, err) BIND(C, name='suews_forcing_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: ts5mindata_ir_len
      INTEGER(c_int), INTENT(out) :: err

      TYPE(suews_forcing_shadow) :: state

      CALL suews_forcing_layout(state, n_flat, ts5mindata_ir_len, err)

   END SUBROUTINE suews_forcing_len


   SUBROUTINE suews_forcing_schema_version(schema_version, err) BIND(C, name='suews_forcing_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_FORCING_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_forcing_schema_version


   SUBROUTINE suews_forcing_default(flat, n_flat, ts5mindata_ir_len, err) BIND(C, name='suews_forcing_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: ts5mindata_ir_len
      INTEGER(c_int), INTENT(out) :: err

      TYPE(suews_forcing_shadow) :: state

      CALL suews_forcing_pack(state, flat, n_flat, ts5mindata_ir_len, err)

   END SUBROUTINE suews_forcing_default


   SUBROUTINE suews_forcing_layout(state, n_flat, ts5mindata_ir_len, err)
      IMPLICIT NONE

      TYPE(suews_forcing_shadow), INTENT(in) :: state
      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: ts5mindata_ir_len
      INTEGER(c_int), INTENT(out) :: err

      ts5mindata_ir_len = 0_c_int
      IF (ALLOCATED(state%ts5mindata_ir)) THEN
         ts5mindata_ir_len = INT(SIZE(state%ts5mindata_ir), c_int)
      END IF

      n_flat = SUEWS_CAPI_FORCING_BASE_LEN + ts5mindata_ir_len
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_forcing_layout


   SUBROUTINE suews_forcing_pack(state, flat, n_flat, ts5mindata_ir_len, err)
      IMPLICIT NONE

      TYPE(suews_forcing_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: ts5mindata_ir_len
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: n_expected
      INTEGER :: idx
      INTEGER :: i

      CALL suews_forcing_layout(state, n_expected, ts5mindata_ir_len, err)
      IF (err /= SUEWS_CAPI_OK) THEN
         RETURN
      END IF

      IF (n_flat < n_expected) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      flat(idx) = state%kdown; idx = idx + 1
      flat(idx) = state%ldown; idx = idx + 1
      flat(idx) = state%rh; idx = idx + 1
      flat(idx) = state%pres; idx = idx + 1
      flat(idx) = state%tair_av_5d; idx = idx + 1
      flat(idx) = state%u; idx = idx + 1
      flat(idx) = state%rain; idx = idx + 1
      flat(idx) = state%wu_m3; idx = idx + 1
      flat(idx) = state%fcld; idx = idx + 1
      flat(idx) = state%lai_obs; idx = idx + 1
      flat(idx) = state%snowfrac; idx = idx + 1
      flat(idx) = state%xsmd; idx = idx + 1
      flat(idx) = state%qf_obs; idx = idx + 1
      flat(idx) = state%qn1_obs; idx = idx + 1
      flat(idx) = state%qs_obs; idx = idx + 1
      flat(idx) = state%temp_c; idx = idx + 1

      IF (ts5mindata_ir_len > 0_c_int) THEN
         DO i = 1, INT(ts5mindata_ir_len)
            flat(idx) = state%ts5mindata_ir(i)
            idx = idx + 1
         END DO
      END IF

      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_forcing_pack


   SUBROUTINE suews_forcing_error_message(code, buffer, buffer_len) BIND(C, name='suews_forcing_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_forcing_error_message

END MODULE module_c_api_forcing

MODULE c_api_forcing_module
   USE module_c_api_forcing
END MODULE c_api_forcing_module
