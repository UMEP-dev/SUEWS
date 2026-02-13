! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SNOW_STATE.
! -----------------------------------------------------------------------------
MODULE module_c_api_snow
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text
   USE module_ctrl_const_allocate, ONLY: nsurf

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SNOW_STATE_LEN = 79_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SNOW_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: snow_state_shadow
      REAL(c_double) :: snowfallcum = 0.0_c_double
      REAL(c_double) :: snowalb = 0.0_c_double
      REAL(c_double) :: chsnow_per_interval = 0.0_c_double
      REAL(c_double) :: mwh = 0.0_c_double
      REAL(c_double) :: mwstore = 0.0_c_double
      REAL(c_double) :: qn_snow = 0.0_c_double
      REAL(c_double) :: qm = 0.0_c_double
      REAL(c_double) :: qmfreez = 0.0_c_double
      REAL(c_double) :: qmrain = 0.0_c_double
      REAL(c_double) :: swe = 0.0_c_double
      REAL(c_double) :: z0vsnow = 0.0_c_double
      REAL(c_double) :: rasnow = 0.0_c_double
      REAL(c_double) :: sice_hpa = 0.0_c_double
      REAL(c_double), DIMENSION(2) :: snowremoval = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: icefrac = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: snowdens = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: snowfrac = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: snowpack = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: snowwater = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: kup_ind_snow = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: qn_ind_snow = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: deltaqi = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: tsurf_ind_snow = 0.0_c_double
      LOGICAL :: iter_safe = .FALSE.
   END TYPE snow_state_shadow

   PUBLIC :: suews_snow_state_len
   PUBLIC :: suews_snow_state_schema_version
   PUBLIC :: suews_snow_state_default
   PUBLIC :: suews_snow_error_message

CONTAINS

   SUBROUTINE suews_snow_state_len(n_flat, err) BIND(C, name='suews_snow_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_SNOW_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_snow_state_len


   SUBROUTINE suews_snow_state_schema_version(schema_version, err) BIND(C, name='suews_snow_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_SNOW_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_snow_state_schema_version


   SUBROUTINE suews_snow_state_default(flat, n_flat, err) BIND(C, name='suews_snow_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(snow_state_shadow) :: state

      CALL snow_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_snow_state_default


   SUBROUTINE snow_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(snow_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER :: idx
      INTEGER :: i

      IF (n_flat < SUEWS_CAPI_SNOW_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1

      flat(idx) = state%snowfallcum; idx = idx + 1
      flat(idx) = state%snowalb; idx = idx + 1
      flat(idx) = state%chsnow_per_interval; idx = idx + 1
      flat(idx) = state%mwh; idx = idx + 1
      flat(idx) = state%mwstore; idx = idx + 1
      flat(idx) = state%qn_snow; idx = idx + 1
      flat(idx) = state%qm; idx = idx + 1
      flat(idx) = state%qmfreez; idx = idx + 1
      flat(idx) = state%qmrain; idx = idx + 1
      flat(idx) = state%swe; idx = idx + 1
      flat(idx) = state%z0vsnow; idx = idx + 1
      flat(idx) = state%rasnow; idx = idx + 1
      flat(idx) = state%sice_hpa; idx = idx + 1

      DO i = 1, 2
         flat(idx) = state%snowremoval(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%icefrac(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%snowdens(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%snowfrac(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%snowpack(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%snowwater(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%kup_ind_snow(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%qn_ind_snow(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%deltaqi(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%tsurf_ind_snow(i)
         idx = idx + 1
      END DO

      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE snow_state_pack


   SUBROUTINE suews_snow_error_message(code, buffer, buffer_len) BIND(C, name='suews_snow_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_snow_error_message

END MODULE module_c_api_snow

MODULE c_api_snow_module
   USE module_c_api_snow
END MODULE c_api_snow_module
