! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for IRRIGATION_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_irrigation_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_IRRIGATION_PRM_LEN = 121_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_IRRIGATION_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: irrig_daywater_shadow
      REAL(c_double) :: monday_flag = 0.0_c_double
      REAL(c_double) :: monday_percent = 0.0_c_double
      REAL(c_double) :: tuesday_flag = 0.0_c_double
      REAL(c_double) :: tuesday_percent = 0.0_c_double
      REAL(c_double) :: wednesday_flag = 0.0_c_double
      REAL(c_double) :: wednesday_percent = 0.0_c_double
      REAL(c_double) :: thursday_flag = 0.0_c_double
      REAL(c_double) :: thursday_percent = 0.0_c_double
      REAL(c_double) :: friday_flag = 0.0_c_double
      REAL(c_double) :: friday_percent = 0.0_c_double
      REAL(c_double) :: saturday_flag = 0.0_c_double
      REAL(c_double) :: saturday_percent = 0.0_c_double
      REAL(c_double) :: sunday_flag = 0.0_c_double
      REAL(c_double) :: sunday_percent = 0.0_c_double
   END TYPE irrig_daywater_shadow

   TYPE :: irrigation_prm_shadow
      REAL(c_double) :: h_maintain = 0.0_c_double
      REAL(c_double) :: faut = 0.0_c_double
      REAL(c_double), DIMENSION(3) :: ie_a = 0.0_c_double
      REAL(c_double), DIMENSION(3) :: ie_m = 0.0_c_double
      INTEGER(c_int) :: ie_start = 0_c_int
      INTEGER(c_int) :: ie_end = 0_c_int
      REAL(c_double) :: internalwateruse_h = 0.0_c_double
      TYPE(irrig_daywater_shadow) :: irr_daywater
      REAL(c_double), DIMENSION(24) :: wuprofa_24hr_working = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: wuprofa_24hr_holiday = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: wuprofm_24hr_working = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: wuprofm_24hr_holiday = 0.0_c_double
   END TYPE irrigation_prm_shadow

   PUBLIC :: suews_irrigation_prm_len
   PUBLIC :: suews_irrigation_prm_schema_version
   PUBLIC :: suews_irrigation_prm_default
   PUBLIC :: suews_irrigation_prm_error_message

CONTAINS

   SUBROUTINE suews_irrigation_prm_len(n_flat, err) BIND(C, name='suews_irrigation_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_IRRIGATION_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_irrigation_prm_len


   SUBROUTINE suews_irrigation_prm_schema_version(schema_version, err) BIND(C, name='suews_irrigation_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_IRRIGATION_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_irrigation_prm_schema_version


   SUBROUTINE suews_irrigation_prm_default(flat, n_flat, err) BIND(C, name='suews_irrigation_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(irrigation_prm_shadow) :: state

      CALL irrigation_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_irrigation_prm_default


   SUBROUTINE irrigation_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(irrigation_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: idx
      INTEGER :: i

      IF (n_flat < SUEWS_CAPI_IRRIGATION_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      flat(idx) = state%h_maintain; idx = idx + 1
      flat(idx) = state%faut; idx = idx + 1

      DO i = 1, 3
         flat(idx) = state%ie_a(i)
         idx = idx + 1
      END DO
      DO i = 1, 3
         flat(idx) = state%ie_m(i)
         idx = idx + 1
      END DO

      flat(idx) = REAL(state%ie_start, c_double); idx = idx + 1
      flat(idx) = REAL(state%ie_end, c_double); idx = idx + 1
      flat(idx) = state%internalwateruse_h; idx = idx + 1

      flat(idx) = state%irr_daywater%monday_flag; idx = idx + 1
      flat(idx) = state%irr_daywater%monday_percent; idx = idx + 1
      flat(idx) = state%irr_daywater%tuesday_flag; idx = idx + 1
      flat(idx) = state%irr_daywater%tuesday_percent; idx = idx + 1
      flat(idx) = state%irr_daywater%wednesday_flag; idx = idx + 1
      flat(idx) = state%irr_daywater%wednesday_percent; idx = idx + 1
      flat(idx) = state%irr_daywater%thursday_flag; idx = idx + 1
      flat(idx) = state%irr_daywater%thursday_percent; idx = idx + 1
      flat(idx) = state%irr_daywater%friday_flag; idx = idx + 1
      flat(idx) = state%irr_daywater%friday_percent; idx = idx + 1
      flat(idx) = state%irr_daywater%saturday_flag; idx = idx + 1
      flat(idx) = state%irr_daywater%saturday_percent; idx = idx + 1
      flat(idx) = state%irr_daywater%sunday_flag; idx = idx + 1
      flat(idx) = state%irr_daywater%sunday_percent; idx = idx + 1

      DO i = 1, 24
         flat(idx) = state%wuprofa_24hr_working(i)
         idx = idx + 1
      END DO
      DO i = 1, 24
         flat(idx) = state%wuprofa_24hr_holiday(i)
         idx = idx + 1
      END DO
      DO i = 1, 24
         flat(idx) = state%wuprofm_24hr_working(i)
         idx = idx + 1
      END DO
      DO i = 1, 24
         flat(idx) = state%wuprofm_24hr_holiday(i)
         idx = idx + 1
      END DO

      err = SUEWS_CAPI_OK

   END SUBROUTINE irrigation_prm_pack


   SUBROUTINE suews_irrigation_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_irrigation_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_irrigation_prm_error_message

END MODULE module_c_api_irrigation_prm

MODULE c_api_irrigation_prm_module
   USE module_c_api_irrigation_prm
END MODULE c_api_irrigation_prm_module
