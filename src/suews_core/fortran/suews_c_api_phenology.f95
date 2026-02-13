! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for PHENOLOGY_STATE.
! -----------------------------------------------------------------------------
MODULE module_c_api_phenology
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text
   USE module_ctrl_const_allocate, ONLY: nsurf, nvegsurf

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_PHENOLOGY_STATE_LEN = 76_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_PHENOLOGY_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: phenology_state_shadow
      REAL(c_double), DIMENSION(nsurf) :: alb = 0.0_c_double
      REAL(c_double), DIMENSION(nvegsurf) :: lai_id = 0.0_c_double
      REAL(c_double), DIMENSION(nvegsurf) :: gdd_id = 0.0_c_double
      REAL(c_double), DIMENSION(nvegsurf) :: sdd_id = 0.0_c_double
      REAL(c_double) :: vegphenlumps = 0.0_c_double
      REAL(c_double) :: porosity_id = 0.0_c_double
      REAL(c_double) :: decidcap_id = 0.0_c_double
      REAL(c_double) :: albdectr_id = 0.0_c_double
      REAL(c_double) :: albevetr_id = 0.0_c_double
      REAL(c_double) :: albgrass_id = 0.0_c_double
      REAL(c_double) :: tmin_id = 0.0_c_double
      REAL(c_double) :: tmax_id = 0.0_c_double
      REAL(c_double) :: lenday_id = 0.0_c_double
      REAL(c_double) :: tempveg = 0.0_c_double
      REAL(c_double), DIMENSION(6, nsurf) :: storedrainprm = 0.0_c_double
      REAL(c_double) :: gfunc = 0.0_c_double
      REAL(c_double) :: gsc = 0.0_c_double
      REAL(c_double) :: g_kdown = 0.0_c_double
      REAL(c_double) :: g_dq = 0.0_c_double
      REAL(c_double) :: g_ta = 0.0_c_double
      REAL(c_double) :: g_smd = 0.0_c_double
      REAL(c_double) :: g_lai = 0.0_c_double
      LOGICAL :: iter_safe = .FALSE.
   END TYPE phenology_state_shadow

   PUBLIC :: suews_phenology_state_len
   PUBLIC :: suews_phenology_state_schema_version
   PUBLIC :: suews_phenology_state_default
   PUBLIC :: suews_phenology_error_message

CONTAINS

   SUBROUTINE suews_phenology_state_len(n_flat, err) BIND(C, name='suews_phenology_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_PHENOLOGY_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_phenology_state_len


   SUBROUTINE suews_phenology_state_schema_version(schema_version, err) BIND(C, name='suews_phenology_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_PHENOLOGY_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_phenology_state_schema_version


   SUBROUTINE suews_phenology_state_default(flat, n_flat, err) BIND(C, name='suews_phenology_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(phenology_state_shadow) :: state

      CALL phenology_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_phenology_state_default


   SUBROUTINE phenology_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(phenology_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER :: idx
      INTEGER :: i
      INTEGER :: j

      IF (n_flat < SUEWS_CAPI_PHENOLOGY_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1

      DO i = 1, nsurf
         flat(idx) = state%alb(i)
         idx = idx + 1
      END DO

      DO i = 1, nvegsurf
         flat(idx) = state%lai_id(i)
         idx = idx + 1
      END DO

      DO i = 1, nvegsurf
         flat(idx) = state%gdd_id(i)
         idx = idx + 1
      END DO

      DO i = 1, nvegsurf
         flat(idx) = state%sdd_id(i)
         idx = idx + 1
      END DO

      flat(idx) = state%vegphenlumps; idx = idx + 1
      flat(idx) = state%porosity_id; idx = idx + 1
      flat(idx) = state%decidcap_id; idx = idx + 1
      flat(idx) = state%albdectr_id; idx = idx + 1
      flat(idx) = state%albevetr_id; idx = idx + 1
      flat(idx) = state%albgrass_id; idx = idx + 1
      flat(idx) = state%tmin_id; idx = idx + 1
      flat(idx) = state%tmax_id; idx = idx + 1
      flat(idx) = state%lenday_id; idx = idx + 1
      flat(idx) = state%tempveg; idx = idx + 1

      DO j = 1, nsurf
         DO i = 1, 6
            flat(idx) = state%storedrainprm(i, j)
            idx = idx + 1
         END DO
      END DO

      flat(idx) = state%gfunc; idx = idx + 1
      flat(idx) = state%gsc; idx = idx + 1
      flat(idx) = state%g_kdown; idx = idx + 1
      flat(idx) = state%g_dq; idx = idx + 1
      flat(idx) = state%g_ta; idx = idx + 1
      flat(idx) = state%g_smd; idx = idx + 1
      flat(idx) = state%g_lai; idx = idx + 1
      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE phenology_state_pack


   SUBROUTINE suews_phenology_error_message(code, buffer, buffer_len) BIND(C, name='suews_phenology_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_phenology_error_message

END MODULE module_c_api_phenology

MODULE c_api_phenology_module
   USE module_c_api_phenology
END MODULE c_api_phenology_module
