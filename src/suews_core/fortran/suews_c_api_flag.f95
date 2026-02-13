! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for flag_STATE (proof type for shared codec).
!
! This adapter intentionally uses a local shadow type to avoid importing
! `module_ctrl_type` and its wider dependency graph.
! -----------------------------------------------------------------------------
MODULE module_c_api_flag
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_FLAG_STATE_LEN = 5_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_FLAG_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: flag_state_shadow
      LOGICAL :: flag_converge = .FALSE.
      INTEGER :: i_iter = 0
      INTEGER :: stebbs_bldg_init = 0
      LOGICAL :: snow_warning_shown = .FALSE.
      LOGICAL :: iter_safe = .TRUE.
   END TYPE flag_state_shadow

   PUBLIC :: suews_flag_state_len
   PUBLIC :: suews_flag_state_schema_version
   PUBLIC :: suews_flag_state_default
   PUBLIC :: suews_flag_error_message

CONTAINS

   SUBROUTINE suews_flag_state_len(n_flat, err) BIND(C, name='suews_flag_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_FLAG_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_flag_state_len


   SUBROUTINE suews_flag_state_schema_version(schema_version, err) BIND(C, name='suews_flag_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_FLAG_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_flag_state_schema_version


   SUBROUTINE suews_flag_state_default(flat, n_flat, err) BIND(C, name='suews_flag_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(flag_state_shadow) :: state

      CALL flag_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_flag_state_default


   SUBROUTINE flag_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(flag_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: n_flat_use
      INTEGER :: idx

      n_flat_use = n_flat
      IF (n_flat_use < SUEWS_CAPI_FLAG_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%flag_converge); idx = idx + 1
      flat(idx) = REAL(state%i_iter, c_double); idx = idx + 1
      flat(idx) = REAL(state%stebbs_bldg_init, c_double); idx = idx + 1
      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%snow_warning_shown); idx = idx + 1
      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE flag_state_pack


   SUBROUTINE suews_flag_error_message(code, buffer, buffer_len) BIND(C, name='suews_flag_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_flag_error_message

END MODULE module_c_api_flag

! Backward compatibility alias (deprecated - will be removed in future version)
MODULE c_api_flag_module
   USE module_c_api_flag
END MODULE c_api_flag_module
