! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for ROUGHNESS_STATE.
! -----------------------------------------------------------------------------
MODULE module_c_api_roughness
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ROUGHNESS_STATE_LEN = 11_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ROUGHNESS_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: roughness_state_shadow
      REAL(c_double) :: faibldg_use = 0.0_c_double
      REAL(c_double) :: faievetree_use = 0.0_c_double
      REAL(c_double) :: faidectree_use = 0.0_c_double
      REAL(c_double) :: fai = 0.0_c_double
      REAL(c_double) :: pai = 0.0_c_double
      REAL(c_double) :: zh = 0.0_c_double
      REAL(c_double) :: z0m = 0.0_c_double
      REAL(c_double) :: z0v = 0.0_c_double
      REAL(c_double) :: zdm = 0.0_c_double
      REAL(c_double) :: zzd = 0.0_c_double
      LOGICAL :: iter_safe = .TRUE.
   END TYPE roughness_state_shadow

   PUBLIC :: suews_roughness_state_len
   PUBLIC :: suews_roughness_state_schema_version
   PUBLIC :: suews_roughness_state_default
   PUBLIC :: suews_roughness_error_message

CONTAINS

   SUBROUTINE suews_roughness_state_len(n_flat, err) BIND(C, name='suews_roughness_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_ROUGHNESS_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_roughness_state_len


   SUBROUTINE suews_roughness_state_schema_version(schema_version, err) BIND(C, name='suews_roughness_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_ROUGHNESS_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_roughness_state_schema_version


   SUBROUTINE suews_roughness_state_default(flat, n_flat, err) BIND(C, name='suews_roughness_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(roughness_state_shadow) :: state

      CALL roughness_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_roughness_state_default


   SUBROUTINE roughness_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(roughness_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_ROUGHNESS_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%faibldg_use
      flat(2) = state%faievetree_use
      flat(3) = state%faidectree_use
      flat(4) = state%fai
      flat(5) = state%pai
      flat(6) = state%zh
      flat(7) = state%z0m
      flat(8) = state%z0v
      flat(9) = state%zdm
      flat(10) = state%zzd
      flat(11) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE roughness_state_pack


   SUBROUTINE suews_roughness_error_message(code, buffer, buffer_len) BIND(C, name='suews_roughness_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_roughness_error_message

END MODULE module_c_api_roughness

MODULE c_api_roughness_module
   USE module_c_api_roughness
END MODULE c_api_roughness_module
