! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for CONDUCTANCE_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_conductance
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_CONDUCTANCE_PRM_LEN = 12_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_CONDUCTANCE_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: conductance_prm_shadow
      REAL(c_double) :: g_max = 0.0_c_double
      REAL(c_double) :: g_k = 0.0_c_double
      REAL(c_double) :: g_q_base = 0.0_c_double
      REAL(c_double) :: g_q_shape = 0.0_c_double
      REAL(c_double) :: g_t = 0.0_c_double
      REAL(c_double) :: g_sm = 0.0_c_double
      REAL(c_double) :: kmax = 0.0_c_double
      INTEGER(c_int) :: gsmodel = 0_c_int
      REAL(c_double) :: s1 = 0.0_c_double
      REAL(c_double) :: s2 = 0.0_c_double
      REAL(c_double) :: th = 0.0_c_double
      REAL(c_double) :: tl = 0.0_c_double
   END TYPE conductance_prm_shadow

   PUBLIC :: suews_conductance_prm_len
   PUBLIC :: suews_conductance_prm_schema_version
   PUBLIC :: suews_conductance_prm_default
   PUBLIC :: suews_conductance_error_message

CONTAINS

   SUBROUTINE suews_conductance_prm_len(n_flat, err) BIND(C, name='suews_conductance_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_CONDUCTANCE_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_conductance_prm_len


   SUBROUTINE suews_conductance_prm_schema_version(schema_version, err) BIND(C, name='suews_conductance_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_CONDUCTANCE_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_conductance_prm_schema_version


   SUBROUTINE suews_conductance_prm_default(flat, n_flat, err) BIND(C, name='suews_conductance_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(conductance_prm_shadow) :: state

      CALL conductance_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_conductance_prm_default


   SUBROUTINE conductance_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(conductance_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_CONDUCTANCE_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%g_max
      flat(2) = state%g_k
      flat(3) = state%g_q_base
      flat(4) = state%g_q_shape
      flat(5) = state%g_t
      flat(6) = state%g_sm
      flat(7) = state%kmax
      flat(8) = REAL(state%gsmodel, c_double)
      flat(9) = state%s1
      flat(10) = state%s2
      flat(11) = state%th
      flat(12) = state%tl

      err = SUEWS_CAPI_OK

   END SUBROUTINE conductance_prm_pack


   SUBROUTINE suews_conductance_error_message(code, buffer, buffer_len) BIND(C, name='suews_conductance_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_conductance_error_message

END MODULE module_c_api_conductance

MODULE c_api_conductance_module
   USE module_c_api_conductance
END MODULE c_api_conductance_module
