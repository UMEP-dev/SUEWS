! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SURF_STORE_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_surf_store
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SURF_STORE_PRM_LEN = 6_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SURF_STORE_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: surf_store_prm_shadow
      REAL(c_double) :: store_min = 0.0_c_double
      REAL(c_double) :: store_max = 0.0_c_double
      REAL(c_double) :: store_cap = 0.0_c_double
      INTEGER(c_int) :: drain_eq = 0_c_int
      REAL(c_double) :: drain_coef_1 = 0.0_c_double
      REAL(c_double) :: drain_coef_2 = 0.0_c_double
   END TYPE surf_store_prm_shadow

   PUBLIC :: suews_surf_store_prm_len
   PUBLIC :: suews_surf_store_prm_schema_version
   PUBLIC :: suews_surf_store_prm_default
   PUBLIC :: suews_surf_store_error_message

CONTAINS

   SUBROUTINE suews_surf_store_prm_len(n_flat, err) BIND(C, name='suews_surf_store_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_SURF_STORE_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_surf_store_prm_len


   SUBROUTINE suews_surf_store_prm_schema_version(schema_version, err) BIND(C, name='suews_surf_store_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_SURF_STORE_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_surf_store_prm_schema_version


   SUBROUTINE suews_surf_store_prm_default(flat, n_flat, err) BIND(C, name='suews_surf_store_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(surf_store_prm_shadow) :: state

      CALL surf_store_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_surf_store_prm_default


   SUBROUTINE surf_store_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(surf_store_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_SURF_STORE_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = state%store_min
      flat(2) = state%store_max
      flat(3) = state%store_cap
      flat(4) = REAL(state%drain_eq, c_double)
      flat(5) = state%drain_coef_1
      flat(6) = state%drain_coef_2

      err = SUEWS_CAPI_OK

   END SUBROUTINE surf_store_prm_pack


   SUBROUTINE suews_surf_store_error_message(code, buffer, buffer_len) BIND(C, name='suews_surf_store_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_surf_store_error_message

END MODULE module_c_api_surf_store

MODULE c_api_surf_store_module
   USE module_c_api_surf_store
END MODULE c_api_surf_store_module
