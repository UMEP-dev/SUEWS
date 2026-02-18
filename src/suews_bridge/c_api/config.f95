! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SUEWS_CONFIG.
! -----------------------------------------------------------------------------
MODULE module_c_api_config
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_CONFIG_LEN = 21_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_CONFIG_SCHEMA_VERSION = 1_c_int

   TYPE :: suews_config_shadow
      INTEGER :: rsl_method = 0
      INTEGER :: emissions_method = 0
      INTEGER :: rough_len_heat_method = 0
      INTEGER :: rough_len_mom_method = 0
      INTEGER :: fai_method = 0
      INTEGER :: smd_method = 0
      INTEGER :: water_use_method = 0
      INTEGER :: net_radiation_method = 0
      INTEGER :: stability_method = 0
      INTEGER :: storage_heat_method = 0
      INTEGER :: diagnose = 0
      INTEGER :: snow_use = 0
      LOGICAL :: use_sw_direct_albedo = .FALSE.
      INTEGER :: ohm_inc_qf = 0
      INTEGER :: diag_qs = 0
      INTEGER :: evap_method = 0
      INTEGER :: lai_method = 0
      INTEGER :: rsl_level = 0
      INTEGER :: stebbs_method = 0
      INTEGER :: rc_method = 0
      LOGICAL :: flag_test = .FALSE.
   END TYPE suews_config_shadow

   PUBLIC :: suews_config_len
   PUBLIC :: suews_config_schema_version
   PUBLIC :: suews_config_default
   PUBLIC :: suews_config_error_message

CONTAINS

   SUBROUTINE suews_config_len(n_flat, err) BIND(C, name='suews_config_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_CONFIG_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_config_len


   SUBROUTINE suews_config_schema_version(schema_version, err) BIND(C, name='suews_config_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_CONFIG_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_config_schema_version


   SUBROUTINE suews_config_default(flat, n_flat, err) BIND(C, name='suews_config_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(suews_config_shadow) :: state

      CALL suews_config_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_config_default


   SUBROUTINE suews_config_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(suews_config_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      IF (n_flat < SUEWS_CAPI_CONFIG_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      flat(1) = REAL(state%rsl_method, c_double)
      flat(2) = REAL(state%emissions_method, c_double)
      flat(3) = REAL(state%rough_len_heat_method, c_double)
      flat(4) = REAL(state%rough_len_mom_method, c_double)
      flat(5) = REAL(state%fai_method, c_double)
      flat(6) = REAL(state%smd_method, c_double)
      flat(7) = REAL(state%water_use_method, c_double)
      flat(8) = REAL(state%net_radiation_method, c_double)
      flat(9) = REAL(state%stability_method, c_double)
      flat(10) = REAL(state%storage_heat_method, c_double)
      flat(11) = REAL(state%diagnose, c_double)
      flat(12) = REAL(state%snow_use, c_double)
      flat(13) = MERGE(1.0_c_double, 0.0_c_double, state%use_sw_direct_albedo)
      flat(14) = REAL(state%ohm_inc_qf, c_double)
      flat(15) = REAL(state%diag_qs, c_double)
      flat(16) = REAL(state%evap_method, c_double)
      flat(17) = REAL(state%lai_method, c_double)
      flat(18) = REAL(state%rsl_level, c_double)
      flat(19) = REAL(state%stebbs_method, c_double)
      flat(20) = REAL(state%rc_method, c_double)
      flat(21) = MERGE(1.0_c_double, 0.0_c_double, state%flag_test)

      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_config_pack


   SUBROUTINE suews_config_error_message(code, buffer, buffer_len) BIND(C, name='suews_config_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_config_error_message

END MODULE module_c_api_config

MODULE c_api_config_module
   USE module_c_api_config
END MODULE c_api_config_module
