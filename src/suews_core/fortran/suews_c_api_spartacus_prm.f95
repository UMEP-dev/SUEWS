! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SPARTACUS_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_spartacus_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SPARTACUS_PRM_BASE_LEN = 14_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SPARTACUS_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: spartacus_prm_shadow
      REAL(c_double) :: air_ext_lw = 0.0_c_double
      REAL(c_double) :: air_ext_sw = 0.0_c_double
      REAL(c_double) :: air_ssa_lw = 0.0_c_double
      REAL(c_double) :: air_ssa_sw = 0.0_c_double
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: height
      REAL(c_double) :: ground_albedo_dir_mult_fact = 0.0_c_double
      INTEGER(c_int) :: n_stream_lw_urban = 0_c_int
      INTEGER(c_int) :: n_stream_sw_urban = 0_c_int
      INTEGER(c_int) :: n_vegetation_region_urban = 0_c_int
      REAL(c_double) :: sw_dn_direct_frac = 0.0_c_double
      REAL(c_double) :: use_sw_direct_albedo = 0.0_c_double
      REAL(c_double) :: veg_contact_fraction_const = 0.0_c_double
      REAL(c_double) :: veg_fsd_const = 0.0_c_double
      REAL(c_double) :: veg_ssa_lw = 0.0_c_double
      REAL(c_double) :: veg_ssa_sw = 0.0_c_double
   END TYPE spartacus_prm_shadow

   PUBLIC :: suews_spartacus_prm_len
   PUBLIC :: suews_spartacus_prm_schema_version
   PUBLIC :: suews_spartacus_prm_default
   PUBLIC :: suews_spartacus_prm_error_message

CONTAINS

   SUBROUTINE suews_spartacus_prm_len(n_flat, height_len, nlayer, err) BIND(C, name='suews_spartacus_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: height_len
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: err

      TYPE(spartacus_prm_shadow) :: state

      CALL spartacus_prm_layout(state, n_flat, height_len, nlayer, err)

   END SUBROUTINE suews_spartacus_prm_len


   SUBROUTINE suews_spartacus_prm_schema_version(schema_version, err) BIND(C, name='suews_spartacus_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_SPARTACUS_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_spartacus_prm_schema_version


   SUBROUTINE suews_spartacus_prm_default(flat, n_flat, height_len, nlayer, err) BIND(C, name='suews_spartacus_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: height_len
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: err

      TYPE(spartacus_prm_shadow) :: state

      CALL spartacus_prm_pack(state, flat, n_flat, height_len, nlayer, err)

   END SUBROUTINE suews_spartacus_prm_default


   SUBROUTINE spartacus_prm_layout(state, n_flat, height_len, nlayer, err)
      IMPLICIT NONE

      TYPE(spartacus_prm_shadow), INTENT(in) :: state
      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: height_len
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: err

      height_len = 0_c_int
      IF (ALLOCATED(state%height)) THEN
         height_len = INT(SIZE(state%height), c_int)
      END IF

      IF (height_len > 0_c_int) THEN
         nlayer = height_len - 1_c_int
      ELSE
         nlayer = 0_c_int
      END IF

      n_flat = SUEWS_CAPI_SPARTACUS_PRM_BASE_LEN + height_len
      err = SUEWS_CAPI_OK

   END SUBROUTINE spartacus_prm_layout


   SUBROUTINE spartacus_prm_pack(state, flat, n_flat, height_len, nlayer, err)
      IMPLICIT NONE

      TYPE(spartacus_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: height_len
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: n_expected
      INTEGER(c_int) :: idx
      INTEGER(c_int) :: i

      CALL spartacus_prm_layout(state, n_expected, height_len, nlayer, err)
      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (n_flat < n_expected) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1_c_int
      flat(idx) = state%air_ext_lw; idx = idx + 1_c_int
      flat(idx) = state%air_ext_sw; idx = idx + 1_c_int
      flat(idx) = state%air_ssa_lw; idx = idx + 1_c_int
      flat(idx) = state%air_ssa_sw; idx = idx + 1_c_int

      IF (height_len > 0_c_int) THEN
         DO i = 1_c_int, height_len
            flat(idx) = state%height(i)
            idx = idx + 1_c_int
         END DO
      END IF

      flat(idx) = state%ground_albedo_dir_mult_fact; idx = idx + 1_c_int
      flat(idx) = REAL(state%n_stream_lw_urban, c_double); idx = idx + 1_c_int
      flat(idx) = REAL(state%n_stream_sw_urban, c_double); idx = idx + 1_c_int
      flat(idx) = REAL(state%n_vegetation_region_urban, c_double); idx = idx + 1_c_int
      flat(idx) = state%sw_dn_direct_frac; idx = idx + 1_c_int
      flat(idx) = state%use_sw_direct_albedo; idx = idx + 1_c_int
      flat(idx) = state%veg_contact_fraction_const; idx = idx + 1_c_int
      flat(idx) = state%veg_fsd_const; idx = idx + 1_c_int
      flat(idx) = state%veg_ssa_lw; idx = idx + 1_c_int
      flat(idx) = state%veg_ssa_sw

      err = SUEWS_CAPI_OK

   END SUBROUTINE spartacus_prm_pack


   SUBROUTINE suews_spartacus_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_spartacus_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_spartacus_prm_error_message

END MODULE module_c_api_spartacus_prm

MODULE c_api_spartacus_prm_module
   USE module_c_api_spartacus_prm
END MODULE c_api_spartacus_prm_module
