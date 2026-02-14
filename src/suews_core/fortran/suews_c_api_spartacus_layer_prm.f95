! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SPARTACUS_LAYER_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_spartacus_layer_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_SPARTACUS_LAYER_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: spartacus_layer_prm_shadow
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: building_frac
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: building_scale
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: veg_frac
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: veg_scale
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: alb_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: emis_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: alb_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: emis_wall
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: roof_albedo_dir_mult_fact
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: wall_specular_frac
   END TYPE spartacus_layer_prm_shadow

   PUBLIC :: suews_spartacus_layer_prm_len
   PUBLIC :: suews_spartacus_layer_prm_schema_version
   PUBLIC :: suews_spartacus_layer_prm_default
   PUBLIC :: suews_spartacus_layer_prm_error_message

CONTAINS

   SUBROUTINE suews_spartacus_layer_prm_len(n_flat, nlayer, nspec, err) BIND(C, name='suews_spartacus_layer_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: nspec
      INTEGER(c_int), INTENT(out) :: err

      TYPE(spartacus_layer_prm_shadow) :: state

      CALL spartacus_layer_prm_layout(state, n_flat, nlayer, nspec, err)

   END SUBROUTINE suews_spartacus_layer_prm_len


   SUBROUTINE suews_spartacus_layer_prm_schema_version(schema_version, err) BIND(C, name='suews_spartacus_layer_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_SPARTACUS_LAYER_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_spartacus_layer_prm_schema_version


   SUBROUTINE suews_spartacus_layer_prm_default(flat, n_flat, nlayer, nspec, err) BIND(C, name='suews_spartacus_layer_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: nspec
      INTEGER(c_int), INTENT(out) :: err

      TYPE(spartacus_layer_prm_shadow) :: state

      CALL spartacus_layer_prm_pack(state, flat, n_flat, nlayer, nspec, err)

   END SUBROUTINE suews_spartacus_layer_prm_default


   SUBROUTINE update_len_from_vec(field, nlayer, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(inout) :: nlayer
      INTEGER(c_int), INTENT(inout) :: err
      INTEGER(c_int) :: n_here

      IF (err /= SUEWS_CAPI_OK) RETURN
      IF (.NOT. ALLOCATED(field)) RETURN

      n_here = INT(SIZE(field), c_int)
      IF (nlayer == 0_c_int) THEN
         nlayer = n_here
      ELSEIF (nlayer /= n_here) THEN
         err = SUEWS_CAPI_BAD_STATE
      END IF

   END SUBROUTINE update_len_from_vec


   SUBROUTINE update_len_from_mat(field, nspec, nlayer, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(inout) :: nspec
      INTEGER(c_int), INTENT(inout) :: nlayer
      INTEGER(c_int), INTENT(inout) :: err
      INTEGER(c_int) :: spec_here
      INTEGER(c_int) :: layer_here

      IF (err /= SUEWS_CAPI_OK) RETURN
      IF (.NOT. ALLOCATED(field)) RETURN

      spec_here = INT(SIZE(field, 1), c_int)
      layer_here = INT(SIZE(field, 2), c_int)

      IF (nspec == 0_c_int) THEN
         nspec = spec_here
      ELSEIF (nspec /= spec_here) THEN
         err = SUEWS_CAPI_BAD_STATE
         RETURN
      END IF

      IF (nlayer == 0_c_int) THEN
         nlayer = layer_here
      ELSEIF (nlayer /= layer_here) THEN
         err = SUEWS_CAPI_BAD_STATE
      END IF

   END SUBROUTINE update_len_from_mat


   SUBROUTINE require_vec_layout(field, nlayer, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int), INTENT(inout) :: err

      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (nlayer == 0_c_int) THEN
         IF (ALLOCATED(field)) err = SUEWS_CAPI_BAD_STATE
      ELSE
         IF (.NOT. ALLOCATED(field)) THEN
            err = SUEWS_CAPI_BAD_STATE
         ELSEIF (INT(SIZE(field), c_int) /= nlayer) THEN
            err = SUEWS_CAPI_BAD_STATE
         END IF
      END IF

   END SUBROUTINE require_vec_layout


   SUBROUTINE require_mat_layout(field, nspec, nlayer, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(in) :: nspec
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int), INTENT(inout) :: err

      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (nspec == 0_c_int .OR. nlayer == 0_c_int) THEN
         IF (ALLOCATED(field)) err = SUEWS_CAPI_BAD_STATE
      ELSE
         IF (.NOT. ALLOCATED(field)) THEN
            err = SUEWS_CAPI_BAD_STATE
         ELSEIF (INT(SIZE(field, 1), c_int) /= nspec .OR. &
                 INT(SIZE(field, 2), c_int) /= nlayer) THEN
            err = SUEWS_CAPI_BAD_STATE
         END IF
      END IF

   END SUBROUTINE require_mat_layout


   SUBROUTINE spartacus_layer_prm_layout(state, n_flat, nlayer, nspec, err)
      IMPLICIT NONE

      TYPE(spartacus_layer_prm_shadow), INTENT(in) :: state
      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: nspec
      INTEGER(c_int), INTENT(out) :: err

      nlayer = 0_c_int
      nspec = 0_c_int
      err = SUEWS_CAPI_OK

      CALL update_len_from_vec(state%building_frac, nlayer, err)
      CALL update_len_from_vec(state%building_scale, nlayer, err)
      CALL update_len_from_vec(state%veg_frac, nlayer, err)
      CALL update_len_from_vec(state%veg_scale, nlayer, err)
      CALL update_len_from_vec(state%alb_roof, nlayer, err)
      CALL update_len_from_vec(state%emis_roof, nlayer, err)
      CALL update_len_from_vec(state%alb_wall, nlayer, err)
      CALL update_len_from_vec(state%emis_wall, nlayer, err)

      CALL update_len_from_mat(state%roof_albedo_dir_mult_fact, nspec, nlayer, err)
      CALL update_len_from_mat(state%wall_specular_frac, nspec, nlayer, err)

      CALL require_vec_layout(state%building_frac, nlayer, err)
      CALL require_vec_layout(state%building_scale, nlayer, err)
      CALL require_vec_layout(state%veg_frac, nlayer, err)
      CALL require_vec_layout(state%veg_scale, nlayer, err)
      CALL require_vec_layout(state%alb_roof, nlayer, err)
      CALL require_vec_layout(state%emis_roof, nlayer, err)
      CALL require_vec_layout(state%alb_wall, nlayer, err)
      CALL require_vec_layout(state%emis_wall, nlayer, err)

      CALL require_mat_layout(state%roof_albedo_dir_mult_fact, nspec, nlayer, err)
      CALL require_mat_layout(state%wall_specular_frac, nspec, nlayer, err)

      IF (err /= SUEWS_CAPI_OK) THEN
         n_flat = 0_c_int
         RETURN
      END IF

      IF (nlayer == 0_c_int) nspec = 0_c_int

      n_flat = 8_c_int*nlayer + 2_c_int*nspec*nlayer
      err = SUEWS_CAPI_OK

   END SUBROUTINE spartacus_layer_prm_layout


   SUBROUTINE pack_vec(field, flat, idx, nlayer)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:), ALLOCATABLE, INTENT(in) :: field
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(inout) :: idx
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int) :: i

      IF (nlayer <= 0_c_int) RETURN
      DO i = 1_c_int, nlayer
         flat(idx) = field(i)
         idx = idx + 1_c_int
      END DO

   END SUBROUTINE pack_vec


   SUBROUTINE pack_mat(field, flat, idx, nspec, nlayer)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(inout) :: idx
      INTEGER(c_int), INTENT(in) :: nspec
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int) :: i
      INTEGER(c_int) :: j

      IF (nspec <= 0_c_int .OR. nlayer <= 0_c_int) RETURN
      DO i = 1_c_int, nspec
         DO j = 1_c_int, nlayer
            flat(idx) = field(i, j)
            idx = idx + 1_c_int
         END DO
      END DO

   END SUBROUTINE pack_mat


   SUBROUTINE spartacus_layer_prm_pack(state, flat, n_flat, nlayer, nspec, err)
      IMPLICIT NONE

      TYPE(spartacus_layer_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: nspec
      INTEGER(c_int), INTENT(out) :: err
      INTEGER(c_int) :: n_expected
      INTEGER(c_int) :: idx

      CALL spartacus_layer_prm_layout(state, n_expected, nlayer, nspec, err)
      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (n_flat < n_expected) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1_c_int
      CALL pack_vec(state%building_frac, flat, idx, nlayer)
      CALL pack_vec(state%building_scale, flat, idx, nlayer)
      CALL pack_vec(state%veg_frac, flat, idx, nlayer)
      CALL pack_vec(state%veg_scale, flat, idx, nlayer)
      CALL pack_vec(state%alb_roof, flat, idx, nlayer)
      CALL pack_vec(state%emis_roof, flat, idx, nlayer)
      CALL pack_vec(state%alb_wall, flat, idx, nlayer)
      CALL pack_vec(state%emis_wall, flat, idx, nlayer)

      CALL pack_mat(state%roof_albedo_dir_mult_fact, flat, idx, nspec, nlayer)
      CALL pack_mat(state%wall_specular_frac, flat, idx, nspec, nlayer)

      err = SUEWS_CAPI_OK

   END SUBROUTINE spartacus_layer_prm_pack


   SUBROUTINE suews_spartacus_layer_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_spartacus_layer_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_spartacus_layer_prm_error_message

END MODULE module_c_api_spartacus_layer_prm

MODULE c_api_spartacus_layer_prm_module
   USE module_c_api_spartacus_layer_prm
END MODULE c_api_spartacus_layer_prm_module
