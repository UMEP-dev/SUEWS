! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for EHC_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_ehc_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_EHC_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: ehc_prm_shadow
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: soil_storecap_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: soil_storecap_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: state_limit_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: state_limit_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: wet_thresh_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: wet_thresh_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tin_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tin_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tin_surf
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: k_roof
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: k_wall
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: k_surf
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: cp_roof
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: cp_wall
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: cp_surf
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: dz_roof
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: dz_wall
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: dz_surf
   END TYPE ehc_prm_shadow

   PUBLIC :: suews_ehc_prm_len
   PUBLIC :: suews_ehc_prm_schema_version
   PUBLIC :: suews_ehc_prm_default
   PUBLIC :: suews_ehc_prm_error_message

CONTAINS

   SUBROUTINE suews_ehc_prm_len(n_flat, nlayer, ndepth, err) BIND(C, name='suews_ehc_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: ndepth
      INTEGER(c_int), INTENT(out) :: err

      TYPE(ehc_prm_shadow) :: state

      CALL ehc_prm_layout(state, n_flat, nlayer, ndepth, err)

   END SUBROUTINE suews_ehc_prm_len


   SUBROUTINE suews_ehc_prm_schema_version(schema_version, err) BIND(C, name='suews_ehc_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_EHC_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_ehc_prm_schema_version


   SUBROUTINE suews_ehc_prm_default(flat, n_flat, nlayer, ndepth, err) BIND(C, name='suews_ehc_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: ndepth
      INTEGER(c_int), INTENT(out) :: err

      TYPE(ehc_prm_shadow) :: state

      CALL ehc_prm_pack(state, flat, n_flat, nlayer, ndepth, err)

   END SUBROUTINE suews_ehc_prm_default


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


   SUBROUTINE update_len_from_mat(field, nlayer, ndepth, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(inout) :: nlayer
      INTEGER(c_int), INTENT(inout) :: ndepth
      INTEGER(c_int), INTENT(inout) :: err
      INTEGER(c_int) :: layer_here
      INTEGER(c_int) :: depth_here

      IF (err /= SUEWS_CAPI_OK) RETURN
      IF (.NOT. ALLOCATED(field)) RETURN

      layer_here = INT(SIZE(field, 1), c_int)
      depth_here = INT(SIZE(field, 2), c_int)

      IF (nlayer == 0_c_int) THEN
         nlayer = layer_here
      ELSEIF (nlayer /= layer_here) THEN
         err = SUEWS_CAPI_BAD_STATE
         RETURN
      END IF

      IF (ndepth == 0_c_int) THEN
         ndepth = depth_here
      ELSEIF (ndepth /= depth_here) THEN
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


   SUBROUTINE require_mat_layout(field, nlayer, ndepth, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int), INTENT(in) :: ndepth
      INTEGER(c_int), INTENT(inout) :: err

      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (nlayer == 0_c_int .OR. ndepth == 0_c_int) THEN
         IF (ALLOCATED(field)) err = SUEWS_CAPI_BAD_STATE
      ELSE
         IF (.NOT. ALLOCATED(field)) THEN
            err = SUEWS_CAPI_BAD_STATE
         ELSEIF (INT(SIZE(field, 1), c_int) /= nlayer .OR. &
                 INT(SIZE(field, 2), c_int) /= ndepth) THEN
            err = SUEWS_CAPI_BAD_STATE
         END IF
      END IF

   END SUBROUTINE require_mat_layout


   SUBROUTINE ehc_prm_layout(state, n_flat, nlayer, ndepth, err)
      IMPLICIT NONE

      TYPE(ehc_prm_shadow), INTENT(in) :: state
      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: ndepth
      INTEGER(c_int), INTENT(out) :: err

      nlayer = 0_c_int
      ndepth = 0_c_int
      err = SUEWS_CAPI_OK

      CALL update_len_from_vec(state%soil_storecap_roof, nlayer, err)
      CALL update_len_from_vec(state%soil_storecap_wall, nlayer, err)
      CALL update_len_from_vec(state%state_limit_roof, nlayer, err)
      CALL update_len_from_vec(state%state_limit_wall, nlayer, err)
      CALL update_len_from_vec(state%wet_thresh_roof, nlayer, err)
      CALL update_len_from_vec(state%wet_thresh_wall, nlayer, err)
      CALL update_len_from_vec(state%tin_roof, nlayer, err)
      CALL update_len_from_vec(state%tin_wall, nlayer, err)
      CALL update_len_from_vec(state%tin_surf, nlayer, err)

      CALL update_len_from_mat(state%k_roof, nlayer, ndepth, err)
      CALL update_len_from_mat(state%k_wall, nlayer, ndepth, err)
      CALL update_len_from_mat(state%k_surf, nlayer, ndepth, err)
      CALL update_len_from_mat(state%cp_roof, nlayer, ndepth, err)
      CALL update_len_from_mat(state%cp_wall, nlayer, ndepth, err)
      CALL update_len_from_mat(state%cp_surf, nlayer, ndepth, err)
      CALL update_len_from_mat(state%dz_roof, nlayer, ndepth, err)
      CALL update_len_from_mat(state%dz_wall, nlayer, ndepth, err)
      CALL update_len_from_mat(state%dz_surf, nlayer, ndepth, err)

      CALL require_vec_layout(state%soil_storecap_roof, nlayer, err)
      CALL require_vec_layout(state%soil_storecap_wall, nlayer, err)
      CALL require_vec_layout(state%state_limit_roof, nlayer, err)
      CALL require_vec_layout(state%state_limit_wall, nlayer, err)
      CALL require_vec_layout(state%wet_thresh_roof, nlayer, err)
      CALL require_vec_layout(state%wet_thresh_wall, nlayer, err)
      CALL require_vec_layout(state%tin_roof, nlayer, err)
      CALL require_vec_layout(state%tin_wall, nlayer, err)
      CALL require_vec_layout(state%tin_surf, nlayer, err)

      CALL require_mat_layout(state%k_roof, nlayer, ndepth, err)
      CALL require_mat_layout(state%k_wall, nlayer, ndepth, err)
      CALL require_mat_layout(state%k_surf, nlayer, ndepth, err)
      CALL require_mat_layout(state%cp_roof, nlayer, ndepth, err)
      CALL require_mat_layout(state%cp_wall, nlayer, ndepth, err)
      CALL require_mat_layout(state%cp_surf, nlayer, ndepth, err)
      CALL require_mat_layout(state%dz_roof, nlayer, ndepth, err)
      CALL require_mat_layout(state%dz_wall, nlayer, ndepth, err)
      CALL require_mat_layout(state%dz_surf, nlayer, ndepth, err)

      IF (err /= SUEWS_CAPI_OK) THEN
         n_flat = 0_c_int
         RETURN
      END IF

      IF (nlayer == 0_c_int) ndepth = 0_c_int

      n_flat = 9_c_int*nlayer + 9_c_int*nlayer*ndepth
      err = SUEWS_CAPI_OK

   END SUBROUTINE ehc_prm_layout


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


   SUBROUTINE pack_mat(field, flat, idx, nlayer, ndepth)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(inout) :: idx
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int), INTENT(in) :: ndepth
      INTEGER(c_int) :: i
      INTEGER(c_int) :: j

      IF (nlayer <= 0_c_int .OR. ndepth <= 0_c_int) RETURN
      DO i = 1_c_int, nlayer
         DO j = 1_c_int, ndepth
            flat(idx) = field(i, j)
            idx = idx + 1_c_int
         END DO
      END DO

   END SUBROUTINE pack_mat


   SUBROUTINE ehc_prm_pack(state, flat, n_flat, nlayer, ndepth, err)
      IMPLICIT NONE

      TYPE(ehc_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: ndepth
      INTEGER(c_int), INTENT(out) :: err
      INTEGER(c_int) :: n_expected
      INTEGER(c_int) :: idx

      CALL ehc_prm_layout(state, n_expected, nlayer, ndepth, err)
      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (n_flat < n_expected) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1_c_int
      CALL pack_vec(state%soil_storecap_roof, flat, idx, nlayer)
      CALL pack_vec(state%soil_storecap_wall, flat, idx, nlayer)
      CALL pack_vec(state%state_limit_roof, flat, idx, nlayer)
      CALL pack_vec(state%state_limit_wall, flat, idx, nlayer)
      CALL pack_vec(state%wet_thresh_roof, flat, idx, nlayer)
      CALL pack_vec(state%wet_thresh_wall, flat, idx, nlayer)
      CALL pack_vec(state%tin_roof, flat, idx, nlayer)
      CALL pack_vec(state%tin_wall, flat, idx, nlayer)
      CALL pack_vec(state%tin_surf, flat, idx, nlayer)

      CALL pack_mat(state%k_roof, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%k_wall, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%k_surf, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%cp_roof, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%cp_wall, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%cp_surf, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%dz_roof, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%dz_wall, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%dz_surf, flat, idx, nlayer, ndepth)

      err = SUEWS_CAPI_OK

   END SUBROUTINE ehc_prm_pack


   SUBROUTINE suews_ehc_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_ehc_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_ehc_prm_error_message

END MODULE module_c_api_ehc_prm

MODULE c_api_ehc_prm_module
   USE module_c_api_ehc_prm
END MODULE c_api_ehc_prm_module
