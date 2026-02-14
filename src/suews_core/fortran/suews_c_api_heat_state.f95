! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for HEAT_STATE.
! -----------------------------------------------------------------------------
MODULE module_c_api_heat_state
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text
   USE module_ctrl_const_allocate, ONLY: nsurf

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_HEAT_STATE_BASE_LEN = 7_c_int*INT(nsurf, c_int) + 79_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_HEAT_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: heat_state_shadow
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: temp_roof
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: temp_wall
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: temp_surf
      REAL(c_double), DIMENSION(:, :), ALLOCATABLE :: temp_surf_dyohm
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tsfc_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tsfc_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tsfc_surf
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tsfc_surf_dyohm
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tsfc_roof_stepstart
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tsfc_wall_stepstart
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: tsfc_surf_stepstart

      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qs_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qn_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qe_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qh_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qh_resist_roof

      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qs_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qn_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qe_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qh_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: qh_resist_wall

      REAL(c_double), DIMENSION(nsurf) :: qs_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: qn_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: qe0_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: qe_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: qh_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: qh_resist_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: tsurf_ind = 0.0_c_double

      REAL(c_double) :: qh_lumps = 0.0_c_double
      REAL(c_double) :: qe_lumps = 0.0_c_double
      REAL(c_double) :: kclear = 0.0_c_double
      REAL(c_double) :: kup = 0.0_c_double
      REAL(c_double) :: ldown = 0.0_c_double
      REAL(c_double) :: lup = 0.0_c_double
      REAL(c_double) :: qe = 0.0_c_double
      REAL(c_double) :: qf = 0.0_c_double
      REAL(c_double) :: qf_sahp = 0.0_c_double
      REAL(c_double) :: qh = 0.0_c_double
      REAL(c_double) :: qh_residual = 0.0_c_double
      REAL(c_double) :: qh_resist = 0.0_c_double
      REAL(c_double) :: qn = 0.0_c_double
      REAL(c_double) :: qn_snowfree = 0.0_c_double
      REAL(c_double) :: qs = 0.0_c_double
      REAL(c_double) :: tsfc_c = 0.0_c_double
      REAL(c_double) :: tsurf = 0.0_c_double
      REAL(c_double) :: qh_init = 0.0_c_double

      REAL(c_double), DIMENSION(15) :: roof_in_sw_spc = 0.0_c_double
      REAL(c_double), DIMENSION(15) :: roof_in_lw_spc = 0.0_c_double
      REAL(c_double), DIMENSION(15) :: wall_in_sw_spc = 0.0_c_double
      REAL(c_double), DIMENSION(15) :: wall_in_lw_spc = 0.0_c_double

      LOGICAL :: iter_safe = .TRUE.
   END TYPE heat_state_shadow

   PUBLIC :: suews_heat_state_len
   PUBLIC :: suews_heat_state_schema_version
   PUBLIC :: suews_heat_state_default
   PUBLIC :: suews_heat_state_error_message

CONTAINS

   SUBROUTINE suews_heat_state_len(n_flat, nlayer, ndepth, err) BIND(C, name='suews_heat_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: ndepth
      INTEGER(c_int), INTENT(out) :: err

      TYPE(heat_state_shadow) :: state

      CALL heat_state_layout(state, n_flat, nlayer, ndepth, err)

   END SUBROUTINE suews_heat_state_len


   SUBROUTINE suews_heat_state_schema_version(schema_version, err) BIND(C, name='suews_heat_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_HEAT_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_heat_state_schema_version


   SUBROUTINE suews_heat_state_default(flat, n_flat, nlayer, ndepth, err) BIND(C, name='suews_heat_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: ndepth
      INTEGER(c_int), INTENT(out) :: err

      TYPE(heat_state_shadow) :: state

      CALL heat_state_pack(state, flat, n_flat, nlayer, ndepth, err)

   END SUBROUTINE suews_heat_state_default


   SUBROUTINE update_nlayer_from_vec(field, nlayer, err)
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

   END SUBROUTINE update_nlayer_from_vec


   SUBROUTINE update_layer_depth_from_mat(field, nlayer, ndepth, err)
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

   END SUBROUTINE update_layer_depth_from_mat


   SUBROUTINE update_depth_from_surf_mat(field, ndepth, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(inout) :: ndepth
      INTEGER(c_int), INTENT(inout) :: err
      INTEGER(c_int) :: surf_here
      INTEGER(c_int) :: depth_here

      IF (err /= SUEWS_CAPI_OK) RETURN
      IF (.NOT. ALLOCATED(field)) RETURN

      surf_here = INT(SIZE(field, 1), c_int)
      depth_here = INT(SIZE(field, 2), c_int)
      IF (surf_here /= INT(nsurf, c_int)) THEN
         err = SUEWS_CAPI_BAD_STATE
         RETURN
      END IF

      IF (ndepth == 0_c_int) THEN
         ndepth = depth_here
      ELSEIF (ndepth /= depth_here) THEN
         err = SUEWS_CAPI_BAD_STATE
      END IF

   END SUBROUTINE update_depth_from_surf_mat


   SUBROUTINE require_layer_vec(field, nlayer, ndepth, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int), INTENT(in) :: ndepth
      INTEGER(c_int), INTENT(inout) :: err

      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (nlayer == 0_c_int .AND. ndepth == 0_c_int) THEN
         IF (ALLOCATED(field)) err = SUEWS_CAPI_BAD_STATE
      ELSE
         IF (.NOT. ALLOCATED(field)) THEN
            err = SUEWS_CAPI_BAD_STATE
         ELSEIF (INT(SIZE(field), c_int) /= nlayer) THEN
            err = SUEWS_CAPI_BAD_STATE
         END IF
      END IF

   END SUBROUTINE require_layer_vec


   SUBROUTINE require_surf_vec(field, nlayer, ndepth, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int), INTENT(in) :: ndepth
      INTEGER(c_int), INTENT(inout) :: err

      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (nlayer == 0_c_int .AND. ndepth == 0_c_int) THEN
         IF (ALLOCATED(field)) err = SUEWS_CAPI_BAD_STATE
      ELSE
         IF (.NOT. ALLOCATED(field)) THEN
            err = SUEWS_CAPI_BAD_STATE
         ELSEIF (INT(SIZE(field), c_int) /= INT(nsurf, c_int)) THEN
            err = SUEWS_CAPI_BAD_STATE
         END IF
      END IF

   END SUBROUTINE require_surf_vec


   SUBROUTINE require_layer_depth_mat(field, nlayer, ndepth, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int), INTENT(in) :: ndepth
      INTEGER(c_int), INTENT(inout) :: err

      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (nlayer == 0_c_int .AND. ndepth == 0_c_int) THEN
         IF (ALLOCATED(field)) err = SUEWS_CAPI_BAD_STATE
      ELSE
         IF (.NOT. ALLOCATED(field)) THEN
            err = SUEWS_CAPI_BAD_STATE
         ELSEIF (INT(SIZE(field, 1), c_int) /= nlayer .OR. &
                 INT(SIZE(field, 2), c_int) /= ndepth) THEN
            err = SUEWS_CAPI_BAD_STATE
         END IF
      END IF

   END SUBROUTINE require_layer_depth_mat


   SUBROUTINE require_surf_depth_mat(field, nlayer, ndepth, err)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      INTEGER(c_int), INTENT(in) :: nlayer
      INTEGER(c_int), INTENT(in) :: ndepth
      INTEGER(c_int), INTENT(inout) :: err

      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (nlayer == 0_c_int .AND. ndepth == 0_c_int) THEN
         IF (ALLOCATED(field)) err = SUEWS_CAPI_BAD_STATE
      ELSE
         IF (.NOT. ALLOCATED(field)) THEN
            err = SUEWS_CAPI_BAD_STATE
         ELSEIF (INT(SIZE(field, 1), c_int) /= INT(nsurf, c_int) .OR. &
                 INT(SIZE(field, 2), c_int) /= ndepth) THEN
            err = SUEWS_CAPI_BAD_STATE
         END IF
      END IF

   END SUBROUTINE require_surf_depth_mat


   SUBROUTINE heat_state_layout(state, n_flat, nlayer, ndepth, err)
      IMPLICIT NONE

      TYPE(heat_state_shadow), INTENT(in) :: state
      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: ndepth
      INTEGER(c_int), INTENT(out) :: err

      nlayer = 0_c_int
      ndepth = 0_c_int
      err = SUEWS_CAPI_OK

      CALL update_nlayer_from_vec(state%tsfc_roof, nlayer, err)
      CALL update_nlayer_from_vec(state%tsfc_wall, nlayer, err)
      CALL update_nlayer_from_vec(state%tsfc_roof_stepstart, nlayer, err)
      CALL update_nlayer_from_vec(state%tsfc_wall_stepstart, nlayer, err)
      CALL update_nlayer_from_vec(state%qs_roof, nlayer, err)
      CALL update_nlayer_from_vec(state%qn_roof, nlayer, err)
      CALL update_nlayer_from_vec(state%qe_roof, nlayer, err)
      CALL update_nlayer_from_vec(state%qh_roof, nlayer, err)
      CALL update_nlayer_from_vec(state%qh_resist_roof, nlayer, err)
      CALL update_nlayer_from_vec(state%qs_wall, nlayer, err)
      CALL update_nlayer_from_vec(state%qn_wall, nlayer, err)
      CALL update_nlayer_from_vec(state%qe_wall, nlayer, err)
      CALL update_nlayer_from_vec(state%qh_wall, nlayer, err)
      CALL update_nlayer_from_vec(state%qh_resist_wall, nlayer, err)

      CALL update_layer_depth_from_mat(state%temp_roof, nlayer, ndepth, err)
      CALL update_layer_depth_from_mat(state%temp_wall, nlayer, ndepth, err)
      CALL update_depth_from_surf_mat(state%temp_surf, ndepth, err)
      CALL update_depth_from_surf_mat(state%temp_surf_dyohm, ndepth, err)

      IF ((nlayer == 0_c_int .AND. ndepth /= 0_c_int) .OR. &
          (nlayer /= 0_c_int .AND. ndepth == 0_c_int)) THEN
         err = SUEWS_CAPI_BAD_STATE
      END IF

      CALL require_layer_depth_mat(state%temp_roof, nlayer, ndepth, err)
      CALL require_layer_depth_mat(state%temp_wall, nlayer, ndepth, err)
      CALL require_surf_depth_mat(state%temp_surf, nlayer, ndepth, err)
      CALL require_surf_depth_mat(state%temp_surf_dyohm, nlayer, ndepth, err)

      CALL require_layer_vec(state%tsfc_roof, nlayer, ndepth, err)
      CALL require_layer_vec(state%tsfc_wall, nlayer, ndepth, err)
      CALL require_surf_vec(state%tsfc_surf, nlayer, ndepth, err)
      CALL require_surf_vec(state%tsfc_surf_dyohm, nlayer, ndepth, err)
      CALL require_layer_vec(state%tsfc_roof_stepstart, nlayer, ndepth, err)
      CALL require_layer_vec(state%tsfc_wall_stepstart, nlayer, ndepth, err)
      CALL require_surf_vec(state%tsfc_surf_stepstart, nlayer, ndepth, err)

      CALL require_layer_vec(state%qs_roof, nlayer, ndepth, err)
      CALL require_layer_vec(state%qn_roof, nlayer, ndepth, err)
      CALL require_layer_vec(state%qe_roof, nlayer, ndepth, err)
      CALL require_layer_vec(state%qh_roof, nlayer, ndepth, err)
      CALL require_layer_vec(state%qh_resist_roof, nlayer, ndepth, err)

      CALL require_layer_vec(state%qs_wall, nlayer, ndepth, err)
      CALL require_layer_vec(state%qn_wall, nlayer, ndepth, err)
      CALL require_layer_vec(state%qe_wall, nlayer, ndepth, err)
      CALL require_layer_vec(state%qh_wall, nlayer, ndepth, err)
      CALL require_layer_vec(state%qh_resist_wall, nlayer, ndepth, err)

      IF (err /= SUEWS_CAPI_OK) THEN
         n_flat = 0_c_int
         RETURN
      END IF

      IF (nlayer == 0_c_int) THEN
         n_flat = SUEWS_CAPI_HEAT_STATE_BASE_LEN
      ELSE
         n_flat = SUEWS_CAPI_HEAT_STATE_BASE_LEN + &
                  2_c_int*nlayer*ndepth + 2_c_int*INT(nsurf, c_int)*ndepth + &
                  14_c_int*nlayer + 3_c_int*INT(nsurf, c_int)
      END IF
      err = SUEWS_CAPI_OK

   END SUBROUTINE heat_state_layout


   SUBROUTINE pack_vec(field, flat, idx, n)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:), ALLOCATABLE, INTENT(in) :: field
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(inout) :: idx
      INTEGER(c_int), INTENT(in) :: n
      INTEGER(c_int) :: i

      IF (n <= 0_c_int) RETURN
      DO i = 1_c_int, n
         flat(idx) = field(i)
         idx = idx + 1_c_int
      END DO

   END SUBROUTINE pack_vec


   SUBROUTINE pack_mat(field, flat, idx, n1, n2)
      IMPLICIT NONE

      REAL(c_double), DIMENSION(:, :), ALLOCATABLE, INTENT(in) :: field
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(inout) :: idx
      INTEGER(c_int), INTENT(in) :: n1
      INTEGER(c_int), INTENT(in) :: n2
      INTEGER(c_int) :: i
      INTEGER(c_int) :: j

      IF (n1 <= 0_c_int .OR. n2 <= 0_c_int) RETURN
      DO i = 1_c_int, n1
         DO j = 1_c_int, n2
            flat(idx) = field(i, j)
            idx = idx + 1_c_int
         END DO
      END DO

   END SUBROUTINE pack_mat


   SUBROUTINE heat_state_pack(state, flat, n_flat, nlayer, ndepth, err)
      IMPLICIT NONE

      TYPE(heat_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: nlayer
      INTEGER(c_int), INTENT(out) :: ndepth
      INTEGER(c_int), INTENT(out) :: err
      INTEGER(c_int) :: n_expected
      INTEGER(c_int) :: idx
      INTEGER(c_int) :: surf_vec_len
      INTEGER(c_int) :: i

      CALL heat_state_layout(state, n_expected, nlayer, ndepth, err)
      IF (err /= SUEWS_CAPI_OK) RETURN

      IF (n_flat < n_expected) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      surf_vec_len = 0_c_int
      IF (nlayer > 0_c_int) surf_vec_len = INT(nsurf, c_int)

      idx = 1_c_int

      CALL pack_mat(state%temp_roof, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%temp_wall, flat, idx, nlayer, ndepth)
      CALL pack_mat(state%temp_surf, flat, idx, INT(nsurf, c_int), ndepth)
      CALL pack_mat(state%temp_surf_dyohm, flat, idx, INT(nsurf, c_int), ndepth)

      CALL pack_vec(state%tsfc_roof, flat, idx, nlayer)
      CALL pack_vec(state%tsfc_wall, flat, idx, nlayer)
      CALL pack_vec(state%tsfc_surf, flat, idx, surf_vec_len)
      CALL pack_vec(state%tsfc_surf_dyohm, flat, idx, surf_vec_len)
      CALL pack_vec(state%tsfc_roof_stepstart, flat, idx, nlayer)
      CALL pack_vec(state%tsfc_wall_stepstart, flat, idx, nlayer)
      CALL pack_vec(state%tsfc_surf_stepstart, flat, idx, surf_vec_len)

      CALL pack_vec(state%qs_roof, flat, idx, nlayer)
      CALL pack_vec(state%qn_roof, flat, idx, nlayer)
      CALL pack_vec(state%qe_roof, flat, idx, nlayer)
      CALL pack_vec(state%qh_roof, flat, idx, nlayer)
      CALL pack_vec(state%qh_resist_roof, flat, idx, nlayer)

      CALL pack_vec(state%qs_wall, flat, idx, nlayer)
      CALL pack_vec(state%qn_wall, flat, idx, nlayer)
      CALL pack_vec(state%qe_wall, flat, idx, nlayer)
      CALL pack_vec(state%qh_wall, flat, idx, nlayer)
      CALL pack_vec(state%qh_resist_wall, flat, idx, nlayer)

      DO i = 1, nsurf
         flat(idx) = state%qs_surf(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, nsurf
         flat(idx) = state%qn_surf(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, nsurf
         flat(idx) = state%qe0_surf(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, nsurf
         flat(idx) = state%qe_surf(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, nsurf
         flat(idx) = state%qh_surf(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, nsurf
         flat(idx) = state%qh_resist_surf(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, nsurf
         flat(idx) = state%tsurf_ind(i)
         idx = idx + 1_c_int
      END DO

      flat(idx) = state%qh_lumps; idx = idx + 1_c_int
      flat(idx) = state%qe_lumps; idx = idx + 1_c_int
      flat(idx) = state%kclear; idx = idx + 1_c_int
      flat(idx) = state%kup; idx = idx + 1_c_int
      flat(idx) = state%ldown; idx = idx + 1_c_int
      flat(idx) = state%lup; idx = idx + 1_c_int
      flat(idx) = state%qe; idx = idx + 1_c_int
      flat(idx) = state%qf; idx = idx + 1_c_int
      flat(idx) = state%qf_sahp; idx = idx + 1_c_int
      flat(idx) = state%qh; idx = idx + 1_c_int
      flat(idx) = state%qh_residual; idx = idx + 1_c_int
      flat(idx) = state%qh_resist; idx = idx + 1_c_int
      flat(idx) = state%qn; idx = idx + 1_c_int
      flat(idx) = state%qn_snowfree; idx = idx + 1_c_int
      flat(idx) = state%qs; idx = idx + 1_c_int
      flat(idx) = state%tsfc_c; idx = idx + 1_c_int
      flat(idx) = state%tsurf; idx = idx + 1_c_int
      flat(idx) = state%qh_init; idx = idx + 1_c_int

      DO i = 1, 15
         flat(idx) = state%roof_in_sw_spc(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, 15
         flat(idx) = state%roof_in_lw_spc(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, 15
         flat(idx) = state%wall_in_sw_spc(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1, 15
         flat(idx) = state%wall_in_lw_spc(i)
         idx = idx + 1_c_int
      END DO

      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE heat_state_pack


   SUBROUTINE suews_heat_state_error_message(code, buffer, buffer_len) BIND(C, name='suews_heat_state_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_heat_state_error_message

END MODULE module_c_api_heat_state

MODULE c_api_heat_state_module
   USE module_c_api_heat_state
END MODULE c_api_heat_state_module
