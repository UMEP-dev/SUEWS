! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for HYDRO_STATE.
! -----------------------------------------------------------------------------
MODULE module_c_api_hydro_state
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

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_HYDRO_STATE_BASE_LEN = 10_c_int*INT(nsurf, c_int) + 34_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_HYDRO_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: hydro_state_shadow
      REAL(c_double), DIMENSION(nsurf) :: soilstore_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: state_surf = 0.0_c_double
      REAL(c_double), DIMENSION(9) :: wuday_id = 0.0_c_double

      REAL(c_double), DIMENSION(:), ALLOCATABLE :: soilstore_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: state_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: soilstore_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: state_wall
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: ev_roof
      REAL(c_double), DIMENSION(:), ALLOCATABLE :: ev_wall

      REAL(c_double), DIMENSION(nsurf) :: ev0_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: ev_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: wu_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: runoff_soil = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: smd_surf = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: drain_surf = 0.0_c_double

      REAL(c_double) :: drain_per_tstep = 0.0_c_double
      REAL(c_double) :: ev_per_tstep = 0.0_c_double
      REAL(c_double) :: wu_ext = 0.0_c_double
      REAL(c_double) :: wu_int = 0.0_c_double

      REAL(c_double) :: runoff_agveg = 0.0_c_double
      REAL(c_double) :: runoff_agimpervious = 0.0_c_double
      REAL(c_double) :: runoff_per_tstep = 0.0_c_double
      REAL(c_double) :: runoff_pipes = 0.0_c_double
      REAL(c_double) :: runoff_soil_per_tstep = 0.0_c_double
      REAL(c_double) :: runoff_waterbody = 0.0_c_double
      REAL(c_double) :: smd = 0.0_c_double
      REAL(c_double) :: soil_state = 0.0_c_double
      REAL(c_double) :: state_per_tstep = 0.0_c_double
      REAL(c_double) :: surf_chang_per_tstep = 0.0_c_double
      REAL(c_double) :: tot_chang_per_tstep = 0.0_c_double
      REAL(c_double) :: runoff_per_interval = 0.0_c_double
      REAL(c_double) :: nwstate_per_tstep = 0.0_c_double

      REAL(c_double) :: soil_moist_cap = 0.0_c_double
      REAL(c_double) :: vsmd = 0.0_c_double

      REAL(c_double) :: additional_water = 0.0_c_double
      REAL(c_double) :: add_impervious = 0.0_c_double
      REAL(c_double) :: add_pipes = 0.0_c_double
      REAL(c_double) :: add_veg = 0.0_c_double
      REAL(c_double) :: add_waterbody = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: add_water = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: frac_water2runoff = 0.0_c_double

      LOGICAL :: iter_safe = .FALSE.
   END TYPE hydro_state_shadow

   PUBLIC :: suews_hydro_state_len
   PUBLIC :: suews_hydro_state_schema_version
   PUBLIC :: suews_hydro_state_default
   PUBLIC :: suews_hydro_state_error_message

CONTAINS

   SUBROUTINE suews_hydro_state_len( &
      n_flat, &
      soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
      err) BIND(C, name='suews_hydro_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: soilstore_roof_len
      INTEGER(c_int), INTENT(out) :: state_roof_len
      INTEGER(c_int), INTENT(out) :: soilstore_wall_len
      INTEGER(c_int), INTENT(out) :: state_wall_len
      INTEGER(c_int), INTENT(out) :: ev_roof_len
      INTEGER(c_int), INTENT(out) :: ev_wall_len
      INTEGER(c_int), INTENT(out) :: err

      TYPE(hydro_state_shadow) :: state

      CALL hydro_state_layout( &
         state, n_flat, &
         soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
         err)

   END SUBROUTINE suews_hydro_state_len


   SUBROUTINE suews_hydro_state_schema_version(schema_version, err) BIND(C, name='suews_hydro_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_HYDRO_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_hydro_state_schema_version


   SUBROUTINE suews_hydro_state_default( &
      flat, n_flat, &
      soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
      err) BIND(C, name='suews_hydro_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: soilstore_roof_len
      INTEGER(c_int), INTENT(out) :: state_roof_len
      INTEGER(c_int), INTENT(out) :: soilstore_wall_len
      INTEGER(c_int), INTENT(out) :: state_wall_len
      INTEGER(c_int), INTENT(out) :: ev_roof_len
      INTEGER(c_int), INTENT(out) :: ev_wall_len
      INTEGER(c_int), INTENT(out) :: err

      TYPE(hydro_state_shadow) :: state

      CALL hydro_state_pack( &
         state, flat, n_flat, &
         soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
         err)

   END SUBROUTINE suews_hydro_state_default


   SUBROUTINE hydro_state_layout( &
      state, n_flat, &
      soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
      err)
      IMPLICIT NONE

      TYPE(hydro_state_shadow), INTENT(in) :: state
      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: soilstore_roof_len
      INTEGER(c_int), INTENT(out) :: state_roof_len
      INTEGER(c_int), INTENT(out) :: soilstore_wall_len
      INTEGER(c_int), INTENT(out) :: state_wall_len
      INTEGER(c_int), INTENT(out) :: ev_roof_len
      INTEGER(c_int), INTENT(out) :: ev_wall_len
      INTEGER(c_int), INTENT(out) :: err

      soilstore_roof_len = 0_c_int
      state_roof_len = 0_c_int
      soilstore_wall_len = 0_c_int
      state_wall_len = 0_c_int
      ev_roof_len = 0_c_int
      ev_wall_len = 0_c_int

      IF (ALLOCATED(state%soilstore_roof)) soilstore_roof_len = INT(SIZE(state%soilstore_roof), c_int)
      IF (ALLOCATED(state%state_roof)) state_roof_len = INT(SIZE(state%state_roof), c_int)
      IF (ALLOCATED(state%soilstore_wall)) soilstore_wall_len = INT(SIZE(state%soilstore_wall), c_int)
      IF (ALLOCATED(state%state_wall)) state_wall_len = INT(SIZE(state%state_wall), c_int)
      IF (ALLOCATED(state%ev_roof)) ev_roof_len = INT(SIZE(state%ev_roof), c_int)
      IF (ALLOCATED(state%ev_wall)) ev_wall_len = INT(SIZE(state%ev_wall), c_int)

      n_flat = SUEWS_CAPI_HYDRO_STATE_BASE_LEN + &
               soilstore_roof_len + state_roof_len + soilstore_wall_len + state_wall_len + ev_roof_len + ev_wall_len
      err = SUEWS_CAPI_OK

   END SUBROUTINE hydro_state_layout


   SUBROUTINE hydro_state_pack( &
      state, flat, n_flat, &
      soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
      err)
      IMPLICIT NONE

      TYPE(hydro_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: soilstore_roof_len
      INTEGER(c_int), INTENT(out) :: state_roof_len
      INTEGER(c_int), INTENT(out) :: soilstore_wall_len
      INTEGER(c_int), INTENT(out) :: state_wall_len
      INTEGER(c_int), INTENT(out) :: ev_roof_len
      INTEGER(c_int), INTENT(out) :: ev_wall_len
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: n_expected
      INTEGER :: idx
      INTEGER :: i

      CALL hydro_state_layout( &
         state, n_expected, &
         soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
         err)
      IF (err /= SUEWS_CAPI_OK) THEN
         RETURN
      END IF

      IF (n_flat < n_expected) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1

      DO i = 1, nsurf
         flat(idx) = state%soilstore_surf(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%state_surf(i)
         idx = idx + 1
      END DO

      DO i = 1, 9
         flat(idx) = state%wuday_id(i)
         idx = idx + 1
      END DO

      IF (soilstore_roof_len > 0_c_int) THEN
         DO i = 1, INT(soilstore_roof_len)
            flat(idx) = state%soilstore_roof(i)
            idx = idx + 1
         END DO
      END IF

      IF (state_roof_len > 0_c_int) THEN
         DO i = 1, INT(state_roof_len)
            flat(idx) = state%state_roof(i)
            idx = idx + 1
         END DO
      END IF

      IF (soilstore_wall_len > 0_c_int) THEN
         DO i = 1, INT(soilstore_wall_len)
            flat(idx) = state%soilstore_wall(i)
            idx = idx + 1
         END DO
      END IF

      IF (state_wall_len > 0_c_int) THEN
         DO i = 1, INT(state_wall_len)
            flat(idx) = state%state_wall(i)
            idx = idx + 1
         END DO
      END IF

      IF (ev_roof_len > 0_c_int) THEN
         DO i = 1, INT(ev_roof_len)
            flat(idx) = state%ev_roof(i)
            idx = idx + 1
         END DO
      END IF

      IF (ev_wall_len > 0_c_int) THEN
         DO i = 1, INT(ev_wall_len)
            flat(idx) = state%ev_wall(i)
            idx = idx + 1
         END DO
      END IF

      DO i = 1, nsurf
         flat(idx) = state%ev0_surf(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%ev_surf(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%wu_surf(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%runoff_soil(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%smd_surf(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%drain_surf(i)
         idx = idx + 1
      END DO

      flat(idx) = state%drain_per_tstep; idx = idx + 1
      flat(idx) = state%ev_per_tstep; idx = idx + 1
      flat(idx) = state%wu_ext; idx = idx + 1
      flat(idx) = state%wu_int; idx = idx + 1

      flat(idx) = state%runoff_agveg; idx = idx + 1
      flat(idx) = state%runoff_agimpervious; idx = idx + 1
      flat(idx) = state%runoff_per_tstep; idx = idx + 1
      flat(idx) = state%runoff_pipes; idx = idx + 1
      flat(idx) = state%runoff_soil_per_tstep; idx = idx + 1
      flat(idx) = state%runoff_waterbody; idx = idx + 1
      flat(idx) = state%smd; idx = idx + 1
      flat(idx) = state%soil_state; idx = idx + 1
      flat(idx) = state%state_per_tstep; idx = idx + 1
      flat(idx) = state%surf_chang_per_tstep; idx = idx + 1
      flat(idx) = state%tot_chang_per_tstep; idx = idx + 1
      flat(idx) = state%runoff_per_interval; idx = idx + 1
      flat(idx) = state%nwstate_per_tstep; idx = idx + 1

      flat(idx) = state%soil_moist_cap; idx = idx + 1
      flat(idx) = state%vsmd; idx = idx + 1

      flat(idx) = state%additional_water; idx = idx + 1
      flat(idx) = state%add_impervious; idx = idx + 1
      flat(idx) = state%add_pipes; idx = idx + 1
      flat(idx) = state%add_veg; idx = idx + 1
      flat(idx) = state%add_waterbody; idx = idx + 1

      DO i = 1, nsurf
         flat(idx) = state%add_water(i)
         idx = idx + 1
      END DO

      DO i = 1, nsurf
         flat(idx) = state%frac_water2runoff(i)
         idx = idx + 1
      END DO

      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE hydro_state_pack


   SUBROUTINE suews_hydro_state_error_message(code, buffer, buffer_len) BIND(C, name='suews_hydro_state_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_hydro_state_error_message

END MODULE module_c_api_hydro_state

MODULE c_api_hydro_state_module
   USE module_c_api_hydro_state
END MODULE c_api_hydro_state_module
