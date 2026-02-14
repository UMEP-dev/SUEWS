! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for STEBBS_STATE.
! -----------------------------------------------------------------------------
MODULE module_c_api_stebbs_state
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_STEBBS_STATE_RSL_LEN = 30_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_STEBBS_STATE_LEN = 154_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_STEBBS_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: stebbs_state_shadow
      REAL(c_double) :: kdown2d = 0.0_c_double
      REAL(c_double) :: kup2d = 0.0_c_double
      REAL(c_double) :: kwest = 0.0_c_double
      REAL(c_double) :: ksouth = 0.0_c_double
      REAL(c_double) :: knorth = 0.0_c_double
      REAL(c_double) :: keast = 0.0_c_double
      REAL(c_double) :: ldown2d = 0.0_c_double
      REAL(c_double) :: lup2d = 0.0_c_double
      REAL(c_double) :: lwest = 0.0_c_double
      REAL(c_double) :: lsouth = 0.0_c_double
      REAL(c_double) :: lnorth = 0.0_c_double
      REAL(c_double) :: least = 0.0_c_double
      REAL(c_double), DIMENSION(30) :: zarray = -999.0_c_double
      REAL(c_double), DIMENSION(30) :: dataout_line_ursl = -999.0_c_double
      REAL(c_double), DIMENSION(30) :: dataout_line_trsl = -999.0_c_double
      REAL(c_double), DIMENSION(30) :: dataout_line_qrsl = -999.0_c_double
      REAL(c_double) :: deep_soil_temperature = 0.0_c_double
      REAL(c_double) :: outdoor_air_start_temperature = 0.0_c_double
      REAL(c_double) :: indoor_air_start_temperature = 0.0_c_double
      REAL(c_double) :: indoor_mass_start_temperature = 0.0_c_double
      REAL(c_double) :: wall_indoor_surface_temperature = 0.0_c_double
      REAL(c_double) :: wall_outdoor_surface_temperature = 0.0_c_double
      REAL(c_double) :: roof_indoor_surface_temperature = 0.0_c_double
      REAL(c_double) :: roof_outdoor_surface_temperature = 0.0_c_double
      REAL(c_double) :: window_indoor_surface_temperature = 0.0_c_double
      REAL(c_double) :: window_outdoor_surface_temperature = 0.0_c_double
      REAL(c_double) :: ground_floor_indoor_surface_temperature = 0.0_c_double
      REAL(c_double) :: ground_floor_outdoor_surface_temperature = 0.0_c_double
      REAL(c_double) :: water_tank_temperature = 0.0_c_double
      REAL(c_double) :: internal_wall_water_tank_temperature = 0.0_c_double
      REAL(c_double) :: external_wall_water_tank_temperature = 0.0_c_double
      REAL(c_double) :: mains_water_temperature = 0.0_c_double
      REAL(c_double) :: domestic_hot_water_temperature_in_use_in_building = 0.0_c_double
      REAL(c_double) :: internal_wall_dhw_vessel_temperature = 0.0_c_double
      REAL(c_double) :: external_wall_dhw_vessel_temperature = 0.0_c_double
      REAL(c_double) :: qs_stebbs = 0.0_c_double
      INTEGER(c_int) :: buildings_count = 0_c_int
      LOGICAL :: iter_safe = .FALSE.
   END TYPE stebbs_state_shadow

   PUBLIC :: suews_stebbs_state_len
   PUBLIC :: suews_stebbs_state_schema_version
   PUBLIC :: suews_stebbs_state_default
   PUBLIC :: suews_stebbs_state_error_message

CONTAINS

   SUBROUTINE suews_stebbs_state_len(n_flat, err) BIND(C, name='suews_stebbs_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_STEBBS_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_stebbs_state_len


   SUBROUTINE suews_stebbs_state_schema_version(schema_version, err) BIND(C, name='suews_stebbs_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_STEBBS_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_stebbs_state_schema_version


   SUBROUTINE suews_stebbs_state_default(flat, n_flat, err) BIND(C, name='suews_stebbs_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(stebbs_state_shadow) :: state

      CALL stebbs_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_stebbs_state_default


   SUBROUTINE stebbs_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(stebbs_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: idx
      INTEGER(c_int) :: i

      IF (n_flat < SUEWS_CAPI_STEBBS_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1_c_int

      flat(idx) = state%kdown2d; idx = idx + 1_c_int
      flat(idx) = state%kup2d; idx = idx + 1_c_int
      flat(idx) = state%kwest; idx = idx + 1_c_int
      flat(idx) = state%ksouth; idx = idx + 1_c_int
      flat(idx) = state%knorth; idx = idx + 1_c_int
      flat(idx) = state%keast; idx = idx + 1_c_int
      flat(idx) = state%ldown2d; idx = idx + 1_c_int
      flat(idx) = state%lup2d; idx = idx + 1_c_int
      flat(idx) = state%lwest; idx = idx + 1_c_int
      flat(idx) = state%lsouth; idx = idx + 1_c_int
      flat(idx) = state%lnorth; idx = idx + 1_c_int
      flat(idx) = state%least; idx = idx + 1_c_int

      DO i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
         flat(idx) = state%zarray(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
         flat(idx) = state%dataout_line_ursl(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
         flat(idx) = state%dataout_line_trsl(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
         flat(idx) = state%dataout_line_qrsl(i)
         idx = idx + 1_c_int
      END DO

      flat(idx) = state%deep_soil_temperature; idx = idx + 1_c_int
      flat(idx) = state%outdoor_air_start_temperature; idx = idx + 1_c_int
      flat(idx) = state%indoor_air_start_temperature; idx = idx + 1_c_int
      flat(idx) = state%indoor_mass_start_temperature; idx = idx + 1_c_int
      flat(idx) = state%wall_indoor_surface_temperature; idx = idx + 1_c_int
      flat(idx) = state%wall_outdoor_surface_temperature; idx = idx + 1_c_int
      flat(idx) = state%roof_indoor_surface_temperature; idx = idx + 1_c_int
      flat(idx) = state%roof_outdoor_surface_temperature; idx = idx + 1_c_int
      flat(idx) = state%window_indoor_surface_temperature; idx = idx + 1_c_int
      flat(idx) = state%window_outdoor_surface_temperature; idx = idx + 1_c_int
      flat(idx) = state%ground_floor_indoor_surface_temperature; idx = idx + 1_c_int
      flat(idx) = state%ground_floor_outdoor_surface_temperature; idx = idx + 1_c_int
      flat(idx) = state%water_tank_temperature; idx = idx + 1_c_int
      flat(idx) = state%internal_wall_water_tank_temperature; idx = idx + 1_c_int
      flat(idx) = state%external_wall_water_tank_temperature; idx = idx + 1_c_int
      flat(idx) = state%mains_water_temperature; idx = idx + 1_c_int
      flat(idx) = state%domestic_hot_water_temperature_in_use_in_building; idx = idx + 1_c_int
      flat(idx) = state%internal_wall_dhw_vessel_temperature; idx = idx + 1_c_int
      flat(idx) = state%external_wall_dhw_vessel_temperature; idx = idx + 1_c_int
      flat(idx) = state%qs_stebbs; idx = idx + 1_c_int
      flat(idx) = REAL(state%buildings_count, c_double); idx = idx + 1_c_int
      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE stebbs_state_pack


   SUBROUTINE suews_stebbs_state_error_message(code, buffer, buffer_len) BIND(C, name='suews_stebbs_state_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_stebbs_state_error_message

END MODULE module_c_api_stebbs_state

MODULE c_api_stebbs_state_module
   USE module_c_api_stebbs_state
END MODULE c_api_stebbs_state_module
