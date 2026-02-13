! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for STEBBS_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_stebbs_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_STEBBS_PRM_PROFILE_STEPS = 144_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_STEBBS_PRM_PROFILE_GROUPS = 2_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_STEBBS_PRM_LEN = 333_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_STEBBS_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: stebbs_prm_shadow
      REAL(c_double) :: wall_internal_convection_coefficient = 0.0_c_double
      REAL(c_double) :: roof_internal_convection_coefficient = 0.0_c_double
      REAL(c_double) :: internal_mass_convection_coefficient = 0.0_c_double
      REAL(c_double) :: floor_internal_convection_coefficient = 0.0_c_double
      REAL(c_double) :: window_internal_convection_coefficient = 0.0_c_double
      REAL(c_double) :: wall_external_convection_coefficient = 0.0_c_double
      REAL(c_double) :: roof_external_convection_coefficient = 0.0_c_double
      REAL(c_double) :: window_external_convection_coefficient = 0.0_c_double
      REAL(c_double) :: ground_depth = 0.0_c_double
      REAL(c_double) :: external_ground_conductivity = 0.0_c_double
      REAL(c_double) :: indoor_air_density = 0.0_c_double
      REAL(c_double) :: indoor_air_cp = 0.0_c_double
      REAL(c_double) :: metabolism_threshold = 0.0_c_double
      REAL(c_double) :: latent_sensible_ratio = 0.0_c_double
      REAL(c_double) :: heating_system_efficiency = 0.0_c_double
      REAL(c_double) :: max_cooling_power = 0.0_c_double
      REAL(c_double) :: cooling_system_cop = 0.0_c_double
      REAL(c_double) :: ventilation_rate = 0.0_c_double
      REAL(c_double) :: water_tank_wall_thickness = 0.0_c_double
      REAL(c_double) :: water_tank_surface_area = 0.0_c_double
      REAL(c_double) :: hot_water_heating_setpoint_temperature = 0.0_c_double
      REAL(c_double) :: hot_water_tank_wall_emissivity = 0.0_c_double
      REAL(c_double) :: dhw_vessel_wall_thickness = 0.0_c_double
      REAL(c_double) :: dhw_water_volume = 0.0_c_double
      REAL(c_double) :: dhw_surface_area = 0.0_c_double
      REAL(c_double) :: hot_water_flow_rate = 0.0_c_double
      REAL(c_double), DIMENSION(0:143, 2) :: hot_water_flow_profile = 0.0_c_double
      REAL(c_double) :: dhw_specific_heat_capacity = 0.0_c_double
      REAL(c_double) :: hot_water_tank_specific_heat_capacity = 0.0_c_double
      REAL(c_double) :: dhw_vessel_specific_heat_capacity = 0.0_c_double
      REAL(c_double) :: dhw_density = 0.0_c_double
      REAL(c_double) :: hot_water_tank_wall_density = 0.0_c_double
      REAL(c_double) :: dhw_vessel_density = 0.0_c_double
      REAL(c_double) :: hot_water_tank_building_wall_view_factor = 0.0_c_double
      REAL(c_double) :: hot_water_tank_internal_mass_view_factor = 0.0_c_double
      REAL(c_double) :: hot_water_tank_wall_conductivity = 0.0_c_double
      REAL(c_double) :: hot_water_tank_internal_wall_convection_coefficient = 0.0_c_double
      REAL(c_double) :: hot_water_tank_external_wall_convection_coefficient = 0.0_c_double
      REAL(c_double) :: dhw_vessel_wall_conductivity = 0.0_c_double
      REAL(c_double) :: dhw_vessel_internal_wall_convection_coefficient = 0.0_c_double
      REAL(c_double) :: dhw_vessel_external_wall_convection_coefficient = 0.0_c_double
      REAL(c_double) :: dhw_vessel_wall_emissivity = 0.0_c_double
      REAL(c_double) :: hot_water_heating_efficiency = 0.0_c_double
      REAL(c_double) :: minimum_volume_of_dhw_in_use = 0.0_c_double
      REAL(c_double) :: maximum_volume_of_dhw_in_use = 0.0_c_double
      LOGICAL :: iter_safe = .TRUE.
   END TYPE stebbs_prm_shadow

   PUBLIC :: suews_stebbs_prm_len
   PUBLIC :: suews_stebbs_prm_schema_version
   PUBLIC :: suews_stebbs_prm_default
   PUBLIC :: suews_stebbs_prm_error_message

CONTAINS

   SUBROUTINE suews_stebbs_prm_len(n_flat, err) BIND(C, name='suews_stebbs_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_STEBBS_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_stebbs_prm_len


   SUBROUTINE suews_stebbs_prm_schema_version(schema_version, err) BIND(C, name='suews_stebbs_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_STEBBS_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_stebbs_prm_schema_version


   SUBROUTINE suews_stebbs_prm_default(flat, n_flat, err) BIND(C, name='suews_stebbs_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(stebbs_prm_shadow) :: state

      CALL stebbs_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_stebbs_prm_default


   SUBROUTINE stebbs_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(stebbs_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: idx
      INTEGER(c_int) :: i
      INTEGER(c_int) :: j

      IF (n_flat < SUEWS_CAPI_STEBBS_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1_c_int

      flat(idx) = state%wall_internal_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%roof_internal_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%internal_mass_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%floor_internal_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%window_internal_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%wall_external_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%roof_external_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%window_external_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%ground_depth; idx = idx + 1_c_int
      flat(idx) = state%external_ground_conductivity; idx = idx + 1_c_int
      flat(idx) = state%indoor_air_density; idx = idx + 1_c_int
      flat(idx) = state%indoor_air_cp; idx = idx + 1_c_int
      flat(idx) = state%metabolism_threshold; idx = idx + 1_c_int
      flat(idx) = state%latent_sensible_ratio; idx = idx + 1_c_int
      flat(idx) = state%heating_system_efficiency; idx = idx + 1_c_int
      flat(idx) = state%max_cooling_power; idx = idx + 1_c_int
      flat(idx) = state%cooling_system_cop; idx = idx + 1_c_int
      flat(idx) = state%ventilation_rate; idx = idx + 1_c_int
      flat(idx) = state%water_tank_wall_thickness; idx = idx + 1_c_int
      flat(idx) = state%water_tank_surface_area; idx = idx + 1_c_int
      flat(idx) = state%hot_water_heating_setpoint_temperature; idx = idx + 1_c_int
      flat(idx) = state%hot_water_tank_wall_emissivity; idx = idx + 1_c_int
      flat(idx) = state%dhw_vessel_wall_thickness; idx = idx + 1_c_int
      flat(idx) = state%dhw_water_volume; idx = idx + 1_c_int
      flat(idx) = state%dhw_surface_area; idx = idx + 1_c_int
      flat(idx) = state%hot_water_flow_rate; idx = idx + 1_c_int

      DO j = 1_c_int, SUEWS_CAPI_STEBBS_PRM_PROFILE_GROUPS
         DO i = 0_c_int, SUEWS_CAPI_STEBBS_PRM_PROFILE_STEPS - 1_c_int
            flat(idx) = state%hot_water_flow_profile(i, j)
            idx = idx + 1_c_int
         END DO
      END DO

      flat(idx) = state%dhw_specific_heat_capacity; idx = idx + 1_c_int
      flat(idx) = state%hot_water_tank_specific_heat_capacity; idx = idx + 1_c_int
      flat(idx) = state%dhw_vessel_specific_heat_capacity; idx = idx + 1_c_int
      flat(idx) = state%dhw_density; idx = idx + 1_c_int
      flat(idx) = state%hot_water_tank_wall_density; idx = idx + 1_c_int
      flat(idx) = state%dhw_vessel_density; idx = idx + 1_c_int
      flat(idx) = state%hot_water_tank_building_wall_view_factor; idx = idx + 1_c_int
      flat(idx) = state%hot_water_tank_internal_mass_view_factor; idx = idx + 1_c_int
      flat(idx) = state%hot_water_tank_wall_conductivity; idx = idx + 1_c_int
      flat(idx) = state%hot_water_tank_internal_wall_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%hot_water_tank_external_wall_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%dhw_vessel_wall_conductivity; idx = idx + 1_c_int
      flat(idx) = state%dhw_vessel_internal_wall_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%dhw_vessel_external_wall_convection_coefficient; idx = idx + 1_c_int
      flat(idx) = state%dhw_vessel_wall_emissivity; idx = idx + 1_c_int
      flat(idx) = state%hot_water_heating_efficiency; idx = idx + 1_c_int
      flat(idx) = state%minimum_volume_of_dhw_in_use; idx = idx + 1_c_int
      flat(idx) = state%maximum_volume_of_dhw_in_use; idx = idx + 1_c_int
      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE stebbs_prm_pack


   SUBROUTINE suews_stebbs_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_stebbs_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_stebbs_prm_error_message

END MODULE module_c_api_stebbs_prm

MODULE c_api_stebbs_prm_module
   USE module_c_api_stebbs_prm
END MODULE c_api_stebbs_prm_module
