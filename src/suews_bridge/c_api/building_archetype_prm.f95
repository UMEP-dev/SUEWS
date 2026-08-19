! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for BUILDING_ARCHETYPE_PRM.
! -----------------------------------------------------------------------------
module module_c_api_building_archetype_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_stebbs, only: BUILDING_ARCHETYPE_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS = 144_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS = 2_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_LEN = 1217_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION = 4_c_int

type :: building_archetype_prm_shadow
   real(c_double) :: building_count = 0.0_c_double
   real(c_double) :: occupants_state = 0.0_c_double
   real(c_double) :: hhs0 = 0.0_c_double
   real(c_double) :: age_0_4 = 0.0_c_double
   real(c_double) :: age_5_11 = 0.0_c_double
   real(c_double) :: age_12_18 = 0.0_c_double
   real(c_double) :: age_19_64 = 0.0_c_double
   real(c_double) :: age_65plus = 0.0_c_double
   real(c_double) :: stebbs_height = 0.0_c_double
   real(c_double) :: footprint_area = 0.0_c_double
   real(c_double) :: wall_external_area = 0.0_c_double
   real(c_double) :: internal_volume_ratio = 0.0_c_double
   real(c_double) :: internal_mass_area = 0.0_c_double
   real(c_double) :: wwr = 0.0_c_double
   real(c_double) :: wall_thickness = 0.0_c_double
   real(c_double) :: wall_effective_conductivity = 0.0_c_double
   real(c_double) :: wall_density = 0.0_c_double
   real(c_double) :: wall_specific_heat_capacity = 0.0_c_double
   real(c_double) :: wall_external_thickness = 0.0_c_double
   real(c_double) :: wall_external_effective_conductivity = 0.0_c_double
   real(c_double) :: wall_external_density = 0.0_c_double
   real(c_double) :: wall_external_specific_heat_capacity = 0.0_c_double
   real(c_double) :: wall_outer_heat_capacity_fraction = 0.0_c_double
   real(c_double) :: wall_external_emissivity = 0.0_c_double
   real(c_double) :: wall_internal_emissivity = 0.0_c_double
   real(c_double) :: wall_transmissivity = 0.0_c_double
   real(c_double) :: wall_absorptivity = 0.0_c_double
   real(c_double) :: wall_reflectivity = 0.0_c_double
   real(c_double) :: roof_thickness = 0.0_c_double
   real(c_double) :: roof_effective_conductivity = 0.0_c_double
   real(c_double) :: roof_density = 0.0_c_double
   real(c_double) :: roof_specific_heat_capacity = 0.0_c_double
   real(c_double) :: roof_external_thickness = 0.0_c_double
   real(c_double) :: roof_external_effective_conductivity = 0.0_c_double
   real(c_double) :: roof_external_density = 0.0_c_double
   real(c_double) :: roof_external_specific_heat_capacity = 0.0_c_double
   real(c_double) :: roof_outer_heat_capacity_fraction = 0.0_c_double
   real(c_double) :: roof_external_emissivity = 0.0_c_double
   real(c_double) :: roof_internal_emissivity = 0.0_c_double
   real(c_double) :: roof_transmissivity = 0.0_c_double
   real(c_double) :: roof_absorptivity = 0.0_c_double
   real(c_double) :: roof_reflectivity = 0.0_c_double
   real(c_double) :: ground_floor_thickness = 0.0_c_double
   real(c_double) :: ground_floor_effective_conductivity = 0.0_c_double
   real(c_double) :: ground_floor_density = 0.0_c_double
   real(c_double) :: ground_floor_specific_heat_capacity = 0.0_c_double
   real(c_double) :: window_thickness = 0.0_c_double
   real(c_double) :: window_effective_conductivity = 0.0_c_double
   real(c_double) :: window_density = 0.0_c_double
   real(c_double) :: window_specific_heat_capacity = 0.0_c_double
   real(c_double) :: window_external_emissivity = 0.0_c_double
   real(c_double) :: window_internal_emissivity = 0.0_c_double
   real(c_double) :: window_transmissivity = 0.0_c_double
   real(c_double) :: window_absorptivity = 0.0_c_double
   real(c_double) :: window_reflectivity = 0.0_c_double
   real(c_double) :: internal_mass_density = 0.0_c_double
   real(c_double) :: internal_mass_specific_heat_capacity = 0.0_c_double
   real(c_double) :: internal_mass_emissivity = 0.0_c_double
   real(c_double) :: max_heating_power = 0.0_c_double
   real(c_double) :: hot_water_tank_volume = 0.0_c_double
   real(c_double) :: maximum_hot_water_heating_power = 0.0_c_double
   real(c_double) :: heating_setpoint_temperature = 0.0_c_double
   real(c_double) :: cooling_setpoint_temperature = 0.0_c_double
   real(c_double), dimension(0:143, 2) :: heating_setpoint_temperature_profile = 0.0_c_double
   real(c_double), dimension(0:143, 2) :: cooling_setpoint_temperature_profile = 0.0_c_double
   real(c_double), dimension(0:143, 2) :: metabolism_profile = 0.0_c_double
   real(c_double), dimension(0:143, 2) :: appliance_profile = 0.0_c_double
   real(c_double) :: lighting_power_density = 0.0_c_double
   logical :: iter_safe = .true.
end type building_archetype_prm_shadow

public :: suews_building_archetype_prm_len
public :: suews_building_archetype_prm_schema_version
public :: suews_building_archetype_prm_default
public :: suews_building_archetype_prm_error_message
public :: building_archetype_prm_unpack

contains

subroutine suews_building_archetype_prm_len(n_flat, err) bind(C, name='suews_building_archetype_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_building_archetype_prm_len

   SUBROUTINE suews_building_archetype_prm_schema_version(schema_version, err) BIND(C, name='suews_building_archetype_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_building_archetype_prm_schema_version

subroutine suews_building_archetype_prm_default(flat, n_flat, err) bind(C, name='suews_building_archetype_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(building_archetype_prm_shadow) :: state

   call building_archetype_prm_pack(state, flat, n_flat, err)

end subroutine suews_building_archetype_prm_default

subroutine building_archetype_prm_pack(state, flat, n_flat, err)
   implicit none

   type(building_archetype_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: j

   if (n_flat<SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int

   flat(idx) = state%building_count; idx = idx + 1_c_int
   flat(idx) = state%occupants_state; idx = idx + 1_c_int
   flat(idx) = state%hhs0; idx = idx + 1_c_int
   flat(idx) = state%age_0_4; idx = idx + 1_c_int
   flat(idx) = state%age_5_11; idx = idx + 1_c_int
   flat(idx) = state%age_12_18; idx = idx + 1_c_int
   flat(idx) = state%age_19_64; idx = idx + 1_c_int
   flat(idx) = state%age_65plus; idx = idx + 1_c_int
   flat(idx) = state%stebbs_height; idx = idx + 1_c_int
   flat(idx) = state%footprint_area; idx = idx + 1_c_int
   flat(idx) = state%wall_external_area; idx = idx + 1_c_int
   flat(idx) = state%internal_volume_ratio; idx = idx + 1_c_int
   flat(idx) = state%internal_mass_area; idx = idx + 1_c_int
   flat(idx) = state%wwr; idx = idx + 1_c_int
   flat(idx) = state%wall_thickness; idx = idx + 1_c_int
   flat(idx) = state%wall_effective_conductivity; idx = idx + 1_c_int
   flat(idx) = state%wall_density; idx = idx + 1_c_int
   flat(idx) = state%wall_specific_heat_capacity; idx = idx + 1_c_int
   flat(idx) = state%wall_external_thickness; idx = idx + 1_c_int
   flat(idx) = state%wall_external_effective_conductivity; idx = idx + 1_c_int
   flat(idx) = state%wall_external_density; idx = idx + 1_c_int
   flat(idx) = state%wall_external_specific_heat_capacity; idx = idx + 1_c_int
   flat(idx) = state%wall_outer_heat_capacity_fraction; idx = idx + 1_c_int
   flat(idx) = state%wall_external_emissivity; idx = idx + 1_c_int
   flat(idx) = state%wall_internal_emissivity; idx = idx + 1_c_int
   flat(idx) = state%wall_transmissivity; idx = idx + 1_c_int
   flat(idx) = state%wall_absorptivity; idx = idx + 1_c_int
   flat(idx) = state%wall_reflectivity; idx = idx + 1_c_int
   flat(idx) = state%roof_thickness; idx = idx + 1_c_int
   flat(idx) = state%roof_effective_conductivity; idx = idx + 1_c_int
   flat(idx) = state%roof_density; idx = idx + 1_c_int
   flat(idx) = state%roof_specific_heat_capacity; idx = idx + 1_c_int
   flat(idx) = state%roof_external_thickness; idx = idx + 1_c_int
   flat(idx) = state%roof_external_effective_conductivity; idx = idx + 1_c_int
   flat(idx) = state%roof_external_density; idx = idx + 1_c_int
   flat(idx) = state%roof_external_specific_heat_capacity; idx = idx + 1_c_int
   flat(idx) = state%roof_outer_heat_capacity_fraction; idx = idx + 1_c_int
   flat(idx) = state%roof_external_emissivity; idx = idx + 1_c_int
   flat(idx) = state%roof_internal_emissivity; idx = idx + 1_c_int
   flat(idx) = state%roof_transmissivity; idx = idx + 1_c_int
   flat(idx) = state%roof_absorptivity; idx = idx + 1_c_int
   flat(idx) = state%roof_reflectivity; idx = idx + 1_c_int
   flat(idx) = state%ground_floor_thickness; idx = idx + 1_c_int
   flat(idx) = state%ground_floor_effective_conductivity; idx = idx + 1_c_int
   flat(idx) = state%ground_floor_density; idx = idx + 1_c_int
   flat(idx) = state%ground_floor_specific_heat_capacity; idx = idx + 1_c_int
   flat(idx) = state%window_thickness; idx = idx + 1_c_int
   flat(idx) = state%window_effective_conductivity; idx = idx + 1_c_int
   flat(idx) = state%window_density; idx = idx + 1_c_int
   flat(idx) = state%window_specific_heat_capacity; idx = idx + 1_c_int
   flat(idx) = state%window_external_emissivity; idx = idx + 1_c_int
   flat(idx) = state%window_internal_emissivity; idx = idx + 1_c_int
   flat(idx) = state%window_transmissivity; idx = idx + 1_c_int
   flat(idx) = state%window_absorptivity; idx = idx + 1_c_int
   flat(idx) = state%window_reflectivity; idx = idx + 1_c_int
   flat(idx) = state%internal_mass_density; idx = idx + 1_c_int
   flat(idx) = state%internal_mass_specific_heat_capacity; idx = idx + 1_c_int
   flat(idx) = state%internal_mass_emissivity; idx = idx + 1_c_int
   flat(idx) = state%max_heating_power; idx = idx + 1_c_int
   flat(idx) = state%hot_water_tank_volume; idx = idx + 1_c_int
   flat(idx) = state%maximum_hot_water_heating_power; idx = idx + 1_c_int
   flat(idx) = state%heating_setpoint_temperature; idx = idx + 1_c_int
   flat(idx) = state%cooling_setpoint_temperature; idx = idx + 1_c_int

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         flat(idx) = state%heating_setpoint_temperature_profile(i, j)
         idx = idx + 1_c_int
      end do
   end do

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         flat(idx) = state%cooling_setpoint_temperature_profile(i, j)
         idx = idx + 1_c_int
      end do
   end do

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         flat(idx) = state%metabolism_profile(i, j)
         idx = idx + 1_c_int
      end do
   end do

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         flat(idx) = state%appliance_profile(i, j)
         idx = idx + 1_c_int
      end do
   end do

   flat(idx) = state%lighting_power_density; idx = idx + 1_c_int
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine building_archetype_prm_pack

subroutine building_archetype_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(BUILDING_ARCHETYPE_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: j

   if (n_flat<SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int

   state%building_count = flat(idx); idx = idx + 1_c_int
   state%occupants_state = flat(idx); idx = idx + 1_c_int
   state%hhs0 = flat(idx); idx = idx + 1_c_int
   state%age_0_4 = flat(idx); idx = idx + 1_c_int
   state%age_5_11 = flat(idx); idx = idx + 1_c_int
   state%age_12_18 = flat(idx); idx = idx + 1_c_int
   state%age_19_64 = flat(idx); idx = idx + 1_c_int
   state%age_65plus = flat(idx); idx = idx + 1_c_int
   state%stebbs_height = flat(idx); idx = idx + 1_c_int
   state%footprint_area = flat(idx); idx = idx + 1_c_int
   state%wall_external_area = flat(idx); idx = idx + 1_c_int
   state%internal_volume_ratio = flat(idx); idx = idx + 1_c_int
   state%internal_mass_area = flat(idx); idx = idx + 1_c_int
   state%wwr = flat(idx); idx = idx + 1_c_int
   state%wall_thickness = flat(idx); idx = idx + 1_c_int
   state%wall_effective_conductivity = flat(idx); idx = idx + 1_c_int
   state%wall_density = flat(idx); idx = idx + 1_c_int
   state%wall_specific_heat_capacity = flat(idx); idx = idx + 1_c_int
   state%wall_external_thickness = flat(idx); idx = idx + 1_c_int
   state%wall_external_effective_conductivity = flat(idx); idx = idx + 1_c_int
   state%wall_external_density = flat(idx); idx = idx + 1_c_int
   state%wall_external_specific_heat_capacity = flat(idx); idx = idx + 1_c_int
   state%wall_outer_heat_capacity_fraction = flat(idx); idx = idx + 1_c_int
   state%wall_external_emissivity = flat(idx); idx = idx + 1_c_int
   state%wall_internal_emissivity = flat(idx); idx = idx + 1_c_int
   state%wall_transmissivity = flat(idx); idx = idx + 1_c_int
   state%wall_absorptivity = flat(idx); idx = idx + 1_c_int
   state%wall_reflectivity = flat(idx); idx = idx + 1_c_int
   state%roof_thickness = flat(idx); idx = idx + 1_c_int
   state%roof_effective_conductivity = flat(idx); idx = idx + 1_c_int
   state%roof_density = flat(idx); idx = idx + 1_c_int
   state%roof_specific_heat_capacity = flat(idx); idx = idx + 1_c_int
   state%roof_external_thickness = flat(idx); idx = idx + 1_c_int
   state%roof_external_effective_conductivity = flat(idx); idx = idx + 1_c_int
   state%roof_external_density = flat(idx); idx = idx + 1_c_int
   state%roof_external_specific_heat_capacity = flat(idx); idx = idx + 1_c_int
   state%roof_outer_heat_capacity_fraction = flat(idx); idx = idx + 1_c_int
   state%roof_external_emissivity = flat(idx); idx = idx + 1_c_int
   state%roof_internal_emissivity = flat(idx); idx = idx + 1_c_int
   state%roof_transmissivity = flat(idx); idx = idx + 1_c_int
   state%roof_absorptivity = flat(idx); idx = idx + 1_c_int
   state%roof_reflectivity = flat(idx); idx = idx + 1_c_int
   state%ground_floor_thickness = flat(idx); idx = idx + 1_c_int
   state%ground_floor_effective_conductivity = flat(idx); idx = idx + 1_c_int
   state%ground_floor_density = flat(idx); idx = idx + 1_c_int
   state%ground_floor_specific_heat_capacity = flat(idx); idx = idx + 1_c_int
   state%window_thickness = flat(idx); idx = idx + 1_c_int
   state%window_effective_conductivity = flat(idx); idx = idx + 1_c_int
   state%window_density = flat(idx); idx = idx + 1_c_int
   state%window_specific_heat_capacity = flat(idx); idx = idx + 1_c_int
   state%window_external_emissivity = flat(idx); idx = idx + 1_c_int
   state%window_internal_emissivity = flat(idx); idx = idx + 1_c_int
   state%window_transmissivity = flat(idx); idx = idx + 1_c_int
   state%window_absorptivity = flat(idx); idx = idx + 1_c_int
   state%window_reflectivity = flat(idx); idx = idx + 1_c_int
   state%internal_mass_density = flat(idx); idx = idx + 1_c_int
   state%internal_mass_specific_heat_capacity = flat(idx); idx = idx + 1_c_int
   state%internal_mass_emissivity = flat(idx); idx = idx + 1_c_int
   state%max_heating_power = flat(idx); idx = idx + 1_c_int
   state%hot_water_tank_volume = flat(idx); idx = idx + 1_c_int
   state%maximum_hot_water_heating_power = flat(idx); idx = idx + 1_c_int
   state%heating_setpoint_temperature = flat(idx); idx = idx + 1_c_int
   state%cooling_setpoint_temperature = flat(idx); idx = idx + 1_c_int

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         state%heating_setpoint_temperature_profile(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         state%cooling_setpoint_temperature_profile(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do
   
   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         state%metabolism_profile(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         state%appliance_profile(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

   ! Keep unpack order aligned with Rust and the Fortran pack routine:
   ! LightingPowerDensity is stored after the two 144x2 profiles.
   state%lighting_power_density = flat(idx); idx = idx + 1_c_int
   state%iter_safe = flat(idx)>=0.5_c_double
   err = SUEWS_CAPI_OK

end subroutine building_archetype_prm_unpack

   SUBROUTINE suews_building_archetype_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_building_archetype_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_building_archetype_prm_error_message

end module module_c_api_building_archetype_prm

module c_api_building_archetype_prm_module
use module_c_api_building_archetype_prm
end module c_api_building_archetype_prm_module
