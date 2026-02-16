! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for STEBBS_PRM.
! -----------------------------------------------------------------------------
module module_c_api_stebbs_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_stebbs, only: STEBBS_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_PRM_PROFILE_STEPS = 144_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_PRM_PROFILE_GROUPS = 2_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_PRM_LEN = 333_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_PRM_SCHEMA_VERSION = 1_c_int

type :: stebbs_prm_shadow
   real(c_double) :: wall_internal_convection_coefficient = 0.0_c_double
   real(c_double) :: roof_internal_convection_coefficient = 0.0_c_double
   real(c_double) :: internal_mass_convection_coefficient = 0.0_c_double
   real(c_double) :: floor_internal_convection_coefficient = 0.0_c_double
   real(c_double) :: window_internal_convection_coefficient = 0.0_c_double
   real(c_double) :: wall_external_convection_coefficient = 0.0_c_double
   real(c_double) :: roof_external_convection_coefficient = 0.0_c_double
   real(c_double) :: window_external_convection_coefficient = 0.0_c_double
   real(c_double) :: ground_depth = 0.0_c_double
   real(c_double) :: external_ground_conductivity = 0.0_c_double
   real(c_double) :: indoor_air_density = 0.0_c_double
   real(c_double) :: indoor_air_cp = 0.0_c_double
   real(c_double) :: metabolism_threshold = 0.0_c_double
   real(c_double) :: latent_sensible_ratio = 0.0_c_double
   real(c_double) :: heating_system_efficiency = 0.0_c_double
   real(c_double) :: max_cooling_power = 0.0_c_double
   real(c_double) :: cooling_system_cop = 0.0_c_double
   real(c_double) :: ventilation_rate = 0.0_c_double
   real(c_double) :: water_tank_wall_thickness = 0.0_c_double
   real(c_double) :: water_tank_surface_area = 0.0_c_double
   real(c_double) :: hot_water_heating_setpoint_temperature = 0.0_c_double
   real(c_double) :: hot_water_tank_wall_emissivity = 0.0_c_double
   real(c_double) :: dhw_vessel_wall_thickness = 0.0_c_double
   real(c_double) :: dhw_water_volume = 0.0_c_double
   real(c_double) :: dhw_surface_area = 0.0_c_double
   real(c_double) :: hot_water_flow_rate = 0.0_c_double
   real(c_double), dimension(0:143, 2) :: hot_water_flow_profile = 0.0_c_double
   real(c_double) :: dhw_specific_heat_capacity = 0.0_c_double
   real(c_double) :: hot_water_tank_specific_heat_capacity = 0.0_c_double
   real(c_double) :: dhw_vessel_specific_heat_capacity = 0.0_c_double
   real(c_double) :: dhw_density = 0.0_c_double
   real(c_double) :: hot_water_tank_wall_density = 0.0_c_double
   real(c_double) :: dhw_vessel_density = 0.0_c_double
   real(c_double) :: hot_water_tank_building_wall_view_factor = 0.0_c_double
   real(c_double) :: hot_water_tank_internal_mass_view_factor = 0.0_c_double
   real(c_double) :: hot_water_tank_wall_conductivity = 0.0_c_double
   real(c_double) :: hot_water_tank_internal_wall_convection_coefficient = 0.0_c_double
   real(c_double) :: hot_water_tank_external_wall_convection_coefficient = 0.0_c_double
   real(c_double) :: dhw_vessel_wall_conductivity = 0.0_c_double
   real(c_double) :: dhw_vessel_internal_wall_convection_coefficient = 0.0_c_double
   real(c_double) :: dhw_vessel_external_wall_convection_coefficient = 0.0_c_double
   real(c_double) :: dhw_vessel_wall_emissivity = 0.0_c_double
   real(c_double) :: hot_water_heating_efficiency = 0.0_c_double
   real(c_double) :: minimum_volume_of_dhw_in_use = 0.0_c_double
   real(c_double) :: maximum_volume_of_dhw_in_use = 0.0_c_double
   logical :: iter_safe = .true.
end type stebbs_prm_shadow

public :: suews_stebbs_prm_len
public :: suews_stebbs_prm_schema_version
public :: suews_stebbs_prm_default
public :: suews_stebbs_prm_error_message
public :: stebbs_prm_unpack

contains

subroutine suews_stebbs_prm_len(n_flat, err) bind(C, name='suews_stebbs_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_STEBBS_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_stebbs_prm_len

subroutine suews_stebbs_prm_schema_version(schema_version, err) bind(C, name='suews_stebbs_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_STEBBS_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_stebbs_prm_schema_version

subroutine suews_stebbs_prm_default(flat, n_flat, err) bind(C, name='suews_stebbs_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(stebbs_prm_shadow) :: state

   call stebbs_prm_pack(state, flat, n_flat, err)

end subroutine suews_stebbs_prm_default

subroutine stebbs_prm_pack(state, flat, n_flat, err)
   implicit none

   type(stebbs_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: j

   if (n_flat<SUEWS_CAPI_STEBBS_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

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

   do j = 1_c_int, SUEWS_CAPI_STEBBS_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_STEBBS_PRM_PROFILE_STEPS - 1_c_int
         flat(idx) = state%hot_water_flow_profile(i, j)
         idx = idx + 1_c_int
      end do
   end do

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
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine stebbs_prm_pack

subroutine stebbs_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(STEBBS_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: j

   if (n_flat<SUEWS_CAPI_STEBBS_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   state%wallinternalconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%roofinternalconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%internalmassconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%floorinternalconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%windowinternalconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%wallexternalconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%roofexternalconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%windowexternalconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%grounddepth = flat(idx); idx = idx + 1_c_int
   state%externalgroundconductivity = flat(idx); idx = idx + 1_c_int
   state%indoorairdensity = flat(idx); idx = idx + 1_c_int
   state%indooraircp = flat(idx); idx = idx + 1_c_int
   state%metabolismthreshold = flat(idx); idx = idx + 1_c_int
   state%latentsensibleratio = flat(idx); idx = idx + 1_c_int
   state%heatingsystemefficiency = flat(idx); idx = idx + 1_c_int
   state%maxcoolingpower = flat(idx); idx = idx + 1_c_int
   state%coolingsystemcop = flat(idx); idx = idx + 1_c_int
   state%ventilationrate = flat(idx); idx = idx + 1_c_int
   state%watertankwallthickness = flat(idx); idx = idx + 1_c_int
   state%watertanksurfacearea = flat(idx); idx = idx + 1_c_int
   state%hotwaterheatingsetpointtemperature = flat(idx); idx = idx + 1_c_int
   state%hotwatertankwallemissivity = flat(idx); idx = idx + 1_c_int
   state%dhwvesselwallthickness = flat(idx); idx = idx + 1_c_int
   state%dhwwatervolume = flat(idx); idx = idx + 1_c_int
   state%dhwsurfacearea = flat(idx); idx = idx + 1_c_int
   state%hotwaterflowrate = flat(idx); idx = idx + 1_c_int

   do j = 1_c_int, SUEWS_CAPI_STEBBS_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_STEBBS_PRM_PROFILE_STEPS - 1_c_int
         state%hotwaterflowprofile(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

   state%dhwspecificheatcapacity = flat(idx); idx = idx + 1_c_int
   state%hotwatertankspecificheatcapacity = flat(idx); idx = idx + 1_c_int
   state%dhwvesselspecificheatcapacity = flat(idx); idx = idx + 1_c_int
   state%dhwdensity = flat(idx); idx = idx + 1_c_int
   state%hotwatertankwalldensity = flat(idx); idx = idx + 1_c_int
   state%dhwvesseldensity = flat(idx); idx = idx + 1_c_int
   state%hotwatertankbuildingwallviewfactor = flat(idx); idx = idx + 1_c_int
   state%hotwatertankinternalmassviewfactor = flat(idx); idx = idx + 1_c_int
   state%hotwatertankwallconductivity = flat(idx); idx = idx + 1_c_int
   state%hotwatertankinternalwallconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%hotwatertankexternalwallconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%dhwvesselwallconductivity = flat(idx); idx = idx + 1_c_int
   state%dhwvesselinternalwallconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%dhwvesselexternalwallconvectioncoefficient = flat(idx); idx = idx + 1_c_int
   state%dhwvesselwallemissivity = flat(idx); idx = idx + 1_c_int
   state%hotwaterheatingefficiency = flat(idx); idx = idx + 1_c_int
   state%minimumvolumeofdhwinuse = flat(idx); idx = idx + 1_c_int
   state%maximumvolumeofdhwinuse = flat(idx); idx = idx + 1_c_int
   state%iter_safe = flat(idx)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine stebbs_prm_unpack

subroutine suews_stebbs_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_stebbs_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_stebbs_prm_error_message

end module module_c_api_stebbs_prm

module c_api_stebbs_prm_module
use module_c_api_stebbs_prm
end module c_api_stebbs_prm_module
