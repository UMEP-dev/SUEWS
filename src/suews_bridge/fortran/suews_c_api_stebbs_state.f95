! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for STEBBS_STATE.
! -----------------------------------------------------------------------------
module module_c_api_stebbs_state
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_stebbs, only: STEBBS_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_STATE_RSL_LEN = 30_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_STATE_LEN = 154_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_STATE_SCHEMA_VERSION = 1_c_int

type :: stebbs_state_shadow
   real(c_double) :: kdown2d = 0.0_c_double
   real(c_double) :: kup2d = 0.0_c_double
   real(c_double) :: kwest = 0.0_c_double
   real(c_double) :: ksouth = 0.0_c_double
   real(c_double) :: knorth = 0.0_c_double
   real(c_double) :: keast = 0.0_c_double
   real(c_double) :: ldown2d = 0.0_c_double
   real(c_double) :: lup2d = 0.0_c_double
   real(c_double) :: lwest = 0.0_c_double
   real(c_double) :: lsouth = 0.0_c_double
   real(c_double) :: lnorth = 0.0_c_double
   real(c_double) :: least = 0.0_c_double
   real(c_double), dimension(30) :: zarray = -999.0_c_double
   real(c_double), dimension(30) :: dataout_line_ursl = -999.0_c_double
   real(c_double), dimension(30) :: dataout_line_trsl = -999.0_c_double
   real(c_double), dimension(30) :: dataout_line_qrsl = -999.0_c_double
   real(c_double) :: deep_soil_temperature = 0.0_c_double
   real(c_double) :: outdoor_air_start_temperature = 0.0_c_double
   real(c_double) :: indoor_air_start_temperature = 0.0_c_double
   real(c_double) :: indoor_mass_start_temperature = 0.0_c_double
   real(c_double) :: wall_indoor_surface_temperature = 0.0_c_double
   real(c_double) :: wall_outdoor_surface_temperature = 0.0_c_double
   real(c_double) :: roof_indoor_surface_temperature = 0.0_c_double
   real(c_double) :: roof_outdoor_surface_temperature = 0.0_c_double
   real(c_double) :: window_indoor_surface_temperature = 0.0_c_double
   real(c_double) :: window_outdoor_surface_temperature = 0.0_c_double
   real(c_double) :: ground_floor_indoor_surface_temperature = 0.0_c_double
   real(c_double) :: ground_floor_outdoor_surface_temperature = 0.0_c_double
   real(c_double) :: water_tank_temperature = 0.0_c_double
   real(c_double) :: internal_wall_water_tank_temperature = 0.0_c_double
   real(c_double) :: external_wall_water_tank_temperature = 0.0_c_double
   real(c_double) :: mains_water_temperature = 0.0_c_double
   real(c_double) :: domestic_hot_water_temperature_in_use_in_building = 0.0_c_double
   real(c_double) :: internal_wall_dhw_vessel_temperature = 0.0_c_double
   real(c_double) :: external_wall_dhw_vessel_temperature = 0.0_c_double
   real(c_double) :: qs_stebbs = 0.0_c_double
   integer(c_int) :: buildings_count = 0_c_int
   logical :: iter_safe = .false.
end type stebbs_state_shadow

public :: suews_stebbs_state_len
public :: suews_stebbs_state_schema_version
public :: suews_stebbs_state_default
public :: suews_stebbs_state_error_message
public :: stebbs_state_unpack

contains

subroutine suews_stebbs_state_len(n_flat, err) bind(C, name='suews_stebbs_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_STEBBS_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_stebbs_state_len

subroutine suews_stebbs_state_schema_version(schema_version, err) bind(C, name='suews_stebbs_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_STEBBS_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_stebbs_state_schema_version

subroutine suews_stebbs_state_default(flat, n_flat, err) bind(C, name='suews_stebbs_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(stebbs_state_shadow) :: state

   call stebbs_state_pack(state, flat, n_flat, err)

end subroutine suews_stebbs_state_default

subroutine stebbs_state_pack(state, flat, n_flat, err)
   implicit none

   type(stebbs_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_STEBBS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

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

   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = state%zarray(i)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = state%dataout_line_ursl(i)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = state%dataout_line_trsl(i)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = state%dataout_line_qrsl(i)
      idx = idx + 1_c_int
   end do

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
   flat(idx) = real(state%buildings_count, c_double); idx = idx + 1_c_int
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine stebbs_state_pack

subroutine stebbs_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(STEBBS_STATE), intent(inout) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_STEBBS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int

   state%kdown2d = flat(idx); idx = idx + 1_c_int
   state%kup2d = flat(idx); idx = idx + 1_c_int
   state%kwest = flat(idx); idx = idx + 1_c_int
   state%ksouth = flat(idx); idx = idx + 1_c_int
   state%knorth = flat(idx); idx = idx + 1_c_int
   state%keast = flat(idx); idx = idx + 1_c_int
   state%ldown2d = flat(idx); idx = idx + 1_c_int
   state%lup2d = flat(idx); idx = idx + 1_c_int
   state%lwest = flat(idx); idx = idx + 1_c_int
   state%lsouth = flat(idx); idx = idx + 1_c_int
   state%lnorth = flat(idx); idx = idx + 1_c_int
   state%least = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      state%zarray(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      state%dataoutlineursl(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      state%dataoutlinetrsl(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      state%dataoutlineqrsl(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%deepsoiltemperature = flat(idx); idx = idx + 1_c_int
   state%outdoorairstarttemperature = flat(idx); idx = idx + 1_c_int
   state%indoorairstarttemperature = flat(idx); idx = idx + 1_c_int
   state%indoormassstarttemperature = flat(idx); idx = idx + 1_c_int
   state%wallindoorsurfacetemperature = flat(idx); idx = idx + 1_c_int
   state%walloutdoorsurfacetemperature = flat(idx); idx = idx + 1_c_int
   state%roofindoorsurfacetemperature = flat(idx); idx = idx + 1_c_int
   state%roofoutdoorsurfacetemperature = flat(idx); idx = idx + 1_c_int
   state%windowindoorsurfacetemperature = flat(idx); idx = idx + 1_c_int
   state%windowoutdoorsurfacetemperature = flat(idx); idx = idx + 1_c_int
   state%groundfloorindoorsurfacetemperature = flat(idx); idx = idx + 1_c_int
   state%groundflooroutdoorsurfacetemperature = flat(idx); idx = idx + 1_c_int
   state%watertanktemperature = flat(idx); idx = idx + 1_c_int
   state%internalwallwatertanktemperature = flat(idx); idx = idx + 1_c_int
   state%externalwallwatertanktemperature = flat(idx); idx = idx + 1_c_int
   state%mainswatertemperature = flat(idx); idx = idx + 1_c_int
   state%domestichotwatertemperatureinuseinbuilding = flat(idx); idx = idx + 1_c_int
   state%internalwalldhwvesseltemperature = flat(idx); idx = idx + 1_c_int
   state%externalwalldhwvesseltemperature = flat(idx); idx = idx + 1_c_int
   state%qs_stebbs = flat(idx); idx = idx + 1_c_int
   state%iter_safe = flat(idx + 1_c_int)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine stebbs_state_unpack

subroutine suews_stebbs_state_error_message(code, buffer, buffer_len) bind(C, name='suews_stebbs_state_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_stebbs_state_error_message

end module module_c_api_stebbs_state

module c_api_stebbs_state_module
use module_c_api_stebbs_state
end module c_api_stebbs_state_module
