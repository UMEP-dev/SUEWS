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
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_STATE_LEN = 155_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_STATE_SCHEMA_VERSION = 2_c_int

type :: stebbs_state_shadow
   real(c_double) :: kdown_2d = 0.0_c_double
   real(c_double) :: kup_2d = 0.0_c_double
   real(c_double) :: k_west = 0.0_c_double
   real(c_double) :: k_south = 0.0_c_double
   real(c_double) :: k_north = 0.0_c_double
   real(c_double) :: k_east = 0.0_c_double
   real(c_double) :: ldown_2d = 0.0_c_double
   real(c_double) :: lup_2d = 0.0_c_double
   real(c_double) :: l_west = 0.0_c_double
   real(c_double) :: l_south = 0.0_c_double
   real(c_double) :: l_north = 0.0_c_double
   real(c_double) :: l_east = 0.0_c_double
   real(c_double), dimension(30) :: z_array = -999.0_c_double
   real(c_double), dimension(30) :: dataout_line_ursl = -999.0_c_double
   real(c_double), dimension(30) :: dataout_line_trsl = -999.0_c_double
   real(c_double), dimension(30) :: dataout_line_qrsl = -999.0_c_double
   real(c_double) :: deep_soil_temperature = 0.0_c_double
   real(c_double) :: month_mean_air_temperature_diffmax = 0.0_c_double
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

   flat(idx) = state%kdown_2d; idx = idx + 1_c_int
   flat(idx) = state%kup_2d; idx = idx + 1_c_int
   flat(idx) = state%k_west; idx = idx + 1_c_int
   flat(idx) = state%k_south; idx = idx + 1_c_int
   flat(idx) = state%k_north; idx = idx + 1_c_int
   flat(idx) = state%k_east; idx = idx + 1_c_int
   flat(idx) = state%ldown_2d; idx = idx + 1_c_int
   flat(idx) = state%lup_2d; idx = idx + 1_c_int
   flat(idx) = state%l_west; idx = idx + 1_c_int
   flat(idx) = state%l_south; idx = idx + 1_c_int
   flat(idx) = state%l_north; idx = idx + 1_c_int
   flat(idx) = state%l_east; idx = idx + 1_c_int

   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = state%z_array(i)
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
   flat(idx) = state%month_mean_air_temperature_diffmax; idx = idx + 1_c_int
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

   state%kdown_2d = flat(idx); idx = idx + 1_c_int
   state%kup_2d = flat(idx); idx = idx + 1_c_int
   state%k_west = flat(idx); idx = idx + 1_c_int
   state%k_south = flat(idx); idx = idx + 1_c_int
   state%k_north = flat(idx); idx = idx + 1_c_int
   state%k_east = flat(idx); idx = idx + 1_c_int
   state%ldown_2d = flat(idx); idx = idx + 1_c_int
   state%lup_2d = flat(idx); idx = idx + 1_c_int
   state%l_west = flat(idx); idx = idx + 1_c_int
   state%l_south = flat(idx); idx = idx + 1_c_int
   state%l_north = flat(idx); idx = idx + 1_c_int
   state%l_east = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      state%z_array(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      state%dataout_line_u_rsl(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      state%dataout_line_t_rsl(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      state%dataout_line_q_rsl(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%deep_soil_temperature = flat(idx); idx = idx + 1_c_int
   state%monthmeanairtemperature_diffmax = flat(idx); idx = idx + 1_c_int
   state%outdoor_air_start_temperature = flat(idx); idx = idx + 1_c_int
   state%indoor_air_start_temperature = flat(idx); idx = idx + 1_c_int
   state%indoor_mass_start_temperature = flat(idx); idx = idx + 1_c_int
   state%wall_indoor_surface_temperature = flat(idx); idx = idx + 1_c_int
   state%wall_outdoor_surface_temperature = flat(idx); idx = idx + 1_c_int
   state%roof_indoor_surface_temperature = flat(idx); idx = idx + 1_c_int
   state%roof_outdoor_surface_temperature = flat(idx); idx = idx + 1_c_int
   state%window_indoor_surface_temperature = flat(idx); idx = idx + 1_c_int
   state%window_outdoor_surface_temperature = flat(idx); idx = idx + 1_c_int
   state%ground_floor_indoor_surface_temperature = flat(idx); idx = idx + 1_c_int
   state%ground_floor_outdoor_surface_temperature = flat(idx); idx = idx + 1_c_int
   state%water_tank_temperature_state = flat(idx); idx = idx + 1_c_int
   state%internal_wall_water_tank_temperature = flat(idx); idx = idx + 1_c_int
   state%external_wall_water_tank_temperature = flat(idx); idx = idx + 1_c_int
   state%mains_water_temperature = flat(idx); idx = idx + 1_c_int
   state%domestic_hot_water_temperature_in_use_in_building = flat(idx); idx = idx + 1_c_int
   state%internal_wall_dhw_vessel_temperature = flat(idx); idx = idx + 1_c_int
   state%external_wall_dhw_vessel_temperature = flat(idx); idx = idx + 1_c_int
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
