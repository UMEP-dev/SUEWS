! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for STEBBS_STATE (including buildings(:)).
! -----------------------------------------------------------------------------
module module_c_api_stebbs_state
use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_double, c_char
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
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
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_STATE_BASE_LEN = 155_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_STATE_LEN = SUEWS_CAPI_STEBBS_STATE_BASE_LEN
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_BLDG_PROFILE_STEPS = 144_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_BLDG_PROFILE_GROUPS = 2_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_BLDG_ENERGY_LEN = 45_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_BLDG_BASE_LEN = 557_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_STEBBS_STATE_SCHEMA_VERSION = 3_c_int

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
public :: suews_stebbs_state_roundtrip
public :: suews_stebbs_state_error_message
public :: stebbs_state_unpack
public :: stebbs_state_pack_real

contains

subroutine suews_stebbs_state_len(n_flat, err) bind(C, name='suews_stebbs_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_STEBBS_STATE_BASE_LEN
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

   if (n_flat<SUEWS_CAPI_STEBBS_STATE_BASE_LEN) then
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

subroutine unpack_checked_c_int(raw, out_val, err)
   implicit none

   real(c_double), intent(in) :: raw
   integer(c_int), intent(out) :: out_val
   integer(c_int), intent(out) :: err

   real(c_double) :: rounded
   real(c_double), parameter :: MIN_C_INT_F64 = -real(huge(0_c_int), c_double) - 1.0_c_double
   real(c_double), parameter :: MAX_C_INT_F64 = real(huge(0_c_int), c_double)

   out_val = 0_c_int
   if (.not. ieee_is_finite(raw)) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if

   rounded = anint(raw)
   if (abs(raw - rounded)>1.0e-9_c_double .or. rounded<MIN_C_INT_F64 .or. rounded>MAX_C_INT_F64) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if

   out_val = int(int(rounded, c_int64_t), c_int)
   err = SUEWS_CAPI_OK

end subroutine unpack_checked_c_int

subroutine stebbs_state_unpack(flat, n_flat, bldg_nlayer, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(in) :: bldg_nlayer
   type(STEBBS_STATE), intent(inout) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: j
   integer(c_int) :: ib
   integer(c_int) :: il
   integer(c_int) :: bldg_count
   integer(c_int) :: int_val
   integer(c_int) :: local_err
   real(c_double) :: raw_bldg_count
   integer(c_int64_t) :: per_bldg_len
   integer(c_int64_t) :: expected_len

   if (n_flat<SUEWS_CAPI_STEBBS_STATE_BASE_LEN .or. bldg_nlayer<0_c_int) then
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

   raw_bldg_count = flat(idx); idx = idx + 1_c_int
   if (.not. ieee_is_finite(raw_bldg_count)) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if
   if (raw_bldg_count<0.0_c_double) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if
   call unpack_checked_c_int(raw_bldg_count, bldg_count, local_err)
   if (local_err/=SUEWS_CAPI_OK .or. bldg_count<0_c_int) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if

   state%iter_safe = flat(idx)>=0.5_c_double; idx = idx + 1_c_int

   if (bldg_count==0_c_int) then
      if (n_flat/=SUEWS_CAPI_STEBBS_STATE_BASE_LEN) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      call state%DEALLOCATE()
      err = SUEWS_CAPI_OK
      return
   end if

   per_bldg_len = int(SUEWS_CAPI_STEBBS_BLDG_BASE_LEN, c_int64_t) + 2_c_int64_t * int(bldg_nlayer, c_int64_t)
   expected_len = int(SUEWS_CAPI_STEBBS_STATE_BASE_LEN, c_int64_t) + int(bldg_count, c_int64_t) * per_bldg_len
   if (int(n_flat, c_int64_t)/=expected_len) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   call state%ALLOCATE(int(bldg_count), int(bldg_nlayer))

   do ib = 1_c_int, bldg_count
      call unpack_checked_c_int(flat(idx), int_val, local_err); idx = idx + 1_c_int
      if (local_err/=SUEWS_CAPI_OK) then
         call state%DEALLOCATE()
         err = local_err
         return
      end if
      state%buildings(ib)%id_lbm = int(int_val)

      call unpack_checked_c_int(flat(idx), int_val, local_err); idx = idx + 1_c_int
      if (local_err/=SUEWS_CAPI_OK) then
         call state%DEALLOCATE()
         err = local_err
         return
      end if
      state%buildings(ib)%appliance_totalnumber = int(int_val)
      state%buildings(ib)%QHload_heating_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QHload_cooling_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QH_metabolism = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QE_metabolism = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QHload_dhw_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Qloss_drain_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%ratio_window_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%a_footprint = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%height_building = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%wall_external_area = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%internal_volume_ratio = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%thickness_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%thickness_wallext = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%thickness_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%thickness_roofext = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%thickness_groundfloor = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%depth_ground = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%thickness_window = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_intwall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_introof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_indoormass = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_intgroundfloor = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_intwindow = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_extwall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_extroof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_extwindow = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_wallext = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_roofext = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_groundfloor = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_window = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_ground = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_wallext = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_roofext = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%weighting_factor_heatcapacity_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%weighting_factor_heatcapacity_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_groundfloor = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_window = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_indoormass = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_air_ind = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_wallext = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_roofext = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_groundfloor = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_window = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_indoormass = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_air_ind = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_extwall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_extroof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_intwall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_introof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_indoormass = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_extwindow = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_intwindow = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%window_transmissivity = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%window_absorptivity = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%window_reflectivity = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%wall_transmissivity_state = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%wall_absorptivity = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%wall_reflectivity = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%roof_transmissivity_state = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%roof_absorptivity = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%roof_reflectivity = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%occupants_state = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%metabolic_rate = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%metabolism_threshold = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%ratio_metabolic_latent_sensible = flat(idx); idx = idx + 1_c_int
      call unpack_checked_c_int(flat(idx), int_val, local_err); idx = idx + 1_c_int
      if (local_err/=SUEWS_CAPI_OK) then
         call state%DEALLOCATE()
         err = local_err
         return
      end if
      state%buildings(ib)%internal_shading = int(int_val)
      state%buildings(ib)%reduction_factor_shading = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%temperature_threshold_shading = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%radiation_threshold_shading = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%appliance_power_rating = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%lighting_power_rating = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%frac_hotwater = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%maxheatingpower_air = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%heating_efficiency_air = flat(idx); idx = idx + 1_c_int
      call unpack_checked_c_int(flat(idx), int_val, local_err); idx = idx + 1_c_int
      if (local_err/=SUEWS_CAPI_OK) then
         call state%DEALLOCATE()
         err = local_err
         return
      end if
      state%buildings(ib)%destination_waste_heat = int(int_val)
      state%buildings(ib)%fraction_convective_heating = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%maxcoolingpower_air = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%coeff_performance_cooling = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Vair_ind = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%ventilation_rate = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%a_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%a_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%v_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%v_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%v_ground_floor = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%a_window = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%v_window = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%v_indoor_mass = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%a_indoor_mass = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Tair_ind = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_indoor_mass = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_int_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_int_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_ext_wall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_ext_roof = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_int_window = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_ext_window = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_int_ground_floor = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%t_ext_ground_floor = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Twater_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Tintwall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Textwall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%thickness_tankwall = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Tincomingwater_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Vwater_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Asurf_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Vwall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%setTwater_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%init_wtTs = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Twater_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Tintwall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Textwall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%thickness_wall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Vwater_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Awater_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%Vwall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%flowrate_water_supply = flat(idx); idx = idx + 1_c_int

      do j = 1_c_int, SUEWS_CAPI_STEBBS_BLDG_PROFILE_GROUPS
         do i = 0_c_int, SUEWS_CAPI_STEBBS_BLDG_PROFILE_STEPS - 1_c_int
            state%buildings(ib)%flowrate_water_supply_profile(i, j) = flat(idx)
            idx = idx + 1_c_int
         end do
      end do

      state%buildings(ib)%single_flowrate_water_supply = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%single_flowrate_water_drain = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_water = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_wall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%cp_wall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_water = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_wall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%density_wall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%BVF_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%MVF_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_wall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_intwall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_extwall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_extwall_tank = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conductivity_wall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_intwall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%conv_coeff_extwall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%emissivity_extwall_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%maxheatingpower_water = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%heating_efficiency_water = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%minVwater_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%maxVwater_vessel = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%minHeatingPower_DHW = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%HeatingPower_DHW = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%qfm_dom = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%qheat_dom = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%qcool_dom = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%qfb_hw_dom = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%qfb_dom_air = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%dom_temp = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QN_bldg_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QEC_bldg_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QS_total_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QH_bldg_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QBAE_bldg_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QWaste_bldg_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QS_bldg_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QS_dhw_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QS_ground_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QEC_heating_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QEC_cooling_tstepFA = flat(idx); idx = idx + 1_c_int
      state%buildings(ib)%QEC_dhw_tstepFA = flat(idx); idx = idx + 1_c_int

      do i = 1_c_int, 2_c_int
         state%buildings(ib)%ts(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 2_c_int
         state%buildings(ib)%initTs(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 5_c_int
         state%buildings(ib)%h_i(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 5_c_int
         state%buildings(ib)%k_eff(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         state%buildings(ib)%h_o(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 6_c_int
         state%buildings(ib)%rho(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 6_c_int
         state%buildings(ib)%cp(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 7_c_int
         state%buildings(ib)%emis(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         state%buildings(ib)%witar(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         state%buildings(ib)%waTAR(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         state%buildings(ib)%roofTAR(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         state%buildings(ib)%h_ts_average(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         state%buildings(ib)%HWTsAverage(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         state%buildings(ib)%hw_power_average(i) = flat(idx); idx = idx + 1_c_int
      end do
      do i = 1_c_int, SUEWS_CAPI_STEBBS_BLDG_ENERGY_LEN
         state%buildings(ib)%energy_exchanges(i) = flat(idx); idx = idx + 1_c_int
      end do

      if (bldg_nlayer>0_c_int) then
         do il = 1_c_int, bldg_nlayer
            state%buildings(ib)%Textwall_C(il) = flat(idx); idx = idx + 1_c_int
         end do
         do il = 1_c_int, bldg_nlayer
            state%buildings(ib)%Textroof_C(il) = flat(idx); idx = idx + 1_c_int
         end do
      end if

      state%buildings(ib)%wall_surface_active = flat(idx)>=0.5_c_double; idx = idx + 1_c_int
      state%buildings(ib)%window_surface_active = flat(idx)>=0.5_c_double; idx = idx + 1_c_int
      state%buildings(ib)%iter_safe = flat(idx)>=0.5_c_double; idx = idx + 1_c_int
   end do

   err = SUEWS_CAPI_OK

end subroutine stebbs_state_unpack

subroutine stebbs_state_pack_real(state, bldg_nlayer, flat, n_flat, err)
   implicit none

   type(STEBBS_STATE), intent(in) :: state
   integer(c_int), intent(in) :: bldg_nlayer
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i
   integer(c_int) :: j
   integer(c_int) :: ib
   integer(c_int) :: il
   integer(c_int) :: bldg_count
   integer(c_int64_t) :: per_bldg_len
   integer(c_int64_t) :: expected_len

   if (bldg_nlayer<0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   bldg_count = 0_c_int
   if (allocated(state%buildings)) bldg_count = int(size(state%buildings), c_int)

   if (bldg_count==0_c_int) then
      expected_len = int(SUEWS_CAPI_STEBBS_STATE_BASE_LEN, c_int64_t)
   else
      per_bldg_len = int(SUEWS_CAPI_STEBBS_BLDG_BASE_LEN, c_int64_t) + 2_c_int64_t * int(bldg_nlayer, c_int64_t)
      expected_len = int(SUEWS_CAPI_STEBBS_STATE_BASE_LEN, c_int64_t) + int(bldg_count, c_int64_t) * per_bldg_len
   end if

   if (int(n_flat, c_int64_t)/=expected_len) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (bldg_count>0_c_int) then
      if (bldg_nlayer>0_c_int) then
         do ib = 1_c_int, bldg_count
            if (.not. allocated(state%buildings(ib)%Textwall_C) .or. &
                .not. allocated(state%buildings(ib)%Textroof_C)) then
               err = SUEWS_CAPI_BAD_STATE
               return
            end if
            if (size(state%buildings(ib)%Textwall_C)/=int(bldg_nlayer) .or. &
                size(state%buildings(ib)%Textroof_C)/=int(bldg_nlayer)) then
               err = SUEWS_CAPI_BAD_STATE
               return
            end if
         end do
      else
         do ib = 1_c_int, bldg_count
            if (allocated(state%buildings(ib)%Textwall_C)) then
               if (size(state%buildings(ib)%Textwall_C)>0) then
                  err = SUEWS_CAPI_BAD_STATE
                  return
               end if
            end if
            if (allocated(state%buildings(ib)%Textroof_C)) then
               if (size(state%buildings(ib)%Textroof_C)>0) then
                  err = SUEWS_CAPI_BAD_STATE
                  return
               end if
            end if
         end do
      end if
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
      flat(idx) = state%z_array(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = state%dataout_line_u_rsl(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = state%dataout_line_t_rsl(i); idx = idx + 1_c_int
   end do
   do i = 1_c_int, SUEWS_CAPI_STEBBS_STATE_RSL_LEN
      flat(idx) = state%dataout_line_q_rsl(i); idx = idx + 1_c_int
   end do

   flat(idx) = state%deep_soil_temperature; idx = idx + 1_c_int
   flat(idx) = state%monthmeanairtemperature_diffmax; idx = idx + 1_c_int
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
   flat(idx) = state%water_tank_temperature_state; idx = idx + 1_c_int
   flat(idx) = state%internal_wall_water_tank_temperature; idx = idx + 1_c_int
   flat(idx) = state%external_wall_water_tank_temperature; idx = idx + 1_c_int
   flat(idx) = state%mains_water_temperature; idx = idx + 1_c_int
   flat(idx) = state%domestic_hot_water_temperature_in_use_in_building; idx = idx + 1_c_int
   flat(idx) = state%internal_wall_dhw_vessel_temperature; idx = idx + 1_c_int
   flat(idx) = state%external_wall_dhw_vessel_temperature; idx = idx + 1_c_int
   flat(idx) = state%qs_stebbs; idx = idx + 1_c_int
   flat(idx) = real(bldg_count, c_double); idx = idx + 1_c_int
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe); idx = idx + 1_c_int

   do ib = 1_c_int, bldg_count
      flat(idx) = real(state%buildings(ib)%id_lbm, c_double); idx = idx + 1_c_int
      flat(idx) = real(state%buildings(ib)%appliance_totalnumber, c_double); idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QHload_heating_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QHload_cooling_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QH_metabolism; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QE_metabolism; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QHload_dhw_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Qloss_drain_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%ratio_window_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%a_footprint; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%height_building; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%wall_external_area; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%internal_volume_ratio; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%thickness_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%thickness_wallext; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%thickness_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%thickness_roofext; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%thickness_groundfloor; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%depth_ground; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%thickness_window; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_intwall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_introof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_indoormass; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_intgroundfloor; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_intwindow; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_extwall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_extroof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_extwindow; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_wallext; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_roofext; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_groundfloor; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_window; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_ground; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_wallext; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_roofext; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%weighting_factor_heatcapacity_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%weighting_factor_heatcapacity_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_groundfloor; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_window; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_indoormass; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_air_ind; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_wallext; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_roofext; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_groundfloor; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_window; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_indoormass; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_air_ind; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_extwall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_extroof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_intwall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_introof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_indoormass; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_extwindow; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_intwindow; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%window_transmissivity; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%window_absorptivity; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%window_reflectivity; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%wall_transmissivity_state; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%wall_absorptivity; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%wall_reflectivity; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%roof_transmissivity_state; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%roof_absorptivity; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%roof_reflectivity; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%occupants_state; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%metabolic_rate; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%metabolism_threshold; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%ratio_metabolic_latent_sensible; idx = idx + 1_c_int
      flat(idx) = real(state%buildings(ib)%internal_shading, c_double); idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%reduction_factor_shading; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%temperature_threshold_shading; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%radiation_threshold_shading; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%appliance_power_rating; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%lighting_power_rating; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%frac_hotwater; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%maxheatingpower_air; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%heating_efficiency_air; idx = idx + 1_c_int
      flat(idx) = real(state%buildings(ib)%destination_waste_heat, c_double); idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%fraction_convective_heating; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%maxcoolingpower_air; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%coeff_performance_cooling; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Vair_ind; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%ventilation_rate; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%a_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%a_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%v_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%v_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%v_ground_floor; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%a_window; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%v_window; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%v_indoor_mass; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%a_indoor_mass; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Tair_ind; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_indoor_mass; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_int_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_int_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_ext_wall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_ext_roof; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_int_window; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_ext_window; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_int_ground_floor; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%t_ext_ground_floor; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Twater_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Tintwall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Textwall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%thickness_tankwall; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Tincomingwater_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Vwater_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Asurf_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Vwall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%setTwater_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%init_wtTs; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Twater_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Tintwall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Textwall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%thickness_wall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Vwater_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Awater_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%Vwall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%flowrate_water_supply; idx = idx + 1_c_int

      do j = 1_c_int, SUEWS_CAPI_STEBBS_BLDG_PROFILE_GROUPS
         do i = 0_c_int, SUEWS_CAPI_STEBBS_BLDG_PROFILE_STEPS - 1_c_int
            flat(idx) = state%buildings(ib)%flowrate_water_supply_profile(i, j)
            idx = idx + 1_c_int
         end do
      end do

      flat(idx) = state%buildings(ib)%single_flowrate_water_supply; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%single_flowrate_water_drain; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_water; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_wall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%cp_wall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_water; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_wall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%density_wall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%BVF_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%MVF_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_wall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_intwall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_extwall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_extwall_tank; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conductivity_wall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_intwall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%conv_coeff_extwall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%emissivity_extwall_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%maxheatingpower_water; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%heating_efficiency_water; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%minVwater_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%maxVwater_vessel; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%minHeatingPower_DHW; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%HeatingPower_DHW; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%qfm_dom; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%qheat_dom; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%qcool_dom; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%qfb_hw_dom; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%qfb_dom_air; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%dom_temp; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QN_bldg_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QEC_bldg_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QS_total_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QH_bldg_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QBAE_bldg_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QWaste_bldg_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QS_bldg_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QS_dhw_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QS_ground_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QEC_heating_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QEC_cooling_tstepFA; idx = idx + 1_c_int
      flat(idx) = state%buildings(ib)%QEC_dhw_tstepFA; idx = idx + 1_c_int

      do i = 1_c_int, 2_c_int
         flat(idx) = state%buildings(ib)%ts(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 2_c_int
         flat(idx) = state%buildings(ib)%initTs(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 5_c_int
         flat(idx) = state%buildings(ib)%h_i(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 5_c_int
         flat(idx) = state%buildings(ib)%k_eff(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         flat(idx) = state%buildings(ib)%h_o(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 6_c_int
         flat(idx) = state%buildings(ib)%rho(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 6_c_int
         flat(idx) = state%buildings(ib)%cp(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 7_c_int
         flat(idx) = state%buildings(ib)%emis(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         flat(idx) = state%buildings(ib)%witar(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         flat(idx) = state%buildings(ib)%waTAR(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         flat(idx) = state%buildings(ib)%roofTAR(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         flat(idx) = state%buildings(ib)%h_ts_average(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         flat(idx) = state%buildings(ib)%HWTsAverage(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, 3_c_int
         flat(idx) = state%buildings(ib)%hw_power_average(i); idx = idx + 1_c_int
      end do
      do i = 1_c_int, SUEWS_CAPI_STEBBS_BLDG_ENERGY_LEN
         flat(idx) = state%buildings(ib)%energy_exchanges(i); idx = idx + 1_c_int
      end do

      if (bldg_nlayer>0_c_int) then
         do il = 1_c_int, bldg_nlayer
            flat(idx) = state%buildings(ib)%Textwall_C(il); idx = idx + 1_c_int
         end do
         do il = 1_c_int, bldg_nlayer
            flat(idx) = state%buildings(ib)%Textroof_C(il); idx = idx + 1_c_int
         end do
      end if

      flat(idx) = merge(1.0_c_double, 0.0_c_double, state%buildings(ib)%wall_surface_active); idx = idx + 1_c_int
      flat(idx) = merge(1.0_c_double, 0.0_c_double, state%buildings(ib)%window_surface_active); idx = idx + 1_c_int
      flat(idx) = merge(1.0_c_double, 0.0_c_double, state%buildings(ib)%iter_safe); idx = idx + 1_c_int
   end do

   err = SUEWS_CAPI_OK

end subroutine stebbs_state_pack_real

subroutine suews_stebbs_state_roundtrip( &
   in_flat, n_in, in_bldg_nlayer, out_flat, n_out, out_bldg_nlayer, err) &
   bind(C, name='suews_stebbs_state_roundtrip')
   implicit none

   real(c_double), intent(in) :: in_flat(*)
   integer(c_int), value, intent(in) :: n_in
   integer(c_int), value, intent(in) :: in_bldg_nlayer
   real(c_double), intent(out) :: out_flat(*)
   integer(c_int), value, intent(in) :: n_out
   integer(c_int), value, intent(in) :: out_bldg_nlayer
   integer(c_int), intent(out) :: err

   type(STEBBS_STATE) :: state
   integer(c_int) :: local_err

   call stebbs_state_unpack(in_flat, n_in, in_bldg_nlayer, state, local_err)
   if (local_err/=SUEWS_CAPI_OK) then
      call state%DEALLOCATE()
      err = local_err
      return
   end if

   call stebbs_state_pack_real(state, out_bldg_nlayer, out_flat, n_out, local_err)
   call state%DEALLOCATE()
   err = local_err

end subroutine suews_stebbs_state_roundtrip

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
