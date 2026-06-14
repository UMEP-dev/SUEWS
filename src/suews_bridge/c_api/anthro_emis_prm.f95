! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for anthroEMIS_PRM.
! -----------------------------------------------------------------------------
module module_c_api_anthro_emis_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_anthro, only: anthroEMIS_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_ANTHRO_EMIS_PRM_LEN = 228_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_ANTHRO_EMIS_PRM_SCHEMA_VERSION = 1_c_int

type :: anthro_heat_prm_shadow
   real(c_double) :: qf0_beu_working = 0.0_c_double
   real(c_double) :: qf0_beu_holiday = 0.0_c_double
   real(c_double) :: qf_a_working = 0.0_c_double
   real(c_double) :: qf_a_holiday = 0.0_c_double
   real(c_double) :: qf_b_working = 0.0_c_double
   real(c_double) :: qf_b_holiday = 0.0_c_double
   real(c_double) :: qf_c_working = 0.0_c_double
   real(c_double) :: qf_c_holiday = 0.0_c_double
   real(c_double) :: baset_cooling_working = 0.0_c_double
   real(c_double) :: baset_cooling_holiday = 0.0_c_double
   real(c_double) :: baset_heating_working = 0.0_c_double
   real(c_double) :: baset_heating_holiday = 0.0_c_double
   real(c_double) :: ah_min_working = 0.0_c_double
   real(c_double) :: ah_min_holiday = 0.0_c_double
   real(c_double) :: ah_slope_cooling_working = 0.0_c_double
   real(c_double) :: ah_slope_cooling_holiday = 0.0_c_double
   real(c_double) :: ah_slope_heating_working = 0.0_c_double
   real(c_double) :: ah_slope_heating_holiday = 0.0_c_double
   real(c_double), dimension(24) :: ahprof_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: ahprof_24hr_holiday = 0.0_c_double
   real(c_double) :: pop_density_daytime_working = 0.0_c_double
   real(c_double) :: pop_density_daytime_holiday = 0.0_c_double
   real(c_double) :: pop_density_nighttime = 0.0_c_double
   real(c_double), dimension(24) :: popprof_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: popprof_24hr_holiday = 0.0_c_double
end type anthro_heat_prm_shadow

type :: anthro_emis_prm_shadow
   integer(c_int) :: start_dls = 0_c_int
   integer(c_int) :: end_dls = 0_c_int
   type(anthro_heat_prm_shadow) :: anthro_heat
   real(c_double) :: ef_umol_co2_per_j = 0.0_c_double
   real(c_double) :: en_ef_v_jkm = 0.0_c_double
   real(c_double) :: fr_fossil_fuel_heat = 0.0_c_double
   real(c_double) :: fr_fossil_fuel_non_heat = 0.0_c_double
   real(c_double), dimension(2) :: fc_ef_v_kgkm = 0.0_c_double
   real(c_double), dimension(24) :: hum_activity_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: hum_activity_24hr_holiday = 0.0_c_double
   real(c_double) :: max_fc_metab = 0.0_c_double
   real(c_double) :: max_qf_metab = 0.0_c_double
   real(c_double) :: min_fc_metab = 0.0_c_double
   real(c_double) :: min_qf_metab = 0.0_c_double
   real(c_double) :: traffic_rate_working = 0.0_c_double
   real(c_double) :: traffic_rate_holiday = 0.0_c_double
   real(c_double) :: traffic_units = 0.0_c_double
   real(c_double), dimension(24) :: traff_prof_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: traff_prof_24hr_holiday = 0.0_c_double
end type anthro_emis_prm_shadow

public :: suews_anthro_emis_prm_len
public :: suews_anthro_emis_prm_schema_version
public :: suews_anthro_emis_prm_default
public :: suews_anthro_emis_prm_error_message
public :: anthro_emis_prm_unpack

contains

subroutine suews_anthro_emis_prm_len(n_flat, err) bind(C, name='suews_anthro_emis_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_ANTHRO_EMIS_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_anthro_emis_prm_len

subroutine suews_anthro_emis_prm_schema_version(schema_version, err) bind(C, name='suews_anthro_emis_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_ANTHRO_EMIS_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_anthro_emis_prm_schema_version

subroutine suews_anthro_emis_prm_default(flat, n_flat, err) bind(C, name='suews_anthro_emis_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(anthro_emis_prm_shadow) :: state

   call anthro_emis_prm_pack(state, flat, n_flat, err)

end subroutine suews_anthro_emis_prm_default

subroutine anthro_emis_prm_pack(state, flat, n_flat, err)
   implicit none

   type(anthro_emis_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer :: i

   if (n_flat<SUEWS_CAPI_ANTHRO_EMIS_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   flat(idx) = real(state%start_dls, c_double); idx = idx + 1
   flat(idx) = real(state%end_dls, c_double); idx = idx + 1

   flat(idx) = state%anthro_heat%qf0_beu_working; idx = idx + 1
   flat(idx) = state%anthro_heat%qf0_beu_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%qf_a_working; idx = idx + 1
   flat(idx) = state%anthro_heat%qf_a_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%qf_b_working; idx = idx + 1
   flat(idx) = state%anthro_heat%qf_b_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%qf_c_working; idx = idx + 1
   flat(idx) = state%anthro_heat%qf_c_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%baset_cooling_working; idx = idx + 1
   flat(idx) = state%anthro_heat%baset_cooling_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%baset_heating_working; idx = idx + 1
   flat(idx) = state%anthro_heat%baset_heating_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%ah_min_working; idx = idx + 1
   flat(idx) = state%anthro_heat%ah_min_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%ah_slope_cooling_working; idx = idx + 1
   flat(idx) = state%anthro_heat%ah_slope_cooling_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%ah_slope_heating_working; idx = idx + 1
   flat(idx) = state%anthro_heat%ah_slope_heating_holiday; idx = idx + 1

   do i = 1, 24
      flat(idx) = state%anthro_heat%ahprof_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%anthro_heat%ahprof_24hr_holiday(i)
      idx = idx + 1
   end do

   flat(idx) = state%anthro_heat%pop_density_daytime_working; idx = idx + 1
   flat(idx) = state%anthro_heat%pop_density_daytime_holiday; idx = idx + 1
   flat(idx) = state%anthro_heat%pop_density_nighttime; idx = idx + 1

   do i = 1, 24
      flat(idx) = state%anthro_heat%popprof_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%anthro_heat%popprof_24hr_holiday(i)
      idx = idx + 1
   end do

   flat(idx) = state%ef_umol_co2_per_j; idx = idx + 1
   flat(idx) = state%en_ef_v_jkm; idx = idx + 1
   flat(idx) = state%fr_fossil_fuel_heat; idx = idx + 1
   flat(idx) = state%fr_fossil_fuel_non_heat; idx = idx + 1

   do i = 1, 2
      flat(idx) = state%fc_ef_v_kgkm(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%hum_activity_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%hum_activity_24hr_holiday(i)
      idx = idx + 1
   end do

   flat(idx) = state%max_fc_metab; idx = idx + 1
   flat(idx) = state%max_qf_metab; idx = idx + 1
   flat(idx) = state%min_fc_metab; idx = idx + 1
   flat(idx) = state%min_qf_metab; idx = idx + 1
   flat(idx) = state%traffic_rate_working; idx = idx + 1
   flat(idx) = state%traffic_rate_holiday; idx = idx + 1
   flat(idx) = state%traffic_units; idx = idx + 1

   do i = 1, 24
      flat(idx) = state%traff_prof_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%traff_prof_24hr_holiday(i)
      idx = idx + 1
   end do

   err = SUEWS_CAPI_OK

end subroutine anthro_emis_prm_pack

subroutine anthro_emis_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(anthroEMIS_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_ANTHRO_EMIS_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   state%start_dls = int(nint(flat(idx))); idx = idx + 1_c_int
   state%end_dls = int(nint(flat(idx))); idx = idx + 1_c_int

   state%anthro_heat%qf0_beu_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%qf0_beu_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%qf_a_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%qf_a_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%qf_b_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%qf_b_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%qf_c_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%qf_c_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%baset_cooling_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%baset_cooling_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%baset_heating_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%baset_heating_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%ah_min_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%ah_min_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%ah_slope_cooling_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%ah_slope_cooling_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%ah_slope_heating_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%ah_slope_heating_holiday = flat(idx); idx = idx + 1_c_int

   do i = 0_c_int, 23_c_int
      state%anthro_heat%ahprof_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%anthro_heat%ahprof_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%anthro_heat%pop_density_daytime_working = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%pop_density_daytime_holiday = flat(idx); idx = idx + 1_c_int
   state%anthro_heat%pop_density_nighttime = flat(idx); idx = idx + 1_c_int

   do i = 0_c_int, 23_c_int
      state%anthro_heat%popprof_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%anthro_heat%popprof_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%ef_umol_co2_per_j = flat(idx); idx = idx + 1_c_int
   state%en_ef_v_jkm = flat(idx); idx = idx + 1_c_int
   state%fr_fossil_fuel_heat = flat(idx); idx = idx + 1_c_int
   state%fr_fossil_fuel_non_heat = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, 2_c_int
      state%fc_ef_v_kgkm(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%hum_activity_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%hum_activity_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%max_fc_metab = flat(idx); idx = idx + 1_c_int
   state%max_qf_metab = flat(idx); idx = idx + 1_c_int
   state%min_fc_metab = flat(idx); idx = idx + 1_c_int
   state%min_qf_metab = flat(idx); idx = idx + 1_c_int
   state%traffic_rate_working = flat(idx); idx = idx + 1_c_int
   state%traffic_rate_holiday = flat(idx); idx = idx + 1_c_int
   state%traffic_units = flat(idx); idx = idx + 1_c_int

   do i = 0_c_int, 23_c_int
      state%traff_prof_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%traff_prof_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   err = SUEWS_CAPI_OK

end subroutine anthro_emis_prm_unpack

subroutine suews_anthro_emis_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_anthro_emis_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_anthro_emis_prm_error_message

end module module_c_api_anthro_emis_prm

module c_api_anthro_emis_prm_module
use module_c_api_anthro_emis_prm
end module c_api_anthro_emis_prm_module
