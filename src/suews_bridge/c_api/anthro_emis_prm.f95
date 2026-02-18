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
   real(c_double) :: popdensdaytime_working = 0.0_c_double
   real(c_double) :: popdensdaytime_holiday = 0.0_c_double
   real(c_double) :: popdensnighttime = 0.0_c_double
   real(c_double), dimension(24) :: popprof_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: popprof_24hr_holiday = 0.0_c_double
end type anthro_heat_prm_shadow

type :: anthro_emis_prm_shadow
   integer(c_int) :: startdls = 0_c_int
   integer(c_int) :: enddls = 0_c_int
   type(anthro_heat_prm_shadow) :: anthroheat
   real(c_double) :: ef_umolco2perj = 0.0_c_double
   real(c_double) :: enef_v_jkm = 0.0_c_double
   real(c_double) :: frfossilfuel_heat = 0.0_c_double
   real(c_double) :: frfossilfuel_nonheat = 0.0_c_double
   real(c_double), dimension(2) :: fcef_v_kgkm = 0.0_c_double
   real(c_double), dimension(24) :: humactivity_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: humactivity_24hr_holiday = 0.0_c_double
   real(c_double) :: maxfcmetab = 0.0_c_double
   real(c_double) :: maxqfmetab = 0.0_c_double
   real(c_double) :: minfcmetab = 0.0_c_double
   real(c_double) :: minqfmetab = 0.0_c_double
   real(c_double) :: trafficrate_working = 0.0_c_double
   real(c_double) :: trafficrate_holiday = 0.0_c_double
   real(c_double) :: trafficunits = 0.0_c_double
   real(c_double), dimension(24) :: traffprof_24hr_working = 0.0_c_double
   real(c_double), dimension(24) :: traffprof_24hr_holiday = 0.0_c_double
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
   flat(idx) = real(state%startdls, c_double); idx = idx + 1
   flat(idx) = real(state%enddls, c_double); idx = idx + 1

   flat(idx) = state%anthroheat%qf0_beu_working; idx = idx + 1
   flat(idx) = state%anthroheat%qf0_beu_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%qf_a_working; idx = idx + 1
   flat(idx) = state%anthroheat%qf_a_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%qf_b_working; idx = idx + 1
   flat(idx) = state%anthroheat%qf_b_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%qf_c_working; idx = idx + 1
   flat(idx) = state%anthroheat%qf_c_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%baset_cooling_working; idx = idx + 1
   flat(idx) = state%anthroheat%baset_cooling_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%baset_heating_working; idx = idx + 1
   flat(idx) = state%anthroheat%baset_heating_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%ah_min_working; idx = idx + 1
   flat(idx) = state%anthroheat%ah_min_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%ah_slope_cooling_working; idx = idx + 1
   flat(idx) = state%anthroheat%ah_slope_cooling_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%ah_slope_heating_working; idx = idx + 1
   flat(idx) = state%anthroheat%ah_slope_heating_holiday; idx = idx + 1

   do i = 1, 24
      flat(idx) = state%anthroheat%ahprof_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%anthroheat%ahprof_24hr_holiday(i)
      idx = idx + 1
   end do

   flat(idx) = state%anthroheat%popdensdaytime_working; idx = idx + 1
   flat(idx) = state%anthroheat%popdensdaytime_holiday; idx = idx + 1
   flat(idx) = state%anthroheat%popdensnighttime; idx = idx + 1

   do i = 1, 24
      flat(idx) = state%anthroheat%popprof_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%anthroheat%popprof_24hr_holiday(i)
      idx = idx + 1
   end do

   flat(idx) = state%ef_umolco2perj; idx = idx + 1
   flat(idx) = state%enef_v_jkm; idx = idx + 1
   flat(idx) = state%frfossilfuel_heat; idx = idx + 1
   flat(idx) = state%frfossilfuel_nonheat; idx = idx + 1

   do i = 1, 2
      flat(idx) = state%fcef_v_kgkm(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%humactivity_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%humactivity_24hr_holiday(i)
      idx = idx + 1
   end do

   flat(idx) = state%maxfcmetab; idx = idx + 1
   flat(idx) = state%maxqfmetab; idx = idx + 1
   flat(idx) = state%minfcmetab; idx = idx + 1
   flat(idx) = state%minqfmetab; idx = idx + 1
   flat(idx) = state%trafficrate_working; idx = idx + 1
   flat(idx) = state%trafficrate_holiday; idx = idx + 1
   flat(idx) = state%trafficunits; idx = idx + 1

   do i = 1, 24
      flat(idx) = state%traffprof_24hr_working(i)
      idx = idx + 1
   end do

   do i = 1, 24
      flat(idx) = state%traffprof_24hr_holiday(i)
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
   state%startdls = int(nint(flat(idx))); idx = idx + 1_c_int
   state%enddls = int(nint(flat(idx))); idx = idx + 1_c_int

   state%anthroheat%qf0_beu_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%qf0_beu_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%qf_a_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%qf_a_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%qf_b_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%qf_b_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%qf_c_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%qf_c_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%baset_cooling_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%baset_cooling_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%baset_heating_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%baset_heating_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%ah_min_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%ah_min_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%ah_slope_cooling_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%ah_slope_cooling_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%ah_slope_heating_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%ah_slope_heating_holiday = flat(idx); idx = idx + 1_c_int

   do i = 0_c_int, 23_c_int
      state%anthroheat%ahprof_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%anthroheat%ahprof_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%anthroheat%popdensdaytime_working = flat(idx); idx = idx + 1_c_int
   state%anthroheat%popdensdaytime_holiday = flat(idx); idx = idx + 1_c_int
   state%anthroheat%popdensnighttime = flat(idx); idx = idx + 1_c_int

   do i = 0_c_int, 23_c_int
      state%anthroheat%popprof_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%anthroheat%popprof_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%EF_umolCO2perJ = flat(idx); idx = idx + 1_c_int
   state%EnEF_v_Jkm = flat(idx); idx = idx + 1_c_int
   state%FrFossilFuel_Heat = flat(idx); idx = idx + 1_c_int
   state%FrFossilFuel_NonHeat = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, 2_c_int
      state%FcEF_v_kgkm(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%HumActivity_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%HumActivity_24hr_holiday(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%MaxFCMetab = flat(idx); idx = idx + 1_c_int
   state%MaxQFMetab = flat(idx); idx = idx + 1_c_int
   state%MinFCMetab = flat(idx); idx = idx + 1_c_int
   state%MinQFMetab = flat(idx); idx = idx + 1_c_int
   state%TrafficRate_working = flat(idx); idx = idx + 1_c_int
   state%TrafficRate_holiday = flat(idx); idx = idx + 1_c_int
   state%TrafficUnits = flat(idx); idx = idx + 1_c_int

   do i = 0_c_int, 23_c_int
      state%TraffProf_24hr_working(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 0_c_int, 23_c_int
      state%TraffProf_24hr_holiday(i) = flat(idx)
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
