! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for LC_EVETR_PRM.
! -----------------------------------------------------------------------------
module module_c_api_lc_evetr_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_landcover, only: LC_EVETR_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_LC_EVETR_PRM_LEN = 57_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_LC_EVETR_PRM_SCHEMA_VERSION = 1_c_int

type :: ohm_coef_lc_shadow
   real(c_double) :: summer_dry = 0.0_c_double
   real(c_double) :: summer_wet = 0.0_c_double
   real(c_double) :: winter_dry = 0.0_c_double
   real(c_double) :: winter_wet = 0.0_c_double
end type ohm_coef_lc_shadow

type :: ohm_prm_shadow
   real(c_double) :: chanohm = 0.0_c_double
   real(c_double) :: cpanohm = 0.0_c_double
   real(c_double) :: kkanohm = 0.0_c_double
   real(c_double) :: ohm_threshsw = 0.0_c_double
   real(c_double) :: ohm_threshwd = 0.0_c_double
   type(ohm_coef_lc_shadow), dimension(3) :: ohm_coef_lc
end type ohm_prm_shadow

type :: soil_prm_shadow
   real(c_double) :: soildepth = 0.0_c_double
   real(c_double) :: soilstorecap = 0.0_c_double
   real(c_double) :: sathydraulicconduct = 0.0_c_double
end type soil_prm_shadow

type :: bioco2_prm_shadow
   real(c_double) :: beta_bioco2 = 0.0_c_double
   real(c_double) :: beta_enh_bioco2 = 0.0_c_double
   real(c_double) :: alpha_bioco2 = 0.0_c_double
   real(c_double) :: alpha_enh_bioco2 = 0.0_c_double
   real(c_double) :: resp_a = 0.0_c_double
   real(c_double) :: resp_b = 0.0_c_double
   real(c_double) :: theta_bioco2 = 0.0_c_double
   real(c_double) :: min_res_bioco2 = 0.0_c_double
end type bioco2_prm_shadow

type :: lai_prm_shadow
   real(c_double) :: baset = 0.0_c_double
   real(c_double) :: gddfull = 0.0_c_double
   real(c_double) :: basete = 0.0_c_double
   real(c_double) :: sddfull = 0.0_c_double
   real(c_double) :: laimin = 0.0_c_double
   real(c_double) :: laimax = 0.0_c_double
   real(c_double), dimension(4) :: laipower = 0.0_c_double
   integer(c_int) :: laitype = 0_c_int
end type lai_prm_shadow

type :: water_dist_prm_shadow
   real(c_double) :: to_paved = 0.0_c_double
   real(c_double) :: to_bldg = 0.0_c_double
   real(c_double) :: to_evetr = 0.0_c_double
   real(c_double) :: to_dectr = 0.0_c_double
   real(c_double) :: to_grass = 0.0_c_double
   real(c_double) :: to_bsoil = 0.0_c_double
   real(c_double) :: to_water = 0.0_c_double
   real(c_double) :: to_soilstore = 0.0_c_double
end type water_dist_prm_shadow

type :: lc_evetr_prm_shadow
   real(c_double) :: sfr = 0.0_c_double
   real(c_double) :: emis = 0.0_c_double
   real(c_double) :: faievetree = 0.0_c_double
   real(c_double) :: evetreeh = 0.0_c_double
   real(c_double) :: alb_min = 0.0_c_double
   real(c_double) :: alb_max = 0.0_c_double
   type(ohm_prm_shadow) :: ohm
   type(soil_prm_shadow) :: soil
   real(c_double) :: statelimit = 0.0_c_double
   real(c_double) :: irrfracevetr = 0.0_c_double
   real(c_double) :: wetthresh = 0.0_c_double
   type(bioco2_prm_shadow) :: bioco2
   real(c_double) :: maxconductance = 0.0_c_double
   type(lai_prm_shadow) :: lai
   type(water_dist_prm_shadow) :: waterdist
end type lc_evetr_prm_shadow

public :: suews_lc_evetr_prm_len
public :: suews_lc_evetr_prm_schema_version
public :: suews_lc_evetr_prm_default
public :: suews_lc_evetr_prm_error_message
public :: lc_evetr_prm_unpack

contains

subroutine suews_lc_evetr_prm_len(n_flat, err) bind(C, name='suews_lc_evetr_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_LC_EVETR_PRM_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_lc_evetr_prm_len

subroutine suews_lc_evetr_prm_schema_version(schema_version, err) bind(C, name='suews_lc_evetr_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_LC_EVETR_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_lc_evetr_prm_schema_version

subroutine suews_lc_evetr_prm_default(flat, n_flat, err) bind(C, name='suews_lc_evetr_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(lc_evetr_prm_shadow) :: state

   call lc_evetr_prm_pack(state, flat, n_flat, err)

end subroutine suews_lc_evetr_prm_default

subroutine lc_evetr_prm_pack(state, flat, n_flat, err)
   implicit none

   type(lc_evetr_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer :: i

   if (n_flat<SUEWS_CAPI_LC_EVETR_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   flat(idx) = state%sfr; idx = idx + 1
   flat(idx) = state%emis; idx = idx + 1
   flat(idx) = state%faievetree; idx = idx + 1
   flat(idx) = state%evetreeh; idx = idx + 1
   flat(idx) = state%alb_min; idx = idx + 1
   flat(idx) = state%alb_max; idx = idx + 1

   flat(idx) = state%ohm%chanohm; idx = idx + 1
   flat(idx) = state%ohm%cpanohm; idx = idx + 1
   flat(idx) = state%ohm%kkanohm; idx = idx + 1
   flat(idx) = state%ohm%ohm_threshsw; idx = idx + 1
   flat(idx) = state%ohm%ohm_threshwd; idx = idx + 1
   do i = 1, 3
      flat(idx) = state%ohm%ohm_coef_lc(i)%summer_dry; idx = idx + 1
      flat(idx) = state%ohm%ohm_coef_lc(i)%summer_wet; idx = idx + 1
      flat(idx) = state%ohm%ohm_coef_lc(i)%winter_dry; idx = idx + 1
      flat(idx) = state%ohm%ohm_coef_lc(i)%winter_wet; idx = idx + 1
   end do

   flat(idx) = state%soil%soildepth; idx = idx + 1
   flat(idx) = state%soil%soilstorecap; idx = idx + 1
   flat(idx) = state%soil%sathydraulicconduct; idx = idx + 1

   flat(idx) = state%statelimit; idx = idx + 1
   flat(idx) = state%irrfracevetr; idx = idx + 1
   flat(idx) = state%wetthresh; idx = idx + 1

   flat(idx) = state%bioco2%beta_bioco2; idx = idx + 1
   flat(idx) = state%bioco2%beta_enh_bioco2; idx = idx + 1
   flat(idx) = state%bioco2%alpha_bioco2; idx = idx + 1
   flat(idx) = state%bioco2%alpha_enh_bioco2; idx = idx + 1
   flat(idx) = state%bioco2%resp_a; idx = idx + 1
   flat(idx) = state%bioco2%resp_b; idx = idx + 1
   flat(idx) = state%bioco2%theta_bioco2; idx = idx + 1
   flat(idx) = state%bioco2%min_res_bioco2; idx = idx + 1

   flat(idx) = state%maxconductance; idx = idx + 1

   flat(idx) = state%lai%baset; idx = idx + 1
   flat(idx) = state%lai%gddfull; idx = idx + 1
   flat(idx) = state%lai%basete; idx = idx + 1
   flat(idx) = state%lai%sddfull; idx = idx + 1
   flat(idx) = state%lai%laimin; idx = idx + 1
   flat(idx) = state%lai%laimax; idx = idx + 1
   do i = 1, 4
      flat(idx) = state%lai%laipower(i); idx = idx + 1
   end do
   flat(idx) = real(state%lai%laitype, c_double); idx = idx + 1

   flat(idx) = state%waterdist%to_paved; idx = idx + 1
   flat(idx) = state%waterdist%to_bldg; idx = idx + 1
   flat(idx) = state%waterdist%to_evetr; idx = idx + 1
   flat(idx) = state%waterdist%to_dectr; idx = idx + 1
   flat(idx) = state%waterdist%to_grass; idx = idx + 1
   flat(idx) = state%waterdist%to_bsoil; idx = idx + 1
   flat(idx) = state%waterdist%to_water; idx = idx + 1
   flat(idx) = state%waterdist%to_soilstore; idx = idx + 1

   err = SUEWS_CAPI_OK

end subroutine lc_evetr_prm_pack

subroutine lc_evetr_prm_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(LC_EVETR_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_LC_EVETR_PRM_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   state%sfr = flat(idx); idx = idx + 1_c_int
   state%emis = flat(idx); idx = idx + 1_c_int
   state%faievetree = flat(idx); idx = idx + 1_c_int
   state%evetreeh = flat(idx); idx = idx + 1_c_int
   state%alb_min = flat(idx); idx = idx + 1_c_int
   state%alb_max = flat(idx); idx = idx + 1_c_int

   state%ohm%chanohm = flat(idx); idx = idx + 1_c_int
   state%ohm%cpanohm = flat(idx); idx = idx + 1_c_int
   state%ohm%kkanohm = flat(idx); idx = idx + 1_c_int
   state%ohm%ohm_threshsw = flat(idx); idx = idx + 1_c_int
   state%ohm%ohm_threshwd = flat(idx); idx = idx + 1_c_int
   do i = 1_c_int, 3_c_int
      state%ohm%ohm_coef_lc(i)%summer_dry = flat(idx); idx = idx + 1_c_int
      state%ohm%ohm_coef_lc(i)%summer_wet = flat(idx); idx = idx + 1_c_int
      state%ohm%ohm_coef_lc(i)%winter_dry = flat(idx); idx = idx + 1_c_int
      state%ohm%ohm_coef_lc(i)%winter_wet = flat(idx); idx = idx + 1_c_int
   end do

   state%soil%soildepth = flat(idx); idx = idx + 1_c_int
   state%soil%soilstorecap = flat(idx); idx = idx + 1_c_int
   state%soil%sathydraulicconduct = flat(idx); idx = idx + 1_c_int

   state%statelimit = flat(idx); idx = idx + 1_c_int
   state%irrfracevetr = flat(idx); idx = idx + 1_c_int
   state%wetthresh = flat(idx); idx = idx + 1_c_int

   state%bioco2%beta_bioco2 = flat(idx); idx = idx + 1_c_int
   state%bioco2%beta_enh_bioco2 = flat(idx); idx = idx + 1_c_int
   state%bioco2%alpha_bioco2 = flat(idx); idx = idx + 1_c_int
   state%bioco2%alpha_enh_bioco2 = flat(idx); idx = idx + 1_c_int
   state%bioco2%resp_a = flat(idx); idx = idx + 1_c_int
   state%bioco2%resp_b = flat(idx); idx = idx + 1_c_int
   state%bioco2%theta_bioco2 = flat(idx); idx = idx + 1_c_int
   state%bioco2%min_res_bioco2 = flat(idx); idx = idx + 1_c_int

   state%maxconductance = flat(idx); idx = idx + 1_c_int

   state%lai%baset = flat(idx); idx = idx + 1_c_int
   state%lai%gddfull = flat(idx); idx = idx + 1_c_int
   state%lai%basete = flat(idx); idx = idx + 1_c_int
   state%lai%sddfull = flat(idx); idx = idx + 1_c_int
   state%lai%laimin = flat(idx); idx = idx + 1_c_int
   state%lai%laimax = flat(idx); idx = idx + 1_c_int
   do i = 1_c_int, 4_c_int
      state%lai%laipower(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   state%lai%laitype = int(nint(flat(idx))); idx = idx + 1_c_int

   state%waterdist%to_paved = flat(idx); idx = idx + 1_c_int
   state%waterdist%to_bldg = flat(idx); idx = idx + 1_c_int
   state%waterdist%to_evetr = flat(idx); idx = idx + 1_c_int
   state%waterdist%to_dectr = flat(idx); idx = idx + 1_c_int
   state%waterdist%to_grass = flat(idx); idx = idx + 1_c_int
   state%waterdist%to_bsoil = flat(idx); idx = idx + 1_c_int
   state%waterdist%to_water = flat(idx); idx = idx + 1_c_int
   state%waterdist%to_soilstore = flat(idx)

   err = SUEWS_CAPI_OK

end subroutine lc_evetr_prm_unpack

subroutine suews_lc_evetr_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_lc_evetr_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_lc_evetr_prm_error_message

end module module_c_api_lc_evetr_prm

module c_api_lc_evetr_prm_module
use module_c_api_lc_evetr_prm
end module c_api_lc_evetr_prm_module
