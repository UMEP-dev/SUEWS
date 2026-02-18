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
integer(c_int), parameter, public :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_LEN = 639_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION = 1_c_int

type :: building_archetype_prm_shadow
   real(c_double) :: buildingcount = 0.0_c_double
   real(c_double) :: occupants = 0.0_c_double
   real(c_double) :: hhs0 = 0.0_c_double
   real(c_double) :: age_0_4 = 0.0_c_double
   real(c_double) :: age_5_11 = 0.0_c_double
   real(c_double) :: age_12_18 = 0.0_c_double
   real(c_double) :: age_19_64 = 0.0_c_double
   real(c_double) :: age_65plus = 0.0_c_double
   real(c_double) :: stebbs_height = 0.0_c_double
   real(c_double) :: footprintarea = 0.0_c_double
   real(c_double) :: wallexternalarea = 0.0_c_double
   real(c_double) :: ratiointernalvolume = 0.0_c_double
   real(c_double) :: wwr = 0.0_c_double
   real(c_double) :: wallthickness = 0.0_c_double
   real(c_double) :: walleffectiveconductivity = 0.0_c_double
   real(c_double) :: walldensity = 0.0_c_double
   real(c_double) :: wallcp = 0.0_c_double
   real(c_double) :: wallextthickness = 0.0_c_double
   real(c_double) :: wallexteffectiveconductivity = 0.0_c_double
   real(c_double) :: wallextdensity = 0.0_c_double
   real(c_double) :: wallextcp = 0.0_c_double
   real(c_double) :: walloutercapfrac = 0.0_c_double
   real(c_double) :: wallexternalemissivity = 0.0_c_double
   real(c_double) :: wallinternalemissivity = 0.0_c_double
   real(c_double) :: walltransmissivity = 0.0_c_double
   real(c_double) :: wallabsorbtivity = 0.0_c_double
   real(c_double) :: wallreflectivity = 0.0_c_double
   real(c_double) :: roofthickness = 0.0_c_double
   real(c_double) :: roofeffectiveconductivity = 0.0_c_double
   real(c_double) :: roofdensity = 0.0_c_double
   real(c_double) :: roofcp = 0.0_c_double
   real(c_double) :: roofextthickness = 0.0_c_double
   real(c_double) :: roofexteffectiveconductivity = 0.0_c_double
   real(c_double) :: roofextdensity = 0.0_c_double
   real(c_double) :: roofextcp = 0.0_c_double
   real(c_double) :: roofoutercapfrac = 0.0_c_double
   real(c_double) :: roofexternalemissivity = 0.0_c_double
   real(c_double) :: roofinternalemissivity = 0.0_c_double
   real(c_double) :: rooftransmissivity = 0.0_c_double
   real(c_double) :: roofabsorbtivity = 0.0_c_double
   real(c_double) :: roofreflectivity = 0.0_c_double
   real(c_double) :: floorthickness = 0.0_c_double
   real(c_double) :: groundflooreffectiveconductivity = 0.0_c_double
   real(c_double) :: groundfloordensity = 0.0_c_double
   real(c_double) :: groundfloorcp = 0.0_c_double
   real(c_double) :: windowthickness = 0.0_c_double
   real(c_double) :: windoweffectiveconductivity = 0.0_c_double
   real(c_double) :: windowdensity = 0.0_c_double
   real(c_double) :: windowcp = 0.0_c_double
   real(c_double) :: windowexternalemissivity = 0.0_c_double
   real(c_double) :: windowinternalemissivity = 0.0_c_double
   real(c_double) :: windowtransmissivity = 0.0_c_double
   real(c_double) :: windowabsorbtivity = 0.0_c_double
   real(c_double) :: windowreflectivity = 0.0_c_double
   real(c_double) :: internalmassdensity = 0.0_c_double
   real(c_double) :: internalmasscp = 0.0_c_double
   real(c_double) :: internalmassemissivity = 0.0_c_double
   real(c_double) :: maxheatingpower = 0.0_c_double
   real(c_double) :: watertankwatervolume = 0.0_c_double
   real(c_double) :: maximumhotwaterheatingpower = 0.0_c_double
   real(c_double) :: heatingsetpointtemperature = 0.0_c_double
   real(c_double) :: coolingsetpointtemperature = 0.0_c_double
   real(c_double), dimension(0:143, 2) :: metabolismprofile = 0.0_c_double
   real(c_double), dimension(0:143, 2) :: applianceprofile = 0.0_c_double
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

   flat(idx) = state%buildingcount; idx = idx + 1_c_int
   flat(idx) = state%occupants; idx = idx + 1_c_int
   flat(idx) = state%hhs0; idx = idx + 1_c_int
   flat(idx) = state%age_0_4; idx = idx + 1_c_int
   flat(idx) = state%age_5_11; idx = idx + 1_c_int
   flat(idx) = state%age_12_18; idx = idx + 1_c_int
   flat(idx) = state%age_19_64; idx = idx + 1_c_int
   flat(idx) = state%age_65plus; idx = idx + 1_c_int
   flat(idx) = state%stebbs_height; idx = idx + 1_c_int
   flat(idx) = state%footprintarea; idx = idx + 1_c_int
   flat(idx) = state%wallexternalarea; idx = idx + 1_c_int
   flat(idx) = state%ratiointernalvolume; idx = idx + 1_c_int
   flat(idx) = state%wwr; idx = idx + 1_c_int
   flat(idx) = state%wallthickness; idx = idx + 1_c_int
   flat(idx) = state%walleffectiveconductivity; idx = idx + 1_c_int
   flat(idx) = state%walldensity; idx = idx + 1_c_int
   flat(idx) = state%wallcp; idx = idx + 1_c_int
   flat(idx) = state%wallextthickness; idx = idx + 1_c_int
   flat(idx) = state%wallexteffectiveconductivity; idx = idx + 1_c_int
   flat(idx) = state%wallextdensity; idx = idx + 1_c_int
   flat(idx) = state%wallextcp; idx = idx + 1_c_int
   flat(idx) = state%walloutercapfrac; idx = idx + 1_c_int
   flat(idx) = state%wallexternalemissivity; idx = idx + 1_c_int
   flat(idx) = state%wallinternalemissivity; idx = idx + 1_c_int
   flat(idx) = state%walltransmissivity; idx = idx + 1_c_int
   flat(idx) = state%wallabsorbtivity; idx = idx + 1_c_int
   flat(idx) = state%wallreflectivity; idx = idx + 1_c_int
   flat(idx) = state%roofthickness; idx = idx + 1_c_int
   flat(idx) = state%roofeffectiveconductivity; idx = idx + 1_c_int
   flat(idx) = state%roofdensity; idx = idx + 1_c_int
   flat(idx) = state%roofcp; idx = idx + 1_c_int
   flat(idx) = state%roofextthickness; idx = idx + 1_c_int
   flat(idx) = state%roofexteffectiveconductivity; idx = idx + 1_c_int
   flat(idx) = state%roofextdensity; idx = idx + 1_c_int
   flat(idx) = state%roofextcp; idx = idx + 1_c_int
   flat(idx) = state%roofoutercapfrac; idx = idx + 1_c_int
   flat(idx) = state%roofexternalemissivity; idx = idx + 1_c_int
   flat(idx) = state%roofinternalemissivity; idx = idx + 1_c_int
   flat(idx) = state%rooftransmissivity; idx = idx + 1_c_int
   flat(idx) = state%roofabsorbtivity; idx = idx + 1_c_int
   flat(idx) = state%roofreflectivity; idx = idx + 1_c_int
   flat(idx) = state%floorthickness; idx = idx + 1_c_int
   flat(idx) = state%groundflooreffectiveconductivity; idx = idx + 1_c_int
   flat(idx) = state%groundfloordensity; idx = idx + 1_c_int
   flat(idx) = state%groundfloorcp; idx = idx + 1_c_int
   flat(idx) = state%windowthickness; idx = idx + 1_c_int
   flat(idx) = state%windoweffectiveconductivity; idx = idx + 1_c_int
   flat(idx) = state%windowdensity; idx = idx + 1_c_int
   flat(idx) = state%windowcp; idx = idx + 1_c_int
   flat(idx) = state%windowexternalemissivity; idx = idx + 1_c_int
   flat(idx) = state%windowinternalemissivity; idx = idx + 1_c_int
   flat(idx) = state%windowtransmissivity; idx = idx + 1_c_int
   flat(idx) = state%windowabsorbtivity; idx = idx + 1_c_int
   flat(idx) = state%windowreflectivity; idx = idx + 1_c_int
   flat(idx) = state%internalmassdensity; idx = idx + 1_c_int
   flat(idx) = state%internalmasscp; idx = idx + 1_c_int
   flat(idx) = state%internalmassemissivity; idx = idx + 1_c_int
   flat(idx) = state%maxheatingpower; idx = idx + 1_c_int
   flat(idx) = state%watertankwatervolume; idx = idx + 1_c_int
   flat(idx) = state%maximumhotwaterheatingpower; idx = idx + 1_c_int
   flat(idx) = state%heatingsetpointtemperature; idx = idx + 1_c_int
   flat(idx) = state%coolingsetpointtemperature; idx = idx + 1_c_int

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         flat(idx) = state%metabolismprofile(i, j)
         idx = idx + 1_c_int
      end do
   end do

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         flat(idx) = state%applianceprofile(i, j)
         idx = idx + 1_c_int
      end do
   end do

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

   state%buildingcount = flat(idx); idx = idx + 1_c_int
   state%occupants = flat(idx); idx = idx + 1_c_int
   state%hhs0 = flat(idx); idx = idx + 1_c_int
   state%age_0_4 = flat(idx); idx = idx + 1_c_int
   state%age_5_11 = flat(idx); idx = idx + 1_c_int
   state%age_12_18 = flat(idx); idx = idx + 1_c_int
   state%age_19_64 = flat(idx); idx = idx + 1_c_int
   state%age_65plus = flat(idx); idx = idx + 1_c_int
   state%stebbs_height = flat(idx); idx = idx + 1_c_int
   state%footprintarea = flat(idx); idx = idx + 1_c_int
   state%wallexternalarea = flat(idx); idx = idx + 1_c_int
   state%ratiointernalvolume = flat(idx); idx = idx + 1_c_int
   state%wwr = flat(idx); idx = idx + 1_c_int
   state%wallthickness = flat(idx); idx = idx + 1_c_int
   state%walleffectiveconductivity = flat(idx); idx = idx + 1_c_int
   state%walldensity = flat(idx); idx = idx + 1_c_int
   state%wallcp = flat(idx); idx = idx + 1_c_int
   state%wallextthickness = flat(idx); idx = idx + 1_c_int
   state%wallexteffectiveconductivity = flat(idx); idx = idx + 1_c_int
   state%wallextdensity = flat(idx); idx = idx + 1_c_int
   state%wallextcp = flat(idx); idx = idx + 1_c_int
   state%walloutercapfrac = flat(idx); idx = idx + 1_c_int
   state%wallexternalemissivity = flat(idx); idx = idx + 1_c_int
   state%wallinternalemissivity = flat(idx); idx = idx + 1_c_int
   state%walltransmissivity = flat(idx); idx = idx + 1_c_int
   state%wallabsorbtivity = flat(idx); idx = idx + 1_c_int
   state%wallreflectivity = flat(idx); idx = idx + 1_c_int
   state%roofthickness = flat(idx); idx = idx + 1_c_int
   state%roofeffectiveconductivity = flat(idx); idx = idx + 1_c_int
   state%roofdensity = flat(idx); idx = idx + 1_c_int
   state%roofcp = flat(idx); idx = idx + 1_c_int
   state%roofextthickness = flat(idx); idx = idx + 1_c_int
   state%roofexteffectiveconductivity = flat(idx); idx = idx + 1_c_int
   state%roofextdensity = flat(idx); idx = idx + 1_c_int
   state%roofextcp = flat(idx); idx = idx + 1_c_int
   state%roofoutercapfrac = flat(idx); idx = idx + 1_c_int
   state%roofexternalemissivity = flat(idx); idx = idx + 1_c_int
   state%roofinternalemissivity = flat(idx); idx = idx + 1_c_int
   state%rooftransmissivity = flat(idx); idx = idx + 1_c_int
   state%roofabsorbtivity = flat(idx); idx = idx + 1_c_int
   state%roofreflectivity = flat(idx); idx = idx + 1_c_int
   state%floorthickness = flat(idx); idx = idx + 1_c_int
   state%groundflooreffectiveconductivity = flat(idx); idx = idx + 1_c_int
   state%groundfloordensity = flat(idx); idx = idx + 1_c_int
   state%groundfloorcp = flat(idx); idx = idx + 1_c_int
   state%windowthickness = flat(idx); idx = idx + 1_c_int
   state%windoweffectiveconductivity = flat(idx); idx = idx + 1_c_int
   state%windowdensity = flat(idx); idx = idx + 1_c_int
   state%windowcp = flat(idx); idx = idx + 1_c_int
   state%windowexternalemissivity = flat(idx); idx = idx + 1_c_int
   state%windowinternalemissivity = flat(idx); idx = idx + 1_c_int
   state%windowtransmissivity = flat(idx); idx = idx + 1_c_int
   state%windowabsorbtivity = flat(idx); idx = idx + 1_c_int
   state%windowreflectivity = flat(idx); idx = idx + 1_c_int
   state%internalmassdensity = flat(idx); idx = idx + 1_c_int
   state%internalmasscp = flat(idx); idx = idx + 1_c_int
   state%internalmassemissivity = flat(idx); idx = idx + 1_c_int
   state%maxheatingpower = flat(idx); idx = idx + 1_c_int
   state%watertankwatervolume = flat(idx); idx = idx + 1_c_int
   state%maximumhotwaterheatingpower = flat(idx); idx = idx + 1_c_int
   state%heatingsetpointtemperature = flat(idx); idx = idx + 1_c_int
   state%coolingsetpointtemperature = flat(idx); idx = idx + 1_c_int

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         state%metabolismprofile(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

   do j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
      do i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
         state%applianceprofile(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

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
