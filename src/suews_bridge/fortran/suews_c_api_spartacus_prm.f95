! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SPARTACUS_PRM.
! -----------------------------------------------------------------------------
module module_c_api_spartacus_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_spartacus, only: SPARTACUS_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_SPARTACUS_PRM_BASE_LEN = 14_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_SPARTACUS_PRM_SCHEMA_VERSION = 1_c_int

type :: spartacus_prm_shadow
   real(c_double) :: air_ext_lw = 0.0_c_double
   real(c_double) :: air_ext_sw = 0.0_c_double
   real(c_double) :: air_ssa_lw = 0.0_c_double
   real(c_double) :: air_ssa_sw = 0.0_c_double
   real(c_double), dimension(:), allocatable :: height
   real(c_double) :: ground_albedo_dir_mult_fact = 0.0_c_double
   integer(c_int) :: n_stream_lw_urban = 0_c_int
   integer(c_int) :: n_stream_sw_urban = 0_c_int
   integer(c_int) :: n_vegetation_region_urban = 0_c_int
   real(c_double) :: sw_dn_direct_frac = 0.0_c_double
   real(c_double) :: use_sw_direct_albedo = 0.0_c_double
   real(c_double) :: veg_contact_fraction_const = 0.0_c_double
   real(c_double) :: veg_fsd_const = 0.0_c_double
   real(c_double) :: veg_ssa_lw = 0.0_c_double
   real(c_double) :: veg_ssa_sw = 0.0_c_double
end type spartacus_prm_shadow

public :: suews_spartacus_prm_len
public :: suews_spartacus_prm_schema_version
public :: suews_spartacus_prm_default
public :: suews_spartacus_prm_error_message
public :: spartacus_prm_unpack

contains

subroutine suews_spartacus_prm_len(n_flat, height_len, nlayer, err) bind(C, name='suews_spartacus_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: height_len
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: err

   type(spartacus_prm_shadow) :: state

   call spartacus_prm_layout(state, n_flat, height_len, nlayer, err)

end subroutine suews_spartacus_prm_len

subroutine suews_spartacus_prm_schema_version(schema_version, err) bind(C, name='suews_spartacus_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_SPARTACUS_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_spartacus_prm_schema_version

subroutine suews_spartacus_prm_default(flat, n_flat, height_len, nlayer, err) bind(C, name='suews_spartacus_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: height_len
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: err

   type(spartacus_prm_shadow) :: state

   call spartacus_prm_pack(state, flat, n_flat, height_len, nlayer, err)

end subroutine suews_spartacus_prm_default

subroutine spartacus_prm_layout(state, n_flat, height_len, nlayer, err)
   implicit none

   type(spartacus_prm_shadow), intent(in) :: state
   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: height_len
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: err

   height_len = 0_c_int
   if (allocated(state%height)) then
      height_len = int(size(state%height), c_int)
   end if

   if (height_len>0_c_int) then
      nlayer = height_len - 1_c_int
   else
      nlayer = 0_c_int
   end if

   n_flat = SUEWS_CAPI_SPARTACUS_PRM_BASE_LEN + height_len
   err = SUEWS_CAPI_OK

end subroutine spartacus_prm_layout

subroutine spartacus_prm_pack(state, flat, n_flat, height_len, nlayer, err)
   implicit none

   type(spartacus_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: height_len
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: err

   integer(c_int) :: n_expected
   integer(c_int) :: idx
   integer(c_int) :: i

   call spartacus_prm_layout(state, n_expected, height_len, nlayer, err)
   if (err/=SUEWS_CAPI_OK) return

   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   flat(idx) = state%air_ext_lw; idx = idx + 1_c_int
   flat(idx) = state%air_ext_sw; idx = idx + 1_c_int
   flat(idx) = state%air_ssa_lw; idx = idx + 1_c_int
   flat(idx) = state%air_ssa_sw; idx = idx + 1_c_int

   if (height_len>0_c_int) then
      do i = 1_c_int, height_len
         flat(idx) = state%height(i)
         idx = idx + 1_c_int
      end do
   end if

   flat(idx) = state%ground_albedo_dir_mult_fact; idx = idx + 1_c_int
   flat(idx) = real(state%n_stream_lw_urban, c_double); idx = idx + 1_c_int
   flat(idx) = real(state%n_stream_sw_urban, c_double); idx = idx + 1_c_int
   flat(idx) = real(state%n_vegetation_region_urban, c_double); idx = idx + 1_c_int
   flat(idx) = state%sw_dn_direct_frac; idx = idx + 1_c_int
   flat(idx) = state%use_sw_direct_albedo; idx = idx + 1_c_int
   flat(idx) = state%veg_contact_fraction_const; idx = idx + 1_c_int
   flat(idx) = state%veg_fsd_const; idx = idx + 1_c_int
   flat(idx) = state%veg_ssa_lw; idx = idx + 1_c_int
   flat(idx) = state%veg_ssa_sw

   err = SUEWS_CAPI_OK

end subroutine spartacus_prm_pack

subroutine spartacus_prm_unpack(flat, n_flat, nlayer, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(in) :: nlayer
   type(SPARTACUS_PRM), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: height_len
   integer(c_int) :: n_expected
   integer(c_int) :: idx
   integer(c_int) :: i

   if (nlayer<0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   height_len = nlayer + 1_c_int
   n_expected = SUEWS_CAPI_SPARTACUS_PRM_BASE_LEN + height_len
   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (allocated(state%height)) deallocate (state%height)
   allocate (state%height(int(height_len)))

   idx = 1_c_int
   state%air_ext_lw = flat(idx); idx = idx + 1_c_int
   state%air_ext_sw = flat(idx); idx = idx + 1_c_int
   state%air_ssa_lw = flat(idx); idx = idx + 1_c_int
   state%air_ssa_sw = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, height_len
      state%height(int(i)) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%ground_albedo_dir_mult_fact = flat(idx); idx = idx + 1_c_int
   state%n_stream_lw_urban = int(nint(flat(idx))); idx = idx + 1_c_int
   state%n_stream_sw_urban = int(nint(flat(idx))); idx = idx + 1_c_int
   state%n_vegetation_region_urban = int(nint(flat(idx))); idx = idx + 1_c_int
   state%sw_dn_direct_frac = flat(idx); idx = idx + 1_c_int
   state%use_sw_direct_albedo = flat(idx); idx = idx + 1_c_int
   state%veg_contact_fraction_const = flat(idx); idx = idx + 1_c_int
   state%veg_fsd_const = flat(idx); idx = idx + 1_c_int
   state%veg_ssa_lw = flat(idx); idx = idx + 1_c_int
   state%veg_ssa_sw = flat(idx)

   err = SUEWS_CAPI_OK

end subroutine spartacus_prm_unpack

subroutine suews_spartacus_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_spartacus_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_spartacus_prm_error_message

end module module_c_api_spartacus_prm

module c_api_spartacus_prm_module
use module_c_api_spartacus_prm
end module c_api_spartacus_prm_module
