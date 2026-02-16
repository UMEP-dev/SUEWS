! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for EHC_PRM.
! -----------------------------------------------------------------------------
module module_c_api_ehc_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_ehc, only: EHC_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_EHC_PRM_SCHEMA_VERSION = 1_c_int

type :: ehc_prm_shadow
   real(c_double), dimension(:), allocatable :: soil_storecap_roof
   real(c_double), dimension(:), allocatable :: soil_storecap_wall
   real(c_double), dimension(:), allocatable :: state_limit_roof
   real(c_double), dimension(:), allocatable :: state_limit_wall
   real(c_double), dimension(:), allocatable :: wet_thresh_roof
   real(c_double), dimension(:), allocatable :: wet_thresh_wall
   real(c_double), dimension(:), allocatable :: tin_roof
   real(c_double), dimension(:), allocatable :: tin_wall
   real(c_double), dimension(:), allocatable :: tin_surf
   real(c_double), dimension(:, :), allocatable :: k_roof
   real(c_double), dimension(:, :), allocatable :: k_wall
   real(c_double), dimension(:, :), allocatable :: k_surf
   real(c_double), dimension(:, :), allocatable :: cp_roof
   real(c_double), dimension(:, :), allocatable :: cp_wall
   real(c_double), dimension(:, :), allocatable :: cp_surf
   real(c_double), dimension(:, :), allocatable :: dz_roof
   real(c_double), dimension(:, :), allocatable :: dz_wall
   real(c_double), dimension(:, :), allocatable :: dz_surf
end type ehc_prm_shadow

public :: suews_ehc_prm_len
public :: suews_ehc_prm_schema_version
public :: suews_ehc_prm_default
public :: suews_ehc_prm_error_message
public :: ehc_prm_unpack

contains

subroutine suews_ehc_prm_len(n_flat, nlayer, ndepth, err) bind(C, name='suews_ehc_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: ndepth
   integer(c_int), intent(out) :: err

   type(ehc_prm_shadow) :: state

   call ehc_prm_layout(state, n_flat, nlayer, ndepth, err)

end subroutine suews_ehc_prm_len

subroutine suews_ehc_prm_schema_version(schema_version, err) bind(C, name='suews_ehc_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_EHC_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_ehc_prm_schema_version

subroutine suews_ehc_prm_default(flat, n_flat, nlayer, ndepth, err) bind(C, name='suews_ehc_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: ndepth
   integer(c_int), intent(out) :: err

   type(ehc_prm_shadow) :: state

   call ehc_prm_pack(state, flat, n_flat, nlayer, ndepth, err)

end subroutine suews_ehc_prm_default

subroutine update_len_from_vec(field, nlayer, err)
   implicit none

   real(c_double), dimension(:), allocatable, intent(in) :: field
   integer(c_int), intent(inout) :: nlayer
   integer(c_int), intent(inout) :: err
   integer(c_int) :: n_here

   if (err/=SUEWS_CAPI_OK) return
   if (.not. allocated(field)) return

   n_here = int(size(field), c_int)
   if (nlayer==0_c_int) then
      nlayer = n_here
   elseif (nlayer/=n_here) then
      err = SUEWS_CAPI_BAD_STATE
   end if

end subroutine update_len_from_vec

subroutine update_len_from_mat(field, nlayer, ndepth, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   integer(c_int), intent(inout) :: nlayer
   integer(c_int), intent(inout) :: ndepth
   integer(c_int), intent(inout) :: err
   integer(c_int) :: layer_here
   integer(c_int) :: depth_here

   if (err/=SUEWS_CAPI_OK) return
   if (.not. allocated(field)) return

   layer_here = int(size(field, 1), c_int)
   depth_here = int(size(field, 2), c_int)

   if (nlayer==0_c_int) then
      nlayer = layer_here
   elseif (nlayer/=layer_here) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if

   if (ndepth==0_c_int) then
      ndepth = depth_here
   elseif (ndepth/=depth_here) then
      err = SUEWS_CAPI_BAD_STATE
   end if

end subroutine update_len_from_mat

subroutine require_vec_layout(field, nlayer, err)
   implicit none

   real(c_double), dimension(:), allocatable, intent(in) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nlayer==0_c_int) then
      if (allocated(field)) err = SUEWS_CAPI_BAD_STATE
   else
      if (.not. allocated(field)) then
         err = SUEWS_CAPI_BAD_STATE
      elseif (int(size(field), c_int)/=nlayer) then
         err = SUEWS_CAPI_BAD_STATE
      end if
   end if

end subroutine require_vec_layout

subroutine require_mat_layout(field, nlayer, ndepth, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nlayer==0_c_int .or. ndepth==0_c_int) then
      if (allocated(field)) err = SUEWS_CAPI_BAD_STATE
   else
      if (.not. allocated(field)) then
         err = SUEWS_CAPI_BAD_STATE
      elseif (int(size(field, 1), c_int)/=nlayer .or. &
              int(size(field, 2), c_int)/=ndepth) then
         err = SUEWS_CAPI_BAD_STATE
      end if
   end if

end subroutine require_mat_layout

subroutine ehc_prm_layout(state, n_flat, nlayer, ndepth, err)
   implicit none

   type(ehc_prm_shadow), intent(in) :: state
   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: ndepth
   integer(c_int), intent(out) :: err

   nlayer = 0_c_int
   ndepth = 0_c_int
   err = SUEWS_CAPI_OK

   call update_len_from_vec(state%soil_storecap_roof, nlayer, err)
   call update_len_from_vec(state%soil_storecap_wall, nlayer, err)
   call update_len_from_vec(state%state_limit_roof, nlayer, err)
   call update_len_from_vec(state%state_limit_wall, nlayer, err)
   call update_len_from_vec(state%wet_thresh_roof, nlayer, err)
   call update_len_from_vec(state%wet_thresh_wall, nlayer, err)
   call update_len_from_vec(state%tin_roof, nlayer, err)
   call update_len_from_vec(state%tin_wall, nlayer, err)
   call update_len_from_vec(state%tin_surf, nlayer, err)

   call update_len_from_mat(state%k_roof, nlayer, ndepth, err)
   call update_len_from_mat(state%k_wall, nlayer, ndepth, err)
   call update_len_from_mat(state%k_surf, nlayer, ndepth, err)
   call update_len_from_mat(state%cp_roof, nlayer, ndepth, err)
   call update_len_from_mat(state%cp_wall, nlayer, ndepth, err)
   call update_len_from_mat(state%cp_surf, nlayer, ndepth, err)
   call update_len_from_mat(state%dz_roof, nlayer, ndepth, err)
   call update_len_from_mat(state%dz_wall, nlayer, ndepth, err)
   call update_len_from_mat(state%dz_surf, nlayer, ndepth, err)

   call require_vec_layout(state%soil_storecap_roof, nlayer, err)
   call require_vec_layout(state%soil_storecap_wall, nlayer, err)
   call require_vec_layout(state%state_limit_roof, nlayer, err)
   call require_vec_layout(state%state_limit_wall, nlayer, err)
   call require_vec_layout(state%wet_thresh_roof, nlayer, err)
   call require_vec_layout(state%wet_thresh_wall, nlayer, err)
   call require_vec_layout(state%tin_roof, nlayer, err)
   call require_vec_layout(state%tin_wall, nlayer, err)
   call require_vec_layout(state%tin_surf, nlayer, err)

   call require_mat_layout(state%k_roof, nlayer, ndepth, err)
   call require_mat_layout(state%k_wall, nlayer, ndepth, err)
   call require_mat_layout(state%k_surf, nlayer, ndepth, err)
   call require_mat_layout(state%cp_roof, nlayer, ndepth, err)
   call require_mat_layout(state%cp_wall, nlayer, ndepth, err)
   call require_mat_layout(state%cp_surf, nlayer, ndepth, err)
   call require_mat_layout(state%dz_roof, nlayer, ndepth, err)
   call require_mat_layout(state%dz_wall, nlayer, ndepth, err)
   call require_mat_layout(state%dz_surf, nlayer, ndepth, err)

   if (err/=SUEWS_CAPI_OK) then
      n_flat = 0_c_int
      return
   end if

   if (nlayer==0_c_int) ndepth = 0_c_int

   n_flat = 9_c_int * nlayer + 9_c_int * nlayer * ndepth
   err = SUEWS_CAPI_OK

end subroutine ehc_prm_layout

subroutine pack_vec(field, flat, idx, nlayer)
   implicit none

   real(c_double), dimension(:), allocatable, intent(in) :: field
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(inout) :: idx
   integer(c_int), intent(in) :: nlayer
   integer(c_int) :: i

   if (nlayer<=0_c_int) return
   do i = 1_c_int, nlayer
      flat(idx) = field(i)
      idx = idx + 1_c_int
   end do

end subroutine pack_vec

subroutine pack_mat(field, flat, idx, nlayer, ndepth)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(inout) :: idx
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   integer(c_int) :: i
   integer(c_int) :: j

   if (nlayer<=0_c_int .or. ndepth<=0_c_int) return
   do i = 1_c_int, nlayer
      do j = 1_c_int, ndepth
         flat(idx) = field(i, j)
         idx = idx + 1_c_int
      end do
   end do

end subroutine pack_mat

subroutine unpack_vec(flat, idx, field, nlayer)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(inout) :: idx
   real(c_double), dimension(:), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int) :: i

   if (nlayer<=0_c_int) return
   do i = 1_c_int, nlayer
      field(i) = flat(idx)
      idx = idx + 1_c_int
   end do

end subroutine unpack_vec

subroutine unpack_mat(flat, idx, field, nlayer, ndepth)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(inout) :: idx
   real(c_double), dimension(:, :), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   integer(c_int) :: i
   integer(c_int) :: j

   if (nlayer<=0_c_int .or. ndepth<=0_c_int) return
   do i = 1_c_int, nlayer
      do j = 1_c_int, ndepth
         field(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

end subroutine unpack_mat

subroutine ensure_vec_alloc(field, nlayer, err)
   implicit none

   real(c_double), dimension(:), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nlayer<=0_c_int) then
      if (allocated(field)) deallocate (field)
      return
   end if

   if (allocated(field)) then
      if (int(size(field), c_int)/=nlayer) then
         deallocate (field)
         allocate (field(int(nlayer)))
      end if
   else
      allocate (field(int(nlayer)))
   end if

end subroutine ensure_vec_alloc

subroutine ensure_mat_alloc(field, nlayer, ndepth, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nlayer<=0_c_int .or. ndepth<=0_c_int) then
      if (allocated(field)) deallocate (field)
      return
   end if

   if (allocated(field)) then
      if (int(size(field, 1), c_int)/=nlayer .or. &
          int(size(field, 2), c_int)/=ndepth) then
         deallocate (field)
         allocate (field(int(nlayer), int(ndepth)))
      end if
   else
      allocate (field(int(nlayer), int(ndepth)))
   end if

end subroutine ensure_mat_alloc

subroutine ehc_prm_pack(state, flat, n_flat, nlayer, ndepth, err)
   implicit none

   type(ehc_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: ndepth
   integer(c_int), intent(out) :: err
   integer(c_int) :: n_expected
   integer(c_int) :: idx

   call ehc_prm_layout(state, n_expected, nlayer, ndepth, err)
   if (err/=SUEWS_CAPI_OK) return

   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   call pack_vec(state%soil_storecap_roof, flat, idx, nlayer)
   call pack_vec(state%soil_storecap_wall, flat, idx, nlayer)
   call pack_vec(state%state_limit_roof, flat, idx, nlayer)
   call pack_vec(state%state_limit_wall, flat, idx, nlayer)
   call pack_vec(state%wet_thresh_roof, flat, idx, nlayer)
   call pack_vec(state%wet_thresh_wall, flat, idx, nlayer)
   call pack_vec(state%tin_roof, flat, idx, nlayer)
   call pack_vec(state%tin_wall, flat, idx, nlayer)
   call pack_vec(state%tin_surf, flat, idx, nlayer)

   call pack_mat(state%k_roof, flat, idx, nlayer, ndepth)
   call pack_mat(state%k_wall, flat, idx, nlayer, ndepth)
   call pack_mat(state%k_surf, flat, idx, nlayer, ndepth)
   call pack_mat(state%cp_roof, flat, idx, nlayer, ndepth)
   call pack_mat(state%cp_wall, flat, idx, nlayer, ndepth)
   call pack_mat(state%cp_surf, flat, idx, nlayer, ndepth)
   call pack_mat(state%dz_roof, flat, idx, nlayer, ndepth)
   call pack_mat(state%dz_wall, flat, idx, nlayer, ndepth)
   call pack_mat(state%dz_surf, flat, idx, nlayer, ndepth)

   err = SUEWS_CAPI_OK

end subroutine ehc_prm_pack

subroutine ehc_prm_unpack(flat, n_flat, nlayer, ndepth, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   type(EHC_PRM), intent(inout) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: n_expected
   integer(c_int) :: idx

   if (nlayer<0_c_int .or. ndepth<0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   n_expected = 9_c_int * nlayer + 9_c_int * nlayer * ndepth
   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   err = SUEWS_CAPI_OK
   call ensure_vec_alloc(state%soil_storecap_roof, nlayer, err)
   call ensure_vec_alloc(state%soil_storecap_wall, nlayer, err)
   call ensure_vec_alloc(state%state_limit_roof, nlayer, err)
   call ensure_vec_alloc(state%state_limit_wall, nlayer, err)
   call ensure_vec_alloc(state%wet_thresh_roof, nlayer, err)
   call ensure_vec_alloc(state%wet_thresh_wall, nlayer, err)
   call ensure_vec_alloc(state%tin_roof, nlayer, err)
   call ensure_vec_alloc(state%tin_wall, nlayer, err)
   call ensure_vec_alloc(state%tin_surf, nlayer, err)

   call ensure_mat_alloc(state%k_roof, nlayer, ndepth, err)
   call ensure_mat_alloc(state%k_wall, nlayer, ndepth, err)
   call ensure_mat_alloc(state%k_surf, nlayer, ndepth, err)
   call ensure_mat_alloc(state%cp_roof, nlayer, ndepth, err)
   call ensure_mat_alloc(state%cp_wall, nlayer, ndepth, err)
   call ensure_mat_alloc(state%cp_surf, nlayer, ndepth, err)
   call ensure_mat_alloc(state%dz_roof, nlayer, ndepth, err)
   call ensure_mat_alloc(state%dz_wall, nlayer, ndepth, err)
   call ensure_mat_alloc(state%dz_surf, nlayer, ndepth, err)
   if (err/=SUEWS_CAPI_OK) return

   idx = 1_c_int
   call unpack_vec(flat, idx, state%soil_storecap_roof, nlayer)
   call unpack_vec(flat, idx, state%soil_storecap_wall, nlayer)
   call unpack_vec(flat, idx, state%state_limit_roof, nlayer)
   call unpack_vec(flat, idx, state%state_limit_wall, nlayer)
   call unpack_vec(flat, idx, state%wet_thresh_roof, nlayer)
   call unpack_vec(flat, idx, state%wet_thresh_wall, nlayer)
   call unpack_vec(flat, idx, state%tin_roof, nlayer)
   call unpack_vec(flat, idx, state%tin_wall, nlayer)
   call unpack_vec(flat, idx, state%tin_surf, nlayer)

   call unpack_mat(flat, idx, state%k_roof, nlayer, ndepth)
   call unpack_mat(flat, idx, state%k_wall, nlayer, ndepth)
   call unpack_mat(flat, idx, state%k_surf, nlayer, ndepth)
   call unpack_mat(flat, idx, state%cp_roof, nlayer, ndepth)
   call unpack_mat(flat, idx, state%cp_wall, nlayer, ndepth)
   call unpack_mat(flat, idx, state%cp_surf, nlayer, ndepth)
   call unpack_mat(flat, idx, state%dz_roof, nlayer, ndepth)
   call unpack_mat(flat, idx, state%dz_wall, nlayer, ndepth)
   call unpack_mat(flat, idx, state%dz_surf, nlayer, ndepth)

   err = SUEWS_CAPI_OK

end subroutine ehc_prm_unpack

subroutine suews_ehc_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_ehc_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_ehc_prm_error_message

end module module_c_api_ehc_prm

module c_api_ehc_prm_module
use module_c_api_ehc_prm
end module c_api_ehc_prm_module
