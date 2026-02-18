! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for SPARTACUS_LAYER_PRM.
! -----------------------------------------------------------------------------
module module_c_api_spartacus_layer_prm
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_spartacus, only: SPARTACUS_LAYER_PRM

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_SPARTACUS_LAYER_PRM_SCHEMA_VERSION = 1_c_int

type :: spartacus_layer_prm_shadow
   real(c_double), dimension(:), allocatable :: building_frac
   real(c_double), dimension(:), allocatable :: building_scale
   real(c_double), dimension(:), allocatable :: veg_frac
   real(c_double), dimension(:), allocatable :: veg_scale
   real(c_double), dimension(:), allocatable :: alb_roof
   real(c_double), dimension(:), allocatable :: emis_roof
   real(c_double), dimension(:), allocatable :: alb_wall
   real(c_double), dimension(:), allocatable :: emis_wall
   real(c_double), dimension(:, :), allocatable :: roof_albedo_dir_mult_fact
   real(c_double), dimension(:, :), allocatable :: wall_specular_frac
end type spartacus_layer_prm_shadow

public :: suews_spartacus_layer_prm_len
public :: suews_spartacus_layer_prm_schema_version
public :: suews_spartacus_layer_prm_default
public :: suews_spartacus_layer_prm_error_message
public :: spartacus_layer_prm_unpack

contains

subroutine suews_spartacus_layer_prm_len(n_flat, nlayer, nspec, err) bind(C, name='suews_spartacus_layer_prm_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: nspec
   integer(c_int), intent(out) :: err

   type(spartacus_layer_prm_shadow) :: state

   call spartacus_layer_prm_layout(state, n_flat, nlayer, nspec, err)

end subroutine suews_spartacus_layer_prm_len

subroutine suews_spartacus_layer_prm_schema_version(schema_version, err) bind(C, name='suews_spartacus_layer_prm_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_SPARTACUS_LAYER_PRM_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_spartacus_layer_prm_schema_version

subroutine suews_spartacus_layer_prm_default(flat, n_flat, nlayer, nspec, err) bind(C, name='suews_spartacus_layer_prm_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: nspec
   integer(c_int), intent(out) :: err

   type(spartacus_layer_prm_shadow) :: state

   call spartacus_layer_prm_pack(state, flat, n_flat, nlayer, nspec, err)

end subroutine suews_spartacus_layer_prm_default

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

subroutine update_len_from_mat(field, nspec, nlayer, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   integer(c_int), intent(inout) :: nspec
   integer(c_int), intent(inout) :: nlayer
   integer(c_int), intent(inout) :: err
   integer(c_int) :: spec_here
   integer(c_int) :: layer_here

   if (err/=SUEWS_CAPI_OK) return
   if (.not. allocated(field)) return

   spec_here = int(size(field, 1), c_int)
   layer_here = int(size(field, 2), c_int)

   if (nspec==0_c_int) then
      nspec = spec_here
   elseif (nspec/=spec_here) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if

   if (nlayer==0_c_int) then
      nlayer = layer_here
   elseif (nlayer/=layer_here) then
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

subroutine require_mat_layout(field, nspec, nlayer, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   integer(c_int), intent(in) :: nspec
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nspec==0_c_int .or. nlayer==0_c_int) then
      if (allocated(field)) err = SUEWS_CAPI_BAD_STATE
   else
      if (.not. allocated(field)) then
         err = SUEWS_CAPI_BAD_STATE
      elseif (int(size(field, 1), c_int)/=nspec .or. &
              int(size(field, 2), c_int)/=nlayer) then
         err = SUEWS_CAPI_BAD_STATE
      end if
   end if

end subroutine require_mat_layout

subroutine spartacus_layer_prm_layout(state, n_flat, nlayer, nspec, err)
   implicit none

   type(spartacus_layer_prm_shadow), intent(in) :: state
   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: nspec
   integer(c_int), intent(out) :: err

   nlayer = 0_c_int
   nspec = 0_c_int
   err = SUEWS_CAPI_OK

   call update_len_from_vec(state%building_frac, nlayer, err)
   call update_len_from_vec(state%building_scale, nlayer, err)
   call update_len_from_vec(state%veg_frac, nlayer, err)
   call update_len_from_vec(state%veg_scale, nlayer, err)
   call update_len_from_vec(state%alb_roof, nlayer, err)
   call update_len_from_vec(state%emis_roof, nlayer, err)
   call update_len_from_vec(state%alb_wall, nlayer, err)
   call update_len_from_vec(state%emis_wall, nlayer, err)

   call update_len_from_mat(state%roof_albedo_dir_mult_fact, nspec, nlayer, err)
   call update_len_from_mat(state%wall_specular_frac, nspec, nlayer, err)

   call require_vec_layout(state%building_frac, nlayer, err)
   call require_vec_layout(state%building_scale, nlayer, err)
   call require_vec_layout(state%veg_frac, nlayer, err)
   call require_vec_layout(state%veg_scale, nlayer, err)
   call require_vec_layout(state%alb_roof, nlayer, err)
   call require_vec_layout(state%emis_roof, nlayer, err)
   call require_vec_layout(state%alb_wall, nlayer, err)
   call require_vec_layout(state%emis_wall, nlayer, err)

   call require_mat_layout(state%roof_albedo_dir_mult_fact, nspec, nlayer, err)
   call require_mat_layout(state%wall_specular_frac, nspec, nlayer, err)

   if (err/=SUEWS_CAPI_OK) then
      n_flat = 0_c_int
      return
   end if

   if (nlayer==0_c_int) nspec = 0_c_int

   n_flat = 8_c_int * nlayer + 2_c_int * nspec * nlayer
   err = SUEWS_CAPI_OK

end subroutine spartacus_layer_prm_layout

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

subroutine pack_mat(field, flat, idx, nspec, nlayer)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(inout) :: idx
   integer(c_int), intent(in) :: nspec
   integer(c_int), intent(in) :: nlayer
   integer(c_int) :: i
   integer(c_int) :: j

   if (nspec<=0_c_int .or. nlayer<=0_c_int) return
   do i = 1_c_int, nspec
      do j = 1_c_int, nlayer
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

subroutine unpack_mat(flat, idx, field, nspec, nlayer)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(inout) :: idx
   real(c_double), dimension(:, :), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: nspec
   integer(c_int), intent(in) :: nlayer
   integer(c_int) :: i
   integer(c_int) :: j

   if (nspec<=0_c_int .or. nlayer<=0_c_int) return
   do i = 1_c_int, nspec
      do j = 1_c_int, nlayer
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

subroutine ensure_mat_alloc(field, nspec, nlayer, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: nspec
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nspec<=0_c_int .or. nlayer<=0_c_int) then
      if (allocated(field)) deallocate (field)
      return
   end if

   if (allocated(field)) then
      if (int(size(field, 1), c_int)/=nspec .or. &
          int(size(field, 2), c_int)/=nlayer) then
         deallocate (field)
         allocate (field(int(nspec), int(nlayer)))
      end if
   else
      allocate (field(int(nspec), int(nlayer)))
   end if

end subroutine ensure_mat_alloc

subroutine spartacus_layer_prm_pack(state, flat, n_flat, nlayer, nspec, err)
   implicit none

   type(spartacus_layer_prm_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: nspec
   integer(c_int), intent(out) :: err
   integer(c_int) :: n_expected
   integer(c_int) :: idx

   call spartacus_layer_prm_layout(state, n_expected, nlayer, nspec, err)
   if (err/=SUEWS_CAPI_OK) return

   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   call pack_vec(state%building_frac, flat, idx, nlayer)
   call pack_vec(state%building_scale, flat, idx, nlayer)
   call pack_vec(state%veg_frac, flat, idx, nlayer)
   call pack_vec(state%veg_scale, flat, idx, nlayer)
   call pack_vec(state%alb_roof, flat, idx, nlayer)
   call pack_vec(state%emis_roof, flat, idx, nlayer)
   call pack_vec(state%alb_wall, flat, idx, nlayer)
   call pack_vec(state%emis_wall, flat, idx, nlayer)

   call pack_mat(state%roof_albedo_dir_mult_fact, flat, idx, nspec, nlayer)
   call pack_mat(state%wall_specular_frac, flat, idx, nspec, nlayer)

   err = SUEWS_CAPI_OK

end subroutine spartacus_layer_prm_pack

subroutine spartacus_layer_prm_unpack(flat, n_flat, nlayer, nspec, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: nspec
   type(SPARTACUS_LAYER_PRM), intent(inout) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: n_expected
   integer(c_int) :: idx

   if (nlayer<0_c_int .or. nspec<0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   n_expected = 8_c_int * nlayer + 2_c_int * nspec * nlayer
   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   err = SUEWS_CAPI_OK
   call ensure_vec_alloc(state%building_frac, nlayer, err)
   call ensure_vec_alloc(state%building_scale, nlayer, err)
   call ensure_vec_alloc(state%veg_frac, nlayer, err)
   call ensure_vec_alloc(state%veg_scale, nlayer, err)
   call ensure_vec_alloc(state%alb_roof, nlayer, err)
   call ensure_vec_alloc(state%emis_roof, nlayer, err)
   call ensure_vec_alloc(state%alb_wall, nlayer, err)
   call ensure_vec_alloc(state%emis_wall, nlayer, err)

   call ensure_mat_alloc(state%roof_albedo_dir_mult_fact, nspec, nlayer, err)
   call ensure_mat_alloc(state%wall_specular_frac, nspec, nlayer, err)
   if (err/=SUEWS_CAPI_OK) return

   idx = 1_c_int
   call unpack_vec(flat, idx, state%building_frac, nlayer)
   call unpack_vec(flat, idx, state%building_scale, nlayer)
   call unpack_vec(flat, idx, state%veg_frac, nlayer)
   call unpack_vec(flat, idx, state%veg_scale, nlayer)
   call unpack_vec(flat, idx, state%alb_roof, nlayer)
   call unpack_vec(flat, idx, state%emis_roof, nlayer)
   call unpack_vec(flat, idx, state%alb_wall, nlayer)
   call unpack_vec(flat, idx, state%emis_wall, nlayer)

   call unpack_mat(flat, idx, state%roof_albedo_dir_mult_fact, nspec, nlayer)
   call unpack_mat(flat, idx, state%wall_specular_frac, nspec, nlayer)

   err = SUEWS_CAPI_OK

end subroutine spartacus_layer_prm_unpack

subroutine suews_spartacus_layer_prm_error_message(code, buffer, buffer_len) bind(C, name='suews_spartacus_layer_prm_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_spartacus_layer_prm_error_message

end module module_c_api_spartacus_layer_prm

module c_api_spartacus_layer_prm_module
use module_c_api_spartacus_layer_prm
end module c_api_spartacus_layer_prm_module
