! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for HEAT_STATE.
! -----------------------------------------------------------------------------
module module_c_api_heat_state
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_ctrl_const_allocate, only: nsurf
use module_type_heat, only: HEAT_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_HEAT_STATE_BASE_LEN = 7_c_int * int(nsurf, c_int) + 79_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_HEAT_STATE_SCHEMA_VERSION = 1_c_int

type :: heat_state_shadow
   real(c_double), dimension(:, :), allocatable :: temp_roof
   real(c_double), dimension(:, :), allocatable :: temp_wall
   real(c_double), dimension(:, :), allocatable :: temp_surf
   real(c_double), dimension(:, :), allocatable :: temp_surf_dyohm
   real(c_double), dimension(:), allocatable :: tsfc_roof
   real(c_double), dimension(:), allocatable :: tsfc_wall
   real(c_double), dimension(:), allocatable :: tsfc_surf
   real(c_double), dimension(:), allocatable :: tsfc_surf_dyohm
   real(c_double), dimension(:), allocatable :: tsfc_roof_stepstart
   real(c_double), dimension(:), allocatable :: tsfc_wall_stepstart
   real(c_double), dimension(:), allocatable :: tsfc_surf_stepstart

   real(c_double), dimension(:), allocatable :: qs_roof
   real(c_double), dimension(:), allocatable :: qn_roof
   real(c_double), dimension(:), allocatable :: qe_roof
   real(c_double), dimension(:), allocatable :: qh_roof
   real(c_double), dimension(:), allocatable :: qh_resist_roof

   real(c_double), dimension(:), allocatable :: qs_wall
   real(c_double), dimension(:), allocatable :: qn_wall
   real(c_double), dimension(:), allocatable :: qe_wall
   real(c_double), dimension(:), allocatable :: qh_wall
   real(c_double), dimension(:), allocatable :: qh_resist_wall

   real(c_double), dimension(nsurf) :: qs_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: qn_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: qe0_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: qe_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: qh_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: qh_resist_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: tsurf_ind = 0.0_c_double

   real(c_double) :: qh_lumps = 0.0_c_double
   real(c_double) :: qe_lumps = 0.0_c_double
   real(c_double) :: kclear = 0.0_c_double
   real(c_double) :: kup = 0.0_c_double
   real(c_double) :: ldown = 0.0_c_double
   real(c_double) :: lup = 0.0_c_double
   real(c_double) :: qe = 0.0_c_double
   real(c_double) :: qf = 0.0_c_double
   real(c_double) :: qf_sahp = 0.0_c_double
   real(c_double) :: qh = 0.0_c_double
   real(c_double) :: qh_residual = 0.0_c_double
   real(c_double) :: qh_resist = 0.0_c_double
   real(c_double) :: qn = 0.0_c_double
   real(c_double) :: qn_snowfree = 0.0_c_double
   real(c_double) :: qs = 0.0_c_double
   real(c_double) :: tsfc_c = 0.0_c_double
   real(c_double) :: tsurf = 0.0_c_double
   real(c_double) :: qh_init = 0.0_c_double

   real(c_double), dimension(15) :: roof_in_sw_spc = 0.0_c_double
   real(c_double), dimension(15) :: roof_in_lw_spc = 0.0_c_double
   real(c_double), dimension(15) :: wall_in_sw_spc = 0.0_c_double
   real(c_double), dimension(15) :: wall_in_lw_spc = 0.0_c_double

   logical :: iter_safe = .true.
end type heat_state_shadow

public :: suews_heat_state_len
public :: suews_heat_state_schema_version
public :: suews_heat_state_default
public :: suews_heat_state_error_message
public :: heat_state_unpack

contains

subroutine suews_heat_state_len(n_flat, nlayer, ndepth, err) bind(C, name='suews_heat_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: ndepth
   integer(c_int), intent(out) :: err

   type(heat_state_shadow) :: state

   call heat_state_layout(state, n_flat, nlayer, ndepth, err)

end subroutine suews_heat_state_len

subroutine suews_heat_state_schema_version(schema_version, err) bind(C, name='suews_heat_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_HEAT_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_heat_state_schema_version

subroutine suews_heat_state_default(flat, n_flat, nlayer, ndepth, err) bind(C, name='suews_heat_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: ndepth
   integer(c_int), intent(out) :: err

   type(heat_state_shadow) :: state

   call heat_state_pack(state, flat, n_flat, nlayer, ndepth, err)

end subroutine suews_heat_state_default

subroutine update_nlayer_from_vec(field, nlayer, err)
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

end subroutine update_nlayer_from_vec

subroutine update_layer_depth_from_mat(field, nlayer, ndepth, err)
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

end subroutine update_layer_depth_from_mat

subroutine update_depth_from_surf_mat(field, ndepth, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   integer(c_int), intent(inout) :: ndepth
   integer(c_int), intent(inout) :: err
   integer(c_int) :: surf_here
   integer(c_int) :: depth_here

   if (err/=SUEWS_CAPI_OK) return
   if (.not. allocated(field)) return

   surf_here = int(size(field, 1), c_int)
   depth_here = int(size(field, 2), c_int)
   if (surf_here/=int(nsurf, c_int)) then
      err = SUEWS_CAPI_BAD_STATE
      return
   end if

   if (ndepth==0_c_int) then
      ndepth = depth_here
   elseif (ndepth/=depth_here) then
      err = SUEWS_CAPI_BAD_STATE
   end if

end subroutine update_depth_from_surf_mat

subroutine require_layer_vec(field, nlayer, ndepth, err)
   implicit none

   real(c_double), dimension(:), allocatable, intent(in) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nlayer==0_c_int .and. ndepth==0_c_int) then
      if (allocated(field)) err = SUEWS_CAPI_BAD_STATE
   else
      if (.not. allocated(field)) then
         err = SUEWS_CAPI_BAD_STATE
      elseif (int(size(field), c_int)/=nlayer) then
         err = SUEWS_CAPI_BAD_STATE
      end if
   end if

end subroutine require_layer_vec

subroutine require_surf_vec(field, nlayer, ndepth, err)
   implicit none

   real(c_double), dimension(:), allocatable, intent(in) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nlayer==0_c_int .and. ndepth==0_c_int) then
      if (allocated(field)) err = SUEWS_CAPI_BAD_STATE
   else
      if (.not. allocated(field)) then
         err = SUEWS_CAPI_BAD_STATE
      elseif (int(size(field), c_int)/=int(nsurf, c_int)) then
         err = SUEWS_CAPI_BAD_STATE
      end if
   end if

end subroutine require_surf_vec

subroutine require_layer_depth_mat(field, nlayer, ndepth, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nlayer==0_c_int .and. ndepth==0_c_int) then
      if (allocated(field)) err = SUEWS_CAPI_BAD_STATE
   else
      if (.not. allocated(field)) then
         err = SUEWS_CAPI_BAD_STATE
      elseif (int(size(field, 1), c_int)/=nlayer .or. &
              int(size(field, 2), c_int)/=ndepth) then
         err = SUEWS_CAPI_BAD_STATE
      end if
   end if

end subroutine require_layer_depth_mat

subroutine require_surf_depth_mat(field, nlayer, ndepth, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (nlayer==0_c_int .and. ndepth==0_c_int) then
      if (allocated(field)) err = SUEWS_CAPI_BAD_STATE
   else
      if (.not. allocated(field)) then
         err = SUEWS_CAPI_BAD_STATE
      elseif (int(size(field, 1), c_int)/=int(nsurf, c_int) .or. &
              int(size(field, 2), c_int)/=ndepth) then
         err = SUEWS_CAPI_BAD_STATE
      end if
   end if

end subroutine require_surf_depth_mat

subroutine heat_state_layout(state, n_flat, nlayer, ndepth, err)
   implicit none

   type(heat_state_shadow), intent(in) :: state
   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: ndepth
   integer(c_int), intent(out) :: err

   nlayer = 0_c_int
   ndepth = 0_c_int
   err = SUEWS_CAPI_OK

   call update_nlayer_from_vec(state%tsfc_roof, nlayer, err)
   call update_nlayer_from_vec(state%tsfc_wall, nlayer, err)
   call update_nlayer_from_vec(state%tsfc_roof_stepstart, nlayer, err)
   call update_nlayer_from_vec(state%tsfc_wall_stepstart, nlayer, err)
   call update_nlayer_from_vec(state%qs_roof, nlayer, err)
   call update_nlayer_from_vec(state%qn_roof, nlayer, err)
   call update_nlayer_from_vec(state%qe_roof, nlayer, err)
   call update_nlayer_from_vec(state%qh_roof, nlayer, err)
   call update_nlayer_from_vec(state%qh_resist_roof, nlayer, err)
   call update_nlayer_from_vec(state%qs_wall, nlayer, err)
   call update_nlayer_from_vec(state%qn_wall, nlayer, err)
   call update_nlayer_from_vec(state%qe_wall, nlayer, err)
   call update_nlayer_from_vec(state%qh_wall, nlayer, err)
   call update_nlayer_from_vec(state%qh_resist_wall, nlayer, err)

   call update_layer_depth_from_mat(state%temp_roof, nlayer, ndepth, err)
   call update_layer_depth_from_mat(state%temp_wall, nlayer, ndepth, err)
   call update_depth_from_surf_mat(state%temp_surf, ndepth, err)
   call update_depth_from_surf_mat(state%temp_surf_dyohm, ndepth, err)

   if ((nlayer==0_c_int .and. ndepth/=0_c_int) .or. &
       (nlayer/=0_c_int .and. ndepth==0_c_int)) then
      err = SUEWS_CAPI_BAD_STATE
   end if

   call require_layer_depth_mat(state%temp_roof, nlayer, ndepth, err)
   call require_layer_depth_mat(state%temp_wall, nlayer, ndepth, err)
   call require_surf_depth_mat(state%temp_surf, nlayer, ndepth, err)
   call require_surf_depth_mat(state%temp_surf_dyohm, nlayer, ndepth, err)

   call require_layer_vec(state%tsfc_roof, nlayer, ndepth, err)
   call require_layer_vec(state%tsfc_wall, nlayer, ndepth, err)
   call require_surf_vec(state%tsfc_surf, nlayer, ndepth, err)
   call require_surf_vec(state%tsfc_surf_dyohm, nlayer, ndepth, err)
   call require_layer_vec(state%tsfc_roof_stepstart, nlayer, ndepth, err)
   call require_layer_vec(state%tsfc_wall_stepstart, nlayer, ndepth, err)
   call require_surf_vec(state%tsfc_surf_stepstart, nlayer, ndepth, err)

   call require_layer_vec(state%qs_roof, nlayer, ndepth, err)
   call require_layer_vec(state%qn_roof, nlayer, ndepth, err)
   call require_layer_vec(state%qe_roof, nlayer, ndepth, err)
   call require_layer_vec(state%qh_roof, nlayer, ndepth, err)
   call require_layer_vec(state%qh_resist_roof, nlayer, ndepth, err)

   call require_layer_vec(state%qs_wall, nlayer, ndepth, err)
   call require_layer_vec(state%qn_wall, nlayer, ndepth, err)
   call require_layer_vec(state%qe_wall, nlayer, ndepth, err)
   call require_layer_vec(state%qh_wall, nlayer, ndepth, err)
   call require_layer_vec(state%qh_resist_wall, nlayer, ndepth, err)

   if (err/=SUEWS_CAPI_OK) then
      n_flat = 0_c_int
      return
   end if

   if (nlayer==0_c_int) then
      n_flat = SUEWS_CAPI_HEAT_STATE_BASE_LEN
   else
      n_flat = SUEWS_CAPI_HEAT_STATE_BASE_LEN + &
               2_c_int * nlayer * ndepth + 2_c_int * int(nsurf, c_int) * ndepth + &
               14_c_int * nlayer + 3_c_int * int(nsurf, c_int)
   end if
   err = SUEWS_CAPI_OK

end subroutine heat_state_layout

subroutine pack_vec(field, flat, idx, n)
   implicit none

   real(c_double), dimension(:), allocatable, intent(in) :: field
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(inout) :: idx
   integer(c_int), intent(in) :: n
   integer(c_int) :: i

   if (n<=0_c_int) return
   do i = 1_c_int, n
      flat(idx) = field(i)
      idx = idx + 1_c_int
   end do

end subroutine pack_vec

subroutine pack_mat(field, flat, idx, n1, n2)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(in) :: field
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(inout) :: idx
   integer(c_int), intent(in) :: n1
   integer(c_int), intent(in) :: n2
   integer(c_int) :: i
   integer(c_int) :: j

   if (n1<=0_c_int .or. n2<=0_c_int) return
   do i = 1_c_int, n1
      do j = 1_c_int, n2
         flat(idx) = field(i, j)
         idx = idx + 1_c_int
      end do
   end do

end subroutine pack_mat

subroutine ensure_vec_alloc(field, n, err)
   implicit none

   real(c_double), dimension(:), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: n
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (n<=0_c_int) then
      if (allocated(field)) deallocate (field)
      return
   end if

   if (allocated(field)) then
      if (int(size(field), c_int)/=n) then
         deallocate (field)
         allocate (field(int(n)))
      end if
   else
      allocate (field(int(n)))
   end if

end subroutine ensure_vec_alloc

subroutine ensure_mat_alloc(field, n1, n2, err)
   implicit none

   real(c_double), dimension(:, :), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: n1
   integer(c_int), intent(in) :: n2
   integer(c_int), intent(inout) :: err

   if (err/=SUEWS_CAPI_OK) return

   if (n1<=0_c_int .or. n2<=0_c_int) then
      if (allocated(field)) deallocate (field)
      return
   end if

   if (allocated(field)) then
      if (int(size(field, 1), c_int)/=n1 .or. &
          int(size(field, 2), c_int)/=n2) then
         deallocate (field)
         allocate (field(int(n1), int(n2)))
      end if
   else
      allocate (field(int(n1), int(n2)))
   end if

end subroutine ensure_mat_alloc

subroutine unpack_vec(flat, idx, field, n)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(inout) :: idx
   real(c_double), dimension(:), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: n
   integer(c_int) :: i

   if (n<=0_c_int) return
   do i = 1_c_int, n
      field(i) = flat(idx)
      idx = idx + 1_c_int
   end do

end subroutine unpack_vec

subroutine unpack_mat(flat, idx, field, n1, n2)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(inout) :: idx
   real(c_double), dimension(:, :), allocatable, intent(inout) :: field
   integer(c_int), intent(in) :: n1
   integer(c_int), intent(in) :: n2
   integer(c_int) :: i
   integer(c_int) :: j

   if (n1<=0_c_int .or. n2<=0_c_int) return
   do i = 1_c_int, n1
      do j = 1_c_int, n2
         field(i, j) = flat(idx)
         idx = idx + 1_c_int
      end do
   end do

end subroutine unpack_mat

subroutine heat_state_pack(state, flat, n_flat, nlayer, ndepth, err)
   implicit none

   type(heat_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: nlayer
   integer(c_int), intent(out) :: ndepth
   integer(c_int), intent(out) :: err
   integer(c_int) :: n_expected
   integer(c_int) :: idx
   integer(c_int) :: surf_vec_len
   integer(c_int) :: i

   call heat_state_layout(state, n_expected, nlayer, ndepth, err)
   if (err/=SUEWS_CAPI_OK) return

   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   surf_vec_len = 0_c_int
   if (nlayer>0_c_int) surf_vec_len = int(nsurf, c_int)

   idx = 1_c_int

   call pack_mat(state%temp_roof, flat, idx, nlayer, ndepth)
   call pack_mat(state%temp_wall, flat, idx, nlayer, ndepth)
   call pack_mat(state%temp_surf, flat, idx, int(nsurf, c_int), ndepth)
   call pack_mat(state%temp_surf_dyohm, flat, idx, int(nsurf, c_int), ndepth)

   call pack_vec(state%tsfc_roof, flat, idx, nlayer)
   call pack_vec(state%tsfc_wall, flat, idx, nlayer)
   call pack_vec(state%tsfc_surf, flat, idx, surf_vec_len)
   call pack_vec(state%tsfc_surf_dyohm, flat, idx, surf_vec_len)
   call pack_vec(state%tsfc_roof_stepstart, flat, idx, nlayer)
   call pack_vec(state%tsfc_wall_stepstart, flat, idx, nlayer)
   call pack_vec(state%tsfc_surf_stepstart, flat, idx, surf_vec_len)

   call pack_vec(state%qs_roof, flat, idx, nlayer)
   call pack_vec(state%qn_roof, flat, idx, nlayer)
   call pack_vec(state%qe_roof, flat, idx, nlayer)
   call pack_vec(state%qh_roof, flat, idx, nlayer)
   call pack_vec(state%qh_resist_roof, flat, idx, nlayer)

   call pack_vec(state%qs_wall, flat, idx, nlayer)
   call pack_vec(state%qn_wall, flat, idx, nlayer)
   call pack_vec(state%qe_wall, flat, idx, nlayer)
   call pack_vec(state%qh_wall, flat, idx, nlayer)
   call pack_vec(state%qh_resist_wall, flat, idx, nlayer)

   do i = 1, nsurf
      flat(idx) = state%qs_surf(i)
      idx = idx + 1_c_int
   end do
   do i = 1, nsurf
      flat(idx) = state%qn_surf(i)
      idx = idx + 1_c_int
   end do
   do i = 1, nsurf
      flat(idx) = state%qe0_surf(i)
      idx = idx + 1_c_int
   end do
   do i = 1, nsurf
      flat(idx) = state%qe_surf(i)
      idx = idx + 1_c_int
   end do
   do i = 1, nsurf
      flat(idx) = state%qh_surf(i)
      idx = idx + 1_c_int
   end do
   do i = 1, nsurf
      flat(idx) = state%qh_resist_surf(i)
      idx = idx + 1_c_int
   end do
   do i = 1, nsurf
      flat(idx) = state%tsurf_ind(i)
      idx = idx + 1_c_int
   end do

   flat(idx) = state%qh_lumps; idx = idx + 1_c_int
   flat(idx) = state%qe_lumps; idx = idx + 1_c_int
   flat(idx) = state%kclear; idx = idx + 1_c_int
   flat(idx) = state%kup; idx = idx + 1_c_int
   flat(idx) = state%ldown; idx = idx + 1_c_int
   flat(idx) = state%lup; idx = idx + 1_c_int
   flat(idx) = state%qe; idx = idx + 1_c_int
   flat(idx) = state%qf; idx = idx + 1_c_int
   flat(idx) = state%qf_sahp; idx = idx + 1_c_int
   flat(idx) = state%qh; idx = idx + 1_c_int
   flat(idx) = state%qh_residual; idx = idx + 1_c_int
   flat(idx) = state%qh_resist; idx = idx + 1_c_int
   flat(idx) = state%qn; idx = idx + 1_c_int
   flat(idx) = state%qn_snowfree; idx = idx + 1_c_int
   flat(idx) = state%qs; idx = idx + 1_c_int
   flat(idx) = state%tsfc_c; idx = idx + 1_c_int
   flat(idx) = state%tsurf; idx = idx + 1_c_int
   flat(idx) = state%qh_init; idx = idx + 1_c_int

   do i = 1, 15
      flat(idx) = state%roof_in_sw_spc(i)
      idx = idx + 1_c_int
   end do
   do i = 1, 15
      flat(idx) = state%roof_in_lw_spc(i)
      idx = idx + 1_c_int
   end do
   do i = 1, 15
      flat(idx) = state%wall_in_sw_spc(i)
      idx = idx + 1_c_int
   end do
   do i = 1, 15
      flat(idx) = state%wall_in_lw_spc(i)
      idx = idx + 1_c_int
   end do

   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine heat_state_pack

subroutine heat_state_unpack(flat, n_flat, nlayer, ndepth, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(in) :: nlayer
   integer(c_int), intent(in) :: ndepth
   type(HEAT_STATE), intent(inout) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: n_expected
   integer(c_int) :: idx
   integer(c_int) :: surf_vec_len
   integer(c_int) :: i

   if (nlayer<0_c_int .or. ndepth<0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if
   if ((nlayer==0_c_int .and. ndepth/=0_c_int) .or. &
       (nlayer/=0_c_int .and. ndepth==0_c_int)) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   if (nlayer==0_c_int) then
      n_expected = SUEWS_CAPI_HEAT_STATE_BASE_LEN
   else
      n_expected = SUEWS_CAPI_HEAT_STATE_BASE_LEN + &
                   2_c_int * nlayer * ndepth + 2_c_int * int(nsurf, c_int) * ndepth + &
                   14_c_int * nlayer + 3_c_int * int(nsurf, c_int)
   end if
   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   surf_vec_len = 0_c_int
   if (nlayer>0_c_int) surf_vec_len = int(nsurf, c_int)

   err = SUEWS_CAPI_OK
   call ensure_mat_alloc(state%temp_roof, nlayer, ndepth, err)
   call ensure_mat_alloc(state%temp_wall, nlayer, ndepth, err)
   call ensure_mat_alloc(state%temp_surf, int(nsurf, c_int), ndepth, err)
   call ensure_mat_alloc(state%temp_surf_dyohm, int(nsurf, c_int), ndepth, err)

   call ensure_vec_alloc(state%tsfc_roof, nlayer, err)
   call ensure_vec_alloc(state%tsfc_wall, nlayer, err)
   call ensure_vec_alloc(state%tsfc_surf, surf_vec_len, err)
   call ensure_vec_alloc(state%tsfc_surf_dyohm, surf_vec_len, err)
   call ensure_vec_alloc(state%tsfc_roof_stepstart, nlayer, err)
   call ensure_vec_alloc(state%tsfc_wall_stepstart, nlayer, err)
   call ensure_vec_alloc(state%tsfc_surf_stepstart, surf_vec_len, err)

   call ensure_vec_alloc(state%qs_roof, nlayer, err)
   call ensure_vec_alloc(state%qn_roof, nlayer, err)
   call ensure_vec_alloc(state%qe_roof, nlayer, err)
   call ensure_vec_alloc(state%qh_roof, nlayer, err)
   call ensure_vec_alloc(state%qh_resist_roof, nlayer, err)

   call ensure_vec_alloc(state%qs_wall, nlayer, err)
   call ensure_vec_alloc(state%qn_wall, nlayer, err)
   call ensure_vec_alloc(state%qe_wall, nlayer, err)
   call ensure_vec_alloc(state%qh_wall, nlayer, err)
   call ensure_vec_alloc(state%qh_resist_wall, nlayer, err)
   if (err/=SUEWS_CAPI_OK) return

   idx = 1_c_int

   call unpack_mat(flat, idx, state%temp_roof, nlayer, ndepth)
   call unpack_mat(flat, idx, state%temp_wall, nlayer, ndepth)
   call unpack_mat(flat, idx, state%temp_surf, int(nsurf, c_int), ndepth)
   call unpack_mat(flat, idx, state%temp_surf_dyohm, int(nsurf, c_int), ndepth)

   call unpack_vec(flat, idx, state%tsfc_roof, nlayer)
   call unpack_vec(flat, idx, state%tsfc_wall, nlayer)
   call unpack_vec(flat, idx, state%tsfc_surf, surf_vec_len)
   call unpack_vec(flat, idx, state%tsfc_surf_dyohm, surf_vec_len)
   call unpack_vec(flat, idx, state%tsfc_roof_stepstart, nlayer)
   call unpack_vec(flat, idx, state%tsfc_wall_stepstart, nlayer)
   call unpack_vec(flat, idx, state%tsfc_surf_stepstart, surf_vec_len)

   call unpack_vec(flat, idx, state%qs_roof, nlayer)
   call unpack_vec(flat, idx, state%qn_roof, nlayer)
   call unpack_vec(flat, idx, state%qe_roof, nlayer)
   call unpack_vec(flat, idx, state%qh_roof, nlayer)
   call unpack_vec(flat, idx, state%qh_resist_roof, nlayer)

   call unpack_vec(flat, idx, state%qs_wall, nlayer)
   call unpack_vec(flat, idx, state%qn_wall, nlayer)
   call unpack_vec(flat, idx, state%qe_wall, nlayer)
   call unpack_vec(flat, idx, state%qh_wall, nlayer)
   call unpack_vec(flat, idx, state%qh_resist_wall, nlayer)

   do i = 1_c_int, int(nsurf, c_int)
      state%qs_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%qn_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%qe0_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%qe_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%qh_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%qh_resist_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%tsurf_ind(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%qh_lumps = flat(idx); idx = idx + 1_c_int
   state%qe_lumps = flat(idx); idx = idx + 1_c_int
   state%kclear = flat(idx); idx = idx + 1_c_int
   state%kup = flat(idx); idx = idx + 1_c_int
   state%ldown = flat(idx); idx = idx + 1_c_int
   state%lup = flat(idx); idx = idx + 1_c_int
   state%qe = flat(idx); idx = idx + 1_c_int
   state%qf = flat(idx); idx = idx + 1_c_int
   state%qf_sahp = flat(idx); idx = idx + 1_c_int
   state%qh = flat(idx); idx = idx + 1_c_int
   state%qh_residual = flat(idx); idx = idx + 1_c_int
   state%qh_resist = flat(idx); idx = idx + 1_c_int
   state%qn = flat(idx); idx = idx + 1_c_int
   state%qn_snowfree = flat(idx); idx = idx + 1_c_int
   state%qs = flat(idx); idx = idx + 1_c_int
   state%tsfc_c = flat(idx); idx = idx + 1_c_int
   state%tsurf = flat(idx); idx = idx + 1_c_int
   state%qh_init = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, 15_c_int
      state%roof_in_sw_spc(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, 15_c_int
      state%roof_in_lw_spc(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, 15_c_int
      state%wall_in_sw_spc(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, 15_c_int
      state%wall_in_lw_spc(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%iter_safe = flat(idx)>=0.5_c_double
   err = SUEWS_CAPI_OK

end subroutine heat_state_unpack

subroutine suews_heat_state_error_message(code, buffer, buffer_len) bind(C, name='suews_heat_state_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_heat_state_error_message

end module module_c_api_heat_state

module c_api_heat_state_module
use module_c_api_heat_state
end module c_api_heat_state_module
