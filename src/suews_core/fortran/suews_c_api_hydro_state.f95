! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for HYDRO_STATE.
! -----------------------------------------------------------------------------
module module_c_api_hydro_state
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_ctrl_const_allocate, only: nsurf
use module_type_hydro, only: HYDRO_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_HYDRO_STATE_BASE_LEN = 10_c_int * int(nsurf, c_int) + 34_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_HYDRO_STATE_SCHEMA_VERSION = 1_c_int

type :: hydro_state_shadow
   real(c_double), dimension(nsurf) :: soilstore_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: state_surf = 0.0_c_double
   real(c_double), dimension(9) :: wuday_id = 0.0_c_double

   real(c_double), dimension(:), allocatable :: soilstore_roof
   real(c_double), dimension(:), allocatable :: state_roof
   real(c_double), dimension(:), allocatable :: soilstore_wall
   real(c_double), dimension(:), allocatable :: state_wall
   real(c_double), dimension(:), allocatable :: ev_roof
   real(c_double), dimension(:), allocatable :: ev_wall

   real(c_double), dimension(nsurf) :: ev0_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: ev_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: wu_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: runoff_soil = 0.0_c_double
   real(c_double), dimension(nsurf) :: smd_surf = 0.0_c_double
   real(c_double), dimension(nsurf) :: drain_surf = 0.0_c_double

   real(c_double) :: drain_per_tstep = 0.0_c_double
   real(c_double) :: ev_per_tstep = 0.0_c_double
   real(c_double) :: wu_ext = 0.0_c_double
   real(c_double) :: wu_int = 0.0_c_double

   real(c_double) :: runoff_agveg = 0.0_c_double
   real(c_double) :: runoff_agimpervious = 0.0_c_double
   real(c_double) :: runoff_per_tstep = 0.0_c_double
   real(c_double) :: runoff_pipes = 0.0_c_double
   real(c_double) :: runoff_soil_per_tstep = 0.0_c_double
   real(c_double) :: runoff_waterbody = 0.0_c_double
   real(c_double) :: smd = 0.0_c_double
   real(c_double) :: soil_state = 0.0_c_double
   real(c_double) :: state_per_tstep = 0.0_c_double
   real(c_double) :: surf_chang_per_tstep = 0.0_c_double
   real(c_double) :: tot_chang_per_tstep = 0.0_c_double
   real(c_double) :: runoff_per_interval = 0.0_c_double
   real(c_double) :: nwstate_per_tstep = 0.0_c_double

   real(c_double) :: soil_moist_cap = 0.0_c_double
   real(c_double) :: vsmd = 0.0_c_double

   real(c_double) :: additional_water = 0.0_c_double
   real(c_double) :: add_impervious = 0.0_c_double
   real(c_double) :: add_pipes = 0.0_c_double
   real(c_double) :: add_veg = 0.0_c_double
   real(c_double) :: add_waterbody = 0.0_c_double
   real(c_double), dimension(nsurf) :: add_water = 0.0_c_double
   real(c_double), dimension(nsurf) :: frac_water2runoff = 0.0_c_double

   logical :: iter_safe = .false.
end type hydro_state_shadow

public :: suews_hydro_state_len
public :: suews_hydro_state_schema_version
public :: suews_hydro_state_default
public :: suews_hydro_state_error_message
public :: hydro_state_unpack

contains

subroutine suews_hydro_state_len( &
   n_flat, &
   soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
   err) bind(C, name='suews_hydro_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: soilstore_roof_len
   integer(c_int), intent(out) :: state_roof_len
   integer(c_int), intent(out) :: soilstore_wall_len
   integer(c_int), intent(out) :: state_wall_len
   integer(c_int), intent(out) :: ev_roof_len
   integer(c_int), intent(out) :: ev_wall_len
   integer(c_int), intent(out) :: err

   type(hydro_state_shadow) :: state

   call hydro_state_layout( &
      state, n_flat, &
      soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
      err)

end subroutine suews_hydro_state_len

subroutine suews_hydro_state_schema_version(schema_version, err) bind(C, name='suews_hydro_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_HYDRO_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_hydro_state_schema_version

subroutine suews_hydro_state_default( &
   flat, n_flat, &
   soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
   err) bind(C, name='suews_hydro_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: soilstore_roof_len
   integer(c_int), intent(out) :: state_roof_len
   integer(c_int), intent(out) :: soilstore_wall_len
   integer(c_int), intent(out) :: state_wall_len
   integer(c_int), intent(out) :: ev_roof_len
   integer(c_int), intent(out) :: ev_wall_len
   integer(c_int), intent(out) :: err

   type(hydro_state_shadow) :: state

   call hydro_state_pack( &
      state, flat, n_flat, &
      soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
      err)

end subroutine suews_hydro_state_default

subroutine hydro_state_layout( &
   state, n_flat, &
   soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
   err)
   implicit none

   type(hydro_state_shadow), intent(in) :: state
   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: soilstore_roof_len
   integer(c_int), intent(out) :: state_roof_len
   integer(c_int), intent(out) :: soilstore_wall_len
   integer(c_int), intent(out) :: state_wall_len
   integer(c_int), intent(out) :: ev_roof_len
   integer(c_int), intent(out) :: ev_wall_len
   integer(c_int), intent(out) :: err

   soilstore_roof_len = 0_c_int
   state_roof_len = 0_c_int
   soilstore_wall_len = 0_c_int
   state_wall_len = 0_c_int
   ev_roof_len = 0_c_int
   ev_wall_len = 0_c_int

   if (allocated(state%soilstore_roof)) soilstore_roof_len = int(size(state%soilstore_roof), c_int)
   if (allocated(state%state_roof)) state_roof_len = int(size(state%state_roof), c_int)
   if (allocated(state%soilstore_wall)) soilstore_wall_len = int(size(state%soilstore_wall), c_int)
   if (allocated(state%state_wall)) state_wall_len = int(size(state%state_wall), c_int)
   if (allocated(state%ev_roof)) ev_roof_len = int(size(state%ev_roof), c_int)
   if (allocated(state%ev_wall)) ev_wall_len = int(size(state%ev_wall), c_int)

   n_flat = SUEWS_CAPI_HYDRO_STATE_BASE_LEN + &
            soilstore_roof_len + state_roof_len + soilstore_wall_len + state_wall_len + ev_roof_len + ev_wall_len
   err = SUEWS_CAPI_OK

end subroutine hydro_state_layout

subroutine hydro_state_pack( &
   state, flat, n_flat, &
   soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
   err)
   implicit none

   type(hydro_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: soilstore_roof_len
   integer(c_int), intent(out) :: state_roof_len
   integer(c_int), intent(out) :: soilstore_wall_len
   integer(c_int), intent(out) :: state_wall_len
   integer(c_int), intent(out) :: ev_roof_len
   integer(c_int), intent(out) :: ev_wall_len
   integer(c_int), intent(out) :: err

   integer(c_int) :: n_expected
   integer :: idx
   integer :: i

   call hydro_state_layout( &
      state, n_expected, &
      soilstore_roof_len, state_roof_len, soilstore_wall_len, state_wall_len, ev_roof_len, ev_wall_len, &
      err)
   if (err/=SUEWS_CAPI_OK) then
      return
   end if

   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1

   do i = 1, nsurf
      flat(idx) = state%soilstore_surf(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%state_surf(i)
      idx = idx + 1
   end do

   do i = 1, 9
      flat(idx) = state%wuday_id(i)
      idx = idx + 1
   end do

   if (soilstore_roof_len>0_c_int) then
      do i = 1, int(soilstore_roof_len)
         flat(idx) = state%soilstore_roof(i)
         idx = idx + 1
      end do
   end if

   if (state_roof_len>0_c_int) then
      do i = 1, int(state_roof_len)
         flat(idx) = state%state_roof(i)
         idx = idx + 1
      end do
   end if

   if (soilstore_wall_len>0_c_int) then
      do i = 1, int(soilstore_wall_len)
         flat(idx) = state%soilstore_wall(i)
         idx = idx + 1
      end do
   end if

   if (state_wall_len>0_c_int) then
      do i = 1, int(state_wall_len)
         flat(idx) = state%state_wall(i)
         idx = idx + 1
      end do
   end if

   if (ev_roof_len>0_c_int) then
      do i = 1, int(ev_roof_len)
         flat(idx) = state%ev_roof(i)
         idx = idx + 1
      end do
   end if

   if (ev_wall_len>0_c_int) then
      do i = 1, int(ev_wall_len)
         flat(idx) = state%ev_wall(i)
         idx = idx + 1
      end do
   end if

   do i = 1, nsurf
      flat(idx) = state%ev0_surf(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%ev_surf(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%wu_surf(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%runoff_soil(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%smd_surf(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%drain_surf(i)
      idx = idx + 1
   end do

   flat(idx) = state%drain_per_tstep; idx = idx + 1
   flat(idx) = state%ev_per_tstep; idx = idx + 1
   flat(idx) = state%wu_ext; idx = idx + 1
   flat(idx) = state%wu_int; idx = idx + 1

   flat(idx) = state%runoff_agveg; idx = idx + 1
   flat(idx) = state%runoff_agimpervious; idx = idx + 1
   flat(idx) = state%runoff_per_tstep; idx = idx + 1
   flat(idx) = state%runoff_pipes; idx = idx + 1
   flat(idx) = state%runoff_soil_per_tstep; idx = idx + 1
   flat(idx) = state%runoff_waterbody; idx = idx + 1
   flat(idx) = state%smd; idx = idx + 1
   flat(idx) = state%soil_state; idx = idx + 1
   flat(idx) = state%state_per_tstep; idx = idx + 1
   flat(idx) = state%surf_chang_per_tstep; idx = idx + 1
   flat(idx) = state%tot_chang_per_tstep; idx = idx + 1
   flat(idx) = state%runoff_per_interval; idx = idx + 1
   flat(idx) = state%nwstate_per_tstep; idx = idx + 1

   flat(idx) = state%soil_moist_cap; idx = idx + 1
   flat(idx) = state%vsmd; idx = idx + 1

   flat(idx) = state%additional_water; idx = idx + 1
   flat(idx) = state%add_impervious; idx = idx + 1
   flat(idx) = state%add_pipes; idx = idx + 1
   flat(idx) = state%add_veg; idx = idx + 1
   flat(idx) = state%add_waterbody; idx = idx + 1

   do i = 1, nsurf
      flat(idx) = state%add_water(i)
      idx = idx + 1
   end do

   do i = 1, nsurf
      flat(idx) = state%frac_water2runoff(i)
      idx = idx + 1
   end do

   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine hydro_state_pack

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

subroutine hydro_state_unpack(flat, n_flat, nlayer, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(in) :: nlayer
   type(HYDRO_STATE), intent(inout) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: n_expected
   integer(c_int) :: idx
   integer(c_int) :: i

   if (nlayer<0_c_int) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   n_expected = SUEWS_CAPI_HYDRO_STATE_BASE_LEN + 6_c_int * nlayer
   if (n_flat<n_expected) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   err = SUEWS_CAPI_OK
   call ensure_vec_alloc(state%soilstore_roof, nlayer, err)
   call ensure_vec_alloc(state%state_roof, nlayer, err)
   call ensure_vec_alloc(state%soilstore_wall, nlayer, err)
   call ensure_vec_alloc(state%state_wall, nlayer, err)
   call ensure_vec_alloc(state%ev_roof, nlayer, err)
   call ensure_vec_alloc(state%ev_wall, nlayer, err)
   if (err/=SUEWS_CAPI_OK) return

   idx = 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      state%soilstore_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nsurf, c_int)
      state%state_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, 9_c_int
      state%wuday_id(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, nlayer
      state%soilstore_roof(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, nlayer
      state%state_roof(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, nlayer
      state%soilstore_wall(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, nlayer
      state%state_wall(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, nlayer
      state%ev_roof(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, nlayer
      state%ev_wall(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nsurf, c_int)
      state%ev0_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%ev_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%wu_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%runoffsoil(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%smd_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   do i = 1_c_int, int(nsurf, c_int)
      state%drain_surf(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%drain_per_tstep = flat(idx); idx = idx + 1_c_int
   state%ev_per_tstep = flat(idx); idx = idx + 1_c_int
   state%wu_ext = flat(idx); idx = idx + 1_c_int
   state%wu_int = flat(idx); idx = idx + 1_c_int

   state%runoffagveg = flat(idx); idx = idx + 1_c_int
   state%runoffagimpervious = flat(idx); idx = idx + 1_c_int
   state%runoff_per_tstep = flat(idx); idx = idx + 1_c_int
   state%runoffpipes = flat(idx); idx = idx + 1_c_int
   state%runoffsoil_per_tstep = flat(idx); idx = idx + 1_c_int
   state%runoffwaterbody = flat(idx); idx = idx + 1_c_int
   state%smd = flat(idx); idx = idx + 1_c_int
   state%soilstate = flat(idx); idx = idx + 1_c_int
   state%state_per_tstep = flat(idx); idx = idx + 1_c_int
   state%surf_chang_per_tstep = flat(idx); idx = idx + 1_c_int
   state%tot_chang_per_tstep = flat(idx); idx = idx + 1_c_int
   state%runoff_per_interval = flat(idx); idx = idx + 1_c_int
   state%nwstate_per_tstep = flat(idx); idx = idx + 1_c_int

   state%soilmoistcap = flat(idx); idx = idx + 1_c_int
   state%vsmd = flat(idx); idx = idx + 1_c_int

   state%additionalwater = flat(idx); idx = idx + 1_c_int
   state%addimpervious = flat(idx); idx = idx + 1_c_int
   state%addpipes = flat(idx); idx = idx + 1_c_int
   state%addveg = flat(idx); idx = idx + 1_c_int
   state%addwaterbody = flat(idx); idx = idx + 1_c_int

   do i = 1_c_int, int(nsurf, c_int)
      state%addwater(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   do i = 1_c_int, int(nsurf, c_int)
      state%frac_water2runoff(i) = flat(idx)
      idx = idx + 1_c_int
   end do

   state%iter_safe = flat(idx)>=0.5_c_double
   err = SUEWS_CAPI_OK

end subroutine hydro_state_unpack

subroutine suews_hydro_state_error_message(code, buffer, buffer_len) bind(C, name='suews_hydro_state_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_hydro_state_error_message

end module module_c_api_hydro_state

module c_api_hydro_state_module
use module_c_api_hydro_state
end module c_api_hydro_state_module
