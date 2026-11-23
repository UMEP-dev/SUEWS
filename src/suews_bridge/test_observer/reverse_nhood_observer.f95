! Hand-maintained reverse binding witness (gh#2096), not generator output.
! Assign named kernel members independently of the production wire order.
module module_test_reverse_nhood_observer
   use, intrinsic :: iso_c_binding, only: c_int, c_double
   use module_type_stebbs, only: NHOOD_STATE
   use module_c_api_driver, only: pack_nhood_state
   use module_c_api_common, only: SUEWS_CAPI_BAD_BUFFER
   implicit none
   private
   public :: suews_obs_pack_nhood_state
contains
   subroutine suews_obs_pack_nhood_state(sample, flat, n_flat, err) bind(C)
      integer(c_int), value, intent(in) :: sample, n_flat
      real(c_double), intent(out) :: flat(*)
      integer(c_int), intent(out) :: err
      type(NHOOD_STATE) :: state
      real(c_double), parameter :: SENTINEL_TAIR = 293.75_c_double
      real(c_double), parameter :: SENTINEL_QN = -41.5_c_double
      real(c_double), parameter :: SENTINEL_U_HBH = 7.125_c_double
      real(c_double), parameter :: SENTINEL_ITER = 19.0_c_double

      if (sample < 0 .or. sample > 2) then
         err = SUEWS_CAPI_BAD_BUFFER
         return
      end if
      ! Three asymmetric cases, with both logical values; not a flat template.
      state%tair_mn_prev = SENTINEL_TAIR + 2.0_c_double * sample
      state%iter_safe = sample == 1
      state%qn_1dravg = SENTINEL_QN - 11.0_c_double * sample
      state%iter_count = SENTINEL_ITER + 3.0_c_double * sample
      state%u_hbh_1dravg = SENTINEL_U_HBH + 0.25_c_double * sample
      call pack_nhood_state(state, flat, n_flat, err)
   end subroutine suews_obs_pack_nhood_state
end module module_test_reverse_nhood_observer
