!> @file suews_accessor_nhood.f95
!> @brief Accessor functions for NHOOD_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access neighbourhood
!> climate variables from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_nhood
   USE module_ctrl_type, ONLY: SUEWS_STATE, NHOOD_STATE
   IMPLICIT NONE

CONTAINS

   !> Get neighbourhood state values
   SUBROUTINE get_nhood_state(state, U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count

      U_hbh_1dravg = state%nhoodState%U_hbh_1dravg
      QN_1dravg = state%nhoodState%QN_1dravg
      Tair_mn_prev = state%nhoodState%Tair_mn_prev
      iter_count = state%nhoodState%iter_count
   END SUBROUTINE get_nhood_state

   !> Set neighbourhood state values
   SUBROUTINE set_nhood_state(state, U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count

      state%nhoodState%U_hbh_1dravg = U_hbh_1dravg
      state%nhoodState%QN_1dravg = QN_1dravg
      state%nhoodState%Tair_mn_prev = Tair_mn_prev
      state%nhoodState%iter_count = iter_count
   END SUBROUTINE set_nhood_state

END MODULE module_accessor_nhood
