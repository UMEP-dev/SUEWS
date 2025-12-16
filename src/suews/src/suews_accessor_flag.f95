!> @file suews_accessor_flag.f95
!> @brief Accessor functions for flag_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access convergence
!> and iteration control flags from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_flag
   USE module_ctrl_type, ONLY: SUEWS_STATE, flag_STATE
   IMPLICIT NONE

CONTAINS

   !> Get flag state values
   SUBROUTINE get_flag_state(state, flag_converge, i_iter, stebbs_bldg_init)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      LOGICAL, INTENT(OUT) :: flag_converge
      INTEGER, INTENT(OUT) :: i_iter, stebbs_bldg_init

      flag_converge = state%flagState%flag_converge
      i_iter = state%flagState%i_iter
      stebbs_bldg_init = state%flagState%stebbs_bldg_init
   END SUBROUTINE get_flag_state

   !> Set flag state values
   SUBROUTINE set_flag_state(state, flag_converge, i_iter, stebbs_bldg_init)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      LOGICAL, INTENT(IN) :: flag_converge
      INTEGER, INTENT(IN) :: i_iter, stebbs_bldg_init

      state%flagState%flag_converge = flag_converge
      state%flagState%i_iter = i_iter
      state%flagState%stebbs_bldg_init = stebbs_bldg_init
   END SUBROUTINE set_flag_state

END MODULE module_accessor_flag
