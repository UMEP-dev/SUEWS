!> @file suews_accessor_ohm.f95
!> @brief Accessor functions for OHM_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access OHM (Objective
!> Hysteresis Model) state variables from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_ohm
   USE module_ctrl_type, ONLY: SUEWS_STATE, OHM_STATE
   IMPLICIT NONE

CONTAINS

   !> Get OHM state basic values (radiation and rates)
   SUBROUTINE get_ohm_state_radiation(state, qn_av, dqndt, qn_s_av, dqnsdt)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: qn_av, dqndt, qn_s_av, dqnsdt

      qn_av = state%ohmState%qn_av
      dqndt = state%ohmState%dqndt
      qn_s_av = state%ohmState%qn_s_av
      dqnsdt = state%ohmState%dqnsdt
   END SUBROUTINE get_ohm_state_radiation

   !> Set OHM state basic values (radiation and rates)
   SUBROUTINE set_ohm_state_radiation(state, qn_av, dqndt, qn_s_av, dqnsdt)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: qn_av, dqndt, qn_s_av, dqnsdt

      state%ohmState%qn_av = qn_av
      state%ohmState%dqndt = dqndt
      state%ohmState%qn_s_av = qn_s_av
      state%ohmState%dqnsdt = dqnsdt
   END SUBROUTINE set_ohm_state_radiation

   !> Get OHM state grid coefficients
   SUBROUTINE get_ohm_state_coef_grid(state, a1, a2, a3)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: a1, a2, a3

      a1 = state%ohmState%a1
      a2 = state%ohmState%a2
      a3 = state%ohmState%a3
   END SUBROUTINE get_ohm_state_coef_grid

   !> Set OHM state grid coefficients
   SUBROUTINE set_ohm_state_coef_grid(state, a1, a2, a3)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: a1, a2, a3

      state%ohmState%a1 = a1
      state%ohmState%a2 = a2
      state%ohmState%a3 = a3
   END SUBROUTINE set_ohm_state_coef_grid

   !> Get OHM state running averages
   SUBROUTINE get_ohm_state_averages(state, t2_prev, ws_rav, tair_prev, qn_rav)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: t2_prev, ws_rav, tair_prev, qn_rav

      t2_prev = state%ohmState%t2_prev
      ws_rav = state%ohmState%ws_rav
      tair_prev = state%ohmState%tair_prev
      qn_rav = state%ohmState%qn_rav
   END SUBROUTINE get_ohm_state_averages

   !> Set OHM state running averages
   SUBROUTINE set_ohm_state_averages(state, t2_prev, ws_rav, tair_prev, qn_rav)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: t2_prev, ws_rav, tair_prev, qn_rav

      state%ohmState%t2_prev = t2_prev
      state%ohmState%ws_rav = ws_rav
      state%ohmState%tair_prev = tair_prev
      state%ohmState%qn_rav = qn_rav
   END SUBROUTINE set_ohm_state_averages

   !> Get OHM state dynamic coefficients for all surfaces
   SUBROUTINE get_ohm_state_coef_surf(state, &
                                      a1_bldg, a2_bldg, a3_bldg, &
                                      a1_paved, a2_paved, a3_paved, &
                                      a1_evetr, a2_evetr, a3_evetr, &
                                      a1_dectr, a2_dectr, a3_dectr, &
                                      a1_grass, a2_grass, a3_grass, &
                                      a1_bsoil, a2_bsoil, a3_bsoil, &
                                      a1_water, a2_water, a3_water)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: a1_bldg, a2_bldg, a3_bldg
      REAL(KIND(1D0)), INTENT(OUT) :: a1_paved, a2_paved, a3_paved
      REAL(KIND(1D0)), INTENT(OUT) :: a1_evetr, a2_evetr, a3_evetr
      REAL(KIND(1D0)), INTENT(OUT) :: a1_dectr, a2_dectr, a3_dectr
      REAL(KIND(1D0)), INTENT(OUT) :: a1_grass, a2_grass, a3_grass
      REAL(KIND(1D0)), INTENT(OUT) :: a1_bsoil, a2_bsoil, a3_bsoil
      REAL(KIND(1D0)), INTENT(OUT) :: a1_water, a2_water, a3_water

      a1_bldg = state%ohmState%a1_bldg
      a2_bldg = state%ohmState%a2_bldg
      a3_bldg = state%ohmState%a3_bldg
      a1_paved = state%ohmState%a1_paved
      a2_paved = state%ohmState%a2_paved
      a3_paved = state%ohmState%a3_paved
      a1_evetr = state%ohmState%a1_evetr
      a2_evetr = state%ohmState%a2_evetr
      a3_evetr = state%ohmState%a3_evetr
      a1_dectr = state%ohmState%a1_dectr
      a2_dectr = state%ohmState%a2_dectr
      a3_dectr = state%ohmState%a3_dectr
      a1_grass = state%ohmState%a1_grass
      a2_grass = state%ohmState%a2_grass
      a3_grass = state%ohmState%a3_grass
      a1_bsoil = state%ohmState%a1_bsoil
      a2_bsoil = state%ohmState%a2_bsoil
      a3_bsoil = state%ohmState%a3_bsoil
      a1_water = state%ohmState%a1_water
      a2_water = state%ohmState%a2_water
      a3_water = state%ohmState%a3_water
   END SUBROUTINE get_ohm_state_coef_surf

   !> Set OHM state dynamic coefficients for all surfaces
   SUBROUTINE set_ohm_state_coef_surf(state, &
                                      a1_bldg, a2_bldg, a3_bldg, &
                                      a1_paved, a2_paved, a3_paved, &
                                      a1_evetr, a2_evetr, a3_evetr, &
                                      a1_dectr, a2_dectr, a3_dectr, &
                                      a1_grass, a2_grass, a3_grass, &
                                      a1_bsoil, a2_bsoil, a3_bsoil, &
                                      a1_water, a2_water, a3_water)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: a1_bldg, a2_bldg, a3_bldg
      REAL(KIND(1D0)), INTENT(IN) :: a1_paved, a2_paved, a3_paved
      REAL(KIND(1D0)), INTENT(IN) :: a1_evetr, a2_evetr, a3_evetr
      REAL(KIND(1D0)), INTENT(IN) :: a1_dectr, a2_dectr, a3_dectr
      REAL(KIND(1D0)), INTENT(IN) :: a1_grass, a2_grass, a3_grass
      REAL(KIND(1D0)), INTENT(IN) :: a1_bsoil, a2_bsoil, a3_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: a1_water, a2_water, a3_water

      state%ohmState%a1_bldg = a1_bldg
      state%ohmState%a2_bldg = a2_bldg
      state%ohmState%a3_bldg = a3_bldg
      state%ohmState%a1_paved = a1_paved
      state%ohmState%a2_paved = a2_paved
      state%ohmState%a3_paved = a3_paved
      state%ohmState%a1_evetr = a1_evetr
      state%ohmState%a2_evetr = a2_evetr
      state%ohmState%a3_evetr = a3_evetr
      state%ohmState%a1_dectr = a1_dectr
      state%ohmState%a2_dectr = a2_dectr
      state%ohmState%a3_dectr = a3_dectr
      state%ohmState%a1_grass = a1_grass
      state%ohmState%a2_grass = a2_grass
      state%ohmState%a3_grass = a3_grass
      state%ohmState%a1_bsoil = a1_bsoil
      state%ohmState%a2_bsoil = a2_bsoil
      state%ohmState%a3_bsoil = a3_bsoil
      state%ohmState%a1_water = a1_water
      state%ohmState%a2_water = a2_water
      state%ohmState%a3_water = a3_water
   END SUBROUTINE set_ohm_state_coef_surf

END MODULE module_accessor_ohm
