!> @file suews_accessor_heat.f95
!> @brief Accessor functions for HEAT_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access temperature arrays
!> and heat flux scalars from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_heat
   USE module_ctrl_type, ONLY: SUEWS_STATE, HEAT_STATE
   USE module_ctrl_const_allocate, ONLY: nsurf
   IMPLICIT NONE

CONTAINS

   !> Get dimensions of HEAT_STATE allocatable arrays
   SUBROUTINE get_heat_state_dims(state, nlayer, ndepth, nsurf_out)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(OUT) :: nlayer, ndepth, nsurf_out

      IF (ALLOCATED(state%heatState%temp_roof)) THEN
         nlayer = SIZE(state%heatState%temp_roof, 1)
         ndepth = SIZE(state%heatState%temp_roof, 2)
      ELSE
         nlayer = 0
         ndepth = 0
      END IF
      nsurf_out = nsurf
   END SUBROUTINE get_heat_state_dims

   !> Get temperature arrays from HEAT_STATE
   SUBROUTINE get_heat_state_temp(state, nlayer, ndepth, nsurf_in, &
                                  temp_roof, temp_wall, temp_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer, ndepth, nsurf_in
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(OUT) :: temp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(OUT) :: temp_wall
      REAL(KIND(1D0)), DIMENSION(nsurf_in, ndepth), INTENT(OUT) :: temp_surf

      temp_roof = state%heatState%temp_roof
      temp_wall = state%heatState%temp_wall
      temp_surf = state%heatState%temp_surf
   END SUBROUTINE get_heat_state_temp

   !> Set temperature arrays in HEAT_STATE
   SUBROUTINE set_heat_state_temp(state, nlayer, ndepth, nsurf_in, &
                                  temp_roof, temp_wall, temp_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer, ndepth, nsurf_in
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(IN) :: temp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(IN) :: temp_wall
      REAL(KIND(1D0)), DIMENSION(nsurf_in, ndepth), INTENT(IN) :: temp_surf

      state%heatState%temp_roof = temp_roof
      state%heatState%temp_wall = temp_wall
      state%heatState%temp_surf = temp_surf
   END SUBROUTINE set_heat_state_temp

   !> Get surface temperature arrays from HEAT_STATE
   SUBROUTINE get_heat_state_tsfc(state, nlayer, nsurf_in, &
                                  tsfc_roof, tsfc_wall, tsfc_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer, nsurf_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nsurf_in), INTENT(OUT) :: tsfc_surf

      tsfc_roof = state%heatState%tsfc_roof
      tsfc_wall = state%heatState%tsfc_wall
      tsfc_surf = state%heatState%tsfc_surf
   END SUBROUTINE get_heat_state_tsfc

   !> Set surface temperature arrays in HEAT_STATE
   SUBROUTINE set_heat_state_tsfc(state, nlayer, nsurf_in, &
                                  tsfc_roof, tsfc_wall, tsfc_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer, nsurf_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nsurf_in), INTENT(IN) :: tsfc_surf

      state%heatState%tsfc_roof = tsfc_roof
      state%heatState%tsfc_wall = tsfc_wall
      state%heatState%tsfc_surf = tsfc_surf
   END SUBROUTINE set_heat_state_tsfc

   !> Get roof heat flux arrays from HEAT_STATE
   SUBROUTINE get_heat_state_flux_roof(state, nlayer, &
                                       QS_roof, QN_roof, qe_roof, qh_roof)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: QS_roof, QN_roof, qe_roof, qh_roof

      QS_roof = state%heatState%QS_roof
      QN_roof = state%heatState%QN_roof
      qe_roof = state%heatState%qe_roof
      qh_roof = state%heatState%qh_roof
   END SUBROUTINE get_heat_state_flux_roof

   !> Get wall heat flux arrays from HEAT_STATE
   SUBROUTINE get_heat_state_flux_wall(state, nlayer, &
                                       QS_wall, QN_wall, qe_wall, qh_wall)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: QS_wall, QN_wall, qe_wall, qh_wall

      QS_wall = state%heatState%QS_wall
      QN_wall = state%heatState%QN_wall
      qe_wall = state%heatState%qe_wall
      qh_wall = state%heatState%qh_wall
   END SUBROUTINE get_heat_state_flux_wall

   !> Get scalar heat state values
   SUBROUTINE get_heat_state_scalars(state, qh, qe, qs, qn, qf, tsurf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: qh, qe, qs, qn, qf, tsurf

      qh = state%heatState%qh
      qe = state%heatState%qe
      qs = state%heatState%qs
      qn = state%heatState%qn
      qf = state%heatState%qf
      tsurf = state%heatState%TSfc_C
   END SUBROUTINE get_heat_state_scalars

   !> Set scalar heat state values
   SUBROUTINE set_heat_state_scalars(state, qh, qe, qs, qn, qf, tsurf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: qh, qe, qs, qn, qf, tsurf

      state%heatState%qh = qh
      state%heatState%qe = qe
      state%heatState%qs = qs
      state%heatState%qn = qn
      state%heatState%qf = qf
      state%heatState%TSfc_C = tsurf
   END SUBROUTINE set_heat_state_scalars

   !> Get per-surface flux arrays from HEAT_STATE
   SUBROUTINE get_heat_state_surf_flux(state, qs_surf, QN_surf, qe_surf, qh_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: qs_surf, QN_surf, qe_surf, qh_surf

      qs_surf = state%heatState%qs_surf
      QN_surf = state%heatState%QN_surf
      qe_surf = state%heatState%qe_surf
      qh_surf = state%heatState%qh_surf
   END SUBROUTINE get_heat_state_surf_flux

   !> Set per-surface flux arrays in HEAT_STATE
   SUBROUTINE set_heat_state_surf_flux(state, qs_surf, QN_surf, qe_surf, qh_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: qs_surf, QN_surf, qe_surf, qh_surf

      state%heatState%qs_surf = qs_surf
      state%heatState%QN_surf = QN_surf
      state%heatState%qe_surf = qe_surf
      state%heatState%qh_surf = qh_surf
   END SUBROUTINE set_heat_state_surf_flux

END MODULE module_accessor_heat
