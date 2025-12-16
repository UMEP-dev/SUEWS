!> @file suews_accessor_hydro.f95
!> @brief Accessor functions for HYDRO_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access soil moisture,
!> wetness state, and evaporation arrays from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_hydro
   USE module_ctrl_type, ONLY: SUEWS_STATE, HYDRO_STATE
   USE module_ctrl_const_allocate, ONLY: nsurf
   IMPLICIT NONE

CONTAINS

   !> Get dimensions of HYDRO_STATE allocatable arrays
   SUBROUTINE get_hydro_state_dims(state, nlayer, nsurf_out)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(OUT) :: nlayer, nsurf_out

      IF (ALLOCATED(state%hydroState%soilstore_roof)) THEN
         nlayer = SIZE(state%hydroState%soilstore_roof)
      ELSE
         nlayer = 0
      END IF
      nsurf_out = nsurf
   END SUBROUTINE get_hydro_state_dims

   !> Get soil store arrays from HYDRO_STATE
   SUBROUTINE get_hydro_state_soilstore(state, nlayer, &
                                        soilstore_roof, soilstore_wall, soilstore_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: soilstore_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: soilstore_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: soilstore_surf

      soilstore_roof = state%hydroState%soilstore_roof
      soilstore_wall = state%hydroState%soilstore_wall
      soilstore_surf = state%hydroState%soilstore_surf
   END SUBROUTINE get_hydro_state_soilstore

   !> Set soil store arrays in HYDRO_STATE
   SUBROUTINE set_hydro_state_soilstore(state, nlayer, &
                                        soilstore_roof, soilstore_wall, soilstore_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: soilstore_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: soilstore_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: soilstore_surf

      state%hydroState%soilstore_roof = soilstore_roof
      state%hydroState%soilstore_wall = soilstore_wall
      state%hydroState%soilstore_surf = soilstore_surf
   END SUBROUTINE set_hydro_state_soilstore

   !> Get wetness state arrays from HYDRO_STATE
   SUBROUTINE get_hydro_state_wetness(state, nlayer, &
                                      state_roof, state_wall, state_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: state_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: state_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: state_surf

      state_roof = state%hydroState%state_roof
      state_wall = state%hydroState%state_wall
      state_surf = state%hydroState%state_surf
   END SUBROUTINE get_hydro_state_wetness

   !> Set wetness state arrays in HYDRO_STATE
   SUBROUTINE set_hydro_state_wetness(state, nlayer, &
                                      state_roof, state_wall, state_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: state_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: state_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: state_surf

      state%hydroState%state_roof = state_roof
      state%hydroState%state_wall = state_wall
      state%hydroState%state_surf = state_surf
   END SUBROUTINE set_hydro_state_wetness

   !> Get evaporation arrays from HYDRO_STATE
   SUBROUTINE get_hydro_state_evap(state, nlayer, &
                                   ev_roof, ev_wall, ev_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: ev_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: ev_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: ev_surf

      ev_roof = state%hydroState%ev_roof
      ev_wall = state%hydroState%ev_wall
      ev_surf = state%hydroState%ev_surf
   END SUBROUTINE get_hydro_state_evap

   !> Get scalar hydro state values
   SUBROUTINE get_hydro_state_scalars(state, smd, runoff_per_tstep, ev_per_tstep, drain_per_tstep)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: smd, runoff_per_tstep, ev_per_tstep, drain_per_tstep

      smd = state%hydroState%smd
      runoff_per_tstep = state%hydroState%runoff_per_tstep
      ev_per_tstep = state%hydroState%ev_per_tstep
      drain_per_tstep = state%hydroState%drain_per_tstep
   END SUBROUTINE get_hydro_state_scalars

END MODULE module_accessor_hydro
