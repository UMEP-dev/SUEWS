!> @file suews_accessor_snow.f95
!> @brief Accessor functions for SNOW_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access snow pack,
!> snow fraction, density, and ice fraction from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_snow
   USE module_ctrl_type, ONLY: SUEWS_STATE, SNOW_STATE
   USE module_ctrl_const_allocate, ONLY: nsurf
   IMPLICIT NONE

CONTAINS

   !> Get dimensions of SNOW_STATE arrays
   !> Returns nsurf (number of surfaces) for consistency with heat/hydro accessors
   SUBROUTINE get_snow_state_dims(state, nsurf_out)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(OUT) :: nsurf_out

      ! Snow arrays use the module constant nsurf
      nsurf_out = nsurf
   END SUBROUTINE get_snow_state_dims

   !> Get snow state arrays
   SUBROUTINE get_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: snowpack
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: snowfrac
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: snowdens
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: icefrac

      snowpack = state%snowState%SnowPack
      snowfrac = state%snowState%SnowFrac
      snowdens = state%snowState%SnowDens
      icefrac = state%snowState%iceFrac
   END SUBROUTINE get_snow_state_arrays

   !> Set snow state arrays
   SUBROUTINE set_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: snowpack
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: snowfrac
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: snowdens
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: icefrac

      state%snowState%SnowPack = snowpack
      state%snowState%SnowFrac = snowfrac
      state%snowState%SnowDens = snowdens
      state%snowState%iceFrac = icefrac
   END SUBROUTINE set_snow_state_arrays

   !> Get scalar snow state values
   SUBROUTINE get_snow_state_scalars(state, snowalb, swe, mwh, qm)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: snowalb, swe, mwh, qm

      snowalb = state%snowState%SnowAlb
      swe = state%snowState%swe
      mwh = state%snowState%mwh
      qm = state%snowState%qm
   END SUBROUTINE get_snow_state_scalars

   !> Set scalar snow state values
   SUBROUTINE set_snow_state_scalars(state, snowalb, swe, mwh, qm)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: snowalb, swe, mwh, qm

      state%snowState%SnowAlb = snowalb
      state%snowState%swe = swe
      state%snowState%mwh = mwh
      state%snowState%qm = qm
   END SUBROUTINE set_snow_state_scalars

END MODULE module_accessor_snow
