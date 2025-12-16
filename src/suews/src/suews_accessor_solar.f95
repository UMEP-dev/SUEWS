!> @file suews_accessor_solar.f95
!> @brief Accessor functions for solar_State in SUEWS_STATE
!>
!> Provides getter and setter functions to access solar position
!> (zenith, azimuth) from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_solar
   USE module_ctrl_type, ONLY: SUEWS_STATE, solar_State
   IMPLICIT NONE

CONTAINS

   !> Get solar state values
   SUBROUTINE get_solar_state(state, azimuth_deg, zenith_deg)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: azimuth_deg, zenith_deg

      azimuth_deg = state%solarState%azimuth_deg
      zenith_deg = state%solarState%zenith_deg
   END SUBROUTINE get_solar_state

   !> Set solar state values
   SUBROUTINE set_solar_state(state, azimuth_deg, zenith_deg)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: azimuth_deg, zenith_deg

      state%solarState%azimuth_deg = azimuth_deg
      state%solarState%zenith_deg = zenith_deg
   END SUBROUTINE set_solar_state

END MODULE module_accessor_solar
