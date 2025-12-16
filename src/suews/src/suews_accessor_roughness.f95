!> @file suews_accessor_roughness.f95
!> @brief Accessor functions for ROUGHNESS_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access aerodynamic
!> roughness parameters (FAI, PAI, z0, zd) from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_roughness
   USE module_ctrl_type, ONLY: SUEWS_STATE, ROUGHNESS_STATE
   IMPLICIT NONE

CONTAINS

   !> Get roughness state values
   SUBROUTINE get_roughness_state(state, FAIBldg_use, FAIEveTree_use, FAIDecTree_use, &
                                  FAI, PAI, Zh, z0m, z0v, zdm, ZZD)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: FAIBldg_use, FAIEveTree_use, FAIDecTree_use
      REAL(KIND(1D0)), INTENT(OUT) :: FAI, PAI, Zh, z0m, z0v, zdm, ZZD

      FAIBldg_use = state%roughnessState%FAIBldg_use
      FAIEveTree_use = state%roughnessState%FAIEveTree_use
      FAIDecTree_use = state%roughnessState%FAIDecTree_use
      FAI = state%roughnessState%FAI
      PAI = state%roughnessState%PAI
      Zh = state%roughnessState%Zh
      z0m = state%roughnessState%z0m
      z0v = state%roughnessState%z0v
      zdm = state%roughnessState%zdm
      ZZD = state%roughnessState%ZZD
   END SUBROUTINE get_roughness_state

   !> Set roughness state values
   SUBROUTINE set_roughness_state(state, FAIBldg_use, FAIEveTree_use, FAIDecTree_use, &
                                  FAI, PAI, Zh, z0m, z0v, zdm, ZZD)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: FAIBldg_use, FAIEveTree_use, FAIDecTree_use
      REAL(KIND(1D0)), INTENT(IN) :: FAI, PAI, Zh, z0m, z0v, zdm, ZZD

      state%roughnessState%FAIBldg_use = FAIBldg_use
      state%roughnessState%FAIEveTree_use = FAIEveTree_use
      state%roughnessState%FAIDecTree_use = FAIDecTree_use
      state%roughnessState%FAI = FAI
      state%roughnessState%PAI = PAI
      state%roughnessState%Zh = Zh
      state%roughnessState%z0m = z0m
      state%roughnessState%z0v = z0v
      state%roughnessState%zdm = zdm
      state%roughnessState%ZZD = ZZD
   END SUBROUTINE set_roughness_state

END MODULE module_accessor_roughness
