!> @file suews_accessor_anthro.f95
!> @brief Accessor functions for anthroEmis_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access anthropogenic
!> emissions state variables (HDD, CO2 fluxes) from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_anthro
   USE module_ctrl_type, ONLY: SUEWS_STATE, anthroEmis_STATE
   IMPLICIT NONE

CONTAINS

   !> Get anthropogenic emissions state HDD array
   SUBROUTINE get_anthro_state_hdd(state, HDD_id)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(12), INTENT(OUT) :: HDD_id

      HDD_id = state%anthroemisState%HDD_id
   END SUBROUTINE get_anthro_state_hdd

   !> Set anthropogenic emissions state HDD array
   SUBROUTINE set_anthro_state_hdd(state, HDD_id)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(12), INTENT(IN) :: HDD_id

      state%anthroemisState%HDD_id = HDD_id
   END SUBROUTINE set_anthro_state_hdd

   !> Get anthropogenic emissions state CO2 flux scalars
   SUBROUTINE get_anthro_state_co2(state, Fc, Fc_anthro, Fc_biogen, Fc_build, &
                                   Fc_metab, Fc_photo, Fc_point, Fc_respi, Fc_traff)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: Fc, Fc_anthro, Fc_biogen, Fc_build
      REAL(KIND(1D0)), INTENT(OUT) :: Fc_metab, Fc_photo, Fc_point, Fc_respi, Fc_traff

      Fc = state%anthroemisState%Fc
      Fc_anthro = state%anthroemisState%Fc_anthro
      Fc_biogen = state%anthroemisState%Fc_biogen
      Fc_build = state%anthroemisState%Fc_build
      Fc_metab = state%anthroemisState%Fc_metab
      Fc_photo = state%anthroemisState%Fc_photo
      Fc_point = state%anthroemisState%Fc_point
      Fc_respi = state%anthroemisState%Fc_respi
      Fc_traff = state%anthroemisState%Fc_traff
   END SUBROUTINE get_anthro_state_co2

   !> Set anthropogenic emissions state CO2 flux scalars
   SUBROUTINE set_anthro_state_co2(state, Fc, Fc_anthro, Fc_biogen, Fc_build, &
                                   Fc_metab, Fc_photo, Fc_point, Fc_respi, Fc_traff)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: Fc, Fc_anthro, Fc_biogen, Fc_build
      REAL(KIND(1D0)), INTENT(IN) :: Fc_metab, Fc_photo, Fc_point, Fc_respi, Fc_traff

      state%anthroemisState%Fc = Fc
      state%anthroemisState%Fc_anthro = Fc_anthro
      state%anthroemisState%Fc_biogen = Fc_biogen
      state%anthroemisState%Fc_build = Fc_build
      state%anthroemisState%Fc_metab = Fc_metab
      state%anthroemisState%Fc_photo = Fc_photo
      state%anthroemisState%Fc_point = Fc_point
      state%anthroemisState%Fc_respi = Fc_respi
      state%anthroemisState%Fc_traff = Fc_traff
   END SUBROUTINE set_anthro_state_co2

END MODULE module_accessor_anthro
