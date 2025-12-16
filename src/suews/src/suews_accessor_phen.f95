!> @file suews_accessor_phen.f95
!> @brief Accessor functions for PHENOLOGY_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access phenology state
!> variables (albedo, LAI, degree days, conductance) from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_phen
   USE module_ctrl_type, ONLY: SUEWS_STATE, PHENOLOGY_STATE
   USE module_ctrl_const_allocate, ONLY: nsurf, nvegsurf
   IMPLICIT NONE

CONTAINS

   !> Get phenology state surface albedo array
   SUBROUTINE get_phen_state_alb(state, alb)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: alb

      alb = state%phenState%alb
   END SUBROUTINE get_phen_state_alb

   !> Set phenology state surface albedo array
   SUBROUTINE set_phen_state_alb(state, alb)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: alb

      state%phenState%alb = alb
   END SUBROUTINE set_phen_state_alb

   !> Get phenology state LAI and degree day arrays
   SUBROUTINE get_phen_state_lai(state, lai_id, GDD_id, SDD_id)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(OUT) :: lai_id, GDD_id, SDD_id

      lai_id = state%phenState%lai_id
      GDD_id = state%phenState%GDD_id
      SDD_id = state%phenState%SDD_id
   END SUBROUTINE get_phen_state_lai

   !> Set phenology state LAI and degree day arrays
   SUBROUTINE set_phen_state_lai(state, lai_id, GDD_id, SDD_id)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_id, GDD_id, SDD_id

      state%phenState%lai_id = lai_id
      state%phenState%GDD_id = GDD_id
      state%phenState%SDD_id = SDD_id
   END SUBROUTINE set_phen_state_lai

   !> Get phenology state scalar values
   SUBROUTINE get_phen_state_scalars(state, porosity_id, decidcap_id, &
                                     albDecTr_id, albEveTr_id, albGrass_id, &
                                     Tmin_id, Tmax_id, lenDay_id, TempVeg)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: porosity_id, decidcap_id
      REAL(KIND(1D0)), INTENT(OUT) :: albDecTr_id, albEveTr_id, albGrass_id
      REAL(KIND(1D0)), INTENT(OUT) :: Tmin_id, Tmax_id, lenDay_id, TempVeg

      porosity_id = state%phenState%porosity_id
      decidcap_id = state%phenState%decidcap_id
      albDecTr_id = state%phenState%albDecTr_id
      albEveTr_id = state%phenState%albEveTr_id
      albGrass_id = state%phenState%albGrass_id
      Tmin_id = state%phenState%Tmin_id
      Tmax_id = state%phenState%Tmax_id
      lenDay_id = state%phenState%lenDay_id
      TempVeg = state%phenState%TempVeg
   END SUBROUTINE get_phen_state_scalars

   !> Set phenology state scalar values
   SUBROUTINE set_phen_state_scalars(state, porosity_id, decidcap_id, &
                                     albDecTr_id, albEveTr_id, albGrass_id, &
                                     Tmin_id, Tmax_id, lenDay_id, TempVeg)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: porosity_id, decidcap_id
      REAL(KIND(1D0)), INTENT(IN) :: albDecTr_id, albEveTr_id, albGrass_id
      REAL(KIND(1D0)), INTENT(IN) :: Tmin_id, Tmax_id, lenDay_id, TempVeg

      state%phenState%porosity_id = porosity_id
      state%phenState%decidcap_id = decidcap_id
      state%phenState%albDecTr_id = albDecTr_id
      state%phenState%albEveTr_id = albEveTr_id
      state%phenState%albGrass_id = albGrass_id
      state%phenState%Tmin_id = Tmin_id
      state%phenState%Tmax_id = Tmax_id
      state%phenState%lenDay_id = lenDay_id
      state%phenState%TempVeg = TempVeg
   END SUBROUTINE set_phen_state_scalars

   !> Get phenology state conductance function values
   SUBROUTINE get_phen_state_conductance(state, gfunc, gsc, g_kdown, g_dq, g_ta, g_smd, g_lai)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: gfunc, gsc, g_kdown, g_dq, g_ta, g_smd, g_lai

      gfunc = state%phenState%gfunc
      gsc = state%phenState%gsc
      g_kdown = state%phenState%g_kdown
      g_dq = state%phenState%g_dq
      g_ta = state%phenState%g_ta
      g_smd = state%phenState%g_smd
      g_lai = state%phenState%g_lai
   END SUBROUTINE get_phen_state_conductance

   !> Set phenology state conductance function values
   SUBROUTINE set_phen_state_conductance(state, gfunc, gsc, g_kdown, g_dq, g_ta, g_smd, g_lai)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: gfunc, gsc, g_kdown, g_dq, g_ta, g_smd, g_lai

      state%phenState%gfunc = gfunc
      state%phenState%gsc = gsc
      state%phenState%g_kdown = g_kdown
      state%phenState%g_dq = g_dq
      state%phenState%g_ta = g_ta
      state%phenState%g_smd = g_smd
      state%phenState%g_lai = g_lai
   END SUBROUTINE set_phen_state_conductance

   !> Get phenology state StoreDrainPrm array
   SUBROUTINE get_phen_state_drain(state, StoreDrainPrm)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(OUT) :: StoreDrainPrm

      StoreDrainPrm = state%phenState%StoreDrainPrm
   END SUBROUTINE get_phen_state_drain

   !> Set phenology state StoreDrainPrm array
   SUBROUTINE set_phen_state_drain(state, StoreDrainPrm)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(IN) :: StoreDrainPrm

      state%phenState%StoreDrainPrm = StoreDrainPrm
   END SUBROUTINE set_phen_state_drain

END MODULE module_accessor_phen
