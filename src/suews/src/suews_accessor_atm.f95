!> @file suews_accessor_atm.f95
!> @brief Accessor functions for atm_state in SUEWS_STATE
!>
!> Provides getter and setter functions to access atmospheric state
!> variables (thermodynamic, vapour, turbulence, diagnostics) via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_atm
   USE module_ctrl_type, ONLY: SUEWS_STATE
   USE module_type_atmosphere, ONLY: atm_state
   USE module_ctrl_const_allocate, ONLY: nsurf
   IMPLICIT NONE

CONTAINS

   !> Get atmospheric state thermodynamic properties
   SUBROUTINE get_atm_state_thermo(state, fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv

      fcld = state%atmState%fcld
      avcp = state%atmState%avcp
      dens_dry = state%atmState%dens_dry
      avdens = state%atmState%avdens
      dq = state%atmState%dq
      lv_J_kg = state%atmState%lv_J_kg
      lvS_J_kg = state%atmState%lvS_J_kg
      tlv = state%atmState%tlv
   END SUBROUTINE get_atm_state_thermo

   !> Set atmospheric state thermodynamic properties
   SUBROUTINE set_atm_state_thermo(state, fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv

      state%atmState%fcld = fcld
      state%atmState%avcp = avcp
      state%atmState%dens_dry = dens_dry
      state%atmState%avdens = avdens
      state%atmState%dq = dq
      state%atmState%lv_J_kg = lv_J_kg
      state%atmState%lvS_J_kg = lvS_J_kg
      state%atmState%tlv = tlv
   END SUBROUTINE set_atm_state_thermo

   !> Get atmospheric state vapour pressure variables
   SUBROUTINE get_atm_state_vapour(state, Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa, &
                                   s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa
      REAL(KIND(1D0)), INTENT(OUT) :: s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa

      Ea_hPa = state%atmState%Ea_hPa
      Es_hPa = state%atmState%Es_hPa
      psyc_hPa = state%atmState%psyc_hPa
      psycIce_hPa = state%atmState%psycIce_hPa
      s_Pa = state%atmState%s_Pa
      s_hpa = state%atmState%s_hpa
      sIce_hpa = state%atmState%sIce_hpa
      vpd_hPa = state%atmState%vpd_hPa
      vpd_pa = state%atmState%vpd_pa
   END SUBROUTINE get_atm_state_vapour

   !> Set atmospheric state vapour pressure variables
   SUBROUTINE set_atm_state_vapour(state, Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa, &
                                   s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa
      REAL(KIND(1D0)), INTENT(IN) :: s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa

      state%atmState%Ea_hPa = Ea_hPa
      state%atmState%Es_hPa = Es_hPa
      state%atmState%psyc_hPa = psyc_hPa
      state%atmState%psycIce_hPa = psycIce_hPa
      state%atmState%s_Pa = s_Pa
      state%atmState%s_hpa = s_hpa
      state%atmState%sIce_hpa = sIce_hpa
      state%atmState%vpd_hPa = vpd_hPa
      state%atmState%vpd_pa = vpd_pa
   END SUBROUTINE set_atm_state_vapour

   !> Get atmospheric state turbulence and stability variables
   SUBROUTINE get_atm_state_turb(state, L_mod, zL, RA_h, RS, UStar, TStar, RB, Tair_av)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: L_mod, zL, RA_h, RS, UStar, TStar, RB, Tair_av

      L_mod = state%atmState%L_mod
      zL = state%atmState%zL
      RA_h = state%atmState%RA_h
      RS = state%atmState%RS
      UStar = state%atmState%UStar
      TStar = state%atmState%TStar
      RB = state%atmState%RB
      Tair_av = state%atmState%Tair_av
   END SUBROUTINE get_atm_state_turb

   !> Set atmospheric state turbulence and stability variables
   SUBROUTINE set_atm_state_turb(state, L_mod, zL, RA_h, RS, UStar, TStar, RB, Tair_av)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: L_mod, zL, RA_h, RS, UStar, TStar, RB, Tair_av

      state%atmState%L_mod = L_mod
      state%atmState%zL = zL
      state%atmState%RA_h = RA_h
      state%atmState%RS = RS
      state%atmState%UStar = UStar
      state%atmState%TStar = TStar
      state%atmState%RB = RB
      state%atmState%Tair_av = Tair_av
   END SUBROUTINE set_atm_state_turb

   !> Get atmospheric state near-surface diagnostics
   SUBROUTINE get_atm_state_diag(state, U10_ms, U_hbh, T2_C, T_hbh_C, q2_gkg, RH2)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: U10_ms, U_hbh, T2_C, T_hbh_C, q2_gkg, RH2

      U10_ms = state%atmState%U10_ms
      U_hbh = state%atmState%U_hbh
      T2_C = state%atmState%T2_C
      T_hbh_C = state%atmState%T_hbh_C
      q2_gkg = state%atmState%q2_gkg
      RH2 = state%atmState%RH2
   END SUBROUTINE get_atm_state_diag

   !> Set atmospheric state near-surface diagnostics
   SUBROUTINE set_atm_state_diag(state, U10_ms, U_hbh, T2_C, T_hbh_C, q2_gkg, RH2)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: U10_ms, U_hbh, T2_C, T_hbh_C, q2_gkg, RH2

      state%atmState%U10_ms = U10_ms
      state%atmState%U_hbh = U_hbh
      state%atmState%T2_C = T2_C
      state%atmState%T_hbh_C = T_hbh_C
      state%atmState%q2_gkg = q2_gkg
      state%atmState%RH2 = RH2
   END SUBROUTINE set_atm_state_diag

   !> Get atmospheric state surface resistance array
   SUBROUTINE get_atm_state_rss_surf(state, rss_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: rss_surf

      rss_surf = state%atmState%rss_surf
   END SUBROUTINE get_atm_state_rss_surf

   !> Set atmospheric state surface resistance array
   SUBROUTINE set_atm_state_rss_surf(state, rss_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: rss_surf

      state%atmState%rss_surf = rss_surf
   END SUBROUTINE set_atm_state_rss_surf

END MODULE module_accessor_atm
