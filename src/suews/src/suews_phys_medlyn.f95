!==================================================================================================
! Medlyn optimal stomatal conductance
!
! Evaluates g_s from the closed-form Cowan-Farquhar (1977) / Medlyn et al.
! (2011) optimisation that maximises carbon gain per unit water loss:
!
!    g_s = g_0 + 1.6 * (1 + g_1 / sqrt(VPD)) * A_n / c_s
!
! where g_s has units of mol H2O m^-2 s^-1, g_0 [mol m^-2 s^-1] is the
! residual conductance, g_1 [kPa^0.5] is the PFT-level slope, VPD is the
! leaf-to-air vapour pressure deficit in kPa, A_n is net photosynthesis in
! umol m^-2 s^-1, and c_s is the CO2 mole fraction at the leaf surface in
! umol mol^-1 (ppm).
!
! The module is deliberately standalone: it takes A_n as input rather than
! solving the coupled A-gs system. The coupled iteration lives in
! module_phys_ags_solver.
!
! References:
!   Cowan, I. R., & Farquhar, G. D. (1977). Symp Soc Exp Biol, 31.
!   Medlyn, B. E. et al. (2011). Global Change Biology, 17(6).
!   Lin, Y.-S. et al. (2015). Nature Climate Change, 5(5).
!
! Error codes:
!   107: medlyn - invalid g_1, g_0, or c_s
!==================================================================================================
MODULE module_phys_medlyn
   USE module_ctrl_error_state, ONLY: set_supy_error

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: medlyn_gs
   PUBLIC :: ci_from_gs

   ! Ratio of H2O to CO2 diffusivity through stomata. Appears in the
   ! 1.6 coefficient of the Medlyn form.
   REAL(KIND(1D0)), PARAMETER, PUBLIC :: H2O_CO2_DIFF_RATIO = 1.6D0

   ! Lower bound on VPD passed into the sqrt(VPD) term to prevent blow-up
   ! at near-saturated conditions. Protects numerics at dew-point crossings.
   REAL(KIND(1D0)), PARAMETER :: VPD_MIN_KPA = 0.05D0

CONTAINS

   !===========================================================================
   ! Medlyn g_s.
   !
   ! Inputs:
   !   a_n_umol : net photosynthesis [umol m^-2 s^-1]
   !   c_s_ppm  : CO2 mole fraction at leaf surface [ppm]
   !   vpd_kpa  : leaf-to-air vapour pressure deficit [kPa]
   !   g_0      : residual stomatal conductance [mol H2O m^-2 s^-1]
   !   g_1      : Medlyn slope parameter [kPa^0.5]
   !
   ! Output:
   !   g_s_mol  : stomatal conductance to H2O [mol m^-2 s^-1]
   !
   ! If A_n <= 0 the optimisation collapses and g_s = g_0. This is the
   ! heat-stress / low-light floor that the plan calls out: once the
   ! photosynthesis drops below respiration (or below zero), the plant
   ! closes stomata down to the residual conductance.
   !===========================================================================
   SUBROUTINE medlyn_gs( &
      a_n_umol, c_s_ppm, vpd_kpa, g_0, g_1, &
      g_s_mol)
      REAL(KIND(1D0)), INTENT(IN) :: a_n_umol
      REAL(KIND(1D0)), INTENT(IN) :: c_s_ppm
      REAL(KIND(1D0)), INTENT(IN) :: vpd_kpa
      REAL(KIND(1D0)), INTENT(IN) :: g_0
      REAL(KIND(1D0)), INTENT(IN) :: g_1
      REAL(KIND(1D0)), INTENT(OUT) :: g_s_mol

      REAL(KIND(1D0)) :: vpd_eff, a_over_cs, extra_term

      IF (g_0 < 0.0D0 .OR. g_1 < 0.0D0 .OR. c_s_ppm <= 0.0D0) THEN
         CALL set_supy_error( &
            107, &
            'medlyn_gs: g_0 >= 0, g_1 >= 0, c_s > 0 required')
         g_s_mol = -999.0D0
         RETURN
      END IF

      vpd_eff = MAX(vpd_kpa, VPD_MIN_KPA)

      IF (a_n_umol <= 0.0D0) THEN
         g_s_mol = g_0
         RETURN
      END IF

      a_over_cs = a_n_umol/c_s_ppm
      extra_term = H2O_CO2_DIFF_RATIO*(1.0D0 + g_1/SQRT(vpd_eff))*a_over_cs
      g_s_mol = g_0 + extra_term
   END SUBROUTINE medlyn_gs

   !===========================================================================
   ! Invert the Fickian diffusion equation to recover c_i from known fluxes.
   !
   ! Uses A_n = g_s_c * (c_s - c_i) where g_s_c is the CO2 conductance:
   !    g_s_c = g_s_mol / 1.6
   ! so c_i = c_s - 1.6 * A_n / g_s_mol
   !
   ! Returns a safe clamped c_i in [0.05*c_s, 0.99*c_s] to prevent the
   ! Newton solver diverging at pathological states.
   !===========================================================================
   SUBROUTINE ci_from_gs( &
      a_n_umol, c_s_ppm, g_s_mol, &
      c_i_ppm)
      REAL(KIND(1D0)), INTENT(IN) :: a_n_umol
      REAL(KIND(1D0)), INTENT(IN) :: c_s_ppm
      REAL(KIND(1D0)), INTENT(IN) :: g_s_mol
      REAL(KIND(1D0)), INTENT(OUT) :: c_i_ppm

      REAL(KIND(1D0)) :: c_i_raw, c_i_lo, c_i_hi

      IF (g_s_mol <= 0.0D0 .OR. c_s_ppm <= 0.0D0) THEN
         c_i_ppm = 0.7D0*c_s_ppm
         RETURN
      END IF

      c_i_raw = c_s_ppm - H2O_CO2_DIFF_RATIO*a_n_umol/g_s_mol
      c_i_lo = 0.05D0*c_s_ppm
      c_i_hi = 0.99D0*c_s_ppm
      c_i_ppm = MAX(c_i_lo, MIN(c_i_hi, c_i_raw))
   END SUBROUTINE ci_from_gs

END MODULE module_phys_medlyn
