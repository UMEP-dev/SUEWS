!==================================================================================================
! Coupled A-gs solver
!
! Orchestrates the simultaneous solve of Farquhar-von Caemmerer-Berry leaf
! biochemistry, Medlyn optimal stomatal conductance, and leaf energy balance
! for a single leaf at a single timestep. The three sub-problems are coupled
! through (A_n, c_i, T_leaf): each one depends on the other two.
!
! Iteration scheme (fixed-point on c_i with T_leaf nested inside):
!   initial: c_i = 0.7 * c_s (C3) or 0.4 * c_s (C4); T_leaf = T_air.
!   repeat (up to MAX_ITER_OUTER times):
!     1. call farquhar_c3 / farquhar_c4 (c_i, T_leaf) -> A_n
!     2. call medlyn_gs(A_n, c_s, VPD) -> g_s_mol
!     3. call leaf_energy_balance(R_n, T_air, VPD, P, g_s, g_bw, g_bh) -> T_leaf_new
!     4. c_i_new = c_s - 1.6 * A_n / g_s_mol, clamped
!     5. damp (under-relaxation 0.5) to help weak-convergence cases
!     6. check convergence: |c_i_new - c_i| < C_I_TOL and |T_leaf_new - T_leaf| < TLEAF_TOL
!
! On non-convergence the solver logs via add_supy_warning (the routine is
! registered in module_ctrl_error_state) and returns the best-effort state.
! The calling driver is free to either propagate the warning or fall back
! to the JARVIS path for that timestep.
!
! References match the underlying modules; see module_phys_farquhar,
! module_phys_medlyn, module_phys_leafeb.
!
! Error codes:
!   109: ags_solver - c_a <= 0 or boundary conductance <= 0
!==================================================================================================
MODULE module_phys_ags_solver
   USE module_ctrl_error_state, ONLY: set_supy_error, add_supy_warning
   USE module_phys_farquhar, ONLY: farquhar_c3, farquhar_c4
   USE module_phys_medlyn, ONLY: medlyn_gs, ci_from_gs, H2O_CO2_DIFF_RATIO
   USE module_phys_leafeb, ONLY: leaf_energy_balance

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: ags_solve_leaf

   ! Convergence tolerances.
   REAL(KIND(1D0)), PARAMETER :: C_I_TOL_PPM = 0.1D0
   REAL(KIND(1D0)), PARAMETER :: TLEAF_TOL_K = 0.01D0
   ! Newton-like under-relaxation factor for c_i updates.
   REAL(KIND(1D0)), PARAMETER :: RELAX_CI = 0.5D0
   INTEGER, PARAMETER :: MAX_ITER_OUTER = 15

CONTAINS

   !===========================================================================
   ! Solve the coupled A-gs problem for a single leaf.
   !
   ! Inputs:
   !   c3c4_flag    : 1 = C3, 2 = C4
   !   par_umol     : absorbed PAR [umol photon m^-2 s^-1]
   !   c_s_ppm      : CO2 at leaf surface [ppm] (take atmospheric c_a for now)
   !   t_air_c      : air temperature [degC]
   !   vpd_kpa      : leaf-to-air VPD [kPa]
   !   pressure_kpa : surface pressure [kPa]
   !   r_n_leaf     : absorbed net radiation per unit leaf area [W m^-2]
   !   g_bw_mol     : boundary-layer H2O conductance [mol m^-2 s^-1]
   !   g_bh_mol     : boundary-layer heat conductance [mol m^-2 s^-1]
   !   vcmax25      : Vcmax at 25 C [umol m^-2 s^-1]
   !   jmax25       : Jmax  at 25 C [umol m^-2 s^-1]
   !   rd25         : Rd    at 25 C [umol m^-2 s^-1]
   !   g_0          : Medlyn residual conductance [mol H2O m^-2 s^-1]
   !   g_1          : Medlyn slope [kPa^0.5]
   !
   ! Outputs:
   !   a_n_out      : net photosynthesis [umol m^-2 s^-1]
   !   g_s_out      : stomatal conductance to H2O [mol m^-2 s^-1]
   !   c_i_out      : intercellular CO2 [ppm]
   !   t_leaf_out   : leaf temperature [degC]
   !   iwue_out     : intrinsic WUE = A / g_s_CO2 [umol CO2 / mol H2O]
   !   converged    : .TRUE. if the joint iteration converged
   !   iter_count   : iterations taken
   !===========================================================================
   SUBROUTINE ags_solve_leaf( &
      c3c4_flag, &
      par_umol, c_s_ppm, t_air_c, vpd_kpa, pressure_kpa, &
      r_n_leaf, g_bw_mol, g_bh_mol, &
      vcmax25, jmax25, rd25, g_0, g_1, &
      a_n_out, g_s_out, c_i_out, t_leaf_out, iwue_out, &
      converged, iter_count)
      INTEGER, INTENT(IN) :: c3c4_flag
      REAL(KIND(1D0)), INTENT(IN) :: par_umol
      REAL(KIND(1D0)), INTENT(IN) :: c_s_ppm
      REAL(KIND(1D0)), INTENT(IN) :: t_air_c
      REAL(KIND(1D0)), INTENT(IN) :: vpd_kpa
      REAL(KIND(1D0)), INTENT(IN) :: pressure_kpa
      REAL(KIND(1D0)), INTENT(IN) :: r_n_leaf
      REAL(KIND(1D0)), INTENT(IN) :: g_bw_mol
      REAL(KIND(1D0)), INTENT(IN) :: g_bh_mol
      REAL(KIND(1D0)), INTENT(IN) :: vcmax25
      REAL(KIND(1D0)), INTENT(IN) :: jmax25
      REAL(KIND(1D0)), INTENT(IN) :: rd25
      REAL(KIND(1D0)), INTENT(IN) :: g_0
      REAL(KIND(1D0)), INTENT(IN) :: g_1
      REAL(KIND(1D0)), INTENT(OUT) :: a_n_out
      REAL(KIND(1D0)), INTENT(OUT) :: g_s_out
      REAL(KIND(1D0)), INTENT(OUT) :: c_i_out
      REAL(KIND(1D0)), INTENT(OUT) :: t_leaf_out
      REAL(KIND(1D0)), INTENT(OUT) :: iwue_out
      LOGICAL, INTENT(OUT) :: converged
      INTEGER, INTENT(OUT) :: iter_count

      REAL(KIND(1D0)) :: c_i, c_i_new, t_leaf, t_leaf_new
      REAL(KIND(1D0)) :: a_n, g_s_mol
      REAL(KIND(1D0)) :: a_c, a_j, a_p, a_i4
      REAL(KIND(1D0)) :: h_leaf, le_leaf, le_w_m2, h_w_m2
      LOGICAL :: leaf_eb_converged
      INTEGER :: i

      ! Defensive parameter checks.
      IF (c_s_ppm <= 0.0D0 .OR. g_bh_mol <= 0.0D0 .OR. g_bw_mol <= 0.0D0) THEN
         CALL set_supy_error( &
            109, &
            'ags_solve_leaf: c_s > 0, g_bw > 0, g_bh > 0 required')
         a_n_out = -999.0D0
         g_s_out = -999.0D0
         c_i_out = -999.0D0
         t_leaf_out = -999.0D0
         iwue_out = -999.0D0
         converged = .FALSE.
         iter_count = 0
         RETURN
      END IF

      ! Initial state.
      IF (c3c4_flag == 2) THEN
         c_i = 0.4D0*c_s_ppm
      ELSE
         c_i = 0.7D0*c_s_ppm
      END IF
      t_leaf = t_air_c
      a_n = 0.0D0
      g_s_mol = g_0
      converged = .FALSE.

      DO i = 1, MAX_ITER_OUTER
         ! 1. Leaf biochemistry at current (c_i, T_leaf).
         IF (c3c4_flag == 2) THEN
            CALL farquhar_c4( &
               par_umol, c_i, t_leaf, vcmax25, rd25, &
               a_c, a_i4, a_p, a_n)
         ELSE
            CALL farquhar_c3( &
               par_umol, c_i, t_leaf, vcmax25, jmax25, rd25, &
               a_c, a_j, a_n)
         END IF

         ! 2. Stomatal conductance from Medlyn.
         CALL medlyn_gs( &
            a_n, c_s_ppm, vpd_kpa, g_0, g_1, g_s_mol)

         ! 3. Leaf energy balance -> T_leaf_new.
         CALL leaf_energy_balance( &
            r_n_leaf, t_air_c, vpd_kpa, pressure_kpa, &
            g_s_mol, g_bw_mol, g_bh_mol, &
            t_leaf_new, le_w_m2, h_w_m2, leaf_eb_converged)
         h_leaf = h_w_m2
         le_leaf = le_w_m2

         ! 4. c_i update (Fickian inversion).
         CALL ci_from_gs(a_n, c_s_ppm, g_s_mol, c_i_new)

         ! 5. Under-relax the c_i update to avoid oscillation.
         c_i_new = RELAX_CI*c_i_new + (1.0D0 - RELAX_CI)*c_i

         ! 6. Convergence.
         iter_count = i
         IF (ABS(c_i_new - c_i) < C_I_TOL_PPM &
             .AND. ABS(t_leaf_new - t_leaf) < TLEAF_TOL_K) THEN
            c_i = c_i_new
            t_leaf = t_leaf_new
            converged = .TRUE.
            EXIT
         END IF

         c_i = c_i_new
         t_leaf = t_leaf_new
      END DO

      IF (.NOT. converged) THEN
         CALL add_supy_warning( &
            'ags_solve_leaf: coupled iteration did not converge within 15 iterations; &
            &returning best-effort state')
      END IF

      ! Pack outputs.
      a_n_out = a_n
      g_s_out = g_s_mol
      c_i_out = c_i
      t_leaf_out = t_leaf
      ! Intrinsic WUE = A / g_s,CO2 where g_s,CO2 = g_s_mol / 1.6.
      IF (g_s_mol > 0.0D0) THEN
         iwue_out = a_n*H2O_CO2_DIFF_RATIO/g_s_mol
      ELSE
         iwue_out = 0.0D0
      END IF
   END SUBROUTINE ags_solve_leaf

END MODULE module_phys_ags_solver
