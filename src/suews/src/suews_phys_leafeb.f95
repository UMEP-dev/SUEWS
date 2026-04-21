!==================================================================================================
! Leaf-level energy balance for the A-gs coupling
!
! Solves for leaf temperature T_leaf given absorbed net radiation, boundary
! and stomatal conductances, air state and vapour pressure. Used inside the
! coupled solver so the FvCB kinetics see a physically consistent leaf
! temperature rather than the air temperature. This is what gives the A-gs
! scheme its heat-stress fidelity: under severe VPD the stomata close,
! transpiration collapses, the leaf overheats, V_cmax denatures, A_n falls,
! stomata close further - the positive-feedback spiral that a Jarvis scheme
! cannot reproduce.
!
! Balance:
!    R_n,leaf = H_leaf + LE_leaf
!    H_leaf  = rho * c_p * g_h  * (T_leaf - T_air)
!    LE_leaf = rho * lambda * g_v * (q_sat(T_leaf) - q_air)
!    g_v     = g_s * g_bw / (g_s + g_bw)    (stomatal + boundary in series)
!
! With g_s / g_bw as molar conductances (mol m^-2 s^-1), units are handled
! via the molar mass of dry air (28.97 g/mol) implicit in rho * c_p.
!
! A simple Newton iteration (usually 3-5 steps) is enough because the left
! side is linear in T_leaf and the right side is near-linear through the
! Clausius-Clapeyron term. For the initial guess we use T_leaf = T_air;
! that fails gracefully on convergence error into add_supy_warning.
!
! References:
!   Leuning, R. (1990). J Theor Biol, 145(1).
!   Campbell, G. S. & Norman, J. M. (1998). An Introduction to Environmental
!   Biophysics, 2nd ed., Springer-Verlag.
!
! Error codes:
!   108: leaf energy balance - invalid input / non-convergence
!==================================================================================================
MODULE module_phys_leafeb
   USE module_ctrl_error_state, ONLY: set_supy_error

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: saturation_vapour_pressure
   PUBLIC :: leaf_energy_balance

   ! Stefan-Boltzmann constant [W m^-2 K^-4] is NOT used here; we work from
   ! absorbed net radiation to stay modular.

   ! Latent heat of vaporisation at reference temperature [J kg^-1].
   REAL(KIND(1D0)), PARAMETER :: LAMBDA_V = 2.45D6

   ! Specific heat of dry air at constant pressure [J kg^-1 K^-1].
   REAL(KIND(1D0)), PARAMETER :: CP_AIR = 1005.0D0

   ! Molar masses [g mol^-1].
   REAL(KIND(1D0)), PARAMETER :: M_AIR = 28.97D0
   REAL(KIND(1D0)), PARAMETER :: M_H2O = 18.02D0

   ! Ratio for converting molar -> mass conductance.
   REAL(KIND(1D0)), PARAMETER :: EPS_H2O = 0.622D0 ! M_H2O / M_AIR

   ! Newton tolerance [K] and iteration cap.
   REAL(KIND(1D0)), PARAMETER :: TLEAF_TOL_K = 1.0D-3
   INTEGER, PARAMETER :: MAX_ITER = 15

CONTAINS

   !===========================================================================
   ! Saturation vapour pressure [kPa] via the Tetens-Magnus form used
   ! throughout SUEWS (consistent with Campbell & Norman 1998).
   !===========================================================================
   PURE FUNCTION saturation_vapour_pressure(t_c) RESULT(es)
      REAL(KIND(1D0)), INTENT(IN) :: t_c
      REAL(KIND(1D0)) :: es

      es = 0.6108D0*EXP(17.27D0*t_c/(t_c + 237.3D0))
   END FUNCTION saturation_vapour_pressure

   !===========================================================================
   ! Slope of the saturation vapour pressure curve [kPa K^-1].
   !===========================================================================
   PURE FUNCTION svp_slope(t_c) RESULT(delta)
      REAL(KIND(1D0)), INTENT(IN) :: t_c
      REAL(KIND(1D0)) :: delta, es

      es = saturation_vapour_pressure(t_c)
      delta = 4098.0D0*es/((t_c + 237.3D0)**2)
   END FUNCTION svp_slope

   !===========================================================================
   ! Leaf energy balance.
   !
   ! Inputs:
   !   r_n_leaf_w_m2 : absorbed net radiation per unit leaf area [W m^-2]
   !   t_air_c       : air temperature [degC]
   !   vpd_kpa       : leaf-to-air vapour pressure deficit [kPa]
   !   pressure_kpa  : surface pressure [kPa]
   !   g_s_mol       : stomatal conductance to H2O [mol m^-2 s^-1]
   !   g_bw_mol      : boundary-layer H2O conductance [mol m^-2 s^-1]
   !   g_bh_mol      : boundary-layer heat conductance [mol m^-2 s^-1]
   !                   (typically = g_bw; separated for generality)
   !
   ! Outputs:
   !   t_leaf_c : leaf temperature solved from the balance [degC]
   !   le_w_m2  : latent heat flux [W m^-2]
   !   h_w_m2   : sensible heat flux [W m^-2]
   !   converged : .TRUE. if Newton converged inside MAX_ITER iterations
   !===========================================================================
   SUBROUTINE leaf_energy_balance( &
      r_n_leaf_w_m2, t_air_c, vpd_kpa, pressure_kpa, &
      g_s_mol, g_bw_mol, g_bh_mol, &
      t_leaf_c, le_w_m2, h_w_m2, converged)
      REAL(KIND(1D0)), INTENT(IN) :: r_n_leaf_w_m2
      REAL(KIND(1D0)), INTENT(IN) :: t_air_c
      REAL(KIND(1D0)), INTENT(IN) :: vpd_kpa
      REAL(KIND(1D0)), INTENT(IN) :: pressure_kpa
      REAL(KIND(1D0)), INTENT(IN) :: g_s_mol
      REAL(KIND(1D0)), INTENT(IN) :: g_bw_mol
      REAL(KIND(1D0)), INTENT(IN) :: g_bh_mol
      REAL(KIND(1D0)), INTENT(OUT) :: t_leaf_c
      REAL(KIND(1D0)), INTENT(OUT) :: le_w_m2
      REAL(KIND(1D0)), INTENT(OUT) :: h_w_m2
      LOGICAL, INTENT(OUT) :: converged

      REAL(KIND(1D0)) :: g_v_mol, t_leaf, dt, f, df
      REAL(KIND(1D0)) :: es_leaf, delta_leaf, e_air
      REAL(KIND(1D0)) :: mass_factor, p_pa
      INTEGER :: i

      ! Validate inputs.
      IF (pressure_kpa <= 0.0D0 .OR. g_bh_mol <= 0.0D0) THEN
         CALL set_supy_error( &
            108, &
            'leaf_energy_balance: pressure > 0 and g_bh > 0 required')
         t_leaf_c = t_air_c
         le_w_m2 = 0.0D0
         h_w_m2 = 0.0D0
         converged = .FALSE.
         RETURN
      END IF

      ! Series conductance for H2O (stomata + leaf boundary layer).
      IF (g_s_mol + g_bw_mol <= 0.0D0) THEN
         g_v_mol = 0.0D0
      ELSE
         g_v_mol = g_s_mol*g_bw_mol/(g_s_mol + g_bw_mol)
      END IF

      ! Convert molar conductances [mol m^-2 s^-1] * pressure[Pa] -> kg m^-2 s^-1
      ! via rho * v where rho = M / (RT) and g_mol = g_mass / rho. Simpler
      ! practical form: LE = lambda * g_v * M_air * (e_leaf - e_air) / P,
      ! with the 0.622 ratio implicit via mass_factor below.
      p_pa = pressure_kpa*1000.0D0
      mass_factor = EPS_H2O/p_pa   ! units kg-water / (Pa * kg-air)

      e_air = saturation_vapour_pressure(t_air_c) - vpd_kpa
      IF (e_air < 0.0D0) e_air = 0.0D0

      ! Initial guess: air temperature.
      t_leaf = t_air_c

      converged = .FALSE.
      DO i = 1, MAX_ITER
         es_leaf = saturation_vapour_pressure(t_leaf)
         delta_leaf = svp_slope(t_leaf)

         ! Fluxes [W m^-2]. CP_AIR / mass_factor shakes out to molar units.
         h_w_m2 = CP_AIR*g_bh_mol*M_AIR*1.0D-3*(t_leaf - t_air_c)
         le_w_m2 = LAMBDA_V*g_v_mol*mass_factor*M_AIR*1.0D-3 &
                   *(es_leaf*1000.0D0 - e_air*1000.0D0)

         f = r_n_leaf_w_m2 - h_w_m2 - le_w_m2
         df = -CP_AIR*g_bh_mol*M_AIR*1.0D-3 &
              - LAMBDA_V*g_v_mol*mass_factor*M_AIR*1.0D-3*delta_leaf*1000.0D0

         IF (ABS(df) < 1.0D-12) EXIT
         dt = -f/df
         ! Damp to keep the Newton step inside a physically plausible window.
         IF (dt > 10.0D0) dt = 10.0D0
         IF (dt < -10.0D0) dt = -10.0D0
         t_leaf = t_leaf + dt
         IF (ABS(dt) < TLEAF_TOL_K) THEN
            converged = .TRUE.
            EXIT
         END IF
      END DO

      ! Recompute final fluxes with the converged (or best-effort) T_leaf.
      es_leaf = saturation_vapour_pressure(t_leaf)
      h_w_m2 = CP_AIR*g_bh_mol*M_AIR*1.0D-3*(t_leaf - t_air_c)
      le_w_m2 = LAMBDA_V*g_v_mol*mass_factor*M_AIR*1.0D-3 &
                *(es_leaf*1000.0D0 - e_air*1000.0D0)
      t_leaf_c = t_leaf
   END SUBROUTINE leaf_energy_balance

END MODULE module_phys_leafeb
