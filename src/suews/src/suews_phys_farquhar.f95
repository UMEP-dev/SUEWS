!==================================================================================================
! Farquhar-von Caemmerer-Berry (FvCB) leaf-level biochemistry
!
! Provides net leaf photosynthesis A_n for C3 (Farquhar et al. 1980 with
! Medlyn 2002 peaked temperature response) and C4 (Collatz et al. 1992
! simplified) plants. The module is deliberately standalone and carries no
! time-stepping state: it is called once per timestep per vegetation surface
! by the A-gs coupled solver (module_phys_ags_solver, forthcoming) once the
! solver has an estimate of intercellular CO2 (c_i) and leaf temperature.
!
! Units are SI/mol throughout the biochemistry (mol m^-2 s^-1, ppm = umol/mol,
! J mol^-1); callers are responsible for converting to SUEWS's m m^-1 / W m^-2
! conventions at the module boundary (done by the ags_solver).
!
! References:
!   Farquhar, G. D., von Caemmerer, S., & Berry, J. A. (1980). Planta, 149.
!   Medlyn, B. E. et al. (2002). Plant, Cell and Environment, 25(9).
!   Bernacchi, C. J. et al. (2001). Plant, Cell and Environment, 24(2).
!   Collatz, G. J., Ribas-Carbo, M., & Berry, J. A. (1992). Aust J Plant Phys.
!   Sharkey, T. D. et al. (2007). Plant, Cell and Environment, 30(9).
!
! Error codes (see module_ctrl_error_state header):
!   106: farquhar_c3 / farquhar_c4 - invalid physiological parameters
!
! Main module following naming standard: matches filename.
!==================================================================================================
MODULE module_phys_farquhar
   USE module_ctrl_error_state, ONLY: set_supy_error

   IMPLICIT NONE
   PRIVATE

   ! Public subroutines / functions.
   PUBLIC :: arrhenius_response
   PUBLIC :: peaked_arrhenius_response
   PUBLIC :: farquhar_c3
   PUBLIC :: farquhar_c4

   ! Ideal gas constant [J mol^-1 K^-1].
   REAL(KIND(1D0)), PARAMETER, PUBLIC :: R_GAS = 8.314D0
   ! Reference temperature for the FvCB 25 C parameters [K].
   REAL(KIND(1D0)), PARAMETER, PUBLIC :: T_REF_K = 298.15D0
   ! Ambient O2 partial pressure [umol mol^-1].
   REAL(KIND(1D0)), PARAMETER, PUBLIC :: O_AIR = 210000.0D0

   ! ---------------------------------------------------------------------------
   ! C3 peaked-Arrhenius temperature-response parameters
   ! (Bernacchi et al. 2001 for K_c, K_o, Gamma*; Medlyn 2002 for V_cmax, J_max)
   ! ---------------------------------------------------------------------------
   ! Activation energies [J mol^-1].
   REAL(KIND(1D0)), PARAMETER :: EA_VCMAX = 65330.0D0
   REAL(KIND(1D0)), PARAMETER :: EA_JMAX = 43540.0D0
   REAL(KIND(1D0)), PARAMETER :: EA_KC = 79430.0D0
   REAL(KIND(1D0)), PARAMETER :: EA_KO = 36380.0D0
   REAL(KIND(1D0)), PARAMETER :: EA_GAMMA_STAR = 37830.0D0
   REAL(KIND(1D0)), PARAMETER :: EA_RD = 46390.0D0
   ! High-temperature deactivation enthalpies [J mol^-1].
   REAL(KIND(1D0)), PARAMETER :: HD_VCMAX = 149250.0D0
   REAL(KIND(1D0)), PARAMETER :: HD_JMAX = 152044.0D0
   ! Entropy terms [J mol^-1 K^-1].
   REAL(KIND(1D0)), PARAMETER :: DS_VCMAX = 485.0D0
   REAL(KIND(1D0)), PARAMETER :: DS_JMAX = 495.0D0
   ! Reference values at 25 C [umol mol^-1].
   REAL(KIND(1D0)), PARAMETER :: KC_25 = 404.9D0
   REAL(KIND(1D0)), PARAMETER :: KO_25 = 278400.0D0
   REAL(KIND(1D0)), PARAMETER :: GAMMA_STAR_25 = 42.75D0
   ! Quantum yield (fraction of absorbed PAR used by PSII).
   REAL(KIND(1D0)), PARAMETER :: ALPHA_PSII = 0.3D0
   ! Curvature of the J-vs-I light-response hyperbola.
   REAL(KIND(1D0)), PARAMETER :: THETA_J = 0.7D0

   ! ---------------------------------------------------------------------------
   ! C4 Collatz 1992 simplified parameters
   ! ---------------------------------------------------------------------------
   ! PEP-carboxylase initial slope at 25 C [mol m^-2 s^-1 per ppm].
   REAL(KIND(1D0)), PARAMETER :: KP_25 = 0.7D0
   REAL(KIND(1D0)), PARAMETER :: EA_KP = 46390.0D0
   ! C4 quantum efficiency (light-saturated intrinsic).
   REAL(KIND(1D0)), PARAMETER :: ALPHA_C4 = 0.067D0

CONTAINS

   !===========================================================================
   ! Simple Arrhenius temperature response (no high-T deactivation).
   ! Used for K_c, K_o, Gamma*, R_d.
   !===========================================================================
   PURE FUNCTION arrhenius_response(k25, ea, t_leaf_c) RESULT(k)
      REAL(KIND(1D0)), INTENT(IN) :: k25     ! parameter at 25 C
      REAL(KIND(1D0)), INTENT(IN) :: ea      ! activation energy [J mol^-1]
      REAL(KIND(1D0)), INTENT(IN) :: t_leaf_c ! leaf temperature [degC]
      REAL(KIND(1D0)) :: k
      REAL(KIND(1D0)) :: t_k

      t_k = t_leaf_c + 273.15D0
      k = k25*EXP(ea*(t_k - T_REF_K)/(T_REF_K*R_GAS*t_k))
   END FUNCTION arrhenius_response

   !===========================================================================
   ! Peaked Arrhenius (Medlyn 2002): includes high-temperature deactivation.
   ! Used for V_cmax and J_max. Peak occurs well above growth temperatures;
   ! above the peak, enzyme denaturation causes a rapid fall-off that drives
   ! the heat-stress stomatal-closure signal the A-gs work is meant to capture.
   !===========================================================================
   PURE FUNCTION peaked_arrhenius_response(k25, ea, hd, ds, t_leaf_c) RESULT(k)
      REAL(KIND(1D0)), INTENT(IN) :: k25     ! parameter at 25 C
      REAL(KIND(1D0)), INTENT(IN) :: ea      ! activation energy [J mol^-1]
      REAL(KIND(1D0)), INTENT(IN) :: hd      ! deactivation enthalpy [J mol^-1]
      REAL(KIND(1D0)), INTENT(IN) :: ds      ! entropy term [J mol^-1 K^-1]
      REAL(KIND(1D0)), INTENT(IN) :: t_leaf_c
      REAL(KIND(1D0)) :: k
      REAL(KIND(1D0)) :: t_k, num, den

      t_k = t_leaf_c + 273.15D0
      num = 1.0D0 + EXP((T_REF_K*ds - hd)/(T_REF_K*R_GAS))
      den = 1.0D0 + EXP((t_k*ds - hd)/(t_k*R_GAS))
      k = arrhenius_response(k25, ea, t_leaf_c)*num/den
   END FUNCTION peaked_arrhenius_response

   !===========================================================================
   ! C3 leaf-level net photosynthesis.
   !
   ! Inputs (all mandatory, caller must convert to these units):
   !   par_umol  : absorbed photosynthetically active radiation [umol m^-2 s^-1]
   !   c_i_ppm   : intercellular CO2 mole fraction [umol mol^-1 == ppm]
   !   t_leaf_c  : leaf temperature [degC]
   !   vcmax25   : V_cmax at 25 C [umol m^-2 s^-1]
   !   jmax25    : J_max  at 25 C [umol m^-2 s^-1]
   !   rd25      : R_d    at 25 C [umol m^-2 s^-1]
   !
   ! Outputs:
   !   a_c, a_j : Rubisco- and RuBP-limited gross photosynthesis [umol m^-2 s^-1]
   !   a_n      : net photosynthesis = min(a_c, a_j) - R_d(T)       [umol m^-2 s^-1]
   !
   ! On invalid input the subroutine sets the Python-visible error flag and
   ! returns safe sentinel values so the driver can abort cleanly without
   ! taking the interpreter down.
   !===========================================================================
   SUBROUTINE farquhar_c3( &
      par_umol, c_i_ppm, t_leaf_c, &
      vcmax25, jmax25, rd25, &
      a_c, a_j, a_n)
      REAL(KIND(1D0)), INTENT(IN) :: par_umol
      REAL(KIND(1D0)), INTENT(IN) :: c_i_ppm
      REAL(KIND(1D0)), INTENT(IN) :: t_leaf_c
      REAL(KIND(1D0)), INTENT(IN) :: vcmax25
      REAL(KIND(1D0)), INTENT(IN) :: jmax25
      REAL(KIND(1D0)), INTENT(IN) :: rd25
      REAL(KIND(1D0)), INTENT(OUT) :: a_c
      REAL(KIND(1D0)), INTENT(OUT) :: a_j
      REAL(KIND(1D0)), INTENT(OUT) :: a_n

      REAL(KIND(1D0)) :: vcmax_t, jmax_t, rd_t
      REAL(KIND(1D0)) :: kc_t, ko_t, gamma_star_t
      REAL(KIND(1D0)) :: i_abs, j_t, disc, numer, denom

      IF (vcmax25 <= 0.0D0 .OR. jmax25 <= 0.0D0 .OR. rd25 < 0.0D0) THEN
         CALL set_supy_error( &
            106, 'farquhar_c3: Vcmax25, Jmax25 > 0 and Rd25 >= 0 required')
         a_c = -999.0D0
         a_j = -999.0D0
         a_n = -999.0D0
         RETURN
      END IF

      ! Temperature-scale enzyme kinetics.
      vcmax_t = peaked_arrhenius_response(vcmax25, EA_VCMAX, HD_VCMAX, DS_VCMAX, t_leaf_c)
      jmax_t = peaked_arrhenius_response(jmax25, EA_JMAX, HD_JMAX, DS_JMAX, t_leaf_c)
      rd_t = arrhenius_response(rd25, EA_RD, t_leaf_c)
      kc_t = arrhenius_response(KC_25, EA_KC, t_leaf_c)
      ko_t = arrhenius_response(KO_25, EA_KO, t_leaf_c)
      gamma_star_t = arrhenius_response(GAMMA_STAR_25, EA_GAMMA_STAR, t_leaf_c)

      ! Rubisco-limited gross photosynthesis.
      denom = c_i_ppm + kc_t*(1.0D0 + O_AIR/ko_t)
      IF (denom <= 0.0D0) THEN
         a_c = 0.0D0
      ELSE
         a_c = vcmax_t*(c_i_ppm - gamma_star_t)/denom
         IF (a_c < 0.0D0) a_c = 0.0D0
      END IF

      ! RuBP-regeneration-limited gross photosynthesis.
      i_abs = ALPHA_PSII*MAX(par_umol, 0.0D0)
      disc = (i_abs + jmax_t)**2 - 4.0D0*THETA_J*i_abs*jmax_t
      IF (disc < 0.0D0) disc = 0.0D0
      ! Take the smaller root (positive and bounded by min(I, Jmax)).
      j_t = (i_abs + jmax_t - SQRT(disc))/(2.0D0*THETA_J)

      numer = j_t*(c_i_ppm - gamma_star_t)
      denom = 4.0D0*(c_i_ppm + 2.0D0*gamma_star_t)
      IF (denom <= 0.0D0) THEN
         a_j = 0.0D0
      ELSE
         a_j = numer/denom
         IF (a_j < 0.0D0) a_j = 0.0D0
      END IF

      a_n = MIN(a_c, a_j) - rd_t
   END SUBROUTINE farquhar_c3

   !===========================================================================
   ! C4 leaf-level net photosynthesis (Collatz et al. 1992, simplified).
   !
   ! The CO2-concentrating mechanism makes C4 essentially CO2-saturated at
   ! the Rubisco site, so A_c is simply V_cmax(T). The other two limits
   ! (Collatz's quasi-quantum and PEP-carboxylation) capture low-light and
   ! low-CO2 behaviour respectively.
   !===========================================================================
   SUBROUTINE farquhar_c4( &
      par_umol, c_i_ppm, t_leaf_c, &
      vcmax25, rd25, &
      a_c, a_i, a_p, a_n)
      REAL(KIND(1D0)), INTENT(IN) :: par_umol
      REAL(KIND(1D0)), INTENT(IN) :: c_i_ppm
      REAL(KIND(1D0)), INTENT(IN) :: t_leaf_c
      REAL(KIND(1D0)), INTENT(IN) :: vcmax25
      REAL(KIND(1D0)), INTENT(IN) :: rd25
      REAL(KIND(1D0)), INTENT(OUT) :: a_c
      REAL(KIND(1D0)), INTENT(OUT) :: a_i
      REAL(KIND(1D0)), INTENT(OUT) :: a_p
      REAL(KIND(1D0)), INTENT(OUT) :: a_n

      REAL(KIND(1D0)) :: vcmax_t, rd_t, kp_t

      IF (vcmax25 <= 0.0D0 .OR. rd25 < 0.0D0) THEN
         CALL set_supy_error( &
            106, 'farquhar_c4: Vcmax25 > 0 and Rd25 >= 0 required')
         a_c = -999.0D0
         a_i = -999.0D0
         a_p = -999.0D0
         a_n = -999.0D0
         RETURN
      END IF

      vcmax_t = peaked_arrhenius_response(vcmax25, EA_VCMAX, HD_VCMAX, DS_VCMAX, t_leaf_c)
      rd_t = arrhenius_response(rd25, EA_RD, t_leaf_c)
      kp_t = arrhenius_response(KP_25, EA_KP, t_leaf_c)

      ! Rubisco-limited: saturated by CCM.
      a_c = vcmax_t
      ! Light-limited (quantum).
      a_i = ALPHA_C4*MAX(par_umol, 0.0D0)
      ! PEP-carboxylase / low CO2 limit.
      a_p = kp_t*MAX(c_i_ppm, 0.0D0)

      a_n = MIN(a_c, a_i, a_p) - rd_t
   END SUBROUTINE farquhar_c4

END MODULE module_phys_farquhar
