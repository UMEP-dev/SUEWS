! Main module following naming standard: matches filename
MODULE module_phys_radiation3d
   !==============================================================================
   ! 3D MORPHOLOGICAL URBAN RADIATION PARAMETERIZATION
   ! Implementation of Mei et al. (2025) urban canopy radiation transfer scheme
   !
   ! Reference: Mei, S.-J., Chen, G., Wang, K., & Hang, J. (2025).
   !            Parameterizing urban canopy radiation transfer using
   !            three-dimensional urban morphological parameters.
   !            Urban Climate, 60, 102363.
   !
   ! This module implements:
   ! 1. Solar gain parameterizations for ground and canyon surfaces
   ! 2. Sky view factor calculations based on morphological parameters
   ! 3. Directional solar absorptivity with solar angle dependence
   ! 4. Morphology-dependent urban albedo calculations
   !
   ! Key morphological parameters:
   ! - λf (lambda_f): Frontal area density [m^-1]
   ! - λp (lambda_p): Plan area density (building footprint fraction) [-]
   !
   ! Last modified: SJ Mei, LX Wang & TS, Dec 2024
   !==============================================================================

   ! USE PhysConstants, ONLY: eps_fp
   IMPLICIT NONE

   ! Local constants (temporary for standalone compilation)
   REAL(KIND(1D0)), PARAMETER :: eps_fp = 1.0E-12  ! floating-point epsilon

   ! Constants from Mei et al. (2025) paper
   ! ======================================

   ! Ground surface solar gain: S*g = 0.0252 × e^(2.264L)
   ! where L = 1.446 - 0.965λp - 0.259λf
   REAL(KIND(1D0)), PARAMETER :: &
      ground_coeff_a = 0.0252D0, &      ! Coefficient in exponential
      ground_coeff_b = 2.264D0, &       ! Exponent multiplier
      ground_L_const = 1.446D0, &       ! L constant term
      ground_L_lambda_p = -0.965D0, &   ! L λp coefficient
      ground_L_lambda_f = -0.259D0      ! L λf coefficient

   ! Canyon surface solar gain: S*c = 0.011λf - 0.749λp + 0.79
   REAL(KIND(1D0)), PARAMETER :: &
      canyon_lambda_f_coeff = 0.011D0, &   ! λf coefficient
      canyon_lambda_p_coeff = -0.749D0, &  ! λp coefficient
      canyon_constant = 0.79D0              ! constant term

   ! Sky view factor - Ground: Gsky = (-55D + 79)^(-0.714)
   ! where D = 1.428 + 0.62λp - 0.78λf
   REAL(KIND(1D0)), PARAMETER :: &
      sky_ground_coeff = -55D0, &        ! D multiplier
      sky_ground_const = 79D0, &         ! constant in parentheses
      sky_ground_power = -0.714D0, &     ! power exponent
      D_const = 1.428D0, &               ! D constant term
      D_lambda_p = 0.62D0, &             ! D λp coefficient
      D_lambda_f = -0.78D0               ! D λf coefficient

   ! Sky view factor - Canyon: Csky = 0.0351*e^(0.418*Dc)
   ! where Dc = 3.32 - λp - λf
   REAL(KIND(1D0)), PARAMETER :: &
      sky_canyon_coeff = 0.0351D0, &     ! coefficient in exponential
      sky_canyon_exp_coeff = 0.418D0, &  ! exponent multiplier
      Dc_const = 3.32D0, &               ! Dc constant term
      Dc_lambda_p = -1.0D0, &            ! Dc λp coefficient
      Dc_lambda_f = -1.0D0               ! Dc λf coefficient

   ! Morphological parameter bounds (from paper validation range)
   REAL(KIND(1D0)), PARAMETER :: &
      lambda_f_min = 0.16D0, &           ! minimum frontal area density
      lambda_f_max = 2.49D0, &           ! maximum frontal area density
      lambda_p_min = 0.16D0, &           ! minimum plan area density
      lambda_p_max = 0.83D0              ! maximum plan area density

   ! Mathematical constants
   REAL(KIND(1D0)), PARAMETER :: &
      PI = 3.14159265358979323846D0      ! π for angle calculations

CONTAINS

   !==============================================================================
   SUBROUTINE RADIATION_3D_MORPHOLOGY( &
      lambda_f, lambda_p, &                      ! input: morphological parameters
      zenith_rad, azimuth_rad, &                 ! input: solar angles [radians]
      kdown, ldown, &                            ! input: incoming radiation [W/m²]
      surface_albedo_base, surface_emissivity, & ! input: surface properties
      urban_albedo, &                            ! output: morphology-dependent albedo
      solar_gain_ground, solar_gain_canyon, &   ! output: surface-specific solar gain [W/m²]
      sky_view_ground, sky_view_canyon, &       ! output: sky view factors [-]
      absorptivity_ucl, absorptivity_canyon, absorptivity_ground, & ! output: absorptivities [-]
      absorption_ground, absorption_ucl, absorption_total) ! output: solar absorptions [W/m²]

      ! Main subroutine for 3D morphological radiation calculations
      ! Implements the complete Mei et al. (2025) parameterization scheme

      IMPLICIT NONE

      ! Input variables
      REAL(KIND(1D0)), INTENT(in) :: lambda_f              ! frontal area density [m^-1]
      REAL(KIND(1D0)), INTENT(in) :: lambda_p              ! plan area density [-]
      REAL(KIND(1D0)), INTENT(in) :: zenith_rad            ! solar zenith angle [rad]
      REAL(KIND(1D0)), INTENT(in) :: azimuth_rad           ! solar azimuth angle [rad]
      REAL(KIND(1D0)), INTENT(in) :: kdown                 ! incoming shortwave [W/m²]
      REAL(KIND(1D0)), INTENT(in) :: ldown                 ! incoming longwave [W/m²]
      REAL(KIND(1D0)), INTENT(in) :: surface_albedo_base   ! base surface albedo [-]
      REAL(KIND(1D0)), INTENT(in) :: surface_emissivity    ! surface emissivity [-]

      ! Output variables
      REAL(KIND(1D0)), INTENT(out) :: urban_albedo         ! effective urban albedo [-]
      REAL(KIND(1D0)), INTENT(out) :: solar_gain_ground    ! ground surface solar gain [W/m²]
      REAL(KIND(1D0)), INTENT(out) :: solar_gain_canyon    ! canyon surface solar gain [W/m²]
      REAL(KIND(1D0)), INTENT(out) :: sky_view_ground      ! ground sky view factor [-]
      REAL(KIND(1D0)), INTENT(out) :: sky_view_canyon      ! canyon sky view factor [-]
      REAL(KIND(1D0)), INTENT(out) :: absorptivity_ucl     ! UCL absorptivity [-]
      REAL(KIND(1D0)), INTENT(out) :: absorptivity_canyon  ! canyon absorptivity [-]
      REAL(KIND(1D0)), INTENT(out) :: absorptivity_ground  ! ground absorptivity [-]
      REAL(KIND(1D0)), INTENT(out) :: absorption_ground    ! ground solar absorption [W/m²]
      REAL(KIND(1D0)), INTENT(out) :: absorption_ucl       ! UCL solar absorption [W/m²]
      REAL(KIND(1D0)), INTENT(out) :: absorption_total     ! total solar absorption [W/m²]

      ! Local variables
      REAL(KIND(1D0)) :: lambda_f_bounded, lambda_p_bounded
      REAL(KIND(1D0)) :: normalized_solar_gain_ground, normalized_solar_gain_canyon

      ! Validate and bound morphological parameters
      lambda_f_bounded = MAX(lambda_f_min, MIN(lambda_f_max, lambda_f))
      lambda_p_bounded = MAX(lambda_p_min, MIN(lambda_p_max, lambda_p))

      ! Calculate normalized solar gains (daily cycle average)
      CALL CALCULATE_SOLAR_GAIN_MORPHOLOGY( &
         lambda_f_bounded, lambda_p_bounded, &
         normalized_solar_gain_ground, normalized_solar_gain_canyon)

      ! Convert normalized gains to actual solar gains
      solar_gain_ground = normalized_solar_gain_ground * kdown
      solar_gain_canyon = normalized_solar_gain_canyon * kdown

      ! Calculate sky view factors
      CALL CALCULATE_SKY_VIEW_MORPHOLOGY( &
         lambda_f_bounded, lambda_p_bounded, &
         sky_view_ground, sky_view_canyon)

      ! Calculate directional solar absorptivities
      CALL CALCULATE_ABSORPTIVITY_DIRECTIONAL( &
         lambda_f_bounded, lambda_p_bounded, zenith_rad, azimuth_rad, &
         absorptivity_ucl, absorptivity_canyon, absorptivity_ground)

      ! Calculate detailed solar absorptions for ground and UCL surfaces
      CALL CALCULATE_SOLAR_ABSORPTIONS( &
         lambda_f_bounded, lambda_p_bounded, zenith_rad, azimuth_rad, kdown, &
         absorption_ground, absorption_ucl, absorption_total)

      ! Calculate morphology-dependent urban albedo: albedo = 1 - UCL_absorption/kdown
      ! UCL absorption IS the total urban absorption
      IF (kdown > 0.0D0) THEN
         urban_albedo = 1.0D0 - absorption_ucl / kdown
      ELSE
         urban_albedo = surface_albedo_base  ! fallback for no incoming radiation
      END IF

      ! Ensure physical bounds (0 <= albedo <= 1)
      urban_albedo = MAX(0.0D0, MIN(1.0D0, urban_albedo))

   END SUBROUTINE RADIATION_3D_MORPHOLOGY

   !==============================================================================
   SUBROUTINE CALCULATE_SOLAR_GAIN_MORPHOLOGY( &
      lambda_f, lambda_p, &
      solar_gain_ground_norm, solar_gain_canyon_norm)

      ! Calculate normalized solar gains for ground and canyon surfaces
      ! Based on Equations (4) and (5) from Mei et al. (2025)

      IMPLICIT NONE

      ! Input variables
      REAL(KIND(1D0)), INTENT(in) :: lambda_f              ! frontal area density [m^-1]
      REAL(KIND(1D0)), INTENT(in) :: lambda_p              ! plan area density [-]

      ! Output variables
      REAL(KIND(1D0)), INTENT(out) :: solar_gain_ground_norm   ! normalized ground solar gain [-]
      REAL(KIND(1D0)), INTENT(out) :: solar_gain_canyon_norm   ! normalized canyon solar gain [-]

      ! Local variables
      REAL(KIND(1D0)) :: L_ground    ! morphological parameter L for ground

      ! Ground surface solar gain: S*g = 0.0252 × e^(2.264L)
      ! where L = 1.446 - 0.965λp - 0.259λf
      L_ground = ground_L_const + ground_L_lambda_p * lambda_p + ground_L_lambda_f * lambda_f
      solar_gain_ground_norm = ground_coeff_a * EXP(ground_coeff_b * L_ground)

      ! Canyon surface solar gain: S*c = 0.011λf - 0.749λp + 0.79
      solar_gain_canyon_norm = canyon_lambda_f_coeff * lambda_f + &
                              canyon_lambda_p_coeff * lambda_p + &
                              canyon_constant

      ! Ensure physical bounds (0 <= gain <= 1)
      solar_gain_ground_norm = MAX(0.0D0, MIN(1.0D0, solar_gain_ground_norm))
      solar_gain_canyon_norm = MAX(0.0D0, MIN(1.0D0, solar_gain_canyon_norm))

   END SUBROUTINE CALCULATE_SOLAR_GAIN_MORPHOLOGY

   !==============================================================================
   SUBROUTINE CALCULATE_SKY_VIEW_MORPHOLOGY( &
      lambda_f, lambda_p, &
      sky_view_ground, sky_view_canyon)

      ! Calculate sky view factors for ground and canyon surfaces
      ! Based on Equations (18) and (21) from Mei et al. (2025)

      IMPLICIT NONE

      ! Input variables
      REAL(KIND(1D0)), INTENT(in) :: lambda_f              ! frontal area density [m^-1]
      REAL(KIND(1D0)), INTENT(in) :: lambda_p              ! plan area density [-]

      ! Output variables
      REAL(KIND(1D0)), INTENT(out) :: sky_view_ground      ! ground sky view factor [-]
      REAL(KIND(1D0)), INTENT(out) :: sky_view_canyon      ! canyon sky view factor [-]

      ! Local variables
      REAL(KIND(1D0)) :: D_param     ! morphological parameter D for ground
      REAL(KIND(1D0)) :: Dc_param    ! morphological parameter Dc for canyon
      REAL(KIND(1D0)) :: ground_base ! base value for ground calculation

      ! Ground sky view factor: Gsky = (-55D + 79)^(-0.714)
      ! where D = 1.428 + 0.62λp - 0.78λf
      D_param = D_const + D_lambda_p * lambda_p + D_lambda_f * lambda_f
      ground_base = sky_ground_coeff * D_param + sky_ground_const

      ! Ensure positive base value for power calculation
      IF (ground_base > eps_fp) THEN
         sky_view_ground = ground_base**sky_ground_power
      ELSE
         sky_view_ground = 1.0D0  ! Default to full sky view if calculation fails
      END IF

      ! Canyon sky view factor: Csky = 0.0351*e^(0.418*Dc)
      ! where Dc = 3.32 - λp - λf
      Dc_param = Dc_const + Dc_lambda_p * lambda_p + Dc_lambda_f * lambda_f
      sky_view_canyon = sky_canyon_coeff * EXP(sky_canyon_exp_coeff * Dc_param)

      ! Ensure physical bounds (0 <= sky view factor <= 1)
      sky_view_ground = MAX(0.0D0, MIN(1.0D0, sky_view_ground))
      sky_view_canyon = MAX(0.0D0, MIN(1.0D0, sky_view_canyon))

   END SUBROUTINE CALCULATE_SKY_VIEW_MORPHOLOGY

   !==============================================================================
   SUBROUTINE CALCULATE_ABSORPTIVITY_DIRECTIONAL( &
      lambda_f, lambda_p, zenith_rad, azimuth_rad, &
      absorptivity_ucl, absorptivity_canyon, absorptivity_ground)

      ! Calculate directional solar absorptivities based on solar angles
      ! Based on complete Equations (8), (9), and (10) from Mei et al. (2025)
      ! Using exact polynomial coefficients from the paper

      IMPLICIT NONE

      ! Input variables
      REAL(KIND(1D0)), INTENT(in) :: lambda_f              ! frontal area density [m^-1]
      REAL(KIND(1D0)), INTENT(in) :: lambda_p              ! plan area density [-]
      REAL(KIND(1D0)), INTENT(in) :: zenith_rad            ! solar zenith angle [rad]
      REAL(KIND(1D0)), INTENT(in) :: azimuth_rad           ! solar azimuth angle [rad]

      ! Output variables
      REAL(KIND(1D0)), INTENT(out) :: absorptivity_ucl     ! UCL absorptivity [-]
      REAL(KIND(1D0)), INTENT(out) :: absorptivity_canyon  ! canyon absorptivity [-]
      REAL(KIND(1D0)), INTENT(out) :: absorptivity_ground  ! ground absorptivity [-]

      ! Local variables
      REAL(KIND(1D0)) :: phi_eff          ! effective azimuth angle [rad] (Φ in paper)
      REAL(KIND(1D0)) :: theta            ! solar elevation angle [rad] (θ in paper)

      ! Convert zenith to elevation and handle azimuth symmetry
      theta = PI/2.0D0 - zenith_rad

      ! Handle azimuth symmetry: Φ = min(φ, π - φ) for angles outside [0, π/2]
      phi_eff = MIN(azimuth_rad, PI - azimuth_rad)
      IF (phi_eff > PI/2.0D0) phi_eff = PI - phi_eff

      ! UCL directional absorptivity (Eq. 8 from Mei et al. 2025)
      ! αo,u = 0.835 + 0.019Φ + 0.003θ - 0.084λp + 0.138λf + ... (complete polynomial)
      absorptivity_ucl = 0.835D0 + 0.019D0*phi_eff + 0.003D0*theta - 0.084D0*lambda_p + 0.138D0*lambda_f - &
                        0.006D0*phi_eff**2 + 0.011D0*phi_eff*theta - 0.033D0*phi_eff*lambda_p - 0.024D0*phi_eff*lambda_f - &
                        0.051D0*theta**2 + 0.037D0*theta*lambda_p + 0.073D0*theta*lambda_f + 0.188D0*lambda_p**2 - &
                        0.287D0*lambda_p*lambda_f - 0.055D0*lambda_f**2 - 0.003D0*phi_eff**3 - 0.007D0*phi_eff**2*theta + &
                        0.010D0*phi_eff**2*lambda_p + 0.012D0*phi_eff**2*lambda_f - 0.000D0*phi_eff*theta**2 + &
                        0.001D0*phi_eff*theta*lambda_p - 0.000D0*phi_eff*theta*lambda_f + 0.016D0*phi_eff*lambda_p**2 + &
                        0.001D0*phi_eff*lambda_p*lambda_f + 0.001D0*phi_eff*lambda_f**2 + 0.011D0*theta**3 + &
                        0.018D0*theta**2*lambda_p + 0.003D0*theta**2*lambda_f - 0.027D0*theta*lambda_p**2 - &
                        0.061D0*theta*lambda_p*lambda_f - 0.008D0*theta*lambda_f**2 - 0.189D0*lambda_p**3 + &
                        0.150D0*lambda_p**2*lambda_f + 0.061D0*lambda_p*lambda_f**2 + 0.003D0*lambda_f**3

      ! Canyon directional absorptivity (Eq. 9 from Mei et al. 2025)
      ! αo,c = 0.840 - 0.020Φ + 0.003θ - 0.886λp + 0.137λf + ... (complete polynomial)
      absorptivity_canyon = 0.840D0 - 0.020D0*phi_eff + 0.003D0*theta - 0.886D0*lambda_p + 0.137D0*lambda_f - &
                            0.006D0*phi_eff**2 + 0.011D0*phi_eff*theta - 0.033D0*phi_eff*lambda_p - 0.024D0*phi_eff*lambda_f - &
                            0.051D0*theta**2 + 0.036D0*theta*lambda_p + 0.073D0*theta*lambda_f + 0.239D0*lambda_p**2 - &
                            0.286D0*lambda_p*lambda_f - 0.055D0*lambda_f**2 - 0.003D0*phi_eff**3 - 0.007D0*phi_eff**2*theta + &
                            0.010D0*phi_eff**2*lambda_p + 0.012D0*phi_eff**2*lambda_f + 0.000D0*phi_eff*theta**2 + &
                            0.002D0*phi_eff*theta*lambda_p + 0.000D0*phi_eff*theta*lambda_f + 0.016D0*phi_eff*lambda_p**2 + &
                            0.001D0*phi_eff*lambda_p*lambda_f + 0.001D0*phi_eff*lambda_f**2 + 0.011D0*theta**3 + &
                            0.018D0*theta**2*lambda_p + 0.003D0*theta**2*lambda_f - 0.027D0*theta*lambda_p**2 - &
                            0.061D0*theta*lambda_p*lambda_f - 0.008D0*theta*lambda_f**2 - 0.121D0*lambda_p**3 + &
                            0.147D0*lambda_p**2*lambda_f + 0.062D0*lambda_p*lambda_f**2 + 0.003D0*lambda_f**3

      ! Ground directional absorptivity (Eq. 10 from Mei et al. 2025)
      ! αo,g = 0.324 - 0.511Φ + 0.653θ - 0.160λp - 0.566λf + ... (complete polynomial)
      absorptivity_ground = 0.324D0 - 0.511D0*phi_eff + 0.653D0*theta - 0.160D0*lambda_p - 0.566D0*lambda_f + &
                            0.294D0*phi_eff**2 - 0.049D0*phi_eff*theta + 0.363D0*phi_eff*lambda_p + 0.023D0*phi_eff*lambda_f - &
                            0.069D0*theta**2 - 0.479D0*theta*lambda_p - 0.319D0*theta*lambda_f - 0.646D0*lambda_p**2 + &
                            0.881D0*lambda_p*lambda_f + 0.201D0*lambda_f**2 + 0.025D0*phi_eff**3 - 0.040D0*phi_eff**2*theta - &
                            0.327D0*phi_eff**2*lambda_p + 0.004D0*phi_eff**2*lambda_f + 0.082D0*phi_eff*theta**2 - &
                            0.001D0*phi_eff*theta*lambda_p - 0.036D0*phi_eff*theta*lambda_f + 0.188D0*phi_eff*lambda_p**2 - &
                            0.006D0*phi_eff*lambda_p*lambda_f + 0.004D0*phi_eff*lambda_f**2 - 0.009D0*theta**3 - &
                            0.124D0*theta**2*lambda_p + 0.193D0*theta**2*lambda_f + 0.173D0*theta*lambda_p**2 + &
                            0.051D0*theta*lambda_p*lambda_f - 0.013D0*theta*lambda_f**2 + 0.402D0*lambda_p**3 - &
                            0.244D0*lambda_p**2*lambda_f - 0.198D0*lambda_p*lambda_f**2 - 0.006D0*lambda_f**3

      ! Ensure physical bounds (0 <= absorptivity <= 1)
      absorptivity_ucl = MAX(0.0D0, MIN(1.0D0, absorptivity_ucl))
      absorptivity_canyon = MAX(0.0D0, MIN(1.0D0, absorptivity_canyon))
      absorptivity_ground = MAX(0.0D0, MIN(1.0D0, absorptivity_ground))

   END SUBROUTINE CALCULATE_ABSORPTIVITY_DIRECTIONAL

   !==============================================================================
   SUBROUTINE CALCULATE_SOLAR_ABSORPTIONS( &
      lambda_f, lambda_p, zenith_rad, azimuth_rad, kdown, &
      absorption_ground, absorption_ucl, absorption_total)

      ! Calculate solar absorptions for ground and UCL surfaces
      ! Based on the complete radiation balance from Mei et al. (2025)

      IMPLICIT NONE

      ! Input variables
      REAL(KIND(1D0)), INTENT(in) :: lambda_f              ! frontal area density [m^-1]
      REAL(KIND(1D0)), INTENT(in) :: lambda_p              ! plan area density [-]
      REAL(KIND(1D0)), INTENT(in) :: zenith_rad            ! solar zenith angle [rad]
      REAL(KIND(1D0)), INTENT(in) :: azimuth_rad           ! solar azimuth angle [rad]
      REAL(KIND(1D0)), INTENT(in) :: kdown                 ! incoming shortwave [W/m²]

      ! Output variables
      REAL(KIND(1D0)), INTENT(out) :: absorption_ground    ! ground solar absorption [W/m²]
      REAL(KIND(1D0)), INTENT(out) :: absorption_ucl       ! UCL solar absorption [W/m²]
      REAL(KIND(1D0)), INTENT(out) :: absorption_total     ! total solar absorption [W/m²]

      ! Local variables
      REAL(KIND(1D0)) :: absorptivity_ucl, absorptivity_canyon, absorptivity_ground
      REAL(KIND(1D0)) :: sky_view_ground, sky_view_canyon

      ! Calculate directional absorptivities
      CALL CALCULATE_ABSORPTIVITY_DIRECTIONAL( &
         lambda_f, lambda_p, zenith_rad, azimuth_rad, &
         absorptivity_ucl, absorptivity_canyon, absorptivity_ground)

      ! Calculate sky view factors
      CALL CALCULATE_SKY_VIEW_MORPHOLOGY( &
         lambda_f, lambda_p, &
         sky_view_ground, sky_view_canyon)

      ! Calculate ground absorption
      absorption_ground = absorptivity_ground * kdown * sky_view_ground

      ! Calculate UCL (urban canopy layer) absorption
      ! This represents total urban absorption including canyon walls and roofs
      absorption_ucl = absorptivity_ucl * kdown

      ! Total absorption
      absorption_total = absorption_ground + absorption_ucl

   END SUBROUTINE CALCULATE_SOLAR_ABSORPTIONS

END MODULE module_phys_radiation3d
