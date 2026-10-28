! Main module following naming standard: matches filename
MODULE module_phys_atmmoiststab
   USE module_ctrl_type, ONLY: atm_state, SUEWS_FORCING, SUEWS_TIMER, SUEWS_STATE
   USE module_ctrl_const_physconst, ONLY: eps_fp
   USE module_ctrl_error, ONLY: ErrorHint
   USE module_ctrl_error_state, ONLY: set_supy_error
   IMPLICIT NONE
   REAL(KIND(1D0)), PARAMETER :: neut_limit = 1.E-4 !Limit for neutral stability
   REAL(KIND(1D0)), PARAMETER :: k = 0.4 !Von Karman's contant
   REAL(KIND(1D0)), PARAMETER :: grav = 9.80665 !g - gravity - physics today august 1987
   ! Under-relaxation of (z-d)/L between outer surface-temperature passes
   REAL(KIND(1D0)), PARAMETER :: ZL_RELAX = 0.3D0 ! factor for the first passes [-]
   INTEGER, PARAMETER :: ZL_RELAX_PASSES = 10 ! passes at the full factor [-]
   REAL(KIND(1D0)), PARAMETER :: ZL_RELAX_DECAY = 0.7D0 ! per-pass decay after that [-]

   ! (z-d)/L solve in cal_Stab (#1962)
   REAL(KIND(1D0)), PARAMETER :: ZL_STABLE_MAX = 0.5D0 ! stable clip [-]
   REAL(KIND(1D0)), PARAMETER :: ZL_UNSTABLE_MIN = -2.0D0 ! unstable clip [-]
   REAL(KIND(1D0)), PARAMETER :: ZL_SCAN_MIN = 1.0D-9 ! smallest |(z-d)/L| scanned [-]
   INTEGER, PARAMETER :: N_ZL_SCAN = 60 ! geometric scan points up to the clip
   REAL(KIND(1D0)), PARAMETER :: ZL_SOLVE_TOL = 1.0D-12 ! residual and bracket tolerance [-]
   INTEGER, PARAMETER :: MAX_ZL_SOLVE = 200 ! false-position iterations

   ! scheme code
   INTEGER, PARAMETER :: W16 = 2 ! Ward et al (2016) based on {unstable: Dyer (1974) modified Hogstrom (1988); and stable: Van Ulden & Holtslag (1985)}
   INTEGER, PARAMETER :: K75 = 3 ! Kondo (1975) adopted by Campbell & Norman eqn 7.26 p 97
   INTEGER, PARAMETER :: B71 = 4 ! Businger et al (1971) modifed  Hogstrom (1988)
   INTEGER, PARAMETER :: J12 = 5 ! Cheng and Brutsaert (2005)
CONTAINS
   PURE FUNCTION relax_zl(zl_prev, zl_new, i_pass) RESULT(zl_relaxed)
      ! Under-relaxed (z-d)/L for the next outer surface-temperature pass.
      ! Fed straight back, the flux -> (z-d)/L -> RA -> QE -> residual QH map can
      ! two-cycle near QH = 0 (one pass unstable, the next on the stable clip),
      ! most often on near-neutral nights. Moving only part of the way towards
      ! the new solve damps that cycle; a fixed point of the plain map is still
      ! a fixed point here.
      ! Near the stable fold the solve itself jumps (from a small (z-d)/L to the
      ! clip as the downward flux grows), and there may be no consistent value
      ! to converge to: a fixed factor then keeps stepping across the jump. After
      ! ZL_RELAX_PASSES passes the factor therefore decays geometrically, so the
      ! iterate settles at the jump instead of cycling to the pass limit.
      REAL(KIND(1D0)), INTENT(IN) :: zl_prev ! (z-d)/L used in the previous pass [-]
      REAL(KIND(1D0)), INTENT(IN) :: zl_new ! (z-d)/L from this pass's solve [-]
      INTEGER, INTENT(IN) :: i_pass ! outer pass number within the timestep [-]
      REAL(KIND(1D0)) :: zl_relaxed
      REAL(KIND(1D0)) :: alpha

      alpha = ZL_RELAX
      IF (i_pass > ZL_RELAX_PASSES) alpha = ZL_RELAX*ZL_RELAX_DECAY**(i_pass - ZL_RELAX_PASSES)
      zl_relaxed = zl_prev + alpha*(zl_new - zl_prev)
   END FUNCTION relax_zl


   SUBROUTINE SUEWS_update_atmState(timer, forcing, modState)
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState
      ! TYPE(atm_state), INTENT(inout) :: atmState
      ASSOCIATE (atmState => modState%atmState)

         ASSOCIATE ( &
            Temp_C => forcing%Temp_C, &
            pres => forcing%pres, &
            RH => forcing%RH, &
            dectime => timer%dectime, &
            dt_since_start => timer%dt_since_start, &
            tstep => timer%tstep, &
            Tair_av => atmState%Tair_av, &
            lv_J_kg => atmState%lv_J_kg, &
            lvS_J_kg => atmState%lv_s_j_kg, &
            es_hPa => atmState%es_hPa, &
            Ea_hPa => atmState%Ea_hPa, &
            VPd_hpa => atmState%VPd_hpa, &
            VPD_Pa => atmState%VPD_Pa, &
            dq => atmState%dq, &
            dens_dry => atmState%dens_dry, &
            avcp => atmState%av_cp, &
            avdens => atmState%av_density &
            )
            CALL cal_AtmMoist( &
               Temp_C, pres, RH, dectime, &
               lv_J_kg, lvS_J_kg, &
               es_hPa, Ea_hPa, VPd_hpa, VPD_Pa, dq, dens_dry, avcp, avdens)
            Tair_av = update_tair_av(Tair_av, dt_since_start, tstep, temp_c)

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_update_atmState

   FUNCTION update_tair_av(tair_av_prev, dt_since_start, tstep, temp_c) RESULT(tair_av_next)
      ! calculate mean air temperature of past 24 hours
      ! TS, 17 Sep 2019
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(in) :: tair_av_prev
      REAL(KIND(1D0)), INTENT(in) :: temp_c
      INTEGER, INTENT(in) :: dt_since_start
      INTEGER, INTENT(in) :: tstep

      REAL(KIND(1D0)) :: tair_av_next

      REAL(KIND(1D0)), PARAMETER :: len_day_s = 24.*3600. ! day length in seconds
      REAL(KIND(1D0)) :: len_cal_s ! length of average period in seconds
      REAL(KIND(1D0)) :: temp_k ! temp in K

      ! determine the average period
      IF (dt_since_start > len_day_s) THEN
         ! if simulation has been running over one day
         len_cal_s = len_day_s
      ELSE
         ! if simulation has been running less than one day
         len_cal_s = dt_since_start + tstep
      END IF
      temp_k = temp_c + 273.15
      tair_av_next = tair_av_prev*(len_cal_s - tstep*1.)/len_cal_s + temp_k*tstep/len_cal_s

   END FUNCTION update_tair_av

   !.c!! For Lumps Version 2 - no stability calculations
   ! Latent heat of sublimation when air temperature below zero added. LJ Nov 2012
   ! explict interface added to all subroutines, TS 08 Aug 2017
   SUBROUTINE cal_AtmMoist( &
      Temp_C, Press_hPa, avRh, dectime, & ! input:
      lv_J_kg, lvS_J_kg, & ! output:
      es_hPa, Ea_hPa, VPd_hpa, VPD_Pa, dq, dens_dry, avcp, air_dens, &
      modState) ! optional: for thread-safe error logging

      USE meteo, ONLY: &
         sat_vap_press_x, spec_heat_beer, &
         lat_vap, lat_vapSublim, spec_hum_def

      IMPLICIT NONE
      REAL(KIND(1D0)) :: vap_dens

      REAL(KIND(1D0)), INTENT(in) :: &
         Temp_C, &
         Press_hPa, &
         avRh, dectime
      REAL(KIND(1D0)), INTENT(out) :: &
         lv_J_kg, & !Latent heat of vaporization in [J kg-1]
         lvS_J_kg, & !Latent heat of sublimation in J/kg
         es_hPa, & !Saturation vapour pressure over water in hPa
         Ea_hPa, & !Vapour pressure of water in hPa
         VPd_hpa, & !vapour pressure deficit in hPa
         VPD_Pa, & !vapour pressure deficit in Pa
         dq, & !Specific humidity deficit in g/kg
         dens_dry, & !Vap density or absolute humidity [kg m-3]
         avcp, & !specific heat capacity in J kg-1 K-1
         air_dens !Air density in kg/m3
      TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState

      REAL(KIND(1D0)), PARAMETER :: &
         !  comp          = 0.9995, &
         !  epsil         = 0.62197,&           !ratio molecular weight of water vapor/dry air (kg/mol/kg/mol)
         !  epsil_gkg     = 621.97, &           !ratio molecular weight of water vapor/dry air in g/kg
         !  dry_gas       = 8.31451,&           !Dry gas constant (J/k/mol)
         !  gas_ct_wat    = 461.05,&            !Gas constant for water (J/kg/K)
         !  molar         = 0.028965,&          !Dry air molar fraction in kg/mol
         !  molar_wat_vap = 0.0180153,&         !Molar fraction of water vapor in kg/mol
         gas_ct_dry = 8.31451/0.028965, & !j/kg/k=dry_gas/molar
         gas_ct_wv = 8.31451/0.0180153 !j/kg/k=dry_gas/molar_wat_vap
      !  waterDens     = 999.8395            !Density of water in 0 cel deg
      INTEGER, PARAMETER :: from = 1

      !Saturation vapour pressure over water in hPa
      es_hPa = sat_vap_press_x(Temp_C, Press_hPa, from, dectime) ! dectime is more or less unnecessary here

      !Vapour pressure of water in hPa
      Ea_hPa = avRh/100*es_hPa

      ! if(debug.and.dectime>55.13.and.dectime<55.2)write(35,*)'%',Temp_C

      VPd_hpa = es_hPa - ea_hpa !vapour pressure deficit in hPa
      VPD_Pa = (es_hPa*100) - (Ea_hPa*100) !vapour pressure deficit in Pa

      dq = spec_hum_def(vpd_hPa, Press_hPa) !Specific humidity deficit in g/kg

      !Vap density or absolute humidity         (kg/m3)
      vap_dens = (Ea_hPa*100/((Temp_C + 273.15)*gas_ct_wv))

      !density Dry Air Beer(1990)        kg/m3
      dens_dry = ((Press_hPa - Ea_hPa)*100)/(gas_ct_dry*(273.15 + Temp_C))

      !Air density in kg/m3
      air_dens = (Press_hPa*100)/(gas_ct_dry*(Temp_C + 273.15))

      !Calculate specific heat capacity in J kg-1 K-1
      avcp = spec_heat_beer(Temp_C, avRh, vap_dens, dens_dry)

      !Latent heat of vaporization in [J kg-1]
      lv_J_kg = lat_vap(Temp_C, Ea_hPa, Press_hPa, avcp, dectime)

      !Latent heat of sublimation in J/kg
      IF (Temp_C < 0.000) THEN
         lvS_J_kg = lat_vapSublim(Temp_C, Ea_hPa, Press_hPa, avcp)
      ELSE
         lvS_J_kg = 2834000
      END IF
      !if(debug)write(*,*)lv_J_kg,Temp_C,'lv2'
      IF (press_hPa < 900) THEN
         CALL ErrorHint(46, 'Function LUMPS_cal_AtmMoist', press_hPa, -55.55D0, -55, modState)
      END IF
      RETURN
   END SUBROUTINE cal_AtmMoist

   !.c!! For Lumps Version 2 - no stability calculations
   !==========================================================
   !     Last change:
   !     TS   08 Aug 2017: added explicit interface
   !     TS   13 Jun 2017: corrections to the integral of stability functions
   !     MH   12 Apr 2017: Stable limit to exit do-loop
   !     LJ   25 Nov 2014: Limits for L
   !     LJ   19 Feb 2010
   !     SG   27 Mar 2000    4:44 pm
   !     ust - friction velocity
   !     L - monin obukhov stability length
   !       Van Ulden & Holtslag (1985) JCAM: 24: 1196-1207

   SUBROUTINE cal_Stab( &
      StabilityMethod, & ! input
      zzd, & !Active measurement height (meas. height-displac. height)
      z0m, & !Aerodynamic roughness length
      zdm, & !Displacement height
      avU1, & !Average wind speed
      Temp_C, & !Air temperature
      QH_init, & !sensible heat flux [W m-2]
      avdens, & ! air density
      avcp, & ! heat capacity of air
      L_MOD, & !Obukhov length! output:
      TStar, & !T*
      UStar, & !Friction velocity
      zL, & !Stability scale
      modState) ! optional: for thread-safe error logging

      USE module_ctrl_error_state, ONLY: supy_error_flag
      IMPLICIT NONE
      INTEGER, INTENT(in) :: StabilityMethod

      REAL(KIND(1D0)), INTENT(in) :: zzd !Active measurement height (meas. height-displac. height)
      REAL(KIND(1D0)), INTENT(in) :: z0m !Aerodynamic roughness length
      REAL(KIND(1D0)), INTENT(in) :: zdm !Displacement height
      REAL(KIND(1D0)), INTENT(in) :: avU1 !Average wind speed
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !Air temperature
      REAL(KIND(1D0)), INTENT(in) :: QH_init !sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: avdens ! air density [kg m-3]
      REAL(KIND(1D0)), INTENT(in) :: avcp ! volumetric heat capacity [J m-3 K-1]

      REAL(KIND(1D0)), INTENT(out) :: L_MOD !Obukhov length
      REAL(KIND(1D0)), INTENT(out) :: TStar !T*
      REAL(KIND(1D0)), INTENT(out) :: UStar !Friction velocity
      REAL(KIND(1D0)), INTENT(out) :: zL ! Stability scale
      TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState
      ! REAL(KIND(1d0)),INTENT(out)::psim   !Stability function of momentum

      REAL(KIND(1D0)) :: G_T_K, &
                         KUZ, &
                         psim, &
                         z0l, &
                         psimz0, &
                         H_init, &
                         h
      INTEGER, PARAMETER :: notUsedI = -55

      LOGICAL, PARAMETER :: debug = .FALSE.

      !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity
      H_init = QH_init/(avdens*avcp)

      IF (debug) WRITE (*, *) StabilityMethod, z0m, avU1, H_init, UStar, L_MOD
      G_T_K = (Grav/(Temp_C + 273.15))*k !gravity constant/(Temperature*Von Karman Constant)
      KUZ = k*AvU1 !Von Karman constant*mean wind speed
      IF (zzd < 0) THEN
         CALL ErrorHint(32, &
                        'Windspeed Ht too low relative to zdm [Stability calc]- values [z-zdm, zdm]', &
                        Zzd, zdm, notUsedI, modState)
         IF (supy_error_flag()) RETURN
      END IF

      IF (LOG(zzd/z0m) < 0.001000) THEN
         ! PRINT*, 1/(z0m-z0m)
         CALL ErrorHint(17, 'In stability subroutine, (z-zd) < z0.', zzd, z0m, notUsedI, modState)
         IF (supy_error_flag()) RETURN
      END IF

      H = H_init
      IF (ABS(H) <= eps_fp) THEN
         ! no buoyancy flux: neutral
         TStar = 0.
         L_MOD = 0.
         zL = 0.
         UStar = MAX(1.0D-12, KUZ/LOG(Zzd/z0m))
      ELSE
         ! (z-d)/L has the sign of -H: stable for downward H, unstable for upward
         IF (H < 0) THEN
            zL = solve_zl_branch(1.0D0, ZL_STABLE_MAX)
         ELSE
            zL = -solve_zl_branch(-1.0D0, -ZL_UNSTABLE_MIN)
         END IF
         ! state consistent with the returned (z-d)/L
         L_MOD = zzd/zL
         z0L = z0m/L_MOD
         psim = stab_psi_mom(StabilityMethod, zL)
         psimz0 = stab_psi_mom(StabilityMethod, z0L)
         UStar = KUZ/(LOG(Zzd/z0m) - psim + psimz0) !Friction velocity in non-neutral situation
         ! Numerical division guard only; (z-d)/L supplies the physical stability bound.
         UStar = MAX(1.0D-12, UStar)
         TStar = (-H/UStar)
      END IF

      ! limit zL to be within [-2,2]
!       zL = MIN(2., MAX(-2., zL))
      ! limit other output variables as well as z/L
      ! L_MOD = zzd/zL
      ! limit L_mod to 3.e4 for consistency with output
      ! IF (ABS(L_MOD) > 3E4) L_MOD = L_MOD/ABS(L_MOD)*3E4
      ! z0L = z0m/L_MOD
      ! psim = stab_psi_mom(StabilityMethod, zL)
      ! psimz0 = stab_psi_mom(StabilityMethod, z0L)

      ! TS 01 Aug 2018:
      ! TS: limit UStar and TStar to reasonable values
      ! to prevent potential issues in other stability-related calcualtions
      ! 02 Aug 2018: set a low limit at 0.15 m/s (Schumann 1988, BLM, DOI: 10.1007/BF00123019)
      ! UStar = KUZ/(LOG(Zzd/z0m) - psim + psimz0)
      ! UStar = MAX(0.15, UStar)
      ! TStar = (-H/UStar)

      ! IF (UStar < 0.0001) THEN       !If u* still too small after iteration, then force quit simulation and write out error info
      !    ! UStar=KUZ/(LOG(Zzd/z0m))
      !    PRINT *, 'UStar', UStar, KUZ, (LOG(Zzd/z0m)), Zzd, z0m
      !    CALL ErrorHint(30, 'SUBROUTINE cal_Stab:[ u*< 0.0001] zl,L_MOD', zl, L_MOD, notUsedI)
      !    CALL ErrorHint(30, 'SUBROUTINE cal_Stab:[ u*< 0.0001] z0l,UStar', z0l, UStar, notUsedI)
      !    CALL ErrorHint(30, 'SUBROUTINE cal_Stab:[ u*< 0.0001] psim,psimz0', psim, psimz0, notUsedI)
      !    CALL ErrorHint(30, 'SUBROUTINE cal_Stab:[ u*< 0.0001] AVU1,log(zzd/z0m)', AVU1, LOG(zzd/z0m), notUsedI)

      !    ! RETURN
      ! ENDIF

      ! ! update associated variables
      ! zL = zzd/L_MOD

      ! if ( L_MOD<-990 ) then
      !   print*, 'L_MOD too low',L_MOD
      !   print*, 1/(L_MOD-L_MOD)
      !
      ! end if

   CONTAINS

      FUNCTION zl_residual(sgn, mag) RESULT(g)
         ! g = |(z-d)/L implied by u*(zL)| - |zL| at zL = sgn*mag  [-]
         ! A root of g is a (z-d)/L consistent with H; g > 0 means the
         ! implied stability is larger than the trial value.
         REAL(KIND(1D0)), INTENT(IN) :: sgn, mag
         REAL(KIND(1D0)) :: g
         REAL(KIND(1D0)) :: z, lm, us, pm, pm0

         z = sgn*mag
         lm = zzd/z
         pm = stab_psi_mom(StabilityMethod, z)
         pm0 = stab_psi_mom(StabilityMethod, z0m/lm)
         us = MAX(1.0D-12, KUZ/(LOG(Zzd/z0m) - pm + pm0))
         ! implied (z-d)/L = zzd*G_T_K*TStar/u*^2 with TStar = -H/u*
         g = sgn*(-zzd*G_T_K*H/us**3) - mag
      END FUNCTION zl_residual

      FUNCTION solve_zl_branch(sgn, mag_max) RESULT(mag)
         ! Magnitude of (z-d)/L on one stability branch, in (0, mag_max].
         ! Takes the root nearest neutral (the branch continuous with
         ! neutral); if the residual stays positive up to the clip, there is
         ! no consistent value inside the clip and the clip is returned.
         ! Replaces a fixed-point loop that stopped on a small step, which
         ! near the stable fold stopped short of its solution (#1962).
         REAL(KIND(1D0)), INTENT(IN) :: sgn, mag_max
         REAL(KIND(1D0)) :: mag
         REAL(KIND(1D0)) :: a, b, ga, gb, c, gc
         INTEGER :: j, side

         ! scan outward from neutral for the first sign change
         a = ZL_SCAN_MIN
         ga = zl_residual(sgn, a)
         IF (ga <= 0.0D0) THEN
            mag = a ! consistent value indistinguishable from neutral
            RETURN
         END IF
         b = a
         gb = ga
         DO j = 1, N_ZL_SCAN
            a = b
            ga = gb
            b = ZL_SCAN_MIN*(mag_max/ZL_SCAN_MIN)**(REAL(j, KIND(1D0))/N_ZL_SCAN)
            IF (j == N_ZL_SCAN) b = mag_max
            gb = zl_residual(sgn, b)
            IF (gb <= 0.0D0) EXIT
         END DO
         IF (gb > 0.0D0) THEN
            mag = mag_max ! no root inside the clip
            RETURN
         END IF

         ! Illinois false position on [a, b], ga > 0 >= gb
         side = 0
         mag = b
         DO j = 1, MAX_ZL_SOLVE
            c = (a*gb - b*ga)/(gb - ga)
            gc = zl_residual(sgn, c)
            mag = c
            IF (ABS(gc) <= ZL_SOLVE_TOL .OR. b - a <= ZL_SOLVE_TOL) EXIT
            IF (gc > 0.0D0) THEN
               a = c
               ga = gc
               IF (side == 1) gb = 0.5D0*gb
               side = 1
            ELSE
               b = c
               gb = gc
               IF (side == -1) ga = 0.5D0*ga
               side = -1
            END IF
         END DO
      END FUNCTION solve_zl_branch

   END SUBROUTINE cal_Stab

   !==================================================================
   ! TS 20210208: restructure stability corrections funtions;
   !  - the previous implementations are kept down below for cross-validation

   ! integral form for momentum
   FUNCTION stab_psi_mom(StabilityMethod, ZL) RESULT(psim)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: zl, psim
      INTEGER :: StabilityMethod
      CHARACTER(LEN=160) :: message

      SELECT CASE (StabilityMethod)
      CASE (W16)
         psim = psi_mom_W16(ZL)

      CASE (J12)
         psim = psi_mom_J12(ZL)

      CASE (K75)
         psim = psi_mom_K75(ZL)

      CASE (B71)
         psim = psi_mom_B71(ZL)

      CASE DEFAULT
         WRITE (message, '(A,I0,A)') 'stab_psi_mom: stability=', StabilityMethod, &
            ' is reserved or unsupported; use 3 (recommended), 2 or 4.'
         CALL set_supy_error(107, TRIM(message))
         ! Neutral value keeps callers finite until the driver reports the error.
         psim = 0.0D0
         RETURN
      END SELECT

   END FUNCTION

   ! integral form for heat
   FUNCTION stab_psi_heat(StabilityMethod, ZL) RESULT(psih)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: zl, psih
      INTEGER :: StabilityMethod
      CHARACTER(LEN=160) :: message

      SELECT CASE (StabilityMethod)
      CASE (W16)
         psih = psi_heat_W16(ZL)

      CASE (J12)
         psih = psi_heat_J12(ZL)

      CASE (K75)
         psih = psi_heat_K75(ZL)

      CASE (B71)
         psih = psi_heat_B71(ZL)

      CASE DEFAULT
         WRITE (message, '(A,I0,A)') 'stab_psi_heat: stability=', StabilityMethod, &
            ' is reserved or unsupported; use 3 (recommended), 2 or 4.'
         CALL set_supy_error(107, TRIM(message))
         ! Neutral value keeps callers finite until the driver reports the error.
         psih = 0.0D0
         RETURN
      END SELECT

   END FUNCTION

   ! correction function for momentum
   FUNCTION stab_phi_mom(StabilityMethod, ZL) RESULT(phim)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: zl, phim
      INTEGER :: StabilityMethod
      CHARACTER(LEN=160) :: message

      SELECT CASE (StabilityMethod)
      CASE (W16)
         ! W16 only has psi forms; its RSL phi formulation needs review (GH#1784).
         CALL set_supy_error(108, &
            'stab_phi_mom: stability=2 (W16) has no phi implementation for RSL; ' // &
            'use stability=3 or 4, or roughness_sublayer=0 (MOST).')
         phim = 1.0D0
         RETURN

      CASE (J12)
         phim = phi_mom_J12(zl)

      CASE (K75)
         phim = phi_mom_K75(zl)

      CASE (B71)
         phim = phi_mom_B71(zl)

      CASE DEFAULT
         WRITE (message, '(A,I0,A)') 'stab_phi_mom: stability=', StabilityMethod, &
            ' is reserved or unsupported; use 3 (recommended) or 4.'
         CALL set_supy_error(107, TRIM(message))
         ! Neutral value keeps callers finite until the driver reports the error.
         phim = 1.0D0
         RETURN
      END SELECT

   END FUNCTION

   ! correction function for heat
   FUNCTION stab_phi_heat(StabilityMethod, ZL) RESULT(phih)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: zl, phih
      INTEGER :: StabilityMethod
      CHARACTER(LEN=160) :: message

      SELECT CASE (StabilityMethod)
      CASE (W16)
         ! W16 only has psi forms; its RSL phi formulation needs review (GH#1784).
         CALL set_supy_error(108, &
            'stab_phi_heat: stability=2 (W16) has no phi implementation for RSL; ' // &
            'use stability=3 or 4, or roughness_sublayer=0 (MOST).')
         phih = 1.0D0
         RETURN

      CASE (J12)
         phih = phi_heat_J12(zl)

      CASE (K75)
         phih = phi_heat_K75(zl)

      CASE (B71)
         phih = phi_heat_B71(zl)

      CASE DEFAULT
         WRITE (message, '(A,I0,A)') 'stab_phi_heat: stability=', StabilityMethod, &
            ' is reserved or unsupported; use 3 (recommended) or 4.'
         CALL set_supy_error(107, TRIM(message))
         ! Neutral value keeps callers finite until the driver reports the error.
         phih = 1.0D0
         RETURN
      END SELECT

   END FUNCTION

   ! ------------------------------------
   ! Jimenez et al. (2012) forms for WRF
   ! https://doi.org/10.1175/mwr-d-11-00056.1
   FUNCTION psi_mom_J12(ZL) RESULT(psim)
      ! Jimenez et al. (2012), eqn 17 and 18
      REAL(KIND(1D0)), PARAMETER :: a = 6.1
      REAL(KIND(1D0)), PARAMETER :: b = 2.5
      REAL(KIND(1D0)) :: zl, psim

      IF (ABS(zL) <= neut_limit) THEN
         psim = 0
      ELSEIF (zL < -neut_limit) THEN
         ! Jimenez et al. (2012), eqn 17
         psim = psi_mom_G00(zL)
      ELSEIF (zL > neut_limit) THEN
         ! Jimenez et al. (2012), eqn 18:
         ! Cheng and Brutsaert (2005)
         psim = psi_mom_CB05(zL)
      END IF

   END FUNCTION

   FUNCTION phi_mom_J12(ZL) RESULT(phim)
      ! not directly provided by J12
      ! equations below are derived from related original formulations
      REAL(KIND(1D0)) :: zl, phim

      IF (ABS(zL) <= neut_limit) THEN
         phim = 1
      ELSEIF (zL < -neut_limit) THEN
         ! derivative of eqn 17 in Jimenez et al. (2012)
         phim = phi_mom_G00(zL)
      ELSEIF (zL > neut_limit) THEN
         ! derivative of eqn 18 in Jimenez et al. (2012)
         phim = phi_mom_CB05(zL)
      END IF

   END FUNCTION

   FUNCTION psi_heat_J12(ZL) RESULT(psih)
      ! Jimenez et al. (2012), eqn 17 and 19
      REAL(KIND(1D0)) :: zl, psih

      IF (ABS(zL) <= neut_limit) THEN
         psih = 0
      ELSEIF (zL < -neut_limit) THEN
         ! Jimenez et al. (2012), eqn 17
         psih = psi_heat_G00(ZL)
      ELSEIF (zL > neut_limit) THEN
         ! Jimenez et al. (2012), eqn 19:
         ! Cheng and Brutsaert (2005)
         psih = psi_heat_CB05(ZL)
      END IF

   END FUNCTION

   FUNCTION phi_heat_J12(ZL) RESULT(phih)
      REAL(KIND(1D0)) :: zl, phih

      IF (ABS(zL) <= neut_limit) THEN
         phih = 1
      ELSEIF (zL < -neut_limit) THEN
         phih = phi_heat_G00(ZL)
      ELSEIF (zL > neut_limit) THEN
         phih = phi_heat_CB05(ZL)
      END IF

   END FUNCTION
   ! ------------------------------------

   ! ------------------------------------
   ! Grachev et al. (2000) forms
   ! https://doi.org/10.1023/a:1002452529672
   ! !ONLY DEFINED FOR UNSTABLE CONDITIONS!
   ! used in J12 under unstable conditions
   FUNCTION psi_mom_G00(ZL) RESULT(psim)
      ! Grachev et al. (2000), eqn 18
      ! determination of `am` see sect. 4.1 of G00
      REAL(KIND(1D0)), PARAMETER :: am = 10
      REAL(KIND(1D0)) :: zl, psim
      REAL(KIND(1D0)) :: psim_k
      REAL(KIND(1D0)) :: psim_c

      IF (ABS(zL) <= neut_limit) THEN
         psim = 0
      ELSEIF (zL < -neut_limit) THEN
         ! Kansas part:
         psim_k = psi_mom_B71(ZL)
         ! convective contribution:
         psim_c = psi_conv(ZL, am)
         ! zL weighted sum:
         psim = DOT_PRODUCT([psim_k, psim_c], [1D0, zL**2])
         psim = psim/SUM([1D0, zL**2])
      END IF

   END FUNCTION

   FUNCTION psi_heat_G00(ZL) RESULT(psih)
      ! Grachev et al. (2000), eqn 18
      ! determination of `ah` see sect. 4.1 of G00
      REAL(KIND(1D0)), PARAMETER :: ah = 34

      REAL(KIND(1D0)) :: zl, psih
      REAL(KIND(1D0)) :: psih_k
      REAL(KIND(1D0)) :: psih_c

      IF (ABS(zL) <= neut_limit) THEN
         psih = 0
      ELSEIF (zL < -neut_limit) THEN
         ! Kansas part:
         psih_k = psi_heat_B71(ZL)
         ! convective contribution:
         psih_c = psi_conv(ZL, ah)
         ! zL weighted sum:
         psih = DOT_PRODUCT([psih_k, psih_c], [1D0, zL**2])
         psih = psih/SUM([1D0, zL**2])

      END IF

   END FUNCTION

   ! NB: the formulation of phi(zL) below is "correct" by definition:
   ! phi=1-zL*d(Psi)/d(zL)
   ! which however shows strange numeric behaviour:
   ! a jumpy transition from phih_K to phih_C and dives into a negative range at some point
   ! thus it's NOT adopted here
   ! instead the following formulations phi_m/phi_h similar to the weighted psi_hm are used:

   FUNCTION phi_mom_G00(ZL) RESULT(phim)
      REAL(KIND(1D0)), PARAMETER :: am = 10
      REAL(KIND(1D0)) :: zl, phim
      REAL(KIND(1D0)) :: phim_K ! Kansas part
      ! REAL(KIND(1D0)):: psim_K ! Kansas part
      REAL(KIND(1D0)) :: phim_C ! convective part
      ! REAL(KIND(1D0)):: psim_C ! convective part

      IF (ABS(zL) <= neut_limit) THEN
         phim = 1
      ELSEIF (zL < -neut_limit) THEN
         ! kansas
         ! psim_K = psi_mom_B71(ZL)
         phim_K = phi_mom_B71(ZL)

         ! convective
         ! psim_C = psi_conv(ZL, am)
         phim_C = phi_conv(ZL, am)

         ! by definition correct but not used due to bizzare numeric behaviour
         ! phim = 1 - zL*dPsi_dzL_G00(zL, psim_K, phim_K, psim_C, phim_C)

         ! weighted sum:
         phim = DOT_PRODUCT([phim_k, phim_c], [1D0, zL**2])
         phim = phim/SUM([1D0, zL**2])

      END IF

   END FUNCTION

   FUNCTION phi_heat_G00(ZL) RESULT(phih)
      REAL(KIND(1D0)), PARAMETER :: ah = 34
      REAL(KIND(1D0)) :: zl, phih
      REAL(KIND(1D0)) :: phih_K ! Kansas part
      ! REAL(KIND(1D0)):: psih_K ! Kansas part
      REAL(KIND(1D0)) :: phih_C ! convective part
      ! REAL(KIND(1D0)):: psih_C ! convective part

      IF (ABS(zL) <= neut_limit) THEN
         phih = 1
      ELSEIF (zL < -neut_limit) THEN
         ! kansas
         ! psih_K = psi_heat_B71(ZL)
         phih_K = phi_heat_B71(ZL)

         ! convective
         ! psih_C = psi_conv(ZL, ah)
         phih_C = phi_conv(ZL, ah)

         phih = DOT_PRODUCT([phih_k, phih_c], [1D0, zL**2])
         phih = phih/SUM([1D0, zL**2])

      END IF

   END FUNCTION

   FUNCTION psi_conv(ZL, ax) RESULT(psiC)
      ! Grachev et al. (2000), eqn 12
      REAL(KIND(1D0)) :: zl, psiC, y, ax
      REAL(KIND(1D0)), PARAMETER :: pi = ACOS(-1.)

      y = (1 - ax*zl)**(1/3.)
      psiC = 3./2*LOG(y**2 + y + 1/3.) - SQRT(3.)*ATAN(2*y + 1/SQRT(3.)) + pi/SQRT(3.)

   END FUNCTION

   FUNCTION phi_conv(ZL, ax) RESULT(phiC)
      ! Grachev et al. (2000), eqn 15
      REAL(KIND(1D0)) :: zl, phiC, ax, dPsi_dzL
      REAL(KIND(1D0)), PARAMETER :: pi = ACOS(-1.)

      ! d(psi_conv)/d(zL)
      dPsi_dzL = (9 + 4*(-3 + SQRT(3.))*ax*zL - 9*(1 - ax*zL)**(1/3.))/ &
                 (6.*zL*(2 - 2*ax*zL + (1 - ax*zL)**(2/3.)))

      ! phi=1-zL*d(Psi)/d(zL)
      phiC = 1 - zl*dPsi_dzL

   END FUNCTION

   FUNCTION dPsi_dzL_G00(ZL, psiK, phiK, psiC, phiC) RESULT(dPsi)
      ! Grachev et al. (2000), eqn 15
      ! note: Psi is a zL-weighted sum of Kansas and convective components
      ! derived from d(Psi)/d(zL)=d((psiK+zL**2*psiC)/(1+zL**2))/d(zL)

      REAL(KIND(1D0)) :: zl, dPsi, psiC, phiC, psiK, phiK
      REAL(KIND(1D0)) :: x_zl, x_psiC, x_phiC, x_psiK, x_phiK

      ! Kansas part
      x_psiK = -(2*psiK*zL)/(1 + zL**2)**2
      x_phiK = -phiK/(zL*(1 + zL**2))

      ! convective contribution
      x_psiC = (2*psiC*zL)/(1 + zL**2)**2
      x_phiC = -(phiC*zL)/(1 + zL**2)

      ! zL related
      x_zl = 1/zL

      ! totoal
      dPsi = x_psiC + x_psiK + x_phiK + x_phiC + x_zl

   END FUNCTION

   ! ------------------------------------

   ! ------------------------------------
   ! Cheng and Brutsaert (2005) forms
   ! https://doi.org/10.1007/s10546-004-1425-4
   ! !ONLY DEFINED FOR STABLE CONDITIONS!
   FUNCTION psi_CB05(ZL, k1, k2) RESULT(psi)
      ! Cheng and Brutsaert (2005), eqn 21/23
      REAL(KIND(1D0)) :: zl, psi, k1, k2
      psi = -k1*LOG(zl + (1 + zl**k2)**(1/k2))
   END FUNCTION

   FUNCTION psi_mom_CB05(ZL) RESULT(psim)
      ! Cheng and Brutsaert (2005), eqn 21
      REAL(KIND(1D0)), PARAMETER :: a = 6.1
      REAL(KIND(1D0)), PARAMETER :: b = 2.5
      REAL(KIND(1D0)) :: zl, psim

      IF (ABS(zL) <= neut_limit) THEN
         psim = 0
      ELSEIF (zL > neut_limit) THEN
         psim = psi_CB05(ZL, a, b)
      END IF

   END FUNCTION

   FUNCTION psi_heat_CB05(ZL) RESULT(psih)
      ! Cheng and Brutsaert (2005), eqn 23
      REAL(KIND(1D0)), PARAMETER :: c = 5.3
      REAL(KIND(1D0)), PARAMETER :: d = 1.1
      REAL(KIND(1D0)) :: zl, psih

      IF (ABS(zL) <= neut_limit) THEN
         psih = 0
      ELSEIF (zL > neut_limit) THEN
         psih = psi_CB05(ZL, c, d)
      END IF

   END FUNCTION

   FUNCTION phi_CB05(ZL, k1, k2) RESULT(phi)
      ! Cheng and Brutsaert (2005), eqn 22/24
      REAL(KIND(1D0)) :: zl, phi, k1, k2, zlk2
      zlk2 = zl**k2
      phi = 1 + k1*( &
            (zl + zlk2*(1 + zlk2)**((1 - k2)/k2)) &
            /(zl + (1 + zlk2)**(1/k2)) &
            )
   END FUNCTION

   FUNCTION phi_mom_CB05(ZL) RESULT(phim)
      ! Cheng and Brutsaert (2005), eqn 22
      REAL(KIND(1D0)), PARAMETER :: a = 6.1
      REAL(KIND(1D0)), PARAMETER :: b = 2.5
      REAL(KIND(1D0)) :: zl, phim

      IF (ABS(zL) <= neut_limit) THEN
         phim = 1
      ELSEIF (zL > neut_limit) THEN
         phim = phi_CB05(ZL, a, b)
      END IF

   END FUNCTION

   FUNCTION phi_heat_CB05(ZL) RESULT(phih)
      ! Cheng and Brutsaert (2005), eqn 24
      REAL(KIND(1D0)), PARAMETER :: c = 5.3
      REAL(KIND(1D0)), PARAMETER :: d = 1.1
      REAL(KIND(1D0)) :: zl, phih, zld

      IF (ABS(zL) <= neut_limit) THEN
         phih = 1
      ELSEIF (zL > neut_limit) THEN
         zld = zl**d
         phih = phi_CB05(ZL, c, d)
      END IF

   END FUNCTION
   ! ------------------------------------

   ! ------------------------------------
   ! Kondo (1975) forms
   ! https://doi.org/10.1007/bf00232256
   FUNCTION phi_mom_K75(ZL) RESULT(phim)
      ! Kondo (1975) adopted by Campbell & Norman eqn 7.22 and 7.23 p 97
      REAL(KIND(1D0)) :: zl, phim

      IF (ABS(zL) <= neut_limit) THEN
         phim = 1
      ELSEIF (zL < -neut_limit) THEN
         phim = 1/(1 - 16*zl)**.25
      ELSEIF (zL > neut_limit) THEN
         phim = 1 + 6*zl/(1 + zL)
      END IF

   END FUNCTION

   FUNCTION phi_heat_K75(ZL) RESULT(phih)
      ! Kondo (1975) adopted by Campbell & Norman eqn 7.22 and 7.23 p 97
      REAL(KIND(1D0)) :: zl, phih

      IF (ABS(zL) <= neut_limit) THEN
         phih = 1
      ELSEIF (zL < -neut_limit) THEN
         phih = phi_mom_K75(zl)**2
      ELSEIF (zL > neut_limit) THEN
         phih = phi_mom_K75(ZL)
      END IF

   END FUNCTION

   FUNCTION psi_mom_K75(ZL) RESULT(psim)
      ! Kondo (1975) adopted by Campbell & Norman eqn 7.26 and 7.27 p 97
      REAL(KIND(1D0)) :: zl, psim

      IF (ABS(zL) <= neut_limit) THEN
         psim = 0
      ELSEIF (zL < -neut_limit) THEN
         psim = 0.6*psi_heat_K75(ZL)
      ELSEIF (zL > neut_limit) THEN
         psim = psi_heat_K75(ZL)
      END IF

   END FUNCTION

   FUNCTION psi_heat_K75(ZL) RESULT(psih)
      ! Kondo (1975) adopted by Campbell & Norman eqn 7.26 and 7.27 p 97
      REAL(KIND(1D0)) :: zl, psih

      IF (ABS(zL) <= neut_limit) THEN
         psih = 0
      ELSEIF (zL < -neut_limit) THEN
         psih = (2)*LOG((1 + (1 - 16*zl)**0.5)/2)
      ELSEIF (zL > neut_limit) THEN
         psih = (-6)*LOG(1 + zl)
      END IF

   END FUNCTION
   ! ------------------------------------

   ! ------------------------------------
   ! Businger et al. (1971) modified by Hogstrom (1988)
   ! DOI: https://doi.org/10.1007/bf00119875
   FUNCTION phi_mom_B71(ZL) RESULT(phim)
      ! Businger et al. (1971)
      ! modified by Hogstrom (1988): see Table VI
      REAL(KIND(1D0)) :: zl, phim

      IF (ABS(zL) <= neut_limit) THEN
         phim = 1
      ELSEIF (zL < -neut_limit) THEN
         phim = (1.-19.3*zl)**(-0.25)
      ELSEIF (zL > neut_limit) THEN
         phim = 1 + 6*zl
      END IF

   END FUNCTION

   FUNCTION phi_heat_B71(ZL) RESULT(phih)
      ! Businger et al. (1971)
      ! modified by Hogstrom (1988): see Table VII
      REAL(KIND(1D0)) :: zl, phih

      IF (ABS(zL) <= neut_limit) THEN
         phih = 1
      ELSEIF (zL < -neut_limit) THEN
         phih = 0.95*(1.-11.6*zl)**(-0.5)
      ELSEIF (zL > neut_limit) THEN
         phih = 0.95 + 7.8*zl
      END IF

   END FUNCTION

   FUNCTION psi_mom_B71(ZL) RESULT(psim)
      ! integral form of phi_mom_B71
      REAL(KIND(1D0)), PARAMETER :: PIOVER2 = ACOS(-1.)/2.
      REAL(KIND(1D0)) :: zl, psim, x, x2

      IF (ABS(zL) <= neut_limit) THEN
         psim = 0
      ELSEIF (zL < -neut_limit) THEN
         x = (1 - 19.3*zl)**(0.25)
         X2 = LOG((1 + (X**2.))/2.)
         psim = (2.*LOG((1 + X)/2.)) + X2 - (2.*ATAN(X)) + PIOVER2

      ELSEIF (zL > neut_limit) THEN
         psim = (-6)*zl
      END IF

   END FUNCTION

   FUNCTION psi_heat_B71(ZL) RESULT(psih)
      ! integral form of phi_heat_B71
      REAL(KIND(1D0)) :: zl, psih, x

      IF (ABS(zL) <= neut_limit) THEN
         psih = 0
      ELSEIF (zL < -neut_limit) THEN
         x = 0.95*(1.-11.6*zl)**(0.5)
         psih = 2*LOG((1 + x)/2)
      ELSEIF (zL > neut_limit) THEN
         IF (zl < 1) THEN
            psih = (-7.8)*zl
         ELSE
            psih = (-7.8)*(1 + LOG(zl))
         END IF
      END IF

   END FUNCTION

   ! ------------------------------------
   ! Based on stab_fn_mom for StabilityMethod == 2 use W16 case (Ward et al. 2016, Urban Climate)
   FUNCTION psi_mom_W16(ZL) RESULT(psym)
      !     PSYM - stability FUNCTION for momentum
      !Modified by LJ Mar 2010
      !Input: Stability (z-d)/L
      ! Note: Unstable part corresponds to Dyer (1974) mod. Hogstrom (1988)
      ! Note: Stable part corresponds to Van Ulden & Holtslag (1985) p 1206 in original code

      ! use mod_z ! Original dependency - removed as not needed in isolated function
      ! use mod_k ! Original dependency - removed as not needed in isolated function

      IMPLICIT NONE

      ! REAL (KIND(1d0)), PARAMETER :: neut_limit = 1.E-4 ! Limit for neutral stability (Assumed from context)
      REAL(KIND(1D0)) :: piover2, psym, zl, x, x2

      PIOVER2 = ACOS(-1.)/2.
      !PRINT*,StabilityMethod,zl,"stab_fn_mom:" ! Original print statement commented out
      IF (ABS(zL) <= neut_limit) THEN
         psym = 0
      ELSEIF (zL < -neut_limit) THEN !Unstable
         !Dyer (1974)(1-16z/L)**.25' k=0.41  mod. Hogstrom (1988)v15.2
         X = (1.-(15.2*zl))**0.25
         X2 = LOG((1 + (X**2.))/2.)
         PSYM = (2.*LOG((1 + X)/2.)) + X2 - (2.*ATAN(X)) + PIOVER2

      ELSEIF (zL > neut_limit) THEN !Stable
         !Van Ulden & Holtslag (1985) p 1206
         PSYM = (-17.*(1.-EXP(-0.29*zl)))

      END IF
      RETURN
   END FUNCTION psi_mom_W16

   !_______________________________________________________________
   !
   FUNCTION psi_heat_W16(ZL) RESULT(psyh)
      ! Based on stab_fn_heat for StabilityMethod == 2
      ! PSYH - stability function for heat
      !Input: Stability (z-d)/L
      ! Note: Unstable part corresponds to Dyer (1974) mod. Hogstrom (1988)
      ! Note: Stable part corresponds to Dyer (1974) mod. Hogstrom (1988) (implicit else case)

      ! use mod_k ! Original dependency - removed as not needed in isolated function
      IMPLICIT NONE

      ! REAL (KIND(1d0)), PARAMETER :: neut_limit = 1.E-4 ! Limit for neutral stability (Assumed from context)
      REAL(KIND(1D0)) :: zl, psyh, x

      IF (ABS(zL) <= neut_limit) THEN !Neutral
         psyh = 0
      ELSEIF (zL < -neut_limit) THEN ! Unstable
         ! Dyer 1974 X=(1.-(16.*ZL))**(0.5)modified Hosgstrom
         x = 0.95*(1.-15.2*zl)**0.5
         ! PSYH=2*LOG((1+x**2)/2) ! Original line from snippet - potentially incorrect B-D form
         PSYH = 2*LOG((1 + x)/2) ! Corrected form based on standard B-D type functions
      ELSE IF (zL > neut_limit) THEN !Stable
         ! Dyer (1974)  PSYH=(-5)*ZL        modifed  Hogstrom (1988) (implicit else case)
         PSYH = (-4.5)*Zl
      END IF

      RETURN
   END FUNCTION psi_heat_W16
END MODULE module_phys_atmmoiststab

! Backward compatibility alias
MODULE AtmMoistStab_module
   USE module_phys_atmmoiststab
END MODULE AtmMoistStab_module
