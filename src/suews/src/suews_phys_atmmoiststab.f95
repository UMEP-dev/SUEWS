MODULE AtmMoistStab_module
   USE SUEWS_DEF_DTS, ONLY: atm_state, SUEWS_FORCING, SUEWS_TIMER, SUEWS_STATE
   IMPLICIT NONE
!nakao b Nov. 2024 : To set neutral limit flexible in suews_phys_rslprof.f95
!   REAL(KIND(1D0)), PARAMETER :: neut_limit = 1.E-2 !Limit for neutral stability
   REAL(KIND(1D0)), save :: neut_limit = 1.E-2 !Limit for neutral stability
!nakao a Nov. 2024
   REAL(KIND(1D0)), PARAMETER :: k = 0.4 !Von Karman's contant
   REAL(KIND(1D0)), PARAMETER :: grav = 9.80665 !g - gravity - physics today august 1987

   ! scheme code
   INTEGER, PARAMETER :: J12 = 2 ! Cheng and Brutsaert (2005)
   INTEGER, PARAMETER :: k75 = 3 ! Kondo (1975) adopted by Campbell & Norman eqn 7.26 p 97
   INTEGER, PARAMETER :: B71 = 4 ! Businger et al (1971) modifed  Hogstrom (1988)

CONTAINS

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
            lvS_J_kg => atmState%lvS_J_kg, &
            es_hPa => atmState%es_hPa, &
            Ea_hPa => atmState%Ea_hPa, &
            VPd_hpa => atmState%VPd_hpa, &
            VPD_Pa => atmState%VPD_Pa, &
            dq => atmState%dq, &
            dens_dry => atmState%dens_dry, &
            avcp => atmState%avcp, &
            avdens => atmState%avdens &
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

      REAL(KIND(1D0)), PARAMETER :: len_day_s = 24*3600 ! day length in seconds
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
      es_hPa, Ea_hPa, VPd_hpa, VPD_Pa, dq, dens_dry, avcp, air_dens)

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
      INTEGER :: from = 1

      !Saturation vapour pressure over water in hPa
      es_hPa = sat_vap_press_x(Temp_C, Press_hPa, from, dectime) ! dectime is more or less unnecessary here

      !Vapour pressure of water in hPa
      Ea_hPa = avRh/100*es_hPa

      ! if(debug.and.dectime>55.13.and.dectime<55.2)write(35,*)'%',Temp_C

      VPd_hpa = es_hPa - ea_hpa !vapour pressure deficit in hPa
      VPD_Pa = (es_hPa*100) - (Ea_hPa*100) !vapour pressure deficit in Pa

      dq = spec_hum_def(vpd_hPa, Press_hPa) !Specific humidity deficit in g/kg

      !Vap density or absolute humidity         (kg/m3)
      vap_dens = (Ea_hPa*100/((Temp_C + 273.16)*gas_ct_wv))

      !density Dry Air Beer(1990)        kg/m3
      dens_dry = ((Press_hPa - Ea_hPa)*100)/(gas_ct_dry*(273.16 + Temp_C))

      !Air density in kg/m3
      air_dens = (Press_hPa*100)/(gas_ct_dry*(Temp_C + 273.16))

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
         CALL ErrorHint(46, 'Function LUMPS_cal_AtmMoist', press_hPa, -55.55D0, -55)
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
      zL) !Stability scale

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
      ! REAL(KIND(1d0)),INTENT(out)::psim   !Stability function of momentum

      REAL(KIND(1D0)) :: G_T_K, &
                         KUZ, &
                         LOLD, &
                         psim, &
                         z0l, &
                         psimz0, &
                         H_init, &
                         h
      INTEGER, PARAMETER :: notUsedI = -55

      INTEGER :: i

      LOGICAL :: debug = .FALSE.

      !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity
      H_init = QH_init/(avdens*avcp)

      IF (debug) WRITE (*, *) StabilityMethod, z0m, avU1, H_init, UStar, L_MOD
      G_T_K = (Grav/(Temp_C + 273.16))*k !gravity constant/(Temperature*Von Karman Constant)
      KUZ = k*AvU1 !Von Karman constant*mean wind speed
      IF (zzd < 0) CALL ErrorHint(32, &
                                  'Windspeed Ht too low relative to zdm [Stability calc]- values [z-zdm, zdm]', &
                                  Zzd, zdm, notUsedI)

      UStar = KUZ/LOG(Zzd/z0m) !Initial setting of u* and calc. of L_MOD (neutral situation)
      IF (ABS(H_init) < 0.001) THEN ! prevent zero TStar
         h = 0.001
      ELSE
         h = H_init
      END IF
      TStar = (-H/UStar)
      L_MOD = (UStar**2)/(G_T_K*TStar)

      IF (LOG(zzd/z0m) < 0.001000) THEN
         ! PRINT*, 1/(z0m-z0m)
         CALL ErrorHint(17, 'In stability subroutine, (z-zd) < z0.', zzd, z0m, notUsedI)
      END IF
      i = 1
      LOLD = -999.
      z0L = z0m/L_MOD !z0m roughness length
      DO WHILE ((ABS(LOLD - L_MOD) > 0.01) .AND. (i < 330)) !NT: add error threshold !Iteration starts
         LOLD = L_MOD
         zL = zzd/L_MOD
         z0L = z0m/L_MOD !z0m roughness length

         ! IF (zL>2)THEN
         !    CALL ErrorHint(73,'LUMPS_atmos_functions_stab.f95: stability scale (z/L), UStar',zL,UStar,notUsedI)
         !    RETURN !MO-theory not necessarily valid above zL>2. Still causes problematic UStar values and correct limit might be 0.3.
         !    !Needs more investigations.
         ! END IF

         psim = stab_psi_mom(StabilityMethod, zL)
         psimz0 = stab_psi_mom(StabilityMethod, z0L)

         UStar = KUZ/(LOG(Zzd/z0m) - psim + psimz0) !Friction velocity in non-neutral situation

         TStar = (-H/UStar)
         L_MOD = (UStar**2)/(G_T_K*TStar)

         ! IF(UStar<0.001000)THEN       !If u* too small
         !    UStar=KUZ/(LOG(Zzd/z0m))
         !    PRINT*, 'UStar info',UStar,KUZ,(LOG(Zzd/z0m)),Zzd,z0m
         !    CALL ErrorHint(30,'SUBROUTINE STAB_lumps:[ u*< 0.001] zl,dectime',zl,dectime,notUsedI)
         !    CALL ErrorHint(30,'SUBROUTINE STAB_lumps:[ u*< 0.001] z0l,UStar',z0l,UStar,notUsedI)
         !    CALL ErrorHint(30,'SUBROUTINE STAB_lumps:[ u*< 0.001] psim,psimz0',psim,psimz0,notUsedI)
         !    CALL ErrorHint(30,'SUBROUTINE STAB_lumps:[ u*< 0.001] AVU1,log(zzd/z0m)',AVU1,LOG(zzd/z0m),notUsedI)
         !
         !    ! RETURN
         ! ENDIF

         i = i + 1
      END DO

      ! limit zL to be within [-5,5]
      ! IF (zL < -5 .OR. zL > 5) THEN
      ! zL = MIN(5., MAX(-5., zL))
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

      ! TS 11 Feb 2021: limit UStar and TStar to reasonable ranges
      ! under all conditions, min(UStar)==0.001 m s-1 (Jimenez et al 2012, MWR, https://doi.org/10.1175/mwr-d-11-00056.1
      UStar = MAX(0.001, UStar)
      ! under convective/unstable condition, min(UStar)==0.15 m s-1: (Schumann 1988, BLM, https://doi.org/10.1007/BF00123019)
      IF (z0L < -neut_limit) UStar = MAX(0.15, UStar)

      ! update associated variables
      TStar = (-H/UStar)
      L_MOD = (UStar**2)/(G_T_K*TStar)
      zL = zzd/L_MOD

      ! if ( L_MOD<-990 ) then
      !   print*, 'L_MOD too low',L_MOD
      !   print*, 1/(L_MOD-L_MOD)
      !
      ! end if

   END SUBROUTINE cal_Stab

   !==================================================================
   ! TS 20210208: restructure stability corrections funtions;
   !  - the previous implementations are kept down below for cross-validation

   ! integral form for momentum
   FUNCTION stab_psi_mom(StabilityMethod, ZL) RESULT(psim)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: zl, psim
      INTEGER :: StabilityMethod

      SELECT CASE (StabilityMethod)
      CASE (J12)
         psim = psi_mom_J12(ZL)

      CASE (K75)
         psim = psi_mom_K75(ZL)

      CASE (B71)
         psim = psi_mom_B71(ZL)

      END SELECT

   END FUNCTION

   ! integral form for heat
   FUNCTION stab_psi_heat(StabilityMethod, ZL) RESULT(psih)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: zl, psih
      INTEGER :: StabilityMethod

      SELECT CASE (StabilityMethod)
      CASE (J12)
         psih = psi_heat_J12(ZL)

      CASE (K75)
         psih = psi_heat_K75(ZL)

      CASE (B71)
         psih = psi_heat_B71(ZL)

      END SELECT

   END FUNCTION

   ! correction function for momentum
   FUNCTION stab_phi_mom(StabilityMethod, ZL) RESULT(phim)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: zl, phim
      INTEGER :: StabilityMethod

      SELECT CASE (StabilityMethod)
      CASE (J12)
         phim = phi_mom_J12(zl)

      CASE (K75)
         phim = phi_mom_K75(zl)

      CASE (B71)
         phim = phi_mom_B71(zl)

      END SELECT

   END FUNCTION

   ! correction function for heat
   FUNCTION stab_phi_heat(StabilityMethod, ZL) RESULT(phih)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: zl, phih
      INTEGER :: StabilityMethod

      SELECT CASE (StabilityMethod)
      CASE (J12)
         phih = phi_heat_J12(zl)

      CASE (K75)
         phih = phi_heat_K75(zl)

      CASE (B71)
         phih = phi_heat_B71(zl)

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

   !==================================================================
   ! previous implementations
   ! TS 10 Feb 2021: commented after checing the new ones above match the ones below
   ! FUNCTION stab_psi_mom(StabilityMethod, ZL) RESULT(psim)
   !    !     StabilityMethod = 1-4 -
   !    !     psim - stability FUNCTION for momentum
   !    !Modified by LJ Mar 2010
   !    !Input:Used stability method, stability (z-d)/L, zeta (either (z-d)/L or z0/L)

   !    ! USE mod_z
   !    ! USE mod_k

   !    IMPLICIT NONE

   !    REAL(KIND(1D0)):: piover2, psim, zl, x, x2
   !    INTEGER ::StabilityMethod

   !    psim = 0

   !    PIOVER2 = ACOS(-1.)/2.
   !    !PRINT*,StabilityMethod,zl,"stab_fn_mom:"
   !    IF (ABS(zL) < neut_limit) THEN
   !       psim = 0
   !    ELSEIF (zL < -neut_limit) THEN    !Unstable

   !       IF (StabilityMethod == 1) THEN     !    Jensen et al 1984 - Van Ulden & Holtslag (1985) p 1206
   !          psim = ((1.-16.*zl)**0.25) - 1

   !       ELSEIF (StabilityMethod == 2) THEN !Dyer (1974)(1-16z/L)**.25' k=0.41  mod. Hogstrom (1988)v15.2
   !          X = (1.-(15.2*zl))**0.25
   !          X2 = LOG((1 + (X**2.))/2.)
   !          psim = (2.*LOG((1 + X)/2.)) + X2 - (2.*ATAN(X)) + PIOVER2

   !       ELSEIF (StabilityMethod == 3) THEN ! Kondo (1975) adopted by Campbell & Norman eqn 7.26 p 97
   !          psim = 0.6*(2)*LOG((1 + (1 - 16*zl)**0.5)/2)

   !       ELSEIF (StabilityMethod == 4) THEN !Businger et al (1971) modifed  Hogstrom (1988)
   !          x = (1 - 19.3*zl)**(0.25) ! M Nag spotted the wrong exponent, TS corrected this from (1 - 19.3*zl_f)**(-0.25)
   !          X2 = LOG((1 + (X**2.))/2.)
   !          psim = (2.*LOG((1 + X)/2.)) + X2 - (2.*ATAN(X)) + PIOVER2

   !          ! TS 20210208: commented out as not mentioned in manual
   !          ! ELSEIF (StabilityMethod == 5) THEN ! Zilitinkevich & Chalikov (1968) modified Hogstrom (1988)
   !          !    IF (zl >= -0.16) THEN
   !          !       x = 1 + 1.38*zl
   !          !    ELSE
   !          !       x = 0.42*(-1)*zl**0.333
   !          !    END IF
   !          !    X2 = LOG((1 + (X**2.))/2.)
   !          !    psim = (2.*LOG((1 + X)/2.)) + X2 - (2.*ATAN(X)) + PIOVER2

   !          ! ELSEIF (StabilityMethod == 6) THEN !     Foken and Skeib (1983)
   !          !    IF (zl >= 0.06) THEN
   !          !       x = 1
   !          !    ELSE
   !          !       x = ((-1)*zl/0.06)**0.25
   !          !    END IF
   !          !    X2 = LOG((1 + (X**2.))/2.)
   !          !    psim = (2.*LOG((1 + X)/2.)) + X2 - (2.*ATAN(X)) + PIOVER2

   !          ! ELSEIF (StabilityMethod == 7) THEN ! Dyer & Bradley (1982) (1-28z/L)**.25' k=0.4
   !          !    X = (1 - (28.*zl))**0.25  ! NT: changed + to - (bug -> checked reference)
   !          !    X2 = LOG((1 + X**2.)/2.)
   !          !    psim = (2.*LOG((1 + X)/2.)) + X2 - (2.*ATAN(X)) + PIOVER2
   !       END IF

   !    ELSEIF (zL > neut_limit) THEN            !Stable

   !       IF (StabilityMethod == 1) THEN         !Dyer (1974) k=0.35 x=1+5*zl Mod. Hogstrom (1988)
   !          psim = (-4.8)*zl
   !       ELSEIF (StabilityMethod == 2) THEN     !Van Ulden & Holtslag (1985) p 1206
   !          IF (zl > 1000.) THEN
   !             zl = 1000.
   !          END IF
   !          psim = (-17.*(1.-EXP(-0.29*zl)))
   !       ELSEIF (StabilityMethod == 3) THEN ! Kondo (1975) adopted by Campbell & Norman eqn 7.26 p 97
   !          psim = (-6)*LOG(1 + zl)
   !       ELSEIF (StabilityMethod == 4) THEN ! Businger et al (1971) modifed  Hogstrom (1988)
   !          ! psim=1+6*zl_f  ! this is NOT the integral form but the stability function, TS 13 Jun 2017
   !          psim = (-6)*zl   ! this is the integral form, TS 13 Jun 2017

   !       END IF
   !    END IF
   !    RETURN
   ! END FUNCTION stab_psi_mom

   ! FUNCTION stab_phi_mom(StabilityMethod, ZL) RESULT(phim)
   !    !     StabilityMethod = 1-4 -
   !    !     phi - stability FUNCTION for momentum
   !    !Modified by NT May 2019 !!!!! check if all are correct!
   !    !Input:Used stability method, stability (z-d)/L, zeta (either (z-d)/L or z0/L)

   !    IMPLICIT NONE

   !    REAL(KIND(1D0)):: phim, zl
   !    INTEGER ::StabilityMethod

   !    phim = 1

   !    IF (ABS(zL) < neut_limit) THEN
   !       phim = 1
   !    ELSEIF (zL < -neut_limit) THEN    !Unstable

   !       IF (StabilityMethod == 1) THEN     !    Jensen et al 1984 - Van Ulden & Holtslag (1985) p 1206&
   !          phim = ((1.-16.*zl)**(-0.25))
   !       ELSEIF (StabilityMethod == 2) THEN !Dyer (1974)(1-16z/L)**.25' k=0.41  mod. Hogstrom (1988)v15.2
   !          phim = (1.-(15.2*zl))**(-0.25)
   !       ELSEIF (StabilityMethod == 3) THEN ! Kondo (1975) adopted by Campbell & Norman eqn 7.26 p 97
   !          phim = ((1.-16.*zl)**(-0.25))
   !       ELSEIF (StabilityMethod == 4) THEN !Businger et al (1971) modifed  Hogstrom (1988)
   !          phim = (1.-19.3*zl)**(-0.25)

   !          ! TS 20210208: methods 5–7 are commented as they are not mentioned in the manual
   !          ! ELSEIF (StabilityMethod == 5) THEN ! Zilitinkevich & Chalikov (1968) modified Hogstrom (1988)
   !          !    IF (zl >= -0.16) THEN
   !          !       phim = 1 + 1.38*zl
   !          !    ELSE
   !          !       phim = 0.42*(-1)*zl*(-0.333)
   !          !    END IF
   !          ! ELSEIF (StabilityMethod == 6) THEN !     Foken and Skeib (1983)
   !          !    IF (zl >= 0.06) THEN
   !          !       phim = 1
   !          !    ELSE
   !          !       phim = ((-1)*zl/0.06)**(-0.25)
   !          !    END IF
   !          ! ELSEIF (StabilityMethod == 7) THEN ! Dyer & Bradley (1982) (1-28z/L)**.25' k=0.4
   !          !    phim = (1.-(28.*zl))**(-0.25)
   !       END IF

   !    ELSEIF (zL > neut_limit) THEN            !Stable

   !       IF (StabilityMethod == 1) THEN         !Dyer (1974) k=0.35 x=1+5*zl Mod. Hogstrom (1988)
   !          phim = 1.+(4.8)*zl
   !       ELSEIF (StabilityMethod == 2) THEN     !Van Ulden & Holtslag (1985) p 1206 ! NT: have no function for phim
   !          phim = 1.+(4.8)*zl
   !       ELSEIF (StabilityMethod == 3) THEN ! Kondo (1975) adopted by Campbell & Norman eqn 7.26 p 97  !!NT: checked
   !          phim = 1.+6.*zl/(1.+zl)  !!NT: checked reference and updated
   !       ELSEIF (StabilityMethod == 4) THEN ! Businger et al (1971) modifed  Hogstrom (1988)
   !          phim = 1 + 6*zl
   !       END IF
   !    END IF
   !    RETURN
   ! END FUNCTION stab_phi_mom
   ! !_______________________________________________________________
   ! !
   ! ! psih - stability function for heat
   ! FUNCTION stab_psi_heat(StabilityMethod, ZL) RESULT(psih)
   !    ! USE mod_k
   !    IMPLICIT NONE

   !    REAL(KIND(1D0)):: zl, psih, x
   !    INTEGER :: StabilityMethod

   !    ! initialisation
   !    psih = 0
   !    x = 0

   !    IF (ABS(zl) < neut_limit) THEN      !Neutral
   !       psih = 0
   !    ELSEIF (zL < -neut_limit) THEN     ! Unstable
   !       IF (StabilityMethod == 3) THEN
   !          !campbell & norman eqn 7.26
   !          psih = (2)*LOG((1 + (1 - 16*zl)**0.5)/2)
   !       ELSE
   !          IF (StabilityMethod == 4) THEN ! Businger et al (1971) modifed  Hogstrom (1988)
   !             x = 0.95*(1.-11.6*zl)**(0.5)
   !          ELSEIF (StabilityMethod == 2) THEN ! Dyer 1974 X=(1.-(16.*ZL))**(0.5)modified Hosgstrom
   !             x = 0.95*(1.-15.2*zl)**0.5

   !             ! TS 20210208: methed=7 removed as not mentioned in manual
   !             ! ELSEIF (StabilityMethod == 7) THEN
   !             !    x = (1 - (28.*ZL))**0.25
   !          END IF
   !          ! psih = 2*LOG((1 + x**2)/2)  ! NT: do not think this is correct
   !          psih = 2*LOG((1 + x)/2)
   !       END IF

   !    ELSE IF (zL > neut_limit) THEN    !Stable
   !       IF (StabilityMethod == 3) THEN
   !          !campbell & norman eqn 7.26
   !          psih = (-6)*LOG(1 + zl)
   !       ELSE
   !          IF (zL <= 1) THEN ! weak/moderate stable
   !             IF (StabilityMethod == 4) THEN !Businger et al (1971) modifed  Hogstrom (1988)
   !                psih = (-7.8)*zl
   !             ELSE !Dyer (1974)  psih=(-5)*ZL        modifed  Hogstrom (1988)
   !                psih = (-4.5)*zl
   !             END IF
   !          ELSE
   !             ! adopt the form as Brutsaert (1982) eqn 4.58. but following the coeffs. of the above eqns
   !             IF (StabilityMethod == 4) THEN !Businger et al (1971) modifed  Hogstrom (1988)
   !                psih = (-7.8)*(1 + LOG(zl))
   !             ELSE !Dyer (1974)  psih=(-5)*ZL        modifed  Hogstrom (1988)
   !                psih = (-4.5)*(1 + LOG(zl))
   !             END IF
   !          END IF
   !       END IF

   !    END IF

   !    RETURN
   ! END FUNCTION stab_psi_heat
   ! !_______________________________________________________________
   ! !
   ! ! Phi - stability function for heat !!!!NT CHECK!!!!
   ! FUNCTION stab_phi_heat(StabilityMethod, ZL) RESULT(phih)

   !    IMPLICIT NONE

   !    REAL(KIND(1D0)):: zl, phih
   !    INTEGER :: StabilityMethod

   !    phih = 1

   !    IF (ABS(zl) < neut_limit) THEN      !Neutral
   !       phih = 1
   !    ELSEIF (zL < -neut_limit) THEN     ! Unstable
   !       IF (StabilityMethod == 3) THEN
   !          !campbell & norman eqn 7.26
   !          phih = (1.-16.*zl)**(-0.5)
   !       ELSEIF (StabilityMethod == 4) THEN ! Businger et al (1971) modifed  Hogstrom (1988)
   !          phih = 0.95*(1.-11.6*zl)**(-0.5)
   !       ELSEIF (StabilityMethod == 7) THEN
   !          phih = (1 - (28.*ZL))**(-0.25)
   !       ELSEIF (StabilityMethod == 2) THEN ! Dyer 1974 X=(1.-(16.*ZL))**(0.5)modified Hosgstrom
   !          phih = 0.95*(1.-15.2*zl)**(-0.5)
   !       END IF

   !    ELSE IF (zL > neut_limit) THEN    !Stable
   !       IF (StabilityMethod == 3) THEN
   !          !campbell & norman eqn 7.26
   !          phih = 1 + 6*zl/(1 + zL)
   !       ELSE
   !          IF (StabilityMethod == 4) THEN !Businger et al (1971) modifed  Hogstrom (1988)
   !             phih = 0.95 + 7.8*zl ! this is NOT the integral form but the stability function, TS 13 Jun 2017
   !          ELSE !Dyer (1974)  psih=(-5)*ZL        modifed  Hogstrom (1988)
   !             phih = 1.+4.5*zl
   !          END IF

   !       END IF
   !    END IF

   !    RETURN
   ! END FUNCTION stab_phi_heat

   !--------------------------------------------------------------------------------
   ! psys - roughness sublayer correction psi_*
   !
   !     Garratt (1980) QJRMS Appendix 1 p 815/816

   ! FUNCTION stab_fn_rou(z, zstar) RESULT(psys)
   !    IMPLICIT NONE
   !    REAL(KIND(1d0))::alpha, zeta, z, psys, zstar, alpha1
   !    !     z wind speed height - z_d
   !    !     zstar height of the roughness sublayer
   !    !     eqn (a4) using alpha=0.5 alpha1=0.7
   !    alpha = 0.5
   !    alpha1 = 0.7
   !    zeta = z/zstar
   !    psys = (alpha - 1)*LOG(zeta) - (alpha*alpha1)*(1 - zeta) - (alpha*alpha1**2) &
   !           *(1 - zeta**2)/6.-(alpha*alpha1**3)*(1 - zeta**3)/24.
   !    RETURN
   ! END FUNCTION stab_fn_rou

END MODULE AtmMoistStab_module
