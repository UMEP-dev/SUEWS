! Main module following naming standard: matches filename
MODULE suews_phys_resist
   IMPLICIT NONE

CONTAINS
   SUBROUTINE AerodynamicResistance( &
      ZZD, & ! input:
      z0m, &
      AVU1, &
      L_mod, &
      UStar, &
      VegFraction, &
      AerodynamicResistanceMethod, &
      StabilityMethod, &
      RoughLenHeatMethod, &
      RA_h, z0V) ! output:

      ! Returns Aerodynamic resistance (RA) to the main program SUEWS_Calculations
      ! All RA equations reported in Thom & Oliver (1977)
      ! Modified by TS 08 Aug 2017 - interface modified
      ! Modified by LJ
      !   -Removal of tabs and cleaning the code
      ! Modified by HCW 03 Dec 2015 - changed lower limit on RA from 2 s m-1 to 10 s m-1 (to avoid unrealistically high evaporation rates)
      ! Modified by LJ in 12 April to not to be used with global variables
      ! To Do:
      !       - Check whether the thresholds 2-200 s m-1 are suitable over a range of z0!! HCW 04 Mar 2015
      ! OUTPUT: RA - Aerodynamic resistance [s m^-1]
      ! INPUT:  AerodynamicResistanceMethod = Method to calculate RA
      !         StabilityMethod = defines the method to calculate atmospheric stability
      !         RoughLenHeatMethod = Method to calculate heat roughness length
      !         *Measurement height minus* Displacement height (m) (was incorrectly labelled, corrected HCW 25 May 2016
      !         z0m = Aerodynamic roughness length (m)
      !         k2 = Power of Van Karman's constant (= 0.16 = 0.4^2)
      !         AVU1 = Mean wind speed
      !         L_mod = Obukhov length (m)
      !         UStar = Friction velocity (m s-1)
      !         VegFraction = Fraction of vegetation
      !               (changed from veg_fr which also includes water surface by HCW 05 Nov 2015)

      USE AtmMoistStab_module, ONLY: stab_psi_heat, stab_psi_mom
      USE sues_data, ONLY: psih

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: ZZD !Active measurement height (meas. height-displac. height)
      REAL(KIND(1D0)), INTENT(in) :: z0m !Aerodynamic roughness length
      REAL(KIND(1D0)), INTENT(in) :: AVU1 !Average wind speed
      REAL(KIND(1D0)), INTENT(in) :: L_mod !Monin-Obukhov length (either measured or modelled)
      REAL(KIND(1D0)), INTENT(in) :: UStar !Friction velocity
      REAL(KIND(1D0)), INTENT(in) :: VegFraction !Fraction of vegetation

      INTEGER, INTENT(in) :: AerodynamicResistanceMethod
      INTEGER, INTENT(in) :: StabilityMethod
      INTEGER, INTENT(in) :: RoughLenHeatMethod

      REAL(KIND(1D0)), INTENT(out) :: RA_h !Aerodynamic resistance for heat/vapour [s m^-1]
      REAL(KIND(1D0)), INTENT(out) :: z0V

      INTEGER, PARAMETER :: notUsedI = -55

      REAL(KIND(1D0)), PARAMETER :: &
         notUsed = -55.5, &
         k2 = 0.16, & !Power of Van Karman's constant (= 0.16 = 0.4^2)
         muu = 1.46E-5 !molecular viscosity
      REAL(KIND(1D0)) :: psim
      ! REAL(KIND(1d0)):: psih

      !Z0V roughness length for vapour
      z0V = cal_z0V(RoughLenHeatMethod, z0m, VegFraction, UStar)

      !1)Monteith (1965)-neutral stability
      IF (AerodynamicResistanceMethod == 1) THEN
         RA_h = (LOG(ZZD/z0m)**2)/(k2*AVU1)

         !2) Non-neutral stability
         !    PSIM - stability function for momentum
         !     psih - stability function for heat
         !    assuming stability functions the same for heat and water
      ELSEIF (AerodynamicResistanceMethod == 2) THEN !Dyer (1974)

         ! psim = stab_psi_mom(StabilityMethod, zzd/L_mod)
         ! psih = stab_psi_heat(StabilityMethod, ZZD/L_mod)
         psim = stab_psi_mom(StabilityMethod, zzd/L_mod) - stab_psi_mom(StabilityMethod, z0m/L_mod)
         psih = stab_psi_heat(StabilityMethod, ZZD/L_mod) - stab_psi_heat(StabilityMethod, z0v/L_mod)

         IF (Zzd/L_mod == 0 .OR. UStar == 0) THEN
            RA_h = (LOG(ZZD/z0m)*LOG(ZZD/z0V))/(k2*AVU1) !Use neutral equation
         ELSE
            RA_h = ((LOG(ZZD/z0m) - psim)*(LOG(ZZD/z0V) - psih))/(K2*AVU1)
            ! RA = AVU1/UStar**2
         END IF

         !3) Thom and Oliver (1977)
      ELSEIF (AerodynamicResistanceMethod == 3) THEN
         RA_h = (4.72*LOG(ZZD/z0m)**2)/(1 + 0.54*AVU1)
      END IF

      !If RA outside permitted range, adjust extreme values !!Check whether these thresholds are suitable over a range of z0
      IF (RA_h > 120) THEN !was 175
         CALL errorHint(7, 'In AerodynamicResistance.f95, calculated RA > 200 s m-1; RA set to 200 s m-1', RA_h, notUsed, notUsedI)
         RA_h = 120
      ELSEIF (RA_h < 10) THEN !found  By Shiho - fix Dec 2012  !Threshold changed from 2 to 10 s m-1 (HCW 03 Dec 2015)
         CALL errorHint(7, 'In AerodynamicResistance.f95, calculated RA < 10 s m-1; RA set to 10 s m-1', RA_h, notUsed, notUsedI)
         RA_h = 10
         ! RA=(log(ZZD/z0m))**2/(k2*AVU1)
         IF (avu1 < 0) WRITE (*, *) avu1, RA_h
      END IF

      RETURN
   END SUBROUTINE AerodynamicResistance

   SUBROUTINE SurfaceResistance( &
      id, it, & ! input:
      SMDMethod, SnowFrac, sfr_surf, avkdn, Tair, dq, xsmd, vsmd, MaxConductance, &
      LAIMax, LAI_id, gsModel, Kmax, &
      G_max, G_k, g_q_base, g_q_shape, G_t, G_sm, TH, TL, S1, S2, &
      g_kdown, g_dq, g_ta, g_smd, g_lai, & ! output:
      gfunc, gsc, RS) ! output:
      ! Calculates bulk surface resistance (ResistSurf [s m-1]) based on Jarvis 1976 approach
      ! Last modified -----------------------------------------------------
      ! MH  01 Feb 2019: gsModel choices to model with air temperature or 2 meter temperature. Added gfunc for photosynthesis calculations
      ! HCW 21 Jul 2016: If no veg surfaces, vsmd = NaN so QE & QH = NaN; if water surfaces only, smd = NaN so QE & QH = NaN.
      !                  Add checks here so that gs (soil part) = 0 in either of these situations.
      !                  This shouldn't change results but handles NaN error.
      ! HCW 01 Mar 2016: SM dependence is now on modelled smd for vegetated surfaces only (vsmd) (Note: obs smd still not operational!)
      ! HCW 18 Jun 2015: Alternative gs parameterisation added using different functional forms and new coefficients
      ! HCW 31 Jul 2014: Modified condition on g6 part to select meas/mod smd
      ! LJ  24 Apr 2013: Added impact of snow fraction in LAI and in soil moisture deficit
      ! -------------------------------------------------------------------

      ! USE allocateArray
      ! USE data_in
      ! USE defaultNotUsed
      ! USE gis_data
      ! USE moist
      ! USE resist
      ! USE sues_data

      IMPLICIT NONE
      ! INTEGER,PARAMETER::BldgSurf=2
      INTEGER, PARAMETER :: ConifSurf = 3
      INTEGER, PARAMETER :: DecidSurf = 4
      INTEGER, PARAMETER :: GrassSurf = 5
      ! INTEGER,PARAMETER::ivConif=1
      ! INTEGER,PARAMETER::ivGrass=3
      ! INTEGER,PARAMETER::MaxNumberOfGrids=2000
      ! INTEGER,PARAMETER::ndays=366
      INTEGER, PARAMETER :: nsurf = 7
      ! INTEGER,PARAMETER::NVegSurf=3
      ! INTEGER,PARAMETER::PavSurf=1
      INTEGER, PARAMETER :: WaterSurf = 7

      INTEGER, INTENT(in) :: id
      INTEGER, INTENT(in) :: it ! time: day of year and hour
      INTEGER, INTENT(in) :: gsModel !Choice of gs parameterisation (1 = Ja11, 2 = Wa16)
      INTEGER, INTENT(in) :: SMDMethod !Method of measured soil moisture
      ! INTEGER,INTENT(in)::ConifSurf!= 3, surface code
      ! INTEGER,INTENT(in)::DecidSurf!= 4, surface code
      ! INTEGER,INTENT(in)::GrassSurf!= 5, surface code
      ! INTEGER,INTENT(in)::WaterSurf!= 7, surface code
      ! INTEGER,INTENT(in)::nsurf!= 7, Total number of surfaces

      REAL(KIND(1D0)), INTENT(in) :: avkdn !Average downwelling shortwave radiation
      REAL(KIND(1D0)), INTENT(in) :: Tair !Air temperature
      REAL(KIND(1D0)), INTENT(in) :: Kmax !Annual maximum hourly solar radiation
      REAL(KIND(1D0)), INTENT(in) :: G_max !Fitted parameters related to max surface resistance
      REAL(KIND(1D0)), INTENT(in) :: G_k !Fitted parameters related to incoming solar radiation
      REAL(KIND(1D0)), INTENT(in) :: g_q_base !Fitted parameters related to humidity deficit, base value
      REAL(KIND(1D0)), INTENT(in) :: g_q_shape !Fitted parameters related to humidity deficit, shape parameter
      REAL(KIND(1D0)), INTENT(in) :: G_t !Fitted parameters related to air temperature
      REAL(KIND(1D0)), INTENT(in) :: G_sm !Fitted parameters related to soil moisture deficit
      REAL(KIND(1D0)), INTENT(in) :: S1 !Fitted parameters related to wilting point
      REAL(KIND(1D0)), INTENT(in) :: S2 !Fitted parameters related to wilting point
      REAL(KIND(1D0)), INTENT(in) :: TH !Maximum temperature limit
      REAL(KIND(1D0)), INTENT(in) :: TL !Minimum temperature limit
      REAL(KIND(1D0)), INTENT(in) :: dq !Specific humidity deficit
      REAL(KIND(1D0)), INTENT(in) :: xsmd !Measured soil moisture deficit
      REAL(KIND(1D0)), INTENT(in) :: vsmd !QUESTION: Soil moisture deficit for vegetated surfaces only (what about BSoil?)

      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: MaxConductance !Max conductance [mm s-1]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: LAIMax !Max LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(3), INTENT(in) :: LAI_id !=LAI(id-1,:), LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac !Surface fraction of snow cover
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !Surface fractions [-]
      ! g_kdown*g_dq*g_ta*g_smd*g_lai
      REAL(KIND(1D0)), INTENT(out) :: g_kdown !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_dq !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_ta !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_smd !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: g_lai !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: gfunc !gdq*gtemp*gs*gq for photosynthesis calculations
      REAL(KIND(1D0)), INTENT(out) :: gsc !Surface Layer Conductance
      REAL(KIND(1D0)), INTENT(out) :: RS !Surface resistance
      REAL(KIND(1D0)), PARAMETER :: gsc_min = 0.1 !Minimum surface conductance

      REAL(KIND(1D0)) :: &
         ! g_lai, & !G(LAI)
         QNM, & !QMAX/(QMAX+G2)
         ! g_kdown, & !G(Q*)
         ! g_dq, & !G(dq)
         TC, & !Temperature parameter 1
         TC2, & !Temperature parameter 2
         ! g_ta, & !G(T)
         sdp !S1/G6+S2
      ! g_smd !G(Soil moisture deficit)

      INTEGER :: iv
      REAL(KIND(1D0)) :: id_real

      REAL(KIND(1D0)), PARAMETER :: notUsed = -55
      ! REAL(KIND(1d0)),PARAMETER :: notUsedi=-55.5

      ! initialisation
      g_dq = 0.5
      g_ta = 0.5
      g_smd = 0.5
      g_kdown = 0.5

      id_real = REAL(id) !Day of year in real number

      !gsModel = 1 - original parameterisation (Jarvi et al. 2011)
      !gsModel = 2 - new parameterisation (Ward et al. 2016)
      !gsModel = 3 - original parameterisation (Jarvi et al. 2011) with 2 m temperature
      !gsModel = 4 - new parameterisation (Ward et al. 2016) with 2 m temperature

      IF (gsModel == 1 .OR. gsModel == 3) THEN
         ! kdown ----
         QNM = Kmax/(Kmax + G_k)
         !gq=(qn1/(g2+qn1))/qnm !With net all-wave radiation
         g_kdown = (avkdn/(G_k + avkdn))/QNM !With Kdown
         ! specific humidity deficit ----
         IF (dq < g_q_shape) THEN
            g_dq = 1 - g_q_base*dq
         ELSE
            g_dq = 1 - g_q_base*g_q_shape
         END IF
         ! air temperature ----
         TC = (TH - G_t)/(G_t - TL)
         TC2 = (G_t - TL)*(TH - G_t)**TC
         !If air temperature below TL or above TH, fit it to TL+0.1/TH-0.1
         IF (Tair <= tl) THEN
            g_ta = (tl + 0.1 - tl)*(th - (tl + 0.1))**tc/tc2
            !Call error only if no snow on ground
            !  IF (MIN(SnowFrac(1),SnowFrac(2),SnowFrac(3),SnowFrac(4),SnowFrac(5),SnowFrac(6))/=1) THEN
            IF (MINVAL(SnowFrac(1:6)) /= 1) THEN
               CALL errorHint(29, 'subroutine SurfaceResistance.f95: T changed to fit limits TL=0.1,Temp_c,id,it', &
                              REAL(Tair, KIND(1D0)), id_real, it)
            END IF
         ELSEIF (Tair >= th) THEN
            g_ta = ((th - 0.1) - tl)*(th - (th - 0.1))**tc/tc2
            CALL errorHint(29, 'subroutine SurfaceResistance.f95: T changed to fit limits TH=39.9,Temp_c,id,it', &
                           REAL(Tair, KIND(1D0)), id_real, it)
         ELSE
            g_ta = (Tair - tl)*(th - Tair)**tc/tc2
         END IF

         ! soil moisture deficit
         ! =======================================================================================
         ! TS 20241210: added comments to explain the numerical implications of the parameters
         ! G_sm: soil moisture sensitivity parameter (>0) - larger values mean stronger sensitivity
         !       - Increase G_sm for steeper response to soil moisture changes
         !       - Decrease G_sm for more gradual response
         ! S1: parameter controlling the baseline conductance by moving the response curve up/down
         !     - Increase S1 to increase the overall conductance values
         !     - Decrease S1 to decrease the overall conductance values
         ! S2: soil moisture deficit threshold (mm) - shifts response curve along x-axis
         !     - Increase S2 to delay moisture stress response
         !     - Decrease S2 for earlier onset of moisture limitation
         ! The exp() term will be large and positive when xsmd > S2, making g_smd small
         ! When smd < S2, exp() term becomes smaller, allowing g_smd to approach 1
         ! Example parameter ranges:
         ! - G_sm: typically 0.01-0.5, higher for drought-sensitive vegetation
         ! - S1: typically 0 to 5, adjust based on baseline conductance needs
         ! - S2: typically 10-100 mm, adjust based on vegetation drought tolerance
         ! Calculate the threshold for moisture stress
         ! =======================================================================================
         ! TS 20241211: restore the threshold-based approach for easier interpretation
         sdp = S1/G_sm + S2
         ! with sdp, the following equation can be interpreted as:
         ! when xsmd > sdp, water stress effect kicks in
         ! when xsmd < sdp, water stress effect is not active
         ! meanwhile,
         ! - G_sm holds the meaning of the sensitivity of the response to soil moisture - larger values --> stronger sensitivity
         ! - use MIN(xsmd - sdp, 0.0) to ensure g_smd is non-negative
         ! =======================================================================================
         IF (SMDMethod > 0) THEN !Modified from ==1 to > 0 by HCW 31/07/2014
            ! Measured soil moisture deficit is used
            ! g_smd = 1 - EXP(G_sm*(xsmd - sdp))
            g_smd = 1 - EXP(G_sm*(MIN(xsmd - sdp, 0.0)))
         ELSE
            ! Modelled soil moisture deficit is used
            ! g_smd = 1 - EXP(G_sm*(vsmd - sdp))
            g_smd = 1 - EXP(G_sm*(MIN(vsmd - sdp, 0.0)))
            IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf) == 0 .OR. sfr_surf(WaterSurf) == 1) THEN
               g_smd = 0 !If no veg so no vsmd, or all water so no smd, set gs=0 (HCW 21 Jul 2016)
            END IF
         END IF

         IF (g_smd < 0) THEN
            CALL errorHint(65, &
                           'subroutine SurfaceResistance.f95 (gsModel=1): g(smd) < 0 calculated, setting to 0.0001', &
                           g_smd, id_real, it)
            g_smd = 0.0001
         END IF

         !LAI
         !Original way
         !gl=((LAI(id,2)*areaunir/lm)+areair)/(areair+areaunir)
         !New way
         g_lai = 0 !First initialize
         ! vegetated surfaces
         ! check basis for values koe - maximum surface conductance
         !  print*,id,it,sfr_surf
         ! DO iv=ivConif,ivGrass
         DO iv = 1, 3
            ! gl=gl+(sfr_surf(iv+2)*(1-SnowFrac(iv+2)))*LAI(id-1,iv)/LAIMax(iv)*MaxConductance(iv)
            g_lai = g_lai + (sfr_surf(iv + 2)*(1 - SnowFrac(iv + 2)))*LAI_id(iv)/LAIMax(iv)*MaxConductance(iv)
         END DO

         IF (avkdn <= 0) THEN !At nighttime set gsc at arbitrary low value: gsc=0.1 mm/s (Shuttleworth, 1988b)
            gsc = gsc_min
         ELSE
            ! Multiply parts together
            gsc = MAX(gsc_min, G_max*g_kdown*g_dq*g_ta*g_smd*g_lai)
         END IF

         IF (gsc <= 0) THEN
            CALL errorHint(65, 'subroutine SurfaceResistance.f95 (gsModel=1): gs <= 0, setting to 0.1 mm s-1', gsc, id_real, it)
            gsc = gsc_min
         END IF

      ELSEIF (gsModel == 2 .OR. gsModel == 4) THEN

         ! ---- g(kdown)----
         QNM = Kmax/(Kmax + G_k)
         g_kdown = (avkdn/(avkdn + G_k))/QNM
         IF (avkdn >= Kmax) THEN !! Add proper error handling later - HCW!!
            WRITE (*, *) 'Kmax exceeds Kdn setting to g(Kdn) to 1'
            g_kdown = 1
         END IF

         ! ---- g(delq) ----
         g_dq = g_q_base + (1 - g_q_base)*(g_q_shape**dq) !Ogink-Hendriks (1995) Eq 12 (using G3 as Kshd and G4 as r)

         ! ---- g(Tair) ----
         Tc = (TH - G_t)/(G_t - TL)
         Tc2 = (G_t - TL)*(TH - G_t)**Tc
         ! If air temperature below TL or above TH, then use value for TL+0.1 or TH-0.1
         IF (Tair <= TL) THEN
            g_ta = (TL + 0.1 - TL)*(TH - (TL + 0.1))**Tc/Tc2
            ! Call error only if no snow on ground
            IF (MIN(SnowFrac(1), SnowFrac(2), SnowFrac(3), SnowFrac(4), SnowFrac(5), SnowFrac(6)) /= 1) THEN
               CALL errorHint(29, 'subroutine SurfaceResistance.f95: T changed to fit limits TL+0.1,Temp_C,id,it', &
                              REAL(Tair, KIND(1D0)), id_real, it)
            END IF
         ELSEIF (Tair >= TH) THEN
            g_ta = ((TH - 0.1) - TL)*(TH - (TH - 0.1))**Tc/Tc2
            CALL errorHint(29, 'subroutine SurfaceResistance.f95: T changed to fit limits TH-0.1,Temp_C,id,it', &
                           REAL(Tair, KIND(1D0)), id_real, it)
         ELSE
            g_ta = (Tair - TL)*(TH - Tair)**Tc/Tc2
         END IF
         ! ---- g(smd) ----
         sdp = S1/G_sm + S2
         IF (SMDMethod > 0) THEN !Modified from ==1 to > 0 by HCW 31/07/2014
            g_smd = (1 - EXP(G_sm*(xsmd - sdp)))/(1 - EXP(G_sm*(-sdp))) !Use measured smd
         ELSE
            !gs=1-EXP(g6*(vsmd-sdp))   !Use modelled smd
            g_smd = (1 - EXP(G_sm*(vsmd - sdp)))/(1 - EXP(G_sm*(-sdp)))
            IF (sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf) == 0 .OR. sfr_surf(WaterSurf) == 1) THEN
               g_smd = 0 !If no veg so no vsmd, or all water so no smd, set gs=0 HCW 21 Jul 2016
            END IF
         END IF

         !gs = gs*(1 - SUM(SnowFrac(1:6))/6)

         IF (g_smd < 0) THEN
            CALL errorHint(65, &
                           'subroutine SurfaceResistance.f95 (gsModel=2): gs < 0 calculated, setting to 0.0001', &
                           g_smd, id_real, it)
            g_smd = 0.0001
         END IF

         ! ---- g(LAI) ----
         g_lai = 0 !Initialise
         ! DO iv=ivConif,ivGrass   !For vegetated surfaces
         DO iv = 1, 3 !For vegetated surfaces
            !  gl=gl+(sfr_surf(iv+2)*(1-SnowFrac(iv+2)))*LAI(id-1,iv)/LAIMax(iv)*MaxConductance(iv)
            g_lai = g_lai + (sfr_surf(iv + 2)*(1 - SnowFrac(iv + 2)))*LAI_id(iv)/LAIMax(iv)*MaxConductance(iv)
         END DO

         IF (avkdn <= 0) THEN !At nighttime set gsc at arbitrary low value: gsc=0.1 mm/s (Shuttleworth, 1988b)
            gsc = gsc_min
         ELSE
            ! Multiply parts together
            gsc = MAX(gsc_min, G_max*g_kdown*g_dq*g_ta*g_smd*g_lai)

         END IF

         IF (gsc <= 0) THEN
            CALL errorHint(65, 'subroutine SurfaceResistance.f95 (gsModel=2): gsc <= 0, setting to 0.1 mm s-1', gsc, id_real, it)
            gsc = gsc_min
         END IF

      ELSEIF (gsModel < 1 .OR. gsModel > 4) THEN
         CALL errorHint(71, 'Value of gsModel not recognised.', notUsed, NotUsed, gsModel)
      END IF

      RS = 1./(gsc/1000.) ![s m-1]
      gfunc = g_dq*g_ta*g_smd*g_kdown

      RETURN
   END SUBROUTINE SurfaceResistance

   SUBROUTINE BoundaryLayerResistance( &
      zzd, & ! input:    !Active measurement height (meas. height-displac. height)
      z0m, & !Aerodynamic roughness length
      avU1, & !Average wind speed
      UStar, & ! input/output:
      rb) ! output:

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: zzd !Active measurement height (meas. height-displac. height)
      REAL(KIND(1D0)), INTENT(in) :: z0m !Aerodynamic roughness length
      REAL(KIND(1D0)), INTENT(in) :: avU1 !Average wind speed

      REAL(KIND(1D0)), INTENT(inout) :: UStar !Friction velocity

      REAL(KIND(1D0)), INTENT(out) :: rb !boundary layer resistance shuttleworth

      REAL(KIND(1D0)), PARAMETER :: k = 0.4

!       IF (UStar < 0.001) THEN
!          UStar = avu1/LOG(zzd/z0m)*k
!       END IF

      rb = (1.1/UStar) + (5.6*(UStar**0.333333)) !rb - boundary layer resistance shuttleworth

      RETURN
   END SUBROUTINE BoundaryLayerResistance

   SUBROUTINE SUEWS_cal_RoughnessParameters( &
      timer, config, forcing, siteInfo, & !input
      modState) ! input/output:
      ! phenState, &
      ! roughnessState)
      ! FAIBldg_use, FAIEveTree_use, FAIDecTree_use, & ! output:
      ! FAI, PAI, & ! output:
      ! Zh, z0m, zdm, ZZD)
      ! Get surface covers and frontal area fractions (LJ 11/2010)
      ! Last modified:
      ! TS  18 Sep 2017 - added explicit interface
      ! HCW 08 Feb 2017 - fixed bug in Zh between grids, added default z0m, zdm
      ! HCW 03 Mar 2015
      ! sg feb 2012 - made separate subroutine
      !--------------------------------------------------------------------------------

      USE SUEWS_DEF_DTS, ONLY: SUEWS_SITE, SUEWS_TIMER, SUEWS_CONFIG, SUEWS_FORCING, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               IRRIGATION_PRM, anthroEmis_STATE, &
                               HYDRO_STATE, PHENOLOGY_STATE, ROUGHNESS_STATE, SUEWS_STATE
      IMPLICIT NONE

      INTEGER, PARAMETER :: nsurf = 7 ! number of surface types
      INTEGER, PARAMETER :: PavSurf = 1 !When all surfaces considered together (1-7)
      INTEGER, PARAMETER :: BldgSurf = 2
      INTEGER, PARAMETER :: ConifSurf = 3
      INTEGER, PARAMETER :: DecidSurf = 4
      INTEGER, PARAMETER :: GrassSurf = 5 !New surface classes: Grass = 5th/7 surfaces
      INTEGER, PARAMETER :: BSoilSurf = 6 !New surface classes: Bare soil = 6th/7 surfaces
      INTEGER, PARAMETER :: WaterSurf = 7
      REAL(KIND(1D0)), PARAMETER :: porosity_evetr = 0.32 ! assumed porosity of evergreen trees, ref: Lai et al. (2022), http://dx.doi.org/10.2139/ssrn.4058842

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INout) :: modState
      ! INTEGER :: RoughLenMomMethod
      ! INTEGER :: FAImethod ! 0 = use FAI provided, 1 = use the simple scheme

      ! TYPE(LC_PAVED_PRM), INTENT(IN) :: pavedPrm
      ! TYPE(LC_BLDG_PRM), INTENT(IN) :: bldgPrm
      ! TYPE(LC_EVETR_PRM), INTENT(IN) :: evetrPrm
      ! TYPE(LC_DECTR_PRM), INTENT(IN) :: dectrPrm
      ! TYPE(LC_GRASS_PRM), INTENT(IN) :: grassPrm
      ! TYPE(LC_BSOIL_PRM), INTENT(IN) :: bsoilPrm
      ! TYPE(LC_WATER_PRM), INTENT(IN) :: waterPrm
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf ! surface fractions
      ! REAL(KIND(1D0)) :: bldgH
      ! REAL(KIND(1D0)) :: EveTreeH
      ! REAL(KIND(1D0)) :: DecTreeH
      ! REAL(KIND(1D0)) :: FAIBldg
      ! REAL(KIND(1D0)) :: FAIEveTree
      ! REAL(KIND(1D0)) :: FAIDecTree

      ! TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      ! REAL(KIND(1D0)) :: surfaceArea ! surface area of whole grid cell
      ! REAL(KIND(1D0)) :: z0m_in ! z0m set in SiteSelect
      ! REAL(KIND(1D0)) :: zdm_in ! zdm set in SiteSelect
      ! REAL(KIND(1D0)) :: Z

      ! TYPE(PHENOLOGY_STATE), INTENT(IN) :: phenState
      ! TYPE(ROUGHNESS_STATE), INTENT(OUT) :: roughnessState

      ! REAL(KIND(1D0)) :: porosity_dectr

      ! REAL(KIND(1D0)), INTENT(out) :: FAI
      ! REAL(KIND(1D0)), INTENT(out) :: PAI
      ! REAL(KIND(1D0)), INTENT(out) :: Zh ! effective height of bluff bodies
      ! REAL(KIND(1D0)), INTENT(out) :: z0m ! aerodynamic roughness length
      ! REAL(KIND(1D0)), INTENT(out) :: zdm ! zero-plance displacement
      ! REAL(KIND(1D0)), INTENT(out) :: ZZD ! z-zdm

      INTEGER, PARAMETER :: notUsedI = -55
      REAL(KIND(1D0)), PARAMETER :: notUsed = -55.5

      !Default values for roughness lengths [m]
      ! Set default values (using Moene & van Dam 2013, Atmos-Veg-Soil Interactions, Table 3.3)
      REAL(KIND(1D0)), PARAMETER :: z0m_Paved = 0.003 !estimate
      REAL(KIND(1D0)), PARAMETER :: z0m_Grass = 0.02
      REAL(KIND(1D0)), PARAMETER :: z0m_BSoil = 0.002
      REAL(KIND(1D0)), PARAMETER :: z0m_Water = 0.0005

      REAL(KIND(1D0)) :: z0m_zh ! z0m for roughness elements (i.e. zh>0)
      REAL(KIND(1D0)) :: zdm_zh ! zdm for roughness elements (i.e. zh>0)
      REAL(KIND(1D0)) :: z0m_zh0 ! z0m for non-roughness elements (i.e. zh=0)
      REAL(KIND(1D0)) :: zdm_zh0 ! zdm for non-roughness elements (i.e. zh=0)

      ! calculated values of FAI
      ! REAL(KIND(1D0)), INTENT(out) :: FAIBldg_use
      ! REAL(KIND(1D0)), INTENT(out) :: FAIEveTree_use
      ! REAL(KIND(1D0)), INTENT(out) :: FAIDecTree_use

      ASSOCIATE ( &
         phenState => modState%phenState, &
         roughnessState => modState%roughnessState, &
         surfacearea => siteInfo%surfacearea, &
         z0m_in => siteInfo%z0m_in, &
         zdm_in => siteInfo%zdm_in, &
         Z => siteInfo%Z, &
         pavedPrm => siteInfo%lc_paved, &
         bldgPrm => siteInfo%lc_bldg, &
         grassPrm => siteInfo%lc_grass, &
         dectrPrm => siteInfo%lc_dectr, &
         evetrPrm => siteInfo%lc_evetr, &
         bsoilPrm => siteInfo%lc_bsoil, &
         waterPrm => siteInfo%lc_water &
         )
         ASSOCIATE ( &
            porosity_dectr => phenState%porosity_id, &
            FAI => roughnessState%FAI, &
            PAI => roughnessState%PAI, &
            Zh => roughnessState%Zh, &
            z0m => roughnessState%z0m, &
            zdm => roughnessState%zdm, &
            ZZD => roughnessState%ZZD, &
            FAIBldg_use => roughnessState%FAIBldg_use, &
            FAIEveTree_use => roughnessState%FAIEveTree_use, &
            FAIDecTree_use => roughnessState%FAIDecTree_use, &
            RoughLenMomMethod => config%RoughLenMomMethod, &
            FAImethod => config%FAImethod, &
            sfr_surf => [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr], &
            bldgH => bldgPrm%bldgH, &
            EveTreeH => evetrPrm%EveTreeH, &
            DecTreeH => dectrPrm%DecTreeH, &
            FAIBldg => bldgPrm%FAIBldg, &
            FAIEveTree => evetrPrm%FAIEveTree, &
            FAIDecTree => dectrPrm%FAIDecTree &
            )

            ! RoughLenMomMethod = methodPrm%RoughLenMomMethod
            ! FAImethod = methodPrm%FAImethod

            ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
            ! bldgH = bldgPrm%bldgH
            ! EveTreeH = evetrPrm%EveTreeH
            ! DecTreeH = dectrPrm%DecTreeH
            ! FAIBldg = bldgPrm%FAIBldg
            ! FAIEveTree = evetrPrm%FAIEveTree
            ! FAIDecTree = dectrPrm%FAIDecTree

            ! surfacearea = siteInfo%surfacearea ! surface area of whole grid cell
            ! z0m_in = siteInfo%z0m_in
            ! zdm_in = siteInfo%zdm_in
            ! Z = siteInfo%Z

            ! porosity_dectr = phenState_prev%porosity_id

            !Total area of buildings and trees
            ! areaZh = (sfr_surf(BldgSurf) + sfr_surf(ConifSurf) + sfr_surf(DecidSurf))
            ! TS 19 Jun 2022: take porosity of trees into account; to be consistent with PAI calculation in RSL
            PAI = DOT_PRODUCT(sfr_surf([BldgSurf, ConifSurf, DecidSurf]), [1D0, 1 - porosity_evetr, 1 - porosity_dectr])

            ! z0m for non-roughness elements (i.e. zh=0)
            z0m_zh0 = (z0m_Paved*sfr_surf(PavSurf) &
                       + z0m_Grass*sfr_surf(GrassSurf) &
                       + z0m_BSoil*sfr_surf(BSoilSurf) &
                       + z0m_Water*sfr_surf(WaterSurf))/(1 - PAI)
            zdm_zh0 = 0

            !------------------------------------------------------------------------------
            !If total area of buildings and trees is larger than zero, use tree heights and building heights to calculate zH and FAI
            IF (PAI /= 0) THEN
               Zh = DOT_PRODUCT( &
                    [bldgH, EveTreeH*(1 - porosity_evetr), DecTreeH*(1 - porosity_dectr)], &
                    sfr_surf([BldgSurf, ConifSurf, DecidSurf]))/PAI
               IF (FAImethod == 0) THEN
                  ! use FAI provided
                  FAIBldg_use = FAIBldg
                  FAIEveTree_use = FAIEveTree*(1 - porosity_evetr)
                  FAIDecTree_use = FAIDecTree*(1 - porosity_dectr)

               ELSEIF (FAImethod == 1) THEN
                  ! use the simple scheme, details in #192

                  ! buildings
                  FAIBldg_use = SQRT(sfr_surf(BldgSurf)/surfaceArea)*bldgH

                  ! evergreen trees
                  FAIEveTree_use = 1.07*sfr_surf(ConifSurf)

                  ! deciduous trees
                  FAIDecTree_use = 1.66*(1 - porosity_dectr)*sfr_surf(DecidSurf)

               END IF
               FAI = SUM(MERGE([FAIBldg_use, FAIEveTree_use, FAIDecTree_use], &
                               [0D0, 0D0, 0D0], &
                               sfr_surf([BldgSurf, ConifSurf, DecidSurf]) > 0))

               ! `1e-5` set to avoid numerical difficulty
               FAI = MAX(FAI, 1E-5)

            ELSE
               Zh = 0 !Set Zh to zero if areaZh = 0
               FAI = 1E-5
            END IF

            IF (Zh /= 0) THEN
               !Calculate z0m and zdm depending on the Z0 method
               IF (RoughLenMomMethod == 2) THEN !Rule of thumb (G&O 1999)
                  z0m_zh = 0.1*Zh
                  zdm_zh = 0.7*Zh
               ELSEIF (RoughLenMomMethod == 3) THEN !MacDonald 1998
                  zdm_zh = (1 + 4.43**(-sfr_surf(BldgSurf))*(sfr_surf(BldgSurf) - 1))*Zh
                  z0m_zh = ((1 - zdm/Zh)*EXP(-(0.5*1.0*1.2/0.4**2*(1 - zdm/Zh)*FAI)**(-0.5)))*Zh
               ELSEIF (RoughLenMomMethod == 4) THEN ! lambdaP dependent as in Fig.1a of G&O (1999)
                  ! these are derived using digitalised points
                  zdm_zh = (-0.182 + 0.722*sigmoid(-1.16 + 3.89*PAI) + 0.493*sigmoid(-5.17 + 32.7*PAI))*Zh
                  z0m_zh = (0.00208 + &
                            0.0165*MIN(PAI, .7) + 2.52*MIN(PAI, .7)**2 + &
                            3.21*MIN(PAI, .7)**3 - 43.6*MIN(PAI, .7)**4 + &
                            76.5*MIN(PAI, .7)**5 - 40.*MIN(PAI, .7)**6)*Zh
               END IF
               ! #271: to smooth the z0m (and zdm) values when other non-rough surfaces are present
               z0m = DOT_PRODUCT([z0m_zh, z0m_zh0], [PAI, 1 - PAI])
               zdm = DOT_PRODUCT([zdm_zh, zdm_zh0], [PAI, 1 - PAI])

            ELSEIF (Zh == 0) THEN !If zh calculated to be zero, set default roughness length and displacement height
               IF (PAI /= 0) CALL ErrorHint(15, 'In SUEWS_RoughnessParameters.f95, zh = 0 m but areaZh > 0', zh, PAI, notUsedI)
               !Estimate z0 and zd using default values and surfaces that do not contribute to areaZh
               IF (PAI /= 1) THEN
                  z0m = z0m_zh0
                  zdm = zdm_zh0
                  CALL ErrorHint(15, 'Setting z0m and zdm using default values', z0m, zdm, notUsedI)
               ELSEIF (PAI == 1) THEN !If, for some reason, Zh = 0 and areaZh == 1, assume height of 10 m and use rule-of-thumb
                  z0m = 1
                  zdm = 7
                  CALL ErrorHint(15, 'Assuming mean height = 10 m, Setting z0m and zdm to default value', z0m, zdm, notUsedI)
               END IF
            END IF

            IF (RoughLenMomMethod == 1) THEN !use values set in SiteSelect
               z0m = z0m_in
               zdm = zdm_in
            END IF

            ZZD = Z - zdm

            ! Error messages if aerodynamic parameters negative
            IF (z0m < 0) CALL ErrorHint(14, 'In SUEWS_cal_RoughnessParameters, z0 < 0 m.', z0m, notUsed, notUsedI)
            IF (zdm < 0) CALL ErrorHint(14, 'In SUEWS_cal_RoughnessParameters, zd < 0 m.', zdm, notUsed, notUsedI)
            IF (zzd < 0) CALL ErrorHint(14, 'In SUEWS_cal_RoughnessParameters, (z-zd) < 0 m.', zzd, notUsed, notUsedI)

         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_cal_RoughnessParameters

   FUNCTION cal_z0V(RoughLenHeatMethod, z0m, VegFraction, UStar) RESULT(z0V)
      ! TS 31 Jul 2018: make this a separate funciton for reuse
      !Z0V roughness length for vapour
      IMPLICIT NONE
      INTEGER, INTENT(in) :: RoughLenHeatMethod
      REAL(KIND(1D0)), INTENT(in) :: z0m !Aerodynamic roughness length
      REAL(KIND(1D0)), INTENT(in) :: VegFraction !Fraction of vegetation
      REAL(KIND(1D0)), INTENT(in) :: UStar !Friction velocity

      REAL(KIND(1D0)) :: z0V !roughness length for vapor/heat

      REAL(KIND(1D0)), PARAMETER :: muu = 1.46E-5 !molecular viscosity

      z0V = 0.01 ! initialise as 0.01

      !Z0V roughness length for vapour
      IF (RoughLenHeatMethod == 1) THEN !Brutsaert (1982) Z0v=z0/10(see Grimmond & Oke, 1986)
         z0V = z0m/10
      ELSEIF (RoughLenHeatMethod == 2) THEN ! Kawai et al. (2007)
         !z0V=z0m*exp(2-(1.2-0.9*veg_fr**0.29)*(UStar*z0m/muu)**0.25)
         ! Changed by HCW 05 Nov 2015 (veg_fr includes water; VegFraction = veg + bare soil)
         z0V = z0m*EXP(2 - (1.2 - 0.9*VegFraction**0.29)*(UStar*z0m/muu)**0.25)
      ELSEIF (RoughLenHeatMethod == 3) THEN
         z0V = z0m*EXP(-20.) ! Voogt and Grimmond, JAM, 2000
      ELSEIF (RoughLenHeatMethod == 4) THEN
         z0V = z0m*EXP(2 - 1.29*(UStar*z0m/muu)**0.25) !See !Kanda and Moriwaki (2007),Loridan et al. (2010)
      ELSEIF (RoughLenHeatMethod == 5) THEN
         ! an adaptive way to determine z0v; TS 06 Feb 2020
         IF (VegFraction > .999) THEN
            ! fully pervious surface
            z0V = z0m/10
         ELSE
            ! impervious-pervious mixed surface
            z0V = z0m*EXP(2 - (1.2 - 0.9*VegFraction**0.29)*(UStar*z0m/muu)**0.25)
         END IF
      END IF

   END FUNCTION cal_z0v

   FUNCTION sigmoid(x) RESULT(res)
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(in) :: x
      REAL(KIND(1D0)) :: res

      res = 1/(1 + EXP(-x))

   END FUNCTION sigmoid

END MODULE suews_phys_resist

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE resist_module
   USE suews_phys_resist
END MODULE resist_module
