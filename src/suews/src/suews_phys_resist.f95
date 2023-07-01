MODULE resist_module
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
      IF (RA_h > 200) THEN !was 175
         CALL errorHint(7, 'In AerodynamicResistance.f95, calculated RA > 200 s m-1; RA set to 200 s m-1', RA_h, notUsed, notUsedI)
         RA_h = 200
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
      SMDMethod, SnowFrac, sfr_surf, avkdn, Temp_C, dq, xsmd, vsmd, MaxConductance, &
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
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !Air temperature
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
         IF (Temp_C <= tl) THEN
            g_ta = (tl + 0.1 - tl)*(th - (tl + 0.1))**tc/tc2
            !Call error only if no snow on ground
            !  IF (MIN(SnowFrac(1),SnowFrac(2),SnowFrac(3),SnowFrac(4),SnowFrac(5),SnowFrac(6))/=1) THEN
            IF (MINVAL(SnowFrac(1:6)) /= 1) THEN
               CALL errorHint(29, 'subroutine SurfaceResistance.f95: T changed to fit limits TL=0.1,Temp_c,id,it', &
                              REAL(Temp_c, KIND(1D0)), id_real, it)
            END IF
         ELSEIF (Temp_C >= th) THEN
            g_ta = ((th - 0.1) - tl)*(th - (th - 0.1))**tc/tc2
            CALL errorHint(29, 'subroutine SurfaceResistance.f95: T changed to fit limits TH=39.9,Temp_c,id,it', &
                           REAL(Temp_c, KIND(1D0)), id_real, it)
         ELSE
            g_ta = (Temp_C - tl)*(th - Temp_C)**tc/tc2
         END IF

         ! soil moisture deficit ----
         sdp = S1/G_sm + S2
         IF (SMDMethod > 0) THEN !Modified from ==1 to > 0 by HCW 31/07/2014
            g_smd = 1 - EXP(G_sm*(xsmd - sdp)) !Measured soil moisture deficit is used
         ELSE
            g_smd = 1 - EXP(G_sm*(vsmd - sdp)) !Modelled is used
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
            gsc = 0.1
         ELSE
            ! Multiply parts together
            gsc = (G_max*g_kdown*g_dq*g_ta*g_smd*g_lai)
         END IF

         IF (gsc <= 0) THEN
            CALL errorHint(65, 'subroutine SurfaceResistance.f95 (gsModel=1): gs <= 0, setting to 0.1 mm s-1', gsc, id_real, it)
            gsc = 0.1
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
         IF (Temp_C <= TL) THEN
            g_ta = (TL + 0.1 - TL)*(TH - (TL + 0.1))**Tc/Tc2
            ! Call error only if no snow on ground
            IF (MIN(SnowFrac(1), SnowFrac(2), SnowFrac(3), SnowFrac(4), SnowFrac(5), SnowFrac(6)) /= 1) THEN
               CALL errorHint(29, 'subroutine SurfaceResistance.f95: T changed to fit limits TL+0.1,Temp_C,id,it', &
                              REAL(Temp_c, KIND(1D0)), id_real, it)
            END IF
         ELSEIF (Temp_C >= TH) THEN
            g_ta = ((TH - 0.1) - TL)*(TH - (TH - 0.1))**Tc/Tc2
            CALL errorHint(29, 'subroutine SurfaceResistance.f95: T changed to fit limits TH-0.1,Temp_C,id,it', &
                           REAL(Temp_c, KIND(1D0)), id_real, it)
         ELSE
            g_ta = (Temp_C - TL)*(TH - Temp_C)**Tc/Tc2
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
            gsc = 0.1
         ELSE
            ! Multiply parts together
            gsc = (G_max*g_kdown*g_dq*g_ta*g_smd*g_lai)
         END IF

         IF (gsc <= 0) THEN
            CALL errorHint(65, 'subroutine SurfaceResistance.f95 (gsModel=2): gsc <= 0, setting to 0.1 mm s-1', gsc, id_real, it)
            gsc = 0.1
         END IF

      ELSEIF (gsModel < 1 .OR. gsModel > 4) THEN
         CALL errorHint(71, 'Value of gsModel not recognised.', notUsed, NotUsed, gsModel)
      END IF

      RS = 1/(gsc/1000) ![s m-1]
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

      IF (UStar < 0.01) THEN
         UStar = avu1/LOG(zzd/z0m)*k
      END IF

      rb = (1.1/UStar) + (5.6*(UStar**0.333333)) !rb - boundary layer resistance shuttleworth

      RETURN
   END SUBROUTINE BoundaryLayerResistance

   SUBROUTINE SUEWS_cal_RoughnessParameters( &
      RoughLenMomMethod, FAImethod, & ! input:
      sfr_surf, & ! surface fractions
      surfacearea, & ! surface area of whole grid cell
      bldgH, EveTreeH, DecTreeH, &
      porosity_dectr, FAIBldg, FAIEveTree, FAIDecTree, &
      z0m_in, zdm_in, Z, &
      FAI, PAI, & ! output:
      Zh, z0m, zdm, ZZD)
      ! Get surface covers and frontal area fractions (LJ 11/2010)
      ! Last modified:
      ! TS  18 Sep 2017 - added explicit interface
      ! HCW 08 Feb 2017 - fixed bug in Zh between grids, added default z0m, zdm
      ! HCW 03 Mar 2015
      ! sg feb 2012 - made separate subroutine
      !--------------------------------------------------------------------------------

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

      INTEGER, INTENT(in) :: RoughLenMomMethod
      INTEGER, INTENT(in) :: FAImethod ! 0 = use FAI provided, 1 = use the simple scheme

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf ! surface fractions

      REAL(KIND(1D0)), INTENT(in) :: surfaceArea ! surface area of whole grid cell
      REAL(KIND(1D0)), INTENT(in) :: bldgH
      REAL(KIND(1D0)), INTENT(in) :: EveTreeH
      REAL(KIND(1D0)), INTENT(in) :: DecTreeH
      REAL(KIND(1D0)), INTENT(in) :: porosity_dectr
      REAL(KIND(1D0)), INTENT(in) :: FAIBldg
      REAL(KIND(1D0)), INTENT(in) :: FAIEveTree
      REAL(KIND(1D0)), INTENT(in) :: FAIDecTree
      REAL(KIND(1D0)), INTENT(in) :: z0m_in ! z0m set in SiteSelect
      REAL(KIND(1D0)), INTENT(in) :: zdm_in ! zdm set in SiteSelect
      REAL(KIND(1D0)), INTENT(in) :: Z

      REAL(KIND(1D0)), INTENT(out) :: FAI
      REAL(KIND(1D0)), INTENT(out) :: PAI
      REAL(KIND(1D0)), INTENT(out) :: Zh ! effective height of bluff bodies
      REAL(KIND(1D0)), INTENT(out) :: z0m ! aerodynamic roughness length
      REAL(KIND(1D0)), INTENT(out) :: zdm ! zero-plance displacement
      REAL(KIND(1D0)), INTENT(out) :: ZZD ! z-zdm

      INTEGER, PARAMETER :: notUsedI = -55
      REAL(KIND(1D0)), PARAMETER :: notUsed = -55.5
      REAL(KIND(1D0)) :: z0m4Paved, z0m4Grass, z0m4BSoil, z0m4Water !Default values for roughness lengths [m]

      ! calculated values of FAI
      REAL(KIND(1D0)) :: FAIBldg_use
      REAL(KIND(1D0)) :: FAIEveTree_use
      REAL(KIND(1D0)) :: FAIDecTree_use

      !Total area of buildings and trees
      ! areaZh = (sfr_surf(BldgSurf) + sfr_surf(ConifSurf) + sfr_surf(DecidSurf))
      ! TS 19 Jun 2022: take porosity of trees into account; to be consistent with PAI calculation in RSL
      PAI = DOT_PRODUCT(sfr_surf([BldgSurf, ConifSurf, DecidSurf]), [1D0, 1 - porosity_evetr, 1 - porosity_dectr])

      ! Set default values (using Moene & van Dam 2013, Atmos-Veg-Soil Interactions, Table 3.3)
      Z0m4Paved = 0.003 !estimate
      Z0m4Grass = 0.02
      Z0m4BSoil = 0.002
      Z0m4Water = 0.0005

      !------------------------------------------------------------------------------
      !If total area of buildings and trees is larger than zero, use tree heights and building heights to calculate zH and FAI
      IF (PAI > 0.02) THEN ! 0.02 is a threshold value, below which we assume no buildings and trees, TS 06 Jun 2023
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
            z0m = 0.1*Zh
            zdm = 0.7*Zh
         ELSEIF (RoughLenMomMethod == 3) THEN !MacDonald 1998
            ! zdm = (1 + 4.43**(-sfr_surf(BldgSurf))*(sfr_surf(BldgSurf) - 1))*Zh
            zdm = (1 + 4.43**(-PAI)*(PAI - 1))*Zh ! changed sfr_surf(BldgSurf) to PAI to be consistent with z0m calculation, TS 04 Jun 2023
            z0m = ((1 - zdm/Zh)*EXP(-(0.5*1.0*1.2/0.4**2*(1 - zdm/Zh)*FAI)**(-0.5)))*Zh
         ELSEIF (RoughLenMomMethod == 4) THEN ! lambdaP dependent as in Fig.1a of G&O (1999)
            ! these are derived using digitalised points
            zdm = (-0.182 + 0.722*sigmoid(-1.16 + 3.89*PAI) + 0.493*sigmoid(-5.17 + 32.7*PAI))*Zh
            z0m = (0.00208 + &
                   0.0165*MIN(PAI, .7) + 2.52*MIN(PAI, .7)**2 + &
                   3.21*MIN(PAI, .7)**3 - 43.6*MIN(PAI, .7)**4 + &
                   76.5*MIN(PAI, .7)**5 - 40.*MIN(PAI, .7)**6)*Zh
         END IF
      ELSEIF (Zh == 0) THEN !If zh calculated to be zero, set default roughness length and displacement height
         IF (PAI /= 0) CALL ErrorHint(15, 'In SUEWS_RoughnessParameters.f95, zh = 0 m but areaZh > 0', zh, PAI, notUsedI)
         !Estimate z0 and zd using default values and surfaces that do not contribute to areaZh
         IF (PAI /= 1) THEN
            z0m = (z0m4Paved*sfr_surf(PavSurf) &
                   + z0m4Grass*sfr_surf(GrassSurf) &
                   + z0m4BSoil*sfr_surf(BSoilSurf) &
                   + z0m4Water*sfr_surf(WaterSurf))/(1 - PAI)
            zdm = 0
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
   END SUBROUTINE SUEWS_cal_RoughnessParameters

   SUBROUTINE SUEWS_cal_RoughnessParameters_DTS( &
      RoughLenMomMethod, FAImethod, & ! input:
      sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, & ! surface fractions
      surfacearea, & ! surface area of whole grid cell
      bldgH, EveTreeH, DecTreeH, &
      porosity_dectr, FAIBldg, FAIEveTree, FAIDecTree, &
      z0m_in, zdm_in, Z, &
      FAIBldg_use, FAIEveTree_use, FAIDecTree_use, & ! output:
      FAI, PAI, & ! output:
      Zh, z0m, zdm, ZZD)
      ! Get surface covers and frontal area fractions (LJ 11/2010)
      ! Last modified:
      ! TS  18 Sep 2017 - added explicit interface
      ! HCW 08 Feb 2017 - fixed bug in Zh between grids, added default z0m, zdm
      ! HCW 03 Mar 2015
      ! sg feb 2012 - made separate subroutine
      !--------------------------------------------------------------------------------

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

      INTEGER, INTENT(in) :: RoughLenMomMethod
      INTEGER, INTENT(in) :: FAImethod ! 0 = use FAI provided, 1 = use the simple scheme

      REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
      REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
      REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
      REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: sfr_water
      REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf ! surface fractions

      REAL(KIND(1D0)), INTENT(in) :: surfaceArea ! surface area of whole grid cell
      REAL(KIND(1D0)), INTENT(in) :: bldgH
      REAL(KIND(1D0)), INTENT(in) :: EveTreeH
      REAL(KIND(1D0)), INTENT(in) :: DecTreeH
      REAL(KIND(1D0)), INTENT(in) :: porosity_dectr
      REAL(KIND(1D0)), INTENT(in) :: FAIBldg
      REAL(KIND(1D0)), INTENT(in) :: FAIEveTree
      REAL(KIND(1D0)), INTENT(in) :: FAIDecTree
      REAL(KIND(1D0)), INTENT(in) :: z0m_in ! z0m set in SiteSelect
      REAL(KIND(1D0)), INTENT(in) :: zdm_in ! zdm set in SiteSelect
      REAL(KIND(1D0)), INTENT(in) :: Z

      REAL(KIND(1D0)), INTENT(out) :: FAI
      REAL(KIND(1D0)), INTENT(out) :: PAI
      REAL(KIND(1D0)), INTENT(out) :: Zh ! effective height of bluff bodies
      REAL(KIND(1D0)), INTENT(out) :: z0m ! aerodynamic roughness length
      REAL(KIND(1D0)), INTENT(out) :: zdm ! zero-plance displacement
      REAL(KIND(1D0)), INTENT(out) :: ZZD ! z-zdm

      INTEGER, PARAMETER :: notUsedI = -55
      REAL(KIND(1D0)), PARAMETER :: notUsed = -55.5
      REAL(KIND(1D0)) :: z0m4Paved, z0m4Grass, z0m4BSoil, z0m4Water !Default values for roughness lengths [m]

      ! calculated values of FAI
      REAL(KIND(1D0)), INTENT(out) :: FAIBldg_use
      REAL(KIND(1D0)), INTENT(out) :: FAIEveTree_use
      REAL(KIND(1D0)), INTENT(out) :: FAIDecTree_use

      sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]

      !Total area of buildings and trees
      ! areaZh = (sfr_surf(BldgSurf) + sfr_surf(ConifSurf) + sfr_surf(DecidSurf))
      ! TS 19 Jun 2022: take porosity of trees into account; to be consistent with PAI calculation in RSL
      PAI = DOT_PRODUCT(sfr_surf([BldgSurf, ConifSurf, DecidSurf]), [1D0, 1 - porosity_evetr, 1 - porosity_dectr])

      ! Set default values (using Moene & van Dam 2013, Atmos-Veg-Soil Interactions, Table 3.3)
      Z0m4Paved = 0.003 !estimate
      Z0m4Grass = 0.02
      Z0m4BSoil = 0.002
      Z0m4Water = 0.0005

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
            z0m = 0.1*Zh
            zdm = 0.7*Zh
         ELSEIF (RoughLenMomMethod == 3) THEN !MacDonald 1998
            zdm = (1 + 4.43**(-sfr_surf(BldgSurf))*(sfr_surf(BldgSurf) - 1))*Zh
            z0m = ((1 - zdm/Zh)*EXP(-(0.5*1.0*1.2/0.4**2*(1 - zdm/Zh)*FAI)**(-0.5)))*Zh
         ELSEIF (RoughLenMomMethod == 4) THEN ! lambdaP dependent as in Fig.1a of G&O (1999)
            ! these are derived using digitalised points
            zdm = (-0.182 + 0.722*sigmoid(-1.16 + 3.89*PAI) + 0.493*sigmoid(-5.17 + 32.7*PAI))*Zh
            z0m = (0.00208 + &
                   0.0165*MIN(PAI, .7) + 2.52*MIN(PAI, .7)**2 + &
                   3.21*MIN(PAI, .7)**3 - 43.6*MIN(PAI, .7)**4 + &
                   76.5*MIN(PAI, .7)**5 - 40.*MIN(PAI, .7)**6)*Zh
         END IF
      ELSEIF (Zh == 0) THEN !If zh calculated to be zero, set default roughness length and displacement height
         IF (PAI /= 0) CALL ErrorHint(15, 'In SUEWS_RoughnessParameters.f95, zh = 0 m but areaZh > 0', zh, PAI, notUsedI)
         !Estimate z0 and zd using default values and surfaces that do not contribute to areaZh
         IF (PAI /= 1) THEN
            z0m = (z0m4Paved*sfr_surf(PavSurf) &
                   + z0m4Grass*sfr_surf(GrassSurf) &
                   + z0m4BSoil*sfr_surf(BSoilSurf) &
                   + z0m4Water*sfr_surf(WaterSurf))/(1 - PAI)
            zdm = 0
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
   END SUBROUTINE SUEWS_cal_RoughnessParameters_DTS

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

END MODULE resist_module
