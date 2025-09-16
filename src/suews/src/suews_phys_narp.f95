MODULE NARP_MODULE
   !==============================================================================
   !NET ALL WAVE RADIATION PARAMETERIZATION ROUTINES
   !B. OFFERLE
   !DEPT OF GEOGRAPHY
   !INDIANA UNIVERSITY
   !bofferle@indiana.edu
   !
   !MODIFIED: 19 DEC 2002
   !CURRENTLY THE SMITH GRID IS ONLY VALID FOR THE N. HEMISPHERE
   !
   !Thomas Loridan, May 2008
   !4.1: MODIFICATION FOR CLOUD FRACTION PARAMETERIZATION AT NIGHT USING THE RATE OF COOLING.
   !     EOSHIFT INTRINSIC FUNCTION WAS ALSO REMOVED BECAUSE IT IS COMPILER DEPENDENT.
   !
   !6.0  T. Loridan - June 2009
   !     Different longwave down options (ldown_option)
   ! 1 - LDOWN Observed (add data as last column in met file)
   ! 2 - LDOWN modelled from observed FCLD (add data as last column in met file)
   ! 3 - LDOWN modelled from FCLD(RH,TA)
   ! 4 - LDOWN modelled from FCLD(Kdown); i.e. day FCLD only
   !     cloud fraction is kept constant throught the night (Offerle et al. 2003, JAM)
   ! 5 - Option 3 at night and 4 during the day (might cause discontinuities in LDOWN)

   !SUEWS   L. Jarvi - Oct 2010
   !Currently LDOWN options 4 and 5 commented out in order to reduce input files.
   !Last modified:
   ! TS 06 Aug 2017 - interface modified to receive explict input and output arguments
   ! LJ 27 Jan 2016 - Removal of tabs, cleaning of the code
   ! FL July 2014 - Variables are moved to modules in NARP subroutine. Snow related should also in future.
   ! FL Nov 2013 - A new sun postion algorithm added
   ! LJ May 2013 - Main program NARP changed to take subsurfaces and snow into account here and not
   ! in the main program
   ! LJ Oct 2012 - Zenith angle change in the calculation of albedo added
   ! sg feb 2012 - Allocatable array module added

   !==============================================================================================
   ! USE allocateArray
   USE time_module, ONLY: day2month, dectime_to_timevec

   IMPLICIT NONE

CONTAINS
   !==============================================================================
   SUBROUTINE RadMethod( &
      NetRadiationMethod, & !input
      SnowUse, & !input
      NetRadiationMethod_use, AlbedoChoice, ldown_option) !output
      IMPLICIT NONE
      INTEGER, INTENT(in) :: NetRadiationMethod ! the one from RunControl setting
      INTEGER, INTENT(in) :: SnowUse
      INTEGER, INTENT(out) :: NetRadiationMethod_use ! processed NetRadiationMethod to be used for other radiation calculations
      INTEGER, INTENT(out) :: AlbedoChoice, ldown_option
      !Determine what should be done with respect to radiation
      AlbedoChoice = 0
      ldown_option = 0
      IF (NetRadiationMethod == 0) THEN !Observed Q* from the met input file will be used
         NetRadiationMethod_use = 0
         !  ldown_option is not required if NetRadiationMethodX=0 as LDOWN calculations are skipped

         IF (SnowUse == 1) THEN !If snow is modelled, NARP is needed for surface temperature
            ! NetRadiationMethod=3000
            NetRadiationMethod_use = 3000
            ldown_option = 3 !LDOWN will be modelled
            !NetRadiationMethod=NetRadiationMethod/1000
         END IF

      ELSEIF (NetRadiationMethod > 0) THEN !Modelled Q* is used (NARP)
         AlbedoChoice = -9
         IF (NetRadiationMethod < 100) THEN
            AlbedoChoice = 0
            ! after the introduction of iteration-based tsurf, TS 20 Sep 2019
            NetRadiationMethod_use = MOD(NetRadiationMethod, 10)
            IF (NetRadiationMethod_use == 1) ldown_option = 1
            IF (NetRadiationMethod_use == 2) ldown_option = 2
            IF (NetRadiationMethod_use == 3) ldown_option = 3
            ! recover values before modulus calculation
            NetRadiationMethod_use = NetRadiationMethod

            ! prior to introduction of iteration-based tsurf, TS 20 Sep 2019
            ! IF (NetRadiationMethod == 1) ldown_option = 1
            ! IF (NetRadiationMethod == 2) ldown_option = 2
            ! IF (NetRadiationMethod == 3) ldown_option = 3
            ! NetRadiationMethod_use = NetRadiationMethod

         ELSEIF (NetRadiationMethod >= 100 .AND. NetRadiationMethod < 1000) THEN
            AlbedoChoice = 1
            IF (NetRadiationMethod == 100) ldown_option = 1
            IF (NetRadiationMethod == 200) ldown_option = 2
            IF (NetRadiationMethod == 300) ldown_option = 3
            NetRadiationMethod_use = NetRadiationMethod/100

            !choose Ldown method for Spartacus
         ELSEIF (NetRadiationMethod > 1000) THEN
            AlbedoChoice = 0
            NetRadiationMethod_use = MOD(NetRadiationMethod, 10)
            IF (NetRadiationMethod_use == 1) ldown_option = 1
            IF (NetRadiationMethod_use == 2) ldown_option = 2
            IF (NetRadiationMethod_use == 3) ldown_option = 3
            ! recover values before modulus calculation
            NetRadiationMethod_use = NetRadiationMethod
            !If bad NetRadiationMethod value
            IF (MOD(NetRadiationMethod, 10) > 3 .OR. AlbedoChoice == -9) THEN
               WRITE (*, *) 'NetRadiationMethod=', NetRadiationMethod_use
               WRITE (*, *) 'Value not usable'
               STOP
            END IF

         END IF

         !If bad NetRadiationMethod value
         IF (MOD(NetRadiationMethod, 10) > 3 .OR. AlbedoChoice == -9) THEN
            WRITE (*, *) 'NetRadiationMethod=', NetRadiationMethod_use
            WRITE (*, *) 'Value not usable'
            STOP
         END IF
      END IF

   END SUBROUTINE RadMethod

   !==============================================================================
   SUBROUTINE NARP( &
      nsurf, sfr_surf, tsfc_surf, SnowFrac, alb, emis, IceFrac, & ! input:
      NARP_TRANS_SITE, NARP_EMIS_SNOW, &
      DTIME, ZENITH_deg, kdown, Temp_C, RH, Press_hPa, qn1_obs, ldown_obs, &
      SnowAlb, &
      AlbedoChoice, ldown_option, &
      NetRadiationMethod_use, DiagQN, &
      qn_surf, &
      QSTARall, QSTAR_SF, QSTAR_S, kclear, KUPall, LDOWN, LUPall, fcld, TSURFall, & ! output:
      qn1_ind_snow, kup_ind_snow, Tsurf_ind_snow, Tsurf_surf, &
      albedo_snowfree)
      !KCLEAR,FCLD,DTIME,KDOWN,QSTARall,KUPall,LDOWN,LUPall,TSURFall,&
      !AlbedoChoice,ldown_option,Temp_C,Press_hPa,Ea_hPa,qn1_obs,RH,&
      !,zenith_degnetRadiationChoice,

      !SUBROUTINE NARP(QSTAR,DTIME,KDOWN,LDOWN,T,RH,PRESS,FCLD,SNOW)
      !returns estimate of Q* given the meteorological fields and prior
      !configuration call.

      !OUTPUT FIELDS
      !QSTARall (W/m2) = net all wave radiation
      !KCLEAR          = clear sky incoming solar radiation
      !KUPall          = reflect solar radiation
      !LDOWN           = incoming longwave radiation (observed or modelled depending on ldown_option)
      !LUPall          = outgoing longwaver radiation
      !FCLD            = estimated cloud fraction (used only for emissivity estimate)
      !                  FCLD USED AS INPUT ALSO
      !TSURFall (DEG C)= estimated surface temperature
      !QSTAR_SF        = net all wave radiation for snow free surface
      !QSTAR_S         = net all wave radiation for SnowPack

      !INPUT FIELDS
      !DTIME (days) = local time, not daylight savings
      !KDOWN (W/m2) = incoming solar radiation
      !T (DEG C)    = air temperature near model height
      !RH (%)       = relative humidity near model height
      !PRESS (mb)   = station pressure, use estimate if unavailable
      !qn1_obs      = Observed Q*
      !ldown_obs      = Observed Ldown

      !INTERNAL FIELDS
      ! TemP_K = air temperature in K
      ! ES_hPs = saturation vapor pressure (hPa)
      ! EA_Pa = vapor pressure (hPa)
      ! TD = dewpoint (C)
      ! ZENITH = solar zenith angle
      ! albedo_snowfree = surface albedo
      ! EMIS0 = surface emissivity
      ! EMIS_A = atmospheric emissivity
      ! TRANS = atmospheric transmissivity
      ! LUPCORR = correction for surface heating by kdown (W/m2)
      ! SIGMATK4 = energy flux density
      ! KDOWN_HR = hourly average insolation
      ! DOY = day of year

      !Modified by LJ to calcuate snow free and SnowPack components (May 2013)
      !Modified to define variables in data_in module
      !-------------------------------------------------------------------------------
      ! USE allocateArray
      ! use gis_data
      ! use data_in ! Included 20140701, FL
      ! use moist   ! Included 20140701, FL
      ! use time    ! Included 20140701, FL
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: alb
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: emis
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: IceFrac
      ! REAL(KIND(1D0)),DIMENSION(365),INTENT(in) ::NARP_G

      REAL(KIND(1D0)), INTENT(in) :: DTIME
      REAL(KIND(1D0)), INTENT(in) :: ZENITH_deg
      ! REAL(KIND(1D0)), INTENT(in) :: tsurf_0
      REAL(KIND(1D0)), INTENT(in) :: kdown
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: RH
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa
      REAL(KIND(1D0)), INTENT(in) :: qn1_obs
      REAL(KIND(1D0)), INTENT(in) :: ldown_obs
      REAL(KIND(1D0)), INTENT(in) :: SnowAlb
      REAL(KIND(1D0)), INTENT(in) :: NARP_TRANS_SITE
      REAL(KIND(1D0)), INTENT(in) :: NARP_EMIS_SNOW

      INTEGER, INTENT(in) :: nsurf
      INTEGER, INTENT(in) :: NetRadiationMethod_use ! the one processed by RadMethod
      ! INTEGER, INTENT(in) :: storageheatmethod ! needed for separate surface temperatures
      INTEGER, INTENT(in) :: AlbedoChoice ! flag if correction to albedo of snow cover should be applied
      INTEGER, INTENT(in) :: ldown_option ! flag for different ldown modelling options; 1 for obs; see code below for other parameterisations
      INTEGER, INTENT(in) :: DiagQN

      REAL(KIND(1D0)), INTENT(out) :: QSTARall
      REAL(KIND(1D0)), INTENT(out) :: QSTAR_SF
      REAL(KIND(1D0)), INTENT(out) :: QSTAR_S
      REAL(KIND(1D0)), INTENT(out) :: kclear
      REAL(KIND(1D0)), INTENT(out) :: KUPall
      REAL(KIND(1D0)), INTENT(out) :: LDOWN
      REAL(KIND(1D0)), INTENT(out) :: LUPall
      REAL(KIND(1D0)), INTENT(out) :: fcld
      REAL(KIND(1D0)), INTENT(out) :: TSURFall

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qn1_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: kup_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Tsurf_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Tsurf_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: qn_surf

      REAL(KIND(1D0)), INTENT(out) :: albedo_snowfree
      REAL(KIND(1D0)) :: albedo_snow

      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_surf_K
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_surf
      REAL(KIND(1D0)), DIMENSION(nsurf) :: lup_surf

      REAL(KIND(1D0)), DIMENSION(nsurf) :: qn1_ind_nosnow
      REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind_nosnow
      REAL(KIND(1D0)), DIMENSION(nsurf) :: lup_ind_nosnow
      REAL(KIND(1D0)), DIMENSION(nsurf) :: Tsurf_ind_nosnow
      REAL(KIND(1D0)), DIMENSION(nsurf) :: lup_ind_snow

      REAL(KIND(1D0)) :: tsurf_0_K
      REAL(KIND(1D0)) :: Temp_K, TD, ZENITH, QSTAR, QSTAR_SNOW, KUP_SNOW, LUP_SNOW
      REAL(KIND(1D0)) :: TSURF_SNOW_K, TSURF_SNOW_C, KUP, LUP, TSURF_K, TSURF_C
      REAL(KIND(1D0)) :: EMIS0, EMIS_A, TRANS !,RH,DTIME,KDOWN
      REAL(KIND(1D0)) :: LUPCORR, SIGMATK4, KDOWN_HR
      INTEGER :: DOY, is

      REAL(KIND(1D0)) :: qn1_cum, kup_cum, lup_cum, tsurf_cum, & !Cumulative radiation components
                         qn_is, kup_is, lup_is, tsurf_is, & !Sub-surface radiation components
                         SF_all

      REAL(KIND(1D0)), PARAMETER :: DEG2RAD = 0.017453292
      ! REAL(KIND(1D0)),PARAMETER   ::RAD2DEG=57.29577951
      REAL(KIND(1D0)), PARAMETER :: SIGMA_SB = 5.67E-8

      ! NB: NARP_G is not assigned with a value in SUEWS_translate.
      ! 3.0 is used here as annual average for mid-latitude areas. TS 24 Oct 2017
      REAL(KIND(1D0)), DIMENSION(365), PARAMETER :: NARP_G = 3.0
      !Initialize variables
      ! RH=avrh
      ! DTIME=dectime
      ! KDOWN=avkdn
      KDOWN_HR = 0.
      tsfc_surf_K = tsfc_surf + 273.16
      ! tsurf_0_K = tsurf_0 + 273.16
      Temp_K = Temp_C + 273.16
      SIGMATK4 = SIGMA_SB*Temp_K**4
      TD = dewpoint_narp(Temp_C, RH)
      ! Sun postition is now calculated in the main loop, FL
      !ZENITH=SOLAR_ZENITH(NARP_LAT,NARP_LONG,NARP_TZ,DTIME)
      !call NARP_cal_SunPosition(NARP_YEAR,DTIME,NARP_TZ,NARP_LAT,NARP_LONG,Alt,AZIMUTH,ZENITH)
      ZENITH = ZENITH_deg*DEG2RAD
      DOY = INT(DTIME)
      IF (DOY == 366) doy = 365

      !===================================================================================
      !Calculate radiation for each sub-surface

      ! initialisation
      qn1_cum = 0
      kup_cum = 0
      lup_cum = 0
      tsurf_cum = 0

      QSTARall = 0
      QSTAR_SF = 0
      QSTAR_S = 0
      kclear = 0
      KUPall = 0
      IF (ldown_option .NE. 1) THEN
         LDOWN = 0
      END IF
      LUPall = 0
      fcld = 0
      TSURFall = 0

      qn1_ind_snow = 0
      kup_ind_snow = 0
      lup_ind_snow = 0
      Tsurf_ind_snow = 0
      Tsurf_surf = 0
      albedo_snowfree = 0.2 ! arbitrary non-zero value for initialisatoin
      albedo_snow = SnowAlb

      IF (ldown_option == 1) THEN !Observed ldown provided as forcing
         ldown = ldown_obs
      ELSE
         ldown = 0 !to be filled in NARP
      END IF

      !Total snowfree surface fraction
      SF_all = 0
      DO is = 1, nsurf
         IF (sfr_surf(is) /= 0) SF_all = SF_all + sfr_surf(is)*(1 - SnowFrac(is))
      END DO

      !looping over the surfaces
      DO is = 1, nsurf
         IF (DiagQN == 1) WRITE (*, *) 'is ', is

         EMIS_A = PRATA_EMIS(Temp_K, Press_hPa)

         !--------------------------------------------------
         !-------SNOW FREE SURFACE--------------------------

         !NARP
         IF (AlbedoChoice == 1 .AND. 180*ZENITH/ACOS(0.0) < 90) THEN
            albedo_snowfree = ALB(is) + 0.5E-16*(180*ZENITH/ACOS(0.0))**8 !AIDA 1982
         ELSE
            albedo_snowfree = ALB(is)
         END IF
         EMIS0 = EMIS(is)

         !Downward longwave radiation
         IF ((ldown_option == 4) .OR. (ldown_option == 5)) THEN !Estimate FCLD from Kdown (Offerle et al. 2003)
            IF (ZENITH < 1.5) THEN !DAYTIME CALCULATIONS
               TRANS = TRANSMISSIVITY(Press_hPa, TD, NARP_G(DOY), ZENITH)
               KCLEAR = ISURFACE(DOY, ZENITH)*TRANS*NARP_TRANS_SITE
               IF (KCLEAR > 50.) THEN
                  FCLD = CLOUD_FRACTION(KDOWN, KCLEAR)
                  EMIS_A = EMIS_CLOUD_SQ(EMIS_A, FCLD)
               ELSE
                  IF (ldown_option == 5) THEN ! Use RH when Kdown can not be used
                     FCLD = WC_fraction(RH, Temp_C)
                     EMIS_A = EMIS_CLOUD(EMIS_A, FCLD)
                  ELSE
                     !FCLD is left to the latest calculable value
                     EMIS_A = EMIS_CLOUD_SQ(EMIS_A, FCLD)
                  END IF
               END IF
            ELSE !NIGHT TIME CALCULATIONS
               IF (ldown_option == 4) THEN
                  !FCLD is left to the latest calculable value
                  EMIS_A = EMIS_CLOUD_SQ(EMIS_A, FCLD)
               ELSEIF ((ldown_option == 5)) THEN ! Use RH
                  FCLD = WC_fraction(RH, Temp_C)
                  EMIS_A = EMIS_CLOUD(EMIS_A, FCLD)
               END IF
            END IF
         ELSEIF (ldown_option == 3) THEN !Always use RH
            FCLD = WC_fraction(RH, Temp_C)
            EMIS_A = EMIS_CLOUD(EMIS_A, FCLD)
         ELSEIF (ldown_option == 2) THEN ! FCLD obs, came from input
            EMIS_A = EMIS_CLOUD(EMIS_A, FCLD)
         END IF
         IF (DiagQN == 1) WRITE (*, *) 'ldown_option: ', ldown_option, 'FCLD:', FCLD

         IF (ldown_option > 1) THEN ! Forcing available if ldown_option=1, model otherwise
            LDOWN = EMIS_A*SIGMATK4
            IF (DiagQN == 1) WRITE (*, *) 'EMIS_A: ', EMIS_A, 'SIGMATK4:', SIGMATK4, 'LDOWN: ', LDOWN
         END IF

         !Upward longwave radiation
         !NARP
         !Note that this is not averaged over the hour for cases where time step < 1hr
         KDOWN_HR = KDOWN
         !calculate Lup correction using Eq. 15 of Offerle et al.
         IF (KDOWN_HR > 0) THEN
            LUPCORR = (1 - albedo_snowfree)*(0.08*KDOWN_HR)
         ELSE
            LUPCORR = 0.
         END IF
         !calculate Lup
         IF (NetRadiationMethod_use < 10) THEN
            ! NARP method
            TSURF_K = ((EMIS0*SIGMATK4 + LUPCORR)/(EMIS0*SIGMA_SB))**0.25 !Eqs. (14) and (15),
            LUP = EMIS0*SIGMATK4 + LUPCORR + (1 - EMIS0)*LDOWN !Eq (16) in Offerle et al. (2002)
         ELSE
            ! use iteration-based approach to calculate LUP and also TSURF; TS 20 Sep 2019
            ! IF (storageheatmethod == 5) THEN
            TSURF_K = tsfc_surf_K(is)
            ! ELSE
            !    TSURF_K = tsurf_0_K
            ! END IF
            ! TSURF = tsfc_surf_K(is)
            LUP = EMIS0*SIGMA_SB*TSURF_K**4 + (1 - EMIS0)*LDOWN
         END IF

         !Upward shortwave radiation of the snowfree surface fractions
         KUP = albedo_snowfree*KDOWN

         !Net all wave
         QSTAR = KDOWN - KUP + LDOWN - LUP
         TSURF_C = TSURF_K - 273.16

         !Define sub-surface radiation components
         qn1_ind_nosnow(is) = QSTAR
         kup_ind_nosnow(is) = KUP
         lup_ind_nosnow(is) = LUP
         Tsurf_ind_nosnow(is) = TSURF_C

         !!!!!!!!!!!!!!!! snow section !!!!!!!!!!!!!!!!!!!!!
         !======================================================================
         !Snow related parameters if snow pack existing
         IF (SnowFrac(is) > 0) THEN
            IF (AlbedoChoice == 1 .AND. 180*ZENITH/ACOS(0.0) < 90) THEN
               albedo_snow = SnowAlb + 0.5E-16*(180*ZENITH/ACOS(0.0))**8 !AIDA 1982
            ELSE
               albedo_snow = SnowAlb
            END IF

            KUP_SNOW = (albedo_snow*(SnowFrac(is) - SnowFrac(is)*IceFrac(is)) + albedo_snowfree*SnowFrac(is)*IceFrac(is))*KDOWN

            IF (NetRadiationMethod_use < 10) THEN
               ! NARP method
               TSURF_SNOW_K = ((NARP_EMIS_SNOW*SIGMATK4)/(NARP_EMIS_SNOW*SIGMA_SB))**0.25 !Snow surface temperature
               !IF (TSURF_SNOW>273.16) TSURF_SNOW=min(273.16,Temp_K)!Set this to 2 degrees (melted water on top)
               !open(34,file='TestingSnowFrac.txt',position='append')
               !write(34,*) dectime,is,albedo_snow,albedo_snowfree,SnowFrac(is),IceFrac(is),KDOWN,KUP_snow
               !close(34)
               LUP_SNOW = NARP_EMIS_SNOW*SIGMA_SB*TSURF_SNOW_K**4 + (1 - NARP_EMIS_SNOW)*LDOWN
            ELSE
               ! use iteration-based approach to calculate LUP and also TSURF; TS 20 Sep 2019
               TSURF_SNOW_K = tsurf_0_K
               LUP_SNOW = NARP_EMIS_SNOW*SIGMA_SB*TSURF_SNOW_K**4 + (1 - NARP_EMIS_SNOW)*LDOWN
            END IF

            QSTAR_SNOW = KDOWN - KUP_SNOW + LDOWN - LUP_SNOW
            TSURF_SNOW_C = TSURF_SNOW_K - 273.16

         ELSE
            KUP_SNOW = 0
            LUP_SNOW = 0
            TSURF_SNOW_C = 0
            QSTAR_SNOW = 0
            TSURF_SNOW_K = 0
            !QSTAR_ICE = 0
            !KUP_ICE = 0
         END IF

         !Define snow sub-surface radiation components
         qn1_ind_snow(is) = QSTAR_SNOW
         kup_ind_snow(is) = KUP_SNOW
         lup_ind_snow(is) = LUP_SNOW
         Tsurf_ind_snow(is) = TSURF_SNOW_K

         IF (SF_all /= 0) THEN
            QSTAR_SF = QSTAR_SF + QSTAR*sfr_surf(is)*(1 - SnowFrac(is))/SF_all
         ELSE
            QSTAR_SF = QSTAR_SF + QSTAR*sfr_surf(is)*(1 - SnowFrac(is))
         END IF

         IF ((1 - SF_all) /= 0) THEN
            QSTAR_S = QSTAR_S + QSTAR_SNOW*sfr_surf(is)*SnowFrac(is)/(1 - SF_all)
         ELSE
            QSTAR_S = QSTAR_S + QSTAR_SNOW*sfr_surf(is)*SnowFrac(is)
         END IF
         !!!!!!!!!!!!!!!! end snow section !!!!!!!!!!!!!!!!!!!!!

         !---------------------------------------------------------------------
         !Calculate snow weighted variables for each subsurface
         qn_is = QSTAR*(1 - SnowFrac(is)) + QSTAR_SNOW*SnowFrac(is)
         kup_is = KUP*(1 - SnowFrac(is)) + KUP_SNOW*SnowFrac(is)
         lup_is = LUP*(1 - SnowFrac(is)) + LUP_SNOW*SnowFrac(is)
         tsurf_is = TSURF_C*(1 - SnowFrac(is)) + TSURF_SNOW_C*SnowFrac(is)

         IF (DiagQN == 1) WRITE (*, *) 'QSTAR', QSTAR, 'QSTAR_SNOW', QSTAR_SNOW, 'SnowFrac', SnowFrac(is)

         !Add up the radiation from each surface
         qn1_cum = qn1_cum + (qn_is*sfr_surf(is))
         kup_cum = kup_cum + (kup_is*sfr_surf(is))
         lup_cum = lup_cum + (lup_is*sfr_surf(is))
         tsurf_cum = tsurf_cum + (tsurf_is*sfr_surf(is))

         !Define sub-surface radiation components
         qn_surf(is) = qn_is
         kup_surf(is) = kup_is
         lup_surf(is) = lup_is
         Tsurf_surf(is) = tsurf_is

         IF (DiagQN == 1) WRITE (*, *) 'qn1_is: ', qn_is

      END DO !End of the surface types

      !Set overall radiation components
      IF (NetRadiationMethod_use /= 3000) THEN !Observed Q* used and snow is modeled
         QSTARall = qn1_cum
      ELSE
         QSTARall = qn1_obs
      END IF
      KUPall = kup_cum
      LUPall = lup_cum
      TSURFall = tsurf_cum

      ! qn1=QSTARall
      ! kup=KUPall
      !LDOWN has same name
      ! LUP=LUPall
      TSURF_K = TSURFall
      !if (kup>500) then
      ! write(*,*) Kdown, kup, kup_ind(1),kup_ind(2),kup_ind(3),kup_ind(4),kup_ind(5),kup_ind(6),SnowAlb
      ! pause
      !endif

      IF (DiagQN == 1) WRITE (*, *) 'kdown: ', kdown, 'kup:', kup, 'LDOWN: ', LDOWN, 'LUP: ', LUP
      IF (DiagQN == 1) WRITE (*, *) 'Qn: ', QSTARall

   END SUBROUTINE NARP

   !==============================================================================
   SUBROUTINE NARP_cal_SunPosition(year, idectime, UTC, &
                                   locationlatitude, locationlongitude, locationaltitude, &
                                   sunazimuth, sunzenith)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: year, idectime, UTC, locationlatitude, locationlongitude, locationaltitude
      REAL(KIND(1D0)), INTENT(out) :: sunazimuth, sunzenith

      REAL(KIND(1D0)) :: sec
      INTEGER :: month, day, hour, min, seas, dayofyear, year_int

      REAL(KIND(1D0)) :: juliancentury, julianday, julianephemeris_century, julianephemeris_day, &
                         julianephemeris_millenium
      REAL(KIND(1D0)) :: earth_heliocentric_positionlatitude, earth_heliocentric_positionlongitude, &
                         earth_heliocentric_positionradius
      REAL(KIND(1D0)) :: sun_geocentric_positionlatitude, sun_geocentric_positionlongitude
      REAL(KIND(1D0)) :: nutationlongitude, nutationobliquity
      REAL(KIND(1D0)) :: corr_obliquity
      REAL(KIND(1D0)) :: aberration_correction
      REAL(KIND(1D0)) :: apparent_sun_longitude
      REAL(KIND(1D0)) :: apparent_stime_at_greenwich
      REAL(KIND(1D0)) :: sun_rigth_ascension
      REAL(KIND(1D0)) :: sun_geocentric_declination
      REAL(KIND(1D0)) :: observer_local_hour
      REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension, topocentric_sun_positionrigth_ascension_parallax
      REAL(KIND(1D0)) :: topocentric_sun_positiondeclination
      REAL(KIND(1D0)) :: topocentric_local_hour
      ! REAL(KIND(1D0)) :: sunazimuth,sunzenith

      ! This function compute the sun position (zenith and azimuth angle (in degrees) at the observer
      ! location) as a function of the observer local time and position.
      !
      ! Input lat and lng should be in degrees, alt in meters.
      !
      ! It is an implementation of the algorithm presented by Reda et Andreas in:
      ! Reda, I., Andreas, A. (2003) Solar position algorithm for solar
      ! radiation application. National Renewable Energy Laboratory (NREL)
      ! Technical report NREL/TP-560-34302.
      ! This document is available at www.osti.gov/bridge
      ! Code is translated from matlab code by Fredrik Lindberg (fredrikl@gvc.gu.se)
      ! Last modified: LJ 27 Jan 2016 - Tabs removed

      ! Convert to timevectors from dectime and year
      CALL dectime_to_timevec(idectime, hour, min, sec)
      dayofyear = FLOOR(idectime)
      year_int = INT(year)
      CALL day2month(dayofyear, month, day, seas, year_int, locationlatitude)

      ! 1. Calculate the Julian Day, and Century. Julian Ephemeris day, century
      ! and millenium are calculated using a mean delta_t of 33.184 seconds.
      CALL julian_calculation(year, month, day, hour, min, sec, UTC, juliancentury, julianday, julianephemeris_century, &
                              julianephemeris_day, julianephemeris_millenium)

      ! 2. Calculate the Earth heliocentric longitude, latitude, and radius
      ! vector (L, B, and R)
      CALL earth_heliocentric_position_calculation(julianephemeris_millenium, earth_heliocentric_positionlatitude,&
           &earth_heliocentric_positionlongitude, earth_heliocentric_positionradius)

      ! 3. Calculate the geocentric longitude and latitude
      CALL sun_geocentric_position_calculation(earth_heliocentric_positionlongitude, earth_heliocentric_positionlatitude,&
           & sun_geocentric_positionlatitude, sun_geocentric_positionlongitude)

      ! 4. Calculate the nutation in longitude and obliquity (in degrees).
      CALL nutation_calculation(julianephemeris_century, nutationlongitude, nutationobliquity)

      ! 5. Calculate the true obliquity of the ecliptic (in degrees).
      CALL corr_obliquity_calculation(julianephemeris_millenium, nutationobliquity, corr_obliquity)

      ! 6. Calculate the aberration correction (in degrees)
      CALL abberation_correction_calculation(earth_heliocentric_positionradius, aberration_correction)

      ! 7. Calculate the apparent sun longitude in degrees)
      CALL apparent_sun_longitude_calculation(sun_geocentric_positionlongitude, nutationlongitude,&
           & aberration_correction, apparent_sun_longitude)

      ! 8. Calculate the apparent sideral time at Greenwich (in degrees)
      CALL apparent_stime_at_greenwich_calculation(julianday, juliancentury, nutationlongitude, &
           &corr_obliquity, apparent_stime_at_greenwich)

      ! 9. Calculate the sun rigth ascension (in degrees)
      CALL sun_rigth_ascension_calculation(apparent_sun_longitude, corr_obliquity, sun_geocentric_positionlatitude, &
           &sun_rigth_ascension)

      ! 10. Calculate the geocentric sun declination (in degrees). Positive or
      ! negative if the sun is north or south of the celestial equator.
      CALL sun_geocentric_declination_calculation(apparent_sun_longitude, corr_obliquity, sun_geocentric_positionlatitude, &
           &sun_geocentric_declination)

      ! 11. Calculate the observer local hour angle (in degrees, westward from south).
      CALL observer_local_hour_calculation(apparent_stime_at_greenwich, locationlongitude, sun_rigth_ascension, observer_local_hour)

      ! 12. Calculate the topocentric sun position (rigth ascension, declination and
      ! rigth ascension parallax in degrees)
      CALL topocentric_sun_position_calculate(topocentric_sun_positionrigth_ascension,&
           &topocentric_sun_positionrigth_ascension_parallax, topocentric_sun_positiondeclination, locationaltitude,&
           &locationlatitude, observer_local_hour, sun_rigth_ascension, sun_geocentric_declination,&
           &earth_heliocentric_positionradius)

      ! 13. Calculate the topocentric local hour angle (in degrees)
      CALL topocentric_local_hour_calculate(observer_local_hour, topocentric_sun_positionrigth_ascension_parallax,&
           & topocentric_local_hour)

      ! 14. Calculate the topocentric zenith and azimuth angle (in degrees)
      CALL sun_topocentric_zenith_angle_calculate(locationlatitude, topocentric_sun_positiondeclination,&
           & topocentric_local_hour, sunazimuth, sunzenith)

   END SUBROUTINE NARP_cal_SunPosition

   SUBROUTINE NARP_cal_SunPosition_DTS( &
      timer, config, forcing, siteInfo, & !input
      modState) ! input/output:
      ! solarState)
      ! sunazimuth, sunzenith)

      USE SUEWS_DEF_DTS, ONLY: &
         SUEWS_TIMER, SUEWS_SITE, SUEWS_CONFIG, SUEWS_FORCING, &
         solar_State, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState

      REAL(KIND(1D0)) :: year, idectime

      REAL(KIND(1D0)) :: sec
      INTEGER :: month, day, hour, min, seas, dayofyear, year_int

      REAL(KIND(1D0)) :: juliancentury, julianday, julianephemeris_century, julianephemeris_day, &
                         julianephemeris_millenium
      REAL(KIND(1D0)) :: earth_heliocentric_positionlatitude, earth_heliocentric_positionlongitude, &
                         earth_heliocentric_positionradius
      REAL(KIND(1D0)) :: sun_geocentric_positionlatitude, sun_geocentric_positionlongitude
      REAL(KIND(1D0)) :: nutationlongitude, nutationobliquity
      REAL(KIND(1D0)) :: corr_obliquity
      REAL(KIND(1D0)) :: aberration_correction
      REAL(KIND(1D0)) :: apparent_sun_longitude
      REAL(KIND(1D0)) :: apparent_stime_at_greenwich
      REAL(KIND(1D0)) :: sun_rigth_ascension
      REAL(KIND(1D0)) :: sun_geocentric_declination
      REAL(KIND(1D0)) :: observer_local_hour
      REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension, topocentric_sun_positionrigth_ascension_parallax
      REAL(KIND(1D0)) :: topocentric_sun_positiondeclination
      REAL(KIND(1D0)) :: topocentric_local_hour

      ASSOCIATE ( &
         solarState => modState%solarState &
         )
         ASSOCIATE ( &
            iy => timer%iy, &
            dectime => timer%dectime, &
            tstep => timer%tstep, &
            UTC => siteInfo%timezone, &
            locationlatitude => siteInfo%lat, &
            locationlongitude => siteInfo%lon, &
            locationaltitude => siteInfo%alt, &
            azimuth_deg => solarState%azimuth_deg, &
            zenith_deg => solarState%zenith_deg &
            )
            year = REAL(iy, KIND(1D0))
            idectime = dectime - tstep/2/86400

            ! This function compute the sun position (zenith and azimuth angle (in degrees) at the observer
            ! location) as a function of the observer local time and position.
            !
            ! Input lat and lng should be in degrees, alt in meters.
            !
            ! It is an implementation of the algorithm presented by Reda et Andreas in:
            ! Reda, I., Andreas, A. (2003) Solar position algorithm for solar
            ! radiation application. National Renewable Energy Laboratory (NREL)
            ! Technical report NREL/TP-560-34302.
            ! This document is available at www.osti.gov/bridge
            ! Code is translated from matlab code by Fredrik Lindberg (fredrikl@gvc.gu.se)
            ! Last modified: LJ 27 Jan 2016 - Tabs removed

            ! Convert to timevectors from dectime and year
            CALL dectime_to_timevec(idectime, hour, min, sec)
            dayofyear = FLOOR(idectime)
            year_int = INT(year)
            CALL day2month(dayofyear, month, day, seas, year_int, locationlatitude)

            ! 1. Calculate the Julian Day, and Century. Julian Ephemeris day, century
            ! and millenium are calculated using a mean delta_t of 33.184 seconds.
            CALL julian_calculation(year, month, day, hour, min, sec, UTC, juliancentury, julianday, julianephemeris_century, &
                                    julianephemeris_day, julianephemeris_millenium)

            ! 2. Calculate the Earth heliocentric longitude, latitude, and radius
            ! vector (L, B, and R)
            CALL earth_heliocentric_position_calculation(julianephemeris_millenium, earth_heliocentric_positionlatitude,&
            &earth_heliocentric_positionlongitude, earth_heliocentric_positionradius)

            ! 3. Calculate the geocentric longitude and latitude
            CALL sun_geocentric_position_calculation(earth_heliocentric_positionlongitude, earth_heliocentric_positionlatitude,&
            & sun_geocentric_positionlatitude, sun_geocentric_positionlongitude)

            ! 4. Calculate the nutation in longitude and obliquity (in degrees).
            CALL nutation_calculation(julianephemeris_century, nutationlongitude, nutationobliquity)

            ! 5. Calculate the true obliquity of the ecliptic (in degrees).
            CALL corr_obliquity_calculation(julianephemeris_millenium, nutationobliquity, corr_obliquity)

            ! 6. Calculate the aberration correction (in degrees)
            CALL abberation_correction_calculation(earth_heliocentric_positionradius, aberration_correction)

            ! 7. Calculate the apparent sun longitude in degrees)
            CALL apparent_sun_longitude_calculation(sun_geocentric_positionlongitude, nutationlongitude,&
            & aberration_correction, apparent_sun_longitude)

            ! 8. Calculate the apparent sideral time at Greenwich (in degrees)
            CALL apparent_stime_at_greenwich_calculation(julianday, juliancentury, nutationlongitude, &
            &corr_obliquity, apparent_stime_at_greenwich)

            ! 9. Calculate the sun rigth ascension (in degrees)
            CALL sun_rigth_ascension_calculation(apparent_sun_longitude, corr_obliquity, sun_geocentric_positionlatitude, &
            &sun_rigth_ascension)

            ! 10. Calculate the geocentric sun declination (in degrees). Positive or
            ! negative if the sun is north or south of the celestial equator.
            CALL sun_geocentric_declination_calculation(apparent_sun_longitude, corr_obliquity, sun_geocentric_positionlatitude, &
            &sun_geocentric_declination)

            ! 11. Calculate the observer local hour angle (in degrees, westward from south).
            CALL observer_local_hour_calculation( &
               apparent_stime_at_greenwich, locationlongitude, sun_rigth_ascension, observer_local_hour)

            ! 12. Calculate the topocentric sun position (rigth ascension, declination and
            ! rigth ascension parallax in degrees)
            CALL topocentric_sun_position_calculate(topocentric_sun_positionrigth_ascension,&
            &topocentric_sun_positionrigth_ascension_parallax, topocentric_sun_positiondeclination, locationaltitude,&
            &locationlatitude, observer_local_hour, sun_rigth_ascension, sun_geocentric_declination,&
            &earth_heliocentric_positionradius)

            ! 13. Calculate the topocentric local hour angle (in degrees)
            CALL topocentric_local_hour_calculate(observer_local_hour, topocentric_sun_positionrigth_ascension_parallax,&
            & topocentric_local_hour)

            ! 14. Calculate the topocentric zenith and azimuth angle (in degrees)
            CALL sun_topocentric_zenith_angle_calculate(locationlatitude, topocentric_sun_positiondeclination,&
            & topocentric_local_hour, azimuth_deg, zenith_deg)
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE NARP_cal_SunPosition_DTS

   !================================ Subfunction definitions ========================================================!
   SUBROUTINE julian_calculation(year, month, day, hour, min, sec, UTC, juliancentury, julianday, julianephemeris_century&
        &, julianephemeris_day, julianephemeris_millenium)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: A, B, D, delta_t
      REAL(KIND(1D0)) :: juliancentury
      REAL(KIND(1D0)) :: julianday
      REAL(KIND(1D0)) :: julianephemeris_century
      REAL(KIND(1D0)) :: julianephemeris_day
      REAL(KIND(1D0)) :: julianephemeris_millenium
      REAL(KIND(1D0)) :: M, sec, year, UTC
      INTEGER :: day, hour, min, month
      !REAL(KIND(1D0)) :: time      !>
      REAL(KIND(1D0)) :: ut_time, Y !tt,
      !
      ! This function compute the julian day and julian century from the local
      ! time and timezone information. Ephemeris are calculated with a delta_t=0
      ! seconds.

      IF (month == 1 .OR. month == 2) THEN
         Y = year - 1.
         M = month + 12
      ELSE
         Y = year
         M = month
      END IF
      ut_time = ((float(hour) - UTC)/24.) + (float(min)/(60.*24.)) + (sec/(60.*60.*24.)) ! time of day in UT time.
      D = day + ut_time ! Day of month in decimal time, ex. 2sd day of month at 12:30:30UT, D=2.521180556

      ! In 1582, the gregorian calendar was adopted
      IF (year == 1582.) THEN
         IF (month == 10) THEN
            IF (day <= 4) THEN ! The Julian calendar ended on October 4, 1582
               B = 0
            ELSE IF (day >= 15) THEN ! The Gregorian calendar started on October 15, 1582
               A = FLOOR(Y/100)
               B = 2 - A + FLOOR(A/4)
            ELSE
               !disp('This date never existed!. Date automatically set to October 4, 1582')
               month = 10
               day = 4
               B = 0
            END IF
         ELSE IF (month < 10) THEN ! Julian calendar
            B = 0
         ELSE ! Gregorian calendar
            A = FLOOR(Y/100)
            B = 2 - A + FLOOR(A/4)
         END IF

      ELSE IF (year < 1582.) THEN ! Julian calendar
         B = 0
      ELSE
         A = FLOOR(Y/100) ! Gregorian calendar
         B = 2 - A + FLOOR(A/4)
      END IF

      julianday = FLOOR(365.25*(Y + 4716.)) + FLOOR(30.6001*(M + 1)) + D + B - 1524.5

      delta_t = 0. ! 33.184;
      julianephemeris_day = julianday + (delta_t/86400)

      juliancentury = (julianday - 2451545.)/36525.

      julianephemeris_century = (julianephemeris_day - 2451545.)/36525.

      julianephemeris_millenium = julianephemeris_century/10.

   END SUBROUTINE julian_calculation

   SUBROUTINE earth_heliocentric_position_calculation(julianephemeris_millenium, earth_heliocentric_positionlatitude&
        &, earth_heliocentric_positionlongitude, earth_heliocentric_positionradius)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: julianephemeris_millenium !>

      REAL(KIND(1D0)), DIMENSION(64) :: A0 !>
      REAL(KIND(1D0)), DIMENSION(34) :: A1 !>
      REAL(KIND(1D0)), DIMENSION(20) :: A2 !>
      REAL(KIND(1D0)), DIMENSION(7) :: A3 !>
      REAL(KIND(1D0)), DIMENSION(3) :: A4 !>
      REAL(KIND(1D0)) :: A5 !>
      REAL(KIND(1D0)), DIMENSION(64) :: B0 !>
      REAL(KIND(1D0)), DIMENSION(34) :: B1 !>
      REAL(KIND(1D0)), DIMENSION(20) :: B2 !>
      REAL(KIND(1D0)), DIMENSION(7) :: B3 !>
      REAL(KIND(1D0)), DIMENSION(3) :: B4 !>
      REAL(KIND(1D0)) :: B5 !>
      REAL(KIND(1D0)), DIMENSION(64) :: C0 !>
      REAL(KIND(1D0)), DIMENSION(34) :: C1 !>
      REAL(KIND(1D0)), DIMENSION(20) :: C2 !>
      REAL(KIND(1D0)), DIMENSION(7) :: C3 !>
      REAL(KIND(1D0)), DIMENSION(3) :: C4 !>
      REAL(KIND(1D0)) :: C5
      REAL(KIND(1D0)), DIMENSION(40) :: A0j !>
      REAL(KIND(1D0)), DIMENSION(10) :: A1j !>
      REAL(KIND(1D0)), DIMENSION(6) :: A2j !>
      REAL(KIND(1D0)), DIMENSION(2) :: A3j !>
      REAL(KIND(1D0)) :: A4j !>
      REAL(KIND(1D0)), DIMENSION(40) :: B0j !>
      REAL(KIND(1D0)), DIMENSION(10) :: B1j !>
      REAL(KIND(1D0)), DIMENSION(6) :: B2j !>
      REAL(KIND(1D0)), DIMENSION(2) :: B3j !>
      REAL(KIND(1D0)) :: B4j !>
      REAL(KIND(1D0)), DIMENSION(40) :: C0j !>
      REAL(KIND(1D0)), DIMENSION(10) :: C1j !>
      REAL(KIND(1D0)), DIMENSION(6) :: C2j !>
      REAL(KIND(1D0)), DIMENSION(2) :: C3j !>
      REAL(KIND(1D0)) :: C4j !>
      REAL(KIND(1D0)), DIMENSION(5) :: A0i
      REAL(KIND(1D0)), DIMENSION(5) :: B0i
      REAL(KIND(1D0)), DIMENSION(5) :: C0i
      REAL(KIND(1D0)), DIMENSION(2) :: A1i
      REAL(KIND(1D0)), DIMENSION(2) :: B1i
      REAL(KIND(1D0)), DIMENSION(2) :: C1i
      REAL(KIND(1D0)) :: earth_heliocentric_positionlatitude !>
      REAL(KIND(1D0)) :: earth_heliocentric_positionlongitude !>
      REAL(KIND(1D0)) :: earth_heliocentric_positionradius !>
      REAL(KIND(1D0)) :: JME !>
      REAL(KIND(1D0)) :: L0 !>
      !REAL(KIND(1D0)) :: L0_terms      !>
      REAL(KIND(1D0)) :: L1 !>
      !REAL(KIND(1D0)) :: L1_terms      !>
      REAL(KIND(1D0)) :: L2 !>
      !REAL(KIND(1D0)) :: L2_terms      !>
      REAL(KIND(1D0)) :: L3 !>
      !REAL(KIND(1D0)) :: L3_terms      !>
      REAL(KIND(1D0)) :: L4 !>
      !REAL(KIND(1D0)) :: L4_terms      !>
      REAL(KIND(1D0)) :: L5 !>
      !REAL(KIND(1D0)), dimension(3) :: L5_terms      !>
      !REAL(KIND(1D0)) :: R0_terms      !>
      !REAL(KIND(1D0)) :: R1_terms      !>
      !REAL(KIND(1D0)) :: R2_terms      !>
      !REAL(KIND(1D0)) :: R3_terms      !>
      !REAL(KIND(1D0)), dimension(3) :: R4_terms      !>
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D+0

      ! This function compute the earth position relative to the sun, using
      ! tabulated values.

      A0 = (/175347046, 3341656, 34894, 3497, 3418, 3136, 2676, 2343, 1324, 1273, 1199, 990, 902, 857, 780, 753, 505,&
           &492, 357, 317, 284, 271, 243, 206, 205, 202, 156, 132, 126, 115, 103, 102, 102, 99, 98, 86, 85, 85, 80, 79, 71,&
           &74, 74, 70, 62, 61, 57, 56, 56, 52, 52, 51, 49, 41, 41, 39, 37, 37, 36, 36, 33, 30, 30, 25/)
      B0 = (/0., 4.669256800, 4.626100, 2.744100, 2.828900, 3.627700, 4.418100, 6.135200, 0.7425000, 2.037100,&
           &1.109600, 5.233000, 2.045000, 3.508000, 1.179000, 2.533000, 4.583000, 4.205000, 2.92, 5.849000,&
           &1.899000, 0.315, 0.345, 4.806000, 1.869000, 2.445800, 0.833, 3.411000, 1.083000, 0.645, 0.636, 0.976,&
           &4.267000, 6.21, 0.68, 5.98, 1.3, 3.67, 1.81, 3.04, 1.76, 3.5, 4.68, 0.83, 3.98, 1.82, 2.78, 4.39, 3.47, 0.19,&
           &1.33, 0.28, 0.49, 5.37, 2.4, 6.17, 6.04, 2.57, 1.71, 1.78, 0.59, 0.44, 2.74, 3.16/)
      C0 = (/0., 6283.075850, 12566.15170, 5753.384900, 3.523100, 77713.77150, 7860.419400, 3930.209700,&
           &11506.76980, 529.6910, 1577.343500, 5884.927, 26.29800, 398.1490, 5223.694, 5507.553,&
           &18849.22800, 775.5230, 0.067, 11790.62900, 796.2980, 10977.07900, 5486.778, 2544.314,&
           &5573.143, 6069.777, 213.2990, 2942.463, 20.77500, 0.98, 4694.003, 15720.83900, 7.114000,&
           &2146.170, 155.4200, 161000.6900, 6275.960, 71430.70, 17260.15, 12036.46, 5088.630, 3154.690,&
           &801.8200, 9437.760, 8827.390, 7084.900, 6286.600, 14143.50, 6279.550, 12139.55, 1748.020,&
           &5856.480, 1194.450, 8429.240, 19651.05, 10447.39, 10213.29, 1059.380, 2352.870, 6812.770,&
           &17789.85, 83996.85, 1349.870, 4690.480/)
      A1 = (/628331966747.000, 206059., 4303., 425., 119., 109., &
             93., 72., 68., 67., 59., 56., 45., 36., 29., 21., 19., 19., 17., 16., &
             16., 15., 12., 12., 12., 12., 11., 10., 10., 9., 9., 8., 6., 6./)
      B1 = (/0., 2.678235, 2.635100, 1.59, 5.796000, 2.966000, 2.59, 1.14, 1.87, 4.41, 2.89, 2.17, 0.40, 0.47,&
           &2.65, 5.34, 1.85, 4.97, 2.99, 0.030, 1.43, 1.21, 2.83, 3.26, 5.27, 2.08, 0.77, 1.3, 4.24, 2.7, 5.64,&
           &5.3, 2.65, 4.67/)
      C1 = (/0., 6283.075850, 12566.15170, 3.523000, 26.29800, 1577.344, 18849.23, 529.6900, 398.1500,&
           &5507.550, 5223.690, 155.4200, 796.3000, 775.5200, 7.11, 0.98, 5486.780, 213.3000, 6275.960,&
           &2544.310, 2146.170, 10977.08, 1748.020, 5088.630, 1194.450, 4694., 553.5700, 3286.600,&
           &1349.870, 242.7300, 951.7200, 2352.870, 9437.760, 4690.480/)
      A2 = (/52919, 8720, 309, 27, 16, 16, 10, 9, 7, 5, 4, 4, 3, 3, 3, 3, 3, 3, 2, 2/)
      B2 = (/0., 1.072100, 0.867, 0.050, 5.19, 3.68, 0.76, 2.06, 0.83, 4.66, 1.03, 3.44, 5.14, 6.05, 1.19,&
           &6.12, 0.31, 2.28, 4.38, 3.75/)
      C2 = (/0., 6283.075800, 12566.15200, 3.52, 26.3, 155.4200, 18849.23, 77713.77, 775.5200, 1577.340,&
           &7.11, 5573.140, 796.3000, 5507.550, 242.7300, 529.6900, 398.1500, 553.5700, 5223.690, 0.98/)
      A3 = (/289, 35, 17, 3, 1, 1, 1/)
      B3 = (/5.8440, 0., 5.4900, 5.2000, 4.7200, 5.3000, 5.9700/)
      C3 = (/6283.076, 0., 12566.15, 155.4200, 3.52, 18849.23, 242.7300/)
      A4 = (/114, 8, 1/)
      B4 = (/3.1420, 4.1300, 3.8400/)
      C4 = (/0., 6283.08, 12566.15/)
      A5 = 1.
      B5 = 3.1400
      C5 = 0.

      JME = julianephemeris_millenium

      ! Compute the Earth Heliochentric longitude from the tabulated values.
      L0 = SUM(A0*COS(B0 + (C0*JME)))
      L1 = SUM(A1*COS(B1 + (C1*JME)))
      L2 = SUM(A2*COS(B2 + (C2*JME)))
      L3 = SUM(A3*COS(B3 + (C3*JME)))
      L4 = SUM(A4*COS(B4 + (C4*JME)))
      L5 = A5*COS(B5 + (C5*JME))

      earth_heliocentric_positionlongitude = &
           &(L0 + (L1*JME) + (L2*JME**2) + (L3*JME**3) + (L4*JME**4) + (L5*JME**5))/1E8
      ! Convert the longitude to degrees.
      earth_heliocentric_positionlongitude = earth_heliocentric_positionlongitude*180./pi
      ! Limit the range to [0,360[;
      earth_heliocentric_positionlongitude = set_to_range(earth_heliocentric_positionlongitude)

      A0i = (/280, 102, 80, 44, 32/)
      B0i = (/3.19900000000000, 5.42200000000000, 3.88000000000000, 3.70000000000000, 4./)
      C0i = (/84334.6620000000, 5507.55300000000, 5223.69000000000, 2352.87000000000, 1577.34000000000/)
      A1i = (/9, 6/)
      B1i = (/3.90000000000000, 1.73000000000000/)
      C1i = (/5507.55000000000, 5223.69000000000/)

      L0 = SUM(A0i*COS(B0i + (C0i*JME)))
      L1 = SUM(A1i*COS(B1i + (C1i*JME)))

      earth_heliocentric_positionlatitude = (L0 + (L1*JME))/1E8
      ! Convert the latitude to degrees.
      earth_heliocentric_positionlatitude = earth_heliocentric_positionlatitude*180/pi
      ! Limit the range to [0,360];
      earth_heliocentric_positionlatitude = set_to_range(earth_heliocentric_positionlatitude)

      A0j = (/100013989, 1670700, 13956, 3084, 1628, 1576, 925, 542, 472, 346, 329, 307, 243, 212, 186, 175, 110,&
           &98, 86, 86, 85, 63, 57, 56, 49, 47, 45, 43, 39, 38, 37, 37, 36, 35, 33, 32, 32, 28, 28, 26/)
      B0j = (/0., 3.09846350, 3.05525000, 5.1985, 1.1739, 2.8469, 5.453, 4.564, 3.661, 0.964, 5.90, 0.299,&
           &4.273, 5.847, 5.022, 3.012, 5.055, 0.890, 5.69, 1.27, 0.270, 0.920, 2.01, 5.24, 3.25, 2.58, 5.54,&
           &6.01, 5.36, 2.39, 0.830, 4.90, 1.67, 1.84, 0.240, 0.180, 1.78, 1.21, 1.90, 4.59/)
      C0j = (/0., 6283.07585, 12566.1517, 77713.7715, 5753.38490, 7860.41940, 11506.7700, 3930.21000,&
           &5884.92700, 5507.55300, 5223.69400, 5573.14300, 11790.6290, 1577.34400, 10977.0790, 18849.2280,&
           &5486.77800, 6069.78000, 15720.8400, 161000.690, 17260.1500, 529.69, 83996.8500, 71430.7000,&
           &2544.31000, 775.52, 9437.76000, 6275.96000, 4694., 8827.39000, 19651.0500, 12139.5500,&
           &12036.4600, 2942.46000, 7084.9, 5088.63000, 398.15, 6286.6, 6279.55000, 10447.3900/)
      A1j = (/103019, 1721, 702, 32, 31, 25, 18, 10, 9, 9/)
      B1j = (/1.10749000, 1.0644, 3.142, 1.02, 2.84, 1.32, 1.42, 5.91, 1.42, 0.270/)
      C1j = (/6283.07585, 12566.1517, 0., 18849.2300, 5507.55000, 5223.69000, 1577.34000, 10977.0800,&
           &6275.96000, 5486.78000/)
      A2j = (/4359, 124, 12, 9, 6, 3/)
      B2j = (/5.7846, 5.579, 3.14, 3.63, 1.87, 5.47/)
      C2j = (/6283.07580, 12566.1520, 0., 77713.7700, 5573.14000, 18849./)
      A3j = (/145, 7/)
      B3j = (/4.273, 3.92/)
      C3j = (/6283.07600, 12566.1500/)
      A4j = 4
      B4j = 2.56
      C4j = 6283.08000

      ! Compute the Earth heliocentric radius vector
      L0 = SUM(A0j*COS(B0j + (C0j*JME)))
      L1 = SUM(A1j*COS(B1j + (C1j*JME)))
      L2 = SUM(A2j*COS(B2j + (C2j*JME)))
      L3 = SUM(A3j*COS(B3j + (C3j*JME)))
      L4 = A4j*COS(B4j + (C4j*JME))

      ! Units are in AU
      earth_heliocentric_positionradius = &
           &(L0 + (L1*JME) + (L2*JME**2) + (L3*JME**3) + (L4*JME**4))/1E8

   END SUBROUTINE earth_heliocentric_position_calculation

   SUBROUTINE sun_geocentric_position_calculation(earth_heliocentric_positionlongitude,&
        &earth_heliocentric_positionlatitude, sun_geocentric_positionlatitude, &
        &sun_geocentric_positionlongitude)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionlongitude !>
      REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionlatitude !>
      REAL(KIND(1D0)) :: sun_geocentric_positionlatitude !>
      REAL(KIND(1D0)) :: sun_geocentric_positionlongitude !>

      ! This function compute the sun position relative to the earth.

      sun_geocentric_positionlongitude = earth_heliocentric_positionlongitude + 180.0
      ! Limit the range to [0,360];
      sun_geocentric_positionlongitude = set_to_range(sun_geocentric_positionlongitude)

      sun_geocentric_positionlatitude = -earth_heliocentric_positionlatitude
      ! Limit the range to [0,360]
      sun_geocentric_positionlatitude = set_to_range(sun_geocentric_positionlatitude)
   END SUBROUTINE sun_geocentric_position_calculation

   SUBROUTINE nutation_calculation(julianephemeris_century, nutationlongitude, nutationobliquity)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: julianephemeris_century !>
      REAL(KIND(1D0)), DIMENSION(63) :: delta_longitude !>
      REAL(KIND(1D0)), DIMENSION(63) :: delta_obliquity !>
      REAL(KIND(1D0)) :: JCE !>
      REAL(KIND(1D0)) :: nutationlongitude !>
      REAL(KIND(1D0)) :: nutationobliquity !>
      REAL(KIND(1D0)), DIMENSION(4) :: p0, p1, p2, p3, p4
      REAL(KIND(1D0)), DIMENSION(63) :: tabulated_argument !>
      REAL(KIND(1D0)) :: X0 !>
      REAL(KIND(1D0)) :: X1 !>
      REAL(KIND(1D0)) :: X2 !>
      REAL(KIND(1D0)) :: X3 !>
      REAL(KIND(1D0)) :: X4 !>
      REAL(KIND(1D0)), DIMENSION(5) :: Xi !>
      INTEGER, DIMENSION(315) :: Y_terms1 !>
      INTEGER, DIMENSION(5, 63) :: Y_terms
      REAL(KIND(1D0)), DIMENSION(252) :: nutation_terms1 !>
      REAL(KIND(1D0)), DIMENSION(4, 63) :: nutation_terms
      INTEGER :: i
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D+0

      ! This function compute the nutation in longtitude and in obliquity, in
      ! degrees.

      ! All Xi are in degrees.
      JCE = julianephemeris_century

      ! 1. Mean elongation of the moon from the sun
      p0 = (/(1/189474.), -0.0019142, 445267.11148, 297.85036/)
      ! X0 = polyval(p, JCE);
      X0 = p0(1)*JCE**3 + p0(2)*JCE**2 + p0(3)*JCE + p0(4) ! This is faster than polyval...

      ! 2. Mean anomaly of the sun (earth)
      p1 = (/-(1/300000.), -0.0001603, 35999.05034, 357.52772/)
      ! X1 = polyval(p, JCE);
      X1 = p1(1)*JCE**3 + p1(2)*JCE**2 + p1(3)*JCE + p1(4)

      ! 3. Mean anomaly of the moon
      p2 = (/(1/56250.), 0.0086972, 477198.867398, 134.96298/)
      ! X2 = polyval(p, JCE);
      X2 = p2(1)*JCE**3 + p2(2)*JCE**2 + p2(3)*JCE + p2(4)

      ! 4. Moon argument of latitude
      p3 = (/(1/327270.), -0.0036825, 483202.017538, 93.27191/)
      ! X3 = polyval(p, JCE);
      X3 = p3(1)*JCE**3 + p3(2)*JCE**2 + p3(3)*JCE + p3(4)

      ! 5. Longitude of the ascending node of the moon's mean orbit on the
      ! ecliptic, measured from the mean equinox of the date
      p4 = (/(1/450000.), 0.0020708, -1934.136261, 125.04452/)
      ! X4 = polyval(p, JCE);
      X4 = p4(1)*JCE**3 + p4(2)*JCE**2 + p4(3)*JCE + p4(4)

      ! Y tabulated terms from the original code
      Y_terms1 = (/0, 0, 0, 0, 1, -2, 0, 0, 2, 2, 0, 0, 0, 2, 2, 0, 0, &
                   0, 0, 2, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, -2, 1, 0, 2, 2, 0, 0, 0, 2, 1, &
                   0, 0, 1, 2, 2, -2, -1, 0, 2, 2, -2, 0, 1, 0, 0, -2, 0, 0, 2, 1, 0, 0, -1, 2, 2, 2, 0, &
                   0, 0, 0, 0, 0, &
                   1, 0, 1, 2, 0, -1, 2, 2, &
                   0, 0, -1, 0, 1, 0, 0, 1, 2, 1, -2, 0, 2, 0, 0, 0, 0, -2, 2, 1, 2, 0, 0, 2, 2, 0, 0, 2, &
                   2, 2, 0, 0, 2, &
                   0, 0, -2, 0, 1, 2, 2, 0, &
                   0, 0, 2, 0, -2, 0, 0, 2, 0, 0, 0, -1, 2, 1, 0, 2, 0, 0, 0, &
                   2, 0, -1, 0, 1, -2, 2, 0, 2, 2, 0, 1, 0, 0, 1, &
                   -2, 0, 1, 0, 1, 0, -1, 0, 0, 1, 0, 0, &
                   2, -2, 0, 2, 0, -1, 2, 1, 2, 0, 1, 2, 2, 0, 1, 0, 2, 2, -2, &
                   1, 1, 0, 0, 0, -1, 0, 2, 2, 2, 0, 0, 2, 1, 2, &
                   0, 1, 0, 0, -2, 0, 2, 2, 2, -2, 0, 1, &
                   2, 1, 2, 0, -2, 0, 1, 2, 0, 0, 0, 1, 0, -1, 1, 0, 0, -2, -1, &
                   0, 2, 1, -2, 0, 0, 0, 1, 0, 0, 2, 2, 1, -2, &
                   0, 2, 0, 1, -2, 1, 0, 2, 1, 0, 0, 1, -2, &
                   0, -1, 0, 1, 0, 0, -2, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 2, &
                   0, 0, 0, -2, 2, 2, -1, -1, 1, 0, 0, 0, 1, &
                   1, 0, 0, 0, -1, 1, 2, 2, 2, -1, -1, 2, 2, &
                   0, 0, 3, 2, 2, 2, -1, 0, 2, 2/)
      Y_terms = RESHAPE(Y_terms1, (/5, 63/))
      nutation_terms1 = (/-171996., -174.2, 92025., 8.9, -13187., -1.6, 5736., -3.1, -2274., -0.2, 977., -0.5, 2062., &
                          0.2, -895., 0.5, &
                          1426., -3.4, 54., -0.1, 712., 0.1, -7., 0., -517., 1.2, 224., -0.6, -386., -0.4, 200., 0., -301., &
                          0., 129., -0.1, &
                          217., -0.5, -95., 0.3, -158., 0., 0., 0., 129., 0.1, -70., 0., 123., 0., -53., 0., 63., 0., 0., 0., &
                          63., 0.1, -33., 0., -59., 0., 26., 0., &
                          -58., -0.1, 32., 0., -51., 0., 27., 0., 48., 0., 0., 0., 46., 0., -24., 0., -38., &
                          0., 16., 0., -31., 0., 13., 0., 29., 0., &
                          0., 0., 29., 0., -12., 0., 26., 0., 0., 0., -22., 0., 0., 0., 21., 0., -10., 0., 17., -0.1, 0., 0., 16., &
                          0., -8., 0., -16., 0.1, 7., 0., &
                          -15., 0., 9., 0., -13., 0., 7., 0., -12., 0., 6., 0., 11., 0., 0., 0., &
                          -10., 0., 5., 0., -8., 0., 3., 0., 7., &
                          0., -3., 0., -7., 0., 0., 0., &
                          -7., 0., 3., 0., -7., 0., 3., 0., 6., 0., 0., 0., 6., 0., -3., 0., 6., 0., &
                          -3., 0., -6., 0., 3., 0., -6., 0., 3., &
                          0., 5., 0., 0., 0., -5., 0., &
                          3., 0., -5., 0., 3., 0., -5., 0., 3., 0., 4., 0., 0., 0., 4., 0., 0., 0., 4., 0., 0., 0., -4., 0., 0., &
                          0., -4., 0., 0., 0., -4., 0., 0., 0., &
                          3., 0., 0., 0., -3., 0., 0., 0., -3., 0., 0., 0., -3., 0., 0., 0., -3., 0., 0., 0., -3., 0., 0., &
                          0., -3., 0., 0., 0., -3., 0., 0., 0./)
      nutation_terms = RESHAPE(nutation_terms1, (/4, 63/))
      ! Using the tabulated values, compute the delta_longitude and
      ! delta_obliquity.
      Xi = (/X0, X1, X2, X3, X4/)

      DO i = 1, 63
         tabulated_argument(i) = ((Y_terms(1, i)*Xi(1)) &
                                  + (Y_terms(2, i)*Xi(2)) + (Y_terms(3, i)*Xi(3)) &
                                  + (Y_terms(4, i)*Xi(4)) + (Y_terms(5, i)*Xi(5)))*pi/180
      END DO

      delta_longitude = ((nutation_terms(1, :) + (nutation_terms(2, :)*JCE)))*SIN(tabulated_argument)
      delta_obliquity = ((nutation_terms(3, :) + (nutation_terms(4, :)*JCE)))*COS(tabulated_argument)

      ! Nutation in longitude
      nutationlongitude = SUM(delta_longitude)/36000000.0

      ! Nutation in obliquity
      nutationobliquity = SUM(delta_obliquity)/36000000.0

   END SUBROUTINE nutation_calculation

   SUBROUTINE corr_obliquity_calculation(julianephemeris_millenium, nutationobliquity, corr_obliquity)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(out) :: corr_obliquity !>
      REAL(KIND(1D0)), INTENT(in) :: julianephemeris_millenium !>
      REAL(KIND(1D0)), INTENT(in) :: nutationobliquity !>
      REAL(KIND(1D0)) :: mean_obliquity !>
      REAL(KIND(1D0)), DIMENSION(11) :: p !>
      REAL(KIND(1D0)) :: U !>

      ! This function compute the true obliquity of the ecliptic.

      p = (/2.45, 5.79, 27.87, 7.12, -39.05, -249.67, -51.38, 1999.25, -1.55, -4680.93, 84381.448/)
      ! mean_obliquity = polyval(p, julian.ephemeris_millenium/10);

      U = julianephemeris_millenium/10
      mean_obliquity =&
           &p(1)*U**10 + p(2)*U**9 + p(3)*U**8 + p(4)*U**7 + p(5)*U**6 + p(6)*U**5 + p(7)*U**4 + &
           &p(8)*U**3 + p(9)*U**2 + p(10)*U + p(11)

      corr_obliquity = (mean_obliquity/3600) + nutationobliquity
   END SUBROUTINE corr_obliquity_calculation

   SUBROUTINE abberation_correction_calculation(earth_heliocentric_positionradius, aberration_correction)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(out) :: aberration_correction !>
      REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionradius !>

      ! This function compute the aberration_correction, as a function of the
      ! earth-sun distance.

      aberration_correction = -20.4898/(3600*earth_heliocentric_positionradius)

   END SUBROUTINE abberation_correction_calculation

   SUBROUTINE apparent_sun_longitude_calculation(sun_geocentric_positionlongitude, nutationlongitude,&
        & aberration_correction, apparent_sun_longitude)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: aberration_correction !>
      REAL(KIND(1D0)), INTENT(out) :: apparent_sun_longitude !>
      REAL(KIND(1D0)), INTENT(in) :: nutationlongitude !>
      REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlongitude !>

      ! This function compute the sun apparent longitude

      apparent_sun_longitude = sun_geocentric_positionlongitude + nutationlongitude + aberration_correction

   END SUBROUTINE apparent_sun_longitude_calculation

   SUBROUTINE apparent_stime_at_greenwich_calculation(julianday, juliancentury, nutationlongitude,&
        & corr_obliquity, apparent_stime_at_greenwich)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(out) :: apparent_stime_at_greenwich !>
      REAL(KIND(1D0)), INTENT(in) :: corr_obliquity !>
      REAL(KIND(1D0)), INTENT(in) :: julianday !>
      REAL(KIND(1D0)), INTENT(in) :: juliancentury !>
      REAL(KIND(1D0)), INTENT(in) :: nutationlongitude !>
      REAL(KIND(1D0)) :: JC !>
      REAL(KIND(1D0)) :: JD !>
      REAL(KIND(1D0)) :: mean_stime !>
      REAL(KIND(1D0)), PARAMETER :: pi = 3.14159265358979D+0

      ! This function compute the apparent sideral time at Greenwich.

      JD = julianday
      JC = juliancentury

      ! Mean sideral time, in degrees
      mean_stime = 280.46061837D+0 + (360.98564736629D+0*(JD - 2451545.0D+0)) + (0.000387933D+0*JC**2) - (JC**3/38710000.0D+0)

      ! Limit the range to [0-360];
      mean_stime = set_to_range(mean_stime)

      apparent_stime_at_greenwich = mean_stime + (nutationlongitude*COS(corr_obliquity*pi/180))
   END SUBROUTINE apparent_stime_at_greenwich_calculation

   SUBROUTINE sun_rigth_ascension_calculation(apparent_sun_longitude, corr_obliquity, &
        &sun_geocentric_positionlatitude, sun_rigth_ascension)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: apparent_sun_longitude !>
      REAL(KIND(1D0)), INTENT(in) :: corr_obliquity !>
      REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlatitude !>
      REAL(KIND(1D0)), INTENT(out) :: sun_rigth_ascension !>
      REAL(KIND(1D0)) :: argument_denominator !>
      REAL(KIND(1D0)) :: argument_numerator !>
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D+0

      ! This function compute the sun rigth ascension.

      argument_numerator = (SIN(apparent_sun_longitude*pi/180.0)*COS(corr_obliquity*pi/180.0)) - &
                           (TAN(sun_geocentric_positionlatitude*pi/180.0)*SIN(corr_obliquity*pi/180.0))
      argument_denominator = COS(apparent_sun_longitude*pi/180.0)

      sun_rigth_ascension = ATAN2(argument_numerator, argument_denominator)*180.0/pi
      ! Limit the range to [0,360];
      sun_rigth_ascension = set_to_range(sun_rigth_ascension)
   END SUBROUTINE sun_rigth_ascension_calculation

   SUBROUTINE sun_geocentric_declination_calculation(apparent_sun_longitude, corr_obliquity, &
        &sun_geocentric_positionlatitude, sun_geocentric_declination)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: apparent_sun_longitude !>
      REAL(KIND(1D0)), INTENT(in) :: corr_obliquity !>
      REAL(KIND(1D0)), INTENT(out) :: sun_geocentric_declination !>
      REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlatitude !>
      REAL(KIND(1D0)) :: argument !>
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D+0

      argument = (SIN(sun_geocentric_positionlatitude*pi/180.0)*COS(corr_obliquity*pi/180.0)) + &
                 (COS(sun_geocentric_positionlatitude*pi/180.0)*SIN(corr_obliquity*pi/180)*SIN(apparent_sun_longitude*pi/180.0))

      sun_geocentric_declination = ASIN(argument)*180.0/pi
   END SUBROUTINE sun_geocentric_declination_calculation

   SUBROUTINE observer_local_hour_calculation(apparent_stime_at_greenwich, locationlongitude, &
        &sun_rigth_ascension, observer_local_hour)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: apparent_stime_at_greenwich !>
      REAL(KIND(1D0)), INTENT(in) :: locationlongitude !>
      REAL(KIND(1D0)), INTENT(out) :: observer_local_hour !>
      REAL(KIND(1D0)), INTENT(in) :: sun_rigth_ascension !>

      observer_local_hour = apparent_stime_at_greenwich + locationlongitude - sun_rigth_ascension
      ! Set the range to [0-360]
      observer_local_hour = set_to_range(observer_local_hour)
   END SUBROUTINE observer_local_hour_calculation

   SUBROUTINE topocentric_sun_position_calculate(topocentric_sun_positionrigth_ascension &
        &, topocentric_sun_positionrigth_ascension_parallax, topocentric_sun_positiondeclination,&
        &locationaltitude, locationlatitude, observer_local_hour, sun_rigth_ascension,&
        &sun_geocentric_declination, earth_heliocentric_positionradius)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionradius
      REAL(KIND(1D0)), INTENT(in) :: locationlatitude !>
      REAL(KIND(1D0)), INTENT(in) :: locationaltitude
      REAL(KIND(1D0)), INTENT(in) :: observer_local_hour !>
      REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_declination !>
      REAL(KIND(1D0)), INTENT(in) :: sun_rigth_ascension !>
      REAL(KIND(1D0)) :: denominator !>
      REAL(KIND(1D0)) :: eq_horizontal_parallax !>
      REAL(KIND(1D0)) :: nominator !>
      REAL(KIND(1D0)) :: sun_rigth_ascension_parallax !>
      REAL(KIND(1D0)) :: topocentric_sun_positiondeclination !>
      REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension !>
      REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension_parallax !>
      REAL(KIND(1D0)) :: u !>
      REAL(KIND(1D0)) :: x !>
      REAL(KIND(1D0)) :: y !>
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D+0

      ! topocentric_sun_positionrigth_ascension_parallax
      ! This function compute the sun position (rigth ascension and declination)
      ! with respect to the observer local position at the Earth surface.

      ! Equatorial horizontal parallax of the sun in degrees
      eq_horizontal_parallax = 8.794/(3600*earth_heliocentric_positionradius)

      ! Term u, used in the following calculations (in radians)
      u = ATAN(0.99664719*TAN(locationlatitude*pi/180))

      ! Term x, used in the following calculations
      x = COS(u) + ((locationaltitude/6378140)*COS(locationaltitude*pi/180))

      ! Term y, used in the following calculations
      y = (0.99664719D+0*SIN(u)) + ((locationaltitude/6378140)*SIN(locationlatitude*pi/180))

      ! Parallax in the sun rigth ascension (in radians)
      nominator = -x*SIN(eq_horizontal_parallax*pi/180.0)*SIN(observer_local_hour*pi/180.0)
      denominator = COS(sun_geocentric_declination*pi/180.0) - &
                    (x*SIN(eq_horizontal_parallax*pi/180.0)*COS(observer_local_hour*pi/180.0))
      sun_rigth_ascension_parallax = ATAN2(nominator, denominator)
      ! Conversion to degrees.
      topocentric_sun_positionrigth_ascension_parallax = sun_rigth_ascension_parallax*180.0/pi

      ! Topocentric sun rigth ascension (in degrees)
      topocentric_sun_positionrigth_ascension = sun_rigth_ascension + (sun_rigth_ascension_parallax*180.0/pi)

      ! Topocentric sun declination (in degrees)
      nominator = (SIN(sun_geocentric_declination*pi/180.0) - (y*SIN(eq_horizontal_parallax*pi/180.0)))&
                 & *COS(sun_rigth_ascension_parallax)
      denominator = COS(sun_geocentric_declination*pi/180.0) - (y*SIN(eq_horizontal_parallax*pi/180.0))&
                   & *COS(observer_local_hour*pi/180.0)
      topocentric_sun_positiondeclination = ATAN2(nominator, denominator)*180.0/pi
   END SUBROUTINE topocentric_sun_position_calculate

   SUBROUTINE topocentric_local_hour_calculate(observer_local_hour, topocentric_sun_positionrigth_ascension_parallax,&
        & topocentric_local_hour)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: observer_local_hour !>
      REAL(KIND(1D0)), INTENT(out) :: topocentric_local_hour !>
      REAL(KIND(1D0)), INTENT(in) :: topocentric_sun_positionrigth_ascension_parallax !>

      ! This function compute the topocentric local jour angle in degrees

      topocentric_local_hour = observer_local_hour - topocentric_sun_positionrigth_ascension_parallax
   END SUBROUTINE topocentric_local_hour_calculate

   SUBROUTINE sun_topocentric_zenith_angle_calculate(locationlatitude, topocentric_sun_positiondeclination, &
        &topocentric_local_hour, sunazimuth, sunzenith)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: locationlatitude !>
      REAL(KIND(1D0)), INTENT(in) :: topocentric_local_hour !>
      REAL(KIND(1D0)), INTENT(in) :: topocentric_sun_positiondeclination
      REAL(KIND(1D0)) :: corr_elevation !>
      REAL(KIND(1D0)) :: apparent_elevation !>
      REAL(KIND(1D0)) :: argument !>
      REAL(KIND(1D0)) :: denominator !>
      REAL(KIND(1D0)) :: nominator !>
      REAL(KIND(1D0)) :: refraction_corr !>
      REAL(KIND(1D0)) :: sunazimuth !>
      REAL(KIND(1D0)) :: sunzenith !>
      REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D+0
      ! This function compute the sun zenith angle, taking into account the
      ! atmospheric refraction. A default temperature of 283K and a
      ! default pressure of 1010 mbar are used.

      ! Topocentric elevation, without atmospheric refraction
      argument = (SIN(locationlatitude*pi/180.0)*SIN(topocentric_sun_positiondeclination*pi/180.0)) + &
           (COS(locationlatitude*pi/180.0)*COS(topocentric_sun_positiondeclination*pi/180.0)* &
           &COS(topocentric_local_hour*pi/180.0))
      corr_elevation = ASIN(argument)*180.0/pi

      ! Atmospheric refraction correction (in degrees)
      argument = corr_elevation + (10.3/(corr_elevation + 5.11))
      refraction_corr = 1.02/(60*TAN(argument*pi/180.0))

      ! For exact pressure and temperature correction, use this,
      ! with P the pressure in mbar amd T the temperature in Kelvins:
      ! refraction_corr = (P/1010) * (283/T) * 1.02 / (60 * tan(argument * pi/180));

      ! Apparent elevation
      apparent_elevation = corr_elevation + refraction_corr

      sunzenith = 90.0 - apparent_elevation

      ! Topocentric azimuth angle. The +180 conversion is to pass from astronomer
      ! notation (westward from south) to navigation notation (eastward from
      ! north);
      nominator = SIN(topocentric_local_hour*pi/180.0)
      denominator = (COS(topocentric_local_hour*pi/180.0)*SIN(locationlatitude*pi/180.0)) - &
                    (TAN(topocentric_sun_positiondeclination*pi/180.0)*COS(locationlatitude*pi/180.0))
      sunazimuth = (ATAN2(nominator, denominator)*180.0/pi) + 180.0
      ! Set the range to [0-360]
      sunazimuth = set_to_range(sunazimuth)

   END SUBROUTINE sun_topocentric_zenith_angle_calculate

   FUNCTION set_to_range(var) RESULT(vari)
      ! This function make sure the variable is in the specified range.

      REAL(KIND(1D0)) :: max_interval !>
      REAL(KIND(1D0)) :: min_interval !>
      REAL(KIND(1D0)) :: var
      REAL(KIND(1D0)) :: vari
      !
      max_interval = 360.0
      min_interval = 0.0

      vari = var - max_interval*FLOOR(var/max_interval)

      IF (vari < min_interval) THEN
         vari = vari + max_interval
      END IF

   END FUNCTION set_to_range

   !==============================================================================
   FUNCTION dewpoint_narp(Temp_C, rh) RESULT(td)
      ! ea = vapor pressure (hPa)
      ! td = dewpoint (oC)
      ! calculates dewpoint in degC from
      ! http://www.atd.ucar.edu/weather_fl/dewpoint.html
      ! dewpoint = (237.3 * ln(e_vp/6.1078)) / (17.27 - (ln(e_vp/6.1078)))

      REAL(KIND(1D0)) :: rh, td, Temp_C, g
      !http://en.wikipedia.org/wiki/Dew_point
      g = ((17.27*Temp_C)/(237.7 + Temp_C)) + LOG(rh/100)
      Td = (237.7*g)/(17.27 - g)
      !td = (237.3 * LOG(ea_hPa/6.1078)) / (17.27 - (LOG(ea_hPa/6.1078)))
   END FUNCTION dewpoint_narp
   !===============================================================================
   FUNCTION PRATA_EMIS(Temp_K, EA_hPa) RESULT(EMIS_A)
      ! clear sky emissivity function Prata 1996
      REAL(KIND(1D0)) :: Temp_K, ea_hPa, EMIS_A
      REAL(KIND(1D0)) :: W

      W = 46.5*(ea_hPa/Temp_K)
      EMIS_A = 1.-(1.+W)*EXP(-SQRT(1.2 + 3.*W))
   END FUNCTION PRATA_EMIS
   !===============================================================================
   FUNCTION EMIS_CLOUD(EMIS_A, FCLD) RESULT(em_adj)
      !calculates adjusted emissivity due to clouds
      REAL(KIND(1D0)) :: EMIS_A, FCLD, em_adj
      !T. Loridan, removing the square for FCLD in the emissivity correction
      !em_adj=EMIS_A+(1.-EMIS_A)*FCLD*FCLD
      em_adj = EMIS_A + (1.-EMIS_A)*FCLD
   END FUNCTION EMIS_CLOUD
   !===============================================================================
   FUNCTION EMIS_CLOUD_SQ(EMIS_A, FCLD) RESULT(em_adj)
      !calculates adjusted emissivity due to clouds
      REAL(KIND(1D0)) :: EMIS_A, FCLD, em_adj
      em_adj = EMIS_A + (1.-EMIS_A)*FCLD*FCLD
   END FUNCTION EMIS_CLOUD_SQ
   !===============================================================================
   FUNCTION cloud_fraction(KDOWN, KCLEAR) RESULT(FCLD)
      REAL(KIND(1D0)) :: KDOWN, KCLEAR, FCLD

      FCLD = 1.-KDOWN/KCLEAR
      IF (FCLD > 1.) FCLD = 1.
      IF (FCLD < 0.) FCLD = 0.
   END FUNCTION cloud_fraction
   !===============================================================================
   FUNCTION WC_fraction(RH, Temp) RESULT(FWC)
      ! Thomas Loridan, King's College London: June 2009
      ! Parameterisation of fraction water content using the relative humidity
      REAL(KIND(1D0)), INTENT(in) :: RH !Relative Humidity in %
      REAL(KIND(1D0)), INTENT(in) :: Temp !Temperature in degre C

      REAL(KIND(1D0)) :: FWC !Fraction water content between 0 and 1
      REAL(KIND(1D0)) :: A, B !Parameters in the expo

      !Parameters
      !A=0.078
      !B=0.026

      A = 0.185
      B = 0.00019*Temp + 0.015

      !FWC parameterization
      FWC = A*(EXP(B*RH) - 1)
      IF (FWC > 1.) FWC = 1.
      IF (FWC < 0.) FWC = 0.
   END FUNCTION WC_fraction
   !===============================================================================
   !FUNCTION solar_zenith(lat,lng,timezone,dectime) RESULT(zenith)
   !  !Stull, 1989
   !  !returns zenith in radians
   !  !lat, lng in RADS
   !  REAL(KIND(1d0)) ::lat,lng,timezone,dectime,zenith, eqtime
   !  REAL(KIND(1d0)) ::ha, decl,tst, time_offset,gamma
   !
   !  gamma=2.*3.141592654/365.25463*dectime
   !  eqtime=229.18*(7.5e-5+1.868e-3*COS(gamma)-0.032077*SIN(gamma)&
   !       -0.014615*COS(2.*gamma)-0.040849*SIN(2.*gamma))
   !  decl=6.918e-3-0.399912*COS(gamma)+0.070257*SIN(gamma)&
   !       -0.006758*COS(2.*gamma)+9.07e-4*SIN(2.*gamma)-2.697e-3*COS(3.*gamma)&
   !       +1.48e-3*SIN(3.*gamma)
   !  time_offset=eqtime-4.*lng*RAD2DEG-60.*timezone
   !  tst=(dectime-FLOOR(dectime))*1440.+time_offset
   !  ha=(tst/4.)-180.
   !  ha=ha*DEG2RAD
   !  zenith=ACOS(SIN(lat)*SIN(decl)+COS(lat)*COS(decl)*COS(ha))
   !END FUNCTION solar_zenith

   !===============================================================================
   FUNCTION ISURFACE(doy, zenith) RESULT(Isurf)
      ! Calculates ground level solar irradiance clear sky
      ! assuming transmissivity = 1
      ! let it report zero if zenith >= 90
      REAL(KIND(1D0)) :: zenith, Isurf
      INTEGER :: doy
      REAL(KIND(1D0)) :: Rmean, Rse, cosZ, Itoa
      REAL(KIND(1D0)), PARAMETER :: DEG2RAD = 0.017453292

      Rmean = 149.6 !Stull 1998
      Rse = solar_ESdist(doy)
      IF (zenith < 90.*DEG2RAD) THEN
         cosZ = COS(zenith)
         Itoa = 1370.*(Rmean/Rse)**2 !top of the atmosphere
         Isurf = Itoa*cosZ !ground level solar irradiance in W/m2
      ELSE
         Isurf = 0.
      END IF

   END FUNCTION ISURFACE

   !===============================================================================
   FUNCTION solar_ESdist(doy) RESULT(Rse)
      !from Stull, 1998   Keep! called from SOLWEIG_clearnessindex_2013b
      INTEGER :: doy
      REAL(KIND(1D0)) :: Rse
      REAL(KIND(1D0)) :: MA, nu, e, a

      e = 0.0167
      a = 146.457

      MA = 2.*3.141592654*(doy - 3)/365.25463 !Mean anomaly
      nu = MA + 0.0333988*SIN(MA) + .0003486*SIN(2.*MA) + 5E-6*SIN(3.*MA) !true anomaly
      Rse = a*(1 - e*e)/(1 + e*COS(nu))

   END FUNCTION solar_ESdist

   !===============================================================================
   FUNCTION SmithLambda(lat) RESULT(G)
      USE FileName
      USE defaultnotUsed
      !read kriged data based on Smith 1966 (JAM)
      ! Smith, William L.
      ! "Note on the relationship between total precipitable water and surface dew point."
      ! Journal of Applied Meteorology 5.5 (1966): 726-727.
      INTEGER :: lat, ios, ilat
      REAL(KIND(1D0)), DIMENSION(365) :: G

      !open(99,file="Smith1966.grd",access="direct",action="read",recl=365*4,iostat=ios)
      !read(99,rec=lat+1,iostat=ios) G
      OPEN (99, file=smithFile, iostat=ios)
      DO ilat = 1, lat
         READ (99, *)
      END DO
      READ (99, *, iostat=ios) ilat, G
      IF (ios /= 0) THEN
         CALL ErrorHint(11, 'reading Smith1966.grd (ios).', notUsed, notUsed, ios)
      END IF
      CLOSE (99)
   END FUNCTION SmithLambda

   !===============================================================================
   FUNCTION transmissivity(Press_hPa, Temp_C_dew, G, zenith) RESULT(trans)
      ! bulk atmospheric transmissivity (Crawford and Duchon, 1999)
      ! P = pressure (hPa)
      ! Td = dewpoint (C)
      ! G parameter is empirical value from Smith 1966 (JAM)
      ! zenith in radians
      ! if zenith > 80 use the value for 80.

      REAL(KIND(1D0)) :: Press_hPa, TemP_C_dew, zenith, G, trans
      REAL(KIND(1D0)) :: m, TrTpg, u, Tw, Ta, cosZ
      REAL(KIND(1D0)) :: Tdf
      REAL(KIND(1D0)), PARAMETER :: DEG2RAD = 0.017453292

      IF (zenith > 80.*DEG2RAD) THEN
         cosZ = COS(80.*DEG2RAD)
      ELSE
         cosZ = COS(zenith)
      END IF

      Tdf = TemP_C_dew*1.8 + 32. !celsius to fahrenheit
      !        Transmission coefficients
      m = 35*cosZ/SQRT(1224.*cosZ*cosZ + 1) !optical air mass at p=1013 mb
      !Rayleigh & permanent gases
      TrTpg = 1.021 - 0.084*SQRT(m*(0.000949*Press_hPa + 0.051)) !first two trans coeff
      u = EXP(0.113 - LOG(G + 1) + 0.0393*Tdf) !precipitable water
      Tw = 1 - 0.077*(u*m)**0.3 !vapor transmission coe3ff.
      Ta = 0.935**m !4th trans coeff
      trans = TrTpg*Tw*Ta !bulk atmospheric transmissivity
   END FUNCTION transmissivity
   !===============================================================================
END MODULE NARP_MODULE
