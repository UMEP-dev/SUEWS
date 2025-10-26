!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
! This is the core function of the BEERS (Building Envelope Energy Radiation Scheme)
! 2020-11-03
! Fredrik Lindberg, fredrikl@gvc.gu.se
! Gothenburg Urban Climate Group
! Gothenburg University
!
!THESE THINGS NEED ATTENTION
!MAJOR
!Tsurf is coming in but this is calculated within BEERS. Which one to use?
!Tg is surface temp on sunlit ground. Not same as Tsurf
!shadowGroundKusaka translation needs to be checked
!tSurfBEERS translation needs to be checked
!
!MINOR:
!shadows on roof is constant. Should change depending on building morhpology and sun altitude
!col and row should be excluded as this is a 1D model now
!Fix unreasonable F_sh. Mayby fixed?
!Change conversion of H/W to SVF using OKE basin equation, p. 352-353
!Added TODO:s throughout the code
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

! Main module following naming standard: matches filename
MODULE module_phys_beers

   ! USE module_ctrl_const_allocate, only: ncolumnsDataOutSol, deg2rad, rad2deg
   ! USE module_ctrl_const_default, only: notUsed, notUsedI
   USE module_phys_narp, ONLY: NARP_cal_SunPosition
   USE module_ctrl_const_allocate, ONLY: ncolumnsDataOutBEERS
   USE module_util_time, ONLY: DAYLEN, SUEWS_cal_weekday, SUEWS_cal_dectime, &
                          Day_Of_Week, SUEWS_cal_DLS, Days_of_Year, LeapYearCalc, day2month, &
                          SUEWS_cal_tstep, month2day, dectime_to_timevec

   IMPLICIT NONE
   REAL(KIND(1D0)), PARAMETER :: pi = ATAN(1.)*4
   REAL(KIND(1D0)), PARAMETER :: DEG2RAD = pi/180
   REAL(KIND(1D0)), PARAMETER :: RAD2deg = 1/DEG2RAD

CONTAINS

   SUBROUTINE BEERS_cal_main(iy, id, dectime, lamdaP, lamdaF, avkdn, ldown, Temp_C, avrh, &
                             Press_hPa, Tsurf, lat, lng, alt, timezone, zenith_deg, azimuth, &
                             alb_ground, alb_bldg, emis_ground, emis_wall, &
                             !   kdir, kdiff, &
                             dataOutLineBEERS) ! output

      IMPLICIT NONE

      INTEGER, INTENT(in) :: iy
      INTEGER, INTENT(in) :: id
      REAL(KIND(1D0)), INTENT(in) :: lamdaP ! plan area fraction
      REAL(KIND(1D0)), INTENT(in) :: lamdaF ! frontal area fraction
      !REAL(KIND(1d0)), INTENT(in)::tilt ! Tilt of building in degrees. Could be included FL
      ! REAL(KIND(1d0)), intent(in) ::lai_id_dectr
      ! REAL(KIND(1d0)), intent(in) ::LAImax_dectr
      ! REAL(KIND(1D0)), intent(in)::TransMin        ! Tranmissivity of K through decidious vegetation (leaf on)
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: avrh
      REAL(KIND(1D0)), INTENT(in) :: avkdn
      REAL(KIND(1D0)), INTENT(in) :: ldown
      REAL(KIND(1D0)), INTENT(in) :: Tsurf
      ! REAL(KIND(1d0)), intent(in) :: kdiff !Actual inputs from metfile.
      ! REAL(KIND(1d0)), intent(in) :: kdir  !Actual inputs from metfile.
      REAL(KIND(1D0)), INTENT(in) :: zenith_deg
      REAL(KIND(1D0)), INTENT(in) :: azimuth
      REAL(KIND(1D0)), INTENT(in) :: dectime
      REAL(KIND(1D0)), INTENT(in) :: timezone, lat, lng, alt
      REAL(KIND(1D0)), INTENT(in) :: alb_bldg
      REAL(KIND(1D0)), INTENT(in) :: alb_ground
      REAL(KIND(1D0)), INTENT(in) :: emis_wall
      REAL(KIND(1D0)), INTENT(in) :: emis_ground

      REAL(KIND(1D0)), PARAMETER :: absL = 0.97 ! Absorption coefficient of longwave radiation of a person
      REAL(KIND(1D0)), PARAMETER :: absK = 0.7 ! Absorption coefficient of shortwave radiation of a person
      REAL(KIND(1D0)), PARAMETER :: Fside = 0.22 ! Standing human shape factor
      REAL(KIND(1D0)), PARAMETER :: Fup = 0.06 ! Standing human shape factor

      ! integer, parameter :: ncolumnsDataOutSol = 34      ! Standing human shape factor
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5), INTENT(OUT) :: dataOutLineBEERS ! 26 columns of output at the moment

      REAL(KIND(1D0)) :: t, psi
      REAL(KIND(1D0)) :: altitude, zen !azimuth,zenith
      REAL(KIND(1D0)) :: CI, c, I0, Kt, Tw, Tg
      REAL(KIND(1D0)) :: Ta, RH, P, radG, radD, radI !,idectime,tdectime!dectime,
      REAL(KIND(1D0)) :: I0et, CIuncorr !,lati
      REAL(KIND(1D0)) :: SNDN, SNUP, DEC, DAYL !,timestepdec,YEAR
      REAL(KIND(1D0)) :: msteg, emis_sky, ea
      REAL(KIND(1D0)) :: shadowground, shadowwalls, shadowroof
      ! REAL(KIND(1d0)),intent(in) ::lai_id
      INTEGER :: DOY !,ith!onlyglobal,usevegdem,x,y,i,  first, second,
      REAL(KIND(1D0)) :: CIlatenight
      REAL(KIND(1D0)) :: dectime_sunrise, zen_sunrise, I0_sunrise
      ! REAL(KIND(1d0)) :: Fside ! fraction of a person seen from each cardinal point
      ! REAL(KIND(1d0)) :: Fup ! fraction of a person seen from down and up
      REAL(KIND(1D0)) :: HW ! building height to width ratio

      REAL(KIND(1D0)) :: svfalfa !, sos
      !REAL(KIND(1d0)) :: gvf   !Ground View Factors (GVF)
      REAL(KIND(1D0)) :: Tmrt, Sstr, F_sh
      !REAL(KIND(1d0))  :: vegsh
      REAL(KIND(1D0)) :: tmp, altmax
      REAL(KIND(1D0)) :: svf_bldg_veg
      REAL(KIND(1D0)) :: svf_ground, svf_roof
      REAL(KIND(1D0)) :: svf_veg
      REAL(KIND(1D0)) :: svf_aveg
      REAL(KIND(1D0)) :: Kdown, Keast, Knorth, Ksouth, Kup2d, Kwest
      REAL(KIND(1D0)) :: Ldown2d, Least, Lnorth, Lsouth, Lup2d, Lwest

      ! Internal grids
      !Search directions for Ground View Factors (GVF)
      !REAL(KIND(1d0)), PARAMETER :: azimuthA(1:18) = [(j*(360.0/18.0), j=0, 17)]
      ! temporary parameters and variables for testing
      ! REAL(KIND(1d0)), PARAMETER   :: pi = 3.141592653589793
      REAL(KIND(1D0)), PARAMETER :: SBC = 5.67051E-8

      INTEGER, PARAMETER :: onlyglobal = 1 !! force to calculate direct and diffuse components, TS 13 Dec 2019 !TODO: should be input parameter FL
      !INTEGER, PARAMETER:: usevegdem = 0  !! force not to use vegetation DEM based calculations, TS 13 Dec 2019
      !INTEGER, PARAMETER:: row = 1  !! force to 1, TS 13 Dec 2019
      !INTEGER, PARAMETER:: col = 1  !! force to 1, TS 13 Dec 2019
      !INTEGER, PARAMETER:: Posture = 1  !! force to 1, TS 13 Dec 2019 ! Not used FL
      ! INTEGER, PARAMETER:: SOLWEIG_ldown = 0  !! force to 0, TS 13 Dec 2019 !TODO: should be input parameter FL
      ! this is just for testing
      INTEGER, PARAMETER :: SOLWEIG_ldown = 1 !! force to 0, TS 13 Dec 2019 !TODO: should be input parameter FL
      INTEGER, PARAMETER :: BEERS_tsurf = 1 !TODO: should be input parameter FL

!!!!!! Begin program !!!!!!
      ! internal grids
      ! ALLOCATE (tmp(1, 1))
      !ALLOCATE (Knight(1, 1))
      !ALLOCATE (Tgmap0(1, 1))
      ! ALLOCATE (svfbuveg(1, 1))
      !allocate(Tgmap0(1,1))

      ! initialise this as ONE
      CIlatenight = 1
      CI = 1

      ! These variables should change name in the future...
      P = Press_hPa
      Ta = Temp_C
      RH = avrh
      radG = avkdn
      DOY = INT(id)
      ! radD = kdiff
      ! radI = kdir

      psi = 0.03 ! Tranmissivity of K through vegetation

      ! alb_bldg = alb(2) ! taken from Bldg (FunctionalTypes)
      ! alb_ground = alb(1) ! taken from Paved (FunctionalTypes)
      ! emis_wall = emis(2) ! taken from Bldg (FunctionalTypes)
      ! emis_ground = emis(1) ! taken from Paved (FunctionalTypes)

      ! Building azimuth offset from cardinal points in degrees
      t = 0 !tilt

      HW = cal_ratio_height2width(lamdaP, lamdaF)

      ! parameterisation using NYC building data
      ! TODO: #5 which SVF should be used here? in python code, SVF_roof is used.
      ! Both are used. svfr for roof fluxes. Changed in code below. FL
      svf_ground = hwToSVF_ground(hw) !TODO: Should change based on Oke equation???
      svf_roof = hwToSVF_roof(hw) !TODO: Should change based on Oke equation???

      svf_veg = 1 ! svfveg: SVF based on vegetation blocking the sky (1 = no vegetation)
      svf_aveg = 1 ! view factor where vegetation is in view before buildings.

      tmp = 1 - (svf_ground + svf_veg - 1)
      IF (tmp <= 1.E-6) tmp = 1.E-6 ! avoiding log(0)
      svfalfa = ASIN(EXP(LOG(tmp)/2))

      ! SVF combines for buildings and vegetation
      svf_bldg_veg = (svf_ground - (1 - svf_veg)*(1 - psi))

      ! Sun position related things
      CALL DAYLEN(DOY, lat, DAYL, DEC, SNDN, SNUP)
      zen = zenith_deg*DEG2RAD
      altitude = 90 - zenith_deg

      !Determination of clear-sky emissivity from Prata (1996) !TODO: Is this calcualted in NARP?
      ea = 6.107*10**((7.5*Ta)/(237.3 + Ta))*(RH/100) !Vapor pressure
      msteg = 46.5*(ea/(Ta + 273.15))
      emis_sky = (1 - (1 + msteg)*EXP(-((1.2 + 3.0*msteg)**0.5)))

      !!! DAYTIME !!!
      IF (altitude > 0.1) THEN

         !Clearness Index on Earth's surface after Crawford and Dunchon (1999) with a correction
         !factor for low sun elevations after Lindberg et al. (2008)
         CALL clearnessindex_2013b(zen, DOY, Ta, RH/100., radG, lat, P/10., & !input
                                   I0, CI, Kt, I0et, CIuncorr) !output
         ! IF (CI > 1) CI = 1

         !Estimation of radD and radI if not measured after Reindl et al. (1990)
         IF (onlyglobal == 1) THEN
            CALL diffusefraction(radG, altitude, Kt, Ta, RH, radI, radD)
         END IF

         CALL shadowGroundKusaka(HW, azimuth, zen, shadowground, shadowwalls)
         shadowroof = 1. ! TODO: should change with time of day etc. Could be parameterizised from e.g. Lindberg et al. 2015 SE

         CALL cylindric_wedge(zen, svfalfa, F_sh)

         !!! Calculation of shortwave daytime radiative fluxes !!!
         CALL KRoof(radI, radD, radG, F_sh, altitude, svf_roof, svf_veg, shadowroof, psi, alb_bldg, Kdown)
         !Kdown2d = radI*shadowroof*SIN(altitude*DEG2RAD) &
         !          + radD*svfr &
         !          ! TODO: #6 F_sh issue: used below is calculated as a variable but
         !          + alb_bldg*(1 - svfr)*(radG*(1 - F_sh) + radD*F_sh)

         Kup2d = alb_ground*( &
                 radI*shadowground*SIN(altitude*DEG2RAD) & ! gvf not defined TODO #2 FIXED
                 + radD*svf_bldg_veg &
                 + alb_bldg*(1 - svf_bldg_veg)*(radG*(1 - F_sh) + radD*F_sh))

         ! TODO: #7 check consistency with python code
         CALL KWalls( &
            svf_ground, svf_veg, shadowground, F_sh, &
            radI, radG, radD, azimuth, altitude, psi, t, alb_ground, alb_bldg, & ! input
            Keast, Knorth, Ksouth, Kwest) ! output

         IF (BEERS_tsurf == 1) THEN
            CALL tSurfBEERS(iy, Ta, RH, radI, I0, dectime, SNUP, altitude, zen, timezone, lat, lng, alt, &
                            Tg, Tw, altmax)
         ELSE
            Tg = Tsurf - Ta !TODO: Tg is the difference (added temperature between TA and Tg) in BEERS
            ! Tg = Tsurf ! changed to absolute sense
            Tw = Tg
         END IF

         !!!! Lup, daytime !!!!
         Lup2d = SBC*emis_ground*((shadowground*Tg + Ta + 273.15)**4)

      !!!!!!! NIGHTTIME !!!!!!!!
      ELSE
         CALL cal_CI_latenight(iy, DOY, Ta, RH/100., radG, lat, P/10., & !input
                               CIlatenight, dectime_sunrise, zen_sunrise, I0_sunrise) !output
         CI = CIlatenight
         I0 = I0_sunrise
         shadowground = 0
         shadowwalls = 0
         shadowroof = 0
         ! Tg = Temp_C !TODO: #11 This need some thought. Use ESTM to improve?
         ! Tw = Temp_C !TODO: This need some thought. Use ESTM to improve?
         IF (BEERS_tsurf == 1) THEN
            Tw = 0
            Tg = 0
         ELSE
            Tg = Tsurf - Ta !TODO: Tg is the difference (added temperature between Ta and Tg) in BEERS
            Tw = Tg
         END IF
         radI = 0
         radD = 0
         F_sh = 1

         !Nocturnal cloud fraction from Offerle et al. 2003
         IF (dectime < (DOY + 0.5) .AND. dectime > DOY .AND. altitude < 1.0) THEN !TODO: THIS (STILL, 20201117) NEED SOME THOUGHT 20150211
            !j=0
            !do while (dectime<(DOY+SNUP/24))
            !!    call ConvertMetData(ith+j) ! read data at sunrise ??
            !    j=j+1
            !end do
            !call NARP_cal_SunPosition(year,dectime,timezone,lat,lng,alt,azimuth,zenith_deg)!this is not good
            !zen=zenith_deg*DEG2RAD
            !call clearnessindex_2013b(zen,DOY,Temp_C,RH/100,avkdn,lat,Press_hPa,I0,CI,Kt,I0et,CIuncorr)
            !!call ConvertMetData(ith) ! read data at current timestep again ??
            !call NARP_cal_SunPosition(year,dectime,timezone,lat,lng,alt,azimuth,zenith_deg)!this is not good

            CI = 1.0
         ELSE
            ! IF (SolweigCount == 1) THEN
            !    CI = 1.0
            ! ELSE
            !    CI = CIlatenight
            ! END IF
            CI = CIlatenight
         END IF

         radI = 0
         radD = 0

         !Nocturnal Kfluxes set to 0
         Kdown = 0.0
         Kwest = 0.0
         Kup2d = 0.0
         Keast = 0.0
         Ksouth = 0.0
         Knorth = 0.0

         !!! Lup !!!
         Lup2d = SBC*emis_ground*((Ta + Tg + 273.15)**4)

      END IF

      !!! Ldown !!!
      IF (SOLWEIG_ldown == 1) THEN ! Third
         Ldown2d = (svf_roof + svf_veg - 1)*emis_sky*SBC*((Ta + 273.15)**4) &
                   + (2 - svf_veg - svf_aveg)*emis_wall*SBC*((Ta + 273.15)**4) &
                   + (svf_aveg - svf_roof)*emis_wall*SBC*((Ta + 273.15 + Tw)**4) &
                   + (2 - svf_roof - svf_veg)*(1 - emis_wall)*emis_sky*SBC*((Ta + 273.15)**4)

         IF (CI < 0.95) THEN !non-clear conditions
            c = 1 - CI
            Ldown2d = Ldown2d*(1 - c) + c*( &
                      (svf_roof + svf_veg - 1)*SBC*((Ta + 273.15)**4) &
                      + (2 - svf_veg - svf_aveg)*emis_wall*SBC*((Ta + 273.15)**4) &
                      + (svf_aveg - svf_roof)*emis_wall*SBC*((Ta + 273.15 + Tw)**4) &
                      + (2 - svf_roof - svf_veg)*(1 - emis_wall)*SBC*((Ta + 273.15)**4))
         END IF

      ELSE
         Ldown2d = (svf_roof + svf_veg - 1)*ldown &
                   + (2 - svf_veg - svf_aveg)*emis_wall*SBC*((Ta + 273.15)**4) &
                   + (svf_aveg - svf_roof)*emis_wall*SBC*((Ta + 273.15 + Tw)**4) &
                   + (2 - svf_roof - svf_veg)*(1 - emis_wall)*ldown
      END IF

      !!! Lside !!!
      CALL LWalls(svf_ground, svf_veg, svf_aveg, &
                  Ldown2d, Lup2d, &
                  altitude, Ta, Tw, SBC, emis_wall, &
                  emis_sky, t, CI, azimuth, ldown, svfalfa, F_sh, &
                  Least, Lnorth, Lsouth, Lwest) ! output

      !!! Calculation of radiant flux density and Tmrt (Mean radiant temperature) !!!
      Sstr = absK*(Kdown*Fup + Kup2d*Fup + Knorth*Fside + Keast*Fside + Ksouth*Fside + Kwest*Fside) &
             + absL*(Ldown2d*Fup + Lup2d*Fup + Lnorth*Fside + Least*Fside + Lsouth*Fside + Lwest*Fside)
      Tmrt = SQRT(SQRT((Sstr/(absL*SBC)))) - 273.15

      dataOutLineBEERS = [azimuth, altitude, radG, radI, radD, &
                          Kdown, Kup2d, Ksouth, Kwest, Knorth, Keast, &
                          Ldown2d, Lup2d, Lsouth, Lwest, Lnorth, Least, &
                          Tmrt, I0, CI, shadowground, shadowwalls, svf_ground, svf_roof, svf_bldg_veg, &
                          emis_sky, &
                          Ta, Tg, Tw]

      ! DEALLOCATE (tmp)
      ! DEALLOCATE (svfbuveg)

   END SUBROUTINE BEERS_cal_main

   SUBROUTINE BEERS_cal_main_DTS( &
      timer, config, forcing, siteInfo, & ! input
      modState, & ! input/output:
      dataOutLineBEERS) ! output

      USE module_ctrl_type, ONLY: SUEWS_CONFIG, SUEWS_TIMER, SUEWS_FORCING, SUEWS_SITE, &
                               PHENOLOGY_STATE, LC_PAVED_PRM, LC_BLDG_PRM, &
                               ROUGHNESS_STATE, HEAT_STATE, solar_State, &
                               SUEWS_STATE, BldgSurf, STEBBS_STATE

      IMPLICIT NONE
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      REAL(KIND(1D0)), PARAMETER :: absL = 0.97 ! Absorption coefficient of longwave radiation of a person
      REAL(KIND(1D0)), PARAMETER :: absK = 0.7 ! Absorption coefficient of shortwave radiation of a person
      REAL(KIND(1D0)), PARAMETER :: Fside = 0.22 ! Standing human shape factor
      REAL(KIND(1D0)), PARAMETER :: Fup = 0.06 ! Standing human shape factor

      ! integer, parameter :: ncolumnsDataOutSol = 34      ! Standing human shape factor
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5), INTENT(OUT) :: dataOutLineBEERS ! 26 columns of output at the moment

      REAL(KIND(1D0)) :: t, psi
      REAL(KIND(1D0)) :: altitude, zen !azimuth,zenith
      REAL(KIND(1D0)) :: CI, c, I0, Kt, Tw, Tg
      REAL(KIND(1D0)) :: Ta, RH, P, radG, radD, radI !,idectime,tdectime!dectime,
      REAL(KIND(1D0)) :: I0et, CIuncorr !,lati
      REAL(KIND(1D0)) :: SNDN, SNUP, DEC, DAYL !,timestepdec,YEAR
      REAL(KIND(1D0)) :: msteg, emis_sky, ea
      REAL(KIND(1D0)) :: shadowground, shadowwalls, shadowroof
      ! REAL(KIND(1d0)),intent(in) ::lai_id
      ! INTEGER :: DOY !,ith!onlyglobal,usevegdem,x,y,i,  first, second,
      REAL(KIND(1D0)) :: CIlatenight
      REAL(KIND(1D0)) :: dectime_sunrise, zen_sunrise, I0_sunrise
      ! REAL(KIND(1d0)) :: Fside ! fraction of a person seen from each cardinal point
      ! REAL(KIND(1d0)) :: Fup ! fraction of a person seen from down and up
      REAL(KIND(1D0)) :: HW ! building height to width ratio

      REAL(KIND(1D0)) :: svfalfa !, sos
      !REAL(KIND(1d0)) :: gvf   !Ground View Factors (GVF)
      REAL(KIND(1D0)) :: Tmrt, Sstr, F_sh
      !REAL(KIND(1d0))  :: vegsh
      REAL(KIND(1D0)) :: tmp, altmax
      REAL(KIND(1D0)) :: svf_bldg_veg
      REAL(KIND(1D0)) :: svf_ground, svf_roof
      REAL(KIND(1D0)) :: svf_veg
      REAL(KIND(1D0)) :: svf_aveg
      ! REAL(KIND(1D0)) :: Kdown, Keast, Knorth, Ksouth, Kup2d, Kwest
      ! REAL(KIND(1D0)) :: Ldown2d, Least, Lnorth, Lsouth, Lup2d, Lwest

      ! Internal grids
      !Search directions for Ground View Factors (GVF)
      !REAL(KIND(1d0)), PARAMETER :: azimuthA(1:18) = [(j*(360.0/18.0), j=0, 17)]
      ! temporary parameters and variables for testing
      ! REAL(KIND(1d0)), PARAMETER   :: pi = 3.141592653589793
      REAL(KIND(1D0)), PARAMETER :: SBC = 5.67051E-8

      INTEGER, PARAMETER :: onlyglobal = 1 !! force to calculate direct and diffuse components, TS 13 Dec 2019 !TODO: should be input parameter FL
      !INTEGER, PARAMETER:: usevegdem = 0  !! force not to use vegetation DEM based calculations, TS 13 Dec 2019
      !INTEGER, PARAMETER:: row = 1  !! force to 1, TS 13 Dec 2019
      !INTEGER, PARAMETER:: col = 1  !! force to 1, TS 13 Dec 2019
      !INTEGER, PARAMETER:: Posture = 1  !! force to 1, TS 13 Dec 2019 ! Not used FL
      ! INTEGER, PARAMETER:: SOLWEIG_ldown = 0  !! force to 0, TS 13 Dec 2019 !TODO: should be input parameter FL
      ! this is just for testing
      INTEGER, PARAMETER :: SOLWEIG_ldown = 0 !! force to 0, TS 13 Dec 2019 !TODO: should be input parameter FL
      INTEGER, PARAMETER :: BEERS_tsurf = 1 !TODO: should be input parameter FL

      ASSOCIATE ( &
         roughnessState => modState%roughnessState, &
         heatState => modState%heatState, &
         solarState => modState%solarState, &
         phenState => modState%phenState, &
         stebbsState => modState%stebbsState &
         )
         ASSOCIATE ( &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            evetrPrm => siteInfo%lc_evetr, &
            dectrPrm => siteInfo%lc_dectr, &
            grassPrm => siteInfo%lc_grass, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            ehcPrm => siteInfo%ehc, &
            nlayer => siteInfo%nlayer, &
            sfr_surf => siteInfo%sfr_surf, &
            sfr_roof => siteInfo%sfr_roof, &
            sfr_wall => siteInfo%sfr_wall, &
            SurfaceArea => siteInfo%SurfaceArea, &
            snowPrm => siteInfo%snow, &
            PipeCapacity => siteInfo%PipeCapacity, &
            RunoffToWater => siteInfo%RunoffToWater, &
            FlowChange => siteInfo%FlowChange, &
            PervFraction => siteInfo%PervFraction, &
            vegfraction => siteInfo%vegfraction, &
            NonWaterFraction => siteInfo%NonWaterFraction, &
            zMeas => siteInfo%z, &
            lat => siteInfo%lat, &
            lng => siteInfo%lon, &
            alt => siteInfo%alt, &
            timezone => siteInfo%timezone, &
            conductancePrm => siteInfo%conductance, &
            tstep_real => timer%tstep_real, &
            avkdn => forcing%kdown, &
            xsmd => forcing%xsmd, &
            Temp_C => forcing%Temp_C, &
            avU1 => forcing%U, &
            avRH => forcing%RH, &
            Press_hPa => forcing%pres, &
            LAI_id => phenState%LAI_id, &
            gfunc => phenState%gfunc, &
            PAI => roughnessState%PAI, &
            FAI => roughnessState%FAI, &
            ldown => heatState%ldown, &
            TSfc_C => heatState%TSfc_C, &
            zenith_deg => solarState%zenith_deg, &
            azimuth_deg => solarState%azimuth_deg, &
            iy => timer%iy, &
            it => timer%it, &
            id => timer%id, &
            dectime => timer%dectime, &
            SMDMethod => config%SMDMethod, &
            storageheatmethod => config%StorageHeatMethod, &
            DiagMethod => config%DiagMethod, &
            StabilityMethod => config%StabilityMethod, &
            EmissionsMethod => config%EmissionsMethod, &
            Diagnose => config%Diagnose, &
            Kdown2d => stebbsState%Kdown2d, &
            Kup2d => stebbsState%Kup2d, &
            Kwest => stebbsState%Kwest, &
            Keast => stebbsState%Keast, &
            Knorth => stebbsState%Knorth, &
            Ksouth => stebbsState%Ksouth, &
            Ldown2d => stebbsState%Ldown2d, &
            Lup2d => stebbsState%Lup2d, &
            Lwest => stebbsState%Lwest, &
            Least => stebbsState%Least, &
            Lnorth => stebbsState%Lnorth, &
            Lsouth => stebbsState%Lsouth &
            )
            IF (sfr_surf(BldgSurf) > 0) THEN
               ! do BEERS calculation
               PAI = sfr_surf(2)/SUM(sfr_surf(1:2))

               ASSOCIATE ( &
                  emis_ground => pavedPrm%emis, &
                  emis_wall => bldgPrm%emis, &
                  alb_ground => phenState%alb(1), &
                  alb_bldg => phenState%alb(2), &
                  P => Press_hPa, &
                  Ta => Temp_C, &
                  RH => avrh, &
                  radG => avkdn, &
                  DOY => INT(id) &
                  )

                  ! initialise this as ONE
                  CIlatenight = 1
                  CI = 1
                  psi = 0.03 ! Tranmissivity of K through vegetation

                  ! Building azimuth offset from cardinal points in degrees
                  t = 0 !tilt

                  HW = cal_ratio_height2width(PAI, FAI)

                  ! parameterisation using NYC building data
                  ! TODO: #5 which SVF should be used here? in python code, SVF_roof is used.
                  ! Both are used. svfr for roof fluxes. Changed in code below. FL
                  svf_ground = hwToSVF_ground(hw) !TODO: Should change based on Oke equation???
                  svf_roof = hwToSVF_roof(hw) !TODO: Should change based on Oke equation???

                  svf_veg = 1 ! svfveg: SVF based on vegetation blocking the sky (1 = no vegetation)
                  svf_aveg = 1 ! view factor where vegetation is in view before buildings.

                  tmp = 1 - (svf_ground + svf_veg - 1)
                  IF (tmp <= 1.E-6) tmp = 1.E-6 ! avoiding log(0)
                  svfalfa = ASIN(EXP(LOG(tmp)/2))

                  ! SVF combines for buildings and vegetation
                  svf_bldg_veg = (svf_ground - (1 - svf_veg)*(1 - psi))

                  ! Sun position related things
                  CALL DAYLEN(DOY, lat, DAYL, DEC, SNDN, SNUP)
                  zen = zenith_deg*DEG2RAD
                  altitude = 90 - zenith_deg

                  !Determination of clear-sky emissivity from Prata (1996) !TODO: Is this calcualted in NARP?
                  ea = 6.107*10**((7.5*Ta)/(237.3 + Ta))*(RH/100) !Vapor pressure
                  msteg = 46.5*(ea/(Ta + 273.15))
                  emis_sky = (1 - (1 + msteg)*EXP(-((1.2 + 3.0*msteg)**0.5)))

                  ! DAYTIME
                  IF (altitude > 0.1) THEN

                     !Clearness Index on Earth's surface after Crawford and Dunchon (1999) with a correction
                     !factor for low sun elevations after Lindberg et al. (2008)
                     CALL clearnessindex_2013b(zen, DOY, Ta, RH/100., radG, lat, P/10., & !input
                                               I0, CI, Kt, I0et, CIuncorr) !output
                     ! IF (CI > 1) CI = 1

                     !Estimation of radD and radI if not measured after Reindl et al. (1990)
                     IF (onlyglobal == 1) THEN
                        CALL diffusefraction(radG, altitude, Kt, Ta, RH, radI, radD)
                     END IF

                     CALL shadowGroundKusaka(HW, azimuth_deg, zen, shadowground, shadowwalls)
                     shadowroof = 1. ! TODO: should change with time of day etc. Could be parameterizised from e.g. Lindberg et al. 2015 SE

                     CALL cylindric_wedge(zen, svfalfa, F_sh)

                     ! Calculation of shortwave daytime radiative fluxes !!!
                     CALL KRoof(radI, radD, radG, F_sh, altitude, svf_roof, svf_veg, shadowroof, psi, alb_bldg, Kdown2d)
                     !Kdown2d = radI*shadowroof*SIN(altitude*DEG2RAD) &
                     !          + radD*svfr &
                     !          ! TODO: #6 F_sh issue: used below is calculated as a variable but
                     !          + alb_bldg*(1 - svfr)*(radG*(1 - F_sh) + radD*F_sh)

                     Kup2d = alb_ground*( &
                             radI*shadowground*SIN(altitude*DEG2RAD) & ! gvf not defined TODO #2 FIXED
                             + radD*svf_bldg_veg &
                             + alb_bldg*(1 - svf_bldg_veg)*(radG*(1 - F_sh) + radD*F_sh))

                     ! TODO: #7 check consistency with python code
                     CALL KWalls( &
                        svf_ground, svf_veg, shadowground, F_sh, &
                        radI, radG, radD, azimuth_deg, altitude, psi, t, alb_ground, alb_bldg, & ! input
                        Keast, Knorth, Ksouth, Kwest) ! output

                     IF (BEERS_tsurf == 1) THEN
                        CALL tSurfBEERS(iy, Ta, RH, radI, I0, dectime, SNUP, altitude, zen, timezone, lat, lng, alt, &
                                        Tg, Tw, altmax)
                     ELSE
                        Tg = TSfc_C - Ta !TODO: Tg is the difference (added temperature between TA and Tg) in BEERS
                        ! Tg = Tsurf ! changed to absolute sense
                        Tw = Tg
                     END IF

                     ! Lup, daytime !!!!
                     Lup2d = SBC*emis_ground*((shadowground*Tg + Ta + 273.15)**4)

                     ! NIGHTTIME !!!!!!!!
                  ELSE
                     CALL cal_CI_latenight(iy, DOY, Ta, RH/100., radG, lat, P/10., & !input
                                           CIlatenight, dectime_sunrise, zen_sunrise, I0_sunrise) !output
                     CI = CIlatenight
                     I0 = I0_sunrise
                     shadowground = 0
                     shadowwalls = 0
                     shadowroof = 0
                     ! Tg = Temp_C !TODO: #11 This need some thought. Use ESTM to improve?
                     ! Tw = Temp_C !TODO: This need some thought. Use ESTM to improve?
                     IF (BEERS_tsurf == 1) THEN
                        Tw = 0
                        Tg = 0
                     ELSE
                        Tg = TSfc_C - Ta !TODO: Tg is the difference (added temperature between Ta and Tg) in BEERS
                        Tw = Tg
                     END IF
                     radI = 0
                     radD = 0
                     F_sh = 1

                     !Nocturnal cloud fraction from Offerle et al. 2003
                     IF (dectime < (DOY + 0.5) .AND. dectime > DOY .AND. altitude < 1.0) THEN !TODO: THIS (STILL, 20201117) NEED SOME THOUGHT 20150211
                        !j=0
                        !do while (dectime<(DOY+SNUP/24))
                        !    call ConvertMetData(ith+j) ! read data at sunrise ??
                        !    j=j+1
                        !end do
                        !call NARP_cal_SunPosition(year,dectime,timezone,lat,lng,alt,azimuth,zenith_deg)!this is not good
                        !zen=zenith_deg*DEG2RAD
                        !call clearnessindex_2013b(zen,DOY,Temp_C,RH/100,avkdn,lat,Press_hPa,I0,CI,Kt,I0et,CIuncorr)
                        !call ConvertMetData(ith) ! read data at current timestep again ??
                        !call NARP_cal_SunPosition(year,dectime,timezone,lat,lng,alt,azimuth,zenith_deg)!this is not good

                        CI = 1.0
                     ELSE
                        ! IF (SolweigCount == 1) THEN
                        !    CI = 1.0
                        ! ELSE
                        !    CI = CIlatenight
                        ! END IF
                        CI = CIlatenight
                     END IF

                     radI = 0
                     radD = 0

                     !Nocturnal Kfluxes set to 0
                     Kdown2d = 0.0
                     Kwest = 0.0
                     Kup2d = 0.0
                     Keast = 0.0
                     Ksouth = 0.0
                     Knorth = 0.0

                     ! Lup !!!
                     Lup2d = SBC*emis_ground*((Ta + Tg + 273.15)**4)

                  END IF

                  ! Ldown !!!
                  IF (SOLWEIG_ldown == 1) THEN ! Third
                     ! ldown is calculated in BEERS as a function of Ta
                     Ldown2d = (svf_roof + svf_veg - 1)*emis_sky*SBC*((Ta + 273.15)**4) &
                               + (2 - svf_veg - svf_aveg)*emis_wall*SBC*((Ta + 273.15)**4) &
                               + (svf_aveg - svf_roof)*emis_wall*SBC*((Ta + 273.15 + Tw)**4) &
                               + (2 - svf_roof - svf_veg)*(1 - emis_wall)*emis_sky*SBC*((Ta + 273.15)**4)

                     IF (CI < 0.95) THEN !non-clear conditions
                        c = 1 - CI
                        Ldown2d = Ldown2d*(1 - c) + c*( &
                                  (svf_roof + svf_veg - 1)*SBC*((Ta + 273.15)**4) &
                                  + (2 - svf_veg - svf_aveg)*emis_wall*SBC*((Ta + 273.15)**4) &
                                  + (svf_aveg - svf_roof)*emis_wall*SBC*((Ta + 273.15 + Tw)**4) &
                                  + (2 - svf_roof - svf_veg)*(1 - emis_wall)*SBC*((Ta + 273.15)**4))
                     END IF

                  ELSE
                     ! use ldown from SUEWS
                     Ldown2d = (svf_roof + svf_veg - 1)*ldown &
                               + (2 - svf_veg - svf_aveg)*emis_wall*SBC*((Ta + 273.15)**4) &
                               + (svf_aveg - svf_roof)*emis_wall*SBC*((Ta + 273.15 + Tw)**4) &
                               + (2 - svf_roof - svf_veg)*(1 - emis_wall)*ldown
                  END IF

                  ! Lside !!!
                  CALL LWalls(svf_ground, svf_veg, svf_aveg, &
                              Ldown2d, Lup2d, &
                              altitude, Ta, Tw, SBC, emis_wall, &
                              emis_sky, t, CI, azimuth_deg, ldown, svfalfa, F_sh, &
                              Least, Lnorth, Lsouth, Lwest) ! output

                  ! Calculation of radiant flux density and Tmrt (Mean radiant temperature) !!!
                  Sstr = absK*(Kdown2d*Fup + Kup2d*Fup + Knorth*Fside + Keast*Fside + Ksouth*Fside + Kwest*Fside) &
                         + absL*(Ldown2d*Fup + Lup2d*Fup + Lnorth*Fside + Least*Fside + Lsouth*Fside + Lwest*Fside)
                  Tmrt = SQRT(SQRT((Sstr/(absL*SBC)))) - 273.15

                  dataOutLineBEERS = [azimuth_deg, altitude, radG, radI, radD, &
                                      Kdown2d, Kup2d, Ksouth, Kwest, Knorth, Keast, &
                                      Ldown2d, Lup2d, Lsouth, Lwest, Lnorth, Least, &
                                      Tmrt, I0, CI, shadowground, shadowwalls, svf_ground, svf_roof, svf_bldg_veg, &
                                      emis_sky, &
                                      Ta, Tg, Tw]

               END ASSOCIATE
            ELSE
               ! set nan values to output
               dataOutLineBEERS = set_nan(dataOutLineBEERS)
            END IF
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE BEERS_cal_main_DTS

   SUBROUTINE cal_CI_latenight(iy, DOY, Ta_degC, RH_frac, radG, lat, P_kPa, &
                               CIlatenight, dectime_sunrise, zen_sunrise, I0_sunrise)
      ! subroutine to calculate nighttime clearness index
      ! Offerle et al. (2003) used sunset value but here a value after sunrise is calculated
      ! TODO: a value at sunset can be retained using module variables

      IMPLICIT NONE
      INTEGER, INTENT(in) :: iy
      INTEGER, INTENT(in) :: DOY
      ! INTEGER, intent(in)           :: timezone
      REAL(KIND(1D0)), INTENT(in) :: Ta_degC
      REAL(KIND(1D0)), INTENT(in) :: RH_frac
      REAL(KIND(1D0)), INTENT(in) :: P_kPa
      REAL(KIND(1D0)), INTENT(in) :: radG
      REAL(KIND(1D0)), INTENT(in) :: lat
      REAL(KIND(1D0)), INTENT(out) :: CIlatenight
      REAL(KIND(1D0)), INTENT(out) :: dectime_sunrise
      REAL(KIND(1D0)), INTENT(out) :: zen_sunrise
      REAL(KIND(1D0)), INTENT(out) :: I0_sunrise
      REAL(KIND(1D0)) :: I0et
      REAL(KIND(1D0)) :: CIuncorr
      REAL(KIND(1D0)) :: Kt
      REAL(KIND(1D0)) :: DAYL, DEC, SNDN, SNUP, azimuth

      CALL DAYLEN(DOY, lat, DAYL, DEC, SNDN, SNUP)
      dectime_sunrise = DOY + (SNUP + .5)/24.
      CALL NARP_cal_SunPosition( &
         REAL(iy, KIND(1D0)), & !input:
         dectime_sunrise, & ! sun position at middle of timestep before
         0.D0, lat, 0.D0, 100.D0, &
         azimuth, zen_sunrise) !output:

      zen_sunrise = zen_sunrise/180.*pi

      CALL clearnessindex_2013b(zen_sunrise, DOY, Ta_degC, RH_frac, radG, lat, P_kPa, & !input
                                I0_sunrise, CIlatenight, Kt, I0et, CIuncorr) !output

   END SUBROUTINE cal_CI_latenight

   SUBROUTINE KRoof( &
      radI, radD, radG, F_sh, altitude, svfr, svfveg, shadow, psi, alb_bldg, & ! input
      Kdown) ! output

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: radI
      REAL(KIND(1D0)), INTENT(in) :: radG
      REAL(KIND(1D0)), INTENT(in) :: radD
      ! REAL(KIND(1D0)), intent(in)   :: zen
      REAL(KIND(1D0)), INTENT(in) :: altitude
      REAL(KIND(1D0)), INTENT(in) :: svfr
      REAL(KIND(1D0)), INTENT(in) :: psi
      REAL(KIND(1D0)), INTENT(in) :: alb_bldg
      REAL(KIND(1D0)), INTENT(in) :: shadow, svfveg
      REAL(KIND(1D0)), INTENT(in) :: F_sh
      REAL(KIND(1D0)), INTENT(out) :: Kdown

      ! REAL(KIND(1D0)) :: svfalfa, tmp
      REAL(KIND(1D0)) :: svfrbuveg

      !Building height angle from svfr
      ! tmp = 1 - (svfr + svfveg - 1)
      ! if (tmp <= 0) tmp = 0.000000001 ! avoiding log(0)
      ! svfalfa = ASIN(EXP(LOG(tmp)/2))
      ! call cal_svfalfa(svfr, svfveg, svfalfa, tmp)

      ! ! Fraction shadow on building walls based on sun altitude and svf
      ! IF (altitude > 0) THEN
      !    call cylindric_wedge(zen, svfalfa, F_sh)
      !    if (F_sh < 0) F_sh = 0.0
      !    if (F_sh > 0.5) F_sh = 0.5 !TODO #8 why there is no such restriction in python code?
      ! else
      !    F_sh = 1.
      ! end if

      svfrbuveg = svfr - (1.0 - svfveg)*(1.0 - psi)

      Kdown = radI*shadow*SIN(altitude*DEG2RAD) &
              + radD*svfrbuveg &
              + alb_bldg*(1 - svfrbuveg)*(radG*(1 - F_sh) + radD*F_sh)

   END SUBROUTINE KRoof

   SUBROUTINE cal_svfalfa(svfr, svfveg, svfalfa, tmp)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: svfr ! svf of roof
      REAL(KIND(1D0)), INTENT(in) :: svfveg ! svf of vegetation
      REAL(KIND(1D0)), INTENT(out) :: svfalfa ! Building height angle from svfr
      REAL(KIND(1D0)), INTENT(out) :: tmp

      ! TODO: we need to use wall area directly to get this
      !Building height angle from svfr
      tmp = 1.-(svfr + svfveg - 1.)
      IF (tmp <= 0) tmp = 0.000000001 ! avoiding log(0)
      svfalfa = ASIN(EXP(LOG(tmp)/2.))

   END SUBROUTINE cal_svfalfa

   FUNCTION cal_ratio_height2width(lamdaP, lamdaF) RESULT(HW)
      IMPLICIT NONE
      REAL(KIND(1D0)), PARAMETER :: a = 0.5598
      REAL(KIND(1D0)), PARAMETER :: b = -0.2485
      REAL(KIND(1D0)), PARAMETER :: c = 0.4112
      REAL(KIND(1D0)), PARAMETER :: d = -0.02528

      REAL(KIND(1D0)), INTENT(in) :: lamdaP ! plan area fraction
      REAL(KIND(1D0)), INTENT(in) :: lamdaF ! frontal area fraction
      REAL(KIND(1D0)) :: HW
      REAL(KIND(1D0)) :: lamdaW !wall area fraction (wallarea / total area)

      ! TODO: we need to use wall area directly to get this
      lamdaW = 4*lamdaF ! assuming square shaped buildings

      hw = (lamdaW*lamdaP)/(2*lamdaP*(1 - lamdaP))

   END FUNCTION cal_ratio_height2width

   FUNCTION hwToSVF_ground(hw) RESULT(svfGround)
      IMPLICIT NONE
      REAL(KIND(1D0)), PARAMETER :: a = 0.5598
      REAL(KIND(1D0)), PARAMETER :: b = -0.2485
      REAL(KIND(1D0)), PARAMETER :: c = 0.4112
      REAL(KIND(1D0)), PARAMETER :: d = -0.02528

      REAL(KIND(1D0)), INTENT(in) :: hw
      REAL(KIND(1D0)) :: svfGround

      ! SvfGround: Parameterisation based on NYC data (500x500 meter grid)
      svfGround = a*EXP(b*hw) + c*EXP(d*hw)

   END FUNCTION hwToSVF_ground

   FUNCTION hwToSVF_roof(hw) RESULT(svfRoof)
      IMPLICIT NONE
      REAL(KIND(1D0)), PARAMETER :: e = 0.5572
      REAL(KIND(1D0)), PARAMETER :: f = 0.0589
      REAL(KIND(1D0)), PARAMETER :: g = 0.4143

      REAL(KIND(1D0)), INTENT(in) :: hw
      REAL(KIND(1D0)) :: svfRoof

      ! SvfGround: Parameterisation based on NYC data (500x500 meter grid)
      svfRoof = e*EXP(-f*hw) + g

   END FUNCTION hwToSVF_roof

   SUBROUTINE tSurfBEERS(iy, Ta, RH, radI, I0, dectime, SNUP, altitude, zen, timezone, lat, lng, alt, &
                         Tg, Tgwall, altmax) ! output
      ! Ground surface temperature wave from SOLWEIG

      IMPLICIT NONE

      INTEGER, INTENT(in) :: iy
      REAL(KIND(1D0)), INTENT(in) :: Ta, RH, radI, I0
      REAL(KIND(1D0)), INTENT(in) :: dectime, SNUP, altitude, zen
      REAL(KIND(1D0)), INTENT(in) :: timezone, lat, lng, alt
      REAL(KIND(1D0)), INTENT(out) :: Tg, Tgwall, altmax
      REAL(KIND(1D0)), PARAMETER :: TgK = 0.37
      REAL(KIND(1D0)), PARAMETER :: Tstart = 3.41
      REAL(KIND(1D0)), PARAMETER :: TgK_wall = 0.37
      REAL(KIND(1D0)), PARAMETER :: Tstart_wall = -3.41
      REAL(KIND(1D0)), PARAMETER :: TmaxLST = 15.
      REAL(KIND(1D0)), PARAMETER :: TmaxLST_wall = 15.

      REAL(KIND(1D0)) :: Ktc, notU, Tgamp, Tgampwall, radI0, corr, CI_Tg
      REAL(KIND(1D0)) :: fifteen, sunmaximum, zen_sunmax, dectimemax, azimuth

      ! finding daily maximum solar altitude
      fifteen = 0.
      sunmaximum = -90.
      zen_sunmax = 90.
      DO WHILE (sunmaximum <= 90.-zen_sunmax)
         sunmaximum = 90.-zen_sunmax
         fifteen = fifteen + 15./1440.
         dectimemax = FLOOR(dectime) + 10/24.+fifteen
         CALL NARP_cal_SunPosition( &
            REAL(iy, KIND(1D0)), & !input:
            dectimemax, & ! sun position at middle of timestep before
            timezone, lat, lng, alt, &
            azimuth, zen_sunmax) !output:
      END DO

      altmax = sunmaximum

      Tgamp = (TgK*altmax - Tstart) + Tstart
      Tgampwall = (TgK_wall*altmax - (Tstart_wall)) + (Tstart_wall)
      Tg = Tgamp*SIN((((dectime - FLOOR(dectime)) - SNUP/24)/(TmaxLST/24 - SNUP/24))*pi/2) + Tstart
      Tgwall = Tgampwall*SIN((((dectime - FLOOR(dectime)) - SNUP/24)/(TmaxLST_wall/24 - SNUP/24))*pi/2) + (Tstart_wall)

      Ktc = 1.0
      CALL diffusefraction(I0, altitude, Ktc, Ta, RH, radI0, notU)
      corr = 0.1473*LOG(90.-(zen*rad2deg)) + 0.3454 ! 20070329 temporary correction of latitude from Lindberg et al. 2008
      CI_Tg = (radI/radI0) + (1.-corr)

      !TODO: FIX THIS in fortran syntax?? if CI_Tg<inf then CI_Tg=1
      IF (CI_Tg > 1) CI_Tg = 1
      Tg = Tg*CI_Tg
      Tgwall = Tgwall*CI_Tg

      IF (Tgwall < 0) Tgwall = 0
      IF (Tg < 0) Tg = 0

   END SUBROUTINE tSurfBEERS

   SUBROUTINE shadowGroundKusaka(HW, azimuth, zen, shadowground, shadowwalls)
      ! Translated from Python! Should be checked by fortran person! FL
      ! This function calculates sunlit fractions on ground and walls for an infinite long, rotating canyon (Kusaka)
      ! (hw, azimuth, zen)
      !DEG2RAD = np.pi / 180.

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: HW, azimuth, zen
      REAL(KIND(1D0)), INTENT(out) :: shadowground, shadowwalls
      ! REAL(KIND(1d0)), PARAMETER   :: pi = 3.141592653589793
      REAL(KIND(1D0)) :: ROOF_HEIGHT, ROAD_WIDTH, THETA_Z, THETA_S, THETA_S180, ndir, wx
      REAL(KIND(1D0)) :: LshadowRoad(1:8), Wallsun(1:8)
      INTEGER :: idir

      ROOF_HEIGHT = hw
      ROAD_WIDTH = 1.
      THETA_Z = zen ! Solar Zenith Angle (radians)
      THETA_S = azimuth*DEG2RAD ! Solar azimuth Angle (radians)
      THETA_S180 = ABS(pi - THETA_S) ! deviation from south acc. Kusaka 2001
      ndir = 8.
      !LshadowRoad = np.zeros(int(ndir))
      !Wallsun = np.zeros(int(ndir))

      DO idir = 1, 8 ! Looping through 8 canyon directions. In Python 1 to 9. Here 1 to 8 Correct???
         wx = 1./SIN(ABS((float(idir)*pi/ndir) - THETA_S180)) ! sun azimuth length in canyon
         LshadowRoad(idir) = (ROOF_HEIGHT/ROAD_WIDTH)*TAN(THETA_Z)*SIN(ABS((float(idir)*pi/ndir) - THETA_S180))
         Wallsun(idir) = (wx/TAN(THETA_Z))/(ROOF_HEIGHT/ROAD_WIDTH)
      END DO

      ! shadow fraction ground
      WHERE (LshadowRoad >= 1.) LshadowRoad = 1.
      LshadowRoad = -1*LshadowRoad + 1. ! Getting sunlit instead of shadow
      shadowground = SUM(LshadowRoad)/ndir

      ! shadow fraction walls
      WHERE (Wallsun >= 1.) Wallsun = 1.
      shadowwalls = (SUM(Wallsun)/ndir)/2. ! two walls which one is always in shadow

   END SUBROUTINE shadowGroundKusaka

   SUBROUTINE clearnessindex_2013b(zen, DOY, Ta_degC, RH_frac, radG, lat, P_kPa, &
                                   I0, CI, Kt, I0et, CIuncorr)
      ! Clearness Index at the Earth's surface calculated from Crawford and Duchon 1999
      IMPLICIT NONE

      INTEGER, INTENT(in) :: DOY
      REAL(KIND(1D0)), INTENT(in) :: zen
      REAL(KIND(1D0)), INTENT(in) :: Ta_degC
      REAL(KIND(1D0)), INTENT(in) :: RH_frac
      REAL(KIND(1D0)), INTENT(in) :: P_kPa
      REAL(KIND(1D0)), INTENT(in) :: radG
      REAL(KIND(1D0)), INTENT(in) :: lat
      REAL(KIND(1D0)), INTENT(out) :: I0
      REAL(KIND(1D0)), INTENT(out) :: CI
      REAL(KIND(1D0)), INTENT(out) :: Kt
      REAL(KIND(1D0)), INTENT(out) :: I0et
      REAL(KIND(1D0)), INTENT(out) :: CIuncorr
      REAL(KIND(1D0)) :: iG, Itoa, p
      REAL(KIND(1D0)), DIMENSION(4) :: G
      ! REAL(KIND(1d0)), PARAMETER    :: pi = 3.141592653589793

      ! Variable declarations
      REAL*8 :: a2 !>
      !logical :: b      !>
      REAL*8 :: b2 !>
      REAL*8 :: corr !>
      REAL*8 :: D !>
      REAL*8 :: m !>
      REAL*8 :: Tar !>
      REAL*8 :: Td !>
      REAL*8 :: Trpg !>
      REAL*8 :: Tw !>
      REAL*8 :: u !>

      IF (P_kPa == -999) THEN
         p = 1013 !Pressure in millibars
      ELSE
         p = P_kPa*10 !Convert from hPa to millibars
      END IF
      !Effective solar constant
      Itoa = 1370 !TODO: Check if this is defined somewhere else?
      CALL sun_distance(DOY, D)
      m = 35.*COS(zen)*((1224.*(COS(zen)**2) + 1.)**(-1./2.)) !optical air mass at p=1013
      Trpg = 1.021 - 0.084*(m*(0.000949*p + 0.051))**0.5 !Transmission coefficient for Rayliegh scattering and permanent gases

      ! empirical constant depending on latitude
      G = 0
      IF (lat < 10) THEN
         G = [3.37, 2.85, 2.80, 2.64]
      ELSE IF (lat >= 10 .AND. lat < 20) THEN
         G = [2.99, 3.02, 2.70, 2.93]
      ELSE IF (lat >= 20 .AND. lat < 30) THEN
         G = [3.60, 3.00, 2.98, 2.93]
      ELSE IF (lat >= 30 .AND. lat < 40) THEN
         G = [3.04, 3.11, 2.92, 2.94]
      ELSE IF (lat >= 40 .AND. lat < 50) THEN
         G = [2.70, 2.95, 2.77, 2.71]
      ELSE IF (lat >= 50 .AND. lat < 60) THEN
         G = [2.52, 3.07, 2.67, 2.93]
      ELSE IF (lat >= 60 .AND. lat < 70) THEN
         G = [1.76, 2.69, 2.61, 2.61]
      ELSE IF (lat >= 70 .AND. lat < 80) THEN
         G = [1.60, 1.67, 2.24, 2.63]
      ELSE IF (lat >= 80 .AND. lat < 90) THEN
         G = [1.11, 1.44, 1.94, 2.02]
      END IF
      IF (DOY > 335 .OR. DOY <= 60) THEN
         iG = G(1)
      ELSE IF (DOY > 60 .AND. DOY <= 152) THEN
         iG = G(2)
      ELSE IF (DOY > 152 .AND. DOY <= 244) THEN
         iG = G(3)
      ELSE IF (DOY > 244 .AND. DOY <= 335) THEN
         iG = G(4)
      END IF
      !dewpoint calculation
      a2 = 17.27
      b2 = 237.7
      Td = (b2*(((a2*Ta_degC)/(b2 + Ta_degC)) + LOG(RH_frac)))/(a2 - (((a2*Ta_degC)/(b2 + Ta_degC)) + LOG(RH_frac)))
      Td = (Td*1.8) + 32 !Dewpoint (degF)
      u = EXP(0.1133 - LOG(iG + 1) + 0.0393*Td) !Precipitable water
      Tw = 1 - 0.077*((u*m)**0.3) !Transmission coefficient for water vapor
      Tar = 0.935**m !Transmission coefficient for aerosols

      I0 = Itoa*COS(zen)*Trpg*Tw*D*Tar

!!! This needs to be checked in fortran environment!!!
      !b=I0==abs(zen)>pi/2
      !I0(b==1)=0
      !clear b
      !if (not(isreal(I0))) then
      !    I0=0
      !end if

      corr = 0.1473*LOG(90 - (zen/pi*180)) + 0.3454 ! 20070329

      CIuncorr = radG/I0
      CI = CIuncorr + (1 - corr)
      I0et = Itoa*COS(zen)*D !extra terrestial solar radiation
      Kt = radG/I0et

      IF (CI > 1) CI = 1

   END SUBROUTINE clearnessindex_2013b

   !===============================================================================

   SUBROUTINE sun_distance(jday, D)

      ! Calculates solar irradiance variation based on mean earth sun distance
      ! with day of year as input.
      ! Partridge and Platt, 1975

      INTEGER :: jday
      REAL(KIND(1D0)) :: b, D

      b = 2*3.141592654*jday/365
      D = SQRT(1.00011 + 0.034221*COS(b) + 0.001280*SIN(b) + 0.000719*COS(2*b) + 0.000077*SIN(2*b))

   END SUBROUTINE sun_distance

   SUBROUTINE cylindric_wedge(zen, svfalfa, F_sh)
      ! Fraction of sunlit walls based on sun altitude and svf wieghted building angles

      IMPLICIT NONE

      ! REAL(KIND(1d0)), PARAMETER                    :: pi = 3.141592653589793
      REAL(KIND(1D0)), INTENT(in) :: zen
      REAL(KIND(1D0)), INTENT(in) :: svfalfa !>
      REAL(KIND(1D0)), INTENT(out) :: F_sh
      REAL(KIND(1D0)) :: beta !>
      REAL(KIND(1D0)) :: alfa, xa, ha, hkil, ba
      REAL(KIND(1D0)) :: Ai, phi, qa, Za
      REAL(KIND(1D0)) :: ukil, Ssurf

      ! ALLOCATE (alfa(1, 1))
      ! ALLOCATE (ba(1, 1))
      ! ALLOCATE (ha(1, 1))
      ! ALLOCATE (xa(1, 1))
      ! ALLOCATE (qa(1, 1))
      ! ALLOCATE (Za(1, 1))
      ! ALLOCATE (phi(1, 1))
      ! ALLOCATE (ukil(1, 1))
      ! ALLOCATE (Ai(1, 1))
      ! ALLOCATE (Ssurf(1, 1))
      ! ALLOCATE (hkil(1, 1))

      beta = zen
      alfa = svfalfa

      xa = 1.-2./(TAN(alfa)*TAN(beta))
      ha = 2./(TAN(alfa)*TAN(beta))
      ba = (1./TAN(alfa))
      hkil = 2.*ba*ha
      qa = 0.0D0

      IF (xa < 0) qa = TAN(beta)/2

      Za = 0.0D0
      phi = 0.0D0
      Ai = 0.0D0
      ukil = 0.0D0
      IF (xa < 0) THEN
         Za = (ba**2 - qa**2/4.)**0.5
         phi = ATAN(Za/qa)
         Ai = (SIN(phi) - phi*COS(phi))/(1 - COS(phi))
         ukil = 2*ba*xa*Ai
      END IF

      Ssurf = hkil + ukil

      F_sh = (2*pi*ba - Ssurf)/(2*pi*ba) !Xa

      IF (F_sh < 0) F_sh = 0.0
      IF (F_sh > 0.5) F_sh = 0.5

      ! DEALLOCATE (alfa)
      ! DEALLOCATE (ba)
      ! DEALLOCATE (ha)
      ! DEALLOCATE (xa)
      ! DEALLOCATE (qa)
      ! DEALLOCATE (Za)
      ! DEALLOCATE (phi)
      ! DEALLOCATE (ukil)
      ! DEALLOCATE (Ai)
      ! DEALLOCATE (Ssurf)
      ! DEALLOCATE (hkil)

   END SUBROUTINE cylindric_wedge

   ! This subroutine estimates diffuse and directbeam radiation according to
   ! Reindl et al (1990), Solar Energy 45:1
   SUBROUTINE diffusefraction(radG, altitude, Kt, Ta, RH, radI, radD)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: radG
      REAL(KIND(1D0)), INTENT(in) :: altitude
      REAL(KIND(1D0)), INTENT(in) :: Kt
      REAL(KIND(1D0)), INTENT(in) :: Ta
      REAL(KIND(1D0)), INTENT(in) :: RH
      REAL(KIND(1D0)), INTENT(out) :: radD ! direct radiation
      REAL(KIND(1D0)), INTENT(out) :: radI ! diffusive radiation
      REAL(KIND(1D0)) :: alfa

      alfa = altitude*DEG2RAD

      IF (Ta <= -99 .OR. RH <= -99) THEN !.or. isnan(Ta) .or. isnan(RH)) then
         IF (Kt <= 0.3) THEN
            radD = radG*(1.020 - 0.248*Kt)
         ELSE IF (Kt > 0.3 .AND. Kt < 0.78) THEN
            radD = radG*(1.45 - 1.67*Kt)
         ELSE IF (Kt >= 0.78) THEN
            radD = radG*0.147
         END IF
      ELSE
         IF (Kt <= 0.3) THEN
            radD = radG*(1 - 0.232*Kt + 0.0239*SIN(alfa) - 0.000682*Ta + 0.0195*(RH/100))
         ELSE IF (Kt > 0.3 .AND. Kt < 0.78) THEN
            radD = radG*(1.329 - 1.716*Kt + 0.267*SIN(alfa) - 0.00357*Ta + 0.106*(RH/100))
         ELSE IF (Kt >= 0.78) THEN
            radD = radG*(0.426*Kt - 0.256*SIN(alfa) + 0.00349*Ta + 0.0734*(RH/100))
         END IF
      END IF
      ! correction of radD
      radD = MAX(0.D0, radD)
      radD = MIN(radG, radD)

      ! calculation of direct beam radiation
      radI = (radG - radD)/(SIN(alfa))

      !! Corrections for low sun altitudes (20130307)
      IF (altitude < 1 .AND. radI > radG) THEN
         radI = radG
      END IF

   END SUBROUTINE diffusefraction

   SUBROUTINE KWalls( &
      svf, svfveg, shadow, F_sh, &
      radI, radG, radD, azimuth, altitude, psi, t, alb_ground, alb_bldg, & ! input
      Keast, Knorth, Ksouth, Kwest) ! output

      IMPLICIT NONE

      ! REAL(KIND(1d0)), PARAMETER :: pi = 3.141592653589793
      REAL(KIND(1D0)), INTENT(in) :: radI
      REAL(KIND(1D0)), INTENT(in) :: radG
      REAL(KIND(1D0)), INTENT(in) :: radD
      REAL(KIND(1D0)), INTENT(in) :: azimuth
      REAL(KIND(1D0)), INTENT(in) :: altitude
      REAL(KIND(1D0)), INTENT(in) :: psi
      REAL(KIND(1D0)), INTENT(in) :: t
      REAL(KIND(1D0)), INTENT(in) :: alb_bldg
      REAL(KIND(1D0)), INTENT(in) :: alb_ground

      ! REAL(KIND(1d0)),  intent(in)   :: shadow, F_sh, svfalfa, svf, svfveg, svfaveg
      REAL(KIND(1D0)), INTENT(in) :: shadow, F_sh, svf, svfveg
      REAL(KIND(1D0)), INTENT(out) :: Keast, Knorth, Ksouth, Kwest

      REAL(KIND(1D0)) :: vikttot, aziE, aziN, aziS, aziW
      REAL(KIND(1D0)) :: viktveg, viktwall
      REAL(KIND(1D0)) :: KeastI, KsouthI, KwestI, KnorthI, Kuptowall
      REAL(KIND(1D0)) :: KeastDG, KsouthDG, KwestDG, KnorthDG
      REAL(KIND(1D0)) :: svfE, svfS, svfW, svfN, svfEveg, svfSveg, svfWveg, svfNveg, svfbuveg
      REAL(KIND(1D0)) :: gvfalb, gvfalbnosh

      ! Internal grids
      REAL(KIND(1D0)) :: svfviktbuveg

      ! ALLOCATE (svfviktbuveg(1, 1))
      vikttot = 4.4897
      aziE = azimuth + t
      aziS = azimuth - 90 + t
      aziW = azimuth - 180 + t
      aziN = azimuth - 270 + t

      svfE = svf
      svfS = svf
      svfW = svf
      svfN = svf
      svfEveg = svfveg
      svfSveg = svfveg
      svfWveg = svfveg
      svfNveg = svfveg

      !!! Direct irradiance !!!
      IF (azimuth > (360 - t) .OR. azimuth <= (180 - t)) THEN
         KeastI = radI*shadow*COS(altitude*deg2rad)*SIN(aziE*deg2rad)
      ELSE
         KeastI = 0
      END IF
      IF (azimuth > (90 - t) .AND. azimuth <= (270 - t)) THEN
         KsouthI = radI*shadow*COS(altitude*deg2rad)*SIN(aziS*deg2rad)
      ELSE
         KsouthI = 0
      END IF
      IF (azimuth > (180 - t) .AND. azimuth <= (360 - t)) THEN
         KwestI = radI*shadow*COS(altitude*deg2rad)*SIN(aziW*deg2rad)
      ELSE
         KwestI = 0
      END IF
      IF (azimuth <= (90 - t) .OR. azimuth > (270 - t)) THEN
         KnorthI = radI*shadow*COS(altitude*deg2rad)*SIN(aziN*deg2rad)
      ELSE
         KnorthI = 0
      END IF

      !!! Diffuse and reflected radiation !!!
      svfbuveg = svfS - (1.0 - svfSveg)*(1.0 - psi)
      gvfalb = shadow*alb_ground ! albedo not defined TODO #3
      gvfalbnosh = (1 - shadow)*alb_ground
      Kuptowall = (gvfalb*radI*SIN(altitude*deg2rad)) &
                  + (radD*svfbuveg + alb_bldg*(1 - svfbuveg)*(radG*(1 - F_sh) + radD*F_sh))*gvfalbnosh

      CALL Kvikt_veg(svfE, svfEveg, vikttot, viktveg, viktwall)
      svfviktbuveg = (viktwall + (viktveg)*(1 - psi))
      KeastDG = (radD*(1 - svfviktbuveg) + alb_bldg*(svfviktbuveg*(radG*(1 - F_sh) + radD*F_sh)) + Kuptowall)*0.5
      Keast = KeastI + KeastDG

      CALL Kvikt_veg(svfS, svfSveg, vikttot, viktveg, viktwall)
      svfviktbuveg = (viktwall + (viktveg)*(1 - psi))
      KsouthDG = (radD*(1 - svfviktbuveg) + alb_bldg*(svfviktbuveg*(radG*(1 - F_sh) + radD*F_sh)) + Kuptowall)*0.5
      Ksouth = KsouthI + KsouthDG

      CALL Kvikt_veg(svfW, svfWveg, vikttot, viktveg, viktwall)
      svfviktbuveg = (viktwall + (viktveg)*(1 - psi))
      KwestDG = (radD*(1 - svfviktbuveg) + alb_bldg*(svfviktbuveg*(radG*(1 - F_sh) + radD*F_sh)) + Kuptowall)*0.5
      Kwest = KwestI + KwestDG

      CALL Kvikt_veg(svfN, svfNveg, vikttot, viktveg, viktwall)
      svfviktbuveg = (viktwall + (viktveg)*(1 - psi))
      KnorthDG = (radD*(1 - svfviktbuveg) + alb_bldg*(svfviktbuveg*(radG*(1 - F_sh) + radD*F_sh)) + Kuptowall)*0.5
      Knorth = KnorthI + KnorthDG

      ! DEALLOCATE (svfviktbuveg)
   END SUBROUTINE KWalls

   SUBROUTINE Kvikt_veg(svf, svfveg, vikttot, & !input
                        viktveg, viktwall) ! output

      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(in) :: vikttot
      REAL(KIND(1D0)), INTENT(in) :: svf
      REAL(KIND(1D0)), INTENT(in) :: svfveg
      REAL(KIND(1D0)), INTENT(out) :: viktveg, viktwall
      REAL(KIND(1D0)) :: svfvegbu

      !! Least
      ! viktwall = (vikttot &
      !             - (63.227*svf**6 - 161.51*svf**5 &
      !                + 156.91*svf**4 - 70.424*svf**3 &
      !                + 16.773*svf**2 - 0.4863*svf))/vikttot

      viktwall = cal_vikt(svf, vikttot)

      svfvegbu = (svfveg + svf - 1) ! Vegetation plus buildings
      ! viktveg = (vikttot &
      !            - (63.227*svfvegbu**6 - 161.51*svfvegbu**5 &
      !               + 156.91*svfvegbu**4 - 70.424*svfvegbu**3 &
      !               + 16.773*svfvegbu**2 - 0.4863*svfvegbu))/vikttot
      viktveg = cal_vikt(svfvegbu, vikttot)
      viktveg = viktveg - viktwall
   END SUBROUTINE Kvikt_veg

   FUNCTION cal_vikt(svf_x, vikttot) RESULT(vikt)
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(in) :: svf_x, vikttot
      REAL(KIND(1D0)) :: vikt

      vikt = (vikttot &
              - (63.227*svf_x**6 - 161.51*svf_x**5 &
                 + 156.91*svf_x**4 - 70.424*svf_x**3 &
                 + 16.773*svf_x**2 - 0.4863*svf_x))/vikttot

   END FUNCTION cal_vikt

   SUBROUTINE LWalls(svf, svfveg, svfaveg, &
                     Ldown2d, Lup2d, &
                     altitude, Ta, Tw, SBC, emis_wall, emis_sky, t, CI, azimuth, ldown, svfalfa, F_sh_in, & !input
                     Least, Lnorth, Lsouth, Lwest)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: altitude, Ta, Tw, SBC, emis_wall, emis_sky, t, CI, azimuth, ldown

      REAL(KIND(1D0)), INTENT(in) :: svfalfa, svf, svfveg, svfaveg, F_sh_in
      REAL(KIND(1D0)), INTENT(in) :: Ldown2d, Lup2d
      REAL(KIND(1D0)), INTENT(out) :: Least, Lnorth, Lsouth, Lwest

      REAL(KIND(1D0)) :: vikttot, aziE, aziN, aziS, aziW, c

      REAL(KIND(1D0)) :: svfalfaE, svfalfaS, svfalfaW, svfalfaN
      REAL(KIND(1D0)) :: alfaB, betaB, betasun
      REAL(KIND(1D0)) :: Lground, Lrefl, Lsky, Lsky_allsky, Lveg, Lwallsh, Lwallsun
      ! REAL(KIND(1d0)) :: viktonlywall, viktaveg, svfvegbu
      ! REAL(KIND(1d0)) :: oneminussvfE, oneminussvfS, oneminussvfW, oneminussvfN
      ! REAL(KIND(1d0)), PARAMETER                   :: pi = 3.141592653589793
      ! INTEGER, PARAMETER:: SOLWEIG_ldown = 0  !! force to 0, TS 13 Dec 2019

      ! set as 1 for testing
      INTEGER, PARAMETER :: SOLWEIG_ldown = 1 !! force to 0, TS 13 Dec 2019

      REAL(KIND(1D0)) :: viktveg, viktsky, viktrefl, viktwall
      REAL(KIND(1D0)) :: svfE, svfS, svfW, svfN, svfEveg, svfSveg, svfWveg, svfNveg
      REAL(KIND(1D0)) :: svfEaveg, svfSaveg, svfWaveg, svfNaveg, F_sh

      ! ALLOCATE (oneminussvfE(1, 1))
      ! ALLOCATE (oneminussvfS(1, 1))
      ! ALLOCATE (oneminussvfW(1, 1))
      ! ALLOCATE (oneminussvfN(1, 1))
      ! ALLOCATE (svfalfaE(1, 1))
      ! ALLOCATE (svfalfaS(1, 1))
      ! ALLOCATE (svfalfaW(1, 1))
      ! ALLOCATE (svfalfaN(1, 1))
      ! ALLOCATE (alfaB(1, 1))
      ! ALLOCATE (betaB(1, 1))
      ! ALLOCATE (betasun(1, 1))
      ! ALLOCATE (Lground(1, 1))
      ! ALLOCATE (Lrefl(1, 1))
      ! ALLOCATE (Lsky(1, 1))
      ! ALLOCATE (Lsky_allsky(1, 1))
      ! ALLOCATE (Lveg(1, 1))
      ! ALLOCATE (Lwallsh(1, 1))
      ! ALLOCATE (Lwallsun(1, 1))
      ! ALLOCATE (viktonlywall(1, 1))
      ! ALLOCATE (viktaveg(1, 1))
      ! ALLOCATE (svfvegbu(1, 1))

      svfE = svf
      svfS = svf
      svfW = svf
      svfN = svf
      svfEveg = svfveg
      svfSveg = svfveg
      svfWveg = svfveg
      svfNveg = svfveg
      svfEaveg = svfaveg
      svfSaveg = svfaveg
      svfWaveg = svfaveg
      svfNaveg = svfaveg

      !Building height angle from svf
      ! oneminussvfE = 1.-svfE; WHERE (oneminussvfE <= 0) oneminussvfE = 0.000000001 ! avoiding log(0)
      ! oneminussvfS = 1.-svfS; WHERE (oneminussvfS <= 0) oneminussvfS = 0.000000001 ! avoiding log(0)
      ! oneminussvfW = 1.-svfW; WHERE (oneminussvfW <= 0) oneminussvfW = 0.000000001 ! avoiding log(0)
      ! oneminussvfN = 1.-svfN; WHERE (oneminussvfN <= 0) oneminussvfN = 0.000000001 ! avoiding log(0)
      ! svfalfaE = ASIN(EXP((LOG(oneminussvfE))/2))
      ! svfalfaS = ASIN(EXP((LOG(oneminussvfS))/2))
      ! svfalfaW = ASIN(EXP((LOG(oneminussvfW))/2))
      ! svfalfaN = ASIN(EXP((LOG(oneminussvfN))/2))
      svfalfaE = svfalfa
      svfalfaS = svfalfa
      svfalfaW = svfalfa
      svfalfaN = svfalfa

      vikttot = 4.4897
      aziW = azimuth + t
      aziN = azimuth - 90 + t
      aziE = azimuth - 180 + t
      aziS = azimuth - 270 + t

      F_sh = 2.*F_sh_in - 1. !(cylindric_wedge scaled 0-1 as only half hemisphere is seen)

      IF (SOLWEIG_ldown == 1) THEN
         c = 1 - CI
         Lsky_allsky = emis_sky*SBC*((Ta + 273.15)**4)*(1 - c) + c*SBC*((Ta + 273.15)**4)
      ELSE
         Lsky_allsky = ldown
      END IF

      !! Least
      CALL Lvikt_veg(svfE, svfEveg, svfEaveg, vikttot, &
                     viktveg, viktsky, viktrefl, viktwall)

      IF (altitude > 0) THEN ! daytime
         alfaB = ATAN(svfalfaE)
         betaB = ATAN(TAN((svfalfaE)*F_sh))
         betasun = ((alfaB - betaB)/2) + betaB
         IF (azimuth > (180 - t) .AND. azimuth <= (360 - t)) THEN
            Lwallsun = SBC*emis_wall*((Ta + 273.15 + Tw*SIN(aziE*(pi/180)))**4)* &
                       viktwall*(1 - F_sh)*COS(betasun)*0.5
            Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*F_sh*0.5
         ELSE
            Lwallsun = 0
            Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*0.5
         END IF
      ELSE !nighttime
         Lwallsun = 0
         Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*0.5
      END IF
      Lsky = ((svfE + svfEveg - 1)*Lsky_allsky)*viktsky*0.5
      Lveg = SBC*emis_wall*((Ta + 273.15)**4)*viktveg*0.5
      Lground = Lup2d*0.5
      Lrefl = (Ldown2d + Lup2d)*(viktrefl)*(1 - emis_wall)*0.5
      Least = Lsky + Lwallsun + Lwallsh + Lveg + Lground + Lrefl

      !! Lsouth
      CALL Lvikt_veg(svfS, svfSveg, svfSaveg, vikttot, &
                     viktveg, viktsky, viktrefl, viktwall)

      IF (altitude > 0) THEN ! daytime
         alfaB = ATAN(svfalfaS)
         betaB = ATAN(TAN((svfalfaS)*F_sh))
         betasun = ((alfaB - betaB)/2) + betaB
         IF (azimuth <= (90 - t) .OR. azimuth > (270 - t)) THEN
            Lwallsun = SBC*emis_wall*((Ta + 273.15 + Tw*SIN(aziS*(pi/180)))**4)* &
                       viktwall*(1 - F_sh)*COS(betasun)*0.5
            Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*F_sh*0.5
         ELSE
            Lwallsun = 0
            Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*0.5
         END IF
      ELSE !nighttime
         Lwallsun = 0
         Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*0.5
      END IF
      Lsky = ((svfS + svfSveg - 1)*Lsky_allsky)*viktsky*0.5
      Lveg = SBC*emis_wall*((Ta + 273.15)**4)*viktveg*0.5
      Lground = Lup2d*0.5
      Lrefl = (Ldown2d + Lup2d)*(viktrefl)*(1 - emis_wall)*0.5
      Lsouth = Lsky + Lwallsun + Lwallsh + Lveg + Lground + Lrefl

      !! Lwest
      CALL Lvikt_veg(svfW, svfWveg, svfWaveg, vikttot, &
                     viktveg, viktsky, viktrefl, viktwall)

      IF (altitude > 0) THEN ! daytime
         alfaB = ATAN(svfalfaW)
         betaB = ATAN(TAN((svfalfaW)*F_sh))
         betasun = ((alfaB - betaB)/2) + betaB
         IF (azimuth > (360 - t) .OR. azimuth <= (180 - t)) THEN
            Lwallsun = SBC*emis_wall*((Ta + 273.15 + Tw*SIN(aziW*(pi/180)))**4)* &
                       viktwall*(1 - F_sh)*COS(betasun)*0.5
            Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*F_sh*0.5
         ELSE
            Lwallsun = 0
            Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*0.5
         END IF
      ELSE !nighttime
         Lwallsun = 0
         Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*0.5
      END IF
      Lsky = ((svfW + svfWveg - 1)*Lsky_allsky)*viktsky*0.5
      Lveg = SBC*emis_wall*((Ta + 273.15)**4)*viktveg*0.5
      Lground = Lup2d*0.5
      Lrefl = (Ldown2d + Lup2d)*(viktrefl)*(1 - emis_wall)*0.5
      Lwest = Lsky + Lwallsun + Lwallsh + Lveg + Lground + Lrefl

      !! Lnorth
      CALL Lvikt_veg(svfN, svfNveg, svfNaveg, vikttot, &
                     viktveg, viktsky, viktrefl, viktwall)

      IF (altitude > 0) THEN ! daytime
         alfaB = ATAN(svfalfaN)
         betaB = ATAN(TAN((svfalfaN)*F_sh))
         betasun = ((alfaB - betaB)/2) + betaB
         IF (azimuth > (90 - t) .AND. azimuth <= (270 - t)) THEN
            Lwallsun = SBC*emis_wall*((Ta + 273.15 + Tw*SIN(aziN*(pi/180)))**4)* &
                       viktwall*(1 - F_sh)*COS(betasun)*0.5
            Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*F_sh*0.5
         ELSE
            Lwallsun = 0
            Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*0.5
         END IF
      ELSE !nighttime
         Lwallsun = 0
         Lwallsh = SBC*emis_wall*((Ta + 273.15)**4)*viktwall*0.5
      END IF
      Lsky = ((svfN + svfNveg - 1)*Lsky_allsky)*viktsky*0.5
      Lveg = SBC*emis_wall*((Ta + 273.15)**4)*viktveg*0.5
      Lground = Lup2d*0.5
      Lrefl = (Ldown2d + Lup2d)*(viktrefl)*(1 - emis_wall)*0.5
      Lnorth = Lsky + Lwallsun + Lwallsh + Lveg + Lground + Lrefl

      ! DEALLOCATE (svfE)
      ! DEALLOCATE (svfS)
      ! DEALLOCATE (svfW)
      ! DEALLOCATE (svfN)
      ! DEALLOCATE (svfEveg)
      ! DEALLOCATE (svfSveg)
      ! DEALLOCATE (svfWveg)
      ! DEALLOCATE (svfNveg)
      ! DEALLOCATE (svfEaveg)
      ! DEALLOCATE (svfSaveg)
      ! DEALLOCATE (svfWaveg)
      ! DEALLOCATE (svfNaveg)
      ! DEALLOCATE (svfalfaE)
      ! DEALLOCATE (svfalfaS)
      ! DEALLOCATE (svfalfaW)
      ! DEALLOCATE (svfalfaN)
      ! DEALLOCATE (alfaB)
      ! DEALLOCATE (betaB)
      ! DEALLOCATE (betasun)
      ! DEALLOCATE (Lground)
      ! DEALLOCATE (Lrefl)
      ! DEALLOCATE (Lsky)
      ! DEALLOCATE (Lsky_allsky)
      ! DEALLOCATE (Lveg)
      ! DEALLOCATE (Lwallsh)
      ! DEALLOCATE (Lwallsun)
      ! DEALLOCATE (viktonlywall)
      ! DEALLOCATE (viktaveg)

   END SUBROUTINE LWalls

   SUBROUTINE Lvikt_veg(isvf, isvfveg, isvfaveg, vikttot, & ! input
                        viktveg, viktsky, viktrefl, viktwall) !output

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: vikttot
      REAL(KIND(1D0)), INTENT(in) :: isvf
      REAL(KIND(1D0)), INTENT(in) :: isvfveg
      REAL(KIND(1D0)), INTENT(in) :: isvfaveg
      REAL(KIND(1D0)), INTENT(out) :: viktveg
      REAL(KIND(1D0)), INTENT(out) :: viktsky
      REAL(KIND(1D0)), INTENT(out) :: viktrefl
      REAL(KIND(1D0)), INTENT(out) :: viktwall

      REAL(KIND(1D0)) :: viktonlywall
      REAL(KIND(1D0)) :: viktaveg
      REAL(KIND(1D0)) :: svfvegbu

      viktonlywall = (vikttot - &
                      (63.227*isvf**6 - 161.51*isvf**5 + 156.91*isvf**4 &
                       - 70.424*isvf**3 + 16.773*isvf**2 - 0.4863*isvf))/vikttot

      viktaveg = (vikttot &
                  - (63.227*isvfaveg**6 - 161.51*isvfaveg**5 &
                     + 156.91*isvfaveg**4 - 70.424*isvfaveg**3 &
                     + 16.773*isvfaveg**2 - 0.4863*isvfaveg))/vikttot

      viktwall = viktonlywall - viktaveg

      svfvegbu = (isvfveg + isvf - 1) ! Vegetation plus buildings
      viktsky = (63.227*svfvegbu**6 - 161.51*svfvegbu**5 &
                 + 156.91*svfvegbu**4 - 70.424*svfvegbu**3 &
                 + 16.773*svfvegbu**2 - 0.4863*svfvegbu)/vikttot
      viktrefl = (vikttot &
                  - (63.227*svfvegbu**6 - 161.51*svfvegbu**5 &
                     + 156.91*svfvegbu**4 - 70.424*svfvegbu**3 &
                     + 16.773*svfvegbu**2 - 0.4863*svfvegbu))/vikttot
      viktveg = (vikttot &
                 - (63.227*svfvegbu**6 - 161.51*svfvegbu**5 &
                    + 156.91*svfvegbu**4 - 70.424*svfvegbu**3 &
                    + 16.773*svfvegbu**2 - 0.4863*svfvegbu))/vikttot
      viktveg = viktveg - viktwall

   END SUBROUTINE Lvikt_veg

   !  FUNCTION TO RETURN 0 IF IX=0, 1 IF 0<IX<MAXPOS+1,-1 OTHERWISE.
   !  MAXPOS is given as the maximum positive Integer.
   ! SUBROUTINE issign(IX, MAXPOS, ISIGNM)
   !    REAL(KIND(1D0)) IX, MAXPOS, ISIGNM
   !    ISIGNM = 1.0
   !    IF (IX < 0 .OR. IX > MAXPOS) ISIGNM = -1
   !    IF (IX == 0) ISIGNM = 0
   !    RETURN
   ! END SUBROUTINE issign

! subroutines included:
! day2month
!
! month2day
! leapYearCalc
!       returns -- number of days in actual year
!            used    -- LUMPS phenology (initalization)
!
! DayofWeek
!       returns -- day of week
!       used    -- for water use and anthropogenic heat
!
! dectime_to_timevec
!       This subroutine converts dectime to individual
!       hours, minutes and seconds
!
! daylen
!       Computes solar day length
!
!sg feb 2012 - moved all time related subroutines together
!===============================================================================

!    SUBROUTINE day2month(b, mb, md, seas, year, latitude)
!       IMPLICIT NONE
!       INTEGER, INTENT(in) :: b !b=doy   --IN
!       INTEGER, INTENT(out) :: mb !month=mb  --OUT
!       INTEGER, INTENT(out) :: md !date=md --OUT
!       INTEGER, INTENT(out) :: seas
!       INTEGER, INTENT(in) :: year
!       INTEGER :: t1, t2, t3
!       INTEGER :: k ! k- accounts for leap year

!       REAL(KIND(1D0)) :: latitude

!       ! initialisation
!       mb = 1

!       !Corrected and calculation of date added LJ (Jun 2010)

!       t1 = 4
!       t2 = 100
!       t3 = 400

!       IF ((MODULO(year, t1) == 0) .AND. (MODULO(year, t2) /= 0) .OR. (MODULO(year, t3) == 0)) THEN
!          K = 1
!       ELSE
!          K = 0
!       END IF

!       IF (B <= 31) THEN !January
!          MB = 1
!          md = B
!       ELSEIF (B > 31 .AND. B <= 59 + K) THEN
!          MB = 2
!          md = B - 31
!       ELSEIF (B > 59 + K .AND. B <= 90 + K) THEN
!          MB = 3
!          md = B - (59 + K)
!       ELSEIF (B > 90 + K .AND. B <= 120 + K) THEN
!          MB = 4
!          md = B - (90 + K)
!       ELSEIF (B > 120 + K .AND. B <= 151 + K) THEN
!          MB = 5
!          md = B - (120 + K)
!       ELSEIF (B > 151 + K .AND. B <= 181 + K) THEN
!          MB = 6
!          md = B - (151 + K)
!       ELSEIF (B > 181 + K .AND. B <= 212 + K) THEN
!          MB = 7
!          md = B - (181 + K)
!       ELSEIF (B > 212 + K .AND. B <= 243 + K) THEN
!          MB = 8
!          md = B - (212 + K)
!       ELSEIF (B > 243 + K .AND. B <= 273 + K) THEN
!          MB = 9
!          md = B - (243 + K)
!       ELSEIF (B > 273 + K .AND. B <= 304 + K) THEN
!          MB = 10
!          md = B - (273 + K)
!       ELSEIF (B > 304 + K .AND. B <= 334 + K) THEN
!          MB = 11
!          md = B - (304 + K)
!       ELSEIF (B > 334 + K) THEN
!          MB = 12
!          md = B - (334 + K)
!       END IF

!       !
!       IF (latitude > 0) THEN ! Northern Hemisphere
!          IF (mb > 3 .AND. mb < 10) THEN !Summer is from Apr to Sep
!             seas = 1
!          ELSE
!             seas = 2 !Winter rest of the months
!          END IF
!       ELSE ! southern hemisphere
!          IF (mb < 4 .OR. mb > 9) THEN !Summer is from Oct to Mar
!             seas = 1
!          ELSE
!             seas = 2 !Winter rest of the months
!          END IF
!       END IF
!       RETURN
!    END SUBROUTINE day2month
! !===============================================================================
!    SUBROUTINE month2day(mon, ne, k, b)
!       IMPLICIT NONE
!       INTEGER :: mon, ne, k, b

!       IF (mon == 1) THEN
!          NE = 32 - B
!       ELSE IF (mon == 2) THEN
!          NE = 60 + K - B
!       ELSE IF (mon == 3) THEN
!          NE = 91 + K - B
!       ELSE IF (mon == 4) THEN
!          NE = 121 + K - B
!       ELSE IF (mon == 5) THEN
!          NE = 152 + K - B
!       ELSE IF (mon == 6) THEN
!          NE = 182 + K - B
!       ELSE IF (mon == 7) THEN
!          NE = 213 + K - B
!       ELSE IF (mon == 8) THEN
!          NE = 244 + K - B
!          !**********PAGE 151 STARTS HERE**************
!       ELSE IF (mon == 9) THEN
!          NE = 274 + K - B
!       ELSE IF (mon == 10) THEN
!          NE = 305 + K - B
!       ELSE IF (mon == 11) THEN
!          NE = 335 + K - B
!       ELSE IF (mon == 12) THEN
!          NE = 366 + K - B
!       END IF
!    END SUBROUTINE month2day
! !===============================================================================
! !Defines the number or days in each year (defines the leap year)
!    SUBROUTINE LeapYearCalc(year_int, nroDays)

!       IMPLICIT NONE

!       INTEGER :: nroDays, year_int

!       IF (MOD(year_int, 100) /= 0 .AND. MOD(year_int, 4) == 0) THEN
!          nroDays = 366
!       ELSEIF (MOD(year_int, 400) == 0) THEN
!          nroDays = 366
!       ELSE
!          nroDays = 365
!       END IF
!    END SUBROUTINE LeapYearCalc

! !===============================================================================
! !Defines the number or days in each year (defines the leap year)
! ! Ting Sun 09 May 2018
!    ELEMENTAL FUNCTION Days_of_Year(year_int) RESULT(nDays)
!       IMPLICIT NONE
!       INTEGER, INTENT(in) :: year_int
!       INTEGER :: nDays

!       IF (MOD(year_int, 100) /= 0 .AND. MOD(year_int, 4) == 0) THEN
!          nDays = 366
!       ELSEIF (MOD(year_int, 400) == 0) THEN
!          nDays = 366
!       ELSE
!          nDays = 365
!       END IF

!    END FUNCTION Days_of_Year

! !===============================================================================

!    SUBROUTINE Day_Of_Week(DATE, MONTH, YEAR, DOW)
!       ! Calculate weekday from year, month and day information.
!       ! DOW: Sunday=1,...Saturday=7
!       ! YEAR fixed to integer, LJ March 2015

!       IMPLICIT NONE

!       INTEGER DATE, MONTH, DAY, YR, MN, N1, N2, DOW, YEAR

!       YR = YEAR
!       MN = MONTH

!       !C
!       !C       IF JANUARY OR FEBRUARY, ADJUST MONTH AND YEAR
!       !C
!       IF (MN > 2) GO TO 10
!       MN = MN + 12
!       YR = YR - 1
! 10    N1 = (26*(MN + 1))/10
!       N2 = (125*YR)/100
!       DAY = (DATE + N1 + N2 - (YR/100) + (YR/400) - 1)
!       DOW = MOD(DAY, 7) + 1

!       RETURN
!    END SUBROUTINE Day_Of_Week

! !===============================================================================

! !FL
!    SUBROUTINE dectime_to_timevec(dectime, HOURS, MINS, SECS)
!       !This subroutine converts dectime to individual
!       !hours, minutes and seconds
!       INTEGER :: HOURS, MINS, doy
!       REAL(KIND(1D0)) :: dectime, SECS, DH, DM, DS
!       !INTEGER :: year

!       doy = FLOOR(dectime)

!       DH = dectime - doy !Decimal hours
!       HOURS = INT(24*DH)

!       DM = 24*DH - HOURS !Decimal minutes
!       MINS = INT(60*DM)

!       DS = 60*DM - MINS
!       SECS = INT(60*DS)

!    END SUBROUTINE dectime_to_timevec

!==============================================================================

!FL

   ! SUBROUTINE DAYLEN(DOY, XLAT, DAYL, DEC, SNDN, SNUP)
   !    !=======================================================================
   !    !  DAYLEN, Real Function, N.B. Pickering, 09/23/1993
   !    !  Computes solar day length (Spitters, 1986).
   !    !=======================================================================
   !    !-----------------------------------------------------------------------
   !    IMPLICIT NONE
   !    INTEGER :: DOY
   !    REAL(KIND(1D0)), INTENT(IN) :: XLAT
   !    REAL(KIND(1D0)), INTENT(OUT) :: DEC, DAYL, SNDN, SNUP
   !    REAL(KIND(1D0)) :: SOC
   !    REAL(KIND(1D0)), PARAMETER :: RAD = PI/180.0

   !    !-----------------------------------------------------------------------
   !    !     Calculation of declination of sun (Eqn. 16). Amplitude= +/-23.45
   !    !     deg. Minimum = DOY 355 (DEC 21), maximum = DOY 172.5 (JUN 21/22).
   !    DEC = -23.45*COS(2.0*PI*(DOY + 10.0)/365.0)

   !    !     Sun angles.  SOC limited for latitudes above polar circles.
   !    SOC = TAN(RAD*DEC)*TAN(RAD*XLAT)
   !    SOC = MIN(MAX(SOC, -1.0), 1.0)

   !    !     Calculate daylength, sunrise and sunset (Eqn. 17)
   !    DAYL = 12.0 + 24.0*ASIN(SOC)/PI
   !    SNUP = 12.0 - DAYL/2.0
   !    SNDN = 12.0 + DAYL/2.0

   ! END SUBROUTINE DAYLEN

!=======================================================================
! DAYLEN Variables
!-----------------------------------------------------------------------
! DAYL  Day length on day of simulation (from sunrise to sunset)  (hr)
! DEC   Solar declination or (90o - solar elevation at noon) (deg.)
! DOY   Day of year (d)
! PI    PI=3.14159 (rad)
! RAD   RAD=PI/180. (rad./deg.)
! SNDN  Time of sunset (hr)
! SNUP  Time of sunrise (hr)
! SOC   Sine over cosine (intermediate calculation)
! XLAT  Latitude (deg.)
!=======================================================================

! ! Calculate dectime
!    SUBROUTINE SUEWS_cal_dectime( &
!       id, it, imin, isec, & ! input
!       dectime) ! output
!       IMPLICIT NONE
!       INTEGER, INTENT(in) :: id, it, imin, isec

!       REAL(KIND(1D0)), INTENT(out) :: dectime ! nsh in type real

!       dectime = REAL(id - 1, KIND(1D0)) &
!                 + REAL(it, KIND(1D0))/24 &
!                 + REAL(imin, KIND(1D0))/(60*24) &
!                 + REAL(isec, KIND(1D0))/(60*60*24)

!    END SUBROUTINE SUEWS_cal_dectime

! ! Calculate tstep-derived variables
!    SUBROUTINE SUEWS_cal_tstep( &
!       tstep, & ! input
!       nsh, nsh_real, tstep_real) ! output
!       IMPLICIT NONE
!       INTEGER, INTENT(in) :: tstep ! number of timesteps per hour
!       ! values that are derived from tstep
!       INTEGER, INTENT(out) :: nsh ! number of timesteps per hour
!       REAL(KIND(1D0)), INTENT(out) :: nsh_real ! nsh in type real
!       REAL(KIND(1D0)), INTENT(out) :: tstep_real ! tstep in type real
!       nsh = 3600/tstep
!       nsh_real = nsh*1.0
!       tstep_real = tstep*1.0

!    END SUBROUTINE SUEWS_cal_tstep

!    SUBROUTINE SUEWS_cal_weekday( &
!       iy, id, lat, & !input
!       dayofWeek_id) !output
!       IMPLICIT NONE

!       INTEGER, INTENT(in) :: iy ! year
!       INTEGER, INTENT(in) :: id ! day of year
!       REAL(KIND(1D0)), INTENT(in) :: lat

!       INTEGER, DIMENSION(3), INTENT(OUT) :: dayofWeek_id

!       INTEGER :: wd
!       INTEGER :: mb
!       INTEGER :: date
!       INTEGER :: seas

!       CALL day2month(id, mb, date, seas, iy, lat) !Calculate real date from doy
!       CALL Day_of_Week(date, mb, iy, wd) !Calculate weekday (1=Sun, ..., 7=Sat)

!       dayofWeek_id(1) = wd !Day of week
!       dayofWeek_id(2) = mb !Month
!       dayofweek_id(3) = seas !Season

!    END SUBROUTINE SUEWS_cal_weekday

!    SUBROUTINE SUEWS_cal_DLS( &
!       id, startDLS, endDLS, & !input
!       DLS) !output
!       IMPLICIT NONE

!       INTEGER, INTENT(in) :: id, startDLS, endDLS
!       INTEGER, INTENT(out) :: DLS

!       DLS = 0
!       IF (id > startDLS .AND. id < endDLS) dls = 1

!    END SUBROUTINE SUEWS_cal_DLS

!===============set variable of invalid value to NAN=====================
   ELEMENTAL FUNCTION set_nan(x) RESULT(xx)
      IMPLICIT NONE
      REAL(KIND(1D0)), PARAMETER :: pNAN = 30000 ! 30000 to prevent water_state being filtered out as it can be large
      REAL(KIND(1D0)), PARAMETER :: pZERO = 1E-8 ! to prevent inconsistency caused by positive or negative zero
      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      REAL(KIND(1D0)), INTENT(in) :: x
      REAL(KIND(1D0)) :: xx

      IF (ABS(x) > pNAN) THEN
         xx = NAN
      ELSEIF (ABS(x) < pZERO) THEN
         xx = 0
      ELSE
         xx = x
      END IF

   END FUNCTION set_nan
!========================================================================

END MODULE module_phys_beers

! Backward compatibility alias
MODULE beers_module
   USE module_phys_beers
END MODULE beers_module
