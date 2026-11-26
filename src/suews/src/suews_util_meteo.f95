!===================================================================================
! Main module following naming standard: matches filename
MODULE module_util_meteo
   USE module_ctrl_const_allocate, ONLY: RAD2DEG, DEG2RAD
   USE module_ctrl_const_physconst, ONLY: C2K, lv_wetbulb, cp_air
   USE module_ctrl_const_mathconst
   USE module_ctrl_error, ONLY: ErrorHint
   USE module_ctrl_type, ONLY: SUEWS_STATE
   IMPLICIT NONE

CONTAINS

   !============================================================================
   FUNCTION sat_vap_press(TK, P) RESULT(es)
      !c sg sept 99 f90
      !c     This uses eqns from Buck (1981) JAM 20, 1527-1532
      !c     units T (K) e (mb) P (mb)
      !c     f corrects for the fact that we are not dealing with pure water
      REAL(KIND(1D0)) :: TK, P, TC, es, e, f
      TC = TK - C2K
      IF (TC == 0) THEN
         TC = 0.001
      END IF
      !Valid for 50>T>-40
      e = 6.1121*EXP(((18.729 - TC/227.3)*TC)/(TC + 257.87))
      f = 1.00072 + P*(3.2E-6 + 5.9E-10*TC**2)
      es = e*f
   END FUNCTION sat_vap_press

   !=====================================================================
   ! sg sept 99 f90
   ! Uses eqns from Buck (1981) JAM 20, 1527-1532
   ! units T (deg C) e (mb) P (mb)
   ! f corrects for the fact that we are not dealing with pure water
   ! LJ Feb 2010
   !Changed to use the updated version (Buck research manual, 1996) from Buck (1981)
   !For water different equations in cold and warm temperatures

   FUNCTION sat_vap_press_x(Temp_c, PRESS_hPa, from, dectime, modState) RESULT(es_hPa)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: temp_C, press_hpa, dectime !,pw
      REAL(KIND(1D0)) :: e_mb, f, press_kpa, es_hPA
      INTEGER, INTENT(IN) :: from
      INTEGER :: iv
      INTEGER, PARAMETER :: notUsedI = -55
      TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState

      es_hPa = 1000 ! initialise as 1000

      !If air temperature between -0.001 -
      IF (ABS(temp_C) < 0.001000) THEN
         IF (from == 1) THEN ! not from determining Tw
            iv = INT(press_Hpa)
            CALL errorHint(29, 'Function sat_vap_press: temp_C, dectime,press_Hpa = ', temp_C, dectime, iv, modState)

         END IF
         temp_C = 0.001000
      END IF

      Press_kPa = Press_hPa/10

      IF (Temp_C < 50 .AND. Temp_C > -40) THEN
         !e_mb=6.1121*EXP(((18.729-Temp_C/227.3)*Temp_C)/(Temp_C+257.87)) !Old one
         !f=1.00072+Press_hPa*(3.2E-6+5.9D-10*Temp_C**2)

         IF (Temp_C >= 0.001000) THEN
            e_mb = 6.1121*EXP(((18.678 - Temp_C/234.5)*Temp_C)/(Temp_C + 257.14))
            f = 1.00072 + Press_kPa*(3.2E-6 + 5.9E-10*Temp_C**2)
            es_hPa = e_mb*f

         ELSEIF (Temp_C <= -0.001000) THEN
            e_mb = 6.1115*EXP(((23.036 - Temp_C/333.7)*Temp_C)/(Temp_C + 279.82))
            f = 1.00022 + Press_kPa*(3.83E-6 + 6.4E-10*Temp_C**2)
            es_hPa = e_mb*f
         END IF

      ELSE
         CALL ErrorHint(28, 'FUNCTION sat_vap_press: [Temperature is out of range], Temp_C,dectime', Temp_C, dectime, notUsedI, modState)

      END IF

      RETURN
   END FUNCTION sat_vap_press_x

   FUNCTION sat_vap_pressIce(Temp_c, PRESS_hPa, from, dectime, modState) RESULT(es_hPa)
      IMPLICIT NONE

      REAL(KIND(1D0)) :: e_mb, f, temp_C, press_hpa, press_kpa, es_hPA, dectime !,pw
      INTEGER, INTENT(IN) :: from
      INTEGER :: iv
      INTEGER, PARAMETER :: notUsedI = -55
      TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState

      ! initialisation
      es_hPa = 10

      !If air temperature between -0.001 -
      IF (ABS(temp_C) < 0.001000) THEN
         IF (from == 1) THEN ! not from determining Tw
            iv = INT(press_Hpa)
            CALL errorHint(29, 'Function sat_vap_press: temp_C, dectime,press_Hpa = ', temp_C, dectime, iv, modState)

         END IF
         temp_C = 0.001000
      END IF

      Press_kPa = Press_hPa/10

      IF (Temp_C < 50 .AND. Temp_C > -40) THEN
         e_mb = 6.1115*EXP(((23.036 - Temp_C/333.7)*Temp_C)/(Temp_C + 279.82))
         f = 1.00022 + Press_kPa*(3.83E-6 + 6.4E-10*Temp_C**2) !In hPa
         es_hPa = e_mb*f

      ELSE
         CALL ErrorHint(28, 'FUNCTION sat_vap_press: [Temperature is out of range], Temp_C,dectime', Temp_C, dectime, notUsedI, modState)

      END IF

      RETURN
   END FUNCTION sat_vap_pressIce

   !==========================================================
   !Output: specific humidity deficit in g/kg
   !Input: Dry air density and air pressure in hPa
   FUNCTION spec_hum_def(vpd_hPa, press_hPa) RESULT(dq)
      IMPLICIT NONE
      REAL(KIND(1D0)) :: press_hPa, vpd_hPa, dq
      REAL(KIND(1D0)), PARAMETER :: epsil_gkg = 621.97 !ratio molecular weight of water vapor/dry air in g/kg
      dq = epsil_gkg*vpd_hPa/press_hPa ! Phd Thesis II.13 p 196
   END FUNCTION spec_hum_def

   ! ==============================================================================
   FUNCTION spec_heat_beer(Temp_C, rh, rho_v, rho_d, modState) RESULT(cp)
      ! Input: Air temperature, relative humidity, water vapour and dry air densities
      ! Output: heat capacity in units J kg-1 K-1
      ! Reference: Tom Beer, CSIRO, 1990. Applied Environmetrics Meteorological Tables.
      ! Can be found from SG:s office from Atmmos Moist map
      !-------------------------------------------------------------------------------

      IMPLICIT NONE

      REAL(KIND(1D0)) :: cp, cpd, cpm, rho_v, rho_d, rh, temp_C
      TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState

      !Garratt equation a20 (1992)
      CPd = cp_air + ((Temp_C + 23.16)**2)/3364.0 !Changed from 23.15 to 23.16

      !Beer (1990) for water vapor
      cpm = 1859 + 0.13*rH + (19.3 + 0.569*rH)*(Temp_C/100.) + &
            (10.+0.5*rH)*(Temp_C/100.)**2

      IF (ABS(rho_d) < 0.000100 .OR. ABS(rho_v) < 0.000100 .OR. ABS(rho_d + rho_v) < 0.000100) THEN
         CALL ErrorHint(42, 'spec-heat_beer', rho_v, rho_d, INT(Temp_C), modState)
      END IF

      cp = cpd*(rho_d/(rho_d + rho_v)) + cpm*(rho_v/(rho_d + rho_v))

      !   print*,"cp: ",cp,cpd,rho_d,rho_v,cpm,rh
   END FUNCTION spec_heat_beer

   !==========================================================
   !Latent_heat.f sg nov 96
   !sg sep 99 converted f90 FUNCTION
   !Added calcualation of latent heat of sublimation, LJ June 2012

   FUNCTION Lat_vap(Temp_C, Ea_hPa, Press_hPa, cp, dectime, modState) RESULT(lv_J_kg)
      !Input: Air temperature, Water vapour pressure, Air pressure, heat capacity
      !Output: latent heat of vaporization

      IMPLICIT NONE
      REAL(KIND(1D0)) :: cp, lv_J_kg, ea_fix, tw, &
                         incr, es_tw, psyc, ea_est, press_hPa, ea_HPa, temp_C, dectime !,Temp_K
      ! REAL(KIND(1d0))::sat_vap_press,psyc_const ! functions

      ! switch1/switch2 record the search direction within one call. They are
      ! assigned below, not initialised in the declaration: an initialiser
      ! implies SAVE, which carried the flags across calls and shared them
      ! between grids running on parallel threads (GH#1741).
      LOGICAL :: switch1, switch2
      INTEGER :: ii
      INTEGER, PARAMETER :: from = 2
      REAL(KIND(1D0)), PARAMETER :: notUsed = -55.55
      TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState

      switch1 = .FALSE.
      switch2 = .FALSE.
      ea_fix = ea_hPa
      !if(debug) write(*,*)Temp_C, 'LV'
      !Temp_K=temp_C+273.15

      !lv=1.91846E6*(Temp_K/(Temp_K-33.91))**2

      lv_J_kg = (lv_wetbulb - 2.365*temp_C)*1000 !First guess for lv in units J/kg

      tw = Temp_C/2. !First estimate for wet bulb temperature
      incr = 3.
      DO ii = 1, 100
         IF (Press_hPa < 900) THEN
            CALL ErrorHint(45, 'function Lat_vap', Press_hPA, notUsed, ii, modState)
         END IF

         ! if(debug.and.dectime>55.13.and.dectime<55.2)write(35,*)'% 1',Tw

         es_tw = sat_vap_press_x(Tw, Press_hPa, from, dectime, modState) !Calculate saturation vapour pressure in hPa

         !if(debug.and.dectime>55.13.and.dectime<55.2)write(35,*)'% 2',Tw

         IF (Press_hPa < 900) THEN
            CALL ErrorHint(45, 'function Lat_vap - 2', Press_hPA, notUsed, ii, modState)
         END IF

         psyc = psyc_const(cp, Press_hPa, lv_J_kg, modState) !in units hPa/K

         IF (Press_hPa < 900) THEN
            CALL ErrorHint(45, 'function Lat _vap -31', Press_hPA, notUsed, ii, modState)
         END IF

         ea_est = es_tw - psyc*(temp_C - tw)

         lv_J_kg = (lv_wetbulb - 2.365*tw)*1E3

         IF (switch1 .AND. switch2) THEN
            incr = incr/10.
            switch1 = .FALSE.
            switch2 = .FALSE.
         END IF
         IF (ABS(ea_est - ea_fix) < 0.001000) THEN
            RETURN
         ELSEIF (ea_est > ea_fix) THEN
            tw = tw - incr
            switch1 = .TRUE.
         ELSEIF (ea_est < ea_fix) THEN
            tw = tw + incr
            switch2 = .TRUE.
         END IF
      END DO

      RETURN
   END FUNCTION Lat_vap

   FUNCTION Lat_vapSublim(Temp_C, Ea_hPa, Press_hPa, cp) RESULT(lvS_J_kg)
      !Input: Air temperature, Water vapour pressure, Air pressure, heat capacity
      !Output: latent heat of sublimation in units J/kg

      IMPLICIT NONE

      REAL(KIND(1D0)) :: lvS_J_kg, temp_C, tw, incr, Ea_hPa, Press_hPa, cp
      ! REAL(KIND(1d0))::ea_fix,es_tw,psyc,ea_est,Temp_K
      ! REAL(KIND(1d0))::sat_vap_pressIce,psyc_const ! functions
      ! LOGICAL:: switch1=.FALSE.,switch2=.FALSE.!,debug=.true.
      ! INTEGER:: ii,from=2

      !Latent heat for sublimation
      !From Rogers&Yau (A short course in cloud physics), Wikipedia

      ! ea_fix=ea_hPa

      lvS_J_kg = (2834.1 - 0.29*temp_C)*1E3 !First guess for Ls in J/kg

      tw = Temp_C/2. !First estimate for wet bulb temperature
      incr = 3.
      Press_hPa = Press_hPa
      Ea_hPa = Ea_hPa
      cp = cp

      !DO ii=1,100

      !    es_tw=sat_vap_pressIce(Tw,Press_hPa,from)  !Calculate saturation vapour pressure in hPa

      ! psyc=psyc_const(cp,Press_hPa,lv_J_kg)

      !   ea_est=es_tw-psyc*(temp_C-tw)
      !  lvS_J_kg=(2834.1-0.29*tw)*1e3

      !   IF(switch1.AND.switch2)THEN
      !      incr=incr/10.
      !     switch1=.FALSE.
      !     switch2=.FALSE.
      !    ENDIF

      !   IF(ABS(ea_est-ea_fix)<0.001)THEN
      !     RETURN
      !   ELSEIF(ea_est > ea_fix)THEN
      !      tw=tw-incr
      !      switch1=.TRUE.
      !   ELSEIF(ea_est< ea_fix)THEN
      !      tw=tw+incr
      !      switch2=.TRUE.
      !    ENDIF
      !   ENDDO

      ! RETURN
   END FUNCTION Lat_vapSublim

   !=====================================================================
   !psyc_const.f   sg   nov 96
   !sg june 99 f90
   !calculate psyc - psychrometic constant Fritschen and Gay (1979)

   FUNCTION psyc_const(cp, Press_hPa, lv_J_kg, modState) RESULT(psyc_hPa) !In units hPa/K
      USE module_ctrl_const_gas
      USE module_ctrl_type, ONLY: SUEWS_STATE

      IMPLICIT NONE
      REAL(KIND(1D0)) :: cp, lv_J_kg, press_hPa, psyc_hpa
      TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState

      ! cp for moist air (shuttleworth p 4.13)
      IF (cp*press_hPa < 900 .OR. lv_J_kg < 10000) THEN
         CALL errorHint(19, &
                        'in psychrometric constant calculation:  cp [J kg-1 K-1], p [hPa], Lv [J kg-1]', &
                        cp, Press_hPa, INT(lv_J_kg), modState)
      END IF

      psyc_hPa = (cp*press_hPa)/(epsil*lv_J_kg)
      !    if(debug)write(*,*)psyc_hpa, 'g',cp,press_HPa,lv
      ! LV MJKg-1
      !www.cimis.water.ca.gov/infoEotPmEquation.jsp
      !psyc_hPa=(0.00163*(Press_hPa/10)/LV)*10
      ! write(*,*)psyc_hpa
      !psyc=psyc*100.! convert into Pa
   END FUNCTION psyc_const

   !==========================================================

   FUNCTION dewpoint(ea_hPa) RESULT(Temp_C_dew)
      ! ea = vapor pressure (hPa)
      ! td = dewpoint (oC)
      !calculates dewpoint in degC from
      ! http://www.atd.ucar.edu/weather_fl/dewpoint.html
      !     dewpoint = (237.3 * ln(e_vp/6.1078)) / (17.27 - (ln(e_vp/6.1078)))

      REAL(KIND(1D0)) :: ea_hPa, temp_C_dew
      Temp_C_dew = (237.3*LOG(ea_hPa/6.1078))/(17.27 - (LOG(ea_hPa/6.1078)))
   END FUNCTION dewpoint
   !===============================================================================
   FUNCTION slope_svp(temp_C) RESULT(s_hPa)
      !COEFFICENTS FOR CALCULATING desat/dT
      !Slope of the saturation vapor pressure vst air temperature curve

      IMPLICIT NONE

      REAL(KIND(1D0)) :: b1, b2, b3, b4, b5, b6, b7, S_hPa, temp_C
      B1 = 4.438099984D-1
      B2 = 2.857002636D-2
      B3 = 7.938054040D-4
      B4 = 1.215215065D-5
      B5 = 1.036561403D-7
      B6 = 3.532421810D-10
      B7 = -7.090244804D-13

      !     s - slope of saturation vapour pressure curve - Lowe (1977) -T (K)
      !     mb /K
      S_hPa = B1 + temp_C*(B2 + temp_C*(B3 + temp_C*(B4 + temp_C*(B5 + temp_C*(B6 + B7*temp_C)))))
      ! write(*,*)'s',s_hpa,temp_C
      !s_Pa=s_Pa*100  ! Pa/K
      !www.cimis.water.ca.gov/infoEotPmEquation.jsp
      ! s_hPa=(((4099 *(es_hPa/10))/(Temp_C+273.3)**2))*10
      ! if(debug)write(*,*)s_hpa
      RETURN
   END FUNCTION slope_svp

   !===============================================================================
   FUNCTION slopeIce_svp(temp_C) RESULT(s_hPa)
      !COEFFICENTS FOR CALCULATING desat/dT
      !Slope of the saturation vapor pressure vst air temperature curve

      IMPLICIT NONE

      REAL(KIND(1D0)) :: b1, b2, b3, b4, b5, b6, b7, S_hPa, temp_C

      B1 = 5.030305237D-1
      B2 = 3.773255020D-2
      B3 = 1.267995369D-3
      B4 = 2.477563108D-5
      B5 = 3.005693132D-7
      B6 = 2.158542548D-9
      B7 = 7.131097725D-12

      ! s - slope of saturation vapour pressure curve - Lowe (1977) -T (K)
      ! mb /K
      S_hPa = B1 + temp_C*(B2 + temp_C*(B3 + temp_C*(B4 + temp_C*(B5 + temp_C*(B6 + B7*temp_C)))))

      RETURN
   END FUNCTION slopeIce_svp

   !------------------------------------------------------------------------
   FUNCTION qsatf(T, PMB, modState) RESULT(qsat)
      !       MRR, 1987
      ! AT TEMPERATURE T (DEG C) AND PRESSURE PMB (MB), GET SATURATION SPECIFIC
      !       HUMIDITY (KG/KG) FROM TETEN FORMULA

      REAL(KIND(1D0)) :: T, es, qsat, PMB
      TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState

      REAL(KIND(1D0)), PARAMETER :: &
         !Teten coefficients
         A = 6.106, &
         B = 17.27, &
         C = 237.3, &
         molar = 0.028965, & !Dry air molar fraction in kg/mol
         molar_wat_vap = 0.0180153 !Molar fraction of water vapor in kg/mol

      IF (t > 55) THEN
         CALL ErrorHint(34, 'Function qsatf', T, 0.00D0, -55, modState)
      END IF

      ES = A*dEXP(B*T/(C + T))
      qsat = (molar_wat_vap/molar)*ES/PMB !(rmh2o/rmair)*ES/PMB
   END FUNCTION qsatf

   FUNCTION RH2qa(RH_dec, pres_hPa, Ta_degC) RESULT(qa_gkg)
      ! convert relative humidity to specific humidity
      ! TS 31 Jul 2018: initial version
      ! Brutsaert (2005) section 2.1.2, eqn 2.2, 2.4 and 2.5.
      REAL(KIND(1D0)), INTENT(in) :: RH_dec ! relative humidity in decimal
      REAL(KIND(1D0)), INTENT(in) :: pres_hPa ! atmospheric pressure in hPa
      REAL(KIND(1D0)), INTENT(in) :: Ta_degC ! air temperature in degC

      REAL(KIND(1D0)) :: es ! saturation vapour pressure in hPa
      REAL(KIND(1D0)) :: ea ! vapour pressure in hPa
      REAL(KIND(1D0)) :: qa_gkg ! specific humidity in (g kg-1)

      es = sat_vap_press(Ta_degC + C2K, pres_hPa)
      ea = es*RH_dec ! Brutsaert (2005) section 2.1.2, eqn 2.3
      qa_gkg = 0.622*ea/(pres_hPa - 0.378*ea)*1000 ! eqn 2.2, 2.4 and 2.5.

   END FUNCTION RH2qa

   FUNCTION qa2RH(qa_gkg, pres_hPa, Ta_degC) RESULT(RH)
      ! convert specific humidity to relative humidity
      ! TS 31 Jul 2018: initial version
      ! Brutsaert (2005) section 2.1.2, eqn 2.2, 2.4 and 2.5.
      REAL(KIND(1D0)), INTENT(in) :: qa_gkg ! specific humidity in (g kg-1)
      REAL(KIND(1D0)), INTENT(in) :: pres_hPa ! atmospheric pressure in hPa
      REAL(KIND(1D0)), INTENT(in) :: Ta_degC ! air temperature in degC
      REAL(KIND(1D0)) :: RH ! relative humidity in decimal

      REAL(KIND(1D0)) :: es ! saturation vapour pressure in hPa
      REAL(KIND(1D0)) :: ea ! vapour pressure in hPa
      REAL(KIND(1D0)) :: qa_kgkg !specific humidity in (kg kg-1)

      qa_kgkg = qa_gkg/1000
      es = sat_vap_press(Ta_degC + C2K, pres_hPa)
      ea = 500*pres_hPa*qa_kgkg/(311 + 189*qa_kgkg)
      ! qa=0.622*ea/(pres_hPa-0.378*ea)*1000 ! eqn 2.2, 2.4 and 2.5.
      RH = ea/es ! Brutsaert (2005) section 2.1.2, eqn 2.3

      ! set an upper limit
      IF (RH > 1) RH = 1

   END FUNCTION qa2RH

END MODULE module_util_meteo

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE METEO
   USE module_util_meteo
END MODULE METEO
