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
! Main module following naming standard: matches filename
MODULE suews_util_time
   IMPLICIT NONE

CONTAINS
   SUBROUTINE day2month(b, mb, md, seas, year, latitude)
      IMPLICIT NONE
      INTEGER, INTENT(in) :: b !b=doy   --IN
      INTEGER, INTENT(out) :: mb !month=mb  --OUT
      INTEGER, INTENT(out) :: md !date=md --OUT
      INTEGER, INTENT(out) :: seas
      INTEGER, INTENT(in) :: year
      INTEGER :: t1, t2, t3
      INTEGER :: k ! k- accounts for leap year

      REAL(KIND(1D0)) :: latitude

      ! initialisation
      mb = 1

      !Corrected and calculation of date added LJ (Jun 2010)

      t1 = 4
      t2 = 100
      t3 = 400

      IF ((MODULO(year, t1) == 0) .AND. (MODULO(year, t2) /= 0) .OR. (MODULO(year, t3) == 0)) THEN
         K = 1
      ELSE
         K = 0
      END IF

      IF (B <= 31) THEN !January
         MB = 1
         md = B
      ELSEIF (B > 31 .AND. B <= 59 + K) THEN
         MB = 2
         md = B - 31
      ELSEIF (B > 59 + K .AND. B <= 90 + K) THEN
         MB = 3
         md = B - (59 + K)
      ELSEIF (B > 90 + K .AND. B <= 120 + K) THEN
         MB = 4
         md = B - (90 + K)
      ELSEIF (B > 120 + K .AND. B <= 151 + K) THEN
         MB = 5
         md = B - (120 + K)
      ELSEIF (B > 151 + K .AND. B <= 181 + K) THEN
         MB = 6
         md = B - (151 + K)
      ELSEIF (B > 181 + K .AND. B <= 212 + K) THEN
         MB = 7
         md = B - (181 + K)
      ELSEIF (B > 212 + K .AND. B <= 243 + K) THEN
         MB = 8
         md = B - (212 + K)
      ELSEIF (B > 243 + K .AND. B <= 273 + K) THEN
         MB = 9
         md = B - (243 + K)
      ELSEIF (B > 273 + K .AND. B <= 304 + K) THEN
         MB = 10
         md = B - (273 + K)
      ELSEIF (B > 304 + K .AND. B <= 334 + K) THEN
         MB = 11
         md = B - (304 + K)
      ELSEIF (B > 334 + K) THEN
         MB = 12
         md = B - (334 + K)
      END IF

      !
      IF (latitude > 0) THEN ! Northern Hemisphere
         IF (mb > 3 .AND. mb < 10) THEN !Summer is from Apr to Sep
            seas = 1
         ELSE
            seas = 2 !Winter rest of the months
         END IF
      ELSE ! southern hemisphere
         IF (mb < 4 .OR. mb > 9) THEN !Summer is from Oct to Mar
            seas = 1
         ELSE
            seas = 2 !Winter rest of the months
         END IF
      END IF
      RETURN
   END SUBROUTINE day2month
!===============================================================================
   SUBROUTINE month2day(mon, ne, k, b)
      IMPLICIT NONE
      INTEGER :: mon, ne, k, b

      IF (mon == 1) THEN
         NE = 32 - B
      ELSE IF (mon == 2) THEN
         NE = 60 + K - B
      ELSE IF (mon == 3) THEN
         NE = 91 + K - B
      ELSE IF (mon == 4) THEN
         NE = 121 + K - B
      ELSE IF (mon == 5) THEN
         NE = 152 + K - B
      ELSE IF (mon == 6) THEN
         NE = 182 + K - B
      ELSE IF (mon == 7) THEN
         NE = 213 + K - B
      ELSE IF (mon == 8) THEN
         NE = 244 + K - B
         !**********PAGE 151 STARTS HERE**************
      ELSE IF (mon == 9) THEN
         NE = 274 + K - B
      ELSE IF (mon == 10) THEN
         NE = 305 + K - B
      ELSE IF (mon == 11) THEN
         NE = 335 + K - B
      ELSE IF (mon == 12) THEN
         NE = 366 + K - B
      END IF
   END SUBROUTINE month2day
!===============================================================================
!Defines the number or days in each year (defines the leap year)
   SUBROUTINE LeapYearCalc(year_int, nroDays)

      IMPLICIT NONE

      INTEGER :: nroDays, year_int

      IF (MOD(year_int, 100) /= 0 .AND. MOD(year_int, 4) == 0) THEN
         nroDays = 366
      ELSEIF (MOD(year_int, 400) == 0) THEN
         nroDays = 366
      ELSE
         nroDays = 365
      END IF
   END SUBROUTINE LeapYearCalc

!===============================================================================
!Defines the number or days in each year (defines the leap year)
! Ting Sun 09 May 2018
   ELEMENTAL FUNCTION Days_of_Year(year_int) RESULT(nDays)
      IMPLICIT NONE
      INTEGER, INTENT(in) :: year_int
      INTEGER :: nDays

      IF (MOD(year_int, 100) /= 0 .AND. MOD(year_int, 4) == 0) THEN
         nDays = 366
      ELSEIF (MOD(year_int, 400) == 0) THEN
         nDays = 366
      ELSE
         nDays = 365
      END IF

   END FUNCTION Days_of_Year

!===============================================================================

   SUBROUTINE Day_Of_Week(DATE, MONTH, YEAR, DOW)
      ! Calculate weekday from year, month and day information.
      ! DOW: Sunday=1,...Saturday=7
      ! YEAR fixed to integer, LJ March 2015

      IMPLICIT NONE

      INTEGER DATE, MONTH, DAY, YR, MN, N1, N2, DOW, YEAR

      YR = YEAR
      MN = MONTH

      !C
      !C       IF JANUARY OR FEBRUARY, ADJUST MONTH AND YEAR
      !C
      IF (MN > 2) GO TO 10
      MN = MN + 12
      YR = YR - 1
10    N1 = (26*(MN + 1))/10
      N2 = (125*YR)/100
      DAY = (DATE + N1 + N2 - (YR/100) + (YR/400) - 1)
      DOW = MOD(DAY, 7) + 1

      RETURN
   END SUBROUTINE Day_Of_Week

!===============================================================================

!FL
   SUBROUTINE dectime_to_timevec(dectime, HOURS, MINS, SECS)
      !This subroutine converts dectime to individual
      !hours, minutes and seconds
      INTEGER :: HOURS, MINS, doy
      REAL(KIND(1D0)) :: dectime, SECS, DH, DM, DS
      !INTEGER :: year

      doy = FLOOR(dectime)

      DH = dectime - doy !Decimal hours
      HOURS = INT(24*DH)

      DM = 24*DH - HOURS !Decimal minutes
      MINS = INT(60*DM)

      DS = 60*DM - MINS
      SECS = INT(60*DS)

   END SUBROUTINE dectime_to_timevec

!==============================================================================

!FL

   SUBROUTINE DAYLEN(DOY, XLAT, DAYL, DEC, SNDN, SNUP)
      !=======================================================================
      !  DAYLEN, Real Function, N.B. Pickering, 09/23/1993
      !  Computes solar day length (Spitters, 1986).
      !=======================================================================
      !-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER :: DOY
      REAL(KIND(1D0)), INTENT(IN) :: XLAT
      REAL(KIND(1D0)), INTENT(OUT) :: DEC, DAYL, SNDN, SNUP
      REAL(KIND(1D0)) :: SOC
      REAL(KIND(1D0)), PARAMETER :: PI = 3.14159, RAD = PI/180.0

      !-----------------------------------------------------------------------
      !     Calculation of declination of sun (Eqn. 16). Amplitude= +/-23.45
      !     deg. Minimum = DOY 355 (DEC 21), maximum = DOY 172.5 (JUN 21/22).
      DEC = -23.45*COS(2.0*PI*(DOY + 10.0)/365.0)

      !     Sun angles.  SOC limited for latitudes above polar circles.
      SOC = TAN(RAD*DEC)*TAN(RAD*XLAT)
      SOC = MIN(MAX(SOC, -1.0), 1.0)

      !     Calculate daylength, sunrise and sunset (Eqn. 17)
      DAYL = 12.0 + 24.0*ASIN(SOC)/PI
      SNUP = 12.0 - DAYL/2.0
      SNDN = 12.0 + DAYL/2.0

   END SUBROUTINE DAYLEN

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

! Calculate dectime
   SUBROUTINE SUEWS_cal_dectime( &
      id, it, imin, isec, & ! input
      dectime) ! output
      IMPLICIT NONE
      INTEGER, INTENT(in) :: id, it, imin, isec

      REAL(KIND(1D0)), INTENT(out) :: dectime ! nsh in type real

      dectime = REAL(id - 1, KIND(1D0)) &
                + REAL(it, KIND(1D0))/24 &
                + REAL(imin, KIND(1D0))/(60*24) &
                + REAL(isec, KIND(1D0))/(60*60*24)

   END SUBROUTINE SUEWS_cal_dectime

   SUBROUTINE SUEWS_cal_dectime_DTS( &
      timer, & ! input
      dectime) ! output
      USE SUEWS_DEF_DTS, ONLY: SUEWS_TIMER

      IMPLICIT NONE
      ! INTEGER, INTENT(in) :: id, it, imin, isec
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer

      REAL(KIND(1D0)), INTENT(out) :: dectime ! nsh in type real

      INTEGER :: id
      INTEGER :: it
      INTEGER :: imin
      INTEGER :: isec

      id = timer%id
      it = timer%it
      imin = timer%imin
      isec = timer%isec

      dectime = REAL(id - 1, KIND(1D0)) &
                + REAL(it, KIND(1D0))/24 &
                + REAL(imin, KIND(1D0))/(60*24) &
                + REAL(isec, KIND(1D0))/(60*60*24)

   END SUBROUTINE SUEWS_cal_dectime_DTS

! Calculate tstep-derived variables
   SUBROUTINE SUEWS_cal_tstep( &
      tstep, & ! input
      nsh, nsh_real, tstep_real) ! output
      IMPLICIT NONE
      INTEGER, INTENT(in) :: tstep ! number of timesteps per hour
      ! values that are derived from tstep
      INTEGER, INTENT(out) :: nsh ! number of timesteps per hour
      REAL(KIND(1D0)), INTENT(out) :: nsh_real ! nsh in type real
      REAL(KIND(1D0)), INTENT(out) :: tstep_real ! tstep in type real
      nsh = 3600/tstep
      nsh_real = nsh*1.0
      tstep_real = tstep*1.0

   END SUBROUTINE SUEWS_cal_tstep

   SUBROUTINE SUEWS_cal_tstep_DTS( &
      timer, & ! input
      nsh, nsh_real, tstep_real) ! output
      USE SUEWS_DEF_DTS, ONLY: SUEWS_TIMER

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer

      INTEGER :: tstep ! number of timesteps per hour
      ! values that are derived from tstep
      INTEGER, INTENT(out) :: nsh ! number of timesteps per hour
      REAL(KIND(1D0)), INTENT(out) :: nsh_real ! nsh in type real
      REAL(KIND(1D0)), INTENT(out) :: tstep_real ! tstep in type real

      tstep = timer%tstep
      nsh = 3600/tstep
      nsh_real = nsh*1.0
      tstep_real = tstep*1.0

   END SUBROUTINE SUEWS_cal_tstep_DTS

   SUBROUTINE SUEWS_cal_weekday( &
      iy, id, lat, & !input
      dayofWeek_id) !output
      IMPLICIT NONE

      INTEGER, INTENT(in) :: iy ! year
      INTEGER, INTENT(in) :: id ! day of year
      REAL(KIND(1D0)), INTENT(in) :: lat

      INTEGER, DIMENSION(3), INTENT(OUT) :: dayofWeek_id

      INTEGER :: wd
      INTEGER :: mb
      INTEGER :: date
      INTEGER :: seas

      CALL day2month(id, mb, date, seas, iy, lat) !Calculate real date from doy
      CALL Day_of_Week(date, mb, iy, wd) !Calculate weekday (1=Sun, ..., 7=Sat)

      dayofWeek_id(1) = wd !Day of week
      dayofWeek_id(2) = mb !Month
      dayofweek_id(3) = seas !Season

   END SUBROUTINE SUEWS_cal_weekday

   SUBROUTINE SUEWS_cal_weekday_DTS( &
      timer, siteInfo, & !input
      dayofWeek_id) !output

      USE SUEWS_DEF_DTS, ONLY: SUEWS_TIMER, SUEWS_SITE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      INTEGER :: iy ! year
      INTEGER :: id ! day of year
      REAL(KIND(1D0)) :: lat

      INTEGER, DIMENSION(3), INTENT(OUT) :: dayofWeek_id

      INTEGER :: wd
      INTEGER :: mb
      INTEGER :: date
      INTEGER :: seas

      iy = timer%iy
      id = timer%id
      lat = siteInfo%lat

      CALL day2month(id, mb, date, seas, iy, lat) !Calculate real date from doy
      CALL Day_of_Week(date, mb, iy, wd) !Calculate weekday (1=Sun, ..., 7=Sat)

      dayofWeek_id(1) = wd !Day of week
      dayofWeek_id(2) = mb !Month
      dayofweek_id(3) = seas !Season

   END SUBROUTINE SUEWS_cal_weekday_DTS

   SUBROUTINE SUEWS_cal_DLS( &
      id, startDLS, endDLS, & !input
      DLS) !output
      IMPLICIT NONE

      INTEGER, INTENT(in) :: id, startDLS, endDLS
      INTEGER, INTENT(out) :: DLS

      DLS = 0
      IF (id > startDLS .AND. id < endDLS) dls = 1

   END SUBROUTINE SUEWS_cal_DLS

   SUBROUTINE SUEWS_cal_DLS_DTS( &
      timer, ahemisPrm, & !input
      DLS) !output

      USE SUEWS_DEF_DTS, ONLY: SUEWS_TIMER, anthroEMIS_PRM

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(anthroEMIS_PRM), INTENT(IN) :: ahemisPrm

      INTEGER :: id, startDLS, endDLS
      INTEGER, INTENT(out) :: DLS

      id = timer%id
      startDLS = ahemisPrm%startDLS
      endDLS = ahemisPrm%endDLS

      DLS = 0
      IF (id > startDLS .AND. id < endDLS) dls = 1

   END SUBROUTINE SUEWS_cal_DLS_DTS

END MODULE suews_util_time

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE time_module
   USE suews_util_time
END MODULE time_module
