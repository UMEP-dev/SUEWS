! subroutines included:
! day2month
!
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
MODULE module_util_time
   USE module_ctrl_const_mathconst, ONLY: DEG2RAD, pi
   USE module_ctrl_const_calendar, ONLY: &
      NDAYS_WEEK, DOW_SUNDAY, DOW_SATURDAY, DAYTYPE_WEEKDAY, DAYTYPE_WEEKEND
   IMPLICIT NONE
   PRIVATE :: SUEWS_cal_local_standard_timer, is_same_calendar_day
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
      DOW = MOD(DAY, NDAYS_WEEK) + DOW_SUNDAY

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

      !-----------------------------------------------------------------------
      !     Calculation of declination of sun (Eqn. 16). Amplitude= +/-23.45
      !     deg. Minimum = DOY 355 (DEC 21), maximum = DOY 172.5 (JUN 21/22).
      DEC = -23.45*COS(2.0*PI*(DOY + 10.0)/365.0)

      !     Sun angles.  SOC limited for latitudes above polar circles.
      SOC = TAN(DEG2RAD*DEC)*TAN(DEG2RAD*XLAT)
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
! PI    Double-precision pi (rad)
! RAD   Shared degree-to-radian factor (rad./deg.)
! SNDN  Time of sunset (hr)
! SNUP  Time of sunrise (hr)
! SOC   Sine over cosine (intermediate calculation)
! XLAT  Latitude (deg.)
!=======================================================================

! Calculate dectime
   SUBROUTINE SUEWS_cal_dectime( &
      timer, & ! input
      dectime) ! output
      USE module_ctrl_type, ONLY: SUEWS_TIMER

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

   END SUBROUTINE SUEWS_cal_dectime

! Calculate tstep-derived variables
   SUBROUTINE SUEWS_cal_tstep( &
      timer, & ! input
      nsh, nsh_real, tstep_real) ! output
      USE module_ctrl_type, ONLY: SUEWS_TIMER
      USE module_ctrl_const_physconst, ONLY: sec_per_hour
      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer

      INTEGER :: tstep ! number of timesteps per hour
      ! values that are derived from tstep
      INTEGER, INTENT(out) :: nsh ! number of timesteps per hour
      REAL(KIND(1D0)), INTENT(out) :: nsh_real ! nsh in type real
      REAL(KIND(1D0)), INTENT(out) :: tstep_real ! tstep in type real

      tstep = timer%tstep
      nsh = sec_per_hour/tstep
      nsh_real = nsh*1.0
      tstep_real = tstep*1.0

   END SUBROUTINE SUEWS_cal_tstep

   SUBROUTINE SUEWS_cal_weekday( &
      timer, siteInfo, & !input
      dayofWeek_id) !output

      USE module_ctrl_type, ONLY: SUEWS_TIMER, SUEWS_SITE

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

   END SUBROUTINE SUEWS_cal_weekday

   SUBROUTINE SUEWS_cal_DLS( &
      timer, ahemisPrm, & !input
      DLS) !output

      USE module_ctrl_type, ONLY: SUEWS_TIMER, anthroEMIS_PRM

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(anthroEMIS_PRM), INTENT(IN) :: ahemisPrm

      INTEGER :: id, startDLS, endDLS
      INTEGER, INTENT(out) :: DLS

      id = timer%id
      startDLS = ahemisPrm%start_dls
      endDLS = ahemisPrm%end_dls

      DLS = 0
      IF (startDLS <= 0 .OR. endDLS <= 0 .OR. startDLS == endDLS) RETURN
      IF (startDLS < endDLS) THEN
         ! Northern Hemisphere (intra-year window): active strictly between startDLS and endDLS.
         IF (id > startDLS .AND. id < endDLS) DLS = 1
      ELSE
         ! Southern Hemisphere (year-wrapping window, startDLS > endDLS): active after startDLS or before endDLS.
         IF (id > startDLS .OR. id < endDLS) DLS = 1
      END IF

   END SUBROUTINE SUEWS_cal_DLS

   !===========================================================================
   ! Centralised time helpers and timer orchestration (GH#1559).
   ! Single definition of the daylight-saving profile-hour shift and the
   ! weekday/weekend index that were previously duplicated across physics
   ! modules, plus a single entry point that computes all derived timer values.
   !===========================================================================

   PURE FUNCTION cal_profile_hour(it, dls) RESULT(ih)
      ! Standard-Time hour for diurnal-profile lookup: undo the daylight-saving
      ! shift so a forcing hour maps onto the Standard-Time profile.
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: it ! forcing hour of day [0-23]
      INTEGER, INTENT(IN) :: dls ! daylight-saving offset [0 or 1 h]
      INTEGER :: ih

      ih = it - dls
      IF (ih < 0) ih = 23
   END FUNCTION cal_profile_hour

   PURE FUNCTION cal_weekday_index(dow1) RESULT(iu)
      ! Weekday/weekend index for profile selection: DAYTYPE_WEEKDAY (1) or DAYTYPE_WEEKEND (2).
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: dow1 ! day of week, dayofWeek_id(1): DOW_SUNDAY..DOW_SATURDAY
      INTEGER :: iu

      iu = DAYTYPE_WEEKDAY
      IF (dow1 == DOW_SUNDAY .OR. dow1 == DOW_SATURDAY) iu = DAYTYPE_WEEKEND
   END FUNCTION cal_weekday_index

   SUBROUTINE SUEWS_cal_local_standard_timer(timer, siteInfo, forcing_timestamp_reference, timer_st)
      ! Derive the site's fixed-offset standard clock without changing the
      ! forcing/main clock. Daylight-saving time is deliberately not applied.
      USE module_ctrl_type, ONLY: SUEWS_TIMER, SUEWS_SITE
      USE module_ctrl_const_physconst, ONLY: min_per_day
      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      INTEGER, INTENT(IN) :: forcing_timestamp_reference
      TYPE(SUEWS_TIMER), INTENT(OUT) :: timer_st

      INTEGER :: offset_minutes
      INTEGER :: total_minutes
      INTEGER :: minutes_in_year
      INTEGER :: year_days
      INTEGER :: day_minutes

      timer_st = timer
      offset_minutes = 0
      IF (forcing_timestamp_reference == 1) THEN
         offset_minutes = NINT(siteInfo%timezone*60.0D0)
      END IF

      total_minutes = (timer%id - 1)*min_per_day + timer%it*60 + timer%imin + offset_minutes
      timer_st%iy = timer%iy

      DO
         CALL LeapYearCalc(timer_st%iy, year_days)
         minutes_in_year = year_days*min_per_day
         IF (total_minutes >= 0 .AND. total_minutes < minutes_in_year) EXIT
         IF (total_minutes < 0) THEN
            timer_st%iy = timer_st%iy - 1
            CALL LeapYearCalc(timer_st%iy, year_days)
            total_minutes = total_minutes + year_days*min_per_day
         ELSE
            total_minutes = total_minutes - minutes_in_year
            timer_st%iy = timer_st%iy + 1
         END IF
      END DO

      timer_st%id = total_minutes/min_per_day + 1
      day_minutes = MOD(total_minutes, INT(min_per_day))
      timer_st%it = day_minutes/60
      timer_st%imin = MOD(day_minutes, 60)
   END SUBROUTINE SUEWS_cal_local_standard_timer

   PURE LOGICAL FUNCTION is_same_calendar_day(a, b) RESULT(same_day)
      USE module_util_datetime, ONLY: datetime
      IMPLICIT NONE
      TYPE(datetime), INTENT(IN) :: a, b

      same_day = (a%getDay() == b%getDay()) .AND. &
                 (a%getMonth() == b%getMonth()) .AND. &
                 (a%getYear() == b%getYear())
   END FUNCTION is_same_calendar_day

   SUBROUTINE SUEWS_cal_clock(timer, siteInfo, ahemisPrm, forcing_timestamp_reference, clock)
      ! Single entry point that derives the per-step clock from the caller's
      ! timer (gh#2333). The timer is read-only; the clock is rebuilt from
      ! scratch every call, so nothing derived can leak between timesteps.
      ! The main clock follows the forcing reference; solar and profile
      ! consumers also receive a fixed-offset site clock and right-closed
      ! local-standard delivery day (gh#2381).
      USE module_ctrl_type, ONLY: SUEWS_TIMER, SUEWS_CLOCK, SUEWS_SITE, anthroEMIS_PRM

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(anthroEMIS_PRM), INTENT(IN) :: ahemisPrm
      INTEGER, INTENT(IN) :: forcing_timestamp_reference
      TYPE(SUEWS_CLOCK), INTENT(OUT) :: clock
      TYPE(SUEWS_TIMER) :: timer_st
      INTEGER :: deliv_year_st, deliv_month_st, deliv_day_st, deliv_doy_st, deliv_dow_st
      LOGICAL :: first_step_of_deliv_day_st

      clock%timer = timer

      CALL SUEWS_cal_dectime(timer, clock%dectime)
      CALL SUEWS_cal_tstep(timer, clock%nsh, clock%nsh_real, clock%tstep_real)
      CALL SUEWS_cal_weekday(timer, siteInfo, clock%dayofWeek_id)
      CALL SUEWS_cal_day_boundaries( &
         timer, siteInfo%lat, clock%dayofWeek_id, &
         clock%first_step_of_run, clock%first_step_of_day, clock%last_step_of_day, &
         clock%prev_step_closed_day, clock%first_step_of_calendar_day, &
         clock%id_day, clock%dayofWeek_day, clock%elapsed_sec_of_day)
      CALL SUEWS_cal_local_standard_timer(timer, siteInfo, forcing_timestamp_reference, timer_st)
      CALL SUEWS_cal_dectime(timer_st, clock%dectime_st)
      CALL SUEWS_cal_weekday(timer_st, siteInfo, clock%dayofWeek_id_st)
      CALL SUEWS_cal_DLS(timer_st, ahemisPrm, clock%DLS)
      clock%iy_st = timer_st%iy
      clock%id_st = timer_st%id
      clock%it_st = timer_st%it
      clock%imin_st = timer_st%imin
      clock%tz_solar = siteInfo%timezone
      IF (forcing_timestamp_reference == 1) clock%tz_solar = 0.0D0
      CALL SUEWS_cal_interval_day_st( &
         clock, deliv_year_st, deliv_month_st, deliv_day_st, &
         deliv_doy_st, deliv_dow_st, first_step_of_deliv_day_st)
      clock%deliv_year_st = deliv_year_st
      clock%deliv_month_st = deliv_month_st
      clock%deliv_day_st = deliv_day_st
      clock%deliv_doy_st = deliv_doy_st
      clock%deliv_dow_st = deliv_dow_st
      clock%first_step_of_deliv_day_st = first_step_of_deliv_day_st
   END SUBROUTINE SUEWS_cal_clock

   SUBROUTINE SUEWS_cal_day_boundaries( &
      timer, lat, dayofWeek_id, &
      first_step_of_run, first_step_of_day, last_step_of_day, &
      prev_step_closed_day, first_step_of_calendar_day, &
      id_day, dayofWeek_day, elapsed_sec_of_day)
      ! Day-boundary predicates and right-closed accumulated-day identity on
      ! the main (forcing) clock (gh#2333, gh#2382).
      ! Under SUEWS's closed="right", label="right" convention, every step
      ! ending at t in (D 00:00, D+1 00:00] belongs to calendar day
      ! date(t - 1 s), matching pandas resample(..., closed="right",
      ! label="right") and SUEWS_cal_interval_day_st across midnight-aligned
      ! and offset regular grids. DyOHM preserves calendar-day midnight
      ! updates via first_step_of_calendar_day.
      USE module_ctrl_type, ONLY: SUEWS_TIMER
      USE module_ctrl_const_physconst, ONLY: sec_per_hour
      USE module_util_datetime, ONLY: datetime, timedelta

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      REAL(KIND(1D0)), INTENT(IN) :: lat
      INTEGER, DIMENSION(3), INTENT(IN) :: dayofWeek_id
      LOGICAL, INTENT(OUT) :: first_step_of_run
      LOGICAL, INTENT(OUT) :: first_step_of_day
      LOGICAL, INTENT(OUT) :: last_step_of_day
      LOGICAL, INTENT(OUT) :: prev_step_closed_day
      LOGICAL, INTENT(OUT) :: first_step_of_calendar_day
      INTEGER, INTENT(OUT) :: id_day
      INTEGER, DIMENSION(3), INTENT(OUT) :: dayofWeek_day
      INTEGER, INTENT(OUT) :: elapsed_sec_of_day

      TYPE(datetime) :: time_now, time_prev, day_now, day_prev, day_next, prev_pred_next
      TYPE(timedelta) :: one_sec, step_dt
      INTEGER :: iy_day, mb_day, date_day, seas_day, wd_day

      one_sec = timedelta(seconds=1)
      step_dt = timedelta(seconds=timer%tstep)
      time_now = datetime(year=timer%iy) + &
                 timedelta(days=timer%id - 1, hours=timer%it, minutes=timer%imin, seconds=timer%isec)
      time_prev = time_now - step_dt
      day_now = time_now - one_sec
      day_prev = time_prev - one_sec
      day_next = (time_now + step_dt) - one_sec

      first_step_of_run = timer%dt_since_start == timer%tstep
      first_step_of_day = .NOT. is_same_calendar_day(day_now, day_prev)
      last_step_of_day = .NOT. is_same_calendar_day(day_now, day_next)
      IF (timer%tstep_prev > 0) THEN
         prev_pred_next = day_prev + timedelta(seconds=timer%tstep_prev)
         prev_step_closed_day = .NOT. is_same_calendar_day(day_prev, prev_pred_next)
      ELSE
         prev_step_closed_day = .FALSE.
      END IF
      first_step_of_calendar_day = .NOT. is_same_calendar_day(time_now, time_prev)

      IF (.NOT. is_same_calendar_day(time_now, day_now)) THEN
         iy_day = day_now%getYear()
         id_day = day_now%yearday()
         CALL day2month(id_day, mb_day, date_day, seas_day, iy_day, lat)
         CALL Day_Of_Week(date_day, mb_day, iy_day, wd_day)
         dayofWeek_day = [wd_day, mb_day, seas_day]
      ELSE
         id_day = timer%id
         dayofWeek_day = dayofWeek_id
      END IF
      elapsed_sec_of_day = day_now%getHour()*NINT(sec_per_hour) &
                           + day_now%getMinute()*60 &
                           + day_now%getSecond() + 1
   END SUBROUTINE SUEWS_cal_day_boundaries

   SUBROUTINE SUEWS_cal_interval_day_st( &
      clock, deliv_year, deliv_month, deliv_day, deliv_doy, deliv_dow, is_new_day)
      ! Resolve the right-closed local-standard delivery day (D 00:00, D+1 00:00]
      ! owning the current forcing interval (t_end - step_dt, t_end] on the
      ! site's fixed-offset local standard clock (gh#2381). Any interval endpoint
      ! t_end in (D 00:00, D+1 00:00] maps to date(t_end - 1 s), and the previous
      ! interval ended at t_prev = t_end - step_dt -> date(t_prev - 1 s), which
      ! stays exact even when adaptive steps or fractional-hour timezones
      ! (e.g. UTC+05:30 with hourly forcing) straddle local midnight.
      USE module_ctrl_type, ONLY: SUEWS_CLOCK
      USE module_ctrl_const_physconst, ONLY: sec_per_day, sec_per_hour
      IMPLICIT NONE

      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      INTEGER, INTENT(OUT) :: deliv_year, deliv_month, deliv_day, deliv_doy, deliv_dow
      LOGICAL, INTENT(OUT) :: is_new_day

      INTEGER :: iy_end, id_end, it_end, imin_end
      INTEGER :: sec_day, sec_hour, step_dt
      INTEGER :: cur_sec, cur_yr, prev_sec, prev_yr, year_days, year_sec
      INTEGER :: cur_doy, prev_doy, seas

      sec_day = NINT(sec_per_day)
      sec_hour = NINT(sec_per_hour)

      IF (clock%iy_st > 0) THEN
         iy_end = clock%iy_st
         id_end = clock%id_st
         it_end = clock%it_st
         imin_end = clock%imin_st
      ELSE
         iy_end = clock%timer%iy
         id_end = clock%timer%id
         it_end = clock%timer%it
         imin_end = clock%timer%imin
      END IF

      IF (clock%timer%tstep > 0) THEN
         step_dt = clock%timer%tstep
      ELSE IF (clock%nsh > 0) THEN
         step_dt = sec_hour/clock%nsh
      ELSE
         step_dt = 0
      END IF

      ! Current delivery day via right-closed endpoint: date(t_end - 1 s)
      cur_yr = iy_end
      cur_sec = (id_end - 1)*sec_day + it_end*sec_hour + imin_end*60 + clock%timer%isec - 1
      DO
         CALL LeapYearCalc(cur_yr, year_days)
         year_sec = year_days*sec_day
         IF (cur_sec >= 0 .AND. cur_sec < year_sec) EXIT
         IF (cur_sec < 0) THEN
            cur_yr = cur_yr - 1
            CALL LeapYearCalc(cur_yr, year_days)
            cur_sec = cur_sec + year_days*sec_day
         ELSE
            cur_sec = cur_sec - year_sec
            cur_yr = cur_yr + 1
         END IF
      END DO
      cur_doy = cur_sec/sec_day + 1

      ! Previous interval's delivery day: date((t_end - step_dt) - 1 s)
      prev_yr = cur_yr
      prev_sec = cur_sec - step_dt
      DO
         CALL LeapYearCalc(prev_yr, year_days)
         year_sec = year_days*sec_day
         IF (prev_sec >= 0 .AND. prev_sec < year_sec) EXIT
         IF (prev_sec < 0) THEN
            prev_yr = prev_yr - 1
            CALL LeapYearCalc(prev_yr, year_days)
            prev_sec = prev_sec + year_days*sec_day
         ELSE
            prev_sec = prev_sec - year_sec
            prev_yr = prev_yr + 1
         END IF
      END DO
      prev_doy = prev_sec/sec_day + 1

      deliv_year = cur_yr
      deliv_doy = cur_doy
      CALL day2month(deliv_doy, deliv_month, deliv_day, seas, deliv_year, 0.0D0)
      CALL Day_Of_Week(deliv_day, deliv_month, deliv_year, deliv_dow)
      is_new_day = (clock%timer%dt_since_start == step_dt) .OR. &
                   (cur_yr /= prev_yr) .OR. (cur_doy /= prev_doy)
   END SUBROUTINE SUEWS_cal_interval_day_st

END MODULE module_util_time
