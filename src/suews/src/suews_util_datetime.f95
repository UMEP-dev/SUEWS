! Courtesy of wavebitscientific
! https://wavebitscientific.github.io/datetime-fortran/

!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD 3-clause license. See LICENSE for details.
!
MODULE mod_strftime
!=======================================================================
!
! mod_strftime: Interfaces to strftime and strptime procedures from
! from C/C++ standard library.
!
!=======================================================================

   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR, C_INT

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: tm_struct
   PUBLIC :: c_strftime
   PUBLIC :: c_strptime

   TYPE, BIND(c) :: tm_struct

      !! A derived type provided for compatibility with C/C++ time struct.
      !! Allows for calling strftime and strptime procedures through the
      !! iso_c_binding.

      INTEGER(kind=C_INT) :: tm_sec !! Seconds      [0-60] (1 leap second)
      INTEGER(kind=C_INT) :: tm_min !! Minutes      [0-59]
      INTEGER(kind=C_INT) :: tm_hour !! Hours        [0-23]
      INTEGER(kind=C_INT) :: tm_mday !! Day          [1-31]
      INTEGER(kind=C_INT) :: tm_mon !! Month        [0-11]
      INTEGER(kind=C_INT) :: tm_year !! Year - 1900
      INTEGER(kind=C_INT) :: tm_wday !! Day of week  [0-6]
      INTEGER(kind=C_INT) :: tm_yday !! Days in year [0-365]
      INTEGER(kind=C_INT) :: tm_isdst !! DST          [-1/0/1]

   END TYPE tm_struct
!=======================================================================

   INTERFACE

      !! Interface to C procedures strftime and strptime through
      !! iso_c_binding.

      FUNCTION c_strftime(str, slen, FORMAT, tm) &
         BIND(c, name='strftime') RESULT(rc)

         !! Returns a formatted time string, given input time struct and
         !! format. Refer to C standard library documentation for more
         !! information.

         IMPORT :: C_CHAR, C_INT
         IMPORT :: tm_struct

         IMPLICIT NONE

         ! Arguments
         CHARACTER(kind=C_CHAR), DIMENSION(*), INTENT(out) :: str !! result string
         INTEGER(kind=C_INT), VALUE, INTENT(in) :: slen !! string length
         CHARACTER(kind=C_CHAR), DIMENSION(*), INTENT(in) :: FORMAT !! time format
         TYPE(tm_struct), INTENT(in) :: tm !! tm_struct instance
         INTEGER(kind=C_INT) :: rc !! return code

      END FUNCTION c_strftime

      FUNCTION c_strptime(str, FORMAT, tm) BIND(c, name='strptime') RESULT(rc)

         !! Returns a time struct object based on the input time string str,
         !! formatted using format. Refer to C standard library documentation
         !! for more information.

         IMPORT :: C_CHAR, C_INT
         IMPORT :: tm_struct

         IMPLICIT NONE

         ! Arguments
         CHARACTER(kind=C_CHAR), DIMENSION(*), INTENT(in) :: str !! input string
         CHARACTER(kind=C_CHAR), DIMENSION(*), INTENT(in) :: FORMAT !! time format
         TYPE(tm_struct), INTENT(out) :: tm !! result tm_struct
         INTEGER(kind=C_INT) :: rc !! return code

      END FUNCTION c_strptime

   END INTERFACE
!=======================================================================
END MODULE mod_strftime

!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2016, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
MODULE mod_constants
!=======================================================================
!
! mod_constants: Basic constants and time conversion factors.
!
!=======================================================================

   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: zero, one, d2h, h2d, d2m, m2d, m2h, s2d, d2s, h2s, s2h, m2s, s2m, MAXSTRLEN

   REAL(kind=REAL64), PARAMETER :: zero = 0_REAL64 !! 0
   REAL(kind=REAL64), PARAMETER :: one = 1_REAL64 !! 1

! Constant multipliers that transform a number
! of some time unit to another:
   REAL(kind=REAL64), PARAMETER :: d2h = 24_REAL64 !! day    -> hour
   REAL(kind=REAL64), PARAMETER :: h2d = one/d2h !! hour   -> day
   REAL(kind=REAL64), PARAMETER :: d2m = d2h*60_REAL64 !! day    -> minute
   REAL(kind=REAL64), PARAMETER :: m2d = one/d2m !! minute -> day
   REAL(kind=REAL64), PARAMETER :: m2h = one/60_REAL64 !! minute -> hour
   REAL(kind=REAL64), PARAMETER :: s2d = m2d/60_REAL64 !! second -> day
   REAL(kind=REAL64), PARAMETER :: d2s = 86400_REAL64 !! day    -> second
   REAL(kind=REAL64), PARAMETER :: h2s = 3600_REAL64 !! hour   -> second
   REAL(kind=REAL64), PARAMETER :: s2h = one/h2s !! second -> hour
   REAL(kind=REAL64), PARAMETER :: m2s = 60_REAL64 !! minute -> second
   REAL(kind=REAL64), PARAMETER :: s2m = one/m2s !! second -> minute

! Maximum string length for strftime.
! Constant for now; may become a preprocessor macro later.
   INTEGER, PARAMETER :: MAXSTRLEN = 99

!=======================================================================
END MODULE mod_constants

!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD 3-clause license. See LICENSE for details.
!
MODULE mod_timedelta
!=======================================================================
!
! mod_timedelta: Module that provides the timedelta class and its
!                type-bound methods and operators.
!
!=======================================================================

   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: timedelta

   TYPE :: timedelta

      !! Class of objects that define difference between two datetime
      !! instances.

      PRIVATE

      INTEGER :: days = 0 !! number of days
      INTEGER :: hours = 0 !! number of hours
      INTEGER :: minutes = 0 !! number of minutes
      INTEGER :: seconds = 0 !! number of seconds
      INTEGER :: milliseconds = 0 !! number of milliseconds

   CONTAINS

      ! getter functions
      PROCEDURE, PASS(self), PUBLIC :: getDays
      PROCEDURE, PASS(self), PUBLIC :: getHours
      PROCEDURE, PASS(self), PUBLIC :: getMinutes
      PROCEDURE, PASS(self), PUBLIC :: getSeconds
      PROCEDURE, PASS(self), PUBLIC :: getMilliseconds

      ! public methods
      PROCEDURE, PUBLIC :: total_seconds

      ! operator overloading procedures
      PROCEDURE, PRIVATE :: timedelta_plus_timedelta
      PROCEDURE, PRIVATE :: timedelta_minus_timedelta
      PROCEDURE, PRIVATE :: unary_minus_timedelta
      PROCEDURE, PRIVATE :: eq
      PROCEDURE, PRIVATE :: neq
      PROCEDURE, PRIVATE :: gt
      PROCEDURE, PRIVATE :: ge
      PROCEDURE, PRIVATE :: lt
      PROCEDURE, PRIVATE :: le

      GENERIC :: OPERATOR(+) => timedelta_plus_timedelta
      GENERIC :: OPERATOR(-) => timedelta_minus_timedelta, &
         unary_minus_timedelta
      GENERIC :: OPERATOR(==) => eq
      GENERIC :: OPERATOR(/=) => neq
      GENERIC :: OPERATOR(>) => gt
      GENERIC :: OPERATOR(>=) => ge
      GENERIC :: OPERATOR(<) => lt
      GENERIC :: OPERATOR(<=) => le

   END TYPE timedelta

   INTERFACE timedelta
      MODULE PROCEDURE :: timedelta_constructor
   END INTERFACE timedelta

!=======================================================================
CONTAINS

   PURE ELEMENTAL TYPE(timedelta) FUNCTION timedelta_constructor(days, &
                                                                 hours, minutes, seconds, milliseconds)

      !! Constructor function for the `timedelta` class.

      INTEGER, INTENT(in), OPTIONAL :: days !! number of days
      INTEGER, INTENT(in), OPTIONAL :: hours !! number of hours
      INTEGER, INTENT(in), OPTIONAL :: minutes !! number of minutes
      INTEGER, INTENT(in), OPTIONAL :: seconds !! number of seconds
      INTEGER, INTENT(in), OPTIONAL :: milliseconds !! number of milliseconds

      IF (PRESENT(days)) THEN
         timedelta_constructor%days = days
      ELSE
         timedelta_constructor%days = 0
      END IF

      IF (PRESENT(hours)) THEN
         timedelta_constructor%hours = hours
      ELSE
         timedelta_constructor%hours = 0
      END IF

      IF (PRESENT(minutes)) THEN
         timedelta_constructor%minutes = minutes
      ELSE
         timedelta_constructor%minutes = 0
      END IF

      IF (PRESENT(seconds)) THEN
         timedelta_constructor%seconds = seconds
      ELSE
         timedelta_constructor%seconds = 0
      END IF

      IF (PRESENT(milliseconds)) THEN
         timedelta_constructor%milliseconds = milliseconds
      ELSE
         timedelta_constructor%milliseconds = 0
      END IF

   END FUNCTION timedelta_constructor

! timedelta getters
!=======================================================================

   PURE ELEMENTAL INTEGER FUNCTION getDays(self)
      !! Returns the number of days.
      CLASS(timedelta), INTENT(in) :: self !! `timedelta` instance
      getDays = self%days
   END FUNCTION getDays

   PURE ELEMENTAL INTEGER FUNCTION getHours(self)
      !! Returns the number of hours.
      CLASS(timedelta), INTENT(in) :: self !! `timedelta` instance
      getHours = self%hours
   END FUNCTION getHours

   PURE ELEMENTAL INTEGER FUNCTION getMinutes(self)
      !! Returns the number of minutes.
      CLASS(timedelta), INTENT(in) :: self !! `timedelta` instance
      getMinutes = self%minutes
   END FUNCTION getMinutes

   PURE ELEMENTAL INTEGER FUNCTION getSeconds(self)
      !! Returns the number of seconds.
      CLASS(timedelta), INTENT(in) :: self !! `timedelta` instance
      getSeconds = self%seconds
   END FUNCTION getSeconds

   PURE ELEMENTAL INTEGER FUNCTION getMilliseconds(self)
      !! Returns the number of milliseconds.
      CLASS(timedelta), INTENT(in) :: self !! `timedelta` instance
      getMilliseconds = self%milliseconds
   END FUNCTION getMilliseconds

   PURE ELEMENTAL REAL(kind=REAL64) FUNCTION total_seconds(self)

      !! Returns a total number of seconds contained in a `timedelta`
      !! instance.

      CLASS(timedelta), INTENT(in) :: self !! `timedelta` instance

      total_seconds = self%days*86400._REAL64 &
                      + self%hours*3600._REAL64 &
                      + self%minutes*60._REAL64 &
                      + self%seconds &
                      + self%milliseconds*1E-3_REAL64

   END FUNCTION total_seconds

   PURE ELEMENTAL FUNCTION timedelta_plus_timedelta(t0, t1) RESULT(t)

      !! Adds two `timedelta` instances together and returns a `timedelta`
      !! instance. Overloads the operator `+`.

      CLASS(timedelta), INTENT(in) :: t0 !! lhs `timedelta` instance
      TYPE(timedelta), INTENT(in) :: t1 !! rhs `timedelta` instance
      TYPE(timedelta) :: t !! result

      t = timedelta(days=t0%days + t1%days, &
                    hours=t0%hours + t1%hours, &
                    minutes=t0%minutes + t1%minutes, &
                    seconds=t0%seconds + t1%seconds, &
                    milliseconds=t0%milliseconds + t1%milliseconds)

   END FUNCTION timedelta_plus_timedelta

   PURE ELEMENTAL FUNCTION timedelta_minus_timedelta(t0, t1) RESULT(t)

      !! Subtracts a `timedelta` instance from another. Returns a
      !! `timedelta` instance. Overloads the operator `-`.

      CLASS(timedelta), INTENT(in) :: t0 !! lhs `timedelta` instance
      TYPE(timedelta), INTENT(in) :: t1 !! lhs `timedelta` instance
      TYPE(timedelta) :: t !! result

      t = t0 + (-t1)

   END FUNCTION timedelta_minus_timedelta

   PURE ELEMENTAL FUNCTION unary_minus_timedelta(t0) RESULT(t)

      !! Takes a negative of a `timedelta` instance. Overloads the operator
      !! `-`.

      CLASS(timedelta), INTENT(in) :: t0 !! `timedelta` instance
      TYPE(timedelta) :: t !! result

      t%days = -t0%days
      t%hours = -t0%hours
      t%minutes = -t0%minutes
      t%seconds = -t0%seconds
      t%milliseconds = -t0%milliseconds

   END FUNCTION unary_minus_timedelta

   PURE ELEMENTAL LOGICAL FUNCTION eq(td0, td1)

      !! `timedelta` object comparison operator. Returns `.true.` if `td0`
      !! is equal to `td1` and `.false.` otherwise. Overloads the operator
      !! `==`.

      CLASS(timedelta), INTENT(in) :: td0 !! lhs `timedelta` instance
      TYPE(timedelta), INTENT(in) :: td1 !! rhs `timedelta` instance

      eq = td0%total_seconds() == td1%total_seconds()

   END FUNCTION eq

   PURE ELEMENTAL LOGICAL FUNCTION neq(td0, td1)

      !! `timedelta` object comparison operator. Returns `.true.` if `td0`
      !! is not equal to `td1` and `.false.` otherwise. Overloads the
      !! operator `/=`.

      CLASS(timedelta), INTENT(in) :: td0 !! lhs `timedelta` instance
      TYPE(timedelta), INTENT(in) :: td1 !! rhs `timedelta` instance

      neq = .NOT. (td0%total_seconds() == td1%total_seconds())

   END FUNCTION neq

   PURE ELEMENTAL LOGICAL FUNCTION gt(td0, td1)

      !! `timedelta` object comparison operator. Returns `.true.` if
      !! `td0` is greater than `td1` and `.false.` otherwise. Overloads the
      !! operator `>`.

      CLASS(timedelta), INTENT(in) :: td0 !! lhs `timedelta` instance
      TYPE(timedelta), INTENT(in) :: td1 !! rhs `timedelta` instance

      gt = td0%total_seconds() > td1%total_seconds()

   END FUNCTION gt

   PURE ELEMENTAL LOGICAL FUNCTION ge(td0, td1)

      !! `timedelta` object comparison operator. Returns `.true.` if `td0`
      !! is greater than or equal to `td1` and `.false.` otherwise.
      !! Overloads the operator >=.

      CLASS(timedelta), INTENT(in) :: td0 !! lhs `timedelta` instance
      TYPE(timedelta), INTENT(in) :: td1 !! rhs `timedelta` instance

      ge = td0%total_seconds() >= td1%total_seconds()

   END FUNCTION ge

   PURE ELEMENTAL LOGICAL FUNCTION lt(td0, td1)

      !! `timedelta` object comparison operator. Returns `.true.` if `td0`
      !! is less than `td1` and `.false.` otherwise. Overloads the operator
      !! `<`.

      CLASS(timedelta), INTENT(in) :: td0 !! lhs `timedelta` instance
      TYPE(timedelta), INTENT(in) :: td1 !! rhs `timedelta` instance

      lt = td0%total_seconds() < td1%total_seconds()

   END FUNCTION lt

   PURE ELEMENTAL LOGICAL FUNCTION le(td0, td1)

      !! `timedelta` object comparison operator. Returns `.true.` if `td0`
      !! is less than or equal to `td1` and `.false.` otherwise. Overloads
      !! the operator `<=`.

      CLASS(timedelta), INTENT(in) :: td0 !! lhs `timedelta` instance
      TYPE(timedelta), INTENT(in) :: td1 !! rhs `timedelta` instance

      le = td0%total_seconds() <= td1%total_seconds()

   END FUNCTION le
!=======================================================================
END MODULE mod_timedelta

!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD 3-clause license. See LICENSE for details.
!
MODULE mod_datetime
!=======================================================================
!
! mod_datetime: Module that provides the datetime class and its
!               type-bound methods and operators. At the time being,
!               this module also includes some procedures not
!               associated with datetime.
!
!=======================================================================

   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_NULL_CHAR
   USE mod_timedelta, ONLY: timedelta
   USE mod_strftime, ONLY: tm_struct, c_strftime, c_strptime
   USE mod_constants

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: datetime
   PUBLIC :: date2num
   PUBLIC :: datetimeRange
   PUBLIC :: daysInMonth
   PUBLIC :: daysInYear
   PUBLIC :: isLeapYear
   PUBLIC :: num2date
   PUBLIC :: strptime
   PUBLIC :: tm2date

   TYPE :: datetime

      !! Main datetime class for date and time representation.

      PRIVATE

      INTEGER :: year = 1 !! year [1-HUGE(year)]
      INTEGER :: month = 1 !! month in year [1-12]
      INTEGER :: day = 1 !! day in month [1-31]
      INTEGER :: hour = 0 !! hour in day [0-23]
      INTEGER :: minute = 0 !! minute in hour [0-59]
      INTEGER :: second = 0 !! second in minute [0-59]
      INTEGER :: millisecond = 0 !! milliseconds in second [0-999]

      REAL(kind=REAL64) :: tz = 0 !! timezone offset from UTC [hours]

   CONTAINS

      ! getter functions
      PROCEDURE, PASS(self), PUBLIC :: getYear
      PROCEDURE, PASS(self), PUBLIC :: getMonth
      PROCEDURE, PASS(self), PUBLIC :: getDay
      PROCEDURE, PASS(self), PUBLIC :: getHour
      PROCEDURE, PASS(self), PUBLIC :: getMinute
      PROCEDURE, PASS(self), PUBLIC :: getSecond
      PROCEDURE, PASS(self), PUBLIC :: getMillisecond
      PROCEDURE, PASS(self), PUBLIC :: getTz

      ! public methods
      PROCEDURE, PASS(self), PUBLIC :: isocalendar
      PROCEDURE, PASS(self), PUBLIC :: isoformat
      PROCEDURE, PASS(self), PUBLIC :: isValid
      PROCEDURE, NOPASS, PUBLIC :: now
      PROCEDURE, PASS(self), PUBLIC :: secondsSinceEpoch
      PROCEDURE, PASS(self), PUBLIC :: strftime
      PROCEDURE, PASS(self), PUBLIC :: tm
      PROCEDURE, PASS(self), PUBLIC :: tzOffset
      PROCEDURE, PASS(self), PUBLIC :: utc
      PROCEDURE, PASS(self), PUBLIC :: weekday
      PROCEDURE, PASS(self), PUBLIC :: isoweekday
      PROCEDURE, PASS(self), PUBLIC :: weekdayLong
      PROCEDURE, PASS(self), PUBLIC :: isoweekdayLong
      PROCEDURE, PASS(self), PUBLIC :: weekdayShort
      PROCEDURE, PASS(self), PUBLIC :: isoweekdayShort
      PROCEDURE, PASS(self), PUBLIC :: yearday

      ! private methods
      PROCEDURE, PASS(self), PRIVATE :: addMilliseconds
      PROCEDURE, PASS(self), PRIVATE :: addSeconds
      PROCEDURE, PASS(self), PRIVATE :: addMinutes
      PROCEDURE, PASS(self), PRIVATE :: addHours
      PROCEDURE, PASS(self), PRIVATE :: addDays

      ! operator overloading procedures
      PROCEDURE, PASS(d0), PRIVATE :: datetime_plus_timedelta
      PROCEDURE, PASS(d0), PRIVATE :: timedelta_plus_datetime
      PROCEDURE, PASS(d0), PRIVATE :: datetime_minus_datetime
      PROCEDURE, PASS(d0), PRIVATE :: datetime_minus_timedelta
      PROCEDURE, PASS(d0), PRIVATE :: eq
      PROCEDURE, PASS(d0), PRIVATE :: neq
      PROCEDURE, PASS(d0), PRIVATE :: gt
      PROCEDURE, PASS(d0), PRIVATE :: ge
      PROCEDURE, PASS(d0), PRIVATE :: lt
      PROCEDURE, PASS(d0), PRIVATE :: le

      GENERIC :: OPERATOR(+) => datetime_plus_timedelta, &
         timedelta_plus_datetime
      GENERIC :: OPERATOR(-) => datetime_minus_datetime, &
         datetime_minus_timedelta
      GENERIC :: OPERATOR(==) => eq
      GENERIC :: OPERATOR(/=) => neq
      GENERIC :: OPERATOR(>) => gt
      GENERIC :: OPERATOR(>=) => ge
      GENERIC :: OPERATOR(<) => lt
      GENERIC :: OPERATOR(<=) => le

   END TYPE datetime

   INTERFACE datetime
      MODULE PROCEDURE :: datetime_constructor
   END INTERFACE datetime

!=======================================================================
CONTAINS

   PURE ELEMENTAL TYPE(datetime) FUNCTION datetime_constructor(year, month, &
                                                               day, hour, minute, second, millisecond, tz)

      !! Constructor function for the `datetime` class.

      INTEGER, INTENT(in), OPTIONAL :: year !! year
      INTEGER, INTENT(in), OPTIONAL :: month !! month
      INTEGER, INTENT(in), OPTIONAL :: day !! day
      INTEGER, INTENT(in), OPTIONAL :: hour !! hour
      INTEGER, INTENT(in), OPTIONAL :: minute !! minute
      INTEGER, INTENT(in), OPTIONAL :: second !! second
      INTEGER, INTENT(in), OPTIONAL :: millisecond !! millisecond
      REAL(kind=REAL64), INTENT(in), OPTIONAL :: tz !! timezone offset in hours

      IF (PRESENT(year)) THEN
         datetime_constructor%year = year
      ELSE
         datetime_constructor%year = 1
      END IF

      IF (PRESENT(month)) THEN
         datetime_constructor%month = month
      ELSE
         datetime_constructor%month = 1
      END IF

      IF (PRESENT(day)) THEN
         datetime_constructor%day = day
      ELSE
         datetime_constructor%day = 1
      END IF

      IF (PRESENT(hour)) THEN
         datetime_constructor%hour = hour
      ELSE
         datetime_constructor%hour = 0
      END IF

      IF (PRESENT(minute)) THEN
         datetime_constructor%minute = minute
      ELSE
         datetime_constructor%minute = 0
      END IF

      IF (PRESENT(second)) THEN
         datetime_constructor%second = second
      ELSE
         datetime_constructor%second = 0
      END IF

      IF (PRESENT(millisecond)) THEN
         datetime_constructor%millisecond = millisecond
      ELSE
         datetime_constructor%millisecond = 0
      END IF

      IF (PRESENT(tz)) THEN
         datetime_constructor%tz = tz
      ELSE
         datetime_constructor%tz = 0
      END IF

   END FUNCTION datetime_constructor

! datetime getters
!=======================================================================

   PURE ELEMENTAL INTEGER FUNCTION getYear(self)
      !! Returns the year component
      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      getYear = self%year
   END FUNCTION getYear

   PURE ELEMENTAL INTEGER FUNCTION getMonth(self)
      !! Returns the year component
      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      getMonth = self%month
   END FUNCTION getMonth

   PURE ELEMENTAL INTEGER FUNCTION getDay(self)
      !! Returns the year component
      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      getDay = self%day
   END FUNCTION getDay

   PURE ELEMENTAL INTEGER FUNCTION getHour(self)
      !! Returns the year component
      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      getHour = self%hour
   END FUNCTION getHour

   PURE ELEMENTAL INTEGER FUNCTION getMinute(self)
      !! Returns the year component
      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      getMinute = self%minute
   END FUNCTION getMinute

   PURE ELEMENTAL INTEGER FUNCTION getSecond(self)
      !! Returns the year component
      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      getSecond = self%second
   END FUNCTION getSecond

   PURE ELEMENTAL INTEGER FUNCTION getMillisecond(self)
      !! Returns the year component
      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      getMillisecond = self%millisecond
   END FUNCTION getMillisecond

   PURE ELEMENTAL REAL(kind=REAL64) FUNCTION getTz(self)
      !! Returns the timezone offset component
      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      getTz = self%tz
   END FUNCTION getTz

   PURE ELEMENTAL SUBROUTINE addMilliseconds(self, ms)

      !! Adds an integer number of milliseconds to self. Called by `datetime`
      !! addition (`+`) and subtraction (`-`) operators.

      CLASS(datetime), INTENT(inout) :: self !! `datetime` instance
      INTEGER, INTENT(in) :: ms !! number of milliseconds to add

      self%millisecond = self%millisecond + ms

      DO
         IF (self%millisecond >= 1000) THEN
            CALL self%addSeconds(self%millisecond/1000)
            self%millisecond = MOD(self%millisecond, 1000)
         ELSEIF (self%millisecond < 0) THEN
            CALL self%addSeconds(self%millisecond/1000 - 1)
            self%millisecond = MOD(self%millisecond, 1000) + 1000
         ELSE
            EXIT
         END IF
      END DO

   END SUBROUTINE addMilliseconds

! datetime-bound methods
!=======================================================================

   PURE ELEMENTAL SUBROUTINE addSeconds(self, s)

      !! Adds an integer number of seconds to self. Called by `datetime`
      !! addition (`+`) and subtraction (`-`) operators.

      CLASS(datetime), INTENT(inout) :: self !! `datetime` instance
      INTEGER, INTENT(in) :: s !! number of seconds to add

      self%second = self%second + s

      DO
         IF (self%second >= 60) THEN
            CALL self%addMinutes(self%second/60)
            self%second = MOD(self%second, 60)
         ELSEIF (self%second < 0) THEN
            CALL self%addMinutes(self%second/60 - 1)
            self%second = MOD(self%second, 60) + 60
         ELSE
            EXIT
         END IF
      END DO

   END SUBROUTINE addSeconds

   PURE ELEMENTAL SUBROUTINE addMinutes(self, m)

      !! Adds an integer number of minutes to self. Called by `datetime`
      !! addition (`+`) and subtraction (`-`) operators.

      CLASS(datetime), INTENT(inout) :: self !! `datetime` instance
      INTEGER, INTENT(in) :: m !! number of minutes to add

      self%minute = self%minute + m

      DO
         IF (self%minute >= 60) THEN
            CALL self%addHours(self%minute/60)
            self%minute = MOD(self%minute, 60)
         ELSEIF (self%minute < 0) THEN
            CALL self%addHours(self%minute/60 - 1)
            self%minute = MOD(self%minute, 60) + 60
         ELSE
            EXIT
         END IF
      END DO

   END SUBROUTINE addMinutes

   PURE ELEMENTAL SUBROUTINE addHours(self, h)

      !! Adds an integer number of hours to self. Called by `datetime`
      !! addition (`+`) and subtraction (`-`) operators.

      CLASS(datetime), INTENT(inout) :: self !! `datetime` instance
      INTEGER, INTENT(in) :: h !! number of hours to add

      self%hour = self%hour + h

      DO
         IF (self%hour >= 24) THEN
            CALL self%addDays(self%hour/24)
            self%hour = MOD(self%hour, 24)
         ELSEIF (self%hour < 0) THEN
            CALL self%addDays(self%hour/24 - 1)
            self%hour = MOD(self%hour, 24) + 24
         ELSE
            EXIT
         END IF
      END DO

   END SUBROUTINE addHours

   PURE ELEMENTAL SUBROUTINE addDays(self, d)

      !! Adds an integer number of dayss to self. Called by `datetime`
      !! addition (`+`) and subtraction (`-`) operators.

      CLASS(datetime), INTENT(inout) :: self !! `datetime` instance
      INTEGER, INTENT(in) :: d !! number of days to add

      INTEGER :: daysInCurrentMonth

      self%day = self%day + d
      DO
         daysInCurrentMonth = daysInMonth(self%month, self%year)
         IF (self%day > daysInCurrentMonth) THEN
            self%day = self%day - daysInCurrentMonth
            self%month = self%month + 1
            IF (self%month > 12) THEN
               self%year = self%year + self%month/12
               self%month = MOD(self%month, 12)
            END IF
         ELSEIF (self%day < 1) THEN
            self%month = self%month - 1
            IF (self%month < 1) THEN
               self%year = self%year + self%month/12 - 1
               self%month = 12 + MOD(self%month, 12)
            END IF
            self%day = self%day + daysInMonth(self%month, self%year)
         ELSE
            EXIT
         END IF
      END DO

   END SUBROUTINE addDays

   PURE ELEMENTAL CHARACTER(len=23) FUNCTION isoformat(self, sep)

      !! Returns character string with time in ISO 8601 format.

      CLASS(datetime), INTENT(in) :: self !! `datetime instance`
      CHARACTER(len=1), INTENT(in), OPTIONAL :: sep
      !! separator character, 'T' is default

      CHARACTER(len=1) :: separator

      IF (PRESENT(sep)) THEN
         separator = sep
      ELSE
         separator = 'T'
      END IF

      ! TODO below is a bit cumbersome and was implemented
      ! at a time before the interface to strftime. Now we
      ! could do something like:
      !
      ! isoformat = self % strftime('%Y-%m-%d'//separator//'%H:%M:%S')
      !
      isoformat = int2str(self%year, 4)//'-'// &
                  int2str(self%month, 2)//'-'// &
                  int2str(self%day, 2)//separator// &
                  int2str(self%hour, 2)//':'// &
                  int2str(self%minute, 2)//':'// &
                  int2str(self%second, 2)//'.'// &
                  int2str(self%millisecond, 3)

   END FUNCTION isoformat

   PURE ELEMENTAL LOGICAL FUNCTION isValid(self)

      !! Checks whether the `datetime` instance has valid component values.
      !! Returns `.true.` if the `datetime` instance is valid, and `.false.`
      !! otherwise.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      ! assume valid
      isValid = .TRUE.

      IF (self%year < 1) THEN
         isValid = .FALSE.
         RETURN
      END IF

      IF (self%month < 1 .OR. self%month > 12) THEN
         isValid = .FALSE.
         RETURN
      END IF

      IF (self%day < 1 .OR. &
          self%day > daysInMonth(self%month, self%year)) THEN
         isValid = .FALSE.
         RETURN
      END IF

      IF (self%hour < 0 .OR. self%hour > 23) THEN
         isValid = .FALSE.
         RETURN
      END IF

      IF (self%minute < 0 .OR. self%minute > 59) THEN
         isValid = .FALSE.
         RETURN
      END IF

      IF (self%second < 0 .OR. self%second > 59) THEN
         isValid = .FALSE.
         RETURN
      END IF

      IF (self%millisecond < 0 .OR. self%millisecond > 999) THEN
         isValid = .FALSE.
         RETURN
      END IF

   END FUNCTION isValid

   TYPE(datetime) FUNCTION now()

      !! Returns a `datetime` instance with current time.
      !! No input arguments.

      CHARACTER(len=5) :: zone
      INTEGER, DIMENSION(8) :: values

      INTEGER :: hour, minute

      ! Obtain local machine time zone information
      CALL DATE_AND_TIME(zone=zone, values=values)

      READ (unit=zone(1:3), fmt='(I3)') hour
      READ (unit=zone(4:5), fmt='(I2)') minute

      now = datetime(year=values(1), &
                     month=values(2), &
                     day=values(3), &
                     hour=values(5), &
                     minute=values(6), &
                     second=values(7), &
                     millisecond=values(8))

      now%tz = hour + minute*m2h

   END FUNCTION now

   PURE ELEMENTAL INTEGER FUNCTION weekday(self)

      !! Returns the day of the week calculated using Zeller's congruence.
      !! Returned value is an integer scalar in the range [0-6], such that:
      !!
      !! 0: Sunday
      !! 1: Monday
      !! 2: Tuesday
      !! 3: Wednesday
      !! 4: Thursday
      !! 5: Friday
      !! 6: Saturday

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      INTEGER :: year, month
      INTEGER :: j, k

      year = self%year
      month = self%month

      IF (month <= 2) THEN
         month = month + 12
         year = year - 1
      END IF

      j = year/100
      k = MOD(year, 100)

      weekday = MOD(self%day + ((month + 1)*26)/10 + k + k/4 + j/4 + 5*j, 7) - 1

      IF (weekday < 0) weekday = 6

   END FUNCTION weekday

   PURE ELEMENTAL INTEGER FUNCTION isoweekday(self)

      !! Returns the day of the week per ISO 8601 returned from weekday().
      !! Returned value is an integer scalar in the range [1-7], such that:
      !!
      !! 1: Monday
      !! 2: Tuesday
      !! 3: Wednesday
      !! 4: Thursday
      !! 5: Friday
      !! 6: Saturday
      !! 7: Sunday

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      isoweekday = self%weekday()

      IF (isoweekday == 0) THEN
         isoweekday = 7
      END IF

   END FUNCTION isoweekday

   PURE ELEMENTAL CHARACTER(len=9) FUNCTION weekdayLong(self)

      !! Returns the full name of the day of the week.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      CHARACTER(len=9), PARAMETER, DIMENSION(7) :: &
         days = ['Sunday   ', 'Monday   ', 'Tuesday  ', 'Wednesday', &
                 'Thursday ', 'Friday   ', 'Saturday ']

      weekdayLong = days(self%weekday() + 1)

   END FUNCTION weekdayLong

   PURE ELEMENTAL CHARACTER(len=9) FUNCTION isoweekdayLong(self)

      !! Returns the full name of the day of the week for ISO 8601
      !! ordered weekdays.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      CHARACTER(len=9), PARAMETER, DIMENSION(7) :: &
         days = ['Monday   ', 'Tuesday  ', 'Wednesday', 'Thursday ', &
                 'Friday   ', 'Saturday ', 'Sunday   ']

      isoweekdayLong = days(self%isoweekday())

   END FUNCTION isoweekdayLong

   PURE ELEMENTAL CHARACTER(len=3) FUNCTION weekdayShort(self)

      !! Returns the short (3-letter) name of the day of the week.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      CHARACTER(len=3), PARAMETER, DIMENSION(7) :: &
         days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']

      weekdayShort = days(self%weekday() + 1)

   END FUNCTION weekdayShort

   PURE ELEMENTAL CHARACTER(len=3) FUNCTION isoweekdayShort(self)

      !! Returns the short (3-letter) name of the day of the week
      !! based on ISO 8601 ordering.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      CHARACTER(len=3), PARAMETER, DIMENSION(7) :: &
         days = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']

      isoweekdayShort = days(self%isoweekday())

   END FUNCTION isoweekdayShort

   FUNCTION isocalendar(self)

      !! Returns an array of 3 integers, year, week number, and week day,
      !! as defined by ISO 8601 week date. Essentially a wrapper around C
      !! `strftime` function.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      INTEGER, DIMENSION(3) :: isocalendar
      INTEGER :: year, week, wday
      INTEGER :: rc
      CHARACTER(len=20) :: string

      rc = c_strftime(string, LEN(string), '%G %V %u'//C_NULL_CHAR, &
                      self%tm())

      READ (unit=string(1:4), fmt='(I4)') year
      READ (unit=string(6:7), fmt='(I2)') week
      READ (unit=string(9:9), fmt='(I1)') wday

      isocalendar = [year, week, wday]

   END FUNCTION isocalendar

   INTEGER FUNCTION secondsSinceEpoch(self)

      !! Returns an integer number of seconds since the UNIX Epoch,
      !! `1970-01-01 00:00:00`. Note that this is a wrapper around C's
      !! `strftime('%s')`, so the number of seconds will reflect the time
      !! zone of the local machine on which the function is being called.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      CHARACTER(len=11) :: string

      string = self%strftime('%s')
      READ (unit=string, fmt='(I10)') secondsSinceEpoch

   END FUNCTION secondsSinceEpoch

   FUNCTION strftime(self, FORMAT)

      !! Wrapper around C/C++ `strftime` function.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance
      CHARACTER(len=*), INTENT(in) :: FORMAT !! format string

      CHARACTER(len=:), ALLOCATABLE :: strftime

      INTEGER :: n, rc
      CHARACTER(len=MAXSTRLEN) :: resultString

      resultString = ""
      rc = c_strftime(resultString, MAXSTRLEN, TRIM(FORMAT)//C_NULL_CHAR, &
                      self%tm())
      strftime = TRIM(resultString)
      n = LEN(strftime)
      strftime = strftime(1:n - 1)

   END FUNCTION strftime

   PURE ELEMENTAL TYPE(tm_struct) FUNCTION tm(self)

      !! Returns a `tm_struct` instance of the current `datetime`.

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      tm%tm_sec = self%second
      tm%tm_min = self%minute
      tm%tm_hour = self%hour
      tm%tm_mday = self%day
      tm%tm_mon = self%month - 1
      tm%tm_year = self%year - 1900
      tm%tm_wday = self%weekday()
      tm%tm_yday = self%yearday() - 1
      tm%tm_isdst = -1

   END FUNCTION tm

   PURE ELEMENTAL CHARACTER(len=5) FUNCTION tzOffset(self)

      !! Returns a character string with timezone offset in hours from UTC,
      !! in format +/-[hh][mm].

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      INTEGER :: hours, minutes

      IF (self%tz < 0) THEN
         tzOffset(1:1) = '-'
      ELSE
         tzOffset(1:1) = '+'
      END IF

      hours = INT(ABS(self%tz))
      minutes = NINT((ABS(self%tz) - hours)*60)

      IF (minutes == 60) THEN
         minutes = 0
         hours = hours + 1
      END IF

      WRITE (unit=tzOffset(2:5), fmt='(2I2.2)') hours, minutes

   END FUNCTION tzOffset

   PURE ELEMENTAL TYPE(datetime) FUNCTION utc(self)

      !! Returns the `datetime` instance at Coordinated Universal Time (UTC).

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      INTEGER :: hours, minutes, sgn

      hours = INT(ABS(self%tz))
      minutes = NINT((ABS(self%tz) - hours)*60)
      sgn = INT(SIGN(one, self%tz))

      utc = self - timedelta(hours=sgn*hours, minutes=sgn*minutes)
      utc%tz = 0

   END FUNCTION utc

   PURE ELEMENTAL INTEGER FUNCTION yearday(self)

      !! Returns the integer day of the year (ordinal date).

      CLASS(datetime), INTENT(in) :: self !! `datetime` instance

      INTEGER :: month

      yearday = 0
      DO month = 1, self%month - 1
         yearday = yearday + daysInMonth(month, self%year)
      END DO
      yearday = yearday + self%day

   END FUNCTION yearday

! datetime operators
!=======================================================================

   PURE ELEMENTAL FUNCTION datetime_plus_timedelta(d0, t) RESULT(d)

      !! Adds a `timedelta` instance to a `datetime` instance, and returns a
      !! new `datetime` instance. Overloads the operator `+`.

      CLASS(datetime), INTENT(in) :: d0 !! `datetime` instance
      CLASS(timedelta), INTENT(in) :: t !! `timedelta` instance
      TYPE(datetime) :: d

      INTEGER :: milliseconds, seconds, minutes, hours, days

      d = datetime(year=d0%getYear(), &
                   month=d0%getMonth(), &
                   day=d0%getDay(), &
                   hour=d0%getHour(), &
                   minute=d0%getMinute(), &
                   second=d0%getSecond(), &
                   millisecond=d0%getMillisecond(), &
                   tz=d0%getTz())

      milliseconds = t%getMilliseconds()
      seconds = t%getSeconds()
      minutes = t%getMinutes()
      hours = t%getHours()
      days = t%getDays()

      IF (milliseconds /= 0) CALL d%addMilliseconds(milliseconds)
      IF (seconds /= 0) CALL d%addSeconds(seconds)
      IF (minutes /= 0) CALL d%addMinutes(minutes)
      IF (hours /= 0) CALL d%addHours(hours)
      IF (days /= 0) CALL d%addDays(days)

   END FUNCTION datetime_plus_timedelta

   PURE ELEMENTAL FUNCTION timedelta_plus_datetime(t, d0) RESULT(d)

      !! Adds a `timedelta` instance to a `datetime` instance, and returns a
      !! new `datetime` instance. Overloads the operator `+`.

      CLASS(timedelta), INTENT(in) :: t !! `timedelta` instance
      CLASS(datetime), INTENT(in) :: d0 !! `datetime` instance
      TYPE(datetime) :: d

      d = d0 + t

   END FUNCTION timedelta_plus_datetime

   PURE ELEMENTAL FUNCTION datetime_minus_timedelta(d0, t) RESULT(d)

      !! Subtracts a `timedelta` instance from a `datetime` instance and
      !! returns a new `datetime` instance. Overloads the operator `-`.

      CLASS(datetime), INTENT(in) :: d0 !! `datetime` instance
      CLASS(timedelta), INTENT(in) :: t !! `timedelta` instance
      TYPE(datetime) :: d

      d = d0 + (-t)

   END FUNCTION datetime_minus_timedelta

   PURE ELEMENTAL FUNCTION datetime_minus_datetime(d0, d1) RESULT(t)

      !! Subtracts a `datetime` instance from another `datetime` instance,
      !! and returns a `timedelta` instance. Overloads the operator `-`.

      CLASS(datetime), INTENT(in) :: d0 !! lhs `datetime` instance
      CLASS(datetime), INTENT(in) :: d1 !! rhs `datetime` instance
      TYPE(timedelta) :: t

      REAL(kind=REAL64) :: daysDiff
      INTEGER :: days, hours, minutes, seconds, milliseconds
      INTEGER :: sign_

      daysDiff = date2num(d0) - date2num(d1)

      IF (daysDiff < 0) THEN
         sign_ = -1
         daysDiff = ABS(daysDiff)
      ELSE
         sign_ = 1
      END IF

      days = INT(daysDiff)
      hours = INT((daysDiff - days)*d2h)
      minutes = INT((daysDiff - days - hours*h2d)*d2m)
      seconds = INT((daysDiff - days - hours*h2d - minutes*m2d)*d2s)
      milliseconds = NINT((daysDiff - days - hours*h2d - minutes*m2d &
                           - seconds*s2d)*d2s*1E3_REAL64)

      t = timedelta(sign_*days, sign_*hours, sign_*minutes, sign_*seconds, &
                    sign_*milliseconds)

   END FUNCTION datetime_minus_datetime

   PURE ELEMENTAL LOGICAL FUNCTION gt(d0, d1)

      !! `datetime` comparison operator that eturns `.true.` if `d0` is
      !! greater than `d1` and `.false.` otherwise. Overloads the
      !! operator `>`.

      CLASS(datetime), INTENT(in) :: d0 !! lhs `datetime` instance
      CLASS(datetime), INTENT(in) :: d1 !! rhs `datetime` instance

      TYPE(datetime) :: d0_utc, d1_utc

      ! Convert to UTC before making comparison
      d0_utc = d0%utc()
      d1_utc = d1%utc()

      ! Year comparison block
      IF (d0_utc%year > d1_utc%year) THEN
         gt = .TRUE.
      ELSEIF (d0_utc%year < d1_utc%year) THEN
         gt = .FALSE.
      ELSE

         ! Month comparison block
         IF (d0_utc%month > d1_utc%month) THEN
            gt = .TRUE.
         ELSEIF (d0_utc%month < d1_utc%month) THEN
            gt = .FALSE.
         ELSE

            ! Day comparison block
            IF (d0_utc%day > d1_utc%day) THEN
               gt = .TRUE.
            ELSEIF (d0_utc%day < d1_utc%day) THEN
               gt = .FALSE.
            ELSE

               ! Hour comparison block
               IF (d0_utc%hour > d1_utc%hour) THEN
                  gt = .TRUE.
               ELSEIF (d0_utc%hour < d1_utc%hour) THEN
                  gt = .FALSE.
               ELSE

                  ! Minute comparison block
                  IF (d0_utc%minute > d1_utc%minute) THEN
                     gt = .TRUE.
                  ELSEIF (d0_utc%minute < d1_utc%minute) THEN
                     gt = .FALSE.
                  ELSE

                     ! Second comparison block
                     IF (d0_utc%second > d1_utc%second) THEN
                        gt = .TRUE.
                     ELSEIF (d0_utc%second < d1_utc%second) THEN
                        gt = .FALSE.
                     ELSE

                        ! Millisecond comparison block
                        IF (d0_utc%millisecond > d1_utc%millisecond) THEN
                           gt = .TRUE.
                        ELSE
                           gt = .FALSE.
                        END IF

                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

   END FUNCTION gt

   PURE ELEMENTAL LOGICAL FUNCTION lt(d0, d1)

      !! `datetime` comparison operator that returns `.true.` if `d0` is
      !! less than `d1` and `.false.` otherwise. Overloads the operator `<`.

      CLASS(datetime), INTENT(in) :: d0 !! lhs `datetime` instance
      CLASS(datetime), INTENT(in) :: d1 !! rhs `datetime` instance

      lt = d1 > d0

   END FUNCTION lt

   PURE ELEMENTAL LOGICAL FUNCTION eq(d0, d1)

      !! `datetime` comparison operator that returns `.true.` if `d0` is
      !! equal to `d1` and `.false.` otherwise. Overloads the operator `==`.

      CLASS(datetime), INTENT(in) :: d0 !! lhs `datetime` instance
      CLASS(datetime), INTENT(in) :: d1 !! rhs `datetime` instance

      TYPE(datetime) :: d0_utc, d1_utc

      ! Convert to UTC before making comparison
      d0_utc = d0%utc()
      d1_utc = d1%utc()

      eq = d0_utc%year == d1_utc%year .AND. &
           d0_utc%month == d1_utc%month .AND. &
           d0_utc%day == d1_utc%day .AND. &
           d0_utc%hour == d1_utc%hour .AND. &
           d0_utc%minute == d1_utc%minute .AND. &
           d0_utc%second == d1_utc%second .AND. &
           d0_utc%millisecond == d1_utc%millisecond

   END FUNCTION eq

   PURE ELEMENTAL LOGICAL FUNCTION neq(d0, d1)

      !! `datetime` comparison operator that eturns `.true.` if `d0` is
      !! not equal to `d1` and `.false.` otherwise. Overloads the operator `/=`.

      CLASS(datetime), INTENT(in) :: d0 !! lhs `datetime` instance
      CLASS(datetime), INTENT(in) :: d1 !! rhs `datetime` instance

      neq = .NOT. d0 == d1

   END FUNCTION neq

   PURE ELEMENTAL LOGICAL FUNCTION ge(d0, d1)

      !! `datetime` comparison operator. Returns `.true.` if `d0` is greater
      !! than or equal to `d1` and `.false.` otherwise. Overloads the
      !! operator `>=`.

      CLASS(datetime), INTENT(in) :: d0 !! lhs `datetime` instance
      CLASS(datetime), INTENT(in) :: d1 !! rhs `datetime` instance

      ge = d0 > d1 .OR. d0 == d1

   END FUNCTION ge

   PURE ELEMENTAL LOGICAL FUNCTION le(d0, d1)

      !! `datetime` comparison operator. Returns `.true.` if `d0` is less
      !! than or equal to `d1`, and `.false.` otherwise. Overloads the
      !! operator `<=`.

      CLASS(datetime), INTENT(in) :: d0 !! lhs `datetime` instance
      CLASS(datetime), INTENT(in) :: d1 !! rhs `datetime` instance

      le = d1 > d0 .OR. d0 == d1

   END FUNCTION le

! public procedures
!=======================================================================

   PURE ELEMENTAL LOGICAL FUNCTION isLeapYear(year)

      !! Returns `.true.` if year is leap year and `.false.` otherwise.

      INTEGER, INTENT(in) :: year !! year

      isLeapYear = (MOD(year, 4) == 0 .AND. .NOT. MOD(year, 100) == 0) &
                   .OR. (MOD(year, 400) == 0)

   END FUNCTION isLeapYear

   PURE FUNCTION datetimeRange(d0, d1, t)

      !! Given start and end `datetime` instances `d0` and `d1` and time
      !! increment as `timedelta` instance `t`, returns an array of
      !! `datetime` instances. The number of elements is the number of whole
      !! time increments contained between datetimes `d0` and `d1`.

      TYPE(datetime), INTENT(in) :: d0 !! start time
      TYPE(datetime), INTENT(in) :: d1 !! end time
      TYPE(timedelta), INTENT(in) :: t !! time increment

      REAL(kind=REAL64) :: datenum0, datenum1, increment
      REAL(kind=REAL64) :: eps

      TYPE(datetime), DIMENSION(:), ALLOCATABLE :: datetimeRange

      INTEGER :: n, nm

      eps = 1E-10_REAL64

      datenum0 = date2num(d0)
      datenum1 = date2num(d1)

      increment = t%total_seconds()*s2d

      nm = FLOOR((datenum1 - datenum0 + eps)/increment) + 1

      ALLOCATE (datetimeRange(nm))

      DO n = 1, nm
         datetimeRange(n) = num2date(datenum0 + (n - 1)*increment)
      END DO

   END FUNCTION datetimeRange

   PURE ELEMENTAL INTEGER FUNCTION daysInMonth(month, year)

      !! Given integer month and year, returns an integer number
      !! of days in that particular month.

      INTEGER, INTENT(in) :: month !! month
      INTEGER, INTENT(in) :: year !! year

      INTEGER, PARAMETER, DIMENSION(12) :: &
         days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      IF (month < 1 .OR. month > 12) THEN
         ! Should raise an error and abort here, however we want to keep
         ! the pure and elemental attributes. Make sure this function is
         ! called with the month argument in range.
         daysInMonth = 0
         RETURN
      END IF

      IF (month == 2 .AND. isLeapYear(year)) THEN
         daysInMonth = 29
      ELSE
         daysInMonth = days(month)
      END IF

   END FUNCTION daysInMonth

   PURE ELEMENTAL INTEGER FUNCTION daysInYear(year)

      !! Returns the number of days in year.

      INTEGER, INTENT(in) :: year !! year

      IF (isLeapYear(year)) THEN
         daysInYear = 366
      ELSE
         daysInYear = 365
      END IF

   END FUNCTION daysInYear

   PURE ELEMENTAL REAL(kind=REAL64) FUNCTION date2num(d)

      !! Given a datetime instance d, returns number of days since
      !! `0001-01-01 00:00:00`, taking into account the timezone offset.

      TYPE(datetime), INTENT(in) :: d !! `datetime` instance

      TYPE(datetime) :: d_utc
      INTEGER :: year

      ! Convert to UTC first
      d_utc = d%utc()

      ! d_utc % year must be positive:
      IF (d_utc%year < 1) THEN
         date2num = 0
         RETURN
      END IF

      date2num = 0
      DO year = 1, d_utc%year - 1
         date2num = date2num + daysInYear(year)
      END DO

      date2num = date2num &
                 + d_utc%yearday() &
                 + d_utc%hour*h2d &
                 + d_utc%minute*m2d &
                 + (d_utc%second + 1E-3_REAL64*d_utc%millisecond)*s2d

   END FUNCTION date2num

   PURE ELEMENTAL TYPE(datetime) FUNCTION num2date(num)

      !! Given number of days since `0001-01-01 00:00:00`, returns a
      !! correspoding `datetime` instance.

      REAL(kind=REAL64), INTENT(in) :: num
      !! number of days since `0001-01-01 00:00:00`

      INTEGER :: year, month, day, hour, minute, second, millisecond
      REAL(kind=REAL64) :: days, totseconds

      ! num must be positive:
      IF (num < 0) THEN
         num2date = datetime(1)
         RETURN
      END IF

      days = num

      year = 1
      DO
         IF (INT(days) <= daysInYear(year)) EXIT
         days = days - daysInYear(year)
         year = year + 1
      END DO

      month = 1
      DO
         IF (INT(days) <= daysInMonth(month, year)) EXIT
         days = days - daysInMonth(month, year)
         month = month + 1
      END DO

      day = INT(days)
      totseconds = (days - day)*d2s
      hour = INT(totseconds*s2h)
      minute = INT((totseconds - hour*h2s)*s2m)
      second = INT(totseconds - hour*h2s - minute*m2s)
      millisecond = NINT((totseconds - INT(totseconds))*1E3_REAL64)

      num2date = datetime(year, month, day, hour, minute, second, millisecond, tz=zero)

      ! Handle a special case caused by floating-point arithmethic:
      IF (num2date%millisecond == 1000) THEN
         num2date%millisecond = 0
         CALL num2date%addSeconds(1)
      END IF

      IF (num2date%second == 60) THEN
         num2date%second = 0
         CALL num2date%addMinutes(1)
      END IF
      IF (num2date%minute == 60) THEN
         num2date%minute = 0
         CALL num2date%addHours(1)
      END IF
      IF (num2date%hour == 60) THEN
         num2date%hour = 0
         CALL num2date%addDays(1)
      END IF

   END FUNCTION num2date

   TYPE(datetime) FUNCTION strptime(str, FORMAT)

      !! A wrapper function around C/C++ strptime function.
      !! Returns a `datetime` instance.

      CHARACTER(len=*), INTENT(in) :: str !! time string
      CHARACTER(len=*), INTENT(in) :: FORMAT !! time format

      INTEGER :: rc
      TYPE(tm_struct) :: tm

      rc = c_strptime(TRIM(str)//C_NULL_CHAR, TRIM(FORMAT)//C_NULL_CHAR, tm)
      strptime = tm2date(tm)

   END FUNCTION strptime

   PURE ELEMENTAL TYPE(datetime) FUNCTION tm2date(ctime)

      !! Given a `tm_struct` instance, returns a corresponding `datetime`
      !! instance.

      TYPE(tm_struct), INTENT(in) :: ctime !! C-style time struct

      tm2date%millisecond = 0
      tm2date%second = ctime%tm_sec
      tm2date%minute = ctime%tm_min
      tm2date%hour = ctime%tm_hour
      tm2date%day = ctime%tm_mday
      tm2date%month = ctime%tm_mon + 1
      tm2date%year = ctime%tm_year + 1900
      tm2date%tz = 0

   END FUNCTION tm2date

! private procedures
!=======================================================================

   PURE FUNCTION int2str(i, length)

      !! Converts an integer `i` into a character string of requested length,
      !! pre-pending zeros if necessary.

      INTEGER, INTENT(in) :: i !! integer to convert to string
      INTEGER, INTENT(in) :: length !! desired length of string

      CHARACTER(len=length) :: int2str
      CHARACTER(len=2) :: string

      WRITE (unit=string, fmt='(I2)') length
      WRITE (unit=int2str, fmt='(I'//string//'.'//string//')') i

   END FUNCTION int2str
!=======================================================================
END MODULE mod_datetime

!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD 3-clause license. See LICENSE for details.
!
MODULE mod_clock
!=======================================================================
!
! mod_clock
!
!=======================================================================

   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_NULL_CHAR
   USE mod_datetime, ONLY: datetime
   USE mod_timedelta, ONLY: timedelta

   IMPLICIT NONE

   PRIVATE

! Derived types:
   PUBLIC :: clock

   TYPE :: clock

      !! A clock object with a start, stop and current times, tick interval
      !! and tick methods.

      TYPE(datetime) :: startTime
      TYPE(datetime) :: stopTime
      TYPE(datetime) :: currentTime

      TYPE(timedelta) :: tickInterval

      ! May become Alarm class in some future release;
      ! for now, just a switch
      LOGICAL :: alarm = .FALSE.

      ! Clock status flags
      LOGICAL :: started = .FALSE.
      LOGICAL :: stopped = .FALSE.

   CONTAINS

      PROCEDURE :: reset
      PROCEDURE :: tick

   END TYPE clock
!=======================================================================
CONTAINS

!=======================================================================
   PURE ELEMENTAL SUBROUTINE reset(self)

      !! Resets the clock to its start time.

      CLASS(clock), INTENT(inout) :: self

      self%currentTime = self%startTime

      self%started = .FALSE.
      self%stopped = .FALSE.

   END SUBROUTINE reset
!=======================================================================

!=======================================================================
   PURE ELEMENTAL SUBROUTINE tick(self)

      !! Increments the currentTime of the clock instance by one tickInterval.

      CLASS(clock), INTENT(inout) :: self

      IF (self%stopped) THEN
         RETURN
      END IF

      IF (.NOT. self%started) THEN
         self%started = .TRUE.
         self%currentTime = self%startTime
      END IF

      self%currentTime = self%currentTime + self%tickInterval

      IF (self%currentTime >= self%stopTime) THEN
         self%stopped = .TRUE.
      END IF

   END SUBROUTINE tick
!=======================================================================
END MODULE mod_clock

! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
MODULE datetime_module

!! Provides entry point to all items defined in datetime, timedelta,
!! clock and strftime modules.

   USE mod_datetime
   USE mod_timedelta
   USE mod_clock
   USE mod_strftime

END MODULE datetime_module
