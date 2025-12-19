!-------------------------------------------------------------------------
! Profile interpolation utilities
!
! Provides functions for interpolating 24-hour profiles to specific times.
! Used by anthropogenic heat calculations and water distribution.
!
! History:
!   TS 05 Jul 2018: Original implementation
!   TS Dec 2024: Extracted from suews_ctrl_input.f95 as part of dead code cleanup (#1039)
!-------------------------------------------------------------------------
MODULE module_ctrl_input_profile
   IMPLICIT NONE

CONTAINS
!===================================================================================
! get interpolated profile values at specified time
! NO normalisation performed
   FUNCTION get_Prof_SpecTime_inst(Hour, Min, Sec, Prof_24h) RESULT(Prof_CurrTime)

      IMPLICIT NONE

      INTEGER :: i, j !Used to count over hours and sub-hourly timesteps
      INTEGER, INTENT(IN) :: Hour, Min, Sec
      INTEGER :: total_sec, SecPerHour
      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(IN) :: Prof_24h
      REAL(KIND(1D0)) :: deltaProf !Change in hourly profiles per model timestep
      REAL(KIND(1D0)) :: Prof_CurrTime

      total_sec = Min*60 + Sec
      SecPerHour = 3600

      i = hour
      j = i + 1
      IF (j == 24) j = 0

      deltaProf = (Prof_24h(j) - Prof_24h(i))/SecPerHour
      Prof_CurrTime = Prof_24h(hour) + deltaProf*total_sec

   END FUNCTION get_Prof_SpecTime_inst

!===================================================================================
! get interpolated profile values at specified time
! normalise so the AVERAGE of the multipliers is equal to 1
   FUNCTION get_Prof_SpecTime_mean(Hour, Min, Sec, Prof_24h) RESULT(Prof_CurrTime)

      IMPLICIT NONE

      INTEGER :: i, j !Used to count over hours and sub-hourly timesteps
      INTEGER, INTENT(IN) :: Hour, Min, Sec
      INTEGER :: total_sec, SecPerHour
      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(IN) :: Prof_24h
      REAL(KIND(1D0)), DIMENSION(0:23) :: Prof_24h_mean
      REAL(KIND(1D0)) :: deltaProf !Change in hourly profiles per model timestep
      REAL(KIND(1D0)) :: Prof_CurrTime

      total_sec = Min*60 + Sec
      SecPerHour = 3600

      Prof_24h_mean = MERGE(Prof_24h/(SUM(Prof_24h)/SIZE(Prof_24h, dim=1)), 0.D0, SUM(Prof_24h) /= 0) ! prevent zero-division
      ! print*, Prof_24h_mean

      i = hour
      j = i + 1
      IF (j == 24) j = 0

      deltaProf = (Prof_24h_mean(j) - Prof_24h_mean(i))/SecPerHour

      ! print*, deltaProf,total_sec
      Prof_CurrTime = Prof_24h_mean(i) + deltaProf*total_sec

   END FUNCTION get_Prof_SpecTime_mean

!===================================================================================
! get interpolated profile values at specified time
! normalise so the SUM of the multipliers is equal to 1
   FUNCTION get_Prof_SpecTime_sum(Hour, Min, Sec, Prof_24h, dt) RESULT(Prof_CurrTime)

      IMPLICIT NONE

      INTEGER :: i, j !Used to count over hours and sub-hourly timesteps
      INTEGER, INTENT(IN) :: Hour, Min, Sec, dt
      INTEGER :: total_sec, SecPerHour
      REAL(KIND(1D0)), DIMENSION(0:23), INTENT(IN) :: Prof_24h
      REAL(KIND(1D0)), DIMENSION(0:23) :: Prof_24h_sum
      REAL(KIND(1D0)) :: deltaProf !Change in hourly profiles per model timestep
      REAL(KIND(1D0)) :: Prof_CurrTime

      total_sec = Min*60 + Sec
      SecPerHour = 3600

      Prof_24h_sum = MERGE(Prof_24h/(SUM(Prof_24h)), 0.D0, SUM(Prof_24h) /= 0) ! prevent zero-division

      i = hour
      j = i + 1
      IF (j == 24) j = 0

      deltaProf = (Prof_24h_sum(j) - Prof_24h_sum(i))/SecPerHour
      Prof_CurrTime = Prof_24h_sum(hour) + deltaProf*total_sec
      Prof_CurrTime = Prof_CurrTime*dt/SecPerHour

   END FUNCTION get_Prof_SpecTime_sum

END MODULE module_ctrl_input_profile

!-------------------------------------------------------------------------
! Utility subroutine for skipping header lines in input files
! Used by ESTM module for reading external temperature data
!-------------------------------------------------------------------------
SUBROUTINE SkipHeader(lfn, skip)
   USE defaultnotUsed
   IMPLICIT NONE

   INTEGER, INTENT(IN) :: lfn   ! Logical file number
   INTEGER, INTENT(IN) :: skip  ! Number of header lines to skip
   INTEGER :: i

   DO i = 1, skip
      READ (lfn, *, err=201, iostat=ios_out)
   END DO

   RETURN

201 reall = REAL(skip)
   CALL ErrorHint(20, 'In SkipHeader subroutine.', reall, notUsed, ios_out)
END SUBROUTINE SkipHeader
