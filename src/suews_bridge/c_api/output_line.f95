! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for output_line.
! -----------------------------------------------------------------------------
MODULE module_c_api_output_line
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text
   USE module_ctrl_const_allocate, ONLY: &
      ncolumnsDataOutSUEWS, ncolumnsDataOutSnow, ncolumnsDataOutESTM, ncolumnsDataOutEHC, &
      ncolumnsDataOutRSL, ncolumnsdataOutBL, ncolumnsDataOutBEERS, ncolumnsDataOutDebug, &
      ncolumnsDataOutSPARTACUS, &
      ncolumnsDataOutDailyState, ncolumnsDataOutSTEBBS, ncolumnsDataOutNHood

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_DATETIME_LEN = 5_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_SUEWS_LEN = INT(ncolumnsDataOutSUEWS, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_SNOW_LEN = INT(ncolumnsDataOutSnow, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_ESTM_LEN = INT(ncolumnsDataOutESTM, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_EHC_LEN = INT(ncolumnsDataOutEHC, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_RSL_LEN = INT(ncolumnsDataOutRSL, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_BEERS_LEN = INT(ncolumnsDataOutBEERS, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_DEBUG_LEN = INT(ncolumnsDataOutDebug, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_SPARTACUS_LEN = INT(ncolumnsDataOutSPARTACUS, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_DAILYSTATE_LEN = INT(ncolumnsDataOutDailyState, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_STEBBS_LEN = INT(ncolumnsDataOutSTEBBS, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_NHOOD_LEN = INT(ncolumnsDataOutNHood, c_int)
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_LEN = &
      SUEWS_CAPI_OUTPUT_LINE_DATETIME_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_SUEWS_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_SNOW_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_ESTM_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_EHC_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_RSL_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_BEERS_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_DEBUG_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_SPARTACUS_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_DAILYSTATE_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_STEBBS_LEN + &
      SUEWS_CAPI_OUTPUT_LINE_NHOOD_LEN

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_BL_LEN = INT(ncolumnsdataOutBL, c_int)

   ! Number of output groups returned by suews_output_group_ncolumns (including datetime and BL).
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_N_GROUPS = 13_c_int

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_LINE_SCHEMA_VERSION = 1_c_int

   TYPE :: output_line_shadow
      REAL(c_double), DIMENSION(5) :: datetime_line = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutSUEWS) :: data_out_line_suews = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutSnow) :: data_out_line_snow = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutESTM) :: data_out_line_estm = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutEHC) :: data_out_line_ehc = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutRSL) :: data_out_line_rsl = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutBEERS) :: data_out_line_beers = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutDebug) :: data_out_line_debug = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutSPARTACUS) :: data_out_line_spartacus = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutDailyState) :: data_out_line_daily_state = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutSTEBBS) :: data_out_line_stebbs = -999.0_c_double
      REAL(c_double), DIMENSION(ncolumnsDataOutNHood) :: data_out_line_nhood = -999.0_c_double
   END TYPE output_line_shadow

   PUBLIC :: suews_output_line_len
   PUBLIC :: suews_output_line_schema_version
   PUBLIC :: suews_output_line_default
   PUBLIC :: suews_output_line_error_message
   PUBLIC :: suews_output_group_ncolumns

CONTAINS

   SUBROUTINE suews_output_line_len(n_flat, err) BIND(C, name='suews_output_line_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_OUTPUT_LINE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_output_line_len


   SUBROUTINE suews_output_line_schema_version(schema_version, err) BIND(C, name='suews_output_line_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_OUTPUT_LINE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_output_line_schema_version


   SUBROUTINE suews_output_line_default(flat, n_flat, err) BIND(C, name='suews_output_line_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(output_line_shadow) :: state

      CALL output_line_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_output_line_default


   SUBROUTINE output_line_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(output_line_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: idx
      INTEGER(c_int) :: i

      IF (n_flat < SUEWS_CAPI_OUTPUT_LINE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1_c_int

      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_DATETIME_LEN
         flat(idx) = state%datetime_line(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_SUEWS_LEN
         flat(idx) = state%data_out_line_suews(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_SNOW_LEN
         flat(idx) = state%data_out_line_snow(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_ESTM_LEN
         flat(idx) = state%data_out_line_estm(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_EHC_LEN
         flat(idx) = state%data_out_line_ehc(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_RSL_LEN
         flat(idx) = state%data_out_line_rsl(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_BEERS_LEN
         flat(idx) = state%data_out_line_beers(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_DEBUG_LEN
         flat(idx) = state%data_out_line_debug(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_SPARTACUS_LEN
         flat(idx) = state%data_out_line_spartacus(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_DAILYSTATE_LEN
         flat(idx) = state%data_out_line_daily_state(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_STEBBS_LEN
         flat(idx) = state%data_out_line_stebbs(i)
         idx = idx + 1_c_int
      END DO
      DO i = 1_c_int, SUEWS_CAPI_OUTPUT_LINE_NHOOD_LEN
         flat(idx) = state%data_out_line_nhood(i)
         idx = idx + 1_c_int
      END DO

      err = SUEWS_CAPI_OK

   END SUBROUTINE output_line_pack


   SUBROUTINE suews_output_line_error_message(code, buffer, buffer_len) BIND(C, name='suews_output_line_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_output_line_error_message

   SUBROUTINE suews_output_group_ncolumns(ncols_arr, n_groups, err) &
         BIND(C, name='suews_output_group_ncolumns')
      ! Return per-group data column counts (excluding datetime prefix) from compiled
      ! Fortran ncolumnsDataOut* constants. Datetime group returns 5.
      ! Order: datetime, SUEWS, snow, ESTM, EHC, RSL, BL, debug, BEERS,
      !        DailyState, SPARTACUS, STEBBS, NHood.
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: ncols_arr(SUEWS_CAPI_OUTPUT_N_GROUPS)
      INTEGER(c_int), INTENT(out) :: n_groups
      INTEGER(c_int), INTENT(out) :: err

      ncols_arr(1)  = SUEWS_CAPI_OUTPUT_LINE_DATETIME_LEN            ! datetime: 5
      ncols_arr(2)  = SUEWS_CAPI_OUTPUT_LINE_SUEWS_LEN - 5_c_int     ! SUEWS
      ncols_arr(3)  = SUEWS_CAPI_OUTPUT_LINE_SNOW_LEN - 5_c_int      ! snow
      ncols_arr(4)  = SUEWS_CAPI_OUTPUT_LINE_ESTM_LEN - 5_c_int      ! ESTM
      ncols_arr(5)  = SUEWS_CAPI_OUTPUT_LINE_EHC_LEN - 5_c_int       ! EHC
      ncols_arr(6)  = SUEWS_CAPI_OUTPUT_LINE_RSL_LEN - 5_c_int       ! RSL
      ncols_arr(7)  = SUEWS_CAPI_OUTPUT_LINE_BL_LEN - 5_c_int        ! BL
      ncols_arr(8)  = SUEWS_CAPI_OUTPUT_LINE_DEBUG_LEN - 5_c_int     ! debug
      ncols_arr(9)  = SUEWS_CAPI_OUTPUT_LINE_BEERS_LEN - 5_c_int     ! BEERS
      ncols_arr(10) = SUEWS_CAPI_OUTPUT_LINE_DAILYSTATE_LEN - 5_c_int ! DailyState
      ncols_arr(11) = SUEWS_CAPI_OUTPUT_LINE_SPARTACUS_LEN - 5_c_int ! SPARTACUS
      ncols_arr(12) = SUEWS_CAPI_OUTPUT_LINE_STEBBS_LEN - 5_c_int    ! STEBBS
      ncols_arr(13) = SUEWS_CAPI_OUTPUT_LINE_NHOOD_LEN - 5_c_int     ! NHood

      n_groups = SUEWS_CAPI_OUTPUT_N_GROUPS
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_output_group_ncolumns


END MODULE module_c_api_output_line

MODULE c_api_output_line_module
   USE module_c_api_output_line
END MODULE c_api_output_line_module
