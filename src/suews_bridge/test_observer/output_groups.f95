! Hand-maintained native OUTPUT_LINE witness; capi-observer only.
! Values originate in real typed members, not a shadow or payload codec.
SUBMODULE (module_ctrl_driver) test_output_groups
   IMPLICIT NONE
CONTAINS
SUBROUTINE suews_obs_output_groups(data, dates, ld, ncols, err) BIND(C)
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double
   USE module_ctrl_type, ONLY: output_line
   IMPLICIT NONE
   INTEGER(c_int), VALUE, INTENT(IN) :: ld, ncols
   REAL(c_double), INTENT(INOUT) :: data(ld, ncols), dates(ld, 5)
   INTEGER(c_int), INTENT(OUT) :: err
   TYPE(output_line) :: line
   INTEGER :: row, expected_cols

   err = 1
   expected_cols = &
      SIZE(line%dataOutLineSUEWS) - 5 + &
      SIZE(line%dataOutLineSnow) - 5 + &
      SIZE(line%dataOutLineBEERS) - 5 + &
      SIZE(line%dataOutLineESTM) - 5 + &
      SIZE(line%dataOutLineEHC) - 5 + &
      SIZE(line%dataOutLineDailyState) - 5 + &
      SIZE(line%dataOutLineRSL) - 5 + &
      SIZE(line%dataOutLineDebug) - 5 + &
      SIZE(line%dataOutLineSPARTACUS) - 5 + &
      SIZE(line%dataOutLineSTEBBS) - 5 + &
      SIZE(line%dataOutLineNHood) - 5
   IF (ld /= 5 .OR. ncols /= expected_cols) RETURN
   DO row = 1, 2
      CALL line%init()
      ! datetimeLine is deliberately different: the writer uses SUEWS's prefix.
      line%datetimeLine = -123.0_c_double
      line%dataOutLineSUEWS(1:5) = [2012.0_c_double, 37.0_c_double, &
         9.0_c_double, REAL(5*row, c_double), 37.375_c_double + row/288.0_c_double]
      ! Seed every data column of all 11 groups (1,355 columns, including NHood)
      ! with globally distinct, binary-exact non-zero sentinels.
      CALL seed_group_line(line%dataOutLineSUEWS, 1, row)
      CALL seed_group_line(line%dataOutLineSnow, 2, row)
      CALL seed_group_line(line%dataOutLineBEERS, 3, row)
      CALL seed_group_line(line%dataOutLineESTM, 4, row)
      CALL seed_group_line(line%dataOutLineEHC, 5, row)
      CALL seed_group_line(line%dataOutLineDailyState, 6, row)
      CALL seed_group_line(line%dataOutLineRSL, 7, row)
      CALL seed_group_line(line%dataOutLineDebug, 8, row)
      CALL seed_group_line(line%dataOutLineSPARTACUS, 9, row)
      CALL seed_group_line(line%dataOutLineSTEBBS, 10, row)
      CALL seed_group_line(line%dataOutLineNHood, 11, row)
      ! Two rows in a five-row allocation exercise both descriptor strides.
      CALL store_output_line(line, row, data(2:4:2, :), dates(2:4:2, :))
   END DO
   err = 0
CONTAINS
   SUBROUTINE seed_group_line(arr, group_idx, row_idx)
      REAL(c_double), INTENT(INOUT) :: arr(:)
      INTEGER, INTENT(IN) :: group_idx, row_idx
      INTEGER :: col, n_data
      REAL(c_double) :: base

      n_data = SIZE(arr) - 5
      base = REAL(group_idx * 10000, c_double)
      DO col = 1, n_data
         arr(5 + col) = base + REAL(col, c_double) + 0.25_c_double * REAL(row_idx, c_double)
      END DO
   END SUBROUTINE seed_group_line
END SUBROUTINE suews_obs_output_groups
END SUBMODULE test_output_groups
