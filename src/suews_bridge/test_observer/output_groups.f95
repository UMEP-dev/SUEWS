! Hand-maintained native OUTPUT_LINE and OUTPUT_BLOCK witnesses; capi-observer only.
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

SUBROUTINE suews_obs_output_block(nrows, widths_out, corners_out, err) &
   BIND(C, name='suews_obs_output_block')
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double
   USE module_ctrl_type, ONLY: output_block
   IMPLICIT NONE
   INTEGER(c_int), VALUE, INTENT(IN) :: nrows
   INTEGER(c_int), INTENT(OUT) :: widths_out(11)
   REAL(c_double), INTENT(OUT) :: corners_out(4, 11)
   INTEGER(c_int), INTENT(OUT) :: err
   TYPE(output_block) :: blk
   INTEGER :: nrows_i
   LOGICAL :: ok

   err = 1
   widths_out = 0_c_int
   corners_out = 0.0_c_double
   nrows_i = INT(nrows)
   IF (nrows_i < 2) RETURN

   CALL blk%init(nrows_i)
   ok = .TRUE.
   ! Order matches TYPE(output_block) declaration and OUTPUT_BLOCK_FIELD_ORDER.
   CALL check_and_seed_block(blk%dataOutBlockSUEWS, 1, nrows_i, widths_out(1), corners_out(:, 1), ok)
   CALL check_and_seed_block(blk%dataOutBlockSnow, 2, nrows_i, widths_out(2), corners_out(:, 2), ok)
   CALL check_and_seed_block(blk%dataOutBlockESTM, 3, nrows_i, widths_out(3), corners_out(:, 3), ok)
   CALL check_and_seed_block(blk%dataOutBlockEHC, 4, nrows_i, widths_out(4), corners_out(:, 4), ok)
   CALL check_and_seed_block(blk%dataOutBlockRSL, 5, nrows_i, widths_out(5), corners_out(:, 5), ok)
   CALL check_and_seed_block(blk%dataOutBlockBEERS, 6, nrows_i, widths_out(6), corners_out(:, 6), ok)
   CALL check_and_seed_block(blk%dataOutBlockDebug, 7, nrows_i, widths_out(7), corners_out(:, 7), ok)
   CALL check_and_seed_block(blk%dataOutBlockSPARTACUS, 8, nrows_i, widths_out(8), corners_out(:, 8), ok)
   CALL check_and_seed_block(blk%dataOutBlockDailyState, 9, nrows_i, widths_out(9), corners_out(:, 9), ok)
   CALL check_and_seed_block(blk%dataOutBlockSTEBBS, 10, nrows_i, widths_out(10), corners_out(:, 10), ok)
   CALL check_and_seed_block(blk%dataOutBlockNHood, 11, nrows_i, widths_out(11), corners_out(:, 11), ok)
   IF (.NOT. ok) THEN
      CALL blk%cleanup()
      err = 2
      RETURN
   END IF

   CALL blk%cleanup()
   IF (ALLOCATED(blk%dataOutBlockSUEWS) .OR. &
       ALLOCATED(blk%dataOutBlockSnow) .OR. &
       ALLOCATED(blk%dataOutBlockESTM) .OR. &
       ALLOCATED(blk%dataOutBlockEHC) .OR. &
       ALLOCATED(blk%dataOutBlockRSL) .OR. &
       ALLOCATED(blk%dataOutBlockBEERS) .OR. &
       ALLOCATED(blk%dataOutBlockDebug) .OR. &
       ALLOCATED(blk%dataOutBlockSPARTACUS) .OR. &
       ALLOCATED(blk%dataOutBlockDailyState) .OR. &
       ALLOCATED(blk%dataOutBlockSTEBBS) .OR. &
       ALLOCATED(blk%dataOutBlockNHood)) THEN
      err = 3
      RETURN
   END IF
   err = 0
CONTAINS
   SUBROUTINE check_and_seed_block(arr, gidx, n_rows, width_out, corners, is_ok)
      REAL(c_double), ALLOCATABLE, INTENT(INOUT) :: arr(:, :)
      INTEGER, INTENT(IN) :: gidx, n_rows
      INTEGER(c_int), INTENT(OUT) :: width_out
      REAL(c_double), INTENT(OUT) :: corners(4)
      LOGICAL, INTENT(INOUT) :: is_ok
      INTEGER :: r, c, n_cols
      REAL(c_double) :: expected_val

      IF (.NOT. is_ok) RETURN
      IF (.NOT. ALLOCATED(arr)) THEN
         is_ok = .FALSE.
         RETURN
      END IF
      IF (SIZE(arr, 1) /= n_rows .OR. SIZE(arr, 2) <= 5) THEN
         is_ok = .FALSE.
         RETURN
      END IF
      IF (ANY(arr /= -999.0_c_double)) THEN
         is_ok = .FALSE.
         RETURN
      END IF
      n_cols = SIZE(arr, 2)
      width_out = INT(n_cols, c_int)
      DO c = 1, n_cols
         DO r = 1, n_rows
            arr(r, c) = REAL(gidx * 100000, c_double) + REAL(r * 1000 + c, c_double) + &
               0.125_c_double * REAL(r, c_double)
         END DO
      END DO
      DO c = 1, n_cols
         DO r = 1, n_rows
            expected_val = REAL(gidx * 100000, c_double) + REAL(r * 1000 + c, c_double) + &
               0.125_c_double * REAL(r, c_double)
            IF (arr(r, c) /= expected_val) THEN
               is_ok = .FALSE.
               RETURN
            END IF
         END DO
      END DO
      corners(1) = arr(1, 1)
      corners(2) = arr(1, n_cols)
      corners(3) = arr(n_rows, 1)
      corners(4) = arr(n_rows, n_cols)
   END SUBROUTINE check_and_seed_block
END SUBROUTINE suews_obs_output_block
END SUBMODULE test_output_groups
