MODULE ctrl_output
   !===========================================================================================
   ! generic output functions for SUEWS
   ! authors: Ting Sun (ting.sun@reading.ac.uk)
   !
   ! disclamier:
   !     This code employs the netCDF Fortran 90 API.
   !     Full documentation of the netCDF Fortran 90 API can be found at:
   !     https://www.unidata.ucar.edu/software/netcdf/netcdf-4/newdocs/netcdf-f90/
   !     Part of the work is under the help of examples provided by the documentation.
   !
   ! purpose:
   ! these subroutines write out the results of SUEWS in netCDF format.
   !
   !
   ! history:
   ! TS 20161209: initial version of netcdf function
   ! TS 20161213: standalise the txt2nc procedure
   ! TS 20170414: generic output procedures
   ! TS 20171016: added support for DailyState
   ! TS 20171017: combined txt and nc wrappers into one: reduced duplicate code at two places
   !===========================================================================================

   USE allocateArray
   USE cbl_module
   USE data_in
   ! USE defaultNotUsed
   ! USE ESTM_data
   USE gis_data
   ! USE initial
   USE sues_data
   USE time
   USE strings

   IMPLICIT NONE

   INTEGER :: n

!Define useful formats here
   CHARACTER(len=10), PARAMETER :: fy = 'i0004' !4 digit integer for year
   CHARACTER(len=10), PARAMETER :: ft = 'i0004' !3 digit integer for id, it, imin
   CHARACTER(len=10), PARAMETER :: fd = 'f08.4' !3 digits + 4 dp for dectime
   CHARACTER(len=10), PARAMETER :: f94 = 'f09.4' !standard output format: 4 dp + 4 digits
   CHARACTER(len=10), PARAMETER :: f104 = 'f10.4' !standard output format: 4 dp + 5 digits
   CHARACTER(len=10), PARAMETER :: f106 = 'f10.6' !standard output format: 6 dp + 3 digits
   CHARACTER(len=10), PARAMETER :: f146 = 'f14.6' !standard output format: 6 dp + 7 digits

! Define aggregation methods here
   CHARACTER(len=1), PARAMETER :: aT = 'T' !time columns
   CHARACTER(len=1), PARAMETER :: aA = 'A' !average
   CHARACTER(len=1), PARAMETER :: aS = 'S' !sum
   CHARACTER(len=1), PARAMETER :: aL = 'L' !last value

   CHARACTER(len=3) :: itext

   ! define type: variable attributes
   TYPE varAttr
      CHARACTER(len=20) :: header ! short name in headers
      CHARACTER(len=12) :: unit ! unit
      CHARACTER(len=10) :: fmt ! output format
      CHARACTER(len=100) :: longNm ! long name for detailed description
      CHARACTER(len=1) :: aggreg ! aggregation method
      CHARACTER(len=10) :: group ! group: datetime, default, ESTM, Snow, etc.
      INTEGER :: level ! output priority level: 0 for highest (defualt output)
   END TYPE varAttr

   ! initialise valist
   TYPE(varAttr) :: varListAll(1400)

   ! =========================================================================
   ! Variable definitions now auto-generated from Python OUTPUT_REGISTRY
   ! Source: src/supy/data_model/output/__init__.py
   ! Generator: src/suews/src/generate_varlist.py
   ! =========================================================================
   ! NOTE: The following groups are not yet migrated to Python:
   !   - SPARTACUS (experimental radiation model)
   !   - EHC (experimental heat capacity model)
   !   - STEBBS (experimental energy balance model)
   !   - NHood (neighbourhood iteration diagnostics)
   ! These will continue to use Fortran-only definitions until migrated.
   ! =========================================================================

   INCLUDE 'varlist_generated.f95'

CONTAINS
   ! main wrapper that handles both txt and nc files
   SUBROUTINE SUEWS_Output(irMax, iv, Gridiv, iyr)
      IMPLICIT NONE
      INTEGER, INTENT(in) :: irMax
! #ifdef nc
!       INTEGER, INTENT(in), OPTIONAL ::iv, Gridiv, iyr
! #else
      INTEGER, INTENT(in) :: iv, Gridiv, iyr
! #endif

      INTEGER :: n_group_use, err, outLevel, i
      TYPE(varAttr), DIMENSION(:), ALLOCATABLE :: varListX
      CHARACTER(len=10) :: groupList0(12)
      CHARACTER(len=10), DIMENSION(:), ALLOCATABLE :: grpList
      LOGICAL :: groupCond(12)

      ! determine outLevel
      SELECT CASE (WriteOutOption)
      CASE (0) !all (not snow-related)
         outLevel = 1
      CASE (1) !all plus snow-related
         outLevel = 2
      CASE (2) !minimal output
         outLevel = 0
      END SELECT

      ! determine groups to output
      ! TODO: needs to be smarter, automate this filtering
      groupList0(1) = 'SUEWS'
      groupList0(2) = 'BEERS'
      groupList0(3) = 'BL'
      groupList0(4) = 'snow'
      groupList0(5) = 'ESTM'
      groupList0(6) = 'DailyState'
      groupList0(7) = 'RSL'
      groupList0(8) = 'debug'
      groupList0(9) = 'SPARTACUS'
      groupList0(10) = 'EHC'
      groupList0(11) = 'STEBBS'

      groupCond = [ &
                  .TRUE., &
                  .TRUE., &
                  CBLuse >= 1, &
                  SnowUse >= 1, &
                  StorageHeatMethod == 4 .OR. StorageHeatMethod == 14, &
                  .TRUE., &
                  .TRUE., &
                  .TRUE., &
                  .TRUE., &
                  StorageHeatMethod == 5, &
                  .TRUE., &
                  .TRUE. &
                  ]
      n_group_use = COUNT(groupCond)

      ! PRINT*, grpList0,xx

      ALLOCATE (grpList(n_group_use), stat=err)
      IF (err /= 0) PRINT *, "grpList: Allocation request denied"

      grpList = PACK(groupList0, mask=groupCond)

      ! PRINT*, grpList,SIZE(grpList, dim=1)

      ! loop over all groups
      DO i = 1, SIZE(grpList), 1
         !PRINT*, 'i',i
         n_group_use = COUNT(varListAll%group == TRIM(grpList(i)), dim=1)
         !  PRINT*, 'number of variables:',xx, 'in group: ',grpList(i)
         !  print*, 'all group names: ',varList%group
         ALLOCATE (varListX(5 + n_group_use), stat=err)
         IF (err /= 0) PRINT *, "varListX: Allocation request denied"
         ! datetime
         varListX(1:5) = varListAll(1:5)
         ! variable
         varListX(6:5 + n_group_use) = PACK(varListAll, mask=(varListAll%group == TRIM(grpList(i))))

         IF (TRIM(varListX(SIZE(varListX))%group) /= 'DailyState') THEN
            ! all output arrays but DailyState
            ! all output frequency option:
            ! as forcing:
            IF (ResolutionFilesOut == Tstep .OR. KeepTstepFilesOut == 1) THEN
               CALL SUEWS_Output_txt_grp(iv, irMax, iyr, varListX, Gridiv, outLevel, Tstep)
            END IF
            !  as specified ResolutionFilesOut:
            IF (ResolutionFilesOut /= Tstep) THEN
               CALL SUEWS_Output_txt_grp(iv, irMax, iyr, varListX, Gridiv, outLevel, ResolutionFilesOut)
            END IF
         ELSE
            !  DailyState array, which does not need aggregation
            CALL SUEWS_Output_txt_grp(iv, irMax, iyr, varListX, Gridiv, outLevel, Tstep)

         END IF

         IF (ALLOCATED(varListX)) DEALLOCATE (varListX, stat=err)
         IF (err /= 0) PRINT *, "varListX: Deallocation request denied"
         !  PRINT*, 'i',i,'end'

      END DO
   END SUBROUTINE SUEWS_Output

   ! output wrapper function for one group
   SUBROUTINE SUEWS_Output_txt_grp(iv, irMax, iyr, varListX, Gridiv, outLevel, outFreq_s)
      IMPLICIT NONE

      TYPE(varAttr), DIMENSION(:), INTENT(in) :: varListX
      INTEGER, INTENT(in) :: iv, irMax, iyr, Gridiv, outLevel, outFreq_s

      INTEGER :: err
      INTEGER :: n_var

      INTEGER, DIMENSION(:), ALLOCATABLE :: id_seq ! id sequence as in the dataOutX/dataOutX_agg
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutX
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutX_agg

      ! number of varialbes for output
      n_var = SIZE(varListX)

      IF (.NOT. ALLOCATED(dataOutX)) THEN
         ALLOCATE (dataOutX(irMax, n_var), stat=err)
         IF (err /= 0) PRINT *, "dataOutX: Allocation request denied"
      END IF

      ! determine dataOutX array according to variable group
      SELECT CASE (TRIM(varListX(n_var)%group))
      CASE ('SUEWS') !default
         dataOutX = dataOutSUEWS(1:irMax, 1:n_var, Gridiv)

      CASE ('BEERS') !SOLWEIG
         dataOutX = dataOutBEERS(1:irMax, 1:n_var, Gridiv)
         ! dataOutX = dataOutSOLWEIG(1:irMax, 1:n_var, Gridiv)

      CASE ('BL') !BL
         dataOutX = dataOutBL(1:irMax, 1:n_var, Gridiv)

      CASE ('snow') !snow
         dataOutX = dataOutSnow(1:irMax, 1:n_var, Gridiv)

      CASE ('ESTM') !ESTM
         dataOutX = dataOutESTM(1:irMax, 1:n_var, Gridiv)

      CASE ('RSL') !RSL
         dataOutX = dataOutRSL(1:irMax, 1:n_var, Gridiv)

      CASE ('debug') !debug
         dataOutX = dataOutDebug(1:irMax, 1:n_var, Gridiv)

      CASE ('SPARTACUS') !SPARTACUS
         dataOutX = dataOutSPARTACUS(1:irMax, 1:n_var, Gridiv)

      CASE ('EHC') !EHC
         dataOutX = dataOutEHC(1:irMax, 1:n_var, Gridiv)

      CASE ('STEBBS') !STEBBS
         dataOutX = dataOutSTEBBS(1:irMax, 1:n_var, Gridiv)

      CASE ('NHood') !NHood
         dataOutX = dataOutNHood(1:irMax, 1:n_var, Gridiv)

      CASE ('DailyState') !DailyState
         ! get correct day index
         CALL unique(INT(PACK(dataOutSUEWS(1:irMax, 2, Gridiv), &
                              mask=(dataOutSUEWS(1:irMax, 3, Gridiv) == 23 &
                                    .AND. dataOutSUEWS(1:irMax, 4, Gridiv) == (nsh - 1.)/nsh*60))), &
                     id_seq)

         IF (ALLOCATED(dataOutX)) THEN
            DEALLOCATE (dataOutX)
            IF (err /= 0) PRINT *, "dataOutX: Deallocation request denied"
         END IF

         IF (.NOT. ALLOCATED(dataOutX)) THEN
            ALLOCATE (dataOutX(SIZE(id_seq), n_var), stat=err)
            IF (err /= 0) PRINT *, "dataOutX: Allocation request denied"
         END IF

         dataOutX = dataOutDailyState(id_seq, 1:n_var, Gridiv)
         ! print*, id_seq
         ! print*, dataOutDailyState(id_seq,1:SIZE(varListX),Gridiv)
         ! print*, 1/(nsh-nsh)
      END SELECT

      ! aggregation:
      ! aggregation is done for every group but 'DailyState'
      IF (TRIM(varListX(SIZE(varListX))%group) /= 'DailyState') THEN

         CALL SUEWS_Output_Agg(dataOutX_agg, dataOutX, varListX, irMax, outFreq_s)
      ELSE
         IF (.NOT. ALLOCATED(dataOutX_agg)) THEN
            ALLOCATE (dataOutX_agg(SIZE(dataOutX, dim=1), SIZE(varListX)), stat=err)
            IF (err /= 0) PRINT *, ": Allocation request denied"
         END IF
         dataOutX_agg = dataOutX
      END IF

      ! output:
      ! initialise file when processing first metblock
      IF (iv == 1) CALL SUEWS_Output_Init(dataOutX_agg, varListX, iyr, Gridiv, outLevel)

      ! append the aggregated data to the specific txt file
      CALL SUEWS_Write_txt(dataOutX_agg, varListX, iyr, Gridiv, outLevel)

   END SUBROUTINE SUEWS_Output_txt_grp

   ! initialise an output file with file name and headers
   SUBROUTINE SUEWS_Output_Init(dataOutX, varList, iyr, Gridiv, outLevel)
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(in) :: dataOutX
      TYPE(varAttr), DIMENSION(:), INTENT(in) :: varList
      INTEGER, INTENT(in) :: iyr, Gridiv, outLevel

      TYPE(varAttr), DIMENSION(:), ALLOCATABLE :: varListSel
      INTEGER :: xx, err, fn, i, nargs
      CHARACTER(len=365) :: FileOutX
      CHARACTER(len=3) :: itextX
      CHARACTER(len=6) :: args(5)
      CHARACTER(len=16*SIZE(varList)) :: FormatOut
      CHARACTER(len=16) :: formatX
      CHARACTER(len=16), DIMENSION(:), ALLOCATABLE :: headerOut

      ! select variables to output
      xx = COUNT((varList%level <= outLevel), dim=1)
      WRITE (itextX, '(i3)') xx
      ALLOCATE (varListSel(xx), stat=err)
      IF (err /= 0) PRINT *, "varListSel: Allocation request denied"
      varListSel = PACK(varList, mask=(varList%level <= outLevel))

      ! generate file name
      CALL filename_gen(dataOutX, varList, iyr, Gridiv, FileOutX)

      ! store right-aligned headers
      ALLOCATE (headerOut(xx), stat=err)
      IF (err /= 0) PRINT *, "headerOut: Allocation request denied"

      ! create format string:
      DO i = 1, SIZE(varListSel)
         CALL parse(varListSel(i)%fmt, 'if.,', args, nargs)
         formatX = ADJUSTL('(a'//TRIM(args(2))//',1x)')
         ! adjust headers to right-aligned
         WRITE (headerOut(i), formatX) ADJUSTR(TRIM(ADJUSTL(varListSel(i)%header)))
         IF (i == 1) THEN
            FormatOut = ADJUSTL(TRIM(formatX))
         ELSE
            FormatOut = TRIM(FormatOut)//' '//ADJUSTL(TRIM(formatX))
         END IF
      END DO
      FormatOut = '('//TRIM(ADJUSTL(FormatOut))//')'

      ! create file
      fn = 9
      OPEN (fn, file=TRIM(ADJUSTL(FileOutX)), status='unknown')
      ! PRINT*, 'FileOutX in SUEWS_Output_Init: ',FileOutX
      ! write out headers
      WRITE (fn, FormatOut) headerOut
      CLOSE (fn)

      ! write out format file
      CALL formatFile_gen(dataOutX, varList, iyr, Gridiv, outLevel)

      ! clean up
      IF (ALLOCATED(varListSel)) DEALLOCATE (varListSel, stat=err)
      IF (err /= 0) PRINT *, "varListSel: Deallocation request denied"
      IF (ALLOCATED(headerOut)) DEALLOCATE (headerOut, stat=err)
      IF (err /= 0) PRINT *, "headerOut: Deallocation request denied"

   END SUBROUTINE SUEWS_Output_Init

   ! generate output format file
   SUBROUTINE formatFile_gen(dataOutX, varList, iyr, Gridiv, outLevel)
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(in) :: dataOutX
      TYPE(varAttr), DIMENSION(:), INTENT(in) :: varList
      INTEGER, INTENT(in) :: iyr, Gridiv, outLevel

      TYPE(varAttr), DIMENSION(:), ALLOCATABLE :: varListSel
      INTEGER :: xx, err, fn, i
      CHARACTER(len=365) :: FileOutX
      CHARACTER(len=100*300) :: str_cat
      CHARACTER(len=100) :: str_x = ''
      CHARACTER(len=3) :: itextX

      ! get filename
      CALL filename_gen(dataOutX, varList, iyr, Gridiv, FileOutX, 1)

      !select variables to output
      xx = COUNT((varList%level <= outLevel), dim=1)
      ALLOCATE (varListSel(xx), stat=err)
      IF (err /= 0) PRINT *, "varListSel: Allocation request denied"
      varListSel = PACK(varList, mask=(varList%level <= outLevel))

      ! create file
      fn = 9
      OPEN (fn, file=TRIM(ADJUSTL(FileOutX)), status='unknown')

      ! write out format strings
      ! column number:
      str_cat = ''
      DO i = 1, SIZE(varListSel)
         WRITE (itextX, '(i3)') i
         IF (i == 1) THEN
            str_cat = TRIM(ADJUSTL(itextX))
         ELSE
            str_cat = TRIM(str_cat)//';'//ADJUSTL(itextX)
         END IF
      END DO
      WRITE (fn, '(a)') TRIM(str_cat)

      ! header:
      str_cat = ''
      DO i = 1, SIZE(varListSel)
         str_x = varListSel(i)%header
         IF (i == 1) THEN
            str_cat = TRIM(ADJUSTL(str_x))
         ELSE
            str_cat = TRIM(str_cat)//';'//ADJUSTL(str_x)
         END IF
      END DO
      WRITE (fn, '(a)') TRIM(str_cat)

      ! long name:
      str_cat = ''
      DO i = 1, SIZE(varListSel)
         str_x = varListSel(i)%longNm
         IF (i == 1) THEN
            str_cat = TRIM(ADJUSTL(str_x))
         ELSE
            str_cat = TRIM(str_cat)//';'//ADJUSTL(str_x)
         END IF
      END DO
      WRITE (fn, '(a)') TRIM(str_cat)

      ! unit:
      str_cat = ''
      DO i = 1, SIZE(varListSel)
         str_x = varListSel(i)%unit
         IF (i == 1) THEN
            str_cat = TRIM(ADJUSTL(str_x))
         ELSE
            str_cat = TRIM(str_cat)//';'//ADJUSTL(str_x)
         END IF
      END DO
      WRITE (fn, '(a)') TRIM(str_cat)

      ! format:
      str_cat = ''
      DO i = 1, SIZE(varListSel)
         str_x = varListSel(i)%fmt
         IF (i == 1) THEN
            str_cat = TRIM(ADJUSTL(str_x))
         ELSE
            str_cat = TRIM(str_cat)//';'//ADJUSTL(str_x)
         END IF
      END DO
      WRITE (fn, '(a)') TRIM(str_cat)

      ! aggregation method:
      str_cat = ''
      DO i = 1, SIZE(varListSel)
         str_x = varListSel(i)%aggreg
         IF (i == 1) THEN
            str_cat = TRIM(ADJUSTL(str_x))
         ELSE
            str_cat = TRIM(str_cat)//';'//ADJUSTL(str_x)
         END IF
      END DO
      WRITE (fn, '(a)') TRIM(str_cat)

      ! close file
      CLOSE (fn)

      ! clean up
      IF (ALLOCATED(varListSel)) DEALLOCATE (varListSel, stat=err)
      IF (err /= 0) PRINT *, "varListSel: Deallocation request denied"

   END SUBROUTINE formatFile_gen

   ! aggregate data to specified resolution
   SUBROUTINE SUEWS_Output_Agg(dataOut_agg, dataOutX, varList, irMax, outFreq_s)
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(in) :: dataOutX
      TYPE(varAttr), DIMENSION(:), INTENT(in) :: varList
      INTEGER, INTENT(in) :: irMax, outFreq_s
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE, INTENT(out) :: dataOut_agg

      INTEGER :: nlinesOut, i, j, x
      REAL(KIND(1D0)) :: dataOut_aggX(1:SIZE(varList))
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOut_agg0
      nlinesOut = INT(nsh/(60.*60/outFreq_s))
      ! nGrid=SIZE(dataOutX, dim=3)

      ALLOCATE (dataOut_agg(INT(irMax/nlinesOut), SIZE(varList)))
      ALLOCATE (dataOut_agg0(nlinesOut, SIZE(varList)))

      DO i = nlinesOut, irMax, nlinesOut
         x = i/nlinesOut
         dataOut_agg0 = dataOutX(i - nlinesOut + 1:i, :)
         DO j = 1, SIZE(varList), 1
            IF (Diagnose == 1) THEN
               PRINT *, "aggregating variable ", j, " of ", varList(j)%header, 'in group ', varList(j)%group
            END IF
            ! aggregating different variables
            SELECT CASE (varList(j)%aggreg)
            CASE (aT) !time columns, aT
               dataOut_aggX(j) = dataOut_agg0(nlinesOut, j)
            CASE (aA) !average, aA
               dataOut_aggX(j) = SUM(dataOut_agg0(:, j))/nlinesOut
            CASE (aS) !sum, aS
               dataOut_aggX(j) = SUM(dataOut_agg0(:, j))
            CASE (aL) !last value, aL
               dataOut_aggX(j) = dataOut_agg0(nlinesOut, j)
            END SELECT

            IF (Diagnose == 1 .AND. i == irMax) THEN
               ! IF ( i==irMax ) THEN
               PRINT *, 'raw data of ', j, ':'
               PRINT *, dataOut_agg0(:, j)
               PRINT *, 'aggregated with method: ', varList(j)%aggreg
               PRINT *, dataOut_aggX(j)
               PRINT *, ''
            END IF
         END DO
         dataOut_agg(x, :) = dataOut_aggX
      END DO

   END SUBROUTINE SUEWS_Output_Agg

   ! append output data to the specific file at the specified outLevel
   SUBROUTINE SUEWS_Write_txt(dataOutX, varList, iyr, Gridiv, outLevel)
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(in) :: dataOutX
      TYPE(varAttr), DIMENSION(:), INTENT(in) :: varList
      INTEGER, INTENT(in) :: iyr, Gridiv, outLevel

      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dataOutSel
      TYPE(varAttr), DIMENSION(:), ALLOCATABLE :: varListSel
      CHARACTER(len=365) :: FileOutX
      INTEGER :: fn, i, xx, err
      INTEGER :: sizeVarListSel, sizedataOutX
      CHARACTER(len=12*SIZE(varList)) :: FormatOut
      ! LOGICAL :: initQ_file
      FormatOut = ''

      IF (Diagnose == 1) WRITE (*, *) 'Writting data of group: ', varList(SIZE(varList))%group

      !select variables to output
      sizeVarListSel = COUNT((varList%level <= outLevel), dim=1)
      ALLOCATE (varListSel(sizeVarListSel), stat=err)
      IF (err /= 0) PRINT *, "varListSel: Allocation request denied"
      varListSel = PACK(varList, mask=(varList%level <= outLevel))

      ! copy data accordingly
      sizedataOutX = SIZE(dataOutX, dim=1)
      ALLOCATE (dataOutSel(sizedataOutX, sizeVarListSel), stat=err)
      IF (err /= 0) PRINT *, "dataOutSel: Allocation request denied"
      ! print*, SIZE(varList%level),PACK((/(i,i=1,SIZE(varList%level))/), varList%level <= outLevel)
      ! print*, irMax,shape(dataOutX)
      dataOutSel = dataOutX(:, PACK((/(i, i=1, SIZE(varList%level))/), varList%level <= outLevel))
      ! do i = 1, 5
      !    print*, 'first several lines of dataOutX:', i, dataOutX(i,:)

      ! end do

      ! create format string:
      DO i = 1, sizeVarListSel
         ! PRINT*,''
         ! PRINT*,i
         ! PRINT*, LEN_TRIM(FormatOut),TRIM(FormatOut)
         ! PRINT*, LEN_TRIM(TRIM(FormatOut)//','),TRIM(FormatOut)//','
         IF (i == 1) THEN
            ! FormatOut=ADJUSTL(varListSel(i)%fmt)
            FormatOut = varListSel(i)%fmt
         ELSE

            ! FormatOut=TRIM(FormatOut)//','//ADJUSTL(varListSel(i)%fmt)
            FormatOut = TRIM(FormatOut)//','//TRIM(varListSel(i)%fmt)
         END IF
         ! PRINT*,''
         ! PRINT*,i
         ! PRINT*, 'FormatOut',FormatOut
      END DO
      FormatOut = '('//TRIM(ADJUSTL(FormatOut))//')'

      ! get filename
      CALL filename_gen(dataOutSel, varListSel, iyr, Gridiv, FileOutX)
      ! PRINT*, 'FileOutX in SUEWS_Write_txt: ',FileOutX

      ! test if FileOutX has been initialised
      ! IF ( .NOT. initQ_file(FileOutX) ) THEN
      !    CALL SUEWS_Output_Init(dataOutSel,varListSel,Gridiv,outLevel)
      ! END IF

      ! write out data
      fn = 50
      OPEN (fn, file=TRIM(FileOutX), position='append') !,err=112)
      DO i = 1, sizedataOutX
         ! PRINT*, 'Writting line',i
         ! PRINT*, 'FormatOut in writing',FormatOut
         ! PRINT*, dataOutSel(i,1:sizeVarListSel)
         WRITE (fn, FormatOut) &
            (INT(dataOutSel(i, xx)), xx=1, 4), &
            (dataOutSel(i, xx), xx=5, sizeVarListSel)
      END DO
      CLOSE (fn)

      IF (ALLOCATED(varListSel)) DEALLOCATE (varListSel, stat=err)
      IF (err /= 0) PRINT *, "varListSel: Deallocation request denied"

      IF (ALLOCATED(dataOutSel)) DEALLOCATE (dataOutSel, stat=err)
      IF (err /= 0) PRINT *, "dataOutSel: Deallocation request denied"

   END SUBROUTINE SUEWS_Write_txt

   SUBROUTINE filename_gen(dataOutX, varList, iyr, Gridiv, FileOutX, opt_fmt)
      USE datetime_module

      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:, :), INTENT(in) :: dataOutX ! to determine year & output frequency
      TYPE(varAttr), DIMENSION(:), INTENT(in) :: varList ! to determine output group
      INTEGER, INTENT(in) :: iyr ! to determine year
      INTEGER, INTENT(in) :: Gridiv ! to determine grid name as in SiteSelect
      INTEGER, INTENT(in), OPTIONAL :: opt_fmt ! to determine if a format file
      CHARACTER(len=365), INTENT(out) :: FileOutX ! the output file name

      CHARACTER(len=20) :: str_out_min, str_grid, &
                           str_date, str_year, str_DOY, str_grp, str_sfx
      INTEGER :: year_int, DOY_int, val_fmt, delta_t_min
      TYPE(datetime) :: dt1, dt2
      TYPE(timedelta) :: dt_x

      ! initialise with a default value
      val_fmt = -999

      IF (PRESENT(opt_fmt)) val_fmt = opt_fmt

      ! PRINT*, varList(:)%header
      ! PRINT*, 'dataOutX(1)',dataOutX(1,:)

      ! date:
      DOY_int = INT(dataOutX(1, 2))
      WRITE (str_DOY, '(i3.3)') DOY_int

! #ifdef nc
!       ! year for nc use that in dataOutX
!       year_int = INT(dataOutX(1, 1))
!       WRITE (str_year, '(i4)') year_int
!       str_date = '_'//TRIM(ADJUSTL(str_year))
!       ! add DOY as a specifier
!       IF (ncMode == 1) str_date = TRIM(ADJUSTL(str_date))//TRIM(ADJUSTL(str_DOY))
! #endif

      ! year for txt use specified value to avoid conflicts when crossing years
      year_int = iyr
      WRITE (str_year, '(i4)') year_int
      str_date = '_'//TRIM(ADJUSTL(str_year))

      ! output frequency in minute:
      IF (varList(6)%group == 'DailyState') THEN
         str_out_min = '' ! ignore this for DailyState
      ELSE
         ! derive output frequency from output arrays
         ! dt_x=
         dt1 = datetime(INT(dataOutX(1, 1)), 1, 1) + &
               timedelta(days=INT(dataOutX(1, 2) - 1), &
                         hours=INT(dataOutX(1, 3)), &
                         minutes=INT(dataOutX(1, 4)))

         dt2 = datetime(INT(dataOutX(2, 1)), 1, 1) + &
               timedelta(days=INT(dataOutX(2, 2) - 1), &
                         hours=INT(dataOutX(2, 3)), &
                         minutes=INT(dataOutX(2, 4)))

         dt_x = dt2 - dt1
         delta_t_min = INT(dt_x%total_seconds()/60)
         WRITE (str_out_min, '(i4)') delta_t_min
         str_out_min = '_'//TRIM(ADJUSTL(str_out_min))
      END IF

      ! group: output type
      str_grp = varList(6)%group
      IF (LEN(TRIM(str_grp)) > 0) str_grp = '_'//TRIM(ADJUSTL(str_grp))

      ! grid name:
      WRITE (str_grid, '(i10)') GridIDmatrix(Gridiv)
! #ifdef nc
!       IF (ncMode == 1) str_grid = '' ! grid name not needed by nc files
! #endif

      ! suffix:
      str_sfx = '.txt'
! #ifdef nc
!       IF (ncMode == 1) str_sfx = '.nc'
! #endif

      ! filename: FileOutX
      FileOutX = TRIM(FileOutputPath)// &
                 TRIM(FileCode)// &
                 TRIM(ADJUSTL(str_grid))// &
                 TRIM(ADJUSTL(str_date))// &
                 TRIM(ADJUSTL(str_grp))// &
                 TRIM(ADJUSTL(str_out_min))// &
                 TRIM(ADJUSTL(str_sfx))

      ! filename: format
      IF (val_fmt == 1) THEN
         FileOutX = TRIM(FileOutputPath)// &
                    TRIM(FileCode)// &
                    TRIM(ADJUSTL(str_grp))// &
                    '_OutputFormat.txt'
      END IF

   END SUBROUTINE filename_gen

   SUBROUTINE unique(vec, vec_unique)
      ! Return only the unique values from vec.

      IMPLICIT NONE

      INTEGER, DIMENSION(:), INTENT(in) :: vec
      INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(out) :: vec_unique

      INTEGER :: i, num
      LOGICAL, DIMENSION(SIZE(vec)) :: mask

      mask = .FALSE.

      DO i = 1, SIZE(vec)

         !count the number of occurrences of this element:
         num = COUNT(vec(i) == vec)

         IF (num == 1) THEN
            !there is only one, flag it:
            mask(i) = .TRUE.
         ELSE
            !flag this value only if it hasn't already been flagged:
            IF (.NOT. ANY(vec(i) == vec .AND. mask)) mask(i) = .TRUE.
         END IF

      END DO

      !return only flagged elements:
      ALLOCATE (vec_unique(COUNT(mask)))
      vec_unique = PACK(vec, mask)

      !if you also need it sorted, then do so.
      ! For example, with slatec routine:
      !call ISORT (vec_unique, [0], size(vec_unique), 1)

   END SUBROUTINE unique

   ! test if a txt file has been initialised
   LOGICAL FUNCTION initQ_file(FileName)
      IMPLICIT NONE
      CHARACTER(len=365), INTENT(in) :: FileName ! the output file name
      LOGICAL :: existQ
      CHARACTER(len=1000) :: longstring

      INQUIRE (file=TRIM(FileName), exist=existQ)
      IF (existQ) THEN
         OPEN (10, file=TRIM(FileName))
         READ (10, '(a)') longstring
         ! print*, 'longstring: ',longstring
         IF (VERIFY(longstring, 'Year') == 0) initQ_file = .FALSE.
         CLOSE (unit=10)
      ELSE
         initQ_file = .FALSE.
      END IF

   END FUNCTION initQ_file

   !========================================================================================
   FUNCTION count_lines(filename) RESULT(nlines)
      ! count the number of valid lines in a file
      ! invalid line starting with -9

      !========================================================================================
      IMPLICIT NONE
      CHARACTER(len=*) :: filename
      INTEGER :: nlines
      INTEGER :: io, iv

      OPEN (10, file=filename, iostat=io, status='old')

      ! if io error found, report iostat and exit
      IF (io /= 0) THEN
         PRINT *, 'io', io, 'for', filename
         STOP 'Cannot open file! '
      END IF

      nlines = 0
      DO
         READ (10, *, iostat=io) iv
         IF (io < 0 .OR. iv == -9) EXIT

         nlines = nlines + 1
      END DO
      CLOSE (10)
      nlines = nlines - 1 ! skip header
   END FUNCTION count_lines

   !===========================================================================!
   ! write the output of final SUEWS results in netCDF
   !   with spatial layout of QGIS convention
   ! the spatial matrix arranges successive rows down the page (i.e., north to south)
   !   and succesive columns across (i.e., west to east)
   ! the output file frequency is the same as metblocks in the main SUEWS loop
   !===========================================================================!

! #ifdef nc

!    SUBROUTINE SUEWS_Output_nc_grp(irMax, varList, outLevel, outFreq_s)
!       IMPLICIT NONE

!       TYPE(varAttr), DIMENSION(:), INTENT(in)::varList
!       INTEGER, INTENT(in) :: irMax, outLevel, outFreq_s

!       REAL(KIND(1d0)), ALLOCATABLE::dataOutX(:, :, :)
!       REAL(KIND(1d0)), ALLOCATABLE::dataOutX_agg(:, :, :), dataOutX_agg0(:, :)
!       INTEGER :: iGrid, err, idMin, idMax
!       INTEGER, DIMENSION(:), ALLOCATABLE  ::id_seq

!       IF (.NOT. ALLOCATED(dataOutX)) THEN
!          ALLOCATE (dataOutX(irMax, SIZE(varList), NumberOfGrids), stat=err)
!          IF (err /= 0) PRINT *, "dataOutX: Allocation request denied"
!       ENDIF

!       ! determine dataOutX array according to variable group
!       SELECT CASE (TRIM(varList(SIZE(varList))%group))
!       CASE ('SUEWS') !default
!          dataOutX = dataOutSUEWS(1:irMax, 1:SIZE(varList), :)

!       CASE ('BEERS') !SOLWEIG
!          ! todo: inconsistent data structure
!          dataOutX = dataOutSOLWEIG(1:irMax, 1:SIZE(varList), :)

!       CASE ('BL') !BL
!          dataOutX = dataOutBL(1:irMax, 1:SIZE(varList), :)

!       CASE ('snow')    !snow
!          dataOutX = dataOutSnow(1:irMax, 1:SIZE(varList), :)

!       CASE ('ESTM')    !ESTM
!          dataOutX = dataOutESTM(1:irMax, 1:SIZE(varList), :)

!       CASE ('DailyState')    !DailyState
!          ! get correct day index
!          CALL unique(INT(PACK(dataOutSUEWS(1:irMax, 2, 1), &
!                               mask=(dataOutSUEWS(1:irMax, 3, Gridiv) == 23 &
!                                     .AND. dataOutSUEWS(1:irMax, 4, Gridiv) == (nsh - 1)/nsh*60))), &
!                      id_seq)
!          IF (ALLOCATED(dataOutX)) THEN
!             DEALLOCATE (dataOutX)
!             IF (err /= 0) PRINT *, "dataOutX: Deallocation request denied"
!          ENDIF

!          IF (.NOT. ALLOCATED(dataOutX)) THEN
!             ALLOCATE (dataOutX(SIZE(id_seq), SIZE(varList), NumberOfGrids), stat=err)
!             IF (err /= 0) PRINT *, "dataOutX: Allocation request denied"
!          ENDIF

!          dataOutX = dataOutDailyState(id_seq, 1:SIZE(varList), :)
!          ! print*, 'idMin line',dataOutX(idMin,1:4,1)
!          ! print*, 'idMax line',dataOutX(idMax,1:4,1)

!       END SELECT

!       ! aggregation:
!       IF (TRIM(varList(SIZE(varList))%group) /= 'DailyState') THEN
!          DO iGrid = 1, NumberOfGrids
!             CALL SUEWS_Output_Agg(dataOutX_agg0, dataOutX(:, :, iGrid), varList, irMax, outFreq_s)
!             IF (.NOT. ALLOCATED(dataOutX_agg)) THEN
!                ALLOCATE (dataOutX_agg(SIZE(dataOutX_agg0, dim=1), SIZE(varList), NumberOfGrids), stat=err)
!                IF (err /= 0) PRINT *, ": Allocation request denied"
!             ENDIF
!             dataOutX_agg(:, :, iGrid) = dataOutX_agg0
!          END DO
!       ELSE
!          IF (.NOT. ALLOCATED(dataOutX_agg)) THEN
!             ALLOCATE (dataOutX_agg(SIZE(dataOutX, dim=1), SIZE(varList), NumberOfGrids), stat=err)
!             IF (err /= 0) PRINT *, ": Allocation request denied"
!          ENDIF
!          dataOutX_agg = dataOutX
!       ENDIF

!       ! write out data
!       CALL SUEWS_Write_nc(dataOutX_agg, varList, outLevel)
!       IF (ALLOCATED(dataOutX_agg)) THEN
!          DEALLOCATE (dataOutX_agg)
!          IF (err /= 0) PRINT *, "dataOutX_agg: Deallocation request denied"
!       ENDIF
!    END SUBROUTINE SUEWS_Output_nc_grp

!    ! SUBROUTINE SUEWS_Write_nc(dataOutX, varList, outLevel)
!    !    ! generic subroutine to write out data in netCDF format
!    !    USE netCDF

!    !    IMPLICIT NONE
!    !    REAL(KIND(1d0)), DIMENSION(:, :, :), INTENT(in)::dataOutX
!    !    TYPE(varAttr), DIMENSION(:), INTENT(in)::varList
!    !    INTEGER, INTENT(in) :: outLevel

!    !    CHARACTER(len=365):: fileOut
!    !    REAL(KIND(1d0)), DIMENSION(:, :, :), ALLOCATABLE::dataOutSel
!    !    TYPE(varAttr), DIMENSION(:), ALLOCATABLE::varListSel

!    !    ! We are writing 3D data, {time, y, x}
!    !    INTEGER, PARAMETER :: NDIMS = 3, iVarStart = 6
!    !    INTEGER :: NX, NY, nTime, nVar, err

!    !    ! When we create netCDF files, variables and dimensions, we get back
!    !    ! an ID for each one.
!    !    INTEGER :: ncID, varID, dimids(NDIMS), varIDGrid
!    !    INTEGER :: x_dimid, y_dimid, time_dimid, iVar, varIDx, varIDy, varIDt, varIDCRS
!    !    REAL(KIND(1d0)), ALLOCATABLE :: varOut(:, :, :), &
!    !                                    varX(:, :), varY(:, :), &
!    !                                    lat(:, :), lon(:, :), &
!    !                                    varSeq0(:), varSeq(:), &
!    !                                    xTime(:), xGridID(:, :)

!    !    INTEGER :: idVar(iVarStart:SIZE(varList))
!    !    CHARACTER(len=50):: header_str, longNm_str, unit_str
!    !    CHARACTER(len=4)  :: yrStr2
!    !    CHARACTER(len=40) :: startStr2
!    !    REAL(KIND(1d0)) :: minLat, maxLat, dLat, minLon, maxLon, dLon
!    !    REAL(KIND(1d0)), DIMENSION(1:6) :: geoTrans
!    !    CHARACTER(len=80) :: strGeoTrans

!    !    ! determine number of times
!    !    nTime = SIZE(dataOutX, dim=1)

!    !    !select variables to output
!    !    nVar = COUNT((varList%level <= outLevel), dim=1)
!    !    ALLOCATE (varListSel(nVar), stat=err)
!    !    IF (err /= 0) PRINT *, "varListSel: Allocation request denied"
!    !    varListSel = PACK(varList, mask=(varList%level <= outLevel))

!    !    ! copy data accordingly
!    !    ALLOCATE (dataOutSel(nTime, nVar, NumberOfGrids), stat=err)
!    !    IF (err /= 0) PRINT *, "dataOutSel: Allocation request denied"
!    !    dataOutSel = dataOutX(:, PACK((/(i, i=1, SIZE(varList))/), varList%level <= outLevel), :)

!    !    ! determine filename
!    !    CALL filename_gen(dataOutSel(:, :, 1), varListSel, 1, FileOut)
!    !    ! PRINT*, 'writing file:',TRIM(fileOut)

!    !    ! set year string
!    !    WRITE (yrStr2, '(i4)') INT(dataOutX(1, 1, 1))
!    !    ! get start for later time unit creation
!    !    startStr2 = TRIM(yrStr2)//'-01-01 00:00:00'

!    !    ! define the dimension of spatial array/frame in the output
!    !    nX = nCol
!    !    nY = nRow

!    !    ALLOCATE (varSeq0(nX*nY))
!    !    ALLOCATE (varSeq(nX*nY))
!    !    ALLOCATE (xGridID(nX, nY))
!    !    ALLOCATE (lon(nX, nY))
!    !    ALLOCATE (lat(nX, nY))
!    !    ALLOCATE (varY(nX, nY))
!    !    ALLOCATE (varX(nX, nY))
!    !    ALLOCATE (xTime(nTime))

!    !    ! GridID:
!    !    varSeq = SurfaceChar(1:nX*nY, 1)
!    !    ! CALL sortSeqReal(varSeq0,varSeq,nY,nX)
!    !    xGridID = RESHAPE(varSeq, (/nX, nY/), order=(/1, 2/))
!    !    ! PRINT*, 'before flipping:',lat(1:2,1)
!    !    xGridID = xGridID(:, nY:1:-1)

!    !    ! latitude:
!    !    varSeq = SurfaceChar(1:nX*nY, 5)
!    !    ! CALL sortSeqReal(varSeq0,varSeq,nY,nX)
!    !    lat = RESHAPE(varSeq, (/nX, nY/), order=(/1, 2/))
!    !    ! PRINT*, 'before flipping:',lat(1:2,1)
!    !    lat = lat(:, nY:1:-1)
!    !    ! PRINT*, 'after flipping:',lat(1:2,1)

!    !    ! longitude:
!    !    varSeq = SurfaceChar(1:nX*nY, 6)
!    !    ! CALL sortSeqReal(varSeq0,varSeq,nY,nX)
!    !    lon = RESHAPE(varSeq, (/nX, nY/), order=(/1, 2/))
!    !    lon = lon(:, nY:1:-1)

!    !    ! pass values to coordinate variables
!    !    varY = lat
!    !    varX = lon

!    !    ! calculate GeoTransform array as needed by GDAL
!    !    ! ref: http://www.perrygeo.com/python-affine-transforms.html
!    !    ! the values below are different from the above ref,
!    !    ! as the layout of SUEWS output is different from the schematic shown there
!    !    ! SUEWS output is arranged northward down the page
!    !    ! if data are formatted as a normal matrix
!    !    minLat = lat(1, 1)               ! the lower-left pixel
!    !    maxLat = lat(1, NY)              ! the upper-left pixel
!    !    IF (nY > 1) THEN
!    !       dLat = (maxLat - minLat)/(nY - 1) ! height of a pixel
!    !    ELSE
!    !       dLat = 1
!    !    END IF

!    !    ! PRINT*, 'lat:',minLat,maxLat,dLat
!    !    minLon = lon(1, 1)              ! the lower-left pixel
!    !    maxLon = lon(NX, 1)             ! the lower-right pixel
!    !    IF (nY > 1) THEN
!    !       dLon = (maxLon - minLon)/(nX - 1) ! width of a pixel
!    !    ELSE
!    !       dLon = 1
!    !    END IF

!    !    ! PRINT*, 'lon:',minLon,maxLon,dLon
!    !    geoTrans(1) = minLon - dLon/2          ! x-coordinate of the lower-left corner of the lower-left pixel
!    !    geoTrans(2) = dLon                   ! width of a pixel
!    !    geoTrans(3) = 0.                     ! row rotation (typically zero)
!    !    geoTrans(4) = minLat - dLat/2          ! y-coordinate of the of the lower-left corner of the lower-left pixel
!    !    geoTrans(5) = 0.                     ! column rotation (typically zero)
!    !    geoTrans(6) = dLat                   ! height of a pixel (typically negative, but here positive)
!    !    ! write GeoTransform to strGeoTrans
!    !    WRITE (strGeoTrans, '(6(f12.8,1x))') geoTrans

!    !    ! Create the netCDF file. The nf90_clobber parameter tells netCDF to
!    !    ! overwrite this file, if it already exists.
!    !    CALL check(nf90_create(TRIM(fileOut), NF90_CLOBBER, ncID))

!    !    ! put global attributes
!    !    CALL check(nf90_put_att(ncID, NF90_GLOBAL, 'Conventions', 'CF1.6'))
!    !    CALL check(nf90_put_att(ncID, NF90_GLOBAL, 'title', 'SUEWS output'))
!    !    CALL check(nf90_put_att(ncID, NF90_GLOBAL, 'source', 'Micromet Group, University of Reading'))
!    !    CALL check(nf90_put_att(ncID, NF90_GLOBAL, 'references', 'http://urban-climate.net/umep/SUEWS'))

!    !    ! Define the dimensions. NetCDF will hand back an ID for each.
!    !    ! nY = ncolumnsDataOutSUEWS-4
!    !    ! nx = NumberOfGrids
!    !    CALL check(nf90_def_dim(ncID, "time", NF90_UNLIMITED, time_dimid))
!    !    CALL check(nf90_def_dim(ncID, "west_east", NX, x_dimid))
!    !    CALL check(nf90_def_dim(ncID, "south_north", NY, y_dimid))
!    !    ! PRINT*, 'good define dim'

!    !    ! The dimids array is used to pass the IDs of the dimensions of
!    !    ! the variables. Note that in fortran arrays are stored in
!    !    ! column-major format.
!    !    dimids = (/x_dimid, y_dimid, time_dimid/)

!    !    ! write out each variable
!    !    ALLOCATE (varOut(nX, nY, nTime))

!    !    ! define all variables
!    !    ! define time variable:
!    !    CALL check(nf90_def_var(ncID, 'time', NF90_REAL, time_dimid, varIDt))
!    !    CALL check(nf90_put_att(ncID, varIDt, 'units', 'minutes since '//startStr2))
!    !    CALL check(nf90_put_att(ncID, varIDt, 'long_name', 'time'))
!    !    CALL check(nf90_put_att(ncID, varIDt, 'standard_name', 'time'))
!    !    CALL check(nf90_put_att(ncID, varIDt, 'calendar', 'gregorian'))
!    !    CALL check(nf90_put_att(ncID, varIDt, 'axis', 'T'))

!    !    ! define coordinate variables:
!    !    CALL check(nf90_def_var(ncID, 'lon', NF90_REAL, (/x_dimid, y_dimid/), varIDx))
!    !    CALL check(nf90_put_att(ncID, varIDx, 'units', 'degree_east'))
!    !    CALL check(nf90_put_att(ncID, varIDx, 'long_name', 'longitude'))
!    !    CALL check(nf90_put_att(ncID, varIDx, 'standard_name', 'longitude'))
!    !    CALL check(nf90_put_att(ncID, varIDx, 'axis', 'X'))

!    !    CALL check(nf90_def_var(ncID, 'lat', NF90_REAL, (/x_dimid, y_dimid/), varIDy))
!    !    CALL check(nf90_put_att(ncID, varIDy, 'units', 'degree_north'))
!    !    CALL check(nf90_put_att(ncID, varIDy, 'long_name', 'latitude'))
!    !    CALL check(nf90_put_att(ncID, varIDy, 'standard_name', 'latitude'))
!    !    CALL check(nf90_put_att(ncID, varIDy, 'axis', 'Y'))

!    !    ! define coordinate referencing system:
!    !    CALL check(nf90_def_var(ncID, 'crsWGS84', NF90_INT, varIDCRS))
!    !    CALL check(nf90_put_att(ncID, varIDCRS, 'grid_mapping_name', 'latitude_longitude'))
!    !    CALL check(nf90_put_att(ncID, varIDCRS, 'long_name', 'CRS definition'))
!    !    CALL check(nf90_put_att(ncID, varIDCRS, 'longitude_of_prime_meridian', '0.0'))
!    !    CALL check(nf90_put_att(ncID, varIDCRS, 'semi_major_axis', '6378137.0'))
!    !    CALL check(nf90_put_att(ncID, varIDCRS, 'inverse_flattening', '298.257223563'))
!    !    CALL check(nf90_put_att(ncID, varIDCRS, 'epsg_code', 'EPSG:4326'))
!    !    CALL check(nf90_put_att(ncID, varIDCRS, 'GeoTransform', TRIM(strGeoTrans)))
!    !    CALL check(nf90_put_att(ncID, varIDCRS, 'spatial_ref',&
!    !         &'GEOGCS["WGS 84",&
!    !         &    DATUM["WGS_1984",&
!    !         &        SPHEROID["WGS 84",6378137,298.257223563,&
!    !         &            AUTHORITY["EPSG","7030"]],&
!    !         &        AUTHORITY["EPSG","6326"]],&
!    !         &    PRIMEM["Greenwich",0],&
!    !         &    UNIT["degree",0.0174532925199433],&
!    !         &    AUTHORITY["EPSG","4326"]]' &
!    !         ))

!    !    ! define grid_ID:
!    !    CALL check(nf90_def_var(ncID, 'grid_ID', NF90_INT, (/x_dimid, y_dimid/), varIDGrid))
!    !    CALL check(nf90_put_att(ncID, varIDGrid, 'coordinates', 'lon lat'))
!    !    CALL check(nf90_put_att(ncID, varIDGrid, 'long_name', 'Grid ID as in SiteSelect'))
!    !    CALL check(nf90_put_att(ncID, varIDGrid, 'grid_mapping', 'crsWGS84'))
!    !    ! varIDGrid=varID

!    !    ! define other 3D variables:
!    !    DO iVar = iVarStart, nVar
!    !       ! define variable name
!    !       header_str = varListSel(iVar)%header
!    !       unit_str = varListSel(iVar)%unit
!    !       longNm_str = varListSel(iVar)%longNm

!    !       ! Define the variable. The type of the variable in this case is
!    !       ! NF90_REAL.

!    !       CALL check(nf90_def_var(ncID, TRIM(ADJUSTL(header_str)), NF90_REAL, dimids, varID))

!    !       CALL check(nf90_put_att(ncID, varID, 'coordinates', 'lon lat'))

!    !       CALL check(nf90_put_att(ncID, varID, 'units', TRIM(ADJUSTL(unit_str))))

!    !       CALL check(nf90_put_att(ncID, varID, 'long_name', TRIM(ADJUSTL(longNm_str))))

!    !       CALL check(nf90_put_att(ncID, varID, 'grid_mapping', 'crsWGS84'))

!    !       idVar(iVar) = varID
!    !    END DO
!    !    CALL check(nf90_enddef(ncID))
!    !    ! End define mode. This tells netCDF we are done defining metadata.

!    !    ! put all variable values into netCDF datasets
!    !    ! put time variable in minute:
!    !    xTime = (dataOutSel(1:nTime, 2, 1) - 1)*24*60 + dataOutSel(1:nTime, 3, 1)*60 + dataOutSel(1:nTime, 4, 1)
!    !    CALL check(nf90_put_var(ncID, varIDt, xTime))

!    !    ! put coordinate variables:
!    !    CALL check(nf90_put_var(ncID, varIDx, varX))
!    !    CALL check(nf90_put_var(ncID, varIDy, varY))

!    !    ! put CRS variable:
!    !    CALL check(nf90_put_var(ncID, varIDCRS, 9999))

!    !    CALL check(NF90_SYNC(ncID))
!    !    ! PRINT*, 'good put var'

!    !    ! put grid_ID:
!    !    CALL check(nf90_put_var(ncID, varIDGrid, xGridID))
!    !    ! PRINT*, 'good put varIDGrid',varIDGrid

!    !    CALL check(NF90_SYNC(ncID))

!    !    ! then other 3D variables
!    !    DO iVar = iVarStart, nVar
!    !       ! reshape dataOutX to be aligned in checker board form
!    !       varOut = RESHAPE(dataOutSel(1:nTime, iVar, :), (/nX, nY, nTime/), order=(/3, 1, 2/))
!    !       varOut = varOut(:, nY:1:-1, :)
!    !       !  get the variable id
!    !       varID = idVar(iVar)

!    !       CALL check(nf90_put_var(ncID, varID, varOut))

!    !       CALL check(NF90_SYNC(ncID))
!    !    END DO

!    !    IF (ALLOCATED(varOut)) DEALLOCATE (varOut)
!    !    IF (ALLOCATED(varSeq0)) DEALLOCATE (varSeq0)
!    !    IF (ALLOCATED(varSeq)) DEALLOCATE (varSeq)
!    !    IF (ALLOCATED(xGridID)) DEALLOCATE (xGridID)
!    !    IF (ALLOCATED(lon)) DEALLOCATE (lon)
!    !    IF (ALLOCATED(lat)) DEALLOCATE (lat)
!    !    IF (ALLOCATED(varY)) DEALLOCATE (varY)
!    !    IF (ALLOCATED(varX)) DEALLOCATE (varX)
!    !    IF (ALLOCATED(xTime)) DEALLOCATE (xTime)

!    !    ! Close the file. This frees up any internal netCDF resources
!    !    ! associated with the file, and flushes any buffers.
!    !    CALL check(nf90_close(ncID))

!    !    ! PRINT*, "*** SUCCESS writing netCDF file:"
!    !    ! PRINT*, FileOut
!    ! END SUBROUTINE SUEWS_Write_nc

!    !===========================================================================!
!    ! convert a vector of grids to a matrix
!    ! the grid IDs in seqGrid2Sort follow the QGIS convention
!    ! the spatial matrix arranges successive rows down the page (i.e., north to south)
!    !   and succesive columns across (i.e., west to east)
!    ! seqGridSorted stores the grid IDs as aligned in matGrid but squeezed into a vector
!    !===========================================================================!
!    SUBROUTINE grid2mat(seqGrid2Sort, seqGridSorted, matGrid, nRow, nCol)

!       IMPLICIT NONE

!       INTEGER, DIMENSION(nRow*nCol) :: seqGrid2Sort, seqGridSorted
!       INTEGER, DIMENSION(nRow, nCol) :: matGrid
!       INTEGER :: nRow, nCol, i, j, loc

!       CALL sortGrid(seqGrid2Sort, seqGridSorted, nRow, nCol)
!       PRINT *, 'old:'
!       PRINT *, seqGrid2Sort(1:5)
!       PRINT *, 'sorted:'
!       PRINT *, seqGridSorted(1:5)
!       PRINT *, ''
!       DO i = 1, nRow
!          DO j = 1, nCol
!             loc = (i - 1)*nCol + j
!             matGrid(i, j) = seqGridSorted(loc)
!          END DO
!       END DO
!    END SUBROUTINE grid2mat

!    !===========================================================================!
!    ! convert sequence of REAL values to a matrix
!    ! the grid IDs in seqGrid2Sort follow the QGIS convention
!    ! the spatial matrix arranges successive rows down the page (i.e., north to south)
!    !   and succesive columns across (i.e., west to east)
!    ! seqGridSorted stores the grid IDs as aligned in matGrid but squeezed into a vector
!    !===========================================================================!
!    SUBROUTINE seq2mat(seq2Sort, seqSorted, matGrid, nRow, nCol)

!       IMPLICIT NONE

!       REAL(KIND(1d0)), DIMENSION(nRow*nCol) :: seq2Sort, seqSorted
!       REAL(KIND(1d0)), DIMENSION(nRow, nCol) :: matGrid
!       INTEGER :: nRow, nCol, i, j, loc

!       CALL sortSeqReal(seq2Sort, seqSorted, nRow, nCol)
!       PRINT *, 'old:'
!       PRINT *, seq2Sort(1:5)
!       PRINT *, 'sorted:'
!       PRINT *, seqSorted(1:5)
!       PRINT *, ''
!       DO i = 1, nRow
!          DO j = 1, nCol
!             loc = (i - 1)*nCol + j
!             matGrid(i, j) = seqSorted(loc)
!          END DO
!       END DO
!    END SUBROUTINE seq2mat

!    !===========================================================================!
!    ! sort a sequence of LONG values into the specially aligned sequence per QGIS
!    !===========================================================================!
!    SUBROUTINE sortGrid(seqGrid2Sort0, seqGridSorted, nRow, nCol)
!       USE qsort_c_module
!       ! convert a vector of grids to a matrix
!       ! the grid IDs in seqGrid2Sort follow the QGIS convention
!       ! the spatial matrix arranges successive rows down the page (i.e., north to south)
!       !   and succesive columns across (i.e., west to east)
!       ! seqGridSorted stores the grid IDs as aligned in matGrid but squeezed into a vector

!       IMPLICIT NONE
!       INTEGER :: nRow, nCol, i = 1, j = 1, xInd, len

!       INTEGER, DIMENSION(nRow*nCol), INTENT(in) :: seqGrid2Sort0
!       INTEGER, DIMENSION(nRow*nCol), INTENT(out) :: seqGridSorted
!       INTEGER, DIMENSION(nRow*nCol) :: seqGrid2Sort, locSorted
!       INTEGER :: loc
!       REAL:: ind(nRow*nCol, 2)
!       REAL, DIMENSION(nRow*nCol) :: seqGrid2SortReal, seqGridSortedReal
!       REAL :: val

!       ! number of grids
!       len = nRow*nCol

!       !sort the input array to make sure the grid order is in QGIS convention
!       ! i.e., diagonally ascending
!       seqGrid2SortReal = seqGrid2Sort0*1.
!       CALL QsortC(seqGrid2SortReal)
!       seqGrid2Sort = INT(seqGrid2SortReal)

!       ! fill in an nRow*nCol array with values to determine sequence
!       xInd = 1
!       DO i = 1, nRow
!          DO j = 1, nCol
!             !  {row, col, value for sorting, index in new sequence}
!             ind(xInd, :) = (/i + j + i/(nRow + 1.), xInd*1./)
!             xInd = xInd + 1
!          END DO
!       END DO

!       ! then sorted ind(:,3) will have the same order as seqGrid2Sort
!       ! sort ind(:,3)
!       seqGridSortedReal = ind(:, 1)*1.
!       CALL QsortC(seqGridSortedReal)
!       ! print*, 'sorted real:'
!       ! print*, seqGridSortedReal

!       ! get index of each element of old sequence in the sorted sequence
!       DO i = 1, len
!          ! value in old sequence
!          !  val=ind(i,3)*1.
!          val = seqGridSortedReal(i)
!          DO j = 1, len
!             IF (val == ind(j, 1)*1.) THEN
!                ! location in sorted sequence
!                locSorted(i) = j
!             END IF
!          END DO
!       END DO

!       ! put elements of old sequence in the sorted order
!       DO i = 1, len
!          loc = locSorted(i)
!          seqGridSorted(loc) = seqGrid2Sort(i)
!       END DO
!       seqGridSorted = seqGridSorted(len:1:-1)

!    END SUBROUTINE sortGrid

!    !===========================================================================!
!    ! sort a sequence of REAL values into the specially aligned sequence per QGIS
!    !===========================================================================!
!    SUBROUTINE sortSeqReal(seqReal2Sort, seqRealSorted, nRow, nCol)
!       USE qsort_c_module
!       ! convert a vector of grids to a matrix
!       ! the grid IDs in seqReal2Sort follow the QGIS convention
!       ! the spatial matrix arranges successive rows down the page (i.e., north to south)
!       !   and succesive columns across (i.e., west to east)
!       ! seqRealSorted stores the grid IDs as aligned in matGrid but squeezed into a vector

!       IMPLICIT NONE
!       INTEGER :: nRow, nCol, i = 1, j = 1, xInd, len

!       REAL(KIND(1d0)), DIMENSION(nRow*nCol), INTENT(in) :: seqReal2Sort
!       REAL(KIND(1d0)), DIMENSION(nRow*nCol), INTENT(out) :: seqRealSorted
!       INTEGER(KIND(1d0)), DIMENSION(nRow*nCol) :: locSorted
!       INTEGER(KIND(1d0)) :: loc
!       REAL:: ind(nRow*nCol, 2)
!       REAL :: seqRealSortedReal(nRow*nCol), val

!       ! number of grids
!       len = nRow*nCol

!       ! fill in an nRow*nCol array with values to determine sequence
!       xInd = 1
!       DO i = 1, nRow
!          DO j = 1, nCol
!             !  {row, col, value for sorting, index in new sequence}
!             ind(xInd, :) = (/i + j + i/(nRow + 1.), xInd*1./)
!             xInd = xInd + 1
!          END DO
!       END DO

!       ! then sorted ind(:,3) will have the same order as seqReal2Sort
!       ! sort ind(:,3)
!       seqRealSortedReal = ind(:, 1)*1.
!       CALL QsortC(seqRealSortedReal)
!       ! print*, 'sorted real:'
!       ! print*, seqRealSortedReal

!       ! get index of each element of old sequence in the sorted sequence
!       DO i = 1, len
!          ! value in old sequence
!          !  val=ind(i,3)*1.
!          val = seqRealSortedReal(i)
!          DO j = 1, len
!             IF (val == ind(j, 1)*1.) THEN
!                ! location in sorted sequence
!                locSorted(i) = j
!             END IF
!          END DO
!       END DO

!       ! put elements of old sequence in the sorted order
!       DO i = 1, len
!          loc = locSorted(i)
!          seqRealSorted(loc) = seqReal2Sort(i)
!       END DO
!       seqRealSorted = seqRealSorted(len:1:-1)

!    END SUBROUTINE sortSeqReal

!    !===========================================================================!
!    ! a wrapper for checking netCDF status
!    !===========================================================================!

!    SUBROUTINE check(status)
!       USE netcdf
!       IMPLICIT NONE

!       INTEGER, INTENT(in) :: status

!       IF (status /= nf90_noerr) THEN
!          PRINT *, TRIM(nf90_strerror(status))
!          STOP "Stopped"
!       END IF
!    END SUBROUTINE check
! #endif

END MODULE ctrl_output
