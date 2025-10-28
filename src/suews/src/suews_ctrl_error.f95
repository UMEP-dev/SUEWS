SUBROUTINE ErrorHint(errh, ProblemFile, VALUE, value2, valueI)
   !errh        -- Create a numbered code for the situation so get a unique message to help solve the problem
   !ProblemFile -- Filename where the problem occurs/error message
   !value       -- Error value (real number with correct type)
   !value2      -- Second error value (real number with correct type)
   !valueI      -- Error value (integer)
   ! Last modified -----------------------------------------------------
   ! MH  12 Apr 2017: Error code for stability added
   ! HCW 17 Feb 2017: Write (serious) errors to problems.txt; write warnings to warnings.txt (program continues)
   ! HCW 13 Dec 2016: Tidied up and improved error hints
   ! HCW 25 May 2016: Added warning/error labels to distinguish serious errors (that stop program)
   ! LJ  02 Oct 2014: addition of comments
   ! sg  29 Jul 2014: close (500)
   ! LJ  08 Feb 2013
   !--------------------------------------------------------------------

   USE data_in
   USE defaultNotUsed
   ! USE WhereWhen

   IMPLICIT NONE

   REAL(KIND(1D0)) :: VALUE, value2

   CHARACTER(len=*) :: ProblemFile ! Name of the problem file
   CHARACTER(len=150) :: text1 ! Initialization of text
   ! CHARACTER(len=20)::filename                  !file name for writting out error info
   INTEGER :: errh, ValueI, ValueI2, ValueI3 ! v7,v8 initialised as false, HCW 28/10/2014
   INTEGER, DIMENSION(80) :: ErrhCount = 0 ! Counts each time a error hint is called. Initialise to zero
   ! INTEGER:: WhichFile                            ! Used to switch between 500 for error file, 501 for warnings file
#ifdef wrf
   CHARACTER(len=1024) :: message ! Used to pass through function wrf_debug() by Zhenkun Li, 10/08/2018
#endif
   CHARACTER(len=1024) :: Errmessage
   CHARACTER(len=1024) :: StopMessage ! used to pass error message to  statement; useful to supy_driver, TS 19 Feb 2019

   ! TS 16 Jul 2018:
   ! these LOGICAL values should NOT be initialised as  is implied
   ! which will cause cross-assignment in parallel mode and thus subsequent unintentional STOP
   LOGICAL :: v1, v2, v3, v4, v5, v6, v7, v8, v9
   LOGICAL :: flag_continue_on_error

   text1 = 'unknown problem' ! Initialization of text
   ! WhichFile = 0 ! Initialization of file code

   ! Initialise returnTrue as false (HCW 29/10/2014)
   ! - need to do this in Fortran as values assigned in declarations are not applied
   ! on subsequent calling of the subroutine
   flag_continue_on_error = .FALSE.
   ! Initialise v1-v8 as false
   v1 = .FALSE.
   v2 = .FALSE.
   v3 = .FALSE.
   v4 = .FALSE.
   v5 = .FALSE.
   v6 = .FALSE.
   v7 = .FALSE.
   v8 = .FALSE.
   v9 = .FALSE.

   !CALL gen_ProblemsText(ProblemFile)   !Call the subroutine that opens the problem.txt file !Moved below, HCW 17 Feb 2017

   !The list of knows possible problems of the code:
   !  text1 is the error message written to the ProblemFile.
   !  v1 -v7 are different possibilities for what numbers will be written out
   !  flag_continue_on_error is true if the model run can continue. (Comment modified by HCW 17/10/2014)
   IF (errh == 1) THEN
      text1 = 'Check value in SUEWS_SiteSelect.txt.'
      v5 = .TRUE.
   ELSEIF (errh == 2) THEN
      text1 = 'Cannot perform disaggregation.'
      v6 = .TRUE.
   ELSEIF (errh == 3) THEN
      text1 = 'Met forcing file should contain only 1 year of data.'
      v1 = .TRUE.
   ELSEIF (errh == 4) THEN
      text1 = 'Rainfall in original met forcing file exceeds intensity threshold.'
      v2 = .TRUE.
      flag_continue_on_error = .TRUE.
      !5
   ELSEIF (errh == 6) THEN
      text1 = 'Value obtained exceeds permitted range, setting to +/-9999 in output file.'
      v1 = .TRUE.
      flag_continue_on_error = .TRUE.
   ELSEIF (errh == 7) THEN
      text1 = 'RA value obtained exceeds permitted range.'
      v1 = .TRUE.
      flag_continue_on_error = .TRUE.
   ELSEIF (errh == 8) THEN
      text1 = 'Check values in SUEWS_WithinGridWaterDist.txt.'
      v1 = .TRUE.
   ELSEIF (errh == 9) THEN
      text1 = 'Check ToRunoff and ToSoilStore values in SUEWS_WithinGridWaterDist.txt.'
      v2 = .TRUE.
   ELSEIF (errh == 10) THEN
      text1 = 'Check values in SUEWS_SiteSelect.txt.'
      v1 = .TRUE.
   ELSEIF (errh == 11) THEN
      text1 = 'File not found.'
      v3 = .TRUE.
      ! 12
   ELSEIF (errh == 13) THEN
      text1 = 'Check met forcing file.'
      v8 = .TRUE.
   ELSEIF (errh == 14) THEN
      text1 = 'Inappropriate value calculated.'
      v1 = .TRUE.
   ELSEIF (errh == 15) THEN
      text1 = 'Check H_Bldgs, H_EveTr and H_DecTr in SUEWS_SiteSelect.txt'
      v2 = .TRUE.
      flag_continue_on_error = .TRUE.
      ! 16
   ELSEIF (errh == 17) THEN
      text1 = 'Problem with (z-zd) and/or z0.'
      v2 = .TRUE.
   ELSEIF (errh == 18) THEN
      text1 = 'Check soil depth relative to soil moisture and capacity.'
      v4 = .TRUE.
   ELSEIF (errh == 19) THEN
      text1 = 'Caution - check range.'
      v4 = .TRUE.
      flag_continue_on_error = .TRUE.
   ELSEIF (errh == 20) THEN
      text1 = ' skip lines, ios_out.'
      v5 = .TRUE.
   ELSEIF (errh == 21) THEN
      text1 = 'Bad input for OHM/AnOHM storage heat flux calculation. Check qn, qn_Sn, qf for issues.'
      v1 = .TRUE.
   ELSEIF (errh == 22) THEN
      text1 = ' QH_observed, QE_observed, QH_choice: '
      v4 = .TRUE.
      flag_continue_on_error = .TRUE.
   ELSEIF (errh == 23) THEN
      text1 = 'CBL-sonde -need to increase size of izm:zmax,izm'
      v5 = .TRUE.
   ELSEIF (errh == 24) THEN
      text1 = 'CBL file problem - opening'
      v8 = .TRUE.
   ELSEIF (errh == 25) THEN
      text1 = 'CBL file problem -- reading sonde data, line:'
      v3 = .TRUE.
   ELSEIF (errh == 26) THEN
      text1 = 'Check that FileCode, FileInputPath and FileOutputPath are specified correctly in RunControl.nml.'
      v8 = .TRUE.
   ELSEIF (errh == 27) THEN
      text1 = 'Problems with Met data -forcing data: variable value, dectime'
      v2 = .TRUE. ! 2 real
   ELSEIF (errh == 28) THEN
      text1 = 'Processing in subroutine indicated has a problem, variables'
      flag_continue_on_error = .TRUE.
      v3 = .TRUE. ! 1 integer
   ELSEIF (errh == 29) THEN
      text1 = 'Processing in subroutine indicated has a problem, time, variables'
      flag_continue_on_error = .TRUE.
      v7 = .TRUE. ! 1 real, 2 integers
   ELSEIF (errh == 30) THEN
      text1 = 'Processing in subroutine indicated has a problem, time, variables'
      ! returntrue=.TRUE.
      v2 = .TRUE. ! 2 real
   ELSEIF (errh == 31) THEN
      text1 = 'Processing in subroutine indicated has a problem, time, variables'
      flag_continue_on_error = .TRUE.
      v1 = .TRUE. ! 1 real
   ELSEIF (errh == 32) THEN
      text1 = 'Model applicable to local scale, z<z0d'
      v2 = .TRUE. ! 2 real
   ELSEIF (errh == 33) THEN
      text1 = 'Number of snow layers too large.'
      v1 = .TRUE. ! 1 real
   ELSEIF (errh == 34) THEN
      text1 = 'Air temperature > 55 C -- will keep running'
      v1 = .TRUE. ! 1 real
      flag_continue_on_error = .TRUE.
   ELSEIF (errh == 35) THEN
      text1 = 'Problems with Met data -forcing data: doy, dectime'
      v2 = .TRUE. ! 2 real
   ELSEIF (errh == 36) THEN
      text1 = 'Problem found in InitialConditions file!'
      v8 = .TRUE.
   ELSEIF (errh == 37) THEN
      text1 = 'Check inputs in InitialConditions file!'
      flag_continue_on_error = .TRUE.
      v2 = .TRUE. !2 real
   ELSEIF (errh == 38) THEN
      text1 = 'H=(qn*0.2)/(avdens*avcp)'
      flag_continue_on_error = .TRUE.
      v1 = .TRUE. !2 real
   ELSEIF (errh == 39) THEN
      text1 = 'Different value of TSTEP needed (300 s recommended). Resolution of forcing data must match TSTEP set in RunControl.'
      v4 = .TRUE. !2 real, 1 int
   ELSEIF (errh == 40) THEN
      text1 = 'SOLWEIG file problem - opening'
      v8 = .TRUE.
   ELSEIF (errh == 41) THEN
      text1 = ' addwaterbody= Error1-- but watersurf=  Error 2'
      v2 = .TRUE. !2 real
   ELSEIF (errh == 42) THEN
      text1 = 'abs(rho_d)<0.001000.OR.abs(rho_v)<0.001000.OR.abs(rho_d+rho_v)<0.001000) rho_v,rho_d, T'
      flag_continue_on_error = .TRUE.
      v4 = .TRUE. !2 real, temperature as an integer
   ELSEIF (errh == 43) THEN
      text1 = 'Switching Years - will keep running'
      flag_continue_on_error = .TRUE.
      v8 = .TRUE.
   ELSEIF (errh == 44) THEN
      text1 = 'Initial File Name - will keep going'
      flag_continue_on_error = .TRUE.
      v8 = .TRUE.
   ELSEIF (errh == 45) THEN
      text1 = 'Pressure < 900 hPa, Loop Number'
      flag_continue_on_error = .TRUE.
      v5 = .TRUE.
   ELSEIF (errh == 46) THEN
      text1 = 'Pressure < 900 hPa'
      flag_continue_on_error = .TRUE.
      v1 = .TRUE.
   ELSEIF (errh == 47) THEN
      text1 = 'File missing'
      !returntrue = .TRUE.
   ELSEIF (errh == 48) THEN
      text1 = 'Something wrong in the rows of the file'
      !returntrue = .TRUE.
   ELSEIF (errh == 49) THEN
      text1 = 'Problems in saving to InitialConditionsYYYY.nml'
   ELSEIF (errh == 50) THEN
      text1 = 'Wrong number of lines read: nsurf, [-1 EOF; -2 EOR]'
      v1 = .TRUE.
   ELSEIF (errh == 51) THEN
      text1 = 'Problems in opening the file'
      WRITE (*, *) ProblemFile
   ELSEIF (errh == 52) THEN
      text1 = 'Problems opening the output file.'
   ELSEIF (errh == 53) THEN
      text1 = 'AH_min=0.and.Ah_slope=0.and.T_Critic=0, AnthropHeatMethod='
      flag_continue_on_error = .TRUE.
      v3 = .TRUE.
   ELSEIF (errh == 54) THEN
      text1 = 'QF_A=0.and.QF_B=0.and.QF_C=0, AnthropHeatMethod='
      flag_continue_on_error = .TRUE.
      v3 = .TRUE.
   ELSEIF (errh == 55) THEN
      text1 = 'InputmetFormat='
      flag_continue_on_error = .TRUE.
      v3 = .TRUE.
   ELSEIF (errh == 56) THEN
      text1 = 'Check input files against manual (N.B. Case sensitive).'
      v8 = .TRUE.
   ELSEIF (errh == 57) THEN
      text1 = 'not found. Check input files.'
      v1 = .TRUE.
   ELSEIF (errh == 58) THEN
      text1 = 'File header not specified in model code.'
      v8 = .TRUE.
   ELSEIF (errh == 59) THEN
      text1 = 'not found. Check SUEWS_SiteSelect.txt.'
      v6 = .TRUE.
   ELSEIF (errh == 60) THEN
      text1 = 'non-unique code.'
      v1 = .TRUE.
   ELSEIF (errh == 61) THEN
      text1 = 'Check coefficients and drainage equation specified in input files.'
      v4 = .TRUE.
      flag_continue_on_error = .TRUE.
   ELSEIF (errh == 62) THEN
      text1 = 'Problem with soil moisture calculation.'
      v5 = .TRUE.
   ELSEIF (errh == 63) THEN
      text1 = 'Problem with calculation.'
      v1 = .TRUE.
   ELSEIF (errh == 64) THEN
      text1 = 'SUEWS cannot currently handle this many grids.'
      v6 = .TRUE.
   ELSEIF (errh == 65) THEN
      text1 = 'Negative gs calculated! Check suitability of parameters in SUEWS_Conductance.txt.'
      flag_continue_on_error = .TRUE.
      v7 = .TRUE.
   ELSEIF (errh == 66) THEN
      text1 = 'Different number of lines in ESTM forcing and Met forcing files.'
      v6 = .TRUE.
   ELSEIF (errh == 67) THEN
      text1 = 'ESTMClass1 automatically set to 100%.'
      flag_continue_on_error = .TRUE.
      v1 = .TRUE.
   ELSEIF (errh == 68) THEN
      text1 = 'Initial Bowen ratio automatically set to 1.'
      flag_continue_on_error = .TRUE.
      v1 = .TRUE.
   ELSEIF (errh == 69) THEN
      text1 = 'Setting QF_traff to zero. Check input data.'
      flag_continue_on_error = .TRUE.
      v2 = .TRUE.
   ELSEIF (errh == 70) THEN
      text1 = 'Specify profile values between 1 (night) and 2 (day).'
      v8 = .TRUE.
   ELSEIF (errh == 71) THEN
      text1 = 'Check input file SUEWS_Conductance.txt.'
      v3 = .TRUE.
   ELSEIF (errh == 72) THEN
      text1 = 'RunControl.nml: ResolutionFilesOut must be an integer multiple of TSTEP'
      v6 = .TRUE.
   ELSEIF (errh == 73) THEN
      text1 = 'Iteration loop stopped for too stable conditions.'
      ! returnTrue=.TRUE.
      v2 = .TRUE.
   ELSEIF (errh == 74) THEN
      text1 = 'Iteration loop stopped for too unstable conditions.'
      ! returnTrue=.TRUE.
      v2 = .TRUE.
   ELSEIF (errh == 75) THEN
      text1 = 'Invalid value for !'
      ! returnTrue=.TRUE.
      v2 = .TRUE.
   ELSEIF (errh == 76) THEN
      text1 = 'erroneous T2 by RSL!'
      flag_continue_on_error = .FALSE.
      v4 = .TRUE.
   ELSEIF (errh == 77) THEN
      text1 = 'CFL failure in ESTM_ext!' &
              //' dz/k should be < 0.1; ' &
              //'current value dz/k for facet i of surf j in group k is '

      flag_continue_on_error = .FALSE.
      v7 = .TRUE.
   END IF

   ErrhCount(errh) = ErrhCount(errh) + 1 ! Increase error count by 1
   ! PRINT*, 'returnTrue',returnTrue

   !---------------------------------------------------------------------
   !This part of the code determines how the error/warning message is written out
   IF (v1) THEN ! 1 real
      WRITE (Errmessage, '(a,f9.4)') ' Value: ', VALUE
   ELSEIF (v2) THEN ! 2 real
      WRITE (Errmessage, '(a,2f9.4)') ' Values: ', VALUE, value2
   ELSEIF (v3) THEN ! 1 integer
      WRITE (Errmessage, '(a,i10)') ' Value: ', valueI
   ELSEIF (v4) THEN ! 2 real, 1 integer
      WRITE (Errmessage, '(a,2f12.4,i10)') ' Values: ', VALUE, value2, valueI
   ELSEIF (v5) THEN ! 1 real 1 integer
      WRITE (Errmessage, '(a,f9.4,i10)') ' Values: ', VALUE, valueI
   ELSEIF (v6) THEN ! 2 integer
      valueI2 = INT(VALUE)
      WRITE (Errmessage, '(a,2i10)') ' Values: ', valueI, valueI2
   ELSEIF (v7) THEN ! 1 real, 2 integer
      valueI2 = INT(value2)
      WRITE (Errmessage, '(a,f9.4,2i10)') ' Values: ', VALUE, valueI2, valueI
   ELSEIF (v8) THEN
      ! no error values
   END IF

   ! Write errors (that stop the program) to problems.txt; warnings to warnings.txt
   IF (flag_continue_on_error) THEN
      IF (SuppressWarnings == 0) THEN
#ifdef wrf
         WRITE (message, *) 'Warning: ', TRIM(ProblemFile)
         CALL wrf_debug(100, message)
         WRITE (message, *) TRIM(text1)
         CALL wrf_debug(100, message)
         CALL wrf_debug(100, Errmessage)
         WRITE (message, '(a,i14)') ' Count: ', ErrhCount(errh)
         CALL wrf_debug(100, message)
#else
         IF (warningChoice == 0) THEN
            OPEN (501, file='warnings.txt')
            WRITE (*, *) '>>> See warnings.txt for possible issues in the run <<<'
            warningChoice = 1
         ELSE
            OPEN (501, file='warnings.txt', position="append")
         END IF

         !Writing of the warnings file
         WRITE (501, *) 'Warning: ', TRIM(ProblemFile)

         ! write warning info
         WRITE (501, *) TRIM(text1)

         ! write warning codes
         WRITE (501, *) TRIM(Errmessage)

         WRITE (501, '(a,i14)') ' Count: ', ErrhCount(errh)
         CLOSE (501)
#endif
      END IF

   ELSE

#ifdef wrf
      WRITE (message, *) 'Problem: ', TRIM(ProblemFile)
      CALL wrf_debug(100, message)
      WRITE (message, *) 'ERROR! Program stopped: ', TRIM(text1)
      CALL wrf_debug(100, message)
      CALL wrf_debug(100, Errmessage)
      WRITE (message, '(i3)') errh
      CALL wrf_debug(100, message)
      WRITE (message, *) 'ERROR! SUEWS run stopped.'
      CALL wrf_debug(100, message)
      WRITE (message, *) 'fatal error in SUEWS:', TRIM(text1)
      CALL wrf_error_fatal(message)
#else
      IF (errorChoice == 0) THEN
         OPEN (500, file='problems.txt')
         WRITE (*, *) '>>> See problems.txt for serious issues in the run <<<'
         errorChoice = 1
      ELSE
         OPEN (500, file='problems.txt', position="append")
      END IF

      !Writing of the problem file
      WRITE (500, *) 'Problem: ', TRIM(ProblemFile)

      WRITE (500, *) 'ERROR! Program stopped: ', TRIM(text1)

      ! write error codes
      WRITE (500, *) TRIM(Errmessage)

      WRITE (500, '(i3)') errh !Add error code to problems.txt
      WRITE (*, *) 'ERROR! SUEWS run stopped.' !Print message to screen if program stopped
      CLOSE (500)

      WRITE (*, *) 'problem: ', TRIM(ProblemFile)
      WRITE (*, *) 'See problems.txt for more info.'
      WRITE (StopMessage, *) 'fatal error in SUEWS:'//NEW_LINE('A')//TRIM(text1)
      STOP 'Fatal error in SUEWS!'

#endif
   END IF

END SUBROUTINE ErrorHint

!=============================================================
