!=========================================================================
! sg feb 2012
! only run once at start - fixed for all grids and all years

SUBROUTINE OverallRunControl
   ! Last modified:
   ! MH 21 Jun 2017 - Added anthropogenic CO2 parameters and changed AnthropogenicHeat to Anthropogenic
   ! MH 16 Jun 2017 - Added biogenic CO2 parameters
   ! HCW 21 Apr 2017 - Added new method for precip disaggregation
   ! HCW 13 Jan 2017 - Changes to RunControl and InitialConditions
   ! HCW 04 Nov 2016 - minor bug fix in LAImin/LAImax warnings related to 3 veg surface types out cf 7 surface types
   ! LJ 27 Jan 2016  - Removal of tabs, cleaning of the code
   ! HCW 06 Mar 2015 - Removed options 10,20,30 (NARPOutput) for NetRadiationMethod
   ! HCW 06 Feb 2015 - File ID numbers changed so they are unique
   ! HCW 19 Dec 2014
   ! To Do:
   !   - Holidays.txt input file needs to be read in and coded into model
   !  - Add column header checks for SiteSelect
   !-------------------------------------------------------------------------

   USE allocateArray
   USE ColNamesInputFiles
   USE data_in
   USE defaultNotUsed
   USE FileName
   USE initial
   USE gis_data
   USE mod_z
   USE resist
   USE snowMod
   USE sues_data
   USE time

   IMPLICIT NONE

   INTEGER :: iv, i, SkipCounter, iFile !iv and i, ii are integers used in do loops
   CHARACTER(len=50) :: FileN
   INTEGER, PARAMETER :: nFile = 13
   CHARACTER(len=50), DIMENSION(nFile) :: &
      FileNames = [CHARACTER(len=50) :: &
                   'SUEWS_NonVeg.txt', 'SUEWS_Veg.txt', 'SUEWS_Water.txt', 'SUEWS_Snow.txt', &
                   'SUEWS_Soil.txt', 'SUEWS_Conductance.txt', 'SUEWS_OHMCoefficients.txt', &
                   'SUEWS_ESTMCoefficients.txt', 'SUEWS_AnthropogenicEmission.txt', 'SUEWS_Irrigation.txt', &
                   'SUEWS_Profiles.txt', 'SUEWS_WithinGridWaterDist.txt', 'SUEWS_BiogenCO2.txt']

   ! ---- Namelist for RunControl.nml ----
   NAMELIST /RunControl/ FileCode, &
      FileInputPath, &
      FileOutputPath, &
      Tstep, &
      MultipleMetFiles, &
      MultipleInitFiles, &
      MultipleESTMFiles, &
      MultipleLayoutFiles, &
      KeepTstepFilesIn, &
      KeepTstepFilesOut, &
      WriteOutOption, &
      ResolutionFilesIn, &
      ResolutionFilesOut, &
      ResolutionFilesInESTM, &
      CBLuse, &
      SnowUse, &
      ! SOLWEIGuse, &
      DiagMethod, &
      BaseTMethod, &
      EmissionsMethod, &
      NetRadiationMethod, &
      RoughLenHeatMethod, &
      RoughLenMomMethod, &
      SMDMethod, &
      StabilityMethod, &
      StorageHeatMethod, &
      OHMIncQF, &
      WaterUseMethod, &
      DisaggMethod, &
      DisaggMethodESTM, &
      RainDisaggMethod, &
      RainAmongN, &
      MultRainAmongN, &
      MultRainAmongNUpperI, &
      KdownZen, &
      SuppressWarnings, &
      ! ncMode, &
      ! nRow, &
      ! nCol, &
      Diagnose, &
      DiagnoseDisagg, &
      DiagnoseDisaggESTM, &
      DiagQN, &
      DiagQS

   ! -------------------------------------

   !Initialise namelist with default values
   KeepTstepFilesIn = 0
   KeepTstepFilesOut = 0
   WriteOutOption = 0
   DisaggMethod = 1 ! linear disaggregation of averages
   DisaggMethodESTM = 1 ! linear disaggregation of averages
   RainDisaggMethod = 100 ! even distribution among all subintervals
   RainAmongN = -999 ! no default setting for number of rainy subintervals
   MultRainAmongN = -999 ! no default setting for number of rainy subintervals
   MultRainAmongNUpperI = -999 ! no default setting for rain intensity upper bound
   KdownZen = 1 ! use zenith angle by default

   SuppressWarnings = 0 ! write warnings file
   ResolutionFilesIn = 0 ! Set to zero so that if not found, automatically set to Tstep below

   ! Set Diagnose switch to off (0). If Diagnose = 1 is set in RunControl, model progress will be printed
   Diagnose = 0
   DiagnoseDisagg = 0
   DiagnoseDisaggESTM = 0
   DiagQN = 0
   DiagQS = 0

   FileCode = 'none'
   !smithFile='Smith1966.grd'

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   !Read in the RunControl.nml file
   OPEN (55, File='RunControl.nml', err=200, status='old') !Change with needs
   READ (55, nml=RunControl, err=201)
   CLOSE (55)

   ! set tstep_prev as tstep ! TS 04 Jul 2018
   ! tstep_prev is NOT used by SUEWS but by WRF-SUEWS through the main interface
   tstep_prev = tstep

   IF (Diagnose == 1) WRITE (*, *) 'Diagnosis switched on (model progress will be printed to screen)...'

   !Check for problems with FileCode
   IF (FileCode == 'none') CALL ErrorHint(26, TRIM("RunControl.nml FileCode is missing"), notUsed, notUsed, notUsedI)

   IF (ResolutionFilesIn == 0) ResolutionFilesIn = Tstep !If ResolutionFilesIn not found, automatically set to Tstep

   !-----------------------------------------------------------------------

   !Write RunControl information to FileChoices.txt
   FileChoices = TRIM(FileOutputPath)//TRIM(FileCode)//'_FileChoices.txt'
   OPEN (12, file=FileChoices, err=203)
   WRITE (12, *) '----- RunControl -----'
   WRITE (12, nml=RunControl)
   CLOSE (12)

   ! !Determine what should be done with respect to radiation
   ! ! TODO: this can be wrapped into a subroutine, TS 20 Oct 2017
   ! AlbedoChoice=0
   ! ldown_option=0
   ! IF(NetRadiationMethod==0)THEN    !Observed Q* from the met input file will be used
   !    IF(SnowUse==1) THEN            !If snow is modelled, NARP is needed for surface temperature
   !       NetRadiationMethod=3000
   !       ldown_option=3              !Ldown will be modelled
   !       !NetRadiationMethod=NetRadiationMethod/1000
   !    ENDIF
   !
   ! ELSEIF(NetRadiationMethod>0)THEN  !Modelled Q* is used (NARP)
   !    AlbedoChoice=-9
   !    IF(NetRadiationMethod<10) THEN
   !       AlbedoChoice=0
   !       IF(NetRadiationMethod==1)ldown_option=1
   !       IF(NetRadiationMethod==2)ldown_option=2
   !       IF(NetRadiationMethod==3)ldown_option=3
   !
   !    ELSEIF(NetRadiationMethod>=100.AND.NetRadiationMethod<1000) THEN
   !       AlbedoChoice=1
   !       IF(NetRadiationMethod==100)ldown_option=1
   !       IF(NetRadiationMethod==200)ldown_option=2
   !       IF(NetRadiationMethod==300)ldown_option=3
   !       NetRadiationMethod=NetRadiationMethod/100
   !    ENDIF
   !
   !    !If bad NetRadiationMethod value
   !    IF(NetRadiationMethod>3.OR. AlbedoChoice==-9)THEN
   !       WRITE(*,*) 'NetRadiationMethod=',NetRadiationMethod
   !       WRITE(*,*) 'Value not usable'
   !       STOP
   !    ENDIF
   ! ENDIF

   ! Adjust input for precip downscaling using different intensities (HCW 21 Apr 2017)
   IF (RainDisaggMethod == 102) THEN
      DO i = 1, 5
         IF (MultRainAmongNUpperI(i) == -999) MultRainAmongNUpperI(i) = MAXVAL(MultRainAmongNUpperI)
      END DO
   END IF

   !------------------------------------------------------------------
   !Print run information on the screen
   WRITE (*, *) '--------------------------------------------------------'
   WRITE (*, *) "LUMPS/SUEWS - relevant references"
   WRITE (*, *) "LUMPS - Grimmond and Oke (2002) JAM, 41, 79-810"
   WRITE (*, *) "OHM - Grimmond and Oke (1999) JAM, 38, 922-940"
   WRITE (*, *) "NARP - Offerle et al. (2003) JAM"
   WRITE (*, *) "SUES - Evaporation Grimmond & Oke (1991) WRR"
   WRITE (*, *) "Water Balance Model Grimmond et al. (1986) WRR"
   WRITE (*, *) "NARP - Long wave improvements (Loridan et al. 2011 JAMC)"
   WRITE (*, *) "SUEWS - Anthropogenic heat, etc (Jarvi et al. 2011 JH)"
   WRITE (*, *) "SUEWS - Snow module included (Jarvi et al. 2014 GMD)"
   WRITE (*, *) "SUEWS - v2016a release (Ward et al. 2016 UC)"
   WRITE (*, *) '--------------------------------------------------------'

   !=======================================================================
   !======================== Read input files =============================
   ! This part reads the input files derived from the SiteInfo spreadsheet

   WRITE (*, *) 'Reading the following input files:'

   !=======================SUEWS_SiteSelect.txt============================
   FileN = 'SUEWS_SiteSelect.txt'
   CALL NumberRows(FileN, SkipHeaderSiteInfo) !Find number of rows in input file
   nlinesSiteSelect = nlines
   ALLOCATE (SiteSelect(nlinesSiteSelect, ncolumnsSiteSelect))
   !Read input file
   OPEN (21, file=TRIM(FileInputPath)//TRIM(FileN), err=300, status='old')
   DO SkipCounter = 1, (SkipHeaderSiteInfo - 1)
      READ (21, *) !Skip lines before header
   END DO
   READ (21, *) (HeaderSiteSelect_File(iv), iv=1, ncolumnsSiteSelect) !Get header

   DO i = 1, nlinesSiteSelect
      READ (21, *) (SiteSelect(i, iv), iv=1, ncolumnsSiteSelect)
      !write(*,*) (SiteSelect(i,iv),iv=1,ncolumnsSiteSelect)
   END DO
   CLOSE (21)

   !call InputHeaderCheck(FileN) !! Need to add column checks for SiteSelect.txt

   ! FileNames = (/'SUEWS_NonVeg.txt', 'SUEWS_Veg.txt', 'SUEWS_Water.txt', 'SUEWS_Snow.txt', &
   !      'SUEWS_Soil.txt', 'SUEWS_Conductance.txt', 'SUEWS_OHMCoefficients.txt', &
   !      'SUEWS_ESTMCoefficients.txt', 'SUEWS_AnthropogenicEmission.txt', 'SUEWS_Irrigation.txt', &
   !      'SUEWS_Profiles.txt', 'SUEWS_WithinGridWaterDist.txt', 'SUEWS_BiogenCO2.txt'/)

   DO iFile = 1, nFile
      CALL NumberRows(FileNames(iFile), SkipHeaderSiteInfo) !Find number of rows in input file
      SELECT CASE (iFile)
      CASE (1)
         nlinesNonVeg = nlines
         ALLOCATE (NonVeg_Coeff(nlinesNonVeg, ncolumnsNonVeg))
         CALL ReadCoeff(FileNames(iFile), nlinesNonVeg, ncolumnsNonVeg, HeaderNonVeg_File, NonVeg_Coeff)
      CASE (2)
         nlinesVeg = nlines
         ALLOCATE (Veg_Coeff(nlinesVeg, ncolumnsVeg))
         CALL ReadCoeff(FileNames(iFile), nlinesVeg, ncolumnsVeg, HeaderVeg_File, Veg_Coeff)
      CASE (3)
         nlinesWater = nlines
         ALLOCATE (Water_Coeff(nlinesWater, ncolumnsWater))
         CALL ReadCoeff(FileNames(iFile), nlinesWater, ncolumnsWater, HeaderWater_File, Water_Coeff)
      CASE (4)
         nlinesSnow = nlines
         ALLOCATE (Snow_Coeff(nlinesSnow, ncolumnsSnow))
         CALL ReadCoeff(FileNames(iFile), nlinesSnow, ncolumnsSnow, HeaderSnow_File, Snow_Coeff)
      CASE (5)
         nlinesSoil = nlines
         ALLOCATE (Soil_Coeff(nlinesSoil, ncolumnsSoil))
         CALL ReadCoeff(FileNames(iFile), nlinesSoil, ncolumnsSoil, HeaderSoil_File, Soil_Coeff)
      CASE (6)
         nlinesConductance = nlines
         ALLOCATE (Conductance_Coeff(nlinesConductance, ncolumnsConductance))
         CALL ReadCoeff(FileNames(iFile), nlinesConductance, ncolumnsConductance, HeaderCond_File, Conductance_Coeff)
      CASE (7)
         nlinesOHMCoefficients = nlines
         ALLOCATE (OHMCoefficients_Coeff(nlinesOHMCoefficients, ncolumnsOHMCoefficients))
         CALL ReadCoeff(FileNames(iFile), nlinesOHMCoefficients, ncolumnsOHMCoefficients, HeaderOHMCoefficients_File, &
                        OHMCoefficients_Coeff)
      CASE (8)
         nlinesESTMCoefficients = nlines
         ALLOCATE (ESTMCoefficients_Coeff(nlinesESTMCoefficients, ncolumnsESTMCoefficients))
         CALL ReadCoeff(FileNames(iFile), nlinesESTMCoefficients, ncolumnsESTMCoefficients, HeaderESTMCoefficients_File, &
                        ESTMCoefficients_Coeff)
      CASE (9)
         nlinesAnthropogenic = nlines
         ALLOCATE (Anthropogenic_Coeff(nlinesAnthropogenic, ncolumnsAnthropogenic))
         CALL ReadCoeff(FileNames(iFile), nlinesAnthropogenic, ncolumnsAnthropogenic, HeaderAnthropogenic_File, Anthropogenic_Coeff)
      CASE (10)
         nlinesIrrigation = nlines
         ALLOCATE (Irrigation_Coeff(nlinesIrrigation, ncolumnsIrrigation))
         CALL ReadCoeff(FileNames(iFile), nlinesIrrigation, ncolumnsIrrigation, HeaderIrrigation_File, Irrigation_Coeff)
      CASE (11)
         nlinesProfiles = nlines
         ALLOCATE (Profiles_Coeff(nlinesProfiles, ncolumnsProfiles))
         CALL ReadCoeff(FileNames(iFile), nlinesProfiles, ncolumnsProfiles, HeaderProfiles_File, Profiles_Coeff)
      CASE (12)
         nlinesWGWaterDist = nlines
         ALLOCATE (WGWaterDist_Coeff(nlinesWGWaterDist, ncolumnsWGWaterDist))
         CALL ReadCoeff(FileNames(iFile), nlinesWGWaterDist, ncolumnsWGWaterDist, HeaderWGWaterDist_File, WGWaterDist_Coeff)
      CASE (13)
         nlinesBiogen = nlines
         ALLOCATE (Biogen_Coeff(nlinesBiogen, ncolumnsBiogen))
         CALL ReadCoeff(FileNames(iFile), nlinesBiogen, ncolumnsBiogen, HeaderBiogen_File, Biogen_Coeff)
      END SELECT
   END DO

   !=======================================================================
   !=======================================================================

   !-----------------------------------------------------------------------
   !SUEWS run information
   InputMetFormat = 10 !Input met data file in LUMPS format(1) or SUEWS format(10)
   LAICalcYes = 1 !Use observed(0) or modelled(1) LAI
   EvapMethod = 2 !Evaporation calculated according to Rutter(1) or Shuttleworth(2)
   WriteDailyState = 1 !Daily state file written
   tstepcount = 0

   t_INTERVAL = 3600 !Number of seconds in an hour

   !Calculate nsh (number of steps per hour) from model timestep (tstep) set in in RunControl
   nsh_real = t_INTERVAL/REAL(tstep, KIND(1D0))

   ! Check nsh is an integer
   IF (nsh_real == INT(nsh_real)) THEN
      nsh = INT(nsh_real)
   ELSE
      CALL ErrorHint(39, &
                     'TSTEP must divide into t_INTERVAL exactly.', REAL(tstep, KIND(1D0)), REAL(t_INTERVAL, KIND(1D0)), notUsedI)
   END IF

   ! Check nsh is reasonable
   IF (nsh_real < 6 .OR. nsh_real > 60) THEN
      CALL ErrorHint(39, 'TSTEP is too small or too large.', REAL(tstep, KIND(1D0)), REAL(t_INTERVAL, KIND(1D0)), notUsedI)
   END IF

   ! Cast integer nsh as nsh_real for use in calculations
   nsh_real = REAL(nsh, KIND(1D0))
   ! Cast integer tstep as tstep_real for use in calculations
   tstep_real = REAL(tstep, KIND(1D0))
   ! get integer nsd from nsh for use in AnOHM checking, 20160708 TS
   nsd = 24*nsh

   !! Check this is still valid for v2016a
   HalfTimeStep = REAL(tstep_real)/2/(24*3600) !Used in NARP_cal_SunPosition to get sunpos in the middle of timestep

   RETURN

   !-------Possible problems-----------------------------------------------
200 CALL ErrorHint(47, 'RunControl.nml', notUsed, notUsed, notUsedI)
201 CALL ErrorHint(48, 'RunControl.nml', notUsed, notUsed, notUsedI)

203 CALL ErrorHint(47, TRIM(FileChoices), notUsed, notUsed, notUsedI)

300 CALL ErrorHint(48, TRIM(FileN), notUsed, notUsed, notUsedI)
   !-----------------------------------------------------------------------

   !pause

END SUBROUTINE OverallRunControl
!=========================================================================

SUBROUTINE ReadCoeff(FileName, nlines, ncolumns, HeaderFile, Coeff)

   USE data_in
   USE defaultNotUsed

   IMPLICIT NONE
   !----------------------------------------------------------------------
   ! dummy arguments
   !----------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: FileName
   INTEGER, INTENT(in) :: nlines, ncolumns
   CHARACTER(len=*), INTENT(out) :: HeaderFile(ncolumns)
   REAL(KIND(1D0)), INTENT(out) :: Coeff(nlines, ncolumns)

   !----------------------------------------------------------------------
   ! local variables
   !----------------------------------------------------------------------
   INTEGER :: SkipCounter, iv, i, ii

   !Read input file
   OPEN (22, file=TRIM(FileInputPath)//TRIM(FileName), err=301, status='old')

   DO SkipCounter = 1, SkipHeaderSiteInfo - 1
      READ (22, *) !Skip lines before header
   END DO
   READ (22, *) (HeaderFile(iv), iv=1, ncolumns) !Get header

   DO i = 1, nlines
      READ (22, *) (Coeff(i, iv), iv=1, ncolumns)
      !write(*,*) (NonVeg_Coeff(i,iv),iv=1,ncolumnsNonVeg)
   END DO
   CLOSE (22)

   CALL InputHeaderCheck(FileName)

   ! Check codes are unique
   DO i = 1, nlines
      DO ii = i + 1, nlines
         IF (Coeff(i, 1) == Coeff(ii, 1) .AND. i /= ii) THEN
            WRITE (*, *) 'Code', Coeff(i, 1), 'in ', TRIM(FileName), ' not unique!'
            CALL ErrorHint(60, FileName, Coeff(i, 1), notUsed, notUsedI)
         END IF
      END DO
   END DO

   RETURN

301 CALL ErrorHint(48, TRIM(FileName), notUsed, notUsed, notUsedI)

END SUBROUTINE ReadCoeff

!=========================================================================
!This subroutine finds the number of rows in each input file
!INPUT: FileN          Name of the input file
!       SkipHeaderLines   Number of header rows to skip
!Made by LJ/HCW in Oct 2014

SUBROUTINE NumberRows(FileN, SkipHeaderLines)

   USE data_in
   USE DefaultNotUsed
   USE Initial

   IMPLICIT NONE

   CHARACTER(len=50) :: FileN
   INTEGER :: SkipHeaderLines, RunNumber
   INTEGER :: SkipCounter
   INTEGER :: ios

   WRITE (*, *) FileN
   OPEN (39, file=TRIM(FileInputPath)//TRIM(FileN), err=204, status='old')

   IF (SkipHeaderLines > 0) THEN
      DO SkipCounter = 1, SkipHeaderLines
         READ (39, *, err=205)
         !write(*,*) SkipCounter, SkipHeaderLines
      END DO
   END IF

   nlines = 0 !Initialize nlines
   DO
      READ (39, *, iostat=ios) RunNumber
      IF (ios < 0 .OR. RunNumber == -9) EXIT !IF(RunNumber==-9) EXIT
      nlines = nlines + 1
   END DO
   !write(*,*) 'nlines read: ',nlines
   CLOSE (39)

   RETURN

204 CALL ErrorHint(47, TRIM(FileInputPath)//TRIM(FileN), notUsed, notUsed, notUsedI)
205 CALL ErrorHint(48, TRIM(FileInputPath)//TRIM(FileN), notUsed, notUsed, notUsedI)

END SUBROUTINE NumberRows
!=========================================================================

!----------------------------------------------------------------------------------------------
!Moves input information corresponding to row rr of SiteSelect into row Gridiv of SurfaceChar
! - currently done once per year for each grid
!Made by HW&LJ Oct 2014.
!Last modified: HCW 26 Jan 2015
! Interpolated hourly energy use profiles to resolution of model timestep.
! Interpolated hourly water use profiles to resolution of model timestep.
! Normalised energy use & water use profiles as required.
! Hourly snow clearing profile is a 0-1 switch, and thus is not interpolated nor normalised.
!  - TODO: Rename profiles 1-24 rather than 0-23?
!----------------------------------------------------------------------------------------------
SUBROUTINE InitializeSurfaceCharacteristics(Gridiv, rr)

   USE allocateArray
   USE ColNamesInputFiles
   USE data_in
   USE defaultNotUsed
   USE Initial
   USE sues_data

   IMPLICIT NONE

   INTEGER :: Gridiv, & !Row of SurfaceChar where input information will be stored
              rr !Row of SiteSelect that matches current grid and year
   INTEGER :: iii, &
              ii

   !-------------------------------------------------------------------------------------------

   ! Initialise row of SurfaceChar
   SurfaceChar(Gridiv, :) = -999

   ! Transfer data in SiteSelect to SurfaceChar
   SurfaceChar(Gridiv, 1:ncolumnsSiteSelect) = SiteSelect(rr, 1:ncolumnsSiteSelect) !Cols in same order as in SiteSelect.txt

   ! ======== Retrieve information from other input files via codes ========

   ! ---- Find code for Paved surface (Impervious) ----
   CALL CodeMatchNonVeg(rr, c_PavedCode)
   ! Transfer characteristics to SurfaceChar for Paved surface
   SurfaceChar(gridiv, c_AlbMin(PavSurf)) = NonVeg_Coeff(iv5, ci_AlbMin)
   SurfaceChar(gridiv, c_AlbMax(PavSurf)) = NonVeg_Coeff(iv5, ci_AlbMax)
   SurfaceChar(gridiv, c_Emis(PavSurf)) = NonVeg_Coeff(iv5, ci_Emis)
   SurfaceChar(gridiv, c_StorMin(PavSurf)) = NonVeg_Coeff(iv5, ci_StorMin)
   SurfaceChar(gridiv, c_StorMax(PavSurf)) = NonVeg_Coeff(iv5, ci_StorMax)
   SurfaceChar(gridiv, c_WetThresh(PavSurf)) = NonVeg_Coeff(iv5, ci_WetThresh)
   SurfaceChar(gridiv, c_StateLimit(PavSurf)) = NonVeg_Coeff(iv5, ci_StateLimit)
   SurfaceChar(gridiv, c_DrEq(PavSurf)) = NonVeg_Coeff(iv5, ci_DrEq)
   SurfaceChar(gridiv, c_DrCoef1(PavSurf)) = NonVeg_Coeff(iv5, ci_DrCoef1)
   SurfaceChar(gridiv, c_DrCoef2(PavSurf)) = NonVeg_Coeff(iv5, ci_DrCoef2)
   SurfaceChar(gridiv, c_SoilTCode(PavSurf)) = NonVeg_Coeff(iv5, ci_SoilTCode)
   SurfaceChar(gridiv, c_SnowLimPat(PavSurf)) = NonVeg_Coeff(iv5, ci_SnowLimPat)
   SurfaceChar(gridiv, c_SnowLimRem(PavSurf)) = NonVeg_Coeff(iv5, ci_SnowLimRem)
   SurfaceChar(gridiv, c_OHMCode_SWet(PavSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_SWet)
   SurfaceChar(gridiv, c_OHMCode_SDry(PavSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_SDry)
   SurfaceChar(gridiv, c_OHMCode_WWet(PavSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_WWet)
   SurfaceChar(gridiv, c_OHMCode_WDry(PavSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_WDry)
   SurfaceChar(gridiv, c_OHMThresh_SW(PavSurf)) = NonVeg_Coeff(iv5, ci_OHMThresh_SW)
   SurfaceChar(gridiv, c_OHMThresh_WD(PavSurf)) = NonVeg_Coeff(iv5, ci_OHMThresh_WD)
   SurfaceChar(gridiv, c_ESTMCode(PavSurf)) = NonVeg_Coeff(iv5, ci_ESTMCode)
   SurfaceChar(Gridiv, c_CpAnOHM(PavSurf)) = NonVeg_Coeff(iv5, ci_CpAnOHM) ! heat capacity, AnOHM TS
   SurfaceChar(Gridiv, c_KkAnOHM(PavSurf)) = NonVeg_Coeff(iv5, ci_KkAnOHM) ! heat conductivity, AnOHM TS
   SurfaceChar(Gridiv, c_ChAnOHM(PavSurf)) = NonVeg_Coeff(iv5, ci_ChAnOHM) ! bulk transfer coef., AnOHM TS

   ! Use SoilCode for Paved to find code for soil characteristics
   CALL CodeMatchSoil(Gridiv, c_SoilTCode(PavSurf))
   ! Transfer soil characteristics to SurfaceChar
   SurfaceChar(gridiv, c_SoilDepth(PavSurf)) = Soil_Coeff(iv5, cSo_SoilDepth)
   SurfaceChar(gridiv, c_SoilStCap(PavSurf)) = Soil_Coeff(iv5, cSo_SoilStCap)
   SurfaceChar(gridiv, c_KSat(PavSurf)) = Soil_Coeff(iv5, cSo_KSat)
   SurfaceChar(gridiv, c_SoilDens(PavSurf)) = Soil_Coeff(iv5, cSo_SoilDens)
   SurfaceChar(gridiv, c_SoilInfRate(PavSurf)) = Soil_Coeff(iv5, cSo_SoilInfRate)
   SurfaceChar(gridiv, c_ObsSMDepth(PavSurf)) = Soil_Coeff(iv5, cSo_ObsSMDepth)
   SurfaceChar(gridiv, c_ObsSMMax(PavSurf)) = Soil_Coeff(iv5, cSo_ObsSMMax)
   SurfaceChar(gridiv, c_ObsSNRFrac(PavSurf)) = Soil_Coeff(iv5, cSo_ObsSNRFrac)

   ! Get OHM characteristics for Paved
   CALL CodeMatchOHM(Gridiv, PavSurf, 'SWet') !Summer wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SWet(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SWet(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SWet(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, PavSurf, 'SDry') !Summer dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SDry(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SDry(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SDry(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, PavSurf, 'WWet') !Winter wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WWet(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WWet(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WWet(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, PavSurf, 'WDry') !Winter dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WDry(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WDry(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WDry(PavSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)

   ! Get water distribution (within grid) for Paved
   CALL CodeMatchDist(rr, c_WGPavedCode, cWG_ToPaved)
   ! Transfer distribution to SurfaceChar
   SurfaceChar(Gridiv, c_WGToPaved(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToPaved)
   SurfaceChar(Gridiv, c_WGToBldgs(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBldgs)
   SurfaceChar(Gridiv, c_WGToEveTr(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToEveTr)
   SurfaceChar(Gridiv, c_WGToDecTr(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToDecTr)
   SurfaceChar(Gridiv, c_WGToGrass(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToGrass)
   SurfaceChar(Gridiv, c_WGToBSoil(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBSoil)
   SurfaceChar(Gridiv, c_WGToWater(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToWater)
   SurfaceChar(Gridiv, c_WGToRunoff(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToRunoff)
   SurfaceChar(Gridiv, c_WGToSoilStore(PavSurf)) = WGWaterDist_Coeff(iv5, cWG_ToSoilStore)

   ! ---- Find code for Bldgs surface (Impervious) ----
   CALL CodeMatchNonVeg(rr, c_BldgsCode)
   ! Transfer characteristics to SurfaceChar for Bldgs surface
   SurfaceChar(gridiv, c_AlbMin(BldgSurf)) = NonVeg_Coeff(iv5, ci_AlbMin)
   SurfaceChar(gridiv, c_AlbMax(BldgSurf)) = NonVeg_Coeff(iv5, ci_AlbMax)
   SurfaceChar(gridiv, c_Emis(BldgSurf)) = NonVeg_Coeff(iv5, ci_Emis)
   SurfaceChar(gridiv, c_StorMin(BldgSurf)) = NonVeg_Coeff(iv5, ci_StorMin)
   SurfaceChar(gridiv, c_StorMax(BldgSurf)) = NonVeg_Coeff(iv5, ci_StorMax)
   SurfaceChar(gridiv, c_WetThresh(BldgSurf)) = NonVeg_Coeff(iv5, ci_WetThresh)
   SurfaceChar(gridiv, c_StateLimit(BldgSurf)) = NonVeg_Coeff(iv5, ci_StateLimit)
   SurfaceChar(gridiv, c_DrEq(BldgSurf)) = NonVeg_Coeff(iv5, ci_DrEq)
   SurfaceChar(gridiv, c_DrCoef1(BldgSurf)) = NonVeg_Coeff(iv5, ci_DrCoef1)
   SurfaceChar(gridiv, c_DrCoef2(BldgSurf)) = NonVeg_Coeff(iv5, ci_DrCoef2)
   SurfaceChar(gridiv, c_SoilTCode(BldgSurf)) = NonVeg_Coeff(iv5, ci_SoilTCode)
   SurfaceChar(gridiv, c_SnowLimPat(BldgSurf)) = NonVeg_Coeff(iv5, ci_SnowLimPat)
   SurfaceChar(gridiv, c_SnowLimRem(BldgSurf)) = NonVeg_Coeff(iv5, ci_SnowLimRem)
   SurfaceChar(gridiv, c_OHMCode_SWet(BldgSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_SWet)
   SurfaceChar(gridiv, c_OHMCode_SDry(BldgSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_SDry)
   SurfaceChar(gridiv, c_OHMCode_WWet(BldgSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_WWet)
   SurfaceChar(gridiv, c_OHMCode_WDry(BldgSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_WDry)
   SurfaceChar(gridiv, c_OHMThresh_SW(BldgSurf)) = NonVeg_Coeff(iv5, ci_OHMThresh_SW)
   SurfaceChar(gridiv, c_OHMThresh_WD(Bldgsurf)) = NonVeg_Coeff(iv5, ci_OHMThresh_WD)
   SurfaceChar(gridiv, c_ESTMCode(BldgSurf)) = NonVeg_Coeff(iv5, ci_ESTMCode)
   SurfaceChar(Gridiv, c_CpAnOHM(BldgSurf)) = NonVeg_Coeff(iv5, ci_CpAnOHM) ! heat capacity, AnOHM TS
   SurfaceChar(Gridiv, c_KkAnOHM(BldgSurf)) = NonVeg_Coeff(iv5, ci_KkAnOHM) ! heat conductivity, AnOHM TS
   SurfaceChar(Gridiv, c_ChAnOHM(BldgSurf)) = NonVeg_Coeff(iv5, ci_ChAnOHM) ! bulk transfer coef., AnOHM TS

   ! Use SoilCode for Bldgs to find code for soil characteristics
   CALL CodeMatchSoil(Gridiv, c_SoilTCode(BldgSurf))
   ! Transfer soil characteristics to SurfaceChar
   SurfaceChar(gridiv, c_SoilDepth(BldgSurf)) = Soil_Coeff(iv5, cSo_SoilDepth)
   SurfaceChar(gridiv, c_SoilStCap(BldgSurf)) = Soil_Coeff(iv5, cSo_SoilStCap)
   SurfaceChar(gridiv, c_KSat(BldgSurf)) = Soil_Coeff(iv5, cSo_KSat)
   SurfaceChar(gridiv, c_SoilDens(BldgSurf)) = Soil_Coeff(iv5, cSo_SoilDens)
   SurfaceChar(gridiv, c_SoilInfRate(BldgSurf)) = Soil_Coeff(iv5, cSo_SoilInfRate)
   SurfaceChar(gridiv, c_ObsSMDepth(BldgSurf)) = Soil_Coeff(iv5, cSo_ObsSMDepth)
   SurfaceChar(gridiv, c_ObsSMMax(BldgSurf)) = Soil_Coeff(iv5, cSo_ObsSMMax)
   SurfaceChar(gridiv, c_ObsSNRFrac(BldgSurf)) = Soil_Coeff(iv5, cSo_ObsSNRFrac)
   !Get OHM characteristics for Bldgs
   CALL CodeMatchOHM(Gridiv, BldgSurf, 'SWet') !Summer wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SWet(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SWet(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SWet(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, BldgSurf, 'SDry') !Summer dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SDry(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SDry(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SDry(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, BldgSurf, 'WWet') !Winter wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WWet(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WWet(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WWet(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, BldgSurf, 'WDry') !Winter dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WDry(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WDry(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WDry(BldgSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)

   ! Get water distribution (within grid) for Bldgs
   CALL CodeMatchDist(rr, c_WGBldgsCode, cWG_ToBldgs)
   ! Transfer distribution to SurfaceChar
   SurfaceChar(Gridiv, c_WGToPaved(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToPaved)
   SurfaceChar(Gridiv, c_WGToBldgs(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBldgs)
   SurfaceChar(Gridiv, c_WGToEveTr(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToEveTr)
   SurfaceChar(Gridiv, c_WGToDecTr(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToDecTr)
   SurfaceChar(Gridiv, c_WGToGrass(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToGrass)
   SurfaceChar(Gridiv, c_WGToBSoil(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBSoil)
   SurfaceChar(Gridiv, c_WGToWater(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToWater)
   SurfaceChar(Gridiv, c_WGToRunoff(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToRunoff)
   SurfaceChar(Gridiv, c_WGToSoilStore(BldgSurf)) = WGWaterDist_Coeff(iv5, cWG_ToSoilStore)

   ! ---- Find code for EveTr surface (Pervious) ----
   CALL CodeMatchVeg(rr, c_EveTrCode)
   ! Transfer characteristics to SurfaceChar for EveTr surface
   ! All surfaces (1-nsurf)
   SurfaceChar(gridiv, c_AlbMin(ConifSurf)) = Veg_Coeff(iv5, cp_AlbMin)
   SurfaceChar(gridiv, c_AlbMax(ConifSurf)) = Veg_Coeff(iv5, cp_AlbMax)
   SurfaceChar(gridiv, c_Emis(ConifSurf)) = Veg_Coeff(iv5, cp_Emis)
   SurfaceChar(gridiv, c_StorMin(ConifSurf)) = Veg_Coeff(iv5, cp_StorMin)
   SurfaceChar(gridiv, c_StorMax(ConifSurf)) = Veg_Coeff(iv5, cp_StorMax)
   SurfaceChar(gridiv, c_WetThresh(ConifSurf)) = Veg_Coeff(iv5, cp_WetThresh)
   SurfaceChar(gridiv, c_StateLimit(ConifSurf)) = Veg_Coeff(iv5, cp_StateLimit)
   SurfaceChar(gridiv, c_DrEq(ConifSurf)) = Veg_Coeff(iv5, cp_DrEq)
   SurfaceChar(gridiv, c_DrCoef1(ConifSurf)) = Veg_Coeff(iv5, cp_DrCoef1)
   SurfaceChar(gridiv, c_DrCoef2(ConifSurf)) = Veg_Coeff(iv5, cp_DrCoef2)
   SurfaceChar(gridiv, c_SoilTCode(ConifSurf)) = Veg_Coeff(iv5, cp_SoilTCode)
   SurfaceChar(gridiv, c_SnowLimPat(ConifSurf)) = Veg_Coeff(iv5, cp_SnowLimPat)
   ! Veg surfaces only (1-nvegsurf)
   SurfaceChar(gridiv, c_BaseT(ivConif)) = Veg_Coeff(iv5, cp_BaseT)
   SurfaceChar(gridiv, c_BaseTe(ivConif)) = Veg_Coeff(iv5, cp_BaseTe)
   SurfaceChar(gridiv, c_GDDFull(ivConif)) = Veg_Coeff(iv5, cp_GDDFull)
   SurfaceChar(gridiv, c_SDDFull(ivConif)) = Veg_Coeff(iv5, cp_SDDFull)
   SurfaceChar(gridiv, c_LAIMin(ivConif)) = Veg_Coeff(iv5, cp_LAIMin)
   SurfaceChar(gridiv, c_LAIMax(ivConif)) = Veg_Coeff(iv5, cp_LAIMax)
   SurfaceChar(gridiv, c_PorosityMin(ivConif)) = Veg_Coeff(iv5, cp_PorosityMin)
   SurfaceChar(gridiv, c_PorosityMax(ivConif)) = Veg_Coeff(iv5, cp_PorosityMax)
   SurfaceChar(gridiv, c_GsMax(ivConif)) = Veg_Coeff(iv5, cp_GsMax)
   SurfaceChar(gridiv, c_LAIEq(ivConif)) = Veg_Coeff(iv5, cp_LAIEq)
   SurfaceChar(gridiv, c_LeafGP1(ivConif)) = Veg_Coeff(iv5, cp_LeafGP1)
   SurfaceChar(gridiv, c_LeafGP2(ivConif)) = Veg_Coeff(iv5, cp_LeafGP2)
   SurfaceChar(gridiv, c_LeafOP1(ivConif)) = Veg_Coeff(iv5, cp_LeafOP1)
   SurfaceChar(gridiv, c_LeafOP2(ivConif)) = Veg_Coeff(iv5, cp_LeafOP2)
   ! OHM codes
   SurfaceChar(gridiv, c_OHMCode_SWet(ConifSurf)) = Veg_Coeff(iv5, cp_OHMCode_SWet)
   SurfaceChar(gridiv, c_OHMCode_SDry(ConifSurf)) = Veg_Coeff(iv5, cp_OHMCode_SDry)
   SurfaceChar(gridiv, c_OHMCode_WWet(ConifSurf)) = Veg_Coeff(iv5, cp_OHMCode_WWet)
   SurfaceChar(gridiv, c_OHMCode_WDry(ConifSurf)) = Veg_Coeff(iv5, cp_OHMCode_WDry)
   SurfaceChar(gridiv, c_OHMThresh_SW(ConifSurf)) = Veg_Coeff(iv5, cp_OHMThresh_SW)
   SurfaceChar(gridiv, c_OHMThresh_WD(ConifSurf)) = Veg_Coeff(iv5, cp_OHMThresh_WD)
   ! ESTM code
   SurfaceChar(gridiv, c_ESTMCode(ConifSurf)) = Veg_Coeff(iv5, cp_ESTMCode)
   ! AnOHM TS
   SurfaceChar(Gridiv, c_CpAnOHM(ConifSurf)) = Veg_Coeff(iv5, cp_CpAnOHM) ! heat capacity, AnOHM TS
   SurfaceChar(Gridiv, c_KkAnOHM(ConifSurf)) = Veg_Coeff(iv5, cp_KkAnOHM) ! heat conductivity, AnOHM TS
   SurfaceChar(Gridiv, c_ChAnOHM(ConifSurf)) = Veg_Coeff(iv5, cp_ChAnOHM) ! bulk transfer coef., AnOHM TS

   SurfaceChar(Gridiv, c_BiogenCO2Code(ivConif)) = Veg_Coeff(iv5, cp_BiogenCO2Code)

   ! ---- Find code for Biogenic CO2 Method ----
   CALL CodeMatchBiogen(gridiv, c_BiogenCO2Code(ivConif))
   ! Transfer Biogenic CO2 characteristics to SurfaceChar
   SurfaceChar(gridiv, c_alpha_bioCO2(ivConif)) = Biogen_Coeff(iv5, cB_alpha)
   SurfaceChar(gridiv, c_beta_bioCO2(ivConif)) = Biogen_Coeff(iv5, cB_beta)
   SurfaceChar(gridiv, c_theta_bioCO2(ivConif)) = Biogen_Coeff(iv5, cB_theta)
   SurfaceChar(gridiv, c_alpha_enh_bioCO2(ivConif)) = Biogen_Coeff(iv5, cB_alpha_enh)
   SurfaceChar(gridiv, c_beta_enh_bioCO2(ivConif)) = Biogen_Coeff(iv5, cB_alpha_enh)
   SurfaceChar(gridiv, c_resp_a(ivConif)) = Biogen_Coeff(iv5, cB_resp_a)
   SurfaceChar(gridiv, c_resp_b(ivConif)) = Biogen_Coeff(iv5, cB_resp_b)
   SurfaceChar(gridiv, c_min_res_bioCO2(ivConif)) = Biogen_Coeff(iv5, cB_min_r)

   ! Use SoilCode for EveTr to find code for soil characteristics
   CALL CodeMatchSoil(Gridiv, c_SoilTCode(ConifSurf))
   ! Transfer soil characteristics to SurfaceChar
   SurfaceChar(gridiv, c_SoilDepth(ConifSurf)) = Soil_Coeff(iv5, cSo_SoilDepth)
   SurfaceChar(gridiv, c_SoilStCap(ConifSurf)) = Soil_Coeff(iv5, cSo_SoilStCap)
   SurfaceChar(gridiv, c_KSat(ConifSurf)) = Soil_Coeff(iv5, cSo_KSat)
   SurfaceChar(gridiv, c_SoilDens(ConifSurf)) = Soil_Coeff(iv5, cSo_SoilDens)
   SurfaceChar(gridiv, c_SoilInfRate(ConifSurf)) = Soil_Coeff(iv5, cSo_SoilInfRate)
   SurfaceChar(gridiv, c_ObsSMDepth(ConifSurf)) = Soil_Coeff(iv5, cSo_ObsSMDepth)
   SurfaceChar(gridiv, c_ObsSMMax(ConifSurf)) = Soil_Coeff(iv5, cSo_ObsSMMax)
   SurfaceChar(gridiv, c_ObsSNRFrac(ConifSurf)) = Soil_Coeff(iv5, cSo_ObsSNRFrac)
   !Get OHM characteristics for Conif
   CALL CodeMatchOHM(Gridiv, ConifSurf, 'SWet') !Summer wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SWet(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SWet(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SWet(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, ConifSurf, 'SDry') !Summer dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SDry(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SDry(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SDry(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, ConifSurf, 'WWet') !Winter wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WWet(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WWet(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WWet(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, ConifSurf, 'WDry') !Winter dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WDry(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WDry(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WDry(ConifSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)

   ! Get water distribution (within grid) for EveTr
   CALL CodeMatchDist(rr, c_WGEveTrCode, cWG_ToEveTr)
   ! Transfer distribution to SurfaceChar
   SurfaceChar(Gridiv, c_WGToPaved(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToPaved)
   SurfaceChar(Gridiv, c_WGToBldgs(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBldgs)
   SurfaceChar(Gridiv, c_WGToEveTr(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToEveTr)
   SurfaceChar(Gridiv, c_WGToDecTr(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToDecTr)
   SurfaceChar(Gridiv, c_WGToGrass(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToGrass)
   SurfaceChar(Gridiv, c_WGToBSoil(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBSoil)
   SurfaceChar(Gridiv, c_WGToWater(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToWater)
   SurfaceChar(Gridiv, c_WGToRunoff(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToRunoff)
   SurfaceChar(Gridiv, c_WGToSoilStore(ConifSurf)) = WGWaterDist_Coeff(iv5, cWG_ToSoilStore)

   ! ---- Find code for DecTr surface (Pervious) ----
   CALL CodeMatchVeg(rr, c_DecTrCode)
   ! Transfer characteristics to SurfaceChar for DecTr surface
   ! All surfaces (1-nsurf)
   SurfaceChar(gridiv, c_AlbMin(DecidSurf)) = Veg_Coeff(iv5, cp_AlbMin)
   SurfaceChar(gridiv, c_AlbMax(DecidSurf)) = Veg_Coeff(iv5, cp_AlbMax)
   SurfaceChar(gridiv, c_Emis(DecidSurf)) = Veg_Coeff(iv5, cp_Emis)
   SurfaceChar(gridiv, c_StorMin(DecidSurf)) = Veg_Coeff(iv5, cp_StorMin)
   SurfaceChar(gridiv, c_StorMax(DecidSurf)) = Veg_Coeff(iv5, cp_StorMax)
   SurfaceChar(gridiv, c_WetThresh(DecidSurf)) = Veg_Coeff(iv5, cp_WetThresh)
   SurfaceChar(gridiv, c_StateLimit(DecidSurf)) = Veg_Coeff(iv5, cp_StateLimit)
   SurfaceChar(gridiv, c_DrEq(DecidSurf)) = Veg_Coeff(iv5, cp_DrEq)
   SurfaceChar(gridiv, c_DrCoef1(DecidSurf)) = Veg_Coeff(iv5, cp_DrCoef1)
   SurfaceChar(gridiv, c_DrCoef2(DecidSurf)) = Veg_Coeff(iv5, cp_DrCoef2)
   SurfaceChar(gridiv, c_SoilTCode(DecidSurf)) = Veg_Coeff(iv5, cp_SoilTCode)
   SurfaceChar(gridiv, c_SnowLimPat(DecidSurf)) = Veg_Coeff(iv5, cp_SnowLimPat)
   ! Veg surfaces only (1-nvegsurf)
   SurfaceChar(gridiv, c_BaseT(ivDecid)) = Veg_Coeff(iv5, cp_BaseT)
   SurfaceChar(gridiv, c_BaseTe(ivDecid)) = Veg_Coeff(iv5, cp_BaseTe)
   SurfaceChar(gridiv, c_GDDFull(ivDecid)) = Veg_Coeff(iv5, cp_GDDFull)
   SurfaceChar(gridiv, c_SDDFull(ivDecid)) = Veg_Coeff(iv5, cp_SDDFull)
   SurfaceChar(gridiv, c_LAIMin(ivDecid)) = Veg_Coeff(iv5, cp_LAIMin)
   SurfaceChar(gridiv, c_LAIMax(ivDecid)) = Veg_Coeff(iv5, cp_LAIMax)
   SurfaceChar(gridiv, c_PorosityMin(ivDecid)) = Veg_Coeff(iv5, cp_PorosityMin)
   SurfaceChar(gridiv, c_PorosityMax(ivDecid)) = Veg_Coeff(iv5, cp_PorosityMax)
   SurfaceChar(gridiv, c_GsMax(ivDecid)) = Veg_Coeff(iv5, cp_GsMax)
   SurfaceChar(gridiv, c_LAIEq(ivDecid)) = Veg_Coeff(iv5, cp_LAIEq)
   SurfaceChar(gridiv, c_LeafGP1(ivDecid)) = Veg_Coeff(iv5, cp_LeafGP1)
   SurfaceChar(gridiv, c_LeafGP2(ivDecid)) = Veg_Coeff(iv5, cp_LeafGP2)
   SurfaceChar(gridiv, c_LeafOP1(ivDecid)) = Veg_Coeff(iv5, cp_LeafOP1)
   SurfaceChar(gridiv, c_LeafOP2(ivDecid)) = Veg_Coeff(iv5, cp_LeafOP2)
   ! OHM codes
   SurfaceChar(gridiv, c_OHMCode_SWet(DecidSurf)) = Veg_Coeff(iv5, cp_OHMCode_SWet)
   SurfaceChar(gridiv, c_OHMCode_SDry(DecidSurf)) = Veg_Coeff(iv5, cp_OHMCode_SDry)
   SurfaceChar(gridiv, c_OHMCode_WWet(DecidSurf)) = Veg_Coeff(iv5, cp_OHMCode_WWet)
   SurfaceChar(gridiv, c_OHMCode_WDry(DecidSurf)) = Veg_Coeff(iv5, cp_OHMCode_WDry)
   SurfaceChar(gridiv, c_OHMThresh_SW(DecidSurf)) = Veg_Coeff(iv5, cp_OHMThresh_SW)
   SurfaceChar(gridiv, c_OHMThresh_WD(DecidSurf)) = Veg_Coeff(iv5, cp_OHMThresh_WD)
   ! ESTM code
   SurfaceChar(gridiv, c_ESTMCode(DecidSurf)) = Veg_Coeff(iv5, cp_ESTMCode)
   ! AnOHM TS
   SurfaceChar(Gridiv, c_CpAnOHM(DecidSurf)) = Veg_Coeff(iv5, cp_CpAnOHM) ! heat capacity, AnOHM TS
   SurfaceChar(Gridiv, c_KkAnOHM(DecidSurf)) = Veg_Coeff(iv5, cp_KkAnOHM) ! heat conductivity, AnOHM TS
   SurfaceChar(Gridiv, c_ChAnOHM(DecidSurf)) = Veg_Coeff(iv5, cp_ChAnOHM) ! bulk transfer coef., AnOHM TS

   SurfaceChar(Gridiv, c_BiogenCO2Code(ivDecid)) = Veg_Coeff(iv5, cp_BiogenCO2Code)

   ! ---- Find code for Biogenic CO2 Method ----
   CALL CodeMatchBiogen(gridiv, c_BiogenCO2Code(ivDecid))
   ! Transfer Biogenic CO2 characteristics to SurfaceChar
   SurfaceChar(gridiv, c_alpha_bioCO2(ivDecid)) = Biogen_Coeff(iv5, cB_alpha)
   SurfaceChar(gridiv, c_beta_bioCO2(ivDecid)) = Biogen_Coeff(iv5, cB_beta)
   SurfaceChar(gridiv, c_theta_bioCO2(ivDecid)) = Biogen_Coeff(iv5, cB_theta)
   SurfaceChar(gridiv, c_alpha_enh_bioCO2(ivDecid)) = Biogen_Coeff(iv5, cB_alpha_enh)
   SurfaceChar(gridiv, c_beta_enh_bioCO2(ivDecid)) = Biogen_Coeff(iv5, cB_alpha_enh)
   SurfaceChar(gridiv, c_resp_a(ivDecid)) = Biogen_Coeff(iv5, cB_resp_a)
   SurfaceChar(gridiv, c_resp_b(ivDecid)) = Biogen_Coeff(iv5, cB_resp_b)
   SurfaceChar(gridiv, c_min_res_bioCO2(ivDecid)) = Biogen_Coeff(iv5, cB_min_r)

   ! Use SoilCode for DecTr to find code for soil characteristics
   CALL CodeMatchSoil(Gridiv, c_SoilTCode(DecidSurf))
   ! Transfer soil characteristics to SurfaceChar
   SurfaceChar(gridiv, c_SoilDepth(DecidSurf)) = Soil_Coeff(iv5, cSo_SoilDepth)
   SurfaceChar(gridiv, c_SoilStCap(DecidSurf)) = Soil_Coeff(iv5, cSo_SoilStCap)
   SurfaceChar(gridiv, c_KSat(DecidSurf)) = Soil_Coeff(iv5, cSo_KSat)
   SurfaceChar(gridiv, c_SoilDens(DecidSurf)) = Soil_Coeff(iv5, cSo_SoilDens)
   SurfaceChar(gridiv, c_SoilInfRate(DecidSurf)) = Soil_Coeff(iv5, cSo_SoilInfRate)
   SurfaceChar(gridiv, c_ObsSMDepth(DecidSurf)) = Soil_Coeff(iv5, cSo_ObsSMDepth)
   SurfaceChar(gridiv, c_ObsSMMax(DecidSurf)) = Soil_Coeff(iv5, cSo_ObsSMMax)
   SurfaceChar(gridiv, c_ObsSNRFrac(DecidSurf)) = Soil_Coeff(iv5, cSo_ObsSNRFrac)
   !Get OHM characteristics for Decid
   CALL CodeMatchOHM(Gridiv, DecidSurf, 'SWet') !Summer wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SWet(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SWet(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SWet(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, DecidSurf, 'SDry') !Summer dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SDry(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SDry(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SDry(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, DecidSurf, 'WWet') !Winter wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WWet(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WWet(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WWet(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, DecidSurf, 'WDry') !Winter dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WDry(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WDry(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WDry(DecidSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)

   ! Get water distribution (within grid) for DecTr
   CALL CodeMatchDist(rr, c_WGDecTrCode, cWG_ToDecTr)
   ! Transfer distribution to SurfaceChar
   SurfaceChar(Gridiv, c_WGToPaved(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToPaved)
   SurfaceChar(Gridiv, c_WGToBldgs(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBldgs)
   SurfaceChar(Gridiv, c_WGToEveTr(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToEveTr)
   SurfaceChar(Gridiv, c_WGToDecTr(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToDecTr)
   SurfaceChar(Gridiv, c_WGToGrass(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToGrass)
   SurfaceChar(Gridiv, c_WGToBSoil(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBSoil)
   SurfaceChar(Gridiv, c_WGToWater(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToWater)
   SurfaceChar(Gridiv, c_WGToRunoff(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToRunoff)
   SurfaceChar(Gridiv, c_WGToSoilStore(DecidSurf)) = WGWaterDist_Coeff(iv5, cWG_ToSoilStore)

   ! ---- Find code for Grass surface (Pervious) ----
   CALL CodeMatchVeg(rr, c_GrassCode)
   ! Transfer characteristics to SurfaceChar for Grass surface
   ! All surfaces (1-nsurf)
   SurfaceChar(gridiv, c_AlbMin(GrassSurf)) = Veg_Coeff(iv5, cp_AlbMin)
   SurfaceChar(gridiv, c_AlbMax(GrassSurf)) = Veg_Coeff(iv5, cp_AlbMax)
   SurfaceChar(gridiv, c_Emis(GrassSurf)) = Veg_Coeff(iv5, cp_Emis)
   SurfaceChar(gridiv, c_StorMin(GrassSurf)) = Veg_Coeff(iv5, cp_StorMin)
   SurfaceChar(gridiv, c_StorMax(GrassSurf)) = Veg_Coeff(iv5, cp_StorMax)
   SurfaceChar(gridiv, c_WetThresh(GrassSurf)) = Veg_Coeff(iv5, cp_WetThresh)
   SurfaceChar(gridiv, c_StateLimit(GrassSurf)) = Veg_Coeff(iv5, cp_StateLimit)
   SurfaceChar(gridiv, c_DrEq(GrassSurf)) = Veg_Coeff(iv5, cp_DrEq)
   SurfaceChar(gridiv, c_DrCoef1(GrassSurf)) = Veg_Coeff(iv5, cp_DrCoef1)
   SurfaceChar(gridiv, c_DrCoef2(GrassSurf)) = Veg_Coeff(iv5, cp_DrCoef2)
   SurfaceChar(gridiv, c_SoilTCode(GrassSurf)) = Veg_Coeff(iv5, cp_SoilTCode)
   SurfaceChar(gridiv, c_SnowLimPat(GrassSurf)) = Veg_Coeff(iv5, cp_SnowLimPat)
   ! Veg surfaces only (1-nvegsurf)
   SurfaceChar(gridiv, c_BaseT(ivGrass)) = Veg_Coeff(iv5, cp_BaseT)
   SurfaceChar(gridiv, c_BaseTe(ivGrass)) = Veg_Coeff(iv5, cp_BaseTe)
   SurfaceChar(gridiv, c_GDDFull(ivGrass)) = Veg_Coeff(iv5, cp_GDDFull)
   SurfaceChar(gridiv, c_SDDFull(ivGrass)) = Veg_Coeff(iv5, cp_SDDFull)
   SurfaceChar(gridiv, c_LAIMin(ivGrass)) = Veg_Coeff(iv5, cp_LAIMin)
   SurfaceChar(gridiv, c_LAIMax(ivGrass)) = Veg_Coeff(iv5, cp_LAIMax)
   SurfaceChar(gridiv, c_PorosityMin(ivGrass)) = Veg_Coeff(iv5, cp_PorosityMin)
   SurfaceChar(gridiv, c_PorosityMax(ivGrass)) = Veg_Coeff(iv5, cp_PorosityMax)
   SurfaceChar(gridiv, c_GsMax(ivGrass)) = Veg_Coeff(iv5, cp_GsMax)
   SurfaceChar(gridiv, c_LAIEq(ivGrass)) = Veg_Coeff(iv5, cp_LAIEq)
   SurfaceChar(gridiv, c_LeafGP1(ivGrass)) = Veg_Coeff(iv5, cp_LeafGP1)
   SurfaceChar(gridiv, c_LeafGP2(ivGrass)) = Veg_Coeff(iv5, cp_LeafGP2)
   SurfaceChar(gridiv, c_LeafOP1(ivGrass)) = Veg_Coeff(iv5, cp_LeafOP1)
   SurfaceChar(gridiv, c_LeafOP2(ivGrass)) = Veg_Coeff(iv5, cp_LeafOP2)
   ! OHM codes
   SurfaceChar(gridiv, c_OHMCode_SWet(GrassSurf)) = Veg_Coeff(iv5, cp_OHMCode_SWet)
   SurfaceChar(gridiv, c_OHMCode_SDry(GrassSurf)) = Veg_Coeff(iv5, cp_OHMCode_SDry)
   SurfaceChar(gridiv, c_OHMCode_WWet(GrassSurf)) = Veg_Coeff(iv5, cp_OHMCode_WWet)
   SurfaceChar(gridiv, c_OHMCode_WDry(GrassSurf)) = Veg_Coeff(iv5, cp_OHMCode_WDry)
   SurfaceChar(gridiv, c_OHMThresh_SW(GrassSurf)) = Veg_Coeff(iv5, cp_OHMThresh_SW)
   SurfaceChar(gridiv, c_OHMThresh_WD(GrassSurf)) = Veg_Coeff(iv5, cp_OHMThresh_WD)
   ! ESTM code
   SurfaceChar(gridiv, c_ESTMCode(GrassSurf)) = Veg_Coeff(iv5, cp_ESTMCode)
   ! AnOHM TS
   SurfaceChar(Gridiv, c_CpAnOHM(GrassSurf)) = Veg_Coeff(iv5, cp_CpAnOHM) ! heat capacity, AnOHM TS
   SurfaceChar(Gridiv, c_KkAnOHM(GrassSurf)) = Veg_Coeff(iv5, cp_KkAnOHM) ! heat conductivity, AnOHM TS
   SurfaceChar(Gridiv, c_ChAnOHM(GrassSurf)) = Veg_Coeff(iv5, cp_ChAnOHM) ! bulk transfer coef., AnOHM TS

   SurfaceChar(Gridiv, c_BiogenCO2Code(ivGrass)) = Veg_Coeff(iv5, cp_BiogenCO2Code)

   ! ---- Find code for Biogenic CO2 Method ----
   CALL CodeMatchBiogen(gridiv, c_BiogenCO2Code(ivGrass))
   ! Transfer Biogenic CO2 characteristics to SurfaceChar
   SurfaceChar(gridiv, c_alpha_bioCO2(ivGrass)) = Biogen_Coeff(iv5, cB_alpha)
   SurfaceChar(gridiv, c_beta_bioCO2(ivGrass)) = Biogen_Coeff(iv5, cB_beta)
   SurfaceChar(gridiv, c_theta_bioCO2(ivGrass)) = Biogen_Coeff(iv5, cB_theta)
   SurfaceChar(gridiv, c_alpha_enh_bioCO2(ivGrass)) = Biogen_Coeff(iv5, cB_alpha_enh)
   SurfaceChar(gridiv, c_beta_enh_bioCO2(ivGrass)) = Biogen_Coeff(iv5, cB_alpha_enh)
   SurfaceChar(gridiv, c_resp_a(ivGrass)) = Biogen_Coeff(iv5, cB_resp_a)
   SurfaceChar(gridiv, c_resp_b(ivGrass)) = Biogen_Coeff(iv5, cB_resp_b)
   SurfaceChar(gridiv, c_min_res_bioCO2(ivGrass)) = Biogen_Coeff(iv5, cB_min_r)

   ! Use SoilCode for Grass to find code for soil characteristics
   CALL CodeMatchSoil(Gridiv, c_SoilTCode(GrassSurf))
   ! Transfer soil characteristics to SurfaceChar
   SurfaceChar(gridiv, c_SoilDepth(GrassSurf)) = Soil_Coeff(iv5, cSo_SoilDepth)
   SurfaceChar(gridiv, c_SoilStCap(GrassSurf)) = Soil_Coeff(iv5, cSo_SoilStCap)
   SurfaceChar(gridiv, c_KSat(GrassSurf)) = Soil_Coeff(iv5, cSo_KSat)
   SurfaceChar(gridiv, c_SoilDens(GrassSurf)) = Soil_Coeff(iv5, cSo_SoilDens)
   SurfaceChar(gridiv, c_SoilInfRate(GrassSurf)) = Soil_Coeff(iv5, cSo_SoilInfRate)
   SurfaceChar(gridiv, c_ObsSMDepth(GrassSurf)) = Soil_Coeff(iv5, cSo_ObsSMDepth)
   SurfaceChar(gridiv, c_ObsSMMax(GrassSurf)) = Soil_Coeff(iv5, cSo_ObsSMMax)
   SurfaceChar(gridiv, c_ObsSNRFrac(GrassSurf)) = Soil_Coeff(iv5, cSo_ObsSNRFrac)
   !Get OHM characteristics for Grass
   CALL CodeMatchOHM(Gridiv, GrassSurf, 'SWet') !Summer wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SWet(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SWet(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SWet(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, GrassSurf, 'SDry') !Summer dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SDry(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SDry(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SDry(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, GrassSurf, 'WWet') !Winter wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WWet(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WWet(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WWet(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, GrassSurf, 'WDry') !Winter dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WDry(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WDry(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WDry(GrassSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)

   ! Get water distribution (within grid) for Grass
   CALL CodeMatchDist(rr, c_WGGrassCode, cWG_ToGrass)
   ! Transfer distribution to SurfaceChar
   SurfaceChar(Gridiv, c_WGToPaved(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToPaved)
   SurfaceChar(Gridiv, c_WGToBldgs(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBldgs)
   SurfaceChar(Gridiv, c_WGToEveTr(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToEveTr)
   SurfaceChar(Gridiv, c_WGToDecTr(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToDecTr)
   SurfaceChar(Gridiv, c_WGToGrass(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToGrass)
   SurfaceChar(Gridiv, c_WGToBSoil(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBSoil)
   SurfaceChar(Gridiv, c_WGToWater(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToWater)
   SurfaceChar(Gridiv, c_WGToRunoff(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToRunoff)
   SurfaceChar(Gridiv, c_WGToSoilStore(GrassSurf)) = WGWaterDist_Coeff(iv5, cWG_ToSoilStore)

   ! ---- Find code for BSoil surface (Impervious) ----
   CALL CodeMatchNonVeg(rr, c_BSoilCode)
   ! Transfer characteristics to SurfaceChar for BSoil surface
   ! All surfaces (1-nsurf)
   SurfaceChar(gridiv, c_AlbMin(BSoilSurf)) = NonVeg_Coeff(iv5, ci_AlbMin)
   SurfaceChar(gridiv, c_AlbMax(BSoilSurf)) = NonVeg_Coeff(iv5, ci_AlbMax)
   SurfaceChar(gridiv, c_Emis(BSoilSurf)) = NonVeg_Coeff(iv5, ci_Emis)
   SurfaceChar(gridiv, c_StorMin(BSoilSurf)) = NonVeg_Coeff(iv5, ci_StorMin)
   SurfaceChar(gridiv, c_StorMax(BSoilSurf)) = NonVeg_Coeff(iv5, ci_StorMax)
   SurfaceChar(gridiv, c_WetThresh(BSoilSurf)) = NonVeg_Coeff(iv5, ci_WetThresh)
   SurfaceChar(gridiv, c_StateLimit(BSoilSurf)) = NonVeg_Coeff(iv5, ci_StateLimit)
   SurfaceChar(gridiv, c_DrEq(BSoilSurf)) = NonVeg_Coeff(iv5, ci_DrEq)
   SurfaceChar(gridiv, c_DrCoef1(BSoilSurf)) = NonVeg_Coeff(iv5, ci_DrCoef1)
   SurfaceChar(gridiv, c_DrCoef2(BSoilSurf)) = NonVeg_Coeff(iv5, ci_DrCoef2)
   SurfaceChar(gridiv, c_SoilTCode(BSoilSurf)) = NonVeg_Coeff(iv5, ci_SoilTCode)
   SurfaceChar(gridiv, c_SnowLimPat(BSoilSurf)) = NonVeg_Coeff(iv5, ci_SnowLimPat)
   SurfaceChar(gridiv, c_SnowLimRem(BSoilSurf)) = NonVeg_Coeff(iv5, ci_SnowLimRem)
   SurfaceChar(gridiv, c_OHMCode_SWet(BSoilSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_SWet)
   SurfaceChar(gridiv, c_OHMCode_SDry(BSoilSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_SDry)
   SurfaceChar(gridiv, c_OHMCode_WWet(BSoilSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_WWet)
   SurfaceChar(gridiv, c_OHMCode_WDry(BSoilSurf)) = NonVeg_Coeff(iv5, ci_OHMCode_WDry)
   SurfaceChar(gridiv, c_OHMThresh_SW(BSoilSurf)) = NonVeg_Coeff(iv5, ci_OHMThresh_SW)
   SurfaceChar(gridiv, c_OHMThresh_WD(BSoilSurf)) = NonVeg_Coeff(iv5, ci_OHMThresh_WD)
   SurfaceChar(gridiv, c_ESTMCode(BSoilSurf)) = NonVeg_Coeff(iv5, ci_ESTMCode)
   SurfaceChar(Gridiv, c_CpAnOHM(BSoilSurf)) = NonVeg_Coeff(iv5, ci_CpAnOHM) ! heat capacity, AnOHM TS
   SurfaceChar(Gridiv, c_KkAnOHM(BSoilSurf)) = NonVeg_Coeff(iv5, ci_KkAnOHM) ! heat conductivity, AnOHM TS
   SurfaceChar(Gridiv, c_ChAnOHM(BSoilSurf)) = NonVeg_Coeff(iv5, ci_ChAnOHM) ! bulk transfer coef., AnOHM TS

   ! Use SoilCode for BSoil to find code for soil characteristics
   CALL CodeMatchSoil(Gridiv, c_SoilTCode(BSoilSurf))
   ! Transfer soil characteristics to SurfaceChar
   SurfaceChar(gridiv, c_SoilDepth(BSoilSurf)) = Soil_Coeff(iv5, cSo_SoilDepth)
   SurfaceChar(gridiv, c_SoilStCap(BSoilSurf)) = Soil_Coeff(iv5, cSo_SoilStCap)
   SurfaceChar(gridiv, c_KSat(BSoilSurf)) = Soil_Coeff(iv5, cSo_KSat)
   SurfaceChar(gridiv, c_SoilDens(BSoilSurf)) = Soil_Coeff(iv5, cSo_SoilDens)
   SurfaceChar(gridiv, c_SoilInfRate(BSoilSurf)) = Soil_Coeff(iv5, cSo_SoilInfRate)
   SurfaceChar(gridiv, c_ObsSMDepth(BSoilSurf)) = Soil_Coeff(iv5, cSo_ObsSMDepth)
   SurfaceChar(gridiv, c_ObsSMMax(BSoilSurf)) = Soil_Coeff(iv5, cSo_ObsSMMax)
   SurfaceChar(gridiv, c_ObsSNRFrac(BSoilSurf)) = Soil_Coeff(iv5, cSo_ObsSNRFrac)

   ! Get OHM characteristics for BSoil
   CALL CodeMatchOHM(Gridiv, BSoilSurf, 'SWet') !Summer wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SWet(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SWet(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SWet(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, BSoilSurf, 'SDry') !Summer dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SDry(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SDry(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SDry(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, BSoilSurf, 'WWet') !Winter wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WWet(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WWet(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WWet(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, BSoilSurf, 'WDry') !Winter dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WDry(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WDry(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WDry(BSoilSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)

   ! Get water distribution (within grid) for Bare soil
   CALL CodeMatchDist(rr, c_WGBSoilCode, cWG_ToBSoil)
   ! Transfer distribution to SurfaceChar
   SurfaceChar(Gridiv, c_WGToPaved(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToPaved)
   SurfaceChar(Gridiv, c_WGToBldgs(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBldgs)
   SurfaceChar(Gridiv, c_WGToEveTr(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToEveTr)
   SurfaceChar(Gridiv, c_WGToDecTr(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToDecTr)
   SurfaceChar(Gridiv, c_WGToGrass(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToGrass)
   SurfaceChar(Gridiv, c_WGToBSoil(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBSoil)
   SurfaceChar(Gridiv, c_WGToWater(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToWater)
   SurfaceChar(Gridiv, c_WGToRunoff(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToRunoff)
   SurfaceChar(Gridiv, c_WGToSoilStore(BSoilSurf)) = WGWaterDist_Coeff(iv5, cWG_ToSoilStore)

   ! ---- Find code for Water surface (Water) ----
   CALL CodeMatchWater(rr, c_WaterCode)
   ! Transfer characteristics to SurfaceChar for Water surface
   ! All surfaces (1-nsurf)
   SurfaceChar(gridiv, c_AlbMin(WaterSurf)) = Water_Coeff(iv5, cw_AlbMin)
   SurfaceChar(gridiv, c_AlbMax(WaterSurf)) = Water_Coeff(iv5, cw_AlbMax)
   SurfaceChar(gridiv, c_Emis(WaterSurf)) = Water_Coeff(iv5, cw_Emis)
   SurfaceChar(gridiv, c_StorMin(WaterSurf)) = Water_Coeff(iv5, cw_StorMin)
   SurfaceChar(gridiv, c_StorMax(WaterSurf)) = Water_Coeff(iv5, cw_StorMax)
   SurfaceChar(gridiv, c_WetThresh(WaterSurf)) = Water_Coeff(iv5, cw_WetThresh)
   SurfaceChar(gridiv, c_StateLimit(WaterSurf)) = Water_Coeff(iv5, cw_StateLimit)
   SurfaceChar(gridiv, c_DrEq(WaterSurf)) = Water_Coeff(iv5, cw_DrEq)
   SurfaceChar(gridiv, c_DrCoef1(WaterSurf)) = Water_Coeff(iv5, cw_DrCoef1)
   SurfaceChar(gridiv, c_DrCoef2(WaterSurf)) = Water_Coeff(iv5, cw_DrCoef2)
   ! Water surface only
   SurfaceChar(gridiv, c_WaterDepth) = Water_Coeff(iv5, cw_WaterDepth)
   ! OHM codes
   SurfaceChar(gridiv, c_OHMCode_SWet(WaterSurf)) = Water_Coeff(iv5, cw_OHMCode_SWet)
   SurfaceChar(gridiv, c_OHMCode_SDry(WaterSurf)) = Water_Coeff(iv5, cw_OHMCode_SDry)
   SurfaceChar(gridiv, c_OHMCode_WWet(WaterSurf)) = Water_Coeff(iv5, cw_OHMCode_WWet)
   SurfaceChar(gridiv, c_OHMCode_WDry(WaterSurf)) = Water_Coeff(iv5, cw_OHMCode_WDry)
   SurfaceChar(gridiv, c_OHMThresh_SW(WaterSurf)) = Water_Coeff(iv5, cw_OHMThresh_SW)
   SurfaceChar(gridiv, c_OHMThresh_WD(WaterSurf)) = Water_Coeff(iv5, cw_OHMThresh_WD)
   ! ESTM code
   SurfaceChar(gridiv, c_ESTMCode(WaterSurf)) = Water_Coeff(iv5, cw_ESTMCode)
   ! AnOHM TS
   SurfaceChar(Gridiv, c_CpAnOHM(WaterSurf)) = Water_Coeff(iv5, cw_CpAnOHM) ! heat capacity, AnOHM TS
   SurfaceChar(Gridiv, c_KkAnOHM(WaterSurf)) = Water_Coeff(iv5, cw_KkAnOHM) ! heat conductivity, AnOHM TS
   SurfaceChar(Gridiv, c_ChAnOHM(WaterSurf)) = Water_Coeff(iv5, cw_ChAnOHM) ! bulk transfer coef., AnOHM TS
   ! Get OHM characteristics for Water
   CALL CodeMatchOHM(Gridiv, WaterSurf, 'SWet') !Summer wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SWet(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SWet(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SWet(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, WaterSurf, 'SDry') !Summer dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SDry(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SDry(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SDry(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, WaterSurf, 'WWet') !Winter wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WWet(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WWet(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WWet(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, WaterSurf, 'WDry') !Winter dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WDry(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WDry(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WDry(WaterSurf)) = OHMCoefficients_Coeff(iv5, cO_a3)

   ! Get water distribution (within grid) for Water
   CALL CodeMatchDist(rr, c_WGWaterCode, cWG_ToWater)
   ! Transfer distribution to SurfaceChar
   SurfaceChar(Gridiv, c_WGToPaved(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToPaved)
   SurfaceChar(Gridiv, c_WGToBldgs(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBldgs)
   SurfaceChar(Gridiv, c_WGToEveTr(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToEveTr)
   SurfaceChar(Gridiv, c_WGToDecTr(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToDecTr)
   SurfaceChar(Gridiv, c_WGToGrass(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToGrass)
   SurfaceChar(Gridiv, c_WGToBSoil(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToBSoil)
   SurfaceChar(Gridiv, c_WGToWater(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToWater)
   SurfaceChar(Gridiv, c_WGToRunoff(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToRunoff)
   SurfaceChar(Gridiv, c_WGToSoilStore(WaterSurf)) = WGWaterDist_Coeff(iv5, cWG_ToSoilStore)

   ! ---- Find code for Snow surface (Snow) ----
   CALL CodeMatchSnow(rr, c_SnowCode)
   ! Transfer characteristics to SurfaceChar for Snow surface
   SurfaceChar(gridiv, c_SnowRMFactor) = Snow_Coeff(iv5, cs_SnowRMFactor)
   SurfaceChar(gridiv, c_SnowTMFactor) = Snow_Coeff(iv5, cs_SnowTMFactor)
   SurfaceChar(gridiv, c_SnowAlbMin) = Snow_Coeff(iv5, cs_SnowAlbMin)
   SurfaceChar(gridiv, c_SnowAlbMax) = Snow_Coeff(iv5, cs_SnowAlbMax)
   !SurfaceChar(gridiv,c_SnowAlb)      = Snow_Coeff(iv5,cs_SnowAlb)
   SurfaceChar(gridiv, c_SnowEmis) = Snow_Coeff(iv5, cs_SnowEmis)
   SurfaceChar(gridiv, c_Snowtau_a) = Snow_Coeff(iv5, cs_Snowtau_a)
   SurfaceChar(gridiv, c_Snowtau_f) = Snow_Coeff(iv5, cs_Snowtau_f)
   SurfaceChar(gridiv, c_SnowPLimAlb) = Snow_Coeff(iv5, cs_SnowPLimAlb)
   SurfaceChar(gridiv, c_SnowSDMin) = Snow_Coeff(iv5, cs_SnowSDMin)
   SurfaceChar(gridiv, c_SnowSDMax) = Snow_Coeff(iv5, cs_SnowSDMax)
   SurfaceChar(gridiv, c_Snowtau_r) = Snow_Coeff(iv5, cs_Snowtau_r)
   SurfaceChar(gridiv, c_SnowCRWMin) = Snow_Coeff(iv5, cs_SnowCRWMin)
   SurfaceChar(gridiv, c_SnowCRWMax) = Snow_Coeff(iv5, cs_SnowCRWMax)
   SurfaceChar(gridiv, c_SnowPLimSnow) = Snow_Coeff(iv5, cs_SnowPLimSnow)
   SurfaceChar(gridiv, c_OHMCode_SWet(nsurf + 1)) = Snow_Coeff(iv5, cs_OHMCode_SWet)
   SurfaceChar(gridiv, c_OHMCode_SDry(nsurf + 1)) = Snow_Coeff(iv5, cs_OHMCode_SDry)
   SurfaceChar(gridiv, c_OHMCode_WWet(nsurf + 1)) = Snow_Coeff(iv5, cs_OHMCode_WWet)
   SurfaceChar(gridiv, c_OHMCode_WDry(nsurf + 1)) = Snow_Coeff(iv5, cs_OHMCode_WDry)
   SurfaceChar(gridiv, c_OHMThresh_SW(nsurf + 1)) = Snow_Coeff(iv5, cs_OHMThresh_SW)
   SurfaceChar(gridiv, c_OHMThresh_WD(nsurf + 1)) = Snow_Coeff(iv5, cs_OHMThresh_WD)

   ! ESTM code
   SurfaceChar(gridiv, c_ESTMCode(nsurf + 1)) = Snow_Coeff(iv5, cs_ESTMCode)
   !    SurfaceChar(Gridiv,c_CpAnOHM(nsurf+1))           = Snow_Coeff(iv5,cs_CpAnOHM)   ! heat capacity, AnOHM TS
   !    SurfaceChar(Gridiv,c_KkAnOHM(nsurf+1))           = Snow_Coeff(iv5,cs_KkAnOHM)  ! heat conductivity, AnOHM TS
   !    SurfaceChar(Gridiv,c_ChAnOHM(nsurf+1))           = Snow_Coeff(iv5,cs_ChAnOHM)  ! bulk transfer coef., AnOHM TS
   ! Get OHM characteristics for Snow
   CALL CodeMatchOHM(Gridiv, (nsurf + 1), 'SWet') !Summer wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SWet(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SWet(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SWet(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, (nsurf + 1), 'SDry') !Summer dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_SDry(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_SDry(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_SDry(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, (nsurf + 1), 'WWet') !Winter wet
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WWet(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WWet(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WWet(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a3)
   CALL CodeMatchOHM(Gridiv, (nsurf + 1), 'WDry') !Winter dry
   ! Transfer OHM characteristics to SurfaceChar
   SurfaceChar(Gridiv, c_a1_WDry(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a1)
   SurfaceChar(Gridiv, c_a2_WDry(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a2)
   SurfaceChar(Gridiv, c_a3_WDry(nsurf + 1)) = OHMCoefficients_Coeff(iv5, cO_a3)

   !Transfer ESTM characteristics to SurfaceChar
   DO iii = 1, (nsurf + 1)
      IF (SurfaceChar(Gridiv, c_ESTMCode(iii)) /= 0) THEN !If ESTM Code not equal to zero, use code as normal
         CALL CodeMatchESTM(Gridiv, iii)
         SurfaceChar(Gridiv, c_Surf_thick1(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick1)
         SurfaceChar(Gridiv, c_Surf_thick2(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick2)
         SurfaceChar(Gridiv, c_Surf_thick3(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick3)
         SurfaceChar(Gridiv, c_Surf_thick4(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick4)
         SurfaceChar(Gridiv, c_Surf_thick5(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick5)
         SurfaceChar(Gridiv, c_Surf_k1(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k1)
         SurfaceChar(Gridiv, c_Surf_k2(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k2)
         SurfaceChar(Gridiv, c_Surf_k3(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k3)
         SurfaceChar(Gridiv, c_Surf_k4(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k4)
         SurfaceChar(Gridiv, c_Surf_k5(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k5)
         SurfaceChar(Gridiv, c_Surf_rhoCp1(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp1)
         SurfaceChar(Gridiv, c_Surf_rhoCp2(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp2)
         SurfaceChar(Gridiv, c_Surf_rhoCp3(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp3)
         SurfaceChar(Gridiv, c_Surf_rhoCp4(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp4)
         SurfaceChar(Gridiv, c_Surf_rhoCp5(iii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp5)
         !Extra characteristics for Bldg surfaces
         IF (iii == BldgSurf) THEN
            SurfaceChar(Gridiv, c_Wall_thick1) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick1)
            SurfaceChar(Gridiv, c_Wall_thick2) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick2)
            SurfaceChar(Gridiv, c_Wall_thick3) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick3)
            SurfaceChar(Gridiv, c_Wall_thick4) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick4)
            SurfaceChar(Gridiv, c_Wall_thick5) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick5)
            SurfaceChar(Gridiv, c_Wall_k1) = ESTMCoefficients_Coeff(iv5, cE_Wall_k1)
            SurfaceChar(Gridiv, c_Wall_k2) = ESTMCoefficients_Coeff(iv5, cE_Wall_k2)
            SurfaceChar(Gridiv, c_Wall_k3) = ESTMCoefficients_Coeff(iv5, cE_Wall_k3)
            SurfaceChar(Gridiv, c_Wall_k4) = ESTMCoefficients_Coeff(iv5, cE_Wall_k4)
            SurfaceChar(Gridiv, c_Wall_k5) = ESTMCoefficients_Coeff(iv5, cE_Wall_k5)
            SurfaceChar(Gridiv, c_Wall_rhoCp1) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp1)
            SurfaceChar(Gridiv, c_Wall_rhoCp2) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp2)
            SurfaceChar(Gridiv, c_Wall_rhoCp3) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp3)
            SurfaceChar(Gridiv, c_Wall_rhoCp4) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp4)
            SurfaceChar(Gridiv, c_Wall_rhoCp5) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp5)
            SurfaceChar(Gridiv, c_Internal_thick1) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick1)
            SurfaceChar(Gridiv, c_Internal_thick2) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick2)
            SurfaceChar(Gridiv, c_Internal_thick3) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick3)
            SurfaceChar(Gridiv, c_Internal_thick4) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick4)
            SurfaceChar(Gridiv, c_Internal_thick5) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick5)
            SurfaceChar(Gridiv, c_Internal_k1) = ESTMCoefficients_Coeff(iv5, cE_Internal_k1)
            SurfaceChar(Gridiv, c_Internal_k2) = ESTMCoefficients_Coeff(iv5, cE_Internal_k2)
            SurfaceChar(Gridiv, c_Internal_k3) = ESTMCoefficients_Coeff(iv5, cE_Internal_k3)
            SurfaceChar(Gridiv, c_Internal_k4) = ESTMCoefficients_Coeff(iv5, cE_Internal_k4)
            SurfaceChar(Gridiv, c_Internal_k5) = ESTMCoefficients_Coeff(iv5, cE_Internal_k5)
            SurfaceChar(Gridiv, c_Internal_rhoCp1) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp1)
            SurfaceChar(Gridiv, c_Internal_rhoCp2) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp2)
            SurfaceChar(Gridiv, c_Internal_rhoCp3) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp3)
            SurfaceChar(Gridiv, c_Internal_rhoCp4) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp4)
            SurfaceChar(Gridiv, c_Internal_rhoCp5) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp5)
            SurfaceChar(Gridiv, c_nroom) = ESTMCoefficients_Coeff(iv5, cE_nroom)
            SurfaceChar(Gridiv, c_alb_ibld) = ESTMCoefficients_Coeff(iv5, cE_alb_ibld)
            SurfaceChar(Gridiv, c_em_ibld) = ESTMCoefficients_Coeff(iv5, cE_em_ibld)
            SurfaceChar(Gridiv, c_CH_iwall) = ESTMCoefficients_Coeff(iv5, cE_CH_iwall)
            SurfaceChar(Gridiv, c_CH_iroof) = ESTMCoefficients_Coeff(iv5, cE_CH_iroof)
            SurfaceChar(Gridiv, c_CH_ibld) = ESTMCoefficients_Coeff(iv5, cE_CH_ibld)
         END IF
         !If ESTM Code equals zero, use codes and surface fractions from SiteSelect.txt for Paved and Bldgs
      ELSEIF (iii == PavSurf .AND. SurfaceChar(Gridiv, c_ESTMCode(iii)) == 0) THEN
         DO ii = 1, 3 !for the 3x Paved ESTM classes
            CALL CodeMatchESTM_Class(Gridiv, iii, ii)
            SurfaceChar(Gridiv, c_Surf_thick1_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick1)
            SurfaceChar(Gridiv, c_Surf_thick2_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick2)
            SurfaceChar(Gridiv, c_Surf_thick3_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick3)
            SurfaceChar(Gridiv, c_Surf_thick4_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick4)
            SurfaceChar(Gridiv, c_Surf_thick5_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick5)
            SurfaceChar(Gridiv, c_Surf_k1_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k1)
            SurfaceChar(Gridiv, c_Surf_k2_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k2)
            SurfaceChar(Gridiv, c_Surf_k3_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k3)
            SurfaceChar(Gridiv, c_Surf_k4_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k4)
            SurfaceChar(Gridiv, c_Surf_k5_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k5)
            SurfaceChar(Gridiv, c_Surf_rhoCp1_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp1)
            SurfaceChar(Gridiv, c_Surf_rhoCp2_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp2)
            SurfaceChar(Gridiv, c_Surf_rhoCp3_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp3)
            SurfaceChar(Gridiv, c_Surf_rhoCp4_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp4)
            SurfaceChar(Gridiv, c_Surf_rhoCp5_Paved(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp5)
         END DO
      ELSEIF (iii == BldgSurf .AND. SurfaceChar(Gridiv, c_ESTMCode(iii)) == 0) THEN
         DO ii = 1, 5 !for the 5x Bldgs ESTM classes
            CALL CodeMatchESTM_Class(Gridiv, iii, ii)
            SurfaceChar(Gridiv, c_Surf_thick1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick1)
            SurfaceChar(Gridiv, c_Surf_thick2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick2)
            SurfaceChar(Gridiv, c_Surf_thick3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick3)
            SurfaceChar(Gridiv, c_Surf_thick4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick4)
            SurfaceChar(Gridiv, c_Surf_thick5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_thick5)
            SurfaceChar(Gridiv, c_Surf_k1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k1)
            SurfaceChar(Gridiv, c_Surf_k2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k2)
            SurfaceChar(Gridiv, c_Surf_k3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k3)
            SurfaceChar(Gridiv, c_Surf_k4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k4)
            SurfaceChar(Gridiv, c_Surf_k5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_k5)
            SurfaceChar(Gridiv, c_Surf_rhoCp1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp1)
            SurfaceChar(Gridiv, c_Surf_rhoCp2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp2)
            SurfaceChar(Gridiv, c_Surf_rhoCp3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp3)
            SurfaceChar(Gridiv, c_Surf_rhoCp4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp4)
            SurfaceChar(Gridiv, c_Surf_rhoCp5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Surf_rhoCp5)
            !Extra characteristics for Bldgs surface
            SurfaceChar(Gridiv, c_Wall_thick1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick1)
            SurfaceChar(Gridiv, c_Wall_thick2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick2)
            SurfaceChar(Gridiv, c_Wall_thick3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick3)
            SurfaceChar(Gridiv, c_Wall_thick4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick4)
            SurfaceChar(Gridiv, c_Wall_thick5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_thick5)
            SurfaceChar(Gridiv, c_Wall_k1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_k1)
            SurfaceChar(Gridiv, c_Wall_k2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_k2)
            SurfaceChar(Gridiv, c_Wall_k3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_k3)
            SurfaceChar(Gridiv, c_Wall_k4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_k4)
            SurfaceChar(Gridiv, c_Wall_k5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_k5)
            SurfaceChar(Gridiv, c_Wall_rhoCp1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp1)
            SurfaceChar(Gridiv, c_Wall_rhoCp2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp2)
            SurfaceChar(Gridiv, c_Wall_rhoCp3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp3)
            SurfaceChar(Gridiv, c_Wall_rhoCp4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp4)
            SurfaceChar(Gridiv, c_Wall_rhoCp5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Wall_rhoCp5)
            SurfaceChar(Gridiv, c_Internal_thick1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick1)
            SurfaceChar(Gridiv, c_Internal_thick2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick2)
            SurfaceChar(Gridiv, c_Internal_thick3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick3)
            SurfaceChar(Gridiv, c_Internal_thick4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick4)
            SurfaceChar(Gridiv, c_Internal_thick5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_thick5)
            SurfaceChar(Gridiv, c_Internal_k1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_k1)
            SurfaceChar(Gridiv, c_Internal_k2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_k2)
            SurfaceChar(Gridiv, c_Internal_k3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_k3)
            SurfaceChar(Gridiv, c_Internal_k4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_k4)
            SurfaceChar(Gridiv, c_Internal_k5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_k5)
            SurfaceChar(Gridiv, c_Internal_rhoCp1_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp1)
            SurfaceChar(Gridiv, c_Internal_rhoCp2_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp2)
            SurfaceChar(Gridiv, c_Internal_rhoCp3_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp3)
            SurfaceChar(Gridiv, c_Internal_rhoCp4_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp4)
            SurfaceChar(Gridiv, c_Internal_rhoCp5_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_Internal_rhoCp5)
            SurfaceChar(Gridiv, c_nroom_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_nroom)
            SurfaceChar(Gridiv, c_alb_ibld_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_alb_ibld)
            SurfaceChar(Gridiv, c_em_ibld_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_em_ibld)
            SurfaceChar(Gridiv, c_CH_iwall_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_CH_iwall)
            SurfaceChar(Gridiv, c_CH_iroof_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_CH_iroof)
            SurfaceChar(Gridiv, c_CH_ibld_Bldgs(ii)) = ESTMCoefficients_Coeff(iv5, cE_CH_ibld)
         END DO
      END IF
   END DO

   ! ---- Find code for Surface conductances ----
   CALL CodeMatchConductance(rr, c_CondCode)
   ! Transfer conductance characteristics to SurfaceChar
   SurfaceChar(gridiv, c_GsG1) = Conductance_Coeff(iv5, cc_GsG1)
   SurfaceChar(gridiv, c_GsG2) = Conductance_Coeff(iv5, cc_GsG2)
   SurfaceChar(gridiv, c_GsG3) = Conductance_Coeff(iv5, cc_GsG3)
   SurfaceChar(gridiv, c_GsG4) = Conductance_Coeff(iv5, cc_GsG4)
   SurfaceChar(gridiv, c_GsG5) = Conductance_Coeff(iv5, cc_GsG5)
   SurfaceChar(gridiv, c_GsG6) = Conductance_Coeff(iv5, cc_GsG6)
   SurfaceChar(gridiv, c_GsTH) = Conductance_Coeff(iv5, cc_GsTH)
   SurfaceChar(gridiv, c_GsTL) = Conductance_Coeff(iv5, cc_GsTL)
   SurfaceChar(gridiv, c_GsS1) = Conductance_Coeff(iv5, cc_GsS1)
   SurfaceChar(gridiv, c_GsS2) = Conductance_Coeff(iv5, cc_GsS2)
   SurfaceChar(gridiv, c_GsKmax) = Conductance_Coeff(iv5, cc_GsKmax)
   SurfaceChar(gridiv, c_gsModel) = Conductance_Coeff(iv5, cc_gsModel)

   ! ---- Find code for Anthropogenic heat ----
   CALL CodeMatchAnthropogenic(rr, c_QFCode)
   ! Transfer Anthropogenic heat characteristics to SurfaceChar
   SurfaceChar(gridiv, c_BaseT_HC) = Anthropogenic_Coeff(iv5, cA_BaseT_HC)
   SurfaceChar(gridiv, c_QF_A1) = Anthropogenic_Coeff(iv5, cA_QF_A1)
   SurfaceChar(gridiv, c_QF_B1) = Anthropogenic_Coeff(iv5, cA_QF_B1)
   SurfaceChar(gridiv, c_QF_C1) = Anthropogenic_Coeff(iv5, cA_QF_C1)
   SurfaceChar(gridiv, c_QF_A2) = Anthropogenic_Coeff(iv5, cA_QF_A2)
   SurfaceChar(gridiv, c_QF_B2) = Anthropogenic_Coeff(iv5, cA_QF_B2)
   SurfaceChar(gridiv, c_QF_C2) = Anthropogenic_Coeff(iv5, cA_QF_C2)
   SurfaceChar(gridiv, c_AHMin_WD) = Anthropogenic_Coeff(iv5, cA_AHMin_WD)
   SurfaceChar(gridiv, c_AHMin_WE) = Anthropogenic_Coeff(iv5, cA_AHMin_WE)
   SurfaceChar(gridiv, c_AHSlopeHeating_WD) = Anthropogenic_Coeff(iv5, cA_AHSlopeHeating_WD)
   SurfaceChar(gridiv, c_AHSlopeHeating_WE) = Anthropogenic_Coeff(iv5, cA_AHSlopeHeating_WE)
   SurfaceChar(gridiv, c_AHSlopeCooling_WD) = Anthropogenic_Coeff(iv5, cA_AHSlopeCooling_WD)
   SurfaceChar(gridiv, c_AHSlopeCooling_WE) = Anthropogenic_Coeff(iv5, cA_AHSlopeCooling_WE)
   SurfaceChar(gridiv, c_TCriticHeating_WD) = Anthropogenic_Coeff(iv5, cA_TCriticHeating_WD)
   SurfaceChar(gridiv, c_TCriticHeating_WE) = Anthropogenic_Coeff(iv5, cA_TCriticHeating_WE)
   SurfaceChar(gridiv, c_TCriticCooling_WD) = Anthropogenic_Coeff(iv5, cA_TCriticCooling_WD)
   SurfaceChar(gridiv, c_TCriticCooling_WE) = Anthropogenic_Coeff(iv5, cA_TCriticCooling_WE)
   SurfaceChar(gridiv, c_EnProfWD) = Anthropogenic_Coeff(iv5, cA_EnProfWD)
   SurfaceChar(gridiv, c_EnProfWE) = Anthropogenic_Coeff(iv5, cA_EnProfWE)
   SurfaceChar(gridiv, c_CO2mWD) = Anthropogenic_Coeff(iv5, cA_CO2mWD)
   SurfaceChar(gridiv, c_CO2mWE) = Anthropogenic_Coeff(iv5, cA_CO2mWE)
   SurfaceChar(gridiv, c_TraffProfWD) = Anthropogenic_Coeff(iv5, cA_TraffProfWD)
   SurfaceChar(gridiv, c_TraffProfWE) = Anthropogenic_Coeff(iv5, cA_TraffProfWE)
   SurfaceChar(gridiv, c_PopProfWD) = Anthropogenic_Coeff(iv5, cA_PopProfWD)
   SurfaceChar(gridiv, c_PopProfWE) = Anthropogenic_Coeff(iv5, cA_PopProfWE)
   SurfaceChar(gridiv, c_MinQFMetab) = Anthropogenic_Coeff(iv5, cA_MinQFMetab)
   SurfaceChar(gridiv, c_MaxQFMetab) = Anthropogenic_Coeff(iv5, cA_MaxQFMetab)
   SurfaceChar(gridiv, c_MinFCMetab) = Anthropogenic_Coeff(iv5, cA_MinFCMetab)
   SurfaceChar(gridiv, c_MaxFCMetab) = Anthropogenic_Coeff(iv5, cA_MaxFCMetab)
   SurfaceChar(gridiv, c_FrPDDwe) = Anthropogenic_Coeff(iv5, cA_FrPDDwe)
   SurfaceChar(gridiv, c_FrFossilFuel_Heat) = Anthropogenic_Coeff(iv5, cA_FrFossilFuel_Heat)
   SurfaceChar(gridiv, c_FrFossilFuel_NonHeat) = Anthropogenic_Coeff(iv5, cA_FrFossilFuel_NonHeat)
   SurfaceChar(gridiv, c_EF_umolCO2perJ) = Anthropogenic_Coeff(iv5, cA_EF_umolCO2perJ)
   SurfaceChar(gridiv, c_EnEF_v_Jkm) = Anthropogenic_Coeff(iv5, cA_EnEF_v_Jkm)
   SurfaceChar(gridiv, c_FcEF_v_kgkmWD) = Anthropogenic_Coeff(iv5, cA_FcEF_v_kgkmWD)
   SurfaceChar(gridiv, c_FcEF_v_kgkmWE) = Anthropogenic_Coeff(iv5, cA_FcEF_v_kgkmWE)
   SurfaceChar(gridiv, c_CO2PointSource) = Anthropogenic_Coeff(iv5, cA_CO2PointSource)
   SurfaceChar(gridiv, c_TrafficUnits) = Anthropogenic_Coeff(iv5, cA_TrafficUnits)

   ! ---- Find code for Irrigation ----
   CALL CodeMatchIrrigation(rr, c_IrrCode)
   ! Transfer Irrigation characteristics to SurfaceChar
   SurfaceChar(gridiv, c_IeStart) = Irrigation_Coeff(iv5, cIr_IeStart)
   SurfaceChar(gridiv, c_IeEnd) = Irrigation_Coeff(iv5, cIr_IeEnd)
   SurfaceChar(gridiv, c_IntWU) = Irrigation_Coeff(iv5, cIr_IntWU)
   SurfaceChar(gridiv, c_Faut) = Irrigation_Coeff(iv5, cIr_Faut)
   SurfaceChar(gridiv, c_h_maintain) = Irrigation_Coeff(iv5, cIr_H_maintain)
   SurfaceChar(gridiv, c_Ie_a) = Irrigation_Coeff(iv5, cIr_Ie_a1:cIr_Ie_a3)
   SurfaceChar(gridiv, c_Ie_m) = Irrigation_Coeff(iv5, cIr_Ie_m1:cIr_Ie_m3)
   SurfaceChar(gridiv, c_DayWat) = Irrigation_Coeff(iv5, cIr_DayWat1:cIr_DayWat7)
   SurfaceChar(gridiv, c_DayWatPer) = Irrigation_Coeff(iv5, cIr_DayWatPer1:cIr_DayWatPer7)

   ! ---- Find code for Hourly Profiles ----
   ! Energy use (weekdays)
   CALL CodeMatchProf(gridiv, c_EnProfWD)
   SurfaceChar(gridiv, c_HrProfEnUseWD) = Profiles_Coeff(iv5, cPr_Hours)
   ! Energy use (weekends)
   CALL CodeMatchProf(gridiv, c_EnProfWE)
   SurfaceChar(gridiv, c_HrProfEnUseWE) = Profiles_Coeff(iv5, cPr_Hours)
   ! Water use profile (manual, weekdays)
   CALL CodeMatchProf(gridiv, c_WProfManuWD)
   SurfaceChar(gridiv, c_HrProfWUManuWD) = Profiles_Coeff(iv5, cPr_Hours)
   ! Water use profile (manual, weekends)
   CALL CodeMatchProf(gridiv, c_WProfManuWE)
   SurfaceChar(gridiv, c_HrProfWUManuWE) = Profiles_Coeff(iv5, cPr_Hours)
   ! Water use profile (automatic, weekdays)
   CALL CodeMatchProf(gridiv, c_WProfAutoWD)
   SurfaceChar(gridiv, c_HrProfWUAutoWD) = Profiles_Coeff(iv5, cPr_Hours)
   ! Water use profile (automatic, weekends)
   CALL CodeMatchProf(gridiv, c_WProfAutoWE)
   SurfaceChar(gridiv, c_HrProfWUAutoWE) = Profiles_Coeff(iv5, cPr_Hours)
   ! Snow clearing profile (weekdays)
   CALL CodeMatchProf(gridiv, c_SnowProfWD)
   SurfaceChar(gridiv, c_HrProfSnowCWD) = Profiles_Coeff(iv5, cPr_Hours)
   ! Snow clearing profile (weekends)
   CALL CodeMatchProf(gridiv, c_SnowProfWE)
   SurfaceChar(gridiv, c_HrProfSnowCWE) = Profiles_Coeff(iv5, cPr_Hours)
   !Human activity (weekdays)
   CALL CodeMatchProf(gridiv, c_CO2mWD)
   SurfaceChar(gridiv, c_HrProfHumActivityWD) = Profiles_Coeff(iv5, cPr_Hours)
   !Human activity (weekends)
   CALL CodeMatchProf(gridiv, c_CO2mWE)
   SurfaceChar(gridiv, c_HrProfHumActivityWE) = Profiles_Coeff(iv5, cPr_Hours)
   !Traffic (weekdays)
   CALL CodeMatchProf(gridiv, c_TraffProfWD)
   SurfaceChar(gridiv, c_HrProfTraffWD) = Profiles_Coeff(iv5, cPr_Hours)
   !Traffic (weekends)
   CALL CodeMatchProf(gridiv, c_TraffProfWE)
   SurfaceChar(gridiv, c_HrProfTraffWE) = Profiles_Coeff(iv5, cPr_Hours)
   !Population (weekdays)
   CALL CodeMatchProf(gridiv, c_PopProfWD)
   SurfaceChar(gridiv, c_HrProfPopWD) = Profiles_Coeff(iv5, cPr_Hours)
   !Population (weekends)
   CALL CodeMatchProf(gridiv, c_PopProfWE)
   SurfaceChar(gridiv, c_HrProfPopWE) = Profiles_Coeff(iv5, cPr_Hours)

   ! TS 05 Jul 2018: No longer needed as interpolation is done through specific subroutines at each required instant
   ! the below is commented out by TS 05 Jul 2018
   ! ! ---- Interpolate Hourly Profiles to model timestep and normalise
   ! TstepProfiles(Gridiv,:,:) = -999   !Initialise TstepProfiles
   ! ! Energy use
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_EnUseWD,c_HrProfEnUseWD)
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_EnUseWE,c_HrProfEnUseWE)
   !
   ! ! For energy use, normalise so the AVERAGE of the multipliers is equal to 1
   ! TstepProfiles(Gridiv,cTP_EnUseWD,:) = TstepProfiles(Gridiv,cTP_EnUseWD,:) / SUM(TstepProfiles(Gridiv,cTP_EnUseWD,:))*24*nsh_real
   ! TstepProfiles(Gridiv,cTP_EnUseWE,:) = TstepProfiles(Gridiv,cTP_EnUseWE,:) / SUM(TstepProfiles(Gridiv,cTP_EnUseWE,:))*24*nsh_real
   !
   ! ! Water use
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_WUManuWD,c_HrProfWUManuWD)
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_WUManuWE,c_HrProfWUManuWE)
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_WUAutoWD,c_HrProfWUAutoWD)
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_WUAutoWE,c_HrProfWUAutoWE)
   ! ! For water use, normalise so the SUM of the multipliers is equal to 1 (profile is multiplied by daily water use)
   ! TstepProfiles(Gridiv,cTP_WUManuWD,:) = TstepProfiles(Gridiv,cTP_WUManuWD,:) / SUM(TstepProfiles(Gridiv,cTP_WUManuWD,:))
   ! TstepProfiles(Gridiv,cTP_WUManuWE,:) = TstepProfiles(Gridiv,cTP_WUManuWE,:) / SUM(TstepProfiles(Gridiv,cTP_WUManuWE,:))
   ! TstepProfiles(Gridiv,cTP_WUAutoWD,:) = TstepProfiles(Gridiv,cTP_WUAutoWD,:) / SUM(TstepProfiles(Gridiv,cTP_WUAutoWD,:))
   ! TstepProfiles(Gridiv,cTP_WUAutoWE,:) = TstepProfiles(Gridiv,cTP_WUAutoWE,:) / SUM(TstepProfiles(Gridiv,cTP_WUAutoWE,:))
   !
   ! ! Human activity for CO2 calculations
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_HumActivityWD,c_HrProfHumActivityWD)
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_HumActivityWE,c_HrProfHumActivityWE)
   !
   ! ! For human activity, check values are between 1 (night) and 2 (day)
   ! IF(ANY(TstepProfiles(Gridiv,cTP_HumActivityWD,:) < 1 .OR. TstepProfiles(Gridiv,cTP_HumActivityWD,:) > 2)) THEN
   !    CALL ErrorHint(70,'Profile value for human activity (WD) exceeds allowed range 1-2.',NotUsed,NotUsed,notUsedI)
   ! ENDIF
   ! IF(ANY(TstepProfiles(Gridiv,cTP_HumActivityWE,:) < 1 .OR. TstepProfiles(Gridiv,cTP_HumActivityWE,:) > 2)) THEN
   !    CALL ErrorHint(70,'Profile value for human activity (WE) exceeds allowed range 1-2.',NotUsed,NotUsed,notUsedI)
   ! ENDIF
   !
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_TraffProfWD,c_HrProfTraffWD)
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_TraffProfWE,c_HrProfTraffWE)
   ! ! For traffic, normalise so the AVERAGE of the multipliers is equal to 1
   ! TstepProfiles(Gridiv,cTP_TraffProfWD,:) = TstepProfiles(Gridiv,cTP_TraffProfWD,:) &
   !      / SUM(TstepProfiles(Gridiv,cTP_TraffProfWD,:))*24*nsh_real
   ! TstepProfiles(Gridiv,cTP_TraffProfWE,:) = TstepProfiles(Gridiv,cTP_TraffProfWE,:) &
   !      / SUM(TstepProfiles(Gridiv,cTP_TraffProfWE,:))*24*nsh_real
   !
   ! ! Population for CO2 calculations
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_PopProfWD,c_HrProfPopWD)
   ! CALL SUEWS_InterpHourlyProfiles(Gridiv,cTP_PopProfWE,c_HrProfPopWE)

END SUBROUTINE InitializeSurfaceCharacteristics

!----------------------------------------------------------------------------------------------
!Calculates the initial conditions for each grid on the first year of run
!Made by sg feb 2012 -
!Latest modified:
!20 Oct 2014, LJ: Saves to slot 0 of the output matrix
!06 Nov 2014 HCW
!----------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------
SUBROUTINE InitialState(GridName, year_int, Gridiv, NumberOfGrids)
   ! Last modified HCW 13 Jan 2017 - Major changes to InitialConditions file
   ! Last modified HCW 24 May 2016 - Removed unused argument year_txt
   ! Last modified HCW 03 Jul 2015 - Added initial conditions albEveTr0 and albGrass0
   ! Last modified HCW 03 Dec 2014
   !------------------------------------------------------------------------

   USE AllocateArray
   USE data_in
   USE ColNamesModelDailyState
   USE defaultNotUsed
   USE FileName
   USE gis_data
   USE InitialCond
   USE mod_z
   USE resist
   USE snowMod
   USE sues_data
   USE time
   USE InitialCond
   USE DailyState_module, ONLY: update_WaterUse
   USE resist_module, ONLY: SUEWS_cal_RoughnessParameters

   IMPLICIT NONE

   CHARACTER(len=20) :: GridName !Name of the evaluated grid
   CHARACTER(len=4) :: year_txt
   INTEGER :: NumberOfGrids

   CHARACTER(len=150) :: fileInit !Initial conditions filename
   INTEGER :: DaysSinceRain, Gridiv, & !number of days since rain, grid number,
              gamma1, gamma2 !switches related to cooling and heating degree days
   INTEGER :: wd, seas, date, mb, & !weekday information, season, date, month
              year_int, switch = 0, & !year as an integer, switch related to previous day
              id_next !next day,counter in irrigation calculations

   REAL(KIND(1D0)) :: PavedState, BldgsState, EveTrState, DecTrState, GrassState, BSoilState, WaterState, &
                      SnowFracPaved, SnowFracBldgs, SnowFracEveTr, SnowFracDecTr, &
                      SnowFracGrass, SnowFracBSoil, SnowFracWater, &
                      SnowDensPaved, SnowDensBldgs, SnowDensEveTr, SnowDensDecTr, &
                      SnowDensGrass, SnowDensBSoil, SnowDensWater

   INTEGER :: LeavesOutInitially !Allows for quick setting of veg-related initial conditions for full leaf-out (1) or leaf-off (0)
   INTEGER :: SnowInitially !Allows for quick setting of snow-related initial conditions for no snow initially (0)

   INTEGER :: GridsInitialised = 0 ! Number of grids initialised at start of model run
   INTEGER :: YearsInitialised = 0 ! Number of years initialised at start of model run
   INTEGER :: dayofWeek_id(3)
   REAL(KIND(1D0)) :: NormalizeVegChar !Function

   ! Define InitialConditions namelist ---------------------------------------
   NAMELIST /InitialConditions/ DaysSinceRain, &
      Temp_C0, &
      !ID_Prev,&  !Now calculated from met forcing file
      LeavesOutInitially, &
      GDD_1_0, &
      GDD_2_0, &
      LAIinitialEveTr, &
      LAIinitialDecTr, &
      LAIinitialGrass, &
      AlbEveTr0, &
      AlbDecTr0, &
      AlbGrass0, &
      DecidCap0, &
      Porosity0, &
      PavedState, &
      BldgsState, &
      EveTrState, &
      DecTrState, &
      GrassState, &
      BSoilState, &
      WaterState, &
      SoilStorePavedState, &
      SoilStoreBldgsState, &
      SoilStoreEveTrState, &
      SoilStoreDecTrState, &
      SoilStoreGrassState, &
      SoilStoreBSoilState, &
      SnowInitially, &
      SnowWaterPavedState, &
      SnowWaterBldgsState, &
      SnowWaterEveTrState, &
      SnowWaterDecTrState, &
      SnowWaterGrassState, &
      SnowWaterBSoilState, &
      SnowWaterWaterState, &
      SnowPackPaved, &
      SnowPackBldgs, &
      SnowPackEveTr, &
      SnowPackDecTr, &
      SnowPackGrass, &
      SnowPackBSoil, &
      SnowPackWater, &
      SnowFracPaved, &
      SnowFracBldgs, &
      SnowFracEveTr, &
      SnowFracDecTr, &
      SnowFracGrass, &
      SnowFracBSoil, &
      SnowFracWater, &
      SnowDensPaved, &
      SnowDensBldgs, &
      SnowDensEveTr, &
      SnowDensDecTr, &
      SnowDensGrass, &
      SnowDensBSoil, &
      SnowDensWater, &
      SnowAlb0 !,&
   ! BoInit ! removed as no longer needed by AnOHM

   ! Initialise namelist to NAN ----------------------------------------------
   DaysSinceRain = INT(NAN)
   Temp_C0 = NAN
   LeavesOutInitially = INT(NAN)
   GDD_1_0 = NAN
   GDD_2_0 = NAN
   LAIinitialEveTr = NAN
   LAIinitialDecTr = NAN
   LAIinitialGrass = NAN
   AlbEveTr0 = NAN
   AlbDecTr0 = NAN
   AlbGrass0 = NAN
   DecidCap0 = NAN
   Porosity0 = NAN
   PavedState = NAN
   BldgsState = NAN
   EveTrState = NAN
   DecTrState = NAN
   GrassState = NAN
   BSoilState = NAN
   WaterState = NAN
   SoilStorePavedState = NAN
   SoilStoreBldgsState = NAN
   SoilStoreEveTrState = NAN
   SoilStoreDecTrState = NAN
   SoilStoreGrassState = NAN
   SoilStoreBSoilState = NAN
   SnowInitially = INT(NAN)
   SnowWaterPavedState = NAN
   SnowWaterBldgsState = NAN
   SnowWaterEveTrState = NAN
   SnowWaterDecTrState = NAN
   SnowWaterGrassState = NAN
   SnowWaterBSoilState = NAN
   SnowWaterWaterState = NAN
   SnowPackPaved = NAN
   SnowPackBldgs = NAN
   SnowPackEveTr = NAN
   SnowPackDecTr = NAN
   SnowPackGrass = NAN
   SnowPackBSoil = NAN
   SnowPackWater = NAN
   SnowFracPaved = NAN
   SnowFracBldgs = NAN
   SnowFracEveTr = NAN
   SnowFracDecTr = NAN
   SnowFracGrass = NAN
   SnowFracBSoil = NAN
   SnowFracWater = NAN
   SnowDensPaved = NAN
   SnowDensBldgs = NAN
   SnowDensEveTr = NAN
   SnowDensDecTr = NAN
   SnowDensGrass = NAN
   SnowDensBSoil = NAN
   SnowDensWater = NAN
   SnowAlb0 = NAN
   ! BoInit=NAN

   WRITE (year_txt, '(I4)') year_int !Get year as a text string

   ! Define InitialConditions file -------------------------------------------
   FileInit = TRIM(FileInputPath)//TRIM("InitialConditions")//TRIM(GridName)//'.nml'
   ! On very first InitialConditions for each grid, can use one initial conditions file specified for all grids
   IF (MultipleInitFiles == 0 .AND. YearsInitialised == 0) THEN
      FileInit = TRIM(FileInputPath)//TRIM("InitialConditions")//TRIM(FileCode)//'_'//TRIM(year_txt)//'.nml'
      GridsInitialised = GridsInitialised + 1
      IF (GridsInitialised == NumberOfGrids) THEN
         YearsInitialised = YearsInitialised + 1
         GridsInitialised = 0 !reset GridsInitialised
      END IF
   END IF
   !write(*,*) TRIM(FileInit)

   ! Open, read and close InitialConditions file -----------------------------
   OPEN (56, File=TRIM(FileInit), err=600, status='old')
   READ (56, iostat=ios_out, nml=InitialConditions, err=601)
   CLOSE (56)

   ! Write InitialConditions to FileChoices ----------------------------------
   FileChoices = TRIM(FileOutputPath)//TRIM(FileCode)//'_FileChoices.txt'
   OPEN (12, file=FileChoices, position='append')
   WRITE (12, *) '----- '//TRIM("InitialConditions")//TRIM(GridName)//'.nml'//' -----'
   WRITE (12, nml=InitialConditions)
   CLOSE (12)

   !--------------------------------------------------------------------------
   ! Check initial conditions and assign values if not provided --------------

   ! Calculate previous day --------------------------------------------------
   id_prev = INT(MetForcingData(1, 2, Gridiv)) - 1

   ! If no. days since rainfall unknown, set to zero -------------------------
   IF (DaysSinceRain == INT(NAN)) DaysSinceRain = 0

   ! If average temperature for previous day unknown, use average of first day
   IF (Temp_C0 == NAN) Temp_C0 = SUM(MetForcingData(1:(24*nsh), 12, Gridiv))/(24*nsh)

   ! Set vegetation-related initial conditions -------------------------------
   ! If LeavesOutInitially is -999, don't use and check all required conditions have been provided
   IF (LeavesOutInitially == INT(NAN)) THEN
      IF (GDD_1_0 == NAN .OR. GDD_2_0 == NAN) THEN
         CALL ErrorHint(36, 'Specify values for GDD_1_0 and GDD_2_0.', notUsed, notUsed, notUsedI)
      END IF
      IF (LAIinitialEveTr == NAN .OR. LAIinitialDecTr == NAN .OR. LAIinitialGrass == NAN) THEN
         CALL ErrorHint(36, 'Specify initial values for LAI for all vegetated surface types.', notUsed, notUsed, notUsedI)
      END IF
      IF (AlbEveTr0 == NAN .OR. AlbDecTr0 == NAN .OR. AlbGrass0 == NAN) THEN
         CALL ErrorHint(36, 'Specify initial values for albedo for all vegetated surface types.', notUsed, notUsed, notUsedI)
      END IF
      IF (DecidCap0 == NAN) THEN
         CALL ErrorHint(36, 'Specify DecidCap0.', notUsed, notUsed, notUsedI)
      END IF
      IF (Porosity0 == NAN) THEN
         CALL ErrorHint(36, 'Specify Porosity0.', notUsed, notUsed, notUsedI)
      END IF
   ELSEIF (LeavesOutInitially == 1) THEN !If leaves out, set to summertime values using SUEWS_Veg.txt
      GDD_1_0 = NormalizeVegChar(c_GDDFull, Gridiv)
      GDD_2_0 = 0
      LAIinitialEveTr = SurfaceChar(Gridiv, c_LAIMax(ivConif)) !Max LAI
      LAIinitialDecTr = SurfaceChar(Gridiv, c_LAIMax(ivDecid))
      LAIinitialGrass = SurfaceChar(Gridiv, c_LAIMax(ivGrass))
      AlbEveTr0 = SurfaceChar(Gridiv, c_AlbMax(ConifSurf)) !Max albedo
      AlbDecTr0 = SurfaceChar(Gridiv, c_AlbMax(DecidSurf))
      AlbGrass0 = SurfaceChar(Gridiv, c_AlbMax(GrassSurf))
      DecidCap0 = SurfaceChar(Gridiv, c_StorMax(DecidSurf)) !Max storage capacity (DecTr only)
      Porosity0 = SurfaceChar(Gridiv, c_PorosityMin(ivDecid)) !Min porosity (DecTr only)
   ELSEIF (LeavesOutInitially == 0) THEN !If leaves off, set to wintertime values using SUEWS_Veg.txt
      GDD_1_0 = 0
      GDD_2_0 = NormalizeVegChar(c_SDDFull, Gridiv)
      LAIinitialEveTr = SurfaceChar(Gridiv, c_LAIMin(ivConif)) !Min LAI
      LAIinitialDecTr = SurfaceChar(Gridiv, c_LAIMin(ivDecid))
      LAIinitialGrass = SurfaceChar(Gridiv, c_LAIMin(ivGrass))
      AlbEveTr0 = SurfaceChar(Gridiv, c_AlbMin(ConifSurf)) !Min albedo
      AlbDecTr0 = SurfaceChar(Gridiv, c_AlbMin(DecidSurf))
      AlbGrass0 = SurfaceChar(Gridiv, c_AlbMin(GrassSurf))
      DecidCap0 = SurfaceChar(Gridiv, c_StorMin(DecidSurf)) !Min storage capacity (DecTr only)
      Porosity0 = SurfaceChar(Gridiv, c_PorosityMax(ivDecid)) !Max porosity (DecTr only)
   ELSE
      CALL ErrorHint(36, 'LeavesOutInitially must be 0, 1, or -999 (or omitted from InitialConditions namelist)', &
                     notUsed, notUsed, notUsedI)
   END IF

   ! If surface wetness states unknown, set to zero --------------------------
   IF (PavedState == NAN) PavedState = 0
   IF (BldgsState == NAN) BldgsState = 0
   IF (EveTrState == NAN) EveTrState = 0
   IF (DecTrState == NAN) DecTrState = 0
   IF (GrassState == NAN) GrassState = 0
   IF (BSoilState == NAN) BSoilState = 0
   ! except for water surface - set using WaterDepth in SUEWS_Water.txt
   IF (WaterState == NAN) WaterState = SurfaceChar(Gridiv, c_WaterDepth)

   ! Check initial soil moisture states are provided -------------------------
   IF (SoilStorePavedState == NAN .OR. SoilStoreBldgsState == NAN .OR. SoilStoreEveTrState == NAN .OR. &
       SoilStoreDecTrState == NAN .OR. SoilStoreGrassState == NAN .OR. SoilStoreBSoilState == NAN) THEN
      CALL ErrorHint(36, 'Initial soil moisture must be provided for all surface types except water.', notUsed, notUsed, notUsedI)
   END IF

   ! Set snow-related initial conditions -------------------------------
   ! If snow part not used, or no snow initially, set all snow-related initial conditions to zero
   IF (SnowUse == 0 .OR. SnowInitially == 0) THEN
      SnowWaterPavedState = 0
      SnowWaterBldgsState = 0
      SnowWaterEveTrState = 0
      SnowWaterDecTrState = 0
      SnowWaterGrassState = 0
      SnowWaterBSoilState = 0
      SnowWaterWaterState = 0
      SnowPackPaved = 0
      SnowPackBldgs = 0
      SnowPackEveTr = 0
      SnowPackDecTr = 0
      SnowPackGrass = 0
      SnowPackBSoil = 0
      SnowPackWater = 0
      SnowFracPaved = 0
      SnowFracBldgs = 0
      SnowFracEveTr = 0
      SnowFracDecTr = 0
      SnowFracGrass = 0
      SnowFracBSoil = 0
      SnowFracWater = 0
      SnowDensPaved = 0
      SnowDensBldgs = 0
      SnowDensEveTr = 0
      SnowDensDecTr = 0
      SnowDensGrass = 0
      SnowDensBSoil = 0
      SnowDensWater = 0
      SnowAlb0 = 0
   ELSEIF (SnowInitially == INT(NAN)) THEN !Check all required snow-related conditions are provided
      IF (SnowWaterPavedState == NAN .OR. SnowWaterBldgsState == NAN .OR. SnowWaterEveTrState == NAN .OR. &
          SnowWaterDecTrState == NAN .OR. SnowWaterGrassState == NAN .OR. SnowWaterBSoilState == NAN .OR. &
          SnowWaterWaterState == NAN) THEN
         CALL ErrorHint(36, 'Specify SnowWater state for all 7 surface types.', notUsed, notUsed, notUsedI)
      END IF
      IF (SnowPackPaved == NAN .OR. SnowPackBldgs == NAN .OR. SnowPackEveTr == NAN .OR. &
          SnowPackDecTr == NAN .OR. SnowPackGrass == NAN .OR. SnowPackBSoil == NAN .OR. &
          SnowPackWater == NAN) THEN
         CALL ErrorHint(36, 'Specify SnowPack for all 7 surface types.', notUsed, notUsed, notUsedI)
      END IF
      IF (SnowFracPaved == NAN .OR. SnowFracBldgs == NAN .OR. SnowFracEveTr == NAN .OR. &
          SnowFracDecTr == NAN .OR. SnowFracGrass == NAN .OR. SnowFracBSoil == NAN .OR. &
          SnowFracWater == NAN) THEN
         CALL ErrorHint(36, 'Specify SnowFrac for all 7 surface types.', notUsed, notUsed, notUsedI)
      END IF
      IF (SnowDensPaved == NAN .OR. SnowDensBldgs == NAN .OR. SnowDensEveTr == NAN .OR. &
          SnowDensDecTr == NAN .OR. SnowDensGrass == NAN .OR. SnowDensBSoil == NAN .OR. &
          SnowDensWater == NAN) THEN
         CALL ErrorHint(36, 'Specify SnowDens for all 7 surface types.', notUsed, notUsed, notUsedI)
      END IF
      IF (SnowAlb0 == NAN) THEN
         CALL ErrorHint(36, 'Specify SnowAlb0.', notUsed, notUsed, notUsedI)
      END IF
   ELSE
      CALL ErrorHint(36, 'SnowInitially must be 0 or -999 (or omitted from InitialConditions namelist)', &
                     notUsed, notUsed, notUsedI)
   END IF

   ! removed as no longer needed, TS 30 Jan 2018
   ! ! If AnOHM option selected, check initial Bowen ratio is provided ---------
   ! IF(StorageHeatMethod==3 .AND. BoInit == NAN) THEN
   !    CALL ErrorHint(36,'Specify BoInit for AnOHM calculations.', notUsed,notUsed,notUsedI)
   ! ENDIF

   ! -------------------------------------------------------------------------
   ! -------------------------------------------------------------------------

   ! Previous day DOY number (needed in file allocations)
   IF (id_prev >= 364) id_prev = 0 !If previous day is larger than 364, set this to zero

   ! Save initial conditions to ModelDailyState array ------------------------
   ModelDailyState(Gridiv, cMDS_id_prev) = id_prev
   ModelDailyState(Gridiv, cMDS_LAIInitialEveTr) = LAIInitialEveTr
   ModelDailyState(Gridiv, cMDS_LAIInitialDecTr) = LAIInitialDecTr
   ModelDailyState(Gridiv, cMDS_LAIInitialGrass) = LAIInitialGrass
   ModelDailyState(Gridiv, cMDS_GDD1_0) = GDD_1_0
   ModelDailyState(Gridiv, cMDS_GDD2_0) = GDD_2_0
   ModelDailyState(Gridiv, cMDS_GDDMin) = 90 !QUESTION: Going to check for minimum GDD
   ModelDailyState(Gridiv, cMDS_GDDMax) = -90 !QUESTION: Going to check for maximum GDD
   ModelDailyState(Gridiv, cMDS_albEveTr) = AlbEveTr0
   ModelDailyState(Gridiv, cMDS_albDecTr) = AlbDecTr0
   ModelDailyState(Gridiv, cMDS_albGrass) = AlbGrass0
   ModelDailyState(Gridiv, cMDS_porosity) = Porosity0
   ModelDailyState(Gridiv, cMDS_DecidCap) = DecidCap0

   ModelDailyState(Gridiv, cMDS_SnowfallCum) = 0 !!Check this

   ModelDailyState(Gridiv, cMDS_DaysSinceRain) = REAL(DaysSinceRain, KIND(1D0))
   ModelDailyState(Gridiv, cMDS_TempC) = Temp_C0
   ! Assume that the temperature has been the same for the previous days
   ModelDailyState(Gridiv, cMDS_TempCOld1) = Temp_C0
   ModelDailyState(Gridiv, cMDS_TempCOld2) = Temp_C0
   ModelDailyState(Gridiv, cMDS_TempCOld3) = Temp_C0

   ! -- Anthropogenic heat flux initializations --
   ! Need to get BaseT_HC from SurfaceChar, as info not transferred until SUEWS_Translate called
   BaseT_HC = SurfaceChar(Gridiv, c_BaseT_HC)

   IF (EmissionsMethod >= 0) THEN
      !Calculations related to heating and cooling degree days (BaseT_HC is used always)
      IF ((Temp_C0 - BaseT_HC) >= 0) THEN !Cooling
         gamma2 = 1
      ELSE
         gamma2 = 0
      END IF
      IF ((BaseT_HC - Temp_C0) >= 0) THEN !Heating
         gamma1 = 1
      ELSE
         gamma1 = 0
      END IF
      ModelDailyState(Gridiv, cMDS_HDD1) = gamma1*(BaseT_HC - Temp_C0) ! Heating
      ModelDailyState(Gridiv, cMDS_HDD2) = gamma2*(Temp_C0 - BaseT_HC) ! Cooling
   END IF

   ! -- Save snow density and snow albedo info in InitialConditions to ModelDailyState array --
   ModelDailyState(Gridiv, cMDS_SnowDens(PavSurf)) = SnowDensPaved
   ModelDailyState(Gridiv, cMDS_SnowDens(BldgSurf)) = SnowDensBldgs
   ModelDailyState(Gridiv, cMDS_SnowDens(ConifSurf)) = SnowDensEveTr
   ModelDailyState(Gridiv, cMDS_SnowDens(DecidSurf)) = SnowDensDecTr
   ModelDailyState(Gridiv, cMDS_SnowDens(GrassSurf)) = SnowDensGrass
   ModelDailyState(Gridiv, cMDS_SnowDens(BSoilSurf)) = SnowDensBSoil
   ModelDailyState(Gridiv, cMDS_SnowDens(WaterSurf)) = SnowDensWater

   ModelDailyState(Gridiv, cMDS_SnowAlb) = SnowAlb0

   ! -------------------------------------------------------------------------

   ! Saving to ModelOutputData array -----------------------------------------

   ! -- Initial wetness status of each surface (above ground) --
   ModelOutputData(0, cMOD_State(PavSurf), Gridiv) = PavedState
   ModelOutputData(0, cMOD_State(BldgSurf), Gridiv) = BldgsState
   ModelOutputData(0, cMOD_State(ConifSurf), Gridiv) = EveTrState
   ModelOutputData(0, cMOD_State(DecidSurf), Gridiv) = DecTrState
   ModelOutputData(0, cMOD_State(GrassSurf), Gridiv) = GrassState
   ModelOutputData(0, cMOD_State(BSoilSurf), Gridiv) = BSoilState
   ModelOutputData(0, cMOD_State(WaterSurf), Gridiv) = WaterState

   ! -- Initial soil stores for each surface (below ground) --
   ModelOutputData(0, cMOD_SoilState(PavSurf), Gridiv) = SoilStorePavedState
   ModelOutputData(0, cMOD_SoilState(BldgSurf), Gridiv) = SoilStoreBldgsState
   ModelOutputData(0, cMOD_SoilState(ConifSurf), Gridiv) = SoilStoreEveTrstate
   ModelOutputData(0, cMOD_SoilState(DecidSurf), Gridiv) = SoilStoreDecTrState
   ModelOutputData(0, cMOD_SoilState(GrassSurf), Gridiv) = SoilStoreGrassState
   ModelOutputData(0, cMOD_SoilState(BSoilSurf), Gridiv) = SoilStoreBSoilState
   ModelOutputData(0, cMOD_SoilState(WaterSurf), Gridiv) = 0 ! No soil layer for water surface

   ! -- Initial liquid (melted) water for each surface --
   ModelOutputData(0, cMOD_SnowWaterState(PavSurf), Gridiv) = SnowWaterPavedState
   ModelOutputData(0, cMOD_SnowWaterState(BldgSurf), Gridiv) = SnowWaterBldgsState
   ModelOutputData(0, cMOD_SnowWaterState(ConifSurf), Gridiv) = SnowWaterEveTrstate
   ModelOutputData(0, cMOD_SnowWaterState(DecidSurf), Gridiv) = SnowWaterDecTrState
   ModelOutputData(0, cMOD_SnowWaterState(GrassSurf), Gridiv) = SnowWaterGrassState
   ModelOutputData(0, cMOD_SnowWaterState(BSoilSurf), Gridiv) = SnowWaterBSoilState
   ModelOutputData(0, cMOD_SnowWaterState(WaterSurf), Gridiv) = SnowWaterWaterState

   ! -- Initial snow water equivalent for each surface --
   ModelOutputData(0, cMOD_SnowPack(PavSurf), Gridiv) = SnowPackPaved
   ModelOutputData(0, cMOD_SnowPack(BldgSurf), Gridiv) = SnowPackBldgs
   ModelOutputData(0, cMOD_SnowPack(ConifSurf), Gridiv) = SnowPackEveTr
   ModelOutputData(0, cMOD_SnowPack(DecidSurf), Gridiv) = SnowPackDecTr
   ModelOutputData(0, cMOD_SnowPack(GrassSurf), Gridiv) = SnowPackGrass
   ModelOutputData(0, cMOD_SnowPack(BSoilSurf), Gridiv) = SnowPackBSoil
   ModelOutputData(0, cMOD_SnowPack(WaterSurf), Gridiv) = SnowPackWater

   ! -- Initial fraction of snow on each surface --
   ModelOutputData(0, cMOD_SnowFrac(PavSurf), Gridiv) = SnowFracPaved
   ModelOutputData(0, cMOD_SnowFrac(BldgSurf), Gridiv) = SnowFracBldgs
   ModelOutputData(0, cMOD_SnowFrac(ConifSurf), Gridiv) = SnowFracEveTr
   ModelOutputData(0, cMOD_SnowFrac(DecidSurf), Gridiv) = SnowFracDecTr
   ModelOutputData(0, cMOD_SnowFrac(GrassSurf), Gridiv) = SnowFracGrass
   ModelOutputData(0, cMOD_SnowFrac(BSoilSurf), Gridiv) = SnowFracBSoil
   ModelOutputData(0, cMOD_SnowFrac(WaterSurf), Gridiv) = SnowFracWater

   IceFrac = 0.2 !Estimated fraction of ice. Should be improved in the future

   ! initialise surface temperatures for ESTM_ext
   tsfc_roof_grids(Gridiv, :) = Temp_C0
   tsfc_wall_grids(Gridiv, :) = Temp_C0
   tsfc_surf_grids(Gridiv, :) = Temp_C0

   ! At this point translate arrays to variables (needed for SUEWS_cal_RoughnessParameters)
   CALL SUEWS_Translate(Gridiv, 0, 0)

   !Calculation of roughness parameters (N.B. uses porosity)
   IF (Diagnose == 1) PRINT *, 'calling in initial state: SUEWS_cal_RoughnessParameters'
   CALL SUEWS_cal_RoughnessParameters( &
      RoughLenMomMethod, sfr_surf, & !input
      bldgH, EveTreeH, DecTreeH, &
      porosity_id, FAIBldg, FAIEveTree, FAIDecTree, &
      z0m_in, zdm_in, Z, &
      FAI, & !output
      Zh, z0m, zdm, ZZD)

   !=============================================================================
   ! If the run start day is at previous year, then calculate the number of days
   ! in that year.

   !First we need to know if the previous day given in initial conditions (id_prev) is
   !on previous year as this is needed in the initialization of DayofWeek matrix.
   !In this case switch is set to one for date calculations.
   IF (id_prev == 0) THEN !If id_prev = 0, means that the first modelled day is 1 Jan
      year_int = year_int - 1 !1) find the number of days on that year
      CALL LeapYearCalc(year_int, id_prev) !2) set switch to 1 so that the code knows to change back to current year (switch=0)
      switch = 1
   END IF

   CALL day2month(id_prev, mb, date, seas, year_int, lat) !Calculate date information (mb = month, date = day,...)
   CALL Day_of_Week(date, mb, year_int, wd) !Calculate weekday of the previous day (wd) (1=Sun, ..., 7=Sat)

   !After the day in previous year switch is changed back to zero:
   !    ie not previous day anymore
   !Also the size of DayofWeek is from 0:NdaysinYear meaning
   !that in zero slot is the previous day information
   IF (switch == 1) THEN
      year_int = year_int + 1
      id_prev = 0
      switch = 0
   END IF

   ! DayofWeek(id_prev,1)=wd   ! day of week
   ! DayofWeek(id_prev,2)=mb   ! month
   ! DayofWeek(id_prev,3)=seas ! season (summer=1, winter=2) needed for accumulation

   ! in case next day goes to next year calculate again the date information for DayofWeek matrix.
   id_next = id_prev + 1
   IF (id_next > nofDaysThisYear) THEN
      id_next = 1
      year_int = year_int + 1
      switch = 1
      CALL ErrorHint(43, 'switch- years', notUsed, notUsed, notUsedI)
   END IF

   CALL day2month(id_next, mb, date, seas, year_int, lat) !Calculate real date from doy
   CALL Day_of_Week(date, mb, year_int, wd) !Calculate weekday (1=Sun, ..., 7=Sat)

   IF (switch == 1) THEN
      iy = iy - 1
      switch = 0
   END IF

   ! DayofWeek(id_next,1)=wd  ! day of week
   ! DayofWeek(id_next,2)=mb  ! month
   ! DayofWeek(id_next,3)=seas ! season

   !=============================================================================

   !id=id_prev
   !it= 23 !!LastTimeOfDay
   IF (id_prev >= startDLS .AND. id_prev <= endDLS) THEN !Summertime
      DLS = 1
   ELSE
      DLS = 0
   END IF

   ! -----------------------------------------------------------------------
   ! Calculate daily water use if modelled (i.e. if WaterUseMethod = 0).
   ! Calculated from previous day information given in InitialConditions file
   CALL SUEWS_cal_weekday( &
      iy, id, lat, & !input
      dayofWeek_id) !output

   state_id = [PavedState, BldgsState, EveTrState, DecTrState, GrassState, BSoilState, WaterState]
   soilstore_id = [SoilStorePavedState, SoilStoreBldgsState, SoilStoreEveTrstate, SoilStoreDecTrState, &
                   SoilStoreGrassState, SoilStoreBSoilState, 0D0]
   CALL update_WaterUse( &
      id, WaterUseMethod, DayofWeek_id, lat, Faut, HDD_id, & !input
      state_id, soilstore_id, SoilStoreCap, H_maintain, & !input
      Ie_a, Ie_m, Ie_start, Ie_end, DayWatPer, DayWat, &
      WUDay_id) !output

   ! ---- AnOHM TS ---------------------
   ! initialize Bowen ratio
   ! Bo_grids(0,:)=2.
   ! mAH_grids(0,:)=25.

   ! -----------------------------------
   CALL SUEWS_TranslateBack(Gridiv, 0, 0)

   RETURN

600 CALL ErrorHint(47, TRIM(FileInit), notUsed, notUsed, notUsedI)
601 CALL ErrorHint(48, TRIM(FileInit), notUsed, notUsed, ios_out)

END SUBROUTINE InitialState

! ===========================================================================
FUNCTION NormalizeVegChar(VegCol, Gridiv) RESULT(NormVegResult)

   USE AllocateArray
   USE ColNamesInputFiles

   IMPLICIT NONE

   INTEGER, DIMENSION(nvegsurf) :: VegCol !Must be column numbers defined for veg surfaces only
   INTEGER :: Gridiv
   REAL(KIND(1D0)) :: NormVegResult
   IF (SurfaceChar(Gridiv, c_FrEveTr) + &
       SurfaceChar(Gridiv, c_FrDecTr) + &
       SurfaceChar(Gridiv, c_FrGrass) == 0.) THEN ! prevent arithmetic error under a full impervious scenario
      NormVegResult = 0.
   ELSE
      NormVegResult = (SurfaceChar(Gridiv, VegCol(ivConif))*SurfaceChar(Gridiv, c_FrEveTr) + &
                       SurfaceChar(Gridiv, VegCol(ivDecid))*SurfaceChar(Gridiv, c_FrDecTr) + &
                       SurfaceChar(Gridiv, VegCol(ivGrass))*SurfaceChar(Gridiv, c_FrGrass))/ &
                      (SurfaceChar(Gridiv, c_FrEveTr) + SurfaceChar(Gridiv, c_FrDecTr) + SurfaceChar(Gridiv, c_FrGrass))
   END IF

   RETURN
END FUNCTION NormalizeVegChar
! ===========================================================================

!-------------------------------------------------------------------------
SUBROUTINE NextInitial(GridName, year_int)
   ! Last modified HCW 24 May 2016
   ! Year of InitialConditions output file fixed. _EndofRun appended to files where the run finishes before the year end
   ! Last modified LJ 06 Jul 2015
   ! Initial conditions of SnowAlb added, densSnow changed to SnowDens
   ! Last modified HCW 03 Jul 2015
   ! Added initial conditions albEveTr0 and albGrass0
   ! Modified by HCW 21 Nov 2014
   ! Last day of year is not anymore the number of days on that year, but rather
   ! id == 1. Thus nofDaysThisYear was changed to 1. LJ 9/4/2015
   !------------------------------------------------------------------------

   USE allocateArray
   USE ColNamesInputFiles
   USE ColNamesModelDailyState
   USE data_in
   USE defaultNotUsed
   USE Initial
   USE sues_data
   USE snowMod
   USE time
   USE InitialCond

   IMPLICIT NONE

   CHARACTER(len=15) :: GridName
   CHARACTER(len=4) :: year_txt2
   INTEGER :: year_int2
   INTEGER :: year_int
   INTEGER :: ID_Prev_Out ! ID_Prev written to next Initial Conditions file
   INTEGER :: nofDaysThisYear_ForOutput !Added HCW 13 Jan 2017

   year = year_int !HCW added 21 Nov 2014

   ! Modified by HCW 24 May 2016
   IF (id == 1 .AND. iy == (year + 1)) THEN !if id = 1 and this is the first row of next year
      year_int2 = INT(year + 1)
      WRITE (year_txt2, '(I4)') year_int2
      OPEN (57, File=TRIM(FileInputPath)//TRIM("InitialConditions")//TRIM(GridName)//'_'//TRIM(ADJUSTL(year_txt2))//'.nml', err=200)
      nofDaysThisYear_ForOutput = nofdaysthisyear
   ELSE
      year_int2 = INT(year) !End of Run but not end of year
      WRITE (year_txt2, '(I4)') year_int2
      OPEN (57, File=TRIM(FileInputPath)//TRIM("InitialConditions")//TRIM(GridName)//'_'//TRIM(ADJUSTL(year_txt2))// &
            '_EndofRun.nml', err=201)
      nofDaysThisYear_ForOutput = id - 1
   END IF
   ID_Prev_Out = (id - 1)

   !! If last time of day, then DailyState variables will have been updated so can write out arrays for id rather than id-1
   !if(it==23 .and. imin == (nsh_real-1)/nsh_real*60) then  !!LastTimeofday
   !   id=id+1
   !endif
   WRITE (57, *) '&InitialConditions'
   WRITE (57, *) 'DaysSinceRain=', INT(HDD_id(12))
   WRITE (57, *) 'Temp_C0=', HDD_id(9)
   !WRITE(57,*)'ID_Prev=',ID_Prev_Out  !No longer included in initial conditions (HCW 13 Jan 2017)
   WRITE (57, *) 'GDD_1_0=', GDD_id(1)
   WRITE (57, *) 'GDD_2_0=', GDD_id(2)
   WRITE (57, *) 'LAIinitialEveTr=', LAI_id(ivConif)
   WRITE (57, *) 'LAIinitialDecTr=', LAI_id(ivDecid)
   WRITE (57, *) 'LAIinitialGrass=', LAI_id(ivGrass)
   WRITE (57, *) 'AlbEveTr0=', AlbEveTr_id
   WRITE (57, *) 'AlbDecTr0=', AlbDecTr_id
   WRITE (57, *) 'AlbGrass0=', AlbGrass_id
   WRITE (57, *) 'DecidCap0=', decidCap_id
   WRITE (57, *) 'Porosity0=', porosity_id
   WRITE (57, *) 'SoilStorePavedState=', soilstore_id(PavSurf)
   WRITE (57, *) 'SoilStoreBldgsState=', soilstore_id(BldgSurf)
   WRITE (57, *) 'SoilStoreEveTrState=', soilstore_id(ConifSurf)
   WRITE (57, *) 'SoilStoreDecTrState=', soilstore_id(DecidSurf)
   WRITE (57, *) 'SoilStoreGrassState=', soilstore_id(GrassSurf)
   WRITE (57, *) 'SoilStoreBSoilState=', soilstore_id(BSoilSurf)
   WRITE (57, *) 'PavedState=', state_id(PavSurf)
   WRITE (57, *) 'BldgsState=', state_id(BldgSurf)
   WRITE (57, *) 'EveTrState=', state_id(ConifSurf)
   WRITE (57, *) 'DecTrState=', state_id(DecidSurf)
   WRITE (57, *) 'GrassState=', state_id(GrassSurf)
   WRITE (57, *) 'BSoilState=', state_id(BSoilSurf)
   WRITE (57, *) 'WaterState=', state_id(WaterSurf)
   ! Only write snow variables if snow part is running
   IF (SnowUse == 1) THEN
      WRITE (57, *) 'SnowWaterPavedState=', SnowWater(PavSurf)
      WRITE (57, *) 'SnowWaterBldgsState=', SnowWater(BldgSurf)
      WRITE (57, *) 'SnowWaterEveTrState=', SnowWater(ConifSurf)
      WRITE (57, *) 'SnowWaterDecTrState=', SnowWater(DecidSurf)
      WRITE (57, *) 'SnowWaterGrassState=', SnowWater(GrassSurf)
      WRITE (57, *) 'SnowWaterBSoilState=', SnowWater(BSoilSurf)
      WRITE (57, *) 'SnowWaterWaterState=', SnowWater(WaterSurf)
      WRITE (57, *) 'SnowPackPaved=', SnowPack(PavSurf)
      WRITE (57, *) 'SnowPackBldgs=', SnowPack(BldgSurf)
      WRITE (57, *) 'SnowPackEveTr=', SnowPack(ConifSurf)
      WRITE (57, *) 'SnowPackDecTr=', SnowPack(DecidSurf)
      WRITE (57, *) 'SnowPackGrass=', SnowPack(GrassSurf)
      WRITE (57, *) 'SnowPackBSoil=', SnowPack(BSoilSurf)
      WRITE (57, *) 'SnowPackWater=', SnowPack(WaterSurf)
      WRITE (57, *) 'SnowFracPaved=', SnowFrac(PavSurf)
      WRITE (57, *) 'SnowFracBldgs=', SnowFrac(BldgSurf)
      WRITE (57, *) 'SnowFracEveTr=', SnowFrac(ConifSurf)
      WRITE (57, *) 'SnowFracDecTr=', SnowFrac(DecidSurf)
      WRITE (57, *) 'SnowFracGrass=', SnowFrac(GrassSurf)
      WRITE (57, *) 'SnowFracBSoil=', SnowFrac(BSoilSurf)
      WRITE (57, *) 'SnowFracWater=', SnowFrac(WaterSurf)
      WRITE (57, *) 'SnowDensPaved=', SnowDens(PavSurf)
      WRITE (57, *) 'SnowDensBldgs=', SnowDens(BldgSurf)
      WRITE (57, *) 'SnowDensEveTr=', SnowDens(ConifSurf)
      WRITE (57, *) 'SnowDensDecTr=', SnowDens(DecidSurf)
      WRITE (57, *) 'SnowDensGrass=', SnowDens(GrassSurf)
      WRITE (57, *) 'SnowDensBSoil=', SnowDens(BSoilSurf)
      WRITE (57, *) 'SnowDensWater=', SnowDens(WaterSurf)
      WRITE (57, *) 'SnowAlb0=', SnowAlb
   END IF
   ! WRITE(57,*)'BoInit=',BoInit
   WRITE (57, *) '/'
   CLOSE (57)

   IF (it == 23 .AND. imin == (nsh_real - 1)/nsh_real*60) THEN
      id = id - 1
   END IF

   RETURN

200 CALL ErrorHint(49, TRIM("InitialConditions")//TRIM(GridName)// &
                  '_'//TRIM(ADJUSTL(year_txt2))//'.nml', notUsed, notUsed, notUsedI)
201 CALL ErrorHint(49, TRIM("InitialConditions")//TRIM(GridName)// &
                  '_'//TRIM(ADJUSTL(year_txt2))//'EoR.nml', notUsed, notUsed, notUsedI)

END SUBROUTINE NextInitial
!-------------------------------------------------------------------------

!=======================================================================
!=======================================================================
!This subroutine prepares a meteorological forcing file.

SUBROUTINE SUEWS_InitializeMetData(lunit)

   USE allocateArray
   USE data_in
   USE sues_data
   USE time
   USE defaultnotUsed
   USE Initial

   IMPLICIT NONE

   INTEGER :: lunit, i, iyy !,RunNumber,NSHcounter
   REAL(KIND(1D0)), DIMENSION(24) :: MetArray
   REAL(KIND(1D0)) :: imin_prev, ih_prev, iday_prev, tstep_met, iy_only !For checks on temporal resolution of met data

   ! initialisation
   iy_only = 1
   ih_prev = 1
   imin_prev = 1
   iday_prev = 1

   !---------------------------------------------------------------

   !Open the file for reading and read the actual data
   !write(*,*) fileMet
   OPEN (lunit, file=TRIM(fileMet), status='old', err=314)
   CALL skipHeader(lunit, SkipHeaderMet)

   ! Skip to the right place in the met file, depending on how many chunks have been read already
   IF (skippedLines > 0) THEN
      DO iyy = 1, skippedLines
         READ (lunit, *)
      END DO
   END IF

   ! Read in next chunk of met data and fill MetForcingData array with data for every timestep
   !NSHcounter = 1
   !write(*,*) 'ReadlinesMetdata:',ReadlinesMetdata
   DO i = 1, ReadlinesMetdata
      CALL MetRead(lunit, MetArray, InputmetFormat, ldown_option, NetRadiationMethod, &
                   SnowUse, SMDMethod, SoilDepthMeas, SoilRocks, SoilDensity, SmCap)
      !DO iv=1,NSH
      !    MetForcingData(NSHcounter,1:24,GridCounter) = MetArray
      !   NSHcounter = NSHcounter + 1
      !ENDDO
      MetForcingData(i, 1:24, GridCounter) = MetArray
      ! Check timestamp of met data file matches TSTEP specified in RunControl
      IF (i == 1) THEN
         imin_prev = MetArray(4)
         ih_prev = MetArray(3)
         iday_prev = MetArray(2)
         iy_only = MetArray(1)
      ELSEIF (i == 2) THEN
         tstep_met = ((MetArray(4) + 60*MetArray(3)) - (imin_prev + 60*ih_prev))*60 !tstep in seconds
         IF (tstep_met /= tstep_real .AND. MetArray(2) == iday_prev) THEN
            CALL ErrorHint(39, 'TSTEP in RunControl does not match TSTEP of met data (DOY).', REAL(tstep, KIND(1D0)), tstep_met, &
                           INT(MetArray(2)))
         END IF
      END IF

      ! Check file only contains a single year --------------------------------------------
      ! Very last data point is allowed to be (should be) timestamped with following year
      IF (MetArray(1) /= iy_only) THEN
         IF (MetArray(1) == iy_only + 1 .AND. MetArray(2) == 1 .AND. MetArray(3) == 0 .AND. MetArray(4) == 0) THEN
            !write(*,*) 'end of year - no problem'
         ELSE
            CALL errorHint(3, 'Problem in SUEWS_Initial: multiple years found in met forcing file.', &
                           MetArray(1), NotUsed, NotUsedI)
         END IF
      END IF

   END DO

   CLOSE (lunit)

   RETURN

314 CALL errorHint(11, TRIM(fileMet), notUsed, notUsed, ios_out)

END SUBROUTINE SUEWS_InitializeMetData
!----------------------------------------------------------------------------------------------

!====================================================================================
SUBROUTINE CheckInitial
   !Check the parameters in InitialConditions file.
   !Modified by HCW 04 Mar 2014, changed soilstore_id(is) checks to use names given in InitialConditions
   !Added by LJ in 8/2/2013

   USE allocateArray
   USE data_in
   USE defaultNotUsed
   USE InitialCond
   USE snowMod
   USE time

   IMPLICIT NONE

   !real(kind(1d0)):: pTol   !Precision tolerance for range checks

   IF (Temp_C0 < (Temp_C - 10) .OR. Temp_C0 > (Temp_C + 10)) THEN
      CALL ErrorHint(37, 'Temp_C0 very different to Tair.', Temp_C0, Temp_C, notUsedI)
   END IF

   !Check more thoroughly if LAI values are OK. Need to treat different hemispheres as well as tropics separately.
   IF (lat > 40) THEN
      IF ((LAIinitialEveTr > LAImin(ConifSurf - 2) + 1 .AND. (id < 60 .OR. id > 330)) .OR. &
          (LAIinitialEveTr < LAImax(ConifSurf - 2) - 1 .AND. (id > 130 .AND. id < 244))) THEN
         CALL ErrorHint(37, 'Check LAIinitialEveTr in InitialConditions file', LAIinitialEveTr, LAImin(ConifSurf - 2), notUsedI)
      END IF
      IF ((LAIinitialDecTr > LAImin(DecidSurf - 2) + 1 .AND. (id < 60 .OR. id > 330)) .OR. &
          (LAIinitialDecTr < LAImax(DecidSurf - 2) - 1 .AND. (id > 130 .AND. id < 244))) THEN
         CALL ErrorHint(37, 'Check LAIinitialDecTr in InitialConditions file', LAIinitialDecTr, LAImin(DecidSurf - 2), notUsedI)
      END IF
      IF ((LAIinitialGrass > LAImin(GrassSurf - 2) + 1 .AND. (id < 60 .OR. id > 330)) .OR. &
          (LAIinitialGrass < LAImax(GrassSurf - 2) - 1 .AND. (id > 130 .AND. id < 244))) THEN
         CALL ErrorHint(37, 'Check LAIinitialGrass in InitialConditions file', LAIinitialGrass, LAImin(GrassSurf - 2), notUsedI)
      END IF

   ELSEIF (lat < -40) THEN
      IF ((LAIinitialEveTr < LAImax(ConifSurf - 2) - 1 .AND. (id < 60 .OR. id > 330)) .OR. &
          (LAIinitialEveTr > LAImin(ConifSurf - 2) + 1 .AND. (id > 130 .AND. id < 244))) THEN
         CALL ErrorHint(37, 'Check LAIinitialEveTr in InitialConditions file', LAIinitialEveTr, LAImax(ConifSurf - 2), notUsedI)
      END IF
      IF ((LAIinitialDecTr > LAImax(DecidSurf - 2) - 1 .AND. (id < 60 .OR. id > 330)) .OR. &
          (LAIinitialDecTr > LAImin(DecidSurf - 2) + 1 .AND. (id > 130 .AND. id < 244))) THEN
         CALL ErrorHint(37, 'Check LAIinitialDecTr in InitialConditions file', LAIinitialDecTr, LAImax(DecidSurf - 2), notUsedI)
      END IF
      IF ((LAIinitialGrass < LAImax(GrassSurf - 2) - 1 .AND. (id < 60 .OR. id > 330)) .OR. &
          (LAIinitialGrass > LAImin(GrassSurf - 2) + 1 .AND. (id > 130 .AND. id < 244))) THEN
         CALL ErrorHint(37, 'Check LAIinitialGrass in InitialConditions file', LAIinitialGrass, LAImax(GrassSurf - 2), notUsedI)
      END IF

   ELSEIF (lat < 10 .AND. lat > -10) THEN

      IF (LAIinitialEveTr < LAImax(ConifSurf - 2) - 0.5) THEN
         CALL ErrorHint(37, 'Check LAIinitialEveTr in InitialConditions file', LAIinitialEveTr, LAImax(ConifSurf - 2), notUsedI)
      END IF
      IF (LAIinitialDecTr < LAImax(DecidSurf - 2) - 0.5) THEN
         CALL ErrorHint(37, 'Check LAIinitialDecTr in InitialConditions file', LAIinitialDecTr, LAImax(DecidSurf - 2), notUsedI)
      END IF
      IF (LAIinitialGrass < LAImax(GrassSurf - 2) - 0.5) THEN
         CALL ErrorHint(37, 'Check LAIinitialGrass in InitialConditions file', LAIinitialGrass, LAImax(GrassSurf - 2), notUsedI)
      END IF

   END IF

   !Soilstore check
   IF (SoilstoreBldgsState > SoilStoreCap(BldgSurf)) THEN
      CALL ErrorHint(37, 'InitialCond: Check initial condition of building soil store.', &
                     SoilstoreBldgsState, SoilStoreCap(BldgSurf), notUsedI)
   END IF
   IF (SoilstorePavedState > SoilStoreCap(PavSurf)) THEN
      CALL ErrorHint(37, 'InitialCond: Check initial condition of paved soil store.', &
                     SoilstorePavedState, SoilStoreCap(PavSurf), notUsedI)
   END IF
   IF (SoilstoreEveTrState > SoilStoreCap(ConifSurf)) THEN
      CALL ErrorHint(37, 'InitialCond: Check initial condition of conif soil store.', &
                     SoilstoreEveTrState, SoilStoreCap(ConifSurf), notUsedI)
   END IF
   IF (SoilstoreDecTrState > SoilStoreCap(DecidSurf)) THEN
      CALL ErrorHint(37, 'InitialCond: Check initial condition of deciduous soil store.', &
                     SoilstoreDecTrState, SoilStoreCap(DecidSurf), notUsedI)
   END IF
   IF (SoilstoreBSoilState > SoilStoreCap(BSoilSurf)) THEN
      CALL ErrorHint(37, 'InitialCond: Check initial condition of bare soil soil store.', &
                     SoilstoreBSoilState, SoilStoreCap(BSoilSurf), notUsedI)
   END IF
   IF (SoilstoreGrassState > SoilStoreCap(GrassSurf)) THEN
      CALL ErrorHint(37, 'InitialCond: Check initial condition of grass soil store.', &
                     SoilstoreGrassState, SoilStoreCap(GrassSurf), notUsedI)
   END IF

   !Snow stuff
   IF (SnowUse == 1) THEN
      IF (SnowWaterBldgsState > CRWmax*SnowPackBldgs) THEN
         CALL ErrorHint(37, 'InitialCond: SnowWaterBldgsState', SnowWaterBldgsState, SnowPackBldgs, notUsedI)
      END IF
      IF (SnowWaterPavedState > CRWmax*SnowPackPaved) THEN
         CALL ErrorHint(37, 'InitialCond: SnowWaterPavedState', SnowWaterPavedState, SnowPackPaved, notUsedI)
      END IF
      IF (SnowWaterEveTrState > CRWmax*SnowPackEveTr) THEN
         CALL ErrorHint(37, 'InitialCond: SnowWaterEveTrstate', SnowWaterEveTrstate, SnowPackEveTr, notUsedI)
      END IF
      IF (SnowWaterDecTrState > CRWmax*SnowPackDecTr) THEN
         CALL ErrorHint(37, 'InitialCond: SnowWaterDecTrState', SnowWaterDecTrState, SnowPackDecTr, notUsedI)
      END IF
      IF (SnowWaterGrassState > CRWmax*SnowPackGrass) THEN
         CALL ErrorHint(37, 'InitialCond: SnowWaterGrassState', SnowWaterGrassState, SnowPackGrass, notUsedI)
      END IF
      IF (SnowWaterBSoilState > CRWmax*SnowPackBSoil) THEN
         CALL ErrorHint(37, 'InitialCond: SnowWaterGrassUnirState', SnowWaterBSoilState, SnowPackBSoil, notUsedI)
      END IF
   END IF

END SUBROUTINE CheckInitial
