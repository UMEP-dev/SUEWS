!Reading one line of meteorological forcing data in.
!Latest change:
!  Feb 2012, LJ:  Input fluxes qh and qe changed _obs as well as qn1_obs ending
!  Oct 2014, LJ:  Variables changed only be used in this part of code and these are passed to calling
!                 function in MetArray.
!  Jan 2015, HCW: Precip_hr, wuh and LAI_hr changed for generic timesteps
!  Jan 2016, LJ:  Removal of tabs
!  Feb 2017, HCW: Added file unit as argument so MetRead can be used for original met forcing file too
!  May 2018, TS: imporved the ability in downscaling datetime by introducing a datetime library
! To Do:
!       - Check observed SM calculation
!---------------------------------------------------------------------------------------------------
SUBROUTINE MetRead(lfn, MetArray, InputmetFormat, ldown_option, NetRadiationMethod, &
                   SnowUse, SMDMethod, SoilDepthMeas, SoilRocks, SoilDensity, SmCap)

   USE module_ctrl_const_default

   IMPLICIT NONE

   !INPUT
   REAL(KIND(1D0)), DIMENSION(24) :: MetArray !Array leaving the subroutine within
   !each INTERVAL (defined in RunControl.nml)
   ! - Met data now provided at a resolution of tstep, HCW Jan 2015

   REAL(KIND(1D0)) :: SmCap, & !Volumetric/gravimetric soil moisture capacity
                      SoilDepthMeas, & !Measured soil depth
                      SoilRocks, & !Rocks on ground
                      SoilDensity !Density of soil

   INTEGER :: InputmetFormat, & !Format of the meteorological forcing file
              ldown_option, & !Method of calculating Ldown
              NetRadiationMethod, & !Method of calculating Q*
              SMDMethod, & !Method of measured soil moisture
              SnowUse

   ! Variables read in
   REAL(KIND(1D0)) :: avkdn, & !Average downwelling shortwave radiation
                      avrh, & !Average relative humidity
                      avu1, & !Average wind speed
                      dectime, & !Decimal time
                      fcld_obs, & !Cloud fraction observed
                      iy, & !Year
                      id, & !Day
                      it, & !Hour
                      imin, & !Minute
                      kdiff, & !Diffuse shortwave radiation
                      kdir, & !Direct shortwave radiation
                      LAI_obs, & !Overall LAI of the study area
                      ldown_obs, & !Downwelling longwave radiation
                      Precip, & !Rainfall [mm]
                      Pres_hPa, & !Station air pressure in hPa
                      Pres_kPa, & !Station air pressure in kPa
                      snowFrac_obs, & !Observed surface fraction of snow (between 0 and 1)
                      qe_obs, & !Observed latent heat flux
                      qf_obs, & !Observed antrhropogeni heat flux
                      qh_obs, & !Observed sensible heat flux
                      qn1_obs, & !Observed net all-wave radiation
                      qs_obs, & !Observed storage heat flux
                      Temp_C, & !Air temperature
                      wdir, & !Wind direction
                      wu_m3, & !Water use provided in met forcing file [m3]
                      xsmd !Measured soil moisture deficit

   INTEGER :: iostat_var, lfn

   !-----------------------------------------------------------------------------------
   !-----------------------------------------------------------------------------------

   IF (InputMetFormat == 0) THEN !Default format using LUMPS only

      READ (lfn, *, iostat=iostat_var) iy, id, it, imin, qn1_obs, avu1, avrh, &
         Temp_C, wdir, Pres_kPa, Precip, avkdn, snowFrac_obs, ldown_obs, fcld_obs

      !Set other variables needed while running SUEWS to zero
      qf_obs = NaN
      qs_obs = NaN
      qh_obs = NaN
      qe_obs = NaN
      xsmd = -99999
      kdiff = NaN
      kdir = NaN
      wdir = NaN

   ELSEIF (InputMetFormat == 10) THEN !SUEWS reading
      READ (lfn, *, iostat=iostat_var) iy, id, it, imin, qn1_obs, qh_obs, qe_obs, qs_obs, qf_obs, avu1, avrh, &
         Temp_C, Pres_kPa, Precip, avkdn, snowFrac_obs, ldown_obs, fcld_obs, &
         wu_m3, xsmd, LAI_obs, kdiff, kdir, wdir

      !write(*,*) 'In LUMPS_MetRead (1)'
      !write(*,*) 'imin',imin
      !write(*,*) 'it',it
      !write(*,*) 'id',id
      !write(*,*) 'iy',iy

      !Calculate observed soil moisture deficits from either volumetric or gravimetric SoilStates
      IF (SMDMethod == 1 .AND. xsmd /= -999) THEN !Soil moisture - volumetric
         xsmd = (SmCap - xsmd)*SoilDepthMeas*SoilRocks
      ELSEIF (SMDMethod == 2 .AND. xsmd /= -999) THEN !Soil moisture -gravimetric
         xsmd = (SmCap - xsmd)*SoilDensity*SoilDepthMeas*SoilRocks
      ELSE
         xsmd = -999
      END IF

   ELSE
      CALL ErrorHint(55, 'RunControl.nml, InputMetFormat not usable.', notUsed, notUsed, InputmetFormat)
   END IF

   !===============Meteorological variables reading done==========================
   Pres_hPa = Pres_kPa*10. ! convert to hPa

   ! get decimal time
   dectime = id + it + imin/60.0

   IF (iostat_var < 0) THEN
      iostat_var = 0
      CLOSE (lfn)
      RETURN
   END IF

   IF (AvKdn < 0) THEN
      CALL ErrorHint(27, 'Met Data: avKdn - needed for StoreDrainPrm. resistance, If present, check file not tab delimited', &
                     avkdn, dectime, notUsedI)
      !sg removed this is causing the problems with resistances
      !  AvKdn=0 !Solar radiation cannot be lower than 1
   END IF

   IF ((ldown_option == 1) .AND. (ldown_obs < 0)) THEN
      CALL ErrorHint(27, 'Met Data: LWdn (ldown_obs) - impact Q* calc', ldown_obs, dectime, notUsedI)

   ELSEIF (ldown_option == 2) THEN
      IF (fcld_obs == -999.0 .OR. fcld_obs < 0 .OR. fcld_obs > 1) THEN
         CALL ErrorHint(27, 'Met Data: flcd_obs - impacts LW & Q* radiation', fcld_obs, dectime, notUsedI)
      END IF
   END IF

   IF (qn1_obs == -999 .AND. NetRadiationMethod == 0) THEN !If measured Q* is used and it is -999
      CALL ErrorHint(27, 'Met Data: Q* - will impact everything', qn1_obs, dectime, notUsedI)
   END IF

   IF (avu1 <= 0) THEN !If wind speed is negative
      CALL ErrorHint(27, 'Met Data: avU1 - impacts aeroydnamic resistances', avU1, dectime, notUsedI)
   END IF

   IF (Temp_C < -50 .OR. Temp_C > 60) THEN !If temperature unrealistic
      CALL ErrorHint(27, 'Met Data: Temp_C - beyond what is expected', Temp_C, dectime, notUsedI)
   END IF

   IF (avrh > 100 .OR. avrh < 1) THEN !If relative humidity larger than 100%
      CALL ErrorHint(27, 'Met Data: avRH - beyond what is expected', avRH, dectime, notUsedI)
   END IF

   IF (Pres_kPa < 80) THEN !If pressure too low
      CALL ErrorHint(27, 'Met Data: Pres_kPa - too low - this could be fixed in model', Pres_kPa, dectime, notUsedI)
   END IF

   IF (Precip < 0) THEN !If rain in negative, set it to zero
      CALL ErrorHint(27, 'Met Data: Precip - less than 0', Precip, dectime, notUsedI)
   END IF

   IF (snowFrac_obs == NAN) snowFrac_obs = 0

   IF (SnowUse == 0 .AND. (snowFrac_obs < 0 .OR. snowFrac_obs > 1)) THEN
      CALL ErrorHint(27, 'Met Data: snow not between [0  1]', snowFrac_obs, dectime, notUsedI)
   END IF

   IF (xsmd < 0 .AND. SMDMethod == 1) THEN !If soil moisture deficit is zero
      CALL ErrorHint(27, 'Met Data: xsmd - less than 0', xsmd, dectime, notUsedI)
   END IF

   !Create an array to be printed out.
   MetArray(1:24) = (/iy, id, it, imin, qn1_obs, qh_obs, qe_obs, qs_obs, qf_obs, avu1, &
                      avrh, Temp_C, Pres_hPa, Precip, avkdn, snowFrac_obs, ldown_obs, &
                      fcld_obs, wu_m3, xsmd, LAI_obs, kdiff, kdir, wdir/)

   !write(*,*) 'In LUMPS_MetRead (2)'
   !write(*,*) 'imin',imin
   !write(*,*) 'it',it
   !write(*,*) 'id',id
   !write(*,*) 'iy',iy

   RETURN

END SUBROUTINE MetRead

! sg Feb 2012 -- moving all related suborutine together
! subroutine run_control - checks that value is reasonable
! subrotuine skipHeader  - jumps over header rows in input files
! Last modified:
! LJ 27 Jan 2016 - Removal of tabs, cleaning of the code
!------------------------------------------------------------

!Information for the run
MODULE run_info
   IMPLICIT NONE
   CHARACTER(len=90), DIMENSION(14) :: text
   INTEGER :: lim0 = 0, lim1 = 1, lim2 = 2, lim4 = 4, lim3 = 3, lim6 = 6, lim8 = 8, lim12 = 12, lfn_us
   LOGICAL :: file_qs
END MODULE run_info

! run_control
! called from: LUMPS_initial

SUBROUTINE run_control(eval, LowerLimit, Upperlimit)
   ! ver - determines if value to be read is an integer or real and returns the value
   ! if ver=-9 - then use integer
   USE run_info
   IMPLICIT NONE
   INTEGER :: eval, i, lowerlimit, upperlimit
   CHARACTER(len=4) :: check

   IF (file_qs) THEN
101   READ (lfn_us, *) check
      WRITE (*, *) check
      DO i = 1, 3
         IF (check(i:i) == "#") THEN
            !   write(*,*)check(i:i),i,"check"
            GOTO 101
         ELSE
            BACKSPACE (lfn_us)
            READ (lfn_us, *) eval
            !write(*,*)eval, TEXT(1)
            EXIT
         END IF
      END DO
   END IF

   WRITE (12, 120) eval, text(1)

   IF (eval < Lowerlimit .OR. eval > upperlimit) THEN
      WRITE (*, *) "Value out of range"
      WRITE (*, *) eval, text(1)
      STOP
   END IF

   WRITE (*, 120) eval, text(1)
120 FORMAT(i4, 2X, a90)

   RETURN
END SUBROUTINE run_control

! Skip Headers------------------------------
SUBROUTINE SkipHeader(lfn, skip)
   USE defaultnotUsed
   IMPLICIT NONE

   INTEGER :: skip, lfn, i
   DO I = 1, skip
      READ (lfn, *, err=201, iostat=ios_out)
   END DO

   RETURN

201 reall = REAL(skip)
   CALL ErrorHint(20, 'In SkipHeader subroutine.', reall, notUsed, ios_out)
END SUBROUTINE SkipHeader

!-------------------------------------------------------------------------
SUBROUTINE InputHeaderCheck(FileName)
   ! Checks columns in input files match the columns expected by model code
   ! Model code columns are defined here
   ! Latest update:
   !   MH 21 Jun 2017 - Added parameters to SUEWS_AnthropogenicEmissions.txt
   !   MH 16 Jun 2017 - Added SUEWS_BiogenCO2.txt
   !   TS 02 Mar 2016  - AnOHM related variables added
   !   LJ 27 Jan 2016  - Removal of tabs
   !   LJ 07 July 2015 - snow albedo removed
   !   HCW 12 Nov 2014
   !-------------------------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   CHARACTER(len=50) :: FileName

   ! ========== Define expected column names here ==========
   ! =======================================================

   ! ========== SUEWS_NonVeg.txt =============
   HeaderNonVeg_Reqd(ci_Code) = "Code"
   HeaderNonVeg_Reqd(ci_AlbMin) = "AlbedoMin"
   HeaderNonVeg_Reqd(ci_AlbMax) = "AlbedoMax"
   HeaderNonVeg_Reqd(ci_Emis) = "Emissivity"
   HeaderNonVeg_Reqd(ci_StorMin) = "StorageMin"
   HeaderNonVeg_Reqd(ci_StorMax) = "StorageMax"
   HeaderNonVeg_Reqd(ci_WetThresh) = "WetThreshold"
   HeaderNonVeg_Reqd(ci_StateLimit) = "StateLimit"
   HeaderNonVeg_Reqd(ci_DrEq) = "DrainageEq"
   HeaderNonVeg_Reqd(ci_DrCoef1) = "DrainageCoef1"
   HeaderNonVeg_Reqd(ci_DrCoef2) = "DrainageCoef2"
   HeaderNonVeg_Reqd(ci_SoilTCode) = "SoilTypeCode"
   HeaderNonVeg_Reqd(ci_SnowLimPat) = "SnowLimPatch"
   HeaderNonVeg_Reqd(ci_SnowLimRem) = "SnowLimRemove"
   HeaderNonVeg_Reqd(ci_OHMCode_SWet) = "OHMCode_SummerWet"
   HeaderNonVeg_Reqd(ci_OHMCode_SDry) = "OHMCode_SummerDry"
   HeaderNonVeg_Reqd(ci_OHMCode_WWet) = "OHMCode_WinterWet"
   HeaderNonVeg_Reqd(ci_OHMCode_WDry) = "OHMCode_WinterDry"
   HeaderNonVeg_Reqd(ci_OHMThresh_SW) = "OHMThresh_SW"
   HeaderNonVeg_Reqd(ci_OHMThresh_WD) = "OHMThresh_WD"
   HeaderNonVeg_Reqd(ci_ESTMCode) = "ESTMCode"
   HeaderNonVeg_Reqd(ci_cpAnOHM) = "AnOHM_Cp" ! AnOHM TS
   HeaderNonVeg_Reqd(ci_kkAnOHM) = "AnOHM_Kk" ! AnOHM TS
   HeaderNonVeg_Reqd(ci_chAnOHM) = "AnOHM_Ch" ! AnOHM TS

   ! ========== SUEWS_Veg.txt ===============
   HeaderVeg_Reqd(cp_Code) = "Code"
   HeaderVeg_Reqd(cp_AlbMin) = "AlbedoMin"
   HeaderVeg_Reqd(cp_AlbMax) = "AlbedoMax"
   HeaderVeg_Reqd(cp_Emis) = "Emissivity"
   HeaderVeg_Reqd(cp_StorMin) = "StorageMin"
   HeaderVeg_Reqd(cp_StorMax) = "StorageMax"
   HeaderVeg_Reqd(cp_WetThresh) = "WetThreshold"
   HeaderVeg_Reqd(cp_StateLimit) = "StateLimit"
   HeaderVeg_Reqd(cp_DrEq) = "DrainageEq"
   HeaderVeg_Reqd(cp_DrCoef1) = "DrainageCoef1"
   HeaderVeg_Reqd(cp_DrCoef2) = "DrainageCoef2"
   HeaderVeg_Reqd(cp_SoilTCode) = "SoilTypeCode"
   HeaderVeg_Reqd(cp_SnowLimPat) = "SnowLimPatch"
   HeaderVeg_Reqd(cp_BaseT) = "BaseT"
   HeaderVeg_Reqd(cp_BaseTe) = "BaseTe"
   HeaderVeg_Reqd(cp_GDDFull) = "GDDFull"
   HeaderVeg_Reqd(cp_SDDFull) = "SDDFull"
   HeaderVeg_Reqd(cp_LAIMin) = "LAIMin"
   HeaderVeg_Reqd(cp_LAIMax) = "LAIMax"
   HeaderVeg_Reqd(cp_PorosityMin) = "PorosityMin"
   HeaderVeg_Reqd(cp_PorosityMax) = "PorosityMax"
   HeaderVeg_Reqd(cp_GsMax) = "MaxConductance"
   HeaderVeg_Reqd(cp_LAIEq) = "LAIEq"
   HeaderVeg_Reqd(cp_LeafGP1) = "LeafGrowthPower1"
   HeaderVeg_Reqd(cp_LeafGP2) = "LeafGrowthPower2"
   HeaderVeg_Reqd(cp_LeafOP1) = "LeafOffPower1"
   HeaderVeg_Reqd(cp_LeafOP2) = "LeafOffPower2"
   HeaderVeg_Reqd(cp_OHMCode_SWet) = "OHMCode_SummerWet"
   HeaderVeg_Reqd(cp_OHMCode_SDry) = "OHMCode_SummerDry"
   HeaderVeg_Reqd(cp_OHMCode_WWet) = "OHMCode_WinterWet"
   HeaderVeg_Reqd(cp_OHMCode_WDry) = "OHMCode_WinterDry"
   HeaderVeg_Reqd(cp_OHMThresh_SW) = "OHMThresh_SW"
   HeaderVeg_Reqd(cp_OHMThresh_WD) = "OHMThresh_WD"
   HeaderVeg_Reqd(cp_ESTMCode) = "ESTMCode"
   HeaderVeg_Reqd(cp_cpAnOHM) = "AnOHM_Cp" ! AnOHM TS
   HeaderVeg_Reqd(cp_kkAnOHM) = "AnOHM_Kk" ! AnOHM TS
   HeaderVeg_Reqd(cp_chAnOHM) = "AnOHM_Ch" ! AnOHM TS
   HeaderVeg_Reqd(cp_BiogenCO2Code) = "BiogenCO2Code"

   ! ========== SUEWS_Water.txt ==================
   HeaderWater_Reqd(cw_Code) = "Code"
   HeaderWater_Reqd(cw_AlbMin) = "AlbedoMin"
   HeaderWater_Reqd(cw_AlbMax) = "AlbedoMax"
   HeaderWater_Reqd(cw_Emis) = "Emissivity"
   HeaderWater_Reqd(cw_StorMin) = "StorageMin"
   HeaderWater_Reqd(cw_StorMax) = "StorageMax"
   HeaderWater_Reqd(cw_WetThresh) = "WetThreshold"
   HeaderWater_Reqd(cw_StateLimit) = "StateLimit"
   HeaderWater_Reqd(cw_WaterDepth) = "WaterDepth"
   HeaderWater_Reqd(cw_DrEq) = "DrainageEq"
   HeaderWater_Reqd(cw_DrCoef1) = "DrainageCoef1"
   HeaderWater_Reqd(cw_DrCoef2) = "DrainageCoef2"
   HeaderWater_Reqd(cw_OHMCode_SWet) = "OHMCode_SummerWet"
   HeaderWater_Reqd(cw_OHMCode_SDry) = "OHMCode_SummerDry"
   HeaderWater_Reqd(cw_OHMCode_WWet) = "OHMCode_WinterWet"
   HeaderWater_Reqd(cw_OHMCode_WDry) = "OHMCode_WinterDry"
   HeaderWater_Reqd(cw_OHMThresh_SW) = "OHMThresh_SW"
   HeaderWater_Reqd(cw_OHMThresh_WD) = "OHMThresh_WD"
   HeaderWater_Reqd(cw_ESTMCode) = "ESTMCode"
   HeaderWater_Reqd(cw_cpAnOHM) = "AnOHM_Cp" ! AnOHM TS
   HeaderWater_Reqd(cw_kkAnOHM) = "AnOHM_Kk" ! AnOHM TS
   HeaderWater_Reqd(cw_chAnOHM) = "AnOHM_Ch" ! AnOHM TS

   ! ========== SUEWS_Snow.txt ===================
   HeaderSnow_Reqd(cs_Code) = "Code"
   HeaderSnow_Reqd(cs_SnowRMFactor) = "RadMeltFactor"
   HeaderSnow_Reqd(cs_SnowTMFactor) = "TempMeltFactor"
   HeaderSnow_Reqd(cs_SnowAlbMin) = "AlbedoMin"
   HeaderSnow_Reqd(cs_SnowAlbMax) = "AlbedoMax"
   HeaderSnow_Reqd(cs_SnowEmis) = "Emissivity"
   HeaderSnow_Reqd(cs_Snowtau_a) = "tau_a"
   HeaderSnow_Reqd(cs_Snowtau_f) = "tau_f"
   HeaderSnow_Reqd(cs_SnowPLimAlb) = "PrecipLimAlb"
   HeaderSnow_Reqd(cs_SnowSDMin) = "SnowDensMin"
   HeaderSnow_Reqd(cs_SnowSDMax) = "SnowDensMax"
   HeaderSnow_Reqd(cs_Snowtau_r) = "tau_r"
   HeaderSnow_Reqd(cs_SnowCRWMin) = "CRWMin"
   HeaderSnow_Reqd(cs_SnowCRWMax) = "CRWMax"
   HeaderSnow_Reqd(cs_SnowPLimSnow) = "PrecipLimSnow"
   HeaderSnow_Reqd(cs_OHMCode_SWet) = "OHMCode_SummerWet"
   HeaderSnow_Reqd(cs_OHMCode_SDry) = "OHMCode_SummerDry"
   HeaderSnow_Reqd(cs_OHMCode_WWet) = "OHMCode_WinterWet"
   HeaderSnow_Reqd(cs_OHMCode_WDry) = "OHMCode_WinterDry"
   HeaderSnow_Reqd(cs_OHMThresh_SW) = "OHMThresh_SW"
   HeaderSnow_Reqd(cs_OHMThresh_WD) = "OHMThresh_WD"
   HeaderSnow_Reqd(cs_ESTMCode) = "ESTMCode"
   HeaderSnow_Reqd(cs_cpAnOHM) = "AnOHM_Cp" ! AnOHM TS
   HeaderSnow_Reqd(cs_kkAnOHM) = "AnOHM_Kk" ! AnOHM TS
   HeaderSnow_Reqd(cs_chAnOHM) = "AnOHM_Ch" ! AnOHM TS

   ! ========== SUEWS_Soil.txt ===================
   HeaderSoil_Reqd(cSo_Code) = "Code"
   HeaderSoil_Reqd(cSo_SoilDepth) = "SoilDepth"
   HeaderSoil_Reqd(cSo_SoilStCap) = "SoilStoreCap"
   HeaderSoil_Reqd(cSo_KSat) = "SatHydraulicCond"
   HeaderSoil_Reqd(cSo_SoilDens) = "SoilDensity"
   HeaderSoil_Reqd(cSo_SoilInfRate) = "InfiltrationRate"
   HeaderSoil_Reqd(cSo_ObsSMDepth) = "OBS_SMDepth"
   HeaderSoil_Reqd(cSo_ObsSMMax) = "OBS_SMCap"
   HeaderSoil_Reqd(cSo_ObsSNRFrac) = "OBS_SoilNotRocks"

   ! ========== SUEWS_Conductance.txt ============
   HeaderCond_Reqd(cc_Code) = "Code"
   HeaderCond_Reqd(cc_GsG1) = "G1"
   HeaderCond_Reqd(cc_GsG2) = "G2"
   HeaderCond_Reqd(cc_GsG3) = "G3"
   HeaderCond_Reqd(cc_GsG4) = "G4"
   HeaderCond_Reqd(cc_GsG5) = "G5"
   HeaderCond_Reqd(cc_GsG6) = "G6"
   HeaderCond_Reqd(cc_GsTH) = "TH"
   HeaderCond_Reqd(cc_GsTL) = "TL"
   HeaderCond_Reqd(cc_GsS1) = "S1"
   HeaderCond_Reqd(cc_GsS2) = "S2"
   HeaderCond_Reqd(cc_GsKmax) = "Kmax"
   HeaderCond_Reqd(cc_gsModel) = "gsModel"

   ! ========== SUEWS_OHMCoefficients.txt ========
   HeaderOHMCoefficients_Reqd(cO_Code) = "Code"
   HeaderOHMCoefficients_Reqd(cO_a1) = "a1"
   HeaderOHMCoefficients_Reqd(cO_a2) = "a2"
   HeaderOHMCoefficients_Reqd(cO_a3) = "a3"

   ! ========== SUEWS_ESTMCoefficients.txt ========
   HeaderESTMCoefficients_Reqd(cE_Code) = "Code"
   HeaderESTMCoefficients_Reqd(cE_Surf_thick1) = "Surf_thick1"
   HeaderESTMCoefficients_Reqd(cE_Surf_k1) = "Surf_k1"
   HeaderESTMCoefficients_Reqd(cE_Surf_rhoCp1) = "Surf_rhoCp1"
   HeaderESTMCoefficients_Reqd(cE_Surf_thick2) = "Surf_thick2"
   HeaderESTMCoefficients_Reqd(cE_Surf_k2) = "Surf_k2"
   HeaderESTMCoefficients_Reqd(cE_Surf_rhoCp2) = "Surf_rhoCp2"
   HeaderESTMCoefficients_Reqd(cE_Surf_thick3) = "Surf_thick3"
   HeaderESTMCoefficients_Reqd(cE_Surf_k3) = "Surf_k3"
   HeaderESTMCoefficients_Reqd(cE_Surf_rhoCp3) = "Surf_rhoCp3"
   HeaderESTMCoefficients_Reqd(cE_Surf_thick4) = "Surf_thick4"
   HeaderESTMCoefficients_Reqd(cE_Surf_k4) = "Surf_k4"
   HeaderESTMCoefficients_Reqd(cE_Surf_rhoCp4) = "Surf_rhoCp4"
   HeaderESTMCoefficients_Reqd(cE_Surf_thick5) = "Surf_thick5"
   HeaderESTMCoefficients_Reqd(cE_Surf_k5) = "Surf_k5"
   HeaderESTMCoefficients_Reqd(cE_Surf_rhoCp5) = "Surf_rhoCp5"
   HeaderESTMCoefficients_Reqd(cE_Wall_thick1) = "Wall_thick1"
   HeaderESTMCoefficients_Reqd(cE_Wall_k1) = "Wall_k1"
   HeaderESTMCoefficients_Reqd(cE_Wall_rhoCp1) = "Wall_rhoCp1"
   HeaderESTMCoefficients_Reqd(cE_Wall_thick2) = "Wall_thick2"
   HeaderESTMCoefficients_Reqd(cE_Wall_k2) = "Wall_k2"
   HeaderESTMCoefficients_Reqd(cE_Wall_rhoCp2) = "Wall_rhoCp2"
   HeaderESTMCoefficients_Reqd(cE_Wall_thick3) = "Wall_thick3"
   HeaderESTMCoefficients_Reqd(cE_Wall_k3) = "Wall_k3"
   HeaderESTMCoefficients_Reqd(cE_Wall_rhoCp3) = "Wall_rhoCp3"
   HeaderESTMCoefficients_Reqd(cE_Wall_thick4) = "Wall_thick4"
   HeaderESTMCoefficients_Reqd(cE_Wall_k4) = "Wall_k4"
   HeaderESTMCoefficients_Reqd(cE_Wall_rhoCp4) = "Wall_rhoCp4"
   HeaderESTMCoefficients_Reqd(cE_Wall_thick5) = "Wall_thick5"
   HeaderESTMCoefficients_Reqd(cE_Wall_k5) = "Wall_k5"
   HeaderESTMCoefficients_Reqd(cE_Wall_rhoCp5) = "Wall_rhoCp5"
   HeaderESTMCoefficients_Reqd(cE_Internal_thick1) = "Internal_thick1"
   HeaderESTMCoefficients_Reqd(cE_Internal_k1) = "Internal_k1"
   HeaderESTMCoefficients_Reqd(cE_Internal_rhoCp1) = "Internal_rhoCp1"
   HeaderESTMCoefficients_Reqd(cE_Internal_thick2) = "Internal_thick2"
   HeaderESTMCoefficients_Reqd(cE_Internal_k2) = "Internal_k2"
   HeaderESTMCoefficients_Reqd(cE_Internal_rhoCp2) = "Internal_rhoCp2"
   HeaderESTMCoefficients_Reqd(cE_Internal_thick3) = "Internal_thick3"
   HeaderESTMCoefficients_Reqd(cE_Internal_k3) = "Internal_k3"
   HeaderESTMCoefficients_Reqd(cE_Internal_rhoCp3) = "Internal_rhoCp3"
   HeaderESTMCoefficients_Reqd(cE_Internal_thick4) = "Internal_thick4"
   HeaderESTMCoefficients_Reqd(cE_Internal_k4) = "Internal_k4"
   HeaderESTMCoefficients_Reqd(cE_Internal_rhoCp4) = "Internal_rhoCp4"
   HeaderESTMCoefficients_Reqd(cE_Internal_thick5) = "Internal_thick5"
   HeaderESTMCoefficients_Reqd(cE_Internal_k5) = "Internal_k5"
   HeaderESTMCoefficients_Reqd(cE_Internal_rhoCp5) = "Internal_rhoCp5"
   HeaderESTMCoefficients_Reqd(cE_nroom) = "nroom"
   HeaderESTMCoefficients_Reqd(cE_alb_ibld) = "Internal_albedo"
   HeaderESTMCoefficients_Reqd(cE_em_ibld) = "Internal_emissivity"
   HeaderESTMCoefficients_Reqd(cE_CH_iwall) = "Internal_CHwall"
   HeaderESTMCoefficients_Reqd(cE_CH_iroof) = "Internal_CHroof"
   HeaderESTMCoefficients_Reqd(cE_CH_ibld) = "Internal_CHbld"

   ! ========== SUEWS_AnthropogenicEmission.txt ======
   HeaderAnthropogenic_Reqd(cA_Code) = "Code"
   HeaderAnthropogenic_Reqd(cA_BaseT_HC) = "BaseT_HC"
   HeaderAnthropogenic_Reqd(cA_QF_A1) = "QF_A_WD"
   HeaderAnthropogenic_Reqd(cA_QF_B1) = "QF_B_WD"
   HeaderAnthropogenic_Reqd(cA_QF_C1) = "QF_C_WD"
   HeaderAnthropogenic_Reqd(cA_QF_A2) = "QF_A_WE"
   HeaderAnthropogenic_Reqd(cA_QF_B2) = "QF_B_WE"
   HeaderAnthropogenic_Reqd(cA_QF_C2) = "QF_C_WE"
   HeaderAnthropogenic_Reqd(cA_AHMin_WD) = "AHMin_WD"
   HeaderAnthropogenic_Reqd(cA_AHMin_WE) = "AHMin_WE"
   HeaderAnthropogenic_Reqd(cA_AHSlopeHeating_WD) = "AHSlope_Heating_WD"
   HeaderAnthropogenic_Reqd(cA_AHSlopeHeating_WE) = "AHSlope_Heating_WE"
   HeaderAnthropogenic_Reqd(cA_AHSlopeCooling_WD) = "AHSlope_Cooling_WD"
   HeaderAnthropogenic_Reqd(cA_AHSlopeCooling_WE) = "AHSlope_Cooling_WE"
   HeaderAnthropogenic_Reqd(cA_TCriticHeating_WD) = "TCritic_Heating_WD"
   HeaderAnthropogenic_Reqd(cA_TCriticHeating_WE) = "TCritic_Heating_WE"
   HeaderAnthropogenic_Reqd(cA_TCriticCooling_WD) = "TCritic_Cooling_WD"
   HeaderAnthropogenic_Reqd(cA_TCriticCooling_WE) = "TCritic_Cooling_WE"
   HeaderAnthropogenic_Reqd(cA_EnProfWD) = "EnergyUseProfWD"
   HeaderAnthropogenic_Reqd(cA_EnProfWE) = "EnergyUseProfWE"
   HeaderAnthropogenic_Reqd(cA_CO2mWD) = "ActivityProfWD"
   HeaderAnthropogenic_Reqd(cA_CO2mWE) = "ActivityProfWE"
   HeaderAnthropogenic_Reqd(cA_TraffProfWD) = "TraffProfWD"
   HeaderAnthropogenic_Reqd(cA_TraffProfWE) = "TraffProfWE"
   HeaderAnthropogenic_Reqd(cA_PopProfWD) = "PopProfWD"
   HeaderAnthropogenic_Reqd(cA_PopProfWE) = "PopProfWE"
   HeaderAnthropogenic_Reqd(cA_MinQFMetab) = "MinQFMetab"
   HeaderAnthropogenic_Reqd(cA_MaxQFMetab) = "MaxQFMetab"
   HeaderAnthropogenic_Reqd(cA_MinFCMetab) = "MinFCMetab"
   HeaderAnthropogenic_Reqd(cA_MaxFCMetab) = "MaxFCMetab"
   HeaderAnthropogenic_Reqd(cA_FrPDDwe) = "FrPDDwe"
   HeaderAnthropogenic_Reqd(cA_FrFossilFuel_Heat) = "FrFossilFuel_Heat"
   HeaderAnthropogenic_Reqd(cA_FrFossilFuel_NonHeat) = "FrFossilFuel_NonHeat"
   HeaderAnthropogenic_Reqd(cA_EF_umolCO2perJ) = "EF_umolCO2perJ"
   HeaderAnthropogenic_Reqd(cA_EnEF_v_Jkm) = "EnEF_v_Jkm"
   HeaderAnthropogenic_Reqd(cA_FcEF_v_kgkmWD) = "FcEF_v_kgkmWD"
   HeaderAnthropogenic_Reqd(cA_FcEF_v_kgkmWE) = "FcEF_v_kgkmWE"
   HeaderAnthropogenic_Reqd(cA_CO2PointSource) = "CO2PointSource"
   HeaderAnthropogenic_Reqd(cA_TrafficUnits) = "TrafficUnits"

   ! ========== SUEWS_Irrigation.txt =============
   HeaderIrrigation_Reqd(cIr_Code) = "Code"
   HeaderIrrigation_Reqd(cIr_IeStart) = "Ie_start"
   HeaderIrrigation_Reqd(cIr_IeEnd) = "Ie_end"
   HeaderIrrigation_Reqd(cIr_IntWU) = "InternalWaterUse"
   HeaderIrrigation_Reqd(cIr_Faut) = "Faut"
   HeaderIrrigation_Reqd(cIr_H_maintain) = "H_maintain"
   HeaderIrrigation_Reqd(cIr_Ie_a1) = "Ie_a1"
   HeaderIrrigation_Reqd(cIr_Ie_a2) = "Ie_a2"
   HeaderIrrigation_Reqd(cIr_Ie_a3) = "Ie_a3"
   HeaderIrrigation_Reqd(cIr_Ie_m1) = "Ie_m1"
   HeaderIrrigation_Reqd(cIr_Ie_m2) = "Ie_m2"
   HeaderIrrigation_Reqd(cIr_Ie_m3) = "Ie_m3"
   HeaderIrrigation_Reqd(cIr_DayWat1) = "DayWat(1)"
   HeaderIrrigation_Reqd(cIr_DayWat2) = "DayWat(2)"
   HeaderIrrigation_Reqd(cIr_DayWat3) = "DayWat(3)"
   HeaderIrrigation_Reqd(cIr_DayWat4) = "DayWat(4)"
   HeaderIrrigation_Reqd(cIr_DayWat5) = "DayWat(5)"
   HeaderIrrigation_Reqd(cIr_DayWat6) = "DayWat(6)"
   HeaderIrrigation_Reqd(cIr_DayWat7) = "DayWat(7)"
   HeaderIrrigation_Reqd(cIr_DayWatPer1) = "DayWatPer(1)"
   HeaderIrrigation_Reqd(cIr_DayWatPer2) = "DayWatPer(2)"
   HeaderIrrigation_Reqd(cIr_DayWatPer3) = "DayWatPer(3)"
   HeaderIrrigation_Reqd(cIr_DayWatPer4) = "DayWatPer(4)"
   HeaderIrrigation_Reqd(cIr_DayWatPer5) = "DayWatPer(5)"
   HeaderIrrigation_Reqd(cIr_DayWatPer6) = "DayWatPer(6)"
   HeaderIrrigation_Reqd(cIr_DayWatPer7) = "DayWatPer(7)"

   ! ========== SUEWS_Profiles.txt ===============
   HeaderProfiles_Reqd(cPr_Code) = "Code"
   HeaderProfiles_Reqd(cPr_Hours(1)) = "0"
   HeaderProfiles_Reqd(cPr_Hours(2)) = "1"
   HeaderProfiles_Reqd(cPr_Hours(3)) = "2"
   HeaderProfiles_Reqd(cPr_Hours(4)) = "3"
   HeaderProfiles_Reqd(cPr_Hours(5)) = "4"
   HeaderProfiles_Reqd(cPr_Hours(6)) = "5"
   HeaderProfiles_Reqd(cPr_Hours(7)) = "6"
   HeaderProfiles_Reqd(cPr_Hours(8)) = "7"
   HeaderProfiles_Reqd(cPr_Hours(9)) = "8"
   HeaderProfiles_Reqd(cPr_Hours(10)) = "9"
   HeaderProfiles_Reqd(cPr_Hours(11)) = "10"
   HeaderProfiles_Reqd(cPr_Hours(12)) = "11"
   HeaderProfiles_Reqd(cPr_Hours(13)) = "12"
   HeaderProfiles_Reqd(cPr_Hours(14)) = "13"
   HeaderProfiles_Reqd(cPr_Hours(15)) = "14"
   HeaderProfiles_Reqd(cPr_Hours(16)) = "15"
   HeaderProfiles_Reqd(cPr_Hours(17)) = "16"
   HeaderProfiles_Reqd(cPr_Hours(18)) = "17"
   HeaderProfiles_Reqd(cPr_Hours(19)) = "18"
   HeaderProfiles_Reqd(cPr_Hours(20)) = "19"
   HeaderProfiles_Reqd(cPr_Hours(21)) = "20"
   HeaderProfiles_Reqd(cPr_Hours(22)) = "21"
   HeaderProfiles_Reqd(cPr_Hours(23)) = "22"
   HeaderProfiles_Reqd(cPr_Hours(24)) = "23"

   ! ========== SUEWS_WithinGridWaterDist.txt ====
   HeaderWGWaterDist_Reqd(cWG_Code) = "Code"
   HeaderWGWaterDist_Reqd(cWG_ToPaved) = "ToPaved"
   HeaderWGWaterDist_Reqd(cWG_ToBldgs) = "ToBldgs"
   HeaderWGWaterDist_Reqd(cWG_ToEveTr) = "ToEveTr"
   HeaderWGWaterDist_Reqd(cWG_ToDecTr) = "ToDecTr"
   HeaderWGWaterDist_Reqd(cWG_ToGrass) = "ToGrass"
   HeaderWGWaterDist_Reqd(cWG_ToBSoil) = "ToBSoil"
   HeaderWGWaterDist_Reqd(cWG_ToWater) = "ToWater"
   HeaderWGWaterDist_Reqd(cWG_ToRunoff) = "ToRunoff"
   HeaderWGWaterDist_Reqd(cWG_ToSoilStore) = "ToSoilStore"

   ! ========== SUEWS_BiogenCO2.txt ======
   HeaderBiogen_Reqd(cB_Code) = "Code"
   HeaderBiogen_Reqd(cB_alpha) = "alpha"
   HeaderBiogen_Reqd(cB_beta) = "beta"
   HeaderBiogen_Reqd(cB_theta) = "theta"
   HeaderBiogen_Reqd(cB_alpha_enh) = "alpha_enh"
   HeaderBiogen_Reqd(cB_beta_enh) = "beta_enh"
   HeaderBiogen_Reqd(cB_resp_a) = "resp_a"
   HeaderBiogen_Reqd(cB_resp_b) = "resp_b"
   HeaderBiogen_Reqd(cB_min_r) = "min_respi"

   ! =======================================================

   !write(*,*) 'Checking header for ', FileName
   ! Check columns in input files match model code

   IF (FileName == 'SUEWS_NonVeg.txt') THEN
      IF (ANY(HeaderNonVeg_File /= HeaderNonVeg_Reqd)) THEN
         WRITE (*, *) HeaderNonVeg_File == HeaderNonVeg_Reqd
         WRITE (*, *) HeaderNonVeg_File
         WRITE (*, *) HeaderNonVeg_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_NonVeg.txt does not match model code.', notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_Veg.txt') THEN
      IF (ANY(HeaderVeg_File /= HeaderVeg_Reqd)) THEN
         WRITE (*, *) HeaderVeg_File == HeaderVeg_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_Veg.txt does not match model code.', notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_Water.txt') THEN
      IF (ANY(HeaderWater_File /= HeaderWater_Reqd)) THEN
         WRITE (*, *) HeaderWater_File == HeaderWater_Reqd
         WRITE (*, *) HeaderWater_File
         WRITE (*, *) HeaderWater_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_Water.txt does not match model code.', notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_Snow.txt') THEN
      IF (ANY(HeaderSnow_File /= HeaderSnow_Reqd)) THEN
         WRITE (*, *) HeaderSnow_File == HeaderSnow_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_Snow.txt does not match model code.', notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_Soil.txt') THEN
      IF (ANY(HeaderSoil_File /= HeaderSoil_Reqd)) THEN
         WRITE (*, *) HeaderSoil_File == HeaderSoil_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_Soil.txt does not match model code.', notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_Conductance.txt') THEN
      IF (ANY(HeaderCond_File /= HeaderCond_Reqd)) THEN
         WRITE (*, *) HeaderCond_File == HeaderCond_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_Cond.txt does not match model code.', notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_OHMCoefficients.txt') THEN
      IF (ANY(HeaderOHMCoefficients_File /= HeaderOHMCoefficients_Reqd)) THEN
         WRITE (*, *) HeaderOHMCoefficients_File == HeaderOHMCoefficients_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_OHMCoefficients.txt does not match model code.', &
                        notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_ESTMCoefficients.txt') THEN
      IF (ANY(HeaderESTMCoefficients_File /= HeaderESTMCoefficients_Reqd)) THEN
         WRITE (*, *) HeaderESTMCoefficients_File == HeaderESTMCoefficients_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_ESTMCoefficients.txt does not match model code.', &
                        notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_AnthropogenicEmission.txt') THEN
      IF (ANY(HeaderAnthropogenic_File /= HeaderAnthropogenic_Reqd)) THEN
         WRITE (*, *) HeaderAnthropogenic_File == HeaderAnthropogenic_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_AnthropogenicEmission.txt does not match model code.', &
                        notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_Irrigation.txt') THEN
      IF (ANY(HeaderIrrigation_File /= HeaderIrrigation_Reqd)) THEN
         WRITE (*, *) HeaderIrrigation_File == HeaderIrrigation_Reqd
         WRITE (*, *) HeaderIrrigation_File
         WRITE (*, *) HeaderIrrigation_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_Irrigation.txt does not match model code.', &
                        notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_Profiles.txt') THEN
      IF (ANY(HeaderProfiles_File /= HeaderProfiles_Reqd)) THEN
         WRITE (*, *) HeaderProfiles_File == HeaderProfiles_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_Profiles.txt does not match model code.', &
                        notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_WithinGridWaterDist.txt') THEN
      IF (ANY(HeaderWGWaterDist_File /= HeaderWGWaterDist_Reqd)) THEN
         WRITE (*, *) HeaderWGWaterDist_File == HeaderWGWaterDist_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_WithinGridWaterDist.txt does not match model code.', &
                        notUsed, notUsed, notUsedI)
      END IF

   ELSEIF (FileName == 'SUEWS_BiogenCO2.txt') THEN
      IF (ANY(HeaderBiogen_File /= HeaderBiogen_Reqd)) THEN
         WRITE (*, *) HeaderBiogen_File == HeaderBiogen_Reqd
         CALL ErrorHint(56, 'Names or order of columns in SUEWS_BiogenCO2.txt does not match model code.', &
                        notUsed, notUsed, notUsedI)
      END IF

   ELSE
      WRITE (*, *) 'Problem in subroutine InputHeaderCheck. File header not specified in model code for ', FileName
      CALL ErrorHint(58, FileName, notUsed, notUsed, notUsedI)

   END IF

END SUBROUTINE InputHeaderCheck

!-------------------------------------------------------------------------
!
! TS 05 Jul 2018: No longer needed as interpolation is done through specific subroutines at each required instant
!Interpolates hourly profiles provided in SUEWS_Profiles.txt
! to resolution of the model timestep
! HCW 06 Feb 2015
!===================================================================================
! SUBROUTINE SUEWS_InterpHourlyProfiles(Gridiv,TstepP_ID,SurfChar_HrProf)
!
!   USE module_ctrl_const_allocate
!   USE module_ctrl_const_colnames_input
!   USE module_ctrl_const_sues
!
!   IMPLICIT NONE
!
!   INTEGER:: i,j, ii   !Used to count over hours and sub-hourly timesteps
!   INTEGER:: Gridiv, TstepP_ID
!   INTEGER,DIMENSION(24):: SurfChar_HrProf
!   REAL(KIND(1d0)):: deltaProf   !Change in hourly profiles per model timestep
!
!   ! Copy value for first hour
!   TstepProfiles(Gridiv,TstepP_ID,1) = SurfaceChar(Gridiv,SurfChar_HrProf(1))
!   DO i=1,24
!      j = (i+1)
!      IF(i == 24) j = 1   !If last hour of day, loop round to first hour of day for interpolation
!      deltaProf = ((SurfaceChar(Gridiv,SurfChar_HrProf(j)) - SurfaceChar(Gridiv,SurfChar_HrProf(i))))/nsh_real
!      DO ii=1,nsh
!         IF((nsh*(i-1)+ii+1) < (23*nsh+nsh+1))  THEN
!            TstepProfiles(Gridiv,TstepP_ID,(nsh*(i-1)+ii+1)) = SurfaceChar(Gridiv,SurfChar_HrProf(i)) + deltaProf*ii
!         ENDIF
!      ENDDO
!   ENDDO
!
! endsubroutine SUEWS_InterpHourlyProfiles
!===================================================================================
MODULE get_prof_module
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

END MODULE get_prof_module

!===================================================================================
! TODO: #79 all the parameter tables can be stored using a custom dictionary for easier and more consistent access:
! this package looks promising: https://github.com/zerothi/fdict

! Subroutines for matching codes in the input files
!  could re-write as a generic function later...

SUBROUTINE CodeMatchOHM(Gridiv, is, SWWD)
   ! Matches OHM coefficients a1, a2, a3 via OHM codes
   ! for summer/winter wet/dry conditions
   ! HCW 03 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: gridiv
   INTEGER :: is
   CHARACTER(len=4) :: SWWD

   iv5 = 0 ! Reset iv5 to zero

   IF (SWWD == 'SWet') THEN

      DO iv5 = 1, nlinesOHMCoefficients
         IF (OHMCoefficients_Coeff(iv5, cO_Code) == SurfaceChar(gridiv, c_OHMCode_SWet(is))) THEN
            EXIT
         ELSEIF (iv5 == nlinesOHMCoefficients) THEN
            WRITE (*, *) 'Program stopped! OHM code (summer wet)', SurfaceChar(gridiv, c_OHMCode_SWet(is)), &
               'not found in OHM_Coefficients.txt for surface', is, '.'
            CALL ErrorHint(57, 'Cannot find OHM code (summer wet)', SurfaceChar(gridiv, c_OHMCode_SWet(is)), notUsed, notUsedI)
         END IF
      END DO

   ELSEIF (SWWD == 'SDry') THEN

      DO iv5 = 1, nlinesOHMCoefficients
         IF (OHMCoefficients_Coeff(iv5, cO_Code) == SurfaceChar(gridiv, c_OHMCode_SDry(is))) THEN
            EXIT
         ELSEIF (iv5 == nlinesOHMCoefficients) THEN
            WRITE (*, *) 'Program stopped! OHM code (summer dry)', SurfaceChar(gridiv, c_OHMCode_SDry(is)), &
               'not found in OHM_Coefficients.txt for surface', is, '.'
            CALL ErrorHint(57, 'Cannot find OHM code (summer dry)', SurfaceChar(gridiv, c_OHMCode_SDry(is)), notUsed, notUsedI)
         END IF
      END DO

   ELSEIF (SWWD == 'WWet') THEN

      DO iv5 = 1, nlinesOHMCoefficients
         IF (OHMCoefficients_Coeff(iv5, cO_Code) == SurfaceChar(gridiv, c_OHMCode_WWet(is))) THEN
            EXIT
         ELSEIF (iv5 == nlinesOHMCoefficients) THEN
            WRITE (*, *) 'Program stopped! OHM code (winter wet)', SurfaceChar(gridiv, c_OHMCode_WWet(is)), &
               'not found in OHM_Coefficients.txt for surface', is, '.'
            CALL ErrorHint(57, 'Cannot find OHM code (winter wet)', SurfaceChar(gridiv, c_OHMCode_WWet(is)), notUsed, notUsedI)
         END IF
      END DO

   ELSEIF (SWWD == 'WDry') THEN

      DO iv5 = 1, nlinesOHMCoefficients
         IF (OHMCoefficients_Coeff(iv5, cO_Code) == SurfaceChar(gridiv, c_OHMCode_WDry(is))) THEN
            EXIT
         ELSEIF (iv5 == nlinesOHMCoefficients) THEN
            WRITE (*, *) 'Program stopped! OHM code (winter dry)', SurfaceChar(gridiv, c_OHMCode_WDry(is)), &
               'not found in OHM_Coefficients.txt for surface', is, '.'
            CALL ErrorHint(57, 'Cannot find OHM code (winter dry)', SurfaceChar(gridiv, c_OHMCode_WDry(is)), notUsed, notUsedI)
         END IF
      END DO

   ELSE
      WRITE (*, *) 'Problem with CodeMatchOHM (in SUEWS_CodeMatch.f95). ', SWWD, ' not recognised. Needs to be one of: ', &
         'SWet = Summer Wet, SDry = Summer Dry, WWet = WinterWet, WDry = Winter Dry. N.B. Case sensitive. '
      STOP
   END IF

   RETURN
END SUBROUTINE CodeMatchOHM
! ---------------------------------------------------------

SUBROUTINE CodeMatchESTM(Gridiv, is)
   ! Matches ESTM coefficients via ESTM code
   ! Modified HCW 16 Jun 2016 - for SUEWS surface types
   !                          - removed summer/winter wet/dry option
   ! S.O. 04 Feb 2016
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: gridiv
   INTEGER :: is

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesESTMCoefficients
      IF (ESTMCoefficients_Coeff(iv5, cE_Code) == SurfaceChar(gridiv, c_ESTMCode(is))) THEN
         EXIT
      ELSEIF (iv5 == nlinesESTMCoefficients) THEN
         WRITE (*, *) 'Program stopped! ESTM code', SurfaceChar(gridiv, c_ESTMCode(is)), &
            'not found in ESTM_Coefficients.txt for surface', is, '.'
         CALL ErrorHint(57, 'Cannot find ESTM code', SurfaceChar(gridiv, c_ESTMCode(is)), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchESTM
! ---------------------------------------------------------

SUBROUTINE CodeMatchESTM_Class(Gridiv, is, ii)
   ! Matches ESTM coefficients via ESTM codes in SiteSelect for Paved and Bldgs ESTM classes
   ! HCW 16 Jun 2016
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: gridiv
   INTEGER :: is, ii

   iv5 = 0 ! Reset iv5 to zero

   IF (is == BldgSurf) THEN
      DO iv5 = 1, nlinesESTMCoefficients
         IF (ESTMCoefficients_Coeff(iv5, cE_Code) == SurfaceChar(gridiv, c_Code_ESTMClass_Bldgs(ii))) THEN
            EXIT
         ELSEIF (iv5 == nlinesESTMCoefficients) THEN
            WRITE (*, *) 'Program stopped! ESTM code', SurfaceChar(gridiv, c_Code_ESTMClass_Bldgs(ii)), &
               'not found in ESTM_Coefficients.txt for surface', is, '.'
            CALL ErrorHint(57, 'Cannot find ESTM code', SurfaceChar(gridiv, c_Code_ESTMClass_Bldgs(ii)), notUsed, notUsedI)
         END IF
      END DO
   ELSEIF (is == PavSurf) THEN
      DO iv5 = 1, nlinesESTMCoefficients
         IF (ESTMCoefficients_Coeff(iv5, cE_Code) == SurfaceChar(gridiv, c_Code_ESTMClass_Paved(ii))) THEN
            EXIT
         ELSEIF (iv5 == nlinesESTMCoefficients) THEN
            WRITE (*, *) 'Program stopped! ESTM code', SurfaceChar(gridiv, c_Code_ESTMClass_Paved(ii)), &
               'not found in ESTM_Coefficients.txt for surface', is, '.'
            CALL ErrorHint(57, 'Cannot find ESTM code', SurfaceChar(gridiv, c_Code_ESTMClass_Paved(ii)), notUsed, notUsedI)
         END IF
      END DO
   ELSE
      WRITE (*, *) 'Problem with CodeMatchESTM_Class (in SUEWS_ctrl_input.f95). ', is, ' not correct. Needs to be either ', &
         '1 = Paved surfaced, 2 = Bldgs surfaces.'
      STOP
   END IF
   RETURN
END SUBROUTINE CodeMatchESTM_Class
! ---------------------------------------------------------

SUBROUTINE CodeMatchProf(Gridiv, SurfaceCharCodeCol)
   ! Matches Soil characteristics via codes in *SurfaceChar*
   ! for energy use/water use/snow clearing
   ! HCW 20 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: Gridiv
   INTEGER :: SurfaceCharCodeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesProfiles
      IF (Profiles_Coeff(iv5, cPr_Code) == SurfaceChar(Gridiv, SurfaceCharCodeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesProfiles) THEN
         WRITE (*, *) 'Program stopped! Profile code ', SurfaceChar(Gridiv, SurfaceCharCodeCol), 'not found in SUEWS_Profiles.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_Profiles.txt', SurfaceChar(Gridiv, SurfaceCharCodeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchProf
! ---------------------------------------------------------

SUBROUTINE CodeMatchDist(rr, CodeCol, codeColSameSurf)
   ! Matches within-grid water distribution via codes
   ! Checks water cannot flow from one surface to the same surface
   ! Checks water distribution fractions sum to 1.
   ! HCW 10 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: rr
   INTEGER :: codeCol, codeColSameSurf

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesWGWaterDist
      IF (WGWaterDist_Coeff(iv5, cWG_Code) == SiteSelect(rr, codeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesWGWaterDist) THEN
         WRITE (*, *) 'Program stopped! Within-grid water distribution code ', SiteSelect(rr, codeCol), &
            'not found in SUEWS_WaterDistWithinGrid.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_WaterDistWithinGrid.txt', SiteSelect(rr, codeCol), notUsed, notUsedI)
      END IF
   END DO

   ! Check water flow to same surface is zero (previously in RunControlByGridByYear in SUEWS_Initial.f95)
   IF (WGWaterDist_Coeff(iv5, codeColSameSurf) /= 0) THEN
      CALL ErrorHint(8, 'Diagonal elements should be zero as water cannot move from one surface to the same surface.', &
                     WGWaterDist_Coeff(iv5, codeColSameSurf), notUsed, notUsedI)
   END IF

   !! QUESTION: MODIFY THIS?
   ! Check water either moves to runoff or soilstore, but not to both
   ! Model returns an error if both ToRunoff and ToSoilStore are non-zero.
   !! - Probably should remove this...
   !! - Also look at SUEWS_translate, as the non-zero value goes into WaterDist
   IF (WGWaterDist_Coeff(iv5, cWG_ToRunoff) /= 0 .AND. WGWaterDist_Coeff(iv5, cWG_ToSoilStore) /= 0) THEN
      CALL ErrorHint(9, 'One of these (ToRunoff,ToSoilStore) should be zero.', &
                     WGWaterDist_Coeff(iv5, cWG_ToRunoff), WGWaterDist_Coeff(iv5, cWG_ToSoilStore), notUsedI)
   END IF

   !! Also do for water surface once implemented
   IF (codeCol /= c_WGWaterCode) THEN ! Except for Water surface
      ! Check total water distribution from each surface adds up to 1
      IF (SUM(WGWaterDist_Coeff(iv5, cWG_ToPaved:cWG_ToSoilStore)) > 1.0000001 &
          .OR. SUM(WGWaterDist_Coeff(iv5, cWG_ToPaved:cWG_ToSoilStore)) < 0.9999999) THEN
         CALL ErrorHint(8, 'Total water distribution from each surface should add up to 1.', &
                        SUM(WGWaterDist_Coeff(iv5, cWG_ToPaved:cWG_ToSoilStore)), notUsed, notUsedI)
      END IF
   END IF

   RETURN
END SUBROUTINE CodeMatchDist
! ---------------------------------------------------------

SUBROUTINE CodeMatchNonVeg(rr, CodeCol)
   ! Matches Impervious characteristics via codes in SiteSelect
   ! HCW 20 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: rr
   INTEGER :: codeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesNonVeg
      IF (NonVeg_Coeff(iv5, ci_Code) == SiteSelect(rr, codeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesNonVeg) THEN
         WRITE (*, *) 'Program stopped! NonVeg code ', SiteSelect(rr, codeCol), 'not found in SUEWS_NonVeg.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_NonVeg.txt', SiteSelect(rr, codeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchNonVeg
! ---------------------------------------------------------

SUBROUTINE CodeMatchVeg(rr, CodeCol)
   ! Matches Pervious characteristics via codes in SiteSelect
   ! HCW 20 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: rr
   INTEGER :: codeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesVeg
      IF (Veg_Coeff(iv5, cp_Code) == SiteSelect(rr, codeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesVeg) THEN
         WRITE (*, *) 'Program stopped! Veg code ', SiteSelect(rr, codeCol), 'not found in SUEWS_Vegs.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_Veg.txt', SiteSelect(rr, codeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchVeg
! ---------------------------------------------------------

SUBROUTINE CodeMatchWater(rr, CodeCol)
   ! Matches Water characteristics via codes in SiteSelect
   ! HCW 20 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: rr
   INTEGER :: codeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesWater
      IF (Water_Coeff(iv5, cw_Code) == SiteSelect(rr, codeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesWater) THEN
         WRITE (*, *) 'Program stopped! Water code ', SiteSelect(rr, codeCol), 'not found in SUEWS_Water.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_Water.txt', SiteSelect(rr, codeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchWater
! ---------------------------------------------------------

SUBROUTINE CodeMatchSnow(rr, CodeCol)
   ! Matches Snow characteristics via codes in SiteSelect
   ! HCW 20 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: rr
   INTEGER :: codeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesSnow
      IF (Snow_Coeff(iv5, cs_Code) == SiteSelect(rr, codeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesSnow) THEN
         WRITE (*, *) 'Program stopped! Snow code ', SiteSelect(rr, codeCol), 'not found in SUEWS_Snow.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_Snow.txt', SiteSelect(rr, codeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchSnow
! ---------------------------------------------------------

SUBROUTINE CodeMatchConductance(rr, CodeCol)
   ! Matches Conductance characteristics via codes in SiteSelect
   ! HCW 20 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: rr
   INTEGER :: codeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesConductance
      IF (Conductance_Coeff(iv5, cc_Code) == SiteSelect(rr, codeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesConductance) THEN
         WRITE (*, *) 'Program stopped! Conductance code ', SiteSelect(rr, codeCol), 'not found in SUEWS_Conductance.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_Conductance.txt', SiteSelect(rr, codeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchConductance
! ---------------------------------------------------------

SUBROUTINE CodeMatchAnthropogenic(rr, CodeCol)
   ! Matches AnthropogenicHeat characteristics via codes in SiteSelect
   ! HCW 20 Nov 2014
   ! MH 21 Jun 2017
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: rr
   INTEGER :: codeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesAnthropogenic
      IF (Anthropogenic_Coeff(iv5, cA_Code) == SiteSelect(rr, codeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesAnthropogenic) THEN
         WRITE (*, *) 'Program stopped! Anthropogenic code ', SiteSelect(rr, codeCol), &
            'not found in SUEWS_AnthropogenicEmission.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_AnthropogenicEmission.txt', &
                        SiteSelect(rr, codeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchAnthropogenic
! ---------------------------------------------------------

SUBROUTINE CodeMatchIrrigation(rr, CodeCol)
   ! Matches Irrigation characteristics via codes in SiteSelect
   ! HCW 20 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: rr
   INTEGER :: codeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesIrrigation
      IF (Irrigation_Coeff(iv5, cIr_Code) == SiteSelect(rr, codeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesIrrigation) THEN
         WRITE (*, *) 'Program stopped! Irrigation code ', SiteSelect(rr, codeCol), 'not found in SUEWS_Irrigation.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_Irrigation.txt', SiteSelect(rr, codeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchIrrigation
! ---------------------------------------------------------

SUBROUTINE CodeMatchSoil(Gridiv, SurfaceCharCodeCol)
   ! Matches Soil characteristics via codes in *SurfaceChar*
   ! HCW 20 Nov 2014
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: Gridiv
   INTEGER :: SurfaceCharCodeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesSoil
      IF (Soil_Coeff(iv5, cSo_Code) == SurfaceChar(Gridiv, SurfaceCharCodeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesSoil) THEN
         WRITE (*, *) 'Program stopped! Soil code ', SurfaceChar(Gridiv, SurfaceCharCodeCol), 'not found in SUEWS_Soil.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_Soil.txt', SurfaceChar(Gridiv, SurfaceCharCodeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchSoil
! ---------------------------------------------------------

SUBROUTINE CodeMatchBiogen(Gridiv, SurfaceCharCodeCol)
   ! Matches Biogen characteristics via codes in *SuraceChar*
   ! MH 16 Jun 2017
   ! ---------------------------------------------------------

   USE module_ctrl_const_allocate
   USE module_ctrl_const_initial
   USE module_ctrl_const_colnames_input
   USE module_ctrl_const_default

   IMPLICIT NONE

   INTEGER :: Gridiv
   INTEGER :: SurfaceCharCodeCol

   iv5 = 0 ! Reset iv5 to zero

   DO iv5 = 1, nlinesBiogen
      IF (Biogen_Coeff(iv5, cB_Code) == SurfaceChar(Gridiv, SurfaceCharCodeCol)) THEN
         EXIT
      ELSEIF (iv5 == nlinesBiogen) THEN
         WRITE (*, *) 'Program stopped! Biogen code ', SurfaceChar(Gridiv, SurfaceCharCodeCol), 'not found in SUEWS_BiogenCO2.txt.'
         CALL ErrorHint(57, 'Cannot find code in SUEWS_BiogenCO2.txt', SurfaceChar(Gridiv, SurfaceCharCodeCol), notUsed, notUsedI)
      END IF
   END DO

   RETURN
END SUBROUTINE CodeMatchBiogen
! ---------------------------------------------------------

MODULE MetDisagg
   !========================================================================================
   ! Disaggregation of meteorological forcing data
   !  Code to disaggregate met forcing data from resolution provided to the model time-step
   ! Created: HCW 10 Feb 2017
   !
   ! ---- Key for MetDisaggMethod settings ----
   !10  ->  linear disaggregation for timestamps representing the end of the averaging period
   !20  ->  linear disaggregation for instantaneous variables (e.g. air temperature, humidity, pressure, wind speed in WFDEI dataset)
   !100 -> evenly distribute rainfall among all subintervals in a rainy interval
   !101 -> evenly distribute rainfall among RainAmongN subintervals in a rainy interval
   !        - requires RainAmongN to be set in RunControl.nml
   ! If KdownZen = 1 -> include additional zenith check in kdown disaggregation
   !
   ! N.B. wdir downscaling is currently not implemented
   !
   !========================================================================================

   USE AllocateArray
   USE module_ctrl_const_colnames_input
   USE Data_In
   USE module_ctrl_const_initial
   USE module_phys_narp, ONLY: NARP_cal_SunPosition

   IMPLICIT NONE

CONTAINS

   !======================================================================================
   SUBROUTINE DisaggregateMet(iBlock, igrid)
      ! Subroutine to disaggregate met forcing data to model time-step
      ! HCW 10 Feb 2017
      !======================================================================================

      USE Sues_Data
      USE DefaultNotUsed

      IMPLICIT NONE

      INTEGER :: lunit = 100
      INTEGER :: tdiff !Time difference (in minutes) between first and second rows of original met forcing file
      INTEGER :: i, ii !counter
      INTEGER :: iBlock, igrid
      INTEGER, DIMENSION(NperTstepIn) :: seq1Nper
      INTEGER, DIMENSION(nsd) :: seq1nsd
      INTEGER, DIMENSION(nColumnsMetForcingData) :: MetDisaggMethod ! Stores method to use for disaggregating met data
      REAL(KIND(1D0)), DIMENSION(nColumnsMetForcingData) :: MetArrayOrig
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrigMetData*NperTstepIn, ncolumnsMetForcingData) :: Met_tt
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrigMetData*NperTstepIn) :: Met_tt_kdownAdj
      CHARACTER(LEN=9), DIMENSION(ncolumnsMetForcingData) :: HeaderMet
      CHARACTER(LEN=10*ncolumnsMetForcingData) :: HeaderMetOut
      ! REAL(KIND(1d0)),DIMENSION(ReadLinesOrigMetData):: dectimeOrig
      ! REAL(KIND(1d0)),DIMENSION(ReadLinesOrigMetData*Nper):: dectimeDscd, dectimeFast
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrigMetData*NperTstepIn) :: dectimeFast
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrigMetData*NperTstepIn) :: idectime ! sun position at middle of time-step before

      ! INTEGER, DIMENSION(Nper):: temp_iy, temp_id, temp_ih, temp_im, temp_ihm ! temorary varaibles for disaggragation
      ! REAL(KIND(1d0)), DIMENSION(Nper):: temp_dectime ! temorary varaibles for disaggragation
      ! INTEGER :: Days_of_Year
      ! INTEGER, DIMENSION(Nper)::ndays_iy ! number of days in iy

      ! Allocate and initialise arrays to receive original forcing data --------------------
      ALLOCATE (MetForDisagg(ReadLinesOrigMetData, nColumnsMetForcingData))
      ALLOCATE (MetForDisaggPrev(nColumnsMetForcingData))
      ALLOCATE (MetForDisaggNext(nColumnsMetForcingData))
      MetForDisagg(:, :) = -999
      MetForDisaggPrev(:) = -999
      MetForDisaggNext(:) = -999
      ! Initialise array to receive disaggregated data
      Met_tt = -999
      ! Intialise disaggregation method
      MetDisaggMethod = -999

      ! Generate useful sequences
      seq1Nper = (/(i, i=1, NperTstepIn, 1)/)
      seq1nsd = (/(i, i=1, nsd, 1)/)

      ! Get methods to use for disaggregation from RunControl
      IF (DiagnoseDisagg == 1) WRITE (*, *) 'DisaggMethod: ', DisaggMethod, 'RainDisaggMethod:', RainDisaggMethod
      IF (DisaggMethod == 1) THEN
         MetDisaggMethod(:) = 10 !linear disaggregation of averages
      ELSEIF (DisaggMethod == 2) THEN
         MetDisaggMethod(:) = 20 !linear disaggregation of instantaneous values
      ELSEIF (DisaggMethod == 3) THEN !WFDEI set up, where T, Q, pres, U are instantaneous
         MetDisaggMethod(:) = 10 !linear disaggregation of averages
         MetDisaggMethod(10:13) = 20 !linear disagg instantaneous values for U, RH, Tair, pres
      ELSE
         CALL errorHint(2, 'Problem in SUEWS_MetDisagg: DisaggMethod value should be 1, 2, or 3', &
                        NotUsed, NotUsed, DisaggMethod)
      END IF
      ! Set rainfall
      MetDisaggMethod(14) = RainDisaggMethod

      ! Read data ---------------------------------------------------------------------
      IF (DiagnoseDisagg == 1) WRITE (*, *) 'Reading file: ', TRIM(FileOrigMet)
      OPEN (lunit, file=TRIM(FileOrigMet), status='old')
      ! CALL skipHeader(lunit,SkipHeaderMet)  !Skip header -> read header instead
      READ (lunit, *) HeaderMet
      !write(*,*) HeaderMet
      ! Skip over lines that have already been read and downscaled
      IF (SkippedLinesOrig > 0) THEN
         DO i = 1, skippedLinesOrig - 1 ! minus 1 here because last line of last block needs to be read again
            READ (lunit, *)
         END DO
         ! Read in last line of previous block
         CALL MetRead(lunit, MetArrayOrig, InputmetFormat, ldown_option, NetRadiationMethod, &
                      SnowUse, SMDMethod, SoilDepthMeas, SoilRocks, SoilDensity, SmCap)
         MetForDisaggPrev(1:ncolumnsMetForcingData) = MetArrayOrig
      END IF
      ! print*, 'MetForDisagg',MetForDisagg(1:3,1:4)
      ! print*, 'ReadLinesOrigMetDataMax',ReadLinesOrigMetDataMax
      ! Read in current block
      DO i = 1, ReadLinesOrigMetDataMax
         CALL MetRead(lunit, MetArrayOrig, InputmetFormat, ldown_option, NetRadiationMethod, &
                      SnowUse, SMDMethod, SoilDepthMeas, SoilRocks, SoilDensity, SmCap)
         MetForDisagg(i, 1:ncolumnsMetForcingData) = MetArrayOrig
      END DO
      ! print*, 'MetForDisagg',MetForDisagg(1:3,1:4)
      ! Read in first line of next block (except for last block)
      IF (iBlock /= ReadBlocksOrigMetData) THEN
         CALL MetRead(lunit, MetArrayOrig, InputmetFormat, ldown_option, NetRadiationMethod, &
                      SnowUse, SMDMethod, SoilDepthMeas, SoilRocks, SoilDensity, SmCap)
         MetForDisaggNext(1:ncolumnsMetForcingData) = MetArrayOrig
      END IF
      CLOSE (lunit)

      ! Check resolution of original met forcing data -------------------------------------
      ! Find time difference (in minutes) between first and second row
      tdiff = INT(MetForDisagg(2, 4) - MetForDisagg(1, 4)) !Try using minutes
      IF (tdiff == 0) tdiff = INT(MetForDisagg(2, 3) - MetForDisagg(1, 3))*60 !If no difference in minutes, try using hours
      IF (tdiff < 0) THEN !If time difference is negative (e.g. change of day), instead use second and third row
         tdiff = INT(MetForDisagg(3, 4) - MetForDisagg(2, 4))
         IF (tdiff == 0) tdiff = INT(MetForDisagg(3, 3) - MetForDisagg(2, 3))*60 !If no difference in minutes, try using hours
      END IF
      ! Check actual resolution matches specified input resolution
      IF (tdiff /= ResolutionFilesIn/60) THEN
         CALL errorHint(2, 'Problem in SUEWS_MetDisagg: timestamps in met forcing file inconsistent with ResolutionFilesIn', &
                        REAL(ResolutionFilesIn, KIND(1D0)), NotUsed, tdiff*60)
      END IF

      ! Check file only contains a single year --------------------------------------------
      ! Very last data point is allowed to be (should be) timestamped with following year
      IF (ANY(MetForDisagg(1:(ReadLinesOrigMetDataMax - 1), 1) /= MetForDisagg(1, 1))) THEN
         CALL errorHint(3, 'Problem in SUEWS_MetDisagg: multiple years found in original met forcing file.', &
                        MetForDisagg(1, 1), NotUsed, NotUsedI)
      END IF

      ! Disaggregate time columns ---------------------------------------------------------
      IF (Diagnose == 1) WRITE (*, *) 'Disaggregating met forcing data (', TRIM(FileOrigMet), ') to model time-step...'

      CALL DisaggregateDateTime(MetForDisagg(:, 1:4), tstep, NperTstepIn, ReadLinesOrigMetDataMax, Met_tt(:, 1:4))

      ! Disaggregate other columns --------------------------------------------------------
      DO ii = 5, ncolumnsMetForcingData
         IF (ii == 14) THEN !Do something different for rainfall and snowfall (if present)
            IF (MetDisaggMethod(14) == 100) THEN
               Met_tt(:, 14) = DisaggP_amongN(MetForDisagg(:, 14), &
                                              NperTstepIn, NperTstepIn, ReadLinesOrigMetData, ReadLinesOrigMetDataMax)
               IF (ALL(MetForDisagg(:, 16) == -999)) THEN
                  Met_tt(:, 16) = -999
               ELSE
                  Met_tt(:, 16) = DisaggP_amongN(MetForDisagg(:, 16), &
                                                 NperTstepIn, NperTstepIn, ReadLinesOrigMetData, ReadLinesOrigMetDataMax)
               END IF
            ELSEIF (MetDisaggMethod(14) == 101) THEN
               IF (RainAmongN == -999) THEN
                  CALL ErrorHint(2, 'Problem in SUEWS_MetDisagg: RainDisaggMethod requires RainAmongN', &
                                 REAL(RainAmongN, KIND(1D0)), NotUsed, RainDisaggMethod)
               ELSEIF (RainAmongN > NperTstepIn) THEN
                  CALL ErrorHint(2, 'Problem in SUEWS_MetDisagg: RainAmongN > Nper', &
                                 REAL(NperTstepIn, KIND(1D0)), NotUsed, RainAmongN)
               ELSE
                  Met_tt(:, 14) = DisaggP_amongN(MetForDisagg(:, 14), &
                                                 RainAmongN, NperTstepIn, ReadLinesOrigMetData, ReadLinesOrigMetDataMax)
                  IF (ALL(MetForDisagg(:, 16) == -999)) THEN
                     Met_tt(:, 16) = -999
                  ELSE
                     Met_tt(:, 16) = DisaggP_amongN(MetForDisagg(:, 16), &
                                                    RainAmongN, NperTstepIn, ReadLinesOrigMetData, ReadLinesOrigMetDataMax)
                  END IF
               END IF
            ELSEIF (MetDisaggMethod(14) == 102) THEN
               IF (ALL(MultRainAmongN == -999)) THEN
                  CALL ErrorHint(2, 'Problem in SUEWS_MetDisagg: RainDisaggMethod requires MultRainAmongN', &
                                 REAL(MultRainAmongN(1), KIND(1D0)), NotUsed, RainDisaggMethod)
               ELSEIF (ALL(MultRainAmongNUpperI == -999)) THEN
                  CALL ErrorHint(2, 'Problem in SUEWS_MetDisagg: RainDisaggMethod requires MultRainAmongNUpperI', &
                                 MultRainAmongNUpperI(1), NotUsed, RainDisaggMethod)
               ELSEIF (ANY(MultRainAmongN > NperTstepIn)) THEN
                  CALL ErrorHint(2, 'Problem in SUEWS_MetDisagg: MultRainAmongN > Nper', REAL(NperTstepIn, KIND(1D0)), NotUsed, &
                                 MAXVAL(MultRainAmongN))
               ELSE
                  Met_tt(:, 14) = DisaggP_amongNMult(MetForDisagg(:, 14), MultRainAmongNUpperI, MultRainAmongN, NperTstepIn, &
                                                     ReadLinesOrigMetData, ReadLinesOrigMetDataMax)
                  IF (ALL(MetForDisagg(:, 16) == -999)) THEN
                     Met_tt(:, 16) = -999
                  ELSE
                     Met_tt(:, 16) = DisaggP_amongNMult(MetForDisagg(:, 16), MultRainAmongNUpperI, MultRainAmongN, NperTstepIn, &
                                                        ReadLinesOrigMetData, ReadLinesOrigMetDataMax)
                  END IF
               END IF
            ELSE
               WRITE (*, *) 'Disaggregation code for rain not recognised'
            END IF
         ELSEIF (ii == 24) THEN !wind direction disaggregation not coded yet...
            IF (ANY(MetForDisagg(:, ii) /= -999)) THEN
               WRITE (*, *) 'Disaggregation of wind direction not currently implemented!'
            END IF
         ELSE
            IF (ALL(MetForDisagg(:, ii) == -999)) THEN
               !IF(DiagnoseDisagg==1) write(*,*) 'No data for col.', ii
               Met_tt(:, ii) = -999
            ELSE
               Met_tt(:, ii) = Disagg_Lin(MetForDisagg(:, ii), MetForDisaggPrev(ii), MetForDisaggNext(ii), MetDisaggMethod(ii), &
                                          NperTstepIn, ReadLinesOrigMetData, ReadLinesOrigMetDataMax, iBlock)
            END IF
         END IF
      END DO

      ! Adjust kdown disaggregation using zenith angle
      IF (KdownZen == 1) THEN
         IF (DiagnoseDisagg == 1) WRITE (*, *) 'Adjusting disaggregated kdown using zenith angle'
         Met_tt_kdownAdj(:) = Met_tt(:, 15)
         ! Translate location data from SurfaceChar to find solar angles
         lat = SurfaceChar(igrid, c_lat)
         lng = SurfaceChar(igrid, c_lng)
         timezone = SurfaceChar(igrid, c_tz)
         alt = SurfaceChar(igrid, c_Alt)
         ! Calculate dectime at downscaled time-step
         dectimeFast(:) = Met_tt(:, 2) + Met_tt(:, 3)/24.0 + Met_tt(:, 4)/(60.0*24.0)
         idectime = dectimeFast - halftimestep ! sun position at middle of timestep before
         DO i = 1, (ReadLinesOrigMetDataMax*NperTstepIn)
            CALL NARP_cal_SunPosition(Met_tt(i, 2), idectime(i), timezone, lat, lng, alt, azimuth, zenith_deg)
            ! If sun below horizon, set disaggregated kdown to zero
            IF (zenith_deg > 90) THEN
               !write(*,*) Met_tt(i,1:4)
               Met_tt_kdownAdj(i) = 0.0
            END IF
         END DO
         ! Redistribute kdown over each day
         DO i = 1, (ReadLinesOrigMetDataMax*NperTstepIn/nsd) ! Loop over each day
            Met_tt_kdownAdj((i - 1)*nsd + seq1nsd) = &
               Met_tt_kdownAdj((i - 1)*nsd + seq1nsd) &
               *SUM(Met_tt((i - 1)*nsd + seq1nsd, 15)) &
               /SUM(Met_tt_kdownAdj((i - 1)*nsd + seq1nsd))
         END DO
         ! Copy adjusted kdown back to Met_tt array
         Met_tt(:, 15) = Met_tt_kdownAdj(:)
      END IF

      ! Copy disaggregated data to MetForcingDataArray
      MetForcingData(:, 1:24, GridCounter) = Met_tt(:, 1:24)

      ! If snow is -999, set to zero (also in LUMPS_metRead.f95)
      IF (ALL(MetForcingData(:, 16, GridCounter) == -999)) MetForcingData(:, 16, GridCounter) = 0

      ! Undo pressure conversion again for writing out
      Met_tt(:, 13) = Met_tt(:, 13)/10.0

      ! Write out disaggregated file ------------------------------------------------------
      IF (KeepTstepFilesIn == 1) THEN
         IF (iBlock == 1) THEN
            ! Prepare header
            DO i = 1, ncolumnsMetForcingData
               IF (i == 1) THEN
                  HeaderMetOut = ADJUSTL(HeaderMet(i))
               ELSE
                  HeaderMetOut = TRIM(HeaderMetOut)//' '//ADJUSTL(HeaderMet(i))
               END IF
            END DO
            ! Write out header
            OPEN (78, file=TRIM(FileDscdMet), err=112)
            WRITE (78, '(a)') HeaderMetOut
         ELSE
            OPEN (78, file=TRIM(FileDscdMet), position='append') !,err=112)
         END IF
         ! Write out data
         DO i = 1, (ReadLinesOrigMetDataMax*NperTstepIn)
            WRITE (78, 303) (INT(Met_tt(i, ii)), ii=1, 4), Met_tt(i, 5:ncolumnsMetForcingData)
         END DO
         !IF(iBlock == ReadBlocksOrigMetData) THEN
         !   WRITE(78,'(i2)') -9
         !   WRITE(78,'(i2)') -9
         !ENDIF
         CLOSE (78) !Close output file
      END IF

303   FORMAT((i4, 1X), 3(i3, 1X), 9(f12.6, 1X), (f9.4, 1X), 10(f9.4, 1X)) !Allows 4 dp for rainfall

      ! Deallocate arrays -----------------------------------------------------------------
      DEALLOCATE (MetForDisagg)
      DEALLOCATE (MetForDisaggPrev)
      DEALLOCATE (MetForDisaggNext)

      RETURN

112   CALL ErrorHint(52, TRIM(FileDscdMet), notUsed, notUsed, notUsedI)

   END SUBROUTINE DisaggregateMet
   !======================================================================================

   !======================================================================================
   SUBROUTINE DisaggregateESTM(iBlock)
      ! Subroutine to disaggregate met forcing data to model time-step
      ! HCW 10 Feb 2017
      !======================================================================================

      USE Sues_Data
      USE DefaultNotUsed

      IMPLICIT NONE

      INTEGER :: lunit = 101
      INTEGER :: tdiff !Time difference (in minutes) between first and second rows of original met forcing file
      INTEGER :: i, ii !counter
      INTEGER :: iBlock
      INTEGER, DIMENSION(NperESTM) :: seq1NperESTM
      INTEGER, DIMENSION(nsd) :: seq1nsd
      INTEGER, DIMENSION(ncolsESTMdata) :: ESTMDisaggMethod ! Stores method to use for disaggregating met data
      REAL(KIND(1D0)), DIMENSION(ncolsESTMdata) :: ESTMArrayOrig
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrigESTMData*NperESTM, ncolsESTMdata) :: ESTM_tt
      CHARACTER(LEN=9), DIMENSION(ncolsESTMdata) :: HeaderESTM
      CHARACTER(LEN=10*ncolsESTMdata) :: HeaderESTMOut
      ! REAL(KIND(1d0)),DIMENSION(ReadLinesOrigESTMData):: dectimeOrig
      ! REAL(KIND(1d0)),DIMENSION(ReadLinesOrigESTMData*NperESTM):: dectimeDscd
      INTEGER :: iostat_var

      ! INTEGER, DIMENSION(NperESTM):: temp_iy, temp_id, temp_ih, temp_im, temp_ihm

      ! Allocate and initialise arrays to receive original forcing data --------------------
      ALLOCATE (ESTMForDisagg(ReadLinesOrigESTMData, ncolsESTMdata))
      ALLOCATE (ESTMForDisaggPrev(ncolsESTMdata))
      ALLOCATE (ESTMForDisaggNext(ncolsESTMdata))
      ESTMForDisagg(:, :) = -999
      ESTMForDisaggPrev(:) = -999
      ESTMForDisaggNext(:) = -999
      ! Initialise array to receive disaggregated data
      ESTM_tt = -999
      ! Intialise disaggregation method
      ESTMDisaggMethod = -999

      ! Generate useful sequences
      seq1NperESTM = (/(i, i=1, NperESTM, 1)/)
      seq1nsd = (/(i, i=1, nsd, 1)/)

      ! Get methods to use for disaggregation from RunControl
      ! (N.B.DisaggMethodESTM is set as 1 or 2 in RunControl; ESTMDisaggMethod is array of ncolsESTMdata used here)
      IF (DisaggMethodESTM == 1) THEN
         ESTMDisaggMethod(:) = 10 !linear disaggregation of averages
      ELSEIF (DisaggMethodESTM == 2) THEN
         ESTMDisaggMethod(:) = 20 !linear disaggregation of instantaneous values
      ELSE
         CALL errorHint(2, 'Problem in SUEWS_ESTMDisagg: DisaggMethodESTM value should be 1 or 2', &
                        NotUsed, NotUsed, DisaggMethodESTM)
      END IF

      ! Read data ---------------------------------------------------------------------
      IF (DiagnoseDisaggESTM == 1) WRITE (*, *) 'Reading file: ', TRIM(FileOrigESTM)
      OPEN (lunit, file=TRIM(FileOrigESTM), status='old')
      ! CALL skipHeader(lunit,SkipHeaderMet)  !Skip header -> read header instead
      READ (lunit, *) HeaderESTM
      !write(*,*) HeaderMet
      ! Skip over lines that have already been read and downscaled
      IF (SkippedLinesOrigESTM > 0) THEN
         DO i = 1, skippedLinesOrigESTM - 1 ! minus 1 here because last line of last block needs to be read again
            READ (lunit, *)
         END DO
         ! Read in last line of previous block
         READ (lunit, *, iostat=iostat_var) ESTMArrayOrig
         ESTMForDisaggPrev(1:ncolsESTMdata) = ESTMArrayOrig
      END IF
      ! Read in current block
      DO i = 1, ReadLinesOrigESTMDataMax
         READ (lunit, *, iostat=iostat_var) ESTMArrayOrig
         ESTMForDisagg(i, 1:ncolsESTMdata) = ESTMArrayOrig
      END DO
      ! Read in first line of next block (except for last block)
      IF (iBlock /= ReadBlocksOrigMetData) THEN
         READ (lunit, *, iostat=iostat_var) ESTMArrayOrig
         ESTMForDisaggNext(1:ncolsESTMdata) = ESTMArrayOrig
      END IF
      CLOSE (lunit)

      ! Check resolution of original met forcing data -------------------------------------
      ! Find time difference (in minutes) between first and second row
      tdiff = INT(ESTMForDisagg(2, 4) - ESTMForDisagg(1, 4)) !Try using minutes
      IF (tdiff == 0) tdiff = INT(ESTMForDisagg(2, 3) - ESTMForDisagg(1, 3))*60 !If no difference in minutes, try using hours
      IF (tdiff < 0) THEN !If time difference is negative (e.g. change of day), instead use second and third row
         tdiff = INT(ESTMForDisagg(3, 4) - ESTMForDisagg(2, 4))
         IF (tdiff == 0) tdiff = INT(ESTMForDisagg(3, 3) - ESTMForDisagg(2, 3))*60 !If no difference in minutes, try using hours
      END IF
      ! Check actual resolution matches specified input resolution
      IF (tdiff /= ResolutionFilesInESTM/60) THEN
         CALL errorHint(2, 'Problem in SUEWS_ESTMDisagg: timestamps in ESTM forcing file inconsistent with ResolutionFilesInESTM', &
                        REAL(ResolutionFilesInESTM, KIND(1D0)), NotUsed, tdiff*60)
      END IF

      ! Disaggregate time columns ---------------------------------------------------------
      IF (Diagnose == 1) THEN
         WRITE (*, *) 'Disaggregating ESTM forcing data (', TRIM(FileOrigESTM), ') to model time-step...'
      END IF
      CALL DisaggregateDateTime(ESTMForDisagg(:, 1:4), tstep, NperESTM, ReadLinesOrigMetDataMax, ESTM_tt(:, 1:4))
      ! ! Convert to dectime
      ! dectimeOrig = ESTMForDisagg(:,2) + ESTMForDisagg(:,3)/24.0 + ESTMForDisagg(:,4)/(60.0*24.0)
      !
      ! DO i=1,ReadLinesOrigESTMDataMax
      !    ! Downscale dectime using dectimeOrig(i) [becomes timestamp of last subinterval]
      !    dectimeDscd(NperESTM*(i-1)+Seq1NperESTM) = dectimeOrig(i) - (tstep/60.0)/(60.0*24.0)*(/(ii, ii=(NperESTM-1),0, -1)/)
      !    ! Convert to required formats
      !    temp_iy   = INT(ESTMForDisagg(i,1))   !Copy year
      !    temp_id   = FLOOR(dectimeDscd(NperESTM*(i-1)+Seq1NperESTM))   !DOY
      !    ! To avoid precision errors, round here
      !    !  - this should not be a problem as a difference of 1 = 1 min, so a difference of 0.001 << 1 min
      !    temp_ihm  = NINT(((dectimeDscd(NperESTM*(i-1)+Seq1NperESTM) - temp_id/1.0)*60.0*24.0)*1000.0)/1000   !Minutes of the day (1440 max)
      !    temp_ih = (temp_ihm-MOD(temp_ihm,60))/60   !Hours
      !    temp_im = MOD(temp_ihm,60)   !Minutes
      !
      !    IF(dectimeOrig(i) == 1.0000 .AND. i > 1) THEN   !If year changes and it is not the beginning of the dataset
      !       WRITE(*,*) 'Year change encountered: ', dectimeOrig(i), dectimeOrig(i-1)
      !       ! Re-downscale dectime using dectimeOrig(i-1)
      !       dectimeDscd(NperESTM*(i-1)+Seq1NperESTM) = dectimeOrig(i-1) + (tstep/60.0)/(60.0*24.0)*Seq1NperESTM
      !       ! Convert to required formats
      !       temp_iy   = INT(ESTMForDisagg(i,1))   !Copy year
      !       temp_id   = FLOOR(dectimeDscd(NperESTM*(i-1)+Seq1NperESTM))   !DOY
      !       temp_ihm  = NINT(((dectimeDscd(NperESTM*(i-1)+Seq1NperESTM) - temp_id/1.0)*60.0*24.0)*1000.0)/1000   !Mins of the day (1440 max)
      !       temp_ih = (temp_ihm-MOD(temp_ihm,60))/60   !Hours
      !       temp_im = MOD(temp_ihm,60)   !Minutes
      !       ! Adjust year and DOY to account for year change
      !       temp_iy(1:(NperESTM-1)) = temp_iy(1:(NperESTM-1)) - 1  !Subtract 1 from year for all except final timestamp
      !       temp_id(NperESTM) = 1  !Set day for final timestamp to 1
      !    ENDIF
      !
      !    !IF(i==1 .or. i== ReadlinesOrigESTMDataMax) THEN
      !    !   write(*,*) temp_iy
      !    !   write(*,*) temp_id
      !    !   !write(*,*) temp_ihm
      !    !   write(*,*) temp_ih
      !    !   write(*,*) temp_im
      !    !ENDIF
      !
      !    ! Copy to ESTM_tt array
      !    ESTM_tt(NperESTM*(i-1)+Seq1NperESTM,1) = temp_iy
      !    ESTM_tt(NperESTM*(i-1)+Seq1NperESTM,2) = temp_id
      !    ESTM_tt(NperESTM*(i-1)+Seq1NperESTM,3) = temp_ih
      !    ESTM_tt(NperESTM*(i-1)+Seq1NperESTM,4) = temp_im
      !
      ! ENDDO

      ! Disaggregate other columns --------------------------------------------------------
      ! All other columns are temperatures
      DO ii = 5, ncolsESTMdata
         IF (ALL(ESTMForDisagg(:, ii) == -999)) THEN
            !IF(DiagnoseDisaggESTM==1) write(*,*) 'No data for col.', ii
            ESTM_tt(:, ii) = -999
         ELSE
            ESTM_tt(:, ii) = Disagg_Lin(ESTMForDisagg(:, ii), ESTMForDisaggPrev(ii), ESTMForDisaggNext(ii), ESTMDisaggMethod(ii), &
                                        NperESTM, ReadLinesOrigESTMData, ReadLinesOrigESTMDataMax, iBlock)
         END IF
      END DO

      ! Copy disaggregated data to MetForcingDataArray
      ESTMForcingData(:, 1:ncolsESTMdata, GridCounter) = ESTM_tt(:, 1:ncolsESTMdata)

      ! Write out disaggregated file ------------------------------------------------------
      IF (KeepTstepFilesIn == 1) THEN
         IF (iBlock == 1) THEN
            ! Prepare header
            DO i = 1, ncolsESTMdata
               IF (i == 1) THEN
                  HeaderESTMOut = ADJUSTL(HeaderESTM(i))
               ELSE
                  HeaderESTMOut = TRIM(HeaderESTMOut)//' '//ADJUSTL(HeaderESTM(i))
               END IF
            END DO
            ! Write out header
            OPEN (78, file=TRIM(FileDscdESTM), err=113)
            WRITE (78, '(a)') HeaderESTMOut
         ELSE
            OPEN (78, file=TRIM(FileDscdESTM), position='append') !,err=113)
         END IF
         ! Write out data
         DO i = 1, (ReadLinesOrigESTMDataMax*NperESTM)
            WRITE (78, 304) (INT(ESTM_tt(i, ii)), ii=1, 4), ESTM_tt(i, 5:ncolsESTMdata)
         END DO
         !IF(iBlock == ReadBlocksOrigMetData) THEN
         !   WRITE(78,'(i2)') -9
         !   WRITE(78,'(i2)') -9
         !ENDIF
         CLOSE (78) !Close output file
      END IF

304   FORMAT((i4, 1X), 3(i3, 1X), 9(f9.4, 1X))

      ! Deallocate arrays -----------------------------------------------------------------
      DEALLOCATE (ESTMForDisagg)
      DEALLOCATE (ESTMForDisaggPrev)
      DEALLOCATE (ESTMForDisaggNext)

      RETURN

113   CALL ErrorHint(52, TRIM(FileDscdESTM), notUsed, notUsed, notUsedI)

   END SUBROUTINE DisaggregateESTM
   !======================================================================================

   !======================================================================================
   SUBROUTINE DisaggregateDateTime(DateTimeForDisagg, tstep, Nper, ReadLinesOrigMetDataMax, DateTimeDscd)
      USE module_util_datetime, ONLY: daysInYear
      IMPLICIT NONE
      INTEGER, INTENT(in) :: tstep, Nper, ReadLinesOrigMetDataMax
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrigMetData, 4), INTENT(in) :: DateTimeForDisagg
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrigMetData*Nper, 4), INTENT(out) :: DateTimeDscd

      REAL(KIND(1D0)), DIMENSION(ReadLinesOrigMetData) :: dectimeOrig
      REAL(KIND(1D0)), DIMENSION(Nper) :: temp_dectime !temorary varaibles for disaggragation
      INTEGER, DIMENSION(Nper) :: temp_iy, temp_id, temp_ih, temp_im, temp_ihm ! temorary varaibles for disaggragation
      INTEGER, DIMENSION(Nper) :: ndays_iy ! number of days in iy

      INTEGER :: i, ii
      INTEGER, DIMENSION(Nper) :: seq1Nper

      ! Generate useful sequences
      seq1Nper = (/(i, i=1, Nper, 1)/)
      ! Convert to dectime
      ! dectimeOrig = MetForDisagg(:,2) + MetForDisagg(:,3)/24.0 + MetForDisagg(:,4)/(60.0*24.0)
      ! correct to dectime(year_start)=0 and dectime(year_end)=days of year (i.e., 365 or 366 if leap year) ! TS 09 May 2018
      dectimeOrig = (DateTimeForDisagg(:, 2) - 1) + DateTimeForDisagg(:, 3)/24.0 + DateTimeForDisagg(:, 4)/(60.0*24.0)

      DO i = 1, ReadLinesOrigMetDataMax

         ! Downscale dectime using dectimeOrig(i) [becomes timestamp of last subinterval]
         temp_dectime = dectimeOrig(i) - (tstep/60.0)/(60.0*24.0)*(/(ii, ii=(Nper - 1), 0, -1)/)
         temp_dectime = NINT(temp_dectime*60*60*24)/(60*60*24*1.)

         ! Convert to required formats
         ! get year: year-1 if dectime <0, copy `year` from metforcing otherwise
         temp_iy = MERGE(INT(DateTimeForDisagg(i, 1)) - 1, INT(DateTimeForDisagg(i, 1)), temp_dectime < 0)

         ! get day of year:
         ndays_iy = daysInYear(temp_iy) ! get days of year for DOY correction when temp_dectime<0 during year-crossing
         temp_dectime = MERGE(temp_dectime + ndays_iy, temp_dectime, temp_dectime < 0) ! correct minus temp_dectime to positive values
         temp_id = FLOOR(temp_dectime) + 1 !DOY

         temp_ihm = NINT((temp_dectime + 1 - temp_id/1.0)*60.0*24.0) !Minutes of the day (1440 max)
         temp_ih = (temp_ihm - MOD(temp_ihm, 60))/60 !Hours
         temp_ih = MERGE(temp_ih, 0, mask=(temp_ih < 24))
         temp_im = MOD(temp_ihm, 60) !Minutes

         ! Copy to Met_tt array
         DateTimeDscd(Nper*(i - 1) + Seq1Nper, 1) = temp_iy
         DateTimeDscd(Nper*(i - 1) + Seq1Nper, 2) = temp_id
         DateTimeDscd(Nper*(i - 1) + Seq1Nper, 3) = temp_ih
         DateTimeDscd(Nper*(i - 1) + Seq1Nper, 4) = temp_im

      END DO

   END SUBROUTINE DisaggregateDateTime
   !======================================================================================

   ! Define functions here:
   FUNCTION Disagg_Lin(Slow, SlowPrev, SlowNext, DisaggType, Nper_loc, ReadLinesOrig_loc, ReadLinesOrigMax_loc, iBlock) RESULT(Fast)

      USE DefaultNotUsed
      USE module_ctrl_const_sues

      IMPLICIT NONE

      INTEGER :: DisaggType !Type of disaggregation: 10 for averaged variables; 20 for instantaneous variables
      INTEGER :: Nper_loc !Number of subintervals per interval (local Nper)
      INTEGER :: ReadLinesOrig_loc, ReadLinesOrigMax_loc !Number of lines to read in original file (local)
      INTEGER :: iBlock
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrig_loc*Nper_loc) :: Fast !Array to receive disaggregated data
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrig_loc) :: Slow !Array to disaggregate
      REAL(KIND(1D0)) :: SlowPrev, SlowNext
      INTEGER, DIMENSION(Nper_loc) :: FastRows !Group of rows that are filled with each iteration
      INTEGER, DIMENSION(FLOOR(Nper_loc/2.0)) :: FirstRows10 !Rows at the beginning that are not filled during iteration (for averages)
      INTEGER, DIMENSION(Nper_loc - FLOOR(Nper_loc/2.0)) :: LastRows10 !Rows at the end that are not filled during iteration
      INTEGER, DIMENSION(Nper_loc) :: FirstRows20 !Rows at the beginning that are not filled during iteration (for instantaneous)
      INTEGER, DIMENSION(Nper_loc) :: seq1Nper_loc !1 to Nper_loc
      INTEGER :: XNper_loc !XNper_loc = 2 for even Nper_loc; XNper_loc=1 for odd Nper_loc
      INTEGER :: i, ii !counters

      ! Calculate XNper_loc (differentiates between disaggregations with odd and even Nper_loc)
      IF (MOD(Nper_loc, 2) == 0) XNper_loc = 2
      IF (MOD(Nper_loc, 2) == 1) XNper_loc = 1

      seq1Nper_loc = (/(i, i=1, Nper_loc, 1)/)

      ! Setup counters for iteration
      IF (DisaggType == 10) THEN
         FastRows = FLOOR(Nper_loc/2.0) + seq1Nper_loc ! Rows to create at model time-step
         FirstRows10 = (/(i, i=1, (FastRows(1) - 1), 1)/) !For start of dataset
         LastRows10 = &
            (/(i, i=Nper_loc*(ReadLinesOrigMax_loc - 1 - 1) + FastRows(Nper_loc) + 1, &
               (ReadLinesOrigMax_loc*Nper_loc), 1)/) ! For end of dataset
      ELSEIF (DisaggType == 20) THEN
         FastRows = Nper_loc + seq1Nper_loc !Rows to create at model time-step
         FirstRows20 = (/(i, i=1, (FastRows(1) - 1), 1)/) !For start of dataset
      END IF

      ! Initialise fast array to -999
      Fast = -999
      ! Linearly disaggregate
      IF (DisaggType == 10) THEN !Averaged variables
         IF (DiagnoseDisagg == 1) WRITE (*, *) 'Linearly disaggregating averaged variable'
         DO i = 1, (ReadLinesOrigMax_loc - 1)
            Fast(Nper_loc*(i - 1) + FastRows) = Slow(i) - &
                                                (Slow(i + 1) - Slow(i))/(XNper_loc*Nper_loc) + &
                                                (Slow(i + 1) - Slow(i))/Nper_loc*(/(ii, ii=1, Nper_loc, 1)/)
         END DO

         ! For first few rows, use previous met block
         IF (iBlock == 1) THEN
            Fast(FirstRows10) = Fast(FastRows(1)) !Use repeat values at the start of the year
         ELSE
            Fast(FirstRows10) = SlowPrev - &
                                (Slow(1) - SlowPrev)/(XNper_loc*Nper_loc) + &
                                (Slow(1) - SlowPrev)/Nper_loc* &
                                (/(ii, ii=(Nper_loc - SIZE(FirstRows10) + 1), Nper_loc, 1)/)
         END IF
         ! For last few rows, use next met block
         IF (iBlock == ReadBlocksOrigMetData) THEN
            Fast(LastRows10) = Fast(Nper_loc*(ReadLinesOrigMax_loc - 1 - 1) + FastRows(Nper_loc)) !Use repeat values at the end of the year
         ELSE
            Fast(LastRows10) = Slow(ReadLinesOrigMax_loc) - &
                               (SlowNext - Slow(ReadLinesOrigMax_loc))/(XNper_loc*Nper_loc) + &
                               (SlowNext - Slow(ReadLinesOrigMax_loc))/Nper_loc* &
                               (/(ii, ii=1, SIZE(LastRows10), 1)/)
         END IF
      ELSEIF (DisaggType == 20) THEN !Instantaneous variables
         IF (DiagnoseDisagg == 1) WRITE (*, *) 'Linearly disaggregating instantaneous variable'
         DO i = 1, (ReadLinesOrigMax_loc - 1)
            Fast(Nper_loc*(i - 1) + FastRows) = (Slow(i) + &
                                                 (Slow(i + 1) - Slow(i))/Nper_loc*2*(seq1Nper_loc - 1) + &
                                                 Slow(i))/2
         END DO
         ! For first few rows, use previous met block
         IF (iBlock == 1) THEN
            Fast(FirstRows20) = Fast(FastRows(1)) !Use repeat values at the start of the year
         ELSE
            Fast(FirstRows20) = (SlowPrev + &
                                 (Slow(1) - SlowPrev)/Nper_loc*2* &
                                 ((/(ii, ii=(Nper_loc - SIZE(FirstRows20) + 1), Nper_loc, 1)/) - 1) + &
                                 SlowPrev)/2
         END IF
         !! Last few rows are already filled for the instantaneous value disaggregation
         !IF(iBlock==ReadBlocksOrigMetData) THEN
         !   Fast(LastRows20) = Fast(Nper_loc*(ReadLinesOrigMax_loc-1-1)+FastRows(Nper_loc))   !Use repeat values at the end of the year
         !ELSE
         !   Fast(LastRows20) = (Slow(ReadLinesOrigMax_loc) + &
         !                     (SlowNext-Slow(ReadLinesOrigMax_loc))/Nper_loc*2 * &
         !                        ((/(ii, ii=1,SIZE(LastRows20), 1)/)-1) + &
         !                       Slow(ReadLinesOrigMax_loc))/2
         !ENDIF
      END IF

      IF (ANY(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999)) THEN
         WRITE (*, *) 'Problem: -999s (', COUNT(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999), ') in disaggregated data.'
         CALL errorHint(13, 'Problem in SUEWS_MetDisagg: -999 values in disaggregated data.', NotUsed, NotUsed, NotUsedI)
      END IF

   END FUNCTION Disagg_Lin
   !======================================================================================

   !======================================================================================
   FUNCTION DisaggP_amongN(Slow, amongN, Nper_loc, ReadLinesOrig_loc, ReadLinesOrigMax_loc) RESULT(Fast)
      ! Subroutine to disaggregate precipitation by evenly distributing among N subintervals
      !  (i.e. equal intensity in N subintervals)
      ! See Ward et al. (in review), meanN, 0.5N or 0.25N approach
      ! HCW 10 Feb 2017
      !======================================================================================

      USE DefaultNotUsed
      USE module_ctrl_const_sues

      IMPLICIT NONE

      INTEGER :: amongN !Number of subintervals over which rain will be distributed
      INTEGER :: Nper_loc !Number of subintervals per interval (local Nper)
      INTEGER :: ReadLinesOrig_loc, ReadLinesOrigMax_loc !Number of lines to read in original file (local)
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrig_loc*Nper_loc) :: Fast !Array to receive disaggregated data
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrig_loc) :: Slow !Array to disaggregate
      INTEGER, DIMENSION(:), ALLOCATABLE :: Subintervals !Array of subintervals that contain rain
      INTEGER, DIMENSION(Nper_loc) :: seq1Nper_loc !1 to Nper_loc
      INTEGER :: i

      ! For each averaging period, get subintervals which will receive rain
      ALLOCATE (Subintervals(amongN))
      Subintervals(:) = -999

      seq1Nper_loc = (/(i, i=1, Nper_loc, 1)/)

      IF (DiagnoseDisagg == 1) WRITE (*, *) 'Distributing over ', amongN, ' subintervals for variable'
      ! If all subintervals are to contain rain, don't need to generate random numbers
      IF (amongN == Nper_loc) THEN
         Subintervals(:) = seq1Nper_loc
      END IF
      IF (amongN > Nper_loc) &
         CALL errorHint(2, 'Problem in SUEWS_MetDisagg: no. of rainy periods cannot exceed number of subintervals', &
                        REAL(Nper_loc, KIND(1D0)), NotUsed, amongN)

      ! Initialise fast array to -999
      Fast = -999
      DO i = 1, ReadLinesOrigMax_loc
         Fast(Nper_loc*(i - 1) + seq1Nper_loc) = 0 !Fill all subintervals with zeros initially
         IF (Slow(i) > 0) THEN !If there is some rainfall during this interval...
            IF (amongN < Nper_loc) THEN
               Subintervals(:) = -999
               Subintervals = RandomSamples(amongN, Nper_loc)
            END IF
            Fast(Nper_loc*(i - 1) + SubIntervals) = Slow(i)/amongN
         END IF
      END DO

      IF (ANY(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999)) THEN
         WRITE (*, *) 'Problem: -999s (', COUNT(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999), ') in disaggregated data'
         CALL errorHint(13, 'Problem in SUEWS_MetDisagg: -999 values in disaggregated data.', NotUsed, NotUsed, NotUsedI)
      END IF

   END FUNCTION DisaggP_amongN
   !======================================================================================

   !======================================================================================
   FUNCTION DisaggP_amongNMult(Slow, multupperI, multamongN, Nper_loc, ReadLinesOrig_loc, ReadLinesOrigMax_loc) RESULT(Fast)
      ! Subroutine to disaggregate precipitation by evenly distributing among N subintervals
      !  (i.e. equal intensity in N subintervals) for different intensity bins
      ! Based on analsysis by Wen Gu
      ! HCW 21 Apr 2017
      !======================================================================================

      USE DefaultNotUsed
      USE module_ctrl_const_sues

      IMPLICIT NONE

      REAL(KIND(1D0)), DIMENSION(5) :: multupperI !Upper bound of intensity bin
      INTEGER, DIMENSION(5) :: multamongN !Number of subintervals over which rain will be distributed (array)
      INTEGER :: thisamongN !Number of subintervals over which rain will be distributed
      INTEGER :: Nper_loc !Number of subintervals per interval (local Nper)
      INTEGER :: ReadLinesOrig_loc, ReadLinesOrigMax_loc !Number of lines to read in original file (local)
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrig_loc*Nper_loc) :: Fast !Array to receive disaggregated data
      REAL(KIND(1D0)), DIMENSION(ReadLinesOrig_loc) :: Slow !Array to disaggregate
      INTEGER, DIMENSION(:), ALLOCATABLE :: Subintervals !Array of subintervals that contain rain
      INTEGER, DIMENSION(Nper_loc) :: seq1Nper_loc !1 to Nper_loc
      INTEGER :: i

      seq1Nper_loc = (/(i, i=1, Nper_loc, 1)/)

      IF (DiagnoseDisagg == 1) WRITE (*, *) 'Distributing over variable subintervals depending on intensity for variable'

      ! Initialise fast array to -999
      Fast = -999
      DO i = 1, ReadLinesOrigMax_loc
         Fast(Nper_loc*(i - 1) + seq1Nper_loc) = 0 !Fill all subintervals with zeros initially
         IF (Slow(i) > 0) THEN !If there is some rainfall during this interval...
            !Use intensity in this interval to decide number of subintervals to fill with rain
            IF (Slow(i) <= multupperI(1)) THEN
               thisamongN = multamongN(1)
            ELSEIF (Slow(i) > multupperI(1) .AND. Slow(i) <= multupperI(2)) THEN
               thisamongN = multamongN(2)
            ELSEIF (Slow(i) > multupperI(2) .AND. Slow(i) <= multupperI(3)) THEN
               thisamongN = multamongN(3)
            ELSEIF (Slow(i) > multupperI(3) .AND. Slow(i) <= multupperI(4)) THEN
               thisamongN = multamongN(4)
            ELSEIF (Slow(i) > multupperI(4) .AND. Slow(i) <= multupperI(5)) THEN
               thisamongN = multamongN(5)
            ELSEIF (Slow(i) > multupperI(5)) THEN
               thisamongN = multamongN(5)
               CALL errorHint(4, 'Precip in met forcing file exceeds maxiumum MultRainAmongNUpperI', &
                              Slow(i), MultRainAmongNUpperI(5), notUsedI)
            END IF

            ! For each averaging period, get subintervals which will receive rain
            ALLOCATE (Subintervals(thisamongN))
            Subintervals(:) = -999

            IF (thisamongN > Nper_loc) CALL errorHint(2, 'Problem in SUEWS_MetDisagg: no. of rainy periods cannot exceed '// &
                                                      'number of subintervals', REAL(Nper_loc, KIND(1D0)), NotUsed, thisamongN)

            IF (thisamongN == Nper_loc) THEN ! If all subintervals are to contain rain, don't need to generate random numbers
               Subintervals(:) = seq1Nper_loc
            ELSEIF (thisamongN < Nper_loc) THEN
               Subintervals = RandomSamples(thisamongN, Nper_loc)
            END IF
            Fast(Nper_loc*(i - 1) + SubIntervals) = Slow(i)/thisamongN
            !write(*,*) Slow(i), thisamongN
            DEALLOCATE (Subintervals)
         END IF
      END DO

      IF (ANY(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999)) THEN
         WRITE (*, *) 'Problem: -999s (', COUNT(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999), ') in disaggregated data'
         CALL errorHint(13, 'Problem in SUEWS_MetDisagg: -999 values in disaggregated data.', NotUsed, NotUsed, NotUsedI)
      END IF

   END FUNCTION DisaggP_amongNMult
   !======================================================================================

   !======================================================================================
   FUNCTION RandomSamples(N, OutOf) RESULT(Samples)
      ! Generates N/OutOf random samples without repeats
      !   e.g. for N = 3 and OutOf = 12, a possibility for Samples = 7,3,11
      ! HCW 10 Feb 2017
      !======================================================================================

      IMPLICIT NONE

      INTEGER :: i !counter
      INTEGER :: N !number of samples to return
      INTEGER :: OutOf !number to sample from
      INTEGER :: X !next sample to be added
      REAL(KIND(1D0)) :: r !random number
      INTEGER, DIMENSION(:), ALLOCATABLE :: Samples !Array to receive random samples

      ! Allocate and initialise Samples
      ALLOCATE (Samples(N))
      Samples(:) = -999

      ! Generate random sample (no repeats)
      i = 0 !Set counter to zero initially
      DO WHILE (ANY(Samples == -999))
         CALL RANDOM_NUMBER(r)
         X = INT(r*OutOf) + 1
         !write(*,*) X
         !write(*,*) COUNT(Samples == X)
         IF (COUNT(Samples == X) == 0) THEN
            ! Only keep if this subinterval has not already been selected
            i = i + 1
            Samples(i) = X
         END IF
         !write(*,*) Samples
      END DO

   END FUNCTION RandomSamples
   !======================================================================================

END MODULE MetDisagg
!========================================================================================
