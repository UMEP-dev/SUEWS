MODULE module_ctrl_const_mathconst

   IMPLICIT NONE

   REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D0 ! circle circumference / diameter
   REAL(KIND(1D0)), PARAMETER :: DEG2RAD = pi/180.0D0 ! [rad degree-1]
   REAL(KIND(1D0)), PARAMETER :: RAD2DEG = 180.0D0/pi ! [degree rad-1]

END MODULE module_ctrl_const_mathconst

! Backward compatibility alias
MODULE MathConstants
   USE module_ctrl_const_mathconst
END MODULE MathConstants

!----------------------------------------------------------------------------------
MODULE module_ctrl_const_physconst

   IMPLICIT NONE

   REAL(KIND(1D0)), PARAMETER :: sec_per_hour = 3600.0D0, sec_per_day = 86400.0D0, min_per_day = 1440.0D0 ! SI time conversions
   REAL(KIND(1D0)), PARAMETER :: arcsec_per_deg = 3600.0D0 ! arcseconds per degree
   REAL(KIND(1D0)), PARAMETER :: cp_air_anohm = 1006.38D0 ! AnOHM air heat capacity [J kg-1 K-1]; differs from cp_air
   REAL(KIND(1D0)), PARAMETER :: lv_anohm = 2264705.0D0 ! AnOHM Lv [J kg-1]; differs from lv_bolton and lv_wetbulb
   REAL(KIND(1D0)), PARAMETER :: C2K = 273.15D0 ! SI Celsius-to-Kelvin offset [K]
   REAL(KIND(1D0)), PARAMETER :: SBConst = 5.670374419D-8 ! CODATA Stefan-Boltzmann constant [W m-2 K-4]
   REAL(KIND(1D0)), PARAMETER :: JtoumolPAR = 4.6D0 ! Convert PAR from W m-2 to umol m-2 s-1
   REAL(KIND(1D0)), PARAMETER :: KdntoPAR = 0.46D0 ! Conversion from Kdn to PAR, originally from Tsubo and Walker (2005), used in Bellucco et al. (2017)
   REAL(KIND(1D0)), PARAMETER :: lv_bolton = 2501000.0D0 ! Bolton (1980) Lv intercept [J kg-1]; empirical intercept, distinct from AnOHM and wet-bulb formulae
   REAL(KIND(1D0)), PARAMETER :: lv_wetbulb = 2500.25D0 ! Wet-bulb Lv intercept [kJ kg-1]; differs from lv_bolton after unit conversion
   REAL(KIND(1D0)), PARAMETER :: cp_air = 1005.0D0 ! specific heat capacity of dry air [J kg-1 K-1]; dry-air intercept in the meteorological formula
   REAL(KIND(1D0)), PARAMETER :: HPA_TO_PA = 100.0D0 ! SI hecto prefix: Pa per hPa
   REAL(KIND(1D0)), PARAMETER :: p_ref_pa_d0 = 101325.0D0 ! Standard atmosphere [Pa], SI defining value
   REAL(KIND(1D0)), PARAMETER :: eps_fp = 1.0D-12 !Epsilon for floating-point near-zero comparisons

END MODULE module_ctrl_const_physconst

! Backward compatibility alias
MODULE PhysConstants
   USE module_ctrl_const_physconst
END MODULE PhysConstants

!----------------------------------------------------------------------------------
MODULE module_ctrl_const_calendar

   IMPLICIT NONE

   ! Number of days in a week and 1-based weekday indices matching Day_Of_Week
   ! (suews_util_time.f95) and legacy SUEWS_Irrigation DayWat(1:7) ordering.
   INTEGER, PARAMETER :: NDAYS_WEEK = 7
   INTEGER, PARAMETER :: DOW_SUNDAY = 1
   INTEGER, PARAMETER :: DOW_MONDAY = 2
   INTEGER, PARAMETER :: DOW_TUESDAY = 3
   INTEGER, PARAMETER :: DOW_WEDNESDAY = 4
   INTEGER, PARAMETER :: DOW_THURSDAY = 5
   INTEGER, PARAMETER :: DOW_FRIDAY = 6
   INTEGER, PARAMETER :: DOW_SATURDAY = 7

   ! Day-type indices for two-day-type diurnal profiles (working day / weekend).
   INTEGER, PARAMETER :: DAYTYPE_WEEKDAY = 1
   INTEGER, PARAMETER :: DAYTYPE_WEEKEND = 2

END MODULE module_ctrl_const_calendar

! sg feb 2012 - added some comments
! sg feb 2012 - changed number of surfaces to allocatable array
! lj jun 2012 - snow part added
! HW, LJ Oct 2014 - fixes to the structure
! HCW 03 Mar 2015 - tidied
! HCW 03 Jul 2015 - added albedo max & min to SUEWS_NonVeg.txt (min not used), SUEWS_Veg.txt, SUEWS_Water.txt (min not used)
! LJ 06 Jul 2015 - changed alb_snow, albsnowmin and albsnowmax to SnowAlb, SnowAlbMin and SnowAlbMax (to be systematic with
!                   other variables). Similarly denssnow changed to SnowDens. cMDS_SnowAlb=29 added.
! HCW 10 Mar 2016 - variable vsmd added for soil moisture of vegetated surfaces
! TS 14 Mar 2016 - multiple addtions for AnOHM
! HCW 14 Jun 2016 - updated columns for ESTM and column names for AnOHM
! HCW 26 Aug 2016 - CO2 flux added
! HCW 16 Jan 2017 - WhereWhen module added to store info relating to grid and datetime
! MH 16 Jun 2017 - Added biogenic CO2 and edited Site_Select
! MH 21 Jun 2017 - Added anthropogenic CO2 and edited Site_Select

!==================================================================================================
MODULE module_ctrl_const_allocate

   USE module_ctrl_const_mathconst, ONLY: DEG2RAD, RAD2DEG

   IMPLICIT NONE

   ! ---- Surface types ---------------------------------------------------------------------------
   INTEGER, PARAMETER :: nsurf = 7 !Total number of surfaces
   INTEGER, PARAMETER :: NVegSurf = 3 !Number of surfaces that are vegetated
   INTEGER, PARAMETER :: ndepth = 5 !Number of depth levels for facets

   ! SPARTACUS related
   INTEGER, PARAMETER :: nspec = 1 !Number of spectral bands
   INTEGER, PARAMETER :: nsw = 1 !Number of shortwave bands
   INTEGER, PARAMETER :: nlw = 1 !Number of longwave bands
   INTEGER, PARAMETER :: ncol = 1 !Number of tiles used: SUEWS does not have multiple tiles so ncol=1

   INTEGER, PARAMETER :: PavSurf = 1, & !When all surfaces considered together (1-7)
                         BldgSurf = 2, &
                         ConifSurf = 3, &
                         DecidSurf = 4, &
                         GrassSurf = 5, & !New surface classes: Grass = 5th/7 surfaces
                         BSoilSurf = 6, & !New surface classes: Bare soil = 6th/7 surfaces
                         WaterSurf = 7, &
                         ExcessSurf = 8, & !Runoff or subsurface soil in WGWaterDist
                         ivConif = 1, & !When only vegetated surfaces considered (1-3)
                         ivDecid = 2, &
                         ivGrass = 3

   ! STEBBS Related
   INTEGER, PARAMETER :: nbtypes = 1 ! Number of building archetypes. TODO: Make dynamic

   ! ---- Set number of columns in output files ---------------------------------------------------
   INTEGER, PARAMETER :: ncolumnsDataOutSUEWS = 90 + 7 + 7 + 7 + 7, & !Main output file (_5.txt): base + Ts_surf + Ts_surf_dyohm + qn_surf + qs_surf
                         ncolumnsDataOutSnow = 103, &
                         ncolumnsdataOutSOLWEIG = 31, &
                         ncolumnsDataOutBEERS = 34, &
                         ncolumnsdataOutBL = 22, &
                         ncolumnsDataOutESTM = 5 + 27, &
                         ncolumnsDataOutEHC = 5 + 7*2 + 15*(1 + 4 + 2)*2 + 7, &
                         ncolumnsDataOutDailyState = 52, &
                         ncolumnsDataOutRSL = 30*4 + 5 + 13 + 2, &
                         ncolumnsDataOutDebug = 1 + 5 + 103 + 14 + 5 + 4 + 3 + 1, &
                         ncolumnsDataOutSPARTACUS = 5 + 8 + 8*15 + 3 + 6*15 + 2*15 + 4 + 1 + 2*15 + 2 + 8*15, &
                         ncolumnsDataOutSTEBBS = 5 + 82, &
                         ncolumnsDataOutNHood = 5 + 1

   ! ---- Define arrays at daily timestep ---------------------------------------------------------
   INTEGER, PARAMETER :: ndays = 366 !Max no. days in a year used to specify size of daily arrays

   ! SPARTACUS namelist input variables (SPARTACUS_Initialise)
   INTEGER :: n_vegetation_region_urban, n_stream_sw_urban, n_stream_lw_urban
   REAL(KIND(1D0)) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
                      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                      veg_fsd_const, veg_contact_fraction_const, &
                      ground_albedo_dir_mult_fact
   LOGICAL :: use_sw_direct_albedo

END MODULE module_ctrl_const_allocate

! Backward compatibility alias
MODULE allocateArray
   USE module_ctrl_const_allocate
END MODULE allocateArray
!==================================================================================================

MODULE module_ctrl_const_datain

   IMPLICIT NONE

   ! NOTE: Version is now managed automatically via get_ver_git.py from git tags (YYYY.M.D format)
   ! Old manual version string 'progname = SUEWS_V2021a' removed as unused (20251014)

   ! ---- Run information ------------------------------------------------------------------------
   CHARACTER(len=20) :: FileCode !Set in RunControl
   CHARACTER(len=150) :: FileInputPath, & !Filepath for input files (set in RunControl)
                         FileOutputPath !Filepath for output files (set in RunControl)
   ! ---- File names -----------------------------------------------------------------------------
   CHARACTER(len=150) :: FileOut, & !Output file name
                         FileChoices, & !Run characteristics file name
                         FileStateInit, & !Run characteristics file name
                         FileMet, & !Meteorological forcing file name
                         FileOrigMet, & !Original meteorological forcing file name (i.e. before downscaling)
                         FileOrigESTM, & !Original ESTM forcing file name (i.e. before downscaling)
                         FileDscdMet, & !Downscaled meteorological forcing file name
                         FileDscdESTM, & !Downscaled ESTM forcing file name
                         FileDaily, & !Daily State output file name
                         FileESTMTs, & !ESTM input file name
                         SOLWEIGpoiOut, & !SOLWEIG poi file name
                         BLout, & !CLB output file name
                         FileOut_tt, & !Output file name (for resolution at model time-step)
                         ESTMOut_tt

   INTEGER :: SkipHeaderSiteInfo = 2 !Number of header lines to skip in SiteInfo files
   INTEGER :: SkipHeaderMet = 1 !Number of header lines to skip in met forcing file

   ! ---- Model options set in RunControl --------------------------------------------------------
   INTEGER :: EmissionsMethod, & ! anthropogenic emissions method
              CBLuse, & !CBL slab model used (1) or not used (0)
              MultipleMetFiles, & !Indicates whether a single met file is used for all grids (0) or one for each grid (1)
              MultipleInitFiles, & !Indicates whether a single initial conditions file is used for all grids (0) or one for each grid (1)
              MultipleESTMFiles, & !Indicates whether a single ESTM input data file is used for all grids (0) or one for each grid (1)
              MultipleLayoutFiles, & !Indicates whether a single grid layout input data file is used for all grids (0) or one for each grid (1)
              KeepTstepFilesIn, & !Delete (0) or keep (1) input met files at resolution of tstep (used by python, not fortran)
              KeepTstepFilesOut, & !Delete (0) or keep (1) output files at resolution of tstep (used by python, not fortran)
              ResolutionFilesIn, & !Specify resolution of input file  [s]
              ResolutionFilesOut, & !Specify resolution of output file [s]
              ResolutionFilesInESTM, & !Specify resolution of ESTM input file  [s]
              WriteOutOption, & !Choose variables to include in main output file
              NetRadiationMethod, & !Options for net all-wave radiation calculation
              OHMIncQF, & !OHM calculation uses Q* only (0) or Q*+QF (1)
              StorageHeatMethod, & !Observed(0); OHM(1); AnOHM(3); ESTM(4); EHC(5); DyOHM(6); STEBBS(7); DyOHM-building flux only(8)
              SnowUse, & !Snow part used (1) or not used (0)
              !  SOLWEIGuse, &           !SOLWEIG part used (calculates Tmrt and other fluxes on a grid, FL)
              SMDMethod, & !Use modelled (0) or observed(1,2) soil moisture
              WaterUseMethod, & !Use modelled (0) or observed (1) water use
              RoughLenMomMethod, & !Defines method for calculating z0 & zd
              DisaggMethod, & ! Sets disaggregation method for original met forcing data
              DisaggMethodESTM, & ! Sets disaggregation method for original met forcing data
              RainDisaggMethod, & ! Sets disaggregation method for original met forcing data for rainfall
              RainAmongN, & ! Number of subintervals over which to disaggregate rainfall
              KdownZen, & ! Controls whether Kdown disaggregation uses zenith angle (1) or not (0)
              SuppressWarnings = 1, & ! Set to 1 to suppress warning output
              RSLMethod, & !Specify the approach for near surface diagnostic: 0, MOST; 1, RSL; 2, Auto (MOST+RSL)
              Diagnose, & !Set to 1 to get print-out of model progress
              DiagnoseDisagg, & !Set to 1 to get print-out of met forcing disaggregation progress
              !  ncMode, &               !Write output file in netCDF (1) or not (0) , TS, 09 Dec 2016
              !  nRow, &                 !number of rows of checker board layout in the netCDF output, TS, 09 Dec 2016
              !  nCol, &                 !number of columns of checker board layout in the netCDF output, TS, 09 Dec 2016
              DiagnoseDisaggESTM, & !Set to 1 to get print-out of ESTM forcing disaggregation progress
              DiagQN, DiagQS, & !Set to 1 to print values/components
              stebbsmethod !Stebbins

   ! For more complex downscaling allow different RainAmongN for different intensities
   INTEGER, DIMENSION(5) :: MultRainAmongN ! RainAmongN for each intensity bin
   REAL(KIND(1D0)), DIMENSION(5) :: MultRainAmongNUpperI ! Upper bound of intensity bin for which to apply MultRainAmongN

   ! ---- Model options currently set in model, but may be moved to RunControl at a later date
   INTEGER :: AlbedoChoice, & !No additional albedo varaition (0); zenith angle calculation (1)
              !Currently set to 0 in SUEWS_Initial
              InputMetFormat, & !Defines format for met input data: LUMPS format(1) or SUEWS format(10)
              !Currently set to 10 in SUEWS_Initial
              EvapMethod, & !Evaporation calculated according to Rutter (1) or Shuttleworth (2)
              !Currently set to 2 in OverallRunControl
              LAIcalcYes, & !Use observed (0) or modelled (1) LAI
              !Currently set to 1 in OverallRunControl
              WriteDailyState !Daily state file written (1)
   !Currently set to 1 in SUEWS_Initial

   ! ---- Other options used within model --------------------------------------------------------
   INTEGER :: ldown_option !Parameterisation used for downward longwave radiation (1/2/3)

   ! ---- Output file numbers --------------------------------------------------------------------
   INTEGER :: lfnout, & !Error Output write units
              lfnoutC, & !Clean output write units
              lfnOld !!Was used for GridConnections

   INTEGER :: OutputFormats !Used to control writing out of output file format

   ! ---- Other options set in RunControl --------------------------------------------------------
   REAL(KIND(1D0)) :: timezone !Timezone (GMT=0)

   ! ---- Variables in alphabetical order --------------------------------------------------------
   !! Add units
   REAL(KIND(1D0)) :: alpha_qhqe, & !Alpha parameter used in LUMPS QH and QE calculations [-]
                      alt, & !Altitude  [m]
                      ! avdens, &    !Average air density, moved to  by TS, 27 Aug 2019
                      avkdn, & !Average downwelling shortwave radiation
                      avrh, & !Average relative humidity
                      avts, & !Average surface temperature
                      avu1, & !Average wind speed
                      avU10_ms, & !Average wind speed at 10 m
                      azimuth, & !Sun azimuth in degrees
                      BaseT_HC, & !Base temperature for QF
                      BuildEnergyUse, & ! Building energy use
                      CO2mWD, & !Diurnal activity profile (weekday)
                      CO2mWE, & !Diurnal activity profile (weekend)
                      CO2PointSource, & !CO2 point source [kg C day-1]
                      E_mod, & !Modelled latent heat flux with LUMPS  [W m-2]
                      EF_umolCO2perJ, & !CO2 emission factor for fuels used for building heating [umol CO2 J-1]
                      emis_snow, & !Emissivity of snow
                      EnEF_v_Jkm, & !Heat release per vehicle per metre of travel [J km-1 veh-1]
                      EnProfWD, & !Diurnal energy use profile (weekday)
                      EnProfWE, & !Diurnal energy use profile (weekend)
                      Fc, & !CO2 flux [umol m-2 s-1]
                      Fc_anthro, & !CO2 flux (anthropogenic part) [umol m-2 s-1]
                      Fc_biogen, & !CO2 flux (biogenic part) [umol m-2 s-1]
                      Fc_build, & !CO2 flux (building energy use component) [umol m-2 s-1]
                      Fc_metab, & !CO2 flux (human metabolism component) [umol m-2 s-1]
                      Fc_photo, & !CO2 flux (photosynthesis component) [umol m-2 s-1]
                      Fc_point, & !CO2 flux (Point source component) [umol m-2 s-1]
                      Fc_respi, & !CO2 flux (non-human respiration component) [umol m-2 s-1]
                      Fc_traff, & !CO2 flux (traffic component) [umol m-2 s-1]
                      fcld, & !Cloud fraction modelled
                      fcld_obs, & !Cloud fraction observed
                      FrFossilFuel_Heat, & !Fraction of fossil fuels used for building heating relative to district heating
                      FrFossilFuel_NonHeat, & !Fraction of fossil fuels used for energy consumption relative to district heating
                      FrPDDwe, & !Fraction of weekend population to weekday population
                      h_mod, & !Modelled sensible heat flux with LUMPS [W m-2]
                      kclear, & !Theoretical downward shortwave radiation
                      kdiff, & !Diffuse shortwave radiation
                      kdir, & !Direct shortwave radiation
                      kup, & !Upward shortwave radiation
                      LAI_dectr, & !LAI for study area provided in met forcing file
                      LAI_evetr, & !LAI for study area provided in met forcing file
                      LAI_grass, & !LAI for study area provided in met forcing file
                      lat, & !Latitude
                      ldown, & !Downward longwave radiation
                      ldown_obs, & !Downwelling longwave radiation
                      lng, & !Longitude
                      lup, & !Upward longwave radiation
                      MaxFCMetab, & !Maximum (day) CO2 from human metabolism
                      MaxQFMetab, & !Maximum (day) anthropogenic heat from human metabolism
                      MinFCMetab, & !Minimum (night) CO2 from human metabolism
                      MinQFMetab, & !Minimum (night) anthropogenic heat from human metabolism
                      PopDensNighttime, & ! Nighttime population density [ha-1] (i.e. residents)
                      PopProfWD, & !Diurnal profile for population density (weekday)
                      PopProfWE, & !Diurnal profile for population density (weekend)
                      Precip, & !Precipitation per timestep [mm]
                      Precip_hr, & !Precipitation [mm hr-1]
                      Press_hPa, & !Station air pressure in hPa
                      Pres_kPa, & !Station air pressure in kPa
                      q2_gkg, & ! Specific humidity at 2 m
                      qe, & !Observed latent heat flux
                      qe_obs, &
                      QF_build, & !Anthropogenic heat flux from building [W m-2]
                      QF_metab, & !Anthropogenic heat flux from human metabolism [W m-2]
                      QF_traff, & !Anthropogenic heat flux from traffic [W m-2]
                      qf_obs, & !Observed anthropogenic heat flux
                      QF_SAHP, & !Anthropogenic heat flux calculated by SAHP
                      QF_SAHP_base, & !Anthropogenic heat flux calculated by SAHP (temp independent part)
                      QF_SAHP_heat, & !Anthropogenic heat flux calculated by SAHP (heating part only)
                      QF_SAHP_ac, & !AC contribution
                      qh, & !Observed sensible heat flux
                      qh_obs, &
                      QH_r, & !Sensible heat flux calculated using resistance method
                      qn1, & !Net all-wave radiation for the study area
                      qn1_bup, &
                      qn1_obs, & !Observed new all-wave radiation
                      qn1_S, & !Total net all-wave radiation for the SnowPack
                      qn1_snowfree, & !Total net all-wave radiation for the snowfree surface
                      qs_obs, & !Observed storage heat flux
                      QSanOHM, & !Simulated storage heat flux by AnOHM, TS 30 May 2016
                      QSestm, & !Simulated storage heat flux by ESTM, TS 30 May 2016
                      snow, & !snow cover
                      snowFrac_obs, & !Observed snow cover
                      Temp_C, & !Air temperature
                      t2_C, & !Air temperature at 2 m, TS 20 May 2017
                      TrafficUnits, & !Option for traffic units (1=[veh km m-2 day-1] 2=[veh km cap-1 day-1])
                      TraffProfWD, & !Diurnal traffic profile (weekday)
                      TraffProfWE, & !Diurnal traffic profile (weekend)
                      trans_site, & !Atmospheric transmissivity
                      tsurf, & !Surface temperature
                      wdir, & ! Wind direction
                      wu_m3, & !Water use provided in met forcing file [m3]
                      xsmd, & !Measured soil moisture deficit
                      year, & !Year of the measurements
                      zenith_deg !Sun zenith angle in degrees

   REAL(KIND(1D0)), DIMENSION(2) :: Qf_A, Qf_B, Qf_C, & !Qf coefficients
                                    AH_MIN, & !Base (temperature-independent) anthropogenic heat flux, daily mean, L11 methods
                                    AH_SLOPE_Heating, & !Slope of the antrhropogenic heat flux calculation (AnthropHeatMethod = 1)
                                    AH_SLOPE_Cooling, &
                                    FcEF_v_kgkm, &
                                    !   NumCapita, &
                                    PopDensDaytime, &
                                    BaseT_Heating, & !Critical temperature
                                    BaseT_Cooling, & !Critical cooling temperature
                                    TrafficRate, & !Traffic rate
                                    QF0_BEU

   ! INTEGER,DIMENSION(2)::DayLightSavingDay   !DOY when daylight saving changes
   INTEGER :: startDLS !DOY when daylight saving starts
   INTEGER :: endDLS !DOY when daylight saving ends

   INTEGER :: nCBLstep !number of time steps of Runge-kutta methods in one hour

   !---------Water bucket (see B. Offerle's PhD)----------------------------------
   REAL(KIND(1D0)) :: DRAINRT, & !Drainage rate of the water bucket [mm hr-1]
                      RAINBUCKET, & !RAINFALL RESERVOIR [mm]
                      RAINCOVER, &
                      RAINMAXRES, & !Maximum water bucket reservoir [mm]
                      RAINRES, & ! [mm]
                      TEMPVEG !TEMPORARY VEGETATIVE SURFACE FRACTION ADJUSTED BY RAINFALL

   !---------SOLWEIG variables---------------------------------------------------
   REAL(KIND(1D0)) :: absL, & ! Absorption coefficient of longwave radiation of a person
                      absK, & ! Absorption coefficient of shortwave radiation of a person
                      heightgravity, & ! Centre of gravity for a standing person
                      TransMin, & ! Tranmissivity of K through decidious vegetation (leaf on)
                      TransMax ! Tranmissivity of K through decidious vegetation (leaf off)

   INTEGER :: Posture, & ! 1.Standing, 2.Sitting
              usevegdem, & ! With vegetation (1)
              row, & ! Y coordinate for point of interest
              col, & ! X coordinate for point of interest
              onlyglobal, & ! if no diffuse and direct SW, then =1
              SOLWEIGpoi_out, & ! write output variables at point of interest
              Tmrt_out, & ! write output Tmrt grid
              Lup2d_out, & ! write output Lup grid
              Ldown2d_out, & ! write output Ldown grid
              Kup2d_out, & ! write output Kup grid
              Kdown2d_out, & ! write output Kdown grid
              GVF_out, & ! write output GroundViewFActor grid
              SOLWEIG_ldown, & ! 1= use SOLWEIG code to estimate Ldown, 0=use SEUWS
              OutInterval, & ! Output interval in minutes
              RunForGrid ! If only one grid should be run. All grids -999

   CHARACTER(len=150) :: DSMPath, & ! Path to DSMs
                         DSMname, & ! Ground and building DSM
                         CDSMname, & ! Canopy DSM
                         TDSMname, & ! Trunk zone DSM
                         SVFPath, & ! Path to SVFs
                         SVFsuffix, & !
                         buildingsname ! Boolean matrix for locations of building pixels

   !--------- AnOHM related variables----------------------------------
   ! to be added here

END MODULE module_ctrl_const_datain

! Backward compatibility alias
MODULE data_in
   USE module_ctrl_const_datain
END MODULE data_in
!==================================================================================================

!==================================================================================================
MODULE module_ctrl_const_default
   IMPLICIT NONE
   REAL(KIND(1D0)) :: notUsed = -55.55, reall, NAN = -999, pNAN = 999
   INTEGER :: notUsedI = -55, ios_out
END MODULE module_ctrl_const_default

! Backward compatibility alias
MODULE defaultNotUsed
   USE module_ctrl_const_default
END MODULE defaultNotUsed
!==================================================================================================

!===================================================================================
MODULE module_ctrl_const_gas
   IMPLICIT NONE
   REAL(KIND(1D0)) :: comp = 0.9995
   REAL(KIND(1D0)) :: epsil = 0.62197 !ratio molecular weight of water vapour/dry air (kg/mol/kg/mol)
   REAL(KIND(1D0)) :: epsil_gkg = 621.97 !ratio molecular weight of water vapour/dry air in g/kg
   REAL(KIND(1D0)) :: dry_gas = 8.31451 !Dry gas constant (J/k/mol)
   REAL(KIND(1D0)) :: gas_ct_wat = 461.05 !Gas constant for water (J/kg/K)
   REAL(KIND(1D0)) :: molar = 0.028965 !Dry air molar fraction in kg/mol
   REAL(KIND(1D0)) :: molar_wat_vap = 0.0180153 !Molar fraction of water vapour in kg/mol
   REAL(KIND(1D0)) :: gas_ct_dry = 8.31451/0.028965 !j/kg/k=dry_gas/molar
   REAL(KIND(1D0)) :: gas_ct_wv = 8.31451/0.0180153 !j/kg/kdry_gas/molar_wat_vap
END MODULE module_ctrl_const_gas

! Backward compatibility alias
MODULE gas
   USE module_ctrl_const_gas
END MODULE gas

MODULE module_ctrl_const_filename
   CHARACTER(len=90) :: smithfile !file for NARP
END MODULE module_ctrl_const_filename

! Backward compatibility alias
MODULE filename
   USE module_ctrl_const_filename
END MODULE filename

!----------------------------------------------------------------------------------

! Integer option codes shared with the Python schema and Rust bridge.
MODULE module_ctrl_const_options
   IMPLICIT NONE
   PRIVATE

   INTEGER, PARAMETER, PUBLIC :: SHM_OBSERVED = 0
   INTEGER, PARAMETER, PUBLIC :: SHM_OHM = 1
   INTEGER, PARAMETER, PUBLIC :: SHM_ANOHM = 3
   INTEGER, PARAMETER, PUBLIC :: SHM_ESTM = 4
   INTEGER, PARAMETER, PUBLIC :: SHM_EHC = 5
   INTEGER, PARAMETER, PUBLIC :: SHM_DYOHM = 6
   INTEGER, PARAMETER, PUBLIC :: SHM_STEBBS = 7
   INTEGER, PARAMETER, PUBLIC :: SHM_DYOHM_BUILDING = 8

   INTEGER, PARAMETER, PUBLIC :: OHM_QF_EXCLUDE = 0
   INTEGER, PARAMETER, PUBLIC :: OHM_QF_INCLUDE = 1

   INTEGER, PARAMETER, PUBLIC :: DIAG_OFF = 0
   INTEGER, PARAMETER, PUBLIC :: DIAG_ON = 1

   INTEGER, PARAMETER, PUBLIC :: STEBBS_NONE = 0
   INTEGER, PARAMETER, PUBLIC :: STEBBS_DEFAULT = 1
   INTEGER, PARAMETER, PUBLIC :: STEBBS_PROVIDED = 2

   INTEGER, PARAMETER, PUBLIC :: RC_DEFAULT = 0
   INTEGER, PARAMETER, PUBLIC :: RC_PROVIDED = 1
   INTEGER, PARAMETER, PUBLIC :: RC_PARAMETERISE = 2

   INTEGER, PARAMETER, PUBLIC :: SETPOINT_CONSTANT = 0
   INTEGER, PARAMETER, PUBLIC :: SETPOINT_DEPENDENT = 1
   INTEGER, PARAMETER, PUBLIC :: SETPOINT_SCHEDULED = 2
END MODULE module_ctrl_const_options
