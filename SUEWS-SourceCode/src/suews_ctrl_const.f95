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
MODULE allocateArray

   IMPLICIT NONE

   ! ---- Set parameters for reading in data ------------------------------------------------------
! #ifdef nc
!    INTEGER, PARAMETER:: MaxNumberOfGrids = 90000   !Max no. grids   !TS changed to 90000 for large-scale simulation based on netCDF IO
! #else
   INTEGER, PARAMETER :: MaxNumberOfGrids = 10000 !Max no. grids   !HCW changed to 2000 from 10000 so prog can run on windows (2GB lim)
! #endif
   INTEGER, PARAMETER :: MaxLinesMet = 8640 !Max no. lines to read in one go (for all grids, ie MaxLinesMet/NumberOfGrids each)

   ! ---- Surface types ---------------------------------------------------------------------------
   INTEGER, PARAMETER :: nsurf = 7 !Total number of surfaces
   INTEGER, PARAMETER :: NVegSurf = 3 !Number of surfaces that are vegetated
   INTEGER, PARAMETER :: nsurfIncSnow = nsurf + 1 !Number of surfaces + snow
   ! INTEGER, DIMENSION(MaxNumberOfGrids) :: nsurf_roof_grid !Number of extra roof facets (e.g., green roofs, etc.)
   ! INTEGER, DIMENSION(MaxNumberOfGrids) :: nsurf_wall_grid !Number of extra building facets (e.g., green walls, etc.)
   INTEGER, PARAMETER :: ndepth = 5 !Number of depth levels for facets
   INTEGER, PARAMETER :: nroof_max = 5 !max Number of allowed roof types
   INTEGER, PARAMETER :: nwall_max = 100 !max Number of allowed roof types

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
                         NSurfDoNotReceiveDrainage = 0, & !Number of surfaces that do not receive drainage water (green roof)
                         ivConif = 1, & !When only vegetated surfaces considered (1-3)
                         ivDecid = 2, &
                         ivGrass = 3

   REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf !Surface fractions [-]

   ! ---- Set number of columns in input files ----------------------------------------------------
   INTEGER, PARAMETER :: ncolumnsSiteSelect = 105 !SUEWS_SiteSelect.txt
   INTEGER, PARAMETER :: ncolumnsNonVeg = 24 !SUEWS_NonVeg.txt
   INTEGER, PARAMETER :: ncolumnsVeg = 38 !SUEWS_Veg.txt
   INTEGER, PARAMETER :: ncolumnsWater = 22 !SUEWS_Water.txt
   INTEGER, PARAMETER :: ncolumnsSnow = 25 !SUEWS_Snow.txt
   INTEGER, PARAMETER :: ncolumnsSoil = 9 !SUEWS_Soil.txt
   INTEGER, PARAMETER :: ncolumnsConductance = 13 !SUEWS_Conductance.txt
   INTEGER, PARAMETER :: ncolumnsOHMCoefficients = 4 !SUEWS_OHMCoefficients.txt
   INTEGER, PARAMETER :: ncolumnsESTMCoefficients = 52 !SUEWS_ESTMCoefficients.txt ! S.O. 04 Feb 2016
   INTEGER, PARAMETER :: ncolumnsAnthropogenic = 39 !SUEWS_AnthropogenicEmission.txt
   INTEGER, PARAMETER :: ncolumnsIrrigation = 26 !SUEWS_Irrigation.txt
   INTEGER, PARAMETER :: ncolumnsProfiles = 25 !SUEWS_Profiles.txt
   INTEGER, PARAMETER :: ncolumnsWGWaterDist = 10 !SUEWS_WithinGridWaterDist.txt
   INTEGER, PARAMETER :: ncolumnsBiogen = 9 !SUEWS_BiogenCO2.txt
   INTEGER, PARAMETER :: ncolumnsMetForcingData = 24 !Meteorological forcing file (_data.txt)
   INTEGER, PARAMETER :: ncolsESTMdata = 13 !ESTM input file (_ESTM_Ts_data.txt))

   ! ---- Set number of columns in output files ---------------------------------------------------
   INTEGER, PARAMETER :: ncolumnsDataOutSUEWS = 87, & !Main output file (_5.txt). dataOutSUEWS created in SUEWS_Calculations.f95
                         ncolumnsDataOutSnow = 103, &
                         ncolumnsdataOutSOLWEIG = 31, &
                         ncolumnsDataOutBEERS = 34, &
                         ncolumnsdataOutBL = 22, &
                         ncolumnsDataOutESTM = 5 + 27, &
                         ncolumnsDataOutESTMExt = 5 &
                         !  + nroof_max*ndepth & ! roof
                         !  + nwall_max*ndepth & ! wall
                         !  + nsurf*ndepth & ! surface
                         !  + nroof_max + nwall_max + nsurf & ! heat fluxes
                         + nsurf*2 + 2, & !bulk heat flux and surface temperature
                         ncolumnsDataOutDailyState = 50, &
                         ncolumnsDataOutRSL = 30*4 + 5 + 13 + 2, &
                         ncolumnsDataOutDebug = 5 + 24, &
                         ncolumnsDataOutSPARTACUS = 5 + 6 + 4*15 + 3 + 4*15 + 4

   ! ---- Define input file headers ---------------------------------------------------------------
   CHARACTER(len=20), DIMENSION(ncolumnsSiteSelect) :: HeaderSiteSelect_File !Header for SiteSelect.txt
   CHARACTER(len=20), DIMENSION(ncolumnsNonVeg) :: HeaderNonVeg_File !Header for the nonveg surface
   CHARACTER(len=20), DIMENSION(ncolumnsNonVeg) :: HeaderNonVeg_Reqd !Expected header for the nonveg surface
   CHARACTER(len=20), DIMENSION(ncolumnsVeg) :: HeaderVeg_File !Header for the veg surface
   CHARACTER(len=20), DIMENSION(ncolumnsVeg) :: HeaderVeg_Reqd !Expected header for the veg surface
   CHARACTER(len=20), DIMENSION(ncolumnsWater) :: HeaderWater_File !Header for water surface
   CHARACTER(len=20), DIMENSION(ncolumnsWater) :: HeaderWater_Reqd !Expected header for water surface
   CHARACTER(len=20), DIMENSION(ncolumnsSnow) :: HeaderSnow_File !Header for Snow surface
   CHARACTER(len=20), DIMENSION(ncolumnsSnow) :: HeaderSnow_Reqd !Expected header for Snow surface
   CHARACTER(len=20), DIMENSION(ncolumnsSoil) :: HeaderSoil_File !Header for soils
   CHARACTER(len=20), DIMENSION(ncolumnsSoil) :: HeaderSoil_Reqd !Expected header for soils
   CHARACTER(len=20), DIMENSION(ncolumnsConductance) :: HeaderCond_File !Header for conductances
   CHARACTER(len=20), DIMENSION(ncolumnsConductance) :: HeaderCond_Reqd !Expected header for conductances
   CHARACTER(len=20), DIMENSION(ncolumnsOHMCoefficients) :: HeaderOHMCoefficients_File !Header for soils
   CHARACTER(len=20), DIMENSION(ncolumnsOHMCoefficients) :: HeaderOHMCoefficients_Reqd !Expected header for soils
   CHARACTER(len=20), DIMENSION(ncolumnsESTMCoefficients) :: HeaderESTMCoefficients_File !Header for soils            ! S.O. 04 Feb 2016
   CHARACTER(len=20), DIMENSION(ncolumnsESTMCoefficients) :: HeaderESTMCoefficients_Reqd !Expected header for soils   ! S.O. 04 Feb 2016
   CHARACTER(len=20), DIMENSION(ncolumnsAnthropogenic) :: HeaderAnthropogenic_File !Header for QF
   CHARACTER(len=20), DIMENSION(ncolumnsAnthropogenic) :: HeaderAnthropogenic_Reqd !Expected header for QF
   CHARACTER(len=20), DIMENSION(ncolumnsIrrigation) :: HeaderIrrigation_File !Header for Irrigation
   CHARACTER(len=20), DIMENSION(ncolumnsIrrigation) :: HeaderIrrigation_Reqd !Expected header for Irrigation
   CHARACTER(len=20), DIMENSION(ncolumnsProfiles) :: HeaderProfiles_File !Header for Profiles
   CHARACTER(len=20), DIMENSION(ncolumnsProfiles) :: HeaderProfiles_Reqd !Expected header for Profiles
   CHARACTER(len=20), DIMENSION(ncolumnsWGWaterDist) :: HeaderWGWaterDist_File !Header for Profiles
   CHARACTER(len=20), DIMENSION(ncolumnsWGWaterDist) :: HeaderWGWaterDist_Reqd !Expected header for Profiles
   CHARACTER(len=20), DIMENSION(ncolumnsBiogen) :: HeaderBiogen_File !Header for Biogen
   CHARACTER(len=20), DIMENSION(ncolumnsBiogen) :: HeaderBiogen_Reqd !Expected header for Biogen

   ! ---- Define output file headers --------------------------------------------------------------
   INTEGER, DIMENSION(:), ALLOCATABLE :: UseColumnsDataOut !Column numbers used to select output variables
   ! If change lengths in SUEWS_Output.f95, also need to adjust here
   CHARACTER(len=14*ncolumnsDataOutSUEWS) :: HeaderUse, FormatUse, HeaderUseNoSep, FormatUseNoSep !Header and format in correct form
   CHARACTER(len=52*ncolumnsDataOutSUEWS) :: LongNmUse
   CHARACTER(len=14*ncolumnsDataOutSUEWS) :: UnitsUse
   CHARACTER(len=3*ncolumnsDataOutSUEWS) :: AggregUse
   CHARACTER(len=4*ncolumnsDataOutSUEWS) :: ColNosUse

   ! ---- Define arrays to store input information from SiteInfo spreadsheet ----------------------
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: SiteSelect !Stores info from SiteSelect.txt
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: NonVeg_Coeff !Coefficients for the nonveg surfaces
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Veg_Coeff !Coefficients for the veg surfaces
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Water_Coeff !Coefficients for the water surface
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Snow_Coeff !Coefficients for snow
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Soil_Coeff !Coefficients for soil
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Conductance_Coeff !Coefficients for conductances
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: OHMCoefficients_Coeff !Coefficients for OHMCoefficients
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: ESTMCoefficients_Coeff !Coefficients for ESTMCoefficients   ! S.O. 04 Feb 2016
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Anthropogenic_Coeff !Coefficients for AnthropogenicEmissions
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Irrigation_Coeff !Coefficients for Irrigation
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Profiles_Coeff !Coefficients for Profiles
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: WGWaterDist_Coeff !Coefficients for WithinGridWaterDist
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: Biogen_Coeff !Coefficients for BiogenCO2

   ! ---- Define arrays for model calculations ----------------------------------------------------
   INTEGER, DIMENSION(:), ALLOCATABLE :: GridIDmatrix !Array containing GridIDs in SiteSelect after sorting
   INTEGER, DIMENSION(:), ALLOCATABLE :: GridIDmatrix0 !Array containing GridIDs in SiteSelect in the original order
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: SurfaceChar !Array for surface characteristics
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: MetForcingData !Array for meteorological forcing data
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: MetForcingData_grid !Array for meteorological forcing data of one grid used by AnOHM
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: ESTMForcingData !Array for ESTM forcing data
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: ModelDailyState !DailyState array
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: DailyStateFirstOpen
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: ModelOutputData !Output data matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutSUEWS !Main data output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutRSL !Main data output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutBL !CBL output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutSOLWEIG !SOLWEIG POI output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutBEERS ! BEERS output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutDebug !debugging info matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutSPARTACUS !SPARTACUS output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutSnow !Main data output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutESTM !ESTM output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutESTMExt !ESTM output matrix
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dataOutDailyState !DailyState output array

   ! -------- output per each timestep ----------------------------------------------------------------
   REAL(KIND(1D0)), DIMENSION(5) :: datetimeLine ! output of datetime info per each timestep
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSUEWS - 5) :: dataOutLineSUEWS ! output of SUEWS results per each timestep
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow - 5) :: dataOutLineSnow ! output of snow results per each timestep
   ! REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5)      ::dataOutLineRSL  ! output of snow results per each timestep
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutRSL - 5 + 12) :: dataOutLineRSL ! output of snow results per each timestep
   REAL(KIND(1D0)), DIMENSION(ncolumnsdataOutSOLWEIG - 5) :: dataOutLineSOLWEIG ! output of snow results per each timestep
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutBEERS - 5) :: dataOutLineBEERS ! output of snow results per each timestep
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDebug) :: dataOutLineDebug ! output line for debugging info
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSPARTACUS) :: dataOutLineSPARTACUS ! output line for SPARTACUS per each timestep (1hr?)
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5) :: DailyStateLine ! output of DailyState results per each timestep

   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: MetForDisagg !Array for original met forcing data (for disaggregation)
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: MetForDisaggPrev, MetForDisaggNext !Stores last and next row of met data

   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: ESTMForDisagg !Array for original ESTM forcing data (for disaggregation)
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: ESTMForDisaggPrev, ESTMForDisaggNext !Stores last and next row of ESTM data

   ! ---- Define array for hourly profiles interpolated to tstep ----------------------------------
   ! REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: TstepProfiles
   ! REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: AHProf_tstep
   ! REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: WUProfM_tstep
   ! REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: WUProfA_tstep
   ! REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: HumActivity_tstep
   ! REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: TraffProf_tstep
   ! REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: PopProf_tstep

   REAL(KIND(1D0)), DIMENSION(0:23, 2) :: AHProf_24hr !Anthropogenic heat profiles for (1)weekdays / (2)weekends
   REAL(KIND(1D0)), DIMENSION(0:23, 2) :: HumActivity_24hr !Human actvity profiles for (1)weekdays / (2)weekends
   REAL(KIND(1D0)), DIMENSION(0:23, 2) :: TraffProf_24hr !Traffic profiles for (1)weekdays / (2)weekends
   REAL(KIND(1D0)), DIMENSION(0:23, 2) :: PopProf_24hr !Population profiles for (1)weekdays / (2)weekends
   REAL(KIND(1D0)), DIMENSION(0:23, 2) :: WUProfM_24Hr !Hourly profiles for water use (manual irrigation)
   REAL(KIND(1D0)), DIMENSION(0:23, 2) :: WUProfA_24Hr !Hourly profiles for water use (automatic irrigation)

   ! ---- For ESTM
   REAL(KIND(1D0)), ALLOCATABLE, DIMENSION(:, :) :: Ts5mindata !surface temperature input data
   REAL(KIND(1D0)), ALLOCATABLE, DIMENSION(:) :: ts5mindata_ir !=ts5mindata(ir,:), ts input for the current timestep
   REAL(KIND(1D0)), ALLOCATABLE, DIMENSION(:) :: Tair24HR
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTM - 5) :: dataOutLineESTM !ESTM output for the current timestep and grid

   ! ---- For ESTM_ext
   REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutESTMExt - 5) :: dataOutLineESTMExt !ESTM output for the current timestep and grid

   ! Column numbers for TstepProfiles
   INTEGER :: cTP_EnUseWD = 1, &
              cTP_EnUseWE = 2, &
              cTP_WUManuWD = 3, &
              cTP_WUManuWE = 4, &
              cTP_WUAutoWD = 5, &
              cTP_WUAutoWE = 6, &
              cTP_SnowCWD = 7, &
              cTP_SnowCWE = 8, &
              cTP_HumActivityWD = 9, &
              cTP_HumActivityWE = 10, &
              cTP_TraffProfWD = 11, &
              cTP_TraffProfWE = 12, &
              cTP_PopProfWD = 13, &
              cTP_PopProfWE = 14
   !-----------------------------------------------------------------------------------------------

   ! ---- Water balance for each surface  ---------------------------------------------------------
   !These variables are expressed as depths [mm] over each surface(is); the depth therefore varies with sfr_surf(is)
   REAL(KIND(1D0)), DIMENSION(nsurf) :: AddWater !Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: AddWaterRunoff !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
   ! N.B. this is not an amount; drain(is)*AddWaterRunoff(is) is the amount [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: drain !Drainage of each surface type [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: evap !Evaporation from each surface type [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff !Runoff from each surface type [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: runoffSoil !Soil runoff from each soil sub-surface [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: smd_nsurf !Soil moisture deficit of each sub-surface [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: smd_nsurfOut !Soil moisture deficit of each sub-surface (written out) [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id !Soil moisture of each surface type [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstoreOld !Soil moisture of each surface type from previous timestep [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id !Wetness status of each surface type [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: stateOut !Wetness status of each surface type (written out) [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: stateOld !Wetness status of each surface type from previous timestep [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: rss_nsurf !Surface resistance after wet/partially wet adjustment for each surface

   REAL(KIND(1D0)), DIMENSION(nsurf) :: WetThresh !When state_id > WetThresh, RS=0 limit in SUEWS_evap [mm] (specified in input files)
   REAL(KIND(1D0)), DIMENSION(nsurf) :: StateLimit !Limit for state_id of each surface type [mm] (specified in input files)

   REAL(KIND(1D0)), DIMENSION(1) :: WaterDepth !Depth of open water

   ! ---- Soil characteristics specified in input files -------------------------------------------
   REAL(KIND(1D0)), DIMENSION(nsurf) :: SatHydraulicConduct !Saturated hydraulic conductivity for each soil subsurface [mm s-1]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilDepth !Depth of sub-surface soil store for each surface [mm]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilStoreCap !Capacity of soil store for each surface [mm]

   ! ---- Within-grid water distribution matrix ---------------------------------------------------
   REAL(KIND(1D0)), DIMENSION(nsurf + 1, nsurf - 1) :: WaterDist !Within-grid water distribution to other surfaces and runoff/soil store [-]

   ! ---- Drainage characteristics ----------------------------------------------------------------
   REAL(KIND(1D0)), DIMENSION(6, nsurf) :: StoreDrainPrm !Storage capacities and drainage equation info for each surface
   ! 1 - min storage capacity [mm]
   ! 2 - Drainage equation to use
   ! 3 - Drainage coeff 1 [units depend on choice of eqn]
   ! 4 - Drainage coeff 2 [units depend on choice of eqn]
   ! 5 - max storage capacity [mm]
   ! 6 - current storage capacity [mm]
   !-----------------------------------------------------------------------------------------------

   ! ---- Define arrays at daily timestep ---------------------------------------------------------
   INTEGER, PARAMETER :: ndays = 366 !Max no. days in a year used to specify size of daily arrays
   !! Could delete NDays and allocate these elsewhere once no. days is known
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays, 5):: GDD          !Growing Degree Days (see SUEWS_DailyState.f95)
   ! REAL(KIND(1d0)),DIMENSION(-4:ndays, 6):: HDD          !Heating Degree Days (see SUEWS_DailyState.f95)
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays, 9):: WUDay       !Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)
   ! REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf):: LAI   !LAI for each veg surface [m2 m-2]

   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDD_id, GDD_id_prev !Growing Degree Days (see SUEWS_DailyState.f95)
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDD_id !Growing Degree Days (see SUEWS_DailyState.f95)
   REAL(KIND(1D0)) :: Tmin_id, Tmax_id, lenDay_id
   REAL(KIND(1D0)), DIMENSION(12) :: HDD_id
   REAL(KIND(1D0)), DIMENSION(9) :: WUDay_id, WUDay_id_prev !Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAI_id, LAI_id_prev !LAI for each veg surface [m2 m-2]

   ! Seasonality of deciduous trees accounted for by the following variables which change with time
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays):: DecidCap   !Storage capacity of deciduous trees [mm]
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays):: porosity   !Porosity of deciduous trees [-]
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays):: albDecTr     !Albedo of deciduous trees [-]
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays):: albEveTr     !Albedo of evergreen trees [-]
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays):: albGrass   !Albedo of grass[-]

   REAL(KIND(1D0)) :: AlbMin_DecTr, & !Min albedo for deciduous trees [-]
                      AlbMax_DecTr, & !Max albedo for deciduous trees [-]
                      CapMin_dec, & !Min storage capacity for deciduous trees [mm] (from input information)
                      CapMax_dec, & !Max storage capacity for deciduous trees [mm] (from input information)
                      PorMin_dec, & !Min porosity for deciduous trees
                      PorMax_dec, & !Max porosity for deciduous trees
                      AlbMin_EveTr, & !Min albedo for evergreen trees [-]
                      AlbMax_EveTr, & !Max albedo for evergreen trees [-]
                      AlbMin_Grass, & !Min albedo for grass [-]
                      AlbMax_Grass !Max albedo for grass [-]

   ! Replicate arrays needed for DailyState, adding dimension to identify the grid, HCW 27 Nov 2014
   !! Could delete MaxNumberOfGrids and allocate these elsewhere once NumberOfGrids is known
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays, 5,MaxNumberOfGrids):: GDD_grids
   ! REAL(KIND(1d0)),DIMENSION(-4:ndays, 6,MaxNumberOfGrids):: HDD_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays, 9,MaxNumberOfGrids):: WUDay_grids
   ! REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf,MaxNumberOfGrids):: LAI_grids

   REAL(KIND(1D0)), DIMENSION(nvegsurf, MaxNumberOfGrids) :: GDD_id_grids
   REAL(KIND(1D0)), DIMENSION(nvegsurf, MaxNumberOfGrids) :: SDD_id_grids
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: Tmin_id_grids
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: Tmax_id_grids
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: lenDay_id_grids
   REAL(KIND(1D0)), DIMENSION(12, MaxNumberOfGrids) :: HDD_id_grids
   REAL(KIND(1D0)), DIMENSION(9, MaxNumberOfGrids) :: WUDay_id_grids
   REAL(KIND(1D0)), DIMENSION(nvegsurf, MaxNumberOfGrids) :: LAI_id_grids

   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: albDecTr_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: DecidCap_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: porosity_grids
   !
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: albEveTr_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: albGrass_grids

   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: DecidCap_id_grids
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: albDecTr_id_grids
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: albEveTr_id_grids
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: albGrass_id_grids
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: porosity_id_grids

   REAL(KIND(1D0)) :: DecidCap_id
   REAL(KIND(1D0)) :: albDecTr_id
   REAL(KIND(1D0)) :: albEveTr_id
   REAL(KIND(1D0)) :: albGrass_id
   REAL(KIND(1D0)) :: porosity_id

   ! AnOHM related: added by TS 01 Mar 2016
   ! store AnOHM coef. of all sfc. by TS 09 Apr 2016
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: Bo_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: mAH_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: a1AnOHM_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: a2AnOHM_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: a3AnOHM_grids
   ! REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids,nsurf,3):: a123AnOHM_gs
   REAL(KIND(1D0)) :: xBo ! daily Bowen ratio
   !! store water states for AnOHM iteration, by TS 13 Apr 2016
   !REAL(KIND(1d0)),DIMENSION(0:ndays,MaxNumberOfGrids,nsurf):: soilmoistDay   !Soil moisture of each surface type at the end of a day [mm], 13 Apr 2016 TS
   !REAL(KIND(1d0)),DIMENSION(0:ndays,MaxNumberOfGrids,nsurf):: stateDay       !Wetness status of each existing surface type at the end of a day [mm], 13 Apr 2016 TS

   ! Day of week, month and season (used for water use and energy use calculations, and in OHM)
   ! INTEGER,DIMENSION(0:ndays,3)::DayofWeek   !1 - day of week; 2 - month; 3 - season
   !-----------------------------------------------------------------------------------------------

   ! --- Vegetation phenology ---------------------------------------------------------------------
   ! Parameters provided in input information for each vegetation surface (SUEWS_Veg.txt)
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: BaseT !Base temperature for growing degree days [degC]
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: BaseTe !Base temperature for senescence degree days [degC]
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDDFull !Growing degree days needed for full capacity [degC]
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDDFull !Senescence degree days needed to initiate leaf off [degC]
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAIMin !Min LAI [m2 m-2]
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAIMax !Max LAI  [m2 m-2]
   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: MaxConductance !Max conductance [mm s-1]
   REAL(KIND(1D0)), DIMENSION(4, nvegsurf) :: LAIPower !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off
   !! N.B. currently DecTr only, although input provided for all veg types
   INTEGER, DIMENSION(nvegsurf) :: LAIType !LAI equation to use: original (0) or new (1)
   !real(kind(1d0))::GDDmax,SDDMax                        ! Max GDD and SDD across all veg types [degC] (removed HCW 03 Mar 2015)

   REAL(KIND(1D0)), DIMENSION(nvegsurf) :: BiogenCO2Code, & !Biogenic CO2 Code for SUEWS_BiogenCO2.txt
                                           alpha_bioCO2, &
                                           beta_bioCO2, &
                                           theta_bioCO2, &
                                           alpha_enh_bioCO2, &
                                           beta_enh_bioCO2, &
                                           resp_a, &
                                           resp_b, &
                                           min_res_bioCO2

   !No longer used (removed HCW 27 Nov 2014)
   !real(kind(1d0)),dimension(0:23)::runT           ! running average T for the day
   !real(kind(1d0)),dimension(0:23)::runP           ! running total Precip for the day
   !real (kind(1d0))::avT_h, totP_h                 ! daily running average Temp, Total precip
   !-----------------------------------------------------------------------------------------------

   ! ---- Variables related to NARP ---------------------------------------------------------------
   REAL(KIND(1D0)), DIMENSION(nsurf) :: alb !Albedo of each surface type [-]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: emis !Emissivity of each surface type [-]

   REAL(KIND(1D0)) :: bulkalbedo !Bulk albedo for whole surface (areally-weighted)

   ! Radiation balance components for different surfaces
   REAL(KIND(1D0)), DIMENSION(nsurf) :: Tsurf_ind, & !Surface temperature for each surface [degC]
                                        Tsurf_ind_snow, & !Snow surface temperature for each surface [degC]
                                        Tsurf_ind_nosnow
   REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind, & !Outgoing shortwave radiation for each surface [W m-2]
                                        kup_ind_snow, & !Outgoing shortwave radiation for each snow surface [W m-2]
                                        kup_ind_nosnow
   REAL(KIND(1D0)), DIMENSION(nsurf) :: lup_ind, & !Outgoing longwave radiation for each surface [W m-2]
                                        lup_ind_snow, & !Outgoing longwave radiation for each snow surface [W m-2]
                                        lup_ind_nosnow
   REAL(KIND(1D0)), DIMENSION(nsurf) :: qn1_ind, & !Net all-wave radiation for each surface [W m-2]
                                        qn1_ind_snow, & !Net all-wave radiation for each snow surface [W m-2]
                                        qn1_ind_nosnow

   ! ---- NARP-specific parameters ----------------------------------------------------------------
   REAL(KIND(1D0)) :: NARP_LAT, NARP_LONG, NARP_YEAR, NARP_TZ, &
                      NARP_ALB_SNOW, NARP_EMIS_SNOW, NARP_TRANS_SITE
   REAL(KIND(1D0)) :: NARP_G(365) !!QUESTION: Should this be NDays? - HCW
   INTEGER :: NARP_NPERHOUR
   REAL(KIND(1D0)), ALLOCATABLE :: NARP_KDOWN_HR(:)
   ! Constants required
   REAL(KIND(1D0)), PARAMETER :: DEG2RAD = 0.017453292, &
                                 RAD2DEG = 57.29577951, &
                                 SIGMA_SB = 5.67E-8
   !-----------------------------------------------------------------------------------------------

   ! ---- OHM coefficients ------------------------------------------------------------------------
   REAL(KIND(1D0)), DIMENSION(nsurf + 1, 4, 3) :: OHM_coef !Array for OHM coefficients
   REAL(KIND(1D0)), DIMENSION(nsurf + 1) :: OHM_threshSW, OHM_threshWD !Arrays for OHM thresholds
   REAL(KIND(1D0)) :: a1, a2, a3 !OHM coefficients, a1 [-]; a2 [h]; a3 [W m-2]
   ! REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE:: qn1_store, qn1_S_store   !Q* values for each timestep over previous hr (_S for snow)
   ! REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE:: qn1_av_store, qn1_S_av_store  !Hourly Q* values for each timestep over previous 2 hr
   ! REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::qn1_store_grid,qn1_av_store_grid
   ! REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::qn1_S_store_grid,qn1_S_av_store_grid

   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tair_av_grids
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qn1_av_grids, qn1_s_av_grids
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: dqndt_grids, dqnsdt_grids
   REAL(KIND(1D0)) :: qn1_av, dqndt
   REAL(KIND(1D0)) :: tair_av
   REAL(KIND(1D0)) :: qn1_s_av, dqnsdt

   !-----------------------------------------------------------------------------------------------

   ! ---- Snow-related variables ------------------------------------------------------------------
   REAL(KIND(1D0)), DIMENSION(nsurf) :: changSnow, & !Change in snowpack in mm
                                        maxSnowVol, & !! Maximum snow volume
                                        SnowWater, & !!Liquid water in the snow pack of ith surface
                                        ev_snow, & !!Evaporation from snowpack in mm
                                        mw_ind, & !Melt water from individual surface in mm
                                        mw_indDay, & !!Melt water per day from each surface type in m3
                                        runoffSnow, & !!Runoff from snowpack in mm and in m3
                                        SnowDens, & !Density of snow
                                        SnowFrac, & !!Surface fraction of snow cover
                                        iceFrac, &
                                        snowInit, &
                                        snowDepth, & !Depth of snow in cm
                                        SnowToSurf, & !Meltwater flowing from snow to surface
                                        volSWE, &
                                        StateFraction, & !Fraction of state that can freeze
                                        freezMelt, & !Amount of freezing meltwater in mm for the ith surface area
                                        Qm_freezState, & !Heat by freezing of surface state
                                        freezState, & !Amount of freezing state in mm for the ith surface area
                                        FreezStateVol, &
                                        Qm_melt, & !Heat consumption by snow melt
                                        Qm_rain, & !Heat by rain falling on snow
                                        rainOnSnow, & !Liquid precipitation falling on snow ()
                                        SnowPackLimit, &
                                        deltaQi

   REAL(KIND(1D0)), DIMENSION(nsurf, MaxNumberOfGrids) :: iceFrac_grids

   REAL(KIND(1D0)), DIMENSION(nsurf) :: snowPack, & !Amount of snow on each surface in mm
                                        snowPackOld
   INTEGER, DIMENSION(nsurf) :: heiG, & !snow layer height
                                snowCoverForms, &
                                snowCalcSwitch = 0 !Defines if snow related balance is made
   !-----------------------------------------------------------------------------------------------

   ! ---- Grid connections ------------------------------------------------------------------------
   !! Grid connections needs coding, currently no water transfer between grids
   ! Added HCW 14 Nov 2014
   INTEGER, PARAMETER :: nconns = 8 !Number of grids for between-grid connections
   REAL(KIND(1D0)), DIMENSION(nconns) :: GridToFrac !Fraction of water moving to the grid specified in GridTo [-]
   REAL(KIND(1D0)), DIMENSION(nconns) :: GridTo !Grid that water moves to
   !!character(len=15),dimension(2,MaxNumberOfGrids)::GridConnections  !List of different grid corrections
   !!real (kind(1d0)),dimension(MaxNumberOfGrids)::   GridConnectionsFrac   !Fraction of water moving between the different grids
   !-----------------------------------------------------------------------------------------------

   ! ---- AnOHM related variable, added by TS, 01 Mar 2016 ---------------------------------------------------------------
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: a1AnOHM, a2AnOHM, a3AnOHM ! OHM coefficients, a1 [-]; a2 [h]; a3 [W m-2]
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: mAHAnOHM ! daily mean AH [W m-2]
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: BoAnOHMStart ! initial Bo for interation [-]
   REAL(KIND(1D0)), DIMENSION(MaxNumberOfGrids) :: BoAnOHMEnd ! final Bo for interation [-]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: cpAnOHM ! heat capacity [J m-3 K-1]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: kkAnOHM ! thermal conductivity [W m-1 K-1]
   REAL(KIND(1D0)), DIMENSION(nsurf) :: chAnOHM ! bulk transfer coef. [-]
   !-----------------------------------------------------------------------------------------------

   !------------------- ESTM variables for SUEWS surfaces---------------------------------------------------------
   INTEGER :: nlayer !Number of vertical layers
   INTEGER, DIMENSION(:), ALLOCATABLE :: nlayer_grids !Number of vertical layers for each grid

   ! roof
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: sfr_roof
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_roof
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_roof
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_roof
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_roof
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: alb_roof
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: emis_roof
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_roof
   ! larger container arrays for different grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: tsfc_roof_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: sfr_roof_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: k_roof_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: cp_roof_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dz_roof_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: tin_roof_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: alb_roof_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: emis_roof_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: temp_roof_grids

   ! wall
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: sfr_wall
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_wall
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_wall
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_wall
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_wall
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_wall
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: alb_wall
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: emis_wall
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_wall
   ! larger container arrays for different grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: tsfc_wall_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: sfr_wall_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: k_wall_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: cp_wall_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dz_wall_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: tin_wall_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: alb_wall_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: emis_wall_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: temp_wall_grids

   ! standard suews surfaces
   ! INTEGER :: nsurf !Number of surf facets
   ! REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: sfr_surf
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_surf
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_surf
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_surf
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_surf
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_surf
   ! INTEGER :: nsurf_grids !Number of surf_grids facets
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: tsfc_surf_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: k_surf_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: cp_surf_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: dz_surf_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: tin_surf_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: temp_surf_grids
   !-----------------------------------------------------------------------------------------------

   !------------------- SPARTACUS variables for SUEWS subsurfaces----------------------------------------------
   ! INTEGER, PARAMETER :: JPRD = SELECTED_REAL_KIND(13,300) ! same as kind(1d0)
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: height
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: building_frac
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_frac
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: building_scale
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_scale
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_ext
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_fsd
   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_contact_fraction
   ! REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: alb_roof
   ! REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: alb_wall
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: roof_albedo_dir_mult_fact
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: wall_specular_frac
   ! REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: emis_roof
   ! REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: emis_wall

   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: height_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: building_frac_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: veg_frac_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: building_scale_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: veg_scale_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: veg_ext_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: veg_fsd_grids
   REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: veg_contact_fraction_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: roof_albedo_dir_mult_fact_grids
   REAL(KIND(1D0)), DIMENSION(:, :, :), ALLOCATABLE :: wall_specular_frac_grids

   ! SPARTACUS input variables
   INTEGER :: n_vegetation_region_urban, &
              n_stream_sw_urban, n_stream_lw_urban
   REAL(KIND(1D0)) :: sw_dn_direct_frac, air_ext_sw, air_ssa_sw, &
                      veg_ssa_sw, air_ext_lw, air_ssa_lw, veg_ssa_lw, &
                      veg_fsd_const, veg_contact_fraction_const, &
                      ground_albedo_dir_mult_fact
   LOGICAL :: use_sw_direct_albedo
   !------------------- ESTM_ext variables for heterogeneous  facets---------------------------------------------------------
   REAL(KIND(1D0)), DIMENSION(5, nsurfIncSnow) :: zSurf_SUEWSsurfs, &
                                                  kSurf_SUEWSsurfs, &
                                                  rSurf_SUEWSsurfs

   !-----------------------------------------------------------------------------------------------

   !---------------------------------- Column numbers ---------------------------------------------

   ! ---- Set column numbering for SurfaceChar ----------------------------------------------------
   ! Columns 1:80 are the same as in SiteSelect.txt and defined below
   INTEGER :: cc !Column counter
   INTEGER, PARAMETER :: ccEndSI = ncolumnsSiteSelect

   ! Applicable to each surface
   INTEGER, DIMENSION(nsurf) :: c_AlbMin = (/(cc, cc=ccEndSI + 0*nsurf + 1, ccEndSI + 0*nsurf + nsurf, 1)/) !Min. albedo
   INTEGER, DIMENSION(nsurf) :: c_AlbMax = (/(cc, cc=ccEndSI + 1*nsurf + 1, ccEndSI + 1*nsurf + nsurf, 1)/) !Max. albedo
   INTEGER, DIMENSION(nsurf) :: c_Emis = (/(cc, cc=ccEndSI + 2*nsurf + 1, ccEndSI + 2*nsurf + nsurf, 1)/) !Emissivity
   INTEGER, DIMENSION(nsurf) :: c_StorMin = (/(cc, cc=ccEndSI + 3*nsurf + 1, ccEndSI + 3*nsurf + nsurf, 1)/) !Min. storage capacity (canopy)
   INTEGER, DIMENSION(nsurf) :: c_StorMax = (/(cc, cc=ccEndSI + 4*nsurf + 1, ccEndSI + 4*nsurf + nsurf, 1)/) !Max. storage capacity (canopy)
   INTEGER, DIMENSION(nsurf) :: c_WetThresh = (/(cc, cc=ccEndSI + 5*nsurf + 1, ccEndSI + 5*nsurf + nsurf, 1)/) !Threshold for wet evaporation [mm]
   INTEGER, DIMENSION(nsurf) :: c_StateLimit = (/(cc, cc=ccEndSI + 6*nsurf + 1, ccEndSI + 6*nsurf + nsurf, 1)/) !Limit for surface state [mm]
   INTEGER, DIMENSION(nsurf) :: c_DrEq = (/(cc, cc=ccEndSI + 7*nsurf + 1, ccEndSI + 7*nsurf + nsurf, 1)/) !Drainage equation
   INTEGER, DIMENSION(nsurf) :: c_DrCoef1 = (/(cc, cc=ccEndSI + 8*nsurf + 1, ccEndSI + 8*nsurf + nsurf, 1)/) !Drainage coef. 1
   INTEGER, DIMENSION(nsurf) :: c_DrCoef2 = (/(cc, cc=ccEndSI + 9*nsurf + 1, ccEndSI + 9*nsurf + nsurf, 1)/) !Drainage coef. 2
   INTEGER, DIMENSION(nsurf) :: c_SoilTCode = (/(cc, cc=ccEndSI + 10*nsurf + 1, ccEndSI + 10*nsurf + nsurf, 1)/) !Soil type code

   ! N.B. not included in SUEWS_Water.txt
   INTEGER, DIMENSION(nsurf) :: c_SnowLimPat = (/(cc, cc=ccEndSI + 11*nsurf + 1, ccEndSI + 11*nsurf + nsurf, 1)/) !Snow limit for patchiness
   ! N.B. currently only in SUEWS_NonVeg.txt
   INTEGER, DIMENSION(nsurf) :: c_SnowLimRem = (/(cc, cc=ccEndSI + 12*nsurf + 1, ccEndSI + 12*nsurf + nsurf, 1)/) !Snow limit for removal
   ! AnOHM TS
   INTEGER, DIMENSION(nsurf) :: c_CpAnOHM = (/(cc, cc=ccEndSI + 13*nsurf + 1, ccEndSI + 13*nsurf + nsurf, 1)/) !heat capacity, AnOHM TS
   INTEGER, DIMENSION(nsurf) :: c_KkAnOHM = (/(cc, cc=ccEndSI + 14*nsurf + 1, ccEndSI + 14*nsurf + nsurf, 1)/) !heat conductivity, AnOHM TS
   INTEGER, DIMENSION(nsurf) :: c_ChAnOHM = (/(cc, cc=ccEndSI + 15*nsurf + 1, ccEndSI + 15*nsurf + nsurf, 1)/) !bulk transfer coef., AnOHM TS

   ! Find current column number
   INTEGER, PARAMETER :: ccEndI = (ccEndSI + 15*nsurf + nsurf) !add columns for AnOHM, AnOHM TS

   ! Applicable to vegetated surfaces only
   INTEGER, DIMENSION(NVegSurf) :: c_BaseT = (/(cc, cc=ccEndI + 0*nvegsurf + 1, ccEndI + 0*nvegsurf + nvegsurf, 1)/) !Base temp. for leaf-on
   INTEGER, DIMENSION(NVegSurf) :: c_BaseTe = (/(cc, cc=ccEndI + 1*nvegsurf + 1, ccEndI + 1*nvegsurf + nvegsurf, 1)/) !Base temp. for leaf-off
   INTEGER, DIMENSION(NVegSurf) :: c_GDDFull = (/(cc, cc=ccEndI + 2*nvegsurf + 1, ccEndI + 2*nvegsurf + nvegsurf, 1)/) !GDD for full LAI
   INTEGER, DIMENSION(NVegSurf) :: c_SDDFull = (/(cc, cc=ccEndI + 3*nvegsurf + 1, ccEndI + 3*nvegsurf + nvegsurf, 1)/) !SDD for start of leaf-fall
   INTEGER, DIMENSION(NVegSurf) :: c_LAIMin = (/(cc, cc=ccEndI + 4*nvegsurf + 1, ccEndI + 4*nvegsurf + nvegsurf, 1)/) !Min. LAI
   INTEGER, DIMENSION(NVegSurf) :: c_LAIMax = (/(cc, cc=ccEndI + 5*nvegsurf + 1, ccEndI + 5*nvegsurf + nvegsurf, 1)/) !Max. LAI
   INTEGER, DIMENSION(NVegSurf) :: c_PorosityMin = (/(cc, cc=ccEndI + 6*nvegsurf + 1, ccEndI + 6*nvegsurf + nvegsurf, 1)/) !Min. Porosity
   INTEGER, DIMENSION(NVegSurf) :: c_PorosityMax = (/(cc, cc=ccEndI + 7*nvegsurf + 1, ccEndI + 7*nvegsurf + nvegsurf, 1)/) !Max. Porosity
   INTEGER, DIMENSION(NVegSurf) :: c_GsMax = (/(cc, cc=ccEndI + 8*nvegsurf + 1, ccEndI + 8*nvegsurf + nvegsurf, 1)/) !Max. conductance
   INTEGER, DIMENSION(NVegSurf) :: c_LAIEq = (/(cc, cc=ccEndI + 9*nvegsurf + 1, ccEndI + 9*nvegsurf + nvegsurf, 1)/) !LAI equation
   INTEGER, DIMENSION(NVegSurf) :: c_LeafGP1 = (/(cc, cc=ccEndI + 10*nvegsurf + 1, ccEndI + 10*nvegsurf + nvegsurf, 1)/) !Leaf growth power 1
   INTEGER, DIMENSION(NVegSurf) :: c_LeafGP2 = (/(cc, cc=ccEndI + 11*nvegsurf + 1, ccEndI + 11*nvegsurf + nvegsurf, 1)/) !Leaf growth power 2
   INTEGER, DIMENSION(NVegSurf) :: c_LeafOP1 = (/(cc, cc=ccEndI + 12*nvegsurf + 1, ccEndI + 12*nvegsurf + nvegsurf, 1)/) !Leaf-off power 1
   INTEGER, DIMENSION(NVegSurf) :: c_LeafOP2 = (/(cc, cc=ccEndI + 13*nvegsurf + 1, ccEndI + 13*nvegsurf + nvegsurf, 1)/) !Leaf-off power 2
   INTEGER, DIMENSION(NVegSurf) :: c_BiogenCO2Code = (/(cc, cc=ccEndI + 14*nvegsurf + 1, ccEndI + 14*nvegsurf + nvegsurf, 1)/) !Biogenic CO2 Code
   ! Find current column number
   INTEGER, PARAMETER :: ccEndP = (ccEndI + 14*nvegsurf + nvegsurf)

   ! Applicable to water surfaces only
   INTEGER :: c_WaterDepth = (ccEndP + 1)

   ! Find current column number
   INTEGER, PARAMETER :: ccEndW = (ccEndP + 1)

   ! Applicable to snow only
   INTEGER :: c_SnowRMFactor = (ccEndW + 1)
   INTEGER :: c_SnowTMFactor = (ccEndW + 2)
   INTEGER :: c_SnowAlbMin = (ccEndW + 3)
   INTEGER :: c_SnowAlbMax = (ccEndW + 4)
   !integer:: c_SnowAlb      = (ccEndW+ 5)
   INTEGER :: c_SnowEmis = (ccEndW + 6)
   INTEGER :: c_Snowtau_a = (ccEndW + 7)
   INTEGER :: c_Snowtau_f = (ccEndW + 8)
   INTEGER :: c_SnowPLimAlb = (ccEndW + 9)
   INTEGER :: c_SnowSDMin = (ccEndW + 10)
   INTEGER :: c_SnowSDMax = (ccEndW + 11)
   INTEGER :: c_Snowtau_r = (ccEndW + 12)
   INTEGER :: c_SnowCRWMin = (ccEndW + 13)
   INTEGER :: c_SnowCRWMax = (ccEndW + 14)
   INTEGER :: c_SnowPLimSnow = (ccEndW + 15)

   ! Find current column number
   INTEGER, PARAMETER :: ccEndSn = (ccEndW + 15)

   ! Soil information
   INTEGER, DIMENSION(nsurf) :: c_SoilDepth = (/(cc, cc=ccEndSn + 0*nsurf + 1, ccEndSn + 0*nsurf + nsurf, 1)/) ! Volumetric SM capacity
   INTEGER, DIMENSION(nsurf) :: c_SoilStCap = (/(cc, cc=ccEndSn + 1*nsurf + 1, ccEndSn + 1*nsurf + nsurf, 1)/) ! Volumetric SM capacity
   INTEGER, DIMENSION(nsurf) :: c_KSat = (/(cc, cc=ccEndSn + 2*nsurf + 1, ccEndSn + 2*nsurf + nsurf, 1)/) ! Saturated hydraulic conductivity
   INTEGER, DIMENSION(nsurf) :: c_SoilDens = (/(cc, cc=ccEndSn + 3*nsurf + 1, ccEndSn + 3*nsurf + nsurf, 1)/) ! Soil Density
   INTEGER, DIMENSION(nsurf) :: c_SoilInfRate = (/(cc, cc=ccEndSn + 4*nsurf + 1, ccEndSn + 4*nsurf + nsurf, 1)/) ! Soil infiltration rate
   INTEGER, DIMENSION(nsurf) :: c_ObsSMDepth = (/(cc, cc=ccEndSn + 5*nsurf + 1, ccEndSn + 5*nsurf + nsurf, 1)/) ! Depth of SM obs
   INTEGER, DIMENSION(nsurf) :: c_ObsSMMax = (/(cc, cc=ccEndSn + 6*nsurf + 1, ccEndSn + 6*nsurf + nsurf, 1)/) ! Obs maximum SM [kg kg-1 OR m3 m-3]
   INTEGER, DIMENSION(nsurf) :: c_ObsSNRFrac = (/(cc, cc=ccEndSn + 7*nsurf + 1, ccEndSn + 7*nsurf + nsurf, 1)/) ! Obs fraction of soil without rocks

   ! Find current column number
   INTEGER, PARAMETER :: ccEndSo = (ccEndSn + 7*nsurf + nsurf)

   ! Surface conductance
   INTEGER :: c_GsG1 = (ccEndSo + 1)
   INTEGER :: c_GsG2 = (ccEndSo + 2)
   INTEGER :: c_GsG3 = (ccEndSo + 3)
   INTEGER :: c_GsG4 = (ccEndSo + 4)
   INTEGER :: c_GsG5 = (ccEndSo + 5)
   INTEGER :: c_GsG6 = (ccEndSo + 6)
   INTEGER :: c_GsTH = (ccEndSo + 7)
   INTEGER :: c_GsTL = (ccEndSo + 8)
   INTEGER :: c_GsS1 = (ccEndSo + 9)
   INTEGER :: c_GsS2 = (ccEndSo + 10)
   INTEGER :: c_GsKmax = (ccEndSo + 11)
   INTEGER :: c_gsModel = (ccEndSo + 12)

   ! Find current column number
   INTEGER, PARAMETER :: ccEndGs = (ccEndSo + 12)

   ! OHM codes
   INTEGER, DIMENSION(nsurfIncSnow) :: c_OHMCode_SWet = (/(cc, cc=ccEndGs + 0*nsurfIncSnow + 1, &
                                                           ccEndGs + 0*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM code (summer wet)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_OHMCode_SDry = (/(cc, cc=ccEndGs + 1*nsurfIncSnow + 1, &
                                                           ccEndGs + 1*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM code (summer dry)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_OHMCode_WWet = (/(cc, cc=ccEndGs + 2*nsurfIncSnow + 1, &
                                                           ccEndGs + 2*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM code (winter wet)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_OHMCode_WDry = (/(cc, cc=ccEndGs + 3*nsurfIncSnow + 1, &
                                                           ccEndGs + 3*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM code (winter dry)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a1_SWet = (/(cc, cc=ccEndGs + 4*nsurfIncSnow + 1, &
                                                      ccEndGs + 4*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a1 (summer wet)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a2_SWet = (/(cc, cc=ccEndGs + 5*nsurfIncSnow + 1, &
                                                      ccEndGs + 5*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a2 (summer wet)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a3_SWet = (/(cc, cc=ccEndGs + 6*nsurfIncSnow + 1, &
                                                      ccEndGs + 6*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a3 (summer wet)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a1_SDry = (/(cc, cc=ccEndGs + 7*nsurfIncSnow + 1, &
                                                      ccEndGs + 7*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a1 (summer dry)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a2_SDry = (/(cc, cc=ccEndGs + 8*nsurfIncSnow + 1, &
                                                      ccEndGs + 8*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a2 (summer dry)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a3_SDry = (/(cc, cc=ccEndGs + 9*nsurfIncSnow + 1, &
                                                      ccEndGs + 9*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a3 (summer dry)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a1_WWet = (/(cc, cc=ccEndGs + 10*nsurfIncSnow + 1, &
                                                      ccEndGs + 10*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a1 (winter wet)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a2_WWet = (/(cc, cc=ccEndGs + 11*nsurfIncSnow + 1, &
                                                      ccEndGs + 11*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a2 (winter wet)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a3_WWet = (/(cc, cc=ccEndGs + 12*nsurfIncSnow + 1, &
                                                      ccEndGs + 12*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a3 (winter wet)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a1_WDry = (/(cc, cc=ccEndGs + 13*nsurfIncSnow + 1, &
                                                      ccEndGs + 13*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a1 (winter dry)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a2_WDry = (/(cc, cc=ccEndGs + 14*nsurfIncSnow + 1, &
                                                      ccEndGs + 14*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a2 (winter dry)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_a3_WDry = (/(cc, cc=ccEndGs + 15*nsurfIncSnow + 1, &
                                                      ccEndGs + 15*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM a3 (winter dry)

   INTEGER, DIMENSION(nsurfIncSnow) :: c_OHMThresh_SW = (/(cc, cc=ccEndGs + 16*nsurfIncSnow + 1, &
                                                           ccEndGs + 16*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM Threshold (summer/winter)
   INTEGER, DIMENSION(nsurfIncSnow) :: c_OHMThresh_WD = (/(cc, cc=ccEndGs + 17*nsurfIncSnow + 1, &
                                                           ccEndGs + 17*nsurfIncSnow + nsurfIncSnow, 1)/) !OHM Threshold (wet/dry)

   ! ESTM code for each surface inclduing snow
   INTEGER, DIMENSION(nsurfIncSnow) :: c_ESTMCode = (/(cc, cc=ccEndGs + 18*nsurfIncSnow + 1, &
                                                       ccEndGs + 18*nsurfIncSnow + nsurfIncSnow, 1)/) !ESTM code

   ! Find current column number
   INTEGER, PARAMETER :: ccEndO = (ccEndGs + 18*nsurfIncSnow + nsurfIncSnow)

   ! Anthropogenic Emissions
   INTEGER :: c_BaseT_HC = (ccEndO + 1)
   INTEGER :: c_QF_A1 = (ccEndO + 2)
   INTEGER :: c_QF_B1 = (ccEndO + 3)
   INTEGER :: c_QF_C1 = (ccEndO + 4)
   INTEGER :: c_QF_A2 = (ccEndO + 5)
   INTEGER :: c_QF_B2 = (ccEndO + 6)
   INTEGER :: c_QF_C2 = (ccEndO + 7)
   INTEGER :: c_AHMin_WD = (ccEndO + 8)
   INTEGER :: c_AHMin_WE = (ccEndO + 9)
   INTEGER :: c_AHSlopeHeating_WD = (ccEndO + 10)
   INTEGER :: c_AHSlopeHeating_WE = (ccEndO + 11)
   INTEGER :: c_AHSlopeCooling_WD = (ccEndO + 12)
   INTEGER :: c_AHSlopeCooling_WE = (ccEndO + 13)
   INTEGER :: c_TCriticHeating_WE = (ccEndO + 14)
   INTEGER :: c_TCriticHeating_WD = (ccEndO + 15)
   INTEGER :: c_TCriticCooling_WE = (ccEndO + 16)
   INTEGER :: c_TCriticCooling_WD = (ccEndO + 17)
   INTEGER :: c_EnProfWD = (ccEndO + 18)
   INTEGER :: c_EnProfWE = (ccEndO + 19)
   INTEGER :: c_CO2mWD = (ccEndO + 20)
   INTEGER :: c_CO2mWE = (ccEndO + 21)
   INTEGER :: c_TraffProfWD = (ccEndO + 22)
   INTEGER :: c_TraffProfWE = (ccEndO + 23)
   INTEGER :: c_PopProfWD = (ccEndO + 24)
   INTEGER :: c_PopProfWE = (ccEndO + 25)
   INTEGER :: c_MinQFMetab = (ccEndO + 26)
   INTEGER :: c_MaxQFMetab = (ccEndO + 27)
   INTEGER :: c_MinFCMetab = (ccEndO + 28)
   INTEGER :: c_MaxFCMetab = (ccEndO + 29)
   INTEGER :: c_FrPDDwe = (ccEndO + 30)
   INTEGER :: c_FrFossilFuel_Heat = (ccEndO + 31)
   INTEGER :: c_FrFossilFuel_NonHeat = (ccEndO + 32)
   INTEGER :: c_EF_umolCO2perJ = (ccEndO + 33)
   INTEGER :: c_EnEF_v_Jkm = (ccEndO + 34)
   INTEGER :: c_FcEF_v_kgkmWD = (ccEndO + 35)
   INTEGER :: c_FcEF_v_kgkmWE = (ccEndO + 36)
   INTEGER :: c_CO2PointSource = (ccEndO + 37)
   INTEGER :: c_TrafficUnits = (ccEndO + 38)

   ! Find current column number
   INTEGER, PARAMETER :: ccEndA = (ccEndO + 38)

   ! Irrigation
   INTEGER :: c_IeStart = (ccEndA + 1)
   INTEGER :: c_IeEnd = (ccEndA + 2)
   INTEGER :: c_IntWU = (ccEndA + 3)
   INTEGER :: c_Faut = (ccEndA + 4)
   INTEGER :: c_h_maintain = (ccEndA + 5)
   INTEGER, DIMENSION(3) :: c_Ie_a = (/(cc, cc=ccEndA + 5 + 0*3 + 1, ccEndA + 5 + 0*3 + 3, 1)/) ! Automatic irrigation coeffs
   INTEGER, DIMENSION(3) :: c_Ie_m = (/(cc, cc=ccEndA + 5 + 1*3 + 1, ccEndA + 5 + 1*3 + 3, 1)/) ! Manual irrigation coeffs
   INTEGER, DIMENSION(7) :: c_DayWat = (/(cc, cc=ccEndA + 11 + 0*7 + 1, ccEndA + 11 + 0*7 + 7, 1)/) ! Irrigation allowed on each day
   INTEGER, DIMENSION(7) :: c_DayWatPer = (/(cc, cc=ccEndA + 11 + 1*7 + 1, ccEndA + 11 + 1*7 + 7, 1)/) ! Fraction properties using irrigation allowed on each day

   ! Find current column number
   INTEGER, PARAMETER :: ccEndIr = (ccEndA + 11 + 1*7 + 7)

   ! Hourly profiles
   INTEGER, DIMENSION(24) :: c_HrProfEnUseWD = (/(cc, cc=ccEndIr + 0*24 + 1, ccEndIr + 0*24 + 24, 1)/) ! Energy use, weekdays
   INTEGER, DIMENSION(24) :: c_HrProfEnUseWE = (/(cc, cc=ccEndIr + 1*24 + 1, ccEndIr + 1*24 + 24, 1)/) ! Energy use, weekends
   INTEGER, DIMENSION(24) :: c_HrProfWUManuWD = (/(cc, cc=ccEndIr + 2*24 + 1, ccEndIr + 2*24 + 24, 1)/) ! Water use, manual, weekdays
   INTEGER, DIMENSION(24) :: c_HrProfWUManuWE = (/(cc, cc=ccEndIr + 3*24 + 1, ccEndIr + 3*24 + 24, 1)/) ! Water use, manual, weekends
   INTEGER, DIMENSION(24) :: c_HrProfWUAutoWD = (/(cc, cc=ccEndIr + 4*24 + 1, ccEndIr + 4*24 + 24, 1)/) ! Water use, automatic, weekdays
   INTEGER, DIMENSION(24) :: c_HrProfWUAutoWE = (/(cc, cc=ccEndIr + 5*24 + 1, ccEndIr + 5*24 + 24, 1)/) ! Water use, automatic, weekends
   INTEGER, DIMENSION(24) :: c_HrProfSnowCWD = (/(cc, cc=ccEndIr + 6*24 + 1, ccEndIr + 6*24 + 24, 1)/) ! Snow clearing, weekdays
   INTEGER, DIMENSION(24) :: c_HrProfSnowCWE = (/(cc, cc=ccEndIr + 7*24 + 1, ccEndIr + 7*24 + 24, 1)/) ! Snow clearing, weekends
   INTEGER, DIMENSION(24) :: c_HrProfHumActivityWD = (/(cc, cc=ccEndIr + 8*24 + 1, ccEndIr + 8*24 + 24, 1)/) ! Human activity, weekdays
   INTEGER, DIMENSION(24) :: c_HrProfHumActivityWE = (/(cc, cc=ccEndIr + 9*24 + 1, ccEndIr + 9*24 + 24, 1)/) ! Human activity, weekends
   INTEGER, DIMENSION(24) :: c_HrProfTraffWD = (/(cc, cc=ccEndIr + 10*24 + 1, ccEndIr + 10*24 + 24, 1)/) ! Traffic, weekdays
   INTEGER, DIMENSION(24) :: c_HrProfTraffWE = (/(cc, cc=ccEndIr + 11*24 + 1, ccEndIr + 11*24 + 24, 1)/) ! Traffic, weekends
   INTEGER, DIMENSION(24) :: c_HrProfPopWD = (/(cc, cc=ccEndIr + 12*24 + 1, ccEndIr + 12*24 + 24, 1)/) ! Population, weekdays
   INTEGER, DIMENSION(24) :: c_HrProfPopWE = (/(cc, cc=ccEndIr + 13*24 + 1, ccEndIr + 13*24 + 24, 1)/) ! Population, weekends

   ! Find current column number
   INTEGER, PARAMETER :: ccEndPr = (ccEndIr + 13*24 + 24)

   ! Within-grid water distribution (for each surface)
   INTEGER, DIMENSION(nsurf) :: c_WGToPaved = (/(cc, cc=ccEndPr + 0*nsurf + 1, ccEndPr + 0*nsurf + nsurf, 1)/) !Water dist to Paved
   INTEGER, DIMENSION(nsurf) :: c_WGToBldgs = (/(cc, cc=ccEndPr + 1*nsurf + 1, ccEndPr + 1*nsurf + nsurf, 1)/) !Water dist to Bldgs
   INTEGER, DIMENSION(nsurf) :: c_WGToEveTr = (/(cc, cc=ccEndPr + 2*nsurf + 1, ccEndPr + 2*nsurf + nsurf, 1)/) !Water dist to EveTr
   INTEGER, DIMENSION(nsurf) :: c_WGToDecTr = (/(cc, cc=ccEndPr + 3*nsurf + 1, ccEndPr + 3*nsurf + nsurf, 1)/) !Water dist to DecTr
   INTEGER, DIMENSION(nsurf) :: c_WGToGrass = (/(cc, cc=ccEndPr + 4*nsurf + 1, ccEndPr + 4*nsurf + nsurf, 1)/) !Water dist to Grass
   INTEGER, DIMENSION(nsurf) :: c_WGToBSoil = (/(cc, cc=ccEndPr + 5*nsurf + 1, ccEndPr + 5*nsurf + nsurf, 1)/) !Water dist to BSoil
   INTEGER, DIMENSION(nsurf) :: c_WGToWater = (/(cc, cc=ccEndPr + 6*nsurf + 1, ccEndPr + 6*nsurf + nsurf, 1)/) !Water dist to Water
   INTEGER, DIMENSION(nsurf) :: c_WGToRunoff = (/(cc, cc=ccEndPr + 7*nsurf + 1, ccEndPr + 7*nsurf + nsurf, 1)/) !Water dist to runoff
   INTEGER, DIMENSION(nsurf) :: c_WGToSoilStore = (/(cc, cc=ccEndPr + 8*nsurf + 1, ccEndPr + 8*nsurf + nsurf, 1)/) !Water dist to sub-surface soil

   ! Find current column number
   INTEGER, PARAMETER :: cBEndWG = (ccEndPr + 8*nsurf + nsurf)

   ! Biogenic CO2
   INTEGER, DIMENSION(nvegsurf) :: c_alpha_bioCO2 = (/(cc, cc=cBEndWG + 0*nvegsurf + 1, cBEndWG + 0*nvegsurf + nvegsurf, 1)/)
   INTEGER, DIMENSION(nvegsurf) :: c_beta_bioCO2 = (/(cc, cc=cBEndWG + 1*nvegsurf + 1, cBEndWG + 1*nvegsurf + nvegsurf, 1)/)
   INTEGER, DIMENSION(nvegsurf) :: c_theta_bioCO2 = (/(cc, cc=cBEndWG + 2*nvegsurf + 1, cBEndWG + 2*nvegsurf + nvegsurf, 1)/)
   INTEGER, DIMENSION(nvegsurf) :: c_alpha_enh_bioCO2 = (/(cc, cc=cBEndWG + 3*nvegsurf + 1, cBEndWG + 3*nvegsurf + nvegsurf, 1)/)
   INTEGER, DIMENSION(nvegsurf) :: c_beta_enh_bioCO2 = (/(cc, cc=cBEndWG + 4*nvegsurf + 1, cBEndWG + 4*nvegsurf + nvegsurf, 1)/)
   INTEGER, DIMENSION(nvegsurf) :: c_resp_a = (/(cc, cc=cBEndWG + 5*nvegsurf + 1, cBEndWG + 5*nvegsurf + nvegsurf, 1)/)
   INTEGER, DIMENSION(nvegsurf) :: c_resp_b = (/(cc, cc=cBEndWG + 6*nvegsurf + 1, cBEndWG + 6*nvegsurf + nvegsurf, 1)/)
   INTEGER, DIMENSION(nvegsurf) :: c_min_res_bioCO2 = (/(cc, cc=cBEndWG + 7*nvegsurf + 1, cBEndWG + 7*nvegsurf + nvegsurf, 1)/)

   ! Find current column number
   INTEGER, PARAMETER :: ccEndB = (cBEndWG + 7*nvegsurf + nvegsurf)

   !ESTM
   ! Roof/surface characteristics for all surfaces including snow
   INTEGER, DIMENSION(nsurfIncSnow) :: &
      c_Surf_thick1 = [(cc, cc=ccEndB + 0*nsurfIncSnow + 1, ccEndB + 0*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_k1 = [(cc, cc=ccEndB + 1*nsurfIncSnow + 1, ccEndB + 1*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_rhoCp1 = [(cc, cc=ccEndB + 2*nsurfIncSnow + 1, ccEndB + 2*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_thick2 = [(cc, cc=ccEndB + 3*nsurfIncSnow + 1, ccEndB + 3*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_k2 = [(cc, cc=ccEndB + 4*nsurfIncSnow + 1, ccEndB + 4*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_rhoCp2 = [(cc, cc=ccEndB + 5*nsurfIncSnow + 1, ccEndB + 5*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_thick3 = [(cc, cc=ccEndB + 6*nsurfIncSnow + 1, ccEndB + 6*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_k3 = [(cc, cc=ccEndB + 7*nsurfIncSnow + 1, ccEndB + 7*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_rhoCp3 = [(cc, cc=ccEndB + 8*nsurfIncSnow + 1, ccEndB + 8*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_thick4 = [(cc, cc=ccEndB + 9*nsurfIncSnow + 1, ccEndB + 9*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_k4 = [(cc, cc=ccEndB + 10*nsurfIncSnow + 1, ccEndB + 10*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_rhoCp4 = [(cc, cc=ccEndB + 11*nsurfIncSnow + 1, ccEndB + 11*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_thick5 = [(cc, cc=ccEndB + 12*nsurfIncSnow + 1, ccEndB + 12*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_k5 = [(cc, cc=ccEndB + 13*nsurfIncSnow + 1, ccEndB + 13*nsurfIncSnow + nsurfIncSnow, 1)], &
      c_Surf_rhoCp5 = [(cc, cc=ccEndB + 14*nsurfIncSnow + 1, ccEndB + 14*nsurfIncSnow + nsurfIncSnow, 1)]
   ! Find current column number
   INTEGER, PARAMETER :: ccEndESTMB = (ccEndB + 14*nsurfIncSnow + nsurfIncSnow)
   ! Other ESTM characteristics are for built surfaces only
   INTEGER :: c_Wall_thick1 = (ccEndESTMB + 1)
   INTEGER :: c_Wall_k1 = (ccEndESTMB + 2)
   INTEGER :: c_Wall_rhoCp1 = (ccEndESTMB + 3)
   INTEGER :: c_Wall_thick2 = (ccEndESTMB + 4)
   INTEGER :: c_Wall_k2 = (ccEndESTMB + 5)
   INTEGER :: c_Wall_rhoCp2 = (ccEndESTMB + 6)
   INTEGER :: c_Wall_thick3 = (ccEndESTMB + 7)
   INTEGER :: c_Wall_k3 = (ccEndESTMB + 8)
   INTEGER :: c_Wall_rhoCp3 = (ccEndESTMB + 9)
   INTEGER :: c_Wall_thick4 = (ccEndESTMB + 10)
   INTEGER :: c_Wall_k4 = (ccEndESTMB + 11)
   INTEGER :: c_Wall_rhoCp4 = (ccEndESTMB + 12)
   INTEGER :: c_Wall_thick5 = (ccEndESTMB + 13)
   INTEGER :: c_Wall_k5 = (ccEndESTMB + 14)
   INTEGER :: c_Wall_rhoCp5 = (ccEndESTMB + 15)
   INTEGER :: c_Internal_thick1 = (ccEndESTMB + 16)
   INTEGER :: c_Internal_k1 = (ccEndESTMB + 17)
   INTEGER :: c_Internal_rhoCp1 = (ccEndESTMB + 18)
   INTEGER :: c_Internal_thick2 = (ccEndESTMB + 19)
   INTEGER :: c_Internal_k2 = (ccEndESTMB + 20)
   INTEGER :: c_Internal_rhoCp2 = (ccEndESTMB + 21)
   INTEGER :: c_Internal_thick3 = (ccEndESTMB + 22)
   INTEGER :: c_Internal_k3 = (ccEndESTMB + 23)
   INTEGER :: c_Internal_rhoCp3 = (ccEndESTMB + 24)
   INTEGER :: c_Internal_thick4 = (ccEndESTMB + 25)
   INTEGER :: c_Internal_k4 = (ccEndESTMB + 26)
   INTEGER :: c_Internal_rhoCp4 = (ccEndESTMB + 27)
   INTEGER :: c_Internal_thick5 = (ccEndESTMB + 28)
   INTEGER :: c_Internal_k5 = (ccEndESTMB + 29)
   INTEGER :: c_Internal_rhoCp5 = (ccEndESTMB + 30)
   INTEGER :: c_nroom = (ccEndESTMB + 31)
   INTEGER :: c_alb_ibld = (ccEndESTMB + 32)
   INTEGER :: c_em_ibld = (ccEndESTMB + 33)
   INTEGER :: c_CH_iwall = (ccEndESTMB + 34)
   INTEGER :: c_CH_iroof = (ccEndESTMB + 35)
   INTEGER :: c_CH_ibld = (ccEndESTMB + 36)
   ! Find current column number
   INTEGER, PARAMETER :: ccEndESTMM = (ccEndESTMB + 36)
   ! For Paved surfaces, there are 3 possible ESTM classes (with _Surf characteristics only)
   INTEGER, DIMENSION(3) :: c_Surf_thick1_Paved = (/(cc, cc=ccEndESTMM + 0*3 + 1, ccEndESTMM + 0*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_k1_Paved = (/(cc, cc=ccEndESTMM + 1*3 + 1, ccEndESTMM + 1*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_rhoCp1_Paved = (/(cc, cc=ccEndESTMM + 2*3 + 1, ccEndESTMM + 2*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_thick2_Paved = (/(cc, cc=ccEndESTMM + 3*3 + 1, ccEndESTMM + 3*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_k2_Paved = (/(cc, cc=ccEndESTMM + 4*3 + 1, ccEndESTMM + 4*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_rhoCp2_Paved = (/(cc, cc=ccEndESTMM + 5*3 + 1, ccEndESTMM + 5*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_thick3_Paved = (/(cc, cc=ccEndESTMM + 6*3 + 1, ccEndESTMM + 6*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_k3_Paved = (/(cc, cc=ccEndESTMM + 7*3 + 1, ccEndESTMM + 7*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_rhoCp3_Paved = (/(cc, cc=ccEndESTMM + 8*3 + 1, ccEndESTMM + 8*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_thick4_Paved = (/(cc, cc=ccEndESTMM + 9*3 + 1, ccEndESTMM + 9*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_k4_Paved = (/(cc, cc=ccEndESTMM + 10*3 + 1, ccEndESTMM + 10*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_rhoCp4_Paved = (/(cc, cc=ccEndESTMM + 11*3 + 1, ccEndESTMM + 11*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_thick5_Paved = (/(cc, cc=ccEndESTMM + 12*3 + 1, ccEndESTMM + 12*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_k5_Paved = (/(cc, cc=ccEndESTMM + 13*3 + 1, ccEndESTMM + 13*3 + 3, 1)/)
   INTEGER, DIMENSION(3) :: c_Surf_rhoCp5_Paved = (/(cc, cc=ccEndESTMM + 14*3 + 1, ccEndESTMM + 14*3 + 3, 1)/)
   ! Find current column number
   INTEGER, PARAMETER :: ccEndESTMMP = (ccEndESTMM + 14*3 + 3)
   ! For Bldgs surfaces, there are 5 possible ESTM classes (all characteristics)
   INTEGER, DIMENSION(5) :: c_Surf_thick1_Bldgs = (/(cc, cc=ccEndESTMMP + 0*5 + 1, ccEndESTMMP + 0*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_k1_Bldgs = (/(cc, cc=ccEndESTMMP + 1*5 + 1, ccEndESTMMP + 1*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_rhoCp1_Bldgs = (/(cc, cc=ccEndESTMMP + 2*5 + 1, ccEndESTMMP + 2*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_thick2_Bldgs = (/(cc, cc=ccEndESTMMP + 3*5 + 1, ccEndESTMMP + 3*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_k2_Bldgs = (/(cc, cc=ccEndESTMMP + 4*5 + 1, ccEndESTMMP + 4*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_rhoCp2_Bldgs = (/(cc, cc=ccEndESTMMP + 5*5 + 1, ccEndESTMMP + 5*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_thick3_Bldgs = (/(cc, cc=ccEndESTMMP + 6*5 + 1, ccEndESTMMP + 6*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_k3_Bldgs = (/(cc, cc=ccEndESTMMP + 7*5 + 1, ccEndESTMMP + 7*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_rhoCp3_Bldgs = (/(cc, cc=ccEndESTMMP + 8*5 + 1, ccEndESTMMP + 8*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_thick4_Bldgs = (/(cc, cc=ccEndESTMMP + 9*5 + 1, ccEndESTMMP + 9*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_k4_Bldgs = (/(cc, cc=ccEndESTMMP + 10*5 + 1, ccEndESTMMP + 10*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_rhoCp4_Bldgs = (/(cc, cc=ccEndESTMMP + 11*5 + 1, ccEndESTMMP + 11*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_thick5_Bldgs = (/(cc, cc=ccEndESTMMP + 12*5 + 1, ccEndESTMMP + 12*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_k5_Bldgs = (/(cc, cc=ccEndESTMMP + 13*5 + 1, ccEndESTMMP + 13*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Surf_rhoCp5_Bldgs = (/(cc, cc=ccEndESTMMP + 14*5 + 1, ccEndESTMMP + 14*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_thick1_Bldgs = (/(cc, cc=ccEndESTMMP + 15*5 + 1, ccEndESTMMP + 15*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_k1_Bldgs = (/(cc, cc=ccEndESTMMP + 16*5 + 1, ccEndESTMMP + 16*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_rhoCp1_Bldgs = (/(cc, cc=ccEndESTMMP + 17*5 + 1, ccEndESTMMP + 17*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_thick2_Bldgs = (/(cc, cc=ccEndESTMMP + 18*5 + 1, ccEndESTMMP + 18*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_k2_Bldgs = (/(cc, cc=ccEndESTMMP + 19*5 + 1, ccEndESTMMP + 19*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_rhoCp2_Bldgs = (/(cc, cc=ccEndESTMMP + 20*5 + 1, ccEndESTMMP + 20*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_thick3_Bldgs = (/(cc, cc=ccEndESTMMP + 21*5 + 1, ccEndESTMMP + 21*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_k3_Bldgs = (/(cc, cc=ccEndESTMMP + 22*5 + 1, ccEndESTMMP + 22*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_rhoCp3_Bldgs = (/(cc, cc=ccEndESTMMP + 23*5 + 1, ccEndESTMMP + 23*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_thick4_Bldgs = (/(cc, cc=ccEndESTMMP + 24*5 + 1, ccEndESTMMP + 24*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_k4_Bldgs = (/(cc, cc=ccEndESTMMP + 25*5 + 1, ccEndESTMMP + 25*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_rhoCp4_Bldgs = (/(cc, cc=ccEndESTMMP + 26*5 + 1, ccEndESTMMP + 26*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_thick5_Bldgs = (/(cc, cc=ccEndESTMMP + 27*5 + 1, ccEndESTMMP + 27*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_k5_Bldgs = (/(cc, cc=ccEndESTMMP + 28*5 + 1, ccEndESTMMP + 28*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Wall_rhoCp5_Bldgs = (/(cc, cc=ccEndESTMMP + 29*5 + 1, ccEndESTMMP + 29*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_thick1_Bldgs = (/(cc, cc=ccEndESTMMP + 30*5 + 1, ccEndESTMMP + 30*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_k1_Bldgs = (/(cc, cc=ccEndESTMMP + 31*5 + 1, ccEndESTMMP + 31*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_rhoCp1_Bldgs = (/(cc, cc=ccEndESTMMP + 32*5 + 1, ccEndESTMMP + 32*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_thick2_Bldgs = (/(cc, cc=ccEndESTMMP + 33*5 + 1, ccEndESTMMP + 33*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_k2_Bldgs = (/(cc, cc=ccEndESTMMP + 34*5 + 1, ccEndESTMMP + 34*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_rhoCp2_Bldgs = (/(cc, cc=ccEndESTMMP + 35*5 + 1, ccEndESTMMP + 35*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_thick3_Bldgs = (/(cc, cc=ccEndESTMMP + 36*5 + 1, ccEndESTMMP + 36*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_k3_Bldgs = (/(cc, cc=ccEndESTMMP + 37*5 + 1, ccEndESTMMP + 37*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_rhoCp3_Bldgs = (/(cc, cc=ccEndESTMMP + 38*5 + 1, ccEndESTMMP + 38*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_thick4_Bldgs = (/(cc, cc=ccEndESTMMP + 39*5 + 1, ccEndESTMMP + 39*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_k4_Bldgs = (/(cc, cc=ccEndESTMMP + 40*5 + 1, ccEndESTMMP + 40*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_rhoCp4_Bldgs = (/(cc, cc=ccEndESTMMP + 41*5 + 1, ccEndESTMMP + 41*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_thick5_Bldgs = (/(cc, cc=ccEndESTMMP + 42*5 + 1, ccEndESTMMP + 42*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_k5_Bldgs = (/(cc, cc=ccEndESTMMP + 43*5 + 1, ccEndESTMMP + 43*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_Internal_rhoCp5_Bldgs = (/(cc, cc=ccEndESTMMP + 44*5 + 1, ccEndESTMMP + 44*5 + 5, 1)/)
   INTEGER, DIMENSION(5) :: c_nroom_Bldgs = (ccEndESTMMP + 44*5 + 5 + 1)
   INTEGER, DIMENSION(5) :: c_alb_ibld_Bldgs = (ccEndESTMMP + 44*5 + 5 + 2)
   INTEGER, DIMENSION(5) :: c_em_ibld_Bldgs = (ccEndESTMMP + 44*5 + 5 + 3)
   INTEGER, DIMENSION(5) :: c_CH_iwall_Bldgs = (ccEndESTMMP + 44*5 + 5 + 4)
   INTEGER, DIMENSION(5) :: c_CH_iroof_Bldgs = (ccEndESTMMP + 44*5 + 5 + 5)
   INTEGER, DIMENSION(5) :: c_CH_ibld_Bldgs = (ccEndESTMMP + 44*5 + 5 + 6)

   !Last column number for SurfaceChar array
   INTEGER, PARAMETER :: MaxNCols_c = (ccEndESTMMP + 44*5 + 5 + 6)
   !-----------------------------------------------------------------------------------------------

   ! ---- Set column numbering for ModelOutputData ------------------------------------------------
   ! Applicable to each surface
   INTEGER, PARAMETER :: ccMOD = 32
   INTEGER, DIMENSION(nsurf) :: cMOD_State = (/(cc, cc=ccMOD + 0*nsurf + 1, ccMOD + 0*nsurf + nsurf, 1)/) !Above ground state
   INTEGER, DIMENSION(nsurf) :: cMOD_SoilState = (/(cc, cc=ccMOD + 1*nsurf + 1, ccMOD + 1*nsurf + nsurf, 1)/) !Below ground state (soil store)
   INTEGER, DIMENSION(nsurf) :: cMOD_SnowWaterState = (/(cc, cc=ccMOD + 2*nsurf + 1, ccMOD + 2*nsurf + nsurf, 1)/) !Liquid (melted) water
   INTEGER, DIMENSION(nsurf) :: cMOD_SnowPack = (/(cc, cc=ccMOD + 3*nsurf + 1, ccMOD + 3*nsurf + nsurf, 1)/) !SWE
   INTEGER, DIMENSION(nsurf) :: cMOD_SnowFrac = (/(cc, cc=ccMOD + 4*nsurf + 1, ccMOD + 4*nsurf + nsurf, 1)/) !Snow fraction
   INTEGER, DIMENSION(nsurf) :: cMOD_SnowDens = (/(cc, cc=ccMOD + 5*nsurf + 1, ccMOD + 5*nsurf + nsurf, 1)/) !Snow density

   !Last column number for ModelOutputData array
   INTEGER, PARAMETER :: MaxNCols_cMOD = ccMOD + 5*nsurf + nsurf
   !-----------------------------------------------------------------------------------------------

   ! ---- Set column numbering for ModelDailyState ------------------------------------------------
   ! Applicable to each surface
   INTEGER, PARAMETER :: ccMDS = 30
   INTEGER, DIMENSION(nsurf) :: cMDS_SnowDens = (/(cc, cc=ccMDS + 0*nsurf + 1, ccMDS + 0*nsurf + nsurf, 1)/) !Snow density

   !Last column number for ModelDailyState array
   INTEGER, PARAMETER :: MaxNCols_cMDS = ccMDS + 0*nsurf + nsurf
   !-----------------------------------------------------------------------------------------------

   ! ---- Set column numbering for ESTM_Ts_data input file ===-------------------------------------
   ! HCW 15 June 2016
   INTEGER, PARAMETER :: cTs_iy = 1
   INTEGER, PARAMETER :: cTs_id = 2
   INTEGER, PARAMETER :: cTs_it = 3
   INTEGER, PARAMETER :: cTs_imin = 4
   INTEGER, PARAMETER :: cTs_Tiair = 5
   INTEGER, PARAMETER :: cTs_Tsurf = 6
   INTEGER, PARAMETER :: cTs_Troof = 7
   INTEGER, PARAMETER :: cTs_Troad = 8
   INTEGER, PARAMETER :: cTs_Twall = 9
   INTEGER, PARAMETER :: cTs_Twall_n = 10
   INTEGER, PARAMETER :: cTs_Twall_e = 11
   INTEGER, PARAMETER :: cTs_Twall_s = 12
   INTEGER, PARAMETER :: cTs_Twall_w = 13

END MODULE allocateArray
!==================================================================================================

!==================================================================================================
MODULE Initial

   IMPLICIT NONE

   INTEGER :: FirstYear, & !First year to run (specified in SiteSelect.txt)
              LastYear, & !Last year to run  (specified in SiteSelect.txt)
              FirstGrid, & !First grid to run (as in SiteSelect)
              LastGrid, & !Last grid to run  (as in SiteSelect)
              NumberOfGrids, & !Number of grids
              GridCounter, & !Counter for grids (i.e. from 1 to NumberOfGrids)
              ReadBlocksMetData, & !Number of blocks of met data to read (for each grid, for each year)
              ReadBlocksOrigMetData, & !Number of blocks of original met data to read (for each grid, for each year)
              ReadLinesMetData, & !Number of lines of met data in each block (for each grid)
              ReadLinesOrigMetData, & !Number of lines of original met data in each block (before downscaling)
              ReadLinesOrigESTMData, & !Number of lines of original ESTM data in each block (before downscaling)
              ReadLinesOrigMetDataMax, & !No. lines of original met data in each block (adjusts for last block (equivalent of irMax))
              ReadLinesOrigESTMDataMax, & !No. lines of original ESTM data in each block
              nlinesOrigMetData, & !Number of lines in original met data file
              nlinesOrigESTMData, & !Number of lines in original ESTM data file
              nlinesMetData, & !Number of lines in Met Forcing file
              nlinesESTMdata, & !Number of lines in ESTM Forcing file
              nlinesSiteSelect, & !Number of lines in SUEWS_SiteSelect.txt
              nlinesNonVeg, & !Number of lines in SUEWS_NonVeg.txt
              nlinesVeg, & !Number of lines in SUEWS_Veg.txt
              nlinesWater, & !Number of lines in SUEWS_Water.txt
              nlinesSnow, & !Number of lines in SUEWS_Snow.txt
              nlinesSoil, & !Number of lines in SUEWS_Soil.txt
              nlinesConductance, & !Number of lines in SUEWS_Conductance.txt
              nlinesOHMCoefficients, & !Number of lines in SUEWS_OHMCoefficients.txt
              nlinesESTMCoefficients, & !Number of lines in SUEWS_ESTMCoefficients.txt
              nlinesAnthropogenic, & !Number of lines in SUEWS_AnthropogenicEmission.txt
              nlinesIrrigation, & !Number of lines in SUEWS_Irrigation.txt
              nlinesProfiles, & !Number of lines in SUEWS_Profiles.txt
              nlinesWGWaterDist, & !Number of lines in SUEWS_WGWaterDist.txt
              nlinesBiogen, & !Number of lines in SUEWS_BiogenCO2.txt
              nlines, & !Number of lines in different files
              SkippedLines, & !Number of lines to skip over before reading each block of met data
              SkippedLinesOrig, & !Number of lines to skip over before reading each block of data from original met file
              SkippedLinesOrigESTM, & !Number of lines to skip over before reading each block of data from original ESTM file
              iv5 !Counter for code matching.

END MODULE Initial
!==================================================================================================

!==================================================================================================
MODULE data_in

   IMPLICIT NONE

   CHARACTER(len=90) :: progname = 'SUEWS_V2021a'

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
              BaseTMethod, & ! base temperature method for HDD/CDD calculations used in QF module
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
              StorageHeatMethod, & !OHM (1); QS in met file (2); AnOHM(3); ESTM(4)
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
              SuppressWarnings = 1, & ! Set to 1 to prevent warnings.txt file from being written
              DiagMethod, & !Specify the approach for near surface diagnostic: 0, MOST; 1, RSL; 2, Auto (MOST+RSL)
              Diagnose, & !Set to 1 to get print-out of model progress
              DiagnoseDisagg, & !Set to 1 to get print-out of met forcing disaggregation progress
              !  ncMode, &               !Write output file in netCDF (1) or not (0) , TS, 09 Dec 2016
              !  nRow, &                 !number of rows of checker board layout in the netCDF output, TS, 09 Dec 2016
              !  nCol, &                 !number of columns of checker board layout in the netCDF output, TS, 09 Dec 2016
              DiagnoseDisaggESTM, & !Set to 1 to get print-out of ESTM forcing disaggregation progress
              DiagQN, DiagQS !Set to 1 to print values/components

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
                      EnEF_v_Jkm, & !Heat release per vehicle per meter of travel [J km-1 veh-1]
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
                      LAI_obs, & !LAI for study area provided in met forcing file
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
                                    AH_MIN, & !Minimum anthropogenic heat flux (AnthropHeatMethod = 1)
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

END MODULE data_in
!==================================================================================================

MODULE snowMod
   IMPLICIT NONE

   REAL(KIND(1D0)) :: AdjMeltFact, & !Factor between melt and freezing factors
                      SnowfallCum, & !Cumulative snowfall
                      fwh, & !Weighted freezing water
                      lvS_J_kg, & !Latent heat of sublimation in J/kg
                      mwh, & !Weighted hourly water melt
                      MwStore, & !Meltwater storage
                      PrecipLimit, & !Temperature limit when precipitation occurs as snow
                      PrecipLimitAlb, & !Precipitation limit for albedo change (in mm)
                      Qm, & !Snow melt associated heat flux
                      QmFreez, & !Energy released in freezing of meltwater or surface state
                      QmRain, &
                      qn1_snow, & !Net all-wave radiation of SnowPack
                      qn1_nosnow, & !Same for the snow free surface
                      RadMeltFact, & !Radiation melt factor
                      SnowAlb, & !Snow albedo
                      SnowAlbMin, & !Minimum snow albedo
                      SnowAlbMax, & !Maximum snow albedo
                      SnowDensMin, & !Minimum density of snow
                      SnowDensMax, & !Maximum density of snow
                      SnowLimBldg, & !Snow removal limits for roofs in mm)
                      SnowLimPaved, & !Snow removal limits for paved surfaces in mm)
                      swe, & !Weighted snow water equivalent (in mm)
                      tau_a, & !Time constans related to albedo change
                      tau_f, &
                      tau_r, & !Time constant for density increase.
                      TempMeltFact, & !Temperature melt factor
                      volDay, & !Volume of the melted water per day
                      zf, &
                      WaterHoldCapFrac, & !Water holding capacity factor
                      CRWmin, & !Free water holding capacity of deep SnowPack
                      CRWmax !Free water holding capacity of shallow SnowPack

   REAL(KIND(1D0)), DIMENSION(2) :: SnowRemoval = 0 ! Removal of snow in mm
   REAL(KIND(1D0)), DIMENSION(0:23, 2) :: SnowProf_24hr ! Timing of snow removal (0 or 1) Hourly, WD/WE

   INTEGER :: SnowFractionChoice = 2 !Choice how fraction of snow is calculated

END MODULE snowMod
!===================================================================================

!==================================================================================================
MODULE defaultNotUsed
   IMPLICIT NONE
   REAL(KIND(1D0)) :: notUsed = -55.55, reall, NAN = -999, pNAN = 999
   INTEGER :: notUsedI = -55, ios_out
   INTEGER :: errorChoice, warningChoice !errorChoice/warningChoice defines if problems.txt/warnings.txt is opened for the first time
END MODULE defaultNotUsed
!==================================================================================================

!==================================================================================================
MODULE time
   INTEGER :: iy, & !Year
              id, & !Day of year
              it, & !Hour
              imin, & !Minutes
              isec, & !Seconds
              DLS !day lightsavings =1 + 1h) =0

   REAL(KIND(1D0)) :: dectime !Decimal time
   REAL(KIND(1D0)) :: tstepcount !Count number of timesteps in this day
   INTEGER :: nofDaysThisYear !Based on whether leap year or not
   INTEGER :: dt_since_start ! time since simulation starts [s]

   INTEGER :: iy_prev_t, id_prev_t !Value of iy and id at previous timestep

END MODULE time
!==================================================================================================

!===================================================================================
MODULE mod_grav
   REAL(KIND(1D0)) :: grav = 9.80665 !g - gravity - physics today august 1987
END MODULE mod_grav

!===================================================================================

!===================================================================================
MODULE Thresh
   REAL(KIND(1D0)) :: IPThreshold_mmhr = 10 !Threshold for intense precipitation [mm hr-1]

END MODULE Thresh

!===================================================================================
MODULE gas
   !   press (mb) ea (mb)
   IMPLICIT NONE
   REAL(KIND(1D0)) :: comp = 0.9995
   REAL(KIND(1D0)) :: epsil = 0.62197 !ratio molecular weight of water vapor/dry air (kg/mol/kg/mol)
   REAL(KIND(1D0)) :: epsil_gkg = 621.97 !ratio molecular weight of water vapor/dry air in g/kg
   REAL(KIND(1D0)) :: dry_gas = 8.31451 !Dry gas constant (J/k/mol)
   REAL(KIND(1D0)) :: gas_ct_wat = 461.05 !Gas constant for water (J/kg/K)
   REAL(KIND(1D0)) :: molar = 0.028965 !Dry air molar fraction in kg/mol
   REAL(KIND(1D0)) :: molar_wat_vap = 0.0180153 !Molar fraction of water vapor in kg/mol
   REAL(KIND(1D0)) :: gas_ct_dry = 8.31451/0.028965 !j/kg/k=dry_gas/molar
   REAL(KIND(1D0)) :: gas_ct_wv = 8.31451/0.0180153 !j/kg/kdry_gas/molar_wat_vap
END MODULE gas

!**********************************************
MODULE mod_z
   REAL(KIND(1D0)) :: zzd, & !Active measurement height (meas. height-displac. height)
                      z0m, & !Aerodynamic roughness length
                      zdm, & !Displacement height
                      z0m_in, & !Aerodynamic roughness length set in SiteSelect
                      zdm_in, & !Displacement height set in SiteSelect
                      z !Windspeed height
   REAL(KIND(1E10)) :: z0V !Roughness length for vapour
END MODULE mod_z

!**********************************************
MODULE resist !Variables related surface resistance calculations (P. 1744 in G&O1991)
   IMPLICIT NONE
   REAL(KIND(1D0)) :: th, & !Maximum temperature limit
                      tl, & !Minimum temperature limit
                      Kmax, & !Annual maximum hourly solar radiation
                      g1, g2, g3, g4, & !Fitted parameters related to
                      g5, g6, s1, s2, & !surface res. calculations
                      tc, & !Temperature parameter 1
                      tc2 !Temperature parameter 2
   INTEGER :: gsModel !Choice of gs parameterisation (1 = Ja11, 2 = Wa16)
END MODULE resist

!**********************************************
MODULE moist
   IMPLICIT NONE

   REAL(KIND(1D0)) :: avcp, & !Specific heat capacity
                      dens_dry, & !Dry air density kg m-3
                      avdens, & !Average air density
                      dq, & !Specific humidity deficit
                      Ea_hPa, & !Water vapour pressure in hPa
                      Es_hPa, & !Saturation vapour pressure in hPa
                      lv_J_kg, & !Latent heat of vaporization in [J kg-1]
                      tlv, & !Latent heat of vaporization per timestep [J kg-1 s-1] (tlv=lv_J_kg/tstep_real)
                      psyc_hPa, & !Psychometric constant in hPa
                      psycIce_hPa, & !Psychometric constant in hPa for snow
                      s_Pa, & !Vapour pressure versus temperature slope in Pa
                      s_hpa, & !Vapour pressure versus temperature slope in hPa
                      sIce_hpa, & !Vapour pressure versus temperature slope in hPa above ice/snow
                      vpd_hPa, & !Vapour pressure deficit in hPa
                      vpd_pa, & !Vapour pressure deficit in Pa
                      waterDens = 999.8395 !Density of water in 0 cel deg

END MODULE moist
!**********************************************

MODULE gis_data
   IMPLICIT NONE

   REAL(KIND(1D0)) :: areaunir, & !Unirrigated area
                      areair, & !Irrigated area
                      bldgH, & !Mean building height
                      FAIbldg, & !Frontal area fraction of buildings
                      FAItree, & !Frontal area fraction of trees
                      FAIEveTree, & !Frontal area fraction of evergreen trees
                      FAIDecTree, & !Frontal area fraction of deciduous trees
                      grassfractionirrigated, & !Irrigated grass fraction for LUMPS
                      pavedfractionirrigated, & !Irrigated paved area fraction for LUMPS
                      TreeH, & !Mean tree height
                      EveTreeH, & !Height of evergreen trees
                      DecTreeH, & !Height of deciduous trees
                      treefractionirrigated, & !Irrigated tree fraction for LUMPS
                      veg_fr, & !Vegetation fraction from land area
                      !- For LUMPS - dependent on user choice    & water
                      VegFraction, & ! sum of vegetation -not including water
                      ImpervFraction, & ! sum of surface cover fractions for impervious surfaces
                      PervFraction, & ! sum of surface cover fractions for pervious surfaces
                      NonWaterFraction, & ! sum of surface cover fractions for all except water surfaces
                      areaZh !=(sfr_surf(BldgSurf)+sfr_surf(ConifSurf)+sfr_surf(DecidSurf)) !Total area of buildings and trees

   INTEGER :: idgis, & !Time integers used in the code
              itgis, & !
              Veg_type = 1 !Defines how vegetation is calculated for LUMPS

END MODULE gis_data

!************************************************************
MODULE sues_data
   IMPLICIT NONE

   INTEGER :: tstep, & !Timestep [s] at which the model is run (set in RunControl)
              tstep_prev, & !Timestep [s] of previous timestamp !NB: not used by SUEWS, but by WRF-SUEWS for adaptive time step
              nsh, & !Number of timesteps per hour
              nsd, & !Number of timesteps per day
              nsdorig, & !Number of timesteps per day for original met forcing file
              t_interval, & !Number of seconds in an hour [s] (now set in OverallRunControl)
              Nper, NperESTM ! Number of model time-steps per input resolution (ResolutionFilesIn/Tstep)

   REAL(KIND(1D0)) :: nsh_real, & !nsh cast as a real for use in calculations
                      tstep_real, & !tstep cast as a real for use in calculations
                      Nper_real, NperESTM_real !Nper as real

   REAL(KIND(1D0)) :: halftimestep !In decimal time based on interval

   !Options for model setup (switches, etc) mainly set in RunControl
   INTEGER :: StabilityMethod, & !Defines stability functions used (set in RunControl)
              RoughLenHeatMethod !Defines method for calculating roughness length for heat (set in RunControl)

   INTEGER :: in
   INTEGER :: is !Integer to count over surface types

   !These are variables which currently have been removed from SuesInput.nml
   INTEGER :: AerodynamicResistanceMethod = 2 !The method used to calculate aerodynamic resistance

   INTEGER :: Ie_start, & !Starting time of water use (DOY)
              Ie_end !Ending time of water use (DOY)

   REAL(KIND(1D0)), DIMENSION(2) :: SurplusEvap !Surplus for evaporation in 5 min timestep
   ! sg -- need to determine size

   !Variables listed in SuesInput.nml
   REAL(KIND(1D0)) :: FlowChange, & !Difference between the input and output flow in the water body
                      PipeCapacity, & !Capacity of pipes to transfer water
                      RunoffToWater, & !Fraction of surface runoff going to water body
                      SmCap, & !Volumetric/gravimetric soil moisture capacity
                      SoilDensity, & !Bulk density of soil
                      SoilDepthMeas, & !Soil depth of the measured soil moisture
                      SoilRocks, & !Fraction of rocks in soil
                      SurfaceArea, & !Surface area of the study area [m2]
                      SurfaceArea_ha, & !Surface area of the study area [ha]
                      WaterBodyType, & !If water body type is pond/lake (=1) or river (=2)
                      WaterStorCap, & !Capacity of water body when surface is wet
                      WUAreaEveTr_m2, & !Water use area (evergreen trees) [m2]
                      WUAreaDecTr_m2, & !Water use area (deciduous trees) [m2]
                      WUAreaGrass_m2, & !Water use area (grass) [m2]
                      WUAreaTotal_m2, & !Water use area (total) [m2]
                      wu_EveTr, & !Water use for evergreen trees/shrubs [mm]
                      wu_DecTr, & !Water use for deciduous trees/shrubs [mm]
                      wu_Grass !Water use for grass [mm]

   !Other related to SUES
   REAL(KIND(1D0)) :: AdditionalWater, & !Water flow from other grids
                      ch_per_interval, & !Change in state per interval
                      chSnow_per_interval, & !Change in snow state per interval
                      dI_dt, & !Water flow between two stores
                      dr_per_interval, & !Drainage per interval
                      ev_per_interval, & !Evaporation per interval
                      surf_chang_per_tstep, & !Change in surface state per timestep [mm] (for whole surface)
                      tot_chang_per_tstep, & !Change in surface and SoilState per timestep [mm] (for whole surface)
                      NWstate_per_tstep, & !State per timestep [mm] (for whole surface, excluding water body)
                      state_per_tstep, & !State per timestep [mm] (for whole surface)
                      drain_per_tstep, & !Drainage per timestep [mm] (for whole surface, excluding water body)
                      runoff_per_tstep, & !Runoff per timestep [mm] (for whole surface)
                      runoffSoil_per_tstep, & !Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
                      ev_per_tstep, & !Evaporation per timestep [mm] (for whole surface)
                      qe_per_tstep, & !QE [W m-2] (for whole surface)
                      p_mm, & !Inputs to surface water balance
                      pin, & !Rain per time interval
                      FAI, & !Areally weighted frontal area fraction
                      rb, & !Boundary layer resistance
                      ! Water leaving each grid for grid-to-grid connectivity
                      runoffAGimpervious, & !Above ground runoff from impervious surface [mm] for whole surface area
                      runoffAGveg, & !Above ground runoff from vegetated surfaces [mm] for whole surface area
                      runoffWaterBody, & !Above ground runoff from water surface [mm] for whole surface area
                      runoffPipes, & !Runoff in pipes [mm] for whole surface area
                      runoffAGimpervious_m3, & !Volume of above ground runoff from impervious surface [m3]
                      runoffAGveg_m3, & !Volume of above ground runoff from vegetated surfaces [m3]
                      runoffWaterBody_m3, & !Volume of above ground runoff from water surface [m3]
                      runoffPipes_m3, & !Volume of runoff in pipes [m3]
                      runoff_per_interval, & ! Total water transported to each grid for grid-to-grid connectivity
                      addImpervious, & !Water from impervious surfaces of other grids [mm] for whole surface area
                      addVeg, & !Water from vegetated surfaces of other grids [mm] for whole surface area
                      addWaterbody, & !Water from water surface of other grids [mm] for whole surface area
                      addPipes, & !Water in pipes from other grids [mm] for whole surface area
                      runoffSoil_per_interval, &
                      qe_per_interval, & !latent heat per interval
                      SoilMoistCap, &
                      SoilState, & !Area-averaged soil moisture [mm] for whole surface
                      st_per_interval, & !Surface state per interval
                      surplusWaterBody, & !Extra runoff that goes to water body [mm] as specified by RunoffToWater
                      tlv_sub, &
                      overuse = 0, &
                      Zh !Areally weighted roughness element height

   !Calculation of u*,stability and aerodynamic resistance
   REAL(KIND(1D0)) :: H, & !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity
                      l_mod, & !Monin-Obukhov length (either measured or modelled)
                      psim, & !Stability function of momentum
                      psih, & !Stability function of heat
                      RA, & !Aerodynamic resistance
                      RAsnow, & !Aerodynamic resistance over snow
                      TStar, & !T*
                      UStar, & !Friction velocity
                      z0_gis !Roughness length for momentum from gis input file

   !Surface resistance related variables
   REAL(KIND(1D0)) :: resistsurf, & !Surface resistance
                      gdq, & !G(dq)
                      qnm, & !QMAX/(QMAX+G2)
                      gq, & !G(Q*)
                      gtemp, & !G(T)
                      gl, & !G(LAI)
                      sdp, & !S1/G6+S2
                      smd, & !Soil moisture deficit of the soil surface layer
                      vsmd, & !Soil moisture deficit for vegetated surfaces only (QUESTION: what about BSoil?)
                      gs, & !G(Soil moisture deficit)
                      gsc, & !Surface Layer Conductance
                      rss !Surface resistance after wet/partially wet adjustment

   !SUES latent heat flux related variables
   REAL(KIND(1D0)) :: vdrc, & !Second term up in calculation of E
                      numPM, & !Numerator of PM equation
                      sp, & !Term in calculation of E
                      sae, & !Same
                      ev, & !Evaporation
                      rst, & !Flag in SUEWS_Evap (gets set to 1 if surface dry; 0 if surface wet)
                      qeph, & !Latent heat flux (W m^-2)
                      qeOut !Latent heat flux [W m-2]

   REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qhforCBL, qeforCBL ! Stores previous timestep qh and qe for CBL model. Added by HCW 21 Mar 2017
   INTEGER :: qh_choice ! selection of qh use to drive CBL growth 1=Suews 2=lumps 3=obs

   !Water use related variables
   REAL(KIND(1D0)) :: ext_wu, & !External water use for the model timestep [mm] (over whole study area)
                      Faut, & !Fraction of irrigated area using automatic irrigation
                      H_maintain, & ! ponding water depth to maintain [mm] (over whole study area)
                      int_wu, & !Internal water use for the model timestep [mm] (over whole study area)
                      IrrFracPaved, & !Fraction of paved which are irrigated
                      IrrFracBldgs, & !Fraction of buildings which are irrigated
                      IrrFracEveTr, & !Fraction of evergreen trees which are irrigated
                      IrrFracDecTr, & !Fraction of deciduous trees which are irrigated
                      IrrFracGrass, & !Fraction of grass which is irrigated
                      IrrFracBSoil, & !Fraction of bare soil which is irrigated
                      IrrFracWater, & !Fraction of water which is irrigated
                      InternalWaterUse_h !Internal water use [mm h-1]

   ! 7 - number of days in week
   REAL(KIND(1D0)), DIMENSION(7) :: DayWatPer, & !% of houses following daily water
                                    DayWat !Days of watering allowed
   ! REAL(KIND(1d0)), DIMENSION(0:23, 2):: WUProfM_24hr, &   !Hourly profiles for water use (manual irrigation)
   !                                       WUProfA_24hr   !Hourly profiles for water use (automatic irrigation)

   REAL(KIND(1D0)), DIMENSION(3) :: Ie_a, Ie_m !Coefficients for automatic and manual irrigation models

END MODULE sues_data

!**********************************************
!===================================================================================
MODULE VegPhenogy
   IMPLICIT NONE
   REAL(KIND(1D0)) :: VegPhenLumps, deltaLAI
END MODULE VegPhenogy

MODULE filename
   CHARACTER(len=90) :: smithfile !file for NARP
END MODULE filename

MODULE InitialCond

   REAL(KIND(1D0)) :: LAIinitialEveTr, &
                      LAIinitialDecTr, &
                      LAIinitialGrass, &
                      porosity0, &
                      DecidCap0, &
                      albDecTr0, &
                      albEveTr0, &
                      albGrass0, &
                      Temp_C0, &
                      GDD_1_0, &
                      GDD_2_0, &
                      SoilStorePavedState, &
                      SoilStoreBldgsState, &
                      SoilStoreEveTrState, &
                      SoilStoreDecTrstate, &
                      SoilStoreGrassState, &
                      SoilStoreBSoilState, &
                      SnowWaterPavedState, &
                      SnowWaterBldgsState, &
                      SnowWaterEveTrState, &
                      SnowWaterDecTrState, &
                      SnowWaterGrassState, &
                      SnowWaterBSoilState, &
                      SnowWaterWaterstate, &
                      SnowPackPaved, &
                      SnowPackBldgs, &
                      SnowPackEveTr, &
                      SnowPackDecTr, &
                      SnowPackGrass, &
                      SnowPackBSoil, &
                      SnowPackWater, &
                      SnowAlb0 !,&
   ! BoInit !initial Bo for AnOHM, TS 13 Jul 2016 ! removed, TS 30 Jan 2018

   INTEGER :: ID_Prev

END MODULE InitialCond

!-------------------------------------------------
!New modules for the column numbers

!-------------------------------------------------------------------------
MODULE ColNamesModelDailyState

   IMPLICIT NONE

   !========== Columns for ModelDailyState array =========================

   INTEGER :: cMDS_id_prev = 3, &
              cMDS_HDD1 = 4, &
              cMDS_HDD2 = 5, &
              cMDS_TempC = 6, &
              cMDS_TempCRM = 7, &
              cMDS_Precip = 8, &
              cMDS_DaysSinceRain = 9, &
              cMDS_TempCOld1 = 10, &
              cMDS_TempCOld2 = 11, &
              cMDS_TempCOld3 = 12, &
              cMDS_GDDMin = 13, &
              cMDS_GDDMax = 14, &
              cMDS_GDD1_0 = 15, &
              cMDS_GDD2_0 = 16, &
              cMDS_LAIInitialEveTr = 17, &
              cMDS_LAIInitialDecTr = 18, &
              cMDS_LAIInitialGrass = 19, &
              cMDS_porosity = 20, &
              cMDS_albEveTr = 21, &
              cMDS_albDecTr = 22, &
              cMDS_albGrass = 23, &
              cMDS_DecidCap = 24, &
              cMDS_SnowfallCum = 25, &
              cMDS_LAIEveTr = 26, &
              cMDS_LAIDecTr = 27, &
              cMDS_LAIGrass = 28, &
              cMDS_SnowAlb = 29, &
              cMDS_BoRatio = 30, & ! noontime Bowen ratio, added by TS
              cMDS_a1AnOHM = 31, & ! a1 of AnOHM, added by TS
              cMDS_a2AnOHM = 32, & ! a2 of AnOHM, added by TS
              cMDS_a3AnOHM = 33 ! a3 of AnOHM, added by TS

END MODULE ColNamesModelDailyState

!-------------------------------------------------------------------------
MODULE ColNamesInputFiles

   IMPLICIT NONE

   INTEGER :: ccc !Column counter

   ! Column names and numbers must match the input files

   !========== Columns for SUEWS_SiteSelect.txt ==========================
   ! Columns 1:97 are the same for SurfaceChar
   INTEGER :: c_Grid = 1, &
              c_Year = 2, &
              c_StartDLS = 3, &
              c_EndDLS = 4, &
              ! Site info
              c_lat = 5, &
              c_lng = 6, &
              c_tz = 7, &
              c_Area = 8, &
              c_Alt = 9, &
              c_z = 10, &
              ! Time info
              c_id = 11, &
              c_it = 12, &
              c_imin = 13, &
              ! Surface fractions
              c_FrPaved = 14, &
              c_FrBldgs = 15, &
              c_FrEveTr = 16, &
              c_FrDecTr = 17, &
              c_FrGrass = 18, &
              c_FrBSoil = 19, &
              c_FrWater = 20, &
              ! Irrigated fractions
              c_IrrPavedFrac = 21, &
              c_IrrBldgsFrac = 22, &
              c_IrrEveTrFrac = 23, &
              c_IrrDecTrFrac = 24, &
              c_IrrGrassFrac = 25, &
              c_IrrBSoilFrac = 26, &
              c_IrrWaterFrac = 27, &
              ! Height information
              c_HBldgs = 28, &
              c_HEveTr = 29, &
              c_HDecTr = 30, &
              c_z0m = 31, &
              c_zdm = 32, &
              c_FAIBldgs = 33, &
              c_FAIEveTr = 34, &
              c_FAIDecTr = 35, &
              ! Population
              c_PopDensDay = 36, &
              c_PopDensNight = 37, &
              c_TrafficRate_WD = 38, & ! Mean traffic rate in modelled area [veh km m-2 s-1] Weekday
              c_TrafficRate_WE = 39, & ! Mean traffic rate in modelled area [veh km m-2 s-1] Weekend
              c_QF0_BEU_WD = 40, & ! Building energy use for modelled area [W m-2] - QUESTION: could change units?
              c_QF0_BEU_WE = 41, &
              ! Codes for different surfaces
              c_PavedCode = 42, & ! Links characteristics in SUEWS_NonVeg.txt
              c_BldgsCode = 43, & ! Links characteristics in SUEWS_NonVeg.txt
              c_EveTrCode = 44, & ! Links characteristics in SUEWS_Veg.txt
              c_DecTrCode = 45, & ! Links characteristics in SUEWS_Veg.txt
              c_GrassCode = 46, & ! Links characteristics in SUEWS_Veg.txt
              c_BSoilCode = 47, & ! Links characteristics in SUEWS_Veg.txt
              c_WaterCode = 48, & ! Links characteristics in SUEWS_Water.txt
              ! LUMPS info
              c_LUMPSDr = 49, &
              c_LUMPSCover = 50, &
              c_LUMPSMaxRes = 51, &
              ! NARP info
              c_NARPTrans = 52, &
              ! Code for conductances
              c_CondCode = 53, & ! Links characteristics in SUEWS_Conductance.txt
              ! Code for snow
              c_SnowCode = 54, & ! Links characteristics in SUEWS_Snow.txt
              ! Codes for human impacts on energy, water and snow
              c_SnowProfWD = 55, & ! Snow-clearing profile in SUEWS_Profile.txt (weekdays)
              c_SnowProfWE = 56, & ! Snow-clearing profile in SUEWS_Profile.txt (weekends)
              c_QFCode = 57, & ! Links anthropogenic heat info in SUEWS_AnthropogenicEmission.txt
              c_IrrCode = 58, & ! Links irrigation info in SUEWS_Irrigation.txt
              c_WProfManuWD = 59, & ! Links to water-use profile in SUEWS_Profile.txt (manual irrigation, weekdays)
              c_WProfManuWE = 60, & ! Links to water-use profile in SUEWS_Profile.txt (manual irrigation, weekends)
              c_WProfAutoWD = 61, & ! Links to water-use profile in SUEWS_Profile.txt (automatic irrigation, weekdays)
              c_WProfAutoWE = 62, & ! Links to water-use profile in SUEWS_Profile.txt (automatic irrigation, weekends)
              ! Flow information
              c_FlowChange = 63, & ! Difference in input & output flows for water surface
              c_RunoffToWater = 64, & ! Fraction of above-ground runoff flowing to water surface
              c_PipeCapacity = 65, & ! Pipe capacity [mm]
              ! Runoff (to 8 adjacent grids)
              c_GridConnection1of8 = 66, &
              c_Fraction1of8 = 67, &
              c_GridConnection2of8 = 68, &
              c_Fraction2of8 = 69, &
              c_GridConnection3of8 = 70, &
              c_Fraction3of8 = 71, &
              c_GridConnection4of8 = 72, &
              c_Fraction4of8 = 73, &
              c_GridConnection5of8 = 74, &
              c_Fraction5of8 = 75, &
              c_GridConnection6of8 = 76, &
              c_Fraction6of8 = 77, &
              c_GridConnection7of8 = 78, &
              c_Fraction7of8 = 79, &
              c_GridConnection8of8 = 80, &
              c_Fraction8of8 = 81, &
              ! Runoff within grid (for each surface type)
              c_WGPavedCode = 82, & ! Links to SUEWS_WaterDistibuteWithinGrid.txt
              c_WGBldgsCode = 83, & ! Links to SUEWS_WaterDistibuteWithinGrid.txt
              c_WGEveTrCode = 84, & ! Links to SUEWS_WaterDistibuteWithinGrid.txt
              c_WGDecTrCode = 85, & ! Links to SUEWS_WaterDistibuteWithinGrid.txt
              c_WGGrassCode = 86, & ! Links to SUEWS_WaterDistibuteWithinGrid.txt
              c_WGBSoilCode = 87, & ! Links to SUEWS_WaterDistibuteWithinGrid.txt
              c_WGWaterCode = 88, & ! Links to SUEWS_WaterDistibuteWithinGrid.txt
              ! Additional info for ESTM
              c_AreaWall = 89 ! Wall surface fraction (Awall/Agridcell)

   INTEGER, DIMENSION(3) :: c_Fr_ESTMClass_Paved = (/(ccc, ccc=90, 92, 1)/) ! Fraction of Paved surface with ESTM Class 1-3
   INTEGER, DIMENSION(3) :: c_Code_ESTMClass_Paved = (/(ccc, ccc=93, 95, 1)/) ! Code for Paved surface ESTM Class 1-3
   INTEGER, DIMENSION(5) :: c_Fr_ESTMClass_Bldgs = (/(ccc, ccc=96, 100, 1)/) ! Fraction of Bldgs surface with ESTM Class 1-5
   INTEGER, DIMENSION(5) :: c_Code_ESTMClass_Bldgs = (/(ccc, ccc=101, 105, 1)/) ! Code for Bldgs surface ESTM Class 1-5

   !========== Columns for SUEWS_NonVeg.txt ==========================
   INTEGER :: ci_Code = 1, &
              ci_AlbMin = 2, &
              ci_AlbMax = 3, &
              ci_Emis = 4, &
              ci_StorMin = 5, &
              ci_StorMax = 6, &
              ci_WetThresh = 7, &
              ci_StateLimit = 8, &
              ci_DrEq = 9, &
              ci_DrCoef1 = 10, &
              ci_DrCoef2 = 11, &
              ci_SoilTCode = 12, &
              ci_SnowLimPat = 13, &
              ci_SnowLimRem = 14, &
              ci_OHMCode_SWet = 15, &
              ci_OHMCode_SDry = 16, &
              ci_OHMCode_WWet = 17, &
              ci_OHMCode_WDry = 18, &
              ci_OHMThresh_SW = 19, &
              ci_OHMThresh_WD = 20, &
              ci_ESTMCode = 21, & ! ESTM code for each surface (if 0 use codes in SiteSelect instead)
              ci_CpAnOHM = 22, & ! heat capacity, added by TS AnOHM
              ci_KkAnOHM = 23, & ! heat conductivity, added by TS AnOHM
              ci_ChAnOHM = 24 ! bulk transfer coef., added by TS AnOHM

   !========== Columns for SUEWS_Veg.txt ============================
   INTEGER :: cp_Code = 1, &
              cp_AlbMin = 2, &
              cp_AlbMax = 3, &
              cp_Emis = 4, &
              cp_StorMin = 5, &
              cp_StorMax = 6, &
              cp_WetThresh = 7, &
              cp_StateLimit = 8, &
              cp_DrEq = 9, &
              cp_DrCoef1 = 10, &
              cp_DrCoef2 = 11, &
              cp_SoilTCode = 12, &
              cp_SnowLimPat = 13, &
              cp_BaseT = 14, &
              cp_BaseTe = 15, &
              cp_GDDFull = 16, &
              cp_SDDFull = 17, &
              cp_LAIMin = 18, &
              cp_LAIMax = 19, &
              cp_PorosityMin = 20, &
              cp_PorosityMax = 21, &
              cp_GsMax = 22, &
              cp_LAIEq = 23, &
              cp_LeafGP1 = 24, &
              cp_LeafGP2 = 25, &
              cp_LeafOP1 = 26, &
              cp_LeafOP2 = 27, &
              cp_OHMCode_SWet = 28, &
              cp_OHMCode_SDry = 29, &
              cp_OHMCode_WWet = 30, &
              cp_OHMCode_WDry = 31, &
              cp_OHMThresh_SW = 32, &
              cp_OHMThresh_WD = 33, &
              cp_ESTMCode = 34, &
              cp_CpAnOHM = 35, & ! heat capacity, added by TS AnOHM
              cp_KkAnOHM = 36, & ! heat conductivity, added by TS AnOHM
              cp_ChAnOHM = 37, & ! bulk transfer coef., added by TS AnOHM
              cp_BiogenCO2Code = 38

   !========== Columns for SUEWS_Water.txt ===============================
   INTEGER :: cw_Code = 1, &
              cw_AlbMin = 2, &
              cw_AlbMax = 3, &
              cw_Emis = 4, &
              cw_StorMin = 5, &
              cw_StorMax = 6, &
              cw_WetThresh = 7, &
              cw_StateLimit = 8, &
              cw_WaterDepth = 9, &
              cw_DrEq = 10, &
              cw_DrCoef1 = 11, &
              cw_DrCoef2 = 12, &
              cw_OHMCode_SWet = 13, &
              cw_OHMCode_SDry = 14, &
              cw_OHMCode_WWet = 15, &
              cw_OHMCode_WDry = 16, &
              cw_OHMThresh_SW = 17, &
              cw_OHMThresh_WD = 18, &
              cw_ESTMCode = 19, &
              cw_CpAnOHM = 20, & ! heat capacity, added by TS AnOHM
              cw_KkAnOHM = 21, & ! heat conductivity, added by TS AnOHM
              cw_ChAnOHM = 22 ! bulk transfer coef., added by TS AnOHM

   !========== Columns for SUEWS_Snow.txt ================================
   INTEGER :: cs_Code = 1, &
              cs_SnowRMFactor = 2, &
              cs_SnowTMFactor = 3, &
              cs_SnowAlbMin = 4, &
              cs_SnowAlbMax = 5, &
              cs_SnowEmis = 6, &
              cs_Snowtau_a = 7, &
              cs_Snowtau_f = 8, &
              cs_SnowPLimAlb = 9, &
              cs_SnowSDMin = 10, &
              cs_SnowSDMax = 11, &
              cs_Snowtau_r = 12, &
              cs_SnowCRWMin = 13, &
              cs_SnowCRWMax = 14, &
              cs_SnowPLimSnow = 15, &
              cs_OHMCode_SWet = 16, &
              cs_OHMCode_SDry = 17, &
              cs_OHMCode_WWet = 18, &
              cs_OHMCode_WDry = 19, &
              cs_OHMThresh_SW = 20, &
              cs_OHMThresh_WD = 21, &
              cs_ESTMCode = 22, &
              cs_CpAnOHM = 23, & ! heat capacity, added by TS
              cs_KkAnOHM = 24, & ! heat conductivity, added by TS
              cs_ChAnOHM = 25 ! bulk transfer coef., added by TS

   !========== Columns for SUEWS_Soil.txt ================================
   INTEGER :: cSo_Code = 1, &
              cSo_SoilDepth = 2, &
              cSo_SoilStCap = 3, &
              cSo_KSat = 4, &
              cSo_SoilDens = 5, &
              cSo_SoilInfRate = 6, &
              cSo_ObsSMDepth = 7, &
              cSo_ObsSMMax = 8, &
              cSo_ObsSNRFrac = 9

   !========== Columns for SUEWS_Conductance.txt =========================
   INTEGER :: cc_Code = 1, &
              cc_GsG1 = 2, &
              cc_GsG2 = 3, &
              cc_GsG3 = 4, &
              cc_GsG4 = 5, &
              cc_GsG5 = 6, &
              cc_GsG6 = 7, &
              cc_GsTH = 8, &
              cc_GsTL = 9, &
              cc_GsS1 = 10, &
              cc_GsS2 = 11, &
              cc_GsKmax = 12, &
              cc_gsModel = 13 !Options for surface conductance calculation (1 - Ja11, 2 - Wa16)

   !========== Columns for SUEWS_OHMCoefficients.txt =====================
   INTEGER :: cO_Code = 1, &
              cO_a1 = 2, &
              cO_a2 = 3, &
              cO_a3 = 4

   !========== Columns for SUEWS_ESTMCoefficients.txt =====================!   ! S.O. 04 Feb 2016
   INTEGER :: cE_Code = 1, &
              cE_Surf_thick1 = 2, & !Characteristics for 5x roof/surface layers
              cE_Surf_k1 = 3, &
              cE_Surf_rhoCp1 = 4, &
              cE_Surf_thick2 = 5, &
              cE_Surf_k2 = 6, &
              cE_Surf_rhoCp2 = 7, &
              cE_Surf_thick3 = 8, &
              cE_Surf_k3 = 9, &
              cE_Surf_rhoCp3 = 10, &
              cE_Surf_thick4 = 11, &
              cE_Surf_k4 = 12, &
              cE_Surf_rhoCp4 = 13, &
              cE_Surf_thick5 = 14, &
              cE_Surf_k5 = 15, &
              cE_Surf_rhoCp5 = 16, &
              cE_Wall_thick1 = 17, & ! Characteristics for 5x external wall layers (used for Bldgs surfaces only)
              cE_Wall_k1 = 18, &
              cE_Wall_rhoCp1 = 19, &
              cE_Wall_thick2 = 20, &
              cE_Wall_k2 = 21, &
              cE_Wall_rhoCp2 = 22, &
              cE_Wall_thick3 = 23, &
              cE_Wall_k3 = 24, &
              cE_Wall_rhoCp3 = 25, &
              cE_Wall_thick4 = 26, &
              cE_Wall_k4 = 27, &
              cE_Wall_rhoCp4 = 28, &
              cE_Wall_thick5 = 29, &
              cE_Wall_k5 = 30, &
              cE_Wall_rhoCp5 = 31, &
              cE_Internal_thick1 = 32, & ! Characteristics for 5x internal wall layers (used for Bldgs surfaces only)
              cE_Internal_k1 = 33, &
              cE_Internal_rhoCp1 = 34, &
              cE_Internal_thick2 = 35, &
              cE_Internal_k2 = 36, &
              cE_Internal_rhoCp2 = 37, &
              cE_Internal_thick3 = 38, &
              cE_Internal_k3 = 39, &
              cE_Internal_rhoCp3 = 40, &
              cE_Internal_thick4 = 41, &
              cE_Internal_k4 = 42, &
              cE_Internal_rhoCp4 = 43, &
              cE_Internal_thick5 = 44, &
              cE_Internal_k5 = 45, &
              cE_Internal_rhoCp5 = 46, &
              cE_nroom = 47, &
              cE_alb_ibld = 48, &
              cE_em_ibld = 49, &
              cE_CH_iwall = 50, &
              cE_CH_iroof = 51, &
              cE_CH_ibld = 52

   !========== Columns for SUEWS_AnthropogenicEmission.txt ===================
   INTEGER :: cA_Code = 1, &
              cA_BaseT_HC = 2, &
              cA_QF_A1 = 3, & !Weekday
              cA_QF_B1 = 4, & !Weekday
              cA_QF_C1 = 5, & !Weekday
              cA_QF_A2 = 6, & !Weekend
              cA_QF_B2 = 7, & !Weekend
              cA_QF_C2 = 8, & !Weekend
              cA_AHMin_WD = 9, & !Weekday
              cA_AHMin_WE = 10, & !Weekend
              cA_AHSlopeHeating_WD = 11, & !Weekday
              cA_AHSlopeHeating_WE = 12, & !Weekend
              cA_AHSlopeCooling_WD = 13, & !Weekday
              cA_AHSlopeCooling_WE = 14, & !Weekend
              cA_TCriticHeating_WD = 15, & !Weekday
              cA_TCriticHeating_WE = 16, & !Weekend
              cA_TCriticCooling_WD = 17, & !Weekday
              cA_TCriticCooling_WE = 18, & !Weekend
              cA_EnProfWD = 19, & !Weekday
              cA_EnProfWE = 20, & !Weekday
              cA_CO2mWD = 21, & !Weekday
              cA_CO2mWE = 22, & !Weekend
              cA_TraffProfWD = 23, & !Weekday
              cA_TraffProfWE = 24, & !Weekend
              cA_PopProfWD = 25, & !Weekday
              cA_PopProfWE = 26, & !Weekend
              cA_MinQFMetab = 27, &
              cA_MaxQFMetab = 28, &
              cA_MinFCMetab = 29, &
              cA_MaxFCMetab = 30, &
              cA_FrPDDwe = 31, &
              cA_FrFossilFuel_Heat = 32, &
              cA_FrFossilFuel_NonHeat = 33, &
              cA_EF_umolCO2perJ = 34, &
              cA_EnEF_v_Jkm = 35, &
              cA_FcEF_v_kgkmWD = 36, &
              cA_FcEF_v_kgkmWE = 37, &
              cA_CO2PointSource = 38, &
              cA_TrafficUnits = 39

   !========== Columns for SUEWS_Irrigation.txt ==========================

   INTEGER :: cIr_Code = 1, &
              cIr_IeStart = 2, &
              cIr_IeEnd = 3, &
              cIr_IntWU = 4, &
              cIr_Faut = 5, &
              cIr_H_maintain = 6, &
              cIr_Ie_a1 = 7, &
              cIr_Ie_a2 = 8, &
              cIr_Ie_a3 = 9, &
              cIr_Ie_m1 = 10, &
              cIr_Ie_m2 = 11, &
              cIr_Ie_m3 = 12, &
              cIr_DayWat1 = 13, &
              cIr_DayWat2 = 14, &
              cIr_DayWat3 = 15, &
              cIr_DayWat4 = 16, &
              cIr_DayWat5 = 17, &
              cIr_DayWat6 = 18, &
              cIr_DayWat7 = 19, &
              cIr_DayWatPer1 = 20, &
              cIr_DayWatPer2 = 21, &
              cIr_DayWatPer3 = 22, &
              cIr_DayWatPer4 = 23, &
              cIr_DayWatPer5 = 24, &
              cIr_DayWatPer6 = 25, &
              cIr_DayWatPer7 = 26
   !========== Columns for SUEWS_Profile.txt =============================

   INTEGER :: cc !Column counter

   INTEGER :: cPr_Code = 1
   INTEGER, DIMENSION(24) :: cPr_Hours = (/(cc, cc=2, 25, 1)/) ! Hourly profile data

   !========== Columns for SUEWS_WithinGridWaterDist.txt =================

   INTEGER :: cWG_Code = 1, &
              cWG_ToPaved = 2, &
              cWG_ToBldgs = 3, &
              cWG_ToEveTr = 4, &
              cWG_ToDecTr = 5, &
              cWG_ToGrass = 6, &
              cWG_ToBSoil = 7, &
              cWG_ToWater = 8, &
              cWG_ToRunoff = 9, &
              cWG_ToSoilStore = 10

   !========== Columns for SUEWS_BiogenCO2.txt ===================
   INTEGER :: cB_Code = 1, &
              cB_alpha = 2, &
              cB_beta = 3, &
              cB_theta = 4, &
              cB_alpha_enh = 5, &
              cB_beta_enh = 6, &
              cB_resp_a = 7, &
              cB_resp_b = 8, &
              cB_min_r = 9

END MODULE ColNamesInputFiles

!----------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------
MODULE WhereWhen
   ! Stores grid and datetime info

   INTEGER :: GridID !Grid number (as specified in SUEWS_SiteSelect.txt)
   CHARACTER(LEN=10) :: GridID_text !Grid number as a text string
   CHARACTER(LEN=15) :: datetime ! YYYY DOY HH MM

END MODULE WhereWhen

!----------------------------------------------------------------------------------
MODULE MathConstants

   REAL(KIND(1D0)), PARAMETER :: pi = 3.14159265359
   REAL(KIND(1D0)), PARAMETER :: dtr = 0.0174532925, rtd = 57.2957795

END MODULE MathConstants

!----------------------------------------------------------------------------------
MODULE PhysConstants

   REAL(KIND(1D0)), PARAMETER :: C2K = 273.15 !Celsius to Kelvin
   REAL(KIND(1D0)), PARAMETER :: SBConst = 5.67051E-8 !Stefan Boltzmann constant [W m-2 K-4]
   REAL(KIND(1D0)), PARAMETER :: JtoumolPAR = 4.6 ! Convert PAR from W m-2 to umol m-2 s-1
   REAL(KIND(1D0)), PARAMETER :: KdntoPAR = 0.46 ! Conversion from Kdn to PAR, originally from Tsubo and Walker (2005), used in Bellucco et al. (2017)

END MODULE PhysConstants
