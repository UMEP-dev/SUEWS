MODULE modulestebbsprecision

   USE ISO_FORTRAN_ENV, ONLY: REAL64

   IMPLICIT NONE

   INTEGER, PARAMETER :: rprc = REAL64

END MODULE

MODULE modulestebbs

   USE modulestebbsprecision

   REAL(rprc), PARAMETER :: sigma = 5.670E-8

   INTEGER, SAVE :: flgtimecheck = 1
   INTEGER :: resolution
   INTEGER :: time_st, time_ed, count_p_sec, count_max ! Time check

   INTEGER :: nbtype
   CHARACTER(len=256), ALLOCATABLE, DIMENSION(:) :: fnmls, cases
   TYPE :: LBM
      CHARACTER(len=256) :: &
         BuildingType, &
         BuildingName, &
         fnmlLBM, &
         CASE
      INTEGER :: idLBM
      INTEGER :: flginit = 0
      INTEGER :: appliance_totalnumber
      REAL(rprc) :: &
         Qtotal_heating, &
         Qtotal_cooling, &
         Qmetabolic_sensible, &
         Qmetabolic_latent, &
         Qtotal_water_tank, &
         qhwtDrain, &
         ratio_window_wall, &
         Afootprint, &
         height_building, &
         wallExternalArea, &
         ratioInternalVolume, &
         thickness_wallroof, &
         thickness_groundfloor, &
         depth_ground, &
         thickness_window, &
         conv_coeff_intwallroof, &
         conv_coeff_indoormass, &
         conv_coeff_intgroundfloor, &
         conv_coeff_intwindow, &
         conv_coeff_extwallroof, &
         conv_coeff_extwindow, &
         conductivity_wallroof, &
         conductivity_groundfloor, &
         conductivity_window, &
         conductivity_ground, &
         density_wallroof, &
         weighting_factor_heatcapacity_wallroof, &
         density_groundfloor, &
         density_window, &
         density_indoormass, &
         density_air_ind, &
         cp_wallroof, &
         cp_groundfloor, &
         cp_window, &
         cp_indoormass, &
         cp_air_ind, &
         emissivity_extwallroof, &
         emissivity_intwallroof, &
         emissivity_indoormass, &
         emissivity_extwindow, &
         emissivity_intwindow, &
         windowTransmissivity, &
         windowAbsorbtivity, &
         windowReflectivity, &
         wallTransmisivity, &
         wallAbsorbtivity, &
         wallReflectivity, &
         BVF_extwall, &
         GVF_extwall, &
         SVF_extwall, &
         occupants, &
         metabolic_rate, &
         ratio_metabolic_latent_sensible, &
         appliance_power_rating, &
         appliance_usage_factor, &
         maxheatingpower_air, &
         heating_efficiency_air, &
         maxcoolingpower_air, &
         coeff_performance_cooling, &
         Vair_ind, &
         ventilation_rate, &
         Awallroof, &
         Vwallroof, &
         Vgroundfloor, &
         Awindow, &
         Vwindow, &
         Vindoormass, &
         Aindoormass, &
         Tair_ind, &
         Tindoormass, &
         Tintwallroof, &
         Textwallroof, &
         Tintwindow, &
         Textwindow, &
         Tintgroundfloor, &
         Textgroundfloor, &
         Twater_tank, &
         Tintwall_tank, &
         Textwall_tank, &
         thickness_tankwall, &
         Tincomingwater_tank, &
         Vwater_tank, &
         Asurf_tank, &
         Vwall_tank, &
         setTwater_tank, &
         init_wtTs, &
         Twater_vessel, &
         Tintwall_vessel, &
         Textwall_vessel, &
         thickness_wall_vessel, &
         Vwater_vessel, &
         Awater_vessel, &
         Vwall_vessel, &
         flowrate_water_supply, &
         flowrate_water_drain, &
         single_flowrate_water_supply, &
         single_flowrate_water_drain, &
         cp_water, &
         cp_wall_tank, &
         cp_wall_vessel, &
         density_water, &
         density_wall_tank, &
         density_wall_vessel, &
         BVF_tank, &
         MVF_tank, &
         conductivity_wall_tank, &
         conv_coeff_intwall_tank, &
         conv_coeff_extwall_tank, &
         emissivity_extwall_tank, &
         conductivity_wall_vessel, &
         conv_coeff_intwall_vessel, &
         conv_coeff_extwall_vessel, &
         emissivity_extwall_vessel, &
         maxheatingpower_water, &
         heating_efficiency_water, &
         minVwater_vessel, &
         minHeatingPower_DHW, &
         HeatingPower_DHW
      REAL(rprc) :: &
         qfm_dom, & ! Metabolic sensible and latent heat
         qheat_dom, & ! Hourly heating load  [W]
         qcool_dom, & ! Hourly cooling load  [W]
         qfb_hw_dom, & ! Hot water
         qfb_dom_air, & ! Sensible heat to air [W]
         dom_temp, & ! Domain temperature   [W]
         QStar, & ! Net radiation        [W m-2]
         QEC, & ! Energy use           [W m-2]
         QH, & ! Sensible heat flux   [W m-2]
         QS, & ! Storage heat flux    [W m-2]
         QBAE, & ! Building exchange    [W m-2]
         QWaste ! Waste heating        [W m-2]
      REAL(rprc), DIMENSION(2) :: Ts, initTs
      REAL(rprc), DIMENSION(4) :: h_i, k_eff
      REAL(rprc), DIMENSION(2) :: h_o
      REAL(rprc), DIMENSION(5) :: rho
      REAL(rprc), DIMENSION(5) :: Cp
      REAL(rprc), DIMENSION(5) :: emis
      REAL(rprc), DIMENSION(3) :: wiTAR, waTAR
      REAL(rprc), DIMENSION(3) :: viewFactors
      REAL(rprc), DIMENSION(3) :: occupantData
      REAL(rprc), DIMENSION(3) :: HTsAverage, HWTsAverage
      REAL(rprc), DIMENSION(3) :: HWPowerAverage
      REAL(rprc), DIMENSION(25) :: EnergyExchanges = 0.0
   END TYPE
   TYPE(LBM), ALLOCATABLE, DIMENSION(:) :: blds
END MODULE modulestebbs
MODULE modulestebbsfunc
   USE modulestebbsprecision
   IMPLICIT NONE
CONTAINS
   !-------------------------------------------------------------------
   ! Function: waterUseEnergyLossToDrains
   ! Parameters:
   !   rho - density of water [kg m-3]
   !   Cp - specific heat capacity [J kg-1 K-1]
   !   vFR - volume Flow Rate [m3 s-1]
   !   Tout - temperature of water in vessel lost to drains [K]
   !   timeResolution - time resolution [s]
   ! Returns:
   !   q_wt - energy lost to drains [J]
   !-------------------------------------------------------------------
   FUNCTION waterUseEnergyLossToDrains(rho, Cp, vFRo, Tout, timeResolution) RESULT(q_wt)
      USE modulestebbsprecision
      IMPLICIT NONE
      INTEGER, INTENT(in) :: timeResolution
      REAL(rprc), INTENT(in) :: rho, Cp, vFRo, Tout
      REAL(rprc) :: q_wt
      q_wt = rho*Cp*Tout*(vFRo*timeResolution)
   END FUNCTION
   !-------------------------------------------------------------------
   ! Function: indoorConvectionHeatTransfer
   ! Description: Indoor Convection on wall surfaces: convective heat transfer between air node and wall node(s)
   ! Parameters:
   !   h - convection coefficient applied for all internal objects [W m-2 K-1]
   !   A - the total internal surface area of internal objects [m2]
   !   Twi - wall surface temperature for surface [K]
   !   Ti - indoor air temperature [K]
   ! Returns:
   !   ind_cht - [W]
   !-------------------------------------------------------------------
   FUNCTION indoorConvectionHeatTransfer(h, A, Twi, Ti) RESULT(ind_cht)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: h, A, Twi, Ti
      REAL(rprc) :: ind_cht
      ind_cht = h*A*(Ti - Twi)
   END FUNCTION indoorConvectionHeatTransfer
   !-------------------------------------------------------------------
   ! Function: internalConvectionHeatTransfer
   ! Description: Indoor Convection on wall surfaces: convective heat transfer between air node and wall node(s)
   ! Parameters:
   !   h - convection coefficient applied for all internal objects [W m-2 K-1]
   !   A - the total internal surface area of internal objects [m2]
   !   Tio - surface temperature of internal objects [K]
   !   Ti - indoor air temperature [K]
   ! Returns:
   !   int_cht - heat transfer to internal objects [W]
   !-------------------------------------------------------------------
   FUNCTION internalConvectionHeatTransfer(h, A, Tio, Ti) RESULT(int_cht)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: h, A, Tio, Ti
      REAL(rprc) :: int_cht
      int_cht = h*A*(Ti - Tio)
   END FUNCTION internalConvectionHeatTransfer
   !-------------------------------------------------------------------
   ! Function: indoorRadiativeHeatTransfer (NOT IMPLEMENTED)
   ! Description: Indoor radiative exchange between wall surfaces and mass internal object
   ! Parameters:
   ! Returns:
   !   q -
   !-------------------------------------------------------------------
   FUNCTION indoorRadiativeHeatTransfer() RESULT(q)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc) :: q
      q = 0.0
      RETURN
   END FUNCTION indoorRadiativeHeatTransfer
   !-------------------------------------------------------------------
   ! Function: outdoorConvectionHeatTransfer
   ! Description: Outdoor Convection on surfaces. Convective heat transfer between outside wall surface and ambient air node
   ! Parameters:
   !   h - convection coefficient for  outside wall surface type (i.e. solid wall, window) [W m-2 K-1]
   !   A - total surface area of  outside wall surface type [m2]
   !   Two - wall surface temperature for surface [K]
   !   Ta - ambient air temperature (should be determined outside this model) [K]
   ! Returns:
   !   out_cht - [W]
   !-------------------------------------------------------------------
   FUNCTION outdoorConvectionHeatTransfer(h, A, Two, Ta) RESULT(out_cht)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: h, A, Two, Ta
      REAL(rprc) :: out_cht
      out_cht = h*A*(Two - Ta)
      RETURN
   END FUNCTION outdoorConvectionHeatTransfer
   !-------------------------------------------------------------------
   ! Function: outdoorRadiativeHeatTransfer
   ! Description: Outdoor Convection on surfaces. Convective heat transfer between outside wall surface and ambient air node
   ! Parameters:
   !   f - View factor for wall [-]
   !   A - building wall surface area for wall component [m2]
   !   emis - emissivity of surface [-]
   !   Two - outdoor surface temperatures for wall component [K]
   !   Ts - list of surface temperatures for surface [K]
   ! Returns:
   !   out_cht - [W]
   !-------------------------------------------------------------------
   FUNCTION outdoorRadiativeHeatTransfer(f, A, emis, Two, Ts) RESULT(q)
      USE modulestebbsprecision
      USE modulestebbs, ONLY: sigma
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: f, A, emis, Two, Ts
      REAL(rprc) :: q
      q = A*f*sigma*emis*(Two**4.0 - Ts**4.0)
      RETURN
   END FUNCTION outdoorRadiativeHeatTransfer
   !-------------------------------------------------------------------
   ! Function: lwoutdoorRadiativeHeatTransfer
   ! Description: Longwave radiative heat transfer between outside surface and ambient air
   ! Parameters:
   !   A - Area of surface [m2]
   !   emis - Emisivity of surface [-]
   !   Two - External temperature of surface [K]
   !   lw - Incoming longwave radiation onto surface [W m-2]
   ! Returns:
   !   q - Longwave radiative heat transfer [W]
   !-------------------------------------------------------------------
   FUNCTION lwoutdoorRadiativeHeatTransfer(A, emis, Two, lw) RESULT(q)
      USE modulestebbsprecision
      USE modulestebbs, ONLY: sigma
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: A, emis, Two, lw
      REAL(rprc) :: q
      q = A*sigma*emis*(Two**4.0) - emis*lw*a ! Revised based on Yiqing's discovery
      RETURN
   END FUNCTION lwoutdoorRadiativeHeatTransfer
   !-------------------------------------------------------------------
   ! Function: windowInsolation
   ! Description: Window Solar Insolation. This should be added as heat gain to internal single mass object
   ! Parameters:
   !   Irr - Irradiance incident on vertical window/wall [W m-2]
   !   Tr - Effective Transmissivity [-]
   !   A - Area of surface [m2]
   ! Returns:
   !   wi_in - Window Insolation [W]
   !-------------------------------------------------------------------
   FUNCTION windowInsolation(Irr, Tr, A) RESULT(wi_in)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: Irr, Tr, A
      REAL(rprc) :: wi_in
      wi_in = Irr*Tr*A
      RETURN
   END FUNCTION windowInsolation
   !-------------------------------------------------------------------
   ! Function: wallInsolation
   ! Description: Solar Insolation on surface. This should be added as heat gain to external surface of wall
   ! Parameters:
   !   Irr - Irradiance incident horiozntal roof or vertical wall [W m-2]
   !   Ab - Effective Absorptance of surface [-]
   !   A - Area of surface [m2]
   ! Returns:
   !   wa_in - [W]
   !-------------------------------------------------------------------
   FUNCTION wallInsolation(Irr, Ab, A) RESULT(wa_in)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: Irr, Ab, A
      REAL(rprc) :: wa_in
      wa_in = Irr*Ab*A
      RETURN
   END FUNCTION wallInsolation
   !-------------------------------------------------------------------
   ! Function: wallConduction
   ! Description: Wall Component Conduction (can be floor/roof/wall/window)
   ! Parameters:
   !   k_eff - conductivity for lumped wall [W m-1 K-1]
   !   A - total surface area of wall [m2]
   !   Twi - indoor wall surface temperature [K]
   !   Two - outdoor wall surface temperature [K]
   !   L - wall thickness [m]
   ! Returns:
   !   wa_co - [W]
   !-------------------------------------------------------------------
   FUNCTION wallConduction(k_eff, A, Twi, Two, L) RESULT(wa_co)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: k_eff, Twi, Two, A, L
      REAL(rprc) :: wa_co
      wa_co = k_eff*A*((Twi - Two)/L)
   END FUNCTION wallConduction
   !-------------------------------------------------------------------
   ! Function: windowConduction
   ! Description: Window Component Conduction (same as wall)
   ! Parameters:
   !   k_eff - thermal conductivity for window surface type [W m-1 K-1]
   !   A - total surface area of window type [m2]
   !   Twi - indoor window surface temperature for window type [K]
   !   Two - outdoor window surface temperature for window type [K]
   !   L - window unit thickness [m]
   ! Returns:
   !   wi_co - [W]
   !-------------------------------------------------------------------
   FUNCTION windowConduction(k_eff, A, Twi, Two, L) RESULT(wi_co)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: k_eff, Twi, Two, A, L
      REAL(rprc) :: wi_co
      wi_co = k_eff*A*((Twi - Two)/L)
   END FUNCTION windowConduction
   !-------------------------------------------------------------------
   ! Function: heating
   ! Description: Heating injected to building
   ! Parameters:
   !   Ts - set point temperature for heating load [K]
   !   Ti - indoor air node temperature [K]
   !   epsilon - rated efficiency of the total heating system [-]
   !   P - maximum power rating of the total heating system [W]
   ! Returns:
   !   q_heating - [W]
   !-------------------------------------------------------------------
   FUNCTION heating(Ts, Ti, epsilon, P) RESULT(q_heating)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: Ts, Ti, epsilon, P
      REAL(rprc) :: q_heating
      q_heating = 0.0
      IF (Ti < Ts) THEN
         q_heating = (P - (P/EXP(Ts - Ti)))*epsilon
      END IF
   END FUNCTION heating
   !-------------------------------------------------------------------
   ! Function: ventilationHeatTransfer
   ! Description: Building Ventilation rate heat transfer (i.e. not recirculated air)
   ! Parameters:
   !   rho - air density [kg m-3]
   !   Cp - specific heat capacity of air at constant pressure [J kg-1 K-1]
   !   V - volumetric flow rate [m3 s-1]
   !   To - outdoor temperature [K]
   !   Ti - indoor temperature [K]
   ! Returns:
   !   q_in - the heat flux resulting in the building [W]
   !-------------------------------------------------------------------
   FUNCTION ventilationHeatTransfer(rho, Cp, V, To, Ti) RESULT(q_in)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: rho, Cp, V, To, Ti
      REAL(rprc) :: q_in
      q_in = rho*Cp*V*(To - Ti)
   END FUNCTION ventilationHeatTransfer
   !-------------------------------------------------------------------
   ! Function: additionalSystemHeatingEnergy
   ! Description: Calculates the additional heat from the heating system due to system efficiency
   ! Parameters:
   !   q_heating - heating applied to building [W]
   !   epsilon - rated efficiency of the total heating system [-]
   ! Returns:
   !   qH_additional - additional heating energy [W]
   !-------------------------------------------------------------------
   FUNCTION additionalSystemHeatingEnergy(q_heating, epsilon) RESULT(qH_additional)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: q_heating, epsilon
      REAL(rprc) :: qH_additional
      qH_additional = 0.0
      qH_additional = (q_heating/epsilon) - q_heating
   END FUNCTION additionalSystemHeatingEnergy
   !-------------------------------------------------------------------
   ! Function: cooling
   ! Description: Cooling of building (heat ejected)
   ! Parameters:
   !   Ts - set point temperature for cooling load [K]
   !   Ti -indoor air node temperature [K]
   !   COP - Rated efficiency of the total heating system [-]
   !   P - maximum power rating of the total heating system [W]
   ! Returns:
   !   q_cooling - [W]
   !-------------------------------------------------------------------
   FUNCTION cooling(Ts, Ti, COP, P) RESULT(q_cooling)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: Ts, Ti, COP, P
      REAL(rprc) :: q_cooling
      q_cooling = 0.0
      IF (Ti > Ts) THEN
         q_cooling = P - (P/EXP(Ti - Ts))
      END IF
   END FUNCTION cooling
   !-------------------------------------------------------------------
   ! Function: additionalSystemCoolingEnergy
   ! Description:  Calculates the additional heat from the cooling system due to system COP
   ! Parameters:
   !   q_cooling - cooling applied to building [W]
   !   COP - rated efficiency of the total heating system [-]
   ! Returns:
   !   qC_additional - additional cooling energy [W]
   !-------------------------------------------------------------------
   FUNCTION additionalSystemCoolingEnergy(q_cooling, COP) RESULT(qC_additional)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: q_cooling, COP
      REAL(rprc) :: qC_additional
      qC_additional = q_cooling/COP
   END FUNCTION additionalSystemCoolingEnergy
   !-------------------------------------------------------------------
   ! Function: internalOccupancyGains
   ! Description: Calculates the internal gains from building occupants
   ! Parameters:
   !   Occupants - number of occupants in building [-]
   !   metRate - the metabolic rate of building occupants [W]
   !   LST - latent sensible ratio (LH/SH) [-]
   ! Returns:
   !   qSL - latent heat and sensible heat from all occupants [W]
   !-------------------------------------------------------------------
   FUNCTION internalOccupancyGains(Occupants, metRate, LSR) RESULT(qSL)
      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: Occupants, metRate, LSR
      REAL(rprc) :: qSen, qLat
      REAL(rprc), DIMENSION(2) :: qSL
      qSL(1) = (metRate*Occupants)/(1.0 + LSR)
      qSL(2) = (metRate*Occupants)*LSR/(1.0 + LSR)
   END FUNCTION internalOccupancyGains
   !-------------------------------------------------------------------
   ! Function: internalApplianceGains
   ! Description:
   ! Parameters:
   !   P - power rating of appliances [W]
   !   f - usage factor of appliance [-]
   !   n - vnumber of appliances [-]
   ! Returns:
   !   qapp - total energy of appliances - assume all goes to heat (sensible) [W]
   !-------------------------------------------------------------------
   FUNCTION internalApplianceGains(P, f, n) RESULT(qapp)
      USE modulestebbsprecision
      IMPLICIT NONE
      INTEGER, INTENT(in) :: n
      REAL(rprc), INTENT(in) :: P, f
      REAL(rprc) :: qapp
      qapp = P*f*n
   END FUNCTION internalApplianceGains
   !-------------------------------------------------------------------
   ! Function: ext_conv_coeff
   ! Description: Calculates the external convection coefficient using Eq. 11 Cole & Sturrock (1977)
   ! Parameters:
   !   wind_speed - Wind speed [m s-1]
   !   dT - Temperature difference [K]
   ! Returns:
   !   hc - External convection coefficient [W m-2 K-1]
   !-------------------------------------------------------------------
   FUNCTION ext_conv_coeff(wind_speed, dT) RESULT(hc)

      USE modulestebbsprecision
      IMPLICIT NONE
      REAL(rprc), INTENT(in) :: wind_speed, dT
      REAL(rprc) :: hn, a, b, Rf, hcglass, hc
      hn = 1.31*(ABS(dT)**(1.0/3.0))
      a = 3.26
      b = 0.89
      Rf = 1.67 ! # Rough brick used from E+ Engineering Reference guide, Walton 1981
      hcglass = ((hn**2) + ((a*(wind_speed**b))**2))**(0.5)
      hc = hn + Rf*(hcglass - hn)
   END FUNCTION ext_conv_coeff
END MODULE modulestebbsfunc
MODULE modulesuewsstebbscouple
   USE modulestebbsprecision
   IMPLICIT NONE
   REAL(rprc) :: Tair_out, Tsurf, Tground_deep, &
                 density_air_out, cp_air_out, &
                 Qsw_dn_extroof, Qsw_dn_extwall, &
                 Qlw_dn_extwall, Qlw_dn_extroof
   TYPE :: suewsprop
      INTEGER :: ntstep, timestep
      CHARACTER(len=256), ALLOCATABLE, DIMENSION(:) :: datetime, hourmin
      REAL(rprc), ALLOCATABLE, DIMENSION(:) :: Tair, Tsurf, Kwall, Kroof, ws, Lroof, Lwall
      INTEGER :: ntskip
      CHARACTER(len=256), ALLOCATABLE, DIMENSION(:) :: datetime_exch, hourmin_exch
      REAL(rprc), ALLOCATABLE, DIMENSION(:) :: Tair_exch, Tsurf_exch, Kwall_exch, Kroof_exch, ws_exch, Lroof_exch, Lwall_exch
   END TYPE
   TYPE(suewsprop) :: sout
END MODULE modulesuewsstebbscouple
SUBROUTINE setdatetime(datetimeLine)
   USE modulestebbsprecision
   USE modulesuewsstebbscouple, ONLY: sout
   IMPLICIT NONE
   REAL(rprc), DIMENSION(5), INTENT(in) :: datetimeLine
   INTEGER :: i
   CHARACTER(len=4) :: cyear
   CHARACTER(len=2) :: cmonth, cday, chour, cmin, csec
   INTEGER, DIMENSION(12) :: stmonth
   INTEGER, DIMENSION(12) :: stmonth_nonleap = (/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/)
   INTEGER, DIMENSION(12) :: stmonth_leap = (/0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/)
   WRITE (cyear, '(i4)') INT(datetimeLine(1))
   IF (MOD(INT(datetimeLine(1)), 4) == 0) THEN
      stmonth = stmonth_leap
   ELSE
      stmonth = stmonth_nonleap
   END IF
   DO i = 1, 11, 1
      IF (stmonth(i) < datetimeLine(2) .AND. datetimeLine(2) <= stmonth(i + 1)) THEN
         WRITE (cmonth, '(i2.2)') i
         WRITE (cday, '(i2.2)') INT(datetimeLine(2)) - stmonth(i)
      END IF
      IF (stmonth(12) < datetimeLine(2)) THEN
         WRITE (cmonth, '(i2.2)') 12
         WRITE (cday, '(i2.2)') INT(datetimeLine(2)) - stmonth(12)
      END IF
   END DO
   WRITE (chour, '(i2.2)') INT(datetimeLine(3))
   WRITE (cmin, '(i2.2)') INT(datetimeLine(4))
   WRITE (csec, '(i2.2)') 0
   sout%datetime(1) = TRIM(cyear//'-'//cmonth//'-'//cday)
   sout%hourmin(1) = TRIM(chour//':'//cmin//':'//csec)
   RETURN
END SUBROUTINE setdatetime
MODULE stebbs_module

   USE modulestebbsprecision, ONLY: rprc
   ! use modulestebbs, ONLY: blds, cases, resolution

CONTAINS

   SUBROUTINE stebbsonlinecouple( &
      timer, config, forcing, siteInfo, & ! Input
      modState, & ! Input/Output
      datetimeLine, &
      dataOutLineSTEBBS) ! Output
      USE modulestebbs, ONLY: blds, cases, resolution
      USE modulesuewsstebbscouple, ONLY: sout ! Defines sout
      USE modulestebbsprecision, ONLY: rprc ! Defines rprc as REAL64
      USE allocateArray, ONLY: ncolumnsDataOutSTEBBS
      USE SUEWS_DEF_DTS, ONLY: SUEWS_CONFIG, SUEWS_TIMER, SUEWS_FORCING, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, &
                               SUEWS_SITE, atm_state, ROUGHNESS_STATE, &
                               HEAT_STATE, SUEWS_STATE, STEBBS_STATE, BUILDING_ARCHETYPE_PRM, STEBBS_PRM
      IMPLICIT NONE
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState
      REAL(KIND(1D0)), INTENT(OUT), DIMENSION(ncolumnsDataOutSTEBBS - 5) :: dataOutLineSTEBBS
      INTEGER, SAVE :: flginit = 0
      REAL(rprc), DIMENSION(5), INTENT(in) :: datetimeLine
      REAL(KIND(1D0)), DIMENSION(4) :: wallStatesK, wallStatesL
      REAL(rprc) :: Kwall_sout, Lwall_sout
      REAL(rprc) :: Tsurf_sout

      ! Output variables
      REAL(rprc) :: ws
      REAL(rprc) :: Tair_sout
      ! REAL(rprc) :: Tsurf_sout
      REAL(rprc) :: Kroof_sout
      REAL(rprc) :: Lroof_sout
      ! REAL(rprc) :: Kwall_sout
      ! REAL(rprc) :: Lwall_sout
      REAL(rprc) :: Tair_ind
      REAL(rprc) :: Tindoormass
      REAL(rprc) :: Tintwallroof
      REAL(rprc) :: Textwallroof
      REAL(rprc) :: Tintwindow
      REAL(rprc) :: Textwindow
      REAL(rprc) :: Tintgroundfloor
      REAL(rprc) :: Textgroundfloor
      REAL(rprc) :: Qtotal_heating
      REAL(rprc) :: Qtotal_cooling
      REAL(rprc) :: Qsw_transmitted_window_tstepTotal
      REAL(rprc) :: Qsw_absorbed_window_tstepTotal
      REAL(rprc) :: Qsw_absorbed_wallroof_tstepTotal
      REAL(rprc) :: Qconv_indair_to_indoormass_tstepTotal
      REAL(rprc) :: Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal
      REAL(rprc) :: Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal
      REAL(rprc) :: Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal
      REAL(rprc) :: Q_appliance_tstepTotal
      REAL(rprc) :: Q_ventilation_tstepTotal
      REAL(rprc) :: Qconv_indair_to_intwallroof_tstepTotal
      REAL(rprc) :: Qconv_indair_to_intwindow_tstepTotal
      REAL(rprc) :: Qconv_indair_to_intgroundfloor_tstepTotal
      REAL(rprc) :: Qloss_efficiency_heating_air_tstepTotal
      REAL(rprc) :: Qcond_wallroof_tstepTotal
      REAL(rprc) :: Qcond_window_tstepTotal
      REAL(rprc) :: Qcond_groundfloor_tstepTotal
      REAL(rprc) :: Qcond_ground_tstepTotal
      REAL(rprc) :: Qlw_net_extwallroof_to_outair_tstepTotal
      REAL(rprc) :: Qlw_net_extwindow_to_outair_tstepTotal
      REAL(rprc) :: Qconv_extwallroof_to_outair_tstepTotal
      REAL(rprc) :: Qconv_extwindow_to_outair_tstepTotal
      REAL(rprc) :: q_cooling_timestepTotal
      REAL(rprc) :: Qtotal_water_tank
      REAL(rprc) :: Qloss_drain
      REAL(rprc) :: Twater_tank
      REAL(rprc) :: Tintwall_tank
      REAL(rprc) :: Textwall_tank
      REAL(rprc) :: Twater_vessel
      REAL(rprc) :: Tintwall_vessel
      REAL(rprc) :: Textwall_vessel
      REAL(rprc) :: Vwater_vessel
      REAL(rprc) :: Awater_vessel
      REAL(rprc) :: Vwall_vessel
      REAL(rprc) :: qsensible_timestepTotal
      REAL(rprc) :: qlatent_timestepTotal
      REAL(rprc) :: QS_tstepTotal
      REAL(rprc) :: QS_fabric_tstepTotal
      REAL(rprc) :: QS_air_tstepTotal
      REAL(rprc) :: Vwall_tank
      REAL(rprc) :: Vwater_tank

      ASSOCIATE ( &
         timestep => timer%tstep, &
         heatState => modState%heatState, &
         atmState => modState%atmState, &
         roughnessState => modState%roughnessState, &
         stebbsState => modState%stebbsState, &
         building_archtype => siteInfo%building_archtype, &
         stebbsPrm => siteInfo%stebbs &
         )

         ASSOCIATE ( &
            ws => atmState%U10_ms, &
            Tair_sout => atmState%t2_C, &
            Tsurf_sout => heatState%Tsurf, &
            Kroof_sout => stebbsState%Kdown2d, &
            Lroof_sout => stebbsState%Ldown2d, &
            ! Create an array of the wall states
            Knorth => stebbsState%Knorth, &
            Ksouth => stebbsState%Ksouth, &
            Keast => stebbsState%Keast, &
            Kwest => stebbsState%Kwest, &
            Lnorth => stebbsState%Lnorth, &
            Lsouth => stebbsState%Lsouth, &
            Least => stebbsState%Least, &
            Lwest => stebbsState%Lwest &
            )

            wallStatesK(1) = Knorth
            wallStatesK(2) = Ksouth
            wallStatesK(3) = Keast
            wallStatesK(4) = Kwest
            ! Calculate the mean of the wall states
            Kwall_sout = SUM(wallStatesK)/SIZE(wallStatesK)

            ! Calculate the mean of the wall states
            wallStatesL(1) = Lnorth
            wallStatesL(2) = Lsouth
            wallStatesL(3) = Least
            wallStatesL(4) = Lwest
            Lwall_sout = SUM(wallStatesL)/SIZE(wallStatesL)

            !       !
            IF (flginit == 0) THEN
               ALLOCATE (cases(1))
               WRITE (*, *) 'Initialising STEBBS'
               ALLOCATE (blds(1))
               resolution = 1
               CALL gen_building(stebbsState, stebbsPrm, building_archtype, blds(1))
               ! call create_building(cases(1),blds(1),1)

               ! Print out all values of blds(1) to check initialization
               ! WRITE (*, *) 'Building Type: ', blds(1)%BuildingType
               ! WRITE (*, *) 'Building Name: ', blds(1)%BuildingName
               ! WRITE (*, *) 'File Name LBM: ', blds(1)%fnmlLBM
               ! WRITE (*, *) 'Case: ', blds(1)%CASE
               ! WRITE (*, *) 'ID LBM: ', blds(1)%idLBM
               ! WRITE (*, *) 'Flag Init: ', blds(1)%flginit
               ! WRITE (*, *) 'Total Number of Appliances: ', blds(1)%appliance_totalnumber
               ! WRITE (*, *) 'Qtotal Heating: ', blds(1)%Qtotal_heating
               ! WRITE (*, *) 'Qtotal Cooling: ', blds(1)%Qtotal_cooling
               ! WRITE (*, *) 'Qmetabolic Sensible: ', blds(1)%Qmetabolic_sensible
               ! WRITE (*, *) 'Qmetabolic Latent: ', blds(1)%Qmetabolic_latent
               ! WRITE (*, *) 'Qtotal Water Tank: ', blds(1)%Qtotal_water_tank
               ! WRITE (*, *) 'qhwt Drain: ', blds(1)%qhwtDrain
               ! WRITE (*, *) 'Ratio Window Wall: ', blds(1)%ratio_window_wall
               ! WRITE (*, *) 'Afootprint: ', blds(1)%Afootprint
               ! WRITE (*, *) 'Height Building: ', blds(1)%height_building
               ! WRITE (*, *) 'Wall External Area: ', blds(1)%wallExternalArea
               ! WRITE (*, *) 'Ratio Internal Volume: ', blds(1)%ratioInternalVolume
               ! WRITE (*, *) 'Thickness Wall Roof: ', blds(1)%thickness_wallroof
               ! WRITE (*, *) 'Thickness Ground Floor: ', blds(1)%thickness_groundfloor
               ! WRITE (*, *) 'Depth Ground: ', blds(1)%depth_ground
               ! WRITE (*, *) 'Thickness Window: ', blds(1)%thickness_window
               ! WRITE (*, *) 'Conv Coeff Int Wall Roof: ', blds(1)%conv_coeff_intwallroof
               ! WRITE (*, *) 'Conv Coeff Indoor Mass: ', blds(1)%conv_coeff_indoormass
               ! WRITE (*, *) 'Conv Coeff Int Ground Floor: ', blds(1)%conv_coeff_intgroundfloor
               ! WRITE (*, *) 'Conv Coeff Int Window: ', blds(1)%conv_coeff_intwindow
               ! WRITE (*, *) 'Conv Coeff Ext Wall Roof: ', blds(1)%conv_coeff_extwallroof
               ! WRITE (*, *) 'Conv Coeff Ext Window: ', blds(1)%conv_coeff_extwindow
               ! WRITE (*, *) 'Conductivity Wall Roof: ', blds(1)%conductivity_wallroof
               ! WRITE (*, *) 'Conductivity Ground Floor: ', blds(1)%conductivity_groundfloor
               ! WRITE (*, *) 'Conductivity Window: ', blds(1)%conductivity_window
               ! WRITE (*, *) 'Conductivity Ground: ', blds(1)%conductivity_ground
               ! WRITE (*, *) 'Density Wall Roof: ', blds(1)%density_wallroof
               ! WRITE (*, *) 'Weighting Factor Heat Capacity Wall Roof: ', blds(1)%weighting_factor_heatcapacity_wallroof
               ! WRITE (*, *) 'Density Ground Floor: ', blds(1)%density_groundfloor
               ! WRITE (*, *) 'Density Window: ', blds(1)%density_window
               ! WRITE (*, *) 'Density Indoor Mass: ', blds(1)%density_indoormass
               ! WRITE (*, *) 'Density Air Indoor: ', blds(1)%density_air_ind
               ! WRITE (*, *) 'Cp Wall Roof: ', blds(1)%cp_wallroof
               ! WRITE (*, *) 'Cp Ground Floor: ', blds(1)%cp_groundfloor
               ! WRITE (*, *) 'Cp Window: ', blds(1)%cp_window
               ! WRITE (*, *) 'Cp Indoor Mass: ', blds(1)%cp_indoormass
               ! WRITE (*, *) 'Cp Air Indoor: ', blds(1)%cp_air_ind
               ! WRITE (*, *) 'Emissivity Ext Wall Roof: ', blds(1)%emissivity_extwallroof
               ! WRITE (*, *) 'Emissivity Int Wall Roof: ', blds(1)%emissivity_intwallroof
               ! WRITE (*, *) 'Emissivity Indoor Mass: ', blds(1)%emissivity_indoormass
               ! WRITE (*, *) 'Emissivity Ext Window: ', blds(1)%emissivity_extwindow
               ! WRITE (*, *) 'Emissivity Int Window: ', blds(1)%emissivity_intwindow
               ! WRITE (*, *) 'Window Transmissivity: ', blds(1)%windowTransmissivity
               ! WRITE (*, *) 'Window Absorptivity: ', blds(1)%windowAbsorbtivity
               ! WRITE (*, *) 'Window Reflectivity: ', blds(1)%windowReflectivity
               ! WRITE (*, *) 'Wall Transmissivity: ', blds(1)%wallTransmisivity
               ! WRITE (*, *) 'Wall Absorptivity: ', blds(1)%wallAbsorbtivity
               ! WRITE (*, *) 'Wall Reflectivity: ', blds(1)%wallReflectivity
               ! WRITE (*, *) 'BVF Ext Wall: ', blds(1)%BVF_extwall
               ! WRITE (*, *) 'GVF Ext Wall: ', blds(1)%GVF_extwall
               ! WRITE (*, *) 'SVF Ext Wall: ', blds(1)%SVF_extwall
               ! WRITE (*, *) 'Occupants: ', blds(1)%occupants
               ! WRITE (*, *) 'Metabolic Rate: ', blds(1)%metabolic_rate
               ! WRITE (*, *) 'Ratio Metabolic Latent Sensible: ', blds(1)%ratio_metabolic_latent_sensible
               ! WRITE (*, *) 'Appliance Power Rating: ', blds(1)%appliance_power_rating
               ! WRITE (*, *) 'Appliance Usage Factor: ', blds(1)%appliance_usage_factor
               ! WRITE (*, *) 'Max Heating Power Air: ', blds(1)%maxheatingpower_air
               ! WRITE (*, *) 'Heating Efficiency Air: ', blds(1)%heating_efficiency_air
               ! WRITE (*, *) 'Max Cooling Power Air: ', blds(1)%maxcoolingpower_air
               ! WRITE (*, *) 'Coeff Performance Cooling: ', blds(1)%coeff_performance_cooling
               ! WRITE (*, *) 'Vair Indoor: ', blds(1)%Vair_ind
               ! WRITE (*, *) 'Ventilation Rate: ', blds(1)%ventilation_rate
               ! WRITE (*, *) 'Awall Roof: ', blds(1)%Awallroof
               ! WRITE (*, *) 'Vwall Roof: ', blds(1)%Vwallroof
               ! WRITE (*, *) 'Vground Floor: ', blds(1)%Vgroundfloor
               ! WRITE (*, *) 'Awindow: ', blds(1)%Awindow
               ! WRITE (*, *) 'Vwindow: ', blds(1)%Vwindow
               ! WRITE (*, *) 'Vindoormass: ', blds(1)%Vindoormass
               ! WRITE (*, *) 'Aindoormass: ', blds(1)%Aindoormass
               ! WRITE (*, *) 'Tair Indoor: ', blds(1)%Tair_ind
               ! WRITE (*, *) 'Tindoormass: ', blds(1)%Tindoormass
               ! WRITE (*, *) 'Tint Wall Roof: ', blds(1)%Tintwallroof
               ! WRITE (*, *) 'Text Wall Roof: ', blds(1)%Textwallroof
               ! WRITE (*, *) 'Tint Window: ', blds(1)%Tintwindow
               ! WRITE (*, *) 'Text Window: ', blds(1)%Textwindow
               ! WRITE (*, *) 'Tint Ground Floor: ', blds(1)%Tintgroundfloor
               ! WRITE (*, *) 'Text Ground Floor: ', blds(1)%Textgroundfloor
               ! WRITE (*, *) 'Twater Tank: ', blds(1)%Twater_tank
               ! WRITE (*, *) 'Tint Wall Tank: ', blds(1)%Tintwall_tank
               ! WRITE (*, *) 'Text Wall Tank: ', blds(1)%Textwall_tank
               ! WRITE (*, *) 'Thickness Tank Wall: ', blds(1)%thickness_tankwall
               ! WRITE (*, *) 'Tincoming Water Tank: ', blds(1)%Tincomingwater_tank
               ! WRITE (*, *) 'Vwater Tank: ', blds(1)%Vwater_tank
               ! WRITE (*, *) 'Asurf Tank: ', blds(1)%Asurf_tank
               ! WRITE (*, *) 'Vwall Tank: ', blds(1)%Vwall_tank
               ! WRITE (*, *) 'Set Twater Tank: ', blds(1)%setTwater_tank
               ! WRITE (*, *) 'Init Wt Ts: ', blds(1)%init_wtTs
               ! WRITE (*, *) 'Twater Vessel: ', blds(1)%Twater_vessel
               ! WRITE (*, *) 'Tint Wall Vessel: ', blds(1)%Tintwall_vessel
               ! WRITE (*, *) 'Text Wall Vessel: ', blds(1)%Textwall_vessel
               ! WRITE (*, *) 'Thickness Wall Vessel: ', blds(1)%thickness_wall_vessel
               ! WRITE (*, *) 'Vwater Vessel: ', blds(1)%Vwater_vessel
               ! WRITE (*, *) 'Awater Vessel: ', blds(1)%Awater_vessel
               ! WRITE (*, *) 'Vwall Vessel: ', blds(1)%Vwall_vessel
               ! WRITE (*, *) 'Flowrate Water Supply: ', blds(1)%flowrate_water_supply
               ! WRITE (*, *) 'Flowrate Water Drain: ', blds(1)%flowrate_water_drain
               ! WRITE (*, *) 'Single Flowrate Water Supply: ', blds(1)%single_flowrate_water_supply
               ! WRITE (*, *) 'Single Flowrate Water Drain: ', blds(1)%single_flowrate_water_drain
               ! WRITE (*, *) 'Cp Water: ', blds(1)%cp_water
               ! WRITE (*, *) 'Cp Water Tank: ', blds(1)%cp_water_tank
               ! WRITE (*, *) 'Cp Wall Tank: ', blds(1)%cp_wall_tank
               ! WRITE (*, *) 'Cp Wall Vessel: ', blds(1)%cp_wall_vessel
               ! WRITE (*, *) 'Density Water: ', blds(1)%density_water
               ! WRITE (*, *) 'Density Wall Tank: ', blds(1)%density_wall_tank
               ! WRITE (*, *) 'Density Wall Vessel: ', blds(1)%density_wall_vessel
               ! WRITE (*, *) 'BVF Tank: ', blds(1)%BVF_tank
               ! WRITE (*, *) 'MVF Tank: ', blds(1)%MVF_tank
               ! WRITE (*, *) 'Conductivity Wall Tank: ', blds(1)%conductivity_wall_tank
               ! WRITE (*, *) 'Conv Coeff Int Wall Tank: ', blds(1)%conv_coeff_intwall_tank
               ! WRITE (*, *) 'Conv Coeff Ext Wall Tank: ', blds(1)%conv_coeff_extwall_tank
               ! WRITE (*, *) 'Emissivity Ext Wall Tank: ', blds(1)%emissivity_extwall_tank
               ! WRITE (*, *) 'Conductivity Wall Vessel: ', blds(1)%conductivity_wall_vessel
               ! WRITE (*, *) 'Conv Coeff Int Wall Vessel: ', blds(1)%conv_coeff_intwall_vessel
               ! WRITE (*, *) 'Conv Coeff Ext Wall Vessel: ', blds(1)%conv_coeff_extwall_vessel
               ! WRITE (*, *) 'Emissivity Ext Wall Vessel: ', blds(1)%emissivity_extwall_vessel
               ! WRITE (*, *) 'Max Heating Power Water: ', blds(1)%maxheatingpower_water
               ! WRITE (*, *) 'Heating Efficiency Water: ', blds(1)%heating_efficiency_water
               ! WRITE (*, *) 'Min Vwater Vessel: ', blds(1)%minVwater_vessel
               ! WRITE (*, *) 'Min Heating Power DHW: ', blds(1)%minHeatingPower_DHW
               ! WRITE (*, *) 'Heating Power DHW: ', blds(1)%HeatingPower_DHW
               ! WRITE (*, *) 'Ts: ', blds(1)%Ts
               ! WRITE (*, *) 'Init Ts: ', blds(1)%initTs
               ! WRITE (*, *) 'h_i: ', blds(1)%h_i
               ! WRITE (*, *) 'k_eff: ', blds(1)%k_eff
               ! WRITE (*, *) 'h_o: ', blds(1)%h_o
               ! WRITE (*, *) 'rho: ', blds(1)%rho
               ! WRITE (*, *) 'Cp: ', blds(1)%Cp
               ! WRITE (*, *) 'emis: ', blds(1)%emis
               ! WRITE (*, *) 'wiTAR: ', blds(1)%wiTAR
               ! WRITE (*, *) 'waTAR: ', blds(1)%waTAR
               ! WRITE (*, *) 'viewFactors: ', blds(1)%viewFactors
               ! WRITE (*, *) 'occupantData: ', blds(1)%occupantData
               ! WRITE (*, *) 'HTsAverage: ', blds(1)%HTsAverage
               ! WRITE (*, *) 'HWTsAverage: ', blds(1)%HWTsAverage
               ! WRITE (*, *) 'HWPowerAverage: ', blds(1)%HWPowerAverage
               ! WRITE (*, *) 'Energy Exchanges: ', blds(1)%EnergyExchanges
               ! WRITE (*, *) 'qfm_dom: ', blds(1)%qfm_dom
               ! WRITE (*, *) 'qheat_dom: ', blds(1)%qheat_dom
               ! WRITE (*, *) 'qcool_dom: ', blds(1)%qcool_dom
               ! WRITE (*, *) 'qfb_hw_dom: ', blds(1)%qfb_hw_dom
               ! WRITE (*, *) 'qfb_dom_air: ', blds(1)%qfb_dom_air
               ! WRITE (*, *) 'dom_temp: ', blds(1)%dom_temp
               ! WRITE (*, *) 'QStar: ', blds(1)%QStar
               ! WRITE (*, *) 'QEC: ', blds(1)%QEC
               ! WRITE (*, *) 'QH: ', blds(1)%QH
               ! WRITE (*, *) 'QS: ', blds(1)%QS
               ! WRITE (*, *) 'QBAE: ', blds(1)%QBAE
               ! WRITE (*, *) 'QWaste: ', blds(1)%QWaste
               sout%ntstep = 1
               ALLOCATE (sout%datetime(sout%ntstep))
               ALLOCATE (sout%hourmin(sout%ntstep))
               ALLOCATE (sout%Tair(sout%ntstep))
               ALLOCATE (sout%Tsurf(sout%ntstep))
               ALLOCATE (sout%Kwall(sout%ntstep))
               ALLOCATE (sout%Kroof(sout%ntstep))
               ALLOCATE (sout%ws(sout%ntstep))
               ALLOCATE (sout%Lroof(sout%ntstep))
               ALLOCATE (sout%Lwall(sout%ntstep))
               ALLOCATE (sout%datetime_exch(sout%ntstep))
               ALLOCATE (sout%hourmin_exch(sout%ntstep))
               ALLOCATE (sout%Tair_exch(sout%ntstep))
               ALLOCATE (sout%Tsurf_exch(sout%ntstep))
               ALLOCATE (sout%Kwall_exch(sout%ntstep))
               ALLOCATE (sout%Kroof_exch(sout%ntstep))
               ALLOCATE (sout%ws_exch(sout%ntstep))
               ALLOCATE (sout%Lroof_exch(sout%ntstep))
               ALLOCATE (sout%Lwall_exch(sout%ntstep))
               !
            END IF

            sout%Tair(1) = Tair_sout
            sout%Tsurf(1) = Tsurf_sout
            sout%Kroof(1) = Kroof_sout
            sout%Kwall(1) = Kwall_sout
            sout%Lwall(1) = Lwall_sout
            sout%Lroof(1) = Lroof_sout
            sout%timestep = timestep
            ! sout%timestep = 3600
            sout%Tair_exch(1) = Tair_sout
            sout%Tsurf_exch(1) = Tsurf_sout
            sout%ws(1) = ws
            sout%ws_exch(1) = ws
            CALL setdatetime(datetimeLine)
            ! nbtype = SIZE(blds)
            ! DO i = 1, nbtype, 1

            CALL suewsstebbscouple( &
               blds(1), flginit, datetimeLine, &
               Tair_ind, Tindoormass, Tintwallroof, Textwallroof, Tintwindow, Textwindow, Tintgroundfloor, &
               Textgroundfloor, Qtotal_heating, Qtotal_cooling, Qsw_transmitted_window_tstepTotal, &
               Qsw_absorbed_window_tstepTotal, Qsw_absorbed_wallroof_tstepTotal, Qconv_indair_to_indoormass_tstepTotal, &
               Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal, &
               Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal, &
               Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal, Q_appliance_tstepTotal, &
               Q_ventilation_tstepTotal, Qconv_indair_to_intwallroof_tstepTotal, Qconv_indair_to_intwindow_tstepTotal, &
               Qconv_indair_to_intgroundfloor_tstepTotal, Qloss_efficiency_heating_air_tstepTotal, &
               Qcond_wallroof_tstepTotal, Qcond_window_tstepTotal, Qcond_groundfloor_tstepTotal, &
               Qcond_ground_tstepTotal, Qlw_net_extwallroof_to_outair_tstepTotal, &
               Qlw_net_extwindow_to_outair_tstepTotal, Qconv_extwallroof_to_outair_tstepTotal, &
               Qconv_extwindow_to_outair_tstepTotal, q_cooling_timestepTotal, Qtotal_water_tank, Qloss_drain, &
               Twater_tank, Tintwall_tank, Textwall_tank, Twater_vessel, Tintwall_vessel, Textwall_vessel, &
               Vwater_vessel, Awater_vessel, Vwall_vessel, qsensible_timestepTotal, qlatent_timestepTotal, &
               QS_tstepTotal, QS_fabric_tstepTotal, QS_air_tstepTotal, &
               Vwall_tank, Vwater_tank &
               )
            ! END DO
            flginit = 1

            dataOutLineSTEBBS = [ &
                                ! Forcing
                                ws, Tair_sout, Tsurf_sout, &
                                Kroof_sout, Lroof_sout, Kwall_sout, Lwall_sout, &
                                ! Temperatures
                                Tair_ind, Tindoormass, Tintwallroof, Textwallroof, Tintwindow, &
                                Textwindow, Tintgroundfloor, &
                                Textgroundfloor, Qtotal_heating, &
                                Qtotal_cooling, Qsw_transmitted_window_tstepTotal, &
                                Qsw_absorbed_window_tstepTotal, Qsw_absorbed_wallroof_tstepTotal, &
                                Qconv_indair_to_indoormass_tstepTotal, &
                                Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal, &
                                Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal, &
                                Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal, &
                                Q_appliance_tstepTotal, &
                                Q_ventilation_tstepTotal, Qconv_indair_to_intwallroof_tstepTotal, &
                                Qconv_indair_to_intwindow_tstepTotal, &
                                Qconv_indair_to_intgroundfloor_tstepTotal, &
                                Qloss_efficiency_heating_air_tstepTotal, &
                                Qcond_wallroof_tstepTotal, Qcond_window_tstepTotal, &
                                Qcond_groundfloor_tstepTotal, &
                                Qcond_ground_tstepTotal, &
                                Qlw_net_extwallroof_to_outair_tstepTotal, &
                                Qlw_net_extwindow_to_outair_tstepTotal, &
                                Qconv_extwallroof_to_outair_tstepTotal, &
                                Qconv_extwindow_to_outair_tstepTotal, q_cooling_timestepTotal, &
                                Qtotal_water_tank, Qloss_drain, &
                                Twater_tank, Tintwall_tank, Textwall_tank, Twater_vessel, &
                                Tintwall_vessel, Textwall_vessel, &
                                Vwater_vessel, Awater_vessel, Vwall_vessel, qsensible_timestepTotal, &
                                qlatent_timestepTotal, &
                                QS_tstepTotal, QS_fabric_tstepTotal, QS_air_tstepTotal, &
                                Vwall_tank, Vwater_tank &
                                ]
            RETURN
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE stebbsonlinecouple
END MODULE stebbs_module
SUBROUTINE readsuewsout()
   USE modulesuewsstebbscouple
   IMPLICIT NONE
   INTEGER :: i, reason, icrop
   sout%timestep = 3600 ! 1hr, hard-coded for the test
   OPEN (8, file='./SUEWS_output_res.csv', form='formatted')

   i = 0
   DO WHILE (.TRUE.)
      READ (8, *, iostat=reason)
      IF (reason < 0) go to 333
      i = i + 1
   END DO
333 CONTINUE
   sout%ntstep = i - 1
   ALLOCATE (sout%datetime(sout%ntstep))
   ALLOCATE (sout%hourmin(sout%ntstep))
   ALLOCATE (sout%Tair(sout%ntstep))
   ALLOCATE (sout%Tsurf(sout%ntstep))
   ALLOCATE (sout%Kwall(sout%ntstep))
   ALLOCATE (sout%Kroof(sout%ntstep))
   ALLOCATE (sout%ws(sout%ntstep))
   ALLOCATE (sout%Lroof(sout%ntstep))
   ALLOCATE (sout%Lwall(sout%ntstep))
   ALLOCATE (sout%datetime_exch(sout%ntstep))
   ALLOCATE (sout%hourmin_exch(sout%ntstep))
   ALLOCATE (sout%Tair_exch(sout%ntstep))
   ALLOCATE (sout%Tsurf_exch(sout%ntstep))
   ALLOCATE (sout%Kwall_exch(sout%ntstep))
   ALLOCATE (sout%Kroof_exch(sout%ntstep))
   ALLOCATE (sout%ws_exch(sout%ntstep))
   ALLOCATE (sout%Lroof_exch(sout%ntstep))
   ALLOCATE (sout%Lwall_exch(sout%ntstep))
   REWIND (8)
   READ (8, *)
   DO i = 1, sout%ntstep, 1
      READ (8, *) sout%datetime(i), sout%hourmin(i), sout%Tair(i), sout%Tsurf(i), &
         sout%Kwall(i), sout%Kroof(i), sout%ws(i), sout%Lroof(i), sout%Lwall(i)
   END DO
   WRITE (*, *) '    + SUEWS output profile'
   WRITE (*, *) '    + Date            : ', TRIM(sout%datetime(1)), ' ', TRIM(sout%hourmin(1)), ' - ', &
      TRIM(sout%datetime(sout%ntstep)), ' ', TRIM(sout%hourmin(sout%ntstep))
   WRITE (*, *) '    + Total data step : ', sout%ntstep
   CLOSE (8)
   sout%ntskip = 12
   OPEN (8, file='./SUEWS_output.csv', form='formatted')
   READ (8, *)

   DO i = 1, sout%ntstep*sout%ntskip, 1
      IF (MOD(i - 1, sout%ntskip) == 0) THEN
         icrop = INT((i - 1)/sout%ntskip) + 1
         READ (8, *) sout%datetime_exch(icrop), sout%hourmin_exch(icrop), &
            sout%Tair_exch(icrop), sout%Tsurf_exch(icrop), &
            sout%Kwall_exch(icrop), sout%Kroof_exch(icrop), &
            sout%ws_exch(icrop), sout%Lroof_exch(icrop), &
            sout%Lwall_exch(icrop)
      ELSE
         READ (8, *)
      END IF
   END DO
   CLOSE (8)
   RETURN
END SUBROUTINE readsuewsout
SUBROUTINE suewsstebbscouple(self, flginit, datetimeLine, &
                             Tair_ind, Tindoormass, Tintwallroof, Textwallroof, Tintwindow, Textwindow, Tintgroundfloor, &
                             Textgroundfloor, Qtotal_heating, Qtotal_cooling, Qsw_transmitted_window_tstepTotal, &
                          Qsw_absorbed_window_tstepTotal, Qsw_absorbed_wallroof_tstepTotal, Qconv_indair_to_indoormass_tstepTotal, &
                             Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal, &
                             Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal, &
                             Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal, Q_appliance_tstepTotal, &
                           Q_ventilation_tstepTotal, Qconv_indair_to_intwallroof_tstepTotal, Qconv_indair_to_intwindow_tstepTotal, &
                             Qconv_indair_to_intgroundfloor_tstepTotal, Qloss_efficiency_heating_air_tstepTotal, &
                             Qcond_wallroof_tstepTotal, Qcond_window_tstepTotal, Qcond_groundfloor_tstepTotal, &
                             Qcond_ground_tstepTotal, Qlw_net_extwallroof_to_outair_tstepTotal, &
                             Qlw_net_extwindow_to_outair_tstepTotal, Qconv_extwallroof_to_outair_tstepTotal, &
                             Qconv_extwindow_to_outair_tstepTotal, q_cooling_timestepTotal, Qtotal_water_tank, Qloss_drain, &
                             Twater_tank, Tintwall_tank, Textwall_tank, Twater_vessel, Tintwall_vessel, Textwall_vessel, &
                             Vwater_vessel, Awater_vessel, Vwall_vessel, qsensible_timestepTotal, qlatent_timestepTotal, &
                             QS_tstepTotal, QS_fabric_tstepTotal, QS_air_tstepTotal, &
                             Vwall_tank, Vwater_tank &
                             ) ! Output

   USE modulestebbsprecision
   USE modulestebbs, ONLY: LBM, resolution
   USE modulestebbsfunc, ONLY: ext_conv_coeff
   USE modulesuewsstebbscouple, ONLY: &
      sout, &
      Tair_out, Tground_deep, Tsurf, density_air_out, &
      cp_air_out, &
      Qsw_dn_extroof, &
      Qsw_dn_extwall, &
      Qlw_dn_extwall, Qlw_dn_extroof
   IMPLICIT NONE
   TYPE(LBM) :: self
   INTEGER :: tstep, i
   INTEGER, INTENT(in) :: flginit
   ! Internal variables
   REAL(rprc) :: Area, qinternal, qe_cool, qe_heat, q_waste, q_ventilation

   ! Output variables with INTENT(OUT)
   REAL(rprc), INTENT(OUT) :: Tair_ind
   REAL(rprc), INTENT(OUT) :: Tindoormass
   REAL(rprc), INTENT(OUT) :: Tintwallroof
   REAL(rprc), INTENT(OUT) :: Textwallroof
   REAL(rprc), INTENT(OUT) :: Tintwindow
   REAL(rprc), INTENT(OUT) :: Textwindow
   REAL(rprc), INTENT(OUT) :: Tintgroundfloor
   REAL(rprc), INTENT(OUT) :: Textgroundfloor
   REAL(rprc), INTENT(OUT) :: Qtotal_heating
   REAL(rprc), INTENT(OUT) :: Qtotal_cooling
   REAL(rprc), INTENT(OUT) :: Qsw_transmitted_window_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qsw_absorbed_window_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qsw_absorbed_wallroof_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qconv_indair_to_indoormass_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal
   REAL(rprc), INTENT(OUT) :: Q_appliance_tstepTotal
   REAL(rprc), INTENT(OUT) :: Q_ventilation_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qconv_indair_to_intwallroof_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qconv_indair_to_intwindow_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qconv_indair_to_intgroundfloor_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qloss_efficiency_heating_air_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qcond_wallroof_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qcond_window_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qcond_groundfloor_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qcond_ground_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qlw_net_extwallroof_to_outair_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qlw_net_extwindow_to_outair_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qconv_extwallroof_to_outair_tstepTotal
   REAL(rprc), INTENT(OUT) :: Qconv_extwindow_to_outair_tstepTotal
   REAL(rprc), INTENT(OUT) :: q_cooling_timestepTotal
   REAL(rprc), INTENT(OUT) :: Qtotal_water_tank
   REAL(rprc), INTENT(OUT) :: Qloss_drain
   REAL(rprc), INTENT(OUT) :: Twater_tank
   REAL(rprc), INTENT(OUT) :: Tintwall_tank
   REAL(rprc), INTENT(OUT) :: Textwall_tank
   REAL(rprc), INTENT(OUT) :: Twater_vessel
   REAL(rprc), INTENT(OUT) :: Tintwall_vessel
   REAL(rprc), INTENT(OUT) :: Textwall_vessel
   REAL(rprc), INTENT(OUT) :: Vwater_vessel
   REAL(rprc), INTENT(OUT) :: Awater_vessel
   REAL(rprc), INTENT(OUT) :: Vwall_vessel
   REAL(rprc), INTENT(OUT) :: qsensible_timestepTotal
   REAL(rprc), INTENT(OUT) :: qlatent_timestepTotal
   REAL(rprc), INTENT(OUT) :: QS_tstepTotal
   REAL(rprc), INTENT(OUT) :: QS_fabric_tstepTotal
   REAL(rprc), INTENT(OUT) :: QS_air_tstepTotal
   REAL(rprc), INTENT(OUT) :: Vwall_tank
   REAL(rprc), INTENT(OUT) :: Vwater_tank

   ! Other declarations
   REAL(rprc), DIMENSION(6) :: bem_qf_1
   REAL(rprc), DIMENSION(25) :: energyEx
   CHARACTER(len=256) :: CASE
   CHARACTER(len=256), DIMENSION(4) :: fout
   REAL(rprc), DIMENSION(5), INTENT(in) :: datetimeLine
   CHARACTER(len=256) :: debug_array_dir

   ! CASE = self%CASE
   Area = self%Afootprint
   DO tstep = 1, sout%ntstep, 1
      Tair_out = sout%Tair(tstep) + 273.15
      Tground_deep = 273.15 + 10.0
      Tsurf = sout%Tsurf(tstep) + 273.15
      density_air_out = 1.225
      cp_air_out = 1005.0
      Qsw_dn_extroof = sout%Kroof(tstep)
      Qsw_dn_extwall = sout%Kwall(tstep)
      Qlw_dn_extwall = sout%Lwall(tstep)
      Qlw_dn_extroof = sout%Lroof(tstep)
      debug_array_dir = './debug_array.csv'
      IF (sout%ws_exch(tstep) < 0) THEN
         sout%ws_exch(tstep) = 0.2
         ! WRITE (*, *) 'Wind speed is negative, set to 0.2'
      END IF

      self%h_o(1) = ext_conv_coeff(sout%ws_exch(tstep), sout%Tair_exch(tstep) - sout%Tsurf_exch(tstep))
      self%h_o(2) = ext_conv_coeff(sout%ws_exch(tstep), sout%Tair_exch(tstep) - sout%Tsurf_exch(tstep))

      CALL timeStepCalculation(self, Tair_out, Tground_deep, Tsurf, &
                               density_air_out, cp_air_out, &
                               Qsw_dn_extroof, Qsw_dn_extwall, &
                               Qlw_dn_extwall, Qlw_dn_extroof, sout%timestep, &
                               resolution, &
                               datetimeLine, &
                               flginit &
                               )

      Tair_ind = self%Tair_ind
      Tindoormass = self%Tindoormass
      Tintwallroof = self%Tintwallroof
      Textwallroof = self%Textwallroof
      Tintwindow = self%Tintwindow
      Textwindow = self%Textwindow
      Tintgroundfloor = self%Tintgroundfloor
      Textgroundfloor = self%Textgroundfloor
      Qtotal_heating = self%Qtotal_heating
      Qtotal_cooling = self%Qtotal_cooling

      Qsw_transmitted_window_tstepTotal = self%EnergyExchanges(1) !Qsw_transmitted_window_tstepTotal
      Qsw_absorbed_window_tstepTotal = self%EnergyExchanges(2) !Qsw_absorbed_window_tstepTotal
      Qsw_absorbed_wallroof_tstepTotal = self%EnergyExchanges(3) !Qsw_absorbed_wallroof_tstepTotal
      Qconv_indair_to_indoormass_tstepTotal = self%EnergyExchanges(4) !Qconv_indair_to_indoormass_tstepTotal
      Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal = self%EnergyExchanges(5) !Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal
      Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal = self%EnergyExchanges(6) !Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal
      Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal = self%EnergyExchanges(7) !Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal
      Q_appliance_tstepTotal = self%EnergyExchanges(8) !Q_appliance_tstepTotal
      Q_ventilation_tstepTotal = self%EnergyExchanges(9) !Q_ventilation_tstepTotal
      Qconv_indair_to_intwallroof_tstepTotal = self%EnergyExchanges(10) !Qconv_indair_to_intwallroof_tstepTotal
      Qconv_indair_to_intwindow_tstepTotal = self%EnergyExchanges(11) !Qconv_indair_to_intwindow_tstepTotal
      Qconv_indair_to_intgroundfloor_tstepTotal = self%EnergyExchanges(12) !Qconv_indair_to_intgroundfloor_tstepTotal
      Qloss_efficiency_heating_air_tstepTotal = self%EnergyExchanges(13) !Qloss_efficiency_heating_air_tstepTotal
      Qcond_wallroof_tstepTotal = self%EnergyExchanges(14) !Qcond_wallroof_tstepTotal
      Qcond_window_tstepTotal = self%EnergyExchanges(15) !Qcond_window_tstepTotal
      Qcond_groundfloor_tstepTotal = self%EnergyExchanges(16) !Qcond_groundfloor_tstepTotal
      Qcond_ground_tstepTotal = self%EnergyExchanges(17) !Qcond_ground_tstepTotal
      Qlw_net_extwallroof_to_outair_tstepTotal = self%EnergyExchanges(18) !Qlw_net_extwallroof_to_outair_tstepTotal
      Qlw_net_extwindow_to_outair_tstepTotal = self%EnergyExchanges(19) !Qlw_net_extwindow_to_outair_tstepTotal
      Qconv_extwallroof_to_outair_tstepTotal = self%EnergyExchanges(20) !Qconv_extwallroof_to_outair_tstepTotal
      Qconv_extwindow_to_outair_tstepTotal = self%EnergyExchanges(21) !Qconv_extwindow_to_outair_tstepTotal
      q_cooling_timestepTotal = self%EnergyExchanges(22) !q_cooling_timestepTotal
      QS_tstepTotal = self%EnergyExchanges(23) !QS_tstepTotal
      QS_fabric_tstepTotal = self%EnergyExchanges(24) !QS_fabric_tstepTotal
      QS_air_tstepTotal = self%EnergyExchanges(25) !QS_air_tstepTotal
      Qloss_drain = self%qhwtDrain !Qloss_drain
      qsensible_timestepTotal = self%Qmetabolic_sensible !qsensible_timestepTotal
      qlatent_timestepTotal = self%Qmetabolic_latent !qlatent_timestepTotal

      Qtotal_water_tank = self%Qtotal_water_tank
      Twater_tank = self%Twater_tank
      Tintwall_tank = self%Tintwall_tank
      Textwall_tank = self%Textwall_tank
      Twater_vessel = self%Twater_vessel
      Tintwall_vessel = self%Tintwall_vessel
      Textwall_vessel = self%Textwall_vessel
      Vwater_vessel = self%Vwater_vessel
      Awater_vessel = self%Awater_vessel
      Vwall_vessel = self%Vwall_vessel
      Vwall_tank = self%Vwall_tank
      Vwater_tank = self%Vwater_tank

      !    bem_qf_1 = (/self%Qtotal_heating, self%Qtotal_cooling, self%EnergyExchanges(8), &
      !                 self%Qtotal_water_tank, self%Qmetabolic_sensible, self%Qmetabolic_latent/)
      !    bem_qf_1 = bem_qf_1/float(sout%timestep)
      !    qfm_dom = bem_qf_1(5) + bem_qf_1(6)
      !    qheat_dom = bem_qf_1(1)
      !    qcool_dom = bem_qf_1(2)
      !    qfb_hw_dom = bem_qf_1(4)
      !    qfb_dom_air = 0
      !    dom_temp = self%Tair_ind - 273.15 ! [K] to deg.
      !    energyEx = self%EnergyExchanges(:)/float(sout%timestep)
      !    !
      !    Qsw_transmitted_window = energyEx(1)/Area ! # transmitted solar radiation through windows [W m-2]
      !    Qsw_absorbed_window = energyEx(2)/Area ! # absorbed solar radiation by windows [W m-2]
      !    Qsw_absorbed_wallroof = energyEx(3)/Area ! #absorbed solar heat by walls [W m-2]
      !    Qlw_net_extwallroof_to_outair = energyEx(18)/Area ! # longwave radiation at external wall [W m-2]
      !    Qlw_net_extwindow_to_outair = energyEx(19)/Area ! #longwave radiation at external windows [W m-2]
      !    QStar = Qsw_transmitted_window + Qsw_absorbed_window + Qsw_absorbed_wallroof &
      !            - Qlw_net_extwallroof_to_outair - Qlw_net_extwindow_to_outair
      !    ! WRITE(*, *) 'Test: ', Qsw_transmitted_window, Qsw_absorbed_window, Qsw_absorbed_wallroof, &
      !    !  Qlw_net_extwallroof_to_outair, Qlw_net_extwindow_to_outair
      !    ! WRITE(*, *) '2: ', Qstar
      !    qinternal = (energyEx(8) + bem_qf_1(5))/Area ! #sensible internal appliance gain and sensible metabolism [W m-2]
      !    qe_cool = qcool_dom/self%coeff_performance_cooling/Area ! #energy use by cooling  [W m-2]
      !    qe_heat = qheat_dom/self%heating_efficiency_air/Area ! #energy use by heating [W m-2]
      !    QEC = qinternal + qe_cool + qe_heat ! # [W m-2] , Notice: energy use by hot water has not been added yet
      !    Qconv_extwindow_to_outair = energyEx(21)/Area ! #convection at windows [W m-2]
      !    Qconv_extwallroof_to_outair = energyEx(20)/Area ! #convection at wall [W m-2]
      !    QH = Qconv_extwallroof_to_outair + Qconv_extwindow_to_outair ! #[W m-2]
      !    qs = energyEx(23)/Area ! #heat storage/release by building fabric and indoor air  [W m-2]
      !    Qcond_ground = energyEx(17)/Area ! #conduction to external ground [W m-2], if assume ground floor is close to isolated, this flux should be close to 0
      !    QS = qs + Qcond_ground ! #[W m-2]
      !    Q_ventilation = energyEx(9)/Area ! #ventilation and infiltration
      !    QBAE = -Q_ventilation ! #[W m-2]
      !    q_waste = energyEx(22)/Area ! # waste heat from cooling  include energy consumption
      !    QWaste = q_waste ! #[W m-2]
      !    Textwallroof = self%Textwallroof ! # external surface temperature of wall [K]
      !    Tintwallroof = self%Tintwallroof ! # internal surface temperature of wall [K]
      !    Textwindow = self%Textwindow ! # external surface temperature of window [K]
      !    Tintwindow = self%Tintwindow ! # internal surface temperature of window [K]
      !    Tair_ind = self%Tair_ind ! # Indoor air temperature [K]
   END DO
   ! self%qfm_dom = qfm_dom
   ! self%qheat_dom = qheat_dom
   ! self%qcool_dom = qcool_dom
   ! self%qfb_hw_dom = qfb_hw_dom
   ! self%dom_temp = dom_temp
   ! self%QStar = QStar
   ! self%QEC = QEC
   ! self%QH = QH
   ! self%QS = QS
   ! self%QBAE = QBAE
   ! self%QWaste = QWaste
   ! self%flginit = 1
   RETURN
END SUBROUTINE suewsstebbscouple
SUBROUTINE timeStepCalculation(self, Tair_out, Tground_deep, Tsurf, &
                               density_air_out, cp_air_out, &
                               Qsw_dn_extroof, Qsw_dn_extwall, &
                               Qlw_dn_extwall, Qlw_dn_extroof, &
                               timestep, resolution, datetimeLine, flginit &
                               )
   USE modulestebbsprecision
   USE modulestebbs, ONLY: LBM
   IMPLICIT NONE
   INTEGER :: timestep, resolution
   INTEGER, INTENT(in) :: flginit
   REAL(rprc) :: Tair_out, Tground_deep, Tsurf, density_air_out, &
                 cp_air_out, Qsw_dn_extroof, Qsw_dn_extwall, &
                 Qlw_dn_extwall, Qlw_dn_extroof
   REAL(rprc), DIMENSION(5), INTENT(in) :: datetimeLine
   TYPE(LBM) :: self
   self%Qtotal_heating = 0.0
   self%Qtotal_cooling = 0.0
   self%Qtotal_water_tank = 0.0
   self%qhwtDrain = 0.0
   CALL tstep( &
      flginit, datetimeLine, Tair_out, Tground_deep, Tsurf, &
      density_air_out, cp_air_out, &
      Qsw_dn_extroof, Qsw_dn_extwall, &
      Qlw_dn_extwall, Qlw_dn_extroof, &
      self%wiTAR(1), self%wiTAR(2), self%wiTAR(3), &
      self%waTAR(1), self%waTAR(2), self%waTAR(3), &
      self%Qtotal_heating, self%Qtotal_cooling, &
      self%height_building, self%ratio_window_wall, self%thickness_wallroof, &
      self%thickness_groundfloor, self%depth_ground, self%thickness_window, &
      self%conv_coeff_intwallroof, self%conv_coeff_indoormass, &
      self%conv_coeff_intgroundfloor, self%conv_coeff_intwindow, &
      ! self%conv_coeff_extwallroof, self%conv_coeff_extwindow,                               &
      self%h_o(1), self%h_o(2), &
      self%conductivity_wallroof, self%conductivity_groundfloor, &
      self%conductivity_window, self%conductivity_ground, &
      self%density_wallroof, self%density_groundfloor, &
      self%density_window, self%density_indoormass, self%density_air_ind, &
      self%cp_wallroof, self%cp_groundfloor, self%cp_window, &
      self%cp_indoormass, self%cp_air_ind, &
      self%emissivity_extwallroof, self%emissivity_intwallroof, self%emissivity_indoormass, &
      self%emissivity_extwindow, self%emissivity_intwindow, &
      self%windowTransmissivity, self%windowAbsorbtivity, self%windowReflectivity, &
      self%wallTransmisivity, self%wallAbsorbtivity, self%wallReflectivity, &
      self%BVF_extwall, self%GVF_extwall, self%SVF_extwall, &
      self%occupants, self%metabolic_rate, self%ratio_metabolic_latent_sensible, &
      self%appliance_power_rating, self%appliance_usage_factor, &
      self%maxheatingpower_air, self%heating_efficiency_air, &
      self%maxcoolingpower_air, self%coeff_performance_cooling, &
      self%Vair_ind, self%ventilation_rate, self%Awallroof, &
      self%Vwallroof, self%Afootprint, self%Vgroundfloor, &
      self%Awindow, self%Vwindow, self%Vindoormass, self%Aindoormass, &
      self%Tair_ind, self%Tindoormass, self%Tintwallroof, self%Textwallroof, &
      self%Tintwindow, self%Textwindow, self%Tintgroundfloor, self%Textgroundfloor, &
      self%Ts, &
      !  self%Ts(1), self%Ts(2),                                                               &
      self%appliance_totalnumber, timestep, resolution, &
      self%Qtotal_water_tank, self%Twater_tank, self%Tintwall_tank, &
      self%Textwall_tank, self%thickness_tankwall, self%Tincomingwater_tank, &
      self%Vwater_tank, self%Asurf_tank, self%Vwall_tank, self%setTwater_tank, &
      self%Twater_vessel, self%Tintwall_vessel, self%Textwall_vessel, &
      self%thickness_wall_vessel, self%Vwater_vessel, self%Awater_vessel, &
      self%Vwall_vessel, self%flowrate_water_supply, self%flowrate_water_drain, &
      self%cp_water, self%cp_wall_tank, self%cp_wall_vessel, &
      self%density_water, self%density_wall_tank, self%density_wall_vessel, &
      self%BVF_tank, self%MVF_tank, self%conductivity_wall_tank, &
      self%conv_coeff_intwall_tank, self%conv_coeff_extwall_tank, &
      self%emissivity_extwall_tank, self%conductivity_wall_vessel, &
      self%conv_coeff_intwall_vessel, self%conv_coeff_extwall_vessel, &
      self%emissivity_extwall_vessel, self%HeatingPower_DHW, &
      self%heating_efficiency_water, self%minVwater_vessel, &
      self%weighting_factor_heatcapacity_wallroof, &
      !
      ! Output only variables
      !
      self%EnergyExchanges(1), & !Qsw_transmitted_window_tstepTotal
      self%EnergyExchanges(2), & !Qsw_absorbed_window_tstepTotal
      self%EnergyExchanges(3), & !Qsw_absorbed_wallroof_tstepTotal
      self%EnergyExchanges(4), & !Qconv_indair_to_indoormass_tstepTotal
      self%EnergyExchanges(5), & !Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal
      self%EnergyExchanges(6), & !Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal
      self%EnergyExchanges(7), & !Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal
      self%EnergyExchanges(8), & !Q_appliance_tstepTotal
      self%EnergyExchanges(9), & !Q_ventilation_tstepTotal
      self%EnergyExchanges(10), & !Qconv_indair_to_intwallroof_tstepTotal
      self%EnergyExchanges(11), & !Qconv_indair_to_intwindow_tstepTotal
      self%EnergyExchanges(12), & !Qconv_indair_to_intgroundfloor_tstepTotal
      self%EnergyExchanges(13), & !Qloss_efficiency_heating_air_tstepTotal
      self%EnergyExchanges(14), & !Qcond_wallroof_tstepTotal
      self%EnergyExchanges(15), & !Qcond_window_tstepTotal
      self%EnergyExchanges(16), & !Qcond_groundfloor_tstepTotal
      self%EnergyExchanges(17), & !Qcond_ground_tstepTotal
      self%EnergyExchanges(18), & !Qlw_net_extwallroof_to_outair_tstepTotal
      self%EnergyExchanges(19), & !Qlw_net_extwindow_to_outair_tstepTotal
      self%EnergyExchanges(20), & !Qconv_extwallroof_to_outair_tstepTotal
      self%EnergyExchanges(21), & !Qconv_extwindow_to_outair_tstepTotal
      self%EnergyExchanges(22), & !q_cooling_timestepTotal
      self%EnergyExchanges(23), & !QS_tstepTotal
      self%EnergyExchanges(24), & !QS_fabric_tstepTotal
      self%EnergyExchanges(25), & !QS_air_tstepTotal
      self%qhwtDrain, & !Qloss_drain
      self%Qmetabolic_sensible, & !qsensible_timestepTotal
      self%Qmetabolic_latent) !qlatent_timestepTotal
   RETURN
END SUBROUTINE timeStepCalculation
SUBROUTINE tstep( &
   flginit, datetimeLine, Tair_out, Tground_deep, Tsurf, &
   density_air_out, cp_air_out, &
   Qsw_dn_extroof, Qsw_dn_extwall, &
   Qlw_dn_extwall, Qlw_dn_extroof, &
   winT, winA, winR, walT, walA, walR, &
   Qtotal_heating, Qtotal_cooling, & !IO
   height_building, ratio_window_wall, thickness_wallroof, &
   thickness_groundfloor, depth_ground, thickness_window, &
   conv_coeff_intwallroof, conv_coeff_indoormass, &
   conv_coeff_intgroundfloor, conv_coeff_intwindow, &
   conv_coeff_extwallroof, conv_coeff_extwindow, &
   conductivity_wallroof, conductivity_groundfloor, &
   conductivity_window, conductivity_ground, &
   density_wallroof, density_groundfloor, &
   density_window, density_indoormass, density_air_ind, &
   cp_wallroof, cp_groundfloor, cp_window, cp_indoormass, cp_air_ind, &
   emissivity_extwallroof, emissivity_intwallroof, emissivity_indoormass, &
   emissivity_extwindow, emissivity_intwindow, &
   windowTransmissivity, windowAbsorbtivity, windowReflectivity, &
   wallTransmisivity, wallAbsorbtivity, wallReflectivity, &
   BVF_extwall, GVF_extwall, SVF_extwall, &
   occupants, metabolic_rate, ratio_metabolic_latent_sensible, &
   appliance_power_rating, appliance_usage_factor, &
   maxheatingpower_air, heating_efficiency_air, &
   maxcoolingpower_air, coeff_performance_cooling, &
   Vair_ind, ventilation_rate, Awallroof, &
   Vwallroof, Afootprint, Vgroundfloor, &
   Awindow, Vwindow, Vindoormass, Aindoormass, &
   Tair_ind, Tindoormass, Tintwallroof, Textwallroof, & !IO
   Tintwindow, Textwindow, Tintgroundfloor, Textgroundfloor, & !IO
   Ts, & !IO
   !  Ts(1), Ts(2),                                                          &
   appliance_totalnumber, timestep, resolution, &
   Qtotal_water_tank, Twater_tank, Tintwall_tank, & !IO
   Textwall_tank, thickness_tankwall, Tincomingwater_tank, & !IO
   Vwater_tank, Asurf_tank, Vwall_tank, setTwater_tank, & !IO
   Twater_vessel, Tintwall_vessel, Textwall_vessel, & !IO
   thickness_wall_vessel, Vwater_vessel, Awater_vessel, & !IO
   Vwall_vessel, flowrate_water_supply, flowrate_water_drain, &
   cp_water, cp_wall_tank, cp_wall_vessel, &
   density_water, density_wall_tank, density_wall_vessel, &
   BVF_tank, MVF_tank, conductivity_wall_tank, &
   conv_coeff_intwall_tank, conv_coeff_extwall_tank, &
   emissivity_extwall_tank, conductivity_wall_vessel, &
   conv_coeff_intwall_vessel, conv_coeff_extwall_vessel, &
   emissivity_extwall_vessel, maxheatingpower_water, &
   heating_efficiency_water, minVwater_vessel, &
   weighting_factor_heatcapacity_wallroof, &
   !
   ! Output only variables
   !
   Qsw_transmitted_window_tstepTotal, & !EE(1)
   Qsw_absorbed_window_tstepTotal, & !EE(2)
   Qsw_absorbed_wallroof_tstepTotal, & !EE(3)
   Qconv_indair_to_indoormass_tstepTotal, & !EE(4)
   Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal, & !EE(5)
   Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal, & !EE(6)
   Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal, & !EE(7)
   Q_appliance_tstepTotal, & !EE(8)
   Q_ventilation_tstepTotal, Qconv_indair_to_intwallroof_tstepTotal, & !EE(9),(10)
   Qconv_indair_to_intwindow_tstepTotal, & !EE(11)
   Qconv_indair_to_intgroundfloor_tstepTotal, & !EE(12)
   Qloss_efficiency_heating_air_tstepTotal, & !EE(13)
   Qcond_wallroof_tstepTotal, Qcond_window_tstepTotal, & !EE(14),(15)
   Qcond_groundfloor_tstepTotal, Qcond_ground_tstepTotal, & !EE(16),(17)
   Qlw_net_extwallroof_to_outair_tstepTotal, & !EE(18)
   Qlw_net_extwindow_to_outair_tstepTotal, & !EE(19)
   Qconv_extwallroof_to_outair_tstepTotal, & !EE(20)
   Qconv_extwindow_to_outair_tstepTotal, & !EE(21)
   q_cooling_timestepTotal, & !EE(22)
   QS_tstepTotal, QS_fabric_tstepTotal, QS_air_tstepTotal, & !EE(23,24,25)
   Qloss_drain, & !qhwtDrain
   qsensible_timestepTotal, qlatent_timestepTotal) !Qmetabolic_sensible, Qmetabolic_latent
   USE modulestebbsprecision
   USE modulestebbsfunc
   IMPLICIT NONE
   INTEGER, INTENT(in) :: flginit
   REAL(rprc), DIMENSION(5), INTENT(in) :: datetimeLine
   INTEGER :: i
   REAL(rprc) :: Tair_out, Tground_deep, Tsurf, &
                 density_air_out, cp_air_out, Qsw_dn_extroof, &
                 Qsw_dn_extwall, Qlw_dn_extwall, Qlw_dn_extroof
   REAL(rprc) :: Twater_tank, & ! Water temperature in Hot Water Tank [K]
                 Tintwall_tank, & ! Hot water tank internal wall temperature [K]
                 Textwall_tank ! Hot water tank external wall temperature [K]
   REAL(rprc) :: dTwater_tank = 0.0, dTintwall_tank = 0.0, dTextwall_tank = 0.0
   REAL(rprc) :: thickness_tankwall ! Hot water tank wall thickness [m]
   REAL(rprc) :: Tincomingwater_tank ! Water temperature of Water coming into the Water Tank [K]
   REAL(rprc) :: Vwater_tank, & ! Volume of Water in Hot Water Tank [m3]
                 Asurf_tank, & ! Surface Area of Hot Water Tank [m2]
                 Vwall_tank, & ! Wall volume of Hot Water Tank [m3]
                 setTwater_tank ! Water Tank setpoint temperature [K]
   REAL(rprc) :: Twater_vessel, & ! Water temperature of water held in use in Building [K]
                 Tintwall_vessel, & ! Hot water tank internal wall temperature [K]
                 Textwall_vessel ! Hot water tank external wall temperature [K]
   REAL(rprc) :: dTwater_vessel = 0.0, dTintwall_vessel = 0.0, dTextwall_vessel = 0.0
   REAL(rprc) :: thickness_wall_vessel ! DHW vessels wall thickness [m]
   REAL(rprc) :: Vwater_vessel ! Volume of water held in use in building [m3]
   REAL(rprc) :: dVwater_vessel = 0.0 ! Change in volume of Domestic Hot Water held in use in building [m3]
   REAL(rprc) :: Awater_vessel, & ! Surface Area of Hot Water in Vessels in Building [m2]
                 Vwall_vessel, & ! Wall volume of Hot water Vessels in Building [m3]
                 flowrate_water_supply, & ! Hot Water Flow Rate [m3 s-1]
                 flowrate_water_drain ! Draining of Domestic Hot Water held in building [m3 s-1]
   REAL(rprc) :: cp_water, & ! Specific Heat Capacity of Domestic Hot Water [J kg-1 K-1]
                 cp_wall_tank, & ! Specific Heat Capacity of Hot Water Tank wall [J kg-1 K-1]
                 cp_wall_vessel ! Specific Heat Capacity of Vessels containing DHW in use in Building [J kg-1 K-1]
   REAL(rprc) :: density_water, & ! Density of water [kg m-3]
                 density_wall_tank, & ! Density of hot water tank wall [kg m-3]
                 density_wall_vessel ! Density of vessels containing DHW in use in buildings [kg m-3]
   REAL(rprc) :: BVF_tank, & ! water tank - building wall view factor [-]
                 MVF_tank ! water tank - building internal mass view factor [-]
   REAL(rprc) :: conductivity_wall_tank, & ! Effective Wall conductivity of the Hot Water Tank [W m-1 K-1]
                 conv_coeff_intwall_tank, & ! Effective Internal Wall convection coefficient of the Hot Water Tank [W m-2 K-1]
                 conv_coeff_extwall_tank, & ! Effective External Wall convection coefficient of the Hot Water Tank [W m-2 K-1]
                 emissivity_extwall_tank, & ! Effective External Wall emissivity of the Hot Water Tank [-]
                 conductivity_wall_vessel, & ! Effective Conductivity of vessels containing DHW in use in Building [W m-1 K-1]
                 conv_coeff_intwall_vessel, & ! Effective Internal Wall convection coefficient of the Vessels holding DHW in use in Building [W m-2 K-1]
                 conv_coeff_extwall_vessel, & ! Effective Enternal Wall convection coefficient of the Vessels holding DHW in use in Building [W m-2 K-1]
                 emissivity_extwall_vessel ! Effective External Wall emissivity of hot water being used within building [-]
   REAL(rprc) :: maxheatingpower_water, & ! [deg C]
                 heating_efficiency_water ! [-]
   REAL(rprc) :: winT, & ! // window transmisivity [-]
                 winA, & ! // window absorptivity [-]
                 winR, & ! // window reflectivity [-]
                 walT, & ! // wall transmisivity [-]
                 walA, & ! // wall absorptivity [-]
                 walR ! // wall reflectivity [-]
   REAL(rprc) :: Qtotal_heating, & ! // currently only sensible but this needs to be  split into sensible and latent heat components
                 Qtotal_cooling ! // currently only sensible but this needs to be  split into sensible and latent heat components
   REAL(rprc) :: height_building, ratio_window_wall, & ! [m], [-]
                 thickness_wallroof, thickness_groundfloor, depth_ground, thickness_window, & ! [m], [m], [m], [m]
                 !    //float height_building, width, depth, ratio_window_wall, thickness_wallroof, thickness_groundfloor, depth_ground, thickness_window;
                 conv_coeff_intwallroof, conv_coeff_indoormass, & ! [W m-2 K-1], [W m-2 K-1]
                 conv_coeff_intgroundfloor, conv_coeff_intwindow, & ! [W m-2 K-1], [W m-2 K-1]
                 conv_coeff_extwallroof, conv_coeff_extwindow, & ! [W m-2 K-1], [W m-2 K-1]
                 conductivity_wallroof, conductivity_groundfloor, & ! [W m-1 K-1], [W m-1 K-1]
                 conductivity_window, conductivity_ground, & ! [W m-1 K-1], [W m-1 K-1]
                 density_wallroof, density_groundfloor, density_window, & ! [kg m-3], [kg m-3], [kg m-3]
                 density_indoormass, density_air_ind, & ! [kg m-3], [kg m-3]
                 cp_wallroof, cp_groundfloor, cp_window, & ! [J kg-1 K-1], [J kg-1 K-1], [J kg-1 K-1]
                 cp_indoormass, cp_air_ind, & ! [J kg-1 K-1], [J kg-1 K-1]
                 emissivity_extwallroof, emissivity_intwallroof, & ! [-], [-]
                 emissivity_indoormass, emissivity_extwindow, emissivity_intwindow, & ! [-], [-], [-]
                 windowTransmissivity, windowAbsorbtivity, windowReflectivity, & ! [-], [-], [-]
                 wallTransmisivity, wallAbsorbtivity, wallReflectivity, & ! [-], [-], [-]
                 BVF_extwall, GVF_extwall, SVF_extwall ! [-], [-], [-]
   REAL(rprc) :: occupants ! Number of occupants [-]
   REAL(rprc) :: metabolic_rate, ratio_metabolic_latent_sensible, & ! [W], [-]
                 appliance_power_rating ! [W]
   INTEGER :: appliance_totalnumber ! Number of appliances [-]
   REAL(rprc) :: appliance_usage_factor, & ! Number of appliances in use [-]
                 maxheatingpower_air, heating_efficiency_air, & ! [W], [-]
                 maxcoolingpower_air, coeff_performance_cooling, & ! [W], [-]
                 Vair_ind, ventilation_rate, & ! Fixed at begining to have no natural ventilation.
                 Awallroof, Vwallroof, & ! [m2], [m3]
                 Afootprint, Vgroundfloor, & ! [m2], [m3]
                 Awindow, Vwindow, & ! [m2], [m3]
                 Vindoormass, Aindoormass ! Assumed internal mass as a cube [m3], [m2]
   REAL(rprc) :: Tair_ind, Tindoormass, Tintwallroof, Textwallroof, & ! [K], [K], [K], [K]
                 Tintwindow, Textwindow, Tintgroundfloor, Textgroundfloor ! [K], [K], [K], [K]
   REAL(rprc) :: dTair_ind = 0.0, dTindoormass = 0.0, dTintwallroof = 0.0, & ! [K], [K], [K]
                 dTextwallroof = 0.0, dTintwindow = 0.0, dTextwindow = 0.0, & ! [K], [K], [K]
                 dTintgroundfloor = 0.0, dTextgroundfloor = 0.0 ! [K], [K]
   REAL(rprc) :: Qconv_water_to_inttankwall = 0.0, & ! heat flux to internal wall of hot water tank
                 Qconv_exttankwall_to_indair = 0.0, & ! convective heat flux to external wall of hot water tank
                 Qlw_net_exttankwall_to_intwallroof = 0.0, & !
                 Qlw_net_exttankwall_to_indoormass = 0.0, & ! radiative heat flux to external wall of hot water tank
                 Qcond_tankwall = 0.0, & ! heat flux through wall of hot water tank
                 Qtotal_water_tank, & ! total heat input into water of hot water tank over simulation, hence do not equate to zero
                 Qconv_water_to_intvesselwall = 0.0, & ! heat flux to internal wall of vessels holding DHW in use in building
                 Qcond_vesselwall = 0.0, & ! heat flux through wall of vessels holding DHW in use in building
                 Qconv_extvesselwall_to_indair = 0.0, & ! convective heat flux to external wall of vessels holding DHW in use in building
                 Qlw_net_extvesselwall_to_wallroof = 0.0, &
                 Qlw_net_extvesselwall_to_indoormass = 0.0, & ! radiative heat flux to external wall of vessels holding DHW in use in building
                 !                      Qloss_drain = 0.0,                         & ! Heat loss as water held in use in building drains to sewer
                 Qloss_efficiency_heating_water = 0.0 ! additional heat release from efficieny losses/gains of heating hot water

   REAL(rprc), INTENT(out) :: Qloss_drain ! Heat loss as water held in use in building drains to sewer
   REAL(rprc) :: Qtotal_net_water_tank = 0.0, Qtotal_net_intwall_tank = 0.0, &
                 Qtotal_net_extwall_tank = 0.0, Qtotal_net_water_vessel = 0.0, &
                 Qtotal_net_intwall_vessel = 0.0, Qtotal_net_extwall_vessel = 0.0
   REAL(rprc) :: qhwt_timestep = 0.0
   REAL(rprc) :: VARatio_water_vessel = 0.0
   REAL(rprc) :: minVwater_vessel
   REAL(rprc) :: weighting_factor_heatcapacity_wallroof
   REAL(rprc), DIMENSION(2) :: Ts ! Heating and Cooling setpoint temperature (K)s, respectively
   REAL(rprc), DIMENSION(2) :: Qm ! Metabolic heat, sensible(1) and latent(2)
   INTEGER :: timestep, resolution
   REAL(rprc) :: Qf_ground_timestep = 0.0, &
                 q_heating_timestep = 0.0, &
                 q_cooling_timestep = 0.0
   REAL(rprc) :: Qsw_transmitted_window = 0.0, Qsw_absorbed_window = 0.0, Qsw_absorbed_wallroof = 0.0, &
                 Qconv_indair_to_indoormass = 0.0, Qlw_net_intwallroof_to_allotherindoorsurfaces = 0.0, &
                 Qlw_net_intwindow_to_allotherindoorsurfaces = 0.0, Qlw_net_intgroundfloor_to_allotherindoorsurfaces = 0.0
   REAL(rprc) :: Q_appliance = 0.0, Q_ventilation = 0.0, Qconv_indair_to_intwallroof = 0.0, &
                 Qconv_indair_to_intwindow = 0.0, Qconv_indair_to_intgroundfloor = 0.0
   REAL(rprc) :: Qloss_efficiency_heating_air = 0.0, Qcond_wallroof = 0.0, Qcond_window = 0.0, &
                 Qcond_groundfloor = 0.0, Qcond_ground = 0.0
   REAL(rprc) :: Qlw_net_extwallroof_to_outair = 0.0, Qlw_net_extwindow_to_outair = 0.0, &
                 Qconv_extwallroof_to_outair = 0.0, Qconv_extwindow_to_outair = 0.0
   REAL(rprc) :: QS_total = 0.0, QS_fabric = 0.0, QS_air = 0.0
   REAL(rprc), INTENT(inout) :: Qsw_transmitted_window_tstepTotal, &
                                Qsw_absorbed_window_tstepTotal, &
                                Qsw_absorbed_wallroof_tstepTotal, &
                                Qconv_indair_to_indoormass_tstepTotal, &
                                Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal, &
                                Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal, &
                                Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal
   REAL(rprc), INTENT(inout) :: Q_appliance_tstepTotal, &
                                Q_ventilation_tstepTotal, &
                                Qconv_indair_to_intwallroof_tstepTotal, &
                                Qconv_indair_to_intwindow_tstepTotal, &
                                Qconv_indair_to_intgroundfloor_tstepTotal
   REAL(rprc), INTENT(inout) :: Qloss_efficiency_heating_air_tstepTotal, &
                                Qcond_wallroof_tstepTotal, &
                                Qcond_window_tstepTotal, &
                                Qcond_groundfloor_tstepTotal, &
                                Qcond_ground_tstepTotal
   REAL(rprc), INTENT(inout) :: Qlw_net_extwallroof_to_outair_tstepTotal, &
                                Qlw_net_extwindow_to_outair_tstepTotal, &
                                Qconv_extwallroof_to_outair_tstepTotal, &
                                Qconv_extwindow_to_outair_tstepTotal
   REAL(rprc), INTENT(inout) :: q_cooling_timestepTotal, &
                                qsensible_timestepTotal, &
                                qlatent_timestepTotal
   REAL(rprc), INTENT(inout) :: QS_tstepTotal, QS_fabric_tstepTotal, QS_air_tstepTotal
   REAL(rprc) :: Qmetabolic_sensible = 0.0, Qmetabolic_latent = 0.0
   REAL(rprc) :: Qtotal_net_indoormass = 0.0, Qtotal_net_indair = 0.0, &
                 Qtotal_net_intwallroof = 0.0, Qtotal_net_extwallroof = 0.0, &
                 Qtotal_net_intwindow = 0.0, Qtotal_net_extwindow = 0.0, &
                 Qtotal_net_intgroundfloor = 0.0, Qtotal_net_extgroundfloor = 0.0
   CHARACTER(len=256) :: fout
   Qsw_transmitted_window_tstepTotal = 0.0
   Qsw_absorbed_window_tstepTotal = 0.0
   Qsw_absorbed_wallroof_tstepTotal = 0.0
   Qconv_indair_to_indoormass_tstepTotal = 0.0
   Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal = 0.0
   Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal = 0.0
   Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal = 0.0
   Q_appliance_tstepTotal = 0.0
   Q_ventilation_tstepTotal = 0.0
   Qconv_indair_to_intwallroof_tstepTotal = 0.0
   Qconv_indair_to_intwindow_tstepTotal = 0.0
   Qconv_indair_to_intgroundfloor_tstepTotal = 0.0
   Qloss_efficiency_heating_air_tstepTotal = 0.0
   Qcond_wallroof_tstepTotal = 0.0
   Qcond_window_tstepTotal = 0.0
   Qcond_groundfloor_tstepTotal = 0.0
   Qcond_ground_tstepTotal = 0.0
   Qlw_net_extwallroof_to_outair_tstepTotal = 0.0
   Qlw_net_extwindow_to_outair_tstepTotal = 0.0
   Qconv_extwallroof_to_outair_tstepTotal = 0.0
   Qconv_extwindow_to_outair_tstepTotal = 0.0
   q_cooling_timestepTotal = 0.0
   qsensible_timestepTotal = 0.0
   qlatent_timestepTotal = 0.0
   QS_tstepTotal = 0.0
   QS_fabric_tstepTotal = 0.0
   QS_air_tstepTotal = 0.0

   ! Used to recalculate Area of DHW in use
   IF (Awater_vessel > 0.0) THEN
      VARatio_water_vessel = Vwater_vessel/Awater_vessel
   END IF

   IF (MOD(timestep, resolution) == 0) THEN
      looptime: DO i = 1, INT(timestep/resolution), 1
         Qsw_transmitted_window = windowInsolation(Qsw_dn_extwall, winT, Awindow)
         Qsw_absorbed_window = windowInsolation(Qsw_dn_extwall, winA, Awindow)
         Qsw_absorbed_wallroof = &
            wallInsolation(Qsw_dn_extwall, walA, Awallroof - Afootprint) + &
            wallInsolation(Qsw_dn_extroof, walA, Afootprint) !//separate the wall and roof
         Qconv_indair_to_indoormass = internalConvectionHeatTransfer(conv_coeff_indoormass, Aindoormass, Tindoormass, Tair_ind)
         Qlw_net_intwallroof_to_allotherindoorsurfaces = indoorRadiativeHeatTransfer() ! //  for wall internal radiative exchange
         Qlw_net_intwindow_to_allotherindoorsurfaces = Qlw_net_intwallroof_to_allotherindoorsurfaces ! //  for window internal radiative exchange - TODO: currently no distinction in internal radiative exchanges
         Qlw_net_intgroundfloor_to_allotherindoorsurfaces = Qlw_net_intwallroof_to_allotherindoorsurfaces ! //  for ground floor internal radiative exchange - TODO: currently no distinction in internal radiative exchanges
         Q_appliance = &
            internalApplianceGains(appliance_power_rating, appliance_usage_factor, appliance_totalnumber)
         Q_ventilation = &
            ventilationHeatTransfer(density_air_ind, cp_air_ind, ventilation_rate, Tair_out, Tair_ind)
         Qconv_indair_to_intwallroof = &
            indoorConvectionHeatTransfer(conv_coeff_intwallroof, Awallroof, Tintwallroof, Tair_ind)
         Qconv_indair_to_intwindow = &
            indoorConvectionHeatTransfer(conv_coeff_intwindow, Awindow, Tintwindow, Tair_ind)
         Qconv_indair_to_intgroundfloor = &
            indoorConvectionHeatTransfer(conv_coeff_intgroundfloor, Afootprint, Tintgroundfloor, Tair_ind)
         q_heating_timestep = heating(Ts(1), Tair_ind, heating_efficiency_air, maxheatingpower_air)
         q_cooling_timestep = cooling(Ts(2), Tair_ind, coeff_performance_cooling, maxcoolingpower_air)

         !internalOccupancyGains(occupants, metabolic_rate, ratio_metabolic_latent_sensible, Qmetabolic_sensible, Qmetabolic_latent)
         Qm = internalOccupancyGains(occupants, metabolic_rate, ratio_metabolic_latent_sensible)
         Qmetabolic_sensible = Qm(1)
         Qmetabolic_latent = Qm(2)
         Qloss_efficiency_heating_air = &
            additionalSystemHeatingEnergy(q_heating_timestep, heating_efficiency_air)
         Qcond_wallroof = &
            wallConduction(conductivity_wallroof, Awallroof, Tintwallroof, Textwallroof, thickness_wallroof)
         Qcond_window = &
            windowConduction(conductivity_window, Awindow, Tintwindow, Textwindow, thickness_window)
         Qcond_groundfloor = &
            wallConduction(conductivity_groundfloor, Afootprint, Tintgroundfloor, Textgroundfloor, thickness_groundfloor)
         Qcond_ground = &
            wallConduction(conductivity_ground, Afootprint, Textgroundfloor, Tground_deep, depth_ground)
         ! ; // conduction from ground floor to external ground - depth of the ground can be set (depth_ground) and ground temperature should be determined accordingly..
         ! //add the longwave radiation between sky and external envelope; Assume surrounding surface and ground surface temperature is the same (e.g. assumed outdoor air temperature as in EnergyPlus)
         ! // Qlw_net_extwallroof_to_outair = outdoorRadiativeHeatTransfer(BVF_extwall, Awallroof, emissivity_extwallroof, Textwallroof, Tsurf)+outdoorRadiativeHeatTransfer(SVF_extwall, Awallroof, emissivity_extwallroof, Textwallroof, Tsky);
         ! // Qlw_net_extwindow_to_outair = outdoorRadiativeHeatTransfer(BVF_extwall, Awindow, emissivity_extwindow, Textwindow, Tsurf)+outdoorRadiativeHeatTransfer(SVF_extwall, Awindow, emissivity_extwindow, Textwindow, Tsky);
         ! // call outdoorRadiativeHeatTransfer with LW instead of temp
         Qlw_net_extwallroof_to_outair = &
            lwoutdoorRadiativeHeatTransfer &
            (Awallroof, emissivity_extwallroof, Textwallroof, &
             ((Qlw_dn_extwall*(Awallroof - Afootprint)) + (Qlw_dn_extroof*Afootprint))/Awallroof)
         Qlw_net_extwindow_to_outair = lwoutdoorRadiativeHeatTransfer(Awindow, emissivity_extwindow, Textwindow, Qlw_dn_extwall)
         Qconv_extwallroof_to_outair = outdoorConvectionHeatTransfer(conv_coeff_extwallroof, Awallroof, Textwallroof, Tair_out)
         Qconv_extwindow_to_outair = outdoorConvectionHeatTransfer(conv_coeff_extwindow, Awindow, Textwindow, Tair_out)

         ifVwater_tank: IF (Vwater_tank > 0.0) THEN
            ! // convective heat flux to internal wall of hot water tank
            Qconv_water_to_inttankwall = &
               indoorConvectionHeatTransfer &
               (conv_coeff_intwall_tank, Asurf_tank, Tintwall_tank, Twater_tank)

            ! // heat flux by conduction through wall of hot water tank
            Qcond_tankwall = &
               wallConduction &
               (conductivity_wall_tank, Asurf_tank, Tintwall_tank, Textwall_tank, thickness_tankwall)

            ! // convective heat flux for external wall of hot water tank
            Qconv_exttankwall_to_indair = &
               outdoorConvectionHeatTransfer &
               (conv_coeff_extwall_tank, Asurf_tank, Textwall_tank, Tair_ind)
            ! // radiative heat flux for external wall of hot water tank
            ! //TODO: Should expand to consider windows and floor as well.

            Qlw_net_exttankwall_to_intwallroof = &
               outdoorRadiativeHeatTransfer &
               (BVF_tank, Asurf_tank, emissivity_extwall_tank, Textwall_tank, Tintwallroof) ! // to building walls
            Qlw_net_exttankwall_to_indoormass = &
               outdoorRadiativeHeatTransfer &
               (MVF_tank, Asurf_tank, emissivity_extwall_tank, Textwall_tank, Tindoormass) ! // to internal mass

            ! // heat input into water of hot water tank
            qhwt_timestep = &
               heating &
               (setTwater_tank, Twater_tank, heating_efficiency_water, maxheatingpower_water)

            ! //Heat release from hot water heating due to efficiency losses
            Qloss_efficiency_heating_water = &
               additionalSystemHeatingEnergy(qhwt_timestep, heating_efficiency_water)
         END IF ifVwater_tank
         ifVwater_vessel: IF (Vwater_vessel > 0.0) THEN
            ! // heat flux to internal wall of vessels holding DHW in use in building
            Qconv_water_to_intvesselwall = &
               indoorConvectionHeatTransfer &
               (conv_coeff_intwall_vessel, Awater_vessel, Tintwall_vessel, Twater_vessel)

            ! // heat flux by conduction through wall of vessels holding DHW in use in building
            Qcond_vesselwall = &
               wallConduction &
               (conductivity_wall_vessel, Awater_vessel, Tintwall_vessel, Textwall_vessel, thickness_wall_vessel)

            ! // convective heat flux to external wall of vessels holding DHW in use in building
            Qconv_extvesselwall_to_indair = &
               outdoorConvectionHeatTransfer &
               (conv_coeff_extwall_vessel, Awater_vessel, Textwall_vessel, Tair_ind)
            ! // radiative heat flux to external wall of vessels holding DHW in use in building
            ! // TODO: Should expand to consider windows and floor as well.
            Qlw_net_extvesselwall_to_wallroof = &
               outdoorRadiativeHeatTransfer &
               (BVF_tank, Awater_vessel, emissivity_extwall_vessel, Textwall_vessel, Tintwallroof)
            Qlw_net_extvesselwall_to_indoormass = &
               outdoorRadiativeHeatTransfer &
               (MVF_tank, Awater_vessel, emissivity_extwall_vessel, Textwall_vessel, Tindoormass)

            ! //Heat transfer due to use and replacement of water
            ! //qhwt_v = waterUseHeatTransfer(density_water, cp_water, flowrate_water_supply, Tincomingwater_tank, Twater_tank)
         ELSEIF (Vwater_vessel == minVwater_vessel .AND. &
                 flowrate_water_supply < flowrate_water_drain) THEN
            ! //Set Drain Flow rate to be same as usage flow rate to avoid hot water storage going below the set minimum threshold (minVwater_vessel)
            flowrate_water_drain = flowrate_water_supply
         END IF ifVwater_vessel

         Qtotal_net_water_tank = qhwt_timestep - Qconv_water_to_inttankwall
         Qtotal_net_intwall_tank = Qconv_water_to_inttankwall - Qcond_tankwall
         Qtotal_net_extwall_tank = &
            Qcond_tankwall - Qconv_exttankwall_to_indair - &
            Qlw_net_exttankwall_to_intwallroof - Qlw_net_exttankwall_to_indoormass
         Qtotal_net_water_vessel = -Qconv_water_to_intvesselwall
         Qtotal_net_intwall_vessel = Qconv_water_to_intvesselwall - Qcond_vesselwall

         Qtotal_net_extwall_vessel = &
            Qcond_vesselwall - Qconv_extvesselwall_to_indair - &
            Qlw_net_extvesselwall_to_wallroof - Qlw_net_extvesselwall_to_indoormass

         Qtotal_net_indoormass = &
            Qsw_transmitted_window + Qconv_indair_to_indoormass + &
            Qlw_net_intwallroof_to_allotherindoorsurfaces + &
            Qlw_net_exttankwall_to_indoormass + Qlw_net_extvesselwall_to_indoormass
         Qtotal_net_indair = &
            Q_appliance + Qmetabolic_sensible + Q_ventilation + &
            q_heating_timestep - q_cooling_timestep - Qconv_indair_to_indoormass - &
            Qlw_net_intwallroof_to_allotherindoorsurfaces - Qconv_indair_to_intwallroof - &
            Qconv_indair_to_intwindow - Qconv_indair_to_intgroundfloor + &
            Qloss_efficiency_heating_air + Qconv_exttankwall_to_indair + &
            Qconv_extvesselwall_to_indair + Qloss_efficiency_heating_water
         Qtotal_net_intwallroof = &
            Qconv_indair_to_intwallroof - Qcond_wallroof - &
            Qlw_net_intwallroof_to_allotherindoorsurfaces + &
            Qlw_net_exttankwall_to_intwallroof + Qlw_net_extvesselwall_to_wallroof
         Qtotal_net_extwallroof = &
            Qcond_wallroof + Qsw_absorbed_wallroof - &
            Qlw_net_extwallroof_to_outair - Qconv_extwallroof_to_outair

         Qtotal_net_intwindow = &
            Qconv_indair_to_intwindow - Qcond_window - &
            Qlw_net_intwindow_to_allotherindoorsurfaces

         Qtotal_net_extwindow = &
            Qcond_window + Qsw_absorbed_window - &
            Qlw_net_extwindow_to_outair - Qconv_extwindow_to_outair
         Qtotal_net_intgroundfloor = &
            Qconv_indair_to_intgroundfloor - Qcond_groundfloor - &
            Qlw_net_intgroundfloor_to_allotherindoorsurfaces

         Qtotal_net_extgroundfloor = Qcond_groundfloor - Qcond_ground

         QS_total = &
            Qtotal_net_extwallroof + Qtotal_net_intwallroof + &
            Qtotal_net_extwindow + Qtotal_net_intwindow + &
            Qtotal_net_extgroundfloor + Qtotal_net_intgroundfloor + &
            Qtotal_net_indoormass + Qtotal_net_indair
         QS_fabric = &
            Qtotal_net_extwallroof + Qtotal_net_intwallroof + Qtotal_net_extwindow + &
            Qtotal_net_intwindow + Qtotal_net_extgroundfloor + Qtotal_net_intgroundfloor + &
            Qtotal_net_indoormass
         QS_air = Qtotal_net_indair
         Qf_ground_timestep = Qcond_ground*resolution
         Qsw_transmitted_window_tstepTotal = &
            Qsw_transmitted_window_tstepTotal + Qsw_transmitted_window*resolution
         Qsw_absorbed_window_tstepTotal = &
            Qsw_absorbed_window_tstepTotal + Qsw_absorbed_window*resolution
         Qsw_absorbed_wallroof_tstepTotal = &
            Qsw_absorbed_wallroof_tstepTotal + Qsw_absorbed_wallroof*resolution
         Qconv_indair_to_indoormass_tstepTotal = &
            Qconv_indair_to_indoormass_tstepTotal + Qconv_indair_to_indoormass*resolution
         Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal = &
            Qlw_net_intwallroof_to_allotherindoorsurfaces_tstepTotal + &
            Qlw_net_intwallroof_to_allotherindoorsurfaces*resolution
         Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal = &
            Qlw_net_intwindow_to_allotherindoorsurfaces_tstepTotal + &
            Qlw_net_intwindow_to_allotherindoorsurfaces*resolution
         Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal = &
            Qlw_net_intgroundfloor_to_allotherindoorsurfaces_tstepTotal + &
            Qlw_net_intgroundfloor_to_allotherindoorsurfaces*resolution
         Q_appliance_tstepTotal = Q_appliance_tstepTotal + Q_appliance*resolution
         Q_ventilation_tstepTotal = Q_ventilation_tstepTotal + Q_ventilation*resolution
         Qconv_indair_to_intwallroof_tstepTotal = &
            Qconv_indair_to_intwallroof_tstepTotal + &
            Qconv_indair_to_intwallroof*resolution
         Qconv_indair_to_intwindow_tstepTotal = &
            Qconv_indair_to_intwindow_tstepTotal + Qconv_indair_to_intwindow*resolution
         Qconv_indair_to_intgroundfloor_tstepTotal = &
            Qconv_indair_to_intgroundfloor_tstepTotal + Qconv_indair_to_intgroundfloor*resolution

         Qloss_efficiency_heating_air_tstepTotal = &
            Qloss_efficiency_heating_air_tstepTotal + Qloss_efficiency_heating_air*resolution

         Qcond_wallroof_tstepTotal = Qcond_wallroof_tstepTotal + Qcond_wallroof*resolution

         Qcond_window_tstepTotal = Qcond_window_tstepTotal + Qcond_window*resolution

         Qcond_groundfloor_tstepTotal = Qcond_groundfloor_tstepTotal + Qcond_groundfloor*resolution

         Qcond_ground_tstepTotal = Qcond_ground_tstepTotal + Qcond_ground*resolution

         Qlw_net_extwallroof_to_outair_tstepTotal = &
            Qlw_net_extwallroof_to_outair_tstepTotal + Qlw_net_extwallroof_to_outair*resolution

         Qlw_net_extwindow_to_outair_tstepTotal = &
            Qlw_net_extwindow_to_outair_tstepTotal + Qlw_net_extwindow_to_outair*resolution

         Qconv_extwallroof_to_outair_tstepTotal = &
            Qconv_extwallroof_to_outair_tstepTotal + Qconv_extwallroof_to_outair*resolution

         Qconv_extwindow_to_outair_tstepTotal = &
            Qconv_extwindow_to_outair_tstepTotal + Qconv_extwindow_to_outair*resolution

         q_cooling_timestepTotal = &
            q_cooling_timestepTotal + &
            (q_cooling_timestep + additionalSystemCoolingEnergy(q_cooling_timestep, coeff_performance_cooling))*resolution

         qsensible_timestepTotal = qsensible_timestepTotal + Qmetabolic_sensible*resolution

         qlatent_timestepTotal = qlatent_timestepTotal + Qmetabolic_latent*resolution
         QS_tstepTotal = QS_tstepTotal + QS_total*resolution
         QS_fabric_tstepTotal = QS_fabric_tstepTotal + QS_fabric*resolution
         QS_air_tstepTotal = QS_air_tstepTotal + QS_air*resolution
         Qtotal_heating = Qtotal_heating + (q_heating_timestep*resolution)
         Qtotal_cooling = Qtotal_cooling + (q_cooling_timestep*resolution)
         Qtotal_water_tank = Qtotal_water_tank + (qhwt_timestep*resolution)
         IF (Vwater_vessel > 0.0) THEN
            dTwater_vessel = &
               (Qtotal_net_water_vessel/(density_water*cp_water)*Vwater_vessel)*resolution

            Twater_vessel = Twater_vessel + dTwater_vessel
         END IF
         IF (Vwall_vessel > 0.0) THEN
            dTintwall_vessel = &
               (Qtotal_net_intwall_vessel/((density_wall_vessel*cp_wall_vessel)*(Vwall_vessel/2)))*resolution
            Tintwall_vessel = Tintwall_vessel + dTintwall_vessel

            dTextwall_vessel = &
               (Qtotal_net_extwall_vessel/((density_wall_vessel*cp_wall_vessel)*(Vwall_vessel/2)))*resolution
            Textwall_vessel = Textwall_vessel + dTextwall_vessel
         END IF
         Qloss_drain = &
            waterUseEnergyLossToDrains(density_water, cp_water, flowrate_water_drain, Twater_vessel, resolution)
         dVwater_vessel = (flowrate_water_supply - flowrate_water_drain)*resolution
         Vwater_vessel = Vwater_vessel + dVwater_vessel
         IF (Vwater_vessel < minVwater_vessel) THEN
            Vwater_vessel = minVwater_vessel
         END IF
         Twater_vessel = &
            (((flowrate_water_supply*resolution)*Twater_tank) + &
             ((Vwater_vessel - (flowrate_water_supply*resolution))*Twater_vessel))/Vwater_vessel
         IF (Vwater_vessel > 0.0) THEN ! //Checks that Volume isn't zero
            Awater_vessel = Vwater_vessel/VARatio_water_vessel
         ELSE ! // if Volume is zero it makes the area also zero
            Awater_vessel = 0.0
         END IF
         Vwall_vessel = Awater_vessel*thickness_wall_vessel
         IF (Vwater_tank > 0.0) THEN
            dTwater_tank = (Qtotal_net_water_tank/((density_water*cp_water)*Vwater_tank))*resolution !//(Q_hwt/((density_water*cp_water)*Vwater_tank))*resolution ! // resolution in seconds
            Twater_tank = Twater_tank + dTwater_tank
         END IF
         dTintwall_tank = &
            (Qtotal_net_intwall_tank/((density_wall_tank*cp_wall_tank)*(Vwall_tank/2)))*resolution
         Tintwall_tank = Tintwall_tank + dTintwall_tank
         dTextwall_tank = &
            (Qtotal_net_extwall_tank/((density_wall_tank*cp_wall_tank)*(Vwall_tank/2)))*resolution
         Textwall_tank = Textwall_tank + dTextwall_tank
         Twater_tank = &
            (((flowrate_water_supply*resolution)*Tincomingwater_tank) + &
             ((Vwater_tank - (flowrate_water_supply*resolution))*Twater_tank))/Vwater_tank
         dTindoormass = (Qtotal_net_indoormass/((density_indoormass*cp_indoormass)*Vindoormass))*resolution ! // resolution in seconds
         Tindoormass = Tindoormass + dTindoormass
         dTair_ind = (Qtotal_net_indair/((density_air_ind*cp_air_ind)*Vair_ind))*resolution ! // resolution in seconds
         Tair_ind = Tair_ind + dTair_ind
         dTintwallroof = &
            (Qtotal_net_intwallroof/((density_wallroof*cp_wallroof)* &
                                     (Vwallroof*(1 - weighting_factor_heatcapacity_wallroof))))*resolution ! // resolution in seconds
         Tintwallroof = Tintwallroof + dTintwallroof
         dTextwallroof = &
            (Qtotal_net_extwallroof/((density_wallroof*cp_wallroof)* &
                                     (Vwallroof*weighting_factor_heatcapacity_wallroof)))*resolution !  // resolution in seconds
         Textwallroof = Textwallroof + dTextwallroof
         dTintwindow = (Qtotal_net_intwindow/((density_window*cp_window)*(Vwindow/2)))*resolution ! // resolution in seconds
         Tintwindow = Tintwindow + dTintwindow
         dTextwindow = (Qtotal_net_extwindow/((density_window*cp_window)*(Vwindow/2)))*resolution ! // resolution in seconds
         Textwindow = Textwindow + dTextwindow
         dTintgroundfloor = &
            (Qtotal_net_intgroundfloor/((density_groundfloor*cp_groundfloor)*(Vgroundfloor/2)))* &
            resolution ! // resolution in seconds
         Tintgroundfloor = Tintgroundfloor + dTintgroundfloor
         dTextgroundfloor = &
            (Qtotal_net_extgroundfloor/((density_groundfloor*cp_groundfloor)*(Vgroundfloor/2)))* &
            resolution !  // resolution in seconds
         Textgroundfloor = Textgroundfloor + dTextgroundfloor

      END DO looptime
   ELSE !iftimestepresolution
      !  printf("Timestep: %i not equally divisible by given resolution: %i.\n", timestep, resolution)
      WRITE (*, *) "Timestep: ", timestep, " not equally divisible by given resolution: ", resolution
   END IF
END SUBROUTINE tstep
SUBROUTINE reinitialiseTemperatures
END SUBROUTINE reinitialiseTemperatures

SUBROUTINE gen_building(stebbsState, stebbsPrm, building_archtype, self)

   USE modulestebbs, ONLY: LBM
   USE SUEWS_DEF_DTS, ONLY: BUILDING_ARCHETYPE_PRM, STEBBS_STATE, STEBBS_PRM
   IMPLICIT NONE
   TYPE(LBM) :: self

   TYPE(STEBBS_STATE), INTENT(IN) :: stebbsState
   TYPE(BUILDING_ARCHETYPE_PRM), INTENT(IN) :: building_archtype
   TYPE(STEBBS_PRM), INTENT(IN) :: stebbsPrm
   ! self%idLBM = bldgState%BuildingName

   self%Qtotal_heating = 0.0 ! currently only sensible but this needs to be  split into sensible and latent heat components
   self%Qtotal_cooling = 0.0 ! currently only sensible but this needs to be  split into sensible and latent heat components

   self%Qmetabolic_sensible = 0.0 ! # Sensible heat flux from people in building
   self%Qmetabolic_latent = 0.0 ! # Latent heat flux from people in building

   self%Qtotal_water_tank = 0.0
   self%qhwtDrain = 0.0
   self%EnergyExchanges(:) = 0.0

   ! self%BuildingType = bldgState%BuildingType
   ! self%BuildingName = bldgState%BuildingName
   self%ratio_window_wall = building_archtype%WWR
   self%Afootprint = building_archtype%FootprintArea
   self%height_building = building_archtype%stebbs_Height
   self%wallExternalArea = building_archtype%WallExternalArea
   self%ratioInternalVolume = building_archtype%RatioInternalVolume
   self%thickness_wallroof = building_archtype%WallThickness
   self%thickness_groundfloor = building_archtype%FloorThickness
   self%depth_ground = stebbsPrm%GroundDepth
   self%thickness_window = building_archtype%WindowThickness
   self%conv_coeff_intwallroof = stebbsPrm%WallInternalConvectionCoefficient
   self%conv_coeff_indoormass = stebbsPrm%InternalMassConvectionCoefficient
   self%conv_coeff_intgroundfloor = stebbsPrm%FloorInternalConvectionCoefficient
   self%conv_coeff_intwindow = stebbsPrm%WindowInternalConvectionCoefficient
   self%conv_coeff_extwallroof = stebbsPrm%WallExternalConvectionCoefficient
   self%conv_coeff_extwindow = stebbsPrm%WindowExternalConvectionCoefficient
   self%conductivity_wallroof = building_archtype%WallEffectiveConductivity
   self%conductivity_groundfloor = building_archtype%GroundFloorEffectiveConductivity
   self%conductivity_window = building_archtype%WindowEffectiveConductivity
   self%conductivity_ground = building_archtype%GroundFloorEffectiveConductivity
   self%density_wallroof = building_archtype%WallDensity
   self%weighting_factor_heatcapacity_wallroof = building_archtype%Wallx1
   self%density_groundfloor = building_archtype%GroundFloorDensity
   self%density_window = building_archtype%WindowDensity
   self%density_indoormass = building_archtype%InternalMassDensity
   self%density_air_ind = stebbsPrm%IndoorAirDensity
   self%cp_wallroof = building_archtype%WallCp
   self%cp_groundfloor = building_archtype%GroundFloorCp
   self%cp_window = building_archtype%WindowCp
   self%cp_indoormass = building_archtype%InternalMassCp
   self%cp_air_ind = stebbsPrm%IndoorAirCp
   self%emissivity_extwallroof = building_archtype%WallExternalEmissivity
   self%emissivity_intwallroof = building_archtype%WallInternalEmissivity
   self%emissivity_indoormass = building_archtype%InternalMassEmissivity
   self%emissivity_extwindow = building_archtype%WindowExternalEmissivity
   self%emissivity_intwindow = building_archtype%WindowInternalEmissivity
   self%windowTransmissivity = building_archtype%WindowTransmissivity
   self%windowAbsorbtivity = building_archtype%WindowAbsorbtivity
   self%windowReflectivity = building_archtype%WindowReflectivity
   self%wallTransmisivity = building_archtype%WallTransmissivity
   self%wallAbsorbtivity = building_archtype%WallAbsorbtivity
   self%wallReflectivity = building_archtype%WallReflectivity
   self%BVF_extwall = stebbsPrm%WallBuildingViewFactor
   self%GVF_extwall = stebbsPrm%WallGroundViewFactor
   self%SVF_extwall = stebbsPrm%WallSkyViewFactor
   self%occupants = building_archtype%Occupants
   self%metabolic_rate = stebbsPrm%MetabolicRate
   self%ratio_metabolic_latent_sensible = stebbsPrm%LatentSensibleRatio
   self%appliance_power_rating = stebbsPrm%ApplianceRating
   self%appliance_totalnumber = INT(stebbsPrm%TotalNumberofAppliances)
   self%appliance_usage_factor = stebbsPrm%ApplianceUsageFactor
   self%maxheatingpower_air = building_archtype%MaxHeatingPower
   self%heating_efficiency_air = stebbsPrm%HeatingSystemEfficiency
   self%maxcoolingpower_air = stebbsPrm%MaxCoolingPower
   self%coeff_performance_cooling = stebbsPrm%CoolingSystemCOP
   self%Vair_ind = &
      (self%Afootprint*self%height_building)* &
      (1 - self%ratioInternalVolume) ! # Multiplied by factor that accounts for internal mass
   self%ventilation_rate = self%Vair_ind*stebbsPrm%VentilationRate/3600.0 ! Fixed at begining to have no natural ventilation. Given in units of volume of air per second
   self%Awallroof = &
      (self%wallExternalArea*(1 - self%ratio_window_wall)) + &
      self%Afootprint ! # last component accounts for the roof as not considered seperately in the model
   self%Vwallroof = self%Awallroof*self%thickness_wallroof
   self%Vgroundfloor = self%Afootprint*self%thickness_groundfloor
   self%Awindow = self%wallExternalArea*self%ratio_window_wall
   self%Vwindow = self%Awindow*self%thickness_window
   self%Vindoormass = self%Vair_ind*self%ratioInternalVolume ! # Multiplied by factor that accounts for internal mass as proportion of total air volume
   self%Aindoormass = 6*(self%Vindoormass**(2./3.)) ! # Assumed internal mass as a cube
   self%h_i = (/self%conv_coeff_intwallroof, self%conv_coeff_indoormass, &
                self%conv_coeff_intgroundfloor, self%conv_coeff_intwindow/)

   self%h_o = (/self%conv_coeff_extwallroof, self%conv_coeff_extwindow/)
   self%k_eff = (/self%conductivity_wallroof, self%conductivity_groundfloor, &
                  self%conductivity_window, self%conductivity_ground/)

   self%rho = (/self%density_wallroof, self%density_groundfloor, &
                self%density_window, self%density_indoormass, &
                self%density_air_ind/)
   self%Cp = (/self%cp_wallroof, self%cp_groundfloor, self%cp_window, &
               self%cp_indoormass, self%cp_air_ind/)
   self%emis = (/self%emissivity_extwallroof, self%emissivity_intwallroof, &
                 self%emissivity_indoormass, self%emissivity_extwindow, &
                 self%emissivity_intwindow/)
   self%wiTAR = (/self%windowTransmissivity, self%windowAbsorbtivity, self%windowReflectivity/)
   self%waTAR = (/self%wallTransmisivity, self%wallAbsorbtivity, self%wallReflectivity/)

   self%viewFactors = (/self%BVF_extwall, self%GVF_extwall, self%SVF_extwall/) !  # Building, ground, and sky view factors
   self%occupantData = (/self%occupants, self%metabolic_rate, &
                         self%ratio_metabolic_latent_sensible/)

   self%Tair_ind = stebbsState%IndoorAirStartTemperature + 273.15 ! # Indoor air temperature (K)
   self%Tindoormass = stebbsState%IndoorMassStartTemperature + 273.15 ! # Indoor mass temperature (K)
   self%Tintwallroof = stebbsState%WallIndoorSurfaceTemperature + 273.15 ! # Wall indoor surface temperature (K)
   self%Textwallroof = stebbsState%WallOutdoorSurfaceTemperature + 273.15 ! # Wall outdoor surface temperature (K)
   self%Tintwindow = stebbsState%WindowIndoorSurfaceTemperature + 273.15 ! # Window indoor surface temperature (K)
   self%Textwindow = stebbsState%WindowOutdoorSurfaceTemperature + 273.15 ! # Window outdoor surface temperature (K)
   self%Tintgroundfloor = stebbsState%GroundFloorIndoorSurfaceTemperature + 273.15 ! # Ground floor indoor surface temperature (K)
   self%Textgroundfloor = stebbsState%GroundFloorOutdoorSurfaceTemperature + 273.15 ! # Ground floor outdoor surface temperature (K)

   self%Ts = (/building_archtype%HeatingSetpointTemperature + 273.15, &
               building_archtype%CoolingSetpointTemperature + 273.15/) ! # Heating and Cooling setpoint temperatures (K), respectively
   self%initTs = (/building_archtype%HeatingSetpointTemperature + 273.15, &
                   building_archtype%CoolingSetpointTemperature + 273.15/)
   self%HTsAverage = (/18 + 273.15, 18 + 273.15, 18 + 273.15/) ! #
   self%HWTsAverage = (/10 + 273.15, 10 + 273.15, 10 + 273.15/)

   self%Twater_tank = stebbsState%WaterTankTemperature + 273.15 ! # Water temperature (K) in Hot Water Tank
   self%Tintwall_tank = stebbsState%InternalWallWaterTankTemperature + 273.15 ! # Hot water tank internal wall temperature (K)
   self%Textwall_tank = stebbsState%ExternalWallWaterTankTemperature + 273.15 ! # Hot water tank external wall temperature (K)
   self%thickness_tankwall = stebbsPrm%WaterTankWallThickness ! # Hot water tank wall thickness (m)
   self%Tincomingwater_tank = stebbsState%MainsWaterTemperature + 273.15 ! # Water temperature (K) of Water coming into the Water Tank
   self%Vwater_tank = building_archtype%WaterTankWaterVolume ! # Volume of Water in Hot Water Tank (m^3)  h = 1.5, (2/(1.5*3.14))^0.5 = r =
   self%Asurf_tank = stebbsPrm%WaterTankSurfaceArea ! # Surface Area of Hot Water Tank(m^2) - cylinder h= 1.5
   self%Vwall_tank = self%Asurf_tank*self%thickness_tankwall ! # Wall volume of Hot Water Tank(m^2)
   self%setTwater_tank = stebbsPrm%HotWaterHeatingSetpointTemperature + 273.15 ! # Water Tank setpoint temperature (K)
   self%init_wtTs = stebbsPrm%HotWaterHeatingSetpointTemperature + 273.15 ! # Initial Water Tank setpoint temperature (K)
   self%Twater_vessel = stebbsState%DomesticHotWaterTemperatureInUseInBuilding + 273.15 ! # Water temperature (K) of water held in use in Building
   self%Tintwall_vessel = stebbsState%InternalWallDHWVesselTemperature + 273.15 ! # Hot water vessel internal wall temperature (K)
   self%Textwall_vessel = stebbsState%ExternalWallDHWVesselTemperature + 273.15 ! # Hot water vessel external wall temperature (K)
   self%thickness_wall_vessel = stebbsPrm%DHWVesselWallThickness ! # DHW vessels wall thickness (m)

   self%Vwater_vessel = stebbsPrm%DHWWaterVolume ! # Volume of water held in use in building (m^3)
   self%Awater_vessel = stebbsPrm%DHWSurfaceArea ! # Surface Area of Hot Water in Vessels in Building (m^2)
   self%Vwall_vessel = self%Awater_vessel*self%thickness_wall_vessel ! # Wall volume of Hot water Vessels in Building
   self%flowrate_water_supply = stebbsPrm%HotWaterFlowRate ! # Hot Water Flow Rate in m^3 / s
   self%flowrate_water_drain = stebbsPrm%DHWDrainFlowRate ! # Draining of Domestic Hot Water held in building

   self%single_flowrate_water_supply = stebbsPrm%HotWaterFlowRate ! # Hot Water Flow Rate in m^3 s^-1 for a single HW unit
   self%flowrate_water_supply = stebbsPrm%HotWaterFlowRate ! # Hot Water Flow Rate in m^3 / s
   self%flowrate_water_drain = stebbsPrm%DHWDrainFlowRate ! # Draining of Domestic Hot Water held in building

   self%single_flowrate_water_supply = stebbsPrm%HotWaterFlowRate ! # Hot Water Flow Rate in m^3 s^-1 for a single HW unit
   self%single_flowrate_water_drain = stebbsPrm%DHWDrainFlowRate ! # Draining of Domestic Hot Water held in building

   self%cp_water = stebbsPrm%DHWSpecificHeatCapacity ! # Specific Heat Capacity of Domestic Hot Water (J/kg K)
   self%cp_wall_tank = stebbsPrm%HotWaterTankSpecificHeatCapacity ! # Specific Heat Capacity of Hot Water Tank wall
   self%cp_wall_vessel = stebbsPrm%DHWVesselSpecificHeatCapacity ! # Specific Heat Capacity of Vessels containing DHW in use in Building (value here is based on MDPE)

   self%density_water = stebbsPrm%DHWDensity ! # Density of water
   self%density_wall_tank = stebbsPrm%HotWaterTankWallDensity ! # Density of hot water tank wall
   self%density_wall_vessel = stebbsPrm%DHWVesselDensity ! # Density of vessels containing DHW in use in buildings

   self%BVF_tank = stebbsPrm%HotWaterTankBuildingWallViewFactor ! # water tank - building wall view factor
   self%MVF_tank = stebbsPrm%HotWaterTankInternalMassViewFactor ! # water tank - building internal mass view factor

   self%conductivity_wall_tank = stebbsPrm%HotWaterTankWallConductivity ! # Effective Wall conductivity of the Hot Water Tank
   self%conv_coeff_intwall_tank = stebbsPrm%HotWaterTankInternalWallConvectionCoefficient ! # Effective Internal Wall convection coefficient of the Hot Water Tank
   self%conv_coeff_extwall_tank = stebbsPrm%HotWaterTankExternalWallConvectionCoefficient ! # Effective External Wall convection coefficient of the Hot Water Tank

   self%emissivity_extwall_tank = stebbsPrm%HotWaterTankWallEmissivity
   self%conductivity_wall_vessel = stebbsPrm%DHWVesselWallConductivity
   self%conv_coeff_intwall_vessel = stebbsPrm%DHWVesselInternalWallConvectionCoefficient
   self%conv_coeff_extwall_vessel = stebbsPrm%HotWaterTankExternalWallConvectionCoefficient ! # Effective Enternal Wall convection coefficient of the Vessels holding DHW in use in Building
   self%emissivity_extwall_vessel = stebbsPrm%DHWVesselWallConductivity ! # Effective External Wall emissivity of hot water being used within building

   self%maxheatingpower_water = building_archtype%MaximumHotWaterHeatingPower ! # Watts
   self%heating_efficiency_water = stebbsPrm%HotWaterHeatingEfficiency
   self%minVwater_vessel = stebbsPrm%MinimumVolumeOfDHWinUse ! # m3

   self%minHeatingPower_DHW = building_archtype%MaximumHotWaterHeatingPower
   self%HeatingPower_DHW = building_archtype%MaximumHotWaterHeatingPower

   self%HWPowerAverage = (/30000, 30000, 30000/)

END SUBROUTINE gen_building

SUBROUTINE create_building(CASE, self, icase)
   USE modulestebbs, ONLY: LBM
   IMPLICIT NONE
   INTEGER, INTENT(in) :: icase
   CHARACTER(len=256) :: CASE
   TYPE(LBM) :: self
   self%idLBM = icase
   ! self%fnmlLBM = './BuildClasses/'//TRIM(CASE)//'.nml'
   self%fnmlLBM = TRIM(CASE)
   self%CASE = TRIM(CASE)
   self%Qtotal_heating = 0.0 ! # currently only sensible but this needs to be  split into sensible and latent heat components
   self%Qtotal_cooling = 0.0 ! # currently only sensible but this needs to be  split into sensible and latent heat components

   self%Qmetabolic_sensible = 0.0 ! # Sensible heat flux from people in building
   self%Qmetabolic_latent = 0.0 ! # Latent heat flux from people in building

   self%Qtotal_water_tank = 0.0
   self%qhwtDrain = 0.0
   self%EnergyExchanges(:) = 0.0
   self%BuildingType = 'None'
   self%BuildingName = 'Default'
   self%ratio_window_wall = 0.4 ! # window to wall ratio
   self%Afootprint = 225.0
   self%height_building = 15.0
   self%wallExternalArea = 450.0
   self%ratioInternalVolume = 0.1
   self%thickness_wallroof = 0.25 ! # wall and roof thickness as not separate (in metres)
   self%thickness_groundfloor = 0.5 ! # ground floor thickness (in metres)
   self%depth_ground = 2.0 ! # depth of ground considered for calculating QfB to ground (in metres)
   self%thickness_window = 0.05 ! # all window's thickness (in metres)
   self%conv_coeff_intwallroof = 1
   self%conv_coeff_indoormass = 1
   self%conv_coeff_intgroundfloor = 1
   self%conv_coeff_intwindow = 1
   self%conv_coeff_extwallroof = 2 ! # Could be changed to react to outdoor wind speed from SUEWS
   self%conv_coeff_extwindow = 2
   self%conductivity_wallroof = 0.2
   self%conductivity_groundfloor = 0.2
   self%conductivity_window = 0.5
   self%conductivity_ground = 1.0
   self%density_wallroof = 50
   self%weighting_factor_heatcapacity_wallroof = 0.5
   self%density_groundfloor = 1000
   self%density_window = 5
   self%density_indoormass = 250
   self%density_air_ind = 1.225
   self%cp_wallroof = 500
   self%cp_groundfloor = 1500
   self%cp_window = 840
   self%cp_indoormass = 1000
   self%cp_air_ind = 1005
   self%emissivity_extwallroof = 0.85
   self%emissivity_intwallroof = 0.85
   self%emissivity_indoormass = 0.85
   self%emissivity_extwindow = 0.85
   self%emissivity_intwindow = 0.85
   self%windowTransmissivity = 0.9
   self%windowAbsorbtivity = 0.05
   self%windowReflectivity = 0.05
   self%wallTransmisivity = 0.0
   self%wallAbsorbtivity = 0.5
   self%wallReflectivity = 0.5
   self%BVF_extwall = 0.4
   self%GVF_extwall = 0.2
   self%SVF_extwall = 0.4
   self%occupants = 15
   self%metabolic_rate = 250
   self%ratio_metabolic_latent_sensible = 0.8
   self%appliance_power_rating = 100
   self%appliance_totalnumber = 45
   self%appliance_usage_factor = 0.8
   self%maxheatingpower_air = 45000
   self%heating_efficiency_air = 0.9
   self%maxcoolingpower_air = 3000
   self%coeff_performance_cooling = 2.0
   self%Vair_ind = &
      (self%Afootprint*self%height_building)* &
      (1 - self%ratioInternalVolume) ! # Multiplied by factor that accounts for internal mass
   self%ventilation_rate = 0 ! # Fixed at begining to have no natural ventilation. Given in units of volume of air per second
   self%Awallroof = &
      (self%wallExternalArea*(1 - self%ratio_window_wall)) + &
      self%Afootprint ! # last component accounts for the roof as not considered seperately in the model
   self%Vwallroof = self%Awallroof*self%thickness_wallroof
   self%Vgroundfloor = self%Afootprint*self%thickness_groundfloor
   self%Awindow = self%wallExternalArea*self%ratio_window_wall
   self%Vwindow = self%Awindow*self%thickness_window
   self%Vindoormass = self%Vair_ind*self%ratioInternalVolume ! # Multiplied by factor that accounts for internal mass as proportion of total air volume
   self%Aindoormass = 6*(self%Vindoormass**(2./3.)) ! # Assumed internal mass as a cube
   self%h_i = (/self%conv_coeff_intwallroof, self%conv_coeff_indoormass, &
                self%conv_coeff_intgroundfloor, self%conv_coeff_intwindow/)

   self%h_o = (/self%conv_coeff_extwallroof, self%conv_coeff_extwindow/)
   self%k_eff = (/self%conductivity_wallroof, self%conductivity_groundfloor, &
                  self%conductivity_window, self%conductivity_ground/)

   self%rho = (/self%density_wallroof, self%density_groundfloor, &
                self%density_window, self%density_indoormass, &
                self%density_air_ind/)
   self%Cp = (/self%cp_wallroof, self%cp_groundfloor, self%cp_window, &
               self%cp_indoormass, self%cp_air_ind/)
   self%emis = (/self%emissivity_extwallroof, self%emissivity_intwallroof, &
                 self%emissivity_indoormass, self%emissivity_extwindow, &
                 self%emissivity_intwindow/)
   self%wiTAR = (/self%windowTransmissivity, self%windowAbsorbtivity, self%windowReflectivity/)
   self%waTAR = (/self%wallTransmisivity, self%wallAbsorbtivity, self%wallReflectivity/)

   self%viewFactors = (/self%BVF_extwall, self%GVF_extwall, self%SVF_extwall/) !  # Building, ground, and sky view factors
   self%occupantData = (/self%occupants, self%metabolic_rate, &
                         self%ratio_metabolic_latent_sensible/)

   self%Tair_ind = 20 + 273.15 ! # Indoor air temperature (K)
   self%Tindoormass = 20 + 273.15 ! # Indoor mass temperature (K)
   self%Tintwallroof = 20 + 273.15 ! # Wall indoor surface temperature (K)
   self%Textwallroof = 20 + 273.15 ! # Wall outdoor surface temperature (K)
   self%Tintwindow = 20 + 273.15 ! # Window indoor surface temperature (K)
   self%Textwindow = 20 + 273.15 ! # Window outdoor surface temperature (K)
   self%Tintgroundfloor = 20 + 273.15 ! # Ground floor indoor surface temperature (K)
   self%Textgroundfloor = 20 + 273.15 ! # Ground floor outdoor surface temperature (K)
   self%Ts = (/18 + 273.15, 24 + 273.15/) ! # Heating and Cooling setpoint temperatures (K), respectively
   self%initTs = (/18 + 273.15, 24 + 273.15/)
   self%HTsAverage = (/18 + 273.15, 18 + 273.15, 18 + 273.15/) ! #
   self%HWTsAverage = (/10 + 273.15, 10 + 273.15, 10 + 273.15/)
   self%Twater_tank = 70 + 273.15 ! # Water temperature (K) in Hot Water Tank
   self%Tintwall_tank = 70 + 273.15 ! # Hot water tank internal wall temperature (K)
   self%Textwall_tank = 30 + 273.15 ! # Hot water tank external wall temperature (K)
   self%thickness_tankwall = 0.1 ! # Hot water tank wall thickness (m)
   self%Tincomingwater_tank = 10 + 273.15 ! # Water temperature (K) of Water coming into the Water Tank
   self%Vwater_tank = 2 ! # Volume of Water in Hot Water Tank (m^3)  h = 1.5, (2/(1.5*3.14))^0.5 = r =
   self%Asurf_tank = 8.8 ! # Surface Area of Hot Water Tank(m^2) - cylinder h= 1.5
   self%Vwall_tank = self%Asurf_tank*self%thickness_tankwall ! # Wall volume of Hot Water Tank(m^2)
   self%setTwater_tank = 60 + 273.15 ! # Water Tank setpoint temperature (K)
   self%init_wtTs = 60 + 273.15 ! # Initial Water Tank setpoint temperature (K)

   self%Twater_vessel = 35 + 273.15 ! # Water temperature (K) of water held in use in Building
   self%Tintwall_vessel = 35 + 273.15 ! # Hot water vessel internal wall temperature (K)
   self%Textwall_vessel = 25 + 273.15 ! # Hot water vessel external wall temperature (K)
   self%thickness_wall_vessel = 0.005 ! # DHW vessels wall thickness (m)

   self%Vwater_vessel = 0 ! # Volume of water held in use in building (m^3)
   self%Awater_vessel = 10 ! # Surface Area of Hot Water in Vessels in Building (m^2)
   self%Vwall_vessel = self%Awater_vessel*self%thickness_wall_vessel ! # Wall volume of Hot water Vessels in Building
   self%flowrate_water_supply = 0 ! # Hot Water Flow Rate in m^3 / s
   self%flowrate_water_drain = 0 ! # Draining of Domestic Hot Water held in building

   self%single_flowrate_water_supply = 0 ! # Hot Water Flow Rate in m^3 s^-1 for a single HW unit
   self%single_flowrate_water_drain = 0 ! # Draining of Domestic Hot Water held in building

   self%cp_water = 4180.1 ! # Specific Heat Capacity of Domestic Hot Water (J/kg K)
   self%cp_wall_tank = 1000 ! # Specific Heat Capacity of Hot Water Tank wall
   self%cp_wall_vessel = 1900 ! # Specific Heat Capacity of Vessels containing DHW in use in Building (value here is based on MDPE)

   self%density_water = 1000 ! # Density of water
   self%density_wall_tank = 50 ! # Density of hot water tank wall
   self%density_wall_vessel = 930 ! # Density of vessels containing DHW in use in buildings

   self%BVF_tank = 0.2 ! # water tank - building wall view factor
   self%MVF_tank = 0.8 ! # water tank - building internal mass view factor

   self%conductivity_wall_tank = 0.1 ! # Effective Wall conductivity of the Hot Water Tank (based on polyurethan foam given in https://www.lsta.lt/files/events/28_jarfelt.pdf and from https://www.sciencedirect.com/science/article/pii/S0360544214011189?via%3Dihub)
   self%conv_coeff_intwall_tank = 243 ! # Effective Internal Wall convection coefficient of the Hot Water Tank (W/m2 . K) given in http://orbit.dtu.dk/fedora/objects/orbit:77843/datastreams/file_2640258/content

   self%conv_coeff_extwall_vessel = 4 ! # Effective Enternal Wall convection coefficient of the Vessels holding DHW in use in Building
   self%emissivity_extwall_vessel = 0.88 ! # Effective External Wall emissivity of hot water being used within building

   self%maxheatingpower_water = 3000 ! # Watts
   self%heating_efficiency_water = 0.95
   self%minVwater_vessel = 0.1 ! # m3

   self%minHeatingPower_DHW = 3000
   self%HeatingPower_DHW = 3000

   self%HWPowerAverage = (/30000, 30000, 30000/)
   RETURN
END SUBROUTINE create_building
