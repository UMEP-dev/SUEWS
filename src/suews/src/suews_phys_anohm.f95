!========================================================================================
!> AnOHM: Analytical Objective Hysteresis Model
!> @author
!> Ting Sun, ting.sun@reading.ac.uk
!> @brief
!> calculate heat storage.
!> @ref model details refer to
!> https://doi.org/10.5194/gmd-2016-300
! history:
! 20160301: initial version
! 20170109: updated dqndt calculation in accordance with SUEWS_OHM.f95 (HCW)
! 20170810: revamped structure
! 20170825: improved Bowen calculation
!========================================================================================
! Main module following naming standard: matches filename
MODULE module_phys_anohm
   USE module_phys_ohm, ONLY: OHM_dqndt_cal_X, OHM_QS_cal

   IMPLICIT NONE
CONTAINS

   !========================================================================================
   !> High level wrapper for AnOHM calculation
   !! @brief
   !! calculate heat storage based within AnOHM framework.
   !! @returns
   !! -# grid ensemble heat storage:
   !! QS = a1*(Q*)+a2*(dQ*/dt)+a3
   !! -# grid ensemble OHM coefficients: a1, a2 and a3
   SUBROUTINE AnOHM( &
      tstep, dt_since_start, &
      qn1, qn_av_prev, dqndt_prev, qf, &
      MetForcingData_grid, moist_surf, &
      alb, emis, cpAnOHM, kkAnOHM, chAnOHM, & ! input
      sfr_surf, nsurf, EmissionsMethod, id, Gridiv, &
      qn_av_next, dqndt_next, &
      a1, a2, a3, qs, deltaQi) ! output

      IMPLICIT NONE
      INTEGER, INTENT(in) :: tstep ! time step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]

      REAL(KIND(1D0)), INTENT(in), DIMENSION(:, :) :: MetForcingData_grid !< met forcing array of grid

      REAL(KIND(1D0)), INTENT(in) :: qn1 !< net all-wave radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qf !< anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: sfr_surf(nsurf) !< surface fraction (0-1) [-]
      REAL(KIND(1D0)), INTENT(in) :: moist_surf(nsurf) !< non-dimensional surface wetness status (0-1) [-]

      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: alb !< albedo [-]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: emis !< emissivity [-]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: cpAnOHM !< heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: kkAnOHM !< thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: chAnOHM !< bulk transfer coef [J m-3 K-1]

      INTEGER, INTENT(in) :: id !< day of year [-]
      INTEGER, INTENT(in) :: Gridiv !< grid id [-]
      INTEGER, INTENT(in) :: EmissionsMethod !< AnthropHeat option [-]
      INTEGER, INTENT(in) :: nsurf !< number of surfaces [-]
      ! INTEGER,INTENT(in):: nsh               !< number of timesteps in one hour [-]

      REAL(KIND(1D0)), INTENT(in) :: qn_av_prev
      REAL(KIND(1D0)), INTENT(out) :: qn_av_next
      REAL(KIND(1D0)), INTENT(in) :: dqndt_prev !Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(out) :: dqndt_next !Rate of change of net radiation [W m-2 h-1] at t-1

      ! REAL(KIND(1d0)),INTENT(inout)::qn1_store(nsh) !< stored qn1 [W m-2]
      ! REAL(KIND(1d0)),INTENT(inout)::qn1_av_store(2*nsh+1) !< average net radiation over previous hour [W m-2]

      REAL(KIND(1D0)), INTENT(out) :: a1 !< AnOHM coefficients of grid [-]
      REAL(KIND(1D0)), INTENT(out) :: a2 !< AnOHM coefficients of grid [h]
      REAL(KIND(1D0)), INTENT(out) :: a3 !< AnOHM coefficients of grid [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: qs !< storage heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: deltaQi(nsurf) !< storage heat flux of snow surfaces

      INTEGER :: is, xid !< @var qn1 net all-wave radiation
      INTEGER, SAVE :: id_save ! store index of the valid day with enough data ! TODO: Remove SAVE states from the model
      REAL(KIND(1D0)), PARAMETER :: NotUsed = -55.5 !< @var qn1 net all-wave radiation
      INTEGER, PARAMETER :: notUsedI = -55 !< @var qn1 net all-wave radiation
      LOGICAL :: idQ ! whether id contains enough data

      ! REAL(KIND(1d0))                  :: dqndt       !< rate of change of net radiation [W m-2 h-1] at t-2
      ! REAL(KIND(1d0))                  :: surfrac     !< surface fraction accounting for SnowFrac if appropriate
      REAL(KIND(1D0)), DIMENSION(nsurf) :: xa1, xa2, xa3 !< temporary AnOHM coefs.

      ! initialize output variables
      xa1 = 0.1
      xa2 = 0.2
      xa3 = 10
      qs = -999
      deltaQi = 0 ! NB: as snow part is not implemented within AnOHM yet, this is just a placeholder

      ! to test if the current met block contains enough data for AnOHM
      ! TODO: more robust selection should be implemented
      ! daylight hours >= 6
      idQ = COUNT(MetForcingData_grid(:, 2) == id .AND. & ! day of year
                  MetForcingData_grid(:, 4) == 0 .AND. & ! minutes
                  MetForcingData_grid(:, 15) > 0) & ! Sd
            >= 6

      ! PRINT*, idQ
      IF (idQ) THEN
         ! given enough data, calculate coefficients of day `id`
         xid = id
         id_save = id ! store index of the valid day with enough data
      ELSE
         ! otherwise calculate coefficients of yesterday: `id-1`
         xid = id_save
      END IF

      DO is = 1, nsurf
         !   call AnOHM to calculate the coefs.
         CALL AnOHM_coef(is, xid, Gridiv, MetForcingData_grid, moist_surf, EmissionsMethod, qf, & !input
                         alb, emis, cpAnOHM, kkAnOHM, chAnOHM, & ! input
                         xa1(is), xa2(is), xa3(is)) ! output
         ! print*, 'AnOHM_coef are: ',xa1,xa2,xa3
      END DO

      !   calculate the areally-weighted OHM coefficients
      a1 = DOT_PRODUCT(xa1, sfr_surf)
      a2 = DOT_PRODUCT(xa2, sfr_surf)
      a3 = DOT_PRODUCT(xa3, sfr_surf)

      !   Calculate radiation part ------------------------------------------------------------
      qs = -999 !qs  = Net storage heat flux  [W m-2]
      IF (qn1 > -999) THEN !qn1 = Net all-wave radiation [W m-2]

         ! Store instantaneous qn1 values for previous hour (qn1_store) and average (qn1_av)
         ! CALL OHM_dqndt_cal(nsh,qn1,qn1_store,qn1_av_store,dqndt)
         CALL OHM_dqndt_cal_X(tstep, dt_since_start, qn_av_prev, qn1, dqndt_prev, &
                              qn_av_next, dqndt_next)

         ! Calculate net storage heat flux
         CALL OHM_QS_cal(qn1, dqndt_prev, a1, a2, a3, qs)

      ELSE
         CALL ErrorHint(21, 'SUEWS_AnOHM.f95: bad value for qn found during qs calculation.', qn1, NotUsed, notUsedI)
      END IF

   END SUBROUTINE AnOHM
   !========================================================================================

   !========================================================================================
   !> High level wrapper for AnOHM coefficients calculation
   !! @brief
   !! calculate OHM coefficients within AnOHM framework.
   !! @returns
   !! -# OHM coefficients of a given surface type: a1, a2 and a3
   SUBROUTINE AnOHM_coef( &
      sfc_typ, xid, xgrid, & !input
      MetForcingData_grid, moist, EmissionsMethod, qf, & !input
      alb, emis, cpAnOHM, kkAnOHM, chAnOHM, & ! input
      xa1, xa2, xa3) ! output

      IMPLICIT NONE

      ! input
      INTEGER, INTENT(in) :: sfc_typ !< surface type [-]
      INTEGER, INTENT(in) :: xid !< day of year [-]
      INTEGER, INTENT(in) :: xgrid !< grid id [-]
      INTEGER, INTENT(in) :: EmissionsMethod !< AnthropHeat option [-]

      REAL(KIND(1D0)), INTENT(in) :: qf !< anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: alb !< albedo [-]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: emis !< emissivity [-]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: cpAnOHM !< heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: kkAnOHM !< thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: chAnOHM !< bulk transfer coef [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:) :: moist !< surface wetness status [-]
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:, :) :: MetForcingData_grid !< met forcing array of grid

      ! output
      REAL(KIND(1D0)), INTENT(out) :: xa1 !< AnOHM coefficients of grid [-]
      REAL(KIND(1D0)), INTENT(out) :: xa2 !< AnOHM coefficients of grid [h]
      REAL(KIND(1D0)), INTENT(out) :: xa3 !< AnOHM coefficients of grid [W m-2]

      ! surface temperature related scales:
      REAL(KIND(1D0)) :: ATs !< daily amplitude of surface temperature [K]
      REAL(KIND(1D0)) :: mTs !< daily mean of surface temperature [K]
      REAL(KIND(1D0)) :: gamma !< phase difference between Ts and Sd [rad]

      !   forcing scales
      REAL(KIND(1D0)) :: ASd, mSd, tSd !< solar radiation
      REAL(KIND(1D0)) :: ATa, mTa, tTa !< air temperature
      REAL(KIND(1D0)) :: tau !< phase lag between Sd and Ta (Ta-Sd)
      REAL(KIND(1D0)) :: mWS, mWF, mAH !< mean values of WS, WF and AH

      !   forcings:
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Sd ! incoming solar radiation [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Ta ! air temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: RH ! relative humidity [%]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: pres ! air pressure [hPa]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: WS ! wind speed [m s-1]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: WF ! water flux density [m3 s-1 m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: AH ! anthropogenic heat [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tHr ! time [hr]

      !   sfc. properties:
      REAL(KIND(1D0)) :: xalb ! albedo,
      REAL(KIND(1D0)) :: xemis ! emissivity,
      REAL(KIND(1D0)) :: xcp ! heat capacity,
      REAL(KIND(1D0)) :: xk ! thermal conductivity,
      REAL(KIND(1D0)) :: xch ! bulk transfer coef.
      REAL(KIND(1D0)) :: xBo ! Bowen ratio
      REAL(KIND(1D0)) :: xeta ! effective absorption coefficient
      REAL(KIND(1D0)) :: xmu ! effective absorption fraction
      REAL(KIND(1D0)) :: xmoist ! surface wetness

      ! locally saved variables:
      ! if coefficients have been calculated, just reload them
      ! otherwise, do the calculation
      INTEGER, SAVE :: id_save, grid_save
      REAL(KIND(1D0)), SAVE :: coeff_grid_day(7, 3) = -999.

      ! PRINT*, 'xid,id_save',xid,id_save
      ! PRINT*, 'xgrid,grid_save',xgrid,grid_save
      ! PRINT*, 'sfc_typ',sfc_typ
      ! PRINT*, 'coeff_grid_day',coeff_grid_day(sfc_typ,:)
      IF (xid == id_save .AND. xgrid == grid_save) THEN
         ! if coefficients have been calculated, just reload them
         !  print*, 'here no repetition'
         xa1 = coeff_grid_day(sfc_typ, 1)
         xa2 = coeff_grid_day(sfc_typ, 2)
         xa3 = coeff_grid_day(sfc_typ, 3)
      ELSE

         ! load forcing characteristics:
         CALL AnOHM_Fc( &
            xid, MetForcingData_grid, EmissionsMethod, qf, & ! input
            ASd, mSd, tSd, ATa, mTa, tTa, tau, mWS, mWF, mAH) ! output

         ! load forcing variables:
         CALL AnOHM_FcLoad( &
            xid, MetForcingData_grid, EmissionsMethod, qf, & ! input
            Sd, Ta, RH, pres, WS, WF, AH, tHr) ! output

         ! load sfc. properties:
         xalb = alb(sfc_typ)
         xemis = emis(sfc_typ)
         xcp = cpAnOHM(sfc_typ)
         xk = kkAnOHM(sfc_typ)
         xch = chAnOHM(sfc_typ)
         xmoist = moist(sfc_typ)

         !  PRINT*, 'xBo before:',xBo
         ! calculate Bowen ratio:
         CALL AnOHM_Bo_cal( &
            sfc_typ, &
            Sd, Ta, RH, pres, tHr, & ! input: forcing
            ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
            xalb, xemis, xcp, xk, xch, xmoist, & ! input: sfc properties
            tSd, & ! input: peaking time of Sd in hour
            xBo) ! output: Bowen ratio
         !  PRINT*, 'xBo after:',xBo

         !  calculate AnOHM coefficients
         SELECT CASE (sfc_typ)
         CASE (1:6) ! land surfaces
            CALL AnOHM_coef_land_cal( &
               ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
               xalb, xemis, xcp, xk, xch, xBo, & ! input: sfc properties
               xa1, xa2, xa3, ATs, mTs, gamma) ! output: surface temperature related scales by AnOHM

         CASE (7) ! water surface
            ! NB:give fixed values for the moment
            xeta = 0.3
            xmu = 0.2
            CALL AnOHM_coef_water_cal( &
               ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
               xalb, xemis, xcp, xk, xch, xBo, xeta, xmu, & ! input: sfc properties
               xa1, xa2, xa3, ATs, mTs, gamma) ! output

            ! save variables for marking status as done
            id_save = xid
            grid_save = xgrid

         END SELECT
         ! save variables for reusing values of the same day
         coeff_grid_day(sfc_typ, :) = (/xa1, xa2, xa3/)
      END IF

   END SUBROUTINE AnOHM_coef
   !========================================================================================

   !========================================================================================
   !> calculate the surface temperature related parameters (ATs, mTs, gamma)
   !> based on forcings and sfc. conditions
   !> @return
   !> @b xTs surface temperature at local time
   SUBROUTINE AnOHM_xTs( &
      sfc_typ, & !input: surface type
      ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
      xalb, xemis, xcp, xk, xch, xBo, & ! input: sfc properties
      tSd, & !input: peaking time of Sd in hour
      xTHr, & ! input: time in hour
      xTs) ! output: surface temperature

      IMPLICIT NONE
      ! input:
      INTEGER, INTENT(in) :: sfc_typ !< surface type (land: 1-6, water: 7)

      ! input: forcing scales
      REAL(KIND(1D0)), INTENT(in) :: ASd !< daily amplitude of solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: mSd !< daily mean solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: ATa !< daily amplitude of air temperature [K]
      REAL(KIND(1D0)), INTENT(in) :: mTa !< daily mean air temperature [K]
      REAL(KIND(1D0)), INTENT(in) :: tau !< phase lag between Sd and Ta (Ta-Sd) [rad]
      REAL(KIND(1D0)), INTENT(in) :: mWS !< daily mean wind speed [m s-1]
      REAL(KIND(1D0)), INTENT(in) :: mWF !< daily mean underground moisture flux [m3 s-1 m-2]
      REAL(KIND(1D0)), INTENT(in) :: mAH !< daily mean anthropogenic heat flux [W m-2]

      ! input: sfc properties
      REAL(KIND(1D0)), INTENT(in) :: xalb !< albedo [-]
      REAL(KIND(1D0)), INTENT(in) :: xemis !< emissivity [-]
      REAL(KIND(1D0)), INTENT(in) :: xcp !< heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xk !< thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xch !< bulk transfer coef [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xBo !< Bowen ratio [-]
      REAL(KIND(1D0)) :: xeta !< effective absorption fraction [-]
      REAL(KIND(1D0)) :: xmu !< effective absorption coefficient [m-1]

      ! input: temporal-related
      REAL(KIND(1D0)), INTENT(in) :: tSd !< local peaking time of Sd, hour
      REAL(KIND(1D0)), INTENT(in) :: xTHr !< local time to calculate Ts, hour

      ! output:
      REAL(KIND(1D0)), INTENT(out) :: xTs !< surface temperature at xTHr(hr)

      !   local
      REAL(KIND(1D0)) :: &
         xa1, xa2, xa3, & !coefficients
         ATs, mTs, gamma !surface temperature related scales by AnOHM

      ! constant:
      REAL(KIND(1D0)), PARAMETER :: PI = ATAN(1.0)*4 ! Pi
      REAL(KIND(1D0)), PARAMETER :: OMEGA = 2*Pi/(24*60*60) ! augular velocity of Earth

      SELECT CASE (sfc_typ)
      CASE (1:6) ! land surfaces
         CALL AnOHM_coef_land_cal( &
            ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
            xalb, xemis, xcp, xk, xch, xBo, & ! input: sfc properties
            xa1, xa2, xa3, ATs, mTs, gamma) ! output: surface temperature related scales by AnOHM

      CASE (7) ! water surface
         ! !   NB:give fixed values for the moment
         xeta = 0.3
         xmu = 0.2
         CALL AnOHM_coef_water_cal( &
            ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
            xalb, xemis, xcp, xk, xch, xBo, xeta, xmu, & ! input: sfc properties
            xa1, xa2, xa3, ATs, mTs, gamma) ! output

      END SELECT

      ! for a local time xTHr (in hour):
      xTs = ATs*SIN(OMEGA*(xTHr - tSd + 6)*3600 - gamma) + mTs

   END SUBROUTINE AnOHM_xTs
   !========================================================================================

   !========================================================================================
   SUBROUTINE AnOHM_coef_land_cal( &
      ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
      xalb, xemis, xcp, xk, xch, xBo, & ! input: sfc properties
      xa1, xa2, xa3, ATs, mTs, gamma) ! output

      IMPLICIT NONE

      ! input: forcing scales
      REAL(KIND(1D0)), INTENT(in) :: ASd !< daily amplitude of solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: mSd !< daily mean solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: ATa !< daily amplitude of air temperature [K]
      REAL(KIND(1D0)), INTENT(in) :: mTa !< daily mean air temperature [K]
      REAL(KIND(1D0)), INTENT(in) :: tau !< phase lag between Sd and Ta (Ta-Sd) [rad]
      REAL(KIND(1D0)), INTENT(in) :: mWS !< daily mean wind speed [m s-1]
      REAL(KIND(1D0)), INTENT(in) :: mWF !< daily mean underground moisture flux [m3 s-1 m-2]
      REAL(KIND(1D0)), INTENT(in) :: mAH !< daily mean anthropogenic heat flux [W m-2]
      ! input: sfc properties
      REAL(KIND(1D0)), INTENT(in) :: xalb !< albedo [-]
      REAL(KIND(1D0)), INTENT(in) :: xemis !< emissivity [-]
      REAL(KIND(1D0)), INTENT(in) :: xcp !< heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xk !< thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xch !< bulk transfer coef [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xBo !< Bowen ratio [-]

      ! output
      REAL(KIND(1D0)), INTENT(out) :: xa1 !< AnOHM coefficients of grid [-]
      REAL(KIND(1D0)), INTENT(out) :: xa2 !< AnOHM coefficients of grid [h]
      REAL(KIND(1D0)), INTENT(out) :: xa3 !< AnOHM coefficients of grid [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: ATs !< daily amplitude of surface temperature [K]
      REAL(KIND(1D0)), INTENT(out) :: mTs !< daily mean of surface temperature [K]
      REAL(KIND(1D0)), INTENT(out) :: gamma !< phase difference between Ts and Sd [K]

      !   constant
      REAL(KIND(1D0)), PARAMETER :: SIGMA = 5.67E-8 ! Stefan-Boltzman
      REAL(KIND(1D0)), PARAMETER :: PI = ATAN(1.0)*4 ! Pi
      REAL(KIND(1D0)), PARAMETER :: OMEGA = 2*Pi/(24*60*60) ! augular velocity of Earth

      !   local variables:
      REAL(KIND(1D0)) :: beta ! inverse Bowen ratio
      REAL(KIND(1D0)) :: f, fL, fT ! energy redistribution factors
      REAL(KIND(1D0)) :: lambda ! thermal diffusivity
      REAL(KIND(1D0)) :: delta, m, n ! water flux related variables
      REAL(KIND(1D0)) :: ms, ns ! m, n related
      REAL(KIND(1D0)) :: ceta, cphi ! phase related temporary variables
      REAL(KIND(1D0)) :: eta, phi, xlag ! phase related temporary variables
      REAL(KIND(1D0)) :: xx1, xx2, xx3, xchWS ! temporary use
      ! LOGICAL         :: flagGood = .TRUE. ! quality flag, T for good, F for bad

      !   give fixed values for test
      !   properties
      !   xalb  = .2
      !   xemis = .9
      !   xcp   = 1e6
      !   xk    = 1.2
      !   xch   = 4
      !   xBo    = 2
      !   forcings
      !   ASd = 400
      !   mSd = 100
      !   ATa = 23
      !   mTa = 23+C2K
      !   tau = PI/6
      !   WS  = 1
      !   AH  = 0
      !   Wf  = 0

      ! PRINT*, '********sfc_typ: ',sfc_typ,' start********'
      !   initialize flagGood as .TRUE.
      ! flagGood = .TRUE.
      ! PRINT*, flagGood

      !   calculate sfc properties related parameters:
      xchWS = xch*mWS

      !     xch    = CRA/RA !update bulk trsf. coeff. with RA (aerodyn. res.)
      IF (ABS(xBo) < 0.001) THEN
         beta = 1/0.001
      ELSE
         beta = 1/xBo
      END IF

      ! PRINT*, 'beta:', beta
      f = ((1 + beta)*xchWS + 4*SIGMA*xemis*mTa**3)
      !     print*, 'xch',xch,'mTa',mTa,'dLu',4*SIGMA*xemis*mTa**3
      fL = 4*SIGMA*xemis*mTa**3
      !     print*, 'fL',fL
      fT = (1 + beta)*xchWS
      !     print*, 'fT',fT
      lambda = xk/xcp
      !     print*, 'lambda',lambda
      delta = SQRT(.5*(mWF**2 + SQRT(mWF**4 + 16*lambda**2*OMEGA**2)))
      !     print*, 'delta',delta
      m = (2*lambda)/(delta + mWF)
      n = delta/OMEGA
      !     print*, 'm',m,'n',n

      !   calculate surface temperature related parameters:
      mTs = (mSd*(1 - xalb)/f) + mTa
      ms = 1 + xk/(f*m)
      ns = xk/(f*n)
      !     print*, 'ms,ns:', ms,ns
      xx1 = f*SIN(tau)*ATa
      !     print*, 'xx1',xx1
      xx2 = (1 - xalb)*ASd + f*COS(tau)*ATa
      !     print*, 'xx2',xx2
      gamma = ATAN(ns/ms) + ATAN(xx1/xx2)
      !     print*, 'gamma:', gamma
      ATs = -(SIN(tau)*ATa)/(ns*COS(gamma) - ms*SIN(gamma))
      !     print*,  'ATs:', ATs

      !   calculate net radiation related parameters:
      xx1 = (ns*COS(gamma) + SIN(gamma) - ms*SIN(gamma))*SIN(tau)*ATa*fL
      xx2 = (xalb - 1)*(ns*COS(gamma) - ms*SIN(gamma))*ASd
      xx3 = (-ms*COS(tau)*SIN(tau) + COS(gamma)*(ns*COS(tau) + SIN(tau)))*ATa*fL
      xx2 = xx2 - xx3
      phi = ATAN(xx1/xx2)
      xx3 = (ns*COS(gamma) - ms*SIN(gamma))
      xx1 = (1 + SIN(gamma)/xx3)*SIN(tau)*ATa*fL
      xx2 = (xalb - 1)*ASd - (COS(tau) + COS(gamma)*SIN(tau)/xx3)*ATa*fL
      cphi = SQRT(xx1**2 + xx2**2)

      !   calculate heat storage related parameters:
      xx1 = m*COS(gamma) - n*SIN(gamma)
      xx2 = m*SIN(gamma) + n*COS(gamma)
      eta = ATAN(xx1/xx2)
      !     if ( eta<0 ) print*, 'lambda,delta,m,n,gamma:', lambda,delta,m,n,gamma
      xx1 = xk**2*(m**2 + n**2)*ATs**2
      xx2 = m**2*n**2
      ceta = SQRT(xx1/xx2)

      !   key phase lag:
      xlag = eta - phi

      !   calculate the OHM coeffs.:
      !   a1:
      xa1 = (ceta*COS(xlag))/cphi

      !   a2:
      xa2 = (ceta*SIN(xlag))/(OMEGA*cphi)
      xa2 = xa2/3600 ! convert the unit from s-1 to h-1

      !   a3:
      xa3 = -xa1*(fT/f)*(mSd*(1 - xalb)) - mAH

      !     print*, 'ceta,cphi', ceta,cphi
      !     print*, 'tau,eta,phi,xlag in deg:',tau/pi*180,eta/pi*180,phi/pi*180,xlag/pi*180

      ! PRINT*, '********sfc_typ: ',sfc_typ,' end********'

   END SUBROUTINE AnOHM_coef_land_cal
   !========================================================================================

   !========================================================================================
   !> a wrapper for retrieving AnOHM coefficients of water body
   !> @returns
   !! -# OHM coefficients of a given surface type: a1, a2 and a3
   !> @Caution: NB: this SUBROUTINE hasn't been well tested.
   SUBROUTINE AnOHM_coef_water_cal( &
      ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
      xalb, xemis, xcp, xk, xch, xBo, xeta, xmu, & ! input: sfc properties
      xa1, xa2, xa3, ATs, mTs, gamma) ! output

      IMPLICIT NONE

      ! input: forcing scales
      REAL(KIND(1D0)), INTENT(in) :: ASd !< daily amplitude of solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: mSd !< daily mean solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: ATa !< daily amplitude of air temperature [K]
      REAL(KIND(1D0)), INTENT(in) :: mTa !< daily mean air temperature [K]
      REAL(KIND(1D0)), INTENT(in) :: tau !< phase lag between Sd and Ta (Ta-Sd) [rad]
      REAL(KIND(1D0)), INTENT(in) :: mWS !< daily mean wind speed [m s-1]
      REAL(KIND(1D0)), INTENT(in) :: mWF !< daily mean underground moisture flux [m3 s-1 m-2]
      REAL(KIND(1D0)), INTENT(in) :: mAH !< daily mean anthropogenic heat flux [W m-2]

      ! input: sfc properties
      REAL(KIND(1D0)), INTENT(in) :: xalb !< albedo [-]
      REAL(KIND(1D0)), INTENT(in) :: xemis !< emissivity [-]
      REAL(KIND(1D0)), INTENT(in) :: xcp !< heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xk !< thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xch !< bulk transfer coef [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xBo !< Bowen ratio [-]
      REAL(KIND(1D0)), INTENT(in) :: xeta !< effective absorption fraction [-]
      REAL(KIND(1D0)), INTENT(in) :: xmu !< effective absorption coefficient [m-1]

      ! output
      REAL(KIND(1D0)), INTENT(out) :: xa1 !< AnOHM coefficients of grid [-]
      REAL(KIND(1D0)), INTENT(out) :: xa2 !< AnOHM coefficients of grid [h]
      REAL(KIND(1D0)), INTENT(out) :: xa3 !< AnOHM coefficients of grid [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: ATs !< daily amplitude of surface temperature [K]
      REAL(KIND(1D0)), INTENT(out) :: mTs !< daily mean of surface temperature [K]
      REAL(KIND(1D0)), INTENT(out) :: gamma !< phase difference between Ts and Sd [K]

      !   constant
      REAL(KIND(1D0)), PARAMETER :: SIGMA = 5.67E-8 ! Stefan-Boltzman
      REAL(KIND(1D0)), PARAMETER :: PI = ATAN(1.0)*4 ! Pi
      REAL(KIND(1D0)), PARAMETER :: OMEGA = 2*Pi/(24*60*60) ! augular velocity of Earth

      !   local variables:
      REAL(KIND(1D0)) :: beta ! inverse Bowen ratio
      REAL(KIND(1D0)) :: f, fL, fT ! energy redistribution factors
      REAL(KIND(1D0)) :: lambda, calb ! temporary use
      REAL(KIND(1D0)) :: delta ! sfc. temperature related variables
      ! REAL(KIND(1d0)) :: delta,m,n              ! sfc. temperature related variables
      REAL(KIND(1D0)) :: xm, xn ! m, n related
      ! REAL(KIND(1d0)) :: gamma              ! phase lag scale
      REAL(KIND(1D0)) :: phi ! phase lag scale
      ! REAL(KIND(1d0)) :: ATs,mTs                ! surface temperature amplitude
      REAL(KIND(1D0)) :: czeta, ctheta ! phase related temporary variables
      REAL(KIND(1D0)) :: zeta, theta, xlag ! phase related temporary variables
      REAL(KIND(1D0)) :: xx1, xx2, xx3 ! temporary use
      REAL(KIND(1D0)) :: kappa ! temporary use
      REAL(KIND(1D0)) :: dtau, dpsi, dphi ! temporary use
      REAL(KIND(1D0)) :: cdtau, cdpsi, cdphi ! temporary use
      REAL(KIND(1D0)) :: xxT, xxkappa, xxdltphi, xchWS ! temporary use
      ! LOGICAL :: flagGood = .TRUE.  ! quality flag, T for good, F for bad

      ! ====not used====
      REAL(KIND(1D0)) :: dummy
      dummy = mah + mwf
      ! ====not used====

      !   calculate sfc properties related parameters:
      xm = xk*xmu**2
      xn = xcp*OMEGA
      phi = ATAN(xn/xm)
      kappa = SQRT(xcp*OMEGA/(2*xk))

      ! mWS=SUM(WS, dim=1, mask=(WS>0))/SIZE(WS, dim=1)
      xchWS = xch*mWS
      beta = 1/xBo
      f = ((1 + beta)*xchWS + 4*SIGMA*xemis*mTa**3)
      fL = 4*SIGMA*xemis*mTa**3
      fT = (1 + beta)*xchWS

      calb = 1 - xalb

      lambda = SQRT(xm**2 + xn**2)

      dtau = ATAN(xk*kappa/(f + xk*kappa))
      dpsi = ATAN((xk - xmu)/kappa)
      dphi = ATAN(kappa*(f + xk*xmu)/(f*(kappa - xmu) + xk*kappa*(2*kappa - xmu)))
      cdtau = SQRT((xk*kappa)**2 + (f + xk*kappa)**2)
      cdpsi = SQRT((xk - xmu)**2 + kappa**2)
      cdphi = cdtau*cdpsi

      !   calculate surface temperature related parameters:
      ! daily mean:
      mTs = (mSd*(1 - xalb + xeta)/f) + mTa
      ! amplitude:
      xx1 = (xk*xeta*xmu*calb*ASd*cdpsi)**2
      xx2 = 2*lambda*SQRT(xx1)*(calb*ASd*SIN(phi - dpsi) + f*ATa*SIN(tau + phi - dpsi))
      xx3 = lambda**2*((calb*ASd + COS(tau)*f*ATa)**2 + (SIN(tau)*f*ATa)**2)
      ATs = 1/(cdtau*lambda)*SQRT(xx1 + xx2 + xx3)
      ! phase lag:
      xx1 = (xk*kappa*calb*ASd + cdtau*f*ATa*SIN(tau + dtau))*lambda &
            + (xk*xeta*xmu*calb*ASd*cdphi*SIN(phi + dphi))
      xx2 = ((f + xk*kappa)*calb*ASd - cdtau*f*ATa*COS(tau + dtau))*lambda &
            - xk*xeta*xmu*calb*ASd*cdphi*COS(phi + dphi)
      delta = ATAN(xx1/xx2)
      gamma = delta

      ! calculate net radiation related parameters:
      ! phase lag:
      xx1 = fL*(ATs*SIN(delta) - ATa*SIN(tau))
      xx2 = calb*ASd - fL*(ATs*COS(delta) - ATa*COS(tau))
      theta = ATAN(xx1/xx2)
      ! amplitude:
      ctheta = SQRT(xx1**2 + xx2**2)

      !   calculate heat storage related parameters:
      ! scales:
      xxT = SQRT(2.)*kappa*lambda*ATs
      xxkappa = cdpsi*xeta*xmu*ASd
      xxdltphi = COS(delta)*SIN(dpsi)*COS(phi) - SIN(delta)*COS(dpsi)*SIN(phi)
      ! phase lag:
      xx1 = xxT*SIN(PI/4 - delta) + xxkappa*SIN(phi + dpsi)
      xx2 = xxT*SIN(PI/4 + delta) - xxkappa*SIN(PHI - dpsi)
      zeta = ATAN(xx1/xx2)
      ! amplitude:
      xx1 = 2*SQRT(2.)*xxkappa*xxT*xxdltphi
      xx2 = (1 - COS(2*dpsi)*COS(2*phi))*xxkappa**2
      xx3 = xxT**2
      czeta = xk/lambda*SQRT(xx1 + xx2 + xx3)

      !   calculate the OHM coeffs.:
      xlag = zeta - theta
      ! a1:
      xa1 = (czeta*COS(xlag))/ctheta

      !   write(*,*) 'ceta,xlag,cphi:', ceta,xlag,cphi
      !   a2:
      xa2 = (czeta*SIN(xlag))/(OMEGA*ctheta)
      xa2 = xa2/3600 ! convert the unit from s-1 to h-1

      !   a3:
      xa3 = mSd*(xalb - 1)*(xeta + (fT - fL*xeta)/f*xa1)

      ! print*,  'sfc_typ_water:', sfc_typ
      ! print*, 'a1,a2,a3:', xa1,xa2,xa3

   END SUBROUTINE AnOHM_coef_water_cal
   !========================================================================================

   !========================================================================================
   SUBROUTINE AnOHM_Fc( &
      xid, MetForcingData_grid, EmissionsMethod, qf, & ! input
      ASd, mSd, tSd, ATa, mTa, tTa, tau, mWS, mWF, mAH) ! output

      IMPLICIT NONE

      !   input
      INTEGER, INTENT(in) :: xid
      INTEGER, INTENT(in) :: EmissionsMethod
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:, :) :: MetForcingData_grid
      REAL(KIND(1D0)), INTENT(in) :: qf !< anthropogenic heat flux [W m-2]

      !   output
      REAL(KIND(1D0)), INTENT(out) :: ASd !< daily amplitude of solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: mSd !< daily mean solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: tSd !< local peaking time of solar radiation [hr]
      REAL(KIND(1D0)), INTENT(out) :: ATa !< daily amplitude of air temperature [degC]
      REAL(KIND(1D0)), INTENT(out) :: mTa !< daily mean air temperature [degC]
      REAL(KIND(1D0)), INTENT(out) :: tTa !< local peaking time of air temperature [hour]
      REAL(KIND(1D0)), INTENT(out) :: tau !< phase lag between Sd and Ta (Ta-Sd) [rad]
      REAL(KIND(1D0)), INTENT(out) :: mWS !< daily mean wind speed [m s-1]
      REAL(KIND(1D0)), INTENT(out) :: mWF !< daily mean underground moisture flux [m3 s-1 m-2]
      REAL(KIND(1D0)), INTENT(out) :: mAH !< daily mean anthropogenic heat flux [W m-2]

      !   forcings:
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Sd !< incoming solar radiation [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: Ta !< air temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: RH !< relative humidity [%]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: pres !< Atmospheric pressure [hPa]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: WS ! wind speed [m s-1]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: WF ! water flux density [m3 s-1 m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: AH ! anthropogenic heat [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tHr ! local time [hr]

      ! load forcing variables:
      CALL AnOHM_FcLoad(xid, MetForcingData_grid, EmissionsMethod, qf, Sd, Ta, RH, pres, WS, WF, AH, tHr)
      ! calculate forcing scales for AnOHM:
      CALL AnOHM_FcCal(Sd, Ta, WS, WF, AH, tHr, ASd, mSd, tSd, ATa, mTa, tTa, tau, mWS, mWF, mAH)

      ! CALL r8vec_print(SIZE(sd, dim=1),sd,'Sd')
      ! PRINT*, ASd,mSd,tSd

      ! CALL r8vec_print(SIZE(ta, dim=1),ta,'Ta')
      ! PRINT*, ATa,mTa,tTa

   END SUBROUTINE AnOHM_Fc
   !========================================================================================

   !========================================================================================
   !> load forcing series for AnOHM_FcCal
   SUBROUTINE AnOHM_FcLoad( &
      xid, MetForcingData_grid, EmissionsMethod, qf, & ! input
      Sd, Ta, RH, pres, WS, WF, AH, tHr) ! output

      IMPLICIT NONE

      !   input
      INTEGER, INTENT(in) :: xid !< day of year
      INTEGER, INTENT(in) :: EmissionsMethod !< AnthropHeat option
      REAL(KIND(1D0)), INTENT(in), DIMENSION(:, :) :: MetForcingData_grid !< met forcing array of grid
      REAL(KIND(1D0)), INTENT(in) :: qf !< anthropogenic heat flux [W m-2]

      !   output
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out), ALLOCATABLE :: Sd !< incoming solar radiation [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out), ALLOCATABLE :: Ta !< air temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out), ALLOCATABLE :: RH !< relative humidity [%]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out), ALLOCATABLE :: pres !< atmospheric pressure [mbar]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out), ALLOCATABLE :: WS !< wind speed [m s-1]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out), ALLOCATABLE :: WF !< water flux density [m3 s-1 m-2]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out), ALLOCATABLE :: AH !< anthropogenic heat [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out), ALLOCATABLE :: tHr !< local time  [hr]

      !   local variables:
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: subMet ! subset array of daytime series

      INTEGER :: err
      INTEGER :: lenMetData, nVar

      LOGICAL, ALLOCATABLE :: metMask(:)

      ! construct mask
      IF (ALLOCATED(metMask)) DEALLOCATE (metMask, stat=err)
      ALLOCATE (metMask(SIZE(MetForcingData_grid, dim=1)))
      metMask = (MetForcingData_grid(:, 2) == xid & ! day=xid
                 .AND. MetForcingData_grid(:, 4) == 0) ! tmin=0

      ! determine the length of subset
      lenMetData = COUNT(metMask)

      ! construct array for time and met variables
      nVar = 8 ! number of variables to retrieve
      ! print*, 'good 1'
      ! allocate subMet:
      IF (ALLOCATED(subMet)) DEALLOCATE (subMet, stat=err)
      ALLOCATE (subMet(lenMetData, nVar))
      subMet = RESHAPE(PACK(MetForcingData_grid(:, (/3, & !time: hour
                                                     15, 12, 11, 13, 10, 12, 9/)), & ! met: Sd, Ta, RH, pres, WS, WF, AH
                            ! NB: WF not used: a placeholder
                            SPREAD(metMask, dim=2, ncopies=nVar)), & ! replicate mask vector to 2D array
                       (/lenMetData, nVar/)) ! convert to target shape

      ! re-allocate arrays as their sizes may change during passing
      IF (ALLOCATED(tHr)) DEALLOCATE (tHr, stat=err)
      ALLOCATE (tHr(lenMetData))
      IF (ALLOCATED(Sd)) DEALLOCATE (Sd, stat=err)
      ALLOCATE (Sd(lenMetData))
      IF (ALLOCATED(Ta)) DEALLOCATE (Ta, stat=err)
      ALLOCATE (Ta(lenMetData))
      IF (ALLOCATED(RH)) DEALLOCATE (RH, stat=err)
      ALLOCATE (RH(lenMetData))
      IF (ALLOCATED(pres)) DEALLOCATE (pres, stat=err)
      ALLOCATE (pres(lenMetData))
      IF (ALLOCATED(WS)) DEALLOCATE (WS, stat=err)
      ALLOCATE (WS(lenMetData))
      IF (ALLOCATED(WF)) DEALLOCATE (WF, stat=err)
      ALLOCATE (WF(lenMetData))
      IF (ALLOCATED(AH)) DEALLOCATE (AH, stat=err)
      ALLOCATE (AH(lenMetData))

      ! load the sublist into forcing variables
      tHr = subMet(:, 1) ! time in hour
      Sd = subMet(:, 2)
      Ta = subMet(:, 3)
      RH = subMet(:, 4)
      pres = subMet(:, 5)
      WS = subMet(:, 6)
      WF = 0 ! set as 0 for the moment
      IF (EmissionsMethod == 0) THEN
         AH = subMet(:, 8) ! read in from MetForcingData_grid,
      ELSE
         ! AH = 0 ! temporarily change to zero;
         AH = qf ! use modelled value
         !  AH = mAH_grids(xid-1,xgrid)
      END IF

   END SUBROUTINE AnOHM_FcLoad
   !========================================================================================

   !========================================================================================
   !> calculate the key parameters of a sinusoidal curve for AnOHM forcings
   !> i.e., a, b, c in a*Sin(Pi/12*t+b)+c
   SUBROUTINE AnOHM_FcCal( &
      Sd, Ta, WS, WF, AH, tHr, & ! input
      ASd, mSd, tSd, ATa, mTa, tTa, tau, mWS, mWF, mAH) ! output
      IMPLICIT NONE

      ! input
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: Sd !< incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: Ta !< air temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: WS !< wind speed [m s-1]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: WF !< water flux density [m3 s-1 m-2]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: AH !< anthropogenic heat [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: tHr !< time [hr]

      ! output
      REAL(KIND(1D0)), INTENT(out) :: ASd !< daily amplitude of solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: mSd !< daily mean solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: tSd !< local peaking time of solar radiation [hr]
      REAL(KIND(1D0)), INTENT(out) :: ATa !< daily amplitude of air temperature [degC]
      REAL(KIND(1D0)), INTENT(out) :: mTa !< daily mean air temperature [degC]
      REAL(KIND(1D0)), INTENT(out) :: tTa !< local peaking time of air temperature [hr]
      REAL(KIND(1D0)), INTENT(out) :: tau !< phase lag between Sd and Ta (Ta-Sd) [rad]
      REAL(KIND(1D0)), INTENT(out) :: mWS !< daily mean wind speed [m s-1]
      REAL(KIND(1D0)), INTENT(out) :: mWF !< daily mean underground moisture flux [m3 s-1 m-2]
      REAL(KIND(1D0)), INTENT(out) :: mAH !< daily mean anthropogenic heat flux [W m-2]

      !   constant
      REAL(KIND(1D0)), PARAMETER :: PI = ATAN(1.0)*4 ! Pi
      REAL(KIND(1D0)), PARAMETER :: C2K = 273.15 ! degC to K

      !   local variables:
      REAL(KIND(1D0)), ALLOCATABLE :: tHrDay(:) ! daytime tHr when Sd>0
      REAL(KIND(1D0)), ALLOCATABLE :: selX(:) ! daytime sublist of met variable when Sd>0

      ! REAL(KIND(1d0)) :: xx              ! temporary use
      INTEGER :: err, lenDay
      LOGICAL, DIMENSION(:), ALLOCATABLE :: SdMask

      ALLOCATE (SdMask(SIZE(Sd, dim=1)), stat=err)
      IF (err /= 0) PRINT *, "SdMask: Allocation request denied"
      SdMask = Sd > 5
      lenDay = COUNT(SdMask)

      ! CALL r8vec_print(24,Sd,'Sd')
      ! CALL r8vec_print(24,tHr,'tHr')
      ALLOCATE (tHrDay(lenDay), stat=err)
      IF (err /= 0) PRINT *, "tHrDay: Allocation request denied"
      tHrDay = PACK(tHr, mask=SdMask)

      ALLOCATE (selX(lenDay), stat=err)
      IF (err /= 0) PRINT *, "selX: Allocation request denied"

      !   calculate sinusoidal scales of Sd:
      ! PRINT*, 'Calc. Sd...'
      selX = PACK(Sd, mask=SdMask)
      ASd = (MAXVAL(selX) - MINVAL(selX))/2
      mSd = SUM(selX)/lenDay
      tSd = 12
      CALL AnOHM_ShapeFit(tHrDay, selX, ASd, mSd, tSd)
      ! CALL r8vec_print(lenDay,selX,'Sd Day:')
      !   modify ill-shaped days to go through
      ! IF ( ASd < 0 .OR. tSd > 15) THEN
      !    !         ASd = abs(ASd)
      !    !         tSd = 12 ! assume Sd peaks at 12:00LST
      !    CALL r8vec_print(lenDay,tHrDay,'tHrDay:')
      !    CALL r8vec_print(lenDay,selX,'Sd Day:')
      !    PRINT*, 'ASd:', ASd
      !    PRINT*, 'mSd:', mSd
      !    PRINT*, 'tSd:', tSd
      ! END IF
      ! PRINT*, 'Sd Day:', selX

      ! add corrections on mSd to fix the overestimated a3 during cold periods
      IF (mSd + ASd < MAXVAL(selX)) THEN
         ! PRINT*, 'Sd max:', MAXVAL(selX)
         ! PRINT*, 'ASd:', ASd
         ! PRINT*, 'mSd before:', mSd
         mSd = MAXVAL(selX) - ASd
         ! PRINT*, 'mSd after:', mSd
         ! PRINT*,''
      END IF

      !   calculate sinusoidal scales of Ta:
      ! PRINT*, 'Calc. Ta...'
      selX = PACK(Ta, mask=SdMask)
      ATa = (MAXVAL(selX) - MINVAL(selX))/2
      mTa = SUM(selX)/lenDay
      tTa = 12
      CALL AnOHM_ShapeFit(tHrDay, selX, ATa, mTa, tTa)
      ! CALL r8vec_print(lenDay,selX,'Ta Day:')
      IF (mTa < 60) mTa = mTa + C2K ! correct the Celsius to Kelvin
      !   modify ill-shaped days to go through
      IF (ATa < 0) THEN
         !         ATa = abs(ATa)
         !         tTa = 14 ! assume Ta peaks at 14:00LST
         CALL r8vec_print(lenDay, selX, 'Ta Day:')
         PRINT *, 'ATa:', ATa
         PRINT *, 'mTa:', mTa
         PRINT *, 'tTa:', tTa
      END IF
      ! PRINT*, 'Ta:', Ta(10:16)

      !   calculate the phase lag between Sd and Ta:
      tau = (tTa - tSd)/24*2*PI
      ! PRINT*, 'tau:', tau

      !   calculate the mean values:
      selX = PACK(WS, mask=SdMask)
      mWS = SUM(selX)/lenDay ! mean value of WS

      selX = PACK(WF, mask=SdMask)
      mWF = SUM(selX)/lenDay ! mean value of WF

      selX = PACK(AH, mask=SdMask)
      mAH = SUM(selX)/lenDay ! mean value of AH
      ! PRINT*, 'mWS:', mWS
      !     print*, 'mWF:', mWF
      !     print*, 'mAH:', mAH
      IF (ALLOCATED(SdMask)) DEALLOCATE (SdMask, stat=err)
      IF (err /= 0) PRINT *, "SdMask: Deallocation request denied"

      IF (ALLOCATED(tHrDay)) DEALLOCATE (tHrDay, stat=err)
      IF (err /= 0) PRINT *, "tHrDay: Deallocation request denied"

      IF (ALLOCATED(selX)) DEALLOCATE (selX, stat=err)
      IF (err /= 0) PRINT *, "selX: Deallocation request denied"

   END SUBROUTINE AnOHM_FcCal
   !========================================================================================

   !========================================================================================
   !> calculate the key parameters of a sinusoidal curve for AnOHM forcings
   !> i.e., a, b, c in a*Sin(Pi/12*t+b)+c, where t is in hour
   SUBROUTINE AnOHM_ShapeFit( &
      tHr, obs, & !input
      amp, mean, tpeak) !output
      IMPLICIT NONE

      !   input
      REAL(KIND(1D0)), INTENT(in) :: tHr(:) !< time in hour
      REAL(KIND(1D0)), INTENT(in) :: obs(:) !< observation

      !   output
      REAL(KIND(1D0)), INTENT(out) :: amp !< amplitude
      REAL(KIND(1D0)), INTENT(out) :: mean !< average
      REAL(KIND(1D0)), INTENT(out) :: tpeak !< peaking time (h)

      INTEGER :: m, n, info, err ! temporary use

      ! EXTERNAL fSin
      REAL(KIND(1D0)), ALLOCATABLE :: fvec(:), x(:)

      REAL(KIND(1D0)) :: tol = 0.00001D+00 ! tolerance

      n = 3 ! number of parameters to determine
      m = SIZE(tHr, dim=1) ! number of observation pairs
      ! PRINT*, 'm',m

      ALLOCATE (fvec(m), stat=err)
      IF (err /= 0) PRINT *, "fvec: Allocation request denied"

      ALLOCATE (x(n), stat=err)
      IF (err /= 0) PRINT *, "x: Allocation request denied"

      ! initial guess for fitting
      x = (/mean, amp, tpeak/)

      ! use minpack subroutine 'lmstr1' to fit the sinusoidal form
      CALL lmdif1(fSin, m, n, x, tHr, obs, fvec, tol, info)

      ! x   = (/mean,amp,delta/), see subroutine 'fSin' for the meaning of this vector
      mean = x(1)
      amp = x(2)
      tpeak = x(3) + 6 ! when t = delta + 6 (hr of LST), t is tpeak.

      ! adjust fitted parameters to physical range:
      ! amp>0
      IF (amp < 0) THEN
         !  PRINT*, ''
         !  PRINT*, 'before:'
         !  PRINT*, amp,tpeak,mean
         !  CALL r8vec_print(SIZE(tHR, dim=1),tHR,'tHR')
         !  CALL r8vec_print(SIZE(obs, dim=1),obs,'obs')
         amp = ABS(amp)
         tpeak = x(3) - 12 + 6 + 24
         tpeak = MOD(tpeak, 24.)
         !  PRINT*, 'after:'
         !  PRINT*, amp,tpeak,mean
      END IF
      tpeak = MOD(tpeak, 24.)

   END SUBROUTINE AnOHM_ShapeFit
   !========================================================================================

   !========================================================================================
   !> sinusoidal function f(t) for fitting:
   !> f(t) = mean+amp*Sin(Pi/12(t-delta))
   !> x    = (/mean,amp,delta/) contains the fitting parameters
   SUBROUTINE fSin(m, n, x, xdat, ydat, fvec, iflag)

      IMPLICIT NONE
      INTEGER(kind=4) m
      INTEGER(kind=4) n

      ! REAL ( kind = 8 ) fjrow(n)
      REAL(kind=8) fvec(m), & ! residual vector
         xdat(m), ydat(m) ! (x,y) observations for fitting

      INTEGER(kind=4) iflag, i
      REAL(kind=8) x(n)
      REAL(KIND(1D0)), PARAMETER :: PI = ATAN(1.0)*4 ! Pi

      IF (iflag == 0) THEN

         WRITE (*, '(a)') ''
         DO i = 1, n
            WRITE (*, '(g14.6)') x(i)
         END DO

      ELSE IF (iflag == 1) THEN
         fvec(1:m) = x(1) + x(2)*SIN(2*PI/24*(xdat(1:m) - x(3))) - ydat(1:m)

      END IF
      RETURN

   END SUBROUTINE fSin
   !========================================================================================

   !========================================================================================
   !> estimate daytime Bowen ratio for calculation of AnOHM coefficients
   SUBROUTINE AnOHM_Bo_cal( &
      sfc_typ, & ! surface type
      Sd, Ta, RH, pres, tHr, & ! input: forcing
      ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
      xalb, xemis, xcp, xk, xch, xSM, & ! input: sfc properties
      tSd, & ! input: peaking time of Sd in hour
      xBo) ! output: Bowen ratio

      IMPLICIT NONE

      ! input:
      INTEGER, INTENT(in) :: sfc_typ ! unknown Bowen ratio

      ! input: daytime series
      REAL(kind=8), INTENT(in), DIMENSION(:) :: Sd !< incoming solar radiation [W m-2]
      REAL(kind=8), INTENT(in), DIMENSION(:) :: Ta !< air temperature [degC]
      REAL(kind=8), INTENT(in), DIMENSION(:) :: RH !< relative humidity [%]
      REAL(kind=8), INTENT(in), DIMENSION(:) :: pres !< Atmospheric pressure [mbar]
      REAL(kind=8), INTENT(in), DIMENSION(:) :: tHr !< local time  [hr]

      ! input: forcing scales
      REAL(KIND(1D0)), INTENT(in) :: ASd !< daily amplitude of solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: mSd !< daily mean solar radiation [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: tSd !< local peaking time of solar radiation [hr]
      REAL(KIND(1D0)), INTENT(in) :: ATa !< daily amplitude of air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: mTa !< daily mean air temperature [degC]
      REAL(KIND(1D0)), INTENT(in) :: tau !< phase lag between Sd and Ta (Ta-Sd) [rad]
      REAL(KIND(1D0)), INTENT(in) :: mWS !< daily mean wind speed [m s-1]
      REAL(KIND(1D0)), INTENT(in) :: mWF !< daily mean underground moisture flux [m3 s-1 m-2]
      REAL(KIND(1D0)), INTENT(in) :: mAH !< daily mean anthropogenic heat flux [W m-2]

      ! input: surface properties
      REAL(KIND(1D0)), INTENT(in) :: xalb !< albedo [-]
      REAL(KIND(1D0)), INTENT(in) :: xemis !< emissivity [-]
      REAL(KIND(1D0)), INTENT(in) :: xcp !< heat capacity [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xk !< thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xch !< bulk transfer coef [J m-3 K-1]
      REAL(KIND(1D0)), INTENT(in) :: xSM !< surface moisture status [-]

      ! output:
      REAL(kind=8), INTENT(out) :: xBo ! unknown Bowen ratio [-]

      ! EXTERNAL fcnBo

      REAL(kind=8), ALLOCATABLE :: x(:), fvec(:), prms(:)

      INTEGER :: lenDay, n, m, info, err, nVar, nPrm
      LOGICAL, DIMENSION(:), ALLOCATABLE :: maskDay
      REAL(kind=8) :: tol = 1E-20

      ! LOGICAL, ALLOCATABLE,DIMENSION(:) :: metMask

      ! daytime mask
      ALLOCATE (maskDay(SIZE(sd)), stat=err)
      maskDay = sd > 5
      ! length of daytime series
      lenDay = SIZE(PACK(sd, mask=maskDay), dim=1)
      ! ! daytime mask
      ! IF (ALLOCATED(metMask)) DEALLOCATE(metMask, stat=err)
      ! ALLOCATE(metMask(lenDay))
      ! metMask=(Sd>0)

      ! assign x vector:
      n = 1
      ALLOCATE (x(n), stat=err)
      IF (err /= 0) PRINT *, "x: Allocation request denied"
      ALLOCATE (fvec(n), stat=err)
      IF (err /= 0) PRINT *, "fvec: Allocation request denied"

      ! pass initial Bowen ratio:
      xBo = 10
      x(1) = xBo

      ! NB: set these numbers properly if any changes made to this subroutine:
      ! number of parameters to pass:
      nPrm = 16
      ! number of met variables to pass:
      nVar = 5
      ! length of x vector that holds unknown and parameters
      m = nPrm + nVar*lenDay

      ALLOCATE (prms(m), stat=err)
      IF (err /= 0) PRINT *, "prms: Allocation request denied"

      ! pass forcing scales:
      prms(1) = ASd
      prms(2) = mSd
      prms(3) = ATa
      prms(4) = mTa
      prms(5) = tau
      prms(6) = mWS
      prms(7) = mWF
      prms(8) = mAH

      ! pass sfc. property scales:
      prms(9) = xalb
      prms(10) = xemis
      prms(11) = xcp
      prms(12) = xk
      prms(13) = xch
      prms(14) = xSM

      ! pass tSd:
      prms(15) = tSd

      ! pass tSd:
      prms(16) = sfc_typ*1.0

      ! extract daytime series
      prms(nPrm + 1:m) = PACK((/Sd, Ta, RH, pres, tHr/), &
                              mask=PACK(SPREAD(maskDay, dim=2, ncopies=nVar), .TRUE.))

      ! PRINT*, 'xBo before solve:',x(1)
      ! PRINT*, 'fvec before solve:',fvec(1)
      ! PRINT*, 'xSM:',xSM
      ! solve nonlinear equation fcnBo(x)=0
      CALL hybrd1(fcnBo, n, x, fvec, tol, info, m, prms)
      xBo = x(1)
      ! PRINT*, 'xBo after solve: ',x(1)
      ! PRINT*, 'fvec after solve:',fvec(1)

      IF (ALLOCATED(x)) DEALLOCATE (x, stat=err)
      IF (err /= 0) PRINT *, "x: Deallocation request denied"
      IF (ALLOCATED(fvec)) DEALLOCATE (fvec, stat=err)
      IF (err /= 0) PRINT *, "fvec: Deallocation request denied"
      IF (ALLOCATED(prms)) DEALLOCATE (prms, stat=err)
      IF (err /= 0) PRINT *, "prms: Deallocation request denied"

   END SUBROUTINE AnOHM_Bo_cal
   !========================================================================================

   !========================================================================================
   !> this fucntion will construct an equaiton for Bo calculation
   SUBROUTINE fcnBo(n, x, fvec, iflag, m, prms)

      IMPLICIT NONE
      !    Input, external FCN, the name of the user-supplied subroutine which
      !    calculates the functions.  The routine should have the form:
      !      subroutine fcn ( n, x, fvec, iflag )
      INTEGER(kind=4) :: n
      INTEGER(kind=4) :: m
      INTEGER(kind=4) :: iflag
      REAL(kind=8) :: fvec(n)
      REAL(kind=8) :: x(n) ! x(1) as unknown Bo
      REAL(kind=8) :: prms(m) ! prms(i) used for passing parameters

      ! the unknow: Bowen ratio
      REAL(kind=8) :: xBo

      ! forcing scales
      REAL(kind=8) :: ASd
      REAL(kind=8) :: mSd
      REAL(kind=8) :: ATa
      REAL(kind=8) :: mTa
      REAL(kind=8) :: tau
      REAL(kind=8) :: mWS
      REAL(kind=8) :: mWF
      REAL(kind=8) :: mAH

      ! sfc. property scales
      REAL(kind=8) :: xalb
      REAL(kind=8) :: xemis
      REAL(kind=8) :: xcp
      REAL(kind=8) :: xk
      REAL(kind=8) :: xch

      ! peaking time of Sd in hour
      REAL(kind=8) :: tSd

      ! surface moisture status [-]
      REAL(kind=8) :: xSM

      ! surface type
      INTEGER :: sfc_typ

      ! array of daytime series
      REAL(kind=8), DIMENSION(:, :), ALLOCATABLE :: dayArray

      ! daytime series
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: Sd !< incoming solar radiation, W m-2
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: Ta !< air temperature, degC
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: RH !< relative humidity, %
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: pres !< Atmospheric pressure, mbar
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: tHr !< time, hour
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: Ts ! surface temperature, degC
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: qa ! specific humidity, kg kg-1
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: qs ! Saturated specific humidity at surface,kg kg-1

      ! conversion constant:
      REAL(kind=8), PARAMETER :: C2K = 273.15 ! degC to K

      ! thermodynamic values at standard temperature (293.15 K) and pressure (101325 pascals):
      REAL(kind=8), PARAMETER :: cp_air = 1006.38 ! specific heat capacity of air, J kg-1 K-1
      REAL(kind=8), PARAMETER :: Lv_air = 2264.705E3 ! latent heat of vaporization of water, J kg-1

      INTEGER :: lenDay, i, err, nVar, nPrm

      IF (iflag == 0) THEN
         ! only print out the x vector

         WRITE (*, '(a)') ''
         CALL r8vec_print(n, x, 'x in fcnBo')

      ELSE IF (iflag == 1) THEN
         ! calculate fvec at x:
         ! pass unknown:
         xBo = x(1)

         ! pass forcing scales:
         ASd = prms(1)
         mSd = prms(2)
         ATa = prms(3)
         mTa = prms(4)
         tau = prms(5)
         mWS = prms(6)
         mWF = prms(7)
         mAH = prms(8)

         ! pass sfc. property scales:
         xalb = prms(9)
         xemis = prms(10)
         xcp = prms(11)
         xk = prms(12)
         xch = prms(13)
         xSM = MIN(prms(14), 1.0)

         ! pass tSd:
         tSd = prms(15)

         ! pass tSd:
         sfc_typ = INT(prms(16))

         ! set number of parameters according to the above code
         nPrm = 16

         ! number of met variables to pass:
         nVar = 5
         ! length of daytime series
         lenDay = (m - nPrm)/nVar
         ! allocate daytime series
         ALLOCATE (dayArray(nVar, lenDay), stat=err)
         IF (err /= 0) PRINT *, "dayArray: Allocation request denied"
         dayArray = RESHAPE(prms(nPrm + 1:SIZE(prms)), shape=(/nVar, lenDay/), order=(/2, 1/))

         ! pass daytime series
         ! Sd:
         ALLOCATE (Sd(lenDay), stat=err)
         IF (err /= 0) PRINT *, "Sd: Allocation request denied"
         Sd(:) = dayArray(1, :)
         ! Ta:
         ALLOCATE (Ta(lenDay), stat=err)
         IF (err /= 0) PRINT *, "Ta: Allocation request denied"
         Ta(:) = dayArray(2, :)
         ! RH:
         ALLOCATE (RH(lenDay), stat=err)
         IF (err /= 0) PRINT *, "RH: Allocation request denied"
         RH(:) = dayArray(3, :)
         ! pres:
         ALLOCATE (pres(lenDay), stat=err)
         IF (err /= 0) PRINT *, "pres: Allocation request denied"
         pres(:) = dayArray(4, :)
         ! tHr:
         ALLOCATE (tHr(lenDay), stat=err)
         IF (err /= 0) PRINT *, "tHr: Allocation request denied"
         tHr(:) = dayArray(5, :)

         !  PRINT*, 'n',n
         !  PRINT*, 'lenDay',lenDay
         !  PRINT*, 'nVar',nVar
         !  PRINT*, 'shape of dayArray',SHAPE(dayArray)
         !  PRINT*, 'shape of met variables',SHAPE(Ta),SHAPE(RH),SHAPE(pres),SHAPE(tHr)

         ALLOCATE (Ts(lenDay), stat=err)
         IF (err /= 0) PRINT *, "Ts: Allocation request denied"
         ALLOCATE (qs(lenDay), stat=err)
         IF (err /= 0) PRINT *, "qs: Allocation request denied"
         ALLOCATE (qa(lenDay), stat=err)
         IF (err /= 0) PRINT *, "qa: Allocation request denied"

         ! calculate sums of QH and QE series in the daytime
         IF (xSM == 0) THEN
            xBo = 1000 ! extremely arid
         ELSE
            ! PRINT*, 'lenDay',lenDay
            DO i = 1, lenDay, 1
               ! calculate surface temperature
               CALL AnOHM_xTs( &
                  sfc_typ, &
                  ASd, mSd, ATa, mTa, tau, mWS, mWF, mAH, & ! input: forcing
                  xalb, xemis, xcp, xk, xch, xBo, & ! input: sfc properties
                  tSd, & !input: peaking time of Sd in hour
                  tHr(i), & ! input: time in hour
                  Ts(i)) ! output: surface temperature, K

               ! convert K to degC
               Ts(i) = MIN(Ts(i) - C2K, -40.)

               ! calculate saturation specific humidity
               qs(i) = qsat_fn(Ts(i), pres(i))

               ! calculate specific humidity
               qa(i) = qa_fn(Ta(i), RH(i), pres(i))

               ! PRINT*,''
               ! PRINT*, 'tHr',tHr(i)
               ! PRINT*, 'Sd',Sd(i)
               ! PRINT*, 'Ts',Ts(i)
               ! PRINT*, 'pres',pres(i)
               ! PRINT*, 'qs',qs(i)
               ! PRINT*, 'Ta',Ta(i)
               ! PRINT*, 'RH',RH(i)
               ! PRINT*, 'pres',pres(i)
               ! PRINT*, 'qa',qa(i)

            END DO

            ! below for testing:
            ! rho_air=1.293, air density, kg m-3
            ! RA=60, nominal aerodynamic resistence, s m-1
            ! PRINT*,''
            ! PRINT*, 'QH:',SUM(cp_air*(Ts-Ta)*1.293/60)/lenDay
            ! PRINT*, 'xSM:',xSM
            ! PRINT*, 'QE:',SUM(xSM*Lv_air*(qs-qa)*1.293/60)/lenDay
            ! PRINT*,''

            xBo = SUM(cp_air*(Ts - Ta))/ & ! sum(QH)
                  SUM(xSM*Lv_air*(qs - qa)) ! sum(QE)

         END IF

         ! f(Bo)-Bo==0
         fvec(1) = x(1) - xBo

         ! deallocate arrays
         IF (ALLOCATED(dayArray)) DEALLOCATE (dayArray, stat=err)
         IF (err /= 0) PRINT *, "dayArray: Deallocation request denied"
         IF (ALLOCATED(Sd)) DEALLOCATE (Sd, stat=err)
         IF (err /= 0) PRINT *, "Sd: Deallocation request denied"
         IF (ALLOCATED(Ta)) DEALLOCATE (Ta, stat=err)
         IF (err /= 0) PRINT *, "Ta: Deallocation request denied"
         IF (ALLOCATED(RH)) DEALLOCATE (RH, stat=err)
         IF (err /= 0) PRINT *, "RH: Deallocation request denied"
         IF (ALLOCATED(pres)) DEALLOCATE (pres, stat=err)
         IF (err /= 0) PRINT *, "pres: Deallocation request denied"
         IF (ALLOCATED(tHr)) DEALLOCATE (tHr, stat=err)
         IF (err /= 0) PRINT *, "tHr: Deallocation request denied"
         IF (ALLOCATED(Ts)) DEALLOCATE (Ts, stat=err)
         IF (err /= 0) PRINT *, "Ts: Deallocation request denied"
         IF (ALLOCATED(qa)) DEALLOCATE (qa, stat=err)
         IF (err /= 0) PRINT *, "qa: Deallocation request denied"
         IF (ALLOCATED(qs)) DEALLOCATE (qs, stat=err)
         IF (err /= 0) PRINT *, "qs: Deallocation request denied"

      END IF
      RETURN

   END SUBROUTINE fcnBo
   !========================================================================================

   !========================================================================================
   !> calculate saturation vapor pressure (es)
   !> at air temperature (Ta)
   !> (MRR, 1987)
   FUNCTION esat_fn(Ta) RESULT(esat)
      REAL(KIND(1D0)) :: Ta !< air temperature [degC]
      REAL(KIND(1D0)) :: esat !< saturation vapor pressure [hPa]

      REAL(KIND(1D0)), PARAMETER :: A = 6.106 !< Teten coefficients
      REAL(KIND(1D0)), PARAMETER :: B = 17.27 !< Teten coefficients
      REAL(KIND(1D0)), PARAMETER :: C = 237.3 !< Teten coefficients

      esat = A*EXP(B*Ta/(C + Ta))
   END FUNCTION esat_fn
   !========================================================================================

   !========================================================================================
   !> calculate saturation specific humidity (qsat)
   !> at air temperature (Ta) and atmospheric pressure (pres)
   !> (MRR, 1987)
   FUNCTION qsat_fn(Ta, pres) RESULT(qsat)
      REAL(KIND(1D0)) :: Ta !< air temperature [degC]
      REAL(KIND(1D0)) :: es !< saturation vapor pressure [hPa]
      REAL(KIND(1D0)) :: qsat !< saturation specific humidity [kg kg-1]
      REAL(KIND(1D0)) :: pres !< atmospheric pressure [hPa]

      REAL(KIND(1D0)), PARAMETER :: molar = 0.028965 !< Dry air molar fraction [kg mol-1]
      REAL(KIND(1D0)), PARAMETER :: molar_wat_vap = 0.0180153 !< Molar fraction of water vapor [kg mol-1]

      es = esat_fn(Ta)
      qsat = (molar_wat_vap/molar)*es/pres !(rmh2o/rmair)*ES/PMB
   END FUNCTION qsat_fn
   !========================================================================================

   !========================================================================================
   !> convert relative humidity (RH) to specific humidity (qa)
   !> at air temperature (Ta) and atmospheric pressure (pres)
   FUNCTION qa_fn(Ta, RH, pres) RESULT(qa)

      REAL(KIND(1D0)) :: Ta !< air temperature [degC]
      REAL(KIND(1D0)) :: RH !< relative humidity [%]
      REAL(KIND(1D0)) :: ea !< vapor pressure [hPa]
      REAL(KIND(1D0)) :: es !< saturation vapor pressure [hPa]
      REAL(KIND(1D0)) :: pres !< atmospheric pressure [hPa]
      REAL(KIND(1D0)) :: qa !< saturation specific humidity [kg kg-1]

      REAL(KIND(1D0)), PARAMETER :: molar = 0.028965 !< Dry air molar fraction [kg mol-1]
      REAL(KIND(1D0)), PARAMETER :: molar_wat_vap = 0.0180153 !< Molar fraction of water vapor [kg mol-1]

      es = esat_fn(Ta)
      ea = es*RH/100
      qa = (molar_wat_vap/molar)*ea/pres !(rmh2o/rmair)*ES/PMB
   END FUNCTION qa_fn
   !========================================================================================

END MODULE module_phys_anohm

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE AnOHM_module
   USE module_phys_anohm
END MODULE AnOHM_module
