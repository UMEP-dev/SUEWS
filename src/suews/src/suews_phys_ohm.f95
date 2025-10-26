! Main module following naming standard: matches filename
MODULE module_phys_ohm
   ! USE module_ctrl_const_allocate
   ! USE module_ctrl_const_datain
   ! USE module_ctrl_const_default
   ! USE module_ctrl_const_gis
   ! USE module_ctrl_const_sues
   ! USE module_ctrl_const_time

   IMPLICIT NONE
CONTAINS
!========================================================================================
   SUBROUTINE OHM(qn1, qn_av_prev, dqndt_prev, qn_av_next, dqndt_next, &
                  qn1_S, qn_s_av_prev, dqnsdt_prev, qn_s_av_next, dqnsdt_next, &
                  tstep, dt_since_start, &
                  sfr_surf, nsurf, &
                  Tair_mav_5d, &
                  OHM_coef, &
                  OHM_threshSW, OHM_threshWD, &
                  soilstore_id, SoilStoreCap, state_id, &
                  BldgSurf, WaterSurf, &
                  SnowUse, SnowFrac, &
                  ws, T_hbh_C, T_prev, &
                  ws_rav, qn_rav, nlayer, &
                  dz_roof, cp_roof, k_roof, &
                  dz_wall, cp_wall, k_wall, &
                  lambda_c, &
                  StorageHeatMethod, DiagQS, timer, &
                  a1_bldg, a2_bldg, a3_bldg, &
                  a1, a2, a3, qs, deltaQi)
      ! Made by HCW Jan 2015 to replace OHMnew (no longer needed).
      ! Calculates net storage heat flux (QS) from Eq 4, Grimmond et al. 1991, Atm Env.
      ! Accounts for variable timesteps in dQ*/dt term.
      ! BareSoilSurfFraction removed so bare soil is now handled like other surfaces.
      ! Snow part changed from summer wet to winter wet coefficients.
      ! Changed -333 checks to -999 checks and added error handling
      ! Gradient now calculated for t-1 (was previously calculated for t-2).
      ! TS 28 Jun 2018:
      !  improved and tested the phase-in method for calculating dqndt
      ! TS & SG 30 Apr 2018:
      !  a new calculation scheme of dqndt by using a phase-in approach that releases
      !  the requirement for storeing multiple qn values for adapting SUEWS into WRF
      ! TS 07 Aug 2017:
      !  1. interface changed to account for explict passing
      !  2. calculation refactorization.
      ! HCW 25 Feb 2015:
      !  Adapted q1,q2,q3 & r1,r2,r3 for multiple grids
      ! HCW 14 Dec 2016:
      !  Thresholds for Summer/Winter and Wet/Dry now provided in input files
      !  Calculation of dqndt now uses hourly running mean rather than instantaneous values
      ! To Do:
      !   - No canyons implemented at the moment [OHM_coef(nsurf+1,,)]
      !========================================================================================

      USE module_ctrl_const_allocate, ONLY: ndepth
      USE module_util_datetime, ONLY: datetime, timedelta
      USE SUEWS_DEF_DTS, ONLY: SUEWS_TIMER

      IMPLICIT NONE
      TYPE(SUEWS_TIMER) :: timer
      INTEGER, INTENT(in) :: StorageHeatMethod !
      INTEGER, INTENT(in) :: tstep ! time step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]

      INTEGER, INTENT(in) :: nlayer ! number of vertical levels in urban canopy

      !INTEGER :: iy, id, it, imin, isec
      !new_day
      TYPE(datetime) :: time_now, time_prev, time_next
      LOGICAL :: first_tstep_Q, last_tstep_Q
      !INTEGER :: tstep_prev

      REAL(KIND(1D0)), INTENT(in) :: qn1 ! net all-wave radiation
      REAL(KIND(1D0)), INTENT(in) :: qn1_S ! net all-wave radiation over snow
      REAL(KIND(1D0)), INTENT(in) :: sfr_surf(nsurf) ! surface fractions
      REAL(KIND(1D0)), INTENT(in) :: SnowFrac(nsurf) ! snow fractions of each surface
      REAL(KIND(1D0)), INTENT(in) :: Tair_mav_5d ! Tair_mav_5d=HDD(id-1,4) HDD at the begining of today (id-1)
      REAL(KIND(1D0)), INTENT(inout) :: OHM_coef(nsurf + 1, 4, 3) ! OHM coefficients
      REAL(KIND(1D0)), INTENT(in) :: OHM_threshSW(nsurf + 1), OHM_threshWD(nsurf + 1) ! OHM thresholds
      REAL(KIND(1D0)), INTENT(in) :: soilstore_id(nsurf) ! soil moisture
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap(nsurf) ! capacity of soil store
      REAL(KIND(1D0)), INTENT(in) :: state_id(nsurf) ! wetness status

      INTEGER, INTENT(in) :: nsurf ! number of surfaces
      ! INTEGER,INTENT(in)::nsh       ! number of timesteps in one hour
      ! integer,intent(in) :: dt      ! current timestep [second]
      ! integer,intent(INOUT) :: dt0  ! period length for qn1 memory
      INTEGER, INTENT(in) :: BldgSurf ! code for specific surfaces
      INTEGER, INTENT(in) :: WaterSurf ! code for specific surfaces
      INTEGER, INTENT(in) :: SnowUse ! option for snow related calculations
      INTEGER, INTENT(in) :: DiagQS ! diagnostic option

      REAL(KIND(1D0)), INTENT(in) :: qn_av_prev
      REAL(KIND(1D0)), INTENT(out) :: qn_av_next
      REAL(KIND(1D0)), INTENT(in) :: dqndt_prev ! Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(out) :: dqndt_next ! Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(in) :: qn_s_av_prev
      REAL(KIND(1D0)), INTENT(out) :: qn_s_av_next
      REAL(KIND(1D0)), INTENT(in) :: dqnsdt_prev ! Rate of change of net radiation [W m-2 h-1] at t-1
      REAL(KIND(1D0)), INTENT(out) :: dqnsdt_next ! Rate of change of net radiation [W m-2 h-1] at t-1

      REAL(KIND(1D0)), INTENT(out) :: qs ! storage heat flux
      ! REAL(KIND(1d0)),INTENT(out)::deltaQi(nsurf+1) ! storage heat flux of snow surfaces
      REAL(KIND(1D0)), INTENT(out) :: deltaQi(nsurf) ! storage heat flux of snow surfaces

      REAL(KIND(1D0)), INTENT(in) :: ws ! wind speed at half building height [m/s]
      REAL(KIND(1D0)), INTENT(in) :: T_hbh_C ! current half building height temperature [°C]
      REAL(KIND(1D0)), INTENT(inout) :: T_prev ! previous midnight air temperature [°C]
      REAL(KIND(1D0)), INTENT(inout) :: ws_rav ! running average of wind speed [m/s]
      REAL(KIND(1D0)), INTENT(inout) :: qn_rav ! running average of net all-wave radiation [W m-2]

      ! Building material properties
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof

      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall

      REAL(KIND(1D0)), INTENT(IN) :: lambda_c ! Building surface to plan area ratio [-]

      REAL(KIND(1D0)), INTENT(INOUT) :: a1_bldg, a2_bldg, a3_bldg ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)), INTENT(out) :: a1, a2, a3 ! OHM coefficients of grid

      ! REAL(KIND(1d0)):: nsh_nna ! number of timesteps per hour with non -999 values (used for spinup)

      ! REAL(KIND(1d0)):: dqndt    !Rate of change of net radiation [W m-2 h-1] at t-1
      ! REAL(KIND(1d0)):: surfrac  !Surface fraction accounting for SnowFrac if appropriate

      REAL(KIND(1D0)) :: deltaQi0 ! temporarily store

      ! REAL(KIND(1d0)):: qn1_store_grid0(nsh), qn1_av_store_grid0(2*nsh+1) ! temporarily store

      !These are now provided in SiteInfo (OHMthresh for Summer/Winter and Wet/Dry)
   !!real(kind(1d0)):: OHM_TForSummer = 5  !Use summer coefficients if 5-day Tair >= 5 degC
      !real(kind(1d0)):: OHM_TForSummer = 10  !Use summer coefficients if 5-day Tair >= 10 degC - modified for UK HCW 14 Dec 2015
      !real(kind(1d0)):: OHM_SMForWet = 0.9  !Use wet coefficients if SM close to soil capacity

      IF (StorageHeatMethod == 6) THEN
         ! MP 14/04/2025
         ! get timestamps
         ASSOCIATE ( &
            iy => timer%iy, &
            id => timer%id, &
            it => timer%it, &
            imin => timer%imin, &
            isec => timer%isec, &
            tstep_prev => timer%tstep_prev, &
            new_day => timer%new_day, &
            dt_since_start_prev => timer%dt_since_start_prev &
            )
            time_now = datetime(year=iy) + timedelta(days=id - 1, hours=it, minutes=imin, seconds=isec)
            ! WRF-SUEWS COUPLING: tstep_prev allows for adaptive timesteps in WRF
            ! In standalone SUEWS, tstep_prev always equals tstep
            time_prev = time_now - timedelta(seconds=tstep_prev)
            time_next = time_now + timedelta(seconds=tstep)

            ! test if time at now is the first/last tstep of today
            first_tstep_Q = time_now%getDay() /= time_prev%getDay()
            last_tstep_Q = time_now%getDay() /= time_next%getDay()

            IF (dt_since_start /= dt_since_start_prev) THEN
               IF (dt_since_start < 86400) THEN
                  ws_rav = ws_rav + (ws - ws_rav)/(dt_since_start/tstep)
                  qn_rav = qn_rav + (qn1 - qn_rav)/(dt_since_start/tstep)
               ELSE
                  ws_rav = ws_rav + (ws - ws_rav)/(86400/tstep)
                  qn_rav = qn_rav + (qn1 - qn_rav)/(86400/tstep)
               END IF
            END IF

            IF (first_tstep_Q .AND. new_day == 1) THEN
               CALL OHM_yl_cal(dt_since_start, &
                               ws_rav, T_hbh_C, T_prev, qn_rav, & ! Input
                               dz_wall(1, 1), cp_wall(1, 1), k_wall(1, 1), lambda_c, &
                               a1_bldg, a2_bldg, a3_bldg & ! Output
                               )
               new_day = 0
               T_prev = T_hbh_C
            ELSE IF (last_tstep_Q) THEN
               new_day = 1
            END IF

            dt_since_start_prev = dt_since_start
         END ASSOCIATE

         OHM_coef(2, 1, 1) = a1_bldg
         OHM_coef(2, 2, 1) = a1_bldg
         OHM_coef(2, 3, 1) = a1_bldg
         OHM_coef(2, 4, 1) = a1_bldg

         OHM_coef(2, 1, 2) = a2_bldg
         OHM_coef(2, 2, 2) = a2_bldg
         OHM_coef(2, 3, 2) = a2_bldg
         OHM_coef(2, 4, 2) = a2_bldg

         OHM_coef(2, 1, 3) = a3_bldg
         OHM_coef(2, 2, 3) = a3_bldg
         OHM_coef(2, 3, 3) = a3_bldg
         OHM_coef(2, 4, 3) = a3_bldg

      END IF

      CALL OHM_coef_cal(sfr_surf, nsurf, &
                        Tair_mav_5d, OHM_coef, OHM_threshSW, OHM_threshWD, &
                        soilstore_id, SoilStoreCap, state_id, &
                        BldgSurf, WaterSurf, &
                        SnowUse, SnowFrac, &
                        a1, a2, a3)

      ! WRITE(*,*) '----- OHM coeffs new-----'
      ! WRITE(*,*) a1,a2,a3

      ! Old OHM calculations (up to v2016a)
   !! Calculate radiation part ------------------------------------------------------------
      !qs=NAN              !qs  = Net storage heat flux  [W m-2]
      !if(qn1>-999) then   !qn1 = Net all-wave radiation [W m-2]
      !   !if(q1>-999.and.q3>-999) then
      !      !dqndt = 0.5*(q3-q1)*nsh_real                !gradient at t-2
      !      dqndt = 0.5*(qn1-q2_grids(Gridiv))*nsh_real   !gradient at t-1
      !
      !      !Calculate net storage heat flux
      !      qs = qn1*a1 + dqndt*a2 + a3   !Eq 4, Grimmond et al. 1991
      !   !endif
      !   !q1=q2  !q1 = net radiation at t-2 (at t-3 when q1 used in next timestep)
      !   !q2=q3  !q2 = net radiation at t-1
      !   !q3=qn1  !q3 = net radiation at t   (at t-1 when q3 used in next timestep)
      !   q1_grids(Gridiv) = q2_grids(Gridiv) !q1 = net radiation at t-2 (at t-3 when q1 used in next timestep)
      !   q2_grids(Gridiv) = q3_grids(Gridiv) !q2 = net radiation at t-1
      !   q3_grids(Gridiv) = qn1              !q3 = net radiation at t (at t-1 when q3 used in next timestep)
      !else
      !   call ErrorHint(21,'Bad value for qn1 found during OHM calculation',qn1,NotUsed,notUsedI)
      !endif

      ! New OHM calculations (v2017a onwards) using running mean (HCW Dec 2016)
      ! Calculate radiation part ------------------------------------------------------------
      qs = -999 !qs  = Net storage heat flux  [W m-2]
      IF (qn1 > -999) THEN !qn1 = Net all-wave radiation [W m-2]
         ! Store instantaneous qn1 values for previous hour (qn1_store_grid) and average (qn1_av)
         ! print*,''
         ! CALL OHM_dqndt_cal(nsh,qn1,qn1_store_grid,qn1_av_store_grid,dqndt)
         ! print*, 'old dqndt',dqndt
         CALL OHM_dqndt_cal_X(tstep, dt_since_start, qn_av_prev, qn1, dqndt_prev, &
                              qn_av_next, dqndt_next)
         ! print*, 'new dqndt',dqndt

         ! Calculate net storage heat flux
         CALL OHM_QS_cal(qn1, dqndt_next, a1, a2, a3, qs)
         IF (DiagQS == 1) WRITE (*, *) 'qs: ', qs, 'qn1:', qn1, 'dqndt: ', dqndt_next

      ELSE
         CALL ErrorHint(21, 'In SUEWS_OHM.f95: bad value for qn1 found during qs calculation.', qn1, -55.55D0, -55)
      END IF

      !write(*,*) qs
      !write(*,*) '--------------------'

      ! Do snow calculations separately -----
      ! Added by LJ in August 2013
      IF (SnowUse == 1) THEN
         deltaQi = -999
         IF (qn1_S > -999) THEN
            ! Old OHM calculations (commented out HCW Dec 2016)
         !!if(r1>-999.and.r3>-999) then
            !   !dqndt = 0.5*(r3-r1)*nsh_real    !gradient at t-2
            !   dqndt = 0.5*(qn1_S-r2_grids(Gridiv))*nsh_real     !gradient at t-1
            !   ! Calculate net storage heat flux for snow surface (winter wet conditions HCW 15/01/2015)
            !   deltaQi = qn1_S*OHM_coef(nsurf+1,3,1) + dqndt*OHM_coef(nsurf+1,3,2) + OHM_coef(nsurf+1,3,3)
         !!endif
            !r1_grids(Gridiv)=r2_grids(Gridiv)
            !r2_grids(Gridiv)=r3_grids(Gridiv)
            !r3_grids(Gridiv)=qn1_S
            ! New OHM calculations
            ! Store instantaneous qn1 values for previous hour (qn1_store_grid) and average (qn1_av)
            ! CALL OHM_dqndt_cal(nsh,qn1_S,qn1_S_store_grid,qn1_S_av_store_grid,dqndt)

            CALL OHM_dqndt_cal_X(tstep, dt_since_start, qn_s_av_prev, qn1_S, dqnsdt_prev, &
                                 qn_s_av_next, dqnsdt_next)

            ! Calculate net storage heat flux for snow surface (winter wet conditions)
            CALL OHM_QS_cal(qn1_S, dqnsdt_next, &
                            OHM_coef(nsurf + 1, 3, 1), OHM_coef(nsurf + 1, 3, 2), OHM_coef(nsurf + 1, 3, 3), &
                            deltaQi0)
            deltaQi = deltaQi0

         ELSE
            CALL ErrorHint(21, 'In SUEWS_OHM.f95: bad value for qn1(snow) found during qs calculation.', qn1_S, -55.55D0, -55)
         END IF

      END IF

      RETURN
   END SUBROUTINE OHM
!========================================================================================

   SUBROUTINE OHM_coef_cal(sfr_surf, nsurf, &
                           Tair_mav_5d, OHM_coef, OHM_threshSW, OHM_threshWD, &
                           soilstore_id, SoilStoreCap, state_id, &
                           BldgSurf, WaterSurf, &
                           SnowUse, SnowFrac, &
                           a1, a2, a3)
      IMPLICIT NONE
      INTEGER, INTENT(in) :: &
         nsurf, & ! number of surfaces
         SnowUse, & ! option for snow related calculations
         BldgSurf, WaterSurf ! code for specific surfaces
      REAL(KIND(1D0)), INTENT(in) :: &
         sfr_surf(nsurf), & ! surface cover fractions
         SnowFrac(nsurf), & ! snow fractions of each surface
         Tair_mav_5d, & ! Tair_mav_5d=HDD(id-1,4) HDD at the begining of today (id-1)
         OHM_coef(nsurf + 1, 4, 3), &
         OHM_threshSW(nsurf + 1), OHM_threshWD(nsurf + 1), & ! OHM thresholds
         soilstore_id(nsurf), & ! soil moisture
         SoilStoreCap(nsurf), & ! capacity of soil store
         state_id(nsurf) ! wetness status
      REAL(KIND(1D0)), INTENT(out) :: a1, a2, a3

      REAL(KIND(1D0)) :: surfrac
      INTEGER :: i, ii, is

      ! OHM coefficients --------
      ! Set to zero initially
      a1 = 0 ![-]
      a2 = 0 ![h]
      a3 = 0 ![W m-2]
      ! -------------------------

      ! Loop through surface types ----------------------------------------------------------
      DO is = 1, nsurf
         surfrac = sfr_surf(is)

         ! Use 5-day running mean Tair to decide whether it is summer or winter ----------------
         IF (Tair_mav_5d >= OHM_threshSW(is)) THEN !Summer
            ii = 0
         ELSE !Winter
            ii = 2
         END IF

         IF (state_id(is) > 0) THEN !Wet surface
            i = ii + 1
         ELSE !Dry surface
            i = ii + 2
            ! If the surface is dry but SM is close to capacity, use coefficients for wet surfaces
            IF (is > BldgSurf .AND. is /= WaterSurf) THEN !Wet soil (i.e. EveTr, DecTr, Grass, BSoil surfaces)
               IF (soilstore_id(is)/SoilStoreCap(is) > OHM_threshWD(is)) THEN
                  i = ii + 1
               END IF
            END IF
         END IF

         ! If snow, adjust surface fractions accordingly
         IF (SnowUse == 1 .AND. is /= BldgSurf .AND. is /= WaterSurf) THEN ! QUESTION: Why is BldgSurf excluded here?
            surfrac = surfrac*(1 - SnowFrac(is))
         END IF

         ! Calculate the areally-weighted OHM coefficients
         a1 = a1 + surfrac*OHM_coef(is, i, 1)
         a2 = a2 + surfrac*OHM_coef(is, i, 2)
         a3 = a3 + surfrac*OHM_coef(is, i, 3)

      END DO !end of loop over surface types ------------------------------------------------
   END SUBROUTINE OHM_coef_cal

! Updated OHM calculations for WRF-SUEWS coupling (v2018b onwards) weighted mean (TS Apr 2018)
   SUBROUTINE OHM_dqndt_cal_X(dt, dt_since_start, qn1_av_prev, qn1, dqndt_prev, qn1_av_next, dqndt_next)
      IMPLICIT NONE
      INTEGER, INTENT(in) :: dt ! time step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]
      REAL(KIND(1D0)), INTENT(in) :: qn1 ! new qn1 value [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: qn1_av_prev ! weighted average of qn1 [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: dqndt_prev ! dQ* per dt for 60 min [W m-2 h-1]
      REAL(KIND(1D0)), INTENT(out) :: qn1_av_next ! weighted average of qn1 [W m-2]
      REAL(KIND(1D0)), INTENT(out) :: dqndt_next ! dQ* per dt for 60 min [W m-2 h-1]
      REAL(KIND(1D0)), PARAMETER :: dt0_thresh = 3600 ! threshold for period of dqndt0 [s]
      REAL(KIND(1D0)), PARAMETER :: window_hr = 2 ! window size for Difference calculation [hr]

      INTEGER :: dt0 ! period of dqndt0 [s]

      REAL(KIND(1D0)) :: qn1_av_0 !, qn1_av_start,qn1_av_end

      ! if previous period shorter than dt0_thresh, expand the storage/memory period
      IF (dt_since_start < dt0_thresh) THEN ! spinup period
         dt0 = dt_since_start + dt

      ELSE ! effective period
         dt0 = dt0_thresh
      END IF

      ! get weighted average at a previous time specified by `window_hr`
      qn1_av_0 = qn1_av_prev - dqndt_prev*(window_hr - dt/3600.)

      ! averaged qn1 for previous period = dt0_thresh
      qn1_av_next = (qn1_av_prev*(dt0 - dt) + qn1*dt)/(dt0)

      ! do weighted average to calculate the difference by using the memory value and new forcing value
      ! NB: keep the output dqndt in [W m-2 h-1]
      dqndt_next = (qn1_av_next - qn1_av_0)/window_hr

   END SUBROUTINE OHM_dqndt_cal_X

! New OHM calculations (v2017a-v2018a) using running mean (HCW Dec 2016)
   SUBROUTINE OHM_dqndt_cal(nsh, qn, qn_store_grid, qn_av_store_grid, dqndt)
      IMPLICIT NONE
      INTEGER, INTENT(in) :: nsh ! number of timesteps in one hour
      REAL(KIND(1D0)), INTENT(in) :: qn
      REAL(KIND(1D0)), INTENT(inout) :: qn_store_grid(nsh) ! instantaneous qn1 values for previous hour
      REAL(KIND(1D0)), INTENT(inout) :: qn_av_store_grid(2*nsh + 1) ! average qn1 values for previous hour
      REAL(KIND(1D0)), INTENT(out) :: dqndt !dQ* per dt for 60 min

      REAL(KIND(1D0)) :: qn_av
      INTEGER :: nsh_nna

      dqndt = -999 ! initialise as -999

      ! Store instantaneous qn1 values for previous hour (qn1_store_grid) and average (qn1_av)
      IF (nsh > 1) THEN
         qn_store_grid = CSHIFT(qn_store_grid, 1) ! shift to left with one place
         qn_store_grid(nsh) = qn
         nsh_nna = COUNT(qn_store_grid /= -999, dim=1) !Find how many are not -999s  !bug fixed HCW 08 Feb 2017
         qn_av = SUM(qn_store_grid, mask=qn_store_grid /= -999)/nsh_nna
      ELSEIF (nsh == 1) THEN
         qn_store_grid(:) = qn
         qn_av = qn
      END IF
      ! Store hourly average values (calculated every timestep) for previous 2 hours
      IF (nsh > 1) THEN
         qn_av_store_grid = CSHIFT(qn_av_store_grid, 1)
         qn_av_store_grid(2*nsh + 1) = qn_av
      ELSEIF (nsh == 1) THEN
         qn_av_store_grid(:) = qn_av
      END IF
      ! Calculate dQ* per dt for 60 min (using running mean Q* at t hours and (t-2) hours)
      IF (ANY(qn_av_store_grid == -999)) THEN
         dqndt = 0 ! Set dqndt term to zero for spinup
      ELSE
         dqndt = 0.5*(qn_av_store_grid((2*nsh + 1)) - qn_av_store_grid(1))
      END IF

   END SUBROUTINE OHM_dqndt_cal

   SUBROUTINE OHM_QS_cal(qn1, dqndt, a1, a2, a3, qs)
      IMPLICIT NONE
      REAL(KIND(1D0)), INTENT(in) :: qn1, dqndt, a1, a2, a3
      REAL(KIND(1D0)), INTENT(out) :: qs
      qs = -999 ! initialise as -999
      qs = qn1*a1 + dqndt*a2 + a3 !Eq 4, Grimmond et al. 1991

   END SUBROUTINE OHM_QS_cal

   SUBROUTINE OHM_yl_cal(dt_since_start, &
                         ws, t_now, t_prev, qstar, & ! Input
                         d, C, k, lambda_c, &
                         a1, a2, a3 & ! Output
                         )
      ! Liu (2025) parameterisation of objective hysteresis model coefficients to improve building storage heat flux accuracy

      IMPLICIT NONE
      ! Input parameters
      INTEGER, INTENT(in) :: dt_since_start ! Time since simulation starts [s]

      ! Building material properties
      REAL(KIND(1D0)) :: d ! Thickness [m]
      REAL(KIND(1D0)) :: C ! Volumetric heat capacity (specific heat * density) [J K-1 m-3]
      REAL(KIND(1D0)) :: k ! Thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)) :: lambda_c ! Building surface to plan area ratio [-]

      ! Meteorology
      REAL(KIND(1D0)) :: ws ! Wind speed [ms-1]
      REAL(KIND(1D0)) :: t_now ! Current 2m-air temperature [°C]
      REAL(KIND(1D0)) :: t_prev ! Previous midnight 2m-air temperature [°C]
      REAL(KIND(1D0)) :: dtair ! Mid-night (00:00) 2m-air temperature difference compared to the previous day (°C)
      REAL(KIND(1D0)) :: qstar ! Daily mean net all-wave radiation (normalized by building footprint area) [W m-2]

      ! Local variables for file I/O
      INTEGER :: iunit, ios
      CHARACTER(LEN=100) :: filename

      ! Output parameters
      REAL(KIND(1D0)) :: a1, a2, a3 ! OHM coefficients

      ! Local variables for reading CSV file
      INTEGER :: id_prev = 0
      INTEGER :: iunit_csv, ios_csv
      CHARACTER(LEN=100) :: csv_filename
      CHARACTER(LEN=100) :: csv_date
      CHARACTER(LEN=256) :: csv_line
      REAL(KIND(1D0)) :: csv_WS, csv_QStar, csv_dTair
      REAL(KIND(1D0)) :: csv_a1_cal, csv_a2_cal, csv_a3_cal
      INTEGER :: csv_tstep
      CHARACTER(LEN=256) :: iomsg
      INTEGER :: row_num
      INTEGER :: csv_year, csv_month, csv_day
      INTEGER :: pos1, pos2, pos3, pos4, pos5, pos6

      ! Ensure wind speed is positive
      IF (ws <= 0) THEN
         ws = 0.1
      END IF

      dtair = t_now - t_prev

      CALL calculate_a1(d, C, k, lambda_c, ws, qstar, a1)
      CALL calculate_a2(d, C, k, ws, qstar, lambda_c, a2)
      CALL calculate_a3(qstar, dtair, a1, lambda_c, a3)

      !! Create a filename for the coefficients file
      !filename = 'OHM_coefficients.csv'
      !
      ! Open the file for appending
      !OPEN (NEWUNIT=iunit, FILE=filename, STATUS='OLD', ACTION='WRITE', POSITION='APPEND', IOSTAT=ios)
      !IF (ios /= 0) THEN
      !   ! If the file does not exist, create it and write the header
      !   OPEN (NEWUNIT=iunit, FILE=filename, STATUS='NEW', ACTION='WRITE', IOSTAT=ios)
      !   IF (ios /= 0) THEN
      !      PRINT *, 'Error opening file: ', filename
      !      STOP
      !   END IF
      !   WRITE (iunit, '(A)') 'ts,ws,dtair,qstar,a1,a2,a3,d,C,k'
      !END IF
      !
      !! Write the coefficients to the file
      !WRITE (iunit, '(I10, ",", F20.10, ",", F20.10, ",", F20.10, ",", F20.10, ",", F20.10, ",", F20.10, ",", F20.10, ",", F20.10, ",", F20.10)') dt_since_start, ws, dtair, qstar, a1, a2, a3, d, C, k
      !
      !! Close the file
      !CLOSE (iunit)

   END SUBROUTINE OHM_yl_cal

   SUBROUTINE calculate_a1(d, C, k, lambda_c, WS, QStar, a1)
      IMPLICIT NONE
      ! Input parameters
      REAL(KIND(1D0)), INTENT(in) :: d ! Thickness [m]
      REAL(KIND(1D0)), INTENT(in) :: C ! Volumetric heat capacity (specific heat * density) [J K-1 m-3]
      REAL(KIND(1D0)), INTENT(in) :: k ! Thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: lambda_c ! Building surface to plan area ratio [-]
      REAL(KIND(1D0)), INTENT(in) :: WS ! Wind speed [ms-1]
      REAL(KIND(1D0)), INTENT(in) :: QStar ! Daily mean net all-wave radiation (normalized by building footprint area) [W m-2]

      ! Output parameter
      REAL(KIND(1D0)), INTENT(out) :: a1 ! OHM coefficient a1

      ! Local variables
      REAL(KIND(1D0)) :: TA ! Thermal admittance
      REAL(KIND(1D0)) :: S_a1
      REAL(KIND(1D0)) :: omega_a1
      REAL(KIND(1D0)) :: theta_a1
      REAL(KIND(1D0)) :: y0_a1

      ! Validate inputs
      IF (d <= 0 .OR. C <= 0 .OR. k <= 0 .OR. lambda_c <= 0) THEN
         PRINT *, "Thickness (d), heat capacity (C), conductivity (k), and lambda_c must be positive."
         STOP
      END IF
      IF (WS < 0) THEN
         PRINT *, "Wind speed (WS) cannot be negative."
         STOP
      END IF

      ! Compute thermal admittance
      TA = SQRT(C*k)

      ! Compute required coefficients
      S_a1 = 0.296*LOG(TA) - 0.00027*(QStar/lambda_c)*LOG(TA) - 0.1185*WS - 1.194
      omega_a1 = -14.8*LOG(SQRT(k)/C) + 2.25*WS*LOG(SQRT(k)/C) + 29.69*WS - 190.01
      theta_a1 = 0.0000161*C - 4.481E-06*C*WS - 3.32*k - 0.1056*(QStar/lambda_c) + 10.673
      y0_a1 = 0.01

      ! Compute final a1 value
      a1 = S_a1 + (y0_a1 - S_a1)*EXP(-theta_a1*d)*COS(omega_a1*d)

   END SUBROUTINE calculate_a1

   SUBROUTINE calculate_a2(d, C, k, WS, QStar, lambda_c, a2)
      IMPLICIT NONE
      ! Input parameters
      REAL(KIND(1D0)), INTENT(in) :: d ! Thickness [m]
      REAL(KIND(1D0)), INTENT(in) :: C ! Volumetric heat capacity (specific heat * density) [J K-1 m-3]
      REAL(KIND(1D0)), INTENT(in) :: k ! Thermal conductivity [W m-1 K-1]
      REAL(KIND(1D0)), INTENT(in) :: WS ! Wind speed [ms-1]
      REAL(KIND(1D0)), INTENT(in) :: QStar ! Daily mean net all-wave radiation (normalized by building footprint area) [W m-2]
      REAL(KIND(1D0)), INTENT(in) :: lambda_c ! Building surface to plan area ratio [-]

      ! Output parameter
      REAL(KIND(1D0)), INTENT(out) :: a2 ! OHM coefficient a2

      ! Local variables
      REAL(KIND(1D0)) :: TA ! Thermal admittance
      REAL(KIND(1D0)) :: TD ! Thermal diffusivity
      REAL(KIND(1D0)) :: S_a2
      REAL(KIND(1D0)) :: omega_a2
      REAL(KIND(1D0)) :: theta_a2
      REAL(KIND(1D0)) :: y0_a2
      REAL(KIND(1D0)) :: n

      ! Validate inputs
      IF (d <= 0 .OR. C <= 0 .OR. k <= 0 .OR. lambda_c <= 0) THEN
         PRINT *, "Thickness (d), heat capacity (C), conductivity (k), and lambda_c must be positive."
         STOP
      END IF
      IF (WS <= 0) THEN
         PRINT *, "Wind speed (WS) must be positive."
         STOP
      END IF

      ! Compute thermal admittance and diffusivity
      TA = SQRT(C*k)
      TD = k/C

      ! Compute required coefficients
      S_a2 = -7.81E-05*TA + 0.00348*(QStar/lambda_c) + 0.013*WS + 0.123
      omega_a2 = -9.44*LOG(TD) + 1.68*WS - 126
      theta_a2 = 1.05E-05*C - 6.67*k - 0.1203*(QStar/lambda_c) - 3.48*WS + 28
      y0_a2 = 1.29E-07*C + 0.0603*k - 0.000796*(QStar/lambda_c) - 0.146*WS + 0.091
      n = 3.33E+04*TD + 507.28*QStar*TD/lambda_c - 1.54E+04*(TD/WS) + 0.0161

      ! Compute final a2 value
      a2 = S_a2 + ((y0_a2 - S_a2) + n*((theta_a2**2 + omega_a2**2)/omega_a2)*SIN(omega_a2*d))*EXP(-theta_a2*d)

   END SUBROUTINE calculate_a2

   SUBROUTINE calculate_a3(QStar, dTair, a1, lambda_c, a3)
      IMPLICIT NONE
      ! Input parameters
      REAL(KIND(1D0)), INTENT(in) :: QStar ! Daily mean net all-wave radiation normalized by building footprint area (W m-2)
      REAL(KIND(1D0)), INTENT(in) :: dTair ! Mid-night (00:00) 2m-air temperature difference compared to the previous day (°C)
      REAL(KIND(1D0)), INTENT(in) :: a1 ! Coefficient a1 derived from building material and meteorological condition
      REAL(KIND(1D0)), INTENT(in) :: lambda_c ! Building surface to plan area ratio (dimensionless)

      ! Output parameter
      REAL(KIND(1D0)), INTENT(out) :: a3 ! OHM coefficient a3

      ! Local variables
      REAL(KIND(1D0)) :: slope

      ! Compute the slope factor; note: the factor 5.2 is based on the default for a one-building, five-surface model.
      slope = -1 + 5.2*lambda_c*dTair/QStar

      ! Compute final a3 value
      a3 = a1*slope*QStar

   END SUBROUTINE calculate_a3

END MODULE module_phys_ohm

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE OHM_module
   USE module_phys_ohm
END MODULE OHM_module
