MODULE OHM_module
   ! USE allocateArray
   ! USE data_in
   ! USE defaultNotUsed
   ! USE gis_data
   ! USE sues_data
   ! USE time

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
                  DiagQS, &
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

      IMPLICIT NONE
      INTEGER, INTENT(in) :: tstep ! time step [s]
      INTEGER, INTENT(in) :: dt_since_start ! time since simulation starts [s]

      REAL(KIND(1D0)), INTENT(in) :: qn1 ! net all-wave radiation
      REAL(KIND(1D0)), INTENT(in) :: qn1_S ! net all-wave radiation over snow
      REAL(KIND(1D0)), INTENT(in) :: sfr_surf(nsurf) ! surface fractions
      REAL(KIND(1D0)), INTENT(in) :: SnowFrac(nsurf) ! snow fractions of each surface
      REAL(KIND(1D0)), INTENT(in) :: Tair_mav_5d ! Tair_mav_5d=HDD(id-1,4) HDD at the begining of today (id-1)
      REAL(KIND(1D0)), INTENT(in) :: OHM_coef(nsurf + 1, 4, 3) ! OHM coefficients
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

END MODULE OHM_module
