MODULE Snow_module
   USE evap_module, ONLY: cal_evap
   USE allocateArray, ONLY: nsurf, PavSurf, BldgSurf, ConifSurf, BSoilSurf, WaterSurf, ncolumnsDataOutSnow

   IMPLICIT NONE

CONTAINS
   !This subroutine makes snow related calculations at the model time step. Needed for the
   !available energy in LUMPS and SUEWS. Made by LJ in Dec 2012
   !SUBROUTINES:
   !  MeltHeat - Calculation of snow related energy processes
   !  SnowCalc - Calculation of snow and soil storages
   !  Evap_SUEWS_Snow - Calculation of evaporation from the SnowPack
   !  snowRem - Removal of snow my snow clearing
   !  SnowDepletionCurve - Calculation of snow fractions
   !Last modified
   !  TS 22 Sep 2019 - split `SnowUpdate` into `albedo` and `density` parts so they can be better integrated with `SUEWS_cal_Qn` and `Snow_cal_MeltHeat`
   !  TS 17 Sep 2017 - added wrapper `Snow_cal_MeltHeat` for `SUEWS_driver`
   !  TS 04 Sep 2017 - added `veg_fr_snow` to update VegFractions with snow effect included
   !  TS 31 Aug 2017 - fixed the incomplete explicit interfaces
   !  LJ 24 Aug 2017 - added explicit interfaces
   !  LJ 3 May 2016  - Changed so that not all surface water freezes in 5-min timestep.
   !                    Re-organization of the snow routine due to this change
   !                    Calculation of albedo moved from MeltHeat to SnowCalc
   !  LJ 27 Jan 2016  - Tabs removed, cleaning of the code
   !  HCW 08 Dec 2015 - Added check for no Paved surfaces
   !  LJ 14 July 2015 - Code fixed to work with tstep.
   !  HCW 06 Mar 2015 - Unused variable 'i' removed.
   !  LJ Jan 2015     - Change the calculation from hourly timestep to timestep defined by nsh
   !  LJ May 2013     - Calculation of the energy balance for the SnowPack was modified
   !                        to use qn1_ind_snow(StoreDrainPrm)
   !=======================================================================================
   SUBROUTINE Snow_cal_MeltHeat( &
      tstep, tau_r, SnowDensMax, & !input
      lvS_J_kg, lv_J_kg, RadMeltFact, TempMeltFact, SnowAlbMax, &
      SnowDensMin, Temp_C, Precip, PrecipLimit, PrecipLimitAlb, &
      nsh_real, sfr_surf, Tsurf_ind, Tsurf_ind_snow, state_surf, qn1_ind_snow, &
      kup_ind_snow, SnowWater, deltaQi, albedo_snow, &
      SnowPack_in, SnowFrac_in, SnowAlb_in, SnowDens_in, SnowfallCum_in, & !input
      SnowPack_out, SnowFrac_out, SnowAlb_out, SnowDens_out, SnowfallCum_out, & !output
      mwh, Qm, QmFreez, QmRain, & ! output
      snowCalcSwitch, Qm_melt, Qm_freezState, Qm_rain, FreezMelt, &
      FreezState, FreezStateVol, rainOnSnow, SnowDepth, mw_ind, &
      dataOutLineSnow) !output

      IMPLICIT NONE
      ! INTEGER, PARAMETER::nsurf = 7
      ! INTEGER, PARAMETER::PavSurf = 1
      ! INTEGER, PARAMETER::BldgSurf = 2
      ! INTEGER, PARAMETER::WaterSurf = 7
      INTEGER, PARAMETER :: ncolumnsDataOutSnow_notime = ncolumnsDataOutSnow - 5
      REAL(KIND(1D0)), PARAMETER :: waterDens = 999.8395 !Density of water in 0 cel deg

      !These are input to the module
      ! INTEGER, INTENT(in) :: SnowUse
      INTEGER, INTENT(in) :: tstep
      ! INTEGER,INTENT(in)::bldgsurf
      ! INTEGER,INTENT(in)::nsurf
      ! INTEGER,INTENT(in)::PavSurf
      ! INTEGER,INTENT(in)::WaterSurf

      REAL(KIND(1D0)), INTENT(in) :: lvS_J_kg
      REAL(KIND(1D0)), INTENT(in) :: lv_J_kg
      REAL(KIND(1D0)), INTENT(in) :: RadMeltFact
      REAL(KIND(1D0)), INTENT(in) :: TempMeltFact
      REAL(KIND(1D0)), INTENT(in) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMax
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMin
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: Precip
      REAL(KIND(1D0)), INTENT(in) :: PrecipLimit
      REAL(KIND(1D0)), INTENT(in) :: PrecipLimitAlb
      REAL(KIND(1D0)), INTENT(in) :: nsh_real
      REAL(KIND(1D0)), INTENT(in) :: albedo_snow
      REAL(KIND(1D0)), INTENT(in) :: tau_r
      ! REAL(KIND(1d0)),INTENT(in)::waterdens

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn1_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: kup_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowWater
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: deltaQi

      !Input and output as this is updated in this subroutine
      REAL(KIND(1D0)), INTENT(in) :: SnowAlb_in
      REAL(KIND(1D0)), INTENT(in) :: SnowfallCum_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac_in
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_in
      REAL(KIND(1D0)), INTENT(out) :: SnowAlb_out
      REAL(KIND(1D0)), INTENT(out) :: SnowfallCum_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowPack_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowFrac_out
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDens_out

      REAL(KIND(1D0)) :: SnowAlb
      REAL(KIND(1D0)) :: SnowfallCum
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowPack
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowFrac
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDens

      !Output:
      REAL(KIND(1D0)), INTENT(out) :: mwh
      REAL(KIND(1D0)) :: fwh
      REAL(KIND(1D0)), INTENT(out) :: Qm
      REAL(KIND(1D0)), INTENT(out) :: QmFreez
      REAL(KIND(1D0)), INTENT(out) :: QmRain

      ! REAL(KIND(1D0)), INTENT(out) :: veg_fr

      INTEGER, DIMENSION(nsurf), INTENT(out) :: snowCalcSwitch

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Qm_melt
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Qm_freezState
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Qm_rain
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: FreezMelt
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: FreezState
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: FreezStateVol
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rainOnSnow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDepth
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: mw_ind

      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutSnow_notime), INTENT(out) :: dataOutLineSnow

      SnowPack = SnowPack_in
      SnowFrac = SnowFrac_in
      SnowAlb = SnowAlb_in
      SnowDens = SnowDens_in
      SnowfallCum = SnowfallCum_in

      ! IF (SnowUse == 1) THEN
      SnowDens = update_snow_dens( &
                 tstep, SnowFrac, SnowDens, &
                 tau_r, SnowDensMax, SnowDensMin)

      CALL MeltHeat( &
         lvS_J_kg, lv_J_kg, tstep*1D0, RadMeltFact, TempMeltFact, & !input
         SnowAlbMax, SnowDensMin, Temp_C, Precip, PrecipLimit, PrecipLimitAlb, &
         nsh_real, sfr_surf, Tsurf_ind, state_surf, qn1_ind_snow, &
         SnowWater, deltaQi, &
         SnowPack, SnowFrac, SnowAlb, SnowDens, SnowfallCum, & !inout
         mwh, fwh, Qm, QmFreez, QmRain, snowCalcSwitch, & !output
         Qm_melt, Qm_freezState, Qm_rain, FreezMelt, FreezState, FreezStateVol, &
         rainOnSnow, SnowDepth, mw_ind)

      ! ELSE ! no snow calculation
      !    mwh = 0
      !    fwh = 0
      !    Qm = 0
      !    QmFreez = 0
      !    QmRain = 0
      !    SnowfallCum = 0
      !    snowCalcSwitch = 0
      !    Qm_melt = 0
      !    Qm_freezState = 0
      !    Qm_rain = 0
      !    FreezMelt = 0
      !    FreezState = 0
      !    FreezStateVol = 0
      !    rainOnSnow = 0
      !    SnowDepth = 0
      !    mw_ind = 0
      !    SnowFrac = 0

      ! END IF

      ! ! update veg_fr
      ! CALL veg_fr_snow( &
      !    sfr_surf, SnowFrac, & !input
      !    veg_fr) !output

      SnowPack_out = SnowPack
      SnowFrac_out = SnowFrac
      SnowAlb_out = SnowAlb
      SnowfallCum_out = SnowfallCum
      SnowDens_out = SnowDens

      ! pack output into one line
      dataOutLineSnow = [ &
                        SnowPack_out(1:nsurf), mw_ind(1:nsurf), Qm_melt(1:nsurf), & !26
                        Qm_rain(1:nsurf), Qm_freezState(1:nsurf), SnowFrac_out(1:(nsurf - 1)), & !46
                        rainOnSnow(1:nsurf), & !53
                        qn1_ind_snow(1:nsurf), kup_ind_snow(1:nsurf), freezMelt(1:nsurf), & !74
                        SnowWater(1:nsurf), SnowDens_out(1:nsurf), & !88
                        snowDepth(1:nsurf), Tsurf_ind_snow(1:nsurf), &
                        albedo_snow]
      ! dataOutLineSnow=set_nan(dataOutLineSnow)

   END SUBROUTINE Snow_cal_MeltHeat

   SUBROUTINE MeltHeat( &
      lvS_J_kg, & !input
      lv_J_kg, &
      tstep_real, &
      RadMeltFact, &
      TempMeltFact, &
      SnowAlbMax, &
      SnowDensMin, &
      Temp_C, &
      Precip, &
      PrecipLimit, &
      PrecipLimitAlb, &
      nsh_real, &
      sfr_surf, &
      Tsurf_ind, &
      state_id, &
      qn1_ind_snow, &
      SnowWater, &
      deltaQi, &
      SnowPack, & !inoout
      SnowFrac, &
      SnowAlb, &
      SnowDens, &
      SnowfallCum, &
      mwh, & !output
      fwh, &
      Qm, &
      QmFreez, &
      QmRain, &
      snowCalcSwitch, &
      Qm_melt, &
      Qm_freezState, &
      Qm_rain, &
      FreezMelt, &
      FreezState, &
      FreezStateVol, &
      rainOnSnow, &
      SnowDepth, &
      mw_ind)

      IMPLICIT NONE

      !These are input to the module
      ! INTEGER, INTENT(in)::bldgsurf
      ! INTEGER, INTENT(in)::nsurf
      ! INTEGER, INTENT(in)::PavSurf
      ! INTEGER, INTENT(in)::WaterSurf
      REAL(KIND(1D0)), PARAMETER :: waterDens = 999.8395 !Density of water in 0 cel deg

      REAL(KIND(1D0)), INTENT(in) :: lvS_J_kg
      REAL(KIND(1D0)), INTENT(in) :: lv_J_kg
      REAL(KIND(1D0)), INTENT(in) :: tstep_real
      REAL(KIND(1D0)), INTENT(in) :: RadMeltFact
      REAL(KIND(1D0)), INTENT(in) :: TempMeltFact
      REAL(KIND(1D0)), INTENT(in) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMin
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: Precip
      REAL(KIND(1D0)), INTENT(in) :: PrecipLimit
      REAL(KIND(1D0)), INTENT(in) :: PrecipLimitAlb
      REAL(KIND(1D0)), INTENT(in) :: nsh_real

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_id
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: qn1_ind_snow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowWater
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: deltaQi

      !Input and output as this is updated in this subroutine
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: SnowPack
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: SnowFrac
      REAL(KIND(1D0)), INTENT(inout) :: SnowAlb
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: SnowDens
      REAL(KIND(1D0)), INTENT(inout) :: SnowfallCum

      !Output:
      REAL(KIND(1D0)), INTENT(out) :: mwh
      REAL(KIND(1D0)), INTENT(out) :: fwh
      REAL(KIND(1D0)), INTENT(out) :: Qm
      REAL(KIND(1D0)), INTENT(out) :: QmFreez
      REAL(KIND(1D0)), INTENT(out) :: QmRain

      !Output, dimension nsurf
      INTEGER, DIMENSION(nsurf), INTENT(out) :: snowCalcSwitch

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Qm_melt
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Qm_freezState
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: Qm_rain
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: FreezMelt
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: FreezState
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: FreezStateVol
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rainOnSnow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDepth
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: mw_ind

      ! local variables:
      REAL(KIND(1D0)) :: AdjMeltFact
      REAL(KIND(1D0)) :: Watfreeze

      REAL(KIND(1D0)), PARAMETER :: cw = 4190 !,ci=2090   !Specific heat capacity of water

      INTEGER :: is, xx

      !Initialize snow variables
      mwh = 0 !Initialize snow melt and heat related to snowmelt
      fwh = 0
      Qm = 0
      QmFreez = 0
      QmRain = 0
      snowCalcSwitch = 0
      Qm_melt = 0
      Qm_freezState = 0
      Qm_rain = 0
      FreezMelt = 0
      FreezState = 0
      FreezStateVol = 0
      rainOnSnow = 0
      SnowDepth = 0
      mw_ind = 0

      !===dummy calculations===
      ! xx = bldgsurf
      ! xx = PavSurf
      !===dummy calculations end===

      !=========================================================================================
      DO is = 1, nsurf !Go each surface type through
         IF (sfr_surf(is) /= 0) THEN !If surface type existing,

            IF (SnowPack(is) > 0) THEN !If SnowPack existing, calculate meltwater related water flows

               SnowDepth(is) = (SnowPack(is)/1000)*waterDens/SnowDens(is) !Snow depth in m

               !Calculate meltwater related water flows with hourly degree-day method.

               !These are for snow melting
               IF (Temp_C >= 0) THEN
                  IF (qn1_ind_snow(is) < 0) THEN
                     mw_ind(is) = TempMeltFact*Temp_C !(mm C−1 h−1)*(C) = in mm h-1
                  ELSE
                     mw_ind(is) = RadMeltFact*(qn1_ind_snow(is)) !(mm m2 W−1 h−1)*(W m-2)= mm h-1 ??
                  END IF

               ELSE !Freezing equations
                  AdjMeltFact = 1 !Relationship between the temperature melt and freezing factors
                  mw_ind(is) = TempMeltFact*Temp_C*AdjMeltFact ! in mm h-1
               END IF
               !Previous equation give the hourly values, divide these with the timestep number
               mw_ind(is) = mw_ind(is)/nsh_real

               IF (mw_ind(is) > SnowPack(is)) mw_ind(is) = SnowPack(is) !Limited by the previous timestep SnowPack

               !-----------------------------------------------------
               ! Heat consumed to snowmelt/refreezing within Tstep.
               ! Converted from mm nsh-1 to mm nsh-1 and to m s-1
               Qm_melt(is) = waterDens*((mw_ind(is)/tstep_real)/1000)*(lvS_J_kg - lv_J_kg)

               !If melt is negative this means freezing water in the SnowPack
               IF (mw_ind(is) < 0) THEN

                  FreezMelt(is) = -mw_ind(is) !Save this to variable FreezMelt
                  mw_ind(is) = 0

                  !Freezing water cannot exceed meltwater store
                  IF (FreezMelt(is) > SnowWater(is)) FreezMelt(is) = SnowWater(is)

                  !Recalculate melt related energy
                  Qm_melt(is) = waterDens*((-FreezMelt(is)/tstep_real)/1000)*(lvS_J_kg - lv_J_kg)
               END IF

               !-----------------------------------------------------
               ! If air temperature is above zero, precipitation causes advective heat to the
               ! SnowPack. Eq (23) in Sun et al., 1999
               ! Calculation done at resolution of the model timestep
               IF (Temp_C >= PrecipLimit .AND. Precip > 0) THEN
                  Qm_rain(is) = waterDens*cw*(Temp_C - PrecipLimit)*(Precip*0.001/tstep_real) !in W m-2
                  IF (Qm_rain(is) < 0) THEN !Can only be positive
                     Qm_rain(is) = 0
                  ELSE
                     rainOnSnow(is) = Precip !Save information on the rain on snow event
                  END IF
               END IF

            END IF !End if SnowPack

            !=================================================================

            !Freeze surface water state_id if cold enough.
            IF (Tsurf_ind(is) < 0 .AND. state_id(is) > 0) THEN

               snowCalcSwitch(is) = 1 !If water on ground this forms ice and snow calculations are made

               !Other surfaces than water treated first
               IF (is /= WaterSurf) THEN

                  !FreezState(is) = state_id(is)
                  !Previously all state_id could freeze in 5-min timestep. Now we calculate how much water
                  !can freeze in a timestep based on the same temperature freezing fraction.
                  FreezState(is) = -TempMeltFact*Tsurf_ind(is)/nsh_real

                  !The amount of freezing water cannot be greater than the surface state_id
                  IF (FreezState(is) > state_id(is)) FreezState(is) = state_id(is)

                  IF (SnowPack(is) == 0 .OR. SnowFrac(is) == 0) THEN !SnowPack forms
                     FreezStateVol(is) = FreezState(is)
                  ELSE !There is snow already on ground
                     FreezStateVol(is) = FreezState(is)*(1 - SnowFrac(is))/SnowFrac(is)
                  END IF

                  ! If the amount of freezing water is very small and there is state_id left to the ground
                  ! no freezing of water will take place
                  IF (FreezStateVol(is) < 0.00000000001 .AND. FreezState(is) < state_id(is)) THEN
                     FreezState(is) = 0
                     FreezStateVol(is) = 0
                  END IF

                  !Calculate the heat exchange in W m-2
                  Qm_freezState(is) = -waterDens*(FreezState(is)/tstep_real/1000)*(lvS_J_kg - lv_J_kg)

                  !Water surface separately
               ELSE
                  !Calculate average value how much water can freeze above the water areas
                  !Equation is -hA(T-T0) = rhoV(Cp+dT +Lf) in 5-min timestep
                  !h=convective heat trasnfer,A, area of water,rwo water density,V volume, dT temperature difference
                  !before and end of the 5-min period. dT equals zero, h=100 and when multiplied with Area, the equation
                  !simplyfies to the this. LJ 14 July 2015
                  Watfreeze = 100*(0 - Temp_C)/(waterDens*(lvS_J_kg - lv_J_kg))
                  FreezState(is) = Watfreeze
                  Qm_freezState(is) = -waterDens*(Watfreeze/tstep_real/1000)*(lvS_J_kg - lv_J_kg)
               END IF

            END IF

            !======================================================================
            ! Define if any snowmelt calculations are made: SnowPack existing,
            ! freezing occuring on ground or from precip
            IF (is /= WaterSurf) THEN
               IF (SnowPack(is) > 0 .OR. (Precip > 0 .AND. Tsurf_ind(is) < 0)) THEN
                  snowCalcSwitch(is) = 1
               END IF
            ELSE !Water surface separately
               IF (SnowPack(WaterSurf) > 0 .OR. FreezState(WaterSurf) > 0) THEN
                  snowCalcSwitch(WaterSurf) = 1
               END IF
            END IF

            !Update snow density of each surface
            IF (Precip > 0 .AND. Tsurf_ind(is) < 0 .AND. SnowPack(is) > 0) THEN
               SnowDens(is) = SnowDens(is)*SnowPack(is)/(SnowPack(is) + Precip) + SnowDensMin*Precip/(SnowPack(is) + Precip)
            END IF

            !Weighted variables for the whole area
            mwh = mwh + mw_ind(is)*sfr_surf(is)*SnowFrac(is) !Snowmelt
            fwh = fwh + FreezMelt(is)*sfr_surf(is)*SnowFrac(is) !Freezing water
            Qm = Qm + Qm_melt(is)*sfr_surf(is)*SnowFrac(is) !Energy consumed to the melt/freezing.
            QmRain = QmRain + Qm_rain(is)*sfr_surf(is)*SnowFrac(is) !Rain on snow
            QmFreez = QmFreez + deltaQi(is)*sfr_surf(is)*SnowFrac(is) + Qm_freezState(is)*sfr_surf(is)*(1 - SnowFrac(is)) !Freezing water
         END IF

      END DO !End surface type

      !Update snow albedo to its maximum value if precipitation exists
      IF (Precip > 0 .AND. SUM(SnowPack) > 0 .AND. Temp_C < 0) THEN

         SnowfallCum = SnowfallCum + Precip

         IF (SnowfallCum > PrecipLimitAlb) THEN

            SnowAlb = SnowAlbMax
            SnowfallCum = 0
         END IF
      ELSE

         SnowfallCum = 0
      END IF

   END SUBROUTINE MeltHeat

   !===============================================================================================
   !===============================================================================================
   SUBROUTINE SnowCalc( &
      tstep, imin, it, dectime, is, & !input
      snowCalcSwitch, & !input
      EvapMethod, CRWmin, CRWmax, nsh_real, lvS_J_kg, avdens, &
      avRh, Press_hPa, Temp_C, RAsnow, psyc_hPa, avcp, sIce_hPa, &
      PervFraction, vegfraction, addimpervious, &
      vpd_hPa, qn_e, s_hPa, ResistSurf, RA, rb, tlv, snowdensmin, SnowProf_24hr, precip, &
      PipeCapacity, RunoffToWater, &
      addVeg, SnowLimPaved, SnowLimBldg, FlowChange, drain, &
      WetThresh, stateOld, mw_ind, SoilStoreCap, rainonsnow, &
      freezmelt, freezstate, freezstatevol, &
      Qm_Melt, Qm_rain, Tsurf_ind, sfr_surf, dayofWeek_id, StoreDrainPrm, SnowPackLimit, &
      AddWater, addwaterrunoff, &
      soilstore_id, SnowPack, SurplusEvap, & !inout
      SnowFrac, SnowWater, iceFrac, SnowDens, &
      runoffAGimpervious, runoffAGveg, surplusWaterBody, &
      ev_tot, qe_tot, runoff_tot, surf_chang_tot, chSnow_tot, & ! output
      rss_surf, & ! output
      runoff_snowfree, chang, changSnow, SnowToSurf, state_id, ev_snow, &
      SnowRemoval, swe, &
      runoffPipes, mwstore, runoffwaterbody)

      !Calculation of snow and water balance on 5 min timestep. Treats snowfree and snow covered
      !areas separately. Weighting is taken into account in the overall values.
      !Last modified:
      !  LJ in 6 May 2015 - Modified to run with timestep
      !  HCW 06 Mar 2015 - Unused variable 'i' removed.
      !  HCW 26 Jan 2015 - Added weekday/weekend option for snow clearing profiles
      !  LJ in 24 May 2013
      !========================================================================
      USE WaterDist_module, ONLY: updateFlood

      IMPLICIT NONE
      ! INTEGER, PARAMETER::nsurf = 7! number of surface types
      ! INTEGER, PARAMETER::PavSurf = 1  !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER, PARAMETER::BldgSurf = 2  !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER, PARAMETER::ConifSurf = 3  !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER,PARAMETER::DecidSurf = 4  !New surface classes: Grass = 5th/7 surfaces
      ! INTEGER,PARAMETER::GrassSurf = 5
      ! INTEGER, PARAMETER::BSoilSurf = 6!New surface classes: Grass = 5th/7 surfaces
      ! INTEGER, PARAMETER::WaterSurf = 7

      INTEGER, PARAMETER :: snowfractionchoice = 2 ! this PARAMETER is used all through the model
      REAL(KIND(1D0)), PARAMETER :: waterDens = 999.8395 !Density of water in 0 cel deg

      ! INTEGER,INTENT(in)::id
      ! INTEGER,INTENT(in)::nsurf
      INTEGER, INTENT(in) :: tstep
      INTEGER, INTENT(in) :: imin
      INTEGER, INTENT(in) :: it
      INTEGER, INTENT(in) :: is

      INTEGER, DIMENSION(nsurf), INTENT(in) :: snowCalcSwitch

      ! INTEGER,INTENT(in)::ConifSurf
      ! INTEGER,INTENT(in)::BSoilSurf
      ! INTEGER,INTENT(in)::BldgSurf
      ! INTEGER,INTENT(in)::PavSurf
      ! INTEGER,INTENT(in)::WaterSurf
      INTEGER, INTENT(in) :: EvapMethod !Evaporation calculated according to Rutter (1) or Shuttleworth (2)
      INTEGER, DIMENSION(3), INTENT(in) :: DayofWeek_id

      REAL(KIND(1D0)), INTENT(in) :: dectime
      REAL(KIND(1D0)), INTENT(in) :: CRWmin
      REAL(KIND(1D0)), INTENT(in) :: CRWmax
      REAL(KIND(1D0)), INTENT(in) :: nsh_real
      REAL(KIND(1D0)), INTENT(in) :: lvS_J_kg
      ! REAL(KIND(1d0)), INTENT(in)::lv_j_kg
      REAL(KIND(1D0)), INTENT(in) :: avdens
      REAL(KIND(1D0)), INTENT(in) :: vpd_hPa ! vapour pressure deficit [hPa]
      REAL(KIND(1D0)), INTENT(in) :: qn_e !net available energy for evaporation
      REAL(KIND(1D0)), INTENT(in) :: avRh
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: RAsnow
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa
      REAL(KIND(1D0)), INTENT(in) :: avcp
      REAL(KIND(1D0)), INTENT(in) :: sIce_hPa
      REAL(KIND(1D0)), INTENT(in) :: PervFraction
      REAL(KIND(1D0)), INTENT(in) :: vegfraction
      REAL(KIND(1D0)), INTENT(in) :: addimpervious
      ! REAL(KIND(1d0)),INTENT(in)::numPM
      REAL(KIND(1D0)), INTENT(in) :: s_hPa
      REAL(KIND(1D0)), INTENT(in) :: ResistSurf
      ! REAL(KIND(1d0)),INTENT(in)::sp
      REAL(KIND(1D0)), INTENT(in) :: RA
      REAL(KIND(1D0)), INTENT(in) :: rb
      REAL(KIND(1D0)), INTENT(in) :: tlv
      REAL(KIND(1D0)), INTENT(in) :: snowdensmin
      REAL(KIND(1D0)), INTENT(in) :: precip
      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater
      REAL(KIND(1D0)), INTENT(in) :: addVeg
      REAL(KIND(1D0)), INTENT(in) :: SnowLimPaved
      REAL(KIND(1D0)), INTENT(in) :: SnowLimBldg
      REAL(KIND(1D0)), INTENT(in) :: FlowChange

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WetThresh
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: stateOld
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: mw_ind
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: rainonsnow
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: freezmelt
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: freezstate
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: freezstatevol
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Qm_Melt
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Qm_rain
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: Tsurf_ind
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPackLimit
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: addwaterrunoff
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(in) :: StoreDrainPrm
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: SnowProf_24hr

      !Updated status: input and output
      REAL(KIND(1D0)), INTENT(inout) :: runoffAGveg
      REAL(KIND(1D0)), INTENT(inout) :: runoffAGimpervious
      REAL(KIND(1D0)), INTENT(inout) :: surplusWaterBody

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: soilstore_id
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: SnowPack
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: SnowFrac
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: SnowWater
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: iceFrac
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: SnowDens
      REAL(KIND(1D0)), DIMENSION(2), INTENT(inout) :: SurplusEvap

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: rss_surf
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoffSnow_surf !Initialize for runoff caused by snowmelting
      REAL(KIND(1D0)), DIMENSION(nsurf) :: runoff_snowfree
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: runoffSoil
      REAL(KIND(1D0)), DIMENSION(nsurf) :: chang
      REAL(KIND(1D0)), DIMENSION(nsurf) :: changSnow
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowToSurf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_id
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDepth
      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_snow
      REAL(KIND(1D0)), DIMENSION(2), INTENT(out) :: SnowRemoval

      REAL(KIND(1D0)), INTENT(out) :: swe
      REAL(KIND(1D0)) :: ev_snowfree
      REAL(KIND(1D0)), INTENT(out) :: ev_tot
      REAL(KIND(1D0)), INTENT(out) :: chSnow_tot
      REAL(KIND(1D0)), INTENT(out) :: qe_tot
      REAL(KIND(1D0)), INTENT(out) :: runoff_tot
      REAL(KIND(1D0)), INTENT(out) :: surf_chang_tot
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes
      REAL(KIND(1D0)), INTENT(out) :: mwstore
      REAL(KIND(1D0)), INTENT(out) :: runoffwaterbody

      REAL(KIND(1D0)) :: qe

      ! REAL(KIND(1d0))::Evap_SUEWS_Snow
      REAL(KIND(1D0)) :: MeltExcess !Excess melt water that needs to leave SnowPack
      REAL(KIND(1D0)) :: snowTotInit
      REAL(KIND(1D0)) :: EvPart
      REAL(KIND(1D0)) :: runoffTest
      REAL(KIND(1D0)) :: snowFracFresh1 !Snow fraction for newly formed SnowPack
      REAL(KIND(1D0)) :: snowFracFresh2 !Snow fraction for newly formed SnowPack from state_id only
      REAL(KIND(1D0)) :: snowFracOld
      REAL(KIND(1D0)) :: WaterHoldCapFrac
      REAL(KIND(1D0)) :: FWC !Water holding capacity of snow in mm
      REAL(KIND(1D0)) :: tlv_sub
      ! REAL(KIND(1d0)):: SnowDepletionCurve

      INTEGER :: iu !1=weekday OR 2=weekend
      REAL(KIND(1D0)), PARAMETER :: IPThreshold_mmhr = 10 !Threshold for intense precipitation [mm hr-1]

      REAL(KIND(1D0)), DIMENSION(7) :: capStore ! current storage capacity [mm]

      !========================================================================
      !Initialize variables for the calculation of water storages and evaporation
      ev_tot = 0
      qe_tot = 0
      runoff_tot = 0
      surf_chang_tot = 0

      !swe = 0
      chSnow_tot = 0
      !runoffPipes = 0
      !mwstore = 0
      !runoffwaterbody = 0
      rss_surf = 0
      state_id = stateOld
      SnowDepth = 0
      ev_snow = 0
      !SnowRemoval = 0
      tlv_sub = 0

      ! Use weekday or weekend snow clearing profile
      iu = 1 !Set to 1=weekday
      IF (DayofWeek_id(1) == 1 .OR. DayofWeek_id(1) == 7) iu = 2 !Set to 2=weekend

      !write(*,*) is
      runoffSnow_surf(is) = 0 !Initialize for runoff caused by snowmelting
      runoff_snowfree(is) = 0
      ! runoffSoil(is) = 0
      chang(is) = 0
      changSnow(is) = 0
      runoffTest = 0
      SnowToSurf(is) = 0
      EvPart = 0
      ev_snowfree = 0
      snowFracFresh1 = 0
      snowFracFresh2 = 0
      snowFracOld = 0

      !Initial SnowPack + meltwater in it
      snowTotInit = SnowPack(is) + SnowWater(is)

      !Calculate water holding capacity (Jin et al. 1999)
      IF (SnowDens(is) >= 200) THEN
         WaterHoldCapFrac = CRWmin
      ELSE
         WaterHoldCapFrac = CRWmin + (CRWmax - CRWmin)*(200 - SnowDens(is))/200
      END IF

      !======================================================================
      ! Calculate evaporation from SnowPack and snow free surfaces (in mm)
      ! IF (SnowFrac(is)<1) CALL Evap_SUEWS !ev and qe for snow free surface out
      capStore(is) = StoreDrainPrm(6, is)
      IF (SnowFrac(is) < 1) CALL cal_evap( &
         EvapMethod, state_id(is), WetThresh(is), capStore(is), & !input
         vpd_hPa, avdens, avcp, qn_e, s_hPa, psyc_hPa, ResistSurf, RA, rb, tlv, &
         rss_surf(is), ev_snowfree, qe) !output

      IF (SnowFrac(is) > 0 .AND. snowCalcSwitch(is) > 0) THEN
         CALL Evap_SUEWS_Snow(Qm_Melt(is), Qm_rain(is), lvS_J_kg, avdens, avRh, Press_hPa, Temp_C, RAsnow, &
                              psyc_hPa, tstep, avcp, sIce_hPa, dectime, ev_snow(is), tlv_sub)
      END IF

      !If not enough water for evaporation in impervious surfaces,
      !evaporation is taken from pervious surfaces
      IF (is > 2) THEN
         IF (PervFraction /= 0) THEN
            EvPart = (SurplusEvap(PavSurf)*sfr_surf(PavSurf) + SurplusEvap(BldgSurf)*sfr_surf(BldgSurf))/PervFraction
         END IF
      END IF

      !============================================================================
      !Water surface is treated separately
      IF (is == WaterSurf .AND. sfr_surf(WaterSurf) > 0) GO TO 606

      !The calculations are divided into 2 main parts
      ! 1) Surface is fully covered with snow at the beginning of the time step
      ! 2) Surface is not fully covered with snow but rather part is snow free OR
      !    surface not orginally covered with snow, but the snow forms at the current timestep

      !1)------------------------------------------------------------------
      !  ------------------------------------------------------------------
      IF (snowCalcSwitch(is) > 0) THEN
         IF (SnowPack(is) > 0 .AND. SnowFrac(is) == 1) THEN

            ev_snow(is) = ev_snow(is) + EvPart !Evaporation surplus

            !(Snowfall per interval+freezing of melt water and surface state_id) - (meltwater+evaporation from SnowPack)
            changSnow(is) = (Precip + freezMelt(is)) - (mw_ind(is) + ev_snow(is)) !Calculate change in SnowPack (in mm)

            !If rain on snow event, add this water to SnowWater
            IF (rainOnSnow(is) > 0) THEN
               changSnow(is) = changSnow(is) - Precip
               SnowWater(is) = SnowWater(is) + rainOnSnow(is)
            END IF

            SnowPack(is) = SnowPack(is) + changSnow(is) !Update SnowPack

            !---------If SnowPack exists after the state_id calculations
            IF (SnowPack(is) > 0) THEN

               !Add melted water to meltstore and freeze water according to freezMelt(is)
               SnowWater(is) = SnowWater(is) + mw_ind(is) - freezMelt(is)

               !Calculate water holding capacity (FWC: Valeo and Ho, 2004) of the SnowPack
               FWC = WaterHoldCapFrac*SnowPack(is)

               !If FWC is exceeded, excess meltwater (MeltExcess) will leave from the SnowPack
               IF (SnowWater(is) >= FWC) THEN
                  MeltExcess = 0 !Initialize the excess meltwater
                  MeltExcess = SnowWater(is) - FWC !Calculate the exceess water
                  SnowWater(is) = FWC !Update the SnowWater to the maximum it can hold
                  runoffSnow_surf(is) = runoffSnow_surf(is) + MeltExcess
               END IF

               !At the end of the hour calculate possible snow removal
               IF (SnowProf_24hr(it, iu) == 1 .AND. is < 3 .AND. (imin == (nsh_real - 1)/nsh_real*60)) &
                  CALL snow_removal( &
                  is, &
                  SnowFrac, sfr_surf, &
                  SnowPack, SnowRemoval, &
                  SnowLimPaved, SnowLimBldg)
               !----------If SnowPack is negative, it melts at this timestep
            ELSEIF (SnowPack(is) < 0) THEN

               !If freezing meltwater inside this timestep, remove it from the SnowWater
               SnowWater(is) = SnowWater(is) - freezMelt(is) + mw_ind(is) + SnowPack(is)
               SnowPack(is) = 0.0 !Set the snow pack and snow
               snowFracOld = 1
               SnowFrac(is) = 0
               snowDens(is) = 0

               IF (SnowWater(is) < 0) THEN !Not enough water in the meltwater store,
                  ev_snow(is) = ev_snow(is) + SnowWater(is) !QUESTION: evaporation from snow is decreased?
                  IF (ev_snow(is) < 0) ev_snow(is) = 0
                  changSnow(is) = changSnow(is) + SnowWater(is)
                  SnowWater(is) = 0
               ELSE
                  chang(is) = SnowWater(is) !Meltwater goes to surface state_id as no snow exists anymore
                  state_id(is) = state_id(is) + chang(is)
                  SnowWater(is) = 0
               END IF
            END IF !SnowPack negative or positive

            !2)------Surface not fully covered with snow-------------------------------------------
            !  ------------------------------------------------------------------------------------
         ELSEIF (SnowFrac(is) < 1) THEN

            !Snow calculations: SnowPack can either exist or form at the current timestep
            IF (SnowPack(is) > 0) THEN
               ev_snow(is) = ev_snow(is) + EvPart !Evaporation surplus

               !----SnowPack water balance for the whole surface area. In reality snow depth = SnowPack/SnowFrac(is)
               !(Snowfall per interval+freezing of melt water and surface state_id) - (meltwater+evaporation from SnowPack)
               changSnow(is) = (Precip + freezMelt(is) + freezStateVol(is)) - (mw_ind(is) + ev_snow(is)) !Calculate change in SnowPack (in mm)

               !If rain on snow event, add this water to SnowWater
               IF (rainOnSnow(is) > 0) THEN
                  changSnow(is) = changSnow(is) - Precip
                  SnowWater(is) = SnowWater(is) + rainOnSnow(is)
               END IF
               SnowPack(is) = SnowPack(is) + changSnow(is)

               !The fraction of snow will update when:
               !a) Surface state_id is dry but precipitation occurs =1
               !b) There is both precipitation and all surface state_id freezes =1
               !c) No precipitation but all state_id freezes at a single timestep =2
               !d) Part of the surface freezes
               IF (Precip > 0 .AND. FreezState(is) == state_id(is)) THEN !both a) and b)
                  snowFracFresh1 = 1
               ELSEIF (Precip == 0 .AND. FreezState(is) > 0 .AND. FreezState(is) == state_id(is)) THEN
                  snowFracFresh1 = 1

                  !snowFracFresh1=SnowDepletionCurve(is,SnowPack(is),SnowPackLimit(is))
                  !if (snowFracFresh1<0.001) snowFracFresh1=0.001
               ELSEIF (FreezState(is) > 0 .AND. FreezState(is) < state_id(is)) THEN !This if not all water freezes
                  snowFracFresh1 = 0.95 !Now this fraction set to something close to one. Should be improved in the future at some point
                  !if (is==1)then
                  ! write(*,*) id,it,imin,SnowFrac(is),FreezState(is),state_id(is)
                  ! pause
                  !endif
               END IF

               !SnowPack can also form at the current timestep (2). If this forms purely from snowfall or/and all water at surface freezes,
               !the whole surface will be covered with snow. If there is water on ground this snowfall can immediately melt
               !and in this case the snow fraction is not necessarily 1 but its information is saved to snowFracFresh that
               !is taken into account in snow fraction after calculation of state_id.
            ELSEIF (SnowPack(is) == 0 .AND. Tsurf_ind(is) < 0) THEN

               !The fraction of snow will get a value of 1 (ie full snow cover):
               !Surface state_id is dry but precipitation occurs, no precipitation but all state_id freezes at a single timestep,
               !There is both precipitation and all surface state_id freezes
               IF ((Precip > 0 .AND. state_id(is) == 0) .OR. (Precip == 0 .AND. FreezState(is) == state_id(is)) .OR. &
                   (Precip > 0 .AND. FreezState(is) == state_id(is))) THEN

                  !ev=ev+EvPart
                  changSnow(is) = Precip + FreezStateVol(is)
                  SnowPack(is) = SnowPack(is) + changSnow(is) !Update SnowPack

                  snowFracFresh1 = 1
                  iceFrac(is) = FreezState(is)/(FreezState(is) + Precip)
                  SnowDens(is) = SnowDensMin
               END IF

               IF (FreezState(is) > 0 .AND. FreezState(is) < state_id(is)) THEN

                  changSnow(is) = Precip + freezStateVol(is)
                  SnowPack(is) = SnowPack(is) + changSnow(is) !Update SnowPack
                  snowFracFresh2 = 0.95 !Now this fraction set to something close to one. Should be improved in the future at some point

                  !snowFracFresh2=SnowDepletionCurve(is,SnowPack(is),SnowPackLimit(is))
                  !if (snowFracFresh2<0.001) snowFracFresh2=0.001
                  iceFrac(is) = 1
                  SnowDens(is) = SnowDensMin
                  !write(*,*) 2,is,id,it,imin,SnowFrac(is),FreezState(is),state_id(is),state_id(is)+Precip
                  !pause

               END IF
            END IF

            !---------If SnowPack exists after the state_id calculations
            IF (SnowPack(is) > 0) THEN

               !Add melted water to meltstore and freeze water according to freezMelt(is)
               SnowWater(is) = SnowWater(is) + mw_ind(is) - freezMelt(is)

               !Calculate water holding capacity (FWC: Valeo and Ho, 2004) of the SnowPack
               FWC = WaterHoldCapFrac*SnowPack(is)

               !If FWC is exceeded, excess meltwater (MeltExcess) will leave from the SnowPack
               IF (SnowWater(is) >= FWC) THEN
                  MeltExcess = 0 !Initialize the excess meltwater
                  MeltExcess = SnowWater(is) - FWC !Calculate the exceess water
                  SnowWater(is) = FWC !Update the SnowWater to the maximum it can hold

                  !If the fraction of snow is greater than 0.8 or if the surface is is buildings,
                  !the excess water will directly go to runoff. Otherwise it will flow to the
                  !snow free area via SnowToSurf(is)
                  IF ((SnowFrac(is) > 0.9 .AND. is /= BldgSurf) .OR. (is == BldgSurf)) THEN
                     runoffSnow_surf(is) = runoffSnow_surf(is) + MeltExcess
                  ELSE
                     SnowToSurf(is) = SnowToSurf(is) + MeltExcess*SnowFrac(is)/(1 - SnowFrac(is))
                  END IF
               END IF

               !At the end of the hour calculate possible snow removal
               IF (SnowProf_24hr(it, iu) == 1 .AND. is < 3 .AND. (imin == (nsh_real - 1)/nsh_real*60)) &
                  CALL snow_removal( &
                  is, &
                  SnowFrac, sfr_surf, &
                  SnowPack, SnowRemoval, &
                  SnowLimPaved, SnowLimBldg)

               !----------If SnowPack is negative, it melts at this timestep
            ELSEIF (SnowPack(is) < 0) THEN

               !If freezing meltwater inside this timestep, remove it from the SnowWater
               SnowWater(is) = SnowWater(is) - freezMelt(is) + mw_ind(is) + SnowPack(is)

               SnowPack(is) = 0.0 !Set the snow pack and snow
               snowFracFresh1 = 0
               snowFracFresh2 = 0
               snowDens(is) = 0

               IF (SnowWater(is) < 0) THEN !Not enough water in the meltwater store,
                  ev_snow(is) = ev_snow(is) + SnowWater(is) !QUESTION: evaporation from snow is decreased.?
                  IF (ev_snow(is) < 0) ev_snow(is) = 0
                  changSnow(is) = changSnow(is) + SnowWater(is)
                  SnowWater(is) = 0
               ELSE
                  SnowToSurf(is) = SnowToSurf(is) + SnowWater(is)*SnowFrac(is)/(1 - SnowFrac(is))
                  SnowWater(is) = 0
               END IF
            END IF !SnowPack negative or positive

            !--------
            !Next the snow free surface (3). Calculations only done if snowfraction is smaller than 1
            IF ((is == PavSurf .OR. is == BldgSurf) .AND. SnowFrac(is) < 1) THEN !Impervious surfaces (paved, buildings)

               !Surface store update. If precipitation is greater than the threshold, the exceeding water
               !goes directly to runoff
               IF (precip > IPThreshold_mmhr/nsh_real) THEN
                  !runoff = runoff + (precipitation+water from the snow surface+water from other surfaces-the thereshold limit)
                  runoff_snowfree(is) = runoff_snowfree(is) + (Precip + SnowToSurf(is) + AddWater(is) - IPThreshold_mmhr/nsh_real)
                  chang(is) = IPThreshold_mmhr/nsh_real - (drain(is) + ev_snowfree + freezState(is))
               ELSE
                  !Add precip and water from other surfaces and remove drainage, evap and freezing of state_id
                  chang(is) = Precip + SnowToSurf(is) + AddWater(is) - (drain(is) + ev_snowfree + freezState(is))
               END IF

               state_id(is) = state_id(is) + chang(is) !Change in state_id (for whole surface area areasfr(is))

               !Add water from impervious grids
               ! Check sfr_surf/=0 added HCW 08 Dec 2015
               IF (is == PavSurf .AND. sfr_surf(PavSurf) > 0) state_id(is) = state_id(is) + (addImpervious)/sfr_surf(PavSurf)

               runoff_snowfree(is) = runoff_snowfree(is) + drain(is)*AddWaterRunoff(is) !Drainage (not flowing to other surfaces) goes to runoff

               IF (state_id(is) < 0.0) THEN !Surface state_id cannot be negative
                  SurplusEvap(is) = ABS(state_id(is)) !take evaporation from other surfaces in mm
                  ev_snowfree = ev_snowfree - SurplusEvap(is)
                  state_id(is) = 0.0
               END IF

            ELSEIF (is >= 3 .AND. SnowFrac(is) < 1) THEN ! Pervious surfaces (conif, decid, grass unirr, grass irr)

               ev_snowfree = ev_snowfree + EvPart

               !Change in water stores
               IF (VegFraction > 0) THEN
                  IF (Precip + addVeg*(sfr_surf(is)/VegFraction) > (IPThreshold_mmhr/nsh_real)) THEN !if 5min precipitation is larger than 10 mm
                     runoff_snowfree(is) = runoff_snowfree(is) + (Precip + addVeg*(sfr_surf(is)/VegFraction) + &
                                                                  SnowToSurf(is) + AddWater(is) - (IPThreshold_mmhr/nsh_real))
                     chang(is) = (IPThreshold_mmhr/nsh_real) - (drain(is) + ev_snowfree + freezState(is))
                  ELSE
                     chang(is) = Precip + addVeg*(sfr_surf(is)/VegFraction) + SnowToSurf(is) + &
                                 AddWater(is) - (drain(is) + ev_snowfree + freezState(is))
                  END IF
               ELSE
                  chang(is) = Precip + SnowToSurf(is) + AddWater(is) - (drain(is) + ev_snowfree + freezState(is))
               END IF

               state_id(is) = state_id(is) + chang(is)

               !Add water in soil store only if ground is not frozen
               IF (Temp_C > 0) THEN
                  soilstore_id(is) = soilstore_id(is) + Drain(is)*AddWaterRunoff(is)*(1 - SnowFrac(is))
               ELSE
                  runoff_snowfree(is) = runoff_snowfree(is) + Drain(is)*AddWaterRunoff(is)
               END IF

               !If state_id of the surface is negative, remove water from soilstore
               IF (state_id(is) < 0.0) THEN

                  IF ((soilstore_id(is) + state_id(is)) >= 0 .AND. Temp_C > 0) THEN !If water in soilstore, water is removed

                     soilstore_id(is) = soilstore_id(is) + state_id(is)*(1 - SnowFrac(is))
                     state_id(is) = 0.0

                  ELSE !If not water in the soilstore evaporation does not occur
                     chang(is) = chang(is) + state_id(is)
                     ev_snowfree = ev_snowfree + state_id(is)
                     state_id(is) = 0.0
                  END IF
               END IF !state_id is negative

               !If soilstorage is full at this point, excess will go to surface runoff
               IF (soilstore_id(is) > SoilStoreCap(is)) THEN
                  runoffTest = runoffTest + (soilstore_id(is) - SoilStoreCap(is))
                  soilstore_id(is) = SoilStoreCap(is)
               ELSEIF (soilstore_id(is) < 0) THEN
                  soilstore_id(is) = 0
               END IF

            END IF !Surface type

         END IF !Surface fraction
         !-------------------------------------------------------------------------------------------------------------------

         !Calculate change in SnowPack and state_id for the respective surface areas
         !Here the case where not all surface state_id freezes is handled
         IF (snowFracFresh2 > 0) THEN
            surf_chang_tot = (state_id(is) - stateOld(is))*sfr_surf(is)*(1 - SnowFrac(is)) &
                             - Precip*sfr_surf(is)*(1 - snowFracFresh2)
            chSnow_tot = ((SnowPack(is) + SnowWater(is)) - snowTotInit)*sfr_surf(is)*(1 - SnowFrac(is)) &
                         - Precip*sfr_surf(is)*snowFracFresh2
         ELSE
            surf_chang_tot = (state_id(is) - stateOld(is))*sfr_surf(is)*(1 - SnowFrac(is))
            chSnow_tot = ((SnowPack(is) + SnowWater(is)) - snowTotInit)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld)
         END IF

         !===Update snow depth, weighted SWE, and Mwstore
         IF (SnowDens(is) /= 0) THEN
            SnowDepth(is) = SnowPack(is)*waterDens/SnowDens(is)
         END IF

         ! Calculate overall snow water equivalent
         swe = swe + SnowPack(is)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld)
         MwStore = MwStore + SnowWater(is)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld)

         !if (id==6.and.it==13.and.imin==20) then!
         !if (id==85.and.it==3.and.imin==10) then!
         ! if (id==92.and.it==21.and.imin==35) then!
         !  write(*,*)  ((SnowPack(is)+SnowWater(is))-snowTotInit)*sfr_surf(is)*(1-SnowFrac(is)),&
         !              runoff(is)*sfr_surf(is)*(1-SnowFrac(is)),&
         !              ev*sfr_surf(is)*(1-SnowFrac(is)),&
         !              (state_id(is)-stateOld(is))*sfr_surf(is)*(1-SnowFrac(is)),Precip*sfr_surf(is)
         !  write(*,*)  changSnow(is),runoff(is),ev,chang(is),runoffTest,FreezState(is) !changSnow(is)-freezMelt(is)
         !  write(*,*)  is,Precip,runoff_per_tstep,ev_per_tstep,surf_chang_per_tstep,chSnow_per_interval
         !  write(*,*)  is,Precip-runoff_per_tstep-ev_per_tstep,surf_chang_per_tstep+chSnow_per_interval
         !  write(*,*)  is,SnowFrac(is),sfr_surf(is),sfr_surf(is)*ev_snow(is)
         !  pause
         ! endif

         !Only now update the new snow fractions both in the case that snow existing already on ground
         !and snow forms at the current timestep
         IF (snowFracFresh1 > 0) SnowFrac(is) = snowFracFresh1
         IF (snowFracFresh2 > 0) SnowFrac(is) = snowFracFresh2

         !Calculate new snow fraction here.
         !Tässä ongelmana että snow fraction muuttuu vain kun on sulamisvettä ja on vika tunti.
         !Tämä ei juuri koskaan toteudu johtuen lämpötilan vuorokausisyklistä
         !Kokeile tässä ajaa kahdella tavalla 1) ei tarvita Mw:tä
         !                                    2) päivitys voi tapahtua millon vain
         !if (SnowFractionChoice==2.and.imin==(nsh_real-1)/nsh_real*60) then
         IF (SnowFractionChoice == 2) THEN
            IF (SnowPack(is) > 0 .AND. mw_ind(is) > 0) THEN
               SnowFrac(is) = SnowDepletionCurve(is, SnowPack(is), SnowPackLimit(is))
               IF (SnowFrac(is) < 0.001) SnowFrac(is) = 0.001 !The snow fraction minimum is 1% of the surface
            ELSEIF (SnowPack(is) == 0) THEN
               SnowFrac(is) = 0
            END IF
         END IF
      END IF !end snowCalcSwitch

      !Add evaporation to total
      IF (is == BldgSurf .OR. is == PavSurf) THEN
         ev_tot = ev_snowfree*sfr_surf(is)*(1 - SnowFrac(is)) + ev_snow(is)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld)
         qe_tot = ev_snow(is)*tlv_sub*sfr_surf(is)*SnowFrac(is) + ev_snowfree*tlv*sfr_surf(is)*(1 - SnowFrac(is))
      ELSE
         ev_tot = ev_snowfree*sfr_surf(is)*(1 - SnowFrac(is)) + ev_snow(is)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld)
         qe_tot = ev_snow(is)*tlv_sub*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld) + ev_snowfree*tlv*sfr_surf(is)*(1 - SnowFrac(is))
      END IF

      !========RUNOFF=======================

      !Add runoff to pipes
      runoffPipes = runoffPipes &
                    + runoffSnow_surf(is)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld) &
                    + runoff_snowfree(is)*sfr_surf(is)*(1 - SnowFrac(is)) &
                    + runoffTest*sfr_surf(is)
      CALL updateFlood( &
         is, runoff_snowfree, & ! input:
         sfr_surf, PipeCapacity, RunoffToWater, &
         runoffAGimpervious, surplusWaterBody, runoffAGveg, runoffPipes) ! inout:

      runoff_tot = runoffSnow_surf(is)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld) &
                   + runoff_snowfree(is)*sfr_surf(is)*(1 - SnowFrac(is)) &
                   + runoffTest*sfr_surf(is)

      ! !===Update snow depth, weighted SWE, and Mwstore
      ! IF (SnowDens(is) /= 0) THEN
      !    SnowDepth(is) = SnowPack(is)*waterDens/SnowDens(is)
      ! END IF

      ! ! Calculate overall snow water equivalent
      ! swe = swe + SnowPack(is)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld)
      ! MwStore = MwStore + SnowWater(is)*sfr_surf(is)*MAX(SnowFrac(is), snowfracOld)

      ! !if (id==6.and.it==13.and.imin==20) then!
      ! !if (id==85.and.it==3.and.imin==10) then!
      ! ! if (id==92.and.it==21.and.imin==35) then!
      ! !  write(*,*)  ((SnowPack(is)+SnowWater(is))-snowTotInit)*sfr_surf(is)*(1-SnowFrac(is)),&
      ! !              runoff(is)*sfr_surf(is)*(1-SnowFrac(is)),&
      ! !              ev*sfr_surf(is)*(1-SnowFrac(is)),&
      ! !              (state_id(is)-stateOld(is))*sfr_surf(is)*(1-SnowFrac(is)),Precip*sfr_surf(is)
      ! !  write(*,*)  changSnow(is),runoff(is),ev,chang(is),runoffTest,FreezState(is) !changSnow(is)-freezMelt(is)
      ! !  write(*,*)  is,Precip,runoff_per_tstep,ev_per_tstep,surf_chang_per_tstep,chSnow_per_interval
      ! !  write(*,*)  is,Precip-runoff_per_tstep-ev_per_tstep,surf_chang_per_tstep+chSnow_per_interval
      ! !  write(*,*)  is,SnowFrac(is),sfr_surf(is),sfr_surf(is)*ev_snow(is)
      ! !  pause
      ! ! endif

      ! !Only now update the new snow fractions both in the case that snow existing already on ground
      ! !and snow forms at the current timestep
      ! IF (snowFracFresh1 > 0) SnowFrac(is) = snowFracFresh1
      ! IF (snowFracFresh2 > 0) SnowFrac(is) = snowFracFresh2

      ! !Calculate new snow fraction here.
      ! !Tässä ongelmana että snow fraction muuttuu vain kun on sulamisvettä ja on vika tunti.
      ! !Tämä ei juuri koskaan toteudu johtuen lämpötilan vuorokausisyklistä
      ! !Kokeile tässä ajaa kahdella tavalla 1) ei tarvita Mw:tä
      ! !                                    2) päivitys voi tapahtua millon vain
      ! !if (SnowFractionChoice==2.and.imin==(nsh_real-1)/nsh_real*60) then
      ! IF (SnowFractionChoice == 2) THEN
      !    IF (SnowPack(is) > 0 .AND. mw_ind(is) > 0) THEN
      !       SnowFrac(is) = SnowDepletionCurve(is, SnowPack(is), SnowPackLimit(is))
      !       IF (SnowFrac(is) < 0.001) SnowFrac(is) = 0.001 !The snow fraction minimum is 1% of the surface
      !    ELSEIF (SnowPack(is) == 0) THEN
      !       SnowFrac(is) = 0
      !    END IF
      ! END IF

      RETURN

      !==========================================================================
      !WATERBODY is treated separately as state_id always below ice if ice existing
      !Calculate change in SnowPack
606   changSnow(WaterSurf) = (Precip + freezMelt(WaterSurf) + freezState(WaterSurf)) - &
                             (mw_ind(WaterSurf) + ev_snow(WaterSurf))

      SnowPack(WaterSurf) = SnowPack(WaterSurf) + changSnow(WaterSurf) !Update SnowPack
      state_id(WaterSurf) = state_id(WaterSurf) + FlowChange - freezState(WaterSurf) !Update state_id below ice

      !If SnowPack exists
      IF (SnowPack(WaterSurf) > 0) THEN

         !Add melted water to meltstore and freeze water according to freezMelt(is)
         SnowWater(WaterSurf) = SnowWater(WaterSurf) + mw_ind(WaterSurf) - freezMelt(WaterSurf)

         !Calculate water holding capacity (FWC: Valeo and Ho, 2004) of the SnowPack
         FWC = WaterHoldCapFrac*SnowPack(WaterSurf)

         !If FWC is exceeded, add meltwater to state_id
         IF (SnowWater(WaterSurf) >= FWC .AND. Temp_C >= 0) THEN
            state_id(WaterSurf) = state_id(WaterSurf) + (SnowWater(WaterSurf) - FWC)
            SnowWater(WaterSurf) = FWC
         END IF

         !If SnowPack is negative, it melts at this timestep
      ELSEIF (SnowPack(is) < 0) THEN

         !Add water to the meltwater store
         !If freezing meltwater inside this hour, remove it from the SnowWater
         SnowWater(WaterSurf) = SnowWater(WaterSurf) - freezMelt(WaterSurf) &
                                + mw_ind(WaterSurf)

         state_id(WaterSurf) = state_id(WaterSurf) + SnowWater(WaterSurf) + SnowPack(WaterSurf) !Add meltwater to state_id
         SnowPack(WaterSurf) = 0
         IF (state_id(WaterSurf) < 0) ev_snow(WaterSurf) = ev_snow(WaterSurf) + state_id(WaterSurf)

      END IF !SnowPack negative or positive

      !Check water state_id separately
      IF (state_id(WaterSurf) > StoreDrainPrm(5, WaterSurf)) THEN
         runoff_snowfree(WaterSurf) = runoff_snowfree(WaterSurf) + (state_id(WaterSurf) - StoreDrainPrm(5, WaterSurf))
         state_id(WaterSurf) = StoreDrainPrm(5, WaterSurf)
         runoffWaterBody = runoffWaterBody + runoff_snowfree(WaterSurf)*sfr_surf(WaterSurf)
      ELSE
         state_id(WaterSurf) = state_id(WaterSurf) + surplusWaterBody

         IF (state_id(WaterSurf) > StoreDrainPrm(5, WaterSurf)) THEN
            runoffWaterBody = runoffWaterBody + (state_id(WaterSurf) - StoreDrainPrm(5, WaterSurf))*sfr_surf(WaterSurf)
            state_id(WaterSurf) = StoreDrainPrm(5, WaterSurf)
         END IF
      END IF

      !Change state_id of snow and surface
      chSnow_tot = ((SnowPack(WaterSurf) + SnowWater(WaterSurf)) - snowTotInit)*sfr_surf(WaterSurf)
      !ch_per_interval=ch_per_interval+(state_id(WaterSurf)-stateOld(WaterSurf))*sfr_surf(WaterSurf)
      surf_chang_tot = (state_id(WaterSurf) - stateOld(WaterSurf))*sfr_surf(WaterSurf)

      !Evaporation
      ev_tot = ev_snowfree*sfr_surf(WaterSurf) + ev_snow(WaterSurf)*sfr_surf(WaterSurf)
      qe_tot = ev_snow(WaterSurf)*tlv_sub*sfr_surf(WaterSurf) + ev_snowfree*tlv*sfr_surf(WaterSurf)
      runoff_tot = runoff_snowfree(is) !The total runoff from the area

      IF (SnowPack(WaterSurf) > 0) THEN !Fraction only 1 or 0
         SnowFrac(WaterSurf) = 1
      ELSE
         SnowFrac(WaterSurf) = 0
      END IF

   END SUBROUTINE SnowCalc

   !==========================================================================
   !==========================================================================
   !Calculates evaporation from snow surface (ev_snow).
   !Last update: LJ/Jan 2019 Function changed to subroutine. tlv_sub added to output
   SUBROUTINE Evap_SUEWS_Snow(Qm, QP, lvS_J_kg, avdens, avRh, Press_hPa, Temp_C, RAsnow, psyc_hPa, &
                              tstep, avcp, sIce_hPa, dectime, ev_snow, tlv_sub)

      USE meteo, ONLY: sat_vap_pressice

      IMPLICIT NONE

      !INPUT
      REAL(KIND(1D0)), INTENT(in) :: Qm !melt heat,
      REAL(KIND(1D0)), INTENT(in) :: QP !advect. heat
      REAL(KIND(1D0)), INTENT(in) :: lvS_J_kg !latent heat of sublimation
      REAL(KIND(1D0)), INTENT(in) :: avdens !air density
      REAL(KIND(1D0)), INTENT(in) :: avRh !relative humidity
      REAL(KIND(1D0)), INTENT(in) :: Press_hPa !air pressure
      REAL(KIND(1D0)), INTENT(in) :: Temp_C !air temperature
      REAL(KIND(1D0)), INTENT(in) :: RAsnow !aerodyn res snow
      REAL(KIND(1D0)), INTENT(in) :: psyc_hPa !psychometric constant
      REAL(KIND(1D0)), INTENT(in) :: avcp !spec. heat,
      REAL(KIND(1D0)), INTENT(in) :: sIce_hPa !satured curve on snow
      REAL(KIND(1D0)), INTENT(in) :: dectime
      INTEGER, INTENT(in) :: tstep

      REAL(KIND(1D0)), INTENT(out) :: ev_snow !Evaporation
      REAL(KIND(1D0)), INTENT(out) :: tlv_sub !Latent heat for sublimation

      !OTHER VARIABLES
      REAL(KIND(1D0)) :: e_snow, & !PM equation obe line
                         sae_snow, & !s * (Available energy)
                         qe_snow, & !Latent heat flux
                         vdrcIce, & !Vapour pressure deficit
                         esIce_hPa, & !Saturation vapor pressure over ice
                         EaIce_hPa, & !Vapour pressure
                         tstep_real !timestep as real

      INTEGER :: from = 1
      !-----------------------------------------------------

      tstep_real = REAL(tstep, KIND(1D0))

      sae_snow = sIce_hPa*(Qp - Qm) !Calculate the driving parameter in calculation of evaporation. Järvi et al. (2015)

      esIce_hPa = sat_vap_pressIce(Temp_C, Press_hPa, from, dectime) !Saturation vapor pressure over ice
      EaIce_hPa = avRh/100*esIce_hPa !Vapour pressure of water
      vdrcIce = (esIce_hPa - eaIce_hpa)*avdens*avcp !Vapour pressure deficit
      tlv_sub = lvS_J_kg/tstep_real !Latent heat for sublimation
      e_snow = sae_snow + vdrcIce/RAsnow !PM equation
      qe_snow = e_snow/(sIce_hPa + psyc_hPa) !Latent heat (W/m^2)
      ev_snow = qe_snow/tlv_sub !Evaporation (in mm)

      RETURN

   END SUBROUTINE Evap_SUEWS_Snow

   !==========================================================================
   !==========================================================================
   ! Calculates mechanical removal of snow from roofs ans roads
   SUBROUTINE snow_removal( &
      is, &
      SnowFrac, sfr_surf, &
      SnowPack, SnowRemoval, &
      SnowLimPaved, SnowLimBldg)

      IMPLICIT NONE
      INTEGER, INTENT(in) :: is
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac, sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowPack
      REAL(KIND(1D0)), DIMENSION(2), INTENT(out) :: SnowRemoval
      REAL(KIND(1D0)), INTENT(in) :: SnowLimPaved, SnowLimBldg
      !write(*,*) is, SnowPack(is),SnowLimPaved,SnowLimBldg

      IF (is == PavSurf) THEN
         IF (SnowPack(PavSurf) > SnowLimPaved) THEN
            SnowRemoval(PavSurf) = (SnowPack(PavSurf) - SnowLimPaved)*sfr_surf(PavSurf)*SnowFrac(PavSurf)
            SnowPack(PavSurf) = SnowLimPaved
            !SnowPack(PavSurf)=SnowPack(PavSurf)/SnowFrac(PavSurf)
         END IF
      END IF
      IF (is == BldgSurf) THEN
         IF (SnowPack(BldgSurf) > SnowLimBldg) THEN
            SnowRemoval(2) = (SnowPack(BldgSurf) - SnowLimBldg)*sfr_surf(BldgSurf)*SnowFrac(BldgSurf)
            SnowPack(BldgSurf) = SnowLimBldg
            !SnowPack(BldgSurf)=SnowPack(BldgSurf)/SnowFrac(BldgSurf)
         END IF
      END IF
      !write(*,*) is, SnowPack(is),SnowLimPaved,SnowLimBldg
      !pause
   END SUBROUTINE snow_removal

   !----------------------------------------------------------------------------
   !----------------------------------------------------------------------------
   FUNCTION SnowDepletionCurve(is, swe, sweD) RESULT(asc)
      !This function calculates surface coverage of snow according to the
      !depletion curves in Valeo and Ho (2004).
      !INPUT: is   Surface type number
      !       swe  Snow water content
      !       sweD Limit for

      USE allocateArray

      IMPLICIT NONE

      INTEGER :: is
      REAL(KIND(1D0)) :: asc, sweD, swe

      ! initialisation
      asc = 1
      !Impervious surface
      IF (is == PavSurf) THEN

         IF (swe <= sweD) THEN !Snow water equivalent below threshold
            asc = ((swe/sweD))**2
         ELSE
            asc = 1
         END IF

         !Bldgs surface
      ELSEIF (is == BldgSurf) THEN

         IF (swe <= sweD) THEN
            IF ((swe/sweD) < 0.9) THEN
               asc = (swe/sweD)*0.5
            ELSE
               asc = (swe/sweD)**8
            END IF
         ELSE
            asc = 1
         END IF
      ELSEIF (is == WaterSurf) THEN
         IF (swe > 0) asc = 1

         !Vegetion surfaces
      ELSE
         IF (swe <= sweD) THEN

            asc = 1 - ((1/3.1416)*ACOS(2*(swe/sweD) - 1))**1.7
         ELSE
            asc = 1
         END IF

      END IF

      !asc=real(int(10000.*asc))/10000  !4 decimal precision

      RETURN
   END FUNCTION SnowDepletionCurve

   SUBROUTINE veg_fr_snow( &
      sfr_surf, SnowFrac, & !input
      veg_fr) !output

      IMPLICIT NONE

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !< surface fractions
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac !< snowy surface fractions [-]

      REAL(KIND(1D0)), INTENT(out) :: veg_fr !< vegetated surface fractions [-]

      veg_fr = DOT_PRODUCT(sfr_surf(3:7), 1 - SnowFrac(3:7))

   END SUBROUTINE veg_fr_snow

   !In this subroutine the snow properties are updated. The aging functions work for hourly air
   !temperature (dt=1). As the code timestep is typically smaller than one hour, the new albedo
   !and density are calculated using the timestep air temperature and then these are scaled to timestep
   !according to NSH.  Made by LJ in sprint 2014
   !Last update:
   ! TS 17 Sep 2017 - Improve the explicit interface
   ! LJ 7 July 2015 - Changed to work with shorter timestep: defined by tstep. Cleaning of the code.
   !
   !========================================================================

   SUBROUTINE SnowUpdate( &
      tstep, & !input
      Temp_C, &
      tau_a, &
      tau_f, &
      tau_r, &
      SnowDensMax, &
      SnowDensMin, &
      SnowAlbMax, &
      SnowAlbMin, &
      SnowPack_prev, SnowAlb_prev, SnowDens_prev, &
      SnowAlb_next, SnowDens_next & ! output
      )

      IMPLICIT NONE

      ! INTEGER, INTENT(in)::nsurf
      INTEGER, INTENT(in) :: tstep

      REAL(KIND(1D0)), INTENT(in) :: Temp_C !Air temperature
      REAL(KIND(1D0)), INTENT(in) :: tau_a
      REAL(KIND(1D0)), INTENT(in) :: tau_f
      REAL(KIND(1D0)), INTENT(in) :: tau_r
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMax
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMin
      REAL(KIND(1D0)), INTENT(in) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(in) :: SnowAlbMin

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_prev

      REAL(KIND(1D0)), INTENT(in) :: SnowAlb_prev
      REAL(KIND(1D0)), INTENT(out) :: SnowAlb_next

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_prev
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: SnowDens_next

      ! INTEGER::is
      REAL(KIND(1D0)) :: alb_change !Change in snow albedo
      REAL(KIND(1D0)) :: dens_change !Change in snow density
      REAL(KIND(1D0)), PARAMETER :: tau_1 = 24*60*60 !Number of seconds in a day

      !Initialize
      alb_change = 0
      dens_change = 0

      !==========================================================
      !Calculation of snow albedo by Lemonsu et al. 2010
      !(org: Verseghy (1991)&Baker et al.(1990))
      ! IF (SUM(SnowPack_prev) > 0) THEN !Check if snow on any of the surfaces
      !    IF (Temp_C < 0) THEN
      !       !alb_change = tau_a*(60*60)/tau_1
      !       alb_change = tau_a*(tstep)/tau_1
      !       SnowAlb_next = SnowAlb_prev - alb_change
      !    ELSE
      !       !alb_change = exp(-tau_f*(60*60)/tau_1)
      !       alb_change = EXP(-tau_f*(tstep)/tau_1)
      !       SnowAlb_next = (SnowAlb_prev - SnowAlbMin)*alb_change + SnowAlbMin
      !    ENDIF
      !    IF (SnowAlb_next < SnowAlbMin) SnowAlb_next = SnowAlbMin !Albedo cannot be smaller than the min albedo
      !    IF (SnowAlb_next > SnowAlbMax) SnowAlb_next = SnowAlbMax !Albedo cannot be larger than the max albedo
      !    if (SnowAlb_next < 0) print *, 'SnowAlbMin/max in SnowUpdate', SnowAlbMin, SnowAlbMax, SnowAlb_next
      ! ELSE
      !    SnowAlb_next = 0
      ! ENDIF
      ! if (SnowAlb_next < 0) print *, 'SnowAlb in SnowUpdate', SnowAlb_next
      SnowAlb_next = update_snow_albedo( &
                     tstep, SnowPack_prev, SnowAlb_prev, Temp_C, &
                     tau_a, tau_f, SnowAlbMax, SnowAlbMin)

      !Update snow density: There is a mistake in Järvi et al. (2014): tau_h should be tau_1
      ! DO is = 1, nsurf
      !    !If SnowPack existing
      !    IF (SnowPack_prev(is) > 0) THEN
      !       dens_change = EXP(-tau_r*(tstep)/tau_1)
      !       IF (SnowPack_prev(is) > 0) SnowDens_next(is) = (SnowDens_prev(is) - SnowDensMax)*dens_change + SnowDensMax
      !       IF (SnowDens_next(is) > SnowDensMax) SnowDens_next(is) = SnowDensMax
      !    ELSE
      !       SnowDens_next(is) = SnowDensMin
      !    ENDIF
      ! ENDDO

      SnowDens_next = update_snow_dens( &
                      tstep, SnowPack_prev, SnowDens_prev, &
                      tau_r, SnowDensMax, SnowDensMin)

   END SUBROUTINE SnowUpdate

   FUNCTION update_snow_albedo( &
      tstep, SnowPack_prev, SnowAlb_prev, Temp_C, &
      tau_a, tau_f, SnowAlbMax, SnowAlbMin) &
      RESULT(SnowAlb_next)
      IMPLICIT NONE
      INTEGER, INTENT(in) :: tstep

      REAL(KIND(1D0)), DIMENSION(7), INTENT(in) :: SnowPack_prev
      REAL(KIND(1D0)), INTENT(in) :: Temp_C
      REAL(KIND(1D0)), INTENT(in) :: SnowAlb_prev
      REAL(KIND(1D0)), INTENT(in) :: tau_a
      REAL(KIND(1D0)), INTENT(in) :: tau_f
      REAL(KIND(1D0)), INTENT(in) :: SnowAlbMax
      REAL(KIND(1D0)), INTENT(in) :: SnowAlbMin

      REAL(KIND(1D0)) :: SnowAlb_next

      REAL(KIND(1D0)) :: alb_change !Change in snow albedo
      REAL(KIND(1D0)), PARAMETER :: tau_1 = 24*60*60 !Number of seconds in a day

      !==========================================================
      !Calculation of snow albedo by Lemonsu et al. 2010
      !(org: Verseghy (1991)&Baker et al.(1990))
      IF (SUM(SnowPack_prev) > 0) THEN !Check if snow on any of the surfaces
         IF (Temp_C < 0) THEN
            !alb_change = tau_a*(60*60)/tau_1
            alb_change = tau_a*(tstep)/tau_1
            SnowAlb_next = SnowAlb_prev - alb_change
         ELSE
            !alb_change = exp(-tau_f*(60*60)/tau_1)
            alb_change = EXP(-tau_f*(tstep)/tau_1)
            SnowAlb_next = (SnowAlb_prev - SnowAlbMin)*alb_change + SnowAlbMin
         END IF
         IF (SnowAlb_next < SnowAlbMin) SnowAlb_next = SnowAlbMin !Albedo cannot be smaller than the min albedo
         IF (SnowAlb_next > SnowAlbMax) SnowAlb_next = SnowAlbMax !Albedo cannot be larger than the max albedo
         IF (SnowAlb_next < 0) PRINT *, 'SnowAlbMin/max in SnowUpdate', SnowAlbMin, SnowAlbMax, SnowAlb_next
      ELSE
         SnowAlb_next = 0
      END IF
      IF (SnowAlb_next < 0) PRINT *, 'SnowAlb in SnowUpdate', SnowAlb_next

   END FUNCTION update_snow_albedo

   FUNCTION update_snow_dens( &
      tstep, SnowPack_prev, SnowDens_prev, &
      tau_r, SnowDensMax, SnowDensMin) &
      RESULT(SnowDens_next)
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowPack_prev
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowDens_prev
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMax
      REAL(KIND(1D0)), INTENT(in) :: SnowDensMin
      REAL(KIND(1D0)), INTENT(in) :: tau_r
      INTEGER, INTENT(in) :: tstep

      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(out)::SnowDens_next

      REAL(KIND(1D0)), DIMENSION(nsurf) :: SnowDens_next
      REAL(KIND(1D0)) :: dens_change

      INTEGER :: is

      REAL(KIND(1D0)), PARAMETER :: tau_1 = 24*60*60
      ! INTEGER,parameter::nsurf=7

      !Update snow density: There is a mistake in Järvi et al. (2014): tau_h should be tau_1
      DO is = 1, nsurf

         !If SnowPack existing
         IF (SnowPack_prev(is) > 0) THEN
            dens_change = EXP(-tau_r*(tstep)/tau_1)
            IF (SnowPack_prev(is) > 0) SnowDens_next(is) = (SnowDens_prev(is) - SnowDensMax)*dens_change + SnowDensMax
            IF (SnowDens_next(is) > SnowDensMax) SnowDens_next(is) = SnowDensMax
         ELSE
            SnowDens_next(is) = SnowDensMin
         END IF
      END DO

   END FUNCTION update_snow_dens

END MODULE Snow_module
