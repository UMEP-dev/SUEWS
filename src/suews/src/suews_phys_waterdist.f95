MODULE WaterDist_module
   USE allocateArray, ONLY: nsurf, &
                            PavSurf, BldgSurf, &
                            ConifSurf, DecidSurf, GrassSurf, &
                            BSoilSurf, WaterSurf, ExcessSurf
   ! USE get_prof_module, ONLY: get_prof_spectime_sum
   IMPLICIT NONE
   ! INTEGER, PARAMETER :: nsurf = 7
   ! INTEGER, PARAMETER :: PavSurf = 1
   ! INTEGER, PARAMETER :: BldgSurf = 2
   ! INTEGER, PARAMETER :: ConifSurf = 3
   ! INTEGER, PARAMETER :: DecidSurf = 4
   ! INTEGER, PARAMETER :: GrassSurf = 5
   ! INTEGER, PARAMETER :: BSoilSurf = 6
   ! INTEGER, PARAMETER :: WaterSurf = 7
   ! INTEGER, PARAMETER :: ExcessSurf = 8
CONTAINS

   !------------------------------------------------------------------------------
   SUBROUTINE drainage( &
      is, & !input
      state_is, &
      StorCap, &
      DrainEq, &
      DrainCoef1, &
      DrainCoef2, &
      nsh_real, &
      drain_is) !output

      !Calculation of drainage for each land surface.
      !INPUT: Storage capacity, type of drainage equation used, drainage coefficients
      !       used in the equation
      !Modified by HCW 16 Feb 2015
      !  Removed option of Eq 4 (calculation needs to be checked before re-implementing).
      !  Code writes an error if calculated drainage exceeds surface state_id (but code continues).
      !  This may indicate inappropriate drainage equation, storage capacities or model tstep.
      !Modified by LJ in Aug 2011. Drainage cannot exceed the surface storage.
      !Modified LJ in 10/2010
      !------------------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER, INTENT(in) :: is ! surface type number

      REAL(KIND(1D0)), INTENT(in) :: state_is !Wetness status of surface type "is" [mm]
      REAL(KIND(1D0)), INTENT(in) :: StorCap !current storage capacity [mm]
      REAL(KIND(1D0)), INTENT(in) :: DrainCoef1 !Drainage coeff 1 [units depend on choice of eqn]
      REAL(KIND(1D0)), INTENT(in) :: DrainCoef2 !Drainage coeff 2 [units depend on choice of eqn]
      REAL(KIND(1D0)), INTENT(in) :: DrainEq !Drainage equation to use
      REAL(KIND(1D0)), INTENT(in) :: nsh_real !nsh cast as a real for use in calculations
      REAL(KIND(1D0)), INTENT(out) :: drain_is !Drainage of surface type "is" [mm]

      !If surface is dry, no drainage occurs
      IF (state_is < 0.000000001) THEN
         drain_is = 0.0
      ELSE
         IF (INT(DrainEq) == 1) THEN !Falk and Niemczynowicz (1978): Drainage equation for paved, buildings and irrigated grass

            IF (state_is < StorCap) THEN
               drain_is = 0 !No drainage if state_id is less than storage capacity
            ELSE
               drain_is = (DrainCoef1*(state_is - StorCap)**DrainCoef2)/nsh_real
            END IF

         ELSEIF (INT(DrainEq) == 2) THEN !Rutter eqn corrected for c=0, see Eq 9 of Calder & Wright 1986
            drain_is = (DrainCoef1*(EXP(DrainCoef2*state_is) - 1))/nsh_real
            ! N.B. -1 is correct here but brackets are wrong in G&O 1991 Eq 5 & Ja11 Eq 18.

         ELSEIF (INT(DrainEq) == 3) THEN !Falk and Niemczynowicz (1978)
            drain_is = (DrainCoef1*(state_is**DrainCoef2))/nsh_real

         END IF

         ! Check value obtained is physically reasonable
         ! More water cannot drain than is in the surface state_id
         ! although high initial rate of drainage tries to drain more water than is in state_id within tstep
         ! May indicate shorter tstep needed, or a more suitable equation
         IF (drain_is > state_is) THEN
            !write(*,*) 'Drainage:', is, drain(is), state_id(is), drain(is)-state_id(is), DrainEq, DrainCoef1, DrainCoef2, nsh_real
            CALL ErrorHint(61, 'SUEWS_drain: drain_is > state_is for surface is ', drain_is, state_is, is)
            drain_is = state_is !All water in state_id is drained (but no more)
         ELSEIF (drain_is < 0.0001) THEN
            drain_is = 0
         END IF
      END IF

      RETURN

   END SUBROUTINE drainage
   !------------------------------------------------------------------------------

   !--------------Calculation of water storage change of specific land cover------------------------------
   SUBROUTINE cal_water_storage( &
      is, sfr_surf, PipeCapacity, RunoffToWater, pin, & ! input:
      WU_surf, &
      drain_surf, AddWater, addImpervious, nsh_real, state_in, frac_water2runoff, &
      PervFraction, addVeg, SoilStoreCap, addWaterBody, FlowChange, StateLimit, &
      runoffAGimpervious, runoffAGveg, runoffPipes, ev, soilstore, & ! inout:
      surplusWaterBody, SurplusEvap, runoffWaterBody, & ! inout:
      runoff, state_out) !output:
      !------------------------------------------------------------------------------
      !Calculation of storage change
      ! TS 30 Nov 2019
      !   - Allow irrigation on all surfaces (previously only on vegetated surfaces)
      ! LJ 27 Jan 2016
      !   - Removed tabs and cleaned the code
      ! HCW 08 Dec 2015
      !   -Added if-loop check for no Paved surfaces
      ! LJ 6 May 2015
      !   - Calculations of the piperunoff exceedings moved to separate subroutine updateFlood.
      !   - Now also called from snow subroutine
      !   - Evaporation is modified using EvapPart
      !   - when no water on impervious surfaces, evap occurs above pervious surfaces instead
      ! Rewritten by HCW 12 Feb 2015
      !   - Old variable 'p' for water input to the surface renamed to 'p_mm'
      !   - All water now added to p_mm first, before threshold checks or other calculations
      !   - Water from other grids now added to p_mm (instead of state_id for impervious surfaces)
      !   - Removed division of runoff by nsh, as whole model now runs at the same timestep
      !   - Adjusted transfer of ev between surfaces to conserve mass (not depth)
      !   - Volumes used for water transport between grids to account for SurfaceArea changing between grids
      !   - Added threshold check for state_id(WaterSurf) - was going negative
      ! Last modified HCW 09 Feb 2015
      !   - Removed StorCap input because it is provided by module allocateArray
      !   - Tidied and commented code
      ! Modified by LJ in November 2012:
      !   - P>10 was not taken into account for impervious surfaces - Was fixed.
      !   - Above impervious surfaces possibility of the state_id to exceed max capacity was limited
      !     although this should be possible - was fixed
      ! Modified by LJ 10/2010
      ! Rewritten mostly by LJ in 2010
      ! To do:
      !   - Finish area normalisation for RG2G & finish coding GridConnections
      !   - What is the 10 mm hr-1 threshold for?
      !  - Decide upon and correct storage capacities here & in evap subroutine
      !  - FlowChange units should be mm hr-1 - need to update everywhere
      !   - Add SurfaceFlood(is)?
      !   - What happens if sfr_surf(is) = 0 or 1?
      !   - Consider how irrigated trees actually works...
      !------------------------------------------------------------------------------

      IMPLICIT NONE

      !Stores flood water when surface state_id exceeds storage capacity [mm]
      INTEGER, INTENT(in) :: is ! surface type

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf ! surface fractions
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater !Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_in !Wetness status of each surface type from previous timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: frac_water2runoff !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StateLimit !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain_surf !Drainage of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WU_surf !external water use of each surface type [mm]

      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity !Capacity of pipes to transfer water
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater !Fraction of surface runoff going to water body
      REAL(KIND(1D0)), INTENT(in) :: pin !Rain per time interval
      REAL(KIND(1D0)), INTENT(in) :: addImpervious !Water from impervious surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: nsh_real !nsh cast as a real for use in calculations
      REAL(KIND(1D0)), INTENT(in) :: PervFraction ! sum of surface cover fractions for impervious surfaces
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addWaterBody !Water from water surface of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: FlowChange !Difference between the input and output flow in the water body

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: soilstore !Soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(2), INTENT(inout) :: SurplusEvap !Surplus for evaporation in 5 min timestep

      REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state_id [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: runoff !Runoff from each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_out !Wetness status of each surface type [mm]

      ! =============================
      ! TS 01 Apr 2022:
      !   these variables were used as inout variables in the original code for water transfer between grids
      !  but they are not used in the new code, so they are removed here
      REAL(KIND(1D0)), INTENT(inout) :: surplusWaterBody !Extra runoff that goes to water body [mm] as specified by RunoffToWater
      REAL(KIND(1D0)), INTENT(inout) :: runoffAGimpervious !Above ground runoff from impervious surface [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(inout) :: runoffAGveg !Above ground runoff from vegetated surfaces [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(inout) :: runoffPipes !Runoff in pipes [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(inout) :: ev !Evaporation
      REAL(KIND(1D0)), INTENT(inout) :: runoffWaterBody !Above ground runoff from water surface [mm] for whole surface area

      REAL(KIND(1D0)) :: p_mm !Inputs to surface water balance

      !Extra evaporation [mm] from impervious surfaces which cannot happen due to lack of water
      REAL(KIND(1D0)) :: EvPart
      REAL(KIND(1D0)), PARAMETER :: NotUsed = -55.5

      !Threshold for intense precipitation [mm hr-1]
      REAL(KIND(1D0)), PARAMETER :: IPThreshold_mmhr = 10 ! NB:this should be an input and can be specified. SG 25 Apr 2018

      !Initialise extra evaporation to zero
      EvPart = 0

      !Initialise runoff to zero
      runoff(is) = 0

      !SurfaceFlood(is) = 0 !!This probably needs to be carried over between timesteps, but reset for now

      !==================================================================
      ! Combine water inputs to the current surface
      ! Add external water use for each surface type
      p_mm = pin + WU_surf(is)

      ! Add water from other surfaces within the same grid (RS2S) ----
      ! AddWater is the water supplied to the current surface from other surfaces
      !  i.e. drain*WaterDist (see SUEWS_ReDistributeWater)
      p_mm = p_mm + AddWater(is)
      !==================================================================

      !========surface-specific calculation=========================
      SELECT CASE (is)
      CASE (PavSurf, BldgSurf)
         !==== Impervious surfaces (Paved, Buildings) ======================
         ! Add water from neighbouring grids (RG2G)
         ! Add to PavSurf only, as water cannot flow onto buildings
         IF (is == PavSurf) THEN
            IF (sfr_surf(PavSurf) /= 0) THEN ! If loop added HCW 08 Dec 2015
               p_mm = p_mm + addImpervious/sfr_surf(PavSurf)
            END IF
         END IF

         ! Calculate change in surface state_id (inputs - outputs)
         chang(is) = p_mm - (drain_surf(is) + ev)

         ! If p_mm is too large, excess goes to runoff (i.e. the rate of water supply is too fast)
         ! and does not affect state_id
         IF (p_mm > IPThreshold_mmhr/nsh_real) THEN
            runoff(is) = runoff(is) + (p_mm - IPThreshold_mmhr/nsh_real)
            chang(is) = IPThreshold_mmhr/nsh_real - (drain_surf(is) + ev)
         END IF

         ! Calculate updated state_id using chang
         state_out(is) = state_in(is) + chang(is)

         ! Check state_id is within physical limits between zero (dry) and max. storage capacity
         IF (state_out(is) < 0.0) THEN ! Cannot have a negative surface state_id
            ! If there is not sufficient water on the surface, then don't allow this evaporation to happen
            ! Allow evaporation only until surface is dry (state_id(is)=0); additional evaporation -> evaporation surplus
            SurplusEvap(is) = ABS(state_out(is)) !Surplus evaporation is that which tries to evaporate non-existent water
            ev = ev - SurplusEvap(is) !Limit evaporation according to water availability
            state_out(is) = 0.0 !Now surface is dry
            ! elseif (state_id(is)>StoreDrainPrm(6,is)) then   !!This should perhaps be StateLimit(is)
            !    !! If state_id exceeds the storage capacity, then the excess goes to surface flooding
            !    !SurfaceFlood(is)=SurfaceFlood(is)+(state_id(is)-StoreDrainPrm(6,is))   !!Need to deal with this properly
            !    runoff(is)=runoff(is)+(state_id(is)-StoreDrainPrm(6,is))   !!needs to go to flooding
            !    state_id(is)=StoreDrainPrm(6,is)              !Now surface state_id is at max (storage) capacity
         END IF

         ! Recalculate change in surface state_id from difference with previous timestep
         chang(is) = state_out(is) - state_in(is)

         ! Runoff -------------------------------------------------------
         ! For impervious surfaces, some of drain(is) becomes runoff
         runoff(is) = runoff(is) + drain_surf(is)*frac_water2runoff(is) !Drainage (that is not flowing to other surfaces) goes to runoff

         !So, up to this point, runoff(is) can have contributions if
         ! p_mm > ipthreshold (water input too fast)
         ! state_id > StoreDrainPrm(6,is) (net water exceeds storage capacity)
         ! WaterDist specifies some fraction of drain(is) -> runoff

      CASE (ConifSurf:BSoilSurf)
         !==== For Conif, Decid, Grass, BSoil surfaces ==================
         ! Transfer evaporation surplus from impervious surfaces to pervious surfaces
         EvPart = MERGE( &
                  DOT_PRODUCT(SurplusEvap(PavSurf:BldgSurf), sfr_surf(PavSurf:BldgSurf)/PervFraction), &
                  0D0, &
                  PervFraction /= 0)
         ! EvPart needs be distributed to each pervious surface weighted by sfr_surf(is)
         ! EvPart = EvPart*sfr_surf(is)/PervFraction
         ! Add surplus evaporation to ev for pervious surfaces
         ev = ev + EvPart

         ! ---- Add water from neighbouring grids (RG2G) ----
         ! Add to Grass and BSoil only, as water cannot flow onto trees
         IF (is == GrassSurf .OR. is == BSoilSurf) THEN
            IF ((sfr_surf(GrassSurf) + sfr_surf(BSoilSurf)) /= 0) THEN
               p_mm = p_mm + addVeg/(sfr_surf(GrassSurf) + sfr_surf(BSoilSurf))
            END IF
         END IF

         ! Calculate change in surface state_id (inputs - outputs)
         chang(is) = p_mm - (drain_surf(is) + ev)

         ! If p_mm is too large, excess goes to runoff (i.e. the rate of water supply is too fast)
         !  and does not affect state_id
         IF (p_mm > IPThreshold_mmhr/nsh_real) THEN
            runoff(is) = runoff(is) + (p_mm - IPThreshold_mmhr/nsh_real)
            chang(is) = IPThreshold_mmhr/nsh_real - (drain_surf(is) + ev)
         END IF

         ! Calculate updated state_id using chang
         state_out(is) = state_in(is) + chang(is)

         ! Check state_id is within physical limits between zero (dry) and max. storage capacity
         IF (state_out(is) < 0.0) THEN ! Cannot have a negative surface state_id
            ! If there is not sufficient water on the surface, then remove water from soilstore
            ! Allow evaporation until soilstore_id is depleted and surface is dry
            IF ((soilstore(is) + state_out(is)) >= 0) THEN
               soilstore(is) = soilstore(is) + state_out(is)
               state_out(is) = 0.0
               ! If there is not sufficient water on the surface or soilstore, then don't allow this evaporation to happen
            ELSE
               ev = ev - ABS(state_out(is)) !Limit evaporation according to water availability
               state_out(is) = 0.0 !Now surface is dry
            END IF

            !elseif (state_id(is)>StoreDrainPrm(6,is)) then   !!This should perhaps be StateLimit(is)
            !   !! If state_id exceeds the storage capacity, then the excess goes to surface flooding
            !   !SurfaceFlood(is)=SurfaceFlood(is)+(state_id(is)-StoreDrainPrm(6,is))   !!Need to deal with this properly
            !   runoff(is)=runoff(is)+(state_id(is)-StoreDrainPrm(6,is))   !!needs to go to flooding
            !   state_id(is)=StoreDrainPrm(6,is)              !Now surface state_id is at max (storage) capacity
         END IF

         ! Recalculate change in surface state_id from difference with previous timestep
         chang(is) = state_out(is) - state_in(is)

         !Where should this go? Used to be before previous part!!
         ! soilstore_id -------------------------------------------------
         ! For pervious surfaces (not water), some of drain(is) goes to soil storage
         ! Drainage (that is not flowing to other surfaces) goes to soil storages
         soilstore(is) = soilstore(is) + drain_surf(is)*frac_water2runoff(is)

         ! If soilstore is full, the excess will go to runoff
         IF (soilstore(is) > SoilStoreCap(is)) THEN ! TODO: this should also go to flooding of some sort
            runoff(is) = runoff(is) + (soilstore(is) - SoilStoreCap(is))
            soilstore(is) = SoilStoreCap(is)
         ELSEIF (soilstore(is) < 0) THEN !! QUESTION: But where does this lack of water go? !!Can this really happen here?
            CALL ErrorHint(62, 'SUEWS_store: soilstore_id(is) < 0 ', soilstore(is), NotUsed, is)
            ! Code this properly - soilstore_id(is) < 0 shouldn't happen given the above loops
            !soilstore_id(is)=0   !Groundwater / deeper soil should kick in
         END IF

      CASE (WaterSurf)
         IF (sfr_surf(WaterSurf) /= 0) THEN

            ! ---- Add water from neighbouring grids (RG2G) ----
            p_mm = p_mm + addWaterBody/sfr_surf(WaterSurf)

            ! Calculate change in surface state_id (inputs - outputs)
            ! No drainage for water surface
            ! FlowChange is the difference in input and output flows [mm hr-1]
            chang(is) = p_mm + FlowChange/nsh_real - ev

            ! Calculate updated state_id using chang
            state_out(is) = state_in(is) + chang(is)

            ! Check state_id is within physical limits between zero (dry) and max. storage capacity
            IF (state_out(is) < 0.0) THEN ! Cannot have a negative surface state_id
               ! If there is not sufficient water on the surface, then don't allow this evaporation to happen
               ev = ev - ABS(state_out(is)) !Limit evaporation according to water availability
               state_out(is) = 0.0 !Now surface is dry
               !elseif (state_id(is)>StoreDrainPrm(6,is)) then   !!This should perhaps be StateLimit(is)
               !   !! If state_id exceeds the storage capacity, then the excess goes to surface flooding
               !   !SurfaceFlood(is)=SurfaceFlood(is)+(state_id(is)-StoreDrainPrm(6,is))   !!Need to deal with this properly
               !   runoff(is)=runoff(is)+(state_id(is)-StoreDrainPrm(6,is))   !!needs to go to flooding
               !   state_id(is)=StoreDrainPrm(6,is)              !Now surface state_id is at max (storage) capacity
            END IF

            ! Recalculate change in surface state_id from difference with previous timestep
            chang(is) = state_out(is) - state_in(is)

            ! If state_id exceeds limit, then excess goes to runoff (currently applies to water StoreDrainPrm only)
            IF (state_out(WaterSurf) > StateLimit(WaterSurf)) THEN
               runoff(WaterSurf) = runoff(WaterSurf) + (state_out(WaterSurf) - StateLimit(WaterSurf))
               state_out(WaterSurf) = StateLimit(WaterSurf)
               runoffWaterBody = runoffWaterBody + runoff(WaterSurf)*sfr_surf(WaterSurf)
            ELSE
               state_out(WaterSurf) = state_out(WaterSurf) + surplusWaterBody
               IF (state_out(WaterSurf) > StateLimit(WaterSurf)) THEN
                  runoffWaterBody = runoffWaterBody + (state_out(WaterSurf) - StateLimit(WaterSurf))*sfr_surf(WaterSurf)
                  state_out(WaterSurf) = StateLimit(WaterSurf)
               END IF
            END IF

            ! Recalculate change in surface state_id from difference with previous timestep
            chang(is) = state_out(is) - state_in(is)
            ! Check for NaN in variables used to calculate state_out for WaterSurf

            IF ( &
               (p_mm /= p_mm) .OR. &
               (FlowChange /= FlowChange) .OR. &
               (ev /= ev) .OR. &
               (state_in(is) /= state_in(is))) THEN
               WRITE (*, *) 'NaN detected in WaterSurf state_out calculation', p_mm, ev, is
            END IF
         END IF
      END SELECT
      !==================================================================

      !==== RUNOFF ======================================================
      ! TODO: to consider areas here - SurfaceArea may vary between grids too
      ! - also implement where water for next surface is calculated (RunoffFromGrid subroutine)
      ! Calculations of the piperunoff exceedensances moved to separate subroutine so that from snow same
      ! calculations can be made. LJ in May 2015

      IF (is < WaterSurf) THEN !Not for water body
         !  CALL updateFlood
         CALL updateFlood( &
            is, runoff, & ! input:
            sfr_surf, PipeCapacity, RunoffToWater, &
            runoffAGimpervious, surplusWaterBody, runoffAGveg, runoffPipes) ! inout:
      END IF

   END SUBROUTINE cal_water_storage
   !------------------------------------------------------------------------------

   ! TODO: continue here  for the multi-facet case
   SUBROUTINE cal_water_storage_surf( &
      pin, nsh_real, SnowFrac_in, & ! input:
      PipeCapacity, RunoffToWater, &
      addImpervious, addVeg, addWaterBody, FlowChange, &
      SoilStoreCap_surf, StateLimit_surf, &
      PervFraction, &
      sfr_surf, drain_surf, AddWater_surf, frac_water2runoff_surf, WU_surf, &
      ev_surf_in, state_surf_in, soilstore_surf_in, &
      runoffAGimpervious_grid, runoffAGveg_grid, runoffPipes_grid, runoffWaterBody_grid, & ! inout
      ev_surf_out, state_surf_out, soilstore_surf_out, & ! output:
      runoff_surf &
      ) !output:
      IMPLICIT NONE

      !Stores flood water when surface state_id exceeds storage capacity [mm]
      INTEGER :: is ! surface type

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf ! surface fractions
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: AddWater_surf !Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: state_surf_in !Wetness status of each surface type from previous timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: frac_water2runoff_surf !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: StateLimit_surf !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: drain_surf !Drainage of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: WU_surf !external water use of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: ev_surf_in !Evaporation
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_surf_in !soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SnowFrac_in !needed whether storage was already calculated in snow equations

      ! REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction !Capacity of pipes to transfer water
      REAL(KIND(1D0)), INTENT(in) :: PipeCapacity !Capacity of pipes to transfer water
      REAL(KIND(1D0)), INTENT(in) :: RunoffToWater !Fraction of surface runoff going to water body
      REAL(KIND(1D0)), INTENT(in) :: pin !Rain per time interval
      REAL(KIND(1D0)), INTENT(in) :: addImpervious !Water from impervious surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: nsh_real !nsh cast as a real for use in calculations
      REAL(KIND(1D0)), INTENT(in) :: PervFraction ! sum of surface cover fractions for impervious surfaces
      REAL(KIND(1D0)), INTENT(in) :: addVeg !Water from vegetated surfaces of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: addWaterBody !Water from water surface of other grids [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(in) :: FlowChange !Difference between the input and output flow in the water body

      ! REAL(KIND(1D0)), INTENT(out) :: runoff_grid !Wetness status of each surface type [mm]
      ! REAL(KIND(1D0)), INTENT(out) :: state_grid !Wetness status of each surface type [mm]
      ! REAL(KIND(1D0)), INTENT(out) :: ev_grid !Wetness status of each surface type [mm]
      ! REAL(KIND(1D0)), INTENT(out) :: surf_chang_grid !Wetness status of each surface type [mm]
      REAL(KIND(1D0)), INTENT(out) :: runoffAGimpervious_grid !Above ground runoff from impervious surface [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(out) :: runoffAGveg_grid !Above ground runoff from vegetated surfaces [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(out) :: runoffPipes_grid !Runoff in pipes [mm] for whole surface area
      REAL(KIND(1D0)), INTENT(out) :: runoffWaterBody_grid !Above ground runoff from water surface [mm] for whole surface area
      ! REAL(KIND(1D0)), INTENT(out) :: NWstate_grid !Above ground runoff from water surface [mm] for whole surface area

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: ev_surf_out !evaporation each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: state_surf_out !Wetness status of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: soilstore_surf_out !Wetness status of each surface type [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: runoff_surf !Runoff from each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore !Soil moisture of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state_id [mm]
      REAL(KIND(1D0)), DIMENSION(2) :: SurplusEvap !Surplus for evaporation in 5 min timestep

      ! =============================
      ! TS 01 Apr 2022:
      !   these variables were used as inout variables in the original code for water transfer between grids
      !  but they are not used in the new code, so they are removed here
      REAL(KIND(1D0)) :: surplusWaterBody !Extra runoff that goes to water body [mm] as specified by RunoffToWater

      ! REAL(KIND(1D0)) :: p_mm !Inputs to surface water balance

      REAL(KIND(1D0)), DIMENSION(nsurf) :: ev_surf !Evaporation

      !Extra evaporation [mm] from impervious surfaces which cannot happen due to lack of water
      ! REAL(KIND(1D0)) :: EvPart
      REAL(KIND(1D0)), PARAMETER :: NotUsed = -55.5

      !Threshold for intense precipitation [mm hr-1]
      REAL(KIND(1D0)), PARAMETER :: IPThreshold_mmhr = 10 ! NB:this should be an input and can be specified. SG 25 Apr 2018

      ev_surf = ev_surf_in
      soilstore = soilstore_surf_in

      SurplusEvap = 0
!      runoffAGveg_grid = 0
!      runoffPipes_grid = 0
!     runoffAGimpervious_grid = 0
!      runoffWaterBody_grid = 0
      surplusWaterBody = 0

      ! surf_chang_grid = 0
      runoff_surf = 0
      state_surf_out = 0
      ! state_grid = 0
      ! NWstate_grid = 0
      ! print *, 'cal_water_storage_surf: entering'

      DO is = 1, nsurf !For each surface in turn
         ! print *, 'cal_water_storage_surf: is = ', is
         IF (SnowFrac_in(is) == 0 .AND. sfr_surf(is) > 0) THEN

            !Surface water balance and soil store updates (can modify ev, updates state_id)
            CALL cal_water_storage( &
               is, sfr_surf, PipeCapacity, RunoffToWater, pin, & ! input:
               WU_surf, &
               drain_surf, AddWater_surf, addImpervious, nsh_real, state_surf_in, frac_water2runoff_surf, &
               PervFraction, addVeg, SoilStoreCap_surf, addWaterBody, FlowChange, StateLimit_surf, &
               runoffAGimpervious_grid, runoffAGveg_grid, runoffPipes_grid, ev_surf(is), soilstore, & ! inout:
               surplusWaterBody, SurplusEvap, runoffWaterBody_grid, & ! inout:
               runoff_surf, state_surf_out) !output:
         END IF

      END DO !end loop over surfaces

      ! update soilstore
      soilstore_surf_out = soilstore

      ! update evaporation
      ev_surf_out = ev_surf
      ! print *, 'cal_water_storage_surf: exiting'

   END SUBROUTINE cal_water_storage_surf

   SUBROUTINE cal_water_storage_building( &
      pin, nsh_real, nlayer, &
      sfr_roof, StateLimit_roof, SoilStoreCap_roof, WetThresh_roof, & ! input:
      ev_roof, state_roof_in, soilstore_roof_in, & ! input:
      sfr_wall, StateLimit_wall, SoilStoreCap_wall, WetThresh_wall, & ! input:
      ev_wall, state_wall_in, soilstore_wall_in, & ! input:
      !       ev_roof_out,
      state_roof_out, soilstore_roof_out, runoff_roof, & ! general output:
      !       ev_wall_out,
      state_wall_out, soilstore_wall_out, runoff_wall, & ! general output:
      state_building, soilstore_building, runoff_building, SoilStoreCap_building)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: nlayer !number of layers
      REAL(KIND(1D0)), INTENT(in) :: pin !Rain per time interval
      REAL(KIND(1D0)), INTENT(in) :: nsh_real !timesteps per hour

      ! input for generic roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: StateLimit_roof !Limit for state_id of each surface type [mm] (specified in input files)
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: WetThresh_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: SoilStoreCap_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_roof_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_roof_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(inout) :: ev_roof

      ! input for generic wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: StateLimit_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: WetThresh_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: SoilStoreCap_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: state_wall_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: soilstore_wall_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(inout) :: ev_wall

      ! output for generic roof facets
!       REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: ev_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: state_roof_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: soilstore_roof_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: runoff_roof

      ! output for generic wall facets
!       REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: ev_wall_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: state_wall_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: soilstore_wall_out
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: runoff_wall

      REAL(KIND(1D0)), INTENT(out) :: state_building !aggregated surface water of building facets [mm]
      REAL(KIND(1D0)), INTENT(out) :: soilstore_building ! aggregated soilstore of building facets[mm]
      REAL(KIND(1D0)), INTENT(out) :: runoff_building !aggregated Runoff of building facets [mm]
      REAL(KIND(1D0)), INTENT(out) :: SoilStoreCap_building

      REAL(KIND(1D0)) :: precip_excess_roof !precipitation excess above IPThreshold_mmhr [mm]
      REAL(KIND(1D0)) :: precip_excess_wall !precipitation excess above limit [mm]
      REAL(KIND(1D0)) :: pin_wall !input water to wall facet [mm]

      REAL(KIND(1D0)), DIMENSION(nlayer) :: chang_roof !Runoff from each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: chang_wall !Runoff from each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: drain_roof !drainage to sewer [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: drain_wall !drainage to sewer [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: infil_roof !infiltration to replenish soil water [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: infil_wall !infiltration to replenish soil water [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: evap_roof !evapotranpiration from each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nlayer) :: evap_wall !evapotranpiration from each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore !Soil moisture of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: chang !Change in state_id [mm]
      ! REAL(KIND(1D0)), DIMENSION(2) :: SurplusEvap !Surplus for evaporation in 5 min timestep

      !Threshold for intense precipitation [mm hr-1]
      REAL(KIND(1D0)), PARAMETER :: IPThreshold_mmhr = 10 ! NB:this should be an input and can be specified. SG 25 Apr 2018
      REAL(KIND(1D0)) :: IPThreshold ! NB:this should be an input and can be specified. SG 25 Apr 2018

      INTEGER :: n_layer, i_layer

      ! Threshold for intense precipitation in each timestep
      IPThreshold = IPThreshold_mmhr/nsh_real

      ! precipitation excess above IPThreshold_mmhr
      precip_excess_roof = pin - IPThreshold

      ! n_layer = SIZE(sfr_roof)
      DO i_layer = 1, nlayer

         ! ===== roof part =====
         ! initialize temporary variables
         runoff_roof(i_layer) = 0.0
         chang_roof(i_layer) = 0.0

         ! let's assume so for now
         ! TODO: introduce a more sophisticated drinage function for green roofs
         drain_roof(i_layer) = state_roof_in(i_layer)*.0

         ! infiltration to replenish soil water
         infil_roof(i_layer) = state_roof_in(i_layer)*.3

         IF (precip_excess_roof > 0) THEN
            ! runoff generated from roof
            runoff_roof(i_layer) = precip_excess_roof
            chang_roof(i_layer) = IPThreshold - ev_roof(i_layer) - drain_roof(i_layer) - infil_roof(i_layer)
         ELSE
            chang_roof(i_layer) = pin - ev_roof(i_layer) - drain_roof(i_layer) - infil_roof(i_layer)
         END IF

         ! change in surface water
         state_roof_out(i_layer) = state_roof_in(i_layer) + chang_roof(i_layer)

         ! Check state_id is within physical limits between zero (dry) and max. storage capacity
         IF (state_roof_out(i_layer) < 0.0) THEN ! Cannot have a negative surface state_id
            ! If there is not sufficient water on the surface, then remove water from soilstore
            ! Allow evaporation until soilstore_id is depleted and surface is dry
            IF ((soilstore_roof_in(i_layer) + state_roof_out(i_layer)) >= 0) THEN
               ! update soilstore with deficit in surface state
               soilstore_roof_out(i_layer) = soilstore_roof_in(i_layer) + state_roof_out(i_layer)
               ! If there is not sufficient water on the surface or soilstore, then don't allow this evaporation to happen
            ELSE
               ev_roof(i_layer) = ev_roof(i_layer) - ABS(state_roof_out(i_layer)) !Limit evaporation according to water availability
            END IF
            ! force surface to dry
            state_roof_out(i_layer) = 0.0
         ELSE
            ! If there is sufficient water on the surface, then allow surface water to replenish soilstore
            IF (soilstore_roof_in(i_layer) + infil_roof(i_layer) > SoilStoreCap_roof(i_layer)) THEN
               ! cap soilstore_id at max. storage capacity if saturated
               soilstore_roof_out(i_layer) = SoilStoreCap_roof(i_layer)

               ! excessive infiltration goes into runoff
               runoff_roof = runoff_roof + (soilstore_roof_in(i_layer) + infil_roof(i_layer) - SoilStoreCap_roof(i_layer))
            ELSE
               soilstore_roof_out(i_layer) = soilstore_roof_in(i_layer) + infil_roof(i_layer)
            END IF

         END IF

         ! ===== wall part =====
         ! initialize temporary variables
         runoff_wall(i_layer) = 0.0
         chang_wall(i_layer) = 0.0
         pin_wall = 0.0

         ! runoff from roof goes into wall as water supply
         ! NB: only a fraction of precipitation is diverted to the wall
         pin_wall = runoff_roof(i_layer) + pin*.2
         precip_excess_wall = pin_wall - StateLimit_wall(i_layer)

         ! let's assume so for now
         ! TODO: introduce a more sophisticated drinage function for green roofs
         drain_wall(i_layer) = state_wall_in(i_layer)*.0

         ! infiltration to replenish soil water
         infil_wall(i_layer) = state_wall_in(i_layer)*.1

         IF (precip_excess_wall > 0) THEN
            ! runoff generated from roof
            runoff_wall(i_layer) = precip_excess_wall
            chang_wall(i_layer) = StateLimit_wall(i_layer) - ev_wall(i_layer) - drain_wall(i_layer) - infil_wall(i_layer)
         ELSE
            chang_wall(i_layer) = pin_wall - ev_wall(i_layer) - drain_wall(i_layer) - infil_wall(i_layer)
         END IF

         ! change in surface water
         state_wall_out(i_layer) = state_wall_in(i_layer) + chang_wall(i_layer)

         ! Check state_id is within physical limits between zero (dry) and max. storage capacity
         IF (state_wall_out(i_layer) < 0.0) THEN ! Cannot have a negative surface state_id
            ! If there is not sufficient water on the surface, then remove water from soilstore
            ! Allow evaporation until soilstore_id is depleted and surface is dry
            IF ((soilstore_wall_in(i_layer) + state_wall_out(i_layer)) >= 0) THEN
               ! update soilstore with deficit in surface state
               soilstore_wall_out(i_layer) = soilstore_wall_in(i_layer) + state_wall_out(i_layer)
               ! If there is not sufficient water on the surface or soilstore, then don't allow this evaporation to happen
            ELSE
               ev_wall(i_layer) = ev_wall(i_layer) - ABS(state_wall_out(i_layer)) !Limit evaporation according to water availability
            END IF
            ! force surface to dry
            state_wall_out(i_layer) = 0.0
         ELSE
            ! If there is sufficient water on the surface, then allow surface water to replenish soilstore
            IF (soilstore_wall_in(i_layer) + infil_wall(i_layer) > SoilStoreCap_wall(i_layer)) THEN
               ! cap soilstore_id at max. storage capacity if saturated
               soilstore_wall_out(i_layer) = SoilStoreCap_wall(i_layer)

               ! excessive infiltration goes into runoff
               runoff_wall(i_layer) = &
                  runoff_wall(i_layer) + &
                  (soilstore_wall_in(i_layer) + infil_wall(i_layer) - SoilStoreCap_wall(i_layer))
            ELSE
               soilstore_wall_out(i_layer) = soilstore_wall_in(i_layer) + infil_wall(i_layer)
            END IF

         END IF

      END DO

      ! diagnostics info:
      ! call r8vec_print(SIZE(soilstore_roof_out), soilstore_roof_in, 'soilstore_roof_in in layer')
      ! call r8vec_print(SIZE(soilstore_roof_out), SoilStoreCap_roof, 'SoilStoreCap_roof in layer')
      ! call r8vec_print(SIZE(soilstore_roof_out), infil_roof, 'infil_roof in layer')
      ! call r8vec_print(SIZE(soilstore_wall_out), soilstore_roof_out, 'soilstore_roof_out in layer')

      ! aggregated values
      state_building = DOT_PRODUCT(state_roof_out, sfr_roof) + DOT_PRODUCT(state_wall_out, sfr_wall)
      soilstore_building = DOT_PRODUCT(soilstore_roof_out, sfr_roof) + DOT_PRODUCT(soilstore_wall_out, sfr_wall)
      SoilStoreCap_building = DOT_PRODUCT(SoilStoreCap_roof, sfr_roof) + DOT_PRODUCT(SoilStoreCap_wall, sfr_wall)
      ! only allow runoff from walls
      runoff_building = DOT_PRODUCT(runoff_wall, sfr_wall)

   END SUBROUTINE cal_water_storage_building

   !------------------------------------------------------------------------------
   SUBROUTINE updateFlood( &
      is, runoff, & ! input:
      sfr_surf, PipeCapacity, RunoffToWater, &
      runoffAGimpervious, surplusWaterBody, runoffAGveg, runoffPipes) ! inout:

      IMPLICIT NONE

      INTEGER, INTENT(in) :: is
      REAL(KIND(1D0)), INTENT(in) :: sfr_surf(nsurf), runoff(nsurf), PipeCapacity, RunoffToWater
      REAL(KIND(1D0)), INTENT(inout) :: runoffAGimpervious, surplusWaterBody, runoffAGveg, runoffPipes

      ! Add runoff to pipes
      runoffPipes = runoffPipes + (runoff(is)*sfr_surf(is))

      ! If pipe capacity is full, surface runoff occurs
      ! N.B. this will happen each loop (replicates pipes filling up)
      IF (runoffPipes > PipeCapacity) THEN

         !------Paved and building surface
         IF (is == PavSurf .OR. is == BldgSurf) THEN
            IF (sfr_surf(WaterSurf) > 0.0000001) THEN
               ! If there is some water present, the water surface will take some of the flood water (fraction RunoffToWater)
               ! RunoffToWater is specified in SUEWS_SiteSelect.txt
               runoffAGimpervious = runoffAGimpervious + (runoffPipes - PipeCapacity)*(1 - RunoffToWater)
               surplusWaterBody = surplusWaterBody + (runoffPipes - PipeCapacity)*RunoffToWater
            ELSE
               ! Otherwise, all flood water must go to runoff
               runoffAGimpervious = runoffAGimpervious + (runoffPipes - PipeCapacity)
            END IF
            !------other surfaces
         ELSEIF (is >= ConifSurf .AND. is <= BSoilSurf) THEN
            IF (sfr_surf(WaterSurf) > 0.0000001) THEN
               ! If there is some water present, the water surface will take some of the flood water (fraction RunoffToWater)
               runoffAGveg = runoffAGveg + (runoffPipes - PipeCapacity)*(1 - RunoffToWater)
               surplusWaterBody = surplusWaterBody + (runoffPipes - PipeCapacity)*RunoffToWater
            ELSE
               ! Otherwise, all flood water must go to runoff
               runoffAGveg = runoffAGveg + (runoffPipes - PipeCapacity)
            END IF
         END IF

         runoffPipes = PipeCapacity !Pipes are at their max capacity

      END IF !If runoff exceed pipe capacity

   END SUBROUTINE updateFlood
   !------------------------------------------------------------------------------

   !------------------------------------------------------------------------------
   SUBROUTINE ReDistributeWater( &
      SnowUse, WaterDist, sfr_surf, Drain, & ! input:
      AddWaterRunoff, AddWater) ! output:
      !Drainage moves into different parts defined by WaterDistSS_YYYY.txt. LJ 2010
      !AddWater(is) is that amount of water that is gained for each surface
      !Latest update takes snow into account. 22/03/2013 LJ
      !-------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER, INTENT(in) :: SnowUse !Snow part used (1) or not used (0)

      REAL(KIND(1D0)), INTENT(in) :: WaterDist(nsurf + 1, nsurf - 1) !Within-grid water distribution to other surfaces and runoff/soil store [-]
      REAL(KIND(1D0)), INTENT(in) :: sfr_surf(nsurf) !Surface fractions [-]
      REAL(KIND(1D0)), INTENT(in) :: Drain(nsurf) !Drainage of each surface type [mm]

      REAL(KIND(1D0)), INTENT(out) :: AddWaterRunoff(nsurf) !Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
      REAL(KIND(1D0)), INTENT(out) :: AddWater(nsurf) !Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]

      INTEGER :: i_receiver, i_contributor, i_surface
      INTEGER :: NSurfDoNotReceiveDrainage = 0 !Number of surfaces that do not receive drainage water (green roof)

      !Fractions that go to runoff from each surface
      DO i_surface = 1, nsurf - 1 !not water in the calculation
         AddWaterRunoff(i_surface) = WaterDist(8, i_surface)
      END DO
      AddWaterRunoff(WaterSurf) = 0
      AddWater = 0

      DO i_receiver = 1, nsurf - NSurfDoNotReceiveDrainage !go through surfaces from 1 to 7. These gain water through drainage
         DO i_contributor = 1, nsurf - (NSurfDoNotReceiveDrainage + 1) !From where surface ii can gain water - can't gain water from itself

            IF (sfr_surf(i_receiver) /= 0) THEN !Water movement takes place only if surface fraction exists

               IF (SnowUse == 0) THEN
                  !No snow calculations!
                  AddWater(i_receiver) = AddWater(i_receiver) + &
                                         (Drain(i_contributor)*sfr_surf(i_contributor) &
                                          /sfr_surf(i_receiver))*WaterDist(i_receiver, i_contributor) !Original

               ELSE
                  !Snow included, This needs to be fixed at some point. LJ Mar 2013
                  AddWaterRunoff(i_contributor) = AddWaterRunoff(i_contributor) &
                                                  + WaterDist(i_receiver, i_contributor) !No receiving surface -> runoff
               END IF

            ELSE
               !If no receiving surface exists, water fraction goes to AddWaterRunoff
               AddWaterRunoff(i_contributor) = AddWaterRunoff(i_contributor) &
                                               + WaterDist(i_receiver, i_contributor)
            END IF
         END DO
      END DO

   END SUBROUTINE ReDistributeWater
   !------------------------------------------------------------------------------

   !------------------------------------------------------------------------------
   SUBROUTINE SUEWS_update_SoilMoist( &
      NonWaterFraction, & !input
      SoilStoreCap, sfr_surf, soilstore_id, &
      SoilMoistCap, SoilState, & !output
      vsmd, smd)
      IMPLICIT NONE

      ! INTEGER,INTENT(in)::nsurf,ConifSurf,DecidSurf,GrassSurf
      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
      REAL(KIND(1D0)), INTENT(in), DIMENSION(nsurf) :: SoilStoreCap, sfr_surf, soilstore_id

      REAL(KIND(1D0)), INTENT(out) :: SoilMoistCap, SoilState
      REAL(KIND(1D0)), INTENT(out) :: vsmd, smd

      INTEGER :: is
      REAL(KIND(1D0)) :: fveg

      SoilMoistCap = 0 !Maximum capacity of soil store [mm] for whole surface
      SoilState = 0 !Area-averaged soil moisture [mm] for whole surface

      IF (NonWaterFraction /= 0) THEN !Soil states only calculated if soil exists. LJ June 2017
         DO is = 1, nsurf - 1 !No water body included
            SoilMoistCap = SoilMoistCap + (SoilStoreCap(is)*sfr_surf(is)/NonWaterFraction)
            SoilState = SoilState + (soilstore_id(is)*sfr_surf(is)/NonWaterFraction)
         END DO
      END IF

      !If loop removed HCW 26 Feb 2015
      !if (ir==1) then  !Calculate initial smd
      smd = SoilMoistCap - SoilState
      !endif

      ! Calculate soil moisture for vegetated surfaces only (for use in surface conductance)
      ! vsmd = 0
      ! IF ((sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf)) > 0) THEN

      !    fveg = sfr_surf(is)/(sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf))
      ! ELSE
      !    fveg = 0
      ! END IF
      ! DO is = ConifSurf, GrassSurf !Vegetated surfaces only
      !    IF (fveg == 0) THEN
      !       vsmd = 0
      !    ELSE
      !       vsmd = vsmd + (SoilStoreCap(is) - soilstore_id(is))*sfr_surf(is)/fveg
      !    END IF
      !    !write(*,*) is, vsmd, smd
      ! END DO

      vsmd = cal_smd_veg(SoilStoreCap, soilstore_id, sfr_surf)

   END SUBROUTINE SUEWS_update_SoilMoist

   ! SUBROUTINE SUEWS_update_SoilMoist_DTS( &
   !    NonWaterFraction, &
   !    sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, & !input
   !    SoilStoreCap_paved, SoilStoreCap_bldg, SoilStoreCap_evetr, &
   !    SoilStoreCap_dectr, SoilStoreCap_grass, SoilStoreCap_bsoil, SoilStoreCap_water, &
   !    soilstore_id, &
   !    SoilMoistCap, SoilState, & !output
   !    vsmd, smd)
   !    IMPLICIT NONE

   !    ! INTEGER,INTENT(in)::nsurf,ConifSurf,DecidSurf,GrassSurf
   !    REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
   !    REAL(KIND(1D0)), INTENT(in), DIMENSION(nsurf) :: soilstore_id

   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_water
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf

   !    REAL(KIND(1D0)), INTENT(IN) :: SoilStoreCap_paved
   !    REAL(KIND(1D0)), INTENT(IN) :: SoilStoreCap_bldg
   !    REAL(KIND(1D0)), INTENT(IN) :: SoilStoreCap_evetr
   !    REAL(KIND(1D0)), INTENT(IN) :: SoilStoreCap_dectr
   !    REAL(KIND(1D0)), INTENT(IN) :: SoilStoreCap_grass
   !    REAL(KIND(1D0)), INTENT(IN) :: SoilStoreCap_bsoil
   !    REAL(KIND(1D0)), INTENT(IN) :: SoilStoreCap_water
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilStoreCap

   !    REAL(KIND(1D0)), INTENT(out) :: SoilMoistCap, SoilState
   !    REAL(KIND(1D0)), INTENT(out) :: vsmd, smd

   !    INTEGER :: is
   !    REAL(KIND(1D0)) :: fveg

   !    sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]
   !    SoilStoreCap(1) = SoilStoreCap_paved
   !    SoilStoreCap(2) = SoilStoreCap_bldg
   !    SoilStoreCap(3) = SoilStoreCap_evetr
   !    SoilStoreCap(4) = SoilStoreCap_dectr
   !    SoilStoreCap(5) = SoilStoreCap_grass
   !    SoilStoreCap(6) = SoilStoreCap_bsoil
   !    SoilStoreCap(7) = SoilStoreCap_water

   !    SoilMoistCap = 0 !Maximum capacity of soil store [mm] for whole surface
   !    SoilState = 0 !Area-averaged soil moisture [mm] for whole surface

   !    IF (NonWaterFraction /= 0) THEN !Soil states only calculated if soil exists. LJ June 2017
   !       DO is = 1, nsurf - 1 !No water body included
   !          SoilMoistCap = SoilMoistCap + (SoilStoreCap(is)*sfr_surf(is)/NonWaterFraction)
   !          SoilState = SoilState + (soilstore_id(is)*sfr_surf(is)/NonWaterFraction)
   !       END DO
   !    END IF

   !    !If loop removed HCW 26 Feb 2015
   !    !if (ir==1) then  !Calculate initial smd
   !    smd = SoilMoistCap - SoilState
   !    !endif

   !    ! Calculate soil moisture for vegetated surfaces only (for use in surface conductance)
   !    ! vsmd = 0
   !    ! IF ((sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf)) > 0) THEN

   !    !    fveg = sfr_surf(is)/(sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf))
   !    ! ELSE
   !    !    fveg = 0
   !    ! END IF
   !    ! DO is = ConifSurf, GrassSurf !Vegetated surfaces only
   !    !    IF (fveg == 0) THEN
   !    !       vsmd = 0
   !    !    ELSE
   !    !       vsmd = vsmd + (SoilStoreCap(is) - soilstore_id(is))*sfr_surf(is)/fveg
   !    !    END IF
   !    !    !write(*,*) is, vsmd, smd
   !    ! END DO

   !    vsmd = cal_smd_veg(SoilStoreCap, soilstore_id, sfr_surf)

   ! END SUBROUTINE SUEWS_update_SoilMoist_DTS

   SUBROUTINE SUEWS_update_SoilMoist_DTS( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:
      ! hydroState)

      USE SUEWS_DEF_DTS, ONLY: SUEWS_SITE, SUEWS_TIMER, SUEWS_CONFIG, SUEWS_FORCING, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               HYDRO_STATE, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      ! INTEGER,INTENT(in)::nsurf,ConifSurf,DecidSurf,GrassSurf
      ! REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
      TYPE(SUEWS_STATE), INTENT(inout) :: modState
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_surf

      ! TYPE(LC_PAVED_PRM), INTENT(IN) :: pavedPrm
      ! TYPE(LC_BLDG_PRM), INTENT(IN) :: bldgPrm
      ! TYPE(LC_EVETR_PRM), INTENT(IN) :: evetrPrm
      ! TYPE(LC_DECTR_PRM), INTENT(IN) :: dectrPrm
      ! TYPE(LC_GRASS_PRM), INTENT(IN) :: grassPrm
      ! TYPE(LC_BSOIL_PRM), INTENT(IN) :: bsoilPrm
      ! TYPE(LC_WATER_PRM), INTENT(IN) :: waterPrm
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf

      REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilStoreCap

      ! REAL(KIND(1D0)), INTENT(out) :: SoilMoistCap, SoilState
      ! REAL(KIND(1D0)), INTENT(out) :: vsmd, smd

      INTEGER :: is
      ! REAL(KIND(1D0)) :: fveg

      ASSOCIATE (hydroState => modState%hydroState)

         ASSOCIATE ( &
            sfr_surf => siteInfo%sfr_surf, &
            NonWaterFraction => siteInfo%NonWaterFraction, &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            evetrPrm => siteInfo%lc_evetr, &
            dectrPrm => siteInfo%lc_dectr, &
            grassPrm => siteInfo%lc_grass, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            SoilMoistCap => hydroState%SoilMoistCap, &
            SoilState => hydroState%SoilState, &
            vsmd => hydroState%vsmd, &
            smd => hydroState%smd)

            soilstore_surf = hydroState%soilstore_surf

            ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
            SoilStoreCap(1) = pavedPrm%soil%soilstorecap
            SoilStoreCap(2) = bldgPrm%soil%soilstorecap
            SoilStoreCap(3) = evetrPrm%soil%soilstorecap
            SoilStoreCap(4) = dectrPrm%soil%soilstorecap
            SoilStoreCap(5) = grassPrm%soil%soilstorecap
            SoilStoreCap(6) = bsoilPrm%soil%soilstorecap
            SoilStoreCap(7) = waterPrm%soil%soilstorecap

            SoilMoistCap = 0 !Maximum capacity of soil store [mm] for whole surface
            SoilState = 0 !Area-averaged soil moisture [mm] for whole surface

            IF (NonWaterFraction /= 0) THEN !Soil states only calculated if soil exists. LJ June 2017
               DO is = 1, nsurf - 1 !No water body included
                  SoilMoistCap = SoilMoistCap + (SoilStoreCap(is)*sfr_surf(is)/NonWaterFraction)
                  SoilState = SoilState + (soilstore_surf(is)*sfr_surf(is)/NonWaterFraction)
               END DO
            END IF

            !If loop removed HCW 26 Feb 2015
            !if (ir==1) then  !Calculate initial smd
            smd = SoilMoistCap - SoilState
            !endif

            ! Calculate soil moisture for vegetated surfaces only (for use in surface conductance)
            ! vsmd = 0
            ! IF ((sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf)) > 0) THEN

            !    fveg = sfr_surf(is)/(sfr_surf(ConifSurf) + sfr_surf(DecidSurf) + sfr_surf(GrassSurf))
            ! ELSE
            !    fveg = 0
            ! END IF
            ! DO is = ConifSurf, GrassSurf !Vegetated surfaces only
            !    IF (fveg == 0) THEN
            !       vsmd = 0
            !    ELSE
            !       vsmd = vsmd + (SoilStoreCap(is) - soilstore_id(is))*sfr_surf(is)/fveg
            !    END IF
            !    !write(*,*) is, vsmd, smd
            ! END DO

            vsmd = cal_smd_veg(SoilStoreCap, soilstore_surf, sfr_surf)
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_update_SoilMoist_DTS
   !------------------------------------------------------------------------------

   ! calculate soil moisture deficit for vegetated surfaces only (for use in surface conductance)
   FUNCTION cal_smd_veg(SoilStoreCap, soilstore_id, sfr_surf) RESULT(vsmd)
      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in), DIMENSION(nsurf) :: SoilStoreCap, soilstore_id, sfr_surf
      REAL(KIND(1D0)) :: vsmd

      REAL(KIND(1D0)), DIMENSION(nsurf) :: smd_surf
      REAL(KIND(1D0)), DIMENSION(3) :: surf_veg, smd_veg
      INTEGER :: is

      vsmd = 0

      ! calculate the soil moisture deficit for each vegetated surface
      smd_surf = SoilStoreCap - soilstore_id
      smd_veg = [(smd_surf(is), is=ConifSurf, GrassSurf)]

      ! calculate the fraction of each vegetated surface among all vegetated surfaces
      surf_veg = [(sfr_surf(is), is=ConifSurf, GrassSurf)]
      surf_veg = surf_veg/SUM(surf_veg)

      ! calculate the weighted soil moisture deficit for vegetated surfaces
      vsmd = DOT_PRODUCT(smd_veg, surf_veg)

   END FUNCTION cal_smd_veg

   !========== Calculate soil moisture of a whole grid ============
   SUBROUTINE SUEWS_cal_SoilState( &
      SMDMethod, xsmd, NonWaterFraction, SoilMoistCap, & !input
      SoilStoreCap_surf, surf_chang_per_tstep, &
      soilstore_surf, soilstore_surf_in, sfr_surf, &
      smd, smd_surf, tot_chang_per_tstep, SoilState) !output

      IMPLICIT NONE
      ! INTEGER, PARAMETER :: nsurf = 7

      INTEGER, INTENT(in) :: SMDMethod
      REAL(KIND(1D0)), INTENT(in) :: xsmd
      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction
      REAL(KIND(1D0)), INTENT(in) :: SoilMoistCap

      REAL(KIND(1D0)), INTENT(in) :: surf_chang_per_tstep
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_surf !Soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: soilstore_surf_in !Soil moisture of each surface type from previous timestep [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: smd_surf !smd for each surface
      REAL(KIND(1D0)), INTENT(out) :: SoilState !Area-averaged soil moisture [mm] for whole surface
      REAL(KIND(1D0)), INTENT(out) :: smd !One value for whole surface
      REAL(KIND(1D0)), INTENT(out) :: tot_chang_per_tstep !Change in surface state_id

      REAL(KIND(1D0)), PARAMETER :: NotUsed = -999
      REAL(KIND(1D0)), PARAMETER :: NAN = -999
      INTEGER :: is

      SoilState = 0 !Area-averaged soil moisture [mm] for whole surface
      IF (NonWaterFraction /= 0) THEN !Fixed for water surfaces only
         DO is = 1, nsurf - 1 !No water body included
            SoilState = SoilState + (soilstore_surf(is)*sfr_surf(is)/NonWaterFraction)
            IF (SoilState < 0) THEN
               CALL ErrorHint(62, 'SUEWS_Calculations: total SoilState < 0 (just added surface is) ', SoilState, NotUsed, is)
            ELSEIF (SoilState > SoilMoistCap) THEN
               CALL ErrorHint(62, 'SUEWS_Calculations: total SoilState > capacity (just added surface is) ', SoilState, NotUsed, is)
               !SoilMoist_state=SoilMoistCap !What is this LJ 10/2010 - QUESTION: SM exceeds capacity, but where does extra go?HCW 11/2014
            END IF
         END DO !end loop over surfaces
         ! SoilState = DOT_PRODUCT(soilstore_id(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction
         ! IF (SoilState < 0) THEN
         !    CALL ErrorHint(62, 'SUEWS_Calculations: total SoilState < 0 (just added surface is) ', SoilState, NotUsed, is)
         ! ELSEIF (SoilState > SoilMoistCap) THEN
         !    CALL ErrorHint(62, 'SUEWS_Calculations: total SoilState > capacity (just added surface is) ', SoilState, NotUsed, is)
         !    !SoilMoist_state=SoilMoistCap !What is this LJ 10/2010 - QUESTION: SM exceeds capacity, but where does extra go?HCW 11/2014
         ! ENDIF
      END IF

      ! Calculate soil moisture deficit
      smd = SoilMoistCap - SoilState !One value for whole surface
      smd_surf = SoilStoreCap_surf - soilstore_surf !smd for each surface

      ! Soil stores can change after horizontal water movements
      ! Calculate total change in surface and soil state_id
      tot_chang_per_tstep = surf_chang_per_tstep !Change in surface state_id
      DO is = 1, (nsurf - 1) !No soil for water surface (so change in soil moisture is zero)
         tot_chang_per_tstep = tot_chang_per_tstep + ((soilstore_surf(is) - soilstore_surf_in(is))*sfr_surf(is)) !Add change in soil state_id
      END DO

      IF (SMDMethod > 0) THEN ! use observed value
         !  smd_nsurf=NAN
         smd_surf = NAN
         smd = xsmd
      END IF

   END SUBROUTINE SUEWS_cal_SoilState

   ! SUBROUTINE SUEWS_cal_SoilState_DTS( &
   !    timer, config, forcing, siteInfo, & ! input
   !    hydroState, hydroState_prev) !output

   !    USE SUEWS_DEF_DTS, ONLY: SUEWS_CONFIG, SUEWS_FORCING, SUEWS_TIMER, SUEWS_SITE, &
   !                             LC_PAVED_PRM, LC_BLDG_PRM, &
   !                             LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
   !                             LC_BSOIL_PRM, LC_WATER_PRM, HYDRO_STATE

   !    IMPLICIT NONE

   !    TYPE(SUEWS_CONFIG), INTENT(IN) :: config
   !    TYPE(SUEWS_TIMER), INTENT(IN) :: timer
   !    TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
   !    TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
   !    ! INTEGER, PARAMETER :: nsurf = 7

   !    TYPE(HYDRO_STATE), INTENT(INout) :: hydroState, hydroState_prev

   !    ! REAL(KIND(1D0)), INTENT(in) :: SoilMoistCap

   !    ! REAL(KIND(1D0)), INTENT(in) :: surf_chang_per_tstep
   !    ! REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id !Soil moisture of each surface type [mm]
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_surf_in !Soil moisture of each surface type from previous timestep [mm]

   !    ! REAL(KIND(1D0)), DIMENSION(NSURF) :: sfr_surf !surface fraction [-]
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]

   !    ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: smd_nsurf !smd for each surface
   !    ! REAL(KIND(1D0)), INTENT(out) :: SoilState !Area-averaged soil moisture [mm] for whole surface
   !    ! REAL(KIND(1D0)), INTENT(out) :: smd !One value for whole surface
   !    ! REAL(KIND(1D0)), INTENT(out) :: tot_chang_per_tstep !Change in surface state_id

   !    REAL(KIND(1D0)), PARAMETER :: NotUsed = -999
   !    REAL(KIND(1D0)), PARAMETER :: NAN = -999
   !    INTEGER :: is
   !    ASSOCIATE ( &
   !       pavedPrm => siteInfo%lc_paved, &
   !       bldgPrm => siteInfo%lc_bldg, &
   !       evetrPrm => siteInfo%lc_evetr, &
   !       dectrPrm => siteInfo%lc_dectr, &
   !       grassPrm => siteInfo%lc_grass, &
   !       bsoilPrm => siteInfo%lc_bsoil, &
   !       waterPrm => siteInfo%lc_water, &
   !       ehcPrm => siteInfo%ehc, &
   !       sfr_surf => siteInfo%sfr_surf, &
   !       sfr_roof => siteInfo%sfr_roof, &
   !       sfr_wall => siteInfo%sfr_wall, &
   !       SurfaceArea => siteInfo%SurfaceArea, &
   !       snowPrm => siteInfo%snow, &
   !       PipeCapacity => siteInfo%PipeCapacity, &
   !       RunoffToWater => siteInfo%RunoffToWater, &
   !       FlowChange => siteInfo%FlowChange, &
   !       PervFraction => siteInfo%PervFraction, &
   !       vegfraction => siteInfo%vegfraction, &
   !       NonWaterFraction => siteInfo%NonWaterFraction, &
   !       SoilMoistCap => hydroState%SoilMoistCap, &
   !       SoilState => hydroState%SoilState, &
   !       tot_chang_per_tstep => hydroState%tot_chang_per_tstep, &
   !       smd_surf => hydroState%smd_surf, &
   !       smd => hydroState%smd, &
   !       surf_chang_per_tstep => hydroState%surf_chang_per_tstep, &
   !       soilstore_surf => hydroState%soilstore_surf, &
   !       tstep_real => timer%tstep_real, &
   !       xsmd => forcing%xsmd, &
   !       SMDMethod => config%SMDMethod, &
   !       Diagnose => config%Diagnose &
   !       )

   !       ! SMDMethod = config%SMDMethod

   !       ! xsmd = forcing%xsmd

   !       soilstore_surf_in = hydroState_prev%soilstore_surf

   !       ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]

   !       SoilStoreCap_surf(1) = pavedPrm%soil%soilstorecap
   !       SoilStoreCap_surf(2) = bldgPrm%soil%soilstorecap
   !       SoilStoreCap_surf(3) = evetrPrm%soil%soilstorecap
   !       SoilStoreCap_surf(4) = dectrPrm%soil%soilstorecap
   !       SoilStoreCap_surf(5) = grassPrm%soil%soilstorecap
   !       SoilStoreCap_surf(6) = bsoilPrm%soil%soilstorecap
   !       SoilStoreCap_surf(7) = waterPrm%soil%soilstorecap

   !       CALL SUEWS_cal_SoilState_meta( &
   !          SMDMethod, xsmd, NonWaterFraction, SoilMoistCap, & !input
   !          SoilStoreCap_surf, surf_chang_per_tstep, &
   !          soilstore_surf, soilstore_surf_in, sfr_surf, &
   !          smd, smd_surf, tot_chang_per_tstep, SoilState) !output

   !       !    SoilState = 0 !Area-averaged soil moisture [mm] for whole surface
   !       !    IF (NonWaterFraction /= 0) THEN !Fixed for water surfaces only
   !       !       DO is = 1, nsurf - 1 !No water body included
   !       !          SoilState = SoilState + (soilstore_surf(is)*sfr_surf(is)/NonWaterFraction)
   !       !          IF (SoilState < 0) THEN
   !       !             CALL ErrorHint(62, 'SUEWS_Calculations: total SoilState < 0 (just added surface is) ', SoilState, NotUsed, is)
   !       !          ELSEIF (SoilState > SoilMoistCap) THEN
   !       !          CALL ErrorHint(62, 'SUEWS_Calculations: total SoilState > capacity (just added surface is) ', SoilState, NotUsed, is)
   !       !             !SoilMoist_state=SoilMoistCap !What is this LJ 10/2010 - QUESTION: SM exceeds capacity, but where does extra go?HCW 11/2014
   !       !          END IF
   !       !       END DO !end loop over surfaces
   !       !       ! SoilState = DOT_PRODUCT(soilstore_id(1:nsurf - 1), sfr_surf(1:nsurf - 1))/NonWaterFraction
   !       !       ! IF (SoilState < 0) THEN
   !       !       !    CALL ErrorHint(62, 'SUEWS_Calculations: total SoilState < 0 (just added surface is) ', SoilState, NotUsed, is)
   !       !       ! ELSEIF (SoilState > SoilMoistCap) THEN
   !       !       !    CALL ErrorHint(62, 'SUEWS_Calculations: total SoilState > capacity (just added surface is) ', SoilState, NotUsed, is)
   !       !       !    !SoilMoist_state=SoilMoistCap !What is this LJ 10/2010 - QUESTION: SM exceeds capacity, but where does extra go?HCW 11/2014
   !       !       ! ENDIF
   !       !    END IF

   !       !    ! Calculate soil moisture deficit
   !       !    smd = SoilMoistCap - SoilState !One value for whole surface
   !       !    smd_nsurf = SoilStoreCap_surf - soilstore_surf !smd for each surface

   !       !    ! Soil stores can change after horizontal water movements
   !       !    ! Calculate total change in surface and soil state_id
   !       !    tot_chang_per_tstep = surf_chang_per_tstep !Change in surface state_id
   !       !    DO is = 1, (nsurf - 1) !No soil for water surface (so change in soil moisture is zero)
   !       !       tot_chang_per_tstep = tot_chang_per_tstep + ((soilstore_surf(is) - soilstore_surf_in(is))*sfr_surf(is)) !Add change in soil state_id
   !       !    END DO

   !       !    IF (SMDMethod > 0) THEN ! use observed value
   !       !       !  smd_nsurf=NAN
   !       !       smd_nsurf = NAN
   !       !       smd = xsmd
   !       !    END IF

   !    END ASSOCIATE

   ! END SUBROUTINE SUEWS_cal_SoilState_DTS
   !===================================================================================

   SUBROUTINE SUEWS_cal_HorizontalSoilWater( &
      sfr_surf, & ! input: ! surface fractions
      SoilStoreCap_surf, & !Capacity of soil store for each surface [mm]
      SoilDepth_surf, & !Depth of sub-surface soil store for each surface [mm]
      SatHydraulicConduct_surf, & !Saturated hydraulic conductivity for each soil subsurface [mm s-1]
      SurfaceArea, & !Surface area of the study area [m2]
      NonWaterFraction, & ! sum of surface cover fractions for all except water surfaces
      tstep_real, & !tstep cast as a real for use in calculations
      soilstore_surf, & ! inout: !Soil moisture of each surface type [mm]
      runoffSoil_surf, & !Soil runoff from each soil sub-surface [mm]
      runoffSoil_per_tstep & !  output:!Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
      )
      !Transfers water in soil stores of land surfaces LJ (2010)
      !Change the model to use varying hydraulic conductivity instead of constant value LJ (7/2011)
      !If one of the surface's soildepth is zero, no water movement is considered
      ! LJ  15/06/2017 Modification:   - Moved location of runoffSoil_per_tstep within previous if-loop to avoid dividing with zero with 100% water surface
      ! HCW 22/02/2017 Modifications:  - Minor bug fixed in VWC1/B_r1 comparison - if statements reversed
      ! HCW 13/08/2014 Modifications:  - Order of surfaces reversed (for both is and jj loops)
      !                                - Number of units (e.g. properties) added to distance calculation
      ! HCW 12/08/2014 Modifications:  - Distance changed from m to mm in dI_dt calculation
      !                                - dI_dt [mm s-1] multiplied by no. seconds in timestep -> dI [mm]
      !                                - if MatPot is set to max. value (100000 mm), Km set to 0 mm s-1
      !                                - Provide parameters for residual volumetric soil moisture [m3 m-3]
      !                                   (currently hard coded as 0.1 m3 m-3 for testing)
      !
      !------------------------------------------------------
      ! use SUES_data
      ! use gis_data
      ! use time
      ! use allocateArray

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(in) :: sfr_surf(nsurf) ! surface fractions
      REAL(KIND(1D0)), INTENT(in) :: SoilStoreCap_surf(nsurf) !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), INTENT(in) :: SoilDepth_surf(nsurf) !Depth of sub-surface soil store for each surface [mm]
      REAL(KIND(1D0)), INTENT(in) :: SatHydraulicConduct_surf(nsurf) !Saturated hydraulic conductivity for each soil subsurface [mm s-1]
      REAL(KIND(1D0)), INTENT(in) :: SurfaceArea !Surface area of the study area [m2]
      REAL(KIND(1D0)), INTENT(in) :: NonWaterFraction ! sum of surface cover fractions for all except water surfaces
      REAL(KIND(1D0)), INTENT(in) :: tstep_real !tstep cast as a real for use in calculations

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: soilstore_surf !Soil moisture of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: runoffSoil_surf !Soil runoff from each soil sub-surface [mm]

      REAL(KIND(1D0)), INTENT(out) :: runoffSoil_per_tstep !Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)

      INTEGER :: jj, is
      REAL(KIND(1D0)) :: &
         DimenWaterCon1, DimenWaterCon2, &
         SoilMoistCap_Vol1, &
         SoilMoist_vol1, &
         SoilMoistCap_Vol2, &
         SoilMoist_vol2, &
         B_r1, MatPot1, Km1, &
         B_r2, MatPot2, Km2, &
         Distance, KmWeight, dI, &
         dI_dt !Water flow between two stores

      REAL(KIND(1D0)), PARAMETER :: &
         alphavG = 0.0005, & !Set alphavG to match value in van Genuchten (1980) [mm-1]
         NUnits = 1 !Can change to represent plot/base unit size

      ! SoilMoist_vol1,2     = Volumetric soil moisture [m3 m-3]
      ! SoilMoistCap_vol1,2  = Volumetric soil moisture capacity [m3 m-3] (from FunctionalTypes)
      ! MatPot1,2            = Water potential (i.e. pressure head) of store [mm]
      ! DimenWaterCon1,2     = Dimensionless water content, or relative saturation [-]
      ! Distance             = Distance between two stores [m]
      ! B_r1,2               = Residual volumetric soil moisture [m3 m-3]
      ! Km1,2                = Hydraulic conductivity of store [mm s-1]
      ! KmWeight             = Weighted hydraulic conductivity [mm s-1]
      ! alphavG              = Parameter (could depend on soil texture) [mm-1]
      ! dI                   = Water flow between stores [mm] dI = dI_dt * no. secs in each timestep
      !                         if dI > 0, first surface gains water, second surface loses water
      ! NUnits               = Number of repeating units (e.g. properties, blocks) for distance calculation [-]
      runoffSoil_surf = 0
      runoffSoil_per_tstep = 0

      DO is = 1, nsurf - 1 !nsurf-1,1,-1  !Loop through each surface, excluding water surface (runs backwards as of 13/08/2014, HCW)

         IF (sfr_surf(is) /= 0 .AND. SoilStoreCap_surf(is) > 0) THEN !If particular surface area exists
            ! and is capable of storing water (SoilStoreCap [mm])
            DO jj = is + 1, nsurf - 1 !is-1,1,-1  !Sub-loop through remaining surfaces (runs backwards as of 13/08/2014, HCW)

               IF (sfr_surf(jj) /= 0 .AND. SoilStoreCap_surf(jj) > 0) THEN !If other surface area exists
                  ! and is capable of storing water

                  ! ---- For surface 1 -----------------------------------------------------
                  ! Calculate non-saturated VWC
                  SoilMoistCap_Vol1 = SoilStoreCap_surf(is)/SoilDepth_surf(is) !Volumetric soil moisture capacity [m3 m-3] (i.e. saturated VWC)
                  SoilMoist_vol1 = soilstore_surf(is)/SoilDepth_surf(is) !Volumetric soil moisture [m3 m-3]

                  !B_r1=SoilMoistCap_Vol1-SoilMoist_vol1  !Residual soil moisture content [m3 m-3]
                  B_r1 = 0.1 !HCW 12/08/2014 Temporary fix
                  ! Need to add residual soil moisture values to FunctionalTypes
                  !B_r1=VolSoilMoistRes(is) !Residual soil moisture content [m3 m-3]

                  !Order of if statements reversed HCW 22 Feb 2017
                  !If soil moisture less than or equal to residual value, set MatPot to max and Km to 0 to suppress water movement
                  IF (B_r1 >= SoilMoist_vol1) THEN
                     MatPot1 = 100000
                     Km1 = 0 !Added by LJ in Nov 2013
                     ! Otherwise, there should be enough water in the soil to allow horizontal transfer
                  ELSE
                     DimenWaterCon1 = (SoilMoist_vol1 - B_r1)/(SoilMoistCap_Vol1 - B_r1) !Dimensionless water content [-]

                     ! If very large or very small, adjust for calculation of MatPot and Km
                     IF (DimenWaterCon1 > 0.99999) THEN
                        DimenWaterCon1 = DimenWaterCon1 - 0.0001 !This cannot equal 1
                     END IF

                     IF (DimenWaterCon1 < 0.00000005) THEN
                        DimenWaterCon1 = DimenWaterCon1 + 0.0000001 !Added HCW 22 Feb 2017
                     END IF

                     !van Genuchten (1980), with n=2 and m = 1-1/n = 1/2
                     !Water potential of first store [mm] (van Genuchten 1980, Eq 3 rearranged)
                     MatPot1 = SQRT(1/DimenWaterCon1**2 - 1)/alphavG

                     !Hydraulic conductivity of first store [mm s-1] (van Genuchten 1980, Eq 8)
                     Km1 = SatHydraulicConduct_surf(is)*SQRT(DimenWaterCon1)*(1 - (1 - DimenWaterCon1**2)**0.5)**2

                     !Check this value (HCW 12/08/2014)
                     IF (MatPot1 > 100000) THEN
                        MatPot1 = 100000 !Max. potential is 100000 mm (van Genuchten 1980)
                        Km1 = 0 !Added by HCW 12/08/2014
                     END IF

                  END IF

                  ! ---- For surface 2 -----------------------------------------------------
                  ! Calculate non-saturated VWC
                  SoilMoistCap_Vol2 = SoilStoreCap_surf(jj)/SoilDepth_surf(jj) !Volumetric soil moisture capacity [m3 m-3] (i.e. saturated VWC)
                  SoilMoist_vol2 = soilstore_surf(jj)/SoilDepth_surf(jj) !Volumetric soil moisture [m3 m-3]

                  !B_r2=SoilMoistCap_Vol2-SoilMoist_vol2  !Residual soil moisture content [m3 m-3]
                  B_r2 = 0.1 !HCW 12/08/2014 Temporary fix
                  ! Need to add residual soil moisture values to FunctionalTypes
                  !B_r2=VolSoilMoistRes(jj) !Residual soil moisture content [m3 m-3]

                  !If soil moisture below residual value, set MatPot to maximum
                  IF (B_r2 >= SoilMoist_vol2) THEN
                     MatPot2 = 100000
                     Km2 = 0 !Added by LJ in Nov 2013
                  ELSE
                     DimenWaterCon2 = (SoilMoist_vol2 - B_r2)/(SoilMoistCap_Vol2 - B_r2) !Dimensionless water content [-]

                     IF (DimenWaterCon2 > 0.99999) THEN
                        DimenWaterCon2 = DimenWaterCon2 - 0.0001 !This cannot equal 1
                     END IF

                     IF (DimenWaterCon2 < 0.00000005) THEN
                        DimenWaterCon2 = DimenWaterCon2 + 0.0000001 !Added HCW 22 Feb 2017
                     END IF
                     ! print*, 'soilstore_id = ', soilstore_id
                     ! print*, 'SoilStoreCap = ', SoilStoreCap
                     ! print*, 'SoilDepth = ', SoilDepth
                     ! print*, 'SoilMoist_vol2 = ', SoilMoist_vol2
                     ! print*, 'SoilMoistCap_Vol2 = ', SoilMoistCap_Vol2
                     ! print*, 'is = ', is, ' jj = ', jj, ' DimenWaterCon2 = ', DimenWaterCon2
                     !van Genuchten (1980), with n=2 and m = 1-1/n = 1/2
                     !Water potential of second store [mm] (van Genuchten 1980, Eq 3 rearranged)
                     MatPot2 = SQRT(1/DimenWaterCon2**2 - 1)/alphavG

                     !Hydraulic conductivity of second store [mm s-1] (van Genuchten 1980, Eq 8)
                     Km2 = SatHydraulicConduct_surf(jj)*SQRT(DimenWaterCon2)*(1 - (1 - DimenWaterCon2**2)**0.5)**2

                     IF ((MatPot2) > 100000) THEN
                        MatPot2 = 100000 !Max. potential is 100000 mm (van Genuchten 1980)
                        Km2 = 0 !Added by HCW 12/08/2014
                     END IF

                  END IF

                  ! ------------------------------------------------------------------------

                  !Find distance between the two stores (see Jarvi et al. 2011)
                  !SurfaceArea in m2 (changed from ha to m2 n SUEWS_Initial), so Distance in m
                  Distance = (SQRT(sfr_surf(is)*SurfaceArea/NUnits) + SQRT(sfr_surf(jj)*SurfaceArea/NUnits))/2

                  !Calculate areally-weighted hydraulic conductivity [mm s-1]
                  KmWeight = (sfr_surf(is)*Km1 + sfr_surf(jj)*Km2)/(sfr_surf(is) + sfr_surf(jj))

                  !Find water flow between the two stores [mm s-1] (Green-Ampt equation, Hillel 1971)
                  !Multiply Distance by 1000 to convert m to mm (HCW 12/08/2014)
                  dI_dt = -(KmWeight)*(-MatPot1 + MatPot2)/(Distance*1000)

                  !Multiply dI_dt by number of seconds in timestep to convert mm s-1 to mm
                  !Added by HCW 12/08/2014
                  dI = dI_dt*tstep_real !Use dI instead of dI_dt in the following calculations

                  !Move water (in mm) ------------------------------------------------------
                  !Water moves only if (i) there is sufficient water to move and (ii) there is space to move it

                  ! If there is sufficient water in both surfaces, allow movement of dI to occur
                  IF ((soilstore_surf(jj) >= dI*sfr_surf(is)/sfr_surf(jj)) .AND. ((soilstore_surf(is) + dI) >= 0)) THEN
                     soilstore_surf(is) = soilstore_surf(is) + dI
                     soilstore_surf(jj) = soilstore_surf(jj) - dI*sfr_surf(is)/sfr_surf(jj) !Check (HCW 13/08/2014) - QUESTION: why adjust for jj and not is?

                     ! If insufficient water in first surface to move dI, instead move as much as possible
                  ELSEIF ((soilstore_surf(is) + dI) < 0) THEN
                     soilstore_surf(jj) = soilstore_surf(jj) + soilstore_surf(is)*sfr_surf(is)/sfr_surf(jj) !HCW 12/08/2014 switched order of these two lines
                     soilstore_surf(is) = 0 !Check (HCW 13/08/2014) - QUESTION: can SM actually go to zero, or is this inconsistent with SMres?

                     ! If insufficient water in second surface to move dI, instead move as much as possible
                  ELSE
                     soilstore_surf(is) = soilstore_surf(is) + soilstore_surf(jj)*sfr_surf(jj)/sfr_surf(is)
                     soilstore_surf(jj) = 0
                  END IF

                  !If soil moisture exceeds capacity, excess goes to soil runoff (first surface)
                  IF (soilstore_surf(is) > SoilStoreCap_surf(is)) THEN
                     runoffSoil_surf(is) = runoffSoil_surf(is) + (soilstore_surf(is) - SoilStoreCap_surf(is))
                     soilstore_surf(is) = SoilStoreCap_surf(is)
                     !elseif (soilstore_id(is)<0) then  !HCW 13/08/2014 commented out as should never be true here anyway...
                     !   soilstore_id(is)=0             ! ... and if so, need to do more here (i.e. account for other water too)
                  END IF

                  !If soil moisture exceeds capacity, excess goes to soil runoff (second surface)
                  IF (soilstore_surf(jj) > SoilStoreCap_surf(jj)) THEN
                     runoffSoil_surf(jj) = runoffSoil_surf(jj) + (soilstore_surf(jj) - SoilStoreCap_surf(jj))
                     soilstore_surf(jj) = SoilStoreCap_surf(jj)
                     !elseif (soilstore_id(jj)<0) then  !HCW 13/08/2014 commented out (as above)
                     !         soilstore_id(jj)=0
                  END IF

               END IF !end if second surface exists and is capable of storing water

            END DO !end jj loop over second surface

            runoffSoil_per_tstep = runoffSoil_per_tstep + (runoffSoil_surf(is)*sfr_surf(is)/NonWaterFraction) !Excludes water body. Moved here as otherwise code crashed when NonWaterFraction=0

         END IF !end if first surface exists and is capable of storing water

         !runoffSoil_per_tstep=runoffSoil_per_tstep+(runoffSoil(is)*sfr_surf(is)/NonWaterFraction)  !Excludes water body

      END DO !is loop over first surface

   END SUBROUTINE SUEWS_cal_HorizontalSoilWater

   SUBROUTINE SUEWS_cal_HorizontalSoilWater_DTS( &
      timer, config, forcing, siteInfo, & ! input
      hydroState) ! inout: !Soil moisture of each surface type [mm]

      !Transfers water in soil stores of land surfaces LJ (2010)
      !Change the model to use varying hydraulic conductivity instead of constant value LJ (7/2011)
      !If one of the surface's soildepth is zero, no water movement is considered
      ! LJ  15/06/2017 Modification:   - Moved location of runoffSoil_per_tstep within previous if-loop to avoid dividing with zero with 100% water surface
      ! HCW 22/02/2017 Modifications:  - Minor bug fixed in VWC1/B_r1 comparison - if statements reversed
      ! HCW 13/08/2014 Modifications:  - Order of surfaces reversed (for both is and jj loops)
      !                                - Number of units (e.g. properties) added to distance calculation
      ! HCW 12/08/2014 Modifications:  - Distance changed from m to mm in dI_dt calculation
      !                                - dI_dt [mm s-1] multiplied by no. seconds in timestep -> dI [mm]
      !                                - if MatPot is set to max. value (100000 mm), Km set to 0 mm s-1
      !                                - Provide parameters for residual volumetric soil moisture [m3 m-3]
      !                                   (currently hard coded as 0.1 m3 m-3 for testing)
      !
      !------------------------------------------------------
      ! use SUES_data
      ! use gis_data
      ! use time
      ! use allocateArray

      USE SUEWS_DEF_DTS, ONLY: SUEWS_CONFIG, SUEWS_TIMER, SUEWS_FORCING, &
                               LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, SUEWS_SITE, HYDRO_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(HYDRO_STATE), INTENT(INOUT) :: hydroState

      REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilDepth_surf !Depth of sub-surface soil store for each surface [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: SatHydraulicConduct_surf !Saturated hydraulic conductivity for each soil subsurface [mm s-1]

      ! REAL(KIND(1D0)) :: SurfaceArea !Surface area of the study area [m2]
      ! REAL(KIND(1D0)), INTENT(in) :: tstep_real !tstep cast as a real for use in calculations

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(inout) :: soilstore_id !Soil moisture of each surface type [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: runoffSoil_surf !Soil runoff from each soil sub-surface [mm]

      ! REAL(KIND(1D0)), INTENT(out) :: runoffSoil_per_tstep !Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)

      INTEGER :: jj, is
      REAL(KIND(1D0)) :: &
         DimenWaterCon1, DimenWaterCon2, &
         SoilMoistCap_Vol1, &
         SoilMoist_vol1, &
         SoilMoistCap_Vol2, &
         SoilMoist_vol2, &
         B_r1, MatPot1, Km1, &
         B_r2, MatPot2, Km2, &
         Distance, KmWeight, dI, &
         dI_dt !Water flow between two stores

      REAL(KIND(1D0)), PARAMETER :: &
         alphavG = 0.0005, & !Set alphavG to match value in van Genuchten (1980) [mm-1]
         NUnits = 1 !Can change to represent plot/base unit size

      ! SoilMoist_vol1,2     = Volumetric soil moisture [m3 m-3]
      ! SoilMoistCap_vol1,2  = Volumetric soil moisture capacity [m3 m-3] (from FunctionalTypes)
      ! MatPot1,2            = Water potential (i.e. pressure head) of store [mm]
      ! DimenWaterCon1,2     = Dimensionless water content, or relative saturation [-]
      ! Distance             = Distance between two stores [m]
      ! B_r1,2               = Residual volumetric soil moisture [m3 m-3]
      ! Km1,2                = Hydraulic conductivity of store [mm s-1]
      ! KmWeight             = Weighted hydraulic conductivity [mm s-1]
      ! alphavG              = Parameter (could depend on soil texture) [mm-1]
      ! dI                   = Water flow between stores [mm] dI = dI_dt * no. secs in each timestep
      !                         if dI > 0, first surface gains water, second surface loses water
      ! NUnits               = Number of repeating units (e.g. properties, blocks) for distance calculation [-]

      ASSOCIATE ( &
         pavedPrm => siteInfo%lc_paved, &
         bldgPrm => siteInfo%lc_bldg, &
         evetrPrm => siteInfo%lc_evetr, &
         dectrPrm => siteInfo%lc_dectr, &
         grassPrm => siteInfo%lc_grass, &
         bsoilPrm => siteInfo%lc_bsoil, &
         waterPrm => siteInfo%lc_water, &
         ehcPrm => siteInfo%ehc, &
         sfr_surf => siteInfo%sfr_surf, &
         sfr_roof => siteInfo%sfr_roof, &
         sfr_wall => siteInfo%sfr_wall, &
         SurfaceArea => siteInfo%SurfaceArea, &
         snowPrm => siteInfo%snow, &
         PipeCapacity => siteInfo%PipeCapacity, &
         RunoffToWater => siteInfo%RunoffToWater, &
         FlowChange => siteInfo%FlowChange, &
         PervFraction => siteInfo%PervFraction, &
         vegfraction => siteInfo%vegfraction, &
         NonWaterFraction => siteInfo%NonWaterFraction, &
         tstep_real => timer%tstep_real, &
         soilstore_surf => hydroState%soilstore_surf, &
         runoffSoil_surf => hydroState%runoffSoil, &
         runoffSoil_per_tstep => hydroState%runoffSoil_per_tstep, &
         Diagnose => config%Diagnose &
         )
         runoffSoil_surf = 0
         runoffSoil_per_tstep = 0

         SoilStoreCap_surf(1) = pavedPrm%soil%soilstorecap
         SoilStoreCap_surf(2) = bldgPrm%soil%soilstorecap
         SoilStoreCap_surf(3) = evetrPrm%soil%soilstorecap
         SoilStoreCap_surf(4) = dectrPrm%soil%soilstorecap
         SoilStoreCap_surf(5) = grassPrm%soil%soilstorecap
         SoilStoreCap_surf(6) = bsoilPrm%soil%soilstorecap
         SoilStoreCap_surf(7) = waterPrm%soil%soilstorecap

         SoilDepth_surf(1) = pavedPrm%soil%soildepth
         SoilDepth_surf(2) = bldgPrm%soil%soildepth
         SoilDepth_surf(3) = evetrPrm%soil%soildepth
         SoilDepth_surf(4) = dectrPrm%soil%soildepth
         SoilDepth_surf(5) = grassPrm%soil%soildepth
         SoilDepth_surf(6) = bsoilPrm%soil%soildepth
         SoilDepth_surf(7) = waterPrm%soil%soildepth

         SatHydraulicConduct_surf(1) = pavedPrm%soil%sathydraulicconduct
         SatHydraulicConduct_surf(2) = bldgPrm%soil%sathydraulicconduct
         SatHydraulicConduct_surf(3) = evetrPrm%soil%sathydraulicconduct
         SatHydraulicConduct_surf(4) = dectrPrm%soil%sathydraulicconduct
         SatHydraulicConduct_surf(5) = grassPrm%soil%sathydraulicconduct
         SatHydraulicConduct_surf(6) = bsoilPrm%soil%sathydraulicconduct
         SatHydraulicConduct_surf(7) = waterPrm%soil%sathydraulicconduct

         CALL SUEWS_cal_HorizontalSoilWater( &
            sfr_surf, & ! input: ! surface fractions
            SoilStoreCap_surf, & !Capacity of soil store for each surface [mm]
            SoilDepth_surf, & !Depth of sub-surface soil store for each surface [mm]
            SatHydraulicConduct_surf, & !Saturated hydraulic conductivity for each soil subsurface [mm s-1]
            SurfaceArea, & !Surface area of the study area [m2]
            NonWaterFraction, & ! sum of surface cover fractions for all except water surfaces
            tstep_real, & !tstep cast as a real for use in calculations
            soilstore_surf, & ! inout: !Soil moisture of each surface type [mm]
            runoffSoil_surf, & !Soil runoff from each soil sub-surface [mm]
            runoffSoil_per_tstep & !  output:!Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
            )

         ! DO is = 1, nsurf - 1 !nsurf-1,1,-1  !Loop through each surface, excluding water surface (runs backwards as of 13/08/2014, HCW)

         !    IF (sfr_surf(is) /= 0 .AND. SoilStoreCap_surf(is) > 0) THEN !If particular surface area exists
         !       ! and is capable of storing water (SoilStoreCap [mm])
         !       DO jj = is + 1, nsurf - 1 !is-1,1,-1  !Sub-loop through remaining surfaces (runs backwards as of 13/08/2014, HCW)

         !          IF (sfr_surf(jj) /= 0 .AND. SoilStoreCap_surf(jj) > 0) THEN !If other surface area exists
         !             ! and is capable of storing water

         !             ! ---- For surface 1 -----------------------------------------------------
         !             ! Calculate non-saturated VWC
         !             SoilMoistCap_Vol1 = SoilStoreCap_surf(is)/SoilDepth_surf(is) !Volumetric soil moisture capacity [m3 m-3] (i.e. saturated VWC)
         !             SoilMoist_vol1 = hydroState%soilstore_surf(is)/SoilDepth_surf(is) !Volumetric soil moisture [m3 m-3]

         !             !B_r1=SoilMoistCap_Vol1-SoilMoist_vol1  !Residual soil moisture content [m3 m-3]
         !             B_r1 = 0.1 !HCW 12/08/2014 Temporary fix
         !             ! Need to add residual soil moisture values to FunctionalTypes
         !             !B_r1=VolSoilMoistRes(is) !Residual soil moisture content [m3 m-3]

         !             !Order of if statements reversed HCW 22 Feb 2017
         !             !If soil moisture less than or equal to residual value, set MatPot to max and Km to 0 to suppress water movement
         !             IF (B_r1 >= SoilMoist_vol1) THEN
         !                MatPot1 = 100000
         !                Km1 = 0 !Added by LJ in Nov 2013
         !                ! Otherwise, there should be enough water in the soil to allow horizontal transfer
         !             ELSE
         !                DimenWaterCon1 = (SoilMoist_vol1 - B_r1)/(SoilMoistCap_Vol1 - B_r1) !Dimensionless water content [-]

         !                ! If very large or very small, adjust for calculation of MatPot and Km
         !                IF (DimenWaterCon1 > 0.99999) THEN
         !                   DimenWaterCon1 = DimenWaterCon1 - 0.0001 !This cannot equal 1
         !                END IF

         !                IF (DimenWaterCon1 < 0.00000005) THEN
         !                   DimenWaterCon1 = DimenWaterCon1 + 0.0000001 !Added HCW 22 Feb 2017
         !                END IF

         !                !van Genuchten (1980), with n=2 and m = 1-1/n = 1/2
         !                !Water potential of first store [mm] (van Genuchten 1980, Eq 3 rearranged)
         !                MatPot1 = SQRT(1/DimenWaterCon1**2 - 1)/alphavG

         !                !Hydraulic conductivity of first store [mm s-1] (van Genuchten 1980, Eq 8)
         !                Km1 = SatHydraulicConduct_surf(is)*SQRT(DimenWaterCon1)*(1 - (1 - DimenWaterCon1**2)**0.5)**2

         !                !Check this value (HCW 12/08/2014)
         !                IF (MatPot1 > 100000) THEN
         !                   MatPot1 = 100000 !Max. potential is 100000 mm (van Genuchten 1980)
         !                   Km1 = 0 !Added by HCW 12/08/2014
         !                END IF

         !             END IF

         !             ! ---- For surface 2 -----------------------------------------------------
         !             ! Calculate non-saturated VWC
         !             SoilMoistCap_Vol2 = SoilStoreCap_surf(jj)/SoilDepth_surf(jj) !Volumetric soil moisture capacity [m3 m-3] (i.e. saturated VWC)
         !             SoilMoist_vol2 = hydroState%soilstore_surf(jj)/SoilDepth_surf(jj) !Volumetric soil moisture [m3 m-3]

         !             !B_r2=SoilMoistCap_Vol2-SoilMoist_vol2  !Residual soil moisture content [m3 m-3]
         !             B_r2 = 0.1 !HCW 12/08/2014 Temporary fix
         !             ! Need to add residual soil moisture values to FunctionalTypes
         !             !B_r2=VolSoilMoistRes(jj) !Residual soil moisture content [m3 m-3]

         !             !If soil moisture below residual value, set MatPot to maximum
         !             IF (B_r2 >= SoilMoist_vol2) THEN
         !                MatPot2 = 100000
         !                Km2 = 0 !Added by LJ in Nov 2013
         !             ELSE
         !                DimenWaterCon2 = (SoilMoist_vol2 - B_r2)/(SoilMoistCap_Vol2 - B_r2) !Dimensionless water content [-]

         !                IF (DimenWaterCon2 > 0.99999) THEN
         !                   DimenWaterCon2 = DimenWaterCon2 - 0.0001 !This cannot equal 1
         !                END IF

         !                IF (DimenWaterCon2 < 0.00000005) THEN
         !                   DimenWaterCon2 = DimenWaterCon2 + 0.0000001 !Added HCW 22 Feb 2017
         !                END IF
         !                ! print*, 'soilstore_id = ', soilstore_id
         !                ! print*, 'SoilStoreCap = ', SoilStoreCap
         !                ! print*, 'SoilDepth = ', SoilDepth
         !                ! print*, 'SoilMoist_vol2 = ', SoilMoist_vol2
         !                ! print*, 'SoilMoistCap_Vol2 = ', SoilMoistCap_Vol2
         !                ! print*, 'is = ', is, ' jj = ', jj, ' DimenWaterCon2 = ', DimenWaterCon2
         !                !van Genuchten (1980), with n=2 and m = 1-1/n = 1/2
         !                !Water potential of second store [mm] (van Genuchten 1980, Eq 3 rearranged)
         !                MatPot2 = SQRT(1/DimenWaterCon2**2 - 1)/alphavG

         !                !Hydraulic conductivity of second store [mm s-1] (van Genuchten 1980, Eq 8)
         !                Km2 = SatHydraulicConduct_surf(jj)*SQRT(DimenWaterCon2)*(1 - (1 - DimenWaterCon2**2)**0.5)**2

         !                IF ((MatPot2) > 100000) THEN
         !                   MatPot2 = 100000 !Max. potential is 100000 mm (van Genuchten 1980)
         !                   Km2 = 0 !Added by HCW 12/08/2014
         !                END IF

         !             END IF

         !             ! ------------------------------------------------------------------------

         !             !Find distance between the two stores (see Jarvi et al. 2011)
         !             !SurfaceArea in m2 (changed from ha to m2 n SUEWS_Initial), so Distance in m
         !             Distance = (SQRT(sfr_surf(is)*SurfaceArea/NUnits) + SQRT(sfr_surf(jj)*SurfaceArea/NUnits))/2

         !             !Calculate areally-weighted hydraulic conductivity [mm s-1]
         !             KmWeight = (sfr_surf(is)*Km1 + sfr_surf(jj)*Km2)/(sfr_surf(is) + sfr_surf(jj))

         !             !Find water flow between the two stores [mm s-1] (Green-Ampt equation, Hillel 1971)
         !             !Multiply Distance by 1000 to convert m to mm (HCW 12/08/2014)
         !             dI_dt = -(KmWeight)*(-MatPot1 + MatPot2)/(Distance*1000)

         !             !Multiply dI_dt by number of seconds in timestep to convert mm s-1 to mm
         !             !Added by HCW 12/08/2014
         !             dI = dI_dt*tstep_real !Use dI instead of dI_dt in the following calculations

         !             !Move water (in mm) ------------------------------------------------------
         !             !Water moves only if (i) there is sufficient water to move and (ii) there is space to move it

         !             ! If there is sufficient water in both surfaces, allow movement of dI to occur
         !             IF ((hydroState%soilstore_surf(jj) >= dI*sfr_surf(is)/sfr_surf(jj)) .AND. &
         !                 ((hydroState%soilstore_surf(is) + dI) >= 0)) THEN
         !                hydroState%soilstore_surf(is) = hydroState%soilstore_surf(is) + dI
         !                hydroState%soilstore_surf(jj) = hydroState%soilstore_surf(jj) - dI*sfr_surf(is)/sfr_surf(jj) !Check (HCW 13/08/2014) - QUESTION: why adjust for jj and not is?

         !                ! If insufficient water in first surface to move dI, instead move as much as possible
         !             ELSEIF ((hydroState%soilstore_surf(is) + dI) < 0) THEN
         !                hydroState%soilstore_surf(jj) = hydroState%soilstore_surf(jj) + &
         !                                                hydroState%soilstore_surf(is)*sfr_surf(is)/sfr_surf(jj) !HCW 12/08/2014 switched order of these two lines
         !                hydroState%soilstore_surf(is) = 0 !Check (HCW 13/08/2014) - QUESTION: can SM actually go to zero, or is this inconsistent with SMres?

         !                ! If insufficient water in second surface to move dI, instead move as much as possible
         !             ELSE
         !                hydroState%soilstore_surf(is) = hydroState%soilstore_surf(is) + &
         !                                                hydroState%soilstore_surf(jj)*sfr_surf(jj)/sfr_surf(is)
         !                hydroState%soilstore_surf(jj) = 0
         !             END IF

         !             !If soil moisture exceeds capacity, excess goes to soil runoff (first surface)
         !             IF (hydroState%soilstore_surf(is) > SoilStoreCap_surf(is)) THEN
         !                runoffSoil_surf(is) = runoffSoil_surf(is) + (hydroState%soilstore_surf(is) - SoilStoreCap_surf(is))
         !                hydroState%soilstore_surf(is) = SoilStoreCap_surf(is)
         !                !elseif (soilstore_id(is)<0) then  !HCW 13/08/2014 commented out as should never be true here anyway...
         !                !   soilstore_id(is)=0             ! ... and if so, need to do more here (i.e. account for other water too)
         !             END IF

         !             !If soil moisture exceeds capacity, excess goes to soil runoff (second surface)
         !             IF (hydroState%soilstore_surf(jj) > SoilStoreCap_surf(jj)) THEN
         !                runoffSoil_surf(jj) = runoffSoil_surf(jj) + (hydroState%soilstore_surf(jj) - SoilStoreCap_surf(jj))
         !                hydroState%soilstore_surf(jj) = SoilStoreCap_surf(jj)
         !                !elseif (soilstore_id(jj)<0) then  !HCW 13/08/2014 commented out (as above)
         !                !         soilstore_id(jj)=0
         !             END IF

         !          END IF !end if second surface exists and is capable of storing water

         !       END DO !end jj loop over second surface

         !       runoffSoil_per_tstep = runoffSoil_per_tstep + (runoffSoil_surf(is)*sfr_surf(is)/NonWaterFraction) !Excludes water body. Moved here as otherwise code crashed when NonWaterFraction=0

         !    END IF !end if first surface exists and is capable of storing water

         !    !runoffSoil_per_tstep=runoffSoil_per_tstep+(runoffSoil(is)*sfr_surf(is)/NonWaterFraction)  !Excludes water body

         ! END DO !is loop over first surface
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_HorizontalSoilWater_DTS
   !===================================================================================

   !===================================================================================
   ! SUBROUTINE SUEWS_cal_WaterUse( &
   !    nsh_real, & ! input:
   !    wu_m3, SurfaceArea, sfr_surf, &
   !    IrrFracPaved, IrrFracBldgs, &
   !    IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
   !    IrrFracBSoil, IrrFracWater, &
   !    DayofWeek_id, WUProfA_24hr, WUProfM_24hr, &
   !    InternalWaterUse_h, HDD_id, WUDay_id, &
   !    WaterUseMethod, NSH, it, imin, DLS, &
   !    wu_surf, wu_int, wu_ext) ! output:
   !    ! Conversion of water use (irrigation)
   !    ! Last modified:
   !    ! TS 30 Nov 2019  - allow external water use for all surfaces
   !    !                    (previously only on vegetated surfaces)
   !    ! TS 30 Oct 2018  - fixed a bug in external water use
   !    ! TS 08 Aug 2017  - addded explicit interface
   !    ! LJ  6 Apr 2017  - WUchoice changed to WaterUseMethod
   !    ! TK 14 Mar 2017  - Corrected the variable name WUAreaEveTr_m2 -> WUAreaGrass_m2 (row 35)
   !    !                   Corrected conversion from m to mm /1000 -> *1000 (row 47 and 60)
   !    ! LJ 27 Jan 2016  - Removing Tab:s and cleaning the code
   !    ! HCW 12 Feb 2015 - Water use [mm] now inidcates the amount of water supplied for each surface
   !    ! HCW 26 Jan 2015 - Water use [mm] is the same for each surface at the moment and indicates the
   !    !                    amount of water supplied for each irrigated area
   !    !
   !    ! To Do:
   !    !        - Add functionality for water on paved surfaces (street cleaning, fountains)

   !    IMPLICIT NONE
   !    ! INTEGER, PARAMETER :: nsurf = 7

   !    REAL(KIND(1D0)), INTENT(in) :: nsh_real
   !    REAL(KIND(1D0)), INTENT(in) :: wu_m3 ! external water input (e.g., irrigation)  [m3]
   !    REAL(KIND(1D0)), INTENT(in) :: SurfaceArea !Surface area of the study area [m2]
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracPaved !Fraction of paved which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracBldgs !Fraction of buildings (e.g., green roofs) which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracEveTr !Fraction of evergreen trees which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracDecTr !Fraction of deciduous trees which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracGrass !Fraction of grass which is irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracBSoil !Fraction of bare soil trees which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracWater !Fraction of water which are irrigated
   !    REAL(KIND(1D0)), INTENT(in) :: InternalWaterUse_h !Internal water use [mm h-1]
   !    REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: WUProfA_24hr !Automatic water use profiles at hourly scales
   !    REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: WUProfM_24hr !Manual water use profiles at hourly scales
   !    REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf !Surface fractions [-]

   !    REAL(KIND(1D0)), DIMENSION(12), INTENT(in) :: HDD_id !HDD(id-1), Heating Degree Days (see SUEWS_DailyState.f95)
   !    REAL(KIND(1D0)), DIMENSION(9), INTENT(in) :: WUDay_id !WUDay(id-1), Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)

   !    INTEGER, INTENT(in) :: DayofWeek_id(3) !DayofWeek(id) 1 - day of week; 2 - month; 3 - season
   !    INTEGER, INTENT(in) :: WaterUseMethod !Use modelled (0) or observed (1) water use
   !    INTEGER, INTENT(in) :: NSH !Number of timesteps per hour
   !    INTEGER, INTENT(in) :: it !Hour
   !    INTEGER, INTENT(in) :: imin !Minutes
   !    INTEGER, INTENT(in) :: DLS !day lightsavings =1 + 1h) =0

   !    REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: wu_surf !external Water use for each surface [mm]
   !    REAL(KIND(1D0)), INTENT(out) :: wu_int !Internal water use for the model timestep [mm] (over whole study area)
   !    REAL(KIND(1D0)), INTENT(out) :: wu_ext !External water use for the model timestep [mm] (over whole study area)

   !    REAL(KIND(1D0)) :: wu_EveTr !Water use for evergreen trees/shrubs [mm]
   !    REAL(KIND(1D0)) :: wu_DecTr !Water use for deciduous trees/shrubs [mm]
   !    REAL(KIND(1D0)) :: wu_Grass !Water use for grass [mm]

   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: WUDay_A_id !modelled Automatic Daily water use for each surface [mm] (see SUEWS_DailyState.f95)
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: WUDay_M_id !modelled Manual Daily water use for each surface [mm] (see SUEWS_DailyState.f95)
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: IrrFrac !faction of irrigated part in each surface [-]
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: WUArea !water use area [m2] for each surface type

   !    REAL(KIND(1D0)) :: WUAreaTotal_m2
   !    REAL(KIND(1D0)) :: InternalWaterUse !Internal water use for the model timestep [mm]
   !    REAL(KIND(1D0)) :: flag_WuM = 1
   !    REAL(KIND(1D0)) :: wu !Water use for the model timestep [mm]
   !    INTEGER :: ih !Hour corrected for Daylight savings
   !    INTEGER :: iu !1=weekday OR 2=weekend
   !    INTEGER :: tstep ! timestep in second
   !    REAL(KIND(1D0)), PARAMETER :: NAN = -999.
   !    REAL(KIND(1D0)) :: OverUse
   !    REAL(KIND(1D0)) :: rain_cum_daily ! accumulated daily rainfall

   !    ! REAL(KIND(1D0)) :: get_Prof_SpecTime_sum

   !    REAL(KIND(1D0)) :: WUProfA_tstep ! automatic water use profile value at tstep
   !    REAL(KIND(1D0)) :: WUProfM_tstep ! mannual water use profile value at tstep

   !    ! NB: set OverUse as 0 as done module_constants, TS 22 Oct 2017
   !    ! and the logic for calculating OverUse to be determined
   !    OverUse = 0

   !    ! initialise wu
   !    wu = 0

   !    ! timestep in second
   !    tstep = INT(3600/NSH)

   !    ! accumulated daily rainfall
   !    rain_cum_daily = HDD_id(11)

   !    ! Irrigated Fraction of each surface
   !    ! TS: as of 20191130, assuming irrigation fraction as ONE except for vegetated surfaces

   !    ! Irrigated Fraction of each surface
   !    ! TS: 20200409, add irrigation fractions for all surfaces
   !    IrrFrac = [IrrFracPaved, IrrFracBldgs, &
   !               IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
   !               IrrFracBSoil, IrrFracWater]

   !    ! --------------------------------------------------------------------------------
   !    ! If water used is observed and provided in the met forcing file, units are m3
   !    ! Divide observed water use (in m3) by water use area to find water use (in mm)
   !    IF (WaterUseMethod == 1) THEN !If water use is observed
   !       ! Calculate water use area [m2] for each surface type

   !       WUArea = IrrFrac*sfr_surf*SurfaceArea
   !       WUAreaTotal_m2 = SUM(WUArea)

   !       !Set water use [mm] for each surface type to zero initially
   !       wu_EveTr = 0
   !       wu_DecTr = 0
   !       wu_Grass = 0

   !       wu_surf = 0
   !       IF (wu_m3 == NAN .OR. wu_m3 == 0) THEN !If no water use
   !          ! wu_m3=0
   !          wu = 0
   !       ELSE !If water use
   !          IF (WUAreaTotal_m2 > 0) THEN
   !             wu = (wu_m3/WUAreaTotal_m2*1000) !Water use in mm for the whole irrigated area

   !             wu_surf = wu*IrrFrac

   !             wu = (wu_m3/SurfaceArea*1000) !Water use for the whole study area in mm
   !          END IF
   !       END IF

   !       ! --------------------------------------------------------------------------------
   !       ! If water use is modelled, calculate at timestep of model resolution [mm]
   !    ELSEIF (WaterUseMethod == 0) THEN !If water use is modelled

   !       ! Account for Daylight saving
   !       ih = it - DLS
   !       IF (ih < 0) ih = 23

   !       ! Weekday or weekend profile
   !       iu = 1 !Set to 1=weekday
   !       !  IF(DayofWeek(id,1)==1.OR.DayofWeek(id,1)==7) THEN
   !       IF (DayofWeek_id(1) == 1 .OR. DayofWeek_id(1) == 7) THEN
   !          iu = 2 !Set to 2=weekend
   !       END IF

   !       !write(*,*) (NSH*(ih+1-1)+imin*NSH/60+1)
   !       WUDay_A_id = 0
   !       WUDay_A_id(ConifSurf) = WUDay_id(2)
   !       WUDay_A_id(DecidSurf) = WUDay_id(5)
   !       WUDay_A_id(GrassSurf) = WUDay_id(8)

   !       WUDay_M_id = 0
   !       WUDay_M_id(ConifSurf) = WUDay_id(3)
   !       WUDay_M_id(DecidSurf) = WUDay_id(6)
   !       WUDay_M_id(GrassSurf) = WUDay_id(9)

   !       ! ---- Automatic irrigation ----
   !       WUProfA_tstep = get_Prof_SpecTime_sum(ih, imin, 0, WUProfA_24hr(:, iu), tstep)

   !       ! ---- Manual irrigation ----
   !       flag_WuM = 1 !Initialize flag_WuM to 1, but if raining, reduce manual fraction of water use
   !       ! If cumulative daily precipitation exceeds 2 mm
   !       IF (rain_cum_daily > 2) THEN !.and.WUDay(id-1,3)>0) then !Commented out HCW 23/01/2015
   !          flag_WuM = 0 ! 0 -> No manual irrigation if raining
   !       END IF

   !       ! Add manual to automatic to find total irrigation
   !       WUProfM_tstep = get_Prof_SpecTime_sum(ih, imin, 0, WUProfM_24hr(:, iu), tstep)

   !       ! sum up irrigation amount of automatic and manual approaches
   !       wu_surf = WUProfA_tstep*WUDay_A_id + WUProfM_tstep*WUDay_M_id*flag_WuM
   !       ! apply irrigation fraction: part of land covers are not irrigated
   !       wu_surf = wu_surf*IrrFrac

   !       ! Total water use for the whole study area [mm]
   !       ! wu = wu_EveTr*sfr_surf(ConifSurf) + wu_DecTr*sfr_surf(DecidSurf) + wu_Grass*sfr_surf(GrassSurf)
   !       wu = DOT_PRODUCT(wu_surf, sfr_surf)

   !    END IF !End WU_choice
   !    ! --------------------------------------------------------------------------------

   !    ! Internal water use is supplied in SUEWS_Irrigation in mm h-1
   !    ! Convert to mm for the model timestep
   !    InternalWaterUse = InternalWaterUse_h/nsh_real

   !    ! Remove InternalWaterUse from the total water use
   !    wu_ext = wu - (InternalWaterUse + OverUse)
   !    ! Check ext_wu cannot be negative
   !    IF (wu_ext < 0) THEN
   !       overUse = ABS(wu_ext)
   !       wu_ext = 0
   !    ELSE
   !       OverUse = 0
   !    END IF

   !    wu_int = wu - wu_ext

   !    ! Decrease the water use for each surface by the same proportion
   !    IF (wu_ext /= 0 .AND. wu /= 0) THEN
   !       wu_surf = wu_surf*wu_ext/wu
   !    END IF

   ! END SUBROUTINE SUEWS_cal_WaterUse

   ! SUBROUTINE SUEWS_cal_WaterUse_DTS( &
   !    nsh_real, & ! input:
   !    wu_m3, SurfaceArea, &
   !    sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water, &
   !    IrrFracPaved, IrrFracBldgs, &
   !    IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
   !    IrrFracBSoil, IrrFracWater, &
   !    DayofWeek_id, &
   !    WUProfA_24hr_working, WUProfA_24hr_holiday, &
   !    wuprofm_24hr_working, wuprofm_24hr_holiday, &
   !    InternalWaterUse_h, HDD_id, WUDay_id, &
   !    WaterUseMethod, NSH, it, imin, DLS, &
   !    wu_surf, wu_int, wu_ext) ! output:
   !    ! Conversion of water use (irrigation)
   !    ! Last modified:
   !    ! TS 30 Nov 2019  - allow external water use for all surfaces
   !    !                    (previously only on vegetated surfaces)
   !    ! TS 30 Oct 2018  - fixed a bug in external water use
   !    ! TS 08 Aug 2017  - addded explicit interface
   !    ! LJ  6 Apr 2017  - WUchoice changed to WaterUseMethod
   !    ! TK 14 Mar 2017  - Corrected the variable name WUAreaEveTr_m2 -> WUAreaGrass_m2 (row 35)
   !    !                   Corrected conversion from m to mm /1000 -> *1000 (row 47 and 60)
   !    ! LJ 27 Jan 2016  - Removing Tab:s and cleaning the code
   !    ! HCW 12 Feb 2015 - Water use [mm] now inidcates the amount of water supplied for each surface
   !    ! HCW 26 Jan 2015 - Water use [mm] is the same for each surface at the moment and indicates the
   !    !                    amount of water supplied for each irrigated area
   !    !
   !    ! To Do:
   !    !        - Add functionality for water on paved surfaces (street cleaning, fountains)

   !    IMPLICIT NONE
   !    ! INTEGER, PARAMETER :: nsurf = 7

   !    REAL(KIND(1D0)), INTENT(in) :: nsh_real
   !    REAL(KIND(1D0)), INTENT(in) :: wu_m3 ! external water input (e.g., irrigation)  [m3]
   !    REAL(KIND(1D0)), INTENT(in) :: SurfaceArea !Surface area of the study area [m2]
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracPaved !Fraction of paved which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracBldgs !Fraction of buildings (e.g., green roofs) which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracEveTr !Fraction of evergreen trees which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracDecTr !Fraction of deciduous trees which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracGrass !Fraction of grass which is irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracBSoil !Fraction of bare soil trees which are irrigated
   !    REAL(KIND(1D0)), INTENT(IN) :: IrrFracWater !Fraction of water which are irrigated
   !    REAL(KIND(1D0)), INTENT(in) :: InternalWaterUse_h !Internal water use [mm h-1]

   !    REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: WUProfA_24hr_working
   !    REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: WUProfA_24hr_holiday
   !    REAL(KIND(1D0)), DIMENSION(0:23, 2) :: WUProfA_24hr !Automatic water use profiles at hourly scales
   !    REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: WUProfM_24hr_working
   !    REAL(KIND(1D0)), DIMENSION(0:23), INTENT(in) :: WUProfM_24hr_holiday
   !    REAL(KIND(1D0)), DIMENSION(0:23, 2) :: WUProfM_24hr !Manual water use profiles at hourly scales

   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_paved
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_bldg
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_evetr
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_dectr
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_grass
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_bsoil
   !    REAL(KIND(1D0)), INTENT(IN) :: sfr_water
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf !Surface fractions [-]

   !    REAL(KIND(1D0)), DIMENSION(12), INTENT(in) :: HDD_id !HDD(id-1), Heating Degree Days (see SUEWS_DailyState.f95)
   !    REAL(KIND(1D0)), DIMENSION(9), INTENT(in) :: WUDay_id !WUDay(id-1), Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)

   !    INTEGER, INTENT(in) :: DayofWeek_id(3) !DayofWeek(id) 1 - day of week; 2 - month; 3 - season
   !    INTEGER, INTENT(in) :: WaterUseMethod !Use modelled (0) or observed (1) water use
   !    INTEGER, INTENT(in) :: NSH !Number of timesteps per hour
   !    INTEGER, INTENT(in) :: it !Hour
   !    INTEGER, INTENT(in) :: imin !Minutes
   !    INTEGER, INTENT(in) :: DLS !day lightsavings =1 + 1h) =0

   !    REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: wu_surf !external Water use for each surface [mm]
   !    REAL(KIND(1D0)), INTENT(out) :: wu_int !Internal water use for the model timestep [mm] (over whole study area)
   !    REAL(KIND(1D0)), INTENT(out) :: wu_ext !External water use for the model timestep [mm] (over whole study area)

   !    REAL(KIND(1D0)) :: wu_EveTr !Water use for evergreen trees/shrubs [mm]
   !    REAL(KIND(1D0)) :: wu_DecTr !Water use for deciduous trees/shrubs [mm]
   !    REAL(KIND(1D0)) :: wu_Grass !Water use for grass [mm]

   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: WUDay_A_id !modelled Automatic Daily water use for each surface [mm] (see SUEWS_DailyState.f95)
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: WUDay_M_id !modelled Manual Daily water use for each surface [mm] (see SUEWS_DailyState.f95)
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: IrrFrac !faction of irrigated part in each surface [-]
   !    REAL(KIND(1D0)), DIMENSION(nsurf) :: WUArea !water use area [m2] for each surface type

   !    REAL(KIND(1D0)) :: WUAreaTotal_m2
   !    REAL(KIND(1D0)) :: InternalWaterUse !Internal water use for the model timestep [mm]
   !    REAL(KIND(1D0)) :: flag_WuM = 1
   !    REAL(KIND(1D0)) :: wu !Water use for the model timestep [mm]
   !    INTEGER :: ih !Hour corrected for Daylight savings
   !    INTEGER :: iu !1=weekday OR 2=weekend
   !    INTEGER :: tstep ! timestep in second
   !    REAL(KIND(1D0)), PARAMETER :: NAN = -999.
   !    REAL(KIND(1D0)) :: OverUse
   !    REAL(KIND(1D0)) :: rain_cum_daily ! accumulated daily rainfall

   !    REAL(KIND(1D0)) :: get_Prof_SpecTime_sum

   !    REAL(KIND(1D0)) :: WUProfA_tstep ! automatic water use profile value at tstep
   !    REAL(KIND(1D0)) :: WUProfM_tstep ! mannual water use profile value at tstep

   !    sfr_surf = [sfr_paved, sfr_bldg, sfr_evetr, sfr_dectr, sfr_grass, sfr_bsoil, sfr_water]
   !    WUProfA_24hr(:, 1) = WUProfA_24hr_working
   !    WUProfA_24hr(:, 2) = WUProfA_24hr_holiday
   !    WUProfM_24hr(:, 1) = WUProfM_24hr_working
   !    WUProfM_24hr(:, 2) = WUProfM_24hr_holiday
   !    ! NB: set OverUse as 0 as done module_constants, TS 22 Oct 2017
   !    ! and the logic for calculating OverUse to be determined
   !    OverUse = 0

   !    ! initialise wu
   !    wu = 0

   !    ! timestep in second
   !    tstep = INT(3600/NSH)

   !    ! accumulated daily rainfall
   !    rain_cum_daily = HDD_id(11)

   !    ! Irrigated Fraction of each surface
   !    ! TS: as of 20191130, assuming irrigation fraction as ONE except for vegetated surfaces

   !    ! Irrigated Fraction of each surface
   !    ! TS: 20200409, add irrigation fractions for all surfaces
   !    IrrFrac = [IrrFracPaved, IrrFracBldgs, &
   !               IrrFracEveTr, IrrFracDecTr, IrrFracGrass, &
   !               IrrFracBSoil, IrrFracWater]

   !    ! --------------------------------------------------------------------------------
   !    ! If water used is observed and provided in the met forcing file, units are m3
   !    ! Divide observed water use (in m3) by water use area to find water use (in mm)
   !    IF (WaterUseMethod == 1) THEN !If water use is observed
   !       ! Calculate water use area [m2] for each surface type

   !       WUArea = IrrFrac*sfr_surf*SurfaceArea
   !       WUAreaTotal_m2 = SUM(WUArea)

   !       !Set water use [mm] for each surface type to zero initially
   !       wu_EveTr = 0
   !       wu_DecTr = 0
   !       wu_Grass = 0

   !       wu_surf = 0
   !       IF (wu_m3 == NAN .OR. wu_m3 == 0) THEN !If no water use
   !          ! wu_m3=0
   !          wu = 0
   !       ELSE !If water use
   !          IF (WUAreaTotal_m2 > 0) THEN
   !             wu = (wu_m3/WUAreaTotal_m2*1000) !Water use in mm for the whole irrigated area

   !             wu_surf = wu*IrrFrac

   !             wu = (wu_m3/SurfaceArea*1000) !Water use for the whole study area in mm
   !          END IF
   !       END IF

   !       ! --------------------------------------------------------------------------------
   !       ! If water use is modelled, calculate at timestep of model resolution [mm]
   !    ELSEIF (WaterUseMethod == 0) THEN !If water use is modelled

   !       ! Account for Daylight saving
   !       ih = it - DLS
   !       IF (ih < 0) ih = 23

   !       ! Weekday or weekend profile
   !       iu = 1 !Set to 1=weekday
   !       !  IF(DayofWeek(id,1)==1.OR.DayofWeek(id,1)==7) THEN
   !       IF (DayofWeek_id(1) == 1 .OR. DayofWeek_id(1) == 7) THEN
   !          iu = 2 !Set to 2=weekend
   !       END IF

   !       !write(*,*) (NSH*(ih+1-1)+imin*NSH/60+1)
   !       WUDay_A_id = 0
   !       WUDay_A_id(ConifSurf) = WUDay_id(2)
   !       WUDay_A_id(DecidSurf) = WUDay_id(5)
   !       WUDay_A_id(GrassSurf) = WUDay_id(8)

   !       WUDay_M_id = 0
   !       WUDay_M_id(ConifSurf) = WUDay_id(3)
   !       WUDay_M_id(DecidSurf) = WUDay_id(6)
   !       WUDay_M_id(GrassSurf) = WUDay_id(9)

   !       ! ---- Automatic irrigation ----
   !       WUProfA_tstep = get_Prof_SpecTime_sum(ih, imin, 0, WUProfA_24hr(:, iu), tstep)

   !       ! ---- Manual irrigation ----
   !       flag_WuM = 1 !Initialize flag_WuM to 1, but if raining, reduce manual fraction of water use
   !       ! If cumulative daily precipitation exceeds 2 mm
   !       IF (rain_cum_daily > 2) THEN !.and.WUDay(id-1,3)>0) then !Commented out HCW 23/01/2015
   !          flag_WuM = 0 ! 0 -> No manual irrigation if raining
   !       END IF

   !       ! Add manual to automatic to find total irrigation
   !       WUProfM_tstep = get_Prof_SpecTime_sum(ih, imin, 0, WUProfM_24hr(:, iu), tstep)

   !       ! sum up irrigation amount of automatic and manual approaches
   !       wu_surf = WUProfA_tstep*WUDay_A_id + WUProfM_tstep*WUDay_M_id*flag_WuM
   !       ! apply irrigation fraction: part of land covers are not irrigated
   !       wu_surf = wu_surf*IrrFrac

   !       ! Total water use for the whole study area [mm]
   !       ! wu = wu_EveTr*sfr_surf(ConifSurf) + wu_DecTr*sfr_surf(DecidSurf) + wu_Grass*sfr_surf(GrassSurf)
   !       wu = DOT_PRODUCT(wu_surf, sfr_surf)

   !    END IF !End WU_choice
   !    ! --------------------------------------------------------------------------------

   !    ! Internal water use is supplied in SUEWS_Irrigation in mm h-1
   !    ! Convert to mm for the model timestep
   !    InternalWaterUse = InternalWaterUse_h/nsh_real

   !    ! Remove InternalWaterUse from the total water use
   !    wu_ext = wu - (InternalWaterUse + OverUse)
   !    ! Check ext_wu cannot be negative
   !    IF (wu_ext < 0) THEN
   !       overUse = ABS(wu_ext)
   !       wu_ext = 0
   !    ELSE
   !       OverUse = 0
   !    END IF

   !    wu_int = wu - wu_ext

   !    ! Decrease the water use for each surface by the same proportion
   !    IF (wu_ext /= 0 .AND. wu /= 0) THEN
   !       wu_surf = wu_surf*wu_ext/wu
   !    END IF

   ! END SUBROUTINE SUEWS_cal_WaterUse_DTS

   SUBROUTINE SUEWS_cal_WaterUse( &
      timer, config, forcing, siteInfo, & ! input
      modState) ! input/output:
      ! anthroEmisState, hydroState) ! INout:
      ! Conversion of water use (irrigation)
      ! Last modified:
      ! TS 30 Nov 2019  - allow external water use for all surfaces
      !                    (previously only on vegetated surfaces)
      ! TS 30 Oct 2018  - fixed a bug in external water use
      ! TS 08 Aug 2017  - addded explicit interface
      ! LJ  6 Apr 2017  - WUchoice changed to WaterUseMethod
      ! TK 14 Mar 2017  - Corrected the variable name WUAreaEveTr_m2 -> WUAreaGrass_m2 (row 35)
      !                   Corrected conversion from m to mm /1000 -> *1000 (row 47 and 60)
      ! LJ 27 Jan 2016  - Removing Tab:s and cleaning the code
      ! HCW 12 Feb 2015 - Water use [mm] now inidcates the amount of water supplied for each surface
      ! HCW 26 Jan 2015 - Water use [mm] is the same for each surface at the moment and indicates the
      !                    amount of water supplied for each irrigated area
      !
      ! To Do:
      !        - Add functionality for water on paved surfaces (street cleaning, fountains)

      USE SUEWS_DEF_DTS, ONLY: SUEWS_SITE, SUEWS_TIMER, SUEWS_CONFIG, SUEWS_FORCING, &
                               LC_PAVED_PRM, LC_BLDG_PRM, LC_EVETR_PRM, LC_DECTR_PRM, &
                               LC_GRASS_PRM, LC_BSOIL_PRM, LC_WATER_PRM, &
                               IRRIGATION_PRM, anthroEmis_STATE, &
                               HYDRO_STATE, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo
      ! INTEGER, PARAMETER :: nsurf = 7
      TYPE(SUEWS_STATE), INTENT(INout) :: modState

      ! TYPE(IRRIGATION_PRM), INTENT(IN) :: irrPrm
      ! REAL(KIND(1D0)), INTENT(in) :: nsh_real
      ! REAL(KIND(1D0)) :: wu_m3 ! external water input (e.g., irrigation)  [m3]
      REAL(KIND(1D0)) :: SurfaceArea !Surface area of the study area [m2]
      ! REAL(KIND(1D0)) :: InternalWaterUse_h !Internal water use [mm h-1]

      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: WUProfA_24hr !Automatic water use profiles at hourly scales
      REAL(KIND(1D0)), DIMENSION(0:23, 2) :: WUProfM_24hr !Manual water use profiles at hourly scales

      ! TYPE(LC_PAVED_PRM), INTENT(IN) :: pavedPrm
      ! TYPE(LC_BLDG_PRM), INTENT(IN) :: bldgPrm
      ! TYPE(LC_EVETR_PRM), INTENT(IN) :: evetrPrm
      ! TYPE(LC_DECTR_PRM), INTENT(IN) :: dectrPrm
      ! TYPE(LC_GRASS_PRM), INTENT(IN) :: grassPrm
      ! TYPE(LC_BSOIL_PRM), INTENT(IN) :: bsoilPrm
      ! TYPE(LC_WATER_PRM), INTENT(IN) :: waterPrm
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: sfr_surf !Surface fractions [-]

      ! TYPE(anthroEmis_STATE), INTENT(IN) :: anthroEmisState
      ! TYPE(HYDRO_STATE), INTENT(INout) :: hydroState
      ! REAL(KIND(1D0)), DIMENSION(12) :: HDD_id !HDD(id-1), Heating Degree Days (see SUEWS_DailyState.f95)
      ! REAL(KIND(1D0)), DIMENSION(9) :: WUDay_id !WUDay(id-1), Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)

      ! INTEGER, INTENT(in) :: DayofWeek_id(3) !DayofWeek(id) 1 - day of week; 2 - month; 3 - season
      ! INTEGER :: WaterUseMethod !Use modelled (0) or observed (1) water use

      ! INTEGER :: it !Hour
      ! INTEGER :: imin !Minutes

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: wu_surf !external Water use for each surface [mm]
      ! REAL(KIND(1D0)), INTENT(out) :: wu_int !Internal water use for the model timestep [mm] (over whole study area)
      ! REAL(KIND(1D0)), INTENT(out) :: wu_ext !External water use for the model timestep [mm] (over whole study area)

      REAL(KIND(1D0)) :: wu_EveTr !Water use for evergreen trees/shrubs [mm]
      REAL(KIND(1D0)) :: wu_DecTr !Water use for deciduous trees/shrubs [mm]
      REAL(KIND(1D0)) :: wu_Grass !Water use for grass [mm]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: WUDay_A_id !modelled Automatic Daily water use for each surface [mm] (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nsurf) :: WUDay_M_id !modelled Manual Daily water use for each surface [mm] (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nsurf) :: IrrFrac !faction of irrigated part in each surface [-]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: WUArea !water use area [m2] for each surface type

      REAL(KIND(1D0)) :: WUAreaTotal_m2
      REAL(KIND(1D0)) :: InternalWaterUse !Internal water use for the model timestep [mm]
      REAL(KIND(1D0)) :: flag_WuM = 1
      REAL(KIND(1D0)) :: wu !Water use for the model timestep [mm]
      INTEGER :: ih !Hour corrected for Daylight savings
      INTEGER :: iu !1=weekday OR 2=weekend
      INTEGER :: tstep ! timestep in second
      REAL(KIND(1D0)), PARAMETER :: NAN = -999.
      REAL(KIND(1D0)) :: OverUse
      REAL(KIND(1D0)) :: rain_cum_daily ! accumulated daily rainfall

      ! REAL(KIND(1D0)) :: get_Prof_SpecTime_sum

      REAL(KIND(1D0)) :: WUProfA_tstep ! automatic water use profile value at tstep
      REAL(KIND(1D0)) :: WUProfM_tstep ! mannual water use profile value at tstep

      ASSOCIATE ( &
         anthroEmisState => modState%anthroEmisState, &
         hydroState => modState%hydroState &
         )
         ASSOCIATE ( &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            grassPrm => siteInfo%lc_grass, &
            dectrPrm => siteInfo%lc_dectr, &
            evetrPrm => siteInfo%lc_evetr, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            irrPrm => siteInfo%irrigation &
            )
            ASSOCIATE ( &
               SurfaceArea => siteInfo%SurfaceArea, &
               sfr_surf => siteInfo%sfr_surf, &
               InternalWaterUse_h => irrPrm%InternalWaterUse_h, &
               HDD_id => anthroEmisState%HDD_id, &
               WUDay_id => hydroState%WUDay_id, &
               WaterUseMethod => config%WaterUseMethod, &
               wu_m3 => forcing%Wuh, &
               wu_surf => hydroState%wu_surf, &
               wu_int => hydroState%wu_int, &
               wu_ext => hydroState%wu_ext, &
               it => timer%it, &
               nsh_real => timer%nsh_real, &
               DayofWeek_id => timer%DayofWeek_id, &
               NSH => timer%NSH, &
               DLS => timer%DLS, &
               imin => timer%imin &
               )

               ! sfr_surf = [pavedPrm%sfr, bldgPrm%sfr, evetrPrm%sfr, dectrPrm%sfr, grassPrm%sfr, bsoilPrm%sfr, waterPrm%sfr]
               WUProfA_24hr(:, 1) = irrPrm%wuprofa_24hr_working
               WUProfA_24hr(:, 2) = irrPrm%wuprofa_24hr_holiday
               WUProfM_24hr(:, 1) = irrPrm%wuprofm_24hr_working
               WUProfM_24hr(:, 2) = irrPrm%wuprofm_24hr_holiday
               ! NB: set OverUse as 0 as done module_constants, TS 22 Oct 2017
               ! and the logic for calculating OverUse to be determined
               OverUse = 0

               ! initialise wu
               wu = 0

               ! timestep in second
               tstep = INT(3600/NSH)

               ! accumulated daily rainfall
               rain_cum_daily = HDD_id(11)

               ! Irrigated Fraction of each surface
               ! TS: as of 20191130, assuming irrigation fraction as ONE except for vegetated surfaces

               ! Irrigated Fraction of each surface
               ! TS: 20200409, add irrigation fractions for all surfaces
               IrrFrac = [pavedPrm%IrrFracPaved, bldgPrm%IrrFracBldgs, &
                          evetrPrm%IrrFracEveTr, dectrPrm%IrrFracDecTr, grassPrm%IrrFracGrass, &
                          bsoilPrm%IrrFracBSoil, waterPrm%IrrFracWater]

               ! --------------------------------------------------------------------------------
               ! If water used is observed and provided in the met forcing file, units are m3
               ! Divide observed water use (in m3) by water use area to find water use (in mm)
               IF (WaterUseMethod == 1) THEN !If water use is observed
                  ! Calculate water use area [m2] for each surface type

                  WUArea = IrrFrac*sfr_surf*SurfaceArea
                  WUAreaTotal_m2 = SUM(WUArea)

                  !Set water use [mm] for each surface type to zero initially
                  wu_EveTr = 0
                  wu_DecTr = 0
                  wu_Grass = 0

                  wu_surf = 0
                  IF (wu_m3 == NAN .OR. wu_m3 == 0) THEN !If no water use
                     ! wu_m3=0
                     wu = 0
                  ELSE !If water use
                     IF (WUAreaTotal_m2 > 0) THEN
                        wu = (wu_m3/WUAreaTotal_m2*1000) !Water use in mm for the whole irrigated area

                        wu_surf = wu*IrrFrac

                        wu = (wu_m3/SurfaceArea*1000) !Water use for the whole study area in mm
                     END IF
                  END IF

                  ! --------------------------------------------------------------------------------
                  ! If water use is modelled, calculate at timestep of model resolution [mm]
               ELSEIF (WaterUseMethod == 0) THEN !If water use is modelled

                  ! Account for Daylight saving
                  ih = it - DLS
                  IF (ih < 0) ih = 23

                  ! Weekday or weekend profile
                  iu = 1 !Set to 1=weekday
                  !  IF(DayofWeek(id,1)==1.OR.DayofWeek(id,1)==7) THEN
                  IF (DayofWeek_id(1) == 1 .OR. DayofWeek_id(1) == 7) THEN
                     iu = 2 !Set to 2=weekend
                  END IF

                  !write(*,*) (NSH*(ih+1-1)+imin*NSH/60+1)
                  WUDay_A_id = 0
                  WUDay_A_id(ConifSurf) = WUDay_id(2)
                  WUDay_A_id(DecidSurf) = WUDay_id(5)
                  WUDay_A_id(GrassSurf) = WUDay_id(8)

                  WUDay_M_id = 0
                  WUDay_M_id(ConifSurf) = WUDay_id(3)
                  WUDay_M_id(DecidSurf) = WUDay_id(6)
                  WUDay_M_id(GrassSurf) = WUDay_id(9)

                  ! ---- Automatic irrigation ----
                  WUProfA_tstep = get_Prof_SpecTime_sum(ih, imin, 0, WUProfA_24hr(:, iu), tstep)

                  ! ---- Manual irrigation ----
                  flag_WuM = 1 !Initialize flag_WuM to 1, but if raining, reduce manual fraction of water use
                  ! If cumulative daily precipitation exceeds 2 mm
                  IF (rain_cum_daily > 2) THEN !.and.WUDay(id-1,3)>0) then !Commented out HCW 23/01/2015
                     flag_WuM = 0 ! 0 -> No manual irrigation if raining
                  END IF

                  ! Add manual to automatic to find total irrigation
                  WUProfM_tstep = get_Prof_SpecTime_sum(ih, imin, 0, WUProfM_24hr(:, iu), tstep)

                  ! sum up irrigation amount of automatic and manual approaches
                  wu_surf = WUProfA_tstep*WUDay_A_id + WUProfM_tstep*WUDay_M_id*flag_WuM
                  ! apply irrigation fraction: part of land covers are not irrigated
                  wu_surf = wu_surf*IrrFrac

                  ! Total water use for the whole study area [mm]
                  ! wu = wu_EveTr*sfr_surf(ConifSurf) + wu_DecTr*sfr_surf(DecidSurf) + wu_Grass*sfr_surf(GrassSurf)
                  wu = DOT_PRODUCT(wu_surf, sfr_surf)

               END IF !End WU_choice
               ! --------------------------------------------------------------------------------

               ! Internal water use is supplied in SUEWS_Irrigation in mm h-1
               ! Convert to mm for the model timestep
               InternalWaterUse = InternalWaterUse_h/nsh_real

               ! Remove InternalWaterUse from the total water use
               wu_ext = wu - (InternalWaterUse + OverUse)
               ! Check ext_wu cannot be negative
               IF (wu_ext < 0) THEN
                  overUse = ABS(wu_ext)
                  wu_ext = 0
               ELSE
                  OverUse = 0
               END IF

               wu_int = wu - wu_ext

               ! Decrease the water use for each surface by the same proportion
               IF (wu_ext /= 0 .AND. wu /= 0) THEN
                  wu_surf = wu_surf*wu_ext/wu
               END IF
            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE
   END SUBROUTINE SUEWS_cal_WaterUse
   !===================================================================================

   ! TODO: this is a temporary workaround for the fact that the compiler does not support function-calling within associate
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

END MODULE WaterDist_module
