!> @file suews_accessor_stebbs.f95
!> @brief Accessor functions for STEBBS_STATE in SUEWS_STATE
!>
!> Provides getter and setter functions to access STEBBS (Simple
!> Thermal-Energy Balance Building Scheme) state variables from Python via f90wrap.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_accessor_stebbs
   USE module_ctrl_type, ONLY: SUEWS_STATE, STEBBS_STATE
   IMPLICIT NONE

CONTAINS

   !> Get STEBBS state shortwave radiation components
   SUBROUTINE get_stebbs_state_krad(state, Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast

      Kdown2d = state%stebbsState%Kdown2d
      Kup2d = state%stebbsState%Kup2d
      Kwest = state%stebbsState%Kwest
      Ksouth = state%stebbsState%Ksouth
      Knorth = state%stebbsState%Knorth
      Keast = state%stebbsState%Keast
   END SUBROUTINE get_stebbs_state_krad

   !> Set STEBBS state shortwave radiation components
   SUBROUTINE set_stebbs_state_krad(state, Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast

      state%stebbsState%Kdown2d = Kdown2d
      state%stebbsState%Kup2d = Kup2d
      state%stebbsState%Kwest = Kwest
      state%stebbsState%Ksouth = Ksouth
      state%stebbsState%Knorth = Knorth
      state%stebbsState%Keast = Keast
   END SUBROUTINE set_stebbs_state_krad

   !> Get STEBBS state longwave radiation components
   SUBROUTINE get_stebbs_state_lrad(state, Ldown2d, Lup2d, Lwest, Lsouth, Lnorth, Least)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: Ldown2d, Lup2d, Lwest, Lsouth, Lnorth, Least

      Ldown2d = state%stebbsState%Ldown2d
      Lup2d = state%stebbsState%Lup2d
      Lwest = state%stebbsState%Lwest
      Lsouth = state%stebbsState%Lsouth
      Lnorth = state%stebbsState%Lnorth
      Least = state%stebbsState%Least
   END SUBROUTINE get_stebbs_state_lrad

   !> Set STEBBS state longwave radiation components
   SUBROUTINE set_stebbs_state_lrad(state, Ldown2d, Lup2d, Lwest, Lsouth, Lnorth, Least)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: Ldown2d, Lup2d, Lwest, Lsouth, Lnorth, Least

      state%stebbsState%Ldown2d = Ldown2d
      state%stebbsState%Lup2d = Lup2d
      state%stebbsState%Lwest = Lwest
      state%stebbsState%Lsouth = Lsouth
      state%stebbsState%Lnorth = Lnorth
      state%stebbsState%Least = Least
   END SUBROUTINE set_stebbs_state_lrad

   !> Get STEBBS state RSL profile arrays
   SUBROUTINE get_stebbs_state_rsl(state, zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(30), INTENT(OUT) :: zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL

      zarray = state%stebbsState%zarray
      dataoutLineURSL = state%stebbsState%dataoutLineURSL
      dataoutLineTRSL = state%stebbsState%dataoutLineTRSL
      dataoutLineqRSL = state%stebbsState%dataoutLineqRSL
   END SUBROUTINE get_stebbs_state_rsl

   !> Set STEBBS state RSL profile arrays
   SUBROUTINE set_stebbs_state_rsl(state, zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(30), INTENT(IN) :: zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL

      state%stebbsState%zarray = zarray
      state%stebbsState%dataoutLineURSL = dataoutLineURSL
      state%stebbsState%dataoutLineTRSL = dataoutLineTRSL
      state%stebbsState%dataoutLineqRSL = dataoutLineqRSL
   END SUBROUTINE set_stebbs_state_rsl

   !> Get STEBBS state temperature scalars (building envelope)
   SUBROUTINE get_stebbs_state_temps_envelope(state, &
         DeepSoilTemperature, OutdoorAirStartTemperature, &
         IndoorAirStartTemperature, IndoorMassStartTemperature, &
         WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature, &
         RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature, &
         WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature, &
         GroundFloorIndoorSurfaceTemperature, GroundFloorOutdoorSurfaceTemperature)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: DeepSoilTemperature, OutdoorAirStartTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: IndoorAirStartTemperature, IndoorMassStartTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: GroundFloorIndoorSurfaceTemperature, GroundFloorOutdoorSurfaceTemperature

      DeepSoilTemperature = state%stebbsState%DeepSoilTemperature
      OutdoorAirStartTemperature = state%stebbsState%OutdoorAirStartTemperature
      IndoorAirStartTemperature = state%stebbsState%IndoorAirStartTemperature
      IndoorMassStartTemperature = state%stebbsState%IndoorMassStartTemperature
      WallIndoorSurfaceTemperature = state%stebbsState%WallIndoorSurfaceTemperature
      WallOutdoorSurfaceTemperature = state%stebbsState%WallOutdoorSurfaceTemperature
      RoofIndoorSurfaceTemperature = state%stebbsState%RoofIndoorSurfaceTemperature
      RoofOutdoorSurfaceTemperature = state%stebbsState%RoofOutdoorSurfaceTemperature
      WindowIndoorSurfaceTemperature = state%stebbsState%WindowIndoorSurfaceTemperature
      WindowOutdoorSurfaceTemperature = state%stebbsState%WindowOutdoorSurfaceTemperature
      GroundFloorIndoorSurfaceTemperature = state%stebbsState%GroundFloorIndoorSurfaceTemperature
      GroundFloorOutdoorSurfaceTemperature = state%stebbsState%GroundFloorOutdoorSurfaceTemperature
   END SUBROUTINE get_stebbs_state_temps_envelope

   !> Set STEBBS state temperature scalars (building envelope)
   SUBROUTINE set_stebbs_state_temps_envelope(state, &
         DeepSoilTemperature, OutdoorAirStartTemperature, &
         IndoorAirStartTemperature, IndoorMassStartTemperature, &
         WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature, &
         RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature, &
         WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature, &
         GroundFloorIndoorSurfaceTemperature, GroundFloorOutdoorSurfaceTemperature)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: DeepSoilTemperature, OutdoorAirStartTemperature
      REAL(KIND(1D0)), INTENT(IN) :: IndoorAirStartTemperature, IndoorMassStartTemperature
      REAL(KIND(1D0)), INTENT(IN) :: WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(IN) :: RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(IN) :: WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(IN) :: GroundFloorIndoorSurfaceTemperature, GroundFloorOutdoorSurfaceTemperature

      state%stebbsState%DeepSoilTemperature = DeepSoilTemperature
      state%stebbsState%OutdoorAirStartTemperature = OutdoorAirStartTemperature
      state%stebbsState%IndoorAirStartTemperature = IndoorAirStartTemperature
      state%stebbsState%IndoorMassStartTemperature = IndoorMassStartTemperature
      state%stebbsState%WallIndoorSurfaceTemperature = WallIndoorSurfaceTemperature
      state%stebbsState%WallOutdoorSurfaceTemperature = WallOutdoorSurfaceTemperature
      state%stebbsState%RoofIndoorSurfaceTemperature = RoofIndoorSurfaceTemperature
      state%stebbsState%RoofOutdoorSurfaceTemperature = RoofOutdoorSurfaceTemperature
      state%stebbsState%WindowIndoorSurfaceTemperature = WindowIndoorSurfaceTemperature
      state%stebbsState%WindowOutdoorSurfaceTemperature = WindowOutdoorSurfaceTemperature
      state%stebbsState%GroundFloorIndoorSurfaceTemperature = GroundFloorIndoorSurfaceTemperature
      state%stebbsState%GroundFloorOutdoorSurfaceTemperature = GroundFloorOutdoorSurfaceTemperature
   END SUBROUTINE set_stebbs_state_temps_envelope

   !> Get STEBBS state water tank temperatures
   SUBROUTINE get_stebbs_state_temps_water(state, &
         WaterTankTemperature, InternalWallWaterTankTemperature, ExternalWallWaterTankTemperature, &
         MainsWaterTemperature, DomesticHotWaterTemperatureInUseInBuilding, &
         InternalWallDHWVesselTemperature, ExternalWallDHWVesselTemperature)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: WaterTankTemperature, InternalWallWaterTankTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: ExternalWallWaterTankTemperature, MainsWaterTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: DomesticHotWaterTemperatureInUseInBuilding
      REAL(KIND(1D0)), INTENT(OUT) :: InternalWallDHWVesselTemperature, ExternalWallDHWVesselTemperature

      WaterTankTemperature = state%stebbsState%WaterTankTemperature
      InternalWallWaterTankTemperature = state%stebbsState%InternalWallWaterTankTemperature
      ExternalWallWaterTankTemperature = state%stebbsState%ExternalWallWaterTankTemperature
      MainsWaterTemperature = state%stebbsState%MainsWaterTemperature
      DomesticHotWaterTemperatureInUseInBuilding = state%stebbsState%DomesticHotWaterTemperatureInUseInBuilding
      InternalWallDHWVesselTemperature = state%stebbsState%InternalWallDHWVesselTemperature
      ExternalWallDHWVesselTemperature = state%stebbsState%ExternalWallDHWVesselTemperature
   END SUBROUTINE get_stebbs_state_temps_water

   !> Set STEBBS state water tank temperatures
   SUBROUTINE set_stebbs_state_temps_water(state, &
         WaterTankTemperature, InternalWallWaterTankTemperature, ExternalWallWaterTankTemperature, &
         MainsWaterTemperature, DomesticHotWaterTemperatureInUseInBuilding, &
         InternalWallDHWVesselTemperature, ExternalWallDHWVesselTemperature)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: WaterTankTemperature, InternalWallWaterTankTemperature
      REAL(KIND(1D0)), INTENT(IN) :: ExternalWallWaterTankTemperature, MainsWaterTemperature
      REAL(KIND(1D0)), INTENT(IN) :: DomesticHotWaterTemperatureInUseInBuilding
      REAL(KIND(1D0)), INTENT(IN) :: InternalWallDHWVesselTemperature, ExternalWallDHWVesselTemperature

      state%stebbsState%WaterTankTemperature = WaterTankTemperature
      state%stebbsState%InternalWallWaterTankTemperature = InternalWallWaterTankTemperature
      state%stebbsState%ExternalWallWaterTankTemperature = ExternalWallWaterTankTemperature
      state%stebbsState%MainsWaterTemperature = MainsWaterTemperature
      state%stebbsState%DomesticHotWaterTemperatureInUseInBuilding = DomesticHotWaterTemperatureInUseInBuilding
      state%stebbsState%InternalWallDHWVesselTemperature = InternalWallDHWVesselTemperature
      state%stebbsState%ExternalWallDHWVesselTemperature = ExternalWallDHWVesselTemperature
   END SUBROUTINE set_stebbs_state_temps_water

   !> Get STEBBS state storage heat flux
   SUBROUTINE get_stebbs_state_qs(state, QS_stebbs)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: QS_stebbs

      QS_stebbs = state%stebbsState%QS_stebbs
   END SUBROUTINE get_stebbs_state_qs

   !> Set STEBBS state storage heat flux
   SUBROUTINE set_stebbs_state_qs(state, QS_stebbs)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: QS_stebbs

      state%stebbsState%QS_stebbs = QS_stebbs
   END SUBROUTINE set_stebbs_state_qs

   !> Get STEBBS building runtime temperatures (roof and wall external temps)
   !> Note: These are the only runtime-modified arrays in buildings(:)
   SUBROUTINE get_stebbs_building_temps(state, bldg_idx, nlayer, Textroof_C, Textwall_C)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: bldg_idx, nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: Textroof_C, Textwall_C

      IF (ALLOCATED(state%stebbsState%buildings)) THEN
         IF (bldg_idx <= SIZE(state%stebbsState%buildings)) THEN
            IF (ALLOCATED(state%stebbsState%buildings(bldg_idx)%Textroof_C)) THEN
               Textroof_C = state%stebbsState%buildings(bldg_idx)%Textroof_C
            END IF
            IF (ALLOCATED(state%stebbsState%buildings(bldg_idx)%Textwall_C)) THEN
               Textwall_C = state%stebbsState%buildings(bldg_idx)%Textwall_C
            END IF
         END IF
      END IF
   END SUBROUTINE get_stebbs_building_temps

   !> Set STEBBS building runtime temperatures (roof and wall external temps)
   !> Note: These are the only runtime-modified arrays in buildings(:)
   SUBROUTINE set_stebbs_building_temps(state, bldg_idx, nlayer, Textroof_C, Textwall_C)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: bldg_idx, nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: Textroof_C, Textwall_C

      IF (ALLOCATED(state%stebbsState%buildings)) THEN
         IF (bldg_idx <= SIZE(state%stebbsState%buildings)) THEN
            IF (ALLOCATED(state%stebbsState%buildings(bldg_idx)%Textroof_C)) THEN
               state%stebbsState%buildings(bldg_idx)%Textroof_C = Textroof_C
            END IF
            IF (ALLOCATED(state%stebbsState%buildings(bldg_idx)%Textwall_C)) THEN
               state%stebbsState%buildings(bldg_idx)%Textwall_C = Textwall_C
            END IF
         END IF
      END IF
   END SUBROUTINE set_stebbs_building_temps

END MODULE module_accessor_stebbs
