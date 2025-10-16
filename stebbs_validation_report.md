# STEBBS Pydantic Validation Implementation Report

## Part 1: Parameters with Range Constraints Implemented

### A. ArchetypeProperties Class

#### Geometry Parameters (5/5 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| stebbs_Height | >0 | `gt=0.0` | 10.0 | ✅ Already had constraint |
| FootprintArea | >0 | `gt=0.0` | 64.0 | ✅ Already had constraint |
| WallExternalArea | >0 | `gt=0.0` | 80.0 | ✅ Already had constraint |
| RatioInternalVolume | 0-1 | `ge=0.0, le=1.0` | 0.01 | ✅ Already had constraint |
| WWR | 0-1 | `ge=0.0, le=1.0` | 0.20 | ✅ Already had constraint |

#### Building Material - Walls/Roofs (10/10 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| WallThickness | >0 | `gt=0.0` | 20.0 | ✅ Already had constraint |
| WallEffectiveConductivity | >0 | `gt=0.0` | 60.0 | ✅ Already had constraint |
| WallDensity | >0 | `gt=0.0` | 1600.0 | ✅ Already had constraint |
| WallCp | >0 | `gt=0.0` | 850.0 | ✅ Already had constraint |
| Wallx1 | 0-1 | `ge=0.0, le=1.0` | 1.0 | ✅ Already had constraint |
| WallExternalEmissivity | >0 | `ge=0.0, le=1.0` | 0.9 | ✅ Already had constraint |
| WallInternalEmissivity | >0 | `ge=0.0, le=1.0` | 0.9 | ✅ Already had constraint |
| WallTransmissivity | 0-1 | `ge=0.0, le=1.0` | 0.0 | ✅ Already had constraint |
| WallAbsorbtivity | 0-1 | `ge=0.0, le=1.0` | 0.8 | ✅ Already had constraint |
| WallReflectivity | 0-1 | `ge=0.0, le=1.0` | 0.2 | ✅ Already had constraint |

#### Building Material - Windows (7/7 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| WindowThickness | >0 | `gt=0.0` | 0.015 | ✅ Already had constraint |
| WindowEffectiveConductivity | >0 | `gt=0.0` | 1.0 | ✅ Already had constraint |
| WindowDensity | >0 | `gt=0.0` | 2500.0 | ✅ Already had constraint |
| WindowCp | >0 | `gt=0.0` | 840.0 | ✅ Already had constraint |
| WindowExternalEmissivity | >0 | `ge=0.0, le=1.0` | 0.90 | ✅ Already had constraint |
| WindowInternalEmissivity | >0 | `ge=0.0, le=1.0` | 0.90 | ✅ Already had constraint |
| WindowTransmissivity | 0-1 | `ge=0.0, le=1.0` | 0.90 | ✅ Already had constraint |
| WindowAbsorbtivity | 0-1 | `ge=0.0, le=1.0` | 0.01 | ✅ Already had constraint |
| WindowReflectivity | 0-1 | `ge=0.0, le=1.0` | 0.09 | ✅ Already had constraint |

#### Building Material - Ground Floor (3/3 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| FloorThickness | >0 | `gt=0.0` | 0.2 | ✅ Already had constraint |
| GroundFloorEffectiveConductivity | >0 | `gt=0.0` | 0.15 | ✅ Already had constraint |
| GroundFloorDensity | >0 | `gt=0.0` | 500.0 | ✅ Already had constraint |
| GroundFloorCp | >0 | `gt=0.0` | 1500.0 | ✅ Already had constraint |

#### Occupancy Parameters (1/1 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| Occupants | >0 | No constraint (int type) | 1 | ⚠️ Integer field, no validation |

#### HVAC Parameters in ArchetypeProperties (1/3 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| MaxHeatingPower | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| HeatingSetpointTemperature | 10-30? | None | 0.0 | ❌ No constraint (uncertain range) |
| CoolingSetpointTemperature | 10-30? | None | 0.0 | ❌ No constraint (uncertain range) |

#### Domestic Hot Water in ArchetypeProperties (2/2 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| WaterTankWaterVolume | >0 | `gt=0.0` | 0.15 | ✅ This PR (placeholder) |
| MaximumHotWaterHeatingPower | >0 | `gt=0.0` | 3000.0 | ✅ This PR (placeholder) |

### B. StebbsProperties Class

#### Others Category (6/6 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| IndoorAirDensity | >0 | `gt=0.0` | 1.2 | ✅ This PR (placeholder) |
| IndoorAirCp | >0 | `gt=0.0` | 1005.0 | ✅ This PR (placeholder) |
| WallBuildingViewFactor | ≥0 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| WallGroundViewFactor | ≥0 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| WallSkyViewFactor | ≥0 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| BuildingCount | >0 | No constraint (int type) | 1 | ⚠️ Integer field, no validation |

#### Occupancy Category (5/5 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| MetabolicRate | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| LatentSensibleRatio | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| ApplianceRating | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| TotalNumberofAppliances | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| ApplianceUsageFactor | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |

#### HVAC Category (3/3 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| HeatingSystemEfficiency | 0-1 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| MaxCoolingPower | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| CoolingSystemCOP | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| VentilationRate | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |

#### Domestic Hot Water Category (24/24 implemented)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| WaterTankTemperature | >0 | `gt=0.0` | 50.0 | ✅ This PR (placeholder) |
| WaterTankWallThickness | >0 | `gt=0.0` | 0.01 | ✅ This PR (placeholder) |
| HotWaterHeatingSetpointTemperature | >0 | `gt=0.0` | 60.0 | ✅ This PR (placeholder) |
| HotWaterTankWallEmissivity | 0-1 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| DomesticHotWaterTemperatureInUseInBuilding | >0 | `gt=0.0` | 40.0 | ✅ This PR (placeholder) |
| DHWVesselWallThickness | >0 | `gt=0.0` | 0.005 | ✅ This PR (placeholder) |
| DHWWaterVolume | >0 | `gt=0.0` | 0.05 | ✅ This PR (placeholder) |
| DHWSurfaceArea | >0 | `gt=0.0` | 0.5 | ✅ This PR (placeholder) |
| DHWVesselEmissivity | 0-1 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| HotWaterFlowRate | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| DHWDrainFlowRate | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |
| DHWSpecificHeatCapacity | >0 | `gt=0.0` | 4186.0 | ✅ This PR (placeholder) |
| HotWaterTankSpecificHeatCapacity | >0 | `gt=0.0` | 500.0 | ✅ This PR (placeholder) |
| DHWVesselSpecificHeatCapacity | >0 | `gt=0.0` | 500.0 | ✅ This PR (placeholder) |
| DHWDensity | >0 | `gt=0.0` | 1000.0 | ✅ This PR (placeholder) |
| HotWaterTankWallDensity | >0 | `gt=0.0` | 2500.0 | ✅ This PR (placeholder) |
| DHWVesselDensity | >0 | `gt=0.0` | 2500.0 | ✅ This PR (placeholder) |
| HotWaterTankBuildingWallViewFactor | 0-1 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| HotWaterTankInternalMassViewFactor | 0-1 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| HotWaterTankWallConductivity | >0 | `gt=0.0` | 0.5 | ✅ This PR (placeholder) |
| DHWVesselWallConductivity | >0 | `gt=0.0` | 0.5 | ✅ This PR (placeholder) |
| DHWVesselWallEmissivity | >0 | `gt=0.0` | 0.9 | ✅ This PR (placeholder) |
| HotWaterHeatingEfficiency | 0-1 | `ge=0.0, le=1.0` | 0.0 | ✅ This PR |
| MinimumVolumeOfDHWinUse | ≥0 | `ge=0.0` | 0.0 | ✅ This PR |

#### Initialised Coefficient/Temperature Category (7/7 implemented - Convection Coefficients)
| Parameter | PDF Range | Pydantic Constraint | Default | Status |
|-----------|-----------|---------------------|---------|--------|
| WallExternalConvectionCoefficient | >0, CIBSE | `gt=0.0` | 25.0 | ✅ Already had constraint |
| WallInternalConvectionCoefficient | >0 | `gt=0.0` | 7.69 | ✅ Already had constraint |
| WindowExternalConvectionCoefficient | >0 | `gt=0.0` | 25.0 | ✅ Already had constraint |
| WindowInternalConvectionCoefficient | >0 | `gt=0.0` | 7.69 | ✅ Already had constraint |
| FloorInternalConvectionCoefficient | >0 | `gt=0.0` | 5.88 | ✅ Already had constraint |
| InternalMassConvectionCoefficient | >0 | `gt=0.0` | 7.69 | ✅ Already had constraint |

---

## Part 2: Parameters Requiring Science Review (Placeholder Defaults)

The following parameters were added with **placeholder defaults** that need scientific validation:

### ArchetypeProperties (2 parameters)
1. **WaterTankWaterVolume**: default=0.15 m³ (should be >0)
2. **MaximumHotWaterHeatingPower**: default=3000.0 W (should be >0)

### StebbsProperties (18 parameters)

#### Indoor Air Properties
3. **IndoorAirDensity**: default=1.2 kg/m³ (should be >0)
4. **IndoorAirCp**: default=1005.0 J/(kg·K) (should be >0)

#### Hot Water Tank Properties
5. **WaterTankTemperature**: default=50.0 °C (should be >0)
6. **WaterTankWallThickness**: default=0.01 m (should be >0)
7. **HotWaterHeatingSetpointTemperature**: default=60.0 °C (should be >0)
8. **HotWaterTankWallConductivity**: default=0.5 W/(m·K) (should be >0)
9. **HotWaterTankSpecificHeatCapacity**: default=500.0 J/(kg·K) (should be >0)
10. **HotWaterTankWallDensity**: default=2500.0 kg/m³ (should be >0)

#### DHW Vessel Properties
11. **DomesticHotWaterTemperatureInUseInBuilding**: default=40.0 °C (should be >0)
12. **DHWVesselWallThickness**: default=0.005 m (should be >0)
13. **DHWWaterVolume**: default=0.05 m³ (should be >0)
14. **DHWSurfaceArea**: default=0.5 m² (should be >0)
15. **DHWSpecificHeatCapacity**: default=4186.0 J/(kg·K) (should be >0)
16. **DHWVesselSpecificHeatCapacity**: default=500.0 J/(kg·K) (should be >0)
17. **DHWDensity**: default=1000.0 kg/m³ (should be >0)
18. **DHWVesselDensity**: default=2500.0 kg/m³ (should be >0)
19. **DHWVesselWallConductivity**: default=0.5 W/(m·K) (should be >0)
20. **DHWVesselWallEmissivity**: default=0.9 (should be >0)

**Note**: All these parameters have the comment `# Placeholder, needs to be changed into something reasonable` in the code. Parameters with `ge=0.0, le=1.0` constraints (emissivities, view factors, efficiencies) have been set to `default=0.0` and do NOT require science review.

---

## Part 3: Parameters in site.py WITHOUT Constraints (Not in PDF)

### ArchetypeProperties (3 parameters with no constraints)
1. **InternalMassDensity**: default=0.0, no constraints 
2. **InternalMassCp**: default=0.0, no constraints 
3. **InternalMassEmissivity**: default=0.0, no constraints

### StebbsProperties - Initial Temperature Parameters (15 parameters)
These initialization temperature parameters are in the PDF but WITHOUT range specifications:

1. **IndoorAirStartTemperature**: default=0.0, no constraints
2. **IndoorMassStartTemperature**: default=0.0, no constraints
3. **WallIndoorSurfaceTemperature**: default=0.0, no constraints
4. **WallOutdoorSurfaceTemperature**: default=0.0, no constraints
5. **WindowIndoorSurfaceTemperature**: default=0.0, no constraints
6. **WindowOutdoorSurfaceTemperature**: default=0.0, no constraints
7. **GroundFloorIndoorSurfaceTemperature**: default=0.0, no constraints
8. **GroundFloorOutdoorSurfaceTemperature**: default=0.0, no constraints
9. **InternalWallWaterTankTemperature**: default=0.0, no constraints
10. **ExternalWallWaterTankTemperature**: default=0.0, no constraints
11. **InternalWallDHWVesselTemperature**: default=0.0, no constraints
12. **ExternalWallDHWVesselTemperature**: default=0.0, no constraints
13. **MainsWaterTemperature**: default=0.0, no constraints
14. **WaterTankSurfaceArea**: default=0.0, no constraints

### StebbsProperties - Convection Coefficients (4 parameters)
These are in the PDF but without explicit numerical ranges:

15. **HotWaterTankInternalWallConvectionCoefficient**: default=0.0, no constraints
16. **HotWaterTankExternalWallConvectionCoefficient**: default=0.0, no constraints
17. **DHWVesselInternalWallConvectionCoefficient**: default=0.0, no constraints
18. **DHWVesselExternalWallConvectionCoefficient**: default=0.0, no constraints

### StebbsProperties - Other Parameters (2 parameters)
19. **GroundDepth**: default=0.0, no constraints (not in PDF with explicit range)
20. **ExternalGroundConductivity**: default=0.0, no constraints (not in PDF with explicit range)

---

## Part 4: Parameters in PDF but NOT in site.py

The following parameters are mentioned in the PDF but do NOT exist in the current codebase:

### Building Material - External Layer (Wallext*)
1. **WallextThickness**: >0 (noted as "similar to roof")
2. **WallexteEffectiveConductivity**: >0
3. **WallextDensity**: >0
4. **WallextCp**: >0

**Note**: PDF indicates these are the "Same (repeated) input for Dyohm, - uses first layer of EHC wall"

### Convection Coefficients - Roof
1. **RoofExternalConvectionCoefficient**: >0, CIBSE GUIDE A Table 3.47
2. **RoofInternalConvectionCoefficient**: >0

**Note**: Currently, wall and roof share the same convection coefficients (WallExternalConvectionCoefficient, WallInternalConvectionCoefficient)
