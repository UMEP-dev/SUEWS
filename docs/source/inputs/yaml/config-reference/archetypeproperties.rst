.. meta::
   :description: SUEWS YAML configuration for archetype properties parameters
   :keywords: SUEWS, YAML, archetypeproperties, parameters, configuration

.. _archetypeproperties:

.. index::
   single: ArchetypeProperties (YAML parameter)
   single: YAML; ArchetypeProperties

Archetype Properties
====================

Urban morphology and archetype properties.

**Parameters:**

.. index::
   single: BuildingType (YAML parameter)
   single: ArchetypeProperties; BuildingType

.. option:: BuildingType

   :Sample value: ``'SampleType'``

.. index::
   single: BuildingName (YAML parameter)
   single: ArchetypeProperties; BuildingName

.. option:: BuildingName

   :Sample value: ``'SampleBuilding'``

.. index::
   single: BuildingCount (YAML parameter)
   single: ArchetypeProperties; BuildingCount

.. option:: BuildingCount

   Number of buildings of this archetype [-]

   :Unit: dimensionless
   :Sample value: ``1``

.. index::
   single: Occupants (YAML parameter)
   single: ArchetypeProperties; Occupants

.. option:: Occupants

   Number of occupants present in building [-]

   :Unit: dimensionless
   :Sample value: ``1``

.. index::
   single: stebbs_Height (YAML parameter)
   single: ArchetypeProperties; stebbs_Height

.. option:: stebbs_Height

   Building height [m]

   :Unit: m
   :Sample value: ``10.0``

.. index::
   single: FootprintArea (YAML parameter)
   single: ArchetypeProperties; FootprintArea

.. option:: FootprintArea

   Building footprint area [m2]

   :Unit: |m^2|
   :Sample value: ``64.0``

.. index::
   single: WallExternalArea (YAML parameter)
   single: ArchetypeProperties; WallExternalArea

.. option:: WallExternalArea

   External wall area (including window area) [m2]

   :Unit: |m^2|
   :Sample value: ``80.0``

.. index::
   single: RatioInternalVolume (YAML parameter)
   single: ArchetypeProperties; RatioInternalVolume

.. option:: RatioInternalVolume

   Ratio of internal mass volume to total building volume [-]

   :Unit: dimensionless
   :Sample value: ``0.01``

.. index::
   single: WWR (YAML parameter)
   single: ArchetypeProperties; WWR

.. option:: WWR

   window to wall ratio [-]

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: WallThickness (YAML parameter)
   single: ArchetypeProperties; WallThickness

.. option:: WallThickness

   Thickness of external wall and roof (weighted) [m]

   :Unit: m
   :Sample value: ``20.0``

.. index::
   single: WallEffectiveConductivity (YAML parameter)
   single: ArchetypeProperties; WallEffectiveConductivity

.. option:: WallEffectiveConductivity

   Effective thermal conductivity of walls and roofs (weighted) [W m-1 K-1]

   :Unit: W |m^-1| |K^-1|
   :Sample value: ``60.0``

.. index::
   single: WallDensity (YAML parameter)
   single: ArchetypeProperties; WallDensity

.. option:: WallDensity

   Effective density of the walls and roof (weighted) [kg m-3]

   :Unit: kg |m^-3|
   :Sample value: ``1600.0``

.. index::
   single: WallCp (YAML parameter)
   single: ArchetypeProperties; WallCp

.. option:: WallCp

   Effective specific heat capacity of walls and roof (weighted) [J kg-1 K-1]

   :Unit: J |kg^-1| |K^-1|
   :Sample value: ``850.0``

.. index::
   single: Wallx1 (YAML parameter)
   single: ArchetypeProperties; Wallx1

.. option:: Wallx1

   Weighting factor for heat capacity of walls and roof [-]

   :Unit: dimensionless
   :Sample value: ``1.0``

.. index::
   single: WallExternalEmissivity (YAML parameter)
   single: ArchetypeProperties; WallExternalEmissivity

.. option:: WallExternalEmissivity

   Emissivity of the external surface of walls and roof [-]

   :Unit: dimensionless
   :Sample value: ``0.9``

.. index::
   single: WallInternalEmissivity (YAML parameter)
   single: ArchetypeProperties; WallInternalEmissivity

.. option:: WallInternalEmissivity

   Emissivity of the internal surface of walls and roof [-]

   :Unit: dimensionless
   :Sample value: ``0.9``

.. index::
   single: WallTransmissivity (YAML parameter)
   single: ArchetypeProperties; WallTransmissivity

.. option:: WallTransmissivity

   Transmissivity of walls and roof [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: WallAbsorbtivity (YAML parameter)
   single: ArchetypeProperties; WallAbsorbtivity

.. option:: WallAbsorbtivity

   Absorbtivity of walls and roof [-]

   :Unit: dimensionless
   :Sample value: ``0.8``

.. index::
   single: WallReflectivity (YAML parameter)
   single: ArchetypeProperties; WallReflectivity

.. option:: WallReflectivity

   Reflectivity of the external surface of walls and roof [-]

   :Unit: dimensionless
   :Sample value: ``0.2``

.. index::
   single: FloorThickness (YAML parameter)
   single: ArchetypeProperties; FloorThickness

.. option:: FloorThickness

   Thickness of ground floor [m]

   :Unit: m
   :Sample value: ``0.2``

.. index::
   single: GroundFloorEffectiveConductivity (YAML parameter)
   single: ArchetypeProperties; GroundFloorEffectiveConductivity

.. option:: GroundFloorEffectiveConductivity

   Effective thermal conductivity of ground floor [W m-1 K-1]

   :Unit: W |m^-1| |K^-1|
   :Sample value: ``0.15``

.. index::
   single: GroundFloorDensity (YAML parameter)
   single: ArchetypeProperties; GroundFloorDensity

.. option:: GroundFloorDensity

   Density of the ground floor [kg m-3]

   :Unit: kg |m^-3|
   :Sample value: ``500.0``

.. index::
   single: GroundFloorCp (YAML parameter)
   single: ArchetypeProperties; GroundFloorCp

.. option:: GroundFloorCp

   Effective specific heat capacity of the ground floor [J kg-1 K-1]

   :Unit: J |kg^-1| |K^-1|
   :Sample value: ``1500.0``

.. index::
   single: WindowThickness (YAML parameter)
   single: ArchetypeProperties; WindowThickness

.. option:: WindowThickness

   Window thickness [m]

   :Unit: m
   :Sample value: ``0.015``

.. index::
   single: WindowEffectiveConductivity (YAML parameter)
   single: ArchetypeProperties; WindowEffectiveConductivity

.. option:: WindowEffectiveConductivity

   Effective thermal conductivity of windows [W m-1 K-1]

   :Unit: W |m^-1| |K^-1|
   :Sample value: ``1.0``

.. index::
   single: WindowDensity (YAML parameter)
   single: ArchetypeProperties; WindowDensity

.. option:: WindowDensity

   Effective density of the windows [kg m-3]

   :Unit: kg |m^-3|
   :Sample value: ``2500.0``

.. index::
   single: WindowCp (YAML parameter)
   single: ArchetypeProperties; WindowCp

.. option:: WindowCp

   Effective specific heat capacity of windows [J kg-1 K-1]

   :Unit: J |kg^-1| |K^-1|
   :Sample value: ``840.0``

.. index::
   single: WindowExternalEmissivity (YAML parameter)
   single: ArchetypeProperties; WindowExternalEmissivity

.. option:: WindowExternalEmissivity

   Emissivity of the external surface of windows [-]

   :Unit: dimensionless
   :Sample value: ``0.9``

.. index::
   single: WindowInternalEmissivity (YAML parameter)
   single: ArchetypeProperties; WindowInternalEmissivity

.. option:: WindowInternalEmissivity

   Emissivity of the internal surface of windows [-]

   :Unit: dimensionless
   :Sample value: ``0.9``

.. index::
   single: WindowTransmissivity (YAML parameter)
   single: ArchetypeProperties; WindowTransmissivity

.. option:: WindowTransmissivity

   Transmissivity of windows [-]

   :Unit: dimensionless
   :Sample value: ``0.9``

.. index::
   single: WindowAbsorbtivity (YAML parameter)
   single: ArchetypeProperties; WindowAbsorbtivity

.. option:: WindowAbsorbtivity

   Absorbtivity of windows [-]

   :Unit: dimensionless
   :Sample value: ``0.01``

.. index::
   single: WindowReflectivity (YAML parameter)
   single: ArchetypeProperties; WindowReflectivity

.. option:: WindowReflectivity

   Reflectivity of the external surface of windows [-]

   :Unit: dimensionless
   :Sample value: ``0.09``

.. index::
   single: InternalMassDensity (YAML parameter)
   single: ArchetypeProperties; InternalMassDensity

.. option:: InternalMassDensity

   Effective density of the internal mass [kg m-3]

   :Unit: kg |m^-3|
   :Sample value: ``0.0``

.. index::
   single: InternalMassCp (YAML parameter)
   single: ArchetypeProperties; InternalMassCp

.. option:: InternalMassCp

   Specific heat capacity of internal mass [J kg-1 K-1]

   :Unit: J |kg^-1| |K^-1|
   :Sample value: ``0.0``

.. index::
   single: InternalMassEmissivity (YAML parameter)
   single: ArchetypeProperties; InternalMassEmissivity

.. option:: InternalMassEmissivity

   Emissivity of internal mass [-]

   :Unit: dimensionless
   :Sample value: ``0.0``

.. index::
   single: MaxHeatingPower (YAML parameter)
   single: ArchetypeProperties; MaxHeatingPower

.. option:: MaxHeatingPower

   Maximum power demand of heating system [W]

   :Unit: W
   :Sample value: ``0.0``

.. index::
   single: WaterTankWaterVolume (YAML parameter)
   single: ArchetypeProperties; WaterTankWaterVolume

.. option:: WaterTankWaterVolume

   Volume of water in hot water tank [m3]

   :Unit: |m^3|
   :Sample value: ``0.0``

.. index::
   single: MaximumHotWaterHeatingPower (YAML parameter)
   single: ArchetypeProperties; MaximumHotWaterHeatingPower

.. option:: MaximumHotWaterHeatingPower

   Maximum power demand of water heating system [W]

   :Unit: W
   :Sample value: ``0.0``

.. index::
   single: HeatingSetpointTemperature (YAML parameter)
   single: ArchetypeProperties; HeatingSetpointTemperature

.. option:: HeatingSetpointTemperature

   Heating setpoint temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: CoolingSetpointTemperature (YAML parameter)
   single: ArchetypeProperties; CoolingSetpointTemperature

.. option:: CoolingSetpointTemperature

   Cooling setpoint temperature [degC]

   :Unit: degC
   :Sample value: ``0.0``

.. index::
   single: ref (YAML parameter)
   single: ArchetypeProperties; ref

.. option:: ref

   :Default: Required - must be specified

   The ``ref`` parameter group is defined by the :doc:`reference` structure.
