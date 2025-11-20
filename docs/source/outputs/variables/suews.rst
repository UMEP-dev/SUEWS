.. meta::
   :description: SUEWS SUEWS output variables
   :keywords: SUEWS, output, suews, variables

.. _suews_output:

.. index::
   single: SUEWS (output group)
   single: Output; SUEWS

SUEWS Output Variables
======================

Core SUEWS energy balance, water balance, and meteorological outputs.

This group contains 85 output variables.

.. index::
   single: AddWater (output variable)
   single: SUEWS; AddWater

.. yaml:option:: AddWater

   Additional water from other grids

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: AlbBulk (output variable)
   single: SUEWS; AlbBulk

.. yaml:option:: AlbBulk

   Bulk albedo

   :Unit: dimensionless
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: AlbSnow (output variable)
   single: SUEWS; AlbSnow

.. yaml:option:: AlbSnow

   Snow albedo

   :Unit: -
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)
   :Format: ``f94``

.. index::
   single: Azimuth (output variable)
   single: SUEWS; Azimuth

.. yaml:option:: Azimuth

   Solar azimuth angle

   :Unit: degree
   :Aggregation: Last (final value in period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: Drainage (output variable)
   single: SUEWS; Drainage

.. yaml:option:: Drainage

   Drainage

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: Evap (output variable)
   single: SUEWS; Evap

.. yaml:option:: Evap

   Evaporation

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: Fc (output variable)
   single: SUEWS; Fc

.. yaml:option:: Fc

   CO2 flux

   :Unit: umol |m^-2| |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: FcBuild (output variable)
   single: SUEWS; FcBuild

.. yaml:option:: FcBuild

   CO2 flux from buildings

   :Unit: umol |m^-2| |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: FcMetab (output variable)
   single: SUEWS; FcMetab

.. yaml:option:: FcMetab

   CO2 flux from metabolism

   :Unit: umol |m^-2| |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: FcPhoto (output variable)
   single: SUEWS; FcPhoto

.. yaml:option:: FcPhoto

   CO2 flux from photosynthesis

   :Unit: umol |m^-2| |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: FcPoint (output variable)
   single: SUEWS; FcPoint

.. yaml:option:: FcPoint

   CO2 flux from point source

   :Unit: umol |m^-2| |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: FcRespi (output variable)
   single: SUEWS; FcRespi

.. yaml:option:: FcRespi

   CO2 flux from respiration

   :Unit: umol |m^-2| |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: FcTraff (output variable)
   single: SUEWS; FcTraff

.. yaml:option:: FcTraff

   CO2 flux from traffic

   :Unit: umol |m^-2| |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: Fcld (output variable)
   single: SUEWS; Fcld

.. yaml:option:: Fcld

   Cloud fraction

   :Unit: dimensionless
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: FlowCh (output variable)
   single: SUEWS; FlowCh

.. yaml:option:: FlowCh

   Additional flow into water body

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: Irr (output variable)
   single: SUEWS; Irr

.. yaml:option:: Irr

   Irrigation

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: Kdown (output variable)
   single: SUEWS; Kdown

.. yaml:option:: Kdown

   Incoming shortwave radiation

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: Kup (output variable)
   single: SUEWS; Kup

.. yaml:option:: Kup

   Outgoing shortwave radiation

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: LAI (output variable)
   single: SUEWS; LAI

.. yaml:option:: LAI

   Leaf area index

   :Unit: |m^2| |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: Ldown (output variable)
   single: SUEWS; Ldown

.. yaml:option:: Ldown

   Incoming longwave radiation

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: Lob (output variable)
   single: SUEWS; Lob

.. yaml:option:: Lob

   Obukhov length

   :Unit: m
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f146``

.. index::
   single: Lup (output variable)
   single: SUEWS; Lup

.. yaml:option:: Lup

   Outgoing longwave radiation

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: MeltWStore (output variable)
   single: SUEWS; MeltWStore

.. yaml:option:: MeltWStore

   Meltwater store

   :Unit: mm
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)

.. index::
   single: MeltWater (output variable)
   single: SUEWS; MeltWater

.. yaml:option:: MeltWater

   Meltwater

   :Unit: mm
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)

.. index::
   single: NWtrState (output variable)
   single: SUEWS; NWtrState

.. yaml:option:: NWtrState

   Surface wetness state (non-water surfaces)

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: Q2 (output variable)
   single: SUEWS; Q2

.. yaml:option:: Q2

   Specific humidity at 2 m

   :Unit: g |kg^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: QE (output variable)
   single: SUEWS; QE

.. yaml:option:: QE

   Latent heat flux

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: QElumps (output variable)
   single: SUEWS; QElumps

.. yaml:option:: QElumps

   Latent heat flux (using LUMPS)

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: QF (output variable)
   single: SUEWS; QF

.. yaml:option:: QF

   Anthropogenic heat flux

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: QH (output variable)
   single: SUEWS; QH

.. yaml:option:: QH

   Sensible heat flux

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: QHinit (output variable)
   single: SUEWS; QHinit

.. yaml:option:: QHinit

   Init Sensible heat flux for TStar

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: QHlumps (output variable)
   single: SUEWS; QHlumps

.. yaml:option:: QHlumps

   Sensible heat flux (using LUMPS)

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: QHresis (output variable)
   single: SUEWS; QHresis

.. yaml:option:: QHresis

   Sensible heat flux (resistance method)

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: QM (output variable)
   single: SUEWS; QM

.. yaml:option:: QM

   Snow-related heat exchange

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)
   :Format: ``f106``

.. index::
   single: QMFreeze (output variable)
   single: SUEWS; QMFreeze

.. yaml:option:: QMFreeze

   Internal energy change

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)
   :Format: ``f146``

.. index::
   single: QMRain (output variable)
   single: SUEWS; QMRain

.. yaml:option:: QMRain

   Heat released by rain on snow

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)
   :Format: ``f106``

.. index::
   single: QN (output variable)
   single: SUEWS; QN

.. yaml:option:: QN

   Net all-wave radiation

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: QNSnow (output variable)
   single: SUEWS; QNSnow

.. yaml:option:: QNSnow

   Net all-wave radiation for snow area

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)
   :Format: ``f94``

.. index::
   single: QNSnowFr (output variable)
   single: SUEWS; QNSnowFr

.. yaml:option:: QNSnowFr

   Net all-wave radiation for non-snow area

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)
   :Format: ``f94``

.. index::
   single: QS (output variable)
   single: SUEWS; QS

.. yaml:option:: QS

   Net storage heat flux

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: RA (output variable)
   single: SUEWS; RA

.. yaml:option:: RA

   Aerodynamic resistance

   :Unit: s |m^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: RH2 (output variable)
   single: SUEWS; RH2

.. yaml:option:: RH2

   Relative humidity at 2 m

   :Unit: %
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: RO (output variable)
   single: SUEWS; RO

.. yaml:option:: RO

   Runoff

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: ROImp (output variable)
   single: SUEWS; ROImp

.. yaml:option:: ROImp

   Runoff over impervious surfaces

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: ROPipe (output variable)
   single: SUEWS; ROPipe

.. yaml:option:: ROPipe

   Runoff to pipes

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: ROSoil (output variable)
   single: SUEWS; ROSoil

.. yaml:option:: ROSoil

   Runoff to soil

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: ROVeg (output variable)
   single: SUEWS; ROVeg

.. yaml:option:: ROVeg

   Runoff over vegetated surfaces

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: ROWater (output variable)
   single: SUEWS; ROWater

.. yaml:option:: ROWater

   Runoff for water surface

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: RS (output variable)
   single: SUEWS; RS

.. yaml:option:: RS

   Surface resistance

   :Unit: s |m^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: Rain (output variable)
   single: SUEWS; Rain

.. yaml:option:: Rain

   Rain

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: SMD (output variable)
   single: SUEWS; SMD

.. yaml:option:: SMD

   Soil Moisture Deficit

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: SMDBSoil (output variable)
   single: SUEWS; SMDBSoil

.. yaml:option:: SMDBSoil

   Soil moisture deficit for bare soil surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: SMDBldgs (output variable)
   single: SUEWS; SMDBldgs

.. yaml:option:: SMDBldgs

   Soil moisture deficit for building surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: SMDDecTr (output variable)
   single: SUEWS; SMDDecTr

.. yaml:option:: SMDDecTr

   Soil moisture deficit for deciduous tree surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: SMDEveTr (output variable)
   single: SUEWS; SMDEveTr

.. yaml:option:: SMDEveTr

   Soil moisture deficit for evergreen tree surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: SMDGrass (output variable)
   single: SUEWS; SMDGrass

.. yaml:option:: SMDGrass

   Soil moisture deficit for grass surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: SMDPaved (output variable)
   single: SUEWS; SMDPaved

.. yaml:option:: SMDPaved

   Soil moisture deficit for paved surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: SWE (output variable)
   single: SUEWS; SWE

.. yaml:option:: SWE

   Snow water equivalent

   :Unit: mm
   :Aggregation: Average (mean over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)

.. index::
   single: SnowCh (output variable)
   single: SUEWS; SnowCh

.. yaml:option:: SnowCh

   Change in snow pack

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)

.. index::
   single: SnowRBldgs (output variable)
   single: SUEWS; SnowRBldgs

.. yaml:option:: SnowRBldgs

   Snow removed from building surface

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)
   :Format: ``f94``

.. index::
   single: SnowRPaved (output variable)
   single: SUEWS; SnowRPaved

.. yaml:option:: SnowRPaved

   Snow removed from paved surface

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Snow Detailed (level 2 - snow-specific detailed output)
   :Format: ``f94``

.. index::
   single: StBSoil (output variable)
   single: SUEWS; StBSoil

.. yaml:option:: StBSoil

   Surface wetness state for bare soil surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: StBldgs (output variable)
   single: SUEWS; StBldgs

.. yaml:option:: StBldgs

   Surface wetness state for building surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: StDecTr (output variable)
   single: SUEWS; StDecTr

.. yaml:option:: StDecTr

   Surface wetness state for deciduous tree surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: StEveTr (output variable)
   single: SUEWS; StEveTr

.. yaml:option:: StEveTr

   Surface wetness state for evergreen tree surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: StGrass (output variable)
   single: SUEWS; StGrass

.. yaml:option:: StGrass

   Surface wetness state for grass surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: StPaved (output variable)
   single: SUEWS; StPaved

.. yaml:option:: StPaved

   Surface wetness state for paved surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: StWater (output variable)
   single: SUEWS; StWater

.. yaml:option:: StWater

   Surface wetness state for water surface

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Extended (level 1 - optional extended output)

.. index::
   single: State (output variable)
   single: SUEWS; State

.. yaml:option:: State

   Surface Wetness State

   :Unit: mm
   :Aggregation: Last (final value in period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: SurfCh (output variable)
   single: SUEWS; SurfCh

.. yaml:option:: SurfCh

   Surface moisture change

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f146``

.. index::
   single: T2 (output variable)
   single: SUEWS; T2

.. yaml:option:: T2

   Air temperature at 2 m

   :Unit: degC
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: TStar (output variable)
   single: SUEWS; TStar

.. yaml:option:: TStar

   Temperature scale

   :Unit: m |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: TotCh (output variable)
   single: SUEWS; TotCh

.. yaml:option:: TotCh

   Surface and soil moisture change

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f146``

.. index::
   single: Ts (output variable)
   single: SUEWS; Ts

.. yaml:option:: Ts

   Skin temperature

   :Unit: degC
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: Tsurf (output variable)
   single: SUEWS; Tsurf

.. yaml:option:: Tsurf

   Bulk surface temperature

   :Unit: degC
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: U10 (output variable)
   single: SUEWS; U10

.. yaml:option:: U10

   Wind speed at 10 m

   :Unit: m |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: UStar (output variable)
   single: SUEWS; UStar

.. yaml:option:: UStar

   Friction velocity

   :Unit: m |s^-1|
   :Aggregation: Average (mean over period)
   :Output Level: Default (level 0 - always included)
   :Format: ``f94``

.. index::
   single: WUDecTr (output variable)
   single: SUEWS; WUDecTr

.. yaml:option:: WUDecTr

   Water use for deciduous trees

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: WUEveTr (output variable)
   single: SUEWS; WUEveTr

.. yaml:option:: WUEveTr

   Water use for evergreen trees

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: WUGrass (output variable)
   single: SUEWS; WUGrass

.. yaml:option:: WUGrass

   Water use for grass

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: WUInt (output variable)
   single: SUEWS; WUInt

.. yaml:option:: WUInt

   Internal water use

   :Unit: mm
   :Aggregation: Sum (cumulative over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: Zenith (output variable)
   single: SUEWS; Zenith

.. yaml:option:: Zenith

   Solar zenith angle

   :Unit: degree
   :Aggregation: Last (final value in period)
   :Output Level: Default (level 0 - always included)

.. index::
   single: z0m (output variable)
   single: SUEWS; z0m

.. yaml:option:: z0m

   Roughness length for momentum

   :Unit: m
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: zL (output variable)
   single: SUEWS; zL

.. yaml:option:: zL

   Stability scale

   :Unit: -
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``

.. index::
   single: zdm (output variable)
   single: SUEWS; zdm

.. yaml:option:: zdm

   Zero-plane displacement height

   :Unit: m
   :Aggregation: Average (mean over period)
   :Output Level: Extended (level 1 - optional extended output)
   :Format: ``f94``
