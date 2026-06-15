.. _scheme_reference:

Scheme Reference
================

Model cards for each physics scheme in SUEWS -- scientific basis, evaluation evidence, technical characteristics, and operational status at a glance.

.. note::
   Auto-generated from YAML model card files.
   See ``src/supy/model_cards/`` for the source data.

.. tab-set::

   .. tab-item:: Net Radiation

      **6 schemes** -- select a ``NetRadiationMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Ldown Observed
            :link: model_card_narp
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Models Q* using NARP (Net All-wave Radiation Parameterization; Offerle et al. 2003, Loridan et al. 2011) with observed longwave down radiation (L↓) from forcing file

            ``NetRadiationMethod`` = 1

         .. grid-item-card:: Ldown Cloud
            :link: model_card_narp
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Models Q* using NARP with L↓ estimated from cloud cover fraction

            ``NetRadiationMethod`` = 2

         .. grid-item-card:: Ldown Air
            :link: model_card_narp
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Models Q* using NARP with L↓ estimated from air temperature and relative humidity

            ``NetRadiationMethod`` = 3

         .. grid-item-card:: Ldown Ss Observed
            :link: model_card_spartacus
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-warning-line:`compute: high` :bdg-warning-line:`data prep: high`

            SPARTACUS-Surface integration with observed L↓ (experimental)

            ``NetRadiationMethod`` = 1001

         .. grid-item-card:: Ldown Ss Cloud
            :link: model_card_spartacus
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-warning-line:`compute: high` :bdg-warning-line:`data prep: high`

            SPARTACUS-Surface integration with L↓ from cloud fraction (experimental)

            ``NetRadiationMethod`` = 1002

         .. grid-item-card:: Ldown Ss Air
            :link: model_card_spartacus
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-warning-line:`compute: high` :bdg-warning-line:`data prep: high`

            SPARTACUS-Surface integration with L↓ from air temperature/humidity (experimental)

            ``NetRadiationMethod`` = 1003

   .. tab-item:: Albedo Uniformity

      **1 scheme** -- select a ``SameAlbedoWall`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Uniform Wall/Roof Albedo Assumption
            :link: model_card_same_albedo
            :link-type: ref

            :bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Controls whether all walls (or roofs) within the urban canopy share the same albedo value, simplifying the radiation calculation for multi-layer schemes.

            ``SameAlbedoWall`` = 0, 1

   .. tab-item:: Storage Heat Flux

      **6 schemes** -- select a ``StorageHeatMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Objective Hysteresis Model
            :link: model_card_ohm
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            Calculates storage heat flux (delta QS) using empirically-fitted hysteresis relations between net all-wave radiation and its rate of change.

            ``StorageHeatMethod`` = 1

         .. grid-item-card:: Analytical Objective Hysteresis Model
            :link: model_card_anohm
            :link-type: ref

            :bdg-danger:`deprecated` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            Calculates storage heat flux using an analytical extension of OHM with coefficients derived from surface thermal properties.

            ``StorageHeatMethod`` = 3

         .. grid-item-card:: Element Surface Temperature Method
            :link: model_card_estm
            :link-type: ref

            :bdg-danger:`deprecated` :bdg-success-line:`peer-reviewed` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

            Calculates storage heat flux and surface temperatures using 1D heat conduction through urban surface elements.

            ``StorageHeatMethod`` = 4

         .. grid-item-card:: Explicit Heat Conduction
            :link: model_card_ehc
            :link-type: ref

            :bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

            Calculates storage heat flux using explicit 1D heat conduction through urban facets (roof, wall, ground) with separate surface temperature outputs for each element.

            ``StorageHeatMethod`` = 5

         .. grid-item-card:: Dynamic Objective Hysteresis Model
            :link: model_card_dyohm
            :link-type: ref

            :bdg-warning:`experimental` :bdg-info-line:`preprint` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            Extends OHM by dynamically calculating the hysteresis coefficients (a1, a2, a3) from material thermal properties and meteorological conditions, removing the need for empirically pre-calibrated values.

            ``StorageHeatMethod`` = 6

         .. grid-item-card:: Surface Temperature Energy Balance Based Scheme
            :link: model_card_stebbs
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

            Calculates facet-level surface temperatures and storage heat flux for buildings by solving explicit energy balances for roof, wall, and ground surfaces with multi-layer heat conduction.

            ``StorageHeatMethod`` = 7

   .. tab-item:: OHM QF Inclusion

      **1 scheme** -- select a ``OhmIncQf`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: OHM Anthropogenic Heat Inclusion
            :link: model_card_ohm_inc_qf
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Controls whether anthropogenic heat flux (QF) is included in the OHM storage heat calculation, switching between Q* only and Q*+QF as input.

            ``OhmIncQf`` = 0, 1

   .. tab-item:: Emissions

      **21 schemes** -- select a ``EmissionsMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Observed
            :link: model_card_qf
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            Uses observed QF values from forcing file (set to zero to exclude QF from energy balance)

            ``EmissionsMethod`` = 0

         .. grid-item-card:: L11
            :link: model_card_qf
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            Loridan et al. (2011) SAHP method with air temperature and population density

            ``EmissionsMethod`` = 1

         .. grid-item-card:: J11
            :link: model_card_qf
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            Järvi et al. (2011) SAHP_2 method with heating/cooling degree days

            ``EmissionsMethod`` = 2

         .. grid-item-card:: L11 Updated
            :link: model_card_qf
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            Modified Loridan method using daily mean air temperature

            ``EmissionsMethod`` = 3

         .. grid-item-card:: J19
            :link: model_card_qf
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            Järvi et al. (2019) method with building energy, metabolism, and traffic

            ``EmissionsMethod`` = 4

         .. grid-item-card:: J19 Updated
            :link: model_card_qf
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

            As method 4 but also calculates CO2 emissions

            ``EmissionsMethod`` = 5

         .. grid-item-card:: Biogen Rect L11
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Rectangular hyperbola photosynthesis + L11 QF (experimental)

            ``EmissionsMethod`` = 11

         .. grid-item-card:: Biogen Rect J11
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Rectangular hyperbola photosynthesis + J11 QF (experimental)

            ``EmissionsMethod`` = 12

         .. grid-item-card:: Biogen Rect L11U
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Rectangular hyperbola photosynthesis + L11_UPDATED QF (experimental)

            ``EmissionsMethod`` = 13

         .. grid-item-card:: Biogen Rect J19
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Rectangular hyperbola photosynthesis + J19 QF (experimental)

            ``EmissionsMethod`` = 14

         .. grid-item-card:: Biogen Rect J19U
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Rectangular hyperbola photosynthesis + J19_UPDATED QF (experimental)

            ``EmissionsMethod`` = 15

         .. grid-item-card:: Biogen Nrect L11
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Non-rectangular hyperbola (Bellucco 2017) + L11 QF (experimental)

            ``EmissionsMethod`` = 21

         .. grid-item-card:: Biogen Nrect J11
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Non-rectangular hyperbola (Bellucco 2017) + J11 QF (experimental)

            ``EmissionsMethod`` = 22

         .. grid-item-card:: Biogen Nrect L11U
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Non-rectangular hyperbola (Bellucco 2017) + L11_UPDATED QF (experimental)

            ``EmissionsMethod`` = 23

         .. grid-item-card:: Biogen Nrect J19
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Non-rectangular hyperbola (Bellucco 2017) + J19 QF (experimental)

            ``EmissionsMethod`` = 24

         .. grid-item-card:: Biogen Nrect J19U
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Non-rectangular hyperbola (Bellucco 2017) + J19_UPDATED QF (experimental)

            ``EmissionsMethod`` = 25

         .. grid-item-card:: Biogen Cond L11
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Conductance-based photosynthesis (Järvi 2019) + L11 QF (experimental)

            ``EmissionsMethod`` = 41

         .. grid-item-card:: Biogen Cond J11
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Conductance-based photosynthesis (Järvi 2019) + J11 QF (experimental)

            ``EmissionsMethod`` = 42

         .. grid-item-card:: Biogen Cond L11U
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Conductance-based photosynthesis (Järvi 2019) + L11_UPDATED QF (experimental)

            ``EmissionsMethod`` = 43

         .. grid-item-card:: Biogen Cond J19
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Conductance-based photosynthesis (Järvi 2019) + J19 QF (experimental)

            ``EmissionsMethod`` = 44

         .. grid-item-card:: Biogen Cond J19U
            :link: model_card_biogen_co2
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

            Conductance-based photosynthesis (Järvi 2019) + J19_UPDATED QF (experimental)

            ``EmissionsMethod`` = 45

   .. tab-item:: Stomatal Conductance

      **2 schemes** -- select a ``GSModel`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Jarvi
            :link: model_card_gs_model
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Original parameterisation (Järvi et al. 2011) based on environmental controls

            ``GSModel`` = 1

         .. grid-item-card:: Ward
            :link: model_card_gs_model
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Updated parameterisation (Ward et al. 2016) with improved temperature and VPD responses

            ``GSModel`` = 2

   .. tab-item:: Frontal Area Index

      **2 schemes** -- select a ``FAIMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Use Provided
            :link: model_card_fai
            :link-type: ref

            :bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Use FAI values provided in site parameters (FAIBldg, FAIEveTree, FAIDecTree)

            ``FAIMethod`` = 0

         .. grid-item-card:: Simple Scheme
            :link: model_card_fai
            :link-type: ref

            :bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Calculate FAI using simple scheme based on surface fractions and heights (see issue #192)

            ``FAIMethod`` = 1

   .. tab-item:: Roughness Length

      **5 schemes** -- select a ``MomentumRoughnessMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Fixed
            :link: model_card_roughness
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Fixed roughness length from site parameters

            ``MomentumRoughnessMethod`` = 1

         .. grid-item-card:: Variable
            :link: model_card_roughness
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Variable based on vegetation LAI using rule of thumb (Grimmond & Oke 1999)

            ``MomentumRoughnessMethod`` = 2

         .. grid-item-card:: Macdonald
            :link: model_card_roughness
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            MacDonald et al. (1998) morphometric method based on building geometry

            ``MomentumRoughnessMethod`` = 3

         .. grid-item-card:: Lambdap Dependent
            :link: model_card_roughness
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Varies with plan area fraction λp (Grimmond & Oke 1999)

            ``MomentumRoughnessMethod`` = 4

         .. grid-item-card:: Alternative
            :link: model_card_roughness
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Alternative variable method

            ``MomentumRoughnessMethod`` = 5

   .. tab-item:: Roughness Sublayer

      **3 schemes** -- select a ``RSLMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Most
            :link: model_card_rsl
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Appropriate for relatively homogeneous, flat surfaces

            ``RSLMethod`` = 0

         .. grid-item-card:: Rst
            :link: model_card_rsl
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Appropriate for heterogeneous urban surfaces with tall roughness elements

            ``RSLMethod`` = 1

         .. grid-item-card:: Variable
            :link: model_card_rsl
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Automatically selects between MOST and RST based on surface morphology (plan area index, frontal area index, and roughness element heights)

            ``RSLMethod`` = 2

   .. tab-item:: Stability Corrections

      **3 schemes** -- select a ``StabilityMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Hoegstrom
            :link: model_card_stability
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Dyer (1974)/Högström (1988) for momentum, Van Ulden & Holtslag (1985) for stable conditions (not recommended)

            ``StabilityMethod`` = 2

         .. grid-item-card:: Campbell Norman
            :link: model_card_stability
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Campbell & Norman (1998) formulations for both momentum and heat

            ``StabilityMethod`` = 3

         .. grid-item-card:: Businger Hoegstrom
            :link: model_card_stability
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Businger et al. (1971)/Högström (1988) formulations (not recommended)

            ``StabilityMethod`` = 4

   .. tab-item:: Soil Moisture

      **3 schemes** -- select a ``SMDMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Modelled
            :link: model_card_smd
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            SMD calculated from water balance using soil parameters

            ``SMDMethod`` = 0

         .. grid-item-card:: Observed Volumetric
            :link: model_card_smd
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Uses observed volumetric soil moisture content (m³/m³) from forcing file

            ``SMDMethod`` = 1

         .. grid-item-card:: Observed Gravimetric
            :link: model_card_smd
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Uses observed gravimetric soil moisture content (kg/kg) from forcing file

            ``SMDMethod`` = 2

   .. tab-item:: Snow

      **1 scheme** -- select a ``SnowUse`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Snow Processes
            :link: model_card_snow
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Controls snow accumulation, melt, and albedo effects on the surface energy balance.

            ``SnowUse`` = 0, 1

   .. tab-item:: Water Use

      **1 scheme** -- select a ``WaterUseMethod`` option to see its model card

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: External Water Use Method
            :link: model_card_water_use
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Controls whether external water use (irrigation) is modelled internally or read from observations.

            ``WaterUseMethod`` = 0, 1

   .. tab-item:: Other

      **Diagnostic / internal modules** not controlled by a method selector.

      .. grid:: 1 1 2 2
         :gutter: 3

         .. grid-item-card:: Building Envelope Energy Radiation Scheme
            :link: model_card_beers
            :link-type: ref

            :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

            Calculates detailed point-specific radiation components and mean radiant temperature for urban thermal comfort assessment.


         .. grid-item-card:: LUMPS Turbulent Flux Scheme
            :link: model_card_lumps
            :link-type: ref

            :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

            Estimates sensible and latent heat fluxes using a simple de Bruin and Holtslag approach modified for urban areas with vegetation fraction.




**Compare Schemes** -- select a method type, then pick two schemes to compare side by side.

.. raw:: html

   <style>
   .mc-compare { font-family: var(--pst-font-family-base, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif); }
   .mc-sel-row { display: flex; gap: 1rem; margin-bottom: 0.75rem; align-items: center; }
   .mc-sel-group { display: flex; flex-direction: column; gap: 0.25rem; }
   .mc-sel-group label {
     font-weight: 600; font-size: 0.75rem; text-transform: uppercase;
     color: var(--pst-color-text-muted, #666);
   }
   .mc-sel-group select {
     padding: 0.5rem 0.75rem; border-radius: 6px;
     border: 1px solid var(--pst-color-border, #555); font-size: 0.95rem;
     background: var(--pst-color-background, #fff); color: var(--pst-color-text-base, #222);
     cursor: pointer;
   }
   .mc-scheme-row { display: flex; gap: 1rem; }
   .mc-scheme-row .mc-sel-group { flex: 1; }
   .mc-scheme-row select { width: 100%; }
   .mc-badge {
     display: inline-block; padding: 0.15rem 0.5rem; border-radius: 10px;
     font-size: 0.75rem; font-weight: 600; margin: 0.15rem;
   }
   .mc-badge-stable { background: #1a7f37; color: #fff; }
   .mc-badge-core { background: #1a7f37; color: #fff; }
   .mc-badge-experimental { background: #d4a017; color: #000; }
   .mc-badge-deprecated { background: #cf222e; color: #fff; }
   .mc-badge-peer-reviewed { background: transparent; border: 1px solid #1a7f37; color: #1a7f37; }
   .mc-badge-internal { background: transparent; border: 1px solid #d4a017; color: #d4a017; }
   .mc-badge-untested { background: transparent; border: 1px solid #cf222e; color: #cf222e; }
   .mc-badge-preprint { background: transparent; border: 1px solid #0969da; color: #0969da; }
   .mc-badge-low { background: transparent; border: 1px solid #1a7f37; color: #1a7f37; }
   .mc-badge-medium { background: transparent; border: 1px solid #0969da; color: #0969da; }
   .mc-badge-high { background: transparent; border: 1px solid #d4a017; color: #d4a017; }
   .mc-table {
     width: 100%; border-collapse: collapse; margin-top: 1rem;
     border: 1px solid var(--pst-color-border, #ddd); border-radius: 8px;
     overflow: hidden;
   }
   .mc-table th, .mc-table td {
     padding: 0.6rem 0.75rem; font-size: 0.85rem;
     border-bottom: 1px solid var(--pst-color-border, #eee);
     text-align: left; vertical-align: top;
   }
   .mc-table th.mc-col-label {
     font-weight: 600; font-size: 0.8rem; text-transform: uppercase;
     color: var(--pst-color-text-muted, #666);
     background: var(--pst-color-surface, #fafafa);
     width: 140px; white-space: nowrap;
   }
   .mc-table th.mc-col-name {
     text-align: center; font-size: 1rem;
     background: var(--pst-color-surface, #f8f8f8);
     border-bottom: 2px solid var(--pst-color-border, #ccc);
   }
   .mc-table td { word-break: break-word; }
   .mc-table td ul { margin: 0; padding-left: 1.2rem; }
   .mc-table td li { margin: 0.15rem 0; }
   .mc-table tr:last-child td, .mc-table tr:last-child th { border-bottom: none; }
   .mc-table th.mc-col-name a { color: inherit; text-decoration: none; border-bottom: 1px dashed currentColor; }
   .mc-table th.mc-col-name a:hover { border-bottom-style: solid; }
   .mc-cross { color: var(--pst-color-text-muted, #999); }
   .mc-empty { text-align: center; padding: 3rem; color: var(--pst-color-text-muted, #999); }
   </style>

   <div class="mc-compare">
     <div class="mc-sel-row">
       <div class="mc-sel-group">
         <label for="mc-cat">Category</label>
         <select id="mc-cat"></select>
       </div>
     </div>
     <div class="mc-scheme-row">
       <div class="mc-sel-group">
         <label for="mc-sel-0">Scheme A</label>
         <select id="mc-sel-0"><option value="">--</option></select>
       </div>
       <div class="mc-sel-group">
         <label for="mc-sel-1">Scheme B</label>
         <select id="mc-sel-1"><option value="">--</option></select>
       </div>
     </div>
     <div id="mc-result" class="mc-empty">Select a category and schemes to compare.</div>
   </div>

   <script>
   (function() {
     const DATA = {
     "anohm": {
       "scheme_name": "anohm",
       "full_name": "Analytical Objective Hysteresis Model",
       "category": "storage_heat",
       "purpose": "Calculates storage heat flux using an analytical extension of OHM with coefficients derived from surface thermal properties.",
       "status": "deprecated",
       "evaluation": "internal",
       "compute": "low",
       "data_prep": "medium",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Material thermal properties per surface type"
       ],
       "forcing": [
         "Net all-wave radiation (Q*)"
       ],
       "outputs": [
         "Storage heat flux (delta QS)"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "S17"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "no",
       "recommended_for": [],
       "not_recommended_for": [
         "Any new applications"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         3
       ]
     },
     "biogen_co2": {
       "scheme_name": "biogen_co2",
       "full_name": "Biogenic CO2 Exchange",
       "category": "co2_vegetation",
       "purpose": "Calculates the biogenic component of the CO2 flux from vegetation photosynthesis and soil/vegetation respiration, combined with an anthropogenic QF method.",
       "status": "experimental",
       "evaluation": "internal",
       "compute": "low",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Photosynthesis coefficients (alpha, beta per vegetation type)",
         "Curvature parameter theta (non-rectangular hyperbola methods only)",
         "Enhanced alpha/beta coefficients (general Bellucco model only)",
         "Respiration coefficients (resp_a, resp_b per vegetation type)",
         "Minimum respiration floor (min_res_bioCO2 per vegetation type)",
         "LAI minimum and maximum per vegetation type",
         "Surface fractions (7 surface types)",
         "All parameters required by the paired QF method"
       ],
       "forcing": [
         "Incoming shortwave radiation (Kdown)",
         "Air temperature (or modelled 2 m temperature)",
         "LAI (from phenology model or prescribed)",
         "Snow fraction per surface type",
         "Stomatal conductance g-function (conductance-based methods only)"
       ],
       "outputs": [
         "Biogenic CO2 flux Fc_biogen [umol m-2 s-1]",
         "Photosynthetic uptake Fc_photo [umol m-2 s-1]",
         "Ecosystem respiration Fc_respi [umol m-2 s-1]",
         "Anthropogenic CO2 flux (from paired QF method)"
       ],
       "dependencies": [
         "Phenology model (LAI calculation)",
         "Snow model (snow fraction per surface)",
         "Stomatal conductance model (g-function, for methods 41-45)",
         "Anthropogenic heat scheme (paired QF method)"
       ],
       "conflicts": [],
       "publications": [
         "J19",
         "B17"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Research studies investigating urban CO2 budgets where both anthropogenic and biogenic components are needed",
         "Sensitivity analyses exploring photosynthesis model choice on urban net CO2 flux"
       ],
       "not_recommended_for": [
         "Production simulations until further multi-site validation is completed",
         "Applications requiring detailed ecosystem carbon accounting (use dedicated vegetation models)",
         "Sites where vegetation is a negligible fraction of the surface"
       ],
       "enum_class": "EmissionsMethod",
       "enum_values": [
         11,
         12,
         13,
         14,
         15,
         21,
         22,
         23,
         24,
         25,
         41,
         42,
         43,
         44,
         45
       ]
     },
     "dyohm": {
       "scheme_name": "dyohm",
       "full_name": "Dynamic Objective Hysteresis Model",
       "category": "storage_heat",
       "purpose": "Extends OHM by dynamically calculating the hysteresis coefficients (a1, a2, a3) from material thermal properties and meteorological conditions, removing the need for empirically pre-calibrated values.",
       "status": "experimental",
       "evaluation": "preprint",
       "compute": "low",
       "data_prep": "medium",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Wall layer thickness (d) [m]",
         "Volumetric heat capacity (C) [J K-1 m-3]",
         "Thermal conductivity (k) [W m-1 K-1]",
         "Complete surface area to plan area ratio (lambda_c)",
         "Surface fractions (7 surface types)"
       ],
       "forcing": [
         "Net all-wave radiation (Q*)",
         "Wind speed",
         "Air temperature (for day-to-day change calculation)"
       ],
       "outputs": [
         "Storage heat flux (delta QS)",
         "Dynamically calculated OHM coefficients (a1, a2, a3) per surface type",
         "Diagnosed surface temperature profiles"
       ],
       "dependencies": [
         "Net radiation scheme (NARP or observed Q*)",
         "OHM framework for flux calculation after coefficient determination"
       ],
       "conflicts": [
         "Cannot be combined with ESTM (StorageHeatMethod=4)"
       ],
       "publications": [
         "Liu25"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "yes",
       "recommended_for": [
         "Sites where OHM coefficients are unavailable but material thermal properties are known",
         "Sensitivity studies exploring the influence of building materials on storage heat flux",
         "Urban applications where dynamic adaptation to changing meteorological conditions is desired"
       ],
       "not_recommended_for": [
         "Production runs until peer-reviewed validation is complete",
         "Sites where well-calibrated OHM coefficients already exist and computational simplicity is preferred",
         "Applications requiring detailed multi-layer temperature profiles (use EHC or STEBBS)"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         6
       ]
     },
     "ehc": {
       "scheme_name": "ehc",
       "full_name": "Explicit Heat Conduction",
       "category": "storage_heat",
       "purpose": "Calculates storage heat flux using explicit 1D heat conduction through urban facets (roof, wall, ground) with separate surface temperature outputs for each element.",
       "status": "stable",
       "evaluation": "internal",
       "compute": "medium",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Thermal conductivity for each layer of roof, wall, and ground facets",
         "Volumetric heat capacity for each layer of roof, wall, and ground facets",
         "Layer thicknesses for roof, wall, and ground facets",
         "Surface fractions for roof, wall, and standard surfaces",
         "Number of vertical levels in the urban canopy (nlayer)"
       ],
       "forcing": [
         "Surface temperature (from energy balance iteration)",
         "Indoor or deep-soil temperature (lower boundary)"
       ],
       "outputs": [
         "Storage heat flux (delta QS) per facet and grid-averaged",
         "Surface temperatures for roof, wall, and ground facets",
         "Internal temperature profiles through each facet"
       ],
       "dependencies": [
         "Net radiation scheme (NARP, SPARTACUS-Surface, or observed Q*)",
         "Energy balance iteration for surface temperature coupling"
       ],
       "conflicts": [
         "Cannot be combined with ESTM (StorageHeatMethod=4)",
         "Cannot be combined with STEBBS (StorageHeatMethod=7)"
       ],
       "publications": [
         "O05"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Studies requiring physically-based surface temperatures for individual urban facets",
         "Applications where material thermal properties are well characterised",
         "Coupling with radiation schemes that use facet-level surface temperatures"
       ],
       "not_recommended_for": [
         "Sites where layer-resolved thermal properties are unavailable (use OHM instead)",
         "Applications requiring only bulk storage heat flux without surface temperatures"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         5
       ]
     },
     "estm": {
       "scheme_name": "estm",
       "full_name": "Element Surface Temperature Method",
       "category": "storage_heat",
       "purpose": "Calculates storage heat flux and surface temperatures using 1D heat conduction through urban surface elements.",
       "status": "deprecated",
       "evaluation": "peer-reviewed",
       "compute": "medium",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Thermal properties per layer (conductivity, heat capacity, thickness)"
       ],
       "forcing": [
         "Surface temperature boundary conditions"
       ],
       "outputs": [
         "Storage heat flux (delta QS)",
         "Element surface temperatures"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "O05"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "no",
       "recommended_for": [],
       "not_recommended_for": [
         "Any new applications"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         4
       ]
     },
     "narp": {
       "scheme_name": "narp",
       "full_name": "Net All-wave Radiation Parameterisation",
       "category": "radiation",
       "purpose": "Calculates net all-wave radiation (Q*) from incoming shortwave radiation, air temperature, humidity, and surface characteristics using empirical longwave parameterisations.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Surface albedo (per surface type)",
         "Surface emissivity (per surface type)",
         "Surface fractions (7 surface types)"
       ],
       "forcing": [
         "Incoming shortwave radiation (Kdown)",
         "Air temperature",
         "Relative humidity",
         "Atmospheric pressure",
         "Cloud cover fraction (optional, depending on method)",
         "Incoming longwave radiation (optional, if observed)"
       ],
       "outputs": [
         "Net all-wave radiation (Q*)",
         "Outgoing shortwave radiation (Kup)",
         "Incoming longwave radiation (Ldown, if modelled)",
         "Outgoing longwave radiation (Lup)"
       ],
       "dependencies": [
         "Sun position calculation (internal)"
       ],
       "conflicts": [
         "Cannot be used simultaneously with SPARTACUS-Surface (methods 1001-1003 use SPARTACUS instead)"
       ],
       "publications": [
         "O03",
         "L11"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Standard urban energy balance simulations at neighbourhood scale",
         "Long-duration runs where computational efficiency is important",
         "Applications where observed longwave down radiation is available"
       ],
       "not_recommended_for": [
         "Studies requiring detailed 3D radiation interactions with building geometry",
         "Mean radiant temperature or pedestrian thermal comfort calculations"
       ],
       "enum_class": "NetRadiationMethod",
       "enum_values": [
         1,
         2,
         3
       ]
     },
     "ohm": {
       "scheme_name": "ohm",
       "full_name": "Objective Hysteresis Model",
       "category": "storage_heat",
       "purpose": "Calculates storage heat flux (delta QS) using empirically-fitted hysteresis relations between net all-wave radiation and its rate of change.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "medium",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "OHM coefficients (a1, a2, a3) for each surface type",
         "Surface fractions (7 surface types)",
         "OHM threshold parameters (SW and wetness)",
         "Soil store capacity (for wet/dry coefficient selection)"
       ],
       "forcing": [
         "Net all-wave radiation (Q*)",
         "Anthropogenic heat flux (QF, if OhmIncQf=1)"
       ],
       "outputs": [
         "Storage heat flux (delta QS)",
         "Area-averaged OHM coefficients"
       ],
       "dependencies": [
         "Net radiation scheme (NARP or observed Q*)",
         "OhmIncQf setting controls whether Q*+QF or Q* alone is used"
       ],
       "conflicts": [
         "StorageHeatMethod=1 requires OhmIncQf=0 (Q* only)"
       ],
       "publications": [
         "G91",
         "GO99",
         "GO02"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Standard urban energy balance runs with well-characterised surfaces",
         "Long-duration simulations where computational efficiency matters",
         "Applications where published OHM coefficients exist for the site type"
       ],
       "not_recommended_for": [
         "Sites with novel surface types lacking OHM coefficient calibration",
         "Studies requiring facet-level surface temperatures (use STEBBS or EHC)"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         1
       ]
     },
     "qf": {
       "scheme_name": "qf",
       "full_name": "Anthropogenic Heat Flux",
       "category": "emissions",
       "purpose": "Calculates anthropogenic heat flux (QF) and associated CO2 emissions from human metabolism, building energy use, and road traffic using temperature-dependent parameterisations and diurnal activity profiles.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "medium",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Population density (daytime and nighttime) [cap ha-1]",
         "QF empirical coefficients (Qf_A, Qf_B, Qf_C for J11)",
         "Base temperatures for heating and cooling thresholds",
         "AH_MIN and AH_SLOPE coefficients (for L11)",
         "Building energy use fraction (QF0_BEU)",
         "Fossil fuel fractions (heating and non-heating)",
         "Traffic rate and emission factors (for J19)",
         "Metabolic heat range (MinQFMetab, MaxQFMetab)",
         "Diurnal profiles (anthropogenic heat, traffic, human activity, population)"
       ],
       "forcing": [
         "Air temperature",
         "Heating and cooling degree days (computed internally from temperature)"
       ],
       "outputs": [
         "Total anthropogenic heat flux QF [W m-2]",
         "Anthropogenic CO2 flux Fc_anthro [umol m-2 s-1]",
         "Component fluxes: metabolism, building, traffic, point source"
       ],
       "dependencies": [
         "Heating/cooling degree day calculation (internal daily state)",
         "Diurnal profile interpolation (internal)"
       ],
       "conflicts": [],
       "publications": [
         "L11",
         "J11",
         "J19",
         "A11",
         "L13"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Standard urban energy balance simulations requiring anthropogenic heat",
         "Studies where QF magnitude and diurnal pattern matter but detailed sectoral breakdown is not essential",
         "Long-duration runs where computational efficiency is important"
       ],
       "not_recommended_for": [
         "Detailed sectoral energy demand studies (use LUCY/LQF or GreaterQF/GQF instead)",
         "Sites with highly heterogeneous population distributions within a single grid cell"
       ],
       "enum_class": "EmissionsMethod",
       "enum_values": [
         0,
         1,
         2,
         3,
         4,
         5
       ]
     },
     "spartacus": {
       "scheme_name": "spartacus",
       "full_name": "SPARTACUS-Surface",
       "category": "radiation",
       "purpose": "Computes 3D shortwave and longwave radiation interactions with complex urban and vegetated canopies using a multi-layer statistical approach.",
       "status": "experimental",
       "evaluation": "internal",
       "compute": "high",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Building fractions, heights, and scales per vertical layer",
         "Vegetation fractions, scales, and LAI",
         "Roof and wall albedo and emissivity",
         "Ground albedo and emissivity (area-weighted from SUEWS surface types)",
         "SPARTACUS-specific namelist parameters (SUEWS_SPARTACUS.nml)",
         "Number of vegetation regions (1 or 2)",
         "Number of shortwave and longwave streams",
         "Vegetation single scattering albedos (SW and LW)"
       ],
       "forcing": [
         "Incoming shortwave radiation (Kdown)",
         "Incoming longwave radiation (Ldown, observed or modelled)",
         "Air temperature",
         "Surface temperature",
         "Solar zenith angle"
       ],
       "outputs": [
         "Net all-wave radiation (Q*)",
         "Outgoing shortwave radiation (Kup)",
         "Outgoing longwave radiation (Lup)",
         "Per-layer net radiation for roof and wall facets",
         "Per-surface net radiation",
         "Multi-layer SPARTACUS diagnostics (SSss_YYYY_SPARTACUS.txt)"
       ],
       "dependencies": [
         "SPARTACUS-Surface library (radsurf_interface)",
         "Building geometry profile data (height, fraction, scale per layer)",
         "SUEWS_SPARTACUS.nml configuration file",
         "LAI from SUEWS vegetation parameters"
       ],
       "conflicts": [
         "Cannot be used simultaneously with NARP (methods 1-3)"
       ],
       "publications": [
         "Hogan2018Jan",
         "Hogan2019Mar",
         "Hogan2019Oct"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "yes",
       "recommended_for": [
         "Research applications requiring 3D radiation transfer in urban canopies",
         "Studies investigating the effect of building geometry on radiation budgets",
         "Configurations where vertical heterogeneity of buildings and trees matters"
       ],
       "not_recommended_for": [
         "Production runs until peer-reviewed validation is complete",
         "Sites lacking detailed building geometry and vegetation profile data",
         "Long-duration simulations where computational cost is a constraint"
       ],
       "enum_class": "NetRadiationMethod",
       "enum_values": [
         1001,
         1002,
         1003
       ]
     },
     "stebbs": {
       "scheme_name": "stebbs",
       "full_name": "Surface Temperature Energy Balance Based Scheme",
       "category": "storage_heat",
       "purpose": "Calculates facet-level surface temperatures and storage heat flux for buildings by solving explicit energy balances for roof, wall, and ground surfaces with multi-layer heat conduction.",
       "status": "experimental",
       "evaluation": "internal",
       "compute": "medium",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Building thermal properties (conductivity, heat capacity per layer)",
         "Layer thicknesses for roof, wall, and ground",
         "Building geometry (heights, plan area fractions)",
         "Surface albedo and emissivity per facet",
         "RC method and weighting factor for heat capacity splitting",
         "Internal temperature settings"
       ],
       "forcing": [
         "Incoming shortwave radiation",
         "Air temperature",
         "Wind speed",
         "Net all-wave radiation (from NARP or observed)"
       ],
       "outputs": [
         "Storage heat flux (delta QS) for buildings",
         "Facet surface temperatures (roof, wall, ground)",
         "Internal building temperature",
         "Detailed STEBBS output columns"
       ],
       "dependencies": [
         "Net radiation scheme (NARP or observed) for non-building surfaces",
         "OHM for non-building surface types"
       ],
       "conflicts": [
         "Cannot be combined with ESTM (StorageHeatMethod=4)"
       ],
       "publications": [],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "yes",
       "recommended_for": [
         "Research applications investigating facet-level urban surface temperatures",
         "Building energy studies requiring internal temperature modelling"
       ],
       "not_recommended_for": [
         "Production runs until peer-reviewed validation is complete",
         "Sites where building thermal property data is unavailable",
         "Long-duration runs where computational cost is a constraint"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         7
       ]
     }
   };

     const GROUP_NAMES = {"StorageHeatMethod": "Storage Heat Flux", "NetRadiationMethod": "Net Radiation", "EmissionsMethod": "Emissions"} || {
       radiation: "Radiation (Q*)",
       storage_heat: "Storage Heat Flux (QS)",
       turbulent_fluxes: "Turbulent Fluxes (QH/QE)",
       emissions: "Anthropogenic Heat (QF)",
       boundary_layer: "Boundary Layer",
       water_balance: "Water Balance / Snow",
       building_energy: "Building Energy",
       co2_vegetation: "CO2 Exchange and Vegetation"
     };

     // Group schemes by enum_class (or category as fallback)
     const byCategory = {};
     Object.entries(DATA).forEach(([name, d]) => {
       const grp = d.enum_class || d.category;
       if (!grp) return;
       if (!byCategory[grp]) byCategory[grp] = [];
       byCategory[grp].push(name);
     });
     // Only keep groups with 2+ schemes (meaningful comparisons)
     Object.keys(byCategory).forEach(k => {
       if (byCategory[k].length < 2) delete byCategory[k];
     });
     // Sort schemes within each group
     Object.values(byCategory).forEach(arr => arr.sort());

     const catSel = document.getElementById("mc-cat");
     const sels = [document.getElementById("mc-sel-0"), document.getElementById("mc-sel-1")];

     // Populate category selector
     Object.keys(byCategory).sort().forEach(cat => {
       const opt = document.createElement("option");
       opt.value = cat;
       opt.textContent = GROUP_NAMES[cat] || cat;
       catSel.appendChild(opt);
     });

     function populateSchemeSelectors() {
       const cat = catSel.value;
       const schemes = byCategory[cat] || [];
       sels.forEach((sel, i) => {
         sel.innerHTML = '<option value="">--</option>';
         schemes.forEach(n => {
           const opt = document.createElement("option");
           opt.value = n;
           opt.textContent = DATA[n].full_name;
           sel.appendChild(opt);
         });
         // Auto-select if available
         if (schemes[i]) sel.value = schemes[i];
       });
       render();
     }

     catSel.addEventListener("change", populateSchemeSelectors);
     sels.forEach(sel => sel.addEventListener("change", render));

     function badge(text, cls) {
       return '<span class="mc-badge mc-badge-' + cls + '">' + text + '</span>';
     }
     function statusBadge(s) { return badge(s, s); }
     function evalBadge(s) { return badge(s, s); }
     function levelBadge(label, s) { return badge(label + ": " + s, s); }

     function listHtml(arr) {
       if (!arr || arr.length === 0) return '<span class="mc-cross">--</span>';
       return "<ul>" + arr.map(x => "<li>" + x + "</li>").join("") + "</ul>";
     }

     function render() {
       const selected = [];
       sels.forEach(sel => {
         const v = sel.value;
         if (v && DATA[v]) selected.push(DATA[v]);
       });
       const el = document.getElementById("mc-result");
       if (selected.length === 0) {
         el.className = "mc-empty";
         el.innerHTML = "Select a category and schemes to compare.";
         return;
       }
       el.className = "";

       const rows = [
         ["Status", d => statusBadge(d.status)],
         ["Evaluation", d => evalBadge(d.evaluation)],
         ["Compute", d => levelBadge("compute", d.compute)],
         ["Data prep", d => levelBadge("data prep", d.data_prep)],
         ["Purpose", d => d.purpose],
         ["Config", d => d.enum_class ? "<code>" + d.enum_class + "</code> = " + (d.enum_values || []).join(", ") : "--"],
         ["Spatial scale", d => d.spatial_scale.join(", ")],
         ["Resolution", d => d.temporal_resolution.join(", ")],
         ["Parameters", d => listHtml(d.parameters)],
         ["Forcing", d => listHtml(d.forcing)],
         ["Outputs", d => listHtml(d.outputs)],
         ["Dependencies", d => listHtml(d.dependencies)],
         ["Conflicts", d => listHtml(d.conflicts)],
         ["Recommended for", d => listHtml(d.recommended_for)],
         ["Not recommended", d => listHtml(d.not_recommended_for)],
         ["Publications", d => listHtml(d.publications)],
         ["Maintainers", d => d.maintainers.join(", ") || "--"],
         ["Active dev", d => d.active_development || "--"],
       ];

       let t = '<table class="mc-table">';
       // Column header: scheme names
       t += "<thead><tr><th></th>";
       selected.forEach(d => {
         const href = d.scheme_name + ".html";
         t += '<th class="mc-col-name"><a href="' + href + '">' + d.full_name + '</a></th>';
       });
       t += "</tr></thead><tbody>";

       rows.forEach(([label, fn]) => {
         t += "<tr>";
         t += '<th class="mc-col-label">' + label + '</th>';
         selected.forEach(d => {
           t += "<td>" + fn(d) + "</td>";
         });
         t += "</tr>";
       });
       t += "</tbody></table>";

       el.innerHTML = t;
     }

     // Initial load: select first category and render
     populateSchemeSelectors();
   })();
   </script>


.. toctree::
   :hidden:

   radiation
   storage_heat
   turbulent_fluxes
   emissions
   boundary_layer
   water_balance
   co2_vegetation
