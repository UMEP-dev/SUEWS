.. _model_card_beers:

Building Envelope Energy Radiation Scheme
=========================================

:bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

Calculates detailed point-specific radiation components and mean radiant temperature for urban thermal comfort assessment.

.. tab-set::

   .. tab-item:: Science

      BEERS is the successor to SOLWEIG and provides point-specific radiation analysis within urban environments. It solves the urban radiation balance by computing sun position using astronomical algorithms, analysing 3D urban geometry to determine sky view factors and shadow patterns, and calculating direct, diffuse, and reflected shortwave and longwave radiation components from all cardinal directions. Surface temperatures for ground, wall, and roof facets are computed from energy balance considerations. Mean radiant temperature (Tmrt) is derived by integrating directional radiation fluxes weighted by human body shape factors for a standing person (Fside=0.22, Fup=0.06) and absorption coefficients for shortwave (absK=0.7) and longwave (absL=0.97) radiation.

      **Key assumptions**

      - Urban geometry is characterised by plan area fraction and frontal area fraction
      - Sky view factors are derived from height-to-width ratio
      - Human body is represented as a standing person with fixed shape factors
      - Direct and diffuse shortwave components are calculated from global radiation (clearness index)
      - Shadow patterns on roofs are treated as constant (not geometry-dependent)

      **Comparison to other schemes**

      BEERS provides point-specific radiation analysis and mean radiant temperature, which NARP and SPARTACUS-Surface do not calculate. While SPARTACUS-Surface resolves multi-layer 3D radiation transfer for energy balance purposes, BEERS focuses on directional radiation components relevant to pedestrian thermal comfort at specific locations.

      **Key publications:** :cite:`F08,FG11`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-warning-line:`internal`

      BEERS inherits the physical basis from SOLWEIG, which has been extensively validated for mean radiant temperature calculations. The SUEWS-integrated version is under ongoing testing. Users should verify results against observations for their specific applications.

      **Datasets**

      - Internal testing within SUEWS framework

      **Intercomparison**

      SOLWEIG (predecessor) has been validated against field measurements of mean radiant temperature in multiple urban environments. Direct intercomparison of BEERS within the SUEWS framework is pending.

   .. tab-item:: Technical

      :Spatial scale: point, neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-info-line:`compute: medium`
      :Data preparation: :bdg-warning-line:`data prep: high`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Plan area fraction (lamdaP)
         - Frontal area fraction (lamdaF)
         - Ground surface albedo
         - Building surface albedo
         - Ground surface emissivity
         - Wall surface emissivity
         - Location coordinates (latitude, longitude, altitude)
         - Time zone

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Incoming shortwave radiation (Kdown)
         - Incoming longwave radiation (Ldown)
         - Air temperature
         - Relative humidity
         - Atmospheric pressure
         - Surface temperature

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Mean radiant temperature (Tmrt)
         - Shortwave radiation at point of interest (Kdown2d, Kup2d)
         - Longwave radiation at point of interest (Ldown2d, Lup2d)
         - Directional radiation components (north, south, east, west)
         - Shadow patterns on ground and walls
         - Sky view factors (ground, roof, building-vegetation)
         - Surface temperatures (ground Tg, wall Tw)

      **Dependencies:** Sun position calculation (NARP_cal_SunPosition); Building morphology parameters from SUEWS site configuration

      **Conflicts:** None (standalone diagnostic module)

   .. tab-item:: Guidance

      **Recommended for**

      - Human thermal comfort studies in urban environments
      - Point-specific radiation analysis at pedestrian level
      - Urban microclimate analysis for planning and design

      **Not recommended for**

      - Production runs until peer-reviewed validation of SUEWS integration is complete
      - Applications requiring only grid-averaged net radiation (use NARP instead)
      - Sites lacking building morphology data

      **Configuration notes**

      BEERS is a standalone diagnostic module within SUEWS. It requires building morphology parameters (plan area fraction, frontal area fraction) and surface optical properties (albedo and emissivity for ground, building, and wall surfaces). Location coordinates and time zone must be correctly specified for accurate sun position calculations.

      .. warning::

         **Common pitfalls**

         - Incorrect building morphology parameters (lamdaP, lamdaF) leading to unrealistic view factors
         - Missing or incorrect location coordinates affecting sun position calculations
         - Confusion between SUEWS-provided surface temperature and BEERS-internal surface temperature

   .. tab-item:: Status

      :Development status: :bdg-warning:`experimental`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2020-11
      :Active development: yes

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

      .. warning::

         **Known issues**

         - Shadow on roof is constant rather than varying with building morphology and sun altitude
         - Surface temperature (Tsurf) is provided by SUEWS but also calculated internally -- interaction needs clarification
         - Conversion of H/W to SVF uses simplified relations rather than the Oke basin equation

