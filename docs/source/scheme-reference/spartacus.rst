.. _model_card_spartacus:

SPARTACUS-Surface
=================

:bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-warning-line:`compute: high` :bdg-warning-line:`data prep: high`

Computes 3D shortwave and longwave radiation interactions with complex urban and vegetated canopies using a multi-layer statistical approach.

**NetRadiationMethod** options:

- ``1001`` **LDOWN_SS_OBSERVED** -- SPARTACUS-Surface integration with observed L↓ (experimental)
- ``1002`` **LDOWN_SS_CLOUD** -- SPARTACUS-Surface integration with L↓ from cloud fraction (experimental)
- ``1003`` **LDOWN_SS_AIR** -- SPARTACUS-Surface integration with L↓ from air temperature/humidity (experimental)

.. tab-set::

   .. tab-item:: Science

      SPARTACUS-Surface (SS) uses a multi-layer description of the urban canopy with a statistical representation of horizontal building and vegetation distributions. Wall-to-wall separation distances follow an exponential probability distribution, from which probabilities of light interception by trees, walls, and the ground are determined. Trees are assumed randomly distributed with one or two vegetation regions per layer. Each interception event can produce diffuse or specular reflection, absorption, or diffuse transmission. Building properties are determined by albedos and emissivities; vegetation properties by extinction coefficients and single scattering albedos. The scheme supports up to 15 vertical layers with user-defined heights and vertically heterogeneous building and vegetation fractions.

      **Key assumptions**

      - Trees are randomly distributed within the canopy
      - Wall-to-wall separation distances follow an exponential probability distribution
      - Vegetation extinction coefficients are uniform across all vegetated layers
      - Building facet and ground temperatures equal SUEWS surface temperature (TSfc_C)
      - Leaf temperatures equal SUEWS air temperature (temp_C)
      - Buildings do not touch as building fraction approaches 1 (crown shyness analogy)

      **Comparison to other schemes**

      SPARTACUS-Surface provides physically-based 3D radiation transfer compared to NARP's bulk parameterisation. It resolves vertical heterogeneity in building and vegetation geometry, whereas NARP treats the surface as a single layer with area-weighted properties. The additional fidelity comes at substantially higher computational cost and data requirements.

      **Key publications:** :cite:`Hogan2018Jan,Hogan2019Mar,Hogan2019Oct`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-warning-line:`internal`

      The standalone SPARTACUS-Surface library has been evaluated in vegetation and urban canopy contexts. The SUEWS coupling is under active testing but peer-reviewed validation of the coupled system has not yet been published.

      **Datasets**

      - Internal testing within SUEWS coupling framework

      **Intercomparison**

      The standalone SPARTACUS-Surface model has been compared against Monte Carlo radiation simulations for idealised urban geometries. Coupled SUEWS-SS intercomparisons are pending.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-warning-line:`compute: high`
      :Data preparation: :bdg-warning-line:`data prep: high`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Building fractions, heights, and scales per vertical layer
         - Vegetation fractions, scales, and LAI
         - Roof and wall albedo and emissivity
         - Ground albedo and emissivity (area-weighted from SUEWS surface types)
         - SPARTACUS-specific namelist parameters (SUEWS_SPARTACUS.nml)
         - Number of vegetation regions (1 or 2)
         - Number of shortwave and longwave streams
         - Vegetation single scattering albedos (SW and LW)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Incoming shortwave radiation (Kdown)
         - Incoming longwave radiation (Ldown, observed or modelled)
         - Air temperature
         - Surface temperature
         - Solar zenith angle

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Net all-wave radiation (Q*)
         - Outgoing shortwave radiation (Kup)
         - Outgoing longwave radiation (Lup)
         - Per-layer net radiation for roof and wall facets
         - Per-surface net radiation
         - Multi-layer SPARTACUS diagnostics (SSss_YYYY_SPARTACUS.txt)

      **Dependencies:** SPARTACUS-Surface library (radsurf_interface); Building geometry profile data (height, fraction, scale per layer); SUEWS_SPARTACUS.nml configuration file; LAI from SUEWS vegetation parameters

      **Conflicts:** Cannot be used simultaneously with NARP (methods 1-3)

   .. tab-item:: Guidance

      **Recommended for**

      - Research applications requiring 3D radiation transfer in urban canopies
      - Studies investigating the effect of building geometry on radiation budgets
      - Configurations where vertical heterogeneity of buildings and trees matters

      **Not recommended for**

      - Production runs until peer-reviewed validation is complete
      - Sites lacking detailed building geometry and vegetation profile data
      - Long-duration simulations where computational cost is a constraint

      **Configuration notes**

      NetRadiationMethod values 1001-1003 select SPARTACUS-Surface with different longwave down options: 1001 = observed Ldown, 1002 = modelled from cloud fraction, 1003 = modelled from air temperature and humidity. Requires SUEWS_SPARTACUS.nml in the input directory with settings for stream counts, vegetation regions, and radiative properties. Building and vegetation fractions in SUEWS_SPARTACUS.nml must be consistent with SUEWS_SiteSelect surface fractions.

      .. warning::

         **Common pitfalls**

         - Inconsistent building/vegetation fractions between SUEWS_SiteSelect and SUEWS_SPARTACUS.nml
         - Missing or incorrectly formatted SUEWS_SPARTACUS.nml file
         - Using default vegetation SSA values when site-specific values are available

   .. tab-item:: Status

      :Development status: :bdg-warning:`experimental`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2021-12
      :Active development: yes

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

      .. warning::

         **Known issues**

         - Peer-reviewed validation of SUEWS-SS coupling not yet published
         - Snow effects are not yet included
         - Building facet temperatures use bulk SUEWS TSfc_C rather than facet-resolved values
         - Leaf temperature uses forcing air temperature, not height-varying RSL temperature

