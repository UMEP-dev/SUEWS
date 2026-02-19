.. _model_card_ohm:

Objective Hysteresis Model
==========================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`low demand`

Calculates storage heat flux (delta QS) using empirically-fitted hysteresis relations between net all-wave radiation and its rate of change.

**StorageHeatMethod** options:

- ``1`` **OHM_WITHOUT_QF** -- Objective Hysteresis Model using Q* only (use with OhmIncQf=0)

.. tab-set::

   .. tab-item:: Science

      OHM models the storage heat flux as a linear combination of net all-wave radiation (Q*) and its temporal rate of change (dQ*/dt), with empirically determined coefficients (a1, a2, a3) that capture the hysteresis between Q* and delta QS. The area-averaged storage heat flux is computed as a weighted sum over surface types, each with their own OHM coefficients. The formulation is: delta QS = a1*Q* + a2*(dQ*/dt) + a3.

      **Key assumptions**

      - Storage heat flux is primarily a function of Q* and dQ*/dt
      - OHM coefficients are constant for each surface type (seasonally or by condition)
      - Area-averaged flux is a linear combination weighted by surface fractions

      **Comparison to other schemes**

      OHM is the original and simplest storage heat flux scheme in SUEWS. EHC provides explicit multi-layer heat conduction, DyOHM calculates OHM coefficients dynamically from material properties, and STEBBS solves facet-level energy balances. OHM remains suitable when empirical coefficients are available for the site.

      **Key publications:** :cite:`G91,GO99,GO02`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Extensively validated since 1991. Performance depends on availability of appropriate OHM coefficients for the surface types present. Well-suited to neighbourhood-scale applications where surface composition is known.

      **Datasets**

      - Multiple urban sites including Vancouver, Tucson, Sacramento, Chicago
      - Urban flux tower observations across different climate zones

      **Intercomparison**

      Compared with residual methods and direct calorimetric estimates. Generally performs well when coefficients are appropriate for the site, but can underperform for novel surface configurations without calibration.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood, city
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`low demand`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - OHM coefficients (a1, a2, a3) for each surface type
         - Surface fractions (7 surface types)
         - OHM threshold parameters (SW and wetness)
         - Soil store capacity (for wet/dry coefficient selection)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Net all-wave radiation (Q*)
         - Anthropogenic heat flux (QF, if OhmIncQf=1)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Storage heat flux (delta QS)
         - Area-averaged OHM coefficients

      **Dependencies:** Net radiation scheme (NARP or observed Q*); OhmIncQf setting controls whether Q*+QF or Q* alone is used

      **Conflicts:** StorageHeatMethod=1 requires OhmIncQf=0 (Q* only)

   .. tab-item:: Guidance

      **Recommended for**

      - Standard urban energy balance runs with well-characterised surfaces
      - Long-duration simulations where computational efficiency matters
      - Applications where published OHM coefficients exist for the site type

      **Not recommended for**

      - Sites with novel surface types lacking OHM coefficient calibration
      - Studies requiring facet-level surface temperatures (use STEBBS or EHC)

      **Configuration notes**

      StorageHeatMethod=1 selects OHM with Q* only (requires OhmIncQf=0). OHM coefficients are specified per surface type, with separate sets for dry/wet and summer/winter conditions selected by threshold parameters.

      .. warning::

         **Common pitfalls**

         - Incorrect OhmIncQf setting for the chosen StorageHeatMethod
         - Using default OHM coefficients for surface types very different from calibration sites

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2025-01
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

