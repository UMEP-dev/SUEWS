.. _model_card_stability:

Atmospheric Stability Corrections
=================================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Provides stability correction functions (psi and phi) for momentum and heat transfer in the surface layer under non-neutral atmospheric conditions.

**StabilityMethod** options:

- ``2`` **HOEGSTROM** -- Dyer (1974)/Högström (1988) for momentum, Van Ulden & Holtslag (1985) for stable conditions (not recommended)
- ``3`` **CAMPBELL_NORMAN** -- Campbell & Norman (1998) formulations for both momentum and heat
- ``4`` **BUSINGER_HOEGSTROM** -- Businger et al. (1971)/Högström (1988) formulations (not recommended)

.. tab-set::

   .. tab-item:: Science

      Atmospheric stability corrections modify the logarithmic wind and temperature profiles to account for buoyancy effects. The corrections are expressed as integrated stability functions psi(z/L) for profile calculations and dimensionless gradients phi(z/L) for flux-gradient relationships, where L is the Obukhov length. Three formulations are available: method 2 uses Dyer (1974) modified by Hogstrom (1988) for the unstable case with Van Ulden and Holtslag (1985) for stable conditions; method 3 follows Kondo (1975) as adopted by Campbell and Norman (1998); method 4 uses Businger et al. (1971) modified by Hogstrom (1988). A neutral stability limit parameter prevents division by zero when conditions are very close to neutral (|z/L| < 1e-4).

      **Key assumptions**

      - Monin-Obukhov similarity applies within the surface layer
      - The Obukhov length L adequately characterises atmospheric stability
      - Stability functions are universal (site-independent) for a given formulation
      - Near-neutral conditions are handled by a neutral-limit threshold

      **Comparison to other schemes**

      All three methods are well-established semi-empirical formulations from the micrometeorological literature. The Campbell and Norman (method 3) formulation, based on Kondo (1975), provides consistent behaviour across the full stability range and is the recommended default. Methods 2 and 4 are retained for backward compatibility but are not recommended for general use.

      **Key publications:** :cite:`CN98,H88,D74`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      The stability functions are well-tested empirical formulations validated against decades of surface-layer observations. Method 3 (Campbell and Norman) provides robust behaviour across a wide range of stability conditions and is recommended as the default.

      **Datasets**

      - Kansas and Minnesota experiments (Businger et al. 1971, Hogstrom 1988)
      - Multiple surface-layer observation campaigns

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Measurement height (z)
         - Displacement height (zd)
         - Roughness lengths (z0m, z0v)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Obukhov length (L)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Integrated stability function for momentum (psi_m)
         - Integrated stability function for heat (psi_h)
         - Dimensionless gradient for momentum (phi_m)
         - Dimensionless gradient for heat (phi_h)

      **Dependencies:** Obukhov length calculation (from sensible heat flux and friction velocity); Roughness length parameterisation

   .. tab-item:: Guidance

      **Recommended for**

      - All SUEWS simulations requiring turbulent flux calculations
      - Surface-layer profile diagnostics and aerodynamic resistance estimation

      **Not recommended for**

      - Methods 2 and 4 are not recommended for general use; prefer method 3

      **Configuration notes**

      StabilityMethod=3 (Campbell and Norman 1998, based on Kondo 1975) is the recommended default and provides robust behaviour across all stability regimes. Method 2 (Dyer/Hogstrom with Van Ulden and Holtslag stable correction) and method 4 (Businger/Hogstrom) are available for backward compatibility or specialist comparisons but are not recommended for production runs. Values 0 and 1 are reserved (NOT_USED) and should not be selected.

      .. warning::

         **Common pitfalls**

         - Using method 2 or 4 without understanding their limitations in strongly stable/unstable conditions
         - Stability corrections become unreliable when z/L is extremely large; the model applies internal limits
         - The choice of stability method affects aerodynamic resistance and thus all turbulent fluxes

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2017-08
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

