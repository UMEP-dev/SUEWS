.. _model_card_roughness:

Roughness Length Parameterisations
==================================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Calculates aerodynamic roughness length for momentum (z0m), zero-plane displacement height (zd) and roughness length for heat (z0v) using morphometric or empirical methods.

**MomentumRoughnessMethod** options:

- ``1`` **FIXED** -- Fixed roughness length from site parameters
- ``2`` **VARIABLE** -- Variable based on vegetation LAI using rule of thumb (Grimmond & Oke 1999)
- ``3`` **MACDONALD** -- MacDonald et al. (1998) morphometric method based on building geometry
- ``4`` **LAMBDAP_DEPENDENT** -- Varies with plan area fraction λp (Grimmond & Oke 1999)
- ``5`` **ALTERNATIVE** -- Alternative variable method

.. tab-set::

   .. tab-item:: Science

      Momentum roughness (z0m) and displacement height (zd) are determined from surface morphology. The simplest approach uses fixed values from site parameters (method 1). Rule-of-thumb methods scale z0m and zd as fixed fractions of mean element height Zh (method 2: z0m=0.1Zh, zd=0.7Zh). The MacDonald et al. (1998) morphometric method (3) uses building plan area fraction and frontal area index to derive z0m and zd analytically. A plan-area-fraction-dependent approach (4) uses digitised relationships from Grimmond and Oke (1999). For heat transfer, a separate set of methods determines z0v (the heat roughness length) via the HeatRoughnessMethod enum: Brutsaert (1982) simple ratio (z0v=z0m/10), Kawai et al. (2009) Reynolds-number dependent formulation, Voogt and Grimmond (2000) exponential scaling, Kanda et al. (2007) approach, and an adaptive method that selects between pervious and impervious formulations based on vegetation coverage.

      **Key assumptions**

      - Roughness elements are horizontally homogeneous at the neighbourhood scale
      - Mean element height Zh is representative of the dominant roughness features
      - Morphometric relationships calibrated for urban-type geometries
      - Heat roughness length differs from momentum roughness length due to bluff-body effects

      **Comparison to other schemes**

      Fixed values (method 1) are simplest but require prior knowledge. The rule-of-thumb (2) and MacDonald (3) methods are widely used in urban climate modelling. MacDonald provides a physically-based approach using frontal area index, while the plan-area-dependent method (4) offers an empirical alternative for varying urban densities.

      **Key publications:** :cite:`GO99,M98,B82`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Morphometric methods reproduce observed roughness parameters well when input morphology data are accurate. The MacDonald method is the most physically grounded for urban surfaces; rule-of-thumb works adequately for preliminary estimates.

      **Datasets**

      - Wind tunnel studies of urban morphology (MacDonald et al. 1998)
      - North American urban flux sites (Grimmond and Oke 1999)

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Mean building/element height (Zh)
         - Building plan area fraction
         - Frontal area index (FAI) (for methods 3 and 4)
         - Surface fractions (7 surface types)
         - Vegetation fraction (for heat roughness methods)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Friction velocity (UStar, for heat roughness methods 2, 4, 5)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Aerodynamic roughness length for momentum (z0m)
         - Zero-plane displacement height (zd)
         - Roughness length for heat/vapour (z0v)

      **Dependencies:** Surface morphology data (building heights, plan area fractions); Vegetation phenology (for seasonally varying LAI methods)

   .. tab-item:: Guidance

      **Recommended for**

      - Urban energy balance simulations requiring dynamic roughness parameters
      - Sites with available building morphology data (methods 3 or 4)
      - Applications where roughness varies seasonally with vegetation state

      **Not recommended for**

      - Sites without morphological data (unless fixed values are well constrained)
      - Method 5 (alternative variable) is not recommended for production use

      **Configuration notes**

      MomentumRoughnessMethod controls z0m/zd: 1=fixed from site parameters, 2=rule-of-thumb (0.1Zh, 0.7Zh), 3=MacDonald et al. (1998) morphometric, 4=plan-area-fraction dependent (Grimmond and Oke 1999), 5=alternative variable (not recommended). A separate HeatRoughnessMethod enum controls z0v: 1=Brutsaert (z0m/10), 2=Kawai et al. (2009), 3=Voogt and Grimmond (2000), 4=Kanda et al. (2007), 5=adaptive based on pervious coverage. Both must be set for a complete roughness configuration.

      .. warning::

         **Common pitfalls**

         - MacDonald method requires accurate frontal area index; errors in FAI directly affect z0m
         - Heat roughness methods 2, 4, 5 depend on friction velocity, introducing coupling with the momentum solution
         - Mixing incompatible momentum and heat roughness methods can produce inconsistent results

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2020-02
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

