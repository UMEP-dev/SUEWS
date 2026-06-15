.. _model_card_narp:

Net All-wave Radiation Parameterisation
=======================================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Calculates net all-wave radiation (Q*) from incoming shortwave radiation, air temperature, humidity, and surface characteristics using empirical longwave parameterisations.

**NetRadiationMethod** options:

- ``1`` **LDOWN_OBSERVED** -- Models Q* using NARP (Net All-wave Radiation Parameterization; Offerle et al. 2003, Loridan et al. 2011) with observed longwave down radiation (L↓) from forcing file
- ``2`` **LDOWN_CLOUD** -- Models Q* using NARP with L↓ estimated from cloud cover fraction
- ``3`` **LDOWN_AIR** -- Models Q* using NARP with L↓ estimated from air temperature and relative humidity

.. tab-set::

   .. tab-item:: Science

      NARP computes outgoing shortwave and incoming/outgoing longwave radiation components based on incoming shortwave radiation, temperature, relative humidity, and surface characteristics (albedo, emissivity). Longwave downward radiation can be observed or modelled from cloud cover fraction (estimated from RH/T or provided directly). The scheme uses bulk surface properties weighted by land cover fractions.

      **Key assumptions**

      - Bulk surface properties (albedo, emissivity) are area-weighted averages over surface types
      - Longwave emission follows Stefan-Boltzmann law with effective surface emissivity
      - Cloud cover fraction can be estimated from air temperature and relative humidity

      **Comparison to other schemes**

      NARP provides a simplified radiation balance suitable for neighbourhood-scale applications. For detailed 3D radiation interactions including building geometry and vegetation shading, SPARTACUS-Surface or BEERS offer higher fidelity at greater computational cost.

      **Key publications:** :cite:`O03,L11`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Well-validated across diverse urban environments. Performs reliably for neighbourhood-scale energy balance calculations where 3D radiation effects are secondary.

      **Datasets**

      - Multiple urban flux tower sites across North America and Europe
      - WRF single-layer urban canopy model evaluation (Loridan et al. 2011)

      **Intercomparison**

      Default recommended radiation scheme in SUEWS. SPARTACUS-Surface provides higher physical fidelity for complex urban canopies but at significantly higher computational cost.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood, city
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Surface albedo (per surface type)
         - Surface emissivity (per surface type)
         - Surface fractions (7 surface types)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Incoming shortwave radiation (Kdown)
         - Air temperature
         - Relative humidity
         - Atmospheric pressure
         - Cloud cover fraction (optional, depending on method)
         - Incoming longwave radiation (optional, if observed)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Net all-wave radiation (Q*)
         - Outgoing shortwave radiation (Kup)
         - Incoming longwave radiation (Ldown, if modelled)
         - Outgoing longwave radiation (Lup)

      **Dependencies:** Sun position calculation (internal)

      **Conflicts:** Cannot be used simultaneously with SPARTACUS-Surface (methods 1001-1003 use SPARTACUS instead)

   .. tab-item:: Guidance

      **Recommended for**

      - Standard urban energy balance simulations at neighbourhood scale
      - Long-duration runs where computational efficiency is important
      - Applications where observed longwave down radiation is available

      **Not recommended for**

      - Studies requiring detailed 3D radiation interactions with building geometry
      - Mean radiant temperature or pedestrian thermal comfort calculations

      **Configuration notes**

      NetRadiationMethod values 1-3 select NARP with different longwave down options: 1 = observed Ldown from forcing file, 2 = modelled from cloud cover fraction, 3 = modelled from air temperature and relative humidity. Method 0 uses observed Q* directly (bypassing NARP).

      .. warning::

         **Common pitfalls**

         - Methods 11-13 (surface temperature variants) should be avoided in production
         - Ensure cloud cover fraction is provided if using method 2

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2017-08
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

      .. warning::

         **Known issues**

         - Surface temperature variants (methods 11-13) are not recommended
         - Zenith angle correction variants (methods 100-300) are not recommended

