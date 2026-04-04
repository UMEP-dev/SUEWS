.. _model_card_qf:

Anthropogenic Heat Flux
=======================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

Calculates anthropogenic heat flux (QF) and associated CO2 emissions from human metabolism, building energy use, and road traffic using temperature-dependent parameterisations and diurnal activity profiles.

**EmissionsMethod** options:

- ``0`` **OBSERVED** -- Uses observed QF values from forcing file (set to zero to exclude QF from energy balance)
- ``1`` **L11** -- Loridan et al. (2011) SAHP method with air temperature and population density
- ``2`` **J11** -- Järvi et al. (2011) SAHP_2 method with heating/cooling degree days
- ``3`` **L11_UPDATED** -- Modified Loridan method using daily mean air temperature
- ``4`` **J19** -- Järvi et al. (2019) method with building energy, metabolism, and traffic
- ``5`` **J19_UPDATED** -- As method 4 but also calculates CO2 emissions

.. tab-set::

   .. tab-item:: Science

      Anthropogenic heat flux is decomposed into three components: human metabolism, building energy use, and road traffic. Each component is modulated by diurnal activity profiles that distinguish weekdays from weekends. The L11 family (methods 1, 3) uses a piecewise-linear relation between air temperature and QF, with heating contributions when temperature falls below a base threshold. Method 3 extends this by using daily mean temperature and adding a cooling (air-conditioning) term above a cooling threshold. The J11 family (method 2) uses heating and cooling degree days (HDD/CDD) with population-density-weighted empirical coefficients. The J19 family (methods 4, 5) calculates each component independently using building energy statistics, vehicle emission factors, and metabolic rates scaled by population density, allowing CO2 fluxes to be derived from local energy and transport data rather than inferred from total QF.

      **Key assumptions**

      - QF can be decomposed into metabolism, building, and traffic components
      - Diurnal profiles adequately capture sub-daily variation in anthropogenic activity
      - Population density (daytime and nighttime) is representative of the grid cell
      - Temperature-QF relationships are transferable across sites with recalibration
      - CO2 emissions scale with fossil fuel fraction and published emission factors

      **Comparison to other schemes**

      The built-in SAHP approaches provide a computationally lightweight alternative to dedicated anthropogenic heat models such as LUCY (LQF) and GreaterQF (GQF), which offer more detailed sectoral disaggregation but require substantially more input data. Pre-calculated QF from external models can be supplied via method 0 when higher fidelity is required.

      **Key publications:** :cite:`L11,J11,J19,A11,L13`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      The L11 and J11 methods have been validated against eddy-covariance-derived residual QF at several urban flux tower sites. The J11 HDD/CDD formulation captures seasonal heating and cooling demand effectively in mid-latitude cities. The J19 component-based approach provides more physically transparent results but requires detailed local energy and transport statistics.

      **Datasets**

      - Helsinki urban flux tower observations (Jarvi et al. 2011)
      - Multiple urban sites in North America and Europe (Loridan et al. 2011)

   .. tab-item:: Technical

      :Spatial scale: neighbourhood, city
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-info-line:`data prep: medium`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Population density (daytime and nighttime) [cap ha-1]
         - QF empirical coefficients (Qf_A, Qf_B, Qf_C for J11)
         - Base temperatures for heating and cooling thresholds
         - AH_MIN and AH_SLOPE coefficients (for L11)
         - Building energy use fraction (QF0_BEU)
         - Fossil fuel fractions (heating and non-heating)
         - Traffic rate and emission factors (for J19)
         - Metabolic heat range (MinQFMetab, MaxQFMetab)
         - Diurnal profiles (anthropogenic heat, traffic, human activity, population)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Air temperature
         - Heating and cooling degree days (computed internally from temperature)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Total anthropogenic heat flux QF [W m-2]
         - Anthropogenic CO2 flux Fc_anthro [umol m-2 s-1]
         - Component fluxes: metabolism, building, traffic, point source

      **Dependencies:** Heating/cooling degree day calculation (internal daily state); Diurnal profile interpolation (internal)

   .. tab-item:: Guidance

      **Recommended for**

      - Standard urban energy balance simulations requiring anthropogenic heat
      - Studies where QF magnitude and diurnal pattern matter but detailed sectoral breakdown is not essential
      - Long-duration runs where computational efficiency is important

      **Not recommended for**

      - Detailed sectoral energy demand studies (use LUCY/LQF or GreaterQF/GQF instead)
      - Sites with highly heterogeneous population distributions within a single grid cell

      **Configuration notes**

      EmissionsMethod 0 reads QF directly from the forcing file (set to zero to exclude QF). Methods 1-3 are QF-only schemes: 1 (L11) uses instantaneous air temperature, 2 (J11) uses HDD/CDD, 3 (L11_UPDATED) uses daily mean temperature with a cooling term. Methods 4-5 (J19 family) calculate each QF component independently from building energy and traffic data; method 5 additionally computes anthropogenic CO2. Methods 3 and 5 are internal updates not yet independently published.

      .. warning::

         **Common pitfalls**

         - Population density must be set for both daytime and nighttime; missing values lead to zero QF
         - Diurnal profiles must be normalised (sum to 1) for correct scaling
         - L11 coefficients (AH_MIN, AH_SLOPE) are site-specific and not directly transferable
         - J19 methods require traffic units to be consistent with emission factor units

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2019-02
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

