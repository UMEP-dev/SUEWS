.. _model_card_lumps:

LUMPS Turbulent Flux Scheme
===========================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Estimates sensible and latent heat fluxes using a simple de Bruin and Holtslag approach modified for urban areas with vegetation fraction.

.. tab-set::

   .. tab-item:: Science

      LUMPS (Local-scale Urban Meteorological Parameterization Scheme) calculates sensible and latent heat fluxes from the available energy (Q* + QF - delta QS) using a modified de Bruin and Holtslag approach. The partitioning between QH and QE depends on the psychrometric constant, the slope of the saturation vapour pressure curve, and an empirical vegetation fraction parameter that captures the urban Bowen ratio behaviour. A dynamic water bucket tracks surface wetness from rainfall, draining at a specified rate, so that wet surfaces evaporate at an enhanced rate. Vegetation phenology is derived from LAI to modulate the effective vegetated fraction seasonally following Loridan et al. (2011).

      **Key assumptions**

      - Available energy is known from the radiation and storage heat flux calculations
      - Surface energy partitioning depends primarily on vegetation fraction and surface wetness
      - The de Bruin and Holtslag framework is applicable at the neighbourhood scale
      - A simple water bucket adequately represents surface moisture availability

      **Comparison to other schemes**

      LUMPS is the simpler of two turbulent flux approaches in SUEWS. The full SUEWS biophysical approach calculates QE via a Penman-Monteith formulation with a detailed surface conductance model (Jarvis type) and derives QH as the energy balance residual. LUMPS is always calculated internally and provides the initial stability estimate used by the full SUEWS scheme. Both sets of fluxes are provided in output files.

      **Key publications:** :cite:`GO02,L11`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      LUMPS provides reasonable first-order estimates of turbulent fluxes with minimal input requirements. It performs best in cities with moderate vegetation fractions. For applications requiring accurate latent heat fluxes, the full SUEWS biophysical approach is preferred as it accounts for surface conductance, soil moisture stress, and irrigation.

      **Datasets**

      - Multiple urban sites including Vancouver, Lodz, Basel (Grimmond and Oke 2002)
      - Urban flux tower observations across different climate zones

   .. tab-item:: Technical

      :Spatial scale: neighbourhood, city
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - LUMPS alpha and beta coefficients
         - Surface fractions (7 surface types)
         - Vegetation LAI parameters (LAImax, LAImin per vegetation type)
         - LUMPS drain rate and rain cover threshold
         - Maximum rain bucket reservoir capacity

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Net all-wave radiation (Q*)
         - Anthropogenic heat flux (QF)
         - Storage heat flux (delta QS)
         - Air temperature
         - Atmospheric pressure
         - Precipitation

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Sensible heat flux (QH_LUMPS)
         - Latent heat flux (QE_LUMPS)
         - Psychrometric constant
         - Slope of saturation vapour pressure curve

      **Dependencies:** Net radiation scheme (Q*); Storage heat flux scheme (delta QS); Anthropogenic heat flux (QF)

   .. tab-item:: Guidance

      **Recommended for**

      - Quick first-order estimates of urban turbulent fluxes with minimal data
      - Internal use as initial stability estimate within the full SUEWS scheme
      - Applications where surface conductance data are unavailable

      **Not recommended for**

      - Studies requiring accurate latent heat flux partitioning (use full SUEWS biophysical approach)
      - Sites with significant irrigation or complex water management
      - Applications where soil moisture stress is an important control on evaporation

      **Configuration notes**

      LUMPS is always calculated internally by SUEWS and does not have its own selection enum. It provides the initial stability estimate used by the full SUEWS energy balance. Both LUMPS and SUEWS turbulent fluxes are written to output files. The choice between reporting LUMPS or SUEWS fluxes as the primary result depends on the user's analysis. For most applications the full SUEWS biophysical QE is preferred.

      .. warning::

         **Common pitfalls**

         - LUMPS QE does not account for surface conductance or soil moisture stress
         - Rain bucket parameters (drain rate, cover threshold) need site-appropriate values
         - Vegetation phenology in LUMPS uses a simplified LAI-based approach that may differ from the full SUEWS phenology

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2022-08
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

