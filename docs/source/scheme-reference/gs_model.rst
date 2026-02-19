.. _model_card_gs_model:

Stomatal Conductance Model
==========================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Selects the stomatal conductance parameterisation used for the Jarvis-type surface resistance in the Penman-Monteith evapotranspiration calculation.

**GSModel** options:

- ``1`` **JARVI** -- Original parameterisation (Järvi et al. 2011) based on environmental controls
- ``2`` **WARD** -- Updated parameterisation (Ward et al. 2016) with improved temperature and VPD responses

.. tab-set::

   .. tab-item:: Science

      Stomatal conductance controls the surface resistance for latent heat flux in the Penman-Monteith framework. The Jarvis-type model multiplies a maximum conductance by dimensionless stress functions for solar radiation, temperature, vapour pressure deficit, and soil moisture. Method 1 (Jarvi et al. 2011) uses the original parameterisation. Method 2 (Ward et al. 2016) provides updated temperature and VPD response functions with improved performance for a wider range of urban sites.

      **Key assumptions**

      - Surface conductance follows a multiplicative Jarvis-type approach
      - Stress functions are independent of each other

      **Key publications:** :cite:`J11,W16`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Both parameterisations validated against urban flux tower data. Ward (2016) provides improved fit across a broader range of climatic conditions.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Maximum conductance (Gmax) per vegetation type
         - Conductance response parameters (G1-G6)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Solar radiation
         - Air temperature
         - Vapour pressure deficit
         - Soil moisture

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Surface conductance (gs)

   .. tab-item:: Guidance

      **Recommended for**

      - All SUEWS simulations using the Penman-Monteith evapotranspiration

      **Configuration notes**

      GSModel=1 uses Jarvi et al. (2011) parameterisation. GSModel=2 uses Ward et al. (2016) updated parameterisation, recommended for general use.

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke

