.. _model_card_snow:

Snow Processes
==============

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Controls snow accumulation, melt, and albedo effects on the surface energy balance.

**SnowUse** options:

- ``0`` **DISABLED** -- Snow processes not included
- ``1`` **ENABLED** -- Snow accumulation, melt, and albedo effects included

.. tab-set::

   .. tab-item:: Science

      When enabled, the snow module tracks snow accumulation, snowmelt, and snow-covered fraction for each surface type. Snow affects the surface albedo, roughness length, and energy balance partitioning. Snowmelt is driven by available energy and air temperature following a degree-day-plus-energy approach (Jarvi et al. 2014).

      **Key assumptions**

      - Snow is distributed across all surface types according to precipitation partitioning
      - Snowmelt rate is controlled by available energy and degree-day parameters

      **Comparison to other schemes**

      A simple toggle for SUEWS snow processes. When disabled (0), all precipitation is treated as liquid regardless of temperature.

      **Key publications:** :cite:`J14`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Validated against Helsinki and other high-latitude urban sites where snow processes are significant (Jarvi et al. 2014).

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Snow density parameters
         - Snow albedo limits (min, max)
         - Degree-day melt factor

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Air temperature
         - Precipitation

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Snow water equivalent per surface type
         - Snow-covered fraction
         - Snowmelt rate

   .. tab-item:: Guidance

      **Recommended for**

      - Sites where snow occurs and affects the energy balance

      **Not recommended for**

      - Tropical or snow-free sites (set SnowUse=0)

      **Configuration notes**

      SnowUse=0 disables snow processes entirely. SnowUse=1 enables full snow accumulation, melt, and albedo effects.

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke

