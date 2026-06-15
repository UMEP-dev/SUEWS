.. _model_card_water_use:

External Water Use Method
=========================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Controls whether external water use (irrigation) is modelled internally or read from observations.

**WaterUseMethod** options:

- ``0`` **MODELLED** -- Water use calculated based on soil moisture deficit and irrigation parameters
- ``1`` **OBSERVED** -- Uses observed water use values from forcing file

.. tab-set::

   .. tab-item:: Science

      External water use adds water to the surface water balance independently of precipitation, affecting soil moisture and evapotranspiration. Method 0 models irrigation based on soil moisture deficit, day of week, and temperature-dependent demand parameters. Method 1 reads observed water use directly from the forcing file.

      **Key assumptions**

      - Irrigation is distributed across irrigable surface types
      - Modelled irrigation responds to soil moisture deficit

      **Key publications:** :cite:`GO02`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Validated as part of the full SUEWS water balance at sites with significant irrigation (e.g., suburban areas in warm climates).

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Irrigation parameters (automatic/manual fractions, day-of-week profiles)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Observed water use (method 1 only)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - External water use per surface type

   .. tab-item:: Guidance

      **Recommended for**

      - Most applications (use method 0 for modelled irrigation)
      - Sites with observed water use data (method 1)

      **Configuration notes**

      WaterUseMethod=0 models irrigation internally based on soil moisture deficit and irrigation parameters. WaterUseMethod=1 reads observed water use values from the forcing file.

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke

