.. _model_card_smd:

Soil Moisture Deficit Method
============================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Controls how soil moisture deficit is determined: modelled from the water balance or provided from observations.

**SMDMethod** options:

- ``0`` **MODELLED** -- SMD calculated from water balance using soil parameters
- ``1`` **OBSERVED_VOLUMETRIC** -- Uses observed volumetric soil moisture content (m³/m³) from forcing file
- ``2`` **OBSERVED_GRAVIMETRIC** -- Uses observed gravimetric soil moisture content (kg/kg) from forcing file

.. tab-set::

   .. tab-item:: Science

      Soil moisture deficit (SMD) controls evapotranspiration through stomatal conductance and surface resistance. Method 0 calculates SMD from the SUEWS water balance model. Methods 1 and 2 use observed soil moisture from the forcing file (volumetric or gravimetric units respectively), bypassing the internal water balance for SMD.

      **Key assumptions**

      - SMD is representative of the neighbourhood-scale root zone
      - Observed values (methods 1-2) are spatially representative of the grid cell

      **Key publications:** :cite:`GO02`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      The modelled SMD approach has been validated as part of the full SUEWS water balance at multiple urban sites.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Soil store capacity per surface type

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Observed soil moisture (methods 1-2 only)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Soil moisture deficit per surface type

   .. tab-item:: Guidance

      **Recommended for**

      - Most applications (use method 0 for modelled SMD)
      - Sites with reliable soil moisture observations (methods 1-2)

      **Configuration notes**

      SMDMethod=0 uses the internal SUEWS water balance. SMDMethod=1 reads volumetric soil moisture (m3/m3) from forcing. SMDMethod=2 reads gravimetric soil moisture (kg/kg) from forcing.

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke

