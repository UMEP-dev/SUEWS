.. _model_card_same_albedo:

Uniform Wall/Roof Albedo Assumption
===================================

:bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Controls whether all walls (or roofs) within the urban canopy share the same albedo value, simplifying the radiation calculation for multi-layer schemes.

.. tab-set::

   .. tab-item:: Science

      When enabled, all wall (or roof) surfaces in the multi-layer urban canopy are assigned the same albedo value. This simplifies the radiation transfer calculation in SPARTACUS-Surface by reducing the number of unique surface property combinations. Two separate enums control walls (SameAlbedoWall) and roofs (SameAlbedoRoof) independently.

      **Key assumptions**

      - Albedo variation between walls (or roofs) at different levels is negligible

   .. tab-item:: Evaluation

      Evaluation status: :bdg-warning-line:`internal`

      Internal setting primarily used with SPARTACUS-Surface multi-layer radiation. Reduces complexity when vertical variation in surface optical properties is not important.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Wall/roof albedo values

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Simplified albedo input to radiation scheme

   .. tab-item:: Guidance

      **Configuration notes**

      SameAlbedoWall=1 applies a uniform wall albedo across all layers. SameAlbedoRoof=1 applies a uniform roof albedo. Both default to 0 (allowing per-layer variation). Primarily relevant when using SPARTACUS-Surface (NetRadiationMethod 1001-1003).

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke

