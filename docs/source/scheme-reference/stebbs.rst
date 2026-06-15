.. _model_card_stebbs:

Surface Temperature Energy Balance Based Scheme
===============================================

:bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

Calculates facet-level surface temperatures and storage heat flux for buildings by solving explicit energy balances for roof, wall, and ground surfaces with multi-layer heat conduction.

**StorageHeatMethod** options:

- ``7`` **STEBBS** -- use STEBBS storage heat flux for building, others use OHM

.. tab-set::

   .. tab-item:: Science

      STEBBS solves the surface energy balance at individual urban facets (roof, wall, ground) using explicit heat conduction through multi-layer building envelopes. Each facet has its own thermal properties (conductivity, heat capacity, layer thicknesses) and exchanges radiation, convection, and conduction. Internal building temperature is modelled, and the heat capacity can be split between envelope and interior using a configurable weighting factor. For buildings, STEBBS computes the storage heat flux; for other surface types, OHM is used.

      **Key assumptions**

      - Building envelope is represented as 1D multi-layer heat conduction
      - Heat capacity splitting between envelope and interior is parameterisable
      - Indoor convection follows standard heat transfer correlations
      - Water use energy losses to drains are accounted for

      **Comparison to other schemes**

      STEBBS provides higher physical detail than OHM by resolving individual facet temperatures and multi-layer heat conduction, similar in concept to the Element Surface Temperature Method (ESTM) but with improved building energy modelling including indoor processes. EHC provides explicit heat conduction without the full building energy model.

   .. tab-item:: Evaluation

      Evaluation status: :bdg-warning-line:`internal`

      Under development and internal testing. Validation against observations is ongoing. Users should verify results for their specific applications before using in production.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-info-line:`compute: medium`
      :Data preparation: :bdg-warning-line:`data prep: high`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Building thermal properties (conductivity, heat capacity per layer)
         - Layer thicknesses for roof, wall, and ground
         - Building geometry (heights, plan area fractions)
         - Surface albedo and emissivity per facet
         - RC method and weighting factor for heat capacity splitting
         - Internal temperature settings

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Incoming shortwave radiation
         - Air temperature
         - Wind speed
         - Net all-wave radiation (from NARP or observed)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Storage heat flux (delta QS) for buildings
         - Facet surface temperatures (roof, wall, ground)
         - Internal building temperature
         - Detailed STEBBS output columns

      **Dependencies:** Net radiation scheme (NARP or observed) for non-building surfaces; OHM for non-building surface types

      **Conflicts:** Cannot be combined with ESTM (StorageHeatMethod=4)

   .. tab-item:: Guidance

      **Recommended for**

      - Research applications investigating facet-level urban surface temperatures
      - Building energy studies requiring internal temperature modelling

      **Not recommended for**

      - Production runs until peer-reviewed validation is complete
      - Sites where building thermal property data is unavailable
      - Long-duration runs where computational cost is a constraint

      **Configuration notes**

      StorageHeatMethod=7 enables STEBBS for building surfaces (other surfaces use OHM). StebbsMethod controls whether STEBBS runs (0=disabled, 1=default parameters, 2=user-provided parameters). RCMethod controls the building envelope heat capacity splitting (0=none, 1=user-defined weighting, 2=parameterised from material properties).

      .. warning::

         **Common pitfalls**

         - StorageHeatMethod must be set to 7 alongside StebbsMethod > 0
         - Missing building layer properties will cause runtime errors
         - Results are sensitive to the number and specification of thermal layers

   .. tab-item:: Status

      :Development status: :bdg-warning:`experimental`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: yes

      **Test coverage**

      - Integration tests via make test

      .. warning::

         **Known issues**

         - Peer-reviewed validation not yet published
         - Requires detailed building thermal property data that may not be readily available

