.. _model_card_ehc:

Explicit Heat Conduction
========================

:bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

Calculates storage heat flux using explicit 1D heat conduction through urban facets (roof, wall, ground) with separate surface temperature outputs for each element.

**StorageHeatMethod** options:

- ``5`` **EHC** -- Explicit Heat Conduction model with separate roof/wall/ground temperatures

.. tab-set::

   .. tab-item:: Science

      EHC solves the one-dimensional heat conduction equation through multi-layer urban facets (roofs, walls, and ground surfaces) using finite-difference methods. Surface temperature boundary conditions are applied at the top (from the energy balance) and indoor or deep-soil temperature at the bottom. Heterogeneous building facets at different vertical levels are supported, with separate thermal property profiles (conductivity, heat capacity, layer thickness) for each facet type. The storage heat flux is computed from the rate of enthalpy change across all layers. Both explicit forward-difference and Crank-Nicolson implicit solvers are available, with adaptive sub-stepping controlled by CFL stability criteria.

      **Key assumptions**

      - Heat transfer through each facet is one-dimensional (vertical)
      - Material thermal properties are constant within each layer
      - Surface temperature is determined by the energy balance at the outer boundary
      - Indoor or deep-soil temperature provides a fixed lower boundary condition
      - Building facets at multiple vertical levels can have distinct thermal properties

      **Comparison to other schemes**

      EHC extends the earlier Element Surface Temperature Method (ESTM) by supporting heterogeneous multi-layer building facets at different vertical levels and all standard ground-level surface types. Unlike OHM, which relies on empirical coefficients, EHC resolves the heat conduction physics directly. Compared to STEBBS, EHC does not include the full building energy model (indoor processes, water use losses) but provides physically-based surface temperatures for each urban element.

      **Key publications:** :cite:`O05`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-warning-line:`internal`

      Tested internally against established heat conduction solutions and the original ESTM implementation. Provides physically consistent surface temperatures for roof, wall, and ground facets. Formal peer-reviewed validation is ongoing.

      **Datasets**

      - Internal validation against ESTM reference cases

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-info-line:`compute: medium`
      :Data preparation: :bdg-warning-line:`data prep: high`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Thermal conductivity for each layer of roof, wall, and ground facets
         - Volumetric heat capacity for each layer of roof, wall, and ground facets
         - Layer thicknesses for roof, wall, and ground facets
         - Surface fractions for roof, wall, and standard surfaces
         - Number of vertical levels in the urban canopy (nlayer)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Surface temperature (from energy balance iteration)
         - Indoor or deep-soil temperature (lower boundary)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Storage heat flux (delta QS) per facet and grid-averaged
         - Surface temperatures for roof, wall, and ground facets
         - Internal temperature profiles through each facet

      **Dependencies:** Net radiation scheme (NARP, SPARTACUS-Surface, or observed Q*); Energy balance iteration for surface temperature coupling

      **Conflicts:** Cannot be combined with ESTM (StorageHeatMethod=4); Cannot be combined with STEBBS (StorageHeatMethod=7)

   .. tab-item:: Guidance

      **Recommended for**

      - Studies requiring physically-based surface temperatures for individual urban facets
      - Applications where material thermal properties are well characterised
      - Coupling with radiation schemes that use facet-level surface temperatures

      **Not recommended for**

      - Sites where layer-resolved thermal properties are unavailable (use OHM instead)
      - Applications requiring only bulk storage heat flux without surface temperatures

      **Configuration notes**

      StorageHeatMethod=5 enables EHC. Thermal properties (conductivity, heat capacity, layer thickness) must be specified for each depth layer of roof, wall, and ground facets. The number of vertical urban canopy levels (nlayer) controls the number of distinct roof and wall sub-facets. Deep-soil or indoor temperature provides the lower boundary condition.

      .. warning::

         **Common pitfalls**

         - Missing or incorrect layer thickness values (dz=-999) bypass the heat conduction solver
         - Very thin layers require small sub-steps for numerical stability, increasing computation time
         - Thermal properties must be specified consistently across all depth layers

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2023-05
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

      .. warning::

         **Known issues**

         - Requires detailed thermal property data for each layer of each facet type
         - CFL-limited sub-stepping can increase computation time for thin layers

