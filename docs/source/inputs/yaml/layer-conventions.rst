.. _layer_conventions:

Vertical and Material Layer Conventions
=======================================

SUEWS uses the word *layer* for two independent coordinates: vertical position
above ground and position from the exposed surface inward. Keeping these
coordinates separate is important when defining roof and wall properties and
when choosing a storage-heat or radiation method.

Two independent layer coordinates
---------------------------------

**SPARTACUS vertical layers** (``vertical_layers`` in YAML) divide the height
range from ground level to the maximum building height. Each layer is a height
interval bounded by horizontal planes, and the layers are ordered from the
ground upward. Layer ``i`` extends from ``height[i]`` to ``height[i+1]``.
``walls[i]`` describes a vertical facade spanning layer ``i``, while
``roofs[i]`` describes horizontal roof area associated with that layer:

.. code-block:: text

   height[i+1] ---------------- roof[i] (horizontal upper surface)
                    |
                    | wall[i] (vertical facade)
                    |
   height[i]   ---------------- lower boundary

Thus, a roof in a vertical layer is still a horizontal surface, and a wall in
a vertical layer is still a vertical surface. The vertical-layer index only
identifies the facet's position between the ground and maximum building height.

**Material layers** (``thermal_layers`` in YAML) are five layers ordered from
the exposed surface inward. The same index in ``dz``, ``k``, and ``rho_cp``
describes the same material layer:

.. code-block:: text

   atmosphere | material layer 0 | layer 1 | layer 2 | layer 3 | layer 4 | interior/deep material
              <----------------------- surface inward ----------------------->

For a horizontal ground or roof facet, the material layers extend downward.
For a vertical wall facet, they extend horizontally into the wall. Material
layers therefore have no fixed horizontal or vertical orientation; their
direction depends on the surface to which they belong.

Material-layer input structure
------------------------------

Each material layer has three thermal properties:

.. list-table::
   :header-rows: 1
   :widths: 18 22 60

   * - YAML field
     - Unit
     - Meaning
   * - ``dz[j]``
     - m
     - Thickness of material layer ``j``. This is an individual thickness, not
       cumulative depth.
   * - ``k[j]``
     - W m\ :sup:`-1` K\ :sup:`-1`
     - Thermal conductivity of material layer ``j``.
   * - ``rho_cp[j]``
     - J m\ :sup:`-3` K\ :sup:`-1`
     - Volumetric heat capacity of material layer ``j``.

Supply five entries for each property so that the YAML structure matches the
five material layers used internally by SUEWS. Use the same order, from the
exposed surface inward, for every land-cover, roof, and wall facet. The
thickness values do not need to increase or decrease systematically: they
represent the physical construction chosen for that surface.

.. warning::

   The current configuration validator checks that required material-property
   arrays are present and non-empty, but it does not enforce a length of five.
   Passing validation therefore does not confirm that the material-layer inputs
   are complete for the selected storage-heat method. When material-layer arrays
   are provided, supply five entries and note how each method uses them:

   - EHC (``5``) uses all five material layers of its conducting facets.
   - DyOHM (``6``) uses material layer ``0`` of every SUEWS land-cover surface.
   - Method ``7`` uses material layer ``0`` only for non-building DyOHM
     surfaces. STEBBS represents the building, so
     ``land_cover.bldgs.thermal_layers`` is not used.
   - Building-only DyOHM (``8``) uses material layer ``0`` of
     ``land_cover.bldgs``; non-building surfaces use ordinary OHM.
   - Ordinary OHM (``1``) does not use material-layer properties.
   - AnOHM (``3``) uses separate bulk properties, such as ``ch_anohm``,
     ``rho_cp_anohm``, and ``k_anohm``.

The following fragment shows one wall facet. It is abbreviated and is not a
complete site configuration.

.. code-block:: yaml

   vertical_layers:
     # Other vertical geometry and facet entries are omitted here.
     walls:
       - alb: {value: 0.25}
         emis: {value: 0.95}
         thermal_layers:
           # exposed surface -> interior
           dz:     {value: [0.02, 0.08, 0.10, 0.05, 0.02]}
           k:      {value: [0.80, 0.04, 1.20, 0.16, 0.50]}
           rho_cp: {value: [1800000, 30000, 2000000, 800000, 1200000]}
         wall_specular_frac: {value: 0.0}

When thermal properties affect the calculation
------------------------------------------------

The selected storage-heat method determines which material layers affect model
results. Values can remain in a complete configuration even when a method does
not use them.

.. list-table::
   :header-rows: 1
   :widths: 22 38 40

   * - Storage-heat method
     - Thermal properties that affect the calculation
     - What to provide
   * - Observed storage heat (``0``) or ordinary OHM (``1``)
     - Roof, wall, and land-cover ``dz``, ``k``, and ``rho_cp`` do not affect
       the storage-heat calculation. Ordinary OHM uses prescribed OHM
       coefficients.
     - Thermal properties may remain in the common configuration but are
       ignored by these storage-heat paths.
   * - EHC (``5``)
     - All five material layers are used for every roof and wall vertical layer
       and for the solid non-building land covers (paved,
       vegetation, and bare soil). The ``land_cover.bldgs`` and water thermal
       arrays are not used by facet-resolved EHC conduction.
     - Provide complete five-entry ``dz``, ``k``, and ``rho_cp`` arrays for all
       roof and wall facets and for the solid non-building land covers.
   * - DyOHM for all surfaces (``6``)
     - Only the outermost material layer, index ``0``, affects the dynamic OHM
       coefficients. Buildings use ``land_cover.bldgs.thermal_layers``; each
       non-building land cover uses its own ``thermal_layers``. Roof and wall
       material properties are not used by DyOHM.
     - Keep the five-entry arrays for a consistent configuration, but only
       element ``0`` affects DyOHM results.
   * - STEBBS buildings with DyOHM non-building surfaces (``7``)
     - STEBBS supplies the building storage heat and roof/wall temperatures, so
       ``land_cover.bldgs.thermal_layers`` is not used. Each non-building land
       cover uses its outermost material layer for dynamic OHM coefficients and
       its DyOHM surface-temperature update. This method requires
       SPARTACUS-Surface net radiation (``1001``, ``1002``, or ``1003``), which
       uses the separate STEBBS roof and wall temperatures.
     - Provide five-entry ``dz``, ``k``, and ``rho_cp`` arrays for the
       non-building surfaces. Material layer ``0`` supplies the properties used
       by this path. No building material-layer properties are required by
       method 7.
   * - Building-only DyOHM (``8``)
     - Only the outermost material layer of
       ``land_cover.bldgs.thermal_layers`` affects the building dynamic OHM
       coefficients. Roof and wall material properties are not used, and
       non-building surfaces use ordinary OHM coefficients.
     - Keep five-entry arrays for a consistent configuration, but only
       ``land_cover.bldgs`` element ``0`` affects the DyOHM building calculation.

When roof and wall optical properties affect the calculation
------------------------------------------------------------

SPARTACUS-Surface uses the vertical-layer geometry and the optical properties
of the roof and wall facets:

.. list-table::
   :header-rows: 1
   :widths: 30 22 48

   * - Property
     - Facet
     - Use
   * - ``alb`` and ``emis``
     - Roof and wall
     - Used by SPARTACUS-Surface for shortwave and longwave radiation in each
       vertical layer.
   * - ``roof_albedo_dir_mult_fact``
     - Roof only
     - Used by SPARTACUS-Surface to derive direct-beam roof albedo. A value on a
       wall item is ignored.
   * - ``wall_specular_frac``
     - Wall only
     - Used by SPARTACUS-Surface as the specular fraction of wall shortwave
       reflection. A value on a roof item is ignored.

If SPARTACUS-Surface is not selected, the height-dependent roof and wall
optical properties do not control the standard aggregate radiation calculation.

Related reference pages
-----------------------

- :ref:`thermallayers`
- :ref:`verticallayers`
- :ref:`rooflayer`
- :ref:`walllayer`
- :ref:`modelphysics`
- :ref:`SPARTACUS-Surface`
