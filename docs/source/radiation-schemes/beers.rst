.. _beers:

BEERS (Building Envelope Energy Radiation Scheme)
==================================================

Overview
--------

**BEERS** is the successor to SOLWEIG and provides advanced radiation modelling for urban environments. Unlike bulk parameterisation schemes, BEERS calculates detailed radiation components at specific points of interest (POI) within urban areas, considering the complex 3D geometry of buildings and vegetation.

**Module:** ``suews_phys_beers.f95``

Key Features
------------

- **Point-specific Analysis:** Calculates radiation at specific points rather than grid averages
- **Directional Radiation:** Provides radiation from cardinal directions (north, south, east, west)
- **Surface Temperature Modelling:** Computes ground, wall, and roof surface temperatures
- **Mean Radiant Temperature:** Calculates mean radiant temperature for human thermal comfort studies
- **Shadow Analysis:** Models shadows cast by buildings and vegetation on ground and walls

Physical Basis
--------------

BEERS solves the urban radiation balance using a detailed geometric approach:

1. **Solar Position Calculation**

   Determines precise sun position using astronomical algorithms based on:

   - Latitude and longitude
   - Date and time (accounting for equation of time)
   - Time zone and daylight saving adjustments

2. **Geometry Analysis**

   Analyses 3D urban geometry to determine:

   - Sky view factors from different surfaces and orientations
   - Shadow patterns from buildings and vegetation
   - View factors between different urban facets
   - Mutual visibility between surfaces

3. **Radiation Transfer**

   Calculates radiation components accounting for:

   - **Direct shortwave radiation:** Direct beam from sun (with shadowing)
   - **Diffuse shortwave radiation:** Sky diffuse radiation weighted by sky view factor
   - **Reflected shortwave radiation:** Multiple reflections between surfaces
   - **Incoming longwave radiation:** Atmospheric emission and emission from surrounding surfaces
   - **Outgoing longwave radiation:** Surface thermal emission based on facet temperatures

4. **Surface Energy Balance**

   Solves energy balance for different urban surfaces:

   - Ground surfaces (sunlit and shaded)
   - Wall surfaces (all orientations)
   - Roof surfaces

   The energy balance determines surface temperatures which feed back into longwave radiation calculations.

5. **Thermal Comfort**

   Computes mean radiant temperature (:math:`T_{mrt}`) for human comfort assessment:

   .. math::

      T_{mrt} = \left[\frac{1}{\sigma}\sum_i F_i \cdot (L_{\downarrow,i} + L_{\uparrow,i})\right]^{1/4} - 273.15

   where :math:`F_i` are view factors from the person to different surfaces and :math:`\sigma` is the Stefan-Boltzmann constant.

Output Variables
----------------

BEERS provides comprehensive radiation output including:

**Radiation Fluxes:**

- **Incoming/Outgoing Shortwave:** Kdown2d, Kup2d at point of interest
- **Incoming/Outgoing Longwave:** Ldown2d, Lup2d at point of interest
- **Directional Components:** Radiation from north, south, east, west directions

**Shadow Information:**

- **SH_Ground:** Shadow patterns on ground surfaces
- **SH_Walls:** Shadow patterns on wall surfaces

**Sky View Factors:**

- **SVF_Ground:** Sky view factor from ground
- **SVF_Roof:** Sky view factor from roof
- **SVF_BdVeg:** Sky view factor considering buildings and vegetation

**Surface Temperatures:**

- **Tg:** Ground surface temperature
- **Tw:** Wall surface temperatures (by orientation)
- **Ta:** Air temperature at point of interest

**Comfort Metrics:**

- **Tmrt:** Mean radiant temperature for thermal comfort assessment

Applications
------------

BEERS is designed for applications requiring detailed spatial radiation information:

- **Urban climate analysis** and heat island studies at the street/neighbourhood scale
- **Building energy assessment** in urban contexts with surrounding geometry effects
- **Human thermal comfort evaluation** in urban spaces (parks, plazas, pedestrian areas)
- **Urban planning and design optimisation** for thermal comfort and energy efficiency
- **Microclimate analysis** for specific locations (e.g., outdoor seating areas, pedestrian routes)
- **Climate adaptation planning** evaluating shading strategies and design interventions

Configuration
-------------

BEERS can be enabled in SUEWS through the model physics settings:

.. code-block:: yaml

   model_physics:
     net_radiation_method: 3  # 3 = BEERS

**Required Inputs:**

Surface Properties:

- **Albedo values** for ground and building surfaces
- **Emissivity values** for ground and wall surfaces
- Building morphology parameters:

  - Plan area fraction of buildings
  - Building height (mean and/or distribution)
  - Building wall-to-plan area ratio

Location Information:

- **Coordinates** (latitude, longitude)
- **Time zone** information
- **Point of interest** coordinates (if different from grid centre)

Geometric Data:

- **Digital Surface Model (DSM)** or building height data
- **Building footprints** (optional, for improved accuracy)
- **Vegetation height** data (if vegetation effects are important)

See :ref:`ModelPhysics <modelphysics>` for detailed configuration options.

Computational Considerations
-----------------------------

**Advantages:**

- Provides detailed point-specific radiation information
- Accounts for complex 3D geometric effects
- Suitable for thermal comfort applications
- Can be used for specific locations within a larger model domain

**Considerations:**

- More computationally intensive than bulk schemes like :ref:`NARP <narp>`
- Requires more detailed geometric input data
- Best suited for specific point analysis rather than large-scale gridded applications
- May require calibration of surface temperature calculations for specific sites

.. note::
   BEERS provides detailed radiation output that is particularly valuable for applications requiring point-specific radiation analysis or human thermal comfort assessment in urban environments. For large-scale gridded applications with limited computational resources, consider :ref:`NARP <narp>` instead.

Comparison with Other Schemes
------------------------------

**BEERS vs. NARP:**

- BEERS: Point-specific, 3D geometry, directional radiation, thermal comfort
- NARP: Grid-averaged, bulk parameterisation, computationally efficient

**BEERS vs. SPARTACUS-Surface:**

- BEERS: Point-specific analysis, deterministic geometry, thermal comfort focus
- SPARTACUS: Multi-layer radiative transfer, statistical geometry, research-grade physics

Technical Notes
---------------

**Shadow Algorithm:**

BEERS uses ray-tracing techniques to determine shadow patterns. For each point of interest and time step, the algorithm:

1. Calculates solar position (azimuth and elevation)
2. Traces ray from POI towards sun
3. Checks intersection with building and vegetation geometry
4. Determines if point is in shadow or sunlit

**Surface Temperature Calculation:**

Surface temperatures are calculated by solving the energy balance equation for each facet:

.. math::

   Q^*_f + Q_F = Q_{conduction} + Q_{convection}

where subscript :math:`f` denotes the specific facet (ground, wall, roof).

**View Factor Computation:**

View factors between surfaces are computed using geometric relations accounting for surface orientation, distance, and mutual visibility. These view factors determine the exchange of longwave radiation between surfaces.

References
----------

BEERS is based on methods developed in SOLWEIG and related radiation models. Key concepts include:

- Solar geometry calculations
- Urban canyon radiation models
- View factor computations
- Mean radiant temperature for thermal comfort

For more information on the theoretical background, see the :ref:`scientific background <scientific_background>` section.
