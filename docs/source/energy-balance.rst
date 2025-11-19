.. _energy_balance:

Energy Balance
==============

The surface energy balance is expressed as:

.. math::

   Q^* + Q_F = \Delta Q_S + Q_H + Q_E

where :math:`Q^*` is the net all-wave radiation, :math:`Q_F` is the anthropogenic heat flux, :math:`\Delta Q_S` is the net storage heat flux, :math:`Q_H` is the turbulent sensible heat flux, and :math:`Q_E` is the turbulent latent heat flux.

This page describes the parameterisations available for each energy balance component.

Net All-Wave Radiation (:math:`Q^*`)
-------------------------------------

Net all-wave radiation (:math:`Q^*`) is a fundamental component of the surface energy balance, representing the net radiative energy available at the surface after accounting for all incoming and outgoing radiation streams.

Physical Basis
^^^^^^^^^^^^^^

The net all-wave radiation is the sum of shortwave (solar) and longwave (thermal infrared) radiation components:

.. math::

   Q^* = K^* + L^* = (K_\downarrow - K_\uparrow) + (L_\downarrow - L_\uparrow)

where:

- :math:`K_\downarrow` is incoming shortwave radiation (solar radiation reaching the surface)
- :math:`K_\uparrow` is outgoing shortwave radiation (reflected solar radiation)
- :math:`L_\downarrow` is incoming longwave radiation (atmospheric thermal radiation)
- :math:`L_\uparrow` is outgoing longwave radiation (surface thermal emission)

**Shortwave radiation** (:math:`\sim 0.3-3` μm) originates from the sun and is controlled by:

- Solar geometry (latitude, time of day, season)
- Atmospheric conditions (clouds, aerosols, water vapour)
- Surface albedo (reflectivity)

**Longwave radiation** (:math:`\sim 3-100` μm) represents thermal emission and is controlled by:

- Atmospheric emissivity (temperature, humidity, cloud cover)
- Surface temperature and emissivity
- Sky view factor (proportion of sky visible from the surface)

Urban Radiation Complexity
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Urban environments present unique challenges for radiation modelling due to:

**Geometric Complexity:**

- Multiple surface orientations (roofs, walls, ground)
- Shadowing and mutual shading between buildings
- Radiation trapping in street canyons (multiple reflections)
- Reduced sky view factors

**Material Heterogeneity:**

- Wide range of surface albedos (0.05-0.85 for urban materials)
- Variable emissivities (0.85-0.95 for most urban surfaces)
- Anisotropic reflection properties (specular vs. diffuse)

**Thermal Heterogeneity:**

- Large surface temperature contrasts (e.g., sunlit walls vs. shaded ground)
- Thermal mass effects influencing longwave emission
- Anthropogenic heat modifying the radiation balance

Available Radiation Schemes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

SUEWS provides three radiation modelling schemes of increasing complexity to accommodate different data availability and application requirements. Each scheme represents a different trade-off between computational cost, input data requirements, and physical fidelity.

For detailed information on each scheme, selection guidance, and comparison tables, see :ref:`Radiation Schemes <radiation_schemes>`.

**Quick Overview:**

- :ref:`NARP <narp>` - Bulk parameterisation for grid-scale applications (computationally efficient, minimal input data)
- :ref:`BEERS <beers>` - Point-specific radiation analysis for microclimate and thermal comfort studies
- :ref:`SPARTACUS-Surface <spartacus_surface>` - Multi-layer radiative transfer for research applications (experimental)

All schemes can optionally use observed radiation components when available (see :ref:`forcing data configuration <forcing-data>`).

Detailed Radiation Scheme Documentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For comprehensive information on each radiation scheme, including:

- Physical basis and governing equations
- Implementation details and assumptions
- Configuration examples and required parameters
- Applications and limitations
- Scheme comparison and selection guidance

See the :ref:`Radiation Schemes <radiation_schemes>` section.


Anthropogenic Heat Flux (:math:`Q_F`)
--------------------------------------

Anthropogenic heat flux represents heat released by human activities, including building energy consumption, transportation, and human metabolism. SUEWS provides four methods ranging from simple observed inputs to component-based parameterisations.

**Available Schemes:**

- :ref:`Observed <anthropogenic_observed>` - Use pre-calculated values from external models (LUCY, GreaterQF, inventories)
- :ref:`L11 <anthropogenic_l11>` - Simple temperature-based parameterisation (heating only)
- :ref:`J11 <anthropogenic_j11>` - Degree day-based (heating + cooling, seasonal adaptation)
- :ref:`J19 <anthropogenic_j19>` - Component-based (buildings/traffic/metabolism, CO₂ co-calculation)

For detailed information on each scheme, selection guidance, and comparison tables, see :ref:`Anthropogenic Heat Schemes <anthropogenic_schemes>`.

**Module:** ``suews_phys_anthro.f95``

Storage Heat Flux (:math:`\Delta Q_S`)
---------------------------------------

**Modules:** ``suews_phys_ohm.f95``, ``suews_phys_anohm.f95``, ``suews_phys_estm.f95``, ``suews_phys_ehc.f95``, ``suews_phys_stebbs.f95``

#. Five sub-models are available to estimate the storage heat flux:

   -  **OHM** (Objective Hysteresis Model) :cite:`G91,GO99,GO02`. Storage heat flux is calculated using empirically-fitted relations with net all-wave radiation and the rate of change in net all-wave radiation.
   -  **AnOHM** (Analytical Objective Hysteresis Model) :cite:`S17`. OHM approach using analytically-derived coefficients. |NotRecmd|
   -  **ESTM** (Element Surface Temperature Method) :cite:`O05`. Heat transfer through urban facets (roof, wall, road, interior) is calculated from surface temperature measurements and knowledge of material properties. |NotRecmd|
   -  **EHC** (Explicit Heat Conduction): Separate roof/wall/ground temperatures
   -  **STEBBS** (Surface Temperature Energy Balance Based Scheme): Facet temperatures (building, paved, vegetation, soil, water)

#. Alternatively, 'observed' storage heat flux can be supplied with the meteorological forcing data.

Turbulent Heat Fluxes (:math:`Q_H` and :math:`Q_E`)
----------------------------------------------------

**Modules:** ``suews_phys_lumps.f95``, ``suews_phys_resist.f95``, ``suews_phys_evap.f95``

#. **LUMPS** (Local-scale Urban Meteorological Parameterization Scheme) :cite:`GO02` provides a simple means of estimating sensible and latent heat fluxes based on the proportion of vegetation in the study area.

#. **SUEWS** adopts a more biophysical approach to calculate the latent heat flux; the sensible heat flux is then calculated as the residual of the energy balance.
   The initial estimate of stability is based on the LUMPS calculations of sensible and latent heat flux.
   Future versions will have alternative sensible heat and storage heat flux options.

Sensible and latent heat fluxes from both LUMPS and SUEWS are provided in the `output_files`.
Whether the turbulent heat fluxes are calculated using LUMPS or SUEWS can have a major impact on the results.
For SUEWS, an appropriate surface conductance parameterisation is also critical :cite:`J11` :cite:`W16`.
For more details see ``Differences_between_SUEWS_LUMPS_and_FRAISE``.

.. _LQF: http://umep-docs.readthedocs.io/en/latest/OtherManuals/LQF_Manual.html
.. _GQF: http://umep-docs.readthedocs.io/en/latest/OtherManuals/GQF_Manual.html
