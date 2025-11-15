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

Modelling Approaches
^^^^^^^^^^^^^^^^^^^^

SUEWS provides three radiation modelling schemes of increasing complexity to accommodate different data availability and application requirements:

**NARP (Net All-wave Radiation Parameterisation):**
A computationally efficient bulk scheme using empirical relations to estimate radiation components from basic meteorological inputs. Suitable for grid-scale applications where computational efficiency is important and detailed 3D geometry is not available.

**BEERS (Building Envelope Energy Radiation Scheme):**
An advanced scheme for point-specific radiation analysis considering 3D urban geometry, directional radiation, and human thermal comfort. Ideal for microclimate studies, building energy assessment, and thermal comfort applications requiring detailed spatial information.

**SPARTACUS-Surface:**
A state-of-the-art multi-layer radiation transfer scheme solving 3D radiative transfer through complex canopies with statistical representation of horizontal heterogeneity. Designed for research applications requiring high physical fidelity in representing radiation-vegetation-building interactions.

The choice of scheme depends on the application, available input data, and computational resources. All schemes can optionally use observed radiation components when available.

Radiation Data Options
^^^^^^^^^^^^^^^^^^^^^^^

SUEWS offers flexibility in radiation data usage depending on availability:

#. Observed net all-wave radiation can be provided as input instead of
   being calculated by the model.
#. Observed incoming shortwave and incoming longwave components can be
   provided as input, instead of incoming longwave being calculated by
   the model.
#. Other data can be provided as input, such as cloud fraction (see
   :ref:`forcing data configuration <forcing-data>`).
#. **NARP** (Net All-wave Radiation Parameterization) :cite:`O03,L11` scheme calculates outgoing
   shortwave and incoming and outgoing longwave radiation components
   based on incoming shortwave radiation, temperature, relative humidity
   and surface characteristics (albedo, emissivity).
#. `SPARTACUS-Surface`_ computes the 3D interaction of shortwave and longwave radiation with complex surface canopies, including vegetated and urban canopies (with or without vegetation).
#. **BEERS** (Building Envelope Energy Radiation Scheme) calculates detailed radiation components for urban surfaces including point-specific radiation analysis.

NARP (Net All-wave Radiation Parameterisation)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**NARP** :cite:`O03,L11` is the standard radiation scheme in SUEWS, providing a computationally efficient bulk parameterisation for calculating radiation components using empirically-derived relations. NARP calculates outgoing shortwave and incoming/outgoing longwave radiation from basic meteorological inputs (incoming shortwave, temperature, humidity) and surface characteristics (albedo, emissivity).

**Best for:** Grid-scale applications, long-term simulations, limited input data

**Module:** ``suews_phys_narp.f95``

:ref:`Read more about NARP → <narp>`

BEERS (Building Envelope Energy Radiation Scheme)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**BEERS** is an advanced radiation scheme (successor to SOLWEIG) that calculates detailed radiation components at specific points of interest within urban areas, considering 3D geometry of buildings and vegetation. BEERS provides directional radiation, surface temperatures, shadow patterns, and mean radiant temperature for thermal comfort assessment.

**Best for:** Microclimate studies, thermal comfort analysis, building energy assessment, urban design optimisation

**Module:** ``suews_phys_beers.f95``

:ref:`Read more about BEERS → <beers>`

SPARTACUS-Surface
^^^^^^^^^^^^^^^^^

.. warning:: This module is highly experimental and not yet fully tested.

**SPARTACUS-Surface** is a state-of-the-art multi-layer radiation transfer scheme that solves 3D radiative transfer through complex urban-vegetation canopies using statistical representation of horizontal heterogeneity. The scheme uses up to 15 vertical layers to compute detailed radiation interactions including multiple scattering, absorption, transmission, and thermal emission.

**Best for:** Research applications, model development, detailed radiation physics studies, canopy-atmosphere interaction research

**Module:** ``suews_phys_spartacus.f95``

:ref:`Read more about SPARTACUS-Surface → <spartacus_surface>`

Detailed Radiation Scheme Documentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. toctree::
   :maxdepth: 2
   :hidden:

   radiation-schemes/narp
   radiation-schemes/beers
   radiation-schemes/spartacus-surface


Anthropogenic Heat Flux (:math:`Q_F`)
--------------------------------------

**Module:** ``suews_phys_anthro.f95``

#. Two simple anthropogenic heat flux sub-models exist within SUEWS:

   -  :cite:t:`J11` approach, based on heating and cooling degree days and population density (allows distinction between weekdays and weekends).
   -  :cite:t:`L11` approach, based on a linear piece-wise relation with air temperature.

#. Pre-calculated values can be supplied with the meteorological forcing data, either derived from knowledge of the study site, or obtained from other models, for example:

   -  **LUCY** :cite:`A11,L13`. A new version has been now included in UMEP. To distinguish it is referred to as `LQF`_
   -  **GreaterQF** :cite:`I11`. A new version has been now included in UMEP. To distinguish it is referred to as `GQF`_

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
