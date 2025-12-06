.. _radiation_schemes:

Radiation Schemes
=================

Overview
--------

Net all-wave radiation (:math:`Q^*`) represents the net radiative energy available at the surface after accounting for all incoming and outgoing radiation streams. SUEWS provides three radiation modelling schemes of increasing complexity to accommodate different data availability and application requirements.

Physical Context
----------------

The net all-wave radiation is expressed as:

.. math::

   Q^* = K^* + L^* = (K_\downarrow - K_\uparrow) + (L_\downarrow - L_\uparrow)

where:

- :math:`K_\downarrow` is incoming shortwave radiation (solar radiation reaching the surface)
- :math:`K_\uparrow` is outgoing shortwave radiation (reflected solar radiation)
- :math:`L_\downarrow` is incoming longwave radiation (atmospheric thermal radiation)
- :math:`L_\uparrow` is outgoing longwave radiation (surface thermal emission)

Urban environments present unique challenges for radiation modelling due to geometric complexity (shadowing, radiation trapping in street canyons), material heterogeneity (wide range of albedos and emissivities), and thermal heterogeneity (large surface temperature contrasts).

Available Schemes
-----------------

NARP (Net All-wave Radiation Parameterisation)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Complexity:** Low | **Computational cost:** Low | **Spatial detail:** Grid-averaged

A computationally efficient bulk parameterisation using empirically-derived relations to estimate radiation components from basic meteorological inputs. NARP calculates outgoing shortwave and incoming/outgoing longwave radiation from incoming shortwave radiation, temperature, humidity, and surface characteristics.

**Best for:**

- Grid-scale urban climate modelling
- Long-term simulations requiring computational efficiency
- Applications with limited input data
- Regional-scale studies without detailed 3D geometry

:doc:`Read more about NARP → <narp>`

BEERS (Building Envelope Energy Radiation Scheme)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Complexity:** Medium-High | **Computational cost:** Medium | **Spatial detail:** Point-specific

An advanced scheme for point-specific radiation analysis considering 3D urban geometry, directional radiation components, and thermal comfort metrics. BEERS provides detailed spatial information on radiation fluxes, surface temperatures, shadow patterns, and mean radiant temperature.

**Best for:**

- Microclimate studies requiring spatial detail
- Thermal comfort assessment
- Building energy analysis
- Urban design optimisation

:doc:`Read more about BEERS → <beers>`

SPARTACUS-Surface
^^^^^^^^^^^^^^^^^

**Complexity:** High | **Computational cost:** High | **Spatial detail:** Multi-layer vertical profile

.. warning:: This module is highly experimental and not yet fully tested.

A state-of-the-art multi-layer radiation transfer scheme solving 3D radiative transfer through complex urban-vegetation canopies. SPARTACUS-Surface uses statistical representation of horizontal heterogeneity and up to 15 vertical layers to compute detailed radiation interactions including multiple scattering, absorption, transmission, and thermal emission.

**Best for:**

- Research applications requiring high physical fidelity
- Model development and radiation physics studies
- Detailed canopy-atmosphere interaction research
- Applications requiring vertical radiation profiles

:doc:`Read more about SPARTACUS-Surface → <spartacus_surface>`

Scheme Comparison
-----------------

.. list-table:: Radiation Scheme Characteristics
   :header-rows: 1
   :widths: 20 25 25 30

   * - Characteristic
     - NARP
     - BEERS
     - SPARTACUS-Surface
   * - **Complexity**
     - Low (empirical)
     - Medium-High (3D geometry)
     - High (multi-layer RT)
   * - **Spatial detail**
     - Grid-averaged
     - Point-specific
     - Vertical profiles
   * - **3D geometry**
     - No (bulk albedo/emissivity)
     - Yes (building/vegetation geometry)
     - Yes (statistical canopy)
   * - **Vertical structure**
     - Single layer
     - Surface-level
     - Up to 15 layers
   * - **Computational cost**
     - Very low (~0.1× baseline)
     - Medium (~5-10× baseline)
     - High (~20-50× baseline)
   * - **Input requirements**
     - Minimal (K↓, T, RH)
     - Moderate (geometry, materials)
     - Extensive (canopy structure, optical properties)
   * - **Output detail**
     - Bulk radiation components
     - Directional fluxes, MRT
     - Layer-resolved fluxes, heating rates
   * - **Typical applications**
     - Long-term simulations, regional studies
     - Urban design, thermal comfort
     - Research, model development
   * - **Maturity**
     - Mature (extensively validated)
     - Mature (successor to SOLWEIG)
     - Experimental (under development)

Selection Guidance
------------------

**Choose NARP if you:**

- Need computational efficiency for long simulations
- Work at neighbourhood-to-city scales
- Have limited input data (only incoming shortwave required)
- Require stable, well-validated radiation estimates

**Choose BEERS if you:**

- Need point-specific radiation information
- Study thermal comfort or human exposure
- Require directional radiation components
- Have detailed 3D geometry available

**Choose SPARTACUS-Surface if you:**

- Conduct radiation physics research
- Need vertical radiation profiles through canopy
- Study radiation-vegetation-building interactions
- Require highest physical fidelity (experimental)

Observed Data Option
--------------------

All schemes can be bypassed by providing observed net all-wave radiation directly in the forcing data (``NetRadiationMethod = 0``). This is recommended when high-quality radiation observations are available, as it eliminates modelling uncertainties.

Configuration
-------------

Radiation schemes are selected via the ``net_radiation_method`` parameter in model physics configuration:

.. code-block:: yaml

   model_physics:
     net_radiation_method: 1  # 0=Observed, 1=NARP, 100+=SPARTACUS variants

See :ref:`ModelPhysics <modelphysics>` for complete configuration options and scheme codes.

Further Reading
---------------

.. toctree::
   :maxdepth: 1

   narp
   beers
   spartacus-surface

References
----------

Key radiation scheme publications:

- :cite:t:`O03` - NARP original formulation
- :cite:t:`L11` - NARP implementation in SUEWS
- :cite:t:`F08` - SOLWEIG (predecessor to BEERS) radiant flux modelling
- :cite:t:`Hogan2019Oct` - SPARTACUS-Surface radiative transfer in urban canopies
