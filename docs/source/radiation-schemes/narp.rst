.. _narp:

NARP (Net All-wave Radiation Parameterisation)
===============================================

Overview
--------

**NARP** :cite:`O03,L11` is the standard radiation scheme in SUEWS, providing a computationally efficient method for calculating radiation components when detailed observations are not available. NARP uses empirically-derived relations to estimate outgoing shortwave and incoming and outgoing longwave radiation based on basic meteorological inputs and surface characteristics.

**Module:** ``suews_phys_narp.f95``

Physical Basis
--------------

NARP calculates radiation components using:

**Inputs:**

- Incoming shortwave radiation (required meteorological forcing)
- Air temperature and relative humidity
- Surface characteristics (albedo, emissivity)

**Calculated Components:**

- Outgoing shortwave radiation: :math:`K_\uparrow = \alpha \cdot K_\downarrow` where :math:`\alpha` is surface albedo
- Incoming longwave radiation: Estimated from air temperature, humidity, and cloud conditions
- Outgoing longwave radiation: :math:`L_\uparrow = \varepsilon \sigma T_s^4` where :math:`\varepsilon` is surface emissivity, :math:`\sigma` is Stefan-Boltzmann constant, and :math:`T_s` is surface temperature

Empirical Relations
-------------------

NARP employs empirically-derived relations that have been calibrated using observational data from multiple sites. These relations provide reasonable estimates of radiation components without requiring detailed 3D geometric information or complex radiation transfer calculations.

The scheme accounts for:

- Cloud effects on incoming longwave radiation
- Humidity effects on atmospheric emissivity
- Surface temperature effects on outgoing longwave radiation
- Spatial averaging over the model grid

Applications
------------

NARP is well-suited for:

- **Grid-scale urban climate modelling** where computational efficiency is important
- **Long-term simulations** requiring lower computational overhead
- **Applications with limited input data** (only incoming shortwave radiation required)
- **Regional-scale studies** where detailed 3D geometry is not available

The scheme provides adequate accuracy for many urban climate applications whilst maintaining computational efficiency.

Configuration
-------------

NARP is configured through the model physics settings in the YAML configuration:

.. code-block:: yaml

   model_physics:
     net_radiation_method: 1  # 1 = NARP

Required surface parameters:

- **Albedo values** for each surface type (paved, grass, bare soil, water, etc.)
- **Emissivity values** for each surface type
- **Surface fractions** for the model grid

See :ref:`ModelPhysics <modelphysics>` for detailed configuration options.

Limitations
-----------

As a bulk parameterisation scheme, NARP has the following limitations:

- Does not account for detailed 3D geometric effects (shadowing, multiple reflections)
- Assumes homogeneous surface properties within each grid cell
- May underestimate radiation trapping in deep street canyons
- Does not provide directional or point-specific radiation information
- Limited ability to represent highly heterogeneous urban environments

For applications requiring more detailed radiation information, consider :ref:`BEERS <beers>` or :ref:`SPARTACUS-Surface <spartacus_surface>`.

References
----------

Key publications:

- :cite:t:`O03` - Original NARP formulation
- :cite:t:`L11` - NARP implementation in SUEWS
