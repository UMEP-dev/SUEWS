.. _supporting_schemes:

Supporting Schemes and Diagnostics
===================================

This section describes the supporting schemes that provide essential inputs to the energy and water balance calculations, as well as diagnostic meteorological profiles.

Leaf Area Index (LAI)
---------------------

Leaf area index (LAI) is a critical parameter that affects both energy and water balance calculations:

**Energy balance impacts:**

- Surface albedo (vegetation reflectance)
- Aerodynamic roughness (affects turbulent fluxes)
- Radiation interception (particularly in SPARTACUS-Surface)

**Water balance impacts:**

- Interception storage capacity
- Transpiration through stomatal conductance
- Evaporation from vegetation surfaces

SUEWS can use:

- Fixed LAI values configured in the vegetation properties section
- Dynamic LAI from observations or models
- LAI profiles for multi-layer schemes (SPARTACUS-Surface)

For SPARTACUS-Surface, the total vertically integrated LAI is distributed across layers to determine vegetation extinction coefficients in each layer. See :ref:`radiation_schemes` for more details on SPARTACUS-Surface LAI configuration.

Atmospheric Stability
---------------------

**Module:** ``suews_phys_atmmoiststab.f95``

Atmospheric stability affects the calculation of turbulent heat fluxes (:math:`Q_H` and :math:`Q_E`) through stability correction functions. SUEWS supports multiple stability parameterisations:

- **Högström** (1988): Standard stability corrections
- **Campbell-Norman**: Alternative formulation
- **Businger-Högström**: Combined approach

The stability state is determined from the Obukhov length, which relates:

- Surface heat flux
- Friction velocity
- Air temperature

Stability corrections modify the aerodynamic and boundary layer resistances used in the Penman-Monteith equation and sensible heat flux calculations.

.. _rsl_mod:

Wind, Temperature and Humidity Profiles in the Roughness Sublayer
------------------------------------------------------------------

**Module:** ``suews_phys_rslprof.f95``

A diagnostic RSL scheme for calculating the wind, temperature and humidity profiles in the roughness sublayer is implemented in 2020a following :cite:t:`HF07, HF08` and :cite:t:`T19`.
An recent application of this RSL scheme can be found in :cite:t:`T21`.

The diagnostic profiles are outputed in 30 uneven levels between the ground and forcing height, which are divided into two groups:

- One group of levels are evenly distributed within the urban canopy layer characterised by mean height of roughness elements (e.g. buildings, trees, etc.) :math:`z_H`, which determines the number of layers within urban canopy :math:`n_{can}`:

.. math::
   :nowrap:

   \[
         n_{can} =
   \begin{cases}
      3 & \text{if } z_H \leq \text{2 m} \\
      10 & \text{if } \text{2 m} \lt z_H \leq \text{10 m} \\
      15 & \text{if } z_H \gt \text{10 m} \\

   \end{cases}
   \]

- The other levels are evenly distributed between the urban canopy layer top and forcing height.


.. note::

   All the diagnostic profiles (wind speed, temperature and humidity) are calculated
   from the forcing data down into the canopy.
   Therefore it is assumed that the forcing temperature and humidity
   are above the blending height.



Common near-surface diagnostics:

   -  T2: air temperature at 2 m agl
   -  Q2: air specific humidity at 2 m agl
   -  RH2: air relative humidity at 2 m agl
   -  U10: wind speed at 10 m agl

are calculated by the :ref:`RSL scheme <rsl_mod>` by interpolating RSL profile results to the corresponding diagnostic heights.

Convective Boundary Layer
--------------------------

**Module:** ``suews_phys_bluews.f95``

A convective boundary layer (CBL) slab model :cite:`CG01` calculates the CBL height, temperature and humidity during daytime :cite:`O15`.

Biogenic CO2 Fluxes
-------------------

**Module:** ``suews_phys_biogenco2.f95``

SUEWS can optionally calculate biogenic CO2 fluxes from vegetation, accounting for:

- Photosynthetic CO2 uptake during daytime
- Respiration CO2 release
- Temperature and light dependencies
- Vegetation type and LAI effects

These calculations are independent of the energy and water balances but provide additional diagnostic information for carbon cycle studies.

Daily State Calculations
-------------------------

**Module:** ``suews_phys_dailystate.f95``

The daily state module tracks and updates various parameters that change on a daily timescale:

- Accumulated heating and cooling degree days (for anthropogenic heat calculations)
- Daily maximum and minimum temperatures
- Growing degree days (for LAI and phenology models)
- Daily water balance totals

These daily aggregations support sub-models that operate on diurnal or longer timescales.
