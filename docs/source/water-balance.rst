.. _water_balance:

Water Balance
=============

Overview
--------

The running water balance at each time step is based on the urban water balance model of :cite:t:`G86` and urban evaporation-interception scheme of :cite:t:`GO91`.

The water balance equation is:

.. math::

   P + I = E + \Delta S + R

where:

- :math:`P` is precipitation (required meteorological forcing)
- :math:`I` is irrigation (modelled or observed)
- :math:`E` is evapotranspiration (linked to latent heat flux :math:`Q_E`)
- :math:`\Delta S` is the change in canopy and soil water storage
- :math:`R` is runoff

Key features:

-  Precipitation is a required variable in the meteorological forcing file.
-  Irrigation can be modelled :cite:`J11` or observed values can be provided if data are available.
-  Drainage equations and coefficients to use must be specified in the input files.
-  Soil moisture can be calculated by the model.
-  Runoff is permitted:

   -  between surface types within each model grid
   -  between model grids (|NotAvail|)
   -  to deep soil
   -  to pipes.

Evapotranspiration
------------------

Evapotranspiration (:math:`E`) is directly related to the latent heat flux (:math:`Q_E`) in the energy balance through:

.. math::

   Q_E = \lambda E

where :math:`\lambda` is the latent heat of vaporisation.

SUEWS calculates evapotranspiration using the Penman-Monteith equation, accounting for:

- Evaporation from wet surfaces (interception storage)
- Transpiration from vegetation through stomatal control
- Evaporation from soil and water surfaces

The surface conductance parameterisation is critical for accurately simulating evapotranspiration :cite:`J11,W16`. See :ref:`heat_fluxes` for details on the LUMPS and SUEWS approaches.

Runoff Generation
-----------------

**Module:** ``suews_phys_waterdist.f95``

SUEWS generates surface runoff through two primary mechanisms, both calculated at each model timestep:

1. **Infiltration Excess Runoff (Hortonian Overland Flow)**

   When the precipitation rate exceeds the infiltration capacity (default: 10 mm hr⁻¹), the excess precipitation becomes runoff:

   .. math::

      R_\text{I} = P - I_\text{th}

   where:

   - :math:`R_\text{I}` is the infiltration excess runoff rate
   - :math:`P` is the precipitation rate at the current timestep
   - :math:`I_\text{th}` is the infiltration capacity threshold (adjusted for timestep duration)

   This mechanism applies to all surface types (impervious and pervious).

2. **Saturation Excess Runoff**

   Runoff is generated when surface or soil storage capacities are exceeded:

   - **Impervious surfaces** (paved, buildings): When surface water storage exceeds the maximum storage capacity
   - **Pervious surfaces** (vegetation, bare soil): When soil moisture storage exceeds the soil storage capacity
   - **Water bodies**: When water level exceeds the defined state limit

   For pervious surfaces, the saturation excess is calculated as:

   .. math::

      R_\text{S} = \max(0, S - S_\text{max})

   where:

   - :math:`R_\text{S}` is the saturation excess runoff
   - :math:`S` is the current soil moisture storage
   - :math:`S_\text{max}` is the soil storage capacity

Timestep Considerations
^^^^^^^^^^^^^^^^^^^^^^^

All runoff calculations are performed at the model timestep (typically 5 minutes to 1 hour). The infiltration threshold is automatically adjusted based on the timestep duration to maintain consistency:

.. math::

   I_\text{th}(\Delta t) = \frac{I_\text{th,hourly}}{n}

where :math:`n` is the number of timesteps per hour.

Water Routing
^^^^^^^^^^^^^

After generation, runoff is routed according to the water distribution matrix (``WaterDist``), which specifies:

- Fractions going directly to runoff pipes
- Fractions redistributed between surfaces
- Fractions directed to soil storage (for pervious surfaces)
- Overflow routing to water bodies via the ``RunoffToWater`` parameter

The total runoff from a grid cell includes contributions from all surface types, subject to pipe capacity limitations.

Snowmelt
--------

**Module:** ``suews_phys_snow.f95``

The snowmelt model is described in :cite:t:`J14`.
Changes since v2016a:

1) previously all surface states could freeze in 1-h time step, now the freezing surface state is
   calculated similarly as melt water and can freeze within the snow pack.
2) Snowmelt-related coefficients can be configured in the snow parameters section of the YAML configuration.
