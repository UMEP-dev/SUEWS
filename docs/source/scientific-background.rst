.. _scientific_background:

Scientific Background
=====================

Overview
--------

The Surface Urban Energy and Water Balance Scheme (SUEWS) simulates the urban surface energy and water balances at neighbourhood to local scales. The model solves two coupled balance equations:

**Energy Balance:**

.. math::

   Q^* + Q_F = \Delta Q_S + Q_H + Q_E

where:

- :math:`Q^*` is the net all-wave radiation
- :math:`Q_F` is the anthropogenic heat flux
- :math:`\Delta Q_S` is the net storage heat flux
- :math:`Q_H` is the turbulent sensible heat flux
- :math:`Q_E` is the turbulent latent heat flux

**Water Balance:**

.. math::

   P + I = E + \Delta S + R

where:

- :math:`P` is precipitation
- :math:`I` is irrigation (optional)
- :math:`E` is evapotranspiration (related to :math:`Q_E`)
- :math:`\Delta S` is the change in water storage
- :math:`R` is runoff

Model Components
----------------

Each component can be calculated using different parameterisations or provided as observed input. The choice of parameterisation is controlled through the ``ModelPhysics`` configuration object. Some schemes are alternatives (e.g., NARP vs BEERS vs SPARTACUS for radiation), whilst others are complementary (e.g., different stability correction functions).

Supporting schemes provide essential inputs to these balance calculations, including leaf area index (LAI), atmospheric stability corrections, and diagnostic meteorological profiles.

Documentation Structure
-----------------------

The following pages provide detailed information on:

- :ref:`energy_balance`: Components of the surface energy balance (:math:`Q^*`, :math:`Q_F`, :math:`\Delta Q_S`, :math:`Q_H`, :math:`Q_E`)
- :ref:`water_balance`: Components of the surface water balance (:math:`P`, :math:`I`, :math:`E`, :math:`\Delta S`, :math:`R`)
- :ref:`supporting_processes`: Supporting processes and schemes (LAI, stability, RSL, CBL, CO2)
- :ref:`suews_publications`: Core SUEWS publications from the development team
- :ref:`refs`: General scientific references
