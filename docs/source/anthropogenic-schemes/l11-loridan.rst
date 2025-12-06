.. _anthropogenic_l11:

L11 (Loridan et al., 2011)
==========================

Overview
--------

The **L11 method** :cite:`L11` provides a simple temperature-based parameterisation of anthropogenic heat flux (SAHP - Simple Anthropogenic Heat Parameterisation). It uses a linear piece-wise relationship with air temperature, where heat flux increases linearly below a base heating temperature. This method is computationally efficient and requires minimal input data.

**Module:** ``suews_phys_anthro.f95``

Physical Basis
--------------

The L11 method is based on the observation that anthropogenic heat flux in cities is primarily driven by building heating demand during cold periods. The parameterisation assumes:

**Key Assumptions:**

- Below a base temperature, heating demand increases linearly with decreasing temperature
- Above the base temperature, :math:`Q_F` remains at a baseline level (appliances, lighting, traffic)
- The relationship is linear rather than non-linear (simplification for computational efficiency)
- Cooling demand (air conditioning) is not explicitly represented

**Governing Equation:**

.. math::

   Q_F = \begin{cases}
   a_{min} + a_{slope}(T_{base} - T_{air}) & \text{if } T_{air} < T_{base} \\
   a_{min} & \text{otherwise}
   \end{cases}

where:

- :math:`Q_F` is anthropogenic heat flux [W m⁻²]
- :math:`a_{min}` is baseline heat flux (temperature-independent) [W m⁻²]
- :math:`a_{slope}` is heating slope [W m⁻² K⁻¹]
- :math:`T_{base}` is base heating temperature [°C]
- :math:`T_{air}` is air temperature [°C]

**Temporal Modulation:**

The calculated :math:`Q_F` is further modulated by:

- **Diurnal profile:** Hourly multipliers accounting for daily activity patterns
- **Weekly cycle:** Separate coefficients for weekdays and weekends

.. math::

   Q_F^{final} = Q_F \times f_{profile}(hour, daytype)

where :math:`f_{profile}` is the diurnal profile factor (normalised to average = 1).

Implementation Details
----------------------

**Method Code:** ``EmissionsMethod = 1``

The implementation follows these steps:

1. **Determine weekday/weekend:**

   - Weekday (Monday-Friday): Use weekday parameters
   - Weekend (Saturday-Sunday): Use weekend parameters

2. **Calculate temperature-dependent component:**

   - If :math:`T_{air} < T_{base}`: :math:`Q_F^{heating} = a_{slope}(T_{base} - T_{air})`
   - Otherwise: :math:`Q_F^{heating} = 0`

3. **Add baseline component:**

   - :math:`Q_F = a_{min} + Q_F^{heating}`

4. **Apply diurnal profile:**

   - :math:`Q_F^{final} = Q_F \times f_{profile}(hour, daytype)`

**Key Parameters:**

.. list-table::
   :header-rows: 1
   :widths: 30 15 15 40

   * - Parameter
     - Unit
     - Typical Range
     - Description
   * - ``ah_min`` [weekday, weekend]
     - W m⁻²
     - 10-50
     - Baseline (minimum) anthropogenic heat flux
   * - ``ah_slope_heating`` [weekday, weekend]
     - W m⁻² K⁻¹
     - 2-15
     - Slope of heating relationship
   * - ``baset_heating`` [weekday, weekend]
     - °C
     - 12-18
     - Base temperature for heating
   * - ``ahprof_24hr``
     - dimensionless
     - 0.5-2.0
     - Hourly profile multipliers (24 values × 2 day types)

**Scaling with Population:**

In the current implementation, :math:`Q_F` scales with population density through the profile:

.. math::

   f_{profile} = f_{hour} \times (\rho_{pop} / \rho_{ref})

where :math:`\rho_{pop}` is the actual population density and :math:`\rho_{ref}` is a reference density used during calibration.

Applications
------------

The L11 method is well-suited for:

- **Data-limited sites** where only basic meteorological forcing is available
- **Exploratory studies** requiring quick :math:`Q_F` estimates without extensive calibration
- **Sensitivity analyses** testing the impact of :math:`Q_F` on urban climate
- **High-latitude cities** where heating dominates and cooling is negligible
- **Long-term simulations** requiring computational efficiency

The method has been successfully applied in:

- European cities with significant heating demand (London, Helsinki, Vancouver)
- Winter period simulations where heating dominates
- Studies comparing different :math:`Q_F` parameterisations

Configuration
-------------

To use the L11 method, configure the model physics and anthropogenic emissions parameters:

.. code-block:: yaml

   model_physics:
     emissions_method: 1  # L11 method

   anthropogenic_emissions:
     heat:
       ah_min: [30.0, 25.0]              # [weekday, weekend] W m-2
       ah_slope_heating: [8.0, 6.5]      # [weekday, weekend] W m-2 K-1
       baset_heating: [15.0, 15.0]       # [weekday, weekend] °C
       ahprof_24hr:                      # Hourly profiles [hour, daytype]
         weekday: [0.7, 0.6, 0.6, 0.6, 0.7, 1.0, 1.4, 1.6, 1.4, 1.2, 1.1, 1.0,
                   1.0, 1.0, 1.0, 1.1, 1.2, 1.4, 1.5, 1.4, 1.2, 1.0, 0.9, 0.8]
         weekend: [0.8, 0.7, 0.7, 0.7, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.3,
                   1.3, 1.2, 1.2, 1.1, 1.1, 1.1, 1.2, 1.2, 1.1, 1.0, 0.9, 0.8]

**Parameter Calibration:**

Parameters can be calibrated using:

- **Inventory data:** Match total annual :math:`Q_F` to inventory estimates
- **Energy statistics:** Use heating degree days from energy consumption records
- **Observations:** Calibrate against eddy covariance residual :math:`Q_F` if available
- **Literature values:** Use published parameters for similar city types

See :ref:`AnthropogenicEmissions <anthropogenicemissions>` for detailed parameter specifications.

Computational Considerations
-----------------------------

**Advantages:**

- **Minimal data requirements:** Only air temperature needed (already in forcing data)
- **Computationally efficient:** Simple linear calculation
- **Physically interpretable:** Clear relationship with heating demand
- **Easy to calibrate:** Only 2-3 key parameters per day type

**Considerations:**

- Does not represent cooling demand (no air conditioning)
- Assumes linear temperature relationship (may oversimplify at extreme temperatures)
- Cannot separate building, traffic, and metabolism components
- Weekday/weekend differences only through profiles (not temperature sensitivity)

Limitations
-----------

The L11 method has the following limitations:

- **No cooling representation:** Air conditioning heat release not modelled (problematic for hot climates or summer periods)
- **Linear assumption:** Real heating demand may be non-linear (e.g., thermostat setpoints, building thermal inertia)
- **No component separation:** Cannot distinguish building, traffic, and metabolic contributions
- **Limited seasonal variation:** Only temperature-dependent, does not capture other seasonal factors (daylight hours, school holidays)
- **Population scaling unclear:** Relationship between :math:`Q_F` and population density must be pre-defined in profiles

For applications requiring cooling representation or component separation, consider :ref:`J11 <anthropogenic_j11>` or :ref:`J19 <anthropogenic_j19>` methods.

Parameter Estimation Example
-----------------------------

**Example calibration for a mid-latitude city:**

1. **Baseline flux (``ah_min``):**

   From summer observations when heating is off:

   - Weekday: 30 W m⁻² (traffic + metabolism + appliances)
   - Weekend: 25 W m⁻² (reduced traffic)

2. **Base temperature (``baset_heating``):**

   From heating degree day analysis or thermostat surveys:

   - Typical values: 15-18°C for mild climates, 18-20°C for cold climates

3. **Heating slope (``ah_slope_heating``):**

   From winter :math:`Q_F` observations:

   - Example: If :math:`Q_F` = 90 W m⁻² at 0°C and baseline = 30 W m⁻²:
   - :math:`a_{slope} = (90 - 30) / (15 - 0) = 4` W m⁻² K⁻¹

4. **Diurnal profiles:**

   From hourly traffic counts and energy meter data:

   - Morning peak: 1.4-1.6× average (commuting)
   - Night minimum: 0.6-0.7× average (reduced activity)
   - Weekend: Flatter profile (less pronounced peaks)

Comparison with Other Methods
------------------------------

**L11 vs. J11:**

- **L11:** Uses instantaneous air temperature, heating only, simpler
- **J11:** Uses degree days (heating + cooling), more parameters, seasonal adaptation

**L11 vs. J19:**

- **L11:** Bulk parameterisation, temperature-based, computationally efficient
- **J19:** Component-based (building/traffic/metabolism), requires detailed inputs, calculates CO₂

**When to use L11:**

- Heating-dominated climates where cooling is negligible
- Exploratory studies or sensitivity analyses
- Data-limited sites without energy/traffic statistics

**When to use alternatives:**

- **J11:** Sites with significant cooling demand, better seasonal representation desired
- **J19:** Component-level understanding needed, policy studies, CO₂ emissions required

References
----------

Key publications:

- :cite:t:`L11` - Original L11 formulation and LUMPS longwave parameterisation
- :cite:t:`J11` - SUEWS evaluation including comparison of :math:`Q_F` methods

For alternative methods:

- :ref:`J11 method <anthropogenic_j11>` - Degree day approach with cooling
- :ref:`J19 method <anthropogenic_j19>` - Component-based detailed approach
