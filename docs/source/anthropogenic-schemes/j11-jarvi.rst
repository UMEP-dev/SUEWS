.. _anthropogenic_j11:

J11 (Järvi et al., 2011)
========================

Overview
--------

The **J11 method** :cite:`J11` (SAHP_2) extends the simple temperature-based approach by using **heating degree days (HDD)** and **cooling degree days (CDD)** to parameterise both heating and cooling contributions to anthropogenic heat flux. This method explicitly accounts for air conditioning in addition to heating, making it suitable for climates with significant seasonal variations in both heating and cooling demand.

**Module:** ``suews_phys_anthro.f95``

Physical Basis
--------------

The J11 method recognises that anthropogenic heat flux varies with both heating demand (winter) and cooling demand (summer). The parameterisation uses degree days to capture cumulative temperature effects:

**Key Assumptions:**

- Heating demand accumulates below a base temperature (typically 15-18°C)
- Cooling demand accumulates above a base temperature (typically 18-21°C)
- Degree days provide a better proxy for energy demand than instantaneous temperature
- The relationship between degree days and :math:`Q_F` is approximately linear

**Governing Equation:**

.. math::

   Q_F = (a + b \cdot CDD + c \cdot HDD) \times f_{profile} \times \rho_{pop}

where:

- :math:`Q_F` is anthropogenic heat flux [W m⁻²]
- :math:`a` is baseline heat flux coefficient [W m⁻² (capita ha⁻¹)⁻¹]
- :math:`b` is cooling coefficient [W m⁻² (capita ha⁻¹)⁻¹ (degree day)⁻¹]
- :math:`c` is heating coefficient [W m⁻² (capita ha⁻¹)⁻¹ (degree day)⁻¹]
- :math:`CDD` is cooling degree days [K·day]
- :math:`HDD` is heating degree days [K·day]
- :math:`f_{profile}` is diurnal/weekly profile factor
- :math:`\rho_{pop}` is population density [capita ha⁻¹]

**Degree Day Calculation:**

Degree days are calculated cumulatively over a moving window:

- **Heating Degree Days (HDD):**

  .. math::

     HDD = \sum_{i=1}^{n} \max(T_{base,heat} - T_{mean,i}, 0)

- **Cooling Degree Days (CDD):**

  .. math::

     CDD = \sum_{i=1}^{n} \max(T_{mean,i} - T_{base,cool}, 0)

where :math:`T_{mean,i}` is the daily mean temperature for day :math:`i`, and :math:`n` is the number of days in the accumulation window (typically 5-30 days).

**Component Breakdown:**

The total flux can be decomposed into:

- :math:`Q_F^{base} = a \times f_{profile} \times \rho_{pop}` - Temperature-independent (traffic, metabolism, appliances)
- :math:`Q_F^{heating} = c \cdot HDD \times f_{profile} \times \rho_{pop}` - Heating contribution
- :math:`Q_F^{cooling} = b \cdot CDD \times f_{profile} \times \rho_{pop}` - Cooling (air conditioning) contribution

Implementation Details
----------------------

**Method Code:** ``EmissionsMethod = 2``

The implementation follows these steps:

1. **Calculate degree days:**

   - Track daily mean temperature over moving window
   - Accumulate HDD and CDD based on base temperatures
   - Update degree day values each day

2. **Determine weekday/weekend:**

   - Weekday (Monday-Friday): Use weekday coefficients
   - Weekend (Saturday-Sunday): Use weekend coefficients

3. **Calculate flux components:**

   - Base: :math:`Q_F^{base} = a_{wd/we} \times \rho_{pop}`
   - Heating: :math:`Q_F^{heat} = c_{wd/we} \times HDD \times \rho_{pop}`
   - Cooling: :math:`Q_F^{cool} = b_{wd/we} \times CDD \times \rho_{pop}`

4. **Apply diurnal profile:**

   - :math:`Q_F = (Q_F^{base} + Q_F^{heat} + Q_F^{cool}) \times f_{profile}(hour, daytype)`

**Key Parameters:**

.. list-table::
   :header-rows: 1
   :widths: 30 15 15 40

   * - Parameter
     - Unit
     - Typical Range
     - Description
   * - ``qf_a`` [weekday, weekend]
     - W m⁻² (cap ha⁻¹)⁻¹
     - 0.3-1.5
     - Baseline coefficient
   * - ``qf_b`` [weekday, weekend]
     - W m⁻² (cap ha⁻¹)⁻¹ (DD)⁻¹
     - 0.005-0.03
     - Cooling coefficient
   * - ``qf_c`` [weekday, weekend]
     - W m⁻² (cap ha⁻¹)⁻¹ (DD)⁻¹
     - 0.01-0.08
     - Heating coefficient
   * - ``baset_heating`` [weekday, weekend]
     - °C
     - 12-18
     - Base temperature for HDD
   * - ``baset_cooling`` [weekday, weekend]
     - °C
     - 18-24
     - Base temperature for CDD
   * - ``popdens_daytime`` [weekday, weekend]
     - capita ha⁻¹
     - 50-500
     - Daytime population density
   * - ``popdens_nighttime``
     - capita ha⁻¹
     - 30-300
     - Night-time (residential) population density

Applications
------------

The J11 method is well-suited for:

- **Climates with both heating and cooling seasons** (mid-latitude cities, continental climates)
- **Studies spanning multiple seasons** where both heating and cooling are important
- **Sites with population density data** and activity patterns
- **Comparative urban climate studies** across different city types
- **Climate change impact assessments** where shifting heating/cooling balance matters

The method has been successfully applied in:

- Los Angeles and Vancouver (original J11 paper)
- Helsinki (Nordic climate with heating dominance)
- Mid-latitude cities in Europe, North America, East Asia
- Multi-year simulations capturing seasonal cycles

Configuration
-------------

To use the J11 method, configure the model physics and anthropogenic emissions parameters:

.. code-block:: yaml

   model_physics:
     emissions_method: 2  # J11 method

   anthropogenic_emissions:
     heat:
       qf_a: [0.8, 0.6]                  # [weekday, weekend] W m-2 (cap ha-1)-1
       qf_b: [0.015, 0.012]              # Cooling coeff [weekday, weekend]
       qf_c: [0.045, 0.035]              # Heating coeff [weekday, weekend]
       baset_heating: [15.0, 15.0]       # [weekday, weekend] °C
       baset_cooling: [21.0, 21.0]       # [weekday, weekend] °C
       popdens_daytime: [200.0, 150.0]   # [weekday, weekend] cap ha-1
       popdens_nighttime: 120.0          # cap ha-1
       ahprof_24hr:                      # Hourly profiles
         weekday: [0.7, 0.6, ...]        # 24 hourly values
         weekend: [0.8, 0.7, ...]        # 24 hourly values

**Parameter Calibration:**

Coefficients can be estimated using:

- **Energy statistics:** Regression of monthly energy consumption against HDD/CDD
- **Inventory data:** Fit to total annual :math:`Q_F` with seasonal breakdown
- **Transfer from similar cities:** Use published coefficients from climatically similar urban areas
- **Optimisation:** Calibrate against observed :math:`Q_F` using least-squares or ensemble methods

See :ref:`AnthropogenicEmissions <anthropogenicemissions>` for detailed parameter specifications.

Computational Considerations
-----------------------------

**Advantages:**

- **Explicit cooling representation:** Air conditioning heat flux explicitly modelled
- **Seasonal adaptation:** Degree days capture cumulative seasonal effects better than instantaneous temperature
- **Weekday/weekend differentiation:** Separate coefficients capture activity differences
- **Physical basis:** Degree days widely used in building energy analysis

**Considerations:**

- More parameters than L11 (6 coefficients vs. 3)
- Requires daily temperature tracking for degree day calculation
- Population density must be specified
- Profiles must be calibrated or obtained from local data

Advantages Over L11
--------------------

The J11 method improves on :ref:`L11 <anthropogenic_l11>` by:

1. **Cooling representation:** Explicitly models air conditioning contribution via CDD
2. **Seasonal memory:** Degree days accumulate over days, capturing sustained cold/hot periods better than instantaneous temperature
3. **Weekday/weekend in coefficients:** Activity differences reflected in both profiles and base coefficients
4. **Better warm-season performance:** L11 cannot represent summer cooling demand

Example comparison:

- **Winter (HDD=200, CDD=0):** Both methods represent heating demand
- **Summer (HDD=0, CDD=150):** J11 captures AC contribution, L11 shows only baseline
- **Shoulder seasons (HDD=50, CDD=30):** J11 captures transition, L11 may underestimate

Limitations
-----------

The J11 method has the following limitations:

- **No component separation:** Cannot distinguish building, traffic, and metabolism (all combined in coefficients)
- **Linear degree day relationship:** Real energy demand may have non-linearities (e.g., efficiency improvements, behavioural responses)
- **Population density required:** Must specify daytime and night-time population, which may be uncertain
- **Profile dependency:** Diurnal patterns must be prescribed, cannot respond to unusual events
- **No automatic CO₂ calculation:** Can estimate CO₂ from :math:`Q_F` using emission factors, but not component-based like J19

For applications requiring component separation or CO₂ emissions, consider :ref:`J19 <anthropogenic_j19>`.

Parameter Estimation Example
-----------------------------

**Example calibration for a mid-latitude city with heating and cooling:**

1. **Population density:**

   - Residential (night): 120 capita ha⁻¹
   - Commercial district (weekday day): 300 capita ha⁻¹
   - Average weekday: 200 capita ha⁻¹

2. **Base temperatures (from energy data):**

   - Heating base: 15°C (heating systems activate below this)
   - Cooling base: 21°C (AC systems activate above this)

3. **Coefficients from regression:**

   Given monthly energy data and HDD/CDD values, perform regression:

   .. math::

      Q_F^{monthly} = a + b \cdot CDD_{monthly} + c \cdot HDD_{monthly}

   Example results:

   - :math:`a` = 0.8 W m⁻² (cap ha⁻¹)⁻¹ (weekday), 0.6 (weekend)
   - :math:`b` = 0.015 W m⁻² (cap ha⁻¹)⁻¹ DD⁻¹
   - :math:`c` = 0.045 W m⁻² (cap ha⁻¹)⁻¹ DD⁻¹

4. **Validation:**

   - Check winter peak: HDD=300, CDD=0, pop=200 → :math:`Q_F` ~ 160 + 2700 = 2860 W m⁻² (unrealistic - coefficients need adjustment)
   - Ensure summer peak: HDD=0, CDD=200, pop=200 → :math:`Q_F` ~ 160 + 600 = 760 W m⁻² (reasonable for intense AC use)

Comparison with Other Methods
------------------------------

**J11 vs. L11:**

- **J11:** Degree days (HDD+CDD), separate cooling, more parameters, better seasonal performance
- **L11:** Instantaneous temperature, heating only, simpler, suitable for heating-dominated sites

**J11 vs. J19:**

- **J11:** Bulk parameterisation, degree day-based, moderate complexity
- **J19:** Component-based (building/traffic/metabolism), detailed inputs, calculates CO₂

**When to use J11:**

- Sites with significant both heating and cooling demand
- Seasonal studies where cumulative temperature effects matter
- When population data available but detailed energy/traffic statistics are not

**When to use alternatives:**

- **L11:** Heating-only climates, data-limited exploratory studies
- **J19:** Component-level understanding needed, policy analysis, CO₂ emissions required

References
----------

Key publications:

- :cite:t:`J11` - Original J11 formulation, SAHP_2 method, SUEWS evaluation
- :cite:t:`L11` - Earlier L11 method for comparison

For alternative methods:

- :ref:`L11 method <anthropogenic_l11>` - Simpler temperature-based approach
- :ref:`J19 method <anthropogenic_j19>` - Advanced component-based approach
