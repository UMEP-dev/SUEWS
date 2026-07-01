.. _anthropogenic_j19:

J19 (Järvi et al., 2019)
========================

Overview
--------

The **J19 method** :cite:`J19` provides a component-based parameterisation of anthropogenic heat flux and CO₂ emissions, separating contributions from **buildings**, **traffic**, and **human metabolism**. This detailed approach uses local energy consumption data, traffic statistics, and population demographics to calculate both :math:`Q_F` and anthropogenic CO₂ flux (:math:`F_c^{anthro}`), making it suitable for urban planning and policy analysis.

**Module:** ``suews_phys_anthro.f95``

Physical Basis
--------------

The J19 method explicitly models the three main sources of anthropogenic emissions in cities:

**Total Fluxes:**

.. math::

   Q_F = Q_F^{buildings} + Q_F^{traffic} + Q_F^{metabolism}

.. math::

   F_c^{anthro} = F_c^{buildings} + F_c^{traffic} + F_c^{metabolism} + F_c^{point}

where each component is calculated independently using local statistics and emission factors.

Building Component
^^^^^^^^^^^^^^^^^^

Building energy use is parameterised using degree days (similar to J11) but with explicit separation of heating and cooling:

.. math::

   Q_F^{buildings} = (a \cdot f_{profile,pop} + b \cdot CDD + c \cdot HDD) \times \rho_{pop}

- :math:`a` - Baseline building energy use (appliances, lighting, hot water)
- :math:`b` - Cooling coefficient (electricity for air conditioning)
- :math:`c` - Heating coefficient (fossil fuel and/or electricity)

CO₂ emissions from buildings depend on fuel mix:

.. math::

   F_c^{buildings} = (Q_F^{heating} \times f_{heat,fossil} + Q_F^{base} \times f_{base,fossil}) \times EF_{CO_2}

- :math:`f_{heat,fossil}` - Fraction of heating from fossil fuels (vs. electricity)
- :math:`f_{base,fossil}` - Fraction of baseline consumption from fossil fuels
- :math:`EF_{CO_2}` - CO₂ emission factor [μmol J⁻¹]

**Note:** Cooling (AC) is assumed to be entirely electric, so produces no **local** CO₂ emissions (emissions occur at power plant).

Traffic Component
^^^^^^^^^^^^^^^^^

Traffic emissions are calculated using vehicle activity data and emission factors:

**Option 1: Traffic rate per unit area**

.. math::

   Q_F^{traffic} = \frac{TR \times EF_{energy}}{86400} \times f_{profile,traffic}

   F_c^{traffic} = \frac{TR \times EF_{CO_2}}{86400} \times f_{profile,traffic}

- :math:`TR` - Traffic rate [vehicle-km m⁻² day⁻¹]
- :math:`EF_{energy}` - Energy emission factor [J km⁻¹ vehicle⁻¹]
- :math:`EF_{CO_2}` - CO₂ emission factor [kg km⁻¹ vehicle⁻¹]

**Option 2: Traffic rate per capita**

.. math::

   Q_F^{traffic} = \frac{TR_{capita} \times EF_{energy}}{86400} \times f_{profile,traffic} \times \rho_{pop}

- :math:`TR_{capita}` - Traffic rate per person [vehicle-km capita⁻¹ day⁻¹]

Metabolism Component
^^^^^^^^^^^^^^^^^^^^

Human metabolism releases both heat and CO₂ based on activity level and population:

.. math::

   Q_F^{metabolism} = \left[\rho_{night} \times QF_{min} \times (2-a_{act})/2 + \rho_{day} \times QF_{max} \times (a_{act}-1)/2\right] / 10000

   F_c^{metabolism} = \left[\rho_{night} \times FC_{min} \times (2-a_{act})/2 + \rho_{day} \times FC_{max} \times (a_{act}-1)/2\right] / 10000

- :math:`\rho_{night}` - Night-time population density [capita ha⁻¹]
- :math:`\rho_{day}` - Daytime population density [capita ha⁻¹]
- :math:`QF_{min}` - Minimum metabolic heat (sleeping) [W capita⁻¹] ≈ 70-90 W
- :math:`QF_{max}` - Maximum metabolic heat (active) [W capita⁻¹] ≈ 140-180 W
- :math:`FC_{min}` - Minimum metabolic CO₂ (sleeping) [μmol capita⁻¹ s⁻¹]
- :math:`FC_{max}` - Maximum metabolic CO₂ (active) [μmol capita⁻¹ s⁻¹]
- :math:`a_{act}` - Activity profile (1=sleeping, 2=fully active)

Implementation Details
----------------------

**Method Code:** ``EmissionsMethod = 4, 5, or 6`` (variants)

The implementation separates calculations for each component:

1. **Calculate building emissions:**

   - Use degree days (HDD/CDD) for temperature dependency
   - Apply population profile for occupancy variation
   - Split between fossil and electric based on fuel mix fractions

2. **Calculate traffic emissions:**

   - Use traffic rate (per area or per capita)
   - Apply traffic profile for diurnal variation
   - Convert vehicle-km to energy and CO₂ using emission factors

3. **Calculate metabolic emissions:**

   - Use population density (day/night values)
   - Interpolate between min/max based on activity profile
   - Apply both population and activity profiles

4. **Sum components:**

   - :math:`Q_F = Q_F^{buildings} + Q_F^{traffic} + Q_F^{metabolism}`
   - :math:`F_c^{anthro} = F_c^{buildings} + F_c^{traffic} + F_c^{metabolism} + F_c^{point}`

**Key Parameters:**

.. list-table::
   :header-rows: 1
   :widths: 35 15 50

   * - Parameter Group
     - Unit
     - Description
   * - **Building Coefficients**
     -
     -
   * - ``qf_a`` [wd, we]
     - W m⁻² (cap ha⁻¹)⁻¹
     - Baseline building energy
   * - ``qf_b`` [wd, we]
     - W m⁻² (cap ha⁻¹)⁻¹ DD⁻¹
     - Cooling coefficient
   * - ``qf_c`` [wd, we]
     - W m⁻² (cap ha⁻¹)⁻¹ DD⁻¹
     - Heating coefficient
   * - ``qf0_beu`` [wd, we]
     - fraction
     - Fraction of baseline from buildings (vs. traffic)
   * - ``frfossilfuel_heat``
     - fraction
     - Fraction of heating from fossil fuels
   * - ``frfossilfuel_nonheat``
     - fraction
     - Fraction of baseline from fossil fuels
   * - **Traffic Parameters**
     -
     -
   * - ``trafficrate`` [wd, we]
     - veh-km m⁻² day⁻¹ or veh-km cap⁻¹ day⁻¹
     - Vehicle activity rate
   * - ``trafficunits``
     - 1 or 2
     - Units: 1=per area, 2=per capita
   * - ``enef_v_jkm``
     - J km⁻¹
     - Energy emission factor for vehicles
   * - ``fcef_v_kgkm`` [wd, we]
     - kg km⁻¹
     - CO₂ emission factor for vehicles
   * - **Metabolism Parameters**
     -
     -
   * - ``minqfmetab``
     - W capita⁻¹
     - Minimum metabolic heat (sleeping)
   * - ``maxqfmetab``
     - W capita⁻¹
     - Maximum metabolic heat (active)
   * - ``minfcmetab``
     - μmol capita⁻¹ s⁻¹
     - Minimum metabolic CO₂
   * - ``maxfcmetab``
     - μmol capita⁻¹ s⁻¹
     - Maximum metabolic CO₂
   * - **Population**
     -
     -
   * - ``popdens_daytime`` [wd, we]
     - capita ha⁻¹
     - Daytime population (workers)
   * - ``popdens_nighttime``
     - capita ha⁻¹
     - Night-time population (residents)
   * - **Profiles**
     -
     -
   * - ``ahprof_24hr``
     - dimensionless
     - Building/population activity profile
   * - ``traffprof_24hr``
     - dimensionless
     - Traffic diurnal profile
   * - ``humactivity_24hr``
     - 1-2
     - Human activity level profile
   * - ``popprof_24hr``
     - 1-2
     - Population presence profile

Applications
------------

The J19 method is designed for applications requiring component-level understanding:

- **Urban planning and policy analysis:** Evaluate impacts of building retrofit, transport electrification, land use changes
- **Emissions inventories:** Provide spatially and temporally resolved :math:`Q_F` and CO₂ estimates
- **Climate change mitigation:** Assess effectiveness of different intervention strategies
- **Component attribution studies:** Understand relative contributions of buildings, traffic, and metabolism
- **Co-benefits analysis:** Evaluate energy efficiency measures for both climate and air quality
- **Future scenario modelling:** Project :math:`Q_F` and CO₂ under different development pathways

The method has been successfully applied in:

- Helsinki (original J19 paper) - high-resolution (250 m) CO₂ modelling
- Cities with detailed energy statistics and mobility data
- Policy studies evaluating decarbonisation strategies

Configuration
-------------

To use the J19 method, configure detailed anthropogenic emissions parameters:

.. code-block:: yaml

   model_physics:
     emissions_method: 4  # J19 method (basic version)
                          # 5 = J19 with updated building calc
                          # 6 = J19 with all components updated

   anthropogenic_emissions:
     heat:
       # Building parameters
       qf_a: [0.8, 0.6]
       qf_b: [0.015, 0.012]
       qf_c: [0.045, 0.035]
       qf0_beu: [0.7, 0.6]  # Fraction of base QF from buildings
       frfossilfuel_heat: 0.85       # 85% heating from fossil fuels
       frfossilfuel_nonheat: 0.40    # 40% baseline from fossil fuels

       # Traffic parameters
       trafficrate: [1.5, 0.8]       # [wd, we] veh-km m-2 day-1
       trafficunits: 1               # 1 = per area
       enef_v_jkm: 2.5e6             # J km-1 (vehicle energy consumption)

       # Metabolism parameters
       minqfmetab: 75.0              # W cap-1 (sleeping)
       maxqfmetab: 175.0             # W cap-1 (active)
       minfcmetab: 0.9               # umol cap-1 s-1
       maxfcmetab: 2.2               # umol cap-1 s-1

       # Population
       popdens_daytime: [250.0, 150.0]  # [wd, we] cap ha-1
       popdens_nighttime: 120.0         # cap ha-1

       # Profiles (24 hourly values for wd and we)
       ahprof_24hr: ...
       traffprof_24hr: ...
       humactivity_24hr: ...
       popprof_24hr: ...

     co2:
       fcef_v_kgkm: [0.12, 0.12]     # [wd, we] kg-CO2 km-1
       ef_umolco2perj: 0.018          # umol-CO2 J-1 (fuel emission factor)
       co2pointsource: 0.0            # kg-C day-1 (if any large point sources)

See :ref:`AnthropogenicEmissions <anthropogenicemissions>` for detailed parameter specifications.

Data Requirements
-----------------

The J19 method requires substantial input data:

**Energy Statistics:**

- Monthly/annual building energy consumption (heating and electricity separately)
- Fuel mix for heating (fossil vs. electric)
- Typical emission factors for local fuel types

**Transportation Data:**

- Vehicle kilometres travelled (from traffic counts or mobility surveys)
- Fleet composition and emission factors
- Diurnal traffic patterns (hourly counts)

**Demographics:**

- Residential population density
- Daytime population (workers/students)
- Activity patterns (from time-use surveys)

**Typical Data Sources:**

- Municipal energy utilities (aggregated consumption data)
- National/regional transport statistics
- Census data and employment statistics
- Mobility surveys and GPS tracking studies
- Energy certification databases (building performance)

Computational Considerations
-----------------------------

**Advantages:**

- **Component separation:** Distinguish building, traffic, and metabolism contributions
- **Policy relevance:** Evaluate specific interventions (e.g., building efficiency, EV adoption)
- **CO₂ co-calculation:** Simultaneous :math:`Q_F` and CO₂ estimation with consistent assumptions
- **Flexibility:** Each component can be updated independently as data improve
- **Physical transparency:** Parameters have clear physical meaning

**Considerations:**

- High data requirements (energy, traffic, population statistics)
- More parameters to calibrate (~15-20 vs. 3-6 for L11/J11)
- Requires detailed diurnal/weekly profiles for each component
- Sensitivity to input uncertainties propagates through components
- More complex configuration and validation

Limitations
-----------

The J19 method has the following limitations:

- **Data intensive:** Requires detailed local statistics not available for all cities
- **Parameterisation complexity:** Many parameters may be poorly constrained
- **Emission factor uncertainty:** Fuel mix and vehicle fleet composition may change over time
- **Spatial aggregation:** Component contributions may vary within grid cell
- **Temporal resolution:** Daily degree days may not capture sub-daily variations well
- **Validation challenges:** Component-level validation requires separate observations of buildings/traffic/metabolism

For data-limited sites or exploratory studies, consider simpler methods (:ref:`L11 <anthropogenic_l11>` or :ref:`J11 <anthropogenic_j11>`).

Advantages Over J11
--------------------

The J19 method extends J11 by:

1. **Component separation:** Buildings, traffic, and metabolism calculated separately
2. **Policy applications:** Can evaluate building retrofit, transport mode shift, population changes independently
3. **CO₂ co-calculation:** Consistent :math:`Q_F` and CO₂ with fuel-specific emission factors
4. **Flexibility:** Update individual components as local data become available
5. **Physical transparency:** Parameters linked to measurable quantities (vehicle-km, energy consumption, metabolic rates)

**Example policy scenarios enabled by J19:**

- Building electrification: Reduce ``frfossilfuel_heat`` → Lower CO₂, slightly higher :math:`Q_F` (heat pump efficiency)
- EV adoption: Reduce ``fcef_v_kgkm`` → Lower CO₂, same :math:`Q_F` (electric vehicles still release heat)
- Increased cycling: Reduce ``trafficrate`` → Lower both :math:`Q_F` and CO₂ from traffic
- Building efficiency: Reduce ``qf_c`` (heating coefficient) → Lower both :math:`Q_F` and CO₂ in winter

Comparison with Other Methods
------------------------------

**J19 vs. L11:**

- **J19:** Component-based, CO₂ co-calculation, detailed inputs, policy-relevant
- **L11:** Bulk temperature-based, minimal inputs, simple, exploratory

**J19 vs. J11:**

- **J19:** Separate buildings/traffic/metabolism, CO₂ emissions, high detail
- **J11:** Bulk degree days, moderate complexity, seasonal variation

**When to use J19:**

- Component-level understanding needed (e.g., building vs. traffic contributions)
- Policy/scenario analysis (e.g., decarbonisation pathways)
- CO₂ emissions required alongside :math:`Q_F`
- Detailed local data available (energy, traffic, population)

**When to use simpler alternatives:**

- **L11/J11:** Data-limited sites, exploratory studies, sensitivity analyses
- **Observed:** High-quality inventory already exists, no need for parameterisation

References
----------

Key publications:

- :cite:t:`J19` - Original J19 formulation with Helsinki application
- :cite:t:`J11` - Earlier degree day method (SAHP_2)
- :cite:t:`L11` - Simple temperature-based method (SAHP)

For alternative methods:

- :ref:`L11 method <anthropogenic_l11>` - Simple temperature-based
- :ref:`J11 method <anthropogenic_j11>` - Degree day with heating+cooling
- :ref:`Observed method <anthropogenic_observed>` - External inventory data
