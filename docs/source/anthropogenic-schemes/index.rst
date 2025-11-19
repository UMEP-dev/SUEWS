.. _anthropogenic_schemes:

Anthropogenic Heat Schemes
===========================

Overview
--------

Anthropogenic heat flux (:math:`Q_F`) represents heat released by human activities, including building energy consumption (heating and cooling), transportation, and human metabolism. This component significantly affects the urban energy balance, particularly in dense urban areas.

Physical Context
----------------

Anthropogenic heat flux is one of the key differences between urban and rural energy balances. In densely populated urban areas, :math:`Q_F` can reach 100-300 W m⁻² during winter in high-latitude cities, rivalling or exceeding net radiation during cold periods.

The total anthropogenic heat flux can be decomposed into three main components:

.. math::

   Q_F = Q_F^{buildings} + Q_F^{traffic} + Q_F^{metabolism}

where:

- :math:`Q_F^{buildings}` - Heat from building energy use (heating, cooling, appliances, lighting)
- :math:`Q_F^{traffic}` - Heat from vehicle combustion and road surface heating
- :math:`Q_F^{metabolism}` - Metabolic heat from human respiration and activity

Each component varies with:

- **Temporal patterns:** Diurnal cycles (commuting peaks, evening heating), weekly cycles (weekday vs. weekend), and seasonal cycles (heating vs. cooling demand)
- **Temperature dependence:** Heating demand increases with decreasing temperature, cooling demand increases with increasing temperature
- **Socioeconomic factors:** Population density, economic activity, transportation mode, building thermal performance

Available Schemes
-----------------

SUEWS provides four methods for calculating or prescribing anthropogenic heat flux, ranging from simple observed inputs to component-based parameterisations.

Observed Method
^^^^^^^^^^^^^^^

**Complexity:** None | **Data requirement:** High | **Flexibility:** None

Use pre-calculated :math:`Q_F` values from external models or observations. This bypasses the parameterisation and directly uses provided values.

**Best for:**

- Sites with available :math:`Q_F` observations or high-quality inventory data
- Model validation studies where :math:`Q_F` is known
- Comparing different :math:`Q_F` estimation approaches

:doc:`Read more about Observed Method → <observed>`

L11 (Loridan et al., 2011)
^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Complexity:** Low | **Data requirement:** Low | **Flexibility:** Medium

A simple temperature-based parameterisation using linear relationships with air temperature. Heat flux increases linearly below a base heating temperature.

**Key equation:**

.. math::

   Q_F = \begin{cases}
   a + b(T_{base} - T_{air}) & \text{if } T_{air} < T_{base} \\
   a & \text{otherwise}
   \end{cases}

**Best for:**

- Sites with limited data availability (only requires temperature)
- Quick assessments and sensitivity studies
- Capturing basic temperature dependence without detailed inventories

:doc:`Read more about L11 Method → <l11-loridan>`

J11 (Järvi et al., 2011)
^^^^^^^^^^^^^^^^^^^^^^^^

**Complexity:** Medium | **Data requirement:** Medium | **Flexibility:** High

A degree-day-based parameterisation that explicitly accounts for both heating and cooling demand using heating degree days (HDD) and cooling degree days (CDD).

**Key equation:**

.. math::

   Q_F = (a + b \cdot CDD + c \cdot HDD) \times f_{profile} \times \rho_{pop}

**Best for:**

- Sites where seasonal heating/cooling patterns are important
- Applications requiring separation of heating and cooling contributions
- Studies with population density and activity pattern data

:doc:`Read more about J11 Method → <j11-jarvi>`

J19 (Järvi et al., 2019)
^^^^^^^^^^^^^^^^^^^^^^^^

**Complexity:** High | **Data requirement:** High | **Flexibility:** Very High

A component-based approach that separately models buildings, traffic, and metabolism using detailed local statistics on energy use, traffic rates, and population density.

**Component calculations:**

- Buildings: Energy consumption data with heating/cooling separation
- Traffic: Vehicle kilometres traveled with emission factors
- Metabolism: Population density with activity levels

**Best for:**

- Research applications requiring component-level understanding
- Policy studies evaluating specific interventions (e.g., electrification, modal shift)
- Sites with detailed energy/traffic/population data
- CO₂ emission modelling (J19 also calculates anthropogenic CO₂)

:doc:`Read more about J19 Method → <j19-jarvi>`

Scheme Comparison
-----------------

.. list-table:: Anthropogenic Heat Scheme Characteristics
   :header-rows: 1
   :widths: 20 20 20 20 20

   * - Characteristic
     - Observed
     - L11
     - J11
     - J19
   * - **Complexity**
     - None (prescribed)
     - Low (temperature-based)
     - Medium (degree days)
     - High (component-based)
   * - **Input data required**
     - Pre-calculated QF
     - Temperature, base temperature
     - Temperature, population, profiles
     - Energy use, traffic, population
   * - **Temperature dependence**
     - As provided
     - Linear (heating only)
     - Linear (heating + cooling)
     - Degree day-based (heating + cooling)
   * - **Temporal variation**
     - As provided
     - Profile-based
     - Profile + degree days
     - Multiple component profiles
   * - **Component separation**
     - No
     - No
     - No (bulk)
     - Yes (building/traffic/metabolism)
   * - **CO₂ emissions**
     - No
     - Optional (from QF)
     - Optional (from QF)
     - Yes (component-based)
   * - **Weekday/weekend**
     - As provided
     - Via profiles
     - Via profiles + coefficients
     - Via profiles
   * - **Calibration effort**
     - None (external)
     - Low (2-3 parameters)
     - Medium (3-6 parameters)
     - High (10+ parameters)
   * - **Typical applications**
     - Validation, comparison
     - Sensitivity studies, data-limited sites
     - Seasonal studies
     - Policy analysis, component studies

Selection Guidance
------------------

**Choose Observed if you:**

- Have high-quality :math:`Q_F` observations or inventory estimates
- Are validating other model components (want to eliminate :math:`Q_F` uncertainty)
- Are comparing :math:`Q_F` estimation methods

**Choose L11 if you:**

- Have minimal data (only meteorological forcing)
- Need a simple, robust parameterisation for exploratory studies
- Are primarily interested in winter heating patterns

**Choose J11 if you:**

- Have population density and diurnal/weekly activity profiles
- Need to represent both heating and cooling contributions
- Work in climates with significant seasonal variations

**Choose J19 if you:**

- Have detailed energy use, traffic, and population statistics
- Need to separate building, traffic, and metabolism components
- Are conducting policy studies (e.g., building retrofit, transport electrification)
- Require simultaneous CO₂ emission estimates

Configuration
-------------

Anthropogenic heat schemes are selected via the ``emissions_method`` parameter in model physics configuration:

.. code-block:: yaml

   model_physics:
     emissions_method: 1  # 0=Observed, 1=L11, 2=J11, 4=J19

See :ref:`ModelPhysics <modelphysics>` for complete configuration options and :ref:`AnthropogenicEmissions <anthropogenicemissions>` for detailed parameter specifications.

Further Reading
---------------

.. toctree::
   :maxdepth: 1

   observed
   l11-loridan
   j11-jarvi
   j19-jarvi

References
----------

Key anthropogenic heat scheme publications:

- :cite:t:`L11` - Temperature-based SAHP method
- :cite:t:`J11` - Degree day SAHP_2 method and SUEWS evaluation
- :cite:t:`J19` - Component-based method with CO₂ emissions
