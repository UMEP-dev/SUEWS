.. _anthropogenic_observed:

Observed Anthropogenic Heat
============================

Overview
--------

The observed method (`Emissions Method = 0`) bypasses internal :math:`Q_F` calculations and uses pre-calculated anthropogenic heat flux values provided in the meteorological forcing file. This allows users to supply :math:`Q_F` estimates from external sources such as detailed inventory models, observations, or other parameterisations.

**Module:** ``suews_phys_anthro.f95``

Physical Basis
--------------

This method does not perform any calculations - it simply reads and uses the provided :math:`Q_F` values directly. The physical basis depends entirely on the source of the input data, which may come from:

**Inventory Models:**

- `LUCY <http://umep-docs.readthedocs.io/en/latest/OtherManuals/LQF_Manual.html>`_ (Large-scale Urban Consumption of energY) - Combines population density, per-capita energy use, vehicle activity, and industrial energy consumption
- `GreaterQF <http://umep-docs.readthedocs.io/en/latest/OtherManuals/GQF_Manual.html>`_ - High-resolution bottom-up inventory for Greater London
- Custom inventory models specific to the study area

**Observational Estimates:**

- Residual method: :math:`Q_F = Q^* - \Delta Q_S - Q_H - Q_E` (calculated from energy balance closure)
- Eddy covariance storage flux measurements (requires careful interpretation)
- Building energy meter data aggregated to grid scale

**Other Parameterisations:**

- Results from other models for comparison or sensitivity studies
- Climatological values from long-term inventories

Implementation Details
----------------------

When ``emissions_method = 0``, the model:

1. Reads the ``qf`` column from the meteorological forcing file at each time step
2. Uses this value directly without modification
3. Skips all internal :math:`Q_F` calculations (L11, J11, J19 methods)
4. Does not calculate component separation (building/traffic/metabolism)

**Important:** If anthropogenic CO₂ emissions are required, they will not be automatically calculated from the provided :math:`Q_F`. Users must either:

- Provide CO₂ fluxes separately (if supported)
- Use one of the parameterised methods (J11 or J19) that calculate both heat and CO₂

Applications
------------

The observed method is well-suited for:

- **Model validation studies** where :math:`Q_F` uncertainty should be eliminated to test other components
- **Comparison studies** evaluating different :math:`Q_F` estimation approaches against a reference
- **High-quality inventory sites** where detailed bottom-up estimates are more reliable than parameterisations
- **Operational forecasting** using pre-calculated :math:`Q_F` from established inventories
- **Sensitivity analyses** prescribing specific :math:`Q_F` scenarios

Configuration
-------------

To use observed :math:`Q_F` values, configure the model physics:

.. code-block:: yaml

   model_physics:
     emissions_method: 0  # Use observed QF from forcing file

**Required Forcing Data:**

The meteorological forcing file must include a ``qf`` column containing anthropogenic heat flux values:

.. list-table::
   :header-rows: 1

   * - Column
     - Unit
     - Description
   * - ``qf``
     - W m⁻²
     - Anthropogenic heat flux

**Example forcing file snippet:**

.. code-block:: text

   # datetime            kdown  temp   RH    press  qf    ...
   2020-01-01 00:00:00   0.0   5.2   85.3  1013.2  45.8  ...
   2020-01-01 01:00:00   0.0   4.8   87.1  1013.5  38.2  ...
   2020-01-01 02:00:00   0.0   4.5   88.5  1013.8  35.1  ...

If the ``qf`` column is missing or contains missing values, the model will use a default value (typically 0 W m⁻²) or raise an error depending on configuration.

See :ref:`forcing data configuration <forcing-data>` for detailed information on meteorological input files.

Computational Considerations
-----------------------------

**Advantages:**

- No computational overhead (no calculations performed)
- Maximum flexibility - any :math:`Q_F` estimate can be used
- Useful for isolating :math:`Q_F` uncertainty in model evaluation
- Allows use of high-quality site-specific inventories

**Considerations:**

- Requires external :math:`Q_F` estimation (additional preprocessing)
- No component separation (cannot distinguish building/traffic/metabolism contributions)
- No automatic CO₂ emission calculation
- Quality depends entirely on input data source
- May introduce inconsistencies if :math:`Q_F` source uses different assumptions than SUEWS

Comparison with Parameterised Methods
--------------------------------------

**Observed vs. L11/J11/J19:**

- **Observed:** Uses external estimates, no internal calculations, maximum flexibility
- **L11/J11/J19:** Internal parameterisations, require fewer inputs, provide component separation (J19), calculate CO₂ (J19)

**When to use Observed instead of parameterisations:**

- When high-quality inventory data exist for the specific site
- For model intercomparison studies (eliminate :math:`Q_F` as a variable)
- When testing new :math:`Q_F` parameterisations against observations
- In operational systems with established inventory workflows

**When to use parameterisations instead of Observed:**

- When inventory data are unavailable or outdated
- For future climate scenarios (inventories cannot predict future :math:`Q_F`)
- When component-level understanding is needed (requires J19)
- For sites without detailed energy/traffic statistics

Limitations
-----------

The observed method has the following limitations:

- Requires external :math:`Q_F` data source (additional workflow step)
- No automatic adjustment to meteorological conditions (fixed temporal pattern)
- Cannot respond to temperature changes unless external source already accounts for this
- No built-in quality control (errors in input data propagate directly)
- Component separation not available (single bulk :math:`Q_F` value)
- CO₂ emissions not calculated automatically

For applications requiring dynamic :math:`Q_F` response to weather, consider :ref:`L11 <anthropogenic_l11>`, :ref:`J11 <anthropogenic_j11>`, or :ref:`J19 <anthropogenic_j19>` methods.

Best Practices
--------------

**Data Quality:**

- Verify temporal resolution matches model time step
- Check for missing values and decide on gap-filling strategy
- Ensure units are W m⁻² (convert if necessary)
- Validate against expected ranges (typically 10-300 W m⁻² for urban areas)

**Source Documentation:**

- Document the source and methodology of :math:`Q_F` estimates
- Note any assumptions that may differ from SUEWS (e.g., surface area definition)
- Record temporal/spatial resolution of original estimates
- Keep metadata on inventory year, emission factors, activity data sources

**Model Consistency:**

- Verify that :math:`Q_F` spatial extent matches grid cell definition
- Check that temporal patterns are consistent with other forcing data
- Consider whether :math:`Q_F` should vary with observed temperature or remain fixed

References
----------

For information on external :math:`Q_F` calculation methods:

- LUCY model: http://umep-docs.readthedocs.io/en/latest/OtherManuals/LQF_Manual.html
- GreaterQF: http://umep-docs.readthedocs.io/en/latest/OtherManuals/GQF_Manual.html
- Review of inventory methods: :cite:t:`S15` (if available in bibliography)

For internal parameterisation alternatives:

- :ref:`L11 method <anthropogenic_l11>` - Temperature-based parameterisation
- :ref:`J11 method <anthropogenic_j11>` - Degree day parameterisation
- :ref:`J19 method <anthropogenic_j19>` - Component-based parameterisation
