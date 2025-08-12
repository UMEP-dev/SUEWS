.. _SUEWS_OHMCoefficients:

:nosearch:

SUEWS_OHMCoefficients.txt
~~~~~~~~~~~~~~~~~~~~~~~~~

OHM, the Objective Hysteresis Model  :cite:`G91`
calculates the storage heat flux as a function of net all-wave radiation
and surface characteristics.

-  For each surface, OHM requires three model coefficients (a1, a2, a3). The three should be selected as a set.
-  The **SUEWS_OHMCoefficients.txt** file provides these coefficients for each surface type.
-  A variety of values has been derived for different materials and can
   be found in the literature (see: `typical_values`).
-  Coefficients can be changed depending on:
    #. surface wetness state (wet/dry) based on the calculated surface wetness state and soil moisture.
    #. season (summer/winter) based on a 5-day running mean air temperature.
-  To use the same coefficients irrespective of wet/dry and
   summer/winter conditions, use the same code for all four OHM columns
   (`OHMCode_SummerWet`, `OHMCode_SummerDry`, `OHMCode_WinterWet` and
   `OHMCode_WinterDry`).


.. note::

    #. AnOHM (set in `RunControl.nml` by `StorageHeatMethod` = 3) does not use the coefficients specified in `SUEWS_OHMCoefficients.txt` but instead requires three parameters to be specified for each surface type (including snow): heat capacity (`AnOHM_Cp`), thermal conductivity (`AnOHM_Kk`) and bulk transfer coefficient (`AnOHM_Ch`). These are specified in `SUEWS_NonVeg.txt`, `SUEWS_Veg.txt`, `SUEWS_Water.txt` and `SUEWS_Snow.txt`. No additional files are required for AnOHM.

    #. AnOHM is under development in v2018b and should NOT be used!

.. DON'T manually modify the csv file below
.. as it is always automatically regenrated by each build:
.. edit the item descriptions in file `Input_Options.rst`

.. csv-table::
  :file: csv-table/SUEWS_OHMCoefficients.csv
  :header-rows: 1
  :widths: 5 25 5 65

.. only:: html

    An example `SUEWS_OHMCoefficients.txt` can be found below:

    .. literalinclude:: sample-table/SUEWS_OHMCoefficients.txt

.. only:: latex

    An example `SUEWS_OHMCoefficients.txt` can be found in the online version.

.. _ohm_custom_coefficients:

Advanced Example: Adding Custom OHM Coefficients
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This advanced example demonstrates how to derive and implement custom OHM coefficients for specialised urban surfaces.

**Use Case**: You have measurement data for a specific urban surface type (e.g., green roofs, solar panels, water features) and want to derive custom OHM coefficients for better storage heat flux representation.

**Step 1: Data Requirements**

To derive OHM coefficients, you need simultaneous measurements of:
- Net all-wave radiation (Q*)
- Storage heat flux (ΔQS) 
- Temporal coverage: At least one full annual cycle

**Step 2: Coefficient Derivation**

The OHM equation is: ΔQS = a₁ × Q* + a₂ × (∂Q*/∂t) + a₃

Where:
- a₁: Represents the fraction of net radiation contributing to storage
- a₂: Accounts for lag effects (phase shift) 
- a₃: Residual term for non-radiation influences

**Python Example using SuPy OHM utilities:**

.. code-block:: python

   import pandas as pd
   import supy as sp
   from supy.util import derive_ohm_coef, replace_ohm_coeffs, sim_ohm
   
   # Load your measured data (must have datetime index)
   df = pd.read_csv('surface_measurements.csv', index_col=0, parse_dates=True)
   
   # Ensure regular time frequency for proper derivative calculation
   df = df.asfreq('H')  # Hourly frequency
   
   # Extract required time series
   ser_QN = df['Q_star']  # Net all-wave radiation
   ser_QS = df['storage_heat_flux']  # Measured storage heat flux
   
   # Derive OHM coefficients using built-in SuPy function
   a1, a2, a3 = derive_ohm_coef(ser_QS, ser_QN)
   
   print(f"Derived OHM Coefficients:")
   print(f"a1 = {a1:.4f}  (fraction)")
   print(f"a2 = {a2:.4f}  (W m-2 / (W m-2 s-1))")  
   print(f"a3 = {a3:.4f}  (W m-2)")

**Step 3: Implementation in SUEWS**

**Option A: Using SuPy utilities (Recommended for single-surface updates):**

.. code-block:: python

   # Load initial model state 
   df_state_init = sp.init_supy('config.yml')  # or your config file
   
   # Update coefficients for specific land cover type
   # Available types: "Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water"
   df_state_updated = replace_ohm_coeffs(
       df_state_init, 
       coefs=(a1, a2, a3),  # coefficients from derive_ohm_coef
       land_cover_type="Grass"  # for green roof example
   )
   
   # Run simulation with updated coefficients
   df_output, df_state_final = sp.run_supy(df_forcing, df_state_updated)

**Option B: Manual file editing (for multiple custom surface types):**

1. **Add new coefficient set** to `SUEWS_OHMCoefficients.txt`:

   .. code-block:: text
   
      Code  a1      a2      a3
      10    0.88    20.55   -27.92   ! Custom green roof coefficients
      11    0.15    5.20    -5.45    ! Custom solar panel coefficients

2. **Reference in surface files**: Update `SUEWS_NonVeg.txt` or `SUEWS_Veg.txt` to use the new codes (10, 11).

**Step 4: Validation**

Validate the derived coefficients using SuPy utilities:

.. code-block:: python

   import numpy as np
   import matplotlib.pyplot as plt
   
   # Simulate storage heat flux using derived coefficients
   ser_qs_modelled = sim_ohm(ser_QN, a1, a2, a3)
   
   # Performance statistics
   rmse = np.sqrt(np.mean((ser_QS - ser_qs_modelled)**2))
   r2 = np.corrcoef(ser_QS, ser_qs_modelled)[0,1]**2
   bias = np.mean(ser_qs_modelled - ser_QS)
   
   print(f"Performance Metrics:")
   print(f"RMSE: {rmse:.2f} W m-2")
   print(f"R²: {r2:.3f}")
   print(f"Bias: {bias:.2f} W m-2")
   
   # Create validation plots
   fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
   
   # Scatter plot
   ax1.scatter(ser_QS, ser_qs_modelled, alpha=0.5)
   ax1.plot([ser_QS.min(), ser_QS.max()], [ser_QS.min(), ser_QS.max()], 'r--')
   ax1.set_xlabel('Observed QS (W m⁻²)')
   ax1.set_ylabel('Modelled QS (W m⁻²)')
   ax1.set_title(f'1:1 Comparison (R² = {r2:.3f})')
   
   # Time series comparison (sample week)
   sample_week = ser_QS.iloc[:168]  # First week
   ax2.plot(sample_week.index, sample_week, label='Observed', alpha=0.7)
   ax2.plot(sample_week.index, ser_qs_modelled.iloc[:168], label='Modelled', alpha=0.7)
   ax2.set_xlabel('Time')
   ax2.set_ylabel('QS (W m⁻²)')
   ax2.set_title('Time Series Comparison')
   ax2.legend()
   
   plt.tight_layout()
   plt.show()

**SuPy OHM Utilities:**

The complete workflow uses SuPy's public OHM utilities from ``supy.util``:
- ``derive_ohm_coef(ser_QS, ser_QN)`` - Derive coefficients from measurement data
- ``replace_ohm_coeffs(df_state, coefs, land_cover_type)`` - Update model state  
- ``sim_ohm(ser_qn, a1, a2, a3)`` - Simulate storage heat flux

**Best Practices:**

- **Surface-specific coefficients**: Derive separate coefficients for materially different surfaces
- **Quality control**: Remove periods with instrument errors or missing data
- **Seasonal analysis**: Check if coefficients vary significantly between seasons
- **Physical validation**: Ensure a₁ values are reasonable (typically 0.1-0.8 for urban surfaces)
- **Documentation**: Keep detailed records of measurement conditions and derivation methods

**Common Issues:**

- **Insufficient data**: Less than 6 months of data often leads to unstable coefficients
- **Measurement errors**: ΔQS measurements are challenging; validate against energy balance closure
- **Scale mismatch**: Point measurements may not represent grid-scale surface behaviour

This approach enables SUEWS to better represent the thermal behaviour of specialised urban surfaces through empirically-derived storage heat flux parameterisations.
