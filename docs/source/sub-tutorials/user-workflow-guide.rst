.. _user_workflow_guide:

User Workflow Guide
===================

This guide provides comprehensive coverage of three essential SUEWS user workflows:
preparing initial conditions, designing simulation scenarios, and examining results.
It consolidates and expands upon legacy documentation to align with the modern
YAML-based workflow.

.. contents:: On this page
   :local:
   :depth: 2

.. _preparing_initial_conditions:

Part 1: Preparing Initial Conditions
------------------------------------

Initial conditions define the starting state of your simulation, including soil moisture,
vegetation phenology, surface temperatures, and snow conditions. Proper initialisation
is critical for realistic simulation results.

Understanding Initial States
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

SUEWS tracks state variables for each land cover type. The ``initial_states`` section
in your YAML configuration defines these values:

.. code-block:: yaml

   sites:
   - name: MySite
     initial_states:
       # Snow albedo (grid-wide)
       snowalb:
         value: 0.0

       # Land cover-specific states
       paved:
         state:           # Surface wetness [mm]
           value: 0.0
         soilstore:       # Soil moisture [mm]
           value: 120.0
         temperature:     # Thermal layer temperatures [degC]
           value: [12.0, 12.0, 12.0, 12.0, 12.0]
         snowfrac:        # Snow-covered fraction [-]
           value: 0.0
         snowpack:        # Snow water equivalent [mm]
           value: 0.0

       # Vegetation states (dectr, evetr, grass have additional params)
       dectr:
         state:
           value: 0.0
         soilstore:
           value: 120.0
         lai_id:          # Current leaf area index [m2/m2]
           value: 1.0     # Winter: use laimin; Summer: use laimax
         gdd_id:          # Growing degree days [degC d]
           value: 0.0
         sdd_id:          # Senescence degree days [degC d]
           value: -450.0
         porosity_id:     # Canopy porosity (deciduous only)
           value: 0.6     # Winter: high; Summer: low
         decidcap_id:     # Storage capacity (deciduous only)
           value: 0.3     # Winter: low; Summer: high

**Key State Variables Explained:**

- **soilstore**: Soil moisture storage [mm]. Critical for evapotranspiration. Typically
  50-150 mm depending on soil type and climate.
- **lai_id**: Leaf area index. Controls transpiration. Use ``laimin`` for winter,
  ``laimax`` for summer, interpolate for spring/autumn.
- **gdd_id/sdd_id**: Growing and senescence degree days. Reset to 0 and -450 at year
  start; model updates dynamically.
- **temperature**: Thermal layer temperatures. Should reflect typical subsurface
  temperatures for your climate and season.

Spin-Up Strategies
^^^^^^^^^^^^^^^^^^

Initial conditions are often uncertain. The **spin-up** approach runs the model for a
period before your analysis period to allow state variables to equilibrate:

**Method 1: Full Year Spin-Up**

Run one year before your analysis period and use the final state as initial conditions:

.. code-block:: python

   from supy import SUEWSSimulation

   # Step 1: Run spin-up year
   sim_spinup = SUEWSSimulation('config_spinup.yml')  # 2014 forcing
   sim_spinup.run()

   # Step 2: Get equilibrated state
   state_equilibrated = sim_spinup.state_final

   # Step 3: Use for analysis period
   sim_analysis = SUEWSSimulation.from_state(state_equilibrated)
   sim_analysis.update_forcing('forcing_2015.txt')  # Analysis year
   sim_analysis.run()

   # Now results are from properly equilibrated model
   results = sim_analysis.results

**Method 2: Repeated Year Spin-Up**

For limited forcing data, repeat the same year until states converge:

.. code-block:: python

   from supy import SUEWSSimulation
   import numpy as np

   # Initial simulation
   sim = SUEWSSimulation('config.yml')
   sim.run()

   # Iterate until soil moisture converges
   n_spinup = 3  # Typically 2-3 years sufficient
   for i in range(n_spinup):
       state_prev = sim.state_final.copy()
       sim = SUEWSSimulation.from_state(sim.state_final)
       sim.update_forcing('forcing_data.txt')  # Same forcing
       sim.run()

       # Check convergence (optional)
       smd_diff = abs(
           sim.state_final['soilstore_id'].mean() -
           state_prev['soilstore_id'].mean()
       )
       print(f"Spin-up {i+1}: Soil moisture change = {smd_diff:.2f} mm")

   # Final run is your analysis
   results = sim.results

Setting Initial Conditions for Different Seasons
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Winter Start (December-February, Northern Hemisphere)**

.. code-block:: yaml

   initial_states:
     dectr:
       lai_id:
         value: 1.0        # Minimum LAI (leaves off)
       porosity_id:
         value: 0.9        # High porosity (bare branches)
       decidcap_id:
         value: 0.3        # Low water storage capacity
       gdd_id:
         value: 0.0        # Reset at year start
       sdd_id:
         value: -450.0
     grass:
       lai_id:
         value: 1.6        # Near minimum
     paved:
       temperature:
         value: [2.0, 4.0, 6.0, 8.0, 10.0]  # Cold surface layers

**Summer Start (June-August, Northern Hemisphere)**

.. code-block:: yaml

   initial_states:
     dectr:
       lai_id:
         value: 5.5        # Maximum LAI (full canopy)
       porosity_id:
         value: 0.1        # Low porosity (dense canopy)
       decidcap_id:
         value: 0.8        # High water storage capacity
       gdd_id:
         value: 300.0      # Accumulated growing degree days
       sdd_id:
         value: 0.0
     grass:
       lai_id:
         value: 5.9        # Near maximum
     paved:
       temperature:
         value: [20.0, 18.0, 16.0, 14.0, 12.0]  # Warm surface

**Automatic Temperature Initialisation**

The validation tool can automatically set initial temperatures based on climate data:

.. code-block:: bash

   suews-validate config.yml

The validator uses CRU climate data to set appropriate temperatures based on your
site's coordinates and the simulation start month.

Common Initial Condition Pitfalls
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**1. Unrealistic Soil Moisture**

*Problem*: Starting with soil too wet or dry causes unrealistic early evaporation.

*Solution*: Use typical values for your climate:

- Humid climates: 100-150 mm
- Semi-arid: 50-100 mm
- Check ``soilstorecap`` in your config (soil moisture cannot exceed this)

**2. Wrong Vegetation State for Season**

*Problem*: Summer LAI in winter causes excessive transpiration.

*Solution*: Match ``lai_id`` to your start date:

.. code-block:: python

   # Determine initial LAI from month
   def get_initial_lai(month, laimin, laimax):
       """Estimate initial LAI for given month."""
       if month in [12, 1, 2]:  # Winter
           return laimin
       elif month in [6, 7, 8]:  # Summer
           return laimax
       else:  # Spring/Autumn - interpolate
           if month in [3, 4, 5]:
               frac = (month - 2) / 4  # Spring green-up
           else:
               frac = 1 - (month - 8) / 4  # Autumn senescence
           return laimin + frac * (laimax - laimin)

**3. Thermal Layer Depth Mismatch**

*Problem*: Temperature values don't match the number of thermal layers.

*Solution*: Check ``thermal_layers.dz`` and provide matching temperature list:

.. code-block:: yaml

   land_cover:
     paved:
       thermal_layers:
         dz:
           value: [0.2, 0.15, 0.01, 0.01, 0.01]  # 5 layers

   initial_states:
     paved:
       temperature:
         value: [15.0, 14.0, 13.0, 12.0, 11.0]  # Must have 5 values


.. _designing_simulation_scenarios:

Part 2: Designing Simulation Scenarios
--------------------------------------

Scenario analysis is fundamental to urban climate research, enabling studies of
climate change impacts, urban development, and intervention strategies.

Multi-Grid Configurations
^^^^^^^^^^^^^^^^^^^^^^^^^

Run multiple sites (grids) simultaneously for comparative studies:

**Single Configuration File, Multiple Sites:**

.. code-block:: yaml

   model:
     control:
       tstep: 300
       forcing_file:
         value: "forcing_data.txt"  # Common forcing

   sites:
   - name: Urban_Core
     gridiv: 1
     properties:
       lat: {value: 51.51}
       lng: {value: -0.12}
       land_cover:
         paved: {sfr: {value: 0.50}}
         bldgs: {sfr: {value: 0.35}}
         grass: {sfr: {value: 0.10}}
         water: {sfr: {value: 0.05}}

   - name: Suburban
     gridiv: 2
     properties:
       lat: {value: 51.52}
       lng: {value: -0.15}
       land_cover:
         paved: {sfr: {value: 0.25}}
         bldgs: {sfr: {value: 0.20}}
         grass: {sfr: {value: 0.30}}
         dectr: {sfr: {value: 0.20}}
         water: {sfr: {value: 0.05}}

   - name: Rural_Reference
     gridiv: 3
     properties:
       lat: {value: 51.53}
       lng: {value: -0.20}
       land_cover:
         grass: {sfr: {value: 0.50}}
         dectr: {sfr: {value: 0.30}}
         bsoil: {sfr: {value: 0.15}}
         water: {sfr: {value: 0.05}}

**Running and Comparing Multi-Site Results:**

.. code-block:: python

   from supy import SUEWSSimulation
   import pandas as pd

   # Run multi-site simulation
   sim = SUEWSSimulation('config_multisite.yml')
   sim.run()

   # Results have multi-index with grid ID
   results = sim.results

   # Access results for specific grid
   urban = results.xs(1, level='grid')    # Grid 1: Urban Core
   suburban = results.xs(2, level='grid') # Grid 2: Suburban
   rural = results.xs(3, level='grid')    # Grid 3: Rural

   # Calculate Urban Heat Island intensity
   uhi = urban[('SUEWS', 'T2')] - rural[('SUEWS', 'T2')]

   # Diurnal UHI pattern
   uhi_diurnal = uhi.groupby(uhi.index.hour).mean()
   print("Peak UHI hour:", uhi_diurnal.idxmax())
   print("Maximum UHI:", uhi_diurnal.max(), "degrees C")

Parameter Sensitivity Studies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Systematically vary parameters to understand model sensitivity:

.. code-block:: python

   from supy import SUEWSSimulation
   import pandas as pd
   import numpy as np

   def run_sensitivity(base_state, param_values, param_setter, forcing):
       """Run sensitivity analysis on a parameter.

       Parameters
       ----------
       base_state : DataFrame
           Base initial state from sim.state_init
       param_values : list
           Parameter values to test
       param_setter : callable
           Function to modify state with new parameter value
       forcing : DataFrame
           Forcing data

       Returns
       -------
       dict : Results keyed by parameter value
       """
       results = {}
       for val in param_values:
           state = base_state.copy()
           param_setter(state, val)

           sim = SUEWSSimulation.from_state(state)
           sim.update_forcing(forcing)
           sim.run(logging_level=90)  # Suppress logging
           results[val] = sim.results

       return results

   # Example: Sensitivity to building fraction
   sim_base = SUEWSSimulation.from_sample_data()
   base_state = sim_base.state_init.copy()
   forcing = sim_base.forcing

   def set_building_fraction(state, bldg_frac):
       """Set building fraction, adjusting paved to compensate."""
       total_nonveg = state.loc[:, ('sfr_surf', '(0,)')].values[0] + \
                      state.loc[:, ('sfr_surf', '(1,)')].values[0]
       state.loc[:, ('sfr_surf', '(1,)')] = bldg_frac
       state.loc[:, ('sfr_surf', '(0,)')] = total_nonveg - bldg_frac

   # Run sensitivity
   bldg_fractions = np.linspace(0.1, 0.6, 6)
   sensitivity_results = run_sensitivity(
       base_state,
       bldg_fractions,
       set_building_fraction,
       forcing.loc['2012']
   )

   # Analyse sensitivity
   summer_temps = {}
   for bldg_frac, results in sensitivity_results.items():
       summer = results.loc['2012-06':'2012-08']
       summer_temps[bldg_frac] = summer[('SUEWS', 'T2')].mean().values[0]

   print("Building fraction sensitivity:")
   for frac, temp in summer_temps.items():
       print(f"  {frac:.1%}: {temp:.1f} degC")

Climate Change Scenarios
^^^^^^^^^^^^^^^^^^^^^^^^

Modify forcing data to simulate future climate conditions.

.. note::
   The simple delta method shown below (adding a fixed temperature offset) is
   suitable for exploratory analysis. For rigorous climate impact studies, consider:

   - Adjusting specific humidity to maintain relative humidity when temperature changes
   - Modifying precipitation intensity and frequency patterns
   - Accounting for changes in solar radiation and cloud cover
   - Using bias-corrected climate model outputs (e.g., UKCP, CORDEX)

**Temperature Increase Scenarios:**

.. code-block:: python

   from supy import SUEWSSimulation
   import pandas as pd

   # Load baseline simulation
   sim = SUEWSSimulation('config.yml')
   forcing_baseline = sim.forcing.copy()

   # Create climate scenarios
   scenarios = {
       'baseline': forcing_baseline.copy(),
       'rcp45_2050': forcing_baseline.copy(),
       'rcp85_2050': forcing_baseline.copy(),
   }

   # Apply temperature increases (example values)
   scenarios['rcp45_2050']['Tair'] += 1.5  # +1.5 degC
   scenarios['rcp85_2050']['Tair'] += 2.5  # +2.5 degC

   # Run scenarios
   scenario_results = {}
   for name, forcing in scenarios.items():
       sim_scenario = SUEWSSimulation('config.yml')
       sim_scenario.update_forcing(forcing)
       sim_scenario.run(logging_level=90)
       scenario_results[name] = sim_scenario.results

   # Compare results
   for name, results in scenario_results.items():
       t2_mean = results[('SUEWS', 'T2')].mean().values[0]
       qh_mean = results[('SUEWS', 'QH')].mean().values[0]
       print(f"{name}: T2={t2_mean:.1f} degC, QH={qh_mean:.1f} W/m2")

**Urban Development Scenarios:**

.. code-block:: python

   # Scenario: Urban densification
   sim = SUEWSSimulation.from_sample_data()
   state_baseline = sim.state_init.copy()

   # Current state
   print("Baseline land cover:")
   print(state_baseline.sfr_surf)

   # Densification scenario: +10% buildings, -10% grass
   state_dense = state_baseline.copy()
   current_bldg = state_dense.loc[:, ('sfr_surf', '(1,)')].values[0]
   current_grass = state_dense.loc[:, ('sfr_surf', '(4,)')].values[0]

   state_dense.loc[:, ('sfr_surf', '(1,)')] = current_bldg + 0.10
   state_dense.loc[:, ('sfr_surf', '(4,)')] = current_grass - 0.10

   # Green scenario: +10% trees, -10% paved
   state_green = state_baseline.copy()
   current_paved = state_green.loc[:, ('sfr_surf', '(0,)')].values[0]
   current_dectr = state_green.loc[:, ('sfr_surf', '(3,)')].values[0]

   state_green.loc[:, ('sfr_surf', '(0,)')] = current_paved - 0.10
   state_green.loc[:, ('sfr_surf', '(3,)')] = current_dectr + 0.10

   # Run scenarios
   forcing = sim.forcing.loc['2012']

   scenarios = {
       'Baseline': state_baseline,
       'Densification': state_dense,
       'Greening': state_green
   }

   for name, state in scenarios.items():
       sim_s = SUEWSSimulation.from_state(state)
       sim_s.update_forcing(forcing)
       sim_s.run(logging_level=90)

       summer = sim_s.results.loc['2012-06':'2012-08']
       t2_summer = summer[('SUEWS', 'T2')].mean().values[0]
       qe_summer = summer[('SUEWS', 'QE')].mean().values[0]

       print(f"{name}: Summer T2={t2_summer:.1f} degC, QE={qe_summer:.1f} W/m2")


.. _examining_results:

Part 3: Examining Simulation Results
------------------------------------

Understanding and analysing SUEWS output is essential for scientific interpretation
and model validation.

Output File Structure
^^^^^^^^^^^^^^^^^^^^^

SUEWS produces results organised by output groups:

**Main Output Groups:**

- **SUEWS**: Primary energy and water balance variables (QN, QH, QE, QS, QF, Runoff, etc.)
- **DailyState**: Daily summary variables (LAI, GDD, HDD, snow density, etc.)
- **snow**: Detailed snow variables by surface type
- **RSL**: Roughness sublayer profiles (temperature, wind, humidity at multiple heights)
- **SPARTACUS**: Radiation scheme outputs (when enabled)
- **BEERS**: Human thermal comfort variables (when enabled)

**Accessing Results:**

.. code-block:: python

   from supy import SUEWSSimulation

   sim = SUEWSSimulation.from_sample_data()
   sim.run()
   results = sim.results

   # Method 1: get_variable() - recommended for simple access
   qh = sim.get_variable('QH')           # Returns DataFrame
   qe = sim.get_variable('QE')

   # Method 2: Direct MultiIndex access
   qn = results[('SUEWS', 'QN')]

   # For variables in multiple groups, specify group
   albedo_daily = sim.get_variable('AlbSnow', group='DailyState')
   albedo_suews = sim.get_variable('AlbSnow', group='SUEWS')

   # List available variables
   print("Available groups:", results.columns.get_level_values('group').unique())
   print("SUEWS variables:",
         results['SUEWS'].columns.tolist()[:20], "...")

Statistical Analysis
^^^^^^^^^^^^^^^^^^^^

**Basic Statistics:**

.. code-block:: python

   import pandas as pd
   import numpy as np

   # Load and run simulation
   sim = SUEWSSimulation.from_sample_data()
   sim.run()

   # Extract energy balance components
   def get_var(name):
       return sim.get_variable(name, group='SUEWS').iloc[:, 0]

   energy_vars = ['QN', 'QF', 'QS', 'QE', 'QH']
   energy_df = pd.DataFrame({v: get_var(v) for v in energy_vars})

   # Summary statistics
   print("Annual Energy Balance Statistics (W/m2):")
   print(energy_df.describe().round(1))

   # Seasonal means
   seasonal = energy_df.groupby(energy_df.index.quarter).mean()
   seasonal.index = ['Winter', 'Spring', 'Summer', 'Autumn']
   print("\nSeasonal Means:")
   print(seasonal.round(1))

   # Monthly means
   monthly = energy_df.groupby(energy_df.index.month).mean()
   print("\nMonthly Means:")
   print(monthly.round(1))

**Energy Balance Closure:**

.. code-block:: python

   # Check energy balance closure: QN + QF = QS + QE + QH
   energy_in = get_var('QN') + get_var('QF')
   energy_out = get_var('QS') + get_var('QE') + get_var('QH')
   residual = energy_in - energy_out

   print(f"Energy balance residual:")
   print(f"  Mean: {residual.mean():.2f} W/m2")
   print(f"  Std:  {residual.std():.2f} W/m2")
   print(f"  Max:  {residual.abs().max():.2f} W/m2")

   # Note: SUEWS enforces energy balance closure by design.
   # Non-zero residuals indicate numerical precision limits only.

**Water Balance Analysis:**

.. code-block:: python

   # Water balance: P + I = E + R + D + dS
   # where dS = change in surface and soil moisture storage

   rain = get_var('Rain')
   evap = get_var('Evap')
   runoff = get_var('RO')
   drainage = get_var('Drainage')
   irr = get_var('Irr')           # Irrigation (if enabled)
   storage_change = get_var('TotCh')  # Total storage change

   # Annual totals (mm/year)
   annual_rain = rain.sum()
   annual_irr = irr.sum()
   annual_evap = evap.sum()
   annual_runoff = runoff.sum()
   annual_drainage = drainage.sum()
   annual_storage = storage_change.sum()

   print("Annual Water Balance (mm):")
   print(f"  Inputs:")
   print(f"    Precipitation: {annual_rain:.1f}")
   print(f"    Irrigation:    {annual_irr:.1f}")
   print(f"  Outputs:")
   print(f"    Evaporation:   {annual_evap:.1f}")
   print(f"    Runoff:        {annual_runoff:.1f}")
   print(f"    Drainage:      {annual_drainage:.1f}")
   print(f"  Storage change:  {annual_storage:.1f}")
   residual = (annual_rain + annual_irr) - annual_evap - annual_runoff - annual_drainage - annual_storage
   print(f"  Residual:        {residual:.1f}")  # Should be near zero

Common Diagnostic Plots
^^^^^^^^^^^^^^^^^^^^^^^

**Energy Balance Time Series:**

.. code-block:: python

   import matplotlib.pyplot as plt

   fig, axes = plt.subplots(2, 2, figsize=(14, 10))

   # 1. Daily energy fluxes
   ax = axes[0, 0]
   daily_energy = energy_df.resample('D').mean()
   daily_energy.plot(ax=ax)
   ax.set_ylabel('Energy Flux (W/m2)')
   ax.set_title('Daily Mean Energy Fluxes')
   ax.legend(loc='upper right')
   ax.axhline(y=0, color='k', linestyle='--', alpha=0.3)

   # 2. Monthly energy partitioning
   ax = axes[0, 1]
   monthly_means = energy_df[['QS', 'QE', 'QH']].groupby(
       energy_df.index.month
   ).mean()
   monthly_means.plot(kind='bar', ax=ax)
   ax.set_xlabel('Month')
   ax.set_ylabel('Energy Flux (W/m2)')
   ax.set_title('Monthly Energy Partitioning')
   ax.legend(loc='upper right')

   # 3. Diurnal cycle (summer)
   ax = axes[1, 0]
   summer_mask = energy_df.index.month.isin([6, 7, 8])
   summer_energy = energy_df[summer_mask]
   hourly_summer = summer_energy.groupby(summer_energy.index.hour).mean()
   hourly_summer.plot(ax=ax, marker='o', markersize=3)
   ax.set_xlabel('Hour of Day')
   ax.set_ylabel('Energy Flux (W/m2)')
   ax.set_title('Summer Diurnal Cycle')
   ax.legend(loc='upper right')
   ax.axhline(y=0, color='k', linestyle='--', alpha=0.3)

   # 4. Bowen ratio (QH/QE) over time
   ax = axes[1, 1]
   bowen = get_var('QH') / get_var('QE').replace(0, np.nan)
   bowen_daily = bowen.resample('D').mean()
   bowen_daily.plot(ax=ax)
   ax.set_ylabel('Bowen Ratio (QH/QE)')
   ax.set_title('Daily Bowen Ratio')
   ax.set_ylim(-2, 5)
   ax.axhline(y=1, color='r', linestyle='--', alpha=0.5, label='Bowen=1')

   plt.tight_layout()
   plt.savefig('energy_balance_diagnostics.png', dpi=150)
   plt.show()

**Temperature Analysis:**

.. code-block:: python

   fig, axes = plt.subplots(2, 2, figsize=(14, 10))

   t2 = get_var('T2')
   tsurf = get_var('Tsurf')

   # 1. Temperature time series
   ax = axes[0, 0]
   t2.resample('D').mean().plot(ax=ax, label='T2 (2m air)')
   tsurf.resample('D').mean().plot(ax=ax, label='Tsurf (surface)')
   ax.set_ylabel('Temperature (degC)')
   ax.set_title('Daily Mean Temperatures')
   ax.legend()

   # 2. Temperature distribution
   ax = axes[0, 1]
   ax.hist(t2, bins=50, alpha=0.7, label='T2')
   ax.hist(tsurf, bins=50, alpha=0.7, label='Tsurf')
   ax.set_xlabel('Temperature (degC)')
   ax.set_ylabel('Frequency')
   ax.set_title('Temperature Distribution')
   ax.legend()

   # 3. Diurnal temperature cycle by season
   ax = axes[1, 0]
   for season_name, months in [('Winter', [12, 1, 2]),
                                ('Spring', [3, 4, 5]),
                                ('Summer', [6, 7, 8]),
                                ('Autumn', [9, 10, 11])]:
       mask = t2.index.month.isin(months)
       hourly = t2[mask].groupby(t2[mask].index.hour).mean()
       ax.plot(hourly.index, hourly.values, marker='o',
               markersize=3, label=season_name)
   ax.set_xlabel('Hour of Day')
   ax.set_ylabel('T2 (degC)')
   ax.set_title('Seasonal Diurnal Temperature Cycles')
   ax.legend()

   # 4. Surface-air temperature difference
   ax = axes[1, 1]
   delta_t = tsurf - t2
   delta_t_hourly = delta_t.groupby(delta_t.index.hour).mean()
   ax.plot(delta_t_hourly.index, delta_t_hourly.values, 'ko-')
   ax.set_xlabel('Hour of Day')
   ax.set_ylabel('Tsurf - T2 (degC)')
   ax.set_title('Surface-Air Temperature Difference')
   ax.axhline(y=0, color='r', linestyle='--', alpha=0.5)

   plt.tight_layout()
   plt.savefig('temperature_diagnostics.png', dpi=150)
   plt.show()

Validation Against Observations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Compare model outputs with observed data:

.. code-block:: python

   import pandas as pd
   import numpy as np
   from scipy import stats

   def validation_statistics(observed, modelled):
       """Calculate validation statistics.

       Parameters
       ----------
       observed : Series
           Observed values
       modelled : Series
           Modelled values (aligned with observed)

       Returns
       -------
       dict : Validation statistics
       """
       # Align data
       obs, mod = observed.align(modelled, join='inner')
       obs = obs.dropna()
       mod = mod.loc[obs.index]

       n = len(obs)
       mean_obs = obs.mean()
       mean_mod = mod.mean()

       # Bias
       bias = mean_mod - mean_obs

       # RMSE
       rmse = np.sqrt(((mod - obs) ** 2).mean())

       # Correlation
       r, p = stats.pearsonr(obs, mod)

       # Mean Absolute Error
       mae = (mod - obs).abs().mean()

       # Index of Agreement (Willmott)
       numer = ((mod - obs) ** 2).sum()
       denom = ((mod - mean_obs).abs() + (obs - mean_obs).abs()) ** 2
       ioa = 1 - numer / denom.sum()

       return {
           'n': n,
           'mean_obs': mean_obs,
           'mean_mod': mean_mod,
           'bias': bias,
           'rmse': rmse,
           'mae': mae,
           'r': r,
           'r2': r**2,
           'p_value': p,
           'ioa': ioa
       }

   # Example: Compare modelled QH with eddy covariance observations
   # (Assumes you have observation data loaded)

   # sim = SUEWSSimulation('config.yml')
   # sim.run()
   # qh_mod = sim.get_variable('QH', group='SUEWS').iloc[:, 0]
   #
   # obs_data = pd.read_csv('flux_observations.csv',
   #                        index_col=0, parse_dates=True)
   # qh_obs = obs_data['QH']
   #
   # stats_qh = validation_statistics(qh_obs, qh_mod)
   # print("QH Validation Statistics:")
   # for key, val in stats_qh.items():
   #     print(f"  {key}: {val:.3f}" if isinstance(val, float) else f"  {key}: {val}")

   # Scatter plot for validation
   def validation_scatter(observed, modelled, variable_name, units=''):
       """Create validation scatter plot."""
       fig, ax = plt.subplots(figsize=(8, 8))

       obs, mod = observed.align(modelled, join='inner')
       obs = obs.dropna()
       mod = mod.loc[obs.index]

       ax.scatter(obs, mod, alpha=0.3, s=10)

       # 1:1 line
       lims = [min(obs.min(), mod.min()), max(obs.max(), mod.max())]
       ax.plot(lims, lims, 'k--', label='1:1 line')

       # Regression line
       slope, intercept = np.polyfit(obs, mod, 1)
       ax.plot(lims, [slope*x + intercept for x in lims], 'r-',
               label=f'Fit: y = {slope:.2f}x + {intercept:.2f}')

       # Statistics annotation
       stats_dict = validation_statistics(observed, modelled)
       stats_text = (f"n = {stats_dict['n']}\n"
                    f"R2 = {stats_dict['r2']:.3f}\n"
                    f"RMSE = {stats_dict['rmse']:.2f}\n"
                    f"Bias = {stats_dict['bias']:.2f}")
       ax.text(0.05, 0.95, stats_text, transform=ax.transAxes,
               verticalalignment='top', fontsize=10,
               bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

       ax.set_xlabel(f'Observed {variable_name} ({units})')
       ax.set_ylabel(f'Modelled {variable_name} ({units})')
       ax.set_title(f'{variable_name} Validation')
       ax.legend(loc='lower right')
       ax.set_aspect('equal')

       plt.tight_layout()
       return fig, ax

Saving and Exporting Results
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Save to Parquet (Recommended for Large Outputs):**

.. code-block:: python

   # Configure parquet output in YAML
   # model:
   #   control:
   #     output_file:
   #       format: parquet
   #       freq: 3600

   sim = SUEWSSimulation('config.yml')
   sim.run()
   sim.save('output_directory/')

   # Read parquet files later
   import pandas as pd
   df = pd.read_parquet('output_directory/01_SUEWS_output.parquet')

**Export to CSV for External Tools:**

.. code-block:: python

   # Export specific variables
   results = sim.results

   # Export SUEWS group
   suews_output = results['SUEWS']
   suews_output.to_csv('suews_output.csv')

   # Export selected variables
   export_vars = ['QN', 'QH', 'QE', 'QS', 'T2', 'RH2']
   export_df = pd.DataFrame({
       var: sim.get_variable(var, group='SUEWS').iloc[:, 0]
       for var in export_vars
   })
   export_df.to_csv('selected_output.csv')

**Export State for Restart:**

.. code-block:: python

   # Save final state for continuation runs
   final_state = sim.state_final
   final_state.to_csv('final_state.csv')

   # Load and use for new run
   # state_df = pd.read_csv('final_state.csv', index_col=0)
   # sim_continue = SUEWSSimulation.from_state(state_df)


Related Resources
-----------------

- :doc:`../tutorials/python/quick-start` - Interactive tutorial with sample data
- :doc:`../tutorials/python/impact-studies` - Advanced sensitivity and scenario analysis
- :doc:`../inputs/yaml/index` - YAML configuration reference
- :doc:`../outputs/index` - Output file documentation
- :doc:`../api/simulation` - SUEWSSimulation API reference
