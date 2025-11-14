.. _Workflow:

Getting Started with SUEWS
===========================

SUEWS is a powerful urban climate modelling tool delivered through **SuPy**, a comprehensive Python interface that integrates seamlessly with the scientific Python ecosystem. This guide provides a clear learning path from your first simulation to advanced urban climate research.

What is SUEWS?
--------------

SUEWS (Surface Urban Energy and Water balance Scheme) is a physics-based model that simulates urban surface energy and water balance through Python. The model helps researchers understand:

- **Urban heat islands** and energy flux dynamics
- **Urban hydrology** and surface water balance  
- **Building and vegetation interactions** with the atmosphere
- **Climate change impacts** on urban environments

**SuPy** (SUEWS in Python) is not just an interface - it IS modern SUEWS. All model functionality is accessed through Python, providing pandas DataFrames for analysis, powerful visualisation capabilities, and seamless integration with the broader scientific Python ecosystem.

Part I: Your First Simulation (Interactive Tutorial)
----------------------------------------------------

The best way to understand SUEWS is to run it immediately. Follow this interactive tutorial to get hands-on experience.

Installation and Setup
^^^^^^^^^^^^^^^^^^^^^^^

**Install SuPy:**

.. code-block:: bash

   # Install SuPy (includes SUEWS)
   pip install supy

**Set up your Python environment:**

.. code-block:: python

   import supy as sp
   import pandas as pd
   import matplotlib.pyplot as plt
   import numpy as np
   
   # Verify installation
   print(f"SuPy version: {sp.__version__}")
   print("✅ SUEWS is ready to use!")

Quick Start: Sample Data Tutorial
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Step 1: Load sample data and run your first simulation**

.. code-block:: python

   from supy import SUEWSSimulation

   # Create simulation with built-in sample data
   sim = SUEWSSimulation.from_sample_data()

   # Run simulation
   sim.run()

   print("🎉 Congratulations! You've run your first urban climate simulation.")
   print(f"Generated {len(sim.results)} time steps of urban climate data")

**Step 2: Explore your results**

.. code-block:: python

   # Quick overview of results structure
   print("📈 Results structure:")
   print(f"Time steps: {len(sim.results)}")
   print(f"Output groups: {sim.results.columns.get_level_values('group').unique().tolist()}")

   # Extract key variables using get_variable() helper
   # (Results use MultiIndex columns with (group, variable) structure)
   qn = sim.get_variable('QN', group='SUEWS')
   qh = sim.get_variable('QH', group='SUEWS')
   print("\nSample energy flux statistics:")
   print(f"Net radiation (QN): {qn.mean().values[0]:.1f} W/m²")
   print(f"Sensible heat (QH): {qh.mean().values[0]:.1f} W/m²")

**Step 3: Create your first urban climate visualisation**

.. code-block:: python

   # Plot energy balance components
   fig, axes = plt.subplots(2, 2, figsize=(15, 10))

   # Daily energy fluxes - extract variables using get_variable()
   qn = sim.get_variable('QN', group='SUEWS')
   qf = sim.get_variable('QF', group='SUEWS')
   qs = sim.get_variable('QS', group='SUEWS')
   qe = sim.get_variable('QE', group='SUEWS')
   qh = sim.get_variable('QH', group='SUEWS')

   # Combine into DataFrame for plotting
   df_energy = pd.DataFrame({
       'QN': qn.iloc[:, 0],
       'QF': qf.iloc[:, 0],
       'QS': qs.iloc[:, 0],
       'QE': qe.iloc[:, 0],
       'QH': qh.iloc[:, 0]
   })
   daily_energy = df_energy.resample('D').mean()

   daily_energy.plot(ax=axes[0,0], title='Daily Mean Energy Fluxes')
   axes[0,0].set_ylabel('Energy Flux (W/m²)')
   axes[0,0].legend(bbox_to_anchor=(1.05, 1), loc='upper left')

   # Monthly patterns
   monthly_energy = df_energy.groupby(df_energy.index.month).mean()
   monthly_energy.plot(kind='bar', ax=axes[0,1], title='Monthly Energy Balance')
   axes[0,1].set_ylabel('Energy Flux (W/m²)')
   axes[0,1].set_xlabel('Month')

   # Diurnal patterns (summer months)
   t2 = sim.get_variable('T2', group='SUEWS')
   summer_mask = t2.index.month.isin([6,7,8])
   summer_temp = t2[summer_mask]
   hourly_temp = summer_temp.groupby(summer_temp.index.hour).mean()
   hourly_temp.iloc[:, 0].plot(ax=axes[1,0], title='Summer Diurnal Temperature Cycle', marker='o')
   axes[1,0].set_ylabel('Air Temperature (°C)')
   axes[1,0].set_xlabel('Hour of Day')
   axes[1,0].grid(True, alpha=0.3)

   # Runoff vs Precipitation
   rain = sim.get_variable('Rain', group='SUEWS')
   runoff = sim.get_variable('Runoff', group='SUEWS')
   df_water = pd.DataFrame({
       'Rain': rain.iloc[:, 0],
       'Runoff': runoff.iloc[:, 0]
   })
   daily_water = df_water.resample('D').sum()
   daily_water.plot(ax=axes[1,1], title='Daily Water Balance')
   axes[1,1].set_ylabel('Water (mm/day)')
   axes[1,1].legend()

   plt.tight_layout()
   plt.show()

Understanding Your Results
^^^^^^^^^^^^^^^^^^^^^^^^^^

The simulation produces comprehensive urban climate data:

.. list-table:: Key SUEWS Output Variables
   :widths: 15 20 65
   :header-rows: 1

   * - Variable
     - Units
     - Description
   * - **QN**
     - W/m²
     - Net all-wave radiation (incoming - outgoing)
   * - **QF**  
     - W/m²
     - Anthropogenic heat flux (human activities)
   * - **QS**
     - W/m²
     - Net storage heat flux (thermal mass)
   * - **QE**
     - W/m²
     - Latent heat flux (evaporation/transpiration)
   * - **QH**
     - W/m²
     - Sensible heat flux (air heating)
   * - **Runoff**
     - mm
     - Surface runoff from precipitation
   * - **T2**
     - °C
     - Air temperature at 2m height
   * - **RH2**
     - %
     - Relative humidity at 2m height

.. note::

   **Energy Balance**: The fundamental equation is QN + QF = QS + QE + QH

   This shows how incoming energy (radiation + anthropogenic) is partitioned between storage in urban materials, evaporation, and heating the air.

**Working with Results (Important!)**

Results are stored with MultiIndex columns ``(group, variable)``. Always use ``sim.get_variable()`` to extract data:

.. code-block:: python

   # ✅ Correct way to access results
   qh = sim.get_variable('QH', group='SUEWS')
   t2 = sim.get_variable('T2', group='SUEWS')

   # Helper function for multiple variables
   def get_variables(sim, var_names, group='SUEWS'):
       """Extract multiple variables into a simple DataFrame"""
       return pd.DataFrame({
           var: sim.get_variable(var, group=group).iloc[:, 0]
           for var in var_names
       })

   # Use the helper
   energy = get_variables(sim, ['QN', 'QF', 'QS', 'QE', 'QH'])
   print(energy.describe())

**Complete Interactive Tutorial**

For the full hands-on experience, run the complete tutorial notebook:

📓 **Interactive Notebook**: :doc:`Complete Quick Start Tutorial <tutorials/python/quick-start>`

This notebook includes:
- Detailed explanations of each step
- Additional visualisation examples  
- Data exploration exercises
- Troubleshooting tips

Part II: Configure SUEWS for Your Site
---------------------------------------

Now that you understand how SUEWS works, let's configure it for your specific research site.

Understanding YAML Configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Modern SUEWS uses YAML configuration files that organise all model parameters in a clear, hierarchical structure. This replaces the legacy table-based approach with a more intuitive format:

.. code-block:: yaml

   # Complete SUEWS configuration example
   model:
     control:
       tstep: 300  # 5-minute time steps
       forcing_file:
         value: "Input/Met_Data.txt"
       start_date: "2015-01-01"
       end_date: "2015-12-31"
     physics:
       netradiationmethod:
         value: 3  # NARP method
       storageheatmethod:
         value: 1  # OHM method
   
   sites:
     - name: MyUrbanSite
       grid_id:
         value: 1
       properties:
         # Geographic location
         lat:
           value: 51.51  # London coordinates
         lng:
           value: -0.12
         alt:
           value: 35.0
         # Surface cover fractions (must sum to 1.0)
         frc_land_cover:
           Paved:
             value: 0.43
           Buildings:
             value: 0.38
           Grass:
             value: 0.15
           DeciduousTrees:
             value: 0.04
         # Surface properties for each land cover type
         land_cover_params:
           Paved:
             alb:
               value: 0.10  # Albedo
             emis:
               value: 0.95  # Emissivity
           Buildings:
             alb:
               value: 0.15
             emis:
               value: 0.90
             bldgh:
               value: 12.0  # Average building height (m)

Validate Your Configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Before running simulations, validate your configuration to catch and fix common issues:

.. code-block:: bash

   # Validate and automatically fix your configuration
   suews-validate config.yml

   # This creates:
   # - updated_config.yml (corrected configuration)
   # - report_config.txt (detailed validation report)

The validator automatically:

- Adds missing parameters with sensible defaults
- Normalises surface fractions to sum to 1.0
- Sets initial temperatures based on location and season
- Ensures physics options are compatible
- Updates deprecated parameter names to current standards

The validation report consolidates all changes made across multiple validation phases, providing a clear summary of what was automatically fixed and what requires your attention.

For more details, see :doc:`/inputs/yaml/validation`.

Setup Your Site Tutorial
^^^^^^^^^^^^^^^^^^^^^^^^^

For detailed guidance on configuring SUEWS for your specific site:

📓 **Interactive Tutorial**: :doc:`Setup Your Own Site <tutorials/python/setup-own-site>`

This comprehensive notebook covers:
- Site characterisation and data collection
- Land cover fraction determination
- Parameter estimation techniques
- Validation and sensitivity testing
- Common configuration challenges

Using Your Configuration
^^^^^^^^^^^^^^^^^^^^^^^^

Once you have a YAML configuration file, use the `SUEWSSimulation` class:

.. code-block:: python

   from supy import SUEWSSimulation

   # Create simulation from YAML configuration
   sim = SUEWSSimulation("path/to/your/config_suews.yml")

   # Run simulation (forcing data loaded from config)
   sim.run()

   # Access results
   print(sim.results)

   # Save outputs
   sim.save("output_directory/")

For detailed examples, see :doc:`/sub-tutorials/suews-simulation-tutorial`.

Data Requirements and Quality
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Essential Data for Your Site:**

- **Meteorological forcing**: Air temperature, humidity, wind speed, radiation, precipitation
- **Site characteristics**: Land cover fractions, building heights, vegetation properties  
- **Geographic information**: Latitude, longitude, altitude

**Data Quality Guidelines:**

- Land cover fractions are critical - ensure accuracy :cite:`W16`
- Quality meteorological data, especially precipitation and radiation :cite:`K18UC`
- Representative measurement heights above the urban canopy
- Continuous data without gaps for the simulation period

.. tip::

   **Data Sources**: Use the `UMEP`_ plugin for QGIS to derive land cover fractions from satellite imagery or local spatial datasets.

Part III: Advanced Applications and Research
---------------------------------------------

SuPy enables sophisticated urban climate research through powerful analysis capabilities and model coupling options.

Multi-Site and Comparative Studies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Parallel Processing for Multiple Sites:**

.. code-block:: python

   from supy import SUEWSSimulation
   from multiprocessing import Pool
   import pandas as pd

   def run_site(config_file):
       """Run SUEWS for a single site configuration"""
       sim = SUEWSSimulation(config_file)
       sim.run()
       return config_file, (sim.results, sim.state_final)

   # Configuration files for different sites
   site_configs = [
       "config_london.yml",
       "config_manchester.yml",
       "config_birmingham.yml"
   ]

   # Parallel execution across all sites
   with Pool() as pool:
       results = pool.map(run_site, site_configs)

   # Note: Each result tuple contains (df_output, df_state_final)
   # We need to access variables using the simulation object or helper

   # Create simulations from stored states to access get_variable()
   monthly_temps = {}
   for name, (output, state) in results:
       # Recreate sim with results for get_variable() access
       sim_temp = SUEWSSimulation()
       sim_temp._df_output = output
       sim_temp._run_completed = True

       t2 = sim_temp.get_variable('T2', group='SUEWS')
       monthly_temps[name] = t2.iloc[:, 0].groupby(t2.index.month).mean()

   temp_comparison = pd.DataFrame(monthly_temps)
   temp_comparison.plot(kind='bar', title='Monthly Temperature Comparison')

Climate Change Impact Studies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Scenario-Based Analysis:**

.. code-block:: python

   from supy import SUEWSSimulation

   # Define climate scenarios with different forcing files
   scenarios = {
       'baseline': 'config_baseline.yml',
       'rcp45_2050': 'config_rcp45.yml',
       'rcp85_2050': 'config_rcp85.yml'
   }

   scenario_results = {}

   for scenario_name, config_file in scenarios.items():
       # Run simulation for each scenario
       sim = SUEWSSimulation(config_file)
       sim.run()
       scenario_results[scenario_name] = sim

   # Calculate climate change impacts using the helper from earlier
   def get_variables(sim, var_names, group='SUEWS'):
       """Extract multiple variables into a simple DataFrame"""
       return pd.DataFrame({
           var: sim.get_variable(var, group=group).iloc[:, 0]
           for var in var_names
       })

   baseline = scenario_results['baseline']
   rcp85 = scenario_results['rcp85_2050']

   # Temperature changes
   temp_baseline = baseline.get_variable('T2', group='SUEWS').iloc[:, 0]
   temp_rcp85 = rcp85.get_variable('T2', group='SUEWS').iloc[:, 0]
   temp_change = temp_rcp85.mean() - temp_baseline.mean()
   print(f"Projected temperature increase: {temp_change:.1f}°C")

   # Energy flux changes
   baseline_fluxes = get_variables(baseline, ['QH', 'QE', 'QS'])
   rcp85_fluxes = get_variables(rcp85, ['QH', 'QE', 'QS'])

   flux_changes = {
       'Sensible Heat': rcp85_fluxes['QH'].mean() - baseline_fluxes['QH'].mean(),
       'Latent Heat': rcp85_fluxes['QE'].mean() - baseline_fluxes['QE'].mean(),
       'Storage Heat': rcp85_fluxes['QS'].mean() - baseline_fluxes['QS'].mean()
   }

**Complete Tutorial**: :doc:`Impact Studies <tutorials/python/impact-studies>`

Model Coupling and Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

SuPy enables integration with other atmospheric and urban models:

.. code-block:: python

   # Example: Prepare SUEWS output for WRF coupling
   def prepare_wrf_coupling(sim, grid_config):
       """Prepare SUEWS output for WRF model coupling

       Parameters
       ----------
       sim : SUEWSSimulation
           Completed SUEWS simulation
       grid_config : dict
           WRF grid configuration
       """
       # Helper to extract variables
       def get_var(name):
           return sim.get_variable(name, group='SUEWS').iloc[:, 0]

       # Extract surface fluxes for WRF
       wrf_fluxes = pd.DataFrame({
           'sensible_heat': get_var('QH'),
           'latent_heat': get_var('QE'),
           'ground_heat': get_var('QS'),
           'momentum_flux': get_var('Tau'),
           'surface_temp': get_var('TSurf')
       })

       return wrf_fluxes

**Complete Tutorial**: :doc:`External Model Integration <integration/external-interaction>`

Advanced Analysis Patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Urban Heat Island Analysis:**

.. code-block:: python

   # Compare urban vs rural sites
   def calculate_uhi_intensity(urban_sim, rural_sim):
       """Calculate urban heat island intensity

       Parameters
       ----------
       urban_sim, rural_sim : SUEWSSimulation
           Completed simulations for urban and rural sites
       """
       # Extract temperature using get_variable()
       urban_temp = urban_sim.get_variable('T2', group='SUEWS').iloc[:, 0]
       rural_temp = rural_sim.get_variable('T2', group='SUEWS').iloc[:, 0]

       # UHI intensity by time of day
       uhi_diurnal = urban_temp.groupby(urban_temp.index.hour).mean() - \
                     rural_temp.groupby(rural_temp.index.hour).mean()

       # Seasonal UHI patterns
       uhi_seasonal = urban_temp.groupby(urban_temp.index.month).mean() - \
                      rural_temp.groupby(rural_temp.index.month).mean()

       return uhi_diurnal, uhi_seasonal

**Energy Balance Analysis:**

.. code-block:: python

   def analyse_energy_balance(sim):
       """Comprehensive energy balance analysis

       Parameters
       ----------
       sim : SUEWSSimulation
           Completed simulation
       """
       # Helper to extract variables
       def get_var(name):
           return sim.get_variable(name, group='SUEWS').iloc[:, 0]

       # Energy balance components
       qn = get_var('QN')
       qf = get_var('QF')
       qs = get_var('QS')
       qe = get_var('QE')
       qh = get_var('QH')

       energy_in = qn + qf
       energy_out = qs + qe + qh

       # Balance closure
       balance_error = energy_in - energy_out

       # Seasonal energy partitioning
       df_fluxes = pd.DataFrame({'QS': qs, 'QE': qe, 'QH': qh})
       seasonal_partition = df_fluxes.groupby(df_fluxes.index.month).mean()

       # Bowen ratio (sensible/latent heat)
       bowen_ratio = qh / qe

       return {
           'balance_closure': balance_error.std(),
           'seasonal_partitioning': seasonal_partition,
           'mean_bowen_ratio': bowen_ratio.mean()
       }

Migration from Legacy SUEWS
----------------------------

**For users transitioning from table-based SUEWS:**

The modern SuPy interface offers significant advantages over legacy formats:

- **Streamlined workflow**: Single Python environment for all operations
- **Better data handling**: Native pandas integration with powerful analysis tools
- **Advanced capabilities**: Parallel processing, automated validation, model coupling
- **Future development**: All new features use the SuPy interface

Migration Process
^^^^^^^^^^^^^^^^^

**Automated Conversion:**

.. code-block:: bash

   # Convert legacy table inputs to modern YAML (pending issue #581)
   suews-convert to-yaml -i legacy_input_dir/ -o modern_config.yml
   
   # Note: This feature is under development (see issue #581)
   # For now, use the SUEWSSimulation class with existing table inputs or YAML files

**Testing Your Migration:**

.. code-block:: python

   from supy import SUEWSSimulation

   # Test migrated configuration
   sim = SUEWSSimulation("migrated_config.yml")

   # Short validation run (24 hours)
   sim.run(end_date="2012-01-02")

   # Check energy balance using get_variable()
   print("✅ Migration validation:")
   energy_fluxes = pd.DataFrame({
       'QE': sim.get_variable('QE', group='SUEWS').iloc[:, 0],
       'QH': sim.get_variable('QH', group='SUEWS').iloc[:, 0],
       'QS': sim.get_variable('QS', group='SUEWS').iloc[:, 0],
       'QF': sim.get_variable('QF', group='SUEWS').iloc[:, 0]
   })
   print(energy_fluxes.describe())

Getting Support and Community
-----------------------------

**SuPy and SUEWS Community:**

- **GitHub Repository**: `SUEWS on GitHub <https://github.com/UMEP-dev/SUEWS>`__ for issues and contributions
- **Mailing List**: `Join the SUEWS community <https://www.lists.reading.ac.uk/mailman/listinfo/met-suews>`__ for discussions
- **Documentation**: :doc:`Complete API reference <inputs/yaml/index>` and parameter guides

**Essential Reading:**

.. bibliography:: assets/refs/refs-SUEWS.bib
   :filter: False
   :list: bullet

   J11
   J14
   W16

**Recent Applications:**

See `Recent Publications <Recent_publications>`__ for the latest research using SUEWS and SuPy.

**Training and Workshops:**

The SUEWS community regularly organises training workshops and webinars. Check the mailing list for announcements of upcoming events.

.. _`UMEP`: http://umep-docs.readthedocs.io/en/latest/index.html