:doc-source: hand-written

.. _getting_started:

Getting Started
===============

SUEWS is a powerful urban climate modelling tool delivered through **SuPy**, a comprehensive Python interface that integrates seamlessly with the scientific Python ecosystem. This guide provides a clear learning path from your first simulation to advanced urban climate research.

What is SUEWS?
--------------

SUEWS (Surface Urban Energy and Water Balance Scheme) is a physics-based model that simulates urban surface energy and water balance through Python. The model helps researchers understand:

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
   print(f"SUEWS version: {sp.__version__}")
   print("[OK] SUEWS is ready to use!")

Quick Start: Sample Data Tutorial
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Step 1: Load sample data and run your first simulation**

.. code-block:: python

   from supy import SUEWSSimulation

   # Create simulation with built-in sample data
   sim = SUEWSSimulation.from_sample_data()

   # Run simulation and capture the SUEWSOutput object
   output = sim.run()

   print("Congratulations! You have run your first urban climate simulation.")
   print(f"Generated {len(output.df)} time steps of urban climate data")

**Step 2: Explore your results**

.. code-block:: python

   # Quick overview of results structure
   print("Results structure:")
   print(f"Time steps: {len(output.df)}")
   print(f"Output groups: {output.groups}")

   # Extract key variables using get_variable() helper
   # (Results use MultiIndex columns with levels 'group' and 'var')
   qn = sim.get_variable('QN', group='SUEWS')
   qh = sim.get_variable('QH', group='SUEWS')
   print("\nSample energy flux statistics:")
   print(f"Net radiation (QN): {qn.mean().values[0]:.1f} W m-2")
   print(f"Sensible heat (QH): {qh.mean().values[0]:.1f} W m-2")

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
    daily_energy = df_energy.resample('D', level='datetime').mean()

    daily_energy.plot(ax=axes[0,0], title='Daily Mean Energy Fluxes')
    axes[0,0].set_ylabel('Energy Flux (W m-2)')
    axes[0,0].legend(bbox_to_anchor=(1.05, 1), loc='upper left')

    # Monthly patterns
    monthly_energy = df_energy.groupby(df_energy.index.get_level_values("datetime").month).mean()
    monthly_energy.plot(kind='bar', ax=axes[0,1], title='Monthly Energy Balance')
    axes[0,1].set_ylabel('Energy Flux (W m-2)')
    axes[0,1].set_xlabel('Month')

    # Diurnal patterns (summer months)
    t2 = sim.get_variable('T2', group='SUEWS')
    summer_mask = t2.index.get_level_values("datetime").month.isin([6,7,8])
    summer_temp = t2[summer_mask]
    hourly_temp = summer_temp.groupby(summer_temp.index.get_level_values("datetime").hour).mean()
    hourly_temp.iloc[:, 0].plot(ax=axes[1,0], title='Summer Diurnal Temperature Cycle', marker='o')
    axes[1,0].set_ylabel('Air Temperature (degC)')
    axes[1,0].set_xlabel('Hour of Day')
    axes[1,0].grid(True, alpha=0.3)

    # Runoff vs Precipitation
    rain = sim.get_variable('Rain', group='SUEWS')
    runoff = sim.get_variable('RO', group='SUEWS')
    df_water = pd.DataFrame({
        'Rain': rain.iloc[:, 0],
        'RO': runoff.iloc[:, 0]
    })
    daily_water = df_water.resample('D', level='datetime').sum()
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
     - W |m^-2|
     - Net all-wave radiation (incoming - outgoing)
   * - **QF**  
     - W |m^-2|
     - Anthropogenic heat flux (human activities)
   * - **QS**
     - W |m^-2|
     - Net storage heat flux (thermal mass)
   * - **QE**
     - W |m^-2|
     - Latent heat flux (evaporation/transpiration)
   * - **QH**
     - W |m^-2|
     - Sensible heat flux (air heating)
   * - **RO**
     - mm
     - Surface runoff from precipitation
   * - **T2**
     - degC
     - Air temperature at 2m height
   * - **RH2**
     - %
     - Relative humidity at 2m height

.. note::

   **Energy Balance**: The fundamental equation is QN + QF = QS + QE + QH

   This shows how incoming energy (radiation + anthropogenic) is partitioned between storage in urban materials, evaporation, and heating the air.

**Working with Results (Important!)**

``output = sim.run()`` returns a ``SUEWSOutput`` object (also accessible later as ``sim.output``, with the full MultiIndex DataFrame at ``output.df``). Results are stored with MultiIndex columns with levels ``group`` and ``var``. Always use ``sim.get_variable()`` (or ``output.get_variable()``) to extract variables:

.. code-block:: python

   # Recommended way to access results
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

**Interactive Tutorial**: :doc:`Complete Quick Start Tutorial </auto_examples/tutorial_01_quick_start>`

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

Modern SUEWS uses YAML configuration files that organise all model parameters in a clear, hierarchical structure. This replaces the legacy table-based approach with a more intuitive format. The abbreviated excerpt below illustrates the layout (use ``suews init`` in the next section to generate a complete starter file):

.. code-block:: yaml

   # Illustrative SUEWS YAML configuration excerpt (abbreviated)
   model:
     control:
       tstep: 300  # 5-minute time steps
       forcing:
         file:
           value: "Input/Met_Data.txt"
       start_time: "2015-01-01"
       end_time: "2015-12-31"
     physics:
       net_radiation:
         value: 3  # NARP method
       storage_heat:
         value: 1  # OHM method

   sites:
     - name: MyUrbanSite
       gridiv: 1
       properties:
         # Geographic location
         lat:
           value: 51.51  # London coordinates
         lng:
           value: -0.12
         alt:
           value: 35.0
         # Surface cover fractions (sfr, must sum to 1.0 across all
         # surfaces) and surface properties for each land cover type
         land_cover:
           paved:
             sfr:
               value: 0.43
             alb:
               value: 0.10  # Albedo
             emis:
               value: 0.95  # Emissivity
           bldgs:
             sfr:
               value: 0.38
             alb:
               value: 0.15
             emis:
               value: 0.90
             bldgh:
               value: 12.0  # Average building height (m)
           grass:
             sfr:
               value: 0.15
           dectr:
             sfr:
               value: 0.04

Validate Your Configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Before running simulations, scaffold a starter case (or bring your own YAML configuration), validate it, and review any issues:

.. code-block:: bash

   # Create a starter case directory with sample_config.yml and its sample forcing file
   suews init my_case

   # Validate the starter configuration and report suggested updates
   suews validate my_case/sample_config.yml

   # Output files written in my_case/:
   # - my_case/updated_sample_config.yml (updated configuration)
   # - my_case/report_sample_config.txt (detailed validation report)
   # - my_case/report_sample_config.json (machine-readable validation report)

For any configuration ``<name>.yml``, ``suews validate`` writes ``updated_<name>.yml``, ``report_<name>.txt``, and ``report_<name>.json`` beside the input file. By default, the validator writes recognised parameter-name updates and may add ``null`` placeholders for missing critical parameters (omitted optional fields are not padded). It reports missing values and physics option conflicts for your attention; the output is not necessarily ready to run.

The command prints "Validation failed" and exits with status 1 if any validation check reports an error; suggestions alone exit with status 0. Before running simulations, address the report's **ACTION NEEDED** section and replace any null placeholders listed there with real values.

Scientific changes, such as initial temperatures and small surface-fraction corrections, are only suggested in the report, not written to the YAML. Use ``--science-fixes apply`` to write these changes, or ``--science-fixes off`` to suppress scientific suggestions while retaining validation checks.

For the scientific fix policy, output files and report sections, see :doc:`/inputs/yaml/validation`.

Setup Your Site Tutorial
^^^^^^^^^^^^^^^^^^^^^^^^^

For detailed guidance on configuring SUEWS for your specific site:

**Interactive Tutorial**: :doc:`Setup Your Own Site </auto_examples/tutorial_02_setup_own_site>`

This comprehensive notebook covers:
- Site characterisation and data collection
- Land cover fraction determination
- Parameter estimation techniques
- Validation and sensitivity testing
- Common configuration challenges

Using Your Configuration
^^^^^^^^^^^^^^^^^^^^^^^^

Once you have a YAML configuration file (for example, ``my_case/sample_config.yml`` created by ``suews init my_case`` above, or your own site configuration), load and run it with :class:`~supy.SUEWSSimulation`:

.. code-block:: python

   from supy import SUEWSSimulation

   # Create simulation from the starter YAML configuration
   # (replace with the path to your own site YAML file)
   sim = SUEWSSimulation("my_case/sample_config.yml")

   # Run simulation (forcing data loaded from config)
   output = sim.run()

   # Access results as a DataFrame
   print(output.df.head())

   # Save outputs
   sim.save("output_directory/")

   # Save the typed restart artefact explicitly
   checkpoint_path = output.checkpoint.to_file(
       "output_directory/KCL_SUEWS_checkpoint.json"
   )

To continue a simulation into a subsequent period, pair the same YAML configuration with the saved checkpoint, your own forcing file for the next contiguous period, and matching date bounds (because ``my_case/sample_config.yml`` sets ``start_time`` and ``end_time`` to 2012, pass ``start_date`` and ``end_date`` to ``run()`` or update those fields in the configuration). The snippet below is a template; replace ``forcing_next_period.txt`` and the date bounds with your next-period forcing file and period:

.. code-block:: python

   from supy import SUEWSSimulation

   # Template: supply your own forcing file and matching date bounds for the next period
   sim_next = SUEWSSimulation.from_checkpoint(
       "my_case/sample_config.yml",
       "output_directory/KCL_SUEWS_checkpoint.json",
   )
   sim_next.update_forcing("forcing_next_period.txt")
   output_next = sim_next.run(start_date="2013-01-01", end_date="2013-12-31")

The checkpoint is intentionally only the typed runtime state. To continue a
run, load the same YAML configuration, attach the next forcing period, and run
from ``SUEWSSimulation.from_checkpoint(...)``. Checkpoints are keyed by grid ID,
so checkpoint/configuration grid mismatches are rejected by the runtime.
The next forcing period must start exactly one model timestep after the
checkpoint's ``last_timestamp``; forcing that overlaps the checkpointed period
or leaves a gap after it is rejected before the run starts. To re-run the same
period from the checkpointed state on purpose (for example when cycling one
year of forcing for spin-up), pass ``check_continuity=False`` to
``from_checkpoint(...)``.

Cold starts and continuation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The result of ``sim.run()`` exposes ``output.metadata["initialisation"]``.
Its ``origin`` is ``"cold_start"`` for a fresh run and ``"checkpoint"`` for
an explicit or same-instance continuation. ``radiation_memory`` is respectively
``"internal"`` or ``"restored"``; ``warmup_completion`` is always
``"not_assessed"``. A checkpoint identifies a continuation, not a guarantee
that the preceding run was equilibrated.

Cold starts retain every requested output interval, using available forcing
without additional antecedent weather, weather cycling or a discarded prefix.
Internal continuation memories (surface radiation histories ``qn_surfs`` and
``dqndt_surf``, vegetation daily irrigation memories ``evetr``/``dectr``/``grass.wu``,
and running timer/atmosphere diagnostics ``dt_since_start``, ``tair_av``,
``tstep_prev``, ``l_mod``, ``ustar``, ``ra_h``, ``rb`` and ``rs``) are rejected
on fresh YAML runs and restored solely through complete checkpoints. Other
unconfigured kernel state members follow fixed cold-start conventions: running
step-1 diagnostics are overwritten before use, AnOHM buffers start at ``-999`` /
``0`` / ``false``, ``tmin_id`` and ``tmax_id`` default to ``0`` assuming a
midnight start, DyOHM (storage heat methods 6--8) dynamic coefficients stay at
``0`` until the first midnight with ``t2_prev`` and ``temp_surf_dyohm`` at
``0`` degC (requiring warm-up), and STEBBS with ``NetRadiationMethod < 1000``
sees ``0`` W m-2 BEERS roof/wall radiation on step 1 before BEERS runs at the
end of the step. Early results can therefore retain startup influence, and no
elapsed-time threshold declares a run fully warmed.

The same initialisation record is written to ``provenance.json`` under ``run``
by ``sim.save()`` and ``suews run``, including streamed runs. Keep that sidecar
with exported data. Bare DataFrame/file exports and ``output.save()`` do not
carry this run-provenance record; use ``sim.save()`` when it is required.

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

   Derive all surface inputs over the same site footprint and record the
   datasets, processing steps, and assumptions.

For detailed guidance on obtaining and preparing surface characteristics data,
see :doc:`/inputs/surface-data-preparation`.

Part III: Advanced Applications and Research
---------------------------------------------

SuPy enables sophisticated urban climate research through powerful analysis capabilities and model coupling options.

Multi-Site and Comparative Studies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Parallel Processing for Multiple Sites:**

The template below illustrates parallel execution across multiple site configurations; supply your own YAML files in place of the placeholder filenames:

.. code-block:: python

   from supy import SUEWSSimulation
   from multiprocessing import Pool
   import pandas as pd

   def run_site(config_file):
       """Run SUEWS for a single site configuration"""
       sim = SUEWSSimulation(config_file)
       output = sim.run()
       return config_file, output

   # Placeholder paths: supply your own site configuration YAML files
   site_configs = [
       "config_london.yml",
       "config_manchester.yml",
       "config_birmingham.yml"
   ]

   # Parallel execution across all sites
   with Pool() as pool:
       results = pool.map(run_site, site_configs)

   # Note: Each result tuple contains a SUEWSOutput object. Its checkpoint
   # can be saved or passed to SUEWSSimulation.from_checkpoint(...) later.

   monthly_temps = {}
   checkpoints = {}
   for name, output in results:
       # One grid per simulation; otherwise apply .xs(grid_id, level="grid") to each result.
       t2 = output.get_variable('T2', group='SUEWS')
       dt = t2.index.get_level_values("datetime")
       monthly_temps[name] = t2.iloc[:, 0].groupby(dt.month).mean()
       checkpoints[name] = output.checkpoint

   temp_comparison = pd.DataFrame(monthly_temps)
   temp_comparison.plot(kind='bar', title='Monthly Temperature Comparison')

Climate Change Impact Studies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Scenario-Based Analysis:**

The template below compares climate scenarios; supply your own scenario YAML files (each pointing to its respective forcing data) in place of the placeholder filenames:

.. code-block:: python

   import pandas as pd
   from supy import SUEWSSimulation

   # Placeholder paths: supply your own scenario configuration YAML files
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
   print(f"Projected temperature increase: {temp_change:.1f} degC")

   # Energy flux changes
   baseline_fluxes = get_variables(baseline, ['QH', 'QE', 'QS'])
   rcp85_fluxes = get_variables(rcp85, ['QH', 'QE', 'QS'])

   flux_changes = {
       'Sensible Heat': rcp85_fluxes['QH'].mean() - baseline_fluxes['QH'].mean(),
       'Latent Heat': rcp85_fluxes['QE'].mean() - baseline_fluxes['QE'].mean(),
       'Storage Heat': rcp85_fluxes['QS'].mean() - baseline_fluxes['QS'].mean()
   }

**Complete Tutorial**: :doc:`Impact Studies </auto_examples/tutorial_04_impact_studies>`

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

       # Extract surface fluxes and states for WRF
       wrf_fluxes = pd.DataFrame({
           'sensible_heat': get_var('QH'),
           'latent_heat': get_var('QE'),
           'ground_heat': get_var('QS'),
           'friction_velocity': get_var('UStar'),
           'surface_temp': get_var('Tsurf')
       })

       return wrf_fluxes

**Complete Tutorial**: :doc:`External Model Integration </auto_examples/tutorial_06_external_coupling>`

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
       # One grid per simulation; otherwise apply .xs(grid_id, level="grid") to each result.
       # Extract temperature using get_variable()
       urban_temp = urban_sim.get_variable('T2', group='SUEWS').iloc[:, 0]
       rural_temp = rural_sim.get_variable('T2', group='SUEWS').iloc[:, 0]

       urban_dt = urban_temp.index.get_level_values("datetime")
       rural_dt = rural_temp.index.get_level_values("datetime")

       # UHI intensity by time of day
       uhi_diurnal = urban_temp.groupby(urban_dt.hour).mean() - \
                     rural_temp.groupby(rural_dt.hour).mean()

       # Seasonal UHI patterns
       uhi_seasonal = urban_temp.groupby(urban_dt.month).mean() - \
                      rural_temp.groupby(rural_dt.month).mean()

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
       # One grid per simulation; otherwise apply .xs(grid_id, level="grid") to each result.
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
       dt = df_fluxes.index.get_level_values("datetime")
       seasonal_partition = df_fluxes.groupby(dt.month).mean()

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

Supply the path to your own legacy ``RunControl.nml`` in place of ``legacy_input_dir/RunControl.nml`` below (see :doc:`/inputs/transition_guide` for full migration instructions):

.. code-block:: bash

   # Convert your legacy table inputs to modern YAML
   suews convert -i legacy_input_dir/RunControl.nml -o migrated_config.yml

**Testing Your Migration:**

.. code-block:: python

   import pandas as pd
   from supy import SUEWSSimulation

   # Test migrated configuration produced above
   sim = SUEWSSimulation("migrated_config.yml")

   # Short validation run: the whole of the first day (adjust end_date to match
   # your forcing period; a date-only bound runs through the end of that day)
   sim.run(end_date="2012-01-01")

   # Check energy balance using get_variable()
   print("[OK] Migration validation:")
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
- **Community**: `Join the SUEWS community <https://community.suews.io>`__ for discussions
- **Documentation**: :doc:`Complete API reference <inputs/yaml/index>` and parameter guides

**Essential Reading:**

.. bibliography:: assets/refs/refs-SUEWS.bib
   :filter: False
   :list: bullet

   J11
   J14
   W16

**Recent Applications:**

See :ref:`Recent Publications <Recent_publications>` for the latest research using SUEWS and SuPy.

**Training and Workshops:**

The SUEWS community regularly organises training workshops and webinars. Check the mailing list for announcements of upcoming events.
