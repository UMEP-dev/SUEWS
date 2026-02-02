.. _suews_simulation_tutorial:

SUEWSSimulation Tutorial
========================

This tutorial demonstrates how to use the simplified SUEWSSimulation interface for running SUEWS simulations.

Introduction
------------

The :class:`~supy.SUEWSSimulation` class provides a clean, intuitive interface for SUEWS with:

- Simple configuration management
- Flexible forcing data loading
- Straightforward simulation execution  
- Multiple output formats via OutputConfig
- Easy configuration updates

Getting Started
---------------

1. Basic Simulation
~~~~~~~~~~~~~~~~~~~

The simplest way to run a SUEWS simulation:

.. code-block:: python

    from supy import SUEWSSimulation
    
    # Create simulation from YAML configuration
    sim = SUEWSSimulation('config.yml')
    
    # Update forcing data
    sim.update_forcing('forcing_data.txt')
    
    # Run the simulation
    sim.run()
    
    # Save results
    sim.save('output_dir/')

2. Using Benchmark Data
~~~~~~~~~~~~~~~~~~~~~~~

Run with the provided benchmark data:

.. code-block:: python

    from pathlib import Path
    from supy import SUEWSSimulation
    
    # Use benchmark configuration and forcing
    config_path = Path('test/benchmark1/benchmark1.yml')
    forcing_path = Path('test/benchmark1/forcing/Kc1_2011_data_5.txt')
    
    # Create and run simulation
    sim = SUEWSSimulation(config_path)
    sim.update_forcing(forcing_path)
    sim.run()
    
    # Access results directly
    results = sim.results
    print(f"Simulation complete! {len(results)} timesteps processed")

Working with Results
--------------------

3. Accessing Output Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Results are returned as multi-level pandas DataFrames with columns organised by (group, variable):

.. code-block:: python

    # Access results property
    results = sim.results

    # Method 1: Use get_variable() (recommended for simple access)
    qh = sim.get_variable('QH')          # Sensible heat flux
    qe = sim.get_variable('QE')          # Latent heat flux

    # Method 2: Direct access using (group, variable) syntax
    qs = results[('SUEWS', 'QS')]        # Storage heat flux
    t2 = results[('SUEWS', 'T2')]        # 2m air temperature

    # Calculate daily averages using pandas
    qh_daily = qh.resample('D').mean()

    # Plot energy balance
    import matplotlib.pyplot as plt

    energy_vars = ['QH', 'QE', 'QS', 'QF']
    for var in energy_vars:
        sim.get_variable(var).plot(label=var)
    plt.legend()
    plt.ylabel('Energy flux (W/m²)')
    plt.show()

**Handling variables in multiple groups:**

Some variables appear in multiple output groups (e.g., ``AlbSnow`` in both ``SUEWS`` and ``DailyState``, ``Kup`` in both ``SUEWS`` and ``SPARTACUS``). The ``get_variable()`` method handles this safely:

.. code-block:: python

    # For ambiguous variables, specify the group
    try:
        albedo = sim.get_variable('AlbSnow')  # Raises error if ambiguous
    except ValueError as e:
        print(e)  # "Variable 'AlbSnow' appears in multiple groups: SUEWS, DailyState..."

    # Resolve by specifying group
    albedo_suews = sim.get_variable('AlbSnow', group='SUEWS')
    albedo_daily = sim.get_variable('AlbSnow', group='DailyState')

    # Alternative: direct MultiIndex access
    kup_suews = results[('SUEWS', 'Kup')]
    kup_spartacus = results[('SPARTACUS', 'Kup')]

4. Saving Results
~~~~~~~~~~~~~~~~~

Save results according to OutputConfig settings:

.. code-block:: python

    # Save using default settings from config
    sim.save()  # Saves to current directory
    
    # Save to specific directory
    sim.save('my_output_dir/')
    
    # The format (txt or parquet) is determined by OutputConfig in YAML:
    # output_file:
    #   format: parquet  # or txt
    #   freq: 3600       # output frequency in seconds

Configuration Management
------------------------

5. Updating Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~

Update configuration parameters without reloading:

.. code-block:: python

    # Update timestep
    sim.update_config({'model': {'control': {'tstep': 600}}})
    
    # Update multiple parameters
    sim.update_config({
        'model': {
            'control': {'tstep': 300},
            'physics': {'stabilitymethod': 2}
        }
    })
    
    # Reset and re-run with new configuration
    sim.reset()
    sim.run()

6. Loading Different Configurations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Switch between configurations:

.. code-block:: python

    # Start with one configuration
    sim = SUEWSSimulation('config_summer.yml')
    sim.update_forcing('forcing_summer.txt')
    summer_results = sim.run()
    
    # Switch to different configuration
    sim.update_config('config_winter.yml')
    sim.update_forcing('forcing_winter.txt')
    sim.reset()
    winter_results = sim.run()

Forcing Data Options
--------------------

7. Multiple Forcing Files
~~~~~~~~~~~~~~~~~~~~~~~~~

Load forcing data from multiple files:

.. code-block:: python

    # List of forcing files (concatenated in order)
    forcing_files = [
        'forcing_2023_jan.txt',
        'forcing_2023_feb.txt',
        'forcing_2023_mar.txt'
    ]
    
    sim.update_forcing(forcing_files)

8. DataFrame Forcing
~~~~~~~~~~~~~~~~~~~~

Use pandas DataFrame as forcing:

.. code-block:: python

    import pandas as pd
    
    # Load or create forcing DataFrame
    df_forcing = pd.read_csv('my_forcing.csv', index_col=0, parse_dates=True)
    
    # Use DataFrame directly
    sim.update_forcing(df_forcing)

Advanced Usage
--------------

9. Accessing Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~

Inspect and modify the configuration object:

.. code-block:: python

    # Access configuration
    config = sim.config
    
    # Check current timestep
    print(f"Timestep: {config.model.control.tstep}")
    
    # Check sites
    for site in config.sites:
        print(f"Site: {site.name}, Grid ID: {site.gridiv}")

10. Forcing Fallback
~~~~~~~~~~~~~~~~~~~~

The simulation automatically loads forcing from config if specified:

.. code-block:: yaml

    # In config.yml
    model:
      control:
        forcing_file: forcing/data.txt  # Relative to config file

.. code-block:: python

    # No need to call update_forcing if forcing_file is in config
    sim = SUEWSSimulation('config.yml')
    sim.run()  # Uses forcing from config

Best Practices
--------------

1. **Always check results**: Verify simulation completed successfully
2. **Use relative paths in config**: Makes projects portable
3. **Save frequently**: Use OutputConfig to control format and frequency
4. **Reset between runs**: Use ``sim.reset()`` when changing parameters
5. **Check forcing data**: Ensure forcing covers simulation period

Error Handling
--------------

Common issues and solutions:

.. code-block:: python

    try:
        sim = SUEWSSimulation('config.yml')
        sim.run()
    except FileNotFoundError:
        print("Configuration file not found")
    except RuntimeError as e:
        if "No forcing data" in str(e):
            print("Remember to load forcing data with update_forcing()")
        else:
            raise

Further Reading
---------------

- :doc:`/api/simulation` - Full API reference
- :doc:`/inputs/yaml/index` - YAML configuration guide
- :doc:`/data-structures/df_output` - Understanding output structure