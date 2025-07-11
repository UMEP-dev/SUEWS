.. _suews_simulation_tutorial:

SUEWSSimulation Tutorial
========================

This tutorial demonstrates how to use the new object-oriented SUEWSSimulation interface for running SUEWS simulations.

Introduction
------------

The :class:`~supy.SUEWSSimulation` class provides a modern, intuitive interface for SUEWS with:

- Clear, chainable workflows
- Built-in validation and error handling  
- Multiple output formats
- Easy parameter overrides
- Integration with pandas DataFrames

Getting Started
---------------

1. Basic Simulation
~~~~~~~~~~~~~~~~~~~

The simplest way to run a SUEWS simulation:

.. code-block:: python

    from supy import SUEWSSimulation
    
    # Create simulation from YAML configuration
    sim = SUEWSSimulation.from_yaml('config.yml')
    
    # Setup forcing data
    sim.setup_forcing('forcing_data.txt')
    
    # Run the simulation
    sim.run()
    
    # View summary
    print(sim.summary())

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
    sim = SUEWSSimulation.from_yaml(config_path)
    sim.setup_forcing(forcing_path)
    sim.run()
    
    # Access results
    results = sim.get_results()
    print(f"Simulation complete! {len(results)} timesteps processed")

Working with Results
--------------------

3. Accessing Output Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Results are returned as multi-level pandas DataFrames:

.. code-block:: python

    # Get all results
    results = sim.get_results()
    
    # Access specific variables using (group, variable) syntax
    qh = results[('SUEWS', 'QH')]        # Sensible heat flux
    qe = results[('SUEWS', 'QE')]        # Latent heat flux
    qs = results[('SUEWS', 'QS')]        # Storage heat flux
    t2 = results[('SUEWS', 'T2')]        # 2m air temperature
    
    # Calculate daily averages
    qh_daily = qh.resample('D').mean()
    
    # Plot energy balance
    import matplotlib.pyplot as plt
    
    energy_vars = ['QH', 'QE', 'QS', 'QF']
    for var in energy_vars:
        results[('SUEWS', var)].plot(label=var)
    plt.legend()
    plt.ylabel('Energy flux (W/m²)')
    plt.show()

4. Quick Visualisation
~~~~~~~~~~~~~~~~~~~~~~

Use built-in plotting methods:

.. code-block:: python

    # Quick plot of key variables
    sim.quick_plot()
    
    # Custom plot of energy balance
    sim.quick_plot(variables=['QH', 'QE', 'QS'], 
                   title='Energy Balance Components')
    
    # View first few rows of results
    sim.see(n=10)

Saving Outputs
--------------

5. Multiple Output Formats
~~~~~~~~~~~~~~~~~~~~~~~~~~

Save results in various formats:

.. code-block:: python

    # CSV format (default)
    sim.save('results.csv')
    
    # Excel format with multiple sheets
    sim.save('results.xlsx')
    
    # NetCDF for large datasets
    sim.save('results.nc')
    
    # Pickle for Python reuse
    sim.save('results.pkl')
    
    # Save specific variables only
    sim.save('energy_fluxes.csv', 
             variables=['QH', 'QE', 'QS', 'QF'])
    
    # Save with resampling
    sim.save('hourly_results.csv', 
             resample='1h',
             resample_method='mean')

Advanced Features
-----------------

6. Parameter Overrides
~~~~~~~~~~~~~~~~~~~~~~

Override specific parameters without modifying the YAML file:

.. code-block:: python

    # Override timestep and output settings
    sim = SUEWSSimulation.from_yaml(
        'config.yml',
        tstep=300,               # 5-minute timestep
        writeoutoption=2,        # Detailed output
        debug_mode=True,         # Enable debug logging
        output_dir='/tmp/test'   # Custom output directory
    )

7. Validation and Debugging
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Validate configuration before running:

.. code-block:: python

    # Validate setup
    validation = sim.validate()
    
    if validation['status'] == 'valid':
        print("Configuration is valid!")
    elif validation['status'] == 'valid_with_warnings':
        print("Warnings:", validation['warnings'])
    else:
        print("Errors:", validation['errors'])
        
    # Enable debug mode for detailed logging
    sim = SUEWSSimulation(config, debug_mode=True)

8. Cloning and Batch Simulations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run multiple scenarios efficiently:

.. code-block:: python

    # Base simulation
    base_sim = SUEWSSimulation.from_yaml('config.yml')
    base_sim.setup_forcing('forcing.txt')
    
    # Clone for different scenarios
    scenarios = {
        'high_albedo': {'alb_bldgs': 0.8, 'alb_roads': 0.6},
        'more_vegetation': {'fr_grass': 0.4, 'fr_trees': 0.3},
        'reduced_traffic': {'qf_a': [10, 10, 10]}  # Reduced anthropogenic heat
    }
    
    results = {}
    for name, params in scenarios.items():
        # Clone base simulation with parameter overrides
        scenario_sim = base_sim.clone(**params)
        scenario_sim.run()
        results[name] = scenario_sim.get_results()
        
    # Compare scenarios
    for name in scenarios:
        results[name][('SUEWS', 'T2')].plot(label=name)
    plt.legend()
    plt.ylabel('Temperature (°C)')
    plt.title('Temperature under different scenarios')

Integration with Existing SuPy
------------------------------

9. Compatibility with Traditional Workflow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SUEWSSimulation class is fully compatible with existing SuPy workflows:

.. code-block:: python

    import supy as sp
    
    # Traditional SuPy approach
    df_state, df_forcing = sp.load_SampleData()
    df_output_traditional, _ = sp.run_supy(df_forcing, df_state)
    
    # New object-oriented approach
    sim = SUEWSSimulation(df_state)
    sim.setup_forcing(df_forcing)
    sim.run()
    df_output_oo = sim.get_results()
    
    # Results are identical
    pd.testing.assert_frame_equal(
        df_output_traditional, 
        df_output_oo,
        check_like=True
    )

Best Practices
--------------

1. **Always validate** your configuration before running
2. **Use debug mode** when troubleshooting issues
3. **Save results** in appropriate formats for your analysis
4. **Clone simulations** for scenario comparisons
5. **Check units** - all times in seconds, temperatures in °C
6. **Use resampling** for long simulations to reduce file sizes

Common Issues
-------------

**Missing forcing data columns**
    The simulation will attempt to fill missing columns with defaults but will warn you.

**Invalid configuration**
    Use ``sim.validate()`` to check for configuration issues before running.

**Memory issues with large simulations**
    Use chunking or save intermediate results:
    
    .. code-block:: python
    
        # Process in chunks
        sim.run(chunk_size=8760)  # Process one year at a time

**Different results from table-based SUEWS**
    Ensure your YAML configuration exactly matches the table parameters using the converter:
    
    .. code-block:: bash
    
        suews-convert to-yaml -i /path/to/tables -o config.yml

Next Steps
----------

- Explore the :doc:`/api/simulation` for detailed method documentation
- See :doc:`/inputs/yaml/index` for YAML configuration options
- Review :doc:`/data-structures/df_output` for output variable descriptions
- Try the :doc:`/sub-tutorials/tutorials` for more examples