Running SUEWS Simulations
-------------------------

Executing SUEWS simulations from the command line or Python.

.. note::
   SUEWS now uses YAML configuration files. The legacy namelist format (RunControl.nml)
   is deprecated.

   To convert existing namelist files to YAML, see :doc:`conversion`.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    # Using namelist format (deprecated)
    suews-run
    suews-run -p path/to/RunControl.nml

.. note::
   The CLI command currently only supports the deprecated namelist format.
   YAML support is being added - see `Issue #834 <https://github.com/UMEP-dev/SUEWS/issues/834>`_.

   For YAML configurations, use the Python API below.

Python API
~~~~~~~~~~

Run SUEWS simulations using YAML configuration files with the ``SUEWSSimulation`` class.

Basic Simulation
~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy import SUEWSSimulation

    # Create simulation from YAML configuration
    sim = SUEWSSimulation("config.yml")

    # Run simulation
    sim.run()

    # Save results
    sim.save("./output")

    print("Simulation completed successfully!")

Accessing Results
~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy import SUEWSSimulation

    # Run simulation
    sim = SUEWSSimulation("config.yml")
    sim.run()

    # Access simulation results
    results = sim.results  # DataFrame with simulation output

    # Access configuration
    config = sim.config

    # Access forcing data
    forcing = sim.forcing

    # Perform custom analysis
    print(f"Mean QH: {results['QH'].mean():.2f} W/m²")
    print(f"Max QE: {results['QE'].max():.2f} W/m²")

Updating Configuration
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy import SUEWSSimulation

    # Load base configuration
    sim = SUEWSSimulation("base_config.yml")

    # Update specific parameters
    sim.update_config({
        "model": {
            "control": {
                "tstep": 300  # Change to 5-minute timestep
            }
        }
    })

    # Reset and run with new configuration
    sim.reset()
    sim.run()
    sim.save("./output_modified")

Custom Forcing Data
~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy import SUEWSSimulation
    import pandas as pd

    # Create simulation
    sim = SUEWSSimulation("config.yml")

    # Load custom forcing data
    df_forcing = pd.read_csv("custom_forcing.csv", index_col=0, parse_dates=True)

    # Update forcing
    sim.update_forcing(df_forcing)

    # Run with custom forcing
    sim.run()
    sim.save("./output_custom_forcing")
