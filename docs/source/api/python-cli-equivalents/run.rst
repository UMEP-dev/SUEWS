Running SUEWS Simulations
-------------------------

Executing SUEWS simulations from the command line or Python.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    suews-run -p RunControl.nml

.. note::
   The CLI command ``suews-run`` only supports legacy namelist (RunControl.nml) format.

   **For YAML configurations**, use the Python API with ``SUEWSSimulation`` class (examples below).
   To convert existing namelist files to YAML, see :doc:`conversion`.

Python API (Recommended)
~~~~~~~~~~~~~~~~~~~~~~~~~

The modern approach uses YAML configuration files with the ``SUEWSSimulation`` class.

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
