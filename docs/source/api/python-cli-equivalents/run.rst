Running SUEWS Simulations
-------------------------

Executing SUEWS simulations programmatically.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    suews-run -p RunControl.nml  # For namelist configuration only

.. note::
   The CLI command ``suews-run`` only supports namelist (RunControl.nml) format.
   For YAML configuration, use the Python API with ``SUEWSSimulation`` class.

Python Equivalent (YAML Configuration)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy import SUEWSSimulation
    from pathlib import Path

    # Create simulation from YAML configuration
    sim = SUEWSSimulation("config.yml")

    # Run simulation
    sim.run()

    # Save results
    sim.save("./output")

    print("Simulation completed successfully!")

Python Equivalent (Namelist Configuration)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import supy
    from pathlib import Path

    # Initialise SUEWS with namelist
    path_runcontrol = Path("RunControl.nml").resolve()
    df_state_init = supy.init_supy(path_runcontrol)

    # Load forcing data
    grid = df_state_init.index[0]  # Use first grid
    df_forcing = supy.load_forcing_grid(path_runcontrol, grid)

    # Run simulation
    df_output, df_state_final = supy.run_supy(df_forcing, df_state_init)

    # Save results
    list_output_files = supy.save_supy(
        df_output,
        df_state_final,
        path_runcontrol=path_runcontrol
    )

    print("Output files created:")
    for file in list_output_files:
        print(f"  {file}")

Multi-Grid Simulations
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import supy
    import pandas as pd
    from pathlib import Path

    # Initialise for multiple grids
    path_runcontrol = Path("RunControl.nml").resolve()
    df_state_init = supy.init_supy(path_runcontrol)

    print(f"Processing {df_state_init.index.size} grids")

    # Process each grid
    results = []
    for grid in df_state_init.index:
        # Load grid-specific forcing
        df_forcing = supy.load_forcing_grid(path_runcontrol, grid)
        df_state_grid = df_state_init.loc[[grid]]

        # Run simulation for this grid
        df_output, df_state_final = supy.run_supy(df_forcing, df_state_grid)
        results.append((df_output, df_state_final))

    # Combine results
    list_df_output, list_df_state_final = zip(*results)
    df_output_combined = pd.concat(list_df_output, names=["grid", "datetime"])
    df_state_combined = pd.concat(list_df_state_final, names=["grid", "datetime"])

    # Save combined results
    supy.save_supy(df_output_combined, df_state_combined, path_runcontrol=path_runcontrol)
