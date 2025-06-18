.. _api_reference:

Python API Reference
=====================

The Python interface (SuPy) provides the complete API for SUEWS with comprehensive functionality for urban climate modelling, data analysis, and integration with the scientific Python ecosystem.

Core Functions
~~~~~~~~~~~~~~

.. currentmodule:: supy

.. autosummary::
    :toctree: _autosummary

    init_supy
    load_forcing_grid
    run_supy
    save_supy
    load_SampleData
    show_version

Utility Functions
~~~~~~~~~~~~~~~~~

.. currentmodule:: supy.util

**ERA-5 Data Processing:**

.. autosummary::
    :toctree: _autosummary

    download_era5
    gen_forcing_era5

**Meteorological Data:**

.. autosummary::
    :toctree: _autosummary

    gen_epw
    read_epw
    fill_gap_all

**Energy Balance Analysis:**

.. autosummary::
    :toctree: _autosummary

    derive_ohm_coef
    sim_ohm

**Surface Conductance:**

.. autosummary::
    :toctree: _autosummary

    cal_gs_suews
    cal_gs_obs
    calib_g

**Plotting and Visualisation:**

.. autosummary::
    :toctree: _autosummary

    plot_comp
    plot_day_clm
    plot_rsl

**WRF Integration:**

.. autosummary::
    :toctree: _autosummary

    extract_reclassification
    plot_reclassification

For comprehensive WRF-SUEWS coupling, see :doc:`integration/wrf-suews`.

**Roughness Calculations:**

.. autosummary::
    :toctree: _autosummary

    cal_z0zd
    cal_neutral

Command-Line Tools
~~~~~~~~~~~~~~~~~~

**suews-run**
    Execute SUEWS simulations from command line with YAML configuration files.

**suews-convert**
    Convert legacy table-based input files to modern YAML configuration format.

For detailed command-line usage, see the :doc:`workflow` guide.

Data Structures
~~~~~~~~~~~~~~~

**Core Data Objects:**

- :doc:`State DataFrame <data-structures/df_state>` - Model state variables and parameters
- :doc:`Forcing DataFrame <data-structures/df_forcing>` - Meteorological forcing data
- :doc:`Output DataFrame <data-structures/df_output>` - Simulation results and diagnostics

**Integration with pandas:**

All SUEWS data uses pandas DataFrames with proper indexing, allowing powerful data analysis:

.. code-block:: python

   import supy as sp

   # Load sample data
   df_state, df_forcing = sp.load_sample_data()

   # Run simulation
   df_output, df_state_final = sp.run_supy(df_forcing, df_state)

   # Analyse results using pandas
   monthly_temp = df_output['T2'].resample('M').mean()
   energy_balance = df_output[['QE', 'QH', 'QS', 'QF']].describe()

