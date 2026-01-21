.. _api_dts:

DTS Interface
=============

.. currentmodule:: supy.dts

The DTS (Derived Type Structure) interface provides direct access to SUEWS
Fortran derived types through f90wrap, enabling a performance-optimised
execution path that bypasses the intermediate DataFrame conversion layer.

When to Use DTS
---------------

The DTS backend is accessed via :meth:`~supy.SUEWSSimulation.run` with
``backend='dts'``:

.. code-block:: python

    from supy import SUEWSSimulation

    sim = SUEWSSimulation('config.yml')
    sim.update_forcing('forcing.txt')
    output = sim.run(backend='dts')

**Use DTS when:**

- Running simulations with YAML-based configuration (``SUEWSConfig``)
- Performance is important (1.2-1.4x speedup from batch execution)
- You need direct Pydantic-to-Fortran execution without DataFrame conversion

**Use traditional backend when:**

- Working with legacy table-based configurations (``df_state_init``)
- You need full DataFrame state tracking (``df_state_final``)
- Maximum compatibility with existing workflows

Performance Characteristics
---------------------------

The DTS backend offers performance improvements through:

- **Batch execution**: Uses ``suews_cal_multitsteps_dts`` to process all
  timesteps in a single Fortran call, avoiding Python loop overhead
- **Direct type mapping**: Populates Fortran derived types directly from
  Pydantic models without intermediate DataFrame conversion
- **Reduced memory allocation**: Pre-allocates forcing and output arrays
  once rather than per-timestep

Typical speedup: **1.2-1.4x** compared to the traditional backend,
particularly noticeable for longer simulations.

State Management
----------------

DTS uses Pydantic ``InitialStates`` for state (not DataFrame):

- Access final state via ``sim.initial_states_final`` property after DTS run
- State persists to YAML format for continuation runs
- Traditional backend state (``sim.state_final``) is ``None`` after DTS run

.. code-block:: python

    # Run with DTS backend
    sim.run(backend='dts')

    # Access Pydantic state (not DataFrame)
    final_state = sim.initial_states_final

    # Save state to YAML for continuation
    final_state.to_yaml('state_after_2012.yaml')

Main Function
-------------

.. autosummary::
   :nosignatures:
   :toctree: generated/

   run_dts

.. autofunction:: run_dts

Factory Functions
-----------------

For advanced users who need to construct DTS objects manually:

.. autosummary::
   :nosignatures:
   :toctree: generated/

   create_suews_config
   create_suews_state
   create_suews_site
   create_suews_forcing
   create_suews_timer
   create_output_line

Population Functions
--------------------

These functions populate Fortran DTS objects from Pydantic models:

.. autosummary::
   :nosignatures:
   :toctree: generated/

   populate_config_from_pydantic
   populate_site_from_pydantic
   populate_state_from_pydantic
   populate_forcing_from_row
   populate_timer_from_datetime

Extraction Functions
--------------------

Functions to extract results from DTS objects:

.. autosummary::
   :nosignatures:
   :toctree: generated/

   extract_output_line_to_dict
   build_output_dataframe_from_block
   build_full_output_dataframe
   extract_state_from_dts

Usage Examples
--------------

Basic Usage via SUEWSSimulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The recommended way to use DTS is through the high-level interface:

.. code-block:: python

    from supy import SUEWSSimulation

    # Load configuration and forcing
    sim = SUEWSSimulation('site_config.yml')
    sim.update_forcing('met_data_2023.txt')

    # Run with DTS backend
    output = sim.run(backend='dts')

    # Access results
    qh = output.get_variable('QH')
    qe = output.get_variable('QE')

    # Access final state (Pydantic model)
    final_state = sim.initial_states_final

Direct DTS Usage (Advanced)
^^^^^^^^^^^^^^^^^^^^^^^^^^^

For users needing fine-grained control over the execution:

.. code-block:: python

    import pandas as pd
    from supy.dts import run_dts
    from supy.data_model.core import SUEWSConfig

    # Load configuration
    config = SUEWSConfig.from_yaml('config.yml')

    # Load forcing data
    df_forcing = pd.read_csv('forcing.txt', parse_dates=['datetime'])
    df_forcing = df_forcing.set_index('datetime')

    # Run simulation
    df_output, final_state = run_dts(
        df_forcing=df_forcing,
        config=config,
        site_index=0,  # For multi-site configs
    )

    # Access Pydantic state from result
    initial_states = final_state['initial_states']

    # Access output DataFrame
    print(df_output[('SUEWS', 'QH')].describe())

Comparison with Traditional Backend
-----------------------------------

- **Configuration source**: Traditional accepts DataFrame or YAML; DTS requires YAML (``SUEWSConfig``)
- **State tracking**: Traditional returns ``df_state_final`` (DataFrame); DTS returns ``initial_states_final`` (Pydantic)
- **Performance**: DTS is 1.2-1.4x faster due to batch Fortran execution
- **Memory**: DTS uses pre-allocated arrays; traditional allocates per-timestep

Related Documentation
---------------------

- :doc:`simulation` - ``SUEWSSimulation`` class reference
- :doc:`/sub-tutorials/suews-simulation-tutorial` - Comprehensive tutorial
