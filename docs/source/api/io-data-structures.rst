.. _io_data_structures:

Key IO Data Structures
======================

Introduction
------------

The Python API uses YAML configuration, a forcing DataFrame, an output
DataFrame, and a typed checkpoint for restart/continuation workflows:

.. code-block:: python

   from supy import SUEWSSimulation

   # Load sample data and run simulation
   sim = SUEWSSimulation.from_sample_data()
   sim.run()

   # Access the data structures
   config = sim.config                 # Input: YAML-backed configuration
   df_forcing = sim.forcing.df         # Input: forcing data
   df_output = output.df               # Output: simulation results
   checkpoint = output.checkpoint      # Output: restart state

**Input DataFrames:**

- YAML ``SUEWSConfig``: User-facing model configuration
- ``df_forcing``: Meteorological forcing data time series

**Output DataFrames:**

- ``df_output`` (or ``sim.results``): Model output results for scientific analysis
- ``SUEWSCheckpoint``: Typed restart state, used together with the YAML
  configuration for subsequent simulations

``df_state_init`` and ``df_state_final`` are retained as legacy compatibility
structures for older tools and file converters. New restart workflows should
use ``config.yml`` plus ``*_SUEWS_checkpoint.json``.


Input
-----

.. _df_state_init:

``df_state_init``: model initial states
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``df_state_init`` is organised with **grids** in rows and **state variables** in columns.

.. code-block:: python

   >>> df_state_init.head()
   var     ah_min       ah_slope_cooling      ...  z z0m_in zdm_in
   ind_dim   (0,)  (1,)             (0,) (1,) ...  0      0      0
   grid
   98        15.0  15.0              2.7  2.7 ... 49.6   1.9   14.2

   [1 rows x 1200 columns]

The details of all state variables can be found in :doc:`/data-structures/df_state`.

**Dimensionality encoding:**

Properties are stored as *flattened values* to fit into the tabular DataFrame format, though they may
actually be of higher dimension. The ``ind_dim`` level in columns indicates variable dimensionality:

- ``0`` for scalars
- ``(ind_dim1, ind_dim2, ...)`` for arrays (vectors are 1D arrays)

For example, ``ohm_coef`` has dimension {8, 4, 3} according to
:ref:`its description <cmdoption-arg-ohm-coef>`. The flattened representation:

.. code-block:: python

   >>> df_state_init.loc[:, "ohm_coef"]
   ind_dim  (0, 0, 0)  (0, 0, 1)  (0, 0, 2)  ...  (7, 3, 1)  (7, 3, 2)
   grid
   98           0.719      0.194      -36.6  ...        0.6      -30.0

   [1 rows x 96 columns]

Users should follow the dimensionality requirements when preparing or modifying ``df_state_init``.


.. _df_forcing:

``df_forcing``: forcing data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``df_forcing`` is organised with **temporal records** in rows and **forcing variables** in columns.

.. code-block:: python

   >>> df_forcing.head()
                          iy  id  it  imin  ...   U     RH    Tair    pres  rain  kdown
   2012-01-01 00:05:00  2012   1   0     5  ...  4.5  85.46  11.77  1001.5   0.0   0.15
   2012-01-01 00:10:00  2012   1   0    10  ...  4.5  85.46  11.77  1001.5   0.0   0.15
   ...

The details of all forcing variables can be found in :doc:`/data-structures/df_forcing`.

Missing values can be specified with ``-999``, which is the default NaN value accepted by SUEWS.

.. note::

   The index of ``df_forcing`` **must be** a ``DatetimeIndex``. The model time-step size
   is determined by the forcing data's temporal resolution:

   .. code-block:: python

      >>> df_forcing.index.freq
      <300 * Seconds>  # 5-minute time step


Output
------

.. _df_output:

``df_output``: model output results
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``df_output`` is organised with **temporal records of grids** in rows (MultiIndex: grid, datetime)
and **output variables by group** in columns (MultiIndex: group, variable).

.. code-block:: python

   >>> df_output.head()
   group                        SUEWS                              ...
   var                          Kdown       Kup    Ldown      Lup  ...
   grid datetime
   98   2012-01-01 00:05:00  0.153333  0.018279  344.31  371.99   ...
        2012-01-01 00:10:00  0.153333  0.018279  344.31  371.99   ...
   ...

   [5 rows x 218 columns]

The details of all output variables can be found in :doc:`/data-structures/df_output`.

Output is recorded at the same temporal resolution as the forcing data:

.. code-block:: python

   >>> df_output.index.levels[1].freq == df_forcing.index.freq
   True


.. _suews_checkpoint:
.. _df_state_final:

``SUEWSCheckpoint``: restart state
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``SUEWSCheckpoint`` stores the Rust backend's typed ``state_json`` payload by
``grid_id``. It is the canonical restart artifact for new workflows and must be
used with the original YAML configuration:

.. code-block:: python

   >>> output = sim.run()
   >>> output.checkpoint.to_file("Kc_SUEWS_checkpoint.json")
   PosixPath('Kc_SUEWS_checkpoint.json')
   >>> sim_next = SUEWSSimulation.from_checkpoint(
   ...     "config.yml",
   ...     "Kc_SUEWS_checkpoint.json",
   ... )

The checkpoint contains:

- ``supy_version``
- ``state_schema_version``
- ``created_at``
- ``last_timestamp``
- ``grid_states`` keyed by ``grid_id``

``df_state_final`` remains available as a legacy DataFrame for compatibility
with older DFState files and developer tools, but it is no longer the preferred
restart artifact.


Related Documentation
---------------------

- :doc:`/data-structures/df_state` - Complete state variable reference
- :doc:`/data-structures/df_forcing` - Forcing variable reference
- :doc:`/data-structures/df_output` - Output variable reference
- :doc:`/auto_examples/tutorial_01_quick_start` - Interactive tutorial with DataFrame examples
