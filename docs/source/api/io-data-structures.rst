.. _io_data_structures:

Key IO Data Structures
======================

Introduction
------------

The Python API uses pandas DataFrame structures for analysis data and a typed
checkpoint object for restart workflows:

.. code-block:: python

   from supy import SUEWSSimulation

   # Load sample data and run simulation
   sim = SUEWSSimulation.from_sample_data()
   output = sim.run()

   # Access the data structures
   df_state_init = sim.df_state_init   # Input: initial states
   df_forcing = sim.df_forcing         # Input: forcing data
   df_output = sim.results             # Output: simulation results
   df_state_final = sim.df_state_final # Legacy/developer final states
   checkpoint = output.checkpoint      # Preferred restart artefact

**Input DataFrames:**

- ``df_state_init``: Model initial states and configuration parameters
- ``df_forcing``: Meteorological forcing data time series

**Output and restart objects:**

- ``df_output`` (or ``sim.results``): Model output results for scientific analysis
- ``df_state_final``: Legacy/developer final states for compatibility and inspection
- ``SUEWSCheckpoint``: Typed runtime state for continuation runs


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


.. _df_state_final:

``df_state_final``: model final states
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``df_state_final`` has the identical data structure as ``df_state_init`` except for an
extra ``datetime`` level in the index, which stores temporal information associated with
model states.

.. code-block:: python

   >>> df_state_final.head()
   var                      ah_min       ah_slope_cooling  ...  z z0m_in zdm_in
   ind_dim                    (0,)  (1,)             (0,)  ...  0      0      0
   datetime            grid
   2012-01-01 00:05:00 98     15.0  15.0              2.7  ... 49.6   1.9   14.2
   2013-01-01 00:05:00 98     15.0  15.0              2.7  ... 49.6   1.9   14.2

   [2 rows x 1200 columns]

This structure is retained for compatibility and state inspection:

- **Runtime diagnostics**: Save intermediate states with ``save_state=True`` in ``run_supy``
- **Developer inspection**: Examine flattened state variables in DataFrame form

The meanings of state variables in ``df_state_final`` are the same as in ``df_state_init``,
documented in :doc:`/data-structures/df_state`.

For new object-oriented continuation workflows, use ``SUEWSCheckpoint`` rather
than ``df_state_final`` as the restart artefact.


.. _suews_checkpoint:

``SUEWSCheckpoint``: typed restart state
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``SUEWSCheckpoint`` carries typed backend runtime state keyed by grid ID. It is
the preferred restart artefact for new object-oriented workflows.

The checkpoint is intentionally only the typed runtime state. It does not
contain the full YAML configuration or forcing data. To continue a run, load the
same YAML configuration, attach the next forcing period, and run from
``SUEWSSimulation.from_checkpoint(...)``:

.. code-block:: python

   from supy import SUEWSCheckpoint, SUEWSSimulation

   checkpoint = SUEWSCheckpoint.from_file("{site}_SUEWS_checkpoint.json")

   sim_next = SUEWSSimulation.from_checkpoint(
       "config.yml",
       checkpoint,
   )
   sim_next.update_forcing("forcing_next_period.txt")
   output_next = sim_next.run()

Use ``checkpoint.to_file(path)`` to write the checkpoint explicitly, or
``sim.save(path)`` to save it alongside the configured output files. Checkpoints
are keyed by grid ID, so checkpoint/configuration grid mismatches are rejected
by the runtime.

.. autoclass:: supy.SUEWSCheckpoint
   :members: from_file, to_file
   :undoc-members:


Related Documentation
---------------------

- :doc:`/data-structures/df_state` - Complete state variable reference
- :doc:`/data-structures/df_forcing` - Forcing variable reference
- :doc:`/data-structures/df_output` - Output variable reference
- :ref:`suews_checkpoint` - Typed checkpoint restart artefact
- :doc:`/auto_examples/tutorial_01_quick_start` - Interactive tutorial with DataFrame examples
