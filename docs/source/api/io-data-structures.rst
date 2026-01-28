.. _io_data_structures:

Key IO Data Structures
======================

Introduction
------------

The Python API uses four key pandas DataFrame structures for simulation input and output:

.. code-block:: python

   from supy import SUEWSSimulation

   # Load sample data and run simulation
   sim = SUEWSSimulation.from_sample_data()
   sim.run()

   # Access the data structures
   df_state_init = sim.df_state_init   # Input: initial states
   df_forcing = sim.df_forcing         # Input: forcing data
   df_output = sim.results             # Output: simulation results
   df_state_final = sim.df_state_final # Output: final states

**Input DataFrames:**

- ``df_state_init``: Model initial states and configuration parameters
- ``df_forcing``: Meteorological forcing data time series

**Output DataFrames:**

- ``df_output`` (or ``sim.results``): Model output results for scientific analysis
- ``df_state_final``: Model final states, usable as initial conditions for subsequent simulations


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

This structure facilitates reuse as initial model states for subsequent simulations:

- **Runtime diagnostics**: Save intermediate states with ``save_state=True`` in ``run_supy``
- **Chained simulations**: Use end state as initial conditions for future runs starting
  at the ending time of previous runs

The meanings of state variables in ``df_state_final`` are the same as in ``df_state_init``,
documented in :doc:`/data-structures/df_state`.


Related Documentation
---------------------

- :doc:`/data-structures/df_state` - Complete state variable reference
- :doc:`/data-structures/df_forcing` - Forcing variable reference
- :doc:`/data-structures/df_output` - Output variable reference
- :doc:`/auto_examples/basic/tutorial_01_quick_start` - Interactive tutorial with DataFrame examples
