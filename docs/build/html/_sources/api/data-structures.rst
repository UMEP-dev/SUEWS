.. _api_data_structures:

Data Structures
===============

SUEWS uses pandas DataFrames as the primary data structure for all inputs and outputs, enabling powerful data analysis and manipulation.

Core Data Objects
-----------------

State DataFrame
~~~~~~~~~~~~~~~

The state DataFrame contains model state variables and parameters for each grid cell.

:doc:`/data-structures/df_state` - Complete reference

Forcing DataFrame
~~~~~~~~~~~~~~~~~

The forcing DataFrame contains meteorological forcing data (temperature, radiation, wind, etc.).

:doc:`/data-structures/df_forcing` - Complete reference

Output DataFrame
~~~~~~~~~~~~~~~~

The output DataFrame contains simulation results and diagnostics.

:doc:`/data-structures/df_output` - Complete reference

Integration with pandas
------------------------

All SUEWS data uses pandas DataFrames with proper indexing, allowing powerful data analysis:

.. code-block:: python

   from supy import SUEWSSimulation

   # Load sample data and run simulation
   sim = SUEWSSimulation.from_sample_data()
   sim.run()

   # Analyse results using pandas
   monthly_temp = sim.results['T2'].resample('M').mean()
   energy_balance = sim.results[['QE', 'QH', 'QS', 'QF']].describe()

Benefits of DataFrame Structure
--------------------------------

- **Familiar interface**: Leverage pandas' powerful data manipulation capabilities
- **Time series analysis**: Built-in resampling, rolling windows, and time-based operations
- **Easy visualisation**: Direct integration with matplotlib and seaborn
- **Data export**: Simple conversion to CSV, Excel, HDF5, and other formats
- **Memory efficient**: Sparse data structures and chunked processing support

Related Documentation
---------------------

- :doc:`io-data-structures` - Overview of key IO data structures
- `pandas documentation <https://pandas.pydata.org/docs/>`_ - Official pandas guide
- :doc:`/auto_examples/index` - Python tutorials with DataFrame examples
