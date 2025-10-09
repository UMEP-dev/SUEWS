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

   import supy as sp

   # Load sample data
   df_state, df_forcing = sp.load_sample_data()

   # Run simulation
   df_output, df_state_final = sp.run_supy(df_forcing, df_state)

   # Analyse results using pandas
   monthly_temp = df_output['T2'].resample('M').mean()
   energy_balance = df_output[['QE', 'QH', 'QS', 'QF']].describe()

Benefits of DataFrame Structure
--------------------------------

- **Familiar interface**: Leverage pandas' powerful data manipulation capabilities
- **Time series analysis**: Built-in resampling, rolling windows, and time-based operations
- **Easy visualisation**: Direct integration with matplotlib and seaborn
- **Data export**: Simple conversion to CSV, Excel, HDF5, and other formats
- **Memory efficient**: Sparse data structures and chunked processing support

Related Documentation
---------------------

- `pandas documentation <https://pandas.pydata.org/docs/>`_ - Official pandas guide
- :doc:`/tutorials/python/tutorial` - Python tutorials with DataFrame examples
