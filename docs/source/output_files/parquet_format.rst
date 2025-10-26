.. _output_parquet_format:

Parquet Format Output Files
============================

.. note:: The parquet output format was introduced alongside the YAML input format. It is only available when using YAML configuration files, not with the legacy namelist format.

When using parquet format, SUEWS produces two output files containing all simulation data:

SSss_SUEWS_output.parquet
-------------------------

Contains all output data from the simulation in a single file:

- All output groups (SUEWS, DailyState, ESTM, RSL, BL, snow, debug) are included
- All years of simulation data are stored together
- Data is stored in columnar format for efficient compression and fast queries
- Multi-index structure preserves grid and temporal information

Typical file size reduction:

- Parquet files are typically 70-80% smaller than equivalent text files
- Provides 2-5x compression compared to uncompressed CSV/text files
- Exact compression ratio depends on data characteristics and patterns

SSss_SUEWS_state_final.parquet
-------------------------------

Contains the final model state for all grids:

- Used for restart runs
- Contains all state variables at the end of simulation
- Preserves the full state structure for seamless continuation

Reading Parquet Files
---------------------

Example Python code to read parquet output::

   import pandas as pd

   # Read output data
   df_output = pd.read_parquet('London_KCL_SUEWS_output.parquet')

   # Access specific group (e.g., SUEWS variables)
   df_suews = df_output['SUEWS']

   # Access specific variable
   qh = df_output[('SUEWS', 'QH')]
