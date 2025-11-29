.. _parquet_output_format:

Parquet Format Output
=====================

Parquet is a modern columnar storage format that provides significant advantages for
large-scale SUEWS simulations.

.. note::

   Parquet output is only available when using YAML configuration files,
   not with the legacy namelist format.

Advantages
----------

- **70-80% smaller file sizes** compared to text format
- **2-5x compression** compared to uncompressed CSV/text
- **Faster reading** in Python, R, and MATLAB
- **All data in two files** (output and state)
- **Multi-year data** stored together efficiently


Output Files
------------

SSss_SUEWS_output.parquet
^^^^^^^^^^^^^^^^^^^^^^^^^

Contains all simulation output in a single file:

- All output groups (SUEWS, DailyState, ESTM, RSL, BL, snow, debug, etc.)
- All years of simulation data
- Multi-index structure preserving grid and temporal information
- Columnar format for efficient compression

SSss_SUEWS_state_final.parquet
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Contains the final model state:

- All state variables at simulation end
- Full state structure for restart runs
- Grid-specific state preservation


Reading Parquet Files
---------------------

Python (pandas)
^^^^^^^^^^^^^^^

.. code-block:: python

   import pandas as pd

   # Read output data
   df_output = pd.read_parquet('London_KCL_SUEWS_output.parquet')

   # Access specific group (e.g., SUEWS variables)
   df_suews = df_output['SUEWS']

   # Access specific variable
   qh = df_output[('SUEWS', 'QH')]

   # Filter by time
   summer = df_output.loc['2012-06':'2012-08']

Python (polars)
^^^^^^^^^^^^^^^

.. code-block:: python

   import polars as pl

   # Read with polars (faster for large files)
   df = pl.read_parquet('London_KCL_SUEWS_output.parquet')

R
^

.. code-block:: r

   library(arrow)

   # Read parquet file
   df <- read_parquet('London_KCL_SUEWS_output.parquet')

MATLAB
^^^^^^

.. code-block:: matlab

   % Requires MATLAB R2019a or later
   df = parquetread('London_KCL_SUEWS_output.parquet');


Configuration
-------------

To enable parquet output in your YAML configuration:

.. code-block:: yaml

   output:
     format: parquet
     # Other output options...

See :doc:`../inputs/yaml/index` for complete configuration options.


Comparison with Text Format
---------------------------

+------------------+------------------+------------------+
| Feature          | Text Format      | Parquet Format   |
+==================+==================+==================+
| File size        | Large            | 70-80% smaller   |
+------------------+------------------+------------------+
| Read speed       | Slow             | Fast             |
+------------------+------------------+------------------+
| Human readable   | Yes              | No               |
+------------------+------------------+------------------+
| Multi-year       | Separate files   | Single file      |
+------------------+------------------+------------------+
| Spreadsheet      | Yes              | Requires library |
+------------------+------------------+------------------+
