.. _parquet_output_format:

Parquet Format Output
=====================

.. versionadded:: 2025.10.15
   See :ref:`release notes <new_2025.10.15>` for details.

Parquet is a modern columnar storage format that provides significant advantages for
large-scale SUEWS simulations.

.. note::

   Parquet output is only available when using YAML configuration files,
   not with the legacy namelist format.


Output Files
------------

When using parquet format, SUEWS produces two output files:

- **SSss_SUEWS_output.parquet** - All simulation output in a single file, including
  all output groups and years. See :doc:`variables/index` for variable details.

- **SSss_SUEWS_state_final.parquet** - Final model state for restart runs.
  See :ref:`State Persistence <state-persistence>` for details.


Reading Parquet Files
---------------------

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

For other languages (R, MATLAB, Julia), see the
`Apache Parquet documentation <https://parquet.apache.org/>`_.


Configuration
-------------

To enable parquet output in your YAML configuration:

.. code-block:: yaml

   model:
     control:
       output_file:
         format: parquet
         freq: 3600

See :doc:`../inputs/yaml/index` for complete configuration options.


Why Parquet?
------------

Parquet format offers several advantages over traditional text output:

- **70-80% smaller file sizes** compared to text format
- **2-5x compression** compared to uncompressed CSV/text
- **Faster reading** in data analysis tools
- **All data in two files** (output and state) rather than many separate files
- **Multi-year data** stored together efficiently

**Comparison with Text Format:**

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
