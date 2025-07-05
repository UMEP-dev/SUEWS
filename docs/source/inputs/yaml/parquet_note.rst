.. _parquet_note:

About Parquet Output Format
===========================

Parquet is a columnar storage format that SUEWS supports as an alternative to traditional text output files. This feature was introduced with the YAML input format and is only available when using YAML configuration files.

What is Parquet?
----------------

Apache Parquet is an open-source columnar data format widely used in data science and analytics. It stores data by column rather than by row, enabling efficient compression and fast queries.

Configuration
-------------

To enable Parquet output in your YAML configuration::

   model:
     control:
       output_file:
         format: parquet
         freq: 3600  # Output frequency in seconds

Installation
------------

To use Parquet output, install PyArrow::

   pip install pyarrow

Reading Parquet Files
---------------------

Example Python code::

   import pandas as pd
   
   # Read the parquet file
   df = pd.read_parquet('TestSite_SUEWS_output.parquet')
   
   # The dataframe has a multi-level column index: (group, variable)
   # Access specific group
   df_suews = df['SUEWS']
   
   # Access specific variable
   qh = df[('SUEWS', 'QH')]

Key Differences from Text Output
---------------------------------

- **Single file**: All output data in one file (vs multiple text files per year/group)
- **Binary format**: Not human-readable (use the code examples above to read)
- **Smaller size**: Typically 70-80% smaller than equivalent text files (2-5x compression)
- **Faster loading**: Especially when reading specific columns

For more information about Parquet: https://parquet.apache.org/