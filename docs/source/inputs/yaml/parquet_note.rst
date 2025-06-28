.. _parquet_note:

About Parquet Output Format
===========================

Parquet is a columnar storage format that SUEWS supports as an alternative to traditional text output files.

What is Parquet?
----------------

Apache Parquet is an open-source columnar data format widely used in data science and analytics. It stores data by column rather than by row, enabling efficient compression and fast queries.

Installation
------------

To use Parquet output, install PyArrow::

   pip install pyarrow

Reading Parquet Files
---------------------

**Python**::

   import pandas as pd
   df = pd.read_parquet('TestSite_SUEWS_output.parquet')

**R**::

   library(arrow)
   df <- read_parquet('TestSite_SUEWS_output.parquet')

**MATLAB** (R2019a+)::

   data = parquetread('TestSite_SUEWS_output.parquet');

Key Differences from Text Output
---------------------------------

- **Single file**: All output data in one file (vs multiple text files per year/group)
- **Binary format**: Not human-readable (use the code examples above to read)
- **Smaller size**: Typically 50-80% smaller than equivalent CSV files
- **Faster loading**: Especially when reading specific columns

For more information about Parquet: https://parquet.apache.org/