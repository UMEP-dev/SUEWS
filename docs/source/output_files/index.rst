.. _output_files:

Output Files
============

SUEWS produces output data in two formats, each with different trade-offs:

- **Text format**: Human-readable, works with spreadsheet software, easier to inspect
- **Parquet format**: Smaller files (70-80% reduction), faster to read in Python/R/MATLAB

.. note:: Temporal information in output files (i.e., ``iy``, ``id``, ``it`` and ``imin`` if existing) are in **local time** (i.e. consistent with :ref:`met_forcing`) and indicate the ending timestamp of corresponding periods: e.g. for hourly data, ``2021-09-12 13:00`` indicates a record for the period between ``2021-09-12 12:00`` (inclusive) and ``2021-09-12 13:00`` (exclusive).


.. toctree::
   :maxdepth: 1
   :caption: Output Format Options

   text_format
   parquet_format

.. toctree::
   :maxdepth: 1
   :caption: Variable Documentation

   variables/index


.. toctree::
   :maxdepth: 1
   :caption: Diagnostic Files

   legacy_diagnostics
