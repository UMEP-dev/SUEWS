.. _met_input:

Meteorological Input File
-------------------------

SUEWS is designed to run using commonly measured meteorological variables (e.g. incoming solar radiation, air temperature, relative humidity, pressure, wind speed, etc.).

When preparing this input file, please note the following:

-  Required inputs must be continuous – i.e. **gap fill** any missing data.
-  Temporal information (i.e., ``iy``, ``id``, ``it`` and ``imin``) should be in **local time** and indicate the ending timestamp of corresponding periods: e.g. for hourly data, ``2021-09-12 13:00`` indicates a record for the period between ``2021-09-12 12:00`` (inclusive) and ``2021-09-12 13:00`` (exclusive).
-  The `table <SSss_YYYY_data_tt.txt>` below gives the must-use (`MU`) and optional (`O`) additional input variables. If an optional input variable (`O`) is not available or will not be used by the model, enter ‘-999’ for this column.


-  One single meteorological file can be used for all grids (**MultipleMetFiles=0** in `RunControl.nml`, no grid number in file name) if appropriate for the study area.
-  Separate met files can be used for each grid if data are available (**MultipleMetFiles=1** in `RunControl.nml`, filename includes grid number).

-  The meteorological forcing file names should be appended with the temporal resolution in minutes: ``tt`` in ``SS_YYYY_data_tt.txt`` (or
   ``SSss_YYYY_data_tt.txt`` for multiple grids).

-  Separate met forcing files should be provided for each year.
-  Files do not need to start/end at the start/end of the year, but they must contain a whole number of days.
-  The meteorological input file should match the information given in `SUEWS_SiteSelect.txt`.
-  If a *partial year* is used that specific year must be given in SUEWS_SiteSelect.txt.
-  If *multiple years* are used, all years should be included in SUEWS_SiteSelect.txt.
-  If a *whole year* (e.g. 2011) is intended to be modelled using and hourly resolution dataset, the number of lines in the met data file should be 8760 and begin and end with::

     iy     id  it  imin
     2011   1   1   0 …
     …
     2012   1   0   0 …



SSss_YYYY_data_tt.txt
~~~~~~~~~~~~~~~~~~~~~

.. versionchanged:: v2017a
   Since v2017a forcing files no longer need to end with two rows containing ‘-9’ in the first column.


Main meteorological data file.

.. csv-table::
  :file: SSss_YYYY_data_tt.csv
  :header-rows: 1
  :widths: auto
