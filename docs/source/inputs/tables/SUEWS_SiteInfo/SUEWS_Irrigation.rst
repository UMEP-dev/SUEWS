.. _SUEWS_Irrigation:

SUEWS_Irrigation.txt
~~~~~~~~~~~~~~~~~~~~

External water use may be used for a wide range of reasons (e.g. cleaning roads, irrigating plants, fountains, washing cars).

SUEWS has two options for External Water use (if non-zero):

1) provide observed data in meteorological forcing file in the `Wuh` column with valid values by setting `WaterUseMethod` = 1 in `RunControl.nml`

2) a simple model that calculates daily water use from the mean daily air temperature, number of days since rain and fraction of irrigated area using automatic/manual irrigation. The user needs to supply coefficients (XXX) for these relations.

   a) sub-daily pattern of water use is detemined from the daily cycles specified in `SUEWS_Profiles.txt`.

   b) surface that the water can be applied to is specified by XX.

   c) water can pond.



.. DON'T manually modify the csv file below
.. as it is always automatically regenrated by each build:
.. edit the item descriptions in file `Input_Options.rst`

.. csv-table::
  :file: csv-table/SUEWS_Irrigation.csv
  :header-rows: 1
  :widths: 5 25 5 65

.. only:: html

    An example `SUEWS_Irrigation.txt` can be found below:

    .. literalinclude:: sample-table/SUEWS_Irrigation.txt

.. only:: latex

    An example `SUEWS_Irrigation.txt` can be found in the online version.
