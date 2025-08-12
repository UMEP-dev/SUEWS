.. _SUEWS_NonVeg:

:nosearch:

SUEWS_NonVeg.txt
~~~~~~~~~~~~~~~~

`SUEWS_NonVeg.txt` specifies the characteristics for the non-vegetated
surface cover types (`Paved`, `Bldgs`, `BSoil`) by linking codes in column 1
of `SUEWS_NonVeg.txt` to the codes specified in `SUEWS_SiteSelect.txt`
(`Code_Paved`, `Code_Bldgs`, `Code_BSoil`). Each row should correspond to a
particular surface type. For suggestions on how to complete this table,
see: `Typical Values <typical_values>`.

.. DON'T manually modify the csv file below
.. as it is always automatically regenrated by each build:
.. edit the item descriptions in file `Input_Options.rst`

.. csv-table::
  :class: longtable
  :file: csv-table/SUEWS_NonVeg.csv
  :header-rows: 1
  :widths: 5 25 5 65

.. only:: html

    An example `SUEWS_NonVeg.txt` can be found below:

    .. literalinclude:: sample-table/SUEWS_NonVeg.txt

.. only:: latex

    An example `SUEWS_NonVeg.txt` can be found in the online version.
