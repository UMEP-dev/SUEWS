.. _SUEWS_Snow:

SUEWS_Snow.txt
~~~~~~~~~~~~~~

SUEWS_Snow.txt specifies the characteristics for snow surfaces when
`SnowUse=1 <SnowUse>` in `RunControl.nml`. If the snow part of
the model is not run, fill this table with ‘-999’ except for the first
(Code) column and set `SnowUse=0 <SnowUse>` in `RunControl.nml`.
For a detailed description of the variables, see Järvi et al.
(2014) :cite:`J14`.


.. DON'T manually modify the csv file below
.. as it is always automatically regenrated by each build:
.. edit the item descriptions in file `Input_Options.rst`

.. csv-table::
  :class: longtable
  :file: csv-table/SUEWS_Snow.csv
  :header-rows: 1
  :widths: 5 25 5 65

.. only:: html

    An example `SUEWS_Snow.txt` can be found below:

    .. literalinclude:: sample-table/SUEWS_Snow.txt

.. only:: latex

    An example `SUEWS_Snow.txt` can be found in the online version.
