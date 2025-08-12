.. _SUEWS_WithinGridWaterDist:

:nosearch:

SUEWS_WithinGridWaterDist.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUEWS_WithinGridWaterDist.txt specifies the movement of water between
surfaces within a grid/area. It allows impervious connectivity to be
taken into account.

Each row corresponds to a surface type (linked by the Code in column 1
to the `SUEWS_SiteSelect.txt` columns:
WithinGridPavedCode, WithinGridBldgsCode, …, WithinGridWaterCode). Each
column contains the fraction of water flowing from the surface type to
each of the other surface types or to runoff or the sub-surface soil
store.

.. note::

  -  The sum of each row (excluding the Code) must equal 1.
  -  Water **CANNOT** flow from one surface to that same surface, so the
     diagonal elements should be zero.
  -  The row corresponding to the water surface should be zero, as there
     is currently no flow permitted from the water surface to other
     surfaces by the model.
  -  Currently water **CANNOT** go to both runoff and soil store (i.e. it
     must go to one or the other – `Runoff` for impervious surfaces;
     `SoilStore` for pervious surfaces).

In the table below, for example,

-  All flow from paved surfaces goes to runoff;
-  90% of flow from buildings goes to runoff, with small amounts going
   to other surfaces (mostly paved surfaces as buildings are often
   surrounded by paved areas);
-  All flow from vegetated and bare soil areas goes into the sub-surface
   soil store;
-  The row corresponding to water contains zeros (as it is currently not
   used).

.. DON'T manually modify the csv file below
.. as it is always automatically regenrated by each build:
.. edit the item descriptions in file `Input_Options.rst`

.. csv-table::
  :file: csv-table/SUEWS_WithinGridWaterDist.csv
  :header-rows: 1


.. only:: html

    An example `SUEWS_WithinGridWaterDist.txt` can be found below:

    .. literalinclude:: sample-table/SUEWS_WithinGridWaterDist.txt

.. only:: latex

    An example `SUEWS_WithinGridWaterDist.txt` can be found in the online version.
