.. _SUEWS_WithinGridWaterDist:



SUEWS_WithinGridWaterDist.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUEWS_WithinGridWaterDist.txt specifies the movement of water between
surfaces within a grid/area. It allows impervious connectivity to be
taken into account.

Each row corresponds to a surface type (linked by the Code in column 1
to the `SUEWS_SiteSelect.txt` columns:
WithinGridPavedCode, WithinGridBldgsCode, …, WithinGridWaterCode). Each
column contains the fraction of drainage flowing from the source surface
to each of the other surface types or to runoff.

.. note::

  -  The sum of each row (excluding the Code) must equal 1.
  -  Water **CANNOT** flow from one surface to that same surface, so the
     diagonal elements should be zero.
  -  The row corresponding to the water surface should be zero, as there
     is currently no flow permitted from the water surface to other
     surfaces by the model.
  -  For pervious surfaces (evergreen trees, deciduous trees, grass, bare
     soil), drainage first infiltrates into the soil store up to remaining
     capacity. The `Runoff` column then applies to the **remaining**
     drainage (after infiltration), with the non-runoff fraction available
     for lateral redistribution to other surfaces.
  -  For impervious surfaces, the `Runoff` column gives the drainage
     fraction routed directly to runoff.

In the table below, for example,

-  All flow from paved surfaces goes to runoff;
-  90% of flow from buildings goes to runoff, with small amounts going
   to other surfaces (mostly paved surfaces as buildings are often
   surrounded by paved areas);
-  For vegetated and bare soil areas, drainage first infiltrates into soil
   (up to capacity); any remaining drainage is then partitioned between
   runoff and redistribution using the row fractions;
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
