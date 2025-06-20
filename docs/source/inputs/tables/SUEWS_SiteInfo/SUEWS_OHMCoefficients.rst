.. _SUEWS_OHMCoefficients:

SUEWS_OHMCoefficients.txt
~~~~~~~~~~~~~~~~~~~~~~~~~

OHM, the Objective Hysteresis Model  :cite:`G91`
calculates the storage heat flux as a function of net all-wave radiation
and surface characteristics.

-  For each surface, OHM requires three model coefficients (a1, a2, a3). The three should be selected as a set.
-  The **SUEWS_OHMCoefficients.txt** file provides these coefficients for each surface type.
-  A variety of values has been derived for different materials and can
   be found in the literature (see: `typical_values`).
-  Coefficients can be changed depending on:
    #. surface wetness state (wet/dry) based on the calculated surface wetness state and soil moisture.
    #. season (summer/winter) based on a 5-day running mean air temperature.
-  To use the same coefficients irrespective of wet/dry and
   summer/winter conditions, use the same code for all four OHM columns
   (`OHMCode_SummerWet`, `OHMCode_SummerDry`, `OHMCode_WinterWet` and
   `OHMCode_WinterDry`).


.. note::

    #. AnOHM (set in `RunControl.nml` by `StorageHeatMethod` = 3) does not use the coefficients specified in `SUEWS_OHMCoefficients.txt` but instead requires three parameters to be specified for each surface type (including snow): heat capacity (`AnOHM_Cp`), thermal conductivity (`AnOHM_Kk`) and bulk transfer coefficient (`AnOHM_Ch`). These are specified in `SUEWS_NonVeg.txt`, `SUEWS_Veg.txt`, `SUEWS_Water.txt` and `SUEWS_Snow.txt`. No additional files are required for AnOHM.

    #. AnOHM is under development in v2018b and should NOT be used!

.. DON'T manually modify the csv file below
.. as it is always automatically regenrated by each build:
.. edit the item descriptions in file `Input_Options.rst`

.. csv-table::
  :file: csv-table/SUEWS_OHMCoefficients.csv
  :header-rows: 1
  :widths: 5 25 5 65

.. only:: html

    An example `SUEWS_OHMCoefficients.txt` can be found below:

    .. literalinclude:: sample-table/SUEWS_OHMCoefficients.txt

.. only:: latex

    An example `SUEWS_OHMCoefficients.txt` can be found in the online version.
