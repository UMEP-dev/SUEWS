.. _SUEWS_Soil:



SUEWS_Soil.txt
~~~~~~~~~~~~~~

SUEWS_Soil.txt specifies the characteristics of the sub-surface soil below each of the non-water surface types (Paved, Bldgs, EveTr, DecTr, Grass, BSoil).
The model does not have a soil store below the water surfaces.
Note that these sub-surface soil stores are different to the bare soil/unmamnaged surface cover type.
Each of the non-water surface types need to link to soil characteristics specified here.
If the soil characteristics are assumed to be the same for all surface types, use a single code value to link the characteristics here with the SoilTypeCode columns in `SUEWS_NonVeg.txt` and `SUEWS_Veg.txt`.

Soil moisture can either be provided using observational data in the met
forcing file (the `xsmd` column when `SMDMethod` = 1 or 2 in `RunControl.nml`) and providing some soil properties here, or modelled by SUEWS (`SMDMethod` = 0 in `RunControl.nml`).


.. .. caution::
..   The option to use observational data is not operational in the current release!


.. DON'T manually modify the csv file below
.. as it is always automatically regenrated by each build:
.. edit the item descriptions in file `Input_Options.rst`

.. csv-table::
  :file: csv-table/SUEWS_Soil.csv
  :header-rows: 1
  :widths: 5 25 5 65

.. only:: html

    An example `SUEWS_Soil.txt` can be found below:

    .. literalinclude:: sample-table/SUEWS_Soil.txt

.. only:: latex

    An example `SUEWS_Soil.txt` can be found in the online version.
