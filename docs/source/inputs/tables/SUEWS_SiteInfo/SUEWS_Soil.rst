.. _SUEWS_Soil:



SUEWS_Soil.txt
~~~~~~~~~~~~~~

SUEWS_Soil.txt specifies the characteristics of the sub-surface soil below each of the non-water surface types (Paved, Bldgs, EveTr, DecTr, Grass, BSoil).
The model does not have a soil store below the water surfaces.
Note that these sub-surface soil stores are different to the bare soil/unmamnaged surface cover type.
Each of the non-water surface types need to link to soil characteristics specified here.
If the soil characteristics are assumed to be the same for all surface types, use a single code value to link the characteristics here with the SoilTypeCode columns in `SUEWS_NonVeg.txt` and `SUEWS_Veg.txt`.

Soil moisture can either be provided using observational data in the met
forcing file (the ``xsmd`` column when ``SMDMethod`` = 1 or 2), or modelled by SUEWS (``SMDMethod`` = 0).

Using Observed Soil Moisture
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When using observed soil moisture (``SMDMethod`` = 1 for volumetric or 2 for gravimetric), you must:

1. Provide the ``xsmd`` column in your forcing data with observed soil moisture values
2. Configure the ``soil_observation`` block in your YAML site properties

The ``soil_observation`` block describes the sensor installation and measurement setup:

.. code-block:: yaml

   model:
     physics:
       smdmethod:
         value: 1  # 1=volumetric, 2=gravimetric

   sites:
   - name: MySite
     properties:
       soil_observation:
         depth: 200.0        # mm - sensor installation depth
         smcap: 0.40         # fraction - saturated moisture content
         soil_not_rocks: 0.9 # fraction - soil volume (not rocks)
         bulk_density: 1.3   # g/cm³ - only needed for gravimetric (SMDMethod=2)

.. note::
   The ``soil_observation`` parameters describe the **measurement setup**, not the SUEWS model's soil properties.
   These values are used to convert observed volumetric/gravimetric moisture to a soil moisture deficit (mm).

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
