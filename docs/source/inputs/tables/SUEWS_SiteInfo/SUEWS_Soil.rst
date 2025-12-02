.. _SUEWS_Soil:



SUEWS_Soil.txt
~~~~~~~~~~~~~~

SUEWS_Soil.txt specifies the characteristics of the sub-surface soil below each of the non-water surface types (Paved, Bldgs, EveTr, DecTr, Grass, BSoil).
The model does not have a soil store below the water surfaces.
Note that these sub-surface soil stores are different to the bare soil/unmamnaged surface cover type.
Each of the non-water surface types need to link to soil characteristics specified here.
If the soil characteristics are assumed to be the same for all surface types, use a single code value to link the characteristics here with the SoilTypeCode columns in `SUEWS_NonVeg.txt` and `SUEWS_Veg.txt`.

Soil moisture can either be provided using observational data in the met
forcing file (the `xsmd` column when `SMDMethod` = 1 or 2 in `RunControl.nml`) together with additional soil observation metadata below, or modelled by SUEWS (`SMDMethod` = 0 in `RunControl.nml`).

.. note::

   **Observed Soil Moisture Configuration**

   When ``SMDMethod`` is set to 1 (volumetric) or 2 (gravimetric), you must provide soil observation metadata.
   Since observed soil moisture is a single point measurement, this is a **site-level** property (not per-surface).

   **Preferred approach (YAML configuration)**:

   Set the ``soil_observation`` block in site properties:

   .. code-block:: yaml

      site:
        properties:
          soil_observation:
            depth: 200          # sensor depth [mm]
            smcap: 0.4          # saturated moisture at sensor [fraction]
            soil_not_rocks: 0.8 # soil fraction (no rocks) [0-1]
            bulk_density: 1.2   # soil bulk density [g/cm³]

   **Legacy approach (SUEWS_Soil.txt)**:

   Set the following fields for **any one** non-water surface (Paved, Bldgs, EveTr, DecTr, Grass, or BSoil):

   - ``OBS_SMDepth`` – depth of the instrumented soil layer [mm]
   - ``OBS_SMCap`` – maximum observed soil moisture (volumetric or gravimetric)
   - ``OBS_SoilNotRocks`` – fraction of the sampled volume that is soil (not rocks)
   - ``SoilDensity`` – soil bulk density (g cm\ :sup:`-3`)

   SuPy searches surfaces 0–5 in order and uses the first one with complete metadata.

   These properties are used to convert the observed values in ``xsmd`` to a soil moisture deficit before they are passed to the SUEWS kernel.


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
