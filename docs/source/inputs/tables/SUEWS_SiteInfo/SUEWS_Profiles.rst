



SUEWS_Profiles.txt
~~~~~~~~~~~~~~~~~~

`SUEWS_Profiles.txt` specifies the daily cycle of variables related to human behaviour (energy use, water use and snow clearing).
Different profiles can be specified for weekdays and weekends.
The profiles are provided at hourly resolution here;
the model will then linearly interpolate the profiles to the resolution of the model time step; some profiles may be normalized either by ``sum`` or by ``mean`` depending on the activity type while others not(see ``Normalisation method`` column of `table below`_).
Thus it does not matter whether columns 2-25 add up to, say 1, 24, or another number, because the model will eventually use the normalised values to rescale the results.

.. note::
  #. Currently, the snow clearing profiles are not interpolated as these are effectively a switch (0 for off and 1 for on).
  #. If the anthropogenic heat flux and water use are specified in the met forcing file, the energy and water use profiles are ignored.


.. _table below:

.. list-table::
    :widths: auto
    :header-rows: 1

    * - Activity
      - Description
      - Normalisation method
      - Weekday option
      - Weekend option
    * - Energy use
      - This profile, in junction with population density (`PopDensDay` and `PopDensNight`), determines the overall anthropogenic heat.
      - ``mean``
      - `EnergyUseProfWD`
      - `EnergyUseProfWE`
    * - Population density
      - This profile, in junction with human activity (`ActivityProfWD` and `ActivityProfWE`), determines the anthropogenic heat due to metabolism.
      - None
      - `PopProfWD`
      - `PopProfWE`
    * - Human activity
      - This profile, in junction with population density (`PopProfWD` and `PopProfWE`), determines the anthropogenic heat due to metabolism.
      - None
      - `ActivityProfWD`
      - `ActivityProfWE`
    * - Traffic
      - This profile determines the anthropogenic heat due to traffic.
      - ``mean``
      - `TraffProfWD`
      - `TraffProfWE`
    * - Water use (manual)
      - This profile determines the irrigation under manual operation.
      - ``sum``
      - `WaterUseProfManuWD`
      - `WaterUseProfManuWE`
    * - Water use (automatic)
      - This profile determines the irrigation under automatic operation.
      - ``sum``
      - `WaterUseProfAutoWD`
      - `WaterUseProfAutoWE`
    * - Snow removal
      - This profile determines if snow removal is conducted at the end of each hour.
      - None
      - `SnowClearingProfWD`
      - `SnowClearingProfWE`


-  Anthropogenic heat flux (weekday and weekend)
-  Water use (weekday and weekend; manual and automatic irrigation)
-  Snow removal (weekday and weekend)
-  Human activity (weekday and weekend).


.. DON'T manually modify the csv file below
.. as it is always automatically regenrated by each build:
.. edit the item descriptions in file `Input_Options.rst`

.. csv-table::
  :file: csv-table/SUEWS_Profiles.csv
  :header-rows: 1
  :widths: 5 25 5 65

.. only:: html

    An example `SUEWS_Profiles.txt` can be found below:

    .. literalinclude:: sample-table/SUEWS_Profiles.txt

.. only:: latex

    An example `SUEWS_Profiles.txt` can be found in the online version.
