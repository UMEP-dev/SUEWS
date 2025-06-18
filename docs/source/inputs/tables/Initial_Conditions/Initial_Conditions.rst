.. _Initial_Conditions:

Initial Conditions file
-----------------------

To start the model, information about the conditions at the start of the
run is required. This information is provided in initial conditions
file. One file can be specified for each grid
(:option:`MultipleInitFiles=1 <MultipleInitFiles>` in
:ref:`RunControl.nml`, filename includes grid number) or,
alternatively, a single file can be specified for all grids
(MultipleInitFiles=0 in :ref:`RunControl.nml`, no grid
number in the filename). After that, a new
InitialConditionsSSss_YYYY.nml file will be written for each grid for
the following years. It is recommended that you look at these files
(written to the input directory) to check the status of various surfaces
at the end or the run. This may help you get more realistic starting
values if you are uncertain what they should be. Note this file will be
created for each year for multiyear runs for each grid. If the run
finishes before the end of the year the InitialConditions file is still
written and the file name is appended with '_EndofRun'.

A sample file of **InitialConditionsSSss_YYYY.nml** looks like

.. literalinclude:: InitialConditionsSaeve_2004.nml


The two most important pieces of information in the initial conditions
file is the soil moisture and state of vegetation at the start of the
run. This is the minimal information required; other information can be
provided if known, otherwise SUEWS will make an estimate of initial
conditions.


The parameters and their setting instructions are provided through the links below:

.. note:: Variables can be in any order


* :ref:`Soil_moisture_states`

  .. hlist::
    + :option:`SoilstorePavedState`
    + :option:`SoilstoreBldgsState`
    + :option:`SoilstoreEveTrState`
    + :option:`SoilstoreDecTrState`
    + :option:`SoilstoreGrassState`
    + :option:`SoilstoreBSoilState`

* :ref:`Vegetation_parameters`

  .. hlist::
    + :option:`LeavesOutInitially`
    + :option:`GDD_1_0`
    + :option:`GDD_2_0`
    + :option:`LAIinitialEveTr`
    + :option:`LAIinitialDecTr`
    + :option:`LAIinitialGrass`
    + :option:`albEveTr0`
    + :option:`albDecTr0`
    + :option:`albGrass0`
    + :option:`decidCap0`
    + :option:`porosity0`

* :ref:`Recent_meteorology`

  .. hlist::
    + :option:`DaysSinceRain`
    + :option:`Temp_C0`

* :ref:`Above_ground_state`

  .. hlist::
    + :option:`PavedState`
    + :option:`BldgsState`
    + :option:`EveTrState`
    + :option:`DecTrState`
    + :option:`GrassState`
    + :option:`BSoilState`
    + :option:`WaterState`

* :ref:`Snow_related_parameters`

  .. hlist::
    + :option:`SnowInitially`
    + :option:`SnowWaterPavedState`
    + :option:`SnowWaterBldgsState`
    + :option:`SnowWaterEveTrState`
    + :option:`SnowWaterDecTrState`
    + :option:`SnowWaterGrassState`
    + :option:`SnowWaterBSoilState`
    + :option:`SnowWaterWaterState`
    + :option:`SnowPackPaved`
    + :option:`SnowPackBldgs`
    + :option:`SnowPackEveTr`
    + :option:`SnowPackDecTr`
    + :option:`SnowPackGrass`
    + :option:`SnowPackBSoil`
    + :option:`SnowPackWater`
    + :option:`SnowFracPaved`
    + :option:`SnowFracBldgs`
    + :option:`SnowFracEveTr`
    + :option:`SnowFracDecTr`
    + :option:`SnowFracGrass`
    + :option:`SnowFracBSoil`
    + :option:`SnowFracWater`
    + :option:`SnowDensPaved`
    + :option:`SnowDensBldgs`
    + :option:`SnowDensEveTr`
    + :option:`SnowDensDecTr`
    + :option:`SnowDensGrass`
    + :option:`SnowDensBSoil`
    + :option:`SnowDensWater`
    + :option:`SnowAlb0`


.. toctree::
   :maxdepth: 1
   :hidden:

   Soil_moisture_states
   Vegetation_parameters
   Recent_meteorology
   Above_ground_state
   Snow_related_parameters
