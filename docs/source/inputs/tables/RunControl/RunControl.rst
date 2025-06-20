.. _RunControl:

RunControl.nml
--------------

The file **RunControl.nml** is a namelist that specifies the options for the model run.
It must be located in the same directory as the executable file.

A sample file of **RunControl.nml** looks like

.. literalinclude:: RunControl.nml


.. note::
     - In *Linux* and *Mac*, please add an empty line after the end slash.
     - The file is not case-sensitive.
     - The parameters and variables can appear in any order.


The parameters and their setting instructions are provided through the links below:


* :ref:`scheme_options`

      .. hlist::
        + :option:`CBLUse`
        + :option:`SnowUse`
        + :option:`NetRadiationMethod`
        + :option:`BaseTMethod`
        + :option:`EmissionsMethod`
        + :option:`StorageHeatMethod`
        + :option:`OHMIncQF`
        + :option:`StabilityMethod`
        + :option:`RoughLenHeatMethod`
        + :option:`RoughLenMomMethod`
        + :option:`SMDMethod`
        + :option:`WaterUseMethod`
        + :option:`DiagMethod`


* :ref:`File_related_options`

      .. hlist::
        + :option:`FileCode`
        + :option:`FileInputPath`
        + :option:`FileOutputPath`
        + :option:`MultipleMetFiles`
        + :option:`MultipleInitFiles`
        + :option:`MultipleESTMFiles`
        + :option:`KeepTstepFilesIn`
        + :option:`KeepTstepFilesOut`
        + :option:`WriteOutOption`
        + :option:`SuppressWarnings`


* :ref:`Time_related_options`

      .. hlist::
        + :option:`Tstep`
        + :option:`ResolutionFilesIn`
        + :option:`ResolutionFilesInESTM`
        + :option:`ResolutionFilesOut`


* :ref:`Options_related_to_disaggregation_of_input_data`

      .. hlist::
        + :option:`DisaggMethod`
        + :option:`KdownZen`
        + :option:`RainDisaggMethod`
        + :option:`RainAmongN`
        + :option:`MultRainAmongN`
        + :option:`MultRainAmongNUpperI`
        + :option:`DisaggMethodESTM`


.. toctree::
   :maxdepth: 1
   :hidden:

   scheme_options
   Time_related_options
   File_related_options
   Options_related_to_disaggregation_of_input_data
