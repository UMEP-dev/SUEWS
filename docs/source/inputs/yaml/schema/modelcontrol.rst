Modelcontrol
============

**Parameters:**

.. option:: tstep <int>

   Time step in seconds for model calculations

   :Default: ``300``

.. option:: forcing_file <RefValue[str]>

   Path to meteorological forcing data file

   :Default: ``forcing.txt``

.. option:: kdownzen <RefValue[int] (Optional)>

   Use zenithal correction for downward shortwave radiation

   :Default: Not specified

.. option:: output_file <str>

   Path to model output file

   :Default: ``'output.txt'``

.. option:: diagnose <int>

   Level of diagnostic output (0=none, 1=basic, 2=detailed)

   :Default: ``0``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
