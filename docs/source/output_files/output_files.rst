.. _output_files:

Output files
============

Runtime diagnostic information
------------------------------

.. _problems.txt:

Error messages: problems.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If there are problems running the program serious error messages will be written to problems.txt.

-  Serious problems will usually cause the program to stop after writing the error message. If this is the case, the last line of `problems.txt` will contain a non-zero number (the error code).
-  If the program runs successfully, problems.txt file ends with::

    Run completed.
    0

SUEWS has a large number of error messages included to try to capture
common errors to help the user determine what the problem is. If you
encounter an error that does not provide an error message please capture
the details so we can hopefully provide better error messages in future.

See `Troubleshooting` section for help solving
problems. If the file paths are not correct the program will return an
error when run (see `Workflow`).

.. _warnings.txt:

Warning messages: warnings.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  If the program encounters a more minor issue it will not stop but a
   warning may be written to warnings.txt. It is advisable to check the
   warnings to ensure there is not a more serious problem.
-  The warnings.txt file can be large (over several GBs) given warning
   messages are written out during a large scale simulation, you can use
   :code:`tail`/:code:`head` to view the ending/starting part without opening
   the whole file on Unix-like systems (Linux/mac OS), which may slow
   down your system.
-  To prevent warnings.txt from being written, set :option:`SuppressWarnings`
   to 1 in `RunControl.nml`.
-  Warning messages are usually written with a grid number, timestamp
   and error count. If the problem occurs in the initial stages (i.e.
   before grid numbers and timestamps are assigned, these are printed as
   00000).

.. _file_choices:

Summary of model parameters: SS_FileChoices.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For each run, the model parameters specified in the input files are written out to the file SS_FileChoices.txt.

Model output files
------------------

Output Format Options
~~~~~~~~~~~~~~~~~~~~~

SUEWS supports two output formats:

1. **Text format** (default): Traditional tab-delimited text files

   - One file per year and output group
   - Human-readable format
   - Compatible with spreadsheet software
   - Larger file sizes

2. **Parquet format**: Modern columnar storage format

   - All data in two files (output and state)
   - Typically 70-80% smaller file sizes (2-5x compression)
   - Much faster to read in Python/R/MATLAB
   - Requires specific libraries to read

The output format is configured in the YAML file. See :doc:`../inputs/yaml/index` for configuration details.

.. note:: Temporal information in output files (i.e., ``iy``, ``id``, ``it`` and ``imin`` if existing) are in **local time** (i.e. consistent with :ref:`met_forcing`) and indicate the ending timestamp of corresponding periods: e.g. for hourly data, ``2021-09-12 13:00`` indicates a record for the period between ``2021-09-12 12:00`` (inclusive) and ``2021-09-12 13:00`` (exclusive).


Text Format Output Files
~~~~~~~~~~~~~~~~~~~~~~~~

When using text format (default), SUEWS produces the following output files:

SSss_YYYY_SUEWS_TT.txt
^^^^^^^^^^^^^^^^^^^^^^

SUEWS produces the main output file (SSss_YYYY_SUEWS_tt.txt) with time resolution (TT min) set by :option:`ResolutionFilesOut` in `RunControl.nml`.

Before these main data files are written out, SUEWS provides a summary of the column names, units and variables included in the file Ss_YYYY_TT_OutputFormat.txt (one file per run).

The variables included in the main output file are determined according to :option:`WriteOutOption` set in :ref:`RunControl.nml`.

**Surface Temperature Variables:**

- **Ts**: Bulk surface temperature (°C) - area-weighted average temperature of all surface types within the grid, used in energy balance calculations and radiation schemes
- **Ts_[Surface]**: Surface temperatures (°C) for specific surface types (e.g., Ts_Paved, Ts_Bldgs, Ts_Grass) available in both EHC and debug output groups

.. note::
   **Consistent Naming**: All surface temperature variables now use the ``Ts`` prefix consistently across all output groups. The same ``Ts_[Surface]`` variables appear in both EHC and debug output. For detailed surface temperatures by urban facet (walls, roofs, ground layers), see the :ref:`ESTM output file <SSss_YYYY_ESTM_TT.txt>` which provides 5-layer temperature profiles for different surface elements.

.. csv-table::
  :file: SSss_YYYY_SUEWS_TT.csv
  :header-rows: 1
  :widths: auto


SSss_DailyState.txt
^^^^^^^^^^^^^^^^^^^

Contains information about the state of the surface and soil and
vegetation parameters at a time resolution of one day. One file is
written for each grid so it may contain multiple years.

.. csv-table::
  :file: SSss_DailyState.csv
  :header-rows: 1
  :widths: auto



InitialConditionsSSss_YYYY.nml
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

At the end of the model run (or the end of each year in the model run) a new InitialConditions file is written out (to the input folder) for each grid, see `Initial_Conditions`

SSss_YYYY_snow_TT.txt
^^^^^^^^^^^^^^^^^^^^^

SUEWS produces a separate output file for snow (when :option:`SnowUse` = 1 in `RunControl.nml`) with details for each surface type.

File format of SSss_YYYY_snow_TT.txt

.. csv-table::
  :file: SSss_YYYY_snow_TT.csv
  :header-rows: 1
  :widths: auto

SSss_YYYY_RSL_TT.txt
^^^^^^^^^^^^^^^^^^^^

SUEWS produces a separate output file for wind, temperature and humidity profiles in the roughness sublayer at 30 levels (see :ref:`rsl_mod` level details).

File format of SSss_YYYY_RSL_TT.txt:

.. csv-table::
  :file: SSss_YYYY_RSL_TT.csv
  :header-rows: 1
  :widths: auto

SSss_YYYY_BL_TT.txt
^^^^^^^^^^^^^^^^^^^

Meteorological variables modelled by CBL portion of the model are output in to this file created for each day with time step (see :ref:`CBL input files`).

.. csv-table::
  :file: SSss_YYYY_BL_TT.csv
  :header-rows: 1
  :widths: auto


.. TODO: #63 add BEERS output description based on SOLWEIG output
.. SOLWEIG is fully removed since 2019a

.. SOLWEIGpoiOut.txt
.. ~~~~~~~~~~~~~~~~~

.. Calculated variables from POI, point of interest (row, col) stated in
.. `SOLWEIGinput.nml`.

.. SOLWEIG model output file format: SOLWEIGpoiOUT.txt


.. .. csv-table::
..   :file: SOLWEIGpoiOut.csv
..   :header-rows: 1
..   :widths: auto



SSss_YYYY_ESTM_TT.txt
^^^^^^^^^^^^^^^^^^^^^

If the ESTM model option is run, the following output file is created.

.. note:: First time steps of storage output could give NaN values during the initial converging phase.

**ESTM Surface Temperature Variables**

The ESTM model calculates detailed surface temperatures for different urban facets:

**Temperature Layers (5 layers each):**
   - **Twall1-5**: Wall temperatures from outer-most (1) to inner-most (5) layer
   - **Troof1-5**: Roof temperatures from outer-most (1) to inner-most (5) layer  
   - **Tground1-5**: Ground temperatures from outer-most (1) to inner-most (5) layer
   - **Tibld1-5**: Internal building element temperatures

**Key Temperature Variables:**
   - **Tabld**: Indoor air temperature within buildings

.. note::
   **Surface Temperature Convention**: ESTM uses detailed layer-specific temperatures (Twall1-5, Troof1-5, etc.) rather than the bulk `Tsurf` variable found in main SUEWS output. The layer temperatures provide much more detailed thermal analysis of urban facets.

**Storage Heat Fluxes:**
   - **QSnet**: Net storage heat flux (sum of all components)
   - **QSwall/QSroof/QSground**: Component-specific storage fluxes
   - **QSair**: Storage heat flux into air
   - **QSibld**: Storage heat flux into internal building elements

.. note::
   These detailed temperature profiles enable analysis of heat transfer through urban facets and are particularly valuable for:
   
   - Building energy assessment
   - Urban heat island analysis  
   - Validation against thermal imaging data
   - Surface temperature pattern studies

ESTM output file format

.. csv-table::
  :file: SSss_YYYY_ESTM_TT.csv
  :header-rows: 1
  :widths: auto


SSss_YYYY_SPARTACUS_TT.txt
^^^^^^^^^^^^^^^^^^^^^^^^^^

If the SPARTACUS model option is run, the following output file is created.


SPARTACUS output file format

.. csv-table::
  :file: SSss_YYYY_SPARTACUS_TT.csv
  :header-rows: 1
  :widths: auto


Parquet Format Output Files
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note:: The parquet output format was introduced alongside the YAML input format. It is only available when using YAML configuration files, not with the legacy namelist format.

When using parquet format, SUEWS produces two output files containing all simulation data:

SSss_SUEWS_output.parquet
^^^^^^^^^^^^^^^^^^^^^^^^^

Contains all output data from the simulation in a single file:

- All output groups (SUEWS, DailyState, ESTM, RSL, BL, snow, debug) are included
- All years of simulation data are stored together
- Data is stored in columnar format for efficient compression and fast queries
- Multi-index structure preserves grid and temporal information

Typical file size reduction:

- Parquet files are typically 70-80% smaller than equivalent text files
- Provides 2-5x compression compared to uncompressed CSV/text files
- Exact compression ratio depends on data characteristics and patterns

SSss_SUEWS_state_final.parquet
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Contains the final model state for all grids:

- Used for restart runs
- Contains all state variables at the end of simulation
- Preserves the full state structure for seamless continuation

Reading Parquet Files
^^^^^^^^^^^^^^^^^^^^^

Example Python code to read parquet output::

   import pandas as pd
   
   # Read output data
   df_output = pd.read_parquet('London_KCL_SUEWS_output.parquet')
   
   # Access specific group (e.g., SUEWS variables)
   df_suews = df_output['SUEWS']
   
   # Access specific variable
   qh = df_output[('SUEWS', 'QH')]

