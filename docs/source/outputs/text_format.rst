.. _text_output_format:

Text Format Output
==================

Since version ``2025.10.15`` (see :ref:`release notes <new_2025.10.15>`), text
format output is organised as tab-delimited simulation files by output group and
simulation year. Current object-oriented workflows also save a typed checkpoint
JSON file for continuation runs:

- **Simulation output files** - Tab-delimited files organised by output group and
  simulation year. See :ref:`Output Groups <output-groups>` for details.

- **Restart checkpoint** - A JSON file (``{site}_SUEWS_checkpoint.json``)
  containing typed runtime state for continuation runs. See
  :ref:`State Persistence <state-persistence>`.

.. note::

   Previous SUEWS versions (prior to ``2025.10.15``) also produced legacy file formats.
   Some features are retained in the modern format (e.g., output level control via
   ``groups``), while others have been adapted:

   - **State persistence**: Legacy ``InitialConditionsSSss_YYYY.nml`` files are
     replaced by typed checkpoint JSON in new object-oriented workflows.
     ``df_state_SSss.csv`` remains available for legacy and developer workflows.
   - **Error/warning messages**: Legacy ``problems.txt`` and ``warnings.txt`` files
     are no longer written; diagnostics are emitted to stdout/stderr and handled by
     the Python runtime logger (SuPy).

   See :ref:`Legacy Features <legacy-output-control>` for migration details.


File Naming Convention
----------------------

Output files follow the naming pattern::

    SSss_YYYY_GROUP_TT.txt

Where:

- **SS** - Site code
- **ss** - Grid number
- **YYYY** - Simulation year
- **GROUP** - Output group name (e.g., SUEWS, snow, ESTM)
- **TT** - Time resolution in minutes

**Examples:**

.. list-table::
   :header-rows: 1
   :widths: 40 60

   * - Filename
     - Meaning
   * - ``Kc01_2012_SUEWS_60.txt``
     - Site "Kc", grid 01, year 2012, main SUEWS output, 60-min resolution
   * - ``London05_2021_snow_30.txt``
     - Site "London", grid 05, year 2021, snow module output, 30-min resolution
   * - ``Sm12_2020_ESTM_60.txt``
     - Site "Sm", grid 12, year 2020, ESTM output, hourly resolution
   * - ``Kc01_DailyState.txt``
     - Site "Kc", grid 01, daily state (no year/resolution - one file spans all years)


.. _output-groups:

Output Groups
-------------

SUEWS organises output variables into logical groups. The groups included in your
output are controlled by the ``groups`` parameter in your YAML configuration:

.. code-block:: yaml

   model:
     control:
       output:
         format: txt
         freq: 3600
         groups:
           - SUEWS
           - DailyState
           - snow  # if snow module enabled

Available output groups:

**Core Groups**

- :ref:`SUEWS <output-suews>` - Main energy and water balance outputs
- :ref:`DailyState <output-dailystate>` - Daily state variables

**Optional Groups** (require specific modules to be enabled)

- :ref:`snow <output-snow>` - Snow module outputs
- :ref:`RSL <output-rsl>` - Roughness Sublayer profiles
- :ref:`BL <output-bl>` - Boundary Layer outputs
- :ref:`ESTM <output-estm>` - Element Surface Temperature Model
- :ref:`SPARTACUS <output-spartacus>` - SPARTACUS radiation model (experimental)

**Development**

- ``debug`` - Debugging diagnostics (not for production use)


Group Details
^^^^^^^^^^^^^

.. _output-suews:

**SUEWS** - ``SSss_YYYY_SUEWS_TT.txt``
   The primary output file containing:

   - Energy balance components (QH, QE, QS, etc.)
   - Water balance components (rain, irrigation, runoff)
   - Meteorological variables (temperature, humidity, wind)
   - Surface state variables
   - **Ts**: Bulk surface temperature (area-weighted average)
   - **Ts_[Surface]**: Surface-specific temperatures (e.g., Ts_Paved, Ts_Bldgs)

.. _output-dailystate:

**DailyState** - ``SSss_DailyState.txt``
   Daily-resolution state information:

   - Surface and soil state
   - Vegetation parameters
   - One file per grid (may contain multiple years)

   .. note::

      **Timestamp Convention for DailyState**

      DailyState uses a different timestamp labelling convention than other output
      groups to improve readability. While other groups label timestamps with the
      END of each period (e.g., data for January 1st labelled as "Jan 2 00:00"),
      DailyState labels with the START of each day (e.g., data for January 1st
      labelled as "Jan 1").

      This means the row labelled "2012-01-15" in DailyState contains the model
      state at the end of January 15th, making the output more intuitive to read.

      When combining DailyState with other groups (e.g., using
      :meth:`SUEWSOutput.resample() <supy.SUEWSOutput.resample>`), the date
      ranges may differ by one day at the boundaries.

.. _output-snow:

**snow** - ``SSss_YYYY_snow_TT.txt``
   Produced when snow module is enabled:

   - Snow depth per surface type
   - Snow water equivalent
   - Snow melt rates
   - Snow cover fraction

.. _output-rsl:

**RSL** - ``SSss_YYYY_RSL_TT.txt``
   Roughness Sublayer vertical profiles at 30 levels:

   - Wind speed profiles
   - Temperature profiles
   - Humidity profiles

   See :ref:`rsl_mod` for level details.

.. _output-bl:

**BL** - ``SSss_YYYY_BL_TT.txt``
   Convective Boundary Layer model outputs:

   - Boundary layer height
   - Mixed layer properties
   - Entrainment fluxes

   Created for each day with the CBL model timestep.

.. _output-estm:

**ESTM** - ``SSss_YYYY_ESTM_TT.txt``
   Element Surface Temperature Model outputs.

   .. note::

      ESTM is fully available in the current version. Include ``ESTM`` in your
      ``output.groups`` configuration to enable this output.

   *Temperature Layers (5 layers each):*
      - **Twall1-5**: Wall temperatures (outer to inner layer)
      - **Troof1-5**: Roof temperatures (outer to inner layer)
      - **Tground1-5**: Ground temperatures (outer to inner layer)
      - **Tibld1-5**: Internal building element temperatures

   *Key Variables:*
      - **Tabld**: Indoor air temperature
      - **QSnet**: Net storage heat flux
      - **QSwall/QSroof/QSground**: Component-specific storage fluxes

   .. note::

      First timesteps may show NaN values during the initial convergence phase.

.. _output-spartacus:

**SPARTACUS** - ``SSss_YYYY_SPARTACUS_TT.txt``
   SPARTACUS radiation model outputs (experimental):

   - Multi-layer radiation fluxes
   - Urban canopy radiation interactions
   - Sky view factors


.. _state-persistence:

State Persistence
-----------------

SUEWS saves typed runtime state for continuation runs using checkpoint JSON.

At the end of each object-oriented simulation, ``sim.run()`` exposes
``output.checkpoint`` and ``sim.save(...)`` writes
``{site}_SUEWS_checkpoint.json``. The checkpoint contains:

- Backend runtime state keyed by grid ID
- SUEWS/SuPy version metadata
- The last forcing timestamp represented by the checkpoint

This file can be used to:

1. **Restart simulations** - Continue from a saved state
2. **Chain simulations** - Use end state as input for subsequent runs
3. **Preserve typed backend state** - Avoid lossy DataFrame restart conversion

The checkpoint is intentionally only the typed runtime state. To continue a run,
load the same YAML configuration, attach the next forcing period, and run from
``SUEWSSimulation.from_checkpoint(...)``.

``df_state_SSss.csv`` and ``df_state_final`` remain legacy/developer-facing
DataFrame structures for backwards compatibility and state inspection. They are
not the preferred restart artefact for new object-oriented workflows. See
`SUEWSCheckpoint: typed restart state <../api/io-data-structures.html#suews-checkpoint>`_
for details.


.. _legacy-output-control:

Legacy Features (Deprecated)
----------------------------

.. deprecated:: 2025.10.15

   The features in this section are deprecated and retained for backwards
   compatibility only. Use the YAML-based :ref:`OutputConfig <outputconfig>` instead.

Legacy Supplementary Files
^^^^^^^^^^^^^^^^^^^^^^^^^^

**Ss_YYYY_TT_OutputFormat.txt**
   In the legacy text format (using ``RunControl.nml``), SUEWS generates this file
   containing column names, units, and variable descriptions. This file is **not
   generated** when using the modern YAML configuration - use :doc:`variables/index`
   instead for variable reference.

Legacy Output Level Control
^^^^^^^^^^^^^^^^^^^^^^^^^^^

**WriteOutOption** (from ``RunControl.nml``)
   The old ``WriteOutOption`` values (0, 1, 2) controlled output verbosity.
   In the modern approach, use the ``groups`` parameter to explicitly select
   which output groups to include:

   .. code-block:: yaml

      # Equivalent to old WriteOutOption=1 (standard output)
      model:
        control:
          output:
            format: txt
            groups:
              - SUEWS
              - DailyState

Legacy Initial Conditions Output
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**InitialConditionsSSss_YYYY.nml**
   The legacy namelist-based initial conditions files are deprecated.
   Use checkpoint JSON for new object-oriented restart workflows (see
   :ref:`State Persistence <state-persistence>`). ``df_state_SSss.csv`` remains
   a legacy/developer compatibility file.

See :doc:`variables/index` for the complete list of output variables.

.. note::

   For detailed column-by-column mappings showing legacy column numbers and
   WriteOutOption mappings, see the :doc:`legacy text column reference <legacy_text_columns>`.
