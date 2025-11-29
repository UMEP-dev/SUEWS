.. _text_output_format:

Text Format Output
==================

Since version ``2025.10.15`` (see :ref:`release notes <new_2025.10.15>`), when using
text format (default), SUEWS produces two types of output files:

1. **Simulation output files** - Tab-delimited files organised by output group and
   simulation year. See :ref:`Output Groups <output-groups>` for details.

2. **State persistence file** - A CSV file (``df_state_SSss.csv``) containing model
   state for restart and analysis. See :ref:`State Persistence <state-persistence>`.

.. note::

   Previous SUEWS versions (prior to ``2025.10.15``) also produced legacy file formats.
   Some features are retained in the modern format (e.g., output level control via
   ``groups``), while others have been adapted:

   - **State persistence**: Legacy ``InitialConditionsSSss_YYYY.nml`` files replaced
     by modern ``df_state_SSss.csv``
   - **Error/warning messages**: Legacy ``problems.txt`` and ``warnings.txt`` files
     superseded by Python runtime logger

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
       output_file:
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
      ``output_file.groups`` configuration to enable this output.

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

SUEWS automatically saves model state for restart and analysis purposes using the
modern CSV-based state file format.

At the end of each simulation, SUEWS writes a ``df_state_SSss.csv`` file containing:

- Model configuration parameters
- Initial and final states for all surface types
- Simulation metadata (SUEWS version, timestamps, etc.)

This file can be used to:

1. **Restart simulations** - Continue from a saved state
2. **Analyse model state** - Inspect internal variables
3. **Chain simulations** - Use end state as input for subsequent runs

See :doc:`index` for more details on state output.


.. _legacy-output-control:

Legacy Features (Deprecated)
----------------------------

.. deprecated:: 2024

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
          output_file:
            format: txt
            groups:
              - SUEWS
              - DailyState

Legacy Initial Conditions Output
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**InitialConditionsSSss_YYYY.nml**
   The legacy namelist-based initial conditions files are deprecated.
   Use the modern ``df_state_SSss.csv`` files instead (see :ref:`State Persistence <state-persistence>`).

See :doc:`variables/index` for the complete list of output variables.

.. note::

   For detailed column-by-column mappings showing legacy column numbers and
   WriteOutOption mappings, see the :doc:`legacy text column reference <legacy_text_columns>`.
