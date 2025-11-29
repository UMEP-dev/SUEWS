.. _text_output_format:

Text Format Output
==================

When using text format (default), SUEWS produces tab-delimited output files organised
by output group and simulation year.

File Naming Convention
----------------------

Output files follow the naming pattern::

    SSss_YYYY_GROUP_TT.txt

Where:

- **SS** - Site code
- **ss** - Grid number
- **YYYY** - Simulation year
- **GROUP** - Output group name (e.g., SUEWS, snow, ESTM)
- **TT** - Time resolution in minutes (set by :option:`ResolutionFilesOut`)

Main Output File: SSss_YYYY_SUEWS_TT.txt
----------------------------------------

The primary output file contains the main SUEWS outputs including:

- Energy balance components (QH, QE, QS, etc.)
- Water balance components (rain, irrigation, runoff)
- Meteorological variables (temperature, humidity, wind)
- Surface state variables

Before the main data files are written, SUEWS provides a summary of column names, units,
and variables in ``Ss_YYYY_TT_OutputFormat.txt``.

The variables included are determined by :option:`WriteOutOption` in :ref:`RunControl.nml`.

**Surface Temperature Variables:**

- **Ts**: Bulk surface temperature - area-weighted average of all surface types
- **Ts_[Surface]**: Surface temperatures for specific types (e.g., Ts_Paved, Ts_Bldgs)


Daily State: SSss_DailyState.txt
--------------------------------

Contains daily-resolution state information:

- Surface and soil state
- Vegetation parameters
- One file per grid (may contain multiple years)


Snow Output: SSss_YYYY_snow_TT.txt
----------------------------------

Produced when :option:`SnowUse` = 1, containing:

- Snow depth per surface type
- Snow water equivalent
- Snow melt rates
- Snow cover fraction


RSL Output: SSss_YYYY_RSL_TT.txt
--------------------------------

Roughness Sublayer vertical profiles at 30 levels:

- Wind speed profiles
- Temperature profiles
- Humidity profiles

See :ref:`rsl_mod` for level details.


BL Output: SSss_YYYY_BL_TT.txt
------------------------------

Convective Boundary Layer model outputs:

- Boundary layer height
- Mixed layer properties
- Entrainment fluxes

Created for each day with the CBL model timestep.


ESTM Output: SSss_YYYY_ESTM_TT.txt
----------------------------------

Element Surface Temperature Model outputs:

**Temperature Layers (5 layers each):**

- **Twall1-5**: Wall temperatures (outer to inner layer)
- **Troof1-5**: Roof temperatures (outer to inner layer)
- **Tground1-5**: Ground temperatures (outer to inner layer)
- **Tibld1-5**: Internal building element temperatures

**Key Variables:**

- **Tabld**: Indoor air temperature
- **QSnet**: Net storage heat flux
- **QSwall/QSroof/QSground**: Component-specific storage fluxes

.. note::

   First timesteps may show NaN values during the initial convergence phase.


SPARTACUS Output: SSss_YYYY_SPARTACUS_TT.txt
--------------------------------------------

SPARTACUS radiation model outputs (experimental):

- Multi-layer radiation fluxes
- Urban canopy radiation interactions
- Sky view factors


Output Level Control
--------------------

SUEWS supports multiple output levels controlled by :option:`WriteOutOption`:

- **0** - Minimal output (core variables only)
- **1** - Standard output (default)
- **2** - Extended output (includes additional diagnostics)

See :doc:`variables/index` for the complete list of variables and their output levels.


Initial Conditions Output
-------------------------

At the end of each simulation year, SUEWS writes an updated initial conditions file::

    InitialConditionsSSss_YYYY.nml

This file can be used to restart simulations from the saved state.
