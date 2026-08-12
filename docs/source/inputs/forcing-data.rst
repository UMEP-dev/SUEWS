.. _met_forcing:

Meteorological Forcing Data
============================

SUEWS requires continuous meteorological data representative of the neighbourhood scale, within the inertial sublayer (i.e. a blended response above the roughness elements of buildings and trees), to drive the urban energy and water balance calculations. This page describes the format and requirements for forcing data files.

.. toctree::
   :maxdepth: 1
   :hidden:

   /data-structures/df_forcing

.. important:: **Forcing Height**

   Forcing data must represent the urban neighbourhood as a whole, not individual buildings or trees. This requires measurements from within the inertial sublayer, where turbulent mixing produces spatially blended values. The forcing height (``z``) tells SUEWS where your data originate, enabling correct profile calculations between this reference level and the surface. In urban environments, the atmospheric boundary layer is divided into:

   - **Urban Canopy Layer (UCL)**: Within the urban canopy, among buildings and trees
   - **Roughness Sublayer (RSL)**: Extends from the surface to approximately 2-5 times the mean building/tree height; flow is spatially heterogeneous
   - **Inertial Sublayer (ISL)**: Above the RSL, where Monin-Obukhov Similarity Theory applies and fluxes are approximately constant with height

   **Guidance for choosing z:**

   - For in-situ measurements: use the actual measurement height (typically flux tower height)
   - For reanalysis data (e.g., ERA5): check the reference height of the dataset
   - For nested model output: use the height of the lowest model level above the surface

   Set the forcing height in your YAML configuration:

   .. code-block:: yaml

      sites:
        - name: "MySite"
          properties:
            z: 50.0  # Forcing height in metres

   See :input:option:`z` for full documentation, and :ref:`rsl_mod` in :doc:`/parameterisations-and-sub-models` for details on profile calculations.

Data Requirements
-----------------

**Essential Variables**

SUEWS requires the following meteorological variables. This table is a quick
preparation checklist; the generated :ref:`df_forcing_var` reference is
authoritative for units, valid ranges, requiredness, interval semantics, and
missing-value policies.

.. list-table::
   :header-rows: 1
   :widths: 20 20 20 40

   * - Variable
     - Units
     - Column Name
     - Notes
   * - Wind speed
     - m |s^-1|
     - U
     - Minimum 0.01 m |s^-1| (to avoid division by zero)
   * - Relative humidity
     - %
     - RH
     - 0-100%
   * - Air temperature
     - :math:`{}^{\circ}\mathrm{C}`
     - Tair
     - 
   * - Atmospheric pressure
     - kPa
     - pres
     - 
   * - Rainfall
     - mm
     - rain
     - Total accumulated over the interval
   * - Incoming shortwave
     - W |m^-2|
     - kdown
     - Must be :math:`\geq 0` (0 at night)

**Time Information**

Each row must include these timestamp columns. Their position is unrestricted
in a named-column file:

1. ``iy`` - Year (YYYY)
2. ``id`` - Day of year (1-365/366)
3. ``it`` - Hour (0-23)
4. ``imin`` - Minute (0-59)

File Format
-----------

**Structure**

- **Format**: Space or tab-delimited text file
- **Extension**: ``.txt``
- **Header**: Required; canonical names are matched case-insensitively
- **Missing values**: Use ``-999`` only for optional fields and inactive
  conditional fields; do not use blanks or ``NaN``

**Canonical columns**

The historical compatibility order is shown below. Modern named-column files
may use any column order:

.. code-block:: text

   iy  id  it  imin  qn  qh  qe  qs  qf  U  RH  Tair  pres  rain  kdown  snow  ldown  fcld  Wuh  xsmd  lai  kdiff  kdir  wdir

Where:

- Columns 1-4: Time stamps (required)
- Columns 5-9: Energy fluxes (requiredness depends on the selected physics)
- Columns 10-15: Essential meteorological variables (required)
- Columns 16-24: Additional conditional or optional variables

**Example**

.. code-block:: text

   iy  id  it  imin  qn  qh  qe  qs  qf  U  RH  Tair  pres  rain  kdown  snow  ldown  fcld  Wuh  xsmd  lai  kdiff  kdir  wdir
   2020  1  1   0  -999  -999  -999  -999  -999  2.1  85  5.2  101.3  0.0  0  -999  315  -999  -999  -999  -999  -999  -999  -999
   2020  1  2   0  -999  -999  -999  -999  -999  2.3  84  5.3  101.3  0.2  0  -999  318  -999  -999  -999  -999  -999  -999  -999
   2020  1  3   0  -999  -999  -999  -999  -999  2.0  86  5.1  101.2  0.0  0  -999  312  -999  -999  -999  -999  -999  -999  -999

.. _named_column_forcing:

Named-column forcing files
--------------------------

Since schema 2026.5, SUEWS reads forcing files by **column name**,
not by column position. The header line is required and its content is
matched, case-insensitively, against the canonical column list above.

* **Required (baseline)**: All timestamp and essential-variable columns listed
  above must be present and contain valid values at every timestamp.
* **Required (physics-conditional)**: Additional columns become mandatory for
  particular physics paths. Use the complete generated
  :ref:`df_forcing_requirements` table for compound and alternative
  requirements. Validation errors identify the offending column and physics
  method.
* **Optional canonical columns**: missing canonical columns outside the
  required set are filled with the ``-999.0`` missing marker. Column
  order is irrelevant.
* **External water use**: bulk ``Wuh`` is a
  non-negative, site-mean depth in **mm accumulated over the forcing time
  step**. It has no finite upper validation cap and is resampled as a sum,
  like ``rain``. Use ``-999`` only when the ``water_use`` requirement is
  inactive. Legacy bulk values in |m^3| must be converted explicitly; see
  :ref:`migrate_bulk_wuh`.
* **Land-cover-specific variants**: the loader also accepts whitelisted
  ``<var>_<surface>`` columns:

  - ``lai_<surface>`` is accepted **only for vegetated surfaces** --
    ``evetr``, ``dectr``, ``grass``. ``lai_paved`` / ``lai_bldgs`` /
    ``lai_bsoil`` / ``lai_water`` are not meaningful and are treated
    as unknown (warn-and-drop).
  - ``wuh_<surface>`` (external water use -- irrigation,
    impervious-surface washing, fountains, ornamental water features)
    is accepted on every surface, including the open-water surface
    via ``wuh_water`` (a fountain or pond top-up adds water to the
    ``water`` surface itself).

    **Units and convention**: each ``wuh_<surface>`` value is a depth
    in **mm per forcing time step**, the same unit as ``rain``. The
    depth is interpreted as falling on **that surface only** -- not
    spread over the whole grid. The grid-total contribution is
    therefore ``wuh_<surface> * sfr_<surface>``. Worked example: with
    grass occupying 20% of the grid and ``wuh_grass = 5`` (mm in this
    time step), the grass surface receives 5 mm of irrigation depth
    and the site-mean external water-use input is
    ``5 * 0.20 = 1`` mm. The rainfall-aligned unit also lets users
    drop ERA5-style hourly water-flux columns straight in without
    extra rescaling.

  Whitelisted columns are preserved on ``SUEWSForcing.extras`` for
  downstream physics work. Land-cover-specific ``lai_<surface>`` and
  ``wuh_<surface>`` columns are passed through to the kernel, which
  continues to use the bulk ``lai`` and ``Wuh`` columns as default
  values for legacy or bulk calculations. These bulk values are applied
  to all applicable surfaces unless a corresponding land-cover-specific
  column is provided, in which case its value overrides the bulk value.
  Fallback is based on column absence: an explicitly supplied ``-999`` does
  not request the bulk value and is invalid while that physics path is active.
  Soil-moisture deficit (``xsmd``) is a bulk site-level quantity and is
  intentionally not land-cover-specific.
* **Unknown columns**: any column not in the canonical or whitelisted
  sets emits a ``UserWarning`` and is dropped.

Important Requirements
----------------------

**Temporal Aspects**

- **Continuous data**: No gaps allowed - missing periods must be gap-filled
- **Timestamps**: Label the **end** of each forcing interval; they are not
  instantaneous sampling times
  
  - For hourly data at 13:00, the interval covers 12:00--13:00
  - For 5-minute data at 10:05, the interval covers 10:00--10:05

- **Weather, radiation, and energy fluxes**: Values are means over the
  interval ending at the timestamp
- **Rainfall and external water use**: Values are totals accumulated over that
  interval
- **State inputs**: LAI, snow cover, and soil moisture apply at the interval
  end

- **Timestamp reference**: The default is **local standard time** (a fixed UTC
  offset). UTC is accepted when declared in YAML. Civil time with
  daylight-saving transitions is not supported.
- **Complete days**: Files must contain whole days of data

.. important:: **Declare UTC; never supply daylight-saving civil time**

   By default, SUEWS interprets forcing timestamps as **local standard time** --
   the fixed UTC offset for the site's time zone, applied uniformly throughout
   the year. UTC forcing may instead be declared explicitly::

      model:
        control:
          forcing:
            file: forcing_utc.txt
            timestamp_reference: utc

   With declared UTC forcing, the main model clock, output timestamps, daily
   state boundaries and ``start_time``/``end_time`` bounds remain in UTC.
   SUEWS derives the site's fixed-offset local standard time only for solar
   calculations and local diurnal activity profiles. Interval-end alignment is
   unchanged. Do **not** supply civil time that includes daylight-saving
   (summer-time) transitions.

   For example, a UK site uses GMT (UTC+0) year-round. Converting to ``Europe/London`` would introduce DST shifts that create one missing row in spring and one duplicate row in autumn, causing SUEWS to reject the forcing file. For a site in France, use CET (UTC+1) year-round, not CEST in summer.

   The :input:option:`timezone` parameter in the YAML configuration is this
   same fixed offset (``0`` for the UK, ``1`` for France). SUEWS accounts for
   daylight saving internally through the :input:option:`startdls` and
   :input:option:`enddls` parameters, which adjust diurnal activity profiles for
   anthropogenic heat and water use; this does not change the forcing or output
   clock.

   When comparing SUEWS output against observational data, verify that both
   datasets use the same time convention. Observations recorded in civil time
   (with DST) must be converted to the declared fixed reference before
   comparison.

**File Naming**

Files should follow this naming convention:

- Single site: ``SS_YYYY_data_tt.txt``
- Multiple grids: ``SSss_YYYY_data_tt.txt``

Where:
- ``SS`` = Two-letter site code
- ``ss`` = Grid number (if using multiple grids)
- ``YYYY`` = Year
- ``tt`` = Time resolution in minutes (e.g., 60 for hourly)

Examples:
- ``Kc_2020_data_60.txt`` - Hourly data for site "Kc" in 2020
- ``Kc01_2020_data_60.txt`` - Hourly data for grid 01 of site "Kc"

**Annual Files**

- Provide separate files for each year
- Files can span partial years but must contain complete days
- For a complete year of hourly data: 8760 rows (8784 for leap years)

YAML Configuration
------------------

In your YAML configuration, specify the forcing file(s) under the
``forcing`` sub-object (schema 2026.5 onwards; see
:ref:`transition_guide` for the rename of the legacy
``model.control.forcing_file`` key):

.. code-block:: yaml

   model:
     control:
       forcing:
         file: "forcing/Kc_2020_data_60.txt"

Or, for continuous multi-year runs, supply a list under the same
``forcing.file`` key (the loader concatenates them in chronological
order):

.. code-block:: yaml

   model:
     control:
       forcing:
         file:
           - "forcing/Kc_2020_data_60.txt"
           - "forcing/Kc_2021_data_60.txt"
           - "forcing/Kc_2022_data_60.txt"

Choosing Conditional and Additional Variables
---------------------------------------------

Use :ref:`df_forcing_requirements` to determine which extra columns an active
physics path requires. The per-variable :ref:`df_forcing_var` reference gives
the authoritative units, ranges, requiredness, interval basis, and
missing-value policies. The following sections provide task-specific
preparation guidance where more context than a field catalogue is useful.

.. _prescribed-lai:

Prescribing Observed LAI
------------------------

By default SUEWS computes leaf area index (LAI) internally using growing-degree-day (GDD) and
senescence-degree-day (SDD) thresholds on daily mean air temperature. For sites where the
observed LAI cycle is driven by rainfall (monsoon grasslands, semi-arid sites) or where a
remote-sensing product is available, users can bypass the internal scheme by:

1. Setting ``model.physics.laimethod: 0`` in the YAML configuration (0 = OBSERVED,
   1 = MODELLED; default is 1).
2. Populating the ``lai`` column of the meteorological forcing file with a **non-negative**
   observation at every timestep, in |m^2| |m^-2|. A genuine zero observation (e.g.
   complete winter dieback) is valid. Choosing the observed path commits the user to
   providing an observation for every timestep; the ``-999`` missing marker is
   **not** a permitted fallback here and the pre-flight validator rejects any strictly
   negative value (including ``-999``). If observations are
   unavailable for part of the run, either switch to ``laimethod: 1`` (internally
   calculated) or gap-fill the ``lai`` column with non-negative values before feeding
   it to SUEWS.

.. note::
   When ``laimethod: 0`` is set, bulk ``lai`` supplies any vegetation class
   without its own ``lai_evetr``, ``lai_dectr``, or ``lai_grass`` column. A
   land-cover-specific column overrides the bulk value for that class.

.. important::
   Observed LAI values are clamped into each vegetation class's
   ``[laimin, laimax]`` envelope at runtime. The same clamp is applied to the
   parameterised branch (``laimethod: 1``); the observed branch enforces it too
   for consistency and because the downstream conductance and active-vegetation
   fraction calculations (``LAI / laimax`` in ``suews_phys_resist`` and
   ``suews_phys_biogenco2``) require
   :math:`\mathrm{LAI} \leq \mathrm{LAI}_{max}` to stay physically
   meaningful.

   If you supply observations that should pass through unchanged -- e.g. a genuine
   winter dieback with ``LAI = 0`` -- configure the corresponding class's
   ``laimin`` to zero in the site configuration. Similarly, widen ``laimax`` if
   observations legitimately exceed the default site canopy capacity. The
   pre-flight validator (``check_forcing()``) issues a warning
   when any forcing value would be clamped, so the user sees once that
   observations are being modified rather than discovering it through
   unexpected outputs.

Generating Forcing Data from ERA5
----------------------------------

SUEWS provides built-in support for downloading and processing ERA5 reanalysis data into forcing files using the :func:`~supy.util.gen_forcing_era5` function.

**Quick Start (Recommended - Fast Method)**

By default, SUEWS uses the fast ERA5 timeseries dataset via CDS API:

.. code-block:: python

   import supy as sp

   # Download 30 years of ERA5 data for Copenhagen (~26 seconds!)
   list_fn = sp.util.gen_forcing_era5(
       55.68, 12.57,              # Latitude, longitude
       "1991-01-01", "2020-12-31",  # Date range
       dir_save="./forcing_data"
   )

   # Files are ready to use in your YAML config
   print(f"Generated {len(list_fn)} forcing files")

**Features:**

- Fast download for point locations
- Surface-level variables only
- Automatically extrapolates to measurement height using ``hgt_agl_diag`` parameter (default 100m)

**Using Traditional Gridded ERA5**

For model-level data or spatial grids, use the gridded dataset:

.. code-block:: python

   list_fn = sp.util.gen_forcing_era5(
       55.68, 12.57,
       "1991-01-01", "2020-12-31",
       data_source="gridded",     # Use gridded ERA5 dataset
       simple_mode=False,         # Complex MOST diagnostics
       scale=1                    # Spatial grid (3x3 for scale=1)
   )

**Requirements:**

- CDS API credentials configured (see `CDS API setup <https://cds.climate.copernicus.eu/api-how-to>`_)

See :func:`~supy.util.gen_forcing_era5` API documentation for all options.

Using EPW Weather Files
-----------------------

EPW (EnergyPlus Weather) files are a common format for building energy simulation, often derived from Typical Meteorological Year (TMY) data. SUEWS can read EPW data via the :func:`~supy.util.read_epw` utility function.

**Important: Measurement Height Assumptions**

EPW files follow standard meteorological station conventions with fixed measurement heights:

.. list-table::
   :header-rows: 1
   :widths: 30 30 40

   * - Variable
     - EPW Height
     - SUEWS Forcing Variable
   * - Wind speed
     - 10 m agl
     - U
   * - Air temperature
     - 2 m agl
     - Tair
   * - Relative humidity
     - 2 m agl
     - RH

**Correct Configuration for EPW Data**

When using EPW data, set the forcing height to match the wind speed measurement:

.. code-block:: yaml

   sites:
     - name: "MySite"
       properties:
         z: 10.0  # Must be 10 m to match EPW wind speed height

.. warning::

   Using EPW data with a different forcing height (e.g., ``z: 50``) will cause incorrect wind profile calculations, as SUEWS assumes all forcing data originate from the specified height.

**Basic Workflow**

.. code-block:: python

   import supy as sp
   import pandas as pd
   from pathlib import Path

   # 1. Read EPW file (wind speed at 10 m by default)
   df_epw = sp.util.read_epw(Path("weather.epw"))

   # 2. Extract and rename columns for SUEWS forcing
   df_forcing = pd.DataFrame({
       'U': df_epw['Wind Speed'],
       'Tair': df_epw['Dry Bulb Temperature'],
       'RH': df_epw['Relative Humidity'],
       'pres': df_epw['Atmospheric Station Pressure'] / 1000,  # Pa to kPa
       'kdown': df_epw['Global Horizontal Radiation'],
       'ldown': df_epw['Horizontal Infrared Radiation Intensity'],
       'rain': df_epw['Liquid Precipitation Depth'],
   }, index=df_epw.index)

   # 3. Fill required time columns
   df_forcing['iy'] = df_forcing.index.year
   df_forcing['id'] = df_forcing.index.dayofyear
   df_forcing['it'] = df_forcing.index.hour
   df_forcing['imin'] = df_forcing.index.minute

**Wind Speed Height Correction**

If you need EPW wind speed at a different height (e.g., to match flux tower measurements at 50 m), use the ``target_height`` parameter:

.. code-block:: python

   # Read EPW and extrapolate wind speed from 10 m to 50 m
   df_epw = sp.util.read_epw(
       Path("weather.epw"),
       target_height=50.0,  # Target height [m]
       z0m=0.5              # Urban roughness length [m]
   )

This applies a logarithmic wind profile correction assuming neutral atmospheric conditions.

.. note::

   The log-law correction assumes neutral atmospheric stability. Under strongly stable or unstable conditions, actual wind profiles may differ significantly. For most applications using EPW data, setting ``z=10`` in your site configuration is the simpler and recommended approach.

**Comparison with ERA5**

Unlike EPW files with fixed heights, ERA5 forcing data from :func:`~supy.util.gen_forcing_era5` are extrapolated to a user-specified height (default 100 m) using Monin-Obukhov Similarity Theory.

.. list-table::
   :header-rows: 1
   :widths: 25 35 40

   * - Data Source
     - Wind Speed Height
     - Recommended ``z`` Setting
   * - EPW files
     - Fixed at 10 m
     - ``z: 10``
   * - ERA5 (timeseries)
     - Extrapolated to ``hgt_agl_diag`` (default 100 m)
     - Match ``hgt_agl_diag`` value
   * - Flux tower
     - Tower-specific
     - Actual measurement height

Data Preparation Tips
---------------------

**Gap Filling**

If your data has gaps, you must fill them before use. SuPy provides :func:`~supy.util.fill_gap_all` for automatic gap filling using neighbouring time periods. Common approaches:

- Use :func:`~supy.util.fill_gap_all` for automated filling from similar days
- Linear interpolation for short gaps (< 2 hours)
- Use data from nearby stations
- Use reanalysis data (ERA5 - see section above)

**Quality Control**

Check your data for:

- Unrealistic values (e.g., negative radiation during daytime)
- Sudden jumps or spikes
- Extended constant values
- Values outside physical limits

**Common Issues**

- **"Division by zero"**: Wind speed below 0.01 m |s^-1|
- **"Negative radiation"**: Check ``kdown`` is always :math:`\geq 0`
- **"Time mismatch"**: Ensure timestamps match the declared reference and
  interval-end convention (see note above)
- **"Missing data"**: Gap-fill baseline and active conditional inputs. Use
  ``-999`` only for optional or inactive conditional fields, never blanks or
  ``NaN``

Validating Forcing Data
-----------------------

SUEWS provides the ``check_forcing()`` function to validate your forcing data files before running simulations. The validation performs four main checks:

1. **Column completeness**: Verifies all expected columns are present
2. **Timestamp validity**: Checks for proper DatetimeIndex, no duplicates, monotonic increasing
3. **Physical ranges**: Validates values are within physically plausible ranges
4. **Physics-specific requirements**: Applies the generated
   :ref:`df_forcing_requirements` rules for the selected model physics options

**Variable contract and enforced ranges**

The registry-derived :ref:`df_forcing_var` reference is authoritative for
canonical names, input units, enforced ranges, requiredness, interval basis,
and missing-value policy.

**Usage**

Basic validation from Python:

.. code-block:: python

   from supy._check import check_forcing
   from supy._load import load_SUEWS_Forcing_met_df_yaml

   # Load forcing data
   df_forcing = load_SUEWS_Forcing_met_df_yaml('forcing/Kc_2020_data_60.txt')

   # Validate (returns list of issues)
   issues = check_forcing(df_forcing, fix=False)

   if issues:
       print(f"Found {len(issues)} validation issues:")
       for issue in issues:
           print(f"  - {issue}")
   else:
       print("Validation passed!")

**Automatic Validation**

When using ``suews-validate``, forcing data validation runs automatically:

.. code-block:: bash

   # Validates configuration AND forcing data
   suews-validate config.yml

   # Skip forcing validation if needed
   suews-validate --forcing off config.yml

The validation report shows any issues found:

.. code-block:: text

   ## ACTION NEEDED
   - Found (2) forcing data validation error(s):
   -- In 'Kc_2020_data_60.txt': Wind speed (`U`) must be >= 0.01 m/s
      to avoid division by zero errors in atmospheric calculations.
      3 values below 0.01 m/s found at line(s): [45, 127, 890]
   -- In 'Kc_2020_data_60.txt': `kdown` should be between [0, 1400]
      but 12 outliers are found at line(s): [156, 234, 567, ...]
      Required fix: Review and correct forcing data file.
      Suggestion: You may want to plot the time series of your input data.


For comprehensive quality control, combine ``check_forcing()`` with visual inspection of time series plots.

See Also
--------

- :ref:`df_forcing_var` - Registry-derived forcing-variable reference
- :doc:`/inputs/yaml/index` - YAML configuration including forcing file specification
- :doc:`/inputs/yaml/validation` - Complete validation system documentation
- :doc:`/inputs/tables/RunControl/RunControl` - Legacy model physics options reference
- :doc:`/troubleshooting` - Common forcing data issues and solutions
