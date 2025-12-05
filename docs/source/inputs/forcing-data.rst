.. _met_forcing:

Meteorological Forcing Data
============================

SUEWS requires continuous meteorological data representative of the neighbourhood scale, within the inertial sublayer (i.e. a blended response above the roughness elements of buildings and trees), to drive the urban energy and water balance calculations. This page describes the format and requirements for forcing data files.

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

   See :yaml:option:`z` for full documentation, and :ref:`rsl_mod` in :doc:`/parameterisations-and-sub-models` for details on profile calculations.

Data Requirements
-----------------

**Essential Variables**

SUEWS requires the following meteorological variables:

.. list-table::
   :header-rows: 1
   :widths: 20 20 20 40

   * - Variable
     - Units
     - Column Name
     - Notes
   * - Wind speed
     - m/s
     - U
     - Minimum 0.01 m/s (to avoid division by zero)
   * - Relative humidity
     - %
     - RH
     - 0-100%
   * - Air temperature
     - °C
     - Tair
     - 
   * - Atmospheric pressure
     - kPa
     - pres
     - 
   * - Rainfall
     - mm
     - rain
     - Per time step
   * - Incoming shortwave
     - W/m²
     - kdown
     - Must be > 0
   * - Incoming longwave
     - W/m²
     - ldown
     - Optional (use -999 if not available)

**Time Information**

Each row must include time stamps with these columns (in order):

1. ``iy`` - Year (YYYY)
2. ``id`` - Day of year (1-365/366)
3. ``it`` - Hour (0-23)
4. ``imin`` - Minute (0-59)

File Format
-----------

**Structure**

- **Format**: Space or tab-delimited text file
- **Extension**: ``.txt``
- **Header**: No header row - data starts from first line
- **Missing values**: Use ``-999`` for optional variables

**Column Order**

The columns must appear in this exact order:

.. code-block:: text

   iy  id  it  imin  qn  qh  qe  qs  qf  U  RH  Tair  pres  rain  kdown  snow  ldown  fcld  Wuh  xsmd  lai  kdiff  kdir  wdir

Where:

- Columns 1-4: Time stamps (required)
- Columns 5-9: Energy fluxes (optional, use -999)
- Columns 10-15: Essential meteorological variables (required)
- Columns 16-24: Additional optional variables (use -999 if not available)

**Example**

.. code-block:: text

   iy  id  it  imin  qn  qh  qe  qs  qf  U  RH  Tair  pres  rain  kdown  snow  ldown  fcld  Wuh  xsmd  lai  kdiff  kdir  wdir
   2020  1  1   0  -999  -999  -999  -999  -999  2.1  85  5.2  101.3  0.0  0  -999  315  -999  -999  -999  -999  -999  -999  -999
   2020  1  1  60  -999  -999  -999  -999  -999  2.3  84  5.3  101.3  0.2  0  -999  318  -999  -999  -999  -999  -999  -999  -999
   2020  1  2   0  -999  -999  -999  -999  -999  2.0  86  5.1  101.2  0.0  0  -999  312  -999  -999  -999  -999  -999  -999  -999

Important Requirements
----------------------

**Temporal Aspects**

- **Continuous data**: No gaps allowed - missing periods must be gap-filled
- **Time stamps**: Indicate the **END** of each measurement period
  
  - For hourly data at 13:00, the measurement covers 12:00-13:00
  - For 5-minute data at 10:05, the measurement covers 10:00-10:05

- **Time zone**: Use **local time**, not UTC
- **Complete days**: Files must contain whole days of data

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

In your YAML configuration, specify the forcing file(s):

.. code-block:: yaml

   model:
     control:
       forcing_file: "forcing/Kc_2020_data_60.txt"
       
       # Or multiple files for continuous multi-year runs:
       forcing_file:
         - "forcing/Kc_2020_data_60.txt"
         - "forcing/Kc_2021_data_60.txt"
         - "forcing/Kc_2022_data_60.txt"

Optional Variables
------------------

These additional variables can enhance model performance but are not required:

.. list-table::
   :header-rows: 1
   :widths: 25 15 15 45

   * - Variable
     - Units
     - Column
     - Usage
   * - Net radiation
     - W/m²
     - qn
     - If NetRadiationMethod = 0
   * - Sensible heat flux
     - W/m²
     - qh
     - For validation/comparison
   * - Latent heat flux
     - W/m²
     - qe
     - For validation/comparison
   * - Storage heat flux
     - W/m²
     - qs
     - For validation/comparison
   * - Anthropogenic heat
     - W/m²
     - qf
     - If not modeled
   * - Snow fraction
     - 0-1
     - snow
     - If SnowUse = 1
   * - Cloud fraction
     - tenths
     - fcld
     - For radiation calculations
   * - External water use
     - m³
     - Wuh
     - For irrigation
   * - Soil moisture
     - m³/m³
     - xsmd
     - For initialization
   * - Leaf area index
     - m²/m²
     - lai
     - If not modeled
   * - Diffuse radiation
     - W/m²
     - kdiff
     - For SOLWEIG
   * - Direct radiation
     - W/m²
     - kdir
     - For SOLWEIG
   * - Wind direction
     - degrees
     - wdir
     - Currently not used

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

- **"Division by zero"**: Wind speed < 0.01 m/s
- **"Negative radiation"**: Check kdown is always ≥ 0
- **"Time mismatch"**: Ensure local time is used
- **"Missing data"**: Use -999, not blank or NaN

Validating Forcing Data
-----------------------

SUEWS provides the ``check_forcing()`` function to validate your forcing data files before running simulations. The validation performs four main checks:

1. **Column completeness**: Verifies all expected columns are present
2. **Timestamp validity**: Checks for proper DatetimeIndex, no duplicates, monotonic increasing
3. **Physical ranges**: Validates values are within physically plausible ranges
4. **Physics-specific requirements**: Ensures required data columns contain valid values based on selected model physics options (see below)

**Variables and Physical Ranges**

.. list-table::
   :header-rows: 1
   :widths: 20 30 50

   * - Variable
     - Valid Range
     - Notes
   * - U (wind speed)
     - ≥ 0.01 m/s
     - Minimum to avoid division by zero
   * - RH (rel. humidity)
     - 0.0001 - 105%
     - Small buffer for measurement uncertainty
   * - Tair (temperature)
     - -50 to 55°C
     - Extreme climate conditions
   * - pres (pressure)
     - 680 - 1300 hPa
     - Sea level to high altitude
   * - rain (rainfall)
     - ≥ 0 mm
     - Cannot be negative
   * - kdown (SW↓)
     - 0 - 1400 W/m²
     - Solar constant at surface
   * - ldown (LW↓)
     - 100 - 600 W/m²
     - Atmospheric thermal radiation
   * - qn, qh, qe, qs, qf
     - -800 to 1400 W/m²
     - Energy flux physical limits
   * - snow
     - 0 - 1
     - Fraction (0-1, or 0-100%)
   * - fcld
     - 0 - 10
     - Cloud cover (oktas, 0-10 tenths)
   * - xsmd
     - 0 - 1 m³/m³
     - Volumetric soil moisture
   * - lai
     - 0 - 15 m²/m²
     - Leaf area index
   * - kdiff, kdir
     - 0 - 1400 W/m²
     - Radiation components
   * - wdir
     - 0 - 360°
     - Wind direction

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

Physics-Specific Data Requirements
-----------------------------------

Certain model physics options require specific forcing data columns to contain valid (non-missing) values.

.. list-table::
   :header-rows: 1
   :widths: 30 20 50

   * - Physics Option
     - Required Column
     - Description
   * - ``netradiationmethod: 0``
     - ``qn``
     - Uses observed net radiation
   * - ``netradiationmethod: 1``
     - ``ldown``
     - Models Q* with observed incoming longwave
   * - ``netradiationmethod: 2``
     - ``kdown``, ``fcld``
     - Models Q* and L↓ using cloud fraction
   * - ``netradiationmethod: 3``
     - ``kdown``
     - Models L↓ from Tair and RH
   * - ``storageheatmethod: 0``
     - ``qs``
     - Uses observed storage heat flux
   * - ``emissionsmethod: 0``
     - ``qf``
     - Uses observed anthropogenic heat flux
   * - ``smdmethod: 1``
     - ``xsmd``
     - Uses observed volumetric soil moisture
   * - ``smdmethod: 2``
     - ``xsmd``
     - Uses observed gravimetric soil moisture

See Also
--------

- :doc:`/inputs/yaml/index` - YAML configuration including forcing file specification
- :doc:`/inputs/yaml/validation` - Complete validation system documentation
- :doc:`/input_files/RunControl/RunControl` - Model physics options reference
- :doc:`/troubleshooting` - Common forcing data issues and solutions