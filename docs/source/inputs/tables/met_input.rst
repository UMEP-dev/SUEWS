.. _met_input:

Meteorological Input File
-------------------------

SUEWS is designed to run using commonly measured meteorological variables (e.g. incoming solar radiation, air temperature, relative humidity, pressure, wind speed, etc.).

When preparing this input file, please note the following:

-  Required inputs must be continuous – i.e. **gap fill** any missing data.
-  Temporal information (i.e., ``iy``, ``id``, ``it`` and ``imin``) should be in **local time** and indicate the ending timestamp of corresponding periods: e.g. for hourly data, ``2021-09-12 13:00`` indicates a record for the period between ``2021-09-12 12:00`` (inclusive) and ``2021-09-12 13:00`` (exclusive).
-  The `table <SSss_YYYY_data_tt.txt>` below gives the must-use (`MU`) and optional (`O`) additional input variables. If an optional input variable (`O`) is not available or will not be used by the model, enter ‘-999’ for this column.


-  One single meteorological file can be used for all grids (**MultipleMetFiles=0** in `RunControl.nml`, no grid number in file name) if appropriate for the study area.
-  Separate met files can be used for each grid if data are available (**MultipleMetFiles=1** in `RunControl.nml`, filename includes grid number).

-  The meteorological forcing file names should be appended with the temporal resolution in minutes: ``tt`` in ``SS_YYYY_data_tt.txt`` (or
   ``SSss_YYYY_data_tt.txt`` for multiple grids).

-  Separate met forcing files should be provided for each year.
-  Files do not need to start/end at the start/end of the year, but they must contain a whole number of days.
-  The meteorological input file should match the information given in `SUEWS_SiteSelect.txt`.
-  If a *partial year* is used that specific year must be given in SUEWS_SiteSelect.txt.
-  If *multiple years* are used, all years should be included in SUEWS_SiteSelect.txt.
-  If a *whole year* (e.g. 2011) is intended to be modelled using and hourly resolution dataset, the number of lines in the met data file should be 8760 and begin and end with::

     iy     id  it  imin
     2011   1   1   0 …
     …
     2012   1   0   0 …



SSss_YYYY_data_tt.txt
~~~~~~~~~~~~~~~~~~~~~

.. versionchanged:: v2017a
   Since v2017a forcing files no longer need to end with two rows containing ‘-9’ in the first column.


Main meteorological data file.

.. csv-table::
  :file: SSss_YYYY_data_tt.csv
  :header-rows: 1
  :widths: auto

.. _prepare_forcing_data:

Preparing Input Forcing Data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This section provides guidance on preparing meteorological forcing data for SUEWS simulations from various data sources.

**Data Requirements**

All SUEWS forcing data must meet these requirements:

- **Temporal consistency**: Regular time intervals (hourly, sub-hourly, or multi-hourly)
- **Gap-free**: All missing values must be filled or interpolated
- **Local time**: Timestamps represent the **end** of each measurement period
- **Physical units**: Data must be in the units specified in the variable table above
- **Quality control**: Remove or flag obviously erroneous values

**Common Data Sources**

**Weather Station Data**

Weather stations provide the most accurate local measurements but may require processing:

.. code-block:: python

   import pandas as pd
   import numpy as np
   
   # Load weather station data
   df = pd.read_csv('weather_station.csv', parse_dates=['datetime'])
   df.set_index('datetime', inplace=True)
   
   # Ensure hourly frequency and fill gaps
   df = df.resample('H').mean()  # or .interpolate() for linear interpolation
   df = df.interpolate(method='linear', limit=3)  # Fill short gaps
   
   # Calculate derived variables if not measured
   # Atmospheric pressure (if only station pressure available)
   df['pres'] = df['station_pres'] * (1 - 0.0065 * elevation / 288.15)**5.257
   
   # Specific humidity from relative humidity and temperature
   es = 6.112 * np.exp(17.67 * df['Tair'] / (df['Tair'] + 243.5))  # Saturation vapour pressure
   e = df['RH'] / 100 * es  # Actual vapour pressure
   df['qh'] = 0.622 * e / (df['pres'] - 0.378 * e)  # Specific humidity

**Reanalysis Data Processing**

Reanalysis datasets (MERRA-2, JRA-55, NCEP) require spatial and temporal processing:

.. code-block:: python

   import xarray as xr
   
   # Load reanalysis data (example with netCDF)
   ds = xr.open_dataset('reanalysis_data.nc')
   
   # Extract data for specific location
   lat_target, lon_target = 51.5074, -0.1278  # London coordinates
   point_data = ds.sel(lat=lat_target, lon=lon_target, method='nearest')
   
   # Convert to DataFrame and ensure proper time formatting
   df = point_data.to_dataframe().reset_index()
   df['datetime'] = pd.to_datetime(df['time'])
   df.set_index('datetime', inplace=True)
   
   # Unit conversions (example: K to °C, m/s to specific humidity)
   df['Tair'] = df['temperature'] - 273.15  # K to °C
   df['RH'] = df['relative_humidity'] * 100  # fraction to percentage
   
   # Resample to required frequency if needed
   df = df.resample('H').interpolate()

**Custom Data Sources**

For research datasets or specialized sensors:

.. code-block:: python

   # Custom processing function
   def process_custom_data(file_path, site_elevation=50):
       df = pd.read_csv(file_path, skiprows=3)  # Skip header rows
       
       # Create proper datetime index
       df['datetime'] = pd.to_datetime(df[['year', 'month', 'day', 'hour']])
       df.set_index('datetime', inplace=True)
       
       # Calculate missing variables
       if 'kdown' not in df.columns and 'global_rad' in df.columns:
           df['kdown'] = df['global_rad']  # Rename if needed
       
       # Quality control
       df.loc[df['Tair'] < -50, 'Tair'] = np.nan  # Remove impossible temperatures
       df.loc[df['RH'] > 100, 'RH'] = 100  # Cap relative humidity
       df.loc[df['kdown'] < 0, 'kdown'] = 0  # Remove negative radiation
       
       return df

**SUEWS Format Conversion**

Convert processed data to SUEWS input format:

.. code-block:: python

   def to_suews_format(df, output_file, year):
       # Extract time components
       df['iy'] = df.index.year
       df['id'] = df.index.dayofyear
       df['it'] = df.index.hour
       df['imin'] = df.index.minute
       
       # Select and order required columns (adjust based on available data)
       suews_cols = ['iy', 'id', 'it', 'imin', 'kdown', 'ldown', 'Tair', 'RH', 
                     'pres', 'rain', 'U', 'qh', 'snow', 'lup', 'xsmd', 'lai']
       
       # Fill missing optional variables with -999
       for col in suews_cols:
           if col not in df.columns:
               df[col] = -999
       
       # Write to file with SUEWS naming convention
       df_out = df[suews_cols]
       df_out.to_csv(f'{output_file}_{year}_data_60.txt', 
                     sep='\t', index=False, float_format='%.2f')

**Quality Control and Validation**

Essential checks before using forcing data:

.. code-block:: python

   def validate_forcing_data(df):
       """Perform quality control on forcing data."""
       issues = []
       
       # Check for missing critical variables
       critical_vars = ['kdown', 'Tair', 'RH', 'pres', 'rain', 'U']
       for var in critical_vars:
           if var not in df.columns:
               issues.append(f"Missing critical variable: {var}")
           elif df[var].isna().sum() > 0:
               issues.append(f"{var} has {df[var].isna().sum()} missing values")
       
       # Physical range checks
       if (df['Tair'] < -50).any() or (df['Tair'] > 60).any():
           issues.append("Temperature outside reasonable range (-50 to 60°C)")
       
       if (df['RH'] < 0).any() or (df['RH'] > 100).any():
           issues.append("Relative humidity outside 0-100% range")
       
       if (df['kdown'] < 0).any():
           issues.append("Negative incoming shortwave radiation")
       
       if (df['rain'] < 0).any():
           issues.append("Negative precipitation")
       
       # Temporal consistency
       time_diff = df.index.to_series().diff().dropna()
       if not (time_diff == time_diff.iloc[0]).all():
           issues.append("Irregular time intervals detected")
       
       return issues

**Example Workflow**

Complete example for processing weather station data:

.. code-block:: python

   # 1. Load and process raw data
   df_raw = pd.read_csv('station_data.csv', parse_dates=['timestamp'])
   df_raw.set_index('timestamp', inplace=True)
   
   # 2. Resample to hourly and fill gaps
   df = df_raw.resample('H').mean()
   df = df.interpolate(method='linear', limit=6)  # Fill gaps up to 6 hours
   
   # 3. Calculate derived variables
   df = calculate_specific_humidity(df)  # Custom function
   df = calculate_pressure_adjustment(df, site_elevation=120)
   
   # 4. Quality control
   issues = validate_forcing_data(df)
   if issues:
       print("Data quality issues found:")
       for issue in issues:
           print(f"  - {issue}")
   
   # 5. Convert to SUEWS format
   for year in df.index.year.unique():
       year_data = df[df.index.year == year]
       to_suews_format(year_data, 'MyCity', year)
   
   print(f"Generated forcing files for {len(df.index.year.unique())} years")

**Best Practices**

- **Document data sources**: Keep records of data origin, processing steps, and any assumptions made
- **Preserve original data**: Always work with copies and maintain original datasets
- **Validate energy balance**: Check that longwave components are physically consistent
- **Site representativeness**: Ensure meteorological data represents the study area scale
- **Gap filling strategies**: Use appropriate methods for different variable types and gap lengths
- **Multiple years**: Process multiple years consistently to capture inter-annual variability

**Common Issues and Solutions**

- **Missing longwave radiation**: Use empirical relationships based on air temperature and humidity
- **Inconsistent time zones**: Ensure all data is in local time for the study location
- **Sub-daily precipitation**: Aggregate appropriately while preserving intensity patterns
- **Wind speed at different heights**: Apply logarithmic wind profile corrections if needed
- **Pressure measurements**: Distinguish between station and sea-level pressure corrections

This workflow ensures high-quality forcing data that will produce reliable SUEWS simulation results.
