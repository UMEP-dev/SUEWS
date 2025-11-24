# suppress pandas warnings

import logging
import os
import shutil
import time
import warnings
import contextlib
from pathlib import Path

import numpy as np
import pandas as pd
from numpy import cos, deg2rad, sin, sqrt

from .._env import logger_supy

warnings.simplefilter(action="ignore", category=FutureWarning)


################################################
# more ERA-5 related functions
################################################


# utility functions
def roundPartial(value, resolution):
    """
    Geopotential Functions on WGS84 Reference Ellipsoid

    This module contains code for converting Geopotential to Geometric and vice-versa on the WGS84 reference ellipsoid

    ERA-5 utility functions from Chris Roth # https://pypi.org/project/eratools/
    """
    return round(value / resolution) * resolution


Rmax_WGS84 = 6378137
Rmin_WGS84 = Rmax_WGS84 * (1 - 1 / 298.257223563)


def _geoid_radius(latitude: float) -> float:
    """Calculates the GEOID radius at a given latitude

    Parameters
    ----------
    latitude : float
        Latitude (degrees)

    Returns
    -------
    R : float
        GEOID Radius (meters)
    """
    lat = deg2rad(latitude)
    return sqrt(1 / (cos(lat) ** 2 / Rmax_WGS84**2 + sin(lat) ** 2 / Rmin_WGS84**2))


def geometric2geopotential(z: float, latitude: float) -> float:
    """Converts geometric height to geopotential height

    Parameters
    ----------
    z : float
        Geometric height (meters)
    latitude : float
        Latitude (degrees)

    Returns
    -------
    h : float
        Geopotential Height (meters) above the reference ellipsoid
    """
    twolat = deg2rad(2 * latitude)
    g = 9.80616 * (1 - 0.002637 * cos(twolat) + 0.0000059 * cos(twolat) ** 2)
    re = _geoid_radius(latitude)
    return z * g * re / (re + z)


def geopotential2geometric(h: float, latitude: float) -> float:
    """Converts geopotential height to geometric height

    Parameters
    ----------
    h : float
        Geopotential height (meters)
    latitude : float
        Latitude (degrees)

    Returns
    -------
    z : float
        Geometric Height (meters) above the reference ellipsoid
    """
    twolat = deg2rad(2 * latitude)
    g = 9.80616 * (1 - 0.002637 * cos(twolat) + 0.0000059 * cos(twolat) ** 2)
    re = _geoid_radius(latitude)
    return h * re / (g * re - h)


# functions to interpolate the atmospheric variables to a specified height/altitude
def get_ser_val_alt(
    lat: float,
    lon: float,
    da_alt_x,
    da_alt,
    da_val,
) -> pd.Series:
    """interpolate atmospheric variable to a specified altitude

    Parameters
    ----------
    lat : float
        latitude of specified site
    lon : float
        longitude of specified site
    da_alt_x
        desired altitude to interpolate variable at
    da_alt
        altitude associated with `da_val`: variable array to interpolate
    da_val
        atmospheric variable to interpolate

    Returns
    -------
    pd.Series
        interpolated values at the specified altitude of site positioned by [`lat`, `lon`]
    """
    from scipy.interpolate import interp1d

    alt_t_1d = da_alt.sel(latitude=lat, longitude=lon, method="nearest")
    val_t_1d = da_val.sel(latitude=lat, longitude=lon, method="nearest")
    alt_x = da_alt_x.sel(latitude=lat, longitude=lon, method="nearest")[0]
    val_alt = np.array([
        interp1d(alt_1d, val_1d)(alt_x) for alt_1d, val_1d in zip(alt_t_1d, val_t_1d)
    ])
    ser_alt = pd.Series(
        val_alt,
        index=da_val.time.values,
        name=da_val.name,
    )
    return ser_alt


def get_df_val_alt(lat: float, lon: float, da_alt_meas, ds_val):
    """interpolate atmospheric variables to a specified altitude

    Parameters
    ----------
    lat : float
        latitude of specified site
    lon : float
        longitude of specified site
    da_alt_meas
        altitude associated with `da_val`: variable array to interpolate
    ds_val
        atmospheric varialble to interpolate

    Returns
    -------
    pd.DataFrame
        interpolated values at the specified altitude of site positioned by [`lat`, `lon`]
    """
    from scipy.interpolate import interp1d

    da_alt = geopotential2geometric(ds_val.z, ds_val.latitude)
    # generate pressure series for grid x
    da_alt_x = da_alt.sel(latitude=lat, longitude=lon, method="nearest")
    alt_meas_x = da_alt_meas.sel(latitude=lat, longitude=lon, method="nearest")[0]

    val_pres = np.array([interp1d(alt, da_alt_x.level)(alt_meas_x) for alt in da_alt_x])
    df_val_alt = pd.concat(
        [
            get_ser_val_alt(lat, lon, da_alt_meas, da_alt, ds_val[var])
            for var in ds_val.data_vars
        ],
        axis=1,
    )
    #     add pressure
    df_val_alt["p"] = val_pres
    df_val_alt.index = df_val_alt.index.set_names("time")
    df_val_alt.columns = df_val_alt.columns.set_names("var")

    return df_val_alt


# cds download related functions
def gen_dict_dt(dt_index):
    list_key = ["year", "month", "day", "time"]
    list_fmt = ["%Y", "%m", "%d", "%H:%M"]
    dict_dt = {
        k: dt_index.dt.strftime(fmt).unique().tolist()
        for k, fmt in zip(list_key, list_fmt)
    }
    return dict_dt


def gen_dict_dt_sub(dt_index):
    # divide by [year, month] for surface level data
    ser_dict_sub = dt_index.groupby(dt_index.dt.strftime("%Y%m")).apply(gen_dict_dt)
    dict_sub = ser_dict_sub.unstack().T.to_dict()
    return dict_sub


# generate filename
def gen_fn(dict_x):
    # centre coordinates
    ar_coords = np.array(dict_x["area"]).reshape(2, -1)
    lat_c, lon_c = ar_coords.T.mean(axis=1)
    # starting year
    yr_x = dict_x["year"][0]
    # starting month
    mon_x = dict_x["month"][0]
    # type of dataset: `sfc` for surface level while `ml` for atmospheric multi-level
    type_x = "sfc" if "2m_temperature" in dict_x["variable"] else "ml"

    # format location coordinates in filename
    lat_c = f"{lat_c}N" if lat_c > 0 else f"{-lat_c}S"
    lon_c = f"{lon_c}E" if lon_c > 0 else f"{-lon_c}W"

    # construct filename
    fn_x = f"{lat_c}{lon_c}-{yr_x}{mon_x}-{type_x}.nc"
    return fn_x


# dict_x: a dict describing download elements
def gen_dict_proc(dict_x):
    type_x = "sfc" if "2m_temperature" in dict_x["variable"] else "ml"
    dict_feed = {
        "sfc": "reanalysis-era5-single-levels",
        "ml": "reanalysis-era5-pressure-levels",
    }
    feed_x = dict_feed[type_x]
    dict_proc = dict(
        name=feed_x,
        request=dict_x,
        target=gen_fn(dict_x),
    )

    return dict_proc


# generate a dict of reqs kwargs for (lat_x,lon_x) spanning [start, end]
@contextlib.contextmanager
def chdir(path):
    orig = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(orig)


def download_cds(fn, dict_req):
    import cdsapi
    import tempfile

    c = cdsapi.Client()
    path_fn = Path(fn)
    if path_fn.exists():
        logger_supy.warning(f"{fn} exists!")
    else:
        logger_supy.info(f"To download: {fn}")

        # this will download the file to a secure temporary directory without requirement for extra permission
        with tempfile.TemporaryDirectory() as td:
            with chdir(td):
                c.retrieve(**dict_req)
                # move the downloaded file to desired location
                shutil.move(path_fn.name, fn)
        # hold on a bit for the next request
        time.sleep(0.0100)


def download_era5_timeseries(
    lat_x: float,
    lon_x: float,
    start: str,
    end: str,
    dir_save=Path("."),
    logging_level=logging.INFO,
) -> str:
    """Download ERA5 data using CDS API timeseries dataset.

    This function uses the ERA5 timeseries dataset which is optimised for
    fast retrieval of point location data. Only provides surface-level variables.

    Parameters
    ----------
    lat_x : float
        Latitude of the point of interest.
    lon_x : float
        Longitude of the point of interest.
    start : str
        Start date in format 'YYYY-MM-DD'.
    end : str
        End date in format 'YYYY-MM-DD'.
    dir_save : Path or str, optional
        Directory to save downloaded file, by default Path(".").
    logging_level : int, optional
        Logging level, by default logging.INFO.

    Returns
    -------
    str
        Path to the downloaded CSV file.

    Note
    ----
    1. This method only provides surface-level variables and should be used with simple_mode=True.
    2. Uses CDS API timeseries dataset for fast point location downloads.
    3. Much faster than traditional gridded ERA5 (~26s for 30 years vs several minutes).
    4. Only works for point locations (no spatial grid).
    5. Downloads CSV format directly (no netCDF dependencies needed).
    6. Requires CDS API credentials configured: https://cds.climate.copernicus.eu/api-how-to

    Reference
    ---------
    ERA5 timeseries documentation: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-timeseries
    """
    import cdsapi
    import zipfile
    import tempfile

    # adjust logging level
    logger_supy.setLevel(logging_level)

    # parse and create (if needed) the saving directory
    path_dir_save = Path(dir_save).expanduser().resolve()
    if not path_dir_save.exists():
        path_dir_save.mkdir(parents=True)

    # define variables available in ERA5 timeseries dataset
    # NOTE: This dataset only provides basic surface variables (16 total).
    # Advanced variables (geopotential, heat fluxes, surface properties) are NOT available.
    # These must be obtained from the gridded ERA5 dataset if needed.
    variables = [
        "2m_dewpoint_temperature",
        "2m_temperature",
        "10m_u_component_of_wind",
        "10m_v_component_of_wind",
        "surface_pressure",
        "surface_solar_radiation_downwards",
        "surface_thermal_radiation_downwards",
        "total_precipitation",
    ]

    # format location coordinates in filename
    lat_c = f"{lat_x}N" if lat_x > 0 else f"{-lat_x}S"
    lon_c = f"{lon_x}E" if lon_x > 0 else f"{-lon_x}W"

    # parse dates
    date_start = pd.to_datetime(start).strftime("%Y-%m-%d")
    date_end = pd.to_datetime(end).strftime("%Y-%m-%d")
    year_start = pd.to_datetime(start).year

    # construct filename (CSV format)
    fn = f"{lat_c}{lon_c}-{year_start}-sfc.csv"
    path_fn = path_dir_save / fn

    if path_fn.exists():
        logger_supy.warning(f"{fn} exists!")
    else:
        logger_supy.info(f"Downloading ERA5 timeseries data using CDS API...")
        logger_supy.info(f"Location: ({lat_x}, {lon_x})")
        logger_supy.info(f"Period: {date_start} to {date_end}")

        # define request parameters (CSV - no netCDF dependencies needed)
        dataset = "reanalysis-era5-single-levels-timeseries"
        request = {
            "variable": variables,
            "location": {"longitude": lon_x, "latitude": lat_x},
            "date": [f"{date_start}/{date_end}"],
            "data_format": "csv",
        }

        # retrieve data to temporary file (CDS API returns zip archives)
        t0 = time.time()
        client = cdsapi.Client()

        with tempfile.TemporaryDirectory() as tmp_dir:
            tmp_path = Path(tmp_dir) / "download.zip"
            client.retrieve(dataset, request).download(str(tmp_path))

            # extract CSV from zip archive
            with zipfile.ZipFile(tmp_path, "r") as zip_ref:
                # find the CSV file in the archive
                csv_files = [f for f in zip_ref.namelist() if f.endswith(".csv")]
                if not csv_files:
                    raise ValueError(f"No CSV file found in downloaded archive")

                # extract the first CSV file
                zip_ref.extract(csv_files[0], path_dir_save)
                extracted_path = path_dir_save / csv_files[0]

                # rename to expected filename
                extracted_path.rename(path_fn)
            # cleanup happens automatically when tmp_dir context exits

        t1 = time.time()
        logger_supy.info(f"Download completed in {t1 - t0:.1f} seconds")

    return str(path_fn)


# generate requests
# load downloaded files
# generate supy forcing using ERA-5 data
def gen_forcing_era5(
    lat_x: float,
    lon_x: float,
    start: str,
    end: str,
    dir_save=Path("."),
    hgt_agl_diag=100.0,
    logging_level=logging.INFO,
) -> list:
    """Generate SUEWS forcing files using ERA-5 timeseries data.

    Downloads ERA5 surface timeseries data from CDS API and converts to SUEWS forcing format.
    Uses simple diagnostics: environmental lapse rate (6.5 K/km) for temperature and
    neutral MOST for wind speed.

    Parameters
    ----------
    lat_x : float
        Latitude of the location.
    lon_x : float
        Longitude of the location.
    start : str
        Start date (e.g., "2020-01-01").
    end : str
        End date (e.g., "2020-12-31").
    dir_save : pathlib.Path or str, optional
        Directory for saving ERA5 CSV files, by default Path(".").
    hgt_agl_diag : float, optional
        Height above ground level for diagnostics (m), by default 100.
    logging_level : int, optional
        Logging level [50=CRITICAL, 40=ERROR, 30=WARNING, 20=INFO, 10=DEBUG].

    Returns
    -------
    list
        List of SUEWS forcing file paths.

    Note
    ----
    1. Requires CDS API configuration: https://cds.climate.copernicus.eu/api-how-to
    2. Uses ERA5 timeseries dataset (surface data, ~26s for 30 years)
    3. Point location only (no spatial grids)
    4. Use supy.util.read_forcing() to import generated files

    Examples
    --------
    >>> list_fn = gen_forcing_era5(50.86, 4.35, "2020-01-01", "2020-01-31")

    Reference
    ---------
    ECMWF, S. P. (2016). In IFS documentation CY41R2 Part IV: Physical Processes.
    ECMWF: Reading, UK, 111-113. https://www.ecmwf.int/en/elibrary/16648-part-iv-physical-processes
    """
    # adjust logging level
    logger_supy.setLevel(logging_level)

    # download ERA5 timeseries CSV
    fn_sfc = download_era5_timeseries(lat_x, lon_x, start, end, dir_save, logging_level)

    # generate diagnostics from CSV
    df_forcing_raw = gen_df_diag_era5_csv(fn_sfc, hgt_agl_diag)

    # extract coordinates
    lat = df_forcing_raw.attrs["latitude"]
    lon = df_forcing_raw.attrs["longitude"]

    # format to SUEWS convention
    df_forcing = format_df_forcing(df_forcing_raw)

    # add coordinates to match expected structure
    df_forcing["latitude"] = lat
    df_forcing["longitude"] = lon
    df_forcing = df_forcing.reset_index().set_index(["latitude", "longitude", "time"])

    # save results as SUEWS met input files
    list_fn = save_forcing_era5(df_forcing, dir_save)

    return list_fn


# format dataframe to SUEWS convention
def format_df_forcing(df_forcing_raw):
    from .._load import dict_var_type_forcing

    df_forcing_grid = df_forcing_raw.copy()

    # convert energy fluxes: [J m-2] to [W m-2]
    df_forcing_grid.loc[:, ["ssrd", "strd", "sshf", "slhf"]] /= 3600

    # reverse the sign of qh and qe
    df_forcing_grid.loc[:, ["sshf", "slhf"]] *= -1

    # convert rainfall: from [m] to [mm]
    df_forcing_grid.loc[:, "tp"] *= 1000

    # convert bulb temperature from K to degC
    df_forcing_grid = df_forcing_grid.assign(Tair=df_forcing_grid.t_z - 273.15)

    # convert atmospheric pressure: [Pa] to [kPa]
    df_forcing_grid.loc[:, "p_z"] /= 1000

    # renaming for consistency with SUEWS
    df_forcing_grid = df_forcing_grid.rename(
        {
            "ssrd": "kdown",
            "strd": "ldown",
            "sshf": "qh",
            "slhf": "qe",
            "tp": "rain",
            "uv_z": "U",
            "RH_z": "RH",
            "p_z": "pres",
        },
        axis=1,
    )

    col_suews = list(dict_var_type_forcing.keys())[:-1] + ["alt_z"]

    df_forcing_grid = df_forcing_grid.reindex(col_suews, axis=1)

    df_forcing_grid = df_forcing_grid.assign(
        iy=df_forcing_grid.index.year,
        id=df_forcing_grid.index.dayofyear,
        it=df_forcing_grid.index.hour,
        imin=df_forcing_grid.index.minute,
    )

    # corrections
    df_forcing_grid.loc[:, "RH"] = df_forcing_grid.loc[:, "RH"].where(
        df_forcing_grid.loc[:, "RH"].between(0.001, 105), 105
    )
    df_forcing_grid.loc[:, "kdown"] = df_forcing_grid.loc[:, "kdown"].where(
        df_forcing_grid.loc[:, "kdown"] > 0, 0
    )

    # trim decimals
    df_forcing_grid.iloc[:, 4:] = df_forcing_grid.iloc[:, 4:].round(2)

    # coerce integer
    df_forcing_grid = df_forcing_grid.astype({
        "iy": "int32",
        "id": "int32",
        "it": "int32",
        "imin": "int32",
    })

    # replace nan with -999
    df_forcing_grid = df_forcing_grid.replace(np.nan, -999).asfreq("1h")

    return df_forcing_grid


# pandas-only version for CSV timeseries (no xarray dependency)
def gen_df_diag_era5_csv(fn_csv, hgt_agl_diag=100):
    """Generate diagnostics from CSV timeseries data using pandas only.

    This function avoids xarray dependency for the common timeseries use case.
    Only supports simple_mode=True (log law + lapse rate).
    """
    from atmosp import calculate as ac

    # load CSV timeseries data
    df = pd.read_csv(fn_csv)
    df["valid_time"] = pd.to_datetime(df["valid_time"])
    df = df.set_index("valid_time")
    df.index.name = "time"

    # extract coordinates
    lat = df["latitude"].iloc[0]
    lon = df["longitude"].iloc[0]

    # extract variables (using abbreviated CSV column names)
    pres_z0 = df["sp"]  # surface pressure [Pa]
    u10 = df["u10"]  # 10m u-wind [m/s]
    v10 = df["v10"]  # 10m v-wind [m/s]
    t2 = df["t2m"]  # 2m temperature [K]
    d2 = df["d2m"]  # 2m dewpoint [K]

    # computed variables
    uv10 = np.sqrt(u10**2 + v10**2)  # wind speed
    q2 = ac("qv", Td=d2, T=t2, p=pres_z0)  # specific humidity

    # default values for missing variables (timeseries doesn't provide these)
    alt_z0 = 0.0  # assume sea level
    z0m = 0.1  # default roughness [m]

    # constants for diagnostics
    env_lapse = 6.5 / 1000.0  # environmental lapse rate [K m^-1]
    grav = 9.80616  # gravity [m s^-2]
    rd = 287.04  # gas constant for dry air [J K^-1 kg^-1]
    z = hgt_agl_diag  # diagnostic height [m]

    # --- Compute diagnostics at height z using simple mode ---

    # temperature correction using lapse rate
    t_z = t2 - (z - 2) * env_lapse

    # barometric equation with varying temperature
    p_z = pres_z0 * (t_z / t2) ** (grav / (rd * env_lapse))

    # humidity assuming invariable relative humidity
    RH_z = ac("RH", qv=q2, p=pres_z0, T=t2)
    q_z = ac("qv", RH=RH_z, p=p_z, T=t_z)

    # wind speed using log law (neutral condition)
    uv_z = uv10 * (np.log((z + z0m) / z0m) / np.log((10 + z0m) / z0m))

    # altitude at diagnostic height
    alt_z = alt_z0 + z

    # create DataFrame with diagnostics
    df_diag = pd.DataFrame(
        {
            "uv_z": uv_z,
            "t_z": t_z,
            "q_z": q_z,
            "RH_z": RH_z,
            "p_z": p_z,
            "alt_z": alt_z,
            # also include surface variables needed downstream
            "ssrd": df["ssrd"],
            "strd": df["strd"],
            "tp": df["tp"],
            "sshf": 0.0,  # not available in timeseries
            "slhf": 0.0,  # not available in timeseries
        },
        index=df.index,
    )

    # add coordinates as attributes (will be used for filename)
    df_diag.attrs = {"latitude": lat, "longitude": lon}

    return df_diag


# diagnose ISL variable using MOST
# diagnose ISL variable using MOST
# save ERA5 forcing dataframe to SUEWS-simulation ready txt files
def save_forcing_era5(df_forcing_era5, dir_save):
    gpb = df_forcing_era5.groupby(["latitude", "longitude"])
    list_grid = list(gpb.groups.keys())
    list_fn = []
    path_dir_save = Path(dir_save)

    # split into grids
    for lat, lon in list_grid:
        df_grid = df_forcing_era5.loc[lat, lon]
        s_lat = f"{lat}N" if lat >= 0 else f"{-lat}S"
        s_lon = f"{lon}E" if lon >= 0 else f"{-lon}W"
        alt_z = df_grid.alt_z.iloc[0]
        df_grid = df_grid.drop("alt_z", axis=1)
        s_alt = f"{alt_z:.1f}A"
        idx_grid = df_grid.index

        # split into years
        grp_year = df_grid.groupby(idx_grid.year)
        for year in grp_year.groups:
            df_year = grp_year.get_group(year)
            idx_year = df_year.index
            s_year = idx_year[0].year
            # Calculate frequency from actual time differences (groupby loses freq metadata)
            time_diff = (
                (idx_year[1] - idx_year[0])
                if len(idx_year) > 1
                else pd.Timedelta("60min")
            )
            s_freq = time_diff / pd.Timedelta("1min")
            s_fn = f"ERA5_UTC-{s_lat}-{s_lon}-{s_alt}_{s_year}_data_{s_freq:.0f}.txt"
            path_fn = path_dir_save / s_fn
            df_year.to_csv(path_fn, sep=" ", index=False)

            # collect file names
            list_fn.append(str(path_fn))

    return list_fn
