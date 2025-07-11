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
def gen_req_sfc(lat_x, lon_x, start, end, grid=None, scale=0):
    """generate a dict of reqs kwargs for (lat_x,lon_x) spanning [start, end]

    Parameters
    ----------
    lat_x : float
        Latitude of centre at the area of interest.
    lon_x : float
        Longitude of centre at the area of interest.
    start : str
        Any datetime-like string that can be parsed by `pandas.daterange()`.
    end : str
        Any datetime-like string that can be parsed by `pandas.daterange()`.
    grid : list, optional
        grid size used in CDS request API, by default [0.125, 0.125].
    scale : int, optional
        scaling factor that determines the area of interest: `area=grid[0]*(2*scale+1)` (i.e., number of grids extending from centre of interest), by default 0.

    Returns
    -------
    dict
        key: name of downloaded file.
        value: CDS API request used for downloading the file named by the corresponding key.

    Note
    ----

        This function uses CDS API to download ERA5 data; follow this for configuration first: https://cds.climate.copernicus.eu/api-how-to

    Examples
    --------
    >>> gen_req_sfc(
    ...     28, 116, "2015-01", "2015-01-31 23", grid=[0.125, 0.125], scale=0
    ... )

    """
    # variable name: data type
    if grid is None:
        grid = [0.125, 0.125]
    dict_var_type_sfc = {
        "10m_u_component_of_wind": "instantaneous",
        "10m_v_component_of_wind": "instantaneous",
        "2m_dewpoint_temperature": "instantaneous",
        "2m_temperature": "instantaneous",
        "geopotential": "instantaneous",
        "surface_pressure": "instantaneous",
        "surface_solar_radiation_downwards": "accumulation",
        "surface_thermal_radiation_downwards": "accumulation",
        "surface_sensible_heat_flux": "accumulation",
        "surface_latent_heat_flux": "accumulation",
        "surface_net_solar_radiation": "accumulation",
        "surface_net_thermal_radiation": "accumulation",
        "total_precipitation": "accumulation",
        "forecast_albedo": "instantaneous",
        "forecast_surface_roughness": "instantaneous",
        "friction_velocity": "instantaneous",
    }
    list_var_sfc = list(dict_var_type_sfc.keys())

    # scale is a factor to rescale grid size
    size = grid[0] * scale
    # generate pd.Series for timestamps
    ser_datetime = pd.date_range(start, end, freq="1h").to_series()
    # surface requests
    lat_c, lon_c = (roundPartial(x, grid[0]) for x in [lat_x, lon_x])
    area = [lat_c + size, lon_c - size, lat_c - size, lon_c + size]
    dict_req_sfc = {
        "variable": list_var_sfc,
        "product_type": "reanalysis",
        "area": area,
        "grid": grid,
        "format": "netcdf",
    }
    list_dict_req_sfc = [
        {**dict_req_sfc, **dict_dt}
        for dict_dt in list(gen_dict_dt_sub(ser_datetime).values())
    ]
    dict_req_sfc = {
        gen_fn(dict_req): gen_dict_proc(dict_req) for dict_req in list_dict_req_sfc
    }
    return dict_req_sfc


def sel_list_pres(ds_sfc_x):
    """
    select proper levels for model level data download
    """
    p_min, p_max = ds_sfc_x.sp.min().values, ds_sfc_x.sp.max().values

    # adjust p_max (p_min) if level for p_max (p_min) is already below (above) that of 1000 (975) hPa
    p_max = p_max if p_max < 1000e2 else 1000e2 - 1
    p_min = p_min if p_min < 900e2 else 900e2 + 1

    list_pres_level = [
        "1",
        "2",
        "3",
        "5",
        "7",
        "10",
        "20",
        "30",
        "50",
        "70",
        "100",
        "125",
        "150",
        "175",
        "200",
        "225",
        "250",
        "300",
        "350",
        "400",
        "450",
        "500",
        "550",
        "600",
        "650",
        "700",
        "750",
        "775",
        "800",
        "825",
        "850",
        "875",
        "900",
        "925",
        "950",
        "975",
        "1000",
    ]
    ser_pres_level = pd.Series(list_pres_level).map(int) * 100
    pos_lev_max, pos_lev_min = (
        ser_pres_level[ser_pres_level > p_max].idxmin(),
        ser_pres_level[ser_pres_level < p_min].idxmax(),
    )
    list_pres_sel = ser_pres_level.loc[pos_lev_min:pos_lev_max] / 100
    list_pres_sel = list_pres_sel.map(int).map(str).to_list()
    return list_pres_sel


# for each sfc data file, determine the necessary vertical levels to model level data download
def gen_req_ml(fn_sfc, grid=None, scale=0):
    if grid is None:
        grid = [0.125, 0.125]
    import xarray as xr

    # variable name: data type
    dict_var_type_ml = {
        "geopotential": "invariant",
        "specific_humidity": "instantaneous",
        "temperature": "instantaneous",
        "u_component_of_wind": "instantaneous",
        "v_component_of_wind": "instantaneous",
    }

    list_var_ml = list(dict_var_type_ml.keys())

    ds_sfc_x = xr.load_dataset(fn_sfc)
    list_pres_sel = sel_list_pres(ds_sfc_x)
    size = grid[0] * scale
    lat_x, lon_x = ds_sfc_x.latitude.values[0], ds_sfc_x.longitude.values[0]
    lat_c, lon_c = (roundPartial(x, grid[0]) for x in [lat_x, lon_x])

    # This is to correct the central coordinate of the area
    lat_c = lat_c - size
    lon_c = lon_c + size

    area = [lat_c + size, lon_c - size, lat_c - size, lon_c + size]
    idx_time = ds_sfc_x.time.to_pandas()
    dict_dt = list(gen_dict_dt_sub(idx_time).values())[0]
    # model level requests
    dict_req_ml = {
        "variable": list_var_ml,
        "product_type": "reanalysis",
        "area": area,
        "grid": grid,
        "format": "netcdf",
    }
    dict_req_ml.update({"level": list_pres_sel})
    dict_req_ml.update(dict_dt)
    dict_req_ml = {gen_fn(dict_req_ml): gen_dict_proc(dict_req_ml)}

    # close nc files
    # ds_sfc_x.close()

    return dict_req_ml


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


def download_era5(
    lat_x: float,
    lon_x: float,
    start: str,
    end: str,
    simple_mode: bool,
    dir_save=Path("."),
    grid=None,
    scale=0,
    logging_level=logging.INFO,
) -> dict:
    """Generate ERA-5 cdsapi-based requests and download data for area of interests.

    Parameters
    ----------
    lat_x : float
        Latitude of centre at the area of interest.
    lon_x : float
        Longitude of centre at the area of interest.
    start : str
        Any datetime-like string that can be parsed by `pandas.daterange()`.
    end : str
        Any datetime-like string that can be parsed by `pandas.daterange()`.
    grid : list, optional
        grid size used in CDS request API, by default [0.125, 0.125].
    scale : int, optional
        scaling factor that determines the area of interest (i.e., `area=grid[0]*scale`), by default 0.
    dir_save: Path or path-like string
        path to directory for saving downloaded ERA5 netCDF files.
    logging_level: logging level
        one of these values [50 (CRITICAL), 40 (ERROR), 30 (WARNING), 20 (INFO), 10 (DEBUG)].
        A lower value informs SuPy for more verbose logging info.

    Returns
    -------
    dict
        key: name of downloaded file.
        value: CDS API request used for downloading the file named by the corresponding key.

    Note
    ----

        This function uses CDS API to download ERA5 data; follow this for configuration first: https://cds.climate.copernicus.eu/api-how-to
    """

    # adjust logging level
    if grid is None:
        grid = [0.125, 0.125]
    logger_supy.setLevel(logging_level)

    # generate requests for surface level data
    dict_req_sfc = gen_req_sfc(lat_x, lon_x, start, end, grid, scale)
    logger_supy.debug(dict_req_sfc)

    # parse and create (if needed) the saving directory
    path_dir_save = Path(dir_save).expanduser().resolve()

    if not path_dir_save.exists():
        path_dir_save.mkdir(parents=True)

    for fn_sfc, dict_req in dict_req_sfc.items():
        download_cds(path_dir_save / fn_sfc, dict_req)

    dict_req_ml = {}
    if simple_mode is False:
        for fn_sfc in dict_req_sfc.keys():
            dict_req = gen_req_ml(path_dir_save / fn_sfc, grid, scale)
            dict_req_ml.update(dict_req)

        logger_supy.debug(dict_req_ml)

        for fn_ml, dict_req in dict_req_ml.items():
            download_cds(path_dir_save / fn_ml, dict_req)

    dict_req_all = {**dict_req_sfc, **dict_req_ml}
    dict_req_all = {
        str(path_dir_save / fn): dict_req for fn, dict_req in dict_req_all.items()
    }

    return dict_req_all


# generate requests
def gen_req_era5(
    lat_x: float,
    lon_x: float,
    start: str,
    end: str,
    simple_mode: bool,
    grid=None,
    scale=0,
    dir_save=Path("."),
) -> dict:
    """Generate ERA-5 cdsapi-based requests and download data for area of interests.

    Parameters
    ----------
    dir_save
    lat_x : float
        Latitude of centre at the area of interest.
    lon_x : float
        Longitude of centre at the area of interest.
    start : str
        Any datetime-like string that can be parsed by `pandas.daterange()`
    end : str
        Any datetime-like string that can be parsed by `pandas.daterange()`
    grid : list, optional
        grid size used in CDS request API, by default [0.125, 0.125]
    scale : int, optional
        scaling factor that determines the area of interest (i.e., `area=grid[0]*scale`), by default 0

    Returns
    -------
    dict
        key: name of downloaded file
        value: CDS API request used for downloading the file named by the corresponding key

    Note
    ----
        1. This function uses CDS API to download ERA5 data; follow this for configuration first: https://cds.climate.copernicus.eu/api-how-to
    """

    # path to directory for saving results
    if grid is None:
        grid = [0.125, 0.125]
    path_dir_save = Path(dir_save).expanduser().resolve()

    # generate requests for surface level data
    dict_req_sfc = gen_req_sfc(lat_x, lon_x, start, end, grid, scale)

    # generate requests for atmospheric level data
    dict_req_ml = {}
    if simple_mode is False:
        for fn_sfc in dict_req_sfc.keys():
            dict_req = gen_req_ml(path_dir_save / fn_sfc, grid, scale)
            dict_req_ml.update(dict_req)

    # collect all requests
    dict_req_all = {**dict_req_sfc, **dict_req_ml}
    dict_req_all = {
        str(path_dir_save / fn): dict_req for fn, dict_req in dict_req_all.items()
    }

    return dict_req_all


# load downloaded files
def load_filelist_era5(
    lat_x: float,
    lon_x: float,
    start: str,
    end: str,
    simple_mode: bool,
    grid=None,
    scale=0,
    dir_save=".",
    force_download=True,
    logging_level=logging.INFO,
):
    path_dir_save = Path(dir_save)
    # download data: existing files will be excluded from the downloading list
    if grid is None:
        grid = [0.125, 0.125]
    if force_download:
        # force download all required files
        download_era5(
            lat_x,
            lon_x,
            start,
            end,
            simple_mode,
            path_dir_save,
            grid,
            scale,
            logging_level,
        )

        # attempt to generate requests
        dict_req_all = gen_req_era5(
            lat_x, lon_x, start, end, simple_mode, grid, scale, path_dir_save
        )

        # downloaded files
        list_fn_sfc = [fn for fn in dict_req_all.keys() if fn.endswith("sfc.nc")]
        list_fn_ml = [fn for fn in dict_req_all.keys() if fn.endswith("ml.nc")]
    else:
        # use files existing in `dir_save`:
        list_fn_sfc = sorted([fn for fn in path_dir_save.glob("*sfc.nc")])
        list_fn_ml = sorted([fn for fn in path_dir_save.glob("*ml.nc")])

    return list_fn_sfc, list_fn_ml


# generate supy forcing using ERA-5 data
def gen_forcing_era5(
    lat_x: float,
    lon_x: float,
    start: str,
    end: str,
    dir_save=Path("."),
    grid=None,
    hgt_agl_diag=100.0,
    scale=0,
    force_download=True,
    simple_mode=True,
    pressure_level=None,
    logging_level=logging.INFO,
) -> list:
    """Generate SUEWS forcing files using ERA-5 data.

    Parameters
    ----------
    lat_x : float
        Latitude of centre at the area of interest.
    lon_x : float
        Longitude of centre at the area of interest.
    start : str
        Any datetime-like string that can be parsed by `pandas.daterange()`.
    end : str
        Any datetime-like string that can be parsed by `pandas.daterange()`.
    dir_save: Path or path-like string
        path to directory for saving downloaded ERA5 netCDF files.
    grid : list, optional
        grid size used in CDS request API, by default [0.125, 0.125].
    hgt_agl_diag: float
        height above ground level to diagnose forcing variables, by default 100; the ground level is taken from ERA5 grid altitude.
    scale : int, optional
        scaling factor that determines the area of interest (i.e., `area=grid[0]*scale`),
        by default 0
    force_download: boolean, optional
        flag to determine whether to download required ERA5 netCDF files; if `False`, all ERA5-related nc files in `dir_save` will be picked up for generation.
        by default True.
    simple_mode: boolean
        if use the *simple* mode for diagnosing the forcing variables, by default `True`.
        In the simple mode, temperature is diagnosed using environmental lapse rate 6.5 K/km and wind speed using MOST under neutral condition.
        If `False`, MOST with consideration of stability conditions will be used to diagnose forcing variables.
    pressure_level: float
        pressure level to retrieve ERA5 atmospheric data, by default None.
        If `None`, this option is ignored.
        If not `None`, calculations implied by `simple_mode` will be skipped: the data at specified pressure level will be used as forcing data and the mean altitude of the pressure level between specified `start` and `end` will be assumed to be the forcing height (i.e., `hgt_agl_diag` will be ignored if set).
    logging_level: logging level
        one of these values [50 (CRITICAL), 40 (ERROR), 30 (WARNING), 20 (INFO), 10 (DEBUG)].
        A lower value informs SuPy for more verbose logging info.


    Returns
    -------
    List
        A list of files in SUEWS forcing input format.

    Note
    ----
        1. This function uses CDS API to download ERA5 data; follow this for configuration first: https://cds.climate.copernicus.eu/api-how-to
        2. The generated forcing files can be imported using `supy.util.read_forcing` to get simulation-ready `pandas.DataFrame`s.
        3. See Section 3.10.2 and 3.10.3 in the reference for details of diagnostics calculation.
        4. For `start`/`end`, it is recommended to use the format `YYYY-MM-DD` to avoid confusion in day/month-first convensions (`an upstream known issue <https://pandas.pydata.org/docs/reference/api/pandas.to_datetime.html#pandas.to_datetime>`_ due to the `dateutil` behavior)

    Reference
    ---------
        ECMWF, S. P. (2016). In IFS documentation CY41R2 Part IV: Physical Processes. ECMWF: Reading, UK, 111-113. https://www.ecmwf.int/en/elibrary/16648-part-iv-physical-processes

    """
    import xarray as xr

    if grid is None:
        grid = [0.125, 0.125]

    # adjust logging level
    logger_supy.setLevel(logging_level)

    # download data
    list_fn_sfc, list_fn_ml = load_filelist_era5(
        lat_x,
        lon_x,
        start,
        end,
        simple_mode,
        grid,
        scale,
        dir_save,
        force_download,
        logging_level,
    )

    # load data from from `sfc` files
    ds_sfc = xr.open_mfdataset(
        list_fn_sfc,
        concat_dim="time",
        combine="nested",
    ).load()
    # close dangling handlers
    ds_sfc.close()

    if pressure_level is None:
        # generate diagnostics at a higher level
        ds_diag = gen_ds_diag_era5(list_fn_sfc, list_fn_ml, hgt_agl_diag, simple_mode)
    else:
        # directly retrieve data at specified pressure level
        ds_diag = get_era5_pressure_level(list_fn_ml, pressure_level)

    # merge diagnostics above with surface variables
    ds_forcing_era5 = ds_sfc.merge(ds_diag)

    # convert to dataframe for further processing
    df_forcing_raw = ds_forcing_era5[
        [
            "ssrd",
            "strd",
            "sshf",
            "slhf",
            "tp",
            "uv_z",
            "t_z",
            "q_z",
            "p_z",
            "alt_z",
            "RH_z",
        ]
    ].to_dataframe()

    # split based on grid coordinates
    grp_grid = df_forcing_raw.groupby(level=["latitude", "longitude"])

    # generate dataframe acceptable by supy
    df_forcing = grp_grid.apply(
        lambda df: format_df_forcing(
            df.reset_index(["latitude", "longitude"], drop=True)
        )
    )

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


# generate supy forcing using ERA-5 data
def gen_ds_diag_era5(list_fn_sfc, list_fn_ml, hgt_agl_diag=100, simple_mode=True):
    import xarray as xr
    from atmosp import calculate as ac

    # load data from from `sfc` files
    ds_sfc = xr.open_mfdataset(
        list_fn_sfc,
        concat_dim="time",
        combine="nested",
    ).load()
    # close dangling handlers
    ds_sfc.close()

    # surface level atmospheric pressure
    pres_z0 = ds_sfc.sp

    # hgt_agl_diag: height where to calculate diagnostics
    # hgt_agl_diag = 100

    # altitude
    alt_z0 = geopotential2geometric(ds_sfc.z, ds_sfc.latitude)

    # ------------------------
    # retrieve surface data

    # wind speed
    u10 = ds_sfc.u10
    v10 = ds_sfc.v10
    uv10 = np.sqrt(u10**2 + v10**2)

    # sensible/latent heat flux [W m-2]
    # conversion from cumulative value to hourly average
    qh = -ds_sfc.sshf / 3600
    qe = -ds_sfc.slhf / 3600

    # surface roughness [m]
    z0m = ds_sfc.fsr

    # friction velocity [m s-1]
    ustar = ds_sfc.zust

    # air temperature
    t2 = ds_sfc.t2m

    # dew point
    d2 = ds_sfc.d2m

    # specific humidity
    q2 = ac("qv", Td=d2, T=t2, p=pres_z0)
    # diagnose wind, temperature and humidity at 100 m agl or `hgt_agl_max` (see below)
    # conform dimensionality using an existing variable
    z = hgt_agl_diag
    da_alt_z = (alt_z0 + z).rename("alt_z")
    ds_alt_z = da_alt_z.to_dataset()

    if simple_mode is False:
        # load data from from `ml` files
        ds_ml = xr.open_mfdataset(
            list_fn_ml,
            concat_dim="time",
            combine="nested",
        ).load()
        # close dangling handlers
        ds_ml.close()

        # hgt_agl_diag: height where to calculate diagnostics
        # hgt_agl_diag = 100

        # determine a lowest level higher than surface at all times
        level_sel = get_level_diag(ds_sfc, ds_ml, hgt_agl_diag)

        # retrieve variables from the identified lowest level
        ds_ll = ds_ml.sel(level=level_sel)

        # altitude
        alt_za = geopotential2geometric(ds_ll.z, ds_ll.latitude)

        # atmospheric pressure [Pa]
        pres_za = pres_z0 * 0 + ds_ll.level * 100

        # u-wind [m s-1]
        u_za = ds_ll.u
        # u-wind [m s-1]
        v_za = ds_ll.v
        # wind speed [m s-1]
        uv_za = np.sqrt(u_za**2 + v_za**2)

        # temperature [K]
        t_za = ds_ll.t

        # specific humidity [kg kg-1]
        q_za = ds_ll.q

        # diagnose wind, temperature and humidity at 100 m agl or `hgt_agl_max` (see below)
        # conform dimensionality using an existing variable
        za = alt_za - alt_z0
        z = za * 0 + hgt_agl_diag
        da_alt_z = (alt_z0 + z).rename("alt_z")
        ds_alt_z = da_alt_z.to_dataset()

    # get dataset of diagnostics
    if simple_mode:
        ds_diag = diag_era5_simple(
            z0m,
            ustar,
            pres_z0,
            uv10,
            t2,
            q2,
            z,
        )
        # merge altitude
        ds_diag = ds_diag.merge(ds_alt_z)
    else:
        ds_diag = diag_era5(
            za,
            uv_za,
            t_za,
            q_za,
            pres_za,
            qh,
            qe,
            z0m,
            ustar,
            pres_z0,
            uv10,
            t2,
            q2,
            z,
        )

        # merge altitude
        ds_diag = ds_diag.merge(ds_alt_z).drop_vars("level")

    return ds_diag


# a simple way to diagnose variables at a higher level
def diag_era5_simple(z0m, ustar, pres_z0, uv10, t2, q2, z):
    from atmosp import calculate as ac
    import xarray as xr

    # constants
    # environmental lapse rate [K m^-1]
    env_lapse = 6.5 / 1000.0
    # gravity [m s^-2]
    grav = 9.80616
    # Gas constant for dry air [J K^-1 kg^-1]
    rd = 287.04

    # correct temperature using lapse rate
    t_z = t2 - (z - 2) * env_lapse

    # barometric equation with varying temperature:
    # (https://en.wikipedia.org/wiki/Barometric_formula)
    p_z = pres_z0 * (t_z / t2) ** (grav / (rd * env_lapse))

    # correct humidity assuming invariable relative humidity
    RH_z = ac("RH", qv=q2, p=pres_z0, T=t2) + 0 * t_z
    q_z = ac("qv", RH=RH_z, p=p_z, T=t_z) + 0 * t_z

    # correct wind speed using log law; assuming neutral condition (without stability correction)
    uv_z = uv10 * (np.log((z + z0m) / z0m) / np.log((10 + z0m) / z0m))

    # generate dataset
    ds_diag = xr.merge([
        uv_z.rename("uv_z"),
        t_z.rename("t_z"),
        q_z.rename("q_z"),
        RH_z.rename("RH_z"),
        p_z.rename("p_z"),
    ])

    return ds_diag


# diagnose ISL variable using MOST
def diag_era5(
    za, uv_za, t_za, q_za, pres_za, qh, qe, z0m, ustar, pres_z0, uv10, t2, q2, z
):
    # reference:
    # Section 3.10.2 and 3.10.3 in
    # IFS Documentation CY41R2: Part IV: Physical Processes
    # https://www.ecmwf.int/en/elibrary/16648-part-iv-physical-processes

    from atmosp import calculate as ac
    import xarray as xr
    from ._atm import cal_lat_vap, cal_cp, cal_psi_mom, cal_psi_heat

    # von Karman constant
    kappa = 0.4

    # gravity acceleration
    g = 9.8

    # note the roughness correction: see EC technical report
    z0m = np.where(z0m < 0.03, z0m, 0.03)

    # air density
    avdens = ac("rho", qv=q2, p=pres_z0, theta=t2)

    # vapour pressure
    # lv_j_kg = cal_lat_vap(q2, t2, pres_z0)

    # heat capacity
    avcp = cal_cp(q2, t2, pres_z0)

    # temperature/humidity scales
    tstar = -qh / (avcp * avdens) / ustar
    # qstar = -qe / (lv_j_kg * avdens) / ustar

    # Obukhov length
    l_mod = ustar**2 / (g / t2 * kappa * tstar)
    zoL = np.where(
        np.abs((z + z0m) / l_mod) < 5,
        (z + z0m) / l_mod,
        np.sign((z + z0m) / l_mod) * 5,
    )

    # stability correction for momentum
    psim_z = cal_psi_mom(zoL)
    psim_z0 = cal_psi_mom(z0m / l_mod)
    psim_10 = cal_psi_mom((10 + z0m) / l_mod)

    # wind speed
    uv_z = uv10 * (
        (np.log((z + z0m) / z0m) - psim_z + psim_z0)
        / (np.log((10 + z0m) / z0m) - psim_10 + psim_z0)
    )

    # stability correction for heat
    psih_z = cal_psi_heat(zoL)
    psih_2 = cal_psi_heat(2 / l_mod)
    psih_z0 = cal_psi_heat(z0m / l_mod)
    psih_za = cal_psi_heat(za / l_mod)

    # atmospheric pressure: assuming same air density at `za`
    # using iteration to get `p_z`
    p_z = pres_z0 + (pres_za - pres_z0) * z / za

    # specific humidity
    q_z = q2 + (q_za - q2) * (
        (np.log(z / z0m) - psih_z + psih_z0) / (np.log(za / z0m) - psih_za + psih_z0)
    )

    # dry static energy: eq 3.5 in EC tech report;
    # also AMS ref: http://glossary.ametsoc.org/wiki/Dry_static_energy
    # 2 m agl:
    cp2 = cal_cp(q2, t2, pres_z0 / 100)
    s2 = g * 2 + cp2 * t2
    # za:
    cp_za = cal_cp(q_za, t_za, pres_za / 100)
    s_za = g * za + cp_za * t_za

    s_z = s2 + (s_za - s2) * (
        (np.log(z / 2) - psih_z + psih_2) / (np.log(za / 2) - psih_za + psih_2)
    )

    # calculate temperature at z
    tx_z = t_za
    dif = 10
    while dif > 0.1:
        cp_z = cal_cp(q_z, tx_z, p_z / 100)
        t_z = (s_z - g * z) / cp_z
        dif = np.mean(np.abs(t_z - tx_z))
        tx_z = t_z
    # get final `t_z` and store in the conformity to `t_za`
    t_z = tx_z + t_za * 0

    # relative humidity; cap at 105% if above
    RH_z = ac("RH", qv=q_z, p=p_z, T=t_z) + 0 * q_z
    RH_z = RH_z.where(RH_z < 105, 105)

    # generate dataset
    ds_diag = xr.merge([
        uv_z.rename("uv_z"),
        t_z.rename("t_z"),
        q_z.rename("q_z"),
        RH_z.rename("RH_z"),
        p_z.rename("p_z"),
    ])
    return ds_diag


# diagnose ISL variable using MOST
def get_era5_pressure_level(list_fn_ml, pressure_level):
    from atmosp import calculate as ac
    import xarray as xr
    from ._atm import cal_lat_vap, cal_cp, cal_psi_mom, cal_psi_heat

    # load data from from `ml` files
    ds_ml = xr.open_mfdataset(
        list_fn_ml,
        concat_dim="time",
        combine="nested",
    ).load()
    # close dangling handlers
    ds_ml.close()

    # retrieve data at specified pressure level
    try:
        ds_ml_p = ds_ml.sel(level=pressure_level)
    except KeyError:
        raise ValueError(
            f"Pressure level {pressure_level} not found in the dataset. "
            f"Available levels: {ds_ml.level.values}"
        )

    # altitude: use mean value of chosen period
    alt_za = geopotential2geometric(ds_ml_p.z, ds_ml_p.latitude)
    da_alt_z = alt_za.mean() + alt_za * 0
    ds_alt_z = da_alt_z.rename("alt_z").to_dataset()

    # wind speed
    uv_z = (ds_ml_p.u**2 + ds_ml_p.v**2) ** 0.5

    # air temperature
    t_z = ds_ml_p.t

    # atmospheric pressure (hPa to Pa)
    p_z = pressure_level * 100 + t_z * 0

    # specific humidity
    q_z = ds_ml_p.q

    # relative humidity; cap at 105% if above
    RH_z = ac("RH", qv=q_z, p=p_z, T=t_z) + 0 * q_z
    RH_z = RH_z.where(RH_z < 105, 105)

    # generate dataset
    ds_diag = xr.merge([
        uv_z.rename("uv_z"),
        t_z.rename("t_z"),
        q_z.rename("q_z"),
        RH_z.rename("RH_z"),
        p_z.rename("p_z"),
    ])

    # merge altitude
    ds_diag = ds_diag.merge(ds_alt_z).drop_vars("level")
    return ds_diag


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
            s_freq = idx_year.freq / pd.Timedelta("1T")
            s_fn = f"ERA5_UTC-{s_lat}-{s_lon}-{s_alt}_{s_year}_data_{s_freq:.0f}.txt"
            path_fn = path_dir_save / s_fn
            df_year.to_csv(path_fn, sep=" ", index=False)

            # collect file names
            list_fn.append(str(path_fn))

    return list_fn


def get_level_diag(ds_sfc, ds_ml, hgt_agl_diag):
    # get altitude from `sfc` files
    da_gph_sfc = ds_sfc.z
    da_lat_sfc = da_gph_sfc.latitude
    da_alt_sfc = geopotential2geometric(da_gph_sfc, da_lat_sfc)

    # get altitude from `ml` files
    da_gph_ml = ds_ml.z
    da_lat_ml = da_gph_ml.latitude
    da_alt_ml = geopotential2geometric(da_gph_ml, da_lat_ml)

    # determine a lowest level higher than surface at all times
    ind_alt = ((da_alt_sfc + hgt_agl_diag) < da_alt_ml).compute()
    level_lowest = ind_alt.sum(dim="level") - 1
    level_sel = da_alt_ml.level[level_lowest]

    return level_sel
