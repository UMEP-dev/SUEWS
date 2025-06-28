#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 17 14:38:16 2022
Author: Csilla V Gal

"""

import pandas as pd
import numpy as np
import supy
from metpy.units import units
from metpy.calc import (
    mixing_ratio_from_relative_humidity,
    specific_humidity_from_mixing_ratio,
)

from pathlib import Path


def convert_UMEPf2epw(path_txt, lat, lon, tz, alt=None, path_epw=None):
    """
    Converts and saves a UMEP-generated, year-long forcing file (.txt)
    with hourly resolution to .epw

    NOTE: A small function is added now at the end to
    clear up .epw file formatting and it's header.
    It can be removed or some aspects integrated into supy.util.gen_epw.

    Parameters
    ----------
    path_txt : path
        Path to the .txt file.
    lat : float
        Latitude of the site.
    lon : float
        Longitude of the site.
    tz : float
        Time zone expressed as a difference from UTC+0, such as -8 for UTC-8.
    alt : float, optional
        Altitude of the site.
    path_epw : path,optional
        Path to the new .epw file.

    Returns
    -------
    df_epw, text_meta, path_epw: Tuple[pd.DataFrame, str, Path]
        - df_epw: uTMY result
        - text_meta: meta-info text
        - path_epw: path to generated `epw` file
    """
    # Dictionary for parameter naming conversion (UMEP to SUEWS)
    # NOTE: To be used with with 1.A column naming option, otherwise redundant.
    dict_var = {
        "%iy": "iy",
        "id": "id",
        "it": "it",
        "imin": "imin",
        "Q*": "QN",
        "QH": "QH",
        "QE": "QE",
        "Qs": "QS",
        "Qf": "QF",
        "Wind": "U10",
        "RH": "RH2",
        "Td": "T2",
        "press": "pres",
        "rain": "Rain",
        "Kdn": "Kdown",
        "snow": "snow",
        "ldown": "Ldown",
        "fcld": "Fcld",
        "wuh": "Wuh",
        "xsmd": "xsmd",
        "lai_hr": "LAI",
        "Kdiff": "kdiff",
        "Kdir": "kdir",
        "Wd": "wdir",
        "isec": "isec",
    }

    # List of the stanard UMEP column names (the header)
    # NOTE: Can be used with with 1.A column naming option, otherwise redundant.
    header_UMEP = [
        "%iy",
        "id",
        "it",
        "imin",
        "Q*",
        "QH",
        "QE",
        "Qs",
        "Qf",
        "Wind",
        "RH",
        "Td",
        "press",
        "rain",
        "Kdn",
        "snow",
        "ldown",
        "fcld",
        "wuh",
        "xsmd",
        "lai_hr",
        "Kdiff",
        "Kdir",
        "Wd",
    ]

    # List of the stanard UMEP column names converted to SUEWS naming standard (the renamed header)
    # NOTE: To be used with with 1.B column naming option, otherwise redundant.
    header_SUEWS = [
        "iy",
        "id",
        "it",
        "imin",
        "QN",
        "QH",
        "QE",
        "QS",
        "QF",
        "U10",
        "RH2",
        "T2",
        "pres",
        "Rain",
        "Kdown",
        "snow",
        "Ldown",
        "Fcld",
        "Wuh",
        "xsmd",
        "LAI",
        "kdiff",
        "kdir",
        "wdir",
    ]

    if path_epw is None:
        path_epw = path_txt[:-4] + ".epw"

    # (0) Load file
    df_data = pd.read_table(path_txt, engine="python", delimiter=" +")

    # (1) Fix column names to work with supy.util.gen_epw (UMEP > SUEWS)

    # # Case A: with renaming (when reading table is not an issue)
    # # NOTE: When correctly loaded: df_data.columns == header_UMEP
    # df_data.rename(columns=dict_var, inplace=True)

    # Case B: with tour-de-force naming (when reading table might be an issue)
    df_data.columns = header_SUEWS

    # (2) Fix index
    df_data.index = pd.to_datetime(
        df_data.iy.astype(str)
        + " "
        + df_data.id.astype(str)
        + " "
        + df_data.it.astype(str)
        + " "
        + df_data.imin.astype(str),
        format="%Y %j %H %M",
    )

    # (3) Convert -999 values to NaN & remove columns with NaN
    # NOTE: The parameters that supy.util.gen_epw needs are: ['Kdown','Ldown','U10','T2','RH2',Q2].
    # NOTE: All extra data could be copied over.
    df_data.replace(-999, np.nan, inplace=True)
    df_data.dropna(axis=1, inplace=True)

    # (4) Match units with SUEWS (kPa > hPa)
    # NOTE: This value is not used by supy.util.gen_epw and units used for atmos. pressure in epw is Pa.
    df_data["pres"] = df_data["pres"] * 10

    # (5) Calculate specific humidity (Q2) for supy.util.gen_epw
    temperature = df_data["T2"].values * units.degC
    relhum = df_data["RH2"].values * units.percent
    press = df_data["pres"].values * units.hPa

    mixing_ratio = mixing_ratio_from_relative_humidity(press, temperature, relhum)
    spec_hum = specific_humidity_from_mixing_ratio(mixing_ratio) * units("kg/kg")
    df_data["Q2"] = spec_hum.to("g/kg")

    # (6) Save data with supy.util.gen_epw
    data_epw, header_epw, path_2epw = supy.util.gen_epw(df_data, lat, lon, tz, path_epw)

    # (7) Patch up the generated .epw.
    # NOTE: This can be turned off/removed.
    data_epw, header_epw, path_2epw = patchup_epw(
        data_epw, header_epw, path_2epw, lat, lon, tz, alt
    )

    return data_epw, header_epw, path_2epw


def patchup_epw(df_data, df_header, path_epw, lat, lon, tz, alt):
    """
    Fixes supy.util.gen_epw-generated .epw file headers, NaN values and rounding rulsed.
    Changes done after https://bigladdersoftware.com/epx/docs/8-3/auxiliary-programs/energyplus-weather-file-epw-data-dictionary.html

    Parameters
    ----------
    data_epw : pd.DataFrame
        The .epw file data table. (Generated by supy.util.gen_epw.)
    header_epw : list
        The .epw file header. (Generated by supy.util.gen_epw.)
    path_epw : path
        The path to the generated .epw file. (After supy.util.gen_epw.)
    lat : float
        Latitude of the site. (Input to convert_UMEPf2epw function.)
    lon : float
        Longitude of the site. (Input to convert_UMEPf2epw function.)
    tz : float
        Time zone expressed as a difference from UTC+0, such as -8 for UTC-8.
        (Input to convert_UMEPf2epw function.)
    alt : float, optional
        Altitude of the site. (Optional input to convert_UMEPf2epw function.)

    Returns
    -------
    df_data,df_header,path_epw : Tuple[pd.DataFrame, list, Path]
        - df_data : the epw data table
        - df_header : the epw header
        - path_epw : path to generated .epw file

    """
    if alt is None:
        alt = "NA"
    else:
        alt = str(alt)

    # DATA
    # Fixing roundings
    df_data.iloc[:, :5] = df_data.iloc[:, :5].astype(int)
    df_data.iloc[:, 6:8] = df_data.iloc[:, 6:8].round(1)
    df_data.iloc[:, 8:21] = df_data.iloc[:, 8:21].round(0).astype(int)
    df_data.iloc[:, 21] = df_data.iloc[:, 21].round(1)  # Wind Speed
    df_data.iloc[:, 22:29] = df_data.iloc[:, 22:29].round(0).astype(int)
    df_data.iloc[:, 29] = df_data.iloc[:, 29].round(3)  # Aerosol Optical Depth >> 0.999
    df_data.iloc[:, 30:] = df_data.iloc[:, 30:].round(0).astype(int)

    # Fixing NaN values
    # Global Horizontal Illuminance, Direct Normal Illuminance, Diffuse Horizontal Illuminance
    df_data.iloc[:, 16:19] = 999999
    # Present Weather Observation
    df_data.iloc[:, 26] = 9
    # Present Weather Codes
    df_data.iloc[:, 27] = 999999999
    # Aerosol Optical Depth
    df_data.iloc[:, 29] = 0.999
    # Liquid Precipitation Quantity
    df_data.iloc[:, 34] = 99

    # HEADER
    # First Line :: LOCATION,NA,NA,NA,UMEP/SuPy,NA,47.50,19.12,1.0,163.0
    line_first = (
        "LOCATION,NA,NA,NA,UMEP/SuPy,NA,"
        + str(lat)
        + ","
        + str(lon)
        + ","
        + str(tz)
        + ","
        + alt
    )
    # Last Line :: DATA PERIODS,1,1,Year_2013,Tuesday,2013/ 1/ 1,2013/12/31
    line_last = (
        "DATA PERIODS,1,1,Year_"
        + df_data.index[0].strftime("%Y")
        + ","
        + df_data.index[0].strftime("%A")
        + ","
        + df_data.index[0].strftime("%-m/%-d")
        + ","
        + df_data.index[-2].strftime("%-m/%-d")
    )

    df_header[0] = line_first
    df_header[1] = "DESIGN CONDITIONS,0"
    df_header[2] = "TYPICAL/EXTREME PERIODS,0"
    df_header[3] = "GROUND TEMPERATURES,0"
    df_header[4] = "HOLIDAYS/DAYLIGHT SAVINGS,No,0,0,0"
    df_header[5] = "COMMENTS 1,Generated by SuPy"
    df_header[6] = "COMMENTS 2,Generated from user-provided data"
    df_header[7] = line_last

    # OverWRITE epw
    filenew = open(path_epw, "w")
    for i in range(len(df_header)):
        filenew.write(df_header[i] + "\n")
    filenew.close()
    df_data.to_csv(path_epw, header=None, index=None, mode="a", line_terminator="\n")

    # Convert results to SuPy standards
    path_epw = Path(path_epw)  # Path FIX

    return df_data, df_header, path_epw


# EXAMPLE
# lat = 47.5
# lon = 19.0
# tz = 1
# alt = 100
# path_UMEPfrcg = 'Budapest-Lorinc.txt'
# path_SAVEepw = path_UMEPfrcg[:-4] + '-TEST.epw'

# df_data,list_header,path_file =  convert_UMEPf2epw(path_UMEPfrcg,lat,lon,tz,alt=100,path_epw=path_SAVEepw)
