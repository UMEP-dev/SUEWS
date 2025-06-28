import functools
from datetime import timedelta
from pathlib import Path

import f90nml
import numpy as np
import pandas as pd


from pathlib import Path

import pandas as pd
from packaging import version


from . import _supy_driver as _sd
from . import supy_driver as sd


from ._env import logger_supy, trv_supy_module
from ._misc import path_insensitive

# choose different second representation to accommodate different pandas versions
# pandas version <1.5
str_second = "S" if version.parse(pd.__version__) < version.parse("1.5.0") else "s"


########################################################################
# get_args_suews can get the interface information
# of the f2py-converted Fortran interface
def get_args_suews(docstring=_sd.f90wrap_suews_driver__suews_cal_multitsteps.__doc__):
    # split doc lines for processing
    docLines = np.array(docstring.splitlines(), dtype=str)

    dict_inout_sd = extract_dict_docs(docLines)

    return dict_inout_sd


def extract_dict_docs(docLines):
    # get the information of input variables for SUEWS_driver
    ser_docs = pd.Series(docLines)
    varInputLines = ser_docs[ser_docs.str.contains("input|in/output")].values
    varInputInfo = np.array([
        [xx.rstrip() for xx in x.split(":")] for x in varInputLines
    ])
    # varInputInfo
    dict_InputInfo = {xx[0]: xx[1] for xx in varInputInfo}
    dict_InOutInfo = {xx[0]: xx[1] for xx in varInputInfo if "in/out" in xx[1]}

    list_var_inout = tuple(dict_InOutInfo.keys())
    list_var_in = tuple(set(dict_InputInfo.keys()))
    # temporary fix for state variables
    list_var_in = list(set(list_var_in) - set(["state_debug", "block_mod_state"]))
    # dict_InOutInfo
    # get the information of output variables for SUEWS_driver

    varOutputLines = ser_docs[ser_docs.str.startswith(("dataout", "dailystate"))].values

    # varOutputLines = docLines[posOutput[0][0] + 2 :]
    varOutputInfo = np.array([
        [xx.rstrip() for xx in x.split(":")] for x in varOutputLines
    ])
    # varOutputInfo
    dict_OutputInfo = {xx[0]: xx[1] for xx in varOutputInfo}
    list_var_out = tuple(dict_OutputInfo.keys())
    list_var_inout = set(list_var_inout) - set(list_var_out)
    # list_var_in= set(list_var_in) - set(list_var_out)
    # pack in/out results:
    dict_inout_sd = {
        # 'input' and 'output' are dict's that store variable information:
        # 1. intent: e.g., input, in/output
        # 2. dimension: e.g., (366,7)
        "input": dict_InputInfo,
        "output": dict_OutputInfo,
        # 'var_input' and 'var_output' are tuples,
        # that keep the order of arguments as in the Fortran subroutine
        "var_input": list_var_in,
        "var_inout": list_var_inout,
        "var_output": list_var_out,
    }
    return dict_inout_sd


# for `suews_cal_multitsteps`
def get_args_suews_multitsteps():
    # split doc lines for processing
    docLines = np.array(
        _sd.f90wrap_suews_driver__suews_cal_multitsteps.__doc__.splitlines(), dtype=str
    )

    dict_inout_sd = extract_dict_docs(docLines)

    return dict_inout_sd


# store these lists for later use
list_var_input = list(get_args_suews()["var_input"])
list_var_inout = list(get_args_suews()["var_inout"])
list_var_output = list(get_args_suews()["var_output"])
set_var_input = set(list_var_input)
set_var_inout = set(list_var_inout)
set_var_ouput = set(list_var_output)

list_var_input_multitsteps = list(get_args_suews_multitsteps()["var_input"])
list_var_inout_multitsteps = list(get_args_suews_multitsteps()["var_inout"])
list_var_output_multitsteps = list(get_args_suews_multitsteps()["var_output"])
set_var_input_multitsteps = set(list_var_input_multitsteps)
set_var_inout_multitsteps = set(list_var_inout_multitsteps)
set_var_ouput_multitsteps = set(list_var_output_multitsteps)

# variables used in df_state
set_var_use = set_var_input_multitsteps
# set_var_use = set_var_input.intersection(set_var_input_multitsteps)

##############################################################################
# input processor
# 1. surface properties will be retrieved and packed together for later use
# 2. met forcing conditions will split into time steps and used to derive
# other information

######################################################################
# descriptive list/dicts for variables
# minimal required input files for configuration:
list_file_input = [
    "SUEWS_AnthropogenicEmission.txt",
    "SUEWS_BiogenCO2.txt",
    "SUEWS_Conductance.txt",
    "SUEWS_ESTMCoefficients.txt",
    "SUEWS_Irrigation.txt",
    "SUEWS_NonVeg.txt",
    "SUEWS_OHMCoefficients.txt",
    "SUEWS_Profiles.txt",
    "SUEWS_SiteSelect.txt",
    "SUEWS_Snow.txt",
    "SUEWS_Soil.txt",
    "SUEWS_Veg.txt",
    "SUEWS_Water.txt",
    "SUEWS_WithinGridWaterDist.txt",
]

# library of all properties
dict_libVar2File = {
    fileX.replace(".txt", "").replace("SUEWS", "lib"): fileX
    for fileX in list_file_input
    if fileX.endswith(".txt")
}

# dictionaries:
# links between code in SiteSelect to properties in according tables
# this is described in SUEWS online manual:
# https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteInfo/SUEWS_SiteInfo.html
path_code2file = trv_supy_module.joinpath("code2file.json")
dict_Code2File = pd.read_json(
    path_code2file,
    typ="series",
    convert_dates=False,
).to_dict()

# variable translation as done in Fortran-SUEWS
path_var2siteselect = trv_supy_module.joinpath("var2siteselect.json")
dict_var2SiteSelect = pd.read_json(
    path_var2siteselect,
    typ="series",
    convert_dates=False,
).to_dict()

# expand dict_Code2File for retrieving surface characteristics
dict_varSiteSelect2File = {
    x: "SUEWS_SiteSelect.txt" for x in dict_var2SiteSelect.keys()
}
dict_Code2File.update(dict_varSiteSelect2File)


# define data types for different resampling schemes
# time: temporal info
# avg: average values of period ending at timestamps
# inst: instantaneous values at timestamps
# sum: sum of period ending at timestamps
dict_var_type_forcing = {
    "iy": "time",
    "id": "time",
    "it": "time",
    "imin": "time",
    "qn": "avg",
    "qh": "avg",
    "qe": "avg",
    "qs": "avg",
    "qf": "avg",
    "U": "inst",
    "RH": "inst",
    "Tair": "inst",
    "pres": "inst",
    "rain": "sum",
    "kdown": "avg",
    "snow": "inst",
    "ldown": "avg",
    "fcld": "inst",
    "Wuh": "sum",
    "xsmd": "inst",
    "lai": "inst",
    "kdiff": "avg",
    "kdir": "avg",
    "wdir": "inst",
    "isec": "time",
}


######################################################################
# functions
# extract metainfo of var from dict_info
def extract_var_info(var_name, dict_info):
    dict_var_info = {"name": var_name}
    var_line = dict_info[var_name]
    list_var_line = var_line.split()
    if "array" in var_line:
        dict_var_info.update({"intent": list_var_line[0]})
        # print(list_var_line)
        rank = int(list_var_line[1].split("-")[-1])
        dict_var_info.update({"rank": rank})
        dtype = list_var_line[2]
        dict_var_info.update({"dtype": dtype})
        if rank > 0:
            bounds = list_var_line[-1]
            dict_var_info.update({"bounds": bounds})
        else:
            dict_var_info.update({"bounds": 0})
    else:
        dict_var_info.update({"intent": list_var_line[0]})
        rank = 0
        dict_var_info.update({"rank": rank})
        dtype = list_var_line[1]
        dict_var_info.update({"dtype": dtype})
        dict_var_info.update({"bounds": 0})
    return dict_var_info


# generate DataFrame of docstring for suews_wrappers generated by f2py
def gen_suews_arg_info_df(docstring):
    dict_info = get_args_suews(docstring)["input"]
    # dict_info.update(get_args_suews(docstring)['output'])
    dict_info = {var: extract_var_info(var, dict_info) for var in dict_info}
    df_info = pd.DataFrame(dict_info).T
    return df_info


# note: infer data types for variables to avoid type conversion
df_info_suews_cal_multitsteps = gen_suews_arg_info_df(
    _sd.f90wrap_suews_driver__suews_cal_multitsteps.__doc__
).infer_objects()

df_var_info = df_info_suews_cal_multitsteps.set_index("name")


# load model settings
# load configurations: mod_config
# process RunControl.nml
# this function can handle all SUEWS nml files
def load_SUEWS_nml_simple(path_file):
    # remove case issues
    # xfile = path_insensitive(xfile)
    try:
        path_file_x = Path(path_insensitive(str(path_file)))
        path_file = path_file_x.resolve()
        str_file = path_insensitive(str(path_file))
        df_res = pd.DataFrame(f90nml.read(str_file))
        return df_res
    except FileNotFoundError:
        logger_supy.exception(f"{path_file} does not exists!")


@functools.lru_cache(maxsize=128)
def load_SUEWS_nml(p_nml):
    try:
        p_nml = Path(path_insensitive(str(p_nml))).resolve()
        parser = f90nml.Parser()
        parser.row_major = True
        df_nml = pd.DataFrame(parser.read(p_nml))
        dict_nml_raw = {
            name: np.array(row.dropna().values[0]) for name, row in df_nml.iterrows()
        }
        dict_nml = {}
        for k, v in dict_nml_raw.items():
            # print(k, v, type(v))
            dict_nml.update(expand_entry(k, v))
        return dict_nml
    except FileNotFoundError:
        logger_supy.exception(f"{p_nml} does not exists!")


# load all tables (xgrid.e., txt files)
@functools.lru_cache(maxsize=128)
def load_SUEWS_table(path_file):
    # remove case issues
    try:
        path_file = path_file.resolve()
    except FileNotFoundError:
        logger_supy.exception(f"{path_file} does not exists!")
    else:
        try:
            rawdata = pd.read_csv(
                path_file,
                sep=r"\s+",  # any whitespace
                comment="!",
                on_bad_lines="error",
                encoding_errors="ignore",
                skiprows=1,
                index_col=0,
            )
            rawdata = rawdata.dropna()
            rawdata.index = rawdata.index.astype(int)
            return rawdata
        except Exception as err:
            logger_supy.exception(f"error {err} in reading {path_file}!")


# load all tables into variables staring with 'lib_' and filename
def load_SUEWS_Libs(path_input):
    dict_libs = {}
    for lib, lib_file in dict_libVar2File.items():
        # print(lib_file)
        # lib_path = os.path.join(path_input, lib_file)
        lib_path = path_input / lib_file
        dict_libs.update({lib: load_SUEWS_table(lib_path)})
    # return DataFrame containing settings
    return dict_libs


# look up properties according to code
@functools.lru_cache(maxsize=None)
def lookup_code_lib(libCode, codeKey, codeValue, path_input):
    str_lib = dict_Code2File[libCode].replace(".txt", "").replace("SUEWS", "lib")
    dict_libs = load_SUEWS_Libs(path_input)
    lib = dict_libs[str_lib]
    # print(str_lib,codeKey,codeValue)
    if codeKey == ":":
        res = lib.loc[int(np.unique(codeValue)), :].tolist()
    else:
        res = lib.loc[int(np.unique(codeValue)), codeKey]
        if isinstance(res, pd.Series):
            # drop duolicates, otherwise duplicate values
            # will be introduced with more complexity
            res = res.drop_duplicates()
            res = res.values[0] if res.size == 1 else res.tolist()
    return res


# a recursive function to retrieve value based on key sequences
@functools.lru_cache(maxsize=None)
def lookup_KeySeq_lib(indexKey, subKey, indexCode, path_input):
    if isinstance(subKey, float):
        res = subKey
    # elif indexKey == 'const':
    #     res = subKey
    elif type(subKey) is str:
        res = lookup_code_lib(indexKey, subKey, indexCode, path_input)
    elif isinstance(subKey, dict):
        # elif isinstance(subKey, HDict):
        res = []
        for indexKeyX, subKeyX in subKey.items():
            # print(indexKeyX, subKeyX)
            if indexKeyX == "const":
                resX = subKeyX
            else:
                # indexKeyX, subKeyX = list(subKey.items())[0]
                indexCodeX = lookup_code_lib(indexKey, indexKeyX, indexCode, path_input)
                if isinstance(subKeyX, list):
                    resX = [
                        lookup_KeySeq_lib(indexKeyX, x_subKeyX, indexCodeX, path_input)
                        for x_subKeyX in subKeyX
                    ]
                else:
                    resX = lookup_KeySeq_lib(indexKeyX, subKeyX, indexCodeX, path_input)
            res.append(resX)
        res = res[0] if len(res) == 1 else res
    elif isinstance(subKey, list):
        # elif isinstance(subKey, HList):
        res = []
        for subKeyX in subKey:
            indexCodeX = indexCode
            resX = lookup_KeySeq_lib(indexKey, subKeyX, indexCodeX, path_input)
            res.append(resX)
        res = res[0] if len(res) == 1 else res
    # final result
    return res


# load surface characteristics
def load_SUEWS_SurfaceChar(path_input):
    # load all libraries
    dict_libs = load_SUEWS_Libs(path_input)
    list_grid = dict_libs["lib_SiteSelect"].index
    # construct a dictionary in the form: {grid:{var:value,...}}
    dict_gridSurfaceChar = {
        grid: {
            k: lookup_KeySeq_lib(k, v, grid, path_input)
            for k, v in dict_var2SiteSelect.items()
        }
        for grid in list_grid
    }
    # convert the above dict to DataFrame
    df_gridSurfaceChar = pd.DataFrame.from_dict(dict_gridSurfaceChar).T
    # empty dict to hold updated values
    # dict_x_grid = {}
    # modify some variables to be compliant with SUEWS requirement
    for xgrid in df_gridSurfaceChar.index:
        # transpoe snowprof:
        df_gridSurfaceChar.at[xgrid, "snowprof_24hr"] = np.array(
            df_gridSurfaceChar.at[xgrid, "snowprof_24hr"], order="F"
        ).T

        # transpoe laipower:
        df_gridSurfaceChar.at[xgrid, "laipower"] = np.array(
            df_gridSurfaceChar.at[xgrid, "laipower"], order="F"
        ).T

        # select non-zero values for waterdist of water surface:
        x = np.array(df_gridSurfaceChar.at[xgrid, "waterdist"][-1])
        df_gridSurfaceChar.at[xgrid, "waterdist"][-1] = x[np.nonzero(x)]

        # surf order as F:
        df_gridSurfaceChar.at[xgrid, "storedrainprm"] = np.array(
            df_gridSurfaceChar.at[xgrid, "storedrainprm"], order="F"
        )

        # convert to np.array
        df_gridSurfaceChar.at[xgrid, "alb"] = np.array(
            df_gridSurfaceChar.at[xgrid, "alb"]
        )

        # convert unit of `surfacearea` from ha to m^2 for table-based inputs
        df_gridSurfaceChar.at[xgrid, "surfacearea"] = np.array(
            df_gridSurfaceChar.at[xgrid, "surfacearea"] * 10000.0
        )

        # print type(df_gridSurfaceChar.loc[xgrid, 'alb'])

        # dict holding updated values that can be converted to DataFrame later
        # dict_x = df_gridSurfaceChar.loc[xgrid, :].to_dict()
        # print 'len(dict_x)',len(dict_x['laipower'])

        # profiles:
        list_varTstep = [
            "ahprof_24hr",
            "popprof_24hr",
            "traffprof_24hr",
            "humactivity_24hr",
            "wuprofm_24hr",
            "wuprofa_24hr",
        ]
        df_gridSurfaceChar.loc[xgrid, list_varTstep] = df_gridSurfaceChar.loc[
            xgrid, list_varTstep
        ].map(np.transpose)

    # convert to DataFrame
    # df_x_grid = pd.DataFrame.from_dict(dict_x_grid).T
    df_x_grid = df_gridSurfaceChar
    return df_x_grid


def func_parse_date(year, doy, hour, minute):
    dt = pd.to_datetime(
        " ".join([str(k) for k in [year, doy, hour, minute]]), format="%Y %j %H %M"
    )
    return dt


def func_parse_date_row(row):
    [year, doy, hour, tmin] = row.loc[["iy", "id", "it", "imin"]]
    # dt = datetime.datetime.strptime(
    #     ' '.join([year, doy, hour, min]), '%Y %j %H %M')
    dt = pd.to_datetime(
        " ".join([str(k) for k in [year, doy, hour, tmin]]), format="%Y %j %H %M"
    )
    return dt


# calculate decimal time
def dectime(timestamp):
    t = timestamp
    time_dec = (t.dayofyear - 1) + (t.hour + (t.minute + t.second / 60.0) / 60.0) / 24
    return time_dec


# resample solar radiation by zenith correction and total amount distribution
def resample_kdn(data_raw_kdn, tstep_mod, timezone, lat, lon, alt):
    # adjust solar radiation
    datetime_mid_local = data_raw_kdn.index - timedelta(seconds=tstep_mod / 2)
    sol_elev = np.array([
        _sd.f90wrap_suews_cal_sunposition(t.year, dectime(t), timezone, lat, lon, alt)[
            -1
        ]
        for t in datetime_mid_local
    ])
    sol_elev_reset = np.zeros_like(sol_elev)
    sol_elev_reset[sol_elev <= 90] = 1.0
    data_tstep_kdn_adj = sol_elev_reset * data_raw_kdn.copy()

    # calculate daily mean values for later rescaling
    avg_raw = data_raw_kdn.resample("D").mean()
    avg_tstep = data_tstep_kdn_adj.resample("D").mean()
    # calculate rescaling ratio
    ratio_SWdown = avg_raw / avg_tstep
    # replace nan with zero as `avg_tstep` might be zero if incomplete days included
    ratio_SWdown = ratio_SWdown.fillna(0)
    # conform to `avg_tstep` index for actual days used
    ratio_SWdown = ratio_SWdown.reindex(index=avg_tstep.index)
    # resample into `tstep_mod` steps
    ratio_SWdown = ratio_SWdown.resample(f"{tstep_mod}{str_second}").mean()
    # fill nan as the above resample will place valid values at the daily start
    ratio_SWdown = ratio_SWdown.fillna(method="pad")
    # conform to `data_tstep_kdn_adj` index for actual period used
    ratio_SWdown = ratio_SWdown.reindex(index=data_tstep_kdn_adj.index)
    # fill nan as the above resample will place valid values at the daily start
    ratio_SWdown = ratio_SWdown.fillna(method="pad")
    # rescale daily amounts
    data_tstep_kdn_adj = ratio_SWdown * data_tstep_kdn_adj
    # data_tstep_kdn_adj = data_tstep_kdn_adj.fillna(method='pad')

    return data_tstep_kdn_adj


# correct precipitation by even redistribution over resampled periods
def resample_sum(data_raw_precip, tstep_in, tstep_mod):
    ratio_precip = 1.0 * tstep_mod / tstep_in
    data_tstep_precip_adj = ratio_precip * data_raw_precip.copy().shift(
        -tstep_in + tstep_mod,
        freq="s",
    ).resample(f"{tstep_mod}{str_second}").mean().interpolate(
        method="polynomial", order=0
    )
    # assign a new start with nan
    # t_start = data_raw_precip.index.shift(-tstep_in + tstep_mod, freq='S')[0]
    t_end = data_raw_precip.index[-1]
    # data_tstep_precip_adj.loc[t_start, :] = np.nan
    data_tstep_precip_adj.loc[t_end] = np.nan
    data_tstep_precip_adj = data_tstep_precip_adj.sort_index()
    data_tstep_precip_adj = data_tstep_precip_adj.asfreq(f"{tstep_mod}{str_second}")
    data_tstep_precip_adj = data_tstep_precip_adj.fillna(value=0.0)
    return data_tstep_precip_adj


# resample input forcing with instantaneous values by linear interpolation
def resample_linear_inst(data_raw_inst, tstep_in, tstep_mod):
    # downscale input data to desired time step
    # downscale input data to desired time step
    data_raw_tstep = data_raw_inst.copy()

    # assign a new start with nan
    t_start = data_raw_inst.index.shift(-tstep_in + tstep_mod, freq="s")[0]
    t_end = data_raw_inst.index[-1]
    data_raw_tstep.loc[t_start, :] = np.nan
    data_raw_tstep.loc[t_end, :] = np.nan

    # re-align the index so after resampling we can have filled heading part
    data_raw_tstep = data_raw_tstep.sort_index()
    data_raw_tstep = data_raw_tstep.asfreq(f"{tstep_mod}{str_second}").interpolate(
        method="linear"
    )
    # fill gaps with valid values
    data_tstep = data_raw_tstep.copy().bfill().ffill().dropna(how="all")

    return data_tstep


# resample input forcing with average values by linear interpolation
def resample_linear_avg(data_raw_avg, tstep_in, tstep_mod):
    # retrieve ending timestamps
    t_start = data_raw_avg.index.shift(-tstep_in + tstep_mod, freq="s")[0]
    t_end = data_raw_avg.index[-1]

    # reset index as timestamps
    # shift by half-tstep_in to generate a time series with instantaneous
    # values
    data_raw_shift = data_raw_avg.shift(-tstep_in / 2, freq="s")
    # print('data_raw_shift.index')
    # print(data_raw_shift.head())

    # re-align the index so after resampling we can have filled heading part
    data_raw_tstep = data_raw_shift.copy()
    data_raw_tstep.loc[t_start, :] = np.nan
    data_raw_tstep.loc[t_end, :] = np.nan
    data_raw_tstep = data_raw_tstep.sort_index()

    # get proper timestamps for filling up
    data_tstep = data_raw_tstep.asfreq(f"{tstep_mod}{str_second}")

    # insert the shifted
    idx_comb = (
        data_raw_tstep.index.append(data_raw_shift.index)
        .append(data_tstep.index)
        .unique()
    )
    data_raw_tstep = data_raw_tstep.reindex(idx_comb)
    data_raw_tstep = data_raw_tstep.sort_index()
    # print('data_raw_tstep')
    # print(data_raw_tstep.head())

    # interpolation so to get the instantaneous values
    data_raw_tstep.loc[data_raw_shift.index] = data_raw_shift.values
    data_raw_tstep = data_raw_tstep.interpolate(method="linear")

    # transfer the interpolated values to the desired time step
    data_tstep = data_raw_tstep.loc[data_tstep.index]
    data_tstep = data_tstep.interpolate(method="linear")
    # print('data_tstep.head')
    # print(data_tstep.head())

    # fill gaps with valid values
    data_tstep = data_tstep.copy().bfill().ffill().dropna(how="all")

    return data_tstep


# resample input met forcing to tstep required by model
def resample_forcing_met(
    data_met_raw, tstep_in, tstep_mod, lat=51, lon=0, alt=100, timezone=0, kdownzen=0
):
    if tstep_in % tstep_mod != 0:
        raise RuntimeError(
            f"`tstep_in` ({tstep_in}) is not divisible by `tstep_mod` ({tstep_mod})"
        )

    data_met_raw = data_met_raw.copy()
    data_met_raw = data_met_raw.replace(-999, np.nan)
    # this line is kept for occasional debugging:
    if logger_supy.level < 20:
        p_data_met_raw = "data_met_raw.pkl"
        data_met_raw.to_pickle(p_data_met_raw)
        logger_supy.debug(f"{p_data_met_raw} has been generated!")

    # linear interpolation:
    # the interpolation schemes differ between instantaneous and average values
    # instantaneous:
    list_var_inst = [
        var for var, data_type in dict_var_type_forcing.items() if data_type == "inst"
    ]
    data_met_tstep_inst = resample_linear_inst(
        data_met_raw.filter(list_var_inst), tstep_in, tstep_mod
    )
    # average:
    list_var_avg = [
        var for var, data_type in dict_var_type_forcing.items() if data_type == "avg"
    ]
    data_met_tstep_avg = resample_linear_avg(
        data_met_raw.filter(list_var_avg), tstep_in, tstep_mod
    )

    # distributing interpolation:
    # sum:
    list_var_sum = [
        var for var, data_type in dict_var_type_forcing.items() if data_type == "sum"
    ]
    data_met_tstep_sum = resample_sum(
        data_met_raw.filter(list_var_sum), tstep_in, tstep_mod
    )

    # combine the resampled individual dataframes
    data_met_tstep = (
        pd.concat([data_met_tstep_inst, data_met_tstep_avg, data_met_tstep_sum], axis=1)
        .interpolate()
        .loc[data_met_tstep_inst.index]
    )

    # adjust solar radiation by zenith correction and total amount distribution
    if kdownzen == 1:
        data_met_tstep["kdown"] = resample_kdn(
            data_met_tstep["kdown"], tstep_mod, timezone, lat, lon, alt
        )

    # assign temporal info
    data_met_tstep["iy"] = data_met_tstep.index.year
    data_met_tstep["id"] = data_met_tstep.index.dayofyear
    data_met_tstep["it"] = data_met_tstep.index.hour
    data_met_tstep["imin"] = data_met_tstep.index.minute
    data_met_tstep["isec"] = data_met_tstep.index.second
    data_met_tstep = data_met_tstep.filter(list(dict_var_type_forcing.keys()))
    data_met_tstep = data_met_tstep.replace(np.nan, -999)

    # to keep the same order as the original data
    data_met_tstep = data_met_tstep.reindex(columns=data_met_raw.columns)

    return data_met_tstep


def set_index_dt(df_raw: pd.DataFrame) -> pd.DataFrame:
    """convert raw SUEWS DataFrame to datetime-aware DataFrame.

    Parameters
    ----------
    df_raw : pd.DataFrame
        raw SUEWS DataFrame

    Returns
    -------
    pd.DataFrame
        datetime-aware DataFrame
    """

    # set timestamp as index
    idx_dt = pd.date_range(
        *df_raw.iloc[[0, -1], :4]
        .astype(int)
        .astype(str)
        .apply(lambda ser: ser.str.cat(sep=" "), axis=1)
        .map(lambda dt: pd.to_datetime(dt, format="%Y %j %H %M")),
        periods=df_raw.shape[0],
    )
    list_dt = [
        *df_raw.iloc[:, :4]
        .astype(int)
        .astype(str)
        .apply(lambda ser: ser.str.cat(sep=" "), axis=1)
        .map(lambda dt: pd.to_datetime(dt, format="%Y %j %H %M"))
    ]
    ser_dt = pd.DatetimeIndex(list_dt).to_series()
    ser_dt_diff = ser_dt.diff()
    ser_dt_diff_sec = ser_dt_diff.dt.total_seconds()
    # check if the timestamps are in order
    dif_dt = ser_dt_diff_sec.abs().min()
    ser_test = ser_dt_diff_sec.eq(dif_dt)
    loc_issue = ser_dt_diff_sec.reset_index().index[~ser_test][1:] + 2
    if loc_issue.size == 0:
        # empty list of `loc_issue`
        freq = ser_dt_diff.iloc[1]
        df_datetime = df_raw.set_index(idx_dt).asfreq(freq)
    else:
        # non-empty list of `loc_issue`
        # locate the problematic indices
        # loc_issue = df_raw[1:].index[~ser_test]
        raise RuntimeError(
            f"Loaded forcing files have gaps/duplicates; "
            f"suspious lines include: {loc_issue.to_list()}. "
            f"NOTE: the reported line numbers are NOT completely accurate: only for your reference."
        )

    return df_datetime


# load raw data: met forcing
def load_SUEWS_Forcing_met_df_raw(
    path_input, filecode, grid, tstep_met_in, multiplemetfiles
):
    # file name pattern for met files
    forcingfile_met_pattern = "{site}{grid}_*_{tstep}.txt".format(
        site=filecode,
        grid=(grid if multiplemetfiles == 1 else ""),
        tstep=int(tstep_met_in / 60),
    )
    # path_forcing_pattern = path_input / forcingfile_met_pattern
    df_forcing_met = load_SUEWS_Forcing_met_df_pattern(
        path_input, forcingfile_met_pattern
    )
    return df_forcing_met


# caching loaded met df for better performance in initialisation
def load_SUEWS_Forcing_met_df_pattern(path_input, file_pattern):
    """Short summary.

    Parameters
    ----------
    path_input: path-like object
        path to SUEWS input folder, where met forcing files are placed

    file_pattern : basestring
        Description of parameter `file_pattern`.

    Returns
    -------
    type
        Description of returned object.

    """
    # from dask import dataframe as dd
    from pathlib import Path
    from .util._io import read_suews

    # list of met forcing files
    path_input = Path(path_input).resolve()
    # forcingfile_met_pattern = os.path.abspath(forcingfile_met_pattern)
    list_file_MetForcing = sorted([
        f for f in path_input.glob(file_pattern) if "ESTM" not in f.name
    ])

    # load met data
    df_forcing_met = pd.concat([read_suews(fn) for fn in list_file_MetForcing])
    # `drop_duplicates` in case some duplicates mixed
    df_forcing_met = df_forcing_met.drop_duplicates()
    # drop `isec`: redundant for this dataframe
    col_suews_met_forcing = list(dict_var_type_forcing.keys())[:-1]
    # rename these columns to match variables via the driver interface
    df_forcing_met.columns = col_suews_met_forcing

    # convert unit from kPa to hPa
    df_forcing_met["pres"] *= 10

    # add `isec` for WRF-SUEWS interface
    df_forcing_met["isec"] = 0

    # set correct data types
    df_forcing_met[["iy", "id", "it", "imin", "isec"]] = df_forcing_met[
        ["iy", "id", "it", "imin", "isec"]
    ].astype(np.int64)

    df_forcing_met = set_index_dt(df_forcing_met)

    return df_forcing_met


def load_SUEWS_Forcing_met_df_yaml(path_forcing):
    from pathlib import Path
    from .util._io import read_suews

    if isinstance(path_forcing, (str, Path)):
        path_forcing = Path(path_forcing).resolve()
        if path_forcing.is_dir():
            path_forcing = sorted(path_forcing.glob("*"))
        else:
            df_forcing_met = read_suews(path_forcing)
    if isinstance(path_forcing, list):
        path_forcing = [Path(p).resolve() for p in path_forcing]
        df_forcing_met = pd.concat([read_suews(fn) for fn in path_forcing])
    if not isinstance(path_forcing, (str, Path)) and not isinstance(path_forcing, list):
        import pdb

        pdb.set_trace()
    # `drop_duplicates` in case some duplicates mixed
    df_forcing_met = df_forcing_met.drop_duplicates()
    # drop `isec`: redundant for this dataframe
    col_suews_met_forcing = list(dict_var_type_forcing.keys())[:-1]
    # rename these columns to match variables via the driver interface
    df_forcing_met.columns = col_suews_met_forcing

    # convert unit from kPa to hPa
    df_forcing_met["pres"] *= 10

    # add `isec` for WRF-SUEWS interface
    df_forcing_met["isec"] = 0

    # set correct data types
    df_forcing_met[["iy", "id", "it", "imin", "isec"]] = df_forcing_met[
        ["iy", "id", "it", "imin", "isec"]
    ].astype(np.int64)

    df_forcing_met = set_index_dt(df_forcing_met)

    return df_forcing_met


# TODO: add support for loading multi-grid forcing datasets
# def load_SUEWS_Forcing_df(dir_site, ser_mod_cfg, df_state_init):
#     pass


###############################################################################
# new initialisation functions below
# for better performance


# expand dict into paired tuples
def exp_dict2tuple(rec):
    if isinstance(rec, str):
        return rec
    elif isinstance(rec, float):
        # expand this for consistency in value indexing
        return [(rec, "base")]
    elif isinstance(rec, dict):
        res = [
            # expand profile values to all hour indices
            (k, [str(i) for i in range(24)]) if v == ":" else (k, exp_dict2tuple(v))
            for k, v in rec.items()
        ]
        # print(res)
        return res
    elif isinstance(rec, list):
        return [exp_dict2tuple(v) for v in rec]


# test if a list exists in a tuple
def list_in_tuple_Q(rec):
    if isinstance(rec, tuple):
        return any(isinstance(x, list) for x in rec)
    else:
        return False


# expand list of nested tuples to simple chained tuples
# (a,[b1,b2])->[(a,b1),(a,b2)]
def exp_list2tuple(rec):
    if isinstance(rec, list):
        if len(rec) == 1:
            return exp_list2tuple(rec[0])
        else:
            return [exp_list2tuple(x) for x in rec]
    elif list_in_tuple_Q(rec):
        for i, v in enumerate(rec):
            if isinstance(v, list):
                res = [
                    (*rec[:i], x) if isinstance(x, (str, int)) else (*rec[:i], *x)
                    for x in v
                ]
                res = [exp_list2tuple(x) for x in res]
                return exp_list2tuple(res)
    else:
        return rec


# collect tracing entries under a reference `code`
def gather_code_set(code, dict_var2SiteSelect):
    set_res = set()
    # print('\n code', code)
    for k, v in dict_var2SiteSelect.items():
        # print(set_res, type(set_res), k, v)
        if k == code:
            if isinstance(v, str):
                set_res.update([v])
            elif isinstance(v, list):
                set_res.update(v)
            elif isinstance(v, dict):
                set_res.update(v.keys())
            else:
                logger_supy.info(f"{k},{v}")

        elif isinstance(v, dict):
            # print(res,v)
            set_res.update(list(gather_code_set(code, v)))
        elif isinstance(v, list):
            # print(set_res, v)
            for v_x in v:
                if isinstance(v_x, dict):
                    set_res.update(list(gather_code_set(code, v_x)))
    return set_res


# generate DataFrame based on reference `code` with index from `df_base`
def build_code_df(code, path_input, df_base):
    # str_lib = dict_Code2File[libCode].replace(
    #     '.txt', '').replace('SUEWS', 'lib')
    dict_libs = load_SUEWS_Libs(path_input)

    keys_code = gather_code_set(code, dict_var2SiteSelect)
    if keys_code == {":"}:
        # for profile values, extract all
        list_keys = code
    else:
        list_keys = [(code, k) for k in list(keys_code)]
    lib_code = dict_Code2File[code]
    str_lib = lib_code.replace(".txt", "").replace("SUEWS", "lib")
    # lib = dict_libs[str_lib]

    df_code0 = pd.concat([dict_libs[str_lib]], axis=1, keys=[code])
    # df_siteselect = dict_libs['lib_SiteSelect']
    # if isinstance(df_base.columns, pd.core.index.MultiIndex):
    if isinstance(df_base.columns, pd.MultiIndex):
        code_base = df_base.columns.levels[0][0]
        code_index = (code_base, code)
    else:
        code_index = code

    list_code = df_base[code_index].astype(int).values

    try:
        df_code = df_code0.loc[list_code, list_keys]
    except Exception as e:
        logger_supy.exception(f"Entries missing from {lib_code}")
        list_missing_code = [code for code in list_code if code not in df_code0.index]
        list_missing_key = [key for key in list_keys if key not in df_code0.columns]
        if list_missing_code:
            logger_supy.exception(f"missing code:\n {list_missing_code}")
        if list_missing_key:
            logger_supy.exception(f"missing key:\n {list_missing_key}")

        # dump data for debugging
        path_dump = Path.cwd() / "df_code0.pkl"
        df_code0.to_pickle(path_dump)
        logger_supy.exception(
            f"`df_code0` has been dumped into {path_dump.resolve()} for debugging!"
        )

        raise e

    df_code.index = df_base.index
    # recover outmost level code if profile values are extracted
    if keys_code == {":"}:
        df_code = pd.concat([df_code], axis=1, keys=[code])
    return df_code


# if to expand
def to_exp_Q(code_str):
    return ("Code" in code_str) or ("Prof" in code_str)


# generate df with all code-references values
@functools.lru_cache(maxsize=16)
def gen_all_code_df(path_input):
    dict_libs = load_SUEWS_Libs(path_input)
    df_siteselect = dict_libs["lib_SiteSelect"]
    list_code = [code for code in df_siteselect.columns if to_exp_Q(code)]

    # dict with code:{vars}
    dict_code_var = {
        code: gather_code_set(code, dict_var2SiteSelect)
        for code in list_code
        if len(gather_code_set(code, dict_var2SiteSelect)) > 0
    }

    # stage one expansion
    df_all_code = pd.concat(
        [build_code_df(code, path_input, df_siteselect) for code in dict_code_var],
        axis=1,
    )

    return df_all_code


# generate df for all const based columns
def gen_all_const_df(path_input):
    dict_libs = load_SUEWS_Libs(path_input)
    df_siteselect = dict_libs["lib_SiteSelect"]

    df_cst_all = df_siteselect.copy()
    dict_var_tuple = exp_dict_full(dict_var2SiteSelect)

    set_const_col = {
        v
        for x in dict_var_tuple.values()
        if isinstance(x, list)
        for v in x
        if "const" in v
    }

    list_const_col = [x[1] for x in set_const_col]
    for cst in set_const_col:
        df_cst_all[cst[1]] = cst[1]
    df_cst_all = df_cst_all.loc[:, list_const_col]
    df_cst_all = pd.concat([df_cst_all], axis=1, keys=["base"])
    df_cst_all = pd.concat([df_cst_all], axis=1, keys=["const"])
    df_cst_all = df_cst_all.swaplevel(i=1, j=-1, axis=1)

    return df_cst_all


# build code expanded ensemble libs
@functools.lru_cache(maxsize=16)
def build_code_exp_df(path_input, code_x):
    # df with all code-references values
    df_all_code = gen_all_code_df(path_input)

    # df with code_x for further exapansion
    df_base_x = df_all_code[code_x]

    # columns in df_base_x for further expansion
    list_code_x = [code for code in df_base_x.columns if to_exp_Q(code)]
    # print(list_code_x)
    df_all_code_x = pd.concat(
        [build_code_df(code, path_input, df_base_x) for code in list_code_x], axis=1
    )
    df_all_code_x = pd.concat([df_all_code_x], axis=1, keys=[code_x])

    # add innermost lables to conform dimensionality
    df_base_x_named = pd.concat([df_base_x], axis=1, keys=[code_x])
    df_base_x_named = pd.concat([df_base_x_named], axis=1, keys=["base"])
    df_base_x_named = df_base_x_named.swaplevel(i=0, j=1, axis=1).swaplevel(
        i=1, j=-1, axis=1
    )

    # print(df_base_x_named.columns)
    # print(df_all_code_x.columns)
    df_res_exp = pd.concat([df_base_x_named, df_all_code_x], axis=1)

    return df_res_exp


@functools.lru_cache(maxsize=16)
def gen_df_siteselect_exp(path_input):
    # df with all code-references values
    df_all_code = gen_all_code_df(path_input)

    # retrieve all `Code`-relaed names
    dict_libs = load_SUEWS_Libs(path_input)
    df_siteselect = dict_libs["lib_SiteSelect"]
    # list_code = [code for code in df_siteselect.columns if 'Code' in code]

    # dict with code:{vars}
    dict_code_var = {
        code: gather_code_set(code, dict_var2SiteSelect)
        for code in df_all_code.columns.levels[0]
    }

    # code with nested references
    dict_code_var_nest = {
        code: dict_code_var[code]
        for code in dict_code_var.keys()
        if any(("Code" in v) or ("Prof" in v) for v in dict_code_var[code])
    }

    # code with simple values
    dict_code_var_simple = {
        code: dict_code_var[code]
        for code in dict_code_var.keys()
        if code not in dict_code_var_nest
    }

    # stage 2: expand all code to actual values
    # nested code: code -> code -> value
    df_code_exp_nest = pd.concat(
        [build_code_exp_df(path_input, code) for code in list(dict_code_var_nest)],
        axis=1,
    )
    # simple code: code -> value
    df_code_exp_simple = pd.concat(
        [
            pd.concat(
                [
                    build_code_df(code, path_input, df_siteselect)
                    for code in list(dict_code_var_simple)
                ],
                axis=1,
            )
        ],
        axis=1,
        keys=["base"],
    )
    df_code_exp_simple = df_code_exp_simple.swaplevel(i=0, j=1, axis=1)
    df_code_exp_simple = df_code_exp_simple.swaplevel(i=1, j=-1, axis=1)

    # combine both nested and simple df's
    df_code_exp = pd.concat([df_code_exp_simple, df_code_exp_nest], axis=1)

    # generate const based columns
    df_cst_all = gen_all_const_df(path_input)

    # df_siteselect_pad: pad levels for pd.concat
    df_siteselect_pad = pd.concat([df_siteselect], axis=1, keys=["base"])
    df_siteselect_pad = df_siteselect_pad.swaplevel(i=0, j=1, axis=1)
    df_siteselect_pad = pd.concat([df_siteselect_pad], axis=1, keys=["base"])
    df_siteselect_pad = df_siteselect_pad.swaplevel(i=0, j=1, axis=1)

    df_siteselect_exp = pd.concat([df_siteselect_pad, df_code_exp, df_cst_all], axis=1)

    return df_siteselect_exp


# fully unravel nested lists into one flattened list
def flatten_list(l):
    res = []
    if isinstance(l, list):
        for x in l:
            if isinstance(x, list):
                res += flatten_list(x)
            else:
                res.append(x)
    else:
        res = l
    return res


# pad a single record to three-element tuple for easier indexing
def pad_rec_single(rec):
    base_pad = ["base" for i in range(3)]
    if isinstance(rec, tuple) and len(rec) < 3:
        # print(rec)
        if all(isinstance(v, (str, float, int)) for v in rec):
            v_pad = tuple(list(rec) + base_pad)[:3]
        else:
            v_pad = rec
    elif isinstance(rec, str):
        v_pad = tuple([rec] + base_pad)[:3]
    else:
        v_pad = rec
    return v_pad


# recursive version to generate three-element tuples
def pad_rec(rec):
    if isinstance(rec, list):
        rec_pad = [pad_rec(v) for v in rec]
    else:
        rec_pad = pad_rec_single(rec)
    return rec_pad


# expand a dict to fully referenced tuples
def exp_dict_full(dict_var2SiteSelect):
    # expand dict to list of tuples
    dict_list_tuple = {k: exp_dict2tuple(v) for k, v in dict_var2SiteSelect.items()}
    # expand list of nested tuples to simple chained tuples
    dict_var_tuple = {k: exp_list2tuple(v) for k, v in dict_list_tuple.items()}

    # pad records to three-element tuples
    dict_var_tuple = {k: pad_rec(v) for k, v in dict_var_tuple.items()}
    return dict_var_tuple


# save this as system variable
# dict_var_tuple = exp_dict_full(dict_var2SiteSelect)


# generate expanded surface characteristics df
# this df contains all the actual values retrived from all tables
@functools.lru_cache(maxsize=16)
def gen_df_gridSurfaceChar_exp(path_input):
    df_siteselect_exp = gen_df_siteselect_exp(path_input)
    dict_var_tuple = exp_dict_full(dict_var2SiteSelect)
    df_gridSurfaceChar_exp = pd.concat(
        {
            k: df_siteselect_exp.loc[:, flatten_list(dict_var_tuple[k])]
            for k in dict_var_tuple
        },
        axis=1,
    )
    return df_gridSurfaceChar_exp


# quickly load surface characteristics as DataFrame
# the dimension info of each variable can be derived from level 1 in columns
@functools.lru_cache(maxsize=16)
def load_SUEWS_SurfaceChar_df(path_input):
    df_gridSurfaceChar_exp = gen_df_gridSurfaceChar_exp(path_input)
    dict_var_ndim = {
        "ahprof_24hr": (24, 2),
        "humactivity_24hr": (24, 2),
        "laipower": (4, 3),
        "ohm_coef": (8, 4, 3),
        "popprof_24hr": (24, 2),
        "snowprof_24hr": (24, 2),
        "storedrainprm": (6, 7),
        "traffprof_24hr": (24, 2),
        "waterdist": (8, 6),
        "wuprofa_24hr": (24, 2),
        "wuprofm_24hr": (24, 2),
    }
    # df_gridSurfaceChar_x=df_gridSurfaceChar_exp.copy()

    len_grid_orig = df_gridSurfaceChar_exp.shape[0]
    df_x = df_gridSurfaceChar_exp
    if len_grid_orig == 1:
        df_gridSurfaceChar_exp = pd.concat([df_x, df_x])

    # update len_grid for reshaping
    list_var = df_gridSurfaceChar_exp.columns.levels[0]
    len_grid = df_gridSurfaceChar_exp.shape[0]
    list_grid = df_gridSurfaceChar_exp.index

    # here we add append a dummy `pos` level to df_gridSurfaceChar_exp
    # to avoid subset difficulty in subseting duplicate entries in multiindex
    df_x = df_gridSurfaceChar_exp.T
    # `pos` has unique values for each entry in columns
    df_x["pos"] = np.arange(len(df_gridSurfaceChar_exp.columns))
    # trasnpose back to have an extra column level `pos`
    df_gridSurfaceChar_exp = df_x.set_index("pos", append=True).T

    # extract column values and construct corresponding variables
    dict_gridSurfaceChar = {
        var: np.squeeze(df_gridSurfaceChar_exp[var].values) for var in list_var
    }

    # correct the unit for 'surfacearea' from ha to m^2 for table-based inputs
    dict_gridSurfaceChar["surfacearea"] *= 1e4

    for var, dim in dict_var_ndim.items():
        # print(var)
        val = df_gridSurfaceChar_exp.loc[:, var].values
        if "_24hr" in var:
            dim_x = dim[-1::-1]
            val_x = val.reshape((len_grid, *dim_x)).transpose((0, 2, 1))
            dict_gridSurfaceChar.update({var: val_x})
        elif var == "laipower":
            dim_x = dim[-1::-1]
            val_x = val.reshape((len_grid, *dim_x)).transpose((0, 2, 1))
            dict_gridSurfaceChar.update({var: val_x})
        elif var == "storedrainprm":
            dim_x = dim
            val_x = val.reshape((len_grid, *dim_x))
            dict_gridSurfaceChar.update({var: val_x})
        elif var == "ohm_coef":
            dim_x = dim
            val_x = val.reshape((len_grid, *dim_x))
            dict_gridSurfaceChar.update({var: val_x})
        elif var == "waterdist":
            dim_x = dict_var_ndim[var]  # [-1::-1]
            val_x0 = val.reshape((len_grid, 9, 6))
            # directly load the values for common land covers
            val_x1 = val_x0[:, :7]
            # process the ToSoilStore and ToRunoff entries
            # since only one of ToSoilStore and ToRunoff can be non-zero for each row, we sum them up and combine them
            val_x2 = val_x0[:, 7:].reshape(len_grid, 6, 2).sum(axis=2).reshape(-1, 1, 6)
            # combine valuees of common land convers and special cases
            val_x = np.hstack((val_x1, val_x2))
            dict_gridSurfaceChar.update({var: val_x})

    # convert to DataFrame
    dict_var = {}
    for var in list_var:
        val = dict_gridSurfaceChar[var]
        # ind_list = list(np.ndindex(val.shape[1:]))
        ind_list = [str(x) for x in np.ndindex(val.shape[1:])]
        df_var = pd.DataFrame(
            val.reshape((val.shape[0], -1)),
            index=list_grid,
            columns=ind_list if len(ind_list) > 1 else ["0"],
        )
        dict_var.update({var: df_var})

    df_sfc_char = pd.concat(dict_var, axis=1)
    if len_grid_orig == 1:
        df_sfc_char = df_sfc_char.drop_duplicates()

    # add DataFrame for proper type detection
    df_sfc_char = pd.DataFrame(df_sfc_char)

    # set level names for columns
    df_sfc_char.columns.set_names(["var", "ind_dim"], inplace=True)

    return df_sfc_char


# filter out unnecessary entries by re-arranging the columns
def trim_df_state(df_state: pd.DataFrame, set_var_use=set_var_use) -> pd.DataFrame:
    # get indices of variables that are included in `set_var_use`: those used by SuPy simulation
    ind_incl = df_state.columns.get_level_values("var").isin(set_var_use)
    # retrieve only necessary columns
    df_state_slim = df_state.loc[:, ind_incl]
    # remove redundant `MultiIndex` levels
    df_state_slim.columns = df_state_slim.columns.remove_unused_levels()

    return df_state_slim


# mod_config: static properties
dict_RunControl_default = {
    # "aerodynamicresistancemethod": 2, #removed in SUEWS v2023
    # "basetmethod": 1, #removed in SUEWS v2023
    # "evapmethod": 2, #removed in SUEWS v2023
    # "laicalcyes": 1, #removed in SUEWS v2023
    "veg_type": 1,
    "diagnose": 0,
    "diagnosedisagg": 0,
    "diagnosedisaggestm": 0,
    # "diagqn": 0, #removed in SUEWS v2023
    # "diagqs": 0, #removed in SUEWS v2023
    "keeptstepfilesin": 0,
    "keeptstepfilesout": 0,
    "writeoutoption": 0,
    "disaggmethod": 1,
    "disaggmethodestm": 1,
    "raindisaggmethod": 100,
    "localclimatemethod": 0,  # 0: use forcing data; 1: use local climate - modelled
    "rainamongn": -999,
    "multrainamongn": -999,
    "multrainamongnupperi": -999,
    "kdownzen": 1,
    "suppresswarnings": 0,
    "resolutionfilesin": 0,
    "stebbsmethod": 0,
}


# load RunControl variables
def load_SUEWS_dict_ModConfig(path_runcontrol, dict_default=dict_RunControl_default):
    # load RunControl variables
    dict_RunControl = dict_default.copy()
    dict_RunControl_x = (
        load_SUEWS_nml_simple(path_runcontrol).loc[:, "runcontrol"].to_dict()
    )
    dict_RunControl.update(dict_RunControl_x)

    # load SPARTACUS-specific variables: do them here as they are globally set
    path_spartacus = (
        path_runcontrol.parent
        / dict_RunControl["fileinputpath"]
        / "SUEWS_SPARTACUS.nml"
    )

    dict_RunControl_x = {k[0]: v for k, v in load_SUEWS_nml(path_spartacus).items()}
    dict_RunControl.update(dict_RunControl_x)

    return dict_RunControl


# load stebbs nml files
def load_SUEWS_dict_Stebbs(path_runcontrol, dict_runconfig):
    # load STEBBS-specific variables:
    stebbs_dict = {}
    if dict_runconfig["stebbsmethod"] == 2:
        path_stebbs_typologies = (
            path_runcontrol.parent
            / path_runcontrol["fileinputpath"]
            / "stebbs_building_typologies.nml"
        )
        path_stebbs_general = (
            path_runcontrol.parent
            / path_runcontrol["fileinputpath"]
            / "stebbs_general_params.nml"
        )
    else:
        path_stebbs_typologies = (
            trv_supy_module
            / "sample_run"
            / "Input"
            / "test_stebbs_building_typologies.nml"
        )
        path_stebbs_general = (
            trv_supy_module / "sample_run" / "Input" / "test_stebbs_general_params.nml"
        )

    stebbs_dict_y = {k[0]: v for k, v in load_SUEWS_nml(path_stebbs_typologies).items()}
    stebbs_dict.update(stebbs_dict_y)

    stebbs_dict_z = {k[0]: v for k, v in load_SUEWS_nml(path_stebbs_general).items()}
    stebbs_dict.update(stebbs_dict_z)

    return stebbs_dict


# initialise InitialCond_df with default values
nan = -999.0
# default initcond list when output as SUEWS binary
dict_InitCond_out = {
    "dayssincerain": 0,
    "temp_c0": 10,
    "gdd_1_0": nan,
    "gdd_2_0": nan,
    "laiinitialevetr": nan,
    "laiinitialdectr": nan,
    "laiinitialgrass": nan,
    "albevetr0": nan,
    "albdectr0": nan,
    "albgrass0": nan,
    "decidcap0": nan,
    "porosity0": nan,
    "soilstorepavedstate": nan,
    "soilstorebldgsstate": nan,
    "soilstoreevetrstate": nan,
    "soilstoredectrstate": nan,
    "soilstoregrassstate": nan,
    "soilstorebsoilstate": nan,
    "soilstorewaterstate": 0,
    "pavedstate": 0,
    "bldgsstate": 0,
    "evetrstate": 0,
    "dectrstate": 0,
    "grassstate": 0,
    "bsoilstate": 0,
    "waterstate": 10,
    "snowinitially": int(0),
    "snowwaterpavedstate": nan,
    "snowwaterbldgsstate": nan,
    "snowwaterevetrstate": nan,
    "snowwaterdectrstate": nan,
    "snowwatergrassstate": nan,
    "snowwaterbsoilstate": nan,
    "snowwaterwaterstate": nan,
    "snowpackpaved": nan,
    "snowpackbldgs": nan,
    "snowpackevetr": nan,
    "snowpackdectr": nan,
    "snowpackgrass": nan,
    "snowpackbsoil": nan,
    "snowpackwater": nan,
    "snowfracpaved": nan,
    "snowfracbldgs": nan,
    "snowfracevetr": nan,
    "snowfracdectr": nan,
    "snowfracgrass": nan,
    "snowfracbsoil": nan,
    "snowfracwater": nan,
    "snowdenspaved": nan,
    "snowdensbldgs": nan,
    "snowdensevetr": nan,
    "snowdensdectr": nan,
    "snowdensgrass": nan,
    "snowdensbsoil": nan,
    "snowdenswater": nan,
    "snowalb0": nan,
}
# extra items used in supy
dict_InitCond_extra = {
    "leavesoutinitially": int(nan),
    "qn_av": 0,
    "qn_s_av": 0,
    "dqndt": 0,
    "dqnsdt": 0,
}
# default items for supy initialisation
dict_InitCond_default = dict_InitCond_extra.copy()
dict_InitCond_default.update(dict_InitCond_out)


# load initial conditions of all grids as a DataFrame
def load_SUEWS_InitialCond_df(path_runcontrol):
    # load basic model settings
    logger_supy.debug("loading runcontrol")
    dict_ModConfig = load_SUEWS_dict_ModConfig(path_runcontrol)
    # path for SUEWS input tables:
    path_input = path_runcontrol.parent / dict_ModConfig["fileinputpath"]

    logger_supy.debug("loading df_gridSurfaceChar")
    df_gridSurfaceChar = load_SUEWS_SurfaceChar_df(path_input)
    # df_gridSurfaceChar.to_pickle("df_gridSurfaceChar.pkl")
    # logger_supy.debug("df_gridSurfaceChar.pkl saved!")

    # only use the first year of each grid as base for initial conditions
    logger_supy.debug("grouping grids")
    grp_df = df_gridSurfaceChar.sort_values(("year", "0")).groupby("Grid")
    # detect if multiple years are set for a single grid:
    # supy will only choose the first year to proceed
    # a warning will be issued
    n_year_grid = grp_df.size()
    if (n_year_grid > 1).any():
        loc_grid = n_year_grid.loc[n_year_grid > 1].index
        logger_supy.warning(
            f"multiple years are set for grids: {loc_grid}; SuPy will proceed with records of the first year of each grid"
        )
    df_init = grp_df.first()
    # df_init = df_gridSurfaceChar
    # rename 'Grid' to 'grid' for consistency
    df_init.index = df_init.index.rename("grid")

    # incorporate numeric entries from dict_ModConfig
    logger_supy.debug("incorporating runcontrol numeric entries")
    list_var_dim_from_dict_ModConfig = []
    for var, val in dict_ModConfig.items():
        if isinstance(val, (float, int, np.number, np.bool_)):
            list_var_dim_from_dict_ModConfig.append((var, 0, val))
        elif isinstance(val, (str)):
            pass
        else:
            print(
                var,
                val,
                type(val),
                "is not included: this should not happen!",
                "please report to the developer: https://github.com/UMEP-dev/SuPy/issues/new",
            )

    # set values according to `list_var_dim_from_dict_ModConfig`
    # and generate the secondary dimensions
    df_init = modify_df_init(df_init, list_var_dim_from_dict_ModConfig)

    # add stebbs values
    logger_supy.debug("loading stebbs values")
    stebbs_dict = load_SUEWS_dict_Stebbs(path_runcontrol, dict_ModConfig)
    list_var_dim_from_stebbs = []
    for var, val in stebbs_dict.items():
        if isinstance(val, (float, int, np.number, np.bool_, str)):
            list_var_dim_from_stebbs.append((var, 0, val))
        else:
            print(
                var,
                val,
                type(val),
                "is not included: this should not happen!",
                "please report to the developer:",
            )

    df_init = modify_df_init(df_init, list_var_dim_from_stebbs)

    # initialise df_InitialCond_grid with default values
    logger_supy.debug("adding initial condition namelists")
    dict_mod = {k: v for k, v in dict_InitCond_default.items()}
    df_init = df_init.assign(**dict_mod)
    # print(df_init.iloc[:,-10:].head())
    # rename columns to conform to multi-index convention
    dict_key_mod = {(k, ""): (k, "0") for k in dict_InitCond_default.keys()}
    df_init.columns = df_init.columns.to_flat_index()
    df_init = df_init.rename(columns=dict_key_mod)
    df_init.columns = pd.MultiIndex.from_tuples(
        df_init.columns.to_flat_index(),
        names=["var", "ind_dim"],
    )

    # for k in dict_InitCond_default:
    #     df_init.loc[:, (k, "0")] = dict_InitCond_default[k]

    # update `temp_c0` with met_forcing
    # # TODO: add temp_c0 from met_forcing

    # update `waterstate` with df_gridSurfaceChar
    # drop_duplicates in case multi-year run is set
    df_init[("waterstate", "0")] = df_init["waterdepth"].values

    # generate proper Initial Condition file names
    base_str = "initialconditions" + dict_ModConfig["filecode"]
    grid_str = (
        df_init.index.astype(str) if dict_ModConfig["multipleinitfiles"] == 1 else ""
    )
    year_str = df_init[("year", "0")].astype(int).astype(str)
    df_init[("file_init", "0")] = base_str + grid_str + "_" + year_str + ".nml"
    df_init[("file_init", "0")] = df_init[("file_init", "0")].map(
        lambda fn: path_input / fn
    )

    # generate proper grid layout file names
    base_str = "GridLayout" + dict_ModConfig["filecode"]
    grid_str = (
        df_init.index.astype(str) if dict_ModConfig["multipleinitfiles"] == 1 else ""
    )
    year_str = df_init[("year", "0")].astype(int).astype(str)
    # ser_path_init =base_str + grid_str + '_' + year_str + '.nml'
    df_init[("file_gridlayout", "0")] = base_str + grid_str + ".nml"
    # ser_path_init = ser_path_init.map(lambda fn: path_input / fn)
    df_init[("file_gridlayout", "0")] = df_init[("file_gridlayout", "0")].map(
        lambda fn: path_input / fn
    )
    return df_init


# expand multi-dimensional entries in dict
def expand_entry(k, v):
    ar = np.array(v)
    ind_dim = ar.shape
    if len(ind_dim) > 1:
        logger_supy.debug("debugging in expand_entry:", k, ind_dim)
    dict_exp = {
        (k, "0" if index == () else str(index)): ar[index]
        for index in np.ndindex(ind_dim)
    }
    return dict_exp


def modify_df_init(df_init, list_var_dim):
    df_init_mod = df_init.copy()
    len_df = df_init_mod.index.size
    # note the two-step process to avoid creating fragmented dataframes
    # 1. set up a new dataframe with added columns
    dict_col_new = {}
    for var, dim, val in list_var_dim:
        ind_dim = [str(i) for i in np.ndindex(int(dim))] if dim > 0 else ["0"]
        for ind in ind_dim:
            if isinstance(val, str) and var not in ["buildingname", "buildingtype"]:
                dict_col_new[(var, ind)] = df_init_mod[val].values.reshape(len_df)
            else:
                dict_col_new[(var, ind)] = np.repeat(val, len_df)
    df_col_new = pd.DataFrame(dict_col_new, index=df_init_mod.index)

    # Update column names to match df_init
    df_col_new.columns.names = df_init.columns.names

    # 2. merge new columns into original dataframe
    df_init_mod = df_init_mod.merge(df_col_new, left_index=True, right_index=True)

    return df_init_mod


# load Initial Condition variables from namelist file
def add_file_init_df(df_init):
    # load all nml info from file names:('file_init', '0')
    dict_init_file = (
        df_init[("file_init", "0")].map(lambda fn: load_SUEWS_nml(fn)).to_dict()
    )
    df_init_file = pd.DataFrame.from_dict(dict_init_file, orient="index")
    df_init_file.index.set_names(["Grid"], inplace=True)

    # merge only those appeard in base df
    df_init.update(df_init_file)

    return df_init


# load nml with multi-dimensional data
def load_nml_multi(fn_nml):
    df_nml = load_SUEWS_nml_simple(fn_nml)
    dict_grid_layout = {
        (name, "0"): r.dropna().values.tolist() for name, r in df_nml.iterrows()
    }
    df_grid_layout = pd.DataFrame(dict_grid_layout)
    return df_grid_layout


# load grid layout from namelist file
def add_file_gridlayout_df(df_init):
    # load all nml info from file names:('file_init', '0')
    dict_grid_layout = (
        df_init[("file_gridlayout", "0")].map(lambda fn: load_SUEWS_nml(fn)).to_dict()
    )
    df_grid_layout = pd.DataFrame.from_dict(dict_grid_layout, orient="index")
    df_grid_layout.index.set_names(["grid"], inplace=True)

    # copy column names from df_init
    df_grid_layout.columns.set_names(df_init.columns.names, inplace=True)

    # merge only those appeard in base df
    df_init = pd.concat([df_init, df_grid_layout], axis=1)

    # drop ('file_init', '0') as this may cause non-numeic errors later on
    df_init = df_init.drop(columns=[("file_gridlayout", "0")])
    return df_init


# add veg info into `df_init`
def add_veg_init_df(df_init):
    # get veg surface fractions
    df_init[("fr_veg_sum", "0")] = df_init["sfr_surf"].iloc[:, 2:5].sum(axis=1)

    # get gdd_0 and sdd_0
    veg_sfr = df_init["sfr_surf"].iloc[:, 2:5].T
    veg_sfr = veg_sfr.reset_index(drop=True).T
    # gdd_0:
    x_gddfull = df_init["gddfull"].T.reset_index(drop=True).T
    df_init[("gdd_0", "0")] = (veg_sfr * x_gddfull).sum(axis=1)
    df_init[("gdd_0", "0")] /= veg_sfr.sum(axis=1)
    # sdd_0:
    x_sddfull = df_init["sddfull"].T.reset_index(drop=True).T
    df_init[("sdd_0", "0")] = (veg_sfr * x_sddfull).sum(axis=1)
    df_init[("sdd_0", "0")] /= veg_sfr.sum(axis=1)
    # const
    df_init[("const", "0")] = 0

    # list of tuples for assignment of leave on/off parameters
    list_var_veg = [
        # aligned for:
        # (var, leavesout==1, leavesout==0)
        (("gdd_1_0", "0"), ("gdd_0", "0"), ("const", "0")),
        (("gdd_2_0", "0"), ("const", "0"), ("sdd_0", "0")),
        (("laiinitialevetr", "0"), ("laimax", "(0,)"), ("laimin", "(0,)")),
        (("laiinitialdectr", "0"), ("laimax", "(1,)"), ("laimin", "(1,)")),
        (("laiinitialgrass", "0"), ("laimax", "(2,)"), ("laimin", "(2,)")),
        (("albevetr0", "0"), ("albmax_evetr", "0"), ("albmin_evetr", "0")),
        (("albdectr0", "0"), ("albmax_dectr", "0"), ("albmin_dectr", "0")),
        (("albgrass0", "0"), ("albmax_grass", "0"), ("albmin_grass", "0")),
        (("decidcap0", "0"), ("capmax_dec", "0"), ("capmin_dec", "0")),
        (("porosity0", "0"), ("pormin_dec", "0"), ("pormax_dec", "0")),
    ]

    # determine veg related parameters based on `leavesoutinitially`
    ser_leaves_on_flag = pd.Series(
        (df_init["leavesoutinitially"] == 1).values.flatten()
    )

    # set veg related parameters
    for var_veg, var_leaves_on, var_leaves_off in list_var_veg:
        # get default values based on ser_leaves_on_flag
        val_dft = df_init[var_leaves_on].where(
            ser_leaves_on_flag, df_init[var_leaves_off]
        )

        # replace with val_dft if set as -999 (i.e., nan)
        ser_valid_flag = ~(df_init[var_veg] == -999)

        df_init[var_veg] = df_init[var_veg].where(ser_valid_flag, val_dft)

    return df_init


# add surface specific info into `df_init`
def add_sfc_init_df(df_init):
    # create snow flag
    ser_snow_use = df_init[("snowuse", "0")]
    ser_snow_init = df_init[("snowinitially", "0")]
    ser_snow_flag = ser_snow_init.where(ser_snow_init == 0, 1)
    ser_snow_flag = ser_snow_use * ser_snow_flag

    # land cover based assignment
    list_sfc = ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]
    # dict {major variable: [minor variables]}
    dict_var_sfc = {
        var: ["{var}{sfc}".format(var=var, sfc=sfc) for sfc in list_sfc]
        for var in ["snowdens", "snowfrac", "snowpack"]
    }
    dict_var_sfc.update({
        "snowwater": ["snowwater{}state".format(sfc) for sfc in list_sfc],
        "soilstore_surf": ["soilstore{}state".format(sfc) for sfc in list_sfc],
        "state_surf": ["{}state".format(sfc) for sfc in list_sfc],
    })

    # set values according to `dict_var_sfc`
    # and generate the secondary dimensions
    for var in dict_var_sfc:
        for ind, var_sfc in zip(np.ndindex(len(list_sfc)), dict_var_sfc[var]):
            ind_str = str(ind)
            df_init[(var, ind_str)] = df_init[var_sfc]
            if var_sfc.startswith("snow"):
                df_init[(var, ind_str)] *= ser_snow_flag.values.flatten()

    return df_init


# add initial daily state into `df_init`
def add_state_init_df(df_init):
    # dimensional assignment
    # (var, dim, `default value` or `reference name`)
    list_var_dim = [
        ("icefrac", 7, 0.2),
        ("snowfallcum", 0, 0.0),
        ("snowalb", 0, "snowalb0"),
        ("porosity_id", 0, "porosity0"),
        ("albdectr_id", 0, "albdectr0"),
        ("albevetr_id", 0, "albevetr0"),
        ("albgrass_id", 0, "albgrass0"),
        ("decidcap_id", 0, "decidcap0"),
        ("lai_id", 3, 0.0),
        ("gdd_id", 3, "gdd_1_0"),
        ("sdd_id", 3, "gdd_2_0"),
        ("tmin_id", 0, 90),
        ("tmax_id", 0, -90),
        ("lenday_id", 0, 0.0),
        ("hdd_id", 12, 0.0),
        ("wuday_id", 9, 0.0),
        ("dt_since_start", 0, 0.0),
        ("tstep_prev", 0, "tstep"),
        ("tair24hr", int(24 * 3600 / df_init["tstep"].values[0, 0]), 273.15),
        ("tair_av", 0, 273.15),
    ]

    # set values according to `list_var_dim`
    # and generate the secondary dimensions
    df_init = modify_df_init(df_init, list_var_dim)

    # modifications for special cases
    # `lai_id` corrections:
    df_init[("lai_id", "(0,)")] = df_init["laiinitialevetr"].values
    df_init[("lai_id", "(1,)")] = df_init["laiinitialdectr"].values
    df_init[("lai_id", "(2,)")] = df_init["laiinitialgrass"].values
    # # `gdd_id` corrections:
    # df_init[('gdd_id', '(2,)')] = 90
    # df_init[('gdd_id', '(3,)')] = -90

    # `popdens` corrections:
    # daytime population density on weekdays
    ser_popdens_day_1 = df_init[("popdensdaytime", "(0,)")]
    # nighttime population density on weekdays
    ser_popdens_night = df_init[("popdensnighttime", "0")]

    # if daytime popdens missing and nighttime one existing:
    flag_replace_day = (ser_popdens_day_1 < 0) & (ser_popdens_night >= 0)
    df_init.loc[flag_replace_day, ("popdensdaytime", "(0,)")] = ser_popdens_night[
        flag_replace_day
    ]
    # if nighttime popdens missing and daytime one existing:
    flag_replace_night = (ser_popdens_night < 0) & (ser_popdens_day_1 >= 0)
    df_init.loc[flag_replace_night, ("popdensnighttime", "0")] = ser_popdens_day_1[
        flag_replace_night
    ]

    # Fraction of weekend population to weekday population
    ser_frpddwe = df_init[("frpddwe", "0")]
    ser_popdens_day_2 = (
        ser_popdens_night + (ser_popdens_day_1 - ser_popdens_night) * ser_frpddwe
    )
    df_init[("popdensdaytime", "(1,)")] = ser_popdens_day_2

    # `gridiv` addition:
    df_init[("gridiv", "0")] = df_init.index

    return df_init


# add initial temperatures into `df_init`
def add_temp_init_df(df_init):
    list_var_add = [
        "tsfc_surf",
        "tsfc_roof",
        "tsfc_wall",
        "temp_wall",
        "temp_roof",
        "temp_surf",
    ]

    # temperature of all inner layers
    ndepth = 5
    temp_init = np.repeat(df_init.filter(like="tin_").values, ndepth, axis=1)
    df_temp = df_init.filter(like="cp_").rename(
        lambda var: var.replace("cp", "temp"),
        axis="columns",
        level="var",
    )
    df_temp.columns = df_temp.columns.remove_unused_levels()
    df_temp.loc[:, :] = temp_init

    # temperature at facet surface
    df_sfc = df_init.filter(like="tin_").rename(
        lambda var: var.replace("tin", "tsfc"),
        axis="columns",
        level="var",
    )
    df_sfc.columns = df_sfc.columns.remove_unused_levels()
    df_sfc.loc[:, :] = df_init.filter(like="tin_").values

    # merge the two dataframes
    df_init = pd.concat([df_init, df_temp, df_sfc], axis=1)

    return df_init


# add additional parameters to `df` produced by `load_SUEWS_InitialCond_df`
def load_InitialCond_grid_df(path_runcontrol, force_reload=True):
    # clean all cached states
    if force_reload:
        import gc

        gc.collect()
        wrappers = [
            a for a in gc.get_objects() if isinstance(a, functools._lru_cache_wrapper)
        ]
        for wrapper in wrappers:
            # print(wrapper.__name__)
            wrapper.cache_clear()
        logger_supy.info("All cache cleared.")

    # load base df of InitialCond
    logger_supy.debug("loading base df_init...")
    df_init = load_SUEWS_InitialCond_df(path_runcontrol)
    # if 'localclimatemethod' in df_init.columns:
    #     print()
    #     print('1')
    #     print('localclimatemethod is in df_init')
    # else:
    #     print('localclimatemethod is not in df_init')

    # add Initial Condition variables from namelist file
    logger_supy.debug("adding initial conditions...")
    df_init = add_file_init_df(df_init)
    # if 'localclimatemethod' in df_init.columns:
    #     print()
    #     print('2')
    #     print('localclimatemethod is in df_init')
    # else:
    #     print('localclimatemethod is not in df_init')

    # add surface specific info into `df_init`
    logger_supy.debug("adding surface specific conditions...")
    df_init = add_sfc_init_df(df_init)
    # if 'localclimatemethod' in df_init.columns:
    #     print()
    #     print('3')
    #     print('localclimatemethod is in df_init')
    # else:
    #     print('localclimatemethod is not in df_init')

    # add veg info into `df_init`
    logger_supy.debug("adding vegetation specific conditions...")
    df_init = add_veg_init_df(df_init)
    # if 'localclimatemethod' in df_init.columns:
    #     print()
    #     print('4')
    #     print('localclimatemethod is in df_init')
    # else:
    #     print('localclimatemethod is not in df_init')

    # add initial daily state into `df_init`
    logger_supy.debug("adding dailystate specific conditions...")
    df_init = add_state_init_df(df_init)
    # if 'localclimatemethod' in df_init.columns:
    #     print()
    #     print('5')
    #     print('localclimatemethod is in df_init')
    # else:
    #     print('localclimatemethod is not in df_init')

    # add Initial Condition variables from namelist file
    logger_supy.debug("adding grid layout info...")
    df_init = add_file_gridlayout_df(df_init)
    # if 'localclimatemethod' in df_init.columns:
    #     print()
    #     print('6')
    #     print('localclimatemethod is in df_init')
    # else:
    #     print('localclimatemethod is not in df_init')

    # add Initial temperatures into `df_init`
    logger_supy.debug("adding initial temperatures...")
    df_init = add_temp_init_df(df_init)
    # if 'localclimatemethod' in df_init.columns:
    # print()
    # print('7')
    # print('localclimatemethod is in df_init')
    # else:
    # print('localclimatemethod is not in df_init')

    # sort column names for consistency
    logger_supy.debug("setting grid level...")
    df_init.index.set_names("grid", inplace=True)

    # filter out unnecessary entries by re-arranging the columns
    logger_supy.debug("cleaning columns...")
    df_init = trim_df_state(df_init)
    # print()
    # print('8')
    # if 'localclimatemethod' in df_init.columns:
    # print('localclimatemethod is in df_init')
    # else:
    # print('localclimatemethod is not in df_init')

    # normalise surface fractions to prevent non-1 sums
    df_sfr_surf = df_init.sfr_surf.copy()
    df_sfr_surf = df_sfr_surf.div(df_sfr_surf.sum(axis=1), axis=0)
    df_init.sfr_surf = df_sfr_surf
    return df_init


# load df_state from csv
def load_df_state(path_csv: Path) -> pd.DataFrame:
    """load `df_state` from `path_csv`

    Parameters
    ----------
    path_csv : Path
        path to the csv file that stores `df_state` produced by a supy run

    Returns
    -------
    pd.DataFrame
        `df_state` produced by a supy run
    """

    df_state = pd.read_csv(
        path_csv,
        header=[0, 1],
        index_col=[0, 1],
        parse_dates=True,
        infer_datetime_format=True,
    )
    return df_state
