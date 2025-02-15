from ast import literal_eval
from shutil import rmtree
import tempfile
import copy
import multiprocess
import os
import sys
import time

# import logging
import traceback
from ast import literal_eval
from pathlib import Path
from typing import Tuple
import pandas

import numpy as np
import pandas as pd
from .supy_driver import suews_driver as sd
import copy

from ._load import (
    df_var_info,
    list_var_inout,
    list_var_inout_multitsteps,
    list_var_input,
    list_var_input_multitsteps,
    list_var_output,
    list_var_output_multitsteps,
)
from ._post import (
    pack_df_output_line,
    pack_df_output_array,
    pack_df_state,
    pack_dict_debug,
    pack_df_debug,
)


from ._env import logger_supy

from .util._debug import save_zip_debug


##############################################################################
# main calculation
# 1. calculation code for one time step
# 2. compact wrapper for running a whole simulation


# 1. calculation code for one time step


# test for performance
# dict_var_inout = {k: None for k in set_var_inout}


# high-level wrapper: suews_cal_tstep
def suews_cal_tstep(dict_state_start, dict_met_forcing_tstep):
    # save_state=False):
    # use single dict as input for suews_cal_main
    dict_input = copy.deepcopy(dict_state_start)
    dict_input.update(dict_met_forcing_tstep)

    for var in list_var_input:
        if var in dict_input:
            pass
        else:
            print(f"{var} is not in dict_input")
            print("\n")

    dict_input = {k: dict_input[k] for k in list_var_input}

    # main calculation:
    try:
        # import pickle
        # pickle.dump(dict_input, open("dict_input.pkl", "wb"))
        # print("dict_input.pkl saved")
        res_suews_tstep = sd.suews_cal_main(**dict_input)
    except Exception as ex:
        # show trace info
        logger_supy.exception(traceback.format_exc())
        # show SUEWS fatal error details produced by SUEWS kernel
        with open("problems.txt", "r") as f:
            logger_supy.critical(f.read())
        # clean slate
        # os.remove('problems.txt')
        # sys.exit()
        logger_supy.critical("SUEWS kernel error")
    else:
        # update state variables
        # if save_state:  # deep copy states results
        dict_state_end = copy.deepcopy(dict_state_start)
        dict_state_end.update(
            {var: copy.copy(dict_input[var]) for var in list_var_inout}
        )

        # update timestep info
        dict_state_end["tstep_prev"] = dict_state_end["tstep"]
        dict_state_end["dt_since_start"] += dict_state_end["tstep"]

        # pack output
        list_var = [
            var
            for var in dir(res_suews_tstep)
            if not var.startswith("_") and not callable(getattr(res_suews_tstep, var))
        ]
        list_arr = [
            (
                getattr(res_suews_tstep, var)
                if "datetime" in var
                else getattr(res_suews_tstep, var)[5:]
            )
            for var in list_var
        ]
        dict_output = dict(zip(list_var, list_arr))
        # dict_output = {k: v for k, v in zip(list_var_output, res_suews_tstep)}

        return dict_state_end, dict_output


# high-level wrapper: suews_cal_tstep
# def suews_cal_tstep_multi(df_state_start_grid, df_met_forcing_block):
def suews_cal_tstep_multi(dict_state_start, df_forcing_block, debug_mode=False):
    from ._post import pack_df_output_block

    # use single dict as input for suews_cal_main
    dict_input = copy.deepcopy(dict_state_start)
    dict_input.update(
        {
            "metforcingblock": np.array(
                df_forcing_block.drop(
                    columns=[
                        "metforcingdata_grid",
                        "ts5mindata_ir",
                        "isec",
                    ]
                ),
                order="F",
            ),
            "ts5mindata_ir": np.array(df_forcing_block["ts5mindata_ir"], order="F"),
            "len_sim": np.array(df_forcing_block.shape[0], dtype=int),
        }
    )

    if debug_mode:
        dict_input["flag_test"] = True
    else:
        dict_input["flag_test"] = False

    dict_input = {k: dict_input[k] for k in list_var_input_multitsteps}
    # main calculation:

    try:
        res_suews_tstep_multi, res_mod_state = sd.suews_cal_multitsteps(**dict_input)

    except Exception as ex:
        # show trace info
        # print(traceback.format_exc())
        # show SUEWS fatal error details produced by SUEWS kernel
        with open("problems.txt", "r") as f:
            logger_supy.critical(f.read())
        # clean slate
        # os.remove('problems.txt')
        # sys.exit()
        # raise RuntimeError("Something bad happened") from exs
        logger_supy.critical("SUEWS kernel error")
    else:
        # update state variables
        # use deep copy to avoid reference issue; also copy the initial dict_state_start
        dict_state_end = copy.deepcopy(dict_state_start)

        # update state variables with the output of the model:
        # note `dict_input` is updated with the output of the model
        dict_state_end.update(
            {var: dict_input[var] for var in list_var_inout_multitsteps}
        )

        # update timestep info
        dict_state_end["tstep_prev"] = dict_state_end["tstep"]
        idx_dt = df_forcing_block.index
        duration_s = int((idx_dt[-1] - idx_dt[0]).total_seconds())
        dict_state_end["dt_since_start"] += duration_s + dict_state_end["tstep"]

        # pack output into dataframe
        list_var = [
            var
            for var in dir(res_suews_tstep_multi)
            if not var.startswith("_")
            and not callable(getattr(res_suews_tstep_multi, var))
        ]
        list_arr = [getattr(res_suews_tstep_multi, var) for var in sorted(list_var)]
        dict_output_array = dict(zip(list_var, list_arr))
        df_output_block = pack_df_output_block(dict_output_array, df_forcing_block)

        # TODO: #233 res_mod_state will be used in the future for debugging purpose
        # convert res_mod_state to a dict
        # Assuming dts_debug is your object instance
        # deepcopy is used to avoid reference issue when passing the object
        dict_debug = copy.deepcopy(pack_dict_debug(res_mod_state))

        return dict_state_end, df_output_block, dict_debug


# dataframe based wrapper
# serial mode:
def run_supy_ser(
    df_forcing: pandas.DataFrame,
    df_state_init: pandas.DataFrame,
    save_state=False,
    chunk_day=3660,
    debug_mode=False,
) -> Tuple[pandas.DataFrame, pandas.DataFrame]:
    """Perform supy simulation.

    Parameters
    ----------
    df_forcing : pandas.DataFrame
        forcing data for all grids in `df_state_init`.
    df_state_init : pandas.DataFrame
        initial model states;
        or a collection of model states with multiple timestamps, whose last temporal record will be used as the initial model states.
    save_state : bool, optional
        flag for saving model states at each time step, which can be useful in diagnosing model runtime performance or performing a restart run.
        (the default is False, which instructs supy not to save runtime model states).
    chunk_day : int, optional
        chunk size (`chunk_day` days) to split simulation periods so memory usage can be reduced.
        (the default is 3660, which implies ~10-year forcing chunks used in simulations).
    debug_mode : bool, optional
        flag for debug mode, which will set `flag_test` to True in the input dictionary.
        (the default is False, which instructs supy not to run in debug mode).

    Returns
    -------
    df_output, df_state_final : Tuple[pandas.DataFrame, pandas.DataFrame]
        - df_output: `output results <df_output_var>`
        - df_state_final: `final model states <df_state_var>`

    Examples
    --------

    >>> df_output, df_state_final = supy.run_supy(df_forcing, df_state_init)


    """

    # save df_init without changing its original data
    # df.copy() in pandas works as a standard python deepcopy
    df_init = df_state_init.copy()

    # retrieve the last temporal record as `df_init`
    # if a `datetime` level existing in the index
    if df_init.index.nlevels > 1:
        idx_dt = df_init.index.get_level_values("datetime").unique()
        dt_last = idx_dt.max()
        df_init = df_init.loc[dt_last]

    # add placeholder variables for df_forcing
    # `metforcingdata_grid` and `ts5mindata_ir` are used by AnOHM and ESTM, respectively
    # they are now temporarily disabled in supy
    df_forcing = df_forcing.assign(
        metforcingdata_grid=0,
        ts5mindata_ir=0,
    ).rename(
        # rename is a workaround to resolve naming inconsistency between
        # suews fortran code interface and input forcing file headers
        columns={
            "%" + "iy": "iy",
            "id": "id",
            "it": "it",
            "imin": "imin",
            "qn": "qn1_obs",
            "qh": "qh_obs",
            "qe": "qe",
            "qs": "qs_obs",
            "qf": "qf_obs",
            "U": "avu1",
            "RH": "avrh",
            "Tair": "temp_c",
            "pres": "press_hpa",
            "rain": "precip",
            "kdown": "kdown",
            "snow": "snowfrac_obs",
            "ldown": "ldown_obs",
            "fcld": "fcld_obs",
            "Wuh": "wu_m3",
            "xsmd": "xsmd",
            "lai": "lai_obs",
            "kdiff": "kdiff",
            "kdir": "kdir",
            "wdir": "wdir",
        }
    )
    # reorder columns of df_forcing to comply with SUEWS kernel convention in receiving the input
    # TODO: this re-ordering can be later put into the planned input checker
    list_var_forcing = [
        "iy",
        "id",
        "it",
        "imin",
        "qn1_obs",
        "qh_obs",
        "qe",
        "qs_obs",
        "qf_obs",
        "avu1",
        "avrh",
        "temp_c",
        "press_hpa",
        "precip",
        "kdown",
        "snowfrac_obs",
        "ldown_obs",
        "fcld_obs",
        "wu_m3",
        "xsmd",
        "lai_obs",
        "kdiff",
        "kdir",
        "wdir",
        "isec",
        "metforcingdata_grid",
        "ts5mindata_ir",
    ]
    df_forcing = df_forcing.loc[:, list_var_forcing]

    # grid list determined by initial states
    list_grid = df_init.index

    # initialise dicts for holding results and model states
    dict_state = {}
    dict_df_output = {}

    # initial and final tsteps retrieved from forcing data
    tstep_init = df_forcing.index[0]
    tstep_final = df_forcing.index[-1]
    # tstep size retrieved from forcing data
    freq = df_forcing.index.freq

    # dict_state is used to save model states for later use
    dict_state = {
        # (t_start, grid): series_state_init.to_dict()
        (tstep_init, grid): pack_grid_dict(ser_state_init)
        for grid, ser_state_init in df_init.iterrows()
    }

    # remove 'problems.txt'
    if Path("problems.txt").exists():
        os.remove("problems.txt")

    # for multi-year run, reduce the whole df_forcing into {chunk_day}-day chunks for less memory consumption
    idx_start = df_forcing.index.min()
    idx_all = df_forcing.index
    if save_state:
        # if save_state is True, the forcing is split into chunks of every timestep
        # this is to ensure that the model states are saved at each timestep
        grp_forcing_chunk = df_forcing.groupby(idx_all)
    else:
        grp_forcing_chunk = df_forcing.groupby(
            (idx_all - idx_start) // pd.Timedelta(chunk_day, "d")
        )
    n_chunk = len(grp_forcing_chunk)
    if n_chunk > 1:
        logger_supy.info(
            f"Forcing is split into {n_chunk:d} chunks for less memory consumption."
        )
        df_state_init_chunk = df_state_init.copy()
        list_df_output = []
        list_df_state = []
        list_df_debug = []
        for grp in grp_forcing_chunk.groups:
            # get forcing of a specific year
            df_forcing_chunk = grp_forcing_chunk.get_group(grp)
            # run supy: actual execution done in the `else` clause below
            df_output_chunk, df_state_final_chunk, df_debug_chunk = run_supy_ser(
                df_forcing_chunk,
                df_state_init_chunk,
                chunk_day=chunk_day,
            )
            df_state_init_chunk = df_state_final_chunk.copy()
            # collect results
            list_df_output.append(df_output_chunk)
            list_df_state.append(df_state_final_chunk)
            list_df_debug.append(df_debug_chunk)

        # re-organise results of each year
        df_output = pd.concat(list_df_output).sort_index()
        df_state_final = pd.concat(list_df_state).sort_index().drop_duplicates()
        df_debug = pd.concat(list_df_debug).sort_index()

    else:
        # for single-chunk run (1 chunk = {chunk_day} days), directly put df_forcing into supy_driver for calculation
        # use higher level wrapper that calculate at a `block` level
        # for better performance by reducing runtime memory usage
        list_dict_state_input = [dict_state[(tstep_init, grid)] for grid in list_grid]

        try:
            list_res_grid = [
                suews_cal_tstep_multi(dict_state_input, df_forcing, debug_mode)
                for dict_state_input in list_dict_state_input
            ]

            list_dict_state_end, list_df_output, list_dict_debug = zip(*list_res_grid)

        except Exception as e:
            path_zip_debug = save_zip_debug(df_forcing, df_state_init, error_info=e)
            raise RuntimeError(
                f"\n====================\n"
                f"SUEWS kernel error!\n"
                f"A zip file for debugging has been saved as `{path_zip_debug.as_posix()}`:"
                f"Please report this issue with the above zip file to the developer at"
                f" https://github.com/UMEP-dev/SuPy/issues/new?assignees=&labels=&template=issue-report.md."
                f"\n====================\n"
            )

        # collect output arrays
        dict_df_output = {
            grid: df_output for grid, df_output in zip(list_grid, list_df_output)
        }

        # collect final states
        dict_state_final_tstep = {
            (tstep_final + freq, grid): dict_state_end
            for grid, dict_state_end in zip(list_grid, list_dict_state_end)
        }
        dict_state.update(dict_state_final_tstep)
        df_state_final = pack_df_state(dict_state).swaplevel(0, 1)
        # pack final model states into a proper dataframe
        df_state_final = pack_df_state_final(df_state_final, df_init)

        # save results as time-aware DataFrame
        df_output0 = pd.concat(dict_df_output, names=["grid"]).sort_index()
        df_output = df_output0.replace(-999.0, np.nan)

        # drop ESTM for now as it is not supported yet
        df_output = df_output.drop("ESTM", axis=1, level="group")
        # trim multi-index based columns
        df_output.columns = df_output.columns.remove_unused_levels()

        # collect debug info
        dict_debug = {
            (tstep_final, grid): debug
            for grid, debug in zip(list_grid, list_dict_debug)
        }
        df_debug = pack_df_debug(dict_debug)

    return df_output, df_state_final, df_debug


def run_save_supy(
    df_forcing_tstep, df_state_init_m, ind, save_state, chunk_day, path_dir_temp, debug_mode=False
):
    """Run SuPy simulation and save results to temporary files.

    Parameters
    ----------
    df_forcing_tstep : pandas.DataFrame
        Forcing data for one grid
    df_state_init_m : pandas.DataFrame
        Initial model states for one grid
    ind : int
        Index identifier for output files
    save_state : bool
        Flag for saving model states at each time step
    chunk_day : int
        Chunk size (days) to split simulation periods
    path_dir_temp : pathlib.Path
        Path to temporary directory for saving results
    debug_mode : bool
        Flag for debug mode

    Returns
    -------
    None
        Results are saved to files in path_dir_temp:
        - {ind}_out.pkl: Output results DataFrame
        - {ind}_state.pkl: Final model states DataFrame
        - {ind}_debug.pkl: Debug information DataFrame
    """
    # run supy in serial mode
    df_output, df_state_final, df_debug = run_supy_ser(
        df_forcing_tstep, df_state_init_m, save_state, chunk_day, debug_mode
    )
    # save to path_dir_temp
    path_out = path_dir_temp / f"{ind}_out.pkl"
    path_state = path_dir_temp / f"{ind}_state.pkl"
    path_debug = path_dir_temp / f"{ind}_debug.pkl"
    df_output.to_pickle(path_out)
    df_state_final.to_pickle(path_state)
    df_debug.to_pickle(path_debug)


# parallel mode: only used on Linux/macOS; Windows is not supported yet.
def run_supy_par(df_forcing_tstep, df_state_init_m, save_state, chunk_day, debug_mode=False):
    """Perform supy simulation in parallel mode.

    Parameters
    ----------
    df_forcing_tstep : pandas.DataFrame
        Forcing data for all grids
    df_state_init_m : pandas.DataFrame
        Initial model states for all grids
    save_state : bool
        Flag for saving model states at each time step
    chunk_day : int
        Chunk size (days) to split simulation periods
    debug_mode : bool
        Flag for debug mode

    Returns
    -------
    Tuple[pandas.DataFrame, pandas.DataFrame, pandas.DataFrame]
        - df_output: Output results
        - df_state_final: Final model states
        - df_debug: Debug information
    """
    n_grid = df_state_init_m.index.size
    list_forcing = [df_forcing_tstep for _ in range(n_grid)]
    list_state = [df_state_init_m.iloc[[i]] for i in np.arange(n_grid)]
    list_save_state = [save_state for _ in range(n_grid)]
    list_chunk_day = [chunk_day for _ in range(n_grid)]
    list_debug_mode = [debug_mode for _ in range(n_grid)]
    # create a temp directory for results
    with tempfile.TemporaryDirectory() as dir_temp:
        path_dir_temp = Path(dir_temp).resolve()
        # print(path_dir_temp)
        list_dir_temp = [path_dir_temp for _ in range(n_grid)]

        # parallel run
        with multiprocess.Pool() as pool:
            pool.starmap(
                run_save_supy,
                zip(
                    list_forcing,
                    list_state,
                    np.arange(n_grid),
                    list_save_state,
                    list_chunk_day,
                    list_dir_temp,
                    list_debug_mode,
                ),
            )

        # load dumped h5 files
        df_output = pd.concat(
            [pd.read_pickle(path_dir_temp / f"{n}_out.pkl") for n in np.arange(n_grid)]
        )
        df_state_final = pd.concat(
            [
                pd.read_pickle(path_dir_temp / f"{n}_state.pkl")
                for n in np.arange(n_grid)
            ]
        )
        df_debug = pd.concat(
            [
                pd.read_pickle(path_dir_temp / f"{n}_debug.pkl")
                for n in np.arange(n_grid)
            ]
        )

    return df_output, df_state_final, df_debug


# main calculation end here
##############################################################################

# pack one Series of var into np.array
def pack_var_old(ser_var):
    dim = np.array(literal_eval(ser_var.index[-1])) + 1
    val = np.array(ser_var.values.reshape(dim), order="F")
    try:
        return val.astype(float)
    except:
        return val


# pack one Series of var into np.array
def pack_var(ser_var: pd.Series) -> np.ndarray:
    """Convert a pandas Series with tuple-like index strings into a numpy array.

    Parameters
    ----------
    ser_var : pandas.Series
        Series with index strings like '(0,1)' representing dimensions

    Returns
    -------
    numpy.ndarray
        Reshaped array based on index dimensions
    """
    # Handle scalar values (single element Series)
    if len(ser_var) == 1:
        return np.array([ser_var.iloc[0]])

    try:
        # Convert index strings to tuples of integers
        # e.g. '(1,2)' -> (1,2)
        # import pdb; pdb.set_trace()
        index_tuples = [
            tuple(map(int, filter(None, idx.strip('()').split(','))))
            for idx in ser_var.index
        ]

        # Create new Series with tuple indices for proper sorting
        ser_var_indexed = pd.Series(
            ser_var.values,
            index=index_tuples
        ).sort_index()

        # Get dimensions from max indices
        # Add 1 since indices are 0-based
        dimensions = np.array(ser_var_indexed.index[-1]) + 1

        # Reshape - NO need to use Fortran-style ordering
        # res = np.array(ser_var_indexed.values).reshape(dimensions, order="F")
        res = np.array(ser_var_indexed.values).reshape(dimensions)

        try:
            return res.astype(float)
        except:
            return res.astype(str)

    except (ValueError, AttributeError) as e:
        # Log error and fall back to scalar handling
        print(f"Error reshaping Series: {e}")
        return np.array([ser_var.iloc[0]])



# pack one Series of grid vars into dict of `np.array`s
def pack_grid_dict(ser_grid):
    ser_dtype = df_var_info.dtype
    list_var_int = df_var_info[(ser_dtype == "int") | (ser_dtype == "array('i')")].index
    list_var = ser_grid.index.levels[0].unique()
    # pack according to dimension info
    dict_var = {}
    for var in list_var:
        if var not in ["file_init"]:
            # print(f"var: {var}")
            # val_packed = pack_var(ser_grid[var])
            # val_packed_old = pack_var_old(ser_grid[var])
            # # Test if the old and new packed values are different
            # if not np.array_equal(val_packed_old, val_packed):
            #     # Save the input Series as a pickle file for debugging
            #     ser_grid.to_pickle("ser_grid_debug.pkl")
            #     # Stop execution
            #     raise ValueError(f"Packed values for variable '{var}' are different between old and new methods.")

            # dict_var[var] = pack_var(ser_grid[var])
            # dict_var[var] = pack_var_old(ser_grid[var])
            try:
                # dict_var[var] = pack_var(ser_grid[var])
                dict_var[var] = pack_var_old(ser_grid[var])
            except Exception as e:
                print(f"Error packing variable '{var}': {e}")
                # dict_var[var] = pack_var_old(ser_grid[var])
                dict_var[var] = pack_var(ser_grid[var])
        else:
            pass
    # dict_var = {
    #     var: pack_var(ser_grid[var])  # .astype(np.float)
    #     for var in list_var
    #     if var not in ["file_init"]
    # }
    # convert to int
    dict_var_int = {
        var: dict_var[var].astype(int) for var in list_var if var in list_var_int
    }
    dict_var.update(dict_var_int)
    return dict_var


# pack final state to the same format as initial state
def pack_df_state_final(df_state_end, df_state_start):
    ser_col_multi = df_state_start.columns.to_series()
    idx = df_state_end.index
    size_idx = idx.size

    dict_packed = {}
    for var in df_state_end.to_dict():
        # print(var)
        # print(df_state_end[var].values.shape)
        # reshape values to (number of columns, number of grids)
        val_flatten = np.concatenate(df_state_end[var].values).ravel()
        val = val_flatten.reshape((size_idx, -1)).T
        col_names = ser_col_multi[var].values
        dict_var = dict(zip(col_names, val))
        dict_packed.update(dict_var)

    df_state_end_packed = pd.DataFrame(dict_packed, index=idx)
    df_state_end_packed.columns.set_names(["var", "ind_dim"], inplace=True)

    # swap index levels to form: {datetime, grid}
    # so using loc to retrieve the last index can get a dataframe for a restart run
    df_state_end_packed = df_state_end_packed.swaplevel()
    # df_state_end_packed.index.set_names('grid', inplace=True)

    return df_state_end_packed
