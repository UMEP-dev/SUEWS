import copy
import multiprocessing
import os
import sys
import tempfile
import time
import traceback
from ast import literal_eval
from pathlib import Path
from shutil import rmtree
from typing import Tuple

import numpy as np
import pandas
import pandas as pd
from .supy_driver import suews_driver as sd

# these are used in debug mode to save extra outputs
from .supy_driver import module_ctrl_type as sd_dts

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
    pack_dts,
    pack_dict_dts_datetime_grid,
)
from ._version import __version__ as sp_version

from ._env import logger_supy, ISSUES_URL
from .util._forcing import convert_observed_soil_moisture

from .util._debug import save_zip_debug


##############################################################################
# Error handling for Fortran kernel errors
# These replace STOP statements that would otherwise terminate Python


class SUEWSKernelError(RuntimeError):
    """Exception raised when SUEWS Fortran kernel encounters a fatal error.

    This exception is raised when the Fortran code sets an error flag instead
    of calling STOP, allowing Python to handle the error gracefully.

    Attributes
    ----------
    code : int
        Error code from the Fortran kernel (corresponds to ErrorHint codes).
    message : str
        Error message describing the problem.
    """

    def __init__(self, code: int, message: str):
        self.code = code
        self.message = message
        super().__init__(f"SUEWS Error {code}: {message}")


def _check_supy_error_from_state(state_block, timestep_idx: int = -1):
    """Check error state from SUEWS_STATE_BLOCK object.

    This is the preferred method for checking errors when state objects
    are available (debug mode). It reads from per-call state rather than
    shared module-level variables.

    Parameters
    ----------
    state_block : SUEWS_STATE_BLOCK
        The state block object containing error state from the kernel call.
    timestep_idx : int, optional
        Index of the timestep to check. Defaults to -1 (last timestep).

    Raises
    ------
    SUEWSKernelError
        If the state indicates an error occurred.
    """
    if state_block is None:
        return

    try:
        # Convert negative index to positive (f90wrap doesn't support negative indexing)
        block_len = len(state_block.block)
        if block_len == 0:
            return  # No states to check

        if timestep_idx < 0:
            timestep_idx = block_len + timestep_idx

        # Access the specified state in the block
        state = state_block.block[timestep_idx]
        error_state = state.errorstate

        if error_state.flag:
            code = int(error_state.code)
            message = str(error_state.message).strip()
            raise SUEWSKernelError(code, message)
    except SUEWSKernelError:
        # Re-raise our own error type
        raise
    except (AttributeError, IndexError, RuntimeError):
        # State object structure not as expected - fall back silently
        # RuntimeError can occur from f90wrap for unallocated arrays
        pass


def _reset_supy_error():
    """Reset error state before calling Fortran kernel."""
    from . import _supy_driver as _sd

    _sd.f90wrap_module_ctrl_error_state__reset_supy_error()


def _format_timer(timer):
    """Format a SUEWS_TIMER object for logging output.

    Parameters
    ----------
    timer : SUEWS_TIMER
        Timer object with iy, id, it, imin fields.

    Returns
    -------
    str
        Formatted timestamp string.
    """
    try:
        iy = int(timer.iy)
        id_ = int(timer.id)
        it = int(timer.it)
        imin = int(timer.imin)
        if iy == 0 and id_ == 0:
            return "no timestamp"
        return f"{iy:04d}-{id_:03d} {it:02d}:{imin:02d}"
    except (AttributeError, ValueError):
        return "unknown"


def _log_warnings_from_state(state_block):
    """Log warnings from state-based error log.

    This reads from the per-call error_state.log array which contains
    the full warning history for subroutines that pass modState.

    Parameters
    ----------
    state_block : SUEWS_STATE_BLOCK
        The state block object containing error state from the kernel call.
    """
    if state_block is None:
        return

    try:
        for state in state_block.block:
            errstate = state.errorstate
            count = int(errstate.count)
            for i in range(count):
                entry = errstate.log[i]
                if not entry.is_fatal:
                    logger_supy.warning(
                        "[%s] %s: %s",
                        _format_timer(entry.timer),
                        str(entry.location).strip(),
                        str(entry.message).strip(),
                    )
    except (AttributeError, IndexError, RuntimeError):
        # State structure not as expected - silently skip
        pass


def _log_supy_warnings():
    """Log module-level warnings (fallback for subroutines without state).

    This reads from shared module-level variables which capture warnings
    from subroutines that don't have access to modState.
    """
    from . import _supy_driver as _sd

    try:
        count = int(_sd.f90wrap_module_ctrl_error_state__get__supy_warning_count())
        if count > 0:
            message = str(
                _sd.f90wrap_module_ctrl_error_state__get__supy_last_warning_message()
            ).strip()
            logger_supy.warning(
                "SUEWS kernel warning (count %d): %s", count, message
            )
    except (AttributeError, RuntimeError):
        # Module-level variable not available - silently skip
        pass


def _reset_supy_warnings():
    """Reset module-level warning state before calling Fortran kernel."""
    from . import _supy_driver as _sd

    try:
        _sd.f90wrap_module_ctrl_error_state__set__supy_warning_count(0)
        _sd.f90wrap_module_ctrl_error_state__set__supy_last_warning_message("")
    except (AttributeError, RuntimeError):
        pass


##############################################################################

# Note: suews_cal_tstep_multi uses state-based error handling which is thread-safe.
# Each call has its own block_mod_state for error state isolation.


def suews_cal_tstep_multi(dict_state_start, df_forcing_block, debug_mode=False):
    from ._post import pack_df_output_block

    # use single dict as input for suews_cal_main
    dict_input = copy.deepcopy(dict_state_start)
    dict_input.update({
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
        "len_sim": int(df_forcing_block.shape[0]),
    })

    if debug_mode:
        dict_input["flag_test"] = True
    else:
        dict_input["flag_test"] = False

    dict_input = {k: dict_input[k] for k in list_var_input_multitsteps}
    # Pickle the input dictionary for debugging purposes
    if debug_mode:
        import pickle
        from pathlib import Path
        import datetime as dt

        # Create a timestamp for the filename
        timestamp = dt.datetime.now().strftime("%Y%m%d_%H%M%S")
        path_pickle = Path(f"supy_input_dict_{timestamp}.pkl")

        # Save the dictionary to a pickle file
        with open(path_pickle, "wb") as f:
            pickle.dump(dict_input, f)

        # Log information about the saved file
        logger_supy.debug(f"Input dictionary pickled to: {path_pickle}")
        logger_supy.debug(f"Dictionary contains {len(dict_input)} variables")
        logger_supy.debug(f"Simulation length: {dict_input['len_sim']} timesteps")

        # The pickled input dictionary can be used for:
        # 1. Debugging model crashes by loading and examining the exact input state
        # 2. Reproducing model runs with identical inputs
        # 3. Comparing inputs between different model versions
        # 4. Validating input data structure before expensive computations
        #
        # To load the pickled dictionary:
        # ```python
        # import pickle
        # from pathlib import Path
        #
        # # Replace with actual pickle file path
        # path_pickle = Path("supy_input_dict_YYYYMMDD_HHMMSS.pkl")
        #
        # with open(path_pickle, "rb") as f:
        #     dict_input_loaded = pickle.load(f)
        # ```
    # main calculation:
    # No kernel lock needed - state-based error handling is thread-safe
    # Each call has its own block_mod_state for error capture
    try:
        # Reset module-level error and warning state before kernel call
        # GH#1103: Must reset error flag to prevent stale errors from previous runs
        _reset_supy_error()
        _reset_supy_warnings()

        # Create and initialize block_mod_state for state-based error handling
        # NOTE: Must initialize from Python before kernel call, as f90wrap cannot
        # reflect Fortran ALLOCATABLE component changes back to Python
        block_mod_state = sd_dts.SUEWS_STATE_BLOCK()
        nlayer = int(dict_input["nlayer"])
        ndepth = 5  # Constant from suews_ctrl_const.f95
        len_sim = int(dict_input["len_sim"])
        block_mod_state.init(nlayer, ndepth, len_sim)

        # state_debug is only created in debug mode (for detailed diagnostics)
        state_debug = sd_dts.SUEWS_DEBUG() if debug_mode else None

        res_suews_tstep_multi = sd.suews_cal_multitsteps(
            **dict_input,
            state_debug=state_debug,
            block_mod_state=block_mod_state,
        )
        # Check for errors using state-based approach (thread-safe)
        _check_supy_error_from_state(block_mod_state)

        # Log warnings from both state-based (full log) and module-level (fallback)
        _log_warnings_from_state(block_mod_state)
        _log_supy_warnings()

    except SUEWSKernelError as e:
        # Fortran kernel set error flag instead of STOP
        logger_supy.critical("SUEWS kernel error: %s", e)
        raise
    except Exception as ex:
        # Include simulation block context for debugging
        len_sim = dict_input.get("len_sim", "unknown")
        logger_supy.exception("Kernel call failed for simulation block of length %s", len_sim)
        raise RuntimeError(f"SUEWS kernel error (block length {len_sim}): {ex}") from ex
    else:
        # update state variables
        # use deep copy to avoid reference issue; also copy the initial dict_state_start
        dict_state_end = copy.deepcopy(dict_state_start)

        # update state variables with the output of the model:
        # note `dict_input` is updated with the output of the model
        dict_state_end.update({
            var: dict_input[var] for var in list_var_inout_multitsteps
        })

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

        # convert res_mod_state to a dict
        # Assuming dts_debug is your object instance
        # deepcopy is used to avoid reference issue when passing the object
        if debug_mode:
            dict_debug = copy.deepcopy(pack_dts(state_debug))
            dts_state = block_mod_state  # save the raw object
        else:
            dict_debug = None
            dts_state = None

        # convert res_state to a dict
        # dict_state = copy.deepcopy(
        #     {
        #         ir: pack_dict_dts(dts_state)
        #         for ir, dts_state in enumerate(res_state.block)
        #     }
        # )
        if debug_mode:
            return dict_state_end, df_output_block, dict_debug, dts_state
        else:
            return dict_state_end, df_output_block


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
    df_init[("version", "0")] = sp_version

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

    # Convert observed soil moisture to deficits (if required)
    df_forcing = convert_observed_soil_moisture(df_forcing, df_init)

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
        list_dts_state = []
        for grp in grp_forcing_chunk.groups:
            # get forcing of a specific year
            df_forcing_chunk = grp_forcing_chunk.get_group(grp)
            # run supy: actual execution done in the `else` clause below
            df_output_chunk, df_state_final_chunk, df_debug_chunk, dts_state_chunk = (
                run_supy_ser(
                    df_forcing_chunk,
                    df_state_init_chunk,
                    chunk_day=chunk_day,
                    debug_mode=debug_mode,
                )
            )
            df_state_init_chunk = df_state_final_chunk.copy()
            # collect results
            list_df_output.append(df_output_chunk)
            list_df_state.append(df_state_final_chunk)
            if df_debug_chunk is not None:
                list_df_debug.append(df_debug_chunk)
            if dts_state_chunk is not None:
                list_dts_state.append(dts_state_chunk)
        # re-organise results of each year
        df_output = pd.concat(list_df_output).sort_index()
        # Note: We don't call drop_duplicates() on df_state_final because:
        # 1. Each chunk produces a unique final state for its time period
        # 2. Some columns contain numpy arrays which cannot be hashed
        df_state_final = pd.concat(list_df_state).sort_index()
        if debug_mode:
            df_debug = pd.concat(list_df_debug).sort_index()
        else:
            df_debug = None
            dict_dts_state = None

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
            if debug_mode:
                list_dict_state_end, list_df_output, list_dict_debug, list_dts_state = (
                    zip(*list_res_grid)
                )
            else:
                list_dict_state_end, list_df_output = zip(*list_res_grid)

        except Exception as e:
            path_zip_debug = save_zip_debug(df_forcing, df_state_init, error_info=e)
            raise RuntimeError(
                f"\n====================\n"
                f"SUEWS kernel error!\n"
                f"A zip file for debugging has been saved as:\n"
                f"  {path_zip_debug.as_posix()}\n"
                f"Please report this issue with the above zip file to the developer at:\n"
                f"  {ISSUES_URL}?template=issue-report.md\n"
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
        from .util import to_nan

        df_output0 = pd.concat(dict_df_output, names=["grid"]).sort_index()
        df_output = to_nan(df_output0)

        # drop ESTM for now as it is not supported yet
        df_output = df_output.drop("ESTM", axis=1, level="group")
        # trim multi-index based columns
        df_output.columns = df_output.columns.remove_unused_levels()
        if debug_mode:
            # collect debug info
            dict_debug = {
                (tstep_final, grid): debug
                for grid, debug in zip(list_grid, list_dict_debug)
            }
            df_debug = pack_dict_dts_datetime_grid(dict_debug)

            # collect state info
            dict_dts_state = {
                grid: dts_state for grid, dts_state in zip(list_grid, list_dts_state)
            }
        else:
            df_debug = None
            dict_dts_state = None

    return df_output, df_state_final, df_debug, dict_dts_state


def run_save_supy(
    df_forcing_tstep,
    df_state_init_m,
    ind,
    save_state,
    chunk_day,
    path_dir_temp,
    debug_mode=False,
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
        - {ind}_debug.pkl: Debug information DataFrame (if debug_mode=True)
        - {ind}_state_obj.pkl: Raw state objects (if debug_mode=True)
    """
    # run supy in serial mode
    result = run_supy_ser(
        df_forcing_tstep, df_state_init_m, save_state, chunk_day, debug_mode
    )

    # Save results based on debug mode
    if debug_mode:
        df_output, df_state_final, df_debug, res_state = result
        # save debug data
        path_debug = path_dir_temp / f"{ind}_debug.pkl"
        df_debug.to_pickle(path_debug)
        # save state objects
        path_state_obj = path_dir_temp / f"{ind}_state_obj.pkl"
        with open(path_state_obj, "wb") as f:
            import pickle

            pickle.dump(res_state, f)
    else:
        df_output, df_state_final, _, _ = result

    # save output and state data (always)
    path_out = path_dir_temp / f"{ind}_out.pkl"
    path_state = path_dir_temp / f"{ind}_state.pkl"
    df_output.to_pickle(path_out)
    df_state_final.to_pickle(path_state)


# parallel mode: only used on Linux/macOS; Windows is not supported yet.
def run_supy_par(
    df_forcing_tstep, df_state_init_m, save_state, chunk_day, debug_mode=False
):
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
    Tuple[pandas.DataFrame, pandas.DataFrame, pandas.DataFrame, dict]
        - df_output: Output results
        - df_state_final: Final model states
        - df_debug: Debug information (None if debug_mode=False)
        - dict_res_state: Raw state objects (None if debug_mode=False)
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
        # Always use spawn context for consistent, portable behaviour:
        # - Avoids fork warnings in multi-threaded contexts (macOS, GH-916)
        # - Avoids fork_exec signature issues (Python 3.14+)
        # - Aligns with Python's direction (3.14 changed default to forkserver)
        # - Performance overhead (~40ms/worker startup) is negligible for SUEWS
        #   since each grid runs heavy Fortran code taking seconds
        # Allow override via SUPY_MP_CONTEXT for edge cases
        mp_context = os.environ.get("SUPY_MP_CONTEXT", "spawn")
        ctx = multiprocessing.get_context(mp_context)
        with ctx.Pool() as pool:
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

        # load dumped pickle files
        df_output = pd.concat([
            pd.read_pickle(path_dir_temp / f"{n}_out.pkl") for n in np.arange(n_grid)
        ])
        df_state_final = pd.concat([
            pd.read_pickle(path_dir_temp / f"{n}_state.pkl") for n in np.arange(n_grid)
        ])

        # Handle debug mode data if available
        if debug_mode:
            df_debug = pd.concat([
                pd.read_pickle(path_dir_temp / f"{n}_debug.pkl")
                for n in np.arange(n_grid)
            ])
            # Load state objects
            import pickle

            dict_res_state = {}
            for n in np.arange(n_grid):
                path_state_obj = path_dir_temp / f"{n}_state_obj.pkl"
                with open(path_state_obj, "rb") as f:
                    dict_res_state[n] = pickle.load(f)
        else:
            df_debug = None
            dict_res_state = None

    return df_output, df_state_final, df_debug, dict_res_state


# main calculation end here
##############################################################################


# pack one Series of var into np.array
def pack_var_old(ser_var):
    dim = np.array(literal_eval(ser_var.index[-1])) + 1
    val = np.array(ser_var.values.reshape(dim), order="F")
    try:
        return val.astype(float)
    except (ValueError, TypeError):
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
            tuple(map(int, filter(None, idx.strip("()").split(","))))
            for idx in ser_var.index
        ]

        # Create new Series with tuple indices for proper sorting
        ser_var_indexed = pd.Series(ser_var.values, index=index_tuples).sort_index()

        # Get dimensions from max indices
        # Add 1 since indices are 0-based
        dimensions = np.array(ser_var_indexed.index[-1]) + 1

        # Reshape - NO need to use Fortran-style ordering
        # res = np.array(ser_var_indexed.values).reshape(dimensions, order="F")
        res = np.array(ser_var_indexed.values).reshape(dimensions)

        try:
            return res.astype(float)
        except (ValueError, TypeError):
            return res.astype(str)

    except (ValueError, AttributeError) as e:
        # Log error and fall back to scalar handling
        logger_supy.warning("Error reshaping Series: %s", e)
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
                # Skip string metadata variables that don't need packing
                if var in ["config", "description"]:
                    dict_var[var] = ser_grid[var].iloc[0]
                else:
                    # For other variables, try the alternative packing method
                    try:
                        dict_var[var] = pack_var(ser_grid[var])
                    except Exception as e2:
                        # Log at WARNING level - dropped variables could cause kernel errors
                        logger_supy.warning(
                            "Could not pack variable '%s' (tried both methods): %s / %s",
                            var, e, e2
                        )
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

    # Extract scalars from single-element arrays for rank-0 variables
    # This fixes NumPy 2.0 deprecation warning about array-to-scalar conversion
    list_var_scalar = df_var_info[df_var_info["rank"] == 0].index
    for var in list_var_scalar:
        if var in dict_var:
            val = dict_var[var]
            if isinstance(val, np.ndarray) and val.size == 1:
                dict_var[var] = val.item()

    return dict_var


# pack final state to the same format as initial state
def pack_df_state_final(df_state_end, df_state_start):
    ser_col_multi = df_state_start.columns.to_series()
    idx = df_state_end.index
    size_idx = idx.size

    dict_packed = {}
    for var in df_state_end.to_dict():
        values = df_state_end[var].values

        first_val = values[0] if len(values) else None
        if isinstance(first_val, np.ndarray) and first_val.ndim > 0:
            # Normal state variables: concatenate and reshape as before
            val_flatten = np.concatenate(values).ravel()
            val = val_flatten.reshape((size_idx, -1)).T
        else:
            # Metadata columns (strings/scalars) must bypass concatenate; ensure we
            # extract scalar values from any 0-D numpy arrays before reshaping.
            scalars = [
                v.item()
                if isinstance(v, np.ndarray) and getattr(v, "ndim", 0) == 0
                else v
                for v in values
            ]
            val = np.array(scalars, dtype=object).reshape((size_idx, -1)).T

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
