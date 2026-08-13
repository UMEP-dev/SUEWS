from pathlib import Path
from typing import Optional, Tuple

import f90nml
import numpy as np
import pandas as pd

# import ray
from ._env import logger_supy
from ._load import load_SUEWS_dict_ModConfig
from ._post import _resample_output, df_var as df_var_out


_OUTPUT_TIMESTAMP_REFERENCES = {
    "follow",
    "utc",
    "local_standard_time",
    "daylight",
}
_FORCING_TIMESTAMP_REFERENCES = {"utc", "local_standard_time"}


def _normalise_output_timestamp_reference(timestamp_reference: object) -> str:
    """Return a validated output timestamp-reference value."""
    label = getattr(timestamp_reference, "value", timestamp_reference)
    label = "follow" if label is None else str(label).lower()
    if label not in _OUTPUT_TIMESTAMP_REFERENCES:
        expected = ", ".join(sorted(_OUTPUT_TIMESTAMP_REFERENCES))
        raise ValueError(
            f"Invalid output timestamp reference {timestamp_reference!r}; "
            f"expected one of: {expected}."
        )
    return label


def _normalise_forcing_timestamp_reference(timestamp_reference: object) -> str:
    """Return a validated forcing timestamp-reference value."""
    label = getattr(timestamp_reference, "value", timestamp_reference)
    label = "local_standard_time" if label is None else str(label).lower()
    if label not in _FORCING_TIMESTAMP_REFERENCES:
        expected = ", ".join(sorted(_FORCING_TIMESTAMP_REFERENCES))
        raise ValueError(
            f"Invalid forcing timestamp reference {timestamp_reference!r}; "
            f"expected one of: {expected}."
        )
    return label


def _timestamp_reference_filename_suffix(timestamp_reference: object) -> str:
    """Return the filename suffix for an explicit output reference."""
    label = _normalise_output_timestamp_reference(timestamp_reference)
    return {
        "follow": "",
        "utc": "_UTC",
        "local_standard_time": "_STANDARD",
        "daylight": "_DAYLIGHT",
    }[label]


def _state_scalar(
    df_state_final: Optional[pd.DataFrame], grid: object, name: str
) -> object:
    """Read a per-grid scalar from DFState's legacy column shapes."""
    if df_state_final is None or df_state_final.empty:
        raise ValueError(
            f"Cannot relabel output timestamps without df_state_final field {name!r}."
        )

    try:
        row = df_state_final.loc[grid]
    except KeyError:
        try:
            row = df_state_final.loc[int(grid)]
        except (KeyError, TypeError, ValueError):
            if len(df_state_final.index) != 1:
                raise
            row = df_state_final.iloc[0]

    if isinstance(row.index, pd.MultiIndex):
        key = (name, "0")
        if key in row.index:
            return row.loc[key]
        matches = [
            column for column in row.index if str(column[0]).lower() == name.lower()
        ]
        if matches:
            return row.loc[matches[0]]

    if name in row.index:
        return row.loc[name]

    matches = [column for column in row.index if str(column).lower() == name.lower()]
    if matches:
        return row.loc[matches[0]]

    raise ValueError(
        f"Cannot relabel output timestamps: missing DFState field {name!r}."
    )


def _dls_mask(
    idx_standard: pd.DatetimeIndex, startdls: float, enddls: float
) -> np.ndarray:
    """Return the daylight-saving mask on the local-standard clock."""
    seconds = (
        idx_standard.hour * 3600
        + idx_standard.minute * 60
        + idx_standard.second
        + idx_standard.microsecond / 1_000_000
    )
    decimal_doy = idx_standard.dayofyear.to_numpy(dtype=float) + seconds / 86400
    if startdls <= enddls:
        return (decimal_doy >= startdls) & (decimal_doy < enddls)
    return (decimal_doy >= startdls) | (decimal_doy < enddls)


def _relabel_datetime_index(
    idx_dt: pd.DatetimeIndex,
    timestamp_reference: object,
    forcing_timestamp_reference: object,
    timezone_offset: object,
    startdls: Optional[object] = None,
    enddls: Optional[object] = None,
) -> pd.DatetimeIndex:
    """Convert one grid's naive index from its forcing clock to the target."""
    target = _normalise_output_timestamp_reference(timestamp_reference)
    source = _normalise_forcing_timestamp_reference(forcing_timestamp_reference)
    if target in {"follow", source}:
        return idx_dt

    timezone_delta = pd.to_timedelta(float(timezone_offset), unit="h")
    idx_standard = (
        idx_dt if source == "local_standard_time" else idx_dt + timezone_delta
    )

    if target == "local_standard_time":
        return idx_standard
    if target == "utc":
        return idx_standard - timezone_delta
    if target == "daylight":
        if startdls is None or enddls is None:
            raise ValueError(
                "output.timestamp_reference='daylight' requires startdls and enddls."
            )
        startdls_value = float(startdls)
        enddls_value = float(enddls)
        if not 1 <= startdls_value <= 366 or not 1 <= enddls_value <= 366:
            raise ValueError(
                "output.timestamp_reference='daylight' requires startdls and enddls "
                "between day 1 and day 366."
            )
        dls_offsets = pd.to_timedelta(
            _dls_mask(idx_standard, startdls_value, enddls_value).astype(int),
            unit="h",
        )
        return idx_standard + dls_offsets

    raise AssertionError(f"Unhandled output timestamp reference: {target}")


def relabel_output_timestamps(
    df_output: pd.DataFrame,
    timestamp_reference: object,
    forcing_timestamp_reference: object,
    df_state_final: Optional[pd.DataFrame],
) -> pd.DataFrame:
    """Relabel output timestamps without changing computed values."""
    target = _normalise_output_timestamp_reference(timestamp_reference)
    source = _normalise_forcing_timestamp_reference(forcing_timestamp_reference)
    if target in {"follow", source}:
        return df_output

    df_relabelled = df_output.copy()
    index = df_relabelled.index

    if isinstance(index, pd.MultiIndex):
        if "datetime" not in index.names:
            raise ValueError(
                "Cannot relabel output timestamps: index has no datetime level."
            )
        datetime_level = index.names.index("datetime")
        grid_level = index.names.index("grid") if "grid" in index.names else None
        idx_dt = pd.DatetimeIndex(index.get_level_values(datetime_level))

        if grid_level is None:
            grid = df_state_final.index[0] if df_state_final is not None else None
            new_dt = _relabel_datetime_index(
                idx_dt,
                target,
                source,
                _state_scalar(df_state_final, grid, "timezone"),
                _state_scalar(df_state_final, grid, "startdls")
                if target == "daylight"
                else None,
                _state_scalar(df_state_final, grid, "enddls")
                if target == "daylight"
                else None,
            )
        else:
            grids = index.get_level_values(grid_level)
            new_dt = pd.Series(idx_dt, index=range(len(idx_dt)), dtype="datetime64[ns]")
            for grid in pd.Index(grids).unique():
                mask = grids == grid
                new_dt.loc[mask] = _relabel_datetime_index(
                    pd.DatetimeIndex(idx_dt[mask]),
                    target,
                    source,
                    _state_scalar(df_state_final, grid, "timezone"),
                    _state_scalar(df_state_final, grid, "startdls")
                    if target == "daylight"
                    else None,
                    _state_scalar(df_state_final, grid, "enddls")
                    if target == "daylight"
                    else None,
                )
            new_dt = pd.DatetimeIndex(new_dt)

        arrays = [
            new_dt if level == datetime_level else index.get_level_values(level)
            for level in range(index.nlevels)
        ]
        df_relabelled.index = pd.MultiIndex.from_arrays(arrays, names=index.names)
        return df_relabelled

    if isinstance(index, pd.DatetimeIndex):
        grid = df_state_final.index[0] if df_state_final is not None else None
        df_relabelled.index = _relabel_datetime_index(
            index,
            target,
            source,
            _state_scalar(df_state_final, grid, "timezone"),
            _state_scalar(df_state_final, grid, "startdls")
            if target == "daylight"
            else None,
            _state_scalar(df_state_final, grid, "enddls")
            if target == "daylight"
            else None,
        )
        return df_relabelled

    raise ValueError("Cannot relabel output timestamps: index is not datetime-like.")


def _output_group_frequencies(df_output: pd.DataFrame) -> dict:
    """Return each output group's nominal source-clock frequency in seconds."""
    dict_group_frequency_s = {}
    for group in df_output.columns.get_level_values("group").unique():
        df_group = df_output[group].dropna(how="all")
        if isinstance(df_group.index, pd.MultiIndex) and "grid" in df_group.index.names:
            grid = df_group.index.get_level_values("grid")[0]
            df_group = df_group.xs(grid, level="grid")
        idx_group = pd.DatetimeIndex(
            df_group.index.get_level_values("datetime")
            if isinstance(df_group.index, pd.MultiIndex)
            else df_group.index
        ).drop_duplicates()
        if len(idx_group) < 2:
            continue
        ser_deltas = idx_group.to_series().diff().dropna()
        ser_positive_deltas = ser_deltas[ser_deltas > pd.Timedelta(0)]
        if ser_positive_deltas.empty:
            continue
        frequency = ser_positive_deltas.mode().iloc[0]
        dict_group_frequency_s[group] = int(frequency.total_seconds())
    return dict_group_frequency_s


def gen_df_save(df_grid_group: pd.DataFrame) -> pd.DataFrame:
    """generate a dataframe for saving

    Parameters
    ----------
    df_grid_group : pd.DataFrame
        an output dataframe of a single group and grid

    Returns
    -------
    pd.DataFrame
        a dataframe with date time info prepended for saving
    """
    # generate df_datetime for prepending
    idx_dt = df_grid_group.index
    ser_year = pd.Series(idx_dt.year, index=idx_dt, name="Year")
    ser_DOY = pd.Series(idx_dt.dayofyear, index=idx_dt, name="DOY")
    ser_hour = pd.Series(idx_dt.hour, index=idx_dt, name="Hour")
    ser_min = pd.Series(idx_dt.minute, index=idx_dt, name="Min")
    df_datetime = pd.concat(
        [
            ser_year,
            ser_DOY,
            ser_hour,
            ser_min,
        ],
        axis=1,
    )
    dt_delta = idx_dt - idx_dt.to_period("d").to_timestamp()
    df_datetime["Dectime"] = ser_DOY - 1 + dt_delta.total_seconds() / (24 * 60 * 60)
    df_save = pd.concat([df_datetime, df_grid_group], axis=1)
    return df_save


def format_df_save(df_save):
    # format datetime columns
    for var in df_save.columns[:4]:
        width_var_name = max([3, len(var)])
        df_save[var] = df_save[var].map(
            lambda s: "{s:{c}>{n}}".format(s=s, n=width_var_name, c=" ")
        )

    df_save.Dectime = df_save.Dectime.map(
        lambda s: "{s:{c}>{n}.4f}".format(s=s, n=8, c=" ")
    )
    # fill nan values
    df_save = df_save.fillna(-999.0)
    # format value columns

    for var in df_save.columns[5:]:
        width_var_name = max([8, len(var)])
        df_save[var] = df_save[var].map(lambda s: f"{s:{' '}>{width_var_name}.4f}")

    # format column names
    col_fmt = df_save.columns.to_series()
    col_fmt[4:] = col_fmt[4:].map(
        lambda s: "{s:{c}>{n}}".format(s=s, n=max([8, len(s)]), c=" ")
    )
    df_save.columns = col_fmt

    return df_save


# def save_df_grid_group_year(
#     df_save, grid, group, year, output_level=1, site="test", dir_save=".",
# ):
#     df_year = gen_df_year(df_save, year, grid, group, output_level)

#     path_out = save_df_grid_group(df_year, grid, group, dir_save, site)
#     return path_out


def gen_df_year(
    df_save,
    year,
    grid,
    group,
    output_level,
    timestamps_are_final=False,
    allow_irregular_timestamps=False,
):
    # retrieve dataframe of grid for `group`
    # First filter by grid from the index
    df_grid = df_save.xs(grid, level="grid")
    # Then select the group from columns
    df_grid_group = df_grid[group].copy()
    # get temporal index
    idx_dt = df_grid_group.index
    if len(idx_dt) > 1 and not allow_irregular_timestamps:
        freq = idx_dt.to_series().diff().iloc[-1]
        if pd.notna(freq):
            df_grid_group = df_grid_group.asfreq(freq)
    # select output variables in `SUEWS` based on output level
    if group == "SUEWS":
        # Filter to only variables that exist in the dataframe
        vars_to_keep = [
            v for v in dict_level_var[output_level] if v in df_grid_group.columns
        ]
        if vars_to_keep:
            df_grid_group = df_grid_group[vars_to_keep]
    # select data from year of interest and shift back to align with SUEWS convention
    df_year = df_grid_group.loc[f"{year}"]
    # Skip timestamp shift for DailyState as it contains end-of-day values
    if group != "DailyState" and not timestamps_are_final:
        df_year.index = df_year.index.shift(1)
    # remove `nan`s
    df_year = df_year.dropna(how="all", axis=0)
    return df_year


def save_df_grid_group(
    df_year,
    grid,
    group,
    dir_save,
    site,
    timestamp_reference="follow",
    freq_s=None,
    allow_irregular_timestamps=False,
):
    # processing path
    path_dir = Path(dir_save)
    # pandas bug here: monotonic datetime index would lose `freq` once `pd.concat`ed
    if freq_s is not None:
        freq = pd.Timedelta(freq_s, unit="s")
        if not allow_irregular_timestamps:
            df_year = df_year.asfreq(freq)
    elif allow_irregular_timestamps:
        raise ValueError(
            "An output frequency is required for irregular daylight timestamps."
        )
    elif df_year.shape[0] > 0 and df_year.index.size >= 2:
        ind = df_year.index
        freq_cal = ind[1] - ind[0]
        df_year = df_year.asfreq(freq_cal)
        freq = pd.Timedelta(df_year.index.freq)
    else:
        df_year = df_year.asfreq("5min")
        freq = pd.Timedelta(df_year.index.freq)
    # output frequency in min
    freq_min = int(freq.total_seconds() / 60)
    # starting year
    try:
        year = df_year.index[0].year
    except (AttributeError, IndexError):
        logger_supy.debug("Could not extract year from df_year index:\n%s", df_year)

    # sample file name: 'Kc98_2012_SUEWS_60.txt'
    timestamp_suffix = _timestamp_reference_filename_suffix(timestamp_reference)
    file_out = f"{site}{grid}_{year}_{group}_{freq_min}{timestamp_suffix}.txt"
    # 'DailyState_1440' will be trimmed
    file_out = file_out.replace("DailyState_1440", "DailyState")
    path_out = path_dir / file_out
    logger_supy.debug(f"writing out: {path_out}")
    import time

    t_start = time.time()
    # generate df_save with datetime info prepended to each row
    df_save = gen_df_save(df_year)
    t_end = time.time()
    logger_supy.debug(
        f"df_save for {path_out.name} is generated in {t_end - t_start:.2f} s"
    )
    # format df_save with right-justified view
    df_save = format_df_save(df_save)
    t_start = time.time()
    # save to txt file
    df_save.to_csv(
        path_out,
        index=False,
        sep="\t",
    )
    t_end = time.time()
    # remove freq info from `DailyState` file
    if "DailyState" in path_out.name:
        str_fn_dd = str(path_out).replace("DailyState_5", "DailyState")
        path_out.rename(Path(str_fn_dd))
        path_out = Path(str_fn_dd)
    logger_supy.debug(f"{path_out} saved in {t_end - t_start:.2f} s")
    return path_out


# @ray.remote
# def save_df_year(df_year, grid, group, year, output_level, site, dir_save):
#     return save_df_grid_group_year(
#         df_year, grid, group, year, output_level, site, dir_save
#     )


# a pd.Series of variables of different output levels
ser_level_var = df_var_out.loc["SUEWS", "outlevel"].astype(int)

# a dict of variables of different output level
dict_level_var = {
    # all but snow-related variables
    0: ser_level_var.loc[ser_level_var <= 1].index,
    # all output variables
    1: ser_level_var.loc[ser_level_var <= 2].index,
    # minimal set of variables
    2: ser_level_var.loc[ser_level_var == 0].index,
}


# save output files
def save_df_output(
    df_output: pd.DataFrame,
    freq_s: int = 3600,
    site: str = "",
    path_dir_save: Path = Path("."),
    save_tstep=False,
    output_level=1,
    save_snow=True,
    debug=False,
    output_groups=None,
    timestamp_reference="follow",
    forcing_timestamp_reference="local_standard_time",
    df_state_final: Optional[pd.DataFrame] = None,
) -> list:
    """save supy output dataframe to txt files

    Parameters
    ----------
    df_output : pd.DataFrame
        output dataframe of supy simulation
    freq_s : int, optional
        output frequency in second (the default is 3600, which indicates the a txt with hourly values)
    path_dir_save : pathlib.Path, optional
        directory to save txt files (the default is '.', which the current working directory)
    site : str, optional
        site code used for filename (the default is '', which indicates no site name prepended to the filename)
    save_tstep : bool, optional
        whether to save results in temporal resolution as in simulation (which may result very large files and slow progress), by default False.
    output_level : integer, optional
        option to determine selection of output variables, by default 1.
        Notes: 0 for all but snow-related; 1 for all; 2 for a minimal set without land cover specific information.
    save_snow : bool, optional
        whether to save snow-related output variables in a separate file, by default True.
    debug : bool, optional
        whether to enable debug mode (e.g., writing out in serial mode, and other debug uses), by default False.
    output_groups : list, optional
        list of output groups to save (e.g., ['SUEWS', 'DailyState', 'ESTM']). If None, defaults to ['SUEWS', 'DailyState'].
    timestamp_reference : str, optional
        Saved timestamp reference. ``follow`` preserves the forcing clock.
    forcing_timestamp_reference : str, optional
        Clock used by ``df_output`` before any saved-output relabelling.
    df_state_final : pandas.DataFrame, optional
        Per-grid timezone and daylight-saving parameters used for relabelling.

    Returns
    -------
    list
        a list of `Path` objects for saved txt files
    """
    # save a local copy
    df_save = df_output.copy()

    # path list of files to save
    list_path_save = []

    # resample output if `freq_s` is different from runtime `freq` (usually 5 min)
    freq_save = pd.Timedelta(freq_s, "s")

    # Handle output groups filtering
    if output_groups is None:
        # Default groups
        output_groups = ["SUEWS", "DailyState"]

    # Get all available groups
    all_groups = df_save.columns.get_level_values("group").unique().tolist()

    # Filter to only requested groups
    groups_to_drop = [g for g in all_groups if g not in output_groups]
    for group in groups_to_drop:
        if group in df_save.columns.get_level_values("group"):
            df_save = df_save.drop(group, axis=1, level="group")

    # drop snow related group from output groups if not requested
    if not save_snow and "snow" in df_save.columns.get_level_values("group"):
        df_save = df_save.drop("snow", axis=1, level="group")

    # Extract DailyState before resampling (it contains daily variables only written at last timestep of each day)
    df_dailystate = None
    if "DailyState" in df_save.columns.get_level_values("group"):
        df_dailystate = df_save.loc[:, ["DailyState"]].copy()
        # Remove all NaN rows from DailyState (keep only the daily values)
        df_dailystate = df_dailystate.dropna(how="all")
        # Drop DailyState from df_save before resampling
        df_save_no_daily = df_save.drop("DailyState", axis=1, level="group")
    else:
        df_save_no_daily = df_save

    # resample `df_output` at `freq_save` (excluding DailyState)
    if len(df_save_no_daily.columns) > 0:
        df_rsmp = _resample_output(df_save_no_daily, freq_save)
    else:
        df_rsmp = None

    # dataframes to save
    if save_tstep:
        # both original and resampled output dataframes
        list_df_save = [
            df for df in [df_save, df_rsmp] if df is not None and len(df.columns) > 0
        ]
    else:
        # combine resampled data with DailyState (if it exists)
        list_df_save = []
        if df_dailystate is not None:
            list_df_save.append(df_dailystate)
        if df_rsmp is not None:
            list_df_save.append(df_rsmp)

    # save output at the resampling frequency
    output_timestamp_reference = _normalise_output_timestamp_reference(
        timestamp_reference
    )
    timestamps_are_final = output_timestamp_reference != "follow"
    for df_save in list_df_save:
        dict_group_frequency_s = (
            _output_group_frequencies(df_save) if timestamps_are_final else None
        )
        # Check if this is DailyState-only data
        is_dailystate_only = len(df_save.columns) > 0 and all(
            df_save.columns.get_level_values("group") == "DailyState"
        )

        if not is_dailystate_only and not timestamps_are_final:
            # For regular output data, shift temporal index to make timestamps indicating the start of periods
            idx_dt = df_save.index.get_level_values("datetime").drop_duplicates()

            # cast freq to index if not associated
            if idx_dt.freq is None:
                ser_idx = idx_dt.to_series()
                if len(ser_idx) > 1:
                    freq = ser_idx.diff().iloc[-1]
                    idx_dt = ser_idx.asfreq(freq).index
                else:
                    idx_dt = ser_idx.index

            # Shift timestamps for non-DailyState data
            if len(idx_dt) > 1:
                idx_dt = idx_dt.shift(-1)

            # Update the index
            df_save.index = df_save.index.set_levels(idx_dt, level="datetime")
        # For DailyState data, we don't need to shift the index as it already represents daily values
        df_save = relabel_output_timestamps(
            df_save,
            timestamp_reference,
            forcing_timestamp_reference,
            df_state_final,
        )
        # tidy up columns so only necessary groups are included in the output
        df_save.columns = df_save.columns.remove_unused_levels()
        # import os
        # if os.name != "nt" and not debug:
        #
        #     try:
        #         # PARALLEL mode:
        #         # supported by ray: only used on Linux/macOS; Windows is not supported yet.
        #         ray.shutdown()
        #         ray.init(object_store_memory=4 * 1000 ** 3)
        #         list_path_save_df = save_df_par(df_save, path_dir_save, site, output_level)
        #         ray.shutdown()
        #     except:
        #         # fallback to SERIAL mode
        #         logger_supy.warning('falling back to serial mode for writing out results.')
        #         list_path_save_df = save_df_ser(df_save, path_dir_save, site, output_level)
        #
        # else:
        # SERIAL mode: only on Windows
        list_path_save_df = save_df_ser(
            df_save,
            path_dir_save,
            site,
            output_level,
            timestamp_reference,
            timestamps_are_final=timestamps_are_final,
            dict_group_frequency_s=dict_group_frequency_s,
        )

        # add up path list
        list_path_save += list_path_save_df
    return list_path_save


# def save_df_year_par(df_year, output_level, site, dir_save):
#     ray.shutdown()
#     info_ray = ray.init(object_store_memory=1 * 1000 ** 3)
#     list_path = []
#     id_df_year = ray.put(df_year)
#     list_grid = df_year.index.levels[0]
#     list_group = df_year.columns.levels[0]
#     for grid in list_grid:
#         for group in list_group:
#             list_path.append(
#                 save_df_year.remote(
#                     id_df_year, grid, group, year, output_level, site, dir_save
#                 )
#             )
#     list_path = ray.get(list_path)
#     ray.shutdown()
#     return list_path


# # save `df_save` in serial mode
# def save_df_par(df_save, path_dir_save, site, output_level):
#     # number of years for grouping
#     n_yr = 5
#     idx_yr = df_save.index.get_level_values("datetime").year
#     grp_year = df_save.groupby((idx_yr - idx_yr.min()) // n_yr,)
#
#     list_path = []
#     for grp in grp_year.groups:
#         df_grp = grp_year.get_group(grp)
#         list_grid = df_grp.index.get_level_values("grid").unique()
#         list_group = df_grp.columns.get_level_values("group").unique()
#         list_year = df_grp.index.get_level_values("datetime").year[-1].unique()
#         # put large df as common data object for parallel mode
#         id_df_grp = ray.put(df_grp)
#
#         # the below runs in parallel by `ray.remote`
#         for year in list_year:
#             for grid in list_grid:
#                 for group in list_group:
#                     list_path.append(
#                         save_df_year.remote(
#                             id_df_grp,
#                             grid,
#                             group,
#                             year,
#                             output_level,
#                             site,
#                             path_dir_save,
#                         )
#                     )
#
#     list_path = ray.get(list_path)
#
#     return list_path


# save `df_save` in serial mode
def save_df_ser(
    df_save,
    path_dir_save,
    site,
    output_level,
    timestamp_reference="follow",
    timestamps_are_final=False,
    dict_group_frequency_s=None,
):
    list_grid = df_save.index.get_level_values("grid").unique()
    list_group = df_save.columns.get_level_values("group").unique()
    is_dailystate_only = len(list_group) == 1 and list_group[0] == "DailyState"
    idx_year = df_save.index.get_level_values("datetime").year
    if is_dailystate_only or timestamps_are_final:
        list_year = idx_year.unique()
    else:
        # the last index value is dropped as supy uses starting timestamp of each year
        # for naming files
        list_year = idx_year[:-1].unique()
    list_path_save_df = []
    for grid in list_grid:
        for group in list_group:
            for year in list_year:
                allow_irregular_timestamps = (
                    _normalise_output_timestamp_reference(timestamp_reference)
                    == "daylight"
                )
                df_year = gen_df_year(
                    df_save,
                    year,
                    grid,
                    group,
                    output_level,
                    timestamps_are_final=timestamps_are_final,
                    allow_irregular_timestamps=allow_irregular_timestamps,
                )
                if df_year.shape[0] > 0:
                    group_frequency_s = (
                        dict_group_frequency_s.get(group)
                        if dict_group_frequency_s is not None
                        else None
                    )
                    path_save = save_df_grid_group(
                        df_year,
                        grid,
                        group,
                        path_dir_save,
                        site,
                        timestamp_reference,
                        freq_s=group_frequency_s,
                        allow_irregular_timestamps=allow_irregular_timestamps,
                    )
                    list_path_save_df.append(path_save)
    return list_path_save_df


# save model state for restart runs
def save_df_state(
    df_state: pd.DataFrame,
    site: str = "",
    path_dir_save: Path = Path("."),
) -> Path:
    """save `df_state` to a csv file

    Parameters
    ----------
    df_state : pd.DataFrame
        a dataframe of model states produced by a supy run
    site : str, optional
        site identifier (the default is '', which indicates an empty site code)
    path_dir_save : pathlib.Path, optional
        path to directory to save results (the default is Path('.'), which the current working directory)

    Returns
    -------
    Path
        path to the saved csv file
    """

    file_state_save = "df_state_{site}.csv".format(site=site)
    # trim filename if site == ''
    file_state_save = file_state_save.replace("_.csv", ".csv")
    path_state_save = Path(path_dir_save) / file_state_save
    logger_supy.debug(f"writing out: {path_state_save}")
    df_state.to_csv(path_state_save)
    return path_state_save


# get information for saving results
def get_save_info(path_runcontrol: str) -> Tuple[int, Path, str]:
    """get necessary information for saving supy results, which are (freq_s, dir_save, site)

    Parameters
    ----------
    path_runcontrol : pathlib.Path
        Path to SUEWS :ref:`RunControl.nml <suews:RunControl.nml>`

    Returns
    -------
    tuple
        A tuple including (freq_s, dir_save, site, writeoutoption):
        freq_s: output frequency in seconds
        dir_save: directory name to save results
        site: site identifier
        writeoutoption: option for selection of output variables
    """

    try:
        path_runcontrol = Path(path_runcontrol).expanduser().resolve()
    except FileNotFoundError:
        logger_supy.exception(f"{path_runcontrol} does not exists!")
    else:
        dict_mod_cfg = load_SUEWS_dict_ModConfig(path_runcontrol)
        freq_s, dir_save, site, save_tstep, writeoutoption = [
            dict_mod_cfg[x]
            for x in [
                "resolutionfilesout",
                "fileoutputpath",
                "filecode",
                "keeptstepfilesout",
                "writeoutoption",
            ]
        ]
        dir_save = path_runcontrol.parent / dir_save
        if not dir_save.exists():
            dir_save.mkdir()
        return freq_s, dir_save, site, save_tstep, writeoutoption


# TODO: fix gdd/sdd initialisation
# dict for {nml_save:(df_state_var,index)}
dict_init_nml = {
    "dayssincerain": ("hdd_id", "(11,)"),
    "temp_c0": ("hdd_id", "(8,)"),
    "gdd_1_0": ("gdd_id", "(0,)"),
    "gdd_2_0": ("sdd_id", "(0,)"),
    "laiinitialevetr": ("lai_id", "(0,)"),
    "laiinitialdectr": ("lai_id", "(1,)"),
    "laiinitialgrass": ("lai_id", "(2,)"),
    "albevetr0": ("albevetr_id", "0"),
    "albdectr0": ("albdectr_id", "0"),
    "albgrass0": ("albgrass_id", "0"),
    "decidcap0": ("decidcap_id", "0"),
    "porosity0": ("porosity_id", "0"),
    "soilstorepavedstate": ("soilstore_surf", "(0,)"),
    "soilstorebldgsstate": ("soilstore_surf", "(1,)"),
    "soilstoreevetrstate": ("soilstore_surf", "(2,)"),
    "soilstoredectrstate": ("soilstore_surf", "(3,)"),
    "soilstoregrassstate": ("soilstore_surf", "(4,)"),
    "soilstorebsoilstate": ("soilstore_surf", "(5,)"),
    "pavedstate": ("state_surf", "(0,)"),
    "bldgsstate": ("state_surf", "(1,)"),
    "evetrstate": ("state_surf", "(2,)"),
    "dectrstate": ("state_surf", "(3,)"),
    "grassstate": ("state_surf", "(4,)"),
    "bsoilstate": ("state_surf", "(5,)"),
    "waterstate": ("state_surf", "(6,)"),
    "snowwaterpavedstate": ("snowwater", "(0,)"),
    "snowwaterbldgsstate": ("snowwater", "(1,)"),
    "snowwaterevetrstate": ("snowwater", "(2,)"),
    "snowwaterdectrstate": ("snowwater", "(3,)"),
    "snowwatergrassstate": ("snowwater", "(4,)"),
    "snowwaterbsoilstate": ("snowwater", "(5,)"),
    "snowwaterwaterstate": ("snowwater", "(6,)"),
    "snowpackpaved": ("snowpack", "(0,)"),
    "snowpackbldgs": ("snowpack", "(1,)"),
    "snowpackevetr": ("snowpack", "(2,)"),
    "snowpackdectr": ("snowpack", "(3,)"),
    "snowpackgrass": ("snowpack", "(4,)"),
    "snowpackbsoil": ("snowpack", "(5,)"),
    "snowpackwater": ("snowpack", "(6,)"),
    "snowfracpaved": ("snowfrac", "(0,)"),
    "snowfracbldgs": ("snowfrac", "(1,)"),
    "snowfracevetr": ("snowfrac", "(2,)"),
    "snowfracdectr": ("snowfrac", "(3,)"),
    "snowfracgrass": ("snowfrac", "(4,)"),
    "snowfracbsoil": ("snowfrac", "(5,)"),
    "snowfracwater": ("snowfrac", "(6,)"),
    "snowdenspaved": ("snowdens", "(0,)"),
    "snowdensbldgs": ("snowdens", "(1,)"),
    "snowdensevetr": ("snowdens", "(2,)"),
    "snowdensdectr": ("snowdens", "(3,)"),
    "snowdensgrass": ("snowdens", "(4,)"),
    "snowdensbsoil": ("snowdens", "(5,)"),
    "snowdenswater": ("snowdens", "(6,)"),
    "snowalb0": ("snowalb", "0"),
}


# save initcond namelist as SUEWS binary
def save_initcond_nml(
    df_state: pd.DataFrame,
    site: str = "",
    path_dir_save: Path = Path("."),
) -> Path:
    # get last time step
    try:
        tstep_last = df_state.index.levels[0].max()
    except AttributeError:
        logger_supy.exception(
            (
                "incorrect structure detected;"
                + " check if `df_state` is the final model state."
            )
        )
        return

    # get year for filename formatting
    year_last = tstep_last.year
    # generate a df with records of the last tstep
    df_state_last_tstep = df_state.loc[tstep_last]
    # get grid list
    list_grid = df_state_last_tstep.index

    # list holder for paths written out in nml
    list_path_nml = []
    for grid in list_grid:
        # generate nml filename
        filename_out_grid = f"InitialConditions{site}{grid}_{year_last}_EndofRun.nml"
        # derive a save path
        path_nml = path_dir_save / filename_out_grid
        # retrieve initcond values from `df_state_last_tstep`
        nml = {
            "InitialConditions": {
                key: df_state_last_tstep.loc[grid, var]
                for key, var in dict_init_nml.items()
            }
        }
        # save nml
        f90nml.write(nml, path_nml, force=True)
        # f90nml.write(nml, nml_file,force=True)
        list_path_nml.append(path_nml)
    return list_path_nml


def save_df_output_parquet(
    df_output: pd.DataFrame,
    df_state_final: pd.DataFrame,
    freq_s: int = 3600,
    site: str = "",
    path_dir_save: Path = Path("."),
    save_tstep=False,
    save_state: bool = True,
    site_metadata: Optional[str] = None,
    timestamp_reference="follow",
    forcing_timestamp_reference="local_standard_time",
) -> list:
    """Save supy output to Parquet format.

    Parameters
    ----------
    df_output : pd.DataFrame
        Output dataframe from supy simulation
    df_state_final : pd.DataFrame
        Final state dataframe
    freq_s : int, optional
        Output frequency in seconds (default 3600)
    site : str, optional
        Site identifier for filename
    path_dir_save : pathlib.Path, optional
        Directory to save Parquet file
    save_tstep : bool, optional
        Whether to save at simulation timestep resolution
    save_state : bool, optional
        Whether to write the legacy final-state Parquet file.
    site_metadata : str, optional
        Original site identifier to record in metadata when ``site`` is a
        filesystem-safe filename token. Defaults to ``site``.
    timestamp_reference : str, optional
        Saved timestamp reference. ``follow`` preserves the forcing clock.
    forcing_timestamp_reference : str, optional
        Clock used by ``df_output`` before any saved-output relabelling.

    Returns
    -------
    list
        List containing paths to saved Parquet files
    """
    # Check if pyarrow is available
    try:
        import pyarrow

        engine = "pyarrow"
    except ImportError as e:
        raise ImportError(
            "Parquet output requires 'pyarrow'. "
            "Install with: pip install 'supy[parquet]' or pip install pyarrow"
        ) from e

    from ._version import __version__

    # Resample if needed
    df_save = df_output.copy()
    freq_save = pd.Timedelta(freq_s, "s")

    if not save_tstep:
        # Resample output
        df_rsmp = _resample_output(df_save, freq_save)

        # MP: TODO: This causes duplicate entries for DailyState. Why keep the original resolution?
        # Keep DailyState at original resolution
        # if 'DailyState' in df_save.columns.get_level_values('group'):
        #     df_daily = df_save.loc[:, ["DailyState"]]
        #     # Combine for saving
        #     df_to_save = pd.concat([df_rsmp, df_daily], axis=1)
        # else:
        df_to_save = df_rsmp
    else:
        df_to_save = df_save

    df_to_save = relabel_output_timestamps(
        df_to_save,
        timestamp_reference,
        forcing_timestamp_reference,
        df_state_final,
    )

    # Construct filenames
    list_path_save = []
    timestamp_suffix = _timestamp_reference_filename_suffix(timestamp_reference)
    timestamp_reference_name = _normalise_output_timestamp_reference(
        timestamp_reference
    )

    # Save output data
    filename_output = (
        f"{site}_SUEWS_output{timestamp_suffix}.parquet"
        if site
        else f"SUEWS_output{timestamp_suffix}.parquet"
    )
    path_output = path_dir_save / filename_output

    # Save with metadata
    metadata = {
        "site": site if site_metadata is None else site_metadata,
        "output_frequency_s": freq_s,
        "save_tstep": save_tstep,
        "timestamp_reference": timestamp_reference_name,
        "creation_time": pd.Timestamp.now().isoformat(),
        "version": __version__,
    }

    # Write output data
    df_to_save.to_parquet(
        path_output,
        engine=engine,
        compression="snappy",
        index=True,  # Preserve multi-index
    )
    list_path_save.append(path_output)

    if save_state:
        filename_state = (
            f"{site}_SUEWS_state_final{timestamp_suffix}.parquet"
            if site
            else f"SUEWS_state_final{timestamp_suffix}.parquet"
        )
        path_state = path_dir_save / filename_state
        df_state_final.to_parquet(
            path_state, engine=engine, compression="snappy", index=True
        )
        list_path_save.append(path_state)

    # Save metadata as a separate small parquet file
    filename_meta = (
        f"{site}_SUEWS_metadata{timestamp_suffix}.parquet"
        if site
        else f"SUEWS_metadata{timestamp_suffix}.parquet"
    )
    path_meta = path_dir_save / filename_meta
    df_meta = pd.DataFrame([metadata])
    df_meta.to_parquet(path_meta, engine=engine)
    list_path_save.append(path_meta)

    logger_supy.info(f"Saved Parquet output to {path_output}")
    return list_path_save
