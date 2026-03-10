import numpy as np
import pandas as pd
import copy
from ._env import logger_supy
from .data_model.output import OUTPUT_REGISTRY

##############################################################################
# post-processing part
# get variable information from Python registry (see #931)
def get_output_info_df():
    """Get output variable information as a DataFrame.

    Returns a DataFrame with MultiIndex (group, var) and columns:
    aggm, outlevel, func

    Returns
    -------
    pd.DataFrame
        Variable metadata indexed by (group, var)
    """
    return OUTPUT_REGISTRY.to_dataframe()


# get variable info as a DataFrame
# save `df_var` for later use
df_var = get_output_info_df()

# dict as df_var but keys in lowercase
dict_var_lower = {group.lower(): group for group in df_var.index.levels[0].str.strip()}

# dict of resampling rules from registry:
#  {group: {var: agg_method}}
dict_var_aggm = OUTPUT_REGISTRY.get_aggregation_rules()


# generate index for variables in different model groups
def gen_group_cols(group_x):
    # get correct group name by cleaning and swapping case
    group = group_x.replace("dataoutline", "").replace("line", "")
    # print group
    group = dict_var_lower[group]
    list_header_group = np.apply_along_axis(
        list, 0, df_var.loc[["datetime", group]].index.values
    )[:, 1]

    # generate MultiIndex if not `datetimeline`
    if not group_x == "datetimeline":
        idx_group = pd.MultiIndex.from_product(
            [[group], list_header_group], names=["group", "var"], sortorder=None
        )
    else:
        idx_group = list_header_group

    return idx_group


# merge_grid: useful for both `dict_output` and `dict_state`
def pack_df_grid(dict_output):
    # pack all grid and times into index/columns
    df_xx = pd.DataFrame.from_dict(dict_output, orient="index")
    # pack
    df_xx0 = df_xx.map(pd.Series)
    df_xx1 = df_xx0.map(pd.DataFrame.from_dict)
    df_xx2 = pd.concat({
        grid: pd.concat(df_xx1[grid].to_dict()).unstack().dropna(axis=1)
        for grid in df_xx1.columns
    })
    # drop redundant levels
    df_xx2.columns = df_xx2.columns.droplevel(0)
    # regroup by `grid`
    df_xx2.index.names = ["grid", "time"]
    gb_xx2 = df_xx2.groupby(level="grid")
    # merge results of each grid
    ar_xx3 = gb_xx2.agg(lambda x: tuple(x.values)).map(np.array)

    return ar_xx3


# generate MultiIndex for variable groups
def gen_index(varline_x):
    var_x = varline_x.replace("dataout", "").replace("block", "").replace("line", "")
    group = dict_var_lower[var_x]
    list_var = df_var.loc[group].index.tolist()
    idx_multi = pd.MultiIndex.from_product([[group], list_var], names=["group", "var"])
    return idx_multi


# generate one MultiIndex from a whole dict
def gen_MultiIndex(dict_x):
    list_keys = dict_x.keys()
    idx_multi = pd.concat([gen_index(k).to_frame() for k in list_keys]).index
    return idx_multi


# generate one Series from a dict entry
def gen_Series(dict_x, varline_x):
    idx_multi = gen_index(varline_x)
    ser_result = pd.Series(dict_x[varline_x], index=idx_multi)
    return ser_result


# merge a whole dict into one Series
def comb_gen_Series(dict_x):
    list_keys = dict_x.keys()
    ser_result = pd.concat([gen_Series(dict_x, k) for k in list_keys])
    return ser_result


# pack up output of `run_suews`
def pack_df_output_line(dict_output):
    import logging
    import pickle

    if logger_supy.isEnabledFor(logging.DEBUG):
        pickle.dump(dict_output, open("dict_output.pkl", "wb"))
        logger_supy.debug("dict_output saved to dict_output.pkl")
    # TODO: add output levels as in the Fortran version
    df_output = pd.DataFrame(dict_output).T
    # df_output = pd.concat(dict_output).to_frame().unstack()
    # set index level names
    idx_output = df_output.index.set_names(["datetime", "grid"])
    # clean columns
    cols_output = gen_MultiIndex(df_output.iloc[0])
    ar_values = np.apply_along_axis(np.hstack, 1, df_output.values)
    df_output = pd.DataFrame(ar_values, index=idx_output, columns=cols_output)
    return df_output


def pack_df_state(dict_state):
    df_state = pd.DataFrame(dict_state).T
    # df_state = pd.concat(dict_state).to_frame().unstack()
    # set index level names
    df_state.index = df_state.index.set_names(["datetime", "grid"])

    return df_state


def pack_df_output_array(dict_output_array, df_forcing):
    list_grid = list(dict_output_array.keys())
    grid_start = list_grid[0]
    cols_df = gen_MultiIndex(dict_output_array[grid_start])
    dict_df = {}
    for grid in list_grid:
        ar_grid = np.hstack([v[:, 5:] for v in dict_output_array[grid].values()])
        df_grid = pd.DataFrame(ar_grid, columns=cols_df, index=df_forcing.index)

        dict_df.update({grid: df_grid})

    # join results of all grids
    df_grid_res = pd.concat(dict_df, keys=dict_df.keys())

    # set index level names
    df_grid_res.index.set_names(["grid", "datetime"], inplace=True)

    return df_grid_res


def pack_df_output_block(dict_output_block, df_forcing_block):
    cols_df = gen_MultiIndex(dict_output_block)
    ar_val_df = np.hstack([ar[:, 5:] for ar in dict_output_block.values()])
    idx_df = df_forcing_block.index.rename("datetime")
    df_out = pd.DataFrame(ar_val_df, columns=cols_df, index=idx_df)

    return df_out


# resample supy output
def resample_output(df_output, freq="60min", dict_aggm=dict_var_aggm, _internal=False):
    """Resample SUEWS simulation output to a different temporal frequency.

    .. deprecated:: 2026.2
        Direct use of this function is deprecated. Use :meth:`SUEWSOutput.resample`
        instead for the recommended object-oriented interface::

            output = sim.run()
            resampled = output.resample(freq="h")

    This function resamples time series data using variable-appropriate
    aggregation methods. Different variable types are handled correctly:

    - **Instantaneous** (temperature, humidity, wind): averaged (mean)
    - **Accumulated** (rainfall, runoff): summed
    - **State** (soil moisture, daily state): last value

    Parameters
    ----------
    df_output : pandas.DataFrame or SUEWSOutput
        Output DataFrame from `run_supy`, with MultiIndex (grid, datetime)
        and MultiIndex columns (group, var). Also accepts SUEWSOutput objects.
    freq : str, optional
        Target frequency using pandas offset aliases.
        Common values: '30min', '60min' or 'h', '3h', 'D'.
        Default is '60min' (hourly).
    dict_aggm : dict, optional
        Custom aggregation rules. Default uses OUTPUT_REGISTRY rules.
        Format: {group: {variable: agg_function}}

    Returns
    -------
    pandas.DataFrame
        Resampled DataFrame with same structure as input.

    Notes
    -----
    **Recommended Usage**

    For new code, use the object-oriented interface::

        sim = SUEWSSimulation('config.yml')
        output = sim.run()
        resampled = output.resample(freq="h")  # Returns SUEWSOutput

    **Timestamp Convention**

    The SUEWS convention uses right-closed intervals with right labels,
    meaning timestamps represent the END of each period. For example,
    hourly data at 13:00 covers the period 12:00-13:00.

    **DailyState Labeling**

    When resampling to daily frequency, the DailyState group uses a different
    labeling convention (``label='left'``) compared to other groups
    (``label='right'``). This is intentional:

    - **SUEWS, snow, ESTM, etc.** (``label='right'``): Daily data is labeled
      with the END of each day. Data for January 1st is labeled "Jan 2"
      (the midnight timestamp at the end of Jan 1).

    - **DailyState** (``label='left'``): Daily data is labeled with the
      START of each day. Data for January 1st is labeled "Jan 1".
      This makes the output more intuitive - the row "Jan 1" contains
      the state at the end of January 1st.

    As a result, when resampling 7 full days of simulation data to daily
    frequency, the combined output may contain 8 unique dates:

    - SUEWS group: Jan 2 through Jan 8 (7 days)
    - DailyState group: Jan 1 through Jan 7 (7 days)
    - Combined: Jan 1 through Jan 8 (8 unique dates)

    The first date (Jan 1) will have NaN for SUEWS variables, and the last
    date (Jan 8) will have NaN for DailyState variables. This asymmetry
    reflects the different semantic meanings of each group's timestamps.

    Examples
    --------
    **Recommended** - Use the OOP interface:

    >>> sim = sp.SUEWSSimulation.from_sample_data()
    >>> output = sim.run()
    >>> resampled = output.resample(freq="h")  # Returns SUEWSOutput

    Legacy usage (deprecated):

    >>> df_state_init, df_forcing = sp.load_SampleData()
    >>> df_output, df_state_final = sp.run_supy(df_forcing, df_state_init)
    >>> df_hourly = sp.resample_output(df_output, freq="h")

    See Also
    --------
    SUEWSOutput.resample : Recommended OOP interface for resampling
    supy.util.gen_epw : Generate EPW files (supports freq parameter)
    supy.data_model.output.OUTPUT_REGISTRY : Aggregation rules source
    """
    # Unwrap SUEWSOutput to raw DataFrame if needed
    if hasattr(df_output, "_df_output"):
        df_output = df_output._df_output

    # Issue deprecation warning for direct external calls
    if not _internal:
        from ._supy_module import _warn_functional_deprecation

        _warn_functional_deprecation("resample_output")

    # Helper function to resample a group with specified parameters
    def _resample_group(df_group, freq, label, dict_aggm_group, group_name=None):
        """Resample a dataframe group with specified aggregation rules.

        Args:
            df_group: DataFrame group to resample
            freq: Resampling frequency
            label: Label parameter for resample ('left' or 'right')
            dict_aggm_group: Aggregation dictionary for this group
            group_name: Name of the group (used to apply dropna only to DailyState)

        Returns:
            Resampled DataFrame
        """
        # Only apply dropna to DailyState group
        # Other groups may have NaN values in some variables (e.g., Fcld)
        if group_name == "DailyState":
            # DailyState contains sparse data (values only at end of each day)
            # Use dropna(how='all') to preserve rows with partial data
            df_with_data = df_group.dropna(how="all")

            if df_with_data.empty:
                # No data at all - return empty DataFrame with proper structure
                return pd.DataFrame(
                    index=pd.DatetimeIndex([]), columns=df_group.columns
                )

            # Resample the non-empty data and return directly
            # Note: We don't reindex because Pandas 3.0's asfreq() behaviour changed
            # and can produce different boundary dates. The resampled data already
            # contains the correct timestamps.
            return df_with_data.resample(freq, closed="right", label=label).agg(
                dict_aggm_group
            )
        else:
            df_to_resample = df_group

        return df_to_resample.resample(freq, closed="right", label=label).agg(
            dict_aggm_group
        )

    # get grid and group names
    list_grid = df_output.index.get_level_values("grid").unique()
    list_group = df_output.columns.get_level_values("group").unique()

    # resampling output according to different rules defined in dict_aggm
    # note the setting in .resample: (closed='right',label='right')
    # which is to conform to SUEWS convention
    # that timestamp refer to the ending of previous period
    df_rsmp = pd.concat(
        {
            grid: pd.concat(
                {
                    group: _resample_group(
                        df_output.xs(grid, level="grid")[group],
                        freq,
                        "right"
                        if group != "DailyState"
                        else "left",  # DailyState uses 'left' label
                        dict_aggm[group],
                        group_name=group,
                    )
                    for group in list_group
                    if group in dict_aggm  # Only process groups that are in dict_aggm
                },
                axis=1,
                names=["group", "var"],
            )
            for grid in list_grid
        },
        names=["grid"],
    )

    # clean results
    df_rsmp = df_rsmp.dropna(how="all", axis=0)

    # Ensure the index has proper names (grid, datetime)
    # After concat, the datetime level may lose its name
    if isinstance(df_rsmp.index, pd.MultiIndex):
        if df_rsmp.index.names[1] is None:
            df_rsmp.index = df_rsmp.index.set_names(["grid", "datetime"])

    # Filter to actual data range to ensure consistent behaviour across Pandas versions
    # Pandas 3.0 may create extra boundary bins that fall outside the actual data range.
    # Use floor/ceil of the data range to allow for label='left' convention (DailyState)
    # where the label can be up to one period before the first data point.
    dt_idx = df_output.index.get_level_values("datetime")
    dt_min, dt_max = dt_idx.min(), dt_idx.max()
    # Extend dt_min by one period to allow for 'left' labels
    dt_min_floor = dt_min.floor(freq)
    rsmp_dt_idx = df_rsmp.index.get_level_values("datetime")
    mask = (rsmp_dt_idx >= dt_min_floor) & (rsmp_dt_idx <= dt_max)
    df_rsmp = df_rsmp[mask]

    return df_rsmp


# RSL post-processing
# -------------------


def proc_df_rsl(df_output, debug=False):
    """
    Process Roughness Sublayer (RSL) model output data.

    This function extracts and reshapes RSL data from the model output
    for easier analysis and visualization of vertical profile data.

    Parameters
    ----------
    df_output : pandas.DataFrame
        Either the complete model output containing RSL data or
        a DataFrame with just the RSL data.
    debug : bool, optional
        If True, additional debug variables are extracted.
        Default is False.

    Returns
    -------
    pandas.DataFrame or tuple
        If debug=False, returns a DataFrame with RSL variables stacked by level.
        If debug=True, returns a tuple (DataFrame, DataFrame) with the first containing
        the stacked RSL variables and the second containing debug variables.

    Notes
    -----
    The returned DataFrame has a MultiIndex with levels for variables and vertical levels,
    making it easier to plot vertical profiles of RSL variables.

    Examples
    --------
    >>> # Basic processing
    >>> df_rsl = proc_df_rsl(df_output)
    >>> # Plot vertical profiles
    >>> df_rsl.xs("u", level="var").T.plot(legend=True)
    >>>
    >>> # With debug information
    >>> df_rsl, df_debug_vars = proc_df_rsl(df_output, debug=True)
    """
    try:
        # If we work on the whole output with multi-index columns
        df_rsl_raw = df_output["RSL"].copy()
    except KeyError:
        # If we directly work on the RSL output
        df_rsl_raw = df_output.copy()

    try:
        # Drop unnecessary timestamp columns if existing
        df_rsl_data = df_rsl_raw.drop(["Year", "DOY", "Hour", "Min", "Dectime"], axis=1)
    except KeyError:
        df_rsl_data = df_rsl_raw

    # Extract the first 120 columns (30 levels × 4 variables)
    # These contain the main RSL profile data (u, tke, theta, q)
    df_rsl = df_rsl_data.iloc[:, : 30 * 4]

    # Convert column names from format "var_level" to MultiIndex (var, level)
    df_rsl.columns = (
        df_rsl.columns.str.split("_")
        .map(lambda l: tuple([l[0], int(l[1])]))
        .rename(["var", "level"])
    )

    # Stack the data to get a hierarchical representation by variable and level
    df_rsl_proc = df_rsl.stack()

    if debug:
        # Extract debug variables (columns after the 120th column)
        df_rsl_debug = df_rsl_data.iloc[:, 120:]
        return df_rsl_proc, df_rsl_debug
    else:
        return df_rsl_proc


def is_numeric(obj):
    """
    Check if an object is numeric (integer, float, complex, or numeric numpy array).

    Parameters
    ----------
    obj : object
        The object to be checked.

    Returns
    -------
    bool
        True if the object is numeric, False otherwise.

    Notes
    -----
    Used by the debug data processing functions to determine how to handle values.
    """
    if isinstance(obj, (int, float, complex)):
        return True
    if isinstance(obj, np.ndarray):
        return np.issubdtype(obj.dtype, np.number)
    return False


