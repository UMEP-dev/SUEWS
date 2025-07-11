import numpy as np
import pandas as pd
import copy
from .supy_driver import suews_driver as sd
from .supy_driver import suews_def_dts as sd_dts


##############################################################################
# post-processing part
# get variable information from Fortran
def get_output_info_df():
    from packaging.version import parse as LooseVersion

    size_var_list = sd.output_size()
    list_var_x = [np.array(sd.output_name_n(i)) for i in np.arange(size_var_list) + 1]

    df_var_list = pd.DataFrame(list_var_x, columns=["var", "group", "aggm", "outlevel"])

    # strip leading and trailing spaces
    fun_strip = lambda x: x.decode().strip()
    if LooseVersion(pd.__version__) >= LooseVersion("2.1.0"):
        # if pandas version is 2.1.0 or above, we can use `df.map`
        df_var_list = df_var_list.map(fun_strip)
    else:
        # otherwise, we need to use `df.applymap`
        df_var_list = df_var_list.applymap(fun_strip)

    df_var_list_x = df_var_list.replace(r"^\s*$", np.nan, regex=True).dropna()
    df_var_dfm = df_var_list_x.set_index(["group", "var"])
    return df_var_dfm


# get variable info as a DataFrame
# save `df_var` for later use
df_var = get_output_info_df()

# dict as df_var but keys in lowercase
dict_var_lower = {group.lower(): group for group in df_var.index.levels[0].str.strip()}

#  generate dict of functions to apply for each variable
# Use lambda for 'last' to avoid pandas compatibility issues
dict_func_aggm = {
    "T": lambda x: x.iloc[-1] if len(x) > 0 else np.nan,  # last value
    "A": "mean",
    "S": "sum",
    "L": lambda x: x.iloc[-1] if len(x) > 0 else np.nan,  # last value
}
df_var["func"] = df_var.aggm.apply(lambda x: dict_func_aggm[x])

# dict of resampling ruls:
#  {group: {var: agg_method}}
dict_var_aggm = {
    group: df_var.loc[group, "func"].to_dict() for group in df_var.index.levels[0]
}


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
    import pickle

    pickle.dump(dict_output, open("dict_output.pkl", "wb"))
    print("dict_output saved to dict_output.pkl")
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
def resample_output(df_output, freq="60T", dict_aggm=dict_var_aggm):
    # Helper function to resample a group with specified parameters
    def _resample_group(df_group, freq, label, dict_aggm_group):
        """Resample a dataframe group with specified aggregation rules.
        
        Args:
            df_group: DataFrame group to resample
            freq: Resampling frequency
            label: Label parameter for resample ('left' or 'right')
            dict_aggm_group: Aggregation dictionary for this group
        
        Returns:
            Resampled DataFrame
        """
        return df_group.dropna().resample(
            freq, 
            closed="right", 
            label=label
        ).agg(dict_aggm_group)
    
    # get grid and group names
    list_grid = df_output.index.get_level_values("grid").unique()
    list_group = df_output.columns.get_level_values("group").unique()

    # Skip DailyState if it somehow gets here (it should be handled separately)
    list_group = [g for g in list_group if g != 'DailyState']

    # resampling output according to different rules defined in dict_aggm
    # note the setting in .resample: (closed='right',label='right')
    # which is to conform to SUEWS convention
    # that timestamp refer to the ending of previous period
    df_rsmp = pd.concat(
        {
            grid: pd.concat(
                {
                    group: _resample_group(
                        df_output.loc[grid, group],
                        freq,
                        "right",  # Regular variables use 'right' label
                        dict_aggm[group]
                    )
                    for group in list_group
                },
                axis=1,
                names=["group", "var"],
            )
            for grid in list_grid
        },
        names=["grid"],
    )

    # Handle DailyState separately if present
    if "DailyState" in df_output and "DailyState" in dict_aggm:
        # DailyState uses label="left" as it represents state at the beginning of the period
        # whereas other variables use label="right" following SUEWS convention for period-ending values
        df_dailystate_rsmp = pd.concat(
            {
                grid: pd.concat(
                    {
                        "DailyState": _resample_group(
                            df_output.loc[grid, "DailyState"],
                            freq,
                            "left",  # DailyState uses 'left' label
                            dict_aggm["DailyState"]
                        )
                    },
                    axis=1,
                    names=["group", "var"],
                )
                for grid in list_grid
            },
            names=["grid"],
        )

        df_rsmp = pd.concat([df_rsmp, df_dailystate_rsmp], axis=1)

    # clean results
    df_rsmp = df_rsmp.dropna(how="all", axis=0)
    return df_rsmp


# Debug-related processing functions
# ---------------------------------
# Typical debug workflow:
# 1. Run simulation with debug_mode=True:
#    df_output, df_state_final, df_debug, res_state = run_supy(
#        df_forcing, df_state_init, debug_mode=True)
#
# 2. Basic analysis of debug data:
#    # View main debug data structure
#    df_debug.columns.levels  # Check available variable groups
#    df_debug[('energy_balance', 'qh')].plot()  # Plot heat flux from debug data
#
# 3. Advanced analysis with RSL data (if present):
#    # Process RSL-specific outputs
#    df_rsl_proc = proc_df_rsl(df_output, debug=True)
#
# 4. Direct inspection of raw state objects (for advanced users):
#    # Convert state block to dict using selective extraction
#    dict_energy_vars = pack_dts_selective(res_state, {'energy_balance': ['qn', 'qh', 'qe']})


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
    except:
        # If we directly work on the RSL output
        df_rsl_raw = df_output.copy()

    try:
        # Drop unnecessary timestamp columns if existing
        df_rsl_data = df_rsl_raw.drop(["Year", "DOY", "Hour", "Min", "Dectime"], axis=1)
    except:
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


def inspect_dts_structure(dts_obj):
    """
    Inspect derived type structure (DTS) object and extract its property hierarchy.

    This function maps the structure of Fortran-derived type objects for efficient access
    later without repeatedly traversing the object hierarchy.

    Parameters
    ----------
    dts_obj : object
        A Fortran derived type object, typically from SUEWS kernel debug output.

    Returns
    -------
    dict
        Dictionary mapping attribute names to their sub-properties.
        None values indicate leaf attributes (no nested structure).

    Notes
    -----
    This is a preprocessing step for `fast_pack_dts()` to improve performance
    when processing multiple debug objects with the same structure.
    """
    dict_props = {}
    for attr in dir(dts_obj):
        if not attr.startswith("_") and not callable(getattr(dts_obj, attr)):
            val = getattr(dts_obj, attr)
            # If it's a nested object, get its properties too
            if hasattr(val, "__dict__"):
                list_sub_props = [
                    sub_attr
                    for sub_attr in dir(val)
                    if not sub_attr.startswith("_")
                    and not callable(getattr(val, sub_attr))
                ]
                dict_props[attr] = list_sub_props
            else:
                dict_props[attr] = None
    return dict_props


def fast_pack_dts(dts_obj, dict_structure=None):
    """
    Convert a derived type object to a Python dictionary efficiently.

    Uses a pre-computed structure map (if provided) to avoid repeated inspection
    of the object hierarchy, making it faster for batch processing.

    Parameters
    ----------
    dts_obj : object
        A Fortran derived type object from SUEWS kernel debug output.
    dict_structure : dict, optional
        Pre-computed structure map from `inspect_dts_structure()`.
        If None, the structure will be inspected on the fly.

    Returns
    -------
    dict
        Nested dictionary representation of the derived type object.

    Examples
    --------
    >>> # For a single object
    >>> dict_debug = fast_pack_dts(state_debug)
    >>>
    >>> # For efficient processing of multiple objects
    >>> dict_structure = inspect_dts_structure(state_debug)
    >>> list_debug_dicts = [
    ...     fast_pack_dts(obj, dict_structure) for obj in debug_objects
    ... ]
    """
    if dict_structure is None:
        dict_structure = inspect_dts_structure(dts_obj)

    dict_result = {}
    for attr, list_sub_props in dict_structure.items():
        val = getattr(dts_obj, attr)
        if list_sub_props:
            # Nested object
            dict_result[attr] = {
                sub_prop: getattr(val, sub_prop) for sub_prop in list_sub_props
            }
        else:
            # Direct value
            dict_result[attr] = val

    return dict_result


def pack_dts_batch(list_dts_objs, dict_structure):
    """
    Process multiple derived type objects at once using a shared structure map.

    This is more efficient than calling fast_pack_dts() on each object separately
    when processing a large number of similar debug objects.

    Parameters
    ----------
    list_dts_objs : list
        List of Fortran derived type objects to process.
    dict_structure : dict
        Pre-computed structure map from `inspect_dts_structure()`.

    Returns
    -------
    list
        List of dictionaries, each representing a derived type object.

    Examples
    --------
    >>> # Efficiently process multiple debug objects
    >>> dict_structure = inspect_dts_structure(debug_objects[0])
    >>> list_debug_data = pack_dts_batch(debug_objects, dict_structure)
    """
    return [fast_pack_dts(obj, dict_structure) for obj in list_dts_objs]


def pack_dts2dict_selective(dts_obj, dict_needed_vars):
    """
    Selectively extract specific variables from a derived type object.

    Useful when only a subset of debug information is needed, reducing
    memory usage and processing time.

    Parameters
    ----------
    dts_obj : object
        Fortran derived type object from SUEWS kernel.
    dict_needed_vars : dict
        Dictionary mapping state names to lists of variable names to extract.
        If a list is empty or None, all variables for that state are extracted.

    Returns
    -------
    dict
        Nested dictionary containing only the requested variables.

    Examples
    --------
    >>> # Extract only specific variables of interest
    >>> dict_needed = {"energy_balance": ["qn", "qh", "qe"], "surface": None}
    >>> dict_subset_data = pack_dts_selective(debug_obj, dict_needed)
    """
    dict_dts = {}
    get = getattr

    for state, list_vars in dict_needed_vars.items():
        try:
            state_obj = get(dts_obj, state)
            if list_vars:  # If specific variables are requested
                dict_dts[state] = {var: get(state_obj, var) for var in list_vars}
            else:  # If None, get all non-private attributes
                dict_dts[state] = {
                    attr: get(state_obj, attr)
                    for attr in dir(state_obj)
                    if not attr.startswith("_") and not callable(get(state_obj, attr))
                }
        except AttributeError:
            # Skip if the state doesn't exist in this object
            continue

    return dict_dts


def pack_dts(dts):
    """
    Fast conversion of a derived type object to a nested dictionary.

    This is the main function used by the SuPy framework to convert debug objects
    returned from the SUEWS kernel into Python dictionaries.

    Parameters
    ----------
    dts : object
        Fortran derived type object from SUEWS kernel (e.g., state_debug).

    Returns
    -------
    dict
        Nested dictionary representation of the object's structure and values.

    Notes
    -----
    This function is used in the debug mode workflow to process state_debug and
    block_mod_state objects in suews_cal_tstep_multi().
    """
    dict_dts = {}
    for attr in dir(dts):
        # Skip magic methods with one check
        if not attr.startswith("_"):
            val = getattr(dts, attr)
            # Most values should be numeric, so check that first
            if isinstance(val, (int, float, bool, np.ndarray)):
                dict_dts[attr] = val
            # Only recurse if not numeric and not callable
            elif not callable(val):
                dict_dts[attr] = pack_dts(val)
    return dict_dts


def has_dict(dict_d):
    """
    Check if a dictionary contains any nested dictionaries.

    Used by pack_df_dts_raw to determine how to handle dictionary values.

    Parameters
    ----------
    dict_d : dict
        Dictionary to check.

    Returns
    -------
    bool
        True if any value in the dictionary is itself a dictionary.
    """
    return any(isinstance(v, dict) for v in dict_d.values())


def pack_dict_dts(dict_dts):
    """
    Convert a nested dictionary of debug information into a pandas DataFrame.

    This is an intermediate step in the process of converting debug data
    to a user-friendly DataFrame format.

    Parameters
    ----------
    dict_dts : dict
        Nested dictionary from pack_dict_dts() containing debug information.

    Returns
    -------
    pandas.DataFrame
        DataFrame with a MultiIndex structure preserving the nested hierarchy.

    Notes
    -----
    This function handles the recursive flattening of potentially deeply nested
    dictionaries into a single DataFrame with a MultiIndex.
    """
    dict_df_dts = {}
    for k, v in dict_dts.items():
        if has_dict(v):
            # Recursively process nested dictionaries
            dict_df_dts[k] = pack_dict_dts(v)
        else:
            # Convert leaf nodes to Series
            dict_df_dts[k] = pd.Series(v)

    # Concatenate all series/dataframes into a single dataframe
    df_dts = pd.concat(dict_df_dts, axis=0)
    return df_dts


sample_dts = sd_dts.SUEWS_STATE_BLOCK()
sample_dts.init(3, 3, 3)
dict_structure = inspect_dts_structure(sample_dts.block[0])


def pack_dict_dts_datetime_grid(dict_dts_datetime_grid):
    """
    Convert dictionary of debug information into a clean, indexed DataFrame.

    This is the final step in processing debug information, producing a DataFrame
    that can be easily analyzed and visualized.

    Parameters
    ----------
    dict_dts_datetime_grid : dict
        Dictionary containing DTS-derived information, typically from dict_debug in
        the SUEWS simulation output.

    Returns
    -------
    pandas.DataFrame
        Clean DataFrame with MultiIndex columns organized by group and variable.

    Notes
    -----
    Usage workflow in debug mode:
    1. Run simulation with debug_mode=True
    2. Receive dict_debug from simulation output
    3. Convert to DataFrame using: df_debug = pack_df_dts(dict_debug)
    4. Analyze specific variables: df_debug.loc[:, ('energy_balance', 'qh')]

    The resulting DataFrame has a datetime index and MultiIndex columns with
    levels for variable groups and specific variables.
    """
    # First convert to raw dataframe with nested index
    df_dts_raw = pack_dict_dts(dict_dts_datetime_grid)

    # Rename index levels for clarity
    df_dts_raw.index = df_dts_raw.index.rename([
        "datetime",
        "grid",
        "step",
        "group",
        "var",
    ])

    # Restructure to have a more user-friendly format
    df_dts = (
        df_dts_raw.unstack(level=["group", "var"])
        .sort_index(level=0, axis=1)
        .dropna(axis=1, how="all")
    )

    return df_dts


def pack_dts_state_selective(
    dict_dts_state,
    df_output,
    dict_vars_sel=dict_structure,
):
    """
    Selectively extract and pack specified variables from a debug state dictionary into a DataFrame.

    Parameters
    ----------
    dict_dts_state : dict
        Dictionary containing debug state information (typically from res_state)
    df_output : pandas.DataFrame
        DataFrame containing simulation output, used to get datetime index
    dict_vars_sel : dict
        Dictionary mapping state categories to lists of specific variables to extract.
        Format: {'state_category': ['var1', 'var2', ...], ...}
        Example: {'heatState': ['qn', 'qh', 'qe'], 'atmState': ['RH2']}
        If a value is None or empty list, all variables in that state will be extracted.

    Returns
    -------
    pandas.DataFrame
        DataFrame containing only the requested variables, with MultiIndex columns organized
        by state category and variable name, and datetime index.
    """
    dict_result = {}

    # retrieve datetime from df_output
    list_datetime = df_output.index.get_level_values("datetime")

    # Process each state block
    for grid_id, state_block in dict_dts_state.items():
        dict_grid_results = {}

        # Process each timestep in the state block
        for time_idx, dts_obj in enumerate(state_block.block):
            dict_timestep_data = {}

            # Extract only the requested variables for each state category
            for state_category, list_var in dict_vars_sel.items():
                if not hasattr(dts_obj, state_category):
                    continue

                state_obj = getattr(dts_obj, state_category)

                # If list_var is None or empty, get all non-private attributes
                if not list_var:
                    list_var = [
                        attr
                        for attr in dir(state_obj)
                        if not attr.startswith("_")
                        and not callable(getattr(state_obj, attr))
                    ]

                # Extract only specified variables from the state category
                for var in list_var:
                    if hasattr(state_obj, var):
                        value = getattr(state_obj, var)
                        # Handle arrays and scalar values
                        if isinstance(value, np.ndarray):
                            dict_timestep_data[(state_category, var)] = value
                        else:
                            dict_timestep_data[(state_category, var)] = value

            dict_grid_results[time_idx] = dict_timestep_data

        dict_result[grid_id] = dict_grid_results

    # Convert the nested dictionary structure to a MultiIndex DataFrame
    list_dfs = []
    for grid_id, dict_grid_data in dict_result.items():
        # Create a list of dictionaries for each timestep
        list_rows = []
        for time_idx, dict_time_data in dict_grid_data.items():
            list_rows.append(dict_time_data)

        if list_rows:
            # Create DataFrame with MultiIndex columns
            df_grid = pd.DataFrame(list_rows)

            # Use datetime index if available
            if list_datetime is not None and len(list_datetime) == len(list_rows):
                df_grid.index = list_datetime
                df_grid.index.name = "datetime"
            else:
                df_grid.index = pd.RangeIndex(len(list_rows), name="timestep")

            if not df_grid.empty:
                df_grid.columns = pd.MultiIndex.from_tuples(
                    df_grid.columns, names=["state", "variable"]
                )
                # Add grid information
                df_grid = df_grid.assign(grid=grid_id)
                df_grid.set_index("grid", append=True, inplace=True)

                # Reorder index levels to put datetime first if present
                if "datetime" in df_grid.index.names:
                    df_grid = df_grid.reorder_levels(["datetime", "grid"])

                list_dfs.append(df_grid)

    if not list_dfs:
        return pd.DataFrame()

    # Combine all grid DataFrames and ensure proper index ordering
    df_combined = pd.concat(list_dfs).sort_index().swaplevel(0, 1, axis=0)

    return df_combined
