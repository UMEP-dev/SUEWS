"""Output extraction functions for DTS simulation results.

This module provides functions to extract output from SUEWS DTS objects
and convert to DataFrame format matching the current run_supy() output.
"""

from typing import Dict, List, Any, Tuple

import numpy as np
import pandas as pd

from ..supy_driver import module_ctrl_type as dts
from .._post import df_var, gen_index


def extract_output_line_to_dict(output_line: dts.output_line) -> Dict[str, np.ndarray]:
    """Extract output_line arrays to dictionary.

    Parameters
    ----------
    output_line : dts.output_line
        Output line object from suews_cal_main.

    Returns
    -------
    dict
        Dictionary with output array names as keys.
    """
    result = {}

    # Main SUEWS outputs - skip first 5 datetime columns
    result["dataoutlinesuews"] = np.array(output_line.dataoutlinesuews).copy()

    # Datetime info
    result["datetimeline"] = np.array(output_line.datetimeline).copy()

    # Optional output groups
    result["dataoutlinesnow"] = np.array(output_line.dataoutlinesnow).copy()
    result["dataoutlinedailystate"] = np.array(output_line.dataoutlinedailystate).copy()
    result["dataoutlinersl"] = np.array(output_line.dataoutlinersl).copy()
    result["dataoutlinebeers"] = np.array(output_line.dataoutlinebeers).copy()
    result["dataoutlinedebug"] = np.array(output_line.dataoutlinedebug).copy()
    result["dataoutlineestm"] = np.array(output_line.dataoutlineestm).copy()
    result["dataoutlineehc"] = np.array(output_line.dataoutlineehc).copy()
    result["dataoutlinespartacus"] = np.array(output_line.dataoutlinespartacus).copy()
    result["dataoutlinestebbs"] = np.array(output_line.dataoutlinestebbs).copy()
    result["dataoutlinenhood"] = np.array(output_line.dataoutlinenhood).copy()

    return result


def build_output_dataframe(
    list_output_dicts: List[Dict[str, np.ndarray]],
    datetime_index: pd.DatetimeIndex,
    grid_id: int = 1,
) -> pd.DataFrame:
    """Build output DataFrame from list of output dictionaries.

    Parameters
    ----------
    list_output_dicts : list of dict
        List of output dictionaries, one per timestep.
    datetime_index : pd.DatetimeIndex
        Datetime index for the output.
    grid_id : int
        Grid identifier.

    Returns
    -------
    pd.DataFrame
        Output DataFrame with MultiIndex columns (group, var).
    """
    # Stack SUEWS outputs - skip first 5 datetime columns
    arr_suews = np.vstack([d["dataoutlinesuews"][5:] for d in list_output_dicts])

    # Get column index for SUEWS group
    idx_suews = gen_index("dataoutlinesuews")

    # Create DataFrame
    df_output = pd.DataFrame(arr_suews, columns=idx_suews, index=datetime_index)

    # Add grid to index
    df_output.index = pd.MultiIndex.from_product(
        [[grid_id], datetime_index], names=["grid", "datetime"]
    )

    return df_output


def build_full_output_dataframe(
    list_output_dicts: List[Dict[str, np.ndarray]],
    datetime_index: pd.DatetimeIndex,
    grid_id: int = 1,
    include_groups: List[str] = None,
) -> pd.DataFrame:
    """Build complete output DataFrame including all output groups.

    Parameters
    ----------
    list_output_dicts : list of dict
        List of output dictionaries, one per timestep.
    datetime_index : pd.DatetimeIndex
        Datetime index for the output.
    grid_id : int
        Grid identifier.
    include_groups : list of str, optional
        List of groups to include. If None, includes SUEWS only.

    Returns
    -------
    pd.DataFrame
        Output DataFrame with MultiIndex columns (group, var).
    """
    if include_groups is None:
        include_groups = ["SUEWS"]

    # Map group names to dataoutline keys
    group_to_key = {
        "SUEWS": "dataoutlinesuews",
        "snow": "dataoutlinesnow",
        "DailyState": "dataoutlinedailystate",
        "RSL": "dataoutlinersl",
        "BEERS": "dataoutlinebeers",
        "debug": "dataoutlinedebug",
        "ESTM": "dataoutlineestm",
        "EHC": "dataoutlineehc",
        "SPARTACUS": "dataoutlinespartacus",
        "STEBBS": "dataoutlinestebbs",
        "Nhood": "dataoutlinenhood",
    }

    dfs = []
    for group in include_groups:
        key = group_to_key.get(group)
        if key is None:
            continue

        # Stack arrays - skip first 5 datetime columns
        try:
            arr = np.vstack([d[key][5:] for d in list_output_dicts])
        except (KeyError, IndexError):
            continue

        # Get column index
        idx = gen_index(key)

        # Create DataFrame
        df_group = pd.DataFrame(arr, columns=idx, index=datetime_index)
        dfs.append(df_group)

    if not dfs:
        return pd.DataFrame()

    # Concatenate all groups
    df_output = pd.concat(dfs, axis=1)

    # Add grid to index
    df_output.index = pd.MultiIndex.from_product(
        [[grid_id], datetime_index], names=["grid", "datetime"]
    )

    return df_output


def extract_datetime_from_output(output_dict: Dict[str, np.ndarray]) -> Tuple[int, int, int, int, int]:
    """Extract datetime components from output dictionary.

    Parameters
    ----------
    output_dict : dict
        Output dictionary with datetimeline.

    Returns
    -------
    tuple
        (year, day_of_year, hour, minute, second)
    """
    dt_line = output_dict["datetimeline"]
    # datetimeline format: [year, doy, hour, min, sec]
    return int(dt_line[0]), int(dt_line[1]), int(dt_line[2]), int(dt_line[3]), int(dt_line[4])
