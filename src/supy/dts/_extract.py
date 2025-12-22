"""Output extraction functions for DTS simulation results.

This module provides functions to extract output from SUEWS DTS objects
and convert to DataFrame format matching the current run_supy() output.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from .._post import gen_index
from ..supy_driver import module_ctrl_type as dts

_OUTPUT_LINE_FIELDS = (
    "dataoutlinesuews",
    "datetimeline",
    "dataoutlinesnow",
    "dataoutlinedailystate",
    "dataoutlinersl",
    "dataoutlinebeers",
    "dataoutlinedebug",
    "dataoutlineestm",
    "dataoutlineehc",
    "dataoutlinespartacus",
    "dataoutlinestebbs",
    "dataoutlinenhood",
)

_GROUP_TO_KEY = {
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


def _attach_grid_index(
    df: pd.DataFrame,
    grid_id: int,
    datetime_index: pd.DatetimeIndex,
) -> pd.DataFrame:
    df.index = pd.MultiIndex.from_product(
        [[grid_id], datetime_index], names=["grid", "datetime"]
    )
    return df


def extract_output_line_to_dict(output_line: dts.output_line) -> dict[str, np.ndarray]:
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
    return {
        key: np.array(getattr(output_line, key)).copy()
        for key in _OUTPUT_LINE_FIELDS
    }


def build_output_dataframe_from_block(
    dataoutblock: np.ndarray,
    datetime_index: pd.DatetimeIndex,
    grid_id: int = 1,
) -> pd.DataFrame:
    """Build output DataFrame from batch output block array.

    This function is used with suews_cal_multitsteps_dts batch execution.

    Parameters
    ----------
    dataoutblock : np.ndarray
        Output array of shape (len_sim, ncolumnsdataoutsuews) from batch execution.
        First 5 columns are datetime (skipped), remaining are output variables.
    datetime_index : pd.DatetimeIndex
        Datetime index for the output.
    grid_id : int
        Grid identifier.

    Returns
    -------
    pd.DataFrame
        Output DataFrame with MultiIndex columns (group, var).
    """
    # Skip first 5 datetime columns
    arr_suews = dataoutblock[:, 5:]

    # Get column index for SUEWS group
    idx_suews = gen_index("dataoutlinesuews")

    df_output = pd.DataFrame(arr_suews, columns=idx_suews, index=datetime_index)
    return _attach_grid_index(df_output, grid_id, datetime_index)


def build_full_output_dataframe(
    list_output_dicts: list[dict[str, np.ndarray]],
    datetime_index: pd.DatetimeIndex,
    grid_id: int = 1,
    include_groups: list[str] | None = None,
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

    dfs = []
    for group in include_groups:
        key = _GROUP_TO_KEY.get(group)
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

    return _attach_grid_index(df_output, grid_id, datetime_index)
