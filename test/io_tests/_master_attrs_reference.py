"""Test-only verbatim master (141f0c347b) copies of attrs-carrying functions.

Extracted from ``git show 141f0c347b:<path>`` with only imports adjusted, for
differential testing against the attrs-stripped implementations in
``supy._output_metadata``, ``supy._post`` and ``supy._save``.
"""

from collections.abc import Mapping
from copy import deepcopy
from typing import Optional

import pandas as pd

from supy import units as unit_conversion
from supy._output_metadata import (
    daily_water_variables,
    output_covered_seconds,
    output_interval_seconds,
    project_output_metadata,
    resolve_held_interval,
    water_flux_variables,
)
from supy._post import (
    _aggregate_bins,
    _calendar_binning,
    _dailystate_period_end,
    _index_freq_matches,
    _is_calendar_frequency,
    dict_var_aggm,
)
from supy._save import (
    StreamedTextFiles,
    _dailystate_rows,
    _gen_df_year,
    _grid_asfreq_index,
    _normalise_output_timestamp_reference,
    _rows_with_data,
    save_df_grid_group,
)


def with_daily_coverage(
    df_output: pd.DataFrame, interval_s: Optional[float] = None,
    *, daily_steps: Optional[pd.Series] = None,
) -> pd.DataFrame:
    """Attach daily coverage before native rows are selected or resampled.

    DailyState accumulates steps stamped within each calendar day. Count those
    steps using the interval the rows hold (see :func:`resolve_held_interval`:
    a declared interval, or the regular cadence pandas finds in the index),
    never a cadence reconstructed from gaps. Streaming callers supply
    daily_steps from the complete forcing slice so a midnight step in the
    preceding chunk still counts towards its day. Without a held interval,
    mark coverage as unknown (None) so later calls with an output interval
    cannot fabricate it; amount conversion remains available.
    """
    key = "supy_output_daily_coverage_s"
    if key in df_output.attrs or not isinstance(df_output.columns, pd.MultiIndex):
        return df_output
    if "DailyState" not in df_output.columns.get_level_values(0):
        return df_output
    step_s = output_interval_seconds(df_output, interval_s, required=False)
    if step_s is None:
        result = df_output.copy(deep=False)
        result.attrs[key] = None
        return result
    dates = df_output.index.get_level_values("datetime")
    grids = df_output.index.get_level_values("grid")
    counts = pd.Series(
        1, index=pd.MultiIndex.from_arrays([grids, dates.normalize()])
    ).groupby(level=[0, 1]).sum()
    daily = df_output["DailyState"].notna().any(axis=1)
    coverage = {}
    for grid, date in df_output.index[daily]:
        count = counts.loc[grid, date.normalize()] if daily_steps is None else daily_steps[date.normalize()]
        coverage[f"{grid}|{date.isoformat()}"] = float(count * step_s)
    result = df_output.copy(deep=False)
    result.attrs[key] = coverage
    return result


def prepare_output(
    df_output: pd.DataFrame, units: Optional[Mapping] = None, group: Optional[str] = None,
    *, interval_s: Optional[int] = None, write: bool = False,
) -> pd.DataFrame:
    """Return output in requested units, leaving the input and kernel untouched.

    Parameters
    ----------
    df_output : pandas.DataFrame
        Output columns, normally identified by (group, variable).
    units : mapping, optional
        Variable names or (group, variable) pairs mapped to target unit strings.
        Ambiguous names require a pair. Without writing or a request, return the original frame.
    group : str, optional
        Group identity for a single-group frame with flat columns.

    interval_s : int, optional
        Duration represented by a row; required when timestamps do not give it.
    write : bool, optional
        Convert all water amounts to canonical rates at the writing boundary.

    Returns
    -------
    pandas.DataFrame
        Converted columns and their units in attrs, or the unchanged input.

    Raises
    ------
    ValueError
        If a variable is missing, ambiguous, unitless or dimensionally incompatible.
    """
    if units is None and not write:
        return df_output
    if units is not None and not isinstance(units, Mapping):
        raise TypeError("units must be a mapping from output variables to unit strings")
    if not units and not write:
        return df_output
    df_output = with_daily_coverage(df_output, interval_s)
    dict_metadata = project_output_metadata(
        df_output.columns, None, group, df_output.attrs.get("supy_output_units"), native=True
    )
    dict_water = water_flux_variables()
    dict_targets = {}
    if write:
        for column in df_output.columns:
            identity = column if isinstance(column, tuple) else (group, column)
            if identity in dict_water:
                dict_targets[column] = df_output.attrs.get("supy_output_units", {}).get(identity[0], {}).get(identity[1], "kg m-2 s-1")
    declared = set()
    for key, target in (units or {}).items():
        matches = [
            column for column in df_output.columns
            if column == key or (isinstance(column, tuple) and column[1] == key)
        ]
        if len(matches) != 1:
            reason = "ambiguous; use (group, variable)" if matches else "not present"
            raise ValueError(f"Output variable {key!r} is {reason}")
        column = matches[0]
        if column in declared:
            raise ValueError(f"Multiple unit requests for output variable {column!r}")
        declared.add(column)
        dict_targets[column] = target
    if not dict_targets:
        return df_output
    df_result = df_output.copy(deep=False)
    dict_units = deepcopy(df_output.attrs.get("supy_output_units", {}))
    list_water_columns = [
        column for column in dict_targets
        if (column if isinstance(column, tuple) else (group, column)) in dict_water
    ]
    if list_water_columns:
        output_interval_seconds(df_output, interval_s, required=False)
    ser_coverage = None
    if any(dict_metadata[column]["units"] == "mm" for column in list_water_columns):
        nominal_s = output_interval_seconds(
            df_output, interval_s,
            required="supy_output_coverage_s" not in df_output.attrs,
        )
        ser_coverage = output_covered_seconds(df_output, nominal_s)
        df_result.attrs["supy_output_interval_s"] = nominal_s
    for column, target in dict_targets.items():
        metadata = dict_metadata[column]
        try:
            parsed = unit_conversion.parse_unit(target)
            if not isinstance(target, str):
                raise unit_conversion.UnitError("target unit must be a unit string")
            identity = column if isinstance(column, tuple) else (group, column)
            if identity in dict_water:
                source = metadata["units"]
                values = df_output[column]
                if source == "mm":
                    values = values / ser_coverage
                    source = "kg m-2 s-1"
                values = unit_conversion.convert_water_rate(values, source, parsed)
            elif identity in daily_water_variables():
                values = unit_conversion.convert_water_amount(df_output[column], metadata["units"], parsed)
            else:
                values = unit_conversion.convert(
                    df_output[column], metadata.get("units"), parsed,
                    units_metadata=metadata.get("units_metadata"),
                )
        except unit_conversion.UnitError as exc:
            raise unit_conversion.UnitError(f"Output variable {column!r}: {exc}") from exc
        df_result[column] = values
        identity = column if isinstance(column, tuple) else (group, column)
        dict_units.setdefault(identity[0], {})[identity[1]] = target
    df_result.attrs["supy_water_rate_columns"] = [
        identity for identity in dict_water
        if identity[1] in dict_units.get(identity[0], {})
    ]
    df_result.attrs["supy_output_units"] = dict_units
    return df_result


def _resample_output(
    df_output,
    freq="60min",
    dict_aggm=dict_var_aggm,
    groups=None,
    freq_source=None,
    drop_empty_rows=True,
):
    """Master (141f0c347b) ``_resample_output`` preserving attrs on intermediates."""
    if hasattr(df_output, "_df_output"):
        held = df_output._held_interval(freq_source)
        df_output = df_output._df_output
    else:
        held = resolve_held_interval(df_output, freq_source)

    if held.kind == "fixed":
        try:
            target_s = pd.tseries.frequencies.to_offset(freq).nanos / 1e9
        except ValueError:
            target_s = None
        if target_s == held.seconds:
            return df_output
    elif held.kind == "unknown" and _index_freq_matches(df_output, freq):
        return df_output

    source_s = held.seconds if held.kind == "fixed" else None
    df_output = with_daily_coverage(df_output, source_s)
    dict_daily_coverage = {}
    dict_units = df_output.attrs.get("supy_output_units", {})
    water_columns = [c for c in df_output.columns if c in water_flux_variables()]
    rate_columns = [c for c in water_columns if c[1] in dict_units.get(c[0], {})]
    ser_coverage = (
        output_covered_seconds(df_output, source_s) if water_columns else None
    )
    dict_coverage = {}

    def _resample_group(
        df_group, freq, label, dict_aggm_group, group_name=None, grid=None
    ):
        if group_name == "DailyState":
            df_to_resample = df_group.dropna(how="all")

            if df_to_resample.empty:
                return pd.DataFrame(
                    index=pd.DatetimeIndex([]), columns=df_group.columns
                )

            coverage = df_output.attrs.get("supy_output_daily_coverage_s") or {}
            ser_daily_seconds = pd.Series(
                [coverage.get(f"{grid}|{date.isoformat()}", float("nan")) for date in df_to_resample.index],
                index=df_to_resample.index,
            )
            ser_daily_seconds = _dailystate_period_end(ser_daily_seconds, source_s)
            df_to_resample = _dailystate_period_end(df_to_resample, source_s)
            daily_seconds = _calendar_binning(ser_daily_seconds, freq).resample(
                freq, closed="right", label=label
            ).sum(min_count=1)
            dict_daily_coverage.update({f"{grid}|{date.isoformat()}": value for date, value in daily_seconds.dropna().items()})
            return _aggregate_bins(
                _calendar_binning(df_to_resample, freq).resample(freq, closed="right", label=label),
                dict_aggm_group,
                df_to_resample.columns,
            )
        else:
            df_to_resample = df_group

        columns_rate = [name for group, name in rate_columns if group == group_name]
        covered = None
        if ser_coverage is not None:
            covered = ser_coverage.xs(grid, level="grid")
            target = _calendar_binning(covered, freq).resample(freq, closed="right", label=label).sum()
            dict_coverage.update({f"{grid}|{date.isoformat()}": value for date, value in target.items()})
        if columns_rate:
            df_to_resample = df_to_resample.copy(deep=False)
            df_to_resample[columns_rate] = df_to_resample[columns_rate].mul(covered, axis=0)
        bins = _calendar_binning(df_to_resample, freq).resample(freq, closed="right", label=label)
        result = _aggregate_bins(bins, dict_aggm_group, df_to_resample.columns)
        if columns_rate:
            result[columns_rate] = result[columns_rate].div(target.where(target > 0), axis=0)
        occupied = bins.size().loc[lambda counts: counts > 0].index
        if len(occupied):
            result = result.loc[occupied[0]:occupied[-1]]
        return result

    calendar = _is_calendar_frequency(freq)
    list_grid = df_output.index.get_level_values("grid").unique()
    list_group = df_output.columns.get_level_values("group").unique()
    if groups is not None:
        list_group = [group for group in list_group if group in groups]

    df_rsmp = pd.concat(
        {
            grid: pd.concat(
                {
                    group: _resample_group(
                        df_output[group].xs(grid, level="grid"),
                        freq,
                        "left"
                        if group == "DailyState" and not calendar
                        else "right",
                        dict_aggm[group],
                        group_name=group,
                        grid=grid,
                    )
                    for group in list_group
                    if group in dict_aggm
                },
                axis=1,
                names=["group", "var"],
            )
            for grid in list_grid
        },
        names=["grid"],
    )

    if drop_empty_rows:
        df_rsmp = df_rsmp.dropna(how="all", axis=0)

    if isinstance(df_rsmp.index, pd.MultiIndex):
        if df_rsmp.index.names[1] is None:
            df_rsmp.index = df_rsmp.index.set_names(["grid", "datetime"])

    df_rsmp.attrs = df_output.attrs.copy()
    try:
        interval_s = pd.tseries.frequencies.to_offset(freq).nanos / 1e9
    except ValueError:
        interval_s = None
    df_rsmp.attrs["supy_output_interval_s"] = interval_s
    if dict_daily_coverage:
        df_rsmp.attrs["supy_output_daily_coverage_s"] = dict_daily_coverage
    elif "DailyState" in df_rsmp.columns.get_level_values("group"):
        df_rsmp.attrs["supy_output_daily_coverage_s"] = df_output.attrs.get(
            "supy_output_daily_coverage_s"
        )
    if dict_coverage:
        dict_actual = {
            f"{grid}|{date.isoformat()}": dict_coverage.get(f"{grid}|{date.isoformat()}", 0.0)
            for grid, date in df_rsmp.index
        }
        df_rsmp.attrs["supy_output_coverage_s"] = {
            key: value for key, value in dict_actual.items() if value != interval_s
        }
    return df_rsmp


def _output_group_frequencies(
    df_output: pd.DataFrame, fallback_frequency_s: int, groups=None
) -> dict:
    """Return each output group's nominal source-clock frequency in seconds.

    ``groups`` restricts the result to those groups (default: every group in
    ``df_output``).
    """
    if groups is None:
        groups = df_output.columns.get_level_values("group").unique()
    dict_group_frequency_s = {}
    for group in groups:
        df_group = df_output[group]
        mask = _rows_with_data(df_group)
        if not mask.any():
            continue
        idx_group = df_group.index[mask]
        if isinstance(idx_group, pd.MultiIndex) and "grid" in idx_group.names:
            grids = idx_group.get_level_values("grid")
            idx_group = idx_group[grids == grids[0]].droplevel("grid")
        idx_group = pd.DatetimeIndex(
            idx_group.get_level_values("datetime")
            if isinstance(idx_group, pd.MultiIndex)
            else idx_group
        ).drop_duplicates()
        if len(idx_group) < 2:
            dict_group_frequency_s[group] = (
                86400 if group == "DailyState" else fallback_frequency_s
            )
            continue
        ser_deltas = idx_group.to_series().diff().dropna()
        ser_positive_deltas = ser_deltas[ser_deltas > pd.Timedelta(0)]
        if ser_positive_deltas.empty:
            continue
        frequency = ser_positive_deltas.mode().iloc[0]
        dict_group_frequency_s[group] = int(frequency.total_seconds())
    return dict_group_frequency_s


def _save_groups(
    df_save: pd.DataFrame,
    list_group: list,
    index: pd.MultiIndex,
    path_dir_save,
    site,
    output_level,
    timestamp_reference="follow",
    timestamps_are_final=False,
    dict_group_frequency_s=None,
    step_s=None,
    metadata_interval_s=None,
    streamed_files: Optional[StreamedTextFiles] = None,
) -> list:
    """Save ``list_group`` of ``df_save`` one grid, group and year at a time.

    ``index`` replaces the row index of ``df_save`` (same length and order).
    Each group is taken as a view of ``df_save``, so the working set is one
    group-year rather than a copy of the table (gh#1849). *step_s* is the
    table's step, for tables of a single row; *streamed_files* is passed to
    :func:`save_df_grid_group`.
    """
    list_grid = index.get_level_values("grid").unique()
    list_year = index.get_level_values("datetime").year.unique()
    allow_irregular_timestamps = (
        _normalise_output_timestamp_reference(timestamp_reference) == "daylight"
    )
    list_path_save = []
    for grid in list_grid:
        for group in list_group:
            df_group = df_save[group]
            df_group.index = index
            df_grid_group = df_group.xs(grid, level="grid")
            if group == "DailyState":
                coverage = df_save.attrs.get("supy_output_daily_coverage_s")
                df_grid_group.attrs["supy_output_daily_coverage_s"] = {
                    f"{grid}|{new_date.isoformat()}": coverage[f"{grid}|{date.isoformat()}"]
                    for (old_grid, date), (_, new_date) in zip(df_save.index, index)
                    if old_grid == grid and f"{grid}|{date.isoformat()}" in coverage
                } if coverage is not None else None
            idx_asfreq = None if group == "DailyState" else _grid_asfreq_index(
                df_grid_group.index, allow_irregular_timestamps
            )
            for year in list_year:
                df_year = _gen_df_year(
                    df_grid_group,
                    group,
                    year,
                    output_level,
                    idx_asfreq,
                    timestamps_are_final,
                    step_s=step_s,
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
                        fallback_freq_s=step_s if step_s is not None else 300,
                        streamed_files=streamed_files,
                        year=year,
                        metadata_interval_s=86400
                        if group == "DailyState"
                        else metadata_interval_s,
                    )
                    list_path_save.append(path_save)
                del df_year
    return list_path_save


def _parquet_chunk_frame(
    df_output: pd.DataFrame, freq_save: pd.Timedelta, tstep_s: Optional[int]
) -> pd.DataFrame:
    """Return the rows one chunk of a streamed run writes to Parquet.

    Every group but DailyState is resampled to *freq_save* (left as it is at
    the model timestep *tstep_s*), keeping a row for every output interval
    even where those groups hold no value, as a run saved whole has one;
    DailyState keeps its own rows, the last step of each day, as the text
    output keeps them, rather than being resampled. Columns follow
    ``df_output``; rows are sorted by grid, then time.
    """
    ser_group = df_output.columns.get_level_values("group")
    list_main = [g for g in ser_group.unique() if g != "DailyState"]
    list_frame = []
    if list_main:
        freq_source = pd.Timedelta(tstep_s, "s") if tstep_s is not None else None
        df_main = _resample_output(
            df_output,
            freq_save,
            groups=list_main,
            freq_source=freq_source,
            drop_empty_rows=False,
        )
        mask_main = df_main.columns.get_level_values("group").isin(list_main)
        list_frame.append(df_main.loc[:, mask_main])
    if "DailyState" in ser_group:
        list_frame.append(_dailystate_rows(df_output))
    df_chunk = list_frame[0] if len(list_frame) == 1 else pd.concat(list_frame, axis=1)
    columns = df_output.columns[ser_group.isin(df_chunk.columns.get_level_values(0))]
    df_chunk = df_chunk.reindex(columns=columns).sort_index()
    # Main rows and sparse daily rows have different interval attributes;
    # concat otherwise drops the explicit unit declarations they share.
    df_chunk.attrs = df_output.attrs.copy()
    if list_main:
        df_chunk.attrs.update(df_main.attrs)
        coverage = df_main.attrs.get("supy_output_coverage_s", {}).copy()
        # Sparse DailyState-only rows carry no main-group water interval.
        coverage.update({
            f"{grid}|{date.isoformat()}": 0.0
            for grid, date in df_chunk.index.difference(df_main.index)
        })
        if coverage:
            df_chunk.attrs["supy_output_coverage_s"] = coverage
    df_chunk.attrs["supy_output_interval_s"] = freq_save.total_seconds()
    return df_chunk
