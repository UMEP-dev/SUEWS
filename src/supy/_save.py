import csv
from functools import partial
import io
import os
from pathlib import Path
import re
import shutil
import time
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
    decimal_doy = idx_standard.day_of_year.to_numpy(dtype=float) + seconds / 86400
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


def _relabel_output_index(
    index: pd.Index,
    timestamp_reference: object,
    forcing_timestamp_reference: object,
    df_state_final: Optional[pd.DataFrame],
) -> pd.Index:
    """Return ``index`` with its datetimes relabelled to the saved clock.

    The index itself is returned when no relabelling is needed, so callers
    can relabel a table without touching its values.
    """
    target = _normalise_output_timestamp_reference(timestamp_reference)
    source = _normalise_forcing_timestamp_reference(forcing_timestamp_reference)
    if target in {"follow", source}:
        return index

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
        return pd.MultiIndex.from_arrays(arrays, names=index.names)

    if isinstance(index, pd.DatetimeIndex):
        grid = df_state_final.index[0] if df_state_final is not None else None
        return _relabel_datetime_index(
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

    raise ValueError("Cannot relabel output timestamps: index is not datetime-like.")


def relabel_output_timestamps(
    df_output: pd.DataFrame,
    timestamp_reference: object,
    forcing_timestamp_reference: object,
    df_state_final: Optional[pd.DataFrame],
) -> pd.DataFrame:
    """Relabel output timestamps without changing computed values."""
    index = _relabel_output_index(
        df_output.index,
        timestamp_reference,
        forcing_timestamp_reference,
        df_state_final,
    )
    if index is df_output.index:
        return df_output

    df_relabelled = df_output.copy()
    df_relabelled.index = index
    return df_relabelled


# Cells handled per chunk when scanning or writing output tables: bounds the
# temporary value arrays and formatted lines to tens of MB whatever the table
# width; only the per-row date tuples of the table being written grow with
# its row count.
_CHUNK_CELLS = 1 << 20


def _rows_with_data(df_data: pd.DataFrame) -> np.ndarray:
    """Return a mask of the rows that hold at least one non-missing value.

    The mask equals the one ``df_data.dropna(how="all")`` keeps, but float
    tables are scanned a chunk of rows at a time, so a view of the held run
    output is never copied whole (gh#1849).
    """
    n_row, n_col = df_data.shape
    if n_col == 0:
        return np.zeros(n_row, dtype=bool)
    if not (df_data.dtypes == np.float64).all():
        return df_data.notna().any(axis=1).to_numpy()
    ar_value = df_data.to_numpy()
    mask = np.empty(n_row, dtype=bool)
    n_row_chunk = max(1, _CHUNK_CELLS // n_col)
    for start in range(0, n_row, n_row_chunk):
        stop = start + n_row_chunk
        np.any(~np.isnan(ar_value[start:stop]), axis=1, out=mask[start:stop])
    return mask


def _drop_empty_rows(df_data: pd.DataFrame) -> pd.DataFrame:
    """Drop all-missing rows, as ``dropna(how="all")``, without a full copy.

    ``df_data`` itself is returned when every row holds data.
    """
    mask = _rows_with_data(df_data)
    if mask.all():
        return df_data
    return df_data.iloc[np.flatnonzero(mask)]


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


def _gen_df_datetime(idx_dt: pd.DatetimeIndex) -> pd.DataFrame:
    """Return the Year, DOY, Hour, Min and Dectime columns of a saved table."""
    ser_year = pd.Series(idx_dt.year, index=idx_dt, name="Year")
    ser_DOY = pd.Series(idx_dt.day_of_year, index=idx_dt, name="DOY")
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
    # "D" not "d": pandas 3 deprecates the lowercase day alias.
    dt_delta = idx_dt - idx_dt.to_period("D").to_timestamp()
    df_datetime["Dectime"] = ser_DOY - 1 + dt_delta.total_seconds() / (24 * 60 * 60)
    return df_datetime


def _txt_header_names(df_year: pd.DataFrame) -> list:
    """Return the header fields of a saved text table, padded as written."""
    list_name_dt = ["Year", "DOY", "Hour", "Min", "Dectime"]
    return list_name_dt[:4] + [
        f"{name:>{max(8, len(name))}}"
        for name in list_name_dt[4:] + list(df_year.columns)
    ]


def _txt_header_line(df_year: pd.DataFrame) -> str:
    """Return the header line :func:`_write_df_txt` writes for *df_year*."""
    buffer = io.StringIO(newline="")
    csv.writer(buffer, delimiter="\t", lineterminator=os.linesep).writerow(
        _txt_header_names(df_year)
    )
    return buffer.getvalue()


def _write_df_txt(df_year: pd.DataFrame, path_out: Path, append: bool = False) -> None:
    r"""Write one grid-group-year table as SUEWS tab-separated text.

    With *append*, the rows are added to the end of an existing file and no
    header is written, so a file built from consecutive tables holds the
    same bytes as one written from their concatenation.

    The layout is the one SuPy has always written: the date columns, then
    every value as ``%.4f`` right-justified to ``max(8, len(name))`` with
    missing values as -999. Rows are formatted and written a block at a
    time, so no string copy of the whole table is built (gh#1849). The bytes
    match formatting every cell to a ``str`` and writing the table with
    ``pandas.DataFrame.to_csv(sep="\t", index=False)``: ``'%W.4f'`` and
    ``format(value, '>W.4f')`` share CPython's float formatting, and the
    header goes through the same csv writer settings pandas uses.
    """
    df_datetime = _gen_df_datetime(df_year.index)
    list_name_dt = [str(name) for name in df_datetime.columns]
    list_name_var = list(df_year.columns)
    list_header = _txt_header_names(df_year)
    fmt_dt = "\t".join(
        [f"%{max(3, len(name))}d" for name in list_name_dt[:4]] + ["%8.4f"]
    )
    fmt_var = "\t".join(f"%{max(8, len(name))}.4f" for name in list_name_var)

    list_dt = list(zip(*(df_datetime[name].tolist() for name in df_datetime.columns)))
    ar_value = df_year.to_numpy(dtype=np.float64)
    n_row, n_col = ar_value.shape
    n_row_chunk = max(1, _CHUNK_CELLS // max(1, n_col))
    linesep = os.linesep
    with open(
        path_out, "a" if append else "w", encoding="utf-8", newline=""
    ) as file_out:
        if not append:
            csv.writer(file_out, delimiter="\t", lineterminator=linesep).writerow(
                list_header
            )
        for start in range(0, n_row, n_row_chunk):
            stop = start + n_row_chunk
            if n_col:
                ar_chunk = ar_value[start:stop]
                ar_chunk = np.where(np.isnan(ar_chunk), -999.0, ar_chunk)
                list_line = [
                    fmt_dt % dt + "\t" + fmt_var % tuple(row)
                    for dt, row in zip(list_dt[start:stop], ar_chunk.tolist())
                ]
            else:
                list_line = [fmt_dt % dt for dt in list_dt[start:stop]]
            file_out.write(linesep.join(list_line))
            file_out.write(linesep)


def _row_label(line: str) -> Optional[pd.Timestamp]:
    """Return the timestamp label of one data line of a saved text file.

    The label is read from the leading Year, DOY, Hour and Min columns;
    ``None`` for a line that does not hold them (an incomplete last line).
    """
    fields = line.split("\t", 4)
    if len(fields) < 5:
        return None
    try:
        year, doy, hour, minute = (int(field) for field in fields[:4])
    except ValueError:
        return None
    return pd.Timestamp(year=year, month=1, day=1) + pd.Timedelta(
        days=doy - 1, hours=hour, minutes=minute
    )


def _keep_rows_up_to(path_out: Path, end_last: pd.Timestamp, header: str) -> bool:
    """Keep only the rows of a saved text file labelled at or before *end_last*.

    Saved rows are labelled by the end of the period they cover (DailyState
    by the last step of its day), so these are the rows a run ending at
    *end_last* produces. The header and the leading rows up to *end_last* are
    kept; the first row past it, an incomplete line, and everything after
    are dropped. The file is rewritten beside itself and renamed over, so an
    interruption leaves it either as it was or trimmed, and trimming again
    changes nothing.

    Returns ``False``, leaving the file as it is, when its first line is not
    *header* (an empty file, a header cut short, or another table's), so the
    caller writes the file anew.
    """
    with open(path_out, encoding="utf-8", newline="") as file_in:
        if file_in.readline() != header:
            return False
    path_tmp = path_out.with_name(path_out.name + ".tmp")
    with (
        open(path_out, encoding="utf-8", newline="") as file_in,
        open(path_tmp, "w", encoding="utf-8", newline="") as file_out,
    ):
        file_out.write(file_in.readline())
        for line in file_in:
            label = _row_label(line)
            if label is None or not line.endswith(("\n", "\r")) or label > end_last:
                break
            file_out.write(line)
    os.replace(path_tmp, path_out)
    return True


class StreamedTextFiles:
    """The text files a run streamed to disk in chunks has opened (gh#1849).

    A file is written with its header the first time the run writes to it
    and appended to afterwards, whichever chunk or year writes it, so a row
    that one year's last chunk writes into the next year's file (a period
    ending at 1 January 00:00, labelled by its end) is kept when the next
    year starts.

    *resume_after* is the ``last_timestamp`` of the checkpoint a run continues
    from. The first write to a file that already exists then keeps the rows
    the run up to the checkpoint produced (those labelled at or before it,
    see :func:`_keep_rows_up_to`) and appends after them, so a run
    resumed after an interruption ends with the files of an uninterrupted
    run, and resuming again from the same checkpoint gives the same files.
    """

    def __init__(self, resume_after: Optional[pd.Timestamp] = None) -> None:
        self.resume_after = (
            pd.Timestamp(resume_after) if resume_after is not None else None
        )
        self._set_started: set = set()

    def open_append(self, path_out: Path, header: str) -> bool:
        """Return whether to append to *path_out*, preparing it if needed.

        *header* is the header line the file starts with; a resumed run
        writes an existing file anew when its header is missing or differs.
        """
        key = Path(path_out).resolve()
        if key in self._set_started:
            return True
        self._set_started.add(key)
        if self.resume_after is not None and Path(path_out).exists():
            return _keep_rows_up_to(Path(path_out), self.resume_after, header)
        return False


_PART_NAME = re.compile(r"^grid(?P<grid>-?\d+)\.part(?P<seq>\d+)\.parquet$")


class StreamedParquetFile:
    """The Parquet output file of a run streamed to disk in chunks (gh#1849).

    Saving the whole run writes one file whose rows run grid by grid, each
    grid in time order. A run streamed in chunks produces the rows chunk by
    chunk, all grids at once, and a Parquet file can be neither reordered
    nor extended once closed; a file still open when the process dies has no
    footer and cannot be read. So each chunk's rows are written, one row
    group per grid, into staging part files in a folder beside the output
    file (``.<output name>.parts``). Every part is closed at each calendar
    year end, before that year's checkpoint is written, so the rows a
    checkpoint covers are always in complete files.

    :meth:`finish` joins the parts into the output file, grid by grid, one
    row group per chunk and grid, written beside it and renamed over it, then
    writes the files saved beside it; :meth:`cleanup` removes the parts.
    Read back, the file holds the frame saving the whole run writes. Where
    two consecutive chunks give a row the same label (daily output: the day
    that closes at a chunk boundary and the DailyState of the day that opens
    there), the row is joined from both, as resampling the whole run joins
    it.

    *path_output* is the output file (see :func:`parquet_output_path`).
    :meth:`prepare`, called before the run's first chunk, removes another
    run's output file and parts or, with *resume_after* (the
    ``last_timestamp`` of the checkpoint a run continues from), drops from
    each part the row groups (chunks) whose rows go past it, logging what it
    drops; new parts then follow the remaining ones, and resuming again from
    the same checkpoint gives the same file. A grid's last part left without
    a footer by an interruption was opened after the last checkpoint, so all
    its rows are written again: it is reported and discarded. Any other part
    that cannot be read holds rows the checkpoint covers, which cannot be
    recovered: the run stops with an error naming it.

    *resample* is whether saving the whole run resamples it (``False`` when
    the output frequency is the model timestep), so that every chunk, a
    short last one included, is treated as the whole run is.
    """

    def __init__(
        self,
        path_output: Path,
        resume_after: Optional[pd.Timestamp] = None,
        resample: bool = True,
    ) -> None:
        self.path_output = Path(path_output)
        self.path_parts = self.path_output.with_name(f".{self.path_output.name}.parts")
        self.resume_after = (
            pd.Timestamp(resume_after) if resume_after is not None else None
        )
        self.resample = resample
        self._schema = None
        self._list_grid: list = []
        self._dict_writer: dict = {}
        self._seq = 0
        self._write_sidecars = None

    def write(self, path_output: Path, df_frame: pd.DataFrame, write_sidecars) -> None:
        """Write one chunk's rows, one row group per grid.

        *write_sidecars* writes the files saved beside the output file and
        returns their paths; :meth:`finish` calls it once.
        """
        import pyarrow as pa
        import pyarrow.parquet as pq

        if Path(path_output) != self.path_output:
            raise ValueError(
                f"stream_chunk: this run writes {self.path_output}, not {path_output}."
            )
        self._write_sidecars = write_sidecars
        tbl = pa.Table.from_pandas(df_frame, preserve_index=True)
        if self._schema is None:
            self._schema = tbl.schema
        else:
            tbl = tbl.cast(self._schema)
        ar_grid = df_frame.index.get_level_values("grid")
        for grid in pd.unique(ar_grid):
            if grid not in self._list_grid:
                self._list_grid.append(grid)
            tbl_grid = tbl.take(np.flatnonzero(ar_grid == grid))
            if len(tbl_grid) == 0:
                continue
            writer = self._dict_writer.get(grid)
            if writer is None:
                path_part = self.path_parts / f"grid{grid}.part{self._seq:05d}.parquet"
                self._seq += 1
                writer = pq.ParquetWriter(path_part, self._schema, compression="snappy")
                self._dict_writer[grid] = writer
            writer.write_table(tbl_grid, row_group_size=max(len(tbl_grid), 1))

    def end_year(self) -> None:
        """Close the open parts, so the rows written so far can be read."""
        for writer in self._dict_writer.values():
            writer.close()
        self._dict_writer = {}

    def finish(self) -> list:
        """Write the output file from the parts, then the files beside it.

        Returns the paths written, the output file first, as
        :func:`save_df_output_parquet` returns them.
        """
        import pyarrow.parquet as pq

        self.end_year()
        dict_part = self._parts()
        list_grid = self._list_grid + sorted(set(dict_part) - set(self._list_grid))
        path_tmp = self.path_output.with_name(self.path_output.name + ".tmp")
        with pq.ParquetWriter(path_tmp, self._schema, compression="snappy") as writer:
            for grid in list_grid:
                tbl_last = None
                for path_part in dict_part.get(grid, []):
                    file_part = pq.ParquetFile(path_part)
                    for i_group in range(file_part.num_row_groups):
                        tbl = file_part.read_row_group(i_group).cast(self._schema)
                        if tbl_last is not None:
                            tbl = self._join_rows(tbl_last, tbl)
                        # The last row may share its label with the next
                        # chunk's first row; hold it until that is known.
                        tbl_last = tbl.slice(len(tbl) - 1)
                        if len(tbl) > 1:
                            writer.write_table(tbl.slice(0, len(tbl) - 1))
                if tbl_last is not None:
                    writer.write_table(tbl_last)
        os.replace(path_tmp, self.path_output)
        logger_supy.info("Saved Parquet output to %s", self.path_output)
        return [self.path_output, *self._write_sidecars()]

    def cleanup(self) -> None:
        """Remove the staging parts once the output file is complete."""
        if self.path_parts.exists():
            shutil.rmtree(self.path_parts)

    def _join_rows(self, tbl_last, tbl):
        """Put the held row *tbl_last* before *tbl*, joining equal labels.

        Each row holds values only where the chunk that wrote it computed
        them, so the joined row takes each value from whichever row has it.
        """
        import pyarrow as pa

        label_last = tbl_last.column("datetime")[0]
        if not label_last.equals(tbl.column("datetime")[0]):
            return pa.concat_tables([tbl_last, tbl])
        df_last = tbl_last.to_pandas()
        df_first = tbl.slice(0, 1).to_pandas()
        df_joined = df_last.where(df_last.notna(), df_first.to_numpy())
        tbl_joined = pa.Table.from_pandas(df_joined, preserve_index=True)
        return pa.concat_tables([tbl_joined.cast(self._schema), tbl.slice(1)])

    def _parts(self) -> dict:
        """Map each grid to its part files, in the order they were written."""
        dict_part: dict = {}
        for path in sorted(self.path_parts.iterdir()):
            match = _PART_NAME.match(path.name)
            if match is not None:
                key = (int(match["seq"]), path)
                dict_part.setdefault(int(match["grid"]), []).append(key)
        return {grid: [path for _, path in sorted(v)] for grid, v in dict_part.items()}

    @staticmethod
    def _last_label(file_part, i_group: int) -> pd.Timestamp:
        """Return the latest row label in row group *i_group* of a part."""
        tbl = file_part.read_row_group(i_group, columns=["datetime"])
        return tbl.column("datetime").to_pandas().max()

    def prepare(self) -> None:
        """Prepare the staging folder before the run's first chunk."""
        path_output = self.path_output
        if self.resume_after is None:
            # A fresh run: another run's parts or output must not mix in.
            path_tmp = path_output.with_name(path_output.name + ".tmp")
            for path in (self.path_parts, path_output, path_tmp):
                if path.exists():
                    logger_supy.info(
                        "stream_chunk: removing %s left by a previous run.",
                        path.resolve(),
                    )
                    if path.is_dir():
                        shutil.rmtree(path)
                    else:
                        path.unlink()
        self.path_parts.mkdir(parents=True, exist_ok=True)
        if self.resume_after is not None:
            self._trim_parts()

    def _trim_parts(self) -> None:
        """Keep in each part only the chunks the run up to the checkpoint wrote."""
        import pyarrow as pa
        import pyarrow.parquet as pq

        dict_part = self._parts()
        seq_max = -1
        for list_path in dict_part.values():
            for pos, path_part in enumerate(list_path):
                seq_max = max(seq_max, int(_PART_NAME.match(path_part.name)["seq"]))
                try:
                    file_part = pq.ParquetFile(path_part)
                except (pa.ArrowInvalid, OSError) as err:
                    if pos < len(list_path) - 1:
                        raise RuntimeError(
                            f"stream_chunk: {path_part} cannot be read ({err}); it "
                            f"holds output the checkpoint at {self.resume_after} "
                            "covers, which cannot be recovered. Run again from the "
                            "start, or from a checkpoint that ends before this "
                            "file was written."
                        ) from err
                    logger_supy.warning(
                        "stream_chunk: %s was left incomplete by the interrupted "
                        "run (%s); its rows all follow the checkpoint at %s, so it "
                        "is discarded and they are written again.",
                        path_part,
                        err,
                        self.resume_after,
                    )
                    path_part.unlink()
                    continue
                n_group = file_part.num_row_groups
                list_keep = [
                    i_group
                    for i_group in range(n_group)
                    if self._last_label(file_part, i_group) <= self.resume_after
                ]
                if len(list_keep) == n_group:
                    continue
                n_row = file_part.metadata.num_rows - sum(
                    file_part.metadata.row_group(i).num_rows for i in list_keep
                )
                logger_supy.info(
                    "stream_chunk: dropping %d rows written after the checkpoint "
                    "at %s from %s.",
                    n_row,
                    self.resume_after,
                    path_part,
                )
                if not list_keep:
                    path_part.unlink()
                    continue
                path_tmp = path_part.with_name(path_part.name + ".tmp")
                with pq.ParquetWriter(
                    path_tmp, file_part.schema_arrow, compression="snappy"
                ) as writer:
                    for i_group in list_keep:
                        writer.write_table(file_part.read_row_group(i_group))
                os.replace(path_tmp, path_part)
        self._seq = seq_max + 1


def _asfreq_frame(df_data: pd.DataFrame, freq) -> pd.DataFrame:
    """Return ``df_data.asfreq(freq)``, without a copy when no row changes.

    The target index comes from pandas' own ``asfreq`` applied to the index
    alone; when it holds the same timestamps, only the index is swapped.
    """
    idx_asfreq = pd.DataFrame(index=df_data.index).asfreq(freq).index
    if idx_asfreq.equals(df_data.index):
        df_out = df_data.copy(deep=False)
        df_out.index = idx_asfreq
        return df_out
    return df_data.reindex(idx_asfreq)


def _grid_asfreq_index(
    idx_dt: pd.DatetimeIndex, allow_irregular_timestamps: bool
) -> Optional[pd.DatetimeIndex]:
    """Return the regular index one grid's output is reindexed to for saving.

    ``None`` when the index is kept as it is: a single timestamp, an
    undefined last interval, or irregular daylight timestamps.
    """
    if len(idx_dt) > 1 and not allow_irregular_timestamps:
        freq = idx_dt.to_series().diff().iloc[-1]
        if pd.notna(freq):
            return pd.DataFrame(index=idx_dt).asfreq(freq).index
    return None


def _gen_df_year(
    df_grid_group: pd.DataFrame,
    group: str,
    year,
    output_level: int,
    idx_asfreq: Optional[pd.DatetimeIndex],
    timestamps_are_final: bool,
    step_s: Optional[int] = None,
) -> pd.DataFrame:
    """Return one year of one grid's group, ready for saving.

    ``df_grid_group`` is the whole run of the group for one grid and may be a
    view of the held output; only the rows and columns of ``year`` are
    copied. ``idx_asfreq`` is the grid's regular index from
    :func:`_grid_asfreq_index`, or ``None`` to keep the timestamps as they
    are. *step_s* is the table's step, used to move a single row, whose
    index carries no frequency, forward to the end of its period.
    """
    columns = None
    if group == "SUEWS":
        # select output variables in `SUEWS` based on output level
        vars_to_keep = [
            v for v in dict_level_var[output_level] if v in df_grid_group.columns
        ]
        if vars_to_keep:
            columns = vars_to_keep
    if idx_asfreq is not None:
        idx_year = pd.DataFrame(index=idx_asfreq).loc[f"{year}"].index
        df_year = df_grid_group.reindex(index=idx_year, columns=columns)
        df_year.index = idx_year
    else:
        df_year = df_grid_group if columns is None else df_grid_group[columns]
        df_year = df_year.loc[f"{year}"].copy(deep=False)
    # shift back to align with SUEWS convention; DailyState holds
    # end-of-day values and is not shifted
    if group != "DailyState" and not timestamps_are_final:
        if df_year.index.freq is None and step_s is not None:
            df_year.index = df_year.index + pd.Timedelta(seconds=step_s)
        else:
            df_year.index = df_year.index.shift(1)
    return _drop_empty_rows(df_year)


def save_df_grid_group(
    df_year,
    grid,
    group,
    dir_save,
    site,
    timestamp_reference="follow",
    freq_s=None,
    allow_irregular_timestamps=False,
    fallback_freq_s=300,
    streamed_files: Optional[StreamedTextFiles] = None,
    year: Optional[int] = None,
):
    """Write one grid-group-year table as a text file; return its path.

    The frequency in the file name is *freq_s*, else inferred from the
    table's first two rows, else *fallback_freq_s* (the table's step) for a
    table of one row. *year* names the file; it is the year
    :func:`_save_groups` selected the rows for, which differs from the year
    of the first label when that label closes the previous year (a period
    ending at 1 January 00:00). Without it, the first label's year is used.
    With *streamed_files*, the rows are appended to a file this run has
    already started (see :class:`StreamedTextFiles`).
    """
    # processing path
    path_dir = Path(dir_save)
    # pandas bug here: monotonic datetime index would lose `freq` once `pd.concat`ed
    if freq_s is not None:
        freq = pd.Timedelta(freq_s, unit="s")
        if not allow_irregular_timestamps:
            df_year = _asfreq_frame(df_year, freq)
    elif allow_irregular_timestamps:
        raise ValueError(
            "An output frequency is required for irregular daylight timestamps."
        )
    elif df_year.shape[0] > 0 and df_year.index.size >= 2:
        ind = df_year.index
        freq_cal = ind[1] - ind[0]
        df_year = _asfreq_frame(df_year, freq_cal)
        freq = pd.Timedelta(df_year.index.freq)
    else:
        df_year = _asfreq_frame(df_year, pd.Timedelta(fallback_freq_s, unit="s"))
        freq = pd.Timedelta(df_year.index.freq)
    # output frequency in min
    freq_min = int(freq.total_seconds() / 60)
    # starting year
    if year is None:
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
    append = streamed_files is not None and streamed_files.open_append(
        path_out, _txt_header_line(df_year)
    )
    t_start = time.time()
    _write_df_txt(df_year, path_out, append=append)
    t_end = time.time()
    # remove freq info from `DailyState` file
    if "DailyState" in path_out.name:
        str_fn_dd = str(path_out).replace("DailyState_5", "DailyState")
        path_out.rename(Path(str_fn_dd))
        path_out = Path(str_fn_dd)
    logger_supy.debug(f"{path_out} saved in {t_end - t_start:.2f} s")
    return path_out


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


def _dailystate_rows(df_output: pd.DataFrame) -> pd.DataFrame:
    """Return the DailyState columns of the rows that hold daily values.

    Equals ``df_output.loc[:, ["DailyState"]].dropna(how="all")``; only the
    (sparse) daily rows are copied.
    """
    df_daily = df_output.iloc[:, df_output.columns.get_loc("DailyState")]
    return df_daily.iloc[np.flatnonzero(_rows_with_data(df_daily))]


def _save_row_index(
    index: pd.MultiIndex,
    shift_to_period_start: bool,
    timestamp_reference: object,
    forcing_timestamp_reference: object,
    df_state_final: Optional[pd.DataFrame],
    step_s: Optional[int] = None,
) -> pd.MultiIndex:
    """Return the row index a table is saved under.

    With ``shift_to_period_start`` the datetime level is moved back one step
    so timestamps mark the start of each period (each year's rows are moved
    forward again in :func:`_gen_df_year`); the result is then relabelled to
    the requested clock. A single timestamp carries no step of its own and
    is moved back by *step_s*, the table's step.
    """
    if shift_to_period_start:
        idx_dt = index.get_level_values("datetime").drop_duplicates()

        # cast freq to index if not associated
        if idx_dt.freq is None:
            ser_idx = idx_dt.to_series()
            if len(ser_idx) > 1:
                freq = ser_idx.diff().iloc[-1]
                idx_dt = ser_idx.asfreq(freq).index
            else:
                idx_dt = ser_idx.index

        if len(idx_dt) > 1:
            idx_dt = idx_dt.shift(-1)
        elif step_s is not None:
            idx_dt = idx_dt - pd.Timedelta(seconds=step_s)

        index = index.set_levels(idx_dt, level="datetime")
    return _relabel_output_index(
        index, timestamp_reference, forcing_timestamp_reference, df_state_final
    )


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
    # Every labelled year is written: once moved to the start of their
    # periods, the rows of a run ending just after 1 January 00:00 belong to
    # the new year, whatever the number of grids.
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
            idx_asfreq = _grid_asfreq_index(
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
                    )
                    list_path_save.append(path_save)
                del df_year
    return list_path_save


# save output files
# Groups ``save_df_output`` writes when no ``output_groups`` are given.
DEFAULT_OUTPUT_GROUPS: tuple[str, ...] = ("SUEWS", "DailyState")


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
    streamed_files: Optional[StreamedTextFiles] = None,
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
    streamed_files : StreamedTextFiles, optional
        Set when ``df_output`` is one chunk of a run streamed to disk: files
        the run has already started are appended to, see
        :class:`StreamedTextFiles`. ``None`` (the default) writes every file
        anew.

    Returns
    -------
    list
        a list of `Path` objects for saved txt files

    Notes
    -----
    ``df_output`` is never copied whole: groups are selected, resampled,
    relabelled and written one (grid, group) at a time (gh#1849).
    """
    # resample output if `freq_s` is different from runtime `freq` (usually 5 min)
    freq_save = pd.Timedelta(freq_s, "s")

    # Handle output groups filtering
    if output_groups is None:
        # Default groups
        output_groups = list(DEFAULT_OUTPUT_GROUPS)

    # requested groups present in the output, in output order
    all_groups = df_output.columns.get_level_values("group").unique().tolist()
    list_group_save = [g for g in all_groups if g in output_groups]

    # drop snow related group from output groups if not requested
    if not save_snow:
        list_group_save = [g for g in list_group_save if g != "snow"]

    # Extract DailyState before resampling (it contains daily variables only written at last timestep of each day)
    df_dailystate = None
    if "DailyState" in list_group_save:
        df_dailystate = _dailystate_rows(df_output)
    list_group_no_daily = [g for g in list_group_save if g != "DailyState"]

    # resample `df_output` at `freq_save` (excluding DailyState); at the
    # native timestep `df_output` itself comes back, unresampled
    df_rsmp = None
    list_group_rsmp = []
    if list_group_no_daily:
        df_rsmp = _resample_output(df_output, freq_save, groups=list_group_no_daily)
        list_group_rsmp = [
            g
            for g in df_rsmp.columns.get_level_values("group").unique()
            if g in list_group_no_daily
        ]

    # tables to save: (table, its groups to save, fallback frequency)
    if save_tstep:
        # both original and resampled output dataframes
        list_df_save = []
        if list_group_save:
            # Preserve the historical five-minute singleton fallback when the
            # native cadence cannot be inferred from more than one timestamp.
            list_df_save.append((df_output, list_group_save, 300))
        # At the native timestep the "resampled" table is the native table and
        # its groups are among the ones the first pass saves, so saving it
        # again would only rewrite the same files and list them twice
        # (gh#1849).
        is_native_repeat = df_rsmp is df_output and set(list_group_rsmp) <= set(
            list_group_save
        )
        if df_rsmp is not None and list_group_rsmp and not is_native_repeat:
            list_df_save.append((df_rsmp, list_group_rsmp, freq_s))
    else:
        # resampled data with DailyState (if it exists)
        list_df_save = []
        if df_dailystate is not None:
            list_df_save.append((df_dailystate, ["DailyState"], 86400))
        if df_rsmp is not None:
            list_df_save.append((df_rsmp, list_group_rsmp, freq_s))

    # save output at the resampling frequency
    output_timestamp_reference = _normalise_output_timestamp_reference(
        timestamp_reference
    )
    timestamps_are_final = output_timestamp_reference != "follow"
    list_path_save = []
    for df_save, list_group, fallback_frequency_s in list_df_save:
        dict_group_frequency_s = (
            _output_group_frequencies(df_save, fallback_frequency_s, groups=list_group)
            if timestamps_are_final
            else None
        )
        # Check if this is DailyState-only data
        is_dailystate_only = len(list_group) > 0 and all(
            group == "DailyState" for group in list_group
        )
        # For regular output data, shift temporal index to make timestamps
        # indicating the start of periods; DailyState already represents
        # daily values and is not shifted
        index_save = _save_row_index(
            df_save.index,
            not is_dailystate_only and not timestamps_are_final,
            timestamp_reference,
            forcing_timestamp_reference,
            df_state_final,
            step_s=fallback_frequency_s,
        )
        list_path_save += _save_groups(
            df_save,
            list_group,
            index_save,
            path_dir_save,
            site,
            output_level,
            timestamp_reference,
            timestamps_are_final=timestamps_are_final,
            dict_group_frequency_s=dict_group_frequency_s,
            step_s=fallback_frequency_s,
            streamed_files=streamed_files,
        )
    return list_path_save


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


def _parquet_engine() -> str:
    """Return the Parquet engine, raising a helpful error without pyarrow."""
    try:
        import pyarrow  # noqa: F401
    except ImportError as e:
        raise ImportError(
            "Parquet output requires 'pyarrow'. "
            "Install with: pip install 'supy[parquet]' or pip install pyarrow"
        ) from e
    return "pyarrow"


def _parquet_file_name(site: str, kind: str, timestamp_suffix: str) -> str:
    """Name of the Parquet file of *kind* (``output``, ``metadata``, ...)."""
    stem = f"{site}_SUEWS_{kind}" if site else f"SUEWS_{kind}"
    return f"{stem}{timestamp_suffix}.parquet"


def parquet_output_path(path_dir_save: Path, site: str, timestamp_reference) -> Path:
    """Path of the Parquet output file :func:`save_df_output_parquet` writes.

    *site* is the filesystem-safe site token.
    """
    timestamp_suffix = _timestamp_reference_filename_suffix(timestamp_reference)
    return Path(path_dir_save) / _parquet_file_name(site, "output", timestamp_suffix)


def _write_parquet_sidecars(
    df_state_final: pd.DataFrame,
    path_dir_save: Path,
    site: str,
    site_metadata: Optional[str],
    freq_s: int,
    save_tstep: bool,
    save_state: bool,
    timestamp_reference,
) -> list:
    """Write the final-state (with *save_state*) and metadata Parquet files.

    These are the files :func:`save_df_output_parquet` writes beside the
    output file; returns their paths.
    """
    from ._version import __version__

    engine = _parquet_engine()
    timestamp_suffix = _timestamp_reference_filename_suffix(timestamp_reference)
    list_path_save = []
    if save_state:
        path_state = path_dir_save / _parquet_file_name(
            site, "state_final", timestamp_suffix
        )
        df_state_final.to_parquet(
            path_state, engine=engine, compression="snappy", index=True
        )
        list_path_save.append(path_state)

    # Save metadata as a separate small parquet file
    metadata = {
        "site": site if site_metadata is None else site_metadata,
        "output_frequency_s": freq_s,
        "save_tstep": save_tstep,
        "timestamp_reference": _normalise_output_timestamp_reference(
            timestamp_reference
        ),
        "creation_time": pd.Timestamp.now().isoformat(),
        "version": __version__,
    }
    path_meta = path_dir_save / _parquet_file_name(site, "metadata", timestamp_suffix)
    df_meta = pd.DataFrame([metadata])
    df_meta.to_parquet(path_meta, engine=engine)
    list_path_save.append(path_meta)
    return list_path_save


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
    streamed_parquet: Optional["StreamedParquetFile"] = None,
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
    streamed_parquet : StreamedParquetFile, optional
        Set when ``df_output`` is one chunk of a run streamed to disk: the
        chunk's rows are handed to it, and the output file and the files
        beside it are written once, when the run ends (see
        :class:`StreamedParquetFile`). Only the output file's path is
        returned. ``None`` (the default) writes every file now.

    Returns
    -------
    list
        List containing paths to saved Parquet files
    """
    engine = _parquet_engine()

    # Resample if needed; values are not written in place (gh#1849)
    df_save = df_output.copy(deep=False)
    freq_save = pd.Timedelta(freq_s, "s")

    # A streamed chunk is resampled exactly when the whole run would be, so a
    # short last chunk at the model timestep is not resampled on its own.
    resample = not save_tstep and (
        streamed_parquet is None or streamed_parquet.resample
    )
    if resample:
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
    path_output = parquet_output_path(path_dir_save, site, timestamp_reference)
    write_sidecars = partial(
        _write_parquet_sidecars,
        df_state_final,
        path_dir_save,
        site,
        site_metadata,
        freq_s,
        save_tstep,
        save_state,
        timestamp_reference,
    )

    if streamed_parquet is not None:
        streamed_parquet.write(path_output, df_to_save, write_sidecars)
        return [path_output]

    # Write output data
    df_to_save.to_parquet(
        path_output,
        engine=engine,
        compression="snappy",
        index=True,  # Preserve multi-index
    )
    list_path_save = [path_output, *write_sidecars()]

    logger_supy.info(f"Saved Parquet output to {path_output}")
    return list_path_save
