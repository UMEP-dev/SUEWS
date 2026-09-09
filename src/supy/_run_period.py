"""Requested simulation period: resolution, slicing and coverage checks.

SUEWS forcing rows are stamped at the *end* of each interval, so a calendar
day ``D`` at a 5-minute timestep is the rows ``D 00:05`` .. ``D+1 00:00``.
This module turns the ``start_date`` / ``end_date`` a user gives to
:meth:`supy.SUEWSSimulation.run` (or ``model.control.start_time`` /
``end_time`` in the YAML) into an explicit row selection under that
convention, and checks that the loaded forcing actually covers the request
instead of silently running on whatever overlap happens to exist (gh#1268).

Bound semantics
---------------
* A **date-only** bound (``"2012-01-01"``, a :class:`datetime.date`) names a
  calendar day. ``start_date`` selects rows strictly after ``D 00:00``;
  ``end_date`` selects rows up to and including ``D+1 00:00``.
* A bound **with a time component** (``"2012-01-01 00:05"``, a
  :class:`pandas.Timestamp`) is an exact row timestamp and is inclusive at
  both ends, matching the historical ``DataFrame.loc`` behaviour.
* ``None`` means "no bound on that side"; the forcing bound is used.

Coverage
--------
The forcing covers the request when its first row is no later than the first
required row and its last row is no earlier than the last required row. A
request that is not covered raises :class:`ValueError` by default; the
caller may opt in to clipping, in which case the overlap is run and the
requested versus actual periods are logged and returned as metadata.
"""

from dataclasses import dataclass
import datetime as _dt
import math
import re
from typing import Any, Optional

import pandas as pd

from ._env import logger_supy

_DATE_ONLY_RE = re.compile(r"^\s*\d{4}-\d{1,2}-\d{1,2}\s*$")


def is_date_only(value: Any) -> bool:
    """Return True when ``value`` names a calendar day rather than an instant."""
    if isinstance(value, _dt.datetime):
        # datetime is a subclass of date; treat it as an instant.
        return False
    if isinstance(value, _dt.date):
        return True
    if isinstance(value, str):
        return bool(_DATE_ONLY_RE.match(value))
    return False


@dataclass(frozen=True)
class RunPeriod:
    """A resolved request for a simulation period.

    ``start`` and ``end`` are instants on the forcing clock. For a date-only
    start, ``start`` is that day's midnight and rows strictly after it are
    selected; for an explicit start the row at ``start`` itself is included.
    ``end`` is always inclusive; for a date-only end it is the following
    midnight, the interval-end stamp of the day's last interval.
    """

    start: Optional[pd.Timestamp]
    end: Optional[pd.Timestamp]
    start_is_date: bool
    end_is_date: bool
    start_raw: Any
    end_raw: Any

    def describe(self) -> str:
        left = "forcing start" if self.start is None else str(self.start_raw)
        right = "forcing end" if self.end is None else str(self.end_raw)
        return f"{left} to {right}"


def _coerce_bound(value: Any, index: pd.DatetimeIndex, name: str) -> pd.Timestamp:
    try:
        ts = pd.Timestamp(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(
            f"{name} must be a date, datetime or parseable string; got {value!r}"
        ) from exc
    if pd.isna(ts):
        raise ValueError(f"{name} must not be NaT")
    if index.tz is not None and ts.tzinfo is None:
        ts = ts.tz_localize(index.tz)
    return ts


def resolve_run_period(
    start_date: Any, end_date: Any, index: pd.DatetimeIndex
) -> RunPeriod:
    """Resolve raw ``start_date`` / ``end_date`` values against a forcing index."""
    start_is_date = start_date is not None and is_date_only(start_date)
    end_is_date = end_date is not None and is_date_only(end_date)

    start = None
    if start_date is not None:
        start = _coerce_bound(start_date, index, "start_date")
        if start_is_date:
            start = start.normalize()

    end = None
    if end_date is not None:
        end = _coerce_bound(end_date, index, "end_date")
        if end_is_date:
            # The day's last interval is stamped at the following midnight.
            end = end.normalize() + pd.Timedelta(days=1)

    empty = (
        start is not None
        and end is not None
        and (end < start or (end == start and start_is_date))
    )
    if empty:
        raise ValueError(
            f"end_date {end_date!r} is before start_date {start_date!r}; "
            "the requested simulation period is empty."
        )

    return RunPeriod(
        start=start,
        end=end,
        start_is_date=start_is_date,
        end_is_date=end_is_date,
        start_raw=start_date,
        end_raw=end_date,
    )


def _forcing_step(
    index: pd.DatetimeIndex, tstep: Optional[int] = None
) -> Optional[pd.Timedelta]:
    """Return the forcing row spacing, or ``None`` if it cannot be known.

    Uses the index ``freq`` when set, otherwise the observed spacing when
    every gap between consecutive rows is the same positive interval (two
    rows are enough), otherwise the model timestep ``tstep`` in seconds when
    given. Irregular spacing with no ``tstep`` yields ``None``.
    """
    freq = index.freq
    if freq is not None:
        try:
            return pd.Timedelta(freq)
        except (TypeError, ValueError):
            freq = None
    if len(index) >= 2:
        gaps = pd.Series(index[1:] - index[:-1])
        first_gap = gaps.iloc[0]
        if first_gap > pd.Timedelta(0) and (gaps == first_gap).all():
            return first_gap
    if tstep is not None and tstep > 0:
        return pd.Timedelta(seconds=int(tstep))
    return None


def _snap_to_grid(
    instant: pd.Timestamp,
    origin: pd.Timestamp,
    step: pd.Timedelta,
    *,
    up: bool,
) -> pd.Timestamp:
    """Move ``instant`` onto the forcing grid, rounding up or down."""
    offset = (instant - origin) / step
    n = math.ceil(offset) if up else math.floor(offset)
    return origin + n * step


def required_rows(
    period: RunPeriod, index: pd.DatetimeIndex, tstep: Optional[int] = None
) -> tuple[Optional[pd.Timestamp], Optional[pd.Timestamp]]:
    """Return the first and last forcing timestamps the request needs.

    ``None`` on a side means that side is unbounded. The row spacing comes
    from the index, the observed gaps, or ``tstep`` (seconds); a date-only
    start needs it to name its first row, so an unknown spacing raises
    rather than leaving the start unchecked. Explicit bounds with unknown
    spacing are used as given.
    """
    step = _forcing_step(index, tstep)
    origin = index[0]

    first = None
    if period.start is not None:
        if period.start_is_date:
            if step is None:
                raise ValueError(
                    f"Cannot check that the forcing covers the date-only "
                    f"start_date {period.start_raw!r}: the forcing rows have "
                    "no regular spacing and no model timestep was given. "
                    "Supply regularly spaced forcing, or give start_date as "
                    "an explicit timestamp."
                )
            first = period.start + step
        elif step is not None:
            first = _snap_to_grid(period.start, origin, step, up=True)
        else:
            first = period.start

    last = None
    if period.end is not None:
        if period.end_is_date or step is None:
            last = period.end
        else:
            last = _snap_to_grid(period.end, origin, step, up=False)

    return first, last


def _period_metadata(
    period: RunPeriod,
    df_slice: pd.DataFrame,
    *,
    clipped: bool,
    clip_to_forcing: bool,
) -> dict:
    """Build the requested-versus-actual period record for a run."""
    has_rows = isinstance(df_slice.index, pd.DatetimeIndex) and not df_slice.empty
    return {
        "requested_start": period.start,
        "requested_end": period.end,
        "requested_start_raw": period.start_raw,
        "requested_end_raw": period.end_raw,
        "actual_start": df_slice.index[0] if has_rows else None,
        "actual_end": df_slice.index[-1] if has_rows else None,
        "clipped": clipped,
        "policy": "clip" if clip_to_forcing else "strict",
    }


def _select_rows(df_forcing: pd.DataFrame, period: RunPeriod) -> pd.DataFrame:
    index = df_forcing.index
    mask = pd.Series(True, index=index)
    if period.start is not None:
        if period.start_is_date:
            mask &= index > period.start
        else:
            mask &= index >= period.start
    if period.end is not None:
        mask &= index <= period.end
    return df_forcing.loc[mask.to_numpy()]


def _check_coverage(
    period: RunPeriod,
    index: pd.DatetimeIndex,
    *,
    clip_to_forcing: bool,
    tstep: Optional[int] = None,
) -> bool:
    """Return True when the forcing covers ``period``; raise or warn otherwise."""
    first, last = required_rows(period, index, tstep)
    available_start, available_end = index[0], index[-1]
    covered = (first is None or available_start <= first) and (
        last is None or available_end >= last
    )
    if covered:
        return True
    detail = (
        f"{period.describe()}; needs forcing rows from "
        f"{first if first is not None else available_start} to "
        f"{last if last is not None else available_end}; "
        f"forcing available {available_start} to {available_end}"
    )
    if not clip_to_forcing:
        raise ValueError(
            f"Forcing does not cover the requested simulation period ({detail}). "
            "Supply forcing covering the period, set start_date/end_date "
            "(model.control.start_time/end_time) to the covered range, or "
            "pass clip_to_forcing=True to run() to run on the overlap only."
        )
    logger_supy.warning(
        f"Requested simulation period is not fully covered by the forcing "
        f"({detail}); clip_to_forcing=True, running on the overlap only."
    )
    return False


def slice_forcing_to_period(
    df_forcing: pd.DataFrame,
    period: RunPeriod,
    *,
    clip_to_forcing: bool = False,
    tstep: Optional[int] = None,
) -> tuple[pd.DataFrame, dict]:
    """Select the forcing rows for ``period`` and check coverage.

    Returns the sliced frame and a metadata dict with the requested and
    actual periods. Raises :class:`ValueError` when the request is not
    covered and ``clip_to_forcing`` is False, and always when the request
    and the forcing do not overlap at all. ``tstep`` (model timestep in
    seconds) is the fallback row spacing when the index carries none.
    """
    if df_forcing.empty:
        raise ValueError("forcing data is empty")
    index = df_forcing.index
    if not isinstance(index, pd.DatetimeIndex):
        # check_forcing reports a non-datetime index with its own message;
        # fall back to label slicing so that message is the one users see.
        df_slice = df_forcing.loc[period.start_raw : period.end_raw]
        return df_slice, _period_metadata(
            period, df_slice, clipped=False, clip_to_forcing=clip_to_forcing
        )

    df_slice = _select_rows(df_forcing, period)
    if df_slice.empty:
        raise ValueError(
            f"Requested simulation period ({period.describe()}) does not "
            f"overlap the loaded forcing (available {index[0]} to {index[-1]}). "
            "Supply forcing for the requested period or change "
            "start_date/end_date (model.control.start_time/end_time)."
        )
    covered = _check_coverage(
        period, index, clip_to_forcing=clip_to_forcing, tstep=tstep
    )
    return df_slice, _period_metadata(
        period, df_slice, clipped=not covered, clip_to_forcing=clip_to_forcing
    )
