import numpy as np
import pandas as pd


def _period_offsets(
    index: pd.DatetimeIndex, loc_key: pd.Timestamp
) -> pd.TimedeltaIndex:
    """Return civil-time offsets from ``loc_key`` within a division of ``freq``."""
    if index.tz is not None:
        key_naive = loc_key.tz_localize(None) if loc_key.tz is not None else loc_key
        return index.tz_localize(None) - key_naive
    return index - loc_key


def _find_gap_candidates(
    ser_test: pd.Series, freq: str = "1D", pattern: str = "010"
) -> tuple[pd.core.resample.Resampler, list[pd.DatetimeIndex], pd.DatetimeIndex]:
    """Return resampler, all period windows matching ``pattern``, and an empty index."""
    rsmp = ser_test.resample(freq)
    ser_TF_10 = rsmp.apply(lambda ser: ser.isna().any()) * 1
    n_gap = int(ser_TF_10.sum())
    n_total = ser_TF_10.size
    ratio_info = 0.0 if n_total == 0 else (n_total - n_gap) / n_total
    if ratio_info < 0.5:
        raise RuntimeError(
            f"too many gaps (n_gap/n_period={n_gap}/{n_total}) to proceed! please enlarge the series to introduce more valid periods."
        )
    empty_index = ser_TF_10.iloc[0:0].index
    if not pattern:
        return rsmp, [], empty_index
    str_TF_10 = ser_TF_10.astype(str).str.cat()
    candidates: list[pd.DatetimeIndex] = []
    start = 0
    while True:
        pos_gap = str_TF_10.find(pattern, start)
        if pos_gap == -1:
            break
        candidates.append(ser_TF_10.iloc[pos_gap : pos_gap + len(pattern)].index)
        start = pos_gap + 1
    return rsmp, candidates, empty_index


# locate the first position of period with in-between gaps
def loc_gap(ser_test, freq="1D", pattern="010"):
    _, candidates, empty_index = _find_gap_candidates(
        ser_test, freq=freq, pattern=pattern
    )
    if not candidates:
        return empty_index
    return candidates[0]


def _fill_gap_at_loc(
    ser_test: pd.Series,
    rsmp: pd.core.resample.Resampler,
    loc_ser: pd.DatetimeIndex,
    pattern: str,
) -> pd.Series:
    """Fill a single located gap window ``loc_ser`` using its neighbouring periods."""
    # assign series:
    # ser_prev: series prior to gapped period
    # ser_gap: series with gaps
    # ser_post: series after gapped period
    if pattern == "010":
        loc_prev, loc_gap_key, loc_post = loc_ser
        ser_prev = rsmp.get_group(loc_prev)
        ser_gap = rsmp.get_group(loc_gap_key).copy()
        ser_post = rsmp.get_group(loc_post)
        fill_parts = [
            pd.Series(
                ser_prev.to_numpy(), index=_period_offsets(ser_prev.index, loc_prev)
            ),
            pd.Series(
                ser_post.to_numpy(), index=_period_offsets(ser_post.index, loc_post)
            ),
        ]
    elif pattern == "01":
        loc_prev, loc_gap_key = loc_ser
        ser_prev = rsmp.get_group(loc_prev)
        ser_gap = rsmp.get_group(loc_gap_key).copy()
        fill_parts = [
            pd.Series(
                ser_prev.to_numpy(), index=_period_offsets(ser_prev.index, loc_prev)
            ),
        ]
    elif pattern == "10":
        loc_gap_key, loc_post = loc_ser
        ser_gap = rsmp.get_group(loc_gap_key).copy()
        ser_post = rsmp.get_group(loc_post)
        fill_parts = [
            pd.Series(
                ser_post.to_numpy(), index=_period_offsets(ser_post.index, loc_post)
            ),
        ]
    else:
        raise ValueError(f"Unsupported gap pattern: {pattern!r}")

    # align neighbouring periods by civil-time offset within each `freq` division
    ser_fill_base = pd.concat(fill_parts)
    ser_fill = (
        ser_fill_base
        .groupby(level=0)
        .median()
        .reindex(_period_offsets(ser_gap.index, loc_gap_key))
    )
    ser_fill.index = ser_gap.index

    # calculate rescaling factor with enough values to robustly rescale
    if (pattern == "010") and (ser_gap.count() > len(ser_gap) / 2):
        scale_fill = (ser_fill / ser_gap).median()
        # correct scale_fill for edge cases
        scale_fill = 1 if abs(scale_fill) > 10 else scale_fill
        scale_fill = 1 if abs(scale_fill) < 0.1 else scale_fill
        scale_fill = 1 if np.isnan(scale_fill) else scale_fill
    else:
        scale_fill = 1
    # rescale fill based on median ratio of fill:orig at available timesteps
    ser_fill_gap = ser_fill / scale_fill

    # fill in gaps with rescaled values of the filling data
    mask_na = ser_gap.isna()
    ser_gap.loc[mask_na] = ser_fill_gap.loc[mask_na]

    # fill the original gapped series
    ser_test_filled = ser_test.copy()
    ser_test_filled.loc[ser_gap.index] = ser_gap
    return ser_test_filled


# fill gap with neighbouring periods
def fill_gap_one(ser_test, freq="1D", pattern="010"):
    # locate the gaps according to gap pattern: 0 for NO gap, 1 for gapped
    rsmp, candidates, _ = _find_gap_candidates(ser_test, freq=freq, pattern=pattern)
    if not candidates or len(candidates[0]) != len(pattern):
        return ser_test
    return _fill_gap_at_loc(ser_test, rsmp, candidates[0], pattern)


def cal_ratio_info(ser_to_fill, freq="1d"):
    rsmp = ser_to_fill.resample(freq)
    ser_TF_10 = rsmp.apply(lambda ser: ser.isna().any()) * 1
    if ser_TF_10.size == 0:
        return 0.0
    n_valid = int((ser_TF_10 == 0).sum())
    return n_valid / ser_TF_10.size


def _infer_step_freq(index: pd.DatetimeIndex):
    """Return the index's regular step frequency, inferring it when unset."""
    if index.freq is not None:
        return index.freq
    if len(index) < 3:
        return None
    inferred = pd.infer_freq(index)
    if inferred is None:
        return None
    return pd.tseries.frequencies.to_offset(inferred)


def _chunk_size(index: pd.DatetimeIndex, freq: str) -> int | None:
    """Return timesteps per ``freq`` division, or ``None`` if non-uniform."""
    step_freq = _infer_step_freq(index)
    if step_freq is None:
        return None
    try:
        ratio = pd.Timedelta(freq) / step_freq
    except (TypeError, ValueError):
        return None
    n_chunk = int(ratio)
    return n_chunk if n_chunk > 0 else None


def _find_balanced_split(
    ser_head: pd.Series,
    n_freq: int,
    n_chunk: int,
    freq: str,
    rinfo_thresh: float = 0.2,
) -> tuple[pd.Series, pd.Series]:
    """Split ``ser_head`` into two non-empty chunk-aligned halves.

    Starts at the midpoint ``(n_freq // 2) * n_chunk`` and shifts by one
    ``n_chunk`` period at a time when one side falls below ``rinfo_thresh``.
    Both halves always remain strictly non-empty (at least ``n_chunk``
    timesteps each) so recursion terminates, and if no balanced split exists
    the midpoint split is returned.
    """
    n_total = n_chunk * n_freq
    n_sep_mid = (n_freq // 2) * n_chunk
    n_sep = n_sep_mid
    ser_left = ser_head.iloc[:n_sep]
    ser_right = ser_head.iloc[n_sep:]
    rinfo1 = cal_ratio_info(ser_left, freq=freq)
    rinfo2 = cal_ratio_info(ser_right, freq=freq)

    visited_seps = {n_sep}
    while not (rinfo1 >= rinfo_thresh and rinfo2 >= rinfo_thresh):
        if rinfo1 < rinfo_thresh and rinfo2 < rinfo_thresh:
            break
        next_sep = n_sep + n_chunk if rinfo1 < rinfo_thresh else n_sep - n_chunk
        if not (n_chunk <= next_sep <= n_total - n_chunk) or next_sep in visited_seps:
            break
        n_sep = next_sep
        visited_seps.add(n_sep)
        ser_left = ser_head.iloc[:n_sep]
        ser_right = ser_head.iloc[n_sep:]
        rinfo1 = cal_ratio_info(ser_left, freq=freq)
        rinfo2 = cal_ratio_info(ser_right, freq=freq)

    if not (rinfo1 >= rinfo_thresh and rinfo2 >= rinfo_thresh):
        ser_left = ser_head.iloc[:n_sep_mid]
        ser_right = ser_head.iloc[n_sep_mid:]

    return ser_left, ser_right


def _fill_gaps_iterative(ser_test_filled: pd.Series, freq: str) -> pd.Series:
    """Fill matching gap windows iteratively until complete or a pass makes no progress."""
    ptn_list = ["010", "01", "10"]
    while ser_test_filled.isna().any():
        made_progress = False
        for ptn in ptn_list:
            try:
                rsmp, candidates, _ = _find_gap_candidates(
                    ser_test_filled, freq=freq, pattern=ptn
                )
            except RuntimeError:
                return ser_test_filled
            for loc_ser in candidates:
                if len(loc_ser) != len(ptn):
                    continue
                n_missing_before = ser_test_filled.isna().sum()
                candidate_filled = _fill_gap_at_loc(ser_test_filled, rsmp, loc_ser, ptn)
                if candidate_filled.isna().sum() < n_missing_before:
                    ser_test_filled = candidate_filled
                    made_progress = True
                    break
            if made_progress:
                break
        if not made_progress:
            return ser_test_filled
    return ser_test_filled


def _fill_residual_tail(
    ser_head_filled: pd.Series,
    ser_residual: pd.Series,
    n_chunk: int,
    freq: str,
) -> pd.Series:
    """Fill gaps in a trailing partial-chunk residual using preceding context."""
    if not ser_residual.isna().any():
        return ser_residual
    n_res = ser_residual.size
    for mult in (1, 2):
        context_len = min(mult * n_chunk, ser_head_filled.size)
        if context_len == 0:
            break
        ser_window = pd.concat([ser_head_filled.iloc[-context_len:], ser_residual])
        ser_window_filled = _fill_gaps_iterative(ser_window, freq=freq)
        ser_residual = ser_window_filled.iloc[-n_res:]
        if not ser_residual.isna().any() or context_len == ser_head_filled.size:
            break
    return ser_residual


# fill gaps iteratively
def fill_gap_all_x(ser_to_fill: pd.Series, freq="1D", limit_fill=1) -> pd.Series:
    """Fill gaps in a time series using data from neighbouring divisions of ``freq``.

    Parameters
    ----------
    ser_to_fill : pd.Series
        Time series to gap-fill
    freq : str, optional
        Frequency to identify gapped divisions, by default '1D'
    limit_fill: int, optional
        Maximum number of consecutive NaNs to fill.
        Any number less than one means no pre-gap-filling interpolation will be done.

    Returns
    -------
    ser_test_filled: pd.Series
        Gap-filled time series. Filling is best-effort: any gaps that cannot be
        resolved (for example, clusters whose valid-period ratio stays below 0.5
        or offsets absent from neighbouring divisions) remain ``NaN`` so callers
        requiring a complete series should check ``not result.isna().any()``.

    Patterns
    --------
    010: missing data in division between others with no missing data
    01:  missing data in division after one with no missing data
    10:  division with missing data before one with no missing data
    """
    if limit_fill > 0:
        ser_test_filled = ser_to_fill.copy().interpolate(limit=limit_fill)
    else:
        ser_test_filled = ser_to_fill.copy()

    if not ser_test_filled.isna().any():
        return ser_test_filled

    n_freq_rsmp = ser_test_filled.resample(freq).size().size
    n_chunk = _chunk_size(ser_test_filled.index, freq) if n_freq_rsmp > 4 else None
    n_freq = ser_test_filled.size // n_chunk if n_chunk is not None else 0

    if n_freq > 4:
        n_total = n_chunk * n_freq
        ser_residual = (
            ser_test_filled.iloc[n_total:] if n_total < ser_test_filled.size else None
        )
        ser_head = ser_test_filled.iloc[:n_total]
        ser_test_filled1, ser_test_filled2 = _find_balanced_split(
            ser_head, n_freq, n_chunk, freq=freq
        )
        ser_test_filled1 = fill_gap_all_x(
            ser_test_filled1, freq=freq, limit_fill=limit_fill
        )
        ser_test_filled2 = fill_gap_all_x(
            ser_test_filled2, freq=freq, limit_fill=limit_fill
        )
        ser_head_filled = pd.concat([ser_test_filled1, ser_test_filled2])
        if ser_residual is not None:
            ser_residual = _fill_residual_tail(
                ser_head_filled, ser_residual, n_chunk, freq=freq
            )
            ser_test_filled = pd.concat([ser_head_filled, ser_residual])
        else:
            ser_test_filled = ser_head_filled
    else:
        ser_test_filled = _fill_gaps_iterative(ser_test_filled, freq=freq)

    return ser_test_filled


def fill_gap_all(
    ser_to_fill: pd.Series,
    freq="1D",
    limit_fill=1,
    thresh_ratio=0.8,
) -> pd.Series:
    """Fill gaps in a time series using data from neighbouring divisions of ``freq``.

    Parameters
    ----------
    ser_to_fill : pandas.Series
        Time series to gap-fill
    freq : str, optional
        Frequency to identify gapped divisions, by default '1D'
    limit_fill : int, optional
        Maximum number of consecutive NaNs to fill.
        Any number less than one means no pre-gap-filling interpolation will be done.
    thresh_ratio : float, optional
        Threshold ratio for gap filling, by default 0.8

    Returns
    -------
    pandas.Series
        Gap-filled time series indexed to ``ser_to_fill.index``. Filling is
        best-effort: the retry loop terminates if a full pass makes no progress,
        leaving any unresolvable gaps (for example dense gap clusters with
        ``limit_fill <= 0`` or offsets missing from neighbouring divisions) as
        ``NaN``. Callers requiring a completely gap-free series should check
        ``not result.isna().any()``.

    Patterns
    --------
    010: missing data in division between others with no missing data
    01:  missing data in division after one with no missing data
    10:  division with missing data before one with no missing data
    """
    ratio_info = cal_ratio_info(ser_to_fill, freq=freq)
    if ratio_info < thresh_ratio:
        raise RuntimeError(
            f"input series is too gapped (valid data ratio = {ratio_info:.2%}) to proceed with the gap-filling method."
        )
    else:
        # normalise into a series with complete diurnal cycles
        ser_test_filled = norm_ser_dt(ser_to_fill)
        ser_test_filled = fill_gap_all_x(
            ser_test_filled, freq=freq, limit_fill=limit_fill
        )
        while ser_test_filled.isna().any():
            n_missing_before = ser_test_filled.isna().sum()
            ser_test_filled = fill_gap_all_x(
                ser_test_filled, freq=freq, limit_fill=limit_fill
            )
            if ser_test_filled.isna().sum() >= n_missing_before:
                break
        ser_test_filled = ser_test_filled.loc[ser_to_fill.index]
        return ser_test_filled


def norm_ser_dt(ser_dt):
    """
    normalise a time series into a time series with complete diurnal cycles.

    Parameters
    ----------
    ser_dt : pandas.Series
        datetime-indexed time series

    Returns
    -------
    pandas.Series
        time series with complete diurnal cycles; missing values may be inserted as np.nan.
    """
    freq = _infer_step_freq(ser_dt.index)
    dt_start, dt_end = ser_dt.index.normalize()[[0, -1]]
    dt_end = dt_end + pd.Timedelta("1d")
    idx_norm = pd.date_range(dt_start, dt_end, freq=freq)[:-1]
    ser_norm = ser_dt.reindex(idx_norm)
    return ser_norm
