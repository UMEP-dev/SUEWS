import pandas as pd
import numpy as np


# locate the first position of period with in-between gaps
def loc_gap(ser_test, freq="1D", pattern="010"):
    rsmp = ser_test.resample(freq)
    ser_TF_10 = rsmp.apply(lambda ser: ser.isna().any()) * 1
    n_gap = ser_TF_10.sum()
    if cal_ratio_info(ser_test) < 0.5:
        raise RuntimeError(
            f"too many gaps (n_gap/n_period={n_gap}/{ser_TF_10.size}) to proceed! please enlarge the series to introduce more valid periods."
        )
    str_TF_10 = ser_TF_10.astype(str).str.cat()
    pos_gap = str_TF_10.find(pattern)
    loc_ser = ser_TF_10.iloc[pos_gap : pos_gap + len(pattern)].index
    return loc_ser


# fill gap with neighbouring days
def fill_gap_one(ser_test, freq="1D", pattern="010"):
    # resample into daily periods
    rsmp = ser_test.resample(freq)
    # locate the gaps according to gap pattern: 0 for NO gap, 1 for gapped
    loc_ser = loc_gap(ser_test, freq, pattern)

    # generator groups
    ser_find = (rsmp.get_group(x) for x in loc_ser)
    if len(loc_ser) == 0:
        return ser_test

    # assign series:
    # ser_prev: series prior to gapped period
    # ser_gap: series with gaps
    # ser_post: series after gapped period
    if pattern == "010":
        ser_prev, ser_gap, ser_post = ser_find
    elif pattern == "01":
        ser_prev, ser_gap = ser_find
        ser_post = pd.Series([])
    elif pattern == "10":
        ser_gap, ser_post = ser_find
        ser_prev = pd.Series([])

    # base series for gap filling
    ser_fill_base = pd.concat([ser_prev, ser_post])
    ser_fill = (
        ser_fill_base.groupby([
            ser_fill_base.index.hour.rename("hr"),
            ser_fill_base.index.minute.rename("min"),
        ])
        .median()
        .reset_index(drop=True)
    )
    try:
        ser_fill.index = ser_gap.index
    except ValueError:
        print(ser_test)
        print(pattern)
        print(ser_fill_base)
        print(ser_gap)

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
    ser_gap.loc[ser_gap.isna()] = ser_fill_gap.loc[ser_gap.isna()]
    ser_filled = pd.concat([ser_prev, ser_gap, ser_post])

    # fill the original gapped series
    ser_test_filled = ser_test.copy()
    ser_test_filled.loc[ser_filled.index] = ser_filled
    return ser_test_filled


def cal_ratio_info(ser_to_fill, freq="1d"):
    rsmp = ser_to_fill.resample(freq)
    ser_TF_10 = rsmp.apply(lambda ser: ser.isna().any()) * 1
    return 1 - ser_TF_10.sum() / ser_TF_10.size


# fill gaps iteratively
def fill_gap_all_x(ser_to_fill: pd.Series, freq="1D", limit_fill=1) -> pd.Series:
    """Fill all gaps in a time series using data from neighbouring divisions of 'freq'

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
        Gap-filled time series.

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

    ptn_list = ["010", "01", "10"]

    # if ratio_info > 0.5:
    n_freq = ser_test_filled.resample(freq).size().size
    if n_freq > 4:
        n_chunk = int(pd.Timedelta(freq) / ser_test_filled.index.freq)
        n_freq = int(ser_test_filled.size / n_chunk)
        if n_chunk * n_freq < ser_test_filled.size:
            ser_residual = ser_test_filled.iloc[n_chunk * n_freq :]
        else:
            ser_residual = pd.Series([])
        ser_test_filled = ser_test_filled.iloc[: n_chunk * n_freq]
        # ser_test_filled
        # n_freq
        if n_freq > 4:
            n_sep = int(n_freq / 2) * n_chunk
            #     offset = pd.Timedelta(freq) * n_sep
            #     ind_sep1 = ser_test_filled.index[0] + offset
            #     ind_sep2 = ser_test_filled.index[1] + offset
            ser_test_filled1 = ser_test_filled.iloc[:n_sep]
            ser_test_filled2 = ser_test_filled.iloc[n_sep:]
            rinfo1 = cal_ratio_info(ser_test_filled1)
            rinfo2 = cal_ratio_info(ser_test_filled2)
            rinfo_thresh = 0.2
            while (not (rinfo1 >= rinfo_thresh and rinfo2 >= rinfo_thresh)) and (
                n_sep < n_freq
            ):
                # print(rinfo1, rinfo2, n_sep)
                if rinfo1 < rinfo_thresh:
                    n_sep += n_chunk
                if rinfo2 < rinfo_thresh:
                    n_sep -= n_chunk
                ser_test_filled1 = ser_test_filled.iloc[:n_sep]
                ser_test_filled2 = ser_test_filled.iloc[n_sep:]
                rinfo1 = cal_ratio_info(ser_test_filled1)
                rinfo2 = cal_ratio_info(ser_test_filled2)

        # print(rinfo1, rinfo2, ser_test_filled1.size, ser_test_filled2.size)
        ser_test_filled1 = fill_gap_all_x(ser_test_filled1)
        ser_test_filled2 = fill_gap_all_x(ser_test_filled2)
        # print(ser_test_filled1.size)
        # print(ser_test_filled2.size)
        # print()
        ser_test_filled = pd.concat([ser_test_filled1, ser_test_filled2, ser_residual])
    else:
        while ser_test_filled.isna().any():
            # print("here")
            # try to different gap patterns and fill gaps
            try:
                ptn_gap = next(
                    ptn
                    for ptn in ptn_list
                    if len(loc_gap(ser_test_filled, freq, ptn)) == len(ptn)
                )
                # print(ser_test_filled,ptn_gap)
                ser_test_filled = fill_gap_one(ser_test_filled, freq, ptn_gap)
            except StopIteration:
                pass
            except RuntimeError as e:
                # print("cannot proceed so can only return the original gapped series!")
                return ser_test_filled

    return ser_test_filled


def fill_gap_all(
    ser_to_fill: pd.Series,
    freq="1D",
    limit_fill=1,
    thresh_ratio=0.8,
) -> pd.Series:
    """Fill all gaps in a time series using data from neighbouring divisions of 'freq'

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
        Gap-filled time series.

    Patterns
    --------
    010: missing data in division between others with no missing data
    01:  missing data in division after one with no missing data
    10:  division with missing data before one with no missing data
    """
    ratio_info = cal_ratio_info(ser_to_fill)
    ser_test_filled = norm_ser_dt(ser_to_fill)
    if ratio_info < thresh_ratio:
        raise RuntimeError(
            f"input series is too gapped (valid data ratio = {ratio_info:.2%}) to proceed with the gap-filling method."
        )
    else:
        # normalise into a series with complete diurnal cycles
        ser_test_filled = fill_gap_all_x(ser_test_filled, freq, limit_fill)
        # i = 0
        while ser_test_filled.isna().any():
            # i += 1
            ser_test_filled = fill_gap_all_x(ser_test_filled, freq, limit_fill)
            ser_gap = ser_test_filled.groupby(ser_test_filled.index.year).apply(
                lambda x: x.isna().sum()
            )
            # print(i, ser_gap)
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
    freq = ser_dt.index.freq
    dt_start, dt_end = ser_dt.index.normalize()[[0, -1]]
    dt_end = dt_end + pd.Timedelta("1d")
    dt_end
    idx_norm = pd.date_range(dt_start, dt_end, freq=freq)[:-1]
    ser_norm = ser_dt.reindex(idx_norm)
    return ser_norm
