from pathlib import Path

import numpy as np
import pandas as pd

from .._env import logger_supy
from .._load import (
    BASELINE_DATETIME_FORCING_COLUMNS,
    BASELINE_FORCING_COLUMNS,
    FORCING_OPTIONAL_FILL,
    load_SUEWS_Forcing_met_df_pattern,
    resample_forcing_met,
    set_index_dt,
)
from ._missing import from_nan, to_nan

# Surface air pressure is never below ~300 hPa (that altitude is well into
# the free troposphere, not a surface site). A DataFrame whose median
# pressure falls below this almost certainly carries kPa values that were
# never converted to the hPa the model expects -- the conversion that file
# loading applies but direct DataFrame assignment historically bypassed
# (gh#1537).
_PRESSURE_HPA_FLOOR = 300.0


def read_suews(path_suews_file: str) -> pd.DataFrame:
    """read in SUEWS input/output file as datetime-aware DataFrame.

    Parameters
    ----------
    path_suews_file : str
        a string that can be converted into a valid path to SUEWS file.

    Returns
    -------
    pd.DataFrame
        datetime-aware DataFrame
    """

    path_suews_file = Path(path_suews_file).resolve()
    df_raw = pd.read_csv(
        path_suews_file,
        sep=r"\s+",  # any whitespace
        comment="!",
        on_bad_lines="error",
    )
    df_suews = set_index_dt(df_raw)
    return df_suews


def read_forcing(path_suews_file: str, tstep_mod=300) -> pd.DataFrame:
    """Read in SUEWS forcing files as DataFrame ready for SuPy simulation.

    Parameters
    ----------
    path_suews_file : str
        a string that represents wildcard pattern can locate SUEWS forcing files,
        which should follow `SUEWS convention
        <https://docs.suews.io/stable/inputs/tables/met_input.html>`_.

    tstep_mod: int or None, optional
        time step [s] for resampling, by default 300.
        If `None`, resampling will be skipped.

    Returns
    -------
    pd.DataFrame
        datetime-aware DataFrame

    See Also
    --------
    supy.SUEWSForcing.from_file : OOP interface with validation features
    """
    path_suews_file = Path(path_suews_file)
    path_input = path_suews_file.parent
    str_pattern = path_suews_file.name

    df_forcing_raw = load_SUEWS_Forcing_met_df_pattern(path_input, str_pattern)
    return resample_forcing_df(df_forcing_raw, tstep_mod=tstep_mod)


def resample_forcing_df(df_forcing_raw: pd.DataFrame, tstep_mod=300) -> pd.DataFrame:
    """Align a datetime-indexed forcing DataFrame to the model timestep.

    Shared by :func:`read_forcing` (file loading) and
    :func:`prepare_dataframe_forcing` (in-memory DataFrame loading) so both
    entry points infer the input timestep and resample identically. The
    input timestep is read from the index spacing; resampling only happens
    when the model timestep is finer than the input, matching the historical
    file-loading behaviour.

    Parameters
    ----------
    df_forcing_raw : pandas.DataFrame
        Datetime-indexed forcing data with canonical column names.
    tstep_mod : int or None, optional
        Model timestep [s] to resample to, by default 300. If ``None``,
        resampling is skipped (the frequency is still set on the index).

    Returns
    -------
    pandas.DataFrame
        Forcing aligned to ``tstep_mod`` (or unchanged if no resampling was
        required).
    """
    tstep_met_in = (
        df_forcing_raw.index.to_series().diff().iloc[-1] / pd.Timedelta("1s")
    )
    df_forcing_raw = df_forcing_raw.asfreq(f"{tstep_met_in:.0f}s")

    df_forcing = df_forcing_raw

    # resampling only when necessary
    if tstep_mod is not None and tstep_mod < tstep_met_in:
        df_forcing = to_nan(df_forcing_raw)
        df_forcing = resample_forcing_met(
            df_forcing, tstep_met_in, tstep_mod, kdownzen=0
        )
        df_forcing = from_nan(df_forcing)

    return df_forcing


def prepare_dataframe_forcing(
    df_forcing: pd.DataFrame, tstep_mod: int = 300
) -> pd.DataFrame:
    """Normalise an in-memory forcing DataFrame for SUEWS simulation.

    Applies the same timestep alignment that file loading performs, after
    validating that the DataFrame is already in the model-ready canonical
    form. Unlike file loading, no column renaming or unit conversion is
    applied: an in-memory DataFrame has no file convention to canonicalise
    against, so it must already match the contract that
    :func:`read_forcing`, :class:`supy.SUEWSForcing`, and
    :attr:`supy.SUEWSSimulation.forcing` produce -- in particular, pressure
    in hPa. Temporal columns (``iy``, ``id``, ``it``, ``imin``, ``isec``)
    are reasserted from the index so they stay consistent after any
    resampling (gh#1537).

    Parameters
    ----------
    df_forcing : pandas.DataFrame
        Datetime-indexed forcing data with canonical column names and
        model-ready units.
    tstep_mod : int, optional
        Model timestep [s] to resample to, by default 300.

    Returns
    -------
    pandas.DataFrame
        Forcing aligned to ``tstep_mod`` with consistent temporal columns.

    Raises
    ------
    ValueError
        If the DataFrame lacks a :class:`pandas.DatetimeIndex`, is missing
        required meteorological columns, has fewer than two timestamps, or
        carries pressure values that look like kPa rather than hPa.
    """
    if not isinstance(df_forcing.index, pd.DatetimeIndex):
        raise ValueError(
            "DataFrame forcing must have a pandas.DatetimeIndex; got "
            f"{type(df_forcing.index).__name__}. Use supy.util.read_forcing "
            "or supy.SUEWSForcing.from_file to build a model-ready DataFrame."
        )

    missing = [c for c in BASELINE_FORCING_COLUMNS if c not in df_forcing.columns]
    if missing:
        raise ValueError(
            f"DataFrame forcing is missing required columns: {missing}. "
            "Forcing is matched by column name; expected canonical names "
            f"include {list(BASELINE_FORCING_COLUMNS)}."
        )

    if df_forcing.index.size < 2:
        raise ValueError(
            "DataFrame forcing must have at least two timestamps so the "
            "input timestep can be inferred; "
            f"got {df_forcing.index.size}."
        )

    # Pressure must be in hPa (the model unit). File loading converts kPa
    # -> hPa; an in-memory DataFrame is taken as already converted. Catch
    # the common "forgot to convert" mistake with a clear, early error
    # rather than letting it surface later as a generic out-of-range check.
    ser_pres = df_forcing["pres"]
    ser_pres_valid = ser_pres[ser_pres > FORCING_OPTIONAL_FILL / 2]
    if ser_pres_valid.size and ser_pres_valid.median() < _PRESSURE_HPA_FLOOR:
        raise ValueError(
            "DataFrame forcing pressure (`pres`) looks like kPa, not hPa "
            f"(median {ser_pres_valid.median():.1f}). SUEWS expects surface "
            "pressure in hPa; multiply by 10 if your data are in kPa, or use "
            "supy.util.read_forcing, which applies the conversion."
        )

    df_forcing = resample_forcing_df(df_forcing.copy(), tstep_mod=tstep_mod)

    # Reassert temporal columns from the (possibly resampled) index so they
    # stay consistent with the data -- in particular `isec`, which a
    # hand-built DataFrame may omit (gh#1537).
    df_forcing["iy"] = df_forcing.index.year
    df_forcing["id"] = df_forcing.index.dayofyear
    df_forcing["it"] = df_forcing.index.hour
    df_forcing["imin"] = df_forcing.index.minute
    df_forcing["isec"] = df_forcing.index.second
    complete_dt_columns = [*BASELINE_DATETIME_FORCING_COLUMNS, "isec"]
    df_forcing[complete_dt_columns] = df_forcing[complete_dt_columns].astype(
        np.int64
    )

    logger_supy.debug(
        "Prepared DataFrame forcing: %d rows at tstep %ss",
        len(df_forcing),
        tstep_mod,
    )
    return df_forcing
