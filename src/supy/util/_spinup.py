from typing import Tuple, Union
import numpy as np
import pandas as pd


def get_spinup_state(
    df_state: pd.DataFrame,
    df_forcing: pd.DataFrame,
    start_analysis: str,
    spinup_days=365,
    save_spinup: bool = False,
) -> Union[pd.DataFrame, Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]]:
    """
    Get the spin-up state for the model.

    Parameters
    ----------
    df_state : pd.DataFrame
        Dataframe containing the initial state of the model.
    df_forcing : pd.DataFrame
        Dataframe containing the forcing data for the model.
    start_analysis : str
        Start date of the analysis period in the format 'YYYY-MM-DD'.
    spinup_days : int, optional
        Number of days to spin up the model, by default 365.
    save_spinup : bool, optional
        Whether to output the spin-up output and state as part of the output for debugging, by default False.

    Returns
    -------
    pd.DataFrame
        Dataframe containing the spin-up state for the model if `save_spinup` is False.
    Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]
        A tuple of dataframes containing the spin-up output, state, and forcing data if `save_spinup` is True.
    """
    # this function is inspired by the spinup function in the supy-lcz project led by @matthiasdemuzere
    from .._supy_module import _run_supy  # import this here to avoid circular import

    # if df_forcing is shorter than one year, raise error
    len_forcing = df_forcing.index.max() - df_forcing.index.min()
    if len_forcing < pd.Timedelta(days=364):
        raise ValueError(
            "The forcing dataframe is shorter than one year, spin-up not possible!"
        )

    # if df_forcing is longer than one year but shorter than two years
    # then duplicate the forcing dataframe to two years
    if len_forcing < pd.Timedelta(days=365 * 2):
        n_year = np.ceil(spinup_days / 365).astype(int)
        print(
            f"The forcing dataframe is longer than one year but shorter than two years, duplicating the forcing dataframe to {n_year} years"
        )
        df_forcing_spinup = pd.concat([df_forcing] * n_year, ignore_index=True)
        df_forcing_spinup.index = pd.date_range(
            df_forcing.index.max()
            - (df_forcing_spinup.index.size - 1) * df_forcing.index.freq,
            df_forcing.index.max(),
            freq=df_forcing.index.freq,
        )

    # print(df_forcing_spinup.index.min(), df_forcing_spinup.index.max())

    # duplicate the state dataframe for later output
    df_state_spinup = df_state.copy()

    # Cycle over all fractions (ft = fractiontype), each time set to 100%
    # construct a new state, run the model, and store the state after spin-up
    # expand the state to seven rows, one for each fraction
    df_state_lc_init = pd.concat(
        [df_state.iloc[[0]]] * 7, ignore_index=True, names=["grid"]
    )
    # recover index name
    df_state_lc_init.index.rename("grid", inplace=True)

    # set the fractions to 100% for each fraction with a 7*7 matrix with 1 on the diagonal
    df_state_lc_init["sfr_surf"] = np.eye(7)

    # set porosity to 0.5, Issue #78
    df_state_lc_init["porosity_id"] = 0.5
    df_state_lc_init["pormax_dec"] = 0.9
    df_state_lc_init["pormin_dec"] = 0.1

    # construct forcing for spin-up
    end_spinup = pd.to_datetime(start_analysis)
    start_spinup = end_spinup - pd.Timedelta(days=spinup_days)

    print(f"==> starting spin-up simulations from {start_spinup} to {end_spinup}!")

    # Slice forcing
    df_forcing_spinup = df_forcing_spinup.loc[start_spinup:end_spinup]

    # Run supy
    df_output_lc, df_state_lc = _run_supy(
        df_forcing=df_forcing_spinup,
        df_state_init=df_state_lc_init,
    )

    print(f"==> spin-up simulations completed, assigning spin-up states!")
    # retrieve the state after spin-up
    idx_spinup = df_forcing_spinup.index[-1] + df_forcing_spinup.index.freq

    # transfer the spun-up state to the state dataframe for simulation
    for var in ["lai_id", "soilstore_surf", "state_surf", "alb"]:
        df_state_spinup.loc[:, var] = np.vstack(
            [df_state_lc.loc[idx_spinup, var].values.diagonal()] * len(df_state)
        )
    # properties specific to deciduous trees
    list_var_dectree = ["porosity_id", "decidcap_id"]
    # porosity
    for var in list_var_dectree:
        df_state_spinup.loc[:, var] = df_state_lc.loc[
            (idx_spinup, 3),
            var,
        ].values[0]

    if save_spinup:
        return df_state_spinup, df_output_lc, df_state_lc
    else:
        return df_state_spinup
