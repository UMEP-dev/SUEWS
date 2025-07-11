# -*- coding: utf-8 -*-
"""
Created on Mon Dec 17 11:13:44 2018

Authors:
George Meachim,
Ting Sun
"""

import numpy as np
import pandas as pd

from ._plot import plot_comp, plot_day_clm
from .._env import logger_supy


# Linear fitting of QS, QN, deltaQN/dt (entire timeseries)
def derive_ohm_coef(ser_QS, ser_QN):
    """
    A function to linearly fit two independant variables to a dependent one.

    Parameters
    ----------
    ser_QS : pd.Series
        The dependent variable QS (Surface heat storage).
    ser_QN : pd.Series
        The first independent variable (Net all wave radiation).

    Returns
    -------
    Tuple
        a1, a2 coefficients and a3 (intercept)
    """

    from sklearn.linear_model import LinearRegression

    # derive dt in hours
    dt_hr = ser_QN.index.freq / pd.Timedelta("1H")

    # Calculate difference between neighbouring QN values
    ser_delta_QN_dt = ser_QN.diff() / dt_hr

    # Drop NaNs and infinite values
    ser_QS = ser_QS.replace([np.inf, -np.inf], np.nan).dropna(how="all")
    ser_QN = ser_QN.loc[ser_QS.index]
    ser_delta_QN_dt = ser_delta_QN_dt.loc[ser_QS.index]

    # Create DataFrame with regression quantities and rename cols
    frames = [ser_QS, ser_QN, ser_delta_QN_dt]
    regression_df = pd.concat(frames, axis=1)
    regression_df.columns = ["QS", "QN", "delta_QN_dt"]

    # Reindex after dropping NaNs
    regression_df.reset_index(drop=True, inplace=True)
    regression_df.fillna(regression_df.mean(), inplace=True)

    feature_cols = ["QN", "delta_QN_dt"]

    X = regression_df[feature_cols].replace([np.inf, -np.inf], np.nan).dropna(how="all")
    y = regression_df.QS

    lm = LinearRegression()
    lm.fit(X, y)

    a1 = lm.coef_[0]
    a2 = lm.coef_[1]
    a3 = lm.intercept_

    return a1, a2, a3


def replace_ohm_coeffs(df_state, coefs, land_cover_type):
    """
    This function takes as input parameters the model initial state DataFrame,
    the new ohm coefficients as calculated by performing linear regression on
    AMF Obs and the land cover type for which they were calculated.

    Parameters
    ----------
    df_state : pandas.DataFrame
        Model state dataframe used in supy
    coefs : tuple
        new a1, a2, a3 coefficients to replace those in `df_state`;
        note:
        1. the format should be (a1, a3, a3);
        2. any of a1/a2/a3 can be either one numeric value or a list-like structure of four values for SW/SD/WW/WD conditions indicated by `ohm_threshsw` and `ohm_threshwd`.
    land_cover_type : str
        one of seven SUEWS land cover types, can be any of
        1. {"Paved","Bldgs","EveTr","DecTr","Grass","BSoil","Water"} (string case does NOT matter); or
        2. an integer between 0 to 6.

    Returns
    -------
    pandas.DataFrame
        a DataFrame with updated OHM coefficients.
    """

    land_cover_type_dict = {
        "paved": 0,
        "bldgs": 1,
        "evetr": 2,
        "dectr": 3,
        "grass": 4,
        "bsoil": 5,
        "water": 6,
    }

    try:
        lc_index = (
            land_cover_type_dict.get(land_cover_type.lower())
            if isinstance(land_cover_type, str)
            else land_cover_type
        )
    except:
        list_lc = list(land_cover_type_dict.keys())
        logger_supy.error(
            f"land_cover_type must be one of {list_lc}, instead of {land_cover_type}"
        )
    else:
        # Instantiate 4x3 matrix of zeros to put old coeffs
        coef_matrix = np.zeros((4, 3))
        coef_matrix[:, 0] = coefs[0]
        coef_matrix[:, 1] = coefs[1]
        coef_matrix[:, 2] = coefs[2]

        # Copy ohm_coef part of df_state_init
        df_ohm = df_state.loc[:, "ohm_coef"].copy()
        # Reshape values into matrix form
        values_ohm = df_ohm.values.reshape((-1, 8, 4, 3))
        # Get ohm values corresponding to user specified land cover and assign to matrix
        values_ohm[:, lc_index] = coef_matrix
        # Place new ohm values into df_ohm
        df_ohm.loc[:, :] = values_ohm.reshape(df_ohm.shape)
        # Make copy of df_state_init
        df_state_init_copy = df_state.copy()
        # Replace  ohm_coef part of df_state_init with new values
        df_state_init_copy.loc[:, "ohm_coef"] = df_ohm.values

        return df_state_init_copy


def sim_ohm(ser_qn: pd.Series, a1: float, a2: float, a3: float) -> pd.Series:
    """Calculate QS using OHM (Objective Hysteresis Model).

    Parameters
    ----------
    ser_qn : pd.Series
        net all-wave radiation.
    a1 : float
        a1 of OHM coefficients.
    a2 : float
        a2 of OHM coefficients.
    a3 : float
        a3 of OHM coefficients.

    Returns
    -------
    pd.Series
        heat storage flux calculated by OHM.
    """

    # derive delta t in hour
    try:
        dt_hr = ser_qn.index.freq / pd.Timedelta("1h")
    except AttributeError as ex:
        raise RuntimeError("frequency info is missing from input `ser_qn`") from ex

    # Calculate rate of change of Net All-wave radiation
    ser_qn_dt = ser_qn.diff() / dt_hr

    # Derive QS from OBS quantities
    ser_qs = a1 * ser_qn + a2 * ser_qn_dt + a3

    return ser_qs


def compare_heat_storage(ser_qn_obs, ser_qs_obs, a1, a2, a3):
    """This function compares the storage heat flux calculated with AMF
    QN and linear regression coefficients with  that output by SUEWS.
    Input params: QN_Ser_Obs: A series of OBS net all-wave radiation.
                  QS_Ser_SUEWS: A series of SUEWS storage heat flux values.
                  a1, a2, a3: Linear regression coefficients from ohm_linregress_clean.
    Returns: MPL plot of diurnal comparison, MPL plot with 1:1 line and fitted line.
    """

    # calculate qs using OHM
    ser_qs_sim = sim_ohm(ser_qn_obs, a1, a2, a3).rename("Sim")

    # re-organise obs and sim info one dataframe
    ser_qs_obs = ser_qs_obs.rename("Obs")
    df_qs = pd.concat([ser_qs_sim, ser_qs_obs.rename("Obs")], axis=1)
    # Plotting
    plot1 = plot_day_clm(df_qs)
    plot2 = plot_comp(df_qs)

    return plot1, plot2
