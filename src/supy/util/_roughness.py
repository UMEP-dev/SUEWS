import numpy as np

from .._env import logger_supy
from ._atm import cal_cp, cal_Lob


def cal_neutral(
    ser_qh,
    ser_ustar,
    ser_ta_c,
    ser_rh_pct,
    ser_pres_hpa,
    ser_ws,
    z_meas,
    h_sfc,
):
    """Calculates the rows associated with neutral condition (threshold=0.01)


    Parameters
    ----------
    ser_qh : pandas.DataFrame
        sensible heat flux [W/m^2]
    ser_ustar : pandas.Series
        friction velocity [m/s]
    ser_ta_c : pandas.Series
        air temperature [°C]
    ser_rh_pct : pandas.Series
        relative humidity [%]
    ser_pres_hpa : pandas.Series
        air pressure [hPa]
    ser_ws : pandas.Series
        wind speed [m/s]
    z_meas : float
        measurement height [m]
    h_sfc : float
        vegetation height [m]

    Returns
    -------
    tuple
        - ser_ws_neutral: pandas.Series - observation time series of WS (Neutral conditions)
        - ser_ustar_neutral: pandas.Series - observation time series of u* (Neutral conditions)
    """

    # calculate Obukhov length
    # ser_Lob = df_val.apply(
    #     lambda ser: cal_Lob(ser.H, ser.USTAR, ser.TA, ser.RH, ser.PA * 10), axis=1
    # )
    ser_Lob = cal_Lob(ser_qh, ser_ustar, ser_ta_c, ser_rh_pct, ser_pres_hpa)

    # zero-plane displacement: estimated using rule f thumb `d=0.7*h_sfc`

    z_d = 0.7 * h_sfc

    if z_d >= z_meas:
        logger_supy.exception(
            "vegetation height is greater than measuring height. Please fix this before continuing ..."
        )

    # calculate stability scale
    ser_zL = (z_meas - z_d) / ser_Lob

    # determine periods under quasi-neutral conditions
    limit_neutral = 0.01
    ind_neutral = ser_zL.between(-limit_neutral, limit_neutral)

    ind_neutral = ind_neutral[ind_neutral].index

    # df_sel = df_val.loc[ind_neutral.index, ["WS", "USTAR"]].dropna()
    ser_ustar_neutral = ser_ustar.loc[ind_neutral]
    ser_ws_neutral = ser_ws.loc[ind_neutral]

    return ser_ws_neutral, ser_ustar_neutral


# calculate z0 and d using curve fitting
def cal_z0zd(
    ser_qh,
    ser_ustar,
    ser_ta_c,
    ser_rh_pct,
    ser_pres_hpa,
    ser_ws,
    z_meas,
    h_sfc,
    debug=False,
):
    """Calculates surface roughness and zero plane displacement height.
    Refer to https://suews-parameters-docs.readthedocs.io/en/latest/steps/roughness-SuPy.html for example

    Parameters
    ----------
    ser_qh: pd.DataFrame
        sensible heat flux [W/m^2]
    ser_ustar: pd.Series
        friction velocity [m/s]
    ser_ta_c: pd.Series
        air temperature [°C]
    ser_rh_pct: pd.Series
        relative humidity [%]
    ser_pres_hpa: pd.Series
        air pressure [hPa]
    z_meas: number
        measurement height in m
    h_sfc: number
        vegetation height in m
    debug : bool, optional
        Option to output final calibrated `ModelResult <lmfit:ModelResult>`, by default False


    Returns
    -------
    z0
        surface roughness length for momentum
    zd
        zero displacement height
    """

    from lmfit import Model, Parameter, Parameters

    # Calculates rows related to neutral conditions
    ser_ws_neutral, ser_ustar_neutral = cal_neutral(
        ser_qh,
        ser_ustar,
        ser_ta_c,
        ser_rh_pct,
        ser_pres_hpa,
        ser_ws,
        z_meas,
        h_sfc,
    )

    # function to optimize
    def cal_uz_neutral(ustar_ntrl, z0, zd, z=z_meas, k=0.4):
        # logarithmic law
        uz = (ustar_ntrl / k) * np.log((z - zd) / z0)
        return uz

    model_uz_neutral = Model(
        cal_uz_neutral,
        independent_vars=["ustar_ntrl"],
        param_names=["z0", "zd"],
    )
    prms = Parameters()
    prm_z0 = Parameter(
        "z0",
        h_sfc * 0.1,
        vary=True,
        min=0.01 * h_sfc,
        max=0.95 * h_sfc,
    )
    prm_zd = Parameter(
        "zd",
        h_sfc * 0.7,
        vary=True,
        min=0.01 * h_sfc,
        max=0.95 * h_sfc,
    )
    prms.add_many(prm_z0, prm_zd)
    try:
        res_fit = model_uz_neutral.fit(
            ser_ws_neutral,
            ustar_ntrl=ser_ustar_neutral,
            params=prms,
        )
        # provide full fitted model if debug == True otherwise only a dict with best fit parameters
        res = res_fit if debug else res_fit.best_values
        if isinstance(res, dict):
            return res["z0"], res["zd"]

        return res
    except Exception as e:
        print(e)
        print("Fitting failed! Use 0.1h and 0.7h for z0 and zd, respectively")
        return h_sfc * 0.1, h_sfc * 0.7
