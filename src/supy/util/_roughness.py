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
    ser_ws: pd.Series
        wind speed [m/s]
    z_meas
        measurement height [m]
    h_sfc
        vegetation height [m]

    Returns
    -------
    ser_ws_neutral: pd.Series
        observation time series of WS (Neutral conditions)
    ser_ustar_neutral: pd.Series
        observation time series of u* (Neutral conditions)
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


# calculate z0 and zd using mutli-objective optimisation
def cal_z0zd_mo(
    ser_qh,
    ser_ustar,
    ser_ta_c,
    ser_rh_pct,
    ser_pres_hpa,
    ser_ws,
    z_meas,
    h_sfc,
):
    """Calculates surface roughness and zero plane displacement height using mutli-objective optimisation.
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
    z_meas
        measurement height in m
    h_sfc
        vegetation height in m

    Returns
    -------
    z0
        surface roughness length for momentum
    zd
        zero displacement height
    ser_ws_neutral: pd.Series
        observation time series of WS (Neutral conditions)
    ser_ustar_neutral: pd.series
        observation time series of u* (Neutral conditions)
    """

    from platypus.core import Problem
    from platypus.types import Real, random
    from platypus.algorithms import NSGAIII

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
    def func_uz(params):
        z0 = params[0]
        d = params[1]
        z = z_meas
        k = 0.4
        uz = (ser_ustar_neutral / k) * np.log((z - d) / z0)  # logarithmic law

        o1 = abs(1 - np.std(uz) / np.std(ser_ws_neutral))  # objective 1: normalized STD
        # objective 2: normalized MAE
        o2 = np.mean(abs(uz - ser_ws_neutral)) / (np.mean(ser_ws_neutral))

        return [o1, o2], [uz.min(), d - z0]

    problem = Problem(2, 2, 2)
    problem.types[0] = Real(0, 10)  # bounds for first parameter (z0)
    problem.types[1] = Real(0, h_sfc)  # bounds for second parameter (zd)

    problem.constraints[0] = ">=0"  # constrain for first parameter
    problem.constraints[1] = ">=0"  # constrain for second parameter

    problem.function = func_uz
    random.seed(12345)
    algorithm = NSGAIII(problem, divisions_outer=50)
    algorithm.run(30000)

    z0s = []
    ds = []
    os1 = []
    os2 = []
    # getting the solution vaiables
    for s in algorithm.result:
        z0s.append(s.variables[0])
        ds.append(s.variables[1])
        os1.append(s.objectives[0])
        os2.append(s.objectives[1])
    # getting the solution associated with minimum obj2 (can be changed)
    idx = os2.index(min(os2, key=lambda x: abs(x - np.mean(os2))))
    z0 = z0s[idx]
    zd = ds[idx]

    return z0, zd


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
