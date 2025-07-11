# surface conductance/resistance related functions
import numpy as np
import pandas as pd

from ._atm import cal_des_dta, cal_dq


def cal_rs_obs(qh, qe, ta, rh, pa, ra, method="iPM"):
    """Calculate surface resistance based on observations, notably turbulent fluxes.

    Parameters
    ----------
    qh : numeric
        sensible heat flux [W m-2]
    qe : numeric
        latent heat flux [W m-2]
    ta : numeric
        air temperature [degC]
    rh : numeric
        relative humidity [%]
    pa : numeric
        air pressure [Pa]
    ra : numeric
        aerodynamic resistance [m s-1]
    method: str
        method used to calculate rs, can be one of {'iPM','FG'}, by default 'iPM'
        'iPM': inverted Penman-Monteith
        'FG': flux gradient

    Returns
    -------
    numeric
        Surface resistance based on observations [s m-1]
    """
    from atmosp import calculate as ac

    # choose calculation method
    dict_fun = {
        "iPM": cal_rs_iPM,
        "FG": cal_rs_FG,
    }
    fun = dict_fun[method]

    # actual calculation via the wrapper function
    ser_rs = fun(qh, qe, ta, rh, pa, ra)

    return ser_rs


def cal_rs_iPM(qh, qe, ta, rh, pa, ra):
    """Calculate surface resistance based on observations, notably turbulent fluxes.

    Parameters
    ----------
    qh : numeric
        sensible heat flux [W m-2]
    qe : numeric
        latent heat flux [W m-2]
    ta : numeric
        air temperature [degC]
    rh : numeric
        relative humidity [%]
    pa : numeric
        air pressure [Pa]
    ra : numeric
        aerodynamic resistance [m s-1]

    Returns
    -------
    numeric
        Surface resistance based on observations [s m-1]
    """
    from atmosp import calculate as ac

    # psychrometric constant [Pa K-1] as a function of air pressure
    ser_gamma = 0.665e-3 * pa

    # air density [kg m-3]
    val_rho = 1.27

    # heat capacity of air [J kg-1 K-1]
    val_cp = 1005

    # convert temp from C to K
    ta_K = ta + 273.15

    # slope of es(Ta) curve at Ta
    ser_des_dTa = cal_des_dta(ta_K, pa, dta=1.0)
    #
    arr_e = ac("e", p=pa, T=ta_K, RH=rh)
    arr_es = ac("es", p=pa, T=ta_K)
    arr_vpd = arr_es - arr_e
    #
    ser_rs_1 = (ser_des_dTa / ser_gamma) * (qh / qe - 1) * ra
    ser_rs_2 = val_rho * val_cp * arr_vpd / (ser_gamma * qe)
    ser_rs = ser_rs_1 + ser_rs_2

    try:
        # try to pack as Series
        ser_rs = pd.Series(ser_rs, index=ta_K.index)
    except AttributeError as ex:
        print(ex, "cannot pack into pd.Series")
        pass

    return ser_rs


def cal_rs_FG(qh, qe, ta, rh, pa, ra):
    """Calculate surface resistance based on observations, notably turbulent fluxes.

    Parameters
    ----------
    qh : numeric
        sensible heat flux [W m-2]
    qe : numeric
        latent heat flux [W m-2]
    ta : numeric
        air temperature [degC]
    rh : numeric
        relative humidity [%]
    pa : numeric
        air pressure [Pa]
    ra : numeric
        aerodynamic resistance [m s-1]

    Returns
    -------
    numeric
        Surface resistance based on observations [s m-1]
    """
    from atmosp import calculate as ac
    from ._atm import cal_dens_vap, cal_lat_vap, cal_qa

    # air density [kg m-3]
    val_rho = 1.27

    # heat capacity of air [J kg-1 K-1]
    val_cp = 1005

    # convert temp from C to K
    ta_K = ta + 273.15

    # canopy bulk surface temperature
    tc_K = qh / (val_rho * val_cp) * ra + ta_K

    # actual atmospheric vapour pressure [Pa]
    ser_qa = ac("qv", p=pa, T=ta_K, RH=rh) + rh * 0

    # saturated atmospheric vapour pressure at canopy surface [Pa]
    ser_qs_c = ac("qvs", p=pa, T=tc_K) + rh * 0

    # # vapour pressure deficit [Pa]
    # arr_vpd = arr_es_c - arr_ea

    # specific humidity [kg kg-1]
    # ser_qa = cal_qa(rh, ta_K, pa / 100)

    # latent heat of vapour [J kg-1]
    ser_lv = cal_lat_vap(ser_qa, ta_K, pa / 100) + pa * 0

    # vapour density [kg m-3]
    rho_v = ac("rho", RH=rh, T=ta_K, p=pa) + pa * 0

    ser_et = qe / ser_lv
    ser_rs = rho_v * (ser_qs_c - ser_qa) / ser_et - ra
    print(
        # ser_qa.median(),
        # ta_K.median(),
        # pa.median(),
        # tc_K.median(),
        # rho_v.median(),
        # (ser_qs_c - ser_qa).median(),
        # ser_et.median(),
        # ra.median(),
        # ser_lv.median(),
    )

    try:
        # try to pack as Series
        ser_rs = pd.Series(ser_rs, index=ta_K.index)
    except AttributeError as ex:
        print(ex, "cannot pack into pd.Series")
        pass

    return ser_rs


def cal_gs_obs(qh, qe, ta, rh, pa, ra):
    """Calculate surface conductance based on observations, notably turbulent fluxes.

    Parameters
    ----------
    qh : numeric
        Sensible heat flux [W m-2]
    qe : numeric
        Latent heat flux [W m-2]
    ta : numeric
        Air temperature [degC]
    rh : numeric
        Relative humidity [%]
    pa : numeric
        Air pressure [Pa]

    Returns
    -------
    numeric
        Surface conductance based on observations [mm s-1]
    """
    rs_obs = cal_rs_obs(qh, qe, ta, rh, pa, ra)
    gs_obs = 1e3 / rs_obs
    return gs_obs


def cal_g_lai(lai, g_lai, lai_max):
    """Calculate LAI-related correction coefficient for surface conductance.

    Parameters
    ----------
    lai : numeric
        Leaf area index [m2 m-2]
    g_lai : numeric
        LAI-related correction parameter [-]
    lai_max : numeric
        Maximum LAI [m2 m-2]

    Returns
    -------
    numeric
        LAI-related correction coefficient [-]
    """
    g_lai = lai / lai_max * g_lai
    return g_lai


def cal_g_kd(kd, g_kd, kd_max=1200.0):
    """Calculate solar radiation-related correction coefficient for surface conductance.

    Parameters
    ----------
    kd : numeric
        Incoming solar radiation [W m-2]
    g2 : numeric
        Solar radiation-related correction parameter [-]
    kd_max : numeric, optional
        Maximum incoming solar radiation [W m-2], by default 1200.

    Returns
    -------
    numeric
        Solar radiation-related correction coefficient [-]
    """
    g_kd_nom = kd / (g_kd + kd)
    g_kd_denom = kd_max / (g_kd + kd_max)
    g_kd = g_kd_nom / g_kd_denom
    return g_kd


def cal_g_dq(dq, g_dq_base, g_dq_shape):
    """Calculate air humidity-related correction coefficient for surface conductance.

    Parameters
    ----------
    dq : numeric
        Specific humidity deficit [g kg-1]
    g_dq_base : numeric
        Specific humidity-related correction parameter [-]
    g_dq_shape : numeric
        Specific humidity-related correction parameter [-]

    Returns
    -------
    numeric
        Air humidity-related correction coefficient
    """
    g_dq = g_dq_base + (1 - g_dq_base) * g_dq_shape**dq
    return g_dq


def cal_g_ta(ta_c, g_ta, tl=-10.0, th=55.0):
    """Calculate air temperature-related correction coefficient for surface conductance.

    Parameters
    ----------
    ta_c : numeric
        Air temperature [degC]
    g_ta : numeric
        Air temperature-related correction parameter
    tl : numeric, optional
        Low temperature limit [degC], by default -10.
    th : numeric, optional
        High temperature limit [degC], by default 55.

    Returns
    -------
    numeric
        Air temperature-related correction coefficient
    """

    tc = (th - g_ta) / (g_ta - tl)
    # set a threshold for avoiding numeric difficulty
    tc = np.min([tc, 20])
    # g_ta = ((ta_c-tl)*(th-ta_c)**tc)/((g_ta-tl)*(th-g_ta)**tc)
    g_ta_nom = (ta_c - tl) * np.power((th - ta_c), tc)
    g_ta_denom = (g_ta - tl) * np.power((th - g_ta), tc)
    g_ta = g_ta_nom / g_ta_denom

    return g_ta


def cal_g_smd(smd, g_smd, wp_smd):
    """Calculate soil moisture-related correction coefficient for surface conductance.

    Parameters
    ----------
    smd : numeric
        Soil moisture deficit [mm].
    g_smd : numeric
        Soil moisture-related correction parameter.
    wp_smd : numeric, optional
        Wilting point indicated by soil moisture deficit [mm]

    Returns
    -------
    numeric
        Soil moisture-related correction coefficient
    """
    # Wilting point calculated following SUEWS
    # wp = wp_smd / g_smd

    g_smd_nom = 1 - np.exp(g_smd * (smd - wp_smd))
    g_smd_denom = 1 - np.exp(g_smd * (0 - wp_smd))
    g_smd = g_smd_nom / g_smd_denom
    return g_smd


def cal_gs_suews(
    kd, ta_c, rh, pa, smd, lai, g_cst, g_max, lai_max, wp_smd, debug=False
):
    """Model surface conductance/resistance using phenology and atmospheric forcing conditions.

    Parameters
    ----------
    kd : numeric
        Incoming solar radiation [W m-2]
    ta_c : numeric
        Air temperature [degC]
    rh : numeric
        Relative humidity [%]
    pa : numeric
        Air pressure [Pa]
    smd : numeric
        Soil moisture deficit [mm]
    lai : numeric
        Leaf area index [m2 m-2]
    g_cst : size-6 array
        Parameters to determine surface conductance/resistance:
        g_lai (LAI related), g_kd (solar radiation related),
        g_dq_base (humidity related), g_dq_shape (humidity related),
        g_ta (air temperature related),
        g_smd (soil moisture related)
    g_max : numeric
        Maximum surface conductance [mm s-1]
    lai_max : numeric
        Maximum LAI [m2 m-2]
    wp_smd : numeric
        Wilting point indicated as soil moisture deficit [mm]

    Returns
    -------
    numeric
        Modelled surface conductance [mm s-1]
    """
    from atmosp import calculate as ac

    # broadcast g_lai – g_smd
    # print('g_cst', g_cst)
    g_lai, g2, g_dq_base, g_dq_shape, g_ta, g_smd = g_cst
    # print(g_lai, g2, g_dq_base, g_dq_shape, g_ta, g_smd)
    # lai related
    g_lai = cal_g_lai(lai, g_lai, lai_max)
    # print('g_lai', g_lai)

    # kdown related
    g_kd = cal_g_kd(kd, g2)
    # print('g_kd', g_kd)
    # dq related
    ta_k = ta_c + 273.15
    dq = cal_dq(rh, ta_c, pa / 100) * 1000 + rh * 0
    g_dq = cal_g_dq(dq, g_dq_base, g_dq_shape)
    # print('g_dq', g_dq)
    # ta related
    ta_c = ta_k - 273.15
    g_ta = cal_g_ta(ta_c, g_ta)
    # print('g_ta', g_ta)
    # smd related
    g_smd = cal_g_smd(smd, g_smd, wp_smd)
    # print('g_smd', g_smd)
    # combine all corrections
    gs_c = g_lai * g_kd * g_dq * g_ta * g_smd
    gs = g_max * gs_c

    if debug:
        # pack results into a dataframe
        df_gs = pd.concat(
            {
                "gs_mod": pd.Series(gs),
                "gs_c": pd.Series(gs_c),
                "g_lai": pd.Series(g_lai),
                "g_kd": pd.Series(g_kd),
                "g_dq": pd.Series(g_dq),
                "g_ta": pd.Series(g_ta),
                "g_smd": pd.Series(g_smd),
            },
            axis=1,
        )

        return df_gs
    else:
        return gs


def calib_g(
    df_fc_suews,
    ser_ra,
    g_max,
    lai_max,
    wp_smd,
    method="cobyla",
    prms_init=None,
    debug=False,
):
    """Calibrate parameters for modelling surface conductance over vegetated surfaces using `LMFIT <https://lmfit.github.io/lmfit-py/model.html>`.

    Parameters
    ----------
    df_fc_suews : pandas.DataFrame
        DataFrame in `SuPy forcing <https://supy.readthedocs.io/en/latest/data-structure/df_forcing.html>`_ format
    ser_ra: pandas.Series
        Series with RA, aerodynamic resistance, [s m-1]
    g_max : numeric
        Maximum surface conductance [mm s-1]
    lai_max : numeric
        Maximum LAI [m2 m-2]
    wp_smd : numeric
        Wilting point indicated as soil moisture deficit [mm]
    method: str, optional
        Method used in minimisation by `lmfit.minimize`: details refer to its `method<lmfit:minimize>`.
    prms_init: lmfit.Parameters, optional
        Initial parameters for calibration
    debug : bool, optional
        Option to output final calibrated `ModelResult <lmfit:ModelResult>`, by default False

    Returns
    -------
    dict, or `ModelResult <lmfit:ModelResult>` if `debug==True`
        1. dict: {parameter_name -> best_fit_value}
        2. `ModelResult`

        Note:
            Parameters for surface conductance:
            g_lai (LAI related), g2 (solar radiation related),
            g_dq_base (humidity related), g_dq_shape (humidity related),
            g_ta (air temperature related),
            g_smd (soil moisture related)

    Note
    ----
    For calibration validity, turbulent fluxes, QH and QE, in `df_fc_suews` should ONLY be observations, i.e., interpolated values should be avoided.
    To do so, please place `np.nan` as missing values for QH and QE.

    """
    from lmfit import Model, Parameter, Parameters

    list_var_sel = ["qh", "qe", "Tair", "RH", "pres", "kdown", "xsmd", "lai"]
    df_obs = df_fc_suews[list_var_sel].copy().dropna()

    # convert to Pa
    df_obs.pres *= 100

    gs_obs = cal_gs_obs(
        df_obs.qh, df_obs.qe, df_obs.Tair, df_obs.RH, df_obs.pres, ser_ra
    )

    def func_fit_g(
        kd, ta, rh, pa, smd, lai, g_lai, g_kd, g_dq_base, g_dq_shape, g_ta, g_smd
    ):
        gs = cal_gs_suews(
            kd,
            ta,
            rh,
            pa,
            smd,
            lai,
            [g_lai, g_kd, g_dq_base, g_dq_shape, g_ta, g_smd],
            g_max,
            lai_max,
            wp_smd,
        )
        return gs

    gmodel = Model(
        func_fit_g,
        independent_vars=["lai", "kd", "ta", "rh", "pa", "smd"],
        param_names=["g_lai", "g_kd", "g_dq_base", "g_dq_shape", "g_ta", "g_smd"],
    )
    if prms_init is None:
        print("Preset parameters will be loaded!")
        print("Please use with caution.")
        prms = Parameters()
        dict_prms_init = {
            "lai": 3.5,
            "kd": 50,
            "q1": 0.1,
            "q2": 0.7,
            "ta": 25,
            "smd": 0.05,
        }
        list_g = (
            Parameter(f"g_{var}", val, True, 0, None, None, None)
            for var, val in dict_prms_init.items()
        )
        prms.add_many(*list_g)
        # set specific bounds:
        # g_lai: LAI related
        prms["g_lai"].set(min=0, max=10)

        prms["g_kd"].set(min=0, max=300)
        # g_dq_base, g_dq_shape: specific humidity related
        prms["g_dq_base"].set(min=0, max=1)
        prms["g_dq_shape"].set(min=0, max=1)
        # g_ta: within reasonable temperature ranges
        prms["g_ta"].set(min=-10, max=55)
        # g_smd: within sensitive ranges of SMD
        prms["g_smd"].set(min=0.02, max=0.1)
    else:
        print("User provided parameters are loaded!")
        prms = prms_init

    # pack into a DataFrame for filtering out nan
    df_fit = pd.concat([gs_obs.rename("gs_obs"), df_obs], axis=1).dropna()

    res_fit = gmodel.fit(
        df_fit.gs_obs,
        kd=df_fit.kdown,
        ta=df_fit.Tair,
        rh=df_fit.RH,
        pa=df_fit.pres,
        smd=df_fit.xsmd,
        lai=df_fit.lai,
        params=prms,
        # useful ones: ['nelder', 'powell', 'cg', 'cobyla', 'bfgs', 'trust-tnc']
        method=method,
        #     nan_policy='omit',
        verbose=True,
    )

    # provide full fitted model if debug == True otherwise only a dict with best fit parameters
    res = res_fit if debug else res_fit.best_values

    return res


def fit_g_ta(ser_ta, ser_gs):
    from lmfit import Model, Parameter, Parameters

    model_g_ta = Model(
        cal_g_ta,
        independent_vars=["ta_c"],
        param_names=["g_ta", "tl", "th"],
    )
    prms = Parameters()
    prm_g_ta = Parameter(
        "g_ta", ser_ta.median(), vary=True, min=ser_ta.min(), max=ser_ta.max()
    )
    prm_tl = Parameter("tl", min=ser_ta.min(), vary=False)
    prm_th = Parameter("th", ser_ta.max(), vary=False)
    prms.add_many(prm_g_ta, prm_tl, prm_th)

    res_fit = model_g_ta.fit(ser_gs / ser_gs.max(), ta_c=ser_ta, params=prms)
    return res_fit


def fit_g_smd(ser_smd, ser_gs, wp_smd):
    from lmfit import Model, Parameter, Parameters

    model_g_smd = Model(
        cal_g_smd,
        independent_vars=["smd"],
        param_names=["g_smd", "wp_smd"],
    )
    prms = Parameters()
    prm_g_smd = Parameter("g_smd", 0.02, vary=True, min=1e-4, max=0.5)
    prm_wp_smd = Parameter("wp_smd", wp_smd, vary=False)
    prms.add_many(prm_g_smd, prm_wp_smd)

    res_fit = model_g_smd.fit(ser_gs / ser_gs.max(), smd=ser_smd, params=prms)
    return res_fit


def fit_g_kd(ser_kd, ser_gs):
    from lmfit import Model, Parameter, Parameters

    model_g_kd = Model(
        cal_g_kd,
        independent_vars=["kd"],
        param_names=["g_kd"],
    )
    prms = Parameters()
    prm_g_kd = Parameter("g_kd", 100, vary=True, min=10, max=300)
    prms.add(prm_g_kd)
    res_fit = model_g_kd.fit(ser_gs / ser_gs.max(), kd=ser_kd, params=prms)
    return res_fit


def fit_g_dq(ser_dq, ser_gs):
    from lmfit import Model, Parameter, Parameters

    model_g_dq = Model(
        cal_g_dq,
        independent_vars=["dq"],
        param_names=["g_dq_base", "g_dq_shape"],
    )
    prms = Parameters()
    prm_g_dq_base = Parameter(
        "g_dq_base",
        ser_gs.min() / ser_gs.max(),
        vary=False,
    )
    prm_g_dq_shape = Parameter("g_dq_shape", 0.1, vary=True, min=0.01, max=0.95)
    prms.add_many(prm_g_dq_base, prm_g_dq_shape)
    res_fit = model_g_dq.fit(ser_gs / ser_gs.max(), dq=ser_dq, params=prms)
    return res_fit


# NOAH parameterisations
# Kdown
def cal_g_kd_noah(kd, rgl=30, lai=2, rc_min=200.0, rc_max=5000):
    f = 0.55 * kd / rgl * 2 / lai
    g_kd = ((rc_min / rc_max) + f) / (1 + f)
    return g_kd


# humidity
def cal_g_dq_noah(dq, hs=31):
    """Calculate air humidity-related correction coefficient for surface conductance.

    Parameters
    ----------
    dq : numeric
        Specific humidity deficit [g kg-1]
    hs : numeric
        Specific humidity-related correction parameter [kg g-1]

    Returns
    -------
    numeric
        Air humidity-related correction coefficient
    """
    g_dq = 1 / (1 + hs * (dq))
    return g_dq


# smc
def cal_g_swc_noah(swc, q_wilt, q_ref):
    """Calculate soil moisture-related correction coefficient for surface conductance.

    Parameters
    ----------
    swc : numeric
        Soil water content (volumetric) [m3 m-3].
    q_wilt : numeric
        wilting point [m3 m-3].
    q_ref : numeric
        reference point [m3 m-3].

    Returns
    -------
    numeric
        Soil moisture-related correction coefficient
    """
    # Wilting point calculated following SUEWS
    # wp = wp_smd / g_smd

    g_swc = (swc - q_wilt) / (q_ref - q_wilt)
    return g_swc


def cal_g_ta_noah(ta_c, b1=0.0016, tref=298 - 273.15):
    """Calculate air temperature-related correction coefficient for surface conductance.

    Parameters
    ----------
    ta_c : numeric
        Air temperature [degC]
    b1 : numeric
        Air temperature-related correction parameter
    tref : numeric, optional
        Low temperature limit [K]

    Returns
    -------
    numeric
        Air temperature-related correction coefficient
    """

    g_ta = 1 - b1 * (tref - ta_c) ** 2

    return g_ta


def deriv_g_ta_noah(b1):
    ar_ta = np.linspace(-10, 50, 100)
    ser_ta = pd.Series(ar_ta, index=ar_ta)
    ser_g = cal_g_ta_noah(ser_ta, b1)
    res_fit = fit_g_ta(ser_ta, ser_g)
    return res_fit


def deriv_g_dq_noah(hs):
    ar_dq = np.linspace(0, 20, 100)
    ser_dq = pd.Series(ar_dq * 1e-3, index=ar_dq)
    ser_g = cal_g_dq_noah(ser_dq, hs)
    res_fit = fit_g_dq(ser_dq * 1e3, ser_g)
    return res_fit


def deriv_g_kd_noah(rgl, lai, rc_min):
    ar_kd = np.linspace(10, 1200, 100)
    ser_kd = pd.Series(ar_kd, index=ar_kd)
    ser_g = cal_g_kd_noah(ser_kd, rgl, lai, rc_min)
    res_fit = fit_g_kd(ser_kd, ser_g)
    return res_fit


def deriv_g_smd_noah(dsoil, q_wilt, q_ref):
    """Calculate soil moisture-related correction coefficient for surface conductance.

    Parameters
    ----------
    dsoil : numeric
        soil depth [mm].
    q_wilt : numeric
        wilting point [m3 m-3].
    q_ref : numeric
        reference point [m3 m-3].

    Returns
    -------
    model fit result
    """

    ar_swc = np.linspace(q_wilt, q_ref, 100)
    ser_swc = pd.Series(ar_swc, index=ar_swc)
    ser_g = cal_g_swc_noah(ser_swc, q_wilt, q_ref)
    ser_smd = dsoil * (q_ref - ser_swc)
    wp_smd = dsoil * (q_ref - q_wilt)
    res_fit = fit_g_smd(ser_smd, ser_g, wp_smd)
    return res_fit
