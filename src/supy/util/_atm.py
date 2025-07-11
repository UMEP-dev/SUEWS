# from scipy.optimize import least_squares
import numpy as np
import pandas as pd


# atmospheric related utilities
def cal_des_dta(ta, pa, dta=1.0):
    """Calculate slope of es(Ta), i.e., saturation evaporation pressure `es` as function of air temperature `ta [K]`

    Parameters
    ----------
    ta : numeric
        Air temperature [K]
    pa : numeric
        Air pressure [Pa]
    dta : float, optional
        change in ta for calculating that in es, by default 1.0 K
    """
    from atmosp import calculate as ac

    des = ac("es", p=pa, T=ta + dta / 2) - ac("es", p=pa, T=ta - dta / 2)
    des_dta = des / dta
    try:
        # try to pack as Series
        des_dta = pd.Series(des_dta, index=ta.index)
    except AttributeError as ex:
        print(ex, "cannot pack into pd.Series")
        pass
    return des_dta


# calculate specific humidity [kg kg-1] using relative humidity [%]
def cal_qa(rh_pct, theta_K, pres_hPa):
    from atmosp import calculate as ac

    qa = ac("qv", RH=rh_pct, p=pres_hPa * 100, theta=theta_K)
    return qa


# calculate specific humidity deficit [kg kg-1] using relative humidity
def cal_dq(rh_pct, ta_c, pres_hPa):
    from atmosp import calculate as ac

    ta_k = ta_c + 273.16
    pa = pres_hPa * 100
    dq = ac("qvs", T=ta_k, p=pa) - ac("qv", T=ta_k, p=pa, RH=rh_pct)

    return dq


# calculate relative humidity using specific humidity
def cal_rh(qa_kgkg, theta_K, pres_hPa):
    from atmosp import calculate as ac

    RH = ac("RH", av=qa_kgkg, p=pres_hPa * 100, theta=theta_K)
    return RH


# calculate latent heat of vaporisation
def cal_lat_vap(qa_kgkg, theta_K, pres_hPa):
    from atmosp import calculate as ac

    # wel-bulb temperature
    tw = ac(
        "Tw",
        qv=qa_kgkg,
        p=pres_hPa * 100,
        theta=theta_K,
        remove_assumptions=("constant Lv"),
    )
    # latent heat [J kg-1]
    Lv = 2.501e6 - 2370.0 * (tw - 273.15)
    return Lv


# calculate specific heat capacity of air [J kg-1 K-1]
def cal_cp(qa_kgkg, ta_K, pres_hPa):
    from atmosp import calculate as ac

    temp_C = ta_K - 273.15

    rh_pct = ac("RH", qv=qa_kgkg, T=ta_K, p=pres_hPa * 100)

    # # Garratt equation a20(1992)
    # cpd = 1005.0 + ((temp_C + 23.16) ** 2) / 3364.0

    # # Beer(1990) for water vapour
    # cpm = (
    #     1859
    #     + 0.13 * rh_pct
    #     + (19.3 + 0.569 * rh_pct) * (temp_C / 100.0)
    #     + (10.0 + 0.5 * rh_pct) * (temp_C / 100.0) ** 2
    # )

    # # air density
    # rho = ac("rho", qv=qa_kgkg, T=ta_K, p=pres_hPa * 100)

    # # water vapour mixing ratio
    # rv = ac("rv", qv=qa_kgkg, T=ta_K, p=pres_hPa * 100)

    # # dry air density
    # rho_d = rv / (1 + rv) * rho

    # # water vapour density
    # rho_v = rho - rho_d

    # # heat capacity of air
    # cp = cpd * (rho_d / (rho_d + rho_v)) + cpm * (rho_v / (rho_d + rho_v))

    cpa = cal_cp_with_rh(temp_C, rh_pct, pres_hPa)

    return cpa


# calculate specific heat capacity of air [J kg-1 K-1]
def cal_cp_with_rh(temp_C, rh_pct, pres_hPa):
    # from atmosp import calculate as ac

    # Garratt equation a20(1992)
    cpd = 1005.0 + ((temp_C + 23.16) ** 2) / 3364.0

    # Beer(1990) for water vapour
    cpm = (
        1859
        + 0.13 * rh_pct
        + (19.3 + 0.569 * rh_pct) * (temp_C / 100.0)
        + (10.0 + 0.5 * rh_pct) * (temp_C / 100.0) ** 2
    )

    # density of dry air
    rho_d = cal_dens_dry(rh_pct, temp_C, pres_hPa)

    # density of vapour
    rho_v = cal_dens_vap(rh_pct, temp_C, pres_hPa)

    # heat capacity of air
    cpa = cpd * (rho_d / (rho_d + rho_v)) + cpm * (rho_v / (rho_d + rho_v))

    return cpa


# stability correction for momentum
def cal_psi_mom(zoL):
    # limit for neutral condition
    lim_neutral = 1e-5

    zoL = np.where(np.abs(zoL) > 5, 5 * np.sign(zoL), zoL)

    # stable, zoL>0
    zoL_stab = np.where(zoL > lim_neutral, zoL, 0)
    psim_stab = (-6) * np.log(1 + zoL_stab)

    # unstable, zoL<0
    zoL_unstab = np.where(zoL < -lim_neutral, zoL, 0)
    psim_unstab = 0.6 * (2) * np.log((1 + (1 - 16 * zoL_unstab) ** 0.5) / 2)

    # populate values with respect to stability
    psim = np.where(zoL > lim_neutral, psim_stab, psim_unstab)
    psim = np.where(np.abs(zoL) <= lim_neutral, 0, psim)

    return psim


# stability correction for heat
def cal_psi_heat(zoL):
    # limit for neutral condition
    lim_neutral = 1e-5

    zoL = np.where(np.abs(zoL) > 5, 5 * np.sign(zoL), zoL)

    # stable, zoL>0
    zoL_stab = np.where(zoL > lim_neutral, zoL, 0)
    psih_stab = -4.5 * zoL_stab

    # unstable, zoL<0
    zoL_unstab = np.where(zoL < -lim_neutral, zoL, 0)
    psih_unstab = (2) * np.log((1 + (1 - 16 * zoL_unstab) ** 0.5) / 2)

    # populate values with respect to stability
    psih = np.where(zoL > lim_neutral, psih_stab, psih_unstab)
    psih = np.where(np.abs(zoL) <= lim_neutral, 0, psih)

    return psih


# saturation vapour pressure [hPa]
def cal_vap_sat(Temp_C, Press_hPa):
    try:
        ser_pres_kPa = pd.Series(Press_hPa / 10)
        ser_TaC = pd.Series(Temp_C)
    except:
        ser_pres_kPa = Press_hPa
        ser_TaC = Temp_C

    # if ser_TaC is close to zero degC, set to 0.001 degC
    ser_TaC = ser_TaC.where(np.abs(ser_TaC) > 0.001, 0.001)

    # ser_es_hPa = cal_vap_sat(0.001, Press_hPa)

    # 0.001000 <= ser_TaC < 50:
    ser_emb_pos = 6.1121 * np.exp(
        ((18.678 - ser_TaC / 234.5) * ser_TaC) / (ser_TaC + 257.14)
    )
    ser_f_pos = 1.00072 + ser_pres_kPa * (3.2e-6 + 5.9e-10 * ser_TaC**2)

    # -40 < ser_TaC <= -0.001000:
    ser_emb_neg = 6.1115 * np.exp(
        ((23.036 - ser_TaC / 333.7) * ser_TaC) / (ser_TaC + 279.82)
    )
    ser_f_neg = 1.00022 + ser_pres_kPa * (3.83e-6 + 6.4e-10 * ser_TaC**2)

    # combine both conditions
    ser_emb = ser_emb_pos.where(ser_TaC >= 0.001, ser_emb_neg)
    ser_f = ser_f_pos.where(ser_TaC >= 0.001, ser_f_neg)
    ser_es_hPa = ser_emb * ser_f
    return ser_es_hPa


# density of dry air [kg m-3]
def cal_dens_dry(RH_pct, Temp_C, Press_hPa):
    gas_ct_dry = 8.31451 / 0.028965  # dry_gas/molar
    es_hPa = cal_vap_sat(Temp_C, Press_hPa)
    Ea_hPa = RH_pct / 100 * es_hPa
    dens_dry = ((Press_hPa - Ea_hPa) * 100) / (gas_ct_dry * (273.16 + Temp_C))
    return dens_dry


# density of vapour [kg m-3]
def cal_dens_vap(RH_pct, Temp_C, Press_hPa):
    gas_ct_wv = 8.31451 / 0.0180153  # dry_gas/molar_wat_vap
    es_hPa = cal_vap_sat(Temp_C, Press_hPa)
    Ea_hPa = RH_pct / 100 * es_hPa
    vap_dens = Ea_hPa * 100 / ((Temp_C + 273.16) * gas_ct_wv)
    return vap_dens


#
# # specific heat capacity of air mass [J kg-1 K-1]
# def cal_cpa(Temp_C, RH_pct, Press_hPa):
#     # heat capacity of dry air depending on air temperature
#     cpd = 1005.0 + ((Temp_C + 23.16) ** 2) / 3364.0
#     # heat capacity of vapour
#     cpm = (
#         1859
#         + 0.13 * RH_pct
#         + (19.3 + 0.569 * RH_pct) * (Temp_C / 100.0)
#         + (10.0 + 0.5 * RH_pct) * (Temp_C / 100.0) ** 2
#     )
#
#     # density of dry air
#     rho_d = cal_dens_dry(RH_pct, Temp_C, Press_hPa)
#
#     # density of vapour
#     rho_v = cal_dens_vap(RH_pct, Temp_C, Press_hPa)
#
#     # specific heat
#     cpa = cpd * (rho_d / (rho_d + rho_v)) + cpm * (rho_v / (rho_d + rho_v))
#     return cpa


# air density [kg m-3]
def cal_dens_air(Press_hPa, Temp_C):
    # dry_gas/molar
    gas_ct_dry = 8.31451 / 0.028965

    # air density [kg m-3]
    dens_air = (Press_hPa * 100) / (gas_ct_dry * (Temp_C + 273.16))
    return dens_air


# Obukhov length
def cal_Lob(QH, UStar, Temp_C, RH_pct, Pres_hPa, g=9.8, k=0.4):
    # gravity constant/(Temperature*Von Karman Constant)
    G_T_K = (g / (Temp_C + 273.16)) * k

    # air density [kg m-3]
    rho = cal_dens_air(Pres_hPa, Temp_C)

    # specific heat capacity of air mass [J kg-1 K-1]
    cpa = cal_cp_with_rh(Temp_C, RH_pct, Pres_hPa)

    # Kinematic sensible heat flux [K m s-1]
    H = QH / (rho * cpa)

    # temperature scale
    # uStar = np.max([0.01, UStar])
    TStar = -H / UStar

    # Obukhov length
    Lob = (UStar**2) / (G_T_K * TStar)

    return Lob


# aerodynamic resistance
def cal_ra_obs(zm, zd, z0m, z0v, ws, Lob):
    """
    calculate aerodynamic resistance

    Parameters
    ----------
    zm : numeric
        measurement height
    zd : numeric
        displacement height
    z0m : numeric
        roughness length for momumtum
    z0v : numeric
        roughness length for vapour
    ws : numeric
        wind speed
    Lob : numeric
        Obukhov length

    Returns
    -------
    numeric
        aerodynamic resistance
    """
    # von Karman constant
    k = 0.4

    # effective measurement height
    zmd = zm - zd

    # z/L, stability scale
    zoL = zmd / Lob

    ra = (
        1
        / (k**2 * ws)
        * (np.log(zmd / z0m) - (cal_psi_mom(zoL) - cal_psi_mom(z0m / Lob)))
        * (np.log(zmd / z0v) - (cal_psi_heat(zoL) - cal_psi_heat(z0v / Lob)))
    )
    return ra
