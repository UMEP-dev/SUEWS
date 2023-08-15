# basic utilities to deal with geospatial runs of supy


import pandas as pd
import numpy as np

def get_geoinfo(df_state):
    """
    Obtain basic geographical information from `df_state` as a pandas DataFrame.

    Parameters
    ----------
    df_state : pandas.DataFrame
        A pandas DataFrame containing state information.

    Returns
    -------
    pandas.DataFrame
        A pandas DataFrame containing geographical information.

    """
    # land cover fractions
    df_sfr = df_state.sfr_surf
    df_sfr.columns = ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]
    df_sfr = (
        df_sfr.eval("built=paved+bldgs").eval("tree=evetr+dectr").eval("veg=tree+grass")
    )

    # coordinates
    df_coords = df_state[["lat", "lng"]].droplevel("ind_dim", axis=1)

    df_geoinfo = pd.concat([df_coords, df_sfr], axis=1)

    return df_geoinfo
