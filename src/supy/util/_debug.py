import zipfile
from ..supy_driver import atmmoiststab_module, rsl_module
import pandas as pd


def diag_rsl(df_forcing, df_state, df_output, include_rsl=False):
    """
    Diagnose near-surface meteorological variables using RSL scheme as in `suews_driver`.

    Parameters
    ----------
    df_forcing : pandas.Dataframe
        Forcing as used in SuPy run.
    df_state : pandas.Dataframe
        Model states as used in SuPy run.
    df_output : pandas.Dataframe
        Model output produced by SuPy run.
    include_rsl : bool, optional
        Flag to determine if full RSL output at all levels should be included, by default False

    Returns
    -------
    df_sfc (if `include_rsl=False`) or (df_sfc, df_rsl)
        df_sfc: DataFrame with only near-surface level variables
        df_rsl: DataFrame with only RSL results at all levels
    """
    grid = df_state.index[0]

    # get SUEWS group from `df_output`
    try:
        df_suews = df_output.loc[grid, "SUEWS"]
    except:
        df_suews = df_output

    sfr = df_state.sfr.values[0]
    zmeas = df_state.z.values[0]
    zh = df_state[["bldgh", "evetreeh", "dectreeh"]].dot(sfr[[1, 2, 3]])
    fai = df_state[
        [
            "faibldg",
            "faievetree",
            "faidectree",
        ]
    ].dot(sfr[[1, 2, 3]])
    stabilitymethod = df_state.stabilitymethod.values[0]
    dict_sfc = {}
    dict_rsl = {}
    for idx in df_suews.index:
        z0m, zdm, l_mod, qh, qe = df_suews.loc[idx, ["z0m", "zdm", "Lob", "QH", "QE"]]
        temp_c, press_hpa, avrh, avu1 = df_forcing.loc[idx, ["Tair", "pres", "RH", "U"]]
        (
            lv_j_kg,
            lvs_j_kg,
            es_hpa,
            ea_hpa,
            vpd_hpa,
            vpd_pa,
            dq,
            dens_dry,
            avcp,
            avdens,
        ) = atmmoiststab_module.cal_atmmoist(temp_c, press_hpa, avrh, 0.0)
        res_rsl_idx = rsl_module.rslprofile(
            zh,
            z0m,
            zdm,
            l_mod,
            sfr,
            fai,
            stabilitymethod,
            avcp,
            lv_j_kg,
            avdens,
            avu1,
            temp_c,
            avrh,
            press_hpa,
            zmeas,
            qh,
            qe,
        )
        dict_sfc.update({idx.isoformat(): res_rsl_idx[:4]})
        dict_rsl.update({idx.isoformat(): res_rsl_idx[4:]})

    # post-process results
    df_sfc = pd.DataFrame.from_dict(
        dict_sfc, orient="index", columns=["T2", "q2", "U10", "RH2"]
    )
    df_sfc.index = pd.to_datetime(df_sfc.index)
    df_rsl = pd.DataFrame.from_dict(dict_rsl, orient="index")
    df_rsl.index = pd.to_datetime(df_rsl.index)

    if include_rsl:
        return df_sfc, df_rsl
    else:
        return df_sfc


def diag_rsl_prm(df_state, df_output):
    """
    Calculate parameters used in RSL scheme.

    Parameters
    ----------
    df_state : pandas.Dataframe
        Model states as used in SuPy run.
    df_output : pandas.Dataframe
        Model output produced by SuPy run.

    Returns
    -------
    df_sfc (if `include_rsl=False`) or (df_sfc, df_rsl)
        df_sfc: DataFrame with only near-surface level variables
        df_rsl: DataFrame with only RSL results at all levels
    """
    grid = df_state.index[0]

    # get SUEWS group from `df_output`
    try:
        df_suews = df_output.loc[grid, "SUEWS"]
    except:
        df_suews = df_output

    # print(df_suews.head())

    zh_min = 0.15
    sfr = df_state.loc[:, "sfr"]
    sfr_obj = sfr.iloc[:, 1:4].values
    zmeas = df_state.z.values
    fai = df_state.loc[:, ["faibldg", "faievetree", "faidectree"]].values
    h_obj = df_state.loc[:, ["bldgh", "evetreeh", "dectreeh"]].values
    zh = pd.Series(
        [pd.Series(h).dot(sfr) for h, sfr in zip(h_obj, sfr_obj)], index=df_state.index
    )
    fai = pd.Series(
        [pd.Series(fai).dot(sfr) for fai, sfr in zip(fai, sfr_obj)],
        index=df_state.index,
    )
    stabilitymethod = df_state.stabilitymethod.values

    dict_prm = {}
    zh = zh.iloc[0]
    fai = fai.iloc[0]
    sfr = sfr.iloc[0]
    # print(zh,fai,sfr)
    for idx in df_suews.index:
        print(df_suews.loc[idx])
        print(df_suews.loc[idx, ["z0m", "zdm", "Lob", "QH", "QE"]])
        z0m, zdm, l_mod, qh, qe = df_suews.loc[idx, ["z0m", "zdm", "Lob", "QH", "QE"]]
        (
            l_mod_rsl,
            zh_rsl,
            lc,
            beta,
            zd,
            z0,
            elm,
            scc,
            f,
            PAI,
        ) = rsl_module.rsl_cal_prms(
            stabilitymethod,
            zh,
            l_mod,
            sfr,
            fai,
        )
        dict_prm.update({
            idx.isoformat(): [l_mod_rsl, zh_rsl, lc, beta, zd, z0, elm, scc, f, PAI]
        })

    # post-process results
    df_prm = pd.DataFrame.from_dict(
        dict_prm,
        orient="index",
        columns=[
            "l_mod_rsl",
            "zh_rsl",
            "lc",
            "beta",
            "zd",
            "z0",
            "elm",
            "scc",
            "f",
            "PAI",
        ],
    )
    df_prm.index = pd.to_datetime(df_prm.index)

    return df_prm


def save_zip_debug(df_forcing, df_state_init, error_info=None):
    import tempfile
    from pathlib import Path
    import traceback
    from datetime import datetime

    from .._version import show_version

    path_dir_save = Path.cwd()
    path_json_version = path_dir_save / "supy_info.json"
    try:
        path_json_version.touch()
    except Exception:
        tempdir = tempfile.gettempdir()
        path_dir_save = Path(tempdir)
        path_json_version = path_dir_save / path_json_version.name

    # save version info
    show_version(as_json=path_json_version.as_posix())

    # save forcing data
    path_forcing = path_dir_save / "df_forcing.pkl"
    df_forcing.to_pickle(path_forcing)

    # save state data
    path_state_init = path_dir_save / "df_state_init.pkl"
    df_state_init.to_pickle(path_state_init)

    # get a random hash to use as a unique identifier for this run
    import hashlib

    hash = hashlib.md5(str(path_dir_save).encode("utf-8")).hexdigest()[:8]

    # save error information if provided
    if error_info is not None:
        path_error = path_dir_save / "error_info.md"
        with open(path_error, "w") as f:
            # Write timestamp as header
            f.write(f"# SuPy Error Report\n\n")
            f.write(f"## Timestamp\n")
            f.write(f"{datetime.now().isoformat()}\n\n")

            # Write error info
            if isinstance(error_info, Exception):
                f.write(f"## Error Type\n")
                f.write(f"`{type(error_info).__name__}`\n\n")
                f.write(f"## Error Message\n")
                f.write(f"```shell\n{str(error_info)}\n```\n\n")
                f.write(f"## Traceback\n")
                f.write("```python\n")
                f.write("".join(traceback.format_tb(error_info.__traceback__)))
                f.write("```\n")
            else:
                f.write(f"## Error Details\n")
                f.write(f"```\n{str(error_info)}\n```\n")

    # bundle all files in a zip file
    path_zip_debug = path_dir_save / f"supy_debug-{hash}.zip"
    with zipfile.ZipFile(path_zip_debug, "w") as myzip:
        myzip.write(path_forcing.as_posix(), arcname=path_forcing.name)
        myzip.write(path_state_init.as_posix(), arcname=path_state_init.name)
        myzip.write(path_json_version.as_posix(), arcname=path_json_version.name)
        if error_info is not None:
            myzip.write(path_error.as_posix(), arcname=path_error.name)

    return path_zip_debug
